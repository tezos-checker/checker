from typing import Any, Dict, Optional, Set
from pytezos import operation, pytezos
from pytezos.client import PyTezosClient
from pytezos.contract.call import ContractCall
from pytezos.contract.interface import ContractInterface
from pytezos.michelson.micheline import micheline_value_to_python_object
from pytezos.michelson.types.adt import ADTMixin
from pytezos.michelson.types.base import MichelsonType, Undefined
from pytezos.michelson.types.pair import PairType
from pytezos.michelson.types import ListType
from pytezos.operation.group import OperationGroup
from pytezos.rpc.helpers import ForgeOperationsQuery
from pytezos.rpc.protocol import BlockQuery
from pytezos.rpc.search import BlockSliceQuery
from pytezos.michelson.types.big_map import BigMapType, big_map_diff_to_lazy_diff
from checker_client import checker as checker_lib
import json
from pytezos.michelson.sections.storage import StorageSection
from pytezos.operation.content import format_mutez
from pprint import pprint

# def skip_nones(**kwargs) -> dict:
#     return {k: v for k, v in kwargs.items() if v is not None}


def scan_bigmap_keys(
    tz: PyTezosClient,
    contract_address: str,
    start_block: int = 1,
    end_after_blocks: Optional[int] = None,
) -> Dict[str, Set[Any]]:
    """Scans for all big_map keys in a contract's storage

    This function works by scanning the main chain for any operations on
    the specified contract which produce a lazy_storage_diff.

    Args:
        tz: pytezos client instance
        contract_address: The address of the contract of interest
        start_block: The block at which to start scanning. Note that setting this to a block after
            the contract's origination may cause you to miss keys.
        end_after_blocks: Optionally end the scan after this many blocks without an update
            to a big_map. The default value of None will cause the function to scan the entire
            chain.
    Returns:
        A dict mapping bigmap_ids to their keys. Note that ids may be missing from here if no
            keys are ever added to the big_map (i.e. it is only allocated).
    """
    diffs = []
    blocks_since_update = None
    blocks: BlockSliceQuery = tz.shell.blocks[start_block:]
    # First, need to collect the lazy storage diffs
    for b in blocks:
        block_diffs = []
        if contract_address in b.context.contracts():
            # Contract exists in this operation. Let's look for operations on it
            ops = tz.shell.blocks[b.hash()].operations.managers()
            for op_group in ops:
                op_group = op_group["contents"]
                for op in op_group:
                    op_result = op["metadata"]["operation_result"]
                    # Skip ops which don't operate on the contract of interest
                    if (
                        op["kind"] == "origination"
                        and contract_address not in op_result["originated_contracts"]
                    ):
                        continue
                    if (
                        op["kind"] == "transaction"
                        and op["destination"] != contract_address
                    ):
                        continue
                    block_diffs += op_result["lazy_storage_diff"]
        diffs += block_diffs
        if block_diffs:
            blocks_since_update = 0
        # Only increment if we have already found the first block operating on this contract
        if not block_diffs and blocks_since_update is not None:
            blocks_since_update += 1

        if (
            end_after_blocks
            and blocks_since_update is not None
            and blocks_since_update > end_after_blocks
        ):
            print(
                f"No storage updates found since {blocks_since_update} blocks. Ending scan."
            )
            break
    # Now we can extract the keys from the diffs
    keys = {}
    for diff in diffs:
        if diff["kind"] != "big_map":
            continue
        bigmap_id = diff["id"]
        diff = diff["diff"]
        # Note: diffs of action type "alloc" allocate a big map, and those with
        # action "update" update a key value pair
        if diff["action"] != "update":
            continue
        if bigmap_id not in keys:
            keys[bigmap_id] = set([])
        for update in diff["updates"]:
            key = micheline_value_to_python_object(update["key"])
            # If the update removes the key value pair, remove it from our map
            # TODO: I am pretty sure this is safe for cases where the value gets updated to
            # an Option with value None, but am not 100% sure.
            if micheline_value_to_python_object(update["value"]) is None:
                keys[bigmap_id].remove(key)
            # Otherwise record the key
            else:
                keys[bigmap_id].add(key)
    return keys


def _render_bigmap(
    field: str, bigmap_id: int, keys: Dict[int, Set[Any]], contract: ContractInterface
):
    full_map = {}
    bigmap_id = str(bigmap_id)
    if bigmap_id not in keys:
        return {}
    for key in keys[bigmap_id]:
        # Call tezos to get the value for this key
        full_map[key] = contract.storage[field][key]()
    return full_map


def storage_with_bigmaps(
    contract: ContractInterface,
    contract_bigmap_keys: Dict[int, Set[Any]],
) -> Any:
    """Queries a contract's storage, rendering bigmaps with their full contents.

    Warning: since this method forces big_maps which are usually lazy, it will load all of
    their contents into memory.

    Args:
        contract: The contract of interest
        contract_bigmap_keys: The keys in this contract's bigmaps. Missing bigmap_ids will be loaded as
            empty dicts.
    Returns:
        A python representation of the contract's storage (similar to calling contract.storage())
    """

    def _rec_load_storage(contract, contract_bigmap_keys, michelson_data):
        storage = {}
        # This node is a big map, let's populate it with its data
        if isinstance(michelson_data, BigMapType):
            return _render_bigmap(
                michelson_data.field_name,
                michelson_data.to_python_object(),
                contract_bigmap_keys,
                contract,
            )
        # This node is not a big map and has no children, just return it
        # Currently this also catches lists, which means that bigmaps nested within a list
        # will not be rendered.
        if not hasattr(michelson_data, "items") or isinstance(michelson_data, ListType):
            # Only render items which are not "undefined". Pytezos uses same behavior internally.
            if michelson_data is Undefined:
                return Undefined
            return michelson_data.to_python_object()
        # Otherwise, this element is a container with child elements, recurse into them
        if isinstance(michelson_data, ADTMixin):
            # Because reasons, need to select the second element from this generator
            child_iterator = map((lambda e: e[1]), michelson_data.iter_values())
        else:
            child_iterator = iter(michelson_data.items)
        for child in child_iterator:
            child_data = _rec_load_storage(contract, contract_bigmap_keys, child)
            if child_data is Undefined:
                continue
            storage[child.field_name] = child_data
        return storage

    return _rec_load_storage(contract, contract_bigmap_keys, contract.storage.data)


client = pytezos.using(
    "http://localhost:11112",
    key="edsk3RFfvaFaxbHx8BMtEW1rKQcPtDML3LXjNqMNLCzC3wLC1bWbAt",
)
checker_address = "KT1PwzHsf6FkA3EsRe7WrfMscNfer5krHQMd"
checker = client.contract(checker_address)

checker_bigmap_keys = scan_bigmap_keys(client, checker_address, end_after_blocks=5)
storage = storage_with_bigmaps(checker, checker_bigmap_keys)

pprint(storage)

# # Create call to `default` entrypoint
# call = checker.create_burrow((2, None)).with_amount(2_000_000)


# def trace_code(
#     call: ContractCall,
#     storage=None,
#     source=None,
#     sender=None,
#     amount=None,
#     balance=None,
#     chain_id=None,
#     gas_limit=None,
# ):
#     """Execute using RPC interpreter.

#     :param storage: initial storage as Python object, leave None if you want to generate a dummy one
#     :param source: patch SOURCE
#     :param sender: patch SENDER
#     :param amount: patch AMOUNT
#     :param balance: patch BALANCE
#     :param chain_id: patch CHAIN_ID
#     :param gas_limit: restrict max consumed gas
#     :rtype: ContractCallResult
#     """
#     storage_ty = StorageSection.match(call.context.storage_expr)
#     if storage is None:
#         initial_storage = storage_ty.dummy(call.context).to_micheline_value(
#             lazy_diff=True
#         )
#     else:
#         initial_storage = storage_ty.from_python_object(storage).to_micheline_value(
#             lazy_diff=True
#         )
#     script = [
#         call.context.parameter_expr,
#         call.context.storage_expr,
#         call.context.code_expr,
#     ]
#     query = skip_nones(
#         script=script,
#         storage=initial_storage,
#         entrypoint=call.parameters["entrypoint"],
#         input=call.parameters["value"],
#         amount=format_mutez(amount or call.amount),
#         chain_id=chain_id or call.context.get_chain_id(),
#         source=sender,
#         payer=source,
#         balance=str(balance or 0),
#         gas=str(gas_limit) if gas_limit is not None else None,
#     )
#     res = call.shell.blocks[call.block_id].helpers.scripts.trace_code.post(query)
#     # TODO: Add a contructor here for the trace_code output
#     # ContractCallResult.from_run_code(res, parameters=call.parameters, context=call.context)
#     return res


# result = trace_code(
#     call,
#     storage=test,
#     source="tz1aSkwEot3L2kmUvcoxzjMomb9mvBNuzFK6",
#     gas_limit=100_000_000,
# )

# with open("test_trace.json", "w") as f:
#     json.dump(result, f)


# gas_trace = [int(t["gas"]) for t in result["trace"]]
# print(call.as_transaction().autofill().contents)

# print((1_040_000_000 - min(gas_trace)) / 1000)

# # Node interpreter
# call.run_operation()

# print(result)
# # TODO: Can we get the gas used from this??
