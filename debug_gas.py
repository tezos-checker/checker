from typing import Any, Dict, Set
from pytezos import operation, pytezos
from pytezos.contract.call import ContractCall
from pytezos.contract.interface import ContractInterface
from pytezos.michelson.micheline import micheline_value_to_python_object
from pytezos.michelson.types.adt import ADTMixin
from pytezos.michelson.types.base import MichelsonType, Undefined
from pytezos.michelson.types.pair import PairType
from pytezos.operation.group import OperationGroup
from pytezos.rpc.helpers import ForgeOperationsQuery
from pytezos.rpc.protocol import BlockQuery
from pytezos.rpc.search import BlockSliceQuery
from pytezos.michelson.types.big_map import BigMapType, big_map_diff_to_lazy_diff
from checker_client import checker as checker_lib
import json
from pytezos.michelson.sections.storage import StorageSection
from pytezos.operation.content import format_mutez


def skip_nones(**kwargs) -> dict:
    return {k: v for k, v in kwargs.items() if v is not None}


client = pytezos.using(
    "http://localhost:11112",
    key="edsk3RFfvaFaxbHx8BMtEW1rKQcPtDML3LXjNqMNLCzC3wLC1bWbAt",
)

# client.shell.blocks[1:].find_origination("KT1KFKFgNv5Di2bDwdRk1cUyud4npZ1Pv727")

# entrypoint = checker.create_burrow((2, None))

# # TODO: Can we can run_operation to run with a higher gas limit?
# Answer: NO :'(
# group = entrypoint.with_amount(10_000_000)
# group.run_operation()
# # .as_transaction()
# # for c in group.contents:
# #     c["gas_limit"] = 200_000_000


# entrypoint.run_code(storage=checker.storage())


# The contract for which we want to read all big maps
contract_id = "KT1PwzHsf6FkA3EsRe7WrfMscNfer5krHQMd"
# Which block to start scanning for diffs
start_from_block = 1
# End the scan after N blocks without updates to the contract storage
end_scan_blocks = 10

diffs = []
blocks_since_update = None

blocks: BlockSliceQuery = client.shell.blocks[start_from_block:]
for b in blocks:
    block_diffs = []
    if contract_id in b.context.contracts():
        # Contract exists in this operation. Let's look for ops on it
        ops = client.shell.blocks[b.hash()].operations.managers()
        for op_group in ops:
            # contents.metadata.operation_result.big_map_diff
            op_group = op_group["contents"]
            for op in op_group:
                op_result = op["metadata"]["operation_result"]
                # Skip ops which don't operate on the contract of interest
                if (
                    op["kind"] == "origination"
                    and contract_id not in op_result["originated_contracts"]
                ):
                    continue
                if op["kind"] == "transaction" and op["destination"] != contract_id:
                    continue
                block_diffs += op_result["lazy_storage_diff"]

    diffs += block_diffs
    if block_diffs:
        blocks_since_update = 0
    if not block_diffs and blocks_since_update is not None:
        blocks_since_update += 1

    if blocks_since_update is not None and blocks_since_update > end_scan_blocks:
        print(f"No storage updates found since {blocks_since_update}. Ending scan.")
        break

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
        # an Option, but am not 100% sure.
        if micheline_value_to_python_object(update["value"]) is None:
            keys[bigmap_id].remove(key)
        # Otherwise record the key
        else:
            keys[bigmap_id].add(key)

checker = client.contract(contract_id)
s = checker.storage()


def render_bigmap(
    field: str, bigmap_id: int, keys: Dict[int, Set[Any]], contract: ContractInterface
):
    full_map = {}
    bigmap_id = bigmap_id
    if bigmap_id not in keys:
        return {}
    for key in keys[bigmap_id]:
        # Call tezos to get the value for this key
        full_map[key] = contract.storage[field][key]()
    return full_map


def storage_with_bigmaps(
    contract: ContractInterface,
    contract_bigmap_keys: Dict[int, Set[Any]],
    michelson_data: MichelsonType,
):
    storage = {}
    # This node is a big map, let's populate it with its data
    if isinstance(michelson_data, BigMapType):
        return render_bigmap(
            michelson_data.field_name,
            michelson_data.to_python_object(),
            keys,
            contract,
        )
    # This node is not a big map and has no children, just return it
    if not hasattr(michelson_data, "items"):
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
        child_data = storage_with_bigmaps(contract, contract_bigmap_keys, child)
        if child_data is Undefined:
            continue
        storage[child.field_name] = child_data
    return storage


# def rec_render_bigmap(michelson_data: MichelsonType):
#     the_map = {}
#     if isinstance(michelson_data, BigMapType):
#         the_map[michelson_data.field_name] = render_bigmap(
#             mich_field.field_name,
#             mich_field.to_python_object(),
#             keys,
#             contract,
#         )
#     if hasattr(michelson_data, "items"):
#         for field in michelson_data:
#             rec_render_bigmap(field)

#         # .items has a list of subfields

#     data = checker.storage()
#     data.update(the_map)

#     # TODO: Recurse subf

test = storage_with_bigmaps(checker, keys, checker.storage.data)

with_funcs = rec_render_bigmap(checker, keys)

# Create call to `default` entrypoint
call = checker.create_burrow((2, None)).with_amount(2_000_000)


def trace_code(
    call: ContractCall,
    storage=None,
    source=None,
    sender=None,
    amount=None,
    balance=None,
    chain_id=None,
    gas_limit=None,
):
    """Execute using RPC interpreter.

    :param storage: initial storage as Python object, leave None if you want to generate a dummy one
    :param source: patch SOURCE
    :param sender: patch SENDER
    :param amount: patch AMOUNT
    :param balance: patch BALANCE
    :param chain_id: patch CHAIN_ID
    :param gas_limit: restrict max consumed gas
    :rtype: ContractCallResult
    """
    storage_ty = StorageSection.match(call.context.storage_expr)
    if storage is None:
        initial_storage = storage_ty.dummy(call.context).to_micheline_value(
            lazy_diff=True
        )
    else:
        initial_storage = storage_ty.from_python_object(storage).to_micheline_value(
            lazy_diff=True
        )
    script = [
        call.context.parameter_expr,
        call.context.storage_expr,
        call.context.code_expr,
    ]
    query = skip_nones(
        script=script,
        storage=initial_storage,
        entrypoint=call.parameters["entrypoint"],
        input=call.parameters["value"],
        amount=format_mutez(amount or call.amount),
        chain_id=chain_id or call.context.get_chain_id(),
        source=sender,
        payer=source,
        balance=str(balance or 0),
        gas=str(gas_limit) if gas_limit is not None else None,
    )
    res = call.shell.blocks[call.block_id].helpers.scripts.trace_code.post(query)
    op_results = []
    query_op = OperationGroup(context=call.context, contents=res["operations"])

    # t = OperationGroup(context=client.context).origination(
    #     script=op["script"],
    #     balance=op["balance"],
    #     delegate=None,
    # )

    for op in res["operations"]:
        #     # Only works shallowly at the moment
        query_op = skip_nones(
            script=op["script"]["code"],
            storage=op["script"]["storage"],
            entrypoint=None,
            input={},
            amount=str(0),
            chain_id=chain_id or call.context.get_chain_id(),
            source=None,
            payer=None,
            balance=format_mutez(op["balance"]),
            gas=str(gas_limit) if gas_limit is not None else None,
        )
        op_res = call.shell.blocks[call.block_id].helpers.scripts.trace_code.post(
            query_op
        )
    # TODO: Add a contructor here for the trace_code output
    # ContractCallResult.from_run_code(res, parameters=call.parameters, context=call.context)
    return res, op_results


result = trace_code(
    call,
    storage=with_funcs,
    source="tz1aSkwEot3L2kmUvcoxzjMomb9mvBNuzFK6",
    gas_limit=None,
)

with open("test_trace.json", "w") as f:
    json.dump(result, f)


gas_trace = [int(t["gas"]) for t in result["trace"]]
print(call.as_transaction().autofill().contents)

print((1_040_000_000 - min(gas_trace)) / 1000)

# Node interpreter
call.run_operation()

print(result)
# TODO: Can we get the gas used from this??
