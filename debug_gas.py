from typing import Any, Dict, Set
from pytezos import pytezos
from pytezos.contract.call import ContractCall
from pytezos.contract.interface import ContractInterface
from pytezos.michelson.micheline import micheline_value_to_python_object
from pytezos.rpc.protocol import BlockQuery
from pytezos.rpc.search import BlockSliceQuery
from pytezos.michelson.types.big_map import BigMapType, big_map_diff_to_lazy_diff
from checker_client import checker as checker_lib

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
contract_id = "KT1FwfJ3xM2qqU7xn7QkjufYgnxFqgJh9qaV"
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
    if diff["action"] != "update":
        continue

    if bigmap_id not in keys:
        keys[bigmap_id] = set([])
    for update in diff["updates"]:
        key = micheline_value_to_python_object(update["key"])
        # If the update removes the key value pair, remove it from our map
        if micheline_value_to_python_object(update["value"]) is None:
            keys[bigmap_id].remove(key)
        # Otherwise record the key
        else:
            keys[bigmap_id].add(key)

checker = client.contract(contract_id)
s = checker.storage()


# If a key is updated with a non-None value, keep it. Otherwise remove it.
def render_bigmap(
    field: str, bigmap_id: int, keys: Dict[int, Set[Any]], contract: ContractInterface
):
    full_map = {}
    for key in keys[f"{bigmap_id}"]:
        # Call tezos to get the value for this key
        full_map[key] = contract.storage[field][key]()
    return full_map


def rec_render_bigmap(contract: ContractInterface, keys):
    the_map = {}
    for mich_field in checker.storage.data:
        if isinstance(mich_field, BigMapType):
            the_map[mich_field.field_name] = render_bigmap(
                mich_field.field_name,
                mich_field.to_python_object(),
                keys,
                contract,
            )
    data = checker.storage()
    data.update(the_map)

    # TODO: Recurse subfields
    return data


with_funcs = rec_render_bigmap(checker, keys)

# Create call to `default` entrypoint
call = checker.mintKit(
    ("KT1AmN2S3hwzijmMKyvXw41MPWCCwDUqTfZr", "KT1BSYRtp4pYo4RQj7BhyTbBg7zCGnHr2LL3")
)


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
    # TODO: Add a contructor here for the trace_code output
    # ContractCallResult.from_run_code(res, parameters=call.parameters, context=call.context)
    return res


result = trace_code(
    call,
    storage=with_funcs,
    source="tz1aSkwEot3L2kmUvcoxzjMomb9mvBNuzFK6",
    gas_limit=None,
)

# sum([int(t["gas"]) for t in result["trace"]])

# Node interpreter
result = call.run_code(
    storage=with_funcs,
    source="tz1aSkwEot3L2kmUvcoxzjMomb9mvBNuzFK6",
    gas_limit=None,
)

print(result)
# TODO: Can we get the gas used from this??
