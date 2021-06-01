from typing import Any, Dict, Set
from pytezos import pytezos
from pytezos.contract.call import ContractCall
from pytezos.contract.interface import ContractInterface
from pytezos.michelson.micheline import micheline_value_to_python_object
from pytezos.rpc.protocol import BlockQuery
from pytezos.rpc.search import BlockSliceQuery
from pytezos.michelson.types.big_map import BigMapType, big_map_diff_to_lazy_diff

client = pytezos.using(
    "http://localhost:11112",
    key="edsk3RFfvaFaxbHx8BMtEW1rKQcPtDML3LXjNqMNLCzC3wLC1bWbAt",
)


# client.shell.blocks[1:].find_origination("KT1KFKFgNv5Di2bDwdRk1cUyud4npZ1Pv727")

# entrypoint = checker.create_burrow((2, None))

# # TODO: Can we can run_operation to run with a higher gas limit?
# group = entrypoint.with_amount(10_000_000)
# group.run_operation()
# # .as_transaction()
# # for c in group.contents:
# #     c["gas_limit"] = 200_000_000


# entrypoint.run_code(storage=checker.storage())


# Construct block slice query
# Replay from block 1 until you find the contract, then start collecting storage diffs

contract_id = "KT1ALzpYUkwUi5dXkDvix95fUeJjZRUVAUuR"
lastCall = "sealContract"
diffs = []

end_scan_blocks = 10


blocks_since_update = None
blocks: BlockSliceQuery = client.shell.blocks[2000:]
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
                print(op_result["lazy_storage_diff"])
                # if (
                #     op["kind"] == "transaction"
                #     and op["parameters"]["entrypoint"] == lastCall
                # ):
                #     print("Found last call for preparing contract")
                #     break
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
        keys[bigmap_id].add(micheline_value_to_python_object(update["key"]))

checker = client.contract("KT1ALzpYUkwUi5dXkDvix95fUeJjZRUVAUuR")
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
call = checker.sealContract(
    ("KT1FCu6H8kL5ortBaXFRK7hv2Rb3CuJR8kZ1", "KT1J17YAMFPJz6DdSWsSsQzSqvi2KYyinwmk")
)

# Node interpreter
result = call.trace_code(
    storage=with_funcs,
    source="tz1aSkwEot3L2kmUvcoxzjMomb9mvBNuzFK6",
    gas_limit=100_000_000,
)

# TODO: Can we get the gas used from this??
