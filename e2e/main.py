from copy import deepcopy
import json
import os
from random import shuffle
import unittest
from collections import OrderedDict
from datetime import datetime
from typing import Callable, Dict, Generator, Tuple

import portpicker
from checker_client.checker import *
from pytezos.contract.interface import ContractInterface
from pytezos.operation import MAX_OPERATIONS_TTL
from pytezos.operation.group import OperationGroup

PROJECT_ROOT = os.path.join(os.path.dirname(__file__), "../")
CHECKER_DIR = os.getenv(
    "CHECKER_DIR", default=os.path.join(PROJECT_ROOT, "generated/michelson")
)
# Optional file at which to output gas costs in end to end tests
WRITE_GAS_COSTS = os.getenv("WRITE_GAS_COSTS")
# Optional file at which to output profiles of gas costs from liquidation auction stress
# tests
WRITE_GAS_PROFILE = os.getenv("WRITE_GAS_PROFILES")


class SandboxedTestCase(unittest.TestCase):
    def setUp(self):
        port = portpicker.pick_unused_port()
        client, docker_client, docker_container = start_sandbox(
            "checker-e2e-container-{}".format(port), port, wait_for_level=2
        )
        self.docker_client = docker_client
        self.docker_container = docker_container
        self.client = client
        self.gas_profiles = {}

    def tearDown(self):
        self.docker_container.kill()
        self.docker_client.close()
        # TODO: Write out gas profiles here
        # print(json.dumps(gas_profiles, indent=4))
        # with open("liquidation_gas_profiles.json", "w") as f:
        #     json.dump(gas_profiles, f, indent=4)


def assert_kit_balance(checker: ContractInterface, address: str, expected_kit: int):
    # TODO: There might be a way to get this from contract metadata
    kit_token_id = 0
    kit_balance = checker.metadata.getBalance((address, kit_token_id)).storage_view()
    assert expected_kit == kit_balance


def avl_storage(checker: ContractInterface, ptr: int) -> Dict:
    """Reads an item from checker's AVL backend using its pointer"""
    return checker.storage["deployment_state"]["sealed"]["liquidation_auctions"][
        "avl_storage"
    ]["mem"][ptr]()


AvlPtr = int
AvlNode = Dict
AvlLeaves = Generator[Tuple[AvlPtr, AvlNode], None, List]
Operation = Dict


def auction_avl_leaves(
    checker: ContractInterface,
    avl_ptr: int,
) -> AvlLeaves:
    """Retrieves a list of all leaves in the specified AVL tree"""
    node = avl_storage(checker, avl_ptr)
    node_type = list(node.keys())[0]

    if node_type == "root":
        next_node, _ = node["root"]
        # Tree is empty
        if next_node is None:
            return []
        else:
            for ptr, leaf_node in auction_avl_leaves(checker, next_node):
                yield ptr, leaf_node
    elif node_type == "leaf":
        yield avl_ptr, node
    else:
        # We are in a branch
        # Traverse left side
        for ptr, leaf_node in auction_avl_leaves(checker, node["branch"]["left"]):
            yield ptr, leaf_node
        # Then the right side
        for ptr, leaf_node in auction_avl_leaves(checker, node["branch"]["right"]):
            yield ptr, leaf_node


# class E2ETest(SandboxedTestCase):
#     def test_e2e(self):
#         gas_costs = {}
#         account = self.client.key.public_key_hash()
#         # Read kit token id for fa2 tests
#         with open(os.path.join(CHECKER_DIR, "functions.json")) as f:
#             kit_token_id = json.load(f)["token_info"]["kit_token_id"]
#         # ===============================================================================
#         # Deploy contracts
#         # ===============================================================================
#         print("Deploying the mock oracle.")
#         oracle = deploy_contract(
#             self.client,
#             source_file=os.path.join(PROJECT_ROOT, "util/mock_oracle.tz"),
#             initial_storage=(self.client.key.public_key_hash(), 1000000),
#             ttl=MAX_OPERATIONS_TTL,
#         )

#         print("Deploying ctez contract.")
#         ctez = deploy_ctez(
#             self.client,
#             ctez_dir=os.path.join(PROJECT_ROOT, "vendor/ctez"),
#             ttl=MAX_OPERATIONS_TTL,
#         )

#         print("Deploying Checker.")
#         checker = deploy_checker(
#             self.client,
#             checker_dir=CHECKER_DIR,
#             oracle=oracle.context.address,
#             ctez=ctez["fa12_ctez"].context.address,
#             ttl=MAX_OPERATIONS_TTL,
#         )

#         print("Deployment finished.")
#         gas_costs = {}

#         def call_endpoint(contract, name, param, amount=0, client=self.client):
#             return inject(
#                 client,
#                 getattr(contract, name)(param)
#                 .with_amount(amount)
#                 .as_transaction()
#                 .autofill(ttl=MAX_OPERATIONS_TTL)
#                 .sign(),
#             )

#         def call_checker_endpoint(
#             name, param, amount=0, contract=checker, client=self.client
#         ):
#             print("Calling", name, "with", param)
#             ret = call_endpoint(contract, name, param, amount, client=client)
#             gas_costs[name] = ret["contents"][0]["gas_limit"]
#             return ret

#         # ===============================================================================
#         # Burrows
#         # ===============================================================================
#         # Create a burrow
#         call_checker_endpoint("create_burrow", (1, None), amount=10_000_000)
#         assert_kit_balance(checker, account, 0)

#         # Get some kit
#         call_checker_endpoint("mint_kit", (1, 1_000_000))
#         assert_kit_balance(checker, account, 1_000_000)

#         # Burn some kit
#         call_checker_endpoint("burn_kit", (1, 10))
#         assert_kit_balance(checker, account, 999_990)

#         # Deposit tez
#         call_checker_endpoint("deposit_tez", 1, amount=2_000_000)

#         # Withdraw tez
#         call_checker_endpoint("withdraw_tez", (2_000_000, 1))

#         # Set delegate
#         call_checker_endpoint("set_burrow_delegate", (1, account))

#         # Deactivate a burrow
#         call_checker_endpoint("create_burrow", (2, None), amount=3_000_000)
#         call_checker_endpoint("deactivate_burrow", (2, account))

#         # Re-activate a burrow
#         call_checker_endpoint("activate_burrow", 2, amount=1_000_000)

#         # Touch checker
#         call_checker_endpoint("touch", None)

#         # Touch burrow
#         call_checker_endpoint("touch_burrow", (account, 1))

#         # ===============================================================================
#         # FA2 interface
#         # ===============================================================================
#         # Note: using alice's account which is assumed to be already initialized in the sandbox
#         checker_alice = pytezos.pytezos.using(
#             shell=checker.shell,
#             key="edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq",
#         ).contract(checker.address)

#         # Transfer from the main test account to alice's account
#         fa2_transfer = {
#             "from_": account,
#             "txs": [
#                 {
#                     "to_": checker_alice.key.public_key_hash(),
#                     "token_id": kit_token_id,
#                     "amount": 90,
#                 },
#             ],
#         }
#         call_checker_endpoint("transfer", [fa2_transfer])
#         assert_kit_balance(checker, checker_alice.key.public_key_hash(), 90)

#         # Add the main account as an operator on alice's account
#         # Note: using a client instance with alice's key for this since she is the
#         # account owner.
#         update_operators = [
#             {
#                 "add_operator": {
#                     "owner": checker_alice.key.public_key_hash(),
#                     "operator": account,
#                     "token_id": kit_token_id,
#                 }
#             },
#         ]
#         call_checker_endpoint(
#             "update_operators",
#             update_operators,
#             client=checker_alice,
#             contract=checker_alice,
#         )

#         # Send some kit back to the main test account
#         fa2_transfer = {
#             "from_": checker_alice.key.public_key_hash(),
#             "txs": [
#                 {
#                     "to_": account,
#                     "token_id": kit_token_id,
#                     "amount": 80,
#                 },
#             ],
#         }
#         call_checker_endpoint("transfer", [fa2_transfer])
#         assert_kit_balance(checker, checker_alice.key.public_key_hash(), 10)

#         # `balance_of` requires a contract callback when executing on-chain. To make tests
#         # more light-weight and avoid needing an additional mock contract, we call it as a view.
#         # This comes with a downside, however, since using a view means that we don't get an
#         # estimate of the gas cost.
#         fa2_balance_of = {
#             "requests": [
#                 {"owner": checker_alice.key.public_key_hash(), "token_id": kit_token_id}
#             ],
#             "callback": None,
#         }
#         print(f"Calling balance_of as an off-chain view with {fa2_balance_of}")
#         balance = checker.balance_of(**fa2_balance_of).callback_view()
#         # Check that this balance agrees with the kit balance view
#         assert_kit_balance(
#             checker, checker_alice.key.public_key_hash(), balance[0]["nat_2"]
#         )

#         # ===============================================================================
#         # CFMM
#         # ===============================================================================
#         # Get some ctez
#         call_endpoint(
#             ctez["ctez"], "create", (1, None, {"any": None}), amount=1_000_000
#         )
#         call_endpoint(ctez["ctez"], "mint_or_burn", (1, 800_000))
#         # Approve checker to spend the ctez
#         call_endpoint(ctez["fa12_ctez"], "approve", (checker.context.address, 800_000))

#         call_checker_endpoint(
#             "add_liquidity", (400_000, 400_000, 5, int(datetime.now().timestamp()) + 20)
#         )
#         # Note: not adding FA2 balance assertions here since the amount of kit depends on the price
#         # and adding an assertion might make the test flaky
#         call_checker_endpoint("buy_kit", (10, 5, int(datetime.now().timestamp()) + 20))
#         call_checker_endpoint("sell_kit", (5, 1, int(datetime.now().timestamp()) + 20))

#         call_checker_endpoint(
#             "remove_liquidity", (5, 1, 1, int(datetime.now().timestamp()) + 20)
#         )

#         print("Gas costs:")
#         gas_costs_sorted = OrderedDict()
#         for k, v in sorted(
#             gas_costs.items(), key=lambda tup: int(tup[1]), reverse=True
#         ):
#             gas_costs_sorted[k] = v

#         print(json.dumps(gas_costs_sorted, indent=4))
#         if WRITE_GAS_COSTS:
#             with open(WRITE_GAS_COSTS, "w") as f:
#                 json.dump(gas_costs_sorted, f, indent=4)


Profiler: Callable[[PyTezosClient, ContractInterface, List[OperationGroup]], Dict]


def general_gas_profiler(
    client: PyTezosClient, checker: ContractInterface, op_batch: List[OperationGroup]
) -> Dict:
    ret = inject(
        client,
        client.bulk(*op_batch).autofill(ttl=MAX_OPERATIONS_TTL).sign(),
    )
    profile = {}
    for op in ret["contents"]:
        name = op["parameters"]["entrypoint"]
        if name not in profile:
            profile[name] = {"gas": []}
        profile[name]["gas"].append(op["gas_limit"])
    return profile


def mark_for_liquidation_profiler(
    client: PyTezosClient, checker: ContractInterface, op_batch: List[OperationGroup]
) -> Dict:
    n = len(
        list(
            auction_avl_leaves(
                checker,
                checker.storage["deployment_state"]["sealed"]["liquidation_auctions"][
                    "queued_slices"
                ](),
            )
        )
    )
    ret = inject(
        client,
        client.bulk(*op_batch).autofill(ttl=MAX_OPERATIONS_TTL).sign(),
    )

    profile = {
        "mark_for_liquidation": {
            "gas": [],
            "queue_size": [],
        }
    }
    for op in ret["contents"]:
        assert op["parameters"]["entrypoint"] == "mark_for_liquidation"
        profile["mark_for_liquidation"]["gas"].append(op["gas_limit"])
        profile["mark_for_liquidation"]["queue_size"].append(n)
        # Assuming that each request_liquidation operation adds a single slice to the queue
        # to avoid having to re-query the storage each time (which can take a long time).
        n += 1
    return profile


def merge_gas_profiles(profile1: Dict, profile2: Dict):
    merged = deepcopy(profile1)
    for name, data2 in profile2.items():
        data2: dict
        if name in profile1:
            data1 = profile1[name]
            for k in data2.keys():
                if k not in data1:
                    raise ValueError("Profiles had mismatching keys")
                merged[name][k] += data2[k]
        else:
            merged[name] = data2
    return merged


class LiquidationsStressTest(SandboxedTestCase):
    def test_liquidations(self):
        print("Deploying the mock oracle.")
        oracle = deploy_contract(
            self.client,
            source_file=os.path.join(PROJECT_ROOT, "util/mock_oracle.tz"),
            initial_storage=(self.client.key.public_key_hash(), 1000000),
            ttl=MAX_OPERATIONS_TTL,
        )

        print("Deploying ctez contract.")
        ctez = deploy_ctez(
            self.client,
            ctez_dir=os.path.join(PROJECT_ROOT, "vendor/ctez"),
            ttl=MAX_OPERATIONS_TTL,
        )

        print("Deploying Checker.")
        checker = deploy_checker(
            self.client,
            checker_dir=CHECKER_DIR,
            oracle=oracle.context.address,
            ctez=ctez["fa12_ctez"].context.address,
            ttl=MAX_OPERATIONS_TTL,
        )

        print("Deployment finished.")

        def call_endpoint(
            contract, name, param, amount=0, profiler=general_gas_profiler
        ):
            # Helper for calling contract endpoints with profiling
            print("Calling", contract.key.public_key_hash(), "/", name, "with", param)
            profile = profiler(
                self.client,
                contract,
                [
                    getattr(contract, name)(param)
                    .with_amount(amount)
                    .as_transaction()
                    .autofill(ttl=MAX_OPERATIONS_TTL)
                    .sign()
                ],
            )
            self.gas_profiles = merge_gas_profiles(self.gas_profiles, profile)

        def call_bulk(bulks, *, batch_size, profiler=general_gas_profiler):
            # Helper for calling operations in batches
            batches = [
                bulks[i : i + batch_size] for i in range(0, len(bulks), batch_size)
            ]
            for batch_no, batch in enumerate(batches, 1):
                print(
                    "Sending",
                    len(batches),
                    "operations as bulk:",
                    "Batch",
                    batch_no,
                    "of",
                    len(batches),
                )
                profile = profiler(self.client, checker, batch)
                self.gas_profiles = merge_gas_profiles(self.gas_profiles, profile)

        # Note: the amount of kit minted here and the kit in all other burrows for this account
        # must be enough to bid on the liquidation auction later in the test.
        call_bulk(
            [
                checker.create_burrow((0, None)).with_amount(200_000_000),
                checker.mint_kit((0, 80_000_000)),
            ],
            batch_size=10,
        )

        call_endpoint(
            ctez["ctez"], "create", (1, None, {"any": None}), amount=2_000_000
        )
        call_endpoint(ctez["ctez"], "mint_or_burn", (1, 100_000))
        call_endpoint(ctez["fa12_ctez"], "approve", (checker.context.address, 100_000))

        call_endpoint(
            checker,
            "add_liquidity",
            (100_000, 100_000, 5, int(datetime.now().timestamp()) + 20),
        )

        burrows = list(range(1, 1001))

        call_bulk(
            [
                checker.create_burrow((burrow_id, None)).with_amount(100_000_000)
                for burrow_id in burrows
            ],
            batch_size=100,
        )

        # Mint as much as possible from the burrows. All should be identical, so we just query the
        # first burrow and mint that much kit from all of them.
        max_mintable_kit = checker.metadata.burrowMaxMintableKit(
            (self.client.key.public_key_hash(), 1)
        ).storage_view()

        call_bulk(
            [checker.mint_kit(burrow_no, max_mintable_kit) for burrow_no in burrows],
            batch_size=100,
        )

        # Change the index (kits are 100x valuable)
        #
        # Keep in mind that we're using a patched checker on tests where the protected index
        # is much faster to update.
        call_endpoint(oracle, "update", 10_000_000)

        # Oracle updates lag one touch on checker
        call_endpoint(checker, "touch", None)
        call_endpoint(checker, "touch", None)
        time.sleep(20)
        call_endpoint(checker, "touch", None)

        # Now burrows should be overburrowed, so we liquidate them all.
        #
        # This should use the push_back method of the AVL tree.
        call_bulk(
            [
                checker.mark_for_liquidation(
                    (self.client.key.public_key_hash(), burrow_no)
                )
                for burrow_no in burrows
            ],
            batch_size=40,
            profiler=mark_for_liquidation_profiler,
        )

        # This touch starts a liquidation auction
        #
        # This would use the split method of the AVL tree. However, if the entire queue
        # has less tez than the limit, the 'split' method would trivially return the entire
        # tree without much effort. Here we ensure that the liquidation queue has
        # more tez than the limit.
        queued_tez = 0
        for _, leaf in auction_avl_leaves(
            checker,
            checker.storage["deployment_state"]["sealed"]["liquidation_auctions"][
                "queued_slices"
            ](),
        ):
            queued_tez += leaf["leaf"]["value"]["contents"]["tez"]
        assert (
            queued_tez > 10_000_000_000
        ), "queued tez in liquidation auction was not greater than Constants.max_lot_size which is required for this test"
        print(f"Auction queue currently has {queued_tez} mutez")
        call_endpoint(checker, "touch", None)

        # Now that there is an auction started in a descending state, we can select a queued slice
        # and be confident that we can cancel it before it gets added to another auction.
        # To successfully do this, we also need to either move the index price such that the
        # cancellation is warranted or deposit extra collateral to the burrow. Here we do the latter.
        # cancel_ops = []
        # for i, (queued_leaf_ptr, leaf) in enumerate(
        #     auction_avl_leaves(
        #         checker,
        #         checker.storage["deployment_state"]["sealed"]["liquidation_auctions"][
        #             "queued_slices"
        #         ](),
        #     )
        # ):
        #     cancel_ops.append(
        #         (
        #             checker.deposit_tez(
        #                 leaf["leaf"]["value"]["contents"]["burrow"][1]
        #             ).with_amount(10_000_000_000),
        #             checker.cancel_liquidation_slice(queued_leaf_ptr),
        #         )
        #     )
        #     if len(cancel_ops) >= 10:
        #         break
        # # Shuffle so we aren't only cancelling the oldest slice
        # shuffle(cancel_ops)
        # flattened_cancel_ops = []
        # for op1, op2 in cancel_ops:
        #     flattened_cancel_ops += [op1, op2]

        # call_bulk(
        #     flattened_cancel_ops,
        #     batch_size=10,
        # )

        # # # Change the index (kits are 0.5x valuable)
        # call_endpoint(oracle, "update", 500_000)
        # call_endpoint(checker, "touch", None)
        # call_endpoint(checker, "touch", None)
        # time.sleep(20)
        # call_endpoint(checker, "touch", None)

        # And we place a bid:
        # call_endpoint(checker, "touch", None)
        print("Sleeping to let auction decay. zzzz")
        print(checker.storage())
        time.sleep(1)
        print(checker.storage())

        ret = checker.metadata.currentLiquidationAuctionMinimumBid().storage_view()
        auction_id, minimum_bid = ret["contents"], ret["nat_1"]
        # The return value is supposed to be annotated as "auction_id" and "minimum_bid", I
        # do not know why we get these names. I think there is an underlying pytezos bug
        # that we should reproduce and create a bug upstream.

        # FIXME: Getting insufficient balance errors here
        # Note the auction ptr for later operations
        current_auctions_ptr = checker.storage["deployment_state"]["sealed"][
            "liquidation_auctions"
        ]["current_auction"]()["contents"]
        call_endpoint(
            checker, "liquidation_auction_place_bid", (auction_id, minimum_bid)
        )

        # Once max(max_bid_interval_in_blocks, max_bid_interval_in_seconds) has elapsed, the liquidation auction we
        # bid on should be complete. Here we go off of level since the patched version of Checker we expect to use
        # sets max_bid_interval_in_blocks=1 and max_bid_interval_in_seconds=1.
        # Sleeping a bit extra to add a bit of a buffer.
        time.sleep(10)

        call_endpoint(checker, "touch", None)
        assert (
            checker.storage["deployment_state"]["sealed"]["liquidation_auctions"][
                "completed_auctions"
            ]()
            is not None
        )
        # # Extra calls for profiling purposes
        # call_bulk(
        #     [checker.touch(None) for _ in range(5)],
        #     batch_size=2,
        # )

        # Before we can claim our winning bid, all of the slices in the completed auction must be touched
        auctioned_slices = []
        for leaf_ptr, leaf in auction_avl_leaves(checker, current_auctions_ptr):
            # Double check that we are only working with leaf ptrs
            assert "leaf" in leaf
            auctioned_slices.append(leaf_ptr)

        # Note: using smaller batches here to increase the number of samples for profiling.
        for i in range(0, len(auctioned_slices), 5):
            slices_to_cancel = auctioned_slices[i : i + 5]
            call_endpoint(checker, "touch_liquidation_slices", slices_to_cancel)

        # With all of the slices touched, we should now be able to claim our hard-earned winnings
        call_endpoint(checker, "liquidation_auction_claim_win", current_auctions_ptr)

        # Extra calls for profiling purposes
        call_bulk(
            [checker.touch(None) for _ in range(5)],
            batch_size=2,
        )


if __name__ == "__main__":
    unittest.main()
