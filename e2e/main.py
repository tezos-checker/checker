import os
import json
import time
from collections import OrderedDict
from typing import Dict, Generator, Tuple
import unittest
from datetime import datetime

import portpicker
from pytezos.contract.interface import ContractInterface
from pytezos.operation import MAX_OPERATIONS_TTL

PROJECT_ROOT = os.path.join(os.path.dirname(__file__), "../")
CHECKER_DIR = os.getenv(
    "CHECKER_DIR", default=os.path.join(PROJECT_ROOT, "generated/michelson")
)
WRITE_GAS_COSTS = os.getenv("WRITE_GAS_COSTS")

from checker_client.checker import *


class SandboxedTestCase(unittest.TestCase):
    def setUp(self):
        port = portpicker.pick_unused_port()
        client, docker_client, docker_container = start_sandbox(
            "checker-e2e-container-{}".format(port), port, wait_for_level=2
        )
        self.docker_client = docker_client
        self.docker_container = docker_container
        self.client = client

    def tearDown(self):
        self.docker_container.kill()
        self.docker_client.close()


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


class E2ETest(SandboxedTestCase):
    def test_e2e(self):
        gas_costs = {}
        account = self.client.key.public_key_hash()
        # ===============================================================================
        # Deploy contracts
        # ===============================================================================
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
        gas_costs = {}

        def call_endpoint(contract, name, param, amount=0):
            return inject(
                self.client,
                getattr(contract, name)(param)
                .with_amount(amount)
                .as_transaction()
                .autofill(ttl=MAX_OPERATIONS_TTL)
                .sign(),
            )

        def call_checker_endpoint(name, param, amount=0):
            print("Calling", name, "with", param)
            ret = call_endpoint(checker, name, param, amount)
            gas_costs[name] = ret["contents"][0]["gas_limit"]
            return ret

        # ===============================================================================
        # Burrows
        # ===============================================================================
        # Create a burrow
        call_checker_endpoint("create_burrow", (1, None), amount=10_000_000)
        assert_kit_balance(checker, account, 0)

        # Get some kit
        call_checker_endpoint("mint_kit", (1, 1_000_000))
        assert_kit_balance(checker, account, 1_000_000)

        # Deposit tez
        call_checker_endpoint("deposit_tez", 1, amount=2_000_000)

        # Withdraw tez
        call_checker_endpoint("withdraw_tez", (2_000_000, 1))

        # Set delegate
        call_checker_endpoint("set_burrow_delegate", (1, account))

        # Deactivate a burrow
        call_checker_endpoint("create_burrow", (2, None), amount=3_000_000)
        call_checker_endpoint("deactivate_burrow", (2, account))

        # Touch checker
        call_checker_endpoint("touch", None)

        # Touch burrow
        call_checker_endpoint("touch_burrow", (account, 1))

        # ===============================================================================
        # CFMM
        # ===============================================================================
        # Get some ctez
        call_endpoint(
            ctez["ctez"], "create", (1, None, {"any": None}), amount=1_000_000
        )
        call_endpoint(ctez["ctez"], "mint_or_burn", (1, 800_000))
        # Approve checker to spend the ctez
        call_endpoint(ctez["fa12_ctez"], "approve", (checker.context.address, 800_000))

        call_checker_endpoint(
            "add_liquidity", (400_000, 400_000, 5, int(datetime.now().timestamp()) + 20)
        )
        # Note: not adding FA2 balance assertions here since the amount of kit depends on the price
        # and adding an assertion might make the test flaky
        call_checker_endpoint("buy_kit", (10, 5, int(datetime.now().timestamp()) + 20))
        call_checker_endpoint("sell_kit", (5, 1, int(datetime.now().timestamp()) + 20))

        call_checker_endpoint(
            "remove_liquidity", (5, 1, 1, int(datetime.now().timestamp()) + 20)
        )

        print("Gas costs:")
        gas_costs_sorted = OrderedDict()
        for k, v in sorted(
            gas_costs.items(), key=lambda tup: int(tup[1]), reverse=True
        ):
            gas_costs_sorted[k] = v

        print(json.dumps(gas_costs_sorted, indent=4))
        if WRITE_GAS_COSTS:
            with open(WRITE_GAS_COSTS, "w") as f:
                json.dump(gas_costs_sorted, f, indent=4)


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

        def call_endpoint(contract, name, param, amount=0):
            print("Calling", contract.key.public_key_hash(), "/", name, "with", param)
            return inject(
                self.client,
                getattr(contract, name)(param)
                .with_amount(amount)
                .as_transaction()
                .autofill(ttl=MAX_OPERATIONS_TTL)
                .sign(),
            )

        def call_bulk(bulks, *, batch_size):
            # FIXME: Add execution time metrics for batches to help detect degenerating efficiency
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
                inject(
                    self.client,
                    self.client.bulk(*batch).autofill(ttl=MAX_OPERATIONS_TTL).sign(),
                )

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
        call_endpoint(oracle, "update", 100_000_000)

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

        call_endpoint(checker, "touch", None)

        # Now that there is an auction started in a descending state, we can select a queued slice
        # and be confident that we can cancel it before it gets added to another auction.
        # To successfully do this, we also need to either move the index price such that the
        # cancellation is warranted or deposit extra collateral to the burrow. Here we do the latter.
        for queued_leaf_ptr, leaf in auction_avl_leaves(
            checker,
            checker.storage["deployment_state"]["sealed"]["liquidation_auctions"][
                "queued_slices"
            ](),
        ):
            # Only retrieve the first queued slice ptr for efficiency
            break
        # Deposit a ton of tez to this burrow to ensure that it is no longer liquidatable
        call_endpoint(
            checker,
            "deposit_tez",
            leaf["leaf"]["value"]["contents"]["burrow"][1],
            amount=100_000_000_000,
        )
        call_endpoint(checker, "cancel_liquidation_slice", queued_leaf_ptr)

        # And we place a bid:

        ret = checker.metadata.currentLiquidationAuctionMinimumBid().storage_view()
        auction_id, minimum_bid = ret["contents"], ret["nat_1"]
        # The return value is supposed to be annotated as "auction_id" and "minimum_bid", I
        # do not know why we get these names. I think there is an underlying pytezos bug
        # that we should reproduce and create a bug upstream.

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

        # Before we can claim our winning bid, all of the slices in the completed auction must be touched
        auctioned_slices = []
        for leaf_ptr, leaf in auction_avl_leaves(checker, current_auctions_ptr):
            # Double check that we are only working with leaf ptrs
            assert "leaf" in leaf
            auctioned_slices.append(leaf_ptr)

        # Note: batching here based on empirical estimates of how many slices we can
        # touch in one go before hitting the gas limit.
        for i in range(0, len(auctioned_slices), 10):
            slices_to_cancel = auctioned_slices[i : i + 10]
            call_endpoint(checker, "touch_liquidation_slices", slices_to_cancel)

        # With all of the slices touched, we should now be able to claim our hard-earned winnings
        call_endpoint(checker, "liquidation_auction_claim_win", current_auctions_ptr)


if __name__ == "__main__":
    unittest.main()
