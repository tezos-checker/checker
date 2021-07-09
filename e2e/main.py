import os
import json
import time
from collections import OrderedDict
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


# class E2ETest(SandboxedTestCase):
#     def test_e2e(self):
#         gas_costs = {}
#         account = self.client.key.public_key_hash()
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

#         def call_endpoint(contract, name, param, amount=0):
#             return inject(
#                 self.client,
#                 getattr(contract, name)(param)
#                 .with_amount(amount)
#                 .as_transaction()
#                 .autofill(ttl=MAX_OPERATIONS_TTL)
#                 .sign(),
#             )

#         def call_checker_endpoint(name, param, amount=0):
#             print("Calling", name, "with", param)
#             ret = call_endpoint(checker, name, param, amount)
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

#         # Deposit tez
#         call_checker_endpoint("deposit_tez", 1, amount=2_000_000)

#         # Withdraw tez
#         call_checker_endpoint("withdraw_tez", (2_000_000, 1))

#         # Set delegate
#         call_checker_endpoint("set_burrow_delegate", (1, account))

#         # Deactivate a burrow
#         call_checker_endpoint("create_burrow", (2, None), amount=3_000_000)
#         call_checker_endpoint("deactivate_burrow", (2, account))

#         # Touch checker
#         call_checker_endpoint("touch", None)

#         # Touch burrow
#         call_checker_endpoint("touch_burrow", (account, 1))

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
#         # ===============================================================================
#         # Liquidation Auctions
#         # ===============================================================================
#         # TODO: Trigger liquidation auction and call endpoints:
#         # * mark_for_liquidation
#         # * cancel_liquidation_slice
#         # * liquidation_auction_claim_win
#         # * liquidation_auction_place_bid
#         # * touch_liquidation_slices
#         # ===============================================================================

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
        # tree without much effort; so here we should ensure that the liquidation queue has
        # more tez than the limit (FIXME).
        call_endpoint(checker, "touch", None)

        # And we place a bid

        ret = checker.metadata.currentLiquidationAuctionMinimumBid().storage_view()
        auction_id, minimum_bid = ret["contents"], ret["nat_1"]
        # The return value is supposed to be annotated as "auction_id" and "minimum_bid", I
        # do not know why we get these names. I think there is an underlying pytezos bug
        # that we should reproduce and create a bug upstream.

        call_endpoint(
            checker, "liquidation_auction_place_bid", (auction_id, minimum_bid)
        )


if __name__ == "__main__":
    unittest.main()
