import json
import os
from collections import OrderedDict
import unittest
from datetime import datetime

import portpicker
from pytezos.contract.interface import ContractInterface
from pytezos.operation import MAX_OPERATIONS_TTL

PROJECT_ROOT = os.path.join(os.path.dirname(__file__), "../")

from checker_client.checker import *


class SandboxedTestCase(unittest.TestCase):
    def setUp(self):
        port = portpicker.pick_unused_port()
        client, docker_client, docker_container = start_sandbox(
            "checker-e2e-container-{}".format(port), port
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
    # FIXME: Once https://github.com/baking-bad/pytezos/issues/233 is resolved we can
    # also use a view for this instead of accessing storage directly.
    try:
        kit_balance = checker.storage["deployment_state"]["sealed"]["fa2_state"][
            "ledger"
        ][(kit_token_id, address)]()
    except KeyError:
        kit_balance = 0
    assert expected_kit == kit_balance


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
            checker_dir=os.path.join(PROJECT_ROOT, "generated/michelson"),
            oracle=oracle.context.address,
            ctez=ctez["fa12_ctez"].context.address,
            num_blocks_wait=100,
            ttl=MAX_OPERATIONS_TTL,
        )

        print("Deployment finished.")
        gas_costs = {}

        def call_endpoint(contract, name, param, amount=0):
            ret = (
                getattr(contract, name)(param)
                .with_amount(amount)
                .as_transaction()
                .autofill(ttl=MAX_OPERATIONS_TTL)
                .sign()
                .inject(
                    min_confirmations=1,
                    num_blocks_wait=100,
                    max_iterations=WAIT_BLOCK_ATTEMPTS,
                    delay_sec=WAIT_BLOCK_DELAY,
                )
            )
            return ret

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
        # ===============================================================================
        # Liquidation Auctions
        # ===============================================================================
        # TODO: Trigger liquidation auction and call endpoints:
        # * mark_for_liquidation
        # * cancel_liquidation_slice
        # * liquidation_auction_claim_win
        # * liquidation_auction_place_bid
        # * touch_liquidation_slices
        # ===============================================================================

        print("Gas costs:")
        gas_costs_sorted = OrderedDict()
        for k, v in sorted(
            gas_costs.items(), key=lambda tup: int(tup[1]), reverse=True
        ):
            gas_costs_sorted[k] = v
        print(json.dumps(gas_costs_sorted, indent=4))


if __name__ == "__main__":
    unittest.main()
