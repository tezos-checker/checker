import os
import json
import time
import logging
import unittest
from pprint import pprint
from datetime import datetime

import docker
import portpicker
import pytezos
import requests
from pytezos.operation import MAX_OPERATIONS_TTL

PROJECT_ROOT = os.path.join(os.path.dirname(__file__), "../")

from checker_client.checker import *


class SandboxedTestCase(unittest.TestCase):
    def setUp(self):
        port = portpicker.pick_unused_port()
        client, docker_client, docker_container = start_sandbox("checker-e2e-container-{}".format(port), port)
        self.docker_container = docker_container
        self.client = client

    def tearDown(self):
        self.docker_container.kill()

class E2ETest(SandboxedTestCase):
    def test_e2e(self):
        gas_costs = {}

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
                   delay_sec=WAIT_BLOCK_DELAY
                )
            )
            return ret

        def call_checker_endpoint(name, param, amount=0):
            ret = call_endpoint(checker, name, param, amount)
            gas_costs[name] = ret["contents"][0]["gas_limit"]
            return ret


        # get some kit
        call_checker_endpoint("create_burrow", (1, None), amount=10_000_000)
        call_checker_endpoint("mint_kit", (1, 1_000_000))
        call_checker_endpoint("touch", None)

        # get some ctez
        call_endpoint(ctez["ctez"], "create", (1, None, { "any": None }), amount=1_000_000)
        call_endpoint(ctez["ctez"], "mint_or_burn", (1, 800_000))

        # approve checker to spend the ctez
        call_endpoint(ctez["fa12_ctez"], "approve", (checker.context.address, 800_000))

        # TODO add liquidity to checker
        call_checker_endpoint("add_liquidity", (400_000, 400_000, 5, int(datetime.now().timestamp()) + 20))

        # TODO use uniswap
        call_checker_endpoint("buy_kit", (10, 5, int(datetime.now().timestamp()) + 20))

        print("Gas costs:")
        print(json.dumps(gas_costs, indent=4))


if __name__ == "__main__":
    unittest.main()
