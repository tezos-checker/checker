import logging
import os
import time
import unittest

import docker
import portpicker
import pytezos
import requests

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
        )

        print("Deployment finished.")

        (
            checker.create_burrow((1, None))
            .with_amount(10_000_000)
            .as_transaction()
            .autofill(branch_offset=1)
            .sign()
            .inject(min_confirmations=1, time_between_blocks=3)
        )

        (
            checker.mint_kit((1, 1_000_000))
            .as_transaction()
            .autofill(branch_offset=1)
            .sign()
            .inject(min_confirmations=1, time_between_blocks=3)
        )


if __name__ == "__main__":
    unittest.main()
