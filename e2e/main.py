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
        self.docker_client = docker.from_env()

        port = portpicker.pick_unused_port()
        self.docker_container = self.docker_client.containers.run(
            "tqtezos/flextesa:20210316",
            command=["edobox", "start"],
            environment={"block_time": 1},
            ports={"20000/tcp": port},
            name="checker-e2e-container-{}".format(port),
            detach=True,
            remove=True,
        )

        self.client = pytezos.pytezos.using(
            shell="http://127.0.0.1:{}".format(port),
            key="edsk3RFfvaFaxbHx8BMtEW1rKQcPtDML3LXjNqMNLCzC3wLC1bWbAt",  # bob's key from "edobox info"
        )
        self.client.loglevel = logging.ERROR

        # wait some time for the node to start
        while True:
            try:
                self.client.shell.node.get("/version/")
                break
            except requests.exceptions.ConnectionError:
                time.sleep(0.1)
        # wait until a block is mined
        time.sleep(3)

    def tearDown(self):
        self.docker_container.kill()


class E2ETest(SandboxedTestCase):
    def test_e2e(self):
        print("Deploying the mock oracle.")
        oracle = deploy_contract(
            self.client,
            source_file=os.path.join(PROJECT_ROOT, "util/mock_oracle.tz"),
            initial_storage=(self.client.key.public_key_hash(), 1000000),
        )

        print("Deploying ctez contract.")
        ctez = deploy_ctez(
            self.client,
            ctez_dir=os.path.join(PROJECT_ROOT, "vendor/ctez"),
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
