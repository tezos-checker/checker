import os
import sys
import time
import logging
import unittest
import argparse

import pytezos
from pytezos.sandbox.node import SandboxedNodeTestCase

PROJECT_ROOT = os.path.join(os.path.dirname(__file__), "../")

sys.path.append(os.path.join(PROJECT_ROOT, "client"))
from checker import *

class E2ETest(SandboxedNodeTestCase):
    def test_e2e(self):
        self.client.loglevel = logging.WARN

        print("Deploying ctez contract.")
        ctez = deploy_ctez(
          self.client,
          ctez_dir=os.path.join(PROJECT_ROOT, "vendor/ctez"),
          bake=self.bake_block
        )

        print("Deploying the mock oracle.")
        oracle = deploy_contract(
          self.client,
          source_file=os.path.join(PROJECT_ROOT, "util/mock_oracle.tz"),
          initial_storage=(self.client.key.public_key_hash(), 1000000),
          bake=self.bake_block
        )

        print("Deploying Checker.")
        checker = deploy_checker(
          self.client,
          checker_dir=os.path.join(PROJECT_ROOT, "generated/michelson"),
          oracle=oracle.context.address,
          ctez=ctez["fa12_ctez"].context.address,
          bake=self.bake_block
        )

        print("Deployment finished.")

        checker.create_burrow((1, None)).with_amount(10_000_000).inject()
        checker.mint_kit((1, 1_000_000)).inject()

if __name__ == '__main__':
    unittest.main()
