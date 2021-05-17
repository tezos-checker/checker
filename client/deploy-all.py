import os
import logging

import pytezos

from checker import *

PROJECT_ROOT = os.path.join(os.path.dirname(__file__), "../")

tz = pytezos.pytezos.using(
  shell='http://127.0.0.1:20000',
  key="edsk3RFfvaFaxbHx8BMtEW1rKQcPtDML3LXjNqMNLCzC3wLC1bWbAt"
)
tz.loglevel = logging.WARN

print("Deploying the mock oracle.")
oracle = deploy_contract(
  tz,
  source_file=os.path.join(PROJECT_ROOT, "util/mock_oracle.tz"),
  initial_storage=(tz.key.public_key_hash(), 1000000),
)

print("Deploying ctez contract.")
ctez = deploy_ctez(
  tz,
  ctez_dir=os.path.join(PROJECT_ROOT, "vendor/ctez"),
)

print("Deploying Checker.")
checker = deploy_checker(
  tz,
  checker_dir=os.path.join(PROJECT_ROOT, "generated/michelson"),
  oracle=oracle.context.address,
  ctez=ctez["fa12_ctez"].context.address,
)
