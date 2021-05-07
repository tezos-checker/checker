import logging
import argparse

import pytezos

from lib import *

tz = pytezos.pytezos.using(
  shell='http://127.0.0.1:20000',
  key="edsk3RFfvaFaxbHx8BMtEW1rKQcPtDML3LXjNqMNLCzC3wLC1bWbAt"
)
tz.loglevel = logging.WARN

print("Deploying the mock oracle.")
oracle = deploy_contract(
  tz,
  source_file="../util/mock_oracle.tz",
  initial_storage=(tz.key.public_key_hash(), 1000000)
)

print("Deploying Checker.")
checker = deploy_checker(
  tz,
  checker_dir="../generated/michelson",
  oracle=oracle.context.address,
  ctez="bar"
)

print(checker)

print("done")
