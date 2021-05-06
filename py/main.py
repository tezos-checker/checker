import argparse

from pytezos import pytezos

pytezos = pytezos.using(
  shell='http://127.0.0.1:20000',
  key="edsk3RFfvaFaxbHx8BMtEW1rKQcPtDML3LXjNqMNLCzC3wLC1bWbAt"
)

print("Finding checker contract")
checker = pytezos.contract("KT1XLhXjUSdgGZ4jXNRFeH9rMMbxQeH3JGzK")

print("Touching")
checker.touch().inject()
