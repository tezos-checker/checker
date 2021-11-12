import json
import os
import time
import unittest
from collections import OrderedDict
from copy import deepcopy
from datetime import datetime
from random import shuffle
from typing import Callable, Dict, Generator, Tuple

import portpicker
from checker_client.checker import *
from checker_builder.config import load_checker_config
from pytezos.contract.interface import ContractInterface
from pytezos.operation import MAX_OPERATIONS_TTL
from pytezos.operation.group import OperationGroup

PROJECT_ROOT = os.path.join(os.path.dirname(__file__), "../")
CHECKER_DIR = os.getenv(
    "CHECKER_DIR", default=os.path.join(PROJECT_ROOT, "generated/michelson")
)
# Optional file at which to output gas costs in end to end tests
WRITE_GAS_COSTS = os.getenv("WRITE_GAS_COSTS")
# Optional file at which to output profiles of gas costs from liquidation auction stress
# tests
WRITE_GAS_PROFILES = os.getenv("WRITE_GAS_PROFILES")


class SandboxedTestCase(unittest.TestCase):
    def setUp(self):
        self.config = load_checker_config()
        #  sometimes doesn't work, needs investigation:
        #    port = portpicker.pick_unused_port()
        port = 20000
        client, teardownFun = start_sandbox(
            "checker-e2e-container-{}".format(port), port, wait_for_level=2
        )
        self.teardownFun = teardownFun
        self.client = client
        self.gas_profiles = {}

    def tearDown(self):
        self.teardownFun()


def assert_kit_balance(checker: ContractInterface, address: str, expected_kit: int):
    # TODO: There might be a way to get this from contract metadata
    kit_token_id = 0
    kit_balance = checker.metadata.getBalance((address, kit_token_id)).storage_view()
    if expected_kit != kit_balance:
        raise AssertionError(f"Expected {expected_kit} but got {kit_balance}")


def assert_fa2_token_balance(
    wrapper: ContractInterface,
    address: str,
    token_id: int,
    expected: int,
):
    fa2_balance_of = {
        "requests": [{"owner": address, "token_id": token_id}],
        "callback": None,
    }
    balance = wrapper.balance_of(**fa2_balance_of).callback_view()[0]["nat_2"]
    if expected != balance:
        raise AssertionError(f"Expected {expected} but got {balance}")


def assert_fa12_token_balance(
    contract: ContractInterface,
    address: str,
    expected: int,
):
    fa12_get_balance = {"owner": address, "callback": None}
    balance = contract.getBalance(**fa12_get_balance).callback_view()
    if expected != balance:
        raise AssertionError(f"Expected {expected} but got {balance}")


def avl_storage(checker: ContractInterface, ptr: int) -> Dict:
    """Reads an item from checker's AVL backend using its pointer"""
    return checker.storage["deployment_state"]["sealed"]["liquidation_auctions"][
        "avl_storage"
    ]["mem"][ptr]()


AvlPtr = int
AvlNode = Dict
AvlLeaves = Generator[Tuple[AvlPtr, AvlNode], None, List]
Operation = Dict


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


# A Profiler is a helper function for wrapping a call to a contract with logic
# for extracting gas cost and other related information.
Profiler: Callable[[PyTezosClient, ContractInterface, List[OperationGroup]], Dict]


def bulk(client: PyTezosClient, *operations) -> OperationGroup:
    """Combines a list of operation groups / contract calls into a single OperationGroup

    **Only for use within this test suite. Not for general consumption.**

    Combines operations using PyTezosClient.bulk() while also setting their gas
    limits using the current protocol hard gas limits and the number of input operations.
    This is necessary because the default approach of passing hard_gas_limit_per_operation
    to the interpreter in order to get a real gas_limit estimate fails in cases where
    hard_gas_limit_per_operation * len(operations) > hard_gas_limit_per_block.

    This method only works for bulk operations which have approximately uniform gas costs.
    Use with caution.

    Args:
        client:
        *operations: An arbitrary number of

    Returns:
        OperationGroup: [description]
    """
    combined_ops = client.bulk(*operations)
    max_block_gas = int(client.context.constants["hard_gas_limit_per_block"])
    max_mean_op_gas = str(
        min(
            int(client.context.constants["hard_gas_limit_per_operation"]),
            (max_block_gas // len(operations)),
        )
    )
    # Since PytezosClient.bulk() resets the gas limits, we need to explicitly set them.
    for op in combined_ops.contents:
        op["gas_limit"] = max_mean_op_gas
    return combined_ops


def general_gas_profiler(
    client: PyTezosClient, checker: ContractInterface, op_batch: List[OperationGroup]
) -> Dict:
    """Calls inject for a list of entrypoint calls, recording the gas cost of each call."""
    ret = inject(
        client,
        bulk(client, *op_batch).autofill(ttl=MAX_OPERATIONS_TTL).sign(),
    )
    profile = {}
    for op in ret["contents"]:
        name = op["parameters"]["entrypoint"]
        if name not in profile:
            profile[name] = {"gas": []}
        profile[name]["gas"].append(op["gas_limit"])
    return profile


def mark_for_liquidation_profiler(
    client: PyTezosClient, checker: ContractInterface, op_batch: List[OperationGroup]
) -> Dict:
    """
    Calls inject for a list of `mark_for_liquidation` calls, recording
    the gas cost and current liquidation queue size for each call.
    """
    n = len(
        list(
            auction_avl_leaves(
                checker,
                checker.storage["deployment_state"]["sealed"]["liquidation_auctions"][
                    "queued_slices"
                ](),
            )
        )
    )
    ret = inject(
        client,
        bulk(client, *op_batch).autofill(ttl=MAX_OPERATIONS_TTL).sign(),
    )

    profile = {
        "mark_for_liquidation": {
            "gas": [],
            "queue_size": [],
        }
    }
    for op in ret["contents"]:
        assert op["parameters"]["entrypoint"] == "mark_for_liquidation"
        profile["mark_for_liquidation"]["gas"].append(op["gas_limit"])
        profile["mark_for_liquidation"]["queue_size"].append(n)
        # Assuming that each request_liquidation operation adds a single slice to the queue
        # to avoid having to re-query the storage each time (which can take a long time).
        n += 1
    return profile


def cancel_liquidation_slice_profiler(
    client: PyTezosClient, checker: ContractInterface, op_batch: List[OperationGroup]
) -> Dict:
    """
    Calls inject for a list of `cancel_liquidation_slice` calls, recording
    the gas cost and current liquidation queue size for each call.
    """
    n = len(
        list(
            auction_avl_leaves(
                checker,
                checker.storage["deployment_state"]["sealed"]["liquidation_auctions"][
                    "queued_slices"
                ](),
            )
        )
    )
    ret = inject(
        client,
        bulk(client, *op_batch).autofill(ttl=MAX_OPERATIONS_TTL).sign(),
    )

    profile = {
        "cancel_liquidation_slice": {
            "gas": [],
            "queue_size": [],
        }
    }
    for op in ret["contents"]:
        # Ignore other operations (e.g. deposit_collateral)
        if op["parameters"]["entrypoint"] != "cancel_liquidation_slice":
            continue
        profile["cancel_liquidation_slice"]["gas"].append(op["gas_limit"])
        profile["cancel_liquidation_slice"]["queue_size"].append(n)
        # Assuming that each cancel_liquidation_slice removes a single slice from
        # the queue to avoid having to re-query the storage each time (which can take a long time).
        # NOTE: This assumption does not always hold (e.g. for Complete liquidations). If we
        # want to use this profiler in more general places we will need to update the queue_size
        # estimation logic here.
        n -= 1
    return profile


def merge_gas_profiles(profile1: Dict, profile2: Dict) -> Dict:
    """Merges two gas profile dictionaries

    In case of conflicting keys, entries from profile2 will be appended to
    entries under the same key in profile1.
    """
    merged = deepcopy(profile1)
    for name, data2 in profile2.items():
        data2: dict
        if name in profile1:
            data1 = profile1[name]
            for k in data2.keys():
                if k not in data1:
                    raise ValueError("Profiles had mismatching keys")
                merged[name][k] += data2[k]
        else:
            merged[name] = data2
    return merged


def write_gas_costs(gas_costs: Dict[str, int], output_path: str) -> None:
    """
    Writes the sorted contents of the provided gas costs dictionary to the specified file.
    If a file already exists at output_path the provided input will be merged with it,
    with the function input taking precedence in cases of conflicting keys.
    """
    if os.path.exists(output_path):
        with open(output_path) as f:
            existing_data = json.load(f)
    else:
        existing_data = {}
    # Favor the input over the existing data
    existing_data.update(gas_costs)

    gas_costs_sorted = OrderedDict()
    for k, v in sorted(
        existing_data.items(), key=lambda tup: int(tup[1]), reverse=True
    ):
        gas_costs_sorted[k] = v

    with open(output_path, "w") as f:
        json.dump(gas_costs_sorted, f, indent=4)


class E2ETest(SandboxedTestCase):
    def test_e2e(self):
        gas_costs = {}
        account = self.client.key.public_key_hash()
        kit_token_id = self.config.tokens.issued.kit.token_id
        collateral_token_id = self.config.tokens.in_use.collateral.token_id
        # Hard-coded in contract, so also hard-coding here
        wctez_token_id = 3
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

        print("Deploying the tez wrapper.")
        tez_wrapper = deploy_tez_wrapper(
            self.client,
            checker_dir=CHECKER_DIR,
            ttl=MAX_OPERATIONS_TTL,
        )

        print("Deploying ctez contract.")
        ctez = deploy_ctez(
            self.client,
            ctez_dir=os.path.join(PROJECT_ROOT, "vendor/ctez"),
            ttl=MAX_OPERATIONS_TTL,
        )

        print("Deploying wctez contract.")
        wctez = deploy_wctez(
            self.client,
            checker_dir=CHECKER_DIR,
            ctez_fa12_address=ctez["fa12_ctez"].context.address,
            ttl=MAX_OPERATIONS_TTL,
        )

        print("Deploying Checker.")
        checker = deploy_checker(
            self.client,
            checker_dir=CHECKER_DIR,
            oracle=oracle.context.address,
            tez_wrapper=tez_wrapper.context.address,
            ctez_fa12=ctez["fa12_ctez"].context.address,
            ctez_cfmm=ctez["cfmm"].context.address,
            wctez=wctez.context.address,
            ttl=MAX_OPERATIONS_TTL,
        )

        print("Deployment finished.")
        gas_costs = {}

        def call_endpoint(contract, name, param, amount=0, client=self.client):
            return inject(
                client,
                getattr(contract, name)(param)
                .with_amount(amount)
                .as_transaction()
                .autofill(ttl=MAX_OPERATIONS_TTL)
                .sign(),
            )

        def call_checker_endpoint(
            name, param, amount=0, contract=checker, client=self.client
        ):
            print("Calling", name, "with", param)
            ret = call_endpoint(contract, name, param, amount, client=client)
            gas_costs[f"checker%{name}"] = int(ret["contents"][0]["gas_limit"])
            return ret

        def call_wctez_endpoint(
            name, param, amount=0, contract=wctez, client=self.client
        ):
            print("Calling", name, "with", param)
            ret = call_endpoint(contract, name, param, amount, client=client)
            gas_costs[f"wctez%{name}"] = int(ret["contents"][0]["gas_limit"])
            return ret

        def get_tez_tokens_and_make_checker_an_operator(amnt):
            call_endpoint(tez_wrapper, "deposit", None, amount=amnt)
            update_operators = [
                {
                    "add_operator": {
                        "owner": account,
                        "operator": checker.context.address,
                        "token_id": collateral_token_id,
                    }
                },
            ]
            call_endpoint(tez_wrapper, "update_operators", update_operators)

        # ===============================================================================
        # Burrows
        # ===============================================================================
        # Create a burrow
        get_tez_tokens_and_make_checker_an_operator(10_000_000)
        call_checker_endpoint("create_burrow", (1, None, 10_000_000))
        assert_kit_balance(checker, account, 0)

        # Get some kit
        call_checker_endpoint("mint_kit", (1, 1_000_000))
        assert_kit_balance(checker, account, 1_000_000)

        # Burn some kit
        call_checker_endpoint("burn_kit", (1, 10))
        assert_kit_balance(checker, account, 999_990)

        # Deposit tez
        get_tez_tokens_and_make_checker_an_operator(2_000_000)
        call_checker_endpoint("deposit_collateral", (1, 2_000_000))

        # Withdraw tez
        call_checker_endpoint("withdraw_collateral", (1, 2_000_000))

        # Set delegate
        call_checker_endpoint("set_burrow_delegate", (1, account))

        # Deactivate a burrow
        get_tez_tokens_and_make_checker_an_operator(3_000_000)
        call_checker_endpoint("create_burrow", (2, None, 3_000_000))
        call_checker_endpoint("deactivate_burrow", (2, account))

        # Re-activate a burrow
        get_tez_tokens_and_make_checker_an_operator(1_000_000)
        call_checker_endpoint("activate_burrow", (2, 1_000_000))

        # Touch checker
        call_checker_endpoint("touch", None)

        # Touch burrow
        call_checker_endpoint("touch_burrow", (account, 1))

        # ===============================================================================
        # FA2 interface
        # ===============================================================================
        # Note: using alice's account which is assumed to be already initialized in the sandbox
        checker_alice = pytezos.pytezos.using(
            shell=checker.shell,
            key="edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq",
        ).contract(checker.address)

        # Transfer from the main test account to alice's account
        fa2_transfer = {
            "from_": account,
            "txs": [
                {
                    "to_": checker_alice.key.public_key_hash(),
                    "token_id": kit_token_id,
                    "amount": 90,
                },
            ],
        }
        call_checker_endpoint("transfer", [fa2_transfer])
        assert_kit_balance(checker, checker_alice.key.public_key_hash(), 90)

        # Add the main account as an operator on alice's account
        # Note: using a client instance with alice's key for this since she is the
        # account owner.
        update_operators = [
            {
                "add_operator": {
                    "owner": checker_alice.key.public_key_hash(),
                    "operator": account,
                    "token_id": kit_token_id,
                }
            },
        ]
        call_checker_endpoint(
            "update_operators",
            update_operators,
            client=checker_alice,
            contract=checker_alice,
        )

        # Send some kit back to the main test account
        fa2_transfer = {
            "from_": checker_alice.key.public_key_hash(),
            "txs": [
                {
                    "to_": account,
                    "token_id": kit_token_id,
                    "amount": 80,
                },
            ],
        }
        call_checker_endpoint("transfer", [fa2_transfer])
        assert_kit_balance(checker, checker_alice.key.public_key_hash(), 10)

        # `balance_of` requires a contract callback when executing on-chain. To make tests
        # more light-weight and avoid needing an additional mock contract, we call it as a view.
        # This comes with a downside, however, since using a view means that we don't get an
        # estimate of the gas cost.
        fa2_balance_of = {
            "requests": [
                {"owner": checker_alice.key.public_key_hash(), "token_id": kit_token_id}
            ],
            "callback": None,
        }
        print(f"Calling balance_of as an off-chain view with {fa2_balance_of}")
        balance = checker.balance_of(**fa2_balance_of).callback_view()
        # Check that this balance agrees with the kit balance view
        assert_kit_balance(
            checker, checker_alice.key.public_key_hash(), balance[0]["nat_2"]
        )

        # ===============================================================================
        # CFMM
        # ===============================================================================
        # Get some ctez (create an oven and mint some ctez)
        call_endpoint(
            ctez["ctez"], "create", (1, None, {"any": None}), amount=1_000_000
        )
        call_endpoint(ctez["ctez"], "mint_or_burn", (1, 800_000))

        # Get some wctez (approve wctez to move the ctez and mint some wctez)
        call_endpoint(ctez["fa12_ctez"], "approve", (wctez.context.address, 800_000))
        call_wctez_endpoint("mint", 800_000)

        # Approve checker to spend the wctez
        update_operators = [
            {
                "add_operator": {
                    "owner": account,
                    "operator": checker.context.address,
                    "token_id": wctez_token_id,
                }
            },
        ]
        call_wctez_endpoint("update_operators", update_operators)

        # Add some liquidity
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
        print(json.dumps(gas_costs, indent=4))
        if WRITE_GAS_COSTS:
            write_gas_costs(gas_costs, WRITE_GAS_COSTS)


class TezWrapperTest(SandboxedTestCase):
    def test_e2e(self):
        gas_costs = {}
        collateral_token_id = self.config.tokens.in_use.collateral.token_id

        wrapper = deploy_tez_wrapper(
            self.client,
            checker_dir=CHECKER_DIR,
            ttl=MAX_OPERATIONS_TTL,
        )
        print("Deployment finished.")

        account = self.client.key.public_key_hash()
        # Note: using alice's account which is assumed to be already initialized in the sandbox
        wrapper_alice = pytezos.pytezos.using(
            shell=wrapper.shell,
            key="edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq",
        ).contract(wrapper.address)
        account_alice = wrapper_alice.key.public_key_hash()

        def call_endpoint(name, param, amount=0, client=self.client, contract=wrapper):
            print(f"Calling {name} with {param} and mutez={amount}")
            ret = inject(
                client,
                getattr(contract, name)(param)
                .with_amount(amount)
                .as_transaction()
                .autofill(ttl=MAX_OPERATIONS_TTL)
                .sign(),
            )
            gas_costs[f"tezWrapper%{name}"] = int(ret["contents"][0]["gas_limit"])
            return ret

        def single_fa2_transfer(
            sender: str, recipient: str, amount: int, token_id=collateral_token_id
        ):
            return [
                {
                    "from_": sender,
                    "txs": [
                        {
                            "to_": recipient,
                            "token_id": token_id,
                            "amount": amount,
                        },
                    ],
                }
            ]

        # Edge case: this call should succeed, according to the FA2 spec. It
        # must come first: by trying to transfer zero tokens before either the
        # source or the target account is created, we ensure that
        # TezWrapper.transfer does not fail due to non-originated vault
        # contracts.
        call_endpoint("transfer", single_fa2_transfer(account, account_alice, 0))

        # ===============================================================================
        # Wrapper-specific entrypoints
        # ===============================================================================
        # Deposit some tez into the test account's vault
        call_endpoint("deposit", None, amount=2_000_000)
        assert_fa2_token_balance(wrapper, account, collateral_token_id, 2_000_000)
        # Withdraw some tez from the test account's vault
        call_endpoint("withdraw", 100)
        assert_fa2_token_balance(wrapper, account, collateral_token_id, 1_999_900)
        # Set test account vault's delegate
        call_endpoint("set_delegate", None)

        # ===============================================================================
        # FA2 interface
        # ===============================================================================
        # Transfer from the test account to alice's account
        call_endpoint("transfer", single_fa2_transfer(account, account_alice, 90))
        assert_fa2_token_balance(wrapper, account, collateral_token_id, 1_999_810)
        assert_fa2_token_balance(wrapper, account_alice, collateral_token_id, 90)
        # Add the main account as an operator on alice's account
        # Note: using a client instance with alice's key for this since she is the
        # account owner.
        update_operators = [
            {
                "add_operator": {
                    "owner": wrapper_alice.key.public_key_hash(),
                    "operator": account,
                    "token_id": collateral_token_id,
                }
            },
        ]
        call_endpoint(
            "update_operators",
            update_operators,
            client=wrapper_alice,
            contract=wrapper_alice,
        )

        # Send some tez tokens back to the main test account
        call_endpoint("transfer", single_fa2_transfer(account_alice, account, 80))
        assert_fa2_token_balance(wrapper, account, collateral_token_id, 1_999_890)
        # `balance_of` requires a contract callback when executing on-chain. To make tests
        # more light-weight and avoid needing an additional mock contract, we call it as a view.
        # This comes with a downside, however, since using a view means that we don't get an
        # estimate of the gas cost.
        fa2_balance_of = {
            "requests": [
                {
                    "owner": wrapper_alice.key.public_key_hash(),
                    "token_id": collateral_token_id,
                }
            ],
            "callback": None,
        }
        print(f"Calling balance_of as an off-chain view with {fa2_balance_of}")
        wrapper.balance_of(**fa2_balance_of).callback_view()

        print("Gas costs:")
        print(json.dumps(gas_costs, indent=4))
        if WRITE_GAS_COSTS:
            write_gas_costs(gas_costs, WRITE_GAS_COSTS)


class WCtezTest(SandboxedTestCase):
    def test_wctez(self):
        gas_costs = {}
        # Hard-coded in contract, so also hard-coding here
        wctez_token_id = 3

        print("Deploying ctez contracts.")
        ctez = deploy_ctez(
            self.client,
            ctez_dir=os.path.join(PROJECT_ROOT, "vendor/ctez"),
            ttl=MAX_OPERATIONS_TTL,
        )

        wctez = deploy_wctez(
            self.client,
            checker_dir=CHECKER_DIR,
            ctez_fa12_address=ctez["fa12_ctez"].context.address,
            ttl=MAX_OPERATIONS_TTL,
        )
        print("Deployment finished.")

        account = self.client.key.public_key_hash()
        # Note: using alice's account which is assumed to be already initialized in the sandbox
        wctez_alice = pytezos.pytezos.using(
            shell=wctez.shell,
            key="edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq",
        ).contract(wctez.address)
        account_alice = wctez_alice.key.public_key_hash()

        def call_endpoint(contract, name, param, amount=0, client=self.client):
            print(f"Calling {name} with {param} and mutez={amount}")
            ret = inject(
                client,
                getattr(contract, name)(param)
                .with_amount(amount)
                .as_transaction()
                .autofill(ttl=MAX_OPERATIONS_TTL)
                .sign(),
            )
            return ret

        def call_wctez_endpoint(name, param, amount=0, client=self.client, wctez=wctez):
            ret = call_endpoint(wctez, name, param, amount, client)
            gas_costs[f"wctez%{name}"] = int(ret["contents"][0]["gas_limit"])
            return ret

        def single_fa2_transfer(
            sender: str, recipient: str, amount: int, token_id=wctez_token_id
        ):
            return [
                {
                    "from_": sender,
                    "txs": [
                        {
                            "to_": recipient,
                            "token_id": token_id,
                            "amount": amount,
                        },
                    ],
                }
            ]

        # Edge case: this call should succeed, according to the FA2 spec
        call_wctez_endpoint("transfer", single_fa2_transfer(account, account_alice, 0))

        # ===============================================================================
        # Wrapper-specific entrypoints
        # ===============================================================================
        # First have to get some ctez
        call_endpoint(
            ctez["ctez"], "create", (1, None, {"any": None}), amount=1_000_000
        )
        call_endpoint(ctez["ctez"], "mint_or_burn", (1, 800_000))
        # Then approve wctez to spend the ctez
        call_endpoint(ctez["fa12_ctez"], "approve", (wctez.context.address, 800_000))

        # Now we can mint wctez
        call_wctez_endpoint("mint", 800_000)
        assert_fa12_token_balance(ctez["fa12_ctez"], account, 0)
        assert_fa12_token_balance(ctez["fa12_ctez"], wctez.context.address, 800_000)
        assert_fa2_token_balance(wctez, account, wctez_token_id, 800_000)

        # And redeem some of it to get ctez back
        call_wctez_endpoint("redeem", 1000)
        assert_fa2_token_balance(wctez, account, wctez_token_id, 799_000)
        assert_fa12_token_balance(ctez["fa12_ctez"], wctez.context.address, 799_000)
        assert_fa12_token_balance(ctez["fa12_ctez"], account, 1000)
        # ===============================================================================
        # FA2 interface
        # ===============================================================================
        # Transfer from the test account to alice's account
        call_wctez_endpoint("transfer", single_fa2_transfer(account, account_alice, 90))
        assert_fa2_token_balance(wctez, account, wctez_token_id, 798_910)
        assert_fa2_token_balance(wctez, account_alice, wctez_token_id, 90)
        # Add the main account as an operator on alice's account
        # Note: using a client instance with alice's key for this since she is the
        # account owner.
        update_operators = [
            {
                "add_operator": {
                    "owner": wctez_alice.key.public_key_hash(),
                    "operator": account,
                    "token_id": wctez_token_id,
                }
            },
        ]
        call_wctez_endpoint(
            "update_operators",
            update_operators,
            client=wctez_alice,
            wctez=wctez_alice,
        )

        # Send some wctez tokens back to the main test account
        call_wctez_endpoint("transfer", single_fa2_transfer(account_alice, account, 80))
        assert_fa2_token_balance(wctez, account, wctez_token_id, 798_990)

        # Note: Using callback_view() here since we don't have a contract to use
        # for the callback.
        fa2_balance_of = {
            "requests": [
                {
                    "owner": wctez_alice.key.public_key_hash(),
                    "token_id": wctez_token_id,
                }
            ],
            "callback": None,
        }
        print(f"Calling balance_of as an off-chain view with {fa2_balance_of}")
        wctez.balance_of(**fa2_balance_of).callback_view()

        print("Gas costs:")
        print(json.dumps(gas_costs, indent=4))
        if WRITE_GAS_COSTS:
            write_gas_costs(gas_costs, WRITE_GAS_COSTS)


class MockFA2Test(SandboxedTestCase):
    def test_mockFA2(self):
        gas_costs = {}
        # Hard-coded in contract, so also hard-coding here
        mock_fa2_token_id = 42

        print("Deploying the mock FA2 contract.")
        mockFA2 = deploy_mockFA2(
            self.client,
            checker_dir=CHECKER_DIR,
            ttl=MAX_OPERATIONS_TTL,
        )
        print("Deployment finished.")

        account = self.client.key.public_key_hash()
        # Note: using alice's account which is assumed to be already initialized in the sandbox
        mockFA2_alice = pytezos.pytezos.using(
            shell=mockFA2.shell,
            key="edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq",
        ).contract(mockFA2.address)
        account_alice = mockFA2_alice.key.public_key_hash()

        def call_endpoint(contract, name, param, amount=0, client=self.client):
            print(f"Calling {name} with {param} and mutez={amount}")
            ret = inject(
                client,
                getattr(contract, name)(param)
                .with_amount(amount)
                .as_transaction()
                .autofill(ttl=MAX_OPERATIONS_TTL)
                .sign(),
            )
            return ret

        def call_mockFA2_endpoint(
            name, param, amount=0, client=self.client, mockFA2=mockFA2
        ):
            ret = call_endpoint(mockFA2, name, param, amount, client)
            gas_costs[f"mockFA2%{name}"] = int(ret["contents"][0]["gas_limit"])
            return ret

        def single_fa2_transfer(
            sender: str, recipient: str, amount: int, token_id=mock_fa2_token_id
        ):
            return [
                {
                    "from_": sender,
                    "txs": [
                        {
                            "to_": recipient,
                            "token_id": token_id,
                            "amount": amount,
                        },
                    ],
                }
            ]

        # Edge case: this call should succeed, according to the FA2 spec
        call_mockFA2_endpoint(
            "transfer", single_fa2_transfer(account, account_alice, 0)
        )

        # ===============================================================================
        # Contract-specific entrypoints
        # ===============================================================================
        # First we have to mint some tokens
        call_mockFA2_endpoint("mint", 800_000)
        assert_fa2_token_balance(mockFA2, account, mock_fa2_token_id, 800_000)

        # And redeem (destroy) some of it
        call_mockFA2_endpoint("redeem", 1000)
        assert_fa2_token_balance(mockFA2, account, mock_fa2_token_id, 799_000)
        # ===============================================================================
        # FA2 interface
        # ===============================================================================
        # Transfer from the test account to alice's account
        call_mockFA2_endpoint(
            "transfer", single_fa2_transfer(account, account_alice, 90)
        )
        assert_fa2_token_balance(mockFA2, account, mock_fa2_token_id, 798_910)
        assert_fa2_token_balance(mockFA2, account_alice, mock_fa2_token_id, 90)
        # Add the main account as an operator on alice's account
        # Note: using a client instance with alice's key for this since she is the
        # account owner.
        update_operators = [
            {
                "add_operator": {
                    "owner": mockFA2_alice.key.public_key_hash(),
                    "operator": account,
                    "token_id": mock_fa2_token_id,
                }
            },
        ]
        call_mockFA2_endpoint(
            "update_operators",
            update_operators,
            client=mockFA2_alice,
            mockFA2=mockFA2_alice,
        )

        # Send some tokens back to the main test account
        call_mockFA2_endpoint(
            "transfer", single_fa2_transfer(account_alice, account, 80)
        )
        assert_fa2_token_balance(mockFA2, account, mock_fa2_token_id, 798_990)

        # Note: Using callback_view() here since we don't have a contract to use
        # for the callback.
        fa2_balance_of = {
            "requests": [
                {
                    "owner": mockFA2_alice.key.public_key_hash(),
                    "token_id": mock_fa2_token_id,
                }
            ],
            "callback": None,
        }
        print(f"Calling balance_of as an off-chain view with {fa2_balance_of}")
        mockFA2.balance_of(**fa2_balance_of).callback_view()

        print("Gas costs:")
        print(json.dumps(gas_costs, indent=4))
        if WRITE_GAS_COSTS:
            write_gas_costs(gas_costs, WRITE_GAS_COSTS)


class LiquidationsStressTest(SandboxedTestCase):
    def test_liquidations(self):
        collateral_token_id = self.config.tokens.in_use.collateral.token_id
        # Hard-coded in contract, so also hard-coding here
        wctez_token_id = 3

        print("Deploying the mock oracle.")
        oracle = deploy_contract(
            self.client,
            source_file=os.path.join(PROJECT_ROOT, "util/mock_oracle.tz"),
            initial_storage=(self.client.key.public_key_hash(), 1000000),
            ttl=MAX_OPERATIONS_TTL,
        )

        print("Deploying the tez wrapper.")
        tez_wrapper = deploy_tez_wrapper(
            self.client,
            checker_dir=CHECKER_DIR,
            ttl=MAX_OPERATIONS_TTL,
        )

        print("Deploying ctez contract.")
        ctez = deploy_ctez(
            self.client,
            ctez_dir=os.path.join(PROJECT_ROOT, "vendor/ctez"),
            ttl=MAX_OPERATIONS_TTL,
        )

        print("Deploying wctez contract.")
        wctez = deploy_wctez(
            self.client,
            checker_dir=CHECKER_DIR,
            ctez_fa12_address=ctez["fa12_ctez"].context.address,
            ttl=MAX_OPERATIONS_TTL,
        )

        print("Deploying Checker.")
        checker = deploy_checker(
            self.client,
            checker_dir=CHECKER_DIR,
            oracle=oracle.context.address,
            tez_wrapper=tez_wrapper.context.address,
            ctez_fa12=ctez["fa12_ctez"].context.address,
            ctez_cfmm=ctez["cfmm"].context.address,
            wctez=wctez.context.address,
            ttl=MAX_OPERATIONS_TTL,
        )

        print("Deployment finished.")

        def call_endpoint(
            contract, name, param, amount=0, profiler=general_gas_profiler
        ):
            # Helper for calling contract endpoints with profiling
            print("Calling", contract.key.public_key_hash(), "/", name, "with", param)
            profile = profiler(
                self.client,
                contract,
                [
                    getattr(contract, name)(param)
                    .with_amount(amount)
                    .as_transaction()
                    .autofill(ttl=MAX_OPERATIONS_TTL)
                    .sign()
                ],
            )
            self.gas_profiles = merge_gas_profiles(self.gas_profiles, profile)

        def call_bulk(bulks, *, batch_size, profiler=general_gas_profiler):
            # Helper for calling operations in batches
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
                profile = profiler(self.client, checker, batch)
                self.gas_profiles = merge_gas_profiles(self.gas_profiles, profile)

        def get_tez_tokens_and_make_checker_an_operator(checker, amnt):
            call_endpoint(tez_wrapper, "deposit", None, amount=amnt)
            update_operators = [
                {
                    "add_operator": {
                        "owner": self.client.key.public_key_hash(),
                        "operator": checker.context.address,
                        "token_id": collateral_token_id,
                    }
                },
            ]
            call_endpoint(tez_wrapper, "update_operators", update_operators)

        # Note: the amount of kit minted here and the kit in all other burrows for this account
        # must be enough to bid on the liquidation auction later in the test.
        get_tez_tokens_and_make_checker_an_operator(checker, 200_000_000)
        call_endpoint(checker, "create_burrow", (0, None, 200_000_000))
        call_endpoint(checker, "mint_kit", (0, 80_000_000), amount=0)

        # Get some ctez (create an oven and mint some ctez)
        call_endpoint(
            ctez["ctez"], "create", (1, None, {"any": None}), amount=2_000_000
        )
        call_endpoint(ctez["ctez"], "mint_or_burn", (1, 100_000))

        # Get some wctez (approve wctez to move the ctez and mint some wctez)
        call_endpoint(ctez["fa12_ctez"], "approve", (wctez.context.address, 100_000))
        call_endpoint(wctez, "mint", 100_000)

        # Approve checker to spend the wctez
        update_operators = [
            {
                "add_operator": {
                    "owner": self.client.key.public_key_hash(),
                    "operator": checker.context.address,
                    "token_id": wctez_token_id,
                }
            },
        ]
        call_endpoint(wctez, "update_operators", update_operators)

        # Add some liquidity
        call_endpoint(
            checker,
            "add_liquidity",
            (100_000, 100_000, 5, int(datetime.now().timestamp()) + 20),
        )

        burrows = list(range(1, 1001))

        get_tez_tokens_and_make_checker_an_operator(
            checker, 100_000_000 * 1000
        )  # 1000 = number of burrows
        call_bulk(
            [
                checker.create_burrow((burrow_id, None, 100_000_000))
                for burrow_id in burrows
            ],
            batch_size=115,
        )
        # Mint as much as possible from the burrows. All should be identical, so we just query the
        # first burrow and mint that much kit from all of them.
        max_mintable_kit = checker.metadata.burrowMaxMintableKit(
            (self.client.key.public_key_hash(), 1)
        ).storage_view()

        call_bulk(
            [checker.mint_kit(burrow_no, max_mintable_kit) for burrow_no in burrows],
            batch_size=350,
        )

        # Change the index (kits are 10x valuable)
        #
        # Keep in mind that we're using a patched checker on tests where the protected index
        # is much faster to update.
        call_endpoint(oracle, "update", 10_000_000)

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
            batch_size=100,
            profiler=mark_for_liquidation_profiler,
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
            queued_tez += leaf["leaf"]["value"]["contents"]["tok"]
        assert (
            queued_tez > 10_000_000_000
        ), "queued tez in liquidation auction was not greater than Constants.max_lot_size which is required for this test"
        print(f"Auction queue currently has {queued_tez} mutez")
        call_endpoint(checker, "touch", None)

        # Now that there is an auction started in a descending state, we can select a queued slice
        # and be confident that we can cancel it before it gets added to another auction.
        # To successfully do this, we also need to either move the index price such that the
        # cancellation is warranted or deposit extra collateral to the burrow. Here we do the latter.
        cancel_ops = []
        get_tez_tokens_and_make_checker_an_operator(
            checker, 1_000_000_000 * 895
        )  # the maximum amount of cancel_ops (see below)
        for i, (queued_leaf_ptr, leaf) in enumerate(
            auction_avl_leaves(
                checker,
                checker.storage["deployment_state"]["sealed"]["liquidation_auctions"][
                    "queued_slices"
                ](),
            )
        ):
            cancel_ops.append(
                (
                    checker.deposit_collateral(
                        (leaf["leaf"]["value"]["contents"]["burrow"][1], 1_000_000_000)
                    ),
                    checker.cancel_liquidation_slice(queued_leaf_ptr),
                )
            )
            # Note: picking 895 here since it is close to the queue_size
            # at this point in the test. This number can be tweaked up
            # or down as desired.
            if len(cancel_ops) >= 895:
                break
        # Shuffle so we aren't only cancelling the oldest slice
        shuffle(cancel_ops)
        flattened_cancel_ops = []
        for op1, op2 in cancel_ops:
            flattened_cancel_ops += [op1, op2]
        call_bulk(
            flattened_cancel_ops,
            batch_size=100,
            profiler=cancel_liquidation_slice_profiler,
        )

        # And we place a bid for the auction we started earlier:
        auction_details = (
            checker.metadata.currentLiquidationAuctionDetails().storage_view()
        )

        auction_id, minimum_bid = auction_details["contents"], auction_details["nat_3"]
        # FIXME: The return value is supposed to be annotated as "auction_id" and "minimum_bid", I
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

        # Note: using smaller batches here to increase the number of samples for profiling.
        for i in range(0, len(auctioned_slices), 5):
            slices_to_cancel = auctioned_slices[i : i + 5]
            call_endpoint(checker, "touch_liquidation_slices", slices_to_cancel)

        # With all of the slices touched, we should now be able to claim our hard-earned winnings
        call_endpoint(checker, "liquidation_auction_claim_win", current_auctions_ptr)

        # Extra calls for profiling purposes
        call_bulk(
            [checker.touch(None) for _ in range(5)],
            batch_size=2,
        )

        # Optionally write out gas profile json
        if WRITE_GAS_PROFILES:
            with open(WRITE_GAS_PROFILES, "w") as f:
                json.dump(self.gas_profiles, f, indent=4)


if __name__ == "__main__":
    unittest.main()
