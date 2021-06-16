"""Helpers for working with operations in pytezos
"""

import time
from datetime import datetime
from typing import Dict, List, Tuple

from pytezos.client import PyTezosClient
from pytezos.operation.group import OperationGroup
from pytezos.rpc.node import RpcError

# Maximum allowed number of reorgs
N_ALLOWED_REORGS = 3
# Maximum allowed amount of time to wait between blocks in seconds
MAX_BLOCK_TIME = 100


class BlockchainReorg(Exception):
    """Raised when a blockchain reorganization is detected."""


# Helper for querying operation data for a given operation at a given
# known block level.
def _get_op_data(tz: PyTezosClient, op_hash: str, level: int) -> dict:
    for op_type in tz.shell.blocks[level].operations():
        for op in op_type:
            if op["hash"] == op_hash:
                return op
    raise Exception(f"Operation {op_hash} not found in specified level {level}")


def await_operations(
    tz: PyTezosClient,
    op_hashes: List[str],
    start_level: int,
    max_blocks: int,
    min_confirmations: int = 1,
) -> Tuple[bool, List[dict]]:
    """Awaits a list of operations to be included in a block.

    Args:
        tz: The pytezos client instance
        op_hashes: A list of operation hashes to await
        start_level: The starting level at which to start searching for operations. This should be a
            level at or before the operations were injected.
        min_confirmations: Minimum number of blocks required to consider operations as having been
            included on the chain.
    Returns:
        A tuple containing a flag indicating whether all operations were confirmed along with
        the corresponding levels of each operation.

    Raises:
        BlockchainReorg: If a reorganization is detected. This invalidates the current search.
    """
    op_hashes = set(op_hashes)
    current_level = start_level
    previous_hash = None

    # Initial time to sleep between blocks in seconds
    sleep_for = 1
    # Counter tracking total time slept between adjacent blocks
    slept = 0
    confirmations = 0
    confirmed_ops = set([])
    op_levels = {}

    while current_level < (start_level + max_blocks):
        # Search the current level for our operations. Need to wait if the block doesn't exist yet.
        try:
            block_ops = set([])
            for group in tz.shell.blocks[current_level].operation_hashes():
                block_ops = block_ops.union({o for o in group})
        except RpcError:
            if slept > MAX_BLOCK_TIME:
                break
            time.sleep(sleep_for)
            slept += sleep_for
            continue
        else:
            # Since we found the block we can reset the sleep counter
            slept = 0

        # If there was a re-org we leave it to the caller to restart the search
        # TODO: There might be a smarter way to handle reorgs by comparing branches, etc.
        current_header = tz.shell.blocks[current_level].header()
        if previous_hash and (current_header["predecessor"] != previous_hash):
            raise BlockchainReorg(
                f"Reorg detected at level {current_level}. Expected predecessor "
                f"{previous_hash} but found {current_header['predecessor']}"
            )
        else:
            previous_hash = current_header["hash"]

        matching_ops = op_hashes.intersection(block_ops)
        confirmed_ops = confirmed_ops.union(matching_ops)
        # Note the level of each operation we found in this block to make querying them easier
        for op in matching_ops:
            op_levels[op] = current_level

        all_confirmed = confirmed_ops == op_hashes
        if all_confirmed:
            confirmations += 1

        # We've confirmed everything, let's get out of here.
        if all_confirmed and (confirmations >= min_confirmations):
            break

        # Compute wait time based on when we expect the next block to appear in *UTC*
        current_block_timestamp = datetime.strptime(
            current_header["timestamp"], "%Y-%m-%dT%H:%M:%SZ"
        )
        # Note: This is only a *minimum* bound.
        block_expected_starting_at = current_block_timestamp.timestamp() + int(
            tz.shell.blocks[current_level].context.constants()["time_between_blocks"][0]
        )
        sleep_for = max(1, block_expected_starting_at - datetime.utcnow().timestamp())
        if sleep_for > MAX_BLOCK_TIME:
            print(
                f"Warning! Estimated arrival time of next block of {sleep_for} is greater than the maximum {MAX_BLOCK_TIME}. "
                "You might want to check the status of the node you are connected to."
            )
        current_level += 1

    return confirmed_ops == op_hashes, op_levels


def inject(
    tz: PyTezosClient, op_group: OperationGroup, max_wait_blocks=100, min_confirmations=1
) -> Dict:
    """A replacement for pytezos's OperationGroup.inject

    Submits the provided operation group and waits until it is confirmed by one block.

    Args:
        tz: pytezos client instance
        op_group: The operation group to inject
        max_wait_blocks: Maximum number of blocks to wait for a confirmation

    Raises:
        BlockchainReorg: If the confirmation search encountered more than 3 reorgs
        Exception: If the operation group was not confirmed

    Returns:
        The confirmed operation group's metadata
    """
    level = tz.shell.head.header()["level"]
    op_hash = op_group.inject(min_confirmations=0)["hash"]
    for attempt in range(1, N_ALLOWED_REORGS + 1):
        try:
            all_confirmed, op_levels = await_operations(
                tz,
                [op_hash],
                level,
                max_blocks=max_wait_blocks,
                min_confirmations=min_confirmations,
            )
            break
        except BlockchainReorg as e:
            if attempt >= N_ALLOWED_REORGS:
                raise e
            time.sleep(1)
    if not all_confirmed:
        raise Exception(f"Operation {op_hash} was not confirmed.")
    return _get_op_data(tz, op_hash, op_levels[op_hash])
