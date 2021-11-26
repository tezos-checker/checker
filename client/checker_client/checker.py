import json
import logging
import math
import os
import sys
import subprocess
import tempfile
import shutil
import time
import signal
from collections import namedtuple
from decimal import Decimal
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

import docker
import pytezos
from pytezos.contract.interface import ContractInterface
import requests
from pytezos.client import PyTezosClient
from pytezos.operation import MAX_OPERATIONS_TTL

from checker_builder.config import CheckerRepo, load_input_config, CheckerConfig, IssuedTokenConfig
from checker_client.operations import inject


# Time between blocks for sandbox container
SANDBOX_TIME_BETWEEN_BLOCKS = "1,1"
# Number of retries to use when awaiting new blocks
WAIT_OP_ATTEMPTS = 10
# Interval between retries when awaiting new blocks
WAIT_OP_DELAY = 5


default_token_metadata = {
    "kit": {
        "name": "kit",
        "symbol": "KIT",
    },
    "liquidity": {
        "name": "kit liquidity",
        "symbol": "KTLQ",
    },
}


def _token_config_to_metadata(token_config: IssuedTokenConfig) -> Tuple[int, Dict]:
    return token_config.token_id, {
        "name": token_config.name,
        "symbol": token_config.symbol,
        "decimals": token_config.decimal_digits,
    }


def token_metadata_view_from_issued_token_config(token_configs: List[IssuedTokenConfig]):
    metadata = [_token_config_to_metadata(token_config) for token_config in token_configs]

    # convert the attributes to bytes
    for _, attrs in metadata:
        for k, v in attrs.items():
            if isinstance(v, bytes):
                pass
            elif isinstance(v, str):
                v = v.encode("utf-8")
            elif isinstance(v, int):
                v = str(v).encode("utf-8")
            elif isinstance(v, dict):
                v = json.dumps(v).encode("utf-8")
            else:
                raise "error encoding the token metadata, key: {}".format(k)
            attrs[k] = v

    # create the TokenMetadata objects
    tokens = [TokenMetadata(token_id, m) for token_id, m in metadata]

    # compile and return the view
    return compile_view_fa2_token_metadata(tokens)


def checker_token_metadata_view_from_config(*, config: CheckerConfig):
    tokens = [
        config.tokens.issued.kit,
        config.tokens.issued.liquidity,
    ]
    token_metadata_view_from_issued_token_config(tokens)


# attrs should be a dict from strings to bytes.
TokenMetadata = namedtuple("TokenMetadata", ["id", "attrs"])


def compile_view_fa2_token_metadata(tokens: List[TokenMetadata]):
    from pytezos.michelson.types.core import BytesType, NatType, StringType
    from pytezos.michelson.types.map import MapType
    from pytezos.michelson.types.pair import PairType

    # this map is of type:
    #   map nat (nat, map string bytes)
    elt = MapType.from_items(
        [
            (
                NatType.from_value(token.id),
                PairType.from_comb(
                    [
                        NatType.from_value(token.id),
                        MapType.from_items(
                            [
                                (StringType.from_value(key), BytesType.from_value(value))
                                for key, value in sorted(token.attrs.items())
                            ]
                        ),
                    ]
                ),
            )
            for token in tokens
        ]
    )

    # Below code takes a '(nat, state)', looks up the 'nat' from the map above ('elt'),
    # and returns the result.
    code = [
        # get the 'fst' of the '(nat, state)' pair
        {"prim": "CAR"},
        # push the 'elt' to the stack
        {"prim": "PUSH", "args": [elt.as_micheline_expr(), elt.to_micheline_value()]},
        # swap the top two elements (to match 'get's parameter order)
        {"prim": "SWAP"},
        # lookup the given 'nat' from the map
        {"prim": "GET"},
        # fail if we don't get something, return otherwise
        {
            "prim": "IF_NONE",
            "args": [  # if none
                [
                    {
                        "prim": "PUSH",
                        "args": [{"prim": "string"}, {"string": "FA2_UNKNOWN_TOKEN"}],
                    },
                    {"prim": "FAILWITH"},
                ],
                # if not none
                [],
            ],
        },
    ]

    return {
        "name": "token_metadata",
        "code": code,
        "parameter": {"prim": "nat"},
        "returnType": {
            "prim": "pair",
            "args": [
                {"prim": "nat"},
                {"prim": "map", "args": [{"prim": "string"}, {"prim": "bytes"}]},
            ],
        },
    }


def start_sandbox(name: str, port: int, wait_for_level=0):
    teardownFun = start_local_sandbox(name, port)

    client = pytezos.pytezos.using(
        shell="http://127.0.0.1:{}".format(port),
        key="edsk3RFfvaFaxbHx8BMtEW1rKQcPtDML3LXjNqMNLCzC3wLC1bWbAt",  # bob's key from "flobox info"
    )
    # Increase log level to avoid connection errors
    client.loglevel = logging.ERROR

    # wait some time for the node to start
    print("Trying to connect to the sandbox.")
    for _ in range(20):
        try:
            client.shell.node.get("/version/")
            break
        except requests.exceptions.ConnectionError as exc:
            print("Connection failed, retrying:", exc)
            time.sleep(1)
    else:
        raise Exception("Can not connect to the sandbox.")

    print(f"Sandbox initialized. Waiting for expected level to be reached... ")
    last_level = 0
    while True:
        # Wait until enough blocks have been baked for further deploy operations, etc.
        level = client.shell.head.header()["level"]
        if level != last_level:
            print(f"Sandbox at level {level} / {wait_for_level}")
            last_level = level
        if level >= wait_for_level:
            break
    print("Sandbox started!")
    return client, teardownFun


# FIXME: Deprecated
def start_docker_sandbox(name: str, port: int):
    docker_client = docker.from_env()
    docker_container = docker_client.containers.run(
        "tqtezos/flextesa:20210514",
        command=["granabox", "start"],
        environment={"block_time": SANDBOX_TIME_BETWEEN_BLOCKS},
        ports={"20000/tcp": port},
        name=name,
        detach=True,
        remove=True,
    )

    def teardownFun():
        docker_container.kill()
        docker_client.close()

    return teardownFun


def start_local_sandbox(name: str, port: int):
    # FIXME: Port selection for flextesa seems to be a bit flaky if port !=20_000.
    alice_key = subprocess.check_output(["flextesa", "key-of-name", "alice"]).decode("utf-8")
    bob_key = subprocess.check_output(["flextesa", "key-of-name", "bob"]).decode("utf-8")

    tmpdir = tempfile.mkdtemp(prefix=name)

    # below command is mainly from the 'granabox' script from flextesa docker
    # container.
    args = [
        "flextesa",
        "mini-network",
        f"--root={tmpdir}",
        f"--base-port={port}",
        "--size=1",
        "--set-history-mode=N000:archive",
        f"--time-between-blocks={SANDBOX_TIME_BETWEEN_BLOCKS}",
        f"--minimal-block-delay=1",
        f"--add-bootstrap-account={alice_key}@2_000_000_000_000",
        f"--add-bootstrap-account={bob_key}@2_000_000_000_000",
        "--no-daemons-for=alice",
        "--no-daemons-for=bob",
        "--until-level=200_000_000",
        "--protocol-hash=PtGRANADsDU8R9daYKAgWnQYAJ64omN1o3KMGVCykShA97vQbvV",
        "--protocol-kind=Granada",
    ]

    handle = subprocess.Popen(args)

    def teardownFun():
        # send a keyboard interrupt and wait
        print("Stopping sandbox process")
        handle.send_signal(signal.SIGINT)
        timeout = 20
        print(f"Waiting up to {timeout}s for sandbox process to exit...")
        handle.wait(timeout=20)
        print("Sandbox exited. Removing sandbox data dir.")
        # remove the state directory
        shutil.rmtree(tmpdir)

    return teardownFun


# FIXME: Deprecated
def is_sandbox_container_running(name: str):
    docker_client = docker.from_env()
    try:
        container = docker_client.containers.get(name)
    except docker.errors.NotFound:
        return False
    if container.status == "running":
        return True
    else:
        return False


def deploy_contract(
    tz: PyTezosClient,
    *,
    source_file: Path,
    initial_storage: Any,
    initial_balance: int = 0,
    ttl: int = MAX_OPERATIONS_TTL,
):
    script = pytezos.ContractInterface.from_file(source_file).script(
        initial_storage=initial_storage
    )

    origination = inject(
        tz, tz.origination(script, balance=initial_balance).autofill(ttl=ttl).sign()
    )

    # opg = tz.shell.blocks[origination["branch"] :].find_operation(origination["hash"])
    res = pytezos.operation.result.OperationResult.from_operation_group(origination)

    addr = res[0].originated_contracts[0]
    return tz.contract(addr)


def deploy_wtez(
    tz: PyTezosClient,
    repo: CheckerRepo,
    ttl: Optional[int] = None,
):
    print("Deploying the wtez contract.")
    src = repo.wtez_contract
    initial_storage = {
        "fa2_state": {
            "ledger": {},
            "operators": {},
        },
        "vaults": {},
        "metadata": {},  # FIXME: populate with TZIP-016 metadata for wtez token
    }
    wrapper = deploy_contract(tz, source_file=src, initial_storage=initial_storage, ttl=ttl)
    print("Done.")
    print(f"Tez Wrapper address: {wrapper.context.address}")
    return wrapper


def deploy_wctez(
    tz: PyTezosClient,
    repo: CheckerRepo,
    ctez_fa12_address: str,
    ttl: Optional[int] = None,
):
    print("Deploying the wctez contract.")
    src = repo.wctez_contract
    initial_storage = {
        "fa2_state": {
            "ledger": {},
            "operators": {},
        },
        "ctez_fa12_address": ctez_fa12_address,
        "metadata": {},  # FIXME: populate with TZIP-016 metadata for wctez token
    }
    wctez = deploy_contract(tz, source_file=src, initial_storage=initial_storage, ttl=ttl)
    print("Done.")
    print(f"wctez address: {wctez.context.address}")
    return wctez


def deploy_mockFA2(
    tz: PyTezosClient,
    repo: CheckerRepo,
    ttl: Optional[int] = None,
):
    print("Deploying the mock FA2 contract.")
    src = repo.mock_fa2_contract
    initial_storage = {
        "fa2_state": {
            "ledger": {},
            "operators": {},
        },
        "metadata": {},  # FIXME: populate with TZIP-016 metadata for mock_fa2 token
    }
    mockFA2 = deploy_contract(tz, source_file=src, initial_storage=initial_storage, ttl=ttl)
    print("Done.")
    print(f"mockFA2 address: {mockFA2.context.address}")
    return mockFA2


def deploy_checker(
    tz: PyTezosClient,
    repo: CheckerRepo,
    *,
    oracle: ContractInterface,
    collateral_fa2: ContractInterface,
    cfmm_token_fa2: ContractInterface,
    ctez_cfmm: ContractInterface,  # FIXME: Wish we didn't need this one
    ttl: Optional[int] = None,
):
    config = load_input_config()

    print("Deploying the wrapper.")

    checker = deploy_contract(
        tz,
        source_file=repo.checker_contract,
        initial_storage=({}, {}, {"unsealed": tz.key.public_key_hash()}),
        ttl=ttl,
    )
    print("Checker address: {}".format(checker.context.address))

    with repo.checker_functions.open() as f:
        functions = json.load(f)

    print("Deploying the TZIP-16 metadata.")

    token_metadata_view = checker_token_metadata_view_from_config(config=config)

    metadata = {
        "interfaces": ["TZIP-012-4b3c67aad5abb"],
        "views": [
            {
                "name": view["name"],
                "implementations": [
                    {
                        "michelsonStorageView": {
                            "parameter": view["parameter"],
                            "returnType": view["returnType"],
                            "code": view["code"],
                        }
                    }
                ],
            }
            for view in functions["views"] + [token_metadata_view]
        ],
        # This field is supposed to be optional but it mistakenly was required before
        # pytezos commit 12911835
        "errors": [],
    }
    metadata_ser = json.dumps(metadata).encode("utf-8")
    chunk_size = 10 * 1024
    metadata_chunks = [
        metadata_ser[i : i + chunk_size] for i in range(0, len(metadata_ser), chunk_size)
    ]
    for chunk_no, chunk in enumerate(metadata_chunks, 1):
        print("Deploying TZIP-16 metadata: chunk {} of {}".format(chunk_no, len(metadata_chunks)))
        inject(tz, checker.deployMetadata(chunk).as_transaction().autofill(ttl=ttl).sign())

    # TODO: implement the batching logic here for speed (see the previous ruby script)
    for fun in functions["lazy_functions"]:
        print("Deploying: {}".format(fun["name"]))
        for chunk_no, chunk in enumerate(fun["chunks"]):
            arg = (int(fun["fn_id"]), "0x" + chunk)
            inject(tz, checker.deployFunction(arg).as_transaction().autofill(ttl=ttl).sign())
            print("  deployed: chunk {}.".format(chunk_no))

    print("Sealing.")

    external_contracts = {
        "oracle": oracle,
        "collateral_fa2": collateral_fa2,
        "ctok_fa2": cfmm_token_fa2,
        "ctez_cfmm": ctez_cfmm,
    }

    inject(
        tz,
        checker.sealContract(external_contracts).as_transaction().autofill(ttl=ttl).sign(),
    )

    return checker


def ligo_compile(src_file: Path, entrypoint: str, out_file: Path):
    """Compiles an mligo file into michelson using ligo"""
    try:
        res = subprocess.run(
            ["ligo", "compile-contract", str(src_file), entrypoint],
            check=True,
            capture_output=True,
        )
    except subprocess.CalledProcessError as e:
        print(e.stdout)
        print(e.stderr)
        raise e
    with out_file.open("wb") as f:
        f.write(res.stdout)


def deploy_ctez(tz: PyTezosClient, repo: CheckerRepo, ttl: Optional[int] = None):
    """Compiles and deploys the ctez contracts.

    Should probably eventually be moved to the ctez repo itself...
    """
    ctez_dir = repo.ctez
    tmpdir = tempfile.TemporaryDirectory(suffix="-checker-ctez")
    with tempfile.TemporaryDirectory(suffix="-checker-ctez") as tmpdir:
        tmpdir = Path(tmpdir)
        ctez_src = Path(ctez_dir).joinpath("ctez.mligo")
        fa12_src = Path(ctez_dir).joinpath("fa12.mligo")
        cfmm_src = Path(ctez_dir).joinpath("cfmm_tez_ctez.mligo")

        ctez_michelson = tmpdir.joinpath(ctez_src.with_suffix(".tz").name)
        fa12_michelson = tmpdir.joinpath(fa12_src.with_suffix(".tz").name)
        cfmm_michelson = tmpdir.joinpath(cfmm_src.with_suffix(".tz").name)

        ligo_compile(ctez_src, "main", ctez_michelson)
        ligo_compile(fa12_src, "main", fa12_michelson)
        ligo_compile(cfmm_src, "main", cfmm_michelson)

        print("Deploying ctez contract...")
        ctez_storage = {
            "ovens": {},
            "target": 1 << 48,
            "drift": 0,
            "last_drift_update": math.floor(time.time()),
            "ctez_fa12_address": "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU",
            "cfmm_address": "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU",
        }
        ctez = deploy_contract(
            tz, source_file=str(ctez_michelson), initial_storage=ctez_storage, ttl=ttl
        )
        print("Done.")

        print("Deploying ctez FA1.2 contract...")
        fa12_ctez_storage = {
            "tokens": {"tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU": 1},
            "allowances": {},
            "admin": ctez.context.address,
            "total_supply": 1,
        }
        fa12_ctez = deploy_contract(
            tz, source_file=str(fa12_michelson), initial_storage=fa12_ctez_storage, ttl=ttl
        )
        print("Done.")

        print("Deploying ctez CFMM contract...")
        cfmm_storage = {
            "tezPool": 1,
            "cashPool": 1,
            "target": 281474976710656,  # Bitwise.shift_left 1n 48n
            "lqtTotal": 1,
            "ctez_address": ctez.context.address,
            "cashAddress": fa12_ctez.context.address,
            "lqtAddress": "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU",
            "lastOracleUpdate": math.floor(time.time()),
            "consumerEntrypoint": f"{ctez.context.address}%cfmm_price",
        }

        cfmm = deploy_contract(
            tz,
            source_file=str(cfmm_michelson),
            initial_storage=cfmm_storage,
            initial_balance=Decimal(0.000001),
            ttl=ttl,
        )
        print("Done.")

        print("Deploying liquidity contract...")
        fa12_lqt_storage = {
            "tokens": {"tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU": 1},
            "allowances": {},
            "admin": cfmm.context.address,
            "total_supply": 1,
        }
        fa12_lqt = deploy_contract(
            tz, source_file=str(fa12_michelson), initial_storage=fa12_lqt_storage, ttl=ttl
        )
        print("Done.")

        print("Setting liquidity address in CFMM contract...")

        inject(
            tz,
            cfmm.setLqtAddress(fa12_lqt.context.address).as_transaction().autofill(ttl=ttl).sign(),
        )

        print("Done.")

        print("Setting CFMM amd ctez FA1.2 addresses in ctez contract...")
        inject(
            tz,
            ctez.set_addresses((cfmm.context.address, fa12_ctez.context.address))
            .as_transaction()
            .autofill(ttl=ttl)
            .sign(),
        )
        print("Done.")

        return {
            "ctez": ctez,
            "fa12_ctez": fa12_ctez,
            "cfmm": cfmm,
            "fa12_lqt": fa12_lqt,
        }
