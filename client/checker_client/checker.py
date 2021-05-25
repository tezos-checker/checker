from pytezos.client import PyTezosClient
from client.checker_client.cli import ctez
import json
import logging
import os
import subprocess
import tempfile
import time
import docker
import pytezos
import requests
from pathlib import Path
import math
from decimal import Decimal

# Note: Setting this to 1 causes weird issues. Keep it >= 2s.
SANDBOX_TIME_BETWEEN_BLOCKS = 5


def start_sandbox(name: str, port: int):
    docker_client = docker.from_env()
    docker_container = docker_client.containers.run(
        "tqtezos/flextesa:20210316",
        command=["edobox", "start"],
        environment={"block_time": SANDBOX_TIME_BETWEEN_BLOCKS},
        ports={"20000/tcp": port},
        name=name,
        detach=True,
        remove=True,
    )
    client = pytezos.pytezos.using(
        shell="http://127.0.0.1:{}".format(port),
        key="edsk3RFfvaFaxbHx8BMtEW1rKQcPtDML3LXjNqMNLCzC3wLC1bWbAt",  # bob's key from "edobox info"
    )
    # Increase log level to avoid connection errors
    client.loglevel = logging.ERROR
    # wait some time for the node to start
    while True:
        try:
            client.shell.node.get("/version/")
            break
        except requests.exceptions.ConnectionError:
            time.sleep(0.1)
    # wait until a block is mined
    time.sleep(3)
    return client, docker_client, docker_container


def stop_sandbox(name: str):
    docker_client = docker.from_env()
    container = docker_client.containers.get(name)
    container.kill()


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
    source_file,
    initial_storage,
    initial_balance=0,
    num_blocks_wait=100,
):
    script = pytezos.ContractInterface.from_file(source_file).script(
        initial_storage=initial_storage
    )

    # FIXME: Can remove this statement after debugging
    print(f"Detected block time: {tz.shell.block.context.constants()['time_between_blocks']}")

    origination = (
        tz.origination(script, balance=initial_balance)
        .autofill()
        .sign()
        .inject(min_confirmations=1, num_blocks_wait=num_blocks_wait)
    )

    opg = tz.shell.blocks[origination["branch"] :].find_operation(origination["hash"])
    res = pytezos.operation.result.OperationResult.from_operation_group(opg)

    addr = res[0].originated_contracts[0]
    return tz.contract(addr)


def deploy_checker(tz, checker_dir, *, oracle, ctez, num_blocks_wait=100):
    print("Deploying the wrapper.")

    checker = deploy_contract(
        tz,
        source_file=os.path.join(checker_dir, "main.tz"),
        initial_storage=({}, {}, {"unsealed": tz.key.public_key_hash()}),
        num_blocks_wait=num_blocks_wait,
    )

    print("Checker address: {}".format(checker.context.address))

    with open(os.path.join(checker_dir, "functions.json")) as f:
        functions = json.load(f)

    print("Deploying the TZIP-16 metadata.")
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
            for view in functions["views"]
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
        (
            checker.deployMetadata(chunk)
            .as_transaction()
            .autofill()
            .sign()
            .inject(min_confirmations=1, num_blocks_wait=num_blocks_wait)
        )

    # TODO: implement the batching logic here for speed (see the previous ruby script)
    for fun in functions["lazy_functions"]:
        print("Deploying: {}".format(fun["name"]))
        for chunk_no, chunk in enumerate(fun["chunks"]):
            arg = (int(fun["fn_id"]), "0x" + chunk)
            (
                checker.deployFunction(arg)
                .as_transaction()
                .autofill()
                .sign()
                .inject(min_confirmations=1, num_blocks_wait=num_blocks_wait)
            )
            print("  deployed: chunk {}.".format(chunk_no))

    print("Sealing.")
    (
        checker.sealContract((oracle, ctez))
        .as_transaction()
        .autofill()
        .sign()
        .inject(min_confirmations=1, num_blocks_wait=num_blocks_wait)
    )

    return checker


def ligo_compile(src_file: Path, entrypoint: str, out_file: Path):
    """Compiles an mligo file into michelson using ligo"""
    with out_file.open("wb") as f:
        subprocess.run(
            ["ligo", "compile-contract", str(src_file), entrypoint],
            check=True,
            stdout=f,
        )


def deploy_ctez(tz: PyTezosClient, ctez_dir, num_blocks_wait=100):
    """Compiles and deploys the ctez contracts.

    Should probably eventually be moved to the ctez repo itself...
    """
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
        ctez = deploy_contract(tz, source_file=str(ctez_michelson), initial_storage=ctez_storage)
        print("Done.")

        print("Deploying ctez FA1.2 contract...")
        fa12_ctez_storage = {
            "tokens": {"tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU": 1},
            "allowances": {},
            "admin": tz.key.public_key_hash(),
            "total_supply": 1,
        }
        fa12_ctez = deploy_contract(
            tz, source_file=str(fa12_michelson), initial_storage=fa12_ctez_storage
        )
        print("Done.")

        print("Deploying ctez CFMM contract...")
        cfmm_storage = {
            "tokenPool": 1,
            "cashPool": 1,
            "lqtTotal": 1,
            "pendingPoolUpdates": 0,
            "tokenAddress": fa12_ctez.context.address,
            "lqtAddress": "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU",
            "lastOracleUpdate": math.floor(time.time()),
            "consumerEntrypoint": ctez.context.address,
        }
        cfmm = deploy_contract(
            tz,
            source_file=str(cfmm_michelson),
            initial_storage=cfmm_storage,
            initial_balance=Decimal(0.000001),
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
            tz, source_file=str(fa12_michelson), initial_storage=fa12_lqt_storage
        )
        print("Done.")

        print("Setting liquidity address in CFMM contract...")
        (
            cfmm.setLqtAddress(fa12_lqt.context.address)
            .as_transaction()
            .autofill()
            .sign()
            .inject(min_confirmations=1, num_blocks_wait=num_blocks_wait)
        )
        print("Done.")

        print("Setting CFMM amd ctez FA1.2 addresses in ctez contract...")
        (
            ctez.set_addresses((cfmm.context.address, fa12_ctez.context.address))
            .as_transaction()
            .autofill()
            .sign()
            .inject(min_confirmations=1, num_blocks_wait=num_blocks_wait)
        )
        print("Done.")

        return {
            "ctez": ctez,
            "fa12_ctez": fa12_ctez,
            "cfmm": cfmm,
            "fa12_lqt": fa12_lqt,
        }
