import json
import logging
import os
import re
import shlex
import subprocess
import tempfile
import time

import docker
import pytezos
import requests


def start_sandbox(name: str, port: int):
    docker_client = docker.from_env()
    docker_container = docker_client.containers.run(
        "tqtezos/flextesa:20210316",
        command=["edobox", "start"],
        environment={"block_time": 1},
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


def deploy_contract(tz, *, source_file, initial_storage):
    script = pytezos.ContractInterface.from_file(source_file).script(
        initial_storage=initial_storage
    )

    origination = (
        tz.origination(script)
        .autofill(branch_offset=1)
        .sign()
        .inject(min_confirmations=1, time_between_blocks=5)
    )

    opg = tz.shell.blocks[origination["branch"] :].find_operation(origination["hash"])
    res = pytezos.operation.result.OperationResult.from_operation_group(opg)

    addr = res[0].originated_contracts[0]
    return tz.contract(addr)


def deploy_checker(tz, checker_dir, *, oracle, ctez):
    print("Deploying the wrapper.")

    checker = deploy_contract(
        tz,
        source_file=os.path.join(checker_dir, "main.tz"),
        initial_storage=({}, {}, {"unsealed": tz.key.public_key_hash()}),
    )

    print("Checker address: {}".format(checker.context.address))

    with open(os.path.join(checker_dir, "functions.json")) as f:
        functions = json.load(f)

    print("Deploying the TZIP-16 metadata.")
    metadata = {
      "interfaces": ["TZIP-012-4b3c67aad5abb"],
      "views": [
        { "name": view["name"],
          "implementations": [ { "michelsonStorageView": { "parameter": view["parameter"],
                                                           "returnType": view["returnType"],
                                                           "code": view["code"]
                                                         }
                               }
                             ]
        }
        for view in functions["views"]
      ],

      # This field is supposed to be optional but it mistakenly was required before
      # pytezos commit 12911835
      "errors": []
    }
    metadata_ser = json.dumps(metadata).encode("utf-8")
    chunk_size = 10 * 1024
    metadata_chunks = [metadata_ser[i:i+chunk_size] for i in range(0, len(metadata_ser), chunk_size)]
    for chunk_no, chunk in enumerate(metadata_chunks, 1):
      print("Deploying TZIP-16 metadata: chunk {} of {}".format(chunk_no, len(metadata_chunks)))
      (checker.deployMetadata(chunk)
        .as_transaction()
        .autofill(branch_offset=1)
        .sign()
        .inject(min_confirmations=1, time_between_blocks=5)
      )

    # TODO: implement the batching logic here for speed (see the previous ruby script)
    for fun in functions["lazy_functions"]:
        print("Deploying: {}".format(fun["name"]))
        for chunk_no, chunk in enumerate(fun["chunks"]):
            arg = (int(fun["fn_id"]), "0x" + chunk)
            (
                checker.deployFunction(arg)
                .as_transaction()
                .autofill(branch_offset=1)
                .sign()
                .inject(min_confirmations=1, time_between_blocks=5)
            )
            print("  deployed: chunk {}.".format(chunk_no))

    print("Sealing.")
    (
        checker.sealContract((oracle, ctez))
        .as_transaction()
        .autofill(branch_offset=1)
        .sign()
        .inject(min_confirmations=1, time_between_blocks=5)
    )

    return checker


def deploy_ctez(tz, ctez_dir):
    with tempfile.TemporaryDirectory(suffix="-checker-ctez") as tmpdir:
        os.environ["TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER"] = "yes"
        os.mkdir(os.path.join(tmpdir, "tezos-client"))
        tc_cmd = [
            "tezos-client",
            "--base-dir",
            os.path.join(tmpdir, "tezos-client"),
            "--endpoint",
            tz.shell.node.uri,
        ]

        subprocess.check_call(tc_cmd + ["bootstrapped"])
        subprocess.check_call(
            tc_cmd
            + [
                "import",
                "secret",
                "key",
                "bootstrap1",
                "unencrypted:{}".format(tz.context.key.secret_key()),
            ]
        )

        with open(os.path.join(ctez_dir, "deploy.sh"), "r") as deploy_script:
            with open(os.path.join(tmpdir, "deploy.sh"), "w") as new_deploy_script:
                new = deploy_script.read()

                new = re.sub(
                    "^TZC=.*", 'TZC="{}"'.format(shlex.join(tc_cmd)), new, flags=re.MULTILINE
                )

                new = re.sub(".*create mockup.*", "", new, flags=re.MULTILINE)

                new_deploy_script.write(new)

        subprocess.check_call(["bash", os.path.join(tmpdir, "deploy.sh")], cwd=ctez_dir)

        def get_ctez_contract(c):
            addr = (
                subprocess.check_output(tc_cmd + ["show", "known", "contract", c])
                .decode("utf-8")
                .strip()
            )
            return tz.contract(addr)

        return {
            "ctez": get_ctez_contract("ctez"),
            "fa12_ctez": get_ctez_contract("fa12_ctez"),
            "cfmm": get_ctez_contract("cfmm"),
            "fa12_lqt": get_ctez_contract("fa12_lqt"),
        }
