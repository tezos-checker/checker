import os
import re
import json
import shlex
import tempfile
import subprocess
from pprint import pprint

import pytezos

def deploy_contract(tz, *, source_file, initial_storage):
    script = (
       pytezos.ContractInterface
         .from_file(source_file)
         .script(initial_storage=initial_storage)
    )

    origination = (
      tz
        .origination(script)
        .autofill()
        .sign()
        .inject(min_confirmations=1, time_between_blocks=5)
    )

    opg = tz.shell.blocks[origination["branch"]:].find_operation(origination["hash"])
    res = pytezos.operation.result.OperationResult.from_operation_group(opg)

    addr = res[0].originated_contracts[0]
    return tz.contract(addr)

def deploy_checker(tz, checker_dir, *, oracle, ctez):
    print("Deploying the wrapper.")

    checker = deploy_contract(
      tz,
      source_file=os.path.join(checker_dir, "main.tz"),
      initial_storage=({}, { "unsealed": tz.key.public_key_hash() })
    )
    print("Checker address: {}".format(checker.context.address))

    with open(os.path.join(checker_dir, "functions.json")) as f:
        functions = json.load(f)

    # TODO: implement the batching logic here for speed (see the previous ruby script)
    for fun in functions["lazy_functions"]:
        print("Deploying: {}".format(fun["name"]))
        for chunk_no, chunk in enumerate(fun["chunks"]):
            arg = (int(fun["fn_id"]), "0x"+chunk)
            (checker
              .deployFunction(arg)
              .as_transaction().autofill().sign()
              .inject(min_confirmations=1, time_between_blocks=5)
            )
            print("  deployed: chunk {}.".format(chunk_no))

    print("Sealing.")
    (checker
       .sealContract((oracle, ctez))
       .as_transaction().autofill().sign()
       .inject(min_confirmations=1, time_between_blocks=5)
    )

    return checker

def deploy_ctez(tz, ctez_dir):
    with tempfile.TemporaryDirectory(suffix="-checker-ctez") as tmpdir:
        os.environ["TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER"] = "yes"
        os.mkdir(os.path.join(tmpdir, "tezos-client"))
        tc_cmd = [
          "tezos-client",
          "--base-dir", os.path.join(tmpdir, "tezos-client"),
          "--endpoint", tz.shell.node.uri
        ]

        subprocess.check_call(tc_cmd + ["bootstrapped"])
        subprocess.check_call(
          tc_cmd + [
            "import", "secret", "key",
            "bootstrap1",
            "unencrypted:{}".format(tz.context.key.secret_key())
        ])

        with open(os.path.join(ctez_dir, "deploy.sh"), "r") as deploy_script:
            with open(os.path.join(tmpdir, "deploy.sh"), "w") as new_deploy_script:
                new = deploy_script.read()

                new = re.sub(
                  "^TZC=.*",
                  "TZC=\"{}\"".format(shlex.join(tc_cmd)),
                  new,
                  flags=re.MULTILINE
                )

                new = re.sub(
                  ".*create mockup.*", "",
                  new,
                  flags=re.MULTILINE
                )

                new_deploy_script.write(new)

        subprocess.check_call([
          "bash",
          os.path.join(tmpdir, "deploy.sh")
        ], cwd=ctez_dir)

        addr = subprocess.check_output(
          tc_cmd + [ "show", "known", "contract", "fa12_ctez" ]
        ).decode("utf-8").strip()

        return tz.contract(addr)



