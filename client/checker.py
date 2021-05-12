import os
import re
import json
import time
import shlex
import tempfile
import threading
import subprocess
from pprint import pprint

import pytezos

def _wait_or_bake(thing, bake):
     if bake is None:
        return thing.inject(min_confirmations=1, time_between_blocks=10)
     else:
        ret = thing.inject()
        time.sleep(2)
        bake()
        time.sleep(2)
        bake()
        time.sleep(2)
        return ret

def deploy_contract(tz, *, source_file, initial_storage, bake=None):
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
    )
    origination = _wait_or_bake(origination, bake)

    opg = tz.shell.blocks[origination["branch"]:].find_operation(origination["hash"])
    res = pytezos.operation.result.OperationResult.from_operation_group(opg)

    addr = res[0].originated_contracts[0]
    return tz.contract(addr)

def deploy_checker(tz, checker_dir, *, oracle, ctez, bake=None):
    print("Deploying the wrapper.")

    checker = deploy_contract(
      tz,
      source_file=os.path.join(checker_dir, "main.tz"),
      initial_storage=({}, { "unsealed": tz.key.public_key_hash() }),
      bake=bake
    )
    print("Checker address: {}".format(checker.context.address))

    with open(os.path.join(checker_dir, "functions.json")) as f:
        functions = json.load(f)

    # TODO: implement the batching logic here for speed (see the previous ruby script)
    for fun in functions["lazy_functions"]:
        print("Deploying: {}".format(fun["name"]))
        for chunk_no, chunk in enumerate(fun["chunks"]):
            arg = (int(fun["fn_id"]), "0x"+chunk)
            _wait_or_bake(
              (checker
                .deployFunction(arg)
                .as_transaction().autofill().sign()
              ),
              bake
            )
            print("  deployed: chunk {}.".format(chunk_no))

    print("Sealing.")
    _wait_or_bake(
      checker
         .sealContract((oracle, ctez))
         .as_transaction().autofill().sign(),
      bake
    )

    return checker

class BakeThread(threading.Thread):
    def __init__(self, bake, *args, **kwargs):
        threading.Thread.__init__(self, *args, **kwargs)
        self._stop = threading.Event()
        self.bake = bake

    def run(self):
        while not self._stop.is_set():
            time.sleep(1)
            self.bake()

    def stop(self):
        self._stop.set()
        time.sleep(1)

def deploy_ctez(tz, ctez_dir, bake=None):
   if bake is not None:
      bake_thread = BakeThread(bake)
      bake_thread.start()

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

        def get_ctez_contract(c):
           addr = subprocess.check_output(
             tc_cmd + [ "show", "known", "contract", c ]
           ).decode("utf-8").strip()
           return tz.contract(addr)

        if bake is not None:
            bake_thread.stop()

        return {
          "ctez": get_ctez_contract("ctez"),
          "fa12_ctez": get_ctez_contract("fa12_ctez"),
          "cfmm": get_ctez_contract("cfmm"),
          "fa12_lqt": get_ctez_contract("fa12_lqt"),
        }



