import os
import json
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
