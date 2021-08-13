[![Build Status](https://github.com/tezos-checker/huxian/workflows/CI/badge.svg)](https://github.com/tezos-checker/huxian/actions)
[![Docs](https://readthedocs.org/projects/checker/badge/?version=latest)](https://checker.readthedocs.io/en/latest/)

# Checker

Checker is an in-development "robocoin" system for the Tezos blockchain,
and is a project supported by [Nomadic Labs](https://nomadic-labs.com/),
[Tweag](https://tweag.io/), and [TZ Connect](https://www.tzconnect.com/en/).

**The code here is currently unverified and unaudited, and is made
publicly available only for exploration and discussion purposes.
You should not use it for anything serious.**

* Original technical (currently out-of-date) document can be found
  [here](https://hackmd.io/teMO2x9PRRy1iTBtrSMBvA?view).
* The source code lives in the [src](./src) folder.
* The tests live in the [tests](./tests) folder.

# Documentation

Various ad-hoc documentation can be found in the [docs](./docs) folder.

The full docs are taking shape under [docs/spec](./docs/spec): use `make spec`
to build them if you're in a Nix shell or have `sphinx` installed). These docs
are published at [checker.readthedocs.io](https://checker.readthedocs.io/).

## Development

Currently the team uses [Nix](https://nixos.org/) to provide all dependencies,
including OCaml packages and appropriate (perhaps even patched) versions of
Ligo and other necessary tools, so this is the recommended method. For the
curious, the dependencies are listed in `shell.nix`.

For development, you'll also need the [ctez](https://github.com/tezos-checker/ctez)
submodule. To fetch all submodules, run:
```console
$ git submodule update --init
```

Within a `nix-shell` (the first time this might take a while, since it must
fetch all dependencies), type

* `make build-ocaml` to build and compile the ocaml code (in [./src](./src)).
* `make generate-ligo` to generate the ligo code (in `./generated/ligo`).
* `make build-ligo` to generate the michelson code (in `./generated/michelson`).
* `make build` to do all of the above.
* `make test` to run all the (OCaml) tests ([./src/tests.ml](./src/tests.ml)). Note that this takes several minutes.
* `make fast-test` to run the fast (OCaml) tests
* `make` to do all the above.

For test coverage report using bisect_ppx, type
*  `make test-coverage` (report in `./_coverage/index.html`), or
*  `make test-coverage.json` (report of per-file summary in `./test-coverage.json`).

For extracting (haddock-style) documentation from the code using dune, type
*  `make docs` (docs entrypoint: `./_build/default/_doc/_html/index.html`)

For running the end-to-end tests, type:
```console
$ CHECKER_DIR=$(nix-build -A michelson --arg e2eTestsHack true --no-out-link) python e2e/main.py
```

## Local Deployment

The contract can be deployed to a local, Docker sandbox run using the provided
[client library](./client). Note that this workflow has only been tested on
Linux.

First, enter a nix shell:
```console
$ nix-shell
```

Generate the LIGO and Michelson code:

```console
$ make build-ligo
```

Ensure that the submodules (ctez in particular) are up-to-date:
```console
$ git pull --recurse-submodules
```

Use the client to start the sandbox and deploy the required ctez and mock
oracle contracts:

```console
$ checker sandbox start
$ checker deploy mock-oracle
$ checker deploy ctez
```

> Note: If no port is specified, the client will attempt to select a default
> one. To view the port number for use with tezos-client, etc. you can use:
> `checker show-config`.

And finally, deploy checker itself:
```console
$ checker deploy checker
```

## Deployment to a Testnet (Manually)

To be able to deploy checker on a testnet you need to have `tezos-client`
installed on your system and configured. Below are some instructions for
setting up from scratch; if you've done this already, you can skip to the
instructions "Deploy Checker" below.

Initial `tezos-client` Download and Setup
---

1. Install `tezos-client` (instructions taken from
   [here](https://assets.tqtezos.com/docs/setup/1-tezos-client/))
```console
$ wget https://github.com/serokell/tezos-packaging/releases/latest/download/tezos-client
$ chmod +x tezos-client
$ mkdir -p $HOME/.local/bin
$ mv tezos-client $HOME/.local/bin
$ echo 'export PATH="$HOME/.local/bin:$PATH"' >> $HOME/.bashrc
$ source $HOME/.bashrc
```

2. Set the node to connect to
```console
$ tezos-client --endpoint https://rpczero.tzbeta.net config update
```

3. Verify that the operation worked
```console
$ tezos-client bootstrapped
```

4. Get a json file from [the faucet (https://faucet.tzalpha.net/),
   if you don't have one already, and import it
```console
$ tezos-client activate account alice with tz1Ukue3ZGoNM6UY3mgGcrQnRqg68DsXECZC.json # your json file from the faucet here
```

5. Make sure to reveal your public key to the chain otherwise injecting operations won't work
```console
$ tezos-client reveal key for alice
```

Deploy Checker
---
This is the checker-specific setup that you need to configure and deploy it.


1. Enter a nix-shell
```console
$ nix-shell
```

2. Build checker
```console
$ make build
```

3. Find the path to CLI config file (usually at $HOME/.config/.checker)
```console
$ checker --help
```

4. Modify the CLI config file
   - Set `tezos_address` to the endpoint URL above (in this case https://rpczero.tzbeta.net).
   - Set `tezos_key` to your private key (you can find this by typing `tezos-client show address alice -S`).
   - Set `tezos_port` to 443 (i.e., standard https port).

5. If `ctez` is not already deployed, deploy one
```console
checker deploy ctez
```

6. If an oracle is not already deployed, you can deploy the mock oracle
```console
checker deploy mock-oracle
```

7. Finally, deploy checker
```console
$ checker deploy checker --ctez <ctez-fa12-address> --oracle <oracle-address>
```
After `--ctez` you need the FA1.2 address for a deployed ctez contract, and
after `--oracle` you need the address of a deployed oracle contract.  However,
if you had to do both `checker deploy ctez` and `checker deploy mock-oracle`,
then `checker deploy checker` should suffice (since both these instructions
update the CLI config file in place).
