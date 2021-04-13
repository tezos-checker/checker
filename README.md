[![Build Status](https://github.com/tzConnectBerlin/huxian/workflows/CI/badge.svg)](https://github.com/tzConnectBerlin/huxian/actions)

# Huxian

* Technical document can be found [here](https://hackmd.io/teMO2x9PRRy1iTBtrSMBvA?view).
* The source code lives in the [src](./src) folder.
* The tests live in the [tests](./tests) folder.
* Documentation, quite underdeveloped at the moment, lives in the [docs](./docs) folder.

## Development

Currently the team uses [Nix](https://nixos.org/) to provide all dependencies, including OCaml packages and appropriate (perhaps even patched) versions of Ligo and other necessary tools, so this is the recommended method. For the curious, the dependencies are listed in `shell.nix`.

Within a `nix-shell` (the first time this might take a while, since it must fetch all dependencies), type

* `make build-ocaml` to build and compile the ocaml code (in [./src](./src))
* `make generate-ligo` to generate the ligo code (in [./generated/ligo](./generated/ligo))
* `make build-ligo` to generate the michelson code (in [./generated/michelson](./generated/michelson))
* `make build` to do all of the above.
* `make test` to run all the (OCaml) tests ([./src/tests.ml](./src/tests.ml)). (note: this might take a while. If you're in a hurry you might want to comment out `TestAvl.suite` in `tests.ml` temporarily).
* `make` to do all the above.

For test coverage report using bisect_ppx, type
*  `make test-coverage` (report in `./_coverage/index.html`)

For extracting (haddock-style) documentation from the code using dune, type
*  `make docs` (docs entrypoint: `./_build/default/_doc/_html/index.html`)

## Local Deployment

The contract can be deployed to a local sandbox run using Docker. Note that this workflow has only been tested on Linux.

First, enter a nix shell:
```console
$ nix-shell
```

Generate the LIGO and Michelson code:

```console
$ make build-ligo
```

Start the Tezos sandbox. This will run a couple of Tezos nodes in a local network using Docker.

```console
$ ./scripts/run-sandbox.sh
```

Since the sandbox runs in the foreground, open another terminal and enter a nix shell:
```console
$ nix-shell
```

The first time you deploy the contract locally, you will need to configure your local tezos-client to use the sandbox. This step can be skipped when you want to
re-deploy the contract later.

```console
## Clean-up left-over configuration

$ tezos-client config reset

## Configure the client to communicate with the sandbox

$ tezos-client --endpoint http://localhost:20000 bootstrapped
$ tezos-client --endpoint http://localhost:20000 config update

## Import the secret keys for two pre-existing accounts, alice and bob:

$ ./scripts/setup-keys.sh
```

Then, compile the packed entrypoints of the contract:
```console
$ ./scripts/compile-lazyfuns.sh
```

And finally, deploy the contract (and packed entrypoints) to the sandbox:

```console
$ ./scripts/deploy-contract.sh
```

To call the contract, we can use the [call-contract.sh](./scripts/call-contract.sh) helper script:

```console
$ ./scripts/call-contract.sh 'Touch ()'
```
