[![Build Status](https://github.com/tezos-checker/huxian/workflows/CI/badge.svg)](https://github.com/tezos-checker/huxian/actions)
[![Docs](https://readthedocs.org/projects/checker/badge/?version=latest)](https://checker.readthedocs.io/en/latest/)

# Checker

Checker is an in-development "robocoin" system for the Tezos blockchain,
and is a project supported by [Nomadic Labs](https://nomadic-labs.com/),
[Tweag](https://tweag.io/) and TZ Connect Berlin.

**The code here is currently unverified and unaudited, and is made
publicly available only for exploration and discussion purposes.
You should not use it for anything serious.**

* Technical document can be found [here](https://hackmd.io/teMO2x9PRRy1iTBtrSMBvA?view).
* The source code lives in the [src](./src) folder.
* The tests live in the [tests](./tests) folder.

# Documentation

Various ad-hoc documentation can be found in the [docs](./docs) folder.

The full docs are taking shape under [docs/spec](./docs/spec): use `make spec` to build them if you're in a Nix shell or have `sphinx` installed). These docs are published at [checker.readthedocs.io](https://checker.readthedocs.io/).

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

The contract can be deployed to a local, Docker sandbox run using the provided [client library](./client). Note that this workflow has only been tested on Linux.

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

Use the client to start the sandbox and deploy the required ctez and mock oracle contracts:

```console
$ checker sandbox start
$ checker deploy mock-oracle
$ checker deploy ctez
```

> Note: If no port is specified, the client will attempt to select a default one. To view the port number for
> use with tezos-client, etc. you can use: `checker show-config`.

And finally, deploy checker itself:
```console
$ checker deploy checker
```
