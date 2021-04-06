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

TODO: Add instructions here about how to patch `tezos-client`, how to run a sandbox, how to deploy the contract, and how to call it.
