[![Build
Status](https://github.com/tezos-checker/huxian/workflows/CI/badge.svg)](https://github.com/tezos-checker/huxian/actions)
[![Docs](https://readthedocs.org/projects/checker/badge/?version=latest)](https://checker.readthedocs.io/en/latest/)

# Checker

Checker is an in-development "robocoin" system for the Tezos blockchain, and is
a project supported by [Nomadic Labs](https://nomadic-labs.com/),
[Tweag](https://tweag.io/), and [TZ Connect](https://www.tzconnect.com/en/).

**The code here is currently unverified and unaudited, and is made publicly
available only for exploration and discussion purposes. You should not use it
for anything serious.**

* Original technical (currently out-of-date) document can be found
  [here](https://hackmd.io/teMO2x9PRRy1iTBtrSMBvA?view).
* The source code lives in the [src](./src) folder.
* The tests live in the [tests](./tests) folder.

# Documentation

Various ad-hoc documentation can be found in the [docs](./docs) folder.

The full docs are taking shape under [docs/spec](./docs/spec): use `earthly
+spec` to build them. These docs are published at
[checker.readthedocs.io](https://checker.readthedocs.io/).

# Development

## Overview

Currently the team uses Docker to provide all
dependencies, including OCaml packages and appropriate (perhaps even patched)
versions of Ligo and other necessary tools.

All Docker images are stored on [GitHub container registry](https://github.com/orgs/tezos-checker/packages?repo_name=checker). Docker builds are orchestrated using [Earthly](https://earthly.dev/), with all image definitions residing in [./Earthfile](./Earthfile).

## Setup

We provide a development Docker container with all of the dependencies you need for building the project, so the minimum requirements for getting started are [Docker](https://docs.docker.com/get-docker/) and git. For development, you'll also need the
[ctez](https://github.com/tezos-checker/ctez) git submodule. To fetch all
submodules, run:

```console
$ git submodule update --init
```

With that out of the way you can start up the dev container. We use [Earthly](https://earthly.dev/) to
specify and build our Docker images. We recommend launching the dev container as follows. This gives
earthly access to your local Docker daemon for executing builds.

```console
docker run -it \
  -v /var/run/docker.sock:/var/run/docker.sock \
  -v $PWD:/checker \
  ghcr.io/tezos-checker/checker/dev
```

The rest of this guide assumes that you are running them in the dev container.

Note that the dev container also includes all language-specific tooling, so you can run language-specific
build commands as desired (e.g. `dune build .` or `poetry run <cmd>`). This can be helpful for ensuring that editor
integrations function properly (e.g. `ocaml-platform` for VSCode).

## Build

All of the build targets specified in the project [Earthfile](./Earthfile) may be built
within the dev container (or locally if you have earthly installed). Some helpful targets are:

* `earthly +build-ocaml` to build and compile the OCaml code (in
  [./src](./src)).
* `earthly +build-ligo` to generate ligo and michelson
  code (produces local output in `./generated`).
* `earthly +test` to run the full test suite.

For linting type:
* `earthly +lint`

For test coverage report using bisect_ppx, type:
* `earthly +test-coverage` (report in `./_coverage/index.html`)

For extracting (haddock-style) documentation from the code using dune, type:
* `earthly --artifact +docs/ ocaml-docs/`

For running the end-to-end tests (can take 10s of minutes to run), type:
* `earthly +test-e2e`

For running mutation tests, type:
* `earthly +test-mutations`
  * You may also specify the following build arguments (via `--build-arg`):
  * `n_mutations` (default=25): Number of mutations to perform
  * `modules` (default='src/burrow.ml src/checker.ml'): A list of src modules to perform mutations on.
  * `test_cmd` (default='dune build @run-fast-tests'): The test suite to run.


## Local Deployment [WIP]

The contract can be deployed to a local flextesa sandbox using the provided
[client library](./client).

Ensure that the submodules (ctez in particular) are up-to-date. You will
need to run this on your host if you don't have GitHub authentication set up in
the dev container.

```console
$ git pull --recurse-submodules
```

Then, within the dev container:

Generate the LIGO and Michelson code:

```console
$ earthly +build-ligo
```

Ensure that the client is up to date:
```console
$ poetry install
$ poetry shell
```

Use the client to start the sandbox and deploy the required ctez and mock oracle
contracts:

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
installed on your system and configured. Below are some instructions for setting
up from scratch; if you've done this already, you can skip to the instructions
"Deploy Checker" below.

### Initial `tezos-client` Download and Setup

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

3. Verify that you can connect to the network
```console
$ tezos-client bootstrapped
```

4. Get a json file from [the faucet (https://faucet.tzalpha.net/), if you don't
   have one already, and import it
```console
$ tezos-client activate account alice with tz1Ukue3ZGoNM6UY3mgGcrQnRqg68DsXECZC.json # your json file from the faucet here
```

5. Make sure to reveal your public key to the chain otherwise injecting
   operations won't work
```console
$ tezos-client reveal key for alice
```

### Deploy Checker

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
   - Set `tezos_address` to the endpoint URL above (in this case
     https://rpczero.tzbeta.net).
   - Set `tezos_key` to your private key (you can find this by typing
     `tezos-client show address alice -S`).
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
