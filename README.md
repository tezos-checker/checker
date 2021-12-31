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

Currently the team uses Docker to provide all dependencies, including OCaml
packages and appropriate (perhaps even patched) versions of Ligo and other
necessary tools.

All Docker images are stored on [GitHub container
registry](https://github.com/orgs/tezos-checker/packages?repo_name=checker).
Docker builds are orchestrated using [Earthly](https://earthly.dev/), with all
image definitions residing in [./Earthfile](./Earthfile).

## Setup

We provide a development Docker container with all of the dependencies you need
for building the project, so the minimum requirements for getting started are
[Docker](https://docs.docker.com/get-docker/) and git. For development, you'll
also need the [ctez](https://github.com/tezos-checker/ctez) git submodule. To
fetch all submodules, run:

```console
$ git submodule update --init
```

With that out of the way you can start up the dev container. We use
[Earthly](https://earthly.dev/) to specify and build our Docker images. We
recommend launching the dev container as follows. This gives earthly access to
your local Docker daemon for executing builds.

```console
docker run --rm -it \
  --name checker-dev-container \
  -e CHECKER_UID=$UID \
  -e DOCKER_GID=$(grep "docker" /etc/group | cut -d: -f3) \
  -v /var/run/docker.sock:/var/run/docker.sock \
  -v $PWD:/checker \
  ghcr.io/tezos-checker/checker/dev
```

The rest of this guide assumes that you are running commands in the dev container.

Note that the dev container also includes all language-specific tooling, so you
can run language-specific build commands as desired (e.g. `dune build .` or
`poetry run <cmd>`). This can be helpful for ensuring that editor integrations
function properly (e.g. `ocaml-platform` for VSCode).

## Build

### Configuration

Certain settings of Checker can be customized at *build-time*. These configurations
can be set by modifying the [checker.yaml](./checker.yaml) configuration file
in the repository root.

The configuration file checked into the repo is the source of truth used in CI
builds and tests. While these values can be adjusted, the full test suite
is not guaranteed to pass since unit tests for calculations are affected by
the values of the constants. Use with caution.

### Commands

All of the build targets specified in the project [Earthfile](./Earthfile) may
be built within the dev container (or locally if you have earthly installed).
Some helpful targets are:

* `earthly +build-ocaml` to build and compile the OCaml code (in
  [./src](./src)).
* `earthly +build-ligo` to generate ligo and michelson code (produces local
  output in `./generated`).
* `earthly +test` to run the full test suite.

For formatting type:
* `earthly +format` (formats in-place)
* `earthly +format-check` (checks formatting)

For test coverage report using bisect_ppx, type:
* `earthly +test-coverage` (report in `./_coverage/index.html`)

For extracting (haddock-style) documentation from the code using dune, type:
* `earthly +docs` (output in `./ocaml-docs`)

For building the spec documentation, type:
* `earthly +spec` (output in `./docs/spec/_build/html`)

For running the end-to-end tests (can take 10s of minutes to run), type:
* `earthly +test-e2e`

For running mutation tests, type:
* `earthly +test-mutations`
  * You may also specify the following build arguments (via `--build-arg`):
  * `n_mutations` (default=25): Number of mutations to perform
  * `modules` (default='src/burrow.ml src/checker.ml'): A list of src modules to
    perform mutations on.
  * `test_cmd` (default='dune build @run-fast-tests'): The test suite to run.


## Local Dev Deployment

The contract can be deployed to a local flextesa sandbox using the provided
[client library](./client).

Ensure that the submodules (ctez in particular) are up-to-date. You will need to
run this on your host if you don't have GitHub authentication set up in the dev
container.

```console
$ git pull --recurse-submodules
```

Then, within the dev container:

Generate the LIGO and Michelson code:

```console
$ earthly +build-ligo
```

By default, the dev container includes the latest version of the client library,
so you can call the `checker` command directly. Note that if you are actively
developing the client library, you'll want to make sure to prepend your local changes to the
`PYTHONPATH` variable (`export PYTHONPATH=$PWD/client:$PYTHONPATH`).

Use the client to start the sandbox. This will run interactively until
cancelled:

```console
$ checker sandbox start
```

In a new terminal, deploy the mock oracle and ctez contracts. To get
a new terminal manually (e.g. if not using VSCode), you can use:

```console
$ docker exec -it -u checker checker-dev-container bash
```

```console
$ checker deploy mock-oracle
$ checker deploy mock-cfmm-oracle
$ checker deploy ctez
$ checker deploy wrapped-ctez
$ checker deploy wtez
```

And finally, deploy checker itself:
```console
$ checker deploy checker \
      --cfmm_token_fa2 <cfmm-fa2-address> \
      --collateral_fa2 <collateral-fa2-address> \
      --oracle <mock-oracle-address>
```

# Deployment to a Testnet (Manually)

To be able to deploy checker on a testnet you need to have `tezos-client`
installed on your system and configured. Below are some instructions for setting
up from scratch; if you've done this already, you can skip to the instructions
"Deploy Checker" below.

## Initial `tezos-client` Download and Setup

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
$ tezos-client --endpoint https://granadanet.api.tez.ie config update
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

## Deploy Checker

While Checker can be deployed from within the dev container using the `checker`
CLI, the CLI is also available as a standalone Docker image which includes the
latest version of the contract and its dependencies. To use the image, you'll
want to mount your account's json file to the Docker container to ensure that it
is available to the CLI as shown in the examples below.

To deploy the version of the contract bundled in the CLI image, first make sure
that your account JSON file is available at `./my-account.json`.

Then, set your node address and port:

```console
  export NODE_ADDRESS=<desired-node-address>
  export NODE_PORT=<desired-node-port>
```

In this example, `<desired-node-address>` could be replaced with
 `https://hangzhounet.api.tez.ie` and `desired-node-port` could be replaced with
 `443` (i.e., the standard https port).

We provide a small wrapper to shorten the calls to docker:
`scripts/deploy-master.sh`. You can edit the docker command there if you need to
adjust any docker flags, etc.

To deploy different versions of Checker, you can set the `$VERSION` environment
variable to either `master` (the current HEAD of the master branch) or a
specific git commit on master (e.g. `d6ea806e3cf90009c45af51e2dc5a1595fc81d27`)

To deploy the contract:

```console
  ./scripts/deploy-master.sh checker \
      --cfmm_token_fa2 <cfmm-fa2-address> \
      --collateral_fa2 <collateral-fa2-address> \
      --ctez-cfmm <ctez-cfmm-address> \
      --oracle <oracle-address>
```

The CLI image bundles the corresponding versions of Checker's supporting
contracts in case you also need to deploy them.

To deploy ctez (vendored version):

```console
  ./scripts/deploy-master.sh ctez
```

To deploy the ctez FA2 wrapper (wctez):

```console
  ./scripts/deploy-master.sh wrapped-ctez --ctez_fa12 <ctez-fa12-address>
```

To deploy the mock oracle:

```console
  ./scripts/deploy-master.sh mock-oracle
```

To deploy the mock cfmm oracle:

```console
  ./scripts/deploy-master.sh mock-cfmm-oracle
```

To deploy the tez FA2 wrapper (wtez):

```console
  ./scripts/deploy-master.sh wtez
```
