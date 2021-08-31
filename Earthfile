generate-entrypoints:
    FROM alpine:3.14
    RUN apk add ruby
    COPY ./scripts/generate-entrypoints.rb ./generate-entrypoints.rb
    COPY ./src/checker.mli checker.mli
    RUN ./generate-entrypoints.rb checker.mli > checkerEntrypoints.ml
    SAVE ARTIFACT checkerEntrypoints.ml AS LOCAL src/checkerEntrypoints.ml
    SAVE ARTIFACT checkerEntrypoints.ml /

build-ocaml:
    FROM alpine:3.14
    RUN apk add make g++ m4 gmp-dev bash opam
    WORKDIR /root

    RUN opam init --disable-sandboxing --bare
    RUN opam switch create . ocaml-base-compiler.4.12.0

    COPY checker.opam ./
    RUN opam install -y --deps-only --with-test --locked=locked ./checker.opam

    COPY src/*.ml src/*.mli src/dune ./src/
    COPY +generate-entrypoints/checkerEntrypoints.ml ./src/
    COPY tests/*.ml tests/dune ./tests/
    COPY dune-project ./

    RUN opam exec -- dune build @install
    RUN opam exec -- dune build @run-fast-tests

ocaml-slow-tests:
    FROM +build-ocaml
    RUN opam exec -- dune build @run-avl-tests

build-ligo:
    FROM alpine:3.14
    RUN apk add bash ruby ruby-etc ruby-json

    COPY +ligo-binary/ligo /bin/ligo

    WORKDIR /root

    COPY ./src/*.ml ./src/*.mligo ./src/
    COPY +generate-entrypoints/checkerEntrypoints.ml ./src/checkerEntrypoints.ml

    COPY ./scripts/compile-ligo.rb ./scripts/
    COPY ./scripts/generate-ligo.sh ./scripts/

    ARG E2E_TESTS_HACK=""
    IF [[ "$E2E_TESTS_HACK" = "true" ]]
       RUN apk add patch
       COPY ./patches/e2e-tests-hack.patch .
       RUN patch -p1 <e2e-tests-hack.patch
    END

    RUN ./scripts/generate-ligo.sh
    RUN ./scripts/compile-ligo.rb

    SAVE ARTIFACT ./generated/michelson/* /

python-deps:
    FROM ubuntu:21.04

    ENV DEBIAN_FRONTEND=noninteractive
    RUN apt update

    RUN apt install -y \
          pkg-config autoconf libtool libev4 \
          libgmp-dev openssl libsodium23 libsodium-dev \
          python3-pip python-is-python3

    # install poetry
    RUN pip install --upgrade pip
    ARG POETRY_VERSION="==1.1.8"
    RUN pip install poetry"$POETRY_VERSION"

    WORKDIR /root

    # We only copy the files necessary for the build to succeed.
    COPY pyproject.toml poetry.lock ./
    COPY ./e2e ./e2e
    COPY ./client ./client
    RUN poetry install
    RUN rm -rf e2e/ client/ pyproject.toml poetry.lock

e2e:
    FROM +python-deps
    ENV DEBIAN_FRONTEND=noninteractive

    RUN apt install -y \
          openssl libsodium23 libsodium-dev \
          curl net-tools libhidapi-dev \
          python3-pip python-is-python3

    # bring ligo, which is required for ctez deployment
    COPY +ligo-binary/ligo /bin/ligo

    # Bring ZCash parameters necessary for the node
    COPY +zcash-params/zcash-params /root/.zcash-params

    WORKDIR /root
    COPY pyproject.toml poetry.lock ./
    COPY ./e2e ./e2e
    COPY ./client ./client

    RUN poetry install

    COPY ./vendor/ctez ./vendor/ctez
    COPY ./util/mock_oracle.tz ./util/

    # Bring flextesa + tezos-* binaries, which are required by the checker client
    COPY +flextesa/* /usr/bin/

    COPY --build-arg E2E_TESTS_HACK=true +build-ligo/ ./generated/michelson

    RUN poetry run python ./e2e/main.py

cli:
    FROM +python-deps

    WORKDIR /root
    COPY pyproject.toml poetry.lock ./
    COPY ./e2e ./e2e
    COPY ./client ./client

    RUN poetry install checker-client

    # TODO

# Utilities

ligo-binary:
    FROM ghcr.io/tezos-checker/ligo:0.22.0-checker
    SAVE ARTIFACT /root/ligo ligo

zcash-params:
    FROM alpine:3.14
    RUN apk add curl wget
    RUN curl https://raw.githubusercontent.com/zcash/zcash/master/zcutil/fetch-params.sh | sh -
    SAVE ARTIFACT /root/.zcash-params /zcash-params

# TODO: We might be able to remove this section dependening on whether we want to stick with flextesa.
# tezos-binaries:
#     FROM ubuntu:21.04

#     ENV DEBIAN_FRONTEND=noninteractive
#     RUN apt update
#     RUN apt install -y curl libgmp-dev git bash opam

#     RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs > /usr/bin/rustup-init
#     RUN chmod +x /usr/bin/rustup-init

#     WORKDIR /root
#     RUN git clone https://gitlab.com/tezos/tezos.git -b v10-release --depth 1

#     WORKDIR /root/tezos

#     RUN ln -sf /bin/bash /bin/sh
#     RUN source scripts/version.sh; rustup-init --profile minimal --default-toolchain $recommended_rust_version -y

#     RUN opam init --disable-sandboxing --bare
#     RUN source scripts/version.sh; opam repository add tezos --dont-select "$opam_repository"

#     RUN OPAMSWITCH=for_tezos
#     RUN source scripts/version.sh; opam switch create . $ocaml_version --repositories=tezos

#     RUN source $HOME/.cargo/env; OPAMSOLVERTIMEOUT=2400 opam exec -- make build-deps
#     RUN source $HOME/.cargo/env; opam exec -- make
#     RUN source $HOME/.cargo/env; opam exec -- make build-sandbox

#     SAVE ARTIFACT tezos-* /

#     SAVE IMAGE tezos-sandbox:latest

flextesa:
    FROM ubuntu:21.04

    ENV DEBIAN_FRONTEND=noninteractive
    RUN apt update
    RUN apt install -y \
        curl \
        git \
        bash \
        opam \
        pkg-config \
        cargo \
        autoconf \
        zlib1g-dev \
        libev-dev \
        libffi-dev \
        libusb-dev \
        libhidapi-dev \
        libgmp-dev
    # TODO: Clear package cache here

    # Install rust (required by tezos)
    RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs > /usr/bin/rustup-init && chmod +x /usr/bin/rustup-init
    RUN rustup-init -y

    # Checkout flextesa
    WORKDIR /root
    RUN git clone https://gitlab.com/tezos/flextesa.git
    WORKDIR /root/flextesa

    ARG FLEXTESA_REV = "95a2badf3b226b121f6281f98d86aaee1de5b6e8"

    RUN git checkout "$FLEXTESA_REV"

    # Create opam switch and install flextesa deps
    ARG OCAML_VERSION = "4.10.2"
    ENV OPAM_SWITCH="flextesa"
    RUN opam init --disable-sandboxing --bare
    RUN opam switch create "$OPAM_SWITCH" "$OCAML_VERSION"
    RUN opam switch import src/tezos-master.opam-switch

    # ENV PATH="/root/.opam/$OPAM_SWITCH/bin:$PATH"

    # Build flextesa
    RUN eval $(opam env) && make vendors
    RUN eval $(opam env) && \
        export CAML_LD_LIBRARY_PATH="/root/.opam/$OPAM_SWITCH/lib/tezos-rust-libs:$CAML_LD_LIBRARY_PATH" && \
        make build

    # Unfortunately unless we want write logic to get the top N protocol exes this will
    # have to be updated whenever we bump the protocol in our end to end tests.
    ARG PROTO_DIR = "010_PtGRANAD"

    # Build tezos exes which are required by flextesa at runtime
    RUN eval $(opam env) && \
        export CAML_LD_LIBRARY_PATH="/root/.opam/$OPAM_SWITCH/lib/tezos-rust-libs:$CAML_LD_LIBRARY_PATH" && \
        cd local-vendor/tezos-master && \
        dune build src/bin_node/main.exe && \
        dune build src/bin_client/main_client.exe && \
        dune build "src/proto_$PROTO_DIR/bin_baker/main_baker_$PROTO_DIR.exe" && \
        dune build "src/proto_$PROTO_DIR/bin_baker/main_baker_$PROTO_DIR.exe" && \
        dune build "src/proto_$PROTO_DIR/bin_endorser/main_endorser_$PROTO_DIR.exe" && \
        dune build "src/proto_$PROTO_DIR/bin_accuser/main_accuser_$PROTO_DIR.exe"

    # Create expected binary names
    RUN cp "local-vendor/tezos-master/_build/default/src/bin_node/main.exe" ./tezos-node && \
        cp "local-vendor/tezos-master/_build/default/src/bin_client/main_client.exe" ./tezos-client && \
        cp "local-vendor/tezos-master/_build/default/src/proto_$PROTO_DIR/bin_baker/main_baker_$PROTO_DIR.exe" "./tezos-baker-$(echo $PROTO_DIR | tr '_' '-')" && \
        cp "local-vendor/tezos-master/_build/default/src/proto_$PROTO_DIR/bin_endorser/main_endorser_$PROTO_DIR.exe" "./tezos-endorser-$(echo $PROTO_DIR | tr '_' '-')" && \
        cp "local-vendor/tezos-master/_build/default/src/proto_$PROTO_DIR/bin_accuser/main_accuser_$PROTO_DIR.exe" "./tezos-accuser-$(echo $PROTO_DIR | tr '_' '-')"

    SAVE ARTIFACT tezos-* /
    SAVE ARTIFACT flextesa /
