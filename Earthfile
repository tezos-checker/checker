generate-entrypoints:
    FROM alpine:3.14
    RUN apk add ruby
    COPY ./scripts/generate-entrypoints.rb ./generate-entrypoints.rb
    COPY ./src/checker.mli checker.mli
    RUN ./generate-entrypoints.rb checker.mli > checkerEntrypoints.ml
    SAVE ARTIFACT checkerEntrypoints.ml AS LOCAL src/checkerEntrypoints.ml

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

e2e:
    FROM ubuntu:21.04

    ENV DEBIAN_FRONTEND=noninteractive
    RUN apt update

    # Download ZCash parameters necessary for the node
    # We do this as early as possible so changing anything else does not invalidate the cache.
    RUN apt install -y curl
    RUN curl https://raw.githubusercontent.com/zcash/zcash/master/zcutil/fetch-params.sh | sh -

    RUN apt install -y \
          pkg-config autoconf libtool libev4 \
          libgmp-dev openssl libsodium23 libsodium-dev \
          curl net-tools libhidapi-dev \
          python3-pip python-is-python3

    # bring ligo, which is required for ctez deployment
    COPY +ligo-binary/ligo /bin/ligo

    # install poetry
    RUN pip install --upgrade pip
    RUN pip install poetry

    WORKDIR /root
    COPY pyproject.toml poetry.lock ./
    COPY ./e2e ./e2e
    COPY ./client ./client

    RUN poetry install

    COPY ./vendor/ctez ./vendor/ctez
    COPY ./util/mock_oracle.tz ./util/

    COPY +tezos-binaries/* /usr/bin/

    COPY --build-arg E2E_TESTS_HACK=true +build-ligo/ ./generated/michelson

    RUN poetry run python ./e2e/main.py

# Utilities

ligo-binary:
    FROM ghcr.io/tezos-checker/ligo:0.22.0-checker
    SAVE ARTIFACT /root/ligo ligo

tezos-binaries:
    FROM ubuntu:21.04

    ENV DEBIAN_FRONTEND=noninteractive
    RUN apt update
    RUN apt install -y curl libgmp-dev git bash opam

    RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs > /usr/bin/rustup-init
    RUN chmod +x /usr/bin/rustup-init

    WORKDIR /root
    RUN git clone https://gitlab.com/tezos/tezos.git -b v10-release --depth 1

    WORKDIR /root/tezos

    RUN ln -sf /bin/bash /bin/sh
    RUN source scripts/version.sh; rustup-init --profile minimal --default-toolchain $recommended_rust_version -y

    RUN opam init --disable-sandboxing --bare
    RUN source scripts/version.sh; opam repository add tezos --dont-select "$opam_repository"

    RUN OPAMSWITCH=for_tezos
    RUN source scripts/version.sh; opam switch create . $ocaml_version --repositories=tezos

    RUN source $HOME/.cargo/env; OPAMSOLVERTIMEOUT=2400 opam exec -- make build-deps
    RUN source $HOME/.cargo/env; opam exec -- make
    RUN source $HOME/.cargo/env; opam exec -- make build-sandbox

    SAVE ARTIFACT tezos-* /



