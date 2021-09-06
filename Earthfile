all:
    # Lint
    BUILD +lint
    # Build checker
    BUILD +build-ocaml
    BUILD +build-ligo
    # Run additional test suites
    BUILD +ocaml-slow-tests
    BUILD +cli

spec:
    FROM ubuntu:21.04

    ENV DEBIAN_FRONTEND=noninteractive
    RUN apt update
    RUN apt install -y \
          make \
          python3-pip \
          python-is-python3

    RUN pip install --upgrade pip && pip install sphinx-rtd-theme
    COPY docs docs
    RUN make -C docs/spec html
    SAVE ARTIFACT docs/spec/_build/html /
    SAVE IMAGE --push ghcr.io/tezos-checker/checker/earthly-cache:spec

generate-entrypoints:
    FROM alpine:3.14
    RUN apk add ruby
    COPY ./scripts/generate-entrypoints.rb ./generate-entrypoints.rb
    COPY ./src/checker.mli checker.mli
    RUN ./generate-entrypoints.rb checker.mli > checkerEntrypoints.ml
    SAVE ARTIFACT checkerEntrypoints.ml AS LOCAL src/checkerEntrypoints.ml
    SAVE ARTIFACT checkerEntrypoints.ml /
    SAVE IMAGE --push ghcr.io/tezos-checker/checker/earthly-cache:generate-entrypoints

ocaml-base:
    FROM ubuntu:21.04

    ENV DEBIAN_FRONTEND=noninteractive
    RUN apt update
    RUN apt install -y \
        m4 \
        git \
        bash \
        opam \
        g++ \
        libgmp-dev \
        fd-find

    WORKDIR /root

    RUN opam init --disable-sandboxing --bare
    RUN opam switch create . ocaml-base-compiler.4.12.0

    COPY checker.opam ./
    RUN opam install -y --deps-only --with-test --locked=locked ./checker.opam
    SAVE IMAGE --push ghcr.io/tezos-checker/checker/earthly-cache:ocaml-base

lint:
    BUILD +lint-ocaml
    BUILD +lint-python

lint-ocaml:
    FROM +src-ocaml
    COPY .git .git
    COPY ./scripts/format-ocaml.sh .
    RUN opam exec -- ./format-ocaml.sh && \
        diff="$(git status --porcelain | grep ' M ')" bash -c 'if [ -n "$diff" ]; then echo "Some files require formatting, run \"scripts/format-ocaml.sh\":"; echo "$diff"; exit 1; fi'

lint-python:
    FROM +python-deps
    RUN apt install -y fd-find git
    COPY .git .git
    COPY ./scripts/format-python.sh .
    RUN poetry run ./format-python.sh && \
        diff="$(git status --porcelain | grep ' M ')" bash -c 'if [ -n "$diff" ]; then echo "Some files require formatting, run \"scripts/format-ocaml.sh\":"; echo "$diff"; exit 1; fi'

src-ocaml:
    FROM +ocaml-base
    COPY src/*.ml src/*.mli src/dune ./src/
    COPY +generate-entrypoints/checkerEntrypoints.ml ./src/
    COPY tests/*.ml tests/dune ./tests/
    COPY dune-project ./
    SAVE IMAGE --cache-hint

build-ocaml:
    FROM +src-ocaml
    RUN opam exec -- dune build @install
    RUN opam exec -- dune build @run-fast-tests
    SAVE IMAGE --push ghcr.io/tezos-checker/checker/earthly-cache:build-ocaml

ocaml-slow-tests:
    FROM +build-ocaml
    RUN opam exec -- dune build @run-avl-tests

test-coverage:
    FROM +build-ocaml
    RUN apt install -y jq gawk
    RUN opam exec -- dune runtest --instrument-with bisect_ppx --force .
    RUN opam exec -- bisect-ppx-report html
    RUN echo "$(opam exec -- bisect-ppx-report summary --per-file)"
    RUN opam exec -- bisect-ppx-report summary --per-file \
	  | awk '{ match($0, "^ *([0-9.]+) *% *[^ ]* *(.*)$", res); print res[1] "|" res[2] }' \
	  | jq -R "split(\"|\") | { \
	      \"value\": .[0] | tonumber, \
	      \"key\": (.[1] | if . == \"Project coverage\" then \"TOTAL\" else ltrimstr(\"src/\") end) \
	    }" \
	  | jq --sort-keys -s 'from_entries' \
	  | tee test-coverage.json
    SAVE ARTIFACT _coverage /_coverage
    SAVE ARTIFACT test-coverage.json /

mutation-tests:
    FROM +build-ocaml

    RUN apt install -y python3 python-is-python3

    # Note: If mutate.py ever depends on external packages we can also install them here
    # as we do for the e2e tests. Skipping this for now since it reduces the layer size and
    # is not currently required.

    ARG test_cmd = 'dune build @run-fast-tests'
    ARG n_mutations = "25"
    ARG modules = 'src/burrow.ml src/checker.ml'

    # Need git tree for restoring mutated src files
    COPY .git .git
    COPY scripts/mutate.py ./mutate.py
    RUN opam exec -- ./mutate.py --test "$test_cmd" --num-mutations "$n_mutations" $modules
    # SAVE IMAGE --push ghcr.io/tezos-checker/checker/cache/mutation-tests:master

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
    SAVE IMAGE --push ghcr.io/tezos-checker/checker/earthly-cache:build-ligo

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
    SAVE IMAGE --push ghcr.io/tezos-checker/checker/earthly-cache:python-deps

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

    RUN WRITE_GAS_PROFILES=$PWD/gas_profiles.json \
        WRITE_GAS_COSTS=$PWD/gas-costs.json \
        poetry run python ./e2e/main.py

    SAVE ARTIFACT gas_profiles.json /gas_profiles.json
    SAVE ARTIFACT gas-costs.json /gas-costs.json
    # SAVE IMAGE --push ghcr.io/tezos-checker/checker/cache/e2e:master

gas-profiles:
    FROM +python-deps
    COPY +e2e/gas_profiles.json .
    RUN poetry run python e2e/plot-gas-profiles.py gas_profiles.json --output auction-gas-profiles.png
    SAVE ARTIFACT auction-gas-profiles.png /auction-gas-profiles.png

cli:
    FROM +python-deps
    ENTRYPOINT poetry run checker
    SAVE IMAGE --push ghcr.io/tezos-checker/checker/checker-client:master

# Utilities

ligo-binary:
    FROM ghcr.io/tezos-checker/ligo:0.22.0-checker
    SAVE ARTIFACT /root/ligo ligo

zcash-params:
    FROM alpine:3.14
    RUN apk add curl wget
    RUN curl https://raw.githubusercontent.com/zcash/zcash/master/zcutil/fetch-params.sh | sh -
    SAVE ARTIFACT /root/.zcash-params /zcash-params
    SAVE IMAGE --cache-hint

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
    SAVE IMAGE --push ghcr.io/tezos-checker/checker/earthly-cache:flextesa
