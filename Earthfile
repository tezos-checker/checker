all:
    BUILD +format
    BUILD +build
    BUILD +test
    BUILD +cli
    BUILD +dev-container

# =============================================================================
# Base images
# =============================================================================
# Base image for building project
builder:
    FROM ubuntu:21.04
    ENV DEBIAN_FRONTEND=noninteractive
    RUN apt update
    RUN apt install -y \
            autoconf \
            bash \
            curl \
            fd-find \
            gawk \
            git \
            g++ \
            jq \
            libev4 \
            libgmp-dev \
            libhidapi-dev \
            libsodium-dev \
            libsodium23 \
            libtool \
            make \
            m4 \
            net-tools \
            opam \
            openssl \
            pkg-config \
            python-is-python3 \
            python3-pip \
            ruby \
            ruby-json

    # Update language-specific package managers
    RUN pip install --upgrade pip
    ARG POETRY_VERSION="==1.1.8"
    RUN pip install poetry"$POETRY_VERSION"

    RUN opam init --disable-sandboxing --bare
    RUN opam update --all

    WORKDIR /root

    # Image for inline caching
    SAVE IMAGE --push ghcr.io/tezos-checker/checker/earthly-cache:builder

deps-ocaml:
    FROM +builder
    RUN opam switch create . ocaml-base-compiler.4.12.0
    COPY checker.opam ./
    RUN opam install -y --deps-only --with-test --locked=locked ./checker.opam
    # Image for inline caching
    SAVE IMAGE --push ghcr.io/tezos-checker/checker/earthly-cache:deps-ocaml

# Note: nesting this below `deps-ocaml` since it is likely to change more often
deps-full:
    FROM +deps-ocaml
    COPY pyproject.toml poetry.lock ./
    COPY ./e2e ./e2e
    COPY ./client ./client
    RUN poetry install
    # Image for inline caching
    SAVE IMAGE --push ghcr.io/tezos-checker/checker/earthly-cache:deps-full

# =============================================================================
# Documentation
# =============================================================================
docs:
    FROM +build-ocaml
    RUN opam exec -- dune build @doc
    SAVE ARTIFACT _build/default/_doc/_html AS LOCAL ./ocaml-docs
    SAVE ARTIFACT _build/default/_doc/_html /ocaml-docs

spec:
    FROM +builder
    RUN pip install sphinx-rtd-theme
    COPY docs docs
    RUN make -C docs/spec html
    SAVE ARTIFACT docs/spec/_build/html AS LOCAL docs/spec/_build/html
    SAVE ARTIFACT docs/spec/_build/html /
    # Image for inline caching
    SAVE IMAGE --push ghcr.io/tezos-checker/checker/earthly-cache:spec

# =============================================================================
# Formatting
# =============================================================================
format:
    BUILD +format-ocaml
    BUILD +format-python

format-ocaml:
    FROM +deps-ocaml
    COPY ./src ./src
    COPY ./tests ./tests
    COPY ./scripts/format-ocaml.sh .
    RUN opam exec -- ./format-ocaml.sh
    SAVE ARTIFACT src AS LOCAL src
    SAVE ARTIFACT tests AS LOCAL tests

format-python:
    FROM +deps-full
    COPY ./scripts ./scripts
    RUN poetry run ./scripts/format-python.sh
    SAVE ARTIFACT scripts AS LOCAL ./scripts
    SAVE ARTIFACT client AS LOCAL ./client
    SAVE ARTIFACT e2e AS LOCAL ./e2e

format-check:
    BUILD +format-ocaml-check
    BUILD +format-python-check

format-ocaml-check:
    FROM +deps-ocaml
    COPY ./src ./src
    COPY ./tests ./tests
    COPY .git .git
    COPY ./scripts/format-ocaml.sh .
    RUN opam exec -- ./format-ocaml.sh && \
        diff="$(git status --porcelain | grep ' M ')" bash -c 'if [ -n "$diff" ]; then echo "Some files require formatting, run \"scripts/format-ocaml.sh\":"; echo "$diff"; exit 1; fi'

format-python-check:
    FROM +deps-full
    COPY .git .git
    COPY ./scripts ./scripts
    RUN poetry run ./scripts/format-python.sh && \
        diff="$(git status --porcelain | grep ' M ')" bash -c 'if [ -n "$diff" ]; then echo "Some files require formatting, run \"scripts/format-python.sh\":"; echo "$diff"; exit 1; fi'

# =============================================================================
# Build & Tests
# =============================================================================
build:
    BUILD +build-ocaml
    BUILD +build-ligo

test:
    BUILD +test-ocaml
    # In the future if we add python unit tests, etc. we can call their target(s) here

generate-entrypoints:
    FROM +deps-ocaml
    COPY ./scripts/generate-entrypoints.rb ./generate-entrypoints.rb
    COPY ./src/checker.mli checker.mli
    RUN ./generate-entrypoints.rb checker.mli > checkerEntrypoints.ml
    # Ensure that the generated module obeys formatting rules:
    RUN opam exec -- ocp-indent -i checkerEntrypoints.ml
    SAVE ARTIFACT checkerEntrypoints.ml AS LOCAL src/checkerEntrypoints.ml
    SAVE ARTIFACT checkerEntrypoints.ml /
    # Image for inline caching
    SAVE IMAGE --push ghcr.io/tezos-checker/checker/earthly-cache:generate-entrypoints

build-ocaml:
    FROM +deps-ocaml
    COPY src/*.ml src/*.mli src/dune ./src/
    COPY +generate-entrypoints/checkerEntrypoints.ml ./src/
    COPY tests/*.ml tests/dune ./tests/
    COPY dune-project ./
    RUN opam exec -- dune build @install
    # Image for inline caching
    SAVE IMAGE --push ghcr.io/tezos-checker/checker/earthly-cache:build-ocaml

build-ligo:
    FROM +builder

    COPY +ligo-binary/ligo /bin/ligo

    WORKDIR /root

    COPY ./src/*.ml ./src/*.mligo ./src/
    COPY +generate-entrypoints/checkerEntrypoints.ml ./src/checkerEntrypoints.ml

    COPY ./scripts/compile-ligo.rb ./scripts/
    COPY ./scripts/generate-ligo.sh ./scripts/
    COPY ./patches/e2e-tests-hack.patch .

    ARG E2E_TESTS_HACK=""

    # Note: using bash if-then here instead of earthly's IF-END because the earthly
    # version was flaky as of version v0.5.23
    RUN bash -c 'if [ "$E2E_TESTS_HACK" = "true" ]; then patch -p1 <e2e-tests-hack.patch; fi'

    RUN ./scripts/generate-ligo.sh
    RUN ./scripts/compile-ligo.rb

    SAVE ARTIFACT ./generated/ligo /ligo
    SAVE ARTIFACT ./generated/michelson /michelson
    SAVE ARTIFACT ./generated/ligo AS LOCAL ./generated/ligo
    SAVE ARTIFACT ./generated/michelson AS LOCAL ./generated/michelson
    SAVE IMAGE --push ghcr.io/tezos-checker/checker/earthly-cache:build-ligo

test-ocaml:
    FROM +build-ocaml
    COPY ./scripts/ensure-unique-errors.sh ./scripts/
    RUN bash ./scripts/ensure-unique-errors.sh
    RUN opam exec -- dune runtest .

test-ocaml-fast:
    FROM +build-ocaml
    COPY ./scripts/ensure-unique-errors.sh ./scripts/
    RUN bash ./scripts/ensure-unique-errors.sh
    RUN opam exec -- dune build @run-fast-tests

test-coverage:
    FROM +build-ocaml
    COPY ./scripts/ensure-unique-errors.sh ./scripts/
    RUN bash ./scripts/ensure-unique-errors.sh
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
    SAVE ARTIFACT _coverage AS LOCAL ./_coverage
    SAVE ARTIFACT test-coverage.json AS LOCAL test-coverage.json
    SAVE ARTIFACT _coverage /_coverage
    SAVE ARTIFACT test-coverage.json /

test-e2e:
    FROM +deps-full
    # Bring ligo, which is required for ctez deployment
    COPY +ligo-binary/ligo /bin/ligo
    # Bring ZCash parameters necessary for the node
    COPY +zcash-params/zcash-params /root/.zcash-params
    # Bring ctez contract and mock oracle (for running checker in sandbox)
    COPY ./vendor/ctez ./vendor/ctez
    COPY ./util/mock_oracle.tz ./util/
    # Bring flextesa + tezos-* binaries, which are required by the checker client
    COPY +flextesa/* /usr/bin/
    # And the checker contract itself
    COPY --build-arg E2E_TESTS_HACK=true +build-ligo/michelson ./generated/michelson
    RUN WRITE_GAS_PROFILES=$PWD/gas_profiles.json \
        WRITE_GAS_COSTS=$PWD/gas-costs.json \
        poetry run python ./e2e/main.py

    RUN poetry run python e2e/plot-gas-profiles.py gas_profiles.json --output auction-gas-profiles.png

    SAVE ARTIFACT gas_profiles.json /gas_profiles.json
    SAVE ARTIFACT gas-costs.json /gas-costs.json
    SAVE ARTIFACT auction-gas-profiles.png /auction-gas-profiles.png

test-mutations:
    FROM +build-ocaml

    ARG test_cmd = 'dune build @run-fast-tests'
    ARG n_mutations = "25"
    ARG modules = 'src/burrow.ml src/checker.ml'

    # Need git tree for restoring mutated src files
    COPY .git .git
    COPY scripts/mutate.py ./mutate.py
    RUN opam exec -- ./mutate.py --test "$test_cmd" --num-mutations "$n_mutations" $modules

# =============================================================================
# Other artifacts
# =============================================================================
dev-container:
    FROM +deps-full

    # Extra dependencies for development
    RUN apt install -y \
        apt-transport-https \
        ca-certificates \
        curl \
        gnupg \
        lsb-release \
        wget

    # Ensure interactive terminal is also already in correct opam switch env
    RUN echo 'eval $(opam env --switch=/root --set-switch)' >> /root/.bashrc
    RUN echo 'eval $(opam env --switch=/root --set-switch); /bin/bash $@' > entrypoint.sh && chmod +x entrypoint.sh

    # Install docker
    ARG TARGETARCH
    RUN curl -fsSL https://download.docker.com/linux/ubuntu/gpg | gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
    RUN echo "deb [arch=$TARGETARCH signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu \
        $(lsb_release -cs) stable" | tee /etc/apt/sources.list.d/docker.list > /dev/null
    RUN apt update && \
        apt install -y docker-ce docker-ce-cli containerd.io && \
        (getent group docker || groupadd docker) && \
        usermod -aG docker root

    # Install earthly.
    # ** Note: earthly will only be usable if the container is launched with access to docker,
    #    e.g. via mounting the host docker socket
    # **
    RUN wget "https://github.com/earthly/earthly/releases/download/v0.5.23/earthly-linux-$TARGETARCH" -O /usr/local/bin/earthly && chmod +x /usr/local/bin/earthly

    # Extra useful applications for development
    COPY +ligo-binary/ligo /bin/ligo
    COPY +zcash-params/zcash-params /root/.zcash-params
    COPY +flextesa/* /usr/bin/

    RUN mkdir /checker
    WORKDIR /checker
    # Ensure that we restore the debian frontend to dialog since the dev container
    # should be interactive.
    ENV DEBIAN_FRONTEND=dialog
    # Set earthly to use caching by default
    ENV EARTHLY_USE_INLINE_CACHE=true

    ENTRYPOINT /root/entrypoint.sh
    ARG TAG_DEV_CONTAINER = "latest"
    # Local image
    SAVE IMAGE checker/dev:latest
    # Published image
    SAVE IMAGE --push ghcr.io/tezos-checker/checker/dev:$TAG_DEV_CONTAINER

# Note: Building CLI independently so that it doesn't include the full closure of all
# of our dev dependencies
cli:
    FROM ubuntu:21.04

    ENV DEBIAN_FRONTEND=noninteractive
    RUN apt update
    RUN apt install -y \
          pkg-config autoconf libtool libev4 \
          libgmp-dev openssl libsodium23 libsodium-dev \
          python3-pip python-is-python3

    RUN pip install --upgrade pip
    ARG POETRY_VERSION="==1.1.8"
    RUN pip install poetry"$POETRY_VERSION"

    WORKDIR /root

    COPY +ligo-binary/ligo /bin/ligo
    COPY +flextesa/* /usr/bin/
    COPY +zcash-params/zcash-params /root/.zcash-params
    COPY ./vendor/ctez ./vendor/ctez
    COPY ./util/mock_oracle.tz ./util/

    # Baking in the current version of Checker for convenience
    COPY +build-ligo/michelson ./generated/michelson

    COPY ./client .
    RUN poetry config virtualenvs.in-project true && poetry install

    # Required dir for pytezos
    RUN mkdir /root/.tezos-client
    ENV PATH="/root/.venv/bin:$PATH"
    CMD checker
    # Local image
    SAVE IMAGE checker-client:latest
    # Published image
    SAVE IMAGE --push ghcr.io/tezos-checker/checker/checker-client:master

# =============================================================================
# Utilities
# =============================================================================
ligo-binary:
    FROM ghcr.io/tezos-checker/ligo:0.22.0-checker
    SAVE ARTIFACT /root/ligo ligo

zcash-params:
    FROM alpine:3.14
    RUN apk add curl wget
    RUN curl https://raw.githubusercontent.com/zcash/zcash/master/zcutil/fetch-params.sh | sh -
    SAVE ARTIFACT /root/.zcash-params /zcash-params
    SAVE IMAGE --push ghcr.io/tezos-checker/checker/earthly-cache:zcash-params

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
