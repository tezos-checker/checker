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
    # RUN opam exec -- dune build @run-avl-tests

build-ligo:
    FROM alpine:3.14
    RUN apk add bash ruby ruby-etc ruby-json

    COPY +ligo-binary/ligo /bin/ligo

    WORKDIR /root

    COPY ./src/*.ml ./src/*.mligo ./src/
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
    FROM earthly/dind:alpine

    RUN apk add make \
          g++ linux-headers libtool autoconf automake \
          zeromq-dev libffi-dev gmp-dev zlib-dev jpeg-dev \
          python3-dev py3-pip
    RUN pip install poetry

    COPY +ligo-binary/ligo /bin/ligo

    WORKDIR /root
    COPY pyproject.toml poetry.lock ./
    COPY ./e2e ./e2e
    COPY ./client ./client
    RUN poetry install

    COPY ./vendor/ctez ./vendor/ctez
    COPY ./util/mock_oracle.tz ./util/

    COPY --build-arg E2E_TESTS_HACK=true +build-ligo/ ./generated/michelson

    WITH DOCKER --pull tqtezos/flextesa:20210514
        RUN poetry run python ./e2e/main.py
    END

# Utilities

ligo-binary:
    FROM ghcr.io/tezos-checker/ligo:0.22.0-checker
    SAVE ARTIFACT /root/ligo ligo

tezos-binaries:
    FROM alpine:3.14

    RUN apk add make g++ m4 gmp-dev bash rustup opam git

    WORKDIR /root
    RUN git clone https://gitlab.com/tezos/tezos -b v10.0-rc3 --depth 1

    WORKDIR /root/tezos

    RUN source scripts/version.sh; rustup-init --profile minimal --default-toolchain $recommended_rust_version -y

    RUN opam init --disable-sandboxing --bare
    RUN source scripts/version.sh; opam switch create . $ocaml_version
    RUN source $HOME/.cargo/env; OPAMSOLVERTIMEOUT=2400 opam exec -- make build-deps

    # FIXME:
    # We're getting the error:
    #
    #    [ERROR] Sorry, resolution of the request timed out.
    #    Try to specify a simpler request, use a different solver, or increase the allowed time by
    #     setting OPAMSOLVERTIMEOUT to a bigger value (currently, it is set to 2400.0 seconds)
    #
    # Even though our OPAMSOLVERTIMEOUT is 40 minutes.
