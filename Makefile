all: install-git-hooks build test

build: build-ocaml build-ligo

build-ocaml:
	dune build @install

generate-ligo:
	mkdir -p generated/ligo
	./generate-ligo.sh

build-ligo: generated/michelson/main.tz generated/michelson/storage.tz

generated/michelson/main.tz: generate-ligo
	mkdir -p generated/michelson
	ligo compile-contract --protocol edo generated/ligo/main.mligo main --output-file generated/michelson/main.tz
	ligo measure-contract generated/ligo/main.mligo main

generated/michelson/storage.tz: generate-ligo
	mkdir -p generated/michelson
	ligo compile-storage --now='2021-01-01T10:10:10Z' --protocol edo generated/ligo/main.mligo main initial_checker > generated/michelson/storage.tz

test:
	dune runtest .

test-coverage:
	dune runtest --instrument-with bisect_ppx --force .
	bisect-ppx-report html
	bisect-ppx-report summary

clean:
	$(RM) -r _build generated

indent:
	ocp-indent -i src/*.ml src/*.mli
	new_dune=$$(mktemp); dune format-dune-file src/dune > $$new_dune && mv $$new_dune src/dune

docs:
	dune build @doc

distclean: clean

install-git-hooks:
	@[ -x .git/hooks/pre-commit ] || (cd .git/hooks && rm -f pre-commit && ln -s ../.pre-commit-hook.sh pre-commit && echo "pre-commit hook installed")

.PHONY: all build build-ocaml generate-ligo build-ligo tests clean indent docs distclean install-git-hooks
