all: install-git-hooks build test

build: build-ocaml build-ligo

src/checkerEntrypoints.ml: src/checker.mli scripts/generate-entrypoints
	ruby scripts/generate-entrypoints src/checker.mli > $@
	ocp-indent -i $@

ocaml-src: src/checkerEntrypoints.ml

build-ocaml: ocaml-src
	dune build @install

generate-ligo: ocaml-src
	mkdir -p generated/ligo
	sh ./scripts/generate-ligo.sh

build-ligo: generate-ligo
	ruby ./scripts/compile-ligo.rb

test: test-main test-long

test-main: ocaml-src
	./scripts/ensure-unique-errors.sh
	dune runtest tests/test_suite_main

test-long:
	dune runtest tests/test_suite_long

test-coverage:
	dune runtest --instrument-with bisect_ppx --force .
	bisect-ppx-report html
	bisect-ppx-report summary

clean:
	$(RM) -r _build generated src/checkerEntrypoints.ml

indent:
	ocp-indent -i src/*.ml src/*.mli tests/**/*.ml
	new_dune=$$(mktemp); dune format-dune-file src/dune > $$new_dune && mv $$new_dune src/dune
	new_dune=$$(mktemp); dune format-dune-file tests/dune > $$new_dune && mv $$new_dune tests/dune

docs: ocaml-src
	cd src && dune build @doc
	@echo "Docs generated in _build/default/_doc/_html"

distclean: clean

install-git-hooks:
	@[ -x .git/hooks/pre-commit ] || (cd .git/hooks && rm -f pre-commit && ln -s ../.pre-commit-hook.sh pre-commit && echo "pre-commit hook installed")

.PHONY: all build ocaml-src build-ocaml generate-ligo build-ligo tests clean indent docs distclean install-git-hooks
