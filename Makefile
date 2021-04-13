all: install-git-hooks build test

build: build-ocaml build-ligo

src/checkerEntrypoints.ml: src/checker.mli scripts/generate-entrypoints
	ruby scripts/generate-entrypoints src/checker.mli > $@
	ocp-indent -i $@

build-ocaml: src/checkerEntrypoints.ml
	dune build @install

generate-ligo: src/checkerEntrypoints.ml
	mkdir -p generated/ligo
	sh ./scripts/generate-ligo.sh

build-ligo: generate-ligo
	sh ./scripts/compile-ligo.sh

test:
	sh ./scripts/ensure-unique-errors.sh
	dune runtest .

test-coverage:
	dune runtest --instrument-with bisect_ppx --force .
	bisect-ppx-report html
	bisect-ppx-report summary

clean:
	$(RM) -r _build generated

indent:
	ocp-indent -i src/*.ml src/*.mli tests/*.ml
	new_dune=$$(mktemp); dune format-dune-file src/dune > $$new_dune && mv $$new_dune src/dune
	new_dune=$$(mktemp); dune format-dune-file tests/dune > $$new_dune && mv $$new_dune tests/dune

docs:
	dune build @doc

distclean: clean

install-git-hooks:
	@[ -x .git/hooks/pre-commit ] || (cd .git/hooks && rm -f pre-commit && ln -s ../.pre-commit-hook.sh pre-commit && echo "pre-commit hook installed")

.PHONY: all build build-ocaml generate-ligo build-ligo tests clean indent docs distclean install-git-hooks
