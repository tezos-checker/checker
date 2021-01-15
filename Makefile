all: install-git-hooks build test

build: build-ocaml build-ligo

build-ocaml:
	dune build @install

generate-ligo:
	mkdir -p generated/ligo
	./generate-ligo.sh

build-ligo: generate-ligo
	mkdir -p generated/michelson
	ligo compile-contract generated/ligo/main.mligo main --output-file generated/michelson/main.tz

test:
	dune runtest .

clean:
	$(RM) -r _build generated

indent:
	ocp-indent -i src/*.ml src/*.mli

docs:
	dune build @doc

distclean: clean

install-git-hooks:
	@[ -x .git/hooks/pre-commit ] || (cd .git/hooks && rm -f pre-commit && ln -s ../.pre-commit-hook.sh pre-commit && echo "pre-commit hook installed")

.PHONY: all tests clean indent docs distclean install-git-hooks
