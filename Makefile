all: install-git-hooks build test

build: build-ocaml build-ligo

build-ocaml:
	dune build @install

generate-ligo:
	mkdir -p generated/ligo
	./generate-ligo.sh

build-ligo: generate-ligo
	mkdir -p generated/michelson
	ligo compile-contract --protocol edo --disable-michelson-typechecking generated/ligo/main.mligo main --output-file generated/michelson/main.tz
	tezos-client --mode mockup --protocol PtEdoTez typecheck script generated/michelson/main.tz
	# ligo compile-storage --protocol edo generated/ligo/main.mligo main initial_checker --output generated/michelson/storage.tz

test:
	dune runtest .

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

.PHONY: all tests clean indent docs distclean install-git-hooks
