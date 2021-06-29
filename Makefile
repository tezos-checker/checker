all: build test

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

test: ocaml-src
	sh ./scripts/ensure-unique-errors.sh
	dune runtest .

test-coverage: ocaml-src
	dune runtest --instrument-with bisect_ppx --force .
	bisect-ppx-report html
	bisect-ppx-report summary

clean:
	$(RM) -r _build _coverage generated src/checkerEntrypoints.ml docs/spec/_build

indent:
	bash ./scripts/format.sh

spec:
	make -C docs/spec html

watch-spec:
	while sleep 0.1; do ls docs/spec/* | entr -d make spec; done

view-spec:
	test -d docs/spec/_build/html || make spec
	live-server docs/spec/_build/html

docs: ocaml-src spec
	cd src && dune build @doc
	@echo "Docs generated in _build/default/_doc/_html"

distclean: clean

.PHONY: all build ocaml-src build-ocaml generate-ligo build-ligo tests clean indent spec docs distclean watch-spec view-spec
