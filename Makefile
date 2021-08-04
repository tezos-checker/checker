all: build test

build: build-ocaml build-ligo

src/checkerLazyEntrypoints.ml: src/checker.mli scripts/generate-entrypoints.rb
	ruby scripts/generate-entrypoints.rb src/checker.mli > $@
	ocp-indent -i $@

ocaml-src: src/checkerLazyEntrypoints.ml

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

fast-test: ocaml-src
	sh ./scripts/ensure-unique-errors.sh
	dune build @run-fast-tests

build-test-coverage: ocaml-src
	dune runtest --instrument-with bisect_ppx --force .

test-coverage: build-test-coverage
	bisect-ppx-report html
	bisect-ppx-report summary

test-coverage.json: build-test-coverage
	bisect-ppx-report summary --per-file \
	  | awk '{ match($$0, "^ *([0-9.]+) *% *[^ ]* *(.*)$$", res); print res[1] "|" res[2] }'  \
	  | jq -R "split(\"|\") | { \
	      \"value\": .[0] | tonumber, \
	      \"key\": (.[1] | if . == \"Project coverage\" then \"TOTAL\" else ltrimstr(\"src/\") end) \
	    }" \
	  | jq --sort-keys -s 'from_entries' \
	  | tee test-coverage.json

clean:
	$(RM) -r _build _coverage generated src/checkerLazyEntrypoints.ml docs/spec/_build test-coverage.json

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
