./scripts/generate-entrypoints.rb ./src/checker.mli > ./src/checkerEntrypoints.ml
python ./checker_tools/builder/cli.py generate --out src
./scripts/generate-ligo.sh
python fix_ligo.py
./scripts/compile-ligo.rb
