#!/usr/bin/env bash

set -o xtrace
set -o errexit

# Sort imports
isort . --extend-skip-glob vendor
# Run formatting
black . --extend-exclude vendor
