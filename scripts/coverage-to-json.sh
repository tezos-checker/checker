#!/usr/bin/env bash

sed -E 's/^ *([0-9]+\.[0-9]+) *% *[^ ]* *(.*)$/\1|\2/g' < "${1:-/dev/stdin}" \
  | jq -R 'split("|") | {
      "value": .[0] | tonumber,
      "key": (.[1] | if . == "Project coverage" then "TOTAL" else ltrimstr("src/") end)
    }' \
  | jq --sort-keys -s 'from_entries'
