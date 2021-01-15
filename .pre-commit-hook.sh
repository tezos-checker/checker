#!/usr/bin/env bash

relevant_files="$(
  git diff --cached --name-status \
    | awk '$1 != "D" { print $2 }' \
    | grep -E '.mli?$' \
)"

has_error=false
while read fname; do
  if [[ -z "$fname" ]]; then continue; fi
  actual="$(git show :$fname)"
  indented="$(echo "$actual" | ocp-indent)"
  if [[ "$indented" != "$actual" ]]; then
     has_error=true
     echo "Not indented: $fname" >&2
  fi
done < <(echo "$relevant_files")

if [[ $has_error = true ]]; then
  echo "Found files with incorrect indentation. Please run 'make indent' under 'src/'." >&2
  exit 1
fi
