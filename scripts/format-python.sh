#!/usr/bin/env bash

set -o xtrace
set -o errexit

FD="fdfind -E vendor/"

$FD -e '.py' -x black
