#!/bin/bash

REMOTE=rust
REPO=https://github.com/rust-lang/rust
REV=ebddfcb0b19471d2277e05cfbc2ad8caa81637b7

set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")"
mkdir -p rust
cd rust

git init

if git remote | grep --fixed-strings --line-regexp --quiet "$REMOTE"; then
    git remote set-url "$REMOTE" "$REPO"
else
    git remote add "$REMOTE" "$REPO"
fi

if ! git cat-file -t "$REV" >/dev/null 2>&1; then
    git fetch "$REMOTE" master
fi

git checkout "$REV"
