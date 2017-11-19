#!/bin/bash

REMOTE=rust
REPO=https://github.com/rust-lang/rust
REV=eb8f2586ebd842dec49d3d7f50e49a985ab31493

set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")"

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
