#!/bin/bash

REV=63d66494aff57411bfec1dd2a3a5f1af900feab6

set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")"
mkdir -p rust
touch rust/COMMIT

if [ "$(cat rust/COMMIT)" != "$REV" ]; then
    rm -rf rust
    mkdir rust
    curl -L "https://github.com/rust-lang/rust/archive/${REV}.tar.gz" \
        | tar xz --directory rust --strip-components 1
    echo "$REV" > rust/COMMIT
fi
