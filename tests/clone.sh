#!/bin/bash

REV=4591a245c7eec9f70d668982b1383cd2a6854af5

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
