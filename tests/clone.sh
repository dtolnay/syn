#!/bin/bash

REV=521d78407471cb78e9bbf47160f6aa23047ac499

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
