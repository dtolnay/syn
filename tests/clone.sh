#!/bin/bash

REV=3f5152e200c0c02dfe0f79367948c98053d35855

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
