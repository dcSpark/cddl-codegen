#!/usr/bin/env bash
# Regenerates the crate under fuzz (fuzz/generated/, gitignored) and the seed corpus.
# Mirrors what integration_tests::preserve_encodings' run_test does: generate with
# --preserve-encodings, then append the @custom_serialize fragments the fixture needs.
set -euo pipefail
cd "$(dirname "$0")/.."

cargo run --release -- --input=tests/preserve-encodings/input.cddl --output=fuzz/generated \
    --preserve-encodings=true --wasm=false
printf '\nuse serialization::*;\n\n' >> fuzz/generated/rust/src/lib.rs
cat tests/custom_serialization_preserve >> fuzz/generated/rust/src/lib.rs

# Seed corpus: every hand-derived RFC 8949 hex vector in the golden-hex suite.
mkdir -p fuzz/corpus/from_cbor_bytes
python3 - <<'EOF'
import re, hashlib
src = open('tests/golden_hex/tests.rs').read()
for a in re.findall(r'&\[((?:\s*0x[0-9a-fA-F]{2},?\s*)+)\]', src):
    data = bytes(int(b, 16) for b in re.findall(r'0x([0-9a-fA-F]{2})', a))
    if data:
        open(f'fuzz/corpus/from_cbor_bytes/{hashlib.sha1(data).hexdigest()[:16]}', 'wb').write(data)
EOF

echo "done — now: cd fuzz && cargo +nightly fuzz run from_cbor_bytes"
