# Source artifacts — provenance

These are the **authoritative upstream specs** the master matrix is derived from, pinned here verbatim so
the data is reproducible: anyone can regenerate `../matrix.json` from `../build_matrix.ts` + this folder,
re-fetch the latest with `./fetch.sh`, and detect upstream drift by comparing checksums.

Retrieved: **2026-06-28**. Upstream branches move; the committed files are the pinned snapshot, and the
`sha256` column lets you verify a re-fetch matches (or flags that upstream changed).

| file | what | upstream | sha256 |
|------|------|----------|--------|
| `cddl.abnf` | CDDL grammar (= RFC 8610 Appendix B) | `raw.githubusercontent.com/cbor-wg/cddl/master/cddl.abnf` | `9de451b3…b5c4019` |
| `cddl-1-1-update.abnf` | errata-corrected grammar (RFC 9682, supersedes 8610 App. B) | `raw.githubusercontent.com/cbor-wg/update-8610-grammar/master/cddl-1-1-update.abnf` | `a90f41b2…381afea` |
| `cddl.prelude` | CDDL standard prelude (= RFC 8610 Appendix D) | `raw.githubusercontent.com/cbor-wg/cddl/master/cddl.prelude` | `174a1c47…0a5d3ee5` |
| `cddl-control-operators.csv` | IANA "CDDL Control Operators" registry (cross-RFC) | `www.iana.org/assignments/cddl/cddl-control-operators.csv` | `fdc273a6…d6f12aad` |
| `appendix_a.json` | CBOR encoding examples (RFC 8949 Appendix A) | `raw.githubusercontent.com/cbor/test-vectors/master/appendix_a.json` | `80e78dc2…dc4e9f3a` |

Full RFC bodies (RFC 8610 / 8949) are large human reference, not regeneration inputs; they're cached
offline (gitignored) under `draft/golden-vectors/` and linked from the matrix `README.md`.

## Refresh / verify
```sh
./fetch.sh            # re-download all artifacts from the upstreams above
sha256sum -c <(grep -oE '`[0-9a-f]{8}…[0-9a-f]{7}`' MANIFEST.md)   # (illustrative; full sums below)
```
Authoritative checksums (full):
```
9de451b363a3dca1ebdd1ff3845ccf1839ff93e6047872e0d31052561b5c4019  cddl.abnf
a90f41b2d250f708ee5146e8aab70fc14428639ed0fd31925d10eb7fa381afea  cddl-1-1-update.abnf
174a1c470540da61dec8d1ce22e21b2374b38db8424bd19bdcc3bf630a5d3ee5  cddl.prelude
fdc273a65beeb9cb4122616e13c50b46b5647916852b7eff4aafbb57d6f12aad  cddl-control-operators.csv
80e78dc2f53cfdc9836094791d09e84c6818edf380f7cdd4be26a5c2dc4e9f3a  appendix_a.json
```
