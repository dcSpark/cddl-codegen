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
| `rfc8610.txt` | Base CDDL specification | `www.rfc-editor.org/rfc/rfc8610.txt` | `3713f2a5…04997089` |
| `rfc9090.txt` | CDDL control operators for SDNV/OID data | `www.rfc-editor.org/rfc/rfc9090.txt` | `16e606aa…0e31cb89` |
| `rfc9165.txt` | Additional general CDDL control operators | `www.rfc-editor.org/rfc/rfc9165.txt` | `94f86116…41976774` |
| `rfc9682.txt` | Updates to the CDDL grammar | `www.rfc-editor.org/rfc/rfc9682.txt` | `d35602e7…6cdea89c` |
| `rfc9741.txt` | Additional CDDL text conversion/processing control operators | `www.rfc-editor.org/rfc/rfc9741.txt` | `3234aeea…1e7a9ee3` |

Full RFC bodies are human reference, not regeneration inputs.

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
3713f2a50e23a2bea0a6147ad6c4433605a00c3d00a868bfa86a42b204997089  rfc8610.txt
16e606aa7e7d0d1f019cb67c95ece5f5e4dbada5585b5251860958c80e31cb89  rfc9090.txt
94f86116c04fde0f11d54576465181090ec33983d78db82527c0703a41976774  rfc9165.txt
d35602e7f784e509a5984283ea8d998adfee23f086ad0d77bd22f98c6cdea89c  rfc9682.txt
3234aeea39d01b7c1c8e9c70d4d7dc8745852e0b6edbea167a3e55b91e7a9ee3  rfc9741.txt
```
