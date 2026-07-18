#!/usr/bin/env sh
# Re-download the upstream spec artifacts the CDDL master matrix is derived from, then verify them
# against the pinned checksums. Run from this directory:  ./fetch.sh
# A checksum mismatch means upstream changed since the pinned snapshot — review the diff before
# updating SHA256SUMS / regenerating ../matrix.json.
set -eu
cd "$(dirname "$0")"

fetch() { # url, dest  (tries master then main for github branches)
  curl -sSf --max-time 60 -o "$2" "$1" || curl -sSf --max-time 60 -o "$2" "$(echo "$1" | sed 's,/master/,/main/,')"
  echo "  $2"
}

echo "fetching:"
fetch https://raw.githubusercontent.com/cbor-wg/cddl/master/cddl.abnf                              cddl.abnf
fetch https://raw.githubusercontent.com/cbor-wg/update-8610-grammar/master/cddl-1-1-update.abnf    cddl-1-1-update.abnf
fetch https://raw.githubusercontent.com/cbor-wg/cddl/master/cddl.prelude                           cddl.prelude
fetch https://www.iana.org/assignments/cddl/cddl-control-operators.csv                             cddl-control-operators.csv
fetch https://raw.githubusercontent.com/cbor/test-vectors/master/appendix_a.json                   appendix_a.json
fetch https://www.rfc-editor.org/rfc/rfc8610.txt                                                   rfc8610.txt
fetch https://www.rfc-editor.org/rfc/rfc8949.txt                                                   rfc8949.txt
fetch https://www.rfc-editor.org/rfc/rfc9090.txt                                                   rfc9090.txt
fetch https://www.rfc-editor.org/rfc/rfc9165.txt                                                   rfc9165.txt
fetch https://www.rfc-editor.org/rfc/rfc9682.txt                                                   rfc9682.txt
fetch https://www.rfc-editor.org/rfc/rfc9741.txt                                                   rfc9741.txt
# Internet-Draft: the URL is version-pinned, so bump the filename+URL together when a new draft lands.
fetch https://www.ietf.org/archive/id/draft-ietf-cbor-cddl-modules-06.txt                          draft-ietf-cbor-cddl-modules-06.txt

echo "verifying against pinned SHA256SUMS:"
sha256sum -c SHA256SUMS
