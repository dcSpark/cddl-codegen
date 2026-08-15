// These tests run only through `wasm_open_rest_parent_mutation_preserves_replay_after_decode`,
// whose `--preserve-encodings=true` profile is part of the acceptance contract.  The host test
// harness executes the generated wasm crate as Rust, which exercises the exact wrapper methods
// and conversion code that wasm-bindgen exports.

const OPEN_WIRE: &[u8] = &[
    0xa2, // map(2)
    0x68, b'r', b'e', b'q', b'u', b'i', b'r', b'e', b'd', 0x07,
    0x78, 0x08, b'o', b'r', b'i', b'g', b'i', b'n', b'a', b'l', 0x18, 0x01,
];
const PAIR_WIRE: &[u8] = &[
    0xa3, // map(3)
    0x68, b'r', b'e', b'q', b'u', b'i', b'r', b'e', b'd', 0x07,
    0x78, 0x04, b's', b'a', b'm', b'e', 0x18, 0x01,
    0x64, b's', b'a', b'm', b'e', 0x02,
];

#[test]
fn wasm_parent_insert_preserves_decoded_open_rest_replay() {
    let mut open = Open::from_cbor_bytes(OPEN_WIRE)
        .ok()
        .expect("the wasm wrapper decodes the non-minimal captured entry");
    open.insert_rest("new".to_owned(), 2)
        .expect("the parent mutation door accepts a new ordinary entry");

    let bytes = open.to_cbor_bytes();
    assert_eq!(bytes[0], 0xa3, "the parent now serializes three entries");
    assert!(
        bytes.windows(OPEN_WIRE.len() - 1).any(|window| window == &OPEN_WIRE[1..]),
        "preserve replay keeps the decoded key/value header widths after parent insertion: {bytes:02x?}"
    );
    let decoded = Open::from_cbor_bytes(&bytes)
        .ok()
        .expect("decode-mutate-serialize output stays valid");
    assert_eq!(decoded.rest().get("original".to_owned()), Some(1));
    assert_eq!(decoded.rest().get("new".to_owned()), Some(2));
}

#[test]
fn wasm_parent_insert_preserves_decoded_pair_rest_replay() {
    let mut pair = Pair::from_cbor_bytes(PAIR_WIRE)
        .ok()
        .expect("the wasm wrapper decodes duplicate preserved entries");
    pair.insert_rest("same".to_owned(), 3)
        .expect("the parent mutation door appends a third equal-key pair");

    let bytes = pair.to_cbor_bytes();
    assert_eq!(bytes[0], 0xa4, "the pair parent serializes every duplicate entry");
    assert!(
        bytes.windows(PAIR_WIRE.len() - 1).any(|window| window == &PAIR_WIRE[1..]),
        "preserve replay keeps the decoded duplicate pair spellings after parent insertion: {bytes:02x?}"
    );
    let decoded = Pair::from_cbor_bytes(&bytes)
        .ok()
        .expect("preserved parent output remains decodable");
    assert_eq!(decoded.rest().len(), 3);
    assert_eq!(decoded.rest().get("same".to_owned()), Some(1));
}
