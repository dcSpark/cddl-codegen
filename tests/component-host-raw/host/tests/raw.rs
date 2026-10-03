//! The existing four-byte raw seam, without CBOR or serde contracts.

use component_raw_host::load;
use wasmtime::Result;

#[test]
fn raw_bytes_roundtrip_and_bad_lengths_return_errors_without_trapping() -> Result<()> {
    let mut h = load()?;
    let (store, api) = h.split();
    let raw = api.raw();
    // These four bytes contain multiple CBOR items and a dangling break. A CBOR seam would fail;
    // RawBytesEncoding accepts them as exactly four opaque bytes under the settled fixture contract.
    let expected = vec![0x00, 0x18, 0x2a, 0xff];
    let handle = raw
        .call_from_raw_bytes(&mut *store, &expected)?
        .expect("the established four-byte raw input must construct");
    assert_eq!(raw.call_to_raw_bytes(&mut *store, handle)?, expected);

    for bad in [vec![1, 2, 3], vec![1, 2, 3, 4, 5]] {
        let error = raw
            .call_from_raw_bytes(&mut *store, &bad)?
            .expect_err("length3 and5 must be inner errors, never traps");
        assert!(!error.is_empty());
        assert_eq!(raw.call_to_raw_bytes(&mut *store, handle)?, expected);
        let after = raw
            .call_from_raw_bytes(&mut *store, &[9, 8, 7, 6])?
            .expect("valid raw input must succeed after every rejection");
        assert_eq!(raw.call_to_raw_bytes(&mut *store, after)?, vec![9, 8, 7, 6]);
    }
    Ok(())
}
