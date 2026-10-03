//! Only --preserve-encodings=true --canonical-form=true, selected by the host registry.

use component_host::load;
use wasmtime::Result;

#[test]
fn canonical_bytes_normalize_without_replacing_preserved_bytes() -> Result<()> {
    let mut h = load()?;
    let (store, api) = h.split();
    let hash = api.hash();
    let payload: Vec<u8> = (0u8..32).collect();
    // Byte string length32 carried with a nonminimal two-byte length head.
    let mut original = vec![0x59, 0x00, 0x20];
    original.extend_from_slice(&payload);
    let mut canonical = vec![0x58, 0x20];
    canonical.extend_from_slice(&payload);
    let handle = hash
        .call_from_cbor_bytes(&mut *store, &original)?
        .expect("a nonminimal32-byte string must decode");
    assert_eq!(hash.call_get(&mut *store, handle)?, payload);
    assert_eq!(hash.call_to_cbor_bytes(&mut *store, handle)?, original);
    assert_eq!(
        hash.call_to_canonical_cbor_bytes(&mut *store, handle)?,
        canonical
    );
    // Canonical serialization must leave the stored original spelling and value unchanged.
    assert_eq!(hash.call_to_cbor_bytes(&mut *store, handle)?, original);
    assert_eq!(hash.call_get(&mut *store, handle)?, payload);
    let normalized = hash
        .call_from_cbor_bytes(&mut *store, &canonical)?
        .expect("the independent canonical vector must decode");
    assert_eq!(
        hash.call_to_canonical_cbor_bytes(&mut *store, normalized)?,
        canonical
    );
    Ok(())
}
