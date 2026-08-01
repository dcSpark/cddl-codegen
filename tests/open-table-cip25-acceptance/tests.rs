// The open-table series' ACCEPTANCE fixture: CIP-25 spelled with generated open tables at all four
// payload levels, measured against the semantics CML's hand-written `CIP25LabelMetadata` ships
// today. This is the fixture that answers "does the feature actually replace ~700 lines of hand
// serialization", so its subject is not any single emitter but the composition: an alias-of-marker
// custom codec keying a typed row, `@custom_wire_major` steering that row's dispatch, four levels of
// typed-row-plus-catch-all nesting, and a v1/v2 type choice discriminated purely by whether the v1
// arm's typed row accepts the payload's keys.
//
// Every hex constant below is either COPIED from CML's own pin vectors (with its provenance stated
// at the constant) or hand-written here from the CBOR grammar. Nothing is copied from generator
// output.
#[cfg(test)]
mod cip25_acceptance {
    use super::*;
    use serialization::{Deserialize, Serialize};

    fn bytes(hex: &str) -> Vec<u8> {
        let hex: String = hex.chars().filter(|c| !c.is_whitespace()).collect();
        (0..hex.len())
            .step_by(2)
            .map(|i| u8::from_str_radix(&hex[i..i + 2], 16).unwrap())
            .collect()
    }

    // ---------------------------------------------------------------------------------------
    // Vectors copied from CML
    // ---------------------------------------------------------------------------------------

    /// The real on-chain CIP-25 v1 golden: SpaceBud #1507, mainnet.
    ///
    /// PROVENANCE: copied verbatim from `cip25/rust/tests/preserve_vectors.rs` in the
    /// cardano-multiplatform-lib checkout, where it is `SPACEBUD_V1_GOLDEN_HEX` and its provenance
    /// is documented as a mainnet transaction. It is the one vector in that file whose bytes are
    /// not builder-emitted, which is exactly what makes it the acceptance oracle: a payload nobody
    /// on either side of this feature authored.
    ///
    /// Shape: `{ 721: { <56 hex digits> : { "SpaceBud1507" : {details} } } }` — the v1 arm, whose
    /// policy key is hex TEXT (major 3, declared by `@custom_wire_major`) and whose asset key is
    /// UTF-8 TEXT. Both levels are open tables here; in CML both are hand-written.
    const SPACEBUD_V1_GOLDEN_HEX: &str = "a11902d1a178386435653662663035303033373864346630646134653864646536626563656337363231636438636266356362623962383730313364346363a16c537061636542756431353037a569617277656176654964782b36737270585a4f54664b5f36324b55724a4b68345664434647305953323731707132304f4d52704535547365696d6167657835697066733a2f2f516d5557503678474875636742557635313467776762743479696a673336615551756e455036317a354438524b53646e616d656e53706163654275642023313530376674726169747385695374617220537569746a4368657374706c6174656442656c7464466c616766506973746f6c647479706565416c69656e";

    /// The 28-byte test policy id CML's noisy vectors use (`baadf00d` × 7), and its two spellings.
    const TEST_POLICY_HEX_TEXT: &str = "baadf00dbaadf00dbaadf00dbaadf00dbaadf00dbaadf00dbaadf00d";
    const TEST_POLICY_BYTES_HEX: &str = "baadf00dbaadf00dbaadf00dbaadf00dbaadf00dbaadf00dbaadf00d";

    // ---------------------------------------------------------------------------------------
    // The v1 golden: four generated levels reproduce a real on-chain payload byte for byte
    // ---------------------------------------------------------------------------------------

    #[test]
    fn the_onchain_v1_golden_round_trips_byte_exact() {
        let wire = bytes(SPACEBUD_V1_GOLDEN_HEX);
        let v = Cip25::from_cbor_bytes(&wire).expect("the mainnet golden must parse");
        assert_eq!(
            v.to_cbor_bytes(),
            wire,
            "a real on-chain CIP-25 v1 payload must survive four levels of generated open table"
        );
    }

    #[test]
    fn the_onchain_v1_golden_discriminates_to_v1_and_decodes_its_levels() {
        let v = Cip25::from_cbor_bytes(&bytes(SPACEBUD_V1_GOLDEN_HEX)).unwrap();
        let LabelMetadata::V1(v1) = &v.key_721 else {
            panic!("a hex-text policy key must discriminate to the v1 arm");
        };
        assert_eq!(v1.entries.len(), 1, "one policy, on the TYPED row");
        assert!(v1.rest.is_empty(), "a clean payload captures nothing");
        let assets = v1.entries.values().next().unwrap();
        assert_eq!(assets.entries.len(), 1, "one asset, on the TYPED row");
        assert!(assets.rest.is_empty());
        let details = assets.entries.values().next().unwrap();
        assert_eq!(details.name.get(), "SpaceBud #1507");
        assert!(
            !details.rest.is_empty(),
            "the golden's unmodelled members (`arweaveId`, `traits`, `type`) land on the details \
             rest row — the delivered open struct-map row this feature composes with"
        );
    }

    // ---------------------------------------------------------------------------------------
    // The version discrimination (CML's P4), executable
    // ---------------------------------------------------------------------------------------

    /// A v2 payload carrying junk at the wrapper level: `{ 721: { "data": { <28 bytes> : { <bytes>
    /// : {details} } }, "version": 2, 7: "junk" } }`. Hand-written from the CBOR grammar.
    ///
    /// The uint key `7` cannot be a v1 policy key (the v1 typed row claims major 3 only), and it
    /// cannot be a v2 fixed key either — it is a metadatum-typed captured entry on the v2 wrapper's
    /// rest row.
    fn v2_with_junk_hex() -> String {
        format!(
            "a11902d1a3{data}{version}{junk}",
            // "data": { <policy bytes> : { <asset bytes> : { "name": "n", "image": "i" } } }
            data = format!(
                "6464617461a1581c{policy}a1421234a2646e616d65616e65696d6167656169",
                policy = TEST_POLICY_BYTES_HEX
            ),
            // "version": 2
            version = "6776657273696f6e02",
            // 7: "junk"
            junk = "07646a756e6b",
        )
    }

    #[test]
    fn a_v2_payload_with_junk_discriminates_to_v2_and_round_trips_byte_exact() {
        let wire = bytes(&v2_with_junk_hex());
        let v = Cip25::from_cbor_bytes(&wire).expect("a v2 payload must parse");
        let LabelMetadata::V2(v2) = &v.key_721 else {
            panic!("a payload with `data`/`version` text keys must discriminate to the v2 arm");
        };
        assert_eq!(v2.rest.len(), 1, "the uint key is captured at the wrapper");
        assert_eq!(v2.data.entries.len(), 1, "one policy on the TYPED row");
        assert_eq!(
            v.to_cbor_bytes(),
            wire,
            "junk included, byte for byte — the whole point of the preserve contract"
        );
    }

    #[test]
    fn discrimination_is_the_v1_typed_rows_refusal_not_a_version_peek() {
        // The v1 arm is tried first and FAILS on this payload: its typed row claims major 3, so the
        // text keys "data"/"version" bind it, and the hex codec refuses them — a hard error, which
        // fails the choice arm and rewinds. Nothing peeks at a version field to decide. The proof
        // is that the v1 arm alone rejects the very bytes the choice accepts.
        let wire = bytes(&v2_with_junk_hex());
        let v2_payload = &wire[4..]; // strip `a1 1902d1` to reach the label-metadata value
        assert!(
            V1::from_cbor_bytes(v2_payload).is_err(),
            "the v1 arm must refuse a v2 payload outright"
        );
        assert!(
            V2::from_cbor_bytes(v2_payload).is_ok(),
            "…and the v2 arm must take it"
        );
    }

    #[test]
    fn a_float_keyed_junk_entry_fails_both_arms() {
        // `{ 721: { 0xf9 3c00 : 1 } }` — a half-float key (major 7). No arm admits it: the v1 typed
        // row claims major 3, the v1 catch-all's metadatum grammar has no float, and v2 needs its
        // two fixed keys. A hard error out of BOTH arms is the property that keeps the choice from
        // silently swallowing a shape neither version can represent.
        let wire = bytes("a11902d1a1f93c0001");
        assert!(
            Cip25::from_cbor_bytes(&wire).is_err(),
            "a float key must fail both arms of the version choice"
        );
    }

    // ---------------------------------------------------------------------------------------
    // The typed-major-but-invalid class, at each level, each beside a positive control
    // ---------------------------------------------------------------------------------------
    //
    // This is the class where the generated semantics DIVERGE from CML's hand tables, deliberately
    // and by contract: dispatch is by wire major, never by success, so a key of the typed row's
    // major that the row's own type refuses is a HARD parse error rather than a capture. CML's hand
    // reader captured such keys as junk. Each vector below is stated with its positive control, so
    // what the divergence costs is visible rather than asserted.

    /// `{ 721: { <text> : { "n": {details} } } }` at the v1 POLICY level.
    fn v1_policy_key(key_hex: &str) -> Vec<u8> {
        bytes(&format!(
            "a11902d1a1{key_hex}a1616ea2646e616d65616e65696d6167656169"
        ))
    }

    #[test]
    fn v1_policy_level_non_hex_text_is_a_hard_error_not_a_capture() {
        // "not-a-policy" — major 3, so the typed row claims it, and the hex codec refuses.
        assert!(
            Cip25::from_cbor_bytes(&v1_policy_key("6c6e6f742d612d706f6c696379")).is_err(),
            "a non-hex TEXT key at the v1 policy level must be a hard error"
        );
        // positive control: the same shape with a well-formed 56-hex-digit key
        let ok = Cip25::from_cbor_bytes(&v1_policy_key(&format!(
            "7838{}",
            hex_of_ascii(TEST_POLICY_HEX_TEXT)
        )))
        .expect("a 56-hex-digit policy key parses");
        let LabelMetadata::V1(v1) = &ok.key_721 else {
            panic!("hex text discriminates v1")
        };
        assert_eq!(v1.entries.len(), 1);
    }

    /// `{ 721: { "data": { <key> : {assets} }, "version": 2 } }` at the v2 POLICY level.
    fn v2_policy_key(key_hex: &str) -> Vec<u8> {
        bytes(&format!(
            "a11902d1a26464617461a1{key_hex}a1421234a2646e616d65616e65696d61676561696776657273696f6e02"
        ))
    }

    #[test]
    fn v2_policy_level_wrong_length_bytes_is_a_hard_error_not_a_capture() {
        // A 3-byte bstr: major 2, so the typed row claims it, and `PolicyId::from_raw_bytes`
        // refuses the length. Dispatch is by major, so the catch-all never sees it.
        assert!(
            Cip25::from_cbor_bytes(&v2_policy_key("43aabbcc")).is_err(),
            "a wrong-length BYTES key at the v2 policy level must be a hard error"
        );
        // positive control: the same shape with a 28-byte key
        assert!(
            Cip25::from_cbor_bytes(&v2_policy_key(&format!("581c{TEST_POLICY_BYTES_HEX}"))).is_ok(),
            "a 28-byte policy key parses"
        );
    }

    #[test]
    fn v2_asset_level_over_long_bytes_is_a_hard_error_not_a_capture() {
        // A 33-byte asset name at the v2 ASSET level: major 2 claims the typed row, and the
        // marker's own refusal propagates out of all four levels.
        let long_name = format!("5821{}", "ab".repeat(33));
        let wire = |name: &str| {
            bytes(&format!(
                "a11902d1a26464617461a1581c{TEST_POLICY_BYTES_HEX}a1{name}a2646e616d65616e65696d61676561696776657273696f6e02"
            ))
        };
        assert!(
            Cip25::from_cbor_bytes(&wire(&long_name)).is_err(),
            "a 33-byte asset name must be a hard error at the v2 asset level"
        );
        assert!(
            Cip25::from_cbor_bytes(&wire("421234")).is_ok(),
            "a 2-byte asset name parses"
        );
    }

    #[test]
    fn v1_asset_level_invalid_utf8_cannot_even_be_spelled() {
        // The v1 asset key is TEXT, and CBOR text is UTF-8 by construction, so the "typed major but
        // invalid" class at this level is the LENGTH refusal instead: a 33-byte name has no v1
        // spelling the marker accepts.
        let wire = |name_hex: &str| {
            bytes(&format!(
                "a11902d1a17838{}a1{name_hex}a2646e616d65616e65696d6167656169",
                hex_of_ascii(TEST_POLICY_HEX_TEXT)
            ))
        };
        // 33 ASCII characters -> 33 bytes -> refused by `AssetName::from_raw_bytes`
        let long = format!("7821{}", hex_of_ascii(&"a".repeat(33)));
        assert!(
            Cip25::from_cbor_bytes(&wire(&long)).is_err(),
            "an over-long v1 asset name must be a hard error"
        );
        assert!(
            Cip25::from_cbor_bytes(&wire(&format!("62{}", hex_of_ascii("ab")))).is_ok(),
            "a 2-character v1 asset name parses"
        );
    }

    fn hex_of_ascii(s: &str) -> String {
        s.bytes().map(|b| format!("{b:02x}")).collect()
    }

    // ---------------------------------------------------------------------------------------
    // The capture rows themselves
    // ---------------------------------------------------------------------------------------

    #[test]
    fn every_level_captures_its_own_junk_byte_exactly() {
        // A v1 payload carrying a captured entry at the policy level (uint key) and one at the
        // asset level (a nested list key), both metadatum-typed. Hand-written from the grammar.
        let wire = bytes(&format!(
            "a11902d1a27838{policy}a2{asset}04636162630101",
            policy = hex_of_ascii(TEST_POLICY_HEX_TEXT),
            // "ab" : { "name": "n", "image": "i" }  and  4 : "abc"
            asset = format!("62{}a2646e616d65616e65696d6167656169", hex_of_ascii("ab")),
        ));
        let v = Cip25::from_cbor_bytes(&wire).expect("captured junk at two levels must parse");
        let LabelMetadata::V1(v1) = &v.key_721 else {
            panic!("v1")
        };
        assert_eq!(v1.entries.len(), 1, "the policy is typed");
        assert_eq!(v1.rest.len(), 1, "the uint key is captured");
        let assets = v1.entries.values().next().unwrap();
        assert_eq!(assets.entries.len(), 1, "the asset is typed");
        assert_eq!(assets.rest.len(), 1, "the uint key is captured");
        assert_eq!(
            v.to_cbor_bytes(),
            wire,
            "the interleave of typed and captured entries replays byte-exact at both levels"
        );
    }
}
