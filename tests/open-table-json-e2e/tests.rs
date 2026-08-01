// Open table (`t = { * K_t => V_t, * K_r => V_r }`) FLATTENED-JSON vectors. One JSON object holds
// BOTH rows' entries, so what these pin is everything that cannot follow from either row alone:
// the TYPED-FIRST read partition (a member name binds the typed row iff `K_t`'s own reading admits
// it), the cross-region write collision check (one set spanning both regions — a typed key and a
// captured key imaging identically would otherwise emit one member name twice), the explicit
// duplicate-member detection serde_json's last-wins parser does not do, and the read failure naming
// all THREE attempts. The two regions image through two different conventions (`K_t`'s serde image /
// `K_r`'s CBOR image), which several vectors here exercise deliberately rather than incidentally.
#[cfg(test)]
mod open_table_json {
    use super::*;
    use crate::generated::any_cbor::AnyCbor;

    fn pid(hex: &str) -> PolicyId {
        let hex: String = hex.chars().filter(|c| !c.is_whitespace()).collect();
        PolicyId::new(
            (0..hex.len())
                .step_by(2)
                .map(|i| u8::from_str_radix(&hex[i..i + 2], 16).unwrap())
                .collect(),
        )
    }

    #[test]
    fn both_regions_render_into_one_object_and_round_trip() {
        let mut l = Labels::new();
        l.entries.insert(pid("aabbccdd"), 7);
        l.rest.insert(Md::new_text("zz".to_owned()), Md::new_int(Int::new_uint(1)));
        let json = serde_json::to_string(&l).unwrap();
        // The typed key images through PolicyId's OWN serde (canonical lowercase hex), NOT through
        // the catch-all's CBOR-bytes convention — which has no image for a bytes key at all.
        assert_eq!(json, r#"{"aabbccdd":7,"zz":{"Int":"1"}}"#, "flattened: {json}");
        let back: Labels = serde_json::from_str(&json).unwrap();
        assert_eq!(back.entries.len(), 1, "the hex name binds the TYPED row");
        assert_eq!(back.rest.len(), 1, "the rest binds the catch-all");
        assert_eq!(
            serde_json::to_string(&back).unwrap(),
            json,
            "JSON is a fixed point"
        );
    }

    #[test]
    fn an_empty_open_table_is_an_empty_object_both_ways() {
        assert_eq!(serde_json::to_string(&Labels::new()).unwrap(), "{}");
        let empty: Labels = serde_json::from_str("{}").unwrap();
        assert!(empty.entries.is_empty() && empty.rest.is_empty());
    }

    #[test]
    fn a_captured_bytes_key_has_no_member_name_image() {
        // The catch-all's `md` admits a BYTES key, which the delivered rest-row image refuses —
        // the failure CML accepts at each of its levels, reused here verbatim.
        let mut l = Labels::new();
        l.rest
            .insert(Md::new_bytes(vec![1, 2]), Md::new_text("x".to_owned()));
        let e = serde_json::to_string(&l).expect_err("a bytes captured key must fail to_json");
        assert!(
            format!("{e}").contains("has no JSON member-name image"),
            "the delivered image error must surface verbatim, got: {e}"
        );
        // positive control: the same value under a TEXT key writes fine
        let mut ok = Labels::new();
        ok.rest
            .insert(Md::new_text("k".to_owned()), Md::new_text("x".to_owned()));
        serde_json::to_string(&ok).expect("a text captured key images fine");
    }

    #[test]
    fn a_captured_key_imaging_onto_a_typed_key_is_a_write_error() {
        // The load-bearing CROSS-REGION check: the two rows live in two containers, so neither
        // container's own duplicate rule can see this. One JSON object cannot carry the name twice.
        let mut l = Labels::new();
        l.entries.insert(pid("aabb"), 1);
        l.rest
            .insert(Md::new_text("aabb".to_owned()), Md::new_text("x".to_owned()));
        let e = serde_json::to_string(&l).expect_err("a cross-region collision must fail to_json");
        assert!(
            format!("{e}").contains("stringify identically to \"aabb\""),
            "the collision must name the member name, got: {e}"
        );
    }

    #[test]
    fn two_typed_keys_imaging_identically_are_a_write_error() {
        // `@duplicates preserve` keeps actual duplicate keys, which image identically by definition —
        // so that container has no JSON image at all. CBOR-only fidelity, stated loudly.
        let mut d = Dupp::new();
        d.entries.insert(pid("aabb"), 1);
        d.entries.insert(pid("aabb"), 2);
        let e = serde_json::to_string(&d).expect_err("duplicate typed keys must fail to_json");
        assert!(
            format!("{e}").contains("stringify identically"),
            "the duplicate must be reported as an identical image, got: {e}"
        );
    }

    #[test]
    fn a_repeated_member_name_is_detected_on_read() {
        // serde_json's object parser is LAST-WINS on duplicate members, so an open table detects
        // them itself — the read-side counterpart of the write-side collision check, and the JSON
        // face of the CBOR `DuplicateKey` rejection.
        let e = serde_json::from_str::<Labels>(r#"{"aabb":1,"aabb":2}"#)
            .expect_err("a repeated member name must fail from_json");
        assert!(
            format!("{e}").contains("carries the member name \"aabb\" twice"),
            "the duplicate member must be named, got: {e}"
        );
    }

    #[test]
    fn a_captured_name_the_typed_reading_admits_rebinds_typed() {
        // The T2 carve-out. `Compat`'s two ranges coincide, so the rebinding is value-preserving and
        // JSON stays a fixed point — what moves is the entry's ROW, i.e. the CBOR major it will be
        // written under. CBOR stays authoritative.
        let mut c = Compat::new();
        c.rest.insert(Md::new_text("ccdd".to_owned()), 9);
        let json = serde_json::to_string(&c).unwrap();
        assert_eq!(json, r#"{"ccdd":9}"#);
        let back: Compat = serde_json::from_str(&json).unwrap();
        assert_eq!(back.entries.len(), 1, "rebound onto the typed row");
        assert_eq!(back.rest.len(), 0, "and left the catch-all");
        assert_eq!(
            serde_json::to_string(&back).unwrap(),
            json,
            "JSON is still a fixed point"
        );
    }

    #[test]
    fn a_bound_typed_name_with_a_refusing_value_is_a_hard_error() {
        // Typed-first is about KEYS, and the value that follows is then read as `V_t` — a refusing
        // value does NOT fall through to the catch-all. Same refinement-not-tolerance posture as the
        // CBOR face, and the reason the rebinding above is only total where the ranges agree.
        let e = serde_json::from_str::<Labels>(r#"{"aabb":{"Text":"x"}}"#)
            .expect_err("a V_t-refusing value under a typed name must fail");
        assert!(
            format!("{e}").contains("expected u64"),
            "the failure must be the VALUE's, not a capture, got: {e}"
        );
    }

    #[test]
    fn a_name_no_row_admits_names_all_three_attempts() {
        // `Strict`'s catch-all is a uint newtype, so most names reach neither row. The message must
        // not read as if the typed reading had never been tried.
        let e = serde_json::from_str::<Strict>(r#"{"zz":"v"}"#)
            .expect_err("a name neither row admits must fail from_json");
        let e = format!("{e}");
        assert!(
            e.contains("as the typed row's key:")
                && e.contains("as the catch-all's key:")
                && e.contains("as uint/nint:")
                && e.contains("as text:"),
            "all three readings must be reported, got: {e}"
        );
    }

    #[test]
    fn the_two_regions_admit_by_their_own_images() {
        // A decimal name that is NOT valid hex reaches the catch-all's numeric reading; a name that
        // IS valid hex binds the typed row even though it also spells a decimal. Two images, one
        // object — the property the docs call out, executed.
        let s: Strict = serde_json::from_str(r#"{"123":"v"}"#).unwrap();
        assert_eq!(s.entries.len(), 0, "odd-length hex refuses the typed row");
        assert_eq!(s.rest.len(), 1, "and the decimal reading takes it");
        let s: Strict = serde_json::from_str(r#"{"1234":7}"#).unwrap();
        assert_eq!(s.entries.len(), 1, "valid hex binds typed first");
        assert_eq!(s.rest.len(), 0);
    }

    #[test]
    fn an_any_range_renders_naturally_on_both_rows() {
        // `any` values ride the NATURAL walk (a bare `5`), never `AnyCbor`'s tagged codec.
        let mut a = Anyval::new();
        a.entries.insert(pid("aabb"), AnyCbor::new_uint(5));
        a.rest
            .insert(Md::new_text("k".to_owned()), AnyCbor::new_text("v".to_owned()));
        let json = serde_json::to_string(&a).unwrap();
        assert_eq!(json, r#"{"aabb":5,"k":"v"}"#, "natural rendering: {json}");
        let back: Anyval = serde_json::from_str(&json).unwrap();
        assert_eq!(serde_json::to_string(&back).unwrap(), json);
    }
}
