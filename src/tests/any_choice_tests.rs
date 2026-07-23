//! Type-choice arms of `any` (loose-CBOR A3 WP1).
//!
//! A BARE `any` type-choice arm (conceptual `Any`, no encoding ops) accepts every CBOR item, so it
//! overlaps every other arm and is only legal as the LAST catch-all — any earlier position leaves
//! the arms after it unreachable (DESIGN §3.5, ruling §10.6). Its dispatch is FORCED backtracking:
//! a typed arm that matches on wire type but fails on content must fall through to `any`, which the
//! `cbor_type()`-dispatch strategy would never do. The strategy selector auto-forces backtracking
//! because `Any::cbor_types` spans all 8 major types; these tests pin that the emitter never picks
//! the type-dispatch form for an `any`-armed choice, and that the parser enforces the last-position
//! rule. A TAGGED `any` arm (`#6.n(any)`) is NOT a catch-all (its `cbor_types()` is `[Tag]`) and is
//! allowed in any position.
//!
//! Runtime round-trip / content-fallthrough coverage lives in the compiled `tests/any-choice`
//! integration fixture; these tests assert the emitted SOURCE shape and the graceful rejections.

use crate::cli::Cli;
use clap::Parser;

/// Run the whole generation pipeline in-process (rust-only) and return every emitted file joined,
/// or the graceful error string. The input path is unique per call (process id + a monotonic
/// counter) so the parallel test runner can't race two specs onto one temp file.
fn generate(spec: &str) -> Result<String, String> {
    use std::sync::atomic::{AtomicU64, Ordering};
    static COUNTER: AtomicU64 = AtomicU64::new(0);
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_any_choice_{}_{}.cddl",
        std::process::id(),
        COUNTER.fetch_add(1, Ordering::Relaxed)
    ));
    std::fs::write(&path, spec).unwrap();
    let cli = Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "any_choice_unused",
        "--wasm=false",
    ]);
    let result = crate::api::generated_strings(&cli)
        .map(|files| files.into_values().collect::<Vec<_>>().join("\n"))
        .map_err(|e| e.to_string());
    std::fs::remove_file(&path).ok();
    result
}

/// The generated `impl Deserialize for X { .. }` block, sliced out of the whole-source haystack so a
/// `cbor_type()`-dispatch assertion is scoped to the CHOICE deserializer — the AnyCbor runtime's own
/// `read` seam legitimately dispatches on `raw.cbor_type()` and lives in a different file/impl.
fn x_deserialize_impl(src: &str) -> String {
    let start = src
        .find("impl Deserialize for X {")
        .expect("generated source must contain the X deserializer");
    let rest = &src[start + "impl Deserialize for X {".len()..];
    // impls are top-level and separated by `\nimpl `; slice to the next one (or end of source).
    let end = rest.find("\nimpl ").unwrap_or(rest.len());
    rest[..end].to_owned()
}

/// The emitter-level statement of the forced-backtracking rule: an `any`-armed choice deserializer
/// must NOT contain the `match raw.cbor_type()?` type-dispatch form, and MUST use the backtracking
/// form (source-order arms, position rewind, collected causes).
#[test]
fn any_arm_choice_deserializer_uses_backtracking_not_type_dispatch() {
    let src = generate("x = uint / any\n").expect("a last-position `any` arm must generate");
    let x_deser = x_deserialize_impl(&src);
    assert!(
        !x_deser.contains("match raw.cbor_type()?"),
        "an `any`-armed choice must not use the cbor_type()-dispatch form:\n{x_deser}"
    );
    assert!(
        x_deser.contains("NoVariantMatchedWithCauses") && x_deser.contains("let mut errs"),
        "an `any`-armed choice must use the backtracking form (collected causes):\n{x_deser}"
    );
}

#[test]
fn bare_any_arm_allowed_in_last_position() {
    let src = generate("x = uint / tstr / any\n").expect("a last `any` arm is legal");
    assert!(
        src.contains("Any(crate::generated::any_cbor::AnyCbor)"),
        "the last `any` arm must lower to the AnyCbor runtime type:\n{src}"
    );
}

#[test]
fn bare_any_arm_rejected_when_not_last() {
    let err = generate("x = any / tstr\n").expect_err("a non-last bare `any` arm must reject");
    assert!(
        err.contains("makes later arms unreachable"),
        "rejection must name the unreachable-arms cause, got: {err}"
    );
}

#[test]
fn duplicate_bare_any_arms_rejected() {
    // `x = any / any` — the FIRST `any` is non-last, so the last-position rule rejects it (no panic).
    let err = generate("x = any / any\n").expect_err("duplicate bare `any` arms must reject");
    assert!(
        err.contains("makes later arms unreachable"),
        "duplicate `any` arms reject via the last-position rule, got: {err}"
    );
}

/// PROBE-1: a tagged `any` arm is not a catch-all (cbor_types = [Tag]); it type-dispatches through
/// the ordinary machinery and is allowed in ANY position — here FIRST, before a `tstr` arm.
#[test]
fn tagged_any_arm_allowed_in_any_position() {
    let src =
        generate("x = #6.5(any) / tstr\n").expect("a tagged `any` arm is position-independent");
    assert!(
        src.contains("Any(crate::generated::any_cbor::AnyCbor)"),
        "the tagged `any` arm must lower to the AnyCbor runtime type:\n{src}"
    );
    // It is NOT the catch-all: the deserializer still dispatches (tag vs text are disjoint types).
    assert!(
        src.contains("impl Deserialize for X {"),
        "the tagged-`any` choice must generate a deserializer:\n{src}"
    );
}
