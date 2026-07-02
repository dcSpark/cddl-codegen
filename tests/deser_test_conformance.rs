// Independent conformance oracle (TESTING_ROADMAP "cddl-crate conformance oracle"): validate our
// serialized bytes against the SOURCE `.cddl` using the `cddl` crate's validator. Its DECODE +
// CONSTRAINT-EVALUATION path is independent of our encoder/decoder (it decodes with ciborium and
// evaluates constraints itself), so it catches a symmetric encoder/decoder bug a round-trip cannot
// (a round-trip only proves our encoder and decoder agree with *each other*). CAVEAT — it is NOT
// fully decorrelated: the validator parses the `.cddl` with the SAME dcSpark `cddl` fork at the SAME
// pinned rev as the generator's own front end (see `CDDL_ORACLE_DEP`), so a fork-level grammar/AST
// misparse corrupts the generator IR and the oracle's spec interpretation identically and that class
// passes silently. A FAILURE is a strong signal: our bytes don't match the spec. A PASS is weak: the
// validator has known gaps (e.g. it does not enforce `uint .size`) AND shares the parser, so it can't
// be the *only* oracle. (Decorrelation options — an anweiss rev, the ruby `cddl` gem already wired in
// cddl-matrix/verify.ts, or a ciborium structural differential — are a TESTING_ROADMAP item.)
//
// `cddl::validate_cbor_from_slice` validates against a spec's first non-generic type rule only, so we
// prepend a synthetic root aliasing the rule under test — letting us point the validator at any rule
// while still resolving the rest of the spec's references.
//
// Appended into a generated crate's `lib.rs`; the crate needs `cddl` as a dep (wired via run_test's
// test_deps) and its source spec on disk (read relative to the generated crate's manifest dir).

#[cfg(test)]
#[allow(dead_code)]
fn cddl_oracle_load_spec(rel_from_manifest: &str) -> String {
    let path = format!("{}/{}", env!("CARGO_MANIFEST_DIR"), rel_from_manifest);
    std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("cddl oracle: cannot read source spec {path}: {e}"))
}

#[cfg(test)]
#[allow(dead_code)]
fn cddl_oracle_rooted(spec: &str, root_rule: &str) -> String {
    format!("__cddl_oracle_root = {root_rule}\n\n{spec}")
}

/// Assert our bytes conform to `root_rule` in `spec`. Fail = strong signal (see file header).
#[cfg(test)]
#[allow(dead_code)]
fn assert_cddl_conforms(spec: &str, root_rule: &str, bytes: &[u8]) {
    let rooted = cddl_oracle_rooted(spec, root_rule);
    if let Err(e) = cddl::validate_cbor_from_slice(&rooted, bytes, None) {
        panic!("cddl conformance failed for rule `{root_rule}`: {e}\n  bytes: {bytes:02x?}");
    }
}

/// Teeth for the oracle itself: assert deliberately non-conformant bytes are rejected, so a broken
/// or no-op validator (or a wrong `root_rule`) can't let `assert_cddl_conforms` pass vacuously.
#[cfg(test)]
#[allow(dead_code)]
fn assert_cddl_rejects(spec: &str, root_rule: &str, bytes: &[u8]) {
    let rooted = cddl_oracle_rooted(spec, root_rule);
    assert!(
        cddl::validate_cbor_from_slice(&rooted, bytes, None).is_err(),
        "cddl oracle unexpectedly ACCEPTED non-conformant bytes for rule `{root_rule}`: {bytes:02x?}"
    );
}
