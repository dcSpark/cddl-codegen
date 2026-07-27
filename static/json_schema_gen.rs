// The `--json-schema-export` runtime helpers a generated `wasm/json-gen` crate imports from here:
// the row registrar `add_schema` and the document's reference-closure check
// `check_schema_ref_closure`. They live in the common runtime crate — ONE copy per workspace —
// and every json-gen crate pointed at that crate `use`s them, instead of each carrying its own
// emitted copy. The crate hosting this module never calls either one itself.
//
// Both checks live in the CONSUMER's own `cargo run` of their json-gen crate rather than in
// cddl-codegen's suite, for the same reason: we are the party that requires the hand-written
// `schemars::JsonSchema` impls they bite, and cddl-codegen's own suite asserting these properties
// over its own fixtures says nothing about a consumer's document.
//
// The panic wordings here are load-bearing. `snapshot_tests` pins the wiring, the
// `json-schema-name-merge` / `json-schema-name-stolen` / `json-schema-ref-dangling` fixtures assert
// on message fragments, and `docs/docs/command_line_flags.mdx` / `docs/docs/comment_dsl.mdx` quote
// them. Reword nothing.

/// The row helper every registration row in a generated `add_schemas` threads through.
/// `subschema_for::<T>()` registers T — and everything T references — into the generator's shared
/// `$defs` and returns a `$ref`, EXCEPT for a type whose `JsonSchema::inline_schema()` is true, where
/// it returns the schema itself and registers nothing (every `@newtype` wrapper over a primitive
/// emits exactly such an impl, since its JSON form IS the primitive's). Publishing the returned
/// schema under the type's own schema name in that case is what keeps EVERY exported row a `$defs`
/// entry.
///
/// The helper also carries the document's NAME-INJECTIVITY guard: a `$defs` key is the published API
/// name (`run-json2ts.js` suffixes it with `JSON` and json2ts emits it as the TypeScript type name),
/// so two Rust types claiming one `schemars::JsonSchema::schema_name()` either publish one type's
/// shape under the other's name (identical `schema_id`s — the default makes `schema_name` do double
/// duty as an identity, so schemars sees ONE type and emits ONE definition) or publish an
/// order-dependent `<name>2` (distinct ids — schemars' `{base}{i}` suffix loop). Both are silent, and
/// both make a published name a function of registration order rather than of the spec.
///
/// The ledger is keyed on the CALLING crate's own rows, never on `--json-schema-dep`: a dep registrar
/// call goes through the DEP's own `add_schemas`, which threads the dep's rows through this same
/// helper with a ledger of its own.
pub fn add_schema<T: schemars::JsonSchema>(
    generator: &mut schemars::SchemaGenerator,
    claimed: &mut std::collections::BTreeMap<String, &'static str>,
) {
    let name = <T as schemars::JsonSchema>::schema_name().into_owned();
    let rust = std::any::type_name::<T>();
    // A — the name ledger. Two rows claiming one published name is a silent MERGE when their
    // `schema_id`s also match (the id DEFAULTS to the name, so schemars sees one type, emits one
    // definition, and every reference to the loser resolves to the winner's shape) and an
    // order-dependent RENAME when the ids differ (schemars' `{base}{i}` suffix loop). The ledger is
    // the only check that can see the merge, since there both returned refs equal the shared name.
    // Keyed on `type_name` rather than on mere presence: two CDDL rules that alias the SAME rust
    // type (`a = ext<uint>` and `b = ext<uint>` both lower to a `pub type … = Ext<u64>;` alias) are
    // two rows for one type and must not trip the guard.
    if let Some(previous) = claimed
        .insert(name.clone(), rust)
        .filter(|previous| *previous != rust)
    {
        panic!("cddl-codegen --json-schema-export: two distinct Rust types both publish the JSON schema name\n\"{name}\":\n  {previous}\n  {rust}\nA schema document can define only one type per name, so one of these is published under the other's name or under an order-dependent \"{name}2\" — decided by registration order, not by your spec. Give each type a `schemars::JsonSchema::schema_name()` that is unique within this crate; for a generic, vary it with the parameters (e.g. `format!(\"Base_{{}}\", T::schema_name())`). Note `schema_id()` DEFAULTS to `schema_name()`, so a hand-written impl that returns a constant name makes every instantiation the same type as far as `schemars` is concerned.");
    }
    let schema = generator.subschema_for::<T>();
    if <T as schemars::JsonSchema>::inline_schema() {
        // C — an inline-schema type registers nothing, so the row publishes the returned schema under
        // its own name. Inserting only when vacant would silently keep a DIFFERENT type's body that
        // already claimed the name (reachable when the claimant is a non-row, non-inline type).
        let value = schema.to_value();
        let existing = generator.definitions().get(name.as_str()).cloned();
        if let Some(existing) = existing {
            if existing != value {
                panic!("cddl-codegen --json-schema-export: {rust} publishes the inline JSON schema name \"{name}\", but the document already defines \"{name}\" with a different body. Give one of them a `schemars::JsonSchema::schema_name()` that is unique within this crate.");
            }
        } else {
            generator.definitions_mut().insert(name.clone(), value);
        }
    } else if let Some(reference) = schema.get("$ref").and_then(|r| r.as_str()) {
        // B — the row kept its OWN name. `subschema_for` returns `<definitions_path>/<assigned>`,
        // where `assigned` is this type's `schema_name()` unless another type claimed it first, in
        // which case schemars hands out `<name>2`. This is what sees a collision whose WINNER has no
        // row of its own (a type reached only through another row's schema), which the ledger cannot.
        // A returned schema with no `$ref` at all is not a naming decision, so it is skipped.
        let definitions_path = generator.settings().definitions_path.to_string();
        let definitions_path = definitions_path
            .strip_prefix('#')
            .unwrap_or(definitions_path.as_str());
        let definitions_path = definitions_path
            .strip_suffix('/')
            .unwrap_or(definitions_path);
        let prefix = format!("#{definitions_path}/");
        // schemars percent-/JSON-pointer-encodes a name whose bytes are not safe inside a URI
        // fragment (`OrderedHashMap<K, V>` becomes `OrderedHashMap%3CK,%20V%3E`) with an encoder that
        // is not part of its public API. Comparing only names no encoder can touch keeps this guard
        // from ever failing a build over a difference it merely cannot decode — the ledger above
        // still covers every collision between two ROWS whatever the spelling.
        let ref_safe_name = name.bytes().all(|b| b.is_ascii_alphanumeric() || b == b'_');
        // `None` when the ref carries some other prefix (nothing to compare), when the name is one
        // the encoder may have touched, or when the row kept its own name.
        let stolen = reference
            .strip_prefix(prefix.as_str())
            .filter(|assigned| ref_safe_name && *assigned != name);
        if let Some(assigned) = stolen {
            panic!("cddl-codegen --json-schema-export: {rust} publishes the JSON schema name \"{name}\", but the document assigned it \"{assigned}\" — another type claimed \"{name}\" first. The published name is then decided by registration order, so an unrelated spec edit can silently swap the two. Give one of them a `schemars::JsonSchema::schema_name()` that is unique within this crate.");
        }
    }
}

fn collect_schema_refs(value: &serde_json::Value, out: &mut std::collections::BTreeSet<String>) {
    match value {
        serde_json::Value::Object(map) => {
            for (key, child) in map {
                if key == "$ref"
                    && let Some(reference) = child.as_str()
                {
                    out.insert(reference.to_owned());
                }
                collect_schema_refs(child, out);
            }
        }
        serde_json::Value::Array(items) => {
            for item in items {
                collect_schema_refs(item, out);
            }
        }
        _ => {}
    }
}

fn decode_schema_ref_name(encoded: &str) -> String {
    // Inverse of schemars' `encode_ref_name`, which is `pub fn` inside a PRIVATE `mod encoding` — not
    // reachable from here, so the decode is written out. Two layers, undone in the order the encoder
    // applied them: percent-decode the URI-fragment layer first (RFC 3986), then the JSON-Pointer
    // escapes (RFC 6901), `~1` before `~0` — a name holding a literal `~1` encodes to `~01` and
    // decodes wrong in the other order.
    //
    // DEcoding is safe here even though the add_schema helper deliberately refuses to ENcode: that
    // guard skips names it cannot reconstruct because reproducing a private encoder risks a false
    // panic on a schemars bump. Decoding is a well-defined standard operation independent of which
    // characters the encoder chose to escape, so it carries no such risk. Do not "fix" the asymmetry.
    let bytes = encoded.as_bytes();
    let mut decoded = Vec::with_capacity(bytes.len());
    let mut i = 0;
    while i < bytes.len() {
        let mut escaped = false;
        if bytes[i] == b'%'
            && i + 2 < bytes.len()
            && let Ok(byte) = u8::from_str_radix(&encoded[i + 1..i + 3], 16)
        {
            decoded.push(byte);
            i += 3;
            escaped = true;
        }
        if !escaped {
            decoded.push(bytes[i]);
            i += 1;
        }
    }
    String::from_utf8_lossy(&decoded)
        .replace("~1", "/")
        .replace("~0", "~")
}

/// The document's REFERENCE-CLOSURE check, run by the json-gen crate's `export_schemas()` before the
/// document is written. Every `$ref` in the finished document must be an internal pointer at one of
/// that same document's definitions; anything else — a bare relative name
/// (`Schema::new_ref("PlutusData")`, the shape a hand-written `JsonSchema` stub produces), a bare
/// `"#"`, an `http(s)://` URL, another document's path, or an internal pointer at a key nothing
/// defined — ships as a `.d.ts` that references a type it never declares (`TS2304`).
pub fn check_schema_ref_closure(document: &serde_json::Value, definitions_path: &str) {
    // The reference namespace is read off the generator's own settings, never hardcoded (the same
    // normalisation the add_schema helper does).
    let definitions_path = definitions_path
        .strip_prefix('#')
        .unwrap_or(definitions_path);
    let definitions_path = definitions_path
        .strip_suffix('/')
        .unwrap_or(definitions_path);
    let prefix = format!("#{definitions_path}/");
    // The definitions MAP is resolved through that same setting (as a JSON pointer into the finished
    // document) rather than assumed to be `$defs`, so the two halves of every comparison come from
    // one source. If it resolves to nothing, the reference namespace and the emitted document shape
    // have diverged — a schemars default change — and every reference would be a false positive.
    // Skipping is much cheaper than panicking in a build that is fine.
    let defs = match document.pointer(definitions_path).and_then(|d| d.as_object()) {
        Some(defs) => defs,
        None => return,
    };
    let mut references = std::collections::BTreeSet::new();
    collect_schema_refs(document, &mut references);
    // Sorted and deduplicated by the BTreeSet, so the same document always produces the same verdict
    // and the same message.
    let mut dangling = Vec::new();
    for reference in &references {
        match reference.strip_prefix(prefix.as_str()) {
            None => dangling.push(format!(
                "  {reference:?} — not an internal \"{prefix}<key>\" reference"
            )),
            Some(encoded) => {
                let key = decode_schema_ref_name(encoded);
                if !defs.contains_key(key.as_str()) {
                    dangling.push(format!(
                        "  {reference:?} — {key:?} is not defined in this document"
                    ));
                }
            }
        }
    }
    if !dangling.is_empty() {
        let dangling = dangling.join("\n");
        panic!("cddl-codegen --json-schema-export: the exported JSON schema document holds references that do not resolve inside it:\n{dangling}\nThe document is self-contained by contract — `run-json2ts.js` compiles it in one pass with no external resolution — so each of these ships as a `.d.ts` that references a type it never declares (TS2304). A reference like this comes from a hand-written `schemars::JsonSchema` impl that returned a REFERENCE where a schema body was expected: return the real body, or give the referenced type a registration row of its own (a CDDL rule, or `--json-schema-root`) so this document defines it.");
    }
}
