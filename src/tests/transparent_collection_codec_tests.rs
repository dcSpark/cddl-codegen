//! Standalone-codec inventory for named homogeneous collection rules.
//!
//! A transparent Rust alias owns no inherent methods or generated trait impls. Its member-position
//! codec can therefore work even when its structural target does not provide a complete standalone
//! `from_cbor_bytes` / `to_cbor_bytes` pair. This inventory makes that boundary executable across
//! the generation profiles rather than silently nominalizing every alias.

use crate::tests::integration_tests::{acquire_scratch_lock, checkout_hash, codegen_cmd, tool_cmd};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Ownership {
    TransparentTarget,
    NominalOwner,
}

#[derive(Clone, Copy)]
struct InventoryRow {
    id: &'static str,
    type_name: &'static str,
    ownership: Ownership,
    /// The right-hand side of the emitted transparent alias. Nominal rows leave this empty.
    target: &'static str,
    profiles: &'static [&'static str],
    complete_standalone_codec: bool,
    /// The hand-derived CBOR value for this named rule itself (or its holder below).
    foreign_wire: &'static str,
    holder: Option<&'static str>,
    holder_foreign_wire: Option<&'static str>,
    /// The alias expansion rustc must identify when its standalone decode door is unavailable.
    red_expansion: Option<&'static str>,
    red_door: Option<RedDoor>,
    repeated_named_plain_group: bool,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum RedDoor {
    Decode,
    Encode,
}

#[derive(Clone, Copy)]
struct Profile {
    id: &'static str,
    flags: &'static [&'static str],
    wasm: bool,
}

const PROFILES: &[Profile] = &[
    Profile {
        id: "default",
        flags: &["--wasm=false"],
        wasm: false,
    },
    Profile {
        id: "preserve",
        flags: &["--wasm=false", "--preserve-encodings=true"],
        wasm: false,
    },
    Profile {
        id: "json",
        flags: &[
            "--wasm=false",
            "--json-serde-derives=true",
            "--json-schema-export=true",
        ],
        wasm: false,
    },
    Profile {
        id: "wasm",
        flags: &["--wasm=true"],
        wasm: true,
    },
];

// The array-record wire is `[[1, "a"], [2, "b"]]`; every holder adds one outer array. The
// `pair_item = [pair]` inlines its named plain group, so nested pairs uses the same nested-array
// wire as the record aliases rather than adding another item-level array.
const ARRAY_RECORD: &str = "&[0x82, 0x82, 0x01, 0x61, 0x61, 0x82, 0x02, 0x61, 0x62]";
const ARRAY_RECORD_HOLDER: &str = "&[0x81, 0x82, 0x82, 0x01, 0x61, 0x61, 0x82, 0x02, 0x61, 0x62]";
const NESTED_PAIRS_HOLDER: &str = ARRAY_RECORD_HOLDER;
const FLAT_PAIRS: &str = "&[0x84, 0x01, 0x61, 0x61, 0x02, 0x61, 0x62]";

const ROWS: &[InventoryRow] = &[
    InventoryRow {
        id: "primitive_loose",
        type_name: "Nums",
        ownership: Ownership::TransparentTarget,
        target: "Vec<u64>",
        profiles: &["default", "preserve", "json", "wasm"],
        complete_standalone_codec: false,
        foreign_wire: "&[0x82, 0x01, 0x02]",
        holder: Some("NumsHolder"),
        holder_foreign_wire: Some("&[0x81, 0x82, 0x01, 0x02]"),
        red_expansion: Some("Vec<u64>"),
        red_door: Some(RedDoor::Encode),
        repeated_named_plain_group: false,
    },
    InventoryRow {
        id: "array_record_loose",
        type_name: "LooseItems",
        ownership: Ownership::TransparentTarget,
        target: "Vec<Item>",
        profiles: &["default", "preserve", "json", "wasm"],
        complete_standalone_codec: false,
        foreign_wire: ARRAY_RECORD_HOLDER,
        holder: Some("LooseHolder"),
        holder_foreign_wire: Some(ARRAY_RECORD_HOLDER),
        red_expansion: Some("Vec<Item>"),
        red_door: Some(RedDoor::Decode),
        repeated_named_plain_group: false,
    },
    InventoryRow {
        id: "array_record_nonempty",
        type_name: "NonemptyItems",
        ownership: Ownership::TransparentTarget,
        target: "NonEmptyVec<Item>",
        profiles: &["default", "preserve", "json", "wasm"],
        complete_standalone_codec: false,
        foreign_wire: ARRAY_RECORD_HOLDER,
        holder: Some("NonemptyHolder"),
        holder_foreign_wire: Some(ARRAY_RECORD_HOLDER),
        red_expansion: Some("NonEmptyVec<Item>"),
        red_door: Some(RedDoor::Decode),
        repeated_named_plain_group: false,
    },
    InventoryRow {
        id: "array_record_bounded",
        type_name: "BoundedItems",
        ownership: Ownership::TransparentTarget,
        target: "BoundedVec<Item, 1, 3>",
        profiles: &["default", "preserve", "json", "wasm"],
        complete_standalone_codec: false,
        foreign_wire: ARRAY_RECORD_HOLDER,
        holder: Some("BoundedHolder"),
        holder_foreign_wire: Some(ARRAY_RECORD_HOLDER),
        red_expansion: Some("BoundedVec<Item, 1, 3>"),
        red_door: Some(RedDoor::Decode),
        repeated_named_plain_group: false,
    },
    InventoryRow {
        id: "array_record_exact",
        type_name: "ExactItems",
        ownership: Ownership::TransparentTarget,
        target: "[Item; 2]",
        profiles: &["default", "preserve", "json", "wasm"],
        complete_standalone_codec: false,
        foreign_wire: ARRAY_RECORD_HOLDER,
        holder: Some("ExactHolder"),
        holder_foreign_wire: Some(ARRAY_RECORD_HOLDER),
        red_expansion: Some("[Item; 2]"),
        red_door: Some(RedDoor::Decode),
        repeated_named_plain_group: false,
    },
    InventoryRow {
        id: "plain_group_nested",
        type_name: "NestedPairs",
        ownership: Ownership::TransparentTarget,
        target: "Vec<PairItem>",
        profiles: &["default", "preserve", "json", "wasm"],
        complete_standalone_codec: false,
        foreign_wire: NESTED_PAIRS_HOLDER,
        holder: Some("NestedHolder"),
        holder_foreign_wire: Some(NESTED_PAIRS_HOLDER),
        red_expansion: Some("Vec<PairItem>"),
        red_door: Some(RedDoor::Decode),
        repeated_named_plain_group: false,
    },
    InventoryRow {
        id: "flat_named_plain_group",
        type_name: "FlatPairs",
        ownership: Ownership::NominalOwner,
        target: "",
        profiles: &["default", "preserve", "json", "wasm"],
        complete_standalone_codec: true,
        foreign_wire: FLAT_PAIRS,
        holder: None,
        holder_foreign_wire: None,
        red_expansion: None,
        red_door: None,
        repeated_named_plain_group: true,
    },
    InventoryRow {
        id: "newtype_remedy",
        type_name: "NewtypeItems",
        ownership: Ownership::NominalOwner,
        target: "",
        profiles: &["default", "preserve", "json", "wasm"],
        complete_standalone_codec: true,
        foreign_wire: ARRAY_RECORD,
        holder: None,
        holder_foreign_wire: None,
        red_expansion: None,
        red_door: None,
        repeated_named_plain_group: false,
    },
];

fn row_source_assertion(row: InventoryRow) -> String {
    match row.ownership {
        Ownership::TransparentTarget => format!("pub type {} = {};", row.type_name, row.target),
        Ownership::NominalOwner => format!("pub struct {}", row.type_name),
    }
}

fn emitted_test_source(rows: &[InventoryRow]) -> String {
    let mut source = String::from(
        "\n#[cfg(test)]\nmod __transparent_collection_codec_inventory {\n    use super::*;\n    use super::serialization::{Deserialize, ToCBORBytes};\n\n    #[test]\n    fn foreign_wire_inventory() {\n",
    );
    for row in rows {
        if row.complete_standalone_codec {
            source.push_str(&format!(
                "        assert_eq!({}::from_cbor_bytes({}).expect(\"{} standalone decode\").to_cbor_bytes(), {}, \"{} standalone codec must round-trip its foreign bytes\");\n",
                row.type_name, row.foreign_wire, row.id, row.foreign_wire, row.id
            ));
        } else if row.id == "primitive_loose" {
            source.push_str(&format!(
                "        assert_eq!({}::from_cbor_bytes({}).expect(\"{} target decode\"), vec![1, 2], \"{} must retain Vec's target-owned decode door\");\n",
                row.type_name, row.foreign_wire, row.id, row.id
            ));
        }
        if !row.complete_standalone_codec
            && let Some(holder) = row.holder
        {
            let holder_wire = row
                .holder_foreign_wire
                .expect("holder rows need hand-derived holder bytes");
            source.push_str(&format!(
                "        assert_eq!({holder}::from_cbor_bytes({}).expect(\"{} member-position decode\").to_cbor_bytes(), {}, \"{} member-position codec must round-trip its foreign bytes\");\n",
                holder_wire, row.id, holder_wire, row.id
            ));
        }
    }
    source.push_str("    }\n}\n");
    source
}

fn wasm_impl_span<'a>(source: &'a str, type_name: &str) -> &'a str {
    let start = source
        .find(&format!("impl {type_name} {{"))
        .unwrap_or_else(|| panic!("wasm wrapper `{type_name}` has no inherent impl:\n{source}"));
    let rest = &source[start..];
    let end = rest.find("\n#[derive").unwrap_or(rest.len());
    &rest[..end]
}

fn write_red_probe(
    root: &std::path::Path,
    row: InventoryRow,
    profile: Profile,
    rust_dir: &std::path::Path,
) -> std::path::PathBuf {
    let probe = root.join(format!("red_{}_{}", profile.id, row.id));
    std::fs::create_dir_all(probe.join("src")).unwrap();
    let package = format!(
        "cycle13-codec-red-{}-{}",
        profile.id,
        row.id.replace('_', "-")
    );
    std::fs::write(
        probe.join("Cargo.toml"),
        format!(
            "[package]\nname = \"{package}\"\nversion = \"0.0.0\"\nedition = \"2024\"\n\n[dependencies]\ncddl-lib = {{ path = \"{}\" }}\n",
            rust_dir.display()
        ),
    )
    .unwrap();
    let (import, call) = match row.red_door.expect("red probe needs an unavailable door") {
        RedDoor::Decode => (
            "use cddl_lib::serialization::Deserialize;",
            format!("cddl_lib::{}::from_cbor_bytes(&[])", row.type_name),
        ),
        RedDoor::Encode => (
            "use cddl_lib::serialization::ToCBORBytes;",
            format!("cddl_lib::{}::new().to_cbor_bytes()", row.type_name),
        ),
    };
    std::fs::write(
        probe.join("src/main.rs"),
        format!("{import}\nfn main() {{\n    let _ = {call};\n}}\n"),
    )
    .unwrap();
    probe
}

/// The API inventory sits at local tier: it builds isolated generated crates, executes every
/// advertised standalone door on hand-derived bytes, then proves each intentional absence with an
/// independent red cargo invocation. It must not be weakened to a source-only assertion: a
/// transparent alias can compile in a member while its standalone target lacks cbor_event's trait.
#[test]
fn transparent_collection_codec_inventory() {
    if !std::process::Command::new("cargo")
        .arg("--version")
        .output()
        .map(|output| output.status.success())
        .unwrap_or(false)
    {
        return;
    }

    for ownership in [Ownership::TransparentTarget, Ownership::NominalOwner] {
        assert!(
            ROWS.iter().any(|row| row.ownership == ownership),
            "inventory lost its {ownership:?} ownership class"
        );
    }
    for profile in PROFILES {
        assert!(
            ROWS.iter().any(|row| row.profiles.contains(&profile.id)),
            "inventory lost every row for the `{}` profile",
            profile.id
        );
    }
    for complete in [true, false] {
        assert!(
            ROWS.iter()
                .any(|row| row.complete_standalone_codec == complete),
            "inventory lost its complete-standalone-codec={complete} class"
        );
    }
    for red_door in [RedDoor::Decode, RedDoor::Encode] {
        assert!(
            ROWS.iter().any(|row| row.red_door == Some(red_door)),
            "inventory lost its {red_door:?} red-door class"
        );
    }
    assert!(
        ROWS.iter()
            .filter(|row| !row.complete_standalone_codec)
            .all(|row| row.red_door.is_some() && row.red_expansion.is_some()),
        "every incomplete row must carry an isolated, expansion-pinned red door"
    );
    assert_eq!(
        ROWS.iter()
            .filter(|row| row.repeated_named_plain_group)
            .count(),
        1,
        "the Cycle-12 repeated named plain-group carrier must remain one deliberate nominal contrast"
    );

    let scratch_name = format!(
        "cddl_codegen_transparent_collection_codec_{:016x}",
        checkout_hash()
    );
    let _scratch_lock = acquire_scratch_lock(&scratch_name);
    let root = std::env::temp_dir().join(&scratch_name);
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(&root).unwrap();
    let input = root.join("inventory.cddl");
    std::fs::write(&input, "nums = [* uint]\nnums_holder = [value: nums]\nitem = [a: uint, b: tstr]\nloose_items = [* item]\nnonempty_items = [+ item]\nbounded_items = [1*3 item]\nexact_items = [2*2 item]\npair = (a: uint, b: tstr)\npair_item = [pair]\nnested_pairs = [* pair_item]\nflat_pairs = [* pair]\nnewtype_items = [* item] ; @newtype entries\nloose_holder = [value: loose_items]\nnonempty_holder = [value: nonempty_items]\nbounded_holder = [value: bounded_items]\nexact_holder = [value: exact_items]\nnested_holder = [value: nested_pairs]\n").unwrap();
    let static_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("static");

    for profile in PROFILES {
        let rows = ROWS
            .iter()
            .copied()
            .filter(|row| row.profiles.contains(&profile.id))
            .collect::<Vec<_>>();
        assert!(
            !rows.is_empty(),
            "{} profile must exercise at least one row",
            profile.id
        );
        let export = root.join(profile.id);
        // Every red package has its own identity, but they all depend on this exact profile's one
        // generated `cddl-lib`. Reusing its target avoids rebuilding the same crate per probe;
        // targets stay profile-scoped because those generated packages share name/version while
        // differing in content and manifest path.
        let target_dir = root.join(format!("target_{}", profile.id));
        let generated = codegen_cmd()
            .arg(format!("--input={}", input.display()))
            .arg(format!("--output={}", export.display()))
            .arg(format!("--static-dir={}", static_dir.display()))
            .args(profile.flags)
            .output()
            .unwrap();
        assert!(
            generated.status.success(),
            "{} profile generation failed:\n{}",
            profile.id,
            String::from_utf8_lossy(&generated.stderr)
        );

        let rust_dir = export.join("rust");
        let generated_mod = rust_dir.join("src/generated/mod.rs");
        let original = std::fs::read_to_string(&generated_mod).unwrap();
        for row in &rows {
            let expected = row_source_assertion(*row);
            assert!(
                original.contains(&expected),
                "{} / {} must emit `{expected}` rather than change ownership:\n{original}",
                profile.id,
                row.id
            );
        }
        std::fs::write(
            &generated_mod,
            format!("{original}{}", emitted_test_source(&rows)),
        )
        .unwrap();
        let executed = tool_cmd("cargo")
            .args(["test", "--lib", "__transparent_collection_codec_inventory"])
            .current_dir(&rust_dir)
            .env("CARGO_TARGET_DIR", &target_dir)
            .output()
            .unwrap();
        assert!(
            executed.status.success(),
            "{} profile foreign-wire execution failed:\n--- stdout ---\n{}\n--- stderr ---\n{}",
            profile.id,
            String::from_utf8_lossy(&executed.stdout),
            String::from_utf8_lossy(&executed.stderr)
        );

        for row in rows.iter().copied().filter(|row| row.red_door.is_some()) {
            let probe = write_red_probe(&root, row, *profile, &rust_dir);
            let red = tool_cmd("cargo")
                .arg("check")
                .current_dir(&probe)
                .env("CARGO_TARGET_DIR", &target_dir)
                .output()
                .unwrap();
            let stderr = String::from_utf8_lossy(&red.stderr);
            let expansion = row
                .red_expansion
                .expect("incomplete row must pin its expansion");
            let (door, missing_trait) = match row.red_door.unwrap() {
                RedDoor::Decode => ("decode", "cbor_event::de::Deserialize"),
                RedDoor::Encode => ("encode", "cbor_event::se::Serialize"),
            };
            assert!(
                !red.status.success()
                    && stderr.contains("error[E0599]")
                    && stderr.contains(expansion)
                    && stderr.contains(missing_trait),
                "{} / {} standalone {door} must remain an isolated E0599 on `{expansion}` caused by the missing `{missing_trait}` trait; a successful check means the door resurfaced and this row must be reclassified. stderr:\n{stderr}",
                profile.id,
                row.id
            );
        }

        if profile.wasm {
            let wasm_source =
                std::fs::read_to_string(export.join("wasm/src/generated/mod.rs")).unwrap();
            for row in &rows {
                assert!(
                    wasm_source.contains(&format!("pub struct {}", row.type_name)),
                    "wasm profile must emit a wrapper class for {}:\n{wasm_source}",
                    row.type_name
                );
                let wasm_impl = wasm_impl_span(&wasm_source, row.type_name);
                let has_decode = wasm_impl.contains("pub fn from_cbor_bytes(");
                let has_encode = wasm_impl.contains("pub fn to_cbor_bytes(");
                assert_eq!(
                    has_decode, row.complete_standalone_codec,
                    "wasm wrapper {} must expose from_cbor_bytes exactly when its Rust row has a complete standalone codec",
                    row.type_name
                );
                assert_eq!(
                    has_encode, row.complete_standalone_codec,
                    "wasm wrapper {} must expose to_cbor_bytes exactly when its Rust row has a complete standalone codec",
                    row.type_name
                );
            }
            let wasm_check = tool_cmd("cargo")
                .arg("check")
                .current_dir(export.join("wasm"))
                .env("CARGO_TARGET_DIR", &target_dir)
                .output()
                .unwrap();
            assert!(
                wasm_check.status.success(),
                "wasm-bearing inventory profile must compile the emitted wasm crate:\n{}",
                String::from_utf8_lossy(&wasm_check.stderr)
            );
        }
    }
    std::fs::remove_dir_all(&root).unwrap_or_else(|error| {
        panic!("successful inventory run could not remove {root:?}: {error}")
    });
}
