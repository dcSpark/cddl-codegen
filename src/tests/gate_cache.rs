use sha2::{Digest, Sha256};
use std::path::{Path, PathBuf};
use std::sync::OnceLock;

const SCHEMA: &str = "gate-cache-v1";
const RUSTFLAGS_AS_NESTED_CARGO_SEES_IT: &str = "";
/// Schema tag for the in-process lockfile memo below. It is NOT part of [`SCHEMA`]: the memo is a
/// derivation cache that never outlives the process, so it shares nothing with the on-disk gate
/// cache and must not perturb its key.
const LOCKFILE_MEMO_SCHEMA: &str = "lockfile-memo-v1";

#[derive(Clone, Debug)]
struct CacheConfig {
    enabled: bool,
    dir: PathBuf,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum GateCacheOutcome {
    Hit,
    RanPass,
    RanFail,
}

impl GateCacheOutcome {
    pub(crate) fn success(self) -> bool {
        matches!(self, Self::Hit | Self::RanPass)
    }

    pub(crate) fn ran(self) -> usize {
        usize::from(!matches!(self, Self::Hit))
    }

    pub(crate) fn cached(self) -> usize {
        usize::from(matches!(self, Self::Hit))
    }
}

pub(crate) fn enabled() -> bool {
    CacheConfig::from_env().enabled
}

/// Memoize a generated-crate cargo verdict. The lockfile is generated before hashing so dependency
/// resolution is part of the consumed tree.
///
/// Timed as ONE bracket around everything below, which is what a cell actually costs the gate: even
/// a HIT pays for the `cargo generate-lockfile` preflight, so timing only the `build()` closure would
/// report a cached cell as free when it is not. `timing_cells::emit` is inert unless check.ts asked
/// for rows, and can never fail the cell — see that module's header.
pub(crate) fn run_cached(
    gate: &str,
    cell: &str,
    generated_root: &Path,
    manifest_subpaths: &[PathBuf],
    argv_for_key: &[String],
    build: impl FnOnce() -> bool,
) -> GateCacheOutcome {
    let started = std::time::Instant::now();
    let outcome = run_cached_timed(
        gate,
        cell,
        generated_root,
        manifest_subpaths,
        argv_for_key,
        build,
    );
    let label = match outcome {
        GateCacheOutcome::Hit => "hit",
        GateCacheOutcome::RanPass => "run_pass",
        GateCacheOutcome::RanFail => "run_fail",
    };
    super::timing_cells::emit(gate, cell, label, started.elapsed().as_millis());
    outcome
}

fn run_cached_timed(
    gate: &str,
    cell: &str,
    generated_root: &Path,
    manifest_subpaths: &[PathBuf],
    argv_for_key: &[String],
    build: impl FnOnce() -> bool,
) -> GateCacheOutcome {
    let config = CacheConfig::from_env();
    if !config.enabled {
        return if build() {
            GateCacheOutcome::RanPass
        } else {
            GateCacheOutcome::RanFail
        };
    }

    for manifest_subpath in manifest_subpaths {
        if !ensure_lockfile(generated_root, manifest_subpath) {
            return if build() {
                GateCacheOutcome::RanPass
            } else {
                GateCacheOutcome::RanFail
            };
        }
    }
    if lockfile_verify_enabled() {
        verify_lockfiles_byte_identical(gate, cell, generated_root, manifest_subpaths);
    }

    run_cached_with(config, gate, cell, generated_root, argv_for_key, build)
}

// ==================================================================================================
// The lockfile preflight, and the in-process memo for its DERIVATION
// ==================================================================================================
//
// Every cell — including a HIT — must end up with the same `Cargo.lock` bytes in its tree that
// `cargo generate-lockfile` would have written, because that lock is hashed into the gate-cache key
// (see `run_cached`'s doc comment). What is memoized is only the DERIVATION: the resolution is run
// once per distinct set of resolution inputs and the resulting bytes are written into every later
// cell that shares them. The key covers exactly what it covered before; nothing about
// `gate_cache_key`, `hash_tree`, or the entry format changes.
//
// The memo is worth having because the generated manifests are IDENTICAL across fixtures — a corpus
// cell's `rust/Cargo.toml` differs only by generation PROFILE, not by fixture — while the preflight
// was paid once per cell per manifest: 952 `cargo generate-lockfile` processes in one measured
// `cargo test --all-features --all-targets` run, each taking cargo's process-wide
// `$CARGO_HOME/.package-cache` lock and therefore serializing across libtest's threads.
//
// What the resolution actually depends on, and where each dependency lives in the key:
//
//   | dependency                                  | in the key                                     |
//   |---------------------------------------------|------------------------------------------------|
//   | the manifest at `manifest_subpath`          | yes — hashed by content                        |
//   | transitive PATH-dep manifests (`wasm` →     | yes — EVERY `Cargo.toml` in the tree is hashed,|
//   | `../rust`, `json-gen` → `../../rust`)       | which is a superset of the closure             |
//   | feature tables (they gate optional deps)    | yes — they are part of those manifests         |
//   | a `.cargo/config.toml` / `.cargo/config`    | yes, when inside the tree; when ABOVE it, the  |
//   | (`[patch]`, source replacement, registries) | tree's parent directory is in the key and all  |
//   |                                             | cells of a gate share one scratch root, so an  |
//   |                                             | equal key means a literally identical ancestry |
//   | a parent workspace `Cargo.toml`             | same as above; and a manifest that resolved to |
//   |                                             | a workspace root elsewhere writes its lock     |
//   |                                             | elsewhere, which disables memoization for it   |
//   | `rust-toolchain{,.toml}` (resolution is     | yes, when inside the tree; outside it is       |
//   | MSRV-aware: "Locking N packages to latest   | process-global, and the rustc version is       |
//   | Rust <v> compatible versions")              | already a `gate_cache_key` component           |
//   | the registry index + `$CARGO_HOME`          | NOT in the key, deliberately: process-global,  |
//   |                                             | and `check.ts` warms then forces               |
//   |                                             | `CARGO_NET_OFFLINE=true` for every gate, so it |
//   |                                             | is frozen for a run. A hand-run that is ONLINE |
//   |                                             | could in principle refresh the index mid-run;  |
//   |                                             | that is what `GATE_CACHE_LOCKFILE_VERIFY`      |
//   |                                             | exists to catch.                               |
//   | a PRE-EXISTING `Cargo.lock` in the tree     | NOT in the key, and NOT an input: probed —     |
//   |                                             | a lock hand-edited to pin `linked-hash-map`    |
//   |                                             | 0.5.4 came back 0.5.6 after                    |
//   |                                             | `generate-lockfile`, byte-identical to a       |
//   |                                             | sibling cell that had no prior lock. It is the |
//   |                                             | OUTPUT of this step, not an input to it.       |
//
// The premise "the lock is a pure function of those inputs" is not left as an argument:
// `GATE_CACHE_LOCKFILE_VERIFY=1` makes every cell re-derive its lockfiles with cargo and asserts the
// resulting tree is BYTE-IDENTICAL to the memoized one. Off by default — it doubles the preflight
// cost, which is the thing being removed.

/// One memoized derivation. `None` = not derived yet (or the last attempt failed, which is
/// deliberately not memoized so a transient cargo failure does not poison later cells).
type LockfileSlot = std::sync::Arc<std::sync::Mutex<Option<Vec<u8>>>>;

static LOCKFILE_MEMO: std::sync::Mutex<std::collections::BTreeMap<String, LockfileSlot>> =
    std::sync::Mutex::new(std::collections::BTreeMap::new());

/// Whether `$GATE_CACHE_LOCKFILE_VERIFY` asks every cell to prove the memo against cargo.
fn lockfile_verify_enabled() -> bool {
    static VERIFY: OnceLock<bool> = OnceLock::new();
    *VERIFY.get_or_init(|| {
        matches!(
            std::env::var("GATE_CACHE_LOCKFILE_VERIFY"),
            Ok(v) if matches!(v.to_ascii_lowercase().as_str(), "1" | "true")
        )
    })
}

/// The unmemoized preflight, unchanged in behaviour from what it replaced: `false` on any failure,
/// which the caller turns into an uncached `build()`.
fn generate_lockfile_fresh(generated_root: &Path, manifest_subpath: &Path) -> bool {
    super::integration_tests::tool_cmd("cargo")
        .arg("generate-lockfile")
        .arg("--manifest-path")
        .arg(generated_root.join(manifest_subpath))
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Where a STANDALONE package's lock lands: next to its manifest. A manifest that belongs to a
/// workspace rooted elsewhere writes its lock at that root instead — which shows up here as the file
/// simply not existing, and disables memoization for that manifest rather than memoizing a lock the
/// cell does not actually own.
fn lockfile_path(generated_root: &Path, manifest_subpath: &Path) -> PathBuf {
    generated_root
        .join(manifest_subpath)
        .with_file_name("Cargo.lock")
}

/// Put the `Cargo.lock` for `manifest_subpath` in the tree, deriving it with cargo only when no
/// earlier cell already derived it from an identical set of resolution inputs.
fn ensure_lockfile(generated_root: &Path, manifest_subpath: &Path) -> bool {
    let Some(key) = lockfile_memo_key(generated_root, manifest_subpath) else {
        return generate_lockfile_fresh(generated_root, manifest_subpath);
    };
    let slot = {
        let mut memo = LOCKFILE_MEMO.lock().unwrap_or_else(|e| e.into_inner());
        memo.entry(key).or_default().clone()
    };
    // Held across the cargo call on purpose: threads that share a manifest set derive it ONCE, the
    // rest wait. Threads with DIFFERENT keys never contend — the map lock above is released first —
    // and the slot locks are taken one at a time, so there is no lock ordering to get wrong.
    let mut slot = slot.lock().unwrap_or_else(|e| e.into_inner());
    let dest = lockfile_path(generated_root, manifest_subpath);
    if let Some(bytes) = slot.as_ref() {
        return std::fs::write(&dest, bytes).is_ok();
    }
    if !generate_lockfile_fresh(generated_root, manifest_subpath) {
        return false;
    }
    if let Ok(bytes) = std::fs::read(&dest) {
        *slot = Some(bytes);
    }
    true
}

/// The memo key. `None` means "do not memoize this manifest" — the caller falls back to a fresh
/// cargo run, which is always correct and merely slower.
fn lockfile_memo_key(generated_root: &Path, manifest_subpath: &Path) -> Option<String> {
    let root = generated_root.canonicalize().ok()?;
    // The tree's PARENT, not the tree: cells of a gate are siblings under one scratch root, so an
    // equal parent path means an identical chain of ancestor directories — the only place an
    // out-of-tree `.cargo/config.toml` or parent workspace manifest could sit.
    let parent = root.parent()?.to_string_lossy().into_owned();
    let mut inputs = Vec::new();
    collect_lockfile_inputs(&root, &root, &mut inputs).ok()?;
    inputs.sort();

    let mut h = Sha256::new();
    h.update(LOCKFILE_MEMO_SCHEMA.as_bytes());
    h.update([0]);
    h.update(parent.as_bytes());
    h.update([0]);
    h.update(slash_path(manifest_subpath).as_bytes());
    h.update([0]);
    for rel in &inputs {
        let bytes = std::fs::read(root.join(rel.split('/').collect::<PathBuf>())).ok()?;
        if rel.ends_with("Cargo.toml") && manifest_path_dep_escapes(rel, &bytes) {
            return None;
        }
        h.update(rel.as_bytes());
        h.update([0]);
        h.update(bytes.len().to_string().as_bytes());
        h.update([0]);
        h.update(&bytes);
        h.update([0]);
    }
    Some(hex(h.finalize()))
}

/// Files inside a generated tree that dependency resolution reads. The `.rs` sources are excluded
/// because they cannot change the resolution — and because including them would give every cell a
/// distinct key, which is the whole memo.
fn is_lockfile_input(rel: &str) -> bool {
    let mut parts = rel.rsplit('/');
    let name = parts.next().unwrap_or(rel);
    if matches!(
        name,
        "Cargo.toml" | "rust-toolchain" | "rust-toolchain.toml"
    ) {
        return true;
    }
    parts.next() == Some(".cargo") && matches!(name, "config" | "config.toml")
}

fn collect_lockfile_inputs(
    root: &Path,
    dir: &Path,
    files: &mut Vec<String>,
) -> std::io::Result<()> {
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        let file_type = entry.file_type()?;
        if file_type.is_dir() {
            if entry.file_name() != "target" {
                collect_lockfile_inputs(root, &path, files)?;
            }
        } else if file_type.is_file() {
            let rel = slash_path(path.strip_prefix(root).unwrap());
            if is_lockfile_input(&rel) {
                files.push(rel);
            }
        }
    }
    Ok(())
}

fn slash_path(path: &Path) -> String {
    path.components()
        .map(|c| c.as_os_str().to_string_lossy())
        .collect::<Vec<_>>()
        .join("/")
}

/// Conservative textual scan for a `path = "…"` dependency pointing OUTSIDE the generated tree.
/// Such a manifest lives in the key while the manifest it points at does not, so the honest answer
/// is to not memoize it at all. Deliberately textual and over-eager: a false positive costs one
/// un-memoized manifest set, and a false negative is what the byte-identity gate exists to catch.
fn manifest_path_dep_escapes(manifest_rel: &str, bytes: &[u8]) -> bool {
    let text = String::from_utf8_lossy(bytes);
    let mut dir: Vec<&str> = manifest_rel.split('/').collect();
    dir.pop(); // the manifest's own file name
    for value in path_key_values(&text) {
        if value.starts_with('/') || value.contains(':') {
            return true; // absolute, or a Windows drive path
        }
        let mut stack = dir.clone();
        for component in value.split(['/', '\\']) {
            match component {
                "" | "." => {}
                ".." => {
                    if stack.pop().is_none() {
                        return true;
                    }
                }
                other => stack.push(other),
            }
        }
    }
    false
}

/// Every `path = "…"` value in a manifest. Not a TOML parse: it matches the key anywhere, so a
/// `path` inside a string value or a comment reads as a dependency too — which only ever costs a
/// memo entry.
fn path_key_values(text: &str) -> Vec<String> {
    let mut out = Vec::new();
    for (i, _) in text.match_indices("path") {
        let key_start = text[..i]
            .chars()
            .next_back()
            .is_none_or(|c| c.is_whitespace() || c == '{' || c == ',');
        if !key_start {
            continue;
        }
        let Some(rest) = text[i + "path".len()..].trim_start().strip_prefix('=') else {
            continue;
        };
        let Some(rest) = rest.trim_start().strip_prefix('"') else {
            continue;
        };
        if let Some(end) = rest.find('"') {
            out.push(rest[..end].to_string());
        }
    }
    out
}

/// `GATE_CACHE_LOCKFILE_VERIFY=1`: re-derive every one of this cell's lockfiles with cargo and prove
/// the tree the memo produced is byte-identical to the freshly-derived one. A difference is a
/// FINDING — the memo key would be missing a resolution input — so it panics with the offending
/// files rather than degrading quietly.
fn verify_lockfiles_byte_identical(
    gate: &str,
    cell: &str,
    generated_root: &Path,
    manifest_subpaths: &[PathBuf],
) {
    let memoized = tree_file_digests(generated_root)
        .expect("GATE_CACHE_LOCKFILE_VERIFY: the generated tree must be readable to be compared");
    for manifest_subpath in manifest_subpaths {
        assert!(
            generate_lockfile_fresh(generated_root, manifest_subpath),
            "GATE_CACHE_LOCKFILE_VERIFY: `cargo generate-lockfile` failed for {manifest_subpath:?} \
             in {gate}/{cell} on the re-derivation pass, so the memo could not be checked"
        );
    }
    let fresh = tree_file_digests(generated_root)
        .expect("GATE_CACHE_LOCKFILE_VERIFY: the generated tree must be readable to be compared");
    if memoized == fresh {
        return;
    }
    let mut differing: Vec<String> = Vec::new();
    for rel in memoized.keys().chain(fresh.keys()) {
        if memoized.get(rel) != fresh.get(rel) && !differing.contains(rel) {
            differing.push(rel.clone());
        }
    }
    differing.sort();
    panic!(
        "GATE_CACHE_LOCKFILE_VERIFY: the memoized lockfile derivation is NOT byte-identical to a \
         fresh one in {gate}/{cell} ({generated_root:?}).\nThe lockfile memo key is missing a \
         resolution input — this is a finding, not a nuisance: the gate cache would key on a tree \
         that differs from what cargo actually resolves.\ndiffering files:\n  {}",
        differing.join("\n  ")
    );
}

/// Per-file content digests over the same file set `hash_tree` walks — a diffable form of the tree
/// hash, used only by the verification pass above.
fn tree_file_digests(root: &Path) -> std::io::Result<std::collections::BTreeMap<String, String>> {
    let root = root.canonicalize()?;
    let mut files = Vec::new();
    collect_files(&root, &root, &mut files)?;
    let mut out = std::collections::BTreeMap::new();
    for rel in files {
        let bytes = std::fs::read(root.join(rel.split('/').collect::<PathBuf>()))?;
        let mut h = Sha256::new();
        h.update(&bytes);
        out.insert(rel, hex(h.finalize()));
    }
    Ok(out)
}

fn run_cached_with(
    config: CacheConfig,
    gate: &str,
    cell: &str,
    generated_root: &Path,
    argv_for_key: &[String],
    build: impl FnOnce() -> bool,
) -> GateCacheOutcome {
    if !config.enabled {
        return if build() {
            GateCacheOutcome::RanPass
        } else {
            GateCacheOutcome::RanFail
        };
    }

    let tree = match hash_tree(generated_root) {
        Ok(tree) => tree,
        Err(_) => {
            return if build() {
                GateCacheOutcome::RanPass
            } else {
                GateCacheOutcome::RanFail
            };
        }
    };
    let (key, rustc) = gate_cache_key(gate, argv_for_key, &tree);
    if read_gate_cache_entry(&config.dir, &key).is_some() {
        println!(
            "[gate-cache] {cell}: cached PASS (key {})",
            &key[..8.min(key.len())]
        );
        return GateCacheOutcome::Hit;
    }

    if build() {
        let entry = GateCacheEntry {
            gate,
            cell,
            argv: argv_for_key,
            rustc: &rustc,
            tree: &tree,
            created: created_timestamp(),
        };
        let _ = write_gate_cache_entry(&config.dir, &key, &entry);
        GateCacheOutcome::RanPass
    } else {
        GateCacheOutcome::RanFail
    }
}

impl CacheConfig {
    fn from_env() -> Self {
        let enabled = !matches!(
            std::env::var("GATE_CACHE"),
            Ok(v) if matches!(v.to_ascii_lowercase().as_str(), "0" | "false")
        );
        let dir = std::env::var_os("GATE_CACHE_DIR")
            .map(PathBuf::from)
            .unwrap_or_else(|| {
                std::env::current_dir()
                    .unwrap_or_else(|_| PathBuf::from("."))
                    .join(".gate-cache")
            });
        Self { enabled, dir }
    }
}

struct GateCacheEntry<'a> {
    gate: &'a str,
    cell: &'a str,
    argv: &'a [String],
    rustc: &'a str,
    tree: &'a str,
    created: String,
}

fn rustc_version_verbose() -> &'static str {
    static RUSTC: OnceLock<String> = OnceLock::new();
    RUSTC.get_or_init(|| {
        std::process::Command::new("rustc")
            .arg("-vV")
            .output()
            .map(|o| {
                let mut out = String::from_utf8_lossy(&o.stdout).into_owned();
                out.push_str(&String::from_utf8_lossy(&o.stderr));
                out
            })
            .unwrap_or_default()
    })
}

fn hash_tree(root: &Path) -> std::io::Result<String> {
    let root = root.canonicalize()?;
    let mut files = Vec::new();
    collect_files(&root, &root, &mut files)?;
    files.sort();

    let mut h = Sha256::new();
    for rel in files {
        let path = root.join(rel.split('/').collect::<PathBuf>());
        let bytes = std::fs::read(&path)?;
        h.update(rel.as_bytes());
        h.update([0]);
        h.update(bytes.len().to_string().as_bytes());
        h.update([0]);
        h.update(&bytes);
        h.update([0]);
    }
    Ok(hex(h.finalize()))
}

fn collect_files(root: &Path, dir: &Path, files: &mut Vec<String>) -> std::io::Result<()> {
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        let file_type = entry.file_type()?;
        if file_type.is_dir() {
            if entry.file_name() != "target" {
                collect_files(root, &path, files)?;
            }
        } else if file_type.is_file() {
            let rel = path
                .strip_prefix(root)
                .unwrap()
                .components()
                .map(|c| c.as_os_str().to_string_lossy())
                .collect::<Vec<_>>()
                .join("/");
            files.push(rel);
        }
    }
    Ok(())
}

fn gate_cache_key(gate: &str, argv: &[String], tree: &str) -> (String, String) {
    let rustc = rustc_version_verbose().to_string();
    let material = key_material(gate, argv, &rustc, RUSTFLAGS_AS_NESTED_CARGO_SEES_IT, tree);
    let mut h = Sha256::new();
    h.update(material.as_bytes());
    (hex(h.finalize()), rustc)
}

fn key_material(gate: &str, argv: &[String], rustc: &str, rustflags: &str, tree: &str) -> String {
    let argv = argv
        .iter()
        .map(|arg| serde_json::to_string(arg).unwrap())
        .collect::<Vec<_>>()
        .join(",");
    format!(
        "{{\"schema\":{},\"gate\":{},\"argv\":[{}],\"rustc\":{},\"rustflags\":{},\"tree\":{}}}\n",
        serde_json::to_string(SCHEMA).unwrap(),
        serde_json::to_string(gate).unwrap(),
        argv,
        serde_json::to_string(rustc).unwrap(),
        serde_json::to_string(rustflags).unwrap(),
        serde_json::to_string(tree).unwrap()
    )
}

fn read_gate_cache_entry(dir: &Path, key: &str) -> Option<()> {
    let raw = std::fs::read_to_string(dir.join(format!("{key}.json"))).ok()?;
    let value: serde_json::Value = serde_json::from_str(&raw).ok()?;
    let obj = value.as_object()?;
    if obj.get("schema")?.as_str()? != SCHEMA
        || obj.get("gate").and_then(|v| v.as_str()).is_none()
        || obj.get("cell").and_then(|v| v.as_str()).is_none()
        || !obj
            .get("argv")
            .and_then(|v| v.as_array())
            .map(|args| args.iter().all(|arg| arg.as_str().is_some()))
            .unwrap_or(false)
        || obj.get("rustc").and_then(|v| v.as_str()).is_none()
        || obj.get("tree").and_then(|v| v.as_str()).is_none()
        || obj.get("created").and_then(|v| v.as_str()).is_none()
    {
        return None;
    }
    Some(())
}

fn write_gate_cache_entry(
    dir: &Path,
    key: &str,
    entry: &GateCacheEntry<'_>,
) -> std::io::Result<()> {
    std::fs::create_dir_all(dir)?;
    let tmp = dir.join(format!(
        ".{}.{}.{}.tmp",
        key,
        std::process::id(),
        TEMP_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
    ));
    let body = entry_json(entry);
    std::fs::write(&tmp, body)?;
    std::fs::rename(tmp, dir.join(format!("{key}.json")))
}

static TEMP_COUNTER: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

fn entry_json(entry: &GateCacheEntry<'_>) -> String {
    serde_json::to_string_pretty(&serde_json::json!({
        "schema": SCHEMA,
        "gate": entry.gate,
        "cell": entry.cell,
        "argv": entry.argv,
        "rustc": entry.rustc,
        "tree": entry.tree,
        "created": entry.created,
    }))
    .unwrap()
        + "\n"
}

fn created_timestamp() -> String {
    match std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH) {
        Ok(d) => format!("unix:{}", d.as_secs()),
        Err(_) => "unix:0".to_string(),
    }
}

fn hex(bytes: impl AsRef<[u8]>) -> String {
    bytes.as_ref().iter().map(|b| format!("{b:02x}")).collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    static TEMP_ID: AtomicUsize = AtomicUsize::new(0);

    fn temp_dir(name: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!(
            "cddl_codegen_gate_cache_test_{}_{}_{}",
            std::process::id(),
            name,
            TEMP_ID.fetch_add(1, Ordering::Relaxed)
        ));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    fn config(dir: PathBuf) -> CacheConfig {
        CacheConfig { enabled: true, dir }
    }

    fn write(path: &Path, bytes: &[u8]) {
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(path, bytes).unwrap();
    }

    #[test]
    fn tree_hash_is_deterministic_and_creation_order_independent() {
        let a = temp_dir("tree_a");
        write(&a.join("z.txt"), b"last");
        write(&a.join("nested/a.txt"), b"first");
        write(&a.join("target/ignored.txt"), b"ignored-1");

        let b = temp_dir("tree_b");
        write(&b.join("target/ignored.txt"), b"ignored-2");
        write(&b.join("nested/a.txt"), b"first");
        write(&b.join("z.txt"), b"last");

        assert_eq!(hash_tree(&a).unwrap(), hash_tree(&a).unwrap());
        assert_eq!(hash_tree(&a).unwrap(), hash_tree(&b).unwrap());
    }

    #[test]
    fn one_byte_change_changes_the_key() {
        let root = temp_dir("byte_change");
        write(&root.join("a.txt"), b"a");
        let argv = vec!["cargo".to_string(), "check".to_string()];
        let before = gate_cache_key("gate", &argv, &hash_tree(&root).unwrap()).0;
        write(&root.join("a.txt"), b"b");
        let after = gate_cache_key("gate", &argv, &hash_tree(&root).unwrap()).0;
        assert_ne!(before, after);
    }

    #[test]
    fn each_key_component_changes_the_key() {
        let root_a = temp_dir("key_a");
        let root_b = temp_dir("key_b");
        write(&root_a.join("a.txt"), b"a");
        write(&root_b.join("a.txt"), b"b");
        let argv = vec!["cargo".to_string(), "check".to_string()];
        let key = gate_cache_key("gate", &argv, &hash_tree(&root_a).unwrap()).0;
        assert_ne!(
            key,
            gate_cache_key("other-gate", &argv, &hash_tree(&root_a).unwrap()).0
        );
        assert_ne!(
            key,
            gate_cache_key(
                "gate",
                &["cargo".to_string(), "test".to_string()],
                &hash_tree(&root_a).unwrap()
            )
            .0
        );
        assert_ne!(
            key,
            gate_cache_key("gate", &argv, &hash_tree(&root_b).unwrap()).0
        );
    }

    #[test]
    fn corrupt_entry_file_is_a_miss() {
        let root = temp_dir("corrupt_root");
        let cache = temp_dir("corrupt_cache");
        write(&root.join("a.txt"), b"a");
        let argv = vec!["cargo".to_string(), "check".to_string()];
        let key = gate_cache_key("gate", &argv, &hash_tree(&root).unwrap()).0;
        write(&cache.join(format!("{key}.json")), b"{not json");

        let calls = AtomicUsize::new(0);
        let outcome = run_cached_with(config(cache), "gate", "cell", &root, &argv, || {
            calls.fetch_add(1, Ordering::SeqCst);
            true
        });
        assert_eq!(outcome, GateCacheOutcome::RanPass);
        assert_eq!(calls.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn disabled_cache_reads_and_writes_nothing() {
        let root = temp_dir("disabled_root");
        let cache = temp_dir("disabled_cache");
        write(&root.join("a.txt"), b"a");
        let argv = vec!["cargo".to_string(), "check".to_string()];
        let key = gate_cache_key("gate", &argv, &hash_tree(&root).unwrap()).0;
        write(
            &cache.join(format!("{key}.json")),
            entry_json(&GateCacheEntry {
                gate: "gate",
                cell: "cell",
                argv: &argv,
                rustc: rustc_version_verbose(),
                tree: &hash_tree(&root).unwrap(),
                created: "fixed".to_string(),
            })
            .as_bytes(),
        );

        let calls = AtomicUsize::new(0);
        let outcome = run_cached_with(
            CacheConfig {
                enabled: false,
                dir: cache.clone(),
            },
            "gate",
            "cell",
            &root,
            &argv,
            || {
                calls.fetch_add(1, Ordering::SeqCst);
                true
            },
        );
        assert_eq!(outcome, GateCacheOutcome::RanPass);
        assert_eq!(calls.load(Ordering::SeqCst), 1);

        let after = std::fs::read_to_string(cache.join(format!("{key}.json"))).unwrap();
        assert!(after.contains("\"created\": \"fixed\""));
    }

    #[test]
    fn lockfile_inputs_are_the_files_resolution_reads() {
        for rel in [
            "Cargo.toml",
            "rust/Cargo.toml",
            "wasm/json-gen/Cargo.toml",
            ".cargo/config.toml",
            "rust/.cargo/config",
            "rust-toolchain.toml",
            "rust/rust-toolchain",
        ] {
            assert!(is_lockfile_input(rel), "{rel} must be a resolution input");
        }
        // `Cargo.lock` is the OUTPUT of the preflight, not an input to it (probed: a hand-edited
        // pin is discarded by `cargo generate-lockfile`, which re-resolves from scratch) — hashing
        // it into the memo key would make the memo self-invalidating.
        for rel in [
            "Cargo.lock",
            "rust/Cargo.lock",
            "rust/src/lib.rs",
            "config.toml",
            "rust/src/.cargo/lib.rs",
        ] {
            assert!(
                !is_lockfile_input(rel),
                "{rel} must not be a resolution input"
            );
        }
    }

    #[test]
    fn a_path_dep_leaving_the_tree_disables_the_memo() {
        // In-tree path deps — the shapes the generator actually emits — stay memoizable.
        assert!(!manifest_path_dep_escapes(
            "wasm/Cargo.toml",
            b"cddl-lib = { path = \"../rust\", features = [\"wasm\"] }"
        ));
        assert!(!manifest_path_dep_escapes(
            "wasm/json-gen/Cargo.toml",
            b"cddl-lib = { path = \"../../rust\" }"
        ));
        assert!(!manifest_path_dep_escapes(
            "rust/Cargo.toml",
            b"cbor_event = { git = \"https://example.invalid\" }"
        ));
        // A dep resolving above the generated root is a manifest the key cannot see.
        assert!(manifest_path_dep_escapes(
            "wasm/Cargo.toml",
            b"other = { path = \"../../elsewhere\" }"
        ));
        assert!(manifest_path_dep_escapes(
            "rust/Cargo.toml",
            b"other = { path = \"/abs/elsewhere\" }"
        ));
    }

    #[test]
    fn the_memo_key_covers_manifests_and_ignores_sources() {
        let parent = temp_dir("memo_parent");
        let a = parent.join("cell_a");
        let b = parent.join("cell_b");
        for root in [&a, &b] {
            write(&root.join("rust/Cargo.toml"), b"[package]\nname = \"x\"\n");
            write(
                &root.join("wasm/Cargo.toml"),
                b"[dependencies]\nx = { path = \"../rust\" }\n",
            );
        }
        write(&a.join("rust/src/lib.rs"), b"// cell a");
        write(
            &b.join("rust/src/lib.rs"),
            b"// cell b, a different fixture entirely",
        );

        let sub = PathBuf::from("rust/Cargo.toml");
        let key_a = lockfile_memo_key(&a, &sub).unwrap();
        // THE point of the memo: two cells whose generated SOURCES differ but whose manifests are
        // identical share one dependency resolution.
        assert_eq!(key_a, lockfile_memo_key(&b, &sub).unwrap());
        // Each manifest in the tree is keyed, including a path dep's — the wasm lock is resolved
        // from `rust/Cargo.toml` too.
        assert_ne!(
            key_a,
            lockfile_memo_key(&a, &PathBuf::from("wasm/Cargo.toml")).unwrap()
        );
        write(&b.join("rust/Cargo.toml"), b"[package]\nname = \"y\"\n");
        assert_ne!(key_a, lockfile_memo_key(&b, &sub).unwrap());
        write(&b.join("rust/Cargo.toml"), b"[package]\nname = \"x\"\n");
        write(
            &b.join("wasm/Cargo.toml"),
            b"[dependencies]\nx = { path = \"../rust\" }\nz = \"1\"\n",
        );
        assert_ne!(key_a, lockfile_memo_key(&b, &sub).unwrap());
    }

    #[test]
    fn trees_under_different_parents_never_share_a_memo_entry() {
        // The ancestry above a generated tree — where an out-of-tree `.cargo/config.toml` or a
        // parent workspace manifest would sit — is not hashed; equal PARENT paths stand in for it,
        // which is exact because a gate's cells are siblings under one scratch root.
        let one = temp_dir("memo_ancestry_one").join("cell");
        let two = temp_dir("memo_ancestry_two").join("cell");
        for root in [&one, &two] {
            write(&root.join("rust/Cargo.toml"), b"[package]\nname = \"x\"\n");
        }
        let sub = PathBuf::from("rust/Cargo.toml");
        assert_ne!(
            lockfile_memo_key(&one, &sub).unwrap(),
            lockfile_memo_key(&two, &sub).unwrap()
        );
    }

    #[test]
    fn an_escaping_path_dep_refuses_a_memo_key() {
        let root = temp_dir("memo_escape");
        write(
            &root.join("wasm/Cargo.toml"),
            b"[dependencies]\nother = { path = \"../../outside\" }\n",
        );
        assert!(lockfile_memo_key(&root, &PathBuf::from("wasm/Cargo.toml")).is_none());
    }

    #[test]
    fn failing_build_writes_nothing() {
        let root = temp_dir("fail_root");
        let cache = temp_dir("fail_cache");
        write(&root.join("a.txt"), b"a");
        let argv = vec!["cargo".to_string(), "check".to_string()];
        let outcome = run_cached_with(config(cache.clone()), "gate", "cell", &root, &argv, || {
            false
        });
        assert_eq!(outcome, GateCacheOutcome::RanFail);
        assert!(std::fs::read_dir(cache).unwrap().next().is_none());
    }
}
