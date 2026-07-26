use sha2::{Digest, Sha256};
use std::path::{Path, PathBuf};
use std::sync::OnceLock;

const SCHEMA: &str = "gate-cache-v1";
const RUSTFLAGS_AS_NESTED_CARGO_SEES_IT: &str = "";

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
        let lockfile = super::integration_tests::tool_cmd("cargo")
            .arg("generate-lockfile")
            .arg("--manifest-path")
            .arg(generated_root.join(manifest_subpath))
            .output();
        if !lockfile.map(|o| o.status.success()).unwrap_or(false) {
            return if build() {
                GateCacheOutcome::RanPass
            } else {
                GateCacheOutcome::RanFail
            };
        }
    }

    run_cached_with(config, gate, cell, generated_root, argv_for_key, build)
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
