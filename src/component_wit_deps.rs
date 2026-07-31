//! `--component-extern-wit`: reading a dependency's committed WIT package, and materializing it
//! into this crate's own WIT tree so the consumer's `use` lines resolve and its
//! `wit_bindgen::generate!` invocation can name the interfaces it imports.
//!
//! # Determinism class
//!
//! The files read here are an **explicit cross-crate INPUT** — the same class as `--extern-import`
//! reading a dependency's `extern-interface/<dep>/**` export, and NOT a read of this run's own prior
//! output. A path handed on the command line names another crate's committed artifact; nothing in
//! this module opens anything under this run's `--output`. "Same inputs -> same bytes" is unaffected,
//! and the file-class comment stamped onto every materialized copy says so in those terms, because
//! the copy lands inside the output tree where the distinction is easiest to lose.
//!
//! # Copied, never re-derived
//!
//! A consumer cannot reconstruct a dependency's WIT from the dependency's cddl: the package id comes
//! from the dep's own `--wit-package` (or its `--lib-name`), and the `// unexported:` rows carry
//! reasons only the dep's projection knows. So the bytes are copied verbatim and everything this
//! module needs — the package id, the interface names, the type names, the exclusion reasons — is
//! READ OUT of the copy. That is also what keeps the `with:` keys byte-correct: `wit_bindgen`
//! matches them against the dep's WIT, not against anything we could re-derive.
//!
//! # Why a hand-written reader
//!
//! `wit-parser` is a DEV-dependency (the WIT-validity gate's pinned oracle) and deliberately not a
//! dependency of the tool itself. The input here is narrow — a WIT package this same tool emitted on
//! the dependency's run — so the reader below understands exactly that shape and refuses anything
//! else with an error naming the file and the line, rather than guessing.

use std::collections::{BTreeMap, BTreeSet};

use crate::cli::Cli;

/// The comment block prepended to every materialized copy. Its job is to state the FILE CLASS at the
/// one place a reader meets the file — inside the output tree, where a copied input is easiest to
/// mistake for generated output or for a prior-run read.
const FILE_CLASS_COMMENT: &str = "\
// FILE CLASS: a verbatim copy of a DEPENDENCY's committed WIT package, materialized here because
// `use <dep-package>/<interface>` resolves only against a package present in this crate's own WIT
// source tree. It is an explicit cross-crate INPUT — the same class as `--extern-import` reading a
// dependency's `extern-interface/<dep>/**` export — and NOT a read of this run's prior output.
//
// Tool-owned and delete-and-recreated on every run: edit the DEPENDENCY's spec and regenerate it,
// never this file. The bytes below are the dependency's own, unchanged.
";

/// One dependency's WIT package, as read out of the copied files.
#[derive(Clone, Debug)]
pub(crate) struct DepWitPackage {
    /// The extern-deps directory name this dependency is declared under — the left side of both
    /// `--component-extern-wit` and `--extern-import`, and the leading component of the non-exported
    /// `ModuleScope` its types land in.
    pub dep: String,
    /// The flag's path value, for error messages that have to name what to fix.
    pub source: String,
    /// The package id verbatim, exactly as the dependency wrote it (`cddl:chain@0.1.0`).
    pub package_id: String,
    /// The package id split at `@`: the base (`cddl:chain`) and the optional version (`0.1.0`). A
    /// `use` path interleaves them (`cddl:chain/types@0.1.0`), which is why they are stored apart.
    package_base: String,
    version: Option<String>,
    /// Interface name -> the WIT type names it declares. Read out of the file so a type's home
    /// interface is never guessed from the consumer's own scope tree.
    pub interfaces: BTreeMap<String, BTreeSet<String>>,
    /// The dependency's `// unexported: <rust ident> — <reason>` rows, keyed by the RUST ident they
    /// name (which is what a consumer holds) and carrying the reason VERBATIM.
    pub unexported: BTreeMap<String, String>,
    /// The copied files, keyed by file name. Written out under
    /// `component/wit/deps/<dep>/<file name>`.
    files: BTreeMap<String, String>,
}

impl DepWitPackage {
    /// The `use` target for one of this package's interfaces: `cddl:chain/types@0.1.0`. Also the
    /// `with:` key the guest crate's `generate!` invocation must carry for that interface — one
    /// spelling, so the two can never disagree.
    pub(crate) fn use_path(&self, interface: &str) -> String {
        match &self.version {
            Some(version) => format!("{}/{interface}@{version}", self.package_base),
            None => format!("{}/{interface}", self.package_base),
        }
    }

    /// The interfaces declaring a WIT type name, in render order. Empty = this package does not
    /// declare it under that name at all.
    pub(crate) fn interfaces_declaring(&self, wit_name: &str) -> Vec<&str> {
        self.interfaces
            .iter()
            .filter(|(_, names)| names.contains(wit_name))
            .map(|(name, _)| name.as_str())
            .collect()
    }
}

/// Every dependency's WIT package, keyed by dep name. Empty off `--component-extern-wit`, which is
/// the state every existing spec generates in — a dep with no flag keeps the projection it has
/// today.
pub(crate) type DepWitPackages = BTreeMap<String, DepWitPackage>;

/// Read every `--component-extern-wit` mapping. A hard error names the flag value and what to fix.
pub(crate) fn load(cli: &Cli) -> Result<DepWitPackages, String> {
    let mut out = BTreeMap::new();
    for (dep, path) in cli.component_extern_wit_paths() {
        out.insert(dep.clone(), load_one(&dep, &path)?);
    }
    Ok(out)
}

fn load_one(dep: &str, path: &str) -> Result<DepWitPackage, String> {
    let root = std::path::PathBuf::from(path);
    if !root.is_dir() {
        return Err(format!(
            "--component-extern-wit {dep}={path} — the path is not a directory. Point it at the \
             dependency's committed `component/wit/` directory (the one holding its `world.wit`), \
             which its own `--component=true` run writes."
        ));
    }
    // The dep's OWN `deps/` subtree would be this consumer's transitive dep packages. Flattening or
    // nesting them are two different layouts and neither has been probed end to end, so the shape is
    // refused rather than emitted on a guess — a WIT package that resolves at generation time and
    // fails inside `wit_bindgen::generate!` is the worst place to find out.
    if root.join("deps").exists() {
        return Err(format!(
            "--component-extern-wit {dep}={path} — the dependency's WIT carries its own `deps/` \
             directory, i.e. it imports a further package. Transitive WIT dependencies are not yet \
             supported by the component face's cross-crate seam: only a dependency whose WIT \
             package stands alone can be imported today. Generate `{dep}` without \
             `--component-extern-wit` of its own, or drop `--component-extern-wit {dep}=…` here \
             (its types are then recorded as `// unexported:` in this crate's WIT)."
        ));
    }
    let mut wit_files = Vec::new();
    for entry in std::fs::read_dir(&root)
        .map_err(|e| format!("--component-extern-wit {dep}={path} — reading the directory: {e}"))?
    {
        let entry =
            entry.map_err(|e| format!("--component-extern-wit {dep}={path} — reading it: {e}"))?;
        let file = entry.path();
        if file.is_file() && file.extension().is_some_and(|e| e == "wit") {
            wit_files.push(file);
        }
    }
    wit_files.sort();
    if wit_files.is_empty() {
        return Err(format!(
            "--component-extern-wit {dep}={path} — no `.wit` files found in the directory. Point it \
             at the dependency's committed `component/wit/` directory, and regenerate the \
             dependency first if it has never been generated with `--component=true`."
        ));
    }

    let mut files = BTreeMap::new();
    for file in &wit_files {
        let name = file
            .file_name()
            .expect("a path that read_dir yielded as a file has a file name")
            .to_string_lossy()
            .into_owned();
        let content = std::fs::read_to_string(file).map_err(|e| {
            format!(
                "--component-extern-wit {dep}={path} — reading {}: {e}",
                file.display()
            )
        })?;
        files.insert(name, content);
    }

    let mut package_id: Option<String> = None;
    let mut interfaces: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    let mut unexported: BTreeMap<String, String> = BTreeMap::new();
    for (name, content) in &files {
        let where_ = format!("{path}/{name}");
        let parsed = parse_wit(content, &where_, dep)?;
        if let Some(id) = parsed.package_id {
            // One WIT directory is one package, so two package declarations that disagree are a
            // directory that cannot resolve — reported here rather than left to the consumer's build.
            if let Some(existing) = &package_id
                && *existing != id
            {
                return Err(format!(
                    "--component-extern-wit {dep}={path} — the directory declares two different \
                     packages ({existing} and {id}). One WIT directory is one package; point the \
                     flag at a single dependency's `component/wit/`."
                ));
            }
            package_id = Some(id);
        }
        for (iface, names) in parsed.interfaces {
            interfaces.entry(iface).or_default().extend(names);
        }
        unexported.extend(parsed.unexported);
    }
    let package_id = package_id.ok_or_else(|| {
        format!(
            "--component-extern-wit {dep}={path} — no `package …;` declaration in any `.wit` file \
             under the directory. Point it at the dependency's committed `component/wit/` \
             directory, which its own `--component=true` run writes."
        )
    })?;
    let (package_base, version) = match package_id.split_once('@') {
        Some((base, version)) => (base.to_owned(), Some(version.to_owned())),
        None => (package_id.clone(), None),
    };

    Ok(DepWitPackage {
        dep: dep.to_owned(),
        source: path.to_owned(),
        package_id,
        package_base,
        version,
        interfaces,
        unexported,
        files,
    })
}

/// What one `.wit` file contributes.
struct ParsedWit {
    package_id: Option<String>,
    interfaces: BTreeMap<String, BTreeSet<String>>,
    unexported: BTreeMap<String, String>,
}

/// The `// unexported: <ident> — <reason>` row's prefix, as `wit::render` writes it. A shared
/// spelling would be nicer, but the two sides are a WRITER of this crate's own WIT and a READER of
/// another crate's — the seam is the file, and pinning both ends against one constant would hide the
/// fact that a dependency generated by an OLDER cddl-codegen is what this reader actually meets.
const UNEXPORTED_PREFIX: &str = "// unexported:";

/// Read one `.wit` file's package id, interfaces (with the type names each declares) and exclusion
/// rows.
///
/// Line-oriented on purpose. The input is a WIT package this tool emitted, whose every declaration
/// starts a line; a shape this reader does not recognize is an error naming the file rather than a
/// silent omission, because an omission here becomes a dangling `use` in the consumer's WIT.
fn parse_wit(content: &str, where_: &str, dep: &str) -> Result<ParsedWit, String> {
    let mut package_id = None;
    let mut interfaces: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    let mut unexported = BTreeMap::new();
    // `None` = at file level; `Some((name, depth))` = inside that interface's braces. `world` blocks
    // are entered as `None`-named so their braces balance without contributing types.
    let mut block: Option<(Option<String>, usize)> = None;
    for (n, raw) in content.lines().enumerate() {
        let line = raw.trim();
        let at = || format!("{where_}:{}", n + 1);
        if let Some(rest) = line.strip_prefix(UNEXPORTED_PREFIX) {
            // `<ident> — <reason>`, with an em dash exactly as `wit::render` writes it. A row this
            // reader cannot split carries no usable reason, and the whole point of reading these is
            // to quote the reason verbatim — so it is skipped rather than half-recorded.
            if let Some((ident, reason)) = rest.split_once('—') {
                unexported.insert(ident.trim().to_owned(), reason.trim().to_owned());
            }
            continue;
        }
        if line.is_empty() || line.starts_with("//") {
            continue;
        }
        if let Some(rest) = line.strip_prefix("package ") {
            if block.is_some() {
                return Err(format!(
                    "--component-extern-wit {dep}: {} declares a package inside a block, which is \
                     not a shape this reader understands. It expects the WIT a cddl-codegen \
                     `--component=true` run emits.",
                    at()
                ));
            }
            package_id = Some(rest.trim().trim_end_matches(';').trim().to_owned());
            continue;
        }
        if block.is_none() {
            if let Some(rest) = line.strip_prefix("interface ") {
                let name = rest.trim().trim_end_matches('{').trim();
                block = Some((Some(unescape(name).to_owned()), 1));
                interfaces.entry(unescape(name).to_owned()).or_default();
                continue;
            }
            if line.starts_with("world ") {
                block = Some((None, 1));
                continue;
            }
            return Err(format!(
                "--component-extern-wit {dep}: {} is not a declaration this reader understands \
                 ({line:?}). It expects the WIT a cddl-codegen `--component=true` run emits: a \
                 `package` line, `interface` blocks and `world` blocks.",
                at()
            ));
        }
        let (name, depth) = block.as_mut().expect("just checked it is Some");
        // Type declarations live at the interface's own brace depth; anything deeper is a resource's
        // members, which are not part of the interface's type namespace.
        if *depth == 1
            && let Some(iface) = name.clone()
            && let Some(declared) = declared_type_name(line)
        {
            interfaces
                .entry(iface)
                .or_default()
                .insert(declared.to_owned());
        }
        *depth += line.matches('{').count();
        *depth -= line.matches('}').count().min(*depth);
        if *depth == 0 {
            block = None;
        }
    }
    Ok(ParsedWit {
        package_id,
        interfaces,
        unexported,
    })
}

/// The type name a line declares inside an interface, or `None` for anything that is not a type
/// declaration (a free function, a `use`, a stray brace).
///
/// The keyword set is WIT's whole type-declaration vocabulary rather than the three this tool
/// currently emits: reading a dependency generated by a DIFFERENT cddl-codegen version is the normal
/// case, and a type this reader failed to see becomes a "the dependency does not export it" error
/// against a type the dependency plainly does.
fn declared_type_name(line: &str) -> Option<&str> {
    const KEYWORDS: &[&str] = &[
        "resource ",
        "record ",
        "variant ",
        "enum ",
        "flags ",
        "type ",
    ];
    let rest = KEYWORDS
        .iter()
        .find_map(|kw| line.strip_prefix(kw))?
        .trim_start();
    let name = rest
        .split(|c: char| c.is_whitespace() || c == '{' || c == '=' || c == ';')
        .next()?
        .trim();
    (!name.is_empty()).then(|| unescape(name))
}

/// Strip WIT's `%` keyword escape. The `%` is syntax, not part of the name — the same rule
/// `wit::wit_escape` applies from the writing side.
fn unescape(name: &str) -> &str {
    name.strip_prefix('%').unwrap_or(name)
}

/// The materialized copies, keyed by path relative to `<output>`
/// (`component/wit/deps/<dep>/<file>`), for the export write loop.
pub(crate) fn materialized_files(packages: &DepWitPackages) -> BTreeMap<String, String> {
    let mut out = BTreeMap::new();
    for package in packages.values() {
        for (name, content) in &package.files {
            out.insert(
                format!(
                    "{}/{}/{name}",
                    crate::generation::layout::COMPONENT_WIT_DEPS_DIR,
                    package.dep
                ),
                format!("{FILE_CLASS_COMMENT}\n{content}"),
            );
        }
    }
    out
}
