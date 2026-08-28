//! One prerequisite policy for every component guest build.
//!
//! Local runs legitimately occur under toolchains that do not ship `wasm32-wasip2`; they must say
//! that the component-execution portion was not run, not mistake a provisioner error for emitter
//! evidence. The `full` tier exports [`REQUIRED_ENV`], turning that same outcome into a hard failure.

pub(crate) const REQUIRED_ENV: &str = "CDDL_COMPONENT_TARGET_REQUIRED";

/// Cargo/rustc spell the absent target differently across toolchain distributions. Keep the
/// classifier deliberately narrow: an unrelated `core` error is still a test failure.
pub(crate) fn is_missing_target(stderr: &str) -> bool {
    stderr.contains("can't find crate for `core`") || stderr.contains("target may not be installed")
}

fn required_from(raw: Option<&str>) -> bool {
    matches!(raw, Some(v) if v == "1" || v.eq_ignore_ascii_case("true"))
}

pub(crate) fn required() -> bool {
    required_from(std::env::var(REQUIRED_ENV).ok().as_deref())
}

/// Avoid a cached PASS disguising a now-unavailable target: cached component cells do not invoke
/// cargo, so their stderr classifier below would not otherwise run. A failed probe is treated as
/// unavailable only when rustc successfully names a target-lib directory that is absent; anything
/// else is left to the real cargo invocation and its narrow stderr classifier.
pub(crate) fn installed() -> bool {
    let output = std::process::Command::new("rustc")
        .args(["--target", "wasm32-wasip2", "--print", "target-libdir"])
        .output();
    let Ok(output) = output else {
        return true;
    };
    if !output.status.success() {
        return true;
    }
    std::path::Path::new(String::from_utf8_lossy(&output.stdout).trim()).is_dir()
}

/// Whether the actual `rustc` command is one rustup manages, not merely whether an unrelated
/// rustup installation happens to be on PATH. A rustup proxy and a direct toolchain compiler both
/// report their selected toolchain's sysroot; `rustup which rustc` must live under that same root.
/// This avoids reasoning from the PATH entry itself, which may be a rustup proxy symlink.
fn rustup_manages_active_rustc(
    active_sysroot: &std::path::Path,
    rustup_which_rustc: Option<&std::path::Path>,
) -> bool {
    rustup_which_rustc.is_some_and(|rustc| rustc.starts_with(active_sysroot))
}

fn canonical_or_self(path: std::path::PathBuf) -> std::path::PathBuf {
    path.canonicalize().unwrap_or(path)
}

fn active_rustc_is_rustup_managed() -> bool {
    let active_sysroot = std::process::Command::new("rustc")
        .args(["--print", "sysroot"])
        .output()
        .ok()
        .filter(|out| out.status.success())
        .map(|out| {
            canonical_or_self(std::path::PathBuf::from(
                String::from_utf8_lossy(&out.stdout).trim(),
            ))
        });
    let Some(active_sysroot) = active_sysroot else {
        return false;
    };
    let rustup_which_rustc = std::process::Command::new("rustup")
        .args(["which", "rustc"])
        .output()
        .ok()
        .filter(|out| out.status.success())
        .and_then(|out| {
            let path = std::path::PathBuf::from(String::from_utf8_lossy(&out.stdout).trim());
            path.canonicalize().ok().or(Some(path))
        });
    rustup_manages_active_rustc(&active_sysroot, rustup_which_rustc.as_deref())
}

fn diagnostic_for(rustup_available: bool) -> String {
    if rustup_available {
        "the wasm32-wasip2 target is not installed under the active Rust toolchain — install it with \
         `rustup target add wasm32-wasip2`".to_owned()
    } else {
        "the active Rust compiler is not rustup-managed and does not provide wasm32-wasip2 — \
         provision the external/Nix toolchain with that target"
            .to_owned()
    }
}

/// Report the shared absence outcome. Callers return from their test after the loud local skip; at
/// full, this panics so a shipped-face guarantee can never be silently omitted.
pub(crate) fn missing_target(gate: &str) {
    let diagnostic = diagnostic_for(active_rustc_is_rustup_managed());
    assert!(
        !required(),
        "{gate}: {diagnostic}\n\nThis is a hard failure because {REQUIRED_ENV}=1 — the `full` \
         tier ships the component face, so its wasm32-wasip2 build may not be skipped."
    );
    println!("{gate}: SKIPPED — {diagnostic}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn recognizes_only_the_two_known_missing_target_signatures() {
        assert!(is_missing_target(
            "error[E0463]: can't find crate for `core`"
        ));
        assert!(is_missing_target(
            "the wasm32-wasip2 target may not be installed"
        ));
        assert!(!is_missing_target(
            "error[E0425]: cannot find value `core` in this scope"
        ));
    }

    #[test]
    fn required_env_accepts_the_documented_truthy_values_only() {
        assert!(required_from(Some("1")));
        assert!(required_from(Some("TRUE")));
        assert!(!required_from(Some("yes")));
        assert!(!required_from(None));
    }

    #[test]
    fn diagnostic_matches_the_available_provisioner() {
        assert!(diagnostic_for(true).contains("rustup target add wasm32-wasip2"));
        let external = diagnostic_for(false);
        assert!(external.contains("external/Nix toolchain"));
        assert!(external.contains("not rustup-managed"));
    }

    #[test]
    fn rustup_remedy_requires_rustup_to_own_the_active_compiler() {
        let managed_sysroot = std::path::Path::new("/home/test/.rustup/toolchains/pinned");
        let nix_sysroot = std::path::Path::new("/nix/store/rust");
        let rustup_which = std::path::Path::new("/home/test/.rustup/toolchains/pinned/bin/rustc");
        assert!(rustup_manages_active_rustc(
            managed_sysroot,
            Some(rustup_which)
        ));
        assert!(!rustup_manages_active_rustc(
            nix_sysroot,
            Some(rustup_which)
        ));
        assert!(!rustup_manages_active_rustc(managed_sysroot, None));
    }
}
