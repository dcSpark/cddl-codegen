//! Versioned runtime-flavor metadata exchanged by a hand-exported static runtime and a
//! `--common-import-override` consumer.
//!
//! This is deliberately a small, strict wire seam rather than a second configuration format: the
//! only presently observable baked-by-value contract is the `AnyCbor` depth guard. The record is
//! always rendered in one canonical order, and a named input rejects anything this version cannot
//! understand rather than guessing how a future runtime was configured.

use std::path::Path;

/// The root-level metadata file an `--export-static-crate` invocation always writes.
pub const FILE_NAME: &str = "cddl-codegen-runtime-flavor.toml";
const FORMAT_VERSION: i64 = 1;

/// The part of an exported runtime's flavor a hand-generated consumer must compare.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeFlavor {
    pub deserialize_depth_limit: Option<u32>,
}

impl RuntimeFlavor {
    pub fn from_depth_limit(deserialize_depth_limit: Option<u32>) -> Self {
        Self {
            deserialize_depth_limit,
        }
    }

    /// Canonical, byte-stable contents for [`FILE_NAME`].
    pub fn render(&self) -> String {
        let depth_limit = match self.deserialize_depth_limit {
            Some(value) => value.to_string(),
            None => "\"unset\"".to_owned(),
        };
        format!("format-version = {FORMAT_VERSION}\ndeserialize-depth-limit = {depth_limit}\n")
    }

    /// Parse a complete record. This seam is intentionally closed: accepting an unrecognised key
    /// could make a newer exported runtime look compatible to an older consumer.
    pub fn parse(text: &str) -> Result<Self, String> {
        let value = toml::from_str::<toml::Value>(text)
            .map_err(|error| format!("invalid TOML: {error}"))?;
        let table = value
            .as_table()
            .ok_or_else(|| "the record root must be a TOML table".to_owned())?;

        for key in table.keys() {
            if !matches!(key.as_str(), "format-version" | "deserialize-depth-limit") {
                return Err(format!("unknown key `{key}`"));
            }
        }

        let version = table
            .get("format-version")
            .ok_or_else(|| "missing required key `format-version`".to_owned())?
            .as_integer()
            .ok_or_else(|| "`format-version` must be an integer".to_owned())?;
        if version != FORMAT_VERSION {
            return Err(format!(
                "unsupported format-version {version}; this cddl-codegen accepts {FORMAT_VERSION}"
            ));
        }

        let depth_limit = table
            .get("deserialize-depth-limit")
            .ok_or_else(|| "missing required key `deserialize-depth-limit`".to_owned())?;
        let deserialize_depth_limit = match depth_limit {
            toml::Value::String(value) if value == "unset" => None,
            toml::Value::Integer(value) => Some(u32::try_from(*value).map_err(|_| {
                "`deserialize-depth-limit` must be an unsigned 32-bit integer or \"unset\""
                    .to_owned()
            })?),
            _ => {
                return Err(
                    "`deserialize-depth-limit` must be an unsigned 32-bit integer or \"unset\""
                        .to_owned(),
                );
            }
        };
        Ok(Self {
            deserialize_depth_limit,
        })
    }

    /// Read an explicitly named cross-crate input, retaining both the flag and path in every
    /// failure so a caller can distinguish a bad record from an unrelated generation error.
    pub fn read(path: &Path) -> Result<Self, String> {
        let text = std::fs::read_to_string(path).map_err(|error| {
            format!(
                "--common-import-flavor={} could not read the runtime-flavor record: {error}",
                path.display()
            )
        })?;
        Self::parse(&text).map_err(|error| {
            format!(
                "--common-import-flavor={} is not a valid runtime-flavor record: {error}",
                path.display()
            )
        })
    }

    pub fn display_depth_limit(&self) -> String {
        self.deserialize_depth_limit
            .map_or_else(|| "unset".to_owned(), |value| value.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::{FILE_NAME, RuntimeFlavor};

    #[test]
    fn runtime_flavor_record_round_trips_with_stable_set_and_unset_bytes() {
        let set = RuntimeFlavor::from_depth_limit(Some(64));
        assert_eq!(
            set.render(),
            "format-version = 1\ndeserialize-depth-limit = 64\n"
        );
        assert_eq!(RuntimeFlavor::parse(&set.render()).unwrap(), set);

        let unset = RuntimeFlavor::from_depth_limit(None);
        assert_eq!(
            unset.render(),
            "format-version = 1\ndeserialize-depth-limit = \"unset\"\n"
        );
        assert_eq!(RuntimeFlavor::parse(&unset.render()).unwrap(), unset);
        assert_eq!(FILE_NAME, "cddl-codegen-runtime-flavor.toml");
    }

    #[test]
    fn runtime_flavor_record_rejects_incomplete_unknown_version_bad_value_and_unknown_key() {
        for (text, expected) in [
            ("not valid = [\n", "invalid TOML"),
            (
                "deserialize-depth-limit = 64\n",
                "missing required key `format-version`",
            ),
            (
                "format-version = 1\n",
                "missing required key `deserialize-depth-limit`",
            ),
            (
                "format-version = 2\ndeserialize-depth-limit = 64\n",
                "unsupported format-version 2",
            ),
            (
                "format-version = 1\ndeserialize-depth-limit = -1\n",
                "unsigned 32-bit integer",
            ),
            (
                "format-version = 1\ndeserialize-depth-limit = 4294967296\n",
                "unsigned 32-bit integer",
            ),
            (
                "format-version = 1\ndeserialize-depth-limit = true\n",
                "unsigned 32-bit integer",
            ),
            (
                "format-version = 1\ndeserialize-depth-limit = 64\nextra = 1\n",
                "unknown key `extra`",
            ),
        ] {
            let error = RuntimeFlavor::parse(text).expect_err("record must be rejected");
            assert!(error.contains(expected), "{error}");
        }
    }

    #[test]
    fn runtime_flavor_record_read_names_flag_and_missing_path() {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_runtime_flavor_missing_{}",
            std::process::id()
        ));
        let error = RuntimeFlavor::read(&path).expect_err("missing record must fail");
        assert!(error.contains("--common-import-flavor="), "{error}");
        assert!(error.contains(&path.display().to_string()), "{error}");
    }
}
