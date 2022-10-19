use clap::{Parser};
use once_cell::sync::Lazy;
// TODO: make non-annotation generate different DeserializeError that is simpler
//       and works with From<cbor_event:Error> only

#[derive(Debug, Parser)]
#[clap()]
pub struct Cli {
    /// Input .cddl file to generate from. If this is a directory then it will read all *.cddl files and generate one output for each.
    #[clap(short, long, parse(from_os_str), value_name = "INPUT_FILE/INPUT_DIR")]
    pub input: std::path::PathBuf,

    /// Output directory for the generated code.
    #[clap(short, long, parse(from_os_str), value_name = "OUTPUT_DIR")]
    pub output: std::path::PathBuf,

    /// Include additional information about where deserialization errors are encountered. This will slightly increase code size.
    #[clap(long, parse(try_from_str), default_value_t = true)]
    pub annotate_fields: bool,

    /// Generate to_bytes() / from_bytes() methods on all types
    #[clap(long, parse(try_from_str), default_value_t = true)]
    pub to_from_bytes_methods: bool,

    /// Generate byte string definitions as new rust types (TODO: look into this or remove it)
    #[clap(long, parse(try_from_str), default_value_t = false)]
    pub binary_wrappers: bool,

    /// Preserves CBOR encoding upon deserialization e.g. definite vs indefinite, map ordering
    #[clap(long, parse(try_from_str), default_value_t = false)]
    pub preserve_encodings: bool,

    /// Allows serialization to canonical CBOR. if preserve-encodings is enabled, this will be as a toggle on serialization functions
    #[clap(long, parse(try_from_str), default_value_t = false)]
    pub canonical_form: bool,

    /// Generates a wasm_bindgen crate for wasm bindings
    #[clap(long, parse(try_from_str), default_value_t = true)]
    pub wasm: bool,

    /// Derives serde::Serialize/serde::Deserialize for types to allow to/from JSON
    #[clap(long, parse(try_from_str), default_value_t = false)]
    pub json_serde_derives: bool,

    /// Tags types with sonSchema derives and generates a crate to export them
    #[clap(long, parse(try_from_str), default_value_t = false)]
    pub json_schema_export: bool,

    /// Generates a npm package.json along with build scripts
    #[clap(long, parse(try_from_str), default_value_t = false)]
    pub package_json: bool,
}

pub static CLI_ARGS: Lazy<Cli> = Lazy::new(|| Cli::parse());