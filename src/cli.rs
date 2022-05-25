use clap::{Parser};
use once_cell::sync::Lazy;
// TODO: make non-annotation generate different DeserializeError that is simpler
//       and works with From<cbor_event:Error> only

#[derive(Debug, Parser)]
#[clap()]
pub struct Cli {
    /// Input .cddl file to generate from.
    #[clap(short, long, parse(from_os_str), value_name = "INPUT_FILE")]
    pub input: std::path::PathBuf,

    /// Output directory for the generated code.
    #[clap(short, long, parse(from_os_str), value_name = "OUTPUT_DIR")]
    pub output: std::path::PathBuf,

    /// Include additional information about where deserialization errors are encountered. This will slightly increase code size.
    #[clap(long, parse(try_from_str), default_value_t = true)]
    pub annotate_fields: bool,

    /// Generate to_bytes() / from_bytes() methods on all types
    #[clap(long, parse(try_from_str), default_value_t = false)]
    pub to_from_bytes_methods: bool,

    /// Use our own extended prelude with types like i32, u64, etc for more control than 'uint', etc
    #[clap(long, parse(try_from_str), default_value_t = true)]
    pub extended_prelude: bool,

    /// Generate byte string definitions as new rust types
    #[clap(long, parse(try_from_str), default_value_t = true)]
    pub binary_wrappers: bool,

    /// Preserves CBOR encoding upon deserialization e.g. definite vs indefinite, map ordering
    #[clap(long, parse(try_from_str), default_value_t = false)]
    pub preserve_encodings: bool,

    /// Allows serialization to canonical CBOR. if preserve-encodings is enabled, this will be as a toggle on serialization functions
    #[clap(long, parse(try_from_str), default_value_t = false)]
    pub canonical_form: bool,

    #[clap(long, parse(try_from_str), default_value_t = true)]
    pub wasm: bool,
}

pub static CLI_ARGS: Lazy<Cli> = Lazy::new(|| Cli::parse());