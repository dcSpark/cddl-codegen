//! A stand-in for "some OTHER crate in the workspace turns `serde_json/arbitrary_precision` on".
//!
//! It has no API on purpose: the only thing a dependent needs from it is the cargo FEATURE
//! unification its manifest triggers. `tests/json-arbitrary-precision` depends on it so the
//! generated crate's own `serde_json` — whose manifest key the tool owns and which therefore cannot
//! carry a second, duplicate `serde_json` entry — is built with the feature on, exactly as it is in
//! the consumer workspace this regression came from.
//!
//! The dependent asserts the feature really did arrive rather than trusting it
//! (`arbitrary_precision_is_live` in that fixture's `tests.rs`): a silently-off feature would make
//! every other assertion there pass vacuously.
