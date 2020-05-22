Example + unit tests for all supported serialization + deserialization features are inside of `test.cddl`.

Move `serialization_unit_tests.rs` to the generated `/export/` directory generated from running cddl-codegen on `test.cddl`, or alternatively copy the code into the generated `export/serialization.rs` to function as unit tests.

To run unit tests run `cargo test` from the `/export/` directory.
