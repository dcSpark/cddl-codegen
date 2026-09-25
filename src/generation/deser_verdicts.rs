//! The deserializability verdict: which idents get no `Deserialize` impl, decided for the whole IR
//! before emission (`seed_no_deserialize_verdicts`) and consulted by every face
//! (`deserialize_generated`).

use super::*;

impl GenerationScope {
    pub(super) fn deserialize_generated(&self, name: &RustIdent) -> bool {
        !self.no_deser_reasons.contains_key(name)
    }

    /// Every ident the rust face declined to give a `Deserialize` impl, as a set the WIT projection
    /// can consult. A verdict, not a rule: it is only complete once the rust face's walk has run.
    pub(super) fn no_deserialize_idents(&self) -> std::collections::BTreeSet<RustIdent> {
        self.no_deser_reasons.keys().cloned().collect()
    }

    pub(super) fn deserialize_generated_for_type(&self, field_type: &ConceptualRustType) -> bool {
        match field_type {
            ConceptualRustType::Fixed(_) => true,
            ConceptualRustType::Primitive(_) => true,
            // `AnyCbor` always has a hand-written `Deserialize` impl in the static runtime.
            ConceptualRustType::Any => true,
            // No `types.is_enum(ident) ||` escape hatch: an enum is exactly as deserializable as
            // its arms, and the verdict for every ident is seeded before emission
            // (`seed_no_deserialize_verdicts`), so consulting it here is order-independent.
            ConceptualRustType::Rust(ident) => self.deserialize_generated(ident),
            ConceptualRustType::Array(ty) => {
                self.deserialize_generated_for_type(&ty.conceptual_type)
            }
            ConceptualRustType::Map(k, v) => {
                self.deserialize_generated_for_type(&k.conceptual_type)
                    && self.deserialize_generated_for_type(&v.conceptual_type)
            }
            ConceptualRustType::Optional(ty) => {
                self.deserialize_generated_for_type(&ty.conceptual_type)
            }
            ConceptualRustType::Alias(_ident, ty) => self.deserialize_generated_for_type(ty),
        }
    }

    /// Idempotent per (ident, reason): the seeding pass below iterates to a fixpoint, so it revisits
    /// every struct on every round and re-derives the refusals it already knows. One line per
    /// DISTINCT cause is also what the warning wants — a cause is identified by its text (it names
    /// the field/variant), so a repeat is a repeat.
    pub(super) fn dont_generate_deserialize(&mut self, name: &RustIdent, reason: String) {
        let reasons = self.no_deser_reasons.entry(name.clone()).or_default();
        if !reasons.contains(&reason) {
            reasons.push(reason);
        }
    }

    /// Decide, for the WHOLE finalized IR and BEFORE a single line is emitted, which idents get no
    /// `Deserialize`. Every live refusal is recorded here; the emitters only consult the verdict
    /// (`deserialize_generated`).
    ///
    /// Why a pre-pass and not a check each emitter makes as it goes: the emission walk visits
    /// `rust_structs()` in IDENT order (a `BTreeMap`), which bears no relation to reference order.
    /// `ch = foo / tstr` emits the enum `Ch` before the arm `Foo`; `gc = [... // tstr]` emits `Gc`
    /// before its arm `Gc0`; `aaa = [? f0: uint, * uint]` + `zzz = aaa / tstr` happens to emit
    /// the arm first. A containing type asking "does this arm have a deserialize?" mid-walk would
    /// therefore get an answer that depends on the two idents' alphabetical order — a correctness
    /// property must not. Seeding the whole verdict up front cannot be order-sensitive: every
    /// refusal rule below is a pure function of the IR.
    ///
    /// Iterated to a fixpoint because refusals PROPAGATE: a record whose field type has no
    /// deserialize has none either, an enum whose arm has none has none either, and an enum over
    /// such an enum has none either. Monotone (reasons are only ever added), so it terminates.
    pub(super) fn seed_no_deserialize_verdicts(&mut self, types: &IntermediateTypes, cli: &Cli) {
        let total_reasons =
            |scope: &Self| -> usize { scope.no_deser_reasons.values().map(Vec::len).sum() };
        loop {
            let before = total_reasons(self);
            for (ident, rust_struct) in types.rust_structs() {
                match rust_struct.variant() {
                    RustStructType::Record(record) => {
                        // A complete custom pair owns the entire item. Its reader neither calls a
                        // field decoder nor observes this record's structural array/map shape, so
                        // those generated-code refusals cannot decide the pair's public decoder
                        // surface (which wasm, WIT, extern checks, and emitted tests all share).
                        if rust_struct.config().custom_serialize.is_none()
                            || rust_struct.config().custom_deserialize.is_none()
                        {
                            for field in &record.fields {
                                if !self.deserialize_generated_for_type(
                                    &field.rust_type.conceptual_type,
                                ) {
                                    self.dont_generate_deserialize(
                                        ident,
                                        format!(
                                            "field {}: {} couldn't generate deserialize",
                                            field.name,
                                            field.rust_type.for_rust_member(types, false, cli)
                                        ),
                                    );
                                }
                            }
                            for reason in Self::record_shape_refusals(types, record, cli) {
                                self.dont_generate_deserialize(ident, reason);
                            }
                        }
                    }
                    RustStructType::TypeChoice { variants } => {
                        self.seed_enum_variant_refusals(types, cli, ident, variants, None);
                    }
                    RustStructType::GroupChoice { variants, rep } => {
                        self.seed_enum_variant_refusals(types, cli, ident, variants, Some(*rep));
                    }
                    // structural containers over another type: the rust face renders them as
                    // newtypes / transparent aliases whose wire path is the wrapped type's, so a
                    // wrapped type with no deserialize leaves them with none either.
                    RustStructType::Wrapper { wrapped, .. } => {
                        // A complete type-level pair owns the whole item. Its reader returns this
                        // nominal wrapper directly, so its structural map/array inner need not (and
                        // for a deliberately non-container custom wire, must not) be deserializable.
                        if !(self.deserialize_generated_for_type(&wrapped.conceptual_type)
                            || (rust_struct.config().custom_serialize.is_some()
                                && rust_struct.config().custom_deserialize.is_some()))
                        {
                            self.dont_generate_deserialize(
                                ident,
                                format!(
                                    "wrapped type {} couldn't generate deserialize",
                                    wrapped.for_rust_member(types, false, cli)
                                ),
                            );
                        }
                    }
                    RustStructType::Array { element_type, .. } => {
                        if !self.deserialize_generated_for_type(&element_type.conceptual_type) {
                            self.dont_generate_deserialize(
                                ident,
                                format!(
                                    "element type {} couldn't generate deserialize",
                                    element_type.for_rust_member(types, false, cli)
                                ),
                            );
                        }
                    }
                    RustStructType::Table { domain, range, .. } => {
                        for (label, ty) in [("key", domain), ("value", range)] {
                            if !self.deserialize_generated_for_type(&ty.conceptual_type) {
                                self.dont_generate_deserialize(
                                    ident,
                                    format!(
                                        "{label} type {} couldn't generate deserialize",
                                        ty.for_rust_member(types, false, cli)
                                    ),
                                );
                            }
                        }
                    }
                    // leaves: a c-style enum serializes inline in its container, and extern /
                    // raw-bytes types name a hand-written impl this tool never declines to emit.
                    RustStructType::CStyleEnum { .. }
                    | RustStructType::Extern
                    | RustStructType::RawBytesType => {}
                }
            }
            if total_reasons(self) == before {
                break;
            }
        }
    }

    /// The refusals a record earns from its own SHAPE (not from its field types), by representation.
    fn record_shape_refusals(
        types: &IntermediateTypes,
        record: &RustRecord,
        cli: &Cli,
    ) -> Vec<String> {
        match record.rep {
            Representation::Array => super::records::array_record_deser_refusals(types, record),
            Representation::Map => super::records::map_record_deser_refusals(types, record, cli),
        }
    }

    /// An enum arm with no deserialize takes the whole enum's with it: the emitted dispatch calls
    /// `Arm::deserialize` (type choice) or `Arm::deserialize_as_embedded_group` (array-rep group
    /// choice) unconditionally, so an enum that kept its own impl over a refused arm would emit a
    /// call to a function that was never generated. The reason names the arm, so the existing loud
    /// `Not generating X::deserialize()` warning says which one.
    fn seed_enum_variant_refusals(
        &mut self,
        types: &IntermediateTypes,
        cli: &Cli,
        ident: &RustIdent,
        variants: &[EnumVariant],
        rep: Option<Representation>,
    ) {
        for variant in variants {
            match &variant.data {
                EnumVariantData::RustType(ty) => {
                    if !self.deserialize_generated_for_type(&ty.conceptual_type) {
                        self.dont_generate_deserialize(
                            ident,
                            format!(
                                "variant {}: {} couldn't generate deserialize",
                                variant.name,
                                ty.for_rust_member(types, false, cli)
                            ),
                        );
                    }
                }
                // an inlined arm record has no struct of its own to refuse — its deserialize IS
                // this enum's, so its shape refusals land here directly.
                EnumVariantData::Inlined(record) => {
                    for field in &record.fields {
                        if !self.deserialize_generated_for_type(&field.rust_type.conceptual_type) {
                            self.dont_generate_deserialize(
                                ident,
                                format!(
                                    "variant {} field {}: {} couldn't generate deserialize",
                                    variant.name,
                                    field.name,
                                    field.rust_type.for_rust_member(types, false, cli)
                                ),
                            );
                        }
                    }
                    if rep == Some(Representation::Array) {
                        for reason in super::records::array_record_deser_refusals(types, record) {
                            self.dont_generate_deserialize(ident, reason);
                        }
                    }
                }
            }
        }
    }

    pub fn print_structs_without_deserialize(&self) {
        for (name, reasons) in &self.no_deser_reasons {
            // Header AND reasons on stderr: the pair is one diagnostic, and splitting the streams
            // meant `2>/dev/null` kept the reasons and dropped what they were reasons FOR.
            crate::warn!("Not generating {name}::deserialize() - reasons:");
            for reason in reasons {
                crate::warn!("\t{reason}");
            }
        }
    }
}
