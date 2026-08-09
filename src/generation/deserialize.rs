use super::*;

#[derive(Debug, Clone)]
pub(super) struct DeserializeConfig<'a> {
    /// for creating unique identifiers for temporaries
    var_name: &'a str,
    /// whether we're generating inside of a DeserializeEmbeddedGroup impl
    in_embedded: bool,
    /// whether this is an optional field
    optional_field: bool,
    /// Extra values associated with the deserialization to return as part of the final tuple e.g. (x, x_encoding, x_key_encodings)/
    /// This will be empty for root calls, but recursive ones
    /// might need to add in extra ones for encoding vars e.g. a tagged other value will have the tagged encoding
    /// var tacked on to the inner values.
    final_exprs: Vec<String>,
    /// Overload for the deserializer's name. Defaults to "raw"
    deserializer_name_overload: Option<&'a str>,
    /// Overload for read_len. This would be a local e.g. for arrays
    read_len_overload: Option<String>,
    /// Override regular deserialization lgoic with a call to this function
    custom_deserialize: Option<String>,
    /// The `@custom_encodings` declaration written beside the pair in `custom_deserialize`, when it
    /// has one: the codec-visible encoding variables of ITS wire, which then decide the shape of the
    /// tuple the custom reader returns instead of the replaced type's inferred demand. Lifted at
    /// exactly the two places the pair itself is lifted (`for_field`, and the `Alias` arm), so it can
    /// never travel without its pair. `None` = no declaration; inference, unchanged.
    custom_encodings: Option<Vec<EncodingKind>>,
    /// The member's type AS DECLARED, when the declaration is an `AliasIdent::Rust` — the spelling
    /// the member-level deserialize CALL TARGETS use instead of the alias's structural target
    /// (`StakeCredential::deserialize` where the field is `sc: StakeCredential`, not
    /// `Credential::deserialize`). Lifted at the `Alias` arm, consumed ONLY through
    /// [`Self::call_target`]; `None` = spell the structural ident, which is also what every
    /// position that is not a call target keeps doing (an error-message string literal and an
    /// enum-variant path name the struct itself, never the member's declaration — see the
    /// `CStyleEnum` arm).
    ///
    /// A spelling is lifted only when the alias RULE owns no encoding operation — see the `Alias`
    /// arm, which is where that test lives (the config cannot see the rule).
    ///
    /// User doc: `docs/docs/output_format.mdx` § "Type spelling at member positions".
    declared_spelling: Option<String>,
    /// number of tag levels already crossed on this member name (0 at the field root). Drives the
    /// `tag`/`tag2`/… encoding-var infix (and the `tag_enc`/`tag_enc2` binding) so stacked tags each
    /// record their own level's size instead of shadowing. See `tag_encoding_infix`.
    tag_depth: usize,
    /// number of `.cbor` payload levels already crossed on this member name (0 at the field root).
    /// Drives `cbor_bytes_infix` (the `{var}_bytes`/`{var}_bytes2` byte vector and its encoding
    /// var), `cbor_payload_reader` (the `inner_de`/`inner_de2` reader overload) and
    /// `cbor_payload_binding_suffix` (the `{var}_payload`/`{var}_payload2` staging local) so the
    /// INLINE `bytes .cbor (bytes .cbor T)` spelling keeps its two depths apart. Undepthed, the
    /// inner level SHADOWS the outer in the same block: the outer's leftover-bytes check re-reads
    /// the inner reader (silent over-acceptance) and, under `--preserve-encodings`, the outer's
    /// final expr reads the inner's string encoding.
    ///
    /// Same lockstep invariant as `tag_depth`, against the same `encoding_fields_impl` counter.
    /// Name-boundary recursions (array element, map key/value) build a FRESH `DeserializeConfig`,
    /// which is where the reset comes from on this side.
    cbor_depth: usize,
}

impl<'a> DeserializeConfig<'a> {
    pub(super) fn new(var_name: &'a str) -> Self {
        Self {
            var_name,
            in_embedded: false,
            optional_field: false,
            final_exprs: Vec::new(),
            deserializer_name_overload: None,
            read_len_overload: None,
            custom_deserialize: None,
            custom_encodings: None,
            declared_spelling: None,
            tag_depth: 0,
            cbor_depth: 0,
        }
    }

    /// THE constructor for deserializing a record field. Use this at every record-field deserialize
    /// site rather than `new(..)` + hand-chained setters: it carries the field's
    /// `@custom_deserialize` directive automatically. Forgetting to re-carry a custom directive when
    /// adding a new call site is a recurring bug class here, so this owns that carry in one place.
    pub(super) fn for_field(field: &'a RustField, in_embedded: bool, optional: bool) -> Self {
        let mut config = Self::new(&field.name)
            .in_embedded(in_embedded)
            .optional_field(optional);
        if let Some(custom_deserialize) = &field.rule_metadata.custom_deserialize {
            config = config.custom_deserialize(custom_deserialize.clone());
            // The field's own `@custom_encodings`, and only its own — see the serialize twin.
            config.custom_encodings = field.rule_metadata.custom_encodings.clone();
        }
        config
    }

    pub(super) fn in_embedded(mut self, in_embedded: bool) -> Self {
        self.in_embedded = in_embedded;
        self
    }

    /// Rename the temporaries this deserialize binds (`{var}_value` / `{var}_encoding`). Use when
    /// the emission site inlines the deserialize into a scope that already holds a same-named
    /// accumulator (no closure isolates the bindings — the `--annotate-fields=false` map-arm case),
    /// so the un-prefixed name would shadow the accumulator and a trailing reassignment would hit
    /// the shadow (E0308). Chain AFTER `for_field` so the `@custom_deserialize` carry is kept.
    pub(super) fn overload_var_name(mut self, var_name: &'a str) -> Self {
        self.var_name = var_name;
        self
    }

    pub(super) fn optional_field(mut self, is_optional: bool) -> Self {
        self.optional_field = is_optional;
        self
    }

    pub(super) fn tag_depth(mut self, tag_depth: usize) -> Self {
        self.tag_depth = tag_depth;
        self
    }

    pub(super) fn cbor_depth(mut self, cbor_depth: usize) -> Self {
        self.cbor_depth = cbor_depth;
        self
    }

    pub(super) fn overload_deserializer(mut self, overload: &'a str) -> Self {
        self.deserializer_name_overload = Some(overload);
        self
    }

    pub(super) fn deserializer_name(&self) -> &'a str {
        self.deserializer_name_overload.unwrap_or("raw")
    }

    pub(super) fn overload_read_len(mut self, overload: String) -> Self {
        self.read_len_overload = Some(overload);
        self
    }

    pub(super) fn custom_deserialize(mut self, func: String) -> Self {
        self.custom_deserialize = Some(func);
        self
    }

    /// Lift a pair's `@custom_encodings` declaration. Always chained onto the same
    /// `custom_deserialize` lift so the two cannot separate.
    pub(super) fn custom_encodings(mut self, kinds: Option<Vec<EncodingKind>>) -> Self {
        self.custom_encodings = kinds;
        self
    }

    /// Lift the member's declared spelling (see [`Self::declared_spelling`]). OUTERMOST WINS,
    /// enforced here rather than at the call site so the `Alias` arm cannot get it wrong:
    /// `alias_b = alias_a = credential` keeps the member's own declared name.
    pub(super) fn declare_spelling(mut self, spelling: String) -> Self {
        if self.declared_spelling.is_none() {
            self.declared_spelling = Some(spelling);
        }
        self
    }

    /// Descend into an `Optional`'s inner: a member position of its OWN, so it spells from its own
    /// declaration (its `Alias` arm lifts it) and must not inherit the outer member's. Without this
    /// a member typed `maybe_cred = credential / null` emits
    /// `Some(MaybeCred::deserialize(raw)?)` — and since `MaybeCred` IS `Option<Credential>`, the
    /// call target resolves to a type with no such associated function: `E0599: the variant,
    /// associated function, or constant 'deserialize' exists for enum 'Option<Credential>', but its
    /// trait bounds were not satisfied`. (Reproduced by removing this clear, not predicted from the
    /// shape — the `Some(..)` wrapping makes an E0308 look likelier than it is.) `Array`/`Map`
    /// inners need no equivalent: they recurse with a fresh `DeserializeConfig`.
    pub(super) fn clear_declared_spelling(mut self) -> Self {
        self.declared_spelling = None;
        self
    }

    /// THE spelling of a member-level deserialize call target. Use at every call-target format in the
    /// `Rust(ident)` arm (`T::deserialize`, `T::from_raw_bytes`, `T::deserialize_as_embedded_group`)
    /// and NOWHERE else: the same arm also interpolates `ident` into a `DeserializeError::new("..")`
    /// string literal (runtime-observable output) and into `{ident}::{variant}` enum-variant paths
    /// (which name the struct's own variants, not the member's type). Routing every call target
    /// through one method is what keeps those two out of the change.
    pub(super) fn call_target(&self, ident: &RustIdent) -> String {
        self.declared_spelling
            .clone()
            .unwrap_or_else(|| ident.to_string())
    }

    pub(super) fn pass_read_len(&self) -> String {
        if let Some(overload) = &self.read_len_overload {
            // the ONLY way to have a name overload is if we have a local variable (e.g. arrays)
            format!("&mut {overload}")
        } else if self.in_embedded {
            "read_len".to_owned()
        } else {
            "&mut read_len".to_owned()
        }
    }
}

/// Output code for deserialization. Includes meta information for better usage to prevent warnings.
#[derive(Default, Debug)]
pub(super) struct DeserializationCode {
    pub(super) content: BlocksOrLines,
    pub(super) read_len_used: bool,
    // whether the embedded-group `len` param is referenced (only the plain-group delegation
    // does; under --preserve-encodings the param is always consumed by `len_encoding` instead)
    pub(super) len_used: bool,
    // whether ? is used in content
    pub(super) throws: bool,
}

impl DeserializationCode {
    pub(super) fn add_to<T>(self, body: &mut T)
    where
        T: CodeBlock + Sized,
    {
        body.push_all(self.content);
    }

    pub(super) fn add_to_code(self, target: &mut Self) {
        if self.read_len_used {
            target.read_len_used = true;
        }
        if self.len_used {
            target.len_used = true;
        }
        if self.throws {
            target.throws = true;
        }
        target.content.push_all(self.content);
    }

    /// dumps self.content into {block} then uses {block} as our new content
    pub(super) fn wrap_in_block(mut self, mut block: Block) -> Self {
        block.push_all(self.content);
        self.content = block.into();
        self
    }

    /// This MUST have self.content be a Result, as if you were going to wrap it in
    /// an error annotation lambda block. If possible this will avoid the need for
    /// the block to avoid clippy warnings.
    pub(super) fn annotate(mut self, annotation: &str, before: &str, after: &str) -> Self {
        if let Some(single_line) = self.content.as_single_line() {
            self.content = BlocksOrLines(vec![BlockOrLine::Line(format!(
                "{before}{single_line}.map_err(|e: DeserializeError| e.annotate(\"{annotation}\")){after}"
            ))]);
            self
        } else {
            self.throws = false;
            self.wrap_in_block(make_err_annotate_block(annotation, before, after))
        }
    }

    pub(super) fn mark_and_extract_content(self, target: &mut Self) -> BlocksOrLines {
        if self.read_len_used {
            target.read_len_used = true;
        }
        if self.len_used {
            target.len_used = true;
        }
        if self.throws {
            target.throws = true;
        }
        self.content
    }
}

impl From<BlocksOrLines> for DeserializationCode {
    fn from(content: BlocksOrLines) -> Self {
        Self {
            content,
            read_len_used: false,
            len_used: false,
            throws: false,
        }
    }
}

/// Context as to how to generate deserialization code.
/// formats as {before}{<deserialized value>}{after} in a line within the body param, allowing freedom e.g.:
/// * {let x = }{<value>}{;} - creation of variables
/// * {x = Some(}{<value>}{);} - variable assignment (could be nested in function call, etc, too)
/// * {}{<value>}{} - for last-expression eval in blocks
/// * etc
///
/// We also keep track of if it expects a result and can adjust the generated code based on that
/// to avoid warnings (e.g. avoid Ok(foo?) and directly do foo instead)
pub(super) struct DeserializeBeforeAfter<'a> {
    before: &'a str,
    after: &'a str,
    expects_result: bool,
}

impl<'a> DeserializeBeforeAfter<'a> {
    pub(super) fn new(before: &'a str, after: &'a str, expects_result: bool) -> Self {
        Self {
            before,
            after,
            expects_result,
        }
    }

    pub(super) fn before_str(&self, is_result: bool) -> String {
        match (self.expects_result, is_result) {
            // T -> Result<T, _>
            (true, false) => format!("{}Ok(", self.before),
            // Result<T, _> => T (nothing to be done in before case)
            // (false, true) => self.before.to_owned(),
            // expected == found, nothing to be done
            (_, _) => self.before.to_owned(),
        }
    }

    /// Whether a value expression emitted at this position is DISCARDED: nothing to the left binds
    /// it (`before` is empty) and no caller is waiting on an `Ok(..)` wrapper. Used by the fixed
    /// bool/null arms under `--preserve-encodings`, whose only value is the unit `()` — emitting it
    /// at a discarding position leaves a degenerate `();` statement that `clippy::no_effect` flags
    /// in every consumer's regenerated crate. Every discarding call site pairs an empty `before`
    /// with an `after` of `""` or `";"`, so suppressing the whole line drops only the terminator.
    pub(super) fn discards_value(&self) -> bool {
        self.before.is_empty() && !self.expects_result
    }

    /// Whether a value emitted at this position forms a complete STATEMENT — `before`/`after`
    /// bracket it into `let x = <value>;` or `x = Some(<value>);`, and no caller is waiting on an
    /// `Ok(..)` wrapper. Then, and only then, may further code be appended AFTER the value: every
    /// other position is a terminal EXPRESSION (a block's tail, a tuple element, a closure body)
    /// where a following statement is not Rust. Used by the `.cbor` payload arm, whose
    /// payload-exhausted check can only run once the payload has been consumed.
    pub(super) fn is_statement(&self) -> bool {
        self.after.ends_with(';') && !self.expects_result
    }

    pub(super) fn after_str(&self, is_result: bool) -> String {
        match (self.expects_result, is_result) {
            // Result<T, _> -> T
            (false, true) => format!("?{}", self.after),
            // T ->
            (true, false) => format!("){}", self.after),
            // expected == found, nothing to be done
            (false, false) | (true, true) => self.after.to_owned(),
        }
    }
}

/// Emit the unit `()` — the value of a construct that only VERIFIES and stores nothing — through a
/// caller's `before`/`after` wrapper, or emit nothing at all when the position DISCARDS it.
///
/// The suppression is the same one the preserve-side fixed Null/Bool arms apply, for the same two
/// reasons: a discarded `();` is a `clippy::no_effect` finding in every consumer's regenerated
/// crate, and at a block tail the missing line is not missing at all — a block with no tail
/// expression already evaluates to `()`.
fn line_unit_value(deser_code: &mut DeserializationCode, before_after: &DeserializeBeforeAfter) {
    if !before_after.discards_value() {
        deser_code.content.line(&format!(
            "{}(){}",
            before_after.before_str(false),
            before_after.after_str(false)
        ));
    }
}

/// Whether a `.cbor` payload's target carries NO value of its own — the test the payload arm
/// applies to choose between reading with a discarding wrapper and STAGING the read into
/// `let {var}_payload = …;`. Callers gate it on `!cli.preserve_encodings`, which is where the
/// profile dependence lives: with encodings on, a fixed value carries one and nothing here is
/// value-less.
///
/// The property belongs to the whole encoding CHAIN, not to its root. A `Fixed` root stores
/// nothing, and a mandatory `Tagged` operation over one stores nothing either — the tag number is a
/// constant, verified on the way past, and the arm it emits evaluates to `()` like its child. So
/// `bytes .cbor #6.1(42)` is exactly as value-less as `bytes .cbor 42`, and testing only the root
/// staged the tagged spelling: `let k_payload = ();` followed by a bare `k_payload` line in the
/// discarding STATEMENT slots a fixed member is read in (an array struct's field sequence, a map
/// entry's presence closure), which is not Rust and aborted at rustfmt rather than refusing.
///
/// A nested `CBORBytes` is seen through for the same reason: without encodings a byte string that
/// merely FRAMES a value-less payload stores nothing either — its bytes are re-derived from the
/// payload on the way out — so `bytes .cbor (bytes .cbor 42)` is exactly as value-less as
/// `bytes .cbor 42`. Testing only the outer level staged `let b_payload = ();` in the discarding
/// statement slots, the same rustfmt-aborting shape described above.
///
/// `OptionallyTagged` is the one operation deliberately NOT seen through: it records the tag's
/// PRESENCE, which is a value the caller's position has to receive in every profile.
fn payload_is_value_less(child: &SerializingRustType) -> bool {
    match child {
        SerializingRustType::Root(ConceptualRustType::Fixed(_), _) => true,
        SerializingRustType::EncodingOperation(CBOREncodingOperation::Tagged(_), inner)
        | SerializingRustType::EncodingOperation(CBOREncodingOperation::CBORBytes, inner) => {
            payload_is_value_less(inner)
        }
        _ => false,
    }
}

// Adds a fixed length check if length is fixed, reads the mandatory amount if there are optional fields, or nothing for dynamic lengths
pub(super) fn add_deserialize_initial_len_check(
    deser_body: &mut dyn CodeBlock,
    len_info: RustStructCBORLen,
    cli: &Cli,
) {
    deser_body.line(&format!(
        "let mut read_len = {}(len);",
        cbor_read_len_ctor(cli)
    ));
    match len_info {
        RustStructCBORLen::Dynamic =>
            /*nothing*/
            {}
        // TODO: direct check here instead of involving read_len
        RustStructCBORLen::OptionalFields(mandatory) => {
            if mandatory != 0 {
                deser_body.line(&format!("read_len.read_elems({mandatory})?;"));
            }
        }
        RustStructCBORLen::Fixed(fixed) => {
            if fixed != 0 {
                deser_body.line(&format!("read_len.read_elems({fixed})?;"));
            }
            // We MUST check even in the fixed case, as you might be parsing something that
            // is a CBOR prefix field-wise to your data e.g.:
            //   foo = [uint, bytes]
            //   bar = [uint, bytes, str]
            // would have any bar be parsable as foo (problematic when we have foo / bar in a choice)
            // so we must ensure we end up with precisely 0 left over at the end even in fixed cases.
            // We do the check right away instead of waiting. We don't do this inside of
            // add_deserialize_final_len_check for all variants as some enum use-cases
            // break as they rely on being able to do the final check without read_len
            deser_body.line("read_len.finish()?;");
        }
    }
}

// Adds final Len check if not fixed + reads for the ending Special::Break for Indefinite arrays
pub(super) fn add_deserialize_final_len_check(
    deser_body: &mut dyn CodeBlock,
    rep: Option<Representation>,
    len_info: RustStructCBORLen,
    cli: &Cli,
) {
    // We only check for Break for arrays since the implementation for maps uses len to decide
    // when to stop reading values, since otherwise with optional parameters it doesn't know.
    // We also can't do it from within deserialize_as_embedded_group() as that interferes with
    // plain groups nested inside other array groups
    let ending_check = match len_info {
        RustStructCBORLen::Fixed(_) => "()", // no need to check - checked at the start
        RustStructCBORLen::OptionalFields(_) | RustStructCBORLen::Dynamic => "read_len.finish()?",
    };
    match rep {
        Some(Representation::Array) => {
            let mut end_len_check = Block::new("match len");
            end_len_check.line(format!(
                "{} => {},",
                cbor_event_len_n("_", cli),
                ending_check
            ));
            let mut indefinite_check = Block::new(format!(
                "{} => match raw.special()?",
                cbor_event_len_indef(cli)
            ));
            indefinite_check.line(format!("cbor_event::Special::Break => {ending_check},"));
            indefinite_check
                .line("_ => return Err(DeserializeFailure::EndingBreakMissing.into()),");
            indefinite_check.after(",");
            end_len_check.push_block(indefinite_check);
            deser_body.push_block(end_len_check);
        }
        // For Fixed, ending_check is the "()" placeholder (length already fully checked at the
        // start); emitting it as a statement would produce a standalone `();` (clippy::no_effect).
        Some(Representation::Map) if !matches!(len_info, RustStructCBORLen::Fixed(_)) => {
            deser_body.line(&format!("{ending_check};"));
        }
        Some(Representation::Map) => {}
        None =>
            /* this should just be for type choices */
            {}
    }
}

// CASE 1 - generate_deserialize_embedded = true:
//     Returns (Deserialize impl, Some(DeserializeEmbeddedGroup impl))
//     The caller should create and push their own deserialize_as_embedded_group to the
//     DeserializeEmbeddedGroup impl which will be called
//     from within deserialize(), and deserialize() should not be expanded upon, just pushed.
// CASE 2 - generate_deserialize_embedded = false:
//     Returns (Deserialize impl, None) and you implement the rest of the deserialize.
//     Only the array/map tag + length are read (including length checks) so far
//     and the user will want to write the rest of deserialize() after that.
//     It would be wise to use add_deserialize_final_len_check() as well since that does a final length check AND
//     reads the ending break closing tag for indefinite arrays (indefinite maps are read as a by-product of implementation)),
//     but this is done automatically for the embedded case.
// In both cases the deserialize function should be created and pushed to the Deserialize impl.
// deser_body shall be the body of deserialize()
// Also, a length check will be done if len_info is passed in, it will be checked at the start
// of deserialize(). An ending check is also done if we are generating the embedded deserialize,
// and should be added manually via CBORReadLen::finish() at the end of deserialize() if not using add_deserialize_final_len_check().
// This (in both options) relies on the use of CBORReadLen at every non-mandatory (if using len_info) element read, or all elements otherwise.
// * `store_encoding` - If present, creates a variable of the provided name in the deserialization impl as a bool to store if definite was used (true) or indefinite (false)
// Only generated if generate_deserialize_embedded is false as otherwise we wouldn't have access to it from within the embedded code block as it is declared in the regular Deserialize
// * `annotated` - true iff deser_body will end up inside an `.annotate(name)` error closure. The
//   tag-mismatch error must then be the locationless form (`DeserializeFailure::..into()`): the
//   closure's map_err supplies the type name, and the location-carrying form
//   (`DeserializeError::new(name, ..)`) would get the name PREPENDED again ("Name.Name"). When no
//   closure exists (annotate_fields=false) the named form is required or the name would be lost
//   entirely. This param governs the NON-embedded emission below; the embedded (plain-group)
//   deserialize() case builds its own scaffolding closures when `cli.annotate_fields` (handled in a
//   dedicated branch at the top of the fn) and ignores `annotated` — the codegen_struct caller
//   therefore passes it `false` for plain groups.
#[allow(clippy::too_many_arguments)]
pub(super) fn create_deserialize_impls(
    ident: &RustIdent,
    rep: Option<Representation>,
    tag: Option<usize>,
    len_info: Option<RustStructCBORLen>,
    generate_deserialize_embedded: bool,
    store_encoding: Option<&str>,
    deser_body: &mut dyn CodeBlock,
    annotated: bool,
    cli: &Cli,
) -> (codegen::Impl, Option<codegen::Impl>) {
    let name = &ident.to_string();
    let mut deser_impl = codegen::Impl::new(name);
    // TODO: add config param to decide if we want to use our deserialize
    //       or theirs using Error::Custom(String) + DeserializeError::to_string()
    //deser_impl.impl_trait("cbor_event::de::Deserialize");
    deser_impl.impl_trait("Deserialize");
    // Plain-group (embedded) deserialize() with annotation on: the pre-delegation scaffolding (tag
    // read/check + container-len read + read_len construction/initial checks) and the post-delegation
    // final-len check each get their OWN `.annotate(name)` error closure so a wrong-major /
    // wrong-length / missing-break rejection carries the type name — exactly like the non-embedded
    // record path already annotates its scaffolding. The `deserialize_as_embedded_group` delegation
    // stays OUTSIDE any closure: its body is already annotated per-field, so wrapping it would
    // double-annotate ("Type.Type.field"). This branch only fires for embedded groups (never the enum
    // path, which passes generate_deserialize_embedded=false); every other case keeps the original
    // sequential emission below unchanged, so non-embedded records and enums stay byte-identical.
    if generate_deserialize_embedded && cli.annotate_fields {
        let rep = rep.expect("embedded groups always have an array/map representation");
        let len_info =
            len_info.expect("embedded plain-group deserialize() is always given its len_info");
        // Pre-delegation scaffolding, built into a closure returning the bindings later code needs.
        let mut pre = BlocksOrLines::default();
        if let Some(tag) = tag {
            if cli.preserve_encodings {
                pre.line("let (tag, tag_encoding) = raw.tag_sz()?;");
            } else {
                pre.line("let tag = raw.tag()?;");
            }
            // Inside the annotate closure, so the locationless form (the closure supplies the name).
            let mut tag_check = Block::new(format!("if tag != {tag}"));
            tag_check.line(format!("return Err(DeserializeFailure::TagMismatch{{ found: tag, expected: {tag} }}.into());"));
            pre.push_block(tag_check);
        }
        match rep {
            Representation::Array => {
                pre.line(if cli.preserve_encodings {
                    "let len = raw.array_sz()?;"
                } else {
                    "let len = raw.array()?;"
                });
            }
            Representation::Map => {
                pre.line(if cli.preserve_encodings {
                    "let len = raw.map_sz()?;"
                } else {
                    "let len = raw.map()?;"
                });
            }
        }
        // Inline the read_len construction + initial checks instead of calling
        // add_deserialize_initial_len_check: here the delegation's `&mut read_len` use lives OUTSIDE
        // the closure, so `read_len` is only mutated inside the closure when a `read_elems` is
        // emitted (Fixed>0 / OptionalFields>0). Binding it `mut` unconditionally (as the shared
        // helper does, correct there because the delegation follows in-scope) would emit `unused_mut`
        // for the Dynamic / Fixed(0) / OptionalFields(0) cases. Everything else matches the helper.
        let read_len_mutated = matches!(len_info, RustStructCBORLen::Fixed(f) if f != 0)
            || matches!(len_info, RustStructCBORLen::OptionalFields(m) if m != 0);
        pre.line(&format!(
            "let {}read_len = {}(len);",
            if read_len_mutated { "mut " } else { "" },
            cbor_read_len_ctor(cli)
        ));
        match len_info {
            RustStructCBORLen::Dynamic => {}
            RustStructCBORLen::OptionalFields(mandatory) => {
                if mandatory != 0 {
                    pre.line(&format!("read_len.read_elems({mandatory})?;"));
                }
            }
            RustStructCBORLen::Fixed(fixed) => {
                if fixed != 0 {
                    pre.line(&format!("read_len.read_elems({fixed})?;"));
                }
                pre.line("read_len.finish()?;");
            }
        }
        pre.line("Ok((len, read_len))");
        let mut pre_closure = make_err_annotate_block(name, "let (len, mut read_len) = ", "?;");
        pre_closure.push_all(pre);
        deser_body.push_block(pre_closure);
        // Delegation OUTSIDE any closure (its per-field errors are already annotated).
        deser_body.line("let ret = Self::deserialize_as_embedded_group(raw, &mut read_len, len);");
        // Post-delegation final-len check (ending break / trailing-length), wrapped in its own
        // annotate closure so a missing-break / definite-len-mismatch rejection carries the name.
        let mut post = BlocksOrLines::default();
        add_deserialize_final_len_check(&mut post, Some(rep), len_info, cli);
        if !post.0.is_empty() {
            let mut post_closure = make_err_annotate_block(name, "", "?;");
            post_closure.push_all(post);
            post_closure.line("Ok(())");
            deser_body.push_block(post_closure);
        }
        deser_body.line("ret");
        let mut embedded_impl = codegen::Impl::new(name);
        embedded_impl.impl_trait("DeserializeEmbeddedGroup");
        return (deser_impl, Some(embedded_impl));
    }
    if let Some(tag) = tag {
        if cli.preserve_encodings {
            deser_body.line("let (tag, tag_encoding) = raw.tag_sz()?;");
        } else {
            deser_body.line("let tag = raw.tag()?;");
        }
        let mut tag_check = Block::new(format!("if tag != {tag}"));
        if annotated {
            tag_check.line(format!("return Err(DeserializeFailure::TagMismatch{{ found: tag, expected: {tag} }}.into());"));
        } else {
            tag_check.line(format!("return Err(DeserializeError::new(\"{name}\", DeserializeFailure::TagMismatch{{ found: tag, expected: {tag} }}));"));
        }
        deser_body.push_block(tag_check);
    }
    if let Some(rep) = rep {
        match rep {
            Representation::Array => {
                if cli.preserve_encodings {
                    deser_body.line("let len = raw.array_sz()?;");
                } else {
                    deser_body.line("let len = raw.array()?;");
                }
                if !generate_deserialize_embedded && let Some(encoding_var_name) = store_encoding {
                    deser_body.line(&format!(
                        "let {encoding_var_name}: LenEncoding = len.into();"
                    ));
                }
                if let Some(len_info) = len_info {
                    add_deserialize_initial_len_check(deser_body, len_info, cli);
                }
                if generate_deserialize_embedded {
                    deser_body.line(
                        "let ret = Self::deserialize_as_embedded_group(raw, &mut read_len, len);",
                    );
                }
            }
            Representation::Map => {
                if cli.preserve_encodings {
                    deser_body.line("let len = raw.map_sz()?;");
                } else {
                    deser_body.line("let len = raw.map()?;");
                }
                if !generate_deserialize_embedded && let Some(encoding_var_name) = store_encoding {
                    deser_body.line(&format!(
                        "let {encoding_var_name}: LenEncoding = len.into();"
                    ));
                }
                if let Some(len_info) = len_info {
                    add_deserialize_initial_len_check(deser_body, len_info, cli);
                }
                if generate_deserialize_embedded {
                    deser_body.line(
                        "let ret = Self::deserialize_as_embedded_group(raw, &mut read_len, len);",
                    );
                }
            }
        };
    } else {
        panic!("TODO: how should we handle this considering we are dealing with Len?");
        //deser_body.line("Self::deserialize_as_embedded_group(serializer)");
    }
    let deser_embedded_impl = if generate_deserialize_embedded {
        if let Some(len_info) = len_info {
            add_deserialize_final_len_check(deser_body, rep, len_info, cli);
        }
        deser_body.line("ret");
        let mut embedded_impl = codegen::Impl::new(name);
        embedded_impl.impl_trait("DeserializeEmbeddedGroup");
        Some(embedded_impl)
    } else {
        None
    };
    (deser_impl, deser_embedded_impl)
}

// We need to execute field deserialization inside a closure in order to capture and annotate with the field name
// without having to put error annotation inside of every single cbor_event call.
pub(super) fn make_err_annotate_block(annotation: &str, before: &str, after: &str) -> Block {
    let mut if_block = Block::new(format!("{before}(|| -> Result<_, DeserializeError>"));
    if_block.after(format!(
        ")().map_err(|e| e.annotate(\"{annotation}\")){after}"
    ));
    if_block
}

pub(super) fn make_deser_loop(len_var: &str, len_expr: &str, cli: &Cli) -> Block {
    Block::new(format!(
        "while match {} {{ {} => {} < n, {} => true, }}",
        len_var,
        cbor_event_len_n("n", cli),
        len_expr,
        cbor_event_len_indef(cli)
    ))
}

fn make_deser_loop_break_check(len_var: &str, deserializer_name: &str, cli: &Cli) -> Block {
    // The probe reads from `deserializer_name`, NOT unconditionally from `raw`: under a `bytes .cbor`
    // overload the collection's elements/keys come from the payload's own `inner_de`, so probing the
    // outer buffer for the break byte would frame the check against a different cursor than the one
    // the loop is consuming.
    //
    // Only INDEFINITE-length collections carry a break byte (`0xff`). For a definite length the loop
    // reads exactly `n` items, so there is nothing to detect here — and we must NOT peek: the break
    // byte shares major type 7 (Special) with bool / null / float16-32-64 / simple, so an ungated
    // Special check would eat a definite-length special element/key.
    //
    // In the indefinite arm we detect the break with `Deserializer::special_break()`: a
    // NON-consuming probe that advances past the `0xff` break iff that's the next byte, and returns
    // `false` WITHOUT advancing on any other Special (a bool/null/float element or key), which then
    // falls through to the element/key deserializer and reads normally. This is why the whole prior
    // "indefinite container of value-specials" limitation is gone — a non-break special is no longer
    // consumed-and-rejected. The `cbor_type` guard stays load-bearing: `special_break` errors on
    // non-Special input.
    let mut brk = Block::new(format!(
        "if matches!({len_var}, {}) && {deserializer_name}.cbor_type()? == cbor_event::Type::Special && {deserializer_name}.special_break()?",
        cbor_event_len_indef(cli)
    ));
    brk.line("break;");
    brk
}

pub(super) fn make_deserialization_function(name: &str, cli: &Cli) -> codegen::Function {
    let mut f = codegen::Function::new(name);
    f.ret("Result<Self, DeserializeError>")
        .arg("raw", "&mut Deserializer");
    // Opt-in recursion depth guard: the first statement of every composite `deserialize` acquires
    // an RAII guard whose Drop restores the thread-local depth on any return path (including `?`).
    // Bound in the outer function scope so it stays alive across the annotator closure the body may
    // be wrapped in. Only the top-level `deserialize` is guarded (not `deserialize_as_embedded_group`,
    // which is part of the same logical type and reached with the guard already held). The limit is
    // baked at generation time from the flag.
    if name == "deserialize"
        && let Some(limit) = cli.deserialize_depth_limit
    {
        f.line(format!(
            "let _depth_guard = DepthGuard::acquire({limit}usize)?;"
        ));
    }
    f
}

impl GenerationScope {
    /// Generates a DeserializationCode to serialize {serializing_rust_type} using the context in {before_after}
    /// This returned value must be in turn pushed into deserialization code to be used.
    #[must_use]
    pub(super) fn generate_deserialize(
        &mut self,
        types: &IntermediateTypes,
        serializing_rust_type: SerializingRustType,
        before_after: DeserializeBeforeAfter,
        mut config: DeserializeConfig,
        cli: &Cli,
    ) -> DeserializationCode {
        //body.line(&format!("println!(\"deserializing {}\");", var_name));
        if !cli.preserve_encodings {
            assert!(config.final_exprs.is_empty());
        }
        let mut deser_code = DeserializationCode::default();
        // joins all config.final_expr together (possibly) with the actual value into a tuple type (if multiple)
        // or otherwise the value just goes through on its own
        let final_expr =
            |mut encoding_exprs: Vec<String>, actual_value: Option<String>| -> String {
                if let Some(e) = actual_value {
                    // possibly less efficient but more concise
                    encoding_exprs.insert(0, e);
                }
                if encoding_exprs.len() > 1 {
                    format!("({})", encoding_exprs.join(", "))
                } else {
                    encoding_exprs.join(", ")
                }
            };
        let convert_err_to_ours = CONVERT_ERR_TO_OURS;
        // Gives a total final expression including the before_after context
        // as well as dealing with avoiding clippy warning which is why we can
        // be conditionally a direct value (if there are encoding vars thus a tuple)
        // or we can be a result that goes straight through (subject to before_after)
        // This helps avoid clippy::needless_question_mark here.
        let final_result_expr_complete =
            |throws: &mut bool, final_exprs: Vec<String>, result_expr: &str| -> String {
                if final_exprs.is_empty() {
                    format!(
                        "{}{}{}",
                        before_after.before_str(true),
                        result_expr,
                        before_after.after_str(true)
                    )
                } else {
                    *throws = true;
                    format!(
                        "{}{}{}",
                        before_after.before_str(false),
                        final_expr(final_exprs, Some(format!("{result_expr}?"))),
                        before_after.after_str(false)
                    )
                }
            };
        let deserializer_name = config.deserializer_name();
        // field-level @custom_deserialize overrides everything
        if let Some(custom_deserialize) = &config.custom_deserialize {
            let deser_err_map = if !config.final_exprs.is_empty() {
                // The pair's OWN declaration wins over the replaced type's inferred demand (serialize
                // twin at `generate_serialize`'s custom branch); undeclared falls back to inference,
                // blind to declarations below since THIS codec owns the wire from here down.
                let enc_fields = match &config.custom_encodings {
                    Some(kinds) => declared_encoding_fields(config.var_name, kinds),
                    None => encoding_fields_impl(
                        types,
                        config.var_name,
                        serializing_rust_type,
                        cli,
                        0,
                        0,
                        AliasDeclarations::Blind,
                    ),
                };
                let (closure_args, tuple_fields) = if enc_fields.is_empty() {
                    (config.var_name.to_owned(), "".to_owned())
                } else {
                    let enc_fields_names = enc_fields
                        .iter()
                        .map(|enc| enc.field_name.clone())
                        .collect::<Vec<String>>()
                        .join(", ");
                    (
                        format!("({}, {})", config.var_name, enc_fields_names),
                        enc_fields_names,
                    )
                };
                Cow::Owned(format!(
                    ".map(|{}| ({}, {}, {}))",
                    closure_args,
                    config.var_name,
                    config.final_exprs.join(", "),
                    tuple_fields
                ))
            } else {
                Cow::Borrowed("")
            };
            deser_code.content.line(&format!(
                "{}{}({}){}{}",
                before_after.before_str(true),
                custom_deserialize,
                deserializer_name,
                deser_err_map,
                before_after.after_str(true),
            ));
        } else {
            match serializing_rust_type {
                SerializingRustType::Root(ConceptualRustType::Fixed(f), _cfg) => {
                    // Without encodings a fixed value carries zero information: this branch only
                    // VERIFIES the constant, so the value it evaluates to is the unit `()`. It is
                    // still a value, and a caller MAY wrap it — the `.cbor` payload arm stages its
                    // target's read into `let {var}_payload = ` / `;` and then USES that binding, so
                    // `[bytes .cbor 42]` reaches here with wrapper text and needs something bound.
                    // Emitting the unit through the caller's wrapper (below, after the match) serves
                    // both: an unwrapped caller gets exactly what it got when this branch asserted
                    // its before/after away, and a wrapping caller gets a well-typed `()`.
                    if config.optional_field {
                        deser_code.content.line("read_len.read_elems(1)?;");
                        deser_code.throws = true;
                        deser_code.read_len_used = true;
                    }
                    match f {
                        FixedValue::Null => {
                            let mut special_block = Block::new(format!(
                                "if {deserializer_name}.special()? != cbor_event::Special::Null"
                            ));
                            special_block
                                .line("return Err(DeserializeFailure::ExpectedNull.into());");
                            deser_code.content.push_block(special_block);
                            if cli.preserve_encodings {
                                // A fixed null/bool contributes no encoding var of its own, but a
                                // WRAPPING path may already have pushed exprs into final_exprs (a
                                // CBOR tag pushes its tag-encoding expr before recursing). Split:
                                // - final_exprs EMPTY: the block's value is the unit `()` — pass it
                                //   explicitly, else the final expr collapses to empty and, under
                                //   `expects_result`, emits `Ok()` (E0061) instead of `Ok(())`.
                                //   (Non-preserve appends `Ok(())` below; preserve produces it here.)
                                // - final_exprs NON-empty: pass None — the value is the encoding
                                //   expr(s) alone (e.g. `Some(tag_enc)` bound to a single
                                //   `let v_tag_encoding = ...`); inserting `()` would mis-shape it
                                //   into `((), Some(tag_enc))` (E0308, seen with
                                //   `[v: #6.1(null), x: uint]`).
                                let unit_if_no_encs =
                                    config.final_exprs.is_empty().then(|| "()".to_owned());
                                // ...and when the value is unit AND nothing consumes it, emit no
                                // value line at all: a block with no tail expression already
                                // evaluates to `()`, while a bare `();` is a `clippy::no_effect`
                                // finding in every consumer's regenerated crate.
                                if !(unit_if_no_encs.is_some() && before_after.discards_value()) {
                                    deser_code.content.line(&format!(
                                        "{}{}{}",
                                        before_after.before_str(false),
                                        final_expr(config.final_exprs, unit_if_no_encs),
                                        before_after.after_str(false)
                                    ));
                                }
                            }
                        }
                        FixedValue::Uint(x) => {
                            if cli.preserve_encodings {
                                deser_code.content.line(&format!(
                                    "let ({}_value, {}_encoding) = {}.unsigned_integer_sz()?;",
                                    config.var_name, config.var_name, deserializer_name
                                ));
                            } else {
                                deser_code.content.line(&format!(
                                    "let {}_value = {}.unsigned_integer()?;",
                                    config.var_name, deserializer_name
                                ));
                            }
                            let mut compare_block =
                                Block::new(format!("if {}_value != {}", config.var_name, x));
                            compare_block.line(format!("return Err(DeserializeFailure::FixedValueMismatch{{ found: Key::Uint({}_value), expected: Key::Uint({}) }}.into());", config.var_name, x));
                            deser_code.content.push_block(compare_block);
                            if cli.preserve_encodings {
                                config
                                    .final_exprs
                                    .push(format!("Some({}_encoding)", config.var_name));
                                deser_code.content.line(&format!(
                                    "{}{}{}",
                                    before_after.before_str(false),
                                    final_expr(config.final_exprs, None),
                                    before_after.after_str(false)
                                ));
                                //body.line(&format!("{}{}{}_encoding{}{}", before, sp, var_name, ep, after));
                            }
                        }
                        FixedValue::Nint(x) => {
                            if cli.preserve_encodings {
                                deser_code.content.line(&format!(
                                    "let ({}_value, {}_encoding) = {}.negative_integer_sz()?;",
                                    config.var_name, config.var_name, deserializer_name
                                ));
                            } else {
                                // we use the _sz variant here too to get around imcomplete nint support in the regular negative_integer()
                                deser_code.content.line(&format!(
                                    "let ({}_value, _) = {}.negative_integer_sz()?;",
                                    config.var_name, deserializer_name
                                ));
                            }
                            let mut compare_block =
                                Block::new(format!("if {}_value != {}", config.var_name, x));
                            // `negative_integer_sz()` yields `(i128, Sz)`, so `{var}_value` is
                            // already `i128` in both profiles and feeds `Key::Nint` directly. Both
                            // sides name the value the CDDL AUTHORED (`-7`), not the nint wire
                            // representation (`-1-N`, i.e. `6`) a u64-only `Key` would have forced.
                            compare_block.line(format!("return Err(DeserializeFailure::FixedValueMismatch{{ found: Key::Nint({}_value), expected: Key::Nint({}) }}.into());", config.var_name, x));
                            deser_code.content.push_block(compare_block);
                            if cli.preserve_encodings {
                                config
                                    .final_exprs
                                    .push(format!("Some({}_encoding)", config.var_name));
                                deser_code.content.line(&format!(
                                    "{}{}{}",
                                    before_after.before_str(false),
                                    final_expr(config.final_exprs, None),
                                    before_after.after_str(false)
                                ));
                                //body.line(&format!("{}{}{}_encoding{}{}", before, sp, var_name, ep, after));
                            }
                        }
                        FixedValue::Text(x) => {
                            if cli.preserve_encodings {
                                deser_code.content.line(&format!(
                                    "let ({}_value, {}_encoding) = {}.text_sz()?;",
                                    config.var_name, config.var_name, deserializer_name
                                ));
                            } else {
                                deser_code.content.line(&format!(
                                    "let {}_value = {}.text()?;",
                                    config.var_name, deserializer_name
                                ));
                            }
                            let mut compare_block = Block::new(format!(
                                "if {}_value != \"{}\"",
                                config.var_name,
                                escape_rust_str(x)
                            ));
                            compare_block.line(format!("return Err(DeserializeFailure::FixedValueMismatch{{ found: Key::Str({}_value), expected: Key::Str(String::from(\"{}\")) }}.into());", config.var_name, escape_rust_str(x)));
                            deser_code.content.push_block(compare_block);
                            if cli.preserve_encodings {
                                config.final_exprs.push(format!(
                                    "StringEncoding::from({}_encoding)",
                                    config.var_name
                                ));
                                deser_code.content.line(&format!(
                                    "{}{}{}",
                                    before_after.before_str(false),
                                    final_expr(config.final_exprs, None),
                                    before_after.after_str(false)
                                ));
                            }
                        }
                        FixedValue::Float(x) => {
                            if cli.preserve_encodings {
                                deser_code.content.line(&format!(
                                    "let ({}_value, {}_encoding) = {}.float_sz()?;",
                                    config.var_name, config.var_name, deserializer_name
                                ));
                            } else {
                                deser_code.content.line(&format!(
                                    "let {}_value = {}.float()?;",
                                    config.var_name, deserializer_name
                                ));
                            }
                            // float_literal, not Display: `{}` on a whole-valued f64 drops the
                            // decimal point (3.0 -> "3"), emitting integer literals in the f64
                            // compare and Key::Float positions (E0308).
                            let mut compare_block = Block::new(format!(
                                "if {}_value != {}",
                                config.var_name,
                                float_fixed_literal(*x)
                            ));
                            compare_block.line(format!("return Err(DeserializeFailure::FixedValueMismatch{{ found: Key::Float({}_value), expected: Key::Float({}) }}.into());", config.var_name, float_fixed_literal(*x)));
                            deser_code.content.push_block(compare_block);
                            if cli.preserve_encodings {
                                config
                                    .final_exprs
                                    .push(format!("Some({}_encoding)", config.var_name));
                                deser_code.content.line(&format!(
                                    "{}{}{}",
                                    before_after.before_str(false),
                                    final_expr(config.final_exprs, None),
                                    before_after.after_str(false)
                                ));
                            }
                        }
                        FixedValue::Bool(b) => {
                            // A bool special has no encoding variation (unlike int/text `_sz`
                            // widths), so — like the Null arm — there is no encoding var to
                            // thread; just verify. `.bool()?` is unambiguous here: statement
                            // position binds the Ok type (bool) and the `?` converts the CBOR
                            // error, the same shape the Uint arm's `.unsigned_integer()?` uses
                            // (the inference hazard the `Primitive::Bool` arm documents only bites
                            // in element/push position).
                            deser_code.content.line(&format!(
                                "let {}_value = {}.bool()?;",
                                config.var_name, deserializer_name
                            ));
                            // `x != true` / `x != false` are `clippy::bool_comparison` findings in
                            // every consumer's regenerated crate (inside this repo's own
                            // `generated_code_clippy_clean` deny set), so spell the mismatch test
                            // as the negation / the identity. Same predicate, and the failure
                            // payload below still names the AUTHORED constant on both sides.
                            let mut compare_block = Block::new(if *b {
                                format!("if !{}_value", config.var_name)
                            } else {
                                format!("if {}_value", config.var_name)
                            });
                            compare_block.line(format!("return Err(DeserializeFailure::FixedValueMismatch{{ found: Key::Bool({}_value), expected: Key::Bool({}) }}.into());", config.var_name, b));
                            deser_code.content.push_block(compare_block);
                            if cli.preserve_encodings {
                                // No encoding var for a bool special, but a wrapping tag may have
                                // pushed into final_exprs — same empty/non-empty split as the
                                // FixedValue::Null arm: unit `()` only when final_exprs is empty
                                // (else `Ok()` E0061); None when non-empty (else `((), tag_enc)`
                                // E0308).
                                let unit_if_no_encs =
                                    config.final_exprs.is_empty().then(|| "()".to_owned());
                                // Same discard suppression as the Null arm: an unconsumed unit
                                // value would emit a degenerate `();` statement.
                                if !(unit_if_no_encs.is_some() && before_after.discards_value()) {
                                    deser_code.content.line(&format!(
                                        "{}{}{}",
                                        before_after.before_str(false),
                                        final_expr(config.final_exprs, unit_if_no_encs),
                                        before_after.after_str(false)
                                    ));
                                }
                            }
                        }
                    };
                    deser_code.throws = true;
                    // The verified constant's value is the unit `()`, emitted through the caller's
                    // wrapper — which yields `Ok(())` for a block that must evaluate to a Result.
                    // Together with the discard suppression inside the helper this reproduces the
                    // previous contract EXACTLY for every caller that passes no wrapper (which, when
                    // this branch asserted its before/after away, was every caller): an empty
                    // `before` with `expects_result` emits `Ok(())`, without it emits nothing.
                    if !cli.preserve_encodings {
                        line_unit_value(&mut deser_code, &before_after);
                    }
                }
                SerializingRustType::Root(ConceptualRustType::Primitive(p), type_cfg) => {
                    if config.optional_field {
                        deser_code.content.line("read_len.read_elems(1)?;");
                        deser_code.read_len_used = true;
                        deser_code.throws = true;
                    }
                    let error_convert = if before_after.expects_result {
                        convert_err_to_ours
                    } else {
                        ""
                    };
                    // `width`: the optional (wmin, wmax) window for a width guard on the value
                    // read — Some only for the narrowing-cast unsigned primitives (u8/u16/u32),
                    // None for every width-safe caller (bytes/text/u64/n64).
                    let mut deser_primitive =
                        |mut final_exprs: Vec<String>,
                         func: &str,
                         x: &str,
                         x_expr: &str,
                         width: Option<(i128, i128)>| {
                            // The nint reader (`negative_integer_sz`) yields the value as `i128`
                            // already, so its RangeCheck `found` needs no widening cast (the unsigned
                            // `func`s read a `u64`, which does).
                            let found_i128 = func == "negative_integer";
                            if cli.preserve_encodings {
                                let enc_expr = match func {
                                    "text" | "bytes" => "StringEncoding::from(enc)",
                                    _ => "Some(enc)",
                                };
                                final_exprs.push(enc_expr.to_owned());
                                let width_fn = width
                                    .map(|(wmin, wmax)| {
                                        width_reject(
                                            &format!("x > {wmax}"),
                                            wmin,
                                            wmax,
                                            "(x, enc)",
                                            "(x, enc)",
                                            !error_convert.is_empty(),
                                            found_i128,
                                        )
                                    })
                                    .unwrap_or_default();
                                let enc_map_fn = match &type_cfg.bounds {
                                    // Convert the error to DeserializeError so the `.and_then`
                                    // closure's `Err(DeserializeFailure::…into())` sees a consistent
                                    // E — but ONLY when no earlier stage of this chain already did.
                                    // The site's `error_convert` and any `width_fn` both leave the
                                    // error type as DeserializeError, so re-converting is a redundant
                                    // identity `From<T> for T`. Same `converted`-flag rule as
                                    // `width_reject`.
                                    Some(bounds) => format!(
                                        "{}.and_then(|({}, enc)| {} else {{ Ok({}) }})",
                                        if error_convert.is_empty() && width_fn.is_empty() {
                                            convert_err_to_ours
                                        } else {
                                            ""
                                        },
                                        x,
                                        bounds_check_if_block(
                                            bounds,
                                            &bounds_check_expr(*p, x),
                                            false,
                                            primitive_non_negative(*p),
                                            None,
                                            found_i128,
                                        ),
                                        final_expr(final_exprs, Some(x_expr.to_owned())),
                                    ),
                                    None => format!(
                                        ".map(|({}, enc)| {})",
                                        x,
                                        final_expr(final_exprs, Some(x_expr.to_owned()))
                                    ),
                                };
                                deser_code.content.line(&format!(
                                    "{}{}.{}_sz(){}{}{}{}",
                                    before_after.before_str(true),
                                    deserializer_name,
                                    func,
                                    error_convert,
                                    width_fn,
                                    enc_map_fn,
                                    before_after.after_str(true)
                                ));
                            } else {
                                let bounds_fn = non_preserve_bounds_fn(*p, x, &type_cfg.bounds);
                                let width_fn = width
                                    .map(|(wmin, wmax)| {
                                        width_reject(
                                            &format!("x > {wmax}"),
                                            wmin,
                                            wmax,
                                            "x",
                                            "x",
                                            !bounds_fn.is_empty(),
                                            found_i128,
                                        )
                                    })
                                    .unwrap_or_default();
                                let cast = match p {
                                    Primitive::U64 | Primitive::Str | Primitive::Bytes => {
                                        Cow::Borrowed("")
                                    }
                                    _ => Cow::Owned(format!(" as {p}")),
                                };
                                deser_code.content.line(&format!(
                                    "{}{}.{}(){}{}?{}{}",
                                    before_after.before_str(false),
                                    deserializer_name,
                                    func,
                                    bounds_fn,
                                    width_fn,
                                    cast,
                                    before_after.after_str(false)
                                ));
                                deser_code.throws = true;
                            }
                        };
                    match p {
                        Primitive::Bytes => {
                            deser_primitive(config.final_exprs, "bytes", "bytes", "bytes", None)
                        }
                        Primitive::U8 | Primitive::U16 | Primitive::U32 => {
                            // The u64 read is wider than the target: width-guard the cast unless
                            // an authored upper bound already caps it.
                            let (wmin, wmax) = prim_window(*p);
                            let width =
                                (!upper_caps(&type_cfg.bounds, wmax)).then_some((wmin, wmax));
                            deser_primitive(
                                config.final_exprs,
                                "unsigned_integer",
                                "x",
                                &format!("x as {}", p),
                                width,
                            )
                        }
                        Primitive::U64 => {
                            deser_primitive(config.final_exprs, "unsigned_integer", "x", "x", None)
                        }
                        Primitive::I8 | Primitive::I16 | Primitive::I32 | Primitive::I64 => {
                            // A signed int splits across two CBOR major types (uint arm / nint arm),
                            // so we classify the value window per arm: a bound may be vacuous here
                            // (drop it), constraining (keep it), or exclude the arm's whole sign
                            // domain (reject unconditionally). The uint arm reads a `u64` and so can
                            // never compare against a negative bound — hence the classification
                            // rather than a raw full-window check.
                            let uint_arm = classify_sign_arm(&type_cfg.bounds, SignArm::Uint);
                            let nint_arm = classify_sign_arm(&type_cfg.bounds, SignArm::Nint);
                            // Width guards for the per-arm narrowing casts: the uint arm reads a
                            // u64 (can exceed the type max — 2^63 would wrap i64 negative) and the
                            // nint readers return i64/i128 (can fall below the type min). Skipped
                            // when the arm's classified check already caps that side.
                            let (wmin, wmax) = prim_window(*p);
                            let uint_width = uint_arm_needs_width(&uint_arm, wmax);
                            let nint_width = nint_arm_needs_width(&nint_arm, wmin);
                            let mut type_check = Block::new(format!(
                                "{}match {}.cbor_type()?",
                                before_after.before_str(false),
                                deserializer_name
                            ));
                            if cli.preserve_encodings {
                                // Fold the accumulated outer-wrapper encoding exprs (e.g. a Tagged
                                // wrapper's `Some(tag_enc)`, a CBORBytes wrapper's StringEncoding)
                                // into the value tuple — as every other primitive path does via
                                // `final_expr`. Both arms MUST emit the same tuple shape. With an
                                // empty `config.final_exprs` this is the byte-identical
                                // `(x as {p}, Some(enc))`; with wrapper exprs it grows to the
                                // 3-tuple the member-level destructure expects (else a preserve-only
                                // E0308).
                                let mut arm_final_exprs = config.final_exprs.clone();
                                arm_final_exprs.push("Some(enc)".to_owned());
                                let arm_tuple =
                                    final_expr(arm_final_exprs, Some(format!("x as {p}")));
                                // `found_i128`: the uint arm reads a `u64` (real widening cast) while
                                // the nint arm reads `negative_integer_sz()` (already `i128` — no cast).
                                let bounds_fn = |arm: &SignArmBounds, found_i128: bool| {
                                    match sign_arm_if_block(arm, "x", false, found_i128) {
                                        // always convert error to have consistent E for the and_then
                                        Some(if_block) => Cow::Owned(format!(
                                            "{}.and_then(|(x, enc)| {} else {{ Ok((x, enc)) }})",
                                            convert_err_to_ours, if_block,
                                        )),
                                        None => Cow::Borrowed(""),
                                    }
                                };
                                let uint_bounds_fn = bounds_fn(&uint_arm, false);
                                let mut pos = Block::new("cbor_event::Type::UnsignedInteger =>");
                                pos.line(format!(
                                    "let (x, enc) = {}.unsigned_integer_sz(){}{}?;",
                                    deserializer_name,
                                    uint_bounds_fn,
                                    if uint_width {
                                        width_reject(
                                            &format!("x > {wmax}"),
                                            wmin,
                                            wmax,
                                            "(x, enc)",
                                            "(x, enc)",
                                            !uint_bounds_fn.is_empty(),
                                            false,
                                        )
                                    } else {
                                        String::new()
                                    }
                                ))
                                .line(&arm_tuple)
                                .after(",");
                                type_check.push_block(pos);
                                // let this cover both the negative int case + error case
                                let nint_bounds_fn = bounds_fn(&nint_arm, true);
                                let mut neg = Block::new("_ =>");
                                neg.line(format!(
                                    "let (x, enc) = {}.negative_integer_sz(){}{}?;",
                                    deserializer_name,
                                    nint_bounds_fn,
                                    if nint_width {
                                        width_reject(
                                            &format!("x < {wmin}"),
                                            wmin,
                                            wmax,
                                            "(x, enc)",
                                            "(x, enc)",
                                            !nint_bounds_fn.is_empty(),
                                            true,
                                        )
                                    } else {
                                        String::new()
                                    }
                                ))
                                .line(&arm_tuple)
                                .after(",");
                                type_check.push_block(neg);
                            } else {
                                // Both arms here read a narrower-than-i128 value (the uint arm a
                                // `u64`, the I8/I16/I32 nint arm an `i64` from `negative_integer()`),
                                // so the widening `as i128` cast is real — never `found_i128`.
                                let non_preserve_arm_fn = |arm: &SignArmBounds, x: &str| {
                                    match sign_arm_if_block(arm, x, false, false) {
                                        // always convert error to have consistent E for the and_then
                                        Some(if_block) => Cow::Owned(format!(
                                            "{}.and_then(|{}| {} else {{ Ok({}) }})",
                                            convert_err_to_ours, x, if_block, x,
                                        )),
                                        None => Cow::Borrowed(""),
                                    }
                                };
                                let uint_arm_fn = non_preserve_arm_fn(&uint_arm, "x");
                                type_check
                                .line(format!(
                                    "cbor_event::Type::UnsignedInteger => {}.unsigned_integer(){}{}? as {},",
                                    deserializer_name,
                                    uint_arm_fn,
                                    if uint_width {
                                        width_reject(&format!("x > {wmax}"), wmin, wmax, "x", "x", !uint_arm_fn.is_empty(), false)
                                    } else {
                                        String::new()
                                    },
                                    p));
                                // negative_integer() reads into i64 and errors on nints below
                                // i64::MIN (upstream's documented pattern is retrying via _sz);
                                // the _sz reader yields i128 across the full nint range, so we use
                                // it directly. It yields the real signed value, so the nint arm
                                // checks the full window directly (no sign partition needed).
                                if *p == Primitive::I64 {
                                    let bounds_fn = match &type_cfg.bounds {
                                        Some(bounds) => Cow::Owned(format!(
                                            "{}.and_then(|(x, _enc)| {} else {{ Ok((x, _enc)) }})",
                                            convert_err_to_ours,
                                            bounds_check_if_block(
                                                bounds,
                                                &bounds_check_expr(*p, "x"),
                                                false,
                                                primitive_non_negative(*p),
                                                None,
                                                // `negative_integer_sz()` yields the value as i128
                                                true,
                                            ),
                                        )),
                                        None => Cow::Borrowed(""),
                                    };
                                    type_check.line(format!(
                                    "_ => {}.negative_integer_sz(){}{}.map(|(x, _enc)| x)? as {},",
                                    deserializer_name, bounds_fn,
                                    if nint_width {
                                        width_reject(&format!("x < {wmin}"), wmin, wmax, "(x, _enc)", "(x, _enc)", !bounds_fn.is_empty(), true)
                                    } else {
                                        String::new()
                                    },
                                    p
                                ));
                                } else {
                                    let nint_arm_fn = non_preserve_arm_fn(&nint_arm, "x");
                                    type_check.line(format!(
                                        "_ => {}.negative_integer(){}{}? as {},",
                                        deserializer_name,
                                        nint_arm_fn,
                                        if nint_width {
                                            width_reject(
                                                &format!("x < {wmin}"),
                                                wmin,
                                                wmax,
                                                "x",
                                                "x",
                                                !nint_arm_fn.is_empty(),
                                                // I8/I16/I32 read `negative_integer()` -> i64: real cast
                                                false,
                                            )
                                        } else {
                                            String::new()
                                        },
                                        p
                                    ));
                                }
                            }
                            type_check.after(before_after.after_str(false));
                            deser_code.content.push_block(type_check);
                            deser_code.throws = true;
                        }
                        Primitive::N64 => {
                            if cli.preserve_encodings {
                                deser_primitive(
                                    config.final_exprs,
                                    "negative_integer",
                                    "x",
                                    // width-safe: the nint domain (-2^64..-1) maps onto the u64
                                    // magnitude exactly, so no guard is needed
                                    "(x + 1).unsigned_abs() as u64",
                                    None,
                                )
                            } else {
                                // negative_integer() reads into i64 and errors on the bottom half
                                // of the nint range (below i64::MIN); the _sz reader yields i128
                                // across the full range, so we use it directly
                                let bounds_fn = match &type_cfg.bounds {
                                    // Convert the read's error to DeserializeError so the `.and_then`
                                    // closure's `Err(DeserializeFailure::…into())` sees a consistent E
                                    // — but ONLY when the site's `error_convert` did not already (it is
                                    // empty under `--annotate-fields=false`, where nothing else on this
                                    // chain converts, so the bare `.and_then` would otherwise infer the
                                    // reader's native `cbor_event::Error` and fail E0277). Same
                                    // convert-at-most-once rule as the I64 nint arm and the bounds fns;
                                    // guarded by `deserialize_converts_error_at_most_once`.
                                    Some(bounds) => Cow::Owned(format!(
                                        "{}.and_then(|(x, _enc)| {} else {{ Ok((x + 1).unsigned_abs() as u64) }})",
                                        if error_convert.is_empty() {
                                            convert_err_to_ours
                                        } else {
                                            ""
                                        },
                                        bounds_check_if_block(
                                            bounds,
                                            &bounds_check_expr(*p, "x"),
                                            false,
                                            primitive_non_negative(*p),
                                            None,
                                            // `negative_integer_sz()` yields the value as i128
                                            true,
                                        ),
                                    )),
                                    None => Cow::Borrowed(
                                        ".map(|(x, _enc)| (x + 1).unsigned_abs() as u64)",
                                    ),
                                };
                                deser_code.content.line(&format!(
                                    "{}{}.negative_integer_sz(){}{}{}",
                                    before_after.before_str(true),
                                    deserializer_name,
                                    error_convert,
                                    bounds_fn,
                                    before_after.after_str(true)
                                ));
                            }
                        }
                        Primitive::Str => {
                            deser_primitive(config.final_exprs, "text", "s", "s", None)
                        }
                        Primitive::Bool => {
                            // no encoding differences for bool. Use `bool::deserialize` (like the
                            // float arms below) rather than `raw.bool().map_err(Into::into)`: the
                            // latter's intermediate error type is unconstrained in element/push
                            // position (`arr.push(<expr>?)`), so with multiple `From<_> for
                            // DeserializeError` impls it fails inference (E0282/E0283) — e.g.
                            // `[* bool]` emitted non-compiling code.
                            deser_code.content.line(&final_result_expr_complete(
                                &mut deser_code.throws,
                                config.final_exprs,
                                &format!("bool::deserialize({deserializer_name})"),
                            ));
                        }
                        Primitive::Float
                        | Primitive::F16
                        | Primitive::F32
                        | Primitive::F64
                        | Primitive::F16To32
                        | Primitive::F32To64 => {
                            // NaN-safe window enforced inline via `and_then` (the value is compared
                            // as f64 so the authored decimal literal is exact). Integer `bounds`
                            // never attach to a float (parsing routes those to float_bounds/reject);
                            // assert it so a routing regression fails loudly instead of silently
                            // skipping enforcement.
                            assert!(
                                type_cfg.bounds.is_none(),
                                "integer bounds on an {p} — parsing must route float constraints to float_bounds"
                            );
                            let is_f32 = p.float_carrier_is_f32();
                            let (min_head, max_head) = p.float_class_window().unwrap();
                            // Width-unconstrained `float` is EVERY float value, so the plain read IS
                            // the whole check — no membership test to emit. Every other class reads
                            // at any head and then tests the decoded VALUE against the window its
                            // CDDL name spans, erroring on a value its own class excludes rather
                            // than accepting one.
                            let unconstrained = *p == Primitive::Float;
                            if cli.preserve_encodings {
                                // The head WIDTH is the float encoding variable, so the preserve read
                                // is `float_sz()` -> `(f64, Sz)` — the same `(value, enc)` tuple shape
                                // every other `_sz` reader yields, so the tail below is `deser_primitive`'s
                                // preserve half with the float window in place of the integer one.
                                //
                                // An f32 member narrows AFTER the read (the CBOR float domain is f64),
                                // and the bounds window is checked on the NARROWED value so a bounded
                                // `float32` accepts/rejects identically in both profiles — the
                                // non-preserve arm below reads through the same helper for the same
                                // reason.
                                let mut final_exprs = config.final_exprs.clone();
                                final_exprs.push("Some(enc)".to_owned());
                                let value_expr = if is_f32 { "narrow_f32(x)" } else { "x" };
                                // The membership-checked read already yields OUR error type (the
                                // check itself is a `DeserializeFailure`), so neither conversion
                                // applies to it — only the bare `float_sz()` read needs converting.
                                let read_expr = if unconstrained {
                                    format!("{deserializer_name}.float_sz(){error_convert}")
                                } else {
                                    format!(
                                        "read_float_sz_width({deserializer_name}, cbor_event::Sz::{min_head}, cbor_event::Sz::{max_head})"
                                    )
                                };
                                let tail = match &type_cfg.float_bounds {
                                    // Convert the read's error to DeserializeError so the `.and_then`
                                    // closure's `Err(DeserializeFailure::…into())` sees a consistent
                                    // E — but ONLY when the site's `error_convert` did not already
                                    // (the convert-at-most-once rule `deserialize_converts_error_at_most_once`
                                    // guards, shared with `deser_primitive`'s bounds arm).
                                    Some(window) => format!(
                                        "{}.and_then(|(x, enc)| {{ let x = {value_expr}; {} else {{ Ok({}) }} }})",
                                        if error_convert.is_empty() && unconstrained {
                                            convert_err_to_ours
                                        } else {
                                            ""
                                        },
                                        bounds_check_if_block_float(
                                            window, is_f32, "x", false, None
                                        ),
                                        final_expr(final_exprs, Some("x".to_owned())),
                                    ),
                                    None => format!(
                                        ".map(|(x, enc)| {})",
                                        final_expr(final_exprs, Some(value_expr.to_owned()))
                                    ),
                                };
                                deser_code.content.line(&format!(
                                    "{}{}{}{}",
                                    before_after.before_str(true),
                                    read_expr,
                                    tail,
                                    before_after.after_str(true)
                                ));
                            } else {
                                // Width-unconstrained `float` IS cbor_event's `f64` blanket impl:
                                // that impl accepts any float head and decoding into `f64` is total
                                // (every CBOR float value is binary64-representable), which is
                                // exactly the vacuous class. So the read is the whole of it and
                                // nothing is emitted around it.
                                //
                                // No other class may use a blanket impl. cbor_event's `f32` impl
                                // asks "is this value binary32-representable" — the NESTED reading
                                // of the CDDL names, under which `1.5` is a `float32`. Our classes
                                // partition instead (`1.5` is a `float16` and not a `float32`), so
                                // all five constrained classes read through the runtime's
                                // membership-checked reader; the `f32`-carried ones narrow after the
                                // check, exactly (never an `as` cast).
                                let read_expr = if unconstrained {
                                    format!("{p}::deserialize({deserializer_name})")
                                } else {
                                    let read = format!(
                                        "read_float_width({deserializer_name}, cbor_event::Sz::{min_head}, cbor_event::Sz::{max_head})"
                                    );
                                    if is_f32 {
                                        format!("{read}.map(narrow_f32)")
                                    } else {
                                        read
                                    }
                                };
                                let result_expr = match &type_cfg.float_bounds {
                                    Some(window) => format!(
                                        "{read_expr}.and_then(|x| {} else {{ Ok(x) }})",
                                        bounds_check_if_block_float(
                                            window, is_f32, "x", false, None
                                        )
                                    ),
                                    None => read_expr,
                                };
                                deser_code.content.line(&final_result_expr_complete(
                                    &mut deser_code.throws,
                                    config.final_exprs,
                                    &result_expr,
                                ));
                            }
                        }
                    };
                }
                // `any` deserializes via `AnyCbor::deserialize` (self-delimiting; leaves the cursor at
                // the item's end). Same composition as a plain Rust struct's `.deserialize()`, minus
                // owner-encoding threading. The type is named through the common-import glue so
                // `--export-static-crate` / `--common-import-override` resolve it (no `use` needed).
                SerializingRustType::Root(ConceptualRustType::Any, _cfg) => {
                    if config.optional_field {
                        deser_code.content.line("read_len.read_elems(1)?;");
                        deser_code.read_len_used = true;
                        deser_code.throws = true;
                    }
                    let final_expr_value = format!(
                        "{}::any_cbor::AnyCbor::deserialize({deserializer_name})",
                        cli.common_import_rust()
                    );
                    deser_code.content.line(&final_result_expr_complete(
                        &mut deser_code.throws,
                        config.final_exprs,
                        &final_expr_value,
                    ));
                }
                SerializingRustType::Root(ConceptualRustType::Rust(ident), type_cfg) => {
                    // check for type-level @custom_deserialize
                    if let Some(custom_deserialize) = &types
                        .rust_struct(ident)
                        .unwrap()
                        .config()
                        .custom_deserialize
                    {
                        // because this is type-level we must handle final_exprs as it could be wrapped in a tag, etc
                        deser_code.content.line(&final_result_expr_complete(
                            &mut deser_code.throws,
                            config.final_exprs,
                            &format!("{}({})", custom_deserialize, deserializer_name),
                        ));
                    } else {
                        match &types.rust_struct(ident).unwrap().variant() {
                            RustStructType::CStyleEnum { variants } => {
                                if config.optional_field {
                                    deser_code.content.line("read_len.read_elems(1)?;");
                                    deser_code.throws = true;
                                    deser_code.read_len_used = true;
                                }
                                // iflet Some(common) = enum_variants_common_constant_type(variants) {
                                //     // TODO: potentially simplified deserialization some day
                                //     // issue: https://github.com/dcSpark/cddl-codegen/issues/145
                                // } else {
                                // A c-style enum has no Deserialize impl of its own: its decode is a
                                // try-each-variant sequence with early `return Ok(Enum::Variant)` + a
                                // trailing NoVariantMatched Err, which only type-checks as the body of a
                                // fn/closure returning `Result<Enum, _>`. When the caller places our
                                // result directly (empty before/after — e.g. a struct field that wraps us
                                // in its own annotate closure, or a type-choice variant's closure) that
                                // body composes as-is. When the caller instead splices our value into a
                                // larger expression (non-empty before/after — the newtype wrapper's
                                // `Ok(Self(<here>))`) the statement form can't be spliced (the early
                                // returns would leak out, dropping the wrapper -> E0308), so we first wrap
                                // it in an immediately-invoked closure to yield a composable
                                // `Result<Enum, _>` expression and let before_after wrap that.
                                //
                                // "Empty before/after composes as-is" holds only while a scaffolding
                                // closure actually exists to catch the early returns, and that is an
                                // `--annotate-fields` property: with the flag off there is no
                                // per-field closure, so a caller wanting a plain value
                                // (`expects_result == false`) splices the statement form straight
                                // into `deserialize()` — every `return Ok(Enum::Variant)` then targets
                                // `deserialize()`'s own return type and the dispatch's `Result` is
                                // left un-`?`ed at its binding (E0308 per variant, plus one at the
                                // binding). So the closure the flag removed is re-supplied here: force
                                // the IIFE for exactly that combination, which leaves every
                                // annotate=true emission byte-identical.
                                let force_iife_for_no_annotate =
                                    !cli.annotate_fields && !before_after.expects_result;
                                let mut enum_body = (!before_after.before.is_empty()
                                    || !before_after.after.is_empty()
                                    || force_iife_for_no_annotate)
                                    .then(|| {
                                        let mut b = Block::new(format!(
                                            "{}(|| -> Result<_, DeserializeError>",
                                            before_after.before_str(true)
                                        ));
                                        b.after(format!(")(){}", before_after.after_str(true)));
                                        b
                                    });
                                {
                                    let target: &mut dyn CodeBlock = match enum_body.as_mut() {
                                        Some(b) => b,
                                        None => &mut deser_code.content,
                                    };
                                    target.line(&format!(
                                        "let initial_position = {deserializer_name}.position();"
                                    ));
                                    // Two DIFFERENT lists, deliberately kept apart. The per-variant
                                    // probe closure below is generated with a FRESH config
                                    // (`make_enum_variant_return_if_deserialized` ->
                                    // `DeserializeConfig::new(variant.name_as_var())`), so its `Ok`
                                    // value carries ONLY this enum's own encoding fields — never the
                                    // exprs a WRAPPING op already pushed into `config.final_exprs`
                                    // (a `bytes .cbor` payload's `StringEncoding::from(..)`, a tag's
                                    // `Some(tag_enc)`). So:
                                    //   - `enc_names` destructures what the closure returns, and is
                                    //     the match PATTERN: plain identifiers, correct arity;
                                    //   - `variant_final_exprs` (outer exprs THEN names) is the
                                    //     returned VALUE, with the outer exprs referenced from
                                    //     enclosing scope where they are already bound (the
                                    //     `let {var}_bytes_encoding` above, the `(tag, tag_enc) =>`
                                    //     arm around us).
                                    // Prefixing the outer exprs into the pattern too made a call expr
                                    // illegal in pattern position (E0164) and the arity wrong even
                                    // when it was legal (E0308) — every wrapped c-style enum under
                                    // `--preserve-encodings` failed to compile.
                                    let mut enc_names = Vec::new();
                                    if cli.preserve_encodings {
                                        for enc_var in encoding_fields(
                                            types,
                                            config.var_name,
                                            variants[0].rust_type(),
                                            false,
                                            cli,
                                        ) {
                                            enc_names.push(enc_var.field_name);
                                        }
                                    }
                                    let mut variant_final_exprs = config.final_exprs.clone();
                                    variant_final_exprs.extend(enc_names.iter().cloned());
                                    for variant in variants {
                                        let mut return_if_deserialized =
                                            make_enum_variant_return_if_deserialized(
                                                self,
                                                types,
                                                variant,
                                                // agrees with the CLOSURE's return arity, not the
                                                // outer list's: this flag makes the closure body end
                                                // in `Ok(())`, which only the `()` pattern below
                                                // matches
                                                enc_names.is_empty(),
                                                None,
                                                target,
                                                deserializer_name,
                                                cli,
                                            );
                                        // pattern parens only for a real tuple (>1), mirroring the
                                        // expression side's final_expr and the non-value enum
                                        // dispatch's names_without_outer.len() > 1 check
                                        let ok_pattern = if enc_names.len() == 1 {
                                            enc_names[0].clone()
                                        } else {
                                            format!("({})", enc_names.join(", "))
                                        };
                                        return_if_deserialized
                            .line(format!("Ok({}) => return Ok({}),",
                            ok_pattern,
                            final_expr(variant_final_exprs.clone(), Some(format!("{}::{}", ident, variant.name)))))
                            .line(format!("Err(_) => {deserializer_name}.set_position(initial_position).unwrap(),"))
                            .after(";");
                                        target.push_block(return_if_deserialized);
                                    }
                                    target.line(&format!(
                        "Err(DeserializeError::new(\"{ident}\", DeserializeFailure::NoVariantMatched))"
                    ));
                                }
                                if let Some(enum_body) = enum_body {
                                    deser_code.content.push_block(enum_body);
                                }
                                if force_iife_for_no_annotate {
                                    // `after_str(true)` appended the `?` that turns the IIFE's
                                    // `Result` back into the plain value the caller asked for.
                                    deser_code.throws = true;
                                }
                            }
                            RustStructType::RawBytesType => {
                                if config.optional_field {
                                    deser_code.content.line("read_len.read_elems(1)?;");
                                    deser_code.throws = true;
                                    deser_code.read_len_used = true;
                                }
                                if cli.preserve_encodings {
                                    // declared spelling BEFORE `final_exprs` is moved out below
                                    let call_target = config.call_target(ident);
                                    config
                                        .final_exprs
                                        .push("StringEncoding::from(enc)".to_owned());
                                    let from_raw_bytes_with_conversions = format!(
                                        "{}::from_raw_bytes(&bytes).map(|bytes| {}).map_err(|e| DeserializeFailure::InvalidStructure(Box::new(e)).into())",
                                        call_target,
                                        final_expr(config.final_exprs, Some("bytes".to_owned()))
                                    );
                                    deser_code.content.line(&format!(
                                        "{}{}.bytes_sz(){}.and_then(|(bytes, enc)| {}){}",
                                        before_after.before_str(true),
                                        deserializer_name,
                                        convert_err_to_ours,
                                        from_raw_bytes_with_conversions,
                                        before_after.after_str(true)
                                    ));
                                } else {
                                    let call_target = config.call_target(ident);
                                    let from_raw_bytes_with_conversions = format!(
                                        "{call_target}::from_raw_bytes(&bytes).map_err(|e| DeserializeFailure::InvalidStructure(Box::new(e)).into())"
                                    );
                                    deser_code.content.line(&format!(
                                        "{}{}.bytes(){}.and_then(|bytes| {}){}",
                                        before_after.before_str(true),
                                        deserializer_name,
                                        convert_err_to_ours,
                                        from_raw_bytes_with_conversions,
                                        before_after.after_str(true)
                                    ));
                                }
                            }
                            // The decode twin of serialize's collection-typedef arms: a named
                            // table/array rule has no `deserialize` of its own (it is a bare rust
                            // typedef onto a collection), so recurse into the collection's
                            // STRUCTURAL conceptual type — the same code the resolved-alias
                            // reference path emits. Reached only from a NOMINAL reference, which
                            // parse-order makes possible when a rule cycle is entered at the
                            // collection rule. `nominal_collection_cfg` reads the per-rule policy
                            // (`@duplicates`, occurrence bounds) back off the struct, which a
                            // nominal reference does not carry.
                            RustStructType::Table { domain, range, .. } => {
                                let structural = ConceptualRustType::Map(
                                    Box::new(domain.clone()),
                                    Box::new(range.clone()),
                                );
                                let cfg = nominal_collection_cfg(types, ident, &type_cfg);
                                return self.generate_deserialize(
                                    types,
                                    SerializingRustType::Root(&structural, cfg),
                                    before_after,
                                    config,
                                    cli,
                                );
                            }
                            RustStructType::Array { element_type, .. } => {
                                let structural =
                                    ConceptualRustType::Array(Box::new(element_type.clone()));
                                let cfg = nominal_collection_cfg(types, ident, &type_cfg);
                                return self.generate_deserialize(
                                    types,
                                    SerializingRustType::Root(&structural, cfg),
                                    before_after,
                                    config,
                                    cli,
                                );
                            }
                            _ => {
                                if types.is_plain_group(ident) && !type_cfg.basic_override {
                                    // This would mess up with length checks otherwise and is probably not a likely situation if this is even valid in CDDL.
                                    // To have this work (if it's valid) you'd either need to generate 2 embedded deserialize methods or pass
                                    // a parameter whether it was an optional field, and if so, read_len.read_elems(embedded mandatory fields)?;
                                    // since otherwise it'd only length check the optional fields within the type.
                                    assert!(!config.optional_field);
                                    deser_code.read_len_used = true;
                                    deser_code.len_used = true;
                                    let final_expr_value = format!(
                                        "{}::deserialize_as_embedded_group({}, {}, len)",
                                        config.call_target(ident),
                                        deserializer_name,
                                        config.pass_read_len()
                                    );

                                    deser_code.content.line(&final_result_expr_complete(
                                        &mut deser_code.throws,
                                        config.final_exprs,
                                        &final_expr_value,
                                    ));
                                } else {
                                    if config.optional_field {
                                        deser_code.content.line("read_len.read_elems(1)?;");
                                        deser_code.read_len_used = true;
                                        deser_code.throws = true;
                                    }
                                    let final_expr_value = format!(
                                        "{}::deserialize({deserializer_name})",
                                        config.call_target(ident)
                                    );
                                    deser_code.content.line(&final_result_expr_complete(
                                        &mut deser_code.throws,
                                        config.final_exprs,
                                        &final_expr_value,
                                    ));
                                }
                            }
                        }
                    }
                }
                SerializingRustType::Root(ConceptualRustType::Optional(ty), _cfg) => {
                    let read_len_check =
                        config.optional_field || (ty.expanded_field_count(types) != Some(1));
                    // codegen crate doesn't support if/else or appending a block after a block, only strings
                    // so we need to create a local bool var and use a match instead
                    let if_label = if ty.cbor_types(types).contains(&cbor_event::Type::Special) {
                        let is_some_check_var = format!("{}_is_some", config.var_name);
                        let mut is_some_check = Block::new(format!(
                            "let {is_some_check_var} = match {deserializer_name}.cbor_type()?"
                        ));
                        let mut special_block = Block::new("cbor_event::Type::Special =>");
                        // `special()` consumes 1-9 bytes depending on the payload (bool/null are 1
                        // byte, two-byte simples 2, f16/f32/f64 are 3/5/9), so this null-peek must
                        // save/restore the position around it rather than rewind a fixed width.
                        special_block.line(format!(
                            "let initial_position = {deserializer_name}.position();"
                        ));
                        special_block
                            .line(format!("let special = {deserializer_name}.special()?;"));
                        special_block.line(format!(
                            "{deserializer_name}.set_position(initial_position).unwrap();"
                        ));
                        let mut special_match = Block::new("match special");
                        // TODO: we need to check that we don't have null / null somewhere
                        special_match.line("cbor_event::Special::Null => false,");
                        // no need to error check - would happen in generated deserialize code
                        special_match.line("_ => true,");
                        special_block.push_block(special_match);
                        special_block.after(",");
                        is_some_check.push_block(special_block);
                        // it's possible the Some case only has Special as its starting tag(s),
                        // but we don't care since it'll fail in either either case anyway,
                        // and would give a good enough error (ie expected Special::X but found non-Special)
                        is_some_check.line("_ => true,");
                        is_some_check.after(";");
                        deser_code.content.push_block(is_some_check);
                        is_some_check_var
                    } else {
                        String::from(&format!(
                            "{deserializer_name}.cbor_type()? != cbor_event::Type::Special"
                        ))
                    };
                    let mut deser_block = Block::new(format!(
                        "{}match {}",
                        before_after.before_str(false),
                        if_label
                    ));
                    let mut some_block = Block::new("true =>");
                    if read_len_check {
                        let mandatory_fields = ty.expanded_mandatory_field_count(types);
                        if mandatory_fields != 0 {
                            some_block.line(format!("read_len.read_elems({mandatory_fields})?;"));
                            deser_code.read_len_used = true;
                        }
                    }
                    let ty_enc_fields = if cli.preserve_encodings {
                        encoding_fields(types, config.var_name, ty, false, cli)
                    } else {
                        vec![]
                    };
                    // Every tag level already crossed on THIS member name contributed its own
                    // element to the tuple the child read produces (`(x, tag_enc, inner_enc)` for
                    // one level), so the `Some`-mapping below has to bind them too and the `None`
                    // arm has to default them. Deriving the count from `config.tag_depth` — the
                    // same counter `encoding_fields_impl` and the tag arms use — is what keeps the
                    // three in lockstep. Before this, `#6.n(T / null)` under `--preserve-encodings`
                    // emitted a 2-binder pattern against a 3-element tuple and the generated crate
                    // did not compile (E0308) — exit 0, uncompilable output.
                    // Only under `--preserve-encodings`: without it no encoding variable exists at
                    // any level, so the child read yields the bare value and the tuple has one
                    // element whatever the tag depth.
                    let crossed_tag_levels = if cli.preserve_encodings {
                        config.tag_depth
                    } else {
                        0
                    };
                    let crossed_tag_vars: Vec<String> = (1..=crossed_tag_levels)
                        .map(|level| {
                            format!("{}_{}_encoding", config.var_name, tag_encoding_infix(level))
                        })
                        .collect();
                    if ty_enc_fields.is_empty() && crossed_tag_vars.is_empty() {
                        self.generate_deserialize(
                            types,
                            (&**ty).into(),
                            DeserializeBeforeAfter::new("Some(", ")", false),
                            // an `Optional` inner is a member position of its OWN
                            config.optional_field(false).clear_declared_spelling(),
                            cli,
                        )
                        .add_to(&mut some_block);
                    } else {
                        let (map_some_before, map_some_after) = if ty.is_fixed_value() {
                            // case 1: no actual return, only encoding values for tags/fixed values, no need to wrap in Some()
                            ("", "".to_owned())
                        } else {
                            // case 2: need to map FIRST element in Some(x)
                            let enc_vars_str = crossed_tag_vars
                                .iter()
                                .cloned()
                                .chain(
                                    ty_enc_fields
                                        .iter()
                                        .map(|enc_field| enc_field.field_name.clone()),
                                )
                                .collect::<Vec<String>>()
                                .join(", ");
                            // we need to annotate the Ok's error type since the compiler gets confused otherwise
                            (
                                "Result::<_, DeserializeError>::Ok(",
                                format!(").map(|(x, {enc_vars_str})| (Some(x), {enc_vars_str}))?"),
                            )
                        };
                        self.generate_deserialize(
                            types,
                            (&**ty).into(),
                            DeserializeBeforeAfter::new(map_some_before, &map_some_after, false),
                            // an `Optional` inner is a member position of its OWN
                            config.optional_field(false).clear_declared_spelling(),
                            cli,
                        )
                        .add_to(&mut some_block);
                    }
                    some_block.after(",");
                    deser_block.push_block(some_block);
                    let mut none_block = Block::new("false =>");
                    if read_len_check {
                        none_block.line("read_len.read_elems(1)?;");
                        deser_code.read_len_used = true;
                    }
                    // we don't use this to avoid the new (true) if cli.preserve_encodings is set
                    //self.generate_deserialize(types, &ConceptualRustType::Fixed(FixedValue::Null), var_name, "", "", in_embedded, false, add_parens, &mut none_block);
                    let mut check_null = Block::new(format!(
                        "if {deserializer_name}.special()? != cbor_event::Special::Null"
                    ));
                    check_null.line("return Err(DeserializeFailure::ExpectedNull.into());");
                    none_block.push_block(check_null);
                    if cli.preserve_encodings {
                        let mut none_elems = if ty.is_fixed_value() {
                            vec![]
                        } else {
                            vec!["None".to_owned()]
                        };
                        // The already-read head size for each crossed tag level, then the inner
                        // type's own defaults — same order as the `Some` arm's binders.
                        //
                        // The tag levels are NOT defaulted, and that asymmetry with the slots
                        // around them is the whole point: a crossed tag's head is bytes this arm
                        // has ALREADY consumed (the enclosing `match .tag_sz()?` bound them to
                        // `tag_enc`, which is in scope here), so defaulting them to `None` made a
                        // widened `d8 0a f6` re-encode as `ca f6` — a preserve-encodings violation
                        // at exit 0. The value slot and the inner type's own encoding slots stay
                        // defaulted BECAUSE the bytes they describe were never on the wire: the
                        // payload is null. Pinned by tests/corpus/tagged_nullable.cddl in both the
                        // rule-body and member positions.
                        none_elems.extend(
                            (1..=crossed_tag_levels)
                                .map(|level| format!("Some({})", tag_enc_binding(level))),
                        );
                        none_elems.extend(
                            ty_enc_fields
                                .iter()
                                .map(|enc_field| enc_field.default_expr.to_owned()),
                        );
                        match none_elems.len() {
                            // this probably isn't properly supported by other parts of code and is so unlikely to be encountered
                            // that we really don't care right now. if you run into this open an issue and it can be investigated
                            0 => unimplemented!("please open a github issue"),
                            1 => none_block.line(none_elems.first().unwrap()),
                            _ => none_block.line(format!("({})", none_elems.join(", "))),
                        };
                    } else {
                        none_block.line("None");
                    }
                    deser_block.after(before_after.after_str(false));
                    deser_block.push_block(none_block);
                    deser_code.content.push_block(deser_block);
                    deser_code.throws = true;
                }
                SerializingRustType::Root(ConceptualRustType::Array(ty), type_cfg) => {
                    if config.optional_field {
                        deser_code.content.line("read_len.read_elems(1)?;");
                        deser_code.read_len_used = true;
                    }
                    let arr_var_name = format!("{}_arr", config.var_name);
                    deser_code
                        .content
                        .line(&format!("let mut {arr_var_name} = Vec::new();"));
                    let elem_var_name = format!("{}_elem", config.var_name);
                    let elem_encs = if cli.preserve_encodings {
                        encoding_fields(types, &elem_var_name, ty, false, cli)
                    } else {
                        vec![]
                    };
                    if cli.preserve_encodings {
                        deser_code
                            .content
                            .line(&format!("let len = {deserializer_name}.array_sz()?;"))
                            .line(&format!("let {}_encoding = len.into();", config.var_name));
                        if !elem_encs.is_empty() {
                            deser_code.content.line(&format!(
                                "let mut {}_elem_encodings = Vec::new();",
                                config.var_name
                            ));
                        }
                    } else {
                        deser_code
                            .content
                            .line(&format!("let len = {deserializer_name}.array()?;"));
                    }
                    // FRESH config, not the outer member's: an array element is a member position of
                    // its own (it spells from its own declaration, via its own `Alias` arm) and must
                    // not inherit the outer member's declared spelling. Same for the map arm's
                    // key/value configs below.
                    let mut elem_config = DeserializeConfig::new(&elem_var_name);
                    // `is_basic` and NOT a bare `is_plain_group`: a plain group SPLICES into this
                    // array only while it is still basic. A wrapper promotes it to a struct that
                    // writes its own array header (`[* [coords]]`), and then the element is ONE
                    // outer item read through the standalone `deserialize` — which is what the two
                    // emitters either side of this one already decide with `is_basic`: the element
                    // READ's embedded-vs-standalone face (the `Rust(ident)` arm's
                    // `is_plain_group(ident) && !type_cfg.basic_override`) and the SERIALIZE
                    // length (`expanded_field_count`, which consults `is_basic` internally and so
                    // writes `1 * n` for the promoted form). Asking the bare predicate here charged
                    // the group's field count against an outer slot the element did not occupy, so
                    // the crate emitted bytes its own decoder rejected with `DefiniteLenMismatch` —
                    // exit 0, compiles, round-trip red. Pinned by
                    // tests/corpus/array_of_wrapped_group.cddl, which spells both forms plus the
                    // map key/value controls (a table counts ENTRIES, so no expansion can be
                    // charged to it and the array arm is the only site with this hazard).
                    // The scrutinee resolves aliases for the same reason the guard already does: an
                    // alias is transparent, so an element spelled `kv_alias` splices exactly like
                    // `kv`. Matching the bare type left the alias spelling on the non-embedded
                    // branch, whose loop counts ELEMENTS against a header the serializer wrote in
                    // ITEMS — the write/read halves of one crate disagreeing. Same resolution as the
                    // serialize length expression this arm is paired with.
                    let (mut deser_loop, plain_len_check) = match ty
                        .conceptual_type
                        .resolve_alias_shallow()
                    {
                        ConceptualRustType::Rust(_) if ty.is_basic(types) => {
                            // two things that must be done differently for embedded plain groups:
                            // 1) We can't directly read the CBOR len's number of items since it could be >1
                            // 2) We need a different cbor read len var to pass into embedded deserialize
                            let read_len_overload = format!("{}_read_len", config.var_name);
                            deser_code.content.line(&format!(
                                "let mut {read_len_overload} = {}(len);",
                                cbor_read_len_ctor(cli)
                            ));
                            // inside of deserialize_as_embedded_group we only modify read_len for things we couldn't
                            // statically know beforehand. This was done for other areas that use plain groups in order
                            // to be able to do static length checks for statically sized groups that contain plain groups
                            // at the start of deserialization instead of many checks for every single field.
                            let plain_len_check = match ty.expanded_mandatory_field_count(types) {
                                0 => None,
                                n => Some(format!("{read_len_overload}.read_elems({n})?;")),
                            };
                            elem_config = elem_config.overload_read_len(read_len_overload);
                            let deser_loop = make_deser_loop(
                                "len",
                                &format!("{}_read_len.read()", config.var_name),
                                cli,
                            );
                            (deser_loop, plain_len_check)
                        }
                        _ => (
                            make_deser_loop("len", &format!("({arr_var_name}.len() as u64)"), cli),
                            None,
                        ),
                    };
                    deser_loop.push_block(make_deser_loop_break_check(
                        "len",
                        deserializer_name,
                        cli,
                    ));
                    if let Some(plain_len_check) = plain_len_check {
                        deser_loop.line(plain_len_check);
                    }
                    elem_config.deserializer_name_overload = config.deserializer_name_overload;
                    if !elem_encs.is_empty() {
                        let elem_var_names_str =
                            encoding_var_names_str(types, &elem_var_name, ty, cli);
                        self.generate_deserialize(
                            types,
                            (&**ty).into(),
                            DeserializeBeforeAfter::new(
                                &format!("let {elem_var_names_str} = "),
                                ";",
                                false,
                            ),
                            elem_config,
                            cli,
                        )
                        .add_to(&mut deser_loop);
                        deser_loop
                            .line(format!("{arr_var_name}.push({elem_var_name});"))
                            .line(format!(
                                "{}_elem_encodings.push({});",
                                config.var_name,
                                tuple_str(
                                    elem_encs.iter().map(|enc| enc.field_name.clone()).collect()
                                )
                            ));
                    } else {
                        self.generate_deserialize(
                            types,
                            (&**ty).into(),
                            DeserializeBeforeAfter::new(
                                &format!("{arr_var_name}.push("),
                                ");",
                                false,
                            ),
                            elem_config,
                            cli,
                        )
                        .add_to(&mut deser_loop);
                    }
                    deser_code.content.push_block(deser_loop);
                    let reject_dups =
                        type_cfg.duplicates == Some(crate::comment_ast::DuplicatesPolicy::Reject);
                    if reject_dups {
                        // `@duplicates reject`: route the collected Vec through the SAME uniqueness
                        // twin `TryFrom` door the API uses, so a duplicate on the wire and a duplicate
                        // built through the API report the identical `DuplicateKey(index)` error and
                        // can never drift. The non-empty flavor's door additionally enforces the `[+]`
                        // min-1 bound (same composed door). Encoding vars stay keyed off the field.
                        let twin = if type_cfg.bounds == Some((Some(1), None)) {
                            "NonEmptyOrderedSet"
                        } else {
                            "OrderedSet"
                        };
                        deser_code.content.line(&format!(
                            "let {arr_var_name} = {twin}::try_from({arr_var_name})?;"
                        ));
                        // A non-`+` reject set may still carry OTHER occurrence bounds (`2*5` etc);
                        // those stay a runtime length check on the accepted (unique) collection.
                        if type_cfg.bounds != Some((Some(1), None))
                            && let Some(bounds) = &type_cfg.bounds
                        {
                            deser_code.content.line(&bounds_check_if_block(
                                bounds,
                                &format!("{arr_var_name}.len()"),
                                true,
                                true,
                                None,
                                // `.len()` is usize — the widening cast is real
                                false,
                            ));
                        }
                    } else if type_cfg.bounds == Some((Some(1), None)) {
                        // `[+ T]`: route the collected Vec through the SAME `TryFrom` door the API
                        // uses, so the wire side and API side report the identical RangeCheck error
                        // ("0 not at least 1") and can never drift. The encoding vars stay keyed off
                        // the field (untouched below) — only the value var is rebound.
                        deser_code.content.line(&format!(
                            "let {arr_var_name} = NonEmptyVec::try_from({arr_var_name})?;"
                        ));
                    } else if let Some(bounds) = &type_cfg.bounds {
                        // we use cargo fmt after so it's okay if we just use .line() here
                        deser_code.content.line(&bounds_check_if_block(
                            bounds,
                            &format!("{arr_var_name}.len()"),
                            true,
                            true,
                            None,
                            // `.len()` is usize — the widening cast is real
                            false,
                        ));
                    }
                    if cli.preserve_encodings {
                        config
                            .final_exprs
                            .push(format!("{}_encoding", config.var_name));
                        if !elem_encs.is_empty() {
                            config
                                .final_exprs
                                .push(format!("{}_elem_encodings", config.var_name));
                        }
                        deser_code.content.line(&format!(
                            "{}{}{}",
                            before_after.before_str(false),
                            final_expr(config.final_exprs, Some(arr_var_name)),
                            before_after.after_str(false)
                        ));
                    } else {
                        deser_code.content.line(&format!(
                            "{}{}{}",
                            before_after.before_str(false),
                            arr_var_name,
                            before_after.after_str(false)
                        ));
                    }
                    deser_code.throws = true;
                }
                SerializingRustType::Root(
                    ConceptualRustType::Map(key_type, value_type),
                    type_cfg,
                ) => {
                    if config.optional_field {
                        deser_code.content.line("read_len.read_elems(1)?;");
                        deser_code.read_len_used = true;
                    }
                    if !self.deserialize_generated_for_type(&key_type.conceptual_type) {
                        todo!();
                        // TODO: where is the best place to check for this? should we pass in a RustIdent to say where we're generating?!
                        //self.dont_generate_deserialize(name, format!("key type {} doesn't support deserialize", key_type.for_rust_member()));
                    } else if !self.deserialize_generated_for_type(&value_type.conceptual_type) {
                        todo!();
                        //self.dont_generate_deserialize(name, format!("value type {} doesn't support deserialize", value_type.for_rust_member()));
                    } else {
                        // `@duplicates preserve` (the pair-map twin): collect into a `Vec<(K, V)>`
                        // (the only shape that can hold duplicate keys) rather than the loose keyed
                        // table, skip the wire-side dup-check, and carry a POSITIONAL encoding sidecar
                        // parallel to the entries (a `BTreeMap` keyed by key value cannot hold two
                        // same-key entries). Everything else in the loop is shared.
                        let preserve_pair_map = type_cfg.duplicates
                            == Some(crate::comment_ast::DuplicatesPolicy::Preserve);
                        let table_var = format!("{}_table", config.var_name);
                        if preserve_pair_map {
                            deser_code
                                .content
                                .line(&format!("let mut {table_var} = Vec::new();"));
                        } else {
                            deser_code.content.line(&format!(
                                "let mut {} = {}::new();",
                                table_var,
                                table_type(cli)
                            ));
                        }
                        let key_var_name = format!("{}_key", config.var_name);
                        let value_var_name = format!("{}_value", config.var_name);
                        let key_encs = if cli.preserve_encodings {
                            encoding_fields(types, &key_var_name, key_type, false, cli)
                        } else {
                            vec![]
                        };
                        let value_encs = if cli.preserve_encodings {
                            encoding_fields(types, &value_var_name, value_type, false, cli)
                        } else {
                            vec![]
                        };
                        let len_var = format!("{}_len", config.var_name);
                        if cli.preserve_encodings {
                            deser_code
                                .content
                                .line(&format!("let {len_var} = {deserializer_name}.map_sz()?;"))
                                .line(&format!(
                                    "let {}_encoding = {}.into();",
                                    config.var_name, len_var
                                ));
                            let encodings_ctor = if preserve_pair_map {
                                "Vec::new()"
                            } else {
                                "BTreeMap::new()"
                            };
                            if !key_encs.is_empty() {
                                deser_code.content.line(&format!(
                                    "let mut {}_key_encodings = {encodings_ctor};",
                                    config.var_name
                                ));
                            }
                            if !value_encs.is_empty() {
                                deser_code.content.line(&format!(
                                    "let mut {}_value_encodings = {encodings_ctor};",
                                    config.var_name
                                ));
                            }
                        } else {
                            deser_code
                                .content
                                .line(&format!("let {len_var} = {deserializer_name}.map()?;"));
                        }
                        let mut deser_loop =
                            make_deser_loop(&len_var, &format!("({table_var}.len() as u64)"), cli);
                        deser_loop.push_block(make_deser_loop_break_check(
                            &len_var,
                            deserializer_name,
                            cli,
                        ));
                        let mut key_config = DeserializeConfig::new(&key_var_name);
                        key_config.deserializer_name_overload = config.deserializer_name_overload;
                        let mut value_config = DeserializeConfig::new(&value_var_name);
                        value_config.deserializer_name_overload = config.deserializer_name_overload;
                        let (key_var_names_str, value_var_names_str) = if cli.preserve_encodings {
                            (
                                encoding_var_names_str(types, &key_var_name, key_type, cli),
                                encoding_var_names_str(types, &value_var_name, value_type, cli),
                            )
                        } else {
                            (key_var_name.clone(), value_var_name.clone())
                        };
                        self.generate_deserialize(
                            types,
                            (&**key_type).into(),
                            DeserializeBeforeAfter::new(
                                &format!("let {key_var_names_str} = "),
                                ";",
                                false,
                            ),
                            key_config,
                            cli,
                        )
                        .add_to(&mut deser_loop);
                        self.generate_deserialize(
                            types,
                            (&**value_type).into(),
                            DeserializeBeforeAfter::new(
                                &format!("let {value_var_names_str} = "),
                                ";",
                                false,
                            ),
                            value_config,
                            cli,
                        )
                        .add_to(&mut deser_loop);
                        if preserve_pair_map {
                            // `@duplicates preserve`: append EVERY entry (duplicate keys included) —
                            // no dup-check, no `DuplicateKey` (that is the reject-mode path only). The
                            // key value is moved into the pair here; the positional encoding pushes
                            // below re-derive their tuples from the already-bound encoding vars, so
                            // they never touch the moved key value (unlike the loose table, which keys
                            // its encoding maps by the key VALUE and so must clone it).
                            deser_loop.line(format!(
                                "{table_var}.push(({key_var_name}, {value_var_name}));"
                            ));
                            if cli.preserve_encodings {
                                if !key_encs.is_empty() {
                                    deser_loop.line(format!(
                                        "{}_key_encodings.push({});",
                                        config.var_name,
                                        tuple_str(
                                            key_encs
                                                .iter()
                                                .map(|enc| enc.field_name.clone())
                                                .collect()
                                        )
                                    ));
                                }
                                if !value_encs.is_empty() {
                                    deser_loop.line(format!(
                                        "{}_value_encodings.push({});",
                                        config.var_name,
                                        tuple_str(
                                            value_encs
                                                .iter()
                                                .map(|enc| enc.field_name.clone())
                                                .collect()
                                        )
                                    ));
                                }
                            }
                            deser_code.content.push_block(deser_loop);
                            if type_cfg.bounds == Some((Some(1), None)) {
                                // `{+ k => v}` preserve: the min-1 door composes non-emptiness with the
                                // vec-of-pairs, routed through the SAME `TryFrom` the API uses so the
                                // wire/API RangeCheck errors are identical.
                                deser_code.content.line(&format!(
                                    "let {table_var} = NonEmptyPairMap::try_from({table_var})?;"
                                ));
                            } else {
                                // `{* k => v}` preserve: any vec of pairs is valid (infallible `From`).
                                deser_code.content.line(&format!(
                                    "let {table_var} = PairMap::from({table_var});"
                                ));
                                // A non-`+` preserve table may still carry OTHER occurrence bounds
                                // (`2*5` etc); those stay a runtime length check on the pair-map.
                                if let Some(bounds) = &type_cfg.bounds {
                                    deser_code.content.line(&bounds_check_if_block(
                                        bounds,
                                        &format!("{table_var}.len()"),
                                        true,
                                        true,
                                        None,
                                        // `.len()` is usize — the widening cast is real
                                        false,
                                    ));
                                }
                            }
                            if cli.preserve_encodings {
                                config
                                    .final_exprs
                                    .push(format!("{}_encoding", config.var_name));
                                if !key_encs.is_empty() {
                                    config
                                        .final_exprs
                                        .push(format!("{}_key_encodings", config.var_name));
                                }
                                if !value_encs.is_empty() {
                                    config
                                        .final_exprs
                                        .push(format!("{}_value_encodings", config.var_name));
                                }
                            }
                            deser_code.content.line(&format!(
                                "{}{}{}",
                                before_after.before_str(false),
                                final_expr(config.final_exprs, Some(table_var)),
                                before_after.after_str(false)
                            ));
                            deser_code.throws = true;
                            return deser_code;
                        }
                        let mut dup_check = Block::new(format!(
                            "if {}.insert({}{}, {}).is_some()",
                            table_var,
                            key_var_name,
                            if key_type.is_copy(types) {
                                ""
                            } else {
                                ".clone()"
                            },
                            value_var_name
                        ));
                        let dup_key_error_key = match &key_type.conceptual_type {
                            ConceptualRustType::Primitive(Primitive::U8)
                            | ConceptualRustType::Primitive(Primitive::U16)
                            | ConceptualRustType::Primitive(Primitive::U32) => {
                                format!("Key::Uint({key_var_name}.into())")
                            }
                            ConceptualRustType::Primitive(Primitive::U64) => {
                                format!("Key::Uint({key_var_name})")
                            }
                            ConceptualRustType::Primitive(Primitive::Str) => {
                                format!("Key::Str({key_var_name})")
                            }
                            // TODO: make a generic one then store serialized CBOR?
                            _ => "Key::Str(String::from(\"some complicated/unsupported type\"))"
                                .to_owned(),
                        };
                        dup_check.line(format!(
                        "return Err(DeserializeFailure::DuplicateKey({dup_key_error_key}).into());"
                    ));
                        deser_loop.push_block(dup_check);
                        if cli.preserve_encodings {
                            if !key_encs.is_empty() {
                                deser_loop.line(format!(
                                    "{}_key_encodings.insert({}{}, {});",
                                    config.var_name,
                                    key_var_name,
                                    // The inserted expr is the key VALUE, so gate the clone on the
                                    // key value's copy-ness (matching the adjacent dup-check block),
                                    // NOT its encoding var's — a composite (e.g. array) key value is
                                    // a non-Copy Vec even though its length-encoding var is Copy, so
                                    // moving it here then reusing it below is a preserve-only E0382.
                                    if key_type.is_copy(types) {
                                        ""
                                    } else {
                                        ".clone()"
                                    },
                                    tuple_str(
                                        key_encs.iter().map(|enc| enc.field_name.clone()).collect()
                                    )
                                ));
                            }
                            if !value_encs.is_empty() {
                                deser_loop.line(format!(
                                    "{}_value_encodings.insert({}{}, {});",
                                    config.var_name,
                                    key_var_name,
                                    // Same as the key-encoding insert: the map is keyed by the key
                                    // VALUE, so gate its clone on the value's copy-ness, not the
                                    // encoding var's.
                                    if key_type.is_copy(types) {
                                        ""
                                    } else {
                                        ".clone()"
                                    },
                                    tuple_str(
                                        value_encs
                                            .iter()
                                            .map(|enc| enc.field_name.clone())
                                            .collect()
                                    )
                                ));
                            }
                        }
                        deser_code.content.push_block(deser_loop);
                        if type_cfg.bounds == Some((Some(1), None)) {
                            // `{+ k => v}`: route the collected map through the SAME `TryFrom` door the
                            // API uses, so the wire side and API side report the identical RangeCheck
                            // error ("0 not at least 1") and can never drift. The encoding vars stay
                            // keyed off the field (untouched below) — only the value var is rebound.
                            deser_code.content.line(&format!(
                                "let {table_var} = NonEmptyMap::try_from({table_var})?;"
                            ));
                        } else if let Some(bounds) = &type_cfg.bounds {
                            // we use cargo fmt after so it's okay if we just use .line() here
                            deser_code.content.line(&bounds_check_if_block(
                                bounds,
                                &format!("{table_var}.len()"),
                                true,
                                true,
                                None,
                                // `.len()` is usize — the widening cast is real
                                false,
                            ));
                        }
                        if cli.preserve_encodings {
                            config
                                .final_exprs
                                .push(format!("{}_encoding", config.var_name));
                            if !key_encs.is_empty() {
                                config
                                    .final_exprs
                                    .push(format!("{}_key_encodings", config.var_name));
                            }
                            if !value_encs.is_empty() {
                                config
                                    .final_exprs
                                    .push(format!("{}_value_encodings", config.var_name));
                            }
                            deser_code.content.line(&format!(
                                "{}{}{}",
                                before_after.before_str(false),
                                final_expr(config.final_exprs, Some(table_var)),
                                before_after.after_str(false)
                            ));
                        } else {
                            deser_code.content.line(&format!(
                                "{}{}{}",
                                before_after.before_str(false),
                                table_var,
                                before_after.after_str(false)
                            ));
                        }
                    }
                    deser_code.throws = true;
                }
                SerializingRustType::Root(ConceptualRustType::Alias(ident, ty), cfg) => {
                    let alias_info = types.type_aliases().get(ident).unwrap();
                    let config_for_alias = if let Some(custom_deserialize) = alias_info
                        .rule_metadata
                        .as_ref()
                        .and_then(|rmd| rmd.custom_deserialize.clone())
                    {
                        // The rule's `@custom_encodings` rides with the pair it is written beside —
                        // see the serialize twin for the two-channel split.
                        config
                            .custom_deserialize(custom_deserialize)
                            .custom_encodings(
                                alias_info
                                    .rule_metadata
                                    .as_ref()
                                    .and_then(|rmd| rmd.custom_encodings.clone()),
                            )
                    } else {
                        config
                    };
                    // The member's DECLARED spelling, for the call targets below (a field
                    // `sc: StakeCredential` is filled by `StakeCredential::deserialize`, not by the
                    // alias's structural target). Same lift-from-the-alias shape as the
                    // `custom_deserialize` above: the alias influences how its target is emitted.
                    // `AliasIdent::Reserved` is excluded because a prelude name is not a rust ident.
                    //
                    // The lift is gated on WHO OWNS the encoding operations this descent crossed,
                    // which is readable HERE and nowhere else. A `RustType`'s operations wrap its
                    // conceptual type (`SerializingRustType::from`), so this arm is reached only
                    // AFTER every one of them is consumed — and the two ownerships need opposite
                    // answers at that same point:
                    //
                    //   * The alias RULE owns them (`cred_bytes = bytes .cbor credential`, whose
                    //     `base_type` carries the `CBORBytes`). Then the ident names the WRAPPED
                    //     form, so it does not denote this position at all: `CredBytes` IS the
                    //     bytes-wrapped thing, and `CredBytes::deserialize(inner_de)` compiles (the
                    //     alias is transparent) while claiming to read a bytes-wrapped value where
                    //     the payload is read. Do not lift.
                    //   * The MEMBER's own type expression owns them (`f: #6.9(stake_credential)`,
                    //     an alias with a bare `base_type` plus a tag pushed on at the reference).
                    //     Then the ident denotes exactly the value being read here, and the field is
                    //     typed `StakeCredential` — so lifting is what makes the call target agree
                    //     with the field rather than what breaks it.
                    //
                    // Testing "did this descent cross an operation" instead cannot tell those apart
                    // and gets the second one wrong. A member-expression tag over a rule-owned
                    // `.cbor` (`#6.9(cred_bytes)`) crosses both and must still seal — which it does,
                    // on the RULE's account, because the test reads only the rule.
                    let config_for_alias = match ident {
                        AliasIdent::Rust(rust_ident)
                            if alias_info.base_type.encodings.is_empty() =>
                        {
                            config_for_alias.declare_spelling(rust_ident.to_string())
                        }
                        AliasIdent::Rust(_) | AliasIdent::Reserved(_) => config_for_alias,
                    };
                    // keep the OUTER config: an Alias's inner is a bare ConceptualRustType (no
                    // config of its own — see `as_alias`), so recursing with `(&**ty).into()`
                    // would default the config and drop e.g. the occurrence-count bounds a named
                    // array alias carries (its length check would silently vanish here while the
                    // constructor check, emitted from the field's RustType, kept working)
                    self.generate_deserialize(
                        types,
                        SerializingRustType::Root(ty, cfg),
                        before_after,
                        config_for_alias,
                        cli,
                    )
                    .add_to_code(&mut deser_code);
                }
                SerializingRustType::EncodingOperation(CBOREncodingOperation::CBORBytes, child) => {
                    // The byte string HOLDING the payload is itself an item of whatever stream this
                    // arm was reached through, so it is read from `deserializer_name` like every
                    // sibling arm — not unconditionally from `raw`. The two differ exactly when a
                    // `.cbor` payload is nested inside another one's collection loop: there the
                    // enclosing reader is the outer payload's `inner_de` overload, and naming `raw`
                    // consumed the next OUTER item as the element's payload. Only the decode side
                    // was affected (the serializer writes the nested payload correctly), so the
                    // shape compiled, encoded to spec, and then rejected its own output.
                    // level (cbor_depth + 1) counted outside-in, in lockstep with
                    // `encoding_fields_impl`: the byte vector, its encoding var, the reader over it
                    // and the staging local all take the level's names, and the payload recurses one
                    // level deeper. Level 1 keeps the historical spellings.
                    let cbor_level = config.cbor_depth + 1;
                    let bytes_infix = cbor_bytes_infix(cbor_level);
                    let bytes_local = format!("{}_{}", config.var_name, bytes_infix);
                    if cli.preserve_encodings {
                        config
                            .final_exprs
                            .push(format!("StringEncoding::from({bytes_local}_encoding)"));
                        deser_code.content.line(&format!(
                            "let ({bytes_local}, {bytes_local}_encoding) = {deserializer_name}.bytes_sz()?;"
                        ));
                    } else {
                        deser_code.content.line(&format!(
                            "let {bytes_local} = {deserializer_name}.bytes()?;"
                        ));
                    };
                    // Shadowing `inner_de` is safe for the nested-COLLECTION shape because the inner
                    // rebind is scoped to the loop-BODY block: the next iteration's length/break
                    // probe and the map arm's key read both sit before it and still see the enclosing
                    // payload's reader — and that element is a fresh member name, so its level
                    // restarts at 1 and the historical spelling is kept. DIRECT nesting within one op
                    // chain has no such block: both readers live in one scope, and the outer's
                    // leftover-bytes check below would silently re-probe the INNER reader. Hence the
                    // level suffix, which is the only thing separating the two here.
                    let name_overload = cbor_payload_reader(cbor_level);
                    deser_code.content.line(&format!(
                        "let {name_overload} = &mut Deserializer::from({bytes_local});"
                    ));
                    // `.cbor` says the byte string IS the payload type's encoding, so bytes left over
                    // after the payload are not a value this type admits. Without the check below the
                    // embed accepted them and — since nothing holds them — re-encoded only the
                    // consumed prefix, so an ACCEPTED input round-tripped to DIFFERENT bytes:
                    // over-acceptance on every profile, and a `--preserve-encodings` fidelity
                    // violation on top. Found by the byte fuzzer (`fuzz/README.md` § "Findings
                    // disposition"); pinned by `structural_rejects` in `tests/core/tests.rs` at both
                    // `.cbor` spellings (rule body and member expression).
                    //
                    // Deliberately the SAME error as the top-level leftover check in
                    // `static/serialization.rs`'s `from_cbor_bytes` — one fact, one spelling, so a
                    // consumer matching on trailing data at the top level matches it here too. It
                    // flows through `DeserializeError`, so the enclosing annotation still names the
                    // member the leftover bytes were found in.
                    let trailing_check = || {
                        let mut block =
                            Block::new(format!("if !{name_overload}.as_slice().is_empty()"));
                        block.line(
                            "return Err(DeserializeFailure::CBOR(cbor_event::Error::TrailingData).into());",
                        );
                        block
                    };
                    // The check has to run once the payload IS consumed, so it follows the payload's
                    // own code — which is only expressible where that code is a complete STATEMENT.
                    // At a terminal position (a block's tail, a tuple element) the payload value is
                    // an expression and nothing may follow it, so bind it first and yield the
                    // binding. Both spellings are emitted rather than always binding, because the
                    // statement positions are the common ones and a `let x = x_payload;` rebinding
                    // in every consumer's generated crate is noise that says nothing.
                    //
                    // A VALUE-LESS payload is a third case, and takes neither spelling. Without
                    // encodings a fixed value stores nothing, so there is nothing to stage: read it
                    // with a discarding wrapper, run the check, then satisfy the caller's wrapper
                    // with the unit the payload evaluates to. Staging it would bind `let x_payload =
                    // ();` and re-emit `x_payload` — the same unit, but at the discarding STATEMENT
                    // slots a fixed member is read in (an array struct's field sequence, a map
                    // entry's `k_present = (|| { .. })()?` block) a bare expression line is not Rust.
                    // Under `--preserve-encodings` a fixed value DOES carry an encoding, so it is a
                    // value like any other and this leg must not take it.
                    //
                    // Value-less-ness is a property of the whole encoding CHAIN, not of its root: a
                    // mandatory `Tagged` operation stores nothing of its own without encodings
                    // either (the number is a constant, verified on the way past), so
                    // `bytes .cbor #6.1(42)` is exactly as value-less as `bytes .cbor 42` and must
                    // take the same leg.
                    let value_less_payload =
                        !cli.preserve_encodings && payload_is_value_less(&child);
                    if value_less_payload {
                        self.generate_deserialize(
                            types,
                            *child,
                            DeserializeBeforeAfter::new("", "", false),
                            config
                                .overload_deserializer(&name_overload)
                                .cbor_depth(cbor_level),
                            cli,
                        )
                        .add_to_code(&mut deser_code);
                        deser_code.content.push_block(trailing_check());
                        line_unit_value(&mut deser_code, &before_after);
                    } else if before_after.is_statement() {
                        self.generate_deserialize(
                            types,
                            *child,
                            before_after,
                            config
                                .overload_deserializer(&name_overload)
                                .cbor_depth(cbor_level),
                            cli,
                        )
                        .add_to_code(&mut deser_code);
                        deser_code.content.push_block(trailing_check());
                    } else {
                        let payload_binding = format!(
                            "{}_{}",
                            config.var_name,
                            cbor_payload_binding_suffix(cbor_level)
                        );
                        self.generate_deserialize(
                            types,
                            *child,
                            DeserializeBeforeAfter::new(
                                &format!("let {payload_binding} = "),
                                ";",
                                false,
                            ),
                            config
                                .overload_deserializer(&name_overload)
                                .cbor_depth(cbor_level),
                            cli,
                        )
                        .add_to_code(&mut deser_code);
                        deser_code.content.push_block(trailing_check());
                        deser_code.content.line(&format!(
                            "{}{payload_binding}{}",
                            before_after.before_str(false),
                            before_after.after_str(false)
                        ));
                    }
                    deser_code.throws = true;
                }
                SerializingRustType::EncodingOperation(
                    CBOREncodingOperation::Tagged(tag),
                    child,
                ) => {
                    // level (tag_depth + 1) counted outside-in. Stacked mandatory tags NEST these
                    // `match .tag_sz()` blocks, so an un-suffixed `tag_enc` binding would let the
                    // inner level shadow the outer and both final exprs would read the innermost
                    // size. Depth-suffix the binding (`tag_enc` -> `tag_enc2` at level >= 2) so each
                    // level's `Some(..)` final expr references its own size.
                    let tag_level = config.tag_depth + 1;
                    let tag_enc_binding = tag_enc_binding(tag_level);
                    if config.optional_field {
                        deser_code.content.line("read_len.read_elems(1)?;");
                        deser_code.read_len_used = true;
                    }
                    let mut tag_check = if cli.preserve_encodings {
                        let mut tag_check = Block::new(format!(
                            "{}match {}.tag_sz()?",
                            before_after.before, deserializer_name
                        ));
                        config.final_exprs.push(format!("Some({tag_enc_binding})"));
                        let some_deser_code = self
                            .generate_deserialize(
                                types,
                                *child,
                                DeserializeBeforeAfter::new("", "", before_after.expects_result),
                                config.optional_field(false).tag_depth(tag_level),
                                cli,
                            )
                            .mark_and_extract_content(&mut deser_code);
                        if let Some(single_line) = some_deser_code.as_single_line() {
                            tag_check.line(format!("({tag}, {tag_enc_binding}) => {single_line},"));
                        } else {
                            let mut deser_block =
                                Block::new(format!("({tag}, {tag_enc_binding}) =>"));
                            deser_block.push_all(some_deser_code);
                            deser_block.after(",");
                            tag_check.push_block(deser_block);
                        }
                        tag_check
                    } else {
                        let mut tag_check = Block::new(format!(
                            "{}match {}.tag()?",
                            before_after.before, deserializer_name
                        ));

                        let some_deser_code = self
                            .generate_deserialize(
                                types,
                                *child,
                                DeserializeBeforeAfter::new("", "", before_after.expects_result),
                                config.optional_field(false).tag_depth(tag_level),
                                cli,
                            )
                            .mark_and_extract_content(&mut deser_code);
                        if let Some(single_line) = some_deser_code.as_single_line() {
                            tag_check.line(format!("{tag} => {single_line},"));
                        } else {
                            let mut deser_block = Block::new(format!("{tag} =>"));
                            deser_block.push_all(some_deser_code);
                            deser_block.after(",");
                            tag_check.push_block(deser_block);
                        }
                        tag_check
                    };
                    tag_check.line(format!(
                    "{} => {}Err(DeserializeFailure::TagMismatch{{ found: tag, expected: {} }}.into()),",
                    if cli.preserve_encodings { "(tag, _enc)" } else { "tag" },
                    if before_after.expects_result { "" } else { "return " },
                    tag));
                    tag_check.after(before_after.after);
                    deser_code.content.push_block(tag_check);
                    deser_code.throws = true;
                }
                SerializingRustType::EncodingOperation(
                    CBOREncodingOperation::OptionallyTagged(tag),
                    child,
                ) => {
                    // The tag is OPTIONAL on the wire, so it can't be fused into the child's value
                    // read the way a mandatory `Tagged` is. Peek the next major type: if it's a tag,
                    // consume+validate it (same mismatch error class as `Tagged`) and record the
                    // presence; otherwise record `Untagged`. Then deserialize the child normally.
                    // Mirrors CML's hand impl (chain/rust/src/utils.rs).
                    let var_name = config.var_name;
                    // level (tag_depth + 1) counted outside-in; the infix keeps the presence-var
                    // member name in lockstep with `encoding_fields_impl` (`tag` -> `tag2` deeper),
                    // and the local it binds stays unique across nested levels.
                    let tag_level = config.tag_depth + 1;
                    let tag_infix = tag_encoding_infix(tag_level);
                    if config.optional_field {
                        deser_code.content.line("read_len.read_elems(1)?;");
                        deser_code.read_len_used = true;
                    }
                    if cli.preserve_encodings {
                        let mut presence_match = Block::new(format!(
                            "let {var_name}_{tag_infix}_encoding = match {deserializer_name}.cbor_type()?"
                        ));
                        let mut tag_arm = Block::new("cbor_event::Type::Tag =>");
                        tag_arm.line(format!(
                            "let (tag, tag_enc) = {deserializer_name}.tag_sz()?;"
                        ));
                        let mut mismatch = Block::new(format!("if tag != {tag}"));
                        mismatch.line(format!(
                            "return Err(DeserializeFailure::TagMismatch {{ found: tag, expected: {tag} }}.into());"
                        ));
                        tag_arm.push_block(mismatch);
                        tag_arm.line("TagPresenceEncoding::Tagged(Some(tag_enc))");
                        tag_arm.after(",");
                        presence_match.push_block(tag_arm);
                        presence_match.line("_ => TagPresenceEncoding::Untagged,");
                        presence_match.after(";");
                        deser_code.content.push_block(presence_match);
                        // FIRST encoding expr, matching `encoding_fields_impl`'s field order (tag
                        // field, then the child's) — the child recursion appends its own after this.
                        config
                            .final_exprs
                            .push(format!("{var_name}_{tag_infix}_encoding"));
                    } else {
                        let mut if_tag = Block::new(format!(
                            "if {deserializer_name}.cbor_type()? == cbor_event::Type::Tag"
                        ));
                        if_tag.line(format!("let tag = {deserializer_name}.tag()?;"));
                        let mut mismatch = Block::new(format!("if tag != {tag}"));
                        mismatch.line(format!(
                            "return Err(DeserializeFailure::TagMismatch {{ found: tag, expected: {tag} }}.into());"
                        ));
                        if_tag.push_block(mismatch);
                        deser_code.content.push_block(if_tag);
                    }
                    self.generate_deserialize(
                        types,
                        *child,
                        before_after,
                        config.optional_field(false).tag_depth(tag_level),
                        cli,
                    )
                    .add_to_code(&mut deser_code);
                    deser_code.throws = true;
                }
            }
        }
        deser_code
    }

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
    /// before its arm `Gc0`; `aaa = [? f0: uint, f1: uint]` + `zzz = aaa / tstr` happens to emit
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
