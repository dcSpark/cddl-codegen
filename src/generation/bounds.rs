use super::*;

pub(super) fn bounds_check_expr(p: Primitive, e: &str) -> String {
    match p {
        Primitive::Bool
        | Primitive::Float
        | Primitive::F16
        | Primitive::F32
        | Primitive::F64
        | Primitive::F16To32
        | Primitive::F32To64
        | Primitive::I8
        | Primitive::I16
        | Primitive::I32
        | Primitive::I64
        | Primitive::U8
        | Primitive::U16
        | Primitive::U32
        | Primitive::U64
        | Primitive::N64 => e.to_owned(),
        Primitive::Str | Primitive::Bytes => format!("{e}.len()"),
    }
}

// pub(crate): emit_tests mirrors ctor fallibility as `needs_bounds_check_if_inlined && this is Some`
pub(crate) fn bounds_check_expr_rust_type(ty: &RustType, e: &str) -> Option<String> {
    match ty.resolve_alias_shallow() {
        ConceptualRustType::Primitive(p) => Some(bounds_check_expr(*p, e)),
        ConceptualRustType::Array(_) |
        ConceptualRustType::Map(_, _) => Some(format!("{e}.len()")),
        // Alias should never be hit due to above alias resolving
        ConceptualRustType::Alias(_, _) => unreachable!(),
        // RustType is covered by passed in ctor
        ConceptualRustType::Rust(_) |
        // `any` carries no value bounds (control operators on `any` are rejected)
        ConceptualRustType::Any |
        // Optional is not passed into ctor, but instead set later
        ConceptualRustType::Optional(_) |
        // FixedValue has no field associated with it
        ConceptualRustType::Fixed(_) => None,
    }
}

/// Whether values of `p` are provably `>= 0` at a bounds-check site, so a `min == 0` lower leg
/// (`e < 0`, or `e.len() < 0`) is dead and can be elided (also silencing `unused_comparisons`). The
/// unsigned integers, `bytes`/`text` (checked via `.len()`), and `bool` are non-negative. The signed
/// integers, floats, and the `nint` u64 magnitude are not — the magnitude IS a u64, but the wrapper
/// deliberately keeps its long form for a stored-magnitude nint, so `N64` stays `false` here to keep
/// every nint site byte-identical.
pub(super) fn primitive_non_negative(p: Primitive) -> bool {
    match p {
        Primitive::Bytes
        | Primitive::Str
        | Primitive::Bool
        | Primitive::U8
        | Primitive::U16
        | Primitive::U32
        | Primitive::U64 => true,
        Primitive::I8
        | Primitive::I16
        | Primitive::I32
        | Primitive::I64
        | Primitive::N64
        | Primitive::Float
        | Primitive::F16
        | Primitive::F32
        | Primitive::F64
        | Primitive::F16To32
        | Primitive::F32To64 => false,
    }
}

/// The `RustType` analog of `primitive_non_negative` for the member/wrapper bounds-check expression
/// over `ty`: the `bytes`/`text`/unsigned/`bool` primitives and every array/map (`.len()`) are
/// non-negative, but ONLY when no CBOR encoding operation wraps the value — a `CBORBytes` wrap changes
/// the runtime shape and the wrapper's original check was conservative there, so the
/// `encodings.is_empty()` guard preserves that exact behavior.
pub(super) fn bounds_check_expr_non_negative(ty: &RustType) -> bool {
    if !ty.encodings.is_empty() {
        return false;
    }
    match ty.resolve_alias_shallow() {
        ConceptualRustType::Primitive(p) => primitive_non_negative(*p),
        ConceptualRustType::Array(_) | ConceptualRustType::Map(_, _) => true,
        _ => false,
    }
}

// we store nint as its u64 magnitude `m = |v + 1| = -v - 1`, which is *decreasing* in the signed
// value `v`. So a value bound `vmin <= v <= vmax` maps to a magnitude bound with the endpoints
// SWAPPED: the value-min becomes the magnitude-max and the value-max becomes the magnitude-min
// (e.g. `nint .ge -5` → `v >= -5` → `m <= 4`). Not swapping inverts the check in the constructor
// (the deserializer, which checks the signed value directly, stays correct — so the two disagree).
pub(crate) fn nint_bounds_to_u64(
    bounds: &(Option<i128>, Option<i128>),
) -> (Option<i128>, Option<i128>) {
    (
        bounds.1.map(|x| (x + 1).abs()),
        bounds.0.map(|x| (x + 1).abs()),
    )
}

/// The `Err(..)` expression for a failed integer range check. `location` `Some(name)` produces a
/// `DeserializeError::new(name, ..)` (the wrapper's `new()`/deserialize sites, which annotate the
/// type), `None` produces a bare `DeserializeFailure::RangeCheck{..}.into()` (member ctor/setter and
/// primitive deserialize sites). Mirrors `range_check_err_float`'s `location` duality.
/// `found_i128` states the found expression is ALREADY `i128` (the nint deserialize arms, whose
/// value comes from `negative_integer_sz()` — an `(i128, Sz)`), so the widening `as i128` is
/// omitted to avoid a `clippy::unnecessary_cast` no-op; every other source (a `u64` uint read, an
/// `i64` `negative_integer()` read, a `.len()` usize, a narrower member field) keeps the real cast.
fn range_check_err(
    e: &str,
    min: Option<i128>,
    max: Option<i128>,
    return_err: bool,
    location: Option<&str>,
    found_i128: bool,
) -> String {
    let possible_return = if return_err { "return " } else { "" };
    let opt = |b: Option<i128>| b.map_or_else(|| "None".to_owned(), |b| format!("Some({b})"));
    let cast = if found_i128 { "" } else { " as i128" };
    let failure = format!(
        "DeserializeFailure::RangeCheck{{ found: {}{}, min: {}, max: {}}}",
        e,
        cast,
        opt(min),
        opt(max),
    );
    let err = match location {
        Some(loc) => format!("DeserializeError::new(\"{loc}\", {failure})"),
        None => format!("{failure}.into()"),
    };
    format!("{{ {possible_return}Err({err}) }}")
}

/// Renders a f64 literal that round-trips exactly (Rust's `{:?}` guarantees this for f64), with the
/// `f64` suffix so it types as f64 even when compared against an f32-derived value.
fn float_literal(v: f64) -> String {
    format!("{v:?}f64")
}

/// The unsuffixed twin of `float_literal` for the FIXED-VALUE emission sites (serialize write,
/// deserialize compare, mismatch-error construction, wasm constant): `{:?}` never drops the
/// decimal point (3.0 -> "3.0", where Display renders "3" — an integer literal in an f64
/// position, E0308), while non-whole values render byte-identically to Display (3.5 -> "3.5").
/// Every such site is already f64-typed, so no suffix is needed. NaN/inf cannot reach these
/// sites: a CDDL fixed float value comes from the grammar's decimal/hexfloat lexemes, which
/// denote finite values.
pub(super) fn float_fixed_literal(v: f64) -> String {
    debug_assert!(v.is_finite(), "fixed-value float literal must be finite");
    format!("{v:?}")
}

/// The ACCEPT-form condition for a float window over `val` (a NaN-safe conjunction of the present
/// sides). Never reject-form (`x < min || x > max`) — under that shape a NaN slips through because
/// both comparisons are false. The caller negates this (`if !(<cond>) {{ Err }}`), so NaN — for
/// which every comparison is false, making the conjunction false — is always rejected.
fn float_accept_cond(
    window: &crate::intermediate::FloatWindow,
    val: &str,
    cast_f64: bool,
) -> String {
    let v = if cast_f64 {
        format!("({val} as f64)")
    } else {
        val.to_owned()
    };
    let mut parts = Vec::new();
    if let Some((min, exclusive)) = window.0 {
        parts.push(format!(
            "{v} {} {}",
            if exclusive { ">" } else { ">=" },
            float_literal(min)
        ));
    }
    if let Some((max, exclusive)) = window.1 {
        parts.push(format!(
            "{v} {} {}",
            if exclusive { "<" } else { "<=" },
            float_literal(max)
        ));
    }
    // a real window always has at least one side; guard the impossible empty case
    if parts.is_empty() {
        unreachable!("float_accept_cond called with an empty window");
    }
    parts.join(" && ")
}

/// The `Err(..)` expression for a failed float window check. `location` `Some(name)` produces a
/// `DeserializeError::new(name, ..)` (wrapper deserialize/new, which annotate the type), `None`
/// produces a bare `DeserializeFailure::RangeCheckFloat{..}.into()` (primitive deserialize and_then).
fn range_check_err_float(
    found_f64: &str,
    window: &crate::intermediate::FloatWindow,
    return_err: bool,
    location: Option<&str>,
) -> String {
    let opt = |side: Option<(f64, bool)>| match side {
        Some((v, _)) => format!("Some({})", float_literal(v)),
        None => "None".to_owned(),
    };
    // stored inclusivity is the negation of the parsed exclusivity flag
    let incl = |side: Option<(f64, bool)>| match side {
        Some((_, exclusive)) => (!exclusive).to_string(),
        None => "false".to_owned(),
    };
    let failure = format!(
        "DeserializeFailure::RangeCheckFloat{{ found: {} as f64, min: {}, max: {}, min_inclusive: {}, max_inclusive: {} }}",
        found_f64,
        opt(window.0),
        opt(window.1),
        incl(window.0),
        incl(window.1),
    );
    let err = match location {
        Some(loc) => format!("DeserializeError::new(\"{loc}\", {failure})"),
        None => format!("{failure}.into()"),
    };
    let possible_return = if return_err { "return " } else { "" };
    format!("{{ {possible_return}Err({err}) }}")
}

/// The NaN-safe `if !(<accept>) {{ Err(..) }}` float bounds check. `cast_f64` casts an f32 value to
/// f64 first so the authored decimal literal is compared exactly.
pub(super) fn bounds_check_if_block_float(
    window: &crate::intermediate::FloatWindow,
    cast_f64: bool,
    e: &str,
    return_err: bool,
    location: Option<&str>,
) -> String {
    // `range_check_err_float` already appends `as f64` to the found value, so pass the raw expr
    // (avoids a redundant `(x as f64) as f64` for an f32 value).
    format!(
        "if !({}) {}",
        float_accept_cond(window, e, cast_f64),
        range_check_err_float(e, window, return_err, location)
    )
}

/// The value bounds check line for a field/setter/variant-ctor site, dispatching to the integer or
/// float path (or `None` if the type carries no value window / no check expression exists — e.g. a
/// bounded named Rust wrapper checks at its own construction). Reproduces the integer path
/// byte-for-byte (same `nint_bounds_to_u64` swap) so existing snapshots are unchanged.
pub(super) fn value_bounds_check_line(ty: &RustType, e: &str, return_err: bool) -> Option<String> {
    // The `[+ T]` shape enforces its `>= 1` bound at the type level (NonEmptyVec's single TryFrom
    // door), so no inline length check is emitted at ctor/setter/deser sites — the invalid state is
    // unrepresentable. Every OTHER array bound (2*5, *3, …) keeps this runtime-check path. Alias-
    // resolving so a field referencing a named `[+ …]` rule skips the check too.
    if ty.is_type_enforced_non_empty() {
        return None;
    }
    if let Some(window) = &ty.config.float_bounds {
        let cast_f64 = matches!(
            ty.resolve_alias_shallow(),
            ConceptualRustType::Primitive(p) if p.float_carrier_is_f32()
        );
        return Some(bounds_check_if_block_float(
            window, cast_f64, e, return_err, None,
        ));
    }
    let bounds = ty.config.bounds.as_ref()?;
    let check_expr = bounds_check_expr_rust_type(ty, e)?;
    let non_negative = bounds_check_expr_non_negative(ty);
    if matches!(
        ty.resolve_alias_shallow(),
        ConceptualRustType::Primitive(Primitive::N64)
    ) {
        Some(bounds_check_if_block(
            &nint_bounds_to_u64(bounds),
            &check_expr,
            return_err,
            non_negative,
            None,
            // member/setter/ctor site: `e` is the stored field (a `u64` magnitude for N64, or an
            // i8..i64/u64 elsewhere) — never already i128, so keep the widening cast.
            false,
        ))
    } else {
        Some(bounds_check_if_block(
            bounds,
            &check_expr,
            return_err,
            non_negative,
            None,
            false,
        ))
    }
}

/// The value bounds check line a COMPONENT-face parameter emits, or `None` when the type carries no
/// value window — or enforces it in its own type system (the `[+ T]` early-out, whose single
/// `TryFrom` door IS the check, exactly as in [`value_bounds_check_line`]).
///
/// The same decision tree as [`value_bounds_check_line`], reached through the SAME condition owners
/// (`reject_cond`, `bounds_check_expr_rust_type`, `bounds_check_expr_non_negative`,
/// `nint_bounds_to_u64`, `float_accept_cond`, `float_literal`): only the `Err(..)` construction
/// forks. Two things force that fork and nothing else does — the component guest reports every
/// failure as the `String` of the rust error's `Display` (through its own `err` helper), and it
/// spells the error types through the generated crate's path (`runtime`) rather than bare, because
/// the guest file imports nothing from that crate. A second CONDITION spelling is precisely the
/// drift this module was consolidated to prevent, so nothing here re-derives one.
///
/// Note the wrap: `DeserializeFailure` derives `Debug` but implements no `Display` — only
/// `DeserializeError` does — so the failure is lifted through `DeserializeError::from` before it
/// reaches a `Display`-bounded helper.
pub(super) fn component_bounds_check_line(ty: &RustType, e: &str, runtime: &str) -> Option<String> {
    if ty.is_type_enforced_non_empty() {
        return None;
    }
    let wrap = |failure: String| {
        format!(
            "return Err(err({runtime}::error::DeserializeError::from({runtime}::error::DeserializeFailure::{failure})));"
        )
    };
    if let Some(window) = &ty.config.float_bounds {
        let cast_f64 = matches!(
            ty.resolve_alias_shallow(),
            ConceptualRustType::Primitive(p) if p.float_carrier_is_f32()
        );
        let opt = |side: Option<(f64, bool)>| match side {
            Some((v, _)) => format!("Some({})", float_literal(v)),
            None => "None".to_owned(),
        };
        // stored inclusivity is the negation of the parsed exclusivity flag
        let incl = |side: Option<(f64, bool)>| match side {
            Some((_, exclusive)) => (!exclusive).to_string(),
            None => "false".to_owned(),
        };
        return Some(format!(
            "if !({}) {{ {} }}",
            float_accept_cond(window, e, cast_f64),
            wrap(format!(
                "RangeCheckFloat{{ found: {e} as f64, min: {}, max: {}, min_inclusive: {}, max_inclusive: {} }}",
                opt(window.0),
                opt(window.1),
                incl(window.0),
                incl(window.1),
            ))
        ));
    }
    let bounds = ty.config.bounds.as_ref()?;
    let check_expr = bounds_check_expr_rust_type(ty, e)?;
    let non_negative = bounds_check_expr_non_negative(ty);
    // The nint endpoint swap is load-bearing: the magnitude is DECREASING in the signed value, so a
    // check written against the unswapped bounds is inverted. Both the reported window and the
    // condition take the swapped pair, exactly as `value_bounds_check_line` does.
    let bounds = if matches!(
        ty.resolve_alias_shallow(),
        ConceptualRustType::Primitive(Primitive::N64)
    ) {
        nint_bounds_to_u64(bounds)
    } else {
        *bounds
    };
    let opt = |b: Option<i128>| b.map_or_else(|| "None".to_owned(), |b| format!("Some({b})"));
    Some(format!(
        "if {} {{ {} }}",
        reject_cond(&bounds, non_negative).render(&check_expr),
        wrap(format!(
            "RangeCheck{{ found: {check_expr} as i128, min: {}, max: {} }}",
            opt(bounds.0),
            opt(bounds.1),
        ))
    ))
}

/// The `if <cond> { Err(RangeCheck..) }` integer bounds check — the single owner of the condition
/// spelling for every member/setter, primitive-deserialize, and wrapper site. `non_negative` asserts
/// the checked expression is provably `>= 0` (an unsigned primitive or a `len()`), which lets a
/// `min == 0` lower leg (`e < 0`, dead there and an `unused_comparisons` wart) be dropped; when
/// unsure a site passes `false` and keeps the long form. `location` threads through to
/// `range_check_err` (the wrapper's name-carrying `new()` copy vs the locationless deserialize copy).
/// The reported `min`/`max` are ALWAYS the original bounds, regardless of how the condition simplifies.
/// `found_i128` threads through to `range_check_err` (whether the found expression is already i128).
pub(super) fn bounds_check_if_block(
    bounds: &(Option<i128>, Option<i128>),
    e: &str,
    return_err: bool,
    non_negative: bool,
    location: Option<&str>,
    found_i128: bool,
) -> String {
    format!(
        "if {} {}",
        reject_cond(bounds, non_negative).render(e),
        range_check_err(e, bounds.0, bounds.1, return_err, location, found_i128)
    )
}

/// The shape of the reject condition an integer value window imposes, held separately from its
/// rendering so the same decision can be both EMITTED (into a generated decoder) and EVALUATED
/// (by the `--emit-tests` minter, which has to pick key/field values the emitted decoder accepts).
/// Two independent derivations of "is V in range?" drift silently — a minter that re-derives the
/// `.ne` exclusion encoding wrong mints a key its own generated decoder rejects — so both callers
/// go through `reject_cond`.
///
/// Each variant names the condition under which a value is REJECTED.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum RejectCond {
    /// reject exactly this value (the `.ne N` single-value exclusion)
    Eq(i128),
    /// reject everything except this value (a single-value window)
    Ne(i128),
    /// reject above this value
    Gt(i128),
    /// reject below this value
    Lt(i128),
    /// reject outside the inclusive window
    Outside(i128, i128),
}

impl RejectCond {
    /// The emitted rust condition expression over `e`.
    fn render(&self, e: &str) -> String {
        match self {
            RejectCond::Eq(x) => format!("{e} == {x}"),
            RejectCond::Ne(x) => format!("{e} != {x}"),
            RejectCond::Gt(x) => format!("{e} > {x}"),
            RejectCond::Lt(x) => format!("{e} < {x}"),
            RejectCond::Outside(min, max) => format!("{e} < {min} || {e} > {max}"),
        }
    }

    /// Whether `v` is rejected — the evaluating twin of `render`.
    pub(crate) fn rejects(&self, v: i128) -> bool {
        match self {
            RejectCond::Eq(x) => v == *x,
            RejectCond::Ne(x) => v != *x,
            RejectCond::Gt(x) => v > *x,
            RejectCond::Lt(x) => v < *x,
            RejectCond::Outside(min, max) => v < *min || v > *max,
        }
    }
}

/// Classify an integer value window into its reject condition. `non_negative` asserts the checked
/// expression is provably `>= 0` (an unsigned primitive or a `len()`), which lets a `min == 0`
/// lower leg (`e < 0`, dead there and an `unused_comparisons` wart) collapse to the one-sided form;
/// when unsure a caller passes `false` and keeps the long form. The two agree on every value the
/// assertion admits, so an evaluating caller that cannot vouch for the sign passes `false`.
pub(crate) fn reject_cond(bounds: &(Option<i128>, Option<i128>), non_negative: bool) -> RejectCond {
    match bounds {
        // `.ne N` is encoded as Range(N+1, N-1) (see parsing.rs NE): min > max means an
        // EXCLUSION of the single value between them, not an (unsatisfiable) window
        (Some(min), Some(max)) if min > max => RejectCond::Eq(min - 1),
        // a single-value window (min == max) is one equality, not the redundant `< N || > N`
        (Some(min), Some(max)) if min == max => RejectCond::Ne(*min),
        // `min == 0` on a provably-non-negative expr: the `e < 0` leg can never fire — drop it
        (Some(0), Some(max)) if non_negative => RejectCond::Gt(*max),
        (Some(min), Some(max)) => RejectCond::Outside(*min, *max),
        (None, Some(max)) => RejectCond::Gt(*max),
        (Some(min), None) => RejectCond::Lt(*min),
        // `classify_sign_arm` never emits a `Check` with both bounds absent (it returns
        // `Unconstrained` instead), and every other caller passes a real range, so this is
        // unreachable by construction rather than a silent panic on empty windows.
        (None, None) => unreachable!("bounds_check_if_block called with no bounds"),
    }
}

/// Whether an integer value window REJECTS `v`, evaluated through the same `reject_cond` the
/// emitted check renders. `non_negative` is deliberately NOT a parameter: an evaluating caller
/// asks about arbitrary candidate values (including negative ones), so it may not assume the
/// simplification the emitter's `min == 0` arm relies on.
///
/// An empty window `(None, None)` accepts everything rather than hitting `reject_cond`'s
/// `unreachable!`: the emitter can never reach it, but the `--emit-tests` minter asks about key
/// domains that legitimately carry no window at all.
pub(crate) fn bounds_reject_value(bounds: &(Option<i128>, Option<i128>), v: i128) -> bool {
    if bounds.0.is_none() && bounds.1.is_none() {
        return false;
    }
    reject_cond(bounds, false).rejects(v)
}

/// The two CBOR sign arms a signed int can decode from (unsigned-integer major type vs
/// negative-integer major type). A value window is classified independently per arm.
#[derive(Clone, Copy)]
pub(super) enum SignArm {
    /// values >= 0, read as a `u64` — a check here can never compare against a negative literal
    Uint,
    /// values <= -1, read as the real signed value via `negative_integer_sz`
    Nint,
}

/// How a value window projects onto one CBOR sign arm.
pub(super) enum SignArmBounds {
    /// The window imposes no constraint on this arm (bounds vacuous here) — emit no check.
    Unconstrained,
    /// The window narrows to these (possibly one-sided) bounds on this arm — emit the check.
    Check((Option<i128>, Option<i128>)),
    /// The window excludes this arm's entire sign domain — every value it decodes is out of
    /// range. Reject unconditionally, reporting the ORIGINAL window (not the empty projection).
    Empty((Option<i128>, Option<i128>)),
}

/// Project a value window onto one CBOR sign arm. Distinguishes "vacuous in this arm" (drop the
/// bound) from "this arm's whole sign domain is excluded" (unconditional reject) — conflating the
/// two is what made the old per-arm filter panic on all-negative / zero-upper windows.
pub(super) fn classify_sign_arm(
    bounds: &Option<(Option<i128>, Option<i128>)>,
    arm: SignArm,
) -> SignArmBounds {
    let bounds = match bounds {
        Some(b) => *b,
        None => return SignArmBounds::Unconstrained,
    };
    // `.ne N` exclusion encoding: min > max excludes the single value min-1. Route the exclusion
    // check to whichever arm the excluded value lives in; the other arm has nothing to check.
    if let (Some(min), Some(max)) = bounds
        && min > max
    {
        let excluded_here = match arm {
            SignArm::Uint => (min - 1) >= 0,
            SignArm::Nint => (min - 1) < 0,
        };
        return if excluded_here {
            SignArmBounds::Check((Some(min), Some(max)))
        } else {
            SignArmBounds::Unconstrained
        };
    }
    let (lower, upper) = bounds;
    match arm {
        SignArm::Uint => {
            // uint arm covers values >= 0
            if matches!(upper, Some(u) if u < 0) {
                // upper < 0 → no non-negative value is in range
                return SignArmBounds::Empty((lower, upper));
            }
            // lower <= 0 is vacuous for a u64; upper >= 0 is kept (u == 0 emits `x > 0`)
            let narrowed_lower = lower.filter(|l| *l > 0);
            if narrowed_lower.is_none() && upper.is_none() {
                SignArmBounds::Unconstrained
            } else {
                SignArmBounds::Check((narrowed_lower, upper))
            }
        }
        SignArm::Nint => {
            // nint arm covers values <= -1
            if matches!(lower, Some(l) if l >= 0) {
                // lower >= 0 → no negative value is in range
                return SignArmBounds::Empty((lower, upper));
            }
            // upper >= -1 is vacuous for a nint; lower <= -1 is kept
            let narrowed_upper = upper.filter(|u| *u < -1);
            if lower.is_none() && narrowed_upper.is_none() {
                SignArmBounds::Unconstrained
            } else {
                SignArmBounds::Check((lower, narrowed_upper))
            }
        }
    }
}

/// The `if <cond> { Err(RangeCheck..) }` for one classified sign arm, or `None` when the arm
/// needs no check. The `Empty` case rejects unconditionally (`if true`) rather than emitting the
/// real comparison, since the uint arm can't compare a `u64` against a negative bound.
pub(super) fn sign_arm_if_block(
    arm: &SignArmBounds,
    e: &str,
    return_err: bool,
    found_i128: bool,
) -> Option<String> {
    match arm {
        SignArmBounds::Unconstrained => None,
        // sign-arm classification is its own concern; keep the long spelling (`non_negative = false`)
        // and stay locationless — the `min == max` collapse is unconditional and still applies.
        SignArmBounds::Check(bounds) => Some(bounds_check_if_block(
            bounds, e, return_err, false, None, found_i128,
        )),
        SignArmBounds::Empty(orig) => Some(format!(
            "if true {}",
            range_check_err(e, orig.0, orig.1, return_err, None, found_i128)
        )),
    }
}

pub(super) const CONVERT_ERR_TO_OURS: &str = ".map_err(Into::<DeserializeError>::into)";

pub(super) fn non_preserve_bounds_fn(
    p: Primitive,
    x: &str,
    bounds: &Option<(Option<i128>, Option<i128>)>,
) -> Cow<'static, str> {
    match bounds {
        // always convert error to have consistent E for the and_then
        Some(bounds) => Cow::Owned(format!(
            "{}.and_then(|{}| {} else {{ Ok({}) }})",
            CONVERT_ERR_TO_OURS,
            x,
            bounds_check_if_block(
                bounds,
                &bounds_check_expr(p, x),
                false,
                primitive_non_negative(p),
                None,
                // this fn only serves the u64 unsigned reads and the `.len()` (bytes/text) checks —
                // never a nint `i128` source, so the widening cast is real.
                false,
            ),
            x,
        )),
        None => Cow::Borrowed(""),
    }
}

// --- width guards for the narrowing casts below ---------------------------
// Every integer read on this path comes back WIDER than the target type (u64
// from the unsigned readers, i64/i128 from the nint readers), so each `as`
// cast must be preceded by a check that makes it lossless: a bare cast
// silently truncated out-of-width values (`uint .size 2` -> u16 decoded 65536
// "successfully" as 0), and the exact-width collapses (`i8 = -128..127`)
// carry NO residual `bounds`, so nothing else rejected. The guard reuses the
// authored-bounds `.and_then(..)` shape and the existing RangeCheck failure
// (reporting the full type window), and is SKIPPED when the authored/
// classified check already caps the failing side (subsuming it), so bounded
// emissions stay byte-identical.
pub(super) fn prim_window(p: Primitive) -> (i128, i128) {
    match p {
        Primitive::U8 => (0i128, u8::MAX as i128),
        Primitive::U16 => (0i128, u16::MAX as i128),
        Primitive::U32 => (0i128, u32::MAX as i128),
        Primitive::I8 => (i8::MIN as i128, i8::MAX as i128),
        Primitive::I16 => (i16::MIN as i128, i16::MAX as i128),
        Primitive::I32 => (i32::MIN as i128, i32::MAX as i128),
        Primitive::I64 => (i64::MIN as i128, i64::MAX as i128),
        _ => unreachable!("width guard only applies to narrowing-cast primitives"),
    }
}

// `.and_then(..)` rejecting `cond` with the full type window via RangeCheck.
// `pat`/`ok` carry the value-only vs (value, encoding)-tuple shapes.
// `converted`: whether an earlier chain stage (an authored-bounds fn or the
// site's error_convert) already mapped the error to DeserializeError — when
// nothing did, the guard prepends the conversion itself (same "consistent E
// for the and_then" rule as the bounds fns).
pub(super) fn width_reject(
    cond: &str,
    wmin: i128,
    wmax: i128,
    pat: &str,
    ok: &str,
    converted: bool,
    found_i128: bool,
) -> String {
    // `found_i128`: the guarded `x` is already `i128` (a nint arm reading `negative_integer_sz()`),
    // so omit the no-op `as i128`; the uint arms (`x: u64`) and the i64 `negative_integer()` arm
    // keep the real widening cast.
    let cast = if found_i128 { "" } else { " as i128" };
    format!(
        "{}.and_then(|{pat}| if {cond} {{ Err(DeserializeFailure::RangeCheck{{ found: x{cast}, min: Some({wmin}), max: Some({wmax}) }}.into()) }} else {{ Ok({ok}) }})",
        if converted { "" } else { CONVERT_ERR_TO_OURS },
    )
}

// A guard is superfluous when the emitted check already caps the arm's failing
// side: an authored/classified upper bound <= the type max (uint side) or lower
// bound >= the type min (nint side). A min>max pair is the `.ne` EXCLUSION
// encoding — it caps nothing.
pub(super) fn upper_caps(bounds: &Option<(Option<i128>, Option<i128>)>, wmax: i128) -> bool {
    matches!(bounds, Some((mn, Some(mx))) if mn.is_none_or(|mn| mn <= *mx) && *mx <= wmax)
}
fn lower_caps(bounds: &Option<(Option<i128>, Option<i128>)>, wmin: i128) -> bool {
    matches!(bounds, Some((Some(mn), mx)) if mx.is_none_or(|mx| *mn <= mx) && *mn >= wmin)
}
pub(super) fn uint_arm_needs_width(arm: &SignArmBounds, wmax: i128) -> bool {
    match arm {
        // the whole arm rejects unconditionally — no value ever reaches the cast
        SignArmBounds::Empty(_) => false,
        SignArmBounds::Check(bounds) => !upper_caps(&Some(*bounds), wmax),
        SignArmBounds::Unconstrained => true,
    }
}
pub(super) fn nint_arm_needs_width(arm: &SignArmBounds, wmin: i128) -> bool {
    match arm {
        SignArmBounds::Empty(_) => false,
        SignArmBounds::Check(bounds) => !lower_caps(&Some(*bounds), wmin),
        SignArmBounds::Unconstrained => true,
    }
}
