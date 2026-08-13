import type { FixedValueSourceFacts, RustEnumVariantFact } from "./adapters/types.ts";
import { matchingRustDelimiter, rustTokens, type RustToken } from "./rust_symbols.ts";

const cp = (left: string, right: string): number => left < right ? -1 : left > right ? 1 : 0;

function fail(message: string): never {
  throw new Error(`FixedValue source authority: ${message}`);
}

function uniqueSequence(tokens: readonly RustToken[], sequence: readonly string[], label: string): number {
  const starts: number[] = [];
  for (let index = 0; index <= tokens.length - sequence.length; index++) {
    if (sequence.every((value, offset) => tokens[index + offset]?.text === value)) starts.push(index);
  }
  if (starts.length !== 1) fail(`expected exactly one ${label}, found ${starts.length}`);
  return starts[0]!;
}

function parseEnumVariant(tokens: readonly RustToken[]): RustEnumVariantFact {
  if (tokens.length === 0 || !/^[A-Za-z_][A-Za-z0-9_]*$/u.test(tokens[0]!.text)) {
    fail(`unparseable enum variant ${tokens.map((value) => value.text).join("")}`);
  }
  const name = tokens[0]!.text;
  if (tokens.length === 1) return Object.freeze({ name, payload: null });
  if (tokens[1]?.text !== "(" || matchingRustDelimiter(tokens, 1) !== tokens.length - 1) {
    fail(`variant ${name} must be a unit or one-field tuple variant`);
  }
  const payload = tokens.slice(2, -1).map((value) => value.text).join("");
  if (payload.length === 0 || tokens.slice(2, -1).some((value) => value.text === ",")) {
    fail(`variant ${name} must have exactly one payload type`);
  }
  return Object.freeze({ name, payload });
}

function enumVariants(tokens: readonly RustToken[], open: number, close: number): readonly RustEnumVariantFact[] {
  const variants: RustEnumVariantFact[] = [];
  let start = open + 1;
  const delimiters: string[] = [];
  for (let index = start; index < close; index++) {
    const token = tokens[index]!.text;
    if (["(", "[", "{"].includes(token)) delimiters.push(token);
    else if ([")", "]", "}"].includes(token)) {
      const expected = token === ")" ? "(" : token === "]" ? "[" : "{";
      if (delimiters.pop() !== expected) fail("mismatched delimiter in enum body");
    } else if (token === "," && delimiters.length === 0) {
      if (index > start) variants.push(parseEnumVariant(tokens.slice(start, index)));
      start = index + 1;
    }
  }
  if (delimiters.length !== 0) fail("unbalanced delimiter in enum body");
  if (start < close) variants.push(parseEnumVariant(tokens.slice(start, close)));
  if (variants.length === 0 || new Set(variants.map((value) => value.name)).size !== variants.length) {
    fail("enum variants must be nonempty and unique");
  }
  return Object.freeze(variants);
}

/** Extract the closed IR vocabulary and independently check the literal-lowering match. */
export function extractFixedValueSourceFacts(rustTypeSource: string, parsingSource: string): FixedValueSourceFacts {
  const rustTypeTokens = rustTokens(rustTypeSource);
  const enumStart = uniqueSequence(rustTypeTokens, ["pub", "enum", "FixedValue", "{"], "`pub enum FixedValue`");
  const enumOpen = enumStart + 3;
  const enumClose = matchingRustDelimiter(rustTypeTokens, enumOpen);
  if (enumClose === undefined) fail("unbalanced `FixedValue` enum body");
  const variants = enumVariants(rustTypeTokens, enumOpen, enumClose);

  const parsingTokens = rustTokens(parsingSource);
  const fnStart = uniqueSequence(parsingTokens, ["fn", "type2_to_fixed_value", "("], "`type2_to_fixed_value` function");
  let fnOpen = fnStart;
  while (fnOpen < parsingTokens.length && parsingTokens[fnOpen]?.text !== "{") fnOpen++;
  const fnClose = matchingRustDelimiter(parsingTokens, fnOpen);
  if (fnOpen >= parsingTokens.length || fnClose === undefined) fail("unbalanced `type2_to_fixed_value` body");
  const lowered = new Set<string>();
  for (let index = fnOpen + 1; index + 2 < fnClose; index++) {
    if (parsingTokens[index]?.text === "FixedValue" && parsingTokens[index + 1]?.text === ":" &&
      parsingTokens[index + 2]?.text === ":" && /^[A-Za-z_][A-Za-z0-9_]*$/u.test(parsingTokens[index + 3]?.text ?? "")) {
      lowered.add(parsingTokens[index + 3]!.text);
    }
  }
  const enumNames = variants.map((value) => value.name).sort(cp);
  const loweredNames = [...lowered].sort(cp);
  if (JSON.stringify(enumNames) !== JSON.stringify(loweredNames)) {
    fail(`enum/lowering variant mismatch (enum=${enumNames.join(",")}; lowering=${loweredNames.join(",")})`);
  }
  return Object.freeze({ variants, lowered_variants: Object.freeze(loweredNames) });
}
