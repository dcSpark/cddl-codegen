/**
 * The Rust lexer and the `#[cfg(test)]` test-symbol extractor.  These derive the `test_symbol`
 * reference universe from the tracked generator sources; nothing here knows about roadmap
 * documents, references, or joins.
 */
import type { TestSymbolFact } from "./adapters/types.ts";
import type { RoadmapIssue } from "./errors.ts";
import { sortRoadmapIssues as sortIssues } from "./errors.ts";
import { codePointSort } from "./kernel.ts";
import type { RepoPath } from "./model/core.ts";
import {
  decodeTrackedText,
  factIssue,
  type RepositoryFactResult,
  type TrackedTextInput,
} from "./repository_facts.ts";

const issue = factIssue;

const textEncoder = new TextEncoder();

function utf8Offsets(value: string): readonly number[] {
  const result: number[] = new Array(value.length + 1).fill(0);
  let bytes = 0;
  for (let index = 0; index < value.length;) {
    result[index] = bytes;
    const point = value.codePointAt(index)!;
    const width = point > 0xffff ? 2 : 1;
    bytes += textEncoder.encode(String.fromCodePoint(point)).byteLength;
    for (let offset = 1; offset <= width; offset += 1) result[index + offset] = bytes;
    index += width;
  }
  result[value.length] = bytes;
  return result;
}

export interface RustToken {
  readonly text: string;
  readonly start: number;
  readonly end: number;
}

export function rustTokens(source: string): readonly RustToken[] {
  const offsets = utf8Offsets(source);
  const tokens: RustToken[] = [];
  let index = 0;
  const push = (start: number, end: number): void => {
    tokens.push({ text: source.slice(start, end), start: offsets[start]!, end: offsets[end]! });
  };
  const skipQuoted = (quote: string): void => {
    index += 1;
    while (index < source.length) {
      if (source[index] === "\\") index += 2;
      else if (source[index] === quote) { index += 1; break; }
      else index += 1;
    }
  };
  const skipCharacterLiteral = (): boolean => {
    let cursor = index + 1;
    if (cursor >= source.length) return false;
    if (source[cursor] === "\\") {
      cursor += 1;
      if (source[cursor] === "u" && source[cursor + 1] === "{") {
        const close = source.indexOf("}", cursor + 2);
        if (close < 0) return false;
        cursor = close + 1;
      } else if (source[cursor] === "x") {
        cursor += 3;
      } else {
        cursor += 1;
      }
    } else {
      const point = source.codePointAt(cursor)!;
      cursor += point > 0xffff ? 2 : 1;
    }
    if (source[cursor] !== "'") return false;
    index = cursor + 1;
    return true;
  };
  while (index < source.length) {
    if (/\s/u.test(source[index]!)) { index += 1; continue; }
    if (source.startsWith("//", index)) {
      const end = source.indexOf("\n", index + 2);
      index = end < 0 ? source.length : end + 1;
      continue;
    }
    if (source.startsWith("/*", index)) {
      let depth = 1;
      index += 2;
      while (index < source.length && depth > 0) {
        if (source.startsWith("/*", index)) { depth += 1; index += 2; }
        else if (source.startsWith("*/", index)) { depth -= 1; index += 2; }
        else index += 1;
      }
      continue;
    }
    const raw = /^(?:b|c)?r(#+)?"/u.exec(source.slice(index));
    if (raw !== null) {
      const hashes = raw[1] ?? "";
      const close = `"${hashes}`;
      const end = source.indexOf(close, index + raw[0].length);
      index = end < 0 ? source.length : end + close.length;
      continue;
    }
    if (
      source[index] === '"' ||
      ((source[index] === "b" || source[index] === "c") && source[index + 1] === '"')
    ) {
      if (source[index] !== '"') index += 1;
      skipQuoted('"');
      continue;
    }
    if (source[index] === "'") {
      if (skipCharacterLiteral()) continue;
      push(index, index + 1);
      index += 1;
      continue;
    }
    if (/[A-Za-z_]/u.test(source[index]!)) {
      const start = index++;
      while (index < source.length && /[A-Za-z0-9_]/u.test(source[index]!)) index += 1;
      push(start, index);
      continue;
    }
    push(index, index + 1);
    index += 1;
  }
  return tokens;
}

export function matchingRustDelimiter(tokens: readonly RustToken[], open: number): number | undefined {
  const opening = tokens[open]?.text;
  const closing = opening === "{" ? "}" : opening === "[" ? "]" : opening === "(" ? ")" : undefined;
  if (closing === undefined) return undefined;
  let depth = 0;
  for (let index = open; index < tokens.length; index += 1) {
    if (tokens[index]!.text === opening) depth += 1;
    if (tokens[index]!.text === closing && --depth === 0) return index;
  }
  return undefined;
}

function tokenSequenceAt(
  tokens: readonly RustToken[],
  start: number,
  sequence: readonly string[],
): boolean {
  return sequence.every((expected, offset) => tokens[start + offset]?.text === expected);
}

function skipMacroRules(tokens: readonly RustToken[], start: number, end: number): number | undefined {
  if (tokens[start]?.text !== "macro_rules" || tokens[start + 1]?.text !== "!") return undefined;
  let body = start + 2;
  while (body < end && !["{", "[", "("].includes(tokens[body]!.text)) body += 1;
  const close = matchingRustDelimiter(tokens, body);
  return close === undefined || close >= end ? end : close + 1;
}

function isRustIdentifier(token: RustToken | undefined): boolean {
  return token !== undefined && /^[A-Za-z_][A-Za-z0-9_]*$/u.test(token.text);
}

/** Skip a complete path-qualified macro invocation, including its optional item semicolon. */
function skipMacroInvocation(
  tokens: readonly RustToken[],
  start: number,
  end: number,
): number | undefined {
  if (!isRustIdentifier(tokens[start])) return undefined;
  let cursor = start + 1;
  while (
    tokens[cursor]?.text === ":" && tokens[cursor + 1]?.text === ":" &&
    isRustIdentifier(tokens[cursor + 2])
  ) cursor += 3;
  if (tokens[cursor]?.text !== "!" || !["{", "[", "("].includes(tokens[cursor + 1]?.text ?? "")) {
    return undefined;
  }
  const close = matchingRustDelimiter(tokens, cursor + 1);
  if (close === undefined || close >= end) return end;
  return tokens[close + 1]?.text === ";" ? close + 2 : close + 1;
}

const TEST_ROOT_DECLARATION = Object.freeze([
  "#", "[", "cfg", "(", "test", ")", "]", "mod", "tests", ";",
] as const);

interface ModuleDeclaration {
  readonly start: number;
  readonly module_index: number;
  readonly name?: string;
  readonly terminator?: ";" | "{";
  readonly end: number;
  readonly exact_crate_visibility: boolean;
}

function moduleDeclarationAt(
  tokens: readonly RustToken[],
  start: number,
  end: number,
): ModuleDeclaration | undefined {
  let moduleIndex: number | undefined;
  let exactCrateVisibility = false;
  if (tokens[start]?.text === "mod") {
    moduleIndex = start;
  } else if (tokens[start]?.text === "pub" && tokens[start + 1]?.text === "mod") {
    moduleIndex = start + 1;
  } else if (tokens[start]?.text === "pub" && tokens[start + 1]?.text === "(") {
    const close = matchingRustDelimiter(tokens, start + 1);
    if (close === undefined || close >= end) {
      return { start, module_index: start, end, exact_crate_visibility: false };
    }
    if (tokens[close + 1]?.text !== "mod") return undefined;
    moduleIndex = close + 1;
    exactCrateVisibility = close === start + 3 && tokens[start + 2]?.text === "crate";
  }
  if (moduleIndex === undefined) return undefined;
  const name = tokens[moduleIndex + 1]?.text;
  const hasName = name !== undefined && /^[A-Za-z_][A-Za-z0-9_]*$/u.test(name);
  const terminatorToken = hasName ? tokens[moduleIndex + 2]?.text : undefined;
  const terminator = terminatorToken === ";" || terminatorToken === "{"
    ? terminatorToken
    : undefined;
  let declarationEnd = Math.min(end, moduleIndex + (hasName ? 2 : 1));
  if (terminator === ";") declarationEnd = moduleIndex + 3;
  if (terminator === "{") {
    const close = matchingRustDelimiter(tokens, moduleIndex + 2);
    declarationEnd = close === undefined || close >= end ? end : close + 1;
  }
  return {
    start,
    module_index: moduleIndex,
    name: hasName ? name : undefined,
    terminator,
    end: declarationEnd,
    exact_crate_visibility: exactCrateVisibility,
  };
}

function contiguousAttributes(
  tokens: readonly RustToken[],
  start: number,
  end: number,
): { readonly attributes: readonly (readonly RustToken[])[]; readonly next: number } {
  const attributes: RustToken[][] = [];
  let index = start;
  while (tokens[index]?.text === "#" && tokens[index + 1]?.text === "[") {
    const close = matchingRustDelimiter(tokens, index + 1);
    if (close === undefined || close >= end) return { attributes, next: end };
    attributes.push(tokens.slice(index, close + 1));
    index = close + 1;
  }
  return { attributes, next: index };
}

function exactCfgTestAttribute(attribute: readonly RustToken[]): boolean {
  return tokenSequenceAt(attribute, 0, ["#", "[", "cfg", "(", "test", ")", "]"]) &&
    attribute.length === 7;
}

function validateTestRoot(tokens: readonly RustToken[]): readonly string[] {
  const problems: string[] = [];
  let declarations = 0;
  for (let index = 0; index < tokens.length;) {
    const afterMacro = skipMacroRules(tokens, index, tokens.length);
    if (afterMacro !== undefined) { index = afterMacro; continue; }
    const afterInvocation = skipMacroInvocation(tokens, index, tokens.length);
    if (afterInvocation !== undefined) { index = afterInvocation; continue; }
    if (tokens[index]!.text === "#" && tokens[index + 1]?.text === "[") {
      const parsed = contiguousAttributes(tokens, index, tokens.length);
      const declaration = moduleDeclarationAt(tokens, parsed.next, tokens.length);
      if (declaration?.name === "tests") {
        if (
          parsed.attributes.length === 1 && exactCfgTestAttribute(parsed.attributes[0]!) &&
          declaration.start === declaration.module_index && declaration.terminator === ";"
        ) declarations += 1;
        else problems.push("root test module declaration must be exactly #[cfg(test)] mod tests;");
        index = declaration.end;
        continue;
      }
      index = parsed.next;
      continue;
    }
    if (tokenSequenceAt(tokens, index, TEST_ROOT_DECLARATION)) {
      declarations += 1;
      index += TEST_ROOT_DECLARATION.length;
      continue;
    }
    if (tokens[index]!.text === "{") {
      const close = matchingRustDelimiter(tokens, index);
      index = close === undefined ? tokens.length : close + 1;
      continue;
    }
    const declaration = moduleDeclarationAt(tokens, index, tokens.length);
    if (declaration?.name === "tests") {
      problems.push("root test module declaration must be exactly #[cfg(test)] mod tests;");
      index = declaration.end;
      continue;
    }
    index += 1;
  }
  if (declarations !== 1) {
    problems.push(`expected exactly one root #[cfg(test)] mod tests; declaration, found ${declarations}`);
  }
  return Object.freeze(problems);
}

interface DeclaredTestModules {
  readonly modules: readonly string[];
  readonly problems: readonly string[];
}

function declaredTestModules(tokens: readonly RustToken[]): DeclaredTestModules {
  const modules: string[] = [];
  const problems: string[] = [];
  for (let index = 0; index < tokens.length;) {
    const afterMacro = skipMacroRules(tokens, index, tokens.length);
    if (afterMacro !== undefined) { index = afterMacro; continue; }
    const afterInvocation = skipMacroInvocation(tokens, index, tokens.length);
    if (afterInvocation !== undefined) { index = afterInvocation; continue; }
    if (tokens[index]!.text === "#" && tokens[index + 1]?.text === "[") {
      const parsed = contiguousAttributes(tokens, index, tokens.length);
      const attributedModule = moduleDeclarationAt(tokens, parsed.next, tokens.length);
      if (attributedModule !== undefined) {
        problems.push(`test module declaration at byte ${tokens[index]!.start} must be exactly pub(crate) mod <ident>;`);
        index = attributedModule.end;
      } else {
        index = parsed.next;
      }
      continue;
    }
    if (tokens[index]!.text === "{") {
      const close = matchingRustDelimiter(tokens, index);
      index = close === undefined ? tokens.length : close + 1;
      continue;
    }
    const declaration = moduleDeclarationAt(tokens, index, tokens.length);
    if (declaration !== undefined) {
      if (
        declaration.exact_crate_visibility && declaration.name !== undefined &&
        declaration.terminator === ";"
      ) modules.push(declaration.name);
      else {
      problems.push(`test module declaration at byte ${tokens[index]!.start} must be exactly pub(crate) mod <ident>;`);
      }
      index = declaration.end;
      continue;
    }
    if (tokens[index]!.text === "pub" && tokens[index + 1]?.text === "(" && matchingRustDelimiter(tokens, index + 1) === undefined) {
      problems.push(`malformed visibility at byte ${tokens[index]!.start} in test module registry`);
      index = tokens.length;
      continue;
    }
    if (tokens[index]!.text === "pub" && tokens[index + 1] === undefined) {
      problems.push(`truncated public declaration at byte ${tokens[index]!.start} in test module registry`);
      index += 1;
      continue;
    }
    index += 1;
  }
  return {
    modules: Object.freeze(modules.sort(codePointSort)),
    problems: Object.freeze(problems.sort(codePointSort)),
  };
}

function scanTestItems(
  source: RepoPath,
  tokens: readonly RustToken[],
  modulePath: readonly string[],
  out: TestSymbolFact[],
  begin = 0,
  end = tokens.length,
): void {
  let testAttribute = false;
  for (let index = begin; index < end;) {
    const afterMacro = skipMacroRules(tokens, index, end);
    if (afterMacro !== undefined) {
      index = afterMacro;
      testAttribute = false;
      continue;
    }
    const afterInvocation = skipMacroInvocation(tokens, index, end);
    if (afterInvocation !== undefined) {
      index = afterInvocation;
      testAttribute = false;
      continue;
    }
    if (["{", "[", "("].includes(tokens[index]!.text)) {
      const close = matchingRustDelimiter(tokens, index);
      const preservesTestAttribute = testAttribute &&
        tokens[index]!.text === "(" && tokens[index - 1]?.text === "pub";
      index = close === undefined || close >= end ? end : close + 1;
      if (!preservesTestAttribute) testAttribute = false;
      continue;
    }
    if (tokens[index]!.text === "#" && tokens[index + 1]?.text === "[") {
      const close = matchingRustDelimiter(tokens, index + 1);
      if (close === undefined || close >= end) return;
      if (close === index + 3 && tokens[index + 2]?.text === "test") testAttribute = true;
      index = close + 1;
      continue;
    }
    if (
      tokens[index]!.text === "mod" &&
      /^[A-Za-z_][A-Za-z0-9_]*$/u.test(tokens[index + 1]?.text ?? "") &&
      tokens[index + 2]?.text === "{"
    ) {
      const close = matchingRustDelimiter(tokens, index + 2);
      if (close === undefined || close > end) return;
      scanTestItems(source, tokens, [...modulePath, tokens[index + 1]!.text], out, index + 3, close);
      index = close + 1;
      testAttribute = false;
      continue;
    }
    let fnIndex = index;
    if (tokens[fnIndex]!.text === "async") fnIndex += 1;
    if (testAttribute && tokens[fnIndex]?.text === "fn") {
      const name = tokens[fnIndex + 1];
      if (name !== undefined && /^[A-Za-z_][A-Za-z0-9_]*$/u.test(name.text)) {
        const symbol = [...modulePath, name.text].join("::");
        out.push({
          test_id: `rust-test:cddl-codegen#${symbol}`,
          symbol,
          source,
          span: { start_byte: name.start, end_byte: name.end },
          module_path: Object.freeze([...modulePath]),
        });
      }
      testAttribute = false;
      index = fnIndex + 2;
      continue;
    }
    if (!["pub", "(", ")", "crate", "async"].includes(tokens[index]!.text)) {
      testAttribute = false;
    }
    index += 1;
  }
}

/** Derive the bounded Rust test-symbol registry from revision-injected tracked bytes. */
export function extractRustTestSymbols(
  inputs: readonly TrackedTextInput[],
): RepositoryFactResult<TestSymbolFact> {
  const facts: TestSymbolFact[] = [];
  const issues: RoadmapIssue[] = [];
  const files = new Map<RepoPath, string>();
  for (const input of [...inputs].sort((left, right) => codePointSort(left.source, right.source))) {
    const text = decodeTrackedText(input, issues);
    if (text !== undefined) files.set(input.source, text);
  }
  const main = files.get("src/main.rs" as RepoPath);
  const testsMod = files.get("src/tests/mod.rs" as RepoPath);
  if (main === undefined) {
    issues.push(issue("E-SOURCE-MISSING", "src/main.rs", "test-symbol-registry", "tracked test root is missing"));
  } else {
    for (const problem of validateTestRoot(rustTokens(main))) {
      issues.push(issue("E-REFERENCE-UNRESOLVED", "src/main.rs", "test-symbol-registry", problem));
    }
  }
  if (testsMod === undefined) {
    issues.push(issue("E-SOURCE-MISSING", "src/tests/mod.rs", "test-symbol-registry", "tracked tests module registry is missing"));
  } else {
    const testsModTokens = rustTokens(testsMod);
    // The registry module is itself a registered source module. Direct tests in this file have the
    // exact `tests::<fn>` identity; child declarations only extend the bounded source universe.
    scanTestItems("src/tests/mod.rs" as RepoPath, testsModTokens, ["tests"], facts);
    const declaration = declaredTestModules(testsModTokens);
    for (const problem of declaration.problems) {
      issues.push(issue("E-REFERENCE-UNRESOLVED", "src/tests/mod.rs", "test-symbol-registry", problem));
    }
    const seen = new Set<string>();
    for (const module of declaration.modules) {
      if (seen.has(module)) {
        issues.push(issue("E-ID-DUPLICATE", "src/tests/mod.rs", `module.${module}`, `test module ${JSON.stringify(module)} is declared more than once`));
        continue;
      }
      seen.add(module);
      const source = `src/tests/${module}.rs` as RepoPath;
      const body = files.get(source);
      if (body === undefined) {
        issues.push(issue("E-SOURCE-MISSING", source, "test-symbol-registry", `declared test module ${JSON.stringify(module)} is missing`));
        continue;
      }
      scanTestItems(source, rustTokens(body), ["tests", module], facts);
    }
  }
  facts.sort((left, right) =>
    codePointSort(left.test_id, right.test_id) || codePointSort(left.source, right.source) ||
    left.span.start_byte - right.span.start_byte || left.span.end_byte - right.span.end_byte
  );
  for (let index = 1; index < facts.length; index += 1) {
    if (facts[index - 1]!.test_id === facts[index]!.test_id) {
      issues.push(issue(
        "E-ID-DUPLICATE",
        facts[index]!.source,
        facts[index]!.test_id,
        `derived test ID ${JSON.stringify(facts[index]!.test_id)} is duplicated`,
        facts[index]!.span,
      ));
    }
  }
  return { facts: Object.freeze(facts), issues: sortIssues(issues) };
}
