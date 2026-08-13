import {
  RoadmapWireError,
  decodeFatalUtf8Lf,
  decodeMarkdownToken,
} from "../markdown_codec.ts";
import { sha256 } from "../kernel.ts";

const UTF8 = new TextEncoder();

export interface RawMarkdownToken {
  readonly id: number;
  readonly start_byte: number;
  readonly end_byte: number;
  readonly raw: Uint8Array;
  readonly physical_lf_count: number;
  readonly placeholder: string;
  readonly placeholder_value: string;
  readonly logical_path: string;
}

interface MutableToken {
  id: number;
  start_byte: number;
  end_byte: number;
  raw: Uint8Array;
  physical_lf_count: number;
  digest: string;
  probe: number;
  placeholder: string;
  placeholder_value: string;
  logical_path?: string;
  consumed_path?: string;
  decoded?: Uint8Array;
}

function wireFail(
  source: string,
  code: "E-CODEC-TOKEN" | "E-CODEC-PLACEHOLDER" | "E-TOML-PARSE",
  logicalPath: string,
  message: string,
  span?: { start_byte: number; end_byte: number },
): never {
  throw new RoadmapWireError({ code, source, logical_path: logicalPath, message, span, exit: 1 });
}

export function placeholderCandidate(
  rawToken: Uint8Array,
  ordinal: number,
  probe: number,
): string {
  return `__ROADMAP_MD_${sha256(rawToken)}_${ordinal}_${probe}__`;
}

export function childLogicalPath(parent: string, key: string): string {
  if (/^[A-Za-z_][A-Za-z0-9_-]*$/.test(key)) return parent === "$" ? key : `${parent}.${key}`;
  return `${parent}[${JSON.stringify(key)}]`;
}

export function indexLogicalPath(parent: string, index: number): string {
  return `${parent}[${index}]`;
}

function countByte(bytes: Uint8Array, start: number, end: number, needle: number): number {
  let count = 0;
  for (let index = start; index < end; index += 1) if (bytes[index] === needle) count += 1;
  return count;
}

function scanSingleBasic(bytes: Uint8Array, source: string, start: number): number {
  for (let index = start + 1; index < bytes.length; index += 1) {
    const byte = bytes[index];
    if (byte === 0x0a) {
      wireFail(source, "E-CODEC-TOKEN", "$", "physical LF is forbidden in a single-line basic string", {
        start_byte: start,
        end_byte: index + 1,
      });
    }
    if (byte !== 0x22) continue;
    let backslashes = 0;
    for (let prior = index - 1; prior > start && bytes[prior] === 0x5c; prior -= 1) backslashes += 1;
    if (backslashes % 2 === 0) return index + 1;
  }
  wireFail(source, "E-CODEC-TOKEN", "$", "unterminated single-line basic string", {
    start_byte: start,
    end_byte: bytes.length,
  });
}

function scanSingleLiteral(bytes: Uint8Array, source: string, start: number): number {
  for (let index = start + 1; index < bytes.length; index += 1) {
    if (bytes[index] === 0x0a) {
      wireFail(source, "E-CODEC-TOKEN", "$", "physical LF is forbidden in a single-line literal string", {
        start_byte: start,
        end_byte: index + 1,
      });
    }
    if (bytes[index] === 0x27) return index + 1;
  }
  wireFail(source, "E-CODEC-TOKEN", "$", "unterminated single-line literal string", {
    start_byte: start,
    end_byte: bytes.length,
  });
}

function quoteRun(bytes: Uint8Array, start: number, quote: number): number {
  let end = start;
  while (end < bytes.length && bytes[end] === quote) end += 1;
  return end - start;
}

function scanMultilineLiteral(bytes: Uint8Array, source: string, start: number): number {
  for (let index = start + 3; index < bytes.length; ) {
    if (bytes[index] !== 0x27) {
      index += 1;
      continue;
    }
    const run = quoteRun(bytes, index, 0x27);
    if (run >= 6) {
      wireFail(source, "E-CODEC-TOKEN", "$", "multiline literal quote run cannot exceed five quotes", {
        start_byte: index,
        end_byte: index + run,
      });
    }
    if (run >= 3) return index + run;
    index += run;
  }
  wireFail(source, "E-CODEC-TOKEN", "$", "unterminated multiline literal string", {
    start_byte: start,
    end_byte: bytes.length,
  });
}

function scanMultilineBasic(bytes: Uint8Array, source: string, start: number): number {
  for (let index = start + 3; index < bytes.length; ) {
    if (bytes[index] !== 0x22) {
      index += 1;
      continue;
    }
    let backslashes = 0;
    for (let prior = index - 1; prior >= start + 3 && bytes[prior] === 0x5c; prior -= 1) {
      backslashes += 1;
    }
    if (backslashes % 2 === 1) {
      index += 1;
      continue;
    }
    const run = quoteRun(bytes, index, 0x22);
    if (run >= 6) {
      wireFail(source, "E-CODEC-TOKEN", "$", "multiline basic quote run cannot exceed five quotes", {
        start_byte: index,
        end_byte: index + run,
      });
    }
    if (run >= 3) return index + run;
    index += run;
  }
  wireFail(source, "E-CODEC-TOKEN", "$", "unterminated multiline basic string", {
    start_byte: start,
    end_byte: bytes.length,
  });
}

function pushToken(bytes: Uint8Array, tokens: MutableToken[], start: number, end: number): void {
  const raw = bytes.slice(start, end);
  const id = tokens.length;
  const digest = sha256(raw);
  const probe = 0;
  const placeholder = `__ROADMAP_MD_${digest}_${id}_${probe}__`;
  const physical_lf_count = countByte(bytes, start, end, 0x0a);
  tokens.push({
    id,
    start_byte: start,
    end_byte: end,
    raw,
    physical_lf_count,
    digest,
    probe,
    placeholder,
    placeholder_value: placeholder + "\n".repeat(physical_lf_count),
  });
}

/**
 * Shield every MULTILINE string token, in both quote forms: since the D7 flip a canonical Markdown
 * value is a multiline literal (`'''`) unless its content forces the basic fallback (`"""`), and
 * no other field spells its value across lines. Single-line strings of either form are ordinary
 * scalars and stay with Bun.
 */
function scanTokens(bytes: Uint8Array, source: string): MutableToken[] {
  const tokens: MutableToken[] = [];
  for (let index = 0; index < bytes.length; ) {
    const byte = bytes[index];
    if (byte === 0x23) {
      while (index < bytes.length && bytes[index] !== 0x0a) index += 1;
      continue;
    }
    if (byte === 0x22) {
      if (quoteRun(bytes, index, 0x22) >= 3) {
        const end = scanMultilineBasic(bytes, source, index);
        pushToken(bytes, tokens, index, end);
        index = end;
        continue;
      }
      index = scanSingleBasic(bytes, source, index);
      continue;
    }
    if (byte === 0x27) {
      if (quoteRun(bytes, index, 0x27) >= 3) {
        const end = scanMultilineLiteral(bytes, source, index);
        pushToken(bytes, tokens, index, end);
        index = end;
        continue;
      }
      index = scanSingleLiteral(bytes, source, index);
      continue;
    }
    index += 1;
  }
  return tokens;
}

function rebuildShadow(bytes: Uint8Array, tokens: readonly MutableToken[]): string {
  const parts: Uint8Array[] = [];
  let cursor = 0;
  for (const token of tokens) {
    parts.push(bytes.subarray(cursor, token.start_byte));
    parts.push(UTF8.encode(`'''${token.placeholder}${"\n".repeat(token.physical_lf_count)}'''`));
    cursor = token.end_byte;
  }
  parts.push(bytes.subarray(cursor));
  const length = parts.reduce((sum, part) => sum + part.length, 0);
  const joined = new Uint8Array(length);
  let offset = 0;
  for (const part of parts) {
    joined.set(part, offset);
    offset += part.length;
  }
  return new TextDecoder().decode(joined);
}

interface StringOccurrence {
  value: string;
  logical_path: string;
}

function collectStrings(value: unknown): StringOccurrence[] {
  const out: StringOccurrence[] = [];
  const visit = (current: unknown, path: string): void => {
    if (typeof current === "string") {
      out.push({ value: current, logical_path: path });
      return;
    }
    if (Array.isArray(current)) {
      current.forEach((entry, index) => visit(entry, indexLogicalPath(path, index)));
      return;
    }
    if (current === null || typeof current !== "object") return;
    for (const key of Object.keys(current)) {
      const child = childLogicalPath(path, key);
      out.push({ value: key, logical_path: `${child}::<key>` });
      visit(Object.getOwnPropertyDescriptor(current, key)?.value, child);
    }
  };
  visit(value, "$ ".trim());
  return out;
}

function parseShadow(source: string, shadow: string): unknown {
  try {
    const parsed: unknown = Bun.TOML.parse(shadow);
    return parsed;
  } catch (error) {
    const suffix = error instanceof Error ? `: ${error.message}` : "";
    wireFail(source, "E-TOML-PARSE", "$", `Bun rejected TOML structure${suffix}`);
  }
}

export class MarkdownBindings {
  readonly source: string;
  readonly parsed: unknown;
  readonly tokens: readonly RawMarkdownToken[];
  readonly #byPlaceholder: ReadonlyMap<string, MutableToken>;

  constructor(source: string, parsed: unknown, tokens: MutableToken[]) {
    this.source = source;
    this.parsed = parsed;
    this.tokens = tokens.map((token) => ({
      id: token.id,
      start_byte: token.start_byte,
      end_byte: token.end_byte,
      raw: token.raw.slice(),
      physical_lf_count: token.physical_lf_count,
      placeholder: token.placeholder,
      placeholder_value: token.placeholder_value,
      logical_path: token.logical_path!,
    }));
    this.#byPlaceholder = new Map(tokens.map((token) => [token.placeholder_value, token]));
  }

  isRegisteredPlaceholder(value: unknown): boolean {
    return typeof value === "string" && this.#byPlaceholder.has(value);
  }

  expectMarkdown(value: unknown, logicalPath: string): Uint8Array {
    if (typeof value !== "string") {
      wireFail(this.source, "E-CODEC-PLACEHOLDER", logicalPath, "Markdown field is not a shielded multiline Markdown token");
    }
    const token = this.#byPlaceholder.get(value);
    if (token === undefined) {
      wireFail(this.source, "E-CODEC-PLACEHOLDER", logicalPath, "Markdown field is an ordinary Bun-decoded string");
    }
    if (token.logical_path !== logicalPath) {
      wireFail(
        this.source,
        "E-CODEC-PLACEHOLDER",
        logicalPath,
        `Markdown placeholder is bound to post-Bun path ${token.logical_path}, not ${logicalPath}`,
      );
    }
    if (token.consumed_path !== undefined) {
      wireFail(
        this.source,
        "E-CODEC-PLACEHOLDER",
        logicalPath,
        `Markdown token was already consumed at ${token.consumed_path}`,
      );
    }
    token.consumed_path = logicalPath;
    token.decoded ??= decodeMarkdownToken(token.raw, this.source, {
      start_byte: token.start_byte,
      end_byte: token.end_byte,
    });
    return token.decoded.slice();
  }

  assertAllConsumed(): void {
    const unconsumed = [...this.#byPlaceholder.values()].filter((token) => token.consumed_path === undefined);
    if (unconsumed.length !== 0) {
      const token = unconsumed.sort((left, right) => left.start_byte - right.start_byte)[0];
      wireFail(
        this.source,
        "E-CODEC-PLACEHOLDER",
        token.logical_path!,
        "multiline Markdown token was not consumed by exact schema decoding",
        { start_byte: token.start_byte, end_byte: token.end_byte },
      );
    }
  }
}

export function shieldTomlMarkdown(bytes: Uint8Array, source: string): MarkdownBindings {
  decodeFatalUtf8Lf(bytes, source);
  const tokens = scanTokens(bytes, source);
  let parsed: unknown;
  let stringCount = 0;
  let attempts = 0;
  let bound = Number.POSITIVE_INFINITY;

  for (;;) {
    parsed = parseShadow(source, rebuildShadow(bytes, tokens));
    const occurrences = collectStrings(parsed);
    stringCount = occurrences.length;
    if (!Number.isFinite(bound)) bound = stringCount + tokens.length + 1;
    const colliding: MutableToken[] = [];
    for (const token of tokens) {
      const matches = occurrences.filter((entry) => entry.value === token.placeholder_value);
      if (matches.length === 1) {
        token.logical_path = matches[0].logical_path;
      } else {
        colliding.push(token);
      }
    }
    if (colliding.length === 0) break;
    attempts += 1;
    if (attempts > bound) {
      wireFail(
        source,
        "E-CODEC-PLACEHOLDER",
        "$",
        `could not mint unique exact-one placeholders within ${stringCount + tokens.length + 1} probes`,
      );
    }
    for (const token of colliding) {
      token.probe += 1;
      token.placeholder = `__ROADMAP_MD_${token.digest}_${token.id}_${token.probe}__`;
      token.placeholder_value = token.placeholder + "\n".repeat(token.physical_lf_count);
      token.logical_path = undefined;
    }
  }

  return new MarkdownBindings(source, parsed!, tokens);
}
