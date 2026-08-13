import type { IssueCode, RoadmapIssue } from "./errors.ts";

const UTF8_FATAL = new TextDecoder("utf-8", { fatal: true });
const UTF8 = new TextEncoder();

export class RoadmapWireError extends Error {
  readonly issue: RoadmapIssue;

  constructor(issue: RoadmapIssue) {
    super(issue.message);
    this.name = "RoadmapWireError";
    this.issue = issue;
  }
}

function fail(
  code: IssueCode,
  source: string,
  logical_path: string,
  message: string,
  span?: { start_byte: number; end_byte: number },
): never {
  throw new RoadmapWireError({ code, source, logical_path, message, span, exit: 1 });
}

export function decodeFatalUtf8Lf(
  bytes: Uint8Array,
  source: string,
  logicalPath = "$",
): string {
  let decoded: string;
  try {
    decoded = UTF8_FATAL.decode(bytes);
  } catch {
    fail("E-CODEC-UTF8", source, logicalPath, "source is not strict UTF-8");
  }
  const cr = bytes.indexOf(0x0d);
  if (cr !== -1) {
    fail(
      "E-CODEC-LINE-END",
      source,
      logicalPath,
      "source must use LF line endings; CR and CRLF are forbidden",
      { start_byte: cr, end_byte: cr + 1 },
    );
  }
  return decoded;
}

function appendUnicodeEscape(out: string[], byte: number): void {
  out.push("\\u", byte.toString(16).padStart(4, "0"));
}

/** Encode one Markdown byte value using the sole canonical TOML spelling. */
export function encodeMarkdownString(bytes: Uint8Array, source = "<markdown>"): string {
  decodeFatalUtf8Lf(bytes, source);
  const out: string[] = ['"""'];
  for (let index = 0; index < bytes.length; ) {
    const byte = bytes[index];
    if (byte >= 0x80) {
      const width = byte < 0xe0 ? 2 : byte < 0xf0 ? 3 : 4;
      out.push(UTF8_FATAL.decode(bytes.subarray(index, index + width)));
      index += width;
      continue;
    }
    switch (byte) {
      case 0x0a:
        out.push(index === 0 ? "\\n" : "\n");
        break;
      case 0x22:
        out.push('\\"');
        break;
      case 0x5c:
        out.push("\\\\");
        break;
      case 0x09:
        out.push("\\t");
        break;
      case 0x08:
        out.push("\\b");
        break;
      case 0x0c:
        out.push("\\f");
        break;
      default:
        if (byte < 0x20 || byte === 0x7f) appendUnicodeEscape(out, byte);
        else out.push(String.fromCharCode(byte));
    }
    index += 1;
  }
  out.push('"""');
  return out.join("");
}

export function encodeMarkdownValue(bytes: Uint8Array, source = "<markdown>"): Uint8Array {
  return UTF8.encode(encodeMarkdownString(bytes, source));
}

function decodedUtf8Width(first: number): number {
  if (first < 0x80) return 1;
  if (first < 0xe0) return 2;
  if (first < 0xf0) return 3;
  return 4;
}

function pushScalar(out: number[], scalar: number): void {
  out.push(...UTF8.encode(String.fromCodePoint(scalar)));
}

function closingQuoteRun(token: Uint8Array, quote: number): number {
  let run = 0;
  for (let index = token.length - 1; index >= 3 && token[index] === quote; index -= 1) run += 1;
  return run;
}

/**
 * Independently decode a TOML multiline-literal token: the body is carried verbatim, with TOML's
 * delimiter-newline trim applied and no escape processing at all. TOML's `mll-char` admits TAB and
 * `newline` as the only control scalars a literal body may hold, and a literal has no escapes with
 * which to spell the others, so any other one is rejected here rather than silently accepted.
 */
function decodeLiteralMarkdownToken(
  token: Uint8Array,
  source: string,
  span: { start_byte: number; end_byte: number },
): Uint8Array {
  const trailingQuotes = closingQuoteRun(token, 0x27);
  if (trailingQuotes < 3 || trailingQuotes > 5) {
    fail("E-CODEC-TOKEN", source, "$", "multiline literal closing quote run must contain three through five quotes", span);
  }
  const contentEnd = token.length - 3;
  let index = 3;
  if (index < contentEnd && token[index] === 0x0a) index += 1;
  const out: number[] = [];
  while (index < contentEnd) {
    const byte = token[index];
    if (byte < 0x20 ? byte !== 0x09 && byte !== 0x0a : byte === 0x7f) {
      fail("E-CODEC-SCALAR", source, "$", "raw control scalar is forbidden in a Markdown token", {
        start_byte: span.start_byte + index,
        end_byte: span.start_byte + index + 1,
      });
    }
    out.push(byte);
    index += 1;
  }
  const decoded = Uint8Array.from(out);
  decodeFatalUtf8Lf(decoded, source);
  return decoded;
}

/**
 * Independently decode a TOML multiline Markdown token in either canonical quote form. Bun's
 * decoded control escapes are never used. This accepts TOML's delimiter-newline trim and 4/5-quote
 * closing forms so canonical composition can diagnose alternate spellings; noncanonical escapes
 * themselves fail here.
 */
export function decodeMarkdownToken(
  token: Uint8Array,
  source: string,
  span: { start_byte: number; end_byte: number },
): Uint8Array {
  decodeFatalUtf8Lf(token, source);
  if (token.length >= 6 && token[0] === 0x27 && token[1] === 0x27 && token[2] === 0x27) {
    return decodeLiteralMarkdownToken(token, source, span);
  }
  if (token.length < 6 || token[0] !== 0x22 || token[1] !== 0x22 || token[2] !== 0x22) {
    fail("E-CODEC-TOKEN", source, "$", "Markdown token is not a multiline string", span);
  }

  const trailingQuotes = closingQuoteRun(token, 0x22);
  if (trailingQuotes < 3 || trailingQuotes > 5) {
    fail("E-CODEC-TOKEN", source, "$", "multiline basic closing quote run must contain three through five quotes", span);
  }

  const contentEnd = token.length - 3;
  let index = 3;
  if (index < contentEnd && token[index] === 0x0a) index += 1;
  const out: number[] = [];

  while (index < contentEnd) {
    const byte = token[index];
    if (byte >= 0x80) {
      const width = decodedUtf8Width(byte);
      out.push(...token.subarray(index, index + width));
      index += width;
      continue;
    }
    if (byte === 0x0a) {
      out.push(byte);
      index += 1;
      continue;
    }
    if (byte === 0x22) {
      out.push(byte);
      index += 1;
      continue;
    }
    if (byte !== 0x5c) {
      if (byte < 0x20 || byte === 0x7f) {
        fail("E-CODEC-SCALAR", source, "$", "raw control scalar is forbidden in a Markdown token", {
          start_byte: span.start_byte + index,
          end_byte: span.start_byte + index + 1,
        });
      }
      out.push(byte);
      index += 1;
      continue;
    }

    const escapeStart = index;
    index += 1;
    if (index >= contentEnd) {
      fail("E-CODEC-TOKEN", source, "$", "unterminated Markdown escape", span);
    }
    const escaped = token[index];
    index += 1;
    switch (escaped) {
      case 0x5c:
        out.push(0x5c);
        break;
      case 0x22:
        out.push(0x22);
        break;
      case 0x74:
        out.push(0x09);
        break;
      case 0x62:
        out.push(0x08);
        break;
      case 0x66:
        out.push(0x0c);
        break;
      case 0x6e:
        if (out.length !== 0) {
          fail("E-CODEC-TOKEN", source, "$", "the \\n escape is canonical only for an initial decoded LF", {
            start_byte: span.start_byte + escapeStart,
            end_byte: span.start_byte + index,
          });
        }
        out.push(0x0a);
        break;
      case 0x75: {
        if (index + 4 > contentEnd) {
          fail("E-CODEC-TOKEN", source, "$", "truncated lowercase \\uXXXX escape", span);
        }
        const digits = String.fromCharCode(token[index], token[index + 1], token[index + 2], token[index + 3]);
        if (!/^[0-9a-f]{4}$/.test(digits)) {
          fail("E-CODEC-TOKEN", source, "$", "Unicode escapes require exactly four lowercase hexadecimal digits", {
            start_byte: span.start_byte + escapeStart,
            end_byte: span.start_byte + index + 4,
          });
        }
        index += 4;
        const scalar = Number.parseInt(digits, 16);
        const canonicalLongControl =
          (scalar >= 0 && scalar < 0x20 && ![0x08, 0x09, 0x0a, 0x0c, 0x0d].includes(scalar)) ||
          scalar === 0x7f;
        if (!canonicalLongControl) {
          fail("E-CODEC-SCALAR", source, "$", "Unicode escape is noncanonical or does not denote a permitted control scalar", {
            start_byte: span.start_byte + escapeStart,
            end_byte: span.start_byte + index,
          });
        }
        pushScalar(out, scalar);
        break;
      }
      default:
        fail("E-CODEC-TOKEN", source, "$", "unsupported or noncanonical Markdown escape", {
          start_byte: span.start_byte + escapeStart,
          end_byte: span.start_byte + index,
        });
    }
  }

  const decoded = Uint8Array.from(out);
  decodeFatalUtf8Lf(decoded, source);
  return decoded;
}

export { bytesEqual } from "./kernel.ts";
