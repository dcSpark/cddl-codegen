import { RoadmapWireError, bytesEqual, decodeMarkdownToken, encodeMarkdownString } from "../markdown_codec.ts";
import type { RoadmapIssue } from "../errors.ts";
import type { SelfTestCandidateCase as SelfTestCase, SelfTestCandidateResult as SelfTestResult } from "../selftest.ts";
import { observeSelfTestIssue } from "./observations.ts";
import { expectString, type DecodeContext } from "../decode/primitives.ts";
import {
  childLogicalPath,
  placeholderCandidate,
  shieldTomlMarkdown,
  type MarkdownBindings,
} from "../decode/raw_markdown.ts";

const UTF8 = new TextEncoder();
const text = (value: string): Uint8Array => UTF8.encode(value);

export const REQUIRED_CODEC_SELFTEST_CASE_IDS = [
  "codec_delimiters",
  "codec_backslash_unicode",
  "codec_fences_comments_markers",
  "codec_tabs_controls_spaces",
  "codec_bun_tab_formfeed_regression",
  "codec_bun_low_control_shield",
  "codec_placeholder_collision",
  "codec_placeholder_line_count",
  "codec_placeholder_path_mismatch",
  "codec_leading_trailing_blanks",
  "codec_eof_lf",
  "codec_eof_none",
  "codec_eof_multibyte",
  "codec_table_list_lazy_continuation",
  "codec_unicode_no_normalization",
  "codec_invalid_utf8",
  "codec_crlf_rejected",
  "codec_bare_cr_rejected",
  "codec_surrogate_escape_rejected",
  "codec_malformed_token_rejected",
  "codec_alternate_string_form_rejected",
  "codec_nonleading_lf_is_physical",
  "codec_toml_terminal_newline",
  "codec_shields_every_multiline_basic_token",
  "codec_comment_delimiter_ignored",
  "codec_basic_and_literal_delimiter_ignored",
  "codec_multiline_literal_delimiter_ignored",
  "codec_quoted_and_dotted_key_binding",
  "codec_array_of_tables_index_binding",
  "codec_false_placeholder_plain_string",
  "codec_false_placeholder_escaped_string",
  "codec_placeholder_all_tokens_consumed",
  "codec_quote_runs_three_four_five",
] as const;

export type RequiredCodecSelfTestCaseId = (typeof REQUIRED_CODEC_SELFTEST_CASE_IDS)[number];

function assert(condition: unknown, message: string): asserts condition {
  if (!condition) throw new Error(message);
}

function assertBytes(actual: Uint8Array, expected: Uint8Array, message: string): void {
  assert(bytesEqual(actual, expected), `${message}: ${JSON.stringify(new TextDecoder().decode(actual))}`);
}

function one(source: string, path = "value"): { bindings: MarkdownBindings; value: Uint8Array } {
  const bindings = shieldTomlMarkdown(text(source), `<codec:${path}>`);
  assert(bindings.parsed !== null && typeof bindings.parsed === "object" && !Array.isArray(bindings.parsed), "root table");
  const value = Object.getOwnPropertyDescriptor(bindings.parsed, path)?.value;
  const decoded = bindings.expectMarkdown(value, path);
  bindings.assertAllConsumed();
  return { bindings, value: decoded };
}

function expectCode(run: () => unknown, code: string, path?: string): void {
  try {
    run();
  } catch (error) {
    assert(error instanceof RoadmapWireError, `expected RoadmapWireError, got ${String(error)}`);
    assert(error.issue.code === code, `expected ${code}, got ${error.issue.code}`);
    if (path !== undefined) assert(error.issue.logical_path === path, `expected path ${path}, got ${error.issue.logical_path}`);
    observeSelfTestIssue(error.issue);
    return;
  }
  throw new Error(`expected ${code}`);
}

const tests: Record<RequiredCodecSelfTestCaseId, () => void> = {
  codec_delimiters: () => {
    const raw = text('quotes """" and apostrophes \'\'\'\' plus \\');
    assertBytes(one(`value = ${encodeMarkdownString(raw)}\n`).value, raw, "delimiter round trip");
    assertBytes(one(`value = ${encodeMarkdownString(new Uint8Array())}\n`).value, new Uint8Array(), "empty Markdown round trip");
  },
  codec_backslash_unicode: () => {
    const raw = text("C:\\fixture\\path and literal \\u1234; 日本語 😀");
    assertBytes(one(`value = ${encodeMarkdownString(raw)}\n`).value, raw, "backslash/unicode round trip");
    assertBytes(
      decodeMarkdownToken(text('"""é\\u0000"""'), "<unicode-before-escape>", { start_byte: 0, end_byte: 14 }),
      Uint8Array.from([...text("é"), 0x00]),
      "Unicode byte offsets before canonical escapes",
    );
  },
  codec_fences_comments_markers: () => {
    const raw = text("```toml\n[[record]]\n```\n<!-- gen:fixture -->\n# not TOML\n");
    assertBytes(one(`value = ${encodeMarkdownString(raw)}\n`).value, raw, "fence/comment bytes");
  },
  codec_tabs_controls_spaces: () => {
    const raw = Uint8Array.from([0x4c, 0x09, 0x08, 0x0c, 0x00, 0x1f, 0x7f, 0x20, 0x20]);
    const encoded = encodeMarkdownString(raw);
    assert(encoded.includes("\\t\\b\\f\\u0000\\u001f\\u007f"), "canonical controls");
    assertBytes(one(`value = ${encoded}\n`).value, raw, "control bytes");
  },
  codec_bun_tab_formfeed_regression: () => {
    const source = 'value = """\\t\\f"""\n';
    const independent = one(source).value;
    assertBytes(independent, Uint8Array.from([0x09, 0x0c]), "independent TAB/FF");
    const bun: unknown = Bun.TOML.parse(source);
    assert(bun !== null && typeof bun === "object", "Bun canary table");
    const bunValue: unknown = Object.getOwnPropertyDescriptor(bun, "value")?.value;
    if (Bun.version === "1.3.14") assert(bunValue !== "\t\f", "Bun 1.3.14 canary must expose swapped controls");
  },
  codec_bun_low_control_shield: () => {
    const source = 'value = """\\u0000X\\u001f"""\n';
    assertBytes(one(source).value, Uint8Array.from([0x00, 0x58, 0x1f]), "low controls survive shield");
    let directRejected = false;
    try { Bun.TOML.parse(source); } catch { directRejected = true; }
    if (Bun.version === "1.3.14") assert(directRejected, "Bun 1.3.14 low-control canary");
  },
  codec_placeholder_collision: () => {
    const token = text('"""x"""');
    const first = placeholderCandidate(token, 0, 0);
    const bindings = shieldTomlMarkdown(text(`value = """x"""\nordinary = "${first}"\n`), "<collision>");
    assert(bindings.tokens[0].placeholder !== first, "colliding token probes independently");
    assert(bindings.parsed !== null && typeof bindings.parsed === "object", "parsed collision root");
    const value = Object.getOwnPropertyDescriptor(bindings.parsed, "value")?.value;
    assertBytes(bindings.expectMarkdown(value, "value"), text("x"), "collision decode");
    bindings.assertAllConsumed();
  },
  codec_placeholder_line_count: () => {
    const source = `# comment """\nplain = '"""'\nmultiline = '''"""\n'''\n[[row]]\n"quoted.key" = """a\nb\n"""\n[[row]]\nvalue = """z"""`;
    const bindings = shieldTomlMarkdown(text(source), "<line-count>");
    assert(bindings.tokens.length === 2, "only multiline-basic tokens shielded");
    assert(bindings.tokens[0].physical_lf_count === 2, "physical LFs retained");
    assert(bindings.tokens[1].physical_lf_count === 0, "no-LF token retained");
  },
  codec_placeholder_path_mismatch: () => {
    const bindings = shieldTomlMarkdown(text('[[row]]\nvalue = """x"""\n'), "<path-mismatch>");
    assert(bindings.parsed !== null && typeof bindings.parsed === "object", "root");
    const rows = Object.getOwnPropertyDescriptor(bindings.parsed, "row")?.value;
    assert(Array.isArray(rows) && rows[0] !== null && typeof rows[0] === "object", "row array");
    const value = Object.getOwnPropertyDescriptor(rows[0], "value")?.value;
    expectCode(() => bindings.expectMarkdown(value, "row[1].value"), "E-CODEC-PLACEHOLDER", "row[1].value");
  },
  codec_leading_trailing_blanks: () => {
    const raw = text("\nalpha\n\n");
    assertBytes(one(`value = ${encodeMarkdownString(raw)}\n`).value, raw, "leading/trailing blanks");
  },
  codec_eof_lf: () => assertBytes(one(`value = ${encodeMarkdownString(text("x\n"))}\n`).value, text("x\n"), "LF EOF"),
  codec_eof_none: () => assertBytes(one(`value = ${encodeMarkdownString(text("x"))}\n`).value, text("x"), "no-LF EOF"),
  codec_eof_multibyte: () => assertBytes(one(`value = ${encodeMarkdownString(text("😀"))}\n`).value, text("😀"), "multibyte EOF"),
  codec_table_list_lazy_continuation: () => {
    const raw = text("| A | B |\n| - | - |\n1. first\n3. gap\n   - nested\n     lazy continuation\n");
    assertBytes(one(`value = ${encodeMarkdownString(raw)}\n`).value, raw, "Markdown structure bytes");
  },
  codec_unicode_no_normalization: () => {
    const raw = text("e\u0301|é|مرحبا|😀");
    assertBytes(one(`value = ${encodeMarkdownString(raw)}\n`).value, raw, "Unicode normalization forbidden");
  },
  codec_invalid_utf8: () => expectCode(() => shieldTomlMarkdown(Uint8Array.from([0x76, 0x3d, 0x22, 0xff]), "<bad-utf8>"), "E-CODEC-UTF8"),
  codec_crlf_rejected: () => expectCode(() => shieldTomlMarkdown(text('value = "x"\r\n'), "<crlf>"), "E-CODEC-LINE-END"),
  codec_bare_cr_rejected: () => expectCode(() => shieldTomlMarkdown(text('value = "x"\r'), "<cr>"), "E-CODEC-LINE-END"),
  codec_surrogate_escape_rejected: () => expectCode(() => one('value = """\\ud800"""\n'), "E-CODEC-SCALAR"),
  codec_malformed_token_rejected: () => expectCode(() => shieldTomlMarkdown(text('value = """unterminated\n'), "<unterminated>"), "E-CODEC-TOKEN"),
  codec_alternate_string_form_rejected: () => {
    const bindings = shieldTomlMarkdown(text('value = "ordinary"\n'), "<alternate>");
    assert(bindings.parsed !== null && typeof bindings.parsed === "object", "root");
    expectCode(() => bindings.expectMarkdown(Object.getOwnPropertyDescriptor(bindings.parsed, "value")?.value, "value"), "E-CODEC-PLACEHOLDER", "value");
  },
  codec_nonleading_lf_is_physical: () => {
    const encoded = encodeMarkdownString(text("a\nb"));
    assert(encoded === '"""a\nb"""', "nonleading LF must remain physical");
  },
  codec_toml_terminal_newline: () => assert(`value = ${encodeMarkdownString(text("x"))}\n`.endsWith("\n"), "canonical TOML terminal LF"),
  codec_shields_every_multiline_basic_token: () => {
    const bindings = shieldTomlMarkdown(text('unknown = """x"""\nknown = """y"""\n'), "<all-token>");
    assert(bindings.tokens.length === 2, "scanner shields unknown and known tokens alike");
  },
  codec_comment_delimiter_ignored: () => assert(shieldTomlMarkdown(text('# """not a token"""\nvalue = "x"\n'), "<comment>").tokens.length === 0, "comment delimiter ignored"),
  codec_basic_and_literal_delimiter_ignored: () => assert(shieldTomlMarkdown(text('a = "\\\"\\\"\\\""\nb = \'"""\'\n'), "<single>").tokens.length === 0, "single-line contexts ignored"),
  codec_multiline_literal_delimiter_ignored: () => assert(shieldTomlMarkdown(text("value = '''\"\"\"\n'''\n"), "<literal>").tokens.length === 0, "multiline literal context ignored"),
  codec_quoted_and_dotted_key_binding: () => {
    const bindings = shieldTomlMarkdown(text('["quoted.key"]\ndotted.value = """x"""\n'), "<keys>");
    const expected = '$["quoted.key"].dotted.value';
    assert(bindings.tokens[0].logical_path === expected, `post-Bun path ${bindings.tokens[0].logical_path}`);
  },
  codec_array_of_tables_index_binding: () => {
    const bindings = shieldTomlMarkdown(text('[[row]]\nvalue = """a"""\n[[row]]\nvalue = """b"""\n'), "<aot>");
    assert(bindings.tokens[0].logical_path === "row[0].value" && bindings.tokens[1].logical_path === "row[1].value", "AoT indices bound after Bun parse");
  },
  codec_false_placeholder_plain_string: () => {
    const bindings = shieldTomlMarkdown(text('value = "__ROADMAP_MD_not_registered_0_0__"\n'), "<false-plain>");
    const ctx: DecodeContext = { source: bindings.source, bindings };
    assert(bindings.parsed !== null && typeof bindings.parsed === "object", "root");
    assert(expectString(ctx, Object.getOwnPropertyDescriptor(bindings.parsed, "value")?.value, "value").startsWith("__ROADMAP_MD_"), "unregistered placeholder-looking text is ordinary");
  },
  codec_false_placeholder_escaped_string: () => {
    const bindings = shieldTomlMarkdown(text('value = "\\u005f_ROADMAP_MD_not_registered_0_0__"\n'), "<false-escaped>");
    const ctx: DecodeContext = { source: bindings.source, bindings };
    assert(bindings.parsed !== null && typeof bindings.parsed === "object", "root");
    assert(expectString(ctx, Object.getOwnPropertyDescriptor(bindings.parsed, "value")?.value, "value").startsWith("__ROADMAP_MD_"), "escaped ordinary string remains ordinary");
  },
  codec_placeholder_all_tokens_consumed: () => {
    const bindings = shieldTomlMarkdown(text('a = """x"""\nb = """y"""\n'), "<consume>");
    assert(bindings.parsed !== null && typeof bindings.parsed === "object", "root");
    bindings.expectMarkdown(Object.getOwnPropertyDescriptor(bindings.parsed, "a")?.value, "a");
    expectCode(() => bindings.assertAllConsumed(), "E-CODEC-PLACEHOLDER", "b");
  },
  codec_quote_runs_three_four_five: () => {
    assertBytes(decodeMarkdownToken(text('""""""'), "<quotes>", { start_byte: 0, end_byte: 6 }), text(""), "empty three-quote close");
    assertBytes(decodeMarkdownToken(text('"""x"""'), "<quotes>", { start_byte: 0, end_byte: 7 }), text("x"), "three quotes");
    assertBytes(decodeMarkdownToken(text('"""x""""'), "<quotes>", { start_byte: 0, end_byte: 8 }), text('x"'), "four quotes");
    assertBytes(decodeMarkdownToken(text('"""x"""""'), "<quotes>", { start_byte: 0, end_byte: 9 }), text('x""'), "five quotes");
  },
};

function failure(id: string, error: unknown): RoadmapIssue {
  if (error instanceof RoadmapWireError) return error.issue;
  return {
    code: "E-SELFTEST-CASE",
    source: "<selftest>",
    logical_path: id,
    message: error instanceof Error ? error.message : String(error),
    exit: 1,
  };
}

const POSITIVE_CODEC_CASE_IDS: readonly RequiredCodecSelfTestCaseId[] = [
  "codec_delimiters",
  "codec_backslash_unicode",
  "codec_fences_comments_markers",
  "codec_tabs_controls_spaces",
  "codec_bun_tab_formfeed_regression",
  "codec_bun_low_control_shield",
  "codec_placeholder_collision",
  "codec_placeholder_line_count",
  "codec_leading_trailing_blanks",
  "codec_eof_lf",
  "codec_eof_none",
  "codec_eof_multibyte",
  "codec_table_list_lazy_continuation",
  "codec_unicode_no_normalization",
  "codec_nonleading_lf_is_physical",
  "codec_toml_terminal_newline",
  "codec_shields_every_multiline_basic_token",
  "codec_comment_delimiter_ignored",
  "codec_basic_and_literal_delimiter_ignored",
  "codec_multiline_literal_delimiter_ignored",
  "codec_quoted_and_dotted_key_binding",
  "codec_array_of_tables_index_binding",
  "codec_false_placeholder_plain_string",
  "codec_false_placeholder_escaped_string",
  "codec_quote_runs_three_four_five",
];

const NEGATIVE_CODEC_CASE_IDS: readonly RequiredCodecSelfTestCaseId[] = [
  "codec_placeholder_path_mismatch",
  "codec_invalid_utf8",
  "codec_crlf_rejected",
  "codec_bare_cr_rejected",
  "codec_surrogate_escape_rejected",
  "codec_malformed_token_rejected",
  "codec_alternate_string_form_rejected",
  "codec_placeholder_all_tokens_consumed",
];

const CODEC_CASE_POLARITY = new Map<RequiredCodecSelfTestCaseId, "positive" | "negative">([
  ...POSITIVE_CODEC_CASE_IDS.map((id) => [id, "positive"] as const),
  ...NEGATIVE_CODEC_CASE_IDS.map((id) => [id, "negative"] as const),
]);
assert(POSITIVE_CODEC_CASE_IDS.length + NEGATIVE_CODEC_CASE_IDS.length === REQUIRED_CODEC_SELFTEST_CASE_IDS.length, "codec case polarity metadata must declare each ID once");
assert(CODEC_CASE_POLARITY.size === REQUIRED_CODEC_SELFTEST_CASE_IDS.length, "codec case polarity metadata must cover each ID exactly once");
for (const id of REQUIRED_CODEC_SELFTEST_CASE_IDS) assert(CODEC_CASE_POLARITY.has(id), `missing explicit polarity for ${id}`);

export const CODEC_SELFTEST_CASES: readonly SelfTestCase[] = REQUIRED_CODEC_SELFTEST_CASE_IDS.map((id) => ({
  id,
  category: "codec" as const,
  run(): SelfTestResult {
    const polarity = CODEC_CASE_POLARITY.get(id)!;
    try {
      tests[id]();
      return { ok: true, polarity };
    } catch (error) {
      return { ok: false, polarity, issues: [failure(id, error)] };
    }
  },
}));

export function runCodecDirectSelfTests(): { executed: number; counts: Readonly<Record<string, number>> } {
  const counts: Record<string, number> = {};
  for (const id of REQUIRED_CODEC_SELFTEST_CASE_IDS) {
    tests[id]();
    counts[id] = (counts[id] ?? 0) + 1;
  }
  assert(Object.keys(counts).length === REQUIRED_CODEC_SELFTEST_CASE_IDS.length && Object.values(counts).every((count) => count === 1), "each codec case must run exactly once");
  return { executed: REQUIRED_CODEC_SELFTEST_CASE_IDS.length, counts };
}
