import { encodeMarkdownString } from "./markdown_codec.ts";
import { codePointSort } from "./kernel.ts";

const UTF8 = new TextEncoder();

export function encodeTomlBasicString(value: string): string {
  const out: string[] = ['"'];
  for (const scalar of value) {
    const code = scalar.codePointAt(0)!;
    switch (code) {
      case 0x08: out.push("\\b"); break;
      case 0x09: out.push("\\t"); break;
      case 0x0a: out.push("\\n"); break;
      case 0x0c: out.push("\\f"); break;
      case 0x0d: out.push("\\r"); break;
      case 0x22: out.push('\\"'); break;
      case 0x5c: out.push("\\\\"); break;
      default:
        if (code < 0x20 || code === 0x7f) out.push(`\\u${code.toString(16).padStart(4, "0")}`);
        else out.push(scalar);
    }
  }
  out.push('"');
  return out.join("");
}

export class CanonicalTomlWriter {
  readonly #blocks: string[][] = [];
  #current?: string[];

  table(path: string): void {
    this.#start(`[${path}]`);
  }

  arrayTable(path: string): void {
    this.#start(`[[${path}]]`);
  }

  #start(header: string): void {
    const block = [header];
    this.#blocks.push(block);
    this.#current = block;
  }

  #line(key: string, encoded: string): void {
    if (this.#current === undefined) throw new Error("TOML assignment requires a table header");
    this.#current.push(`${key} = ${encoded}`);
  }

  string(key: string, value: string): void {
    this.#line(key, encodeTomlBasicString(value));
  }

  markdown(key: string, value: Uint8Array): void {
    this.#line(key, encodeMarkdownString(value));
  }

  integer(key: string, value: number): void {
    if (!Number.isSafeInteger(value)) throw new Error(`${key} is not a safe integer`);
    this.#line(key, String(value));
  }

  number(key: string, value: number): void {
    if (!Number.isFinite(value)) throw new Error(`${key} is not finite`);
    this.#line(key, String(value));
  }

  boolean(key: string, value: boolean): void {
    this.#line(key, value ? "true" : "false");
  }

  strings(key: string, values: readonly string[], sort = false): void {
    const entries = sort ? [...values].sort(codePointSort) : [...values];
    this.#line(key, `[${entries.map(encodeTomlBasicString).join(", ")}]`);
  }

  /** One element per line: an ordered ID list stays reviewable and hand-editable at any length. */
  stringList(key: string, values: readonly string[]): void {
    this.#line(
      key,
      values.length === 0
        ? "[]"
        : `[\n${values.map((value) => `  ${encodeTomlBasicString(value)},`).join("\n")}\n]`,
    );
  }

  finish(): Uint8Array {
    if (this.#blocks.length === 0) throw new Error("canonical TOML document cannot be empty");
    return UTF8.encode(`${this.#blocks.map((block) => block.join("\n")).join("\n\n")}\n`);
  }
}
