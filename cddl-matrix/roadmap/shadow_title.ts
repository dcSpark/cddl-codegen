import type { RoadmapName } from "./model/core.ts";

function utf8(bytes: Uint8Array): string | undefined {
  try {
    return new TextDecoder("utf-8", { fatal: true }).decode(bytes);
  } catch {
    return undefined;
  }
}

/** The strict ATX-heading title grammar used by durable legacy campaign reservations. */
export function markdownHeadingTitle(source: Uint8Array): string | undefined {
  const text = utf8(source);
  if (text === undefined) return undefined;
  const newline = text.indexOf("\n");
  const firstLine = text.slice(0, newline < 0 ? text.length : newline);
  return /^(#{1,6})[ \t]+(.+?)(?:[ \t]+#+)?[ \t]*$/u.exec(firstLine)?.[2];
}

/**
 * Recover the reviewed title spelling of one raw shadow record. Campaign reservations deliberately
 * do not call this: they remain heading-only. Matrix v0 additionally retains pre-normalization
 * top-level bullet owners, whose finite source spellings are decoded here without heuristics.
 */
export function shadowRecordSourceTitle(
  source: Uint8Array,
  namespace: RoadmapName,
): string | undefined {
  const heading = markdownHeadingTitle(source);
  if (heading !== undefined || namespace !== "matrix") return heading;
  const text = utf8(source);
  if (text === undefined) return undefined;
  const newline = text.indexOf("\n");
  const firstLine = text.slice(0, newline < 0 ? text.length : newline);
  if (!firstLine.startsWith("- ")) return undefined;

  const bold = /^- \*\*([\s\S]*?)\*\*/u.exec(text)?.[1];
  if (bold !== undefined) {
    const title = bold.replace(/\n\s*/gu, " ");
    return title.length === 0 ? undefined : title;
  }
  if (firstLine.startsWith("- Float-family")) return "Float-family table key domains";
  if (firstLine.startsWith("- Bytes/nint/float")) return "Bytes, nint, and float fixed map keys";
  if (firstLine.startsWith("- `.size`")) return "Signed int .size close-out";
  if (firstLine.startsWith("- File ")) return "Typed-tag fixed-payload report";
  if (firstLine.startsWith("- When ")) {
    const title = firstLine.slice(2).split(/ \(upstream|: /u)[0];
    return title === undefined || title.length === 0 ? undefined : title;
  }
  return undefined;
}
