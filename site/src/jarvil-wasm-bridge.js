// Bridge between CodeMirror and the Jarvil compiler running as WebAssembly.
//
// There is no language server here and no JSON-RPC. `jarvil-lsp` cannot compile
// to wasm (tokio has no `io-std` there) and would be pointless if it could:
// LSP is a transport for talking to a separate process, and a browser tab has
// none. So this calls the analysis functions directly -- the same ones the real
// language server calls, so the browser and the editor cannot disagree.
//
// # Offsets
//
// The compiler works in **byte** offsets; JavaScript strings are indexed in
// **UTF-16 code units**. They coincide for ASCII and diverge the moment anyone
// types an accent or an emoji, at which point every range on the line is wrong.
// Everything crossing this boundary is converted.

let wasm = null;
let loading = null;

/** Loads the compiler, once, on first use. */
export async function loadCompiler() {
  if (wasm) return wasm;

  if (!loading) {
    loading = import("./wasm/jarvil_wasm.js").then(async (module) => {
      await module.default();
      wasm = module;
      return wasm;
    });
  }

  return loading;
}

const encoder = new TextEncoder();

/**
 * Byte offset of a UTF-16 index.
 *
 * An index can fall *between* the halves of a surrogate pair -- an emoji spans
 * two UTF-16 units. Slicing there would split the character, and `TextEncoder`
 * substitutes U+FFFD for the lone half, inflating the count by three bytes and
 * shifting every range after it. So a mid-pair index snaps back to the start of
 * its character, matching what `toUtf16Index` returns for the same position.
 */
export function toByteOffset(text, utf16Index) {
  let index = Math.max(0, Math.min(utf16Index, text.length));

  if (index > 0 && index < text.length) {
    const unit = text.charCodeAt(index);

    // a low surrogate here means `index` is the tail half of a pair
    if (unit >= 0xdc00 && unit <= 0xdfff) index -= 1;
  }

  return encoder.encode(text.slice(0, index)).length;
}

/**
 * UTF-16 index of a byte offset.
 *
 * Walks the string accumulating byte lengths rather than decoding, which is
 * enough for the sizes a playground deals with.
 */
export function toUtf16Index(text, byteOffset) {
  if (byteOffset <= 0) return 0;

  let bytes = 0;

  for (let i = 0; i < text.length; ) {
    const codePoint = text.codePointAt(i);
    const width = codePoint > 0xffff ? 2 : 1;

    // length in UTF-8 of this code point
    const size =
      codePoint < 0x80 ? 1 : codePoint < 0x800 ? 2 : codePoint < 0x10000 ? 3 : 4;

    if (bytes + size > byteOffset) return i;

    bytes += size;
    i += width;
  }

  return text.length;
}

/** Every diagnostic for `source`, with ranges converted to UTF-16 indices. */
export async function diagnostics(source) {
  const compiler = await loadCompiler();

  return compiler.diagnostics(source).map((d) => ({
    ...d,
    from: toUtf16Index(source, d.start),
    to: toUtf16Index(source, d.end),
    labels: d.labels.map((label) => ({
      ...label,
      from: toUtf16Index(source, label.start),
      to: toUtf16Index(source, label.end),
    })),
  }));
}

/** Compiles to Python. Returns `{ python, diagnostics }`; `python` is null on error. */
export async function compile(source) {
  const compiler = await loadCompiler();
  const result = compiler.compile(source);

  return {
    python: result.python ?? null,
    diagnostics: result.diagnostics.map((d) => ({
      ...d,
      from: toUtf16Index(source, d.start),
      to: toUtf16Index(source, d.end),
    })),
  };
}

/** Hover information at a UTF-16 position, or null. */
export async function hover(source, utf16Pos) {
  const compiler = await loadCompiler();
  const found = compiler.hover(source, toByteOffset(source, utf16Pos));

  if (!found) return null;

  return {
    contents: found.contents,
    from: toUtf16Index(source, found.start),
    to: toUtf16Index(source, found.end),
  };
}

/** Declaration site for the symbol at a UTF-16 position, or null. */
export async function definition(source, utf16Pos) {
  const compiler = await loadCompiler();
  const found = compiler.definition(source, toByteOffset(source, utf16Pos));

  if (!found) return null;

  return {
    targetFrom: toUtf16Index(source, found.target_start),
    targetTo: toUtf16Index(source, found.target_end),
    originFrom: toUtf16Index(source, found.origin_start),
    originTo: toUtf16Index(source, found.origin_end),
  };
}
