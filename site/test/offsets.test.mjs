// Offset conversion between the compiler's byte offsets and CodeMirror's UTF-16
// indices. Wrong conversions are invisible on ASCII and shift every range on a
// line the moment someone types an accent or an emoji, so this checks the real
// implementation rather than a copy.
//
//   node test/offsets.test.mjs

import { toByteOffset, toUtf16Index } from "../src/jarvil-wasm-bridge.js";

const encoder = new TextEncoder();

const cases = [
  ['ascii only',      'def main():\n    print(1)\n'],
  ['2-byte (accent)', 'let café = "naïve"\n'],
  ['3-byte (CJK)',    'let 名前 = "日本語"\n'],
  ['4-byte (emoji)',  'let s = "😀 hi 🎉"\n'],
  ['mixed',           '// héllo 世界 😀\ndef main():\n    print("ünïcode")\n'],
];

const charStarts = (text) => { const s = new Set(); for (let i = 0; i < text.length;) { s.add(i); i += text.codePointAt(i) > 0xffff ? 2 : 1; } s.add(text.length); return s; };

let failures = 0;
for (const [label, text] of cases) {
  const starts = charStarts(text);
  const totalBytes = encoder.encode(text).length;
  let ok = true, note = '';

  for (let i = 0; i <= text.length; i++) {
    const back = toUtf16Index(text, toByteOffset(text, i));
    if (!starts.has(back)) { ok = false; note = `idx ${i} -> ${back} is not a char boundary`; break; }
    if (back > i)          { ok = false; note = `idx ${i} -> ${back} overshot`; break; }
    if (starts.has(i) && back !== i) { ok = false; note = `boundary ${i} did not round-trip (got ${back})`; break; }
  }
  // byte offsets must always yield an in-range boundary
  if (ok) for (let b = 0; b <= totalBytes; b++) {
    const idx = toUtf16Index(text, b);
    if (!starts.has(idx)) { ok = false; note = `byte ${b} -> ${idx} not a boundary`; break; }
  }

  console.log(`  ${ok ? '✓' : '✗'} ${label.padEnd(17)} ${String(text.length).padStart(2)} utf16 / ${String(totalBytes).padStart(2)} bytes ${note}`);
  if (!ok) failures++;
}
console.log(failures === 0 ? '\n  ✓ every boundary round-trips; mid-surrogate snaps back safely' : `\n  ${failures} FAILED`);
process.exit(failures ? 1 : 0);
