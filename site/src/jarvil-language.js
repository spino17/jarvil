// A CodeMirror language mode for Jarvil.
//
// This is a stream parser rather than a Lezer grammar: it classifies tokens by
// shape, which is all highlighting needs, and avoids maintaining a second
// full grammar alongside the compiler's. Meaning-aware colouring would need a
// tree-sitter grammar -- see the limitations page.
//
// Keyword lists are the same ones the compiler's `constants::common` defines;
// `crates/jarvil-parser/tests/grammar.rs` guards the equivalent copy in the VS
// Code grammar against drift.

import { StreamLanguage } from "@codemirror/language";

const KEYWORDS = new Set([
  "if", "elif", "else", "for", "while", "continue", "break", "return",
  "match", "case", "in", "and", "or", "not",
]);

const DECLARATIONS = new Set([
  "def", "let", "type", "interface", "struct", "enum", "lambda",
  "declare", "implements",
]);

const ATOMIC_TYPES = new Set(["int", "float", "str", "bool"]);

const CONSTANTS = new Set(["True", "False"]);

// Reserved so the generated Python stays valid. Using one as an identifier is a
// syntax error, so they are flagged rather than left looking like normal names.
const RESERVED = new Set([
  "None", "as", "assert", "class", "del", "except", "finally", "from",
  "global", "import", "is", "nonlocal", "pass", "raise", "try", "with",
  "yield", "async", "await", "__peg_parser__",
]);

const jarvilMode = {
  name: "jarvil",

  startState() {
    return { inBlockComment: false };
  },

  token(stream, state) {
    if (state.inBlockComment) {
      while (!stream.eol()) {
        if (stream.match("*/")) {
          state.inBlockComment = false;
          break;
        }
        stream.next();
      }
      if (state.inBlockComment) stream.skipToEnd();
      return "comment";
    }

    if (stream.eatSpace()) return null;

    if (stream.match("//")) {
      stream.skipToEnd();
      return "comment";
    }

    if (stream.match("/*")) {
      state.inBlockComment = true;
      return "comment";
    }

    // strings, with escape handling so `"a\"b"` does not terminate early
    const quote = stream.peek();
    if (quote === '"' || quote === "'") {
      stream.next();
      let escaped = false;
      while (!stream.eol()) {
        const ch = stream.next();
        if (escaped) {
          escaped = false;
        } else if (ch === "\\") {
          escaped = true;
        } else if (ch === quote) {
          break;
        }
      }
      return "string";
    }

    if (stream.match(/^\d+\.\d+/)) return "number";
    if (stream.match(/^\d+/)) return "number";

    if (stream.match(/^[A-Za-z_]\w*/)) {
      const word = stream.current();

      if (RESERVED.has(word)) return "invalid";
      if (DECLARATIONS.has(word)) return "definitionKeyword";
      if (KEYWORDS.has(word)) return "keyword";
      if (ATOMIC_TYPES.has(word)) return "typeName";
      if (CONSTANTS.has(word)) return "bool";
      if (word === "self") return "self";

      // a capitalised name in this language is a type by convention
      if (/^[A-Z]/.test(word)) return "typeName";

      // an identifier immediately followed by `(` is being called
      if (stream.match(/^\s*(?=\()/, false)) return "variableName.function";

      return "variableName";
    }

    if (stream.match(/^(->|::|==|!=|<=|>=|\*\*)/)) return "operator";
    if (stream.match(/^[+\-*/<>=]/)) return "operator";
    if (stream.match(/^[[\]{}(),.:]/)) return "punctuation";

    stream.next();
    return null;
  },

  languageData: {
    commentTokens: { line: "//", block: { open: "/*", close: "*/" } },
    indentOnInput: /^\s*(elif|else|case)\b.*:$/,
  },
};

export const jarvil = StreamLanguage.define(jarvilMode);
