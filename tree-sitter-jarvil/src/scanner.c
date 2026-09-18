// External scanner for Jarvil's indentation.
//
// tree-sitter has no notion of significant whitespace, so a language whose
// blocks are delimited by indentation has to supply one. This emits three
// tokens the grammar treats as block delimiters:
//
//   NEWLINE   ends a statement
//   INDENT    opens a block
//   DEDENT    closes one (possibly several in a row)
//
// The approach is the same one tree-sitter-python takes: keep a stack of open
// indentation columns, compare each line's leading whitespace against the top
// of it, and emit the difference.
//
// Two details matter for correctness:
//
//   - Blank lines and comment-only lines carry no indentation information and
//     must not close a block. A file is full of them, so getting this wrong
//     breaks almost everything.
//   - At end of input every open block must be closed, or the tree is left
//     with unterminated nodes.
//   - The NEWLINE that terminates the last statement of a file with no trailing
//     line break is necessarily zero-width, and `source_file` accepts a bare
//     NEWLINE as a repetition element. Emitting it more than once would let the
//     parser shift it, re-scan the same position and loop forever, so it is
//     emitted at most once -- see `emitted_eof_newline`.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tree_sitter/parser.h>

enum TokenType {
  NEWLINE,
  INDENT,
  DEDENT,
};

// Indentation columns of the currently open blocks. Index 0 is always 0, the
// top level, so there is always something to compare against.
#define MAX_DEPTH 256

typedef struct {
  uint16_t depth;
  // Whether the zero-width NEWLINE at end of input has already been emitted.
  bool emitted_eof_newline;
  uint16_t columns[MAX_DEPTH];
} Scanner;

void *tree_sitter_jarvil_external_scanner_create(void) {
  Scanner *scanner = calloc(1, sizeof(Scanner));
  scanner->depth = 1;
  scanner->columns[0] = 0;
  scanner->emitted_eof_newline = false;
  return scanner;
}

void tree_sitter_jarvil_external_scanner_destroy(void *payload) {
  free(payload);
}

unsigned tree_sitter_jarvil_external_scanner_serialize(void *payload, char *buffer) {
  Scanner *scanner = (Scanner *)payload;
  unsigned size = 0;

  buffer[size++] = (char)scanner->depth;
  buffer[size++] = (char)scanner->emitted_eof_newline;

  for (uint16_t i = 0; i < scanner->depth && size + 1 < TREE_SITTER_SERIALIZATION_BUFFER_SIZE; i++) {
    buffer[size++] = (char)scanner->columns[i];
  }

  return size;
}

void tree_sitter_jarvil_external_scanner_deserialize(void *payload, const char *buffer,
                                                     unsigned length) {
  Scanner *scanner = (Scanner *)payload;

  scanner->depth = 1;
  scanner->columns[0] = 0;
  scanner->emitted_eof_newline = false;

  if (length == 0) return;

  unsigned size = 0;
  uint16_t depth = (uint8_t)buffer[size++];

  if (size < length) scanner->emitted_eof_newline = (bool)buffer[size++];

  for (uint16_t i = 0; i < depth && size < length; i++) {
    scanner->columns[i] = (uint8_t)buffer[size++];
  }

  scanner->depth = depth > 0 ? depth : 1;
}

static void advance(TSLexer *lexer) { lexer->advance(lexer, false); }
static void skip(TSLexer *lexer) { lexer->advance(lexer, true); }

bool tree_sitter_jarvil_external_scanner_scan(void *payload, TSLexer *lexer,
                                              const bool *valid_symbols) {
  Scanner *scanner = (Scanner *)payload;

  // End of file: close every block that is still open, then allow a final
  // NEWLINE so the last statement terminates.
  //
  // Both tokens here are zero-width. DEDENT is safe to repeat because each one
  // pops the stack and so cannot recur forever, but NEWLINE changes no state:
  // `source_file` accepts a bare NEWLINE as a repetition element, so emitting
  // it unconditionally would have the parser shift it and scan this same
  // position again without end. Emit it once and then decline.
  if (lexer->eof(lexer)) {
    if (valid_symbols[DEDENT] && scanner->depth > 1) {
      scanner->depth--;
      lexer->result_symbol = DEDENT;
      return true;
    }

    if (valid_symbols[NEWLINE] && !scanner->emitted_eof_newline) {
      scanner->emitted_eof_newline = true;
      lexer->result_symbol = NEWLINE;
      return true;
    }

    return false;
  }

  bool found_line_end = false;
  // Set once the token's end has been pinned in front of a comment, which also
  // suppresses the re-marking after the indentation scan below.
  bool ended_before_comment = false;
  uint16_t indent = 0;

  // Consume any run of line endings and the whitespace after them, tracking the
  // indentation of the last line that actually had content. Blank lines and
  // comment-only lines are skipped over rather than measured -- they say
  // nothing about block structure.
  for (;;) {
    if (lexer->lookahead == '\n' || lexer->lookahead == '\r') {
      found_line_end = true;
      indent = 0;
      skip(lexer);
      continue;
    }

    if (lexer->lookahead == ' ') {
      indent++;
      skip(lexer);
      continue;
    }

    if (lexer->lookahead == '\t') {
      // Treat a tab as the next multiple of eight, matching how most editors
      // render it. Jarvil's own lexer rejects tabs for indentation, so this
      // only has to be self-consistent.
      indent += 8 - (indent % 8);
      skip(lexer);
      continue;
    }

    // A comment occupying the rest of the line carries no structure.
    if (found_line_end && lexer->lookahead == '/') {
      // Cannot un-read, so only skip when it really is a comment. A single `/`
      // is division and must be handed back to the grammar.
      //
      // Pin the token's end in front of the first comment and leave it there.
      // Scanning on to find the next line's indentation moves the lexer past
      // the comment, and re-marking afterwards would stretch this token over
      // it -- the comment would then be inside a NEWLINE rather than a node of
      // its own, so a standalone comment would never be highlighted.
      if (!ended_before_comment) lexer->mark_end(lexer);
      advance(lexer);

      if (lexer->lookahead == '/') {
        ended_before_comment = true;
        while (!lexer->eof(lexer) && lexer->lookahead != '\n') skip(lexer);
        continue;
      }

      if (lexer->lookahead == '*') {
        // A block comment may span lines, so stop rather than try to measure
        // past it. The scan after the grammar has taken the comment measures
        // whichever line it actually ends on.
        ended_before_comment = true;
        break;
      }

      // It was division at the start of a line: fall through and let the
      // indentation logic below run with what we measured.
      break;
    }

    break;
  }

  if (lexer->eof(lexer)) {
    if (valid_symbols[DEDENT] && scanner->depth > 1) {
      scanner->depth--;
      lexer->result_symbol = DEDENT;
      return true;
    }

    if (found_line_end && valid_symbols[NEWLINE]) {
      lexer->result_symbol = NEWLINE;
      return true;
    }

    return false;
  }

  // No line break to consume. This happens on the scan immediately after a
  // NEWLINE was emitted: that token already skipped past the line break and the
  // indentation, so there is nothing left to measure -- yet blocks may still be
  // waiting to close at this column. Ask the lexer where we are instead.
  if (!found_line_end) {
    // Sitting directly on a comment, because the previous token stopped in
    // front of one. The column here belongs to the comment, and a comment says
    // nothing about block structure -- measuring it would close every block
    // indented past a comment written at the margin. Decline instead and let
    // the grammar take the comment as an extra; the scan after it sees a real
    // line end and measures the next line of code.
    if (lexer->lookahead == '/') return false;

    if (valid_symbols[DEDENT] && scanner->depth > 1 &&
        lexer->get_column(lexer) < scanner->columns[scanner->depth - 1]) {
      scanner->depth--;
      lexer->result_symbol = DEDENT;
      return true;
    }

    return false;
  }

  // Unless the end is already pinned in front of a comment, the token runs to
  // here: past the line break and the indentation it introduced.
  if (!ended_before_comment) lexer->mark_end(lexer);

  uint16_t current = scanner->columns[scanner->depth - 1];

  // Deeper than the enclosing block: open a new one.
  if (indent > current) {
    if (valid_symbols[INDENT] && scanner->depth < MAX_DEPTH) {
      scanner->columns[scanner->depth++] = indent;
      lexer->result_symbol = INDENT;
      return true;
    }
  }

  // Shallower: close blocks until the stack matches. One DEDENT per call, so
  // several closing at once are emitted over successive scans.
  if (indent < current) {
    if (valid_symbols[DEDENT] && scanner->depth > 1) {
      scanner->depth--;
      lexer->result_symbol = DEDENT;
      return true;
    }
  }

  // Same depth: just a statement boundary.
  if (valid_symbols[NEWLINE]) {
    lexer->result_symbol = NEWLINE;
    return true;
  }

  return false;
}
