// Translation between the compiler's byte offsets and LSP positions.
//
// This is fiddlier than it looks. LSP positions are (line, character) pairs
// where `character` counts UTF-16 code units by default, not bytes and not
// characters. For pure-ASCII source all three coincide, which is exactly why
// getting it wrong stays invisible until someone puts a non-ASCII character in
// a string literal or a comment and every range on that line shifts.

use tower_lsp::lsp_types::{Position, Range};

pub struct LineIndex {
    // byte offset at which each line starts
    line_starts: Vec<u32>,
    text: String,
}

impl LineIndex {
    pub fn new(text: &str) -> Self {
        let mut line_starts = vec![0];

        for (offset, byte) in text.bytes().enumerate() {
            if byte == b'\n' {
                line_starts.push(offset as u32 + 1);
            }
        }

        LineIndex {
            line_starts,
            text: text.to_string(),
        }
    }

    pub fn text(&self) -> &str {
        &self.text
    }

    // Byte offset of an LSP position. Clamped rather than failing: an editor can
    // legitimately ask about a position one past the end of a line, and a
    // slightly-off answer beats refusing to respond.
    pub fn offset(&self, position: Position) -> u32 {
        let line = position.line as usize;

        let Some(&line_start) = self.line_starts.get(line) else {
            return self.text.len() as u32;
        };

        let line_end = self
            .line_starts
            .get(line + 1)
            .map(|&next| next - 1)
            .unwrap_or(self.text.len() as u32);

        let line_text = &self.text[line_start as usize..line_end as usize];

        // walk the line accumulating UTF-16 units until we reach `character`
        let mut utf16_units = 0;

        for (byte_offset, ch) in line_text.char_indices() {
            if utf16_units >= position.character {
                return line_start + byte_offset as u32;
            }

            utf16_units += ch.len_utf16() as u32;
        }

        line_end
    }

    // LSP position of a byte offset.
    pub fn position(&self, offset: u32) -> Position {
        let offset = offset.min(self.text.len() as u32);

        // index of the last line starting at or before `offset`
        let line = match self.line_starts.binary_search(&offset) {
            Ok(exact) => exact,
            Err(insertion_point) => insertion_point - 1,
        };

        let line_start = self.line_starts[line];

        let character = self.text[line_start as usize..offset as usize]
            .chars()
            .map(|ch| ch.len_utf16() as u32)
            .sum();

        Position {
            line: line as u32,
            character,
        }
    }

    pub fn range(&self, start: u32, end: u32) -> Range {
        Range {
            start: self.position(start),
            end: self.position(end),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn maps_positions_on_ascii_text() {
        let index = LineIndex::new("def main():\n    print(1)\n");

        assert_eq!(index.offset(Position::new(0, 0)), 0);
        assert_eq!(index.offset(Position::new(1, 4)), 16);
        assert_eq!(index.position(16), Position::new(1, 4));
    }

    #[test]
    fn round_trips_every_offset() {
        let text = "def f():\n    let x = 1\n    return x\n";
        let index = LineIndex::new(text);

        for offset in 0..text.len() as u32 {
            let position = index.position(offset);

            assert_eq!(
                index.offset(position),
                offset,
                "offset {} did not round-trip via {:?}",
                offset,
                position
            );
        }
    }

    #[test]
    fn counts_utf16_units_not_bytes() {
        // `é` is two bytes but one UTF-16 unit; an emoji outside the BMP is
        // four bytes and two UTF-16 units. A byte-counting implementation
        // passes the first case and fails the second.
        let text = "let s = \"é\"\nlet t = \"😀\"\n";
        let index = LineIndex::new(text);

        let e_byte = text.find('é').unwrap() as u32;
        assert_eq!(index.position(e_byte), Position::new(0, 9));

        let emoji_byte = text.find('😀').unwrap() as u32;

        // `let t = "` is nine ASCII characters, so the emoji starts at 9
        let position = index.position(emoji_byte);
        assert_eq!(position, Position::new(1, 9));

        // and spans two UTF-16 units despite being four bytes, so the closing
        // quote after it is at 11, not 10 and not 13
        let after_emoji = index.position(emoji_byte + '😀'.len_utf8() as u32);
        assert_eq!(after_emoji, Position::new(1, 11));

        // and the mapping is invertible across the astral character
        assert_eq!(index.offset(Position::new(1, 11)), emoji_byte + 4);
    }

    #[test]
    fn clamps_positions_past_the_end() {
        let index = LineIndex::new("abc\n");

        assert_eq!(index.offset(Position::new(99, 0)), 4);
        assert_eq!(index.offset(Position::new(0, 99)), 3);
    }
}
