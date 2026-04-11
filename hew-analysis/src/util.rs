//! Pure utility functions: line offsets, word extraction, coordinate conversion.

use crate::OffsetSpan;

/// Return `None` for an empty vec, `Some(v)` otherwise.
#[must_use]
pub fn non_empty<T>(v: Vec<T>) -> Option<Vec<T>> {
    if v.is_empty() {
        None
    } else {
        Some(v)
    }
}

/// Compute byte offsets of each line start.
#[must_use]
pub fn compute_line_offsets(source: &str) -> Vec<usize> {
    let mut offsets = vec![0];
    for (i, ch) in source.bytes().enumerate() {
        if ch == b'\n' {
            offsets.push(i + 1);
        }
    }
    offsets
}

/// Convert byte offset to (line, character) — both 0-based, character in UTF-16 code units.
#[must_use]
pub fn offset_to_line_col(source: &str, line_offsets: &[usize], offset: usize) -> (usize, usize) {
    let line = line_offsets
        .partition_point(|&o| o <= offset)
        .saturating_sub(1);
    let line_start = line_offsets[line];
    let byte_col = offset - line_start;
    // Count UTF-16 code units from line start to offset
    let col_utf16 = source[line_start..line_start + byte_col]
        .chars()
        .map(char::len_utf16)
        .sum();
    (line, col_utf16)
}

/// Convert line/character (UTF-16 code units) to a byte offset in source,
/// using pre-computed line offsets.
#[must_use]
pub fn position_to_offset(source: &str, lo: &[usize], line: u32, character: u32) -> usize {
    let line_idx = line as usize;
    if line_idx < lo.len() {
        let line_start = lo[line_idx];
        let line_end = lo.get(line_idx + 1).copied().unwrap_or(source.len());
        let line_text = &source[line_start..line_end];
        let mut utf16_count = 0u32;
        let target = character;
        for (byte_idx, ch) in line_text.char_indices() {
            if utf16_count >= target {
                return line_start + byte_idx;
            }
            #[expect(
                clippy::cast_possible_truncation,
                reason = "len_utf16() returns 1 or 2, always fits in u32"
            )]
            {
                utf16_count += ch.len_utf16() as u32;
            }
        }
        // If we've consumed all chars, return end of line text
        line_end
    } else {
        source.len()
    }
}

/// Convert a byte-offset range to `(start_line, start_col, end_line, end_col)` as `u32`,
/// using pre-computed line offsets. Character offsets are in UTF-16 code units.
#[must_use]
#[expect(
    clippy::cast_possible_truncation,
    reason = "line/column values in source files will not exceed u32"
)]
pub fn span_to_line_col_range(
    source: &str,
    lo: &[usize],
    start: usize,
    end: usize,
) -> (u32, u32, u32, u32) {
    let (sl, sc) = offset_to_line_col(source, lo, start);
    let (el, ec) = offset_to_line_col(source, lo, end);
    (sl as u32, sc as u32, el as u32, ec as u32)
}

/// Extract the word (identifier) at a byte offset in the source.
/// Also tries `offset - 1` when the cursor is right after an identifier.
#[must_use]
pub fn word_at_offset(source: &str, offset: usize) -> Option<String> {
    if let Some(w) = word_at_offset_exact(source, offset) {
        return Some(w);
    }
    // Cursor may be right after the identifier end — try one position back.
    if offset > 0 {
        return word_at_offset_exact(source, offset - 1);
    }
    None
}

/// Extract the identifier at exactly `offset`, or try to extend it with a
/// preceding dot-separated qualifier (e.g. `http.listen` -> `"http.listen"`).
#[must_use]
fn word_at_offset_exact(source: &str, offset: usize) -> Option<String> {
    if offset >= source.len() {
        return None;
    }
    let bytes = source.as_bytes();
    let is_ident = |b: u8| b.is_ascii_alphanumeric() || b == b'_';
    if !is_ident(bytes[offset]) {
        return None;
    }
    let start = source[..offset]
        .bytes()
        .rev()
        .take_while(|b| is_ident(*b))
        .count();
    let end = source[offset..]
        .bytes()
        .take_while(|b| is_ident(*b))
        .count();
    let word_start = offset - start;
    let word_end = offset + end;
    let word = &source[word_start..word_end];
    if word.is_empty() {
        return None;
    }

    // Check for double-colon-qualified prefix (e.g. `Counter::increment`).
    if word_start >= 3 && bytes[word_start - 1] == b':' && bytes[word_start - 2] == b':' {
        let prefix_end = word_start - 2;
        let prefix_len = source[..prefix_end]
            .bytes()
            .rev()
            .take_while(|b| is_ident(*b))
            .count();
        if prefix_len > 0 {
            let qualified = &source[prefix_end - prefix_len..word_end];
            return Some(qualified.to_string());
        }
    }

    // Check for dot-qualified prefix (e.g. `http.listen`).
    if word_start >= 2 && bytes[word_start - 1] == b'.' {
        let prefix_end = word_start - 1;
        let prefix_len = source[..prefix_end]
            .bytes()
            .rev()
            .take_while(|b| is_ident(*b))
            .count();
        if prefix_len > 0 {
            let qualified = &source[prefix_end - prefix_len..word_end];
            return Some(qualified.to_string());
        }
    }

    Some(word.to_string())
}

/// Find the byte span of `name` within `source`, scanning forward from `search_from`.
///
/// Matches only at word boundaries (surrounded by non-ident bytes or start/end of
/// string). Returns the first match, falling back to `search_from..search_from+name.len()`
/// if none is found.
#[must_use]
pub fn find_name_span(source: &str, search_from: usize, name: &str) -> OffsetSpan {
    let is_ident = |b: u8| b.is_ascii_alphanumeric() || b == b'_';
    let bytes = source.as_bytes();
    let search_region = source.get(search_from..).unwrap_or("");
    let name_bytes = name.as_bytes();

    let mut i = 0;
    while i + name_bytes.len() <= search_region.len() {
        if search_region.as_bytes()[i..i + name_bytes.len()] == *name_bytes {
            // Check left word boundary
            let left_ok = if i == 0 {
                true
            } else {
                !is_ident(search_region.as_bytes()[i - 1])
            };
            // Check right word boundary
            let right_ok = match search_region.as_bytes().get(i + name_bytes.len()) {
                None => true,
                Some(&b) => !is_ident(b),
            };
            if left_ok && right_ok {
                let abs_start = search_from + i;
                return OffsetSpan {
                    start: abs_start,
                    end: abs_start + name_bytes.len(),
                };
            }
        }
        // Advance by one byte (safe: we work on ASCII identifier bytes in practice)
        i += 1;
        // Skip to next valid ident start to avoid partial UTF-8 issues
        while i < search_region.len() && (bytes[search_from + i] & 0b1100_0000) == 0b1000_0000 {
            i += 1;
        }
    }

    // Fallback: point to search_from
    let end = (search_from + name_bytes.len()).min(source.len());
    OffsetSpan {
        start: search_from,
        end,
    }
}

/// Find the simple identifier name at the given byte offset (no `dot/::` qualifiers).
/// Returns the word and its byte-offset span.
#[must_use]
pub fn simple_word_at_offset(source: &str, offset: usize) -> Option<(String, OffsetSpan)> {
    if offset >= source.len() {
        return None;
    }
    let bytes = source.as_bytes();
    let is_ident = |b: u8| b.is_ascii_alphanumeric() || b == b'_';

    let probe = if is_ident(bytes[offset]) {
        offset
    } else if offset > 0 && is_ident(bytes[offset - 1]) {
        offset - 1
    } else {
        return None;
    };

    let start = probe
        - source[..probe]
            .bytes()
            .rev()
            .take_while(|b| is_ident(*b))
            .count();
    let end = probe + source[probe..].bytes().take_while(|b| is_ident(*b)).count();
    let word = &source[start..end];
    if word.is_empty() {
        None
    } else {
        Some((word.to_string(), OffsetSpan { start, end }))
    }
}
