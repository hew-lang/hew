//! Hew runtime: Markdown to HTML conversion.
//!
//! Provides Markdown-to-HTML rendering for compiled Hew programs using
//! [`pulldown_cmark`]. All returned strings are allocated with `libc::malloc`
//! and NUL-terminated.

#[cfg(feature = "export-meta")]
pub mod export_meta;

use hew_cabi::cabi::{cstr_to_str, str_to_malloc};
use std::ffi::c_char;

use pulldown_cmark::{html, Options, Parser};

/// Strip raw HTML tags from the rendered HTML output.
///
/// This performs a simple pass that removes `<`…`>` sequences that look like
/// HTML tags (not entities). It is intentionally conservative: it removes
/// tags but preserves text content and HTML entities.
fn strip_html_tags(html_str: &str) -> String {
    let mut result = String::with_capacity(html_str.len());
    let mut chars = html_str.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == '<' {
            // Skip until '>' (consume the tag).
            for inner in chars.by_ref() {
                if inner == '>' {
                    break;
                }
            }
        } else {
            result.push(ch);
        }
    }
    result
}

// ---------------------------------------------------------------------------
// C ABI exports
// ---------------------------------------------------------------------------

/// Convert a Markdown string to HTML.
///
/// Returns a `malloc`-allocated, NUL-terminated C string containing the
/// rendered HTML. The caller must free it with `libc::free`.
/// Returns null on error.
///
/// # Safety
///
/// `md` must be a valid NUL-terminated C string.
#[cfg_attr(
    feature = "export-meta",
    hew_export_macro::hew_export(
        module = "std::encoding::markdown",
        doc = "Render Markdown to HTML"
    )
)]
#[no_mangle]
pub unsafe extern "C" fn hew_markdown_to_html(md: *const c_char) -> *mut c_char {
    // SAFETY: md is a valid NUL-terminated C string per caller contract.
    let Some(md_str) = (unsafe { cstr_to_str(md) }) else {
        return std::ptr::null_mut();
    };
    let parser = Parser::new_ext(md_str, Options::all());
    let mut html_output = String::new();
    html::push_html(&mut html_output, parser);
    str_to_malloc(&html_output)
}

/// Convert a Markdown string to sanitized HTML (raw HTML tags stripped).
///
/// Like [`hew_markdown_to_html`] but additionally strips any raw HTML tags
/// from the output, leaving only the Markdown-generated structure and text.
/// Returns a `malloc`-allocated, NUL-terminated C string. The caller must
/// be freed with `libc::free`. Returns null on error.
///
/// # Safety
///
/// `md` must be a valid NUL-terminated C string.
#[cfg_attr(
    feature = "export-meta",
    hew_export_macro::hew_export(
        module = "std::encoding::markdown",
        doc = "Render Markdown to HTML with raw HTML tags stripped"
    )
)]
#[no_mangle]
pub unsafe extern "C" fn hew_markdown_to_html_safe(md: *const c_char) -> *mut c_char {
    // SAFETY: md is a valid NUL-terminated C string per caller contract.
    let Some(md_str) = (unsafe { cstr_to_str(md) }) else {
        return std::ptr::null_mut();
    };
    let parser = Parser::new_ext(md_str, Options::all());
    let mut html_output = String::new();
    html::push_html(&mut html_output, parser);
    let safe = strip_html_tags(&html_output);
    str_to_malloc(&safe)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    /// Helper: convert markdown, read the result, and free it.
    unsafe fn md_to_html(md: &str) -> String {
        let c = CString::new(md).unwrap();
        // SAFETY: c is a valid NUL-terminated C string.
        let ptr = unsafe { hew_markdown_to_html(c.as_ptr()) };
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid NUL-terminated C string from malloc.
        let s = unsafe { cstr_to_str(ptr) }.unwrap().to_owned();
        // SAFETY: ptr was allocated with malloc.
        unsafe { libc::free(ptr.cast()) };
        s
    }

    #[test]
    fn heading() {
        // SAFETY: test helper uses valid pointers.
        let html = unsafe { md_to_html("# Hello World") };
        assert!(html.contains("<h1>"), "expected <h1> in: {html}");
        assert!(html.contains("Hello World"), "expected text in: {html}");
    }

    #[test]
    fn unordered_list() {
        // SAFETY: test helper uses valid pointers.
        let html = unsafe { md_to_html("- item one\n- item two\n- item three") };
        assert!(html.contains("<ul>"), "expected <ul> in: {html}");
        assert!(html.contains("<li>"), "expected <li> in: {html}");
        assert!(html.contains("item two"), "expected text in: {html}");
    }

    #[test]
    fn code_block() {
        let md = "```rust\nfn main() {}\n```";
        // SAFETY: test helper uses valid pointers.
        let html = unsafe { md_to_html(md) };
        assert!(html.contains("<code"), "expected <code in: {html}");
        assert!(html.contains("fn main()"), "expected code text in: {html}");
    }

    #[test]
    fn link() {
        // SAFETY: test helper uses valid pointers.
        let html = unsafe { md_to_html("[Hew](https://hew.sh)") };
        assert!(html.contains("<a"), "expected <a in: {html}");
        assert!(html.contains("https://hew.sh"), "expected href in: {html}");
        assert!(html.contains("Hew"), "expected link text in: {html}");
    }

    #[test]
    fn safe_strips_raw_html() {
        let md = "Hello <script>alert('xss')</script> world";
        let c = CString::new(md).unwrap();
        // SAFETY: c is a valid NUL-terminated C string.
        let ptr = unsafe { hew_markdown_to_html_safe(c.as_ptr()) };
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid NUL-terminated C string from malloc.
        let s = unsafe { cstr_to_str(ptr) }.unwrap().to_owned();
        // SAFETY: ptr was allocated with malloc.
        unsafe { libc::free(ptr.cast()) };
        assert!(!s.contains("<script>"), "raw HTML should be stripped: {s}");
        assert!(s.contains("Hello"), "text should be preserved: {s}");
    }

    #[test]
    fn null_safety() {
        // SAFETY: testing null pointer handling — should not crash.
        unsafe {
            assert!(hew_markdown_to_html(std::ptr::null()).is_null());
            assert!(hew_markdown_to_html_safe(std::ptr::null()).is_null());
        }
    }
}
