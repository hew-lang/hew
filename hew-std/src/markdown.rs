//! Hew runtime: Markdown to HTML conversion.
//!
//! Provides Markdown-to-HTML rendering for compiled Hew programs using
//! [`pulldown_cmark`]. Returned strings are header-aware Hew strings and are
//! NUL-terminated.
use hew_cabi::cabi::{cstr_to_str, str_to_malloc};
use std::ffi::c_char;

use pulldown_cmark::{html, CowStr, Event, Options, Parser, Tag, TagEnd};

/// URL schemes a sanitized document may link to or embed.
///
/// Anything else — `javascript:`, `data:`, `vbscript:`, `file:`, or a scheme
/// this list does not name — is refused. A relative or fragment URL carries no
/// scheme and is allowed.
const ALLOWED_URL_SCHEMES: [&str; 5] = ["http", "https", "mailto", "tel", "ftp"];

/// The destination a refused URL is rewritten to.
///
/// Dropping the attribute would leave a bare `<a>` that reads as a link but
/// goes nowhere silently; an explicit inert destination says the URL was
/// refused.
const REFUSED_URL: &str = "about:blank#refused";

/// Decide whether a link or image destination may appear in sanitized output.
///
/// The scheme is the text before the first `:`, but only when no `/`, `?`, or
/// `#` precedes it — `foo/bar:baz` is a relative path, not a `foo` URL. A
/// destination with no scheme is relative and is allowed.
fn url_is_allowed(url: &str) -> bool {
    let trimmed = url.trim_start_matches([' ', '\t', '\n', '\r', '\u{0b}', '\u{0c}']);
    let Some(colon) = trimmed.find(':') else {
        return true;
    };
    let before = &trimmed[..colon];
    if before.contains('/') || before.contains('?') || before.contains('#') {
        return true;
    }
    // A scheme is ALPHA *( ALPHA / DIGIT / "+" / "-" / "." ) per RFC 3986 §3.1.
    // Anything else is not a scheme, so the destination is relative.
    let mut chars = before.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !first.is_ascii_alphabetic() {
        return true;
    }
    if !chars.all(|c| c.is_ascii_alphanumeric() || c == '+' || c == '-' || c == '.') {
        return true;
    }
    ALLOWED_URL_SCHEMES
        .iter()
        .any(|scheme| before.eq_ignore_ascii_case(scheme))
}

fn sanitize_url(url: &CowStr<'_>) -> CowStr<'static> {
    if url_is_allowed(url) {
        CowStr::Boxed(url.to_string().into_boxed_str())
    } else {
        CowStr::Borrowed(REFUSED_URL)
    }
}

/// Rewrite a parsed Markdown event stream so the rendered HTML is safe to
/// embed.
///
/// Two things are policed, and only two:
///
/// * Raw HTML from the source document is dropped. `Event::Html`,
///   `Event::InlineHtml`, and the two raw-HTML block events carry text the
///   author wrote verbatim, which is where `<script>`, `<img onerror=...>`,
///   and event attributes come from. They are removed rather than escaped, so
///   nothing the author wrote reaches the output as markup.
/// * Link and image destinations are checked against an explicit scheme
///   policy and rewritten to an inert URL when refused.
///
/// Markdown-generated structure — headings, emphasis, lists, code, tables,
/// allowed links — is untouched, because the generator, not the author,
/// produced it.
fn sanitize_events<'a, I>(events: I) -> Vec<Event<'a>>
where
    I: Iterator<Item = Event<'a>>,
{
    let mut out = Vec::new();
    for event in events {
        match event {
            Event::Html(_)
            | Event::InlineHtml(_)
            | Event::Start(Tag::HtmlBlock)
            | Event::End(TagEnd::HtmlBlock) => {}
            Event::Start(Tag::Link {
                link_type,
                dest_url,
                title,
                id,
            }) => out.push(Event::Start(Tag::Link {
                link_type,
                dest_url: sanitize_url(&dest_url),
                title: CowStr::Boxed(title.to_string().into_boxed_str()),
                id: CowStr::Boxed(id.to_string().into_boxed_str()),
            })),
            Event::Start(Tag::Image {
                link_type,
                dest_url,
                title,
                id,
            }) => out.push(Event::Start(Tag::Image {
                link_type,
                dest_url: sanitize_url(&dest_url),
                title: CowStr::Boxed(title.to_string().into_boxed_str()),
                id: CowStr::Boxed(id.to_string().into_boxed_str()),
            })),
            other => out.push(other),
        }
    }
    out
}

// ---------------------------------------------------------------------------
// C ABI exports
// ---------------------------------------------------------------------------

/// Convert a Markdown string to HTML.
///
/// Returns a header-aware, NUL-terminated Hew string containing the rendered
/// HTML. The caller must release it with `hew_string_drop`.
/// Returns null on error.
///
/// # Safety
///
/// `md` must be a valid NUL-terminated C string.
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

/// Convert a Markdown string to sanitized HTML.
///
/// Like [`hew_markdown_to_html`], but raw HTML written by the document author
/// is dropped and link/image destinations are held to an explicit URL scheme
/// policy. Markdown-generated structure — headings, emphasis, lists, code,
/// tables, allowed links — is preserved exactly.
/// Returns a header-aware, NUL-terminated Hew string. The caller must release
/// it with `hew_string_drop`. Returns null on error.
///
/// # Safety
///
/// `md` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_markdown_to_html_safe(md: *const c_char) -> *mut c_char {
    // SAFETY: md is a valid NUL-terminated C string per caller contract.
    let Some(md_str) = (unsafe { cstr_to_str(md) }) else {
        return std::ptr::null_mut();
    };
    let parser = Parser::new_ext(md_str, Options::all());
    let events = sanitize_events(parser);
    let mut html_output = String::new();
    html::push_html(&mut html_output, events.into_iter());
    str_to_malloc(&html_output)
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
        // SAFETY: ptr is a valid header-aware NUL-terminated Hew string.
        let s = unsafe { cstr_to_str(ptr) }.unwrap().to_owned();
        // SAFETY: ptr was allocated by the header-aware string allocator.
        unsafe { hew_cabi::cabi::free_cstring(ptr) }; // CSTRING-FREE: str-open (test str_to_malloc html)
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

    /// Helper: convert markdown in sanitized mode, read the result, and free it.
    unsafe fn md_to_html_safe(md: &str) -> String {
        let c = CString::new(md).unwrap();
        // SAFETY: c is a valid NUL-terminated C string.
        let ptr = unsafe { hew_markdown_to_html_safe(c.as_ptr()) };
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid header-aware NUL-terminated Hew string.
        let s = unsafe { cstr_to_str(ptr) }.unwrap().to_owned();
        // SAFETY: ptr was allocated by the header-aware string allocator.
        unsafe { hew_cabi::cabi::free_cstring(ptr) }; // CSTRING-FREE: str-open (test str_to_malloc)
        s
    }

    #[test]
    fn safe_strips_raw_html() {
        let md = "Hello <script>alert('xss')</script> world";
        let c = CString::new(md).unwrap();
        // SAFETY: c is a valid NUL-terminated C string.
        let ptr = unsafe { hew_markdown_to_html_safe(c.as_ptr()) };
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid header-aware NUL-terminated Hew string.
        let s = unsafe { cstr_to_str(ptr) }.unwrap().to_owned();
        // SAFETY: ptr was allocated by the header-aware string allocator.
        unsafe { hew_cabi::cabi::free_cstring(ptr) }; // CSTRING-FREE: str-open (test str_to_malloc)
        assert!(!s.contains("<script>"), "raw HTML should be stripped: {s}");
        assert!(s.contains("Hello"), "text should be preserved: {s}");
    }

    /// Sanitizing must not destroy the document. Blind tag stripping removed
    /// every generated tag and left plain text that still claimed to be HTML.
    #[test]
    fn sanitizer_retains_generated_structure() {
        // SAFETY: test helper uses valid pointers.
        let html = unsafe {
            md_to_html_safe(
                "# Title\n\n**bold** and `code`\n\n- one\n- two\n\n[link](https://example.com)\n\n```\nfn main() {}\n```\n",
            )
        };
        for expected in [
            "<h1>",
            "<strong>",
            "<code>",
            "<ul>",
            "<li>",
            "<pre>",
            "<a href=\"https://example.com\">",
        ] {
            assert!(html.contains(expected), "expected {expected} in: {html}");
        }
    }

    #[test]
    fn sanitizer_drops_raw_html_written_by_the_author() {
        // SAFETY: test helper uses valid pointers.
        let html = unsafe {
            md_to_html_safe(
                "Hello <script>alert(1)</script>\n\n<img src=x onerror=alert(1)>\n\n<div onclick=\"steal()\">text</div>\n",
            )
        };
        for forbidden in ["<script", "onerror", "onclick", "<img", "<div"] {
            assert!(
                !html.contains(forbidden),
                "raw HTML `{forbidden}` must not survive: {html}"
            );
        }
        assert!(html.contains("Hello"), "text must be preserved: {html}");
    }

    #[test]
    fn sanitizer_refuses_dangerous_link_and_image_urls() {
        for md in [
            "[x](javascript:alert(1))",
            "[x](JavaScript:alert(1))",
            "[x](  javascript:alert(1))",
            "[x](data:text/html;base64,PHNjcmlwdD4=)",
            "[x](vbscript:msgbox(1))",
            "![x](javascript:alert(1))",
        ] {
            // SAFETY: test helper uses valid pointers.
            let html = unsafe { md_to_html_safe(md) };
            assert!(
                !html.contains("javascript:")
                    && !html.contains("data:")
                    && !html.contains("vbscript:"),
                "dangerous URL survived sanitizing of `{md}`: {html}"
            );
            assert!(
                html.contains(REFUSED_URL),
                "a refused URL must be rewritten to an inert destination, got: {html}"
            );
        }
    }

    #[test]
    fn sanitizer_keeps_ordinary_urls() {
        for (md, expected) in [
            (
                "[x](https://example.com/a?b=c#d)",
                "https://example.com/a?b=c#d",
            ),
            ("[x](http://example.com)", "http://example.com"),
            ("[x](mailto:a@example.com)", "mailto:a@example.com"),
            ("[x](/relative/path)", "/relative/path"),
            ("[x](#fragment)", "#fragment"),
            ("[x](relative/path:with-colon)", "relative/path:with-colon"),
        ] {
            // SAFETY: test helper uses valid pointers.
            let html = unsafe { md_to_html_safe(md) };
            assert!(
                html.contains(expected),
                "expected `{expected}` to survive sanitizing of `{md}`: {html}"
            );
            assert!(
                !html.contains(REFUSED_URL),
                "an allowed URL must not be refused: {html}"
            );
        }
    }

    #[test]
    fn url_policy_decides_by_scheme() {
        assert!(url_is_allowed("https://example.com"));
        assert!(url_is_allowed("HTTPS://example.com"));
        assert!(url_is_allowed("/a/b"));
        assert!(url_is_allowed("a/b:c"));
        assert!(url_is_allowed("?q=1"));
        assert!(!url_is_allowed("javascript:alert(1)"));
        assert!(!url_is_allowed("  \t javascript:alert(1)"));
        assert!(!url_is_allowed("data:text/html,<script>"));
        assert!(!url_is_allowed("file:///etc/passwd"));
        assert!(!url_is_allowed("vbscript:x"));
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
