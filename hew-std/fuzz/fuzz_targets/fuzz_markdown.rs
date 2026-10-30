//! `std.text.markdown` rendering, with an independent check of safe mode.
//!
//! Properties: both renderers never panic; the sanitized renderer emits only
//! generator markup (known tags and attributes), never author-written HTML,
//! and every `href` and `src` it emits, once HTML entities are decoded, is
//! relative or uses an allowed scheme.
#![no_main]

use hew_std::markdown::{hew_markdown_to_html, hew_markdown_to_html_safe};
use libfuzzer_sys::fuzz_target;

mod support;
use support::{take, Managed};

const TAGS: &[&str] = &[
    "a",
    "blockquote",
    "br",
    "code",
    "dd",
    "del",
    "div",
    "dl",
    "dt",
    "em",
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "hr",
    "img",
    "input",
    "li",
    "ol",
    "p",
    "pre",
    "span",
    "strong",
    "sub",
    "sup",
    "table",
    "tbody",
    "td",
    "th",
    "thead",
    "tr",
    "ul",
];
const ATTRIBUTES: &[&str] = &[
    "alt", "checked", "class", "disabled", "href", "id", "src", "start", "style", "title", "type",
];
const SCHEMES: &[&str] = &["http", "https", "mailto", "tel", "ftp"];
const REFUSED: &str = "about:blank#refused";

fn unescape(value: &str) -> String {
    let mut out = String::new();
    let mut rest = value;
    while let Some(at) = rest.find('&') {
        out.push_str(&rest[..at]);
        rest = &rest[at..];
        let Some(end) = rest.find(';') else {
            break;
        };
        let entity = &rest[1..end];
        let decoded = match entity {
            "amp" => Some('&'),
            "lt" => Some('<'),
            "gt" => Some('>'),
            "quot" => Some('"'),
            "apos" => Some('\''),
            _ => entity
                .strip_prefix("#x")
                .or_else(|| entity.strip_prefix("#X"))
                .and_then(|hex| u32::from_str_radix(hex, 16).ok())
                .or_else(|| entity.strip_prefix('#').and_then(|dec| dec.parse().ok()))
                .and_then(char::from_u32),
        };
        match decoded {
            Some(ch) => {
                out.push(ch);
                rest = &rest[end + 1..];
            }
            None => {
                out.push('&');
                rest = &rest[1..];
            }
        }
    }
    out.push_str(rest);
    out
}

/// What a browser would take as the scheme: WHATWG URL parsing strips
/// leading C0 controls and spaces and removes every ASCII tab and newline,
/// but never percent-decodes a scheme.
fn browser_scheme(url: &str) -> Option<String> {
    let cleaned: String = url
        .trim_start_matches(|c: char| c <= ' ')
        .chars()
        .filter(|c| !matches!(c, '\t' | '\n' | '\r'))
        .collect();
    let colon = cleaned.find(':')?;
    let scheme = &cleaned[..colon];
    let mut chars = scheme.chars();
    let first = chars.next()?;
    (first.is_ascii_alphabetic()
        && chars.all(|c| c.is_ascii_alphanumeric() || matches!(c, '+' | '-' | '.')))
    .then(|| scheme.to_ascii_lowercase())
}

fn check_url(tag: &str, value: &str) {
    let url = unescape(value);
    if url == REFUSED {
        return;
    }
    if let Some(scheme) = browser_scheme(&url) {
        assert!(
            SCHEMES.contains(&scheme.as_str()),
            "<{tag}> carries a `{scheme}:` URL: {url:?}"
        );
    }
}

/// Check one tag's interior: `name attr="value" ...`, optionally self-closed.
fn check_tag(inner: &str) {
    let inner = inner.strip_suffix('/').unwrap_or(inner);
    let (name, mut rest) = inner.split_once(' ').unwrap_or((inner, ""));
    let name = name.strip_prefix('/').unwrap_or(name);
    assert!(TAGS.contains(&name), "sanitized output has a <{name}> tag");
    loop {
        rest = rest.trim_start();
        if rest.is_empty() {
            break;
        }
        let eq = rest
            .find('=')
            .unwrap_or_else(|| panic!("bare attribute in <{name} {rest}>"));
        let attribute = &rest[..eq];
        assert!(
            ATTRIBUTES.contains(&attribute),
            "sanitized output has a `{attribute}` attribute on <{name}>"
        );
        let after = rest[eq + 1..]
            .strip_prefix('"')
            .unwrap_or_else(|| panic!("unquoted `{attribute}` on <{name}>"));
        let close = after.find('"').expect("quoted attribute closes");
        let value = &after[..close];
        if matches!(attribute, "href" | "src") {
            check_url(name, value);
        }
        rest = &after[close + 1..];
    }
}

fn check_safe(html: &str) {
    let mut rest = html;
    while let Some(open) = rest.find('<') {
        let after = &rest[open + 1..];
        let close = after.find('>').expect("every emitted tag closes");
        check_tag(&after[..close]);
        rest = &after[close + 1..];
    }
}

fuzz_target!(|data: &[u8]| {
    let Ok(text) = std::str::from_utf8(data) else {
        return;
    };
    let input = Managed::new(text);
    // SAFETY: `input` is live; each result is an owned managed string.
    let (raw, safe) = unsafe {
        (
            take(hew_markdown_to_html(input.ptr())),
            take(hew_markdown_to_html_safe(input.ptr())),
        )
    };
    drop(raw);
    check_safe(&safe);
});
