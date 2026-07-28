//! Hew `std::encoding::xml` — XML parsing and serialization.
//!
//! Provides XML parsing, serialization, and node access for compiled Hew
//! programs. All returned strings are allocated with `libc::malloc` and
//! NUL-terminated. All returned [`HewXmlNode`] pointers are heap-allocated
//! via `Box` and must be freed with [`hew_xml_free`].
use hew_cabi::cabi::str_to_malloc;
use std::cell::RefCell;
use std::ffi::CStr;
use std::os::raw::c_char;

const MAX_XML_DEPTH: usize = 256;

std::thread_local! {
    static LAST_XML_ERROR: RefCell<Option<String>> = const { RefCell::new(None) };
}

fn set_xml_last_error(msg: impl Into<String>) {
    LAST_XML_ERROR.with(|error| *error.borrow_mut() = Some(msg.into()));
}

fn clear_xml_last_error() {
    LAST_XML_ERROR.with(|error| *error.borrow_mut() = None);
}

fn clone_xml_last_error() -> Option<String> {
    LAST_XML_ERROR.with(|error| error.borrow().clone())
}

// ---------------------------------------------------------------------------
// Internal tree representation
// ---------------------------------------------------------------------------

/// An XML node: either an element with tag, attributes, and children, or a
/// text node containing character data.
#[derive(Debug, Clone)]
enum XmlNodeKind {
    /// `<tag attr="val">children…</tag>`
    Element {
        tag: String,
        attributes: Vec<(String, String)>,
        children: Vec<XmlNodeKind>,
    },
    /// Character data between tags.
    Text(String),
}

/// Opaque wrapper around an XML node tree.
///
/// Returned by [`hew_xml_parse`] and navigation functions.
/// Must be freed with [`hew_xml_free`].
#[derive(Debug)]
pub struct HewXmlNode {
    inner: XmlNodeKind,
}

/// Wrap an [`XmlNodeKind`] into a heap-allocated [`HewXmlNode`].
fn boxed_node(kind: XmlNodeKind) -> *mut HewXmlNode {
    Box::into_raw(Box::new(HewXmlNode { inner: kind }))
}

// ---------------------------------------------------------------------------
// Parsing (quick-xml event reader → tree)
// ---------------------------------------------------------------------------

use quick_xml::events::BytesStart;

/// Recursive frame used during tree construction.
struct Frame {
    tag: String,
    attributes: Vec<(String, String)>,
    children: Vec<XmlNodeKind>,
}

#[derive(Debug)]
enum ParseXmlError {
    Malformed,
    Reader(String),
    MaximumDepthExceeded,
    NoRootElement,
    MultipleRootElements,
    TextOutsideRoot,
    MismatchedClose { expected: String, found: String },
    UnexpectedClose(String),
    InvalidAttribute(String),
    UnknownEntity(String),
}

impl ParseXmlError {
    fn message(&self) -> String {
        match self {
            Self::Malformed => "xml: parse error".to_owned(),
            Self::Reader(detail) => format!("xml: {detail}"),
            Self::MaximumDepthExceeded => "xml: maximum nesting depth (256) exceeded".to_owned(),
            Self::NoRootElement => "xml: document has no root element".to_owned(),
            Self::MultipleRootElements => "xml: document has more than one root element".to_owned(),
            Self::TextOutsideRoot => {
                "xml: non-whitespace text is not allowed outside the root element".to_owned()
            }
            Self::MismatchedClose { expected, found } => {
                format!("xml: `</{found}>` does not close `<{expected}>`")
            }
            Self::UnexpectedClose(tag) => {
                format!("xml: `</{tag}>` closes an element that was never opened")
            }
            Self::InvalidAttribute(tag) => {
                format!("xml: element `{tag}` has a malformed attribute")
            }
            Self::UnknownEntity(name) => {
                format!("xml: `&{name};` is not a known entity reference")
            }
        }
    }
}

/// Extract tag name and attributes from a [`BytesStart`] event.
///
/// A malformed attribute is a parse failure, not something to drop: silently
/// discarding it would hand back an element that claims not to carry the
/// attribute the document tried to give it.
fn extract_tag_and_attrs(
    e: &BytesStart<'_>,
) -> Result<(String, Vec<(String, String)>), ParseXmlError> {
    let tag = String::from_utf8_lossy(e.name().as_ref()).to_string();
    let mut attributes = Vec::new();
    for attribute in e.attributes() {
        let Ok(a) = attribute else {
            return Err(ParseXmlError::InvalidAttribute(tag));
        };
        let key = String::from_utf8_lossy(a.key.as_ref()).to_string();
        let Ok(val) = a.normalized_value(quick_xml::XmlVersion::Implicit1_0) else {
            return Err(ParseXmlError::InvalidAttribute(tag));
        };
        let val = val.into_owned();
        attributes.push((key, val));
    }
    Ok((tag, attributes))
}

/// Push a node onto the parent frame or into the top-level list.
fn push_node(stack: &mut [Frame], top_level: &mut Vec<XmlNodeKind>, node: XmlNodeKind) {
    if let Some(parent) = stack.last_mut() {
        parent.children.push(node);
    } else {
        top_level.push(node);
    }
}

/// Flush the accumulated text buffer into the tree if it contains
/// non-whitespace content.
fn flush_text(buf: &mut String, stack: &mut [Frame], top_level: &mut Vec<XmlNodeKind>) {
    if buf.trim().is_empty() {
        buf.clear();
    } else {
        let text = std::mem::take(buf);
        push_node(stack, top_level, XmlNodeKind::Text(text));
    }
}

/// Validate the document-level shape and return its sole element root.
fn sole_document_root(mut roots: Vec<XmlNodeKind>) -> Result<XmlNodeKind, ParseXmlError> {
    if !roots
        .iter()
        .any(|node| matches!(node, XmlNodeKind::Element { .. }))
    {
        return Err(ParseXmlError::NoRootElement);
    }
    if roots.iter().any(|node| {
        matches!(
            node,
            XmlNodeKind::Text(text)
                if !text
                    .chars()
                    .all(|ch| matches!(ch, ' ' | '\t' | '\r' | '\n'))
        )
    }) {
        return Err(ParseXmlError::TextOutsideRoot);
    }
    roots.retain(|node| !matches!(node, XmlNodeKind::Text(_)));
    match roots.len() {
        0 => Err(ParseXmlError::NoRootElement),
        1 => Ok(roots.remove(0)),
        _ => Err(ParseXmlError::MultipleRootElements),
    }
}

/// Build a tree of [`XmlNodeKind`] from an XML string using quick-xml.
fn parse_xml(xml: &str) -> Result<XmlNodeKind, ParseXmlError> {
    use quick_xml::events::Event;
    use quick_xml::reader::Reader;

    let mut reader = Reader::from_str(xml);
    reader.config_mut().expand_empty_elements = false;
    // quick-xml 0.39 does not expose an external-entity-resolution toggle:
    // general/entity references are surfaced as `Event::GeneralRef` and are not
    // expanded against external resources by default. Preserve that fail-closed
    // behavior here rather than attempting any custom resolution.
    let mut stack: Vec<Frame> = Vec::new();
    let mut top_level: Vec<XmlNodeKind> = Vec::new();

    // quick-xml 0.38+ emits entity references (`&lt;`, `&amp;`, etc.) as
    // separate `Event::GeneralRef` events rather than embedding them in
    // `BytesText`.  We accumulate consecutive Text and GeneralRef events
    // into a single buffer and flush it as one `XmlNodeKind::Text` when a
    // non-text event arrives.
    let mut text_buf = String::new();

    loop {
        match reader.read_event() {
            Ok(Event::Start(ref e)) => {
                flush_text(&mut text_buf, &mut stack, &mut top_level);
                if stack.len() + 1 > MAX_XML_DEPTH {
                    return Err(ParseXmlError::MaximumDepthExceeded);
                }
                let (tag, attributes) = extract_tag_and_attrs(e)?;
                stack.push(Frame {
                    tag,
                    attributes,
                    children: Vec::new(),
                });
            }
            Ok(Event::End(ref e)) => {
                flush_text(&mut text_buf, &mut stack, &mut top_level);
                let closing = String::from_utf8_lossy(e.name().as_ref()).to_string();
                let Some(frame) = stack.pop() else {
                    return Err(ParseXmlError::UnexpectedClose(closing));
                };
                if frame.tag != closing {
                    return Err(ParseXmlError::MismatchedClose {
                        expected: frame.tag,
                        found: closing,
                    });
                }
                let node = XmlNodeKind::Element {
                    tag: frame.tag,
                    attributes: frame.attributes,
                    children: frame.children,
                };
                push_node(&mut stack, &mut top_level, node);
            }
            Ok(Event::Empty(ref e)) => {
                flush_text(&mut text_buf, &mut stack, &mut top_level);
                if stack.len() + 1 > MAX_XML_DEPTH {
                    return Err(ParseXmlError::MaximumDepthExceeded);
                }
                let (tag, attributes) = extract_tag_and_attrs(e)?;
                let node = XmlNodeKind::Element {
                    tag,
                    attributes,
                    children: Vec::new(),
                };
                push_node(&mut stack, &mut top_level, node);
            }
            Ok(Event::Text(ref e)) => {
                text_buf.push_str(&String::from_utf8_lossy(e.as_ref()));
            }
            // Deref `BytesRef` to its `[u8]` target explicitly: `as_ref()` is
            // ambiguous once sibling stdlib modules pull `winnow`, which adds
            // its own `AsRef` impls on `[u8]` into scope.
            Ok(Event::GeneralRef(ref e)) => match &**e {
                b"lt" => text_buf.push('<'),
                b"gt" => text_buf.push('>'),
                b"amp" => text_buf.push('&'),
                b"quot" => text_buf.push('"'),
                b"apos" => text_buf.push('\''),
                _ => {
                    // An unresolvable reference is malformed XML. Echoing it
                    // back as literal text would silently turn a broken
                    // document into one that parses.
                    let Ok(Some(ch)) = e.resolve_char_ref() else {
                        return Err(ParseXmlError::UnknownEntity(
                            String::from_utf8_lossy(e.as_ref()).to_string(),
                        ));
                    };
                    text_buf.push(ch);
                }
            },
            Ok(Event::CData(ref e)) => {
                flush_text(&mut text_buf, &mut stack, &mut top_level);
                let text = String::from_utf8_lossy(e.as_ref()).to_string();
                if !text.is_empty() {
                    push_node(&mut stack, &mut top_level, XmlNodeKind::Text(text));
                }
            }
            Ok(Event::Eof) => {
                flush_text(&mut text_buf, &mut stack, &mut top_level);
                break;
            }
            Ok(_) => {}
            Err(err) => return Err(ParseXmlError::Reader(err.to_string())),
        }
    }

    if !stack.is_empty() {
        return Err(ParseXmlError::Malformed);
    }

    // A well-formed XML document has exactly one root element. Wrapping
    // several in a synthetic tagless element would invent structure the
    // document never had, and serializing that tree back produces `<>`.
    sole_document_root(top_level)
}

// ---------------------------------------------------------------------------
// Serialization (tree → XML string)
// ---------------------------------------------------------------------------

/// Serialize an [`XmlNodeKind`] tree back to an XML string.
fn serialize_xml(node: &XmlNodeKind) -> String {
    let mut buf = String::new();
    write_node(&mut buf, node);
    buf
}

fn write_node(buf: &mut String, node: &XmlNodeKind) {
    match node {
        XmlNodeKind::Element {
            tag,
            attributes,
            children,
        } => {
            buf.push('<');
            buf.push_str(tag);
            for (k, v) in attributes {
                buf.push(' ');
                buf.push_str(k);
                buf.push_str("=\"");
                buf.push_str(&escape_attr(v));
                buf.push('"');
            }
            if children.is_empty() {
                buf.push_str("/>");
            } else {
                buf.push('>');
                for child in children {
                    write_node(buf, child);
                }
                buf.push_str("</");
                buf.push_str(tag);
                buf.push('>');
            }
        }
        XmlNodeKind::Text(text) => {
            buf.push_str(&escape_text(text));
        }
    }
}

/// Escape special characters in text content.
fn escape_text(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
}

/// Escape special characters in attribute values.
fn escape_attr(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

/// Recursively collect text content from a node tree.
fn collect_text(node: &XmlNodeKind, buf: &mut String) {
    match node {
        XmlNodeKind::Text(t) => buf.push_str(t),
        XmlNodeKind::Element { children, .. } => {
            for child in children {
                collect_text(child, buf);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// C ABI exports
// ---------------------------------------------------------------------------

/// Parse an XML string into a [`HewXmlNode`] tree.
///
/// Returns null on parse error or invalid input.
///
/// # Safety
///
/// `xml_str` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_xml_parse(xml_str: *const c_char) -> *mut HewXmlNode {
    if xml_str.is_null() {
        set_xml_last_error("xml: invalid input: null pointer");
        return std::ptr::null_mut();
    }
    // SAFETY: xml_str is a valid NUL-terminated C string per caller contract.
    let Ok(s) = unsafe { CStr::from_ptr(xml_str) }.to_str() else {
        set_xml_last_error("xml: invalid input: input was not valid UTF-8");
        return std::ptr::null_mut();
    };
    match parse_xml(s) {
        Ok(tree) => {
            clear_xml_last_error();
            boxed_node(tree)
        }
        Err(err) => {
            set_xml_last_error(err.message());
            std::ptr::null_mut()
        }
    }
}

/// Return this actor's last XML parse error.
///
/// Returns a `malloc`-allocated, NUL-terminated C string. The caller must free
/// it with [`hew_xml_string_free`]. Returns null when no XML error has been
/// recorded.
#[no_mangle]
pub extern "C" fn hew_xml_last_error() -> *mut c_char {
    match clone_xml_last_error() {
        Some(message) => str_to_malloc(&message),
        None => std::ptr::null_mut(),
    }
}

/// Serialize a [`HewXmlNode`] tree back to an XML string.
///
/// Returns a `malloc`-allocated, NUL-terminated C string. The caller must free
/// it with [`hew_xml_string_free`]. Returns null on error.
///
/// # Safety
///
/// `node` must be a valid pointer to a [`HewXmlNode`].
#[no_mangle]
pub unsafe extern "C" fn hew_xml_to_string(node: *const HewXmlNode) -> *mut c_char {
    if node.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: node is a valid HewXmlNode pointer per caller contract.
    let n = unsafe { &*node };
    let s = serialize_xml(&n.inner);
    str_to_malloc(&s)
}

/// Get the tag name of an XML element node.
///
/// Returns a `malloc`-allocated, NUL-terminated C string. Returns an empty
/// string for text nodes. The caller must free it with [`hew_xml_string_free`].
///
/// # Safety
///
/// `node` must be a valid pointer to a [`HewXmlNode`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_xml_get_tag(node: *const HewXmlNode) -> *mut c_char {
    if node.is_null() {
        return str_to_malloc("");
    }
    // SAFETY: node is a valid HewXmlNode pointer per caller contract.
    let n = unsafe { &*node };
    match &n.inner {
        XmlNodeKind::Element { tag, .. } => str_to_malloc(tag),
        XmlNodeKind::Text(_) => str_to_malloc(""),
    }
}

/// Get an attribute value by name from an XML element node.
///
/// Returns a `malloc`-allocated, NUL-terminated C string containing the
/// attribute value if found, or an empty string if the attribute is not
/// present or the node is a text node. The caller must free the result
/// with [`hew_xml_string_free`].
///
/// # Safety
///
/// `node` must be a valid pointer to a [`HewXmlNode`], or null.
/// `name` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_xml_get_attribute(
    node: *const HewXmlNode,
    name: *const c_char,
) -> *mut c_char {
    if node.is_null() || name.is_null() {
        return str_to_malloc("");
    }
    // SAFETY: name is a valid NUL-terminated C string per caller contract.
    let Ok(attr_name) = unsafe { CStr::from_ptr(name) }.to_str() else {
        return str_to_malloc("");
    };
    // SAFETY: node is a valid HewXmlNode pointer per caller contract.
    let n = unsafe { &*node };
    if let XmlNodeKind::Element { attributes, .. } = &n.inner {
        for (k, v) in attributes {
            if k == attr_name {
                return str_to_malloc(v);
            }
        }
    }
    str_to_malloc("")
}

/// Get the number of child nodes of an XML element.
///
/// Returns 0 for text nodes or null pointers.
///
/// # Safety
///
/// `node` must be a valid pointer to a [`HewXmlNode`], or null.
#[no_mangle]
#[expect(
    clippy::cast_possible_wrap,
    clippy::cast_possible_truncation,
    reason = "XML child counts won't exceed i32::MAX in practice"
)]
pub unsafe extern "C" fn hew_xml_children_count(node: *const HewXmlNode) -> i32 {
    if node.is_null() {
        return 0;
    }
    // SAFETY: node is a valid HewXmlNode pointer per caller contract.
    let n = unsafe { &*node };
    match &n.inner {
        XmlNodeKind::Element { children, .. } => children.len() as i32,
        XmlNodeKind::Text(_) => 0,
    }
}

/// Get a child node by index.
///
/// Returns a new heap-allocated [`HewXmlNode`] (clone of the child). The
/// caller must free it with [`hew_xml_free`]. Returns null if the index is
/// out of bounds or the node is a text node.
///
/// # Safety
///
/// `node` must be a valid pointer to a [`HewXmlNode`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_xml_get_child(node: *const HewXmlNode, index: i32) -> *mut HewXmlNode {
    if node.is_null() || index < 0 {
        return std::ptr::null_mut();
    }
    // SAFETY: node is a valid HewXmlNode pointer per caller contract.
    let n = unsafe { &*node };
    #[expect(
        clippy::cast_sign_loss,
        reason = "C ABI: negative values checked before cast"
    )]
    match &n.inner {
        XmlNodeKind::Element { children, .. } => children
            .get(index as usize)
            .map_or(std::ptr::null_mut(), |c| boxed_node(c.clone())),
        XmlNodeKind::Text(_) => std::ptr::null_mut(),
    }
}

/// Get the concatenated text content of a node and all its descendants.
///
/// For element nodes, this recursively collects all text node content.
/// For text nodes, returns the text directly.
///
/// Returns a `malloc`-allocated, NUL-terminated C string. The caller must
/// free it with [`hew_xml_string_free`].
///
/// # Safety
///
/// `node` must be a valid pointer to a [`HewXmlNode`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_xml_get_text(node: *const HewXmlNode) -> *mut c_char {
    if node.is_null() {
        return str_to_malloc("");
    }
    // SAFETY: node is a valid HewXmlNode pointer per caller contract.
    let n = unsafe { &*node };
    let mut buf = String::new();
    collect_text(&n.inner, &mut buf);
    str_to_malloc(&buf)
}

/// Return whether a node is an element (1) or text (0).
///
/// Returns -1 for null pointers.
///
/// # Safety
///
/// `node` must be a valid pointer to a [`HewXmlNode`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_xml_is_element(node: *const HewXmlNode) -> i32 {
    if node.is_null() {
        return -1;
    }
    // SAFETY: node is a valid HewXmlNode pointer per caller contract.
    let n = unsafe { &*node };
    match &n.inner {
        XmlNodeKind::Element { .. } => 1,
        XmlNodeKind::Text(_) => 0,
    }
}

/// Free a [`HewXmlNode`] previously returned by any of the `hew_xml_*`
/// functions.
///
/// # Safety
///
/// `node` must be a pointer previously returned by a `hew_xml_*` function,
/// and must not have been freed already.
#[no_mangle]
pub unsafe extern "C" fn hew_xml_free(node: *mut HewXmlNode) {
    if node.is_null() {
        return;
    }
    // SAFETY: node was allocated with Box::into_raw and has not been freed.
    drop(unsafe { Box::from_raw(node) });
}

/// Free a C string previously returned by `hew_xml_to_string`,
/// `hew_xml_get_tag`, `hew_xml_get_attribute`, or `hew_xml_get_text`.
///
/// # Safety
///
/// `s` must be a pointer previously returned by a `hew_xml_*` string function,
/// and must not have been freed already.
#[no_mangle]
pub unsafe extern "C" fn hew_xml_string_free(s: *mut c_char) {
    if s.is_null() {
        return;
    }
    // SAFETY: s was allocated with libc::malloc and has not been freed.
    unsafe { hew_cabi::cabi::free_cstring(s) }; // CSTRING-FREE: str-open (test frees str_to_malloc output)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    /// Establish that an XML string export transfers one independently
    /// releasable owner to its caller.
    ///
    /// Two simultaneously-live calls must return distinct allocations (R1),
    /// each allocation must already be unique at handoff (R2), and releasing
    /// both must leave the source node usable for a third equivalent call
    /// (R3). Together these are the executable authority for the
    /// `result-retention = "transferred"` rows on the four XML string
    /// producers.
    fn assert_string_result_is_transferred(
        symbol: &str,
        expected: &str,
        call: impl Fn() -> *mut c_char,
    ) {
        let first = call();
        let second = call();
        assert!(
            !first.is_null() && !second.is_null(),
            "{symbol}: expected two live results"
        );
        assert_ne!(
            first, second,
            "{symbol}: two live results share an address, so the export retained \
             or re-borrowed its result"
        );

        for (label, ptr) in [("first", first), ("second", second)] {
            // SAFETY: each pointer is a live header-aware string returned by
            // one of the XML exports under test.
            let unique = unsafe { hew_cabi::cabi::cstring_ensure_unique(ptr) };
            assert_eq!(
                unique, ptr,
                "{symbol}: the {label} result was not solely owned at handoff"
            );
            // SAFETY: `ptr` remains live because the uniqueness probe returned
            // it unchanged.
            let actual = unsafe { CStr::from_ptr(ptr) }
                .to_str()
                .expect("XML is UTF-8");
            assert_eq!(actual, expected);
        }

        // SAFETY: R2 established that each result is a distinct sole owner.
        unsafe {
            hew_xml_string_free(first);
            hew_xml_string_free(second);
        }

        let third = call();
        assert!(
            !third.is_null(),
            "{symbol}: source did not survive releases"
        );
        // SAFETY: `third` is a live result from the same XML export.
        let actual = unsafe { CStr::from_ptr(third) }
            .to_str()
            .expect("XML is UTF-8");
        assert_eq!(
            actual, expected,
            "{symbol}: releasing earlier results changed the source"
        );
        // SAFETY: `third` is the export's fresh sole-owner result.
        unsafe { hew_xml_string_free(third) };
    }

    /// Helper: parse an XML string and return the owned pointer.
    fn parse(xml: &str) -> *mut HewXmlNode {
        let c = CString::new(xml).unwrap();
        // SAFETY: c is a valid NUL-terminated C string.
        unsafe { hew_xml_parse(c.as_ptr()) }
    }

    /// Helper: read a C string pointer and free it.
    unsafe fn read_and_free_cstr(ptr: *mut c_char) -> String {
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid NUL-terminated C string from malloc.
        let s = unsafe { CStr::from_ptr(ptr) }.to_str().unwrap().to_owned();
        // SAFETY: ptr was allocated with malloc.
        unsafe { hew_xml_string_free(ptr) };
        s
    }

    unsafe fn read_and_free_optional_cstr(ptr: *mut c_char) -> Option<String> {
        if ptr.is_null() {
            return None;
        }
        // SAFETY: ptr is a valid NUL-terminated C string from malloc.
        let s = unsafe { CStr::from_ptr(ptr) }.to_str().unwrap().to_owned();
        // SAFETY: ptr was allocated with malloc.
        unsafe { hew_xml_string_free(ptr) };
        Some(s)
    }

    #[test]
    fn parse_simple_element() {
        let node = parse("<greeting>Hello</greeting>");
        assert!(!node.is_null());

        // SAFETY: node is a valid HewXmlNode from parse.
        unsafe {
            assert_eq!(hew_xml_is_element(node), 1);
            let tag = read_and_free_cstr(hew_xml_get_tag(node));
            assert_eq!(tag, "greeting");
            let text = read_and_free_cstr(hew_xml_get_text(node));
            assert_eq!(text, "Hello");
            hew_xml_free(node);
        }
    }

    #[test]
    fn parse_with_attributes() {
        let node = parse(r#"<item id="42" colour="red">Test</item>"#);
        assert!(!node.is_null());

        // SAFETY: node is a valid HewXmlNode from parse.
        unsafe {
            let id_key = CString::new("id").unwrap();
            let id_val = hew_xml_get_attribute(node, id_key.as_ptr());
            assert_eq!(read_and_free_cstr(id_val), "42");

            let colour_key = CString::new("colour").unwrap();
            let colour_val = hew_xml_get_attribute(node, colour_key.as_ptr());
            assert_eq!(read_and_free_cstr(colour_val), "red");

            let missing_key = CString::new("missing").unwrap();
            let missing_val = hew_xml_get_attribute(node, missing_key.as_ptr());
            assert_eq!(read_and_free_cstr(missing_val), "");

            hew_xml_free(node);
        }
    }

    #[test]
    fn string_results_are_transferred_to_the_caller() {
        let node = parse(r#"<item id="42">hello</item>"#);
        assert!(!node.is_null());
        let id = CString::new("id").unwrap();

        // SAFETY: `node` is live for the duration of every call and `id` is a
        // valid NUL-terminated attribute name.
        assert_string_result_is_transferred("hew_xml_get_tag", "item", || unsafe {
            hew_xml_get_tag(node)
        });
        assert_string_result_is_transferred("hew_xml_get_text", "hello", || {
            // SAFETY: `node` remains live until the final free below.
            unsafe { hew_xml_get_text(node) }
        });
        assert_string_result_is_transferred("hew_xml_get_attribute", "42", || {
            // SAFETY: `node` is live and `id` is NUL-terminated.
            unsafe { hew_xml_get_attribute(node, id.as_ptr()) }
        });
        assert_string_result_is_transferred(
            "hew_xml_to_string",
            r#"<item id="42">hello</item>"#,
            || {
                // SAFETY: `node` remains live until the final free below.
                unsafe { hew_xml_to_string(node) }
            },
        );

        // SAFETY: the string-result probes borrow the node and release only
        // their own returned buffers.
        unsafe { hew_xml_free(node) };
    }

    #[test]
    fn parse_nested_children() {
        let xml = "<root><a>1</a><b>2</b><c>3</c></root>";
        let node = parse(xml);
        assert!(!node.is_null());

        // SAFETY: node is a valid HewXmlNode from parse.
        unsafe {
            assert_eq!(hew_xml_children_count(node), 3);

            let child0 = hew_xml_get_child(node, 0);
            assert!(!child0.is_null());
            let tag0 = read_and_free_cstr(hew_xml_get_tag(child0));
            assert_eq!(tag0, "a");
            let text0 = read_and_free_cstr(hew_xml_get_text(child0));
            assert_eq!(text0, "1");
            hew_xml_free(child0);

            let child2 = hew_xml_get_child(node, 2);
            assert!(!child2.is_null());
            let tag2 = read_and_free_cstr(hew_xml_get_tag(child2));
            assert_eq!(tag2, "c");
            hew_xml_free(child2);

            // Out of bounds
            assert!(hew_xml_get_child(node, 5).is_null());

            hew_xml_free(node);
        }
    }

    #[test]
    fn get_text_recursive() {
        let xml = "<p>Hello <b>world</b>!</p>";
        let node = parse(xml);
        assert!(!node.is_null());

        // SAFETY: node is a valid HewXmlNode from parse.
        unsafe {
            let text = read_and_free_cstr(hew_xml_get_text(node));
            assert_eq!(text, "Hello world!");
            hew_xml_free(node);
        }
    }

    #[test]
    fn roundtrip_serialize() {
        let xml = "<book><title>Hew Guide</title><author>Team</author></book>";
        let node = parse(xml);
        assert!(!node.is_null());

        // SAFETY: node is a valid HewXmlNode from parse.
        unsafe {
            let serialized = read_and_free_cstr(hew_xml_to_string(node));
            assert_eq!(serialized, xml);
            hew_xml_free(node);
        }
    }

    #[test]
    fn self_closing_tags() {
        let xml = r#"<root><br/><img src="a.png"/></root>"#;
        let node = parse(xml);
        assert!(!node.is_null());

        // SAFETY: node is a valid HewXmlNode from parse.
        unsafe {
            assert_eq!(hew_xml_children_count(node), 2);

            let br = hew_xml_get_child(node, 0);
            let tag = read_and_free_cstr(hew_xml_get_tag(br));
            assert_eq!(tag, "br");
            assert_eq!(hew_xml_children_count(br), 0);
            hew_xml_free(br);

            let img = hew_xml_get_child(node, 1);
            let src_key = CString::new("src").unwrap();
            let src_val = hew_xml_get_attribute(img, src_key.as_ptr());
            assert_eq!(read_and_free_cstr(src_val), "a.png");
            hew_xml_free(img);

            hew_xml_free(node);
        }
    }

    #[test]
    fn parse_invalid_returns_null() {
        let node = parse("<unclosed>");
        assert!(node.is_null());
        // SAFETY: hew_xml_last_error returns a malloc-allocated error string or null.
        let last_error = unsafe { read_and_free_optional_cstr(hew_xml_last_error()) };
        assert_eq!(last_error, Some("xml: parse error".to_string()));

        // SAFETY: null pointer is safe for hew_xml_parse.
        unsafe {
            assert!(hew_xml_parse(std::ptr::null()).is_null());
            assert_eq!(
                read_and_free_optional_cstr(hew_xml_last_error()),
                Some("xml: invalid input: null pointer".to_string())
            );
        }
    }

    #[test]
    fn null_handling() {
        // SAFETY: Testing null-safety of all API functions.
        unsafe {
            assert_eq!(hew_xml_is_element(std::ptr::null()), -1);
            assert_eq!(hew_xml_children_count(std::ptr::null()), 0);
            assert!(hew_xml_get_child(std::ptr::null(), 0).is_null());

            let text = read_and_free_cstr(hew_xml_get_text(std::ptr::null()));
            assert_eq!(text, "");

            let tag = read_and_free_cstr(hew_xml_get_tag(std::ptr::null()));
            assert_eq!(tag, "");
        }
    }

    #[test]
    fn escaped_content() {
        let xml = "<data>&lt;hello&gt; &amp; world</data>";
        let node = parse(xml);
        assert!(!node.is_null());

        // SAFETY: node is a valid HewXmlNode from parse.
        unsafe {
            let text = read_and_free_cstr(hew_xml_get_text(node));
            assert_eq!(text, "<hello> & world");

            // Roundtrip preserves escaping
            let serialized = read_and_free_cstr(hew_xml_to_string(node));
            assert_eq!(serialized, "<data>&lt;hello&gt; &amp; world</data>");

            hew_xml_free(node);
        }
    }

    #[test]
    fn parse_empty_root() {
        let node = parse("<root/>");
        assert!(!node.is_null());

        // SAFETY: node is a valid HewXmlNode from parse.
        unsafe {
            let tag = read_and_free_cstr(hew_xml_get_tag(node));
            assert_eq!(tag, "root");
            assert_eq!(hew_xml_children_count(node), 0);
            hew_xml_free(node);
        }
    }

    #[test]
    fn parse_reasonable_nesting_succeeds() {
        let xml = "<a><b><c><d><e>ok</e></d></c></b></a>";
        let node = parse(xml);
        assert!(!node.is_null());

        // SAFETY: node is a valid HewXmlNode from parse.
        unsafe {
            let text = read_and_free_cstr(hew_xml_get_text(node));
            assert_eq!(text, "ok");
            assert!(read_and_free_optional_cstr(hew_xml_last_error()).is_none());
            hew_xml_free(node);
        }
    }

    #[test]
    fn parse_rejects_excessive_nesting_depth() {
        let open = "<x>".repeat(10_000);
        let close = "</x>".repeat(10_000);
        let xml = format!("{open}{close}");

        let node = parse(&xml);
        assert!(node.is_null());
        // SAFETY: hew_xml_last_error returns a malloc-allocated error string or null.
        let last_error = unsafe { read_and_free_optional_cstr(hew_xml_last_error()) };
        assert_eq!(
            last_error,
            Some("xml: maximum nesting depth (256) exceeded".to_string())
        );
    }

    #[test]
    fn general_entity_references_are_refused_not_echoed_as_text() {
        // quick-xml 0.39 surfaces `&lol;` as a GeneralRef event and does not
        // expand it via the DTD. Echoing the reference back as literal text
        // would hand the caller text the document never contained, so the
        // document is refused instead.
        let xml = r#"<!DOCTYPE foo [<!ENTITY lol "LOL">]><root>&lol;</root>"#;
        let node = parse(xml);
        assert!(node.is_null(), "an unexpanded entity must not parse");
        // SAFETY: hew_xml_last_error returns a malloc-allocated error string or null.
        let last_error = unsafe { read_and_free_optional_cstr(hew_xml_last_error()) };
        assert_eq!(
            last_error,
            Some("xml: `&lol;` is not a known entity reference".to_string())
        );
    }

    #[test]
    fn predefined_entity_references_still_expand() {
        let xml = "<root>a &lt; b &amp; c &gt; d &quot;e&quot; &apos;f&apos;</root>";
        let node = parse(xml);
        assert!(!node.is_null());
        // SAFETY: node is a valid HewXmlNode from parse.
        unsafe {
            let text = read_and_free_cstr(hew_xml_get_text(node));
            assert_eq!(text, "a < b & c > d \"e\" 'f'");
            hew_xml_free(node);
        }
    }

    #[test]
    fn attribute_entities_are_decoded_before_exposure() {
        let node = parse(r#"<root label="a &lt; b &amp; &quot;q&quot;"/>"#);
        assert!(!node.is_null());
        let name = c"label";
        // SAFETY: node and attribute name are valid.
        unsafe {
            let value = read_and_free_cstr(hew_xml_get_attribute(node, name.as_ptr()));
            assert_eq!(value, "a < b & \"q\"");
            hew_xml_free(node);
        }
    }

    #[test]
    fn unknown_attribute_entity_is_rejected() {
        let node = parse(r#"<root label="&notDeclared;"/>"#);
        assert!(node.is_null());
        // SAFETY: accessor returns null or a valid allocated string.
        let last_error = unsafe { read_and_free_optional_cstr(hew_xml_last_error()) };
        assert_eq!(
            last_error,
            Some("xml: element `root` has a malformed attribute".to_owned())
        );
    }

    #[test]
    fn numeric_character_references_still_expand() {
        let node = parse("<root>&#72;&#x69;</root>");
        assert!(!node.is_null());
        // SAFETY: node is a valid HewXmlNode from parse.
        unsafe {
            let text = read_and_free_cstr(hew_xml_get_text(node));
            assert_eq!(text, "Hi");
            hew_xml_free(node);
        }
    }

    #[test]
    fn multiple_root_elements_are_refused_not_wrapped() {
        // Wrapping them in a synthetic tagless element invents structure the
        // document never had, and serializes back as `<>`.
        let node = parse("<a/><b/>");
        assert!(node.is_null());
        // SAFETY: hew_xml_last_error returns a malloc-allocated error string or null.
        let last_error = unsafe { read_and_free_optional_cstr(hew_xml_last_error()) };
        assert_eq!(
            last_error,
            Some("xml: document has more than one root element".to_string())
        );
    }

    #[test]
    fn non_whitespace_text_outside_the_single_root_is_rejected() {
        for xml in ["before<root/>", "<root/>after"] {
            let node = parse(xml);
            assert!(node.is_null(), "{xml:?} must not parse");
            // SAFETY: accessor returns null or a valid allocated string.
            let last_error = unsafe { read_and_free_optional_cstr(hew_xml_last_error()) };
            assert_eq!(
                last_error,
                Some("xml: non-whitespace text is not allowed outside the root element".to_owned())
            );
        }
    }

    #[test]
    fn xml_whitespace_around_the_single_root_is_allowed() {
        let node = parse(" \t\r\n<root/>\n");
        assert!(!node.is_null());
        // SAFETY: node is a valid HewXmlNode from parse.
        unsafe { hew_xml_free(node) };
    }

    #[test]
    fn a_close_tag_that_does_not_match_its_open_tag_is_refused() {
        let node = parse("<a><b></c></a>");
        assert!(node.is_null());
        // SAFETY: hew_xml_last_error returns a malloc-allocated error string or null.
        let last_error = unsafe { read_and_free_optional_cstr(hew_xml_last_error()) };
        assert_eq!(
            last_error,
            Some("xml: ill-formed document: expected `</b>`, but `</c>` was found".to_string())
        );
    }

    #[test]
    fn a_close_tag_with_no_open_tag_is_refused() {
        let node = parse("<a/></b>");
        assert!(node.is_null());
        // SAFETY: hew_xml_last_error returns a malloc-allocated error string or null.
        let last_error = unsafe { read_and_free_optional_cstr(hew_xml_last_error()) };
        assert_eq!(
            last_error,
            Some(
                "xml: ill-formed document: close tag `</b>` does not match any open tag"
                    .to_string()
            )
        );
    }

    #[test]
    fn a_malformed_attribute_is_refused_not_dropped() {
        // `b` has no value. Dropping it hands back an element that claims not
        // to carry the attribute the document tried to give it.
        let node = parse("<a b>text</a>");
        assert!(node.is_null());
        // SAFETY: hew_xml_last_error returns a malloc-allocated error string or null.
        let last_error = unsafe { read_and_free_optional_cstr(hew_xml_last_error()) };
        assert_eq!(
            last_error,
            Some("xml: element `a` has a malformed attribute".to_string())
        );
    }

    #[test]
    fn a_text_only_document_has_no_root_element() {
        let node = parse("just text");
        assert!(node.is_null());
        // SAFETY: hew_xml_last_error returns a malloc-allocated error string or null.
        let last_error = unsafe { read_and_free_optional_cstr(hew_xml_last_error()) };
        assert_eq!(
            last_error,
            Some("xml: document has no root element".to_string())
        );
    }

    #[test]
    fn a_single_root_with_a_declaration_and_comments_still_parses() {
        let xml = "<?xml version=\"1.0\"?><!-- lead --><root a=\"1\"><kid/></root><!-- trail -->";
        let node = parse(xml);
        assert!(!node.is_null());
        // SAFETY: node is a valid HewXmlNode from parse.
        unsafe {
            assert_eq!(read_and_free_cstr(hew_xml_get_tag(node)), "root");
            assert_eq!(
                read_and_free_cstr(hew_xml_get_attribute(node, c"a".as_ptr())),
                "1"
            );
            assert_eq!(hew_xml_children_count(node), 1);
            hew_xml_free(node);
        }
    }

    #[test]
    fn to_string_null_returns_null() {
        // SAFETY: testing null-safety of hew_xml_to_string.
        unsafe {
            let s = hew_xml_to_string(std::ptr::null());
            assert!(s.is_null());
        }
    }

    #[test]
    fn get_attribute_null_node_returns_empty() {
        let key = CString::new("id").unwrap();
        // SAFETY: testing null-safety of hew_xml_get_attribute.
        unsafe {
            let val = hew_xml_get_attribute(std::ptr::null(), key.as_ptr());
            assert!(!val.is_null());
            let result = read_and_free_cstr(val);
            assert_eq!(result, "");
        }
    }
}
