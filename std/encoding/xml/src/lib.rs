//! Hew `std::encoding::xml` — XML parsing and serialization.
//!
//! Provides XML parsing, serialization, and node access for compiled Hew
//! programs. All returned strings are allocated with `libc::malloc` and
//! NUL-terminated. All returned [`HewXmlNode`] pointers are heap-allocated
//! via `Box` and must be freed with [`hew_xml_free`].

// Force-link hew-runtime so the linker can resolve hew_vec_* symbols
// referenced by hew-cabi's object code.
#[cfg(test)]
extern crate hew_runtime;

use hew_cabi::cabi::str_to_malloc;
use std::ffi::CStr;
use std::os::raw::c_char;

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

/// Extract tag name and attributes from a [`BytesStart`] event.
fn extract_tag_and_attrs(e: &BytesStart<'_>) -> (String, Vec<(String, String)>) {
    let tag = String::from_utf8_lossy(e.name().as_ref()).to_string();
    let attributes = e
        .attributes()
        .filter_map(Result::ok)
        .map(|a| {
            let key = String::from_utf8_lossy(a.key.as_ref()).to_string();
            let val = String::from_utf8_lossy(&a.value).to_string();
            (key, val)
        })
        .collect();
    (tag, attributes)
}

/// Push a node onto the parent frame or into the top-level list.
fn push_node(stack: &mut [Frame], top_level: &mut Vec<XmlNodeKind>, node: XmlNodeKind) {
    if let Some(parent) = stack.last_mut() {
        parent.children.push(node);
    } else {
        top_level.push(node);
    }
}

/// Build a tree of [`XmlNodeKind`] from an XML string using quick-xml.
fn parse_xml(xml: &str) -> Option<XmlNodeKind> {
    use quick_xml::events::Event;
    use quick_xml::reader::Reader;

    let mut reader = Reader::from_str(xml);
    let mut stack: Vec<Frame> = Vec::new();
    let mut top_level: Vec<XmlNodeKind> = Vec::new();

    loop {
        match reader.read_event() {
            Ok(Event::Start(ref e)) => {
                let (tag, attributes) = extract_tag_and_attrs(e);
                stack.push(Frame {
                    tag,
                    attributes,
                    children: Vec::new(),
                });
            }
            Ok(Event::End(_)) => {
                let frame = stack.pop()?;
                let node = XmlNodeKind::Element {
                    tag: frame.tag,
                    attributes: frame.attributes,
                    children: frame.children,
                };
                push_node(&mut stack, &mut top_level, node);
            }
            Ok(Event::Empty(ref e)) => {
                let (tag, attributes) = extract_tag_and_attrs(e);
                let node = XmlNodeKind::Element {
                    tag,
                    attributes,
                    children: Vec::new(),
                };
                push_node(&mut stack, &mut top_level, node);
            }
            Ok(Event::Text(ref e)) => {
                let text = e.unescape().ok()?.to_string();
                if !text.trim().is_empty() {
                    push_node(&mut stack, &mut top_level, XmlNodeKind::Text(text));
                }
            }
            Ok(Event::CData(ref e)) => {
                let text = String::from_utf8_lossy(e.as_ref()).to_string();
                if !text.is_empty() {
                    push_node(&mut stack, &mut top_level, XmlNodeKind::Text(text));
                }
            }
            Ok(Event::Eof) => break,
            Ok(_) => {}
            Err(_) => return None,
        }
    }

    if !stack.is_empty() {
        return None;
    }

    match top_level.len() {
        0 => None,
        1 => Some(top_level.remove(0)),
        _ => Some(XmlNodeKind::Element {
            tag: String::new(),
            attributes: Vec::new(),
            children: top_level,
        }),
    }
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
        return std::ptr::null_mut();
    }
    // SAFETY: xml_str is a valid NUL-terminated C string per caller contract.
    let Ok(s) = unsafe { CStr::from_ptr(xml_str) }.to_str() else {
        return std::ptr::null_mut();
    };
    match parse_xml(s) {
        Some(tree) => boxed_node(tree),
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
pub unsafe extern "C" fn hew_xml_get_child(
    node: *const HewXmlNode,
    index: i32,
) -> *mut HewXmlNode {
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
        XmlNodeKind::Element { children, .. } => {
            children.get(index as usize).map_or(std::ptr::null_mut(), |c| boxed_node(c.clone()))
        }
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
    unsafe { libc::free(s.cast()) };
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

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

        // SAFETY: null pointer is safe for hew_xml_parse.
        unsafe {
            assert!(hew_xml_parse(std::ptr::null()).is_null());
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
}
