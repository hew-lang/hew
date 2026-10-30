//! `std.encoding.xml` documents through the C ABI.
//!
//! Properties: parsing never panics or overflows the stack, whatever the
//! nesting; navigating a parsed tree never panics; and serialization keeps
//! every value: the printed text parses back and prints identically.
#![no_main]

use hew_std::xml::{
    hew_xml_children_count, hew_xml_free, hew_xml_get_child, hew_xml_get_tag, hew_xml_get_text,
    hew_xml_parse, hew_xml_to_string, HewXmlNode,
};
use libfuzzer_sys::fuzz_target;

mod support;
use support::{take, Managed};

fn parse(text: &str) -> *mut HewXmlNode {
    let input = Managed::new(text);
    // SAFETY: `input` is live for the call.
    unsafe { hew_xml_parse(input.ptr()) }
}

fn print(node: *const HewXmlNode) -> String {
    // SAFETY: `node` is a live parsed tree.
    take(unsafe { hew_xml_to_string(node) })
}

/// Visit the first few children of each level through the navigation ABI.
fn walk(node: *const HewXmlNode, budget: &mut usize) {
    if *budget == 0 {
        return;
    }
    *budget -= 1;
    // SAFETY: `node` is live; each child is an owned clone released below.
    unsafe {
        drop(take(hew_xml_get_tag(node)));
        drop(take(hew_xml_get_text(node)));
        let count = hew_xml_children_count(node);
        for index in 0..count.min(4) {
            let child = hew_xml_get_child(node, index);
            assert!(!child.is_null(), "child {index} of {count} is absent");
            walk(child, budget);
            hew_xml_free(child);
        }
    }
}

fuzz_target!(|data: &[u8]| {
    let Ok(text) = std::str::from_utf8(data) else {
        return;
    };
    let tree = parse(text);
    if tree.is_null() {
        return;
    }
    walk(tree, &mut 64);
    let first = print(tree);
    let again = parse(&first);
    assert!(!again.is_null(), "XML refuses its own output: {first:?}");
    let second = print(again);
    assert_eq!(first, second, "XML serialization lost a value");
    // SAFETY: both trees are live and released once.
    unsafe {
        hew_xml_free(again);
        hew_xml_free(tree);
    }
});
