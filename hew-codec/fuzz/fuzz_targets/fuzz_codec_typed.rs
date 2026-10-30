//! The typed decode walk over a fixed record shape in every format.
//!
//! The shape covers each node kind: integers with ranges, floats, char,
//! string, bytes, options, tuples, sequences, sets, string- and
//! integer-keyed maps, a payload-carrying variant and a recursive tree.
//! Properties: decoding never panics and stays within the heap bound; a
//! fully decoded value encodes, that encoding decodes without error, and
//! encoding is a fixed point (sets and maps come back in canonical order).
#![no_main]

use std::sync::OnceLock;

use hew_codec::property::{Key, Node, Shape};
use hew_codec::{typed, Member};
use libfuzzer_sys::fuzz_target;

mod support;

fn key(name: &str, tag: u64, flags: u32) -> Key {
    Key {
        name: name.to_owned(),
        tag: Some(tag),
        flags,
    }
}

fn shape() -> &'static Shape {
    static SHAPE: OnceLock<Shape> = OnceLock::new();
    SHAPE.get_or_init(|| {
        let nodes = vec![
            /* 0 */
            Node::Record(vec![
                (key("name", 1, 0), 1),
                (key("port", 2, 0), 2),
                (key("tags", 3, 0), 3),
                (key("limits", 4, 0), 5),
                (key("blob", 5, 0), 6),
                (key("timeout", 6, Member::ACCEPT_ABSENT), 7),
                (key("outcome", 7, 0), 8),
                (key("ratio", 8, 0), 9),
                (key("pair", 9, 0), 10),
                (key("ids", 10, 0), 11),
                (key("grade", 11, 0), 12),
                (key("tree", 12, Member::ACCEPT_ABSENT), 13),
                (
                    key("codes", 13, Member::OMIT_NULL | Member::ACCEPT_ABSENT),
                    15,
                ),
                (key("small", 14, 0), 16),
                (key("triple", 15, 0), 17),
            ]),
            /* 1 */ Node::String,
            /* 2 */ Node::Unsigned { max: 65_535 },
            /* 3 */ Node::Sequence(1),
            /* 4 */
            Node::Signed {
                min: i64::MIN,
                max: i64::MAX,
            },
            /* 5 */ Node::Map { key: 1, value: 4 },
            /* 6 */ Node::Bytes,
            /* 7 */ Node::Option(4),
            /* 8 */
            Node::Variant(vec![
                (key("Ok", 0, Member::PAYLOAD), Some(4)),
                (key("Err", 1, Member::PAYLOAD), Some(1)),
                (key("Pending", 2, 0), None),
            ]),
            /* 9 */ Node::Float { bits: 64 },
            /* 10 */ Node::Tuple(vec![4, 9]),
            /* 11 */ Node::Set(4),
            /* 12 */ Node::Char,
            /* 13 */
            Node::Record(vec![(key("value", 1, 0), 4), (key("children", 2, 0), 14)]),
            /* 14 */ Node::Sequence(13),
            /* 15 */ Node::Map { key: 4, value: 1 },
            /* 16 */ Node::Float { bits: 32 },
            /* 17 */
            Node::Array {
                element: 18,
                len: 3,
            },
            /* 18 */
            Node::Signed {
                min: -128,
                max: 127,
            },
        ];
        let count = nodes.len();
        Shape {
            root: 0,
            nodes,
            names: vec![String::new(); count],
            aggregates: vec![None; count],
            variants: vec![None; count],
            grouped_payloads: vec![false; count],
        }
    })
}

fuzz_target!(|data: &[u8]| {
    let Some((&selector, document)) = data.split_first() else {
        return;
    };
    let format = support::format(selector);
    let shape = shape();
    let bound = support::heap_bound(document.len());
    let Ok(decoded) = support::bounded("decode", bound, || typed::decode(shape, format, document))
    else {
        return;
    };
    if decoded.error.is_some() {
        return;
    }
    let encoded = typed::encode(shape, format, &decoded.value)
        .unwrap_or_else(|err| panic!("{format:?} cannot encode a value it decoded: {err:?}"));
    let again = typed::decode(shape, format, &encoded).unwrap_or_else(|err| {
        panic!(
            "{format:?} refuses its own encoding ({err:?}): {:?}",
            String::from_utf8_lossy(&encoded)
        )
    });
    assert!(
        again.error.is_none(),
        "{format:?} refuses its own encoding: {:?}",
        again.error
    );
    let reencoded = typed::encode(shape, format, &again.value)
        .unwrap_or_else(|err| panic!("{format:?} cannot re-encode its own value: {err:?}"));
    assert_eq!(
        encoded, reencoded,
        "{format:?} encoding is not a fixed point: {:?} then {:?}",
        decoded.value, again.value
    );
});
