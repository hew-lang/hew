//! Structure-aware valid codec programs through physical and LLVM lowering.
//!
//! Raw bytes choose values and documents; the harness builds valid Hew source
//! that instantiates all five typed codecs. This reaches encoding-specific
//! ownership, aggregate and visitor lowering on every iteration instead of
//! waiting for byte mutations to discover a valid generic program.
#![no_main]

use std::fmt::Write as _;

use libfuzzer_sys::fuzz_target;

mod compiler;

fn program(data: &[u8]) -> String {
    let id = data
        .get(..8)
        .and_then(|bytes| bytes.try_into().ok())
        .map(i64::from_le_bytes)
        .unwrap_or(0);
    let enabled = data.get(8).is_some_and(|byte| byte & 1 == 1);
    let label_bytes = data.get(9..41).unwrap_or(&[]);
    let label = String::from_utf8_lossy(label_bytes);
    let label = serde_json::to_string(label.as_ref()).expect("string JSON encoding");
    let payload = data.get(41..105).unwrap_or(&[]);
    let mut payload_literal = String::new();
    for (index, byte) in payload.iter().enumerate() {
        if index != 0 {
            payload_literal.push_str(", ");
        }
        write!(&mut payload_literal, "{byte}").expect("write to string");
    }
    let text = String::from_utf8_lossy(data.get(105..233).unwrap_or(&[]));
    let text = serde_json::to_string(text.as_ref()).expect("string JSON encoding");
    format!(
        r#"import std.encoding.cbor;
import std.encoding.json;
import std.encoding.msgpack;
import std.encoding.toml;
import std.encoding.yaml;

type Packet {{
    id: i64;
    label: string;
    payload: bytes;
    enabled: bool;
    tags: Vec<i64>;
}}

fn main() {{
    let value = Packet {{
        id: {id},
        label: {label},
        payload: bytes [{payload_literal}],
        enabled: {enabled},
        tags: [{id}, 0, -1],
    }};
    let _json = json.decode<Packet>(json.encode(value));
    let _yaml = yaml.decode<Packet>(yaml.encode(value));
    let _toml = toml.decode<Packet>(toml.encode(value));
    let _cbor = cbor.decode<Packet>(cbor.encode(value));
    let _msgpack = msgpack.decode<Packet>(msgpack.encode(value));
    let text = {text};
    let _json_input = json.decode<Packet>(text);
    let _yaml_input = yaml.decode<Packet>(text);
    let _toml_input = toml.decode<Packet>(text);
    let raw = bytes [{payload_literal}];
    let _cbor_input = cbor.decode<Packet>(raw);
    let _msgpack_input = msgpack.decode<Packet>(raw);
}}
"#
    )
}

fuzz_target!(|data: &[u8]| {
    compiler::exercise(&program(data), true);
});
