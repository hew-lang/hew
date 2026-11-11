//! Every format's document parser, then the format's own writer.
//!
//! The first byte selects JSON, YAML, TOML, CBOR or MessagePack; the rest is
//! the document. Properties: parsing never panics and stays within the heap
//! bound; a parsed document writes back to text the same parser accepts, and
//! writing is a fixed point (`write(parse(write(v))) == write(v)`).
#![no_main]

use hew_codec::{Format, Sink, Source, Value};
use libfuzzer_sys::fuzz_target;

mod support;

fn read(format: Format, bytes: &[u8]) -> Option<Value> {
    let mut source = Source::new(format, bytes).ok()?;
    Some(
        source
            .read_any()
            .expect("read_any never fails on a parsed document"),
    )
}

fn write(format: Format, value: &Value) -> Vec<u8> {
    let mut sink = Sink::new(format);
    sink.value(value);
    sink.finish()
}

fuzz_target!(|data: &[u8]| {
    let Some((&selector, document)) = data.split_first() else {
        return;
    };
    let format = support::format(selector);
    let bound = support::heap_bound(document.len());
    let Some(value) = support::bounded("parse", bound, || read(format, document)) else {
        return;
    };
    let first = write(format, &value);
    let Some(reread) = read(format, &first) else {
        panic!(
            "{format:?} writer emitted text its parser refuses: {:?}",
            String::from_utf8_lossy(&first)
        );
    };
    let second = write(format, &reread);
    assert_eq!(
        first, second,
        "{format:?} writing is not a fixed point for {value:?}"
    );
});
