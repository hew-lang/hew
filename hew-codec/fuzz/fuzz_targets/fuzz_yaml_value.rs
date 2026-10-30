//! Ordinary YAML values behind the anchor and alias guard.
//!
//! Properties: parsing never panics; whatever the guard admits parses within
//! the heap bound, so alias expansion cannot amplify a small document into a
//! large tree; and an admitted document reads exactly as `serde_yaml` reads it.
#![no_main]

use libfuzzer_sys::fuzz_target;

mod support;

fuzz_target!(|data: &[u8]| {
    let Ok(text) = std::str::from_utf8(data) else {
        return;
    };
    let bound = support::heap_bound(text.len());
    let ours = support::bounded("yaml", bound, || hew_codec::yaml_value::parse(text));
    if let Ok(value) = ours {
        let reference: serde_yaml::Value =
            serde_yaml::from_str(text).expect("the guard only refuses, it never repairs");
        assert_eq!(value, reference);
    }
});
