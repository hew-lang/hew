//! Ordinary YAML values behind the anchor and alias guard.
//!
//! Properties: parsing never panics; peak heap respects the absolute alias
//! expansion policy plus parser overhead; and an admitted document reads
//! exactly as `serde_yaml` reads it.
#![no_main]

use libfuzzer_sys::fuzz_target;

mod support;

fuzz_target!(|data: &[u8]| {
    let Ok(text) = std::str::from_utf8(data) else {
        return;
    };
    // Ordinary YAML deliberately uses an absolute expansion budget, not a
    // ratio to the input size. Allow tree/container overhead for that budget;
    // keep the stricter input-relative bound for the typed wire decoders.
    let bound = support::heap_bound(text.len())
        .max(16 * hew_codec::yaml_value::YAML_EXPANSION_BUDGET_UNITS);
    let ours = support::bounded("yaml", bound, || hew_codec::yaml_value::parse(text));
    if let Ok(value) = ours {
        let reference: serde_yaml::Value =
            serde_yaml::from_str(text).expect("the guard only refuses, it never repairs");
        assert_eq!(value, reference);
    }
});
