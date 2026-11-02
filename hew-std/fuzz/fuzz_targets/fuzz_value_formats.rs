//! `std.encoding.json`, `yaml` and `toml` dynamic values through the C ABI.
//!
//! The first byte selects the format. Properties: parsing never panics or
//! leaks; a parsed value stringifies, and the text parses back to an equal
//! value under the format's own equality (or, for a TOML NaN, which that
//! equality never matches, to the same text).
#![no_main]

use hew_std::{json, toml, yaml};
use libfuzzer_sys::fuzz_target;

mod support;
use support::{take, Managed};

macro_rules! round_trip {
    ($text:expr, $module:ident, $parse:ident, $stringify:ident, $eq:ident, $free:ident, $last:ident) => {{
        let input = Managed::new($text);
        // SAFETY: `input` is a live managed string for each borrow below, and
        // every value handle is released exactly once.
        unsafe {
            let value = $module::$parse(input.ptr());
            if value.is_null() {
                return;
            }
            let printed = $module::$stringify(value);
            let error = take($module::$last());
            if printed.is_null() && !error.is_empty() {
                // serde_yaml's emitter refuses some tagged mapping keys its
                // parser accepts; that reports an error rather than failing.
                assert!(
                    stringify!($module) == "yaml" && error.starts_with("yaml stringify failed"),
                    "{} cannot print a value it parsed: {error}",
                    stringify!($module)
                );
                $module::$free(value);
                return;
            }
            let printed = Managed::adopt(printed);
            let reparsed = $module::$parse(printed.ptr());
            let error = take($module::$last());
            if reparsed.is_null()
                && stringify!($module) == "yaml"
                && error.contains("scanning a tag")
            {
                // serde_yaml writes a local tag such as `!0]` verbatim, flow
                // indicators included, and its scanner then refuses it.
                $module::$free(value);
                return;
            }
            assert!(
                !reparsed.is_null(),
                "{} refuses its own output ({error}): {:?}",
                stringify!($module),
                printed.text()
            );
            if $module::$eq(value, reparsed) != 1 {
                // TOML equality is IEEE, so a NaN never equals itself; the
                // printed text must still be a fixed point.
                let again = Managed::adopt($module::$stringify(reparsed));
                assert!(
                    printed.text().contains("nan") && again.text() == printed.text(),
                    "{} round trip changed the value: {:?}",
                    stringify!($module),
                    printed.text()
                );
            }
            $module::$free(reparsed);
            $module::$free(value);
        }
    }};
}

fuzz_target!(|data: &[u8]| {
    let Some((selector, text)) = support::selector_text(data) else {
        return;
    };
    match selector % 3 {
        0 => round_trip!(
            text,
            json,
            hew_json_parse,
            hew_json_stringify,
            hew_json_eq,
            hew_json_free,
            hew_json_last_error
        ),
        1 => round_trip!(
            text,
            yaml,
            hew_yaml_parse,
            hew_yaml_stringify,
            hew_yaml_eq,
            hew_yaml_free,
            hew_yaml_last_error
        ),
        _ => round_trip!(
            text,
            toml,
            hew_toml_parse,
            hew_toml_stringify,
            hew_toml_eq,
            hew_toml_free,
            hew_toml_last_error
        ),
    }
});
