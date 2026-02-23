//! Stdlib stub generator for the Hew programming language.
//!
//! Collects all `#[hew_export]` metadata from the runtime and per-package
//! crates, then prints `.hew` stub files to stdout for review.
//!
//! Note: Type-checker tables are now generated at build time by
//! `hew-types/build.rs` from the canonical `.hew` files.
//!
//! # Usage
//!
//! ```text
//! cargo run -p hew-stdlib-gen
//! ```

use std::collections::BTreeMap;
use std::fmt::Write as FmtWrite;
use std::io;

use hew_export_types::ExportMeta;

fn main() -> io::Result<()> {
    let mut exports = hew_runtime::export_meta::all_exports();
    exports.extend(hew_std_misc_uuid::export_meta::all_exports());
    exports.extend(hew_std_misc_log::export_meta::all_exports());
    exports.extend(hew_std_encoding_markdown::export_meta::all_exports());

    // Group functions by module path, preserving insertion order per module.
    let mut by_module: BTreeMap<&str, Vec<&ExportMeta>> = BTreeMap::new();
    for meta in &exports {
        by_module.entry(meta.module).or_default().push(meta);
    }

    for (module, fns) in &by_module {
        let hew_src = render_hew_stub(module, fns);
        println!("--- {module} ---");
        print!("{hew_src}");
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// .hew stub rendering
// ---------------------------------------------------------------------------

fn render_hew_stub(module: &str, fns: &[&ExportMeta]) -> String {
    let mut s = String::new();

    // Module-level doc comment
    writeln!(s, "//! Functions exported from `{module}`.").unwrap();
    writeln!(s, "//!").unwrap();
    writeln!(s, "//! import {module};").unwrap();
    writeln!(s).unwrap();

    // Extern block
    writeln!(s, "extern \"C\" {{").unwrap();
    for meta in fns {
        if !meta.doc.is_empty() {
            writeln!(s, "    /// {}", meta.doc).unwrap();
        }
        let params: Vec<String> = meta
            .params
            .iter()
            .map(|(name, ty)| format!("{name}: {ty}"))
            .collect();
        let params_str = params.join(", ");
        match meta.return_type {
            Some(ret) => {
                writeln!(s, "    fn {}({params_str}) -> {ret};", meta.hew_name).unwrap();
            }
            None => {
                writeln!(s, "    fn {}({params_str});", meta.hew_name).unwrap();
            }
        }
    }
    writeln!(s, "}}").unwrap();

    s
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn exports_collected_from_packages() {
        let mut exports = hew_runtime::export_meta::all_exports();
        exports.extend(hew_std_misc_uuid::export_meta::all_exports());
        exports.extend(hew_std_misc_log::export_meta::all_exports());
        exports.extend(hew_std_encoding_markdown::export_meta::all_exports());

        // uuid: v4, v7, parse
        assert!(exports.iter().any(|e| e.c_name == "hew_uuid_v4"));
        assert!(exports.iter().any(|e| e.c_name == "hew_uuid_v7"));
        assert!(exports.iter().any(|e| e.c_name == "hew_uuid_parse"));

        // markdown: to_html, to_html_safe
        assert!(exports.iter().any(|e| e.c_name == "hew_markdown_to_html"));
        assert!(exports
            .iter()
            .any(|e| e.c_name == "hew_markdown_to_html_safe"));

        // log: init, init_level, error, warn, info, debug, trace
        assert!(exports.iter().any(|e| e.c_name == "hew_log_init"));
        assert!(exports.iter().any(|e| e.c_name == "hew_log_init_level"));
        assert!(exports.iter().any(|e| e.c_name == "hew_log_error"));

        assert_eq!(exports.len(), 12, "expected 3+2+7=12 total exports");
    }

    #[test]
    fn render_hew_stub_basic() {
        use hew_export_types::ExportMeta;
        let meta = ExportMeta {
            module: "std::test",
            hew_name: "hello",
            c_name: "hew_test_hello",
            params: vec![("name", "string")],
            return_type: Some("string"),
            doc: "Say hello.",
        };
        let stub = render_hew_stub("std::test", &[&meta]);
        assert!(stub.contains("fn hello(name: string) -> string;"));
        assert!(stub.contains("/// Say hello."));
        assert!(stub.contains("import std::test;"));
    }
}
