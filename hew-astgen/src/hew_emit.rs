//! Hew module skeleton emitter for `#[no_mangle] extern "C"` exports.
//!
//! Parses a Rust crate's `lib.rs`, extracts every `#[no_mangle] pub extern "C"`
//! function, maps its C-ABI types to Hew surface types, and emits a `.hew`
//! module skeleton with an `extern "C"` block.
//!
//! # ABI type-mapping convention
//!
//! | C ABI type           | Hew type  | Notes                                     |
//! |----------------------|-----------|-------------------------------------------|
//! | `*mut c_char`        | `String`  | malloc-allocated NUL-terminated string    |
//! | `*const c_char`      | `String`  | input string (caller owns)                |
//! | `i32`                | `bool`    | 0/1 sentinel pattern (hew-cabi convention)|
//! | `()`  / no return    | —         | procedure; no `->` in extern block        |
//!
//! The `i32 → bool` mapping is applied **only** when the function returns `i32`.
//! Functions whose parameter is `*mut c_char` or `*const c_char` have that
//! parameter mapped to `String`.
//!
//! # Skipped functions
//!
//! Functions whose name ends with `_free` are the memory-management companion
//! exported by hew-cabi modules.  They are runtime/Drop concerns, not public
//! API; the generator omits them.
//!
//! # Calibration shim note
//!
//! The `i32 → bool` heuristic is an approximation: it is correct for the
//! uuid module (`hew_uuid_parse` returns 0/1) but not for functions that
//! return an opaque discriminant or an integer count.  When this becomes
//! incorrect for a module, mark the affected line with
//! `// INTERNAL-ABI: <reason>` in the hand-edited `.hew` file, per
//! `json.hew` conventions.  The generator is intentionally conservative and
//! emits `i32` for all other integer-returning functions to avoid silent
//! misclassification.

use std::fmt::Write;

use quote::ToTokens;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HewEmitError {
    MissingTypePathSegment { ty: String },
}

impl std::fmt::Display for HewEmitError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingTypePathSegment { ty } => {
                write!(f, "failed to map Rust type with no path segments: {ty}")
            }
        }
    }
}

impl std::error::Error for HewEmitError {}

/// A single extern "C" function extracted from Rust source.
#[derive(Debug)]
pub struct ExternFn {
    /// The Rust symbol name (e.g. `hew_uuid_v4`).
    pub name: String,
    /// Hew-mapped parameter types, as `(param_name, hew_type)` pairs.
    pub params: Vec<(String, String)>,
    /// Hew-mapped return type, or `None` for `()`.
    pub return_ty: Option<String>,
}

/// Extract all `#[no_mangle] pub extern "C"` functions from Rust source.
///
/// Skips functions whose name ends with `_free` — those are memory-management
/// companions, not public Hew API.
pub fn extract_extern_fns(source: &str) -> Result<Vec<ExternFn>, HewEmitError> {
    let file = syn::parse_file(source).expect("Failed to parse Rust source");
    extract_extern_fns_from_file(&file)
}

fn extract_extern_fns_from_file(file: &syn::File) -> Result<Vec<ExternFn>, HewEmitError> {
    let mut fns = Vec::new();

    for item in &file.items {
        let syn::Item::Fn(f) = item else { continue };

        // Require #[no_mangle]
        if !has_no_mangle(&f.attrs) {
            continue;
        }
        // Require `extern "C"` ABI
        if !is_extern_c(f) {
            continue;
        }

        let name = f.sig.ident.to_string();

        // Skip memory-management companions — they are Drop/runtime concerns.
        // WHY: hew_uuid_free (and analogues) are not callable from Hew source;
        //      Drop is handled by the runtime.
        // WHEN obsolete: if Hew ever exposes explicit `free` for a type, remove
        //                this skip and emit a `Drop` impl instead.
        // REAL SOLUTION: synthesise `impl Drop for <type>` when `_free` is found.
        if name.ends_with("_free") {
            continue;
        }

        let params = extract_params(&f.sig)?;
        let return_ty = extract_return_ty(&f.sig)?;

        fns.push(ExternFn {
            name,
            params,
            return_ty,
        });
    }

    Ok(fns)
}

/// Generate a `.hew` module skeleton from a list of extern functions.
///
/// The `module_prefix` is the function-name prefix to strip when forming the
/// extern block entry names (e.g. `"hew_uuid_"` → strip to expose bare names).
/// If `module_prefix` is empty, names are emitted as-is.
///
/// The generated file has:
/// 1. A `// TODO` module doc comment placeholder.
/// 2. `extern "C"` block listing the ABI-mapped signatures.
///
/// It intentionally omits public wrapper functions and inline docs — those
/// require semantic knowledge a human must supply.
pub fn generate_hew_module(fns: &[ExternFn], module_prefix: &str) -> String {
    let mut out = String::with_capacity(1024);

    // Extern block
    let _ = writeln!(out, r#"extern "C" {{"#);
    for f in fns {
        let sym = &f.name;
        let params_str = f
            .params
            .iter()
            .map(|(n, t)| format!("{n}: {t}"))
            .collect::<Vec<_>>()
            .join(", ");
        match &f.return_ty {
            Some(ret) => {
                let _ = writeln!(out, "    fn {sym}({params_str}) -> {ret};");
            }
            None => {
                let _ = writeln!(out, "    fn {sym}({params_str});");
            }
        }
    }
    let _ = writeln!(out, "}}");

    // After the extern block, emit stub public wrappers as a guide.
    // Each wrapper is a TODO: the human fills in the correct Hew name,
    // docstring, and any type refinements.
    if !fns.is_empty() {
        out.push('\n');
        out.push_str(
            "// ── Public API stubs (rename, document, refine types) ─────────────────────\n",
        );
        out.push('\n');
        for f in fns {
            let sym = &f.name;
            // Strip module prefix to form a candidate public name.
            let pub_name = sym.strip_prefix(module_prefix).unwrap_or(sym.as_str());
            let params_str = f
                .params
                .iter()
                .map(|(n, t)| format!("{n}: {t}"))
                .collect::<Vec<_>>()
                .join(", ");
            let call_args = f
                .params
                .iter()
                .map(|(n, _)| n.as_str())
                .collect::<Vec<_>>()
                .join(", ");
            if let Some(ret) = &f.return_ty {
                let _ = writeln!(out, "pub fn {pub_name}({params_str}) -> {ret} {{");
                let _ = writeln!(out, "    unsafe {{ {sym}({call_args}) }}");
                let _ = writeln!(out, "}}");
            } else {
                let _ = writeln!(out, "pub fn {pub_name}({params_str}) {{");
                let _ = writeln!(out, "    unsafe {{ {sym}({call_args}) }}");
                let _ = writeln!(out, "}}");
            }
            out.push('\n');
        }
    }

    out
}

// ── Helpers ───────────────────────────────────────────────────────────────────

fn has_no_mangle(attrs: &[syn::Attribute]) -> bool {
    attrs.iter().any(|a| a.path().is_ident("no_mangle"))
}

fn is_extern_c(f: &syn::ItemFn) -> bool {
    match &f.sig.abi {
        Some(abi) => match &abi.name {
            Some(name) => name.value() == "C",
            None => true, // bare `extern fn` defaults to C
        },
        None => false,
    }
}

/// Map a syn `Type` to a Hew surface type string.
fn map_type(ty: &syn::Type) -> Result<String, HewEmitError> {
    match ty {
        syn::Type::Ptr(ptr) => {
            // *mut c_char and *const c_char → String
            if let syn::Type::Path(tp) = ptr.elem.as_ref() {
                let ident = last_path_ident(&tp.path, ptr.elem.as_ref())?;
                if ident == "c_char" {
                    return Ok("String".to_string());
                }
            }
            // Other pointer types: emit as unknown annotation
            Ok(format!("/* {} */", ty.to_token_stream()))
        }
        syn::Type::Path(tp) => {
            let ident = last_path_ident(&tp.path, ty)?;
            Ok(match ident.as_str() {
                "i32" => "i32".to_string(),
                "i64" => "i64".to_string(),
                "u32" => "u32".to_string(),
                "u64" => "u64".to_string(),
                "bool" => "bool".to_string(),
                "f64" => "f64".to_string(),
                "usize" => "usize".to_string(),
                // Anything else is named as-is — the human will fix it.
                _ => ident,
            })
        }
        syn::Type::Tuple(t) if t.elems.is_empty() => {
            // () — not used as a return type in the generated output; caller
            // maps this to `return_ty = None`.
            Ok("()".to_string())
        }
        _ => Ok(format!("/* {} */", ty.to_token_stream())),
    }
}

fn last_path_ident(path: &syn::Path, ty: &syn::Type) -> Result<String, HewEmitError> {
    path.segments
        .last()
        .map(|segment| segment.ident.to_string())
        .ok_or_else(|| HewEmitError::MissingTypePathSegment {
            ty: {
                let ty = ty.to_token_stream().to_string();
                if ty.is_empty() {
                    "<empty path>".to_string()
                } else {
                    ty
                }
            },
        })
}

fn extract_params(sig: &syn::Signature) -> Result<Vec<(String, String)>, HewEmitError> {
    let mut out = Vec::new();
    for (i, input) in sig.inputs.iter().enumerate() {
        let (name, hew_ty) = match input {
            syn::FnArg::Receiver(_) => continue,
            syn::FnArg::Typed(pat_ty) => {
                let name = match pat_ty.pat.as_ref() {
                    syn::Pat::Ident(pi) => pi.ident.to_string(),
                    _ => format!("arg{i}"),
                };
                let hew_ty = map_type(&pat_ty.ty)?;
                (name, hew_ty)
            }
        };
        out.push((name, hew_ty));
    }
    Ok(out)
}

fn extract_return_ty(sig: &syn::Signature) -> Result<Option<String>, HewEmitError> {
    match &sig.output {
        syn::ReturnType::Default => Ok(None),
        syn::ReturnType::Type(_, ty) => {
            // Treat `()` as no return
            if let syn::Type::Tuple(t) = ty.as_ref() {
                if t.elems.is_empty() {
                    return Ok(None);
                }
            }
            let mapped = map_type(ty)?;
            // Apply i32 → bool for functions whose C ABI uses 0/1 sentinel.
            // HEURISTIC: i32 return → bool when the mapped name is "i32".
            // WHY: hew-cabi convention for boolean predicates (e.g. hew_uuid_parse).
            // WHEN obsolete: if a module returns a true integer result as i32
            //   (e.g. an array length), the hand-edit adds
            //   `// INTERNAL-ABI: C ABI returns count as i32` and keeps `i32`.
            // REAL SOLUTION: explicit annotation on the Rust side (attribute or
            //   naming convention) that distinguishes sentinel from count/discriminant.
            if mapped == "i32" {
                return Ok(Some("bool".to_string()));
            }
            Ok(Some(mapped))
        }
    }
}

// ── Tests ──────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ── extract_extern_fns ─────────────────────────────────────────────────

    #[test]
    fn skips_fns_without_no_mangle() {
        let src = r#"
            pub extern "C" fn hew_foo() -> i32 { 0 }
        "#;
        let fns = extract_extern_fns(src).unwrap();
        assert!(fns.is_empty(), "should skip fn without #[no_mangle]");
    }

    #[test]
    fn skips_fns_without_extern_c() {
        let src = r"
            #[no_mangle]
            pub fn hew_foo() -> i32 { 0 }
        ";
        let fns = extract_extern_fns(src).unwrap();
        assert!(fns.is_empty(), "should skip fn without extern \"C\"");
    }

    #[test]
    fn skips_free_fns() {
        let src = r#"
            #[no_mangle]
            pub unsafe extern "C" fn hew_uuid_free(s: *mut c_char) {}
        "#;
        let fns = extract_extern_fns(src).unwrap();
        assert!(fns.is_empty(), "should skip _free functions");
    }

    #[test]
    fn extracts_no_arg_string_return() {
        let src = r#"
            #[no_mangle]
            pub extern "C" fn hew_uuid_v4() -> *mut c_char {
                todo!()
            }
        "#;
        let fns = extract_extern_fns(src).unwrap();
        assert_eq!(fns.len(), 1);
        assert_eq!(fns[0].name, "hew_uuid_v4");
        assert!(fns[0].params.is_empty());
        assert_eq!(fns[0].return_ty.as_deref(), Some("String"));
    }

    #[test]
    fn maps_const_char_ptr_param_to_string() {
        let src = r#"
            use std::ffi::c_char;
            #[no_mangle]
            pub unsafe extern "C" fn hew_uuid_parse(s: *const c_char) -> i32 {
                0
            }
        "#;
        let fns = extract_extern_fns(src).unwrap();
        assert_eq!(fns.len(), 1);
        assert_eq!(fns[0].params.len(), 1);
        assert_eq!(fns[0].params[0].0, "s");
        assert_eq!(fns[0].params[0].1, "String");
    }

    #[test]
    fn maps_i32_return_to_bool() {
        let src = r#"
            #[no_mangle]
            pub unsafe extern "C" fn hew_uuid_parse(s: *const c_char) -> i32 {
                0
            }
        "#;
        let fns = extract_extern_fns(src).unwrap();
        assert_eq!(fns[0].return_ty.as_deref(), Some("bool"));
    }

    #[test]
    fn errors_when_a_type_path_has_no_segments() {
        let mut file = syn::parse_file(
            r#"
                #[no_mangle]
                pub unsafe extern "C" fn hew_uuid_parse(s: i32) -> i32 {
                    s
                }
            "#,
        )
        .unwrap();
        let syn::Item::Fn(func) = &mut file.items[0] else {
            panic!("expected function item");
        };
        let syn::FnArg::Typed(pat_ty) = &mut func.sig.inputs[0] else {
            panic!("expected typed argument");
        };
        *pat_ty.ty = syn::Type::Path(syn::TypePath {
            qself: None,
            path: syn::Path {
                leading_colon: None,
                segments: syn::punctuated::Punctuated::new(),
            },
        });

        let err = extract_extern_fns_from_file(&file).unwrap_err();
        assert_eq!(
            err,
            HewEmitError::MissingTypePathSegment {
                ty: "<empty path>".to_string()
            }
        );
    }

    // ── generate_hew_module ────────────────────────────────────────────────

    #[test]
    fn extern_block_contains_all_extracted_fns() {
        let fns = vec![
            ExternFn {
                name: "hew_uuid_v4".to_string(),
                params: vec![],
                return_ty: Some("String".to_string()),
            },
            ExternFn {
                name: "hew_uuid_v7".to_string(),
                params: vec![],
                return_ty: Some("String".to_string()),
            },
            ExternFn {
                name: "hew_uuid_parse".to_string(),
                params: vec![("s".to_string(), "String".to_string())],
                return_ty: Some("bool".to_string()),
            },
        ];
        let out = generate_hew_module(&fns, "hew_uuid_");
        assert!(out.contains(r#"extern "C" {"#));
        assert!(out.contains("fn hew_uuid_v4() -> String;"));
        assert!(out.contains("fn hew_uuid_v7() -> String;"));
        assert!(out.contains("fn hew_uuid_parse(s: String) -> bool;"));
    }

    #[test]
    fn public_stubs_strip_module_prefix() {
        let fns = vec![ExternFn {
            name: "hew_uuid_v4".to_string(),
            params: vec![],
            return_ty: Some("String".to_string()),
        }];
        let out = generate_hew_module(&fns, "hew_uuid_");
        assert!(out.contains("pub fn v4() -> String {"));
        assert!(out.contains("unsafe { hew_uuid_v4() }"));
    }

    #[test]
    fn uuid_lib_rs_extracts_three_fns_skips_free() {
        // Integration: run against the real uuid lib.rs.
        // Verifies that _free is skipped and the three public fns are found.
        let manifest_dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let lib_rs_path = manifest_dir.join("../std/misc/uuid/src/lib.rs");
        let Ok(src) = std::fs::read_to_string(&lib_rs_path) else {
            // Path doesn't exist in test environment — skip.
            return;
        };
        let fns = extract_extern_fns(&src).unwrap();
        assert_eq!(
            fns.len(),
            3,
            "Expected 3 fns (v4, v7, parse), skipping _free; got: {:?}",
            fns.iter().map(|f| &f.name).collect::<Vec<_>>()
        );
        let names: Vec<_> = fns.iter().map(|f| f.name.as_str()).collect();
        assert!(names.contains(&"hew_uuid_v4"));
        assert!(names.contains(&"hew_uuid_v7"));
        assert!(names.contains(&"hew_uuid_parse"));
    }

    #[test]
    fn uuid_generated_output_parses_as_valid_hew() {
        // Integration: generate a Hew module from uuid/src/lib.rs and verify
        // the output is valid Hew syntax.  We call hew-parser directly;
        // no binary required.
        let manifest_dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let lib_rs_path = manifest_dir.join("../std/misc/uuid/src/lib.rs");
        let Ok(src) = std::fs::read_to_string(&lib_rs_path) else {
            return;
        };
        let fns = extract_extern_fns(&src).unwrap();
        let hew_src = generate_hew_module(&fns, "hew_uuid_");

        let result = hew_parser::parse(&hew_src);
        assert!(
            result.errors.is_empty(),
            "Generated Hew module has parse errors:\n{}",
            result
                .errors
                .iter()
                .map(|e| format!("  {}: {}", e.span.start, e.message))
                .collect::<Vec<_>>()
                .join("\n")
        );
    }
}
