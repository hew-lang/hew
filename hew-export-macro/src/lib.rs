//! Proc macro for annotating Hew runtime exports.
//!
//! The `#[hew_export]` attribute serves as the single source of truth for
//! stdlib function signatures, replacing the current triple-maintenance
//! problem where signatures must be kept in sync across the Rust runtime,
//! the type checker, and the `.hew` stub files.
//!
//! See [`hew_export_types::ExportMeta`] for the companion type returned by
//! the generated metadata functions.
//!
//! # Usage
//!
//! ```ignore
//! #[hew_export(module = "std::encoding::json", name = "parse", doc = "Parse a JSON string")]
//! pub unsafe extern "C" fn hew_json_parse(input: *const c_char) -> *const c_char {
//!     // ...
//! }
//! ```
//!
//! The attribute:
//! 1. Validates the function has `extern "C"` ABI.
//! 2. Generates a companion `__hew_export_meta_<fn_name>()` function that
//!    returns a [`hew_export_types::ExportMeta`] describing the function's
//!    Hew-level signature.
//! 3. Passes the original function through unchanged.

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    Expr, FnArg, Ident, ItemFn, Lit, Pat, PatType, ReturnType, Token, Type,
};

// ---------------------------------------------------------------------------
// Attribute argument parsing
// ---------------------------------------------------------------------------

struct HewExportArgs {
    module: String,
    name: Option<String>,
    doc: Option<String>,
}

/// A single `key = "value"` pair inside the attribute argument list.
struct KeyValue {
    key: Ident,
    value: String,
}

impl Parse for KeyValue {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let key: Ident = input.parse()?;
        let _eq: Token![=] = input.parse()?;
        let expr: Expr = input.parse()?;
        let value = match expr {
            Expr::Lit(expr_lit) => match expr_lit.lit {
                Lit::Str(s) => s.value(),
                other => return Err(syn::Error::new_spanned(other, "expected a string literal")),
            },
            other => return Err(syn::Error::new_spanned(other, "expected a string literal")),
        };
        Ok(KeyValue { key, value })
    }
}

impl Parse for HewExportArgs {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let pairs = Punctuated::<KeyValue, Token![,]>::parse_terminated(input)?;

        let mut module: Option<String> = None;
        let mut name: Option<String> = None;
        let mut doc: Option<String> = None;

        for kv in pairs {
            match kv.key.to_string().as_str() {
                "module" => module = Some(kv.value),
                "name" => name = Some(kv.value),
                "doc" => doc = Some(kv.value),
                other => {
                    return Err(syn::Error::new_spanned(
                        kv.key,
                        format!("unknown argument `{other}`; expected module, name, or doc"),
                    ))
                }
            }
        }

        let module = module.ok_or_else(|| {
            syn::Error::new(Span::call_site(), "missing required argument `module`")
        })?;

        Ok(HewExportArgs { module, name, doc })
    }
}

// ---------------------------------------------------------------------------
// Type mapping
// ---------------------------------------------------------------------------

/// Map a C/FFI Rust type path to its Hew type name.
///
/// Returns `None` when the type is unrecognised (the macro will emit a compile
/// error in that case).
fn map_type(ty: &Type) -> Option<&'static str> {
    match ty {
        Type::Ptr(ptr) => {
            // *const c_char / *mut c_char → string
            // *const u8    / *mut u8      → bytes
            match ptr.elem.as_ref() {
                Type::Path(p) => {
                    let seg = p.path.segments.last()?.ident.to_string();
                    match seg.as_str() {
                        "c_char" => Some("string"),
                        "u8" => Some("bytes"),
                        _ => None,
                    }
                }
                _ => None,
            }
        }
        Type::Path(p) => {
            let seg = p.path.segments.last()?.ident.to_string();
            match seg.as_str() {
                "i32" => Some("i32"),
                "i64" => Some("i64"),
                "f64" => Some("f64"),
                "bool" => Some("bool"),
                _ => None,
            }
        }
        Type::Tuple(t) if t.elems.is_empty() => Some("void"),
        _ => None,
    }
}

/// Map the return type annotation of a function to a Hew type name.
///
/// Returns `None` for void (no return / `-> ()`).
fn map_return_type(ret: &ReturnType) -> Result<Option<&'static str>, syn::Error> {
    match ret {
        ReturnType::Default => Ok(None),
        ReturnType::Type(_, ty) => {
            if let Type::Tuple(t) = ty.as_ref() {
                if t.elems.is_empty() {
                    return Ok(None);
                }
            }
            map_type(ty).map(Some).ok_or_else(|| {
                syn::Error::new_spanned(
                    ty,
                    "unsupported return type for #[hew_export]; \
                     supported types: *const c_char, *mut c_char, *const u8, *mut u8, \
                     i32, i64, f64, bool",
                )
            })
        }
    }
}

// ---------------------------------------------------------------------------
// Proc macro entry point
// ---------------------------------------------------------------------------

/// Annotate a `pub extern "C"` runtime function with Hew stdlib metadata.
///
/// # Arguments
///
/// - `module` *(required)* — Hew module path, e.g. `"std::encoding::json"`.
/// - `name` *(optional)* — Hew function name.  Defaults to stripping the
///   `hew_<last_module_segment>_` prefix from the Rust function name, or
///   just `hew_` if the module segment doesn't match.
/// - `doc` *(optional)* — Documentation string included in the metadata.
///
/// The attribute emits the original function unchanged, plus a companion
/// `#[doc(hidden)] pub fn __hew_export_meta_<fn_name>() ->
/// hew_export_types::ExportMeta` function that a build tool can collect to
/// auto-generate type-checker stubs and `.hew` source files.
///
/// # Example
///
/// ```ignore
/// #[hew_export(module = "std::encoding::json", name = "parse", doc = "Parse a JSON string")]
/// pub unsafe extern "C" fn hew_json_parse(input: *const c_char) -> *const c_char {
///     todo!()
/// }
/// ```
#[proc_macro_attribute]
pub fn hew_export(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr as HewExportArgs);
    let func = parse_macro_input!(item as ItemFn);

    match hew_export_impl(args, func) {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn hew_export_impl(
    args: HewExportArgs,
    func: ItemFn,
) -> Result<proc_macro2::TokenStream, syn::Error> {
    // ------------------------------------------------------------------
    // 1. Validate extern "C" ABI
    // ------------------------------------------------------------------
    let is_extern_c = func
        .sig
        .abi
        .as_ref()
        .and_then(|a| a.name.as_ref())
        .is_some_and(|n| n.value() == "C");

    if !is_extern_c {
        return Err(syn::Error::new_spanned(
            &func.sig,
            "#[hew_export] can only be applied to `extern \"C\"` functions",
        ));
    }

    // ------------------------------------------------------------------
    // 2. Derive the Hew-level function name
    // ------------------------------------------------------------------
    let c_name = func.sig.ident.to_string();

    let hew_name = args.name.unwrap_or_else(|| {
        // Strip `hew_<last_module_segment>_` prefix if present, otherwise
        // strip just `hew_` prefix, otherwise use the fn name as-is.
        let last_segment = args.module.split("::").last().unwrap_or("").to_string();
        let long_prefix = format!("hew_{last_segment}_");
        let short_prefix = "hew_";
        if let Some(stripped) = c_name.strip_prefix(&*long_prefix) {
            stripped.to_string()
        } else if let Some(stripped) = c_name.strip_prefix(short_prefix) {
            stripped.to_string()
        } else {
            c_name.clone()
        }
    });

    let doc_str = args.doc.unwrap_or_default();

    // ------------------------------------------------------------------
    // 3. Build parameter metadata
    // ------------------------------------------------------------------
    let mut param_tokens: Vec<proc_macro2::TokenStream> = Vec::new();

    for input in &func.sig.inputs {
        let (param_name, param_ty) = match input {
            FnArg::Receiver(r) => {
                return Err(syn::Error::new_spanned(
                    r,
                    "#[hew_export] functions must not have a `self` receiver",
                ));
            }
            FnArg::Typed(PatType { pat, ty, .. }) => {
                let name = match pat.as_ref() {
                    Pat::Ident(pi) => pi.ident.to_string(),
                    other => {
                        return Err(syn::Error::new_spanned(
                            other,
                            "#[hew_export]: only simple identifier patterns \
                             are supported in parameters",
                        ))
                    }
                };
                let hew_ty = map_type(ty).ok_or_else(|| {
                    syn::Error::new_spanned(
                        ty,
                        "unsupported parameter type for #[hew_export]; \
                         supported types: *const c_char, *mut c_char, \
                         *const u8, *mut u8, i32, i64, f64, bool",
                    )
                })?;
                (name, hew_ty)
            }
        };

        param_tokens.push(quote! { (#param_name, #param_ty) });
    }

    // ------------------------------------------------------------------
    // 4. Map return type
    // ------------------------------------------------------------------
    let ret_hew = map_return_type(&func.sig.output)?;

    let return_type_expr = if let Some(t) = ret_hew {
        quote! { ::core::option::Option::Some(#t) }
    } else {
        quote! { ::core::option::Option::None }
    };

    // ------------------------------------------------------------------
    // 5. Build the companion metadata function identifier
    // ------------------------------------------------------------------
    let meta_fn_ident = Ident::new(&format!("__hew_export_meta_{c_name}"), Span::call_site());

    let module_str = &args.module;

    // ------------------------------------------------------------------
    // 6. Emit original function + companion metadata function
    // ------------------------------------------------------------------
    Ok(quote! {
        #func

        #[doc(hidden)]
        pub fn #meta_fn_ident() -> ::hew_export_types::ExportMeta {
            ::hew_export_types::ExportMeta {
                module: #module_str,
                hew_name: #hew_name,
                c_name: #c_name,
                params: ::std::vec![#(#param_tokens),*],
                return_type: #return_type_expr,
                doc: #doc_str,
            }
        }
    })
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    /// The type-mapping and name-derivation logic is tested here directly.
    /// Full macro expansion is verified by applying the attribute to a sample
    /// function and checking the generated metadata at runtime.
    #[test]
    fn type_mapping_c_char() {
        use syn::parse_str;
        let ty: syn::Type = parse_str("*const c_char").unwrap();
        assert_eq!(super::map_type(&ty), Some("string"));

        let ty: syn::Type = parse_str("*mut c_char").unwrap();
        assert_eq!(super::map_type(&ty), Some("string"));

        let ty: syn::Type = parse_str("*const u8").unwrap();
        assert_eq!(super::map_type(&ty), Some("bytes"));

        let ty: syn::Type = parse_str("*mut u8").unwrap();
        assert_eq!(super::map_type(&ty), Some("bytes"));
    }

    #[test]
    fn type_mapping_primitives() {
        use syn::parse_str;
        for (src, expected) in [
            ("i32", "i32"),
            ("i64", "i64"),
            ("f64", "f64"),
            ("bool", "bool"),
        ] {
            let ty: syn::Type = parse_str(src).unwrap();
            assert_eq!(super::map_type(&ty), Some(expected), "failed for {src}");
        }
    }

    #[test]
    fn type_mapping_unit() {
        use syn::parse_str;
        let ty: syn::Type = parse_str("()").unwrap();
        assert_eq!(super::map_type(&ty), Some("void"));
    }

    #[test]
    fn type_mapping_unknown_returns_none() {
        use syn::parse_str;
        let ty: syn::Type = parse_str("MyCustomType").unwrap();
        assert_eq!(super::map_type(&ty), None);
    }

    #[test]
    fn return_type_default_is_none() {
        use syn::ReturnType;
        assert_eq!(super::map_return_type(&ReturnType::Default).unwrap(), None);
    }

    #[test]
    fn hew_name_derivation() {
        let derive = |c_name: &str, module: &str| -> String {
            let last_segment = module.split("::").last().unwrap_or("").to_string();
            let long_prefix = format!("hew_{last_segment}_");
            let short_prefix = "hew_";
            if c_name.starts_with(&long_prefix) {
                c_name[long_prefix.len()..].to_string()
            } else if c_name.starts_with(short_prefix) {
                c_name[short_prefix.len()..].to_string()
            } else {
                c_name.to_string()
            }
        };

        assert_eq!(derive("hew_json_parse", "std::encoding::json"), "parse");
        assert_eq!(derive("hew_file_read", "std::file"), "read");
        // Falls back to stripping just "hew_" when module segment doesn't match
        assert_eq!(derive("hew_stdin_read_line", "std::io"), "stdin_read_line");
        // No known prefix: use as-is
        assert_eq!(derive("my_func", "std::misc"), "my_func");
    }
}
