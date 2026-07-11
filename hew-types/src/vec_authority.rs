//! Authoritative `Vec<T>` method and runtime-symbol resolution.
//!
//! The method surface and symbol templates come from the real
//! `impl<T> Vec<T>` declaration in `std/builtins.hew`. This module adds the
//! compiler-owned element ABI classification and applies the ownership/layout
//! rules needed to choose one concrete runtime export. Both the checker and MIR
//! call [`resolve_runtime_symbol`]; neither keeps a parallel method/symbol table.

use std::collections::{HashMap, HashSet};
use std::sync::OnceLock;

use hew_parser::ast::{Item, TypeExpr};

use crate::check::dispatch::{RuntimeAbi, VecMethod};
use crate::check::TypeDef;
use crate::extern_symbol::{ExternSymbolTemplate, TemplateSegment};
use crate::ty::Ty;

const BUILTINS_HEW_SOURCE: &str = include_str!("../../std/builtins.hew");
const RUNTIME_SYMBOL_CATALOG: &str = include_str!("../../scripts/jit-symbol-classification.toml");

/// The runtime ABI class of one `Vec<T>` element.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum VecElementToken {
    Bool,
    I8,
    U8,
    I16,
    U16,
    I32,
    I64,
    F32,
    F64,
    Str,
    Ptr,
    Layout,
}

impl VecElementToken {
    #[must_use]
    pub const fn canonical_token(self) -> &'static str {
        match self {
            Self::Bool => "bool",
            Self::I8 => "i8",
            Self::U8 => "u8",
            Self::I16 => "i16",
            Self::U16 => "u16",
            Self::I32 => "i32",
            Self::I64 => "i64",
            Self::F32 => "f32",
            Self::F64 => "f64",
            Self::Str => "str",
            Self::Ptr => "ptr",
            Self::Layout => "layout",
        }
    }
}

/// Source-derived metadata for one runtime-backed Vec method.
#[derive(Debug, Clone)]
pub struct VecMethodSpec {
    pub method: VecMethod,
    pub name: String,
    pub template: ExternSymbolTemplate,
}

impl VecMethod {
    pub const ALL: [Self; 12] = [
        Self::Push,
        Self::Pop,
        Self::Len,
        Self::Get,
        Self::Set,
        Self::Remove,
        Self::Contains,
        Self::IsEmpty,
        Self::Clear,
        Self::Clone,
        Self::Append,
        Self::Join,
    ];

    #[must_use]
    pub const fn name(self) -> &'static str {
        match self {
            Self::Push => "push",
            Self::Pop => "pop",
            Self::Len => "len",
            Self::Get => "get",
            Self::Set => "set",
            Self::Remove => "remove",
            Self::Contains => "contains",
            Self::IsEmpty => "is_empty",
            Self::Clear => "clear",
            Self::Clone => "clone",
            Self::Append => "append",
            Self::Join => "join",
        }
    }

    #[must_use]
    pub fn from_name(name: &str) -> Option<Self> {
        Some(match name {
            "push" => Self::Push,
            "pop" => Self::Pop,
            "len" => Self::Len,
            "get" => Self::Get,
            "set" => Self::Set,
            "remove" => Self::Remove,
            "contains" => Self::Contains,
            "is_empty" => Self::IsEmpty,
            "clear" => Self::Clear,
            "clone" => Self::Clone,
            "append" => Self::Append,
            "join" => Self::Join,
            _ => return None,
        })
    }

    #[must_use]
    pub const fn runtime_abi(self) -> RuntimeAbi {
        match self {
            Self::Push | Self::Pop | Self::Set | Self::Remove | Self::Clear | Self::Append => {
                RuntimeAbi::ByRefMut
            }
            Self::Len | Self::Get | Self::Contains | Self::IsEmpty | Self::Clone | Self::Join => {
                RuntimeAbi::ByRef
            }
        }
    }

    #[must_use]
    pub const fn can_defer_element_resolution(self) -> bool {
        matches!(
            self,
            Self::Push
                | Self::Pop
                | Self::Get
                | Self::Set
                | Self::Remove
                | Self::Contains
                | Self::Clone
        )
    }
}

/// Classify a concrete Vec element using the same typed verdict for
/// constructor selection, checker method resolution, and MIR monomorphisation.
#[must_use]
#[allow(
    clippy::implicit_hasher,
    reason = "uses the checker's concrete TypeDef table shape"
)]
#[allow(
    clippy::match_same_arms,
    reason = "tuple and nested-collection ownership cases stay explicit"
)]
pub fn classify_element<S: std::hash::BuildHasher>(
    ty: &Ty,
    type_defs: &HashMap<String, TypeDef, S>,
) -> Option<VecElementToken> {
    Some(match ty {
        Ty::Bool => VecElementToken::Bool,
        Ty::I8 => VecElementToken::I8,
        Ty::U8 => VecElementToken::U8,
        Ty::I16 => VecElementToken::I16,
        Ty::U16 => VecElementToken::U16,
        Ty::Char | Ty::I32 | Ty::U32 => VecElementToken::I32,
        Ty::I64 | Ty::U64 | Ty::Isize | Ty::Usize | Ty::Duration => VecElementToken::I64,
        Ty::Named {
            builtin: Some(crate::builtin_type::BuiltinType::Instant),
            ..
        } => VecElementToken::I64,
        Ty::F32 => VecElementToken::F32,
        Ty::F64 => VecElementToken::F64,
        Ty::String => VecElementToken::Str,
        Ty::Tuple(_) => VecElementToken::Layout,
        Ty::Named {
            builtin: Some(b), ..
        } if b.lowers_as_pointer_vec_element() => VecElementToken::Ptr,
        Ty::Named {
            builtin: Some(crate::builtin_type::BuiltinType::Vec),
            args,
            ..
        } if args
            .first()
            .is_some_and(|e| matches!(e, Ty::Function { .. } | Ty::Closure { .. })) =>
        {
            VecElementToken::Ptr
        }
        Ty::Named {
            builtin:
                Some(
                    crate::builtin_type::BuiltinType::Vec
                    | crate::builtin_type::BuiltinType::HashMap
                    | crate::builtin_type::BuiltinType::HashSet,
                ),
            ..
        } => VecElementToken::Layout,
        Ty::Function { .. } | Ty::Closure { .. } => VecElementToken::Ptr,
        Ty::Named { name, .. } => match type_defs.get(name) {
            Some(td) if td.is_indirect => VecElementToken::Ptr,
            Some(_) => VecElementToken::Layout,
            None => return None,
        },
        _ => return None,
    })
}

/// Element facts needed by the single runtime-symbol resolver.
#[derive(Debug, Clone, Copy)]
#[allow(
    clippy::struct_excessive_bools,
    reason = "orthogonal ABI, ownership, layout, function, and abstraction facts"
)]
pub struct VecElementProfile {
    pub abi: Option<VecElementToken>,
    pub is_owned: bool,
    pub is_copy_layout: bool,
    pub is_function_like: bool,
    pub is_abstract: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VecResolutionContext {
    CheckerConcrete,
    MonomorphizedPlaceholder,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VecUnsupported {
    FunctionGet,
    FunctionSharedCopy,
    Layout {
        expected_symbol: String,
        bitcopy_supported: bool,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VecSymbolResolution {
    Resolved(String),
    Deferred,
    Unavailable,
    Unsupported(VecUnsupported),
}

#[must_use]
pub fn method_specs() -> &'static [VecMethodSpec] {
    static SPECS: OnceLock<Vec<VecMethodSpec>> = OnceLock::new();
    SPECS.get_or_init(load_method_specs)
}

#[must_use]
///
/// # Panics
///
/// Panics if `std/builtins.hew` does not declare exactly one annotated method
/// for the requested runtime-backed Vec operation.
pub fn method_spec(method: VecMethod) -> &'static VecMethodSpec {
    method_specs()
        .iter()
        .find(|spec| spec.method == method)
        .unwrap_or_else(|| {
            panic!(
                "std/builtins.hew is missing Vec::{} #[extern_symbol] metadata",
                method.name()
            )
        })
}

fn load_method_specs() -> Vec<VecMethodSpec> {
    let parsed = hew_parser::parse(BUILTINS_HEW_SOURCE);
    assert!(
        parsed.errors.is_empty(),
        "std/builtins.hew failed to parse while loading Vec authority: {:?}",
        parsed.errors
    );

    let mut specs = Vec::new();
    for (item, _) in parsed.program.items {
        let Item::Impl(impl_decl) = item else {
            continue;
        };
        if impl_decl.trait_bound.is_some() {
            continue;
        }
        let TypeExpr::Named { name, .. } = &impl_decl.target_type.0 else {
            continue;
        };
        if name != "Vec" {
            continue;
        }
        for method in impl_decl.methods {
            let Some(method_kind) = VecMethod::from_name(&method.name) else {
                continue;
            };
            let attr = method
                .attributes
                .iter()
                .find(|attr| attr.name == "extern_symbol")
                .unwrap_or_else(|| {
                    panic!(
                        "std/builtins.hew Vec::{} is missing #[extern_symbol]",
                        method.name
                    )
                });
            assert_eq!(
                attr.args.len(),
                1,
                "Vec::{} #[extern_symbol] must have one template",
                method.name
            );
            let raw = attr.args[0].as_str();
            let template = ExternSymbolTemplate::parse(raw).unwrap_or_else(|error| {
                panic!(
                    "invalid Vec::{} #[extern_symbol] template {raw:?}: {error:?}",
                    method.name
                )
            });
            specs.push(VecMethodSpec {
                method: method_kind,
                name: method.name,
                template,
            });
        }
    }

    for method in VecMethod::ALL {
        assert_eq!(
            specs.iter().filter(|spec| spec.method == method).count(),
            1,
            "std/builtins.hew must declare exactly one runtime-backed Vec::{} method",
            method.name()
        );
    }
    specs
}

fn expand_template(template: &ExternSymbolTemplate, token: &str) -> String {
    let mut symbol = String::with_capacity(template.raw.len() + token.len());
    for segment in &template.segments {
        match segment {
            TemplateSegment::Literal(literal) => symbol.push_str(literal),
            TemplateSegment::Placeholder(_) => symbol.push_str(token),
        }
    }
    symbol
}

fn runtime_symbols() -> &'static HashSet<&'static str> {
    static SYMBOLS: OnceLock<HashSet<&'static str>> = OnceLock::new();
    SYMBOLS.get_or_init(|| {
        let mut symbols = HashSet::new();
        for line in RUNTIME_SYMBOL_CATALOG.lines() {
            let line = line.trim();
            if let Some(rest) = line.strip_prefix('"') {
                if let Some(symbol) = rest.split('"').next() {
                    symbols.insert(symbol);
                }
            } else if let Some(rest) = line.strip_prefix("symbol = \"") {
                if let Some(symbol) = rest.split('"').next() {
                    symbols.insert(symbol);
                }
            }
        }
        symbols
    })
}

fn available(symbol: String) -> VecSymbolResolution {
    if runtime_symbols().contains(symbol.as_str()) {
        VecSymbolResolution::Resolved(symbol)
    } else {
        VecSymbolResolution::Unavailable
    }
}

/// Resolve one Vec method to its concrete runtime symbol.
///
/// `Deferred` is reserved for an abstract type parameter whose method can be
/// re-resolved after monomorphisation. `Unavailable` means the template expands
/// to an ABI family the runtime does not export. `Unsupported` carries the
/// precise ownership/layout reason the checker turns into a diagnostic.
#[must_use]
pub fn resolve_runtime_symbol(
    method: VecMethod,
    profile: VecElementProfile,
    context: VecResolutionContext,
) -> VecSymbolResolution {
    let spec = method_spec(method);

    if profile.is_function_like {
        if method == VecMethod::Get {
            return VecSymbolResolution::Unsupported(VecUnsupported::FunctionGet);
        }
        if matches!(method, VecMethod::Clone | VecMethod::Append) {
            return VecSymbolResolution::Unsupported(VecUnsupported::FunctionSharedCopy);
        }
    }

    if profile.is_abstract && method.can_defer_element_resolution() {
        return VecSymbolResolution::Deferred;
    }

    if matches!(
        method,
        VecMethod::Len | VecMethod::IsEmpty | VecMethod::Join
    ) {
        return available(spec.template.raw.clone());
    }

    if matches!(
        method,
        VecMethod::Clear | VecMethod::Clone | VecMethod::Append
    ) {
        if profile.is_owned && method == VecMethod::Clone {
            return available(format!("{}_owned", spec.template.raw));
        }
        if profile.abi == Some(VecElementToken::Layout) {
            let expected_symbol = format!("{}_layout", spec.template.raw);
            if method == VecMethod::Clone && profile.is_copy_layout {
                if context == VecResolutionContext::MonomorphizedPlaceholder {
                    return VecSymbolResolution::Unavailable;
                }
                return available(expected_symbol);
            }
            return VecSymbolResolution::Unsupported(VecUnsupported::Layout {
                expected_symbol,
                bitcopy_supported: method == VecMethod::Clone,
            });
        }
        return available(spec.template.raw.clone());
    }

    if profile.is_owned {
        let owned = expand_template(&spec.template, "owned");
        if method == VecMethod::Get {
            return available("hew_vec_get_clone".to_string());
        }
        return available(owned);
    }

    let Some(abi) = profile.abi else {
        return VecSymbolResolution::Unavailable;
    };

    if abi == VecElementToken::Layout {
        let expected_symbol = expand_template(&spec.template, "layout");
        if method == VecMethod::Contains && profile.is_copy_layout {
            return available("hew_vec_contains_thunk".to_string());
        }
        let bitcopy_supported = matches!(
            method,
            VecMethod::Push | VecMethod::Get | VecMethod::Set | VecMethod::Pop | VecMethod::Remove
        );
        if !bitcopy_supported || !profile.is_copy_layout {
            return VecSymbolResolution::Unsupported(VecUnsupported::Layout {
                expected_symbol,
                bitcopy_supported,
            });
        }
    }

    let candidate = expand_template(&spec.template, abi.canonical_token());
    if method == VecMethod::Get {
        if runtime_symbols().contains(candidate.as_str()) {
            return available("hew_vec_get_clone".to_string());
        }
        return VecSymbolResolution::Unavailable;
    }
    available(candidate)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn source_declares_each_runtime_backed_vec_method_once() {
        assert_eq!(method_specs().len(), VecMethod::ALL.len());
    }

    #[test]
    fn runtime_catalog_filters_unsupported_contains_families() {
        let plain = |abi| VecElementProfile {
            abi: Some(abi),
            is_owned: false,
            is_copy_layout: abi == VecElementToken::Layout,
            is_function_like: false,
            is_abstract: false,
        };
        assert_eq!(
            resolve_runtime_symbol(
                VecMethod::Contains,
                plain(VecElementToken::Bool),
                VecResolutionContext::CheckerConcrete
            ),
            VecSymbolResolution::Unavailable
        );
        assert_eq!(
            resolve_runtime_symbol(
                VecMethod::Contains,
                plain(VecElementToken::Ptr),
                VecResolutionContext::CheckerConcrete
            ),
            VecSymbolResolution::Unavailable
        );
        assert_eq!(
            resolve_runtime_symbol(
                VecMethod::Contains,
                plain(VecElementToken::I64),
                VecResolutionContext::CheckerConcrete
            ),
            VecSymbolResolution::Resolved("hew_vec_contains_i64".to_string())
        );
    }

    #[test]
    fn source_templates_drive_scalar_symbol_matrix() {
        let profile = |abi| VecElementProfile {
            abi: Some(abi),
            is_owned: false,
            is_copy_layout: false,
            is_function_like: false,
            is_abstract: false,
        };
        for (method, abi, expected) in [
            (VecMethod::Push, VecElementToken::Bool, "hew_vec_push_bool"),
            (VecMethod::Push, VecElementToken::I8, "hew_vec_push_i8"),
            (VecMethod::Push, VecElementToken::U16, "hew_vec_push_u16"),
            (VecMethod::Push, VecElementToken::F32, "hew_vec_push_f32"),
            (VecMethod::Pop, VecElementToken::Str, "hew_vec_pop_str"),
            (VecMethod::Set, VecElementToken::Ptr, "hew_vec_set_ptr"),
            (
                VecMethod::Remove,
                VecElementToken::I64,
                "hew_vec_remove_at_i64",
            ),
        ] {
            assert_eq!(
                resolve_runtime_symbol(method, profile(abi), VecResolutionContext::CheckerConcrete),
                VecSymbolResolution::Resolved(expected.to_string()),
                "{} / {abi:?}",
                method.name()
            );
        }
    }

    #[test]
    fn layout_owned_and_get_overrides_share_one_resolver() {
        let copy_layout = VecElementProfile {
            abi: Some(VecElementToken::Layout),
            is_owned: false,
            is_copy_layout: true,
            is_function_like: false,
            is_abstract: false,
        };
        assert_eq!(
            resolve_runtime_symbol(
                VecMethod::Push,
                copy_layout,
                VecResolutionContext::CheckerConcrete
            ),
            VecSymbolResolution::Resolved("hew_vec_push_layout".to_string())
        );
        assert_eq!(
            resolve_runtime_symbol(
                VecMethod::Contains,
                copy_layout,
                VecResolutionContext::CheckerConcrete
            ),
            VecSymbolResolution::Resolved("hew_vec_contains_thunk".to_string())
        );
        assert_eq!(
            resolve_runtime_symbol(
                VecMethod::Get,
                copy_layout,
                VecResolutionContext::CheckerConcrete
            ),
            VecSymbolResolution::Resolved("hew_vec_get_clone".to_string())
        );

        let owned = VecElementProfile {
            abi: None,
            is_owned: true,
            is_copy_layout: false,
            is_function_like: false,
            is_abstract: false,
        };
        for (method, expected) in [
            (VecMethod::Push, "hew_vec_push_owned"),
            (VecMethod::Pop, "hew_vec_pop_owned"),
            (VecMethod::Set, "hew_vec_set_owned"),
            (VecMethod::Remove, "hew_vec_remove_at_owned"),
            (VecMethod::Contains, "hew_vec_contains_owned"),
            (VecMethod::Clone, "hew_vec_clone_owned"),
            (VecMethod::Get, "hew_vec_get_clone"),
        ] {
            assert_eq!(
                resolve_runtime_symbol(
                    method,
                    owned,
                    VecResolutionContext::MonomorphizedPlaceholder
                ),
                VecSymbolResolution::Resolved(expected.to_string())
            );
        }
    }

    #[test]
    fn abstract_element_methods_defer_to_monomorphisation() {
        let abstract_element = VecElementProfile {
            abi: None,
            is_owned: false,
            is_copy_layout: false,
            is_function_like: false,
            is_abstract: true,
        };
        for method in [
            VecMethod::Push,
            VecMethod::Pop,
            VecMethod::Get,
            VecMethod::Set,
            VecMethod::Remove,
            VecMethod::Contains,
            VecMethod::Clone,
        ] {
            assert_eq!(
                resolve_runtime_symbol(
                    method,
                    abstract_element,
                    VecResolutionContext::CheckerConcrete
                ),
                VecSymbolResolution::Deferred
            );
        }
    }
}
