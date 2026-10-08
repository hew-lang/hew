#![allow(
    dead_code,
    reason = "shared by build.rs and stdlib_authority tests; each compilation consumes a different subset"
)]

use std::collections::BTreeMap;

use hew_parser::ast::{ExternBlock, ImplDecl, Item, Program, TypeBodyItem, TypeDeclKind, TypeExpr};

#[derive(Clone, Copy)]
pub struct AuthoritySource<'a> {
    pub module: &'a str,
    pub path: &'a str,
    pub source: &'a str,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DerivedBuiltinEnum {
    pub owner: String,
    pub name: &'static str,
    pub canonical_name: String,
    pub variants: Vec<DerivedBuiltinVariant>,
    pub suppress_from_sandbox_emit: bool,
}

/// One variant in declaration order with its payload, spelled as the
/// generated catalog's `BuiltinEnumPayload` constructors.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DerivedBuiltinVariant {
    pub name: String,
    pub payload: Vec<&'static str>,
}

struct BuiltinEnumAbi {
    module: &'static str,
    name: &'static str,
    variant_count: usize,
    order_fingerprint: u64,
    suppress_from_sandbox_emit: bool,
}

const BUILTIN_ENUM_ABI: &[BuiltinEnumAbi] = &[
    BuiltinEnumAbi {
        module: "std.builtins",
        name: "NodeError",
        variant_count: 1,
        order_fingerprint: 0xb89f_e6dc_5d57_7668,
        suppress_from_sandbox_emit: true,
    },
    BuiltinEnumAbi {
        module: "std.builtins",
        name: "LookupError",
        variant_count: 7,
        order_fingerprint: 0x66e4_5f01_5f05_d553,
        suppress_from_sandbox_emit: true,
    },
    BuiltinEnumAbi {
        module: "std.builtins",
        name: "SendError",
        variant_count: 11,
        order_fingerprint: 0x6bb1_0898_daed_4845,
        suppress_from_sandbox_emit: false,
    },
    BuiltinEnumAbi {
        module: "std.builtins",
        name: "LinkError",
        variant_count: 1,
        order_fingerprint: 0xc3cb_f26b_7368_2d2e,
        suppress_from_sandbox_emit: true,
    },
    BuiltinEnumAbi {
        module: "std.failure",
        name: "CrashAction",
        variant_count: 3,
        order_fingerprint: 0xce24_b136_af5b_7fd7,
        suppress_from_sandbox_emit: true,
    },
    BuiltinEnumAbi {
        module: "std.failure",
        name: "CrashKind",
        variant_count: 3,
        order_fingerprint: 0x8201_2311_7fda_bb95,
        suppress_from_sandbox_emit: true,
    },
];

pub fn derive_builtin_enums(
    sources: &[AuthoritySource<'_>],
) -> Result<Vec<DerivedBuiltinEnum>, String> {
    let mut declarations = BTreeMap::new();
    for source in sources {
        let parsed = hew_parser::parse(source.source);
        if !parsed.errors.is_empty() {
            let diagnostics = parsed
                .errors
                .into_iter()
                .map(|error| error.message)
                .collect::<Vec<_>>()
                .join("; ");
            return Err(format!("{} failed to parse: {diagnostics}", source.path));
        }
        for (item, _) in parsed.program.items {
            let Item::TypeDecl(decl) = item else {
                continue;
            };
            if decl.kind != TypeDeclKind::Enum {
                continue;
            }
            if !BUILTIN_ENUM_ABI
                .iter()
                .any(|abi| abi.module == source.module && abi.name == decl.name.name.as_str())
            {
                continue;
            }
            let mut variants = Vec::new();
            for body_item in decl.body {
                let TypeBodyItem::Variant(variant) = body_item else {
                    continue;
                };
                variants.push(derive_variant(
                    source.module,
                    decl.name.name.as_str(),
                    &variant,
                )?);
            }
            declarations.insert(
                (source.module.to_string(), decl.name.to_string()),
                (source.module.to_string(), variants),
            );
        }
    }

    BUILTIN_ENUM_ABI
        .iter()
        .map(|abi| {
            let key = (abi.module.to_string(), abi.name.to_string());
            let (owner, variants) = declarations.get(&key).ok_or_else(|| {
                format!(
                    "missing ABI-bearing enum {}::{} in stdlib authority sources",
                    abi.module, abi.name
                )
            })?;
            let names = variants
                .iter()
                .map(|variant| variant.name.clone())
                .collect::<Vec<_>>();
            let actual_fingerprint = enum_order_fingerprint(&names);
            if variants.len() != abi.variant_count
                || actual_fingerprint != abi.order_fingerprint
            {
                return Err(format!(
                    "discriminant ABI drift for {}::{}: declaration-order fingerprint changed \
                     (expected {} variants / {:#018x}, found {} / {:#018x}); enum variant order is ABI",
                    abi.module,
                    abi.name,
                    abi.variant_count,
                    abi.order_fingerprint,
                    variants.len(),
                    actual_fingerprint
                ));
            }
            Ok(DerivedBuiltinEnum {
                owner: owner.clone(),
                name: abi.name,
                canonical_name: format!("{owner}.{}", abi.name),
                variants: variants.clone(),
                suppress_from_sandbox_emit: abi.suppress_from_sandbox_emit,
            })
        })
        .collect()
}

pub fn derive_monitor_ref_projection(
    builtins: AuthoritySource<'_>,
    link_monitor: AuthoritySource<'_>,
) -> Result<String, String> {
    let mut projected_items = Vec::new();
    for source in [builtins, link_monitor] {
        let parsed = hew_parser::parse(source.source);
        if !parsed.errors.is_empty() {
            let diagnostics = parsed
                .errors
                .into_iter()
                .map(|error| error.message)
                .collect::<Vec<_>>()
                .join("; ");
            return Err(format!("{} failed to parse: {diagnostics}", source.path));
        }
        for (item, span) in parsed.program.items {
            let projected = match item {
                Item::TypeDecl(decl)
                    if source.module == "std.builtins"
                        && decl.name.name.as_str() == "LinkError" =>
                {
                    Some(Item::TypeDecl(decl))
                }
                Item::TypeDecl(decl)
                    if source.module == "std.link_monitor"
                        && matches!(
                            decl.name.name.as_str(),
                            "PartitionPolicy"
                                | "MonitorId"
                                | "DownTarget"
                                | "DownReason"
                                | "DownNotification"
                                | "MonitorRef"
                        ) =>
                {
                    Some(Item::TypeDecl(decl))
                }
                Item::Impl(mut decl)
                    if source.module == "std.link_monitor"
                        && impl_target_name(&decl) == Some("MonitorRef") =>
                {
                    decl.methods
                        .retain(|method| matches!(method.name.name.as_str(), "close" | "id"));
                    (!decl.methods.is_empty()).then_some(Item::Impl(decl))
                }
                Item::ExternBlock(mut block) if source.module == "std.link_monitor" => {
                    block
                        .functions
                        .retain(|function| function.name.name.as_str() == "hew_actor_demonitor");
                    (!block.functions.is_empty()).then_some(Item::ExternBlock(block))
                }
                _ => None,
            };
            if let Some(item) = projected {
                projected_items.push((item, span));
            }
        }
    }

    let projected_declarations = projected_items
        .iter()
        .map(|(item, _)| item.clone())
        .collect::<Vec<_>>();
    ensure_monitor_projection_complete(&projected_declarations)?;
    Ok(hew_parser::fmt::format_program(&Program {
        items: projected_items,
        module_doc: None,
        module_graph: None,
    }))
}

fn ensure_monitor_projection_complete(items: &[Item]) -> Result<(), String> {
    for expected in [
        "LinkError",
        "PartitionPolicy",
        "MonitorId",
        "DownTarget",
        "DownReason",
        "DownNotification",
        "MonitorRef",
    ] {
        if !items
            .iter()
            .any(|item| matches!(item, Item::TypeDecl(decl) if decl.name.name.as_str() == expected))
        {
            return Err(format!(
                "monitor prelude projection is missing owning declaration `{expected}`"
            ));
        }
    }
    if !items.iter().any(|item| {
        matches!(
            item,
            Item::Impl(decl)
                if impl_target_name(decl) == Some("MonitorRef")
                    && decl.methods.iter().any(|method| method.name.name.as_str() == "close")
        )
    }) {
        return Err("monitor prelude projection is missing `MonitorRef::close`".to_string());
    }
    if !items.iter().any(|item| {
        matches!(
            item,
            Item::ExternBlock(ExternBlock { functions, .. })
                if functions.iter().any(|function| function.name.name.as_str() == "hew_actor_demonitor")
        )
    }) {
        return Err(
            "monitor prelude projection is missing `hew_actor_demonitor` declaration".to_string(),
        );
    }
    Ok(())
}

fn impl_target_name(decl: &ImplDecl) -> Option<&str> {
    match &decl.target_type.0 {
        TypeExpr::Named { path, .. } => path.as_single().map(|ident| ident.name.as_str()),
        _ => None,
    }
}

/// One builtin enum variant with its payload. The layout catalog describes
/// unit variants and tuple variants of `i64` fields.
fn derive_variant(
    module: &str,
    enum_name: &str,
    variant: &hew_parser::ast::VariantDecl,
) -> Result<DerivedBuiltinVariant, String> {
    let payload = match &variant.kind {
        hew_parser::ast::VariantKind::Unit => Vec::new(),
        hew_parser::ast::VariantKind::Tuple(fields) => fields
            .iter()
            .map(|(field, _)| builtin_payload(field))
            .collect::<Option<Vec<_>>>()
            .ok_or_else(|| {
                format!(
                    "{module}::{enum_name} variant `{}` carries a payload the builtin enum \
                     layout catalog cannot describe; only `i64` fields are supported",
                    variant.name
                )
            })?,
        hew_parser::ast::VariantKind::Struct(_) => {
            return Err(format!(
                "{module}::{enum_name} has record variant `{}`; the builtin enum layout \
                 catalog supports unit and `i64` tuple variants",
                variant.name
            ));
        }
    };
    Ok(DerivedBuiltinVariant {
        name: variant.name.to_string(),
        payload,
    })
}

/// The generated `BuiltinEnumPayload` constructor for one payload field.
fn builtin_payload(field: &TypeExpr) -> Option<&'static str> {
    match field {
        TypeExpr::Named {
            path,
            type_args: None,
        } if path.as_single().map(|ident| ident.name.as_str()) == Some("i64") => Some("I64"),
        _ => None,
    }
}

fn enum_order_fingerprint(variants: &[String]) -> u64 {
    let mut hash = 0xcbf2_9ce4_8422_2325_u64;
    for variant in variants {
        for byte in variant.bytes().chain(std::iter::once(0xff)) {
            hash ^= u64::from(byte);
            hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
        }
    }
    hash
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reordered_abi_enum_is_rejected() {
        let reordered_failure = include_str!("../../../std/failure.hew").replacen(
            "    Restart;\n    Escalate;",
            "    Escalate;\n    Restart;",
            1,
        );
        assert_ne!(reordered_failure, include_str!("../../../std/failure.hew"));
        let sources = [
            AuthoritySource {
                module: "std.builtins",
                path: "std/builtins.hew",
                source: include_str!("../../../std/builtins.hew"),
            },
            AuthoritySource {
                module: "std.failure",
                path: "scratch/failure.hew",
                source: &reordered_failure,
            },
            AuthoritySource {
                module: "std.link_monitor",
                path: "std/link_monitor.hew",
                source: include_str!("../../../std/link_monitor.hew"),
            },
        ];

        let error = derive_builtin_enums(&sources)
            .expect_err("reordering a .hew enum must fail the discriminant ABI derivation");
        assert!(
            error.contains("discriminant ABI drift for std.failure::CrashAction"),
            "unexpected error: {error}"
        );
    }

    #[test]
    fn derived_enums_retain_exact_source_owner_identity() {
        let sources = [
            AuthoritySource {
                module: "std.builtins",
                path: "std/builtins.hew",
                source: include_str!("../../../std/builtins.hew"),
            },
            AuthoritySource {
                module: "std.failure",
                path: "std/failure.hew",
                source: include_str!("../../../std/failure.hew"),
            },
            AuthoritySource {
                module: "std.link_monitor",
                path: "std/link_monitor.hew",
                source: include_str!("../../../std/link_monitor.hew"),
            },
        ];

        let enums = derive_builtin_enums(&sources).expect("derive builtin enum facts");
        let identities: Vec<_> = enums
            .iter()
            .map(|fact| (fact.owner.as_str(), fact.name, fact.canonical_name.as_str()))
            .collect();
        for expected in [
            ("std.builtins", "LookupError", "std.builtins.LookupError"),
            ("std.builtins", "LinkError", "std.builtins.LinkError"),
            ("std.failure", "CrashAction", "std.failure.CrashAction"),
            ("std.failure", "CrashKind", "std.failure.CrashKind"),
        ] {
            assert!(
                identities.contains(&expected),
                "missing exact fact {expected:?}"
            );
        }
    }
}
