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
    pub name: &'static str,
    pub variants: Vec<String>,
    pub suppress_from_sandbox_emit: bool,
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
        module: "builtins",
        name: "LookupError",
        variant_count: 8,
        order_fingerprint: 0x7ab9_6324_a0dd_7d1f,
        suppress_from_sandbox_emit: true,
    },
    BuiltinEnumAbi {
        module: "builtins",
        name: "SendError",
        variant_count: 10,
        order_fingerprint: 0xc506_1bd0_75d7_e256,
        suppress_from_sandbox_emit: false,
    },
    BuiltinEnumAbi {
        module: "builtins",
        name: "AskError",
        variant_count: 22,
        order_fingerprint: 0xb130_e184_57d4_ff62,
        suppress_from_sandbox_emit: false,
    },
    BuiltinEnumAbi {
        module: "builtins",
        name: "TimeoutError",
        variant_count: 1,
        order_fingerprint: 0xe9ae_0b1c_348b_0715,
        suppress_from_sandbox_emit: true,
    },
    BuiltinEnumAbi {
        module: "builtins",
        name: "LinkError",
        variant_count: 10,
        order_fingerprint: 0x3a85_b1f1_0849_9938,
        suppress_from_sandbox_emit: true,
    },
    BuiltinEnumAbi {
        module: "failure",
        name: "CrashAction",
        variant_count: 3,
        order_fingerprint: 0x61cb_3d18_fe7b_947f,
        suppress_from_sandbox_emit: true,
    },
    BuiltinEnumAbi {
        module: "failure",
        name: "CrashKind",
        variant_count: 3,
        order_fingerprint: 0xe445_0b1a_10e5_0b27,
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
            let mut variants = Vec::new();
            for body_item in decl.body {
                let TypeBodyItem::Variant(variant) = body_item else {
                    continue;
                };
                if !matches!(variant.kind, hew_parser::ast::VariantKind::Unit) {
                    return Err(format!(
                        "{}::{name} has payloaded variant `{}`; the builtin enum layout catalog only supports unit variants",
                        source.module,
                        variant.name,
                        name = decl.name
                    ));
                }
                variants.push(variant.name);
            }
            declarations.insert((source.module.to_string(), decl.name), variants);
        }
    }

    BUILTIN_ENUM_ABI
        .iter()
        .map(|abi| {
            let key = (abi.module.to_string(), abi.name.to_string());
            let variants = declarations.get(&key).ok_or_else(|| {
                format!(
                    "missing ABI-bearing enum {}::{} in stdlib authority sources",
                    abi.module, abi.name
                )
            })?;
            let actual_fingerprint = enum_order_fingerprint(variants);
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
                name: abi.name,
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
                Item::TypeDecl(decl) if source.module == "builtins" && decl.name == "LinkError" => {
                    Some(Item::TypeDecl(decl))
                }
                Item::TypeDecl(decl)
                    if source.module == "link_monitor"
                        && matches!(
                            decl.name.as_str(),
                            "MonitorError"
                                | "PartitionPolicy"
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
                    if source.module == "link_monitor"
                        && impl_target_name(&decl) == Some("MonitorRef") =>
                {
                    decl.methods
                        .retain(|method| method.name == "close" || method.name == "id");
                    (!decl.methods.is_empty()).then_some(Item::Impl(decl))
                }
                Item::ExternBlock(mut block) if source.module == "link_monitor" => {
                    block
                        .functions
                        .retain(|function| function.name == "hew_actor_demonitor");
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
        "MonitorError",
        "PartitionPolicy",
        "MonitorId",
        "DownTarget",
        "DownReason",
        "DownNotification",
        "MonitorRef",
    ] {
        if !items
            .iter()
            .any(|item| matches!(item, Item::TypeDecl(decl) if decl.name == expected))
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
                    && decl.methods.iter().any(|method| method.name == "close")
        )
    }) {
        return Err("monitor prelude projection is missing `MonitorRef::close`".to_string());
    }
    if !items.iter().any(|item| {
        matches!(
            item,
            Item::ExternBlock(ExternBlock { functions, .. })
                if functions.iter().any(|function| function.name == "hew_actor_demonitor")
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
        TypeExpr::Named { name, .. } => Some(name),
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
        let sources = [
            AuthoritySource {
                module: "builtins",
                path: "std/builtins.hew",
                source: include_str!("../../../std/builtins.hew"),
            },
            AuthoritySource {
                module: "failure",
                path: "scratch/failure.hew",
                source: &reordered_failure,
            },
        ];

        let error = derive_builtin_enums(&sources)
            .expect_err("reordering a .hew enum must fail the discriminant ABI derivation");
        assert!(
            error.contains("discriminant ABI drift for failure::CrashAction"),
            "unexpected error: {error}"
        );
    }
}
