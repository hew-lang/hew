use std::collections::HashMap;

use hew_analysis::calls::{
    collect_calls_in_block, collect_calls_in_item, collect_calls_in_named_body,
};
use hew_parser::ast::{Item, TypeDeclKind};
use hew_parser::ParseResult;
use tower_lsp::lsp_types::{
    CallHierarchyIncomingCall, CallHierarchyItem, CallHierarchyOutgoingCall, Range, SymbolKind,
    TypeHierarchyItem, Url,
};

use super::span_to_range;

// ── Type hierarchy helpers ──────────────────────────────────────────

pub(super) fn find_type_hierarchy_item(
    uri: &Url,
    source: &str,
    lo: &[usize],
    parse_result: &ParseResult,
    name: &str,
) -> Option<TypeHierarchyItem> {
    for (item, span) in &parse_result.program.items {
        let (item_name, kind) = match item {
            Item::TypeDecl(td) => {
                let kind = match td.kind {
                    TypeDeclKind::Struct => SymbolKind::STRUCT,
                    TypeDeclKind::Enum => SymbolKind::ENUM,
                };
                (td.name.as_str(), kind)
            }
            Item::Actor(a) => (a.name.as_str(), SymbolKind::CLASS),
            Item::Trait(t) => (t.name.as_str(), SymbolKind::INTERFACE),
            _ => continue,
        };
        if item_name == name {
            let range = span_to_range(source, lo, span);
            return Some(TypeHierarchyItem {
                name: item_name.to_string(),
                kind,
                tags: None,
                detail: None,
                uri: uri.clone(),
                range,
                selection_range: range,
                data: None,
            });
        }
    }
    None
}

pub(super) fn collect_supertypes(
    uri: &Url,
    name: &str,
    source: &str,
    lo: &[usize],
    parse_result: &ParseResult,
) -> Vec<TypeHierarchyItem> {
    let mut supers = Vec::new();

    for (item, _) in &parse_result.program.items {
        match item {
            // For actors, collect their declared super_traits and return immediately.
            Item::Actor(a) if a.name == name => {
                if let Some(bounds) = &a.super_traits {
                    for bound in bounds {
                        if let Some(hi) =
                            find_type_hierarchy_item(uri, source, lo, parse_result, &bound.name)
                        {
                            supers.push(hi);
                        }
                    }
                }
                return supers;
            }
            // For types, find impl blocks: `impl TraitName for TypeName`
            Item::Impl(impl_decl) => {
                let target = match &impl_decl.target_type.0 {
                    hew_parser::ast::TypeExpr::Named { name: tname, .. } => tname.as_str(),
                    _ => continue,
                };
                if target == name {
                    if let Some(bound) = &impl_decl.trait_bound {
                        if let Some(hi) =
                            find_type_hierarchy_item(uri, source, lo, parse_result, &bound.name)
                        {
                            supers.push(hi);
                        }
                    }
                }
            }
            _ => {}
        }
    }
    supers
}

pub(super) fn collect_subtypes(
    uri: &Url,
    trait_name: &str,
    source: &str,
    lo: &[usize],
    parse_result: &ParseResult,
) -> Vec<TypeHierarchyItem> {
    let mut subs = Vec::new();

    // Find types that implement this trait via `impl TraitName for TypeName`.
    for (item, _) in &parse_result.program.items {
        if let Item::Impl(impl_decl) = item {
            if let Some(bound) = &impl_decl.trait_bound {
                if bound.name == trait_name {
                    let target = match &impl_decl.target_type.0 {
                        hew_parser::ast::TypeExpr::Named { name: tname, .. } => tname.as_str(),
                        _ => continue,
                    };
                    if let Some(hi) =
                        find_type_hierarchy_item(uri, source, lo, parse_result, target)
                    {
                        subs.push(hi);
                    }
                }
            }
        }
    }

    // Find actors that declare this trait as a super_trait.
    for (item, _) in &parse_result.program.items {
        if let Item::Actor(a) = item {
            if let Some(bounds) = &a.super_traits {
                if bounds.iter().any(|b| b.name == trait_name) {
                    if let Some(hi) =
                        find_type_hierarchy_item(uri, source, lo, parse_result, &a.name)
                    {
                        subs.push(hi);
                    }
                }
            }
        }
    }
    subs
}

// ── Call hierarchy helpers ──────────────────────────────────────────

pub(super) fn find_callable_at(
    uri: &Url,
    source: &str,
    lo: &[usize],
    parse_result: &ParseResult,
    name: &str,
) -> Option<CallHierarchyItem> {
    for (item, item_span) in &parse_result.program.items {
        match item {
            Item::Function(f) if f.name == name => {
                let range = span_to_range(source, lo, item_span);
                return Some(CallHierarchyItem {
                    name: f.name.clone(),
                    kind: SymbolKind::FUNCTION,
                    tags: None,
                    detail: None,
                    uri: uri.clone(),
                    range,
                    selection_range: range,
                    data: None,
                });
            }
            Item::Actor(a) => {
                for recv in &a.receive_fns {
                    if recv.name == name {
                        let fn_span = if recv.span.is_empty() {
                            item_span
                        } else {
                            &recv.span
                        };
                        let range = span_to_range(source, lo, fn_span);
                        return Some(CallHierarchyItem {
                            name: recv.name.clone(),
                            kind: SymbolKind::METHOD,
                            tags: None,
                            detail: Some(format!("actor {}", a.name)),
                            uri: uri.clone(),
                            range,
                            selection_range: range,
                            data: None,
                        });
                    }
                }
            }
            _ => {}
        }
    }
    None
}

#[expect(
    clippy::too_many_lines,
    reason = "exhaustive match over caller item kinds is clearest as one function"
)]
pub(super) fn find_incoming_calls(
    uri: &Url,
    source: &str,
    lo: &[usize],
    parse_result: &ParseResult,
    target_name: &str,
) -> Vec<CallHierarchyIncomingCall> {
    let mut result = Vec::new();
    for (item, item_span) in &parse_result.program.items {
        // Collect all call sites inside this item using the exhaustive item walker.
        let mut calls = Vec::new();
        collect_calls_in_item(item, &mut calls);
        let matching: Vec<_> = calls.iter().filter(|c| c.name == target_name).collect();
        if matching.is_empty() {
            continue;
        }
        let from_ranges: Vec<_> = matching
            .iter()
            .map(|c| span_to_range(source, lo, &c.span))
            .collect();

        // Derive display metadata from the item kind.
        match item {
            Item::Function(f) => {
                let range = span_to_range(source, lo, item_span);
                result.push(CallHierarchyIncomingCall {
                    from: CallHierarchyItem {
                        name: f.name.clone(),
                        kind: SymbolKind::FUNCTION,
                        tags: None,
                        detail: None,
                        uri: uri.clone(),
                        range,
                        selection_range: range,
                        data: None,
                    },
                    from_ranges,
                });
            }
            Item::Actor(a) => {
                // Walk each named body independently for precise attribution.
                // init body (no dedicated name span — fall back to item span).
                if let Some(init) = &a.init {
                    let mut body_calls = Vec::new();
                    collect_calls_in_block(&init.body, &mut body_calls);
                    let fn_calls: Vec<_> = body_calls
                        .iter()
                        .filter(|c| c.name == target_name)
                        .collect();
                    if !fn_calls.is_empty() {
                        let range = span_to_range(source, lo, item_span);
                        result.push(CallHierarchyIncomingCall {
                            from: CallHierarchyItem {
                                name: format!("{}.init", a.name),
                                kind: SymbolKind::METHOD,
                                tags: None,
                                detail: None,
                                uri: uri.clone(),
                                range,
                                selection_range: range,
                                data: None,
                            },
                            from_ranges: fn_calls
                                .iter()
                                .map(|c| span_to_range(source, lo, &c.span))
                                .collect(),
                        });
                    }
                }
                // terminate body.
                if let Some(term) = &a.terminate {
                    let mut body_calls = Vec::new();
                    collect_calls_in_block(&term.body, &mut body_calls);
                    let fn_calls: Vec<_> = body_calls
                        .iter()
                        .filter(|c| c.name == target_name)
                        .collect();
                    if !fn_calls.is_empty() {
                        let range = span_to_range(source, lo, item_span);
                        result.push(CallHierarchyIncomingCall {
                            from: CallHierarchyItem {
                                name: format!("{}.terminate", a.name),
                                kind: SymbolKind::METHOD,
                                tags: None,
                                detail: None,
                                uri: uri.clone(),
                                range,
                                selection_range: range,
                                data: None,
                            },
                            from_ranges: fn_calls
                                .iter()
                                .map(|c| span_to_range(source, lo, &c.span))
                                .collect(),
                        });
                    }
                }
                // Receive functions.
                for recv in &a.receive_fns {
                    let mut body_calls = Vec::new();
                    collect_calls_in_block(&recv.body, &mut body_calls);
                    let fn_calls: Vec<_> = body_calls
                        .iter()
                        .filter(|c| c.name == target_name)
                        .collect();
                    if fn_calls.is_empty() {
                        continue;
                    }
                    let fn_span = if recv.span.is_empty() {
                        item_span
                    } else {
                        &recv.span
                    };
                    let range = span_to_range(source, lo, fn_span);
                    result.push(CallHierarchyIncomingCall {
                        from: CallHierarchyItem {
                            name: recv.name.clone(),
                            kind: SymbolKind::METHOD,
                            tags: None,
                            detail: Some(format!("actor {}", a.name)),
                            uri: uri.clone(),
                            range,
                            selection_range: range,
                            data: None,
                        },
                        from_ranges: fn_calls
                            .iter()
                            .map(|c| span_to_range(source, lo, &c.span))
                            .collect(),
                    });
                }
                // Actor methods.
                for method in &a.methods {
                    let mut body_calls = Vec::new();
                    collect_calls_in_block(&method.body, &mut body_calls);
                    let fn_calls: Vec<_> = body_calls
                        .iter()
                        .filter(|c| c.name == target_name)
                        .collect();
                    if fn_calls.is_empty() {
                        continue;
                    }
                    let range = if method.decl_span.is_empty() {
                        span_to_range(source, lo, item_span)
                    } else {
                        span_to_range(source, lo, &method.decl_span)
                    };
                    result.push(CallHierarchyIncomingCall {
                        from: CallHierarchyItem {
                            name: method.name.clone(),
                            kind: SymbolKind::METHOD,
                            tags: None,
                            detail: Some(format!("actor {}", a.name)),
                            uri: uri.clone(),
                            range,
                            selection_range: range,
                            data: None,
                        },
                        from_ranges: fn_calls
                            .iter()
                            .map(|c| span_to_range(source, lo, &c.span))
                            .collect(),
                    });
                }
            }
            Item::Impl(i) => {
                let impl_name = match &i.target_type.0 {
                    hew_parser::ast::TypeExpr::Named { name: tname, .. } => tname.clone(),
                    _ => "<impl>".to_string(),
                };
                let range = span_to_range(source, lo, item_span);
                result.push(CallHierarchyIncomingCall {
                    from: CallHierarchyItem {
                        name: impl_name,
                        kind: SymbolKind::NAMESPACE,
                        tags: None,
                        detail: None,
                        uri: uri.clone(),
                        range,
                        selection_range: range,
                        data: None,
                    },
                    from_ranges,
                });
            }
            Item::TypeDecl(td) => {
                let range = span_to_range(source, lo, item_span);
                result.push(CallHierarchyIncomingCall {
                    from: CallHierarchyItem {
                        name: td.name.clone(),
                        kind: SymbolKind::STRUCT,
                        tags: None,
                        detail: None,
                        uri: uri.clone(),
                        range,
                        selection_range: range,
                        data: None,
                    },
                    from_ranges,
                });
            }
            Item::Trait(t) => {
                let range = span_to_range(source, lo, item_span);
                result.push(CallHierarchyIncomingCall {
                    from: CallHierarchyItem {
                        name: t.name.clone(),
                        kind: SymbolKind::INTERFACE,
                        tags: None,
                        detail: None,
                        uri: uri.clone(),
                        range,
                        selection_range: range,
                        data: None,
                    },
                    from_ranges,
                });
            }
            Item::Supervisor(s) => {
                let range = span_to_range(source, lo, item_span);
                result.push(CallHierarchyIncomingCall {
                    from: CallHierarchyItem {
                        name: s.name.clone(),
                        kind: SymbolKind::MODULE,
                        tags: None,
                        detail: None,
                        uri: uri.clone(),
                        range,
                        selection_range: range,
                        data: None,
                    },
                    from_ranges,
                });
            }
            Item::Machine(m) => {
                let range = span_to_range(source, lo, item_span);
                result.push(CallHierarchyIncomingCall {
                    from: CallHierarchyItem {
                        name: m.name.clone(),
                        kind: SymbolKind::ENUM,
                        tags: None,
                        detail: None,
                        uri: uri.clone(),
                        range,
                        selection_range: range,
                        data: None,
                    },
                    from_ranges,
                });
            }
            Item::Const(c) => {
                let range = span_to_range(source, lo, item_span);
                result.push(CallHierarchyIncomingCall {
                    from: CallHierarchyItem {
                        name: c.name.clone(),
                        kind: SymbolKind::CONSTANT,
                        tags: None,
                        detail: None,
                        uri: uri.clone(),
                        range,
                        selection_range: range,
                        data: None,
                    },
                    from_ranges,
                });
            }
            _ => {}
        }
    }
    result
}

pub(super) fn find_outgoing_calls(
    uri: &Url,
    source: &str,
    lo: &[usize],
    parse_result: &ParseResult,
    caller_name: &str,
) -> Vec<CallHierarchyOutgoingCall> {
    let mut call_sites = Vec::new();

    // Collect calls only from the specific body that matches caller_name.
    // For multi-body items (Actor, Impl, TypeDecl, Trait) this walks only the
    // matching sub-body, preventing sibling methods from bleeding into the
    // outgoing call set.
    for (item, _) in &parse_result.program.items {
        collect_calls_in_named_body(item, caller_name, &mut call_sites);
    }

    // Group call sites by callee name
    let mut grouped: HashMap<String, Vec<Range>> = HashMap::new();
    for cs in &call_sites {
        grouped
            .entry(cs.name.clone())
            .or_default()
            .push(span_to_range(source, lo, &cs.span));
    }

    grouped
        .into_iter()
        .filter_map(|(callee_name, ranges)| {
            let target = find_callable_at(uri, source, lo, parse_result, &callee_name)?;
            Some(CallHierarchyOutgoingCall {
                to: target,
                from_ranges: ranges,
            })
        })
        .collect()
}
