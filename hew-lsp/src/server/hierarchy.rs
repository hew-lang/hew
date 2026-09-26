use hew_parser::ast::Ident;
use std::collections::HashMap;

use hew_analysis::calls::{
    collect_calls_in_block, collect_calls_in_item, collect_calls_in_named_body,
};
use hew_parser::ast::{Item, TraitItem, TypeBodyItem, TypeDeclKind};
use hew_parser::ParseResult;
use hew_types::check::scope::Resolution;
use hew_types::{DeclarationKind, DefId, TypeCheckOutput};
use tower_lsp_server::lsp_types::{
    CallHierarchyIncomingCall, CallHierarchyItem, CallHierarchyOutgoingCall, Range, SymbolKind,
    TypeHierarchyItem, Uri as Url,
};

use super::span_to_range;

fn callable_item(
    uri: &Url,
    source: &str,
    lo: &[usize],
    name: &str,
    kind: SymbolKind,
    span: &std::ops::Range<usize>,
    selection: &std::ops::Range<usize>,
    detail: Option<String>,
) -> CallHierarchyItem {
    CallHierarchyItem {
        name: name.to_string(),
        kind,
        tags: None,
        detail,
        uri: uri.clone(),
        range: span_to_range(source, lo, span),
        selection_range: span_to_range(source, lo, selection),
        data: None,
    }
}

/// Select the callable whose declaration token actually contains the cursor.
/// Equal method names in different impls remain separate source occurrences.
pub(super) fn find_callable_at_offset(
    uri: &Url,
    source: &str,
    lo: &[usize],
    parsed: &ParseResult,
    offset: usize,
) -> Option<CallHierarchyItem> {
    let contains = |span: &std::ops::Range<usize>| span.start <= offset && offset < span.end;
    for (item, _) in &parsed.program.items {
        match item {
            Item::Function(function) if contains(&function.decl_span) => {
                return Some(callable_item(
                    uri,
                    source,
                    lo,
                    function.name.name.as_str(),
                    SymbolKind::FUNCTION,
                    &function.fn_span,
                    &function.decl_span,
                    None,
                ))
            }
            Item::Impl(implementation) => {
                for method in &implementation.methods {
                    if contains(&method.decl_span) {
                        return Some(callable_item(
                            uri,
                            source,
                            lo,
                            method.name.name.as_str(),
                            SymbolKind::METHOD,
                            &method.fn_span,
                            &method.decl_span,
                            None,
                        ));
                    }
                }
            }
            Item::TypeDecl(declaration) => {
                for method in declaration.body.iter().filter_map(|part| match part {
                    TypeBodyItem::Method(method) => Some(method),
                    _ => None,
                }) {
                    if contains(&method.decl_span) {
                        return Some(callable_item(
                            uri,
                            source,
                            lo,
                            method.name.name.as_str(),
                            SymbolKind::METHOD,
                            &method.fn_span,
                            &method.decl_span,
                            Some(format!("type {}", declaration.name)),
                        ));
                    }
                }
            }
            Item::Actor(actor) => {
                for method in &actor.methods {
                    if contains(&method.decl_span) {
                        return Some(callable_item(
                            uri,
                            source,
                            lo,
                            method.name.name.as_str(),
                            SymbolKind::METHOD,
                            &method.fn_span,
                            &method.decl_span,
                            Some(format!("actor {}", actor.name)),
                        ));
                    }
                }
                for receive in &actor.receive_fns {
                    let name = super::navigation::find_identifier_span_in_range(
                        source,
                        receive.span.clone(),
                        receive.name.name.as_str(),
                    );
                    if let Some(name) =
                        name.filter(|name| name.start <= offset && offset < name.end)
                    {
                        return Some(callable_item(
                            uri,
                            source,
                            lo,
                            receive.name.name.as_str(),
                            SymbolKind::METHOD,
                            &receive.span,
                            &(name.start..name.end),
                            Some(format!("actor {}", actor.name)),
                        ));
                    }
                }
            }
            Item::Trait(trait_decl) => {
                for method in trait_decl.items.iter().filter_map(|part| match part {
                    TraitItem::Method(method) => Some(method),
                    _ => None,
                }) {
                    let name = super::navigation::find_identifier_span_in_range(
                        source,
                        method.span.clone(),
                        method.name.name.as_str(),
                    );
                    if let Some(name) =
                        name.filter(|name| name.start <= offset && offset < name.end)
                    {
                        return Some(callable_item(
                            uri,
                            source,
                            lo,
                            method.name.name.as_str(),
                            SymbolKind::METHOD,
                            &method.span,
                            &(name.start..name.end),
                            Some(format!("trait {}", trait_decl.name)),
                        ));
                    }
                }
            }
            _ => {}
        }
    }
    None
}

fn declaration_for_hierarchy_item(
    output: &TypeCheckOutput,
    source: &str,
    lo: &[usize],
    item: &CallHierarchyItem,
) -> Option<DefId> {
    output.fn_sigs.keys().copied().find(|id| {
        output.defs.name(*id).as_str() == item.name
            && output.defs.site(*id).is_some_and(|site| {
                site.module() == output.defs.root_module()
                    && span_to_range(source, lo, &site.span()) == item.range
            })
    })
}

fn call_reaches(
    call: &hew_analysis::calls::CallSite,
    name: &str,
    selected: Option<DefId>,
    output: Option<&TypeCheckOutput>,
) -> bool {
    if let (Some(selected), Some(output)) = (selected, output) {
        return match hew_analysis::calls::checked_call_target(call, output, 0) {
            Some(Some(target)) => target == selected,
            Some(None) => false,
            None => call.name == name,
        };
    }
    call.name == name
}

fn callable_for_declaration(
    uri: &Url,
    source: &str,
    lo: &[usize],
    parsed: &ParseResult,
    output: &TypeCheckOutput,
    declaration: DefId,
) -> Option<CallHierarchyItem> {
    let target = hew_analysis::identity::declaration_target(output, Resolution::Def(declaration))?;
    if target.occurrence.module() != output.defs.root_module() {
        return None;
    }
    let site = target.occurrence.span();
    let name = hew_analysis::identity::declaration_name_span(source, parsed, &target)?;
    let kind = match target.occurrence.kind() {
        DeclarationKind::Function | DeclarationKind::ExternFunction => SymbolKind::FUNCTION,
        _ => SymbolKind::METHOD,
    };
    Some(callable_item(
        uri,
        source,
        lo,
        &target.name,
        kind,
        &site,
        &(name.start..name.end),
        None,
    ))
}

fn caller_item_contains_body(
    item: &Item,
    source: &str,
    lo: &[usize],
    caller: &CallHierarchyItem,
) -> bool {
    let has_range = |span: &std::ops::Range<usize>| span_to_range(source, lo, span) == caller.range;
    match item {
        Item::Function(function) => has_range(&function.fn_span),
        Item::Impl(implementation) => implementation
            .methods
            .iter()
            .any(|method| has_range(&method.fn_span)),
        Item::TypeDecl(declaration) => declaration
            .body
            .iter()
            .any(|part| matches!(part, TypeBodyItem::Method(method) if has_range(&method.fn_span))),
        Item::Actor(actor) => {
            actor
                .methods
                .iter()
                .any(|method| has_range(&method.fn_span))
                || actor
                    .receive_fns
                    .iter()
                    .any(|receive| has_range(&receive.span))
        }
        Item::Trait(trait_decl) => trait_decl
            .items
            .iter()
            .any(|part| matches!(part, TraitItem::Method(method) if has_range(&method.span))),
        _ => false,
    }
}

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
                (td.name.name.as_str(), kind)
            }
            Item::Actor(a) => (a.name.name.as_str(), SymbolKind::CLASS),
            Item::Trait(t) => (t.name.name.as_str(), SymbolKind::INTERFACE),
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
            Item::Actor(a) if a.name == Ident::new(name) => {
                if let Some(bounds) = &a.super_traits {
                    for bound in bounds {
                        if let Some(hi) = find_type_hierarchy_item(
                            uri,
                            source,
                            lo,
                            parse_result,
                            &bound.path.to_string(),
                        )
                        // TRANSITION(P1): deleted by A1 commit 2
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
                    hew_parser::ast::TypeExpr::Named { path, .. } => path.to_string(), // TRANSITION(P1): deleted by A1 commit 2
                    _ => continue,
                };
                if target == name {
                    if let Some(bound) = &impl_decl.trait_bound {
                        if let Some(hi) = find_type_hierarchy_item(
                            uri,
                            source,
                            lo,
                            parse_result,
                            &bound.path.to_string(),
                        )
                        // TRANSITION(P1): deleted by A1 commit 2
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
                if bound.path.to_string() == trait_name {
                    // TRANSITION(P1): deleted by A1 commit 2
                    let target = match &impl_decl.target_type.0 {
                        hew_parser::ast::TypeExpr::Named { path, .. } => path.to_string(), // TRANSITION(P1): deleted by A1 commit 2
                        _ => continue,
                    };
                    if let Some(hi) =
                        find_type_hierarchy_item(uri, source, lo, parse_result, &target)
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
                if bounds.iter().any(|b| b.path.to_string() == trait_name) {
                    // TRANSITION(P1): deleted by A1 commit 2
                    if let Some(hi) = find_type_hierarchy_item(
                        uri,
                        source,
                        lo,
                        parse_result,
                        a.name.name.as_str(),
                    ) {
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
            Item::Function(f) if f.name == Ident::new(name) => {
                let range = span_to_range(source, lo, item_span);
                return Some(CallHierarchyItem {
                    name: f.name.to_string(),
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
                    if recv.name == Ident::new(name) {
                        let fn_span = if recv.span.is_empty() {
                            item_span
                        } else {
                            &recv.span
                        };
                        let range = span_to_range(source, lo, fn_span);
                        return Some(CallHierarchyItem {
                            name: recv.name.to_string(),
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
    target_item: Option<&CallHierarchyItem>,
    type_output: Option<&TypeCheckOutput>,
) -> Vec<CallHierarchyIncomingCall> {
    let selected = target_item.and_then(|item| {
        type_output.and_then(|output| declaration_for_hierarchy_item(output, source, lo, item))
    });
    let mut result = Vec::new();
    for (item, item_span) in &parse_result.program.items {
        // Collect all call sites inside this item using the exhaustive item walker.
        let mut calls = Vec::new();
        collect_calls_in_item(item, &mut calls);
        let matching: Vec<_> = calls
            .iter()
            .filter(|c| call_reaches(c, target_name, selected, type_output))
            .collect();
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
                        name: f.name.to_string(),
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
                        .filter(|c| call_reaches(c, target_name, selected, type_output))
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
                // Receive functions.
                for recv in &a.receive_fns {
                    let mut body_calls = Vec::new();
                    collect_calls_in_block(&recv.body, &mut body_calls);
                    let fn_calls: Vec<_> = body_calls
                        .iter()
                        .filter(|c| call_reaches(c, target_name, selected, type_output))
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
                            name: recv.name.to_string(),
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
                        .filter(|c| call_reaches(c, target_name, selected, type_output))
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
                            name: method.name.to_string(),
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
                    hew_parser::ast::TypeExpr::Named { path, .. } => path.to_string(), // TRANSITION(P1): deleted by A1 commit 2
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
                        name: td.name.to_string(),
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
                        name: t.name.to_string(),
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
                        name: s.name.to_string(),
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
                        name: m.name.to_string(),
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
                        name: c.name.to_string(),
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
    caller_item: Option<&CallHierarchyItem>,
    type_output: Option<&TypeCheckOutput>,
) -> Vec<CallHierarchyOutgoingCall> {
    let mut call_sites = Vec::new();

    // Collect calls only from the specific body that matches caller_name.
    // For multi-body items (Actor, Impl, TypeDecl, Trait) this walks only the
    // matching sub-body, preventing sibling methods from bleeding into the
    // outgoing call set.
    for (item, _) in &parse_result.program.items {
        if caller_item.is_some_and(|caller| !caller_item_contains_body(item, source, lo, caller)) {
            continue;
        }
        collect_calls_in_named_body(item, caller_name, &mut call_sites);
    }

    #[derive(PartialEq, Eq, Hash)]
    enum Target {
        Declaration(DefId),
        Name(String),
    }
    let mut grouped: HashMap<Target, Vec<Range>> = HashMap::new();
    for cs in &call_sites {
        let target = match type_output
            .and_then(|output| hew_analysis::calls::checked_call_target(cs, output, 0))
        {
            Some(Some(declaration)) => Target::Declaration(declaration),
            Some(None) => continue,
            None => Target::Name(cs.name.clone()),
        };
        grouped
            .entry(target)
            .or_default()
            .push(span_to_range(source, lo, &cs.span));
    }

    grouped
        .into_iter()
        .filter_map(|(callee, ranges)| {
            let target = match callee {
                Target::Declaration(id) => {
                    callable_for_declaration(uri, source, lo, parse_result, type_output?, id)?
                }
                Target::Name(name) => find_callable_at(uri, source, lo, parse_result, &name)?,
            };
            Some(CallHierarchyOutgoingCall {
                to: target,
                from_ranges: ranges,
            })
        })
        .collect()
}
