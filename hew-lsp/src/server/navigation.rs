use std::collections::{HashMap, HashSet};

use dashmap::DashMap;
use hew_analysis::util::compute_line_offsets;
use hew_parser::ast::{ImportDecl, ImportSpec, Item, Span};
use hew_parser::ParseResult;
use tower_lsp::lsp_types::{
    DocumentLink, Location, PrepareRenameResponse, Range, TextEdit, Url, WorkspaceEdit,
};

use super::{offset_range_to_lsp, span_to_range, DocumentState};

// ── Go-to-definition ─────────────────────────────────────────────────

/// Search for a definition matching `word` in the AST, returning an LSP `Range`.
pub(super) fn find_definition_in_ast(
    source: &str,
    lo: &[usize],
    parse_result: &ParseResult,
    word: &str,
) -> Option<Range> {
    let span = hew_analysis::definition::find_definition(source, parse_result, word)?;
    Some(offset_range_to_lsp(source, lo, span.start, span.end))
}

// ── Import path resolution ────────────────────────────────────────────

/// Compute the absolute file path that an `ImportDecl` would resolve to,
/// searching the workspace root first then the importing file's directory.
///
/// Does **not** check whether the file exists on disk; callers decide whether
/// to check existence before performing I/O.
pub(super) fn compute_import_path(uri: &Url, import: &ImportDecl) -> Option<std::path::PathBuf> {
    // String-literal import: `import "relative/path.hew";`
    if let Some(fp) = &import.file_path {
        let file_dir = uri
            .to_file_path()
            .ok()
            .and_then(|p| p.parent().map(std::path::Path::to_path_buf))?;
        return Some(file_dir.join(fp));
    }

    if import.path.is_empty() {
        return None;
    }

    let relative = format!("{}.hew", import.path.join("/"));

    // Workspace root: the directory that directly contains a `std/` folder.
    let workspace_root = uri.to_file_path().ok().and_then(|p| {
        let mut dir = p.parent().map(std::path::Path::to_path_buf);
        while let Some(d) = dir {
            if d.join("std").is_dir() {
                return Some(d);
            }
            dir = d.parent().map(std::path::Path::to_path_buf);
        }
        None
    });

    // Prefer workspace root when the file already exists there.
    if let Some(root) = workspace_root {
        let candidate = root.join(&relative);
        if candidate.exists() {
            return Some(candidate);
        }
    }

    // Fall back to the directory of the importing file.
    let file_dir = uri
        .to_file_path()
        .ok()
        .and_then(|p| p.parent().map(std::path::Path::to_path_buf))?;
    Some(file_dir.join(&relative))
}

pub(super) fn collect_import_items(parse_result: &ParseResult) -> Vec<(ImportDecl, Span)> {
    parse_result
        .program
        .items
        .iter()
        .filter_map(|(item, span)| match item {
            Item::Import(import) => Some((import.clone(), span.clone())),
            _ => None,
        })
        .collect()
}

pub(super) fn is_identifier_char(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}

pub(super) fn find_identifier_span_in_range(
    source: &str,
    range: std::ops::Range<usize>,
    ident: &str,
) -> Option<hew_analysis::OffsetSpan> {
    if ident.is_empty() {
        return None;
    }

    let slice = source.get(range.clone())?;
    let mut search_start = 0;
    while let Some(relative_start) = slice[search_start..].find(ident) {
        let start = range.start + search_start + relative_start;
        let end = start + ident.len();
        let prev_is_ident = source[..start]
            .chars()
            .next_back()
            .is_some_and(is_identifier_char);
        let next_is_ident = source[end..].chars().next().is_some_and(is_identifier_char);
        if !prev_is_ident && !next_is_ident {
            return Some(hew_analysis::OffsetSpan { start, end });
        }
        search_start += relative_start + ident.len();
    }

    None
}

pub(super) fn find_named_import_spans(
    source: &str,
    item_span: &Span,
    import_name: &hew_parser::ast::ImportName,
) -> Option<(hew_analysis::OffsetSpan, hew_analysis::OffsetSpan)> {
    let item_text = source.get(item_span.clone())?;
    let open_brace = item_text.find('{')?;
    let close_brace = item_text.rfind('}')?;
    if open_brace >= close_brace {
        return None;
    }

    let names_range = item_span.start + open_brace + 1..item_span.start + close_brace;
    let import_name_span =
        find_identifier_span_in_range(source, names_range.clone(), &import_name.name)?;
    let visible_name_span = match &import_name.alias {
        Some(alias) => find_identifier_span_in_range(source, names_range, alias)?,
        None => import_name_span,
    };

    Some((import_name_span, visible_name_span))
}

/// Cross-file references/rename currently follow explicit named imports between
/// open documents, reusing the existing import path resolution without a
/// broader project index.
#[derive(Debug, Clone)]
pub(super) struct NamedImportMatch {
    importer_uri: Url,
    imported_uri: Url,
    imported_name: String,
    visible_name: String,
    import_name_span: hew_analysis::OffsetSpan,
    visible_name_span: hew_analysis::OffsetSpan,
}

impl NamedImportMatch {
    fn is_aliased(&self) -> bool {
        self.visible_name != self.imported_name
    }
}

pub(super) fn find_named_import_match(
    current_uri: &Url,
    source: &str,
    parse_result: &ParseResult,
    word: &str,
    documents: &DashMap<Url, DocumentState>,
) -> Option<NamedImportMatch> {
    for (import, item_span) in collect_import_items(parse_result) {
        let Some(ImportSpec::Names(names)) = &import.spec else {
            continue;
        };
        let Some(path) = compute_import_path(current_uri, &import) else {
            continue;
        };
        let Ok(imported_uri) = Url::from_file_path(&path) else {
            continue;
        };
        let Some(target_doc) = documents.get(&imported_uri) else {
            continue;
        };

        for import_name in names {
            let visible_name = import_name
                .alias
                .as_deref()
                .unwrap_or(import_name.name.as_str());
            if visible_name != word {
                continue;
            }
            if hew_analysis::definition::find_definition(
                &target_doc.source,
                &target_doc.parse_result,
                &import_name.name,
            )
            .is_none()
            {
                continue;
            }
            let Some((import_name_span, visible_name_span)) =
                find_named_import_spans(source, &item_span, import_name)
            else {
                continue;
            };
            return Some(NamedImportMatch {
                importer_uri: current_uri.clone(),
                imported_uri,
                imported_name: import_name.name.clone(),
                visible_name: visible_name.to_string(),
                import_name_span,
                visible_name_span,
            });
        }
    }

    None
}

pub(super) fn span_contains_offset(span: hew_analysis::OffsetSpan, offset: usize) -> bool {
    span.start <= offset && offset < span.end
}

pub(super) fn find_resolved_named_import_match(
    current_uri: &Url,
    doc: &DocumentState,
    offset: usize,
    word: &str,
    documents: &DashMap<Url, DocumentState>,
) -> Option<(NamedImportMatch, Vec<hew_analysis::OffsetSpan>)> {
    let import_match =
        find_named_import_match(current_uri, &doc.source, &doc.parse_result, word, documents)?;
    let usage_spans = hew_analysis::references::find_import_binding_references(
        &doc.parse_result,
        &import_match.visible_name,
    );

    if span_contains_offset(import_match.visible_name_span, offset)
        || usage_spans
            .iter()
            .any(|span| span_contains_offset(*span, offset))
    {
        Some((import_match, usage_spans))
    } else {
        None
    }
}

pub(super) fn find_open_named_importers(
    target_uri: &Url,
    target_name: &str,
    documents: &DashMap<Url, DocumentState>,
) -> Vec<NamedImportMatch> {
    let mut matches = Vec::new();

    for entry in documents {
        let importer_uri = entry.key().clone();
        if importer_uri == *target_uri {
            continue;
        }
        let doc = entry.value();

        for (import, item_span) in collect_import_items(&doc.parse_result) {
            let Some(ImportSpec::Names(names)) = &import.spec else {
                continue;
            };
            let Some(path) = compute_import_path(&importer_uri, &import) else {
                continue;
            };
            let Ok(resolved_uri) = Url::from_file_path(&path) else {
                continue;
            };
            if resolved_uri != *target_uri {
                continue;
            }

            for import_name in names {
                if import_name.name != target_name {
                    continue;
                }
                let Some((import_name_span, visible_name_span)) =
                    find_named_import_spans(&doc.source, &item_span, import_name)
                else {
                    continue;
                };
                matches.push(NamedImportMatch {
                    importer_uri: importer_uri.clone(),
                    imported_uri: resolved_uri.clone(),
                    imported_name: import_name.name.clone(),
                    visible_name: import_name
                        .alias
                        .as_deref()
                        .unwrap_or(import_name.name.as_str())
                        .to_string(),
                    import_name_span,
                    visible_name_span,
                });
            }
        }
    }

    matches
}

pub(super) fn find_definition_name_span(
    source: &str,
    parse_result: &ParseResult,
    name: &str,
) -> Option<hew_analysis::OffsetSpan> {
    let item_span = hew_analysis::definition::find_definition(source, parse_result, name)?;
    find_identifier_span_in_range(source, item_span.start..item_span.end, name)
}

pub(super) fn push_location_for_span(
    locations: &mut Vec<Location>,
    uri: &Url,
    source: &str,
    line_offsets: &[usize],
    span: hew_analysis::OffsetSpan,
) {
    locations.push(Location {
        uri: uri.clone(),
        range: offset_range_to_lsp(source, line_offsets, span.start, span.end),
    });
}

pub(super) fn collect_local_reference_locations(
    uri: &Url,
    doc: &DocumentState,
    offset: usize,
    include_declaration: bool,
) -> Vec<Location> {
    let mut locations = Vec::new();

    if let Some((_name, spans)) =
        hew_analysis::references::find_all_references(&doc.source, &doc.parse_result, offset)
    {
        for span in spans {
            push_location_for_span(&mut locations, uri, &doc.source, &doc.line_offsets, span);
        }
    }

    if include_declaration {
        if let Some((name, _)) = hew_analysis::util::simple_word_at_offset(&doc.source, offset) {
            if let Some(def_span) =
                hew_analysis::definition::find_definition(&doc.source, &doc.parse_result, &name)
            {
                push_location_for_span(
                    &mut locations,
                    uri,
                    &doc.source,
                    &doc.line_offsets,
                    def_span,
                );
            }
        }
    }

    locations
}

pub(super) fn build_prepare_rename_response(
    uri: &Url,
    doc: &DocumentState,
    offset: usize,
    documents: &DashMap<Url, DocumentState>,
) -> Option<PrepareRenameResponse> {
    let span = if let Some((name, span)) =
        hew_analysis::util::simple_word_at_offset(&doc.source, offset)
    {
        if find_resolved_named_import_match(uri, doc, offset, &name, documents).is_some() {
            span
        } else {
            hew_analysis::rename::prepare_rename(&doc.source, &doc.parse_result, offset)?
        }
    } else {
        hew_analysis::rename::prepare_rename(&doc.source, &doc.parse_result, offset)?
    };
    let range = offset_range_to_lsp(&doc.source, &doc.line_offsets, span.start, span.end);
    Some(PrepareRenameResponse::Range(range))
}

pub(super) fn sort_and_dedup_locations(locations: &mut Vec<Location>) {
    locations.sort_by(|left, right| {
        left.uri
            .as_str()
            .cmp(right.uri.as_str())
            .then(left.range.start.line.cmp(&right.range.start.line))
            .then(left.range.start.character.cmp(&right.range.start.character))
            .then(left.range.end.line.cmp(&right.range.end.line))
            .then(left.range.end.character.cmp(&right.range.end.character))
    });
    locations.dedup_by(|left, right| left.uri == right.uri && left.range == right.range);
}

pub(super) fn collect_local_rename_edits(
    doc: &DocumentState,
    offset: usize,
    new_name: &str,
) -> Vec<hew_analysis::RenameEdit> {
    hew_analysis::rename::rename(&doc.source, &doc.parse_result, offset, new_name)
        .unwrap_or_default()
}

pub(super) fn sort_and_dedup_rename_edits(edits: &mut Vec<hew_analysis::RenameEdit>) {
    edits.sort_by(|left, right| {
        left.span
            .start
            .cmp(&right.span.start)
            .then(left.span.end.cmp(&right.span.end))
            .then_with(|| left.new_text.cmp(&right.new_text))
    });
    edits.dedup_by(|left, right| left.span == right.span && left.new_text == right.new_text);
}

pub(super) fn rename_edit_to_text_edit(
    doc: &DocumentState,
    edit: hew_analysis::RenameEdit,
) -> TextEdit {
    TextEdit {
        range: offset_range_to_lsp(
            &doc.source,
            &doc.line_offsets,
            edit.span.start,
            edit.span.end,
        ),
        new_text: edit.new_text,
    }
}

pub(super) fn workspace_edit_from_changes(
    uri: &Url,
    doc: &DocumentState,
    documents: &DashMap<Url, DocumentState>,
    mut changes: HashMap<Url, Vec<hew_analysis::RenameEdit>>,
) -> Option<WorkspaceEdit> {
    for edits in changes.values_mut() {
        sort_and_dedup_rename_edits(edits);
    }
    changes.retain(|_, edits| !edits.is_empty());

    if changes.is_empty() {
        return None;
    }

    let mut lsp_changes = HashMap::new();
    for (target_uri, edits) in changes {
        let text_edits = if target_uri == *uri {
            edits
                .into_iter()
                .map(|edit| rename_edit_to_text_edit(doc, edit))
                .collect()
        } else if let Some(target_doc) = documents.get(&target_uri) {
            edits
                .into_iter()
                .map(|edit| rename_edit_to_text_edit(&target_doc, edit))
                .collect()
        } else {
            continue;
        };
        lsp_changes.insert(target_uri, text_edits);
    }

    Some(WorkspaceEdit {
        changes: Some(lsp_changes),
        ..Default::default()
    })
}

pub(super) fn build_reference_locations(
    uri: &Url,
    doc: &DocumentState,
    offset: usize,
    include_declaration: bool,
    documents: &DashMap<Url, DocumentState>,
) -> Vec<Location> {
    let Some((name, _)) = hew_analysis::util::simple_word_at_offset(&doc.source, offset) else {
        return Vec::new();
    };

    if let Some((import_match, usage_spans)) =
        find_resolved_named_import_match(uri, doc, offset, &name, documents)
    {
        let mut locations = Vec::new();
        push_location_for_span(
            &mut locations,
            &import_match.importer_uri,
            &doc.source,
            &doc.line_offsets,
            import_match.visible_name_span,
        );
        for span in usage_spans {
            push_location_for_span(
                &mut locations,
                &import_match.importer_uri,
                &doc.source,
                &doc.line_offsets,
                span,
            );
        }

        if let Some(target_doc) = documents.get(&import_match.imported_uri) {
            if let Some(def_span) = find_definition_name_span(
                &target_doc.source,
                &target_doc.parse_result,
                &import_match.imported_name,
            ) {
                locations.extend(collect_local_reference_locations(
                    &import_match.imported_uri,
                    &target_doc,
                    def_span.start,
                    include_declaration,
                ));
            }
        }

        for importer in find_open_named_importers(
            &import_match.imported_uri,
            &import_match.imported_name,
            documents,
        ) {
            if importer.importer_uri == import_match.importer_uri {
                continue;
            }
            if let Some(importer_doc) = documents.get(&importer.importer_uri) {
                push_location_for_span(
                    &mut locations,
                    &importer.importer_uri,
                    &importer_doc.source,
                    &importer_doc.line_offsets,
                    importer.import_name_span,
                );
                for span in hew_analysis::references::find_import_binding_references(
                    &importer_doc.parse_result,
                    &importer.visible_name,
                ) {
                    push_location_for_span(
                        &mut locations,
                        &importer.importer_uri,
                        &importer_doc.source,
                        &importer_doc.line_offsets,
                        span,
                    );
                }
            }
        }

        sort_and_dedup_locations(&mut locations);
        return locations;
    }

    let mut locations = collect_local_reference_locations(uri, doc, offset, include_declaration);

    if hew_analysis::references::is_top_level_name(&doc.parse_result, &name) {
        for importer in find_open_named_importers(uri, &name, documents) {
            if let Some(importer_doc) = documents.get(&importer.importer_uri) {
                push_location_for_span(
                    &mut locations,
                    &importer.importer_uri,
                    &importer_doc.source,
                    &importer_doc.line_offsets,
                    importer.import_name_span,
                );
                for span in hew_analysis::references::find_import_binding_references(
                    &importer_doc.parse_result,
                    &importer.visible_name,
                ) {
                    push_location_for_span(
                        &mut locations,
                        &importer.importer_uri,
                        &importer_doc.source,
                        &importer_doc.line_offsets,
                        span,
                    );
                }
            }
        }
    }

    sort_and_dedup_locations(&mut locations);
    locations
}

#[expect(
    clippy::too_many_lines,
    reason = "cross-file rename handles local, imported-definition, and importer fanout paths"
)]
pub(super) fn build_workspace_edit(
    uri: &Url,
    doc: &DocumentState,
    offset: usize,
    new_name: &str,
    documents: &DashMap<Url, DocumentState>,
) -> Option<WorkspaceEdit> {
    let (name, _) = hew_analysis::util::simple_word_at_offset(&doc.source, offset)?;

    if let Some((import_match, usage_spans)) =
        find_resolved_named_import_match(uri, doc, offset, &name, documents)
    {
        let mut changes: HashMap<Url, Vec<hew_analysis::RenameEdit>> = HashMap::new();
        let mut importer_edits: Vec<_> = usage_spans
            .into_iter()
            .map(|span| hew_analysis::RenameEdit {
                span,
                new_text: new_name.to_string(),
            })
            .collect();
        importer_edits.push(hew_analysis::RenameEdit {
            span: import_match.visible_name_span,
            new_text: new_name.to_string(),
        });
        changes.insert(uri.clone(), importer_edits);

        if !import_match.is_aliased() {
            if let Some(target_doc) = documents.get(&import_match.imported_uri) {
                if let Some(def_span) = find_definition_name_span(
                    &target_doc.source,
                    &target_doc.parse_result,
                    &import_match.imported_name,
                ) {
                    let target_edits =
                        collect_local_rename_edits(&target_doc, def_span.start, new_name);
                    if !target_edits.is_empty() {
                        changes
                            .entry(import_match.imported_uri.clone())
                            .or_default()
                            .extend(target_edits);
                    }
                }
            }

            for importer in find_open_named_importers(
                &import_match.imported_uri,
                &import_match.imported_name,
                documents,
            ) {
                if importer.importer_uri == *uri {
                    continue;
                }
                changes
                    .entry(importer.importer_uri.clone())
                    .or_default()
                    .push(hew_analysis::RenameEdit {
                        span: importer.import_name_span,
                        new_text: new_name.to_string(),
                    });

                if importer.is_aliased() {
                    continue;
                }

                if let Some(importer_doc) = documents.get(&importer.importer_uri) {
                    let importer_edits: Vec<_> =
                        hew_analysis::references::find_import_binding_references(
                            &importer_doc.parse_result,
                            &importer.visible_name,
                        )
                        .into_iter()
                        .map(|span| hew_analysis::RenameEdit {
                            span,
                            new_text: new_name.to_string(),
                        })
                        .collect();
                    if !importer_edits.is_empty() {
                        changes
                            .entry(importer.importer_uri.clone())
                            .or_default()
                            .extend(importer_edits);
                    }
                }
            }
        }

        return workspace_edit_from_changes(uri, doc, documents, changes);
    }

    let mut changes: HashMap<Url, Vec<hew_analysis::RenameEdit>> = HashMap::new();
    let local_edits = collect_local_rename_edits(doc, offset, new_name);
    if !local_edits.is_empty() {
        changes.insert(uri.clone(), local_edits);
    }

    if hew_analysis::references::is_top_level_name(&doc.parse_result, &name) {
        for importer in find_open_named_importers(uri, &name, documents) {
            changes
                .entry(importer.importer_uri.clone())
                .or_default()
                .push(hew_analysis::RenameEdit {
                    span: importer.import_name_span,
                    new_text: new_name.to_string(),
                });

            if importer.is_aliased() {
                continue;
            }

            if let Some(importer_doc) = documents.get(&importer.importer_uri) {
                let importer_edits: Vec<_> =
                    hew_analysis::references::find_import_binding_references(
                        &importer_doc.parse_result,
                        &importer.visible_name,
                    )
                    .into_iter()
                    .map(|span| hew_analysis::RenameEdit {
                        span,
                        new_text: new_name.to_string(),
                    })
                    .collect();
                if !importer_edits.is_empty() {
                    changes
                        .entry(importer.importer_uri.clone())
                        .or_default()
                        .extend(importer_edits);
                }
            }
        }
    }

    workspace_edit_from_changes(uri, doc, documents, changes)
}

/// Plan a rename with cross-file conflict detection.
///
/// Returns:
/// - `Ok(Some(edit))` when the rename can be applied; `edit` is the
///   same `WorkspaceEdit` [`build_workspace_edit`] would produce.
/// - `Ok(None)` when the cursor is not on a valid rename target.
/// - `Err(RenameError)` when the rename is refused — the new name is
///   a keyword / builtin, is not a valid identifier, or would clash
///   with an existing binding in any file that would be edited.
///
/// Unlike [`hew_analysis::rename::plan_rename`] (which validates a
/// single file's own edits), this function additionally walks all files
/// that would be modified across the workspace to check that `new_name`
/// does not clash with top-level or imported names in any of them. For
/// non-aliased imports, the check includes both the definition file and
/// all other open files that import the same name.
pub(super) fn plan_workspace_rename(
    uri: &Url,
    doc: &DocumentState,
    offset: usize,
    new_name: &str,
    documents: &DashMap<Url, DocumentState>,
) -> Result<Option<WorkspaceEdit>, hew_analysis::RenameError> {
    // Probe the local file's plan_rename to surface same-file conflicts
    // with rich spans before falling back to the cross-file compositor.
    match hew_analysis::rename::plan_rename(&doc.source, &doc.parse_result, offset, new_name) {
        Ok(_edits) => {}
        Err(err) => return Err(err),
    }

    // For each file that will receive cross-file edits, check that
    // `new_name` is not already a top-level or imported name there.
    let mut cross_file_conflicts: Vec<hew_analysis::RenameConflict> = Vec::new();
    let Some((name, _)) = hew_analysis::util::simple_word_at_offset(&doc.source, offset) else {
        return Ok(None);
    };

    // If this rename has cross-file reach (imported or definition of a
    // top-level item), walk each other file that would be edited.
    if let Some((import_match, _)) =
        find_resolved_named_import_match(uri, doc, offset, &name, documents)
    {
        // Check the definition file and every open importer (except
        // this one, which was already validated).
        if !import_match.is_aliased() {
            if let Some(target_doc) = documents.get(&import_match.imported_uri) {
                collect_cross_file_conflict(
                    &target_doc,
                    new_name,
                    &import_match.imported_name,
                    &mut cross_file_conflicts,
                );

                // `collect_cross_file_conflict` checks top-level and import
                // clashes in the definition file, but does NOT walk that file's
                // own local/param scopes (it has no offset to anchor a
                // `plan_rename` call). Do it here so that a `let <new_name>`
                // shadowing a call-site of the definition in the same file is
                // caught on the importer-originated path, matching the
                // definition-originated path (which already runs `plan_rename`
                // against the current doc at line ~747 above).
                if let Some(def_span) = hew_analysis::definition::find_definition(
                    &target_doc.source,
                    &target_doc.parse_result,
                    &import_match.imported_name,
                ) {
                    if let Err(hew_analysis::RenameError::Conflicts { conflicts }) =
                        hew_analysis::rename::plan_rename(
                            &target_doc.source,
                            &target_doc.parse_result,
                            def_span.start,
                            new_name,
                        )
                    {
                        cross_file_conflicts.extend(conflicts);
                    }
                }
            }
            for importer in find_open_named_importers(
                &import_match.imported_uri,
                &import_match.imported_name,
                documents,
            ) {
                if importer.importer_uri == *uri {
                    continue;
                }
                if let Some(importer_doc) = documents.get(&importer.importer_uri) {
                    collect_cross_file_conflict(
                        &importer_doc,
                        new_name,
                        &importer.visible_name,
                        &mut cross_file_conflicts,
                    );
                }
            }
        }
    } else if hew_analysis::references::is_top_level_name(&doc.parse_result, &name) {
        for importer in find_open_named_importers(uri, &name, documents) {
            // Aliased importers (`import foo::{x as y}`) will have their
            // import-name token rewritten to `new_name` but the visible name
            // in the importer file remains the alias, not `new_name`. Treating
            // the alias as an existing binding named `new_name` is a false
            // positive — mirror the guard at the import-originated path above.
            if importer.is_aliased() {
                continue;
            }
            if let Some(importer_doc) = documents.get(&importer.importer_uri) {
                collect_cross_file_conflict(
                    &importer_doc,
                    new_name,
                    &importer.visible_name,
                    &mut cross_file_conflicts,
                );
            }
        }
    }

    // Deduplicate conflicts on (existing_span, offending_span, kind).
    // Both collect_cross_file_conflict and the plan_rename probe may report
    // the same ShadowsTopLevel/ShadowsImport conflict, inflating the count
    // in the editor UI.
    let mut deduped = Vec::new();
    for conflict in cross_file_conflicts {
        if !deduped
            .iter()
            .any(|existing: &hew_analysis::RenameConflict| {
                existing.existing_span == conflict.existing_span
                    && existing.offending_span == conflict.offending_span
                    && existing.kind == conflict.kind
            })
        {
            deduped.push(conflict);
        }
    }
    cross_file_conflicts = deduped;

    if !cross_file_conflicts.is_empty() {
        return Err(hew_analysis::RenameError::Conflicts {
            conflicts: cross_file_conflicts,
        });
    }

    Ok(build_workspace_edit(uri, doc, offset, new_name, documents))
}

/// Inspect another file's document for a pre-existing top-level item,
/// imported name, or local variable/parameter equal to `new_name`. If
/// found, push a conflict whose `offending_span` points at
/// `offending_visible_name`'s binding in that file (the name the
/// cross-file compositor would rewrite).
fn collect_cross_file_conflict(
    other_doc: &DocumentState,
    new_name: &str,
    offending_visible_name: &str,
    conflicts: &mut Vec<hew_analysis::RenameConflict>,
) {
    if hew_analysis::references::is_top_level_name(&other_doc.parse_result, new_name) {
        if let Some(existing) = hew_analysis::definition::find_definition(
            &other_doc.source,
            &other_doc.parse_result,
            new_name,
        ) {
            let offending = hew_analysis::definition::find_definition(
                &other_doc.source,
                &other_doc.parse_result,
                offending_visible_name,
            )
            .unwrap_or(existing);
            conflicts.push(hew_analysis::RenameConflict {
                kind: hew_analysis::RenameConflictKind::ShadowsTopLevel,
                existing_span: existing,
                offending_span: offending,
                message: format!(
                    "renaming would clash with existing top-level '{new_name}' in another file"
                ),
            });
        }
    } else if let Some(existing) =
        hew_analysis::resolver::find_matching_import(&other_doc.parse_result, new_name)
    {
        let offending = hew_analysis::definition::find_definition(
            &other_doc.source,
            &other_doc.parse_result,
            offending_visible_name,
        )
        .unwrap_or(existing);
        conflicts.push(hew_analysis::RenameConflict {
            kind: hew_analysis::RenameConflictKind::ShadowsImport,
            existing_span: existing,
            offending_span: offending,
            message: format!("renaming would clash with imported '{new_name}' in another file"),
        });
    }

    // Check every usage site of the imported binding in this file: if
    // `new_name` is already a local variable or parameter in scope at any
    // usage, renaming would silently shadow it there.
    let usage_spans = hew_analysis::references::find_import_binding_references(
        &other_doc.parse_result,
        offending_visible_name,
    );
    for usage in usage_spans {
        if let Some(existing) = hew_analysis::definition::find_local_binding_definition(
            &other_doc.source,
            &other_doc.parse_result,
            new_name,
            usage.start,
        ) {
            // Deduplicate on (existing, offending): many usages may share
            // the same in-scope local, and we only need one conflict per pair.
            if !conflicts
                .iter()
                .any(|c| c.existing_span == existing && c.offending_span == usage)
            {
                conflicts.push(hew_analysis::RenameConflict {
                    kind: hew_analysis::RenameConflictKind::ShadowsLocal,
                    existing_span: existing,
                    offending_span: usage,
                    message: format!(
                        "renaming would shadow local '{new_name}' at a usage site in another file"
                    ),
                });
            }
            continue;
        }
        if let Some(existing) = hew_analysis::definition::find_param_definition(
            &other_doc.parse_result,
            new_name,
            usage.start,
        ) {
            if !conflicts
                .iter()
                .any(|c| c.existing_span == existing && c.offending_span == usage)
            {
                conflicts.push(hew_analysis::RenameConflict {
                    kind: hew_analysis::RenameConflictKind::ShadowsLocal,
                    existing_span: existing,
                    offending_span: usage,
                    message: format!(
                        "renaming would shadow parameter '{new_name}' at a usage site in another file"
                    ),
                });
            }
        }
    }
}

// ── Cross-file go-to-definition ───────────────────────────────────────

/// Search for the definition of `word` in the files imported by the current
/// file's `parse_result`.
///
/// Resolution order for each matching import:
/// 1. Open documents already held by the server (no disk I/O).
/// 2. The file on disk (parsed on demand).
///
/// Returns `(target_uri, range)` for the first match found, or `None`.
pub(super) fn find_cross_file_definition(
    current_uri: &Url,
    imports: &[hew_parser::ast::ImportDecl],
    word: &str,
    documents: &DashMap<Url, DocumentState>,
) -> Option<(Url, Range)> {
    const MAX_TRANSITIVE_IMPORT_HOPS: usize = 1;

    let mut seen = HashSet::from([current_uri.clone()]);
    find_cross_file_definition_impl(
        current_uri,
        imports,
        word,
        documents,
        &mut seen,
        MAX_TRANSITIVE_IMPORT_HOPS,
    )
}

fn find_cross_file_definition_impl(
    current_uri: &Url,
    imports: &[hew_parser::ast::ImportDecl],
    word: &str,
    documents: &DashMap<Url, DocumentState>,
    seen: &mut HashSet<Url>,
    remaining_hops: usize,
) -> Option<(Url, Range)> {
    for import in imports {
        let Some(path) = compute_import_path(current_uri, import) else {
            continue;
        };
        let Ok(target_uri) = Url::from_file_path(&path) else {
            continue;
        };

        // Determine which name to search for in the target file, based on
        // what this import makes visible in the current scope.
        let search_name: &str = match &import.spec {
            Some(ImportSpec::Names(names)) => {
                // Find the entry whose visible name (alias if present, otherwise
                // the original name) matches `word`.
                let Some(entry) = names
                    .iter()
                    .find(|n| n.alias.as_deref().unwrap_or(n.name.as_str()) == word)
                else {
                    continue; // this import does not bring `word` into scope
                };
                &entry.name // search target file by the *original* name
            }
            Some(ImportSpec::Glob) => word, // glob: any name may come from here
            None => {
                // Bare path import (`import foo::bar`).  The last path segment
                // is itself a navigable target (e.g. cursor on `bar`).
                if import.path.last().map(String::as_str) == Some(word)
                    && (documents.contains_key(&target_uri) || path.exists())
                {
                    return Some((target_uri, Range::default()));
                }
                continue;
            }
        };
        if !seen.insert(target_uri.clone()) {
            continue;
        }

        let result = load_navigation_target(&target_uri, &path, documents).and_then(
            |(source, line_offsets, parse_result)| {
                if let Some(range) =
                    find_definition_in_ast(&source, &line_offsets, &parse_result, search_name)
                {
                    return Some((target_uri.clone(), range));
                }

                if remaining_hops == 0 {
                    return None;
                }

                let nested_imports = collect_import_items(&parse_result)
                    .into_iter()
                    .map(|(import, _)| import)
                    .collect::<Vec<_>>();

                find_cross_file_definition_impl(
                    &target_uri,
                    &nested_imports,
                    search_name,
                    documents,
                    seen,
                    remaining_hops - 1,
                )
            },
        );

        seen.remove(&target_uri);

        if result.is_some() {
            return result;
        }
    }
    None
}

fn load_navigation_target(
    target_uri: &Url,
    path: &std::path::Path,
    documents: &DashMap<Url, DocumentState>,
) -> Option<(String, Vec<usize>, ParseResult)> {
    if let Some(doc) = documents.get(target_uri) {
        let source = doc.source.clone();
        let line_offsets = doc.line_offsets.clone();
        let parse_result = hew_parser::parse(&source);
        return Some((source, line_offsets, parse_result));
    }

    if path.exists() {
        if let Ok(source) = std::fs::read_to_string(path) {
            let parse_result = hew_parser::parse(&source);
            let line_offsets = compute_line_offsets(&source);
            return Some((source, line_offsets, parse_result));
        }
    }

    None
}

// ── Document links ──────────────────────────────────────────────────

pub(super) fn build_document_links(
    uri: &Url,
    source: &str,
    lo: &[usize],
    parse_result: &ParseResult,
) -> Vec<DocumentLink> {
    let mut links = Vec::new();

    for (item, span) in &parse_result.program.items {
        if let Item::Import(import) = item {
            let Some(path) = compute_import_path(uri, import) else {
                continue;
            };
            if !path.exists() {
                continue;
            }
            if let Ok(target_uri) = Url::from_file_path(&path) {
                let relative = format!("{}.hew", import.path.join("/"));
                links.push(DocumentLink {
                    range: span_to_range(source, lo, span),
                    target: Some(target_uri),
                    tooltip: Some(format!("Open {relative}")),
                    data: None,
                });
            }
        }
    }
    links
}
