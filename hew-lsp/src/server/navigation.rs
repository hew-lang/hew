use std::collections::{HashMap, HashSet};

use dashmap::DashMap;
use hew_analysis::util::compute_line_offsets;
use hew_parser::ast::{ImportDecl, ImportSpec, Item, Span};
use hew_parser::ParseResult;
use tower_lsp::lsp_types::{
    DocumentLink, Location, PrepareRenameResponse, Range, TextEdit, Url, WorkspaceEdit,
};

use super::workspace::find_workspace_root_for_uri;
use super::{offset_range_to_lsp, span_to_range, DocumentState};

// ── Helpers ──────────────────────────────────────────────────────────

/// Load source code from a URI, returning a `RenameError::Io` on any I/O or
/// URI-conversion failure. Encapsulates the "could not convert URI to file path"
/// and "could not read source file" error patterns.
fn read_source_or_io_error(uri: &Url) -> Result<String, hew_analysis::RenameError> {
    let path = uri
        .to_file_path()
        .map_err(|_e| hew_analysis::RenameError::Io {
            path: uri.to_string(),
            message: "could not convert URI to file path".to_string(),
        })?;
    std::fs::read_to_string(&path).map_err(|e| hew_analysis::RenameError::Io {
        path: path.display().to_string(),
        message: e.to_string(),
    })
}

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

    // Prefer workspace root when the file already exists there.
    if let Some(root) = find_workspace_root_for_uri(uri) {
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
    let mut imports = Vec::with_capacity(parse_result.program.items.len());
    for (item, span) in &parse_result.program.items {
        if let Item::Import(import) = item {
            imports.push((import.clone(), span.clone()));
        }
    }
    imports
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

type NamedImporterIndex = HashMap<(Url, String), Vec<NamedImportMatch>>;

fn build_named_importer_index(documents: &DashMap<Url, DocumentState>) -> NamedImporterIndex {
    let mut index: NamedImporterIndex = HashMap::new();

    for entry in documents {
        let importer_uri = entry.key().clone();
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

            for import_name in names {
                let Some((import_name_span, visible_name_span)) =
                    find_named_import_spans(&doc.source, &item_span, import_name)
                else {
                    continue;
                };
                let visible_name = import_name
                    .alias
                    .as_deref()
                    .unwrap_or(import_name.name.as_str())
                    .to_string();
                index
                    .entry((resolved_uri.clone(), import_name.name.clone()))
                    .or_default()
                    .push(NamedImportMatch {
                        importer_uri: importer_uri.clone(),
                        imported_uri: resolved_uri.clone(),
                        imported_name: import_name.name.clone(),
                        visible_name,
                        import_name_span,
                        visible_name_span,
                    });
            }
        }
    }

    index
}

fn indexed_named_importers<'a>(
    index: &'a NamedImporterIndex,
    target_uri: &Url,
    target_name: &str,
) -> impl Iterator<Item = &'a NamedImportMatch> {
    index
        .get(&(target_uri.clone(), target_name.to_string()))
        .into_iter()
        .flat_map(|matches| matches.iter())
}

pub(super) fn collect_importer_fanout_spans(
    parse_result: &ParseResult,
    importer: &NamedImportMatch,
) -> Vec<hew_analysis::OffsetSpan> {
    let mut spans = Vec::with_capacity(1);
    spans.push(importer.import_name_span);
    if importer.is_aliased() {
        return spans;
    }

    let reference_spans = hew_analysis::references::find_import_binding_references(
        parse_result,
        &importer.visible_name,
    );
    spans.reserve(reference_spans.len());
    spans.extend(reference_spans);
    spans
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

        for import_name in names {
            let visible_name = import_name
                .alias
                .as_deref()
                .unwrap_or(import_name.name.as_str());
            if visible_name != word {
                continue;
            }

            // Check if the definition exists in the imported file.
            // Try the open-documents path first (fast), then fall back to disk (slow).
            let has_definition = if let Some(target_doc) = documents.get(&imported_uri) {
                hew_analysis::definition::find_definition(
                    &target_doc.source,
                    &target_doc.parse_result,
                    &import_name.name,
                )
                .is_some()
            } else {
                // File not open; check on disk (degrades gracefully on I/O error).
                if let Ok(file_path) = imported_uri.to_file_path() {
                    if let Ok(file_source) = std::fs::read_to_string(&file_path) {
                        let file_parse = hew_parser::parse(&file_source);
                        hew_analysis::definition::find_definition(
                            &file_source,
                            &file_parse,
                            &import_name.name,
                        )
                        .is_some()
                    } else {
                        false
                    }
                } else {
                    false
                }
            };

            if !has_definition {
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
) -> Result<Vec<hew_analysis::RenameEdit>, hew_analysis::RenameError> {
    hew_analysis::rename::plan_rename(&doc.source, &doc.parse_result, offset, new_name)
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
) -> Result<Option<WorkspaceEdit>, hew_analysis::RenameError> {
    for edits in changes.values_mut() {
        sort_and_dedup_rename_edits(edits);
    }
    changes.retain(|_, edits| !edits.is_empty());

    if changes.is_empty() {
        return Ok(None);
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
            // File is not open; load from disk to generate edits.
            // If disk read/parse fails, return an error to reject the rename entirely.
            let target_source = read_source_or_io_error(&target_uri)?;
            let target_line_offsets = hew_analysis::util::compute_line_offsets(&target_source);
            edits
                .into_iter()
                .map(|edit| TextEdit {
                    range: offset_range_to_lsp(
                        &target_source,
                        &target_line_offsets,
                        edit.span.start,
                        edit.span.end,
                    ),
                    new_text: edit.new_text,
                })
                .collect()
        };
        lsp_changes.insert(target_uri, text_edits);
    }

    Ok(Some(WorkspaceEdit {
        changes: Some(lsp_changes),
        ..Default::default()
    }))
}

pub(super) fn build_reference_locations(
    uri: &Url,
    doc: &DocumentState,
    offset: usize,
    include_declaration: bool,
    documents: &DashMap<Url, DocumentState>,
) -> Vec<Location> {
    let importer_index = build_named_importer_index(documents);
    let Some((name, _)) = hew_analysis::util::simple_word_at_offset(&doc.source, offset) else {
        return Vec::new();
    };

    if let Some((import_match, usage_spans)) =
        find_resolved_named_import_match(uri, doc, offset, &name, documents)
    {
        let mut locations = Vec::with_capacity(1 + usage_spans.len());
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

        for importer in indexed_named_importers(
            &importer_index,
            &import_match.imported_uri,
            &import_match.imported_name,
        ) {
            if importer.importer_uri == import_match.importer_uri {
                continue;
            }
            if let Some(importer_doc) = documents.get(&importer.importer_uri) {
                for span in collect_importer_fanout_spans(&importer_doc.parse_result, importer) {
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
        for importer in indexed_named_importers(&importer_index, uri, &name) {
            if let Some(importer_doc) = documents.get(&importer.importer_uri) {
                for span in collect_importer_fanout_spans(&importer_doc.parse_result, importer) {
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
    } else {
        // The name is not a top-level definition in this file.  It may be a
        // stdlib builtin (e.g. `println`) that is always in scope without an
        // explicit import.  Walk the whole workspace to collect occurrences.
        let workspace_locs =
            collect_workspace_references(uri, &name, include_declaration, documents);
        locations.extend(workspace_locs);
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
) -> Result<Option<WorkspaceEdit>, hew_analysis::RenameError> {
    let importer_index = build_named_importer_index(documents);
    let (name, _) = hew_analysis::util::simple_word_at_offset(&doc.source, offset).ok_or(
        hew_analysis::RenameError::InvalidIdentifier {
            name: String::new(),
            message: "cursor not on a valid identifier".to_string(),
        },
    )?;

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
                        collect_local_rename_edits(&target_doc, def_span.start, new_name)?;
                    if !target_edits.is_empty() {
                        changes
                            .entry(import_match.imported_uri.clone())
                            .or_default()
                            .extend(target_edits);
                    }
                }
            } else {
                // Definition file is closed; load from disk and compute edits.
                let target_source = read_source_or_io_error(&import_match.imported_uri)?;
                let target_parse = hew_parser::parse(&target_source);
                if let Some(def_span) = find_definition_name_span(
                    &target_source,
                    &target_parse,
                    &import_match.imported_name,
                ) {
                    let target_edits = hew_analysis::rename::plan_rename(
                        &target_source,
                        &target_parse,
                        def_span.start,
                        new_name,
                    )?;
                    if !target_edits.is_empty() {
                        changes
                            .entry(import_match.imported_uri.clone())
                            .or_default()
                            .extend(target_edits);
                    }
                }
            }

            for importer in indexed_named_importers(
                &importer_index,
                &import_match.imported_uri,
                &import_match.imported_name,
            ) {
                if importer.importer_uri == *uri {
                    continue;
                }
                if let Some(importer_doc) = documents.get(&importer.importer_uri) {
                    let importer_edits: Vec<_> =
                        collect_importer_fanout_spans(&importer_doc.parse_result, importer)
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

            // Include edits for unopened sibling importers (parallel to the conflict
            // scan in plan_workspace_rename). For aliased imports we still rewrite
            // the `import_name` token (matching the open-importer path) but leave the
            // alias binding and its usages alone.
            if let Some(root) =
                super::workspace::find_workspace_root_for_uri(&import_match.imported_uri)
            {
                let open_uris: HashSet<Url> = documents.iter().map(|e| e.key().clone()).collect();
                // Propagate I/O errors from unopened-importer discovery so the caller
                // can refuse the rename if a disk scan fails, matching the policy
                // used in plan_workspace_rename (#1288).
                let unopened_importers = collect_unopened_sibling_importers_for_edits(
                    &import_match.imported_uri,
                    &import_match.imported_name,
                    &root,
                    &open_uris,
                )?;
                for unopened in unopened_importers {
                    changes
                        .entry(unopened.importer_uri.clone())
                        .or_default()
                        .push(hew_analysis::RenameEdit {
                            span: unopened.import_name_span,
                            new_text: new_name.to_string(),
                        });

                    // Load the unopened file and compute edits for import-binding references.
                    // Skip reference walk for aliased imports (same constraint as open-importer loop at L715-717).
                    let unopened_source = read_source_or_io_error(&unopened.importer_uri)?;
                    let unopened_parse = hew_parser::parse(&unopened_source);
                    let unopened_edits: Vec<_> =
                        collect_importer_fanout_spans(&unopened_parse, &unopened)
                            .into_iter()
                            .skip(1)
                            .map(|span| hew_analysis::RenameEdit {
                                span,
                                new_text: new_name.to_string(),
                            })
                            .collect();
                    if !unopened_edits.is_empty() {
                        changes
                            .entry(unopened.importer_uri.clone())
                            .or_default()
                            .extend(unopened_edits);
                    }
                }
            }
        }

        return workspace_edit_from_changes(uri, doc, documents, changes);
    }

    let mut changes: HashMap<Url, Vec<hew_analysis::RenameEdit>> = HashMap::new();
    let local_edits = collect_local_rename_edits(doc, offset, new_name)?;
    if !local_edits.is_empty() {
        changes.insert(uri.clone(), local_edits);
    }

    if hew_analysis::references::is_top_level_name(&doc.parse_result, &name) {
        for importer in indexed_named_importers(&importer_index, uri, &name) {
            if let Some(importer_doc) = documents.get(&importer.importer_uri) {
                let importer_edits: Vec<_> =
                    collect_importer_fanout_spans(&importer_doc.parse_result, importer)
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

        // Include edits for unopened sibling importers (symmetric to the
        // import-originated path above). Definition-file renaming should also
        // update unopened importers.
        if let Some(root) = super::workspace::find_workspace_root_for_uri(uri) {
            let open_uris: HashSet<Url> = documents.iter().map(|e| e.key().clone()).collect();
            // Propagate I/O errors from unopened-importer discovery (#1288).
            let unopened_importers =
                collect_unopened_sibling_importers_for_edits(uri, &name, &root, &open_uris)?;
            for unopened in unopened_importers {
                changes
                    .entry(unopened.importer_uri.clone())
                    .or_default()
                    .push(hew_analysis::RenameEdit {
                        span: unopened.import_name_span,
                        new_text: new_name.to_string(),
                    });

                // Load the unopened file and compute edits for import-binding references.
                // Skip reference walk for aliased imports (same constraint as open-importer loop at L715-717).
                let unopened_source = read_source_or_io_error(&unopened.importer_uri)?;
                let unopened_parse = hew_parser::parse(&unopened_source);
                let unopened_edits: Vec<_> =
                    collect_importer_fanout_spans(&unopened_parse, &unopened)
                        .into_iter()
                        .skip(1)
                        .map(|span| hew_analysis::RenameEdit {
                            span,
                            new_text: new_name.to_string(),
                        })
                        .collect();
                if !unopened_edits.is_empty() {
                    changes
                        .entry(unopened.importer_uri.clone())
                        .or_default()
                        .extend(unopened_edits);
                }
            }
        }
    }

    workspace_edit_from_changes(uri, doc, documents, changes)
}

fn collect_import_originated_rename_conflicts(
    uri: &Url,
    import_match: &NamedImportMatch,
    new_name: &str,
    documents: &DashMap<Url, DocumentState>,
    importer_index: &NamedImporterIndex,
    cross_file_conflicts: &mut Vec<hew_analysis::RenameConflict>,
) -> Result<(), hew_analysis::RenameError> {
    if import_match.is_aliased() {
        return Ok(());
    }

    if let Some(target_doc) = documents.get(&import_match.imported_uri) {
        collect_cross_file_conflict(
            &target_doc,
            new_name,
            &import_match.imported_name,
            cross_file_conflicts,
        );

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
    } else {
        let source = read_source_or_io_error(&import_match.imported_uri)?;
        let parse_result = hew_parser::parse(&source);
        collect_cross_file_conflict_raw(
            &source,
            &parse_result,
            new_name,
            &import_match.imported_name,
            cross_file_conflicts,
        );

        if let Some(def_span) = hew_analysis::definition::find_definition(
            &source,
            &parse_result,
            &import_match.imported_name,
        ) {
            if let Err(hew_analysis::RenameError::Conflicts { conflicts }) =
                hew_analysis::rename::plan_rename(&source, &parse_result, def_span.start, new_name)
            {
                cross_file_conflicts.extend(conflicts);
            }
        }
    }

    for importer in indexed_named_importers(
        importer_index,
        &import_match.imported_uri,
        &import_match.imported_name,
    ) {
        if importer.importer_uri == *uri {
            continue;
        }
        if let Some(importer_doc) = documents.get(&importer.importer_uri) {
            collect_cross_file_conflict(
                &importer_doc,
                new_name,
                &importer.visible_name,
                cross_file_conflicts,
            );
        }
    }

    if let Some(root) = super::workspace::find_workspace_root_for_uri(&import_match.imported_uri) {
        let open_uris: HashSet<Url> = documents.iter().map(|e| e.key().clone()).collect();
        scan_disk_importers_for_conflicts(
            &import_match.imported_uri,
            &import_match.imported_name,
            new_name,
            &root,
            &open_uris,
            cross_file_conflicts,
        )?;
    }

    Ok(())
}

fn collect_definition_originated_rename_conflicts(
    uri: &Url,
    name: &str,
    new_name: &str,
    documents: &DashMap<Url, DocumentState>,
    importer_index: &NamedImporterIndex,
    cross_file_conflicts: &mut Vec<hew_analysis::RenameConflict>,
) -> Result<(), hew_analysis::RenameError> {
    for importer in indexed_named_importers(importer_index, uri, name) {
        if importer.is_aliased() {
            continue;
        }
        if let Some(importer_doc) = documents.get(&importer.importer_uri) {
            collect_cross_file_conflict(
                &importer_doc,
                new_name,
                &importer.visible_name,
                cross_file_conflicts,
            );
        }
    }

    if let Some(root) = super::workspace::find_workspace_root_for_uri(uri) {
        let open_uris: HashSet<Url> = documents.iter().map(|e| e.key().clone()).collect();
        scan_disk_importers_for_conflicts(
            uri,
            name,
            new_name,
            &root,
            &open_uris,
            cross_file_conflicts,
        )?;
    }

    Ok(())
}

fn dedup_cross_file_conflicts(
    conflicts: Vec<hew_analysis::RenameConflict>,
) -> Vec<hew_analysis::RenameConflict> {
    let mut deduped = Vec::with_capacity(conflicts.len());
    for conflict in conflicts {
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
    deduped
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
    let importer_index = build_named_importer_index(documents);

    match hew_analysis::rename::plan_rename(&doc.source, &doc.parse_result, offset, new_name) {
        Ok(_edits) => {}
        Err(err) => return Err(err),
    }

    let mut cross_file_conflicts: Vec<hew_analysis::RenameConflict> = Vec::new();
    let Some((name, _)) = hew_analysis::util::simple_word_at_offset(&doc.source, offset) else {
        return Ok(None);
    };

    if name == new_name {
        return Ok(None);
    }

    if let Some((import_match, _)) =
        find_resolved_named_import_match(uri, doc, offset, &name, documents)
    {
        collect_import_originated_rename_conflicts(
            uri,
            &import_match,
            new_name,
            documents,
            &importer_index,
            &mut cross_file_conflicts,
        )?;
    } else if hew_analysis::references::is_top_level_name(&doc.parse_result, &name) {
        collect_definition_originated_rename_conflicts(
            uri,
            &name,
            new_name,
            documents,
            &importer_index,
            &mut cross_file_conflicts,
        )?;
    }

    cross_file_conflicts = dedup_cross_file_conflicts(cross_file_conflicts);

    if !cross_file_conflicts.is_empty() {
        return Err(hew_analysis::RenameError::Conflicts {
            conflicts: cross_file_conflicts,
        });
    }

    build_workspace_edit(uri, doc, offset, new_name, documents)
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
    collect_cross_file_conflict_raw(
        &other_doc.source,
        &other_doc.parse_result,
        new_name,
        offending_visible_name,
        conflicts,
    );
}

/// Core conflict-detection logic over raw `(source, parse_result)`.  Called
/// from both `collect_cross_file_conflict` (open docs) and the disk-importer
/// scanner (#1285).
fn collect_cross_file_conflict_raw(
    source: &str,
    parse_result: &ParseResult,
    new_name: &str,
    offending_visible_name: &str,
    conflicts: &mut Vec<hew_analysis::RenameConflict>,
) {
    if hew_analysis::references::is_top_level_name(parse_result, new_name) {
        if let Some(existing) =
            hew_analysis::definition::find_definition(source, parse_result, new_name)
        {
            let offending = hew_analysis::definition::find_definition(
                source,
                parse_result,
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
        hew_analysis::resolver::find_matching_import(parse_result, new_name)
    {
        let offending =
            hew_analysis::definition::find_definition(source, parse_result, offending_visible_name)
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
        parse_result,
        offending_visible_name,
    );
    for usage in usage_spans {
        if let Some(existing) = hew_analysis::definition::find_local_binding_definition(
            source,
            parse_result,
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
        if let Some(existing) =
            hew_analysis::definition::find_param_definition(parse_result, new_name, usage.start)
        {
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

// ── Workspace-disk importer scan (#1285, #1287, #1288, #1290) ────────

/// Walk every `*.hew` file under `root` that is NOT in `open_uris`, parse it
/// on-demand, check whether it imports the renamed symbol from `definition_uri`,
/// and if so run `collect_cross_file_conflict_raw` against it.
///
/// Uses the shared `for_each_hew_file` walker from workspace.rs, which:
/// - Skips symlinked directories via `symlink_metadata` (no cycles, #1290).
/// - Applies `should_skip_workspace_dir` for `.git`, `target`, etc.
/// - Propagates I/O errors as `RenameError::Io` rather than silently
///   swallowing them (#1288).
///
/// SHIM: `O(#disk_files)` per rename; acceptable because rename is
/// user-initiated and infrequent.  A proper solution would maintain an index
/// of importers.
fn scan_disk_importers_for_conflicts(
    definition_uri: &Url,
    renamed_name: &str,
    new_name: &str,
    root: &std::path::Path,
    open_uris: &HashSet<Url>,
    conflicts: &mut Vec<hew_analysis::RenameConflict>,
) -> Result<(), hew_analysis::RenameError> {
    super::workspace::for_each_hew_file(root, |path| -> Result<(), hew_analysis::RenameError> {
        let Ok(file_uri) = Url::from_file_path(path) else {
            return Ok(());
        };
        // Already checked by the open-documents pass.
        if open_uris.contains(&file_uri) || file_uri == *definition_uri {
            return Ok(());
        }
        let source = std::fs::read_to_string(path)
            .map_err(|e| hew_analysis::RenameError::from((path.to_path_buf(), e)))?;
        let parse_result = hew_parser::parse(&source);

        // Only proceed if this file imports `renamed_name` (non-aliased) from
        // `definition_uri`.  Aliased importers (`import foo::{ x as y }`) only
        // rewrite the imported token — the visible binding remains the alias, so
        // they cannot introduce a `renamed_name`-visible conflict (#1285 quality).
        let imports_target_nonaliased =
            collect_import_items(&parse_result)
                .into_iter()
                .any(|(import, _)| {
                    let Some(ImportSpec::Names(names)) = &import.spec else {
                        return false;
                    };
                    if !names
                        .iter()
                        .any(|n| n.name == renamed_name && n.alias.is_none())
                    {
                        return false;
                    }
                    let Some(resolved) = compute_import_path(&file_uri, &import) else {
                        return false;
                    };
                    Url::from_file_path(&resolved).ok().as_ref() == Some(definition_uri)
                });
        if !imports_target_nonaliased {
            return Ok(());
        }

        collect_cross_file_conflict_raw(&source, &parse_result, new_name, renamed_name, conflicts);
        Ok(())
    })
}

/// Collect unopened files on disk that import a symbol from `definition_uri`.
///
/// Used to extend the edit set in [`build_workspace_edit`] for unopened sibling
/// importers.  I/O errors propagate as `RenameError::Io` (#1288).
fn collect_unopened_sibling_importers_for_edits(
    definition_uri: &Url,
    renamed_name: &str,
    root: &std::path::Path,
    open_uris: &HashSet<Url>,
) -> Result<Vec<NamedImportMatch>, hew_analysis::RenameError> {
    let mut matches = Vec::new();
    super::workspace::for_each_hew_file(root, |path| -> Result<(), hew_analysis::RenameError> {
        let Ok(file_uri) = Url::from_file_path(path) else {
            return Ok(());
        };
        if open_uris.contains(&file_uri) || file_uri == *definition_uri {
            return Ok(());
        }
        let source = std::fs::read_to_string(path)
            .map_err(|e| hew_analysis::RenameError::from((path.to_path_buf(), e)))?;
        let parse_result = hew_parser::parse(&source);

        // Collect all imports (aliased and non-aliased) for the unopened-importer
        // edits. The loop consuming these matches will emit import-name edits for
        // both, but only walk references for non-aliased (same constraint as the
        // open-importer loop at the call sites).
        for (import, item_span) in collect_import_items(&parse_result) {
            let Some(ImportSpec::Names(names)) = &import.spec else {
                continue;
            };
            let Some(resolved) = compute_import_path(&file_uri, &import) else {
                continue;
            };
            let Ok(resolved_uri) = Url::from_file_path(&resolved) else {
                continue;
            };
            if resolved_uri != *definition_uri {
                continue;
            }

            for import_name in names {
                if import_name.name != renamed_name {
                    continue;
                }
                let Some((import_name_span, visible_name_span)) =
                    find_named_import_spans(&source, &item_span, import_name)
                else {
                    continue;
                };
                let visible_name = import_name
                    .alias
                    .clone()
                    .unwrap_or_else(|| import_name.name.clone());
                matches.push(NamedImportMatch {
                    importer_uri: file_uri.clone(),
                    imported_uri: resolved_uri.clone(),
                    imported_name: import_name.name.clone(),
                    visible_name,
                    import_name_span,
                    visible_name_span,
                });
            }
        }
        Ok(())
    })?;
    Ok(matches)
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

        let result = if let Some(doc) = documents.get(&target_uri) {
            if let Some(range) = find_definition_in_ast(
                &doc.source,
                &doc.line_offsets,
                &doc.parse_result,
                search_name,
            ) {
                Some((target_uri.clone(), range))
            } else if remaining_hops == 0 {
                None
            } else {
                let nested_imports = collect_import_items(&doc.parse_result)
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
            }
        } else {
            load_navigation_target(&path).and_then(|(source, line_offsets, parse_result)| {
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
            })
        };

        seen.remove(&target_uri);

        if result.is_some() {
            return result;
        }
    }
    None
}

fn load_navigation_target(path: &std::path::Path) -> Option<(String, Vec<usize>, ParseResult)> {
    if path.exists() {
        if let Ok(source) = std::fs::read_to_string(path) {
            let parse_result = hew_parser::parse(&source);
            let line_offsets = compute_line_offsets(&source);
            return Some((source, line_offsets, parse_result));
        }
    }

    None
}

// ── Stdlib / workspace-wide navigation ───────────────────────────────

/// Search the workspace's stdlib tree and all `.hew` files for a definition of
/// `word`, without requiring an explicit `import` declaration in the caller.
///
/// This covers builtin functions defined in `std/builtins.hew` (e.g. `println`)
/// that are always in scope but never imported.  It falls back gracefully when
/// no workspace root exists or when the symbol is a Rust-registered type-method
/// builtin with no `.hew` source.
///
/// SHIM: `O(#workspace_files)` per goto-def miss. Acceptable for now — goto-def
/// misses are rare in practice and `collect_hew_files` already exists for
/// rename. A proper solution would add a workspace-level symbol index populated
/// at startup.
pub(super) fn find_stdlib_definition(
    current_uri: &Url,
    imports: &[hew_parser::ast::ImportDecl],
    word: &str,
    documents: &DashMap<Url, DocumentState>,
) -> Option<(Url, Range)> {
    let root = find_workspace_root_for_uri(current_uri)?;

    // Build the set of files already searched by the import-gated path so we
    // don't duplicate work.  We include the current file and any files
    // reachable via explicit imports.
    let mut already_searched: HashSet<Url> = HashSet::from([current_uri.clone()]);
    for import in imports {
        if let Some(path) = compute_import_path(current_uri, import) {
            if let Ok(u) = Url::from_file_path(&path) {
                already_searched.insert(u);
            }
        }
    }

    // Collect all workspace .hew files and check each one for a definition.
    // collect_hew_files is best-effort (I/O errors silently skipped) which is
    // correct here: a parse error in one stdlib file must not block navigation
    // for the whole workspace.
    let workspace_files = super::workspace::collect_hew_files(&root);
    for path in workspace_files {
        let Ok(file_uri) = Url::from_file_path(&path) else {
            continue;
        };
        if already_searched.contains(&file_uri) {
            continue;
        }

        if let Some(doc) = documents.get(&file_uri) {
            if let Some(range) =
                find_definition_in_ast(&doc.source, &doc.line_offsets, &doc.parse_result, word)
            {
                return Some((file_uri.clone(), range));
            }
        } else if let Some((source, line_offsets, parse_result)) = load_navigation_target(&path) {
            if let Some(range) = find_definition_in_ast(&source, &line_offsets, &parse_result, word)
            {
                return Some((file_uri.clone(), range));
            }
        }
    }

    None
}

/// Collect all occurrences of `name` across every `.hew` file in the workspace,
/// returning `Location` values for each call site.
///
/// This is used by `build_reference_locations` when `name` is a stdlib/builtin
/// symbol not found via the import-gated path.  Only fires when `name` is
/// confirmed to have a definition somewhere in the workspace (checked by the
/// caller).
///
/// SHIM: `O(#files × #tokens)` per call. Acceptable for now — references is
/// user-initiated and infrequent. A proper solution would maintain an inverted
/// index at startup.
fn collect_workspace_references(
    current_uri: &Url,
    name: &str,
    include_declaration: bool,
    documents: &DashMap<Url, DocumentState>,
) -> Vec<Location> {
    let Some(root) = find_workspace_root_for_uri(current_uri) else {
        return Vec::new();
    };

    let workspace_files = super::workspace::collect_hew_files(&root);
    let mut locations = Vec::new();

    for path in workspace_files {
        let Ok(file_uri) = Url::from_file_path(&path) else {
            continue;
        };

        if let Some(doc) = documents.get(&file_uri) {
            // For the definition file: include the definition span if requested.
            if include_declaration {
                if let Some(def_span) =
                    hew_analysis::definition::find_definition(&doc.source, &doc.parse_result, name)
                {
                    push_location_for_span(
                        &mut locations,
                        &file_uri,
                        &doc.source,
                        &doc.line_offsets,
                        def_span,
                    );
                }
            }
            // Collect all call-site references in this file.
            let ref_spans = hew_analysis::references::find_all_name_occurrences(
                &doc.source,
                &doc.parse_result,
                name,
            );
            for span in ref_spans {
                push_location_for_span(
                    &mut locations,
                    &file_uri,
                    &doc.source,
                    &doc.line_offsets,
                    span,
                );
            }
        } else if let Some((source, line_offsets, parse_result)) = load_navigation_target(&path) {
            if include_declaration {
                if let Some(def_span) =
                    hew_analysis::definition::find_definition(&source, &parse_result, name)
                {
                    push_location_for_span(
                        &mut locations,
                        &file_uri,
                        &source,
                        &line_offsets,
                        def_span,
                    );
                }
            }
            let ref_spans =
                hew_analysis::references::find_all_name_occurrences(&source, &parse_result, name);
            for span in ref_spans {
                push_location_for_span(&mut locations, &file_uri, &source, &line_offsets, span);
            }
        }
    }

    locations
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

#[cfg(test)]
mod tests {
    use super::*;

    fn make_doc(source: &str) -> DocumentState {
        DocumentState {
            source: source.to_string(),
            line_offsets: compute_line_offsets(source),
            parse_result: hew_parser::parse(source),
            type_output: None,
            diagnostics_by_uri: HashMap::new(),
        }
    }

    fn make_test_uri(posix_path: &str) -> Url {
        #[cfg(windows)]
        return Url::parse(&format!("file:///C:{posix_path}")).unwrap();
        #[cfg(not(windows))]
        return Url::parse(&format!("file://{posix_path}")).unwrap();
    }

    fn first_named_import_match(
        source: &str,
        source_uri: &Url,
        target_uri: &Url,
    ) -> NamedImportMatch {
        let parse_result = hew_parser::parse(source);
        let (import, span) = collect_import_items(&parse_result)
            .into_iter()
            .next()
            .expect("expected import item");
        let Some(ImportSpec::Names(names)) = import.spec else {
            panic!("expected named import");
        };
        let import_name = names.first().expect("expected imported name");
        let (import_name_span, visible_name_span) =
            find_named_import_spans(source, &span, import_name).expect("expected import spans");
        NamedImportMatch {
            importer_uri: source_uri.clone(),
            imported_uri: target_uri.clone(),
            imported_name: import_name.name.clone(),
            visible_name: import_name
                .alias
                .as_deref()
                .unwrap_or(import_name.name.as_str())
                .to_string(),
            import_name_span,
            visible_name_span,
        }
    }

    #[test]
    fn importer_fanout_helper_includes_usage_spans_for_non_aliased_imports() {
        let source = "import util::{ greet };\nfn main() -> i32 { greet() }";
        let source_uri = make_test_uri("/project/main.hew");
        let target_uri = make_test_uri("/project/util.hew");
        let importer = first_named_import_match(source, &source_uri, &target_uri);
        let parse_result = hew_parser::parse(source);

        let spans = collect_importer_fanout_spans(&parse_result, &importer);
        assert_eq!(spans.len(), 2, "expected import token + usage span");
        assert_eq!(spans[0], importer.import_name_span);
        assert!(spans[1].start > importer.import_name_span.end);
    }

    #[test]
    fn importer_fanout_helper_skips_usage_spans_for_aliased_imports() {
        let source = "import util::{ greet as hello };\nfn main() -> i32 { hello() }";
        let source_uri = make_test_uri("/project/main.hew");
        let target_uri = make_test_uri("/project/util.hew");
        let importer = first_named_import_match(source, &source_uri, &target_uri);
        let parse_result = hew_parser::parse(source);

        let spans = collect_importer_fanout_spans(&parse_result, &importer);
        assert_eq!(spans, vec![importer.import_name_span]);
    }

    #[test]
    fn import_originated_rename_conflicts_detect_definition_file_clash() {
        let main_source = "import util::{ greet };\nfn main() -> i32 { greet() }";
        let util_source = "pub fn greet() -> i32 { 1 }\npub fn hello() -> i32 { 2 }";
        let main_uri = make_test_uri("/project/main.hew");
        let util_uri = make_test_uri("/project/util.hew");
        let documents: DashMap<Url, DocumentState> = DashMap::new();
        documents.insert(main_uri.clone(), make_doc(main_source));
        documents.insert(util_uri.clone(), make_doc(util_source));

        let main_doc = documents.get(&main_uri).expect("main doc");
        let (import_match, _) = find_resolved_named_import_match(
            &main_uri,
            &main_doc,
            main_source.find("greet").expect("import token"),
            "greet",
            &documents,
        )
        .expect("expected resolved import match");
        let importer_index = build_named_importer_index(&documents);
        let mut conflicts = Vec::new();

        collect_import_originated_rename_conflicts(
            &main_uri,
            &import_match,
            "hello",
            &documents,
            &importer_index,
            &mut conflicts,
        )
        .expect("helper should complete");

        assert!(conflicts.iter().any(|conflict| {
            conflict.kind == hew_analysis::RenameConflictKind::ShadowsTopLevel
        }));
    }

    // ── stdlib navigation tests ───────────────────────────────────────────────

    /// Build a temporary workspace with the given (`relative_path`, content) pairs.
    /// Returns the workspace root and the list of file URIs.
    fn make_temp_workspace(files: &[(&str, &str)]) -> (std::path::PathBuf, Vec<Url>) {
        let root = std::env::temp_dir().join(format!(
            "hew-nav-test-{}",
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .map_or(0, |d| d.subsec_nanos())
        ));
        let mut uris = Vec::new();
        for (rel, content) in files {
            let path = root.join(rel);
            if let Some(parent) = path.parent() {
                std::fs::create_dir_all(parent).expect("create dir");
            }
            std::fs::write(&path, content).expect("write file");
            uris.push(Url::from_file_path(&path).expect("valid path"));
        }
        (root, uris)
    }

    #[test]
    fn goto_def_stdlib_returns_location_in_builtins() {
        // Workspace: main.hew uses println (no import needed — it's a stdlib builtin)
        // std/builtins.hew defines println
        let main_src = "fn main() {\n    println(42);\n}\n";
        let builtins_src = "pub fn println(value: dyn Display) {}\n";

        let (root, uris) =
            make_temp_workspace(&[("std/builtins.hew", builtins_src), ("main.hew", main_src)]);
        let main_uri = &uris[1];

        let documents: DashMap<Url, DocumentState> = DashMap::new();
        documents.insert(main_uri.clone(), make_doc(main_src));

        // offset of `println` in main_src: "fn main() {\n    " = 17 chars, then `println`
        let offset = main_src.find("println").expect("println in source");
        let word = "println";
        let imports: Vec<hew_parser::ast::ImportDecl> = Vec::new(); // no imports

        let result = find_stdlib_definition(main_uri, &imports, word, &documents);

        std::fs::remove_dir_all(&root).ok();

        let (target_uri, _range) = result.expect("should find println in stdlib");
        assert!(
            target_uri.path().contains("builtins"),
            "expected builtins.hew, got: {target_uri}"
        );
        let _ = offset; // used to verify the test setup
    }

    #[test]
    fn find_references_stdlib_println_returns_cross_file_locations() {
        // Workspace: main.hew + other.hew both call println, std/builtins.hew defines it
        let main_src = "fn main() {\n    println(1);\n    println(2);\n}\n";
        let other_src = "fn run() {\n    println(3);\n}\n";
        let builtins_src = "pub fn println(value: dyn Display) {}\n";

        let (root, uris) = make_temp_workspace(&[
            ("std/builtins.hew", builtins_src),
            ("main.hew", main_src),
            ("other.hew", other_src),
        ]);
        let main_uri = &uris[1];
        let other_uri = &uris[2];

        let documents: DashMap<Url, DocumentState> = DashMap::new();
        documents.insert(main_uri.clone(), make_doc(main_src));
        documents.insert(other_uri.clone(), make_doc(other_src));

        // offset of first `println` in main_src
        let offset = main_src.find("println").expect("println in source");
        let main_doc = documents.get(main_uri).expect("main doc");

        let locations = build_reference_locations(main_uri, &main_doc, offset, true, &documents);

        drop(main_doc);
        std::fs::remove_dir_all(&root).ok();

        // Must have at least 2 cross-file locations (main.hew + other.hew)
        assert!(
            locations.len() >= 2,
            "expected >=2 locations for stdlib println, got {}: {locations:?}",
            locations.len()
        );
        let uris_found: Vec<_> = locations.iter().map(|l| l.uri.as_str()).collect();
        let has_cross_file = uris_found
            .iter()
            .any(|u| u.contains("other") || u.contains("builtins"));
        assert!(
            has_cross_file,
            "expected at least one cross-file location, got: {uris_found:?}"
        );
    }

    #[test]
    fn definition_originated_rename_conflicts_detect_importer_shadow() {
        let util_source = "pub fn foo() -> i32 { 1 }";
        let main_source =
            "import util::{ foo };\nimport other::{ bar };\nfn main() -> i32 { foo() + bar() }";
        let other_source = "pub fn bar() -> i32 { 2 }";
        let util_uri = make_test_uri("/project/util.hew");
        let main_uri = make_test_uri("/project/main.hew");
        let other_uri = make_test_uri("/project/other.hew");
        let documents: DashMap<Url, DocumentState> = DashMap::new();
        documents.insert(util_uri.clone(), make_doc(util_source));
        documents.insert(main_uri.clone(), make_doc(main_source));
        documents.insert(other_uri, make_doc(other_source));

        let importer_index = build_named_importer_index(&documents);
        let mut conflicts = Vec::new();
        collect_definition_originated_rename_conflicts(
            &util_uri,
            "foo",
            "bar",
            &documents,
            &importer_index,
            &mut conflicts,
        )
        .expect("helper should complete");

        assert!(conflicts
            .iter()
            .any(|conflict| { conflict.kind == hew_analysis::RenameConflictKind::ShadowsImport }));
    }
}
