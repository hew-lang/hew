use tower_lsp_server::jsonrpc::{Error, ErrorCode, Result};
use tower_lsp_server::ls_types::{
    GotoDefinitionParams, GotoDefinitionResponse, Location, PrepareRenameResponse, ReferenceParams,
    RenameParams, WorkspaceEdit,
};

use super::super::{
    collect_import_items, find_cross_file_definition, find_definition_in_ast,
    find_stdlib_definition, non_empty, offset_range_to_lsp, position_to_offset, word_at_offset,
    DocumentState, HewLanguageServer,
};

fn source_definition_location(
    uri: &tower_lsp_server::ls_types::Uri,
    doc: &DocumentState,
    word: &str,
    offset: usize,
) -> Option<Location> {
    let span = hew_analysis::definition::find_local_binding_definition(
        &doc.source,
        &doc.parse_result,
        word,
        offset,
    )
    .or_else(|| hew_analysis::definition::find_param_definition(&doc.parse_result, word, offset));
    let range = if let Some(span) = span {
        offset_range_to_lsp(&doc.source, &doc.line_offsets, span.start, span.end)
    } else {
        find_definition_in_ast(&doc.source, &doc.line_offsets, &doc.parse_result, word)?
    };
    Some(Location {
        uri: uri.clone(),
        range,
    })
}

pub(crate) fn rename_error_to_jsonrpc(
    err: &hew_analysis::RenameError,
) -> tower_lsp_server::jsonrpc::Error {
    let message: String = match err {
        hew_analysis::RenameError::InvalidIdentifier { message, .. }
        | hew_analysis::RenameError::Builtin { message, .. } => message.clone(),
        hew_analysis::RenameError::Conflicts { conflicts } => {
            let first = conflicts
                .first()
                .map_or_else(|| "rename conflict".to_string(), |c| c.message.clone());
            if conflicts.len() > 1 {
                format!("{first} (+{} more)", conflicts.len() - 1)
            } else {
                first
            }
        }
        hew_analysis::RenameError::Io { path, message } => {
            format!("rename failed: {path}: {message}")
        }
        _ => "rename failed".to_string(),
    };
    Error {
        code: ErrorCode::ServerError(-32803),
        message: message.into(),
        data: None,
    }
}

pub(crate) fn goto_definition(
    server: &HewLanguageServer,
    params: &GotoDefinitionParams,
) -> Option<GotoDefinitionResponse> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    let doc = server.documents.get(uri)?;

    let offset = position_to_offset(&doc.source, &doc.line_offsets, position);
    if let Some(location) =
        super::super::navigation::identity_definition_location(uri, &doc, offset, &server.documents)
    {
        return Some(GotoDefinitionResponse::Scalar(location));
    }
    if let Some(output) = &doc.type_output {
        if let Some((_, resolution)) = hew_analysis::identity::resolution_at(output, 0, offset) {
            use hew_types::check::scope::Resolution;
            let authored_declaration = match resolution {
                Resolution::Def(id) | Resolution::Member(id) => output.defs.site(id).is_some(),
                Resolution::Nominal(id) => output.defs.site(id.declaration()).is_some(),
                Resolution::Local(_) | Resolution::Field(_, _) => true,
                Resolution::Param(_)
                | Resolution::Module(_)
                | Resolution::Builtin(_)
                | Resolution::Variant(_, _) => false,
            };
            if authored_declaration {
                // The checker made a positive identity claim. A missing source
                // location is a mapping gap, not permission to pick a namesake.
                return None;
            }
        }
    }
    let word = word_at_offset(&doc.source, offset)?;

    if let Some(location) = source_definition_location(uri, &doc, &word, offset) {
        return Some(GotoDefinitionResponse::Scalar(location));
    }

    if let Some(method) = word.rsplit('.').next() {
        if method != word {
            if let Some(range) =
                find_definition_in_ast(&doc.source, &doc.line_offsets, &doc.parse_result, method)
            {
                return Some(GotoDefinitionResponse::Scalar(Location {
                    uri: uri.clone(),
                    range,
                }));
            }
        }
    }

    let imports: Vec<hew_parser::ast::ImportDecl> = collect_import_items(&doc.parse_result)
        .into_iter()
        .map(|(import, _)| import)
        .collect();

    // Compute before dropping doc: locals and params are fully resolved in the
    // current file; the stdlib workspace fallback must not redirect goto-def on
    // them to an unrelated top-level definition in another file.
    let is_local_or_param = hew_analysis::definition::find_local_binding_definition(
        &doc.source,
        &doc.parse_result,
        &word,
        offset,
    )
    .is_some()
        || hew_analysis::definition::find_param_definition(&doc.parse_result, &word, offset)
            .is_some();
    drop(doc);

    if let Some((target_uri, range)) =
        find_cross_file_definition(uri, &imports, &word, &server.documents)
    {
        return Some(GotoDefinitionResponse::Scalar(Location {
            uri: target_uri,
            range,
        }));
    }

    if let Some((prefix, _)) = word.split_once('.') {
        if let Some((target_uri, range)) =
            find_cross_file_definition(uri, &imports, prefix, &server.documents)
        {
            return Some(GotoDefinitionResponse::Scalar(Location {
                uri: target_uri,
                range,
            }));
        }
    }

    // Fallback: search the whole workspace for a definition.  This catches
    // stdlib builtins (e.g. `println`) that are always in scope but never
    // explicitly imported, so the import-gated paths above will not find them.
    // Skip for locally-bound names and params: those are resolved in-file and
    // must not jump to an unrelated top-level definition in another file.
    if !is_local_or_param {
        if let Some((target_uri, range)) =
            find_stdlib_definition(uri, &imports, &word, &server.documents)
        {
            return Some(GotoDefinitionResponse::Scalar(Location {
                uri: target_uri,
                range,
            }));
        }
    }

    None
}

pub(crate) fn references(
    server: &HewLanguageServer,
    params: &ReferenceParams,
) -> Option<Vec<Location>> {
    let uri = &params.text_document_position.text_document.uri;
    let position = params.text_document_position.position;

    let doc = server.documents.get(uri)?;

    let offset = position_to_offset(&doc.source, &doc.line_offsets, position);
    let include_declaration = params.context.include_declaration;
    let locations = super::super::build_reference_locations(
        uri,
        &doc,
        offset,
        include_declaration,
        &server.documents,
    );
    non_empty(locations)
}

pub(crate) fn prepare_rename(
    server: &HewLanguageServer,
    params: &tower_lsp_server::ls_types::TextDocumentPositionParams,
) -> Option<PrepareRenameResponse> {
    super::super::project_rename::prepare(server, &params.text_document.uri, params.position)
}

pub(crate) fn rename(
    server: &HewLanguageServer,
    params: &RenameParams,
) -> Result<Option<WorkspaceEdit>> {
    let uri = &params.text_document_position.text_document.uri;

    match super::super::project_rename::rename(
        server,
        uri,
        params.text_document_position.position,
        &params.new_name,
    ) {
        Ok(edit) => Ok(edit),
        Err(err) => Err(rename_error_to_jsonrpc(&err)),
    }
}
