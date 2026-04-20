use tower_lsp::jsonrpc::{Error, ErrorCode, Result};
use tower_lsp::lsp_types::{
    GotoDefinitionParams, GotoDefinitionResponse, Location, PrepareRenameResponse, ReferenceParams,
    RenameParams, WorkspaceEdit,
};

use super::super::{
    collect_import_items, find_cross_file_definition, find_definition_in_ast, non_empty,
    offset_range_to_lsp, plan_workspace_rename, position_to_offset, word_at_offset,
    HewLanguageServer,
};

pub(crate) fn rename_error_to_jsonrpc(
    err: &hew_analysis::RenameError,
) -> tower_lsp::jsonrpc::Error {
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
    let word = word_at_offset(&doc.source, offset)?;

    if let Some(resolution) = hew_analysis::resolver::resolve_symbol_at_raw(
        &doc.source,
        &doc.parse_result,
        doc.type_output.as_ref(),
        uri.as_str(),
        offset,
    ) {
        if let Some((_res_uri, span)) = resolution.def_location() {
            let range = offset_range_to_lsp(&doc.source, &doc.line_offsets, span.start, span.end);
            return Some(GotoDefinitionResponse::Scalar(Location {
                uri: uri.clone(),
                range,
            }));
        }
    }

    for separator in [".", "::"] {
        if let Some(method) = word.rsplit(separator).next() {
            if method != word {
                if let Some(range) = find_definition_in_ast(
                    &doc.source,
                    &doc.line_offsets,
                    &doc.parse_result,
                    method,
                ) {
                    return Some(GotoDefinitionResponse::Scalar(Location {
                        uri: uri.clone(),
                        range,
                    }));
                }
            }
        }
    }

    let imports: Vec<hew_parser::ast::ImportDecl> = collect_import_items(&doc.parse_result)
        .into_iter()
        .map(|(import, _)| import)
        .collect();
    drop(doc);

    if let Some((target_uri, range)) =
        find_cross_file_definition(uri, &imports, &word, &server.documents)
    {
        return Some(GotoDefinitionResponse::Scalar(Location {
            uri: target_uri,
            range,
        }));
    }

    for separator in [".", "::"] {
        if let Some((prefix, _)) = word.split_once(separator) {
            if let Some((target_uri, range)) =
                find_cross_file_definition(uri, &imports, prefix, &server.documents)
            {
                return Some(GotoDefinitionResponse::Scalar(Location {
                    uri: target_uri,
                    range,
                }));
            }
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
    params: &tower_lsp::lsp_types::TextDocumentPositionParams,
) -> Option<PrepareRenameResponse> {
    let uri = &params.text_document.uri;

    let doc = server.documents.get(uri)?;

    let offset = position_to_offset(&doc.source, &doc.line_offsets, params.position);
    super::super::build_prepare_rename_response(uri, &doc, offset, &server.documents)
}

pub(crate) fn rename(
    server: &HewLanguageServer,
    params: &RenameParams,
) -> Result<Option<WorkspaceEdit>> {
    let uri = &params.text_document_position.text_document.uri;

    let Some(doc) = server.documents.get(uri) else {
        return Ok(None);
    };

    let offset = position_to_offset(
        &doc.source,
        &doc.line_offsets,
        params.text_document_position.position,
    );
    match plan_workspace_rename(uri, &doc, offset, &params.new_name, &server.documents) {
        Ok(edit) => Ok(edit),
        Err(err) => Err(rename_error_to_jsonrpc(&err)),
    }
}
