use tower_lsp::lsp_types::{
    CallHierarchyIncomingCall, CallHierarchyIncomingCallsParams, CallHierarchyItem,
    CallHierarchyOutgoingCall, CallHierarchyOutgoingCallsParams, CallHierarchyPrepareParams,
    TypeHierarchyItem, TypeHierarchyPrepareParams, TypeHierarchySubtypesParams,
    TypeHierarchySupertypesParams,
};

use super::super::{
    collect_subtypes, collect_supertypes, find_callable_at, find_incoming_calls,
    find_outgoing_calls, find_type_hierarchy_item, non_empty, position_to_offset, word_at_offset,
    HewLanguageServer,
};

pub(crate) fn prepare_type_hierarchy(
    server: &HewLanguageServer,
    params: &TypeHierarchyPrepareParams,
) -> Option<Vec<TypeHierarchyItem>> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;
    let doc = server.documents.get(uri)?;

    let offset = position_to_offset(&doc.source, &doc.line_offsets, position);
    let word = word_at_offset(&doc.source, offset)?;

    let item = find_type_hierarchy_item(
        uri,
        &doc.source,
        &doc.line_offsets,
        &doc.parse_result,
        &word,
    );
    item.map(|i| vec![i])
}

pub(crate) fn supertypes(
    server: &HewLanguageServer,
    params: &TypeHierarchySupertypesParams,
) -> Option<Vec<TypeHierarchyItem>> {
    let item = &params.item;
    let doc = server.documents.get(&item.uri)?;

    let supers = collect_supertypes(
        &item.uri,
        &item.name,
        &doc.source,
        &doc.line_offsets,
        &doc.parse_result,
    );
    non_empty(supers)
}

pub(crate) fn subtypes(
    server: &HewLanguageServer,
    params: &TypeHierarchySubtypesParams,
) -> Option<Vec<TypeHierarchyItem>> {
    let item = &params.item;
    let doc = server.documents.get(&item.uri)?;

    let subs = collect_subtypes(
        &item.uri,
        &item.name,
        &doc.source,
        &doc.line_offsets,
        &doc.parse_result,
    );
    non_empty(subs)
}

pub(crate) fn prepare_call_hierarchy(
    server: &HewLanguageServer,
    params: &CallHierarchyPrepareParams,
) -> Option<Vec<CallHierarchyItem>> {
    let uri = &params.text_document_position_params.text_document.uri;
    let pos = params.text_document_position_params.position;
    let doc = server.documents.get(uri)?;

    let offset = position_to_offset(&doc.source, &doc.line_offsets, pos);
    let word = word_at_offset(&doc.source, offset)?;

    let item = find_callable_at(
        uri,
        &doc.source,
        &doc.line_offsets,
        &doc.parse_result,
        &word,
    );
    item.map(|it| vec![it])
}

pub(crate) fn incoming_calls(
    server: &HewLanguageServer,
    params: &CallHierarchyIncomingCallsParams,
) -> Option<Vec<CallHierarchyIncomingCall>> {
    let item = &params.item;
    let doc = server.documents.get(&item.uri)?;

    let calls = find_incoming_calls(
        &item.uri,
        &doc.source,
        &doc.line_offsets,
        &doc.parse_result,
        &item.name,
    );
    non_empty(calls)
}

pub(crate) fn outgoing_calls(
    server: &HewLanguageServer,
    params: &CallHierarchyOutgoingCallsParams,
) -> Option<Vec<CallHierarchyOutgoingCall>> {
    let item = &params.item;
    let doc = server.documents.get(&item.uri)?;

    let calls = find_outgoing_calls(
        &item.uri,
        &doc.source,
        &doc.line_offsets,
        &doc.parse_result,
        &item.name,
    );
    non_empty(calls)
}
