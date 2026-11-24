//! LSP server capability advertisement and semantic token legend definitions.

use serde_json::json;
use tower_lsp_server::ls_types::{
    CallHierarchyServerCapability, CodeActionKind, CodeActionOptions, CodeActionProviderCapability,
    CodeLensOptions, CompletionOptions, DiagnosticOptions, DiagnosticServerCapabilities,
    DocumentLinkOptions, ExecuteCommandOptions, FoldingRangeProviderCapability,
    HoverProviderCapability, ImplementationProviderCapability, InlayHintOptions,
    InlayHintServerCapabilities, OneOf, RenameOptions, SelectionRangeProviderCapability,
    SemanticTokenModifier, SemanticTokenType, SemanticTokensFullOptions, SemanticTokensLegend,
    SemanticTokensOptions, SemanticTokensServerCapabilities, ServerCapabilities,
    SignatureHelpOptions, TextDocumentSyncCapability, TextDocumentSyncKind,
    TextDocumentSyncOptions, TextDocumentSyncSaveOptions, TypeDefinitionProviderCapability,
    WorkDoneProgressOptions,
};

// ── Semantic token types ─────────────────────────────────────────────

const TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::KEYWORD,
    SemanticTokenType::TYPE,
    SemanticTokenType::FUNCTION,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::NUMBER,
    SemanticTokenType::STRING,
    SemanticTokenType::OPERATOR,
    SemanticTokenType::COMMENT,
];

const TOKEN_MODIFIERS: &[SemanticTokenModifier] = &[
    SemanticTokenModifier::DECLARATION, // bit 0
    SemanticTokenModifier::READONLY,    // bit 1
];

pub(super) const RUN_TEST_COMMAND: &str = "hew.runTest";
pub(super) const GENERATE_COMMAND: &str = "hew.generate";
pub(super) const REMOVE_UNUSED_IMPORTS_KIND: &str = "source.removeUnusedImports";

pub(super) fn modifier_bit(m: &SemanticTokenModifier) -> u32 {
    TOKEN_MODIFIERS
        .iter()
        .position(|x| x == m)
        .map_or(0, |i| 1 << i)
}

pub(super) fn build_server_capabilities() -> ServerCapabilities {
    ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::FULL),
                save: Some(TextDocumentSyncSaveOptions::Supported(true)),
                ..Default::default()
            },
        )),
        completion_provider: Some(CompletionOptions {
            trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
            ..Default::default()
        }),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        definition_provider: Some(OneOf::Left(true)),
        type_definition_provider: Some(TypeDefinitionProviderCapability::Simple(true)),
        implementation_provider: Some(ImplementationProviderCapability::Simple(true)),
        document_highlight_provider: Some(OneOf::Left(true)),
        selection_range_provider: Some(SelectionRangeProviderCapability::Simple(true)),
        diagnostic_provider: Some(DiagnosticServerCapabilities::Options(DiagnosticOptions {
            inter_file_dependencies: true,
            workspace_diagnostics: false,
            ..Default::default()
        })),
        document_symbol_provider: Some(OneOf::Left(true)),
        semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
            SemanticTokensOptions {
                legend: SemanticTokensLegend {
                    token_types: TOKEN_TYPES.to_vec(),
                    token_modifiers: TOKEN_MODIFIERS.to_vec(),
                },
                full: Some(SemanticTokensFullOptions::Bool(true)),
                range: None,
                work_done_progress_options: WorkDoneProgressOptions::default(),
            },
        )),
        call_hierarchy_provider: Some(CallHierarchyServerCapability::Simple(true)),
        code_lens_provider: Some(CodeLensOptions {
            resolve_provider: Some(false),
        }),
        execute_command_provider: Some(ExecuteCommandOptions {
            commands: vec![RUN_TEST_COMMAND.to_string(), GENERATE_COMMAND.to_string()],
            work_done_progress_options: WorkDoneProgressOptions::default(),
        }),
        workspace_symbol_provider: Some(OneOf::Left(true)),
        document_link_provider: Some(DocumentLinkOptions {
            resolve_provider: Some(false),
            work_done_progress_options: WorkDoneProgressOptions::default(),
        }),
        references_provider: Some(OneOf::Left(true)),
        rename_provider: Some(OneOf::Right(RenameOptions {
            prepare_provider: Some(true),
            work_done_progress_options: WorkDoneProgressOptions::default(),
        })),
        inlay_hint_provider: Some(OneOf::Right(InlayHintServerCapabilities::Options(
            InlayHintOptions {
                work_done_progress_options: WorkDoneProgressOptions::default(),
                resolve_provider: None,
            },
        ))),
        signature_help_provider: Some(SignatureHelpOptions {
            trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
            retrigger_characters: None,
            work_done_progress_options: WorkDoneProgressOptions::default(),
        }),
        code_action_provider: Some(CodeActionProviderCapability::Options(CodeActionOptions {
            code_action_kinds: Some(vec![
                CodeActionKind::QUICKFIX,
                CodeActionKind::SOURCE,
                CodeActionKind::from(REMOVE_UNUSED_IMPORTS_KIND),
            ]),
            ..Default::default()
        })),
        folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
        document_formatting_provider: Some(OneOf::Left(true)),
        // ls-types 0.0.6 still has no `type_hierarchy_provider` field on
        // `ServerCapabilities`. Advertise via `experimental` so clients that
        // inspect `experimental.typeHierarchyProvider` can reach the three
        // implemented handlers. The field survives capability serialization.
        experimental: Some(json!({"typeHierarchyProvider": true, "hewAnalysisContext": 1})),
        ..Default::default()
    }
}
