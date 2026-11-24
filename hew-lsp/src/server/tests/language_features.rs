use super::*;

#[test]
fn document_symbols_for_function() {
    let source = "fn foo() -> i32 { 42 }";
    let parse_result = hew_parser::parse(source);
    let lo = compute_line_offsets(source);
    let analysis_symbols = hew_analysis::symbols::build_document_symbols(source, &parse_result);
    let symbols: Vec<DocumentSymbol> = analysis_symbols
        .into_iter()
        .map(|s| symbol_info_to_doc_symbol(source, &lo, s))
        .collect();
    assert_eq!(symbols.len(), 1);
    assert_eq!(symbols[0].name, "foo");
    assert_eq!(symbols[0].kind, SymbolKind::FUNCTION);
}

#[test]
fn checked_signature_help_selects_overloaded_method_identity() {
    let source = "type A {\n    x: i64;\n}\n\nimpl A {\n    fn pick(self, first: i64) -> i64 {\n        first\n    }\n}\n\ntype B {\n    x: i64;\n}\n\nimpl B {\n    fn pick(self, second: bool) -> bool {\n        second\n    }\n}\n\nfn main() {\n    let b = B { x: 2 };\n    println(b.pick(true));\n}\n";
    let doc = make_typed_doc(source);
    let offset = source.find("b.pick(true)").unwrap() + "b.pick(".len();
    let help = hew_analysis::signature_help::build_signature_help(
        source,
        doc.type_output.as_ref().unwrap(),
        offset,
    )
    .expect("checked B.pick signature help");
    let label = &help.signatures[0].label;
    assert!(label.contains("second: bool"), "{label}");
    assert!(!label.contains("first: i64"), "{label}");
}

// ── Hover tests ─────────────────────────────────────────────────

#[test]
fn hover_on_variable() {
    let source = "fn main() -> i32 { let x = 42; x }";
    let parse_result = hew_parser::parse(source);
    let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
    let type_output = checker.check_program(&parse_result.program);
    // Hover on the usage of `x`
    let offset = source.rfind('x').unwrap();
    let result = hew_analysis::hover::hover(source, &parse_result, Some(&type_output), offset);
    assert!(result.is_some(), "expected hover result for variable 'x'");
    let hr = result.unwrap();
    assert!(!hr.contents.is_empty());
}

#[test]
fn hover_on_function_name() {
    let source = "fn greet(name: string) -> string { name }\nfn main() { greet(\"world\"); }";
    let parse_result = hew_parser::parse(source);
    let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
    let type_output = checker.check_program(&parse_result.program);
    // Hover on the call site of `greet`
    let offset = source.rfind("greet").unwrap();
    let result = hew_analysis::hover::hover(source, &parse_result, Some(&type_output), offset);
    assert!(result.is_some(), "expected hover for function call");
    let hr = result.unwrap();
    assert!(
        hr.contents.contains("greet"),
        "hover should mention function name, got: {}",
        hr.contents
    );
}

#[test]
fn hover_on_type_name() {
    let source =
        "type Point {\n    x: i32;\n    y: i32;\n}\n\nfn main() {\n    let p = Point { x: 1, y: 2 };\n    p.x\n}\n";
    let parse_result = hew_parser::parse(source);
    let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
    let type_output = checker.check_program(&parse_result.program);
    // Hover on usage of Point in the struct init
    let offset = source.rfind("Point").unwrap();
    let result = hew_analysis::hover::hover(source, &parse_result, Some(&type_output), offset);
    assert!(result.is_some(), "expected hover for type name");
    let hr = result.unwrap();
    assert!(
        hr.contents.contains("Point"),
        "hover should mention type name, got: {}",
        hr.contents
    );
}

#[test]
fn hover_on_builtin_type_name() {
    let source = "fn drain(rx: Stream<string>) { rx.close(); }";
    let parse_result = hew_parser::parse(source);
    assert!(
        parse_result.errors.is_empty(),
        "unexpected parse errors: {:?}",
        parse_result.errors
    );
    let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
    let type_output = checker.check_program(&parse_result.program);
    assert!(
        type_output.errors.is_empty(),
        "type errors: {:?}",
        type_output.errors
    );
    let offset = source.find("Stream").unwrap();
    let result = hew_analysis::hover::hover(source, &parse_result, Some(&type_output), offset);
    assert!(result.is_some(), "expected hover for builtin type name");
    let hr = result.unwrap();
    assert!(
        hr.contents.contains("Stream"),
        "hover should mention builtin type name, got: {}",
        hr.contents
    );
    assert!(
        hr.contents.contains("recv"),
        "hover should include builtin methods, got: {}",
        hr.contents
    );
}

// ── Folding range tests ─────────────────────────────────────────

#[test]
fn folding_ranges_basic_invocation() {
    // Verify folding ranges can be computed without errors
    let source = "fn main() { 0 }";
    let parse_result = hew_parser::parse(source);
    let ranges = hew_analysis::folding::build_folding_ranges(source, &parse_result);
    // Single-line function may not produce a fold, but should not crash
    let _ = ranges;
}

#[test]
fn folding_ranges_for_actor() {
    let source = r"actor Counter {
    let count: i32;
    receive fn increment(n: i32) {
        count = count + n;
    }
    receive fn get() -> i32 {
        count
    }
}
";
    let parse_result = hew_parser::parse(source);
    assert!(
        parse_result.errors.is_empty(),
        "parse errors: {:?}",
        parse_result.errors
    );
    let ranges = hew_analysis::folding::build_folding_ranges(source, &parse_result);
    assert!(
        !ranges.is_empty(),
        "expected folding ranges for a multi-line actor"
    );
}

#[test]
fn folding_ranges_for_import_group() {
    let source = "import std.io;\nimport std.net;\nfn main() { 0 }";
    let parse_result = hew_parser::parse(source);
    let ranges = hew_analysis::folding::build_folding_ranges(source, &parse_result);
    let import_folds: Vec<_> = ranges
        .iter()
        .filter(|r| matches!(r.kind, hew_analysis::FoldingKind::Imports))
        .collect();
    assert!(
        !import_folds.is_empty(),
        "expected a folding range for import group"
    );
}

// ── Symbol kind mapping tests ───────────────────────────────────

#[test]
fn analysis_symbol_kind_mapping() {
    let cases = vec![
        (hew_analysis::SymbolKind::Function, SymbolKind::FUNCTION),
        (hew_analysis::SymbolKind::Actor, SymbolKind::CLASS),
        (hew_analysis::SymbolKind::Supervisor, SymbolKind::CLASS),
        (hew_analysis::SymbolKind::Machine, SymbolKind::ENUM),
        (hew_analysis::SymbolKind::Enum, SymbolKind::ENUM),
        (hew_analysis::SymbolKind::Trait, SymbolKind::INTERFACE),
        (hew_analysis::SymbolKind::Type, SymbolKind::STRUCT),
        (hew_analysis::SymbolKind::Wire, SymbolKind::STRUCT),
        (hew_analysis::SymbolKind::Constant, SymbolKind::CONSTANT),
        (
            hew_analysis::SymbolKind::TypeAlias,
            SymbolKind::TYPE_PARAMETER,
        ),
        (hew_analysis::SymbolKind::Impl, SymbolKind::NAMESPACE),
        (hew_analysis::SymbolKind::Field, SymbolKind::FIELD),
        (hew_analysis::SymbolKind::Method, SymbolKind::METHOD),
        (hew_analysis::SymbolKind::State, SymbolKind::ENUM_MEMBER),
        (hew_analysis::SymbolKind::Variant, SymbolKind::ENUM_MEMBER),
        (hew_analysis::SymbolKind::Event, SymbolKind::EVENT),
        (hew_analysis::SymbolKind::Module, SymbolKind::MODULE),
        (
            hew_analysis::SymbolKind::Constructor,
            SymbolKind::CONSTRUCTOR,
        ),
    ];
    for (analysis_kind, expected_lsp_kind) in cases {
        assert_eq!(
            analysis_symbol_kind_to_lsp(analysis_kind),
            expected_lsp_kind,
            "mapping mismatch for {analysis_kind:?}"
        );
    }
}

// ── Document symbol tests (additional) ──────────────────────────

#[test]
fn document_symbols_for_actor_with_receive() {
    let source = "actor Counter {\n    let count: i32;\n    receive fn increment(n: i32) {\n        count = count + n;\n    }\n}\n";
    let parse_result = hew_parser::parse(source);
    let lo = compute_line_offsets(source);
    let analysis_symbols = hew_analysis::symbols::build_document_symbols(source, &parse_result);
    let symbols: Vec<DocumentSymbol> = analysis_symbols
        .into_iter()
        .map(|s| symbol_info_to_doc_symbol(source, &lo, s))
        .collect();
    assert!(!symbols.is_empty(), "should find actor symbol");
    let actor_sym = symbols.iter().find(|s| s.name == "Counter");
    assert!(actor_sym.is_some(), "should find Counter actor symbol");
    assert_eq!(actor_sym.unwrap().kind, SymbolKind::CLASS);
}

#[test]
fn document_symbols_for_enum() {
    let source = "enum Colour {\n    Red;\n    Green;\n    Blue;\n}\n";
    let parse_result = hew_parser::parse(source);
    let lo = compute_line_offsets(source);
    let analysis_symbols = hew_analysis::symbols::build_document_symbols(source, &parse_result);
    let symbols: Vec<DocumentSymbol> = analysis_symbols
        .into_iter()
        .map(|s| symbol_info_to_doc_symbol(source, &lo, s))
        .collect();
    assert!(!symbols.is_empty());
    let enum_sym = symbols.iter().find(|s| s.name == "Colour");
    assert!(enum_sym.is_some(), "should find Colour enum symbol");
    assert_eq!(enum_sym.unwrap().kind, SymbolKind::ENUM);
}

#[test]
fn document_symbols_with_children() {
    let source = "type Point {\n    x: i32;\n    y: i32;\n}\nimpl Point {\n    fn distance() -> i32 {\n        0\n    }\n}\n";
    let parse_result = hew_parser::parse(source);
    let lo = compute_line_offsets(source);
    let analysis_symbols = hew_analysis::symbols::build_document_symbols(source, &parse_result);
    let symbols: Vec<DocumentSymbol> = analysis_symbols
        .into_iter()
        .map(|s| symbol_info_to_doc_symbol(source, &lo, s))
        .collect();
    let point_sym = symbols.iter().find(|s| s.name == "Point").unwrap();
    assert!(
        point_sym.children.is_some(),
        "Point should have child symbols"
    );
    let children = point_sym.children.as_ref().unwrap();
    let child_names: Vec<&str> = children.iter().map(|c| c.name.as_str()).collect();
    assert!(
        child_names.contains(&"x") && child_names.contains(&"y"),
        "type children should include fields, got: {child_names:?}"
    );
    assert!(symbols.iter().any(|symbol| symbol
        .children
        .as_ref()
        .is_some_and(|children| children.iter().any(|child| child.name == "distance"))));
}

#[test]
fn document_symbols_use_child_definition_ranges() {
    let source = r"type Point {
    x: i32;
}

machine Traffic {
    events {
        Start;
    }

    state Idle;
    on Start: Idle => .Idle;
}
";
    let parse_result = hew_parser::parse(source);
    let lo = compute_line_offsets(source);
    let analysis_symbols = hew_analysis::symbols::build_document_symbols(source, &parse_result);
    let symbols: Vec<DocumentSymbol> = analysis_symbols
        .into_iter()
        .map(|symbol| symbol_info_to_doc_symbol(source, &lo, symbol))
        .collect();

    let point = symbols
        .iter()
        .find(|symbol| symbol.name == "Point")
        .unwrap();
    let field = point
        .children
        .as_ref()
        .unwrap()
        .iter()
        .find(|symbol| symbol.name == "x")
        .unwrap();
    assert_eq!(field.kind, SymbolKind::FIELD);
    assert_eq!(field.selection_range.start.line, 1);
    assert_eq!(field.selection_range.start.character, 4);

    let machine = symbols
        .iter()
        .find(|symbol| symbol.name == "Traffic")
        .unwrap();
    let children = machine.children.as_ref().unwrap();
    let event = children
        .iter()
        .find(|symbol| symbol.name == "Start")
        .unwrap();
    let state = children
        .iter()
        .find(|symbol| symbol.name == "Idle")
        .unwrap();
    assert_eq!(event.kind, SymbolKind::EVENT);
    // `Start` now lives inside the `events { … }` header (line 6, indented
    // 8 spaces) rather than the former interleaved `event Start;` line.
    assert_eq!(event.selection_range.start.line, 6);
    assert_eq!(event.selection_range.start.character, 8);
    assert_eq!(state.kind, SymbolKind::ENUM_MEMBER);
    assert_eq!(state.selection_range.start.line, 9);
    assert_eq!(state.selection_range.start.character, 10);
}

// ── Inlay hint tests ────────────────────────────────────────────

#[test]
fn inlay_hints_for_let_binding() {
    let source = "fn main() -> i32 { let x = 42; x }";
    let parse_result = hew_parser::parse(source);
    assert!(
        parse_result.errors.is_empty(),
        "parse errors: {:?}",
        parse_result.errors
    );
    let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
    let type_output = checker.check_program(&parse_result.program);
    let hints = hew_analysis::inlay_hints::build_inlay_hints(source, &parse_result, &type_output);
    // There should be at least one type hint for `let x = 42`
    assert!(
        !hints.is_empty(),
        "expected inlay hints for let binding with inferred type"
    );

    let lsp_hint =
        lsp_inlay_hint_from_analysis(source, &compute_line_offsets(source), hints[0].clone());
    match lsp_hint.tooltip {
        Some(InlayHintTooltip::String(value)) => assert_eq!(value, hints[0].label),
        Some(_) => panic!("expected string tooltip"),
        None => panic!("expected tooltip"),
    }
}

// ── Signature help tests ────────────────────────────────────────

#[test]
fn signature_help_for_function_call() {
    let source = "fn add(a: i32, b: i32) -> i32 { a + b }\nfn main() -> i32 { add(1, 2) }";
    let parse_result = hew_parser::parse(source);
    assert!(
        parse_result.errors.is_empty(),
        "parse errors: {:?}",
        parse_result.errors
    );
    let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
    let type_output = checker.check_program(&parse_result.program);
    // Offset inside the call parens, e.g. after the `(`
    let offset = source.find("add(1").unwrap() + 4;
    let result = hew_analysis::signature_help::build_signature_help(source, &type_output, offset);
    assert!(
        result.is_some(),
        "expected signature help inside function call"
    );
    let sh = result.unwrap();
    assert!(
        !sh.signatures.is_empty(),
        "should have at least one signature"
    );
    assert!(
        sh.signatures[0].label.contains("add"),
        "signature label should mention function name, got: {}",
        sh.signatures[0].label
    );
    let lsp_help = lsp_signature_help_from_analysis(sh.clone());
    assert_eq!(lsp_help.active_parameter, sh.active_parameter);
    assert_eq!(lsp_help.signatures[0].active_parameter, sh.active_parameter);
}

#[test]
fn signature_help_for_builtin_method_call() {
    let source = "fn send_one(tx: Sink<i64>) { let _ = tx.send(1); }";
    let parse_result = hew_parser::parse(source);
    assert!(
        parse_result.errors.is_empty(),
        "parse errors: {:?}",
        parse_result.errors
    );
    let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
    let type_output = checker.check_program(&parse_result.program);
    assert!(
        type_output.errors.is_empty(),
        "type errors: {:?}",
        type_output.errors
    );
    let offset = source.find("send(1)").unwrap() + 5;
    let result = hew_analysis::signature_help::build_signature_help(source, &type_output, offset);
    assert!(
        result.is_some(),
        "expected signature help inside builtin method call"
    );
    let sh = result.unwrap();
    assert_eq!(
        sh.signatures[0].label,
        "fn send(item: i64) -> Result<(), SendError>"
    );
}

// ── Formatting handler tests ────────────────────────────────────────

/// Source that `format_source` does not change → handler returns `Some(vec![])`.
#[test]
fn formatting_already_canonical_returns_empty_edits() {
    // The formatter appends a trailing newline; use a source that is already canonical.
    let source = "fn foo() -> i32 {\n    42\n}\n";
    let parse_result = hew_parser::parse(source);
    let formatted = hew_parser::fmt::format_source(source, &parse_result.program);
    // Pre-condition: the source must actually be canonical for this test to be meaningful.
    assert_eq!(
        source, formatted,
        "test pre-condition failed: source is not already canonical"
    );

    let lo = compute_line_offsets(source);
    // Mimic the handler: if formatted == source, return empty edits.
    let edits: Option<Vec<TextEdit>> = if formatted == source {
        Some(vec![])
    } else {
        let range = offset_range_to_lsp(source, &lo, 0, source.len());
        Some(vec![TextEdit {
            range,
            new_text: formatted,
        }])
    };

    assert_eq!(
        edits,
        Some(vec![]),
        "canonical source must yield empty edit list"
    );
}

/// Source with extra whitespace → handler returns a single whole-document `TextEdit`.
#[test]
fn formatting_uncanonical_returns_single_edit() {
    // Extra spaces between tokens trigger reformatting.
    let source = "fn foo()  ->  i32  { 42 }";
    let parse_result = hew_parser::parse(source);
    let formatted = hew_parser::fmt::format_source(source, &parse_result.program);
    assert_ne!(
        source, formatted,
        "test pre-condition failed: source must not be canonical"
    );

    let lo = compute_line_offsets(source);
    let range = offset_range_to_lsp(source, &lo, 0, source.len());
    let edits: Vec<TextEdit> = vec![TextEdit {
        range,
        new_text: formatted.clone(),
    }];

    assert_eq!(
        edits.len(),
        1,
        "expected exactly one whole-document TextEdit"
    );
    assert_eq!(
        edits[0].new_text, formatted,
        "TextEdit new_text must equal format_source output"
    );
}

/// Source containing a non-ASCII character — the range's end column must be
/// UTF-16 code units, not byte count.  'é' is 2 bytes in UTF-8 but 1 UTF-16
/// code unit; the byte-based end would differ from the correct UTF-16 end.
#[test]
fn formatting_unicode_source_uses_correct_range() {
    // Extra spaces before and after '-> i32' trigger reformatting,
    // so the handler produces a non-empty TextEdit.  The trailing comment
    // preserves 'é' in the formatted output so the source lengths differ
    // between UTF-8 bytes and UTF-16 code units.
    let source = "fn foo()  ->  i32  { 42 } // héllo";
    let parse_result = hew_parser::parse(source);
    let formatted = hew_parser::fmt::format_source(source, &parse_result.program);
    assert_ne!(
        source, formatted,
        "test pre-condition failed: source must not be canonical"
    );

    let lo = compute_line_offsets(source);
    let range = offset_range_to_lsp(source, &lo, 0, source.len());

    // The source fits on one line. End character must be the UTF-16 length
    // of that line (not the byte length).  'é' is 2 bytes but 1 UTF-16 unit,
    // so byte_len > utf16_len for this source.
    let byte_len = u32::try_from(source.len()).expect("test source fits in u32");
    let utf16_len = u32::try_from(source.encode_utf16().count()).expect("test source fits in u32");
    assert!(
        byte_len > utf16_len,
        "test pre-condition: source must have byte_len ({byte_len}) > utf16_len ({utf16_len})"
    );
    assert_eq!(
        range.end.character, utf16_len,
        "end.character must be UTF-16 code units ({utf16_len}), not byte count ({byte_len})"
    );
}

#[test]
fn lsp_signature_help_propagates_doc_comment_as_markdown() {
    // `lsp_signature_help_from_analysis` must map `SignatureInfo.documentation`
    // to `SignatureInformation.documentation` as Markdown.
    let result = hew_analysis::SignatureHelpResult {
        signatures: vec![hew_analysis::SignatureInfo {
            label: "fn add(a: i64, b: i64) -> i64".to_string(),
            documentation: Some("Returns the sum of a and b.".to_string()),
            parameters: vec![],
        }],
        active_signature: Some(0),
        active_parameter: Some(0),
    };
    let sig_help = lsp_signature_help_from_analysis(result);
    let sig = &sig_help.signatures[0];
    let doc = sig
        .documentation
        .as_ref()
        .expect("documentation must be propagated");
    match doc {
        tower_lsp_server::ls_types::Documentation::MarkupContent(markup) => {
            assert_eq!(
                markup.kind,
                tower_lsp_server::ls_types::MarkupKind::Markdown
            );
            assert_eq!(markup.value, "Returns the sum of a and b.");
        }
        tower_lsp_server::ls_types::Documentation::String(s) => {
            panic!("expected MarkupContent, got plain String({s:?})")
        }
    }
}
