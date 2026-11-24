use super::*;

#[test]
fn completions_include_keywords() {
    let parse_result = hew_parser::parse("");
    let doc = DocumentState {
        expanded_program: None,
        source: String::new(),
        line_offsets: compute_line_offsets(""),
        parse_result,
        type_output: None,
        dependency_uris: None,
        diagnostics_by_uri: HashMap::new(),
    };
    let items = hew_analysis::completions::complete(
        &doc.source,
        &doc.parse_result,
        doc.type_output.as_ref(),
        0,
    );
    let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
    assert!(labels.contains(&"fn"));
    assert!(labels.contains(&"actor"));
    assert!(labels.contains(&"spawn"));
}

#[test]
fn snippets_match_select_syntax() {
    let snippets = hew_analysis::completions::keyword_snippets();
    let select = snippets
        .iter()
        .find(|item| item.label == "select...")
        .unwrap();
    let select_text = select.insert_text.as_deref().unwrap();
    assert!(select_text.contains(" from "));
    assert!(select_text.contains("after"));

    let select_from = snippets
        .iter()
        .find(|item| item.label == "select from...")
        .unwrap();
    assert!(select_from
        .insert_text
        .as_deref()
        .unwrap()
        .contains(" from "));
}

#[test]
fn dot_completions_for_struct() {
    // Use valid code with a field access so the parser and type checker succeed.
    // Place the cursor right after the dot (before the field name) to simulate
    // the user typing `p.` and requesting completions.
    let source =
        "type Point {\n    x: i32;\n    y: i32;\n}\n\nfn main() {\n    let p = Point { x: 1, y: 2 };\n    p.x\n}\n";
    let parse_result = hew_parser::parse(source);
    assert!(
        parse_result.errors.is_empty(),
        "unexpected parse errors: {:?}",
        parse_result.errors
    );
    let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
    let type_output = checker.check_program(&parse_result.program);

    let doc = DocumentState {
        expanded_program: None,
        source: source.to_string(),
        line_offsets: compute_line_offsets(source),
        parse_result,
        type_output: Some(type_output),
        dependency_uris: None,
        diagnostics_by_uri: HashMap::new(),
    };
    // Offset right after the dot in `p.x`
    let dot_pos = source.find("p.x").unwrap() + 2;
    let items = hew_analysis::completions::complete(
        &doc.source,
        &doc.parse_result,
        doc.type_output.as_ref(),
        dot_pos,
    );
    let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
    assert!(
        labels.contains(&"x"),
        "expected field 'x' in completions, got: {labels:?}"
    );
    assert!(
        labels.contains(&"y"),
        "expected field 'y' in completions, got: {labels:?}"
    );
}

#[test]
fn dot_completions_for_builtin_stream() {
    // Use the ordinary editor frontend: collect/lines/chunks are source
    // methods loaded with the prelude, rather than checker builtins.
    let source = "fn probe(s: Stream<string>) { s.recv(); }";
    let uri = "file:///lsp/stream_completion.hew".parse().unwrap();
    let doc = analyze_document(&uri, source, &DashMap::new(), &[]);
    let type_output = doc.type_output.as_ref().expect("checked Stream parameter");
    assert!(
        type_output.errors.is_empty(),
        "type errors: {:?}",
        type_output.errors
    );
    let dot_pos = source.find("s.recv").unwrap() + 2;
    let items = hew_analysis::completions::complete(
        &doc.source,
        &doc.parse_result,
        doc.type_output.as_ref(),
        dot_pos,
    );
    let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
    assert!(
        labels.contains(&"recv"),
        "expected method 'recv' in completions, got: {labels:?}"
    );
    assert!(
        labels.contains(&"try_recv"),
        "expected method 'try_recv' in completions, got: {labels:?}"
    );
    // Stream uses channel-family .recv and implements Iterator through an
    // ordinary source .next method, alongside its source adapters.
    assert!(
        labels.contains(&"next"),
        "expected Iterator method 'next' in completions, got: {labels:?}"
    );
    assert!(
        labels.contains(&"collect"),
        "expected method 'collect' in completions, got: {labels:?}"
    );
    // `lines`, `chunks` and `take` are stream methods on the final path.
    assert!(
        labels.contains(&"lines"),
        "expected method 'lines' in completions, got: {labels:?}"
    );
    let recv_item = items
        .iter()
        .find(|item| item.label == "recv")
        .expect("recv completion should exist");
    assert!(
        recv_item
            .detail
            .as_deref()
            .is_some_and(|detail| detail.contains("string")),
        "expected Stream<string> detail for recv(), got: {:?}",
        recv_item.detail
    );
}

#[test]
fn dot_completions_for_actor_handle_surface_its_receive_handlers() {
    // An actor is the type of its handle, so a dot on a handle offers the
    // actor's declared receive handlers. A `send` completion is valid only
    // when the actor declares `receive fn send`; internal state fields
    // stay hidden.
    let repo_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .to_path_buf();
    let registry = hew_types::module_registry::ModuleRegistry::new(vec![repo_root]);
    let source = concat!(
        "actor Counter {\n",
        "    var count: i64;\n",
        "    receive fn increment(n: i64) { count = count + n; }\n",
        "}\n",
        "fn main() {\n",
        "    let c = spawn Counter { count: 0 };\n",
        "    c.increment(1);\n",
        "}",
    );
    let parse_result = hew_parser::parse(source);
    assert!(
        parse_result.errors.is_empty(),
        "unexpected parse errors: {:?}",
        parse_result.errors
    );
    let mut checker = Checker::new(registry);
    let type_output = checker.check_program(&parse_result.program);
    let doc = DocumentState {
        expanded_program: None,
        source: source.to_string(),
        line_offsets: compute_line_offsets(source),
        parse_result,
        type_output: Some(type_output),
        dependency_uris: None,
        diagnostics_by_uri: HashMap::new(),
    };
    // Cursor right after `c.` in `c.increment(1)`.
    let dot_pos = source.rfind("c.increment").unwrap() + 2;
    let items = hew_analysis::completions::complete(
        &doc.source,
        &doc.parse_result,
        doc.type_output.as_ref(),
        dot_pos,
    );
    let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
    assert!(
        !labels.contains(&"send"),
        "`send` is not a method on an actor handle; delivery is by handler \
             name, got: {labels:?}"
    );
    let removed_method = ["to_remote", "_via"].concat();
    assert!(
        !labels.contains(&removed_method.as_str()),
        "removed remote-conversion helper must not appear in actor-handle completions, got: \
             {labels:?}"
    );
    assert!(
        labels.contains(&"increment"),
        "expected `increment` (receive handler) in actor-handle completions, got: {labels:?}"
    );
    assert!(
        !labels.contains(&"count"),
        "internal actor field `count` must not appear in actor-handle completions, got: {labels:?}"
    );
}

#[test]
fn completions_enum_variant_after_dot() {
    let source = "enum Color {\n    Blue;\n    Point { x: i32; y: i32;  }\n    Rgb(u8, u8, u8);\n}\n\nfn main() {\n    let color = Color.Blue;\n}\n";
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

    let doc = DocumentState {
        expanded_program: None,
        source: source.to_string(),
        line_offsets: compute_line_offsets(source),
        parse_result,
        type_output: Some(type_output),
        dependency_uris: None,
        diagnostics_by_uri: HashMap::new(),
    };
    let offset = source.find("Color.Blue").unwrap() + "Color.".len();
    let items = hew_analysis::completions::complete(
        &doc.source,
        &doc.parse_result,
        doc.type_output.as_ref(),
        offset,
    );

    let labels: Vec<&str> = items.iter().map(|item| item.label.as_str()).collect();
    assert_eq!(labels, vec!["Blue", "Point", "Rgb"]);

    let point_item = items
        .iter()
        .find(|item| item.label == "Point")
        .expect("Point completion should exist");
    assert_eq!(point_item.kind, CompletionKind::Constant);
    assert_eq!(point_item.detail.as_deref(), Some("{ x: i32, y: i32 }"));
}

#[test]
fn spawn_completions_only_actors() {
    let source = "actor Foo {}\nactor Bar {}\nfn baz() {}\nfn main() { let h = spawn  }";
    let parse_result = hew_parser::parse(source);
    let doc = DocumentState {
        expanded_program: None,
        source: source.to_string(),
        line_offsets: compute_line_offsets(source),
        parse_result,
        type_output: None,
        dependency_uris: None,
        diagnostics_by_uri: HashMap::new(),
    };
    let spawn_offset = source.find("spawn ").unwrap() + 7;
    let items = hew_analysis::completions::complete(
        &doc.source,
        &doc.parse_result,
        doc.type_output.as_ref(),
        spawn_offset,
    );
    let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
    assert!(labels.contains(&"Foo"), "expected Foo, got: {labels:?}");
    assert!(labels.contains(&"Bar"), "expected Bar, got: {labels:?}");
    assert!(
        !labels.contains(&"baz"),
        "baz should not appear after spawn"
    );
}

#[test]
fn is_rhs_completion_includes_builtin_primitive_types() {
    // After the `is` keyword, the LSP completion provider must surface
    // builtin primitive type names (`i32`, `string`, `bool`, `u64`,
    // etc.) as candidates in addition to user-defined type names.
    // The set is sourced from `hew_types::ty::PRIMITIVE_ALIASES` so it
    // stays aligned with what the checker's `is` type-pattern resolver
    // accepts via `Ty::from_name`.
    let source = r"
            type Holder {
                v: i64;
            }

            fn main() {
                let h = Holder { v: 1 };
                let _eq: bool = h is
            }
        ";
    let parse_result = hew_parser::parse(source);
    let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
    let type_output = checker.check_program(&parse_result.program);
    let doc = DocumentState {
        expanded_program: None,
        source: source.to_string(),
        line_offsets: compute_line_offsets(source),
        parse_result,
        type_output: Some(type_output),
        dependency_uris: None,
        diagnostics_by_uri: HashMap::new(),
    };
    // Position the cursor immediately after the trailing `is`.
    let offset = source.find("h is").unwrap() + "h is".len();
    let items = hew_analysis::completions::complete(
        &doc.source,
        &doc.parse_result,
        doc.type_output.as_ref(),
        offset,
    );
    let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
    for primitive in &["i32", "i64", "u64", "bool", "string", "char", "bytes"] {
        assert!(
            labels.contains(primitive),
            "expected builtin primitive `{primitive}` in is-RHS completion, got: {labels:?}",
        );
    }
    // User types must still be present alongside the builtins.
    assert!(
        labels.contains(&"Holder"),
        "user type `Holder` must still appear in is-RHS completion, got: {labels:?}",
    );
}

#[test]
fn completions_include_local_variables() {
    let source = "fn main() { let counter = 42; let name = \"hello\"; 0 }";
    let parse_result = hew_parser::parse(source);
    assert!(
        parse_result.errors.is_empty(),
        "parse errors: {:?}",
        parse_result.errors
    );

    let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
    let type_output = checker.check_program(&parse_result.program);
    let doc = DocumentState {
        expanded_program: None,
        source: source.to_string(),
        line_offsets: compute_line_offsets(source),
        parse_result,
        type_output: Some(type_output),
        dependency_uris: None,
        diagnostics_by_uri: HashMap::new(),
    };
    let offset = source.find("0 }").unwrap();
    let items = hew_analysis::completions::complete(
        &doc.source,
        &doc.parse_result,
        doc.type_output.as_ref(),
        offset,
    );
    let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
    assert!(
        labels.contains(&"counter"),
        "expected local var 'counter', got: {labels:?}"
    );
    assert!(
        labels.contains(&"name"),
        "expected local var 'name', got: {labels:?}"
    );
}

#[test]
fn completions_do_not_leak_if_block_locals_after_if() {
    let source = r"fn main() {
    if true {
        let branch_only = 1;
    }
    let outside = 2;
    outside
}";
    let parse_result = hew_parser::parse(source);
    assert!(
        parse_result.errors.is_empty(),
        "parse errors: {:?}",
        parse_result.errors
    );
    let doc = DocumentState {
        expanded_program: None,
        source: source.to_string(),
        line_offsets: compute_line_offsets(source),
        parse_result,
        type_output: None,
        dependency_uris: None,
        diagnostics_by_uri: HashMap::new(),
    };
    let offset = source.rfind("outside\n").unwrap();
    let items = hew_analysis::completions::complete(
        &doc.source,
        &doc.parse_result,
        doc.type_output.as_ref(),
        offset,
    );
    let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
    assert!(labels.contains(&"outside"));
    assert!(
        !labels.contains(&"branch_only"),
        "branch-only local should not be in scope after if block: {labels:?}"
    );
}

#[test]
fn completions_cover_type_impl_methods_if_else_match_and_patterns() {
    let source = r"type Point {
    x: i32;
    y: i32;
}

enum Result {
    Ok(i32);
    Err(i32);
}

type Worker {}
impl Worker {
    fn process(input: i32, point: Point, result: Result) -> i32 {
        if input > 0 {
            let then_local = input;
            then_local
        } else if input < 0 {
            let else_if_local = input;
            else_if_local
        } else {
            match result {
                Result.Ok(ok_value) => {
                    let Point { x, y: y_value } = point;
                    let match_local = ok_value + x + y_value;
                    match_local
                }
                Result.Err(err_a) | Result.Err(err_b) => {
                    err_a + err_b
                }
            }
        }
    }
}

impl Worker {
    fn apply(code: i32, result: Result) -> i32 {
        let Result.Ok(inner) = result;
        inner + code
    }
}
";
    let parse_result = hew_parser::parse(source);
    assert!(
        parse_result.errors.is_empty(),
        "parse errors: {:?}",
        parse_result.errors
    );
    let doc = DocumentState {
        expanded_program: None,
        source: source.to_string(),
        line_offsets: compute_line_offsets(source),
        parse_result,
        type_output: None,
        dependency_uris: None,
        diagnostics_by_uri: HashMap::new(),
    };

    let else_if_offset = source.find("else_if_local\n").unwrap();
    let else_if_items = hew_analysis::completions::complete(
        &doc.source,
        &doc.parse_result,
        doc.type_output.as_ref(),
        else_if_offset,
    );
    let else_if_labels: Vec<&str> = else_if_items.iter().map(|i| i.label.as_str()).collect();
    assert!(else_if_labels.contains(&"input"));
    assert!(else_if_labels.contains(&"else_if_local"));

    let match_offset = source.find("match_local\n").unwrap();
    let match_items = hew_analysis::completions::complete(
        &doc.source,
        &doc.parse_result,
        doc.type_output.as_ref(),
        match_offset,
    );
    let match_labels: Vec<&str> = match_items.iter().map(|i| i.label.as_str()).collect();
    assert!(match_labels.contains(&"ok_value"));
    assert!(match_labels.contains(&"x"));
    assert!(match_labels.contains(&"y_value"));
    assert!(match_labels.contains(&"match_local"));

    let or_pattern_offset = source.find("err_a + err_b").unwrap();
    let or_pattern_items = hew_analysis::completions::complete(
        &doc.source,
        &doc.parse_result,
        doc.type_output.as_ref(),
        or_pattern_offset,
    );
    let or_pattern_labels: Vec<&str> = or_pattern_items.iter().map(|i| i.label.as_str()).collect();
    assert!(or_pattern_labels.contains(&"err_a"));
    assert!(or_pattern_labels.contains(&"err_b"));

    let impl_offset = source.find("inner + code").unwrap();
    let impl_items = hew_analysis::completions::complete(
        &doc.source,
        &doc.parse_result,
        doc.type_output.as_ref(),
        impl_offset,
    );
    let impl_labels: Vec<&str> = impl_items.iter().map(|i| i.label.as_str()).collect();
    assert!(impl_labels.contains(&"inner"));
    assert!(impl_labels.contains(&"code"));
}

#[test]
fn checked_imported_record_completion_uses_the_selected_module() {
    let main_source = "import ma;\nimport mb;\nfn main() { let shape = ma.Shape { width: 1 }; println(shape.width); }";
    let selected_source = "pub type Shape {\n    width: i64;\n}\n";
    let other_source = "pub type Shape {\n    colour: i64;\n}\n";
    let main_uri = make_test_uri("/fake/identity-completion/main.hew");
    let documents = DashMap::new();
    documents.insert(
        make_test_uri("/fake/identity-completion/ma.hew"),
        make_doc(selected_source),
    );
    documents.insert(
        make_test_uri("/fake/identity-completion/mb.hew"),
        make_doc(other_source),
    );
    let doc = analyze_document(&main_uri, main_source, &documents, &[]);
    let errors = published_errors(&doc, &main_uri);
    assert!(
        errors.is_empty(),
        "qualified record should check: {errors:?}"
    );
    let offset = main_source.find("ma.Shape {").unwrap() + "ma.Shape {".len();
    let items = hew_analysis::completions::complete(
        main_source,
        &doc.parse_result,
        doc.type_output.as_ref(),
        offset,
    );
    let labels: Vec<_> = items.iter().map(|item| item.label.as_str()).collect();
    assert!(labels.contains(&"width"), "{labels:?}");
    assert!(!labels.contains(&"colour"), "{labels:?}");
}

#[test]
fn user_stream_completion_excludes_builtin_stream_methods() {
    let source = "type Stream {\n    value: i64;\n}\n\nimpl Stream {\n    fn own(self) -> i64 {\n        self.value\n    }\n}\n\nfn main() {\n    let stream = Stream { value: 1 };\n    println(stream.own());\n}\n";
    let doc = make_typed_doc(source);
    let offset = source.find("stream.own").unwrap() + "stream.".len();
    let items = hew_analysis::completions::complete(
        source,
        &doc.parse_result,
        doc.type_output.as_ref(),
        offset,
    );
    let labels: Vec<_> = items.iter().map(|item| item.label.as_str()).collect();
    assert!(labels.contains(&"own"), "{labels:?}");
    assert!(labels.contains(&"value"), "{labels:?}");
    assert!(!labels.contains(&"recv"), "{labels:?}");
    assert!(!labels.contains(&"try_recv"), "{labels:?}");
}

// ── Completion conversion tests ─────────────────────────────────

#[test]
fn to_lsp_completion_maps_kinds() {
    use hew_analysis::{CompletionItem as AnalysisItem, CompletionKind};
    let cases = vec![
        (CompletionKind::Function, CompletionItemKind::FUNCTION),
        (CompletionKind::Variable, CompletionItemKind::VARIABLE),
        (CompletionKind::Keyword, CompletionItemKind::KEYWORD),
        (CompletionKind::Snippet, CompletionItemKind::SNIPPET),
        (CompletionKind::Type, CompletionItemKind::STRUCT),
        (CompletionKind::Actor, CompletionItemKind::CLASS),
        (CompletionKind::Constant, CompletionItemKind::CONSTANT),
        (CompletionKind::Field, CompletionItemKind::FIELD),
        (CompletionKind::Method, CompletionItemKind::METHOD),
        (CompletionKind::Module, CompletionItemKind::MODULE),
    ];
    for (analysis_kind, expected_lsp_kind) in cases {
        let item = AnalysisItem {
            label: "test".to_string(),
            kind: analysis_kind,
            detail: None,
            documentation: None,
            insert_text: None,
            insert_text_is_snippet: false,
            sort_text: None,
        };
        let lsp_item = to_lsp_completion(item);
        assert_eq!(lsp_item.kind, Some(expected_lsp_kind));
    }
}

#[test]
fn to_lsp_completion_snippet_format() {
    use hew_analysis::{CompletionItem as AnalysisItem, CompletionKind};
    let item = AnalysisItem {
        label: "if".to_string(),
        kind: CompletionKind::Snippet,
        detail: Some("if statement".to_string()),
        documentation: None,
        insert_text: Some("if ${1:condition} {\n\t$0\n}".to_string()),
        insert_text_is_snippet: true,
        sort_text: Some("0001".to_string()),
    };
    let lsp_item = to_lsp_completion(item);
    assert_eq!(lsp_item.label, "if");
    assert_eq!(lsp_item.detail.as_deref(), Some("if statement"));
    assert_eq!(lsp_item.insert_text_format, Some(InsertTextFormat::SNIPPET));
    assert_eq!(lsp_item.sort_text.as_deref(), Some("0001"));
}

// ── Documentation field wiring (LSP layer) ──────────────────────

#[test]
fn to_lsp_completion_propagates_doc_comment_as_markdown() {
    // `to_lsp_completion` must map `CompletionItem.documentation` to
    // `ls_types::Documentation::MarkupContent` with `MarkupKind::Markdown`.
    let item = hew_analysis::CompletionItem {
        label: "greet".to_string(),
        kind: CompletionKind::Function,
        detail: Some("fn greet(name: string)".to_string()),
        documentation: Some("Greets the named entity.".to_string()),
        insert_text: None,
        insert_text_is_snippet: false,
        sort_text: None,
    };
    let lsp_item = to_lsp_completion(item);
    let doc = lsp_item
        .documentation
        .expect("documentation field must be populated");
    match doc {
        tower_lsp_server::ls_types::Documentation::MarkupContent(markup) => {
            assert_eq!(
                markup.kind,
                tower_lsp_server::ls_types::MarkupKind::Markdown,
                "documentation kind must be Markdown"
            );
            assert_eq!(markup.value, "Greets the named entity.");
        }
        tower_lsp_server::ls_types::Documentation::String(s) => {
            panic!("expected MarkupContent, got plain String({s:?})")
        }
    }
}

#[test]
fn to_lsp_completion_none_doc_comment_yields_none_documentation() {
    // A completion item with no documentation must yield `None` on the LSP
    // response — not an empty MarkupContent.
    let item = hew_analysis::CompletionItem {
        label: "bare".to_string(),
        kind: CompletionKind::Function,
        detail: None,
        documentation: None,
        insert_text: None,
        insert_text_is_snippet: false,
        sort_text: None,
    };
    let lsp_item = to_lsp_completion(item);
    assert!(
        lsp_item.documentation.is_none(),
        "completion item with no documentation must yield None"
    );
}
