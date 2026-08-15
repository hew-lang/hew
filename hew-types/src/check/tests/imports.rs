#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

#[test]
fn colliding_import_publishes_none_of_its_other_bindings() {
    let first = make_user_import(
        &["left"],
        Some(ImportSpec::Names(vec![ImportName {
            name: "first".to_string(),
            alias: Some("shared".to_string()),
        }])),
        vec![(Item::Function(make_pub_fn("first", vec![], None)), 0..5)],
    );
    let second = make_user_import(
        &["right"],
        Some(ImportSpec::Names(vec![
            ImportName {
                name: "second".to_string(),
                alias: Some("shared".to_string()),
            },
            ImportName {
                name: "only_second".to_string(),
                alias: None,
            },
        ])),
        vec![
            (Item::Function(make_pub_fn("second", vec![], None)), 0..6),
            (
                Item::Function(make_pub_fn("only_second", vec![], None)),
                7..18,
            ),
        ],
    );
    let output = check_items(vec![
        (Item::Import(first), 0..20),
        (Item::Import(second), 21..50),
    ]);

    assert!(output
        .errors
        .iter()
        .any(|error| error.kind == TypeErrorKind::ImportBindingCollision));
    assert!(output.fn_sigs.contains_key("shared"));
    assert!(!output.fn_sigs.contains_key("only_second"));
}

#[test]
fn prelude_collision_rejects_the_entire_import() {
    let import = make_user_import(
        &["user", "helpers"],
        Some(ImportSpec::Names(vec![
            ImportName {
                name: "custom_print".to_string(),
                alias: Some("Iterator".to_string()),
            },
            ImportName {
                name: "safe_helper".to_string(),
                alias: None,
            },
        ])),
        vec![
            (
                Item::Function(make_pub_fn("custom_print", vec![], None)),
                0..12,
            ),
            (
                Item::Function(make_pub_fn("safe_helper", vec![], None)),
                13..24,
            ),
        ],
    );
    let output = check_items(vec![(Item::Import(import), 0..40)]);

    assert!(output
        .errors
        .iter()
        .any(|error| error.kind == TypeErrorKind::ImportPreludeCollision));
    assert!(!output.fn_sigs.contains_key("safe_helper"));
}

#[test]
fn peer_files_resolve_same_module_alias_from_their_own_imports() {
    let alpha = hew_parser::parse("pub fn value() -> i64 { 7 }");
    let beta = hew_parser::parse("pub fn value() -> bool { true }");
    let mut left =
        hew_parser::parse("import alpha as util; pub fn from_left() -> i64 { util.value() }");
    let mut right =
        hew_parser::parse("import beta as util; pub fn from_right() -> bool { util.value() }");
    for parsed in [&alpha, &beta, &left, &right] {
        assert!(
            parsed.errors.is_empty(),
            "fixture parse: {:?}",
            parsed.errors
        );
    }
    for (program, resolved) in [
        (&mut left.program, alpha.program.items),
        (&mut right.program, beta.program.items),
    ] {
        let import = program
            .items
            .iter_mut()
            .find_map(|(item, _)| match item {
                Item::Import(import) => Some(import),
                _ => None,
            })
            .expect("peer import");
        import.resolved_items = Some(resolved);
    }

    let root_id = ModuleId::root();
    let shared_id = ModuleId::new(vec!["shared".to_string()]);
    let mut graph = ModuleGraph::new(root_id.clone());
    let mut shared_items = left.program.items;
    let left_count = shared_items.len();
    shared_items.extend(right.program.items);
    graph
        .add_module(Module {
            id: shared_id.clone(),
            items: shared_items,
            imports: vec![],
            source_paths: vec!["shared/left.hew".into(), "shared/right.hew".into()],
            doc: None,
        })
        .expect("shared module");
    graph
        .add_module(Module {
            id: root_id.clone(),
            items: vec![],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        })
        .expect("root module");
    graph.item_sources.insert(
        "shared".to_string(),
        std::iter::repeat_n("shared/left.hew".into(), left_count)
            .chain(std::iter::repeat_n(
                "shared/right.hew".into(),
                graph.modules[&shared_id].items.len() - left_count,
            ))
            .collect(),
    );
    graph.topo_order = vec![shared_id, root_id];

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&Program {
        items: vec![],
        module_graph: Some(graph),
        module_doc: None,
    });
    assert!(
        output.errors.is_empty(),
        "peer imports must remain file-local: {:#?}",
        output.errors
    );
    let owners: HashSet<_> = output
        .module_import_bindings
        .iter()
        .filter(|((module, _, binding), _)| {
            module.as_deref() == Some("shared") && binding == "util"
        })
        .map(|(_, owner)| owner.as_str())
        .collect();
    assert_eq!(owners, HashSet::from(["alpha", "beta"]));
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "the regression constructs a multi-file module graph with an early import seed"
)]
fn early_lifecycle_seed_uses_the_importing_peer_file_index() {
    let failure_path: std::path::PathBuf = "std/failure.hew".into();
    let first_peer: std::path::PathBuf = "consumer/first.hew".into();
    let second_peer: std::path::PathBuf = "consumer/second.hew".into();
    let failure = hew_parser::parse("pub enum CrashKind { Crashed; }");
    let first = hew_parser::parse("pub fn untouched() {}");
    let mut second = hew_parser::parse(
        "import std::failure::{ CrashKind as Kind }; pub type Holder { kind: Kind; }",
    );
    for parsed in [&failure, &first, &second] {
        assert!(
            parsed.errors.is_empty(),
            "fixture parse: {:?}",
            parsed.errors
        );
    }
    let import = second
        .program
        .items
        .iter_mut()
        .find_map(|(item, _)| match item {
            Item::Import(import) => Some(import),
            _ => None,
        })
        .expect("peer import");
    import.resolved_items = Some(failure.program.items.clone());
    import.resolved_item_source_paths =
        std::iter::repeat_n(failure_path.clone(), failure.program.items.len()).collect();
    import.resolved_source_paths = vec![failure_path.clone()];

    let root_id = ModuleId::root();
    let failure_id = ModuleId::new(vec!["std".to_string(), "failure".to_string()]);
    let consumer_id = ModuleId::new(vec!["consumer".to_string()]);
    let mut consumer_items = first.program.items;
    let first_count = consumer_items.len();
    consumer_items.extend(second.program.items);
    let mut graph = ModuleGraph::new(root_id.clone());
    graph
        .add_module(Module {
            id: failure_id.clone(),
            items: failure.program.items,
            imports: vec![],
            source_paths: vec![failure_path],
            doc: None,
        })
        .expect("failure module");
    graph
        .add_module(Module {
            id: consumer_id.clone(),
            items: consumer_items,
            imports: vec![hew_parser::module::ModuleImport {
                target: failure_id.clone(),
                spec: Some(ImportSpec::Names(vec![ImportName {
                    name: "CrashKind".to_string(),
                    alias: Some("Kind".to_string()),
                }])),
                span: 0..45,
            }],
            source_paths: vec![first_peer.clone(), second_peer.clone()],
            doc: None,
        })
        .expect("consumer module");
    graph
        .add_module(Module {
            id: root_id.clone(),
            items: vec![],
            imports: vec![],
            source_paths: vec!["main.hew".into()],
            doc: None,
        })
        .expect("root module");
    graph.item_sources.insert(
        "consumer".to_string(),
        std::iter::repeat_n(first_peer, first_count)
            .chain(std::iter::repeat_n(
                second_peer.clone(),
                graph.modules[&consumer_id].items.len() - first_count,
            ))
            .collect(),
    );
    graph.topo_order = vec![failure_id, consumer_id, root_id];
    let second_file_idx = graph
        .file_span_indices()
        .path_index(&second_peer)
        .expect("second peer index");

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&Program {
        items: vec![],
        module_graph: Some(graph),
        module_doc: None,
    });
    assert_eq!(
        output.import_type_name_aliases.get(&(
            Some("consumer".to_string()),
            second_file_idx,
            "Kind".to_string(),
        )),
        Some(&"std.failure.CrashKind".to_string())
    );
    assert!(
        !output.import_type_name_aliases.contains_key(&(
            Some("consumer".to_string()),
            0,
            "Kind".to_string(),
        )),
        "a non-root import must never publish under root file index 0: {:#?}",
        output.import_type_name_aliases
    );
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "the regression constructs a complete three-module graph with source attribution"
)]
fn resolved_module_copy_reenters_declaring_file_import_scope() {
    let color_path: std::path::PathBuf = "pkgs/aliassrc.hew".into();
    let consumer_path: std::path::PathBuf = "pkgs/deepalias.hew".into();
    let mut color = hew_parser::parse("pub enum Color { Blue(i64); }");
    let mut consumer = hew_parser::parse(
        r"
        import hew::aliassrc::{ Color as Hue };
        pub type AliasBox { item: Hue; }
        pub enum AliasWrap { Has(Hue); }
        pub fn make() -> Hue { Hue::Blue(7) }
        pub fn score() -> i64 {
            let boxed: AliasBox = AliasBox { item: make() };
            let wrapped: AliasWrap = AliasWrap::Has(boxed.item);
            match wrapped {
                AliasWrap::Has(color) => match color {
                    Hue::Blue(value) => value,
                    _ => 0,
                },
            }
        }
        ",
    );
    let mut root = hew_parser::parse("import hew::deepalias;");
    for parsed in [&color, &consumer, &root] {
        assert!(
            parsed.errors.is_empty(),
            "fixture parse: {:?}",
            parsed.errors
        );
    }

    let color_item_count = color.program.items.len();
    let consumer_import = consumer
        .program
        .items
        .iter_mut()
        .find_map(|(item, _)| match item {
            Item::Import(import) => Some(import),
            _ => None,
        })
        .expect("consumer import");
    consumer_import.resolved_items = Some(color.program.items.clone());
    consumer_import.resolved_item_source_paths =
        std::iter::repeat_n(color_path.clone(), color_item_count).collect();
    consumer_import.resolved_source_paths = vec![color_path.clone()];

    let consumer_item_count = consumer.program.items.len();
    let root_import = root
        .program
        .items
        .iter_mut()
        .find_map(|(item, _)| match item {
            Item::Import(import) => Some(import),
            _ => None,
        })
        .expect("root import");
    root_import.resolved_items = Some(consumer.program.items.clone());
    root_import.resolved_item_source_paths =
        std::iter::repeat_n(consumer_path.clone(), consumer_item_count).collect();
    root_import.resolved_source_paths = vec![consumer_path.clone()];

    let root_id = ModuleId::root();
    let color_id = ModuleId::new(vec!["hew".to_string(), "aliassrc".to_string()]);
    let consumer_id = ModuleId::new(vec!["hew".to_string(), "deepalias".to_string()]);
    let mut graph = ModuleGraph::new(root_id.clone());
    graph
        .add_module(Module {
            id: color_id.clone(),
            items: std::mem::take(&mut color.program.items),
            imports: vec![],
            source_paths: vec![color_path.clone()],
            doc: None,
        })
        .expect("color module");
    graph
        .add_module(Module {
            id: consumer_id.clone(),
            items: consumer.program.items.clone(),
            imports: vec![],
            source_paths: vec![consumer_path.clone()],
            doc: None,
        })
        .expect("consumer module");
    graph
        .add_module(Module {
            id: root_id.clone(),
            items: vec![],
            imports: vec![],
            source_paths: vec!["main.hew".into()],
            doc: None,
        })
        .expect("root module");
    graph.item_sources.insert(
        "hew.aliassrc".to_string(),
        std::iter::repeat_n(color_path, color_item_count).collect(),
    );
    graph.item_sources.insert(
        "hew.deepalias".to_string(),
        std::iter::repeat_n(consumer_path, consumer_item_count).collect(),
    );
    graph.topo_order = vec![color_id, consumer_id, root_id];

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&Program {
        items: root.program.items,
        module_graph: Some(graph),
        module_doc: None,
    });
    assert!(
        output.errors.is_empty(),
        "resolved module copies must keep their source-file imports: {:#?}",
        output.errors
    );
    assert_eq!(
        output
            .fn_sigs
            .get("hew.deepalias.make")
            .map(|sig| &sig.return_type),
        Some(&Ty::named("hew.aliassrc.Color", vec![]))
    );
}

#[test]
fn failed_member_lookup_explains_lexical_module_shadowing() {
    let mut root =
        hew_parser::parse("import helper as util; fn probe(util: i64) { util.missing(); }");
    assert!(root.errors.is_empty(), "fixture parse: {:?}", root.errors);
    let import = root
        .program
        .items
        .iter_mut()
        .find_map(|(item, _)| match item {
            Item::Import(import) => Some(import),
            _ => None,
        })
        .expect("fixture import");
    import.resolved_items = Some(vec![]);

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&root.program);
    let error = output
        .errors
        .iter()
        .find(|error| error.kind == TypeErrorKind::UndefinedMethod)
        .expect("lexical receiver should fail as an ordinary value method lookup");
    assert!(error
        .notes
        .iter()
        .any(|(_, note, _)| note.contains("shadows the imported module")));
}

#[test]
fn should_import_name_bare_import_returns_false() {
    assert!(!Checker::should_import_name("helper", &None));
}

fn check_resolved_testffi_import(root_source: &str) -> (Checker, TypeCheckOutput) {
    let module = hew_parser::parse(include_str!(
        "../../../../tests/pkg-import/pkgs/testffi/testffi.hew"
    ));
    assert!(module.errors.is_empty(), "parse: {:?}", module.errors);
    let mut root = hew_parser::parse(root_source);
    assert!(root.errors.is_empty(), "parse: {:?}", root.errors);
    let import = root
        .program
        .items
        .iter_mut()
        .find_map(|(item, _)| match item {
            Item::Import(import) => Some(import),
            _ => None,
        })
        .expect("fixture import");
    import.resolved_items = Some(module.program.items);

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&root.program);
    (checker, output)
}

/// Resolve the two same-leaf package fixtures exactly as the package loader
/// does: imports retain their lexical bindings while declarations retain their
/// full source owners (`hew.closableerr` and `hew.closableerr2`).
fn check_resolved_closableerr_import(
    root_source: &str,
    include_second_owner: bool,
) -> (Checker, TypeCheckOutput) {
    let primary = hew_parser::parse(include_str!(
        "../../../../tests/pkg-import/pkgs/closableerr/closableerr.hew"
    ));
    assert!(primary.errors.is_empty(), "parse: {:?}", primary.errors);
    let secondary = hew_parser::parse(include_str!(
        "../../../../tests/pkg-import/pkgs/closableerr2/closableerr2.hew"
    ));
    assert!(secondary.errors.is_empty(), "parse: {:?}", secondary.errors);
    let mut root = hew_parser::parse(root_source);
    assert!(root.errors.is_empty(), "parse: {:?}", root.errors);

    for (item, _) in &mut root.program.items {
        let Item::Import(import) = item else {
            continue;
        };
        match import.path.as_slice() {
            [package, module] if package == "hew" && module == "closableerr" => {
                import.resolved_items = Some(primary.program.items.clone());
            }
            [package, module]
                if include_second_owner && package == "hew" && module == "closableerr2" =>
            {
                import.resolved_items = Some(secondary.program.items.clone());
            }
            _ => {}
        }
    }

    // A resolved import surface alone records the declarations, but a trait
    // method signature is collected from its declaring module. Mirror the
    // package loader's graph so the conformance check reads
    // `hew.closableerr.Closable::close`, never an importer-local placeholder.
    let root_id = ModuleId::root();
    let primary_id = ModuleId::new(vec!["hew".to_string(), "closableerr".to_string()]);
    let secondary_id = ModuleId::new(vec!["hew".to_string(), "closableerr2".to_string()]);
    let mut module_graph = ModuleGraph::new(root_id.clone());
    module_graph
        .add_module(Module {
            id: primary_id.clone(),
            items: primary.program.items,
            imports: vec![],
            source_paths: vec![],
            doc: None,
        })
        .expect("add primary fixture module");
    if include_second_owner {
        module_graph
            .add_module(Module {
                id: secondary_id.clone(),
                items: secondary.program.items,
                imports: vec![],
                source_paths: vec![],
                doc: None,
            })
            .expect("add second fixture module");
    }
    module_graph
        .add_module(Module {
            id: root_id.clone(),
            items: root.program.items.clone(),
            imports: vec![],
            source_paths: vec![],
            doc: None,
        })
        .expect("add root fixture module");
    module_graph.topo_order = std::iter::once(primary_id)
        .chain(include_second_owner.then_some(secondary_id))
        .chain(std::iter::once(root_id))
        .collect();

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&Program {
        items: root.program.items,
        module_graph: Some(module_graph),
        module_doc: None,
    });
    (checker, output)
}

#[test]
fn qualified_nested_trait_signature_uses_source_owner_and_credits_module_binding() {
    let (checker, output) = check_resolved_closableerr_import(
        include_str!("../../../../tests/pkg-import/qualified_trait_sig.hew"),
        false,
    );

    assert!(
        output.errors.is_empty(),
        "qualified nested trait signature must typecheck: {:#?}",
        output.errors
    );
    assert!(
        !output.warnings.iter().any(|warning| {
            warning.kind == TypeErrorKind::UnusedImport && warning.message.contains("closableerr")
        }),
        "the lexical whole-module binding must be marked used: {:#?}",
        output.warnings
    );
    assert_eq!(
        output
            .fn_sigs
            .get("ClosableHandle::close")
            .map(|sig| &sig.return_type),
        Some(&Ty::result(
            Ty::Unit,
            Ty::Named {
                builtin: None,
                name: "hew.closableerr.CloseError".to_string(),
                args: vec![],
            },
        )),
        "the nested impl return must retain the exact source owner"
    );
    assert!(
        checker.type_defs.contains_key("hew.closableerr.CloseError"),
        "source declaration must be registered under its canonical owner"
    );
}

#[test]
fn selective_trait_import_keeps_module_qualifier_for_exact_sibling_identity() {
    let (_, output) = check_resolved_closableerr_import(
        include_str!("../../../../tests/pkg-import/aliased_trait_sig.hew"),
        false,
    );

    assert!(
        output.errors.is_empty(),
        "a selective trait alias must retain its source module for sibling types: {:#?}",
        output.errors
    );
    assert_eq!(
        output
            .module_import_bindings
            .get(&(None, 0, "closableerr".to_string()))
            .map(String::as_str),
        Some("hew.closableerr"),
        "the lexical `closableerr` qualifier must retain the exact owner even \
         when only `Closable as C` was selectively imported"
    );
    assert_eq!(
        output
            .fn_sigs
            .get("ClosableHandle::close")
            .map(|sig| &sig.return_type),
        Some(&Ty::result(
            Ty::Unit,
            Ty::Named {
                builtin: None,
                name: "hew.closableerr.CloseError".to_string(),
                args: vec![],
            },
        )),
        "the qualified sibling must resolve through its source owner"
    );
    assert!(
        !output.warnings.iter().any(|warning| {
            warning.kind == TypeErrorKind::UnusedImport && warning.message.contains("closableerr")
        }),
        "using the selective trait alias and its sibling qualifier must credit the import: {:#?}",
        output.warnings
    );
}

#[test]
fn aliased_trait_rejects_same_leaf_nested_type_from_different_source_owner() {
    let (_, output) = check_resolved_closableerr_import(
        include_str!("../../../../tests/pkg-import/aliased_trait_cross_module_sig_reject.hew"),
        true,
    );

    assert!(
        output.errors.iter().any(|error| {
            matches!(
                &error.kind,
                TypeErrorKind::TraitImplSignatureMismatch {
                    detail: "return type",
                    ..
                }
            )
        }),
        "a trait alias must not collapse `hew.closableerr.CloseError` with \
         `hew.closableerr2.CloseError`: {:#?}",
        output.errors,
    );
}

#[test]
fn imported_actor_i32_uses_exact_module_binding_and_owner() {
    let (checker, output) = check_resolved_testffi_import(include_str!(
        "../../../../tests/pkg-import/imported_actor_ask_i32.hew"
    ));

    assert!(
        output.errors.is_empty(),
        "imported i32 actor ask must typecheck: {:#?}",
        output.errors
    );
    assert!(checker.type_defs.contains_key("hew.testffi.Db"));
    assert!(checker
        .module_type_exports
        .get("hew.testffi")
        .is_some_and(|exports| exports.contains("Db")));
    assert!(!checker.module_type_exports.contains_key("testffi"));
    assert_eq!(
        output
            .module_import_bindings
            .iter()
            .find(|((_, _, binding), _)| binding == "testffi")
            .map(|(_, owner)| owner.as_str()),
        Some("hew.testffi"),
    );
    assert_eq!(
        output
            .fn_sigs
            .get("hew.testffi.Db::count32")
            .map(|sig| &sig.return_type),
        Some(&Ty::I32)
    );
}

#[test]
fn imported_actor_record_impl_and_extern_share_exact_owner() {
    let (checker, output) = check_resolved_testffi_import(include_str!(
        "../../../../tests/pkg-import/imported_actor_ask_record.hew"
    ));

    let result_ty = Ty::Named {
        builtin: None,
        name: "hew.testffi.Result".to_string(),
        args: vec![],
    };
    assert!(
        checker.registry.has_type_markers("hew.testffi.Result"),
        "canonical record marker metadata must be published"
    );
    assert!(
        checker
            .registry
            .implements_marker(&result_ty, crate::traits::MarkerTrait::Send),
        "canonical i64-only Result must derive Send"
    );
    assert!(
        output.errors.is_empty(),
        "imported record actor ask must typecheck: {:#?}",
        output.errors
    );
    assert_eq!(
        output
            .fn_sigs
            .get("hew.testffi.Db::query")
            .map(|sig| &sig.return_type),
        Some(&result_ty)
    );
    assert_eq!(
        output
            .fn_sigs
            .get("hew.testffi.hew_testffi_query")
            .map(|sig| &sig.return_type),
        Some(&result_ty)
    );
    assert!(checker
        .type_defs
        .get("hew.testffi.Result")
        .is_some_and(|result| result.methods.contains_key("echo_len")));
    let echo_id = output
        .impl_method_declaration_ids
        .get("hew.testffi.Result::echo_len")
        .unwrap_or_else(|| {
            panic!(
                "canonical emitted impl symbol must publish its declaration ID; keys: {:?}",
                output
                    .impl_method_declaration_ids
                    .keys()
                    .collect::<Vec<_>>()
            )
        });
    assert!(echo_id
        .full_path()
        .starts_with("hew.testffi.Result::<impl "));
}

#[test]
fn module_private_extern_call_publishes_exact_executable_target() {
    let output = check_source_in_module(
        include_str!("../../../../tests/pkg-import/pkgs/testffi/testffi.hew"),
        vec!["hew".to_string(), "testffi".to_string()],
    );
    assert!(
        output.errors.is_empty(),
        "type errors: {:#?}",
        output.errors
    );
    assert!(
        output.direct_call_targets.values().any(|target| {
            matches!(
                target,
                crate::check::dispatch::CallTarget::Extern {
                    declaration,
                    endpoint,
                    ..
                } if declaration.full_path() == "hew.testffi.hew_testffi_query"
                    && endpoint == "hew_testffi_query"
            )
        }),
        "direct targets: {:?}",
        output.direct_call_targets
    );
}

#[test]
fn same_leaf_impl_methods_publish_distinct_full_declaration_ids() {
    let left = hew_parser::parse(
        r"
        pub type Result { left: i64; }
        impl Result {
            fn echo(self) -> i64 { self.left }
        }
        ",
    );
    let right = hew_parser::parse(
        r"
        pub type Result { right: string; }
        impl Result {
            fn echo(self) -> string { self.right }
        }
        ",
    );
    assert!(left.errors.is_empty());
    assert!(right.errors.is_empty());
    let mut root = hew_parser::parse(
        r"
        import left::render as left_render;
        import right::render as right_render;
        fn main() {}
        ",
    );
    assert!(root.errors.is_empty());
    for (item, _) in &mut root.program.items {
        let Item::Import(import) = item else {
            continue;
        };
        import.resolved_items = Some(if import.path.first().is_some_and(|part| part == "left") {
            left.program.items.clone()
        } else {
            right.program.items.clone()
        });
    }

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&root.program);
    assert!(
        output.errors.is_empty(),
        "type errors: {:#?}",
        output.errors
    );
    let left_id = output
        .impl_method_declaration_ids
        .get("left.render.Result::echo")
        .expect("left emitted impl symbol");
    let right_id = output
        .impl_method_declaration_ids
        .get("right.render.Result::echo")
        .expect("right emitted impl symbol");
    assert_ne!(left_id, right_id);
    assert!(left_id
        .full_path()
        .starts_with("left.render.Result::<impl "));
    assert!(right_id
        .full_path()
        .starts_with("right.render.Result::<impl "));
}

#[test]
fn user_channel_lookalike_retains_nested_sender_and_receiver_identity() {
    let user_module = hew_parser::parse(
        r"
        pub type Sender { marker: i64; }
        pub type Receiver { marker: i64; }
        ",
    );
    assert!(
        user_module.errors.is_empty(),
        "parse: {:#?}",
        user_module.errors
    );

    let mut root = hew_parser::parse(
        r"
        import std::channel::channel as ch;
        fn probe(tx: ch.Sender, rx: ch.Receiver) {}
        ",
    );
    assert!(root.errors.is_empty(), "parse: {:#?}", root.errors);
    let user_source = std::path::PathBuf::from("/user/project/std/channel/channel.hew");
    let import = root
        .program
        .items
        .iter_mut()
        .find_map(|(item, _)| match item {
            Item::Import(import) => Some(import),
            _ => None,
        })
        .expect("fixture import");
    import.resolved_items = Some(user_module.program.items);
    import.resolved_source_paths = vec![user_source.clone()];
    import.resolved_item_source_paths = vec![user_source; 2];

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&root.program);
    assert!(
        output.errors.is_empty(),
        "type errors: {:#?}",
        output.errors
    );
    let params = &output.fn_sigs["probe"].params;
    assert!(matches!(
        &params[0],
        Ty::Named {
            name,
            args,
            builtin: None,
        } if name == "std.channel.channel.Sender" && args.is_empty()
    ));
    assert!(matches!(
        &params[1],
        Ty::Named {
            name,
            args,
            builtin: None,
        } if name == "std.channel.channel.Receiver" && args.is_empty()
    ));
}

#[test]
fn should_import_name_glob_returns_true() {
    assert!(Checker::should_import_name(
        "helper",
        &Some(ImportSpec::Glob)
    ));
    assert!(Checker::should_import_name(
        "anything",
        &Some(ImportSpec::Glob)
    ));
}

#[test]
fn should_import_name_named_match() {
    let spec = Some(ImportSpec::Names(vec![
        ImportName {
            name: "helper".to_string(),
            alias: None,
        },
        ImportName {
            name: "parse".to_string(),
            alias: None,
        },
    ]));
    assert!(Checker::should_import_name("helper", &spec));
    assert!(Checker::should_import_name("parse", &spec));
    assert!(!Checker::should_import_name("other", &spec));
}

// -- Bare import: qualified only --

#[test]
fn bare_import_registers_qualified_name() {
    let helper = make_pub_fn(
        "helper",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let import = make_user_import(
        &["myapp", "utils"],
        None, // bare import
        vec![(Item::Function(helper), 0..0)],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    assert!(
        output.fn_sigs.contains_key("utils.helper"),
        "bare import should register qualified name 'utils.helper'"
    );
    assert!(
        !output.fn_sigs.contains_key("helper"),
        "bare import should NOT register unqualified name 'helper'"
    );
}

// -- C2 qualified-by-default import surface (types + machines) --
//
// These mirror the function-arm gate (`bare_import_registers_qualified_name`)
// for the type/machine arms. A plain `import m;` publishes only the qualified
// binding; bare publication is opt-in via `::{ Name }` or glob. The source
// module's own bare `type_defs` entry is always kept (the qualified alias copy
// reads it) — what the gate controls is the *importer-scope* binding recorded
// in `unqualified_to_module` / `known_types`.

/// Helper: build a single-field public struct `TypeDecl`.
fn make_pub_struct(name: &str, field: &str) -> TypeDecl {
    TypeDecl {
        visibility: Visibility::Pub,
        kind: TypeDeclKind::Struct,
        name: name.to_string(),
        type_params: None,
        where_clause: None,
        body: vec![TypeBodyItem::Field {
            name: field.to_string(),
            ty: (
                TypeExpr::Named {
                    name: "i64".to_string(),
                    type_args: None,
                },
                0..0,
            ),
            attributes: Vec::new(),
            doc_comment: None,
            span: 0..0,
        }],
        doc_comment: None,
        wire: None,
        is_indirect: false,
        resource_marker: hew_parser::ast::ResourceMarker::None,
        is_opaque: false,
        consuming_methods: Vec::new(),
        lang_item: None,
    }
}

/// P1 — a bare `import m;` of a `pub type` publishes only its full source
/// owner; the importer-scope bare binding is NOT recorded.
#[test]
fn bare_import_type_registers_qualified_only() {
    let reply = make_pub_struct("Reply", "code");
    let import = make_user_import(
        &["myapp", "mod_a"],
        None, // bare import
        vec![(Item::TypeDecl(reply), 0..0)],
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&Program {
        module_graph: None,
        items: vec![(Item::Import(import), 0..0)],
        module_doc: None,
    });

    // Full-owner authority is always published; the lexical module spelling
    // remains a resolver binding, not a second TypeDef identity.
    assert!(
        output.type_defs.contains_key("myapp.mod_a.Reply"),
        "bare import should register the canonical type `myapp.mod_a.Reply`"
    );
    assert!(!output.type_defs.contains_key("mod_a.Reply"));
    assert!(!output.type_defs.contains_key("Reply"));
    // The importer-scope binding is not published.
    assert!(
        !checker
            .unqualified_to_module
            .contains_key(&(None, 0, "Reply".to_string())),
        "bare import must NOT publish the importer-scope bare binding for `Reply`"
    );
    assert!(
        !checker.known_types.contains("Reply"),
        "bare import must NOT publish bare `Reply` into the importer's known types"
    );
}

/// P3 — an explicit `import m::{ Reply };` restores the bare binding.
#[test]
fn named_import_type_publishes_bare_binding() {
    let reply = make_pub_struct("Reply", "code");
    let import = make_user_import(
        &["myapp", "mod_a"],
        Some(ImportSpec::Names(vec![ImportName {
            name: "Reply".to_string(),
            alias: None,
        }])),
        vec![(Item::TypeDecl(reply), 0..0)],
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&Program {
        module_graph: None,
        items: vec![(Item::Import(import), 0..0)],
        module_doc: None,
    });

    assert!(
        output.type_defs.contains_key("myapp.mod_a.Reply"),
        "named import should still register the exact source-qualified type"
    );
    assert!(
        checker
            .unqualified_to_module
            .contains_key(&(None, 0, "Reply".to_string())),
        "named import must publish the importer-scope bare binding for `Reply`"
    );
    assert!(
        checker.known_types.contains("Reply"),
        "named import must publish bare `Reply` into the importer's known types"
    );
}

/// P3-alias — a named import alias publishes under the aliased name only.
#[test]
fn named_import_type_alias_publishes_alias_binding() {
    let reply = make_pub_struct("Reply", "code");
    let import = make_user_import(
        &["myapp", "mod_a"],
        Some(ImportSpec::Names(vec![ImportName {
            name: "Reply".to_string(),
            alias: Some("R".to_string()),
        }])),
        vec![(Item::TypeDecl(reply), 0..0)],
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.check_program(&Program {
        module_graph: None,
        items: vec![(Item::Import(import), 0..0)],
        module_doc: None,
    });

    assert!(
        checker
            .unqualified_to_module
            .contains_key(&(None, 0, "R".to_string())),
        "aliased named import must publish the alias binding `R`"
    );
    assert!(
        !checker
            .unqualified_to_module
            .contains_key(&(None, 0, "Reply".to_string())),
        "aliased named import must NOT publish the source name `Reply`"
    );
}

/// P3-alias-identity — an aliased opt-in (`import m::{ Reply as R }`) makes the
/// bare binding `R` resolve to the SOURCE identity `myapp.mod_a.Reply`, not a
/// phantom `myapp.mod_a.R`. This is the resolver half of the aliased-import fix: the published-bare
/// map carries the owner-qualified source name, so `published_bare_type_qualified`
/// returns the type `m` actually exports under `Reply`.
#[test]
fn alias_import_resolves_bare_binding_to_source_identity() {
    let reply = make_pub_struct("Reply", "code");
    let import = make_user_import(
        &["myapp", "mod_a"],
        Some(ImportSpec::Names(vec![ImportName {
            name: "Reply".to_string(),
            alias: Some("R".to_string()),
        }])),
        vec![(Item::TypeDecl(reply), 0..0)],
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.check_program(&Program {
        module_graph: None,
        items: vec![(Item::Import(import), 0..0)],
        module_doc: None,
    });

    assert_eq!(
        checker.published_bare_type_qualified("R"),
        Some("myapp.mod_a.Reply".to_string()),
        "aliased binding `R` must resolve to the full source identity `myapp.mod_a.Reply`, not `myapp.mod_a.R`"
    );
    // The reconstructed `myapp.mod_a.R` must never exist as a registered def — the bug
    // was binding it (or failing closed) instead of the real source type.
    assert!(
        !checker.type_defs.contains_key("myapp.mod_a.R"),
        "no `myapp.mod_a.R` def should exist; the alias binds the source `Reply`"
    );
}

/// P3-alias-no-conflation — `import m::{ Reply as Other }` where `m` ALSO exports
/// a DISTINCT `Other` must bind the alias to the SOURCE `m.Reply`, never the
/// same-named export `m.Other`. Source-name matching opts in only `Reply`; the
/// real `Other` is not opted in by the alias, and the published-bare map records
/// the source identity so the binding cannot conflate the two nominal types.
#[test]
fn alias_import_does_not_conflate_with_same_named_export() {
    let reply = make_pub_struct("Reply", "code");
    let other = make_pub_struct("Other", "tag");
    let import = make_user_import(
        &["myapp", "mod_a"],
        Some(ImportSpec::Names(vec![ImportName {
            name: "Reply".to_string(),
            alias: Some("Other".to_string()),
        }])),
        vec![(Item::TypeDecl(reply), 0..0), (Item::TypeDecl(other), 0..0)],
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&Program {
        module_graph: None,
        items: vec![(Item::Import(import), 0..0)],
        module_doc: None,
    });

    // Both distinct source types keep their own qualified identity.
    assert!(
        output.type_defs.contains_key("myapp.mod_a.Reply"),
        "source `Reply` must register its qualified identity"
    );
    assert!(
        output.type_defs.contains_key("myapp.mod_a.Other"),
        "the distinct source `Other` must register its own qualified identity"
    );
    // The bare binding `Other` denotes the ALIASED source
    // `myapp.mod_a.Reply`, NOT the same-named export `myapp.mod_a.Other`.
    assert_eq!(
        checker.published_bare_type_qualified("Other"),
        Some("myapp.mod_a.Reply".to_string()),
        "aliased binding `Other` must resolve to `myapp.mod_a.Reply`, not the same-named export `myapp.mod_a.Other`"
    );
    // The real `Other` export is not opted in by the alias, so it is not itself
    // published under its own bare name.
    assert!(
        !checker
            .unqualified_to_module
            .contains_key(&(None, 0, "Reply".to_string())),
        "the source name `Reply` is not published bare (only the alias binding `Other` is)"
    );
}

/// P7 — a glob import publishes every exported type bare (intentional opt-in).
#[test]
fn glob_import_type_publishes_bare_binding() {
    let reply = make_pub_struct("Reply", "code");
    let import = make_user_import(
        &["myapp", "mod_a"],
        Some(ImportSpec::Glob),
        vec![(Item::TypeDecl(reply), 0..0)],
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&Program {
        module_graph: None,
        items: vec![(Item::Import(import), 0..0)],
        module_doc: None,
    });

    assert!(
        output.type_defs.contains_key("myapp.mod_a.Reply"),
        "glob import should still register the qualified type"
    );
    assert!(
        checker
            .unqualified_to_module
            .contains_key(&(None, 0, "Reply".to_string())),
        "glob import must publish the importer-scope bare binding for `Reply`"
    );
}

// -- Finding 2: stdlib Hew-source imports obey the same bare-publication gate --
//
// A C-backed stdlib module that also ships Hew source registers through
// `register_stdlib_hew_items`. A plain `import std::…` must NOT expose its
// types bare (so `Server` is reached only as `websocket.Server`), exactly like
// a user-package import; a named opt-in publishes the bare binding; and the
// compiled-in `Prelude` bootstrap surfaces (always-in-scope) keep publishing
// bare unconditionally. The qualified alias + module export are always
// recorded so the qualified spelling and the use-time "exported by module X"
// diagnostic work regardless of the gate.

/// A plain stdlib import (`Import(&None)`) registers the qualified authority
/// but does NOT publish the bare binding — closing the asymmetry where stdlib
/// types slipped past the qualified-by-default gate.
#[test]
fn stdlib_plain_import_does_not_publish_bare_type() {
    let server = make_pub_struct("Server", "fd");
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.modules.insert("websocket".to_string());
    checker.register_stdlib_hew_items(
        "websocket",
        "std.net.websocket",
        &[(Item::TypeDecl(server), 0..0)],
        StdlibBarePublication::Import(&None),
    );

    assert!(
        checker.type_defs.contains_key("std.net.websocket.Server"),
        "plain stdlib import must register the canonical type `std.net.websocket.Server`"
    );
    assert!(!checker.type_defs.contains_key("websocket.Server"));
    assert!(!checker.type_defs.contains_key("Server"));
    assert!(
        checker
            .module_type_exports
            .get("websocket")
            .is_some_and(|s| s.contains("Server")),
        "plain stdlib import must record the module export so the use-time gate names it"
    );
    assert!(
        !checker
            .unqualified_to_module
            .contains_key(&(None, 0, "Server".to_string())),
        "plain stdlib import must NOT publish bare `Server` (qualified-by-default)"
    );
}

/// A named stdlib opt-in (`Import(&Some(Names))`) publishes the bare binding.
#[test]
fn stdlib_named_import_publishes_bare_type() {
    let server = make_pub_struct("Server", "fd");
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.modules.insert("websocket".to_string());
    checker.register_stdlib_hew_items(
        "websocket",
        "std.net.websocket",
        &[(Item::TypeDecl(server), 0..0)],
        StdlibBarePublication::Import(&Some(ImportSpec::Names(vec![ImportName {
            name: "Server".to_string(),
            alias: None,
        }]))),
    );

    assert!(
        checker
            .unqualified_to_module
            .contains_key(&(None, 0, "Server".to_string())),
        "named stdlib opt-in must publish bare `Server`"
    );
}

#[test]
fn stdlib_const_uses_full_registry_owner() {
    let parsed = hew_parser::parse("pub const MAX_READS: i64 = 4096;");
    assert!(parsed.errors.is_empty(), "parse: {:?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.modules.insert("codec".to_string());
    checker.module_import_bindings.insert(
        (None, 0, "codec".to_string()),
        "std.net.http.codec".to_string(),
    );

    checker.register_stdlib_hew_items(
        "codec",
        "std.net.http.codec",
        &parsed.program.items,
        StdlibBarePublication::Import(&None),
    );

    assert!(
        checker
            .env
            .lookup_ref("std.net.http.codec.MAX_READS")
            .is_some(),
        "the constant key must match the exact owner behind the lexical module binding"
    );
    assert!(checker.env.lookup_ref("codec.MAX_READS").is_none());
}

#[test]
fn stdlib_type_binding_is_republished_for_each_importer_after_declaration_dedup() {
    let connection = make_pub_struct("Connection", "fd");
    let resolved_items = vec![(Item::TypeDecl(connection), 0..0)];
    let plain_decl = ImportDecl {
        path: vec!["std".to_string(), "net".to_string()],
        path_separators: Vec::new(),
        spec: None,
        spec_separator: None,
        selection_trailing_comma: false,
        module_alias: None,
        file_path: None,
        resolved_items: Some(resolved_items.clone()),
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: Vec::new(),
    };
    let named_spec = Some(ImportSpec::Names(vec![ImportName {
        name: "Connection".to_string(),
        alias: None,
    }]));
    let named_decl = ImportDecl {
        spec: named_spec.clone(),
        ..plain_decl.clone()
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.current_module = Some("transitive".to_string());
    checker.register_resolved_stdlib_hew_source(
        &plain_decl,
        "std::net",
        "net",
        "std.net",
        &resolved_items,
        StdlibBarePublication::Import(&None),
    );

    checker.current_module = None;
    checker.register_resolved_stdlib_hew_source(
        &named_decl,
        "std::net",
        "net",
        "std.net",
        &resolved_items,
        StdlibBarePublication::Import(&named_spec),
    );

    assert!(
        checker
            .unqualified_to_module
            .contains_key(&(None, 0, "Connection".to_string())),
        "the root's named import must publish Connection even when a transitive importer registered std::net first"
    );
    assert_eq!(
        checker
            .import_type_name_aliases
            .get(&(None, 0, "Connection".to_string()))
            .map(String::as_str),
        Some("std.net.Connection"),
        "HIR must receive the root import's exact source identity"
    );
}

#[test]
fn canonical_stdlib_source_signature_replaces_registry_surface_signature() {
    let parsed = hew_parser::parse(
        "pub enum NetError { Failed(i64); }\n\
         pub fn net_error() -> NetError { NetError::Failed(1) }\n",
    );
    assert!(parsed.errors.is_empty(), "parse: {:?}", parsed.errors);

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    // This is the shape a legacy registry wrapper publishes before the parsed
    // `std::net` source is registered: the surface module name is not a
    // declaration identity. Source publication must replace it regardless of
    // registration order.
    checker.fn_sigs.insert(
        "std.net.net_error".to_string(),
        FnSig {
            return_type: Ty::Named {
                name: "net.NetError".to_string(),
                args: vec![],
                builtin: None,
            },
            ..FnSig::default()
        },
    );

    checker.register_stdlib_hew_items(
        "net",
        "std.net",
        &parsed.program.items,
        StdlibBarePublication::Import(&None),
    );

    let signature = checker
        .fn_sigs
        .get("std.net.net_error")
        .expect("source declaration must publish its canonical signature");
    assert!(matches!(
        signature.return_type,
        Ty::Named { ref name, .. } if name == "std.net.NetError"
    ));
}

/// A compiled-in `Prelude` bootstrap surface publishes its bare binding
/// unconditionally — these are always-in-scope and have no user import.
#[test]
fn stdlib_prelude_publishes_bare_type() {
    let close_error = make_pub_struct("CloseError", "code");
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.modules.insert("closable".to_string());
    checker.register_stdlib_hew_items(
        "closable",
        "std.io.closable",
        &[(Item::TypeDecl(close_error), 0..0)],
        StdlibBarePublication::Prelude,
    );

    assert!(
        checker
            .unqualified_to_module
            .contains_key(&(None, 0, "CloseError".to_string())),
        "prelude bootstrap surface must publish bare `CloseError` unconditionally"
    );
}

#[test]
fn stdlib_nested_private_local_bare_type_uses_full_module_identity() {
    let mut private_wrap = make_pub_struct("Wrap", "value");
    private_wrap.visibility = Visibility::Private;
    let holder = TypeDecl {
        visibility: Visibility::Pub,
        kind: TypeDeclKind::Struct,
        name: "Holder".to_string(),
        type_params: None,
        where_clause: None,
        body: vec![TypeBodyItem::Field {
            name: "wrap".to_string(),
            ty: (
                TypeExpr::Named {
                    name: "Wrap".to_string(),
                    type_args: None,
                },
                0..0,
            ),
            attributes: Vec::new(),
            doc_comment: None,
            span: 0..0,
        }],
        doc_comment: None,
        wire: None,
        is_indirect: false,
        resource_marker: hew_parser::ast::ResourceMarker::None,
        is_opaque: false,
        consuming_methods: Vec::new(),
        lang_item: None,
    };
    let items = vec![
        (Item::TypeDecl(private_wrap.clone()), 0..0),
        (Item::TypeDecl(holder), 0..0),
    ];

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.current_module = Some("std.net.tls".to_string());
    checker.register_type_decl(&private_wrap);
    checker.register_qualified_type_alias("tls", "Wrap");
    checker.register_stdlib_hew_items(
        "tls",
        "std.net.tls",
        &items,
        StdlibBarePublication::Import(&None),
    );

    assert!(
        checker.errors.is_empty(),
        "a private type referenced bare within its nested declaring module must be accessible: {:?}",
        checker.errors
    );
    let holder = checker
        .type_defs
        .get("std.net.tls.Holder")
        .expect("canonical Holder definition must be registered");
    match holder.fields.get("wrap") {
        Some(Ty::Named { name, .. }) => assert_eq!(
            name, "std.net.tls.Wrap",
            "the private local member must retain its exact source-qualified type identity"
        ),
        other => panic!("Holder.wrap must be a named type, got {other:?}"),
    }
}

/// P2 — the canonical definition published for a bare import carries the
/// source definition's fields after temporary registration keys are retired.
#[test]
fn bare_import_type_qualified_alias_has_fields() {
    let reply = make_pub_struct("Reply", "code");
    let import = make_user_import(
        &["myapp", "mod_a"],
        None,
        vec![(Item::TypeDecl(reply), 0..0)],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    let qualified = output
        .type_defs
        .get("myapp.mod_a.Reply")
        .expect("canonical type `myapp.mod_a.Reply` must be registered");
    assert!(
        qualified.fields.contains_key("code"),
        "qualified alias must carry the source def's fields (alias-copy ordering)"
    );
}

fn assert_same_leaf_canonical_type_defs_keep_distinct_shapes(left_first: bool) {
    let left = make_user_import(
        &["pkg", "left"],
        None,
        vec![(Item::TypeDecl(make_pub_struct("Shared", "left_only")), 0..0)],
    );
    let right = make_user_import(
        &["pkg", "right"],
        None,
        vec![(
            Item::TypeDecl(make_pub_struct("Shared", "right_only")),
            0..0,
        )],
    );
    let imports = if left_first {
        vec![(Item::Import(left), 0..0), (Item::Import(right), 0..0)]
    } else {
        vec![(Item::Import(right), 0..0), (Item::Import(left), 0..0)]
    };
    let output = check_items(imports);

    let left_def = output.type_defs.get("pkg.left.Shared").unwrap_or_else(|| {
        panic!(
            "left module must retain its canonical Shared definition; keys: {:?}",
            output.type_defs.keys().collect::<Vec<_>>()
        )
    });
    let right_def = output
        .type_defs
        .get("pkg.right.Shared")
        .expect("right module must retain its canonical Shared definition");
    assert!(left_def.fields.contains_key("left_only"));
    assert!(!left_def.fields.contains_key("right_only"));
    assert!(right_def.fields.contains_key("right_only"));
    assert!(!right_def.fields.contains_key("left_only"));
}

#[test]
fn same_leaf_type_defs_keep_distinct_full_owners_left_then_right() {
    assert_same_leaf_canonical_type_defs_keep_distinct_shapes(true);
}

#[test]
fn same_leaf_type_defs_keep_distinct_full_owners_right_then_left() {
    assert_same_leaf_canonical_type_defs_keep_distinct_shapes(false);
}

#[test]
fn flat_file_owner_selection_ignores_same_leaf_package_owner() {
    let flat = make_user_import(
        &["pkg", "flat"],
        None,
        vec![(Item::TypeDecl(make_pub_struct("Result", "local")), 0..0)],
    );
    let package = make_user_import(
        &["pkg", "package"],
        None,
        vec![(Item::TypeDecl(make_pub_struct("Result", "remote")), 0..0)],
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.check_program(&Program {
        module_graph: None,
        items: vec![(Item::Import(flat), 0..0), (Item::Import(package), 0..0)],
        module_doc: None,
    });
    checker
        .flat_file_import_module_names
        .insert("pkg.flat".to_string());

    assert_eq!(
        checker.flat_file_import_type_owner("Result"),
        Some("pkg.flat.Result".to_string())
    );
}

#[test]
fn canonical_module_variants_shadow_builtin_variants() {
    let output = check_source_in_module(
        r"
        pub enum AppErr { NotFound(string); Timeout; }

        pub fn payload(msg: string) -> AppErr { NotFound(msg) }
        pub fn unit() -> AppErr { Timeout }
        ",
        vec!["shadow".to_string()],
    );

    assert!(
        output.errors.is_empty(),
        "type errors: {:#?}",
        output.errors
    );
}

#[test]
fn same_leaf_named_imports_publish_one_resolved_ty_spelling_per_owner() {
    let selected = |alias: &str| {
        Some(ImportSpec::Names(vec![ImportName {
            name: "Shared".to_string(),
            alias: Some(alias.to_string()),
        }]))
    };
    let left = make_user_import(
        &["pkg", "left"],
        selected("LeftShared"),
        vec![(Item::TypeDecl(make_pub_struct("Shared", "left_only")), 0..0)],
    );
    let right = make_user_import(
        &["pkg", "right"],
        selected("RightShared"),
        vec![(
            Item::TypeDecl(make_pub_struct("Shared", "right_only")),
            0..0,
        )],
    );
    let parsed = hew_parser::parse(
        "fn keep_left(value: LeftShared) -> LeftShared { value }\n\
         fn keep_right(value: RightShared) -> RightShared { value }",
    );
    assert!(
        parsed.errors.is_empty(),
        "fixture parse: {:?}",
        parsed.errors
    );
    let mut items = vec![(Item::Import(left), 0..0), (Item::Import(right), 0..0)];
    items.extend(parsed.program.items);

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&Program {
        items,
        module_graph: None,
        module_doc: None,
    });
    assert!(output.errors.is_empty(), "typecheck: {:?}", output.errors);

    for (function, expected) in [
        ("keep_left", "pkg.left.Shared"),
        ("keep_right", "pkg.right.Shared"),
    ] {
        let signature = output.fn_sigs.get(function).expect("function signature");
        assert!(matches!(
            signature.params.as_slice(),
            [Ty::Named { name, builtin: None, .. }] if name == expected
        ));
        assert!(matches!(
            &signature.return_type,
            Ty::Named { name, builtin: None, .. } if name == expected
        ));
    }

    let resolved_names: HashSet<&str> = output
        .resolved_expr_types
        .values()
        .filter_map(|ty| match ty {
            ResolvedTy::Named {
                name,
                builtin: None,
                ..
            } => Some(name.as_str()),
            _ => None,
        })
        .collect();
    assert!(resolved_names.contains("pkg.left.Shared"));
    assert!(resolved_names.contains("pkg.right.Shared"));
    assert!(resolved_names
        .iter()
        .all(|name| !matches!(*name, "Shared" | "LeftShared" | "RightShared")));

    for alias in [
        "Shared",
        "LeftShared",
        "RightShared",
        "left.Shared",
        "right.Shared",
    ] {
        assert!(
            !output.type_defs.contains_key(alias),
            "TypeDef alias survived: {alias}"
        );
        assert!(
            !checker.type_def_spans.contains_key(alias),
            "declaration-span alias survived: {alias}"
        );
        assert!(
            !checker.registry.has_type_markers(alias),
            "marker-registry alias survived: {alias}"
        );
    }
    for canonical in ["pkg.left.Shared", "pkg.right.Shared"] {
        assert!(output.type_defs.contains_key(canonical));
        assert!(checker.type_def_spans.contains_key(canonical));
        assert!(checker.registry.has_type_markers(canonical));
    }
}

// (The machine arm is structurally identical to the type arm; its gate is
// proven end-to-end by the `import-qual-c2` probe corpus and the examples
// cutover ratchet rather than a hand-built `MachineDecl` literal.)

// -- Glob import: everything unqualified --

#[test]
fn glob_import_registers_unqualified_names() {
    let helper = make_pub_fn(
        "helper",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let other = make_pub_fn(
        "other",
        vec![],
        Some(TypeExpr::Named {
            name: "string".to_string(),
            type_args: None,
        }),
    );
    let import = make_user_import(
        &["myapp", "utils"],
        Some(ImportSpec::Glob),
        vec![
            (Item::Function(helper), 0..0),
            (Item::Function(other), 0..0),
        ],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    // Both qualified and unqualified should be registered
    assert!(output.fn_sigs.contains_key("myapp.utils.helper"));
    assert!(output.fn_sigs.contains_key("myapp.utils.other"));
    assert!(
        output.fn_sigs.contains_key("helper"),
        "glob import should register unqualified 'helper'"
    );
    assert!(
        output.fn_sigs.contains_key("other"),
        "glob import should register unqualified 'other'"
    );
}

// -- Named import: specific names only --

#[test]
fn named_import_registers_specified_names_only() {
    let helper = make_pub_fn(
        "helper",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let other = make_pub_fn(
        "other",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let import = make_user_import(
        &["myapp", "utils"],
        Some(ImportSpec::Names(vec![ImportName {
            name: "helper".to_string(),
            alias: None,
        }])),
        vec![
            (Item::Function(helper), 0..0),
            (Item::Function(other), 0..0),
        ],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    // Both should retain their exact source-qualified declaration identity.
    assert!(output.fn_sigs.contains_key("myapp.utils.helper"));
    assert!(output.fn_sigs.contains_key("myapp.utils.other"));
    // Only "helper" should be unqualified
    assert!(
        output.fn_sigs.contains_key("helper"),
        "named import should register 'helper' unqualified"
    );
    assert!(
        !output.fn_sigs.contains_key("other"),
        "named import should NOT register 'other' unqualified"
    );
}

// -- Pub visibility enforcement --

#[test]
fn non_pub_functions_registered_for_enforcement_but_not_bare() {
    // Private functions ARE registered in fn_sigs under their qualified name so
    // the reference-site enforcement check can produce a precise E_VISIBILITY
    // diagnostic instead of a generic "unknown function" error.  They must NOT
    // receive an unqualified (bare) binding even when the import is a glob.
    let priv_fn = make_priv_fn("secret");
    let pub_fn = make_pub_fn(
        "visible",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let import = make_user_import(
        &["myapp", "utils"],
        Some(ImportSpec::Glob), // even glob should not expose private fns unqualified
        vec![
            (Item::Function(priv_fn), 0..0),
            (Item::Function(pub_fn), 0..0),
        ],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    assert!(
        output.fn_sigs.contains_key("myapp.utils.secret"),
        "private function must be registered under its exact source-qualified name for enforcement"
    );
    assert!(
        !output.fn_sigs.contains_key("secret"),
        "private function must NOT receive an unqualified (bare) binding"
    );
    assert!(output.fn_sigs.contains_key("myapp.utils.visible"));
    assert!(output.fn_sigs.contains_key("visible"));
}

// -- User module const registration --

#[test]
fn user_module_registers_pub_consts() {
    use hew_parser::ast::ConstDecl;

    let pub_const = ConstDecl {
        visibility: Visibility::Pub,
        name: "MAX_SIZE".to_string(),
        ty: (
            TypeExpr::Named {
                name: "i32".to_string(),
                type_args: None,
            },
            0..0,
        ),
        value: make_int_literal(100, 0..3),
        doc_comment: None,
    };
    let priv_const = ConstDecl {
        visibility: Visibility::Private,
        name: "INTERNAL".to_string(),
        ty: (
            TypeExpr::Named {
                name: "i32".to_string(),
                type_args: None,
            },
            0..0,
        ),
        value: make_int_literal(42, 0..2),
        doc_comment: None,
    };
    let import = make_user_import(
        &["myapp", "config"],
        Some(ImportSpec::Glob),
        vec![
            (Item::Const(pub_const), 0..0),
            (Item::Const(priv_const), 0..0),
        ],
    );

    let program = Program {
        module_graph: None,
        items: vec![(Item::Import(import), 0..0)],
        module_doc: None,
    };
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let _output = checker.check_program(&program);

    // pub const should be findable in the environment
    assert!(
        checker.env.lookup_ref("myapp.config.MAX_SIZE").is_some(),
        "pub const should be registered under its exact source owner"
    );
    assert!(
        checker.env.lookup_ref("MAX_SIZE").is_some(),
        "pub const should be unqualified with glob import"
    );
    assert!(
        checker.env.lookup_ref("myapp.config.INTERNAL").is_none(),
        "private const should NOT be registered"
    );
    assert!(
        checker.env.lookup_ref("INTERNAL").is_none(),
        "private const should NOT be registered unqualified"
    );
}

#[test]
fn user_module_const_bare_import_qualified_only() {
    use hew_parser::ast::ConstDecl;

    let pub_const = ConstDecl {
        visibility: Visibility::Pub,
        name: "LIMIT".to_string(),
        ty: (
            TypeExpr::Named {
                name: "i32".to_string(),
                type_args: None,
            },
            0..0,
        ),
        value: make_int_literal(50, 0..2),
        doc_comment: None,
    };
    let import = make_user_import(
        &["myapp", "config"],
        None, // bare import
        vec![(Item::Const(pub_const), 0..0)],
    );

    let program = Program {
        module_graph: None,
        items: vec![(Item::Import(import), 0..0)],
        module_doc: None,
    };
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let _output = checker.check_program(&program);

    assert!(
        checker.env.lookup_ref("myapp.config.LIMIT").is_some(),
        "pub const should be registered under its exact source owner"
    );
    assert!(
        checker.env.lookup_ref("LIMIT").is_none(),
        "bare import should NOT register const unqualified"
    );
}

// -- Module-qualified const field access --

/// `module.CONST` resolves to the const's declared type without an
/// "undefined variable" diagnostic.  This covers the `check_field_access`
/// pre-dispatch added to fix R2 (module-scope const binding).
#[test]
fn module_qualified_const_field_access_resolves() {
    use hew_parser::ast::ConstDecl;

    let pub_const = ConstDecl {
        visibility: Visibility::Pub,
        name: "LIMIT".to_string(),
        ty: (
            TypeExpr::Named {
                name: "i64".to_string(),
                type_args: None,
            },
            0..0,
        ),
        value: make_int_literal(50, 0..2),
        doc_comment: None,
    };
    // Parse a function that references the const via qualified access.
    let mut root = hew_parser::parse(
        r"
import myapp::config;

fn caller() -> i64 {
    config.LIMIT
}
",
    );
    assert!(
        root.errors.is_empty(),
        "program should parse cleanly, got: {:#?}",
        root.errors
    );
    // Inject resolved items into the import so the checker sees the const.
    let import_decl = root
        .program
        .items
        .iter_mut()
        .find_map(|(item, _)| match item {
            Item::Import(imp) => Some(imp),
            _ => None,
        })
        .expect("import decl should exist");
    import_decl.resolved_items = Some(vec![(Item::Const(pub_const), 0..0)]);

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&root.program);
    assert!(
        output.errors.is_empty(),
        "module-qualified const access should resolve cleanly, got: {:#?}",
        output.errors
    );
}

/// Accessing a non-existent const via `module.NONEXISTENT` should produce a
/// targeted "module has no exported constant" diagnostic rather than the leaky
/// "undefined variable `module`" error.
#[test]
fn module_qualified_const_undefined_emits_targeted_diagnostic() {
    use hew_parser::ast::ConstDecl;

    let pub_const = ConstDecl {
        visibility: Visibility::Pub,
        name: "LIMIT".to_string(),
        ty: (
            TypeExpr::Named {
                name: "i64".to_string(),
                type_args: None,
            },
            0..0,
        ),
        value: make_int_literal(50, 0..2),
        doc_comment: None,
    };
    let mut root = hew_parser::parse(
        r"
import myapp::config;

fn caller() -> i64 {
    config.NONEXISTENT
}
",
    );
    assert!(
        root.errors.is_empty(),
        "program should parse cleanly, got: {:#?}",
        root.errors
    );
    let import_decl = root
        .program
        .items
        .iter_mut()
        .find_map(|(item, _)| match item {
            Item::Import(imp) => Some(imp),
            _ => None,
        })
        .expect("import decl should exist");
    import_decl.resolved_items = Some(vec![(Item::Const(pub_const), 0..0)]);

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&root.program);
    assert!(
        output.errors.iter().any(|err| {
            err.kind == TypeErrorKind::PathMemberNotFound
                && err
                    .message
                    .contains("module `config` has no exported value `NONEXISTENT`")
        }),
        "expected targeted 'no exported constant' diagnostic, got: {:#?}",
        output.errors
    );
    // Must NOT produce the leaky "undefined variable `config`" error.
    assert!(
        !output
            .errors
            .iter()
            .any(|err| err.message.contains("undefined variable `config`")),
        "must not emit 'undefined variable `config`' leak, got: {:#?}",
        output.errors
    );
}

// -- User module type registration --

#[test]
fn user_module_registers_types() {
    let struct_decl = TypeDecl {
        visibility: Visibility::Pub,
        kind: TypeDeclKind::Struct,
        name: "Config".to_string(),
        type_params: None,
        where_clause: None,
        body: vec![TypeBodyItem::Field {
            name: "value".to_string(),
            ty: (
                TypeExpr::Named {
                    name: "i32".to_string(),
                    type_args: None,
                },
                0..0,
            ),
            attributes: Vec::new(),
            doc_comment: None,
            span: 0..0,
        }],
        doc_comment: None,
        wire: None,
        is_indirect: false,
        resource_marker: hew_parser::ast::ResourceMarker::None,
        is_opaque: false,
        consuming_methods: Vec::new(),
        lang_item: None,
    };
    let import = make_user_import(
        &["myapp", "config"],
        None, // bare import
        vec![(Item::TypeDecl(struct_decl), 0..0)],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    assert!(
        output.type_defs.contains_key("myapp.config.Config"),
        "user module type should be registered under its full owner"
    );
    assert!(!output.type_defs.contains_key("Config"));
    assert!(!output.type_defs.contains_key("config.Config"));
}

// -- user_modules set --

#[test]
fn user_modules_set_populated() {
    let helper = make_pub_fn(
        "helper",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let import = make_user_import(
        &["myapp", "utils"],
        None,
        vec![(Item::Function(helper), 0..0)],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    assert!(
        output.user_modules.contains("utils"),
        "user_modules should contain the module short name"
    );
}

#[test]
fn stdlib_not_in_user_modules() {
    // A stdlib import should NOT appear in user_modules
    let import = ImportDecl {
        path: vec!["std".to_string(), "fs".to_string()],
        path_separators: Vec::new(),
        spec: None,
        spec_separator: None,
        selection_trailing_comma: false,
        module_alias: None,
        file_path: None,
        resolved_items: None,
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: Vec::new(),
    };
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    assert!(
        !output.user_modules.contains("fs"),
        "stdlib module should NOT be in user_modules"
    );
}

// -- Function signature correctness --

#[test]
fn user_module_fn_sig_has_correct_types() {
    let helper = make_pub_fn(
        "add",
        vec![
            Param {
                name: "a".to_string(),
                ty: (
                    TypeExpr::Named {
                        name: "i32".to_string(),
                        type_args: None,
                    },
                    0..0,
                ),
                is_mutable: false,
                is_consume: false,
            },
            Param {
                name: "b".to_string(),
                ty: (
                    TypeExpr::Named {
                        name: "i32".to_string(),
                        type_args: None,
                    },
                    0..0,
                ),
                is_mutable: false,
                is_consume: false,
            },
        ],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let import = make_user_import(
        &["mylib", "math"],
        None,
        vec![(Item::Function(helper), 0..0)],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    let sig = output
        .fn_sigs
        .get("math.add")
        .expect("math.add should be registered");
    assert_eq!(sig.params.len(), 2, "should have 2 params");
    assert_eq!(sig.params[0], Ty::I32);
    assert_eq!(sig.params[1], Ty::I32);
    assert_eq!(sig.return_type, Ty::I32);
    assert_eq!(sig.param_names, vec!["a", "b"]);
}

// -- Multiple modules don't collide --

#[test]
fn two_modules_same_fn_name_no_collision() {
    let helper_a = make_pub_fn(
        "run",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let helper_b = make_pub_fn(
        "run",
        vec![],
        Some(TypeExpr::Named {
            name: "string".to_string(),
            type_args: None,
        }),
    );
    let import_a = make_user_import(
        &["pkg", "alpha"],
        None,
        vec![(Item::Function(helper_a), 0..0)],
    );
    let import_b = make_user_import(
        &["pkg", "beta"],
        None,
        vec![(Item::Function(helper_b), 0..0)],
    );
    let output = check_items(vec![
        (Item::Import(import_a), 0..0),
        (Item::Import(import_b), 0..0),
    ]);

    assert!(output.fn_sigs.contains_key("alpha.run"));
    assert!(output.fn_sigs.contains_key("beta.run"));
    // Both should have different return types
    assert_eq!(output.fn_sigs["alpha.run"].return_type, Ty::I32);
    assert_eq!(output.fn_sigs["beta.run"].return_type, Ty::String);
}

// -- Import with no resolved items (stdlib) still works --

#[test]
fn import_without_resolved_items_emits_unresolved_error() {
    // An import with resolved_items = None and no stdlib match (empty registry)
    // must now emit an UnresolvedImport error rather than silently dropping.
    let import = ImportDecl {
        path: vec!["unknown".to_string(), "pkg".to_string()],
        path_separators: Vec::new(),
        spec: None,
        spec_separator: None,
        selection_trailing_comma: false,
        module_alias: None,
        file_path: None,
        resolved_items: None,
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: Vec::new(),
    };
    let output = check_items(vec![(Item::Import(import), 0..0)]);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::UnresolvedImport),
        "expected UnresolvedImport error, got: {errors:?}",
        errors = output.errors
    );
    assert!(!output.user_modules.contains("pkg"));
}

#[test]
fn import_with_resolved_items_no_error() {
    // When resolved_items is provided the user-module path is taken and no
    // UnresolvedImport diagnostic should be emitted.
    let import = make_user_import(&["myapp", "util"], None, vec![]);
    let output = check_items(vec![(Item::Import(import), 0..0)]);
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::UnresolvedImport),
        "unexpected UnresolvedImport error for user module with resolved_items"
    );
    assert!(output.user_modules.contains("util"));
}

#[test]
fn stdlib_import_keeps_stream_from_file_stream_typed_after_fs_import() {
    let stream_import = ImportDecl {
        path: vec!["std".to_string(), "stream".to_string()],
        path_separators: Vec::new(),
        spec: None,
        spec_separator: None,
        selection_trailing_comma: false,
        module_alias: None,
        file_path: None,
        resolved_items: None,
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: Vec::new(),
    };
    let fs_import = ImportDecl {
        path: vec!["std".to_string(), "fs".to_string()],
        path_separators: Vec::new(),
        spec: None,
        spec_separator: None,
        selection_trailing_comma: false,
        module_alias: None,
        file_path: None,
        resolved_items: None,
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: Vec::new(),
    };
    let program = Program {
        module_graph: None,
        items: vec![
            (Item::Import(stream_import), 0..0),
            (Item::Import(fs_import), 0..0),
        ],
        module_doc: None,
    };

    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&program);
    let stream_from_file = output
        .fn_sigs
        .get("std.stream.from_file")
        .expect("expected std::stream import to register std.stream.from_file");
    assert!(
        !output.fn_sigs.contains_key("stream.from_file"),
        "the stdlib function registry must not retain a leaf-qualified declaration identity"
    );

    assert_eq!(
        stream_from_file.return_type,
        Ty::result(Ty::stream(Ty::String), Ty::String),
        "std::stream import should keep from_file() typed as Result<Stream<string>, string>"
    );
}

#[test]
fn file_import_without_resolved_items_emits_unresolved_error() {
    let import = ImportDecl {
        path: vec![],
        path_separators: Vec::new(),
        spec: None,
        spec_separator: None,
        selection_trailing_comma: false,
        module_alias: None,
        file_path: Some("missing.hew".to_string()),
        resolved_items: None,
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: Vec::new(),
    };
    let program = Program {
        module_graph: None,
        items: vec![(Item::Import(import), 0..20)],
        module_doc: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);
    let error = output
        .errors
        .iter()
        .find(|e| e.kind == TypeErrorKind::UnresolvedImport)
        .expect("expected UnresolvedImport error for unresolved file import");

    assert!(
        error.message.contains("missing.hew"),
        "unresolved file import should mention the missing file path: {error:?}"
    );
}

#[test]
fn merged_file_import_duplicate_pub_name_rejects_the_whole_import() {
    let shared_decl = make_pub_fn(
        "shared",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let import = ImportDecl {
        path: vec![],
        path_separators: Vec::new(),
        spec: None,
        spec_separator: None,
        selection_trailing_comma: false,
        module_alias: None,
        file_path: Some("pkg.hew".to_string()),
        resolved_items: Some(vec![
            (Item::Function(shared_decl.clone()), 0..5),
            (Item::Function(shared_decl), 10..15),
        ]),
        resolved_item_source_paths: vec![
            std::path::PathBuf::from("pkg/pkg.hew"),
            std::path::PathBuf::from("pkg/helpers.hew"),
        ],
        resolved_source_paths: vec![
            std::path::PathBuf::from("pkg/pkg.hew"),
            std::path::PathBuf::from("pkg/helpers.hew"),
        ],
    };
    let output = check_items(vec![(Item::Import(import), 0..20)]);
    let error = output
        .errors
        .iter()
        .find(|e| e.kind == TypeErrorKind::ImportBindingCollision)
        .expect("merged file import should fail closed on duplicate pub names");

    assert!(
        error.message.contains("shared"),
        "duplicate pub name error should mention the colliding binding: {error:?}"
    );
    assert!(
        !output.fn_sigs.contains_key("shared"),
        "an internally-colliding import must publish none of its declarations"
    );
}

#[test]
fn repeated_flat_file_import_with_same_resolved_source_does_not_reregister_items() {
    let shared_source = std::path::PathBuf::from("pkg/pkg.hew");
    let import = ImportDecl {
        path: vec![],
        path_separators: Vec::new(),
        spec: None,
        spec_separator: None,
        selection_trailing_comma: false,
        module_alias: None,
        file_path: Some("pkg.hew".to_string()),
        resolved_items: Some(vec![(
            Item::Function(make_pub_fn(
                "shared",
                vec![],
                Some(TypeExpr::Named {
                    name: "i32".to_string(),
                    type_args: None,
                }),
            )),
            0..5,
        )]),
        resolved_item_source_paths: vec![shared_source.clone()],
        resolved_source_paths: vec![shared_source],
    };
    let output = check_items(vec![
        (Item::Import(import.clone()), 0..5),
        (Item::Import(import), 10..15),
    ]);

    assert!(
        output.errors.is_empty(),
        "same resolved flat file import should stay idempotent: {:?}",
        output.errors
    );
    assert!(
        output.fn_sigs.contains_key("shared"),
        "flat file import should still register the imported function"
    );
}

#[test]
fn flat_file_imported_pub_fn_publishes_root_call_target() {
    let helper_path = std::path::PathBuf::from("helper.hew");
    let helper = hew_parser::parse("pub fn double(value: i64) -> i64 { value * 2 }");
    let mut root = hew_parser::parse(
        r#"
        import "helper.hew";
        fn main() -> i64 { double(21) }
        "#,
    );
    assert!(
        helper.errors.is_empty(),
        "helper parse: {:?}",
        helper.errors
    );
    assert!(root.errors.is_empty(), "root parse: {:?}", root.errors);

    let import = root
        .program
        .items
        .iter_mut()
        .find_map(|(item, _)| match item {
            Item::Import(import) => Some(import),
            _ => None,
        })
        .expect("root file import");
    import.resolved_items = Some(helper.program.items.clone());
    import.resolved_item_source_paths =
        std::iter::repeat_n(helper_path.clone(), helper.program.items.len()).collect();
    import.resolved_source_paths = vec![helper_path.clone()];

    let root_id = ModuleId::root();
    let helper_id = ModuleId::new(vec!["helper".to_string()]);
    let mut graph = ModuleGraph::new(root_id.clone());
    graph
        .add_module(Module {
            id: helper_id.clone(),
            items: helper.program.items,
            imports: vec![],
            source_paths: vec![helper_path.clone()],
            doc: None,
        })
        .expect("helper module");
    graph
        .add_module(Module {
            id: root_id.clone(),
            items: vec![],
            imports: vec![],
            source_paths: vec!["main.hew".into()],
            doc: None,
        })
        .expect("root module");
    graph.item_sources.insert(
        "helper".to_string(),
        std::iter::repeat_n(helper_path, graph.modules[&helper_id].items.len()).collect(),
    );
    graph.topo_order = vec![helper_id, root_id];

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&Program {
        items: root.program.items,
        module_graph: Some(graph),
        module_doc: None,
    });
    assert!(
        output.errors.is_empty(),
        "flat imported call must typecheck: {:#?}",
        output.errors
    );
    assert!(
        output.direct_call_targets.values().any(|target| matches!(
            target,
            crate::check::dispatch::CallTarget::User(declaration)
                if declaration.full_path() == "helper.double"
        )),
        "flat imported bare call must retain helper.double: {:#?}",
        output.direct_call_targets
    );
}

#[test]
fn repeated_stdlib_import_does_not_duplicate_hew_items() {
    let repo_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .to_path_buf();
    let fs_path = repo_root.join("std/fs.hew");
    let source = std::fs::read_to_string(&fs_path).expect("std/fs.hew should exist");
    let parsed = hew_parser::parse(&source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors in std/fs.hew: {:?}",
        parsed.errors
    );

    let import = ImportDecl {
        path: vec!["std".to_string(), "fs".to_string()],
        path_separators: Vec::new(),
        spec: None,
        spec_separator: None,
        selection_trailing_comma: false,
        module_alias: None,
        file_path: None,
        resolved_items: Some(parsed.program.items),
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: vec![fs_path],
    };
    let program = Program {
        module_graph: None,
        items: vec![
            (Item::Import(import.clone()), 0..0),
            (Item::Import(import), 0..0),
        ],
        module_doc: None,
    };

    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "unexpected errors for repeated stdlib import: {:?}",
        output.errors
    );
    assert!(
        output.type_defs.contains_key("std.fs.IoError"),
        "expected std::fs Hew items to remain registered"
    );
    assert!(!output.type_defs.contains_key("IoError"));
}

// -- Empty module import --

#[test]
fn empty_module_import_no_crash() {
    let import = make_user_import(&["myapp", "empty"], None, vec![]);
    let output = check_items(vec![(Item::Import(import), 0..0)]);
    assert!(output.user_modules.contains("empty"));
    assert!(output.errors.is_empty());
}

// -- Import alias binding --

#[test]
fn import_alias_binds_under_alias_name() {
    // import mymod::{foo as bar} — "bar" must resolve, "foo" must not be unqualified
    let helper = make_pub_fn(
        "foo",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let import = make_user_import(
        &["mymod"],
        Some(ImportSpec::Names(vec![ImportName {
            name: "foo".to_string(),
            alias: Some("bar".to_string()),
        }])),
        vec![(Item::Function(helper), 0..0)],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    // qualified form always uses original name
    assert!(
        output.fn_sigs.contains_key("mymod.foo"),
        "qualified 'mymod.foo' should be registered regardless of alias"
    );
    // unqualified binding must use the alias
    assert!(
        output.fn_sigs.contains_key("bar"),
        "aliased import should register unqualified binding 'bar'"
    );
    // original unqualified name must NOT be registered
    assert!(
        !output.fn_sigs.contains_key("foo"),
        "aliased import must NOT register unqualified 'foo'"
    );
}

#[test]
fn import_alias_multiple_names() {
    // import pkg::{alpha as a, beta as b}
    let fn_alpha = make_pub_fn(
        "alpha",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let fn_beta = make_pub_fn(
        "beta",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let import = make_user_import(
        &["pkg"],
        Some(ImportSpec::Names(vec![
            ImportName {
                name: "alpha".to_string(),
                alias: Some("a".to_string()),
            },
            ImportName {
                name: "beta".to_string(),
                alias: Some("b".to_string()),
            },
        ])),
        vec![
            (Item::Function(fn_alpha), 0..0),
            (Item::Function(fn_beta), 0..0),
        ],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    assert!(
        output.fn_sigs.contains_key("a"),
        "'a' alias should be registered"
    );
    assert!(
        output.fn_sigs.contains_key("b"),
        "'b' alias should be registered"
    );
    assert!(
        !output.fn_sigs.contains_key("alpha"),
        "original 'alpha' must not be unqualified"
    );
    assert!(
        !output.fn_sigs.contains_key("beta"),
        "original 'beta' must not be unqualified"
    );
}

// -- #2202: import alias in type-declaration MEMBER position --
//
// A bare import alias used as a record/struct field type or an enum-variant
// payload type is resolved in Pass 1 (`collect_types`) BEFORE imports are
// processed in Pass 2 (`collect_functions`). The Pass 1.5 re-resolution
// (`reresolve_member_types_after_imports`) upgrades the frozen bare alias to its
// canonical source identity once the alias maps are live, so the stored member
// type matches what the construction site (Pass 3) resolves it to.

/// Build a single-field public struct whose field has the given Named type.
fn make_struct_with_field_ty(name: &str, field: &str, field_type: &str) -> TypeDecl {
    TypeDecl {
        visibility: Visibility::Pub,
        kind: TypeDeclKind::Struct,
        name: name.to_string(),
        type_params: None,
        where_clause: None,
        body: vec![TypeBodyItem::Field {
            name: field.to_string(),
            ty: (
                TypeExpr::Named {
                    name: field_type.to_string(),
                    type_args: None,
                },
                0..0,
            ),
            attributes: Vec::new(),
            doc_comment: None,
            span: 0..0,
        }],
        doc_comment: None,
        wire: None,
        is_indirect: false,
        resource_marker: hew_parser::ast::ResourceMarker::None,
        is_opaque: false,
        consuming_methods: Vec::new(),
        lang_item: None,
    }
}

/// The canonical `Ty` an aliased member must upgrade to.
fn named_ty(name: &str) -> Ty {
    Ty::Named {
        builtin: None,
        name: name.to_string(),
        args: vec![],
    }
}

#[test]
fn import_alias_in_record_field_resolves_to_source_identity() {
    // mod_a exports `pub type Payload { code: i64 }`; root imports it as `Tag`
    // and declares `pub type Boxed { item: Tag }`. The stored field type must be
    // the canonical `myapp.mod_a.Payload`, not the frozen bare alias `Tag` — otherwise
    // the field freezes mismatched against the construction site (#2202).
    let payload = make_pub_struct("Payload", "code");
    let import = make_user_import(
        &["myapp", "mod_a"],
        Some(ImportSpec::Names(vec![ImportName {
            name: "Payload".to_string(),
            alias: Some("Tag".to_string()),
        }])),
        vec![(Item::TypeDecl(payload), 0..0)],
    );
    let boxed = make_struct_with_field_ty("Boxed", "item", "Tag");
    let output = check_items(vec![
        (Item::Import(import), 0..0),
        (Item::TypeDecl(boxed), 0..0),
    ]);

    let boxed_def = output
        .type_defs
        .get("Boxed")
        .expect("`Boxed` must be registered");
    assert_eq!(
        boxed_def.fields.get("item"),
        Some(&named_ty("myapp.mod_a.Payload")),
        "field `item: Tag` must resolve to the canonical source identity \
         `myapp.mod_a.Payload`, not the frozen bare alias `Tag`"
    );
}

#[test]
fn import_alias_in_enum_payload_resolves_to_source_identity() {
    // Root declares `pub enum Wrap { Has(Tag) }`; the variant payload AND its
    // constructor `fn_sig` must both upgrade to the canonical `myapp.mod_a.Payload`.
    let payload = make_pub_struct("Payload", "code");
    let import = make_user_import(
        &["myapp", "mod_a"],
        Some(ImportSpec::Names(vec![ImportName {
            name: "Payload".to_string(),
            alias: Some("Tag".to_string()),
        }])),
        vec![(Item::TypeDecl(payload), 0..0)],
    );
    let wrap = TypeDecl {
        visibility: Visibility::Pub,
        kind: TypeDeclKind::Enum,
        name: "Wrap".to_string(),
        type_params: None,
        where_clause: None,
        body: vec![TypeBodyItem::Variant(hew_parser::ast::VariantDecl {
            name: "Has".to_string(),
            kind: VariantKind::Tuple(vec![(
                TypeExpr::Named {
                    name: "Tag".to_string(),
                    type_args: None,
                },
                0..0,
            )]),
            doc_comment: None,
            span: 0..0,
        })],
        doc_comment: None,
        wire: None,
        is_indirect: false,
        resource_marker: hew_parser::ast::ResourceMarker::None,
        is_opaque: false,
        consuming_methods: Vec::new(),
        lang_item: None,
    };
    let output = check_items(vec![
        (Item::Import(import), 0..0),
        (Item::TypeDecl(wrap), 0..0),
    ]);

    let wrap_def = output
        .type_defs
        .get("Wrap")
        .expect("`Wrap` must be registered");
    assert_eq!(
        wrap_def.variants.get("Has"),
        Some(&VariantDef::Tuple(vec![named_ty("myapp.mod_a.Payload")])),
        "enum variant payload `Has(Tag)` must resolve to `myapp.mod_a.Payload`"
    );
    assert_eq!(
        output.fn_sigs.get("Has").map(|sig| sig.params.clone()),
        Some(vec![named_ty("myapp.mod_a.Payload")]),
        "the variant constructor `Has` must be re-keyed to take `myapp.mod_a.Payload`"
    );
}

#[test]
fn local_type_shadows_import_alias_in_member_position() {
    // Root declares BOTH a local `type Tag { code: i64 }` and imports
    // `Payload as Tag`. The unqualified `Tag` in member position must bind the
    // LOCAL type (local-shadows-imported), never the import's `mod_a.Payload`.
    let payload = make_pub_struct("Payload", "code");
    let import = make_user_import(
        &["myapp", "mod_a"],
        Some(ImportSpec::Names(vec![ImportName {
            name: "Payload".to_string(),
            alias: Some("Tag".to_string()),
        }])),
        vec![(Item::TypeDecl(payload), 0..0)],
    );
    let local_tag = make_pub_struct("Tag", "code");
    let boxed = make_struct_with_field_ty("Boxed", "item", "Tag");
    let output = check_items(vec![
        (Item::Import(import), 0..0),
        (Item::TypeDecl(local_tag), 0..0),
        (Item::TypeDecl(boxed), 0..0),
    ]);

    let boxed_def = output
        .type_defs
        .get("Boxed")
        .expect("`Boxed` must be registered");
    assert_eq!(
        boxed_def.fields.get("item"),
        Some(&named_ty("Tag")),
        "a local `type Tag` must shadow the import alias `Tag` in member position; \
         the field must NOT upgrade to `mod_a.Payload`"
    );
}

#[test]
fn aliased_member_matches_qualified_member_type() {
    // The aliased member (`item: Tag`) and the qualified member
    // (`item: myapp.mod_a.Payload`) must resolve to the SAME stored field type, so
    // every member-derived fact (Send/Copy/Frozen markers, serializable set) is
    // identical regardless of which spelling the user wrote (Risk #1).
    let payload = make_pub_struct("Payload", "code");
    let import = make_user_import(
        &["myapp", "mod_a"],
        Some(ImportSpec::Names(vec![ImportName {
            name: "Payload".to_string(),
            alias: Some("Tag".to_string()),
        }])),
        vec![(Item::TypeDecl(payload), 0..0)],
    );
    let aliased = make_struct_with_field_ty("AliasedBox", "item", "Tag");
    let qualified = make_struct_with_field_ty("QualifiedBox", "item", "myapp.mod_a.Payload");
    let output = check_items(vec![
        (Item::Import(import), 0..0),
        (Item::TypeDecl(aliased), 0..0),
        (Item::TypeDecl(qualified), 0..0),
    ]);

    let aliased_field = output
        .type_defs
        .get("AliasedBox")
        .and_then(|d| d.fields.get("item"));
    let qualified_field = output
        .type_defs
        .get("QualifiedBox")
        .and_then(|d| d.fields.get("item"));
    assert_eq!(
        aliased_field,
        Some(&named_ty("myapp.mod_a.Payload")),
        "the aliased member must resolve to the canonical `myapp.mod_a.Payload`"
    );
    assert_eq!(
        aliased_field, qualified_field,
        "aliased member `Tag` and qualified member `myapp.mod_a.Payload` must resolve to \
         the identical stored field type"
    );
}

// -- Trait import from module --

#[test]
fn import_trait_from_module_glob() {
    use hew_parser::ast::{TraitDecl, TraitItem, TraitMethod};

    let trait_decl = TraitDecl {
        visibility: Visibility::Pub,
        name: "Renderable".to_string(),
        type_params: None,
        super_traits: None,
        items: vec![TraitItem::Method(TraitMethod {
            attributes: vec![],
            consumes_self: false,
            name: "display".to_string(),
            type_params: None,
            params: vec![],
            return_type: None,
            where_clause: None,
            body: None,
            span: 0..0,
            doc_comment: None,
            lang_item: None,
        })],
        doc_comment: None,
        lang_item: None,
    };
    let import = make_user_import(
        &["mylib", "fmt"],
        Some(ImportSpec::Glob),
        vec![(Item::Trait(trait_decl), 0..0)],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    assert!(
        output.errors.is_empty(),
        "importing a pub trait should not produce errors: {:?}",
        output.errors
    );
    // The module should be registered as a user module
    assert!(
        output.user_modules.contains("fmt"),
        "module 'fmt' should be in user_modules"
    );
}

#[test]
fn import_private_trait_not_registered() {
    use hew_parser::ast::{TraitDecl, TraitItem, TraitMethod};

    let private_trait = TraitDecl {
        visibility: Visibility::Private,
        name: "Internal".to_string(),
        type_params: None,
        super_traits: None,
        items: vec![TraitItem::Method(TraitMethod {
            attributes: vec![],
            consumes_self: false,
            name: "internal_op".to_string(),
            type_params: None,
            params: vec![],
            return_type: None,
            where_clause: None,
            body: None,
            span: 0..0,
            doc_comment: None,
            lang_item: None,
        })],
        doc_comment: None,
        lang_item: None,
    };
    let import = make_user_import(
        &["mylib", "internals"],
        Some(ImportSpec::Glob),
        vec![(Item::Trait(private_trait), 0..0)],
    );
    // Should complete without errors; private trait is simply ignored
    let output = check_items(vec![(Item::Import(import), 0..0)]);
    assert!(output.errors.is_empty(), "errors: {:?}", output.errors);
}

// -- Orphan rule warning --

#[test]
fn orphan_impl_emits_warning() {
    use hew_parser::ast::TraitBound;
    // impl ExternalTrait for ExternalType → neither is local → orphan warning
    let impl_decl = ImplDecl {
        type_params: None,
        trait_bound: Some(TraitBound {
            name: "SomeTrait".to_string(),
            type_args: None,
            assoc_type_bindings: vec![],
        }),
        target_type: (
            TypeExpr::Named {
                name: "SomeType".to_string(),
                type_args: None,
            },
            0..0,
        ),
        where_clause: None,
        type_aliases: vec![],
        methods: vec![],
    };
    let output = check_items(vec![(Item::Impl(impl_decl), 0..0)]);

    let has_orphan_warning = output
        .warnings
        .iter()
        .any(|w| w.kind == crate::error::TypeErrorKind::OrphanImpl);
    assert!(
        has_orphan_warning,
        "expected OrphanImpl warning when neither trait nor type is local, got: {:?}",
        output.warnings
    );
}

#[test]
fn local_type_impl_no_orphan_warning() {
    use hew_parser::ast::TraitBound;
    // Locally defined type: impl SomeExternalTrait for LocalType → no orphan warning
    let type_decl = TypeDecl {
        visibility: Visibility::Pub,
        kind: TypeDeclKind::Struct,
        name: "LocalType".to_string(),
        type_params: None,
        where_clause: None,
        body: vec![],
        doc_comment: None,
        wire: None,
        is_indirect: false,
        resource_marker: hew_parser::ast::ResourceMarker::None,
        is_opaque: false,
        consuming_methods: Vec::new(),
        lang_item: None,
    };
    let impl_decl = ImplDecl {
        type_params: None,
        trait_bound: Some(TraitBound {
            name: "ExternalTrait".to_string(),
            type_args: None,
            assoc_type_bindings: vec![],
        }),
        target_type: (
            TypeExpr::Named {
                name: "LocalType".to_string(),
                type_args: None,
            },
            0..0,
        ),
        where_clause: None,
        type_aliases: vec![],
        methods: vec![],
    };
    let output = check_items(vec![
        (Item::TypeDecl(type_decl), 0..0),
        (Item::Impl(impl_decl), 0..0),
    ]);

    let has_orphan = output
        .warnings
        .iter()
        .any(|w| w.kind == crate::error::TypeErrorKind::OrphanImpl);
    assert!(
        !has_orphan,
        "impl on a locally defined type must NOT produce an orphan warning"
    );
}

#[test]
fn local_actor_impl_no_orphan_warning() {
    use hew_parser::ast::{ActorDecl, TraitBound, Visibility};
    // A same-file actor declares a nominal type, so `impl ExternalTrait for
    // Counter` is local-typed — not an orphan. Mirrors the struct case above:
    // the actor's name must seed `local_type_defs` like any other type.
    let actor = ActorDecl {
        visibility: Visibility::Pub,
        name: "Counter".to_string(),
        type_params: vec![],
        super_traits: None,
        init: None,
        fields: vec![],
        receive_fns: vec![],
        methods: vec![],
        mailbox_capacity: None,
        overflow_policy: None,
        is_isolated: false,
        doc_comment: None,
        max_heap_bytes: None,
    };
    let impl_decl = ImplDecl {
        type_params: None,
        trait_bound: Some(TraitBound {
            name: "ExternalTrait".to_string(),
            type_args: None,
            assoc_type_bindings: vec![],
        }),
        target_type: (
            TypeExpr::Named {
                name: "Counter".to_string(),
                type_args: None,
            },
            0..0,
        ),
        where_clause: None,
        type_aliases: vec![],
        methods: vec![],
    };
    let output = check_items(vec![
        (Item::Actor(actor), 0..0),
        (Item::Impl(impl_decl), 0..0),
    ]);

    let has_orphan = output
        .warnings
        .iter()
        .any(|w| w.kind == crate::error::TypeErrorKind::OrphanImpl);
    assert!(
        !has_orphan,
        "impl on a locally defined actor must NOT produce an orphan warning; warnings: {:?}",
        output.warnings
    );
}

#[test]
fn test_file_import_private_items_not_visible() {
    use hew_parser::ast::{
        Block, ConstDecl, Expr, FnDecl, ImportDecl, Item, Literal, Program, Spanned, TypeDecl,
        TypeDeclKind, TypeExpr,
    };

    let private_fn = Item::Function(FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Private,
        name: "private_func".to_string(),
        type_params: None,
        params: vec![],
        return_type: None,
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: None,
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
        consumes_self: false,
    });

    let private_const = Item::Const(ConstDecl {
        visibility: Visibility::Private,
        name: "PRIVATE_CONST".to_string(),
        ty: (
            TypeExpr::Named {
                name: "i64".to_string(),
                type_args: None,
            },
            0..0,
        ),
        value: (
            Expr::Literal(Literal::Integer {
                value: 42,
                radix: hew_parser::ast::IntRadix::Decimal,
            }),
            0..0,
        ),
        doc_comment: None,
    });

    let private_type = Item::TypeDecl(TypeDecl {
        visibility: Visibility::Private,
        kind: TypeDeclKind::Struct,
        name: "PrivateType".to_string(),
        type_params: None,
        where_clause: None,
        body: vec![],
        doc_comment: None,
        wire: None,
        is_indirect: false,
        resource_marker: hew_parser::ast::ResourceMarker::None,
        is_opaque: false,
        consuming_methods: Vec::new(),
        lang_item: None,
    });

    let resolved: Vec<Spanned<Item>> = vec![
        (private_fn, 0..0),
        (private_const, 0..0),
        (private_type, 0..0),
    ];

    let import_decl = ImportDecl {
        path: vec![],
        path_separators: Vec::new(),
        spec: None,
        spec_separator: None,
        selection_trailing_comma: false,
        module_alias: None,
        file_path: Some("private_lib.hew".to_string()),
        resolved_items: Some(resolved),
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: Vec::new(),
    };

    let program = Program {
        module_graph: None,
        items: vec![(Item::Import(import_decl), 0..0)],
        module_doc: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);

    assert!(
        !output.fn_sigs.contains_key("private_func"),
        "private function must not be registered from file import"
    );
    assert!(
        checker.env.lookup("PRIVATE_CONST").is_none(),
        "private const must not be registered from file import"
    );
    assert!(
        !checker.known_types.contains("PrivateType"),
        "private type must not be registered from file import"
    );
}

/// Harness for the qualified-variant-under-expected-nominal rule: a module
/// `m` exporting `enum Mode` + `fn pick(m: Mode)`, checked from a root that
/// reaches `Mode` only through the call's expected parameter type.
fn check_qualified_variant_root(root_source: &str) -> TypeCheckOutput {
    let module = hew_parser::parse(
        "pub enum Mode {\n    A;\n    B;\n    Present(i64);\n    Named { value: i64 }\n}\n\npub type Box<T> {\n    value: T;\n}\n\nimpl<T> Box<T> {\n    pub fn make(value: T) -> Box<T> {\n        Box<T> { value: value }\n    }\n}\n\npub type Factory {\n    marker: i64;\n}\n\nimpl Factory {\n    pub fn make(value: i64) -> i64 {\n        value\n    }\n}\n\npub fn pick(m: Mode) -> i64 {\n    match m {\n        Mode::A => 1,\n        Mode::B => 2,\n        Mode::Present(value) => value,\n        Mode::Named { value } => value,\n    }\n}\n\n#[test]\nfn module_local_unit_variant() {\n    assert(Mode::A == Mode::A);\n}\n",
    );
    assert!(module.errors.is_empty(), "parse: {:?}", module.errors);
    let mut root = hew_parser::parse(root_source);
    assert!(root.errors.is_empty(), "parse: {:?}", root.errors);
    for (item, _) in &mut root.program.items {
        if let Item::Import(import) = item {
            if import.path.as_slice() == ["m"] {
                import.resolved_items = Some(module.program.items.clone());
            }
        }
    }
    let root_id = ModuleId::root();
    let m_id = ModuleId::new(vec!["m".to_string()]);
    let mut module_graph = ModuleGraph::new(root_id.clone());
    module_graph
        .add_module(Module {
            id: m_id.clone(),
            items: module.program.items,
            imports: vec![],
            source_paths: vec![],
            doc: None,
        })
        .expect("add module m");
    module_graph
        .add_module(Module {
            id: root_id.clone(),
            items: root.program.items.clone(),
            imports: vec![],
            source_paths: vec![],
            doc: None,
        })
        .expect("add root");
    module_graph.topo_order = vec![m_id, root_id];
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.check_program(&Program {
        items: root.program.items,
        module_graph: Some(module_graph),
        module_doc: None,
    })
}

/// A selected import publishes a bare type spelling while its declaration is
/// retained only under the module-owned key. Dotted construction must follow
/// that exact binding to the canonical enum rather than probing a retired leaf
/// entry or scanning for a matching final segment.
#[test]
fn imported_bare_enum_dotted_constructor_uses_canonical_owner() {
    let output = check_qualified_variant_root(
        "import m.{ Mode };\n\nfn main() {\n    let value: Mode = Mode.Present(42);\n    let x = m.pick(value);\n    print(\"{x}\");\n}\n",
    );
    assert!(
        output.errors.is_empty(),
        "selected imported enum constructor must resolve canonically: {:?}",
        output.errors
    );
}

/// A plain module import keeps both the module and nominal segments in the
/// expression path. The checker must continue through the export-proven type
/// identity instead of treating `m.Mode` as a value receiver.
#[test]
fn module_qualified_enum_dotted_constructor_uses_canonical_owner() {
    let output = check_qualified_variant_root(
        "import m;\n\nfn main() {\n    let value: m.Mode = m.Mode.Present(42);\n    let x = m.pick(value);\n    print(\"{x}\");\n}\n",
    );
    assert!(
        output.errors.is_empty(),
        "module-qualified enum constructor must resolve canonically: {:?}",
        output.errors
    );
}

#[test]
fn imported_dotted_struct_variants_use_canonical_owner() {
    let selected = check_qualified_variant_root(
        "import m.{ Mode };\n\nfn main() {\n    let value: Mode = Mode.Named { value: 7 };\n    assert(m.pick(value) == 7);\n}\n",
    );
    assert!(
        selected.errors.is_empty(),
        "selected imported struct variant must resolve canonically: {:?}",
        selected.errors
    );

    let qualified = check_qualified_variant_root(
        "import m;\n\nfn main() {\n    let value: m.Mode = m.Mode.Named { value: 7 };\n    assert(m.pick(value) == 7);\n}\n",
    );
    assert!(
        qualified.errors.is_empty(),
        "module-qualified struct variant must resolve canonically: {:?}",
        qualified.errors
    );
}

#[test]
fn dotted_struct_variant_roots_obey_lexical_value_precedence() {
    let module_shadow = check_qualified_variant_root(
        "import m;\n\nfn build(m: i64) -> i64 {\n    let selected: m.Mode = m.Mode.Named { value: 7 };\n    0\n}\n",
    );
    assert!(
        module_shadow.errors.iter().any(|error| {
            error.kind == TypeErrorKind::UndefinedType && error.message.contains("m.Mode.Named")
        }),
        "a parameter named `m` must block module-path struct construction: {:?}",
        module_shadow.errors
    );

    let type_shadow = check_qualified_variant_root(
        "import m.{ Mode };\n\nfn build() -> i64 {\n    let Mode: i64 = 0;\n    let selected: m.Mode = Mode.Named { value: 7 };\n    0\n}\n",
    );
    assert!(
        type_shadow.errors.iter().any(|error| {
            error.kind == TypeErrorKind::UndefinedType && error.message.contains("Mode.Named")
        }),
        "a binding named `Mode` must block selected-type struct construction: {:?}",
        type_shadow.errors
    );
}

#[test]
fn imported_associated_calls_use_canonical_owner() {
    let selected = check_qualified_variant_root(
        "import m.{ Box, Factory };\n\nfn main() {\n    let inferred: Box<i64> = Box.make(42);\n    let explicit: Box<i64> = Box<i64>.make(42);\n    let plain = Factory.make(42);\n    assert(inferred.value == explicit.value);\n    assert(plain == 42);\n}\n",
    );
    assert!(
        selected.errors.is_empty(),
        "selected imported associated calls must resolve canonically: {:?}",
        selected.errors
    );
    assert!(
        !selected
            .warnings
            .iter()
            .any(|warning| warning.kind == TypeErrorKind::UnusedImport),
        "selected generic and non-generic associated calls must credit their imports: {:?}",
        selected.warnings
    );

    let qualified = check_qualified_variant_root(
        "import m;\n\nfn main() {\n    let inferred: m.Box<i64> = m.Box.make(42);\n    let explicit: m.Box<i64> = m.Box<i64>.make(42);\n    assert(inferred.value == explicit.value);\n}\n",
    );
    assert!(
        qualified.errors.is_empty(),
        "module-qualified associated calls must resolve canonically: {:?}",
        qualified.errors
    );
}

/// A qualified unit-variant expression (`Mode::A`) in a position whose
/// expected type is the module-owned nominal resolves through that expected
/// identity — mirroring pattern position — without requiring the bare name
/// to be published by the plain `import`.
#[test]
fn qualified_variant_expression_resolves_through_expected_nominal_identity() {
    let output = check_qualified_variant_root(
        "import m;\n\nfn main() {\n    let x = m.pick(Mode::A);\n    print(\"{x}\");\n}\n",
    );
    assert!(
        output.errors.is_empty(),
        "expected-nominal authority must resolve `Mode::A`; errors: {:?}",
        output.errors
    );
}

/// A root-local enum with the same leaf claims the spelling: `Mode::A` then
/// denotes the LOCAL nominal, which is distinct from `m.Mode` — the pairing
/// stays a type mismatch (no false merge through the expected type).
#[test]
fn local_same_leaf_enum_does_not_merge_with_expected_module_nominal() {
    let output = check_qualified_variant_root(
        "import m;\n\nenum Mode {\n    A;\n    Z;\n}\n\nfn main() {\n    let x = m.pick(Mode::A);\n    print(\"{x}\");\n}\n",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("type mismatch")),
        "local `Mode` is a distinct nominal; errors: {:?}",
        output.errors
    );
}

/// A wrong owner prefix (`Other::A`) is never folded into the expected
/// nominal, even though the variant name matches.
#[test]
fn wrong_owner_variant_prefix_is_rejected_against_expected_nominal() {
    let output = check_qualified_variant_root(
        "import m;\n\nenum Other {\n    A;\n}\n\nfn main() {\n    let x = m.pick(Other::A);\n    print(\"{x}\");\n}\n",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("type mismatch")),
        "`Other` must not fold into `m.Mode`; errors: {:?}",
        output.errors
    );
}
