#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

#[test]
fn indirect_calls_publish_closure_candidates_and_opaque_origins() {
    let source = "fn invoke(f: fn() -> i64) -> i64 {\n    f()\n}\n\ntype Bag {\n    callback: fn() -> i64;\n}\n\nfn main() {\n    let local = || 1;\n    let selected = if true {\n        || 2\n    } else {\n        || 3\n    };\n    let bag = Bag { callback: || 4 };\n    println(local());\n    println(selected());\n    println(bag.callback());\n    println(invoke(|| 5));\n}\n";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let closure = |text: &str| {
        let start = source.find(text).expect("closure literal");
        let key = output
            .closure_escape_facts
            .keys()
            .find(|key| key.start == start)
            .expect("checker-published closure span");
        CallableCandidate::Closure(key.clone())
    };
    let call_key = |text: &str| {
        let start = source.find(text).expect("call expression");
        output
            .direct_call_targets
            .iter()
            .find(|(key, target)| {
                key.start == start
                    && matches!(
                        target,
                        crate::check::dispatch::CallTarget::IndirectFunctionValue
                    )
            })
            .map(|(key, _)| key.clone())
            .expect("checked indirect call")
    };
    assert_eq!(
        output.indirect_call_candidates.get(&call_key("local()")),
        Some(&IndirectCallCandidates {
            known: vec![closure("|| 1")],
            may_be_unknown: false,
        })
    );
    assert_eq!(
        output.indirect_call_candidates.get(&call_key("selected()")),
        Some(&IndirectCallCandidates {
            known: vec![closure("|| 2"), closure("|| 3"),],
            may_be_unknown: false,
        })
    );
    let formal_start = source.find("f: fn").unwrap();
    let Some(crate::check::scope::Resolution::Local(formal)) = output
        .resolutions
        .get(&SpanKey::in_module(&(formal_start..formal_start + 1), 0))
    else {
        panic!("function formal must have exact identity");
    };
    assert_eq!(
        output.indirect_call_candidates.get(&call_key("f()")),
        Some(&IndirectCallCandidates {
            known: vec![CallableCandidate::Formal(*formal)],
            may_be_unknown: false,
        })
    );
    assert_eq!(
        output
            .indirect_call_candidates
            .get(&call_key("bag.callback()")),
        Some(&IndirectCallCandidates {
            known: vec![],
            may_be_unknown: true,
        })
    );
}

#[test]
fn imported_function_value_keeps_its_declaration_at_indirect_call() {
    let source = "import m; fn main() { let f: fn() -> i64 = m.host; println(f()); }";
    let module = hew_parser::parse("pub fn host() -> i64 { 3 }");
    assert!(module.errors.is_empty(), "{:#?}", module.errors);
    let mut parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);
    let (Item::Import(import), _) = &mut parsed.program.items[0] else {
        panic!("expected module import");
    };
    import.resolved_items = Some(module.program.items.clone().into());
    let root = ModulePath::root();
    let m = ModulePath::new(["m"]);
    let mut graph = ModuleGraph::new(root.clone());
    for (id, items) in [
        (m.clone(), module.program.items),
        (root.clone(), parsed.program.items.clone()),
    ] {
        graph
            .add_module(Module {
                id,
                items,
                imports: Vec::new(),
                source_paths: Vec::new(),
                doc: None,
            })
            .unwrap();
    }
    graph.topo_order = vec![m, root];
    parsed.program.module_graph = Some(graph);
    let output = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let declaration = output.defs.lookup_path("m.host").expect("imported host");
    let call = source.rfind("f()").unwrap();
    let key = SpanKey::in_module(&(call..call + 3), 0);
    assert_eq!(
        output.indirect_call_candidates.get(&key),
        Some(&IndirectCallCandidates {
            known: vec![CallableCandidate::Declaration(declaration)],
            may_be_unknown: false,
        })
    );
}

#[test]
fn indirect_branch_keeps_known_closure_and_opaque_parameter() {
    let source = "fn choose(consume opaque: fn() -> i64) -> i64 { \
        let selected = if true { || 1 } else { opaque }; selected() \
    }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let literal_start = source.find("|| 1").unwrap();
    let closure = output
        .closure_escape_facts
        .keys()
        .find(|key| key.start == literal_start)
        .expect("checker-published closure")
        .clone();
    let formal_start = source.find("opaque:").unwrap();
    let Some(crate::check::scope::Resolution::Local(formal)) = output.resolutions.get(
        &SpanKey::in_module(&(formal_start..formal_start + "opaque".len()), 0),
    ) else {
        panic!("opaque parameter must have exact identity");
    };
    let call_start = source.find("selected()").unwrap();
    let call = output
        .direct_call_targets
        .iter()
        .find(|(key, target)| {
            key.start == call_start
                && matches!(
                    target,
                    crate::check::dispatch::CallTarget::IndirectFunctionValue
                )
        })
        .map(|(key, _)| key)
        .expect("checked indirect call");
    assert_eq!(
        output.indirect_call_candidates.get(call),
        Some(&IndirectCallCandidates {
            known: vec![
                CallableCandidate::Closure(closure),
                CallableCandidate::Formal(*formal)
            ],
            may_be_unknown: false,
        })
    );
}

#[test]
fn callable_actuals_follow_exact_formals_through_helpers() {
    let source = "fn invoke(consume f: fn() -> i64) -> i64 { f() } \
        fn forward(consume f: fn() -> i64) -> i64 { invoke(f) } \
        fn main() { let callback = || 1; println(forward(callback)); }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let formal = |marker: &str| {
        let start = source.find(marker).unwrap() + marker.len() - 1;
        let Some(crate::check::scope::Resolution::Local(id)) = output
            .resolutions
            .get(&SpanKey::in_module(&(start..start + 1), 0))
        else {
            panic!("{marker} has no formal identity");
        };
        *id
    };
    let invoke_formal = formal("fn invoke(consume f");
    let forward_formal = formal("fn forward(consume f");
    let closure_start = source.find("|| 1").unwrap();
    let closure = output
        .closure_escape_facts
        .keys()
        .find(|key| key.start == closure_start)
        .expect("checker-published closure")
        .clone();
    let flow = |text: &str| {
        let start = source.rfind(text).unwrap();
        output
            .callable_argument_flows
            .iter()
            .find(|(key, _)| key.start == start)
            .map(|(_, flow)| flow.as_slice())
            .expect("checked argument flow")
    };
    assert_eq!(
        flow("forward(callback)"),
        &[CallableArgumentFlow {
            callee: output.defs.lookup_path("forward").unwrap(),
            formal: forward_formal,
            candidates: IndirectCallCandidates {
                known: vec![CallableCandidate::Closure(closure)],
                may_be_unknown: false,
            },
        }]
    );
    assert_eq!(
        flow("invoke(f)"),
        &[CallableArgumentFlow {
            callee: output.defs.lookup_path("invoke").unwrap(),
            formal: invoke_formal,
            candidates: IndirectCallCandidates {
                known: vec![CallableCandidate::Formal(forward_formal)],
                may_be_unknown: false,
            },
        }]
    );
}

#[test]
fn imported_method_callback_flows_to_its_exact_formal() {
    let module_source = "pub type Runner {\n    value: i64;\n}\n\nimpl Runner {\n    fn apply(self, f: fn() -> i64) -> i64 {\n        f()\n    }\n}\n";
    let source = "import m; fn main() { \
        let runner = m.Runner { value: 0 }; \
        println(runner.apply(|| 1)); \
    }";
    let module = hew_parser::parse(module_source);
    assert!(module.errors.is_empty(), "{:#?}", module.errors);
    let mut parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);
    let (Item::Import(import), _) = &mut parsed.program.items[0] else {
        panic!("expected module import");
    };
    import.resolved_items = Some(module.program.items.clone().into());
    let root = ModulePath::root();
    let m = ModulePath::new(["m"]);
    let mut graph = ModuleGraph::new(root.clone());
    for (id, items) in [
        (m.clone(), module.program.items),
        (root.clone(), parsed.program.items.clone()),
    ] {
        graph
            .add_module(Module {
                id,
                items,
                imports: Vec::new(),
                source_paths: Vec::new(),
                doc: None,
            })
            .unwrap();
    }
    graph.topo_order = vec![m, root];
    parsed.program.module_graph = Some(graph);
    let output = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let call_start = source.find("runner.apply").unwrap();
    let flow = output
        .callable_argument_flows
        .iter()
        .find(|(key, _)| key.module_idx == 0 && key.start == call_start)
        .map(|(_, flow)| flow)
        .expect("imported method argument flow");
    let callback = flow.last().expect("callback formal flow");
    assert_eq!(
        output.defs.kind(callback.callee),
        crate::DeclarationKind::ImplMethod
    );
    assert!(output.defs.path(callback.callee).starts_with("m.Runner::"));
    let closure_start = source.find("|| 1").unwrap();
    let closure = output
        .closure_escape_facts
        .keys()
        .find(|key| key.start == closure_start)
        .unwrap();
    assert_eq!(
        callback.candidates,
        IndirectCallCandidates {
            known: vec![CallableCandidate::Closure(closure.clone())],
            may_be_unknown: false,
        }
    );
    let indirect = output
        .indirect_call_candidates
        .iter()
        .find(|(key, _)| key.module_idx != 0 && key.start == module_source.rfind("f()").unwrap())
        .map(|(_, candidates)| candidates)
        .expect("imported callback invocation");
    assert_eq!(
        indirect.known,
        vec![CallableCandidate::Formal(callback.formal)]
    );
}

#[test]
fn imported_generic_aggregate_publishes_selected_callback_field() {
    let module_source = "pub type Map<A, B> {\n    f: fn(A) -> B;\n}\n\npub fn map<A, B>(consume f: fn(A) -> B) -> Map<A, B> {\n    Map { f: f }\n}\n";
    let source = "import m; fn main() { let mapped = m.map(|x: i64| x + 1); }";
    let module = hew_parser::parse(module_source);
    assert!(module.errors.is_empty(), "{:#?}", module.errors);
    let parsed_field_spans = module.program.items.iter().find_map(|(item, _)| {
        let Item::Function(function) = item else {
            return None;
        };
        let (Expr::StructInit { field_labels, .. }, _) = &**function.body.trailing_expr.as_ref()?
        else {
            return None;
        };
        Some(field_labels.clone())
    });
    assert!(
        parsed_field_spans
            .as_ref()
            .is_some_and(|spans| spans.len() == 1),
        "parsed spans: {parsed_field_spans:?}"
    );
    let mut parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);
    let (Item::Import(import), _) = &mut parsed.program.items[0] else {
        panic!("expected module import");
    };
    import.resolved_items = Some(module.program.items.clone().into());
    let root = ModulePath::root();
    let m = ModulePath::new(["m"]);
    let mut graph = ModuleGraph::new(root.clone());
    for (id, items) in [
        (m.clone(), module.program.items),
        (root.clone(), parsed.program.items.clone()),
    ] {
        graph
            .add_module(Module {
                id,
                items,
                imports: Vec::new(),
                source_paths: Vec::new(),
                doc: None,
            })
            .unwrap();
    }
    graph.topo_order = vec![m, root];
    parsed.program.module_graph = Some(graph);
    let output = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let ctor_start = module_source.find("Map { f: f }").unwrap();
    let fields = output
        .aggregate_field_candidates
        .iter()
        .find(|(key, _)| key.module_idx != 0 && key.start == ctor_start)
        .map(|(_, fields)| fields)
        .expect("imported generic constructor");
    assert_eq!(
        fields.len(),
        1,
        "field origin must be published for imported constructor"
    );
    assert_eq!(
        fields[0].owner,
        output.defs.lookup_nominal("m.Map").unwrap()
    );
    assert_eq!(fields[0].index, 0);
    assert!(!fields[0].candidates.may_be_unknown);
}

#[test]
fn imported_generic_trait_call_publishes_receiver_actual_for_concrete_impl() {
    let module_source = "pub trait Runner {\n    fn run(self) -> i64;\n}\n\npub type Map {\n    f: fn() -> i64;\n}\n\nimpl Runner for Map {\n    fn run(self) -> i64 {\n        (self.f)()\n    }\n}\n\npub fn collect<I: Runner>(it: I) -> i64 {\n    it.run()\n}\n";
    let source = "import m; fn main() { println(m.collect(m.Map { f: || 7 })); }";
    let module = hew_parser::parse(module_source);
    assert!(module.errors.is_empty(), "{:#?}", module.errors);
    let mut parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);
    let (Item::Import(import), _) = &mut parsed.program.items[0] else {
        panic!("expected module import");
    };
    import.resolved_items = Some(module.program.items.clone().into());
    let root = ModulePath::root();
    let m = ModulePath::new(["m"]);
    let mut graph = ModuleGraph::new(root.clone());
    for (id, items) in [
        (m.clone(), module.program.items),
        (root.clone(), parsed.program.items.clone()),
    ] {
        graph
            .add_module(Module {
                id,
                items,
                imports: Vec::new(),
                source_paths: Vec::new(),
                doc: None,
            })
            .unwrap();
    }
    graph.topo_order = vec![m, root];
    parsed.program.module_graph = Some(graph);
    let output = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let call_start = module_source.find("it.run()").unwrap();
    let actuals = output
        .generic_trait_call_arguments
        .iter()
        .find(|(key, _)| key.module_idx != 0 && key.start == call_start)
        .map(|(_, actuals)| actuals)
        .expect("generic trait call actuals");
    assert_eq!(actuals.len(), 1);
    assert_eq!(actuals[0].slot, 0);
    let map_method = output
        .defs
        .ids()
        .find(|id| {
            output.defs.kind(*id) == crate::DeclarationKind::ImplMethod
                && output.defs.path(*id).starts_with("m.Map::<impl ")
                && output.defs.path(*id).ends_with("::run")
        })
        .expect("concrete Map.run declaration");
    assert_eq!(
        output
            .callable_formals
            .get(&crate::check::effects::EffectBody::Declaration(map_method))
            .map(Vec::len),
        Some(1)
    );
    assert!(!actuals[0].candidates.may_be_unknown);
}

#[test]
fn lazy_map_collect_preserves_symbolic_callback_field_origin() {
    let source = "type Map {\n    f: fn(i64) -> i64;\n}\n\nimpl Map {\n    fn next(self, value: i64) -> i64 {\n        (self.f)(value)\n    }\n}\n\nfn map(consume f: fn(i64) -> i64) -> Map {\n    Map { f: f }\n}\n\nfn collect(consume it: Map) -> i64 {\n    it.next(1)\n}\n\nfn main() {\n    println(collect(map(|x: i64| x + 1)));\n}\n";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let map_decl = output.defs.lookup_path("map").expect("map declaration");
    let f_start = source.find("consume f:").unwrap() + "consume ".len();
    let Some(crate::check::scope::Resolution::Local(f_formal)) = output
        .resolutions
        .get(&SpanKey::in_module(&(f_start..f_start + 1), 0))
    else {
        panic!("map callback formal has no identity");
    };
    let aggregate_start = source.find("Map { f: f }").unwrap();
    let aggregate = output
        .aggregate_field_candidates
        .keys()
        .find(|key| key.start == aggregate_start)
        .expect("checked Map constructor");
    assert_eq!(
        output
            .callable_return_candidates
            .get(&crate::check::effects::EffectBody::Declaration(map_decl)),
        Some(&IndirectCallCandidates {
            known: vec![CallableCandidate::Aggregate(aggregate.clone())],
            may_be_unknown: false,
        })
    );
    assert_eq!(
        output.aggregate_field_candidates.get(aggregate),
        Some(&vec![CallableFieldFlow {
            owner: output.defs.lookup_nominal("Map").expect("Map nominal"),
            index: 0,
            candidates: IndirectCallCandidates {
                known: vec![CallableCandidate::Formal(*f_formal)],
                may_be_unknown: false,
            },
        }])
    );
    let map_call_start = source.rfind("map(|x:").unwrap();
    let map_call = output
        .direct_call_targets
        .keys()
        .find(|key| key.start == map_call_start)
        .expect("selected map call");
    let collect_start = source.rfind("collect(map(").unwrap();
    let collect_flow = output
        .callable_argument_flows
        .iter()
        .find(|(key, _)| key.start == collect_start)
        .map(|(_, flows)| flows)
        .expect("collect actual-to-formal flow");
    assert_eq!(
        collect_flow[0].candidates,
        IndirectCallCandidates {
            known: vec![CallableCandidate::CallResult(map_call.clone())],
            may_be_unknown: false,
        }
    );
    let it_start = source.find("consume it:").unwrap() + "consume ".len();
    let Some(crate::check::scope::Resolution::Local(it_formal)) = output
        .resolutions
        .get(&SpanKey::in_module(&(it_start..it_start + 2), 0))
    else {
        panic!("collect iterator formal has no identity");
    };
    let next_start = source.find("it.next(1)").unwrap();
    let next_flow = output
        .callable_argument_flows
        .iter()
        .find(|(key, _)| key.start == next_start)
        .map(|(_, flows)| flows)
        .expect("Map.next receiver flow");
    assert_eq!(
        next_flow[0].candidates,
        IndirectCallCandidates {
            known: vec![CallableCandidate::Formal(*it_formal)],
            may_be_unknown: false,
        }
    );
    let field_start = source.find("self.f").unwrap();
    let field_candidates = output
        .indirect_call_candidates
        .iter()
        .find(|(key, _)| key.start <= field_start && field_start < key.end)
        .map(|(_, candidates)| candidates)
        .expect("Map.next field invocation");
    assert!(matches!(
        field_candidates.known.as_slice(),
        [CallableCandidate::Field { owner, index: 0, .. }]
            if *owner == output.defs.lookup_nominal("Map").unwrap()
    ));
    assert!(!field_candidates.may_be_unknown);
}

#[test]
fn reassigned_function_value_keeps_every_possible_closure() {
    let source = "fn main() { \
        var f: fn() -> i64 = || 1; \
        f = || 2; \
        println(f()); \
    }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let closure = |text: &str| {
        let start = source.find(text).unwrap();
        let key = output
            .closure_escape_facts
            .keys()
            .find(|key| key.start == start)
            .expect("checker-published closure span");
        CallableCandidate::Closure(key.clone())
    };
    let call_start = source.find("f()").unwrap();
    let call = output
        .direct_call_targets
        .keys()
        .find(|key| key.start == call_start)
        .expect("checked indirect call");
    assert_eq!(
        output.indirect_call_candidates.get(call),
        Some(&IndirectCallCandidates {
            known: vec![closure("|| 1"), closure("|| 2")],
            may_be_unknown: false,
        })
    );
}

#[test]
fn authored_static_method_wins_over_runtime_name() {
    let source = "type Node {\n    v: i64;\n}\n\nimpl Node {\n    fn shutdown() {\n        println(\"user shutdown\");\n    }\n}\n\nfn main() {\n    Node.shutdown();\n}\n";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let declaration = *output
        .impl_method_declaration_ids
        .get("Node::shutdown")
        .expect("authored static method identity");
    let start = source.rfind("Node.shutdown()").unwrap();
    let key = SpanKey::in_module(&(start..start + "Node.shutdown()".len()), 0);
    assert!(matches!(
        output.method_call_rewrites.get(&key),
        Some(MethodCallRewrite::RewriteModuleQualifiedToFunction {
            target: crate::check::dispatch::CallTarget::ImplMethod(id),
            ..
        }) if *id == declaration
    ));
}

#[test]
fn function_value_and_direct_call_segments_keep_declarations() {
    let source = "fn helper(value: i64) -> i64 { value } \
        fn main() { let f: fn(i64) -> i64 = helper; \
        let _ = f(4); let _ = helper(5); }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let declaration = output
        .defs
        .lookup_path("helper")
        .expect("helper declaration");
    for start in [
        source.find("= helper").unwrap() + 2,
        source.rfind("helper(5)").unwrap(),
    ] {
        assert_eq!(
            output
                .resolutions
                .get(&SpanKey::in_module(&(start..start + 6), 0)),
            Some(&crate::check::scope::Resolution::Def(declaration))
        );
    }
}

#[test]
fn explicit_generic_function_value_keeps_its_bare_declaration() {
    let source = "fn id<T>(value: T) -> T { value } \
        fn main() { let f = id<i64>; let _ = f(4); }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let declaration = output.defs.lookup_path("id").expect("generic function");
    let start = source.rfind("id<i64>").unwrap();
    assert_eq!(
        output
            .resolutions
            .get(&SpanKey::in_module(&(start..start + 2), 0)),
        Some(&crate::check::scope::Resolution::Def(declaration))
    );
}

#[test]
fn actor_self_projection_publishes_the_state_member() {
    let source = "actor Counter {\n    let value: i64;\n    receive fn get() -> i64 {\n        self.value\n    }\n}\n";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let owner = output
        .defs
        .lookup_path("Counter")
        .expect("actor declaration");
    let field =
        crate::check::scope::Resolution::Field(crate::NominalId::from_minted_declaration(owner), 0);
    let start = source.find("self.value").unwrap();
    let projection = output
        .actor_self_state_fields
        .iter()
        .find(|site| site.start == start)
        .expect("checked actor projection");
    for span in [projection.start..projection.end, start + 5..start + 10] {
        assert_eq!(
            output.resolutions.get(&SpanKey::in_module(&span, 0)),
            Some(&field),
            "actor state use at {span:?}"
        );
    }
}

#[test]
fn actor_self_assignment_publishes_the_state_member() {
    let source = "actor Counter {\n    var value: i64 = 0;\n    receive fn set(next: i64) {\n        self.value = next;\n    }\n}\n";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let owner = output
        .defs
        .lookup_path("Counter")
        .expect("actor declaration");
    let field =
        crate::check::scope::Resolution::Field(crate::NominalId::from_minted_declaration(owner), 0);
    let start = source.find("self.value").unwrap();
    let projection = output
        .actor_self_state_fields
        .iter()
        .find(|site| site.start == start)
        .expect("checked actor assignment target");
    for span in [projection.start..projection.end, start + 5..start + 10] {
        assert_eq!(
            output.resolutions.get(&SpanKey::in_module(&span, 0)),
            Some(&field),
            "actor state write at {span:?}"
        );
    }
}

#[test]
fn imported_generic_function_value_publishes_module_and_member_segments() {
    for (source, surface) in [
        (
            "import m; fn main() { let f: fn(i64) -> i64 = m.id; let _ = f(4); }",
            "m",
        ),
        (
            "import m as alias; fn main() { let f: fn(i64) -> i64 = alias.id; let _ = f(4); }",
            "alias",
        ),
        (
            "import m as alias; fn main() { let f = alias.id<i64>; let _ = f(4); }",
            "alias",
        ),
    ] {
        let module = hew_parser::parse("pub fn id<T>(value: T) -> T { value }");
        assert!(module.errors.is_empty(), "{:#?}", module.errors);
        let mut parsed = hew_parser::parse(source);
        assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);
        let (Item::Import(import), _) = &mut parsed.program.items[0] else {
            panic!("expected module import");
        };
        import.resolved_items = Some(module.program.items.into());
        // In-memory graphs can identify a module by path without carrying
        // source-file paths. The scope binding must still use that exact graph
        // module for every written segment.
        let root = ModulePath::root();
        let m = ModulePath::new(["m"]);
        let mut graph = ModuleGraph::new(root.clone());
        for (id, items) in [
            (root.clone(), Vec::new()),
            (
                m.clone(),
                import.resolved_items.as_ref().unwrap().as_ref().clone(),
            ),
        ] {
            graph
                .add_module(Module {
                    id,
                    items,
                    imports: Vec::new(),
                    source_paths: Vec::new(),
                    doc: None,
                })
                .unwrap();
        }
        graph.topo_order = vec![m, root];
        parsed.program.module_graph = Some(graph);
        let output = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
        assert!(output.errors.is_empty(), "{:#?}", output.errors);
        let function = output.defs.lookup_path("m.id").expect("imported function");
        let module = output.defs.module(function).expect("function owner module");
        let alias = source.rfind(&format!("{surface}.id")).unwrap();
        let member = alias + surface.len() + 1;
        assert_eq!(
            output
                .resolutions
                .get(&SpanKey::in_module(&(alias..alias + surface.len()), 0)),
            Some(&crate::check::scope::Resolution::Module(module))
        );
        assert_eq!(
            output
                .resolutions
                .get(&SpanKey::in_module(&(member..member + 2), 0)),
            Some(&crate::check::scope::Resolution::Def(function))
        );
    }
}

#[test]
fn root_extern_function_shadows_an_imported_function() {
    let source = "import m.{answer}; extern \"C\" { fn answer() -> i64; } \
        fn main() -> i64 { unsafe { answer() } }";
    let imported = hew_parser::parse("pub fn answer() -> i64 { 7 }");
    assert!(imported.errors.is_empty(), "{:#?}", imported.errors);
    let mut parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);
    let (Item::Import(import), _) = &mut parsed.program.items[0] else {
        panic!("expected import");
    };
    import.resolved_items = Some(imported.program.items.clone().into());
    let root = ModulePath::root();
    let m = ModulePath::new(["m"]);
    let mut graph = ModuleGraph::new(root.clone());
    for (id, items) in [
        (m.clone(), imported.program.items),
        (root.clone(), parsed.program.items.clone()),
    ] {
        graph
            .add_module(Module {
                id,
                items,
                imports: Vec::new(),
                source_paths: Vec::new(),
                doc: None,
            })
            .unwrap();
    }
    graph.topo_order = vec![m, root];
    parsed.program.module_graph = Some(graph);
    let output = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let extern_id = output.defs.lookup_path("answer").expect("root extern");
    let start = source.rfind("answer()").unwrap();
    assert_eq!(
        output
            .resolutions
            .get(&SpanKey::in_module(&(start..start + 6), 0)),
        Some(&crate::check::scope::Resolution::Def(extern_id))
    );
    let selected = output
        .direct_call_targets
        .iter()
        .find(|(site, _)| site.module_idx == 0 && site.start == start)
        .map(|(_, target)| target);
    assert!(
        matches!(
            selected,
            Some(crate::check::dispatch::CallTarget::Extern { declaration, .. })
                if *declaration == extern_id
        ),
        "selected target: {selected:?}"
    );
}

#[test]
fn source_resolutions_keep_top_level_consts_as_declarations() {
    let source = "const A: i64 = 10; const B: i64 = A + 1; \
        fn main() -> i64 { B }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let a = output.defs.lookup_path("A").expect("A declaration");
    let b = output.defs.lookup_path("B").expect("B declaration");
    let at = |start| SpanKey::in_module(&(start..start + 1), 0);
    let initializer_a = source.find("= A +").unwrap() + 2;
    let body_b = source.rfind("{ B").unwrap() + 2;
    assert_eq!(
        output.resolutions.get(&at(initializer_a)),
        Some(&crate::check::scope::Resolution::Def(a))
    );
    assert_eq!(
        output.resolutions.get(&at(body_b)),
        Some(&crate::check::scope::Resolution::Def(b))
    );
}

#[test]
fn source_resolutions_publish_imported_const_declarations() {
    let source = "import ma; fn main() -> i64 { ma.C }";
    let module = hew_parser::parse("pub const C: i64 = 7;");
    assert!(module.errors.is_empty(), "{:#?}", module.errors);
    let mut parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);
    let (Item::Import(import), _) = &mut parsed.program.items[0] else {
        panic!("expected module import");
    };
    import.resolved_items = Some(module.program.items.into());
    import.resolved_source_paths = vec![std::path::PathBuf::from("ma.hew")];
    let root = ModulePath::root();
    let ma = ModulePath::new(["ma"]);
    let mut graph = ModuleGraph::new(root.clone());
    for (id, items, path) in [
        (root.clone(), Vec::new(), "main.hew"),
        (
            ma.clone(),
            import.resolved_items.as_ref().unwrap().as_ref().clone(),
            "ma.hew",
        ),
    ] {
        graph
            .add_module(Module {
                id,
                items,
                imports: Vec::new(),
                source_paths: vec![std::path::PathBuf::from(path)],
                doc: None,
            })
            .unwrap();
    }
    graph.topo_order = vec![ma, root];
    parsed.program.module_graph = Some(graph);
    let output = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let c = output
        .defs
        .lookup_path("ma.C")
        .expect("imported C declaration");
    let use_site = source.rfind("ma.C").unwrap() + 3;
    assert_eq!(
        output
            .resolutions
            .get(&SpanKey::in_module(&(use_site..use_site + 1), 0)),
        Some(&crate::check::scope::Resolution::Def(c))
    );
}

#[test]
fn source_resolutions_join_local_definition_and_use() {
    let source = "fn main() { let value: i64 = 1; let next = value; }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let definition = source.find("value:").expect("binding definition");
    let use_site = source.rfind("value;").expect("binding use");
    let at = |start| SpanKey::in_module(&(start..start + "value".len()), 0);
    let declared = output.resolutions.get(&at(definition));
    assert!(matches!(
        declared,
        Some(crate::check::scope::Resolution::Local(_))
    ));
    assert_eq!(output.resolutions.get(&at(use_site)), declared);
}

#[test]
fn source_resolutions_join_unannotated_local_token_and_use() {
    let source = "fn main() { let result = 41; println(result + 1); }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let declared = source.find("result =").unwrap();
    let used = source.rfind("result +").unwrap();
    let at = |start| SpanKey::in_module(&(start..start + "result".len()), 0);
    let binding = output.resolutions.get(&at(declared));
    assert!(matches!(
        binding,
        Some(crate::check::scope::Resolution::Local(_))
    ));
    assert_eq!(output.resolutions.get(&at(used)), binding);
}

#[test]
fn source_resolutions_join_numeric_local_uses_in_short_circuit_comparisons() {
    let source = "fn probe(result: i64, d: i64, max_last_digit: i64) -> bool { \
        let cutoff = -922337203685477580; \
        result < cutoff || result == cutoff && d > max_last_digit }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let positions: Vec<_> = source
        .match_indices("cutoff")
        .map(|(start, _)| start)
        .collect();
    assert_eq!(positions.len(), 3);
    let at = |start| SpanKey::in_module(&(start..start + "cutoff".len()), 0);
    let declared = output.resolutions.get(&at(positions[0]));
    assert!(matches!(
        declared,
        Some(crate::check::scope::Resolution::Local(_))
    ));
    for start in positions.into_iter().skip(1) {
        assert_eq!(output.resolutions.get(&at(start)), declared);
    }
}

#[test]
fn source_resolutions_publish_return_annotation_nominal() {
    let source = "type Point {\n    x: i64;\n}\n\nfn origin() -> Point {\n    Point { x: 1 }\n}\n";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let name = source.find("-> Point").unwrap() + "-> ".len();
    assert!(matches!(
        output
            .resolutions
            .get(&SpanKey::in_module(&(name..name + "Point".len()), 0)),
        Some(crate::check::scope::Resolution::Nominal(_))
    ));
}

#[test]
fn source_resolutions_join_recovery_binding_and_use() {
    let source = "fn recover(problem: string) -> i64 { 7 } \
        fn f(value: Result<i64, string>) -> i64 { value handle problem { recover(problem) } }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let declared = source.find("handle problem").unwrap() + "handle ".len();
    let used = source.rfind("problem)").unwrap();
    let at = |start| SpanKey::in_module(&(start..start + "problem".len()), 0);
    let binding = output.resolutions.get(&at(declared));
    assert!(matches!(
        binding,
        Some(crate::check::scope::Resolution::Local(_))
    ));
    assert_eq!(output.resolutions.get(&at(used)), binding);
}

#[test]
fn source_resolutions_distinguish_shorthand_pattern_binders() {
    let source = "enum Config {\n    Named { key: string; value: string;  }\n    Anonymous;\n}\n\nfn probe(config: Config) -> string {\n    let Config.Named { key, value } = config else {\n        return \"anonymous\";\n    };\n    let _ = key;\n    value\n}\n";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let pattern = source.find("{ key, value }").unwrap();
    let key = pattern + "{ ".len();
    let value = pattern + "{ key, ".len();
    let use_key = source.find("= key;").unwrap() + "= ".len();
    let use_value = source.rfind("value }").unwrap();
    let at = |start, len| SpanKey::in_module(&(start..start + len), 0);
    let key_binding = output.resolutions.get(&at(key, "key".len()));
    let value_binding = output.resolutions.get(&at(value, "value".len()));
    assert!(matches!(
        key_binding,
        Some(crate::check::scope::Resolution::Local(_))
    ));
    assert!(matches!(
        value_binding,
        Some(crate::check::scope::Resolution::Local(_))
    ));
    assert_ne!(key_binding, value_binding);
    assert_eq!(
        output.resolutions.get(&at(use_key, "key".len())),
        key_binding
    );
    assert_eq!(
        output.resolutions.get(&at(use_value, "value".len())),
        value_binding
    );
}

#[test]
fn source_resolutions_join_var_statement_and_use() {
    let source = "fn main() { var x: i64 = 1; x = 2; println(x); }";
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);
    let (Item::Function(main), _) = &parsed.program.items[0] else {
        panic!("expected main function");
    };
    let declaration = &main.body.stmts[0].1;
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let use_site = source.rfind("x)").unwrap();
    let binding = output.resolutions.get(&SpanKey::in_module(declaration, 0));
    assert!(matches!(
        binding,
        Some(crate::check::scope::Resolution::Local(_))
    ));
    let name = source.find("var x").unwrap() + "var ".len();
    let statement_start = source.find("var x").unwrap();
    assert!(
        !output.resolutions.contains_key(&SpanKey::in_module(
            &(statement_start..statement_start + 1),
            0
        )),
        "the `var` keyword must not become an identifier token row"
    );
    assert_eq!(
        output
            .resolutions
            .get(&SpanKey::in_module(&(name..name + 1), 0)),
        binding
    );
    assert_eq!(
        output
            .resolutions
            .get(&SpanKey::in_module(&(use_site..use_site + 1), 0)),
        binding
    );
}

#[test]
fn source_resolutions_do_not_publish_var_keyword_as_a_name() {
    let source = "fn main() { var value = 1; println(value); }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let statement = source.find("var value").unwrap();
    let name = statement + "var ".len();
    let use_site = source.rfind("value)").unwrap();
    let at = |start| SpanKey::in_module(&(start..start + "value".len()), 0);
    let declared = output.resolutions.get(&at(name));
    assert!(matches!(
        declared,
        Some(crate::check::scope::Resolution::Local(_))
    ));
    assert_eq!(output.resolutions.get(&at(use_site)), declared);
    assert!(
        !output.resolutions.contains_key(&at(statement)),
        "`var v` must not overlap the authored name token"
    );
}

#[test]
fn source_resolutions_join_function_and_closure_parameters_to_uses() {
    let source = "fn identity(value: i64) -> i64 { value } \
        fn main() { let f = |n: i64| -> i64 { n + 1 }; println(f(identity(2))); }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    for (name, definition, use_site) in [
        (
            "value",
            source.find("value:").unwrap(),
            source.find("value }").unwrap(),
        ),
        (
            "n",
            source.find("n: i64").unwrap(),
            source.find("n +").unwrap(),
        ),
    ] {
        let at = |start| SpanKey::in_module(&(start..start + name.len()), 0);
        let declared = output.resolutions.get(&at(definition));
        assert!(matches!(
            declared,
            Some(crate::check::scope::Resolution::Local(_))
        ));
        assert_eq!(
            output.resolutions.get(&at(use_site)),
            declared,
            "{name}: {:?}",
            output.resolutions
        );
    }
}

#[test]
fn source_resolutions_join_implicit_self_to_its_use() {
    let source = "type Box {\n    value: i64;\n}\n\nimpl Box {\n    fn get(self) -> i64 {\n        self.value\n    }\n}\n";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let definition = source.find("self)").unwrap();
    let use_site = source.find("self.value").unwrap();
    let at = |start| SpanKey::in_module(&(start..start + "self".len()), 0);
    let declared = output.resolutions.get(&at(definition));
    assert!(matches!(
        declared,
        Some(crate::check::scope::Resolution::Local(_))
    ));
    assert_eq!(output.resolutions.get(&at(use_site)), declared);
}

#[test]
fn source_resolutions_distinguish_same_named_fields_by_owner() {
    let source = "type A {\n    x: i64;\n}\n\ntype B {\n    x: i64;\n}\n\nfn main() {\n    let a = A { x: 1 };\n    let b = B { x: 2 };\n    println(a.x);\n    println(b.x);\n}\n";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let a = source.find("a.x").expect("A field") + 2;
    let b = source.find("b.x").expect("B field") + 2;
    let a_label = source.find("let a = A { x:").expect("A initializer") + "let a = A { ".len();
    let b_label = source.find("let b = B { x:").expect("B initializer") + "let b = B { ".len();
    let at = |start| SpanKey::in_module(&(start..start + 1), 0);
    let a_field = output.resolutions.get(&at(a));
    let b_field = output.resolutions.get(&at(b));
    assert!(matches!(
        a_field,
        Some(crate::check::scope::Resolution::Field(_, 0))
    ));
    assert!(matches!(
        b_field,
        Some(crate::check::scope::Resolution::Field(_, 0))
    ));
    assert_ne!(a_field, b_field);
    assert_eq!(output.resolutions.get(&at(a_label)), a_field);
    assert_eq!(output.resolutions.get(&at(b_label)), b_field);
}

#[test]
fn source_resolutions_publish_qualified_record_constructor_segments() {
    let source = "import ma; fn main() { let shape = ma.Shape { x: 1 }; println(shape.x); }";
    let module = hew_parser::parse("pub type Shape {\n    x: i64;\n}\n");
    assert!(module.errors.is_empty(), "{:#?}", module.errors);
    let mut parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);
    let (Item::Import(import), _) = &mut parsed.program.items[0] else {
        panic!("expected module import");
    };
    import.resolved_items = Some(module.program.items.into());
    import.resolved_source_paths = vec![std::path::PathBuf::from("ma.hew")];
    let root = ModulePath::root();
    let ma = ModulePath::new(["ma"]);
    let mut graph = ModuleGraph::new(root.clone());
    graph
        .add_module(Module {
            id: root.clone(),
            items: Vec::new(),
            imports: Vec::new(),
            source_paths: vec![std::path::PathBuf::from("main.hew")],
            doc: None,
        })
        .unwrap();
    graph
        .add_module(Module {
            id: ma.clone(),
            items: import.resolved_items.as_ref().unwrap().as_ref().clone(),
            imports: Vec::new(),
            source_paths: import.resolved_source_paths.clone(),
            doc: None,
        })
        .unwrap();
    graph.topo_order = vec![ma, root];
    parsed.program.module_graph = Some(graph);
    let output = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let start = source.find("ma.Shape {").unwrap();
    let at = |offset, len| SpanKey::in_module(&(offset..offset + len), 0);
    assert!(matches!(
        output.resolutions.get(&at(start, 2)),
        Some(crate::check::scope::Resolution::Module(_))
    ));
    assert!(matches!(
        output.resolutions.get(&at(start + 3, 5)),
        Some(crate::check::scope::Resolution::Nominal(_))
    ));
}

#[test]
fn source_resolutions_join_actor_field_uses_across_handlers() {
    let source = "actor Counter {\n    let count: i64;\n    receive fn get() -> i64 {\n        count\n    }\n    receive fn next() -> i64 {\n        count + 1\n    }\n}\n";
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);
    let (Item::Actor(actor), _) = &parsed.program.items[0] else {
        panic!("expected actor");
    };
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let declaration = output
        .resolutions
        .get(&SpanKey::in_module(&actor.fields[0].name_span, 0));
    assert!(matches!(
        declaration,
        Some(crate::check::scope::Resolution::Field(_, 0))
    ));
    let count_sites = source
        .match_indices("count")
        .map(|(index, _)| index)
        .collect::<Vec<_>>();
    assert_eq!(count_sites.len(), 3, "one declaration and two handler uses");
    for written in count_sites.into_iter().skip(1) {
        assert_eq!(
            output
                .resolutions
                .get(&SpanKey::in_module(&(written..written + "count".len()), 0)),
            declaration
        );
    }
}

#[test]
fn source_resolutions_publish_selected_function_and_method() {
    let source = "type A {\n    x: i64;\n}\n\nimpl A {\n    fn get(self) -> i64 {\n        self.x\n    }\n}\n\nfn helper() -> i64 {\n    1\n}\n\nfn main() {\n    let a = A { x: 2 };\n    println(helper());\n    println(a.get());\n}\n";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let helper = source.rfind("helper()").expect("helper call");
    let get = source.rfind("get()").expect("method call");
    let at = |start, len| SpanKey::in_module(&(start..start + len), 0);
    let Some(crate::check::scope::Resolution::Def(function)) =
        output.resolutions.get(&at(helper, "helper".len()))
    else {
        panic!(
            "function call has no declaration resolution: {:?}",
            output.resolutions
        );
    };
    assert_eq!(output.defs.name(*function).as_str(), "helper");
    let Some(crate::check::scope::Resolution::Member(method)) =
        output.resolutions.get(&at(get, "get".len()))
    else {
        panic!(
            "method call has no declaration resolution: {:?}",
            output.resolutions
        );
    };
    assert_eq!(output.defs.name(*method).as_str(), "get");
}

#[test]
fn source_resolutions_publish_trait_bound_method_declaration() {
    let source = "trait Describable {\n    fn describe(self) -> string;\n}\n\ntype Label {\n    text: string;\n}\n\nimpl Describable for Label {\n    fn describe(self) -> string {\n        self.text\n    }\n}\n\nfn probe<T: Describable>(item: T) -> string {\n    item.describe()\n}\n";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let method = source.find("item.describe()").unwrap() + "item.".len();
    let key = SpanKey::in_module(&(method..method + "describe".len()), 0);
    let Some(crate::check::scope::Resolution::Member(selected)) = output.resolutions.get(&key)
    else {
        panic!(
            "trait-bound call has no selected declaration: {:?}",
            output.resolutions
        );
    };
    let trait_id = output.defs.lookup_path("Describable").unwrap();
    assert_eq!(output.defs.owner(*selected), Some(trait_id));
    assert_eq!(
        output.defs.kind(*selected),
        crate::DeclarationKind::TraitMethod
    );
}

#[test]
fn checker_output_contract_intersects_assignment_target_side_tables() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.assign_target_kinds.insert(
        SpanKey {
            start: 1,
            end: 2,
            module_idx: 0,
        },
        AssignTargetKind::LocalVar,
    );
    checker.assign_target_shapes.insert(
        SpanKey {
            start: 3,
            end: 4,
            module_idx: 0,
        },
        AssignTargetShape { is_unsigned: false },
    );

    let mut expr_types = HashMap::new();
    let mut type_defs = HashMap::new();
    let mut fn_sigs = HashMap::new();
    let mut call_type_args = HashMap::new();
    let mut record_init_type_args = HashMap::new();
    checker.validate_checker_output_contract(
        &mut expr_types,
        &mut type_defs,
        &mut fn_sigs,
        &mut call_type_args,
        &mut record_init_type_args,
    );

    assert!(
        checker.assign_target_kinds.is_empty(),
        "orphan assign_target_kinds entries should be pruned at the output boundary: {:?}",
        checker.assign_target_kinds
    );
    assert!(
        checker.assign_target_shapes.is_empty(),
        "orphan assign_target_shapes entries should be pruned at the output boundary: {:?}",
        checker.assign_target_shapes
    );
}

#[test]
fn expr_output_contract_rechecks_normalized_unresolved_subset() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let sender_var = TypeVar::fresh();
    let covered_var = TypeVar::fresh();
    let span = SpanKey {
        start: 10,
        end: 20,
        module_idx: 0,
    };
    let mut expr_types = HashMap::from([(
        span.clone(),
        Ty::Tuple(vec![
            Ty::named_for_test("Sender", vec![Ty::Var(sender_var)]),
            Ty::Var(covered_var),
        ]),
    )]);

    // Channel endpoints carry ordinary type parameters now, so a var inside
    // `Sender<?a>` is an ordinary unresolved var: it is covered or it is a leak.
    // The uncovered case is the sibling negative control
    // (`validate_expr_output_contract_reports_and_prunes_ty_var_leak`).
    checker
        .validate_expr_output_contract(&mut expr_types, &HashSet::from([covered_var, sender_var]));

    assert!(
        checker
            .errors
            .iter()
            .all(|error| error.kind != TypeErrorKind::InferenceFailed),
        "normalized covered vars must not emit InferenceFailed: {checker_errors:#?}",
        checker_errors = checker.errors
    );
    assert!(
        !expr_types.contains_key(&span),
        "covered unresolved expr types should still be pruned after normalization: {expr_types:?}"
    );
}

// ── method-call output-contract validation ───────────────────────────────────

/// Valid method-call metadata must survive the output-contract boundary when
/// the corresponding `expr_types` entry is present and fully resolved.
#[test]
fn checker_output_contract_retains_valid_method_call_metadata() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let span = SpanKey {
        start: 10,
        end: 20,
        module_idx: 0,
    };
    checker.method_call_receiver_kinds.insert(
        span.clone(),
        MethodCallReceiverKind::NamedTypeInstance {
            type_name: "Foo".to_string(),
        },
    );
    checker
        .method_call_rewrites
        .insert(span.clone(), MethodCallRewrite::DeferToLowering);

    // expr_types has the matching span with a concrete, fully-resolved type.
    let mut expr_types = HashMap::new();
    expr_types.insert(span.clone(), Ty::I64);
    // type_defs must include "Foo" so validate_method_call_receiver_kinds_output_contract
    // retains the NamedTypeInstance entry after validate_method_call_output_contract passes it.
    let mut type_defs = HashMap::from([(
        crate::NominalId::from_minted_declaration(checker.defs.mint_for_test("Foo")),
        TypeDef {
            kind: TypeDefKind::Struct,
            name: "Foo".to_string(),
            type_params: vec![],
            bounds: crate::check::ParamBounds::default(),
            fields: HashMap::new(),
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: None,
            field_order: vec![],
            is_indirect: false,
        },
    )]);
    let mut fn_sigs = HashMap::new();
    let mut call_type_args = HashMap::new();
    let mut record_init_type_args = HashMap::new();
    checker.validate_checker_output_contract(
        &mut expr_types,
        &mut type_defs,
        &mut fn_sigs,
        &mut call_type_args,
        &mut record_init_type_args,
    );

    assert!(
        checker.method_call_receiver_kinds.contains_key(&span),
        "valid method_call_receiver_kinds entry must be retained: {:?}",
        checker.method_call_receiver_kinds
    );
    assert!(
        checker.method_call_rewrites.contains_key(&span),
        "valid method_call_rewrites entry must be retained: {:?}",
        checker.method_call_rewrites
    );
}

/// Orphaned method-call metadata — where the corresponding `expr_types` span
/// was pruned — must be removed at the output-contract boundary.
#[test]
fn checker_output_contract_prunes_orphaned_method_call_metadata() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    // Insert metadata keyed to spans that have NO corresponding expr_types entry.
    checker.method_call_receiver_kinds.insert(
        SpanKey {
            start: 10,
            end: 20,
            module_idx: 0,
        },
        MethodCallReceiverKind::NamedTypeInstance {
            type_name: "Bar".to_string(),
        },
    );
    checker.method_call_rewrites.insert(
        SpanKey {
            start: 30,
            end: 40,
            module_idx: 0,
        },
        MethodCallRewrite::RewriteToFunction {
            target: CallTarget::Unsupported {
                reason: "orphaned test metadata".to_string(),
            },
            c_symbol: "hew_bar_method".to_string(),
            descriptor: None,
            extern_identity: None,
            consumes_receiver: false,
            requires_mutable_receiver: false,
            receiver_update: crate::ReceiverUpdate::Replace,
            returns_receiver_identity: false,
        },
    );

    // expr_types is empty — no span survives.
    let mut expr_types = HashMap::new();
    let mut type_defs = HashMap::new();
    let mut fn_sigs = HashMap::new();
    let mut call_type_args = HashMap::new();
    let mut record_init_type_args = HashMap::new();
    checker.validate_checker_output_contract(
        &mut expr_types,
        &mut type_defs,
        &mut fn_sigs,
        &mut call_type_args,
        &mut record_init_type_args,
    );

    assert!(
        checker.method_call_receiver_kinds.is_empty(),
        "orphan method_call_receiver_kinds entries must be pruned: {:?}",
        checker.method_call_receiver_kinds
    );
    assert!(
        checker.method_call_rewrites.is_empty(),
        "orphan method_call_rewrites entries must be pruned: {:?}",
        checker.method_call_rewrites
    );
}

/// When a method-call expression's `expr_types` entry is pruned because it
/// carries an unresolved inference variable (simulating a failed / error-typed
/// receiver), the corresponding receiver-kind and rewrite side-table entries
/// must not leak to the output.
#[test]
fn checker_output_contract_prunes_method_call_metadata_for_leaked_inference_var_expr() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let leaked_span = SpanKey {
        start: 50,
        end: 60,
        module_idx: 0,
    };
    let good_span = SpanKey {
        start: 70,
        end: 80,
        module_idx: 0,
    };

    // The leaked span has an unresolved inference var — validate_expr_output_contract
    // will strip it from expr_types, so the method-call metadata must follow.
    checker.method_call_receiver_kinds.insert(
        leaked_span.clone(),
        MethodCallReceiverKind::NamedTypeInstance {
            type_name: "Bad".to_string(),
        },
    );
    checker
        .method_call_rewrites
        .insert(leaked_span.clone(), MethodCallRewrite::DeferToLowering);
    // The good span carries a fully-resolved type and its metadata should survive.
    checker.method_call_receiver_kinds.insert(
        good_span.clone(),
        MethodCallReceiverKind::NamedTypeInstance {
            type_name: "Good".to_string(),
        },
    );
    checker
        .method_call_rewrites
        .insert(good_span.clone(), MethodCallRewrite::DeferToLowering);

    // Build expr_types: leaked entry has a fresh (unresolved) inference var;
    // good entry carries a concrete type.
    let mut expr_types = HashMap::new();
    expr_types.insert(leaked_span.clone(), Ty::Var(TypeVar::fresh()));
    expr_types.insert(good_span.clone(), Ty::Bool);

    // type_defs must include "Good" so validate_method_call_receiver_kinds_output_contract
    // retains the NamedTypeInstance entry for the good span after the span-based pruner passes it.
    let mut type_defs = HashMap::from([(
        crate::NominalId::from_minted_declaration(checker.defs.mint_for_test("Good")),
        TypeDef {
            kind: TypeDefKind::Struct,
            name: "Good".to_string(),
            type_params: vec![],
            bounds: crate::check::ParamBounds::default(),
            fields: HashMap::new(),
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: None,
            field_order: vec![],
            is_indirect: false,
        },
    )]);
    let mut fn_sigs = HashMap::new();
    let mut call_type_args = HashMap::new();
    let mut record_init_type_args = HashMap::new();
    checker.validate_checker_output_contract(
        &mut expr_types,
        &mut type_defs,
        &mut fn_sigs,
        &mut call_type_args,
        &mut record_init_type_args,
    );

    // The leaked span must have been pruned from expr_types by
    // validate_expr_output_contract, which in turn must cascade to prune the
    // orphaned method-call metadata.
    assert!(
        !expr_types.contains_key(&leaked_span),
        "leaked inference-var expr must be pruned from expr_types"
    );
    assert!(
        !checker
            .method_call_receiver_kinds
            .contains_key(&leaked_span),
        "method_call_receiver_kinds entry for pruned expr must not survive: {:?}",
        checker.method_call_receiver_kinds
    );
    assert!(
        !checker.method_call_rewrites.contains_key(&leaked_span),
        "method_call_rewrites entry for pruned expr must not survive: {:?}",
        checker.method_call_rewrites
    );

    // The good span must be retained in all three maps.
    assert!(
        expr_types.contains_key(&good_span),
        "fully-resolved expr must be retained in expr_types"
    );
    assert!(
        checker.method_call_receiver_kinds.contains_key(&good_span),
        "method_call_receiver_kinds entry for valid expr must survive: {:?}",
        checker.method_call_receiver_kinds
    );
    assert!(
        checker.method_call_rewrites.contains_key(&good_span),
        "method_call_rewrites entry for valid expr must survive: {:?}",
        checker.method_call_rewrites
    );
}

#[test]
fn module_qualified_call_rewrites_record_owning_module_endpoint() {
    let parsed = hew_parser::parse(
        r#"
import std.fs;

fn main() {
    let _ = fs.exists("test.txt");
}
"#,
    );
    assert!(
        parsed.errors.is_empty(),
        "expected clean parse, got: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&parsed.program);
    assert!(
        output.errors.is_empty(),
        "expected clean typecheck, got: {:#?}",
        output.errors
    );
    assert!(
        output.method_call_rewrites.values().any(|rewrite| matches!(
            rewrite,
            MethodCallRewrite::RewriteModuleQualifiedToFunction { c_symbol, .. }
                if c_symbol == "std.fs.exists"
        )),
        "expected the module-qualified rewrite to name the owning module endpoint, got: {:?}",
        output.method_call_rewrites
    );
}

#[test]
fn module_qualified_pure_hew_stdlib_wrapper_rewrites_to_qualified_symbol() {
    let parsed = hew_parser::parse(
        r#"
import std.path;

fn main() {
    let _ = path.dirname("a/b");
}
"#,
    );
    assert!(
        parsed.errors.is_empty(),
        "expected clean parse, got: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&parsed.program);
    assert!(
        output.errors.is_empty(),
        "expected clean typecheck, got: {:#?}",
        output.errors
    );
    assert!(
        output.method_call_rewrites.values().any(|rewrite| matches!(
            rewrite,
            MethodCallRewrite::RewriteModuleQualifiedToFunction { c_symbol, .. }
                if c_symbol == "std.path.dirname"
        )),
        "expected pure-Hew stdlib wrapper to rewrite to module-qualified symbol, got: {:?}",
        output.method_call_rewrites
    );
}

#[test]
fn tail_ok_publication_preserves_the_source_payload_type() {
    let source = "fn wrap(value: i64) -> i64 fails string {\n    value\n}\n";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let start = source.rfind("value\n").expect("tail identifier");
    let key = output
        .tail_ok_coercions
        .iter()
        .find(|key| key.start == start && key.module_idx == 0)
        .expect("tail identifier must carry the Ok-coercion marker");
    assert_eq!(output.expr_types.get(key), Some(&Ty::I64));
}

#[test]
fn scope_body_with_spawned_call_and_trailing_value_checks_cleanly() {
    let output = check_source(
        r"
        actor Worker {
            receive fn run() {}
        }

        fn main() {
            scope {
                let worker = spawn Worker;
                let _ = worker.run();
                0
            };
        }
        ",
    );

    assert!(
        output.errors.is_empty(),
        "scope body with a spawned worker and trailing value must typecheck cleanly; got: {:#?}",
        output.errors
    );
}

// Helper functions for testing AST construction

#[test]
fn empty_select_and_match_preserve_source_diagnostics() {
    for (source, expects_error) in [
        // A select with no arms waits on nothing and is refused; an empty
        // match keeps reporting its scrutinee's own diagnostic.
        ("fn main() { let _ = select {}; }", true),
        ("fn main() { let _ = match missing() {}; }", true),
    ] {
        let parsed = hew_parser::parse(source);
        assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&parsed.program);
        assert_eq!(
            !output.errors.is_empty(),
            expects_error,
            "{source}: {:#?}",
            output.errors
        );
    }
}

#[test]
fn expected_variant_type_reaches_nested_binding_blocks() {
    let parsed = hew_parser::parse("enum Value {\n    Text { text: string;  }\n}\n\nfn main() {\n    let value: Value = {\n        {\n            .Text { text: \"retained\" }\n        }\n    };\n    match value {\n        .Text { text } => println(text),\n    }\n}\n");
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&parsed.program);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

#[test]
fn source_type_parameters_publish_distinct_declaration_owned_ids() {
    let source = "type T { value: i64; } fn first<T>(consume value: T) -> T { value } fn second<T>(consume value: T) -> T { value }";
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let output = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    let mut parameters = Vec::new();
    for (item, _) in &parsed.program.items {
        let Item::Function(function) = item else {
            continue;
        };
        let hew_parser::ast::TypeExpr::Named { path, .. } = &function.params[0].ty.0 else {
            panic!("parameter annotation");
        };
        let key = SpanKey::in_module(&path.segments[0].1, 0);
        let Some(crate::check::scope::Resolution::Param(parameter)) = output.resolutions.get(&key)
        else {
            panic!(
                "generic annotation did not select its binder: {:?}",
                output.resolutions.get(&key)
            );
        };
        assert_eq!(
            parameter.owner,
            output
                .defs
                .lookup_path(function.name.name.as_str())
                .unwrap()
        );
        assert_eq!(parameter.index, 0);
        parameters.push(*parameter);
    }
    assert_eq!(parameters.len(), 2);
    assert_ne!(parameters[0], parameters[1]);
}
