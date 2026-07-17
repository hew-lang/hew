#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

#[test]
fn should_import_name_bare_import_returns_false() {
    assert!(!Checker::should_import_name("helper", &None));
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

/// P1 — a bare `import m;` of a `pub type` publishes only the qualified
/// binding; the importer-scope bare binding is NOT recorded.
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

    // Qualified authority is always published.
    assert!(
        output.type_defs.contains_key("mod_a.Reply"),
        "bare import should register the qualified type `mod_a.Reply`"
    );
    // The source module's own bare def stays (the alias copy reads it); the
    // importer-scope binding does NOT.
    assert!(
        !checker
            .unqualified_to_module
            .contains_key(&(None, "Reply".to_string())),
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
        output.type_defs.contains_key("mod_a.Reply"),
        "named import should still register the qualified type"
    );
    assert!(
        checker
            .unqualified_to_module
            .contains_key(&(None, "Reply".to_string())),
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
            .contains_key(&(None, "R".to_string())),
        "aliased named import must publish the alias binding `R`"
    );
    assert!(
        !checker
            .unqualified_to_module
            .contains_key(&(None, "Reply".to_string())),
        "aliased named import must NOT publish the source name `Reply`"
    );
}

/// P3-alias-identity — an aliased opt-in (`import m::{ Reply as R }`) makes the
/// bare binding `R` resolve to the SOURCE identity `m.Reply`, not a phantom
/// `m.R`. This is the resolver half of the aliased-import fix: the published-bare
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
        Some("mod_a.Reply".to_string()),
        "aliased binding `R` must resolve to the source identity `mod_a.Reply`, not `mod_a.R`"
    );
    // The reconstructed `mod_a.R` must never exist as a registered def — the bug
    // was binding it (or failing closed) instead of the real source type.
    assert!(
        !checker.type_defs.contains_key("mod_a.R"),
        "no `mod_a.R` def should exist; the alias binds the source `Reply`"
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
        output.type_defs.contains_key("mod_a.Reply"),
        "source `Reply` must register its qualified identity"
    );
    assert!(
        output.type_defs.contains_key("mod_a.Other"),
        "the distinct source `Other` must register its own qualified identity"
    );
    // The bare binding `Other` denotes the ALIASED source `mod_a.Reply`, NOT the
    // same-named export `mod_a.Other`.
    assert_eq!(
        checker.published_bare_type_qualified("Other"),
        Some("mod_a.Reply".to_string()),
        "aliased binding `Other` must resolve to `mod_a.Reply`, not the same-named export `mod_a.Other`"
    );
    // The real `Other` export is not opted in by the alias, so it is not itself
    // published under its own bare name.
    assert!(
        !checker
            .unqualified_to_module
            .contains_key(&(None, "Reply".to_string())),
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
        output.type_defs.contains_key("mod_a.Reply"),
        "glob import should still register the qualified type"
    );
    assert!(
        checker
            .unqualified_to_module
            .contains_key(&(None, "Reply".to_string())),
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
        &[(Item::TypeDecl(server), 0..0)],
        StdlibBarePublication::Import(&None),
    );

    assert!(
        checker.type_defs.contains_key("websocket.Server"),
        "plain stdlib import must register the qualified type `websocket.Server`"
    );
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
            .contains_key(&(None, "Server".to_string())),
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
        &[(Item::TypeDecl(server), 0..0)],
        StdlibBarePublication::Import(&Some(ImportSpec::Names(vec![ImportName {
            name: "Server".to_string(),
            alias: None,
        }]))),
    );

    assert!(
        checker
            .unqualified_to_module
            .contains_key(&(None, "Server".to_string())),
        "named stdlib opt-in must publish bare `Server`"
    );
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
        &[(Item::TypeDecl(close_error), 0..0)],
        StdlibBarePublication::Prelude,
    );

    assert!(
        checker
            .unqualified_to_module
            .contains_key(&(None, "CloseError".to_string())),
        "prelude bootstrap surface must publish bare `CloseError` unconditionally"
    );
}

/// P2 — the qualified type alias is published for a bare import, and its
/// definition mirrors the source module's bare def (the alias-copy ordering
/// canary). If the source module's bare `register_type_decl` is ever dropped,
/// the qualified def goes empty and this test catches it.
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
        .get("mod_a.Reply")
        .expect("qualified type `mod_a.Reply` must be registered");
    assert!(
        qualified.fields.contains_key("code"),
        "qualified alias must carry the source def's fields (alias-copy ordering)"
    );
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
    assert!(output.fn_sigs.contains_key("utils.helper"));
    assert!(output.fn_sigs.contains_key("utils.other"));
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

    // Both should be qualified
    assert!(output.fn_sigs.contains_key("utils.helper"));
    assert!(output.fn_sigs.contains_key("utils.other"));
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
        output.fn_sigs.contains_key("utils.secret"),
        "private function must be registered under its qualified name for enforcement"
    );
    assert!(
        !output.fn_sigs.contains_key("secret"),
        "private function must NOT receive an unqualified (bare) binding"
    );
    assert!(output.fn_sigs.contains_key("utils.visible"));
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
        checker.env.lookup_ref("config.MAX_SIZE").is_some(),
        "pub const should be registered as qualified"
    );
    assert!(
        checker.env.lookup_ref("MAX_SIZE").is_some(),
        "pub const should be unqualified with glob import"
    );
    assert!(
        checker.env.lookup_ref("config.INTERNAL").is_none(),
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
        checker.env.lookup_ref("config.LIMIT").is_some(),
        "pub const should be registered as qualified"
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
            err.kind == TypeErrorKind::UndefinedField
                && err
                    .message
                    .contains("module `config` has no exported constant `NONEXISTENT`")
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
        output.type_defs.contains_key("Config"),
        "user module type should be registered unqualified"
    );
    assert!(
        output.type_defs.contains_key("config.Config"),
        "user module type should also be registered as qualified"
    );
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
        spec: None,
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
        spec: None,
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
        spec: None,
        module_alias: None,
        file_path: None,
        resolved_items: None,
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: Vec::new(),
    };
    let fs_import = ImportDecl {
        path: vec!["std".to_string(), "fs".to_string()],
        spec: None,
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
        .get("stream.from_file")
        .expect("expected std::stream import to register stream.from_file");

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
        spec: None,
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
fn merged_file_import_duplicate_pub_name_emits_duplicate_definition() {
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
        spec: None,
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
        .find(|e| e.kind == TypeErrorKind::DuplicateDefinition)
        .expect("merged file import should fail closed on duplicate pub names");

    assert!(
        error.message.contains("shared"),
        "duplicate pub name error should mention the colliding binding: {error:?}"
    );
    assert_eq!(
        error.notes.first().map(|(span, _)| span.clone()),
        Some(0..5),
        "duplicate pub name should point back to the first merged definition"
    );
}

#[test]
fn repeated_flat_file_import_with_same_resolved_source_does_not_reregister_items() {
    let shared_source = std::path::PathBuf::from("pkg/pkg.hew");
    let import = ImportDecl {
        path: vec![],
        spec: None,
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
        spec: None,
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
        output.type_defs.contains_key("IoError"),
        "expected std::fs Hew items to remain registered"
    );
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
    // the canonical `mod_a.Payload`, not the frozen bare alias `Tag` — otherwise
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
        Some(&named_ty("mod_a.Payload")),
        "field `item: Tag` must resolve to the canonical source identity \
         `mod_a.Payload`, not the frozen bare alias `Tag`"
    );
}

#[test]
fn import_alias_in_enum_payload_resolves_to_source_identity() {
    // Root declares `pub enum Wrap { Has(Tag) }`; the variant payload AND its
    // constructor `fn_sig` must both upgrade to the canonical `mod_a.Payload`.
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
        Some(&VariantDef::Tuple(vec![named_ty("mod_a.Payload")])),
        "enum variant payload `Has(Tag)` must resolve to `mod_a.Payload`"
    );
    assert_eq!(
        output.fn_sigs.get("Has").map(|sig| sig.params.clone()),
        Some(vec![named_ty("mod_a.Payload")]),
        "the variant constructor `Has` must be re-keyed to take `mod_a.Payload`"
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
    // (`item: mod_a.Payload`) must resolve to the SAME stored field type, so
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
    let qualified = make_struct_with_field_ty("QualifiedBox", "item", "mod_a.Payload");
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
        Some(&named_ty("mod_a.Payload")),
        "the aliased member must resolve to the canonical `mod_a.Payload`"
    );
    assert_eq!(
        aliased_field, qualified_field,
        "aliased member `Tag` and qualified member `mod_a.Payload` must resolve to \
         the identical stored field type"
    );
}

// -- Trait import from module --

#[test]
fn import_trait_from_module_glob() {
    use hew_parser::ast::{TraitDecl, TraitItem, TraitMethod};

    let trait_decl = TraitDecl {
        visibility: Visibility::Pub,
        name: "Display".to_string(),
        type_params: None,
        super_traits: None,
        items: vec![TraitItem::Method(TraitMethod {
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
        spec: None,
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
