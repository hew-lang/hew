//! Checker methods grouped by responsibility: builtins.
//! Split from `registration.rs`: checker methods, part 1 of 6.
#![allow(
    unused_imports,
    redundant_imports,
    clippy::wildcard_imports,
    reason = "chunk files share the parent module's import header"
)]
use super::super::types::ImportBindingKey;
#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::super::*;
use super::*;
use crate::BuiltinType;
use hew_parser::ast::Ident;
use hew_parser::ast::WireMetadata;

impl Checker {
    #[expect(
        clippy::too_many_lines,
        reason = "all builtins registered in one place"
    )]
    pub(in crate::check) fn register_builtins(&mut self) {
        // Print functions
        self.register_builtin_fn("println_int", vec![Ty::I64], Ty::Unit);
        self.register_builtin_fn("println_str", vec![Ty::String], Ty::Unit);
        self.register_builtin_fn("print_int", vec![Ty::I64], Ty::Unit);
        self.register_builtin_fn("print_str", vec![Ty::String], Ty::Unit);
        self.register_builtin_fn("println_float", vec![Ty::F64], Ty::Unit);
        self.register_builtin_fn("println_bool", vec![Ty::Bool], Ty::Unit);
        self.register_builtin_fn("print_float", vec![Ty::F64], Ty::Unit);
        self.register_builtin_fn("print_bool", vec![Ty::Bool], Ty::Unit);
        // Generic print/println require Display.
        self.register_builtin_fn_with_bounds(
            "println",
            vec!["T".to_string()],
            HashMap::from([("T".to_string(), vec!["Display".to_string()])]),
            vec![Ty::Named {
                builtin: None,
                name: "T".to_string(),
                args: vec![],
            }],
            Ty::Unit,
        );
        self.register_builtin_fn_with_bounds(
            "print",
            vec!["T".to_string()],
            HashMap::from([("T".to_string(), vec!["Display".to_string()])]),
            vec![Ty::Named {
                builtin: None,
                name: "T".to_string(),
                args: vec![],
            }],
            Ty::Unit,
        );

        // Numeric conversion. The math functions live in `std.math`; there is
        // no bare spelling for them (A409).

        // String operations
        self.register_builtin_fn_with_bounds(
            "to_string",
            vec!["T".to_string()],
            HashMap::from([("T".to_string(), vec!["Display".to_string()])]),
            vec![Ty::Named {
                builtin: None,
                name: "T".to_string(),
                args: vec![],
            }],
            Ty::String,
        );
        self.register_builtin_fn("len", vec![Ty::Var(TypeVar::fresh())], Ty::I64);

        // I/O and system
        // `instant::now` has no source declaration, but it is an ordinary
        // compiler-provided runtime call.  Register its joined parser spelling
        // here so the checker publishes the same typed runtime target that
        // HIR and MIR consume for every other builtin call.
        self.register_builtin_fn(
            "instant::now",
            vec![],
            Ty::Named {
                name: "instant".to_string(),
                args: vec![],
                builtin: Some(BuiltinType::Instant),
            },
        );
        self.register_builtin_fn("sleep", vec![Ty::Duration], Ty::Unit);
        for view in ["mailbox", "policy"] {
            self.register_builtin_fn(
                view,
                vec![
                    Ty::Var(TypeVar::fresh()),
                    crate::actor_delivery::nominal(crate::actor_delivery::ON_FULL_TYPE, Vec::new()),
                ],
                Ty::Var(TypeVar::fresh()),
            );
        }
        self.register_builtin_fn(
            "sleep_until",
            vec![Ty::Named {
                name: "instant".to_string(),
                args: vec![],
                builtin: Some(BuiltinType::Instant),
            }],
            Ty::Unit,
        );
        // `close(actor)` requests a cooperative stop and waits for terminal
        // cleanup; `closed(actor)` waits without requesting. Both are ordinary
        // calls, so `fork close(actor)` is the non-waiting request.
        // An actor handle is the actor's own type, so these signatures carry a
        // free variable and the call-site arms in `calls.rs` require an actor
        // handle by the checked fact rather than by a wrapper type.
        self.register_builtin_fn("close", vec![Ty::Var(TypeVar::fresh())], Ty::Unit);
        self.register_builtin_fn("closed", vec![Ty::Var(TypeVar::fresh())], Ty::Unit);
        self.register_builtin_fn("exit", vec![Ty::I64], Ty::Never);
        self.register_builtin_fn("panic", vec![Ty::String], Ty::Never);

        // Link and monitor share the closed LinkError vocabulary. A monitor
        // returns the owned handle used to end its registration.
        self.register_builtin_fn(
            "link",
            vec![Ty::Var(TypeVar::fresh())],
            Ty::result(Ty::Unit, Ty::link_error()),
        );
        self.register_builtin_fn("unlink", vec![Ty::Var(TypeVar::fresh())], Ty::Unit);
        self.register_builtin_fn(
            "monitor",
            vec![Ty::Var(TypeVar::fresh())],
            Ty::result(Ty::monitor_ref(), Ty::link_error()),
        );
        // Cross-node link: `link_remote(RemotePid<T>, PartitionPolicy)`
        // links a local actor to a remote actor so the remote's death fires the
        // per-link `PartitionPolicy` (`CrashLinked` crashes the local actor). The
        // immediate return is `Result<(), LinkError>` (registration success — the
        // EXIT arrives async). The `PartitionPolicy` enum is declared in
        // `std/link_monitor.hew`; the call-site checker validates the precise arg
        // type. The local `link(<actor handle>)` form keeps its 1-arg shape; a
        // `link(RemotePid)` is rejected with a "use link_remote" diagnostic
        // (`calls.rs`), so the cross-node link is never a silent type mismatch.
        let link_remote_t = TypeVar::fresh();
        self.register_builtin_fn(
            "link_remote",
            vec![
                Ty::remote_pid(Ty::Var(link_remote_t)),
                Ty::Named {
                    name: "PartitionPolicy".to_string(),
                    args: vec![],
                    builtin: None,
                },
            ],
            Ty::result(Ty::Unit, Ty::link_error()),
        );

        // Supervisor child access
        self.register_builtin_fn(
            "supervisor_child",
            vec![Ty::Var(TypeVar::fresh()), Ty::I64],
            Ty::Var(TypeVar::fresh()),
        );
        self.register_builtin_fn("supervisor_stop", vec![Ty::Var(TypeVar::fresh())], Ty::Unit);

        // `assert(condition)`; the message form is admitted in `assertion.rs`.
        self.register_builtin_fn("assert", vec![Ty::Bool], Ty::Unit);

        // Option/Result constructors
        // Option/Result constructors are handled specially in check_call
        // (they need fresh linked type vars per invocation)

        // Collection constructors (path-style calls: Vec::new(), HashMap::new()).
        // Declared generic so every call site instantiates its own element
        // variables: `var a = Vec.new()` and `var b = Vec.new()` in one body
        // must be free to settle on different element types. The constructor
        // family is the executable identity whatever those variables become,
        // so it is published here rather than reconstructed from a spelling.
        self.register_collection_constructor(
            "Vec::new",
            BuiltinType::Vec,
            &["T"],
            crate::runtime_call::RuntimeCallFamily::VecNew,
        );
        self.register_builtin_fn_with_bounds(
            "Vec::with_capacity",
            vec!["T".to_string()],
            HashMap::new(),
            vec![Ty::I64],
            Ty::Named {
                builtin: Some(BuiltinType::Vec),
                name: "Vec".to_string(),
                args: vec![Ty::named("T", vec![])],
            },
        );
        self.register_collection_constructor(
            "HashMap::new",
            BuiltinType::HashMap,
            &["K", "V"],
            crate::runtime_call::RuntimeCallFamily::HashMapNew,
        );
        self.register_collection_constructor(
            "HashSet::new",
            BuiltinType::HashSet,
            &["T"],
            crate::runtime_call::RuntimeCallFamily::HashSetNew,
        );
        self.register_builtin_fn("bytes::new", vec![], Ty::Bytes);

        // Rc<T> constructor — Rc::new(value: T) -> Rc<T>
        // Each call site gets a fresh type variable; the actual Rc<T> type is
        // inferred from the argument type.
        {
            let t = TypeVar::fresh();
            let family = crate::runtime_call::RuntimeCallFamily::RcNew;
            let signature_key = family
                .checker_signature_key()
                .expect("RcNew has a checker signature identity");
            self.register_builtin_fn(signature_key, vec![Ty::Var(t)], Ty::rc(Ty::Var(t)));
            self.builtin_call_targets
                .insert(signature_key.to_string(), CallTarget::Runtime(family));
        }

        // More print variants
        self.register_builtin_fn("println_f64", vec![Ty::F64], Ty::Unit);
        self.register_builtin_fn("print_f64", vec![Ty::F64], Ty::Unit);
        self.register_builtin_fn("println_i64", vec![Ty::I64], Ty::Unit);
        self.register_builtin_fn("println_char", vec![Ty::Char], Ty::Unit);

        // String utilities
        self.register_builtin_fn("int_to_string", vec![Ty::I64], Ty::String);
        self.register_builtin_fn("float_to_string", vec![Ty::F64], Ty::String);
        self.register_builtin_fn("char_to_string", vec![Ty::Char], Ty::String);
        self.register_builtin_fn("bool_to_string", vec![Ty::Bool], Ty::String);

        // Node/distributed builtins
        let node_config = Ty::Named {
            builtin: None,
            name: "NodeConfig".to_string(),
            args: vec![],
        };
        let node_error = Ty::builtin_named(BuiltinType::NodeError, vec![]);
        let node_result = Ty::result(Ty::Unit, node_error);
        self.register_builtin_fn("Node::start", vec![node_config], node_result.clone());
        self.register_builtin_fn("Node::shutdown", vec![], Ty::Unit);
        self.register_builtin_fn("Node::connect", vec![Ty::String], node_result);
        self.register_builtin_fn("Node::identity_key", vec![], Ty::String);
        self.register_builtin_fn(
            "Node::id",
            vec![],
            Ty::option(Ty::builtin_named(BuiltinType::NodeId, vec![])),
        );
        // `Node::register(name: String, actor: A) -> i32`. Codegen assumes a
        // local actor handle (it calls `hew_actor_pid` to extract the u64 before
        // forwarding to `hew_node_api_register_by_pid`), so the call-site arm in
        // `calls.rs` requires one; a `RemotePid<T>` or bare `u64` is caught
        // there rather than failing with a cryptic codegen error.
        self.register_builtin_fn(
            "Node::register",
            vec![Ty::String, Ty::Var(TypeVar::fresh())],
            Ty::I32,
        );
        // `Node::lookup<T>(name: String) -> Result<RemotePid<T>, LookupError>`.
        // The runtime extern returns a packed `u64` pid (0 == not found); the
        // codegen branch lowers this into a `Result` construction inline.
        self.register_builtin_fn_with_bounds(
            "Node::lookup",
            vec!["T".to_string()],
            HashMap::new(),
            vec![Ty::String],
            Ty::result(
                Ty::remote_pid(Ty::Named {
                    builtin: None,
                    name: "T".to_string(),
                    args: vec![],
                }),
                crate::builtin_enums::monomorphic_builtin_enum_ty("LookupError")
                    .expect("generated builtin enum catalog must contain LookupError"),
            ),
        );

        // Register the compiled-in primitive/builtin receiver impls that must
        // be visible without an explicit stdlib import: Display blanket impls
        // from `std/builtins.hew`, plus declarative string/bytes FFI receiver
        // methods from `std/string.hew` and `std/io.hew`.
        self.register_builtins_hew_impls();
        self.register_builtin_error_prelude_bindings();
        // Crash-hook signatures are an import-free language surface in both
        // inline and on-disk programs. Register only their `CrashInfo` and
        // `CrashAction` declarations before module-graph discovery;
        // `CrashNotification` remains source-import-only for `#[on(exit)]`.
        self.register_builtin_failure_surface();
        if !self.module_registry.has_search_paths() {
            self.register_builtin_monitor_ref_surface();
        }
    }

    /// Parse compiled-in stdlib receiver impl sources and feed only the
    /// selected `Item::Impl` blocks through the existing stdlib registration
    /// path.
    ///
    /// `register_stdlib_hew_items` runs Pass 1 (types/traits/functions) and
    /// Pass 2 (impl methods) on its input.  For `std/builtins.hew` we
    /// deliberately filter to just the impl items so:
    ///
    /// - The `pub trait Display { fn fmt(...) }` declaration is not
    ///   inserted into `trait_defs`, leaving the existing user-redeclare
    ///   path untouched (a user's in-file `trait Display` continues to win
    ///   namespace registration via `register_type_namespace_name`).
    /// - The `pub fn println(value: dyn Display)` etc. wrapper signatures
    ///   in builtins.hew do not collide with the `register_builtin_fn_with_bounds`
    ///   registrations above, which already encode the canonical
    ///   `T: Display`-bounded shape these helpers expose to user code.
    ///
    /// Pass 2 does not validate `trait_bound.name` against `trait_defs`; it
    /// only requires the target type name to canonicalise to a primitive or
    /// builtin generic key.  All eleven impls (`i8`–`i64`, `u8`–`u64`,
    /// `bool`, `char`) target primitives that round-trip through
    /// `Ty::from_name` → `canonical_lowering_name`, so each one lands as a
    /// `(canonical_key, "Display") → { "fmt" → FnSig }` entry in
    /// `primitive_trait_impls`.
    pub(super) fn register_builtins_hew_impls(&mut self) {
        const BUILTINS_HEW_SOURCE: &str = include_str!("../../../../std/builtins.hew");
        let parsed = hew_parser::parse(BUILTINS_HEW_SOURCE);
        // The compiled-in source is part of the build; a parse failure is
        // a compiler bug, not a user-facing error.  Surface it loudly in
        // debug builds so contributors notice; in release, fail closed by
        // skipping registration (the existing "no method `fmt` on int"
        // diagnostic is the worst-case fallback, which matches today's
        // pre-fix behaviour).
        debug_assert!(
            parsed.errors.is_empty(),
            "std/builtins.hew failed to parse: {:?}",
            parsed.errors
        );
        if !parsed.errors.is_empty() {
            return;
        }
        let builtins_module = self.defs.mint_module("std.builtins", &[]);
        for (item_ordinal, (item, span)) in parsed.program.items.iter().enumerate() {
            self.mint_item_declaration_identities(
                Some(builtins_module),
                Some(builtins_module),
                crate::check::NominalNamespace::Owned,
                item_ordinal,
                item,
                span,
            );
        }
        // Pre-register the public trait/type definitions from builtins.hew
        // into `trait_defs` / `type_defs` WITHOUT claiming `type_def_spans`
        // for them.  The
        // checker output-boundary validator (admissibility.rs:491-505)
        // retains `MethodCallReceiverKind::PrimitiveTraitImpl` entries only
        // when their `trait_name` is present in `trait_defs`; without this
        // pre-registration, the dispatch metadata for `x.fmt()` would be
        // pruned at the boundary even though `primitive_trait_impls` was
        // populated correctly.  Skipping `register_type_namespace_name`
        // preserves the user-redeclare path: a user `pub trait Display`
        // declared in their own source file still registers cleanly (no
        // duplicate-definition error) and overwrites the trait_defs entry
        // with the user's version.  The primitive_trait_impls side table
        // remains keyed independently by canonical receiver kind so the
        // `x.fmt()` dispatch continues to find the builtins-registered
        // impl regardless of which `trait_defs[Display]` shape is current.
        for (item, span) in &parsed.program.items {
            match item {
                Item::Trait(tr) if tr.visibility.is_pub() => {
                    self.pre_register_builtin_trait(tr, span);
                }
                Item::TypeDecl(td) if td.visibility.is_pub() => {
                    let saved_module = self.current_module.replace("std.builtins".to_string());
                    self.pre_register_type_decl(td);
                    self.current_module = saved_module;
                    let canonical = format!("std.builtins.{}", td.name);
                    if let Some(source_def) = self.type_defs.get(&canonical).cloned() {
                        self.register_canonical_type_def(
                            "std.builtins",
                            td.name.name.as_str(),
                            &source_def,
                        );
                    }
                    // Compiler-carrier builtins (`RemotePid`, `NodeId`, ...)
                    // retain the catalog's canonical identity; this source file
                    // supplies their declarative surface but does not turn them
                    // into `std.builtins.*` user nominals. Builtin error enums
                    // that need an explicit bare prelude binding are published
                    // separately after every declaration has its true owner.
                    if crate::lookup_builtin_type(td.name.name.as_str()).is_none() {
                        self.record_published_bare_type(
                            td.name.name.as_str(),
                            &format!("std.builtins.{}", td.name),
                        );
                    }
                }
                _ => {}
            }
        }
        // Now feed only the `Item::Impl` blocks through the existing
        // stdlib registration path.  Pass 1 of `register_stdlib_hew_items`
        // is a no-op on this filtered list (no traits/types/functions to
        // register), and Pass 2 records each `impl Display for <prim>` in
        // `primitive_trait_impls` via the same `record_primitive_trait_impl_method`
        // helper that user-source impls go through.  All eleven targets
        // (i8/i16/i32/i64/u8/u16/u32/u64/bool/char) round-trip through
        // `Ty::from_name` → `canonical_lowering_name`, so each lands as a
        // `(canonical_key, "Display") → { "fmt" → FnSig }` entry.
        self.register_embedded_builtin_externs(&parsed.program);
        let impl_items: Vec<Spanned<Item>> = parsed
            .program
            .items
            .into_iter()
            .filter(|(item, _)| matches!(item, Item::Impl(_)))
            .collect();
        if impl_items.is_empty() {
            return;
        }
        // Module short name "builtins" matches the on-disk file stem and
        // would be the namespace if anything ever imports `std::builtins`
        // directly; nothing currently does, so this name is only visible
        // as the qualified-key prefix on per-method `td.methods` insertions
        // (none of which fire for primitive targets that lack a
        // `type_defs` entry).
        self.register_stdlib_hew_items(
            "builtins",
            "std.builtins",
            &impl_items,
            StdlibBarePublication::Prelude,
        );
        self.register_compiled_stdlib_receiver_impls(
            "string",
            include_str!("../../../../std/string.hew"),
            &["string"],
        );
        self.register_compiled_stdlib_receiver_impls(
            "io",
            include_str!("../../../../std/io.hew"),
            &["bytes"],
        );
        self.register_compiled_stdlib_receiver_impls(
            "option",
            include_str!("../../../../std/option.hew"),
            &["Option"],
        );
        self.register_compiled_stdlib_receiver_impls(
            "result",
            include_str!("../../../../std/result.hew"),
            &["Result"],
        );
    }

    pub(super) fn register_embedded_builtin_externs(&mut self, program: &hew_parser::ast::Program) {
        // Embedded resource owners use the same source extern declarations
        // and lifecycle contracts as imported modules. Their definitions do
        // not have a module-graph collection pass to register these later.
        let saved_module = self.current_module.replace("std.builtins".to_string());
        let saved_origin = self
            .registration_origin_module
            .replace("std.builtins".to_string());
        for (item, span) in &program.items {
            if let Item::ExternBlock(block) = item {
                self.register_extern_block(block, span);
            }
        }
        self.current_module = saved_module;
        self.registration_origin_module = saved_origin;
        for (item, _) in &program.items {
            if let Item::ExternBlock(block) = item {
                for function in &block.functions {
                    let canonical = format!("std.builtins.{}", function.name);
                    self.publish_stdlib_hew_function_binding(
                        function.name.to_string(),
                        &canonical,
                        StdlibBarePublication::Prelude,
                    );
                    self.builtin_call_targets.insert(
                        function.name.to_string(),
                        self.call_target_for_signature(&canonical),
                    );
                }
            }
        }
    }

    /// Pre-register one `pub trait` declared by `std/builtins.hew`.
    ///
    /// Seeds `trait_defs` under the bare, `builtins.`- and `std.builtins.`-
    /// qualified spellings, registers the trait's method signatures under the
    /// `std.builtins` module, and harvests its `#[lang_item("…")]` keys.
    /// Nothing here claims `type_def_spans` or `lang_item_spans`: a user file
    /// that declares its own `Display` (or builtins.hew itself, when it is the
    /// file under check) must re-register cleanly rather than collide with the
    /// seed.
    pub(super) fn pre_register_builtin_trait(&mut self, tr: &TraitDecl, span: &Span) {
        let info = Self::trait_info_from_decl(
            tr,
            Some("std.builtins".to_string()),
            self.current_module_idx,
        );
        self.trait_defs
            .entry(tr.name.to_string())
            .or_insert_with(|| info.clone());
        let qualified = format!("builtins.{}", tr.name);
        self.trait_defs
            .entry(qualified)
            .or_insert_with(|| info.clone());
        let canonical = format!("std.builtins.{}", tr.name);
        self.trait_defs.entry(canonical.clone()).or_insert(info);
        // A builtin trait's supertraits are part of its obligation
        // (`trait Error: Display`), so record the same owner-qualified edges
        // the ordinary registration path records. All three trait_defs
        // spellings carry them, because an impl site keys off whichever
        // spelling `trait_defs_key_for_bound` resolves.
        if let Some(supers) = &tr.super_traits {
            let super_keys: Vec<String> = supers
                .iter()
                .map(|s| format!("std.builtins.{}", s.path)) // TRANSITION(P1): deleted by A1 commit 2
                .collect();
            for key in [
                tr.name.to_string(),
                format!("builtins.{}", tr.name),
                canonical.clone(),
            ] {
                self.trait_super.entry(key).or_insert(super_keys.clone());
            }
        }
        self.published_bare_trait_owners
            .entry((
                self.current_module.clone(),
                self.current_module_idx,
                tr.name.to_string(),
            ))
            .or_default()
            .insert(canonical);
        // Builtin traits are parsed outside the ordinary program collection
        // pass, so mint their exact declaration IDs here as well as their
        // TraitInfo. Dynamic dispatch (for example `dyn Index::at`) consumes
        // these IDs and must not reconstruct them from the bare trait
        // spelling.
        let saved_module = self.current_module.replace("std.builtins".to_string());
        let trait_scope = self.enter_primary_sig_scope(&[(tr.type_params.as_ref(), None)]);
        for trait_item in &tr.items {
            if let TraitItem::Method(method) = trait_item {
                self.register_trait_method_sig(tr.name.name.as_str(), method, span);
            }
        }
        self.exit_primary_sig_scope(trait_scope);
        self.current_module = saved_module;
        // Harvest #[lang_item("...")] from the stdlib-shipped trait
        // declaration so HIR f-string lowering can discover the canonical
        // Display::fmt name through `LangItemRegistry` even when the user's
        // program never declares the trait itself. Without this, every `f"…"`
        // lowering in user code would fail closed with "no lang-item
        // registered for key `display_fmt`".
        self.seed_trait_lang_items(tr, span);
    }

    /// Register the built-in `MonitorRef` surface so `monitor()` can return a
    /// Hew value type with `#[resource]` / `close()` behaviour in inline tests that
    /// do not have a stdlib search path.
    pub(super) fn register_builtin_monitor_ref_surface(&mut self) {
        let identity = "module:std.link_monitor";
        if self.registered_stdlib_hew_sources.contains(identity) {
            return;
        }
        self.registered_stdlib_hew_sources
            .insert(identity.to_string());
        let parsed = hew_parser::parse(MONITOR_REF_HEW);
        debug_assert!(
            parsed.errors.is_empty(),
            "std/link_monitor.hew failed to parse: {:?}",
            parsed.errors
        );
        if parsed.errors.is_empty() {
            // The generated projection carries LinkError's declaration so its
            // ABI surface is source-derived, but that declaration is owned by
            // std/builtins.hew and was already registered by
            // register_builtins_hew_impls. Never re-register it under the
            // std.link_monitor owner used by the rest of this projection.
            let items: Vec<_> = parsed
                .program
                .items
                .into_iter()
                .filter(
                    |(item, _)| !matches!(item, Item::TypeDecl(decl) if decl.name == Ident::new("LinkError")),
                )
                .collect();
            self.register_stdlib_hew_items(
                "link_monitor",
                "std.link_monitor",
                &items,
                StdlibBarePublication::Prelude,
            );
            self.record_canonical_lifecycle_prelude_authority("std.link_monitor");
            // The on-disk stdlib path derives this from the `#[resource]` marker
            // in `stdlib_loader` (resource types are pushed into `drop_types`),
            // which makes the trait registry treat `MonitorRef` as move-only so
            // its inherent `close(self)` actually consumes the receiver. The
            // inline surface bypasses `stdlib_loader`, so mirror that derivation
            // here. A `#[resource]` record whose fields are all `Copy` would
            // otherwise derive `Copy` structurally and the consume-on-close move
            // would silently no-op. The consume *detection* is supplied by the
            // `#[resource]` inherent-close path, so no `consume_receiver_methods`
            // entry is needed.
            self.registry.register_drop_type("MonitorRef".to_string());
        }
    }

    /// Register the built-in `std.failure` surface so lifecycle hooks can
    /// name its payload types in their signatures without
    /// `import std.failure;`.  Inline tests (no stdlib search path) rely on
    /// this; on-disk programs reach the same types via the module graph.
    ///
    /// The whole shipped source registers, not a two-name subset (rc1-F1
    /// stage D). Publishing `CrashNotification` / `CrashKind` as bindings while
    /// declaring only `CrashInfo` / `CrashAction` left the binding tables
    /// naming `std.failure.CrashNotification` and the declaration tables
    /// knowing no such type — one declaration with two answers, which is what
    /// made an `#[on(exit)]` payload fail the hook ABI compare and a
    /// whole-module alias fail its exported-type lookup. `std::link_monitor`
    /// registers its full projection for the same reason.
    pub(super) fn register_builtin_failure_surface(&mut self) {
        let identity = "module:std.failure";
        if self.registered_stdlib_hew_sources.contains(identity) {
            return;
        }
        self.registered_stdlib_hew_sources
            .insert(identity.to_string());
        let parsed = hew_parser::parse(FAILURE_HEW);
        debug_assert!(
            parsed.errors.is_empty(),
            "std/failure.hew failed to parse: {:?}",
            parsed.errors
        );
        if parsed.errors.is_empty() {
            let items: Vec<_> = parsed.program.items.into_iter().collect();
            self.register_stdlib_hew_items(
                "failure",
                "std.failure",
                &items,
                StdlibBarePublication::Prelude,
            );
            // `CrashInfo` / `CrashAction` are genuine prelude: a
            // `#[on(crash)]` signature names them with no import, always.
            for source_name in ["CrashInfo", "CrashAction"] {
                self.canonical_lifecycle_import_authority.insert((
                    None,
                    source_name.to_string(),
                    format!("std.failure.{source_name}"),
                ));
            }
            // The linked-actor payloads (`CrashNotification` / `CrashKind`)
            // require an explicit `import std::failure` whenever the module
            // graph can supply that source. With no stdlib search path there
            // is no import to write — this compiled-in projection IS the
            // source — so the bootstrap carries the same source-path-proven
            // authority the import would have granted.
            if !self.module_registry.has_search_paths() {
                self.record_canonical_lifecycle_prelude_authority("std.failure");
            }
        }
    }

    /// Publish the import-free spellings of std.builtins-owned error enums.
    /// Their declarations were already registered by
    /// `register_builtins_hew_impls`; this adds only lexical prelude bindings
    /// and must never mint a second synthetic source owner.
    pub(super) fn register_builtin_error_prelude_bindings(&mut self) {
        for name in ["LinkError", "LookupError", "NodeError", "ScopeFailure"]
            .into_iter()
            .chain(crate::actor_delivery::DECLARATIONS.iter().copied())
        {
            let canonical = format!("std.builtins.{name}");
            debug_assert!(
                self.type_defs.contains_key(&canonical),
                "builtins prelude binding requires its source declaration: {canonical}"
            );
            self.known_types.insert(name.to_string());
            self.record_published_bare_type(name, &canonical);
            self.unqualified_to_module.insert(
                (
                    self.current_module.clone(),
                    self.current_module_idx,
                    name.to_string(),
                ),
                "std.builtins".to_string(),
            );
        }
    }

    pub(in crate::check) fn register_builtin_fn(
        &mut self,
        name: &str,
        params: Vec<Ty>,
        return_type: Ty,
    ) {
        self.register_builtin_sig(
            name,
            FnSig {
                params,
                return_type,
                ..FnSig::default()
            },
        );
    }

    pub(in crate::check) fn register_builtin_fn_with_bounds(
        &mut self,
        name: &str,
        type_params: Vec<String>,
        type_param_bounds: HashMap<String, Vec<String>>,
        params: Vec<Ty>,
        return_type: Ty,
    ) {
        self.register_builtin_sig(
            name,
            FnSig {
                type_params,
                type_param_bounds,
                params,
                return_type,
                ..FnSig::default()
            },
        );
    }

    pub(super) fn register_builtin_sig(&mut self, name: &str, sig: FnSig) {
        if name.contains('.') {
            self.module_fn_exports.insert(name.to_string());
        }
        self.fn_sigs.insert(name.to_string(), sig);
        // Record executable runtime authority at builtin-registration time.
        // `fn_sigs` itself intentionally remains an open-set lookup index: it
        // can contain user and imported-source functions.  No call-site may
        // infer a runtime endpoint merely because a signature happens to have
        // a runtime-looking name.
        // Namespace separators are source syntax, not ABI spelling.  The
        // only compiler builtin with a namespace-qualified source name today
        // is `instant::now`; retain an explicit mapping rather than making
        // arbitrary source names look like runtime symbols.
        let runtime_symbol = match name {
            "instant::now" => "hew_instant_now".to_string(),
            _ => format!("hew_{name}"),
        };
        let target = crate::runtime_call::RuntimeCallFamily::from_c_symbol(&runtime_symbol)
            .map_or_else(
                || CallTarget::Builtin {
                    endpoint: name.to_string(),
                },
                CallTarget::Runtime,
            );
        // Effects belong to these compiler declarations, so a source shadow
        // retains its separately checked declaration identity and body effect.
        if matches!(name, "sleep" | "sleep_until") {
            self.effect_graph.builtin_suspensions.insert(target.clone());
        }
        self.builtin_call_targets.insert(name.to_string(), target);
    }

    /// Canonical receiver key used by the primitive-and-builtin trait impl
    /// table.  Returns `Some(canonical)` for any receiver kind whose user
    /// trait impls cannot be hung off `type_defs`:
    ///
    /// * Primitives — keyed by `Ty::canonical_lowering_name()`.  This collapses
    ///   the user-facing alias set (`isize` → `i64`) so registration and
    ///   dispatch agree on a single key.  `int` and `Int` are no longer
    ///   accepted; the resolver hard-errors at the type-position lookup.
    /// * Compiler-builtin generics `Vec`, `HashMap`, `HashSet`, `Generator`,
    ///   and the synthetic iterator cursors — keyed by their
    ///   catalog name; these lack a user-selectable nominal declaration entry
    ///   that executable impl dispatch can safely attach methods to.
    ///
    /// Returns `None` for receivers that already flow through `type_defs`
    /// (user structs, actors, opaque handle types).
    #[must_use]
    pub(crate) fn canonical_primitive_or_builtin_key(ty: &Ty) -> Option<String> {
        if let Some(canonical) = ty.canonical_lowering_name() {
            return Some(canonical.to_string());
        }
        if let Ty::Named {
            builtin: Some(builtin),
            ..
        } = ty
        {
            if let Some(identity) = match builtin {
                BuiltinType::VecIter => Some("std.builtins.VecIter"),
                BuiltinType::HashMapIter => Some("std.builtins.HashMapIter"),
                _ => None,
            } {
                return Some(identity.to_string());
            }
            // An actor is the type of its handle, so an actor handle's
            // canonical key is the actor's own nominal, never a builtin
            // presentation name.
            if builtin.is_collection()
                || matches!(
                    builtin,
                    BuiltinType::Generator | BuiltinType::ChildRef | BuiltinType::RemotePid
                )
            {
                return Some(builtin.canonical_name().to_string());
            }
        }
        None
    }

    /// Canonical receiver key at an impl declaration. Context-free raw-name
    /// lookup intentionally excludes synthetic cursors because a user may
    /// declare the same leaf. The provenance-aware resolver admits them only
    /// while registering their shipped stdlib declarations.
    pub(super) fn canonical_primitive_or_builtin_key_for_impl_name(
        &self,
        name: &str,
    ) -> Option<String> {
        Self::canonical_primitive_or_builtin_key_from_name(name).or_else(|| {
            self.resolved_builtin_type(name)
                .filter(|builtin| {
                    matches!(
                        builtin,
                        BuiltinType::VecIter
                            | BuiltinType::HashMapIter
                            | BuiltinType::ChildRef
                            | BuiltinType::RemotePid
                    )
                })
                .map(|builtin| match builtin {
                    BuiltinType::VecIter => "std.builtins.VecIter".to_string(),
                    BuiltinType::HashMapIter => "std.builtins.HashMapIter".to_string(),
                    BuiltinType::ChildRef | BuiltinType::RemotePid => {
                        builtin.canonical_name().to_string()
                    }
                    _ => unreachable!("filter admits only compiler carrier builtins"),
                })
        })
    }

    /// Same as [`Self::canonical_primitive_or_builtin_key`] but accepts the
    /// raw type-name string seen at impl-block registration (e.g. `"int"`,
    /// `"string"`, `"Vec"`).  Returns `None` for names that aren't primitive
    /// aliases or compiler-builtin generics.
    #[must_use]
    pub(in crate::check) fn canonical_primitive_or_builtin_key_from_name(
        name: &str,
    ) -> Option<String> {
        if let Some(prim) = Ty::from_name(name) {
            return Self::canonical_primitive_or_builtin_key(&prim);
        }
        if let Some(builtin) = crate::lookup_builtin_type(name) {
            if builtin.is_collection() || matches!(builtin, BuiltinType::Generator) {
                return Some(builtin.canonical_name().to_string());
            }
        }
        None
    }

    /// Snapshot the compiler-assumed part of the implicit prelude before source
    /// registration begins.
    ///
    /// The prelude manifest is deliberately broader than this protected set:
    /// ordinary builtins remain normal lexical bindings and may be shadowed by
    /// user declarations. Only declaration-level lang items plus the core
    /// enum/desugaring heads below are names the compiler cannot let source
    /// replace without changing language semantics.
    pub(in crate::check) fn capture_protected_prelude_bindings(&mut self) {
        self.protected_prelude_bindings.clear();
        self.protected_prelude_declaration_collisions.clear();

        let authority = crate::stdlib_authority::authority();
        let prelude_exports = authority.prelude_exports();
        let mut protected_names: HashSet<String> = authority
            .lang_items()
            .values()
            .filter(|binding| {
                matches!(
                    binding.kind,
                    crate::stdlib_authority::AuthorityDeclarationKind::Type
                        | crate::stdlib_authority::AuthorityDeclarationKind::Trait
                )
            })
            .map(|binding| binding.declaration.clone())
            .collect();
        // Option/Result construction and propagation, plus for-loop
        // conversion, are compiler desugarings whose declarations predate the
        // corresponding lang-item annotations.
        protected_names.extend(
            ["Option", "Result", "IntoIterator"]
                .into_iter()
                .map(str::to_string),
        );

        for export in prelude_exports {
            let source_name = &export.name;
            let binding = export.alias.as_ref().unwrap_or(source_name);
            if protected_names.contains(source_name) {
                self.protected_prelude_bindings
                    .insert(binding.clone(), export.module.clone());
            }
        }
    }

    pub(in crate::check) fn reject_protected_prelude_declaration(
        &mut self,
        name: &str,
        span: &Span,
    ) -> bool {
        let owner = self.current_module.clone();
        self.reject_protected_prelude_declaration_for_owner(owner.as_deref(), name, span)
    }

    /// Reject a user declaration that would replace an always-in-scope prelude
    /// binding. Authority follows the declaration's owner, not whichever
    /// importer happens to be active while that declaration is published.
    pub(super) fn reject_protected_prelude_declaration_for_owner(
        &mut self,
        declaration_owner: Option<&str>,
        name: &str,
        span: &Span,
    ) -> bool {
        let compiling_canonical_stdlib = self.checking_embedded_builtins
            || self.in_stdlib_registration
            || self
                .canonical_std_module_sources
                .contains(declaration_owner.unwrap_or_default())
            || (declaration_owner.is_none() && !self.canonical_std_root_sources.is_empty());
        if compiling_canonical_stdlib || !self.protected_prelude_bindings.contains_key(name) {
            return false;
        }
        let declaration_key = (declaration_owner.map(str::to_string), name.to_string());
        if self
            .protected_prelude_declaration_collisions
            .insert(declaration_key)
        {
            let mut error = TypeError::new(
                TypeErrorKind::PreludeDeclCollision,
                span.clone(),
                format!(
                    "declaration `{name}` collides with the protected prelude binding `{name}`"
                ),
            );
            error.source_module = declaration_owner.map(str::to_string);
            self.errors.push(error);
        }
        true
    }

    /// Diagnose protected-prelude declarations in every non-root module once,
    /// including private declarations that are never published by an import.
    pub(in crate::check) fn reject_non_root_protected_prelude_declarations(
        &mut self,
        program: &Program,
    ) {
        let Some(module_graph) = &program.module_graph else {
            return;
        };
        for module_id in &module_graph.topo_order {
            if *module_id == module_graph.root {
                continue;
            }
            let Some(module) = module_graph.modules.get(module_id) else {
                continue;
            };
            let owner = module_id.dotted();
            for (item, span) in &module.items {
                let name = match item {
                    Item::Const(item) => Some(item.name.name.as_str()),
                    Item::TypeDecl(item) => Some(item.name.name.as_str()),
                    Item::TypeAlias(item) => Some(item.name.name.as_str()),
                    Item::Trait(item) => Some(item.name.name.as_str()),
                    Item::Function(item) => Some(item.name.name.as_str()),
                    Item::Actor(item) => Some(item.name.name.as_str()),
                    Item::Supervisor(item) => Some(item.name.name.as_str()),
                    Item::Machine(item) => Some(item.name.name.as_str()),
                    Item::Record(item) => Some(item.name.name.as_str()),
                    Item::Import(_) | Item::Impl(_) | Item::ExternBlock(_) => None,
                };
                if let Some(name) = name {
                    self.reject_protected_prelude_declaration_for_owner(Some(&owner), name, span);
                }
            }
        }
    }
}
