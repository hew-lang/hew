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
use hew_parser::ast::WireMetadata;

impl Checker {
    pub(super) fn mark_import_module_used_for_owner(
        &self,
        owner: Option<&str>,
        imported_module: &str,
    ) {
        for ((scope, file, binding), source) in &self.module_import_bindings {
            if scope.as_deref() == owner
                && *file == self.current_module_idx
                && (binding == imported_module || source == imported_module)
            {
                self.used_modules.borrow_mut().insert(ImportKey::in_file(
                    owner.map(str::to_string),
                    *file,
                    binding.clone(),
                ));
            }
        }
    }

    pub(super) fn mark_loaded_trait_owner_import_used(
        &self,
        module: Option<&str>,
        trait_name: &str,
    ) {
        let candidate_owners = [
            module.map(str::to_string),
            self.current_module.clone(),
            None::<String>,
        ];
        let mut used = self.used_modules.borrow_mut();
        for key in self.import_spans.keys() {
            if !candidate_owners
                .iter()
                .any(|owner| owner.as_ref() == key.owner_module.as_ref())
            {
                continue;
            }
            let qualified = format!("{}.{}", key.short_name, trait_name);
            if self.trait_defs.contains_key(&qualified) {
                used.insert(key.clone());
            }
        }
    }

    pub(super) fn mark_imported_trait_used(&self, module: Option<&str>, trait_name: &str) {
        if let Some((imported_module, _)) = trait_name.split_once('.') {
            if self.modules.contains(imported_module) {
                self.mark_import_module_used_for_owner(module, imported_module);
                if self.current_module.as_deref() != module {
                    self.mark_import_module_used_for_owner(
                        self.current_module.as_deref(),
                        imported_module,
                    );
                }
            }
            return;
        }

        if let Some(source_key) = self.trait_import_bindings.get(&(
            module.unwrap_or_default().to_string(),
            trait_name.to_string(),
        )) {
            if let Some((imported_module, _)) = source_key.rsplit_once('.') {
                if Some(imported_module) == module {
                    return;
                }
                self.mark_import_module_used_for_owner(module, imported_module);
                if self.current_module.as_deref() != module {
                    self.mark_import_module_used_for_owner(
                        self.current_module.as_deref(),
                        imported_module,
                    );
                }
            }
        } else if let Some(imported_module) = self.unqualified_to_module.get(&(
            module.map(str::to_string),
            self.current_module_idx,
            trait_name.to_string(),
        )) {
            self.mark_import_module_used_for_owner(module, imported_module.as_str());
            if self.current_module.as_deref() != module {
                self.mark_import_module_used_for_owner(
                    self.current_module.as_deref(),
                    imported_module.as_str(),
                );
            }
        } else {
            self.mark_loaded_trait_owner_import_used(module, trait_name);
        }
    }

    pub(super) fn mark_imported_trait_used_for_module_aliases(
        &self,
        module_short: &str,
        trait_name: &str,
    ) {
        self.mark_imported_trait_used(Some(module_short), trait_name);

        let owner_aliases: Vec<String> = self
            .import_spans
            .keys()
            .filter_map(|key| key.owner_module.as_deref())
            .filter(|owner| owner.rsplit("::").next() == Some(module_short))
            .map(str::to_string)
            .collect();
        for owner in owner_aliases {
            self.mark_imported_trait_used(Some(&owner), trait_name);
        }
    }

    pub(super) fn refresh_handle_bearing_structs(&mut self) {
        // Tracked for testing: callers can assert this stays O(1) after the
        // deferred-refresh fix (see `ensure_handle_bearing_fresh`).
        self.refresh_call_count += 1;

        let struct_names: Vec<String> = self
            .type_defs
            .iter()
            .filter_map(|(name, type_def)| {
                (type_def.kind == TypeDefKind::Struct).then_some(name.clone())
            })
            .collect();

        self.handle_bearing_structs = struct_names
            .into_iter()
            .filter(|name| self.type_name_contains_owned_handle(name, &mut HashSet::new()))
            .collect();
    }

    /// Refresh the handle-bearing set once, iff it has been dirtied since the
    /// last refresh. Converts O(N²) repeated full scans during batch
    /// registration into a single fixpoint pass before the first lookup.
    pub(in crate::check) fn ensure_handle_bearing_fresh(&mut self) {
        if self.handle_bearing_dirty {
            self.handle_bearing_dirty = false;
            self.refresh_handle_bearing_structs();
        }
    }

    pub(super) fn type_name_contains_owned_handle(
        &self,
        type_name: &str,
        visiting: &mut HashSet<String>,
    ) -> bool {
        let Some(lookup_name) = self.registered_type_def_name(type_name) else {
            return false;
        };
        if !visiting.insert(lookup_name.clone()) {
            return false;
        }
        let contains_owned_handle = self.type_defs.get(&lookup_name).is_some_and(|type_def| {
            type_def.kind == TypeDefKind::Struct
                && type_def
                    .fields
                    .values()
                    .any(|field_ty| self.ty_contains_owned_handle(field_ty, visiting))
        });
        visiting.remove(&lookup_name);
        contains_owned_handle
    }

    pub(super) fn ty_contains_owned_handle(&self, ty: &Ty, visiting: &mut HashSet<String>) -> bool {
        match ty {
            Ty::Tuple(items) => items
                .iter()
                .any(|item_ty| self.ty_contains_owned_handle(item_ty, visiting)),
            Ty::Array(element_ty, _) | Ty::Slice(element_ty) => {
                self.ty_contains_owned_handle(element_ty, visiting)
            }
            Ty::Named { name, args, .. } => {
                self.canonical_owned_handle_type_name(name).is_some()
                    || args
                        .iter()
                        .any(|arg_ty| self.ty_contains_owned_handle(arg_ty, visiting))
                    || self.type_name_contains_owned_handle(name, visiting)
            }
            Ty::I8
            | Ty::I16
            | Ty::I32
            | Ty::I64
            | Ty::U8
            | Ty::U16
            | Ty::U32
            | Ty::U64
            | Ty::Isize
            | Ty::Usize
            | Ty::F32
            | Ty::F64
            | Ty::IntLiteral
            | Ty::FloatLiteral
            | Ty::Bool
            | Ty::Char
            | Ty::String
            | Ty::Bytes
            | Ty::CancellationToken
            | Ty::Duration
            | Ty::Unit
            | Ty::Never
            | Ty::Var(_)
            | Ty::Function { .. }
            | Ty::Closure { .. }
            | Ty::Pointer { .. }
            // `&T` borrow is non-owning: a borrow never holds an owned handle
            // (the owner is borrowed from, elsewhere). Mirrors the Pointer arm.
            | Ty::Borrow { .. }
            | Ty::TraitObject { .. }
            | Ty::Error
            // Task<T> is compiler-internal; it does not appear in user-declared
            // struct field types (there is no surface annotation for Task<T>),
            // so this arm is structurally unreachable today. Explicit rather
            // than wildcard so the sweep stays honest.
            | Ty::Task(_)
            // Ty::AssocType is a projection carrier present only in generic
            // signatures during checking; field-type validation walks
            // user-declared struct/record/enum fields, which cannot themselves
            // be associated-type projections (no `field: T::Item` surface).
            // If a future surface admits projections in field types, this arm
            // must descend into `base`.
            | Ty::AssocType { .. } => false,
        }
    }

    pub(in crate::check) fn canonical_owned_handle_type_name(
        &self,
        type_name: &str,
    ) -> Option<String> {
        self.module_registry
            .canonical_owned_type_identity(type_name)
    }

    pub(in crate::check) fn registered_type_def_name(&self, name: &str) -> Option<String> {
        if self.type_defs.contains_key(name) {
            return Some(name.to_string());
        }
        self.strip_module_prefix(name)
            .filter(|unqualified| self.type_defs.contains_key(*unqualified))
            .map(str::to_string)
    }

    pub(in crate::check) fn structural_member_types_for_type(type_def: &TypeDef) -> Vec<Ty> {
        let mut member_types: Vec<Ty> = type_def.fields.values().cloned().collect();
        for variant in type_def.variants.values() {
            match variant {
                VariantDef::Unit => {}
                VariantDef::Tuple(tys) => member_types.extend(tys.iter().cloned()),
                VariantDef::Struct(fields) => {
                    member_types.extend(fields.iter().map(|(_, ty)| ty.clone()));
                }
            }
        }
        member_types
    }

    /// Expand type aliases in a member-type list before it is handed to the
    /// `TraitRegistry` for marker derivation (Send/Frozen/Sync/Copy/Eq/Hash/
    /// Encode/Decode/…). `TraitRegistry` has no alias table of its own — it
    /// only knows nominal struct/record/enum member sets — so a field typed
    /// `Ty::Named { Label }` where `Label` is a top-level alias reads as an
    /// unknown nominal and derives conservatively false for every marker
    /// (`cannot send AppConfig to actor: type is not Send` even when `Label`
    /// is `string`). `type_def.fields` itself stays unexpanded: alias
    /// identity is still needed at annotation/impl-lookup sites (A316); only
    /// this admission-facing copy is normalized.
    pub(super) fn expand_for_marker_registration(&self, types: &[Ty]) -> Vec<Ty> {
        types.iter().map(|ty| self.normalize_for_use(ty)).collect()
    }

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

        // Assertions (test support)
        self.register_builtin_fn("assert", vec![Ty::Bool], Ty::Unit);
        self.register_builtin_fn_with_bounds(
            "assert_eq",
            vec!["T".to_string()],
            HashMap::from([(
                "T".to_string(),
                vec!["Eq".to_string(), "Display".to_string()],
            )]),
            vec![
                Ty::Named {
                    builtin: None,
                    name: "T".to_string(),
                    args: vec![],
                },
                Ty::Named {
                    builtin: None,
                    name: "T".to_string(),
                    args: vec![],
                },
            ],
            Ty::Unit,
        );
        self.register_builtin_fn_with_bounds(
            "assert_ne",
            vec!["T".to_string()],
            HashMap::from([(
                "T".to_string(),
                vec!["Eq".to_string(), "Display".to_string()],
            )]),
            vec![
                Ty::Named {
                    builtin: None,
                    name: "T".to_string(),
                    args: vec![],
                },
                Ty::Named {
                    builtin: None,
                    name: "T".to_string(),
                    args: vec![],
                },
            ],
            Ty::Unit,
        );

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
        let builtins_module = self.identity.mint_module("std.builtins", &[]);
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
                        self.register_canonical_type_def("std.builtins", &td.name, &source_def);
                    }
                    // Compiler-carrier builtins (`RemotePid`, `NodeId`, ...)
                    // retain the catalog's canonical identity; this source file
                    // supplies their declarative surface but does not turn them
                    // into `std.builtins.*` user nominals. Builtin error enums
                    // that need an explicit bare prelude binding are published
                    // separately after every declaration has its true owner.
                    if crate::lookup_builtin_type(&td.name).is_none() {
                        self.record_published_bare_type(
                            &td.name,
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
                        function.name.clone(),
                        &canonical,
                        StdlibBarePublication::Prelude,
                    );
                    self.builtin_call_targets.insert(
                        function.name.clone(),
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
            .entry(tr.name.clone())
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
                .map(|s| format!("std.builtins.{}", s.name))
                .collect();
            for key in [
                tr.name.clone(),
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
                tr.name.clone(),
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
                self.register_trait_method_sig(&tr.name, method, span);
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

    pub(super) fn register_compiled_stdlib_receiver_impls(
        &mut self,
        module_short: &str,
        source: &str,
        receiver_names: &[&str],
    ) {
        let parsed = hew_parser::parse(source);
        debug_assert!(
            parsed.errors.is_empty(),
            "std/{module_short}.hew failed to parse: {:?}",
            parsed.errors
        );
        if !parsed.errors.is_empty() {
            return;
        }
        let impl_items: Vec<Spanned<Item>> = parsed
            .program
            .items
            .into_iter()
            .filter(|(item, _)| {
                let Item::Impl(id) = item else {
                    return false;
                };
                let TypeExpr::Named { name, .. } = &id.target_type.0 else {
                    return false;
                };
                receiver_names.iter().any(|receiver| name == receiver)
            })
            .collect();
        if !impl_items.is_empty() {
            let module_full_path = format!("std.{module_short}");
            self.register_stdlib_hew_items(
                module_short,
                &module_full_path,
                &impl_items,
                StdlibBarePublication::Prelude,
            );
        }
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
                    |(item, _)| !matches!(item, Item::TypeDecl(decl) if decl.name == "LinkError"),
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

    /// Register a builtin collection constructor: a nullary generic function
    /// returning the canonical collection over its own type parameters, with
    /// the constructor's runtime family as its executable call target.
    pub(super) fn register_collection_constructor(
        &mut self,
        name: &str,
        builtin: BuiltinType,
        type_params: &[&str],
        family: crate::runtime_call::RuntimeCallFamily,
    ) {
        self.register_builtin_fn_with_bounds(
            name,
            type_params
                .iter()
                .map(|param| (*param).to_string())
                .collect(),
            HashMap::new(),
            vec![],
            Ty::Named {
                builtin: Some(builtin),
                name: builtin.canonical_name().to_string(),
                args: type_params
                    .iter()
                    .map(|param| Ty::named(*param, vec![]))
                    .collect(),
            },
        );
        self.builtin_call_targets
            .insert(name.to_string(), CallTarget::Runtime(family));
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

    pub(super) fn resolve_registered_annotation_ty(
        &mut self,
        type_expr: &Spanned<TypeExpr>,
        hole_vars: &mut Vec<TypeVar>,
    ) -> Ty {
        self.resolve_registered_annotation_ty_with_context(
            type_expr,
            hole_vars,
            TypeResolutionContext::Ordinary,
        )
    }

    pub(super) fn resolve_registered_annotation_ty_with_context(
        &mut self,
        type_expr: &Spanned<TypeExpr>,
        hole_vars: &mut Vec<TypeVar>,
        context: TypeResolutionContext,
    ) -> Ty {
        let ty = self.resolve_type_expr_tracking_holes_with_context(type_expr, hole_vars, context);
        self.validate_concrete_collection_types(&ty, &type_expr.1);
        ty
    }

    pub(super) fn resolve_registered_annotation_ty_no_holes(
        &mut self,
        type_expr: &Spanned<TypeExpr>,
    ) -> Ty {
        let mut hole_vars = Vec::new();
        self.resolve_registered_annotation_ty(type_expr, &mut hole_vars)
    }

    /// Populate `declared_type_param_names` with every type-parameter name
    /// declared anywhere in the program and its modules — on type / record /
    /// trait / impl / machine / actor declarations and on every generic method
    /// (impl method, trait method, actor receive-fn) or free function — and
    /// `declared_nominal_type_names` with every declared NOMINAL type name
    /// (type / type-alias / record / trait / actor / supervisor / machine, plus
    /// the synthesised `<Machine>Event` companion).
    ///
    /// The undefined-named-type guard consults both sets. A name declared as a
    /// type parameter somewhere is intentionally left opaque (`Ty::named`) by
    /// the resolver and re-resolved at several secondary sites (signature
    /// rebuilds, receiver probes, trait-conformance checks) WITHOUT its scope
    /// re-pushed, so it must never be reported as undefined. A nominal type
    /// declared in an imported `module_graph` module is likewise resolvable even
    /// while that module's signatures are registered in a pass where the global
    /// `trait_defs` / `known_types` still hold only the root module's
    /// declarations. A genuinely undefined type (`Bogus`) is in neither set, so
    /// it is still caught.
    pub(in crate::check) fn collect_declared_type_param_names(&mut self, program: &Program) {
        for (item, _) in &program.items {
            self.collect_item_type_param_names(item);
            self.collect_item_nominal_type_name(item);
        }
        if let Some(mg) = &program.module_graph {
            for module in mg.modules.values() {
                for (item, _) in &module.items {
                    self.collect_item_type_param_names(item);
                    self.collect_item_nominal_type_name(item);
                }
            }
        }
        // Harvest trait-level type parameters from every registered trait def.
        // Built-in and stdlib traits (e.g. `Index<Idx>` from std/builtins.hew)
        // are registered into `trait_defs` by `register_builtins` rather than
        // appearing in the walked program AST; their parameter names surface in
        // user code when a `dyn Trait<...>` annotation pulls the trait's method
        // signatures through resolution without the trait scope re-pushed.
        let trait_param_names: Vec<String> = self
            .trait_defs
            .values()
            .flat_map(|trait_def| trait_def.type_params.iter().cloned())
            .collect();
        self.declared_type_param_names.extend(trait_param_names);
    }

    pub(super) fn collect_item_type_param_names(&mut self, item: &Item) {
        match item {
            Item::Supervisor(sd) => self.insert_type_param_names(&sd.type_params),
            Item::Function(fd) => self.insert_opt_type_param_names(fd.type_params.as_ref()),
            Item::TypeDecl(td) => self.insert_opt_type_param_names(td.type_params.as_ref()),
            Item::Record(rd) => self.insert_opt_type_param_names(rd.type_params.as_ref()),
            Item::Trait(tr) => {
                self.insert_opt_type_param_names(tr.type_params.as_ref());
                for trait_item in &tr.items {
                    if let TraitItem::Method(method) = trait_item {
                        self.insert_opt_type_param_names(method.type_params.as_ref());
                    }
                }
            }
            Item::Impl(id) => {
                self.insert_opt_type_param_names(id.type_params.as_ref());
                for method in &id.methods {
                    self.insert_opt_type_param_names(method.type_params.as_ref());
                }
            }
            Item::Actor(ad) => {
                self.insert_type_param_names(&ad.type_params);
                for receive_fn in &ad.receive_fns {
                    self.insert_opt_type_param_names(receive_fn.type_params.as_ref());
                }
                for method in &ad.methods {
                    self.insert_opt_type_param_names(method.type_params.as_ref());
                }
            }
            _ => {}
        }
    }

    pub(super) fn insert_opt_type_param_names(&mut self, tps: Option<&Vec<TypeParam>>) {
        if let Some(tps) = tps {
            self.insert_type_param_names(tps);
        }
    }

    /// Record the nominal type name a top-level item declares (if any) into
    /// `declared_nominal_type_names`. Mirrors the type-name registration in
    /// `collect_types`, but is a program-wide harvest the undefined-named-type
    /// guard consults so an imported module's own types/traits resolve even in
    /// the pass that registers that module's signatures.
    pub(super) fn collect_item_nominal_type_name(&mut self, item: &Item) {
        match item {
            Item::TypeDecl(td) => {
                self.declared_nominal_type_names.insert(td.name.clone());
            }
            Item::TypeAlias(ta) => {
                self.declared_nominal_type_names.insert(ta.name.clone());
            }
            Item::Trait(tr) => {
                self.declared_nominal_type_names.insert(tr.name.clone());
            }
            Item::Actor(ad) => {
                self.declared_nominal_type_names.insert(ad.name.clone());
            }
            Item::Supervisor(sd) => {
                self.declared_nominal_type_names.insert(sd.name.clone());
            }
            Item::Record(rd) => {
                self.declared_nominal_type_names.insert(rd.name.clone());
            }
            _ => {}
        }
    }

    pub(super) fn insert_type_param_names(&mut self, tps: &[TypeParam]) {
        for tp in tps {
            self.declared_type_param_names.insert(tp.name.clone());
        }
    }

    /// Establish compiler-floor authority from the module graph's resolved
    /// source, before that module publishes signatures or imports. A lexical
    /// `std.*` path is user-controlled; only the exact shipped path may enable
    /// compiler intrinsic metadata.
    pub(super) fn record_canonical_std_module_source(
        &mut self,
        module_name: &str,
        source_paths: &[std::path::PathBuf],
    ) {
        if source_paths.iter().any(|source| {
            crate::module_registry::is_canonical_stdlib_module_source(source, module_name)
        }) {
            self.canonical_std_module_sources
                .insert(module_name.to_string());
        }
    }

    /// Seed lifecycle import bindings from module-graph edges before type
    /// declaration members are pre-registered.
    ///
    /// `collect_types` intentionally runs before the ordinary import pass, but
    /// enum/record members can themselves name an imported lifecycle type
    /// (`std.link_monitor`'s `Crashed(CrashKind)`).  A source declaration alone
    /// is not authority: require a matching resolved graph edge, then publish
    /// exactly the same lexical/canonical bindings the later import pass would.
    #[expect(
        clippy::too_many_lines,
        reason = "lifecycle import seeding mirrors all declaration kinds atomically"
    )]
    pub(super) fn seed_resolved_lifecycle_import_bindings(
        &mut self,
        module: &hew_parser::module::Module,
        importer: Option<&str>,
        module_graph: &hew_parser::module::ModuleGraph,
    ) {
        let saved_importer_file_idx = self.current_module_idx;
        let span_indices = module_graph.file_span_indices();
        for (item_idx, (item, _)) in module.items.iter().enumerate() {
            // These bindings are written before the ordinary import pass, but
            // they still belong to the importing SOURCE FILE. A directory
            // module can assemble imports from several peers, so module-level
            // or ambient indexing would publish a cross-file binding.
            self.current_module_idx = if module.id == module_graph.root {
                0
            } else {
                span_indices
                    .item_index(&module.id, item_idx)
                    .unwrap_or_default()
            };
            let Item::Import(decl) = item else {
                continue;
            };
            let Some(resolved) = module
                .imports
                .iter()
                .find(|edge| edge.target.path == decl.path)
            else {
                continue;
            };
            let canonical_module = resolved.target.path.join(".");
            let owner = match canonical_module.as_str() {
                "std.failure" => "std.failure",
                "std.link_monitor" => "std.link_monitor",
                _ => continue,
            };
            let target_is_canonical =
                module_graph
                    .modules
                    .get(&resolved.target)
                    .is_some_and(|target| {
                        target.source_paths.iter().any(|source| {
                            crate::module_registry::is_canonical_stdlib_module_source(
                                source,
                                &canonical_module,
                            )
                        })
                    });
            let module_binding = decl
                .module_alias
                .clone()
                .or_else(|| decl.path.last().cloned())
                .unwrap_or_else(|| owner.rsplit('.').next().unwrap_or(owner).to_string());
            // Every module-path import supplies an exact lexical owner for
            // qualified sibling references. A selective import such as
            // `import hew::closableerr::{ Closable as C }` still makes
            // `closableerr.CloseError` an explicit source spelling; keeping
            // that owner fact only for whole-module imports would leave the
            // type resolver to compare a short surface name against a full
            // declaration identity.
            self.module_import_bindings.insert(
                (
                    importer.map(str::to_owned),
                    self.current_module_idx,
                    module_binding.clone(),
                ),
                canonical_module.clone(),
            );

            let lifecycle_names: &[&str] = match owner {
                "std.failure" => &["CrashNotification", "CrashKind"],
                "std.link_monitor" => &[
                    "MonitorId",
                    "DownTarget",
                    "DownReason",
                    "DownNotification",
                    "MonitorError",
                    "MonitorRef",
                ],
                _ => unreachable!("matched canonical lifecycle owner"),
            };
            for source_name in lifecycle_names {
                let source_identity = format!("{owner}.{source_name}");
                if target_is_canonical {
                    self.canonical_lifecycle_import_authority.insert((
                        importer.map(str::to_owned),
                        if decl.spec.is_none() {
                            module_binding.clone()
                        } else {
                            let Some(binding) =
                                StdlibBarePublication::Import(&decl.spec).bare_binding(source_name)
                            else {
                                continue;
                            };
                            binding
                        },
                        source_identity.clone(),
                    ));
                    if decl.spec.is_none() {
                        // HIR does not re-resolve module imports. Publish the
                        // checker-proven whole-module spelling so a hook
                        // annotation such as `f.CrashNotification` retains the
                        // canonical lifecycle identity across TypeCheckOutput.
                        let qualified_surface = format!("{module_binding}.{source_name}");
                        if qualified_surface != source_identity {
                            self.import_type_name_aliases.insert(
                                (
                                    importer.map(str::to_owned),
                                    self.current_module_idx,
                                    qualified_surface,
                                ),
                                source_identity.clone(),
                            );
                        }
                    }
                }
                let Some(binding) =
                    StdlibBarePublication::Import(&decl.spec).bare_binding(source_name)
                else {
                    continue;
                };
                self.known_types.insert(binding.clone());
                self.record_published_bare_type(&binding, &source_identity);
                self.import_type_name_aliases.insert(
                    (
                        importer.map(str::to_owned),
                        self.current_module_idx,
                        binding.clone(),
                    ),
                    source_identity,
                );
                self.unqualified_to_module.insert(
                    (
                        importer.map(str::to_owned),
                        self.current_module_idx,
                        binding,
                    ),
                    canonical_module.clone(),
                );
            }
        }
        self.current_module_idx = saved_importer_file_idx;
    }

    /// Record direct lexical authority for lifecycle types imported from an
    /// exact shipped stdlib source.  A user module can be named `std.failure`,
    /// so module spelling and ordinary visibility are intentionally not proof.
    pub(super) fn record_canonical_lifecycle_import_authority(
        &mut self,
        decl: &ImportDecl,
        importer: Option<&str>,
    ) {
        let module_name = decl.path.join(".");
        let owner = match module_name.as_str() {
            "std.failure" => "std.failure",
            "std.link_monitor" => "std.link_monitor",
            _ => return,
        };
        if !decl.resolved_source_paths.iter().any(|source| {
            crate::module_registry::is_canonical_stdlib_module_source(source, &module_name)
        }) {
            return;
        }
        let module_binding = decl
            .module_alias
            .clone()
            .or_else(|| decl.path.last().cloned())
            .unwrap_or_else(|| owner.rsplit('.').next().unwrap_or(owner).to_string());
        let lifecycle_names: &[&str] = match owner {
            "std.failure" => &["CrashInfo", "CrashAction", "CrashNotification", "CrashKind"],
            "std.link_monitor" => &[
                "MonitorId",
                "DownTarget",
                "DownReason",
                "DownNotification",
                "MonitorError",
                "MonitorRef",
            ],
            _ => unreachable!("matched canonical lifecycle owner"),
        };
        for source_name in lifecycle_names {
            let Some(binding) = StdlibBarePublication::Import(&decl.spec).bare_binding(source_name)
            else {
                if decl.spec.is_none() {
                    let source_identity = format!("{owner}.{source_name}");
                    self.canonical_lifecycle_import_authority.insert((
                        importer.map(str::to_owned),
                        module_binding.clone(),
                        source_identity.clone(),
                    ));
                    let qualified_surface = format!("{module_binding}.{source_name}");
                    if qualified_surface != source_identity {
                        self.import_type_name_aliases.insert(
                            (
                                importer.map(str::to_owned),
                                self.current_module_idx,
                                qualified_surface,
                            ),
                            source_identity,
                        );
                    }
                }
                continue;
            };
            self.canonical_lifecycle_import_authority.insert((
                importer.map(str::to_owned),
                binding,
                format!("{owner}.{source_name}"),
            ));
        }
    }

    /// The isolated-checker prelude is compiled from the shipped lifecycle
    /// sources. Preserve its intentionally import-free bare spellings, but
    /// make that trust explicit with the same canonical-source proof used for
    /// ordinary imports.
    pub(super) fn record_canonical_lifecycle_prelude_authority(&mut self, module_name: &str) {
        let owner = match module_name {
            "std.failure" => "std.failure",
            "std.link_monitor" => "std.link_monitor",
            _ => return,
        };
        let source = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("hew-types crate has a workspace parent")
            .join("std")
            .join(format!("{}.hew", owner.rsplit('.').next().unwrap_or(owner)));
        if !crate::module_registry::is_canonical_stdlib_module_source(&source, module_name) {
            return;
        }
        let lifecycle_names: &[&str] = match owner {
            "std.failure" => &["CrashInfo", "CrashAction", "CrashNotification", "CrashKind"],
            "std.link_monitor" => &[
                "MonitorId",
                "DownTarget",
                "DownReason",
                "DownNotification",
                "MonitorError",
                "MonitorRef",
            ],
            _ => unreachable!("matched canonical lifecycle owner"),
        };
        for source_name in lifecycle_names {
            self.canonical_lifecycle_import_authority.insert((
                None,
                (*source_name).to_string(),
                format!("{owner}.{source_name}"),
            ));
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "type registration handles all root item variants in one place"
    )]
    /// Pass 1: Collect type definitions
    pub(in crate::check) fn collect_types(&mut self, program: &Program) {
        // Pre-register TypeDecls from non-root module_graph modules into
        // `type_defs` so non-root module body checking can access struct
        // fields and enum variants of types defined within those modules.
        //
        // Uses `pre_register_type_decl` which populates `type_defs` with
        // correct field/variant data but skips `type_def_spans` (so the
        // import path's `register_type_namespace_name` succeeds) and skips
        // trait-registry / wire-method side effects (those are handled by
        // the import path's full `register_type_decl` for pub types, and
        // are not needed for internal non-pub types).
        if let Some(ref mg) = program.module_graph {
            let span_indices = mg.file_span_indices();
            for mod_id in &mg.topo_order {
                if *mod_id == mg.root {
                    continue;
                }
                if let Some(module) = mg.modules.get(mod_id) {
                    let module_name = mod_id.path.join(".");
                    self.current_module = Some(module_name.clone());
                    self.seed_resolved_lifecycle_import_bindings(module, Some(&module_name), mg);
                    // Temporarily scope local_type_defs so that resolve_type_expr
                    // inside field type resolution does not inject fresh type vars
                    // on handle types from this module.
                    let saved_local_type_defs = self.local_type_defs.clone();
                    let saved_source_type_defs = self.source_type_defs.clone();
                    for (item, _) in &module.items {
                        match item {
                            Item::TypeDecl(td) => {
                                self.local_type_defs.insert(td.name.clone());
                                self.source_type_defs.insert(td.name.clone());
                            }
                            Item::Machine(md) => {
                                // Pre-seed the machine name so that resolve_type_expr
                                // inside state/event field resolution sees the machine
                                // as locally-non-generic instead of injecting a fresh var.
                                // Also seed the synthesised `<Name>Event` companion so
                                // imported machines surface their event union as a
                                // locally-defined type for the non-root module body.
                                self.local_type_defs.insert(md.name.clone());
                                self.source_type_defs.insert(md.name.clone());
                                let event_type_name = format!("{}Event", md.name);
                                self.local_type_defs.insert(event_type_name.clone());
                                self.source_type_defs.insert(event_type_name);
                            }
                            _ => {}
                        }
                    }
                    let err_before = self.errors.len();
                    let warn_before = self.warnings.len();
                    // Per-file lexical type authority (rc1-F1 stage C):
                    // record which FILE declares each type name, so
                    // extern-signature nominal identity can resolve a bare
                    // name to its declaring file's minted identity.
                    let item_sources = self.module_item_sources.get(&module_name).cloned();
                    for (item_idx, (item, _)) in module.items.iter().enumerate() {
                        let declared = match item {
                            Item::TypeDecl(td) => Some(td.name.clone()),
                            _ => None,
                        };
                        if let (Some(name), Some(source)) = (
                            declared,
                            item_sources
                                .as_ref()
                                .and_then(|sources| sources.get(item_idx)),
                        ) {
                            self.file_type_decls
                                .entry(source.clone())
                                .or_default()
                                .insert(name);
                        }
                    }
                    for (item_idx, (item, item_span)) in module.items.iter().enumerate() {
                        self.current_module_idx = span_indices
                            .item_index(mod_id, item_idx)
                            .unwrap_or_default();
                        match item {
                            Item::TypeDecl(td) => {
                                self.pre_register_type_decl(td);
                            }
                            Item::TypeAlias(decl) => {
                                self.register_type_alias_decl(decl, item_span);
                            }
                            // Function-signature registration runs over
                            // module-graph bodies before the root import
                            // declarations are processed. Seed each source
                            // trait under its exact module owner here so a
                            // same-leaf declaration (`alpha.Render` /
                            // `beta.Render`) has a canonical identity when
                            // `register_trait_method_sig` mints its DefIds.
                            // Import processing later owns visibility and
                            // binding publication; this is declaration
                            // identity only.
                            Item::Trait(td) => {
                                let qualified = format!("{module_name}.{}", td.name);
                                self.trait_defs.entry(qualified).or_insert_with(|| {
                                    Self::trait_info_from_decl(
                                        td,
                                        Some(module_name.clone()),
                                        self.current_module_idx,
                                    )
                                });
                            }
                            // Register machine state/event binding tables for the
                            // non-root module path, mirroring the root-loop arm at
                            // line ~1029. Deliberately skips
                            // `register_machine_type_namespace_names` (which claims
                            // `type_def_spans`) because the import-surface path
                            // handles namespace dedup for exported names; claiming
                            // spans here would cause false duplicate-definition
                            // errors when the import path later registers the same
                            // machine. Idempotency guard matches `pre_register_type_decl`.
                            _ => {}
                        }
                    }
                    for e in &mut self.errors[err_before..] {
                        if e.source_module.is_none() {
                            e.source_module = Some(module_name.clone());
                        }
                    }
                    for w in &mut self.warnings[warn_before..] {
                        if w.source_module.is_none() {
                            w.source_module = Some(module_name.clone());
                        }
                    }
                    self.local_type_defs = saved_local_type_defs;
                    self.source_type_defs = saved_source_type_defs;
                }
            }
        }
        self.current_module = None;
        self.current_module_idx = 0;

        // The root module follows the same source-order-independent rule as
        // imported modules: direct, canonical lifecycle import edges and every
        // root-owned nominal name must be visible before the first record/enum
        // member annotation is resolved. This is deliberately a narrow seed:
        // only graph-proven shipped lifecycle sources acquire ABI authority.
        if let Some(ref mg) = program.module_graph {
            if let Some(root) = mg.modules.get(&mg.root) {
                self.seed_resolved_lifecycle_import_bindings(root, None, mg);
            }
        }
        self.seed_type_registration_scope(&program.items);

        // Process root module items (full registration with namespace dedup).
        for (item, span) in &program.items {
            match item {
                Item::TypeDecl(td) => {
                    if !self.register_type_namespace_name(None, &td.name, span) {
                        continue;
                    }
                    self.register_type_decl(td);
                    self.local_type_defs.insert(td.name.clone());
                    self.source_type_defs.insert(td.name.clone());
                }
                Item::Actor(ad) => {
                    if !self.register_type_namespace_name(None, &ad.name, span) {
                        continue;
                    }
                    self.register_actor_decl(ad);
                    self.local_type_defs.insert(ad.name.clone());
                    self.source_type_defs.insert(ad.name.clone());
                }
                Item::TypeAlias(ta) => {
                    if !self.register_type_namespace_name(None, &ta.name, span) {
                        continue;
                    }
                    self.register_type_alias_decl(ta, span);
                    self.local_type_defs.insert(ta.name.clone());
                    self.source_type_defs.insert(ta.name.clone());
                }
                Item::Trait(td) => {
                    if !self.register_type_namespace_name(None, &td.name, span) {
                        continue;
                    }
                    let mut trait_errors = Vec::new();
                    let info = Self::trait_info_from_decl_with_diagnostics(
                        td,
                        self.current_module.clone(),
                        self.current_module_idx,
                        &mut trait_errors,
                    );
                    self.errors.extend(trait_errors);
                    self.trait_defs.insert(td.name.clone(), info);
                    self.local_trait_defs.insert(td.name.clone());
                    // Record super-trait relationships
                    if let Some(supers) = &td.super_traits {
                        let super_names: Vec<String> = supers
                            .iter()
                            .map(|s| {
                                self.mark_imported_trait_used(None, &s.name);
                                s.name.clone()
                            })
                            .collect();
                        self.trait_super
                            .insert(td.name.clone(), super_names.clone());
                        if let Some(module) = self.current_module.as_deref() {
                            self.trait_super
                                .insert(format!("{module}.{}", td.name), super_names);
                        }
                    }
                    // Harvest `#[lang_item("…")]` attributes into the
                    // lang-item registry so downstream passes (HIR f-string
                    // lowering) can discover the trait/method names by role
                    // rather than by hard-coded surface symbols. Trait-level
                    // tags register with `method_name: None`; method-level
                    // tags carry the enclosing trait's name so HIR can build
                    // the `<SelfType>::<method>` impl symbol.
                    self.register_trait_lang_items(td, span.clone());
                }
                Item::Supervisor(sd) => {
                    self.reject_wasm_feature(span, WasmUnsupportedFeature::SupervisionTrees);
                    if !self.register_type_namespace_name(None, &sd.name, span) {
                        continue;
                    }
                    // Root items: `current_module` is cleared above, so the
                    // declaration identity is the bare name.
                    let identity = self.declaration_identity(&sd.name);
                    self.register_supervisor_decl_as(sd, &identity);
                    self.local_type_defs.insert(identity.clone());
                    self.source_type_defs.insert(identity);
                }
                Item::Record(rd) => {
                    if !self.register_type_namespace_name(None, &rd.name, span) {
                        continue;
                    }
                    self.register_record_decl(rd);
                    self.local_type_defs.insert(rd.name.clone());
                    self.source_type_defs.insert(rd.name.clone());
                }
                // Machines are normalized into ordinary declarations before
                // registration runs.
                Item::Import(_)
                | Item::Const(_)
                | Item::Impl(_)
                | Item::Machine(_)
                | Item::Function(_)
                | Item::ExternBlock(_) => {}
            }
        }
    }

    /// Register the target under the declaration's canonical identity. Imports
    /// publish bindings separately and never create another alias definition.
    pub(super) fn register_type_alias_decl(
        &mut self,
        decl: &hew_parser::ast::TypeAliasDecl,
        span: &Span,
    ) {
        let path = scoped_module_item_name(self.current_module.as_deref(), &decl.name)
            .unwrap_or_else(|| decl.name.clone());
        let Some(declaration) = self.require_declaration_path(&path, span) else {
            return;
        };
        let identity = declaration.full_path().to_string();
        self.known_types.insert(identity.clone());
        self.type_visibility.insert(
            identity.clone(),
            (decl.visibility, self.current_module.clone()),
        );
        self.type_def_spans
            .entry(identity.clone())
            .or_insert_with(|| span.clone());
        let type_params: Vec<String> = decl
            .type_params
            .iter()
            .flatten()
            .map(|param| param.name.clone())
            .collect();
        self.generic_ctx.push(
            type_params
                .iter()
                .map(|param| (param.clone(), Ty::named(param, vec![])))
                .collect(),
        );
        let mut holes = Vec::new();
        let target = self.resolve_type_expr_tracking_holes(&decl.ty, &mut holes);
        self.generic_ctx.pop();
        self.type_aliases.insert(
            identity.clone(),
            TypeAliasDef {
                declaration,
                type_params,
                target,
                source_module: self.current_module.clone(),
                file_index: self.current_module_idx,
            },
        );
        self.record_type_def_inference_holes(&identity, holes);
    }

    /// Complete alias targets once all lexical imports have been published.
    pub(in crate::check) fn resolve_alias_declarations(&mut self, program: &Program) {
        if let Some(graph) = &program.module_graph {
            let indices = graph.file_span_indices();
            for module_id in &graph.topo_order {
                if *module_id == graph.root {
                    continue;
                }
                let Some(module) = graph.modules.get(module_id) else {
                    continue;
                };
                self.current_module = Some(module_id.path.join("."));
                for (index, (item, span)) in module.items.iter().enumerate() {
                    self.current_module_idx =
                        indices.item_index(module_id, index).unwrap_or_default();
                    if let Item::TypeAlias(decl) = item {
                        self.register_type_alias_decl(decl, span);
                    }
                }
            }
        }
        self.current_module = None;
        self.current_module_idx = 0;
        for (item, span) in &program.items {
            if let Item::TypeAlias(decl) = item {
                self.register_type_alias_decl(decl, span);
            }
        }
        for (name, alias) in self.type_aliases.clone() {
            if self.alias_expansion_is_recursive(&name) {
                let span = self.type_def_spans.get(&name).cloned().unwrap_or_default();
                let mut error = TypeError::new(TypeErrorKind::InvalidOperation, span,
                    format!("type alias `{name}` is recursive: aliases cannot refer to themselves, directly or through a chain"));
                error.source_module = alias.source_module;
                self.errors.push(error);
            }
        }
    }

    pub(in crate::check) fn resolved_type_aliases(
        &mut self,
    ) -> HashMap<crate::DefId, TypeAliasDef> {
        let previous_module = self.current_module.clone();
        let previous_index = self.current_module_idx;
        let aliases = self.type_aliases.values().cloned().collect::<Vec<_>>();
        let mut resolved = HashMap::new();
        for mut alias in aliases {
            self.current_module.clone_from(&alias.source_module);
            self.current_module_idx = alias.file_index;
            alias.target = self
                .normalize_for_type_params(&alias.target, &alias.type_params)
                .materialize_literal_defaults();
            resolved.insert(alias.declaration.clone(), alias);
        }
        self.current_module = previous_module;
        self.current_module_idx = previous_index;
        resolved
    }

    /// Seed root-owned names before resolving any root declaration members.
    ///
    /// Besides making declarations source-order independent, this is the
    /// local-shadow boundary for source-owned lifecycle spellings: a local
    /// `CrashNotification` remains an ordinary user type even when a canonical
    /// std import with the same leaf name is present.
    pub(super) fn seed_type_registration_scope(&mut self, items: &[Spanned<Item>]) {
        for (item, _) in items {
            match item {
                Item::TypeDecl(td) => {
                    self.local_type_defs.insert(td.name.clone());
                    self.source_type_defs.insert(td.name.clone());
                }
                Item::Actor(ad) => {
                    self.local_type_defs.insert(ad.name.clone());
                    self.source_type_defs.insert(ad.name.clone());
                }
                Item::TypeAlias(ta) => {
                    self.local_type_defs.insert(ta.name.clone());
                    self.source_type_defs.insert(ta.name.clone());
                }
                Item::Record(rd) => {
                    self.local_type_defs.insert(rd.name.clone());
                    self.source_type_defs.insert(rd.name.clone());
                }
                _ => {}
            }
        }
    }

    /// Pass 1.5 — re-resolve type-declaration MEMBER types after import
    /// processing (#2202).
    ///
    /// `collect_types` (Pass 1) resolves record/struct field types, enum-variant
    /// payload types, and machine state/event field types BEFORE
    /// `collect_functions` (Pass 2) processes imports and populates the
    /// import-alias maps (`published_bare_type_owners` / `import_type_name_aliases`).
    /// A bare import alias used in member position therefore froze as an
    /// unresolved `Named("Tag")` while its construction (Pass 3) resolves to the
    /// canonical `aliassrc.Payload`, producing a spurious mismatch.
    ///
    /// This pass runs immediately after `collect_functions`, when every module's
    /// alias maps are live, and re-resolves each type declaration's member types
    /// under the OWNING module's context. A member that upgrades from a bare
    /// alias to its canonical qualified identity is committed back into
    /// `type_defs` (bare + module-qualified keys) and the member-derived facts
    /// are re-run over the canonical types: the structural marker set
    /// (`register_type` — Send/Copy/Frozen/Clone/Encode), the
    /// `Serializable` member set, the per-module qualified marker
    /// mirror (the ask-reply Send-gate anti-clobber), the variant-constructor
    /// `fn_sigs`, the wire codec layout, and the `Encode`-driven JSON/YAML/TOML
    /// methods. Members that did not change are left untouched, so the common
    /// (alias-free) path is a no-op and no derivation is re-run.
    ///
    /// The local-shadow rule is preserved: a local `type U` shadowing an import
    /// alias keeps `local_type_defs`/`source_type_defs` populated for the owning
    /// module, so `published_bare_type_qualified` returns `None` and the member
    /// stays bound to the local definition. Diagnostics emitted while
    /// re-resolving are dropped by this driver: Pass 1 already emitted for
    /// genuinely-unresolvable members and the value/use sites (Pass 3) re-emit,
    /// so this upgrade-only pass must never be the sole emitter.
    pub(in crate::check) fn reresolve_member_types_after_imports(&mut self, program: &Program) {
        let errors_before = self.errors.len();
        let warnings_before = self.warnings.len();
        let preferred_modules = collision_preferred_package_module_ids(program, &HashSet::new());
        // Member re-resolution is a secondary fix-up pass (it overwrites the
        // member types computed during `collect_types` once imports are
        // visible) and runs with `type_decls_registered` already true. Suppress
        // the undefined-named-type guard for its duration: a type declaration's
        // members are out of the F1 diagnostic's remit (they keep the existing
        // `E_MIR: unknown type` path), and emitting here would also let the
        // guard substitute `Ty::Error` for the member type, overwriting the
        // good type computed during `collect_types` and tripping the HIR
        // field-access checker-boundary conversion downstream.
        let prev_suppress = self.suppress_undefined_type_report;
        self.suppress_undefined_type_report = true;

        if let Some(ref mg) = program.module_graph {
            let span_indices = mg.file_span_indices();
            for mod_id in &mg.topo_order {
                if *mod_id == mg.root {
                    continue;
                }
                let Some(module) = mg.modules.get(mod_id) else {
                    continue;
                };
                self.current_module = Some(mod_id.path.join("."));
                let saved_local_type_defs = self.local_type_defs.clone();
                let saved_source_type_defs = self.source_type_defs.clone();
                self.seed_member_reresolution_scope(&module.items);
                for (item_idx, (item, item_span)) in module.items.iter().enumerate() {
                    self.current_module_idx = span_indices
                        .item_index(mod_id, item_idx)
                        .unwrap_or_default();
                    if member_item_is_absorbed_from_distinct_child(
                        program,
                        &preferred_modules,
                        mod_id,
                        item,
                        item_span,
                    ) {
                        continue;
                    }
                    self.reresolve_item_member_types(item);
                }
                self.local_type_defs = saved_local_type_defs;
                self.source_type_defs = saved_source_type_defs;
            }
        }

        self.current_module = None;
        self.current_module_idx = 0;
        let saved_local_type_defs = self.local_type_defs.clone();
        let saved_source_type_defs = self.source_type_defs.clone();
        self.seed_member_reresolution_scope(&program.items);
        for (item, _) in &program.items {
            self.reresolve_item_member_types(item);
        }
        self.local_type_defs = saved_local_type_defs;
        self.source_type_defs = saved_source_type_defs;

        self.errors.truncate(errors_before);
        self.warnings.truncate(warnings_before);
        self.suppress_undefined_type_report = prev_suppress;
    }

    /// Admit `optional` wire fields only when their final semantic type is
    /// `Option<T>`.
    ///
    /// This deliberately runs after [`Self::reresolve_member_types_after_imports`].
    /// The parser cannot make this decision from syntax without rejecting a
    /// valid alias, and the first registration pass predates import aliases.
    /// Keeping this as the one checker-side gate means every later consumer
    /// sees either an admitted `Option<T>` field or a hard type error.
    pub(in crate::check) fn validate_wire_optional_field_admission(&mut self, program: &Program) {
        if let Some(module_graph) = &program.module_graph {
            for module_id in &module_graph.topo_order {
                if *module_id == module_graph.root {
                    continue;
                }
                let Some(module) = module_graph.modules.get(module_id) else {
                    continue;
                };
                self.current_module = Some(module_id.path.join("."));
                for (item, _) in &module.items {
                    self.validate_type_decl_wire_optional_fields(item);
                }
            }
        }

        self.current_module = None;
        for (item, _) in &program.items {
            self.validate_type_decl_wire_optional_fields(item);
        }
        self.current_module_idx = 0;
    }

    pub(super) fn validate_type_decl_wire_optional_fields(&mut self, item: &Item) {
        let Item::TypeDecl(type_decl) = item else {
            return;
        };
        let Some(wire) = &type_decl.wire else {
            return;
        };

        let type_def_key = self.authoritative_type_def_key(&type_decl.name);
        let Some(type_def) = self.type_defs.get(&type_def_key).cloned() else {
            return;
        };
        let fields = type_def.fields.clone();
        self.validate_wire_type_members(type_decl, &type_def);

        for metadata in wire.field_meta.iter().filter(|field| field.is_optional) {
            let field_span = type_decl
                .body
                .iter()
                .find_map(|item| match item {
                    TypeBodyItem::Field { name, ty, .. } if name == &metadata.field_name => {
                        Some(ty.1.clone())
                    }
                    _ => None,
                })
                .unwrap_or(0..0);
            let resolved_ty = fields
                .get(&metadata.field_name)
                .map(|field_ty| self.normalize_for_use(field_ty));

            if resolved_ty.as_ref().and_then(Ty::as_option).is_none() {
                self.report_error(
                    TypeErrorKind::WireOptionalFieldRequiresOption,
                    &field_span,
                    format!(
                        "E_WIRE_OPTIONAL_REQUIRES_OPTION: wire field `{}` is marked `optional` but must have type `Option<T>`",
                        metadata.field_name
                    ),
                );
            }
        }
    }

    /// Every member of a `#[wire]` declaration must itself have a wire
    /// encoding; the resolved member types come from its checked definition
    /// and the spans from the source declaration.
    pub(super) fn validate_wire_type_members(&mut self, type_decl: &TypeDecl, type_def: &TypeDef) {
        let identity = self.current_module_identity().map_or_else(
            || type_decl.name.clone(),
            |module| format!("{module}.{}", type_decl.name),
        );
        let mut members = Vec::new();
        for item in &type_decl.body {
            match item {
                TypeBodyItem::Field { name, ty, .. } => {
                    if let Some(field_ty) = type_def.fields.get(name) {
                        members.push((format!("field `{name}`"), field_ty.clone(), ty.1.clone()));
                    }
                }
                TypeBodyItem::Variant(variant) => {
                    let payload: Vec<(String, Ty, Span)> =
                        match (&variant.kind, type_def.variants.get(&variant.name)) {
                            (VariantKind::Tuple(spans), Some(VariantDef::Tuple(tys))) => spans
                                .iter()
                                .zip(tys)
                                .enumerate()
                                .map(|(index, (span, ty))| {
                                    (
                                        format!("variant `{}` payload {index}", variant.name),
                                        ty.clone(),
                                        span.1.clone(),
                                    )
                                })
                                .collect(),
                            (VariantKind::Struct(spans), Some(VariantDef::Struct(fields))) => spans
                                .iter()
                                .filter_map(|(name, span)| {
                                    let (_, ty) = fields.iter().find(|(field, _)| field == name)?;
                                    Some((
                                        format!("variant `{}` field `{name}`", variant.name),
                                        ty.clone(),
                                        span.1.clone(),
                                    ))
                                })
                                .collect(),
                            _ => Vec::new(),
                        };
                    members.extend(payload);
                }
                TypeBodyItem::Method(_) => {}
            }
        }
        self.validate_wire_type_encoding(&identity, members);
    }

    /// Seed `local_type_defs`/`source_type_defs` with the current scope's own
    /// type names so member re-resolution (a) treats them as locally-defined
    /// (no fresh-var injection) and (b) shadows any same-named import alias —
    /// the local-shadow rule. Mirrors the seeding `collect_types` performs.
    pub(super) fn seed_member_reresolution_scope(&mut self, items: &[Spanned<Item>]) {
        for (item, _) in items {
            match item {
                Item::TypeDecl(td) => {
                    self.local_type_defs.insert(td.name.clone());
                    self.source_type_defs.insert(td.name.clone());
                }
                Item::Record(rd) => {
                    self.local_type_defs.insert(rd.name.clone());
                    self.source_type_defs.insert(rd.name.clone());
                }
                Item::Actor(ad) => {
                    self.source_type_defs.insert(ad.name.clone());
                }
                Item::TypeAlias(ta) => {
                    self.local_type_defs.insert(ta.name.clone());
                    self.source_type_defs.insert(ta.name.clone());
                }
                _ => {}
            }
        }
    }

    pub(super) fn reresolve_item_member_types(&mut self, item: &Item) {
        match item {
            Item::TypeDecl(td) => self.reresolve_type_decl_members(td),
            Item::Record(rd) => self.reresolve_record_members(rd),
            Item::Machine(md) => self.reresolve_machine_members(md),
            Item::Actor(ad) => self.reresolve_actor_members(ad),
            _ => {}
        }
    }

    /// Re-resolve an actor's state fields and init parameters.
    ///
    /// An actor is the type of its handle (D489), and the handle carrier is
    /// stamped by [`Self::canonicalize_actor_handles`] from the declarations
    /// registered so far. `actor Alpha { let beta: Beta }` with `Beta`
    /// declared below it, or in a module `collect_functions` registers later,
    /// therefore froze `beta` as a bare nominal while `Beta`'s own back
    /// reference carried the discriminator. Declaration order is not a
    /// semantic fact, so this pass re-reads the same authority once every
    /// actor, supervisor and module is registered.
    pub(super) fn reresolve_actor_members(&mut self, ad: &ActorDecl) {
        let has_type_params = !ad.type_params.is_empty();
        if has_type_params {
            let bounds = self.collect_type_param_bounds(Some(&ad.type_params), None);
            self.current_type_param_bounds
                .push(TypeParamScope::new(bounds, HashMap::new()));
        }
        let mut hole_vars = Vec::new();
        let mut fields: HashMap<String, Ty> = HashMap::new();
        for field in &ad.fields {
            let field_ty = self.resolve_registered_annotation_ty(&field.ty, &mut hole_vars);
            fields.insert(field.name.clone(), field_ty);
        }
        let init_params: Vec<ActorInitParamInfo> = ad.init.as_ref().map_or_else(Vec::new, |init| {
            init.params
                .iter()
                .map(|p| ActorInitParamInfo {
                    name: p.name.clone(),
                    ty: self.resolve_registered_annotation_ty(&p.ty, &mut hole_vars),
                })
                .collect()
        });
        if has_type_params {
            self.current_type_param_bounds.pop();
        }

        let identity = self.authoritative_type_def_key(&ad.name);
        let mut changed = false;
        if let Some(stored) = self.type_defs.get_mut(&identity) {
            if stored.kind == TypeDefKind::Actor && stored.fields != fields {
                stored.fields = fields;
                changed = true;
            }
        }
        if let Some(stored) = self.actor_init_params.get_mut(&identity) {
            if *stored != init_params {
                *stored = init_params;
                changed = true;
            }
        }
        if changed {
            self.handle_bearing_dirty = true;
        }
    }

    /// The collision-free key under which this scope's `TypeDef` is stored: the
    /// module-qualified `{module_short}.{name}` for a non-root module (when it
    /// exists), else the bare `name` for the root program.
    pub(super) fn authoritative_type_def_key(&self, bare_name: &str) -> String {
        if let Some(module_owner) = self.current_module_identity() {
            let qualified = format!("{module_owner}.{bare_name}");
            if self.type_defs.contains_key(&qualified) {
                return qualified;
            }
        }
        bare_name.to_string()
    }

    /// Commit a re-resolved `TypeDef` under its declaration identity.
    /// Non-root declarations publish only their full owner; root declarations
    /// retain the bare key because that is their canonical identity.
    pub(super) fn commit_reresolved_type_def(&mut self, name: &str, type_def: TypeDef) {
        if let Some(module_owner) = self.current_module_identity().map(str::to_string) {
            self.register_canonical_type_def(&module_owner, name, &type_def);
        } else {
            self.type_defs.insert(name.to_string(), type_def);
        }
        self.handle_bearing_dirty = true;
    }

    /// Re-resolve a `type`/`enum` declaration's member types under the now-live
    /// import-alias maps; on a member upgrade, patch `type_defs` and re-run every
    /// member-derived fact. Mirrors `register_type_decl`'s member resolution and
    /// derivation tail. No-op when no member changed.
    pub(super) fn reresolve_type_decl_members(&mut self, td: &TypeDecl) {
        let scope =
            self.enter_primary_sig_scope(&[(td.type_params.as_ref(), td.where_clause.as_ref())]);
        self.reresolve_type_decl_members_in_scope(td);
        self.exit_primary_sig_scope(scope);
    }

    pub(super) fn reresolve_type_decl_members_in_scope(&mut self, td: &TypeDecl) {
        let kind = match td.kind {
            TypeDeclKind::Struct => TypeDefKind::Struct,
            TypeDeclKind::Enum => TypeDefKind::Enum,
        };
        let type_param_names: Vec<String> = td.type_params.as_ref().map_or(vec![], |params| {
            params.iter().map(|p| p.name.clone()).collect()
        });

        let mut fields = HashMap::new();
        let mut field_order: Vec<String> = Vec::new();
        let mut variants = HashMap::new();
        let mut hole_vars = Vec::new();
        for item in &td.body {
            match item {
                TypeBodyItem::Field { name, ty, .. } => {
                    let field_ty = self.resolve_registered_annotation_ty(ty, &mut hole_vars);
                    field_order.push(name.clone());
                    fields.insert(name.clone(), field_ty);
                }
                TypeBodyItem::Variant(variant) => match &variant.kind {
                    VariantKind::Unit => {
                        variants.insert(variant.name.clone(), VariantDef::Unit);
                    }
                    VariantKind::Tuple(tuple_fields) => {
                        let variant_tys: Vec<Ty> = tuple_fields
                            .iter()
                            .map(|f| self.resolve_registered_annotation_ty(f, &mut hole_vars))
                            .collect();
                        variants.insert(variant.name.clone(), VariantDef::Tuple(variant_tys));
                    }
                    VariantKind::Struct(struct_fields) => {
                        let variant_fields: Vec<(String, Ty)> = struct_fields
                            .iter()
                            .map(|(n, f)| {
                                (
                                    n.clone(),
                                    self.resolve_registered_annotation_ty(f, &mut hole_vars),
                                )
                            })
                            .collect();
                        variants.insert(variant.name.clone(), VariantDef::Struct(variant_fields));
                    }
                },
                TypeBodyItem::Method(_) => {}
            }
        }

        let stored_key = self.authoritative_type_def_key(&td.name);
        let Some(stored) = self.type_defs.get(&stored_key) else {
            return;
        };
        if stored.fields == fields && stored.variants == variants {
            return;
        }

        let type_def = TypeDef {
            kind,
            name: td.name.clone(),
            type_params: type_param_names.clone(),
            bounds: stored.bounds.clone(),
            fields,
            field_order,
            variants,
            methods: stored.methods.clone(),
            doc_comment: td.doc_comment.clone(),
            is_indirect: td.is_indirect,
        };

        // Re-key tuple variant constructors over the canonical payload types.
        // Unit/struct variants carry no member-dependent constructor signature.
        for (variant_name, variant_def) in &type_def.variants {
            if let VariantDef::Tuple(variant_tys) = variant_def {
                if let Some(sig) = self.fn_sigs.get_mut(variant_name) {
                    sig.params.clone_from(variant_tys);
                }
            }
        }

        // Re-derive member-dependent facts (all replace-semantics).
        let field_types: Vec<Ty> = if kind == TypeDefKind::Enum {
            Self::structural_member_types_for_type(&type_def)
        } else {
            type_def.fields.values().cloned().collect()
        };
        let field_types = self.expand_for_marker_registration(&field_types);
        self.registry.register_type(td.name.clone(), field_types);
        self.seed_qualified_type_markers_for_current_module(&td.name);
        self.commit_reresolved_type_def(&td.name, type_def);

        if let Some(ref wire) = td.wire {
            let variant_order: Vec<String> = td
                .body
                .iter()
                .filter_map(|i| match i {
                    TypeBodyItem::Variant(v) => Some(v.name.clone()),
                    _ => None,
                })
                .collect();
            self.register_wire_methods(&td.name, wire, &variant_order);
        }
    }

    /// Re-resolve a `record` declaration's member types. Mirrors
    /// `register_record_decl`'s named/tuple split and derivation tail. No-op when
    /// no member changed. Positional constructors use the same canonical declaration key.
    pub(super) fn reresolve_record_members(&mut self, rd: &RecordDecl) {
        let scope =
            self.enter_primary_sig_scope(&[(rd.type_params.as_ref(), rd.where_clause.as_ref())]);
        self.reresolve_record_members_in_scope(rd);
        self.exit_primary_sig_scope(scope);
    }
}
