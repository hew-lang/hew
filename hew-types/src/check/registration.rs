#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;
use crate::BuiltinType;

/// Embedded source for `std/io/closable.hew`.
///
/// Parsed at import-registration time for `std::io::closable` so the
/// `Closable` trait and `CloseError` enum are visible in the checker even
/// in programs that were not loaded through the module-graph path (e.g.
/// inline programs in tests).
const CLOSABLE_HEW: &str = include_str!("../../../std/io/closable.hew");

/// Embedded source for the built-in actor monitor handle wrapper.
///
/// This copy intentionally omits the `import std::io::closable;` line used by
/// the on-disk stdlib module because inline checker tests register the
/// `Closable` / `CloseError` surface directly before parsing this snippet.
const MONITOR_REF_HEW: &str = r#"
pub type MonitorRef {
    ref_id: i64;
}

/// Error returned by `link(handle)`.
///
/// `AlreadyLinked` is an idempotent non-fatal condition — callers may
/// treat it as `Ok(())`.  `TargetDead` is returned when the target actor
/// has already exited; the caller's exit handler will fire immediately.
pub enum LinkError {
    AlreadyLinked;
    TargetDead;
}

impl Closable for MonitorRef {
    fn close(monitor_ref: MonitorRef) -> Result<(), CloseError> {
        unsafe {
            hew_actor_demonitor(monitor_ref.ref_id)
        };
        Ok(())
    }
}

impl Drop for MonitorRef {
    fn drop(monitor_ref: MonitorRef) {
        unsafe {
            hew_actor_demonitor(monitor_ref.ref_id)
        };
    }
}

extern "C" {
    fn hew_actor_demonitor(ref_id: i64);
}
"#;

/// Embedded source for `std/failure.hew`.
///
/// Parsed at import-registration time for `std::failure` so the
/// `CrashInfo` struct and `CrashAction` enum used in `#[on(crash)]` hook
/// signatures are visible in the checker even in programs that were not
/// loaded through the module-graph path (e.g. inline programs in tests).
///
/// Mirrors the on-disk `std/failure.hew` byte-for-byte modulo the same
/// `import` line elision applied to `MONITOR_REF_HEW`: inline tests do
/// not flow through the import resolver, so the embedded snippet omits
/// any top-of-file imports the file itself does not need (none today).
const FAILURE_HEW: &str = r"
pub type CrashInfo {
    code: i64;
}

pub enum CrashAction {
    Restart;
    Escalate;
    Kill;
}
";

/// Embedded source for the built-in `LookupError` enum used as the `Err`
/// variant of `Node::lookup<T>(name) -> Result<RemotePid<T>, LookupError>`.
///
/// Inline tests do not flow through the import resolver, so this stdlib
/// surface is registered directly into the checker via
/// `register_stdlib_hew_items` the same way `LinkError` / `CrashAction`
/// are bootstrapped from `MONITOR_REF_HEW` / `FAILURE_HEW`.
const LOOKUP_ERROR_HEW: &str = r"
pub enum LookupError {
    NotFound;
}
";

/// Raw TOML text of `scripts/jit-symbol-classification.toml`, embedded at
/// compile time so `extern "rt"` validation does not require a runtime file
/// read and works in test environments without a workspace checkout.
const JIT_CLASSIFICATION_TOML: &str =
    include_str!("../../../scripts/jit-symbol-classification.toml");

/// Parse the `stable = [ ... ]` and `stable-stdlib = [ ... ]` blocks from
/// `JIT_CLASSIFICATION_TOML` and return their union.
///
/// Returns a `HashSet<&'static str>` so membership checks are O(1).
/// The parsing is line-based (no full TOML dep): each quoted string inside
/// either block is extracted. The `codegen-stable` and `internal` blocks
/// are excluded — those tiers are not user-callable via `extern "rt"`.
///
/// `stable` covers runtime exports; `stable-stdlib` covers sibling stdlib
/// crate exports (e.g. `hew_datetime_*`) that user code is permitted to
/// name from an `extern "rt"` block and that the native linker pulls in.
///
/// WHY no dep: the block format is simple and has been stable since the file
/// was introduced; adding a toml dep to hew-types for a single string-list
/// parse is unjustified overhead.
fn jit_stable_symbols() -> &'static std::collections::HashSet<&'static str> {
    static SET: OnceLock<std::collections::HashSet<&'static str>> = OnceLock::new();
    SET.get_or_init(|| {
        let mut set = std::collections::HashSet::new();
        for header in ["stable = [", "stable-stdlib = ["] {
            let mut inside = false;
            for line in JIT_CLASSIFICATION_TOML.lines() {
                let trimmed = line.trim();
                if trimmed.starts_with(header) {
                    inside = true;
                    continue;
                }
                if inside && trimmed == "]" {
                    break;
                }
                if inside {
                    if let Some(rest) = trimmed.strip_prefix('"') {
                        if let Some(sym) = rest.split('"').next() {
                            if !sym.is_empty() {
                                set.insert(sym);
                            }
                        }
                    }
                }
            }
        }
        set
    })
}

impl Checker {
    fn refresh_handle_bearing_structs(&mut self) {
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
    pub(super) fn ensure_handle_bearing_fresh(&mut self) {
        if self.handle_bearing_dirty {
            self.handle_bearing_dirty = false;
            self.refresh_handle_bearing_structs();
        }
    }

    fn type_name_contains_owned_handle(
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

    fn ty_contains_owned_handle(&self, ty: &Ty, visiting: &mut HashSet<String>) -> bool {
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
            | Ty::Duration
            | Ty::Unit
            | Ty::Never
            | Ty::Var(_)
            | Ty::Function { .. }
            | Ty::Closure { .. }
            | Ty::Pointer { .. }
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

    pub(super) fn canonical_owned_handle_type_name(&self, type_name: &str) -> Option<String> {
        if self.module_registry.drop_func_for(type_name).is_some()
            || self.module_registry.is_drop_type(type_name)
            || self.module_registry.is_handle_type(type_name)
        {
            return Some(type_name.to_string());
        }

        let qualified = self.module_registry.qualify_handle_type(type_name)?;
        (self.module_registry.drop_func_for(&qualified).is_some()
            || self.module_registry.is_drop_type(&qualified)
            || self.module_registry.is_handle_type(&qualified))
        .then_some(qualified)
    }

    pub(super) fn registered_type_def_name(&self, name: &str) -> Option<String> {
        if self.type_defs.contains_key(name) {
            return Some(name.to_string());
        }
        self.strip_module_prefix(name)
            .filter(|unqualified| self.type_defs.contains_key(*unqualified))
            .map(str::to_string)
    }

    fn register_rcfree_members_for_type(&mut self, type_name: &str, type_def: &TypeDef) {
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
        self.registry
            .register_rcfree_members(type_name.to_string(), member_types);
    }

    #[expect(
        clippy::too_many_lines,
        reason = "all builtins registered in one place"
    )]
    pub(super) fn register_builtins(&mut self) {
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

        // Math functions
        self.register_builtin_fn("abs", vec![Ty::I64], Ty::I64);
        self.register_builtin_fn("sqrt", vec![Ty::F64], Ty::F64);
        self.register_builtin_fn("min", vec![Ty::I64, Ty::I64], Ty::I64);
        self.register_builtin_fn("max", vec![Ty::I64, Ty::I64], Ty::I64);
        self.register_builtin_fn("to_float", vec![Ty::I64], Ty::F64);

        // String operations
        self.register_builtin_fn("string_concat", vec![Ty::String, Ty::String], Ty::String);
        self.register_builtin_fn("string_length", vec![Ty::String], Ty::I64);
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
        self.register_builtin_fn("read_file", vec![Ty::String], Ty::String);
        self.register_builtin_fn("write_file", vec![Ty::String, Ty::String], Ty::Unit);
        self.register_builtin_fn("sleep_ms", vec![Ty::I64], Ty::Unit);
        self.register_builtin_fn("sleep", vec![Ty::I64], Ty::Unit);
        self.register_builtin_fn("stop", vec![Ty::Var(TypeVar::fresh())], Ty::Unit);
        let close_t = TypeVar::fresh();
        self.register_builtin_fn(
            "close",
            vec![Ty::local_pid(Ty::Var(close_t))],
            Ty::local_pid(Ty::Var(close_t)),
        );
        self.register_builtin_fn("exit", vec![Ty::I64], Ty::Never);
        self.register_builtin_fn("panic", vec![Ty::String], Ty::Never);

        // Actor link/monitor (Erlang-style fault propagation)
        // `link` is idempotent on already-linked actors; `AlreadyLinked` and
        // `TargetDead` are the error discriminants (declared in std/link_monitor.hew,
        // B3 slice). `monitor` returns the handle the caller uses to stop watching.
        let link_t = TypeVar::fresh();
        self.register_builtin_fn(
            "link",
            vec![Ty::local_pid(Ty::Var(link_t))],
            Ty::result(Ty::Unit, Ty::link_error()),
        );
        let unlink_t = TypeVar::fresh();
        self.register_builtin_fn("unlink", vec![Ty::local_pid(Ty::Var(unlink_t))], Ty::Unit);
        let monitor_t = TypeVar::fresh();
        self.register_builtin_fn(
            "monitor",
            vec![Ty::local_pid(Ty::Var(monitor_t))],
            Ty::monitor_ref(),
        );

        // Supervisor child access
        let sup_child_t = TypeVar::fresh();
        let sup_child_ret = TypeVar::fresh();
        self.register_builtin_fn(
            "supervisor_child",
            vec![Ty::local_pid(Ty::Var(sup_child_t)), Ty::I64],
            Ty::local_pid(Ty::Var(sup_child_ret)),
        );
        let sup_stop_t = TypeVar::fresh();
        self.register_builtin_fn(
            "supervisor_stop",
            vec![Ty::local_pid(Ty::Var(sup_stop_t))],
            Ty::Unit,
        );

        // Assertions (test support)
        self.register_builtin_fn("assert", vec![Ty::Bool], Ty::Unit);
        self.register_builtin_fn_with_bounds(
            "assert_eq",
            vec!["T".to_string()],
            HashMap::from([("T".to_string(), vec!["Display".to_string()])]),
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
            HashMap::from([("T".to_string(), vec!["Display".to_string()])]),
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

        // Collection constructors (path-style calls: Vec::new(), HashMap::new())
        // These also need fresh vars per call but are less critical
        self.register_builtin_fn(
            "Vec::new",
            vec![],
            Ty::Named {
                builtin: Some(BuiltinType::Vec),
                name: "Vec".to_string(),
                args: vec![Ty::Var(TypeVar::fresh())],
            },
        );
        self.register_builtin_fn(
            "Vec::with_capacity",
            vec![Ty::I64],
            Ty::Named {
                builtin: Some(BuiltinType::Vec),
                name: "Vec".to_string(),
                args: vec![Ty::Var(TypeVar::fresh())],
            },
        );
        self.register_builtin_fn(
            "HashMap::new",
            vec![],
            Ty::Named {
                builtin: Some(BuiltinType::HashMap),
                name: "HashMap".to_string(),
                args: vec![Ty::Var(TypeVar::fresh()), Ty::Var(TypeVar::fresh())],
            },
        );
        self.register_builtin_fn(
            "HashSet::new",
            vec![],
            Ty::Named {
                builtin: Some(BuiltinType::HashSet),
                name: "HashSet".to_string(),
                args: vec![Ty::Var(TypeVar::fresh())],
            },
        );
        self.register_builtin_fn("bytes::new", vec![], Ty::Bytes);
        self.register_builtin_fn("duration::from_nanos", vec![Ty::I64], Ty::Duration);

        // Rc<T> constructor — Rc::new(value: T) -> Rc<T>
        // Each call site gets a fresh type variable; the actual Rc<T> type is
        // inferred from the argument type.
        {
            let t = TypeVar::fresh();
            self.register_builtin_fn("Rc::new", vec![Ty::Var(t)], Ty::rc(Ty::Var(t)));
        }

        // More print variants
        self.register_builtin_fn("println_f64", vec![Ty::F64], Ty::Unit);
        self.register_builtin_fn("print_f64", vec![Ty::F64], Ty::Unit);
        self.register_builtin_fn("println_i64", vec![Ty::I64], Ty::Unit);
        self.register_builtin_fn("println_char", vec![Ty::Char], Ty::Unit);

        // String utilities
        self.register_builtin_fn("string_char_at", vec![Ty::String, Ty::I64], Ty::Char);
        self.register_builtin_fn("string_equals", vec![Ty::String, Ty::String], Ty::Bool);
        self.register_builtin_fn("string_from_int", vec![Ty::I64], Ty::String);
        self.register_builtin_fn("string_contains", vec![Ty::String, Ty::String], Ty::Bool);
        self.register_builtin_fn(
            "string_split",
            vec![Ty::String, Ty::String],
            Ty::Named {
                builtin: Some(BuiltinType::Vec),
                name: "Vec".to_string(),
                args: vec![Ty::String],
            },
        );
        self.register_builtin_fn("string_starts_with", vec![Ty::String, Ty::String], Ty::Bool);
        self.register_builtin_fn("substring", vec![Ty::String, Ty::I64, Ty::I64], Ty::String);
        self.register_builtin_fn(
            "string_slice",
            vec![Ty::String, Ty::I64, Ty::I64],
            Ty::String,
        );
        self.register_builtin_fn("string_trim", vec![Ty::String], Ty::String);
        self.register_builtin_fn("string_to_int", vec![Ty::String], Ty::I64);
        self.register_builtin_fn("string_find", vec![Ty::String, Ty::String], Ty::I64);
        self.register_builtin_fn(
            "string_replace",
            vec![Ty::String, Ty::String, Ty::String],
            Ty::String,
        );
        self.register_builtin_fn("string_to_upper", vec![Ty::String], Ty::String);
        self.register_builtin_fn("string_to_lower", vec![Ty::String], Ty::String);
        self.register_builtin_fn("string_ends_with", vec![Ty::String, Ty::String], Ty::Bool);
        self.register_builtin_fn("int_to_string", vec![Ty::I64], Ty::String);
        self.register_builtin_fn("float_to_string", vec![Ty::F64], Ty::String);
        self.register_builtin_fn("char_to_string", vec![Ty::Char], Ty::String);
        self.register_builtin_fn("bool_to_string", vec![Ty::Bool], Ty::String);

        // Node/distributed builtins
        self.register_builtin_fn("Node::start", vec![Ty::String], Ty::Unit);
        self.register_builtin_fn("Node::shutdown", vec![], Ty::Unit);
        self.register_builtin_fn("Node::connect", vec![Ty::String], Ty::Unit);
        self.register_builtin_fn("Node::set_transport", vec![Ty::String], Ty::Unit);
        // `Node::register<T>(name: String, pid: LocalPid<T>) -> i32`
        // The second argument is tightened to `LocalPid<T>` so that passing a
        // `RemotePid<T>` or bare `u64` is caught at the checker rather than
        // failing with a cryptic codegen error. Codegen already assumes a
        // `LocalPid<T>` alloca (it calls `hew_actor_pid` to extract the u64
        // before forwarding to `hew_node_api_register_by_pid`).
        {
            let t = TypeVar::fresh();
            self.register_builtin_fn(
                "Node::register",
                vec![Ty::String, Ty::local_pid(Ty::Var(t))],
                Ty::I32,
            );
        }
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
                Ty::Named {
                    builtin: None,
                    name: "LookupError".to_string(),
                    args: vec![],
                },
            ),
        );

        // std::math module — always available, no import needed
        self.modules.insert("math".to_string());
        // Single-argument math functions: f64 → f64
        for name in &[
            "exp", "log", "sqrt", "sin", "cos", "floor", "ceil", "abs", "abs_f", "tanh", "log2",
            "log10", "exp2",
        ] {
            self.register_builtin_fn(&format!("math.{name}"), vec![Ty::F64], Ty::F64);
        }
        // Two-argument math functions: (f64, f64) → f64
        for name in &["pow", "max", "min", "max_f", "min_f"] {
            self.register_builtin_fn(&format!("math.{name}"), vec![Ty::F64, Ty::F64], Ty::F64);
        }
        self.register_builtin_fn("math.clamp_f", vec![Ty::F64, Ty::F64, Ty::F64], Ty::F64);
        // Constants (zero-argument): () → f64
        for name in &["pi", "e"] {
            self.register_builtin_fn(&format!("math.{name}"), vec![], Ty::F64);
        }

        // std::random module — always available, no import needed
        self.modules.insert("random".to_string());
        self.register_builtin_fn("random.seed", vec![Ty::I64], Ty::Unit);
        self.register_builtin_fn("random.random", vec![], Ty::F64);
        self.register_builtin_fn("random.gauss", vec![Ty::F64, Ty::F64], Ty::F64);
        self.register_builtin_fn("random.randint", vec![Ty::I64, Ty::I64], Ty::I64);
        self.register_builtin_fn("random.shuffle", vec![Ty::Var(TypeVar::fresh())], Ty::Unit);
        self.register_builtin_fn(
            "random.choices",
            vec![Ty::Var(TypeVar::fresh()), Ty::F64, Ty::I64],
            Ty::I64,
        );

        // Duplex / channel constructors — compiler builtins.
        //
        // WHY: slice 5 codegen and the HIR-level accept fixtures below need these
        //   names resolvable without importing a stdlib module.
        // WHEN-OBSOLETE: once `std::channel` (stdlib slice 6) ships, the stdlib
        //   module replaces these registrations and these calls are removed.
        // WHAT-REAL-SOLUTION: `std/channel.hew` module with proper source definitions.
        //
        // `duplex_pair<S: Send, R: Send>(capacity: int) -> (Duplex<S, R>, Duplex<R, S>)`
        // Returns a cross-wired pair of Duplex handles backed by a shared buffer.
        // All construction goes through this; there is no per-direction constructor
        // (`hew_duplex_new` was removed in the M2 runtime refactor).
        self.register_builtin_fn_with_bounds(
            "duplex_pair",
            vec!["S".to_string(), "R".to_string()],
            HashMap::from([
                ("S".to_string(), vec!["Send".to_string()]),
                ("R".to_string(), vec!["Send".to_string()]),
            ]),
            vec![Ty::I64],
            Ty::Tuple(vec![
                Ty::duplex(
                    Ty::Named {
                        builtin: None,
                        name: "S".to_string(),
                        args: vec![],
                    },
                    Ty::Named {
                        builtin: None,
                        name: "R".to_string(),
                        args: vec![],
                    },
                ),
                Ty::duplex(
                    Ty::Named {
                        builtin: None,
                        name: "R".to_string(),
                        args: vec![],
                    },
                    Ty::Named {
                        builtin: None,
                        name: "S".to_string(),
                        args: vec![],
                    },
                ),
            ]),
        );

        // `duplex<S: Send, R: Send>(capacity: int) -> Duplex<S, R>`
        // Constructs a detached Duplex handle with no peer. A transport must be
        // attached via `.attach(transport)` before send/recv will succeed (the
        // go-nil / detached pattern per §5.16.7 + Q16).
        self.register_builtin_fn_with_bounds(
            "duplex",
            vec!["S".to_string(), "R".to_string()],
            HashMap::from([
                ("S".to_string(), vec!["Send".to_string()]),
                ("R".to_string(), vec!["Send".to_string()]),
            ]),
            vec![Ty::I64],
            Ty::duplex(
                Ty::Named {
                    builtin: None,
                    name: "S".to_string(),
                    args: vec![],
                },
                Ty::Named {
                    builtin: None,
                    name: "R".to_string(),
                    args: vec![],
                },
            ),
        );

        // `channel<T: Send>(capacity: int) -> (Sink<T>, Stream<T>)`
        // Constructs a unidirectional channel pair: the Sink writes, the Stream reads.
        self.register_builtin_fn_with_bounds(
            "channel",
            vec!["T".to_string()],
            HashMap::from([("T".to_string(), vec!["Send".to_string()])]),
            vec![Ty::I64],
            Ty::Tuple(vec![
                Ty::sink(Ty::Named {
                    builtin: None,
                    name: "T".to_string(),
                    args: vec![],
                }),
                Ty::stream(Ty::Named {
                    builtin: None,
                    name: "T".to_string(),
                    args: vec![],
                }),
            ]),
        );

        // Register the eleven `impl Display for <primitive>` blanket impls
        // declared in `std/builtins.hew`.  Without this, method-form
        // primitive Display dispatch (`x.fmt()` for `x: i64` etc.) cannot
        // find an entry in `primitive_trait_impls` because the file is not
        // routed through any `import` that would invoke
        // `register_stdlib_hew_items` for the root module.  The Display
        // *marker* (`MarkerTrait::Display`) already satisfies `T: Display`
        // bounds for `print` / `println`, but receiver-keyed method dispatch
        // (Stage A2 / A3) reads the impl table directly.  See #1669.
        self.register_builtins_hew_impls();
        if !self.module_registry.has_search_paths() {
            self.register_builtin_closable_surface();
            self.register_builtin_monitor_ref_surface();
            self.register_builtin_failure_surface();
            self.register_builtin_lookup_error_surface();
        }
    }

    /// Parse the compiled-in `std/builtins.hew` source and feed only its
    /// `Item::Impl` blocks through the existing stdlib registration path.
    ///
    /// `register_stdlib_hew_items` runs Pass 1 (types/traits/functions) and
    /// Pass 2 (impl methods) on its input.  We deliberately filter to just
    /// the impl items so:
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
    fn register_builtins_hew_impls(&mut self) {
        const BUILTINS_HEW_SOURCE: &str = include_str!("../../../std/builtins.hew");
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
        for (item, _) in &parsed.program.items {
            match item {
                Item::Trait(tr) if tr.visibility.is_pub() => {
                    let info = Self::trait_info_from_decl(tr);
                    self.trait_defs
                        .entry(tr.name.clone())
                        .or_insert_with(|| info.clone());
                    let qualified = format!("builtins.{}", tr.name);
                    self.trait_defs.entry(qualified).or_insert(info);
                    // Harvest #[lang_item("...")] from the stdlib-shipped trait
                    // declaration so HIR f-string lowering can discover the
                    // canonical Display::fmt name through `LangItemRegistry`
                    // even when the user's program never declares the trait
                    // itself. Without this, every `f"…"` lowering in user code
                    // would fail-closed with "no lang-item registered for key
                    // `display_fmt`".
                    self.register_trait_lang_items(tr, 0..0);
                }
                Item::TypeDecl(td) if td.visibility.is_pub() => {
                    self.pre_register_type_decl(td);
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
        self.register_stdlib_hew_items("builtins", &impl_items);
    }

    fn register_builtin_closable_surface(&mut self) {
        let identity = "module:std::io::closable";
        if self.registered_stdlib_hew_sources.contains(identity) {
            return;
        }
        self.registered_stdlib_hew_sources
            .insert(identity.to_string());
        let parsed = hew_parser::parse(CLOSABLE_HEW);
        debug_assert!(
            parsed.errors.is_empty(),
            "std/io/closable.hew failed to parse: {:?}",
            parsed.errors
        );
        if parsed.errors.is_empty() {
            let items: Vec<_> = parsed.program.items.into_iter().collect();
            self.register_stdlib_hew_items("closable", &items);
        }
    }

    /// Register the built-in `MonitorRef` surface so `monitor()` can return a
    /// Hew value type with `close()` / `Drop` behaviour in inline tests that
    /// do not have a stdlib search path.
    fn register_builtin_monitor_ref_surface(&mut self) {
        let identity = "module:std::link_monitor";
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
            let items: Vec<_> = parsed.program.items.into_iter().collect();
            self.register_stdlib_hew_items("link_monitor", &items);
            self.consume_receiver_methods
                .insert("MonitorRef::close".to_string());
            self.registry.register_drop_type("MonitorRef".to_string());
        }
    }

    /// Register the built-in `CrashInfo` / `CrashAction` surface so
    /// `#[on(crash)]` lifecycle hooks can name them in their signatures
    /// without `import std::failure;`.  Inline tests (no stdlib search
    /// path) rely on this; on-disk programs reach the same types via
    /// the module graph.
    fn register_builtin_failure_surface(&mut self) {
        let identity = "module:std::failure";
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
            self.register_stdlib_hew_items("failure", &items);
        }
    }

    /// Register the built-in `LookupError` enum so `Node::lookup<T>` callers
    /// can pattern-match `Err(LookupError::NotFound)` without an explicit
    /// import (inline-test parity with `LinkError` / `CrashAction`).
    fn register_builtin_lookup_error_surface(&mut self) {
        let identity = "module:std::lookup_error";
        if self.registered_stdlib_hew_sources.contains(identity) {
            return;
        }
        self.registered_stdlib_hew_sources
            .insert(identity.to_string());
        let parsed = hew_parser::parse(LOOKUP_ERROR_HEW);
        debug_assert!(
            parsed.errors.is_empty(),
            "std/builtins.hew::LookupError failed to parse: {:?}",
            parsed.errors
        );
        if parsed.errors.is_empty() {
            let items: Vec<_> = parsed.program.items.into_iter().collect();
            self.register_stdlib_hew_items("lookup_error", &items);
        }
    }

    pub(super) fn register_builtin_fn(&mut self, name: &str, params: Vec<Ty>, return_type: Ty) {
        self.register_builtin_sig(
            name,
            FnSig {
                params,
                return_type,
                ..FnSig::default()
            },
        );
    }

    pub(super) fn register_builtin_fn_with_bounds(
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

    fn register_builtin_sig(&mut self, name: &str, sig: FnSig) {
        if name.contains('.') {
            self.module_fn_exports.insert(name.to_string());
        }
        self.fn_sigs.insert(name.to_string(), sig);
    }

    fn resolve_registered_annotation_ty(
        &mut self,
        type_expr: &Spanned<TypeExpr>,
        hole_vars: &mut Vec<TypeVar>,
    ) -> Ty {
        let ty = self.resolve_type_expr_tracking_holes(type_expr, hole_vars);
        self.validate_concrete_collection_types(&ty, &type_expr.1);
        ty
    }

    fn resolve_registered_annotation_ty_no_holes(&mut self, type_expr: &Spanned<TypeExpr>) -> Ty {
        let mut hole_vars = Vec::new();
        self.resolve_registered_annotation_ty(type_expr, &mut hole_vars)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "type registration handles all root item variants in one place"
    )]
    /// Pass 1: Collect type definitions
    pub(super) fn collect_types(&mut self, program: &Program) {
        // Pre-register TypeDecls from non-root module_graph modules into
        // `type_defs` so that:
        //   (a) `locally_non_generic` in `resolve_type_expr` can suppress
        //       fresh-var injection for opaque handle types (Sender, Receiver)
        //   (b) non-root module body-checking can access struct fields and
        //       enum variants of types defined within those modules
        //
        // Uses `pre_register_type_decl` which populates `type_defs` with
        // correct field/variant data but skips `type_def_spans` (so the
        // import path's `register_type_namespace_name` succeeds) and skips
        // trait-registry / wire-method side effects (those are handled by
        // the import path's full `register_type_decl` for pub types, and
        // are not needed for internal non-pub types). `RcFree` is the
        // bounded exception because collection admissibility during non-root
        // body checking depends on that structural marker.
        if let Some(ref mg) = program.module_graph {
            for mod_id in &mg.topo_order {
                if *mod_id == mg.root {
                    continue;
                }
                if let Some(module) = mg.modules.get(mod_id) {
                    let module_name = mod_id.path.join(".");
                    self.current_module = Some(module_name.clone());
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
                    for (item, _) in &module.items {
                        match item {
                            Item::TypeDecl(td) => {
                                self.pre_register_type_decl(td);
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
                            Item::Machine(md) if !self.type_defs.contains_key(&md.name) => {
                                self.register_machine_decl(md);
                            }
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

        // Process root module items (full registration with namespace dedup).
        for (item, span) in &program.items {
            match item {
                Item::TypeDecl(td) => {
                    if !self.register_type_namespace_name(&td.name, span) {
                        continue;
                    }
                    self.register_type_decl(td);
                    self.local_type_defs.insert(td.name.clone());
                    self.source_type_defs.insert(td.name.clone());
                }
                Item::Actor(ad) => {
                    if !self.register_type_namespace_name(&ad.name, span) {
                        continue;
                    }
                    self.register_actor_decl(ad);
                    self.source_type_defs.insert(ad.name.clone());
                }
                Item::Wire(wd) => {
                    if !self.register_type_namespace_name(&wd.name, span) {
                        continue;
                    }
                    self.register_wire_decl(wd);
                    self.source_type_defs.insert(wd.name.clone());
                }
                Item::TypeAlias(ta) => {
                    if !self.register_type_namespace_name(&ta.name, span) {
                        continue;
                    }
                    let mut hole_vars = Vec::new();
                    let resolved = self.resolve_type_expr_tracking_holes(&ta.ty, &mut hole_vars);
                    self.type_aliases.insert(ta.name.clone(), resolved);
                    self.record_type_def_inference_holes(&ta.name, hole_vars);
                    self.source_type_defs.insert(ta.name.clone());
                }
                Item::Trait(td) => {
                    if !self.register_type_namespace_name(&td.name, span) {
                        continue;
                    }
                    let mut trait_errors = Vec::new();
                    let info = Self::trait_info_from_decl_with_diagnostics(td, &mut trait_errors);
                    self.errors.extend(trait_errors);
                    self.trait_defs.insert(td.name.clone(), info);
                    self.local_trait_defs.insert(td.name.clone());
                    // Record super-trait relationships
                    if let Some(supers) = &td.super_traits {
                        let super_names: Vec<String> =
                            supers.iter().map(|s| s.name.clone()).collect();
                        self.trait_super.insert(td.name.clone(), super_names);
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
                    // Partition children by kind in source order. Slot index for each
                    // child is its 0-based position within its own partition, matching
                    // the runtime layout (children[] for static, pool_slots[] for pool).
                    let mut statics = Vec::new();
                    let mut pools = Vec::new();
                    for c in &sd.children {
                        let entry = (c.name.clone(), c.actor_type.clone());
                        if c.is_pool {
                            pools.push(entry);
                        } else {
                            statics.push(entry);
                        }
                    }
                    self.supervisor_children.insert(
                        sd.name.clone(),
                        crate::check::types::SupervisorChildren { statics, pools },
                    );
                }
                Item::Machine(md) => {
                    if !self.register_machine_type_namespace_names(&md.name, span) {
                        continue;
                    }
                    self.register_machine_decl(md);
                    self.local_type_defs.insert(md.name.clone());
                    self.source_type_defs.insert(md.name.clone());
                }
                Item::Record(rd) => {
                    if !self.register_type_namespace_name(&rd.name, span) {
                        continue;
                    }
                    self.register_record_decl(rd);
                    self.local_type_defs.insert(rd.name.clone());
                    self.source_type_defs.insert(rd.name.clone());
                }
                Item::Import(_)
                | Item::Const(_)
                | Item::Impl(_)
                | Item::Function(_)
                | Item::ExternBlock(_) => {}
            }
        }
    }

    /// Populate `type_defs` with a full `TypeDef` for a non-root module's
    /// `TypeDecl`, including resolved fields and variant constructors.
    ///
    /// Deliberately skips:
    ///   - `type_def_spans` — the import path handles namespace dedup
    ///   - `TraitRegistry` registration — the import path (or C module
    ///     registry) handles trait derivation for exported types
    ///   - Wire-method registration — only relevant for the import surface
    ///
    /// It still registers enum-constructor `fn_sigs` so non-root module body
    /// checking can construct local values. The import path's later
    /// `register_type_decl` call overwrites those signatures for `pub` types
    /// with the fully side-effected version.
    #[expect(clippy::too_many_lines, reason = "type resolution requires many cases")]
    fn pre_register_type_decl(&mut self, td: &TypeDecl) {
        if self.type_defs.contains_key(&td.name) {
            return;
        }
        let kind = match td.kind {
            TypeDeclKind::Struct => TypeDefKind::Struct,
            TypeDeclKind::Enum => TypeDefKind::Enum,
        };
        let type_param_names: Vec<String> = td.type_params.as_ref().map_or(vec![], |params| {
            params.iter().map(|p| p.name.clone()).collect()
        });

        // Reject duplicate type parameter names — same check as `register_type_decl`.
        {
            let mut seen: std::collections::HashSet<&str> = std::collections::HashSet::new();
            for name in &type_param_names {
                if !seen.insert(name.as_str()) {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::DuplicateDefinition,
                        0..0,
                        format!(
                            "type parameter `{name}` is defined more than once in `{}`",
                            td.name
                        ),
                    ));
                }
            }
        }

        let mut fields = HashMap::new();
        let mut variants = HashMap::new();
        let mut hole_vars = Vec::new();
        let enum_return_args: Vec<Ty> = type_param_names
            .iter()
            .map(|name| Ty::Named {
                builtin: None,
                name: name.clone(),
                args: vec![],
            })
            .collect();

        for item in &td.body {
            match item {
                TypeBodyItem::Field { name, ty, .. } => {
                    let field_ty = self.resolve_registered_annotation_ty(ty, &mut hole_vars);
                    fields.insert(name.clone(), field_ty);
                }
                TypeBodyItem::Variant(variant) => {
                    let return_type = Ty::Named {
                        builtin: None,
                        name: td.name.clone(),
                        args: enum_return_args.clone(),
                    };
                    match &variant.kind {
                        VariantKind::Unit => {
                            variants.insert(variant.name.clone(), VariantDef::Unit);
                            // Register variant constructor so body-checking can construct values
                            self.fn_sigs.insert(
                                variant.name.clone(),
                                FnSig {
                                    type_params: type_param_names.clone(),
                                    return_type,
                                    ..FnSig::default()
                                },
                            );
                        }
                        VariantKind::Tuple(tfields) => {
                            let variant_tys: Vec<Ty> = tfields
                                .iter()
                                .map(|field| {
                                    self.resolve_registered_annotation_ty(field, &mut hole_vars)
                                })
                                .collect();
                            variants.insert(
                                variant.name.clone(),
                                VariantDef::Tuple(variant_tys.clone()),
                            );
                            self.fn_sigs.insert(
                                variant.name.clone(),
                                FnSig {
                                    type_params: type_param_names.clone(),
                                    params: variant_tys,
                                    return_type,
                                    ..FnSig::default()
                                },
                            );
                        }
                        VariantKind::Struct(sfields) => {
                            let variant_fields: Vec<(String, Ty)> = sfields
                                .iter()
                                .map(|(name, field)| {
                                    (
                                        name.clone(),
                                        self.resolve_registered_annotation_ty(
                                            field,
                                            &mut hole_vars,
                                        ),
                                    )
                                })
                                .collect();
                            variants
                                .insert(variant.name.clone(), VariantDef::Struct(variant_fields));
                        }
                    }
                }
                TypeBodyItem::Method(_) => {}
            }
        }

        let type_def = TypeDef {
            kind,
            name: td.name.clone(),
            type_params: type_param_names,
            fields,
            variants,
            methods: HashMap::new(),
            doc_comment: td.doc_comment.clone(),
            is_indirect: td.is_indirect,
        };
        self.register_rcfree_members_for_type(&td.name, &type_def);
        self.type_defs.insert(td.name.clone(), type_def);
        self.record_type_def_inference_holes(&td.name, hole_vars);
        self.handle_bearing_dirty = true;
    }

    pub(super) fn register_type_namespace_name(&mut self, name: &str, span: &Span) -> bool {
        if let Some(prev_span) = self.type_def_spans.get(name).cloned() {
            self.report_duplicate_type_namespace_name(name, span, prev_span);
            return false;
        }

        self.type_def_spans.insert(name.to_string(), span.clone());
        true
    }

    pub(super) fn report_duplicate_type_namespace_name(
        &mut self,
        name: &str,
        span: &Span,
        prev_span: Span,
    ) {
        self.errors.push(TypeError::duplicate_definition(
            span.clone(),
            name,
            prev_span,
        ));
    }

    pub(super) fn register_machine_type_namespace_names(
        &mut self,
        machine_name: &str,
        span: &Span,
    ) -> bool {
        if let Some(prev_span) = self.type_def_spans.get(machine_name).cloned() {
            self.report_duplicate_type_namespace_name(machine_name, span, prev_span);
            return false;
        }

        let event_type_name = format!("{machine_name}Event");
        if let Some(prev_span) = self.type_def_spans.get(&event_type_name).cloned() {
            self.report_duplicate_type_namespace_name(&event_type_name, span, prev_span);
            return false;
        }

        self.type_def_spans
            .insert(machine_name.to_string(), span.clone());
        self.type_def_spans.insert(event_type_name, span.clone());
        true
    }

    #[expect(clippy::too_many_lines, reason = "type resolution requires many cases")]
    pub(super) fn register_type_decl(&mut self, td: &TypeDecl) {
        let kind = match td.kind {
            TypeDeclKind::Struct => TypeDefKind::Struct,
            TypeDeclKind::Enum => TypeDefKind::Enum,
        };

        let mut fields = HashMap::new();
        let mut variants = HashMap::new();
        let mut hole_vars = Vec::new();
        let type_param_names: Vec<String> = td.type_params.as_ref().map_or(vec![], |params| {
            params.iter().map(|p| p.name.clone()).collect()
        });

        // Reject duplicate type parameter names within the same declaration.
        // The parser cannot catch this because `parse_type_params` has no
        // seen-name accumulator; the checker is the authoritative gatekeeper.
        {
            let mut seen: std::collections::HashSet<&str> = std::collections::HashSet::new();
            for name in &type_param_names {
                if !seen.insert(name.as_str()) {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::DuplicateDefinition,
                        0..0,
                        format!(
                            "type parameter `{name}` is defined more than once in `{}`",
                            td.name
                        ),
                    ));
                }
            }
        }

        let type_param_bounds =
            self.collect_type_param_bounds(td.type_params.as_ref(), td.where_clause.as_ref());
        let enum_return_args: Vec<Ty> = type_param_names
            .iter()
            .map(|name| Ty::Named {
                builtin: None,
                name: name.clone(),
                args: vec![],
            })
            .collect();

        for item in &td.body {
            match item {
                TypeBodyItem::Field { name, ty, .. } => {
                    let field_ty = self.resolve_registered_annotation_ty(ty, &mut hole_vars);
                    fields.insert(name.clone(), field_ty);
                }
                TypeBodyItem::Variant(variant) => {
                    let return_type = Ty::Named {
                        builtin: None,
                        name: td.name.clone(),
                        args: enum_return_args.clone(),
                    };
                    match &variant.kind {
                        VariantKind::Unit => {
                            variants.insert(variant.name.clone(), VariantDef::Unit);
                            self.fn_sigs.insert(
                                variant.name.clone(),
                                FnSig {
                                    type_params: type_param_names.clone(),
                                    type_param_bounds: type_param_bounds.clone(),
                                    return_type,
                                    ..FnSig::default()
                                },
                            );
                        }
                        VariantKind::Tuple(fields) => {
                            let variant_tys: Vec<Ty> = fields
                                .iter()
                                .map(|field| {
                                    self.resolve_registered_annotation_ty(field, &mut hole_vars)
                                })
                                .collect();
                            variants.insert(
                                variant.name.clone(),
                                VariantDef::Tuple(variant_tys.clone()),
                            );

                            // Register variant constructor as function
                            self.fn_sigs.insert(
                                variant.name.clone(),
                                FnSig {
                                    type_params: type_param_names.clone(),
                                    type_param_bounds: type_param_bounds.clone(),
                                    params: variant_tys,
                                    return_type,
                                    ..FnSig::default()
                                },
                            );
                        }
                        VariantKind::Struct(fields) => {
                            let variant_fields: Vec<(String, Ty)> = fields
                                .iter()
                                .map(|(name, field)| {
                                    (
                                        name.clone(),
                                        self.resolve_registered_annotation_ty(
                                            field,
                                            &mut hole_vars,
                                        ),
                                    )
                                })
                                .collect();
                            variants
                                .insert(variant.name.clone(), VariantDef::Struct(variant_fields));
                        }
                    }
                }
                TypeBodyItem::Method(_) => {
                    // Methods are handled in pass 2
                }
            }
        }

        let type_def = TypeDef {
            kind,
            name: td.name.clone(),
            type_params: type_param_names.clone(),
            fields,
            variants,
            methods: HashMap::new(),
            doc_comment: td.doc_comment.clone(),
            is_indirect: td.is_indirect,
        };

        // Register with trait registry for Send/Frozen derivation
        let field_types: Vec<_> = type_def.fields.values().cloned().collect();
        let all_fields_encodable = td.wire.is_none()
            && kind == TypeDefKind::Struct
            && field_types
                .iter()
                .all(|f| self.registry.implements_marker(f, MarkerTrait::Encode));

        self.registry.register_type(td.name.clone(), field_types);
        self.register_rcfree_members_for_type(&td.name, &type_def);

        self.type_defs.insert(td.name.clone(), type_def);
        self.record_type_def_inference_holes(&td.name, hole_vars);
        self.handle_bearing_dirty = true;

        // If this is a wire type, register encode/decode/to_json/from_json/to_yaml/from_yaml methods
        if let Some(ref wire) = td.wire {
            self.register_wire_methods(&td.name);
            self.validate_wire_version_constraints(&td.name, wire);
        }

        // For non-wire struct types: if all fields are Encode, register
        // serialization methods (to_json, from_json, to_yaml, from_yaml, to_toml, from_toml)
        if all_fields_encodable {
            self.register_encode_methods(&td.name);
        }
    }

    /// Register a `record` declaration into the type table.
    ///
    /// Named-field form: populates `type_defs.fields` so that
    /// `check_struct_init` and `check_field_access` resolve field types by
    /// name.
    ///
    /// Tuple-positional form: registers a constructor `fn_sig` so that
    /// `R(1, 2)` resolves as a function call returning `Ty::Named { name: R
    /// }`.  The `fields` map is left empty — this deliberately prevents
    /// `.0`/`.1` index-style access (A-D2: positional destructuring only).
    ///
    /// In both cases `type_defs` receives a `TypeDef` with
    /// `kind = TypeDefKind::Record` so the field-write rejection in
    /// `statements.rs` can identify record types.
    pub(super) fn register_record_decl(&mut self, rd: &RecordDecl) {
        let type_param_names: Vec<String> = rd.type_params.as_ref().map_or(vec![], |params| {
            params.iter().map(|p| p.name.clone()).collect()
        });
        let type_param_bounds =
            self.collect_type_param_bounds(rd.type_params.as_ref(), rd.where_clause.as_ref());

        // Build the return type for constructors: `R` or `R<T1, T2, …>`
        let enum_return_args: Vec<Ty> = type_param_names
            .iter()
            .map(|name| Ty::Named {
                builtin: None,
                name: name.clone(),
                args: vec![],
            })
            .collect();
        let return_type = Ty::Named {
            builtin: None,
            name: rd.name.clone(),
            args: enum_return_args,
        };

        let mut fields: HashMap<String, Ty> = HashMap::new();
        let mut hole_vars = Vec::new();
        // Positional field types for tuple records, collected for marker
        // derivation (A-4). Named-record fields come from `type_def.fields`.
        let mut tuple_field_types: Vec<Ty> = Vec::new();

        match &rd.kind {
            RecordKind::Named(record_fields) => {
                for rf in record_fields {
                    let field_ty = self.resolve_registered_annotation_ty(&rf.ty, &mut hole_vars);
                    fields.insert(rf.name.clone(), field_ty);
                }
            }
            RecordKind::Tuple(positional_types) => {
                // Resolve each positional field type for the constructor signature.
                let param_tys: Vec<Ty> = positional_types
                    .iter()
                    .map(|te| self.resolve_registered_annotation_ty(te, &mut hole_vars))
                    .collect();

                // Capture positional types for marker registration before moving
                // param_tys into fn_sigs. The `fields` map intentionally stays
                // empty — `.0`/`.1` access is not permitted on tuple records (A-D2).
                tuple_field_types.clone_from(&param_tys);

                // Register a constructor function so `R(1, 2)` resolves via
                // `check_call`.  The `fields` map intentionally stays empty —
                // `.0`/`.1` access is not permitted on tuple records (A-D2).
                self.fn_sigs.insert(
                    rd.name.clone(),
                    FnSig {
                        type_params: type_param_names.clone(),
                        type_param_bounds: type_param_bounds.clone(),
                        params: param_tys,
                        return_type: return_type.clone(),
                        ..FnSig::default()
                    },
                );
            }
        }

        let type_def = TypeDef {
            kind: TypeDefKind::Record,
            name: rd.name.clone(),
            type_params: type_param_names.clone(),
            fields,
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: rd.doc_comment.clone(),
            is_indirect: false,
        };

        // Register all field types for marker derivation (Eq/Hash/Send/Frozen/
        // Clone/Copy). Named-field records use type_def.fields; tuple records
        // use the positional types captured above (type_def.fields is empty for
        // tuple records by design — A-D2).
        let field_types: Vec<Ty> = if tuple_field_types.is_empty() {
            type_def.fields.values().cloned().collect()
        } else {
            tuple_field_types
        };
        self.registry.register_type(rd.name.clone(), field_types);
        // Mark this as a record type so implements_marker applies the correct
        // value-type semantics (Resource always false; all other markers field-driven).
        self.registry.register_record_type(rd.name.clone());
        self.register_rcfree_members_for_type(&rd.name, &type_def);

        self.type_defs.insert(rd.name.clone(), type_def);
        self.record_type_def_inference_holes(&rd.name, hole_vars);
        self.handle_bearing_dirty = true;
    }

    /// Register codec methods for a wire type.
    ///
    /// - Wire structs expose binary + JSON/YAML helpers.
    /// - Wire enums expose JSON/YAML helpers.
    pub(super) fn register_wire_methods(&mut self, type_name: &str) {
        let self_ty = Ty::Named {
            builtin: None,
            name: type_name.to_string(),
            args: vec![],
        };
        let bytes_ty = Ty::Bytes;

        let Some(type_def) = self.type_defs.get(type_name) else {
            return;
        };
        let is_wire_struct = type_def.kind == TypeDefKind::Struct;
        let is_unit_wire_enum = type_def.kind == TypeDefKind::Enum
            && type_def
                .variants
                .values()
                .all(|variant| matches!(variant, VariantDef::Unit));
        let is_payload_wire_enum = type_def.kind == TypeDefKind::Enum
            && type_def
                .variants
                .values()
                .any(|variant| !matches!(variant, VariantDef::Unit));
        let is_serial_wire_enum = is_unit_wire_enum || is_payload_wire_enum;

        let instance_methods = if is_wire_struct {
            vec![
                ("encode", vec![], bytes_ty.clone()),
                ("to_json", vec![], Ty::String),
                ("to_yaml", vec![], Ty::String),
            ]
        } else if is_serial_wire_enum {
            vec![
                ("to_json", vec![], Ty::String),
                ("to_yaml", vec![], Ty::String),
            ]
        } else {
            vec![]
        };

        if let Some(type_def) = self.type_defs.get_mut(type_name) {
            for (method_name, params, return_type) in instance_methods {
                type_def.methods.insert(
                    method_name.to_string(),
                    FnSig {
                        params,
                        return_type,
                        is_pure: true,
                        ..FnSig::default()
                    },
                );
            }
        }

        let static_methods = if is_wire_struct {
            vec![
                ("decode", vec![bytes_ty], self_ty.clone()),
                ("from_json", vec![Ty::String], self_ty.clone()),
                ("from_yaml", vec![Ty::String], self_ty),
            ]
        } else if is_serial_wire_enum {
            vec![
                ("from_json", vec![Ty::String], self_ty.clone()),
                ("from_yaml", vec![Ty::String], self_ty),
            ]
        } else {
            vec![]
        };

        for (method_name, params, return_type) in static_methods {
            let qualified_name = format!("{type_name}.{method_name}");
            self.fn_sigs.insert(
                qualified_name,
                FnSig {
                    params,
                    return_type,
                    is_pure: true,
                    ..FnSig::default()
                },
            );
        }
    }

    /// Register serialization methods for a struct type that implements `Encode`.
    ///
    /// Adds `to_json`, `to_yaml`, `to_toml` instance methods and
    /// `from_json`, `from_yaml`, `from_toml` static methods.
    ///
    /// The `from_*` static methods return `Result<Self, String>`.  Errors are
    /// returned for: (1) top-level parse failure, (2) missing required field,
    /// (3) a string field that is not a string at runtime, and (4) a scalar
    /// (int/bool/float) field whose runtime type code does not match the
    /// expected kind for the format.
    pub(super) fn register_encode_methods(&mut self, type_name: &str) {
        let self_ty = Ty::Named {
            builtin: None,
            name: type_name.to_string(),
            args: vec![],
        };

        // Instance methods: to_json(self) -> String, to_yaml(self) -> String, to_toml(self) -> String
        let instance_methods = [
            ("to_json", Ty::String),
            ("to_yaml", Ty::String),
            ("to_toml", Ty::String),
        ];

        if let Some(type_def) = self.type_defs.get_mut(type_name) {
            for (method_name, return_type) in instance_methods {
                type_def.methods.insert(
                    method_name.to_string(),
                    FnSig {
                        return_type,
                        is_pure: true,
                        ..FnSig::default()
                    },
                );
            }
        }

        // Static methods: TypeName.from_json(String) -> Result<Self, String>, etc.
        // Returns Result so callers can distinguish valid input from a malformed document
        // without a runtime panic.
        let result_ty = Ty::result(self_ty.clone(), Ty::String);
        let static_methods = [
            ("from_json", vec![Ty::String], result_ty.clone()),
            ("from_yaml", vec![Ty::String], result_ty.clone()),
            ("from_toml", vec![Ty::String], result_ty),
        ];

        for (method_name, params, return_type) in static_methods {
            let qualified_name = format!("{type_name}.{method_name}");
            self.fn_sigs.insert(
                qualified_name,
                FnSig {
                    params,
                    return_type,
                    is_pure: true,
                    ..FnSig::default()
                },
            );
        }
    }

    /// Validate version constraints on a wire type.
    pub(super) fn validate_wire_version_constraints(
        &mut self,
        type_name: &str,
        wire: &hew_parser::ast::WireMetadata,
    ) {
        use crate::error::Severity;

        let decl_span = self.type_def_spans.get(type_name).cloned().unwrap_or(0..0);
        let version = wire.version;
        let min_version = wire.min_version;

        // min_version cannot exceed version
        if let (Some(min_v), Some(v)) = (min_version, version) {
            if min_v > v {
                self.errors.push(TypeError {
                    severity: Severity::Error,
                    kind: TypeErrorKind::InvalidOperation,
                    span: decl_span.clone(),
                    message: format!(
                        "wire `{type_name}`: min_version ({min_v}) cannot exceed version ({v})"
                    ),
                    notes: vec![],
                    suggestions: vec![],
                    source_module: self.current_module.clone(),
                });
            }
        }

        // Per-field `since` constraints
        for fm in &wire.field_meta {
            if let Some(since) = fm.since {
                if version.is_none() {
                    // since has no effect without a schema version
                    self.warnings.push(TypeError {
                        severity: Severity::Warning,
                        kind: TypeErrorKind::StyleSuggestion,
                        span: decl_span.clone(),
                        message: format!(
                            "wire `{type_name}.{}`: field has `since {since}` but struct \
                             has no #[wire(version = N)] attribute",
                            fm.field_name
                        ),
                        notes: vec![],
                        suggestions: vec![],
                        source_module: self.current_module.clone(),
                    });
                }

                // since cannot exceed version
                if let Some(v) = version {
                    if since > v {
                        self.errors.push(TypeError {
                            severity: Severity::Error,
                            kind: TypeErrorKind::InvalidOperation,
                            span: decl_span.clone(),
                            message: format!(
                                "wire `{type_name}.{}`: since ({since}) cannot exceed \
                                 schema version ({v})",
                                fm.field_name
                            ),
                            notes: vec![],
                            suggestions: vec![],
                            source_module: self.current_module.clone(),
                        });
                    }
                }
            }

            // Warn if version > 1 and a non-optional field lacks `since`
            if let Some(v) = version {
                if v > 1 && fm.since.is_none() && !fm.is_optional {
                    self.warnings.push(TypeError {
                        severity: Severity::Warning,
                        kind: TypeErrorKind::StyleSuggestion,
                        span: decl_span.clone(),
                        message: format!(
                            "wire `{type_name}.{}`: non-optional field has no `since` annotation \
                             (schema version is {v})",
                            fm.field_name
                        ),
                        notes: vec![],
                        suggestions: vec![],
                        source_module: self.current_module.clone(),
                    });
                }
            }
        }
    }

    /// Register a machine declaration as a type definition with variants and methods.
    #[expect(
        clippy::too_many_lines,
        reason = "machine registration covers states, events, and generated methods"
    )]
    pub(super) fn register_machine_decl(&mut self, md: &MachineDecl) {
        // Build the machine's self-type: `Machine` or `Machine<T, U, …>`.
        // MachineDecl.type_params is Vec<TypeParam> — we extract bare names
        // here for the self-type and collect declared trait bounds into a
        // side table consulted at use sites (struct-state brace init) and
        // mirrored onto unit-state constructor FnSigs for the call path.
        let type_param_names: Vec<String> = md.type_params.iter().map(|p| p.name.clone()).collect();
        let type_param_bounds = self.collect_type_param_bounds(Some(&md.type_params), None);
        if !type_param_bounds.is_empty() {
            self.machine_type_param_bounds
                .insert(md.name.clone(), type_param_bounds.clone());
        }
        let machine_generic_args: Vec<Ty> = type_param_names
            .iter()
            .map(|name| Ty::Named {
                builtin: None,
                name: name.clone(),
                args: vec![],
            })
            .collect();
        let machine_ty = Ty::Named {
            builtin: None,
            name: md.name.clone(),
            args: machine_generic_args.clone(),
        };

        let event_type_name = format!("{}Event", md.name);
        let event_ty = Ty::Named {
            builtin: None,
            name: event_type_name.clone(),
            args: machine_generic_args.clone(),
        };

        // Build state variants
        let mut variants = HashMap::new();
        let mut machine_hole_vars = Vec::new();
        for state in &md.states {
            if state.fields.is_empty() {
                variants.insert(state.name.clone(), VariantDef::Unit);
                // Register unit state constructor as a function. For generic
                // machines (e.g. `machine Worker<T>`), the constructor returns
                // `Worker<T>` so callers can instantiate with concrete args.
                self.fn_sigs.insert(
                    state.name.clone(),
                    FnSig {
                        type_params: type_param_names.clone(),
                        type_param_bounds: type_param_bounds.clone(),
                        return_type: machine_ty.clone(),
                        is_pure: true,
                        ..FnSig::default()
                    },
                );
            } else {
                let variant_fields: Vec<(String, Ty)> = state
                    .fields
                    .iter()
                    .map(|(name, spanned_te)| {
                        (
                            name.clone(),
                            self.resolve_registered_annotation_ty(
                                spanned_te,
                                &mut machine_hole_vars,
                            ),
                        )
                    })
                    .collect();
                variants.insert(state.name.clone(), VariantDef::Struct(variant_fields));
            }
        }

        let type_def = TypeDef {
            kind: TypeDefKind::Machine,
            name: md.name.clone(),
            type_params: type_param_names.clone(),
            fields: HashMap::new(),
            variants,
            methods: HashMap::new(),
            doc_comment: None,
            is_indirect: false,
        };

        // Register field types for Send/Frozen derivation
        let mut all_field_types = Vec::new();
        for state in &md.states {
            for (_, spanned_te) in &state.fields {
                all_field_types.push(self.resolve_type_expr(spanned_te));
            }
        }
        self.registry
            .register_type(md.name.clone(), all_field_types);
        self.register_rcfree_members_for_type(&md.name, &type_def);

        self.type_defs.insert(md.name.clone(), type_def);
        self.record_type_def_inference_holes(&md.name, machine_hole_vars);
        self.known_types.insert(md.name.clone());

        // Register the generated event companion enum
        let mut event_variants = HashMap::new();
        let mut event_hole_vars = Vec::new();
        for event in &md.events {
            if event.fields.is_empty() {
                event_variants.insert(event.name.clone(), VariantDef::Unit);
            } else {
                let variant_fields: Vec<(String, Ty)> = event
                    .fields
                    .iter()
                    .map(|(name, spanned_te)| {
                        (
                            name.clone(),
                            self.resolve_registered_annotation_ty(spanned_te, &mut event_hole_vars),
                        )
                    })
                    .collect();
                event_variants.insert(event.name.clone(), VariantDef::Struct(variant_fields));
            }
        }
        let event_type_def = TypeDef {
            kind: TypeDefKind::Enum,
            name: event_type_name.clone(),
            type_params: type_param_names.clone(),
            fields: HashMap::new(),
            variants: event_variants,
            methods: HashMap::new(),
            doc_comment: None,
            is_indirect: false,
        };
        self.register_rcfree_members_for_type(&event_type_name, &event_type_def);
        self.type_defs
            .insert(event_type_name.clone(), event_type_def);
        self.record_type_def_inference_holes(&event_type_name, event_hole_vars);
        self.known_types.insert(event_type_name);

        // Register the step() method on the machine type
        if let Some(td) = self.type_defs.get_mut(&md.name) {
            td.methods.insert(
                "step".to_string(),
                FnSig {
                    param_names: vec!["event".to_string()],
                    params: vec![event_ty],
                    ..FnSig::default()
                },
            );
            // Register state_name() method
            td.methods.insert(
                "state_name".to_string(),
                FnSig {
                    return_type: Ty::String,
                    is_pure: true,
                    ..FnSig::default()
                },
            );
        }
    }

    fn report_machine_transition_forbidden_exprs(
        &mut self,
        machine_name: &str,
        transition: &hew_parser::ast::MachineTransition,
    ) -> bool {
        let mut hits = Vec::new();
        Self::collect_machine_transition_forbidden_exprs(
            &transition.body.0,
            &transition.body.1,
            &mut hits,
        );
        for (kind, span, label) in &hits {
            let message = match kind {
                TypeErrorKind::GenBlockInMachineTransition => format!(
                    "E_GENBLOCK_IN_MACHINE_TRANSITION: `gen {{ }}` blocks are forbidden inside \
                     machine `{machine_name}` transition `{}`: {} -> {}; transition bodies \
                     must be pure and cannot suspend",
                    transition.event_name, transition.source_state, transition.target_state
                ),
                TypeErrorKind::AwaitInMachineTransition => format!(
                    "E_AWAIT_IN_MACHINE_TRANSITION: `{label}` is forbidden inside machine \
                     `{machine_name}` transition `{}`: {} -> {}; transition bodies must be pure \
                     and cannot suspend",
                    transition.event_name, transition.source_state, transition.target_state
                ),
                _ => unreachable!("machine transition purity scanner only emits its own kinds"),
            };
            self.report_error(kind.clone(), span, message);
        }
        !hits.is_empty()
    }

    fn collect_machine_transition_forbidden_block(
        block: &Block,
        hits: &mut Vec<(TypeErrorKind, Span, &'static str)>,
    ) {
        for (stmt, span) in &block.stmts {
            Self::collect_machine_transition_forbidden_stmt(stmt, span, hits);
        }
        if let Some(expr) = &block.trailing_expr {
            Self::collect_machine_transition_forbidden_exprs(&expr.0, &expr.1, hits);
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "fail-closed transition purity scanner must cover every AST expression shape"
    )]
    fn collect_machine_transition_forbidden_stmt(
        stmt: &Stmt,
        span: &Span,
        hits: &mut Vec<(TypeErrorKind, Span, &'static str)>,
    ) {
        match stmt {
            Stmt::Let { value, .. }
            | Stmt::Var { value, .. }
            | Stmt::Break { value, .. }
            | Stmt::Return(value) => {
                if let Some((expr, expr_span)) = value {
                    Self::collect_machine_transition_forbidden_exprs(expr, expr_span, hits);
                }
            }
            Stmt::Assign { target, value, .. } => {
                Self::collect_machine_transition_forbidden_exprs(&target.0, &target.1, hits);
                Self::collect_machine_transition_forbidden_exprs(&value.0, &value.1, hits);
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                Self::collect_machine_transition_forbidden_exprs(&condition.0, &condition.1, hits);
                Self::collect_machine_transition_forbidden_block(then_block, hits);
                if let Some(else_block) = else_block {
                    if let Some(if_stmt) = &else_block.if_stmt {
                        Self::collect_machine_transition_forbidden_stmt(
                            &if_stmt.0, &if_stmt.1, hits,
                        );
                    }
                    if let Some(block) = &else_block.block {
                        Self::collect_machine_transition_forbidden_block(block, hits);
                    }
                }
            }
            Stmt::IfLet {
                expr,
                body,
                else_body,
                ..
            } => {
                Self::collect_machine_transition_forbidden_exprs(&expr.0, &expr.1, hits);
                Self::collect_machine_transition_forbidden_block(body, hits);
                if let Some(block) = else_body {
                    Self::collect_machine_transition_forbidden_block(block, hits);
                }
            }
            Stmt::Match { scrutinee, arms } => {
                Self::collect_machine_transition_forbidden_exprs(&scrutinee.0, &scrutinee.1, hits);
                for arm in arms {
                    if let Some((guard, guard_span)) = &arm.guard {
                        Self::collect_machine_transition_forbidden_exprs(guard, guard_span, hits);
                    }
                    Self::collect_machine_transition_forbidden_exprs(
                        &arm.body.0,
                        &arm.body.1,
                        hits,
                    );
                }
            }
            Stmt::Loop { body, .. } | Stmt::While { body, .. } => {
                Self::collect_machine_transition_forbidden_block(body, hits);
            }
            Stmt::For {
                is_await,
                iterable,
                body,
                ..
            } => {
                if *is_await {
                    hits.push((
                        TypeErrorKind::AwaitInMachineTransition,
                        span.clone(),
                        "for await",
                    ));
                }
                Self::collect_machine_transition_forbidden_exprs(&iterable.0, &iterable.1, hits);
                Self::collect_machine_transition_forbidden_block(body, hits);
            }
            Stmt::WhileLet { expr, body, .. } => {
                Self::collect_machine_transition_forbidden_exprs(&expr.0, &expr.1, hits);
                Self::collect_machine_transition_forbidden_block(body, hits);
            }
            Stmt::Defer(expr) => {
                Self::collect_machine_transition_forbidden_exprs(&expr.0, &expr.1, hits);
            }
            Stmt::Expression(expr) => {
                Self::collect_machine_transition_forbidden_exprs(&expr.0, &expr.1, hits);
            }
            Stmt::Continue { .. } => {}
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "fail-closed transition purity scanner must cover every AST expression shape"
    )]
    fn collect_machine_transition_forbidden_exprs(
        expr: &Expr,
        span: &Span,
        hits: &mut Vec<(TypeErrorKind, Span, &'static str)>,
    ) {
        match expr {
            Expr::GenBlock { body } => {
                hits.push((
                    TypeErrorKind::GenBlockInMachineTransition,
                    span.clone(),
                    "gen",
                ));
                Self::collect_machine_transition_forbidden_block(body, hits);
            }
            Expr::Await(inner) => {
                hits.push((
                    TypeErrorKind::AwaitInMachineTransition,
                    span.clone(),
                    "await",
                ));
                Self::collect_machine_transition_forbidden_exprs(&inner.0, &inner.1, hits);
            }
            Expr::Binary { left, right, .. }
            | Expr::Is {
                lhs: left,
                rhs: right,
            } => {
                Self::collect_machine_transition_forbidden_exprs(&left.0, &left.1, hits);
                Self::collect_machine_transition_forbidden_exprs(&right.0, &right.1, hits);
            }
            Expr::Unary { operand, .. }
            | Expr::ForkChild { expr: operand, .. }
            | Expr::PostfixTry(operand)
            | Expr::Yield(Some(operand)) => {
                Self::collect_machine_transition_forbidden_exprs(&operand.0, &operand.1, hits);
            }
            Expr::Tuple(exprs) | Expr::Array(exprs) | Expr::Join(exprs) => {
                for (expr, expr_span) in exprs {
                    Self::collect_machine_transition_forbidden_exprs(expr, expr_span, hits);
                }
            }
            Expr::ArrayRepeat { value, count } => {
                Self::collect_machine_transition_forbidden_exprs(&value.0, &value.1, hits);
                Self::collect_machine_transition_forbidden_exprs(&count.0, &count.1, hits);
            }
            Expr::MapLiteral { entries } => {
                for ((key, key_span), (value, value_span)) in entries {
                    Self::collect_machine_transition_forbidden_exprs(key, key_span, hits);
                    Self::collect_machine_transition_forbidden_exprs(value, value_span, hits);
                }
            }
            Expr::Block(block) | Expr::Scope { body: block } | Expr::ForkBlock { body: block } => {
                Self::collect_machine_transition_forbidden_block(block, hits);
            }
            Expr::UnsafeBlock(block) => {
                Self::collect_machine_transition_forbidden_block(block, hits);
            }
            Expr::If {
                condition,
                then_block,
                else_block,
            } => {
                Self::collect_machine_transition_forbidden_exprs(&condition.0, &condition.1, hits);
                Self::collect_machine_transition_forbidden_exprs(
                    &then_block.0,
                    &then_block.1,
                    hits,
                );
                if let Some(else_block) = else_block {
                    Self::collect_machine_transition_forbidden_exprs(
                        &else_block.0,
                        &else_block.1,
                        hits,
                    );
                }
            }
            Expr::IfLet {
                expr,
                body,
                else_body,
                ..
            } => {
                Self::collect_machine_transition_forbidden_exprs(&expr.0, &expr.1, hits);
                Self::collect_machine_transition_forbidden_block(body, hits);
                if let Some(block) = else_body {
                    Self::collect_machine_transition_forbidden_block(block, hits);
                }
            }
            Expr::Match { scrutinee, arms } => {
                Self::collect_machine_transition_forbidden_exprs(&scrutinee.0, &scrutinee.1, hits);
                for arm in arms {
                    if let Some((guard, guard_span)) = &arm.guard {
                        Self::collect_machine_transition_forbidden_exprs(guard, guard_span, hits);
                    }
                    Self::collect_machine_transition_forbidden_exprs(
                        &arm.body.0,
                        &arm.body.1,
                        hits,
                    );
                }
            }
            Expr::Lambda { body, .. } | Expr::SpawnLambdaActor { body, .. } => {
                Self::collect_machine_transition_forbidden_exprs(&body.0, &body.1, hits);
            }
            Expr::Spawn { target, args } => {
                Self::collect_machine_transition_forbidden_exprs(&target.0, &target.1, hits);
                for (_, (arg, arg_span)) in args {
                    Self::collect_machine_transition_forbidden_exprs(arg, arg_span, hits);
                }
            }
            Expr::ScopeDeadline { duration, body } => {
                Self::collect_machine_transition_forbidden_exprs(&duration.0, &duration.1, hits);
                Self::collect_machine_transition_forbidden_block(body, hits);
            }
            Expr::InterpolatedString(parts) => {
                for part in parts {
                    if let StringPart::Expr((expr, expr_span)) = part {
                        Self::collect_machine_transition_forbidden_exprs(expr, expr_span, hits);
                    }
                }
            }
            Expr::Call { function, args, .. } => {
                Self::collect_machine_transition_forbidden_exprs(&function.0, &function.1, hits);
                for arg in args {
                    let (arg_expr, arg_span) = arg.expr();
                    Self::collect_machine_transition_forbidden_exprs(arg_expr, arg_span, hits);
                }
            }
            Expr::MethodCall { receiver, args, .. } => {
                Self::collect_machine_transition_forbidden_exprs(&receiver.0, &receiver.1, hits);
                for arg in args {
                    let (arg_expr, arg_span) = arg.expr();
                    Self::collect_machine_transition_forbidden_exprs(arg_expr, arg_span, hits);
                }
            }
            Expr::StructInit { fields, base, .. } => {
                for (_, (field, field_span)) in fields {
                    Self::collect_machine_transition_forbidden_exprs(field, field_span, hits);
                }
                if let Some(base) = base {
                    Self::collect_machine_transition_forbidden_exprs(&base.0, &base.1, hits);
                }
            }
            Expr::Select { arms, timeout } => {
                for arm in arms {
                    Self::collect_machine_transition_forbidden_exprs(
                        &arm.source.0,
                        &arm.source.1,
                        hits,
                    );
                    Self::collect_machine_transition_forbidden_exprs(
                        &arm.body.0,
                        &arm.body.1,
                        hits,
                    );
                }
                if let Some(timeout) = timeout {
                    Self::collect_machine_transition_forbidden_exprs(
                        &timeout.duration.0,
                        &timeout.duration.1,
                        hits,
                    );
                    Self::collect_machine_transition_forbidden_exprs(
                        &timeout.body.0,
                        &timeout.body.1,
                        hits,
                    );
                }
            }
            Expr::Timeout { expr, duration } => {
                Self::collect_machine_transition_forbidden_exprs(&expr.0, &expr.1, hits);
                Self::collect_machine_transition_forbidden_exprs(&duration.0, &duration.1, hits);
            }
            Expr::FieldAccess { object, .. } => {
                Self::collect_machine_transition_forbidden_exprs(&object.0, &object.1, hits);
            }
            Expr::Index { object, index } => {
                Self::collect_machine_transition_forbidden_exprs(&object.0, &object.1, hits);
                Self::collect_machine_transition_forbidden_exprs(&index.0, &index.1, hits);
            }
            Expr::Cast { expr, .. } => {
                Self::collect_machine_transition_forbidden_exprs(&expr.0, &expr.1, hits);
            }
            Expr::Range { start, end, .. } => {
                if let Some(start) = start {
                    Self::collect_machine_transition_forbidden_exprs(&start.0, &start.1, hits);
                }
                if let Some(end) = end {
                    Self::collect_machine_transition_forbidden_exprs(&end.0, &end.1, hits);
                }
            }
            Expr::MachineEmit { fields, .. } => {
                for (_, (field, field_span)) in fields {
                    Self::collect_machine_transition_forbidden_exprs(field, field_span, hits);
                }
            }
            Expr::Literal(_)
            | Expr::Identifier(_)
            | Expr::Yield(None)
            | Expr::Cooperate
            | Expr::This
            | Expr::RegexLiteral(_)
            | Expr::ByteStringLiteral(_)
            | Expr::ByteArrayLiteral(_) => {}
        }
    }

    /// Validate that every trait named in a machine's generic bounds resolves
    /// to a registered trait. Emits `UndefinedType` at the machine decl span
    /// for any unknown name. Called from `check_machine_exhaustiveness`,
    /// after Pass 2 has populated `trait_defs` for all in-scope traits.
    pub(super) fn validate_machine_type_param_bounds(&mut self, md: &MachineDecl, span: &Span) {
        for param in &md.type_params {
            for bound in &param.bounds {
                if self.is_known_trait(&bound.name) {
                    continue;
                }
                let similar = crate::error::find_similar(
                    &bound.name,
                    self.trait_defs.keys().map(String::as_str),
                );
                self.report_error_with_suggestions(
                    TypeErrorKind::UndefinedType,
                    span,
                    format!(
                        "unknown trait `{bound}` in bound on type parameter `{param_name}` of machine `{machine}`",
                        bound = bound.name,
                        param_name = param.name,
                        machine = md.name,
                    ),
                    similar,
                );
            }
        }
    }

    /// Resolve a trait-bound name against the registered trait table,
    /// accepting both unqualified and module-qualified forms.
    fn is_known_trait(&self, name: &str) -> bool {
        if self.trait_defs.contains_key(name) {
            return true;
        }
        if let Some(uq) = self.strip_module_qualifier(name) {
            if self.trait_defs.contains_key(uq) {
                return true;
            }
        }
        false
    }

    /// Check that the machine's state × event matrix is fully covered.
    #[expect(
        clippy::too_many_lines,
        reason = "exhaustiveness checking requires many validation steps"
    )]
    pub(super) fn check_machine_exhaustiveness(&mut self, md: &MachineDecl, span: &Span) {
        self.validate_machine_type_param_bounds(md, span);
        let state_names: Vec<&str> = md.states.iter().map(|s| s.name.as_str()).collect();
        let event_names: Vec<&str> = md.events.iter().map(|e| e.name.as_str()).collect();

        // Fix 4: Enforce minimum cardinality
        if md.states.len() < 2 {
            self.errors.push(TypeError::new(
                TypeErrorKind::MachineExhaustivenessError,
                span.clone(),
                format!("machine '{}' must declare at least 2 states", md.name),
            ));
        }
        if md.events.is_empty() {
            self.errors.push(TypeError::new(
                TypeErrorKind::MachineExhaustivenessError,
                span.clone(),
                format!("machine '{}' must declare at least 1 event", md.name),
            ));
        }

        // Build coverage: track explicit (state, event) pairs and wildcard events
        let mut covered: HashSet<(String, String)> = HashSet::new();
        let mut wildcard_events: HashSet<String> = HashSet::new();

        for transition in &md.transitions {
            let transition_has_forbidden_expr =
                self.report_machine_transition_forbidden_exprs(&md.name, transition);

            // Fix 2: Reject unknown event names
            if !event_names.contains(&transition.event_name.as_str()) {
                self.errors.push(TypeError::new(
                    TypeErrorKind::MachineExhaustivenessError,
                    span.clone(),
                    format!(
                        "machine '{}': transition references unknown event '{}'",
                        md.name, transition.event_name
                    ),
                ));
            }

            // Fix 1: Reject unknown source/target state names
            if transition.source_state != "_"
                && !state_names.contains(&transition.source_state.as_str())
            {
                self.errors.push(TypeError::new(
                    TypeErrorKind::MachineExhaustivenessError,
                    span.clone(),
                    format!(
                        "machine '{}': transition references unknown state '{}'",
                        md.name, transition.source_state
                    ),
                ));
            }
            if transition.target_state != "_"
                && !state_names.contains(&transition.target_state.as_str())
            {
                self.errors.push(TypeError::new(
                    TypeErrorKind::MachineExhaustivenessError,
                    span.clone(),
                    format!(
                        "machine '{}': transition references unknown state '{}'",
                        md.name, transition.target_state
                    ),
                ));
            }

            if transition.source_state == "_" {
                // Fix 3: Reject duplicate wildcard transitions for same event
                if wildcard_events.contains(&transition.event_name) {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::MachineExhaustivenessError,
                        span.clone(),
                        format!(
                            "machine '{}': duplicate wildcard transition for event '{}'",
                            md.name, transition.event_name
                        ),
                    ));
                }
                wildcard_events.insert(transition.event_name.clone());
            } else {
                let key = (
                    transition.source_state.clone(),
                    transition.event_name.clone(),
                );
                // Fix 5: Reject duplicate explicit transitions (unless guarded)
                if covered.contains(&key) && transition.guard.is_none() {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::MachineExhaustivenessError,
                        span.clone(),
                        format!(
                            "machine '{}': duplicate transition for event '{}' in state '{}'",
                            md.name, transition.event_name, transition.source_state
                        ),
                    ));
                }
                covered.insert(key);
            }

            // Push the machine's declared generic-param bounds so that
            // `type_param_carries_bound` / resolver projection inside the
            // transition body see `T: Resource` and recognise `T` as
            // satisfying its bound. Pops at the end of this iteration's
            // body block.
            let machine_bounds_scope =
                self.collect_type_param_scope_with_bounds(Some(&md.type_params), None);
            let pushed_machine_bounds = !machine_bounds_scope.is_empty();
            if pushed_machine_bounds {
                self.current_type_param_bounds.push(machine_bounds_scope);
            }

            // Fix 6: Transition body validation with source-state field scoping.
            // Bind `state` as the machine type, and track the source state so
            // that `state.field` access resolves correctly for payload states.
            // (`state` rather than `self` to avoid confusion with actor self)
            self.env.push_scope();
            // Bind `state` as the machine self-type, preserving generic args
            // so that field access on generic machines resolves correctly.
            let transition_machine_args: Vec<Ty> = md
                .type_params
                .iter()
                .map(|param| Ty::Named {
                    builtin: None,
                    name: param.name.clone(),
                    args: vec![],
                })
                .collect();
            self.env.define(
                "state".to_string(),
                Ty::Named {
                    builtin: None,
                    name: md.name.clone(),
                    args: transition_machine_args,
                },
                false,
            );
            // Bind `event` as the event companion enum type so that
            // `event.field` resolves for events with payload fields.
            let event_type_name = format!("{}Event", md.name);
            self.env.define(
                "event".to_string(),
                Ty::Named {
                    builtin: None,
                    name: event_type_name,
                    args: md
                        .type_params
                        .iter()
                        .map(|param| Ty::Named {
                            builtin: None,
                            name: param.name.clone(),
                            args: vec![],
                        })
                        .collect(),
                },
                false,
            );
            if transition.source_state == "_" {
                self.current_machine_transition = Some((
                    md.name.clone(),
                    "_".to_string(),
                    transition.event_name.clone(),
                ));
            } else {
                self.current_machine_transition = Some((
                    md.name.clone(),
                    transition.source_state.clone(),
                    transition.event_name.clone(),
                ));
            }
            // Type-check guard expression if present
            if let Some((guard_expr, guard_span)) = &transition.guard {
                self.check_against(guard_expr, guard_span, &Ty::Bool);
            }
            if !transition_has_forbidden_expr {
                // Check the transition body against the machine type so that the
                // expected-type context flows into struct-variant pre-seeding
                // (expressions.rs enum-struct-variant arm).  Without an expected
                // type, `synthesize` cannot seed the type-params for generic
                // machines and bare state constructors like `Faulted { error: … }`
                // fail to resolve when the state has a generic field.
                let expected_machine_ty = Ty::Named {
                    builtin: None,
                    name: md.name.clone(),
                    args: md
                        .type_params
                        .iter()
                        .map(|param| Ty::Named {
                            builtin: None,
                            name: param.name.clone(),
                            args: vec![],
                        })
                        .collect(),
                };
                self.check_against(&transition.body.0, &transition.body.1, &expected_machine_ty);
            }
            self.current_machine_transition = None;
            self.env.pop_scope();
            if pushed_machine_bounds {
                self.current_type_param_bounds.pop();
            }
        }

        // Check that every (state, event) pair is covered
        // If has_default is true, unhandled pairs default to self-transition
        for state in &state_names {
            for event in &event_names {
                let key = (state.to_string(), event.to_string());
                if !covered.contains(&key) && !wildcard_events.contains(*event) && !md.has_default {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::MachineExhaustivenessError,
                        span.clone(),
                        format!(
                            "machine '{}': state '{}' does not handle event '{}'",
                            md.name, state, event
                        ),
                    ));
                }
            }
        }
    }

    pub(super) fn register_actor_decl(&mut self, ad: &ActorDecl) {
        let mut fields = HashMap::new();
        let mut hole_vars = Vec::new();
        for field in &ad.fields {
            let field_ty = self.resolve_registered_annotation_ty(&field.ty, &mut hole_vars);
            fields.insert(field.name.clone(), field_ty);
        }

        let type_def = TypeDef {
            kind: TypeDefKind::Actor,
            name: ad.name.clone(),
            type_params: vec![],
            fields,
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: ad.doc_comment.clone(),
            is_indirect: false,
        };

        // Actors are always Send
        self.registry.register_actor(ad.name.clone());
        self.register_rcfree_members_for_type(&ad.name, &type_def);

        self.type_defs.insert(ad.name.clone(), type_def);
        self.record_type_def_inference_holes(&ad.name, hole_vars);

        // Collect init() parameter shapes for supervisor wired_to type-compatibility checks.
        // Stores (param_name, outer_type, first_type_arg) for each init param.
        // Only `TypeExpr::Named` params are represented; complex types store the outer name only.
        //
        // Always insert — actors with no init block get an empty vec so that a
        // `wired_to:` reference to such an actor correctly fires
        // "no parameter named X" (`E_SUPERVISOR_WIRED_TO_TYPE_MISMATCH`) rather
        // than silently passing through the "unknown actor" early-return.
        let params: Vec<(String, String, Option<String>)> = if let Some(init) = &ad.init {
            init.params
                .iter()
                .map(|p| {
                    let (outer, inner) = match &p.ty.0 {
                        TypeExpr::Named { name, type_args } => (
                            name.clone(),
                            type_args.as_ref().and_then(|args| {
                                args.first().and_then(|(te, _)| {
                                    if let TypeExpr::Named { name: n, .. } = te {
                                        Some(n.clone())
                                    } else {
                                        None
                                    }
                                })
                            }),
                        ),
                        _ => (String::new(), None),
                    };
                    (p.name.clone(), outer, inner)
                })
                .collect()
        } else {
            vec![]
        };
        self.actor_init_params.insert(ad.name.clone(), params);
    }

    pub(super) fn register_wire_decl(&mut self, wd: &WireDecl) {
        // Wire types are similar to regular types but use string field types
        let mut fields = HashMap::new();
        for field in &wd.fields {
            let ty = Ty::from_name(&field.ty).unwrap_or_else(|| Ty::Named {
                builtin: None,
                name: field.ty.clone(),
                args: vec![],
            });
            fields.insert(field.name.clone(), ty);
        }

        let mut variants = HashMap::new();
        let mut hole_vars = Vec::new();
        for variant in &wd.variants {
            match &variant.kind {
                VariantKind::Unit => {
                    variants.insert(variant.name.clone(), VariantDef::Unit);
                }
                VariantKind::Tuple(fields) => {
                    let variant_tys = fields
                        .iter()
                        .map(|field| self.resolve_registered_annotation_ty(field, &mut hole_vars))
                        .collect();
                    variants.insert(variant.name.clone(), VariantDef::Tuple(variant_tys));
                }
                VariantKind::Struct(fields) => {
                    let variant_fields: Vec<(String, Ty)> = fields
                        .iter()
                        .map(|(name, field)| {
                            (
                                name.clone(),
                                self.resolve_registered_annotation_ty(field, &mut hole_vars),
                            )
                        })
                        .collect();
                    variants.insert(variant.name.clone(), VariantDef::Struct(variant_fields));
                }
            }
        }

        let type_def = TypeDef {
            kind: match wd.kind {
                WireDeclKind::Struct => TypeDefKind::Struct,
                WireDeclKind::Enum => TypeDefKind::Enum,
            },
            name: wd.name.clone(),
            type_params: vec![],
            fields,
            variants,
            methods: HashMap::new(),
            doc_comment: None,
            is_indirect: false,
        };

        let field_types: Vec<_> = type_def.fields.values().cloned().collect();
        self.registry.register_type(wd.name.clone(), field_types);
        self.register_rcfree_members_for_type(&wd.name, &type_def);

        self.type_defs.insert(wd.name.clone(), type_def);
        self.record_type_def_inference_holes(&wd.name, hole_vars);
        self.register_wire_methods(&wd.name);
    }

    pub(super) fn trait_info_from_decl(tr: &TraitDecl) -> TraitInfo {
        Self::trait_info_from_decl_with_diagnostics(tr, &mut Vec::new())
    }

    /// Harvest `#[lang_item("…")]` tags from a trait declaration into
    /// [`Checker::lang_items`].
    ///
    /// Two kinds of entries are produced:
    ///
    /// * Trait-level (`#[lang_item("display")]` on the `trait` itself) →
    ///   `LangItemBinding { trait_name: <td.name>, method_name: None }`.
    /// * Method-level (`#[lang_item("display_fmt")]` on a `TraitItem::Method`)
    ///   → `LangItemBinding { trait_name: <td.name>, method_name:
    ///   Some(<m.name>) }`. The enclosing trait name is propagated so HIR
    ///   lowering can derive `<SelfType>::<method_name>` impl symbols.
    ///
    /// Duplicate keys raise `TypeError::duplicate_definition` against the
    /// trait's span so the registry remains one-binding-per-key.
    pub(super) fn register_trait_lang_items(&mut self, td: &TraitDecl, span: Span) {
        if let Some(key) = &td.lang_item {
            if let Some(prev) = self.lang_item_spans.insert(key.clone(), span.clone()) {
                self.errors
                    .push(TypeError::duplicate_definition(span.clone(), key, prev));
            } else {
                self.lang_items.insert(
                    key.clone(),
                    crate::LangItemBinding {
                        trait_name: td.name.clone(),
                        method_name: None,
                    },
                );
            }
        }
        for item in &td.items {
            if let TraitItem::Method(m) = item {
                if let Some(key) = &m.lang_item {
                    let method_span = m.span.clone();
                    if let Some(prev) = self
                        .lang_item_spans
                        .insert(key.clone(), method_span.clone())
                    {
                        self.errors
                            .push(TypeError::duplicate_definition(method_span, key, prev));
                    } else {
                        self.lang_items.insert(
                            key.clone(),
                            crate::LangItemBinding {
                                trait_name: td.name.clone(),
                                method_name: Some(m.name.clone()),
                            },
                        );
                    }
                }
            }
        }
    }

    /// Build `TraitInfo` and surface trait-body diagnostics. Duplicate
    /// `type Bar; type Bar;` declarations are reported here (the impl-side
    /// duplicate-detection is handled separately in `build_impl_alias_entries`).
    pub(super) fn trait_info_from_decl_with_diagnostics(
        tr: &TraitDecl,
        errors: &mut Vec<TypeError>,
    ) -> TraitInfo {
        let mut methods = Vec::new();
        let mut associated_types: Vec<TraitAssociatedTypeInfo> = Vec::new();
        let mut seen_assoc: HashMap<String, Span> = HashMap::new();
        for item in &tr.items {
            match item {
                TraitItem::Method(m) => methods.push(m.clone()),
                TraitItem::AssociatedType {
                    name,
                    bounds,
                    default,
                    span,
                } => {
                    if let Some(prev_span) = seen_assoc.insert(name.clone(), span.clone()) {
                        errors.push(TypeError::duplicate_definition(
                            span.clone(),
                            name,
                            prev_span,
                        ));
                        continue;
                    }
                    associated_types.push(TraitAssociatedTypeInfo {
                        name: name.clone(),
                        bounds: bounds.clone(),
                        default: default.clone(),
                        span: span.clone(),
                    });
                }
            }
        }
        let type_params = tr
            .type_params
            .as_ref()
            .map(|params| params.iter().map(|p| p.name.clone()).collect())
            .unwrap_or_default();
        TraitInfo {
            methods,
            associated_types,
            type_params,
        }
    }

    pub(super) fn build_impl_alias_entries(
        &mut self,
        id: &ImplDecl,
    ) -> HashMap<String, ImplAliasEntry> {
        let mut entries = HashMap::new();
        let mut seen_spans: HashMap<String, Span> = HashMap::new();
        for alias in &id.type_aliases {
            if let Some(prev_span) = seen_spans.insert(alias.name.clone(), alias.ty.1.clone()) {
                self.errors.push(TypeError::duplicate_definition(
                    alias.ty.1.clone(),
                    &alias.name,
                    prev_span,
                ));
                continue;
            }
            entries.insert(
                alias.name.clone(),
                ImplAliasEntry {
                    expr: alias.ty.clone(),
                    resolved: None,
                    resolving: false,
                },
            );
        }
        if let Some(tb) = &id.trait_bound {
            if let Some(trait_info) = self.trait_defs.get(&tb.name) {
                for assoc in &trait_info.associated_types {
                    if entries.contains_key(&assoc.name) {
                        continue;
                    }
                    if let Some(default) = &assoc.default {
                        entries.insert(
                            assoc.name.clone(),
                            ImplAliasEntry {
                                expr: default.clone(),
                                resolved: None,
                                resolving: false,
                            },
                        );
                    }
                }
            }
        }
        entries
    }

    pub(super) fn enter_impl_scope(
        &mut self,
        id: &ImplDecl,
        span: &Span,
        type_name: Option<&str>,
        enforce: bool,
    ) -> bool {
        let Some(target_name) = type_name else {
            return false;
        };
        let entries = self.build_impl_alias_entries(id);
        let impl_bounds_map = self.collect_type_param_scope_with_bounds(
            id.type_params.as_ref(),
            id.where_clause.as_ref(),
        );
        let pushed_impl_bounds = !impl_bounds_map.is_empty();
        if pushed_impl_bounds {
            self.current_type_param_bounds.push(impl_bounds_map);
        }
        // Populate impl_assoc_type_bindings on every enter (not gated on
        // `enforce`) so projection collapse can find bindings during
        // call-site monomorphisation even when this scope was entered by
        // a non-enforcing registration sweep. The first writer wins;
        // subsequent calls with the same impl idempotently re-resolve.
        if let Some(tb) = &id.trait_bound {
            // Snapshot trait-side assoc-type list to avoid double-borrow
            // of trait_defs while we call resolve_type_expr.
            let assoc_names: Vec<String> = self
                .trait_defs
                .get(&tb.name)
                .map(|info| {
                    info.associated_types
                        .iter()
                        .map(|a| a.name.clone())
                        .collect()
                })
                .unwrap_or_default();
            let tb_name = tb.name.clone();
            let target_owned = target_name.to_string();
            for assoc_name in assoc_names {
                let key = (target_owned.clone(), tb_name.clone(), assoc_name.clone());
                if self.impl_assoc_type_bindings.contains_key(&key) {
                    continue;
                }
                if let Some(entry) = entries.get(&assoc_name) {
                    let expr = entry.expr.clone();
                    let resolved = self.resolve_type_expr(&expr);
                    if !matches!(resolved, Ty::Error) {
                        self.impl_assoc_type_bindings.insert(key, resolved);
                    }
                }
            }
        }
        if enforce {
            if let Some(tb) = &id.trait_bound {
                // Snapshot trait-side data we need; cloned so we can release
                // the borrow on `self.trait_defs` before calling into the
                // resolver / bound-checker which need `&mut self`.
                let trait_snapshot = self
                    .trait_defs
                    .get(&tb.name)
                    .map(|info| info.associated_types.clone());
                if let Some(associated_types) = trait_snapshot {
                    let missing: Vec<TraitAssociatedTypeInfo> = associated_types
                        .iter()
                        .filter(|assoc| !entries.contains_key(&assoc.name))
                        .cloned()
                        .collect();
                    let target_name_owned = target_name.to_string();
                    let tb_name = tb.name.clone();
                    for assoc in missing {
                        self.report_error_with_note(
                            TypeErrorKind::UndefinedType,
                            span,
                            format!(
                                "impl `{tb_name}` for `{target_name_owned}` must define associated type `{}`",
                                assoc.name
                            ),
                            &assoc.span,
                            "required associated type declared here".to_string(),
                        );
                    }
                    self.check_assoc_type_bounds(
                        &associated_types,
                        &entries,
                        &tb_name,
                        &target_name_owned,
                        id,
                    );
                }
            }
        }
        if pushed_impl_bounds {
            self.current_type_param_bounds.pop();
        }
        self.impl_alias_scopes.push(ImplAliasScope {
            span: span.clone(),
            entries,
            missing_reported: HashSet::new(),
            report_missing: enforce,
        });
        true
    }

    pub(super) fn exit_impl_scope(&mut self) {
        self.impl_alias_scopes.pop();
    }

    /// Enforce trait-side bounds on each impl-side associated-type binding.
    ///
    /// For `trait Foo { type Out: Display; }` and `impl Foo for X { type Out = Y; }`,
    /// verifies `Y: Display`. Handles two distinct shapes for the chosen `Y`:
    ///
    /// - **Concrete type** (`Ty::Named { name, .. }` where `name` is a known
    ///   type-def): consult `type_satisfies_trait_bound` directly.
    /// - **Impl type-param** (`Ty::Named { name, .. }` where `name` is one of
    ///   the impl's declared type params, e.g. `impl<T: Display> Foo for X { type Out = T; }`):
    ///   consult the impl's own `collect_type_param_bounds` map, because at
    ///   impl-registration time `current_function` is not set and
    ///   `type_satisfies_trait_bound`'s `type_param_carries_bound` fallback
    ///   would return false-negative.
    fn check_assoc_type_bounds(
        &mut self,
        associated_types: &[TraitAssociatedTypeInfo],
        entries: &HashMap<String, ImplAliasEntry>,
        trait_name: &str,
        target_name: &str,
        id: &ImplDecl,
    ) {
        // Pre-collect impl-side type-param bounds. Keys are param names
        // (e.g. `T`), values are bound trait names. Reused across all assoc
        // types in this impl.
        let impl_param_bounds: HashMap<String, Vec<String>> =
            self.collect_type_param_bounds(id.type_params.as_ref(), id.where_clause.as_ref());
        let impl_param_names: HashSet<String> = id
            .type_params
            .as_ref()
            .map(|tps| tps.iter().map(|tp| tp.name.clone()).collect())
            .unwrap_or_default();

        for assoc in associated_types {
            if assoc.bounds.is_empty() {
                continue;
            }
            let Some(entry) = entries.get(&assoc.name) else {
                continue;
            };
            let expr = entry.expr.clone();
            let entry_span = expr.1.clone();
            let resolved = self.resolve_type_expr(&expr);
            // Skip bounds checking when the RHS itself failed to resolve.
            // `resolve_type_expr` already emitted the primary diagnostic;
            // running `type_satisfies_trait_bound(&Ty::Error, _)` here would
            // produce a spurious cascading `BoundsNotSatisfied` on top of it.
            if matches!(resolved, Ty::Error) {
                continue;
            }
            for bound in &assoc.bounds {
                let bound_name = &bound.name;
                let satisfied = match &resolved {
                    Ty::Named { name, .. } if impl_param_names.contains(name) => {
                        // Impl type-param: check the impl's own bounds map.
                        impl_param_bounds.get(name).is_some_and(|bs| {
                            bs.iter()
                                .any(|b| b == bound_name || self.trait_extends(b, bound_name))
                        })
                    }
                    _ => self.type_satisfies_trait_bound(&resolved, bound_name),
                };
                if satisfied {
                    continue;
                }
                self.report_error(
                    TypeErrorKind::BoundsNotSatisfied,
                    &entry_span,
                    format!(
                        "associated type `{}::{}` in impl for `{}` is bound by trait \
                         `{}` but `{}` does not implement `{}`",
                        trait_name,
                        assoc.name,
                        target_name,
                        bound_name,
                        resolved.user_facing(),
                        bound_name,
                    ),
                );
            }
        }
    }

    pub(super) fn resolve_impl_associated_type(&mut self, alias: &str) -> Option<Ty> {
        let scope_index = self.impl_alias_scopes.len().checked_sub(1)?;
        let expr = {
            let scope = &mut self.impl_alias_scopes[scope_index];
            if let Some(entry) = scope.entries.get_mut(alias) {
                if let Some(resolved) = &entry.resolved {
                    return Some(resolved.clone());
                }
                if entry.resolving {
                    let should_report =
                        scope.report_missing && scope.missing_reported.insert(alias.to_string());
                    let err_span = entry.expr.1.clone();
                    if should_report {
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            &err_span,
                            format!(
                                "associated type `Self::{alias}` recursively references itself"
                            ),
                        );
                    }
                    return Some(Ty::Error);
                }
                entry.resolving = true;
                entry.expr.clone()
            } else {
                let should_report =
                    scope.report_missing && scope.missing_reported.insert(alias.to_string());
                let err_span = scope.span.clone();
                if should_report {
                    self.report_error(
                        TypeErrorKind::UndefinedType,
                        &err_span,
                        format!("type alias `Self::{alias}` is not defined in this impl"),
                    );
                }
                return Some(Ty::Error);
            }
        };
        let ty = self.resolve_type_expr(&expr);
        if let Some(scope) = self.impl_alias_scopes.get_mut(scope_index) {
            if let Some(entry) = scope.entries.get_mut(alias) {
                entry.resolving = false;
                entry.resolved = Some(ty.clone());
            }
        }
        Some(ty)
    }

    /// Pass 2: Collect function signatures
    pub(super) fn collect_functions(&mut self, program: &Program) {
        // Process module graph items first (if multi-module).
        // Skip the root module — its items are already in program.items and
        // will be processed below with current_module = None (bare names).
        if let Some(ref mg) = program.module_graph {
            for mod_id in &mg.topo_order {
                if *mod_id == mg.root {
                    continue;
                }
                if let Some(module) = mg.modules.get(mod_id) {
                    let module_name = mod_id.path.join(".");
                    self.current_module = Some(module_name.clone());
                    // Temporarily scope local_type_defs to this module so
                    // that register_channel_recv_builtins (called from
                    // register_extern_block) can detect module-local types
                    // like Receiver, and locally_non_generic suppresses
                    // fresh-var injection for handle types like Sender.
                    let saved_local_type_defs = self.local_type_defs.clone();
                    let saved_source_type_defs = self.source_type_defs.clone();
                    for (item, _) in &module.items {
                        match item {
                            Item::TypeDecl(td) => {
                                self.local_type_defs.insert(td.name.clone());
                                self.source_type_defs.insert(td.name.clone());
                            }
                            Item::Machine(md) => {
                                self.local_type_defs.insert(md.name.clone());
                                self.source_type_defs.insert(md.name.clone());
                                let event_type_name = format!("{}Event", md.name);
                                self.local_type_defs.insert(event_type_name.clone());
                                self.source_type_defs.insert(event_type_name);
                            }
                            _ => {}
                        }
                    }

                    // Snapshot error/warning counts before signature registration
                    // for this module.  Diagnostics emitted during collect_function_item
                    // (e.g. duplicate-definition errors, import errors) are tagged with
                    // the module name below so the CLI renders them against the correct
                    // source file rather than the root compilation unit.
                    let err_before = self.errors.len();
                    let warn_before = self.warnings.len();

                    for (item, span) in &module.items {
                        self.collect_function_item(item, span);
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

        // Process main module items.
        self.current_module = None;
        for (item, span) in &program.items {
            self.collect_function_item(item, span);
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "expression type checking requires many cases"
    )]
    pub(super) fn collect_function_item(&mut self, item: &Item, span: &Span) {
        match item {
            Item::Function(fd) => {
                let scoped_name = match &self.current_module {
                    Some(m) => format!("{m}.{}", fd.name),
                    None => fd.name.clone(),
                };
                if let Some((prev_span, _)) = self.fn_def_spans.get(&scoped_name) {
                    self.errors.push(TypeError::duplicate_definition(
                        span.clone(),
                        &scoped_name,
                        prev_span.clone(),
                    ));
                } else {
                    self.fn_def_spans
                        .insert(scoped_name, (span.clone(), self.current_module.clone()));
                }
                self.register_fn_sig(fd);
            }
            Item::Actor(ad) => {
                for rf in &ad.receive_fns {
                    self.register_receive_fn(&ad.name, rf);
                }
                for method in &ad.methods {
                    let method_name = format!("{}::{}", ad.name, method.name);
                    self.register_fn_sig_with_name(&method_name, method);
                }
            }
            Item::Impl(id) => {
                // Register impl methods with Type::method naming
                if let TypeExpr::Named {
                    name: type_name,
                    type_args,
                } = &id.target_type.0
                {
                    // Do NOT push generic_ctx here — type params like T should remain
                    // as Ty::Named so that substitute_named_param can replace them
                    // at method call sites with concrete type arguments.

                    // Set current_self_type for resolving `Self` in method parameters
                    let prev_self_type = self.current_self_type.take();
                    let self_type_args: Vec<Ty> = type_args
                        .as_ref()
                        .map(|args| {
                            args.iter()
                                .map(|type_arg| self.resolve_type_expr(type_arg))
                                .collect()
                        })
                        .unwrap_or_default();
                    self.current_self_type = Some((type_name.clone(), self_type_args.clone()));
                    let scope_pushed =
                        self.enter_impl_scope(id, span, Some(type_name.as_str()), false);

                    let primitive_key = id.trait_bound.as_ref().and_then(|_| {
                        Self::canonical_primitive_or_builtin_key_from_name(type_name)
                    });

                    for method in &id.methods {
                        let sig = self.register_impl_method(
                            type_name,
                            method,
                            id.type_params.as_ref(),
                            id.where_clause.as_ref(),
                        );
                        // Stage A1: when the receiver is a primitive or compiler-builtin
                        // generic, `lookup_type_def_mut(type_name)` returns `None` so the
                        // sig has nowhere to live for later dispatch.  Mirror it onto the
                        // side table keyed by the canonical receiver kind + trait name so
                        // method-resolution can find it.
                        if let (Some(canonical), Some(tb)) =
                            (primitive_key.clone(), id.trait_bound.as_ref())
                        {
                            self.record_primitive_trait_impl_method(
                                canonical,
                                tb.name.clone(),
                                method.name.clone(),
                                sig,
                            );
                        }
                    }

                    // Register default trait methods not overridden in this impl
                    if let Some(tb) = &id.trait_bound {
                        self.record_trait_impl(type_name, &tb.name);

                        let overridden: HashSet<&str> =
                            id.methods.iter().map(|m| m.name.as_str()).collect();
                        if let Some(trait_methods) = self.trait_defs.get(&tb.name) {
                            let defaults: Vec<_> = trait_methods
                                .methods
                                .iter()
                                .filter(|m| {
                                    m.body.is_some() && !overridden.contains(m.name.as_str())
                                })
                                .cloned()
                                .collect();
                            for m in defaults {
                                let method_key = format!("{type_name}::{}", m.name);
                                let skip = usize::from(
                                    m.params.first().is_some_and(|p| self.is_receiver_param(p)),
                                );
                                let param_names: Vec<String> =
                                    m.params.iter().skip(skip).map(|p| p.name.clone()).collect();
                                self.register_trait_method_sig(&tb.name, &m, span);
                                let trait_method_key = format!("{}::{}", tb.name, m.name);
                                let concrete_self = Ty::Named {
                                    builtin: None,
                                    name: type_name.clone(),
                                    args: self_type_args.clone(),
                                };
                                let (params, return_type) = if let Some(sig) =
                                    self.fn_sigs.get(&trait_method_key).cloned()
                                {
                                    // Qualified trait signatures registered outside an impl
                                    // scope can still include a concrete receiver
                                    // (`fn bump(box: CounterBox)`) because receiver
                                    // detection there only knows about `Self`.
                                    // When copying defaults onto a concrete impl,
                                    // drop that leading receiver iff the trait sig
                                    // still has it.
                                    let sig_skip = usize::from(
                                        skip == 1 && sig.params.len() == m.params.len(),
                                    );
                                    (
                                        sig.params
                                            .iter()
                                            .skip(sig_skip)
                                            .map(|ty| {
                                                ty.substitute_named_param("Self", &concrete_self)
                                            })
                                            .collect::<Vec<_>>(),
                                        sig.return_type
                                            .substitute_named_param("Self", &concrete_self),
                                    )
                                } else {
                                    (
                                        m.params
                                            .iter()
                                            .skip(skip)
                                            .map(|p| {
                                                self.resolve_registered_annotation_ty_no_holes(
                                                    &p.ty,
                                                )
                                            })
                                            .collect(),
                                        m.return_type.as_ref().map_or(Ty::Unit, |ret| {
                                            self.resolve_registered_annotation_ty_no_holes(ret)
                                        }),
                                    )
                                };
                                let sig = FnSig {
                                    param_names: param_names.clone(),
                                    params: params.clone(),
                                    return_type: return_type.clone(),
                                    is_pure: m.is_pure,
                                    ..FnSig::default()
                                };
                                self.fn_sigs.insert(method_key, sig);
                                if let Some(td) = self.lookup_type_def_mut(type_name) {
                                    td.methods.insert(
                                        m.name.clone(),
                                        FnSig {
                                            param_names,
                                            params,
                                            return_type,
                                            is_pure: m.is_pure,
                                            ..FnSig::default()
                                        },
                                    );
                                }
                            }
                        }
                    }

                    // Restore previous self type
                    self.current_self_type = prev_self_type;
                    if scope_pushed {
                        self.exit_impl_scope();
                    }
                }
            }
            Item::TypeDecl(td) => {
                // Register methods defined inside struct/enum bodies
                for item in &td.body {
                    if let TypeBodyItem::Method(method) = item {
                        let method_key = format!("{}::{}", td.name, method.name);
                        self.register_fn_sig_with_name(&method_key, method);
                        let skip = usize::from(
                            method
                                .params
                                .first()
                                .is_some_and(|p| self.is_receiver_param(p)),
                        );
                        let param_names: Vec<String> = method
                            .params
                            .iter()
                            .skip(skip)
                            .map(|p| p.name.clone())
                            .collect();
                        let params: Vec<Ty> = method
                            .params
                            .iter()
                            .skip(skip)
                            .map(|p| self.resolve_registered_annotation_ty_no_holes(&p.ty))
                            .collect();
                        let return_type = method.return_type.as_ref().map_or(Ty::Unit, |ret| {
                            self.resolve_registered_annotation_ty_no_holes(ret)
                        });
                        let is_async = method.is_async;
                        let method_name = method.name.clone();
                        let type_name = td.name.clone();
                        if let Some(type_def) = self.lookup_type_def_mut(&type_name) {
                            type_def.methods.insert(
                                method_name,
                                FnSig {
                                    param_names,
                                    params,
                                    return_type,
                                    is_async,
                                    is_pure: method.is_pure,
                                    ..FnSig::default()
                                },
                            );
                        }
                    }
                }
            }
            Item::Trait(td) => {
                for trait_item in &td.items {
                    if let TraitItem::Method(method) = trait_item {
                        self.register_trait_method_sig(&td.name, method, span);
                    }
                }
            }
            Item::ExternBlock(eb) => {
                self.register_extern_block(eb);
            }
            Item::Import(id) => {
                // Always track the import span. For non-root modules the span is a byte
                // offset into the sub-module's own source file; the stored `source_module`
                // in `import_spans` tells the diagnostic renderer which file owns the span.
                self.register_import(id, Some(span));
            }
            Item::Const(_)
            | Item::TypeAlias(_)
            | Item::Wire(_)
            | Item::Supervisor(_)
            | Item::Machine(_)
            | Item::Record(_) => {
                // Records have no method body items in v0.5; method registration
                // is a no-op here.  TODO(A-4): if records gain methods, register
                // them via a `register_record_methods` pass here.
            }
        }
    }

    pub(super) fn register_fn_sig(&mut self, fd: &FnDecl) {
        self.register_fn_sig_with_name(&fd.name, fd);
    }

    fn register_trait_method_sig(
        &mut self,
        trait_name: &str,
        method: &hew_parser::ast::TraitMethod,
        span: &Span,
    ) {
        let method_key = format!("{trait_name}::{}", method.name);
        if self.fn_sigs.contains_key(&method_key) {
            return;
        }
        // Activate trait-body `Self::Bar` projection while resolving this
        // method's signature, so `Self::Item` in the return type becomes a
        // deferred `Ty::AssocType` carrier instead of an opaque named type.
        let prev_trait_self = self
            .current_trait_for_self_projection
            .replace(trait_name.to_string());
        self.register_fn_sig_with_name(
            &method_key,
            &FnDecl {
                attributes: vec![],
                is_async: false,
                is_generator: false,
                visibility: hew_parser::ast::Visibility::Private,
                is_pure: method.is_pure,
                name: method.name.clone(),
                type_params: method.type_params.clone(),
                params: method.params.clone(),
                return_type: method.return_type.clone(),
                where_clause: method.where_clause.clone(),
                body: hew_parser::ast::Block {
                    stmts: vec![],
                    trailing_expr: None,
                },
                doc_comment: None,
                decl_span: span.clone(),
                fn_span: 0..0,
                intrinsic: None,
            },
        );
        self.current_trait_for_self_projection = prev_trait_self;
    }

    /// Like `collect_type_param_bounds` but always includes a key for every
    /// declared type param, with an empty `Vec` when no bounds are
    /// declared. Used by the resolver to distinguish "type param in scope
    /// with no bounds" (emit missing-bound diagnostic) from "name is not a
    /// type param at all" (fall through to other resolution paths).
    pub(super) fn collect_type_param_scope_with_bounds(
        &self,
        type_params: Option<&Vec<TypeParam>>,
        where_clause: Option<&WhereClause>,
    ) -> HashMap<String, Vec<String>> {
        let mut map: HashMap<String, Vec<String>> = HashMap::new();
        if let Some(params) = type_params {
            for param in params {
                map.entry(param.name.clone()).or_default();
            }
        }
        let with_bounds = self.collect_type_param_bounds(type_params, where_clause);
        for (k, v) in with_bounds {
            let entry = map.entry(k).or_default();
            for b in v {
                if !entry.iter().any(|existing| existing == &b) {
                    entry.push(b);
                }
            }
        }
        map
    }

    #[expect(
        clippy::unused_self,
        reason = "method signature is part of the checker API"
    )]
    pub(super) fn collect_type_param_bounds(
        &self,
        type_params: Option<&Vec<TypeParam>>,
        where_clause: Option<&WhereClause>,
    ) -> HashMap<String, Vec<String>> {
        let mut bounds = HashMap::new();
        let mut declared = HashSet::new();
        if let Some(params) = type_params {
            for param in params {
                declared.insert(param.name.clone());
                if param.bounds.is_empty() {
                    continue;
                }
                let entry = bounds.entry(param.name.clone()).or_default();
                for bound in &param.bounds {
                    Self::push_unique_bound(entry, &bound.name);
                }
            }
        }
        if let Some(wc) = where_clause {
            for predicate in &wc.predicates {
                if let TypeExpr::Named { name, type_args } = &predicate.ty.0 {
                    if !declared.contains(name) {
                        continue;
                    }
                    if type_args.as_ref().is_some_and(|args| !args.is_empty()) {
                        continue;
                    }
                    let entry = bounds.entry(name.clone()).or_default();
                    for bound in &predicate.bounds {
                        Self::push_unique_bound(entry, &bound.name);
                    }
                }
            }
        }
        bounds
    }

    pub(super) fn push_unique_bound(entry: &mut Vec<String>, bound: &str) {
        if !entry.iter().any(|b| b == bound) {
            entry.push(bound.to_string());
        }
    }

    /// Check whether a parameter is the receiver (i.e. the implicit first
    /// parameter of an impl/trait method).  A parameter is a receiver if its
    /// declared type matches `Self` or the current impl target type.
    /// Note: name-based matching (`p.name == "self"`) has been intentionally
    /// removed — receivers are identified by type, not by name.
    pub(super) fn is_receiver_param(&mut self, p: &Param) -> bool {
        match &p.ty.0 {
            TypeExpr::Named { name, type_args } => {
                if name == "Self" {
                    return true;
                }
                // Clone to avoid borrowing self while we resolve type args.
                let impl_target = self.current_self_type.clone();
                if let Some((self_name, self_type_args)) = impl_target {
                    if name != &self_name {
                        return false;
                    }
                    // Name matches — also verify generic arguments match the
                    // impl target so that e.g. `impl Box<int>` rejects a
                    // parameter typed `Box<string>`.
                    let param_args: Vec<Ty> = type_args
                        .as_ref()
                        .map(|args| {
                            args.iter()
                                .map(|type_arg| self.resolve_type_expr(type_arg))
                                .collect()
                        })
                        .unwrap_or_default();
                    param_args == self_type_args
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    pub(super) fn register_fn_sig_with_name(&mut self, name: &str, fd: &FnDecl) {
        // Only filter out the receiver for methods (Type::method), not free
        // functions that happen to have a parameter named `self`.
        let is_method = name.contains("::");
        let skip = if is_method {
            usize::from(fd.params.first().is_some_and(|p| self.is_receiver_param(p)))
        } else {
            0
        };
        // Push the type-param bounds map BEFORE resolving the signature so
        // the resolver can validate `T::Bar` projections that appear in
        // param/return types. Includes type params with no bounds so the
        // resolver can distinguish "in scope with no bounds" (emit
        // missing-bound diagnostic) from "not in scope" (fall through).
        let fn_bounds = self.collect_type_param_scope_with_bounds(
            fd.type_params.as_ref(),
            fd.where_clause.as_ref(),
        );
        let pushed_bounds = !fn_bounds.is_empty();
        if pushed_bounds {
            self.current_type_param_bounds.push(fn_bounds);
        }
        let mut hole_vars = Vec::new();
        let param_names = fd
            .params
            .iter()
            .skip(skip)
            .map(|p| p.name.clone())
            .collect();
        let params = fd
            .params
            .iter()
            .skip(skip)
            .map(|p| self.resolve_registered_annotation_ty(&p.ty, &mut hole_vars))
            .collect();
        let declared_return = fd.return_type.as_ref().map_or(Ty::Unit, |ret| {
            self.resolve_registered_annotation_ty(ret, &mut hole_vars)
        });
        if pushed_bounds {
            self.current_type_param_bounds.pop();
        }
        // Wrap return type for generator functions
        let return_type = if fd.is_generator && fd.is_async {
            Ty::async_generator(declared_return)
        } else if fd.is_generator {
            Ty::generator(declared_return, Ty::Unit)
        } else {
            declared_return
        };

        let sig = FnSig {
            type_params: fd.type_params.as_ref().map_or(vec![], |params| {
                params.iter().map(|p| p.name.clone()).collect()
            }),
            type_param_bounds: self
                .collect_type_param_bounds(fd.type_params.as_ref(), fd.where_clause.as_ref()),
            param_names,
            params,
            return_type,
            is_async: fd.is_async,
            is_pure: fd.is_pure,
            doc_comment: fd.doc_comment.clone(),
            ..FnSig::default()
        };

        let key = scoped_module_item_name(self.current_module.as_deref(), name)
            .unwrap_or_else(|| name.to_string());
        self.fn_sigs.insert(key.clone(), sig);
        self.record_fn_sig_inference_holes(&key, hole_vars);
        // If the declaration carries `#[intrinsic("name")]`, record the
        // mapping so HIR lowering can skip the body and wire to the catalog.
        if let Some(intrinsic_key) = &fd.intrinsic {
            self.intrinsic_declarations
                .insert(key, intrinsic_key.clone());
        }
    }

    /// Register an impl method on a type's method table and `fn_sigs`.
    ///
    /// `impl_type_params` carries the enclosing `impl<T, U, …>` type
    /// parameter names so they are included in the resulting `FnSig`.
    ///
    /// `impl_where_clause` carries the enclosing impl block's where-clause so
    /// that bounds of the form `impl<T> Holder<T> where T: Display` are
    /// propagated into the method signature — both the `td.methods` entry and
    /// the `fn_sigs` entry consulted by `type_param_carries_bound`.
    ///
    /// **Important**: the caller must have already pushed the impl-level type
    /// params into `self.generic_ctx` so that type resolution sees them.
    ///
    /// Returns the built `FnSig` for callers that need to insert it
    /// on additional type names (e.g., qualified aliases).
    pub(super) fn register_impl_method(
        &mut self,
        type_name: &str,
        method: &FnDecl,
        impl_type_params: Option<&Vec<TypeParam>>,
        impl_where_clause: Option<&WhereClause>,
    ) -> FnSig {
        let method_key = format!("{type_name}::{}", method.name);
        // Push impl-level bounds onto the resolver's stack so the method
        // signature can reference `T::Bar` where `T` is an impl type param
        // (e.g. `impl<I: Iterator> Foo for X { fn next() -> I::Item }`).
        let impl_bounds_map =
            self.collect_type_param_scope_with_bounds(impl_type_params, impl_where_clause);
        let pushed_impl_bounds = !impl_bounds_map.is_empty();
        if pushed_impl_bounds {
            self.current_type_param_bounds.push(impl_bounds_map);
        }
        self.register_fn_sig_with_name(&method_key, method);
        if pushed_impl_bounds {
            self.current_type_param_bounds.pop();
        }

        // Patch the fn_sigs entry to include impl-level type params and their
        // bounds. `register_fn_sig_with_name` only records method-level params,
        // so `type_param_carries_bound` would otherwise miss impl-level bounds
        // such as `T: Display` in `impl<T: Display> Holder<T>`.
        if let Some(impl_tps) = impl_type_params {
            let impl_bounds = self.collect_type_param_bounds(impl_type_params, impl_where_clause);
            let key = scoped_module_item_name(self.current_module.as_deref(), &method_key)
                .unwrap_or_else(|| method_key.clone());
            if let Some(sig) = self.fn_sigs.get_mut(&key) {
                for tp in impl_tps {
                    if !sig.type_params.contains(&tp.name) {
                        sig.type_params.push(tp.name.clone());
                    }
                }
                for (param, bounds) in impl_bounds {
                    let entry = sig.type_param_bounds.entry(param).or_default();
                    for bound in bounds {
                        Self::push_unique_bound(entry, &bound);
                    }
                }
            }
        }

        let impl_bounds_map =
            self.collect_type_param_scope_with_bounds(impl_type_params, impl_where_clause);
        let pushed_impl_bounds = !impl_bounds_map.is_empty();
        if pushed_impl_bounds {
            self.current_type_param_bounds.push(impl_bounds_map);
        }
        let skip = usize::from(
            method
                .params
                .first()
                .is_some_and(|p| self.is_receiver_param(p)),
        );
        let params: Vec<Ty> = method
            .params
            .iter()
            .skip(skip)
            .map(|p| self.resolve_registered_annotation_ty_no_holes(&p.ty))
            .collect();
        let return_type = method.return_type.as_ref().map_or(Ty::Unit, |ret| {
            self.resolve_registered_annotation_ty_no_holes(ret)
        });
        if pushed_impl_bounds {
            self.current_type_param_bounds.pop();
        }
        let param_names: Vec<String> = method
            .params
            .iter()
            .skip(skip)
            .map(|p| p.name.clone())
            .collect();

        // Collect type param names: impl-level + method-level.
        let mut all_type_params: Vec<String> = impl_type_params
            .map(|tps| tps.iter().map(|tp| tp.name.clone()).collect())
            .unwrap_or_default();
        if let Some(method_tps) = &method.type_params {
            all_type_params.extend(method_tps.iter().map(|tp| tp.name.clone()));
        }

        // Collect bounds from both the impl's type params/where-clause and the
        // method's own where-clause. Impl-level bounds cover both inline
        // (`impl<T: Display>`) and where-clause (`impl<T> … where T: Display`)
        // shapes because `collect_type_param_bounds` reads both sources.
        let mut type_param_bounds =
            self.collect_type_param_bounds(impl_type_params, impl_where_clause);
        for (type_param, bounds) in self
            .collect_type_param_bounds(method.type_params.as_ref(), method.where_clause.as_ref())
        {
            let entry = type_param_bounds.entry(type_param).or_default();
            for bound in bounds {
                Self::push_unique_bound(entry, &bound);
            }
        }
        // Method where-clause may also constrain impl-level type params (e.g.
        // an additional bound on T added at the method level).
        for (type_param, bounds) in
            self.collect_type_param_bounds(impl_type_params, method.where_clause.as_ref())
        {
            let entry = type_param_bounds.entry(type_param).or_default();
            for bound in bounds {
                Self::push_unique_bound(entry, &bound);
            }
        }

        let sig = FnSig {
            type_params: all_type_params,
            type_param_bounds,
            param_names,
            params,
            return_type,
            is_async: method.is_async,
            is_pure: method.is_pure,
            ..FnSig::default()
        };
        if let Some(td) = self.lookup_type_def_mut(type_name) {
            td.methods.insert(method.name.clone(), sig.clone());
        }
        sig
    }

    pub(super) fn record_trait_impl(&mut self, type_name: &str, trait_name: &str) {
        self.trait_impls_set
            .insert((type_name.to_string(), trait_name.to_string()));
    }

    /// Canonical receiver key used by the primitive-and-builtin trait impl
    /// table.  Returns `Some(canonical)` for any receiver kind whose user
    /// trait impls cannot be hung off `type_defs`:
    ///
    /// * Primitives — keyed by `Ty::canonical_lowering_name()`.  This collapses
    ///   the user-facing alias set (`isize` → `i64`) so registration and
    ///   dispatch agree on a single key.  `int` and `Int` are no longer
    ///   accepted; the resolver hard-errors at the type-position lookup.
    /// * Compiler-builtin generics `Vec`, `HashMap`, `HashSet` — keyed by their
    ///   bare name; these already lack a `type_defs` entry that user impls can
    ///   attach methods to.
    ///
    /// Returns `None` for receivers that already flow through `type_defs`
    /// (user structs, actors, opaque handle types).
    #[must_use]
    pub(super) fn canonical_primitive_or_builtin_key(ty: &Ty) -> Option<String> {
        if let Some(canonical) = ty.canonical_lowering_name() {
            return Some(canonical.to_string());
        }
        if let Ty::Named {
            builtin: Some(builtin),
            ..
        } = ty
        {
            if builtin.is_collection() {
                return Some(builtin.canonical_name().to_string());
            }
        }
        None
    }

    /// Same as [`Self::canonical_primitive_or_builtin_key`] but accepts the
    /// raw type-name string seen at impl-block registration (e.g. `"int"`,
    /// `"string"`, `"Vec"`).  Returns `None` for names that aren't primitive
    /// aliases or compiler-builtin generics.
    #[must_use]
    pub(super) fn canonical_primitive_or_builtin_key_from_name(name: &str) -> Option<String> {
        if let Some(prim) = Ty::from_name(name) {
            return Self::canonical_primitive_or_builtin_key(&prim);
        }
        if crate::lookup_builtin_type(name).is_some_and(crate::BuiltinType::is_collection) {
            return Some(name.to_string());
        }
        None
    }

    /// Record an `impl <Trait> for <PrimitiveOrBuiltinGeneric>` method in the
    /// side table.  `canonical_key` must come from
    /// [`Self::canonical_primitive_or_builtin_key_from_name`] so registration
    /// and dispatch agree.
    pub(super) fn record_primitive_trait_impl_method(
        &mut self,
        canonical_key: String,
        trait_name: String,
        method_name: String,
        sig: FnSig,
    ) {
        self.primitive_trait_impls
            .entry((canonical_key, trait_name))
            .or_default()
            .insert(method_name, sig);
    }

    /// Look up a method on the primitive/builtin-generic impl table.
    ///
    /// Walks every trait registered for the receiver's canonical kind and
    /// returns the first method whose name matches.  Returns the resolved
    /// `FnSig` (receiver already filtered) plus the trait name that
    /// provided it, so callers can record dispatch metadata keyed on the
    /// resolved trait rather than re-deriving it from a name string.
    #[must_use]
    pub(super) fn lookup_primitive_trait_method(
        &self,
        receiver_ty: &Ty,
        method: &str,
    ) -> Option<(String, FnSig)> {
        let canonical = Self::canonical_primitive_or_builtin_key(receiver_ty)?;
        for ((rx_key, trait_name), methods) in &self.primitive_trait_impls {
            if rx_key != &canonical {
                continue;
            }
            if let Some(sig) = methods.get(method) {
                return Some((trait_name.clone(), sig.clone()));
            }
        }
        None
    }

    pub(super) fn register_receive_fn(&mut self, actor_name: &str, rf: &ReceiveFnDecl) {
        let mut generic_bindings = std::collections::HashMap::new();
        if let Some(type_params) = &rf.type_params {
            for tp in type_params {
                generic_bindings.insert(
                    tp.name.clone(),
                    Ty::Named {
                        builtin: None,
                        name: tp.name.clone(),
                        args: vec![],
                    },
                );
            }
        }
        if !generic_bindings.is_empty() {
            self.generic_ctx.push(generic_bindings);
        }

        let rf_scope = self.collect_type_param_scope_with_bounds(
            rf.type_params.as_ref(),
            rf.where_clause.as_ref(),
        );
        let pushed_rf_bounds = !rf_scope.is_empty();
        if pushed_rf_bounds {
            self.current_type_param_bounds.push(rf_scope);
        }

        let mut hole_vars = Vec::new();
        let param_names = rf.params.iter().map(|p| p.name.clone()).collect();
        let params = rf
            .params
            .iter()
            .map(|p| self.resolve_registered_annotation_ty(&p.ty, &mut hole_vars))
            .collect();
        let declared_return_type = rf.return_type.as_ref().map_or(Ty::Unit, |ret| {
            self.resolve_registered_annotation_ty(ret, &mut hole_vars)
        });
        let return_type = if rf.is_generator {
            Ty::stream(declared_return_type)
        } else {
            declared_return_type
        };

        if pushed_rf_bounds {
            self.current_type_param_bounds.pop();
        }
        if rf.type_params.as_ref().is_some_and(|tp| !tp.is_empty()) {
            self.generic_ctx.pop();
        }

        let type_param_bounds =
            self.collect_type_param_bounds(rf.type_params.as_ref(), rf.where_clause.as_ref());
        let sig = FnSig {
            type_params: rf.type_params.as_ref().map_or(vec![], |params| {
                params.iter().map(|p| p.name.clone()).collect()
            }),
            type_param_bounds,
            param_names,
            params,
            return_type,
            is_pure: rf.is_pure,
            ..FnSig::default()
        };

        let method_name = format!("{}::{}", actor_name, rf.name);
        if rf.is_generator {
            self.receive_generator_methods.insert(method_name.clone());
        }
        self.actor_receive_methods.insert(method_name.clone());
        self.record_fn_sig_inference_holes(&method_name, hole_vars);
        self.fn_sigs.insert(method_name, sig);
    }

    pub(super) fn register_extern_block(&mut self, eb: &ExternBlock) {
        // `extern "rt"` is the Hew-side declaration surface for JIT-visible
        // runtime functions. Validate each declared symbol against the `stable`
        // section of scripts/jit-symbol-classification.toml. Fail-closed: an
        // unclassified symbol is a hard error so the failure surfaces at check
        // time rather than at link time or (worse) silently routing to a wrong
        // runtime entry.
        //
        // `extern "C"` remains the raw user FFI surface (unsafe, no
        // validation). Other ABI strings are not yet defined by the language
        // and fall through to fn_sigs registration unchanged.
        if eb.abi == "rt" {
            let stable = jit_stable_symbols();
            for f in &eb.functions {
                if !stable.contains(f.name.as_str()) {
                    self.errors.push(TypeError {
                        severity: crate::error::Severity::Error,
                        kind: TypeErrorKind::ExternRtSymbolUnclassified {
                            symbol_name: f.name.clone(),
                            hint: format!(
                                "add `\"{}\"` to the `stable` list in \
                                 scripts/jit-symbol-classification.toml, \
                                 or use `extern \"C\"` for raw FFI symbols \
                                 that are not part of the Hew JIT runtime ABI",
                                f.name
                            ),
                        },
                        span: f.span.clone(),
                        message: format!(
                            "`extern \"rt\" fn {}` names a symbol not in the JIT \
                             runtime stable ABI — only symbols classified as `stable` \
                             in scripts/jit-symbol-classification.toml may appear in \
                             `extern \"rt\"` blocks",
                            f.name
                        ),
                        notes: vec![(
                            f.span.clone(),
                            "The `internal` classification covers lifecycle/shutdown \
                             symbols; `codegen-stable` covers compiler-emitted symbols \
                             (e.g. cooperate safepoints, actor-state locks). Neither \
                             may be named by user code in `extern \"rt\"` blocks."
                                .to_string(),
                        )],
                        suggestions: vec![format!(
                            "add `\"{}\"` to the `stable` list in \
                             scripts/jit-symbol-classification.toml",
                            f.name
                        )],
                        source_module: self.current_module.clone(),
                    });
                }
            }
        }

        for f in &eb.functions {
            let mut hole_vars = Vec::new();
            let param_names = f.params.iter().map(|p| p.name.clone()).collect();
            let params = f
                .params
                .iter()
                .map(|p| self.resolve_registered_annotation_ty(&p.ty, &mut hole_vars))
                .collect();
            let return_type = f.return_type.as_ref().map_or(Ty::Unit, |ret| {
                self.resolve_registered_annotation_ty(ret, &mut hole_vars)
            });
            let sig = FnSig {
                param_names,
                params,
                return_type,
                ..FnSig::default()
            };
            let key = scoped_module_item_name(self.current_module.as_deref(), &f.name)
                .unwrap_or_else(|| f.name.clone());
            self.record_fn_sig_inference_holes(&key, hole_vars);
            self.fn_sigs.insert(key.clone(), sig);
            self.unsafe_functions.insert(key);
        }

        // Register codegen-intercepted channel functions that use
        // out-parameter ABI and cannot appear in extern blocks.
        // Without these entries standalone `hew check` on channel.hew
        // reports "undefined function" for recv/try_recv calls.
        self.register_channel_recv_builtins();
    }

    /// Registers synthetic `fn_sigs` entries for channel `recv`/`try_recv`
    /// functions whose calling convention is handled entirely by codegen.
    ///
    /// These functions use an out-parameter ABI for `Option<T>` and must
    /// NOT be declared in `extern "C"` blocks — the codegen intercepts
    /// them by name and emits custom MLIR.  We register them here so the
    /// type checker can resolve calls inside `unsafe` blocks.
    ///
    /// Only activates when we're actually in the channel module: the
    /// local module must define `Receiver` AND the extern block must
    /// have already registered `hew_channel_send`.
    pub(super) fn register_channel_recv_builtins(&mut self) {
        let send_key = scoped_module_item_name(self.current_module.as_deref(), "hew_channel_send")
            .unwrap_or_else(|| "hew_channel_send".to_string());
        if !self.local_type_defs.contains("Receiver") || !self.fn_sigs.contains_key(&send_key) {
            return;
        }

        let receiver_ty = Ty::Named {
            builtin: None,
            name: "Receiver".to_string(),
            args: vec![],
        };

        let builtins: &[(&str, Ty)] = &[
            ("hew_channel_recv", Ty::option(Ty::String)),
            ("hew_channel_recv_int", Ty::option(Ty::I64)),
            ("hew_channel_try_recv", Ty::option(Ty::String)),
            ("hew_channel_try_recv_int", Ty::option(Ty::I64)),
        ];

        for (name, ret_ty) in builtins {
            let key = scoped_module_item_name(self.current_module.as_deref(), name)
                .unwrap_or_else(|| (*name).to_string());
            if self.fn_sigs.contains_key(&key) {
                continue;
            }
            let sig = FnSig {
                param_names: vec!["rx".to_string()],
                params: vec![receiver_ty.clone()],
                return_type: ret_ty.clone(),
                ..FnSig::default()
            };
            self.fn_sigs.insert(key.clone(), sig);
            self.unsafe_functions.insert(key);
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "import registration consolidates stdlib, user-module, and error paths in one place"
    )]
    pub(super) fn register_import(&mut self, decl: &ImportDecl, import_span: Option<&Span>) {
        let module_path = decl.path.join("::");

        // Try to load from the registry first, keeping any error detail owned so the
        // `self.module_registry` borrow ends before we mutate `self.errors`.
        let load_error_detail: Option<String> = match self.module_registry.load(&module_path) {
            Ok(info) => {
                if info.unsupported_type_signatures.is_empty() {
                    // Clone all data from ModuleInfo before mutating self, because
                    // info borrows from self.module_registry.
                    let functions = info.functions.clone();
                    let wrapper_fns = info.wrapper_fns.clone();
                    let clean_names = info.clean_names.clone();
                    let handle_types = info.handle_types.clone();
                    let drop_types = info.drop_types.clone();

                    let short = module_path
                        .rsplit("::")
                        .next()
                        .unwrap_or(&module_path)
                        .to_string();

                    // Register extern C function signatures
                    for func in functions {
                        let accepts_kwargs = module_path == "std::misc::log"
                            && Self::LOG_KWARGS_FUNCTIONS.contains(&func.name.as_str());
                        let sig = FnSig {
                            params: func.params,
                            return_type: func.return_type,
                            accepts_kwargs,
                            ..FnSig::default()
                        };
                        self.unsafe_functions.insert(func.name.clone());
                        self.fn_sigs.insert(func.name, sig);
                    }

                    // Register wrapper pub fn signatures
                    for wfn in wrapper_fns {
                        let accepts_kwargs = module_path == "std::misc::log"
                            && Self::LOG_KWARGS_FUNCTIONS.contains(&wfn.name.as_str());
                        let sig = FnSig {
                            params: wfn.params,
                            return_type: wfn.return_type,
                            accepts_kwargs,
                            ..FnSig::default()
                        };
                        self.fn_sigs.insert(wfn.name, sig);
                    }

                    // Register module and clean names
                    self.modules.insert(short.clone());
                    if let Some(span) = import_span {
                        self.import_spans.insert(
                            ImportKey::new(self.current_module.clone(), short.clone()),
                            (span.clone(), self.current_module.clone()),
                        );
                    }
                    for (method, c_symbol) in &clean_names {
                        // Prefer the wrapper function's own signature (registered under
                        // the method name) over the extern C function's signature.
                        // E.g. `log.setup()` should have 0 params (the wrapper's sig),
                        // not 1 param (the extern `hew_log_set_level(level)` sig).
                        let wrapper_sig = self.fn_sigs.get(method.as_str()).cloned();
                        let sig = wrapper_sig
                            .clone()
                            .or_else(|| self.fn_sigs.get(c_symbol.as_str()).cloned());
                        if let Some(sig) = sig {
                            let key = format!("{short}.{method}");
                            self.module_fn_exports.insert(key.clone());
                            self.fn_sigs.insert(key.clone(), sig);
                            if wrapper_sig.is_none() {
                                self.unsafe_functions.insert(key);
                            }
                        }
                    }

                    // Register handle type names so they can be used in type annotations
                    for type_name in &handle_types {
                        self.known_types.insert(type_name.clone());
                    }

                    // Populate TraitRegistry with handle/drop types
                    for ht in &handle_types {
                        self.registry.register_handle_type(ht.clone());
                    }
                    for dt in &drop_types {
                        self.registry.register_drop_type(dt.clone());
                    }

                    // Process resolved Hew source items from stdlib modules that ship
                    // alongside their C/Rust bindings so trait methods stay visible.
                    if let Some(ref resolved_items) = decl.resolved_items {
                        if !self.stdlib_hew_source_already_registered(decl, &module_path) {
                            self.register_stdlib_hew_items(&short, resolved_items);
                        }
                    }

                    // `std::io::closable` is a pure-Hew trait module with no C
                    // bindings.  The normal `resolved_items` path only fires for
                    // modules whose items were pre-parsed in a module graph; for
                    // inline programs we use the embedded source instead.  Parsing
                    // it here registers `Closable` in `trait_defs` and `CloseError`
                    // in `type_defs`.  The `register_stdlib_hew_items` loop then
                    // fires the `tr.name == "Closable"` arm which wires
                    // `"Closable::close"` into `consume_receiver_methods`.
                    if module_path == "std::io::closable" && decl.resolved_items.is_none() {
                        let identity = format!("module:{module_path}");
                        if !self
                            .registered_stdlib_hew_sources
                            .contains(identity.as_str())
                        {
                            self.registered_stdlib_hew_sources.insert(identity);
                            let parsed = hew_parser::parse(CLOSABLE_HEW);
                            debug_assert!(
                                parsed.errors.is_empty(),
                                "std/io/closable.hew failed to parse: {:?}",
                                parsed.errors,
                            );
                            if parsed.errors.is_empty() {
                                let items: Vec<_> = parsed.program.items.into_iter().collect();
                                self.register_stdlib_hew_items(&short, &items);
                            }
                        }
                    }

                    self.handle_bearing_dirty = true;
                    return;
                }
                Some(format!(
                    "module file contains unsupported slice annotations in signature(s): {}. \
                     MLIR lowering cannot lower slice types yet",
                    info.unsupported_type_signatures.join(", ")
                ))
            }
            Err(ModuleError::NotFound { .. }) => {
                Some("module not found in any search path".to_string())
            }
            Err(ModuleError::ParseError {
                ref file_path,
                line,
                column,
                ref message,
                ..
            }) => Some(format!(
                "module file `{}` has parse error at {line}:{column}: {message}",
                file_path.display()
            )),
        };
        // `self.module_registry` borrow is released here.

        // --- User module path ---
        if let Some(ref resolved_items) = decl.resolved_items {
            if decl.path.is_empty() {
                if self.flat_file_import_already_registered(decl) {
                    return;
                }
                // File imports register top-level names without a module namespace.
                self.register_file_import_items(resolved_items);
            } else {
                let short = decl.path.last().expect("import path is non-empty").clone();
                self.modules.insert(short.clone());
                self.user_modules.insert(short.clone());
                if let Some(span) = import_span {
                    self.import_spans.insert(
                        ImportKey::new(self.current_module.clone(), short.clone()),
                        (span.clone(), self.current_module.clone()),
                    );
                }
                // Dedup pure-Hew modules (e.g. `std::fs`) that may be transitively
                // imported by multiple stdlib sub-modules.  Without this guard,
                // each referring module's `ImportDecl` carries its own
                // `resolved_items` copy and `register_user_module` would register
                // types like `IoError` once per importer, triggering duplicate-
                // definition errors.  The `registered_stdlib_hew_sources` set tracks
                // by canonical `module_path` so all `import std::fs` ImportDecls
                // collapse to the same key.
                if !self.stdlib_hew_source_already_registered(decl, &module_path) {
                    self.register_user_module(&short, resolved_items, &decl.spec);
                }
            }
        } else if let Some(error) =
            Self::unresolved_import_error(decl, import_span, &module_path, load_error_detail)
        {
            self.errors.push(error);
        }
    }

    fn stdlib_hew_source_identity(decl: &ImportDecl, module_path: &str) -> String {
        // Always prefer the canonical module-path key when available so that
        // multiple ImportDecl objects for the same stdlib module (e.g. `import
        // std::fs` appearing in quic.hew, tls.hew, and the user file) all hash
        // to the same identity string even when only some of them have a
        // resolved_source_paths populated.  Using the file path as the primary
        // key produces two different strings for the same logical module and
        // defeats the registered_stdlib_hew_sources dedup guard.
        if module_path.is_empty() {
            decl.resolved_source_paths.first().map_or_else(
                || String::from("module:"),
                |p| format!("path:{}", p.display()),
            )
        } else {
            format!("module:{module_path}")
        }
    }

    fn unresolved_import_error(
        decl: &ImportDecl,
        import_span: Option<&Span>,
        module_path: &str,
        load_error_detail: Option<String>,
    ) -> Option<TypeError> {
        let detail = if decl.path.is_empty() {
            Some("file import was not resolved before type checking".to_string())
        } else {
            load_error_detail
        }?;
        let span = import_span.cloned().unwrap_or(0..0);
        let import_target = if decl.path.is_empty() {
            decl.file_path.as_deref().unwrap_or("<file import>")
        } else {
            module_path
        };
        Some(TypeError::unresolved_import(span, import_target, &detail))
    }

    /// Determine whether a name should be imported unqualified based on the `ImportSpec`.
    #[expect(clippy::ref_option, reason = "avoids cloning the option contents")]
    pub(super) fn should_import_name(name: &str, spec: &Option<ImportSpec>) -> bool {
        match spec {
            None => false,                  // bare import → qualified only
            Some(ImportSpec::Glob) => true, // import foo::*; → everything
            Some(ImportSpec::Names(names)) => names
                .iter()
                .any(|n| n.name == name || n.alias.as_deref() == Some(name)),
        }
    }

    /// Resolve the binding name for an imported symbol, applying any alias.
    #[expect(clippy::ref_option, reason = "avoids cloning the option contents")]
    pub(super) fn resolve_import_name(spec: &Option<ImportSpec>, name: &str) -> Option<String> {
        match spec {
            Some(ImportSpec::Names(names)) => names
                .iter()
                .find(|n| n.name == name)
                .map(|n| n.alias.as_deref().unwrap_or(&n.name).to_string()),
            Some(ImportSpec::Glob) => Some(name.to_string()),
            None => None,
        }
    }

    /// Register type declarations, trait declarations, and impl blocks from
    /// stdlib modules that have Hew source files. This makes trait methods
    /// (e.g. bench.Suite.add) visible to the type checker.
    #[expect(
        clippy::too_many_lines,
        reason = "three-pass registration loop with local_type_defs scoping"
    )]
    pub(super) fn register_stdlib_hew_items(
        &mut self,
        module_short: &str,
        items: &[Spanned<Item>],
    ) {
        // Temporarily scope local_type_defs so that locally_non_generic in
        // resolve_type_expr suppresses fresh-var injection for opaque handle
        // types (e.g. Sender, Receiver) declared in this module.  Without
        // this, impl-method signatures resolved here would get Sender<?T>
        // while the same signatures registered during collect_functions
        // (module_graph traversal) use bare Sender — causing a type mismatch
        // when body-checking the non-root module.
        let saved_local_type_defs = self.local_type_defs.clone();
        let saved_source_type_defs = self.source_type_defs.clone();
        for (item, _) in items {
            match item {
                Item::TypeDecl(td) => {
                    self.local_type_defs.insert(td.name.clone());
                    self.source_type_defs.insert(td.name.clone());
                }
                Item::Machine(md) => {
                    self.local_type_defs.insert(md.name.clone());
                    self.source_type_defs.insert(md.name.clone());
                    let event_type_name = format!("{}Event", md.name);
                    self.local_type_defs.insert(event_type_name.clone());
                    self.source_type_defs.insert(event_type_name);
                }
                _ => {}
            }
        }

        // Pass 1: Register types, traits, and functions first
        for (item, span) in items {
            match item {
                Item::TypeDecl(td) => {
                    if !td.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_type_namespace_name(&td.name, span) {
                        continue;
                    }
                    self.register_type_decl(td);
                    self.known_types.insert(td.name.clone());
                }
                Item::Machine(md) => {
                    if !md.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_machine_type_namespace_names(&md.name, span) {
                        continue;
                    }
                    self.register_machine_decl(md);
                    self.known_types.insert(md.name.clone());
                    self.known_types.insert(format!("{}Event", md.name));
                }
                Item::Trait(tr) => {
                    if !tr.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_type_namespace_name(&tr.name, span) {
                        continue;
                    }
                    let info = Self::trait_info_from_decl(tr);
                    self.trait_defs.insert(tr.name.clone(), info.clone());
                    let qualified = format!("{module_short}.{}", tr.name);
                    self.trait_defs.insert(qualified, info);
                    // When the stdlib `Closable` trait is registered, wire its
                    // `close` method into the consume-receiver set so the
                    // move-checker marks the receiver moved at every call site.
                    // This must happen at trait-load time (not Checker::new) so
                    // programs that never import std::io::closable do not see
                    // phantom consume markers.
                    if tr.name == "Closable" {
                        self.consume_receiver_methods
                            .insert("Closable::close".to_string());
                    }
                }
                Item::Function(fd) => {
                    if !fd.visibility.is_pub() {
                        continue;
                    }
                    let qualified = format!("{module_short}.{}", fd.name);
                    if !self.fn_sigs.contains_key(&qualified) {
                        let sig = self.build_fn_sig_from_decl(fd);
                        self.module_fn_exports.insert(qualified.clone());
                        self.fn_sigs.insert(qualified, sig);
                    }
                }
                Item::Actor(ad) => {
                    if !self.register_type_namespace_name(&ad.name, span) {
                        continue;
                    }
                    self.register_actor_base(ad);
                }
                _ => {}
            }
        }
        // Pass 2: Register impl methods (after types exist)
        for (item, span) in items {
            if let Item::Impl(id) = item {
                if let TypeExpr::Named {
                    name: type_name,
                    type_args,
                } = &id.target_type.0
                {
                    // Set current_self_type for resolving `Self` in method parameters
                    let prev_self_type = self.current_self_type.take();
                    let self_type_args = type_args
                        .as_ref()
                        .map(|args| {
                            args.iter()
                                .map(|type_arg| self.resolve_type_expr(type_arg))
                                .collect()
                        })
                        .unwrap_or_default();
                    self.current_self_type = Some((type_name.clone(), self_type_args));
                    let scope_pushed =
                        self.enter_impl_scope(id, span, Some(type_name.as_str()), false);

                    let primitive_key = id.trait_bound.as_ref().and_then(|_| {
                        Self::canonical_primitive_or_builtin_key_from_name(type_name)
                    });
                    for method in &id.methods {
                        let sig = self.register_impl_method(
                            type_name,
                            method,
                            id.type_params.as_ref(),
                            id.where_clause.as_ref(),
                        );
                        // Also register on qualified type name
                        let qualified_type = format!("{module_short}.{type_name}");
                        if let Some(td) = self.lookup_type_def_mut(&qualified_type) {
                            td.methods.insert(method.name.clone(), sig.clone());
                        }
                        if let (Some(canonical), Some(tb)) =
                            (primitive_key.clone(), id.trait_bound.as_ref())
                        {
                            self.record_primitive_trait_impl_method(
                                canonical,
                                tb.name.clone(),
                                method.name.clone(),
                                sig,
                            );
                        }
                    }
                    if let Some(tb) = &id.trait_bound {
                        self.record_trait_impl(type_name, &tb.name);
                    }

                    // Restore previous self type
                    self.current_self_type = prev_self_type;
                    if scope_pushed {
                        self.exit_impl_scope();
                    }
                }
            }
        }
        // Pass 3: Create qualified type aliases (after impls have been registered)
        for (item, _span) in items {
            match item {
                Item::TypeDecl(td) => {
                    if !td.visibility.is_pub() {
                        continue;
                    }
                    self.register_qualified_type_alias(module_short, &td.name);
                    self.record_module_type_export(module_short, &td.name);
                }
                Item::Machine(md) => {
                    if !md.visibility.is_pub() {
                        continue;
                    }
                    self.register_qualified_type_alias(module_short, &md.name);
                    self.register_qualified_type_alias(module_short, &format!("{}Event", md.name));
                }
                Item::Actor(ad) => {
                    self.register_qualified_type_alias(module_short, &ad.name);
                    self.record_module_type_export(module_short, &ad.name);
                }
                _ => {}
            }
        }
        self.local_type_defs = saved_local_type_defs;
        self.source_type_defs = saved_source_type_defs;
    }

    /// Register items from a file-based import as top-level names (no module namespace).
    #[expect(
        clippy::too_many_lines,
        reason = "single-pass walk over every Item variant with parallel registration paths"
    )]
    pub(super) fn register_file_import_items(&mut self, items: &[Spanned<Item>]) {
        let mut current_import_pub_spans = HashMap::new();
        let mut skipped_type_names = HashSet::new();

        for (item, span) in items {
            match item {
                Item::Function(fd) => {
                    if !fd.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_flat_file_import_pub_name(
                        &mut current_import_pub_spans,
                        &fd.name,
                        span,
                    ) {
                        continue;
                    }
                    let sig = self.build_fn_sig_from_decl(fd);
                    self.fn_sigs.insert(fd.name.clone(), sig);
                }
                Item::Const(cd) => {
                    if !cd.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_flat_file_import_pub_name(
                        &mut current_import_pub_spans,
                        &cd.name,
                        span,
                    ) {
                        continue;
                    }
                    let ty = self.resolve_registered_annotation_ty_no_holes(&cd.ty);
                    self.env.define(cd.name.clone(), ty, false);
                }
                Item::TypeDecl(td) => {
                    if !td.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_flat_file_import_type_name(
                        &mut current_import_pub_spans,
                        &td.name,
                        span,
                    ) {
                        skipped_type_names.insert(td.name.clone());
                        continue;
                    }
                    self.register_type_decl(td);
                    self.known_types.insert(td.name.clone());
                }
                Item::Machine(md) => {
                    if !md.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_flat_file_import_type_name(
                        &mut current_import_pub_spans,
                        &md.name,
                        span,
                    ) {
                        skipped_type_names.insert(md.name.clone());
                        continue;
                    }
                    let event_type_name = format!("{}Event", md.name);
                    if !self.register_flat_file_import_type_name(
                        &mut current_import_pub_spans,
                        &event_type_name,
                        span,
                    ) {
                        skipped_type_names.insert(event_type_name);
                        continue;
                    }
                    self.register_machine_decl(md);
                    self.known_types.insert(md.name.clone());
                    self.known_types.insert(format!("{}Event", md.name));
                }
                Item::Trait(tr) => {
                    if !tr.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_flat_file_import_type_name(
                        &mut current_import_pub_spans,
                        &tr.name,
                        span,
                    ) {
                        continue;
                    }
                    let info = Self::trait_info_from_decl(tr);
                    self.trait_defs.insert(tr.name.clone(), info);
                }
                Item::Actor(ad) => {
                    if !ad.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_flat_file_import_type_name(
                        &mut current_import_pub_spans,
                        &ad.name,
                        span,
                    ) {
                        continue;
                    }
                    self.register_actor_base(ad);
                }
                Item::Impl(id) => {
                    if let TypeExpr::Named {
                        name: type_name, ..
                    } = &id.target_type.0
                    {
                        if skipped_type_names.contains(type_name) {
                            continue;
                        }
                        let primitive_key = id.trait_bound.as_ref().and_then(|_| {
                            Self::canonical_primitive_or_builtin_key_from_name(type_name)
                        });
                        for method in &id.methods {
                            if !method.visibility.is_pub() {
                                continue;
                            }
                            let sig = self.register_impl_method(
                                type_name,
                                method,
                                id.type_params.as_ref(),
                                id.where_clause.as_ref(),
                            );
                            if let (Some(canonical), Some(tb)) =
                                (primitive_key.clone(), id.trait_bound.as_ref())
                            {
                                self.record_primitive_trait_impl_method(
                                    canonical,
                                    tb.name.clone(),
                                    method.name.clone(),
                                    sig,
                                );
                            }
                        }
                        // Track trait implementations
                        if let Some(tb) = &id.trait_bound {
                            self.record_trait_impl(type_name, &tb.name);
                        }
                    }
                }
                _ => {}
            }
        }

        self.flat_file_import_pub_spans
            .extend(current_import_pub_spans);
    }

    pub(super) fn register_flat_file_import_pub_name(
        &mut self,
        current_import_pub_spans: &mut HashMap<String, Span>,
        name: &str,
        span: &Span,
    ) -> bool {
        if let Some(prev_span) = self
            .flat_file_import_pub_spans
            .get(name)
            .cloned()
            .or_else(|| current_import_pub_spans.get(name).cloned())
        {
            self.errors.push(TypeError::duplicate_definition(
                span.clone(),
                name,
                prev_span,
            ));
            return false;
        }

        match current_import_pub_spans.entry(name.to_string()) {
            Entry::Occupied(_) => {}
            Entry::Vacant(entry) => {
                entry.insert(span.clone());
            }
        }

        true
    }

    pub(super) fn register_flat_file_import_type_name(
        &mut self,
        current_import_pub_spans: &mut HashMap<String, Span>,
        name: &str,
        span: &Span,
    ) -> bool {
        self.register_flat_file_import_pub_name(current_import_pub_spans, name, span)
            && self.register_type_namespace_name(name, span)
    }

    fn flat_file_import_already_registered(&mut self, decl: &ImportDecl) -> bool {
        let import_source = decl
            .resolved_source_paths
            .first()
            .cloned()
            .or_else(|| decl.file_path.as_ref().map(std::path::PathBuf::from));
        let Some(import_source) = import_source else {
            return false;
        };
        !self
            .registered_flat_file_import_sources
            .insert(import_source)
    }

    fn stdlib_hew_source_already_registered(
        &mut self,
        decl: &ImportDecl,
        module_path: &str,
    ) -> bool {
        !self
            .registered_stdlib_hew_sources
            .insert(Self::stdlib_hew_source_identity(decl, module_path))
    }

    /// Register items from a user module under the module's namespace.
    #[expect(
        clippy::too_many_lines,
        clippy::ref_option,
        reason = "statement type checking requires many cases"
    )]
    pub(super) fn register_user_module(
        &mut self,
        module_short: &str,
        items: &[Spanned<Item>],
        spec: &Option<ImportSpec>,
    ) {
        // Temporarily scope local_type_defs so that locally_non_generic
        // suppresses fresh-var injection for handle types defined in this
        // module, matching the resolution context used during collect_functions.
        let saved_local_type_defs = self.local_type_defs.clone();
        let saved_source_type_defs = self.source_type_defs.clone();
        for (item, _) in items {
            match item {
                Item::TypeDecl(td) => {
                    self.local_type_defs.insert(td.name.clone());
                    self.source_type_defs.insert(td.name.clone());
                }
                Item::Machine(md) => {
                    self.local_type_defs.insert(md.name.clone());
                    self.source_type_defs.insert(md.name.clone());
                    let event_type_name = format!("{}Event", md.name);
                    self.local_type_defs.insert(event_type_name.clone());
                    self.source_type_defs.insert(event_type_name);
                }
                _ => {}
            }
        }

        for (item, span) in items {
            match item {
                Item::Function(fd) => {
                    // Skip non-pub functions (enforce visibility)
                    if !fd.visibility.is_pub() {
                        continue;
                    }

                    let sig = self.build_fn_sig_from_decl(fd);
                    let qualified = format!("{module_short}.{}", fd.name);
                    self.module_fn_exports.insert(qualified.clone());
                    self.fn_sigs.insert(qualified, sig.clone());

                    // If named import or glob, also register unqualified (using alias if present)
                    if Self::should_import_name(&fd.name, spec) {
                        let binding_name = Self::resolve_import_name(spec, &fd.name)
                            .unwrap_or_else(|| fd.name.clone());
                        self.fn_sigs.insert(binding_name.clone(), sig);
                        self.unqualified_to_module.insert(
                            (self.current_module.clone(), binding_name),
                            module_short.to_string(),
                        );
                    }
                }
                Item::TypeDecl(td) => {
                    if !td.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_type_namespace_name(&td.name, span) {
                        continue;
                    }
                    self.register_type_decl(td);
                    self.register_qualified_type_alias(module_short, &td.name);
                    self.record_module_type_export(module_short, &td.name);
                    self.known_types.insert(td.name.clone());
                }
                Item::Machine(md) => {
                    if !md.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_machine_type_namespace_names(&md.name, span) {
                        continue;
                    }
                    self.register_machine_decl(md);
                    self.register_qualified_type_alias(module_short, &md.name);
                    self.register_qualified_type_alias(module_short, &format!("{}Event", md.name));
                    self.known_types.insert(md.name.clone());
                    self.known_types.insert(format!("{}Event", md.name));
                }
                Item::Trait(tr) => {
                    if !tr.visibility.is_pub() {
                        continue;
                    }
                    let info = Self::trait_info_from_decl(tr);
                    let import_binding = if Self::should_import_name(&tr.name, spec) {
                        let binding_name = Self::resolve_import_name(spec, &tr.name)
                            .unwrap_or_else(|| tr.name.clone());
                        if self.register_type_namespace_name(&binding_name, span) {
                            Some(binding_name)
                        } else {
                            None
                        }
                    } else {
                        None
                    };

                    // Register under qualified name (e.g. "mymod.Drawable")
                    let qualified = format!("{module_short}.{}", tr.name);
                    self.trait_defs.insert(qualified.clone(), info.clone());

                    // Record super-trait relationships for both qualified and unqualified
                    if let Some(supers) = &tr.super_traits {
                        let super_names: Vec<String> =
                            supers.iter().map(|s| s.name.clone()).collect();
                        self.trait_super.insert(qualified, super_names.clone());
                        if let Some(binding_name) = import_binding.as_ref() {
                            self.trait_super.insert(binding_name.clone(), super_names);
                        }
                    }

                    // If glob or named import, also register unqualified (using alias if present)
                    if let Some(binding_name) = import_binding {
                        self.trait_defs.insert(binding_name.clone(), info.clone());
                        self.unqualified_to_module.insert(
                            (self.current_module.clone(), binding_name),
                            module_short.to_string(),
                        );
                    }
                }
                Item::Const(cd) => {
                    if !cd.visibility.is_pub() {
                        continue;
                    }
                    let ty = self.resolve_registered_annotation_ty_no_holes(&cd.ty);
                    let qualified = format!("{module_short}.{}", cd.name);
                    self.env.define(qualified, ty.clone(), false);
                    if Self::should_import_name(&cd.name, spec) {
                        let binding_name = Self::resolve_import_name(spec, &cd.name)
                            .unwrap_or_else(|| cd.name.clone());
                        self.env.define(binding_name, ty, false);
                    }
                }
                Item::Impl(id) => {
                    // Register impl methods for types defined in the module
                    if let TypeExpr::Named {
                        name: type_name,
                        type_args,
                    } = &id.target_type.0
                    {
                        // Set current_self_type for resolving `Self` in method parameters
                        let prev_self_type = self.current_self_type.take();
                        let self_type_args = type_args
                            .as_ref()
                            .map(|args| {
                                args.iter()
                                    .map(|type_arg| self.resolve_type_expr(type_arg))
                                    .collect()
                            })
                            .unwrap_or_default();
                        self.current_self_type = Some((type_name.clone(), self_type_args));
                        let scope_pushed =
                            self.enter_impl_scope(id, span, Some(type_name.as_str()), false);

                        let primitive_key = id.trait_bound.as_ref().and_then(|_| {
                            Self::canonical_primitive_or_builtin_key_from_name(type_name)
                        });
                        for method in &id.methods {
                            if !method.visibility.is_pub() {
                                continue;
                            }
                            let sig = self.register_impl_method(
                                type_name,
                                method,
                                id.type_params.as_ref(),
                                id.where_clause.as_ref(),
                            );
                            if let (Some(canonical), Some(tb)) =
                                (primitive_key.clone(), id.trait_bound.as_ref())
                            {
                                self.record_primitive_trait_impl_method(
                                    canonical,
                                    tb.name.clone(),
                                    method.name.clone(),
                                    sig,
                                );
                            }
                        }
                        if let Some(tb) = &id.trait_bound {
                            self.record_trait_impl(type_name, &tb.name);
                        }

                        // Restore previous self type
                        self.current_self_type = prev_self_type;
                        if scope_pushed {
                            self.exit_impl_scope();
                        }
                    }
                }
                Item::Actor(ad) => {
                    if !self.register_type_namespace_name(&ad.name, span) {
                        continue;
                    }
                    self.register_actor_base(ad);
                    self.register_qualified_type_alias(module_short, &ad.name);
                    self.record_module_type_export(module_short, &ad.name);
                    // If named import or glob, also register unqualified
                    if Self::should_import_name(&ad.name, spec) {
                        let binding_name = Self::resolve_import_name(spec, &ad.name)
                            .unwrap_or_else(|| ad.name.clone());
                        self.unqualified_to_module.insert(
                            (self.current_module.clone(), binding_name),
                            module_short.to_string(),
                        );
                    }
                }
                _ => {}
            }
        }
        self.local_type_defs = saved_local_type_defs;
        self.source_type_defs = saved_source_type_defs;
    }

    /// Build a `FnSig` from a function declaration (used for user module registration).
    pub(super) fn build_fn_sig_from_decl(&mut self, fd: &FnDecl) -> FnSig {
        let param_names = fd.params.iter().map(|p| p.name.clone()).collect();
        let params = fd
            .params
            .iter()
            .map(|p| self.resolve_registered_annotation_ty_no_holes(&p.ty))
            .collect();
        let declared_return = fd.return_type.as_ref().map_or(Ty::Unit, |ret| {
            self.resolve_registered_annotation_ty_no_holes(ret)
        });
        let type_params = fd.type_params.as_ref().map_or(vec![], |params| {
            params.iter().map(|p| p.name.clone()).collect()
        });
        let type_param_bounds =
            self.collect_type_param_bounds(fd.type_params.as_ref(), fd.where_clause.as_ref());
        let return_type = if fd.is_generator && fd.is_async {
            Ty::async_generator(declared_return)
        } else if fd.is_generator {
            Ty::generator(declared_return, Ty::Unit)
        } else {
            declared_return
        };
        FnSig {
            type_params,
            type_param_bounds,
            param_names,
            params,
            return_type,
            is_async: fd.is_async,
            is_pure: fd.is_pure,
            doc_comment: fd.doc_comment.clone(),
            ..FnSig::default()
        }
    }

    /// Register an actor's core items: the type declaration, receive functions,
    /// and inline methods.  This block is identical across all three import
    /// registration paths; only the qualified-alias and unqualified-binding
    /// steps differ and are therefore kept in each caller.
    pub(super) fn register_actor_base(&mut self, ad: &ActorDecl) {
        self.register_actor_decl(ad);
        self.known_types.insert(ad.name.clone());
        for rf in &ad.receive_fns {
            self.register_receive_fn(&ad.name, rf);
        }
        for method in &ad.methods {
            let method_name = format!("{}::{}", ad.name, method.name);
            self.register_fn_sig_with_name(&method_name, method);
        }
    }

    /// Insert a qualified alias (`module_short.Name`) for a type that has
    /// already been registered under its bare name.
    pub(super) fn register_qualified_type_alias(&mut self, module_short: &str, name: &str) {
        let qualified = format!("{module_short}.{name}");
        if let Some(def) = self.type_defs.get(name).cloned() {
            self.type_defs.insert(qualified.clone(), def);
            if let Some(span) = self.type_def_spans.get(name).cloned() {
                self.type_def_spans.insert(qualified, span);
            }
            self.handle_bearing_dirty = true;
        }
    }

    /// Record that an imported module exports a type/actor name.
    ///
    /// Mirrors the `module_fn_exports` precedent (`register_builtin_sig` /
    /// `register_user_module` `Item::Function` arm) for type names.  Drives the
    /// module-qualified value-constructor pre-dispatch in `check_field_access`
    /// and `check_struct_init` so we can emit a precise "module `m` has no
    /// exported type `T`" diagnostic instead of leaking through to the
    /// "undefined variable" / "undefined type" fallbacks.
    pub(super) fn record_module_type_export(&mut self, module_short: &str, name: &str) {
        self.module_type_exports
            .entry(module_short.to_string())
            .or_default()
            .insert(name.to_string());
    }
}
