#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;
use crate::BuiltinType;
use hew_parser::ast::WireMetadata;

/// Whether a stdlib Hew-source registration publishes its types' bare names
/// into the importer's scope, passed to `register_stdlib_hew_items`.
///
/// `Prelude` is the compiled-in bootstrap path (`builtins`, `closable`,
/// `link_monitor`, the receiver-impl surfaces, …): these are genuine
/// always-in-scope prelude surfaces with no user `import` statement, so their
/// bare names are published unconditionally.
///
/// `Import(spec)` is a real `import std::…` of a C-backed stdlib module that
/// also ships Hew source. It obeys the same qualified-by-default gate as a
/// user-package import: a plain `import` (spec `None`) publishes only the
/// qualified name, and a named/glob/aliased import publishes the bare (or
/// aliased) binding. This closes the asymmetry where a plain stdlib import
/// exposed `Server` bare while the equivalent user-package import rejected it.
#[derive(Clone, Copy)]
pub(super) enum StdlibBarePublication<'a> {
    Prelude,
    Import(&'a Option<ImportSpec>),
}

impl StdlibBarePublication<'_> {
    /// The bare binding name to publish for `name`, or `None` if this
    /// registration does not publish it unqualified. `Prelude` always
    /// publishes the bare name as-is; `Import(spec)` publishes only when the
    /// spec opts the name in (named/glob/aliased), applying any alias — the
    /// same gate `register_user_module` uses via `should_import_name` /
    /// `resolve_import_name`.
    fn bare_binding(self, name: &str) -> Option<String> {
        match self {
            Self::Prelude => Some(name.to_string()),
            Self::Import(spec) => Checker::should_import_name(name, spec).then(|| {
                Checker::resolve_import_name(spec, name).unwrap_or_else(|| name.to_string())
            }),
        }
    }
}

/// A trait reference (`impl <Trait> for ...`) resolved to its OWNER-QUALIFIED
/// identity, so trait conformance never keys off the bare `Trait::method` name
/// (which is first-write-wins and polluted under same-name collisions). Produced
/// by `resolve_trait_conformance_identity`.
pub(super) struct ResolvedTraitIdentity {
    /// The trait's defining module (`Some("srccollidea")` for an
    /// aliased/imported-bare/single-owner-import trait), or `None` for a
    /// local/root trait or a reference that did not resolve to a single owner.
    owner: Option<String>,
    /// The trait's source name (the name as declared in its defining module,
    /// recovering through an alias: `import m::{ Trait as C }` resolves `C` to
    /// source name `Trait`).
    source_trait_name: String,
    /// `true` when the reference binds to a LOCAL trait declaration, which
    /// shadows any imported same-name trait. Callers source the required-method
    /// set and signatures from the local `TraitInfo`, never the polluted bare
    /// `fn_sigs` key.
    is_local: bool,
}

/// The scope a trait name is resolved in. A primary trait / bound is spelled in
/// the importing program (`Current`); a SUPERTRAIT edge is spelled inside the
/// declaring module (`Declaring`) and must follow that module's import bindings
/// (the re-export chain), never the importer's same-name trait. See
/// `resolve_trait_ref`.
#[derive(Clone, Copy)]
pub(super) enum TraitRefScope<'a> {
    Current,
    Declaring { module: &'a str },
}

/// Context for canonicalizing trait-vs-impl signature types to a single
/// defining-module-qualified identity before comparison
/// (`check_impl_method_against_trait`). Carries only borrowed predicates so the
/// recursion holds no `&self` borrow across the later mutable error reporting.
struct TraitSigCanonCtx<'a> {
    /// In-scope module short names; a `module.Name` whose `module` is here is an
    /// explicit, unambiguous identity that keeps its qualifier.
    modules: &'a std::collections::HashSet<String>,
    /// The trait's defining module, used to qualify a bare type name written in
    /// the trait declaration. `None` for a root/local trait.
    trait_owner: Option<&'a str>,
    /// Whether a `{owner}.{bare}` spelling is a registered type def.
    defines_qualified: &'a dyn Fn(&str) -> bool,
    /// Whether a bare name shadows one of the impl scope's own types (then it
    /// keeps the bare identity rather than being qualified to the trait owner).
    is_local: &'a dyn Fn(&str) -> bool,
}

/// Embedded source for `std/io/closable.hew`.
///
/// Parsed at import-registration time for `std::io::closable` so the
/// `Closable` trait and `CloseError` enum are visible in the checker even
/// in programs that were not loaded through the module-graph path (e.g.
/// inline programs in tests).
const CLOSABLE_HEW: &str = include_str!("../../../std/io/closable.hew");

/// Embedded source for `std/concurrency/lambda_actor.hew`.
///
/// Like `std::io::closable`, this is a pure-Hew stdlib surface whose
/// methods must be visible to inline typechecker tests even when the
/// module graph did not pre-populate `resolved_items`.
const LAMBDA_ACTOR_HEW: &str = include_str!("../../../std/concurrency/lambda_actor.hew");

/// Embedded source for the built-in actor monitor handle wrapper.
///
/// This copy intentionally mirrors the on-disk stdlib module without imports:
/// inline checker tests do not flow through the stdlib resolver before this
/// prelude surface is registered.
const MONITOR_REF_HEW: &str = r#"
#[resource]
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

pub enum MonitorError {
    Partition;
    StaleRef;
    LocalShutdown;
    VersionMismatch;
    Unauthorized;
    DecodeFailure;
    MonitorLost;
}

pub enum PartitionPolicy {
    FailFast;
    Deadline;
    MonitorLost;
    CrashLinked;
    Quarantine;
}

impl MonitorRef {
    fn close(monitor_ref: MonitorRef) {
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
    message: string;
}

pub enum CrashAction {
    Restart;
    Escalate;
    Kill;
}

pub type CrashNotification {
    actor_id: u64;
    kind: CrashKind;
}

pub enum CrashKind {
    Crashed;
    HeapExceeded;
    PartitionDetected;
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
    Partition;
    Timeout;
    StaleRef;
    Cancelled;
    LocalShutdown;
    VersionMismatch;
    Unauthorized;
}
";

/// Stdlib-floor modules permitted to DECLARE `#[intrinsic("…")]` functions.
///
/// A605 (ratified): the `#[intrinsic]` surface is compiler-internal-only — no
/// user-reachable module may declare an intrinsic. Each entry is a `.`-joined
/// module path matched against the checker's `current_module`
/// (e.g. `ModuleId { path: ["std", "math"] }` → `"std.math"`).
///
/// The list is an explicit, enumerated allowlist (not a prefix match): a module
/// is a floor module only if its full dotted path is present here. This is the
/// security boundary's authority — keep it as small as the floor requires.
///
/// Current members:
/// - `std.math` — the math intrinsics (`exp`/`log`/`sqrt`/…) are declared as
///   typed `#[intrinsic("math.*")]` stubs in `std/math/math.hew`; the catalog
///   supplies the lowering. NOTE (W5.005): the `math.*` rows use
///   `CompilerIntrinsic` linkage and route through builtin method-rewrites —
///   they are NOT emitted as callable floor functions and a direct
///   `Terminator::Call` to one is fail-closed at MIR (`NotYetImplemented`).
///   Migrating math onto the same callable-floor mechanism as `mem.*`
///   (synthesized bodies via `intrinsic_id`) is tracked follow-up work, not
///   part of F1b.
/// - `std.mem` — the memory-intrinsic floor (`mem.alloc`/`mem.realloc`/
///   `mem.dealloc` + byte-level `mem.ptr_offset`/`mem.ptr_copy`) declared as
///   typed `#[intrinsic("mem.*")]` stubs in `std/mem/mem.hew` (W5.005 / F1b).
///   The pointer ops are byte-level monomorphic (A612) — no `<T>`. These are
///   unsafe heap primitives; A605 keeps them compiler-internal-only — no user
///   surface may reach them. Codegen synthesizes their trampoline bodies from
///   the catalog id threaded on `RawMirFunction::intrinsic_id` (Decision 4
///   Option A); an unrecognised id is fail-closed (D343), never a silent
///   empty-body no-op.
const INTRINSIC_FLOOR_MODULES: &[&str] = &["std.math", "std.mem"];

#[must_use]
pub fn intrinsic_floor_modules() -> &'static [&'static str] {
    INTRINSIC_FLOOR_MODULES
}

/// Returns `true` iff `module` is a stdlib-floor module permitted to declare
/// `#[intrinsic]` functions (see [`INTRINSIC_FLOOR_MODULES`]).
///
/// Fail-closed: the root/user module (`None`) is never a floor module, and any
/// module path not in the explicit allowlist is rejected.
fn is_intrinsic_floor_module(module: Option<&str>) -> bool {
    module.is_some_and(|m| INTRINSIC_FLOOR_MODULES.contains(&m))
}

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
    fn mark_import_module_used_for_owner(&self, owner: Option<String>, imported_module: &str) {
        self.used_modules
            .borrow_mut()
            .insert(ImportKey::new(owner, imported_module.to_string()));
    }

    fn mark_loaded_trait_owner_import_used(&self, module: Option<&str>, trait_name: &str) {
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

    fn mark_imported_trait_used(&self, module: Option<&str>, trait_name: &str) {
        if let Some((imported_module, _)) = trait_name.split_once('.') {
            if self.modules.contains(imported_module) {
                self.mark_import_module_used_for_owner(module.map(str::to_string), imported_module);
                if self.current_module.as_deref() != module {
                    self.mark_import_module_used_for_owner(
                        self.current_module.clone(),
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
            if let Some((imported_module, _)) = source_key.split_once('.') {
                if Some(imported_module) == module {
                    return;
                }
                self.mark_import_module_used_for_owner(module.map(str::to_string), imported_module);
                if self.current_module.as_deref() != module {
                    self.mark_import_module_used_for_owner(
                        self.current_module.clone(),
                        imported_module,
                    );
                }
            }
        } else if let Some(imported_module) = self
            .unqualified_to_module
            .get(&(module.map(str::to_string), trait_name.to_string()))
        {
            self.mark_import_module_used_for_owner(
                module.map(str::to_string),
                imported_module.as_str(),
            );
            if self.current_module.as_deref() != module {
                self.mark_import_module_used_for_owner(
                    self.current_module.clone(),
                    imported_module.as_str(),
                );
            }
        } else {
            self.mark_loaded_trait_owner_import_used(module, trait_name);
        }
    }

    fn mark_imported_trait_used_for_module_aliases(&self, module_short: &str, trait_name: &str) {
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

    fn structural_member_types_for_type(type_def: &TypeDef) -> Vec<Ty> {
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

    fn register_rcfree_members_for_type(&mut self, type_name: &str, type_def: &TypeDef) {
        let member_types = Self::structural_member_types_for_type(type_def);
        self.registry
            .register_rcfree_members(type_name.to_string(), member_types);
    }

    fn register_serializable_members_for_type(&mut self, type_name: &str, type_def: &TypeDef) {
        let member_types = Self::structural_member_types_for_type(type_def);
        self.registry
            .register_serializable_type(type_name.to_string(), member_types);
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
        self.register_builtin_fn("pow", vec![Ty::F64, Ty::F64], Ty::F64);
        self.register_builtin_fn("floor", vec![Ty::F64], Ty::F64);
        self.register_builtin_fn("ceil", vec![Ty::F64], Ty::F64);
        self.register_builtin_fn("round", vec![Ty::F64], Ty::F64);
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
        self.register_builtin_fn("sleep", vec![Ty::Duration], Ty::Unit);
        self.register_builtin_fn(
            "sleep_until",
            vec![Ty::Named {
                name: "instant".to_string(),
                args: vec![],
                builtin: Some(BuiltinType::Instant),
            }],
            Ty::Unit,
        );
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
        // Cross-node link: `link_remote(RemotePid<T>, PartitionPolicy)`
        // links a local actor to a remote actor so the remote's death fires the
        // per-link `PartitionPolicy` (`CrashLinked` crashes the local actor). The
        // immediate return is `Result<(), LinkError>` (registration success — the
        // EXIT arrives async). The `PartitionPolicy` enum is declared in
        // `std/link_monitor.hew`; the call-site checker validates the precise arg
        // type. The local `link(LocalPid)` form keeps its 1-arg shape; a
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
        // `Node::load_keys(path: String)` — load/persist this node's mesh
        // identity. `Node::allow_peer(spki_hex: String)` — pin a peer's SPKI.
        // Both are pre-start peer-auth setup; codegen routes them through the
        // shared RuntimeFfiShim path (catalog), like start/connect.
        self.register_builtin_fn("Node::load_keys", vec![Ty::String], Ty::Unit);
        self.register_builtin_fn("Node::allow_peer", vec![Ty::String], Ty::Unit);
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
            "exp", "log", "sqrt", "sin", "cos", "floor", "ceil", "tanh", "log2", "log10", "exp2",
        ] {
            self.register_builtin_fn(&format!("math.{name}"), vec![Ty::F64], Ty::F64);
        }
        self.register_builtin_num_math_fn("math.abs", 1);
        // Two-argument math functions: (f64, f64) → f64
        self.register_builtin_fn("math.pow", vec![Ty::F64, Ty::F64], Ty::F64);
        self.register_builtin_num_math_fn("math.max", 2);
        self.register_builtin_num_math_fn("math.min", 2);
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

        // Duplex constructors — compiler builtins.
        //
        // WHY: `duplex_pair` and `duplex` are constructor-only surfaces with no
        //   stdlib module equivalent yet; they must remain resolvable without an
        //   explicit import. The former `channel` builtin constructor was removed;
        //   callers use `std::channel::channel.new` instead.
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

        // Register the compiled-in primitive/builtin receiver impls that must
        // be visible without an explicit stdlib import: Display blanket impls
        // from `std/builtins.hew`, plus declarative string/bytes FFI receiver
        // methods from `std/string.hew` and `std/io.hew`.
        self.register_builtins_hew_impls();
        if !self.module_registry.has_search_paths() {
            self.register_builtin_closable_surface();
            self.register_builtin_monitor_ref_surface();
            self.register_builtin_failure_surface();
            self.register_builtin_lookup_error_surface();
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
                    // `display_fmt`". Seed WITHOUT claiming `lang_item_spans`
                    // (mirrors the trait_defs pre-registration above): when the
                    // file under check IS builtins.hew, the normal source pass
                    // re-registers the same `Display` trait and must not trip a
                    // duplicate-definition error against this seed.
                    self.seed_trait_lang_items(tr);
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
        self.register_stdlib_hew_items("builtins", &impl_items, StdlibBarePublication::Prelude);
        self.register_compiled_stdlib_receiver_impls(
            "string",
            include_str!("../../../std/string.hew"),
            &["string"],
        );
        self.register_compiled_stdlib_receiver_impls(
            "io",
            include_str!("../../../std/io.hew"),
            &["bytes"],
        );
        self.register_compiled_stdlib_receiver_impls(
            "option",
            include_str!("../../../std/option.hew"),
            &["Option"],
        );
        self.register_compiled_stdlib_receiver_impls(
            "result",
            include_str!("../../../std/result.hew"),
            &["Result"],
        );
    }

    fn register_compiled_stdlib_receiver_impls(
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
            self.register_stdlib_hew_items(
                module_short,
                &impl_items,
                StdlibBarePublication::Prelude,
            );
        }
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
            self.register_stdlib_hew_items("closable", &items, StdlibBarePublication::Prelude);
        }
    }

    /// Register the built-in `MonitorRef` surface so `monitor()` can return a
    /// Hew value type with `#[resource]` / `close()` behaviour in inline tests that
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
            self.register_stdlib_hew_items("link_monitor", &items, StdlibBarePublication::Prelude);
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
            self.register_stdlib_hew_items("failure", &items, StdlibBarePublication::Prelude);
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
            self.register_stdlib_hew_items("lookup_error", &items, StdlibBarePublication::Prelude);
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

    fn register_builtin_num_math_fn(&mut self, name: &str, arity: usize) {
        let t = Ty::Named {
            builtin: None,
            name: "T".to_string(),
            args: vec![],
        };
        self.register_builtin_fn_with_bounds(
            name,
            vec!["T".to_string()],
            HashMap::from([("T".to_string(), vec!["Num".to_string()])]),
            vec![t.clone(); arity],
            t,
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
                    for (item, item_span) in &module.items {
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
                                self.register_machine_decl(md, item_span);
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
                    self.source_type_defs.insert(ad.name.clone());
                }
                Item::TypeAlias(ta) => {
                    if !self.register_type_namespace_name(None, &ta.name, span) {
                        continue;
                    }
                    let mut hole_vars = Vec::new();
                    let resolved = self.resolve_type_expr_tracking_holes(&ta.ty, &mut hole_vars);
                    self.type_aliases.insert(ta.name.clone(), resolved);
                    self.record_type_def_inference_holes(&ta.name, hole_vars);
                    self.source_type_defs.insert(ta.name.clone());
                }
                Item::Trait(td) => {
                    if !self.register_type_namespace_name(None, &td.name, span) {
                        continue;
                    }
                    let mut trait_errors = Vec::new();
                    let info = Self::trait_info_from_decl_with_diagnostics(td, &mut trait_errors);
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
                    if !self.register_machine_type_namespace_names(None, &md.name, span) {
                        continue;
                    }
                    self.register_machine_decl(md, span);
                    self.local_type_defs.insert(md.name.clone());
                    self.source_type_defs.insert(md.name.clone());
                }
                Item::Record(rd) => {
                    if !self.register_type_namespace_name(None, &rd.name, span) {
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
    /// `Serializable`/`RcFree` member sets, the per-module qualified marker
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
    pub(super) fn reresolve_member_types_after_imports(&mut self, program: &Program) {
        let errors_before = self.errors.len();
        let warnings_before = self.warnings.len();

        if let Some(ref mg) = program.module_graph {
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
                for (item, _) in &module.items {
                    self.reresolve_item_member_types(item);
                }
                self.local_type_defs = saved_local_type_defs;
                self.source_type_defs = saved_source_type_defs;
            }
        }

        self.current_module = None;
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
    }

    /// Seed `local_type_defs`/`source_type_defs` with the current scope's own
    /// type names so member re-resolution (a) treats them as locally-defined
    /// (no fresh-var injection) and (b) shadows any same-named import alias —
    /// the local-shadow rule. Mirrors the seeding `collect_types` performs.
    fn seed_member_reresolution_scope(&mut self, items: &[Spanned<Item>]) {
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
                Item::Machine(md) => {
                    self.local_type_defs.insert(md.name.clone());
                    self.source_type_defs.insert(md.name.clone());
                    let event_type_name = format!("{}Event", md.name);
                    self.local_type_defs.insert(event_type_name.clone());
                    self.source_type_defs.insert(event_type_name);
                }
                Item::Actor(ad) => {
                    self.source_type_defs.insert(ad.name.clone());
                }
                Item::TypeAlias(ta) => {
                    self.source_type_defs.insert(ta.name.clone());
                }
                _ => {}
            }
        }
    }

    fn reresolve_item_member_types(&mut self, item: &Item) {
        match item {
            Item::TypeDecl(td) => self.reresolve_type_decl_members(td),
            Item::Record(rd) => self.reresolve_record_members(rd),
            Item::Machine(md) => self.reresolve_machine_members(md),
            _ => {}
        }
    }

    /// The collision-free key under which this scope's `TypeDef` is stored: the
    /// module-qualified `{module_short}.{name}` for a non-root module (when it
    /// exists), else the bare `name` for the root program.
    fn authoritative_type_def_key(&self, bare_name: &str) -> String {
        if let Some(module_short) = self.current_module_short() {
            let qualified = format!("{module_short}.{bare_name}");
            if self.type_defs.contains_key(&qualified) {
                return qualified;
            }
        }
        bare_name.to_string()
    }

    /// Commit a re-resolved `TypeDef`: insert the bare entry (last-write-wins
    /// across modules) and, for a non-root module, copy it to the
    /// collision-free `{module_short}.{name}` qualified key while mirroring the
    /// marker tables — exactly the bare+qualified pairing `pre_register_type_decl`
    /// and `register_qualified_type_alias` establish during normal registration.
    fn commit_reresolved_type_def(&mut self, name: &str, type_def: TypeDef) {
        self.type_defs.insert(name.to_string(), type_def);
        if let Some(module_short) = self.current_module_short().map(str::to_string) {
            self.register_qualified_type_alias(&module_short, name);
        }
        self.handle_bearing_dirty = true;
    }

    /// Re-resolve a `type`/`enum` declaration's member types under the now-live
    /// import-alias maps; on a member upgrade, patch `type_defs` and re-run every
    /// member-derived fact. Mirrors `register_type_decl`'s member resolution and
    /// derivation tail. No-op when no member changed.
    #[expect(
        clippy::too_many_lines,
        reason = "mirrors register_type_decl's member resolution and derivation tail"
    )]
    fn reresolve_type_decl_members(&mut self, td: &TypeDecl) {
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
        let all_fields_encodable = td.wire.is_none()
            && kind == TypeDefKind::Struct
            && field_types
                .iter()
                .all(|f| self.registry.implements_marker(f, MarkerTrait::Encode));
        self.registry.register_type(td.name.clone(), field_types);
        if td.wire.is_some() || kind == TypeDefKind::Enum {
            self.register_serializable_members_for_type(&td.name, &type_def);
        }
        self.register_rcfree_members_for_type(&td.name, &type_def);
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
        if all_fields_encodable {
            self.register_encode_methods(&td.name);
        }
    }

    /// Re-resolve a `record` declaration's member types. Mirrors
    /// `register_record_decl`'s named/tuple split and derivation tail. No-op when
    /// no member changed. Records are root-only, so there is no qualified key.
    fn reresolve_record_members(&mut self, rd: &RecordDecl) {
        let type_param_names: Vec<String> = rd.type_params.as_ref().map_or(vec![], |params| {
            params.iter().map(|p| p.name.clone()).collect()
        });
        let mut hole_vars = Vec::new();

        match &rd.kind {
            RecordKind::Named(record_fields) => {
                let mut fields: HashMap<String, Ty> = HashMap::new();
                let mut field_order: Vec<String> = Vec::new();
                for rf in record_fields {
                    let field_ty = self.resolve_registered_annotation_ty(&rf.ty, &mut hole_vars);
                    field_order.push(rf.name.clone());
                    fields.insert(rf.name.clone(), field_ty);
                }

                let stored_key = self.authoritative_type_def_key(&rd.name);
                let Some(stored) = self.type_defs.get(&stored_key) else {
                    return;
                };
                if stored.fields == fields {
                    return;
                }

                let type_def = TypeDef {
                    kind: TypeDefKind::Record,
                    name: rd.name.clone(),
                    type_params: type_param_names,
                    bounds: stored.bounds.clone(),
                    fields,
                    field_order,
                    variants: HashMap::new(),
                    methods: stored.methods.clone(),
                    doc_comment: rd.doc_comment.clone(),
                    is_indirect: false,
                };
                let field_types: Vec<Ty> = type_def.fields.values().cloned().collect();
                self.registry
                    .register_type(rd.name.clone(), field_types.clone());
                self.registry
                    .register_serializable_type(rd.name.clone(), field_types);
                self.register_rcfree_members_for_type(&rd.name, &type_def);
                self.commit_reresolved_type_def(&rd.name, type_def);
            }
            RecordKind::Tuple(positional_types) => {
                let param_tys: Vec<Ty> = positional_types
                    .iter()
                    .map(|te| self.resolve_registered_annotation_ty(te, &mut hole_vars))
                    .collect();
                // Tuple records store no fields (`.0`/`.1` access is forbidden);
                // the positional types live only in the constructor `fn_sig`.
                let unchanged = self
                    .fn_sigs
                    .get(&rd.name)
                    .is_some_and(|sig| sig.params == param_tys);
                if unchanged {
                    return;
                }
                if let Some(sig) = self.fn_sigs.get_mut(&rd.name) {
                    sig.params.clone_from(&param_tys);
                }
                self.registry
                    .register_type(rd.name.clone(), param_tys.clone());
                self.registry
                    .register_serializable_type(rd.name.clone(), param_tys);
                if let Some(module_short) = self.current_module_short().map(str::to_string) {
                    self.registry
                        .alias_type_markers(&rd.name, &format!("{module_short}.{}", rd.name));
                }
                self.handle_bearing_dirty = true;
            }
        }
    }

    /// Re-resolve a `machine` declaration's state and event field types. Mirrors
    /// `register_machine_decl`'s state-variant / event-companion resolution and
    /// marker derivation. State and event companions are patched independently.
    fn reresolve_machine_members(&mut self, md: &MachineDecl) {
        // --- State fields → machine `type_def` variants ---
        let mut variants = HashMap::new();
        let mut machine_hole_vars = Vec::new();
        for state in &md.states {
            if state.fields.is_empty() {
                variants.insert(state.name.clone(), VariantDef::Unit);
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

        let machine_key = self.authoritative_type_def_key(&md.name);
        if let Some(stored) = self.type_defs.get(&machine_key) {
            if stored.variants != variants {
                let type_def = TypeDef {
                    kind: TypeDefKind::Machine,
                    name: md.name.clone(),
                    type_params: stored.type_params.clone(),
                    bounds: stored.bounds.clone(),
                    fields: HashMap::new(),
                    field_order: vec![],
                    variants,
                    methods: stored.methods.clone(),
                    doc_comment: stored.doc_comment.clone(),
                    is_indirect: stored.is_indirect,
                };
                // Register field types for Send/Frozen derivation (mirrors the
                // `resolve_type_expr` flatten at the registration site).
                let mut all_field_types = Vec::new();
                for state in &md.states {
                    for (_, spanned_te) in &state.fields {
                        all_field_types.push(self.resolve_type_expr(spanned_te));
                    }
                }
                self.registry
                    .register_type(md.name.clone(), all_field_types);
                self.register_rcfree_members_for_type(&md.name, &type_def);
                self.commit_reresolved_type_def(&md.name, type_def);
            }
        }

        // --- Event fields → `{Name}Event` companion enum ---
        let event_type_name = format!("{}Event", md.name);
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
        let event_key = self.authoritative_type_def_key(&event_type_name);
        if let Some(stored) = self.type_defs.get(&event_key) {
            if stored.variants != event_variants {
                let event_type_def = TypeDef {
                    kind: TypeDefKind::Enum,
                    name: event_type_name.clone(),
                    type_params: stored.type_params.clone(),
                    bounds: stored.bounds.clone(),
                    fields: HashMap::new(),
                    field_order: vec![],
                    variants: event_variants,
                    methods: stored.methods.clone(),
                    doc_comment: stored.doc_comment.clone(),
                    is_indirect: stored.is_indirect,
                };
                self.register_rcfree_members_for_type(&event_type_name, &event_type_def);
                self.commit_reresolved_type_def(&event_type_name, event_type_def);
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
        // Idempotency guard, keyed per-module. Two non-root modules that each
        // declare a type of the same bare name (`badpkg.Reply` and
        // `goodpkg.Reply`) must BOTH register: the bare `type_defs` entry is
        // last-write-wins across modules (the qualified alias is the authority),
        // but each module's qualified marker set must be seeded so the ask-reply
        // Send gate derives `Send` from the correct module's fields. Keying the
        // guard on the bare name skipped the second module's `Reply` entirely,
        // leaving the gate to read whichever module won the bare-key race.
        let guard_key = self
            .current_module_short()
            .map_or_else(|| td.name.clone(), |m| format!("{m}.{}", td.name));
        if self.type_defs.contains_key(&guard_key) {
            return;
        }
        // #1295: record `#[resource]` types from pre-registered (imported)
        // modules too, so an imported handle type's inherent `close(self)`
        // consumes its receiver at the call site (mirrors `register_type_decl`).
        if td.resource_marker == hew_parser::ast::ResourceMarker::Resource {
            self.registry.register_resource_type(td.name.clone());
        }
        let kind = match td.kind {
            TypeDeclKind::Struct => TypeDefKind::Struct,
            TypeDeclKind::Enum => TypeDefKind::Enum,
        };
        let type_param_names: Vec<String> = td.type_params.as_ref().map_or(vec![], |params| {
            params.iter().map(|p| p.name.clone()).collect()
        });
        let type_param_bounds =
            self.collect_type_param_bounds(td.type_params.as_ref(), td.where_clause.as_ref());

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
        let mut field_order: Vec<String> = Vec::new();
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
                    field_order.push(name.clone());
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
                                    type_param_bounds: type_param_bounds.clone(),
                                    return_type,
                                    is_builtin_variant: self.in_stdlib_registration,
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
                                    type_param_bounds: type_param_bounds.clone(),
                                    params: variant_tys,
                                    return_type,
                                    is_builtin_variant: self.in_stdlib_registration,
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
            bounds: type_param_bounds,
            fields,
            field_order,
            variants,
            methods: HashMap::new(),
            doc_comment: td.doc_comment.clone(),
            is_indirect: td.is_indirect,
        };
        self.register_rcfree_members_for_type(&td.name, &type_def);

        // Seed the trait-registry structural member set for imported module
        // types, mirroring `register_type_decl`. An imported actor whose `ask`
        // replies with one of these types (e.g. a `pub type Result { ... }`)
        // is gated on the reply being `Send` at the dispatch site
        // (`record_actor_method_dispatch`, `E_DUPLEX_NON_SEND`). Send and the
        // sibling structural markers derive from a named type's member set; if
        // the importer's registry has no `type_fields` entry the derivation
        // hits the "unknown type — conservatively fail" branch and rejects a
        // plainly-Send imported record. Seeding it here resolves the marker
        // through the imported record's actual fields. Enums register their
        // variant-payload member set (an empty `fields` map would derive a
        // spurious Copy/Frozen); the `Serializable` subset follows the same
        // wire/enum condition as the full registration path.
        let field_types: Vec<_> = if kind == TypeDefKind::Enum {
            Self::structural_member_types_for_type(&type_def)
        } else {
            type_def.fields.values().cloned().collect()
        };
        self.registry.register_type(td.name.clone(), field_types);
        self.registry
            .register_type_params(td.name.clone(), type_def.type_params.clone());
        if td.wire.is_some() || kind == TypeDefKind::Enum {
            self.register_serializable_members_for_type(&td.name, &type_def);
        }
        // Mirror the markers under the module-qualified key so a same-bare-name
        // reply from another package cannot clobber this type's Send derivation
        // at the ask-reply gate.
        self.seed_qualified_type_markers_for_current_module(&td.name);

        // Insert the bare `type_defs` entry (last-write-wins across modules) and,
        // for a non-root module, a per-module qualified alias. The qualified
        // entry is the collision-free authority the per-module guard above keys
        // on, and the field source that non-pub reply types (e.g. an actor's
        // `receive fn` returning a module-private record) resolve through.
        if let Some(module_short) = self.current_module_short() {
            let qualified = format!("{module_short}.{}", td.name);
            self.type_defs.insert(qualified, type_def.clone());
        }
        self.type_defs.insert(td.name.clone(), type_def);
        self.record_type_def_inference_holes(&td.name, hole_vars);
        self.handle_bearing_dirty = true;
    }

    /// Reserve a type-name in the given module's namespace and reject a second
    /// declaration of the same name *within the same module*.
    ///
    /// `module_short` is the defining module (the last import-path segment for
    /// stdlib / user modules; `None` for the root program and flat file imports,
    /// which share one namespace). Two distinct modules may each declare a type
    /// of the same bare name — the durable cross-module identity is the qualified
    /// `{module}.{name}` key inserted by `register_qualified_type_alias`. The bare
    /// `type_def_spans` entry is still populated for the span-lookup consumers
    /// (cycle / actor-ref diagnostics); it is last-write-wins across modules and
    /// is no longer the uniqueness authority.
    pub(super) fn register_type_namespace_name(
        &mut self,
        module_short: Option<&str>,
        name: &str,
        span: &Span,
    ) -> bool {
        let owner_key = (module_short.map(str::to_string), name.to_string());
        if let Some(prev_span) = self.type_namespace_owners.get(&owner_key).cloned() {
            self.report_duplicate_type_namespace_name(name, span, prev_span);
            return false;
        }

        self.type_namespace_owners.insert(owner_key, span.clone());
        self.type_def_spans
            .entry(name.to_string())
            .or_insert_with(|| span.clone());
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
        module_short: Option<&str>,
        machine_name: &str,
        span: &Span,
    ) -> bool {
        let machine_key = (module_short.map(str::to_string), machine_name.to_string());
        if let Some(prev_span) = self.type_namespace_owners.get(&machine_key).cloned() {
            self.report_duplicate_type_namespace_name(machine_name, span, prev_span);
            return false;
        }

        let event_type_name = format!("{machine_name}Event");
        let event_key = (module_short.map(str::to_string), event_type_name.clone());
        if let Some(prev_span) = self.type_namespace_owners.get(&event_key).cloned() {
            self.report_duplicate_type_namespace_name(&event_type_name, span, prev_span);
            return false;
        }

        self.type_namespace_owners.insert(machine_key, span.clone());
        self.type_namespace_owners.insert(event_key, span.clone());
        self.type_def_spans
            .entry(machine_name.to_string())
            .or_insert_with(|| span.clone());
        self.type_def_spans
            .entry(event_type_name)
            .or_insert_with(|| span.clone());
        true
    }

    #[expect(clippy::too_many_lines, reason = "type resolution requires many cases")]
    pub(super) fn register_type_decl(&mut self, td: &TypeDecl) {
        // #1295: record `#[resource]` types so their inherent `close(self)`
        // dispatch can mark the receiver moved + consume it (suppressing the
        // duplicate scope-exit implicit drop). HIR owns the close-discipline
        // diagnostics (W3.030); the checker only needs the marker fact here.
        if td.resource_marker == hew_parser::ast::ResourceMarker::Resource {
            self.registry.register_resource_type(td.name.clone());
        }
        // Track user-declared `#[opaque]` types so `record_clone_admissibility`
        // can detect opaque fields transitively. The module_registry only
        // carries opaque types imported via `use module::*`; user-declared
        // opaques in the same file are NOT registered there.
        if td.is_opaque {
            self.user_opaque_type_names.insert(td.name.clone());
        }

        let kind = match td.kind {
            TypeDeclKind::Struct => TypeDefKind::Struct,
            TypeDeclKind::Enum => TypeDefKind::Enum,
        };

        let mut fields = HashMap::new();
        let mut field_order: Vec<String> = Vec::new();
        let mut variants = HashMap::new();
        let mut variant_order = Vec::new();
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
                    field_order.push(name.clone());
                    fields.insert(name.clone(), field_ty);
                }
                TypeBodyItem::Variant(variant) => {
                    variant_order.push(variant.name.clone());
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
            bounds: type_param_bounds,
            fields,
            field_order,
            variants,
            methods: HashMap::new(),
            doc_comment: td.doc_comment.clone(),
            is_indirect: td.is_indirect,
        };

        // Register with trait registry for Send/Frozen/Copy/... derivation.
        //
        // Structural markers (`Copy`, `Send`, `Clone`, …) derive from a Named
        // type's reachable member types. For a struct/record those are its
        // fields; for an ENUM they are the variant PAYLOAD types — an enum with
        // a `string`-payload variant is NOT Copy even though it has no named
        // fields. The marker registry stores member types by name, and its
        // derivation walks them with `all(...)` (vacuously true on an empty
        // list), so an enum registered with only its (empty) `fields` would be
        // spuriously Copy/Frozen. Register the variant-inclusive member set for
        // enums so the marker derivation is correct (W5.016: the spurious-Copy
        // bug routed owned-payload enum Vecs down the BitCopy path → runtime
        // stride panic).
        let field_types: Vec<_> = if kind == TypeDefKind::Enum {
            Self::structural_member_types_for_type(&type_def)
        } else {
            type_def.fields.values().cloned().collect()
        };
        let all_fields_encodable = td.wire.is_none()
            && kind == TypeDefKind::Struct
            && field_types
                .iter()
                .all(|f| self.registry.implements_marker(f, MarkerTrait::Encode));

        self.registry.register_type(td.name.clone(), field_types);
        self.registry
            .register_type_params(td.name.clone(), type_param_names.clone());
        if td.wire.is_some() || kind == TypeDefKind::Enum {
            self.register_serializable_members_for_type(&td.name, &type_def);
        }
        self.register_rcfree_members_for_type(&td.name, &type_def);
        // Mirror the markers under the module-qualified key (when this type is
        // declared in a non-root module) so a same-bare-name reply from another
        // package cannot clobber this type's Send derivation at the ask-reply
        // gate. `register_qualified_type_alias` repeats this for the pub import
        // surface; this covers the registration call itself.
        self.seed_qualified_type_markers_for_current_module(&td.name);

        self.type_defs.insert(td.name.clone(), type_def);
        self.record_type_def_inference_holes(&td.name, hole_vars);
        self.handle_bearing_dirty = true;

        // If this is a wire type, register encode/decode/to_json/from_json/to_yaml/from_yaml methods
        if let Some(ref wire) = td.wire {
            self.register_wire_methods(&td.name, wire, &variant_order);
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
        let mut field_order: Vec<String> = Vec::new();
        let mut hole_vars = Vec::new();
        // Positional field types for tuple records, collected for marker
        // derivation (A-4). Named-record fields come from `type_def.fields`.
        let mut tuple_field_types: Vec<Ty> = Vec::new();

        match &rd.kind {
            RecordKind::Named(record_fields) => {
                for rf in record_fields {
                    let field_ty = self.resolve_registered_annotation_ty(&rf.ty, &mut hole_vars);
                    field_order.push(rf.name.clone());
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
            bounds: type_param_bounds,
            fields,
            field_order,
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
        self.registry
            .register_type(rd.name.clone(), field_types.clone());
        self.registry
            .register_type_params(rd.name.clone(), type_param_names.clone());
        // Mark this as a record type so implements_marker applies the correct
        // value-type semantics (Resource always false; all other markers field-driven).
        self.registry.register_record_type(rd.name.clone());
        self.registry
            .register_serializable_type(rd.name.clone(), field_types);
        self.register_rcfree_members_for_type(&rd.name, &type_def);

        self.type_defs.insert(rd.name.clone(), type_def);
        self.record_type_def_inference_holes(&rd.name, hole_vars);
        self.handle_bearing_dirty = true;
    }

    /// Register codec methods for a wire type.
    ///
    /// - Wire structs expose binary + JSON/YAML helpers.
    /// - Wire enums expose JSON/YAML helpers.
    pub(super) fn register_wire_methods(
        &mut self,
        type_name: &str,
        wire: &WireMetadata,
        variant_order: &[String],
    ) {
        let self_ty = Ty::Named {
            builtin: None,
            name: type_name.to_string(),
            args: vec![],
        };
        let bytes_ty = Ty::Bytes;

        let Some((is_wire_struct, is_serial_wire_enum, layout_entry)) =
            self.type_defs.get(type_name).map(|type_def| {
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
                let layout_entry = Self::wire_layout_entry_from_metadata(
                    type_def,
                    wire,
                    is_wire_struct,
                    variant_order,
                );
                (is_wire_struct, is_serial_wire_enum, layout_entry)
            })
        else {
            return;
        };
        // Track wire structs and wire enums so the method-dispatch arms can
        // recognise the binary `encode`/`decode` codec calls (which lower to the
        // `__hew_cbor_serialize_*` / `__hew_cbor_deserialize_*` thunks) without
        // re-deriving wire-ness. Both ride the CBOR body codec: structs as a
        // tag-keyed map, enums as the "map-of-one" shape.
        if is_wire_struct {
            self.wire_struct_types.insert(type_name.to_string());
        }
        if is_serial_wire_enum {
            self.wire_enum_types.insert(type_name.to_string());
        }
        self.wire_layouts
            .insert(type_name.to_string(), layout_entry);

        // Wire structs and wire enums carry the same method surface: the binary
        // CBOR codec (`encode`/`decode`) plus the text-format helpers. The body
        // shapes differ at codegen (struct = tag-keyed map, enum =
        // "map-of-one"), but the registered signatures are identical.
        let instance_methods = if is_wire_struct || is_serial_wire_enum {
            vec![
                ("encode", vec![], bytes_ty.clone()),
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
                        ..FnSig::default()
                    },
                );
            }
        }

        // `decode` returns bare `Self` (binary CBOR is trap-on-failure); the
        // text-format `from_json`/`from_yaml` parsers can fail on arbitrary
        // user input (config files, HTTP bodies), so they return
        // `Result<Self, string>` — the only honest shape for a fallible parse.
        // This matches the non-wire `Encode` path (`register_encode_methods`).
        let from_result_ty = Ty::result(self_ty.clone(), Ty::String);
        let static_methods = if is_wire_struct || is_serial_wire_enum {
            vec![
                ("decode", vec![bytes_ty], self_ty),
                ("from_json", vec![Ty::String], from_result_ty.clone()),
                ("from_yaml", vec![Ty::String], from_result_ty),
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
                    ..FnSig::default()
                },
            );
        }
    }

    fn wire_layout_entry_from_metadata(
        type_def: &TypeDef,
        wire: &WireMetadata,
        is_wire_struct: bool,
        variant_order: &[String],
    ) -> WireLayoutEntry {
        let fields = if is_wire_struct {
            wire.field_meta
                .iter()
                .map(|field| WireFieldLayout {
                    name: field.field_name.clone(),
                    tag: field.field_number,
                    json_name: field.json_name.clone(),
                    yaml_name: field.yaml_name.clone(),
                    optional: field.is_optional,
                    repeated: field.is_repeated,
                })
                .collect()
        } else {
            Vec::new()
        };

        let variant_tags: HashMap<&str, u32> = wire
            .field_meta
            .iter()
            .map(|field| (field.field_name.as_str(), field.field_number))
            .collect();
        let variant_names: Vec<String> = if variant_order.is_empty() {
            let mut names: Vec<_> = type_def.variants.keys().cloned().collect();
            names.sort();
            names
        } else {
            variant_order
                .iter()
                .filter(|name| type_def.variants.contains_key(*name))
                .cloned()
                .collect()
        };
        let variants = if is_wire_struct {
            Vec::new()
        } else {
            variant_names
                .into_iter()
                .enumerate()
                .map(|(index, name)| {
                    #[expect(
                        clippy::cast_possible_truncation,
                        reason = "wire enum variant counts are bounded by source size"
                    )]
                    let default_tag = index as u32;
                    let tag = variant_tags
                        .get(name.as_str())
                        .copied()
                        .unwrap_or(default_tag);
                    (name, tag)
                })
                .collect()
        };

        WireLayoutEntry {
            is_struct: is_wire_struct,
            json_case: wire.json_case,
            yaml_case: wire.yaml_case,
            version: wire.version,
            min_version: wire.min_version,
            fields,
            variants,
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
    pub(super) fn register_machine_decl(&mut self, md: &MachineDecl, span: &Span) {
        // Build the machine's self-type: `Machine` or `Machine<T, U, …>`.
        // MachineDecl.type_params is Vec<TypeParam> — we extract bare names
        // here for the self-type and collect declared trait bounds into a
        // side table consulted at use sites (struct-state brace init) and
        // mirrored onto unit-state constructor FnSigs for the call path.
        //
        // Validate before collect_type_param_bounds erases positional type args.
        self.validate_type_param_bound_shapes(
            Some(&md.type_params),
            md.where_clause.as_ref(),
            span,
        );
        let type_param_names: Vec<String> = md.type_params.iter().map(|p| p.name.clone()).collect();
        // Collect inline `<T: Trait>` and `where T: Trait` bounds into a
        // single side table keyed by machine name then param name. At
        // the checker layer, a bound's source (inline vs where clause)
        // does not affect the enforcement question — the bound is
        // "satisfied at the instantiation site iff the substituted
        // type implements the trait" regardless of where the bound
        // was authored — so duplicates on the same (param, trait) pair
        // dedupe. Source provenance is preserved at the parser layer
        // (separate `type_params` / `where_clause` fields on
        // `MachineDecl`) so future lowering layers that want to point
        // diagnostics at the predicate's span can recover it.
        let type_param_bounds =
            self.collect_type_param_bounds(Some(&md.type_params), md.where_clause.as_ref());
        if !type_param_bounds.is_empty() {
            self.machine_type_param_bounds
                .insert(md.name.clone(), type_param_bounds.clone());
        }
        // W3.039 Stage 2: register const-generic parameter declarations
        // into the side table so instantiation-site validation
        // (Stage 3 — gated on W3.033c) can recover arity, types, and
        // defaults without re-walking the parser AST. We also enforce
        // here that const-param names do not shadow type-param names.
        if !md.const_params.is_empty() {
            let type_param_names: std::collections::HashSet<&str> =
                md.type_params.iter().map(|p| p.name.as_str()).collect();
            let mut const_param_decls: Vec<super::types::MachineConstParamDecl> =
                Vec::with_capacity(md.const_params.len());
            let mut seen_const_names: std::collections::HashSet<&str> =
                std::collections::HashSet::new();
            for cp in &md.const_params {
                if type_param_names.contains(cp.name.as_str()) {
                    self.errors.push(crate::error::TypeError::new(
                        crate::error::TypeErrorKind::DuplicateDefinition,
                        span.clone(),
                        format!(
                            "const parameter `{}` on machine `{}` shadows a type parameter \
                             of the same name",
                            cp.name, md.name
                        ),
                    ));
                    continue;
                }
                if !seen_const_names.insert(cp.name.as_str()) {
                    self.errors.push(crate::error::TypeError::new(
                        crate::error::TypeErrorKind::DuplicateDefinition,
                        span.clone(),
                        format!(
                            "duplicate const parameter `{}` on machine `{}`",
                            cp.name, md.name
                        ),
                    ));
                    continue;
                }
                let ty = match cp.ty {
                    hew_parser::ast::ConstParamTy::Usize => {
                        super::types::MachineConstParamTy::Usize
                    }
                };
                const_param_decls.push(super::types::MachineConstParamDecl {
                    name: cp.name.clone(),
                    ty,
                    default: cp.default,
                });
            }
            if !const_param_decls.is_empty() {
                self.machine_const_params
                    .insert(md.name.clone(), const_param_decls);
            }
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
            bounds: type_param_bounds.clone(),
            fields: HashMap::new(),
            field_order: vec![],
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
        self.registry
            .register_type_params(md.name.clone(), type_param_names.clone());
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
            bounds: type_param_bounds.clone(),
            fields: HashMap::new(),
            field_order: vec![],
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
            // `await_restart` is a cooperative suspension point, forbidden in a
            // machine transition for the same reason as `await`.
            Expr::AwaitRestart(inner) => {
                hits.push((
                    TypeErrorKind::AwaitInMachineTransition,
                    span.clone(),
                    "await_restart",
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
            | Expr::Clone(operand)
            | Expr::ForkChild { expr: operand, .. }
            | Expr::PostfixTry(operand)
            | Expr::Yield(Some(operand))
            | Expr::Return(Some(operand)) => {
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
            Expr::Spawn { target, args, .. } => {
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
            | Expr::Return(None)
            | Expr::This
            | Expr::RegexLiteral(_)
            | Expr::ByteStringLiteral(_)
            | Expr::ByteArrayLiteral(_) => {}
        }
    }

    /// Validate that no trait bound in the given type parameters or
    /// where-clause carries positional type arguments (e.g. `T: Eq<U>`).
    /// Such forms are not valid in Hew — the checker cannot enforce
    /// phantom-parameterised marker bounds, and admitting them would silently
    /// erase the type arguments in `collect_type_param_bounds`, reducing
    /// `Eq<U>` to bare `Eq` without any diagnostic.
    ///
    /// Emits `UnknownTraitBoundShape` at `span` for every offending bound.
    /// Must be called before `collect_type_param_bounds` erases `type_args`.
    /// Covers fn/impl/impl-method/machine declaration positions.
    pub(super) fn validate_type_param_bound_shapes(
        &mut self,
        type_params: Option<&Vec<TypeParam>>,
        where_clause: Option<&WhereClause>,
        span: &Span,
    ) {
        // Check inline type-param bounds: e.g. `<T: Eq<U>>`.
        if let Some(params) = type_params {
            for param in params {
                for bound in &param.bounds {
                    if bound.type_args.as_ref().is_some_and(|a| !a.is_empty()) {
                        self.report_error(
                            TypeErrorKind::UnknownTraitBoundShape {
                                trait_name: bound.name.clone(),
                            },
                            span,
                            format!(
                                "trait bound `{}` on type parameter `{}` carries positional \
                                 type arguments, which are not supported; use associated-type \
                                 bindings (`Trait<Assoc = Ty>`) instead",
                                bound.name, param.name,
                            ),
                        );
                    }
                }
            }
        }
        // Check where-clause bounds: `where T: Eq<U>`.
        if let Some(wc) = where_clause {
            for predicate in &wc.predicates {
                for bound in &predicate.bounds {
                    if bound.type_args.as_ref().is_some_and(|a| !a.is_empty()) {
                        self.report_error(
                            TypeErrorKind::UnknownTraitBoundShape {
                                trait_name: bound.name.clone(),
                            },
                            span,
                            format!(
                                "trait bound `{}` in where-clause carries positional \
                                 type arguments, which are not supported; use associated-type \
                                 bindings (`Trait<Assoc = Ty>`) instead",
                                bound.name,
                            ),
                        );
                    }
                }
            }
        }
    }

    /// Thin wrapper for the fn-decl path; delegates to
    /// `validate_type_param_bound_shapes` using the function's own
    /// type-param list, where-clause, and declaration span.
    pub(super) fn validate_fn_type_param_bound_shapes(&mut self, fd: &FnDecl) {
        self.validate_type_param_bound_shapes(
            fd.type_params.as_ref(),
            fd.where_clause.as_ref(),
            &fd.decl_span,
        );
    }

    /// Validate that every trait named in a machine's generic bounds resolves
    /// to a registered trait. Emits `UndefinedType` at the machine decl span
    /// for any unknown name. Called from `check_machine_exhaustiveness`,
    /// after Pass 2 has populated `trait_defs` for all in-scope traits.
    ///
    /// Walks both inline `<T: Trait>` bounds (via `md.type_params`) and
    /// `where T: Trait` clause predicates (via `md.where_clause`). The
    /// where-clause arm also verifies the predicate's left-hand side
    /// names one of the machine's own declared type parameters — a
    /// `where Foo: Trait` for an undeclared `Foo` is a closed user
    /// error (`UndefinedType` at the predicate span) rather than a
    /// silently-ignored predicate.
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

        let Some(where_clause) = md.where_clause.as_ref() else {
            return;
        };
        let declared_params: std::collections::HashSet<&str> =
            md.type_params.iter().map(|p| p.name.as_str()).collect();
        for predicate in &where_clause.predicates {
            // Left-hand side of the predicate must name one of the
            // machine's declared type params. `where Foo: Resource`
            // for a `Foo` that isn't in `<…>` is a user error.
            let lhs_name = match &predicate.ty.0 {
                hew_parser::ast::TypeExpr::Named { name, type_args } if type_args.is_none() => {
                    Some(name.as_str())
                }
                _ => None,
            };
            match lhs_name {
                Some(name) if declared_params.contains(name) => {}
                Some(name) => {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::UndefinedType,
                        predicate.ty.1.clone(),
                        format!(
                            "where-clause predicate references `{name}` which is not a declared type parameter of machine `{machine}`",
                            machine = md.name,
                        ),
                    ));
                }
                None => {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::UndefinedType,
                        predicate.ty.1.clone(),
                        format!(
                            "where-clause predicate on machine `{machine}` must name a single type parameter",
                            machine = md.name,
                        ),
                    ));
                }
            }
            for bound in &predicate.bounds {
                if self.is_known_trait(&bound.name) {
                    continue;
                }
                let similar = crate::error::find_similar(
                    &bound.name,
                    self.trait_defs.keys().map(String::as_str),
                );
                let lhs_label = lhs_name.unwrap_or("<predicate>");
                self.report_error_with_suggestions(
                    TypeErrorKind::UndefinedType,
                    &predicate.ty.1,
                    format!(
                        "unknown trait `{bound}` in where-clause bound on `{lhs_label}` of machine `{machine}`",
                        bound = bound.name,
                        machine = md.name,
                    ),
                    similar,
                );
            }
        }
    }

    /// Resolve a trait-bound name against the registered trait table,
    /// accepting both unqualified and module-qualified forms.
    pub(super) fn is_known_trait(&self, name: &str) -> bool {
        if MarkerTrait::from_name(name).is_some() {
            return true;
        }
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
            let mut machine_scope_holes = Vec::new();
            let machine_bounds_scope = self.collect_type_param_scope_with_assoc_bindings(
                Some(&md.type_params),
                md.where_clause.as_ref(),
                &mut machine_scope_holes,
            );
            let pushed_machine_bounds = !machine_bounds_scope.bounds.is_empty();
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

        // Check state entry/exit lifecycle blocks.
        //
        // Scope: `state` (the machine value) is in scope; `event` is NOT
        // bound here — entry/exit are state lifecycle hooks, not transition
        // event scopes.  Referencing `event` inside an entry/exit block is
        // therefore an undefined-variable error, which is the intended
        // fail-closed behaviour.
        for state in &md.states {
            let has_lifecycle = state.entry.is_some() || state.exit.is_some();
            if !has_lifecycle {
                continue;
            }

            // Push generic-param bounds so that type-param-bound resolution
            // inside a lifecycle block mirrors what transition bodies see.
            let mut machine_scope_holes = Vec::new();
            let machine_bounds_scope = self.collect_type_param_scope_with_assoc_bindings(
                Some(&md.type_params),
                md.where_clause.as_ref(),
                &mut machine_scope_holes,
            );
            let pushed_machine_bounds = !machine_bounds_scope.bounds.is_empty();
            if pushed_machine_bounds {
                self.current_type_param_bounds.push(machine_bounds_scope);
            }

            self.env.push_scope();
            // Bind `state` as the machine self-type — identical binding to
            // the one used inside transition bodies, so that field access on
            // payload states resolves correctly.
            let machine_args: Vec<Ty> = md
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
                    args: machine_args,
                },
                false,
            );
            // NOTE: `event` is deliberately NOT bound here.
            let previous_lifecycle = self
                .current_machine_lifecycle
                .replace((md.name.clone(), state.name.clone()));

            if let Some(entry_block) = &state.entry {
                // Entry blocks are statement-sequences; their trailing value
                // (if any) is discarded — we check without an expected type.
                self.check_block(entry_block, None);
            }
            if let Some(exit_block) = &state.exit {
                self.check_block(exit_block, None);
            }

            self.current_machine_lifecycle = previous_lifecycle;
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
        let identity = ad.name.clone();
        self.register_actor_decl_as(ad, &identity);
    }

    /// Register an actor declaration under an explicit identity key.
    ///
    /// `identity` is the bare name for root/flat actors and the dotted
    /// `{module_short}.{name}` form for module actors (see
    /// [`Self::actor_identity`]). Every per-actor side table — `type_defs`,
    /// the Send registry, type-param bounds, init params — is keyed by this
    /// identity so two same-named actors from different modules occupy
    /// distinct entries instead of last-write-wins clobbering.
    pub(super) fn register_actor_decl_as(&mut self, ad: &ActorDecl, identity: &str) {
        let mut fields = HashMap::new();
        let mut field_order: Vec<String> = Vec::new();
        let mut hole_vars = Vec::new();
        for field in &ad.fields {
            let field_ty = self.resolve_registered_annotation_ty(&field.ty, &mut hole_vars);
            field_order.push(field.name.clone());
            fields.insert(field.name.clone(), field_ty);
        }

        // Extract type-param names from the declaration so the TypeDef's
        // positional `type_params` vector is populated for bound lookups in
        // `enforce_actor_instantiation_bounds`. This mirrors the machine
        // registration path; actors without type params get an empty vec.
        let type_param_names: Vec<String> =
            ad.type_params.iter().map(|tp| tp.name.clone()).collect();
        let type_param_bounds = self.collect_type_param_bounds(Some(&ad.type_params), None);

        let type_def = TypeDef {
            kind: TypeDefKind::Actor,
            name: identity.to_string(),
            type_params: type_param_names,
            bounds: type_param_bounds.clone(),
            fields,
            field_order,
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: ad.doc_comment.clone(),
            is_indirect: false,
        };

        // Record trait bounds for generic type parameters (e.g. `<T: Send>`).
        // The bounds table is keyed by actor name and consulted at spawn sites
        // by `enforce_actor_instantiation_bounds`. Non-generic actors produce
        // an empty map; the helper short-circuits on empty `type_args` anyway.
        if !type_param_bounds.is_empty() {
            self.actor_type_param_bounds
                .insert(identity.to_string(), type_param_bounds);
        }

        // `#[every]` periodic handlers are armed by spawn-site codegen
        // (`emit_periodic_handler_arming`); record which actors declare them
        // so `check_supervisor` can reject child specs whose runtime spawn
        // path would silently skip the arming.
        if let Some(periodic_rf) = ad
            .receive_fns
            .iter()
            .find(|rf| rf.attributes.iter().any(|a| a.name == "every"))
        {
            self.actors_with_periodic_handlers
                .insert(ad.name.clone(), periodic_rf.name.clone());
        }

        // Actors are always Send
        self.registry.register_actor(identity.to_string());
        self.register_rcfree_members_for_type(identity, &type_def);

        self.type_defs.insert(identity.to_string(), type_def);
        // A new handle-bearing candidate entered `type_defs`; invalidate the
        // cached handle-bearing classification the same way the qualified
        // type-alias path does.
        self.handle_bearing_dirty = true;

        // Collect resolved init() parameter types for supervisor checks.  The
        // byte-copy wall is fail-closed: any shape that does not resolve to a
        // scalar `Ty` is rejected at the parameter type span.
        //
        // Always insert — actors with no init block get an empty vec so that a
        // `wired_to:` reference to such an actor correctly fires
        // "no parameter named X" (`E_SUPERVISOR_WIRED_TO_TYPE_MISMATCH`) rather
        // than silently passing through the "unknown actor" early-return.
        let params: Vec<ActorInitParamInfo> = if let Some(init) = &ad.init {
            init.params
                .iter()
                .map(|p| {
                    let mut init_param_hole_vars = Vec::new();
                    let ty =
                        self.resolve_registered_annotation_ty(&p.ty, &mut init_param_hole_vars);
                    ActorInitParamInfo {
                        name: p.name.clone(),
                        ty,
                        span: p.ty.1.clone(),
                    }
                })
                .collect()
        } else {
            vec![]
        };
        self.actor_init_params.insert(identity.to_string(), params);
        self.record_type_def_inference_holes(identity, hole_vars);
    }

    pub(super) fn trait_info_from_decl(tr: &TraitDecl) -> TraitInfo {
        Self::trait_info_from_decl_with_diagnostics(tr, &mut Vec::new())
    }

    /// Idempotently seed `#[lang_item("…")]` bindings from a compiled-in
    /// `std/builtins.hew` trait declaration WITHOUT claiming `lang_item_spans`.
    ///
    /// Mirrors the `trait_defs` pre-registration in `register_builtins_hew_impls`
    /// that deliberately skips `type_def_spans`: the user-redeclare path — which
    /// includes type-checking `std/builtins.hew` itself, where the same `Display`
    /// trait is processed a second time through the normal source-registration
    /// pass — must register cleanly without a `duplicate_definition` error.
    /// Genuine collisions between two source traits both tagging the same key
    /// are still caught: that path runs through `register_trait_lang_items`,
    /// which owns `lang_item_spans`.
    pub(super) fn seed_trait_lang_items(&mut self, td: &TraitDecl) {
        if let Some(key) = &td.lang_item {
            if self.lang_items.get(key).is_none() {
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
                    if self.lang_items.get(key).is_none() {
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
            let trait_key = self.trait_defs_key_for_bound(&tb.name);
            if let Some(trait_info) = self.trait_defs.get(&trait_key) {
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
        // Validate before collect_type_param_scope_with_bounds erases positional type args.
        self.validate_type_param_bound_shapes(
            id.type_params.as_ref(),
            id.where_clause.as_ref(),
            span,
        );
        let entries = self.build_impl_alias_entries(id);
        let mut impl_scope_holes = Vec::new();
        let impl_bounds_map = self.collect_type_param_scope_with_assoc_bindings(
            id.type_params.as_ref(),
            id.where_clause.as_ref(),
            &mut impl_scope_holes,
        );
        let pushed_impl_bounds = !impl_bounds_map.bounds.is_empty();
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
            //
            // Keyed on the BARE `tb.name`, deliberately. This site POPULATES
            // `impl_assoc_type_bindings` for call-site projection collapse, and
            // its `(target, trait, assoc)` key — plus the carrier `trait_name`
            // the projection lookups (`substitute_trait_sig_for_impl`,
            // `project_assoc_types`) build from the bound AS WRITTEN — are all on
            // the bound's in-scope spelling. Re-keying the `trait_defs` read on
            // the owner-qualified identity here makes `trait_defs.get` SUCCEED for
            // an aliased bound (`import m::{ Carrier as A }`) where the bare alias
            // has no entry, writing a binding that was previously absent and
            // making the projection collapse fire asymmetrically (the trait side
            // collapses to the concrete assoc while the impl side, written as
            // `Self::Item`, does not) — a false signature-mismatch on a sound
            // impl. The poisoning H12 closes is the required-assoc-type ENFORCEMENT
            // (the `enforce` block below) and the default registration, not this
            // projection-binding population, which is keyed end-to-end on the
            // bound spelling.
            //
            // FAIL-CLOSED, with a KNOWN LIMITATION (deferred to v0.5.3). No
            // invalid program is accepted through this path. There IS, however, a
            // valid program it over-rejects: an ALIASED trait
            // (`import m::{ Carrier as A }`) whose impl uses an associated-type
            // PROJECTION in a method signature (`fn item(w) -> Self::Item`). The
            // bare `tb.name` is the alias `A`, which has no `trait_defs` entry, so
            // no binding is populated and the `Self::Item` projection never
            // collapses to the concrete assoc type — yielding a false
            // `returns i64 but trait requires W::Item` mismatch. Re-keying this
            // read alone fixes the alias lookup but desyncs the carrier
            // `trait_name` the projection lookups build from the bound as written,
            // collapsing the trait side without the impl side. The full fix is the
            // aliased-assoc-type projection-collapse reshape — populate AND project
            // through the one owner-qualified identity together — tracked as a
            // v0.5.3 follow-up. Until then the alias+projection case is rejected,
            // not accepted: a deferred over-rejection, never a soundness hole.
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
                // resolver / bound-checker which need `&mut self`. Keyed off the
                // owner-qualified identity, so two modules importing same-named
                // traits cannot let one's empty `trait_defs[name]` mask the
                // other's required `type` and accept an incomplete impl.
                let trait_key = self.trait_defs_key_for_bound(&tb.name);
                let trait_snapshot = self
                    .trait_defs
                    .get(&trait_key)
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
                    self.fn_def_spans.insert(
                        scoped_name.clone(),
                        (span.clone(), self.current_module.clone()),
                    );
                    self.fn_visibility.insert(scoped_name, fd.visibility);
                }
                self.register_fn_sig(fd);
            }
            Item::Actor(ad) => {
                // Module actors are identified by the dotted
                // `{module_short}.{name}` key throughout the checker; root
                // actors stay bare. Registering the full declaration here
                // (not only the signatures) covers PRIVATE module actors,
                // which never pass through the pub-only import paths but
                // still need a type def for in-module spawn checking.
                let module_short = self.current_module_short().map(str::to_owned);
                let identity = Self::actor_identity(module_short.as_deref(), &ad.name);
                if module_short.is_some() {
                    self.register_actor_decl_as(ad, &identity);
                }
                for rf in &ad.receive_fns {
                    self.register_receive_fn(&identity, rf);
                }
                for method in &ad.methods {
                    let method_name = format!("{identity}::{}", method.name);
                    self.register_fn_sig_with_name(&method_name, method);
                }
            }
            Item::Impl(id) => {
                if Self::impl_decl_is_drop_impl(id) {
                    self.report_unsupported_impl_drop(span);
                    return;
                }
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

                    // Validate the impl provides EXACTLY the trait's method set
                    // (every required method present, no extraneous method),
                    // resolved through the trait's owner-qualified identity. Runs
                    // ONCE per impl, before the per-method loop, so an impl with
                    // ZERO methods (an empty `impl A for W { }` missing every
                    // required method) is still validated. Per-method signature
                    // equivalence is checked separately inside the loop.
                    if let Some(tb) = id.trait_bound.as_ref() {
                        self.check_impl_method_set_against_trait(type_name, tb, &id.methods, span);
                    }

                    for method in &id.methods {
                        let sig = self.register_impl_method(
                            type_name,
                            method,
                            id.type_params.as_ref(),
                            id.where_clause.as_ref(),
                        );
                        // Q004: enforce impl-vs-trait signature equivalence at
                        // the impl site so mismatches surface where the user
                        // wrote them, not as a confusing "type does not satisfy
                        // trait" downstream. See LESSONS row `diagnostic-trust`.
                        if let Some(tb) = id.trait_bound.as_ref() {
                            self.check_impl_method_against_trait(
                                type_name,
                                &self_type_args,
                                tb,
                                method,
                                &sig,
                            );
                        }
                        // When the receiver is a primitive or compiler-builtin
                        // generic, `lookup_type_def_mut(type_name)` returns `None` so the
                        // sig has nowhere to live for later dispatch.  Mirror it onto the
                        // side table keyed by the canonical receiver kind + trait name so
                        // method-resolution can find it.
                        if let (Some(canonical), Some(tb)) =
                            (primitive_key.clone(), id.trait_bound.as_ref())
                        {
                            self.record_primitive_trait_impl_self_args(
                                canonical.clone(),
                                tb.name.clone(),
                                self_type_args.clone(),
                                &id.target_type.1,
                            );
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
                        self.record_trait_impl_methods(
                            type_name,
                            &tb.name,
                            id.methods.iter().map(|method| method.name.clone()),
                        );
                        self.record_trait_impl(type_name, &tb.name);

                        let overridden: HashSet<&str> =
                            id.methods.iter().map(|m| m.name.as_str()).collect();
                        // Owner-qualified key so a same-name trait in another
                        // module cannot inject the wrong default-method bodies.
                        let trait_key = self.trait_defs_key_for_bound(&tb.name);
                        if let Some(trait_methods) = self.trait_defs.get(&trait_key) {
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
                                    ..FnSig::default()
                                },
                            );
                        }
                    }
                }
            }
            Item::Trait(td) => {
                if let Some(supers) = &td.super_traits {
                    let owner = self.current_module.as_deref();
                    for super_trait in supers {
                        self.mark_imported_trait_used(owner, &super_trait.name);
                    }
                }
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
            | Item::Supervisor(_)
            | Item::Machine(_)
            | Item::Record(_) => {
                // Records have no method body items in v0.5; method registration
                // is a no-op here.  TODO(A-4): if records gain methods, register
                // them via a `register_record_methods` pass here.
            }
        }
    }

    pub(super) fn impl_decl_is_drop_impl(id: &ImplDecl) -> bool {
        id.trait_bound
            .as_ref()
            .is_some_and(|trait_bound| trait_bound.name == "Drop")
    }

    pub(super) fn report_unsupported_impl_drop(&mut self, span: &Span) {
        self.errors.push(TypeError::new(
            TypeErrorKind::InvalidOperation,
            span.clone(),
            "`impl Drop` is not supported (its `drop` method would not run); use \
             `#[resource]` with a `close()` method for deterministic cleanup, or \
             rely on automatic field-wise drop",
        ));
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
        // The owner-qualified key (`{module}.{trait}::{method}`) is collision-free
        // even when two imported modules export a same-named trait. The bare
        // `{trait}::{method}` key is first-write-wins, so with a same-name
        // collision it holds whichever module registered first and silently
        // shadows the other's signature. Trait-conformance resolves an aliased
        // trait to its source owner and looks up this qualified key
        // authoritatively, so it must always be present when an owner is known.
        let owner_qualified_key = self
            .current_module_short()
            .map(|short| format!("{short}.{method_key}"));

        // Build the trait method's FnDecl once; reuse it for both the bare and
        // owner-qualified registrations. `Self::Bar` projection is active while
        // the signature resolves so `Self::Item` becomes a deferred
        // `Ty::AssocType` carrier instead of an opaque named type.
        let decl = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: hew_parser::ast::Visibility::Private,
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
            consumes_self: false,
        };

        let prev_trait_self = self
            .current_trait_for_self_projection
            .replace(trait_name.to_string());
        // Register the owner-qualified key first (collision-free) regardless of
        // whether the bare key is already taken by another module's same-named
        // trait, so the authoritative lookup never misses for a known owner.
        if let Some(qualified_key) = owner_qualified_key.as_ref() {
            if !self.fn_sigs.contains_key(qualified_key) {
                self.register_fn_sig_with_name(qualified_key, &decl);
            }
        }
        // The bare key keeps first-write-wins for the local/non-aliased path.
        if !self.fn_sigs.contains_key(&method_key) {
            self.register_fn_sig_with_name(&method_key, &decl);
        }
        self.current_trait_for_self_projection = prev_trait_self;
    }

    /// Collect the full resolver scope for declared type params: trait-bound
    /// names plus any associated-type bindings attached to those bounds.
    pub(super) fn collect_type_param_scope_with_assoc_bindings(
        &mut self,
        type_params: Option<&Vec<TypeParam>>,
        where_clause: Option<&WhereClause>,
        hole_vars: &mut Vec<TypeVar>,
    ) -> TypeParamScope {
        let bounds = self.collect_type_param_scope_with_bounds(type_params, where_clause);
        let pushed_bounds_for_assoc = !bounds.is_empty();
        if pushed_bounds_for_assoc {
            self.current_type_param_bounds
                .push(TypeParamScope::new(bounds.clone(), HashMap::new()));
        }
        let assoc_bindings =
            self.collect_type_param_assoc_bindings(type_params, where_clause, hole_vars);
        if pushed_bounds_for_assoc {
            self.current_type_param_bounds.pop();
        }
        TypeParamScope::new(bounds, assoc_bindings)
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

    pub(super) fn collect_type_param_assoc_bindings(
        &mut self,
        type_params: Option<&Vec<TypeParam>>,
        where_clause: Option<&WhereClause>,
        hole_vars: &mut Vec<TypeVar>,
    ) -> HashMap<(String, String, String), Ty> {
        let mut bindings = HashMap::new();
        let mut declared = HashSet::new();
        if let Some(params) = type_params {
            for param in params {
                declared.insert(param.name.clone());
                for bound in &param.bounds {
                    self.collect_type_param_bound_assoc_bindings(
                        &param.name,
                        bound,
                        &mut bindings,
                        hole_vars,
                    );
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
                    for bound in &predicate.bounds {
                        self.collect_type_param_bound_assoc_bindings(
                            name,
                            bound,
                            &mut bindings,
                            hole_vars,
                        );
                    }
                }
            }
        }
        bindings
    }

    fn collect_type_param_bound_assoc_bindings(
        &mut self,
        param_name: &str,
        bound: &TraitBound,
        bindings: &mut HashMap<(String, String, String), Ty>,
        hole_vars: &mut Vec<TypeVar>,
    ) {
        for binding in &bound.assoc_type_bindings {
            let key = (
                param_name.to_string(),
                bound.name.clone(),
                binding.name.clone(),
            );
            bindings
                .entry(key)
                .or_insert_with(|| self.resolve_registered_annotation_ty(&binding.ty, hole_vars));
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

    /// Ingest a `#[extern_symbol("…")]` attribute (Stage 2 of W3.001).
    ///
    /// Stage 1 already validated the attribute's **attachment position**
    /// (parser rejects it on free fns, actors, trait fns,
    /// type-decl methods). This helper runs at FnSig-ingest time on
    /// the surviving attachment sites (extern `"C"` block fns,
    /// inherent impl methods, trait-impl methods) and:
    ///
    /// 1. Finds the (at most one) `extern_symbol` attribute.
    /// 2. Parses its template via
    ///    [`crate::extern_symbol::ExternSymbolTemplate::parse`].
    /// 3. On success returns a populated
    ///    [`crate::extern_symbol::ExternSymbolSpec`].
    /// 4. On failure emits a span-anchored
    ///    [`TypeErrorKind::InvalidExternSymbolTemplate`] diagnostic
    ///    and returns `None` (fail-closed: the `FnSig` records no
    ///    template, so Stage-3 monomorphic dispatch will surface the
    ///    same call site as an unresolved-symbol diagnostic rather
    ///    than silently routing through a malformed template).
    ///
    /// Returns `None` when no `extern_symbol` attribute is present —
    /// the normal case for ordinary functions and methods.
    pub(super) fn ingest_extern_symbol_attrs(
        &mut self,
        attrs: &[Attribute],
    ) -> Option<crate::extern_symbol::ExternSymbolSpec> {
        let attr = attrs.iter().find(|a| a.name == "extern_symbol")?;
        // Stage 1 parser accepts only a single positional string argument
        // for `#[extern_symbol("...")]` (see hew-parser tests at
        // `extern_symbol_attribute_on_*_is_captured`). If a future
        // parser regression lets a malformed shape through, fail closed
        // with a precise diagnostic rather than panic.
        let raw_payload = match attr.args.as_slice() {
            [AttributeArg::Positional(s)] => s.as_str(),
            [] => {
                self.errors.push(TypeError::new(
                    TypeErrorKind::InvalidExternSymbolTemplate {
                        reason: "missing template string — expected `#[extern_symbol(\"...\")]`"
                            .to_string(),
                    },
                    attr.span.clone(),
                    "`#[extern_symbol]` requires a single string argument naming the C-ABI \
                     runtime symbol (with optional `{T}` placeholders for per-monomorphization \
                     dispatch)"
                        .to_string(),
                ));
                return None;
            }
            _ => {
                self.errors.push(TypeError::new(
                    TypeErrorKind::InvalidExternSymbolTemplate {
                        reason: "expected exactly one positional string argument".to_string(),
                    },
                    attr.span.clone(),
                    "`#[extern_symbol(\"hew_symbol\")]` accepts exactly one positional string \
                     argument; multi-argument and key-value forms are not part of the W3.001 \
                     grammar"
                        .to_string(),
                ));
                return None;
            }
        };
        match crate::extern_symbol::ExternSymbolTemplate::parse(raw_payload) {
            Ok(template) => Some(crate::extern_symbol::ExternSymbolSpec {
                template,
                span: attr.span.clone(),
            }),
            Err(err) => {
                let reason = err.reason();
                self.errors.push(TypeError::new(
                    TypeErrorKind::InvalidExternSymbolTemplate {
                        reason: reason.clone(),
                    },
                    attr.span.clone(),
                    format!("invalid `#[extern_symbol]` template: {reason}"),
                ));
                None
            }
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
        // Validate that no bound carries unsupported positional type arguments
        // (e.g. `T: Eq<U>`) before `collect_type_param_bounds` erases them.
        self.validate_fn_type_param_bound_shapes(fd);
        // Push the type-param bounds map BEFORE resolving the signature so
        // the resolver can validate `T::Bar` projections that appear in
        // param/return types. Includes type params with no bounds so the
        // resolver can distinguish "in scope with no bounds" (emit
        // missing-bound diagnostic) from "not in scope" (fall through).
        let mut hole_vars = Vec::new();
        let fn_scope = self.collect_type_param_scope_with_assoc_bindings(
            fd.type_params.as_ref(),
            fd.where_clause.as_ref(),
            &mut hole_vars,
        );
        let pushed_bounds = !fn_scope.bounds.is_empty();
        if pushed_bounds {
            self.current_type_param_bounds.push(fn_scope.clone());
        }
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

        let fn_assoc_bindings = fn_scope.assoc_bindings;
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
            doc_comment: fd.doc_comment.clone(),
            extern_symbol: self.ingest_extern_symbol_attrs(&fd.attributes),
            // Receiver mutability flag — see `FnSig::requires_mutable_receiver`.
            // Only methods (Type::method) can carry a receiver; free functions
            // whose first parameter happens to be named `self` are not methods
            // in this sense (matches the `skip` logic above).
            requires_mutable_receiver: is_method
                && fd
                    .params
                    .first()
                    .is_some_and(|p| self.is_receiver_param(p) && p.is_mutable),
            ..FnSig::default()
        };

        let key = scoped_module_item_name(self.current_module.as_deref(), name)
            .unwrap_or_else(|| name.to_string());
        self.fn_sigs.insert(key.clone(), sig);
        self.fn_type_param_assoc_bindings
            .insert(key.clone(), fn_assoc_bindings);
        self.record_fn_sig_inference_holes(&key, hole_vars);
        // If the declaration carries `#[intrinsic("name")]`, validate its
        // placement and (if accepted) record the mapping so HIR lowering can
        // skip the body and wire to the catalog.
        if let Some(intrinsic_key) = &fd.intrinsic {
            self.register_intrinsic_declaration(key, intrinsic_key, name, fd);
        }
    }

    /// Validate a `#[intrinsic("…")]` declaration's placement and, if it lives
    /// in a stdlib-floor module **and** is a top-level free function, record
    /// the name→key mapping consumed by HIR lowering.
    ///
    /// P0 surface-immutability gate (A605, plan §7 risk 4): the `#[intrinsic]`
    /// surface is compiler-internal-only. Any declaration outside the
    /// designated stdlib-floor modules — including the root/user module — is a
    /// hard `E_INTRINSIC_OUTSIDE_FLOOR` error so a user program (or any
    /// non-floor module) cannot wire itself to a compiler intrinsic. Fail-closed:
    /// every non-allowlisted module path is rejected, never silently allowed.
    ///
    /// **Two-axis gate** — this is the single complete enforcement point:
    ///
    /// * **(a) Module axis**: the current module must be on the floor allowlist.
    ///   `#[intrinsic]` in the root/user module or any non-floor module is
    ///   rejected with `E_INTRINSIC_OUTSIDE_FLOOR`.
    ///
    /// * **(b) Shape axis**: the `key` must be a top-level free-function name
    ///   (no `::` separator). A method key of the form `"Type::method"` — which
    ///   arises for impl methods, actor methods, and trait-impl methods that flow
    ///   through `register_fn_sig_with_name` — is rejected with
    ///   `E_INTRINSIC_ON_METHOD`, regardless of module. Compiler intrinsics are
    ///   wired to standalone catalog entries; they are never method dispatch
    ///   slots.
    ///
    /// A declaration that passes both axes is inserted into
    /// `intrinsic_declarations`. A rejected declaration is never inserted, so
    /// it cannot become a live intrinsic dispatch target.
    fn register_intrinsic_declaration(
        &mut self,
        key: String,
        intrinsic_key: &str,
        name: &str,
        fd: &FnDecl,
    ) {
        // Shape axis (b): a key containing `::` is a method (e.g. `"Type::method"`).
        // Impl methods, actor methods, and trait-impl methods all reach here with
        // such a key via `register_fn_sig_with_name`.  Methods are never valid
        // intrinsic declarations, regardless of which module they live in.
        // Reject early and do NOT insert into `intrinsic_declarations`.
        if key.contains("::") {
            self.errors.push(TypeError {
                severity: crate::error::Severity::Error,
                kind: TypeErrorKind::IntrinsicOnMethod {
                    intrinsic_key: intrinsic_key.to_string(),
                    method_key: key.clone(),
                },
                span: fd.decl_span.clone(),
                message: format!(
                    "E_INTRINSIC_ON_METHOD: `#[intrinsic(\"{intrinsic_key}\")]` on \
                     `{name}` (key `{key}`) is declared on an impl method — \
                     the `#[intrinsic]` surface is valid only on top-level free \
                     functions inside a stdlib-floor module; method dispatch \
                     slots are never wired to compiler intrinsics (A605)"
                ),
                notes: vec![(
                    fd.decl_span.clone(),
                    "Compiler intrinsics are catalog entries keyed on bare function \
                     names; they cannot be dispatched through a receiver. Expose the \
                     intrinsic as a top-level free function in the floor module and \
                     call it from the impl method body if needed."
                        .to_string(),
                )],
                suggestions: vec![
                    "remove the `#[intrinsic(\"…\")]` attribute from this method".to_string(),
                    "if a new intrinsic is genuinely needed, declare it as a top-level \
                     free function in the appropriate stdlib-floor module"
                        .to_string(),
                ],
                source_module: self.current_module.clone(),
            });
            // Deliberately do NOT record the intrinsic mapping: a rejected
            // declaration must not become a live intrinsic dispatch target.
            return;
        }
        // Module axis (a): the declaration must live in a stdlib-floor module.
        if is_intrinsic_floor_module(self.current_module.as_deref()) {
            self.intrinsic_declarations
                .insert(key, intrinsic_key.to_string());
            return;
        }
        let module_label = self
            .current_module
            .clone()
            .unwrap_or_else(|| "(root)".to_string());
        self.errors.push(TypeError {
            severity: crate::error::Severity::Error,
            kind: TypeErrorKind::IntrinsicOutsideFloor {
                intrinsic_key: intrinsic_key.to_string(),
                module: module_label.clone(),
            },
            span: fd.decl_span.clone(),
            message: format!(
                "E_INTRINSIC_OUTSIDE_FLOOR: `#[intrinsic(\"{intrinsic_key}\")]` on \
                 `{name}` is declared in `{module_label}`, which is not a \
                 stdlib-floor module — the `#[intrinsic]` surface is \
                 compiler-internal-only and cannot be declared by user code"
            ),
            notes: vec![(
                fd.decl_span.clone(),
                "Memory and math intrinsics are wired by the compiler; user \
                 programs call the stdlib functions that the floor exposes, \
                 they never declare `#[intrinsic]` themselves. There is no \
                 user-visible `unsafe`/`@unsafe` surface (A605)."
                    .to_string(),
            )],
            suggestions: vec!["remove the `#[intrinsic(\"…\")]` attribute and call the \
                 corresponding stdlib function instead"
                .to_string()],
            source_module: self.current_module.clone(),
        });
        // Deliberately do NOT record the intrinsic mapping: a rejected
        // declaration must not become a live intrinsic dispatch target.
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
    #[allow(
        clippy::too_many_lines,
        reason = "single-source-of-truth for impl-method registration; \
                  factoring sub-passes would obscure the ordering invariants \
                  the surrounding code relies on (bounds push/pop, \
                  receiver-skip, double-write of fn_sigs + td.methods, \
                  W3.001 Stage-2 extern_symbol mirror)"
    )]
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
        let mut impl_scope_holes = Vec::new();
        let impl_bounds_map = self.collect_type_param_scope_with_assoc_bindings(
            impl_type_params,
            impl_where_clause,
            &mut impl_scope_holes,
        );
        let pushed_impl_bounds = !impl_bounds_map.bounds.is_empty();
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
            let mut impl_scope_holes = Vec::new();
            let impl_scope = self.collect_type_param_scope_with_assoc_bindings(
                impl_type_params,
                impl_where_clause,
                &mut impl_scope_holes,
            );
            let impl_bounds = impl_scope.bounds;
            let impl_assoc_bindings = impl_scope.assoc_bindings;
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
            let bindings = self.fn_type_param_assoc_bindings.entry(key).or_default();
            for (assoc_key, ty) in impl_assoc_bindings {
                bindings.entry(assoc_key).or_insert(ty);
            }
        }

        let mut impl_scope_holes = Vec::new();
        let impl_bounds_map = self.collect_type_param_scope_with_assoc_bindings(
            impl_type_params,
            impl_where_clause,
            &mut impl_scope_holes,
        );
        let pushed_impl_bounds = !impl_bounds_map.bounds.is_empty();
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
        // Re-use the structured `extern_symbol` already parsed by
        // `register_fn_sig_with_name` above. Re-parsing the attribute
        // here would emit duplicate `InvalidExternSymbolTemplate`
        // diagnostics for the same source span; cloning the resolved
        // spec keeps the diagnostic surface single-shot while still
        // propagating the field onto the `td.methods` entry consumed
        // by Stage-3 method-call rewrites.
        let extern_symbol = {
            let key = scoped_module_item_name(self.current_module.as_deref(), &method_key)
                .unwrap_or_else(|| method_key.clone());
            self.fn_sigs.get(&key).and_then(|s| s.extern_symbol.clone())
        };

        let sig = FnSig {
            type_params: all_type_params,
            type_param_bounds,
            param_names,
            params,
            return_type,
            is_async: method.is_async,
            extern_symbol,
            // Mirror `register_fn_sig_with_name`'s computation so that
            // `lookup_named_method_sig` (which prefers `td.methods` before
            // `fn_sigs`) returns a sig with the receiver-mutability flag
            // set. Without this, the call-site mutable-binding gate at
            // `methods.rs` (Q297 Stage 1) silently misses every trait impl
            // method on a user type.
            requires_mutable_receiver: method
                .params
                .first()
                .is_some_and(|p| self.is_receiver_param(p) && p.is_mutable),
            consumes_receiver: method.consumes_self,
            ..FnSig::default()
        };
        if let Some(td) = self.lookup_type_def_mut(type_name) {
            td.methods.insert(method.name.clone(), sig.clone());
        }
        // A `consuming self` inherent method moves its receiver at every call
        // site. Register the qualified `Type::method` name into the
        // consume-receiver set so the dispatch site marks the receiver moved
        // (a later use surfaces `UseAfterMove`) and records the per-call-site
        // flag HIR lowers as `IntentKind::Consume`. This is the single
        // authority the move-checker reads (`checker-output-boundary`); keyed
        // on the resolved-method consume fact, never re-inferred from AST shape
        // downstream.
        if method.consumes_self {
            self.consume_receiver_methods.insert(method_key.clone());
        }

        // Concrete-specialised-impl dual registration (#2270).
        //
        // When two `impl Trait for Wrapper<i64>` and `impl Trait for Wrapper<string>`
        // blocks are present, both produce `method_key = "Wrapper::describe"`.  The
        // second call clobbers the first in `fn_sigs`, and codegen later emits two
        // LLVM functions under the same name → linkage crash.
        //
        // Detection: no impl-level type params (concrete impl, not `impl<T>`),
        // AND the enclosing impl's self-type has non-empty concrete type args
        // (from `current_self_type`).  For such impls we ALSO register a
        // mangled-key entry (`"Wrapper$$i64::describe"`), and that mangled key is
        // what method-call dispatch and HIR will actually use.  The bare key entry
        // in `fn_sigs` is kept as a fallback for lookup paths that have not yet
        // been updated (e.g. method-set validation, external lookup).
        let is_concrete_specialised_impl = impl_type_params.is_none_or(Vec::is_empty); // None → no impl type params → concrete
        if is_concrete_specialised_impl {
            if let Some((self_type_name, self_type_args)) = &self.current_self_type.clone() {
                if self_type_name == type_name && !self_type_args.is_empty() {
                    // Try to resolve each type arg to a concrete `ResolvedTy` and
                    // compute the mangled self-type name.  Fails gracefully if any
                    // arg contains an inference variable or error node (which cannot
                    // appear for a concrete specialised impl, but we are defensive).
                    let resolved_args: Option<Vec<ResolvedTy>> = self_type_args
                        .iter()
                        .map(|ty| ResolvedTy::from_ty(ty).ok())
                        .collect();
                    if let Some(resolved_args) = resolved_args {
                        if let Some(mangled_self) = crate::resolved_ty::mangle_impl_self_name(
                            self_type_name,
                            &resolved_args,
                        ) {
                            let mangled_method_key = format!("{mangled_self}::{}", method.name);
                            // Register the full sig under the mangled key.  A
                            // previous concrete-impl registration may already be
                            // present; overwriting is correct because each impl
                            // block processes its own concrete args in sequence.
                            self.fn_sigs.insert(mangled_method_key.clone(), sig.clone());
                            // Propagate consume-receiver membership to the mangled key
                            // so HIR dispatch does not lose the move contract.
                            if method.consumes_self {
                                self.consume_receiver_methods.insert(mangled_method_key);
                            }
                        }
                    }
                }
            }
        }

        sig
    }

    /// Substitute trait-side type references into impl-side concrete types.
    ///
    /// Walks `ty` recursively and replaces:
    /// * `Ty::Named { name: "Self", args: [] }` → `impl_self`
    /// * `Ty::Named { name: <trait type param>, args: [] }` → the impl-supplied
    ///   type arg from `trait_param_map`
    /// * `Ty::AssocType { base: Self, trait_name == trait_name, assoc_name }`
    ///   → the impl's `type <assoc_name> = X` binding when present
    ///
    /// Used by [`Self::check_impl_method_against_trait`] to project the trait
    /// method's declared signature into the concrete shape the impl method
    /// must match. Returns the input unchanged for any subterm the
    /// substitution cannot resolve (so comparison errs on the side of
    /// accepting rather than firing on partial information).
    fn substitute_trait_sig_for_impl(
        &self,
        ty: &Ty,
        impl_self: &Ty,
        impl_target_name: &str,
        trait_name: &str,
        trait_param_map: &HashMap<String, Ty>,
    ) -> Ty {
        match ty {
            Ty::Named { name, args, .. } if args.is_empty() && name == "Self" => impl_self.clone(),
            Ty::Named { name, args, .. } if args.is_empty() => {
                if let Some(mapped) = trait_param_map.get(name) {
                    return mapped.clone();
                }
                ty.clone()
            }
            Ty::AssocType {
                base,
                trait_name: tn,
                assoc_name,
            } => {
                let base_is_self = matches!(&**base, Ty::Named { name, args, .. } if name == "Self" && args.is_empty());
                if base_is_self && tn.as_ref() == trait_name {
                    let key = (
                        impl_target_name.to_string(),
                        trait_name.to_string(),
                        assoc_name.as_ref().to_string(),
                    );
                    if let Some(resolved) = self.impl_assoc_type_bindings.get(&key) {
                        return resolved.clone();
                    }
                }
                let new_base = self.substitute_trait_sig_for_impl(
                    base,
                    impl_self,
                    impl_target_name,
                    trait_name,
                    trait_param_map,
                );
                Ty::AssocType {
                    base: Box::new(new_base),
                    trait_name: tn.clone(),
                    assoc_name: assoc_name.clone(),
                }
            }
            _ => ty.map_children_pub(&|child| {
                self.substitute_trait_sig_for_impl(
                    child,
                    impl_self,
                    impl_target_name,
                    trait_name,
                    trait_param_map,
                )
            }),
        }
    }

    /// Rename method-level type parameter names in `ty` from the trait's
    /// declared names to the impl's declared names, paired positionally.
    ///
    /// Accepts the legitimate case where an impl renames a trait's method
    /// type param (`fn map<T>` in the trait, `fn map<U>` in the impl): after
    /// renaming, `Ty::Named { name: "T" }` becomes `Ty::Named { name: "U" }`
    /// in the expected sig so structural equality with the impl sig holds.
    ///
    /// Only renames when both sides declare the same number of method-level
    /// type params. Skips otherwise so a separate arity-of-type-params
    /// diagnostic (future) is not preempted.
    fn rename_method_type_params(
        ty: &Ty,
        trait_method_tps: Option<&Vec<hew_parser::ast::TypeParam>>,
        impl_method_tps: Option<&Vec<hew_parser::ast::TypeParam>>,
    ) -> Ty {
        let trait_names: Vec<&str> = trait_method_tps
            .map(|v| v.iter().map(|tp| tp.name.as_str()).collect())
            .unwrap_or_default();
        let impl_names: Vec<&str> = impl_method_tps
            .map(|v| v.iter().map(|tp| tp.name.as_str()).collect())
            .unwrap_or_default();
        if trait_names.is_empty() || trait_names.len() != impl_names.len() {
            return ty.clone();
        }
        // Build the full rename map and substitute in parallel.  Sequential
        // substitution aliases entries when trait names and impl names
        // permute: renaming T→U then U→T would map both back to T.
        // Identity entries (t == u) are harmless to include.
        let subst_map: HashMap<String, Ty> = trait_names
            .iter()
            .zip(impl_names.iter())
            .map(|(t, u)| {
                (
                    (*t).to_string(),
                    Ty::Named {
                        builtin: None,
                        name: (*u).to_string(),
                        args: vec![],
                    },
                )
            })
            .collect();
        ty.substitute_named_params_parallel(&subst_map)
    }

    /// Resolve a trait reference as written in an `impl ... for ...` block to
    /// its OWNER-QUALIFIED identity, so trait conformance never keys off the
    /// bare `Trait::method` name. The bare name is polluted under same-name
    /// collisions: `register_trait_method_sig` writes the bare `fn_sigs` key
    /// first-write-wins, so when two imported modules (or an import plus a
    /// local declaration) share a trait name, the bare key holds whichever
    /// registered first and silently shadows the others. Resolution covers all
    /// three reference kinds uniformly:
    ///
    ///   * **aliased / imported-bare** — `published_bare_trait_owners` maps the
    ///     in-scope binding to its source identity `{module}.{Trait}`. The
    ///     owner-qualified `fn_sigs` key `{module}.{Trait}::{method}` is
    ///     collision-free (always registered for module traits).
    ///   * **local / root** — a trait declared in the importing program shadows
    ///     any imported same-name trait. The local `trait_defs[bare]` entry is
    ///     authoritative (last-write-wins), so its required method set and
    ///     signatures are derived from that `TraitInfo` directly, never from the
    ///     polluted bare `fn_sigs` key.
    ///   * **unambiguous single-owner import** — recovered by scanning
    ///     `trait_defs` for a single `{module}.{Trait}` qualified key.
    ///
    /// `trait_name` is the name as written (`A`, `Source`). Returns the resolved
    /// identity; `owner` is the defining module (`None` for a local/root trait
    /// or an unresolved name), and `is_local` records the local-shadow case so
    /// callers source the required-method set from the local `TraitInfo`.
    pub(super) fn resolve_trait_conformance_identity(
        &self,
        trait_name: &str,
    ) -> ResolvedTraitIdentity {
        // A primary trait reference (`impl <Trait> for ...`, a bound) is spelled
        // in the CURRENT module, so it resolves through the current scope: local
        // shadow first, then the importer's published-bare binding, then a
        // single-owner suffix scan. This is the `Current` arm of the one
        // canonical resolver — keeping every landed H1–H10 behaviour byte-for-byte.
        self.resolve_trait_ref(trait_name, TraitRefScope::Current)
    }

    /// Whether a primary trait reference resolves to a LOCAL declaration. Routes
    /// through the one canonical resolver so callers outside this module (the
    /// orphan rule) decide "is this trait local" by the same authoritative
    /// identity every trait-reference site uses, never the bare spelling.
    pub(super) fn trait_ref_is_local(&self, trait_name: &str) -> bool {
        self.resolve_trait_ref(trait_name, TraitRefScope::Current)
            .is_local
    }

    /// The owner-qualified SOURCE identity (`owner.Name`) a bare TRAIT reference
    /// resolves to when exactly one imported module PUBLISHED the bare binding
    /// into `importer` and a qualified def for it is registered; `None` otherwise
    /// (local shadow, zero/ambiguous publishers, or no registered def). The exact
    /// mirror of `published_bare_type_qualified` (there are no builtin traits, so
    /// no builtin-exempt branch). The stored value IS the source identity, so an
    /// aliased opt-in (`import m::{ T as U }`) binds `U` to `m.T`, never a
    /// reconstructed `m.U`.
    pub(super) fn published_bare_trait_qualified(
        &self,
        name: &str,
        importer: Option<&str>,
    ) -> Option<String> {
        if self.local_trait_defs.contains(name) {
            return None;
        }
        let identities = self
            .published_bare_trait_owners
            .get(&(importer.map(str::to_string), name.to_string()))?;
        if identities.len() != 1 {
            return None;
        }
        let qualified = identities.iter().next()?;
        self.trait_defs
            .contains_key(qualified)
            .then(|| qualified.clone())
    }

    /// THE canonical trait-reference resolver. Resolves a trait name spelled in a
    /// given scope to one owner-qualified `ResolvedTraitIdentity`, composing on
    /// the proven `published_bare_trait_owners` single-publisher-or-fail-closed
    /// primitive. Every trait reference — primary trait, bound, and supertrait
    /// edge — routes through here so identity is keyed by OWNER, never by the bare
    /// spelling (which is first-write-wins and polluted under same-name collisions).
    ///
    /// Scope is load-bearing:
    ///   * `Current` — the name is spelled in the importing program; a local
    ///     declaration shadows every imported same-name trait, and the importer's
    ///     own published-bare binding decides the owner.
    ///   * `Declaring { module }` — the name is a SUPERTRAIT edge spelled inside
    ///     `module`'s own declaration (`trait Sub: Base`). It is resolved through
    ///     `module`'s import bindings (the re-export chain to the original owner),
    ///     NEVER the importer's local trait of the same name — which is why the
    ///     local-shadow step is gated on `Current`.
    pub(super) fn resolve_trait_ref(
        &self,
        name: &str,
        scope: TraitRefScope<'_>,
    ) -> ResolvedTraitIdentity {
        // (1) LOCAL SHADOW — only in the CURRENT module's scope. A supertrait edge
        //     spelled in a DECLARING module is never the importer's local trait of
        //     the same name; gating this on `Current` is the H11 scope correctness.
        if matches!(scope, TraitRefScope::Current) && self.local_trait_defs.contains(name) {
            return ResolvedTraitIdentity {
                owner: None,
                source_trait_name: name.to_string(),
                is_local: true,
            };
        }

        // (2) DECLARING-module import binding (closes H11): a re-exported super
        //     edge follows the chain to its original owner. `reexsub`'s
        //     `import reexbase::{ Base }` records `("reexsub","Base") -> "reexbase.Base"`,
        //     so `Sub: Base` resolves `Base` to `reexbase.Base` regardless of what
        //     same-named `Base` the final importer has in scope.
        if let TraitRefScope::Declaring { module } = scope {
            if let Some(source_key) = self
                .trait_import_bindings
                .get(&(module.to_string(), name.to_string()))
            {
                return self.identity_from_trait_defs_key(source_key);
            }
        }

        // (3) PUBLISHED-BARE single-publisher (the promoted primitive), only in
        //     the current scope: a `Declaring` edge's own-module super and
        //     re-exports are handled by (2) and (4) and never consult the final
        //     importer's published map. Handles aliased / imported-bare.
        if matches!(scope, TraitRefScope::Current) {
            if let Some(qualified) =
                self.published_bare_trait_qualified(name, self.current_module.as_deref())
            {
                return self.identity_from_trait_defs_key(&qualified);
            }
        }

        // (4) UNAMBIGUOUS SINGLE-OWNER suffix scan: a non-aliased name whose
        //     binding == its source name, recovered from the single
        //     `{module}.{Trait}` qualified key in `trait_defs`. Also serves a
        //     `Declaring` edge whose super is the declaring module's OWN def
        //     (`{module}.Base` registered) — so no separate own-def-first branch.
        //     With zero or an ambiguous set, leave the owner unresolved and fall
        //     back to the source-name comparison (best-effort for an external
        //     reference not in the loaded graph; the downstream method-set / sig
        //     check fires honestly against an absent/empty set — fail-closed).
        let suffix = format!(".{name}");
        let mut owners: Vec<&str> = self
            .trait_defs
            .keys()
            .filter_map(|k| k.strip_suffix(&suffix))
            .filter(|module| !module.is_empty() && self.modules.contains(*module))
            .collect();
        owners.sort_unstable();
        owners.dedup();
        let owner = match owners.as_slice() {
            [single] => Some((*single).to_string()),
            _ => None,
        };
        ResolvedTraitIdentity {
            owner,
            source_trait_name: name.to_string(),
            is_local: false,
        }
    }

    /// The set of method names a trait requires an impl to provide, resolved
    /// through the trait's OWNER-QUALIFIED identity so a same-name collision
    /// cannot leak a neighbouring trait's method set. A method with a default
    /// body is optional (impls may omit it), so only bodyless methods of the
    /// resolved trait ITSELF are "required". The "known" set additionally
    /// includes the trait's whole super-trait chain: an impl of a sub-trait may
    /// legitimately provide a super-trait method inline (`trait Sub: Super` →
    /// `impl Sub for T { fn <super-method> … }`), so such a method must not be
    /// flagged as extraneous. Super-trait REQUIRED methods are NOT folded into
    /// `required` here — they are satisfied by a separate `impl Super for T` (the
    /// idiomatic form) or enforced at the bound site, and folding them in would
    /// falsely reject that separate-impl pattern.
    ///
    /// Returns `(required, known)`, or `None` when the trait reference does not
    /// resolve to a known trait (a separate diagnostic covers an unknown bound).
    fn trait_required_and_known_methods(
        &self,
        identity: &ResolvedTraitIdentity,
    ) -> Option<(HashSet<String>, HashSet<String>)> {
        // The one owner-qualified `trait_defs` key for this identity; collision-
        // free even when a same-name trait registered the bare key first. The
        // local-shadow and unresolved paths fall back to the identity's SOURCE
        // name (authoritative for a local trait, best-effort for an unresolved
        // reference) — derived by the single `trait_defs_key_for_identity` home.
        let lookup_key = self.trait_defs_key_for_identity(identity);
        let info = self.trait_defs.get(&lookup_key)?;

        let mut required = HashSet::new();
        let mut known = HashSet::new();
        for m in &info.methods {
            known.insert(m.name.clone());
            if m.body.is_none() {
                required.insert(m.name.clone());
            }
        }
        // Fold every (transitive) super-trait's declared methods into `known`
        // so an inline super-trait method is permitted, not flagged as extra.
        let mut visited: HashSet<String> = HashSet::new();
        self.collect_super_trait_method_names(&lookup_key, &mut known, &mut visited);
        Some((required, known))
    }

    /// Resolve a supertrait edge written inside `module_short` to its
    /// OWNER-QUALIFIED `trait_defs` key, through the one canonical resolver in the
    /// `Declaring` scope. A `trait Sub: Base` declaration spells `Base` bare, but
    /// it names the trait `module_short` itself resolves `Base` to: its OWN
    /// same-package `Base` (`{module_short}.Base`, found by the resolver's
    /// single-owner suffix scan), or a RE-IMPORTED `Base`
    /// (`import other::{ Base }`, followed through `module_short`'s import bindings
    /// to the original owner `other.Base`).
    ///
    /// The re-imported case is the H11 case the old `{module_short}.Base`-only
    /// check missed: there is no `{module_short}.Base` def for a re-imported super,
    /// so it fell back to the bare name and bound whatever `Base` the final
    /// importer had in scope (a collision-unsafe fail-open). When the super is
    /// genuinely external to the loaded graph, the resolver yields no owner and
    /// this returns the source name; the downstream method-set / signature check
    /// then fires honestly against an absent set — fail-closed, never accept-all.
    fn resolve_super_trait_edge(&self, module_short: &str, super_name: &str) -> String {
        let identity = self.resolve_trait_ref(
            super_name,
            TraitRefScope::Declaring {
                module: module_short,
            },
        );
        self.trait_defs_key_for_identity(&identity)
    }

    /// Walk the (transitive) super-trait chain of `trait_key`, inserting every
    /// super-trait's declared method name into `known`. `visited` guards against
    /// cycles. Super-trait edges are owner-qualified `trait_defs` keys (an
    /// imported `Sub`'s edge points at `{owner}.Base`, never the importer's bare
    /// `Base`; a local trait's edge is its bare local key, which is authoritative
    /// for a local trait), so the recursion resolves each super against its
    /// defining trait, collision-free.
    fn collect_super_trait_method_names(
        &self,
        trait_key: &str,
        known: &mut HashSet<String>,
        visited: &mut HashSet<String>,
    ) {
        if !visited.insert(trait_key.to_string()) {
            return;
        }
        let Some(supers) = self.trait_super.get(trait_key) else {
            return;
        };
        for super_name in supers.clone() {
            if let Some(super_info) = self.trait_defs.get(&super_name) {
                for m in &super_info.methods {
                    known.insert(m.name.clone());
                }
            }
            self.collect_super_trait_method_names(&super_name, known, visited);
        }
    }

    /// The `trait_defs` key for a resolved trait identity: the owner-qualified
    /// `{owner}.{source}` key when an owner resolved and that key is registered,
    /// otherwise the identity's SOURCE name (authoritative for a local trait, best
    /// effort for an unresolved reference). Mirrors the `lookup_key` derivation in
    /// `trait_required_and_known_methods`. The fallback is the source name — not
    /// the sub-trait the impl wrote — so a declaring SUPERTRAIT identity reached
    /// through the super chain keys on the supertrait's own name (`Base`), never
    /// the sub-trait's (`Sub`).
    fn trait_defs_key_for_identity(&self, identity: &ResolvedTraitIdentity) -> String {
        identity
            .owner
            .as_ref()
            .map(|owner| format!("{owner}.{}", identity.source_trait_name))
            .filter(|q| self.trait_defs.contains_key(q))
            .unwrap_or_else(|| identity.source_trait_name.clone())
    }

    /// The owner-qualified `trait_defs` key for a bare trait-bound name spelled in
    /// an `impl <Trait> for <Type>` (the CURRENT module's scope). The impl-side
    /// associated-type / default-method machinery (required-assoc enforcement,
    /// default-assoc collection, default-method registration) keys its
    /// `trait_defs` lookups on this instead of the bare `tb.name`, so a same-name
    /// trait imported by a *different* module cannot poison the global bare
    /// `trait_defs[name]` entry and let an impl skip a required `type` or inherit
    /// the wrong defaults. Routes through the one canonical conformance resolver,
    /// exactly as `check_impl_method_set_against_trait` does for the method set.
    pub(super) fn trait_defs_key_for_bound(&self, name: &str) -> String {
        let identity = self.resolve_trait_conformance_identity(name);
        self.trait_defs_key_for_identity(&identity)
    }

    /// Resolve which trait DECLARES `method_name` for an `impl <Trait>`: the
    /// primary trait when it declares the method directly, otherwise the
    /// supertrait in the OWNER-QUALIFIED super chain that declares it (an impl of
    /// a sub-trait may provide an inherited supertrait method inline). Returns the
    /// declaring trait's owner-qualified identity so the caller's signature
    /// lookup and trait-owner canonicalization key off the SUPERTRAIT's owner —
    /// not the sub-trait's — when the method is inherited. Without this, an inline
    /// supermethod is never found on the primary trait and its signature goes
    /// unchecked (a fail-open: a wrong-signature inherited method is accepted).
    ///
    /// `primary_key` is the primary trait's `trait_defs` key (owner-qualified).
    /// The walk follows `trait_super` (owner-qualified edges) and matches each
    /// super against its `trait_defs` entry, so a same-name supertrait collision
    /// resolves through the defining owner, never the importer namespace.
    fn resolve_declaring_trait_identity(
        &self,
        primary_identity: &ResolvedTraitIdentity,
        primary_key: &str,
        method_name: &str,
    ) -> Option<ResolvedTraitIdentity> {
        if self
            .trait_defs
            .get(primary_key)
            .is_some_and(|info| info.methods.iter().any(|m| m.name == method_name))
        {
            return Some(ResolvedTraitIdentity {
                owner: primary_identity.owner.clone(),
                source_trait_name: primary_identity.source_trait_name.clone(),
                is_local: primary_identity.is_local,
            });
        }
        let mut visited: HashSet<String> = HashSet::new();
        let mut stack: Vec<String> = self
            .trait_super
            .get(primary_key)
            .cloned()
            .unwrap_or_default();
        while let Some(super_key) = stack.pop() {
            if !visited.insert(super_key.clone()) {
                continue;
            }
            let declares = self
                .trait_defs
                .get(&super_key)
                .is_some_and(|info| info.methods.iter().any(|m| m.name == method_name));
            if declares {
                return Some(self.identity_from_trait_defs_key(&super_key));
            }
            if let Some(supers) = self.trait_super.get(&super_key) {
                stack.extend(supers.iter().cloned());
            }
        }
        None
    }

    /// Recover a trait identity from a `trait_defs` key. An owner-qualified
    /// `{module}.{Trait}` key whose module is in scope yields
    /// `owner = Some(module)`, `source = Trait`; a bare key yields a local
    /// identity (`is_local = true`, no owner). Used to re-anchor the signature
    /// lookup on a declaring SUPERTRAIT reached through the super chain.
    fn identity_from_trait_defs_key(&self, key: &str) -> ResolvedTraitIdentity {
        match key.split_once('.') {
            Some((module, source)) if self.modules.contains(module) => ResolvedTraitIdentity {
                owner: Some(module.to_string()),
                source_trait_name: source.to_string(),
                is_local: false,
            },
            _ => ResolvedTraitIdentity {
                owner: None,
                source_trait_name: key.to_string(),
                is_local: true,
            },
        }
    }

    /// Validate that an `impl <Trait> for <Type>` provides EXACTLY the trait's
    /// method set: every required (bodyless) trait method present, and no method
    /// that is not declared on the trait. Keyed off the trait's owner-qualified
    /// identity (`resolve_trait_conformance_identity`), so a same-name trait
    /// collision can never leak a neighbour's method set into the comparison.
    /// Per-method signature equivalence is enforced separately by
    /// `check_impl_method_against_trait`.
    pub(super) fn check_impl_method_set_against_trait(
        &mut self,
        type_name: &str,
        trait_bound: &TraitBound,
        impl_methods: &[FnDecl],
        impl_span: &Span,
    ) {
        let trait_name = &trait_bound.name;
        let identity = self.resolve_trait_conformance_identity(trait_name);
        let Some((required, known)) = self.trait_required_and_known_methods(&identity) else {
            return;
        };

        let provided: HashSet<&str> = impl_methods.iter().map(|m| m.name.as_str()).collect();

        // INHERITED method names: every method declared by a (transitively
        // reachable) SUPERTRAIT of this trait. A sub-trait may REDECLARE its
        // supertrait's methods (`trait Sub: Base { fn base(self); ... }`), which
        // makes them bodyless-required on `Sub` even though they belong to
        // `Base`. Such an inherited requirement is satisfied by a SEPARATE
        // `impl Base for T` block (not the `impl Sub for T` block), exactly as a
        // non-redeclared inherited method is — supertrait conformance is resolved
        // lazily at the call site (`no method X on T` if no impl supplies it),
        // never eagerly forced onto the sub-trait's own impl block. Dropping an
        // inherited method from `missing` here keeps redeclared and non-redeclared
        // inherited methods behaving identically; the per-method signature check
        // (`resolve_declaring_trait_identity`) still validates any inline copy.
        let lookup_key = self.trait_defs_key_for_identity(&identity);
        let mut inherited: HashSet<String> = HashSet::new();
        let mut inherited_visited: HashSet<String> = HashSet::new();
        self.collect_super_trait_method_names(&lookup_key, &mut inherited, &mut inherited_visited);

        // Missing: a required (bodyless) trait method the impl never provided AND
        // that is not inherited from a supertrait (the supertrait's own impl
        // block carries that obligation).
        let mut missing: Vec<String> = required
            .iter()
            .filter(|name| !provided.contains(name.as_str()))
            .filter(|name| !inherited.contains(name.as_str()))
            .cloned()
            .collect();
        missing.sort_unstable();
        if !missing.is_empty() {
            self.report_error_with_note(
                TypeErrorKind::TraitImplMissingMethods {
                    trait_name: trait_name.clone(),
                    type_name: type_name.to_string(),
                    methods: missing.clone(),
                },
                impl_span,
                format!(
                    "impl `{trait_name}` for `{type_name}` is missing required method(s): {}",
                    missing.join(", ")
                ),
                impl_span,
                format!(
                    "trait `{trait_name}` requires {}",
                    if missing.len() == 1 {
                        format!("method `{}`", missing[0])
                    } else {
                        format!("methods {}", missing.join(", "))
                    }
                ),
            );
        }

        // Extra: an impl method that is not declared on the trait at all.
        let mut extra: Vec<String> = impl_methods
            .iter()
            .map(|m| m.name.clone())
            .filter(|name| !known.contains(name))
            .collect();
        extra.sort_unstable();
        extra.dedup();
        if !extra.is_empty() {
            self.report_error_with_note(
                TypeErrorKind::TraitImplExtraMethods {
                    trait_name: trait_name.clone(),
                    type_name: type_name.to_string(),
                    methods: extra.clone(),
                },
                impl_span,
                format!(
                    "impl `{trait_name}` for `{type_name}` declares method(s) not on the trait: {}",
                    extra.join(", ")
                ),
                impl_span,
                format!("trait `{trait_name}` declares no such method(s)"),
            );
        }
    }

    /// Enforce that an impl method's signature matches the declared trait
    /// method's signature, after substituting `Self`, trait type parameters,
    /// and the impl's associated-type aliases. Q004 / LESSONS
    /// `diagnostic-trust`: emits at the impl method's local span so the user
    /// sees the actual divergence instead of a confusing
    /// "type does not satisfy trait" cascaded from a later call site.
    ///
    /// Silent (no diagnostic) when:
    /// * the trait method is not declared (the impl-site method-SET check in
    ///   `check_impl_method_set_against_trait` reports an extra method; this
    ///   per-method check only enforces equivalence of methods the trait
    ///   declares);
    /// * the trait signature was not registered (already produced a
    ///   diagnostic, would double-fire);
    /// * any side of the comparison contains `Ty::Error` (cascading
    ///   suppression — see the `cascading-Ty::Error` invariant);
    /// * the receiver-skip stripped a different number of params on either
    ///   side because the impl elided the receiver (treat as receiver-kind
    ///   mismatch and report).
    #[allow(
        clippy::too_many_lines,
        reason = "single-source-of-truth for the impl-vs-trait sig comparison; \
                  factoring would obscure the substitution / projection / \
                  renaming order that the comparison depends on"
    )]
    pub(super) fn check_impl_method_against_trait(
        &mut self,
        type_name: &str,
        self_type_args: &[Ty],
        trait_bound: &TraitBound,
        method: &FnDecl,
        impl_sig: &FnSig,
    ) {
        // Two `Ty::Named` that share a name + args but disagree only on the
        // `builtin` discriminator denote the same nominal type: the tag is a
        // derived property of the name, stamped when a type resolves against a
        // canonical builtin source and left `None` when the same name resolves
        // against its in-scope user definition. The std dual-surface error
        // enums (`CloseError`, `SendError`, …) hit this — a trait method
        // declared in `std/io/closable.hew` carries the local-enum form
        // (`builtin: None`) while an `impl Closable` in another module resolves
        // the bare name to the builtin surface (`builtin: Some(CloseError)`).
        // Re-derive the tag from the name on both sides so trait-conformance
        // compares nominal identity rather than the incidental resolution path.
        //
        // Under qualified-by-default the trait declaration records its sibling
        // types by their BARE name (as written inside the defining module) while
        // an importer's `impl` spells the same type through its module qualifier
        // (`closable.CloseError`). These name the one type, so both spellings
        // must canonicalize to a single DEFINING-MODULE-qualified identity before
        // the comparison — never to a bare name. Stripping any known-module
        // prefix and comparing bare names is unsound: it collapses two distinct
        // nominal types that merely share a bare name across modules
        // (`closableerr.CloseError` vs `closableerr2.CloseError`), accepting an
        // impl that returns the wrong module's type. Instead:
        //   * an already module-qualified name keeps its qualifier (it is an
        //     explicit, unambiguous identity);
        //   * a bare name written in the TRAIT DECLARATION denotes the trait's
        //     own defining module's type, so it ALWAYS qualifies against that
        //     module when the module defines it. The trait side is canonicalized
        //     with `preserve_local_shadow = false` — the importer's local type
        //     names are irrelevant to what the trait declaration requires;
        //   * a bare name on the IMPL/ACTUAL side is canonicalized with
        //     `preserve_local_shadow = true`: if it shadows a local type in the
        //     impl's scope it stays bare so the local identity is preserved (and
        //     so a local `CloseError` correctly MISMATCHES the trait's
        //     `closableerr.CloseError` rather than being conflated with it).
        //
        // The carve-out MUST be side-specific. Applying the local-shadow filter
        // to BOTH sides with one shared predicate is fail-open: the trait's bare
        // `CloseError` would also be left bare when the importer has a local
        // `CloseError`, so it would compare EQUAL to the impl's local type
        // instead of to the trait owner's required `closableerr.CloseError`,
        // falsely accepting a wrong-module impl.
        //
        // The user-facing diagnostics still render the original, untouched types.
        //
        // `trait_owner` is the trait's defining module (`Some("closableerr")`)
        // or `None` for a root/local trait. `ctx` carries the in-scope module
        // set, the registered-type predicate, and the local-shadow predicate so
        // the recursion needs no `&self` borrow held across the later mutable
        // error-reporting calls. `preserve_local_shadow` selects the side.
        fn canonicalize_type_identity(
            ty: &Ty,
            ctx: &TraitSigCanonCtx,
            preserve_local_shadow: bool,
        ) -> Ty {
            let rec = |t: &Ty| canonicalize_type_identity(t, ctx, preserve_local_shadow);
            match ty {
                Ty::Tuple(elems) => Ty::Tuple(elems.iter().map(rec).collect()),
                Ty::Array(elem, n) => Ty::Array(Box::new(rec(elem)), *n),
                Ty::Slice(elem) => Ty::Slice(Box::new(rec(elem))),
                Ty::Named { name, args, .. } => {
                    let canonical = match name.split_once('.') {
                        // Already module-qualified by a known module: keep the
                        // qualifier as the type identity.
                        Some((module, _)) if ctx.modules.contains(module) => name.clone(),
                        // Bare name: qualify against the trait's defining module
                        // when that module defines it. On the impl/actual side a
                        // local shadow is preserved (left bare); on the trait
                        // side the local-shadow carve-out does NOT apply, so a
                        // bare trait-declared name always qualifies to its owner.
                        // Otherwise (builtin, type param, or — on the impl side —
                        // a genuine local) leave it bare so its identity survives.
                        _ => ctx
                            .trait_owner
                            .filter(|_| !(preserve_local_shadow && (ctx.is_local)(name)))
                            .map(|owner| format!("{owner}.{name}"))
                            .filter(|qualified| (ctx.defines_qualified)(qualified))
                            .unwrap_or_else(|| name.clone()),
                    };
                    // Primitive types (i64, bool, f64, …) are represented in two
                    // ways: as the flat `Ty::I64` / `Ty::Bool` / … variants (from
                    // `resolve_type_expr` hitting the `Ty::from_name` fast-path)
                    // and as `Ty::Named { name: "i64", builtin: Some(I64), … }`
                    // (from `Ty::normalize_named` when a `Self` annotation is
                    // eagerly substituted via `current_self_type` during
                    // `lookup_trait_method` resolution).  Both representations are
                    // semantically identical, but `Ty::Named { … } != Ty::I64` as
                    // Rust enum discriminants, so trait-impl signature comparison
                    // falsely rejects them.
                    //
                    // Collapsing the canonical name to the flat primitive variant
                    // here is the canonical reconcile point: both the expected
                    // (trait) and actual (impl) sides pass through this function
                    // before comparison, so a single normalization here handles
                    // every path (fn_sigs registered before impl, lookup_trait_method
                    // eager substitution, and substitute_trait_sig_for_impl output).
                    // Only fires for zero-arg names (primitives never carry type args).
                    let canonical_args = args.iter().map(rec).collect::<Vec<_>>();
                    if canonical_args.is_empty() {
                        if let Some(prim) = Ty::from_name(&canonical) {
                            return prim;
                        }
                    }
                    Ty::normalize_named(canonical, canonical_args)
                }
                Ty::Function { params, ret } => Ty::Function {
                    params: params.iter().map(rec).collect(),
                    ret: Box::new(rec(ret)),
                },
                Ty::Closure {
                    params,
                    ret,
                    captures,
                } => Ty::Closure {
                    params: params.iter().map(rec).collect(),
                    ret: Box::new(rec(ret)),
                    captures: captures.iter().map(rec).collect(),
                },
                Ty::Pointer {
                    is_mutable,
                    pointee,
                } => Ty::Pointer {
                    is_mutable: *is_mutable,
                    pointee: Box::new(rec(pointee)),
                },
                Ty::Borrow { pointee } => Ty::Borrow {
                    pointee: Box::new(rec(pointee)),
                },
                Ty::Task(inner) => Ty::Task(Box::new(rec(inner))),
                other => other.clone(),
            }
        }

        let trait_name = trait_bound.name.clone();

        // Resolve the trait as written in the impl (`impl C for X`) to its
        // OWNER-QUALIFIED identity, uniformly across all three reference kinds
        // (aliased / imported-bare, local-root shadow, unambiguous single-owner
        // import). The bare `Trait::method` key is first-write-wins and pollutes
        // under same-name collisions, so it is NEVER the authority when the
        // identity resolves to a real owner or a local trait. See
        // `resolve_trait_conformance_identity`.
        let primary_identity = self.resolve_trait_conformance_identity(&trait_name);
        let primary_key = self.trait_defs_key_for_identity(&primary_identity);

        // An `impl Sub for T` (where `trait Sub: Base`) may provide an inherited
        // SUPERTRAIT method (`base`) inline. Resolve which trait actually DECLARES
        // this method — the primary trait, or the declaring supertrait reached
        // through the OWNER-QUALIFIED super chain — and key the signature check
        // off THAT trait's identity. Skipping inherited methods here is a
        // fail-open: a wrong-signature inline supermethod would never be compared.
        let Some(identity) =
            self.resolve_declaring_trait_identity(&primary_identity, &primary_key, &method.name)
        else {
            return;
        };
        // The declaring trait's `trait_defs` entry supplies the trait method AST
        // (its type params, span, and receiver shape) for the comparison below.
        let declaring_key = self.trait_defs_key_for_identity(&identity);
        let Some(trait_info) = self.trait_defs.get(&declaring_key).cloned() else {
            return;
        };
        let Some(trait_method) = trait_info
            .methods
            .iter()
            .find(|m| m.name == method.name)
            .cloned()
        else {
            return;
        };

        // Materialise the trait method's required signature through the resolved
        // identity, collision-free:
        //   * a resolved owner reads the owner-qualified `fn_sigs` key
        //     `m.Trait::method` (always registered for module traits);
        //   * a LOCAL trait derives the signature from its own `TraitInfo`
        //     method AST — the polluted bare `fn_sigs` key may hold an imported
        //     same-name trait's signature (first-write-wins), so it must never be
        //     consulted for a local trait;
        //   * an unresolved reference falls back to the scoped/bare key (the
        //     genuinely-unambiguous case, where no collision is possible).
        let trait_sig = if let Some(owner) = identity.owner.as_ref() {
            let owner_key = format!("{owner}.{}::{}", identity.source_trait_name, method.name);
            match self.fn_sigs.get(&owner_key).cloned() {
                Some(sig) => sig,
                None => return,
            }
        } else if identity.is_local {
            // A local/root trait resolves its required signature from its own
            // `trait_defs` entry (last-write-wins → authoritative) rather than the
            // polluted bare `fn_sigs` key. `lookup_trait_method` strips the
            // receiver and projects `Self::Bar`, mirroring what
            // `register_trait_method_sig` would have written. The DECLARING trait
            // is keyed (`identity.source_trait_name`): for an inline supermethod
            // of a local sub-trait this is the supertrait that declares it, not
            // the sub-trait written in the impl.
            match self.lookup_trait_method(&identity.source_trait_name, &method.name) {
                Some(sig) => sig,
                None => return,
            }
        } else {
            let trait_method_key = format!("{}::{}", identity.source_trait_name, method.name);
            let scoped_trait_key =
                scoped_module_item_name(self.current_module.as_deref(), &trait_method_key)
                    .unwrap_or_else(|| trait_method_key.clone());
            match self
                .fn_sigs
                .get(&scoped_trait_key)
                .or_else(|| self.fn_sigs.get(&trait_method_key))
                .cloned()
            {
                Some(sig) => sig,
                None => return,
            }
        };

        // The trait's defining module anchors how a bare type name written in
        // the trait declaration is canonicalized. A local/root trait has no
        // owner module (`None`), so its bare sibling-type names stay bare. For an
        // inline supermethod this is the SUPERTRAIT's owner (its declaration's
        // bare sibling types belong to its module), not the sub-trait's.
        let trait_owner_module: Option<String> = identity.owner.clone();

        // The DECLARING trait's source name drives `Self::Assoc` projection and
        // associated-type-binding lookup in `substitute_trait_sig_for_impl`. For
        // an inline supermethod this is the supertrait that declared the assoc
        // type, not the sub-trait written in the impl.
        let declaring_trait_name = identity.source_trait_name.clone();

        // Build trait-type-param substitution map.
        let mut trait_param_map: HashMap<String, Ty> = HashMap::new();
        if let Some(args) = trait_bound.type_args.as_ref() {
            for (param_name, arg_expr) in trait_info.type_params.iter().zip(args.iter()) {
                let resolved = self.resolve_type_expr(arg_expr);
                trait_param_map.insert(param_name.clone(), resolved);
            }
        }

        // Construct `impl_self` as the canonical `Ty` for the implementing
        // type. For primitive types (i64, bool, f64, …) `Ty::from_name`
        // returns the flat primitive variant (e.g. `Ty::I64`), which is what
        // the impl's annotation resolves to via
        // `resolve_registered_annotation_ty_no_holes`.  Using `Ty::Named` for
        // a primitive name produces a different enum variant than the impl's
        // resolved param type, causing a false "has type i64 but requires i64"
        // mismatch on non-receiver Self params.  `Ty::from_name` is the single
        // source of truth for primitive name → variant; non-primitive names
        // that have no flat variant (user-defined types, generics) take the
        // `Ty::Named` path as before.  Primitive types never carry type args,
        // so the from_name path only fires when self_type_args is empty.
        let impl_self = if self_type_args.is_empty() {
            Ty::from_name(type_name).unwrap_or_else(|| Ty::Named {
                builtin: None,
                name: type_name.to_string(),
                args: Vec::new(),
            })
        } else {
            Ty::Named {
                builtin: None,
                name: type_name.to_string(),
                args: self_type_args.to_vec(),
            }
        };

        // Materialise the expected impl-side signature.
        let expected_params: Vec<Ty> = trait_sig
            .params
            .iter()
            .map(|p| {
                let projected = self.substitute_trait_sig_for_impl(
                    p,
                    &impl_self,
                    type_name,
                    &declaring_trait_name,
                    &trait_param_map,
                );
                Self::rename_method_type_params(
                    &projected,
                    trait_method.type_params.as_ref(),
                    method.type_params.as_ref(),
                )
            })
            .collect();
        let expected_return = {
            let projected = self.substitute_trait_sig_for_impl(
                &trait_sig.return_type,
                &impl_self,
                type_name,
                &declaring_trait_name,
                &trait_param_map,
            );
            Self::rename_method_type_params(
                &projected,
                trait_method.type_params.as_ref(),
                method.type_params.as_ref(),
            )
        };

        // Cascading-Ty::Error suppression: if anything in expected or actual
        // is Error, skip — earlier diagnostics already explain the failure.
        let any_error = expected_params.iter().any(Ty::contains_error)
            || expected_return.contains_error()
            || impl_sig.params.iter().any(Ty::contains_error)
            || impl_sig.return_type.contains_error();
        if any_error {
            return;
        }

        let report_span = if method.decl_span.start != method.decl_span.end {
            method.decl_span.clone()
        } else if method.fn_span.start != method.fn_span.end {
            method.fn_span.clone()
        } else {
            // Defensive: if the parser left both blank, fall back to the trait
            // method span so the message still anchors to a real source range.
            trait_method.span.clone()
        };

        if expected_params.len() != impl_sig.params.len() {
            // Arity mismatch — also fires when the impl wrote a different
            // receiver shape (e.g. `(it: X)` vs `(self)`), because the impl's
            // non-Self first param is not detected as a receiver and so is
            // *not* skipped, producing a different post-skip arity.
            self.report_error_with_note(
                TypeErrorKind::TraitImplSignatureMismatch {
                    trait_name: trait_name.clone(),
                    method_name: method.name.clone(),
                    detail: "arity",
                },
                &report_span,
                format!(
                    "impl method `{type_name}::{}` has {} parameter(s) but trait `{trait_name}` declares {} \
                     (after substituting `Self` and projecting associated types)",
                    method.name,
                    impl_sig.params.len(),
                    expected_params.len(),
                ),
                &trait_method.span,
                format!(
                    "trait method `{trait_name}::{}` declared here",
                    method.name
                ),
            );
            return;
        }

        // Receiver-mutability axis (Q297 Stage 1): when both sides declare a
        // receiver, the `is_mutable` flag must match. A trait declaring
        // `fn next(var self)` and an impl declaring `fn next(self)` (or vice
        // versa) is a hard reject — the receiver-mutability axis is part of
        // the contract, not a free parameter the impl may choose.
        //
        // Determine each side's receiver-mutability flag by checking the
        // first parameter for receiver-shape. This mirrors how
        // `register_impl_method` and `register_fn_sig_with_name` already
        // detect-and-skip receivers when building the signature's params
        // list; the receiver's `is_mutable` flag is otherwise dropped on
        // the floor, which is precisely the contract gap this check closes.
        let trait_receiver_mut = trait_method
            .params
            .first()
            .is_some_and(|p| self.is_receiver_param(p) && p.is_mutable);
        let impl_receiver_mut = method
            .params
            .first()
            .is_some_and(|p| self.is_receiver_param(p) && p.is_mutable);
        if trait_receiver_mut != impl_receiver_mut {
            let (trait_shape, impl_shape) = if trait_receiver_mut {
                (
                    "`var self` (mutable receiver)",
                    "`self` (by-value receiver)",
                )
            } else {
                (
                    "`self` (by-value receiver)",
                    "`var self` (mutable receiver)",
                )
            };
            self.report_error_with_note(
                TypeErrorKind::TraitImplSignatureMismatch {
                    trait_name: trait_name.clone(),
                    method_name: method.name.clone(),
                    detail: "receiver mutability",
                },
                &report_span,
                format!(
                    "impl method `{type_name}::{}` declares {impl_shape} but trait `{trait_name}` requires {trait_shape}",
                    method.name,
                ),
                &trait_method.span,
                format!(
                    "trait method `{trait_name}::{}` declared here",
                    method.name
                ),
            );
            return;
        }

        // Canonicalize every comparison type to a defining-module-qualified
        // identity up front, holding the read-only `self` borrow only for this
        // block so the later mutable error reporting is unencumbered. The owned
        // canonical `Ty` values then drive the comparisons; the diagnostics
        // still render the original, un-canonicalized spellings.
        let (
            canon_expected_params,
            canon_actual_params,
            canon_expected_return,
            canon_actual_return,
        ) = {
            let ctx = TraitSigCanonCtx {
                modules: &self.modules,
                trait_owner: trait_owner_module.as_deref(),
                defines_qualified: &|qualified: &str| self.type_defs.contains_key(qualified),
                is_local: &|name: &str| {
                    self.local_type_defs.contains(name) || self.source_type_defs.contains(name)
                },
            };
            // EXPECTED is the trait declaration's required signature: a bare
            // name there denotes the trait owner's sibling type, so it ALWAYS
            // qualifies to the owner (`preserve_local_shadow = false`). The
            // importer's local type names do not change what the trait requires.
            let canon_expected_params: Vec<Ty> = expected_params
                .iter()
                .map(|t| canonicalize_type_identity(t, &ctx, false))
                .collect();
            // ACTUAL is the impl's written signature: a bare name that shadows a
            // local type keeps its local identity (`preserve_local_shadow =
            // true`), so a local `CloseError` correctly mismatches the trait's
            // `closableerr.CloseError` instead of being conflated with it.
            let canon_actual_params: Vec<Ty> = impl_sig
                .params
                .iter()
                .map(|t| canonicalize_type_identity(t, &ctx, true))
                .collect();
            let canon_expected_return = canonicalize_type_identity(&expected_return, &ctx, false);
            let canon_actual_return = canonicalize_type_identity(&impl_sig.return_type, &ctx, true);
            (
                canon_expected_params,
                canon_actual_params,
                canon_expected_return,
                canon_actual_return,
            )
        };

        for (i, (expected, actual)) in expected_params
            .iter()
            .zip(impl_sig.params.iter())
            .enumerate()
        {
            if canon_expected_params[i] != canon_actual_params[i] {
                let param_label = impl_sig.param_names.get(i).map_or_else(
                    || format!("parameter {}", i + 1),
                    |n| format!("parameter `{n}`"),
                );
                self.report_error_with_note(
                    TypeErrorKind::TraitImplSignatureMismatch {
                        trait_name: trait_name.clone(),
                        method_name: method.name.clone(),
                        detail: "parameter",
                    },
                    &report_span,
                    format!(
                        "impl method `{type_name}::{}` {param_label} has type `{}` but trait `{trait_name}` \
                         requires `{}`",
                        method.name,
                        actual.user_facing(),
                        expected.user_facing(),
                    ),
                    &trait_method.span,
                    format!(
                        "trait method `{trait_name}::{}` declared here",
                        method.name
                    ),
                );
                return;
            }
        }

        if canon_expected_return != canon_actual_return {
            self.report_error_with_note(
                TypeErrorKind::TraitImplSignatureMismatch {
                    trait_name: trait_name.clone(),
                    method_name: method.name.clone(),
                    detail: "return type",
                },
                &report_span,
                format!(
                    "impl method `{type_name}::{}` returns `{}` but trait `{trait_name}` requires `{}`",
                    method.name,
                    impl_sig.return_type.user_facing(),
                    expected_return.user_facing(),
                ),
                &trait_method.span,
                format!(
                    "trait method `{trait_name}::{}` declared here",
                    method.name
                ),
            );
        }
    }

    pub(super) fn record_trait_impl(&mut self, type_name: &str, trait_name: &str) {
        self.trait_impls_set
            .insert((type_name.to_string(), trait_name.to_string()));
    }

    pub(super) fn record_trait_impl_methods(
        &mut self,
        type_name: &str,
        trait_name: &str,
        method_names: impl IntoIterator<Item = String>,
    ) {
        let entry = self
            .trait_impl_method_names
            .entry((type_name.to_string(), trait_name.to_string()))
            .or_default();
        entry.extend(method_names);
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
        // First-wins, matching `record_primitive_trait_impl_self_args` and the
        // assoc-type binding table. Under Hew's coherence rule there is at most
        // one impl of a trait per constructor, so a second registration is
        // either the same impl reprocessed across phases or a rejected
        // conflicting impl (diagnosed at `record_trait_impl`); in neither case
        // may it overwrite the surviving impl's signature. Keeping every side
        // table first-wins guarantees the dispatched method signature and the
        // applicability proof (self-args) always come from the *same* impl, so
        // they cannot drift.
        self.primitive_trait_impls
            .entry((canonical_key, trait_name))
            .or_default()
            .entry(method_name)
            .or_insert(sig);
    }

    /// Record the impl's `Self` type arguments for an `impl <Trait> for
    /// <PrimitiveOrBuiltinGeneric>` so a later dispatch on a concrete receiver
    /// can bind the impl's type parameters (see
    /// [`Checker::primitive_trait_impl_self_args`]). Idempotent within one impl:
    /// every method of that impl records the SAME `Self` args, so the first
    /// recorded entry per `(canonical, trait)` wins.
    ///
    /// Coherence enforcement (single-impl-per-constructor): if a *different*
    /// `Self` shape is already recorded for this `(canonical, trait)`, two
    /// distinct impls target the same builtin constructor with the same trait —
    /// e.g. a blanket `impl<T> Acc for Vec<T>` (`self_args = [T]`) and a concrete
    /// `impl Acc for Vec<i64>` (`self_args = [i64]`). Hew has no specialization
    /// or overlapping impls (single-crate coherence, mission Q66.b), so the
    /// second impl is rejected at its declaration site with a clean diagnostic
    /// and does NOT overwrite the first (first-wins keeps the surviving impl's
    /// method signatures and this `Self`-arg applicability proof from the SAME
    /// impl, so they cannot drift).
    ///
    /// Comparing the `Self` shape (rather than a source span) makes this robust
    /// to the registration architecture re-processing the same impl across
    /// module/import phases: a reprocessed impl re-presents an *identical*
    /// `Self` shape and is correctly treated as the same impl, while a genuine
    /// overlap presents a *different* shape. It also naturally scopes the check
    /// to builtin/primitive receivers — user-record impls never reach this side
    /// table — which is exactly where the drift fail-open lived.
    ///
    /// KNOWN GAP (tracked): two *genuinely-distinct* impls that share an
    /// *identical* `Self` shape — e.g. two literal `impl<T> Acc for Vec<T>`
    /// blocks — are NOT rejected here (they compare shape-equal and are treated
    /// as a re-presentation). Closing this needs the impl's DEFINING identity,
    /// but the only readily-available per-impl span (`impl.target_type` span)
    /// cannot be used as the coherence key because the documented user-redeclare
    /// path lets a user `pub trait Display` shadow the prelude `Display`: the
    /// prelude `impl Display for i64` and the user `impl Display for i64` are
    /// distinct traits that collapse to the same `(canonical, "Display")` key,
    /// so a defining-span key would falsely reject that legal shadow. A correct
    /// fix must additionally key on the trait's DEFINING identity (not its
    /// name), which is a larger cross-cutting change tracked as a separate
    /// follow-up ("trait-impl coherence: reject duplicate same-(type,trait)
    /// impls via defining-identity"). The projection fix this method supports is
    /// unaffected: first-wins keeps the dispatched method signature and the
    /// applicability proof from the SAME (first) impl, so even an accepted
    /// duplicate cannot cause the mis-projection fail-open this fix closes.
    pub(super) fn record_primitive_trait_impl_self_args(
        &mut self,
        canonical_key: String,
        trait_name: String,
        self_args: Vec<Ty>,
        impl_span: &Span,
    ) {
        match self
            .primitive_trait_impl_self_args
            .get(&(canonical_key.clone(), trait_name.clone()))
        {
            None => {
                self.primitive_trait_impl_self_args
                    .insert((canonical_key, trait_name), self_args);
            }
            Some(existing) if existing == &self_args => {
                // Same impl (same Self shape), re-presented across a later
                // registration phase — not a conflict. (See KNOWN GAP above:
                // this also admits a genuine same-shape duplicate, tracked.)
            }
            Some(_) => {
                // A second, structurally-different impl of the same trait on the
                // same builtin constructor: an overlapping impl, which Hew does
                // not permit. Reject it; keep the first-registered impl.
                let dedup = (
                    canonical_key.clone(),
                    trait_name.clone(),
                    impl_span.start,
                    impl_span.end,
                );
                if self.conflicting_trait_impl_reported.insert(dedup) {
                    self.report_error(
                        TypeErrorKind::ConflictingTraitImpl {
                            trait_name: trait_name.clone(),
                            type_name: canonical_key.clone(),
                        },
                        impl_span,
                        format!(
                            "conflicting implementation of trait `{trait_name}` for `{canonical_key}`: \
                             a trait may be implemented at most once per type constructor \
                             (Hew has no specialization or overlapping impls)"
                        ),
                    );
                }
            }
        }
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

        let mut hole_vars = Vec::new();
        let rf_scope = self.collect_type_param_scope_with_assoc_bindings(
            rf.type_params.as_ref(),
            rf.where_clause.as_ref(),
            &mut hole_vars,
        );
        let pushed_rf_bounds = !rf_scope.bounds.is_empty();
        if pushed_rf_bounds {
            self.current_type_param_bounds.push(rf_scope.clone());
        }

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
            ..FnSig::default()
        };

        let method_name = format!("{}::{}", actor_name, rf.name);
        if rf.is_generator {
            self.receive_generator_methods.insert(method_name.clone());
        }
        self.actor_receive_methods.insert(method_name.clone());
        self.record_fn_sig_inference_holes(&method_name, hole_vars);
        self.fn_type_param_assoc_bindings
            .insert(method_name.clone(), rf_scope.assoc_bindings);
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
                extern_symbol: self.ingest_extern_symbol_attrs(&f.attributes),
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

    /// Registers synthetic `fn_sigs` entries for the channel layout-witness
    /// `send`/`recv`/`try_recv` entries, whose calling convention is handled
    /// entirely by codegen.
    ///
    /// The real runtime ABI carries an out-parameter and an element-layout
    /// witness pointer (`hew_channel_recv_layout(rx, out, witness)`), which
    /// cannot be expressed in an `extern "C"` block — codegen intercepts the
    /// call by name and emits the witness ABI. We register them here so the
    /// type checker can resolve the stdlib impl-body calls inside `unsafe`
    /// blocks (the declared `string` element types are placeholders; the
    /// intercept derives the element type from the call site).
    ///
    /// Only activates when we're actually in the channel module: the local
    /// module must define `Receiver` AND the extern block must have already
    /// registered the `hew_channel_new` constructor.
    pub(super) fn register_channel_recv_builtins(&mut self) {
        let marker_key = scoped_module_item_name(self.current_module.as_deref(), "hew_channel_new")
            .unwrap_or_else(|| "hew_channel_new".to_string());
        if !self.local_type_defs.contains("Receiver") || !self.fn_sigs.contains_key(&marker_key) {
            return;
        }

        let receiver_ty = Ty::Named {
            builtin: None,
            name: "Receiver".to_string(),
            args: vec![],
        };
        let sender_ty = Ty::Named {
            builtin: None,
            name: "Sender".to_string(),
            args: vec![],
        };

        let builtins: &[(&str, &str, Ty, Ty)] = &[
            (
                "hew_channel_recv_layout",
                "rx",
                receiver_ty.clone(),
                Ty::option(Ty::String),
            ),
            (
                "hew_channel_try_recv_layout",
                "rx",
                receiver_ty.clone(),
                Ty::option(Ty::String),
            ),
        ];

        for (name, param_name, param_ty, ret_ty) in builtins {
            let key = scoped_module_item_name(self.current_module.as_deref(), name)
                .unwrap_or_else(|| (*name).to_string());
            if self.fn_sigs.contains_key(&key) {
                continue;
            }
            let sig = FnSig {
                param_names: vec![(*param_name).to_string()],
                params: vec![param_ty.clone()],
                return_type: ret_ty.clone(),
                ..FnSig::default()
            };
            self.fn_sigs.insert(key.clone(), sig);
            self.unsafe_functions.insert(key);
        }

        // The typed-serialise send takes the value by reference plus the
        // witness in the real ABI; the placeholder 2-arg shape carries arity
        // for the stdlib impl body.
        let send_key =
            scoped_module_item_name(self.current_module.as_deref(), "hew_channel_send_layout")
                .unwrap_or_else(|| "hew_channel_send_layout".to_string());
        if !self.fn_sigs.contains_key(&send_key) {
            let sig = FnSig {
                param_names: vec!["tx".to_string(), "data".to_string()],
                params: vec![sender_ty, Ty::String],
                return_type: Ty::Unit,
                ..FnSig::default()
            };
            self.fn_sigs.insert(send_key.clone(), sig);
            self.unsafe_functions.insert(send_key);
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
                            type_params: wfn.type_params,
                            type_param_bounds: wfn.type_param_bounds,
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
                            self.register_stdlib_hew_items(
                                &short,
                                resolved_items,
                                StdlibBarePublication::Import(&decl.spec),
                            );
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
                                self.register_stdlib_hew_items(
                                    &short,
                                    &items,
                                    StdlibBarePublication::Prelude,
                                );
                            }
                        }
                    }

                    if module_path == "std::concurrency::lambda_actor"
                        && decl.resolved_items.is_none()
                    {
                        let identity = format!("module:{module_path}");
                        if !self
                            .registered_stdlib_hew_sources
                            .contains(identity.as_str())
                        {
                            self.registered_stdlib_hew_sources.insert(identity);
                            let parsed = hew_parser::parse(LAMBDA_ACTOR_HEW);
                            debug_assert!(
                                parsed.errors.is_empty(),
                                "std/concurrency/lambda_actor.hew failed to parse: {:?}",
                                parsed.errors,
                            );
                            if parsed.errors.is_empty() {
                                let items: Vec<_> = parsed.program.items.into_iter().collect();
                                self.register_stdlib_hew_items(
                                    &short,
                                    &items,
                                    StdlibBarePublication::Prelude,
                                );
                            }
                        }
                    }

                    self.handle_bearing_dirty = true;
                    return;
                }
                Some(format!(
                    "module file contains unsupported slice annotations in signature(s): {}. \
                     slice type composite lowering is not yet implemented",
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
                // The qualifier a bare reference reaches this module's names
                // through. A whole-module alias (`import path as m;`) overrides
                // the default last-segment qualifier, so the module's qualified
                // keys, exports, and importer bindings are all keyed on `m`.
                let short = decl
                    .module_alias
                    .clone()
                    .unwrap_or_else(|| decl.path.last().expect("import path is non-empty").clone());
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
                    // The full dot-path (e.g. "subpkg.helper") is the declaring-module
                    // identity used in access-check side tables.
                    let full_dot_path = decl.path.join(".");
                    self.register_user_module(&short, &full_dot_path, resolved_items, &decl.spec);
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

    /// Determine whether a SOURCE export named `name` is opted in by the
    /// `ImportSpec`. Matching is by SOURCE NAME only: in `import m::{ T as U }`,
    /// the export `T` is opted in and `U` is the binding it publishes under — a
    /// DISTINCT source export literally named `U` is NOT opted in by that alias.
    /// Matching `alias == name` here would falsely opt a real `U` in and publish
    /// it under `U` too, conflating two distinct nominal types. The alias affects
    /// only the binding name (`resolve_import_name`), never which source item the
    /// spec selects.
    #[expect(clippy::ref_option, reason = "avoids cloning the option contents")]
    pub(super) fn should_import_name(name: &str, spec: &Option<ImportSpec>) -> bool {
        match spec {
            None => false,                  // bare import → qualified only
            Some(ImportSpec::Glob) => true, // import foo::*; → everything
            Some(ImportSpec::Names(names)) => names.iter().any(|n| n.name == name),
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
        import_spec: StdlibBarePublication<'_>,
    ) {
        for (item, _span) in items {
            let Item::Import(decl) = item else {
                continue;
            };
            if decl.resolved_items.is_some() {
                // Load the imported stdlib module so its re-exported traits become
                // visible to the eager trait-use path. Pass `None` for the import
                // span deliberately: this import statement lives in a stdlib source
                // file, so its span indexes that file — not the user document the
                // diagnostics are reported against. Recording it in `import_spans`
                // would make it a user-facing unused-import lint candidate whose
                // span cannot be resolved to any user source, mis-attributing a
                // stdlib-internal offset to the user's document.
                let saved_current_module = self.current_module.clone();
                self.current_module = Some(module_short.to_string());
                self.register_import(decl, None);
                self.current_module = saved_current_module;
            }
        }

        self.record_trait_import_bindings(module_short, items);

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
                    // Record visibility for all TypeDecls (both pub and non-pub)
                    // so the enforcement check can distinguish "private" from "unknown".
                    let qualified_type = format!("{module_short}.{}", td.name);
                    self.type_visibility
                        .entry(qualified_type.clone())
                        .or_insert((td.visibility, Some(module_short.to_string())));
                    // Record the declaration span so E_VISIBILITY can point "declared
                    // here" at the actual declaration for both pub and non-pub types.
                    self.type_def_spans
                        .entry(qualified_type)
                        .or_insert_with(|| span.clone());
                    if !td.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_type_namespace_name(Some(module_short), &td.name, span) {
                        continue;
                    }
                    self.in_stdlib_registration = true;
                    self.register_type_decl(td);
                    self.in_stdlib_registration = false;
                    self.known_types.insert(td.name.clone());
                    // Qualified authority is always published, mirroring the
                    // user-module path: the qualified alias and the module-export
                    // record that drives the use-time gate's "exported by module
                    // X" diagnostic and ambiguity candidate naming.
                    self.register_qualified_type_alias(module_short, &td.name);
                    self.record_module_type_export(module_short, &td.name);
                    // The importer-scope bare binding obeys the qualified-by-
                    // default gate: `Prelude` (compiled-in bootstrap surfaces)
                    // always publishes bare; a real `import` publishes bare only
                    // on a named/glob/aliased opt-in, exactly like a user module.
                    if let Some(binding) = import_spec.bare_binding(&td.name) {
                        let source_identity = format!("{module_short}.{}", td.name);
                        self.record_published_bare_type(&binding, &source_identity);
                        self.unqualified_to_module.insert(
                            (self.current_module.clone(), binding),
                            module_short.to_string(),
                        );
                    }
                }
                Item::Machine(md) => {
                    // Record visibility for all Machines (both pub and non-pub).
                    let qualified_type = format!("{module_short}.{}", md.name);
                    self.type_visibility
                        .entry(qualified_type.clone())
                        .or_insert((md.visibility, Some(module_short.to_string())));
                    // Record the declaration span for non-pub machines.
                    self.type_def_spans
                        .entry(qualified_type)
                        .or_insert_with(|| span.clone());
                    if !md.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_machine_type_namespace_names(
                        Some(module_short),
                        &md.name,
                        span,
                    ) {
                        continue;
                    }
                    let event_name = format!("{}Event", md.name);
                    self.register_machine_decl(md, span);
                    self.known_types.insert(md.name.clone());
                    self.known_types.insert(event_name.clone());
                    self.register_qualified_type_alias(module_short, &md.name);
                    self.register_qualified_type_alias(module_short, &event_name);
                    self.record_module_type_export(module_short, &md.name);
                    self.record_module_type_export(module_short, &event_name);
                    // Bare publication of the machine and its companion event
                    // enum is gated together so a named/glob import exposes both
                    // or neither; `Prelude` publishes both unconditionally.
                    if let Some(binding) = import_spec.bare_binding(&md.name) {
                        let source_identity = format!("{module_short}.{}", md.name);
                        self.record_published_bare_type(&binding, &source_identity);
                        self.unqualified_to_module.insert(
                            (self.current_module.clone(), binding),
                            module_short.to_string(),
                        );
                    }
                    if let Some(binding) = import_spec.bare_binding(&event_name) {
                        let source_identity = format!("{module_short}.{event_name}");
                        self.record_published_bare_type(&binding, &source_identity);
                        self.unqualified_to_module.insert(
                            (self.current_module.clone(), binding),
                            module_short.to_string(),
                        );
                    }
                }
                Item::Trait(tr) => {
                    if let Some(supers) = &tr.super_traits {
                        for super_trait in supers {
                            self.mark_imported_trait_used_for_module_aliases(
                                module_short,
                                &super_trait.name,
                            );
                        }
                    }
                    // Record visibility for all traits (both pub and non-pub) so a
                    // cross-module qualified reference to a non-pub trait produces a
                    // precise E_VISIBILITY at the reference site instead of leaking an
                    // `E_MIR: unknown type` at the MIR boundary. Mirrors the TypeDecl
                    // and Machine registration above; traits share the type-namespace
                    // and the same qualified-reference enforcement path in resolution.
                    let qualified_type = format!("{module_short}.{}", tr.name);
                    self.type_visibility
                        .entry(qualified_type.clone())
                        .or_insert((tr.visibility, Some(module_short.to_string())));
                    self.type_def_spans
                        .entry(qualified_type)
                        .or_insert_with(|| span.clone());
                    if !tr.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_type_namespace_name(Some(module_short), &tr.name, span) {
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
                    let qualified = format!("{module_short}.{}", fd.name);
                    // Record visibility for all functions in the visibility table.
                    self.fn_visibility
                        .entry(qualified.clone())
                        .or_insert(fd.visibility);
                    if !self.fn_sigs.contains_key(&qualified) {
                        let (sig, assoc_bindings) = self.build_fn_sig_from_decl_with_assoc(fd);
                        // Only `Pub` functions are module exports: `package fn` must
                        // pass the access-allowed check at every call site, so it must
                        // NOT bypass the check by entering the exports set.  Non-pub
                        // functions are still registered into fn_sigs so the enforcement
                        // check can produce a precise E_VISIBILITY diagnostic.
                        if fd.visibility == hew_parser::ast::Visibility::Pub {
                            self.module_fn_exports.insert(qualified.clone());
                        }
                        self.fn_type_param_assoc_bindings
                            .insert(qualified.clone(), assoc_bindings);
                        self.fn_sigs.insert(qualified, sig);
                    } else if fd.where_clause.is_some()
                        && self
                            .fn_type_param_assoc_bindings
                            .get(&qualified)
                            .is_none_or(HashMap::is_empty)
                    {
                        // The module-short signature was registered through an
                        // earlier collection pass that keyed only the bare/short
                        // form and did not carry the where-clause associated-type
                        // bindings (`where I: Iterator<Item = A>`). The
                        // projection-pin at the cross-module call site reads these
                        // bindings under the module-short key to resolve a type
                        // param reachable only through the bound; without them the
                        // param stays free, no monomorphisation key is minted, and
                        // a generic terminal like `iter::count` fails closed at MIR.
                        // Backfill from the declaration so producer and call site
                        // agree on the bindings under the same key.
                        let (_sig, assoc_bindings) = self.build_fn_sig_from_decl_with_assoc(fd);
                        if !assoc_bindings.is_empty() {
                            self.fn_type_param_assoc_bindings
                                .insert(qualified.clone(), assoc_bindings);
                        }
                    }
                }
                Item::Actor(ad) => {
                    // Record visibility for all actors (both pub and non-pub) so a
                    // cross-module qualified reference to a non-pub actor produces a
                    // precise E_VISIBILITY at the reference site instead of leaking an
                    // `E_MIR: unknown type` at the MIR boundary. Mirrors the TypeDecl,
                    // Machine, and Trait registration above; actors share the
                    // type-namespace and the same qualified-reference enforcement path.
                    let qualified_type = format!("{module_short}.{}", ad.name);
                    self.type_visibility
                        .entry(qualified_type.clone())
                        .or_insert((ad.visibility, Some(module_short.to_string())));
                    self.type_def_spans
                        .entry(qualified_type)
                        .or_insert_with(|| span.clone());
                    if !self.register_type_namespace_name(Some(module_short), &ad.name, span) {
                        continue;
                    }
                    self.register_actor_base(ad, Some(module_short));
                }
                // Register pub consts from C-backed stdlib modules that also
                // ship Hew source (e.g. `std::misc::log` with `pub const JSON`).
                // `register_user_module` handles this for pure-Hew user modules;
                // this arm mirrors it for the stdlib Hew-source path so that
                // `module.CONST` field access resolves in the type checker via
                // the same `env.lookup_ref("{module}.{field}")` guard in
                // `check_field_access`.
                //
                // SHIM (visibility): non-pub consts are not registered, so a
                // cross-module `module.PRIVATE_CONST` reference fails closed with
                // "module has no exported constant" rather than a dedicated
                // E_VISIBILITY. WHY: const references resolve through the value
                // env / field-access path, which has no visibility-enforcement
                // consult point — unlike traits/actors/types which share the
                // type-reference path. WHEN obsolete: when a const-visibility
                // table + a field-access enforcement consult are added. WHAT the
                // real solution is: record (visibility, decl_module) for every
                // const here and check access_allowed in check_field_access,
                // emitting visibility_violation. Tracked as a follow-on; the
                // current message is already a clean fail-closed diagnostic.
                Item::Const(cd) => {
                    if !cd.visibility.is_pub() {
                        continue;
                    }
                    let ty = self.resolve_registered_annotation_ty_no_holes(&cd.ty);
                    let qualified = format!("{module_short}.{}", cd.name);
                    self.env.define(qualified, ty, false);
                }
                _ => {}
            }
        }
        // Pass 2: Register impl methods (after types exist)
        for (item, span) in items {
            if let Item::Impl(id) = item {
                if Self::impl_decl_is_drop_impl(id) {
                    self.report_unsupported_impl_drop(span);
                    continue;
                }
                if let TypeExpr::Named {
                    name: type_name,
                    type_args,
                } = &id.target_type.0
                {
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
                    // The compiled-in `std/result.hew` / `std/option.hew` impl
                    // blocks are the origin of the builtin `Result`/`Option`
                    // receiver method surface. Snapshot each canonical sig into
                    // a dedicated side table keyed by builtin discriminant +
                    // method name BEFORE any user `type Result`/`type Option`
                    // impl can clobber the colliding bare `Result::<method>` key
                    // in `fn_sigs`. Dispatch on a builtin receiver resolves
                    // against this table only, so a user method on a same-named
                    // user type can never be selected for a builtin wrapper.
                    let builtin_receiver = crate::lookup_builtin_type(type_name)
                        .filter(|b| matches!(b, BuiltinType::Result | BuiltinType::Option));
                    for method in &id.methods {
                        let sig = self.register_impl_method(
                            type_name,
                            method,
                            id.type_params.as_ref(),
                            id.where_clause.as_ref(),
                        );
                        if let Some(builtin) = builtin_receiver {
                            self.builtin_result_option_method_sigs
                                .insert((builtin, method.name.clone()), sig.clone());
                        }
                        // Also register on qualified type name
                        let qualified_type = format!("{module_short}.{type_name}");
                        if let Some(td) = self.lookup_type_def_mut(&qualified_type) {
                            td.methods.insert(method.name.clone(), sig.clone());
                        }
                        if let (Some(canonical), Some(tb)) =
                            (primitive_key.clone(), id.trait_bound.as_ref())
                        {
                            self.record_primitive_trait_impl_self_args(
                                canonical.clone(),
                                tb.name.clone(),
                                self_type_args.clone(),
                                &id.target_type.1,
                            );
                            self.record_primitive_trait_impl_method(
                                canonical,
                                tb.name.clone(),
                                method.name.clone(),
                                sig,
                            );
                        }
                    }
                    if let Some(tb) = &id.trait_bound {
                        self.mark_imported_trait_used_for_module_aliases(module_short, &tb.name);
                        self.record_trait_impl_methods(
                            type_name,
                            &tb.name,
                            id.methods.iter().map(|method| method.name.clone()),
                        );
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
                    // The dotted `{module_short}.{name}` entry is authored
                    // directly by `register_actor_base`; only the export
                    // record is added here.
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
                    let (sig, assoc_bindings) = self.build_fn_sig_from_decl_with_assoc(fd);
                    self.fn_type_param_assoc_bindings
                        .insert(fd.name.clone(), assoc_bindings);
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
                    self.register_machine_decl(md, span);
                    self.known_types.insert(md.name.clone());
                    self.known_types.insert(format!("{}Event", md.name));
                }
                Item::Trait(tr) => {
                    if let Some(supers) = &tr.super_traits {
                        for super_trait in supers {
                            self.mark_imported_trait_used(None, &super_trait.name);
                        }
                    }
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
                    self.register_actor_base(ad, None);
                }
                Item::Impl(id) => {
                    if let TypeExpr::Named {
                        name: type_name,
                        type_args: target_type_args,
                        ..
                    } = &id.target_type.0
                    {
                        if skipped_type_names.contains(type_name) {
                            continue;
                        }
                        // Validate before collect_type_param_bounds erases positional type args.
                        // This path bypasses enter_impl_scope so validation must be explicit.
                        self.validate_type_param_bound_shapes(
                            id.type_params.as_ref(),
                            id.where_clause.as_ref(),
                            span,
                        );
                        // The impl's `Self` type arguments (e.g. `[E]` for
                        // `impl<E> Index for Vec<E>`), resolved so a later
                        // dispatch on a concrete receiver can bind the impl's
                        // type parameters. This path bypasses `enter_impl_scope`
                        // so `E` resolves to a bare `Ty::Named { name: "E" }`,
                        // which is exactly the placeholder the dispatch-time
                        // binding zips against the receiver's concrete args.
                        let self_type_args: Vec<Ty> = target_type_args
                            .as_ref()
                            .map(|type_args| {
                                type_args
                                    .iter()
                                    .map(|type_arg| self.resolve_type_expr(type_arg))
                                    .collect()
                            })
                            .unwrap_or_default();
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
                                self.record_primitive_trait_impl_self_args(
                                    canonical.clone(),
                                    tb.name.clone(),
                                    self_type_args.clone(),
                                    &id.target_type.1,
                                );
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
                            self.mark_imported_trait_used(None, &tb.name);
                            self.record_trait_impl_methods(
                                type_name,
                                &tb.name,
                                id.methods.iter().map(|method| method.name.clone()),
                            );
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
        // Flat file imports register top-level names without a module namespace,
        // sharing the root/flat namespace (`None`).
        self.register_flat_file_import_pub_name(current_import_pub_spans, name, span)
            && self.register_type_namespace_name(None, name, span)
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

    /// Record `module_short`'s own trait import bindings into
    /// `trait_import_bindings`, so a supertrait edge declared in this module that
    /// names a re-imported trait resolves to the original owner (the re-export
    /// chain) rather than a same-named trait in the final importer's scope.
    ///
    /// For each `import other::path::{ Name }` (or `{ Name as B }`) the binding
    /// `Name`/`B` records the source identity `{imported_short}.{Name}`, where
    /// `imported_short` is the import's whole-module alias or its last path
    /// segment — identical to how the module's own qualified keys are formed. A
    /// whole-module `import other::m;` (no brace spec) publishes no bare binding,
    /// so it records nothing. The module's own pub traits self-register
    /// (`(module_short, T) -> {module_short}.T`) so a chain terminates at the
    /// origin. The recorded value is a string; `resolve_trait_ref` only treats it
    /// as a trait when it matches a registered `trait_defs` key, so recording a
    /// (possibly type) import binding here is harmless.
    fn record_trait_import_bindings(&mut self, module_short: &str, items: &[Spanned<Item>]) {
        for (item, _) in items {
            match item {
                Item::Import(decl) => {
                    let imported_short = decl
                        .module_alias
                        .clone()
                        .or_else(|| decl.path.last().cloned());
                    let Some(imported_short) = imported_short else {
                        continue;
                    };
                    match &decl.spec {
                        Some(ImportSpec::Names(names)) => {
                            for import_name in names {
                                let binding = import_name
                                    .alias
                                    .clone()
                                    .unwrap_or_else(|| import_name.name.clone());
                                let source_identity =
                                    format!("{imported_short}.{}", import_name.name);
                                self.trait_import_bindings
                                    .insert((module_short.to_string(), binding), source_identity);
                            }
                        }
                        None => {
                            let prefix = format!("{imported_short}.");
                            let loaded_traits: Vec<String> = self
                                .trait_defs
                                .keys()
                                .filter_map(|key| key.strip_prefix(&prefix))
                                .filter(|name| !name.contains('.'))
                                .map(str::to_string)
                                .collect();
                            for trait_name in loaded_traits {
                                self.trait_import_bindings.insert(
                                    (module_short.to_string(), trait_name.clone()),
                                    format!("{imported_short}.{trait_name}"),
                                );
                            }
                            if let Some(resolved_items) = &decl.resolved_items {
                                for (imported_item, _) in resolved_items {
                                    if let Item::Trait(tr) = imported_item {
                                        if tr.visibility.is_pub() {
                                            self.trait_import_bindings.insert(
                                                (module_short.to_string(), tr.name.clone()),
                                                format!("{imported_short}.{}", tr.name),
                                            );
                                        }
                                    }
                                }
                            }
                        }
                        // Glob imports publish trait names through the normal import
                        // surface, but they do not carry resolved names in the AST.
                        // The loaded-owner fallback below handles their unused-import
                        // marking without manufacturing binding entries here.
                        Some(ImportSpec::Glob) => {}
                    }
                }
                // Self-register the module's own pub traits so a re-export chain
                // terminates here (a sub-trait whose super is this module's own
                // trait resolves directly, without consulting an import).
                Item::Trait(tr) if tr.visibility.is_pub() => {
                    self.trait_import_bindings.insert(
                        (module_short.to_string(), tr.name.clone()),
                        format!("{module_short}.{}", tr.name),
                    );
                }
                _ => {}
            }
        }
    }

    /// Register items from a user module under the module's short-name namespace.
    ///
    /// `module_full_path` is the full dot-separated module path
    /// (e.g. `"subpkg.helper"` for `import subpkg::helper;`).  It is recorded
    /// in `fn_def_spans` as the declaring-module identity so the access-allowed
    /// check can compare full paths (not just the short-name qualifier) when
    /// enforcing package-visibility boundaries.
    #[expect(
        clippy::too_many_lines,
        clippy::ref_option,
        reason = "statement type checking requires many cases"
    )]
    pub(super) fn register_user_module(
        &mut self,
        module_short: &str,
        module_full_path: &str,
        items: &[Spanned<Item>],
        spec: &Option<ImportSpec>,
    ) {
        // Record this module's own trait import bindings BEFORE any of its trait
        // declarations are registered, so a supertrait edge (`trait Sub: Base`)
        // that names a re-imported `Base` resolves through the chain to the
        // original owner (the H11 fix). Topo order guarantees the owner's def is
        // already registered by the time this module's sub-trait edge is built.
        self.record_trait_import_bindings(module_short, items);

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
                    let qualified = format!("{module_short}.{}", fd.name);
                    // Record visibility for all functions in the visibility table.
                    self.fn_visibility
                        .entry(qualified.clone())
                        .or_insert(fd.visibility);
                    // Record fn_def_spans under the SHORT-NAME key so the access-
                    // allowed check in methods.rs can look up the declaring module's
                    // FULL path.  `collect_function_item` stores the same span under
                    // the full-path key (e.g. "subpkg.helper.secret"); we mirror it
                    // under the short-name key ("helper.secret") used at call sites.
                    self.fn_def_spans
                        .entry(qualified.clone())
                        .or_insert_with(|| (span.clone(), Some(module_full_path.to_string())));

                    let (sig, assoc_bindings) = self.build_fn_sig_from_decl_with_assoc(fd);
                    // Only `Pub` functions are module exports: `package fn` must
                    // pass the access-allowed check at every call site, so it must
                    // NOT bypass the check by entering the exports set.  Non-pub
                    // functions are still registered into fn_sigs so the enforcement
                    // check can produce a precise E_VISIBILITY diagnostic.
                    if fd.visibility == hew_parser::ast::Visibility::Pub {
                        self.module_fn_exports.insert(qualified.clone());
                    }
                    self.fn_type_param_assoc_bindings
                        .insert(qualified.clone(), assoc_bindings.clone());
                    self.fn_sigs.insert(qualified, sig.clone());

                    // If named import or glob, also register unqualified (using alias if present).
                    // Only pub functions are eligible for unqualified import bindings — private
                    // functions cannot be imported bare even if they appear in a glob import spec.
                    if fd.visibility.is_pub() && Self::should_import_name(&fd.name, spec) {
                        let binding_name = Self::resolve_import_name(spec, &fd.name)
                            .unwrap_or_else(|| fd.name.clone());
                        self.fn_type_param_assoc_bindings
                            .insert(binding_name.clone(), assoc_bindings);
                        self.fn_sigs.insert(binding_name.clone(), sig);
                        self.unqualified_to_module.insert(
                            (self.current_module.clone(), binding_name),
                            module_short.to_string(),
                        );
                    }
                }
                Item::TypeDecl(td) => {
                    // Record visibility for all TypeDecls so the enforcement check
                    // can distinguish "private, not accessible" from "unknown symbol".
                    // Use module_full_path (not just module_short) so cross-package
                    // checks compare full paths and don't conflate e.g. "helper" (short)
                    // with the root package when the module lives at subpkg::helper.
                    let qualified_type = format!("{module_short}.{}", td.name);
                    self.type_visibility
                        .entry(qualified_type.clone())
                        .or_insert((td.visibility, Some(module_full_path.to_string())));
                    // Record the declaration span so visibility-violation diagnostics
                    // can point "declared here" at the actual declaration even when the
                    // type is not pub (and therefore skips the full namespace registration).
                    self.type_def_spans
                        .entry(qualified_type)
                        .or_insert_with(|| span.clone());
                    if !td.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_type_namespace_name(Some(module_short), &td.name, span) {
                        continue;
                    }
                    // Qualified authority is always published: the source
                    // module's own bare def (read by the alias copy), the
                    // qualified alias, and the module-export record that drives
                    // use-time ambiguity candidate naming.
                    self.register_type_decl(td);
                    self.register_qualified_type_alias(module_short, &td.name);
                    self.record_module_type_export(module_short, &td.name);
                    // The importer-scope bare binding is opt-in: a plain
                    // `import m;` publishes only the qualified name, mirroring
                    // the function/trait arms. Named (`::{ T }`) and glob
                    // imports publish the bare (or aliased) binding.
                    if Self::should_import_name(&td.name, spec) {
                        let binding_name = Self::resolve_import_name(spec, &td.name)
                            .unwrap_or_else(|| td.name.clone());
                        self.known_types.insert(binding_name.clone());
                        let source_identity = format!("{module_short}.{}", td.name);
                        self.record_published_bare_type(&binding_name, &source_identity);
                        // When the importer chose an alias (`T as U`), record
                        // the mapping so HIR `lower_type` can canonicalise the
                        // alias name in type-annotation position.  Key by
                        // (importer_module, alias) so aliases from different
                        // modules cannot overwrite each other.
                        if binding_name != td.name {
                            self.import_type_name_aliases.insert(
                                (self.current_module.clone(), binding_name.clone()),
                                source_identity.clone(),
                            );
                        }
                        self.unqualified_to_module.insert(
                            (self.current_module.clone(), binding_name),
                            module_short.to_string(),
                        );
                    }
                }
                Item::Machine(md) => {
                    // Record visibility for all Machines so the enforcement check
                    // can distinguish "private, not accessible" from "unknown symbol".
                    // Use module_full_path for the same reason as TypeDecl above.
                    let qualified_machine = format!("{module_short}.{}", md.name);
                    self.type_visibility
                        .entry(qualified_machine.clone())
                        .or_insert((md.visibility, Some(module_full_path.to_string())));
                    // Record the declaration span for non-pub machine types so the
                    // "declared here" note in E_VISIBILITY points at the actual decl.
                    self.type_def_spans
                        .entry(qualified_machine)
                        .or_insert_with(|| span.clone());
                    if !md.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_machine_type_namespace_names(
                        Some(module_short),
                        &md.name,
                        span,
                    ) {
                        continue;
                    }
                    let event_name = format!("{}Event", md.name);
                    // Qualified authority is always published for the machine
                    // and its companion event enum.
                    self.register_machine_decl(md, span);
                    self.register_qualified_type_alias(module_short, &md.name);
                    self.register_qualified_type_alias(module_short, &event_name);
                    self.record_module_type_export(module_short, &md.name);
                    self.record_module_type_export(module_short, &event_name);
                    // Bare publication of the machine and its event enum is
                    // opt-in, gated together so a named/glob import exposes both
                    // or neither.
                    if Self::should_import_name(&md.name, spec) {
                        let machine_binding = Self::resolve_import_name(spec, &md.name)
                            .unwrap_or_else(|| md.name.clone());
                        let event_binding = Self::resolve_import_name(spec, &event_name)
                            .unwrap_or_else(|| event_name.clone());
                        self.known_types.insert(machine_binding.clone());
                        self.known_types.insert(event_binding.clone());
                        let machine_identity = format!("{module_short}.{}", md.name);
                        let event_identity = format!("{module_short}.{event_name}");
                        self.record_published_bare_type(&machine_binding, &machine_identity);
                        self.record_published_bare_type(&event_binding, &event_identity);
                        self.unqualified_to_module.insert(
                            (self.current_module.clone(), machine_binding),
                            module_short.to_string(),
                        );
                        self.unqualified_to_module.insert(
                            (self.current_module.clone(), event_binding),
                            module_short.to_string(),
                        );
                    }
                }
                Item::Trait(tr) => {
                    if let Some(supers) = &tr.super_traits {
                        for super_trait in supers {
                            self.mark_imported_trait_used(Some(module_short), &super_trait.name);
                        }
                    }
                    // Record visibility for all traits so the enforcement check can
                    // distinguish "private, not accessible" from "unknown symbol".
                    // Use module_full_path (matching TypeDecl/Machine) so cross-package
                    // checks compare full paths. Without this a cross-module qualified
                    // reference to a non-pub trait leaks an `E_MIR: unknown type` at the
                    // MIR boundary instead of a precise E_VISIBILITY at the reference site.
                    let qualified_trait = format!("{module_short}.{}", tr.name);
                    self.type_visibility
                        .entry(qualified_trait.clone())
                        .or_insert((tr.visibility, Some(module_full_path.to_string())));
                    self.type_def_spans
                        .entry(qualified_trait)
                        .or_insert_with(|| span.clone());
                    if !tr.visibility.is_pub() {
                        continue;
                    }
                    let info = Self::trait_info_from_decl(tr);
                    let import_binding = if Self::should_import_name(&tr.name, spec) {
                        let binding_name = Self::resolve_import_name(spec, &tr.name)
                            .unwrap_or_else(|| tr.name.clone());
                        // The unqualified trait binding lands in the *importing*
                        // module's namespace, not the source module's.
                        let importer = self.current_module.clone();
                        if self.register_type_namespace_name(
                            importer.as_deref(),
                            &binding_name,
                            span,
                        ) {
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

                    // Record super-trait relationships for both qualified and
                    // unqualified bindings. A supertrait reference written inside
                    // the source module (`trait Sub: Base`) is bare in the source
                    // spelling, but it names the trait `module_short` resolves
                    // `Base` to: its own same-package `Base` (`{module_short}.Base`),
                    // OR a re-imported `Base` followed through this module's import
                    // bindings to the original owner. Store the OWNER-QUALIFIED
                    // identity so `trait_super` values are collision-free
                    // `trait_defs` keys. Resolving the bare source spelling in the
                    // IMPORTER's namespace instead is both collision-unsafe (binds
                    // whatever `Base` the importer has) and over-strict (an
                    // import-only-`Sub` would fail to find its supertrait's method
                    // set, falsely rejecting an inline supermethod as extra).
                    if let Some(supers) = &tr.super_traits {
                        let super_keys: Vec<String> = supers
                            .iter()
                            .map(|s| {
                                self.mark_imported_trait_used(Some(module_short), &s.name);
                                self.resolve_super_trait_edge(module_short, &s.name)
                            })
                            .collect();
                        self.trait_super
                            .insert(qualified.clone(), super_keys.clone());
                        if let Some(binding_name) = import_binding.as_ref() {
                            self.trait_super.insert(binding_name.clone(), super_keys);
                        }
                    }

                    // If glob or named import, also register unqualified (using alias if present)
                    if let Some(binding_name) = import_binding {
                        self.trait_defs.insert(binding_name.clone(), info.clone());
                        // Record the SOURCE identity (`module_short.tr.name`) under
                        // the binding so trait-conformance can recover the owner +
                        // original trait name for an aliased import. `qualified` is
                        // the source identity even when `binding_name` is an alias.
                        self.published_bare_trait_owners
                            .entry((self.current_module.clone(), binding_name.clone()))
                            .or_default()
                            .insert(qualified.clone());
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
                                self.record_primitive_trait_impl_self_args(
                                    canonical.clone(),
                                    tb.name.clone(),
                                    self_type_args.clone(),
                                    &id.target_type.1,
                                );
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
                    // Record visibility for all actors so a cross-module qualified
                    // reference to a non-pub actor produces a precise E_VISIBILITY at
                    // the reference site instead of leaking an `E_MIR: unknown type` at
                    // the MIR boundary. Use module_full_path (matching TypeDecl/Machine/
                    // Trait) so cross-package checks compare full paths.
                    let qualified_actor = format!("{module_short}.{}", ad.name);
                    self.type_visibility
                        .entry(qualified_actor.clone())
                        .or_insert((ad.visibility, Some(module_full_path.to_string())));
                    self.type_def_spans
                        .entry(qualified_actor)
                        .or_insert_with(|| span.clone());
                    // Skip non-pub actors (enforce visibility), matching every
                    // other item kind in this loop. A private actor must never
                    // become a module type export, qualified alias, or registered
                    // base in the importer's view: otherwise `spawn module.Account()`
                    // would accept a private target and -- after the qualifier is
                    // stripped to the bare name in HIR -- silently route to a
                    // same-named root/pub actor. This `Item::Actor` arm was the
                    // lone exporter that ignored `pub`, recording private actors in
                    // `module_type_exports` (the authoritative export registry).
                    if !ad.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_type_namespace_name(Some(module_short), &ad.name, span) {
                        continue;
                    }
                    // `register_actor_base` authors the dotted
                    // `{module_short}.{name}` identity directly; no bare key
                    // and no copy-based qualified alias.
                    self.register_actor_base(ad, Some(module_short));
                    self.record_module_type_export(module_short, &ad.name);
                    // If named import or glob, also register unqualified
                    if Self::should_import_name(&ad.name, spec) {
                        let binding_name = Self::resolve_import_name(spec, &ad.name)
                            .unwrap_or_else(|| ad.name.clone());
                        let source_identity = format!("{module_short}.{}", ad.name);
                        self.record_published_bare_type(&binding_name, &source_identity);
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
    pub(super) fn build_fn_sig_from_decl_with_assoc(
        &mut self,
        fd: &FnDecl,
    ) -> (FnSig, HashMap<(String, String, String), Ty>) {
        let mut hole_vars = Vec::new();
        let scope = self.collect_type_param_scope_with_assoc_bindings(
            fd.type_params.as_ref(),
            fd.where_clause.as_ref(),
            &mut hole_vars,
        );
        let pushed_bounds = !scope.bounds.is_empty();
        if pushed_bounds {
            self.current_type_param_bounds.push(scope.clone());
        }
        let param_names = fd.params.iter().map(|p| p.name.clone()).collect();
        let params = fd
            .params
            .iter()
            .map(|p| self.resolve_registered_annotation_ty(&p.ty, &mut hole_vars))
            .collect();
        let declared_return = fd.return_type.as_ref().map_or(Ty::Unit, |ret| {
            self.resolve_registered_annotation_ty(ret, &mut hole_vars)
        });
        if pushed_bounds {
            self.current_type_param_bounds.pop();
        }
        let type_params = fd.type_params.as_ref().map_or(vec![], |params| {
            params.iter().map(|p| p.name.clone()).collect()
        });
        let return_type = if fd.is_generator && fd.is_async {
            Ty::async_generator(declared_return)
        } else if fd.is_generator {
            Ty::generator(declared_return, Ty::Unit)
        } else {
            declared_return
        };
        let assoc_bindings = scope.assoc_bindings;
        let sig = FnSig {
            type_params,
            type_param_bounds: self
                .collect_type_param_bounds(fd.type_params.as_ref(), fd.where_clause.as_ref()),
            param_names,
            params,
            return_type,
            is_async: fd.is_async,
            doc_comment: fd.doc_comment.clone(),
            ..FnSig::default()
        };
        (sig, assoc_bindings)
    }

    /// Compute the checker identity for an actor declared in `module_short`.
    ///
    /// Module actors are identified by the dotted `{module_short}.{name}` key
    /// (the same authoritative form `resolve_module_type` reads); root and
    /// flat-file actors keep the bare name. This is the single authority for
    /// the actor-identity key shape — every registration and lookup site
    /// derives the key through here so producer and consumer cannot drift.
    pub(super) fn actor_identity(module_short: Option<&str>, name: &str) -> String {
        match module_short {
            Some(m) => format!("{m}.{name}"),
            None => name.to_string(),
        }
    }

    /// Register an actor's core items: the type declaration, receive functions,
    /// and inline methods.  This block is identical across all three import
    /// registration paths; only the export-record and unqualified-binding
    /// steps differ and are therefore kept in each caller.
    ///
    /// Actor identity is the dotted `{module_short}.{name}` key for module
    /// actors (authored directly here — NOT copied from a bare entry, which
    /// is last-write-wins across modules) and the bare name for root and
    /// flat-file actors. The bare key is never written for module actors, so
    /// a second same-named import cannot clobber another module's actor.
    pub(super) fn register_actor_base(&mut self, ad: &ActorDecl, module_short: Option<&str>) {
        let identity = Self::actor_identity(module_short, &ad.name);
        self.register_actor_decl_as(ad, &identity);
        self.known_types.insert(identity.clone());
        for rf in &ad.receive_fns {
            self.register_receive_fn(&identity, rf);
        }
        for method in &ad.methods {
            let method_name = format!("{identity}::{}", method.name);
            self.register_fn_sig_with_name(&method_name, method);
        }
    }

    /// Seed the module-qualified marker-derivation alias for a type declared
    /// in a non-root module, immediately after its bare registration.
    ///
    /// The trait registry keys marker derivation by name; the bare key is
    /// last-write-wins across modules. Two imported packages that each export a
    /// type named `Reply` collide on the single bare `"Reply"` key, so a Send
    /// lookup at the ask-reply gate can read the wrong module's fields. The
    /// importer qualifies the dispatched actor's reply type as
    /// `{module_short}.{name}` (matching `actor_identity`), so the qualified
    /// registry alias gives the gate a collision-free identity to look up.
    ///
    /// Unlike `register_qualified_type_alias` (pub-only, import-surface), this
    /// runs for EVERY type a non-root module declares — including the non-pub
    /// records reachable only as an actor's `receive fn` reply type (the
    /// `testffi` fixture's `type Result` is one such non-pub reply). Bare
    /// lookups are unchanged; root / flat-file types (no `current_module`) are a
    /// no-op.
    fn seed_qualified_type_markers_for_current_module(&mut self, name: &str) {
        if let Some(module_short) = self.current_module_short() {
            let qualified = format!("{module_short}.{name}");
            self.registry.alias_type_markers(name, &qualified);
        }
    }

    /// Insert a qualified alias (`module_short.Name`) for a type that has
    /// already been registered under its bare name.
    ///
    /// Actors do NOT use this copy-based alias: their dotted key is authored
    /// directly by [`Self::register_actor_base`], so the qualified entry is
    /// always the module's own actor rather than whichever bare entry won.
    pub(super) fn register_qualified_type_alias(&mut self, module_short: &str, name: &str) {
        let qualified = format!("{module_short}.{name}");
        if let Some(def) = self.type_defs.get(name).cloned() {
            self.type_defs.insert(qualified.clone(), def);
            if let Some(span) = self.type_def_spans.get(name).cloned() {
                self.type_def_spans.insert(qualified.clone(), span);
            }
            // Mirror the marker-derivation tables under the qualified key. The
            // bare `type_fields` (and sibling member maps) are last-write-wins
            // across modules — two packages each exporting `Reply` collide on
            // the single bare key, so a same-bare-name reply derives `Send`
            // from whichever module won the race. The qualified alias gives the
            // Send gate a collision-free identity to look up.
            self.registry.alias_type_markers(name, &qualified);
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

    /// Record that the bare binding `bare_binding` was published into the current
    /// importer's scope, denoting `source_identity` (the owner-qualified SOURCE
    /// type name `owner.OriginalName`). Populated at every site that inserts a
    /// bare type binding into `unqualified_to_module`, this set is the authority
    /// the use-time ambiguity check reads: a bare reference is ambiguous only
    /// when more than one source identity is published under it, so a plain
    /// `import` that exported but did not publish it cannot poison an explicit
    /// named import of the same bare name.
    ///
    /// The value is the SOURCE identity, not merely the owner module, so an
    /// aliased import (`import m::{ T as U }`) records `U -> m.T` — the binding
    /// `U` resolves to the type `m` actually exports under `T`, never the wrong
    /// `m.U`. `published_bare_type_qualified` reads this identity back verbatim.
    pub(super) fn record_published_bare_type(&mut self, bare_binding: &str, source_identity: &str) {
        self.published_bare_type_owners
            .entry((self.current_module.clone(), bare_binding.to_string()))
            .or_default()
            .insert(source_identity.to_string());
    }
}

#[cfg(test)]
mod failure_surface_lockstep_tests {
    use super::FAILURE_HEW;

    /// R4 lockstep (M-5/M-7): the embedded `FAILURE_HEW` must declare the same
    /// type surface as the on-disk `std/failure.hew`, or module-graph and
    /// inline-test checking diverge. Pins the four type names the on-disk file
    /// declares — `CrashInfo`, `CrashAction`, `CrashNotification`, `CrashKind` —
    /// so adding/removing one from either copy without the other fails here.
    #[test]
    fn embedded_failure_surface_declares_full_type_set() {
        let parsed = hew_parser::parse(FAILURE_HEW);
        assert!(
            parsed.errors.is_empty(),
            "embedded FAILURE_HEW must parse cleanly: {:?}",
            parsed.errors
        );
        let declared: std::collections::HashSet<String> = parsed
            .program
            .items
            .iter()
            .filter_map(|(item, _)| match item {
                hew_parser::ast::Item::TypeDecl(decl) => Some(decl.name.clone()),
                _ => None,
            })
            .collect();
        for expected in ["CrashInfo", "CrashAction", "CrashNotification", "CrashKind"] {
            assert!(
                declared.contains(expected),
                "embedded FAILURE_HEW is missing `{expected}` (drifted from std/failure.hew); \
                 declared: {declared:?}"
            );
        }
    }

    /// M-5: `CrashInfo` carries `code: i64` AND `message: string` in the
    /// embedded copy (the field-presence half of `wire-contract-test-presence`).
    #[test]
    fn embedded_crash_info_has_code_and_message_fields() {
        let parsed = hew_parser::parse(FAILURE_HEW);
        let crash_info = parsed
            .program
            .items
            .iter()
            .find_map(|(item, _)| match item {
                hew_parser::ast::Item::TypeDecl(decl) if decl.name == "CrashInfo" => Some(decl),
                _ => None,
            })
            .expect("embedded FAILURE_HEW must declare CrashInfo");
        let field_names: Vec<&str> = crash_info
            .body
            .iter()
            .filter_map(|item| match item {
                hew_parser::ast::TypeBodyItem::Field { name, .. } => Some(name.as_str()),
                _ => None,
            })
            .collect();
        assert_eq!(
            field_names,
            vec!["code", "message"],
            "CrashInfo must carry exactly `code` then `message`"
        );
    }
}
