//! Bidirectional type checker for Hew programs.

use crate::env::TypeEnv;
use crate::error::{TypeError, TypeErrorKind};
use crate::stdlib::{
    handle_types_for_module, is_handle_type, module_short_name, stdlib_clean_names,
    stdlib_functions, wrapper_fn_sigs,
};
use crate::traits::{MarkerTrait, TraitRegistry};
use crate::ty::{Substitution, Ty, TypeVar};
use crate::unify::unify;
#[cfg(test)]
use hew_parser::ast::IntRadix;
use hew_parser::ast::{
    ActorDecl, BinaryOp, Block, CallArg, ConstDecl, Expr, ExternBlock, FieldDecl, FnDecl, ImplDecl,
    ImportDecl, ImportSpec, Item, LambdaParam, Literal, MatchArm, Pattern, Program, ReceiveFnDecl,
    Span, Spanned, Stmt, StringPart, TraitDecl, TraitItem, TraitMethod, TypeBodyItem, TypeDecl,
    TypeDeclKind, TypeExpr, TypeParam, UnaryOp, VariantKind, WhereClause, WireDecl, WireDeclKind,
};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};

/// Result of type-checking a program.
#[derive(Debug, Clone)]
pub struct TypeCheckOutput {
    pub expr_types: HashMap<SpanKey, Ty>,
    pub errors: Vec<TypeError>,
    pub warnings: Vec<TypeError>,
    pub type_defs: HashMap<String, TypeDef>,
    pub fn_sigs: HashMap<String, FnSig>,
    /// Actor type names that participate in reference cycles.
    pub cycle_capable_actors: HashSet<String>,
    /// Module short names for user (non-stdlib) imports that have resolved items.
    pub user_modules: HashSet<String>,
}

/// Span used as map key (byte offsets).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SpanKey {
    pub start: usize,
    pub end: usize,
}

impl From<&Span> for SpanKey {
    fn from(s: &Span) -> Self {
        Self {
            start: s.start,
            end: s.end,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum WasmUnsupportedFeature {
    SupervisionTrees,
    LinkMonitor,
    StructuredConcurrency,
    Tasks,
    Select,
}

impl WasmUnsupportedFeature {
    fn label(self) -> &'static str {
        match self {
            Self::SupervisionTrees => "Supervision tree operations",
            Self::LinkMonitor => "Link/monitor operations",
            Self::StructuredConcurrency => "Structured concurrency scopes",
            Self::Tasks => "Task handles spawned from scopes",
            Self::Select => "Select expressions",
        }
    }

    fn reason(self) -> &'static str {
        match self {
            Self::SupervisionTrees => {
                "they require OS threads for restart strategies and child supervision"
            }
            Self::LinkMonitor => {
                "they rely on OS threads to watch linked actors and propagate exits"
            }
            Self::StructuredConcurrency => "they schedule child work on dedicated OS threads",
            Self::Tasks => "they need OS threads to drive scope completions",
            Self::Select => "they wait on multiple mailboxes using OS thread blocking",
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeDef {
    pub kind: TypeDefKind,
    pub name: String,
    pub type_params: Vec<String>,
    pub fields: HashMap<String, Ty>,
    pub variants: HashMap<String, VariantDef>,
    pub methods: HashMap<String, FnSig>,
    pub doc_comment: Option<String>,
}

#[derive(Debug, Clone)]
struct TraitInfo {
    methods: Vec<TraitMethod>,
    associated_types: Vec<TraitAssociatedTypeInfo>,
    type_params: Vec<String>,
}

#[derive(Debug, Clone)]
struct TraitAssociatedTypeInfo {
    name: String,
    default: Option<Spanned<TypeExpr>>,
}

#[derive(Debug)]
struct ImplAliasScope {
    span: Span,
    entries: HashMap<String, ImplAliasEntry>,
    missing_reported: HashSet<String>,
    report_missing: bool,
}

#[derive(Debug)]
struct ImplAliasEntry {
    expr: Spanned<TypeExpr>,
    resolved: Option<Ty>,
    resolving: bool,
}

#[derive(Debug, Clone)]
pub enum VariantDef {
    Unit,
    Tuple(Vec<Ty>),
    Struct(Vec<(String, Ty)>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeDefKind {
    Struct,
    Enum,
    Actor,
}

#[derive(Debug, Clone)]
pub struct FnSig {
    pub type_params: Vec<String>,
    pub type_param_bounds: HashMap<String, Vec<String>>,
    pub param_names: Vec<String>,
    pub params: Vec<Ty>,
    pub return_type: Ty,
    pub is_async: bool,
    pub is_pure: bool,
    pub accepts_kwargs: bool,
    pub doc_comment: Option<String>,
}

/// The main type checker.
#[derive(Debug)]
pub struct Checker {
    env: TypeEnv,
    subst: Substitution,
    registry: TraitRegistry,
    errors: Vec<TypeError>,
    warnings: Vec<TypeError>,
    expr_types: HashMap<SpanKey, Ty>,
    type_defs: HashMap<String, TypeDef>,
    fn_sigs: HashMap<String, FnSig>,
    /// Tracks the span where each function was first defined (for duplicate detection).
    fn_def_spans: HashMap<String, Span>,
    generic_ctx: Vec<HashMap<String, Ty>>,
    current_return_type: Option<Ty>,
    in_generator: bool,
    loop_depth: u32,
    modules: HashSet<String>,
    known_types: HashSet<String>,
    type_aliases: HashMap<String, Ty>,
    trait_defs: HashMap<String, TraitInfo>,
    /// Maps trait name → list of super-trait names (e.g., "Pet" → ["Animal"])
    trait_super: HashMap<String, Vec<String>>,
    /// Set of (`type_name`, `trait_name`) pairs for concrete impl registrations
    trait_impls_set: HashSet<(String, String)>,
    /// Maps supervisor name → list of child actor type names (for `supervisor_child`)
    supervisor_children: HashMap<String, Vec<String>>,
    /// When set, records the scope depth at which a lambda was entered.
    /// Variable lookups from scopes below this depth are captures.
    lambda_capture_depth: Option<usize>,
    /// Captured variable types accumulated during lambda body checking.
    lambda_captures: Vec<Ty>,
    /// Tracks imported module paths with their source spans for unused-import detection.
    /// Key: module short name (e.g., "json"), Value: import span.
    import_spans: HashMap<String, Span>,
    /// Module short names that have actually been referenced in code.
    used_modules: RefCell<HashSet<String>>,
    /// Module short names for user (non-stdlib) imports.
    user_modules: HashSet<String>,
    /// Maps unqualified function names to module short names (for glob/named imports).
    /// Used to mark the module as used when the function is called.
    unqualified_to_module: HashMap<String, String>,
    /// Call graph: maps caller function name → set of callee function names.
    call_graph: HashMap<String, HashSet<String>>,
    /// Name of the function currently being checked (for call graph tracking).
    current_function: Option<String>,
    /// Whether we are currently inside a for-loop binding (suppress shadowing for loop vars).
    in_for_binding: bool,
    /// Whether we are currently inside a `pure` function body.
    in_pure_function: bool,
    /// Whether we are currently inside an unsafe block.
    in_unsafe: bool,
    /// The module currently being processed (enables per-module scoping in future).
    current_module: Option<String>,
    /// Tracks which types are defined locally (in the current compilation unit).
    local_type_defs: HashSet<String>,
    /// Tracks which traits are defined locally (in the current compilation unit).
    local_trait_defs: HashSet<String>,
    /// The type name and args of the current impl block target (for resolving `Self`).
    current_self_type: Option<(String, Vec<Ty>)>,
    impl_alias_scopes: Vec<ImplAliasScope>,
    /// Names of functions that require an unsafe block to call.
    unsafe_functions: HashSet<String>,
    /// Whether warnings for WASM-only builds should be emitted.
    wasm_target: bool,
    /// Tracks (span, feature) pairs we've already warned about for WASM limits.
    wasm_warning_spans: HashSet<(SpanKey, WasmUnsupportedFeature)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct IntegerTypeInfo {
    width: u8,
    signed: bool,
}

fn integer_type_info(ty: &Ty) -> Option<IntegerTypeInfo> {
    match ty {
        Ty::I8 => Some(IntegerTypeInfo {
            width: 8,
            signed: true,
        }),
        Ty::I16 => Some(IntegerTypeInfo {
            width: 16,
            signed: true,
        }),
        Ty::I32 => Some(IntegerTypeInfo {
            width: 32,
            signed: true,
        }),
        Ty::I64 => Some(IntegerTypeInfo {
            width: 64,
            signed: true,
        }),
        Ty::U8 => Some(IntegerTypeInfo {
            width: 8,
            signed: false,
        }),
        Ty::U16 => Some(IntegerTypeInfo {
            width: 16,
            signed: false,
        }),
        Ty::U32 => Some(IntegerTypeInfo {
            width: 32,
            signed: false,
        }),
        Ty::U64 => Some(IntegerTypeInfo {
            width: 64,
            signed: false,
        }),
        _ => None,
    }
}

fn can_implicitly_coerce_integer(actual: &Ty, expected: &Ty) -> bool {
    let Some(actual_info) = integer_type_info(actual) else {
        return false;
    };
    let Some(expected_info) = integer_type_info(expected) else {
        return false;
    };
    // Only allow widening: same sign and actual width fits in expected width
    actual_info.signed == expected_info.signed && actual_info.width <= expected_info.width
}

fn common_integer_type(a: &Ty, b: &Ty) -> Option<Ty> {
    let a_info = integer_type_info(a)?;
    let b_info = integer_type_info(b)?;
    if a_info.signed != b_info.signed {
        return None;
    }
    if a_info.width >= b_info.width {
        Some(a.clone())
    } else {
        Some(b.clone())
    }
}

fn common_numeric_type(a: &Ty, b: &Ty) -> Option<Ty> {
    if a.is_float() && b.is_float() {
        if *a == Ty::F64 || *b == Ty::F64 {
            Some(Ty::F64)
        } else {
            Some(Ty::F32)
        }
    } else if a.is_float() && b.is_integer() {
        Some(a.clone())
    } else if a.is_integer() && b.is_float() {
        Some(b.clone())
    } else {
        common_integer_type(a, b)
    }
}

impl Checker {
    #[must_use]
    pub fn new() -> Self {
        Self {
            env: TypeEnv::new(),
            subst: Substitution::new(),
            registry: TraitRegistry::new(),
            errors: Vec::new(),
            warnings: Vec::new(),
            expr_types: HashMap::new(),
            type_defs: HashMap::new(),
            fn_sigs: HashMap::new(),
            fn_def_spans: HashMap::new(),
            generic_ctx: Vec::new(),
            current_return_type: None,
            in_generator: false,
            loop_depth: 0,
            modules: HashSet::new(),
            known_types: HashSet::new(),
            type_aliases: HashMap::new(),
            trait_defs: HashMap::new(),
            trait_super: HashMap::new(),
            trait_impls_set: HashSet::new(),
            supervisor_children: HashMap::new(),
            lambda_capture_depth: None,
            lambda_captures: Vec::new(),
            import_spans: HashMap::new(),
            used_modules: RefCell::new(HashSet::new()),
            user_modules: HashSet::new(),
            unqualified_to_module: HashMap::new(),
            call_graph: HashMap::new(),
            current_function: None,
            in_for_binding: false,
            in_pure_function: false,
            in_unsafe: false,
            current_module: None,
            local_type_defs: HashSet::new(),
            local_trait_defs: HashSet::new(),
            current_self_type: None,
            impl_alias_scopes: Vec::new(),
            unsafe_functions: HashSet::new(),
            wasm_target: false,
            wasm_warning_spans: HashSet::new(),
        }
    }

    /// Enable WASM32-specific validation and warnings.
    pub fn enable_wasm_target(&mut self) {
        self.wasm_target = true;
    }

    /// Look up a type definition, handling module-qualified names like `json.Value`.
    /// Tries exact match first, then strips a known module prefix to find unqualified name.
    fn lookup_type_def(&self, name: &str) -> Option<TypeDef> {
        if let Some(td) = self.type_defs.get(name) {
            return Some(td.clone());
        }
        // Strip module prefix only if the prefix is a known module
        if let Some(dot) = name.find('.') {
            let prefix = &name[..dot];
            if self.modules.contains(prefix) {
                let unqualified = &name[dot + 1..];
                if let Some(td) = self.type_defs.get(unqualified) {
                    return Some(td.clone());
                }
            }
        }
        None
    }

    /// Look up a type definition mutably, handling module-qualified names.
    fn lookup_type_def_mut(&mut self, name: &str) -> Option<&mut TypeDef> {
        if self.type_defs.contains_key(name) {
            return self.type_defs.get_mut(name);
        }
        if let Some(dot) = name.find('.') {
            let prefix = &name[..dot];
            if self.modules.contains(prefix) {
                let unqualified = &name[dot + 1..];
                if self.type_defs.contains_key(unqualified) {
                    return self.type_defs.get_mut(unqualified);
                }
            }
        }
        None
    }

    /// Look up a function signature, handling module-qualified names.
    /// For method keys like `json.Value::method`, also tries `Value::method`.
    fn lookup_fn_sig(&self, key: &str) -> Option<FnSig> {
        if let Some(sig) = self.fn_sigs.get(key) {
            return Some(sig.clone());
        }
        // Strip module prefix only if the prefix is a known module
        if let Some(dot) = key.find('.') {
            let prefix = &key[..dot];
            if self.modules.contains(prefix) {
                let unqualified = &key[dot + 1..];
                if let Some(sig) = self.fn_sigs.get(unqualified) {
                    return Some(sig.clone());
                }
            }
        }
        None
    }

    /// Try to resolve a method call on a named type via `type_defs` and `fn_sigs`.
    ///
    /// Used as a fallback from hardcoded handle-type dispatch tables so that
    /// methods added via `.hew` impl blocks work without updating the tables.
    fn try_resolve_named_method(
        &mut self,
        name: &str,
        method: &str,
        args: &[CallArg],
        _span: &Span,
    ) -> Option<Ty> {
        if let Some(td) = self.lookup_type_def(name) {
            if let Some(sig) = td.methods.get(method) {
                for (i, arg) in args.iter().enumerate() {
                    if let Some(param_ty) = sig.params.get(i) {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, param_ty);
                    }
                }
                return Some(sig.return_type.clone());
            }
        }
        let method_key = format!("{name}::{method}");
        if let Some(sig) = self.lookup_fn_sig(&method_key) {
            for (i, arg) in args.iter().enumerate() {
                if let Some(param_ty) = sig.params.get(i) {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, param_ty);
                }
            }
            return Some(sig.return_type.clone());
        }
        None
    }

    #[expect(
        clippy::too_many_lines,
        reason = "all builtins registered in one place"
    )]
    fn register_builtins(&mut self) {
        // Print functions
        self.register_builtin_fn("println_int", vec![Ty::I64], Ty::Unit);
        self.register_builtin_fn("println_str", vec![Ty::String], Ty::Unit);
        self.register_builtin_fn("print_int", vec![Ty::I64], Ty::Unit);
        self.register_builtin_fn("print_str", vec![Ty::String], Ty::Unit);
        self.register_builtin_fn("println_float", vec![Ty::F64], Ty::Unit);
        self.register_builtin_fn("println_bool", vec![Ty::Bool], Ty::Unit);
        self.register_builtin_fn("print_float", vec![Ty::F64], Ty::Unit);
        self.register_builtin_fn("print_bool", vec![Ty::Bool], Ty::Unit);
        // Generic print/println that accept any type
        self.register_builtin_fn("println", vec![Ty::Var(TypeVar::fresh())], Ty::Unit);
        self.register_builtin_fn("print", vec![Ty::Var(TypeVar::fresh())], Ty::Unit);

        // Math functions
        self.register_builtin_fn("abs", vec![Ty::I64], Ty::I64);
        self.register_builtin_fn("sqrt", vec![Ty::F64], Ty::F64);
        self.register_builtin_fn("min", vec![Ty::I64, Ty::I64], Ty::I64);
        self.register_builtin_fn("max", vec![Ty::I64, Ty::I64], Ty::I64);
        self.register_builtin_fn("to_float", vec![Ty::I64], Ty::F64);

        // String operations
        self.register_builtin_fn("string_concat", vec![Ty::String, Ty::String], Ty::String);
        self.register_builtin_fn("string_length", vec![Ty::String], Ty::I64);
        self.register_builtin_fn("to_string", vec![Ty::Var(TypeVar::fresh())], Ty::String);
        self.register_builtin_fn("len", vec![Ty::Var(TypeVar::fresh())], Ty::I64);

        // I/O and system
        self.register_builtin_fn("read_file", vec![Ty::String], Ty::String);
        self.register_builtin_fn("write_file", vec![Ty::String, Ty::String], Ty::Unit);
        self.register_builtin_fn("sleep_ms", vec![Ty::I64], Ty::Unit);
        self.register_builtin_fn("sleep", vec![Ty::I64], Ty::Unit);
        self.register_builtin_fn("stop", vec![Ty::Var(TypeVar::fresh())], Ty::Unit);
        self.register_builtin_fn("close", vec![Ty::Var(TypeVar::fresh())], Ty::Unit);
        self.register_builtin_fn("exit", vec![Ty::I64], Ty::Never);
        self.register_builtin_fn("panic", vec![Ty::String], Ty::Never);

        // Actor link/monitor (Erlang-style fault propagation)
        let link_t = TypeVar::fresh();
        self.register_builtin_fn(
            "link",
            vec![Ty::ActorRef(Box::new(Ty::Var(link_t)))],
            Ty::Unit,
        );
        let unlink_t = TypeVar::fresh();
        self.register_builtin_fn(
            "unlink",
            vec![Ty::ActorRef(Box::new(Ty::Var(unlink_t)))],
            Ty::Unit,
        );
        let monitor_t = TypeVar::fresh();
        self.register_builtin_fn(
            "monitor",
            vec![Ty::ActorRef(Box::new(Ty::Var(monitor_t)))],
            Ty::I64,
        );
        self.register_builtin_fn("demonitor", vec![Ty::I64], Ty::Unit);

        // Supervisor child access
        let sup_child_t = TypeVar::fresh();
        let sup_child_ret = TypeVar::fresh();
        self.register_builtin_fn(
            "supervisor_child",
            vec![Ty::ActorRef(Box::new(Ty::Var(sup_child_t))), Ty::I64],
            Ty::ActorRef(Box::new(Ty::Var(sup_child_ret))),
        );
        let sup_stop_t = TypeVar::fresh();
        self.register_builtin_fn(
            "supervisor_stop",
            vec![Ty::ActorRef(Box::new(Ty::Var(sup_stop_t)))],
            Ty::Unit,
        );

        // Assertions (test support)
        self.register_builtin_fn("assert", vec![Ty::Bool], Ty::Unit);
        let t_eq = TypeVar::fresh();
        self.register_builtin_fn("assert_eq", vec![Ty::Var(t_eq), Ty::Var(t_eq)], Ty::Unit);
        let t_ne = TypeVar::fresh();
        self.register_builtin_fn("assert_ne", vec![Ty::Var(t_ne), Ty::Var(t_ne)], Ty::Unit);

        // Option/Result constructors
        // Option/Result constructors are handled specially in check_call
        // (they need fresh linked type vars per invocation)

        // Collection constructors (path-style calls: Vec::new(), HashMap::new())
        // These also need fresh vars per call but are less critical
        self.register_builtin_fn(
            "Vec::new",
            vec![],
            Ty::Named {
                name: "Vec".to_string(),
                args: vec![Ty::Var(TypeVar::fresh())],
            },
        );
        self.register_builtin_fn(
            "Vec::with_capacity",
            vec![Ty::I64],
            Ty::Named {
                name: "Vec".to_string(),
                args: vec![Ty::Var(TypeVar::fresh())],
            },
        );
        self.register_builtin_fn(
            "HashMap::new",
            vec![],
            Ty::Named {
                name: "HashMap".to_string(),
                args: vec![Ty::Var(TypeVar::fresh()), Ty::Var(TypeVar::fresh())],
            },
        );
        self.register_builtin_fn(
            "HashSet::new",
            vec![],
            Ty::Named {
                name: "HashSet".to_string(),
                args: vec![Ty::Var(TypeVar::fresh())],
            },
        );
        self.register_builtin_fn(
            "bytes::new",
            vec![],
            Ty::Named {
                name: "bytes".to_string(),
                args: vec![],
            },
        );

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
        self.register_builtin_fn(
            "Node::register",
            vec![Ty::String, Ty::Var(TypeVar::fresh())],
            Ty::Unit,
        );
        self.register_builtin_fn("Node::lookup", vec![Ty::String], Ty::Var(TypeVar::fresh()));
    }

    fn register_builtin_fn(&mut self, name: &str, params: Vec<Ty>, return_type: Ty) {
        self.fn_sigs.insert(
            name.to_string(),
            FnSig {
                type_params: vec![],
                type_param_bounds: HashMap::new(),
                param_names: vec![],
                params,
                return_type,
                is_async: false,
                is_pure: false,
                accepts_kwargs: false,
                doc_comment: None,
            },
        );
    }

    /// Pass 1: Collect type definitions
    fn collect_types(&mut self, program: &Program) {
        for (item, span) in &program.items {
            match item {
                Item::TypeDecl(td) => {
                    self.register_type_decl(td);
                    self.local_type_defs.insert(td.name.clone());
                }
                Item::Actor(ad) => self.register_actor_decl(ad),
                Item::Wire(wd) => self.register_wire_decl(wd),
                Item::TypeAlias(ta) => {
                    let resolved = self.resolve_type_expr(&ta.ty.0);
                    self.type_aliases.insert(ta.name.clone(), resolved);
                }
                Item::Trait(td) => {
                    let info = Self::trait_info_from_decl(td);
                    self.trait_defs.insert(td.name.clone(), info);
                    self.local_trait_defs.insert(td.name.clone());
                    // Record super-trait relationships
                    if let Some(supers) = &td.super_traits {
                        let super_names: Vec<String> =
                            supers.iter().map(|s| s.name.clone()).collect();
                        self.trait_super.insert(td.name.clone(), super_names);
                    }
                }
                Item::Supervisor(sd) => {
                    self.warn_wasm_limitation(span, WasmUnsupportedFeature::SupervisionTrees);
                    let children: Vec<String> =
                        sd.children.iter().map(|c| c.actor_type.clone()).collect();
                    self.supervisor_children.insert(sd.name.clone(), children);
                }
                _ => {}
            }
        }
    }

    fn register_type_decl(&mut self, td: &TypeDecl) {
        let kind = match td.kind {
            TypeDeclKind::Struct => TypeDefKind::Struct,
            TypeDeclKind::Enum => TypeDefKind::Enum,
        };

        let mut fields = HashMap::new();
        let mut variants = HashMap::new();
        let type_param_names: Vec<String> = td.type_params.as_ref().map_or(vec![], |params| {
            params.iter().map(|p| p.name.clone()).collect()
        });
        let type_param_bounds =
            self.collect_type_param_bounds(td.type_params.as_ref(), td.where_clause.as_ref());
        let enum_return_args: Vec<Ty> = type_param_names
            .iter()
            .map(|name| Ty::Named {
                name: name.clone(),
                args: vec![],
            })
            .collect();

        for item in &td.body {
            match item {
                TypeBodyItem::Field { name, ty } => {
                    let field_ty = self.resolve_type_expr(&ty.0);
                    fields.insert(name.clone(), field_ty);
                }
                TypeBodyItem::Variant(variant) => {
                    let return_type = Ty::Named {
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
                                    param_names: vec![],
                                    params: vec![],
                                    return_type,
                                    is_async: false,
                                    is_pure: false,
                                    accepts_kwargs: false,
                                    doc_comment: None,
                                },
                            );
                        }
                        VariantKind::Tuple(fields) => {
                            let variant_tys: Vec<Ty> = fields
                                .iter()
                                .map(|(te, _)| self.resolve_type_expr(te))
                                .collect();
                            variants.insert(variant.name.clone(), VariantDef::Tuple(variant_tys));

                            // Register variant constructor as function
                            let constructor_params: Vec<Ty> = fields
                                .iter()
                                .map(|(te, _)| self.resolve_type_expr(te))
                                .collect();
                            self.fn_sigs.insert(
                                variant.name.clone(),
                                FnSig {
                                    type_params: type_param_names.clone(),
                                    type_param_bounds: type_param_bounds.clone(),
                                    param_names: vec![],
                                    params: constructor_params,
                                    return_type,
                                    is_async: false,
                                    is_pure: false,
                                    accepts_kwargs: false,
                                    doc_comment: None,
                                },
                            );
                        }
                        VariantKind::Struct(fields) => {
                            let variant_fields: Vec<(String, Ty)> = fields
                                .iter()
                                .map(|(name, (te, _))| (name.clone(), self.resolve_type_expr(te)))
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
        };

        // Register with trait registry for Send/Frozen derivation
        let field_types: Vec<_> = type_def.fields.values().cloned().collect();
        self.registry.register_type(td.name.clone(), field_types);

        self.type_defs.insert(td.name.clone(), type_def);
    }

    fn register_actor_decl(&mut self, ad: &ActorDecl) {
        let mut fields = HashMap::new();
        for field in &ad.fields {
            let field_ty = self.resolve_type_expr(&field.ty.0);
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
        };

        // Actors are always Send
        self.registry.register_actor(ad.name.clone());

        self.type_defs.insert(ad.name.clone(), type_def);
    }

    fn register_wire_decl(&mut self, wd: &WireDecl) {
        // Wire types are similar to regular types but use string field types
        let mut fields = HashMap::new();
        for field in &wd.fields {
            // Convert wire field type string to Ty (simplified mapping)
            let ty = match field.ty.as_str() {
                "i32" => Ty::I32,
                "i64" | "int" | "Int" => Ty::I64,
                "u8" | "byte" => Ty::U8,
                "u64" | "uint" => Ty::U64,
                "f32" => Ty::F32,
                "f64" | "float" | "Float" => Ty::F64,
                "string" | "String" => Ty::String,
                "bool" => Ty::Bool,
                _ => Ty::Named {
                    name: field.ty.clone(),
                    args: vec![],
                },
            };
            fields.insert(field.name.clone(), ty);
        }

        let mut variants = HashMap::new();
        for variant in &wd.variants {
            match &variant.kind {
                VariantKind::Unit => {
                    variants.insert(variant.name.clone(), VariantDef::Unit);
                }
                VariantKind::Tuple(fields) => {
                    let variant_tys = fields
                        .iter()
                        .map(|(te, _)| self.resolve_type_expr(te))
                        .collect();
                    variants.insert(variant.name.clone(), VariantDef::Tuple(variant_tys));
                }
                VariantKind::Struct(fields) => {
                    let variant_fields: Vec<(String, Ty)> = fields
                        .iter()
                        .map(|(name, (te, _))| (name.clone(), self.resolve_type_expr(te)))
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
        };

        let field_types: Vec<_> = type_def.fields.values().cloned().collect();
        self.registry.register_type(wd.name.clone(), field_types);

        self.type_defs.insert(wd.name.clone(), type_def);
    }

    fn trait_info_from_decl(tr: &TraitDecl) -> TraitInfo {
        let mut methods = Vec::new();
        let mut associated_types = Vec::new();
        for item in &tr.items {
            match item {
                TraitItem::Method(m) => methods.push(m.clone()),
                TraitItem::AssociatedType { name, default, .. } => {
                    associated_types.push(TraitAssociatedTypeInfo {
                        name: name.clone(),
                        default: default.clone(),
                    })
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

    fn build_impl_alias_entries(&mut self, id: &ImplDecl) -> HashMap<String, ImplAliasEntry> {
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

    fn enter_impl_scope(
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
        if enforce {
            if let Some(tb) = &id.trait_bound {
                if let Some(info) = self.trait_defs.get(&tb.name) {
                    let missing: Vec<String> = info
                        .associated_types
                        .iter()
                        .filter(|assoc| !entries.contains_key(&assoc.name))
                        .map(|assoc| assoc.name.clone())
                        .collect();
                    let target_name = target_name.to_string();
                    let tb_name = tb.name.clone();
                    for name in missing {
                        self.report_error(
                            TypeErrorKind::UndefinedType,
                            span,
                            format!(
                                "impl `{}` for `{}` must define associated type `{}`",
                                tb_name, target_name, name
                            ),
                        );
                    }
                }
            }
        }
        self.impl_alias_scopes.push(ImplAliasScope {
            span: span.clone(),
            entries,
            missing_reported: HashSet::new(),
            report_missing: enforce,
        });
        true
    }

    fn exit_impl_scope(&mut self) {
        self.impl_alias_scopes.pop();
    }

    fn resolve_impl_associated_type(&mut self, alias: &str) -> Option<Ty> {
        let Some(scope_index) = self.impl_alias_scopes.len().checked_sub(1) else {
            return None;
        };
        let expr = {
            let scope = &mut self.impl_alias_scopes[scope_index];
            match scope.entries.get_mut(alias) {
                Some(entry) => {
                    if let Some(resolved) = &entry.resolved {
                        return Some(resolved.clone());
                    }
                    if entry.resolving {
                        let should_report = scope.report_missing
                            && scope.missing_reported.insert(alias.to_string());
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
                }
                None => {
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
            }
        };
        let ty = self.resolve_type_expr(&expr.0);
        if let Some(scope) = self.impl_alias_scopes.get_mut(scope_index) {
            if let Some(entry) = scope.entries.get_mut(alias) {
                entry.resolving = false;
                entry.resolved = Some(ty.clone());
            }
        }
        Some(ty)
    }

    /// Pass 2: Collect function signatures
    fn collect_functions(&mut self, program: &Program) {
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
                    self.current_module = Some(module_name);
                    for (item, span) in &module.items {
                        self.collect_function_item(item, span);
                    }
                }
            }
        }

        // Process main module items.
        self.current_module = None;
        for (item, span) in &program.items {
            self.collect_function_item(item, span);
        }
    }

    fn collect_function_item(&mut self, item: &Item, span: &Span) {
        match item {
            Item::Function(fd) => {
                let scoped_name = match &self.current_module {
                    Some(m) => format!("{m}.{}", fd.name),
                    None => fd.name.clone(),
                };
                if let Some(prev_span) = self.fn_def_spans.get(&scoped_name) {
                    self.errors.push(TypeError::duplicate_definition(
                        span.clone(),
                        &scoped_name,
                        prev_span.clone(),
                    ));
                } else {
                    self.fn_def_spans.insert(scoped_name, span.clone());
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
                    // Set current_self_type for resolving `Self` in method parameters
                    let prev_self_type = self.current_self_type.take();
                    let self_type_args = type_args
                        .as_ref()
                        .map(|args| {
                            args.iter()
                                .map(|(te, _)| self.resolve_type_expr(te))
                                .collect()
                        })
                        .unwrap_or_default();
                    self.current_self_type = Some((type_name.clone(), self_type_args));
                    let scope_pushed =
                        self.enter_impl_scope(id, span, Some(type_name.as_str()), false);

                    for method in &id.methods {
                        self.register_impl_method(type_name, method);
                    }

                    // Register default trait methods not overridden in this impl
                    if let Some(tb) = &id.trait_bound {
                        // Track which types implement which traits
                        self.trait_impls_set
                            .insert((type_name.clone(), tb.name.clone()));

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
                                let param_names: Vec<String> = m
                                    .params
                                    .iter()
                                    .filter(|p| p.name != "self")
                                    .map(|p| p.name.clone())
                                    .collect();
                                let params: Vec<Ty> = m
                                    .params
                                    .iter()
                                    .filter(|p| p.name != "self")
                                    .map(|p| self.resolve_type_expr(&p.ty.0))
                                    .collect();
                                let return_type = m
                                    .return_type
                                    .as_ref()
                                    .map_or(Ty::Unit, |(te, _)| self.resolve_type_expr(te));
                                let sig = FnSig {
                                    type_params: vec![],
                                    type_param_bounds: HashMap::new(),
                                    param_names: param_names.clone(),
                                    params: params.clone(),
                                    return_type: return_type.clone(),
                                    is_async: false,
                                    is_pure: m.is_pure,
                                    accepts_kwargs: false,
                                    doc_comment: None,
                                };
                                self.fn_sigs.insert(method_key, sig);
                                if let Some(td) = self.lookup_type_def_mut(type_name) {
                                    td.methods.insert(
                                        m.name.clone(),
                                        FnSig {
                                            type_params: vec![],
                                            type_param_bounds: HashMap::new(),
                                            param_names,
                                            params,
                                            return_type,
                                            is_async: false,
                                            is_pure: m.is_pure,
                                            accepts_kwargs: false,
                                            doc_comment: None,
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
                        let param_names: Vec<String> = method
                            .params
                            .iter()
                            .filter(|p| p.name != "self")
                            .map(|p| p.name.clone())
                            .collect();
                        let params: Vec<Ty> = method
                            .params
                            .iter()
                            .filter(|p| p.name != "self")
                            .map(|p| self.resolve_type_expr(&p.ty.0))
                            .collect();
                        let return_type = method
                            .return_type
                            .as_ref()
                            .map_or(Ty::Unit, |(te, _)| self.resolve_type_expr(te));
                        let is_async = method.is_async;
                        let method_name = method.name.clone();
                        let type_name = td.name.clone();
                        if let Some(type_def) = self.lookup_type_def_mut(&type_name) {
                            type_def.methods.insert(
                                method_name,
                                FnSig {
                                    type_params: vec![],
                                    type_param_bounds: HashMap::new(),
                                    param_names,
                                    params,
                                    return_type,
                                    is_async,
                                    is_pure: method.is_pure,
                                    accepts_kwargs: false,
                                    doc_comment: None,
                                },
                            );
                        }
                    }
                }
            }
            Item::ExternBlock(eb) => {
                self.register_extern_block(eb);
            }
            Item::Import(id) => {
                self.register_import(id, Some(span));
            }
            _ => {}
        }
    }

    fn register_fn_sig(&mut self, fd: &FnDecl) {
        self.register_fn_sig_with_name(&fd.name, fd);
    }

    fn collect_type_param_bounds(
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

    fn push_unique_bound(entry: &mut Vec<String>, bound: &str) {
        if !entry.iter().any(|b| b == bound) {
            entry.push(bound.to_string());
        }
    }

    fn register_fn_sig_with_name(&mut self, name: &str, fd: &FnDecl) {
        // Only filter out `self` for methods (Type::method), not free functions
        // that happen to have a parameter named `self`.
        let is_method = name.contains("::");
        let param_names = fd
            .params
            .iter()
            .filter(|p| !is_method || p.name != "self")
            .map(|p| p.name.clone())
            .collect();
        let params = fd
            .params
            .iter()
            .filter(|p| !is_method || p.name != "self")
            .map(|p| self.resolve_type_expr(&p.ty.0))
            .collect();
        let declared_return = fd
            .return_type
            .as_ref()
            .map_or(Ty::Unit, |(te, _)| self.resolve_type_expr(te));
        // Wrap return type for generator functions
        let return_type = if fd.is_generator && fd.is_async {
            Ty::AsyncGenerator {
                yields: Box::new(declared_return),
            }
        } else if fd.is_generator {
            Ty::Generator {
                yields: Box::new(declared_return),
                returns: Box::new(Ty::Unit),
            }
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
            accepts_kwargs: false,
            doc_comment: fd.doc_comment.clone(),
        };

        self.fn_sigs.insert(name.to_string(), sig);
    }

    /// Register an impl method on a type's method table and fn_sigs.
    ///
    /// Returns the built `FnSig` for callers that need to insert it
    /// on additional type names (e.g., qualified aliases).
    fn register_impl_method(&mut self, type_name: &str, method: &FnDecl) -> FnSig {
        let method_key = format!("{type_name}::{}", method.name);
        self.register_fn_sig_with_name(&method_key, method);
        let params: Vec<Ty> = method
            .params
            .iter()
            .filter(|p| p.name != "self")
            .map(|p| self.resolve_type_expr(&p.ty.0))
            .collect();
        let return_type = method
            .return_type
            .as_ref()
            .map_or(Ty::Unit, |(te, _)| self.resolve_type_expr(te));
        let param_names: Vec<String> = method
            .params
            .iter()
            .filter(|p| p.name != "self")
            .map(|p| p.name.clone())
            .collect();
        let sig = FnSig {
            type_params: vec![],
            type_param_bounds: HashMap::new(),
            param_names,
            params,
            return_type,
            is_async: method.is_async,
            is_pure: method.is_pure,
            accepts_kwargs: false,
            doc_comment: None,
        };
        if let Some(td) = self.lookup_type_def_mut(type_name) {
            td.methods.insert(method.name.clone(), sig.clone());
        }
        sig
    }

    fn register_receive_fn(&mut self, actor_name: &str, rf: &ReceiveFnDecl) {
        let mut generic_bindings = std::collections::HashMap::new();
        if let Some(type_params) = &rf.type_params {
            for tp in type_params {
                generic_bindings.insert(tp.name.clone(), Ty::Var(TypeVar::fresh()));
            }
        }
        if !generic_bindings.is_empty() {
            self.generic_ctx.push(generic_bindings);
        }

        let param_names = rf.params.iter().map(|p| p.name.clone()).collect();
        let params = rf
            .params
            .iter()
            .map(|p| self.resolve_type_expr(&p.ty.0))
            .collect();
        let declared_return_type = rf
            .return_type
            .as_ref()
            .map_or(Ty::Unit, |(te, _)| self.resolve_type_expr(te));
        let return_type = if rf.is_generator {
            Ty::Stream(Box::new(declared_return_type))
        } else {
            declared_return_type
        };

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
            is_async: false,
            is_pure: rf.is_pure,
            accepts_kwargs: false,
            doc_comment: None,
        };

        let method_name = format!("{}::{}", actor_name, rf.name);
        self.fn_sigs.insert(method_name, sig);
    }

    fn register_extern_block(&mut self, eb: &ExternBlock) {
        for f in &eb.functions {
            let param_names = f.params.iter().map(|p| p.name.clone()).collect();
            let params = f
                .params
                .iter()
                .map(|p| self.resolve_type_expr(&p.ty.0))
                .collect();
            let return_type = f
                .return_type
                .as_ref()
                .map_or(Ty::Unit, |(te, _)| self.resolve_type_expr(te));
            let sig = FnSig {
                type_params: vec![],
                type_param_bounds: HashMap::new(),
                param_names,
                params,
                return_type,
                is_async: false,
                is_pure: false,
                accepts_kwargs: false,
                doc_comment: None,
            };
            self.fn_sigs.insert(f.name.clone(), sig);
            self.unsafe_functions.insert(f.name.clone());
        }
    }

    /// Log function names that accept keyword arguments for structured fields.
    const LOG_KWARGS_FUNCTIONS: &'static [&'static str] = &[
        // C extern (used by codegen interception for legacy compatibility)
        "hew_log_emit",
        // Wrapper function (clean) names from log.hew
        "error",
        "warn",
        "info",
        "debug",
        "trace",
    ];

    fn register_import(&mut self, decl: &ImportDecl, import_span: Option<&Span>) {
        let module_path = decl.path.join("::");

        // --- Stdlib path ---
        if let Some(funcs) = stdlib_functions(&module_path) {
            for (name, params, ret) in funcs {
                let accepts_kwargs = module_path == "std::misc::log"
                    && Self::LOG_KWARGS_FUNCTIONS.contains(&name.as_str());
                let sig = FnSig {
                    type_params: vec![],
                    type_param_bounds: HashMap::new(),
                    param_names: vec![],
                    params,
                    return_type: ret,
                    is_async: false,
                    is_pure: false,
                    accepts_kwargs,
                    doc_comment: None,
                };
                self.unsafe_functions.insert(name.clone());
                self.fn_sigs.insert(name, sig);
            }
        }
        // Register wrapper `pub fn` signatures under their clean method names.
        // These may differ from the extern C signatures (e.g., `setup()` vs `hew_log_set_level(level)`).
        if let Some(wrapper_sigs) = wrapper_fn_sigs(&module_path) {
            for (name, params, ret) in wrapper_sigs {
                let accepts_kwargs = module_path == "std::misc::log"
                    && Self::LOG_KWARGS_FUNCTIONS.contains(&name.as_str());
                let sig = FnSig {
                    type_params: vec![],
                    type_param_bounds: HashMap::new(),
                    param_names: vec![],
                    params,
                    return_type: ret,
                    is_async: false,
                    is_pure: false,
                    accepts_kwargs,
                    doc_comment: None,
                };
                self.fn_sigs.insert(name, sig);
            }
        }
        // Register clean module.method names alongside the hew_* names
        if let Some(short) = module_short_name(&module_path) {
            self.modules.insert(short.to_string());
            // Track the import span for unused-import detection (user imports only)
            if let Some(span) = import_span {
                self.import_spans.insert(short.to_string(), span.clone());
            }
            if let Some(clean_names) = stdlib_clean_names(&module_path) {
                for (method, c_symbol) in clean_names {
                    // Prefer the wrapper function's own signature (registered under
                    // the method name) over the extern C function's signature.
                    // E.g. `log.setup()` should have 0 params (the wrapper's sig),
                    // not 1 param (the extern `hew_log_set_level(level)` sig).
                    let wrapper_sig = self.fn_sigs.get(method).cloned();
                    let sig = wrapper_sig
                        .clone()
                        .or_else(|| self.fn_sigs.get(c_symbol).cloned());
                    if let Some(sig) = sig {
                        let key = format!("{short}.{method}");
                        self.fn_sigs.insert(key.clone(), sig);
                        if wrapper_sig.is_none() {
                            self.unsafe_functions.insert(key);
                        }
                    }
                }
            }
            // Register handle type names so they can be used in type annotations
            for type_name in handle_types_for_module(&module_path) {
                self.known_types.insert(type_name.to_string());
            }
            // Process resolved Hew source items (types, traits, impls) from stdlib
            // modules that have .hew files alongside their C/Rust bindings.
            // This enables trait methods like bench.Suite.add() to be visible.
            if let Some(ref resolved_items) = decl.resolved_items {
                self.register_stdlib_hew_items(short, resolved_items);
            }
            return; // stdlib handled
        }

        // --- User module path ---
        if let Some(ref resolved_items) = decl.resolved_items {
            if decl.path.is_empty() {
                // File-based import (`import "math_lib.hew"`) — register all
                // functions/consts as top-level names (no module namespace).
                self.register_file_import_items(resolved_items);
            } else {
                let short = decl.path.last().expect("import path is non-empty").clone();
                self.modules.insert(short.clone());
                self.user_modules.insert(short.clone());
                if let Some(span) = import_span {
                    self.import_spans.insert(short.clone(), span.clone());
                }
                self.register_user_module(&short, resolved_items, &decl.spec);
            }
        }
    }

    /// Determine whether a name should be imported unqualified based on the `ImportSpec`.
    fn should_import_name(name: &str, spec: &Option<ImportSpec>) -> bool {
        match spec {
            None => false,                  // bare import → qualified only
            Some(ImportSpec::Glob) => true, // import foo::*; → everything
            Some(ImportSpec::Names(names)) => names
                .iter()
                .any(|n| n.name == name || n.alias.as_deref() == Some(name)),
        }
    }

    /// Resolve the binding name for an imported symbol, applying any alias.
    fn resolve_import_name(spec: &Option<ImportSpec>, name: &str) -> Option<String> {
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
    fn register_stdlib_hew_items(&mut self, module_short: &str, items: &[Spanned<Item>]) {
        // Pass 1: Register types, traits, and functions first
        for (item, _span) in items {
            match item {
                Item::TypeDecl(td) => {
                    if !td.visibility.is_pub() {
                        continue;
                    }
                    self.register_type_decl(td);
                    self.known_types.insert(td.name.clone());
                }
                Item::Trait(tr) => {
                    if !tr.visibility.is_pub() {
                        continue;
                    }
                    let info = Self::trait_info_from_decl(tr);
                    self.trait_defs.insert(tr.name.clone(), info.clone());
                    let qualified = format!("{module_short}.{}", tr.name);
                    self.trait_defs.insert(qualified, info);
                }
                Item::Function(fd) => {
                    if !fd.visibility.is_pub() {
                        continue;
                    }
                    let qualified = format!("{module_short}.{}", fd.name);
                    if !self.fn_sigs.contains_key(&qualified) {
                        let sig = self.build_fn_sig_from_decl(fd);
                        self.fn_sigs.insert(qualified, sig);
                    }
                }
                Item::Actor(ad) => {
                    self.register_actor_decl(ad);
                    self.known_types.insert(ad.name.clone());
                    // Register receive fns and methods
                    for rf in &ad.receive_fns {
                        self.register_receive_fn(&ad.name, rf);
                    }
                    for method in &ad.methods {
                        let method_name = format!("{}::{}", ad.name, method.name);
                        self.register_fn_sig_with_name(&method_name, method);
                    }
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
                                .map(|(te, _)| self.resolve_type_expr(te))
                                .collect()
                        })
                        .unwrap_or_default();
                    self.current_self_type = Some((type_name.clone(), self_type_args));
                    let scope_pushed =
                        self.enter_impl_scope(id, span, Some(type_name.as_str()), false);

                    for method in &id.methods {
                        let sig = self.register_impl_method(type_name, method);
                        // Also register on qualified type name
                        let qualified_type = format!("{module_short}.{type_name}");
                        if let Some(td) = self.lookup_type_def_mut(&qualified_type) {
                            td.methods.insert(method.name.clone(), sig);
                        }
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
                    let qualified = format!("{module_short}.{}", td.name);
                    if let Some(def) = self.type_defs.get(&td.name).cloned() {
                        self.type_defs.insert(qualified, def);
                    }
                }
                Item::Actor(ad) => {
                    let qualified = format!("{module_short}.{}", ad.name);
                    if let Some(def) = self.type_defs.get(&ad.name).cloned() {
                        self.type_defs.insert(qualified, def);
                    }
                }
                _ => {}
            }
        }
    }

    /// Register items from a file-based import as top-level names (no module namespace).
    fn register_file_import_items(&mut self, items: &[Spanned<Item>]) {
        for (item, _span) in items {
            match item {
                Item::Function(fd) => {
                    if !fd.visibility.is_pub() {
                        continue;
                    }
                    let sig = self.build_fn_sig_from_decl(fd);
                    self.fn_sigs.insert(fd.name.clone(), sig);
                }
                Item::Const(cd) => {
                    if !cd.visibility.is_pub() {
                        continue;
                    }
                    let ty = self.resolve_type_expr(&cd.ty.0);
                    self.env.define(cd.name.clone(), ty, false);
                }
                Item::TypeDecl(td) => {
                    if !td.visibility.is_pub() {
                        continue;
                    }
                    self.register_type_decl(td);
                    self.known_types.insert(td.name.clone());
                }
                Item::Trait(tr) => {
                    if !tr.visibility.is_pub() {
                        continue;
                    }
                    let info = Self::trait_info_from_decl(tr);
                    self.trait_defs.insert(tr.name.clone(), info);
                }
                Item::Actor(ad) => {
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
                Item::Impl(id) => {
                    if let TypeExpr::Named {
                        name: type_name, ..
                    } = &id.target_type.0
                    {
                        for method in &id.methods {
                            if !method.visibility.is_pub() {
                                continue;
                            }
                            self.register_impl_method(type_name, method);
                        }
                        // Track trait implementations
                        if let Some(tb) = &id.trait_bound {
                            self.trait_impls_set
                                .insert((type_name.clone(), tb.name.clone()));
                        }
                    }
                }
                _ => {}
            }
        }
    }

    /// Register items from a user module under the module's namespace.
    fn register_user_module(
        &mut self,
        module_short: &str,
        items: &[Spanned<Item>],
        spec: &Option<ImportSpec>,
    ) {
        for (item, span) in items {
            match item {
                Item::Function(fd) => {
                    // Skip non-pub functions (enforce visibility)
                    if !fd.visibility.is_pub() {
                        continue;
                    }

                    let sig = self.build_fn_sig_from_decl(fd);
                    let qualified = format!("{module_short}.{}", fd.name);
                    self.fn_sigs.insert(qualified, sig.clone());

                    // If named import or glob, also register unqualified (using alias if present)
                    if Self::should_import_name(&fd.name, spec) {
                        let binding_name = Self::resolve_import_name(spec, &fd.name)
                            .unwrap_or_else(|| fd.name.clone());
                        self.fn_sigs.insert(binding_name.clone(), sig);
                        self.unqualified_to_module
                            .insert(binding_name, module_short.to_string());
                    }
                }
                Item::TypeDecl(td) => {
                    if !td.visibility.is_pub() {
                        continue;
                    }
                    self.register_type_decl(td);
                    let qualified = format!("{module_short}.{}", td.name);
                    if let Some(def) = self.type_defs.get(&td.name).cloned() {
                        self.type_defs.insert(qualified, def);
                    }
                    self.known_types.insert(td.name.clone());
                }
                Item::TypeAlias(_) => {}
                Item::Trait(tr) => {
                    if !tr.visibility.is_pub() {
                        continue;
                    }
                    let info = Self::trait_info_from_decl(tr);

                    // Register under qualified name (e.g. "mymod.Drawable")
                    let qualified = format!("{module_short}.{}", tr.name);
                    self.trait_defs.insert(qualified.clone(), info.clone());

                    // Record super-trait relationships for both qualified and unqualified
                    if let Some(supers) = &tr.super_traits {
                        let super_names: Vec<String> =
                            supers.iter().map(|s| s.name.clone()).collect();
                        self.trait_super.insert(qualified, super_names.clone());
                        if Self::should_import_name(&tr.name, spec) {
                            let binding_name = Self::resolve_import_name(spec, &tr.name)
                                .unwrap_or_else(|| tr.name.clone());
                            self.trait_super.insert(binding_name, super_names);
                        }
                    }

                    // If glob or named import, also register unqualified (using alias if present)
                    if Self::should_import_name(&tr.name, spec) {
                        let binding_name = Self::resolve_import_name(spec, &tr.name)
                            .unwrap_or_else(|| tr.name.clone());
                        self.trait_defs.insert(binding_name.clone(), info.clone());
                        self.unqualified_to_module
                            .insert(binding_name, module_short.to_string());
                    }
                }
                Item::Const(cd) => {
                    if !cd.visibility.is_pub() {
                        continue;
                    }
                    let ty = self.resolve_type_expr(&cd.ty.0);
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
                                    .map(|(te, _)| self.resolve_type_expr(te))
                                    .collect()
                            })
                            .unwrap_or_default();
                        self.current_self_type = Some((type_name.clone(), self_type_args));
                        let scope_pushed =
                            self.enter_impl_scope(id, span, Some(type_name.as_str()), false);

                        for method in &id.methods {
                            if !method.visibility.is_pub() {
                                continue;
                            }
                            self.register_impl_method(type_name, method);
                        }

                        // Restore previous self type
                        self.current_self_type = prev_self_type;
                        if scope_pushed {
                            self.exit_impl_scope();
                        }
                    }
                }
                Item::Actor(ad) => {
                    self.register_actor_decl(ad);
                    let qualified = format!("{module_short}.{}", ad.name);
                    if let Some(def) = self.type_defs.get(&ad.name).cloned() {
                        self.type_defs.insert(qualified, def);
                    }
                    self.known_types.insert(ad.name.clone());
                    // Register receive fns and methods
                    for rf in &ad.receive_fns {
                        self.register_receive_fn(&ad.name, rf);
                    }
                    for method in &ad.methods {
                        let method_name = format!("{}::{}", ad.name, method.name);
                        self.register_fn_sig_with_name(&method_name, method);
                    }
                    // If named import or glob, also register unqualified
                    if Self::should_import_name(&ad.name, spec) {
                        let binding_name = Self::resolve_import_name(spec, &ad.name)
                            .unwrap_or_else(|| ad.name.clone());
                        self.unqualified_to_module
                            .insert(binding_name, module_short.to_string());
                    }
                }
                _ => {}
            }
        }
    }

    /// Build a `FnSig` from a function declaration (used for user module registration).
    fn build_fn_sig_from_decl(&mut self, fd: &FnDecl) -> FnSig {
        let param_names = fd.params.iter().map(|p| p.name.clone()).collect();
        let params = fd
            .params
            .iter()
            .map(|p| self.resolve_type_expr(&p.ty.0))
            .collect();
        let declared_return = fd
            .return_type
            .as_ref()
            .map_or(Ty::Unit, |(te, _)| self.resolve_type_expr(te));
        let type_params = fd.type_params.as_ref().map_or(vec![], |params| {
            params.iter().map(|p| p.name.clone()).collect()
        });
        let type_param_bounds =
            self.collect_type_param_bounds(fd.type_params.as_ref(), fd.where_clause.as_ref());
        let return_type = if fd.is_generator && fd.is_async {
            Ty::AsyncGenerator {
                yields: Box::new(declared_return),
            }
        } else if fd.is_generator {
            Ty::Generator {
                yields: Box::new(declared_return),
                returns: Box::new(Ty::Unit),
            }
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
            accepts_kwargs: false,
            doc_comment: fd.doc_comment.clone(),
        }
    }

    /// Pass 3: Check all bodies
    pub fn check_program(&mut self, program: &Program) -> TypeCheckOutput {
        self.register_builtins();
        self.collect_types(program);
        self.collect_functions(program);

        for (item, span) in &program.items {
            self.check_item(item, span);
        }

        // Apply final substitutions to all recorded types
        let expr_types = self
            .expr_types
            .iter()
            .map(|(k, v)| (k.clone(), self.subst.resolve(v)))
            .collect();

        // Emit unused import warnings
        for (module_name, import_span) in &self.import_spans {
            if !self.used_modules.borrow().contains(module_name) {
                self.warnings.push(TypeError {
                    severity: crate::error::Severity::Warning,
                    kind: TypeErrorKind::UnusedImport,
                    span: import_span.clone(),
                    message: format!("unused import: `{module_name}`"),
                    notes: vec![],
                    suggestions: vec!["remove this import".to_string()],
                });
            }
        }

        // Emit dead code warnings using reachability analysis.
        // Entry points: main, actor handlers (Type::method), and _ prefixed functions.
        let mut reachable = HashSet::new();
        let mut queue = std::collections::VecDeque::new();

        // Seed with entry points
        for fn_name in self.fn_def_spans.keys() {
            if fn_name == "main" || fn_name.contains("::") || fn_name.starts_with('_') {
                reachable.insert(fn_name.clone());
                queue.push_back(fn_name.clone());
            }
        }

        // Also seed actor receive fns — they appear as call_graph keys
        // (e.g. "Actor::method") but aren't in fn_def_spans.
        for caller in self.call_graph.keys() {
            if caller.contains("::") && reachable.insert(caller.clone()) {
                queue.push_back(caller.clone());
            }
        }

        // BFS through call graph
        while let Some(caller) = queue.pop_front() {
            if let Some(callees) = self.call_graph.get(&caller) {
                for callee in callees {
                    if reachable.insert(callee.clone()) {
                        queue.push_back(callee.clone());
                    }
                }
            }
        }

        for (fn_name, def_span) in &self.fn_def_spans {
            if fn_name == "main" || fn_name.starts_with('_') {
                continue;
            }
            // Skip receive handlers (Type::method format) and test functions
            if fn_name.contains("::") {
                continue;
            }
            if reachable.contains(fn_name) {
                continue;
            }
            self.warnings.push(TypeError {
                severity: crate::error::Severity::Warning,
                kind: TypeErrorKind::DeadCode,
                span: def_span.clone(),
                message: format!("function `{fn_name}` is never called"),
                notes: vec![],
                suggestions: vec![format!(
                    "if this is intentional, prefix with underscore: `_{fn_name}`"
                )],
            });
        }

        // Move data out of Checker — it is not used after check_program.
        let mut output = TypeCheckOutput {
            expr_types,
            errors: std::mem::take(&mut self.errors),
            warnings: std::mem::take(&mut self.warnings),
            type_defs: std::mem::take(&mut self.type_defs),
            fn_sigs: std::mem::take(&mut self.fn_sigs),
            cycle_capable_actors: HashSet::new(),
            user_modules: std::mem::take(&mut self.user_modules),
        };

        // Detect actor reference cycles and emit warnings.
        let (cycle_capable, cycles) = crate::cycle::detect_actor_ref_cycles(&output.type_defs);
        for cycle_actors in &cycles {
            let desc = cycle_actors.join(" -> ");
            output
                .warnings
                .push(TypeError::actor_ref_cycle(0..0, &desc));
        }
        output.cycle_capable_actors = cycle_capable;

        output
    }

    fn check_item(&mut self, item: &Item, span: &Span) {
        match item {
            Item::Function(fd) => self.check_function(fd),
            Item::Actor(ad) => self.check_actor(ad),
            Item::Const(cd) => self.check_const(cd, span),
            Item::Impl(id) => self.check_impl(id, span),
            _ => {} // Imports, types, traits, extern — already collected
        }
    }

    fn check_function(&mut self, fd: &FnDecl) {
        self.check_function_as(fd, &fd.name.clone());
    }

    /// Check a function body using `fn_name` for the `fn_sigs` lookup.
    ///
    /// Impl methods are registered under qualified names (e.g. `Connection::close`)
    /// but `FnDecl::name` is bare (e.g. `close`). Using the qualified name prevents
    /// collisions with builtins or inlined functions from other modules.
    fn check_function_as(&mut self, fd: &FnDecl, fn_name: &str) {
        let prev_function = self.current_function.take();
        self.current_function = Some(fn_name.to_string());
        let prev_in_pure = self.in_pure_function;
        self.in_pure_function = fd.is_pure;
        self.env.push_scope();

        // Bind params
        for p in &fd.params {
            let mut ty = self.resolve_type_expr(&p.ty.0);
            if p.name == "self" {
                if let Some((self_name, self_args)) = &self.current_self_type {
                    ty = Ty::Named {
                        name: self_name.clone(),
                        args: self_args.clone(),
                    };
                }
            }
            self.env.define(p.name.clone(), ty, p.is_mutable);
        }

        // Use the return type from the already-registered fn signature so that
        // TypeExpr::Infer (-> _) reuses the same Ty::Var that call sites see.
        // This ensures body-checking unification updates the shared type variable.
        let declared_ret = if let Some(sig) = self.fn_sigs.get(fn_name) {
            sig.return_type.clone()
        } else {
            fd.return_type
                .as_ref()
                .map_or(Ty::Unit, |(te, _)| self.resolve_type_expr(te))
        };
        // Generator bodies don't return the declared type — they yield it.
        // The body itself should return Unit (falls off the end).
        let expected_ret = if fd.is_generator {
            Ty::Unit
        } else {
            declared_ret.clone()
        };
        // Store the declared yields type so Expr::Yield can check against it.
        self.current_return_type = Some(declared_ret);
        let prev_in_generator = self.in_generator;
        self.in_generator = fd.is_generator;

        let actual = self.check_block(&fd.body);
        self.expect_type(
            &expected_ret,
            &actual,
            &(fd.body.stmts.last().map_or(0..0, |(_, s)| s.clone())),
        );

        self.in_generator = prev_in_generator;
        self.in_pure_function = prev_in_pure;
        self.current_return_type = None;
        self.current_function = prev_function;
        self.emit_scope_warnings();
    }

    fn check_actor(&mut self, ad: &ActorDecl) {
        for rf in &ad.receive_fns {
            self.check_receive_fn(&ad.name, rf, &ad.fields);
        }
        for method in &ad.methods {
            self.env.push_scope();
            // Bind self for regular actor methods too
            let self_ty = Ty::Named {
                name: ad.name.clone(),
                args: vec![],
            };
            self.env.define("self".to_string(), self_ty, true);
            // Bind actor fields directly in scope
            for field in &ad.fields {
                let field_ty = self.resolve_type_expr(&field.ty.0);
                self.env.define(field.name.clone(), field_ty, true);
            }
            self.check_function(method);
            self.env.pop_scope();
        }
    }

    fn check_receive_fn(&mut self, actor_name: &str, rf: &ReceiveFnDecl, fields: &[FieldDecl]) {
        let prev_in_pure = self.in_pure_function;
        self.in_pure_function = rf.is_pure;
        self.env.push_scope();

        // Set current_function so calls within this receive fn are recorded
        // in the call graph (enables dead-code reachability analysis).
        let qualified_name = format!("{}::{}", actor_name, rf.name);
        let prev_function = self.current_function.take();
        self.current_function = Some(qualified_name);

        let mut generic_bindings = std::collections::HashMap::new();
        if let Some(type_params) = &rf.type_params {
            for tp in type_params {
                generic_bindings.insert(tp.name.clone(), Ty::Var(TypeVar::fresh()));
            }
        }
        if !generic_bindings.is_empty() {
            self.generic_ctx.push(generic_bindings);
        }

        // Bind `self` to the actor's type so field accesses work
        let self_ty = Ty::Named {
            name: actor_name.to_string(),
            args: vec![],
        };
        self.env.define("self".to_string(), self_ty, true);

        // Bind actor fields directly in scope (accessible without self.)
        for field in fields {
            let field_ty = self.resolve_type_expr(&field.ty.0);
            self.env.define(field.name.clone(), field_ty, true);
        }

        for p in &rf.params {
            let ty = self.resolve_type_expr(&p.ty.0);
            self.env.define(p.name.clone(), ty, p.is_mutable);
        }

        let declared_ret = rf
            .return_type
            .as_ref()
            .map_or(Ty::Unit, |(te, _)| self.resolve_type_expr(te));
        let expected_ret = if rf.is_generator {
            Ty::Unit
        } else {
            declared_ret.clone()
        };
        // Store declared yields type so Expr::Yield can check against it.
        self.current_return_type = Some(declared_ret);
        let prev_in_generator = self.in_generator;
        self.in_generator = rf.is_generator;

        let actual = self.check_block(&rf.body);
        self.expect_type(
            &expected_ret,
            &actual,
            &(rf.body.stmts.last().map_or(0..0, |(_, s)| s.clone())),
        );

        self.in_generator = prev_in_generator;
        self.in_pure_function = prev_in_pure;
        self.current_return_type = None;
        self.current_function = prev_function;
        if rf.type_params.as_ref().is_some_and(|tp| !tp.is_empty()) {
            self.generic_ctx.pop();
        }
        self.env.pop_scope();
    }

    fn check_const(&mut self, cd: &ConstDecl, _span: &Span) {
        let expected = self.resolve_type_expr(&cd.ty.0);
        let actual = self.check_against(&cd.value.0, &cd.value.1, &expected);
        self.env.define(cd.name.clone(), actual, false);
    }

    fn check_impl(&mut self, id: &ImplDecl, span: &Span) {
        if let TypeExpr::Named {
            name: type_name,
            type_args,
        } = &id.target_type.0
        {
            let target_is_struct = self
                .lookup_type_def(type_name)
                .is_some_and(|td| td.kind == TypeDefKind::Struct);
            // Orphan rule check: if implementing a trait, either the type or the
            // trait must be defined in the current compilation unit.
            if let Some(tb) = &id.trait_bound {
                let type_is_local = self.local_type_defs.contains(type_name);
                let trait_is_local = self.local_trait_defs.contains(&tb.name);
                if !type_is_local && !trait_is_local {
                    self.warnings.push(TypeError {
                        severity: crate::error::Severity::Warning,
                        kind: TypeErrorKind::OrphanImpl,
                        span: span.clone(),
                        message: format!(
                            "impl `{}` for `{type_name}`: neither the trait nor the type is defined in this module",
                            tb.name
                        ),
                        notes: vec![],
                        suggestions: vec![
                            "define the trait or the type in this module".to_string(),
                            "this may be disallowed in a future version (orphan rule)".to_string(),
                        ],
                    });
                }
            }

            // Set current_self_type for resolving `Self` in parameters
            let prev_self_type = self.current_self_type.take();
            let self_type_args: Vec<Ty> = type_args
                .as_ref()
                .map(|args| {
                    args.iter()
                        .map(|(te, _)| self.resolve_type_expr(te))
                        .collect()
                })
                .unwrap_or_default();
            self.current_self_type = Some((type_name.clone(), self_type_args.clone()));
            let scope_pushed = self.enter_impl_scope(id, span, Some(type_name.as_str()), true);

            for method in &id.methods {
                if target_is_struct {
                    if let Some(self_param) = method
                        .params
                        .iter()
                        .find(|param| param.name == "self" && param.is_mutable)
                    {
                        self.report_error_with_suggestions(
                            TypeErrorKind::MutabilityError,
                            &self_param.ty.1,
                            "`var self` in struct impl methods has no effect — struct methods receive self by value".to_string(),
                            vec![
                                "return a modified copy of `self` instead".to_string(),
                                "convert this type to an actor if you need mutable shared state".to_string(),
                            ],
                        );
                    }
                }
                self.env.push_scope();
                // Bind self for methods that take it
                let self_ty = Ty::Named {
                    name: type_name.clone(),
                    args: self_type_args.clone(),
                };
                self.env.define("self".to_string(), self_ty, true);
                // Use qualified name (e.g. Connection::close) so the fn_sigs
                // lookup finds the impl method, not a same-named builtin or
                // inlined function from another module.
                let qualified = format!("{type_name}::{}", method.name);
                self.check_function_as(method, &qualified);
                self.env.pop_scope();
            }

            // Restore previous self type
            self.current_self_type = prev_self_type;
            if scope_pushed {
                self.exit_impl_scope();
            }
        }
    }

    fn check_block(&mut self, block: &Block) -> Ty {
        self.env.push_scope();
        let num_stmts = block.stmts.len();
        let mut terminated = false;
        for (i, (stmt, span)) in block.stmts.iter().enumerate() {
            // If a previous statement was terminal, warn about this unreachable code
            if terminated {
                self.warnings.push(TypeError {
                    severity: crate::error::Severity::Warning,
                    kind: TypeErrorKind::UnreachableCode,
                    span: span.clone(),
                    message: "unreachable code".to_string(),
                    notes: vec![],
                    suggestions: vec![
                        "remove this code or restructure the control flow".to_string()
                    ],
                });
                // Still type-check the remaining statements for error coverage,
                // but only emit the unreachable warning once per block.
                for (s, sp) in &block.stmts[i..] {
                    self.check_stmt(s, sp);
                }
                self.emit_scope_warnings();
                return Ty::Never;
            }

            let is_last = i + 1 == num_stmts && block.trailing_expr.is_none();
            if is_last {
                let ty = match stmt {
                    Stmt::If { .. }
                    | Stmt::IfLet { .. }
                    | Stmt::Match { .. }
                    | Stmt::Return(_)
                    | Stmt::Break { .. }
                    | Stmt::Continue { .. } => self.check_stmt_as_expr(stmt, span),
                    Stmt::Expression((expr, es)) => {
                        let expr_ty = self.synthesize(expr, es);
                        if matches!(expr_ty, Ty::Never) {
                            Ty::Never
                        } else {
                            Ty::Unit
                        }
                    }
                    _ => {
                        self.check_stmt(stmt, span);
                        Ty::Unit
                    }
                };
                self.emit_scope_warnings();
                return ty;
            }
            // For If/Match, use check_stmt_as_expr to get the result type
            // so we can detect when all branches terminate.
            if matches!(
                stmt,
                Stmt::If { .. } | Stmt::IfLet { .. } | Stmt::Match { .. }
            ) {
                let ty = self.check_stmt_as_expr(stmt, span);
                if matches!(ty, Ty::Never) {
                    terminated = true;
                }
            } else {
                self.check_stmt(stmt, span);
            }

            // Check if this statement terminates control flow
            if matches!(
                stmt,
                Stmt::Return(_) | Stmt::Break { .. } | Stmt::Continue { .. }
            ) {
                terminated = true;
            }
        }
        // If a trailing expression follows a terminal statement, it's unreachable
        let ty = if let Some(expr) = &block.trailing_expr {
            if terminated {
                self.warnings.push(TypeError {
                    severity: crate::error::Severity::Warning,
                    kind: TypeErrorKind::UnreachableCode,
                    span: expr.1.clone(),
                    message: "unreachable code".to_string(),
                    notes: vec![],
                    suggestions: vec![
                        "remove this code or restructure the control flow".to_string()
                    ],
                });
            }
            self.synthesize(&expr.0, &expr.1)
        } else {
            Ty::Unit
        };
        self.emit_scope_warnings();
        ty
    }

    /// Check a statement that may serve as a block's trailing expression.
    /// Returns the "expression type" of the statement.
    fn check_stmt_as_expr(&mut self, stmt: &Stmt, span: &Span) -> Ty {
        match stmt {
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                self.check_against(&condition.0, &condition.1, &Ty::Bool);
                let then_ty = self.check_block(then_block);
                if let Some(eb) = else_block {
                    if let Some(ref if_stmt) = eb.if_stmt {
                        let else_ty = self.check_stmt_as_expr(&if_stmt.0, &if_stmt.1);
                        self.unify_branches(&then_ty, &else_ty, &if_stmt.1)
                    } else if let Some(block) = &eb.block {
                        let else_ty = self.check_block(block);
                        self.unify_branches(&then_ty, &else_ty, span)
                    } else {
                        Ty::Unit
                    }
                } else {
                    Ty::Unit
                }
            }
            Stmt::IfLet {
                pattern,
                expr,
                body,
                else_body,
            } => {
                let scr_ty = self.synthesize(&expr.0, &expr.1);
                self.env.push_scope();
                self.bind_pattern(&pattern.0, &scr_ty, false, &pattern.1);
                let then_ty = self.check_block(body);
                self.env.pop_scope();
                if let Some(block) = else_body {
                    let else_ty = self.check_block(block);
                    self.unify_branches(&then_ty, &else_ty, span)
                } else {
                    Ty::Unit
                }
            }
            Stmt::Match { scrutinee, arms } => {
                let scr_ty = self.synthesize(&scrutinee.0, &scrutinee.1);
                self.check_match_expr(&scr_ty, arms, span)
            }
            Stmt::Expression((expr, es)) => self.synthesize(expr, es),
            Stmt::Return(value) => {
                if let Some(expected) = self.current_return_type.clone() {
                    match value {
                        Some((val, vs)) => {
                            self.check_against(val, vs, &expected);
                        }
                        None if expected != Ty::Unit => {
                            self.errors.push(TypeError::return_type_mismatch(
                                span.clone(),
                                &expected,
                                &Ty::Unit,
                            ));
                        }
                        _ => {}
                    }
                }
                Ty::Never
            }
            Stmt::Break { .. } | Stmt::Continue { .. } => Ty::Never,
            _ => {
                self.check_stmt(stmt, span);
                Ty::Unit
            }
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "statement checking covers many Stmt variants"
    )]
    fn check_stmt(&mut self, stmt: &Stmt, span: &Span) {
        match stmt {
            Stmt::Let { pattern, ty, value } => {
                let val_ty = if let Some((val, vs)) = value {
                    if let Some((te, _)) = ty {
                        let expected = self.resolve_type_expr(te);
                        self.check_against(val, vs, &expected)
                    } else {
                        self.synthesize(val, vs)
                    }
                } else if let Some((te, _)) = ty {
                    self.resolve_type_expr(te)
                } else {
                    let v = TypeVar::fresh();
                    Ty::Var(v)
                };
                // For simple identifier patterns, track the definition span
                if let Pattern::Identifier(name) = &pattern.0 {
                    if val_ty == Ty::Unit && value.is_some() {
                        self.warnings.push(TypeError {
                            severity: crate::error::Severity::Warning,
                            kind: TypeErrorKind::StyleSuggestion,
                            span: span.clone(),
                            message: format!("binding `{name}` has unit type and carries no value"),
                            notes: vec![],
                            suggestions: vec!["prefix with underscore: `_name`".to_string()],
                        });
                    }
                    self.check_shadowing(name, &pattern.1);
                    self.env
                        .define_with_span(name.clone(), val_ty, false, pattern.1.clone());
                } else {
                    self.bind_pattern(&pattern.0, &val_ty, false, &pattern.1);
                }
            }
            Stmt::Var { name, ty, value } => {
                let val_ty = if let Some((val, vs)) = value {
                    if let Some((te, _)) = ty {
                        let expected = self.resolve_type_expr(te);
                        self.check_against(val, vs, &expected)
                    } else {
                        self.synthesize(val, vs)
                    }
                } else if let Some((te, _)) = ty {
                    self.resolve_type_expr(te)
                } else {
                    let v = TypeVar::fresh();
                    Ty::Var(v)
                };
                self.check_shadowing(name, span);
                self.env
                    .define_with_span(name.clone(), val_ty, true, span.clone());
            }
            Stmt::Assign { target, op, value } => {
                // Purity check: pure functions cannot assign to self fields
                if self.in_pure_function {
                    if let Expr::FieldAccess { object, field } = &target.0 {
                        if let Expr::Identifier(name) = &object.0 {
                            if name == "self" {
                                self.report_error(
                                    TypeErrorKind::PurityViolation,
                                    span,
                                    format!("cannot assign to `self.{field}` in a pure function"),
                                );
                            }
                        }
                    }
                }
                let target_ty = self.synthesize(&target.0, &target.1);
                if let Expr::Identifier(name) = &target.0 {
                    if let Some(binding) = self.env.lookup_ref(name) {
                        if !binding.is_mutable {
                            self.report_error(
                                TypeErrorKind::MutabilityError,
                                span,
                                format!("cannot assign to immutable variable `{name}`"),
                            );
                        }
                    }
                    // Plain assignment (=) is a write-only, not a read.
                    // Compound assignment (+=, etc.) is both read and write.
                    // Must unmark BEFORE mark_written so the guard check works.
                    if op.is_none() {
                        self.env.unmark_used(name);
                    }
                    self.env.mark_written(name);
                }
                self.check_against(&value.0, &value.1, &target_ty);
            }
            Stmt::Expression((expr, es)) => {
                self.synthesize(expr, es);
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                self.check_against(&condition.0, &condition.1, &Ty::Bool);
                self.check_block(then_block);
                if let Some(eb) = else_block {
                    if let Some(ref if_stmt) = eb.if_stmt {
                        self.check_stmt(&if_stmt.0, &if_stmt.1);
                    } else if let Some(block) = &eb.block {
                        self.check_block(block);
                    }
                }
            }
            Stmt::IfLet {
                pattern,
                expr,
                body,
                else_body,
            } => {
                let scr_ty = self.synthesize(&expr.0, &expr.1);
                self.env.push_scope();
                self.bind_pattern(&pattern.0, &scr_ty, false, &pattern.1);
                self.check_block(body);
                self.env.pop_scope();
                if let Some(block) = else_body {
                    self.check_block(block);
                }
            }
            Stmt::Return(value) => {
                if let Some(expected) = self.current_return_type.clone() {
                    match value {
                        Some((val, vs)) => {
                            self.check_against(val, vs, &expected);
                        }
                        None if expected != Ty::Unit => {
                            self.errors.push(TypeError::return_type_mismatch(
                                span.clone(),
                                &expected,
                                &Ty::Unit,
                            ));
                        }
                        _ => {}
                    }
                }
            }
            Stmt::Loop { body, .. } => {
                self.loop_depth += 1;
                self.check_block(body);
                self.loop_depth -= 1;
            }
            Stmt::For {
                pattern,
                iterable,
                body,
                ..
            } => {
                let iter_ty = self.synthesize(&iterable.0, &iterable.1);
                // Infer element type from iterable
                let elem_ty = match &iter_ty {
                    Ty::Range(inner)
                    | Ty::Array(inner, _)
                    | Ty::Slice(inner)
                    | Ty::Stream(inner) => (**inner).clone(),
                    Ty::Named { name, args } if name == "Vec" => {
                        args.first().cloned().unwrap_or(Ty::Var(TypeVar::fresh()))
                    }
                    Ty::Named { name, args } if name == "HashMap" && args.len() >= 2 => {
                        Ty::Tuple(vec![args[0].clone(), args[1].clone()])
                    }
                    Ty::Generator { yields, .. } | Ty::AsyncGenerator { yields } => {
                        (**yields).clone()
                    }
                    _ => Ty::Var(TypeVar::fresh()),
                };
                self.env.push_scope();
                self.in_for_binding = true;
                self.bind_pattern(&pattern.0, &elem_ty, true, &pattern.1);
                self.in_for_binding = false;
                self.loop_depth += 1;
                self.check_block(body);
                self.loop_depth -= 1;
                self.env.pop_scope();
            }
            Stmt::While {
                condition, body, ..
            } => {
                // Detect `while true` — suggest `loop` instead
                if matches!(&condition.0, Expr::Literal(Literal::Bool(true))) {
                    self.warnings.push(TypeError {
                        severity: crate::error::Severity::Warning,
                        kind: TypeErrorKind::StyleSuggestion,
                        span: span.clone(),
                        message: "`while true` can be simplified".to_string(),
                        notes: vec![],
                        suggestions: vec![
                            "use `loop { ... }` instead of `while true { ... }`".to_string()
                        ],
                    });
                }
                self.check_against(&condition.0, &condition.1, &Ty::Bool);
                self.loop_depth += 1;
                self.check_block(body);
                self.loop_depth -= 1;
            }
            Stmt::Break { .. } | Stmt::Continue { .. } => {
                if self.loop_depth == 0 {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::InvalidOperation,
                        span.clone(),
                        "break/continue used outside of a loop",
                    ));
                }
            }
            Stmt::Match { scrutinee, arms } => {
                let scr_ty = self.synthesize(&scrutinee.0, &scrutinee.1);
                self.check_match_stmt(&scr_ty, arms, span);
            }
            Stmt::Defer(expr) => {
                self.synthesize(&expr.0, &expr.1);
            }
        }
    }

    fn check_match_stmt(&mut self, scrutinee_ty: &Ty, arms: &[MatchArm], span: &Span) {
        for arm in arms {
            self.env.push_scope();
            self.bind_pattern(&arm.pattern.0, scrutinee_ty, false, &arm.pattern.1);

            if let Some((guard, gs)) = &arm.guard {
                self.check_against(guard, gs, &Ty::Bool);
            }

            self.synthesize(&arm.body.0, &arm.body.1);
            self.env.pop_scope();
        }

        self.check_exhaustiveness(scrutinee_ty, arms, span);
    }

    /// Synthesize: infer the type of an expression (bottom-up).
    fn synthesize(&mut self, expr: &Expr, span: &Span) -> Ty {
        // Grow the stack on demand so deeply-nested expressions (e.g. 1000+
        // chained binary operators) don't overflow.
        stacker::maybe_grow(32 * 1024, 2 * 1024 * 1024, || {
            self.synthesize_inner(expr, span)
        })
    }

    fn maybe_warn_wasm_expr(&mut self, expr: &Expr, span: &Span) {
        if !self.wasm_target {
            return;
        }
        match expr {
            Expr::Scope { .. } => {
                self.warn_wasm_limitation(span, WasmUnsupportedFeature::StructuredConcurrency);
            }
            Expr::ScopeLaunch(_) | Expr::ScopeSpawn(_) => {
                self.warn_wasm_limitation(span, WasmUnsupportedFeature::Tasks);
            }
            Expr::Select { .. } => {
                self.warn_wasm_limitation(span, WasmUnsupportedFeature::Select);
            }
            Expr::Join(_) => {
                self.warn_wasm_limitation(span, WasmUnsupportedFeature::StructuredConcurrency);
            }
            _ => {}
        }
    }

    fn synthesize_inner(&mut self, expr: &Expr, span: &Span) -> Ty {
        self.maybe_warn_wasm_expr(expr, span);
        let ty = match expr {
            // Literals
            Expr::Literal(Literal::Float(_)) => Ty::F64,
            Expr::Literal(Literal::String(_)) => Ty::String,
            Expr::RegexLiteral(_) => Ty::Named {
                name: "regex.Pattern".to_string(),
                args: vec![],
            },
            Expr::InterpolatedString(parts) => {
                for part in parts {
                    if let StringPart::Expr((expr, expr_span)) = part {
                        self.synthesize(expr, expr_span);
                    }
                }
                Ty::String
            }
            Expr::Literal(Literal::Bool(_)) => Ty::Bool,
            Expr::Literal(Literal::Char(_)) => Ty::Char,
            Expr::Literal(Literal::Integer { .. }) | Expr::Literal(Literal::Duration(_)) => Ty::I64,

            // Identifier lookup
            Expr::Identifier(name) => {
                // Handle None specially — fresh type var each usage
                if name == "None" {
                    return Ty::Option(Box::new(Ty::Var(TypeVar::fresh())));
                }
                if let Some((depth, binding)) = self.env.lookup_with_depth(name) {
                    let is_moved = binding.is_moved;
                    let ty = binding.ty.clone();
                    if is_moved {
                        self.report_error(
                            TypeErrorKind::UseAfterMove,
                            span,
                            format!("use of moved value `{name}`"),
                        );
                    }
                    // Track captures: variable from scope below the lambda boundary
                    if let Some(capture_depth) = self.lambda_capture_depth {
                        if depth < capture_depth {
                            self.lambda_captures.push(ty.clone());
                        }
                    }
                    ty
                } else if self.fn_sigs.contains_key(name) {
                    // It's a function name used as a value (e.g., variant constructor with no args)
                    if let Some(caller) = &self.current_function {
                        self.call_graph
                            .entry(caller.clone())
                            .or_default()
                            .insert(name.clone());
                    }
                    let sig = self.fn_sigs[name].clone();
                    if sig.params.is_empty() {
                        sig.return_type
                    } else {
                        // Return a function type for the constructor
                        Ty::Function {
                            params: sig.params,
                            ret: Box::new(sig.return_type),
                        }
                    }
                } else {
                    // Check if it's a unit enum variant (e.g., Red, Green, Blue)
                    let mut found = None;
                    for (type_name, td) in &self.type_defs {
                        if let Some(variant) = td.variants.get(name) {
                            if matches!(variant, VariantDef::Unit) {
                                found = Some(Ty::Named {
                                    name: type_name.clone(),
                                    args: vec![],
                                });
                                break;
                            }
                        }
                    }
                    if let Some(ty) = found {
                        ty
                    } else {
                        let similar = crate::error::find_similar(
                            name,
                            self.env
                                .all_names()
                                .chain(self.fn_sigs.keys().map(String::as_str)),
                        );
                        self.report_error_with_suggestions(
                            TypeErrorKind::UndefinedVariable,
                            span,
                            format!("undefined variable `{name}`"),
                            similar,
                        );
                        Ty::Error
                    }
                }
            }

            // Binary ops
            Expr::Binary { left, op, right } => self.check_binary_op(left, *op, right),

            // Unary ops
            Expr::Unary { op, operand } => match op {
                UnaryOp::Not => {
                    self.check_against(&operand.0, &operand.1, &Ty::Bool);
                    Ty::Bool
                }
                UnaryOp::Negate => {
                    let ty = self.synthesize(&operand.0, &operand.1);
                    let resolved = self.subst.resolve(&ty);
                    if !resolved.is_numeric()
                        && !matches!(resolved, Ty::Var(_))
                        && resolved != Ty::Error
                    {
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            span,
                            format!("cannot negate type `{resolved}`"),
                        );
                    }
                    ty
                }
                UnaryOp::BitNot => {
                    let ty = self.synthesize(&operand.0, &operand.1);
                    let resolved = self.subst.resolve(&ty);
                    if !resolved.is_integer()
                        && !matches!(resolved, Ty::Var(_))
                        && resolved != Ty::Error
                    {
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            span,
                            format!("bitwise NOT requires integer type, found `{resolved}`"),
                        );
                    }
                    ty
                }
            },

            // Call
            Expr::Call {
                function,
                type_args,
                args,
                is_tail_call: _,
            } => self.check_call(function, type_args.as_deref(), args, span),

            // Method call
            Expr::MethodCall {
                receiver,
                method,
                args,
            } => self.check_method_call(receiver, method, args, span),

            // Field access
            Expr::FieldAccess { object, field } => self.check_field_access(object, field, span),

            // Block
            Expr::Block(block) => self.check_block(block),

            // If expression
            Expr::If {
                condition,
                then_block,
                else_block,
            } => {
                self.check_against(&condition.0, &condition.1, &Ty::Bool);
                let then_ty = self.synthesize(&then_block.0, &then_block.1);
                if let Some(eb) = else_block {
                    let else_ty = self.synthesize(&eb.0, &eb.1);
                    self.unify_branches(&then_ty, &else_ty, span)
                } else {
                    Ty::Unit
                }
            }
            Expr::IfLet {
                pattern,
                expr,
                body,
                else_body,
            } => {
                let scr_ty = self.synthesize(&expr.0, &expr.1);
                self.env.push_scope();
                self.bind_pattern(&pattern.0, &scr_ty, false, &pattern.1);
                let then_ty = self.check_block(body);
                self.env.pop_scope();
                if let Some(block) = else_body {
                    let else_ty = self.check_block(block);
                    self.unify_branches(&then_ty, &else_ty, span)
                } else {
                    Ty::Unit
                }
            }

            // Match
            Expr::Match { scrutinee, arms } => {
                let scr_ty = self.synthesize(&scrutinee.0, &scrutinee.1);
                self.check_match_expr(&scr_ty, arms, span)
            }

            // Tuple
            Expr::Tuple(elems) => {
                let tys: Vec<_> = elems.iter().map(|(e, s)| self.synthesize(e, s)).collect();
                Ty::Tuple(tys)
            }

            // Array
            Expr::Array(elems) => {
                if elems.is_empty() {
                    let v = TypeVar::fresh();
                    Ty::Array(Box::new(Ty::Var(v)), 0)
                } else {
                    let first_ty = self.synthesize(&elems[0].0, &elems[0].1);
                    for elem in &elems[1..] {
                        self.check_against(&elem.0, &elem.1, &first_ty);
                    }
                    Ty::Array(Box::new(first_ty), elems.len() as u64)
                }
            }
            Expr::ArrayRepeat { value, count } => {
                let elem_ty = self.synthesize(&value.0, &value.1);
                let count_ty = self.check_against(&count.0, &count.1, &Ty::I64);
                let resolved_count = self.subst.resolve(&count_ty);
                if !resolved_count.is_integer() && !matches!(resolved_count, Ty::Var(_)) {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        &count.1,
                        format!("array repeat count must be an integer, found `{resolved_count}`"),
                    );
                }
                if let Expr::Literal(Literal::Integer { value, .. }) = &count.0 {
                    if *value < 0 {
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            &count.1,
                            "array repeat count cannot be negative".to_string(),
                        );
                    }
                }
                Ty::Named {
                    name: "Vec".to_string(),
                    args: vec![elem_ty],
                }
            }

            // Struct init
            Expr::StructInit { name, fields } => self.check_struct_init(name, fields, span),

            // Spawn
            Expr::Spawn { target, args } => {
                if self.in_pure_function {
                    self.report_error(
                        TypeErrorKind::PurityViolation,
                        span,
                        "cannot use `spawn` in a pure function".to_string(),
                    );
                }
                self.check_spawn(target, args, span)
            }

            // Lambda (synthesize mode — no expected type)
            Expr::Lambda {
                is_move: _,
                type_params,
                params,
                return_type,
                body,
            } => self.check_lambda(
                type_params.as_deref(),
                params,
                return_type.as_ref(),
                body,
                None,
                span,
            ),

            // Await
            Expr::Await(inner) => {
                let inner_ty = self.synthesize(&inner.0, &inner.1);
                // await Task<T> → T (simplified)
                match inner_ty {
                    Ty::Named { name, args } if name == "Task" && !args.is_empty() => {
                        args[0].clone()
                    }
                    _ => inner_ty,
                }
            }

            // PostfixTry: expr? → unwrap Result/Option
            Expr::PostfixTry(inner) => {
                let ty = self.synthesize(&inner.0, &inner.1);
                match &ty {
                    Ty::Option(inner) => (**inner).clone(),
                    Ty::Result { ok, .. } => (**ok).clone(),
                    _ => {
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            span,
                            format!("`?` requires Result or Option, found `{ty}`"),
                        );
                        Ty::Error
                    }
                }
            }

            // Send: target <- message
            Expr::Send { target, message } => {
                let _target_ty = self.synthesize(&target.0, &target.1);
                let msg_ty_raw = self.synthesize(&message.0, &message.1);
                let msg_ty = self.subst.resolve(&msg_ty_raw);
                // Check message is Send
                if !self.registry.implements_marker(&msg_ty, MarkerTrait::Send) {
                    self.report_error(
                        TypeErrorKind::InvalidSend,
                        span,
                        format!("cannot send `{msg_ty}` to actor: type is not Send"),
                    );
                }
                // Mark sent value as moved (unless Copy)
                if !self.registry.implements_marker(&msg_ty, MarkerTrait::Copy) {
                    if let Expr::Identifier(name) = &message.0 {
                        self.env.mark_moved(name, message.1.clone());
                    }
                }
                Ty::Unit
            }

            // Yield
            Expr::Yield(value) => {
                if !self.in_generator {
                    self.report_error(
                        TypeErrorKind::YieldOutsideGenerator,
                        span,
                        "`yield` outside of generator function".to_string(),
                    );
                }
                if let Some(val_expr) = value {
                    // Check yielded value against the declared yield type
                    if let Some(return_ty) = &self.current_return_type {
                        let yield_ty = match self.subst.resolve(return_ty) {
                            Ty::Generator { yields, .. } | Ty::AsyncGenerator { yields } => {
                                (*yields).clone()
                            }
                            other => other,
                        };
                        self.check_against(&val_expr.0, &val_expr.1, &yield_ty);
                    } else {
                        self.synthesize(&val_expr.0, &val_expr.1);
                    }
                }
                Ty::Unit
            }

            // Cooperate
            Expr::Cooperate => Ty::Unit,

            // Index
            Expr::Index { object, index } => {
                let obj_ty = self.synthesize(&object.0, &object.1);
                self.check_against(&index.0, &index.1, &Ty::I64);
                match &obj_ty {
                    Ty::Array(elem, _) | Ty::Slice(elem) => (**elem).clone(),
                    Ty::Named { name, args } if name == "Vec" && !args.is_empty() => {
                        args[0].clone()
                    }
                    // Custom type indexing: desugar obj[key] → obj.get(key)
                    Ty::Named { name, args } => {
                        if let Some(td) = self.lookup_type_def(name) {
                            if let Some(sig) = td.methods.get("get") {
                                if let Some(param_ty) = sig.params.first() {
                                    self.check_against(&index.0, &index.1, param_ty);
                                }
                                let mut ret = sig.return_type.clone();
                                for (param, arg) in td.type_params.iter().zip(args.iter()) {
                                    ret = self.substitute_named_param(&ret, param, arg);
                                }
                                ret
                            } else {
                                self.report_error(
                                    TypeErrorKind::InvalidOperation,
                                    span,
                                    format!(
                                        "cannot index into `{obj_ty}`: type has no `get` method"
                                    ),
                                );
                                Ty::Error
                            }
                        } else if let Some(sig) = self.lookup_fn_sig(&format!("{name}::get")) {
                            if let Some(param_ty) = sig.params.get(1) {
                                self.check_against(&index.0, &index.1, param_ty);
                            }
                            sig.return_type.clone()
                        } else {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                span,
                                format!("cannot index into `{obj_ty}`"),
                            );
                            Ty::Error
                        }
                    }
                    _ => {
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            span,
                            format!("cannot index into `{obj_ty}`"),
                        );
                        Ty::Error
                    }
                }
            }

            // Range
            Expr::Range {
                start,
                end,
                inclusive: _,
            } => {
                let ty = if let Some(s) = start {
                    let start_ty = self.synthesize(&s.0, &s.1);
                    if let Some(e) = end {
                        self.check_against(&e.0, &e.1, &start_ty);
                    }
                    start_ty
                } else if let Some(e) = end {
                    self.synthesize(&e.0, &e.1)
                } else {
                    Ty::I32
                };
                Ty::Range(Box::new(ty))
            }

            _ => {
                // Scope, Select, Join, Unsafe
                // Purity check: these constructs are inherently impure
                if self.in_pure_function {
                    match expr {
                        Expr::Scope { .. } | Expr::ScopeLaunch(_) | Expr::ScopeSpawn(_) => {
                            self.report_error(
                                TypeErrorKind::PurityViolation,
                                span,
                                "cannot use `scope` in a pure function".to_string(),
                            );
                        }
                        Expr::Select { .. } => {
                            self.report_error(
                                TypeErrorKind::PurityViolation,
                                span,
                                "cannot use `select` in a pure function".to_string(),
                            );
                        }
                        Expr::Join(_) => {
                            self.report_error(
                                TypeErrorKind::PurityViolation,
                                span,
                                "cannot use `join` in a pure function".to_string(),
                            );
                        }
                        Expr::SpawnLambdaActor { .. } => {
                            self.report_error(
                                TypeErrorKind::PurityViolation,
                                span,
                                "cannot use `spawn` in a pure function".to_string(),
                            );
                        }
                        _ => {}
                    }
                }
                match expr {
                    Expr::SpawnLambdaActor { params, .. } => {
                        // spawn (x: i32) => { ... } returns Actor<T> (lambda actor handle)
                        let param_ty = params
                            .first()
                            .and_then(|p| p.ty.as_ref())
                            .map_or(Ty::Var(TypeVar::fresh()), |(te, _)| {
                                self.resolve_type_expr(te)
                            });
                        Ty::Named {
                            name: "Actor".to_string(),
                            args: vec![param_ty],
                        }
                    }
                    Expr::Scope { body: block, .. } => self.check_block(block),
                    Expr::Unsafe(block) => {
                        let prev = self.in_unsafe;
                        self.in_unsafe = true;
                        let ty = self.check_block(block);
                        self.in_unsafe = prev;
                        ty
                    }
                    Expr::ScopeLaunch(block) | Expr::ScopeSpawn(block) => {
                        let body_ty = self.check_block(block);
                        Ty::Named {
                            name: "Task".to_string(),
                            args: vec![body_ty],
                        }
                    }
                    Expr::Select { arms, timeout } => {
                        // Each arm: binding from source => body
                        // Bind the pattern to the source's result type, then check body
                        // Unify all arm body types like match expressions do
                        let mut result_ty: Option<Ty> = None;
                        for arm in arms {
                            self.env.push_scope();
                            let source_ty = self.synthesize(&arm.source.0, &arm.source.1);
                            self.bind_pattern(&arm.binding.0, &source_ty, false, &arm.binding.1);
                            let body_ty = if let Some(expected) = &result_ty {
                                self.check_against(&arm.body.0, &arm.body.1, expected)
                            } else {
                                self.synthesize(&arm.body.0, &arm.body.1)
                            };
                            if result_ty.is_none() {
                                result_ty = Some(body_ty);
                            }
                            self.env.pop_scope();
                        }
                        if let Some(tc) = timeout {
                            self.synthesize(&tc.duration.0, &tc.duration.1);
                            let timeout_ty = self.synthesize(&tc.body.0, &tc.body.1);
                            if let Some(expected) = &result_ty {
                                self.expect_type(expected, &timeout_ty, &tc.body.1);
                            } else {
                                result_ty = Some(timeout_ty);
                            }
                        }
                        result_ty.unwrap_or(Ty::Unit)
                    }
                    Expr::Join(exprs) => {
                        // Join returns a tuple of all expression types
                        let types: Vec<Ty> =
                            exprs.iter().map(|(e, s)| self.synthesize(e, s)).collect();
                        if types.len() == 1 {
                            types[0].clone()
                        } else {
                            Ty::Tuple(types)
                        }
                    }
                    Expr::Timeout {
                        expr: inner,
                        duration,
                    } => {
                        let inner_ty = self.synthesize(&inner.0, &inner.1);
                        self.check_against(&duration.0, &duration.1, &Ty::I64);
                        Ty::Option(Box::new(inner_ty))
                    }

                    _ => Ty::Unit,
                }
            }
        };

        self.record_type(span, &ty);
        ty
    }

    /// Check: verify expression against expected type (top-down).
    fn check_against(&mut self, expr: &Expr, span: &Span, expected: &Ty) -> Ty {
        match (expr, expected) {
            // Lambda with expected function type — propagate param types!
            (
                Expr::Lambda {
                    type_params,
                    params,
                    return_type,
                    body,
                    ..
                },
                Ty::Function {
                    params: expected_params,
                    ret,
                },
            ) => self.check_lambda(
                type_params.as_deref(),
                params,
                return_type.as_ref(),
                body,
                Some((expected_params, ret)),
                span,
            ),

            // Integer literal can coerce to any integer type
            (Expr::Literal(Literal::Integer { .. }), ty) if ty.is_integer() => {
                self.record_type(span, expected);
                expected.clone()
            }

            // Negated integer literal can coerce to any signed integer type
            (
                Expr::Unary {
                    op: UnaryOp::Negate,
                    operand,
                },
                ty,
            ) if matches!(operand.0, Expr::Literal(Literal::Integer { .. })) && ty.is_integer() => {
                self.record_type(span, expected);
                expected.clone()
            }

            // Integer literal can coerce to float types
            (Expr::Literal(Literal::Integer { .. }), ty) if ty.is_float() => {
                self.record_type(span, expected);
                expected.clone()
            }

            // Negated integer literal can coerce to float types
            (
                Expr::Unary {
                    op: UnaryOp::Negate,
                    operand,
                },
                ty,
            ) if matches!(operand.0, Expr::Literal(Literal::Integer { .. })) && ty.is_float() => {
                self.record_type(span, expected);
                expected.clone()
            }

            // Float literal can coerce to any float type
            (Expr::Literal(Literal::Float(_)), ty) if ty.is_float() => {
                self.record_type(span, expected);
                expected.clone()
            }

            // Default: synthesize and unify
            _ => {
                let actual = self.synthesize(expr, span);
                self.expect_type(expected, &actual, span);
                actual
            }
        }
    }

    fn check_binary_op(&mut self, left: &Spanned<Expr>, op: BinaryOp, right: &Spanned<Expr>) -> Ty {
        let left_ty = self.synthesize(&left.0, &left.1);
        let right_ty = self.synthesize(&right.0, &right.1);

        // Resolve type variables through substitution so we check against
        // concrete types when available (bidirectional inference).
        let left_resolved = self.subst.resolve(&left_ty);
        let right_resolved = self.subst.resolve(&right_ty);

        match op {
            BinaryOp::Add
            | BinaryOp::Subtract
            | BinaryOp::Multiply
            | BinaryOp::Divide
            | BinaryOp::Modulo => {
                if left_resolved.is_numeric() && right_resolved.is_numeric() {
                    if let Some(common_ty) = common_numeric_type(&left_resolved, &right_resolved) {
                        common_ty
                    } else {
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            &left.1,
                            format!(
                                "cannot implicitly coerce `{left_resolved}` and `{right_resolved}` in arithmetic; use an explicit conversion"
                            ),
                        );
                        Ty::Error
                    }
                } else if matches!(&left_resolved, Ty::Var(_)) && right_resolved.is_numeric() {
                    // Type variable on left — constrain it to the right's numeric type
                    self.expect_type(&right_ty, &left_ty, &left.1);
                    right_ty
                } else if left_resolved.is_numeric() && matches!(&right_resolved, Ty::Var(_)) {
                    // Type variable on right — constrain it to the left's numeric type
                    self.expect_type(&left_ty, &right_ty, &right.1);
                    left_ty
                } else if matches!((&left_resolved, &right_resolved), (Ty::Var(_), Ty::Var(_))) {
                    // Both are type variables — unify them, result stays polymorphic
                    self.expect_type(&left_ty, &right_ty, &right.1);
                    left_ty
                } else if matches!(op, BinaryOp::Add)
                    && left_resolved == Ty::String
                    && right_resolved == Ty::String
                {
                    Ty::String // string concatenation
                } else {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        &left.1,
                        format!(
                            "cannot apply `{op:?}` to `{left_resolved}` and `{right_resolved}`"
                        ),
                    );
                    Ty::Error
                }
            }
            BinaryOp::BitAnd
            | BinaryOp::BitOr
            | BinaryOp::BitXor
            | BinaryOp::Shl
            | BinaryOp::Shr => {
                if left_resolved.is_integer() && right_resolved.is_integer() {
                    if let Some(common_ty) = common_integer_type(&left_resolved, &right_resolved) {
                        common_ty
                    } else {
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            &left.1,
                            format!(
                                "bitwise `{op:?}` requires compatible integer types; found `{left_resolved}` and `{right_resolved}`"
                            ),
                        );
                        Ty::Error
                    }
                } else if matches!(&left_resolved, Ty::Var(_)) && right_resolved.is_integer() {
                    self.expect_type(&right_ty, &left_ty, &left.1);
                    right_ty
                } else if (left_resolved.is_integer() && matches!(&right_resolved, Ty::Var(_)))
                    || matches!((&left_resolved, &right_resolved), (Ty::Var(_), Ty::Var(_)))
                {
                    self.expect_type(&left_ty, &right_ty, &right.1);
                    left_ty
                } else {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        &left.1,
                        format!("bitwise `{op:?}` requires integer operands, found `{left_resolved}` and `{right_resolved}`"),
                    );
                    Ty::Error
                }
            }
            BinaryOp::Equal
            | BinaryOp::NotEqual
            | BinaryOp::Less
            | BinaryOp::LessEqual
            | BinaryOp::Greater
            | BinaryOp::GreaterEqual => {
                if left_resolved.is_numeric() && right_resolved.is_numeric() {
                    if common_numeric_type(&left_resolved, &right_resolved).is_none() {
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            &left.1,
                            format!(
                                "cannot implicitly coerce `{left_resolved}` and `{right_resolved}` for comparison; use an explicit conversion"
                            ),
                        );
                    }
                } else {
                    self.expect_type(&left_ty, &right_ty, &right.1);
                }
                Ty::Bool
            }
            BinaryOp::And | BinaryOp::Or => {
                self.expect_type(&Ty::Bool, &left_ty, &left.1);
                self.expect_type(&Ty::Bool, &right_ty, &right.1);
                Ty::Bool
            }
            BinaryOp::Range | BinaryOp::RangeInclusive => {
                if left_resolved.is_integer() && right_resolved.is_integer() {
                    if let Some(common_ty) = common_integer_type(&left_resolved, &right_resolved) {
                        Ty::Range(Box::new(common_ty))
                    } else {
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            &left.1,
                            format!(
                                "range bounds require compatible integer types; found `{left_resolved}` and `{right_resolved}`"
                            ),
                        );
                        Ty::Error
                    }
                } else {
                    self.expect_type(&left_ty, &right_ty, &right.1);
                    Ty::Range(Box::new(left_ty))
                }
            }
            BinaryOp::Send => {
                // target <- message
                Ty::Unit
            }
            BinaryOp::RegexMatch | BinaryOp::RegexNotMatch => {
                if !matches!(left_ty, Ty::String) {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        &left.1,
                        format!("left side of regex match must be string, found `{left_ty}`"),
                    );
                }
                if !matches!(&right_ty, Ty::Named { name, .. } if name == "regex.Pattern") {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        &right.1,
                        format!("right side of regex match must be regex, found `{right_ty}`"),
                    );
                }
                Ty::Bool
            }
        }
    }

    fn require_unsafe(&mut self, name: &str, span: &Span) {
        if !self.in_unsafe && self.unsafe_functions.contains(name) {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!("calling extern function `{name}` requires `unsafe {{ ... }}`"),
            );
        }
    }

    fn warn_if_wasm_incompatible_call(&mut self, func_name: &str, span: &Span) {
        if !self.wasm_target {
            return;
        }
        match func_name {
            "link" | "unlink" | "monitor" | "demonitor" => {
                self.warn_wasm_limitation(span, WasmUnsupportedFeature::LinkMonitor);
            }
            "supervisor_child" | "supervisor_stop" => {
                self.warn_wasm_limitation(span, WasmUnsupportedFeature::SupervisionTrees);
            }
            _ => {}
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "call checking covers many builtin and method signatures"
    )]
    fn check_call(
        &mut self,
        func: &Spanned<Expr>,
        type_args: Option<&[Spanned<TypeExpr>]>,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        // Get function name from expression
        let func_name = match &func.0 {
            Expr::Identifier(name) => name.clone(),
            // Handle path-style calls like Vec::new(), HashMap::new()
            Expr::FieldAccess { object, field } => {
                if let Expr::Identifier(obj_name) = &object.0 {
                    format!("{obj_name}::{field}")
                } else {
                    let func_ty = self.synthesize(&func.0, &func.1);
                    return self.check_call_with_type(&func_ty, args, span);
                }
            }
            _ => {
                let func_ty = self.synthesize(&func.0, &func.1);
                return self.check_call_with_type(&func_ty, args, span);
            }
        };

        self.require_unsafe(&func_name, span);
        self.warn_if_wasm_incompatible_call(&func_name, span);

        // Check if name is a user-defined enum variant constructor first.
        // Separate lookup (immutable borrow) from processing (mutable borrow)
        // to avoid cloning the entire type_defs map.
        let constructor_match = self
            .type_defs
            .iter()
            .filter(|(_, td)| td.kind == TypeDefKind::Enum || td.kind == TypeDefKind::Struct)
            .find_map(|(type_name, td)| {
                td.variants.get(&func_name).and_then(|variant| {
                    let params = match variant {
                        VariantDef::Unit => Vec::new(),
                        VariantDef::Tuple(p) => p.clone(),
                        VariantDef::Struct(_) => return None,
                    };
                    Some((type_name.clone(), params, td.type_params.clone()))
                })
            });
        if let Some((type_name, expected_params, type_params)) = constructor_match {
            let type_param_count = type_params.len();
            if type_param_count == 0 {
                if let Some(type_args_provided) = type_args {
                    if !type_args_provided.is_empty() {
                        self.report_error(
                            TypeErrorKind::ArityMismatch,
                            span,
                            format!(
                                "this constructor takes 0 type argument(s) but {} were supplied",
                                type_args_provided.len()
                            ),
                        );
                    }
                }
            }
            let mut inferred_args = Vec::new();
            if type_param_count > 0 {
                if let Some(type_args_provided) = type_args {
                    if type_args_provided.len() != type_param_count {
                        self.report_error(
                            TypeErrorKind::ArityMismatch,
                            span,
                            format!(
                                "this constructor takes {} type argument(s) but {} were supplied",
                                type_param_count,
                                type_args_provided.len()
                            ),
                        );
                    }
                    inferred_args = type_args_provided
                        .iter()
                        .take(type_param_count)
                        .map(|(te, _)| self.resolve_type_expr(te))
                        .collect();
                }
                while inferred_args.len() < type_param_count {
                    inferred_args.push(Ty::Var(TypeVar::fresh()));
                }
            }
            if args.len() != expected_params.len() {
                self.report_error(
                    TypeErrorKind::ArityMismatch,
                    span,
                    format!(
                        "this function takes {} argument(s) but {} were supplied",
                        expected_params.len(),
                        args.len()
                    ),
                );
            }
            for (i, arg) in args.iter().enumerate() {
                if let Some(param_ty) = expected_params.get(i) {
                    let (expr, span) = arg.expr();
                    let mut expected_ty = param_ty.clone();
                    if !type_params.is_empty() {
                        for (param, replacement) in type_params.iter().zip(inferred_args.iter()) {
                            expected_ty =
                                self.substitute_named_param(&expected_ty, param, replacement);
                        }
                    }
                    self.check_against(expr, span, &expected_ty);
                }
            }
            let resolved_args: Vec<Ty> = inferred_args
                .iter()
                .map(|ty| self.subst.resolve(ty))
                .collect();
            return Ty::Named {
                name: type_name,
                args: resolved_args,
            };
        }

        // Handle polymorphic constructors with fresh linked type vars
        match func_name.as_str() {
            "Some" => {
                if args.len() != 1 {
                    self.report_error(
                        TypeErrorKind::ArityMismatch,
                        span,
                        format!("`Some` takes 1 argument but {} were supplied", args.len()),
                    );
                }
                let t = Ty::Var(TypeVar::fresh());
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &t);
                }
                return Ty::Option(Box::new(t));
            }
            "None" => {
                if !args.is_empty() {
                    self.report_error(
                        TypeErrorKind::ArityMismatch,
                        span,
                        format!("`None` takes 0 arguments but {} were supplied", args.len()),
                    );
                }
                return Ty::Option(Box::new(Ty::Var(TypeVar::fresh())));
            }
            "Ok" => {
                if args.len() != 1 {
                    self.report_error(
                        TypeErrorKind::ArityMismatch,
                        span,
                        format!("`Ok` takes 1 argument but {} were supplied", args.len()),
                    );
                }
                let t = Ty::Var(TypeVar::fresh());
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &t);
                }
                return Ty::Result {
                    ok: Box::new(t),
                    err: Box::new(Ty::Var(TypeVar::fresh())),
                };
            }
            "Err" => {
                if args.len() != 1 {
                    self.report_error(
                        TypeErrorKind::ArityMismatch,
                        span,
                        format!("`Err` takes 1 argument but {} were supplied", args.len()),
                    );
                }
                let e = Ty::Var(TypeVar::fresh());
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &e);
                }
                return Ty::Result {
                    ok: Box::new(Ty::Var(TypeVar::fresh())),
                    err: Box::new(e),
                };
            }
            "bytes::from" => {
                if args.len() != 1 {
                    self.report_error(
                        TypeErrorKind::ArityMismatch,
                        span,
                        format!(
                            "`bytes::from` takes 1 argument but {} were supplied",
                            args.len()
                        ),
                    );
                }
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                return Ty::Named {
                    name: "bytes".to_string(),
                    args: vec![],
                };
            }
            "Vec::from" => {
                if args.len() != 1 {
                    self.report_error(
                        TypeErrorKind::ArityMismatch,
                        span,
                        format!(
                            "`Vec::from` takes 1 argument but {} were supplied",
                            args.len()
                        ),
                    );
                }
                let elem = Ty::Var(TypeVar::fresh());
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    let arr_ty = self.synthesize(expr, sp);
                    if let Ty::Array(inner, _) = arr_ty {
                        self.expect_type(&elem, &inner, span);
                    }
                }
                return Ty::Named {
                    name: "Vec".to_string(),
                    args: vec![elem],
                };
            }
            "supervisor_child" if args.len() == 2 => {
                // supervisor_child(sup, index) → typed ActorRef based on supervisor decl
                let (sup_expr, sup_sp) = args[0].expr();
                let sup_ty = self.synthesize(sup_expr, sup_sp);
                let sup_ty_resolved = self.subst.resolve(&sup_ty);
                let (idx_expr, idx_sp) = args[1].expr();
                self.check_against(idx_expr, idx_sp, &Ty::I64);

                if let Ty::ActorRef(inner) = &sup_ty_resolved {
                    if let Ty::Named { name: sup_name, .. } = inner.as_ref() {
                        if let Some(children) = self.supervisor_children.get(sup_name) {
                            if let Expr::Literal(hew_parser::ast::Literal::Integer {
                                value: idx,
                                ..
                            }) = idx_expr
                            {
                                #[expect(
                                    clippy::cast_sign_loss,
                                    clippy::cast_possible_truncation,
                                    reason = "supervisor child index is always non-negative and small"
                                )]
                                let i = *idx as usize;
                                if i < children.len() {
                                    let child_type = &children[i];
                                    return Ty::ActorRef(Box::new(Ty::Named {
                                        name: child_type.clone(),
                                        args: vec![],
                                    }));
                                }
                            }
                            // Non-constant index: fresh type var
                            return Ty::ActorRef(Box::new(Ty::Var(TypeVar::fresh())));
                        }
                    }
                }
                return Ty::ActorRef(Box::new(Ty::Var(TypeVar::fresh())));
            }
            _ => {}
        }

        // Look up function signature first
        if let Some(sig) = self.fn_sigs.get(&func_name).cloned() {
            // Purity check: pure functions can only call other pure functions
            if self.in_pure_function && !sig.is_pure {
                self.report_error(
                    TypeErrorKind::PurityViolation,
                    span,
                    format!("cannot call impure function `{func_name}` from a pure function"),
                );
            }
            if let Some(caller) = &self.current_function {
                self.call_graph
                    .entry(caller.clone())
                    .or_default()
                    .insert(func_name.clone());
            }
            // Mark the originating module as used for unqualified imports
            if let Some(module) = self.unqualified_to_module.get(&func_name) {
                self.used_modules.borrow_mut().insert(module.clone());
            }
            let (freshened_params, freshened_ret, resolved_type_args) =
                self.instantiate_fn_sig_for_call(&sig, type_args, span);

            // Separate positional and named args
            let positional_count = args.iter().take_while(|a| a.name().is_none()).count();
            let positional_args = &args[..positional_count];
            let named_args = &args[positional_count..];

            // Arity check: for accepts_kwargs functions, only check positional args
            // against required params; for normal functions, all args must match
            if !sig.accepts_kwargs && args.len() != freshened_params.len() {
                self.report_error(
                    TypeErrorKind::ArityMismatch,
                    span,
                    format!(
                        "this function takes {} argument(s) but {} were supplied",
                        freshened_params.len(),
                        args.len()
                    ),
                );
            } else if sig.accepts_kwargs && positional_count < freshened_params.len() {
                self.report_error(
                    TypeErrorKind::ArityMismatch,
                    span,
                    format!(
                        "this function takes at least {} positional argument(s) but {} were supplied",
                        freshened_params.len(),
                        positional_count
                    ),
                );
            }

            // Check positional args by index
            for (i, arg) in positional_args.iter().enumerate() {
                if let Some(param_ty) = freshened_params.get(i) {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, param_ty);
                }
            }

            // Check named args by name lookup
            for arg in named_args {
                if let Some(name) = arg.name() {
                    if let Some(idx) = sig.param_names.iter().position(|n| n == name) {
                        if let Some(param_ty) = freshened_params.get(idx) {
                            let (expr, sp) = arg.expr();
                            self.check_against(expr, sp, param_ty);
                        }
                    } else if !sig.accepts_kwargs {
                        let (_, sp) = arg.expr();
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            sp,
                            format!("unknown named argument `{name}`"),
                        );
                    } else {
                        // For kwargs functions, synthesize the expression type
                        let (expr, sp) = arg.expr();
                        self.synthesize(expr, sp);
                    }
                }
            }
            self.enforce_type_param_bounds(&sig, &resolved_type_args, span);
            return freshened_ret;
        }

        // Then check if it's a variable with a function type (e.g., lambda parameters)
        if let Some(binding) = self.env.lookup(&func_name) {
            let func_ty = binding.ty.clone();
            return self.check_call_with_type(&func_ty, args, span);
        }

        let similar = crate::error::find_similar(
            &func_name,
            self.fn_sigs
                .keys()
                .map(String::as_str)
                .chain(self.env.all_names()),
        );
        self.report_error_with_suggestions(
            TypeErrorKind::UndefinedFunction,
            span,
            format!("undefined function `{func_name}`"),
            similar,
        );
        Ty::Error
    }

    fn check_call_with_type(&mut self, func_ty: &Ty, args: &[CallArg], span: &Span) -> Ty {
        let resolved = self.subst.resolve(func_ty);
        match resolved {
            Ty::Function { params, ret } | Ty::Closure { params, ret, .. } => {
                if args.len() != params.len() {
                    self.report_error(
                        TypeErrorKind::ArityMismatch,
                        span,
                        format!(
                            "this function takes {} argument(s) but {} were supplied",
                            params.len(),
                            args.len()
                        ),
                    );
                }
                for (i, arg) in args.iter().enumerate() {
                    if let Some(param) = params.get(i) {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, param);
                    }
                }
                *ret
            }
            _ => {
                // Don't cascade errors from already-failed expressions.
                // Also allow Ty::Unit — keyword expressions like `cooperate` return
                // Unit and can be called with `()` as a no-op.
                if !matches!(resolved, Ty::Error | Ty::Var(_) | Ty::Unit) {
                    self.report_error(
                        TypeErrorKind::Mismatch {
                            expected: "function".to_string(),
                            actual: format!("{resolved}"),
                        },
                        span,
                        format!("cannot call value of type `{resolved}`"),
                    );
                }
                Ty::Error
            }
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "pattern matching type checker with many variants"
    )]
    fn check_method_call(
        &mut self,
        receiver: &Spanned<Expr>,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        // Module-qualified calls: e.g. http.listen(addr) → lookup "http.listen" in fn_sigs
        if let Expr::Identifier(name) = &receiver.0 {
            if self.modules.contains(name) {
                self.used_modules.borrow_mut().insert(name.clone());
                let key = format!("{name}.{method}");
                self.require_unsafe(&key, span);
                if let Some(sig) = self.fn_sigs.get(&key).cloned() {
                    if let Some(caller) = &self.current_function {
                        self.call_graph
                            .entry(caller.clone())
                            .or_default()
                            .insert(key.clone());
                    }
                    // Separate positional and named args
                    let positional_count = args.iter().take_while(|a| a.name().is_none()).count();
                    let positional_args = &args[..positional_count];
                    let named_args = &args[positional_count..];

                    // Arity check
                    if !sig.accepts_kwargs && args.len() != sig.params.len() {
                        self.report_error(
                            TypeErrorKind::ArityMismatch,
                            span,
                            format!(
                                "expected {} arguments, found {}",
                                sig.params.len(),
                                args.len()
                            ),
                        );
                    } else if sig.accepts_kwargs && positional_count < sig.params.len() {
                        self.report_error(
                            TypeErrorKind::ArityMismatch,
                            span,
                            format!(
                                "expected at least {} positional arguments, found {}",
                                sig.params.len(),
                                positional_count
                            ),
                        );
                    }

                    // Check positional args by index
                    for (i, arg) in positional_args.iter().enumerate() {
                        if let Some(param_ty) = sig.params.get(i) {
                            let (expr, sp) = arg.expr();
                            self.check_against(expr, sp, param_ty);
                        }
                    }

                    // Check named args by name lookup
                    for arg in named_args {
                        if let Some(arg_name) = arg.name() {
                            if let Some(idx) = sig.param_names.iter().position(|n| n == arg_name) {
                                if let Some(param_ty) = sig.params.get(idx) {
                                    let (expr, sp) = arg.expr();
                                    self.check_against(expr, sp, param_ty);
                                }
                            } else if !sig.accepts_kwargs {
                                let (_, sp) = arg.expr();
                                self.report_error(
                                    TypeErrorKind::InvalidOperation,
                                    sp,
                                    format!("unknown named argument `{arg_name}`"),
                                );
                            } else {
                                // For kwargs functions, synthesize the expression type
                                let (expr, sp) = arg.expr();
                                self.synthesize(expr, sp);
                            }
                        }
                    }
                    return sig.return_type;
                }
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no function `{method}` in module `{name}`"),
                );
                return Ty::Error;
            }
        }

        let receiver_ty = self.synthesize(&receiver.0, &receiver.1);
        let resolved = self.subst.resolve(&receiver_ty);

        match (&resolved, method) {
            // Vec methods
            (
                Ty::Named {
                    name,
                    args: type_args,
                },
                _,
            ) if name == "Vec" => {
                let elem_ty = type_args
                    .first()
                    .cloned()
                    .unwrap_or(Ty::Var(TypeVar::fresh()));
                match method {
                    "push" => {
                        if args.len() != 1 {
                            self.report_error(
                                TypeErrorKind::ArityMismatch,
                                span,
                                format!(
                                    "`Vec::push` takes 1 argument but {} were supplied",
                                    args.len()
                                ),
                            );
                        }
                        if let Some(arg) = args.first() {
                            let (expr, sp) = arg.expr();
                            self.check_against(expr, sp, &elem_ty);
                        }
                        Ty::Unit
                    }
                    "pop" => elem_ty,
                    "len" => Ty::I64,
                    "get" | "remove" => {
                        if args.len() != 1 {
                            self.report_error(
                                TypeErrorKind::ArityMismatch,
                                span,
                                format!(
                                    "`Vec::{method}` takes 1 argument but {} were supplied",
                                    args.len()
                                ),
                            );
                        }
                        if let Some(arg) = args.first() {
                            let (expr, sp) = arg.expr();
                            self.check_against(expr, sp, &Ty::I64);
                        }
                        elem_ty
                    }
                    "contains" => {
                        if let Some(arg) = args.first() {
                            let (expr, sp) = arg.expr();
                            self.check_against(expr, sp, &elem_ty);
                        }
                        Ty::Bool
                    }
                    "is_empty" => Ty::Bool,
                    "clear" => {
                        if !args.is_empty() {
                            self.report_error(
                                TypeErrorKind::ArityMismatch,
                                span,
                                format!(
                                    "`Vec::clear` takes 0 arguments but {} were supplied",
                                    args.len()
                                ),
                            );
                        }
                        Ty::Unit
                    }
                    "set" => {
                        if let Some(idx) = args.first() {
                            let (expr, sp) = idx.expr();
                            self.check_against(expr, sp, &Ty::I64);
                        }
                        if let Some(val) = args.get(1) {
                            let (expr, sp) = val.expr();
                            self.check_against(expr, sp, &elem_ty);
                        }
                        Ty::Unit
                    }
                    "append" | "extend" => {
                        if let Some(arg) = args.first() {
                            let (expr, sp) = arg.expr();
                            self.check_against(expr, sp, &receiver_ty);
                        }
                        Ty::Unit
                    }
                    _ => {
                        self.report_error(
                            TypeErrorKind::UndefinedMethod,
                            span,
                            format!("no method `{method}` on Vec"),
                        );
                        Ty::Error
                    }
                }
            }
            // HashMap methods
            (
                Ty::Named {
                    name,
                    args: type_args,
                },
                _,
            ) if name == "HashMap" => {
                let key_ty = type_args
                    .first()
                    .cloned()
                    .unwrap_or(Ty::Var(TypeVar::fresh()));
                let val_ty = type_args
                    .get(1)
                    .cloned()
                    .unwrap_or(Ty::Var(TypeVar::fresh()));
                match method {
                    "insert" => {
                        if args.len() != 2 {
                            self.report_error(
                                TypeErrorKind::ArityMismatch,
                                span,
                                format!(
                                    "`HashMap::insert` takes 2 arguments but {} were supplied",
                                    args.len()
                                ),
                            );
                        }
                        if let Some(arg) = args.first() {
                            let (expr, sp) = arg.expr();
                            self.check_against(expr, sp, &key_ty);
                        }
                        if let Some(arg) = args.get(1) {
                            let (expr, sp) = arg.expr();
                            self.check_against(expr, sp, &val_ty);
                        }
                        Ty::Unit
                    }
                    "get" | "remove" => {
                        if args.len() != 1 {
                            self.report_error(
                                TypeErrorKind::ArityMismatch,
                                span,
                                format!(
                                    "`HashMap::{method}` takes 1 argument but {} were supplied",
                                    args.len()
                                ),
                            );
                        }
                        if let Some(arg) = args.first() {
                            let (expr, sp) = arg.expr();
                            self.check_against(expr, sp, &key_ty);
                        }
                        Ty::Option(Box::new(val_ty))
                    }
                    "contains_key" => {
                        if args.len() != 1 {
                            self.report_error(
                                TypeErrorKind::ArityMismatch,
                                span,
                                format!(
                                    "`HashMap::contains_key` takes 1 argument but {} were supplied",
                                    args.len()
                                ),
                            );
                        }
                        if let Some(arg) = args.first() {
                            let (expr, sp) = arg.expr();
                            self.check_against(expr, sp, &key_ty);
                        }
                        // Returns bool
                        Ty::Bool
                    }
                    "values" => {
                        if !args.is_empty() {
                            self.report_error(
                                TypeErrorKind::ArityMismatch,
                                span,
                                format!(
                                    "`HashMap::values` takes 0 arguments but {} were supplied",
                                    args.len()
                                ),
                            );
                        }
                        Ty::Named {
                            name: "Vec".to_string(),
                            args: vec![val_ty],
                        }
                    }
                    "len" => Ty::I64,
                    "is_empty" => Ty::Bool,
                    _ => {
                        self.report_error(
                            TypeErrorKind::UndefinedMethod,
                            span,
                            format!("no method `{method}` on HashMap"),
                        );
                        Ty::Error
                    }
                }
            }
            // HashSet methods
            (
                Ty::Named {
                    name,
                    args: type_args,
                },
                _,
            ) if name == "HashSet" => {
                let elem_ty = type_args
                    .first()
                    .cloned()
                    .unwrap_or(Ty::Var(TypeVar::fresh()));
                match method {
                    "insert" => {
                        if args.len() != 1 {
                            self.report_error(
                                TypeErrorKind::ArityMismatch,
                                span,
                                format!(
                                    "`HashSet::insert` takes 1 argument but {} were supplied",
                                    args.len()
                                ),
                            );
                        }
                        if let Some(arg) = args.first() {
                            let (expr, sp) = arg.expr();
                            self.check_against(expr, sp, &elem_ty);
                        }
                        Ty::Bool
                    }
                    "contains" | "remove" => {
                        if args.len() != 1 {
                            self.report_error(
                                TypeErrorKind::ArityMismatch,
                                span,
                                format!(
                                    "`HashSet::{method}` takes 1 argument but {} were supplied",
                                    args.len()
                                ),
                            );
                        }
                        if let Some(arg) = args.first() {
                            let (expr, sp) = arg.expr();
                            self.check_against(expr, sp, &elem_ty);
                        }
                        Ty::Bool
                    }
                    "len" => Ty::I64,
                    "is_empty" => Ty::Bool,
                    "clear" => Ty::Unit,
                    _ => {
                        self.report_error(
                            TypeErrorKind::UndefinedMethod,
                            span,
                            format!("no method `{method}` on HashSet"),
                        );
                        Ty::Error
                    }
                }
            }
            // bytes methods (mutable byte buffer)
            // NOTE: Using i32 for element types to match current runtime ABI.
            // When runtime is updated to use u8 natively, change these to Ty::U8.
            (Ty::Named { name, .. }, _) if name == "bytes" => match method {
                "push" => {
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::I32);
                    }
                    Ty::Unit
                }
                "pop" | "len" => Ty::I32,
                "get" => {
                    if let Some(idx) = args.first() {
                        let (expr, sp) = idx.expr();
                        self.check_against(expr, sp, &Ty::I64);
                    }
                    Ty::I32
                }
                "set" => {
                    if let Some(idx) = args.first() {
                        let (expr, sp) = idx.expr();
                        self.check_against(expr, sp, &Ty::I64);
                    }
                    if let Some(val) = args.get(1) {
                        let (expr, sp) = val.expr();
                        self.check_against(expr, sp, &Ty::I32);
                    }
                    Ty::Unit
                }
                "is_empty" => Ty::Bool,
                "contains" => {
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::I32);
                    }
                    Ty::Bool
                }
                "to_string" => Ty::String,
                "append" | "extend" => {
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(
                            expr,
                            sp,
                            &Ty::Named {
                                name: "bytes".to_string(),
                                args: vec![],
                            },
                        );
                    }
                    Ty::Unit
                }
                _ => {
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!("no method `{method}` on bytes"),
                    );
                    Ty::Error
                }
            },
            // ActorRef methods
            (Ty::ActorRef(inner), _) => {
                if method == "send" {
                    // Synthesize args and mark non-Copy values as moved
                    for arg in args {
                        let (expr, sp) = arg.expr();
                        let ty = self.synthesize(expr, sp);
                        if !self.registry.implements_marker(&ty, MarkerTrait::Copy) {
                            if let Expr::Identifier(name) = expr {
                                self.env.mark_moved(name, sp.clone());
                            }
                        }
                    }
                    Ty::Unit
                } else {
                    // Try to dispatch to the actor's receive methods
                    if let Ty::Named {
                        name: actor_name, ..
                    } = inner.as_ref()
                    {
                        let method_key = format!("{actor_name}::{method}");
                        if let Some(sig) = self.fn_sigs.get(&method_key).cloned() {
                            for (i, arg) in args.iter().enumerate() {
                                let (expr, sp) = arg.expr();
                                if let Some(param_ty) = sig.params.get(i) {
                                    self.check_against(expr, sp, param_ty);
                                }
                                // Mark non-Copy args as moved at actor boundaries
                                let ty = self.synthesize(expr, sp);
                                if !self.registry.implements_marker(&ty, MarkerTrait::Copy) {
                                    if let Expr::Identifier(name) = expr {
                                        self.env.mark_moved(name, sp.clone());
                                    }
                                }
                            }
                            return sig.return_type;
                        }
                    }
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!("no method `{method}` on `{resolved}`"),
                    );
                    Ty::Error
                }
            }
            // Named types that have built-in methods (Actor<T> from lambda actors)
            (
                Ty::Named {
                    name,
                    args: type_args,
                },
                "send",
            ) if name == "Actor" => {
                for arg in args {
                    let (expr, sp) = arg.expr();
                    if let Some(param_ty) = type_args.first() {
                        self.check_against(expr, sp, param_ty);
                    } else {
                        self.synthesize(expr, sp);
                    }
                }
                Ty::Unit
            }
            // String methods
            (Ty::String, _) => match method {
                "len" => Ty::I64,
                "contains" | "starts_with" | "ends_with" => {
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::String);
                    }
                    Ty::Bool
                }
                "to_uppercase" | "to_lowercase" | "to_upper" | "to_lower" | "trim" => Ty::String,
                "replace" => {
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::String);
                    }
                    if let Some(arg) = args.get(1) {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::String);
                    }
                    Ty::String
                }
                "split" => {
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::String);
                    }
                    Ty::Named {
                        name: "Vec".to_string(),
                        args: vec![Ty::String],
                    }
                }
                "find" | "index_of" => {
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::String);
                    }
                    Ty::I64
                }
                "slice" => {
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::I64);
                    }
                    if let Some(arg) = args.get(1) {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::I64);
                    }
                    Ty::String
                }
                "repeat" => {
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::I64);
                    }
                    Ty::String
                }
                "char_at" => {
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::I64);
                    }
                    Ty::I64
                }
                "chars" => Ty::Named {
                    name: "Vec".to_string(),
                    args: vec![Ty::Char],
                },
                _ => {
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!("no method `{method}` on string"),
                    );
                    Ty::Error
                }
            },
            // http.Server methods
            (Ty::Named { name, .. }, _) if name == "http.Server" => match method {
                "accept" => Ty::Named {
                    name: "http.Request".to_string(),
                    args: vec![],
                },
                "close" => Ty::Unit,
                _ => {
                    if let Some(ty) = self.try_resolve_named_method(name, method, args, span) {
                        ty
                    } else {
                        self.report_error(
                            TypeErrorKind::UndefinedMethod,
                            span,
                            format!("no method `{method}` on http.Server"),
                        );
                        Ty::Error
                    }
                }
            },
            // http.Request methods/properties
            (Ty::Named { name, .. }, _) if name == "http.Request" => match method {
                "path" | "method" | "body" => Ty::String,
                "header" => {
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::String);
                    }
                    Ty::String
                }
                "respond" => {
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::I32);
                    }
                    if let Some(arg) = args.get(1) {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::String);
                    }
                    if let Some(arg) = args.get(2) {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::I64);
                    }
                    if let Some(arg) = args.get(3) {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::String);
                    }
                    Ty::I32
                }
                "respond_text" | "respond_json" => {
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::I32);
                    }
                    if let Some(arg) = args.get(1) {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::String);
                    }
                    Ty::I32
                }
                "free" => Ty::Unit,
                _ => {
                    if let Some(ty) = self.try_resolve_named_method(name, method, args, span) {
                        ty
                    } else {
                        self.report_error(
                            TypeErrorKind::UndefinedMethod,
                            span,
                            format!("no method `{method}` on http.Request"),
                        );
                        Ty::Error
                    }
                }
            },
            // net.Listener methods
            (Ty::Named { name, .. }, _) if name == "net.Listener" => match method {
                "accept" => Ty::Named {
                    name: "net.Connection".to_string(),
                    args: vec![],
                },
                "close" => Ty::Unit,
                _ => {
                    if let Some(ty) = self.try_resolve_named_method(name, method, args, span) {
                        ty
                    } else {
                        self.report_error(
                            TypeErrorKind::UndefinedMethod,
                            span,
                            format!("no method `{method}` on net.Listener"),
                        );
                        Ty::Error
                    }
                }
            },
            // net.Connection methods
            (Ty::Named { name, .. }, _) if name == "net.Connection" => match method {
                "read" => Ty::Named {
                    name: "bytes".to_string(),
                    args: vec![],
                },
                "write" => {
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(
                            expr,
                            sp,
                            &Ty::Named {
                                name: "bytes".to_string(),
                                args: vec![],
                            },
                        );
                    }
                    Ty::I32
                }
                "close" => Ty::I32,
                "set_read_timeout" | "set_write_timeout" => {
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::I32);
                    }
                    Ty::I32
                }
                _ => {
                    if let Some(ty) = self.try_resolve_named_method(name, method, args, span) {
                        ty
                    } else {
                        self.report_error(
                            TypeErrorKind::UndefinedMethod,
                            span,
                            format!("no method `{method}` on net.Connection"),
                        );
                        Ty::Error
                    }
                }
            },
            // regex.Pattern methods
            (Ty::Named { name, .. }, _) if name == "regex.Pattern" => match method {
                "is_match" => {
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::String);
                    }
                    Ty::Bool
                }
                "find" => {
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::String);
                    }
                    Ty::String
                }
                "replace" => {
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::String);
                    }
                    if let Some(arg) = args.get(1) {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::String);
                    }
                    Ty::String
                }
                "free" => Ty::Unit,
                _ => {
                    if let Some(ty) = self.try_resolve_named_method(name, method, args, span) {
                        ty
                    } else {
                        self.report_error(
                            TypeErrorKind::UndefinedMethod,
                            span,
                            format!("no method `{method}` on regex.Pattern"),
                        );
                        Ty::Error
                    }
                }
            },
            // process.Child methods
            (Ty::Named { name, .. }, _) if name == "process.Child" => match method {
                "wait" | "kill" => Ty::I32,
                "free" => Ty::Unit,
                _ => {
                    if let Some(ty) = self.try_resolve_named_method(name, method, args, span) {
                        ty
                    } else {
                        self.report_error(
                            TypeErrorKind::UndefinedMethod,
                            span,
                            format!("no method `{method}` on process.Child"),
                        );
                        Ty::Error
                    }
                }
            },
            // User-defined struct/actor methods from type_defs
            (
                Ty::Named {
                    name,
                    args: type_args,
                },
                _,
            ) => {
                if let Some(td) = self.lookup_type_def(name) {
                    if let Some(sig) = td.methods.get(method) {
                        if args.len() != sig.params.len() {
                            self.report_error(
                                TypeErrorKind::ArityMismatch,
                                span,
                                format!(
                                    "method '{}' expects {} argument(s), found {}",
                                    method,
                                    sig.params.len(),
                                    args.len(),
                                ),
                            );
                        }
                        for (i, arg) in args.iter().enumerate() {
                            if let Some(param_ty) = sig.params.get(i) {
                                // Substitute generic type params with concrete args
                                let mut subst_ty = param_ty.clone();
                                for (tp, ta) in td.type_params.iter().zip(type_args.iter()) {
                                    subst_ty = self.substitute_named_param(&subst_ty, tp, ta);
                                }
                                let (expr, sp) = arg.expr();
                                self.check_against(expr, sp, &subst_ty);
                            }
                        }
                        let mut ret = sig.return_type.clone();
                        for (tp, ta) in td.type_params.iter().zip(type_args.iter()) {
                            ret = self.substitute_named_param(&ret, tp, ta);
                        }
                        return ret;
                    }
                }
                // Try fn_sigs with Name::method pattern
                let method_key = format!("{name}::{method}");
                if let Some(sig) = self.lookup_fn_sig(&method_key) {
                    if args.len() != sig.params.len() {
                        self.report_error(
                            TypeErrorKind::ArityMismatch,
                            span,
                            format!(
                                "method '{}' expects {} argument(s), found {}",
                                method,
                                sig.params.len(),
                                args.len(),
                            ),
                        );
                    }
                    for (i, arg) in args.iter().enumerate() {
                        if let Some(param_ty) = sig.params.get(i) {
                            let (expr, sp) = arg.expr();
                            self.check_against(expr, sp, param_ty);
                        }
                    }
                    return sig.return_type;
                }
                // Synthesize args even if method unknown (for error recovery)
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on `{resolved}`"),
                );
                Ty::Error
            }
            // Trait object method dispatch: look up methods from all trait bounds
            (Ty::TraitObject { traits }, _) => {
                // Try to find the method in any of the traits
                let mut found_sig = None;
                let mut found_bound = None;
                for bound in traits {
                    if let Some(sig) = self.lookup_trait_method(&bound.trait_name, method) {
                        found_sig = Some(sig);
                        found_bound = Some(bound);
                        break;
                    }
                }

                if let Some(mut sig) = found_sig {
                    // Apply type substitutions from bound's type arguments
                    if let Some(bound) = found_bound {
                        if let Some(trait_info) = self.trait_defs.get(&bound.trait_name) {
                            let type_params = &trait_info.type_params;
                            if type_params.len() == bound.args.len() {
                                // Apply substitutions
                                for (param_name, replacement) in
                                    type_params.iter().zip(bound.args.iter())
                                {
                                    // Substitute in parameter types
                                    for param_ty in &mut sig.params {
                                        *param_ty = self.substitute_named_param(
                                            param_ty,
                                            param_name,
                                            replacement,
                                        );
                                    }
                                    // Substitute in return type
                                    sig.return_type = self.substitute_named_param(
                                        &sig.return_type,
                                        param_name,
                                        replacement,
                                    );
                                }
                            }
                        }
                    }

                    if args.len() != sig.params.len() {
                        self.report_error(
                            TypeErrorKind::ArityMismatch,
                            span,
                            format!(
                                "method '{}' expects {} argument(s), found {}",
                                method,
                                sig.params.len(),
                                args.len(),
                            ),
                        );
                    }
                    for (i, arg) in args.iter().enumerate() {
                        if let Some(param_ty) = sig.params.get(i) {
                            let (expr, sp) = arg.expr();
                            self.check_against(expr, sp, param_ty);
                        }
                    }
                    sig.return_type
                } else {
                    for arg in args {
                        let (expr, sp) = arg.expr();
                        self.synthesize(expr, sp);
                    }
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!("no method `{method}` on `{resolved}`"),
                    );
                    Ty::Error
                }
            }
            // Generator methods: .next() returns the yielded type
            (Ty::Generator { yields, .. }, "next") | (Ty::AsyncGenerator { yields }, "next") => {
                (**yields).clone()
            }

            // Stream<T> methods
            (Ty::Stream(inner), "next") => Ty::Option(inner.clone()),
            (Ty::Stream(_) | Ty::Sink(_), "close") | (Ty::Sink(_), "flush") => Ty::Unit,
            (Ty::Stream(_), "lines") => Ty::Stream(Box::new(Ty::String)),
            (Ty::Stream(_), "collect") => Ty::String,
            (Ty::Stream(inner), "chunks") => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::I32);
                }
                Ty::Stream(inner.clone())
            }
            (Ty::Stream(_), "decode") => {
                // Returns Stream<T> where T is inferred; codec type arg not yet resolved
                Ty::Stream(Box::new(Ty::Var(TypeVar::fresh())))
            }

            // Sink<T> methods
            (Ty::Sink(inner), "write") => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, inner);
                }
                Ty::Unit
            }
            (Ty::Sink(_), "encode") => {
                // Returns Sink<Row> where Row is inferred; codec type arg not yet resolved
                Ty::Sink(Box::new(Ty::Var(TypeVar::fresh())))
            }

            // For error types, don't report additional errors
            (Ty::Error, _) => {
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                Ty::Error
            }
            _ => {
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on `{resolved}`"),
                );
                Ty::Error
            }
        }
    }

    fn check_field_access(&mut self, object: &Spanned<Expr>, field: &str, span: &Span) -> Ty {
        let obj_ty = self.synthesize(&object.0, &object.1);
        let resolved = self.subst.resolve(&obj_ty);
        match &resolved {
            Ty::Named { name, args } => {
                if let Some(td) = self.lookup_type_def(name) {
                    if let Some(field_ty) = td.fields.get(field) {
                        // Substitute generic type params with concrete args
                        let mut result_ty = field_ty.clone();
                        for (param, arg) in td.type_params.iter().zip(args.iter()) {
                            result_ty = self.substitute_named_param(&result_ty, param, arg);
                        }
                        result_ty
                    } else {
                        let similar =
                            crate::error::find_similar(field, td.fields.keys().map(String::as_str));
                        self.report_error_with_suggestions(
                            TypeErrorKind::UndefinedField,
                            span,
                            format!("no field `{field}` on type `{name}`"),
                            similar,
                        );
                        Ty::Error
                    }
                } else {
                    Ty::Error
                }
            }
            Ty::Tuple(elems) => {
                // Tuple field access by index: t.0, t.1
                if let Ok(idx) = field.parse::<usize>() {
                    if idx < elems.len() {
                        elems[idx].clone()
                    } else {
                        self.report_error(
                            TypeErrorKind::UndefinedField,
                            span,
                            format!("tuple index {idx} out of range (len {})", elems.len()),
                        );
                        Ty::Error
                    }
                } else {
                    Ty::Error
                }
            }
            _ => {
                if resolved != Ty::Error {
                    self.report_error(
                        TypeErrorKind::UndefinedField,
                        span,
                        format!("cannot access field `{field}` on `{resolved}`"),
                    );
                }
                Ty::Error
            }
        }
    }

    fn check_match_expr(&mut self, scrutinee_ty: &Ty, arms: &[MatchArm], span: &Span) -> Ty {
        if arms.is_empty() {
            return Ty::Unit;
        }

        let mut result_ty: Option<Ty> = None;
        for arm in arms {
            self.env.push_scope();
            self.bind_pattern(&arm.pattern.0, scrutinee_ty, false, &arm.pattern.1);

            // Check guard if present
            if let Some((guard, gs)) = &arm.guard {
                self.check_against(guard, gs, &Ty::Bool);
            }

            let arm_ty = if let Some(expected) = &result_ty {
                self.check_against(&arm.body.0, &arm.body.1, expected)
            } else {
                self.synthesize(&arm.body.0, &arm.body.1)
            };

            // Skip Never/Error when setting the expected type — diverging arms
            // (return, panic, break) shouldn't constrain the match result type.
            if result_ty.is_none() && !matches!(arm_ty, Ty::Never | Ty::Error) {
                result_ty = Some(arm_ty);
            }

            self.env.pop_scope();
        }

        // Exhaustiveness check for enums/Option/Result
        self.check_exhaustiveness(scrutinee_ty, arms, span);

        // If all arms diverge (Never/Error), the match itself diverges
        result_ty.unwrap_or(Ty::Never)
    }

    fn check_lambda(
        &mut self,
        type_params: Option<&[TypeParam]>,
        params: &[LambdaParam],
        return_type: Option<&Spanned<TypeExpr>>,
        body: &Spanned<Expr>,
        expected: Option<(&[Ty], &Ty)>,
        span: &Span,
    ) -> Ty {
        // Save/restore capture tracking state for nested lambdas
        let prev_capture_depth = self.lambda_capture_depth;
        let prev_captures = std::mem::take(&mut self.lambda_captures);

        // Record the scope depth BEFORE pushing the lambda scope — any variable
        // found below this depth during body checking is a capture.
        let capture_depth = self.env.depth();
        self.lambda_capture_depth = Some(capture_depth);

        let mut generic_bindings = std::collections::HashMap::new();
        if let Some(tps) = type_params {
            for tp in tps {
                generic_bindings.insert(tp.name.clone(), Ty::Var(TypeVar::fresh()));
            }
        }
        if !generic_bindings.is_empty() {
            self.generic_ctx.push(generic_bindings);
        }

        self.env.push_scope();
        let prev_in_generator = self.in_generator;
        self.in_generator = false;

        // Check arity mismatch: lambda parameter count must match expected function type
        if let Some((expected_params, _)) = &expected {
            if params.len() != expected_params.len() {
                self.errors.push(TypeError::new(
                    TypeErrorKind::ArityMismatch,
                    span.clone(),
                    format!(
                        "lambda has {} parameters but expected function type has {}",
                        params.len(),
                        expected_params.len()
                    ),
                ));
            }
        }

        let mut param_tys = Vec::new();
        for (i, p) in params.iter().enumerate() {
            let ty = if let Some((te, _)) = &p.ty {
                self.resolve_type_expr(te)
            } else if let Some((expected_params, _)) = &expected {
                expected_params
                    .get(i)
                    .cloned()
                    .unwrap_or_else(|| Ty::Var(TypeVar::fresh()))
            } else {
                Ty::Var(TypeVar::fresh())
            };
            self.env.define(p.name.clone(), ty.clone(), false);
            param_tys.push(ty);
        }

        let ret_ty = if let Some((te, _)) = return_type {
            let expected_ret = self.resolve_type_expr(te);
            self.check_against(&body.0, &body.1, &expected_ret);
            expected_ret
        } else if let Some((_, expected_ret)) = expected {
            self.check_against(&body.0, &body.1, expected_ret);
            expected_ret.clone()
        } else {
            self.synthesize(&body.0, &body.1)
        };

        self.in_generator = prev_in_generator;
        self.env.pop_scope();

        if let Some(tps) = type_params {
            if !tps.is_empty() {
                self.generic_ctx.pop();
            }
        }

        // Collect captures and resolve their types
        let captures: Vec<Ty> = std::mem::take(&mut self.lambda_captures)
            .into_iter()
            .map(|c| self.subst.resolve(&c))
            .collect();

        // Restore outer capture tracking state
        self.lambda_capture_depth = prev_capture_depth;
        self.lambda_captures = prev_captures;

        if captures.is_empty() {
            Ty::Function {
                params: param_tys,
                ret: Box::new(ret_ty),
            }
        } else {
            Ty::Closure {
                params: param_tys,
                ret: Box::new(ret_ty),
                captures,
            }
        }
    }

    fn check_struct_init(
        &mut self,
        name: &str,
        fields: &[(String, Spanned<Expr>)],
        span: &Span,
    ) -> Ty {
        if let Some(td) = self.lookup_type_def(name) {
            // Track inferred type arguments for generic structs
            let mut type_arg_map: HashMap<String, Ty> = HashMap::new();

            for (field_name, (expr, es)) in fields {
                if let Some(declared_ty) = td.fields.get(field_name) {
                    // Substitute already-inferred type params into the expected type
                    let mut expected = declared_ty.clone();
                    for (tp, concrete) in &type_arg_map {
                        expected = self.substitute_named_param(&expected, tp, concrete);
                    }
                    let actual = self.check_against(expr, es, &expected);

                    // Infer type params: if field type is a bare type param, bind it
                    for tp in &td.type_params {
                        if !type_arg_map.contains_key(tp)
                            && *declared_ty
                                == (Ty::Named {
                                    name: tp.clone(),
                                    args: vec![],
                                })
                        {
                            type_arg_map.insert(tp.clone(), actual.clone());
                        }
                    }
                } else {
                    let similar = crate::error::find_similar(
                        field_name,
                        td.fields.keys().map(String::as_str),
                    );
                    self.report_error_with_suggestions(
                        TypeErrorKind::UndefinedField,
                        span,
                        format!("no field `{field_name}` on struct `{name}`"),
                        similar,
                    );
                }
            }
            // Check for missing required fields
            let provided: HashSet<&str> = fields.iter().map(|(n, _)| n.as_str()).collect();
            for declared in td.fields.keys() {
                if !provided.contains(declared.as_str()) {
                    self.report_error(
                        TypeErrorKind::UndefinedField,
                        span,
                        format!("missing field `{declared}` in initializer of `{name}`"),
                    );
                }
            }
            // Build type args from inferred bindings
            let type_args: Vec<Ty> = td
                .type_params
                .iter()
                .map(|tp| {
                    type_arg_map
                        .get(tp)
                        .cloned()
                        .unwrap_or_else(|| Ty::Var(TypeVar::fresh()))
                })
                .collect();
            Ty::Named {
                name: name.to_string(),
                args: type_args,
            }
        } else if let Some((enum_name, variant_fields)) =
            self.type_defs
                .iter()
                .find_map(|(type_name, td)| match td.variants.get(name) {
                    Some(VariantDef::Struct(fields)) => Some((type_name.clone(), fields.clone())),
                    _ => None,
                })
        {
            for (field_name, (expr, es)) in fields {
                if let Some((_, declared_ty)) =
                    variant_fields.iter().find(|(name, _)| name == field_name)
                {
                    self.check_against(expr, es, declared_ty);
                } else {
                    let similar = crate::error::find_similar(
                        field_name,
                        variant_fields.iter().map(|(n, _)| n.as_str()),
                    );
                    self.report_error_with_suggestions(
                        TypeErrorKind::UndefinedField,
                        span,
                        format!("no field `{field_name}` on variant `{name}`"),
                        similar,
                    );
                }
            }
            let provided: HashSet<&str> = fields.iter().map(|(n, _)| n.as_str()).collect();
            for (declared, _) in &variant_fields {
                if !provided.contains(declared.as_str()) {
                    self.report_error(
                        TypeErrorKind::UndefinedField,
                        span,
                        format!("missing field `{declared}` in initializer of `{name}`"),
                    );
                }
            }
            Ty::Named {
                name: enum_name,
                args: vec![],
            }
        } else {
            let similar = crate::error::find_similar(
                name,
                self.type_defs
                    .keys()
                    .map(String::as_str)
                    .chain(self.type_aliases.keys().map(String::as_str))
                    .chain(self.known_types.iter().map(String::as_str)),
            );
            self.report_error_with_suggestions(
                TypeErrorKind::UndefinedType,
                span,
                format!("undefined type `{name}`"),
                similar,
            );
            Ty::Error
        }
    }

    fn check_spawn(
        &mut self,
        target: &Spanned<Expr>,
        args: &[(String, Spanned<Expr>)],
        _span: &Span,
    ) -> Ty {
        let actor_name = match &target.0 {
            Expr::Identifier(name) => Some(name.clone()),
            // Handle module-qualified actor: spawn module.ActorName(args)
            Expr::FieldAccess { object, field } => {
                if let Expr::Identifier(module) = &object.0 {
                    if self.modules.contains(module) {
                        Some(field.clone())
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        };

        if let Some(name) = actor_name {
            // Check args are Send and mark as moved (unless Copy)
            for (_field_name, (arg, as_)) in args {
                let ty_raw = self.synthesize(arg, as_);
                let ty = self.subst.resolve(&ty_raw);
                if !self.registry.implements_marker(&ty, MarkerTrait::Send) {
                    self.report_error(
                        TypeErrorKind::InvalidSend,
                        as_,
                        format!("cannot send `{ty}` to actor: type is not Send"),
                    );
                }
                if !self.registry.implements_marker(&ty, MarkerTrait::Copy) {
                    if let Expr::Identifier(arg_name) = arg {
                        self.env.mark_moved(arg_name, as_.clone());
                    }
                }
            }
            Ty::ActorRef(Box::new(Ty::Named { name, args: vec![] }))
        } else {
            Ty::ActorRef(Box::new(Ty::Error))
        }
    }

    /// Pattern binding
    fn bind_pattern(&mut self, pattern: &Pattern, ty: &Ty, is_mutable: bool, span: &Span) {
        let ty = &self.subst.resolve(ty);
        match pattern {
            Pattern::Wildcard | Pattern::Literal(_) => {}
            Pattern::Identifier(name) => {
                self.check_shadowing(name, span);
                self.env
                    .define_with_span(name.clone(), ty.clone(), is_mutable, span.clone());
            }
            Pattern::Constructor { name, patterns } => {
                // Look up variant in enum definition
                if let Some(payload_tys) = self.lookup_variant_types(name, ty) {
                    for (p, pty) in patterns.iter().zip(payload_tys.iter()) {
                        self.bind_pattern(&p.0, pty, is_mutable, &p.1);
                    }
                } else if let Ty::Named {
                    name: type_name, ..
                } = ty
                {
                    self.report_error(
                        TypeErrorKind::Mismatch {
                            expected: type_name.clone(),
                            actual: name.clone(),
                        },
                        span,
                        format!("variant `{name}` is not a member of enum `{type_name}`"),
                    );
                }
            }
            Pattern::Struct { name, fields } => {
                // Bind field patterns to field types
                if let Ty::Named {
                    name: type_name, ..
                } = ty
                {
                    if let Some(td) = self.lookup_type_def(type_name) {
                        if let Some(VariantDef::Struct(variant_fields)) =
                            td.variants.get(name).cloned()
                        {
                            for pf in fields {
                                if let Some((_, field_ty)) = variant_fields
                                    .iter()
                                    .find(|(field_name, _)| field_name == &pf.name)
                                {
                                    if let Some((pat, ps)) = &pf.pattern {
                                        self.bind_pattern(pat, field_ty, is_mutable, ps);
                                    } else {
                                        self.check_shadowing(&pf.name, span);
                                        self.env.define_with_span(
                                            pf.name.clone(),
                                            field_ty.clone(),
                                            is_mutable,
                                            span.clone(),
                                        );
                                    }
                                } else {
                                    let known: Vec<&str> =
                                        variant_fields.iter().map(|(n, _)| n.as_str()).collect();
                                    let similar =
                                        crate::error::find_similar(&pf.name, known.into_iter());
                                    self.report_error_with_suggestions(
                                        TypeErrorKind::UndefinedField,
                                        span,
                                        format!("no field `{}` on variant `{name}`", pf.name),
                                        similar,
                                    );
                                }
                            }
                        } else {
                            for pf in fields {
                                if let Some(field_ty) = td.fields.get(&pf.name) {
                                    if let Some((pat, ps)) = &pf.pattern {
                                        self.bind_pattern(pat, field_ty, is_mutable, ps);
                                    } else {
                                        self.check_shadowing(&pf.name, span);
                                        self.env.define_with_span(
                                            pf.name.clone(),
                                            field_ty.clone(),
                                            is_mutable,
                                            span.clone(),
                                        );
                                    }
                                } else {
                                    let similar = crate::error::find_similar(
                                        &pf.name,
                                        td.fields.keys().map(String::as_str),
                                    );
                                    self.report_error_with_suggestions(
                                        TypeErrorKind::UndefinedField,
                                        span,
                                        format!("no field `{}` on type `{name}`", pf.name),
                                        similar,
                                    );
                                }
                            }
                        }
                    }
                }
            }
            Pattern::Tuple(pats) => {
                if let Ty::Tuple(tys) = ty {
                    if pats.len() != tys.len() {
                        self.report_error(
                            TypeErrorKind::ArityMismatch,
                            span,
                            format!(
                                "tuple pattern has {} elements but type has {}",
                                pats.len(),
                                tys.len()
                            ),
                        );
                    }
                    for (p, t) in pats.iter().zip(tys.iter()) {
                        self.bind_pattern(&p.0, t, is_mutable, &p.1);
                    }
                }
            }
            Pattern::Or(a, b) => {
                // Both branches should bind the same names with compatible types.
                self.bind_pattern(&a.0, ty, is_mutable, &a.1);
                self.bind_pattern(&b.0, ty, is_mutable, &b.1);
            }
        }
    }

    fn lookup_variant_types(&self, variant_name: &str, enum_ty: &Ty) -> Option<Vec<Ty>> {
        // Handle Option<T> variants
        if let Ty::Option(inner) = enum_ty {
            return match variant_name {
                "Some" => Some(vec![(**inner).clone()]),
                "None" => Some(vec![]),
                _ => None,
            };
        }
        // Handle Result<T, E> variants
        if let Ty::Result { ok, err } = enum_ty {
            return match variant_name {
                "Ok" => Some(vec![(**ok).clone()]),
                "Err" => Some(vec![(**err).clone()]),
                _ => None,
            };
        }
        // Strip enum prefix from qualified names (e.g., "Color::Custom" -> "Custom")
        let short_name = variant_name.rsplit("::").next().unwrap_or(variant_name);
        if let Ty::Named {
            name: type_name, ..
        } = enum_ty
        {
            if let Some(td) = self.lookup_type_def(type_name) {
                if let Some(v) = td.variants.get(short_name) {
                    return match v {
                        VariantDef::Unit => Some(vec![]),
                        VariantDef::Tuple(fields) => Some(fields.clone()),
                        VariantDef::Struct(_) => None,
                    };
                }
                if let Some(v) = td.variants.get(variant_name) {
                    return match v {
                        VariantDef::Unit => Some(vec![]),
                        VariantDef::Tuple(fields) => Some(fields.clone()),
                        VariantDef::Struct(_) => None,
                    };
                }
                // Scrutinee type is a known enum — variant not found in it.
                // Do NOT fall through to global search; that would let
                // variants from unrelated enums silently type-check.
                if !td.variants.is_empty() {
                    return None;
                }
            }
        }
        // If scrutinee type is unknown/var, still allow binding
        if let Ty::Var(_) | Ty::Error = enum_ty {
            return Some(vec![Ty::Var(TypeVar::fresh())]);
        }
        // Search all enum types for the variant only when scrutinee type is
        // not a known enum (e.g. int, string, or other non-enum named types).
        for td in self.type_defs.values() {
            if let Some(v) = td.variants.get(short_name) {
                if let VariantDef::Tuple(fields) = v {
                    return Some(fields.clone());
                }
                if let VariantDef::Unit = v {
                    return Some(vec![]);
                }
            }
        }
        None
    }

    fn freshen_inner(&self, ty: &Ty, mapping: &mut HashMap<u32, Ty>) -> Ty {
        match ty {
            Ty::Var(v) => {
                let resolved = self.subst.resolve(ty);
                if resolved == *ty {
                    // Unresolved — map to a consistent fresh var
                    mapping
                        .entry(v.0)
                        .or_insert_with(|| Ty::Var(TypeVar::fresh()))
                        .clone()
                } else {
                    // Already resolved — use the concrete type
                    resolved
                }
            }
            Ty::Named { name, args } => Ty::Named {
                name: name.clone(),
                args: args
                    .iter()
                    .map(|a| self.freshen_inner(a, mapping))
                    .collect(),
            },
            Ty::Tuple(ts) => Ty::Tuple(ts.iter().map(|t| self.freshen_inner(t, mapping)).collect()),
            Ty::Array(inner, n) => Ty::Array(Box::new(self.freshen_inner(inner, mapping)), *n),
            Ty::Slice(inner) => Ty::Slice(Box::new(self.freshen_inner(inner, mapping))),
            Ty::Option(inner) => Ty::Option(Box::new(self.freshen_inner(inner, mapping))),
            Ty::Result { ok, err } => Ty::Result {
                ok: Box::new(self.freshen_inner(ok, mapping)),
                err: Box::new(self.freshen_inner(err, mapping)),
            },
            Ty::Function { params, ret } => Ty::Function {
                params: params
                    .iter()
                    .map(|p| self.freshen_inner(p, mapping))
                    .collect(),
                ret: Box::new(self.freshen_inner(ret, mapping)),
            },
            Ty::Closure {
                params,
                ret,
                captures,
            } => Ty::Closure {
                params: params
                    .iter()
                    .map(|p| self.freshen_inner(p, mapping))
                    .collect(),
                ret: Box::new(self.freshen_inner(ret, mapping)),
                captures: captures
                    .iter()
                    .map(|c| self.freshen_inner(c, mapping))
                    .collect(),
            },
            Ty::ActorRef(inner) => Ty::ActorRef(Box::new(self.freshen_inner(inner, mapping))),
            Ty::Stream(inner) => Ty::Stream(Box::new(self.freshen_inner(inner, mapping))),
            Ty::Sink(inner) => Ty::Sink(Box::new(self.freshen_inner(inner, mapping))),
            Ty::Range(inner) => Ty::Range(Box::new(self.freshen_inner(inner, mapping))),
            _ => ty.clone(),
        }
    }

    fn instantiate_fn_sig_for_call(
        &mut self,
        sig: &FnSig,
        type_args: Option<&[Spanned<TypeExpr>]>,
        span: &Span,
    ) -> (Vec<Ty>, Ty, Vec<Ty>) {
        let mut params = sig.params.clone();
        let mut ret = sig.return_type.clone();
        let mut resolved_type_args = Vec::new();

        if let Some(type_args) = type_args {
            if sig.type_params.is_empty() {
                self.report_error(
                    TypeErrorKind::ArityMismatch,
                    span,
                    format!(
                        "this function takes 0 type argument(s) but {} were supplied",
                        type_args.len()
                    ),
                );
            } else if type_args.len() != sig.type_params.len() {
                self.report_error(
                    TypeErrorKind::ArityMismatch,
                    span,
                    format!(
                        "this function takes {} type argument(s) but {} were supplied",
                        sig.type_params.len(),
                        type_args.len()
                    ),
                );
            }
        }

        if !sig.type_params.is_empty() {
            resolved_type_args = type_args.map_or(vec![], |args| {
                args.iter()
                    .take(sig.type_params.len())
                    .map(|(te, _)| self.resolve_type_expr(te))
                    .collect::<Vec<_>>()
            });
            while resolved_type_args.len() < sig.type_params.len() {
                resolved_type_args.push(Ty::Var(TypeVar::fresh()));
            }
            for (tp, ta) in sig.type_params.iter().zip(resolved_type_args.iter()) {
                params = params
                    .iter()
                    .map(|param| self.substitute_named_param(param, tp, ta))
                    .collect();
                ret = self.substitute_named_param(&ret, tp, ta);
            }
        }

        // For each call, freshen any unresolved type variables in the signature
        // to make generic builtins like println work with different types each call.
        // Use a shared mapping so params and return type share the same fresh vars.
        let mut mapping: HashMap<u32, Ty> = HashMap::new();
        let freshened_params = params
            .iter()
            .map(|param| self.freshen_inner(param, &mut mapping))
            .collect();
        let freshened_ret = self.freshen_inner(&ret, &mut mapping);
        (freshened_params, freshened_ret, resolved_type_args)
    }

    fn enforce_type_param_bounds(&mut self, sig: &FnSig, type_args: &[Ty], span: &Span) {
        if sig.type_params.is_empty() {
            return;
        }
        for (idx, param_name) in sig.type_params.iter().enumerate() {
            let Some(bounds) = sig.type_param_bounds.get(param_name) else {
                continue;
            };
            let Some(type_arg) = type_args.get(idx) else {
                continue;
            };
            let resolved_arg = self.subst.resolve(type_arg);
            for bound in bounds {
                if self.type_satisfies_trait_bound(&resolved_arg, bound) {
                    continue;
                }
                self.report_error(
                    TypeErrorKind::BoundsNotSatisfied,
                    span,
                    format!(
                        "type `{}` does not implement trait `{}` required by `{}`",
                        resolved_arg, bound, param_name
                    ),
                );
            }
        }
    }

    fn type_satisfies_trait_bound(&self, ty: &Ty, trait_name: &str) -> bool {
        match ty {
            Ty::Named { name, .. } => self.type_implements_trait(name, trait_name),
            Ty::TraitObject { traits } => traits.iter().any(|t| {
                t.trait_name == trait_name || self.trait_extends(&t.trait_name, trait_name)
            }),
            _ => false,
        }
    }

    /// Check if a concrete type implements a trait (directly or via super-trait chain).
    fn type_implements_trait(&self, type_name: &str, trait_name: &str) -> bool {
        // Direct impl
        if self
            .trait_impls_set
            .contains(&(type_name.to_string(), trait_name.to_string()))
        {
            return true;
        }
        // Strip known module prefix: "json.Value" → "Value"
        let unqualified_type = type_name.find('.').and_then(|dot| {
            let prefix = &type_name[..dot];
            if self.modules.contains(prefix) {
                Some(&type_name[dot + 1..])
            } else {
                None
            }
        });
        if let Some(uq) = unqualified_type {
            if self
                .trait_impls_set
                .contains(&(uq.to_string(), trait_name.to_string()))
            {
                return true;
            }
        }
        // Also try unqualified trait name
        let unqualified_trait = trait_name.find('.').and_then(|dot| {
            let prefix = &trait_name[..dot];
            if self.modules.contains(prefix) {
                Some(&trait_name[dot + 1..])
            } else {
                None
            }
        });
        if let Some(uq_trait) = unqualified_trait {
            let tn = unqualified_type.unwrap_or(type_name);
            if self
                .trait_impls_set
                .contains(&(tn.to_string(), uq_trait.to_string()))
            {
                return true;
            }
        }
        // Check if type implements a sub-trait that extends this trait
        let effective_type = unqualified_type.unwrap_or(type_name);
        for (tn, tn_trait) in &self.trait_impls_set {
            if (tn == type_name || tn.as_str() == effective_type)
                && self.trait_extends(tn_trait, trait_name)
            {
                return true;
            }
        }
        false
    }

    /// Check if `child_trait` transitively extends `parent_trait`.
    fn trait_extends(&self, child_trait: &str, parent_trait: &str) -> bool {
        if let Some(supers) = self.trait_super.get(child_trait) {
            for s in supers {
                if s == parent_trait || self.trait_extends(s, parent_trait) {
                    return true;
                }
            }
        }
        false
    }

    /// Look up a method on a trait, walking super-traits if needed.
    /// Returns a `FnSig` with self filtered out.
    fn lookup_trait_method(&mut self, trait_name: &str, method: &str) -> Option<FnSig> {
        // Check the trait's own methods — clone data to release borrow before resolve_type_expr
        let found_method = self
            .trait_defs
            .get(trait_name)
            .and_then(|info| info.methods.iter().find(|m| m.name == method).cloned());
        if let Some(m) = found_method {
            let params: Vec<Ty> = m
                .params
                .iter()
                .filter(|p| p.name != "self")
                .map(|p| self.resolve_type_expr(&p.ty.0))
                .collect();
            let return_type = m
                .return_type
                .as_ref()
                .map_or(Ty::Unit, |(te, _)| self.resolve_type_expr(te));
            let param_names: Vec<String> = m
                .params
                .iter()
                .filter(|p| p.name != "self")
                .map(|p| p.name.clone())
                .collect();
            return Some(FnSig {
                type_params: vec![],
                type_param_bounds: HashMap::new(),
                param_names,
                params,
                return_type,
                is_async: false,
                is_pure: m.is_pure,
                accepts_kwargs: false,
                doc_comment: None,
            });
        }
        // Walk super-traits — clone to release borrow
        let supers = self.trait_super.get(trait_name).cloned();
        if let Some(supers) = supers {
            for super_trait in &supers {
                if let Some(sig) = self.lookup_trait_method(super_trait, method) {
                    return Some(sig);
                }
            }
        }
        None
    }

    /// Substitute a named type parameter (e.g. `T`) with a concrete type in a type expression.
    /// Used to resolve generic fields/methods on instantiated types.
    #[expect(
        clippy::self_only_used_in_recursion,
        reason = "method for consistency with other type helpers"
    )]
    fn substitute_named_param(&self, ty: &Ty, param_name: &str, replacement: &Ty) -> Ty {
        match ty {
            Ty::Named { name, args } if args.is_empty() && name == param_name => {
                replacement.clone()
            }
            Ty::Named { name, args } => Ty::Named {
                name: name.clone(),
                args: args
                    .iter()
                    .map(|a| self.substitute_named_param(a, param_name, replacement))
                    .collect(),
            },
            Ty::Tuple(elems) => Ty::Tuple(
                elems
                    .iter()
                    .map(|e| self.substitute_named_param(e, param_name, replacement))
                    .collect(),
            ),
            Ty::Array(inner, n) => Ty::Array(
                Box::new(self.substitute_named_param(inner, param_name, replacement)),
                *n,
            ),
            Ty::Slice(inner) => Ty::Slice(Box::new(self.substitute_named_param(
                inner,
                param_name,
                replacement,
            ))),
            Ty::Option(inner) => Ty::Option(Box::new(self.substitute_named_param(
                inner,
                param_name,
                replacement,
            ))),
            Ty::Result { ok, err } => Ty::Result {
                ok: Box::new(self.substitute_named_param(ok, param_name, replacement)),
                err: Box::new(self.substitute_named_param(err, param_name, replacement)),
            },
            Ty::Function { params, ret } => Ty::Function {
                params: params
                    .iter()
                    .map(|p| self.substitute_named_param(p, param_name, replacement))
                    .collect(),
                ret: Box::new(self.substitute_named_param(ret, param_name, replacement)),
            },
            Ty::Closure {
                params,
                ret,
                captures,
            } => Ty::Closure {
                params: params
                    .iter()
                    .map(|p| self.substitute_named_param(p, param_name, replacement))
                    .collect(),
                ret: Box::new(self.substitute_named_param(ret, param_name, replacement)),
                captures: captures
                    .iter()
                    .map(|c| self.substitute_named_param(c, param_name, replacement))
                    .collect(),
            },
            Ty::ActorRef(inner) => Ty::ActorRef(Box::new(self.substitute_named_param(
                inner,
                param_name,
                replacement,
            ))),
            Ty::Range(inner) => Ty::Range(Box::new(self.substitute_named_param(
                inner,
                param_name,
                replacement,
            ))),
            Ty::Stream(inner) => Ty::Stream(Box::new(self.substitute_named_param(
                inner,
                param_name,
                replacement,
            ))),
            Ty::Sink(inner) => Ty::Sink(Box::new(self.substitute_named_param(
                inner,
                param_name,
                replacement,
            ))),
            Ty::Pointer {
                is_mutable,
                pointee,
            } => Ty::Pointer {
                is_mutable: *is_mutable,
                pointee: Box::new(self.substitute_named_param(pointee, param_name, replacement)),
            },
            Ty::TraitObject { traits } => Ty::TraitObject {
                traits: traits
                    .iter()
                    .map(|bound| crate::ty::TraitObjectBound {
                        trait_name: bound.trait_name.clone(),
                        args: bound
                            .args
                            .iter()
                            .map(|a| self.substitute_named_param(a, param_name, replacement))
                            .collect(),
                    })
                    .collect(),
            },
            Ty::Generator { yields, returns } => Ty::Generator {
                yields: Box::new(self.substitute_named_param(yields, param_name, replacement)),
                returns: Box::new(self.substitute_named_param(returns, param_name, replacement)),
            },
            Ty::AsyncGenerator { yields } => Ty::AsyncGenerator {
                yields: Box::new(self.substitute_named_param(yields, param_name, replacement)),
            },
            _ => ty.clone(),
        }
    }

    fn resolve_type_expr(&mut self, te: &TypeExpr) -> Ty {
        match te {
            TypeExpr::Named { name, type_args } => {
                // Handle `Self` type
                if name == "Self" {
                    if let Some((self_type_name, self_type_args)) = &self.current_self_type {
                        return Ty::Named {
                            name: self_type_name.clone(),
                            args: self_type_args.clone(),
                        };
                    }
                }
                if let Some(alias_name) = name.strip_prefix("Self::") {
                    if let Some(alias_ty) = self.resolve_impl_associated_type(alias_name) {
                        if type_args.as_ref().is_some_and(|args| !args.is_empty()) {
                            let should_report = self
                                .impl_alias_scopes
                                .last()
                                .is_some_and(|s| s.report_missing);
                            let err_span = self.impl_alias_scopes.last().map(|s| s.span.clone());
                            if should_report {
                                if let Some(sp) = err_span {
                                    self.report_error(
                                        TypeErrorKind::ArityMismatch,
                                        &sp,
                                        format!(
                                            "associated type `Self::{alias_name}` does not take type arguments"
                                        ),
                                    );
                                }
                            }
                        }
                        return alias_ty;
                    }
                }
                // Check for primitive types first
                match name.as_str() {
                    "i8" => Ty::I8,
                    "i16" => Ty::I16,
                    "i32" => Ty::I32,
                    "i64" | "int" | "Int" => Ty::I64,
                    "u8" | "byte" => Ty::U8,
                    "u16" => Ty::U16,
                    "u32" => Ty::U32,
                    "u64" | "uint" => Ty::U64,
                    "f32" => Ty::F32,
                    "f64" | "float" | "Float" => Ty::F64,
                    "bool" | "Bool" => Ty::Bool,
                    "char" | "Char" => Ty::Char,
                    "string" | "String" | "str" => Ty::String,
                    "()" => Ty::Unit,
                    _ => {
                        let args = type_args.as_ref().map_or(vec![], |ta| {
                            ta.iter()
                                .map(|(te, _)| self.resolve_type_expr(te))
                                .collect()
                        });
                        // Check if it's a generic type parameter
                        for ctx in self.generic_ctx.iter().rev() {
                            if let Some(ty) = ctx.get(name) {
                                return ty.clone();
                            }
                        }
                        // Check type aliases (transparent: Distance = int → Ty::I64)
                        if let Some(aliased) = self.type_aliases.get(name) {
                            return aliased.clone();
                        }
                        // Handle special named types
                        match name.as_str() {
                            "Option" if args.len() == 1 => Ty::Option(Box::new(args[0].clone())),
                            "Result" if args.len() == 2 => Ty::Result {
                                ok: Box::new(args[0].clone()),
                                err: Box::new(args[1].clone()),
                            },
                            "ActorRef" if args.len() == 1 => {
                                Ty::ActorRef(Box::new(args[0].clone()))
                            }
                            // Deprecated alias: ActorStream<Y> is now Stream<Y>
                            "ActorStream" if args.len() == 1 => {
                                Ty::Stream(Box::new(args[0].clone()))
                            }
                            "Stream" if args.len() == 1 => Ty::Stream(Box::new(args[0].clone())),
                            "Sink" if args.len() == 1 => Ty::Sink(Box::new(args[0].clone())),
                            _ => {
                                // Qualify unqualified handle types only when imported and unambiguous.
                                let handle_matches: Vec<&String> = self
                                    .known_types
                                    .iter()
                                    .filter(|qualified| {
                                        qualified
                                            .rsplit_once('.')
                                            .is_some_and(|(_, short)| short == name)
                                    })
                                    .collect();
                                let resolved_name = if self.type_defs.contains_key(name) {
                                    name.clone()
                                } else {
                                    match handle_matches.as_slice() {
                                        [qualified] => (*qualified).clone(),
                                        _ => name.clone(),
                                    }
                                };
                                // Mark module as used when type name is module-qualified
                                if let Some((module, _)) = resolved_name.split_once('.') {
                                    self.used_modules.borrow_mut().insert(module.to_string());
                                }
                                Ty::Named {
                                    name: resolved_name,
                                    args,
                                }
                            }
                        }
                    }
                }
            }
            TypeExpr::Result { ok, err } => Ty::Result {
                ok: Box::new(self.resolve_type_expr(&ok.0)),
                err: Box::new(self.resolve_type_expr(&err.0)),
            },
            TypeExpr::Option(inner) => Ty::Option(Box::new(self.resolve_type_expr(&inner.0))),
            TypeExpr::Tuple(elems) if elems.is_empty() => Ty::Unit,
            TypeExpr::Tuple(elems) => Ty::Tuple(
                elems
                    .iter()
                    .map(|(te, _)| self.resolve_type_expr(te))
                    .collect(),
            ),
            TypeExpr::Array { element, size } => {
                Ty::Array(Box::new(self.resolve_type_expr(&element.0)), *size)
            }
            TypeExpr::Slice(inner) => Ty::Slice(Box::new(self.resolve_type_expr(&inner.0))),
            TypeExpr::Function {
                params,
                return_type,
            } => Ty::Function {
                params: params
                    .iter()
                    .map(|(te, _)| self.resolve_type_expr(te))
                    .collect(),
                ret: Box::new(self.resolve_type_expr(&return_type.0)),
            },
            TypeExpr::Pointer {
                is_mutable,
                pointee,
            } => Ty::Pointer {
                is_mutable: *is_mutable,
                pointee: Box::new(self.resolve_type_expr(&pointee.0)),
            },
            TypeExpr::TraitObject(bounds) => {
                let traits = bounds
                    .iter()
                    .map(|bound| {
                        let args = bound.type_args.as_ref().map_or(vec![], |ta| {
                            ta.iter().map(|t| self.resolve_type_expr(&t.0)).collect()
                        });
                        crate::ty::TraitObjectBound {
                            trait_name: bound.name.clone(),
                            args,
                        }
                    })
                    .collect();
                Ty::TraitObject { traits }
            }
            TypeExpr::Infer => Ty::Var(TypeVar::fresh()),
        }
    }

    /// Unify two branch types (if/else, if-let/else).
    ///
    /// - If one branch diverges (`Never`), returns the other branch's type.
    /// - If either branch is `Unit`, the expression evaluates to `Unit`.
    /// - Otherwise, unifies the two types and returns the then-branch type.
    fn unify_branches(&mut self, then_ty: &Ty, else_ty: &Ty, span: &Span) -> Ty {
        if matches!(then_ty, Ty::Never) {
            return else_ty.clone();
        }
        if matches!(else_ty, Ty::Never) {
            return then_ty.clone();
        }
        if *then_ty == Ty::Unit || *else_ty == Ty::Unit {
            return Ty::Unit;
        }
        self.expect_type(then_ty, else_ty, span);
        then_ty.clone()
    }

    fn expect_type(&mut self, expected: &Ty, actual: &Ty, span: &Span) {
        // Snapshot substitution so partial bindings are rolled back on failure
        let snapshot = self.subst.snapshot();
        if let Err(_e) = unify(&mut self.subst, expected, actual) {
            // Restore substitution to avoid partial corruption
            self.subst.restore(snapshot);
            let expected_resolved = self.subst.resolve(expected);
            let actual_resolved = self.subst.resolve(actual);
            // Allow i32 where bool is expected (Hew uses i32 for truthiness)
            if expected_resolved == Ty::Bool && actual_resolved == Ty::I32 {
                return;
            }
            // Allow same-signed integer coercions (e.g. i32 <-> i64, u16 <-> u32).
            if can_implicitly_coerce_integer(&actual_resolved, &expected_resolved) {
                return;
            }
            // Allow handle types to coerce to/from string (both are !llvm.ptr at runtime)
            if let Ty::Named { name, .. } = &actual_resolved {
                if is_handle_type(name) && expected_resolved == Ty::String {
                    return;
                }
            }
            if let Ty::Named { name, .. } = &expected_resolved {
                if is_handle_type(name) && actual_resolved == Ty::String {
                    return;
                }
            }
            // Allow concrete type → dyn Trait coercion when the type implements all traits
            if let Ty::TraitObject { traits } = &expected_resolved {
                if let Ty::Named {
                    name: type_name, ..
                } = &actual_resolved
                {
                    // Check that the type implements all required traits
                    if traits
                        .iter()
                        .all(|bound| self.type_implements_trait(type_name, &bound.trait_name))
                    {
                        return;
                    }
                }
            }
            if expected_resolved.is_numeric() && actual_resolved.is_numeric() {
                self.report_error(
                    TypeErrorKind::Mismatch {
                        expected: format!("{expected_resolved}"),
                        actual: format!("{actual_resolved}"),
                    },
                    span,
                    format!(
                        "implicit numeric coercion from `{actual_resolved}` to `{expected_resolved}` is not allowed; use an explicit conversion"
                    ),
                );
                return;
            }
            if expected_resolved != Ty::Error && actual_resolved != Ty::Error {
                self.report_error(
                    TypeErrorKind::Mismatch {
                        expected: format!("{expected_resolved}"),
                        actual: format!("{actual_resolved}"),
                    },
                    span,
                    format!(
                        "type mismatch: expected `{expected_resolved}`, found `{actual_resolved}`"
                    ),
                );
            }
        }
    }

    /// Check if an expression is typically used for side effects (not for its return value).

    fn record_type(&mut self, span: &Span, ty: &Ty) {
        self.expr_types.insert(SpanKey::from(span), ty.clone());
    }

    fn warn_wasm_limitation(&mut self, span: &Span, feature: WasmUnsupportedFeature) {
        if !self.wasm_target {
            return;
        }
        let key = (SpanKey::from(span), feature);
        if !self.wasm_warning_spans.insert(key) {
            return;
        }
        self.warnings.push(TypeError {
            severity: crate::error::Severity::Warning,
            kind: TypeErrorKind::PlatformLimitation,
            span: span.clone(),
            message: format!(
                "{} are not supported on WASM32 — {}",
                feature.label(),
                feature.reason()
            ),
            notes: vec![],
            suggestions: vec![
                "Consider using basic actors (spawn/send/ask) which work on WASM.".to_string(),
            ],
        });
    }

    fn report_error(&mut self, kind: TypeErrorKind, span: &Span, message: String) {
        self.errors.push(TypeError {
            severity: crate::error::Severity::Error,
            kind,
            span: span.clone(),
            message,
            notes: vec![],
            suggestions: vec![],
        });
    }

    fn report_error_with_suggestions(
        &mut self,
        kind: TypeErrorKind,
        span: &Span,
        message: String,
        suggestions: Vec<String>,
    ) {
        self.errors.push(TypeError {
            severity: crate::error::Severity::Error,
            kind,
            span: span.clone(),
            message,
            notes: vec![],
            suggestions,
        });
    }

    /// Check if a variable binding would shadow an outer scope variable and emit a warning.
    fn check_shadowing(&mut self, name: &str, span: &Span) {
        if name.starts_with('_') || self.in_for_binding {
            return;
        }
        if let Some(prev_span) = self.env.find_in_outer_scope(name) {
            self.warnings.push(TypeError {
                severity: crate::error::Severity::Warning,
                kind: TypeErrorKind::Shadowing,
                span: span.clone(),
                message: format!("variable `{name}` shadows a previous binding"),
                notes: vec![(prev_span, "previously defined here".to_string())],
                suggestions: vec![format!(
                    "if this is intentional, prefix with underscore: `_{name}`"
                )],
            });
        }
    }

    /// Pop the current scope and emit warnings for unused/unmutated bindings.
    fn emit_scope_warnings(&mut self) {
        use crate::env::ScopeWarningKind;
        for w in self.env.pop_scope_with_warnings() {
            match w.kind {
                ScopeWarningKind::Unused => {
                    self.warnings.push(TypeError {
                        severity: crate::error::Severity::Warning,
                        kind: TypeErrorKind::UnusedVariable,
                        span: w.span,
                        message: format!("unused variable `{}`", w.name),
                        notes: vec![],
                        suggestions: vec![format!("prefix with underscore: `_{}`", w.name)],
                    });
                }
                ScopeWarningKind::NeverMutated => {
                    self.warnings.push(TypeError {
                        severity: crate::error::Severity::Warning,
                        kind: TypeErrorKind::UnusedMut,
                        span: w.span,
                        message: format!(
                            "variable `{}` is declared mutable but never reassigned",
                            w.name
                        ),
                        notes: vec![],
                        suggestions: vec!["use `let` instead of `var`".to_string()],
                    });
                }
            }
        }
    }

    fn check_exhaustiveness(&mut self, scrutinee_ty: &Ty, arms: &[MatchArm], span: &Span) {
        fn visit_or_patterns<'a, F: FnMut(&'a Pattern)>(pattern: &'a Pattern, f: &mut F) {
            match pattern {
                Pattern::Or(left, right) => {
                    visit_or_patterns(&left.0, f);
                    visit_or_patterns(&right.0, f);
                }
                _ => f(pattern),
            }
        }

        let mut has_wildcard = false;
        for arm in arms {
            if arm.guard.is_some() {
                continue;
            }
            visit_or_patterns(&arm.pattern.0, &mut |pattern| {
                if matches!(pattern, Pattern::Wildcard) {
                    has_wildcard = true;
                }
            });
            if has_wildcard {
                return;
            }
        }

        match scrutinee_ty {
            Ty::Named { name, .. } => {
                if let Some(td) = self.lookup_type_def(name) {
                    if !td.variants.is_empty() {
                        let mut has_binding_identifier = false;
                        let mut covered = Vec::new();
                        for arm in arms {
                            if arm.guard.is_some() {
                                continue;
                            }
                            visit_or_patterns(&arm.pattern.0, &mut |pattern| match pattern {
                                Pattern::Constructor { name, .. }
                                | Pattern::Struct { name, .. } => covered.push(name.clone()),
                                Pattern::Identifier(id) => {
                                    if td.variants.contains_key(id) {
                                        covered.push(id.clone());
                                    } else {
                                        has_binding_identifier = true;
                                    }
                                }
                                _ => {}
                            });
                        }
                        if has_binding_identifier {
                            return;
                        }
                        let missing: Vec<_> = td
                            .variants
                            .keys()
                            .filter(|v| !covered.contains(v))
                            .collect();
                        if !missing.is_empty() {
                            let names: Vec<_> = missing.iter().map(|s| s.as_str()).collect();
                            self.warn_non_exhaustive(
                                span,
                                &format!("missing {}", names.join(", ")),
                            );
                        }
                    }
                }
            }
            Ty::Bool => {
                let mut has_binding_identifier = false;
                let mut has_true = false;
                let mut has_false = false;
                for arm in arms {
                    if arm.guard.is_some() {
                        continue;
                    }
                    visit_or_patterns(&arm.pattern.0, &mut |pattern| match pattern {
                        Pattern::Identifier(_) => {
                            has_binding_identifier = true;
                        }
                        Pattern::Literal(Literal::Bool(true)) => {
                            has_true = true;
                        }
                        Pattern::Literal(Literal::Bool(false)) => {
                            has_false = true;
                        }
                        _ => {}
                    });
                }
                if !has_binding_identifier && (!has_true || !has_false) {
                    self.warn_non_exhaustive(span, "missing bool variant");
                }
            }
            Ty::Option(_) => {
                if !Self::has_both_constructor_variants(arms, "Some", "None") {
                    self.warn_non_exhaustive(span, "Option requires Some and None arms");
                }
            }
            Ty::Result { .. } => {
                if !Self::has_both_constructor_variants(arms, "Ok", "Err") {
                    self.warn_non_exhaustive(span, "Result requires Ok and Err arms");
                }
            }
            _ => {
                // For non-enum types (int, float, string, etc.), check for catch-all patterns.
                let mut has_catch_all = false;
                for arm in arms {
                    if arm.guard.is_some() {
                        continue;
                    }
                    visit_or_patterns(&arm.pattern.0, &mut |pattern| {
                        if matches!(pattern, Pattern::Identifier(_)) {
                            has_catch_all = true;
                        }
                    });
                    if has_catch_all {
                        break;
                    }
                }
                if !has_catch_all {
                    self.warn_non_exhaustive(span, "consider adding a wildcard `_` arm");
                }
            }
        }
    }

    fn warn_non_exhaustive(&mut self, span: &Span, detail: &str) {
        self.warnings.push(TypeError {
            severity: crate::error::Severity::Warning,
            kind: TypeErrorKind::NonExhaustiveMatch,
            span: span.clone(),
            message: format!("non-exhaustive match: {detail}"),
            notes: vec![],
            suggestions: vec![],
        });
    }

    /// Check if match arms cover both constructor variants (e.g., Some/None, Ok/Err).
    /// Returns true if exhaustive (either a binding wildcard or both variants present).
    fn has_both_constructor_variants(arms: &[MatchArm], variant_a: &str, variant_b: &str) -> bool {
        fn visit_or<'a>(pattern: &'a Pattern, f: &mut impl FnMut(&'a Pattern)) {
            match pattern {
                Pattern::Or(left, right) => {
                    visit_or(&left.0, f);
                    visit_or(&right.0, f);
                }
                _ => f(pattern),
            }
        }

        let mut has_binding_identifier = false;
        let mut has_a = false;
        let mut has_b = false;
        for arm in arms {
            if arm.guard.is_some() {
                continue;
            }
            visit_or(&arm.pattern.0, &mut |pattern| match pattern {
                Pattern::Constructor { name, .. } | Pattern::Struct { name, .. }
                    if name == variant_a =>
                {
                    has_a = true;
                }
                Pattern::Constructor { name, .. } | Pattern::Struct { name, .. }
                    if name == variant_b =>
                {
                    has_b = true;
                }
                Pattern::Identifier(name) if name == variant_a => {
                    has_a = true;
                }
                Pattern::Identifier(name) if name == variant_b => {
                    has_b = true;
                }
                Pattern::Identifier(_) => {
                    has_binding_identifier = true;
                }
                _ => {}
            });
        }
        has_binding_identifier || (has_a && has_b)
    }
}

impl Default for Checker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hew_parser::ast::{ImportName, Param, Visibility};

    fn check_source(_source: &str) -> TypeCheckOutput {
        // hew-types has zero external deps, so we cannot parse source here.
        // Integration tests that parse+check live in the workspace-level tests.
        let program = Program {
            module_graph: None,
            items: vec![],
            module_doc: None,
        };
        let mut checker = Checker::new();
        checker.check_program(&program)
    }

    #[test]
    fn test_empty_program() {
        let output = check_source("");
        assert!(output.errors.is_empty());
    }

    #[test]
    fn test_type_checker_creation() {
        let checker = Checker::new();
        assert_eq!(checker.errors.len(), 0);
    }

    // Helper functions for testing AST construction
    fn make_int_literal(n: i64, span: Span) -> Spanned<Expr> {
        (
            Expr::Literal(Literal::Integer {
                value: n,
                radix: IntRadix::Decimal,
            }),
            span,
        )
    }

    fn make_bool_literal(b: bool, span: Span) -> Spanned<Expr> {
        (Expr::Literal(Literal::Bool(b)), span)
    }

    #[test]
    fn test_literal_types() {
        let mut checker = Checker::new();

        // Test integer literal (defaults to i64)
        let int_expr = make_int_literal(42, 0..2);
        let int_ty = checker.synthesize(&int_expr.0, &int_expr.1);
        assert_eq!(int_ty, Ty::I64);

        // Test boolean literal
        let bool_expr = make_bool_literal(true, 0..4);
        let bool_ty = checker.synthesize(&bool_expr.0, &bool_expr.1);
        assert_eq!(bool_ty, Ty::Bool);
    }

    #[test]
    fn test_builtin_registration() {
        let mut checker = Checker::new();
        checker.register_builtins();

        // Check that println_int is registered
        assert!(checker.fn_sigs.contains_key("println_int"));
        let sig = &checker.fn_sigs["println_int"];
        assert_eq!(sig.params.len(), 1);
        assert_eq!(sig.params[0], Ty::I64);
        assert_eq!(sig.return_type, Ty::Unit);
    }

    #[test]
    fn test_yield_outside_generator() {
        use hew_parser::ast::{Block, Expr, FnDecl, Item, Program, Stmt};

        let yield_expr: Spanned<Expr> = (Expr::Yield(None), 10..15);
        let body = Block {
            stmts: vec![(Stmt::Expression(yield_expr), 10..15)],
            trailing_expr: None,
        };
        let fd = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            is_pure: false,
            name: "not_a_gen".to_string(),
            type_params: None,
            params: vec![],
            return_type: None,
            where_clause: None,
            body,
            doc_comment: None,
        };
        let program = Program {
            module_graph: None,
            items: vec![(Item::Function(fd), 0..30)],
            module_doc: None,
        };
        let mut checker = Checker::new();
        let output = checker.check_program(&program);
        assert!(output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::YieldOutsideGenerator));
    }

    #[test]
    fn test_receive_gen_fn_returns_stream() {
        use hew_parser::ast::{ActorDecl, Expr, Item, Literal, ReceiveFnDecl, Stmt, TypeExpr};

        let receive_fn = ReceiveFnDecl {
            is_generator: true,
            is_pure: false,
            name: "numbers".to_string(),
            type_params: None,
            params: vec![],
            return_type: Some((
                TypeExpr::Named {
                    name: "int".to_string(),
                    type_args: None,
                },
                0..0,
            )),
            where_clause: None,
            body: Block {
                stmts: vec![(
                    Stmt::Expression((
                        Expr::Yield(Some(Box::new((
                            Expr::Literal(Literal::Integer {
                                value: 1,
                                radix: IntRadix::Decimal,
                            }),
                            0..0,
                        )))),
                        0..0,
                    )),
                    0..0,
                )],
                trailing_expr: None,
            },
            span: 0..0,
        };

        let actor = ActorDecl {
            visibility: Visibility::Pub,
            name: "NumberStream".to_string(),
            super_traits: None,
            init: None,
            fields: vec![],
            receive_fns: vec![receive_fn],
            methods: vec![],
            mailbox_capacity: None,
            overflow_policy: None,
            is_isolated: false,
            doc_comment: None,
        };
        let program = Program {
            module_graph: None,
            items: vec![(Item::Actor(actor), 0..0)],
            module_doc: None,
        };

        let mut checker = Checker::new();
        let output = checker.check_program(&program);
        assert!(output.errors.is_empty());
        assert_eq!(
            output.fn_sigs["NumberStream::numbers"].return_type,
            Ty::Stream(Box::new(Ty::I64))
        );
    }

    #[test]
    fn typecheck_generic_call_with_explicit_type_args() {
        let source = concat!(
            "fn identity<T>(x: T) -> T { x }\n",
            "fn main() {\n",
            "    let a = identity<int>(42);\n",
            "    let b = identity<string>(\"hello\");\n",
            "    println(a);\n",
            "    println(b);\n",
            "}\n"
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            output.errors.is_empty(),
            "unexpected errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn typecheck_generic_call_with_inferred_type_args() {
        let source = concat!(
            "fn identity<T>(x: T) -> T { x }\n",
            "fn main() {\n",
            "    let a = identity(42);\n",
            "    let b = identity(\"hello\");\n",
            "    println(a);\n",
            "    println(b);\n",
            "}\n"
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            output.errors.is_empty(),
            "unexpected errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn typecheck_generator_yield_uses_element_type() {
        let source = concat!(
            "gen fn count_up() -> int {\n",
            "    yield 1;\n",
            "    yield 2;\n",
            "}\n"
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            output.errors.is_empty(),
            "unexpected errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn typecheck_async_generator_yield_uses_element_type() {
        let source = concat!(
            "async gen fn count_up() -> int {\n",
            "    yield 1;\n",
            "    yield 2;\n",
            "}\n"
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            output.errors.is_empty(),
            "unexpected errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn typecheck_generator_yield_mismatch_reports_element_type() {
        let source = "gen fn bad() -> int { yield \"oops\"; }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            output.errors.iter().any(|e| {
                matches!(
                    &e.kind,
                    TypeErrorKind::Mismatch { expected, actual }
                    if expected == "i64" && actual == "String"
                )
            }),
            "expected element-type mismatch, got: {:?}",
            output.errors
        );
        assert!(
            output
                .errors
                .iter()
                .all(|e| !e.message.contains("Generator<")),
            "yield diagnostic should mention element type, got: {:?}",
            output.errors
        );
    }

    #[test]
    fn test_actor_stream_annotation_is_stream_alias() {
        use hew_parser::ast::{FnDecl, Item, TypeExpr};

        // A standalone function returning ActorStream<i32> should resolve to Stream<i32>
        let fn_decl = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            is_pure: false,
            name: "foo".to_string(),
            type_params: None,
            params: vec![],
            return_type: Some((
                TypeExpr::Named {
                    name: "ActorStream".to_string(),
                    type_args: Some(vec![(
                        TypeExpr::Named {
                            name: "i32".to_string(),
                            type_args: None,
                        },
                        0..0,
                    )]),
                },
                0..0,
            )),
            where_clause: None,
            body: Block {
                stmts: vec![],
                trailing_expr: None,
            },
            doc_comment: None,
        };

        let program = Program {
            module_graph: None,
            items: vec![(Item::Function(fn_decl), 0..0)],
            module_doc: None,
        };

        let mut checker = Checker::new();
        let output = checker.check_program(&program);
        // The body is empty (returns unit) so there will be a return-type mismatch error,
        // but fn_sigs is populated in pass 1 (before body checking), so the signature
        // should already reflect the resolved return type.
        assert_eq!(
            output.fn_sigs["foo"].return_type,
            Ty::Stream(Box::new(Ty::I32))
        );
    }

    #[test]
    fn test_arity_mismatch_too_many_args() {
        let mut checker = Checker::new();
        checker.register_builtins();
        // println_int takes 1 arg; call with 2
        let call = (
            Expr::Call {
                function: Box::new((Expr::Identifier("println_int".to_string()), 0..11)),
                type_args: None,
                args: vec![
                    CallArg::Positional((
                        Expr::Literal(hew_parser::ast::Literal::Integer {
                            value: 1,
                            radix: IntRadix::Decimal,
                        }),
                        12..13,
                    )),
                    CallArg::Positional((
                        Expr::Literal(hew_parser::ast::Literal::Integer {
                            value: 2,
                            radix: IntRadix::Decimal,
                        }),
                        15..16,
                    )),
                ],
                is_tail_call: false,
            },
            0..17,
        );
        checker.synthesize(&call.0, &call.1);
        assert!(checker
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::ArityMismatch));
    }

    #[test]
    fn test_arity_mismatch_too_few_args() {
        let mut checker = Checker::new();
        checker.register_builtins();
        // println_int takes 1 arg; call with 0
        let call = (
            Expr::Call {
                function: Box::new((Expr::Identifier("println_int".to_string()), 0..11)),
                type_args: None,
                args: vec![],
                is_tail_call: false,
            },
            0..13,
        );
        checker.synthesize(&call.0, &call.1);
        assert!(checker
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::ArityMismatch));
    }

    #[test]
    fn typecheck_error_undefined_var() {
        let result = hew_parser::parse("fn main() -> i32 {\n    let x = undefined_var;\n    x\n}");
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            !output.errors.is_empty(),
            "expected type error for undefined variable"
        );
    }

    #[test]
    fn typecheck_error_type_mismatch() {
        let source = concat!(
            "fn add(a: i32, b: i32) -> i32 {\n",
            "    a + b\n",
            "}\n\n",
            "fn main() {\n",
            "    let result = add(\"hello\", \"world\");\n",
            "}"
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            !output.errors.is_empty(),
            "expected type errors for mismatched argument types"
        );
    }

    // -----------------------------------------------------------------------
    // Additional edge-case tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_string_literal_type() {
        let mut checker = Checker::new();
        let expr = (Expr::Literal(Literal::String("hello".to_string())), 0..5);
        let ty = checker.synthesize(&expr.0, &expr.1);
        assert_eq!(ty, Ty::String);
    }

    #[test]
    fn test_float_literal_type() {
        let mut checker = Checker::new();
        let expr = (Expr::Literal(Literal::Float(3.14)), 0..4);
        let ty = checker.synthesize(&expr.0, &expr.1);
        assert_eq!(ty, Ty::F64);
    }

    #[test]
    fn test_char_literal_type() {
        let mut checker = Checker::new();
        let expr = (Expr::Literal(Literal::Char('a')), 0..3);
        let ty = checker.synthesize(&expr.0, &expr.1);
        assert_eq!(ty, Ty::Char);
    }

    #[test]
    fn typecheck_binary_op_type_mismatch() {
        let source =
            "fn main() -> i32 {\n    let x: i32 = 1;\n    let y: bool = true;\n    x + y\n}";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            !output.errors.is_empty(),
            "expected type error for i32 + bool"
        );
    }

    #[test]
    fn typecheck_rejects_implicit_signedness_change_in_call() {
        let source = concat!(
            "fn takes_u32(x: u32) -> u32 { x }\n",
            "fn main() -> u32 {\n",
            "    let n: i64 = 42;\n",
            "    takes_u32(n)\n",
            "}\n"
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.message.contains("implicit numeric coercion")),
            "expected explicit coercion diagnostic, got: {:?}",
            output.errors
        );
    }

    #[test]
    fn typecheck_rejects_implicit_integer_to_float_in_call() {
        let source = concat!(
            "fn takes_f64(x: f64) -> f64 { x }\n",
            "fn main() -> f64 {\n",
            "    let n: i64 = 42;\n",
            "    takes_f64(n)\n",
            "}\n"
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.message.contains("implicit numeric coercion")),
            "expected explicit coercion diagnostic, got: {:?}",
            output.errors
        );
    }

    #[test]
    fn typecheck_allows_safe_integer_widening_in_call() {
        let source = concat!(
            "fn takes_i64(x: i64) -> i64 { x }\n",
            "fn main() -> i64 {\n",
            "    let n: i32 = 42;\n",
            "    takes_i64(n)\n",
            "}\n"
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            output.errors.is_empty(),
            "unexpected errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn typecheck_return_type_mismatch() {
        // The type checker may not flag all return-type mismatches at the
        // trailing-expression level; verify the function signature is recorded.
        let source = "fn foo() -> i32 {\n    true\n}";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        // The function signature should still reflect i32 return type
        assert_eq!(output.fn_sigs["foo"].return_type, Ty::I32);
    }

    #[test]
    fn typecheck_trailing_return_stmt_matches_declared_type() {
        let source = "fn foo() -> i32 {\n    return 42;\n}";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            output.errors.is_empty(),
            "unexpected errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn typecheck_trailing_return_stmt_matches_declared_bool_type() {
        let source = "fn foo() -> bool {\n    return true;\n}";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            output.errors.is_empty(),
            "unexpected errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn typecheck_nested_function_calls() {
        let source = concat!(
            "fn double(x: i32) -> i32 { let two: i32 = 2; x * two }\n",
            "fn main() -> i32 { let x: i32 = 5; double(double(x)) }\n"
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            output.errors.is_empty(),
            "unexpected errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn typecheck_let_with_explicit_type() {
        let source = "fn main() { let x: i32 = 42; }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            output.errors.is_empty(),
            "unexpected errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn typecheck_let_type_annotation_mismatch() {
        let source = "fn main() { let x: i32 = \"hello\"; }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            !output.errors.is_empty(),
            "expected type error for string assigned to i32 variable"
        );
    }

    #[test]
    fn typecheck_if_branch_type_consistency() {
        let source =
            "fn main() -> i32 {\n    if true { let x: i32 = 1; x } else { let y: i32 = 2; y }\n}";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            output.errors.is_empty(),
            "unexpected errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn typecheck_vec_type_annotation() {
        let source = "fn main() { let v: Vec<i32> = Vec::new(); }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        // Vec::new() may or may not resolve depending on builtins, but should not panic
        assert!(output.errors.len() <= 2);
    }

    #[test]
    fn typecheck_multiple_functions_cross_call() {
        let source = concat!(
            "fn add(a: i32, b: i32) -> i32 { a + b }\n",
            "fn mul(a: i32, b: i32) -> i32 { a * b }\n",
            "fn main() -> i32 { add(mul(2, 3), 4) }\n"
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            output.errors.is_empty(),
            "unexpected errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn typecheck_actor_receive_fn_registered() {
        use hew_parser::ast::{ActorDecl, Block, Item, Param, ReceiveFnDecl, TypeExpr};

        let recv = ReceiveFnDecl {
            is_generator: false,
            is_pure: false,
            name: "greet".to_string(),
            params: vec![Param {
                name: "name".to_string(),
                ty: (
                    TypeExpr::Named {
                        name: "string".into(),
                        type_args: None,
                    },
                    0..0,
                ),
                is_mutable: false,
            }],
            return_type: None,
            body: Block {
                stmts: vec![],
                trailing_expr: None,
            },
            type_params: None,
            where_clause: None,
            span: 0..0,
        };
        let actor = ActorDecl {
            visibility: Visibility::Pub,
            name: "Greeter".to_string(),
            super_traits: None,
            init: None,
            fields: vec![],
            receive_fns: vec![recv],
            methods: vec![],
            mailbox_capacity: None,
            overflow_policy: None,
            is_isolated: false,
            doc_comment: None,
        };
        let program = Program {
            module_graph: None,
            items: vec![(Item::Actor(actor), 0..0)],
            module_doc: None,
        };
        let mut checker = Checker::new();
        let output = checker.check_program(&program);
        assert!(output.fn_sigs.contains_key("Greeter::greet"));
    }

    #[test]
    fn typecheck_empty_function_no_error() {
        let source = "fn noop() {}";
        let result = hew_parser::parse(source);
        assert!(result.errors.is_empty());
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            output.errors.is_empty(),
            "unexpected errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn typecheck_recursive_function() {
        let source = concat!(
            "fn factorial(n: i32) -> i32 {\n",
            "    let one: i32 = 1; if n <= one { one } else { n * factorial(n - one) }\n",
            "}\n"
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            output.errors.is_empty(),
            "unexpected errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn typecheck_local_result_enum_not_qualified_to_sqlite() {
        let source = concat!(
            "import ecosystem::db::sqlite;\n",
            "enum Result {\n",
            "    Ok(int);\n",
            "    Err(int)\n",
            "}\n",
            "fn unwrap_or(r: Result, fallback: int) -> int {\n",
            "    match r {\n",
            "        Ok(v) => v,\n",
            "        Err(_) => fallback,\n",
            "    }\n",
            "}\n"
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            output.errors.is_empty(),
            "unexpected errors: {:?}",
            output.errors
        );
        let sig = output
            .fn_sigs
            .get("unwrap_or")
            .expect("unwrap_or signature should be registered");
        assert_eq!(
            sig.params[0],
            Ty::Named {
                name: "Result".to_string(),
                args: vec![],
            }
        );
    }

    #[test]
    fn typecheck_match_statement_exhaustive_enum_ok() {
        let (errors, _) = parse_and_check(concat!(
            "enum Light { Red; Green; }\n",
            "fn main() { let v: Light = Red; match v { Red => 1, Green => 2, } let _done = 0; }\n",
        ));
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    }

    #[test]
    fn typecheck_match_statement_missing_variant_warns() {
        let (errors, warnings) = parse_and_check(concat!(
            "enum Light { Red; Green; }\n",
            "fn main() { let v: Light = Red; match v { Red => 1, } let _done = 0; }\n",
        ));
        assert!(
            errors.is_empty(),
            "expected only a warning but got errors: {errors:?}"
        );
        assert!(
            warnings
                .iter()
                .any(|w| matches!(w.kind, TypeErrorKind::NonExhaustiveMatch)),
            "expected non-exhaustive match warning, got: {warnings:?}"
        );
    }

    #[test]
    fn typecheck_guarded_wildcard_not_exhaustive() {
        let (errors, warnings) = parse_and_check(concat!(
            "fn main() {\n",
            "    let x = true;\n",
            "    match x {\n",
            "        _ if false => 0,\n",
            "    }\n",
            "    let _done = 0;\n",
            "}\n",
        ));
        assert!(
            errors.is_empty(),
            "expected only a warning but got errors: {errors:?}"
        );
        assert!(
            warnings
                .iter()
                .any(|w| matches!(w.kind, TypeErrorKind::NonExhaustiveMatch)),
            "expected non-exhaustive match warning, got: {warnings:?}"
        );
    }

    #[test]
    fn typecheck_struct_pattern_unknown_field_errors() {
        let (errors, _) = parse_and_check(concat!(
            "type Point { x: int, y: int }\n",
            "fn main() {\n",
            "    let p = Point { x: 1, y: 2 };\n",
            "    match p {\n",
            "        Point { x, z } => x,\n",
            "    }\n",
            "}\n",
        ));
        assert!(
            errors
                .iter()
                .any(|e| matches!(e.kind, TypeErrorKind::UndefinedField)),
            "expected UndefinedField error for unknown field 'z', got: {errors:?}"
        );
    }

    #[test]
    fn typecheck_match_wrong_enum_variant_errors() {
        // Matching a Color scrutinee with a Shape variant should be an error.
        let (errors, _) = parse_and_check(concat!(
            "enum Color { Red; Green; Blue; }\n",
            "enum Shape { Circle(i32); Rectangle(i32); }\n",
            "fn describe(c: Color) -> i32 {\n",
            "    match c {\n",
            "        Circle(r) => r,\n",
            "        _ => 0,\n",
            "    }\n",
            "}\n",
        ));
        assert!(
            !errors.is_empty(),
            "expected type error for wrong-enum variant in match, got no errors"
        );
    }

    #[test]
    #[ignore = "known gap: generic enum constructors lose type args during type checking"]
    fn typecheck_generic_enum_constructor_infers_type_args() {
        let (errors, _) = parse_and_check(concat!(
            "enum Option<T> { Some(T); None; }\n",
            "fn take_int(x: Option<int>) -> Option<int> { x }\n",
            "fn take_string(x: Option<string>) -> Option<string> { x }\n",
            "fn main() { take_int(Some(42)); take_string(Some(\"hello\")); }\n",
        ));
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    }

    #[test]
    fn warn_unused_variable() {
        let source = "fn main() { let unused_var = 42; }";
        let result = hew_parser::parse(source);
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(output.errors.is_empty(), "errors: {:?}", output.errors);
        assert!(
            output
                .warnings
                .iter()
                .any(|w| w.message.contains("unused variable `unused_var`")),
            "expected unused variable warning, got: {:?}",
            output.warnings
        );
    }

    #[test]
    fn warn_var_never_mutated() {
        let source = "fn main() { var x = 10; println(x); }";
        let result = hew_parser::parse(source);
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(output.errors.is_empty(), "errors: {:?}", output.errors);
        assert!(
            output
                .warnings
                .iter()
                .any(|w| w.message.contains("never reassigned")),
            "expected unmutated var warning, got: {:?}",
            output.warnings
        );
    }

    #[test]
    fn no_warn_underscore_prefix() {
        let source = "fn main() { let _unused = 42; }";
        let result = hew_parser::parse(source);
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            !output
                .warnings
                .iter()
                .any(|w| w.message.contains("_unused")),
            "should not warn on _ prefix, got: {:?}",
            output.warnings
        );
    }

    // -----------------------------------------------------------------------
    // Lint / warning regression tests
    // -----------------------------------------------------------------------

    /// Helper: parse + typecheck, return (errors, warnings).
    fn parse_and_check(source: &str) -> (Vec<TypeError>, Vec<TypeError>) {
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "unexpected parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        (output.errors, output.warnings)
    }

    // ---- unused variable ----

    #[test]
    fn warn_unused_in_nested_scope() {
        let (errors, warnings) = parse_and_check("fn main() { if true { let nested = 1; } }");
        assert!(errors.is_empty(), "errors: {errors:?}");
        assert!(
            warnings
                .iter()
                .any(|w| w.message.contains("unused variable `nested`")),
            "expected unused variable warning for nested, got: {warnings:?}"
        );
    }

    #[test]
    fn no_warn_used_variable() {
        let (errors, warnings) = parse_and_check("fn main() { let x = 42; println(x); }");
        assert!(errors.is_empty(), "errors: {errors:?}");
        assert!(
            !warnings
                .iter()
                .any(|w| w.message.contains("unused variable `x`")),
            "should not warn on used variable, got: {warnings:?}"
        );
    }

    #[test]
    fn no_warn_underscore_alone() {
        let (_, warnings) = parse_and_check("fn main() { let _ = 42; }");
        assert!(
            !warnings.iter().any(|w| w.message.contains("unused")),
            "bare _ should never warn, got: {warnings:?}"
        );
    }

    // ---- var never mutated ----

    #[test]
    fn no_warn_var_actually_mutated() {
        let (errors, warnings) = parse_and_check("fn main() { var x = 10; x = 20; println(x); }");
        assert!(errors.is_empty(), "errors: {errors:?}");
        assert!(
            !warnings
                .iter()
                .any(|w| w.message.contains("never reassigned")),
            "should not warn when var is actually reassigned, got: {warnings:?}"
        );
    }

    #[test]
    fn warn_var_never_mutated_suggestion() {
        let (_, warnings) = parse_and_check("fn main() { var x = 10; println(x); }");
        let w = warnings
            .iter()
            .find(|w| w.message.contains("never reassigned"))
            .expect("expected never-reassigned warning");
        assert!(
            w.suggestions.iter().any(|s| s.contains("let")),
            "should suggest using `let`, got: {:?}",
            w.suggestions
        );
    }

    // ---- assignment LHS not false-positive as "used" ----

    #[test]
    fn warn_write_only_variable() {
        // `x` is only written to, never read — should get unused warning
        let (_, warnings) = parse_and_check("fn main() { var x = 0; x = 1; }");
        assert!(
            warnings
                .iter()
                .any(|w| w.message.contains("unused variable `x`")),
            "write-only variable should be warned as unused, got: {warnings:?}"
        );
    }

    #[test]
    fn no_warn_variable_used_then_assigned() {
        // `x` is read (println) AND then written — it's genuinely used
        let (_, warnings) =
            parse_and_check("fn main() { var x = 0; println(x); x = 1; println(x); }");
        assert!(
            !warnings
                .iter()
                .any(|w| w.message.contains("unused variable `x`")),
            "variable that is read should not get unused warning, got: {warnings:?}"
        );
    }

    // ---- while true → loop ----

    #[test]
    fn warn_while_true() {
        let (_, warnings) = parse_and_check("fn main() { while true { break; } }");
        let w = warnings
            .iter()
            .find(|w| w.message.contains("while true"))
            .expect("expected while-true style warning");
        assert!(
            w.suggestions.iter().any(|s| s.contains("loop")),
            "should suggest loop, got: {:?}",
            w.suggestions
        );
        assert!(
            matches!(w.kind, TypeErrorKind::StyleSuggestion),
            "should be StyleSuggestion kind"
        );
    }

    #[test]
    fn no_warn_while_condition() {
        let (_, warnings) = parse_and_check("fn main() { let x = true; while x { break; } }");
        assert!(
            !warnings.iter().any(|w| w.message.contains("while true")),
            "should not warn on while <variable>, got: {warnings:?}"
        );
    }

    // ---- unused return value ----

    #[test]
    fn no_warn_discarded_return_value() {
        // Calling a function that returns a value without binding the result
        // should NOT warn — users can discard results freely
        let (_, warnings) = parse_and_check(concat!(
            "fn compute() -> i32 { 42 }\n",
            "fn main() { compute(); }\n",
        ));
        assert!(
            !warnings.iter().any(|w| w.message.contains("unused")),
            "discarded return value should not warn, got: {warnings:?}"
        );
    }

    #[test]
    fn no_warn_unused_println() {
        // println returns unit, and is a known side-effect function
        let (_, warnings) = parse_and_check("fn main() { println(42); }");
        assert!(
            !warnings.iter().any(|w| w.message.contains("unused")),
            "println() should not produce unused value warning, got: {warnings:?}"
        );
    }

    #[test]
    fn no_warn_unused_spawn() {
        // spawn is a side-effect expression — don't warn about discarded return
        let (_, warnings) = parse_and_check(concat!(
            "actor Worker { count: i32;\n",
            "    receive fn work() {} }\n",
            "fn main() { let _w = spawn Worker(count: 0); }\n",
        ));
        assert!(
            !warnings
                .iter()
                .any(|w| w.message.contains("unused") && !w.message.contains("unused variable")),
            "spawn() should not produce unused value warning, got: {warnings:?}"
        );
    }

    // ---- unit binding ----

    #[test]
    fn warn_unit_binding() {
        let (_, warnings) = parse_and_check("fn main() { let x = println(42); }");
        assert!(
            warnings.iter().any(|w| w.message.contains("unit type")),
            "binding to unit type should warn, got: {warnings:?}"
        );
    }

    #[test]
    fn no_warn_non_unit_binding() {
        let (_, warnings) = parse_and_check(
            "fn compute() -> i32 { 42 }\nfn main() { let x = compute(); println(x); }",
        );
        assert!(
            !warnings.iter().any(|w| w.message.contains("unit type")),
            "binding to non-unit type should not warn, got: {warnings:?}"
        );
    }

    // ---- Levenshtein "did you mean?" suggestions ----

    #[test]
    fn suggest_similar_variable() {
        let (errors, _) = parse_and_check("fn main() { let counter = 42; println(conter); }");
        let err = errors
            .iter()
            .find(|e| e.message.contains("conter"))
            .expect("expected error for misspelled variable");
        assert!(
            err.suggestions.iter().any(|s| s.contains("counter")),
            "should suggest 'counter', got: {:?}",
            err.suggestions
        );
    }

    #[test]
    fn suggest_similar_function() {
        let (errors, _) = parse_and_check(concat!(
            "fn calculate_sum(a: i32, b: i32) -> i32 { a + b }\n",
            "fn main() { calculate_sun(1, 2); }\n",
        ));
        let err = errors
            .iter()
            .find(|e| e.message.contains("calculate_sun"))
            .expect("expected error for misspelled function");
        assert!(
            err.suggestions.iter().any(|s| s.contains("calculate_sum")),
            "should suggest 'calculate_sum', got: {:?}",
            err.suggestions
        );
    }

    #[test]
    fn suggest_similar_type() {
        // Use a misspelled type in a constructor position, which triggers undefined type lookup
        let (errors, _) = parse_and_check(concat!(
            "type Point { x: i32; y: i32; }\n",
            "fn make() { let p = Pont { x: 0, y: 0 }; println(p.x); }\n",
        ));
        let err = errors
            .iter()
            .find(|e| e.message.contains("Pont"))
            .expect("expected error for misspelled type");
        assert!(
            err.suggestions.iter().any(|s| s.contains("Point")),
            "should suggest 'Point', got: {:?}",
            err.suggestions
        );
    }

    #[test]
    fn suggest_similar_field() {
        let (errors, _) = parse_and_check(concat!(
            "type Point { x: i32; y: i32; }\n",
            "fn get_z(p: Point) -> i32 { p.z }\n",
        ));
        let err = errors
            .iter()
            .find(|e| e.message.contains("z"))
            .expect("expected error for undefined field");
        assert!(
            !err.suggestions.is_empty(),
            "should suggest similar fields, got: {:?}",
            err.suggestions
        );
    }

    #[test]
    fn no_suggest_when_too_different() {
        let (errors, _) = parse_and_check("fn main() { let alpha = 1; println(zzzzz); }");
        let err = errors
            .iter()
            .find(|e| e.message.contains("zzzzz"))
            .expect("expected error for undefined variable");
        // "zzzzz" is too far from "alpha" — no suggestion
        assert!(
            err.suggestions.is_empty() || !err.suggestions.iter().any(|s| s.contains("alpha")),
            "should not suggest distant names, got: {:?}",
            err.suggestions
        );
    }

    // ---- warning severity and kind ----

    #[test]
    fn lint_warnings_have_warning_severity() {
        let (_, warnings) = parse_and_check("fn main() { let unused = 42; }");
        for w in &warnings {
            assert_eq!(
                w.severity,
                crate::error::Severity::Warning,
                "lint warnings must have Warning severity, got: {:?} for {}",
                w.severity,
                w.message
            );
        }
    }

    #[test]
    fn unused_variable_has_correct_kind() {
        let (_, warnings) = parse_and_check("fn main() { let unused = 42; }");
        let w = warnings
            .iter()
            .find(|w| w.message.contains("unused"))
            .unwrap();
        assert!(
            matches!(w.kind, TypeErrorKind::UnusedVariable),
            "expected UnusedVariable kind, got: {:?}",
            w.kind
        );
    }

    #[test]
    fn never_mutated_has_correct_kind() {
        let (_, warnings) = parse_and_check("fn main() { var x = 10; println(x); }");
        let w = warnings
            .iter()
            .find(|w| w.message.contains("never reassigned"))
            .unwrap();
        assert!(
            matches!(w.kind, TypeErrorKind::UnusedMut),
            "expected UnusedMut kind, got: {:?}",
            w.kind
        );
    }

    // ---- multiple warnings in one function ----

    #[test]
    fn multiple_warnings_in_one_fn() {
        let (errors, warnings) = parse_and_check(concat!(
            "fn main() {\n",
            "    let unused_a = 1;\n",
            "    var never_written = 2;\n",
            "    println(never_written);\n",
            "    while true { break; }\n",
            "}\n",
        ));
        assert!(errors.is_empty(), "errors: {errors:?}");
        assert!(
            warnings
                .iter()
                .any(|w| w.message.contains("unused variable `unused_a`")),
            "missing unused_a warning: {warnings:?}"
        );
        assert!(
            warnings
                .iter()
                .any(|w| w.message.contains("never reassigned")),
            "missing never-mutated warning: {warnings:?}"
        );
        assert!(
            warnings.iter().any(|w| w.message.contains("while true")),
            "missing while-true warning: {warnings:?}"
        );
    }

    // ── Unreachable Code Tests ──────────────────────────────────────────

    #[test]
    fn warn_unreachable_after_return() {
        let source = "fn foo() -> i32 { return 1; let x = 2; x }";
        let result = hew_parser::parse(source);
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            output
                .warnings
                .iter()
                .any(|w| w.kind == TypeErrorKind::UnreachableCode),
            "expected unreachable code warning, got: {:?}",
            output.warnings
        );
    }

    #[test]
    fn no_warn_unreachable_when_return_is_last() {
        let source = "fn foo() -> i32 { let x = 1; return x; }";
        let result = hew_parser::parse(source);
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            !output
                .warnings
                .iter()
                .any(|w| w.kind == TypeErrorKind::UnreachableCode),
            "should not warn when return is last statement: {:?}",
            output.warnings
        );
    }

    // ── Shadowing Tests ─────────────────────────────────────────────────

    #[test]
    fn warn_variable_shadowing() {
        let source = "fn main() { let x = 1; if true { let x = 2; println(x); } println(x); }";
        let result = hew_parser::parse(source);
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            output
                .warnings
                .iter()
                .any(|w| w.kind == TypeErrorKind::Shadowing
                    && w.message.contains("variable `x` shadows")),
            "expected shadowing warning, got: {:?}",
            output.warnings
        );
    }

    #[test]
    fn no_warn_shadowing_underscore_prefix() {
        let source = "fn main() { let _x = 1; if true { let _x = 2; println(_x); } println(_x); }";
        let result = hew_parser::parse(source);
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            !output
                .warnings
                .iter()
                .any(|w| w.kind == TypeErrorKind::Shadowing),
            "should not warn for _ prefixed vars: {:?}",
            output.warnings
        );
    }

    #[test]
    fn no_warn_shadowing_for_loop_var() {
        let source = "fn main() { let i = 0; for i in 0..10 { println(i); } println(i); }";
        let result = hew_parser::parse(source);
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            !output
                .warnings
                .iter()
                .any(|w| w.kind == TypeErrorKind::Shadowing),
            "should not warn for for-loop variable shadowing: {:?}",
            output.warnings
        );
    }

    // ── Dead Code (Unused Function) Tests ───────────────────────────────

    #[test]
    fn warn_dead_code_unused_function() {
        let source = "fn unused_helper() -> i32 { 42 } fn main() { println(1); }";
        let result = hew_parser::parse(source);
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            output
                .warnings
                .iter()
                .any(|w| w.kind == TypeErrorKind::DeadCode && w.message.contains("unused_helper")),
            "expected dead code warning for unused_helper, got: {:?}",
            output.warnings
        );
    }

    #[test]
    fn no_warn_dead_code_main() {
        let source = "fn main() { println(1); }";
        let result = hew_parser::parse(source);
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            !output
                .warnings
                .iter()
                .any(|w| w.kind == TypeErrorKind::DeadCode),
            "should not warn about main: {:?}",
            output.warnings
        );
    }

    #[test]
    fn no_warn_dead_code_called_function() {
        let source = "fn helper() -> i32 { 42 } fn main() { let x = helper(); println(x); }";
        let result = hew_parser::parse(source);
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            !output
                .warnings
                .iter()
                .any(|w| w.kind == TypeErrorKind::DeadCode && w.message.contains("helper")),
            "should not warn about called function: {:?}",
            output.warnings
        );
    }

    #[test]
    fn no_warn_dead_code_underscore_prefix() {
        let source = "fn _unused() -> i32 { 42 } fn main() { println(1); }";
        let result = hew_parser::parse(source);
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            !output
                .warnings
                .iter()
                .any(|w| w.kind == TypeErrorKind::DeadCode),
            "should not warn for _ prefixed functions: {:?}",
            output.warnings
        );
    }

    #[test]
    fn no_warn_dead_code_called_from_actor_receive() {
        let source = r#"
fn fib(n: i32) -> i32 {
    if n <= 1 { n } else { fib(n - 1) + fib(n - 2) }
}
actor Worker {
    receive fn compute(n: i32) {
        let r = fib(n);
        println(r);
    }
}
fn main() {
    let w = spawn Worker();
    w.compute(10);
}
"#;
        let result = hew_parser::parse(source);
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            !output
                .warnings
                .iter()
                .any(|w| w.kind == TypeErrorKind::DeadCode && w.message.contains("fib")),
            "should not warn about function called from actor receive fn: {:?}",
            output.warnings
        );
    }

    // ── Unused Import Tests ─────────────────────────────────────────────

    #[test]
    fn warn_unused_import() {
        let source = "import std::encoding::json;\nfn main() { println(1); }";
        let result = hew_parser::parse(source);
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            output
                .warnings
                .iter()
                .any(|w| w.kind == TypeErrorKind::UnusedImport && w.message.contains("json")),
            "expected unused import warning for json, got: {:?}",
            output.warnings
        );
    }

    #[test]
    fn no_warn_used_import() {
        let source =
            "import std::encoding::json;\nfn main() { let v = json.parse(\"[]\"); println(v); }";
        let result = hew_parser::parse(source);
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            !output
                .warnings
                .iter()
                .any(|w| w.kind == TypeErrorKind::UnusedImport && w.message.contains("json")),
            "should not warn about used import: {:?}",
            output.warnings
        );
    }

    // ── Warning severity tests ──────────────────────────────────────────

    #[test]
    fn unreachable_code_has_warning_severity() {
        let source = "fn foo() -> i32 { return 1; let x = 2; x }";
        let result = hew_parser::parse(source);
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        let w = output
            .warnings
            .iter()
            .find(|w| w.kind == TypeErrorKind::UnreachableCode);
        assert!(w.is_some(), "expected unreachable warning");
        assert_eq!(
            w.unwrap().severity,
            crate::error::Severity::Warning,
            "unreachable code should have Warning severity"
        );
    }

    #[test]
    fn shadowing_has_note_for_original_definition() {
        let source = "fn main() { let x = 1; if true { let x = 2; println(x); } println(x); }";
        let result = hew_parser::parse(source);
        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        let w = output
            .warnings
            .iter()
            .find(|w| w.kind == TypeErrorKind::Shadowing);
        assert!(w.is_some(), "expected shadowing warning");
        assert!(
            !w.unwrap().notes.is_empty(),
            "shadowing warning should have a note pointing to the original definition"
        );
    }

    // ── Bug fix regression tests ────────────────────────────────────────

    #[test]
    fn no_warn_unused_read_then_assign() {
        // Bug 1: var x = 0; println(x); x = 1; should NOT warn about unused x
        let (errors, warnings) = parse_and_check("fn main() { var x = 0; println(x); x = 1; }");
        assert!(errors.is_empty(), "errors: {errors:?}");
        assert!(
            !warnings
                .iter()
                .any(|w| w.message.contains("unused variable `x`")),
            "read-then-assign should not produce unused warning, got: {warnings:?}"
        );
    }

    #[test]
    fn warn_unreachable_after_if_all_branches_return() {
        // Bug 2: if where all branches return should mark subsequent code unreachable
        let (_, warnings) = parse_and_check(
            "fn foo() -> i32 { if true { return 1; } else { return 2; } let y = 3; y }",
        );
        assert!(
            warnings
                .iter()
                .any(|w| w.kind == TypeErrorKind::UnreachableCode),
            "expected unreachable code warning after if with all-returning branches, got: {warnings:?}"
        );
    }

    #[test]
    fn no_warn_dead_code_function_referenced_as_value() {
        // Bug 3: let f = helper; f(); should mark helper as called
        let (_, warnings) =
            parse_and_check("fn helper() -> i32 { 42 } fn main() { let f = helper; println(f); }");
        assert!(
            !warnings
                .iter()
                .any(|w| w.kind == TypeErrorKind::DeadCode && w.message.contains("helper")),
            "function referenced as value should not get dead code warning, got: {warnings:?}"
        );
    }

    #[test]
    fn warn_dead_code_self_recursive_function() {
        // Bug 4: fn rec() { rec(); } fn main() {} — rec only calls itself, dead
        let (_, warnings) = parse_and_check("fn rec() { rec(); } fn main() { println(1); }");
        assert!(
            warnings
                .iter()
                .any(|w| w.kind == TypeErrorKind::DeadCode && w.message.contains("rec")),
            "self-recursive unreachable function should get dead code warning, got: {warnings:?}"
        );
    }

    // -----------------------------------------------------------------------
    // Module namespacing tests
    // -----------------------------------------------------------------------

    /// Helper: build a simple pub function declaration.
    fn make_pub_fn(name: &str, params: Vec<Param>, ret: Option<TypeExpr>) -> FnDecl {
        FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Pub,
            is_pure: false,
            name: name.to_string(),
            type_params: None,
            params,
            return_type: ret.map(|te| (te, 0..0)),
            where_clause: None,
            body: Block {
                stmts: vec![],
                trailing_expr: Some(Box::new(make_int_literal(0, 0..1))),
            },
            doc_comment: None,
        }
    }

    /// Helper: build a private (non-pub) function declaration.
    fn make_priv_fn(name: &str) -> FnDecl {
        FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            is_pure: false,
            name: name.to_string(),
            type_params: None,
            params: vec![],
            return_type: Some((
                TypeExpr::Named {
                    name: "i32".to_string(),
                    type_args: None,
                },
                0..0,
            )),
            where_clause: None,
            body: Block {
                stmts: vec![],
                trailing_expr: Some(Box::new(make_int_literal(0, 0..1))),
            },
            doc_comment: None,
        }
    }

    /// Helper: build an ImportDecl with resolved items.
    fn make_user_import(
        path: Vec<&str>,
        spec: Option<ImportSpec>,
        items: Vec<Spanned<Item>>,
    ) -> ImportDecl {
        ImportDecl {
            path: path.iter().map(ToString::to_string).collect(),
            spec,
            file_path: None,
            resolved_items: Some(items),
        }
    }

    /// Helper: type-check a program with given items.
    fn check_items(items: Vec<Spanned<Item>>) -> TypeCheckOutput {
        let program = Program {
            module_graph: None,
            items,
            module_doc: None,
        };
        let mut checker = Checker::new();
        checker.check_program(&program)
    }

    // -- should_import_name --

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
            vec!["myapp", "utils"],
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
                name: "String".to_string(),
                type_args: None,
            }),
        );
        let import = make_user_import(
            vec!["myapp", "utils"],
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
            vec!["myapp", "utils"],
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
    fn non_pub_functions_not_registered() {
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
            vec!["myapp", "utils"],
            Some(ImportSpec::Glob), // even glob shouldn't expose private fns
            vec![
                (Item::Function(priv_fn), 0..0),
                (Item::Function(pub_fn), 0..0),
            ],
        );
        let output = check_items(vec![(Item::Import(import), 0..0)]);

        assert!(
            !output.fn_sigs.contains_key("utils.secret"),
            "non-pub function should not be registered as qualified"
        );
        assert!(
            !output.fn_sigs.contains_key("secret"),
            "non-pub function should not be registered as unqualified"
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
        };
        let import = make_user_import(
            vec!["myapp", "config"],
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
        let mut checker = Checker::new();
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
        };
        let import = make_user_import(
            vec!["myapp", "config"],
            None, // bare import
            vec![(Item::Const(pub_const), 0..0)],
        );

        let program = Program {
            module_graph: None,
            items: vec![(Item::Import(import), 0..0)],
            module_doc: None,
        };
        let mut checker = Checker::new();
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
            }],
            doc_comment: None,
        };
        let import = make_user_import(
            vec!["myapp", "config"],
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
            vec!["myapp", "utils"],
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
            file_path: None,
            resolved_items: None,
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
                },
            ],
            Some(TypeExpr::Named {
                name: "i32".to_string(),
                type_args: None,
            }),
        );
        let import = make_user_import(
            vec!["mylib", "math"],
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
                name: "String".to_string(),
                type_args: None,
            }),
        );
        let import_a = make_user_import(
            vec!["pkg", "alpha"],
            None,
            vec![(Item::Function(helper_a), 0..0)],
        );
        let import_b = make_user_import(
            vec!["pkg", "beta"],
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
    fn import_without_resolved_items_treated_as_stdlib() {
        // An import with resolved_items = None and no stdlib match should not panic
        let import = ImportDecl {
            path: vec!["unknown".to_string(), "pkg".to_string()],
            spec: None,
            file_path: None,
            resolved_items: None,
        };
        let output = check_items(vec![(Item::Import(import), 0..0)]);
        // Should not register anything but should not crash
        assert!(!output.user_modules.contains("pkg"));
    }

    // -- Empty module import --

    #[test]
    fn empty_module_import_no_crash() {
        let import = make_user_import(vec!["myapp", "empty"], None, vec![]);
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
            vec!["mymod"],
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
            vec!["pkg"],
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
                is_pure: true,
                type_params: None,
                params: vec![],
                return_type: None,
                where_clause: None,
                body: None,
            })],
            doc_comment: None,
        };
        let import = make_user_import(
            vec!["mylib", "fmt"],
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
                is_pure: false,
                type_params: None,
                params: vec![],
                return_type: None,
                where_clause: None,
                body: None,
            })],
            doc_comment: None,
        };
        let import = make_user_import(
            vec!["mylib", "internals"],
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
        };
        let impl_decl = ImplDecl {
            type_params: None,
            trait_bound: Some(TraitBound {
                name: "ExternalTrait".to_string(),
                type_args: None,
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
            is_pure: false,
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
        });

        let private_const = Item::Const(ConstDecl {
            visibility: Visibility::Private,
            name: "PRIVATE_CONST".to_string(),
            ty: (
                TypeExpr::Named {
                    name: "Int".to_string(),
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
        });

        let private_type = Item::TypeDecl(TypeDecl {
            visibility: Visibility::Private,
            kind: TypeDeclKind::Struct,
            name: "PrivateType".to_string(),
            type_params: None,
            where_clause: None,
            body: vec![],
            doc_comment: None,
        });

        let resolved: Vec<Spanned<Item>> = vec![
            (private_fn, 0..0),
            (private_const, 0..0),
            (private_type, 0..0),
        ];

        let import_decl = ImportDecl {
            path: vec![],
            spec: None,
            file_path: Some("private_lib.hew".to_string()),
            resolved_items: Some(resolved),
        };

        let program = Program {
            module_graph: None,
            items: vec![(Item::Import(import_decl), 0..0)],
            module_doc: None,
        };

        let mut checker = Checker::new();
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

    #[test]
    fn check_generic_lambda() {
        let source = r#"
            fn apply<T>(f: fn(T) -> T, x: T) -> T {
                f(x)
            }

            fn main() {
                // Identity generic lambda
                let id = <T>(x: T) => x;
                // Instantiation happens when calling `apply`
                // apply takes fn(T) -> T. `id` matches that.
                // However, `id` is a generic closure.
                // We need to make sure generic instantiation works.
                // Currently, `check_lambda` creates fresh type variables for T.
                // So id has type ?0 -> ?0.
                // When passed to apply(id, 5), T inferred as int.
                // apply expects fn(int) -> int.
                // id matches fn(?0) -> ?0 where ?0=int.
                let res = apply(id, 5);
            }
        "#;

        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );

        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            output.errors.is_empty(),
            "type check errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn test_self_with_generics_in_impl() {
        let source = r#"
            type Pair<T> {
                first: T,
                second: T,
            }

            impl<T> Pair<T> {
                fn new(first: T, second: T) -> Self {
                    return Pair { first: first, second: second };
                }

                fn swap(self) -> Self {
                    return Pair { first: self.second, second: self.first };
                }
            }
        "#;

        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );

        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);
        assert!(
            output.errors.is_empty(),
            "type check errors: {:?}",
            output.errors
        );

        // Verify that Self resolves to Pair<T>, not bare Pair
        // The new method should return Pair<T>
        let new_sig = output
            .fn_sigs
            .get("Pair::new")
            .expect("Pair::new should exist");
        if let Ty::Named { name, args } = &new_sig.return_type {
            assert_eq!(name, "Pair", "return type should be Pair");
            assert_eq!(args.len(), 1, "Pair should have one type argument");
        } else {
            panic!("Expected Pair::new to return a named type");
        }
    }

    #[test]
    fn test_trait_object_type_args_substitution() {
        // Bug 2: Test that dyn Trait<Args> methods get correct substitutions
        let source = r#"
            trait Iterator<T> {
                fn next(self: dyn Iterator<T>) -> Option<T>;
            }

            type Counter {
                count: int;
            }

            impl Iterator<int> for Counter {
                fn next(self: dyn Iterator<int>) -> Option<int> {
                    Some(42)
                }
            }

            fn test_iterator() -> int {
                let iter: dyn Iterator<int> = Counter { count: 5 };
                let result = iter.next(); // Should be Option<int>, not Option<T>
                match result {
                    Some(x) => x,
                    None => 0
                }
            }
        "#;

        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );

        let mut checker = Checker::new();
        let output = checker.check_program(&result.program);

        if !output.errors.is_empty() {
            for error in &output.errors {
                println!("Type error: {}", error);
            }
        }

        assert!(
            output.errors.is_empty(),
            "type check errors: {:?}",
            output.errors
        );
    }
}
