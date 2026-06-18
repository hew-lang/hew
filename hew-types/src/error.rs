//! Type error representation with rich diagnostics.
//!
//! This module defines the error types produced by the type checker,
//! including source locations, suggestions, and secondary notes.

use crate::ty::Ty;
use hew_parser::ast::Span;
use std::fmt;

/// Compute the Levenshtein (edit) distance between two strings.
fn levenshtein(a: &str, b: &str) -> usize {
    let (a, b) = (a.as_bytes(), b.as_bytes());
    let (m, n) = (a.len(), b.len());
    let mut prev: Vec<usize> = (0..=n).collect();
    let mut curr = vec![0; n + 1];
    for i in 1..=m {
        curr[0] = i;
        for j in 1..=n {
            let cost = usize::from(a[i - 1] != b[j - 1]);
            curr[j] = (prev[j] + 1).min(curr[j - 1] + 1).min(prev[j - 1] + cost);
        }
        std::mem::swap(&mut prev, &mut curr);
    }
    prev[n]
}

/// Find names similar to `target` from `candidates`, returning up to 3 suggestions.
///
/// A candidate is considered similar if its Levenshtein distance is at most
/// max(1, `target.len()` / 3), which scales with identifier length.
pub fn find_similar<'a, I>(target: &str, candidates: I) -> Vec<String>
where
    I: IntoIterator<Item = &'a str>,
{
    let max_dist = (target.len() / 3).max(1);
    let mut matches: Vec<(usize, String)> = candidates
        .into_iter()
        .filter(|c| *c != target)
        .filter_map(|c| {
            let d = levenshtein(target, c);
            (d <= max_dist).then(|| (d, c.to_string()))
        })
        .collect();
    matches.sort_by_key(|(d, _)| *d);
    matches.truncate(3);
    matches.into_iter().map(|(_, s)| s).collect()
}

/// Diagnostic severity level.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    /// A hard error that prevents compilation.
    Error,
    /// A warning that does not block compilation.
    Warning,
}

/// A type error with location, message, and diagnostic hints.
#[derive(Debug, Clone)]
pub struct TypeError {
    /// The severity of this diagnostic.
    pub severity: Severity,
    /// The kind of error
    pub kind: TypeErrorKind,
    /// Source location of the error
    pub span: Span,
    /// Human-readable error message
    pub message: String,
    /// Additional context with locations
    pub notes: Vec<(Span, String)>,
    /// "Did you mean?" suggestions
    pub suggestions: Vec<String>,
    /// Dotted module path of the non-root module this diagnostic originates from
    /// (e.g. `"net.http"`).  `None` means the root compilation unit.
    /// Used by the CLI to route the error to the correct source file for display.
    pub source_module: Option<String>,
}

impl TypeError {
    /// Create a new type error.
    #[must_use]
    pub fn new(kind: TypeErrorKind, span: Span, message: impl Into<String>) -> Self {
        Self {
            severity: Severity::Error,
            kind,
            span,
            message: message.into(),
            notes: Vec::new(),
            suggestions: Vec::new(),
            source_module: None,
        }
    }

    /// Attach a source module path to this diagnostic (builder).
    ///
    /// The module path is the dotted form (e.g. `"net.http"`) used by
    /// [`check_program`](crate::Checker::check_program) to tag non-root
    /// module body diagnostics.  The CLI uses it to select the right source
    /// file when rendering the diagnostic.
    #[must_use]
    pub fn with_source_module(mut self, module: impl Into<String>) -> Self {
        self.source_module = Some(module.into());
        self
    }

    /// Add a note with location.
    #[must_use]
    pub fn with_note(mut self, span: Span, note: impl Into<String>) -> Self {
        self.notes.push((span, note.into()));
        self
    }

    /// Add a suggestion.
    #[must_use]
    pub fn with_suggestion(mut self, suggestion: impl Into<String>) -> Self {
        self.suggestions.push(suggestion.into());
        self
    }

    /// Create a type mismatch error.
    #[must_use]
    pub fn mismatch(span: Span, expected: &Ty, actual: &Ty) -> Self {
        Self::new(
            TypeErrorKind::Mismatch {
                expected: expected.user_facing().to_string(),
                actual: actual.user_facing().to_string(),
            },
            span,
            format!(
                "expected `{}`, found `{}`",
                expected.user_facing(),
                actual.user_facing()
            ),
        )
    }

    /// Create an undefined function error.
    #[must_use]
    pub fn undefined_function(span: Span, name: &str) -> Self {
        Self::new(
            TypeErrorKind::UndefinedFunction,
            span,
            format!("cannot find function `{name}` in this scope"),
        )
    }

    /// Create an undefined field error.
    #[must_use]
    pub fn undefined_field(span: Span, ty: &Ty, field: &str) -> Self {
        Self::new(
            TypeErrorKind::UndefinedField,
            span,
            format!("no field `{field}` on type `{}`", ty.user_facing()),
        )
    }

    /// Create an invalid operation error.
    #[must_use]
    pub fn invalid_operation(span: Span, op: &str, ty: &Ty) -> Self {
        Self::new(
            TypeErrorKind::InvalidOperation,
            span,
            format!("cannot apply `{op}` to type `{}`", ty.user_facing()),
        )
    }

    /// Create an inference failed error.
    #[must_use]
    pub fn inference_failed(span: Span, context: &str) -> Self {
        Self::new(
            TypeErrorKind::InferenceFailed,
            span,
            format!(
                "cannot infer type{}",
                if context.is_empty() {
                    String::new()
                } else {
                    format!(" for {context}")
                }
            ),
        )
        .with_suggestion("consider adding a type annotation".to_string())
    }

    /// Create a non-exhaustive match error.
    #[must_use]
    pub fn non_exhaustive_match(span: Span, missing_patterns: &[String]) -> Self {
        let detail = if missing_patterns.is_empty() {
            "missing some patterns".to_string()
        } else {
            format!("missing {}", missing_patterns.join(", "))
        };
        Self::non_exhaustive_match_detail(span, Severity::Error, detail)
    }

    /// Create a non-exhaustive match diagnostic with an explicit severity.
    #[must_use]
    pub(crate) fn non_exhaustive_match_detail(
        span: Span,
        severity: Severity,
        detail: impl Into<String>,
    ) -> Self {
        Self {
            severity,
            kind: TypeErrorKind::NonExhaustiveMatch,
            span,
            message: format!("non-exhaustive match: {}", detail.into()),
            notes: Vec::new(),
            suggestions: Vec::new(),
            source_module: None,
        }
    }

    /// Create a duplicate definition error.
    #[must_use]
    pub fn duplicate_definition(span: Span, name: &str, prev_span: Span) -> Self {
        Self::new(
            TypeErrorKind::DuplicateDefinition,
            span,
            format!("`{name}` is defined multiple times"),
        )
        .with_note(prev_span, "previous definition here".to_string())
    }

    /// Create a mutability error.
    #[must_use]
    pub fn mutability_error(span: Span, name: &str) -> Self {
        Self::new(
            TypeErrorKind::MutabilityError,
            span,
            format!("cannot assign to immutable variable `{name}`"),
        )
        .with_suggestion(format!("consider changing this to `var {name}`"))
    }

    /// Create a mutability error for an assignment to an immutable actor
    /// state field outside `init { }`.
    ///
    /// `decl_span` is the field's declaration site; the note anchors the
    /// diagnostic there so the fix (declare the field with `var`) lands on
    /// the right line.
    #[must_use]
    pub fn immutable_field_assignment(span: Span, name: &str, decl_span: Span) -> Self {
        Self::new(
            TypeErrorKind::MutabilityError,
            span,
            format!("cannot assign to immutable field `{name}` outside `init`"),
        )
        .with_note(
            decl_span,
            format!("field `{name}` is declared immutable here"),
        )
        .with_suggestion(format!("declare the field with `var`: `var {name}: ...`"))
    }

    /// Create a return type mismatch error.
    #[must_use]
    pub fn return_type_mismatch(span: Span, expected: &Ty, actual: &Ty) -> Self {
        Self::new(
            TypeErrorKind::ReturnTypeMismatch,
            span,
            format!(
                "return type mismatch: expected `{}`, found `{}`",
                expected.user_facing(),
                actual.user_facing()
            ),
        )
    }

    /// Create a use-after-move error.
    #[must_use]
    pub fn use_after_move(span: Span, name: &str, moved_at: &Span) -> Self {
        Self::new(
            TypeErrorKind::UseAfterMove,
            span,
            format!("use of moved value `{name}`"),
        )
        .with_note(moved_at.clone(), "value was moved here")
    }

    /// Create an unresolved-import error.
    #[must_use]
    pub fn unresolved_import(span: Span, module_path: &str, detail: &str) -> Self {
        Self::new(
            TypeErrorKind::UnresolvedImport,
            span,
            format!("cannot resolve import `{module_path}`: {detail}"),
        )
    }

    /// Create an actor reference cycle warning.
    #[must_use]
    pub fn actor_ref_cycle(span: Span, cycle_desc: &str) -> Self {
        Self {
            severity: Severity::Warning,
            kind: TypeErrorKind::ActorRefCycle,
            span,
            message: format!("actor reference cycle detected: {cycle_desc}"),
            notes: Vec::new(),
            suggestions: vec![
                "consider using weak references or restructuring the supervision tree".to_string(),
            ],
            source_module: None,
        }
    }

    /// Create an infinitely-sized recursive value type error.
    #[must_use]
    pub fn recursive_value_type(
        span: Span,
        type_kind: &str,
        type_name: &str,
        member_desc: &str,
        referenced_type: &str,
    ) -> Self {
        Self::new(
            TypeErrorKind::RecursiveValueType {
                type_name: type_name.to_string(),
                referenced_type: referenced_type.to_string(),
            },
            span,
            format!(
                "{type_kind} `{type_name}` is infinitely sized: {member_desc} contains \
                 `{referenced_type}` by value; recursive value types are not supported \
                 in v0.5"
            ),
        )
        .with_suggestion("use a reference or other heap-owning indirection to break the cycle")
    }

    /// Create an or-pattern binding symmetry error.
    #[must_use]
    pub fn or_pattern_binding_mismatch(
        span: Span,
        left_span: Span,
        left_names: &[String],
        right_span: Span,
        right_names: &[String],
    ) -> Self {
        let describe_names = |names: &[String]| {
            if names.is_empty() {
                "binds no names".to_string()
            } else {
                let names = names
                    .iter()
                    .map(|name| format!("`{name}`"))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("binds {names}")
            }
        };

        Self::new(
            TypeErrorKind::OrPatternBindingMismatch,
            span,
            "or-pattern branches must bind the same names",
        )
        .with_note(
            left_span,
            format!("left branch {}", describe_names(left_names)),
        )
        .with_note(
            right_span,
            format!("right branch {}", describe_names(right_names)),
        )
    }
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)?;
        for suggestion in &self.suggestions {
            write!(f, "\n  help: {suggestion}")?;
        }
        Ok(())
    }
}

impl std::error::Error for TypeError {}

/// The specific kind of type error.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeErrorKind {
    /// Type mismatch between expected and actual types
    Mismatch {
        /// Expected type
        expected: String,
        /// Actual type found
        actual: String,
    },
    /// Variable not found in scope
    UndefinedVariable,
    /// Type not found in scope
    UndefinedType,
    /// Function not found in scope
    UndefinedFunction,
    /// Field not found on type
    UndefinedField,
    /// Method not found on type
    UndefinedMethod,
    /// A trait-bounded method call resolves to multiple distinct declaring
    /// traits after supertrait expansion and dedup, so static dispatch
    /// cannot pick one without qualification. Distinct from
    /// `UndefinedMethod`: the method is found in multiple traits, not
    /// missing. See W3.022 §4 V6 / V14.
    AmbiguousTraitMethod,
    /// An unqualified type name is exported by more than one imported module
    /// (e.g. both `std::net::smtp` and `std::net::websocket` export `Conn`),
    /// so the reference cannot bind to a single definition without a module
    /// qualifier. Distinct from `UndefinedType` (no def) and
    /// `DuplicateDefinition` (two defs in one module): the name resolves to
    /// several valid defs across modules.
    AmbiguousType,
    /// Value cannot be sent to another actor
    InvalidSend,
    /// Operation not supported for this type
    InvalidOperation,
    /// Execution-context reader used outside an actor handler context
    ContextReaderOutsideHandler,
    /// Wrong number of arguments
    ArityMismatch,
    /// Generic bounds not satisfied
    BoundsNotSatisfied,
    /// Cannot infer type
    InferenceFailed,
    /// Match expression is not exhaustive
    NonExhaustiveMatch,
    /// Name defined multiple times
    DuplicateDefinition,
    /// Assigning to immutable variable
    MutabilityError,
    /// Return statement type doesn't match function signature
    ReturnTypeMismatch,
    /// Value used after it was moved to an actor
    UseAfterMove,
    /// Yield used outside a generator function
    YieldOutsideGenerator,
    /// Actor types form a reference cycle via `ActorRef` fields
    ActorRefCycle,
    /// A value-typed enum/record/struct contains itself by value, directly or
    /// through another inline value type, making its layout infinitely sized.
    RecursiveValueType {
        /// Recursive type being rejected.
        type_name: String,
        /// Value type reached by the reported field/variant edge.
        referenced_type: String,
    },
    /// Variable defined but never used
    UnusedVariable,
    /// Variable declared `var` but never reassigned
    UnusedMut,
    /// Code style suggestion (e.g., `while true` → `loop`)
    StyleSuggestion,
    /// Imported module never referenced
    UnusedImport,
    /// `is TypeName` where the static LHS type already equals the RHS type
    /// pattern, so the comparison is a compile-time tautology that lowers
    /// to `true` and silently dead-codes the `else` branch.
    RedundantIs,
    /// Code after a `return`, `break`, or `continue` is never executed
    UnreachableCode,
    /// A variable binding shadows a binding from an outer scope
    Shadowing,
    /// A function is defined but never called
    DeadCode,
    /// Impl block violates the orphan rule: neither the type nor the trait is local
    OrphanImpl,
    /// Feature is not available on the selected compilation target
    PlatformLimitation,
    /// `#[on(upgrade)]` is parsed but rejected: it is reserved and not supported.
    OnUpgradeNotYetWired,
    /// Machine state × event exhaustiveness violation
    MachineExhaustivenessError,
    /// Import cannot be resolved: module not found or failed to parse
    UnresolvedImport,
    /// Blocking call inside an actor receive function can starve the scheduler
    BlockingCallInReceiveFn,
    /// Returning a borrowed Rc<T> parameter (or a local tainted by storing
    /// such a parameter) without cloning aliases the caller's pointer,
    /// causing a double-free.  Fail-closed: always an error.
    BorrowedParamReturn,
    /// Branches of an or-pattern bind different names.
    OrPatternBindingMismatch,
    /// Storing `Rc<T>` (or a type that transitively contains `Rc<T>`) inside
    /// a collection (`Vec`, `HashMap`, `HashSet`).  The runtime collections do
    /// not track ownership of pointer-valued elements — `hew_vec_free` does not
    /// drop elements, and `HashMap` confuses Rc pointers with strings.  Until
    /// the runtime properly manages owned-type element drops, this is unsound.
    UnsafeCollectionElement,
    /// User wrote `Task<T>` in a source type position (annotation, return type,
    /// parameter type, struct field). `Task<T>` is a compiler-internal type; it
    /// cannot be named in user source. Bindings of this type are inferred from
    /// `fork name = expr` context only.
    TaskNotNameable,
    /// An operation that requires an `unsafe { ... }` block was performed
    /// outside of one.  The `operation` field names the specific unsafe
    /// construct (e.g. `"raw pointer dereference"`, `"extern fn call"`).
    ///
    /// Envelope code: `E_M5_UNSAFE_BLOCK_REQUIRED`.
    UnsafeOperationRequiresBlock {
        /// Short identifier for the unsafe operation, e.g. `"raw pointer dereference"`.
        operation: String,
    },
    /// A raw-pointer operation was used inside `unsafe { ... }` but the
    /// compiler does not lower it to HIR/MIR/codegen.  Emitted so
    /// users get a deterministic, source-located rejection instead of a
    /// later "unsupported expression" failure deep in lowering.
    ///
    /// Envelope code: `E_M5_RAW_POINTER_OP_NOT_LOWERED`.
    RawPointerOpNotLowered {
        /// Short identifier for the raw-pointer operation, e.g.
        /// `"raw pointer dereference"`.
        operation: String,
    },
    /// A trait is used in `dyn` position but violates the v0.5 object-safety
    /// predicate.  Object safety in v0.5 rejects traits with generic methods
    /// (`fn foo<U>(self, u: U)`) and traits with `Self`-returning methods
    /// (`fn clone(self) -> Self`) at every `T → dyn Trait` coercion site.
    ///
    /// Envelope code: `E_TRAIT_NOT_OBJECT_SAFE`.
    TraitNotObjectSafe {
        /// Trait whose dyn-coercion was rejected.
        trait_name: String,
        /// Method name that broke object safety.
        method_name: String,
        /// Short reason: `"generic method"` or `"Self-returning method"`.
        reason: &'static str,
    },
    /// A trait object omits required associated-type bindings.
    ///
    /// Rust-aligned object safety requires every associated type declared by a
    /// trait to be fully projected in `dyn Trait` position, e.g.
    /// `dyn Iterator<Item = int>` instead of bare `dyn Iterator`.
    ///
    /// Envelope code: `E_MISSING_ASSOC_TYPE_BINDING`.
    MissingAssocTypeBinding {
        /// Trait whose dyn-object projection was incomplete.
        trait_name: String,
        /// Associated type names missing from the projection.
        missing: Vec<String>,
    },
    /// A closure implicitly captured a non-`Copy` binding by value.
    ///
    /// v0.5 closure captures are by value only; non-copy values must be captured
    /// with an explicit `move |...|` closure so the source binding is consumed
    /// at a visible source span.
    ClosureExplicitMoveRequired {
        /// Captured binding name.
        name: String,
        /// User-facing type of the captured binding.
        ty: String,
    },
    /// A closure capture's body-usage inference produced no resolved
    /// `ClosureCaptureMode`. This is a structural bug, not a user-code
    /// shape — the checker→MIR contract requires every fact's `mode` to
    /// be one of `Copy`/`Move`/`Borrow`/`BorrowMut` before lowering.
    /// Fail-closed defense.
    ClosureCaptureModeUnresolved {
        /// Captured binding name whose mode the checker could not classify.
        name: String,
    },
    /// A closure captures a non-`Sync` binding by mutable reference, and
    /// the closure body contains a suspend point (`await`, channel recv,
    /// fork-handle await). Hard error until a future auto-lock pass
    /// subscribes to this kind and rewrites the closure.
    NonSyncMutCaptureCrossesSuspend {
        /// Captured binding name being mutated across the suspend point.
        capture_name: String,
        /// Surface-facing label for the suspend point form ("await", "for await", …).
        suspend_kind: String,
    },
    /// The escape classifier produced no `ClosureEscapeKind` at all
    /// (distinct from returning `Escapes` conservatively). Structural
    /// bug in the classifier, not user-code shape.
    ClosureEscapeKindUnresolved,
    /// Advisory (non-blocking): a closure was conservatively classified
    /// `Escapes` and could be restructured to admit `Local`. Names the
    /// inference rule that fired so the user can see why.
    ClosureEscapeAdvisory {
        /// Surface-facing label for the rule that rejected `Local`.
        rule: String,
    },
    /// A closure attempted to capture the binding being defined by the same
    /// closure literal. Recursive closures require a fixed-point surface that
    /// v0.5 intentionally does not expose.
    RecursiveClosureUnsupported {
        /// Recursive binding name.
        name: String,
    },
    /// A regular fn-closure attempted to capture a lambda-actor handle
    /// (`Duplex<S, R>`) from an enclosing scope and call it with call syntax.
    ///
    /// Lambda-actor handles have no materialization protocol through a closure
    /// capture env: the MIR routing discriminator (`Place::LambdaActorHandle`)
    /// is attached to the spawning-scope slot, not to the env-loaded copy, and
    /// the runtime ABI (`hew_lambda_actor_send` vs `hew_duplex_send`) cannot
    /// be selected without it. Until a full env-materialization protocol exists,
    /// fn-closure capture of lambda handles must be refused at the checker
    /// boundary so the permissive MIR path (`lower_lambda_actor_call` via
    /// `capture_env_sources`) is never silently reached.
    ///
    /// Envelope code: `E_CLOSURE_CAPTURES_LAMBDA_HANDLE`.
    ClosureCapturesDuplexHandle {
        /// Name of the captured binding.
        name: String,
    },
    /// A dyn associated-type binding could not be projected from the concrete impl.
    ///
    /// Envelope code: `E_ASSOC_TYPE_PROJECTION_FAILED`.
    AssocTypeProjectionFailed {
        /// Concrete type being coerced into a dyn trait object.
        type_name: String,
        /// Trait whose associated type projection failed.
        trait_name: String,
        /// Associated type name that failed to project.
        assoc_name: String,
    },
    /// Two `receive fn`s in the same actor hash to the same `msg_id`.
    ///
    /// Emitted by [`crate::actor_protocol::ActorProtocolDescriptor::from_handlers`]
    /// when the default `SipHash-1-3` → low-32-bits derivation produces the
    /// same `msg_id` for two distinct fully-qualified handler names. The
    /// checker refuses to publish a descriptor for the offending actor,
    /// MIR/codegen never see a collided protocol, and the user is told to
    /// rename one of the handlers. `#[msg_id(N)]` pinning is mentioned in
    /// the hint but is not yet parseable surface — the later Q87 slice
    /// introduces the attribute.
    ActorProtocolCollision {
        /// Actor whose protocol could not be published.
        actor_name: String,
        /// One of the colliding handler names.
        handler_a: String,
        /// The other colliding handler name.
        handler_b: String,
        /// The 32-bit `msg_id` both handlers hashed to.
        msg_id: u32,
    },
    /// An `extern "rt"` function declaration names a symbol that is not in
    /// the `stable` section of `scripts/jit-symbol-classification.toml`.
    ///
    /// `extern "rt"` is the user-facing surface for JIT-runtime functions that
    /// the Hew compiler validates at check time. Only symbols in the `stable`
    /// classification are legal `extern "rt"` targets; `internal` symbols are
    /// scheduler/lifecycle-only and must not be named in user code.
    ///
    /// Envelope code: `E_EXTERN_RT_SYMBOL_UNCLASSIFIED`.
    ExternRtSymbolUnclassified {
        /// The symbol name from the `extern "rt"` declaration.
        symbol_name: String,
        /// Actionable hint for the user.
        hint: String,
    },
    /// A `gen { }` generator block appeared inside an actor receive handler.
    ///
    /// The scheduler holds the actor-state lock for the entire handler
    /// invocation; a mid-handler yield cannot release it, so generator blocks
    /// are statically forbidden inside actor receive handlers.
    ///
    /// Envelope code: `E_GENBLOCK_IN_ACTOR_RECEIVE`.
    GenBlockInActorReceive,
    /// A `gen { }` generator block appeared inside a machine transition body.
    ///
    /// Machine transition bodies are pure state transformations; generator
    /// suspension would make the transition non-atomic and is statically
    /// forbidden.
    ///
    /// Envelope code: `E_GENBLOCK_IN_MACHINE_TRANSITION`.
    GenBlockInMachineTransition,
    /// An `await` expression appeared inside a machine transition body.
    ///
    /// Machine transition bodies are pure state transformations; awaiting a
    /// task would suspend inside the transition and is statically forbidden.
    ///
    /// Envelope code: `E_AWAIT_IN_MACHINE_TRANSITION`.
    AwaitInMachineTransition,
    /// A `gen { }` generator block whose body contains no `yield` expression.
    ///
    /// The checker infers the yield type from `yield` expressions inside the
    /// body via unification.  When no `yield` is present the yield type-variable
    /// remains unbound, so the generator's element type cannot be determined.
    ///
    /// Envelope code: `E_EMPTY_GENERATOR`.
    EmptyGenerator,
    /// A closure referenced its own let-binding name inside its body.
    ///
    /// By-value capture cannot capture a value before construction without
    /// cycles. Recursive closures require a fixed-point surface that v0.5
    /// intentionally does not expose; use a named function instead.
    ///
    /// Envelope code: `E_CLOSURE_RECURSIVE`.
    ClosureRecursive {
        /// The name of the binding the closure attempted to capture recursively.
        name: String,
    },
    /// `Sink<T>` or `Stream<T>` payload type does not implement both `Encode`
    /// and `Decode` marker traits, which are required for wire transport.
    ///
    /// Closures, raw pointers, `dyn Trait` objects, and other non-serialisable
    /// types cannot cross actor message boundaries via a channel.
    ///
    /// Envelope code: `E_SINK_PAYLOAD_NOT_WIRE`.
    SinkPayloadNotWire {
        /// User-facing representation of the rejected payload type.
        payload_ty: String,
        /// Marker trait names that the payload type is missing (`"Encode"`,
        /// `"Decode"`, or both).
        missing_traits: Vec<String>,
    },
    /// A constructor or struct-variant match arm contains a payload subpattern
    /// that is not a plain binding (`x`) or wildcard (`_`).
    ///
    /// The v0.5 payload subpattern surface is binding/wildcard-only.  Literal
    /// tests (`Shape::Line(1)`), nested constructors (`Shape::Line(Other::Foo)`),
    /// tuple/struct destructures inside a payload, and or-patterns inside a
    /// payload are not yet lowered.  Accepting them silently would produce an
    /// incorrect wildcard match; the checker rejects them with this diagnostic
    /// until the substrate lane adds full nested-predicate support.
    ///
    /// Envelope code: `E_UNSUPPORTED_PAYLOAD_SUBPATTERN`.
    UnsupportedPayloadSubpattern {
        /// Variant constructor whose payload subpattern was rejected.
        variant_name: String,
        /// Short human-readable label for the rejected subpattern kind,
        /// e.g. `"literal"`, `"nested constructor"`, `"tuple"`, `"struct"`,
        /// or `"or-pattern"`.
        kind_label: String,
    },
    /// A `re"..."` regex literal (in expression or pattern position) contains
    /// an invalid regex pattern rejected by the `regex` crate.
    ///
    /// The checker validates every regex literal at compile time using the
    /// same engine the runtime uses, so syntactically invalid patterns are
    /// a hard error before any code is emitted.
    ///
    /// Envelope code: `E_INVALID_REGEX_LITERAL`.
    InvalidRegexLiteral {
        /// The offending pattern string (as it appears in source, after
        /// delimiter normalisation).
        pattern: String,
        /// The error message from `regex::Regex::new`.
        error: String,
    },
    /// A regex pattern in a match arm is used against a non-`string`
    /// scrutinee.  Regex matching is defined only over `string` values.
    ///
    /// Envelope code: `E_REGEX_PATTERN_NOT_STRING`.
    RegexPatternNotString {
        /// User-facing representation of the actual scrutinee type.
        actual_ty: String,
    },
    /// A trait bound on a generic type parameter uses positional type
    /// arguments (e.g. `T: Eq<U>`) which is not a recognised bound form.
    /// The Hew type-checker cannot validate or enforce such bounds; admitting
    /// them would silently drop the type arguments and reduce `Eq<U>` to bare
    /// `Eq`, which is misleading.  Use associated-type bindings instead
    /// (e.g. `Iterator<Item = i64>`) for traits with output types.
    ///
    /// Envelope code: `E_UNKNOWN_TRAIT_BOUND_SHAPE`.
    UnknownTraitBoundShape {
        /// The trait name that carried the unknown positional type arguments.
        trait_name: String,
    },
    /// A generic actor was spawned without a turbofish type-argument list.
    ///
    /// Generic actors declare `<T>` type parameters on their declaration.
    /// Every `spawn` call for such an actor must supply explicit type arguments
    /// (`spawn Foo<i64>(...)`) so the type checker can substitute them into the
    /// PID type and enforce bounds. Inference of actor type-args is not yet
    /// supported and may be added in a follow-on improvement.
    ///
    /// Envelope code: `E_MISSING_ACTOR_TYPE_ARGS`.
    MissingActorTypeArgs {
        /// Name of the generic actor whose type arguments were omitted.
        actor_name: String,
        /// Number of type parameters the actor declaration declares.
        expected_arity: usize,
    },
    /// A bare actor reference (e.g. `spawn Account(...)`) matches actors
    /// exported by two or more modules, with no local actor of that name to
    /// win the local-first resolution.
    ///
    /// Resolution is never silent first-wins: the user must qualify the
    /// reference (`spawn bank.Account(...)`) to pick a module. Mirrors the
    /// `per-module-type-identity` bare-name policy for `pub type`s.
    ///
    /// Envelope code: `E_AMBIGUOUS_ACTOR_REFERENCE`.
    AmbiguousActorReference {
        /// The bare actor name as written at the reference site.
        actor_name: String,
        /// Short names of every module exporting an actor of that name,
        /// sorted for deterministic diagnostics.
        candidate_modules: Vec<String>,
    },
    /// A generic actor was spawned with the wrong number of type arguments.
    ///
    /// Envelope code: `E_ACTOR_TYPE_ARG_ARITY_MISMATCH`.
    ActorTypeArgArityMismatch {
        /// Name of the generic actor.
        actor_name: String,
        /// Number of type parameters declared on the actor.
        expected: usize,
        /// Number of type arguments supplied at the spawn site.
        got: usize,
    },
    /// A `#[extern_symbol("…")]` attribute's template payload failed the
    /// Stage-2 grammar check defined in
    /// `crate::extern_symbol::ExternSymbolTemplate::parse`.
    ///
    /// The grammar is deliberately narrow (literal runs of
    /// `[A-Za-z0-9_]` interleaved with `{T}` placeholders); see the
    /// `extern_symbol` module for the full spec. `reason` is the
    /// short, deterministic string from
    /// `TemplateError::reason()` — Stage-5 diagnostic-precision tests
    /// pin against these exact spellings.
    ///
    /// Envelope code (planned, wired in Stage 3): `E_W3_001_INVALID_EXTERN_SYMBOL_TEMPLATE`.
    InvalidExternSymbolTemplate {
        /// Short deterministic reason; safe to pin in tests.
        reason: String,
    },
    /// An impl-block method's declared signature does not match the trait
    /// method's signature after substituting `Self` with the impl target type
    /// and the trait's type parameters with the impl-supplied type arguments.
    ///
    /// Emitted at the impl method's declaration span so the diagnostic fires
    /// locally where the user wrote the wrong shape, instead of cascading
    /// downstream as a confusing "type does not satisfy trait" at a call site.
    /// See LESSONS row `diagnostic-trust` (P1).
    ///
    /// Envelope code: `E_TRAIT_IMPL_SIGNATURE_MISMATCH`.
    TraitImplSignatureMismatch {
        /// Trait whose method declaration was violated.
        trait_name: String,
        /// Method name whose impl-side signature diverges from the trait.
        method_name: String,
        /// Short label for the kind of divergence: `"arity"`, `"parameter"`,
        /// or `"return type"`. Used by tests to pin diagnostic precision.
        detail: &'static str,
    },
    /// An `impl <Trait> for <Type>` omits one or more REQUIRED (bodyless) trait
    /// methods. The required method set is resolved through the trait's
    /// owner-qualified identity, so a same-name trait collision cannot leak a
    /// neighbour's method set into the comparison. Fail-closed: an impl missing
    /// a required method is rejected at the impl site rather than accepted and
    /// surfacing later as a confusing call-site failure.
    ///
    /// Envelope code: `E_TRAIT_IMPL_MISSING_METHODS`.
    TraitImplMissingMethods {
        /// Trait whose required method(s) the impl failed to provide.
        trait_name: String,
        /// The impl target type.
        type_name: String,
        /// Sorted names of the required methods that are missing.
        methods: Vec<String>,
    },
    /// An `impl <Trait> for <Type>` declares one or more methods that the trait
    /// does not declare. The known method set is resolved through the trait's
    /// owner-qualified identity. Fail-closed: an extraneous method is a
    /// contract error rejected at the impl site.
    ///
    /// Envelope code: `E_TRAIT_IMPL_EXTRA_METHODS`.
    TraitImplExtraMethods {
        /// Trait the impl is for.
        trait_name: String,
        /// The impl target type.
        type_name: String,
        /// Sorted names of the impl methods that are not on the trait.
        methods: Vec<String>,
    },
    /// A refutable pattern was used in a `let` binding.
    ///
    /// `let` bindings have no failure arm, so only irrefutable patterns
    /// (plain identifiers, wildcards, product-type records/structs, and
    /// tuples) are legal. Enum variants, literal patterns, and or-patterns
    /// are refutable and must be used in `if let` or `match` instead.
    ///
    /// Envelope code: `E_REFUTABLE_LET_PATTERN`.
    RefutableLetPattern {
        /// Human-readable label for the rejected pattern kind,
        /// e.g. `"enum variant"`, `"literal"`, or `"or-pattern"`.
        kind_label: String,
    },
    /// A function carrying `#[intrinsic("…")]` was declared outside the
    /// designated stdlib-floor modules.
    ///
    /// A605 (ratified): the `#[intrinsic]` surface is compiler-internal-only.
    /// Memory/lifecycle intrinsics (`alloc`/`size_of`/typed-ptr/auto-`Drop`)
    /// and the existing math intrinsics must never be user-declarable; doing so
    /// would leak unsafe/aliasing primitives onto the surface, violating the
    /// durable "Hew feels immutable at the surface" tenet. The gate is
    /// fail-closed: any module not on the floor allowlist — including the
    /// root/user module — is rejected, so a user program (or any non-floor
    /// module) cannot wire itself to a compiler intrinsic.
    ///
    /// Envelope code: `E_INTRINSIC_OUTSIDE_FLOOR`.
    IntrinsicOutsideFloor {
        /// The intrinsic catalog key from the `#[intrinsic("…")]` attribute.
        intrinsic_key: String,
        /// The module path where the declaration appeared (`"(root)"` for the
        /// user's root module). Used by tests to pin diagnostic precision.
        module: String,
    },
    /// A `#[intrinsic("…")]` attribute was placed on an impl-block method
    /// rather than a top-level free function.
    ///
    /// A605 (ratified): `#[intrinsic]` is valid **only** on top-level free
    /// functions inside a stdlib-floor module. Impl methods, trait-impl
    /// methods, and actor methods are never intrinsics — the compiler wires
    /// intrinsics to standalone catalog entries, not to method dispatch
    /// slots. Fail-closed: a method-shaped declaration is rejected even if
    /// it appears inside an allowlisted floor module, and is never inserted
    /// into `intrinsic_declarations`.
    ///
    /// Envelope code: `E_INTRINSIC_ON_METHOD`.
    IntrinsicOnMethod {
        /// The intrinsic catalog key from the `#[intrinsic("…")]` attribute.
        intrinsic_key: String,
        /// The fully-qualified method key (`"Type::method"`) that carried
        /// the attribute. Used by tests to pin diagnostic precision.
        method_key: String,
    },
}

impl TypeErrorKind {
    /// Return the stable string key for this error kind.
    ///
    /// These keys are part of the LSP/WASM diagnostic protocol and must not
    /// change without a corresponding update to editor integrations.
    #[must_use]
    pub fn as_kind_str(&self) -> &'static str {
        match self {
            Self::Mismatch { .. } => "Mismatch",
            Self::UndefinedVariable => "UndefinedVariable",
            Self::UndefinedType => "UndefinedType",
            Self::UndefinedFunction => "UndefinedFunction",
            Self::UndefinedField => "UndefinedField",
            Self::UndefinedMethod => "UndefinedMethod",
            Self::AmbiguousTraitMethod => "AmbiguousTraitMethod",
            Self::AmbiguousType => "AmbiguousType",
            Self::InvalidSend => "InvalidSend",
            Self::InvalidOperation => "InvalidOperation",
            Self::ContextReaderOutsideHandler => "ContextReaderOutsideHandler",
            Self::ArityMismatch => "ArityMismatch",
            Self::BoundsNotSatisfied => "BoundsNotSatisfied",
            Self::InferenceFailed => "InferenceFailed",
            Self::NonExhaustiveMatch => "NonExhaustiveMatch",
            Self::DuplicateDefinition => "DuplicateDefinition",
            Self::MutabilityError => "MutabilityError",
            Self::ReturnTypeMismatch => "ReturnTypeMismatch",
            Self::UseAfterMove => "UseAfterMove",
            Self::YieldOutsideGenerator => "YieldOutsideGenerator",
            Self::ActorRefCycle => "ActorRefCycle",
            Self::RecursiveValueType { .. } => "RecursiveValueType",
            Self::UnusedVariable => "UnusedVariable",
            Self::UnusedMut => "UnusedMut",
            Self::StyleSuggestion => "StyleSuggestion",
            Self::UnusedImport => "UnusedImport",
            Self::RedundantIs => "RedundantIs",
            Self::UnreachableCode => "UnreachableCode",
            Self::Shadowing => "Shadowing",
            Self::DeadCode => "DeadCode",
            Self::OrphanImpl => "OrphanImpl",
            Self::PlatformLimitation => "PlatformLimitation",
            Self::OnUpgradeNotYetWired => "OnUpgradeNotYetWired",
            Self::MachineExhaustivenessError => "MachineExhaustivenessError",
            Self::UnresolvedImport => "UnresolvedImport",
            Self::BlockingCallInReceiveFn => "BlockingCallInReceiveFn",
            Self::BorrowedParamReturn => "BorrowedParamReturn",
            Self::OrPatternBindingMismatch => "OrPatternBindingMismatch",
            Self::UnsafeCollectionElement => "UnsafeCollectionElement",
            Self::TaskNotNameable => "TaskNotNameable",
            Self::UnsafeOperationRequiresBlock { .. } => "UnsafeOperationRequiresBlock",
            Self::RawPointerOpNotLowered { .. } => "RawPointerOpNotLowered",
            Self::TraitNotObjectSafe { .. } => "TraitNotObjectSafe",
            Self::MissingAssocTypeBinding { .. } => "MissingAssocTypeBinding",
            Self::ClosureExplicitMoveRequired { .. } => "ClosureExplicitMoveRequired",
            Self::ClosureCaptureModeUnresolved { .. } => "ClosureCaptureModeUnresolved",
            Self::NonSyncMutCaptureCrossesSuspend { .. } => "NonSyncMutCaptureCrossesSuspend",
            Self::ClosureEscapeKindUnresolved => "ClosureEscapeKindUnresolved",
            Self::ClosureEscapeAdvisory { .. } => "ClosureEscapeAdvisory",
            Self::RecursiveClosureUnsupported { .. } => "RecursiveClosureUnsupported",
            Self::ClosureCapturesDuplexHandle { .. } => "ClosureCapturesDuplexHandle",
            Self::AssocTypeProjectionFailed { .. } => "AssocTypeProjectionFailed",
            Self::ActorProtocolCollision { .. } => "ActorProtocolCollision",
            Self::ExternRtSymbolUnclassified { .. } => "ExternRtSymbolUnclassified",
            Self::GenBlockInActorReceive => "GenBlockInActorReceive",
            Self::GenBlockInMachineTransition => "GenBlockInMachineTransition",
            Self::AwaitInMachineTransition => "AwaitInMachineTransition",
            Self::EmptyGenerator => "EmptyGenerator",
            Self::ClosureRecursive { .. } => "ClosureRecursive",
            Self::SinkPayloadNotWire { .. } => "SinkPayloadNotWire",
            Self::UnsupportedPayloadSubpattern { .. } => "UnsupportedPayloadSubpattern",
            Self::InvalidRegexLiteral { .. } => "InvalidRegexLiteral",
            Self::RegexPatternNotString { .. } => "RegexPatternNotString",
            Self::UnknownTraitBoundShape { .. } => "UnknownTraitBoundShape",
            Self::InvalidExternSymbolTemplate { .. } => "InvalidExternSymbolTemplate",
            Self::MissingActorTypeArgs { .. } => "MissingActorTypeArgs",
            Self::AmbiguousActorReference { .. } => "AmbiguousActorReference",
            Self::ActorTypeArgArityMismatch { .. } => "ActorTypeArgArityMismatch",
            Self::TraitImplSignatureMismatch { .. } => "TraitImplSignatureMismatch",
            Self::TraitImplMissingMethods { .. } => "TraitImplMissingMethods",
            Self::TraitImplExtraMethods { .. } => "TraitImplExtraMethods",
            Self::IntrinsicOutsideFloor { .. } => "IntrinsicOutsideFloor",
            Self::IntrinsicOnMethod { .. } => "IntrinsicOnMethod",
            Self::RefutableLetPattern { .. } => "RefutableLetPattern",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_with_suggestions() {
        let err =
            TypeError::inference_failed(0..5, "variable").with_suggestion("use let x: i32 = ...");
        assert!(!err.suggestions.is_empty());
    }

    #[test]
    fn test_error_with_notes() {
        let err = TypeError::duplicate_definition(10..20, "foo", 0..5);
        assert_eq!(err.notes.len(), 1);
    }

    #[test]
    fn test_levenshtein() {
        assert_eq!(levenshtein("kitten", "sitting"), 3);
        assert_eq!(levenshtein("", "abc"), 3);
        assert_eq!(levenshtein("abc", "abc"), 0);
        assert_eq!(levenshtein("abc", "ab"), 1);
    }

    #[test]
    fn test_find_similar() {
        let names = ["count", "counter", "total", "value", "result"];
        let similar = find_similar("cont", names.iter().copied());
        // "count" is distance 1 from "cont" (insert 'u'→'n' swap)
        assert!(similar.contains(&"count".to_string()), "got: {similar:?}");
    }

    #[test]
    fn test_find_similar_no_match() {
        let names = ["alpha", "beta", "gamma"];
        let similar = find_similar("zzz", names.iter().copied());
        assert!(similar.is_empty(), "got: {similar:?}");
    }

    #[test]
    fn test_levenshtein_single_edit() {
        // insertion
        assert_eq!(levenshtein("color", "colour"), 1);
        // deletion
        assert_eq!(levenshtein("colour", "color"), 1);
        // substitution
        assert_eq!(levenshtein("cat", "bat"), 1);
    }

    #[test]
    fn test_levenshtein_empty_strings() {
        assert_eq!(levenshtein("", ""), 0);
        assert_eq!(levenshtein("a", ""), 1);
        assert_eq!(levenshtein("", "a"), 1);
    }

    #[test]
    fn test_find_similar_returns_sorted_by_distance() {
        // Target "println" (len 7) → threshold = max(1, 7/3) = 2
        // "printl" dist 1, "printlns" dist 1, "eprintln" dist 1, "print" dist 2
        let names = ["print", "printl", "printlns", "eprintln", "random"];
        let similar = find_similar("println", names.iter().copied());
        // 4 candidates match (dist ≤ 2), but truncate(3) keeps only 3.
        // Sorted by distance, stable within same distance (iterator order):
        //   dist-1: "printl", "printlns", "eprintln"  →  all 3 slots filled
        assert_eq!(
            similar,
            vec!["printl", "printlns", "eprintln"],
            "should return closest matches sorted by distance"
        );
    }

    #[test]
    fn test_find_similar_excludes_exact_match() {
        // find_similar filters out the exact match — we only want *similar* names
        let names = ["foo", "bar", "baz"];
        let similar = find_similar("foo", names.iter().copied());
        assert!(
            !similar.contains(&"foo".to_string()),
            "exact match should be excluded"
        );
    }

    #[test]
    fn test_find_similar_max_three_results() {
        let names = ["ab", "ac", "ad", "ae", "af"];
        let similar = find_similar("aa", names.iter().copied());
        assert!(
            similar.len() <= 3,
            "should cap at 3 results, got {}",
            similar.len()
        );
    }

    // ── Constructor Display output tests ─────────────────────────────

    #[test]
    fn test_mismatch_display() {
        let err = TypeError::mismatch(0..10, &Ty::I32, &Ty::Bool);
        assert_eq!(err.to_string(), "expected `i32`, found `bool`");
        assert_eq!(
            err.kind,
            TypeErrorKind::Mismatch {
                expected: "i32".to_string(),
                actual: "bool".to_string(),
            }
        );
        assert_eq!(err.severity, Severity::Error);
        assert_eq!(err.span, (0..10));
    }

    #[test]
    fn test_mismatch_display_uses_explicit_width() {
        let err = TypeError::mismatch(0..10, &Ty::I64, &Ty::option(Ty::I64));
        assert_eq!(err.to_string(), "expected `i64`, found `Option<i64>`");
        assert_eq!(
            err.kind,
            TypeErrorKind::Mismatch {
                expected: "i64".to_string(),
                actual: "Option<i64>".to_string(),
            }
        );
    }

    #[test]
    fn test_undefined_variable_display() {
        let err = TypeError::new(
            TypeErrorKind::UndefinedVariable,
            5..15,
            "cannot find value `colour` in this scope",
        );
        assert_eq!(err.to_string(), "cannot find value `colour` in this scope");
        assert_eq!(err.kind, TypeErrorKind::UndefinedVariable);
        assert_eq!(err.span, (5..15));
    }

    #[test]
    fn test_undefined_type_display() {
        let err = TypeError::new(
            TypeErrorKind::UndefinedType,
            0..6,
            "cannot find type `Colour` in this scope",
        );
        assert_eq!(err.to_string(), "cannot find type `Colour` in this scope");
        assert_eq!(err.kind, TypeErrorKind::UndefinedType);
    }

    #[test]
    fn test_undefined_function_display() {
        let err = TypeError::undefined_function(3..8, "greet");
        assert_eq!(
            err.to_string(),
            "cannot find function `greet` in this scope"
        );
        assert_eq!(err.kind, TypeErrorKind::UndefinedFunction);
    }

    #[test]
    fn test_undefined_field_display() {
        let err = TypeError::undefined_field(
            10..20,
            &Ty::Named {
                builtin: None,
                name: "Point".into(),
                args: vec![],
            },
            "colour",
        );
        assert_eq!(err.to_string(), "no field `colour` on type `Point`");
        assert_eq!(err.kind, TypeErrorKind::UndefinedField);
    }

    #[test]
    fn test_undefined_method_display() {
        let err = TypeError::new(
            TypeErrorKind::UndefinedMethod,
            10..20,
            "no method named `sort_by` found for type `Vec<i32>`",
        );
        assert_eq!(
            err.to_string(),
            "no method named `sort_by` found for type `Vec<i32>`"
        );
        assert_eq!(err.kind, TypeErrorKind::UndefinedMethod);
    }

    #[test]
    fn test_invalid_send_display() {
        let err = TypeError::new(
            TypeErrorKind::InvalidSend,
            0..5,
            "`Rc<i32>` cannot be sent to another actor",
        )
        .with_note(
            0..5,
            "the type must implement `Send` to cross actor boundaries".to_string(),
        );
        assert_eq!(err.to_string(), "`Rc<i32>` cannot be sent to another actor");
        assert_eq!(err.kind, TypeErrorKind::InvalidSend);
        assert_eq!(err.notes.len(), 1);
        assert_eq!(
            err.notes[0].1,
            "the type must implement `Send` to cross actor boundaries"
        );
    }

    #[test]
    fn test_invalid_operation_display() {
        let err = TypeError::invalid_operation(0..3, "+", &Ty::Bool);
        assert_eq!(err.to_string(), "cannot apply `+` to type `bool`");
        assert_eq!(err.kind, TypeErrorKind::InvalidOperation);
    }

    #[test]
    fn test_arity_mismatch_display() {
        let err = TypeError::new(
            TypeErrorKind::ArityMismatch,
            0..10,
            "this function takes 2 argument(s) but 3 were supplied",
        );
        assert_eq!(
            err.to_string(),
            "this function takes 2 argument(s) but 3 were supplied"
        );
        assert_eq!(err.kind, TypeErrorKind::ArityMismatch);
    }

    #[test]
    fn test_bounds_not_satisfied_display() {
        let err = TypeError::new(
            TypeErrorKind::BoundsNotSatisfied,
            0..5,
            "`i32` does not satisfy the bound `Display`",
        );
        assert_eq!(
            err.to_string(),
            "`i32` does not satisfy the bound `Display`"
        );
        assert_eq!(err.kind, TypeErrorKind::BoundsNotSatisfied);
    }

    #[test]
    fn test_inference_failed_display_with_context() {
        let err = TypeError::inference_failed(0..5, "variable `x`");
        assert_eq!(
            err.to_string(),
            "cannot infer type for variable `x`\n  help: consider adding a type annotation"
        );
        assert_eq!(err.kind, TypeErrorKind::InferenceFailed);
        // Built-in suggestion from the constructor
        assert!(err
            .suggestions
            .contains(&"consider adding a type annotation".to_string()));
    }

    #[test]
    fn test_inference_failed_display_empty_context() {
        let err = TypeError::inference_failed(0..5, "");
        assert_eq!(
            err.to_string(),
            "cannot infer type\n  help: consider adding a type annotation"
        );
    }

    #[test]
    fn test_non_exhaustive_match_display() {
        let err = TypeError::non_exhaustive_match(0..20, &["Red".to_string(), "Blue".to_string()]);
        assert_eq!(err.to_string(), "non-exhaustive match: missing Red, Blue");
        assert_eq!(err.kind, TypeErrorKind::NonExhaustiveMatch);
    }

    #[test]
    fn test_non_exhaustive_match_empty_patterns() {
        let err = TypeError::non_exhaustive_match(0..10, &[]);
        assert_eq!(
            err.to_string(),
            "non-exhaustive match: missing some patterns"
        );
    }

    #[test]
    fn test_non_exhaustive_match_warning_detail() {
        let err = TypeError::non_exhaustive_match_detail(
            0..10,
            Severity::Warning,
            "consider adding a wildcard `_` arm",
        );
        assert_eq!(
            err.to_string(),
            "non-exhaustive match: consider adding a wildcard `_` arm"
        );
        assert_eq!(err.severity, Severity::Warning);
        assert_eq!(err.kind, TypeErrorKind::NonExhaustiveMatch);
    }

    #[test]
    fn test_duplicate_definition_display() {
        let err = TypeError::duplicate_definition(10..20, "foo", 0..5);
        assert_eq!(err.to_string(), "`foo` is defined multiple times");
        assert_eq!(err.kind, TypeErrorKind::DuplicateDefinition);
        assert_eq!(err.notes.len(), 1);
        assert_eq!(err.notes[0].0, (0..5));
        assert_eq!(err.notes[0].1, "previous definition here");
    }

    #[test]
    fn test_mutability_error_display() {
        let err = TypeError::mutability_error(0..10, "count");
        assert_eq!(
            err.to_string(),
            "cannot assign to immutable variable `count`\n  help: consider changing this to `var count`"
        );
        assert_eq!(err.kind, TypeErrorKind::MutabilityError);
        assert!(err
            .suggestions
            .contains(&"consider changing this to `var count`".to_string()));
    }

    #[test]
    fn test_return_type_mismatch_display() {
        let err = TypeError::return_type_mismatch(0..10, &Ty::String, &Ty::I32);
        assert_eq!(
            err.to_string(),
            "return type mismatch: expected `string`, found `i32`"
        );
        assert_eq!(err.kind, TypeErrorKind::ReturnTypeMismatch);
    }

    #[test]
    fn test_return_type_mismatch_display_uses_explicit_width() {
        let err = TypeError::return_type_mismatch(0..10, &Ty::I64, &Ty::option(Ty::I64));
        assert_eq!(
            err.to_string(),
            "return type mismatch: expected `i64`, found `Option<i64>`"
        );
    }

    #[test]
    fn test_use_after_move_display() {
        let err = TypeError::use_after_move(20..25, "data", &(5..10));
        assert_eq!(err.to_string(), "use of moved value `data`");
        assert_eq!(err.kind, TypeErrorKind::UseAfterMove);
        assert_eq!(err.notes.len(), 1);
        assert_eq!(err.notes[0].0, (5..10));
        assert_eq!(err.notes[0].1, "value was moved here");
    }

    #[test]
    fn test_actor_ref_cycle_display() {
        let err = TypeError::actor_ref_cycle(0..30, "A -> B -> A");
        assert_eq!(
            err.to_string(),
            "actor reference cycle detected: A -> B -> A\n  help: consider using weak references or restructuring the supervision tree"
        );
        assert_eq!(err.severity, Severity::Warning);
        assert_eq!(err.kind, TypeErrorKind::ActorRefCycle);
    }

    // ── Builder method tests ─────────────────────────────────────────

    #[test]
    fn test_with_note_chaining() {
        let err = TypeError::new(TypeErrorKind::UndefinedVariable, 0..5, "test error")
            .with_note(10..15, "first note")
            .with_note(20..25, "second note");
        assert_eq!(err.notes.len(), 2);
        assert_eq!(err.notes[0].1, "first note");
        assert_eq!(err.notes[1].1, "second note");
    }

    #[test]
    fn test_with_suggestion_chaining() {
        let err = TypeError::new(TypeErrorKind::UndefinedVariable, 0..5, "test")
            .with_suggestion("try A")
            .with_suggestion("try B");
        assert_eq!(err.suggestions.len(), 2);
        assert_eq!(err.suggestions[0], "try A");
        assert_eq!(err.suggestions[1], "try B");
    }

    // ── Display formatting with multiple suggestions ─────────────────

    #[test]
    fn test_display_multiple_suggestions() {
        let err = TypeError::new(TypeErrorKind::UndefinedVariable, 0..5, "not found")
            .with_suggestion("did you mean `x`?")
            .with_suggestion("did you mean `y`?");
        let output = err.to_string();
        assert_eq!(
            output,
            "not found\n  help: did you mean `x`?\n  help: did you mean `y`?"
        );
    }

    #[test]
    fn test_display_no_suggestions() {
        let err = TypeError::new(TypeErrorKind::UndefinedVariable, 0..5, "not found");
        assert_eq!(err.to_string(), "not found");
    }

    // ── Severity tests ───────────────────────────────────────────────

    #[test]
    fn test_default_severity_is_error() {
        let err = TypeError::new(
            TypeErrorKind::Mismatch {
                expected: "i32".into(),
                actual: "bool".into(),
            },
            0..5,
            "mismatch",
        );
        assert_eq!(err.severity, Severity::Error);
    }

    // ── std::error::Error impl ───────────────────────────────────────

    #[test]
    fn test_error_trait_impl() {
        let err = TypeError::mismatch(0..5, &Ty::I32, &Ty::Bool);
        // Verify it can be used as a dyn Error
        let dyn_err: &dyn std::error::Error = &err;
        assert!(dyn_err.to_string().contains("expected `i32`"));
    }

    // ── TypeErrorKind equality ───────────────────────────────────────

    #[test]
    fn test_type_error_kind_equality() {
        assert_eq!(
            TypeErrorKind::UndefinedVariable,
            TypeErrorKind::UndefinedVariable
        );
        assert_ne!(
            TypeErrorKind::UndefinedVariable,
            TypeErrorKind::UndefinedField
        );
        assert_eq!(
            TypeErrorKind::Mismatch {
                expected: "i32".into(),
                actual: "bool".into()
            },
            TypeErrorKind::Mismatch {
                expected: "i32".into(),
                actual: "bool".into()
            },
        );
        assert_ne!(
            TypeErrorKind::Mismatch {
                expected: "i32".into(),
                actual: "bool".into()
            },
            TypeErrorKind::Mismatch {
                expected: "i64".into(),
                actual: "bool".into()
            },
        );
    }

    // ── TypeError Clone ──────────────────────────────────────────────

    // ── Multi-error collection ───────────────────────────────────────

    #[test]
    fn test_multi_error_collection() {
        let errors: Vec<TypeError> = vec![
            TypeError::mismatch(0..5, &Ty::I32, &Ty::Bool),
            TypeError::new(
                TypeErrorKind::UndefinedVariable,
                10..15,
                "cannot find value `x` in this scope",
            ),
            TypeError::new(
                TypeErrorKind::ArityMismatch,
                20..30,
                "this function takes 2 argument(s) but 0 were supplied",
            ),
        ];
        assert_eq!(errors.len(), 3);
        assert_eq!(
            errors[0].kind,
            TypeErrorKind::Mismatch {
                expected: "i32".into(),
                actual: "bool".into(),
            }
        );
        assert_eq!(errors[1].kind, TypeErrorKind::UndefinedVariable);
        assert_eq!(errors[2].kind, TypeErrorKind::ArityMismatch);

        // Verify each formats independently
        assert!(errors[0].to_string().contains("expected `i32`"));
        assert!(errors[1].to_string().contains("cannot find value `x`"));
        assert!(errors[2].to_string().contains("takes 2 argument(s) but 0"));
    }

    // ── Constructor with generic types ───────────────────────────────

    #[test]
    fn test_mismatch_with_complex_types() {
        let expected = Ty::Function {
            params: vec![Ty::I32, Ty::Bool],
            ret: Box::new(Ty::String),
        };
        let actual = Ty::Tuple(vec![Ty::I32, Ty::Bool]);
        let err = TypeError::mismatch(0..20, &expected, &actual);
        assert!(err
            .to_string()
            .contains("expected `fn(i32, bool) -> string`"));
        assert!(err.to_string().contains("found `(i32, bool)`"));
    }

    #[test]
    fn test_undefined_method_on_primitive() {
        let err = TypeError::new(
            TypeErrorKind::UndefinedMethod,
            0..5,
            "no method named `frobnicate` found for type `i32`",
        );
        assert_eq!(
            err.to_string(),
            "no method named `frobnicate` found for type `i32`"
        );
    }

    #[test]
    fn test_undefined_field_on_generic_type() {
        let ty = Ty::Named {
            builtin: None,
            name: "HashMap".into(),
            args: vec![Ty::String, Ty::I32],
        };
        let err = TypeError::undefined_field(0..10, &ty, "colour");
        assert_eq!(
            err.to_string(),
            "no field `colour` on type `HashMap<string, i32>`"
        );
    }
}
