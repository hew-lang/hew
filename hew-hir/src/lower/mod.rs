//! AST-to-HIR lowering: `LowerCtx` and the program entry points.

#![allow(
    clippy::wildcard_imports,
    reason = "the LowerCtx impl spans these submodules, which share one namespace"
)]

use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use hew_parser::ast::{
    condition_exprs, ActorDecl, ArrayElement, AttributeArg, BinaryOp, Block, CallArg,
    CompoundAssignOp, ConditionItem, ConstDecl, Expr, FnDecl, Item, LambdaParam, Literal,
    MachineDecl, Param, Pattern, Program, ReceiveFnDecl, RecordDecl, RecordKind, RestartPolicy,
    SelectArm, ShutdownDirective, Span, Spanned, Stmt, StringPart, SupervisorDecl,
    SupervisorStrategy, TimeoutClause, TraitItem, TraitMethod, TypeBodyItem, TypeDecl,
    TypeDeclKind, TypeExpr, UnaryOp,
};
use hew_types::builtin_enums::BuiltinMonomorphicEnumVariant;
use hew_types::BuiltinType;
use hew_types::{
    ActorMethodKind, ActorStateGuard, AssignTargetKind, AssignTargetShape, CallTarget, ChildSlot,
    ClosureCaptureFact, ClosureEscapeFact, ExecutionContextReader, LoweringFact,
    MethodCallReceiverKind, MethodCallRewrite, PatternKind, RcIntrinsicOp, ResolvedTraitBound,
    ResolvedTy, SpanKey, Ty, TypeCheckOutput, UserComparisonDispatch, WireCodecDirection,
};

use crate::builtin_type_classes::seed_builtin_type_classes;
use crate::diagnostic::{HirDiagnostic, HirDiagnosticKind};
use crate::ids::{BindingId, IdGen, ItemId, ResolvedRef, ScopeId, SiteId};
use crate::monomorph::{
    contains_recursive_polymorphic_self, substitute_type_params, EnumLayoutRegistry, EnumMonoKey,
    EnumVariantLayout, MonoKey, MonoRegistry, RecordLayout, RecordLayoutRegistry, RecordMonoKey,
    MONOMORPHISATION_REGISTRY_CAP,
};
use crate::node::{
    ExternProvenance, HirActorDecl, HirActorInit, HirActorMethod, HirActorReceiveFn,
    HirActorStateGuard, HirBinding, HirBlock, HirCaptureKind, HirClosureCapture,
    HirDestructureField, HirDestructureSelector, HirExpr, HirExprKind, HirField, HirFn,
    HirGenCapture, HirGenCaptureSource, HirItem, HirLambdaCapture, HirLifecycleHook,
    HirLifecycleHookKind, HirLiteral, HirMatchArm, HirMatchArmBinding, HirMatchArmPredicate,
    HirModule, HirPayloadPredicate, HirPayloadVariantPredicate, HirRecordDecl, HirRegexLiteral,
    HirRestartPolicy, HirSelect, HirSelectArm, HirSelectArmKind, HirShutdownDirective, HirStmt,
    HirStmtKind, HirSupervisorChild, HirSupervisorDecl, HirSupervisorStrategy, HirTypeDecl,
    HirTypeDeclKind, HirVarSelfMethodTarget, HirVariant, HirVariantKind,
};
use crate::stdlib_catalog::{self, BuiltinEntry, BuiltinLinkage};
use crate::{IntentKind, ResourceMarker};

mod actors;
mod builders;
mod call_shape_gates;
mod calls;
mod captures;
mod closures;
mod collections;
mod consts;
mod ctx;
mod dispatch;
mod expr;
mod fork;
mod gates;
mod identifiers;
mod impl_plan;
mod imports;
mod items;
mod iteration;
mod literals;
mod methods;
mod monomorphisation;
mod patterns;
mod private_refs;
mod program;
mod race;
mod recovery;
mod registry;
mod resource_lifecycles;
mod runtime_calls;
mod scope;
mod select;
mod spawn;
mod stmt;
mod substitution;
mod types;
mod var_self;
mod vec_index_gates;
mod wire;

use self::call_shape_gates::*;
use self::calls::*;
use self::captures::*;
use self::gates::*;
use self::impl_plan::*;
use self::imports::*;
use self::iteration::*;
use self::patterns::*;
use self::private_refs::*;
use self::resource_lifecycles::*;
use self::substitution::*;
use self::vec_index_gates::*;

#[cfg(any(test, feature = "internal-test-hooks"))]
pub use self::call_shape_gates::run_call_shape_gates_for_test;
pub use self::program::lower_program_with_mono_cap;
pub use self::substitution::substitute_ty;

/// Target architecture for compilation. Subset of the full `TargetSpec`
/// from `hew-cli/src/target.rs`, exposed at the HIR boundary so target gates
/// can reject unsupported constructs before codegen. Kept minimal to avoid
/// introducing a `hew-hir → hew-cli` dependency.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TargetArch {
    Aarch64,
    X86_64,
    Wasm32,
    /// Any other target (e.g. riscv64, powerpc64). Used for target gates
    /// that reject coroutine-dependent constructs on non-x86_64/aarch64.
    Other,
}

/// Resolve the type of an `if`/`else` expression from its two branch types,
/// mirroring the checker's `unify_branches` Never-handling.
///
/// A branch that diverges (`return`/`break`/`continue`) lowers to a block of
/// type `ResolvedTy::Never`; it must NOT determine the construct's value type.
/// When one branch is `Never`, the construct's type is the OTHER branch's type
/// (the value branch). This makes `let x = if c { v } else { return … }` carry
/// `v`'s type instead of `Never`, so a later `x + 1` lowers as integer
/// arithmetic rather than failing closed on a non-integer operand at MIR.
///
/// With no else branch the construct yields `Unit`; with both branches present
/// and neither `Never`, the then branch's type wins (the checker already
/// reconciled the two, so they agree here).
fn if_branch_result_ty(then_ty: &ResolvedTy, else_ty: Option<&ResolvedTy>) -> ResolvedTy {
    match else_ty {
        None => ResolvedTy::Unit,
        Some(else_ty) => {
            if matches!(else_ty, ResolvedTy::Never) {
                then_ty.clone()
            } else {
                else_ty.clone()
            }
        }
    }
}

/// True when a tail-less block's statements guarantee control never falls off
/// the end — i.e. the block diverges and has type `ResolvedTy::Never`.
///
/// A block diverges when its last reachable statement diverges: a bare
/// `return`, or a value/expression statement whose lowered type is `Never`
/// (a `return <expr>` expression, or an `if`/`match` whose every branch
/// diverges). This is the HIR mirror of the checker's `check_block`
/// divergence tracking, kept narrow: only the LAST statement is inspected,
/// matching the well-formed case where any earlier diverging statement makes
/// the rest unreachable (the checker already warned on that).
/// The source extent a block's lexical scope covers: from its first statement
/// to the end of its tail expression.
fn block_extent(block: &Block) -> std::ops::Range<usize> {
    let start = block
        .stmts
        .first()
        .map(|(_, span)| span.start)
        .or_else(|| block.trailing_expr.as_ref().map(|expr| expr.1.start));
    let end = block
        .trailing_expr
        .as_ref()
        .map(|expr| expr.1.end)
        .or_else(|| block.stmts.last().map(|(_, span)| span.end));
    match (start, end) {
        (Some(start), Some(end)) if end > start => start..end,
        _ => 0..0,
    }
}

fn block_diverges(statements: &[HirStmt]) -> bool {
    let Some(last) = statements.last() else {
        return false;
    };
    match &last.kind {
        HirStmtKind::Return(_) => true,
        HirStmtKind::Expr(expr) | HirStmtKind::Let(_, Some(expr)) => {
            matches!(expr.ty, ResolvedTy::Never)
        }
        _ => false,
    }
}

fn literal_to_hir(lit: &Literal) -> (HirLiteral, ResolvedTy) {
    match lit {
        Literal::Integer { value, .. } => (HirLiteral::Integer(*value), ResolvedTy::I64),
        Literal::Float(value) => (HirLiteral::Float(*value), ResolvedTy::F64),
        Literal::String(value) => (HirLiteral::String(value.clone()), ResolvedTy::String),
        Literal::Bool(value) => (HirLiteral::Bool(*value), ResolvedTy::Bool),
        Literal::Char(value) => (HirLiteral::Char(*value), ResolvedTy::Char),
        Literal::Duration(value) => (HirLiteral::Duration(*value), ResolvedTy::Duration),
    }
}

#[derive(Debug, Clone)]
enum ForIterNextCall {
    BuiltinVecIter,
    /// A concrete iterator's own `next(var self)` impl method.
    VarSelf,
    /// `for x in stream` over `Stream<T>` — each iteration borrows the
    /// stream binding and emits the layout-witness runtime recv call
    /// (`hew_stream_next_layout`), reusing MIR's existing
    /// `Terminator::SuspendingStreamNext` flip.
    StreamRecv,
    /// `for x in <generator>` — each iteration consumes one value via the
    /// generator `.next()` consumption seam (`HirExprKind::GeneratorNext`).
    /// The generator handle is the loop's `__hew_for_iter_*` binding; it is
    /// borrowed by each `.next()` and freed by its scope-exit drop on loop exit.
    Generator,
}

fn literal_match_supported(lit: &HirLiteral, ty: &ResolvedTy) -> bool {
    match (lit, ty) {
        (HirLiteral::Integer(_), ty) => ty.is_integer_literal_match_scrutinee(),
        (HirLiteral::Float(_), ResolvedTy::F32 | ResolvedTy::F64)
        | (HirLiteral::Bool(_), ResolvedTy::Bool)
        | (HirLiteral::Char(_), ResolvedTy::Char)
        | (HirLiteral::String(_), ResolvedTy::String) => true,
        _ => false,
    }
}

impl TargetArch {
    /// Returns the target architecture of the host running the compiler.
    /// Used for tests that don't need to test cross-compilation behavior.
    #[must_use]
    pub fn host() -> Self {
        #[cfg(target_arch = "x86_64")]
        return TargetArch::X86_64;
        #[cfg(target_arch = "aarch64")]
        return TargetArch::Aarch64;
        #[cfg(target_arch = "wasm32")]
        return TargetArch::Wasm32;
        #[cfg(not(any(
            target_arch = "x86_64",
            target_arch = "aarch64",
            target_arch = "wasm32"
        )))]
        return TargetArch::Other;
    }
}

type ScopeBinding = (BindingId, ResolvedTy, std::ops::Range<usize>);
type ScopeMap = HashMap<String, ScopeBinding>;
type OuterClosureBinding = (String, ResolvedTy, std::ops::Range<usize>);
type ClosureCaptureCandidate = (BindingId, String, std::ops::Range<usize>);
const SYNTHETIC_OPTION_ITEM: ItemId = ItemId(u32::MAX - 1);
const SYNTHETIC_RESULT_ITEM: ItemId = ItemId(u32::MAX - 2);
/// `LookupError` is declared in `std/builtins.hew`, but builtins.hew is
/// loaded out-of-band (not via `module_graph`), so the user-enum walk in
/// `lower_program` never sees it. Surface it through the same builtin-enum
/// path as `Option` / `Result` so `Err(LookupError::NotFound)` match arms
/// resolve via `machine_ctor_registry`.
const SYNTHETIC_LOOKUP_ERROR_ITEM: ItemId = ItemId(u32::MAX - 1000);
/// The status codes a send folds into `SendError`: `named` pairs one runtime
/// status with its error; every other nonzero status is `otherwise`.
#[derive(Clone, Copy)]
struct SendStatusCodes {
    named: (i128, &'static str),
    otherwise: &'static str,
}

/// A pipe reports `1` when its reader left and `2` when full.
const PIPE_SEND_STATUS: SendStatusCodes = SendStatusCodes {
    named: (1, "Closed"),
    otherwise: "Full",
};

/// A node reports `HEW_ERR_STALE_REF` for a superseded pid; every other
/// failure leaves the peer unreachable.
const REMOTE_SEND_STATUS: SendStatusCodes = SendStatusCodes {
    named: (-16, "StaleRef"),
    otherwise: "Partition",
};

/// `SendError` is also declared in `std/builtins.hew` and likewise invisible
/// to the user-enum walk. Surface it so `match e { SendError::NodeRoutingNotWired
/// => ... }` arms inside `Result<(), SendError>` matches resolve via
/// `machine_ctor_registry`.
const SYNTHETIC_SEND_ERROR_ITEM: ItemId = ItemId(u32::MAX - 1001);
const SYNTHETIC_NODE_ERROR_ITEM: ItemId = ItemId(u32::MAX - 1010);
/// `TimeoutError` is declared in `std/builtins.hew` and likewise invisible to
/// the user-enum walk. Surface it so `match e { TimeoutError::Timeout => ... }`
/// arms inside `Result<Option<T>, TimeoutError>` matches resolve via
/// `machine_ctor_registry`.
const SYNTHETIC_TIMEOUT_ERROR_ITEM: ItemId = ItemId(u32::MAX - 1005);
/// `LinkError` is the `Err` variant of `Result<(), LinkError>` returned by
/// `link()` in value position. Declared in `std/builtins.hew` and — like
/// `SendError` / `TimeoutError` — invisible to the user-enum walk in
/// `lower_program` (builtins.hew is loaded out-of-band, not via `module_graph`).
/// Surface it through the same builtin-enum path so
/// `Err(LinkError::AlreadyLinked)` / `Err(LinkError::TargetDead)` match arms
/// resolve via `machine_ctor_registry`.
const SYNTHETIC_LINK_ERROR_ITEM: ItemId = ItemId(u32::MAX - 1004);
/// Sentinel `ItemId` for the synthetic `HashMapIter<K, V>` record — the
/// `for (k, v) in m` desugar target. Like `VecIter`, it is declared in
/// `std/builtins.hew` but never emitted as a HIR `Record`/`TypeDecl` item, so
/// `layout_mono` seeds its decl from `hashmap_iter_field_shape`.
pub(crate) const SYNTHETIC_HASHMAP_ITER_ITEM: ItemId = ItemId(u32::MAX - 1006);
const SYNTHETIC_CRASH_ACTION_ITEM: ItemId = ItemId(u32::MAX - 1007);
const SYNTHETIC_CRASH_KIND_ITEM: ItemId = ItemId(u32::MAX - 1008);
const SYNTHETIC_MONITOR_ERROR_ITEM: ItemId = ItemId(u32::MAX - 1009);
const BUILTINS_HEW_SOURCE: &str = include_str!("../../../std/builtins.hew");

/// One compiler-owned cursor record admitted at the HIR layout boundary.
///
/// The typed builtin discriminator is the identity. Presentation names are
/// derived from it and never participate in lookup, so a user declaration with
/// the same leaf cannot enter the synthetic record namespace.
pub(crate) struct SyntheticCursorLayoutSpec {
    pub(crate) builtin: BuiltinType,
    pub(crate) origin: ItemId,
    pub(crate) type_params: &'static [&'static str],
}

pub(crate) const SYNTHETIC_CURSOR_LAYOUT_SPECS: &[SyntheticCursorLayoutSpec] =
    &[SyntheticCursorLayoutSpec {
        builtin: BuiltinType::HashMapIter,
        origin: SYNTHETIC_HASHMAP_ITER_ITEM,
        type_params: &["K", "V"],
    }];

/// Resolve the declaration and substituted field shape for one synthetic
/// cursor instantiation. Every origin-site and post-monomorphisation layout
/// registration consumes this catalog path.
pub(crate) fn synthetic_cursor_layout(
    builtin: BuiltinType,
    type_args: &[ResolvedTy],
) -> Option<(
    &'static SyntheticCursorLayoutSpec,
    Vec<(String, ResolvedTy)>,
)> {
    let spec = SYNTHETIC_CURSOR_LAYOUT_SPECS
        .iter()
        .find(|spec| spec.builtin == builtin)?;
    let fields = match (builtin, type_args) {
        (BuiltinType::HashMapIter, [key, value]) => hashmap_iter_field_shape(key, value),
        _ => return None,
    };
    Some((spec, fields))
}

/// The single field-order/type authority for `HashMapIter<K, V>`, shared by the
/// `std/builtins.hew` declaration, the for-in desugar's `StructInit`, and
/// `layout_mono`'s synthetic-decl seeding so the three can never disagree. The
/// `for (k, v) in m` desugar constructs the literal with exactly these fields in
/// this order: parallel key/value snapshot `Vec`s plus the cursor index.
pub(crate) fn hashmap_iter_field_shape(
    key_ty: &ResolvedTy,
    val_ty: &ResolvedTy,
) -> Vec<(String, ResolvedTy)> {
    vec![
        ("ks".to_string(), LowerCtx::resolved_vec_ty(key_ty.clone())),
        ("vs".to_string(), LowerCtx::resolved_vec_ty(val_ty.clone())),
        ("idx".to_string(), ResolvedTy::I64),
    ]
}

/// Synthetic-builtin sentinel `ItemId`s for the actor `link(target)` /
/// `monitor(target)` builtins. These have no AST `fn` item, so
/// `seed_stdlib_fn_registry` mints them in the `u32::MAX / 2` band — the same
/// band as `supervisor_stop` (`u32::MAX / 2`). The ids are registry
/// placeholders only: their
/// `FnEntry` rows carry `builtin_family`, so `lower_identifier` resolves
/// the names to `ResolvedRef::Builtin(family)` and MIR reads the C symbol
/// off the catalog bijection.
const SYNTHETIC_LINK_ITEM: ItemId = ItemId(u32::MAX / 2 - 9);
const SYNTHETIC_MONITOR_ITEM: ItemId = ItemId(u32::MAX / 2 - 10);
/// Synthetic-builtin sentinel `ItemId` for the user-facing `unlink` builtin.
/// Mirrors `SYNTHETIC_LINK_ITEM` / `SYNTHETIC_MONITOR_ITEM`; the checker
/// (`registration.rs`) registers `unlink` as a 1-arg actor-handle → `Unit`
/// builtin with no AST `fn` item. Resolves to
/// `ResolvedRef::Builtin(ActorUnlink)` via `builtin_family`.
const SYNTHETIC_UNLINK_ITEM: ItemId = ItemId(u32::MAX / 2 - 18);
/// Synthetic-builtin sentinel `ItemId` for the user-facing `link_remote` builtin.
/// The checker (`registration.rs`) registers `link_remote` as a 2-arg
/// `(RemotePid<T>, PartitionPolicy) → Result<(), LinkError>` builtin with no AST
/// `fn` item; it resolves to `ResolvedRef::Builtin(LinkRemote)` via
/// `builtin_family` and MIR reads the C symbol `hew_node_link_remote_location` off the
/// catalog bijection.
const SYNTHETIC_LINK_REMOTE_ITEM: ItemId = ItemId(u32::MAX / 2 - 20);

/// Synthetic-builtin sentinel `ItemId` for the static `instant::now()`
/// constructor. The `impl instant` block in `std/builtins.hew` binds it via
/// `#[extern_symbol(hew_instant_now)]`, but a no-receiver static call resolves
/// through the bare callee name `"instant::now"` (the parser joins the `::`
/// path into one identifier). Seeding it here with `builtin_family`
/// (`InstantNow`) makes `lower_identifier` resolve the callee to
/// `ResolvedRef::Builtin(InstantNow)`, so MIR's `runtime_symbol_for_call_expr`
/// reads `hew_instant_now` off the catalog bijection — mirroring `link` /
/// `monitor`. `instant` is i64-backed, so `return_ty` is `I64`.
const SYNTHETIC_INSTANT_NOW_ITEM: ItemId = ItemId(u32::MAX / 2 - 19);

/// Synthetic-builtin sentinel `ItemId`s for the pipe layout-witness
/// recv/send symbols (`hew_stream_next_layout`, `hew_stream_try_next_layout`,
/// `hew_stream_send_layout`, `hew_stream_try_send_layout`).
///
/// These symbols are checker-owned method rewrites and are NOT
/// extern-declared in `.hew` source (their real ABI carries an
/// out-parameter and/or an element-layout witness pointer that cannot be
/// expressed as a plain `extern "C"` declaration). Without a synthetic
/// `fn_registry` entry the HIR import filter (`collect_all_bare_call_names`)
/// would see these bare names as unresolvable and skip the `recv`/`try_recv`
/// impl methods entirely, leaving the user with a "no method recv" error.
///
/// By seeding them here:
///   1. The import filter admits `recv`/`try_recv` (the body calls are now
///      resolvable via `fn_registry`).
///   2. Each `FnEntry` carries `builtin_family`, so `lower_identifier`
///      resolves them to `ResolvedRef::Builtin(family)` rather than
///      `Unresolved`; the HIR callable-set gate then accepts them
///      unconditionally (a typed runtime-builtin reference is callable by
///      construction) instead of raising the premature `IndirectCallUnsupported`.
///   3. MIR's `runtime_symbol_for_call_expr` returns `None` for them (their
///      `family.c_symbol()` is a pre-staged symbol absent from the
///      `known_runtime_symbols` allowlist), so MIR falls through to
///      `module_fn_names` (which this seeding also populates) →
///      `lower_direct_call` → `Terminator::Call`, carrying the typed family.
///   4. Codegen intercepts the `Terminator::Call` by callee name and emits the
///      layout-witness ABI (`i32 sym(handle, out, witness)` for recv;
///      `void sym(handle, data_ptr, witness)` for send), deriving the element
///      type from the dest/value local's checker-resolved type.
const SYNTHETIC_STREAM_NEXT_LAYOUT_ITEM: ItemId = ItemId(u32::MAX / 2 - 15);
const SYNTHETIC_STREAM_TRY_NEXT_LAYOUT_ITEM: ItemId = ItemId(u32::MAX / 2 - 16);
const SYNTHETIC_STREAM_SEND_LAYOUT_ITEM: ItemId = ItemId(u32::MAX / 2 - 17);
const SYNTHETIC_STREAM_TRY_SEND_LAYOUT_ITEM: ItemId = ItemId(u32::MAX / 2 - 14);

// NB the synthetic-builtin sentinel band (`u32::MAX / 2 - N`) is
// documentation/segregation only. Item-resolved builtin names carry their
// typed family on the resolution itself, not through a numeric band predicate.
// The load-bearing invariant — every minted sentinel id is pairwise distinct
// (two colliding ids silently overwrite each other's `fn_registry` row) — is
// pinned by `synthetic_builtin_sentinel_ids_are_pairwise_distinct`.

/// Description of a built-in tagged union for the HIR pre-pass that seeds
/// the same registries user enums populate (`machine_ctor_registry`,
/// `enum_variants_by_name`, `enum_type_params`, `enum_item_ids`). Variant
/// payload type-parameter names index into `type_params`.
#[derive(Clone, Copy)]
pub(crate) struct BuiltinEnumSpec {
    /// Exact declaration identity used by every semantic registry.
    pub(crate) canonical_type_name: &'static str,
    pub(crate) item_id: ItemId,
    pub(crate) type_params: &'static [&'static str],
    variants: BuiltinEnumVariants,
}

#[derive(Clone, Copy)]
enum BuiltinEnumVariants {
    Generic(&'static [hew_types::builtin_type::BuiltinEnumVariant]),
    Monomorphic(&'static [BuiltinMonomorphicEnumVariant]),
}

struct BuiltinEnumVariantNames {
    variants: BuiltinEnumVariants,
    index: usize,
}

impl Iterator for BuiltinEnumVariantNames {
    type Item = &'static str;

    fn next(&mut self) -> Option<Self::Item> {
        let name = match self.variants {
            BuiltinEnumVariants::Generic(variants) => {
                variants.get(self.index).map(|variant| variant.name)
            }
            BuiltinEnumVariants::Monomorphic(variants) => {
                variants.get(self.index).map(|variant| variant.name)
            }
        };
        self.index += usize::from(name.is_some());
        name
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = match self.variants {
            BuiltinEnumVariants::Generic(variants) => variants.len(),
            BuiltinEnumVariants::Monomorphic(variants) => variants.len(),
        }
        .saturating_sub(self.index);
        (remaining, Some(remaining))
    }
}

impl ExactSizeIterator for BuiltinEnumVariantNames {}

impl BuiltinEnumSpec {
    fn variant_names(&self) -> BuiltinEnumVariantNames {
        BuiltinEnumVariantNames {
            variants: self.variants,
            index: 0,
        }
    }

    fn variant_payload(&self, index: usize) -> Vec<&'static str> {
        match self.variants {
            BuiltinEnumVariants::Generic(variants) => variants[index]
                .payload_type_args
                .iter()
                .map(|parameter| self.type_params[*parameter])
                .collect(),
            BuiltinEnumVariants::Monomorphic(_) => Vec::new(),
        }
    }
}

/// The one receive handler every synthesized lambda actor declares.
const LAMBDA_ACTOR_HANDLER: &str = "call";

const MONOMORPHIC_BUILTIN_ENUMS: &[hew_types::builtin_enums::BuiltinMonomorphicEnum] =
    hew_types::builtin_enums::monomorphic_builtin_enums();
const BUILTIN_ENUM_SPEC_COUNT: usize = 2 + MONOMORPHIC_BUILTIN_ENUMS.len();
const EMPTY_BUILTIN_ENUM_SPEC: BuiltinEnumSpec = BuiltinEnumSpec {
    canonical_type_name: "",
    item_id: ItemId(0),
    type_params: &[],
    variants: BuiltinEnumVariants::Generic(&[]),
};
/// HIR registration order is observable for duplicate bare variant names:
/// later specs replace earlier entries in `machine_ctor_registry`.
const MONOMORPHIC_BUILTIN_ENUM_HIR_ORDER: &[(&str, ItemId)] = &[
    ("std.builtins.LookupError", SYNTHETIC_LOOKUP_ERROR_ITEM),
    ("std.builtins.SendError", SYNTHETIC_SEND_ERROR_ITEM),
    ("std.builtins.NodeError", SYNTHETIC_NODE_ERROR_ITEM),
    ("std.builtins.TimeoutError", SYNTHETIC_TIMEOUT_ERROR_ITEM),
    ("std.builtins.LinkError", SYNTHETIC_LINK_ERROR_ITEM),
    ("std.failure.CrashAction", SYNTHETIC_CRASH_ACTION_ITEM),
    ("std.failure.CrashKind", SYNTHETIC_CRASH_KIND_ITEM),
    (
        "std.link_monitor.MonitorError",
        SYNTHETIC_MONITOR_ERROR_ITEM,
    ),
];

const fn const_str_eq(left: &str, right: &str) -> bool {
    let left = left.as_bytes();
    let right = right.as_bytes();
    if left.len() != right.len() {
        return false;
    }
    let mut index = 0;
    while index < left.len() {
        if left[index] != right[index] {
            return false;
        }
        index += 1;
    }
    true
}

const fn monomorphic_builtin_enum(
    canonical_type_name: &str,
) -> hew_types::builtin_enums::BuiltinMonomorphicEnum {
    let mut index = 0;
    while index < MONOMORPHIC_BUILTIN_ENUMS.len() {
        let candidate = MONOMORPHIC_BUILTIN_ENUMS[index];
        if const_str_eq(candidate.canonical_name, canonical_type_name) {
            return candidate;
        }
        index += 1;
    }
    panic!("HIR builtin enum is missing from the hew-types catalog");
}

const fn hir_order_entry_count(type_name: &str) -> usize {
    let mut count = 0;
    let mut index = 0;
    while index < MONOMORPHIC_BUILTIN_ENUM_HIR_ORDER.len() {
        if const_str_eq(MONOMORPHIC_BUILTIN_ENUM_HIR_ORDER[index].0, type_name) {
            count += 1;
        }
        index += 1;
    }
    count
}

const fn derive_builtin_enum_specs() -> [BuiltinEnumSpec; BUILTIN_ENUM_SPEC_COUNT] {
    assert!(
        MONOMORPHIC_BUILTIN_ENUMS.len() == MONOMORPHIC_BUILTIN_ENUM_HIR_ORDER.len(),
        "monomorphic builtin enum catalog and HIR item ids differ in length"
    );
    let mut catalog_index = 0;
    while catalog_index < MONOMORPHIC_BUILTIN_ENUMS.len() {
        assert!(
            hir_order_entry_count(MONOMORPHIC_BUILTIN_ENUMS[catalog_index].canonical_name) == 1,
            "monomorphic builtin enum must have exactly one HIR order entry"
        );
        catalog_index += 1;
    }

    let mut specs = [EMPTY_BUILTIN_ENUM_SPEC; BUILTIN_ENUM_SPEC_COUNT];
    specs[0] = BuiltinEnumSpec {
        canonical_type_name: "Option",
        item_id: SYNTHETIC_OPTION_ITEM,
        type_params: BuiltinType::Option.generic_enum().unwrap().type_params,
        variants: BuiltinEnumVariants::Generic(
            BuiltinType::Option.generic_enum().unwrap().variants,
        ),
    };
    specs[1] = BuiltinEnumSpec {
        canonical_type_name: "Result",
        item_id: SYNTHETIC_RESULT_ITEM,
        type_params: BuiltinType::Result.generic_enum().unwrap().type_params,
        variants: BuiltinEnumVariants::Generic(
            BuiltinType::Result.generic_enum().unwrap().variants,
        ),
    };

    let mut index = 0;
    while index < MONOMORPHIC_BUILTIN_ENUM_HIR_ORDER.len() {
        let (canonical_type_name, item_id) = MONOMORPHIC_BUILTIN_ENUM_HIR_ORDER[index];
        let catalog_entry = monomorphic_builtin_enum(canonical_type_name);
        specs[index + 2] = BuiltinEnumSpec {
            canonical_type_name: catalog_entry.canonical_name,
            item_id,
            type_params: &[],
            variants: BuiltinEnumVariants::Monomorphic(catalog_entry.variants),
        };
        index += 1;
    }
    specs
}

const BUILTIN_ENUM_SPEC_ARRAY: [BuiltinEnumSpec; BUILTIN_ENUM_SPEC_COUNT] =
    derive_builtin_enum_specs();
pub(crate) const BUILTIN_ENUM_SPECS: &[BuiltinEnumSpec] = &BUILTIN_ENUM_SPEC_ARRAY;

fn builtin_enum_variant_names() -> impl Iterator<Item = &'static str> {
    BUILTIN_ENUM_SPECS
        .iter()
        .flat_map(BuiltinEnumSpec::variant_names)
}

fn is_builtin_enum_variant_bare_name(name: &str) -> bool {
    builtin_enum_variant_names().any(|candidate| candidate == name)
}

pub(crate) fn builtin_enum_hir_variants(spec: &BuiltinEnumSpec) -> Vec<HirVariant> {
    spec.variant_names()
        .enumerate()
        .map(|(index, name)| {
            let payload_params = spec.variant_payload(index);
            let kind = if payload_params.is_empty() {
                HirVariantKind::Unit
            } else {
                HirVariantKind::Tuple(
                    payload_params
                        .iter()
                        .map(|param| ResolvedTy::Named {
                            head: hew_types::TypeHead::param(param),
                            args: Vec::new(),
                            is_opaque: false,
                        })
                        .collect(),
                )
            };
            HirVariant {
                name: name.to_string(),
                kind,
            }
        })
        .collect()
}

#[derive(Debug, Clone, Default)]
pub struct ResolutionCtx;

/// Output of [`lower_program`].
///
/// The `diagnostics` field is checked by all production pipelines before the
/// `module` is passed downstream.  The [`LowerOutput::into_result`] method
/// provides a fail-closed boundary: it returns `Err(diagnostics)` when any
/// `CheckerBoundaryViolation` is present, making it impossible to silently
/// continue past checker-boundary failures.
#[derive(Debug, Clone, PartialEq)]
pub struct LowerOutput {
    pub module: HirModule,
    pub diagnostics: Vec<HirDiagnostic>,
}

impl LowerOutput {
    /// Converts `self` into `Ok(module)` when no fatal diagnostics are
    /// present, or `Err(diagnostics)` when at least one fatal diagnostic
    /// exists.
    ///
    /// **Fatal diagnostic kinds** (fail-closed set):
    /// - [`HirDiagnosticKind::CheckerBoundaryViolation`] — a checker-authority
    ///   invariant was violated (e.g. a leaked inference variable).
    /// - [`HirDiagnosticKind::RecordLayoutMissing`] — a generic record init
    ///   site was accepted by the checker but its type-arg entry was absent,
    ///   meaning the downstream `Named { args: [] }` shape would be wrong.
    ///
    /// - [`HirDiagnosticKind::TargetCoroutineUnsupported`] — the program uses
    ///   actors/tasks/coroutines on a target that does not support them.
    /// - [`HirDiagnosticKind::NestedSupervisorAccessorUnsupported`] — dead
    ///   code; `sup.nested` field access on a nested supervisor now lowers
    ///   via `hew_supervisor_nested_get` and never constructs this variant.
    /// - [`HirDiagnosticKind::BinaryOperatorUnsupportedInMir`] — value-position
    ///   range operator (FC-P1-D).
    /// - [`HirDiagnosticKind::CallableUnsupportedInMir`] — a call expression
    ///   resolves to an item with no MIR body or runtime-ABI lowering.
    /// - [`HirDiagnosticKind::IndirectCallUnsupported`] — a call expression
    ///   has an unresolved callee with callable static type that the MIR
    ///   producer cannot dispatch.
    /// - [`HirDiagnosticKind::SupervisorSpawnArgsUnsupported`] — the program
    ///   calls `spawn AppSupervisor(...)` with init args.
    /// - [`HirDiagnosticKind::VecIndexElementTypeUnsupported`] — `xs[i]` on
    ///   a `Vec<T>` whose element type `T` has no `hew_vec_get_T` getter.
    /// - [`HirDiagnosticKind::VecSliceElementTypeUnsupported`] — `xs[a..b]`
    ///   on a `Vec<T>` whose element type `T` has no
    ///   `hew_vec_slice_range_T` runtime symbol.
    /// - [`HirDiagnosticKind::MonomorphisationCallTypeArgsViolation`] — a
    ///   generic-function call's `call_type_args` entry failed the
    ///   `ResolvedTy::from_ty` boundary conversion (e.g. contained `Ty::Error`
    ///   or an unresolved inference variable); passing a corrupt monomorphisation
    ///   entry downstream produces an invalid MIR.
    /// - [`HirDiagnosticKind::RecordLayoutTypeArgsViolation`] — same as above
    ///   for the record-layout side-table.
    ///
    /// Callers that want to continue despite non-fatal diagnostics should
    /// check `.diagnostics` directly.  This method is the recommended
    /// entry-point for production pipelines because it makes the fail-closed
    /// contract impossible to accidentally bypass.
    ///
    /// # Errors
    ///
    /// Returns `Err(diagnostics)` when the output contains at least one fatal
    /// diagnostic (see above).
    ///
    /// TODO: if more fail-closed diagnostic kinds are added, consider a
    /// `severity()` method on `HirDiagnosticKind` returning `Fatal | NonFatal`
    /// so this match does not need updating each time (approach (b)).
    pub fn into_result(self) -> Result<HirModule, Vec<HirDiagnostic>> {
        let has_fatal = self.diagnostics.iter().any(|d| {
            matches!(
                d.kind,
                crate::HirDiagnosticKind::CheckerBoundaryViolation { .. }
                    | crate::HirDiagnosticKind::RecordLayoutMissing { .. }
                    | crate::HirDiagnosticKind::ImportedBodyMissingPrivateHelper { .. }
                    | crate::HirDiagnosticKind::ImportedFreeFnBodyUnresolvedBareCall { .. }
                    | crate::HirDiagnosticKind::TargetCoroutineUnsupported { .. }
                    | crate::HirDiagnosticKind::TaskSpawnSignatureUnsupported { .. }
                    | crate::HirDiagnosticKind::TaskSpawnCalleeUnsupported { .. }
                    | crate::HirDiagnosticKind::DeadlineBodyUnsupported { .. }
                    | crate::HirDiagnosticKind::NestedSupervisorAccessorUnsupported { .. }
                    | crate::HirDiagnosticKind::BinaryOperatorUnsupportedInMir { .. }
                    | crate::HirDiagnosticKind::CallableUnsupportedInMir { .. }
                    | crate::HirDiagnosticKind::IndirectCallUnsupported { .. }
                    | crate::HirDiagnosticKind::SupervisorSpawnArgsUnsupported { .. }
                    | crate::HirDiagnosticKind::VecIndexElementTypeUnsupported { .. }
                    | crate::HirDiagnosticKind::VecSliceElementTypeUnsupported { .. }
                    | crate::HirDiagnosticKind::CloneNotYetSupported { .. }
                    | crate::HirDiagnosticKind::MonomorphisationCallTypeArgsViolation { .. }
                    | crate::HirDiagnosticKind::RecordLayoutTypeArgsViolation { .. }
            )
        });
        if has_fatal {
            Err(self.diagnostics)
        } else {
            Ok(self.module)
        }
    }
}

/// Pre-collected signature of a top-level function item.
#[derive(Debug, Clone)]
struct FnEntry {
    id: ItemId,
    return_ty: ResolvedTy,
    param_tys: Vec<ResolvedTy>,
    linkage: Option<BuiltinLinkage>,
    /// Source-declared generic type parameter names, in order. Empty for
    /// non-generic functions. Consulted at `Expr::Call` lowering sites to
    /// decide whether the callee is a generic top-level user fn that
    /// requires a monomorphisation-registry entry.
    type_params: Vec<String>,
    /// `Some(family)` for checker-registered runtime builtins with no
    /// AST `fn` item (`supervisor_stop`, `link`, `monitor`, `unlink`).
    /// `lower_identifier` resolves these to
    /// [`ResolvedRef::Builtin`] carrying the typed family instead of a
    /// synthetic-sentinel `Item` id, so MIR never reverse-maps the
    /// user-visible name through a string bridge. `None` for every real
    /// function item.
    builtin_family: Option<hew_types::runtime_call::RuntimeCallFamily>,
}

/// Pre-collected shape of a top-level `const NAME: T = ...;` declaration.
///
/// Populated in the first pass (`register_const_entry`) so that const
/// references resolve to a stable `ItemId` regardless of source order. The
/// folded value is NOT stored here — it is produced once, canonically, in the
/// emit pass (`lower_const`) on the `HirItem::Const`. A const reference only
/// needs the id (for `ResolvedRef::Const`) and the declared type.
#[derive(Debug, Clone)]
struct ConstEntry {
    id: ItemId,
    ty: ResolvedTy,
}
///
/// Populated in the type-decl / record pre-pass and consulted at
/// `Expr::StructInit` lowering to (a) decide whether a record-init site
/// needs a per-instantiation `RecordLayout` and (b) substitute the
/// type-params with the concrete args from `record_init_type_args` to
/// produce the layout's field shape.
///
/// Tuple-form records are admitted with `fields = vec![]` — their
/// constructor is reached via `Expr::Call`, not `StructInit`, so they
/// never hit the record-layout registry. The entry is kept for shape
/// uniformity with the named-form path.
#[derive(Debug)]
struct RecordEntry {
    id: ItemId,
    /// Source-declared generic type-parameter names, in order. Empty for
    /// monomorphic record/type declarations.
    type_params: Vec<String>,
    /// Field shape as written in source — types still mention the
    /// declaration's type params verbatim (no substitution yet). The
    /// record-layout registry walks these and substitutes per
    /// instantiation.
    fields: Vec<(String, ResolvedTy)>,
}

/// Per-impl-block context threaded into `lower_impl_block` for imported
/// modules. Carries the set of method names to skip because their bodies or signatures cannot
/// be resolved safely across the module boundary. `symbol_self_name` is the
/// exact declaration-keyed owner selected by the pre-lowering body plan,
/// including any concrete type-argument suffix.
struct ImportedImplLowering<'a> {
    skip_methods: &'a HashSet<String>,
    symbol_self_name: Option<&'a str>,
}

/// Pre-lowering proof that a checker-selected impl declaration will emit one
/// exact HIR body symbol.  This bridges source-order root bodies (which may
/// call a file/package imported impl before the imported module is emitted)
/// without treating a first-pass signature row as executable evidence.
#[derive(Debug, Default)]
struct ImplBodyPlan {
    symbols: HashMap<hew_types::DefId, String>,
    /// Declarations whose executable bodies are supplied by the compiler,
    /// rather than selected from source/import traversal.
    compiler_selected: HashSet<hew_types::DefId>,
    symbol_self_names: HashMap<*const hew_parser::ast::ImplDecl, String>,
}

#[must_use]
pub fn lower_program(
    program: &Program,
    type_check_output: &TypeCheckOutput,
    ctx: &ResolutionCtx,
    target_arch: TargetArch,
) -> LowerOutput {
    lower_program_with_mono_cap(
        program,
        type_check_output,
        ctx,
        MONOMORPHISATION_REGISTRY_CAP,
        target_arch,
    )
}

/// Convenience helper that defaults to the host's target architecture.
/// Used primarily in tests that don't care about cross-compilation.
#[must_use]
pub fn lower_program_host_target(
    program: &Program,
    type_check_output: &TypeCheckOutput,
    ctx: &ResolutionCtx,
) -> LowerOutput {
    lower_program(program, type_check_output, ctx, TargetArch::host())
}

/// Linker-safe internal symbol for an authored `main` that is not the selected
/// process entry. A single `$` delimiter cannot be written in Hew source and is
/// distinct from the `$$` generic-instantiation separator.
fn authored_main_callable_symbol(
    defs: &hew_types::DefTable,
    declaration: hew_types::DefId,
) -> String {
    format!(
        "__hew_callable${}",
        crate::symbol::declaration_symbol(defs, declaration)
    )
}

/// Construct the sole legacy surface key for a tagged-union constructor.
///
/// `machine_ctor_registry` is keyed by exact source-owner paths for imported
/// declarations. This spelling is only the compatibility alias for a unique
/// source form such as `Toggle::On`; it never selects an owner by itself. The
/// checker-provided type at the use site and the pre-pass uniqueness count
/// remain the authority for admitting that alias.
fn tagged_union_surface_ctor_key(owner: &str, variant: &str) -> String {
    format!("{owner}::{variant}")
}

/// The user-facing companion type for a machine's events.
fn machine_event_surface_type(machine: &str) -> String {
    format!("{machine}Event")
}

type TraitMethodBindingKey = (Option<String>, u32, String, String);

#[derive(Debug)]
struct LowerCtx {
    ids: IdGen,
    scopes: Vec<ScopeMap>,
    /// Maps function name → pre-allocated `ItemId` + return type + param types.
    fn_registry: HashMap<String, FnEntry>,
    /// Item-keyed linker-symbol substitutions for source callables whose
    /// surface spelling is reserved by a generated process adapter.
    fn_symbol_overrides: HashMap<ItemId, String>,
    /// Source-declared `extern` symbols. A resource argument crosses one of
    /// these bodyless ABI boundaries by borrow only when the generated
    /// per-symbol/per-parameter ownership contract says so; ordinary Hew
    /// functions that merely share a spelling never inherit that privilege.
    extern_fn_names: HashSet<String>,
    /// Same-module actor identity rewrites active while lowering imported
    /// module bodies. Keys are source-visible bare actor names; values are the
    /// fully-qualified actor-layout identities used by MIR.
    imported_actor_rewrites: Option<HashMap<String, String>>,
    /// Bare-name → `ConstEntry` map active while lowering an imported module's
    /// function bodies.  A module may reference its own module-level consts
    /// by bare name (e.g. `STATUS_OK` inside `tls.hew`), but only the
    /// qualified key `"tls.STATUS_OK"` is in the global `const_registry`.
    /// This scoped map bridges the gap: it is populated before lowering each
    /// module's bodies and cleared after.
    imported_module_consts: Option<HashMap<String, ConstEntry>>,
    /// Per-named-type marker + close-method registry. Pre-populated from
    /// every `Item::TypeDecl` before function bodies lower so that
    /// Resource lifecycle lookup consumes the marker and close method.
    /// Also seeded with the substrate types (Sink, Stream, etc.) via
    /// `builtin_type_classes::seed_builtin_type_classes` before the `TypeDecl` loop.
    type_classes: crate::value_class::TypeClassTable,
    /// Checker-derived closeable-opaque candidates awaiting resolved HIR
    /// close-body admission.
    opaque_resource_candidates: hew_types::OpaqueResourceCandidateGraph,
    /// Resource declarations for which `check_resource_close_discipline`
    /// already pushed a user-facing close-discipline diagnostic
    /// (`ResourceMissingClose`, `ResourceCloseMustReturnUnit` or
    /// `ResourceCloseSourceUnsupported`). `admit_resource_record_lifecycles`
    /// consults this set so it never re-derives the same missing-or-invalid
    /// close fact as a `CheckerBoundaryViolation`: that diagnostic means an
    /// HIR-internal invariant broke on a program this pass already believes
    /// is sound, and a declaration already reported here is known unsound,
    /// not an invariant violation.
    resource_close_discipline_failures: HashSet<hew_types::DefId>,
    diagnostics: Vec<HirDiagnostic>,
    /// Checker-owned function and method signatures used for iterator dispatch
    /// and concrete call instantiation.
    /// TRANSITION(B1): signatures by the keys the checker published.
    fn_sigs_by_path: HashMap<String, hew_types::FnSig>,
    /// Checker-selected targets for ordinary calls, keyed by the call span.
    /// Missing facts lower as an explicit unsupported target; HIR never
    /// guesses from the callee spelling.
    direct_call_targets: HashMap<SpanKey, CallTarget>,
    /// Checker-published canonical `(trait, method)` declaration identities,
    /// keyed by owner-qualified source method paths. HIR only carries these
    /// declaration facts; it does not rebuild IDs from method spellings.
    trait_method_ids: HashMap<String, (hew_types::DefId, hew_types::DefId)>,
    /// Checker-published canonical trait identities addressed through an exact
    /// importer binding.  This resolves default-method ownership for `impl
    /// AliasTrait for Type` without a leaf-name or suffix lookup.
    trait_method_ids_by_binding:
        HashMap<TraitMethodBindingKey, (hew_types::DefId, hew_types::DefId)>,
    /// Checker-owned impl-method declaration identities.  Keys are linker
    /// presentation strings retained only to locate the already-allocated ID;
    /// HIR never constructs an ID from a method spelling.
    impl_method_declaration_ids: HashMap<String, hew_types::DefId>,
    consuming_inherent_methods: HashSet<hew_types::DefId>,
    /// Exact declaration-ID → emitted-body-symbol projection, populated only
    /// after HIR emits an impl body.  This is deliberately separate from
    /// `impl_method_declaration_ids`: the checker table retains compatibility
    /// aliases (including methods HIR deliberately skips), while direct
    /// dispatch needs the one symbol HIR actually emitted for the selected
    /// declaration.
    impl_method_body_symbols: HashMap<hew_types::DefId, String>,
    /// Exact pre-lowering proof for impl bodies scheduled to emit.  Unlike
    /// `fn_registry`, this excludes imported methods rejected by the shared
    /// body/signature eligibility analysis.
    impl_body_plan: ImplBodyPlan,
    /// Checker-owned method-call lowering decisions. Keyed by the method-call
    /// expression span. `Expr::MethodCall` lowering looks up each call site
    /// here and rewrites to `HirExprKind::Call` with the runtime symbol.
    /// A missing entry is a fail-closed diagnostic (`MethodCallNoRewrite`).
    method_call_rewrites: HashMap<SpanKey, MethodCallRewrite>,
    /// Checker-proven affine close calls. Their receiver release obligation is
    /// discharged, but their closed handle storage remains readable.
    method_call_discharges_receiver: HashSet<SpanKey>,
    /// Receiver-identity calls whose result is discarded in statement
    /// position. These call sites borrow the original owner into the exact
    /// receiver/result alias instead of moving it away.
    method_call_preserves_receiver_identity: HashSet<SpanKey>,
    /// Checker-owned width-conversion method lowering decisions keyed by the
    /// method-call expression span. HIR checks this before `method_call_rewrites`
    /// to emit `NumericCast` (wrapping) or `SaturatingWidthCast` (saturating)
    /// from a zero-arg method call. Integer opt-out arithmetic
    /// (`.wrapping_*`/`.checked_*`/`.saturating_*`) is an ordinary
    /// `method_call_rewrites` entry (`RuntimeCallFamily::IntArith`), not a
    /// separate side table.
    width_cast_lowerings: HashMap<SpanKey, hew_types::WidthCastLowering>,
    /// Checker-owned exact numeric conversion decisions keyed by method-call
    /// expression span. HIR emits `TryWidthCast` directly from this table.
    try_width_cast_lowerings: HashMap<SpanKey, hew_types::TryWidthCastLowering>,
    /// Checker-owned actor receive dispatch decisions keyed by method-call span.
    /// HIR consumes these to choose `ActorSend` / `ActorAsk` without reclassifying
    /// receiver types.
    actor_method_dispatch: HashMap<SpanKey, ActorMethodKind>,
    actor_delivery_calls: HashMap<SpanKey, hew_types::actor_delivery::ActorDeliveryCall>,
    /// Checker-owned machine method dispatch decisions keyed by method-call span.
    /// HIR checks this before `method_call_rewrites` to produce `MachineStep` /
    /// `MachineStateName` nodes rather than falling through to `MethodCallNoRewrite`.
    machine_method_dispatch: HashMap<SpanKey, hew_types::MachineMethodKind>,
    /// Checker-owned function-tail Ok-coercion sites keyed by the tail
    /// expression's span. Each entry marks a `Result<Ok, Err>`-returning
    /// function tail whose value is the `Ok` payload; `lower_expr` wraps the
    /// lowered expression at this span in a synthetic `Ok(..)` variant
    /// constructor so it returns the declared `Result`. See
    /// `TypeCheckOutput::tail_ok_coercions`.
    tail_ok_coercions: std::collections::HashSet<SpanKey>,
    result_return_coercions: HashMap<SpanKey, hew_types::ResultReturnKind>,
    recovery_kinds: HashMap<SpanKey, hew_types::check::RecoveryKind>,
    /// Checker-bound parameter slot of each source argument, for calls whose
    /// named arguments bind out of source order.
    call_argument_slots: HashMap<SpanKey, Vec<usize>>,
    checked_call_effects: HashMap<SpanKey, hew_types::check::effects::SuspensionEffect>,
    select_sources: HashMap<SpanKey, Vec<hew_types::check::CheckedSelectSource>>,
    checked_fork_transfers: HashMap<SpanKey, hew_types::check::effects::ForkTransferFact>,
    fork_call_inputs: Option<fork::ForkCallInputs>,
    /// Checker-owned method-call receiver classifications. These facts prevent
    /// HIR from reclassifying a lexical spelling as a module and fail closed
    /// when a classified module or actor call lacks its dispatch fact.
    method_call_receiver_kinds: HashMap<SpanKey, MethodCallReceiverKind>,
    /// Per-call-site `T → dyn Trait` coercion side-table. Keyed by the
    /// argument expression span. `lower_expr` consults this at every
    /// expression's exit; if the just-lowered expression's span has an
    /// entry, the result is wrapped in `HirExprKind::CoerceToDynTrait`.
    /// Carries the checker's authoritative method-table resolution.
    dyn_trait_coercions: HashMap<SpanKey, hew_types::DynCoercion>,
    /// Per-call-site `dyn Trait` method-dispatch side-table. Keyed by the
    /// method-call expression span. `lower_method_call` consults this
    /// before the `method_call_rewrites` branch so that
    /// `obj.method()` on a `dyn`-typed receiver lowers to
    /// `HirExprKind::CallDynMethod` (vtable slot index attached) rather
    /// than failing closed on the missing rewrite entry.
    dyn_trait_method_calls: HashMap<SpanKey, hew_types::DynMethodCall>,
    /// Checker-resolved `(ImplId, MethodTarget)` verdict per method-call
    /// site, keyed by the method-call expression span. Populated by the
    /// checker's `populate_collection_dispatch` for builtin-generic
    /// (HashMap/HashSet today; Vec/Option/Result on the migration roadmap)
    /// method calls.
    ///
    /// `lower_method_call` consults this **after** the existing checker
    /// side-tables (numeric / actor / machine / dyn-trait) and **before**
    /// the legacy `method_call_rewrites` branch. The lookup runs
    /// unconditionally so any boundary-type conversion failure surfaces
    /// as a real diagnostic; when the verdict survives boundary conversion,
    /// `lower_method_call` emits `HirExprKind::ResolvedImplCall` carrying
    /// `MethodTarget.symbol_name` verbatim. MIR consumes that symbol via
    /// `Terminator::Call`. This is the production path for builtin
    /// HashMap/HashSet dispatch today; Vec/Option/Result migrate later.
    resolved_calls: HashMap<SpanKey, hew_types::ResolvedCall>,
    /// Checker-inferred types for every expression, keyed by expression span.
    /// Consulted at `Expr::Call` sites to determine the call-result type from
    /// checker authority rather than re-deriving from the callee's HIR type.
    /// This is the canonical source of truth for builtin callee result types
    /// (e.g. `link`) that have no AST `fn` entry and therefore no
    /// `fn_registry` hit.
    expr_types: HashMap<SpanKey, Ty>,
    /// Checker-authoritative closed ownership classification for each concrete
    /// type instance. HIR projects this fact but does not derive a second
    /// ownership answer from type shape.
    type_facts: std::collections::BTreeMap<hew_types::TypeInstanceKey, hew_types::TypeFacts>,
    /// Checked declaration metadata supplies representation facts that the
    /// source annotation and `Ty::Named` expression spelling cannot carry.
    type_declarations: std::collections::BTreeMap<String, hew_types::value_class::DeclaredType>,
    interpolation_display_types: HashMap<SpanKey, Ty>,
    /// `==`/`!=`/`<`/`<=`/`>`/`>=` binary expressions dispatching to a user
    /// trait impl instead of the structural default (D340). Consulted at
    /// `Expr::Binary` lowering; see [`UserComparisonDispatch`].
    user_comparison_dispatch: HashMap<SpanKey, UserComparisonDispatch>,
    numeric_operand_coercions: HashMap<SpanKey, Ty>,
    /// Declared C-boundary signatures for the `#[extern_symbol]` methods every
    /// checked module dispatched, accumulated across passes. Each becomes one
    /// `HirItem::ExternFn`, so an extern method and an `extern` block reach
    /// later stages through the same declaration shape.
    extern_method_signatures: HashMap<(hew_types::DefId, String), hew_types::ExternMethodSignature>,
    /// W4.047 P1.2 — the **typed** checker→HIR handoff map (the shadow of
    /// `expr_types`). Carries `ResolvedTy` (never `Ty::Var`/`Ty::Error`/literal)
    /// for every concrete accepted span; cloned verbatim from
    /// `TypeCheckOutput::resolved_expr_types`.
    ///
    /// In Phase 1 this is a *shadow*: lowering still derives every node type
    /// from `expr_types`, and the typed map is only consulted by the
    /// `assert_resolved_ty_totality` net to prove — across the whole real
    /// corpus — that it agrees with the live `expr_types`→`from_ty` path at
    /// every fail-open / boundary-violation site. Zero behaviour change in
    /// Phase 1; Phase 2 promotes this to the primary read path.
    resolved_expr_types: HashMap<SpanKey, ResolvedTy>,
    /// Checker-authoritative RHS spans for accepted `lhs is TypeName`
    /// patterns. When present, the RHS identifier is a type pattern, not a
    /// value expression to lower through lexical bindings.
    is_type_patterns: HashMap<SpanKey, Ty>,
    /// Checker-authoritative general-closure capture facts keyed by closure
    /// literal span. HIR consumes this ledger fail-closed when materialising
    /// `HirExprKind::Closure`; it does not infer capture legality from syntax.
    closure_capture_facts: HashMap<SpanKey, Vec<ClosureCaptureFact>>,
    /// Checker-authoritative escape classification keyed by closure literal
    /// span (the checker's `closure_escape_facts`). HIR threads this fact into
    /// `HirExprKind::Closure::escape_kind` so MIR's
    /// `ClosureEnvLayout::allocation_strategy` can dispatch between
    /// `Local`/`Forked`/`Escapes` storage without re-deriving classification.
    closure_escape_facts: HashMap<SpanKey, ClosureEscapeFact>,
    /// Stack of checker-inferred generator Yield parameters while lowering
    /// nested `gen {}` bodies. `Expr::Yield` consumes the innermost entry.
    generator_yield_tys: Vec<ResolvedTy>,
    /// Depth counter for nested `scope{}` bodies. When > 0, statement-expression
    /// calls are inferred as child-task spawns (TI-1); outside any scope body
    /// all calls are synchronous (TI-3). Using a depth counter rather than a
    /// bool supports nested `scope{}` blocks correctly.
    scope_depth: u32,
    /// The `ScopeId` of the innermost lexical block currently being lowered.
    /// Updated by `lower_block` immediately after `self.ids.scope()` allocates
    /// the block's identity. Read by `Stmt::Defer` lowering to tag the deferred
    /// body with the owning scope so MIR can materialise it at scope exits.
    current_scope_id: ScopeId,
    /// Return type of the innermost function-like body currently being lowered.
    /// Used by `Expr::PostfixTry` to synthesize `return Err(e)` / `return None`
    /// with the enclosing body's return type rather than the scrutinee type.
    current_return_type: Option<ResolvedTy>,
    /// `Some((let_id, let_name))` while lowering the body of an actor-lambda
    /// that is the value of `let <let_name> = actor |..| { .. }`. The
    /// capture-strength classifier inside the body walk compares each
    /// resolved capture's `BindingId` to `let_id` to discriminate the
    /// self-reference (Weak, §5.9 ratification 2) from every other captured
    /// binding (Strong). Nested actor-lambdas restore the prior value via
    /// `mem::replace` so the outer self-binding doesn't leak into an inner
    /// lambda's classification.
    current_actor_self: Option<(BindingId, String)>,
    /// Checker-resolved `self.field` projections that name the enclosing
    /// actor's own state. Keyed by the projection's span; the field name is in
    /// the AST at that span. The checker decides this while the actor's fields
    /// and the enclosing scope are both in hand, so lowering consumes the
    /// answer rather than re-deriving it from a mirror of the field names.
    actor_self_state_fields: HashSet<SpanKey>,
    /// Type-annotation spans of state fields that `init` initializes (D447).
    actor_deferred_field_decls: HashSet<SpanKey>,
    /// Assignment target spans that are a deferred field's first store (D447).
    actor_init_first_stores: HashSet<SpanKey>,
    /// Iterable spans of `for` loops the checker admitted in borrow mode (D432).
    borrowed_element_for_loops: HashSet<SpanKey>,
    /// `xs[i]` spans the checker admitted as a borrowed element read (D432).
    borrowed_element_index_reads: HashSet<SpanKey>,
    /// `get` sites whose element has no semantic clone: `Some` carries a loan
    /// of the slot the collection still owns.
    borrowed_element_option_reads: HashSet<SpanKey>,
    /// `VecIter` cursor sites whose element has no semantic clone: `next()`
    /// moves each element out instead of copying it.
    owning_take_vec_cursors: HashSet<SpanKey>,
    /// Checker-resolved type arguments for generic function calls that
    /// lack explicit type annotations. Keyed by the call expression span.
    ///
    /// Consumed at `Expr::Call` lowering (G-1.a, producer-bridge wakeup):
    /// every callsite whose callee is a generic top-level user fn
    /// (non-empty `FnEntry.type_params`) is paired with the entry here
    /// and inserted into `mono_registry`. A poisoned entry (failing the
    /// `ResolvedTy::from_ty` boundary conversion) emits
    /// `MonomorphisationCallTypeArgsViolation`; a generic callsite with
    /// no entry at all is treated as the trivially-monomorphic case
    /// (e.g. a builtin or runtime-symbol call) and skipped.
    /// (LESSONS: checker-authority P0, end-to-end-before-layer-thickening P1)
    call_type_args: HashMap<SpanKey, Vec<Ty>>,
    /// Checker-authoritative ABI-selector facts for erased runtime types.
    /// Currently covers `HashSet` element-type dispatch (`i64`/`u64`/`str`
    /// → `Int64` or `String` ABI variant).
    ///
    /// Passive pass-through: `HashSet` ABI selection lives in MIR/codegen, not
    /// in HIR lowering.  Future consumer: E4 codegen and slice 4.7 spine
    /// widening when `HashSet` operations enter the Rust pipeline.
    /// (LESSONS: checker-authority P0, end-to-end-before-layer-thickening P1)
    #[expect(
        dead_code,
        reason = "passive pass-through; future consumer is HashSet ABI selection in E4 codegen"
    )]
    lowering_facts: HashMap<SpanKey, LoweringFact>,
    /// Checker-resolved assignment target classification keyed by the target
    /// expression span.
    ///
    /// Selects indexed-write lowering before consuming the resolved mutation.
    assign_target_kinds: HashMap<SpanKey, AssignTargetKind>,
    /// Checker-resolved assignment target type-shape metadata (signedness flag)
    /// keyed by the target expression span.  Populated alongside
    /// `assign_target_kinds` for every accepted assignment.
    ///
    /// Passive pass-through: same consumer timeline as `assign_target_kinds`.
    /// (LESSONS: checker-authority P0, end-to-end-before-layer-thickening P1)
    #[expect(
        dead_code,
        reason = "passive pass-through; future consumer is compound-assignment signedness in codegen"
    )]
    assign_target_shapes: HashMap<SpanKey, AssignTargetShape>,
    checked_indexed_place_operations:
        HashMap<SpanKey, (hew_types::RuntimeCallFamily, hew_types::RuntimeCallFamily)>,
    indexed_place_operations:
        HashMap<SiteId, (hew_types::RuntimeCallFamily, hew_types::RuntimeCallFamily)>,
    /// Checker-owned actor receive-handler guard policy keyed by receive span.
    actor_handler_state_guards: HashMap<SpanKey, ActorStateGuard>,
    /// Actor type names that participate in reference cycles, computed by the
    /// checker's cycle-detection pass. Consumed by `lower_actor` to populate
    /// `HirActorDecl.cycle_capable`. Future runtime consumer: actor codegen
    /// (refcount-cycle-breaking strategy selection).
    /// (LESSONS: end-to-end-before-layer-thickening P1)
    cycle_capable_actors: HashSet<String>,
    /// Per-actor protocol descriptors lifted from the checker
    /// (`TypeCheckOutput.actor_protocol_descriptors`). Consumed by
    /// `lower_actor` to populate `HirActorDecl.protocol_descriptor`. An
    /// actor missing from this map either declares no receive handlers or
    /// the checker emitted an `ActorProtocolCollision` for it; either way
    /// the lowered HIR carries `protocol_descriptor: None` and downstream
    /// MIR fails closed when it tries to derive a `msg_id`.
    actor_protocol_descriptors: HashMap<String, hew_types::ActorProtocolDescriptor>,
    /// Resolver-minted identities for each `actor |msg| { .. }` expression,
    /// keyed by its span. See `TypeCheckOutput::lambda_actor_declarations`.
    lambda_actor_declarations: HashMap<SpanKey, hew_types::actor_protocol::LambdaActorIdentity>,
    /// Actor declarations synthesized from lambda actors during body
    /// lowering, appended to the module's items once lowering finishes.
    pending_lambda_actors: Vec<HirActorDecl>,
    /// Names of every `TypeDefKind::Actor` declaration in the program, lifted
    /// from `TypeCheckOutput.type_defs`. Consumed by `lower_actor` to recognise
    /// an actor-state field whose annotated type is a bare actor name (e.g.
    /// `let out: W;` where `W` is an actor): such a field holds an actor
    /// handle, never the actor's state by value, so its lowered type is
    /// canonicalised to `W`'s own actor-handle type. That canonical form is
    /// the one the MIR state-clone classifier and codegen already lower
    /// (bit-copyable pid), matching `spawn W`'s handle result. Without it
    /// the bare `W` field reaches MIR as an unresolvable nested user record
    /// and fails closed.
    actor_type_names: HashSet<String>,
    /// Distinct concrete instantiations of generic top-level user fns,
    /// accumulated as `Expr::Call` lowering walks the program. Drained
    /// into `HirModule.monomorphisations` at the end of `lower_program`.
    /// Cap is configurable via `lower_program_with_mono_cap` for
    /// fail-closed-diagnostic tests.
    mono_registry: MonoRegistry,
    /// Tracks whether the `MonomorphisationCapExceeded` diagnostic has
    /// already been emitted for this lowering invocation, to avoid
    /// spamming one diagnostic per overflowing callsite.
    mono_cap_diag_emitted: bool,
    /// Per-call-site recorded concrete type arguments. Populated by
    /// `record_call_site_type_args` whenever a `HirExprKind::Call` is
    /// produced for a generic top-level user-fn callee. Drained into
    /// `HirModule.call_site_type_args` at end of `lower_program`.
    ///
    /// Includes "still-abstract" entries (where the callee is generic
    /// and the call is itself inside a generic body, so the recorded
    /// args contain the enclosing fn's type-parameter symbols). MIR
    /// lowering substitutes those symbols via the per-monomorphisation
    /// substitution map; the registry's closure-under-substitution pass
    /// uses the same data to discover inner monomorphisations.
    call_site_type_args: HashMap<SiteId, Vec<ResolvedTy>>,
    /// Pre-collected record/type-decl shape registry. Populated in the
    /// type-decl pre-pass before function bodies lower, so that
    /// `Expr::StructInit` lowering can answer "is this a generic user
    /// record?" and look up its source field shape. Tuple-form records
    /// land here with `fields = []`; they never trigger record-layout
    /// emission (their constructor goes through `Expr::Call`).
    record_registry: HashMap<String, RecordEntry>,
    /// Nominal type names appearing in the signature (params or return) of an
    /// `#[extern_symbol]` impl method, collected at impl lowering — the only
    /// stage where the attribute is still visible (the lowered `HirFn` carries
    /// no extern marker). Stored in the bare spelling and, when lowered under
    /// a module context, the module-qualified spelling, matching the two
    /// `record_registry` keys. Consumed by
    /// `finalize_user_record_value_classes` to admit a zero-field record that
    /// is provably FFI-backed as a `BitCopy` handle stand-in while a bare
    /// `type Empty {}` stays fail-closed uninferred.
    extern_backed_record_names: HashSet<String>,
    /// Bare imported record names that have distinct definitions in more than
    /// one canonical module and therefore require module-qualified identities.
    colliding_imported_record_names: HashSet<String>,
    /// Dotted paths of module-graph modules whose items were spliced into the
    /// root program by `flatten_file_import_items` (file imports). A file
    /// import publishes its types into the ROOT namespace with bare identity —
    /// root constructs and dispatches them bare — so a bare self-record
    /// reference inside a file-import module must NOT be owner-qualified the
    /// way a genuine package-module reference is (#2208). Package modules are
    /// absent from this set and DO owner-qualify.
    file_import_module_names: HashSet<String>,
    /// Bare record/type-decl names declared by more than one declaring scope
    /// across the whole program (any two of: a package module, a file-import
    /// module, or the root). ONLY these genuinely-colliding names are
    /// owner-qualified inside their declaring package module: a name unique to
    /// one module (e.g. stdlib `xml.Node`) keeps its bare identity, byte-
    /// identical to the pre-#2208 behaviour, so the qualification never touches
    /// records that have no cross-module ambiguity. Unlike
    /// `colliding_imported_record_names` (which counts only 2+ PACKAGE modules,
    /// per `imported_type_name_collides`), this set ALSO counts a file-import /
    /// root declaration as a colliding scope — the mixed file-import + package
    /// same-bare-name shape the actor-ask coupling depends on.
    cross_module_colliding_record_names: HashSet<String>,
    /// Checker-resolved type arguments for generic record-init sites
    /// (`R { ... }` against a `pub type R<T>` or `record R<T>`),
    /// keyed by the struct-init expression span.
    ///
    /// Consumed at `Expr::StructInit` lowering: every init site whose
    /// record has non-empty `type_params` is paired with the entry here
    /// and inserted into `record_layout_registry`. A poisoned entry
    /// (failing the `ResolvedTy::from_ty` boundary conversion) emits
    /// `RecordLayoutTypeArgsViolation`; a generic init site with no
    /// entry at all is skipped (the checker only records sites whose
    /// args resolved fully concretely).
    /// (LESSONS: `checker-authority` P0, `end-to-end-before-layer-thickening` P1)
    record_init_type_args: HashMap<SpanKey, Vec<Ty>>,
    /// Distinct user-record instantiations observed at `StructInit`
    /// sites, accumulated as expression lowering walks the program.
    /// Drained into `HirModule.record_layouts` at the end of
    /// `lower_program`. Cap is configurable via
    /// `lower_program_with_mono_cap` (shared with the fn registry).
    record_layout_registry: RecordLayoutRegistry,
    /// Tracks whether the `RecordLayoutCapExceeded` diagnostic has
    /// already been emitted for this lowering invocation.
    record_layout_cap_diag_emitted: bool,
    /// Per-enum type-parameter names, keyed by enum type name. Populated
    /// during the type-decl second pass alongside `enum_variants_by_name`.
    /// Consumed by the enum-layout discovery pass to know which names in a
    /// variant's field types are type-parameter symbols vs. concrete named
    /// types, so `substitute_type_params` can substitute correctly.
    enum_type_params: HashMap<String, Vec<String>>,
    /// Per-enum `ItemId`, keyed by enum type name. Populated alongside
    /// `enum_type_params` so the `EnumMonoKey.origin` field can be set to
    /// the HIR-allocated `ItemId` of the originating enum declaration rather
    /// than a synthetic value.
    enum_item_ids: HashMap<String, ItemId>,
    /// Distinct user-enum instantiations observed at enum-ctor sites and
    /// match scrutinees, accumulated during HIR lowering. Drained into
    /// `HirModule.enum_layouts` at the end of `lower_program`. Cap is
    /// shared with the fn and record registries.
    enum_layout_registry: EnumLayoutRegistry,
    /// Checker-authoritative mapping from qualified function name to the
    /// intrinsic catalog key declared via `#[intrinsic("key")]`. Functions
    /// present here must be validated against `stdlib_catalog` and must have
    /// their bodies skipped during lowering (the body is a placeholder).
    /// Fail-closed: an unknown key emits `UnknownIntrinsic`.
    intrinsic_declarations: HashMap<String, String>,
    /// Checker-side supervisor child-slot table, keyed by the `SpanKey` of
    /// each `FieldAccess` expression that the checker resolved as a supervisor
    /// child accessor. Cloned from `tc_output.supervisor_child_slots` at
    /// construction. Read-only during lowering; lookups translate the span key
    /// to the pre-allocated `SiteId` and accumulate into `supervisor_child_slots`.
    supervisor_child_slots_checker: HashMap<SpanKey, ChildSlot>,
    /// Per-field-access `SiteId` → `ChildSlot` accumulator. Populated during
    /// `HirExprKind::FieldAccess` lowering whenever
    /// `supervisor_child_slots_checker` contains an entry for the expression
    /// span. Drained into `HirModule.supervisor_child_slots` at the end of
    /// `lower_program` (mirrors the `call_site_type_args` pattern).
    supervisor_child_slots: HashMap<SiteId, ChildSlot>,
    /// Checker-side static-pool accessor table (`sup.pool[i]` / `.get(i)` /
    /// `.len()`), keyed by the OUTER expr `SpanKey`. Cloned from
    /// `tc_output.pool_accessor_sites` at construction.
    pool_accessor_sites_checker: HashMap<SpanKey, hew_types::PoolAccessor>,
    /// Per-accessor `SiteId` → `PoolAccessor` accumulator. Populated during
    /// `Index` / `MethodCall` lowering whenever `pool_accessor_sites_checker`
    /// has an entry. Drained into `HirModule.pool_accessor_sites`.
    pool_accessor_sites: HashMap<SiteId, hew_types::PoolAccessor>,
    /// Module-level regex literal table. Accumulates distinct compiled
    /// patterns observed in match arms (and standalone `re"..."` expressions).
    /// Deduplicated by raw pattern string equality (no flags in v0.5).
    /// Each entry's `literal_id` matches its 0-based index in this `Vec`.
    ///
    /// Drained into `HirModule.regex_literals` at the end of `lower_program`.
    regex_literals: Vec<HirRegexLiteral>,
    /// Deduplication index for `regex_literals`: maps pattern string to its
    /// allocated `literal_id`. Lookups via `alloc_regex_literal` avoid
    /// scanning the `Vec` linearly.
    regex_literal_index: HashMap<String, u32>,
    /// Module-scope registry of tagged-union unit constructors, keyed by the
    /// surface identifier the user writes at the construction site. Covers
    /// three surface forms that share one tagged-union substrate:
    ///   1. Machine states (`TrafficLight::Red`, bare `Red`).
    ///   2. Machine event companions (`TrafficLightEvent::Tick`, bare `Tick`).
    ///   3. User-defined enum unit variants (`Colour::Red`, bare `Red`).
    ///
    /// Built by a pre-pass over `program.items` before any function body is
    /// lowered so declaration order does not constrain resolution.
    ///
    /// Two key shapes are stored for every unit constructor:
    ///   - **Qualified**: `"<TaggedUnionType>::<Variant>"` (always registered).
    ///   - **Bare**: `"<Variant>"`, registered only when the variant name is
    ///     **unambiguous** across all three surface forms in the module.
    ///     Ambiguous bare names are omitted so the lexical/`fn_registry`
    ///     fall-through preserves user-binding precedence.
    ///
    /// Each value is `(tagged_union_typename, variant_idx)`. `variant_idx` is
    /// the declaration-order index among all variants of that type (including
    /// payload-bearing ones, which are not registered here but occupy slots).
    ///
    /// Consumed by `lower_identifier` to produce `HirExprKind::MachineVariantCtor`
    /// instead of an unresolved `BindingRef` when the user names a unit
    /// constructor at module scope (`var light = Red;`, `light.step(Tick);`,
    /// `let next = TrafficLight::Green;`, `let c = Colour::Red;`).
    machine_ctor_registry: HashMap<String, (String, usize)>,
    /// Maps module-level `const` name → pre-allocated `ItemId` + declared type.
    /// Populated in the first pass so const references resolve to a stable id
    /// regardless of source order. See [`ConstEntry`].
    const_registry: HashMap<String, ConstEntry>,
    /// Same-module integer const values folded earlier in source order.
    /// Populates `ConstEnv` for subsequent const initializers; values are not
    /// used for ordinary expression lowering, which continues to resolve const
    /// references through `const_registry`.
    folded_integer_consts: HashMap<String, i128>,
    /// Per-enum variant descriptors keyed by the enum's type name. Populated
    /// between the type-decl second pass and the source-order third pass so
    /// `Expr::Call` (tuple variant ctors like `Shape::Line(5)`) and
    /// `Expr::StructInit` (struct variant ctors like `Shape::Box { w, h }`)
    /// lowering can dispatch on the variant's `HirVariantKind` without
    /// re-walking the parser AST. The vec is in declaration order, so
    /// `machine_ctor_registry`'s `(type_name, variant_idx)` indexes directly
    /// into it.
    enum_variants_by_name: HashMap<String, Vec<HirVariant>>,
    /// Names of every enum declared `indirect`, keyed exactly as
    /// `enum_variants_by_name` is. A generic instantiation of one keeps the
    /// heap shape: its variables and self-referential payloads are pointers.
    indirect_enum_names: HashSet<String>,
    /// Structural member types per named type, in declaration order: record
    /// fields, plus every enum variant's payload types. Populated alongside
    /// `type_classes` in the type-decl pre-pass, so it is complete before any
    /// function body lowers.
    ///
    /// `resolved_ty_transfers_ownership_to_mailbox` walks it to see THROUGH an
    /// aggregate: a plain record carries no ownership marker of its own, but a
    /// `#[resource]` field inside it still transfers when the record crosses a
    /// mailbox.
    type_member_tys: HashMap<String, Vec<ResolvedTy>>,
    /// Checker-resolved per-arm pattern classifications. Cloned from
    /// `tc_output.pattern_resolutions` at construction. Keyed by the
    /// `SpanKey` of each match arm's pattern span. Consumed by
    /// `Expr::Match` lowering to map arms to their resolved enum variant
    /// (or wildcard) without re-resolving names against the type registry.
    pattern_resolutions: HashMap<SpanKey, hew_types::ArmResolution>,
    /// Checker-authored canonical record-pattern plans keyed by pattern span.
    pattern_plans: HashMap<SpanKey, hew_types::PatternPlan>,
    /// Compiler-recognised lang-item registry surfaced by the checker.
    ///
    /// HIR f-string lowering looks up the `Display` trait method here
    /// (`LANG_ITEM_DISPLAY_FMT` → method name) instead of hard-coding
    /// `"fmt"`. Without an entry the lowering pass refuses to fabricate
    /// the dispatch — surfacing a fail-closed diagnostic.
    lang_items: hew_types::LangItemRegistry,
    /// Target architecture for compilation. Used by target gates to reject
    /// constructs that would panic at runtime on unsupported targets
    /// (P0.1-P0.4 fail-closed gates per slepp A222).
    target_arch: TargetArch,
    /// W3.042 Stage 2 — when lowering bodies of methods that live inside an
    /// `impl ... for <SelfType> { ... }` (or inherent `impl <SelfType>`) block,
    /// `Self` in any annotated `TypeExpr` position must resolve to the
    /// concrete self type rather than escaping to MIR as a literal
    /// `ResolvedTy::named_path(&self.defs, &"Self", _)` (which has no entry in the
    /// record-field-order table and fail-closes at MIR boundary).
    ///
    /// Set by `lower_impl_block` before lowering each method, cleared after.
    /// `None` outside an impl-method context — top-level free functions, trait
    /// declarations, and actor/machine method lowerings do not touch this.
    current_impl_self_ty: Option<ResolvedTy>,
    /// Declared generic type-parameter names of the function whose body is
    /// currently being lowered (impl-level params concatenated with the
    /// method's own). Set by `lower_fn_with_name_and_impl_params` around the
    /// body, restored after. Lets lowering recognise a bare `Named { name,
    /// args: [] }` operand as an abstract type parameter (the checker lowers
    /// `T` to `ResolvedTy::Named`, not `ResolvedTy::TypeParam`) so generic
    /// `Display` dispatch can defer to monomorphisation. Empty outside a
    /// function body.
    current_fn_type_params: HashSet<String>,
    /// Checker-selected trait bindings and default source bodies.
    trait_bindings: HashMap<(Option<String>, u32, String), hew_types::DefId>,
    trait_defaults: HashMap<hew_types::DefId, Vec<hew_types::ResolvedTraitDefault>>,
    /// Source-declared type names visible in the root namespace, including
    /// declarations flattened from file imports. These identities must be
    /// considered before the compiler-only `Task`, `Unit`, and
    /// `CancellationToken` fallbacks, including for generic source types.
    root_visible_source_type_short_names: HashSet<String>,
    /// Bare→qualified identity projection for declarations reaching root
    /// through a flat file import (`import "x.hew";`). The spliced items share
    /// the root bare namespace at the source surface, but their declaration
    /// identity — and every layout registry key derived from it — is the
    /// defining file's `{module}.{name}`. Root annotations resolve through
    /// this table so binding types and layout keys agree.
    file_import_root_type_aliases: HashMap<String, String>,
    /// Exact `{module_owner}.{type}` identities declared by non-root modules.
    /// This lets the three compiler-special spellings retain their source
    /// identity inside a declaring module and through named imports without
    /// broadening the general builtin-shadow rules.
    source_type_identities: HashSet<String>,
    /// Source declarations owned by canonical `std.*` modules, keyed with the
    /// same full owner identity as `source_type_identities`. This provenance
    /// distinguishes shipped source carriers from a user package with the same
    /// leaf module and type spelling.
    canonical_std_source_type_identities: HashSet<String>,
    /// Exact nominal identities accepted by the checker. This proves that a
    /// self-qualified spelling really belongs to the full current owner;
    /// downstream lowering never recovers that proof from a short name.
    checked_type_defs: HashMap<String, hew_types::check::TypeDef>,
    /// Mirrors `Checker::current_module_idx`: 0 for root items, N for the N-th
    /// non-root module's items (1-based, matching topo order).  Used by
    /// `mk_key` to produce module-scoped `SpanKey` lookups that agree with
    /// the checker's module-scoped inserts, preventing byte-offset collisions
    /// across stdlib files (L23 defect root cause).
    current_module_idx: u32,
    /// Source-order discriminator used only when a source-less AST surface
    /// gives every top-level item the synthetic `0..0` span.
    current_item_ordinal: usize,
    /// `ItemId`s of function items PROVABLY lowered from the root compilation
    /// unit's own source (recorded when `current_module_idx == 0` AND
    /// `lowering_injected_items` is false), EXCLUDING generated trait
    /// default-method bodies (whose bodies are copied from the trait
    /// declaration and index the trait's source, which may be an imported
    /// module). Moved onto [`HirModule::root_item_ids`] at construction; codegen
    /// resolves each function's `SourceOrigin` from it so a fail-closed caret is
    /// rendered against the root source ONLY for a proven-root function. A
    /// positive record — never inferred by absence-from-a-foreign-set.
    root_item_ids: HashSet<ItemId>,
    /// True while lowering items that are INJECTED at root `current_module_idx`
    /// but do NOT index the user's root source — currently the `std/builtins.hew`
    /// callable impls (and the Vec iterator harness), which are lowered
    /// out-of-band at module index 0 rather than through `module_graph`. Their
    /// spans index `std/builtins.hew`, so they must be kept OUT of
    /// `root_item_ids` even though `current_module_idx == 0`: a codegen
    /// fail-closed reachable in a builtin method must degrade to a bare line, not
    /// render a false caret against the user's root source. Gates the
    /// `root_item_ids` inserts alongside the `current_module_idx == 0` check.
    lowering_injected_items: bool,
    /// Full dotted path of the module currently being lowered (e.g.
    /// `"subpkg.helper"`): `None` for root items, `Some(path.join("."))` for
    /// imported package/file modules.  Mirrors the checker's
    /// `Checker::current_module` EXACTLY (same `mod_id.path.join(".")`
    /// derivation) so the per-module alias map key in `resolve_named_type_ref`
    /// and sibling alias lookups agrees with the checker's inserts for depth-≥2
    /// importers; the short last segment would diverge and miss.
    current_module_name: Option<String>,
    /// Exact checker-minted source module selected by the parser's per-file
    /// span index. A directory module's peer file therefore retains its own
    /// `ModuleId` even while `current_module_name` remains the assembled module
    /// spelling used for lexical resolution.
    declaration_module_by_file_index: HashMap<u32, hew_types::ModuleId>,
    /// Immutable checker declaration authority. This view cannot mint.
    defs: std::sync::Arc<hew_types::DefTable>,
    type_aliases: HashMap<hew_types::DefId, hew_types::TypeAliasDef>,
    /// Checker-authoritative import resolution table: maps `(importer_module,
    /// source spelling)` → canonical qualified source identity for named/glob
    /// imports and canonical lifecycle whole-module aliases.
    ///
    /// Sourced from [`hew_types::check::TypeCheckOutput::import_type_name_aliases`]
    /// at `LowerCtx::new` time and consulted in:
    /// - `resolve_named_type_ref`: type-annotation position (`fn f(x: Tag)` or
    ///   `fn f(x: lifecycle.CrashNotification)`).
    /// - `lookup_variant_ctor`: `Tag::Variant` enum-constructor paths.
    ///
    /// Per-module keying prevents a same-named alias from a different imported
    /// module from hijacking the lookup (last-write-wins flat map defect).
    /// Type references consult the source binding before the builtin catalog,
    /// so an explicitly imported user `Stream` cannot become the pipe
    /// half. Local-shadow filtering remains checker-authoritative.
    import_type_name_aliases: HashMap<(Option<String>, u32, String), String>,
    /// Exact owner identities for lexical module qualifiers. Both whole and
    /// selective module-path imports carry this fact: after
    /// `import hew::closableerr::{ Closable as C }`,
    /// `closableerr.CloseError` still resolves to
    /// `hew.closableerr.CloseError` without a leaf-name retry.
    module_import_bindings: HashMap<(Option<String>, u32, String), String>,
    /// Exact owner identities for the bare constant bindings an import
    /// published, keyed by the file that wrote the import. A file the root
    /// pulled in with `import "sub.hew";` is spliced into `program.items`, so
    /// its own `import lib.{ LIB_K };` never reaches HIR as an item; this fact
    /// is how a bare `LIB_K` in that file resolves to `lib.LIB_K` under the
    /// same scope the checker admitted it in.
    published_bare_const_owners:
        HashMap<(Option<String>, u32, String), std::collections::BTreeSet<String>>,
    /// Exact owner identities for the bare function bindings an import
    /// published, keyed by the file that wrote the import. The companion of
    /// `published_bare_const_owners`; see `resolved_bare_function_symbol`.
    import_fn_name_aliases: HashMap<(Option<String>, u32, String), String>,
    /// Root-scope value bindings the program itself declares. A root
    /// declaration outranks a name an import published into the root scope.
    root_value_bindings: HashSet<String>,
}

/// Whether `ty` transitively carries a value whose SOLE ownership crosses an
/// actor message boundary: a substrate handle (the builtin list owned by
/// [`hew_types::BuiltinType::transfers_ownership_across_actor_boundary`],
/// which the env checker reads too, so the two ownership authorities cannot
/// drift), or a user `#[resource]` / `#[linear]` declaration.
///
/// The nominal arm dispatches on `type_classes`, never on the bare source
/// name, and only for `builtin: None` types — a user `record Sink` keeps
/// ordinary copy treatment while the real builtin handle transfers.
///
/// Actor references (`ActorHandle`, `BoxedActor`, `ActorFn`, `MonitorRef`)
/// carry the `Resource` MARKER for drop elaboration but are shareable
/// addresses; sending a pid must not consume the sender's own handle, so they
/// are excluded on both sides.
///
/// Record/enum FIELD recursion is deliberately absent: an aggregate hiding a
/// handle still reaches the serialiser walk and fails closed there.
/// Positional payload types of a variant, for the mailbox-transfer member set.
fn hew_hir_variant_field_tys(variant: &HirVariant) -> Vec<ResolvedTy> {
    variant.field_tys()
}

fn resolved_ty_transfers_ownership_to_mailbox(
    ty: &ResolvedTy,
    type_classes: &crate::value_class::TypeClassTable,
    type_member_tys: &HashMap<String, Vec<ResolvedTy>>,
) -> bool {
    let mut visiting = HashSet::new();
    transfers_ownership_to_mailbox_guarded(ty, type_classes, type_member_tys, &mut visiting)
}

/// Recursion body for [`resolved_ty_transfers_ownership_to_mailbox`].
///
/// `visiting` makes the walk total over recursive type graphs
/// (`type Node { next: Vec<Node> }`). Re-entering a name already on the stack
/// contributes no NEW ownership edge, so it answers `false` — the neutral
/// element of the `any(...)` disjunction — and the verdict is decided by the
/// non-recursive members.
fn transfers_ownership_to_mailbox_guarded(
    ty: &ResolvedTy,
    type_classes: &crate::value_class::TypeClassTable,
    type_member_tys: &HashMap<String, Vec<ResolvedTy>>,
    visiting: &mut HashSet<String>,
) -> bool {
    match ty {
        ResolvedTy::CancellationToken => true,
        ResolvedTy::Named { head, args, .. } => {
            let name = head.registry_key();
            let builtin = head.builtin();
            if builtin
                .is_some_and(hew_types::BuiltinType::transfers_ownership_across_actor_boundary)
            {
                return true;
            }
            // A source-declared lifecycle type reaches some positions spelled
            // by its qualified path with no `builtin` tag attached, so the tag
            // test alone misses it. Only the DOTTED spelling resolves here: a
            // bare user `type MonitorRef` shadow is a clone-total record and
            // must keep ordinary value semantics.
            if name.contains('.')
                && hew_types::lookup_builtin_type(name)
                    .is_some_and(hew_types::BuiltinType::transfers_ownership_across_actor_boundary)
            {
                return true;
            }
            if builtin.is_none()
                && matches!(
                    type_classes.get(name).map(|(marker, _)| *marker),
                    Some(ResourceMarker::Resource | ResourceMarker::Linear)
                )
            {
                return true;
            }
            if args.iter().any(|arg| {
                transfers_ownership_to_mailbox_guarded(arg, type_classes, type_member_tys, visiting)
            }) {
                return true;
            }
            // A builtin carries no user member set to descend into, and its
            // ownership verdict is already decided above.
            if builtin.is_some() || !visiting.insert(name.to_string()) {
                return false;
            }
            let carries = type_member_tys.get(name).is_some_and(|members| {
                members.iter().any(|member| {
                    transfers_ownership_to_mailbox_guarded(
                        member,
                        type_classes,
                        type_member_tys,
                        visiting,
                    )
                })
            });
            visiting.remove(name);
            carries
        }
        ResolvedTy::Tuple(elements) => elements.iter().any(|element| {
            transfers_ownership_to_mailbox_guarded(element, type_classes, type_member_tys, visiting)
        }),
        ResolvedTy::Array(inner, _) | ResolvedTy::Slice(inner) => {
            transfers_ownership_to_mailbox_guarded(inner, type_classes, type_member_tys, visiting)
        }
        _ => false,
    }
}

#[cfg(test)]
mod tests;

#[cfg(test)]
mod builtin_enum_catalog_identity_tests;
