use hew_parser::ast::Span;
use hew_types::ResolvedTy;

use crate::ids::{BindingId, HirNodeId, ResolvedRef, SiteId};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirDiagnostic {
    pub kind: HirDiagnosticKind,
    pub span: Span,
    pub note: String,
}

impl HirDiagnostic {
    #[must_use]
    pub fn new(kind: HirDiagnosticKind, span: Span, note: impl Into<String>) -> Self {
        Self {
            kind,
            span,
            note: note.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirDiagnosticKind {
    /// A syntactic construct that is outside the current vertical slice.
    /// The `construct` names the unsupported form; `slice_target` says which
    /// slice is planned to add support.  Fail-closed: the driver must stop on
    /// the first `CutoverUnsupported` diagnostic.
    CutoverUnsupported {
        construct: String,
        slice_target: String,
    },
    UnresolvedSymbol {
        name: String,
    },
    UnresolvedInferenceVar,
    DuplicateBindingId {
        id: BindingId,
    },
    DuplicateSiteId {
        id: SiteId,
    },
    DuplicateNodeId {
        id: HirNodeId,
    },
    DanglingRef {
        resolved: ResolvedRef,
    },
    ReturnTypeMismatch {
        expected: ResolvedTy,
        actual: ResolvedTy,
    },
    /// `#[resource]` type body has no method named `close` declared with
    /// `consuming self`. The implicit-drop contract for `@resource` types
    /// requires this method; missing it is a fail-closed compile error.
    ResourceMissingClose {
        name: String,
    },
    /// `#[linear]` type body declares zero `consuming self` methods. A
    /// linear type with no consuming methods cannot be exhausted on an
    /// exit path, which would make `MirCheck::MustConsume` unable to fire
    /// — caught here as a structural error.
    LinearNoConsumingMethods {
        name: String,
    },
    /// `#[resource]` or `#[linear]` type carries type parameters. Generic
    /// resource/linear types are out of scope for the v0.5 vertical slice
    /// (the marker→ValueClass mapping is keyed by type name, not by
    /// instantiated type).
    ResourceGenericUnsupported {
        name: String,
    },
}
