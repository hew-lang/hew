//! Interned, provenance-invariant compile-time identity.
//!
//! The [`DefTable`] is the single minting authority for module and
//! declaration identity in a compile. It is owned by the checker, minted once
//! at `check_program` entry from the module graph, and published in
//! `TypeCheckOutput` so later pipeline stages consume identity instead of
//! re-deriving it from spellings.
//!
//! A dual-imported or root-vs-imported source resolves to one module identity,
//! and an exact source declaration resolves to one `DefId`. Downstream code
//! must carry these identities rather than reconstructing them from names.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::NominalInstance;
use hew_parser::ast::Symbol;

/// Closed classification of source declarations and source-owned child bodies.
///
/// Together with [`DeclarationOccurrence`], this distinguishes declarations
/// which share an enclosing item span (actor and machine children). Adding a
/// new source declaration form therefore requires an explicit identity design
/// choice instead of silently falling back to a name-derived identity.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DeclarationKind {
    Const,
    Function,
    ExternFunction,
    Type,
    TypeAlias,
    Record,
    Trait,
    TraitMethod,
    TypeMethod,
    ImplMethod,
    Actor,
    ActorInit,
    ActorReceive,
    ActorMethod,
    Supervisor,
    SupervisorBootstrap,
    Machine,
    MachineState,
    MachineEvent,
    MachineStateEntry,
    MachineStateExit,
    MachineTransition,
    /// An enum variant, a member of its enum.
    Variant,
    /// A compiler-provided codec entry point of a wire type (`decode`,
    /// `from_json`): a sourceless member of the type.
    CodecMethod,
    /// A sourceless builtin receiver anchor (`i64`, `Vec`).
    BuiltinType,
    /// A trait default body materialized for one concrete receiver.
    DefaultImplMethod,
    /// The request record the checker synthesizes for a handler with no
    /// source declaration of its own (a lambda actor's `call`).
    RequestProtocol,
}

/// Exact source occurrence of a declaration.
///
/// `item_start..item_end` identifies the top-level parsed item in one source
/// module. `ordinal` is zero for the item itself and is the source-order index
/// among children of the same [`DeclarationKind`]. It is deliberately not a
/// display name: renamed import routes and aliases must converge.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DeclarationOccurrence {
    module: Option<ModuleId>,
    item_start: usize,
    item_end: usize,
    synthetic_item_ordinal: u32,
    kind: DeclarationKind,
    ordinal: u32,
}

impl DeclarationOccurrence {
    #[must_use]
    pub fn new(
        module: Option<ModuleId>,
        item_span: &std::ops::Range<usize>,
        kind: DeclarationKind,
        ordinal: usize,
    ) -> Self {
        Self::new_with_synthetic_ordinal(module, item_span, 0, kind, ordinal)
    }

    /// Construct an occurrence with an explicit discriminator for source-less
    /// AST inventories whose spans are synthetic (`0..0`). Real source spans
    /// deliberately ignore this value so route-dependent module assembly
    /// order can never split one physical declaration.
    ///
    /// # Panics
    ///
    /// Panics if either ordinal exceeds the compiler's `u32` occurrence
    /// representation.
    #[must_use]
    pub fn new_with_synthetic_ordinal(
        module: Option<ModuleId>,
        item_span: &std::ops::Range<usize>,
        synthetic_item_ordinal: usize,
        kind: DeclarationKind,
        ordinal: usize,
    ) -> Self {
        Self {
            module,
            item_start: item_span.start,
            item_end: item_span.end,
            synthetic_item_ordinal: if item_span.is_empty() {
                u32::try_from(synthetic_item_ordinal)
                    .expect("more than u32::MAX synthetic source declarations in one module")
            } else {
                0
            },
            kind,
            ordinal: u32::try_from(ordinal)
                .expect("more than u32::MAX same-kind child declarations in one item"),
        }
    }

    #[must_use]
    pub fn module(self) -> Option<ModuleId> {
        self.module
    }

    /// Attach the checker-interned source module to a frontend-selected
    /// occurrence. Discovery happens before the checker owns its module table,
    /// so root selections deliberately arrive with no module and are completed
    /// exactly once at the checker boundary.
    #[must_use]
    pub fn with_module(self, module: Option<ModuleId>) -> Self {
        Self { module, ..self }
    }

    #[must_use]
    pub fn kind(self) -> DeclarationKind {
        self.kind
    }

    /// The parsed item's source span, for diagnostics that point back at an
    /// already-established declaration.
    #[must_use]
    pub fn span(self) -> std::ops::Range<usize> {
        self.item_start..self.item_end
    }
}

/// Two distinct source declarations claimed one canonical path.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DeclarationIdentityError {
    PathAlreadyDeclared {
        path: String,
        established_occurrence: DeclarationOccurrence,
        conflicting_occurrence: DeclarationOccurrence,
    },
    /// One declaration occurrence offered a second canonical spelling.
    SecondSpelling {
        established: String,
        conflicting: String,
    },
}

impl std::fmt::Display for DeclarationIdentityError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::PathAlreadyDeclared { path, .. } => {
                write!(
                    f,
                    "declaration path `{path}` was claimed by two source declarations"
                )
            }
            Self::SecondSpelling {
                established,
                conflicting,
            } => write!(
                f,
                "declaration `{established}` was offered a second spelling `{conflicting}`"
            ),
        }
    }
}

impl std::error::Error for DeclarationIdentityError {}

/// The declaration paths a lambda actor (`actor |msg| { .. }`) is minted under.
///
/// A lambda actor has no source name, so its identity is keyed by the exact
/// source span of its `actor` expression: unique within a module by
/// construction, and stable across every stage that re-derives it. HIR
/// synthesizes the actor declaration these identities belong to.
#[must_use]
pub fn lambda_actor_declaration_path(
    module: Option<&str>,
    span: &std::ops::Range<usize>,
) -> String {
    let leaf = format!("#lambda@{}_{}", span.start, span.end);
    module.map_or(leaf.clone(), |module| format!("{module}.{leaf}"))
}

/// The single receive handler of a lambda actor, under its actor's path.
#[must_use]
pub fn lambda_actor_handler_path(actor_path: &str) -> String {
    format!("{actor_path}::call")
}

/// Identity of one declared definition: a row index into the compilation's
/// [`DefTable`].
///
/// A `DefId` is meaningful only against the table that minted it. It carries
/// no spelling: diagnostics render it with [`DefTable::display`], linker
/// symbols with [`DefTable::path`].
///
/// # Compile-time boundary
///
/// A downstream layer cannot mint a definition identity, from a spelling or
/// from an index:
///
/// ```compile_fail
/// use hew_types::DefId;
///
/// let _identity = DefId::new("Widget");
/// ```
///
/// ```compile_fail
/// use hew_types::DefId;
///
/// let _identity = DefId(3);
/// ```
///
/// A spelling is not a key into an identity-keyed table:
///
/// ```compile_fail
/// use hew_types::DefId;
///
/// let table: std::collections::HashMap<DefId, ()> = std::collections::HashMap::new();
/// let _ = table.get("Widget");
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DefId(u32);

impl DefId {
    fn index(self) -> usize {
        self.0 as usize
    }

    /// The row index, for orderings that must not depend on anything but
    /// the table. Never a key: an index means nothing outside its table.
    pub(crate) fn index_u32(self) -> u32 {
        self.0
    }
}

/// Canonical identity of a declared nominal type.
///
/// A nominal is backed by the declaration identity, so two `Box` declarations
/// from different modules can never compare equal merely because they share a
/// leaf spelling.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NominalId {
    declaration: DefId,
}

impl NominalId {
    #[must_use]
    pub(crate) fn from_minted_declaration(declaration: DefId) -> Self {
        Self { declaration }
    }

    /// The nominal identity of a declared type, actor or machine.
    #[must_use]
    pub fn of_declaration(declaration: DefId) -> Self {
        Self { declaration }
    }

    #[must_use]
    pub fn declaration(self) -> DefId {
        self.declaration
    }
}

/// Identity of one generic binder: the declaration that binds it and its
/// position in that declaration's parameter list.
///
/// A binder is never looked up by its spelling: `fn g<T>(x: T)` and a nominal
/// `type T` in the same module are distinct identities, so a parameter cannot
/// resolve to a nominal of the same name.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeParamId {
    pub owner: DefId,
    pub index: u16,
}

impl TypeParamId {
    /// The `index`-th parameter bound by `owner`.
    ///
    /// # Panics
    ///
    /// Panics if a declaration binds more than `u16::MAX` parameters.
    #[must_use]
    pub fn new(owner: DefId, index: usize) -> Self {
        Self {
            owner,
            index: u16::try_from(index).expect("more than u16::MAX type parameters on one item"),
        }
    }
}

/// The compiler predicates: marker traits the compiler decides structurally.
/// None has a source declaration; each is one sourceless row minted when the
/// table is created and bound in the prelude by its spelling, so a user trait
/// spelled like a predicate is a different identity (D554 rule 2).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Predicate {
    Send,
    Sync,
    Frozen,
    Copy,
    Clone,
    Eq,
    PartialOrd,
    Ord,
    Num,
    Hash,
    Debug,
    Decode,
    Encode,
    Serializable,
    Resource,
}

impl Predicate {
    pub const ALL: [Self; 15] = [
        Self::Send,
        Self::Sync,
        Self::Frozen,
        Self::Copy,
        Self::Clone,
        Self::Eq,
        Self::PartialOrd,
        Self::Ord,
        Self::Num,
        Self::Hash,
        Self::Debug,
        Self::Decode,
        Self::Encode,
        Self::Serializable,
        Self::Resource,
    ];

    /// The prelude spelling the predicate is bound under.
    #[must_use]
    pub const fn spelling(self) -> &'static str {
        match self {
            Self::Send => "Send",
            Self::Sync => "Sync",
            Self::Frozen => "Frozen",
            Self::Copy => "Copy",
            Self::Clone => "Clone",
            Self::Eq => "Eq",
            Self::PartialOrd => "PartialOrd",
            Self::Ord => "Ord",
            Self::Num => "Num",
            Self::Hash => "Hash",
            Self::Debug => "Debug",
            Self::Decode => "Decode",
            Self::Encode => "Encode",
            Self::Serializable => "Serializable",
            Self::Resource => "Resource",
        }
    }
}

/// The std declarations the compiler constructs types of without resolving
/// a spelling: the actor delivery protocol, the uninhabited
/// `Never` and the collection cursors. Each has one row minted when the table
/// is created, which the source declaration adopts when it is inventoried, so
/// a type the compiler builds and the type the source names are one identity.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum KnownDecl {
    ActorError,
    Never,
    ActorMailbox,
    ActorPolicy,
    Message,
    SendFailure,
    Delivery,
    OnFull,
    RejectSend,
    WaitSend,
    DropNewestSend,
    ReplaceLatestSend,
    ActorRequest,
    ActorRequestOwner,
    ActorRequestAdmission,
    VecIter,
    HashMapIter,
    NodeConfig,
    PartitionPolicy,
}

impl KnownDecl {
    pub const ALL: [Self; 19] = [
        Self::ActorError,
        Self::Never,
        Self::ActorMailbox,
        Self::ActorPolicy,
        Self::Message,
        Self::SendFailure,
        Self::Delivery,
        Self::OnFull,
        Self::RejectSend,
        Self::WaitSend,
        Self::DropNewestSend,
        Self::ReplaceLatestSend,
        Self::ActorRequest,
        Self::ActorRequestOwner,
        Self::ActorRequestAdmission,
        Self::VecIter,
        Self::HashMapIter,
        Self::NodeConfig,
        Self::PartitionPolicy,
    ];

    /// The declaration's canonical path in `std.builtins`.
    #[must_use]
    pub const fn path(self) -> &'static str {
        match self {
            Self::ActorError => "std.builtins.ActorError",
            Self::Never => "std.builtins.Never",
            Self::ActorMailbox => "std.builtins.ActorMailbox",
            Self::ActorPolicy => "std.builtins.ActorPolicy",
            Self::Message => "std.builtins.Message",
            Self::SendFailure => "std.builtins.SendFailure",
            Self::Delivery => "std.builtins.Delivery",
            Self::OnFull => "std.builtins.OnFull",
            Self::RejectSend => "std.builtins.RejectSend",
            Self::WaitSend => "std.builtins.WaitSend",
            Self::DropNewestSend => "std.builtins.DropNewestSend",
            Self::ReplaceLatestSend => "std.builtins.ReplaceLatestSend",
            Self::ActorRequest => "std.builtins.ActorRequest",
            Self::ActorRequestOwner => "std.builtins.ActorRequestOwner",
            Self::ActorRequestAdmission => "std.builtins.ActorRequestAdmission",
            Self::VecIter => "std.builtins.VecIter",
            Self::HashMapIter => "std.builtins.HashMapIter",
            Self::NodeConfig => "std.builtins.NodeConfig",
            Self::PartitionPolicy => "std.link_monitor.PartitionPolicy",
        }
    }

    const fn leaf(self) -> &'static str {
        match self {
            Self::ActorError => "ActorError",
            Self::Never => "Never",
            Self::ActorMailbox => "ActorMailbox",
            Self::ActorPolicy => "ActorPolicy",
            Self::Message => "Message",
            Self::SendFailure => "SendFailure",
            Self::Delivery => "Delivery",
            Self::OnFull => "OnFull",
            Self::RejectSend => "RejectSend",
            Self::WaitSend => "WaitSend",
            Self::DropNewestSend => "DropNewestSend",
            Self::ReplaceLatestSend => "ReplaceLatestSend",
            Self::ActorRequest => "ActorRequest",
            Self::ActorRequestOwner => "ActorRequestOwner",
            Self::ActorRequestAdmission => "ActorRequestAdmission",
            Self::VecIter => "VecIter",
            Self::HashMapIter => "HashMapIter",
            Self::NodeConfig => "NodeConfig",
            Self::PartitionPolicy => "PartitionPolicy",
        }
    }

    /// The nominal identity of this declaration in every table.
    #[must_use]
    pub fn nominal(self) -> NominalId {
        NominalId::from_minted_declaration(DefTable::known(self))
    }

    /// The type head of this declaration. The collection cursors are
    /// compiler builtins whose layout the std declaration supplies.
    #[must_use]
    pub fn head(self) -> crate::TypeHead {
        match self {
            Self::VecIter => crate::TypeHead::Builtin(crate::BuiltinType::VecIter),
            Self::HashMapIter => crate::TypeHead::Builtin(crate::BuiltinType::HashMapIter),
            _ => crate::TypeHead::Nominal(crate::NominalHead::new(self.nominal(), self.path())),
        }
    }

    /// The known declaration spelled `leaf` in `std.builtins`.
    ///
    /// TRANSITION(P2): read only by the second checker run over the embedded
    /// builtin source, which re-declares the cursors at its own root; deleted
    /// with that run (B1).
    #[must_use]
    pub fn from_leaf(leaf: Symbol) -> Option<Self> {
        Self::ALL
            .into_iter()
            .find(|known| Symbol::intern(known.leaf()) == leaf)
    }

    /// The known declaration a builtin cursor is declared as.
    #[must_use]
    pub fn of_builtin(builtin: crate::BuiltinType) -> Option<Self> {
        match builtin {
            crate::BuiltinType::VecIter => Some(Self::VecIter),
            crate::BuiltinType::HashMapIter => Some(Self::HashMapIter),
            _ => None,
        }
    }

    /// The known declaration a nominal identity is, when it is one.
    #[must_use]
    pub fn of(nominal: NominalId) -> Option<Self> {
        Self::ALL
            .into_iter()
            .find(|known| known.nominal() == nominal)
    }
}

/// The builtin receivers a trait impl can anchor on without a source
/// declaration: primitives and the compiler-owned collection and handle
/// types. Each has one sourceless row minted when the table is created, so an
/// `impl Show for i64` and a call on an `i64` receiver key one identity.
///
/// TRANSITION(P2): deleted by A1 commit 2, when `TypeHead::Builtin` carries
/// the builtin itself and no receiver needs a declaration row.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BuiltinAnchor {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    Isize,
    Usize,
    F32,
    F64,
    Bool,
    Char,
    String,
    Bytes,
    Generator,
    Vec,
    HashMap,
    ChildRef,
    RemotePid,
    NodeId,
    Location,
}

impl BuiltinAnchor {
    const ALL: [Self; 23] = [
        Self::I8,
        Self::I16,
        Self::I32,
        Self::I64,
        Self::U8,
        Self::U16,
        Self::U32,
        Self::U64,
        Self::Isize,
        Self::Usize,
        Self::F32,
        Self::F64,
        Self::Bool,
        Self::Char,
        Self::String,
        Self::Bytes,
        Self::Generator,
        Self::Vec,
        Self::HashMap,
        Self::ChildRef,
        Self::RemotePid,
        Self::NodeId,
        Self::Location,
    ];

    /// The render this anchor's row carries: the keyword or builtin spelling
    /// an `impl` names it by.
    const fn spelling(self) -> &'static str {
        match self {
            Self::I8 => "i8",
            Self::I16 => "i16",
            Self::I32 => "i32",
            Self::I64 => "i64",
            Self::U8 => "u8",
            Self::U16 => "u16",
            Self::U32 => "u32",
            Self::U64 => "u64",
            Self::Isize => "isize",
            Self::Usize => "usize",
            Self::F32 => "f32",
            Self::F64 => "f64",
            Self::Bool => "bool",
            Self::Char => "char",
            Self::String => "string",
            Self::Bytes => "bytes",
            Self::Generator => "Generator",
            Self::Vec => "Vec",
            Self::HashMap => "HashMap",
            Self::ChildRef => "ChildRef",
            Self::RemotePid => "RemotePid",
            Self::NodeId => "NodeId",
            Self::Location => "Location",
        }
    }
}

/// Interned handle for one source module, independent of how the module was
/// reached (root unit, import, alias, dual-import). Index into the owning
/// [`DefTable`]; meaningless across tables.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(u32);

#[derive(Debug, Clone, PartialEq, Eq)]
struct ModuleEntry {
    /// Canonical dotted render (`std.process`, `mymod`). For every graph
    /// module this is its canonical dotted path; for the root unit it is the
    /// identity the same file would carry when imported (its file stem), so
    /// root and import compiles of one source mint one spelling.
    canonical_path: String,
    /// The interned source this identity was minted from, when it has one.
    /// Diagnostics that must name the actual FILE — a directory module's peer
    /// files share one assembled render — read it back here.
    canonical_source: Option<PathBuf>,
}

const SYNTHETIC_ROOT_PATH: &str = "#synthetic-root";

/// One declaration row.
#[derive(Debug, Clone, PartialEq, Eq)]
struct DefRow {
    /// Display leaf: the declared spelling.
    name: Symbol,
    kind: DeclarationKind,
    /// The source module the declaration occurs in; `None` for sourceless
    /// rows (builtin anchors, materialized trait defaults).
    module: Option<ModuleId>,
    /// The declaration this one is a member of: a method's type, trait or
    /// actor, a machine's states and events, a state's entry and exit hooks.
    owner: Option<DefId>,
    /// The occurrence that established the row; `None` when sourceless.
    site: Option<DeclarationOccurrence>,
    /// The canonical render linker symbols and diagnostics derive from. Kept
    /// verbatim as the checker spelled it, so symbols are independent of row
    /// order.
    path: String,
}

/// Key of a trait default materialized for one concrete receiver: the
/// declaring trait, the receiver instance and the method spelling.
type DefaultBodyKey = (DefId, NominalInstance, Symbol);

/// The per-compilation declaration table: the single minting authority for
/// module and declaration identity. The checker owns it mutably while it
/// inventories the program and publishes it as `TypeCheckOutput.defs`;
/// every later stage reads the same `Arc`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefTable {
    modules: Vec<ModuleEntry>,
    module_by_source: HashMap<PathBuf, ModuleId>,
    module_by_path: HashMap<String, ModuleId>,
    root: Option<ModuleId>,
    defs: Vec<DefRow>,
    by_occurrence: HashMap<DeclarationOccurrence, DefId>,
    /// TRANSITION(P2): deleted by A1 commit 3, when every consumer holds the
    /// id `Scope::resolve` returned instead of a rendered path.
    by_path: HashMap<String, DefId>,
    default_bodies: HashMap<DefaultBodyKey, DefId>,
    /// The std declaration each compiler builtin with a source declaration
    /// is (`CrashInfo` is `std.failure.CrashInfo`), bound when that
    /// declaration is established.
    builtin_declarations: HashMap<crate::BuiltinType, DefId>,
    /// Each owner's members by declared name; the first declaration of a
    /// name wins, and a duplicate is reported by the checker.
    members: HashMap<(DefId, Symbol), Vec<DefId>>,
}

impl Default for DefTable {
    fn default() -> Self {
        Self::new()
    }
}

impl DefTable {
    /// An empty table holding only the builtin receiver anchors.
    #[must_use]
    pub fn new() -> Self {
        let mut table = Self {
            modules: Vec::new(),
            module_by_source: HashMap::new(),
            module_by_path: HashMap::new(),
            root: None,
            defs: Vec::new(),
            by_occurrence: HashMap::new(),
            by_path: HashMap::new(),
            default_bodies: HashMap::new(),
            builtin_declarations: HashMap::new(),
            members: HashMap::new(),
        };
        for anchor in BuiltinAnchor::ALL {
            table.push_row(DefRow {
                name: Symbol::intern(anchor.spelling()),
                kind: DeclarationKind::BuiltinType,
                module: None,
                owner: None,
                site: None,
                path: anchor.spelling().to_string(),
            });
        }
        for predicate in Predicate::ALL {
            table.push_row(DefRow {
                name: Symbol::intern(predicate.spelling()),
                kind: DeclarationKind::Trait,
                module: None,
                owner: None,
                site: None,
                path: predicate.spelling().to_string(),
            });
        }
        for known in KnownDecl::ALL {
            let id = table.push_row(DefRow {
                name: Symbol::intern(known.leaf()),
                kind: DeclarationKind::Type,
                module: None,
                owner: None,
                site: None,
                path: known.path().to_string(),
            });
            table.by_path.insert(known.path().to_string(), id);
        }
        table
    }

    /// The row of a known `std.builtins` declaration: known rows follow the
    /// predicates, in [`KnownDecl::ALL`] order.
    ///
    /// # Panics
    ///
    /// Never: the fixed rows number far below `u32::MAX`.
    #[must_use]
    pub fn known(known: KnownDecl) -> DefId {
        DefId(
            u32::try_from(BuiltinAnchor::ALL.len() + Predicate::ALL.len())
                .expect("fixed row count fits u32")
                + known as u32,
        )
    }

    /// The row of a compiler predicate: predicates follow the builtin
    /// anchors, in [`Predicate::ALL`] order.
    ///
    /// # Panics
    ///
    /// Never: the fixed rows number far below `u32::MAX`.
    #[must_use]
    pub fn predicate(predicate: Predicate) -> DefId {
        DefId(
            u32::try_from(BuiltinAnchor::ALL.len()).expect("anchor count fits u32")
                + predicate as u32,
        )
    }

    /// The predicate a definition is, when it is one.
    ///
    /// # Panics
    ///
    /// Never: the fixed rows number far below `u32::MAX`.
    #[must_use]
    pub fn as_predicate(id: DefId) -> Option<Predicate> {
        let first = u32::try_from(BuiltinAnchor::ALL.len()).expect("anchor count fits u32");
        id.0.checked_sub(first)
            .and_then(|index| Predicate::ALL.get(index as usize).copied())
    }

    fn push_row(&mut self, row: DefRow) -> DefId {
        let id = DefId(
            u32::try_from(self.defs.len()).expect("more than u32::MAX declarations in one compile"),
        );
        if let Some(owner) = row.owner {
            self.members.entry((owner, row.name)).or_default().push(id);
        }
        self.defs.push(row);
        id
    }

    /// The member of `owner` declared as `name`: a method, a receive
    /// handler, a machine state.
    #[must_use]
    pub fn member(&self, owner: DefId, name: Symbol) -> Option<DefId> {
        self.members.get(&(owner, name))?.first().copied()
    }

    /// Mint (or return) a sourceless row at `path`: a module item registered
    /// from a route that reads it before the module's own declarations are
    /// minted, which adopt the row when they are.
    ///
    /// TRANSITION(A1 commit 4): WHY registry and module-surface routes
    /// register a module's items before its declarations are minted. WHEN one
    /// `Checker` mints every declaration before any registration reads it.
    /// WHAT: the source `declare` is the only mint.
    pub(crate) fn mint_sourceless(&mut self, path: &str, kind: DeclarationKind) -> DefId {
        if let Some(&established) = self.by_path.get(path) {
            return established;
        }
        let name = Symbol::intern(path.rsplit_once('.').map_or(path, |(_, leaf)| leaf));
        let id = self.push_row(DefRow {
            name,
            kind,
            module: None,
            owner: None,
            site: None,
            path: path.to_string(),
        });
        self.by_path.insert(path.to_string(), id);
        id
    }

    /// Mint (or return) the compiler-provided codec entry point `name` of a
    /// wire type (`decode`, `from_json`): a sourceless member of `owner`,
    /// like a materialized trait default, with no source body.
    pub(crate) fn mint_codec_member(&mut self, owner: DefId, name: Symbol) -> DefId {
        if let Some(member) = self.member_of_kind(owner, name, DeclarationKind::CodecMethod) {
            return member;
        }
        let path = format!("{}::<codec {name}>", self.path(owner));
        let id = self.push_row(DefRow {
            name,
            kind: DeclarationKind::CodecMethod,
            module: self.module(owner),
            owner: Some(owner),
            site: None,
            path: path.clone(),
        });
        self.by_path.insert(path, id);
        id
    }

    /// The member of `owner` declared as `name` with `kind`.
    #[must_use]
    pub fn member_of_kind(
        &self,
        owner: DefId,
        name: Symbol,
        kind: DeclarationKind,
    ) -> Option<DefId> {
        self.members
            .get(&(owner, name))?
            .iter()
            .copied()
            .find(|member| self.kind(*member) == kind)
    }

    fn row(&self, id: DefId) -> &DefRow {
        &self.defs[id.index()]
    }

    /// The declared spelling of a definition, for display.
    #[must_use]
    pub fn name(&self, id: DefId) -> Symbol {
        self.row(id).name
    }

    /// The canonical declaration path (`ma.T::m`). Linker symbols derive from
    /// it; nothing may parse it back into an identity.
    #[must_use]
    pub fn path(&self, id: DefId) -> &str {
        &self.row(id).path
    }

    /// The diagnostic render of a definition: its path without the module
    /// qualification (`T::m`, `Shape`).
    ///
    /// TRANSITION(P2): rendered from the stored path until A1 renders it from
    /// the owner chain.
    #[must_use]
    pub fn display(&self, id: DefId) -> &str {
        crate::short_name(self.path(id))
    }

    #[must_use]
    pub fn kind(&self, id: DefId) -> DeclarationKind {
        self.row(id).kind
    }

    /// The source module a definition occurs in; `None` when sourceless.
    #[must_use]
    pub fn module(&self, id: DefId) -> Option<ModuleId> {
        self.row(id).module
    }

    /// The definition this one is a member of.
    #[must_use]
    pub fn owner(&self, id: DefId) -> Option<DefId> {
        self.row(id).owner
    }

    /// The occurrence that established a definition; `None` when sourceless.
    #[must_use]
    pub fn site(&self, id: DefId) -> Option<DeclarationOccurrence> {
        self.row(id).site
    }

    /// The number of rows.
    #[must_use]
    pub fn len(&self) -> usize {
        self.defs.len()
    }

    /// Whether the table holds only the builtin anchors.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.defs.len() == BuiltinAnchor::ALL.len() + Predicate::ALL.len() + KnownDecl::ALL.len()
    }

    /// Whether `self` holds every row of `base` and only appended rows. A
    /// known declaration `base` still held sourceless may have been adopted
    /// by its source; it keeps its identity and path.
    #[must_use]
    pub fn extends(&self, base: &DefTable) -> bool {
        self.defs.get(..base.defs.len()).is_some_and(|prefix| {
            prefix.iter().zip(&base.defs).all(|(row, base_row)| {
                row == base_row
                    || (base_row.site.is_none()
                        && row.name == base_row.name
                        && row.kind == base_row.kind
                        && row.owner == base_row.owner
                        && row.path == base_row.path)
            })
        })
    }

    /// The table a second checker run over the embedded builtin source mints
    /// into: every row of `self` keeps its id, so the std.builtins
    /// declarations the run re-declares resolve to the same identities, while
    /// the run's own synthetic root starts with an empty namespace.
    ///
    /// TRANSITION(P2): WHY HIR type-checks the injected builtin impls with a
    /// second `Checker`; WHEN B1 deletes that run; WHAT one `Checker` per
    /// compilation (identity plan §3.2).
    #[must_use]
    pub(crate) fn fork_for_embedded(&self) -> DefTable {
        let mut fork = self.clone();
        if let Some(root) = fork.root.take() {
            let defs = &fork.defs;
            fork.by_path
                .retain(|_, id| defs[id.index()].module != Some(root));
        }
        fork
    }

    /// Every definition in mint order.
    ///
    /// # Panics
    ///
    /// Never: `push_row` refuses a table past `u32::MAX` rows.
    pub fn ids(&self) -> impl Iterator<Item = DefId> + '_ {
        (0..self.defs.len())
            .map(|index| DefId(u32::try_from(index).expect("row count fits the minted id range")))
    }

    /// The row of a builtin receiver anchor: the anchors are the first rows
    /// of every table, in [`BuiltinAnchor::ALL`] order.
    #[must_use]
    pub(crate) fn anchor(anchor: BuiltinAnchor) -> DefId {
        DefId(anchor as u32)
    }

    /// Canonicalize a source path for interning. Falls back to the path as
    /// given when the file is not resolvable on this filesystem (synthetic
    /// test paths, embedded sources): identity must be deterministic, never
    /// dependent on `canonicalize` succeeding.
    fn intern_source_key(source: &Path) -> PathBuf {
        std::fs::canonicalize(source).unwrap_or_else(|_| source.to_path_buf())
    }

    /// Mint (or resolve) the identity of a graph module with canonical dotted
    /// path `canonical_path`. Dedupe axis is the canonical source: a second
    /// spelling reaching the same source resolves to the existing identity.
    ///
    /// A render collision with a DIFFERENT source is disambiguated fail-closed
    /// (`#mod` suffix, appended until the render is genuinely unique), exactly
    /// as `mint_source_file_module` and `mint_root_module` do: returning the
    /// established identity would equate two distinct sources, and every
    /// declaration table keyed by the render would merge with it. Sourceless
    /// mints (compiler-owned surfaces such as `std.builtins`) keep the path as
    /// their only dedupe axis.
    pub(crate) fn mint_module(&mut self, canonical_path: &str, sources: &[PathBuf]) -> ModuleId {
        let source_key = sources.first().map(|s| Self::intern_source_key(s));
        let Some(key) = source_key else {
            if let Some(existing) = self.module_by_path.get(canonical_path) {
                return *existing;
            }
            return self.insert_module(canonical_path.to_string(), None);
        };
        if let Some(existing) = self.module_by_source.get(&key) {
            return *existing;
        }
        let mut render = canonical_path.to_string();
        while let Some(established) = self.module_by_path.get(&render) {
            // A path minted without a source is a placeholder for this very
            // module; adopt it rather than splitting the render.
            if self.modules[established.0 as usize]
                .canonical_source
                .is_none()
            {
                let id = *established;
                self.modules[id.0 as usize].canonical_source = Some(key.clone());
                self.module_by_source.insert(key, id);
                return id;
            }
            render.push_str("#mod");
        }
        self.insert_module(render, Some(key))
    }

    /// Mint the ROOT compilation unit's identity from its canonical source.
    ///
    /// Provenance invariance: if the root source is also reachable as a graph
    /// module (directly checking an importable file), the root REUSES that
    /// module's identity — the same declaration minted through either route
    /// carries one identity. Otherwise the render is the file stem, exactly
    /// the identity the file would carry when imported as a single-file
    /// module.
    ///
    /// String-keyed collision guard: while fn-sig registries remain
    /// string-keyed, a root stem that collides with a DIFFERENT module's
    /// dotted path would merge two key namespaces. Disambiguate with a
    /// `#root` suffix — `#` cannot appear in a dotted import path, and the
    /// root identity is never displayed (root diagnostics render bare leaves).
    ///
    /// Returns `None` when the root has no source (synthetic roots, unit-test
    /// programs without source paths). Callers must establish the explicit
    /// synthetic-root occurrence authority instead.
    pub(crate) fn mint_root_module(&mut self, sources: &[PathBuf]) -> Option<ModuleId> {
        let source = sources.first()?;
        let source_key = Self::intern_source_key(source);
        if let Some(existing) = self.module_by_source.get(&source_key) {
            self.root = Some(*existing);
            return self.root;
        }
        let stem = source.file_stem()?.to_str()?;
        if stem.is_empty() {
            return None;
        }
        let render: String = stem
            .chars()
            .map(|c| {
                if c.is_ascii_alphanumeric() || c == '_' {
                    c
                } else {
                    '_'
                }
            })
            .collect();
        let render = if self.module_by_path.contains_key(&render) {
            format!("{render}#root")
        } else {
            render
        };
        let id = self.insert_module(render, Some(source_key));
        self.root = Some(id);
        self.root
    }

    /// Establish a non-colliding occurrence authority for a source-less root.
    /// Its display/canonical-name projection remains absent: the reserved
    /// path exists only to distinguish declaration occurrences.
    pub(crate) fn mint_synthetic_root(&mut self) -> ModuleId {
        if let Some(root) = self.root {
            return root;
        }
        let id = self.insert_module(SYNTHETIC_ROOT_PATH.to_string(), None);
        self.root = Some(id);
        id
    }

    fn insert_module(
        &mut self,
        canonical_path: String,
        canonical_source: Option<PathBuf>,
    ) -> ModuleId {
        let id = ModuleId(
            u32::try_from(self.modules.len()).expect("more than u32::MAX modules in one compile"),
        );
        if let Some(key) = canonical_source.clone() {
            self.module_by_source.insert(key, id);
        }
        self.module_by_path.insert(canonical_path.clone(), id);
        self.modules.push(ModuleEntry {
            canonical_path,
            canonical_source,
        });
        id
    }

    /// Mint (or resolve) the identity of one SOURCE FILE of a directory
    /// module. A directory module assembles its primary file plus every peer
    /// `.hew` file, and one file can be reached both as a peer and as its own
    /// imported module (`pkg/aaa.hew` via `import pkg` and via
    /// `import pkg.aaa`); the dedupe axis is the canonical source, so every
    /// route mints ONE identity for the file. When the file is not already a
    /// minted module's primary source, its render is `{assembler}.{stem}` —
    /// exactly the identity the file carries when imported directly. A render
    /// collision with a DIFFERENT module's path is disambiguated fail-closed
    /// (`#file` suffix, appended until the render is genuinely unique — a
    /// THIRD colliding file must not re-derive the second's suffixed render):
    /// a false split diagnoses loudly, a false merge would equate two
    /// declarations.
    pub(crate) fn mint_source_file_module(&mut self, assembler: &str, source: &Path) -> ModuleId {
        let source_key = Self::intern_source_key(source);
        if let Some(existing) = self.module_by_source.get(&source_key) {
            return *existing;
        }
        let stem: String = source
            .file_stem()
            .and_then(|stem| stem.to_str())
            .unwrap_or("file")
            .chars()
            .map(|c| {
                if c.is_ascii_alphanumeric() || c == '_' {
                    c
                } else {
                    '_'
                }
            })
            .collect();
        let mut render = format!("{assembler}.{stem}");
        while self.module_by_path.contains_key(&render) {
            render.push_str("#file");
        }
        self.insert_module(render, Some(source_key))
    }

    /// The minted module identity of a canonical source file, when any mint
    /// recorded that source.
    #[must_use]
    pub fn module_for_source(&self, source: &Path) -> Option<ModuleId> {
        self.module_by_source
            .get(&Self::intern_source_key(source))
            .copied()
    }

    /// Resolve a graph module's canonical dotted path to its interned identity.
    #[must_use]
    pub fn module_for_path(&self, canonical_path: &str) -> Option<ModuleId> {
        self.module_by_path.get(canonical_path).copied()
    }

    /// Canonical dotted render of a source file's minted identity.
    #[must_use]
    pub fn module_path_for_source(&self, source: &Path) -> Option<&str> {
        self.module_for_source(source)
            .map(|id| self.module_path(id))
    }

    /// Every module with its canonical render, in mint order.
    ///
    /// # Panics
    ///
    /// Never: `insert_module` refuses a table past `u32::MAX` modules.
    pub fn modules(&self) -> impl Iterator<Item = (ModuleId, &str)> + '_ {
        self.modules.iter().enumerate().map(|(index, entry)| {
            (
                ModuleId(u32::try_from(index).expect("module count fits the minted id range")),
                entry.canonical_path.as_str(),
            )
        })
    }

    /// Canonical dotted render of a minted module.
    #[must_use]
    pub fn module_path(&self, id: ModuleId) -> &str {
        &self.modules[id.0 as usize].canonical_path
    }

    /// The root compilation unit's identity, when one was minted.
    #[must_use]
    pub fn root_module(&self) -> Option<ModuleId> {
        self.root
    }

    /// Canonical dotted render of the root unit, when one was minted.
    #[must_use]
    pub fn root_module_path(&self) -> Option<&str> {
        self.root
            .map(|id| self.module_path(id))
            .filter(|path| *path != SYNTHETIC_ROOT_PATH)
    }

    /// The source file a minted module identity was established from.
    #[must_use]
    pub(crate) fn module_source(&self, id: ModuleId) -> Option<&Path> {
        self.modules[id.0 as usize].canonical_source.as_deref()
    }

    /// Establish exactly one identity for a source declaration.
    ///
    /// `name` is the declared spelling, `owner` the declaration it is a
    /// member of, and `path` the canonical render symbols derive from.
    /// Repeating the same claim is idempotent (the same source may be visited
    /// through multiple import routes). Every other claim is refused: a
    /// second spelling for an established occurrence would give one
    /// declaration two renders, and two different occurrences claiming one
    /// path would equate two declarations.
    pub(crate) fn declare(
        &mut self,
        occurrence: DeclarationOccurrence,
        name: Symbol,
        owner: Option<DefId>,
        path: impl Into<String>,
    ) -> Result<DefId, DeclarationIdentityError> {
        let path = path.into();
        if let Some(&established) = self.by_occurrence.get(&occurrence) {
            if self.path(established) == path {
                return Ok(established);
            }
            return Err(DeclarationIdentityError::SecondSpelling {
                established: self.path(established).to_string(),
                conflicting: path,
            });
        }
        if let Some(&established) = self.by_path.get(&path) {
            // A known declaration's pre-minted row is adopted by its source.
            if self.row(established).site.is_none() {
                let row = &mut self.defs[established.index()];
                row.name = name;
                row.kind = occurrence.kind();
                row.module = occurrence.module();
                row.owner = owner;
                row.site = Some(occurrence);
                if let Some(owner) = owner {
                    self.members
                        .entry((owner, name))
                        .or_default()
                        .push(established);
                }
                self.by_occurrence.insert(occurrence, established);
                return Ok(established);
            }
            return Err(DeclarationIdentityError::PathAlreadyDeclared {
                path,
                established_occurrence: self
                    .site(established)
                    .expect("a path-indexed declaration has a source occurrence"),
                conflicting_occurrence: occurrence,
            });
        }
        let id = self.push_row(DefRow {
            name,
            kind: occurrence.kind(),
            module: occurrence.module(),
            owner,
            site: Some(occurrence),
            path: path.clone(),
        });
        self.by_occurrence.insert(occurrence, id);
        self.by_path.insert(path, id);
        Ok(id)
    }

    /// Record that `declaration` is the source declaration of `builtin`.
    pub(crate) fn bind_builtin_declaration(
        &mut self,
        builtin: crate::BuiltinType,
        declaration: DefId,
    ) {
        self.builtin_declarations.insert(builtin, declaration);
    }

    /// The source declaration of a compiler builtin, when its std module
    /// declares it.
    #[must_use]
    pub fn builtin_declaration(&self, builtin: crate::BuiltinType) -> Option<NominalId> {
        self.builtin_declarations
            .get(&builtin)
            .copied()
            .map(NominalId::from_minted_declaration)
    }

    /// The builtin a declaration is, when it is one.
    #[must_use]
    pub fn declared_builtin(&self, declaration: DefId) -> Option<crate::BuiltinType> {
        self.builtin_declarations
            .iter()
            .find_map(|(builtin, id)| (*id == declaration).then_some(*builtin))
    }

    /// Bind a FURTHER occurrence to the declaration already established under
    /// `canonical_path`.
    ///
    /// [`Self::declare`] refuses two occurrences claiming one path because
    /// that would equate two declarations. An `extern "C"` symbol is the one
    /// place where they ARE one: the linker binds every call to a single
    /// implementation, peer files of one directory module routinely
    /// re-declare a runtime symbol, and the checker keys every declaration of
    /// it by the same fn-sig path — so the extern table resolves the
    /// redeclaration against the established ABI contract rather than minting
    /// a second one. Binding never creates a path or a declaration: it
    /// answers `None` when nothing owns `canonical_path`, and it leaves an
    /// occurrence that already resolves untouched.
    pub(crate) fn bind_redeclaration(
        &mut self,
        occurrence: DeclarationOccurrence,
        canonical_path: &str,
    ) -> Option<DefId> {
        let established = *self.by_path.get(canonical_path)?;
        Some(*self.by_occurrence.entry(occurrence).or_insert(established))
    }

    /// Mint the body identity of a trait default materialized for one
    /// concrete receiver. The trait method identity remains the dispatch key;
    /// this row names the body emitted for the exact receiver instance.
    pub(crate) fn mint_default_impl_body(
        &mut self,
        declaring_trait: DefId,
        receiver: &NominalInstance,
        method: Symbol,
    ) -> DefId {
        let key = (declaring_trait, receiver.clone(), method);
        if let Some(&established) = self.default_bodies.get(&key) {
            return established;
        }
        let nominal = self.path(receiver.nominal.declaration());
        let rendered_receiver = if receiver.args.is_empty() {
            nominal.to_string()
        } else {
            format!(
                "{nominal}<{}>",
                receiver
                    .args
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };
        let path = format!(
            "{nominal}::<default impl {} for {rendered_receiver}>::{method}",
            self.path(declaring_trait),
        );
        let id = self.push_row(DefRow {
            name: method,
            kind: DeclarationKind::DefaultImplMethod,
            module: None,
            owner: Some(receiver.nominal.declaration()),
            site: None,
            path,
        });
        self.default_bodies.insert(key, id);
        id
    }

    /// The row of a checker-synthesized request record keyed by `method_id`:
    /// the handler's own declaration when it has one, else a sourceless row
    /// rendered as `method_id`.
    ///
    /// TRANSITION(P2): deleted by A1 commit 3, when the request type carries
    /// the handler's `DefId` instead of its path.
    pub(crate) fn request_protocol(&mut self, method_id: &str) -> DefId {
        if let Some(&established) = self.by_path.get(method_id) {
            return established;
        }
        let id = self.push_row(DefRow {
            name: Symbol::intern(method_id),
            kind: DeclarationKind::RequestProtocol,
            module: None,
            owner: None,
            site: None,
            path: method_id.to_string(),
        });
        self.by_path.insert(method_id.to_string(), id);
        id
    }

    /// The body identity the checker minted for a trait default materialized
    /// on `receiver`.
    #[must_use]
    pub fn default_impl_body(
        &self,
        declaring_trait: DefId,
        receiver: &NominalInstance,
        method: Symbol,
    ) -> Option<DefId> {
        self.default_bodies
            .get(&(declaring_trait, receiver.clone(), method))
            .copied()
    }

    /// Resolve an exact source occurrence.
    #[must_use]
    pub fn declaration(&self, occurrence: DeclarationOccurrence) -> Option<DefId> {
        self.by_occurrence.get(&occurrence).copied()
    }

    /// The nominal identity established for a source occurrence.
    #[must_use]
    pub fn nominal(&self, occurrence: DeclarationOccurrence) -> Option<NominalId> {
        self.declaration(occurrence)
            .map(NominalId::from_minted_declaration)
    }

    /// The source occurrence that established a canonical path.
    #[must_use]
    pub(crate) fn occurrence_by_path(&self, canonical_path: &str) -> Option<DeclarationOccurrence> {
        self.by_path
            .get(canonical_path)
            .and_then(|&id| self.site(id))
    }

    /// Resolve a canonical declaration path established by the checker.
    /// This is a name-resolution index into the same authority, not a mint.
    ///
    /// TRANSITION(P2): deleted by A1 commit 3.
    #[must_use]
    pub fn lookup_path(&self, canonical_path: &str) -> Option<DefId> {
        self.by_path.get(canonical_path).copied()
    }

    /// The nominal identity at an established canonical path.
    ///
    /// TRANSITION(P2): deleted by A1 commit 2.
    #[must_use]
    pub fn lookup_nominal(&self, canonical_path: &str) -> Option<NominalId> {
        self.lookup_path(canonical_path)
            .map(NominalId::from_minted_declaration)
    }

    /// The declaration kind established for a canonical path.
    ///
    /// Call resolution needs the kind, not only the identity: `Counter::helper`
    /// and `Counter::bump` are both `{actor}::{leaf}` spellings, and only the
    /// `ActorMethod` one is a callable a sibling handler may name directly.
    #[must_use]
    pub fn declaration_kind_by_path(&self, canonical_path: &str) -> Option<DeclarationKind> {
        self.lookup_path(canonical_path).map(|id| self.kind(id))
    }

    /// Whether this source module already contributed rows for its source
    /// items.
    ///
    /// Registry mirrors can re-parse source that the module graph already
    /// inventoried. They are lookup adapters, not a second source authority;
    /// callers use this predicate to avoid claiming a second set of spans for
    /// the same declarations. A registry-loaded extern declares a row with an
    /// empty span before the module's source is read; it is not a source item
    /// and does not count.
    #[must_use]
    pub(crate) fn module_has_source_declarations(&self, module: ModuleId) -> bool {
        self.defs.iter().any(|row| {
            row.module == Some(module)
                && row
                    .site
                    .is_some_and(|site| site.item_start != site.item_end)
        })
    }

    /// Every source declaration with its establishing occurrence, in mint
    /// order. Read-only: tests and tooling inventory the table with it;
    /// nothing downstream may derive a new identity from the rows.
    pub fn declarations(&self) -> impl Iterator<Item = (DeclarationOccurrence, DefId)> + '_ {
        self.ids()
            .filter_map(|id| self.site(id).map(|site| (site, id)))
    }
}

#[cfg(any(test, feature = "test"))]
thread_local! {
    /// The table hand-built test fixtures mint into; see [`DefId::for_test`].
    static FIXTURE_DEFS: std::cell::RefCell<DefTable> = std::cell::RefCell::new(DefTable::new());
}

#[cfg(any(test, feature = "test"))]
impl DefId {
    /// Mint (or return) a fixture row rendered as `path` in this thread's
    /// fixture table. A fixture module that renders its ids carries
    /// [`DefTable::fixture`].
    #[doc(hidden)]
    pub fn for_test(path: impl AsRef<str>) -> DefId {
        FIXTURE_DEFS.with(|defs| defs.borrow_mut().mint_for_test(path))
    }
}

#[cfg(any(test, feature = "test"))]
impl NominalId {
    #[doc(hidden)]
    pub fn for_test(path: impl AsRef<str>) -> NominalId {
        NominalId::from_minted_declaration(DefId::for_test(path))
    }
}

/// Fixture minting for tests that build IR by hand.
#[cfg(any(test, feature = "test"))]
impl DefTable {
    /// A snapshot of this thread's fixture table, holding every row
    /// [`DefId::for_test`] minted so far.
    #[doc(hidden)]
    #[must_use]
    pub fn fixture() -> std::sync::Arc<DefTable> {
        FIXTURE_DEFS.with(|defs| std::sync::Arc::new(defs.borrow().clone()))
    }

    /// Mint (or return) a sourceless row rendered as `path`; the path is also
    /// its display name.
    #[doc(hidden)]
    pub fn mint_for_test(&mut self, path: impl AsRef<str>) -> DefId {
        let path = path.as_ref();
        if let Some(&established) = self.by_path.get(path) {
            return established;
        }
        let id = self.push_row(DefRow {
            name: Symbol::intern(path),
            kind: DeclarationKind::Function,
            module: None,
            owner: None,
            site: None,
            path: path.to_string(),
        });
        self.by_path.insert(path.to_string(), id);
        id
    }

    #[doc(hidden)]
    pub fn mint_nominal_for_test(&mut self, path: impl AsRef<str>) -> NominalId {
        NominalId::from_minted_declaration(self.mint_for_test(path))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn builtin_anchors_are_the_leading_rows() {
        let table = DefTable::new();
        for anchor in BuiltinAnchor::ALL {
            let id = DefTable::anchor(anchor);
            assert_eq!(table.path(id), anchor.spelling());
            assert_eq!(table.kind(id), DeclarationKind::BuiltinType);
            assert_eq!(
                table.lookup_path(anchor.spelling()),
                None,
                "anchors claim no path"
            );
        }
    }

    /// The embedded builtin check mints into a fork: rows the compilation
    /// established keep their ids, the fork only appends, and the
    /// compilation's root namespace is not visible to the fork's own root.
    #[test]
    fn embedded_fork_keeps_ids_and_frees_the_root_namespace() {
        let mut table = DefTable::new();
        let builtins = table.mint_module("std.builtins", &[]);
        let root = table
            .mint_root_module(&[PathBuf::from("/nonexistent/app.hew")])
            .expect("source-backed root");
        let shared = table
            .declare(
                DeclarationOccurrence::new(Some(builtins), &(0..8), DeclarationKind::Type, 0),
                Symbol::intern("Cursor"),
                None,
                "std.builtins.Cursor",
            )
            .unwrap();
        table
            .declare(
                DeclarationOccurrence::new(Some(root), &(0..8), DeclarationKind::Type, 0),
                Symbol::intern("Cursor"),
                None,
                "Cursor",
            )
            .unwrap();
        let mut fork = table.fork_for_embedded();
        assert_eq!(fork.lookup_path("std.builtins.Cursor"), Some(shared));
        assert_eq!(fork.lookup_path("Cursor"), None);
        let fork_root = fork.mint_synthetic_root();
        assert_ne!(fork_root, root);
        fork.declare(
            DeclarationOccurrence::new(Some(fork_root), &(0..8), DeclarationKind::Type, 0),
            Symbol::intern("Cursor"),
            None,
            "Cursor",
        )
        .expect("the fork's root namespace starts empty");
        assert!(fork.extends(&table));
    }

    #[test]
    fn same_source_root_and_module_intern_one_identity() {
        let mut table = DefTable::new();
        let source = PathBuf::from("/nonexistent/oracle_mod.hew");
        let module = table.mint_module("oracle_mod", std::slice::from_ref(&source));
        let root = table
            .mint_root_module(std::slice::from_ref(&source))
            .expect("root with a source mints an identity");
        assert_eq!(module, root, "one source = one identity, however reached");
        assert_eq!(table.module_path(root), "oracle_mod");
    }

    #[test]
    fn peer_file_mints_one_identity_across_assembly_routes() {
        let mut table = DefTable::new();
        let pkg_hew = PathBuf::from("/nonexistent/pkg/pkg.hew");
        let aaa_hew = PathBuf::from("/nonexistent/pkg/aaa.hew");
        table.mint_module("pkg", &[pkg_hew.clone(), aaa_hew.clone()]);
        let as_peer = table.mint_source_file_module("pkg", &aaa_hew);
        let as_module = table.mint_module("pkg.aaa", std::slice::from_ref(&aaa_hew));
        assert_eq!(as_peer, as_module, "one file = one identity, either route");
        assert_eq!(table.module_path(as_peer), "pkg.aaa");
        assert_eq!(table.module_path_for_source(&pkg_hew), Some("pkg"));
        assert_eq!(table.module_path_for_source(&aaa_hew), Some("pkg.aaa"));
    }

    #[test]
    fn source_file_render_colliding_with_distinct_module_is_disambiguated() {
        let mut table = DefTable::new();
        table.mint_module("pkg.aaa", &[PathBuf::from("/elsewhere/aaa.hew")]);
        let peer = table.mint_source_file_module("pkg", &PathBuf::from("/nonexistent/pkg/aaa.hew"));
        assert_eq!(
            table.module_path(peer),
            "pkg.aaa#file",
            "distinct sources must never merge under one render"
        );
    }

    /// Three distinct files whose stems sanitize to ONE render must mint
    /// three DISTINCT identities. A single-application disambiguator
    /// re-derives the same suffixed string for the third file and silently
    /// overwrites the second's `by_path` slot — a false merge on the very
    /// axis this table exists to make injective.
    #[test]
    fn repeated_render_collisions_mint_distinct_identities() {
        let mut table = DefTable::new();
        let a = table.mint_source_file_module("pkg", &PathBuf::from("/nonexistent/pkg/a-b.hew"));
        let b = table.mint_source_file_module("pkg", &PathBuf::from("/nonexistent/pkg/a+b.hew"));
        let c = table.mint_source_file_module("pkg", &PathBuf::from("/nonexistent/pkg/a.b.hew"));
        assert_ne!(a, b);
        assert_ne!(b, c);
        assert_ne!(a, c);
        let renders = [
            table.module_path(a).to_string(),
            table.module_path(b).to_string(),
            table.module_path(c).to_string(),
        ];
        let unique: std::collections::HashSet<&String> = renders.iter().collect();
        assert_eq!(
            unique.len(),
            3,
            "every colliding file must keep its own render, got {renders:?}"
        );
        for (id, render) in [(a, &renders[0]), (b, &renders[1]), (c, &renders[2])] {
            assert_eq!(
                table.module_path(id),
                render,
                "render lookup must stay stable after later mints"
            );
        }
    }

    /// Two distinct sources reaching `mint_module` under one canonical path
    /// must mint two identities. Returning the established one would equate
    /// the declarations of two files, which is the merge this table exists to
    /// prevent.
    #[test]
    fn module_render_colliding_with_distinct_source_is_disambiguated() {
        let mut table = DefTable::new();
        let first = table.mint_module("lib", &[PathBuf::from("/one/lib.hew")]);
        let second = table.mint_module("lib", &[PathBuf::from("/two/lib.hew")]);
        assert_ne!(first, second, "distinct sources must never merge");
        assert_eq!(table.module_path(first), "lib");
        assert_eq!(table.module_path(second), "lib#mod");
        assert_eq!(
            table.module_path_for_source(Path::new("/one/lib.hew")),
            Some("lib")
        );
        assert_eq!(
            table.module_path_for_source(Path::new("/two/lib.hew")),
            Some("lib#mod")
        );
    }

    /// A sourceless mint (`std.builtins`) is a placeholder for the module, not
    /// a competing identity: the first source to claim that path adopts it.
    #[test]
    fn sourceless_module_render_is_adopted_by_its_first_source() {
        let mut table = DefTable::new();
        let placeholder = table.mint_module("std.builtins", &[]);
        let sourced = table.mint_module("std.builtins", &[PathBuf::from("/std/builtins.hew")]);
        assert_eq!(placeholder, sourced, "one module, one identity");
        assert_eq!(table.module_path(sourced), "std.builtins");
        assert_eq!(
            table.module_path_for_source(Path::new("/std/builtins.hew")),
            Some("std.builtins")
        );
    }

    #[test]
    fn root_stem_colliding_with_distinct_module_path_is_disambiguated() {
        let mut table = DefTable::new();
        table.mint_module("util", &[PathBuf::from("/imported/util.hew")]);
        let root = table
            .mint_root_module(&[PathBuf::from("/elsewhere/util.hew")])
            .expect("root mints");
        assert_eq!(
            table.module_path(root),
            "util#root",
            "distinct sources must not merge string-keyed namespaces"
        );
    }

    #[test]
    fn sourceless_root_has_occurrence_identity_but_keeps_bare_namespace() {
        let mut table = DefTable::new();
        assert_eq!(table.mint_root_module(&[]), None);
        assert_eq!(table.mint_synthetic_root(), table.root_module().unwrap());
        assert_eq!(table.root_module_path(), None);
    }

    #[test]
    fn declaration_routes_converge_on_source_occurrence() {
        let mut table = DefTable::new();
        let source = PathBuf::from("/nonexistent/pkg/worker.hew");
        let imported = table.mint_module("pkg.worker", std::slice::from_ref(&source));
        let rooted = table
            .mint_root_module(std::slice::from_ref(&source))
            .expect("source-backed root");
        assert_eq!(imported, rooted);
        let occurrence =
            DeclarationOccurrence::new(Some(imported), &(10..40), DeclarationKind::Function, 0);
        let first = table
            .declare(occurrence, Symbol::intern("run"), None, "pkg.worker.run")
            .expect("first claim");
        let second = table
            .declare(occurrence, Symbol::intern("run"), None, "pkg.worker.run")
            .expect("same route-independent claim");
        assert_eq!(first, second);
        assert_eq!(table.declaration(occurrence), Some(first));
    }

    #[test]
    fn same_leaf_in_different_modules_has_distinct_identity() {
        let mut table = DefTable::new();
        let left = table.mint_module("left", &[PathBuf::from("/nonexistent/left.hew")]);
        let right = table.mint_module("right", &[PathBuf::from("/nonexistent/right.hew")]);
        let left_occurrence =
            DeclarationOccurrence::new(Some(left), &(0..8), DeclarationKind::Type, 0);
        let right_occurrence =
            DeclarationOccurrence::new(Some(right), &(0..8), DeclarationKind::Type, 0);
        let left_id = table
            .declare(left_occurrence, Symbol::intern("Item"), None, "left.Item")
            .unwrap();
        let right_id = table
            .declare(right_occurrence, Symbol::intern("Item"), None, "right.Item")
            .unwrap();
        assert_ne!(left_id, right_id);
    }

    #[test]
    fn a_second_path_for_one_occurrence_is_refused_naming_both_spellings() {
        let mut table = DefTable::new();
        let module = table.mint_module("m", &[PathBuf::from("/nonexistent/m.hew")]);
        let occurrence =
            DeclarationOccurrence::new(Some(module), &(0..8), DeclarationKind::Function, 0);
        let established = table
            .declare(occurrence, Symbol::intern("run"), None, "m.run")
            .unwrap();
        // Repeating the established claim is idempotent.
        assert_eq!(
            table
                .declare(occurrence, Symbol::intern("run"), None, "m.run")
                .unwrap(),
            established
        );
        let error = table
            .declare(occurrence, Symbol::intern("run"), None, "pkg.m.run")
            .expect_err("one declaration has one render");
        let message = error.to_string();
        assert!(
            message.contains("`m.run`") && message.contains("`pkg.m.run`"),
            "{message}"
        );
        assert_eq!(table.lookup_path("m.run"), Some(established));
        assert_eq!(table.lookup_path("pkg.m.run"), None);
    }

    #[test]
    fn two_declarations_claiming_one_path_fail_closed() {
        let mut table = DefTable::new();
        let module = table.mint_module("m", &[PathBuf::from("/nonexistent/m.hew")]);
        let first = DeclarationOccurrence::new(Some(module), &(0..8), DeclarationKind::Function, 0);
        let second =
            DeclarationOccurrence::new(Some(module), &(9..17), DeclarationKind::Function, 0);
        table
            .declare(first, Symbol::intern("run"), None, "m.run")
            .unwrap();
        assert!(matches!(
            table.declare(second, Symbol::intern("run"), None, "m.run"),
            Err(DeclarationIdentityError::PathAlreadyDeclared { .. })
        ));
        assert!(table.declaration(second).is_none());
        // A second occurrence must not be able to steal a spelling either.
        let mut table = DefTable::new();
        let module = table.mint_module("m", &[PathBuf::from("/nonexistent/m.hew")]);
        let first = DeclarationOccurrence::new(Some(module), &(0..8), DeclarationKind::Function, 0);
        let second =
            DeclarationOccurrence::new(Some(module), &(9..17), DeclarationKind::Function, 0);
        let first_id = table
            .declare(first, Symbol::intern("run"), None, "m.run")
            .unwrap();
        table
            .declare(second, Symbol::intern("stop"), None, "m.stop")
            .unwrap();
        assert!(table
            .declare(second, Symbol::intern("run"), None, "m.run")
            .is_err());
        assert_eq!(table.lookup_path("m.run"), Some(first_id));
        assert_eq!(table.declaration(first), Some(first_id));
    }
}
