//! Single-owner extern contract table (rc1-F1 stage B).
//!
//! One C symbol resolves under exactly ONE contract per compile. The table is
//! the checker's single authority for extern symbol identity: every
//! `extern "C"` declaration either MINTS its symbol's contract (first
//! declaration wins) or must resolve to the established contract through the
//! canonicalized structural signature compare — the compare ALWAYS runs, for
//! every further declaration of an established symbol; there is no
//! same-declaration-site shortcut, because a byte-offset span carries no
//! file identity and peer files of one directory module can align spans
//! exactly. A declaration that disagrees is a conflict at the declaration
//! site, program-wide — visibility does not partition ABI identity, the
//! linker sees one symbol.
//!
//! Provenance stays PER DECLARATION. The contract owns the symbol's ABI;
//! each declaration record keeps its own declaring module and endpoint, and
//! call-site facts (call authority, `trusted_compiled_stdlib`, lifecycle
//! authorization) derive from the declaration used at the call site — never
//! from whichever declaration happened to mint the symbol. A user
//! re-declaration of a stdlib symbol therefore adopts the ABI contract
//! without inheriting stdlib provenance, and registration order cannot
//! change a call's authority.
//!
//! The table also carries the extern-declaration index that gates `unsafe`:
//! a call requires `unsafe` exactly when its resolved declaration key names
//! an extern declaration recorded here. Every declaration registers —
//! including one whose signature CONFLICTS with the established contract
//! (registration first, diagnosis second): the conflict is a hard error, but
//! the declaration must not fall out of the unsafe/extern indexes while the
//! rest of the program is checked.
//!
//! Registered but contract-less declarations (`declaration_only`) cover the
//! two extern surfaces with no comparable ABI contract:
//! * registry-backed stdlib imports — their signature metadata is a legacy
//!   presentation surface (short-owner spellings), not a source contract;
//! * codegen-intercepted layout-witness builtins (`hew_channel_*_layout`) —
//!   their declared signatures are arity placeholders for the stdlib impl
//!   bodies, the real out-parameter ABI is emitted by codegen.
//!
//! WHEN OBSOLETE (`declaration_only`): when registry imports resolve through
//! compiled source surfaces carrying real contracts, and the channel witness
//! ABI is expressible in an extern block, fold both into minted contracts.

use std::collections::{HashMap, HashSet};

use hew_parser::ast::Span;

use crate::ty::Ty;
use crate::DefId;

/// Index of a contract in its owning [`ExternTable`]; meaningless across
/// tables.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExternContractId(u32);

/// The single ABI contract of one C symbol.
///
/// `owner` is the fn-sig declaration identity of the declaration that MINTED
/// the contract (the first declaration in registration order). Later
/// agreeing declarations of the same symbol adopt this contract; their own
/// provenance stays on their [`ExternDeclaration`] records.
#[derive(Debug, Clone)]
pub struct ExternContract {
    pub owner: DefId,
    /// Monomorphic C symbol.
    pub symbol: String,
    /// Registered parameter types, exactly as stored on the minting
    /// declaration's `FnSig`. Contract comparison canonicalizes both sides
    /// at the compare site (the checker owns spelling→identity resolution).
    pub params: Vec<Ty>,
    pub return_type: Ty,
    /// Authoritative ABI ownership disposition (who frees each argument).
    /// Adoption requires EXACT equality of consuming modes, so every
    /// declaration that resolves to this contract stores identical modes —
    /// the per-declaration `fn_param_ownership` view cannot diverge from
    /// this field by construction.
    pub consuming_params: Vec<bool>,
    pub is_variadic: bool,
    /// Minting declaration's span, for conflict diagnostics.
    pub span: Span,
    pub declaring_module: Option<String>,
}

/// One extern declaration's own record — the provenance unit.
#[derive(Debug, Clone)]
pub struct ExternDeclaration {
    /// The established contract this declaration resolved to. `None` for a
    /// template declaration (no call-independent symbol) and for a
    /// declaration whose signature conflicted with the established contract
    /// (registered for `unsafe`/call-target indexing; the conflict is a
    /// separate hard error).
    pub contract: Option<ExternContractId>,
    /// This declaration's own endpoint. Empty for template declarations.
    pub symbol: String,
    /// This declaration's own declaring module — the authority for
    /// call-site provenance (`trusted_compiled_stdlib`, lifecycle joins).
    pub declaring_module: Option<String>,
}

/// Per-compile single-owner extern authority. One instance per
/// `check_program` run; published in `TypeCheckOutput`.
#[derive(Debug, Clone, Default)]
pub struct ExternTable {
    contracts: Vec<ExternContract>,
    by_symbol: HashMap<String, ExternContractId>,
    /// Every source extern declaration, keyed by its fn-sig declaration
    /// identity.
    declarations: HashMap<DefId, ExternDeclaration>,
    /// First declaration of `(symbol, declaring module)` — the
    /// opaque-handle method resolution probe (mirrors the legacy
    /// first-match scan over source declarations).
    by_symbol_and_module: HashMap<(String, Option<String>), DefId>,
    /// Extern declarations that gate `unsafe` but carry no comparable ABI
    /// contract (see module docs).
    declaration_only: HashSet<DefId>,
}

impl ExternTable {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// The established contract for a C symbol, if one was minted.
    #[must_use]
    pub fn established(&self, symbol: &str) -> Option<(ExternContractId, &ExternContract)> {
        let id = *self.by_symbol.get(symbol)?;
        Some((id, &self.contracts[id.0 as usize]))
    }

    /// Mint the contract for a declaration. First declaration of a symbol
    /// wins; minting a symbol that already has a contract is a caller bug
    /// (resolve through [`Self::established`] first).
    ///
    /// # Panics
    ///
    /// Panics when the symbol already has a contract (a silent second mint
    /// would orphan the first contract), when the symbol is empty (template
    /// declarations have no symbol-owned contract — register them with
    /// [`Self::register_detached_declaration`]), or when more than
    /// `u32::MAX` contracts are minted in one compile.
    pub(crate) fn mint(&mut self, contract: ExternContract) -> ExternContractId {
        assert!(
            !contract.symbol.is_empty(),
            "a symbol-less declaration cannot own a symbol contract"
        );
        let id = ExternContractId(
            u32::try_from(self.contracts.len()).expect("more than u32::MAX extern contracts"),
        );
        let previous = self.by_symbol.insert(contract.symbol.clone(), id);
        assert!(
            previous.is_none(),
            "extern contract for `{}` minted twice",
            contract.symbol
        );
        self.record_declaration(
            contract.owner.clone(),
            ExternDeclaration {
                contract: Some(id),
                symbol: contract.symbol.clone(),
                declaring_module: contract.declaring_module.clone(),
            },
        );
        self.contracts.push(contract);
        id
    }

    /// Record a further declaration that resolved (by signature agreement)
    /// to an established contract. The declaration keeps its OWN provenance;
    /// only the ABI contract is shared.
    pub(crate) fn adopt_declaration(
        &mut self,
        declaration: DefId,
        symbol: String,
        declaring_module: Option<String>,
        id: ExternContractId,
    ) {
        self.record_declaration(
            declaration,
            ExternDeclaration {
                contract: Some(id),
                symbol,
                declaring_module,
            },
        );
    }

    /// Record a source extern declaration that carries no contract slot: a
    /// template declaration (empty `symbol`), or a declaration whose
    /// signature CONFLICTED with the established contract. Registration is
    /// unconditional so the declaration keeps its `unsafe` gate and its
    /// call-target endpoint while the conflict error propagates.
    pub(crate) fn register_detached_declaration(
        &mut self,
        declaration: DefId,
        symbol: String,
        declaring_module: Option<String>,
    ) {
        self.record_declaration(
            declaration,
            ExternDeclaration {
                contract: None,
                symbol,
                declaring_module,
            },
        );
    }

    fn record_declaration(&mut self, declaration: DefId, record: ExternDeclaration) {
        if !record.symbol.is_empty() {
            self.by_symbol_and_module
                .entry((record.symbol.clone(), record.declaring_module.clone()))
                .or_insert_with(|| declaration.clone());
        }
        self.declarations.insert(declaration, record);
    }

    /// Record an extern declaration that gates `unsafe` but carries no
    /// comparable ABI contract (see module docs).
    pub(crate) fn register_declaration_only(&mut self, declaration: DefId) {
        self.declaration_only.insert(declaration);
    }

    /// Whether `declaration` names a registered extern declaration — the
    /// `unsafe`-gating authority.
    #[must_use]
    pub fn requires_unsafe(&self, declaration: &str) -> bool {
        self.declarations.contains_key(declaration) || self.declaration_only.contains(declaration)
    }

    /// The declaration record for a fn-sig declaration key — the call-site
    /// provenance authority.
    #[must_use]
    pub fn declaration(&self, declaration: &str) -> Option<&ExternDeclaration> {
        self.declarations.get(declaration)
    }

    /// The contract a declaration key resolves to, when it is a
    /// contract-bearing extern declaration.
    #[must_use]
    pub fn contract_for_declaration(&self, declaration: &str) -> Option<&ExternContract> {
        let id = self.declarations.get(declaration)?.contract?;
        Some(&self.contracts[id.0 as usize])
    }

    /// The first declaration of `symbol` declared by exactly `module`, with
    /// its record — the module-authority probe behind opaque-handle method
    /// resolution.
    #[must_use]
    pub fn declaration_by_symbol_and_module(
        &self,
        symbol: &str,
        module: &str,
    ) -> Option<(&DefId, &ExternDeclaration)> {
        let key = self
            .by_symbol_and_module
            .get(&(symbol.to_string(), Some(module.to_string())))?;
        Some((key, &self.declarations[key]))
    }
}
