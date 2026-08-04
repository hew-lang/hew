//! Single-owner extern contract table (rc1-F1 stage B).
//!
//! One C symbol resolves under exactly ONE contract per compile. The table is
//! the checker's single authority for extern identity: every `extern "C"`
//! declaration either MINTS its symbol's contract (first declaration wins) or
//! must resolve to the established contract; a declaration that disagrees is
//! a conflict at the declaration site, program-wide — visibility does not
//! partition ABI identity, the linker sees one symbol.
//!
//! The table also carries the extern-declaration index that gates `unsafe`:
//! a call requires `unsafe` exactly when its resolved declaration key is an
//! extern declaration recorded here. This replaces the former
//! `unsafe_functions: HashSet<String>` side registry, whose contents could
//! only drift from extern registration.
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
/// agreeing declarations of the same symbol adopt this contract; their keys
/// resolve to it through [`ExternTable::contract_for_declaration`].
#[derive(Debug, Clone)]
pub struct ExternContract {
    pub owner: DefId,
    /// Monomorphic C symbol. Empty for template declarations
    /// (`#[extern_symbol("…{T}…")]`), which have no call-independent
    /// expansion and therefore no symbol-keyed contract slot.
    pub symbol: String,
    /// Registered parameter types, exactly as stored on the declaring
    /// `FnSig`. Contract comparison canonicalizes both sides at the compare
    /// site (the checker owns spelling→identity resolution).
    pub params: Vec<Ty>,
    pub return_type: Ty,
    pub consuming_params: Vec<bool>,
    pub is_variadic: bool,
    /// Declaring span, for conflict diagnostics and same-declaration-site
    /// recognition (one source file assembled into two modules registers the
    /// same declaration twice — same span, shared source).
    pub span: Span,
    pub declaring_module: Option<String>,
    /// Modules whose (agreeing) declarations adopted this contract after it
    /// was minted — the peer-assembled second owner, or an independent
    /// re-declaration. Together with `declaring_module` these are the modules
    /// entitled to resolve handle methods against the contract's endpoint.
    pub adopting_modules: std::collections::BTreeSet<String>,
}

/// Per-compile single-owner extern authority. One instance per
/// `check_program` run; published in `TypeCheckOutput`.
#[derive(Debug, Clone, Default)]
pub struct ExternTable {
    contracts: Vec<ExternContract>,
    by_symbol: HashMap<String, ExternContractId>,
    by_declaration: HashMap<DefId, ExternContractId>,
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
    /// Panics when more than `u32::MAX` contracts are minted in one compile.
    pub fn mint(&mut self, contract: ExternContract) -> ExternContractId {
        let id = ExternContractId(
            u32::try_from(self.contracts.len()).expect("more than u32::MAX extern contracts"),
        );
        if !contract.symbol.is_empty() {
            let previous = self.by_symbol.insert(contract.symbol.clone(), id);
            debug_assert!(
                previous.is_none(),
                "extern contract for `{}` minted twice",
                contract.symbol
            );
        }
        self.by_declaration.insert(contract.owner.clone(), id);
        self.contracts.push(contract);
        id
    }

    /// Record a further declaration that resolved to an established
    /// contract (same declaration site reached through a second module
    /// assembly, or an independently-agreeing re-declaration). The
    /// declaration key becomes a second name of the ONE contract.
    pub fn adopt_declaration(
        &mut self,
        declaration: DefId,
        declaring_module: Option<String>,
        id: ExternContractId,
    ) {
        self.by_declaration.insert(declaration, id);
        if let Some(module) = declaring_module {
            self.contracts[id.0 as usize]
                .adopting_modules
                .insert(module);
        }
    }

    /// The established contract for `symbol` when `module` is one of its
    /// declaring modules (minter or agreeing adopter) — the module-authority
    /// probe behind opaque-handle method resolution.
    #[must_use]
    pub fn contract_declared_by_module(
        &self,
        symbol: &str,
        module: &str,
    ) -> Option<&ExternContract> {
        let (_, contract) = self.established(symbol)?;
        (contract.declaring_module.as_deref() == Some(module)
            || contract.adopting_modules.contains(module))
        .then_some(contract)
    }

    /// Record an extern declaration that gates `unsafe` but carries no
    /// comparable ABI contract (see module docs).
    pub fn register_declaration_only(&mut self, declaration: DefId) {
        self.declaration_only.insert(declaration);
    }

    /// Whether `declaration` names a registered extern declaration — the
    /// `unsafe`-gating authority.
    #[must_use]
    pub fn requires_unsafe(&self, declaration: &str) -> bool {
        self.by_declaration.contains_key(declaration) || self.declaration_only.contains(declaration)
    }

    /// The contract a declaration key resolves to, if it is a contract-
    /// bearing extern declaration.
    #[must_use]
    pub fn contract_for_declaration(&self, declaration: &str) -> Option<&ExternContract> {
        let id = *self.by_declaration.get(declaration)?;
        Some(&self.contracts[id.0 as usize])
    }
}
