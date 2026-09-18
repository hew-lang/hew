//! Source attribution SIR projects from HIR for native debug metadata.
//!
//! SIR is the last stage holding the HIR module, so it owns the projection of
//! HIR's lexical scope tree and per-site spans. Bindings already carry their
//! name and span on [`crate::Binding`]; nothing here duplicates them.

use std::collections::BTreeMap;

use hew_hir::{HirModule, SiteId};

/// One lexical block of the root compilation unit.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SemDebugScope {
    pub id: u32,
    pub parent: Option<u32>,
    /// Half-open source byte extent of the block.
    pub start: u32,
    pub end: u32,
}

/// Root-unit source attribution for debug metadata.
///
/// Foreign-module sites and scopes are excluded: the debug compile unit is the
/// root source file, and a span from another file's coordinate space would
/// attribute a line that file never had.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct SemDebugFacts {
    /// Lexical blocks in HIR scope order.
    pub scopes: Vec<SemDebugScope>,
    /// Source byte offset where each operation site begins.
    pub sites: BTreeMap<SiteId, u32>,
}

impl SemDebugFacts {
    /// Project the root unit's scope tree and site offsets out of HIR.
    #[must_use]
    pub fn project(module: &HirModule) -> Self {
        let extents = hew_hir::collect_scope_extents(module);
        let mut scopes: Vec<SemDebugScope> = extents
            .iter()
            .filter(|(_, extent)| extent.source_module.is_none())
            .map(|(id, extent)| SemDebugScope {
                id: id.0,
                // A parent in another file cannot scope a root-unit block.
                parent: extent
                    .parent
                    .filter(|parent| {
                        extents
                            .get(parent)
                            .is_some_and(|parent| parent.source_module.is_none())
                    })
                    .map(|parent| parent.0),
                start: clamp_offset(extent.span.start),
                end: clamp_offset(extent.span.end),
            })
            .collect();
        scopes.sort_unstable_by_key(|scope| scope.id);
        let sites = hew_hir::collect_site_spans(module)
            .into_iter()
            .filter(|(_, source)| source.source_module.is_none())
            .map(|(id, source)| (id, clamp_offset(source.span.start)))
            .collect();
        Self { scopes, sites }
    }

    /// The source byte an operation's provenance names, when it has one in the
    /// root unit.
    #[must_use]
    pub fn site_offset(&self, provenance: &crate::Provenance) -> Option<u32> {
        match provenance {
            crate::Provenance::Site(site) => self.sites.get(site).copied(),
            crate::Provenance::Derived(sites) => {
                sites.iter().find_map(|site| self.sites.get(site).copied())
            }
            crate::Provenance::Synthesized => None,
        }
    }
}

fn clamp_offset(offset: usize) -> u32 {
    u32::try_from(offset).unwrap_or(u32::MAX)
}
