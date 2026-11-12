//! Source attribution SIR projects from HIR for native debug metadata.
//!
//! SIR is the last stage holding the HIR module, so it owns the projection of
//! HIR's lexical scope tree and per-site spans. Bindings already carry their
//! name and span on [`crate::Binding`]; nothing here duplicates them.
//!
//! Every fact names the file it indexes by its file span index
//! (`hew_parser::module::FileSpanIndices`): 0 is the root file, and imported
//! modules, `std` and the bodies monomorphized from them use their own.

use std::collections::BTreeMap;

use hew_hir::{HirAnalysis, ItemId, SiteId};

/// The file span index of the root compilation unit.
pub const ROOT_FILE: u32 = 0;

/// One lexical block.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SemDebugScope {
    pub id: u32,
    pub parent: Option<u32>,
    /// The file `start..end` indexes.
    pub file: u32,
    /// Half-open source byte extent of the block.
    pub start: u32,
    pub end: u32,
}

/// One source point: a file and a byte offset into it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SemSourceSite {
    pub file: u32,
    pub offset: u32,
}

/// Source attribution for debug metadata and fault sites.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct SemDebugFacts {
    /// Full site attribution, including imported files, for coverage projection.
    pub coverage_sites: BTreeMap<SiteId, hew_hir::HirSiteSource>,
    /// Lexical blocks in HIR scope order.
    pub scopes: Vec<SemDebugScope>,
    /// Where each operation site begins.
    pub sites: BTreeMap<SiteId, SemSourceSite>,
    /// The file each item with a body was lowered from.
    pub item_files: BTreeMap<ItemId, u32>,
    /// The `Self` type of each impl method body.
    pub item_receivers: BTreeMap<ItemId, hew_types::ResolvedTy>,
}

impl SemDebugFacts {
    /// Project the scope tree, site offsets and item files out of HIR.
    #[must_use]
    pub fn project(analysis: HirAnalysis<'_>) -> Self {
        let hew_hir::HirSourceFacts {
            sites: sources,
            scopes: extents,
            item_files,
            item_receivers,
        } = analysis.into_source_facts();
        let mut scopes: Vec<SemDebugScope> = extents
            .iter()
            .filter_map(|(id, extent)| {
                let file = extent.file?;
                Some(SemDebugScope {
                    id: id.0,
                    // A parent in another file cannot scope this block.
                    parent: extent
                        .parent
                        .filter(|parent| {
                            extents
                                .get(parent)
                                .is_some_and(|parent| parent.file == Some(file))
                        })
                        .map(|parent| parent.0),
                    file,
                    start: clamp_offset(extent.span.start),
                    end: clamp_offset(extent.span.end),
                })
            })
            .collect();
        scopes.sort_unstable_by_key(|scope| scope.id);
        let coverage_sites: BTreeMap<_, _> = sources.into_iter().collect();
        let sites = coverage_sites
            .iter()
            .filter_map(|(id, source)| {
                Some((
                    *id,
                    SemSourceSite {
                        file: source.file?,
                        offset: clamp_offset(source.span.start),
                    },
                ))
            })
            .collect();
        Self {
            coverage_sites,
            scopes,
            sites,
            item_files: item_files.into_iter().collect(),
            item_receivers: item_receivers.into_iter().collect(),
        }
    }

    /// The source point an operation's provenance names, when it has one.
    #[must_use]
    pub fn site(&self, provenance: &crate::Provenance) -> Option<SemSourceSite> {
        match provenance {
            crate::Provenance::Site(site) => self.sites.get(site).copied(),
            crate::Provenance::Derived(sites) => {
                sites.iter().find_map(|site| self.sites.get(site).copied())
            }
            crate::Provenance::Synthesized => None,
        }
    }

    /// The byte an operation's provenance names in the root file, for
    /// consumers that render against the root source alone.
    #[must_use]
    pub fn site_offset(&self, provenance: &crate::Provenance) -> Option<u32> {
        self.site(provenance)
            .filter(|site| site.file == ROOT_FILE)
            .map(|site| site.offset)
    }

    /// The impl receiver a method body belongs to.
    #[must_use]
    pub fn item_receiver(&self, item: ItemId) -> Option<&hew_types::ResolvedTy> {
        self.item_receivers.get(&item)
    }

    /// The file a function body indexes.
    #[must_use]
    pub fn item_file(&self, item: ItemId) -> Option<u32> {
        self.item_files.get(&item).copied()
    }
}

fn clamp_offset(offset: usize) -> u32 {
    u32::try_from(offset).unwrap_or(u32::MAX)
}
