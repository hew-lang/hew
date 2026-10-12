//! Counter identity and source attribution shared by native and VM emission.

use std::collections::{BTreeMap, BTreeSet};

use crate::{BlockId, CallableId, Provenance, SemModule};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CoverageCounter {
    pub id: u32,
    pub sites: Vec<(Option<String>, usize)>,
}

pub type CoveragePlan = BTreeMap<(CallableId, BlockId), CoverageCounter>;

impl SemModule {
    /// Assign each source-bearing block one counter after semantic CFG changes.
    ///
    /// # Panics
    /// Panics if a module contains more than `u32::MAX` coverage counters.
    pub fn instrument_coverage(&mut self) {
        let mut counters = BTreeMap::new();
        for function in &self.functions {
            for block in &function.blocks {
                let mut sites = BTreeSet::new();
                let mut add = |provenance: &Provenance| {
                    let ids: &[hew_hir::SiteId] = match provenance {
                        Provenance::Site(site) => std::slice::from_ref(site),
                        Provenance::Derived(sites) => sites,
                        Provenance::Synthesized => &[],
                    };
                    for site in ids {
                        if let Some(source) = self.debug.coverage_sites.get(site) {
                            sites.insert((source.source_module.clone(), source.span.start));
                        }
                    }
                };
                for op in &block.ops {
                    add(&op.provenance);
                }
                add(&block.terminator_provenance);
                if !sites.is_empty() {
                    let id = u32::try_from(counters.len()).expect("coverage counter capacity");
                    counters.insert(
                        (function.callable, block.id),
                        CoverageCounter {
                            id,
                            sites: sites.into_iter().collect(),
                        },
                    );
                }
            }
        }
        self.coverage = counters;
    }
}
