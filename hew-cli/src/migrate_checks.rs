use std::cmp::Reverse;
use std::collections::VecDeque;
use std::path::{Path, PathBuf};
use std::time::Instant;

use hew_compile::{DocumentFrontendState, FrontendOptions, SourceSet};
use hew_parser::ast::Item;
use hew_parser::module::FileSpanIndices;

pub(crate) struct CheckUnit {
    pub root: PathBuf,
    pub members: Vec<usize>,
}

pub(crate) struct CheckUnits {
    pass: &'static str,
    pending: VecDeque<CheckUnit>,
    total: usize,
    roots: usize,
    checks: usize,
    started: Instant,
}

impl CheckUnits {
    pub fn new(files: &[(PathBuf, String)], pass: &'static str) -> Self {
        let mut units: Vec<CheckUnit> = Vec::new();
        for (index, (path, _)) in files.iter().enumerate() {
            let root = hew_compile::module_membership(path, &FrontendOptions::default())
                .filter(hew_types::module_registry::ModuleMembership::checks_as_directory_module)
                .map_or_else(|| path.clone(), |membership| membership.entry);
            match units.iter_mut().find(|unit| unit.root == root) {
                Some(unit) => unit.members.push(index),
                None => units.push(CheckUnit {
                    root,
                    members: vec![index],
                }),
            }
        }
        units.sort_by_cached_key(|unit| {
            let imports: usize = unit
                .members
                .iter()
                .map(|&index| {
                    hew_parser::parse(&files[index].1)
                        .program
                        .items
                        .iter()
                        .filter(|(item, _)| matches!(item, Item::Import(_)))
                        .count()
                })
                .sum();
            (Reverse(imports), unit.root.clone())
        });
        Self {
            pass,
            total: units.len(),
            pending: units.into(),
            roots: 0,
            checks: 0,
            started: Instant::now(),
        }
    }

    pub fn next(&mut self) -> Option<CheckUnit> {
        let unit = self.pending.pop_front()?;
        self.roots += 1;
        Some(unit)
    }

    pub fn retain_candidates(&mut self, candidates: &[bool]) {
        self.pending
            .retain(|unit| unit.members.iter().any(|&index| candidates[index]));
        self.total = self.pending.len();
    }

    pub fn include_loaded(
        &mut self,
        unit: &mut CheckUnit,
        files: &[(PathBuf, String)],
        indices: &FileSpanIndices,
    ) {
        self.pending.retain(|candidate| {
            if candidate
                .members
                .iter()
                .all(|&index| source_index(indices, &files[index].0, &unit.root).is_some())
            {
                unit.members.extend(&candidate.members);
                false
            } else {
                true
            }
        });
        unit.members.sort_unstable();
        unit.members.dedup();
    }

    pub fn check(&mut self, root: &Path, documents: &SourceSet) -> DocumentFrontendState {
        self.checks += 1;
        let measure = std::env::var_os("HEW_MEASURE_TIMINGS").is_some();
        let started = Instant::now();
        if measure {
            eprintln!(
                "hew measure: migration {} check {} start: {} ({} covering roots, {} pending units)",
                self.pass,
                self.checks,
                root.display(),
                self.roots,
                self.pending.len()
            );
        }
        let state = hew_compile::run_document_frontend(
            &root.display().to_string(),
            &FrontendOptions {
                documents: documents.clone(),
                ..FrontendOptions::default()
            },
        );
        if measure {
            let checked = state
                .typecheck_result
                .as_ref()
                .is_some_and(|result| result.tco.is_some());
            eprintln!(
                "hew measure: migration {} check {} end: {:.3} ms, checked={checked}, {} diagnostics",
                self.pass,
                self.checks,
                started.elapsed().as_secs_f64() * 1000.0,
                state.diagnostics.len()
            );
        }
        state
    }

    pub fn report(&self) {
        if std::env::var_os("HEW_MEASURE_TIMINGS").is_some() {
            eprintln!(
                "hew measure: migration {}: {} units, {} covering roots, {} frontend checks",
                self.pass, self.total, self.roots, self.checks
            );
            crate::measure_compile_phase(
                &format!("migration-{}", self.pass),
                self.started.elapsed(),
            );
        }
    }
}

pub(crate) fn source_index(indices: &FileSpanIndices, path: &Path, root: &Path) -> Option<u32> {
    let canonical = std::fs::canonicalize(path).ok();
    canonical
        .as_deref()
        .and_then(|path| indices.path_index(path))
        .or_else(|| indices.path_index(path))
        .or_else(|| {
            let root = std::fs::canonicalize(root).ok();
            (canonical.is_some() && canonical == root).then_some(0)
        })
}
