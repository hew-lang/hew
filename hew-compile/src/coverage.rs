use std::collections::BTreeSet;

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct SourceCoverageMap {
    pub counters: Vec<SourceCoverageCounter>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SourceCoverageCounter {
    pub id: u32,
    pub file: String,
    pub lines: Vec<u32>,
}

impl super::SessionOutput {
    /// Instrument shared SIR and resolve its sites through the existing source graph.
    ///
    /// # Panics
    /// Panics if source lines or coverage counters exceed the `u32` range.
    pub fn instrument_coverage(
        &mut self,
        program: &hew_parser::ast::Program,
        source: &str,
        filename: &str,
    ) -> SourceCoverageMap {
        self.sir.module.instrument_coverage();
        let sources = super::build_module_source_map(program, &super::DocumentSet::new());
        let mut counters = Vec::new();
        for counter in self.sir.module.coverage.values() {
            let mut files = std::collections::BTreeMap::<String, BTreeSet<u32>>::new();
            for (module, offset) in &counter.sites {
                let (text, file) = match module {
                    None => (source, filename),
                    Some(name) => {
                        if name == "std" || name.starts_with("std.") {
                            continue;
                        }
                        let Some((text, file)) = sources.get(name) else {
                            continue;
                        };
                        (text.as_str(), file.as_str())
                    }
                };
                if *offset > text.len() {
                    continue;
                }
                let line = u32::try_from(
                    text.as_bytes()[..*offset]
                        .split(|byte| *byte == b'\n')
                        .count(),
                )
                .expect("source line capacity");
                let path = std::path::Path::new(file)
                    .canonicalize()
                    .unwrap_or_else(|_| file.into());
                files
                    .entry(super::display_path(&path))
                    .or_default()
                    .insert(line);
            }
            counters.extend(
                files
                    .into_iter()
                    .map(|(file, lines)| SourceCoverageCounter {
                        id: counter.id,
                        file,
                        lines: lines.into_iter().collect(),
                    }),
            );
        }
        SourceCoverageMap { counters }
    }
}
