//! Merge isolated test executions into deterministic source-line lcov.

use std::collections::BTreeMap;
use std::fmt::Write as _;
use std::path::Path;
use std::sync::Mutex;

use hew_compile::SourceCoverageMap;

#[derive(Default)]
pub struct Coverage {
    lines: Mutex<BTreeMap<String, BTreeMap<u32, u64>>>,
}

impl Coverage {
    pub fn include(&self, map: &SourceCoverageMap) {
        self.record(map, &BTreeMap::new());
    }

    pub fn record(&self, map: &SourceCoverageMap, counters: &BTreeMap<u32, u64>) {
        let mut execution = BTreeMap::<&str, BTreeMap<u32, u64>>::new();
        for counter in &map.counters {
            let hits = counters.get(&counter.id).copied().unwrap_or(0);
            let lines = execution.entry(&counter.file).or_default();
            for &line in &counter.lines {
                let value = lines.entry(line).or_default();
                *value = (*value).max(hits);
            }
        }
        let mut totals = self
            .lines
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        for (file, lines) in execution {
            let total = totals.entry(file.to_string()).or_default();
            for (line, hits) in lines {
                let value = total.entry(line).or_default();
                *value = value.saturating_add(hits);
            }
        }
    }

    pub fn write(&self, path: &Path) -> Result<(), String> {
        let totals = self
            .lines
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let mut output = String::new();
        for (file, lines) in &*totals {
            if file.contains(['\n', '\r']) {
                return Err("lcov cannot represent a source filename containing a newline".into());
            }
            writeln!(output, "TN:\nSF:{file}").expect("string formatting");
            for (line, hits) in lines {
                writeln!(output, "DA:{line},{hits}").expect("string formatting");
            }
            writeln!(
                output,
                "LF:{}\nLH:{}\nend_of_record",
                lines.len(),
                lines.values().filter(|&&hits| hits > 0).count()
            )
            .expect("string formatting");
        }
        if let Some(parent) = path
            .parent()
            .filter(|parent| !parent.as_os_str().is_empty())
        {
            std::fs::create_dir_all(parent)
                .map_err(|error| format!("create coverage directory: {error}"))?;
        }
        std::fs::write(path, output).map_err(|error| format!("write {}: {error}", path.display()))
    }
}
