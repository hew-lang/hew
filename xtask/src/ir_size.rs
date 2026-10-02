//! The IR-size ratchet.
//!
//! `tests/ll-oracle/ir-budget.tsv` holds one ceiling per program: the most
//! LLVM instructions the O2 module of that program may contain. The corpus is
//! every `tests/ll-oracle/corpus/*.hew`, generated suspension programs of one
//! and five units, and one real suspend-heavy core-acceptance case. Each is
//! compiled once by the given compiler binary with `--emit-llvm --opt-level 2`
//! and its `.ll` instructions are counted. Counts, not wall-clock, so the same
//! compiler gives the same numbers on every machine.
//!
//! A count over its ceiling fails. A count more than 10% under its ceiling
//! prints a hint to lower the row; ceilings are lowered, never raised.
use std::collections::BTreeMap;
use std::fmt::Write as _;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::Result;

const BUDGET_PATH: &str = "tests/ll-oracle/ir-budget.tsv";
const CORPUS_DIR: &str = "tests/ll-oracle/corpus";
const REAL_PROGRAM: &str =
    "tests/core-acceptance/cases/on-start-suspension-on-down-suspension-2.hew";
const GENERATED_UNITS: [usize; 2] = [1, 5];
/// Percent over the measured count that `--propose` grants a new row.
const HEADROOM_PERCENT: usize = 5;
/// Percent under its ceiling at which a program earns a lowering hint.
const HINT_PERCENT: usize = 10;

/// Header of the generated suspension programs: a watcher whose `down`
/// handler sleeps, so every unit suspends on spawn-watch-stop-poll-close.
const SUSPEND_HEADER: &str = "import std.link_monitor;

actor Target {
    receive fn ping() -> i64 {
        1
    }
}

actor Watcher {
    var downs: i64 = 0;
    receive fn watch(target: Target) -> link_monitor.MonitorRef {
        monitor(target).expect(\"monitor\")
    }
    receive fn seen() -> i64 {
        downs
    }
    #[on(down)]
    fn down(_note: link_monitor.DownNotification) {
        sleep(1ms);
        downs = downs + 1;
        println(\"down\");
    }
}

fn main() {
";

fn suspend_program(units: usize) -> String {
    let mut source = SUSPEND_HEADER.to_string();
    for i in 0..units {
        let _ = write!(
            source,
            "    let t{i} = spawn Target;
    let w{i} = spawn Watcher;
    let m{i} = w{i}.watch(t{i}).expect(\"watch\");
    stop(t{i});
    stopped(t{i});
    while w{i}.seen().expect(\"seen\") == 0 {{
        sleep(1ms);
    }}
    m{i}.close();
    stop(w{i});
    stopped(w{i});
"
        );
    }
    source.push_str("}\n");
    source
}

/// Instructions in the function bodies of a textual module: indented,
/// non-comment lines between a `define` and its closing brace. Labels, blank
/// lines, comments, declarations and globals are not instructions.
pub(crate) fn count_instructions(ir: &str) -> usize {
    let mut in_body = false;
    let mut count = 0;
    for line in ir.lines() {
        if in_body {
            if line == "}" {
                in_body = false;
            } else if line.starts_with(char::is_whitespace) && !line.trim_start().starts_with(';') {
                count += 1;
            }
        } else if line.starts_with("define ") && line.trim_end().ends_with('{') {
            in_body = true;
        }
    }
    count
}

/// Parse `program<TAB>max_instructions` rows; `#` lines and blanks are skipped.
pub(crate) fn parse_budget(text: &str) -> Result<BTreeMap<String, usize>> {
    let mut rows = BTreeMap::new();
    for (index, line) in text.lines().enumerate() {
        if line.trim().is_empty() || line.starts_with('#') {
            continue;
        }
        let number = index + 1;
        let (program, ceiling) = line.split_once('\t').ok_or_else(|| {
            format!("{BUDGET_PATH}:{number}: expected program<TAB>max_instructions")
        })?;
        let ceiling = ceiling
            .parse::<usize>()
            .map_err(|error| format!("{BUDGET_PATH}:{number}: bad ceiling {ceiling:?}: {error}"))?;
        if rows.insert(program.to_string(), ceiling).is_some() {
            return Err(format!(
                "{BUDGET_PATH}:{number}: duplicate program {program}"
            ));
        }
    }
    Ok(rows)
}

#[derive(Debug, Default, PartialEq, Eq)]
pub(crate) struct Verdict {
    pub(crate) failures: Vec<String>,
    pub(crate) hints: Vec<String>,
}

/// Compare measured counts with the ceilings.
pub(crate) fn evaluate(
    measured: &BTreeMap<String, usize>,
    budget: &BTreeMap<String, usize>,
) -> Verdict {
    let mut verdict = Verdict::default();
    for (program, count) in measured {
        match budget.get(program) {
            None => verdict.failures.push(format!(
                "{program}: {count} instructions and no row in {BUDGET_PATH}"
            )),
            Some(ceiling) if count > ceiling => verdict.failures.push(format!(
                "{program}: {count} instructions exceed the ceiling {ceiling}"
            )),
            Some(ceiling)
                if count * 100 < ceiling * (100 - HINT_PERCENT)
                    && propose_ceiling(*count) < *ceiling =>
            {
                verdict.hints.push(format!(
                    "{program}: {count} is more than {HINT_PERCENT}% under {ceiling}; lower the row to {}",
                    propose_ceiling(*count)
                ));
            }
            Some(_) => {}
        }
    }
    for program in budget
        .keys()
        .filter(|program| !measured.contains_key(*program))
    {
        verdict.failures.push(format!(
            "{program}: row in {BUDGET_PATH} names no program in the corpus"
        ));
    }
    verdict
}

fn propose_ceiling(count: usize) -> usize {
    (count * (100 + HEADROOM_PERCENT)).div_ceil(100)
}

/// The corpus as `(program name, source path)`, generated programs written
/// under `scratch`.
fn corpus(root: &Path, scratch: &Path) -> Result<Vec<(String, PathBuf)>> {
    let mut programs = Vec::new();
    let dir = root.join(CORPUS_DIR);
    for entry in fs::read_dir(&dir).map_err(|error| format!("{}: {error}", dir.display()))? {
        let path = entry.map_err(|error| error.to_string())?.path();
        if path.extension().is_some_and(|ext| ext == "hew") {
            let stem = path
                .file_stem()
                .and_then(|stem| stem.to_str())
                .unwrap_or_default();
            programs.push((format!("corpus/{stem}"), path));
        }
    }
    for units in GENERATED_UNITS {
        let path = scratch.join(format!("suspend_unroll{units}.hew"));
        fs::write(&path, suspend_program(units))
            .map_err(|error| format!("{}: {error}", path.display()))?;
        programs.push((format!("generated/suspend_unroll{units}"), path));
    }
    programs.push((
        "cases/on-start-suspension-on-down".to_string(),
        root.join(REAL_PROGRAM),
    ));
    programs.sort();
    Ok(programs)
}

fn measure(hew_bin: &Path, program: &Path, scratch: &Path) -> Result<usize> {
    let emit_dir = scratch.join("emit");
    let _ = fs::remove_dir_all(&emit_dir);
    let output = Command::new(hew_bin)
        .args([
            "tool",
            "compile",
            "--emit-llvm",
            "--opt-level",
            "2",
            "--emit-dir",
        ])
        .arg(&emit_dir)
        .arg(program)
        .output()
        .map_err(|error| format!("{}: {error}", hew_bin.display()))?;
    if !output.status.success() {
        return Err(format!(
            "{}: compile failed\n{}",
            program.display(),
            String::from_utf8_lossy(&output.stderr)
        ));
    }
    let stem = program.file_stem().unwrap_or_default();
    let ll = emit_dir.join(stem).with_extension("ll");
    let ir = fs::read_to_string(&ll).map_err(|error| format!("{}: {error}", ll.display()))?;
    Ok(count_instructions(&ir))
}

pub(crate) fn run(args: &[String]) -> Result<()> {
    let root = crate::workspace_root()?;
    let mut hew_bin = root.join("target/debug/hew");
    let mut propose = false;
    let mut iter = args.iter();
    while let Some(arg) = iter.next() {
        match arg.as_str() {
            "--hew-bin" => {
                hew_bin = PathBuf::from(iter.next().ok_or("--hew-bin needs a path")?);
            }
            "--propose" => propose = true,
            other => return Err(format!("ir-size: unknown option {other}")),
        }
    }
    let scratch = tempfile::tempdir().map_err(|error| error.to_string())?;
    let mut measured = BTreeMap::new();
    for (name, path) in corpus(&root, scratch.path())? {
        measured.insert(name, measure(&hew_bin, &path, scratch.path())?);
    }
    if propose {
        for (program, count) in &measured {
            println!("{program}\t{}", propose_ceiling(*count));
        }
        return Ok(());
    }
    let path = root.join(BUDGET_PATH);
    let budget = parse_budget(
        &fs::read_to_string(&path).map_err(|error| format!("{}: {error}", path.display()))?,
    )?;
    let verdict = evaluate(&measured, &budget);
    for (program, count) in &measured {
        let ceiling = budget
            .get(program)
            .map_or("-".to_string(), ToString::to_string);
        println!("{program}\t{count}\t{ceiling}");
    }
    for hint in &verdict.hints {
        println!("hint: {hint}");
    }
    if verdict.failures.is_empty() {
        println!("ir-size: OK ({} programs)", measured.len());
        Ok(())
    } else {
        Err(format!("ir-size: FAILED\n{}", verdict.failures.join("\n")))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn map(rows: &[(&str, usize)]) -> BTreeMap<String, usize> {
        rows.iter()
            .map(|(name, n)| ((*name).to_string(), *n))
            .collect()
    }

    #[test]
    fn counts_only_body_instructions() {
        let ir = "; ModuleID\n@g = constant i32 0\ndeclare void @f()\n\n\
define i32 @main() {\nentry:\n  %a = add i32 1, 2\n  ; note\n\n  ret i32 %a\n}\n\
define void @g() {\n  ret void\n}\n";
        assert_eq!(count_instructions(ir), 3);
    }

    #[test]
    fn parses_rows_and_rejects_malformed_ones() {
        assert_eq!(
            parse_budget("# c\na\t10\nb\t20\n").unwrap(),
            map(&[("a", 10), ("b", 20)])
        );
        assert!(parse_budget("a 10\n").is_err());
        assert!(parse_budget("a\tten\n").is_err());
        assert!(parse_budget("a\t1\na\t2\n").is_err());
    }

    #[test]
    fn within_ceiling_passes() {
        let verdict = evaluate(&map(&[("a", 100)]), &map(&[("a", 105)]));
        assert_eq!(verdict, Verdict::default());
    }

    #[test]
    fn over_ceiling_fails() {
        let verdict = evaluate(&map(&[("a", 106)]), &map(&[("a", 105)]));
        assert_eq!(verdict.failures.len(), 1);
        assert!(verdict.failures[0].contains("exceed the ceiling 105"));
    }

    #[test]
    fn far_under_ceiling_hints_a_lower_row() {
        let verdict = evaluate(&map(&[("a", 80)]), &map(&[("a", 105)]));
        assert!(verdict.failures.is_empty());
        assert!(verdict.hints[0].contains("lower the row to 84"));
    }

    #[test]
    fn unlisted_and_stale_rows_fail() {
        let verdict = evaluate(&map(&[("new", 1)]), &map(&[("old", 1)]));
        assert_eq!(verdict.failures.len(), 2);
    }

    #[test]
    fn generated_program_scales_by_unit() {
        let one = suspend_program(1);
        let five = suspend_program(5);
        assert_eq!(five.matches("spawn Watcher").count(), 5);
        assert!(one.contains("let t0") && five.contains("let t4"));
    }
}
