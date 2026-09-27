//! Executable documentation examples for `hew test --doc`.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use super::discovery::{self, DocTest, TestCase};

pub struct PreparedDocTests {
    pub tests: Vec<TestCase>,
    _generated: tempfile::TempDir,
}

struct Fence {
    code: String,
    item: String,
    ignored: bool,
    no_run: bool,
    expected_stdout: Option<String>,
}

pub fn prepare(paths: &[String]) -> Result<PreparedDocTests, String> {
    let generated = tempfile::Builder::new()
        .prefix("hew_doc_tests_")
        .tempdir()
        .map_err(|error| format!("create doc test directory: {error}"))?;
    let root = std::env::current_dir()
        .and_then(|path| path.canonicalize())
        .map_err(|error| format!("resolve test directory: {error}"))?;
    let mut files = Vec::new();
    for requested in paths {
        collect_files(Path::new(requested), &mut files)?;
    }
    files.sort();
    files.dedup();

    let mut tests = Vec::new();
    for file in files {
        let source = std::fs::read_to_string(&file)
            .map_err(|error| format!("read {}: {error}", file.display()))?;
        let fences = extract_fences(&source, file.extension().is_some_and(|ext| ext == "hew"));
        let mut by_item = HashMap::<String, usize>::new();
        for fence in fences {
            let number = by_item.entry(fence.item.clone()).or_default();
            *number += 1;
            let relative = file.strip_prefix(&root).unwrap_or(&file).display();
            let identity = format!("{relative}::doc({})#{}", fence.item, number);
            let selector = format!("{}::doc({})#{}", file.display(), fence.item, number);
            let generated_path = generated.path().join(format!("doc_{}.hew", tests.len()));
            let generated_source = example_source(&source, &fence, &file);
            std::fs::write(&generated_path, generated_source)
                .map_err(|error| format!("write {}: {error}", generated_path.display()))?;
            let path = generated_path.display().to_string();
            let mut discovered = discovery::discover_tests_in_file(&path)?;
            let parse_error = discovered
                .parse_errors
                .iter()
                .find(|error| error.severity == hew_parser::Severity::Error)
                .map(|error| format!("{identity}: {}", error.message));
            if parse_error.is_some() {
                std::fs::write(&generated_path, "#[test]\nfn __hew_doc_example() {}\n")
                    .map_err(|error| format!("write {}: {error}", generated_path.display()))?;
                discovered = discovery::discover_tests_in_file(&path)?;
            }
            let mut test = discovered
                .tests
                .into_iter()
                .next()
                .ok_or_else(|| format!("{identity}: generated doc test has no entry"))?;
            test.ignored = fence.ignored;
            test.doc = Some(DocTest {
                identity,
                selector,
                origin: file.display().to_string(),
                expected_stdout: fence.expected_stdout,
                no_run: fence.no_run,
                parse_error,
            });
            tests.push(test);
        }
    }
    Ok(PreparedDocTests {
        tests,
        _generated: generated,
    })
}

fn collect_files(path: &Path, files: &mut Vec<PathBuf>) -> Result<(), String> {
    if path.is_file() {
        if matches!(
            path.extension().and_then(|ext| ext.to_str()),
            Some("hew" | "md")
        ) {
            files.push(path.to_path_buf());
        }
        return Ok(());
    }
    let entries =
        std::fs::read_dir(path).map_err(|error| format!("read {}: {error}", path.display()))?;
    for entry in entries {
        let entry = entry.map_err(|error| format!("read directory entry: {error}"))?;
        let child = entry.path();
        let kind = entry
            .file_type()
            .map_err(|error| format!("inspect {}: {error}", child.display()))?;
        if kind.is_dir() {
            if !matches!(
                child.file_name().and_then(|name| name.to_str()),
                Some("target" | ".git" | "node_modules")
            ) {
                collect_files(&child, files)?;
            }
        } else if kind.is_file()
            && matches!(
                child.extension().and_then(|ext| ext.to_str()),
                Some("hew" | "md")
            )
        {
            files.push(child);
        }
    }
    Ok(())
}

fn extract_fences(source: &str, hew_source: bool) -> Vec<Fence> {
    let lines = source.lines().collect::<Vec<_>>();
    let mut fences = Vec::new();
    let mut index = 0;
    while index < lines.len() {
        let Some(line) = doc_line(lines[index], hew_source) else {
            index += 1;
            continue;
        };
        let marker = line.trim();
        let mode = marker.strip_prefix("```");
        let Some(mode) = mode else {
            index += 1;
            continue;
        };
        let (ignored, no_run) = match mode {
            "hew" | "" if hew_source || mode == "hew" => (false, false),
            "hew,ignore" => (true, false),
            "hew,no_run" => (false, true),
            _ => {
                index += 1;
                continue;
            }
        };
        let item = if hew_source {
            enclosing_item(&lines, index)
        } else {
            "module".to_string()
        };
        index += 1;
        let mut code = String::new();
        while index < lines.len() {
            let Some(content) = doc_line(lines[index], hew_source) else {
                break;
            };
            if content.trim() == "```" {
                index += 1;
                break;
            }
            code.push_str(content);
            code.push('\n');
            index += 1;
        }
        let expected_stdout = expected_output(&code);
        fences.push(Fence {
            code,
            item,
            ignored,
            no_run,
            expected_stdout,
        });
    }
    fences
}

fn doc_line(line: &str, hew_source: bool) -> Option<&str> {
    if !hew_source {
        return Some(line);
    }
    let line = line.trim_start();
    line.strip_prefix("///")
        .or_else(|| line.strip_prefix("//!"))
        .map(|line| line.strip_prefix(' ').unwrap_or(line))
}

fn enclosing_item(lines: &[&str], fence_start: usize) -> String {
    if lines[fence_start].trim_start().starts_with("//!") {
        return "module".to_string();
    }
    for line in lines.iter().skip(fence_start + 1) {
        let line = line.trim();
        if line.starts_with("///") || line.starts_with("#[") || line.is_empty() {
            continue;
        }
        let tokens = line
            .split(|ch: char| !ch.is_alphanumeric() && ch != '_')
            .filter(|part| !part.is_empty())
            .collect::<Vec<_>>();
        if let Some(position) = tokens.iter().position(|token| {
            matches!(
                *token,
                "fn" | "struct" | "enum" | "actor" | "trait" | "record"
            )
        }) {
            if let Some(name) = tokens.get(position + 1) {
                return (*name).to_string();
            }
        }
        break;
    }
    "module".to_string()
}

fn expected_output(code: &str) -> Option<String> {
    let lines = code.lines().collect::<Vec<_>>();
    let start = lines.iter().position(|line| line.trim() == "// Output:")?;
    let output = lines[start + 1..]
        .iter()
        .take_while(|line| line.trim_start().starts_with("//"))
        .map(|line| {
            line.trim_start()
                .strip_prefix("//")
                .expect("output line begins with comment marker")
                .trim_start()
                .trim_end_matches('\r')
        })
        .collect::<Vec<_>>()
        .join("\n");
    Some(if output.is_empty() {
        output
    } else {
        format!("{output}\n")
    })
}

fn example_source(module: &str, fence: &Fence, path: &Path) -> String {
    if fence.ignored {
        return "#[test]\nfn __hew_doc_example() {}\n".to_string();
    }
    let standalone = path.extension().is_some_and(|ext| ext == "md")
        || fence.code.contains("fn main(")
        || fence
            .code
            .lines()
            .any(|line| line.trim_start().starts_with("import "));
    let base = if standalone { "" } else { module };
    if fence.code.contains("fn main(") {
        format!(
            "{base}\n{}\n#[test]\nfn __hew_doc_example() {{ main(); }}\n",
            fence.code
        )
    } else {
        format!(
            "{base}\n#[test]\nfn __hew_doc_example() {{\n{}\n}}\n",
            fence.code
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn source_fences_keep_item_and_output_contract() {
        let source = "/// Example\n/// ```hew\n/// println(3);\n/// // Output:\n/// // 3\n/// ```\npub fn split() {}\n";
        let examples = extract_fences(source, true);
        assert_eq!(examples.len(), 1);
        assert_eq!(examples[0].item, "split");
        assert_eq!(examples[0].expected_stdout.as_deref(), Some("3\n"));
        assert!(
            example_source(source, &examples[0], Path::new("split.hew")).contains("pub fn split()")
        );
    }

    #[test]
    fn markdown_modes_select_compile_run_and_skip() {
        let source = "```hew\nprintln(1);\n```\n```hew,no_run\nprintln(2);\n```\n```hew,ignore\nprintln(3);\n```\n";
        let examples = extract_fences(source, false);
        assert_eq!(examples.len(), 3);
        assert!(examples[1].no_run);
        assert!(examples[2].ignored);
    }
}
