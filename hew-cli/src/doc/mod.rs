//! Documentation generator for the Hew programming language.
//!
//! This module extracts doc comments from parsed Hew source files and renders
//! them as static HTML pages or Markdown files.

mod extract;
pub mod highlight;
pub mod markdown;
mod render;
mod template;

pub use extract::extract_docs;
pub use highlight::highlight;
pub use render::{render_index, render_module};
pub use template::wrap_page;

/// Derive a module name from a file path relative to a root directory.
///
/// Includes the root directory name as the first component to produce
/// fully-qualified names. If the file stem matches its immediate parent
/// directory, the redundant component is collapsed:
/// `std/encoding/json/json.hew` → `std::encoding::json`.
fn derive_module_name(file_path: &std::path::Path, root: &std::path::Path) -> String {
    let relative = file_path.strip_prefix(root).unwrap_or(file_path);
    let stem = relative
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("unknown");
    let parent = relative.parent().unwrap_or(std::path::Path::new(""));

    // Start with the root directory name
    let root_name = root
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or("unknown");

    let mut parts: Vec<&str> = vec![root_name];
    parts.extend(parent.components().filter_map(|c| c.as_os_str().to_str()));

    // Collapse foo/foo.hew → foo
    if parts.last().is_none_or(|last| *last != stem) {
        parts.push(stem);
    }

    parts.join("::")
}

/// Convert a module name to a filename.
///
/// Replaces `::` with `.` so `std::encoding::json` becomes `std.encoding.json.html`.
pub fn module_to_filename(name: &str, ext: &str) -> String {
    format!("{}.{ext}", name.replace("::", "."))
}

/// Recursively collect `.hew` files from a directory.
fn collect_hew_files(dir: &std::path::Path, files: &mut Vec<std::path::PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    let mut sorted: Vec<_> = entries.flatten().collect();
    sorted.sort_by_key(std::fs::DirEntry::path);
    for entry in sorted {
        let p = entry.path();
        if p.is_dir() {
            collect_hew_files(&p, files);
        } else if p.extension().is_some_and(|e| e == "hew") {
            files.push(p);
        }
    }
}

#[expect(
    clippy::too_many_lines,
    reason = "CLI entry point with straightforward sequential logic"
)]
pub fn cmd_doc(args: &[String]) {
    let mut input_paths: Vec<String> = Vec::new();
    let mut output_dir = String::from("./doc");
    let mut open_after = false;
    let mut format = String::from("html");

    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--output-dir" | "-o" => {
                i += 1;
                if i < args.len() {
                    output_dir.clone_from(&args[i]);
                } else {
                    eprintln!("Error: --output-dir requires an argument");
                    std::process::exit(1);
                }
            }
            "--format" | "-f" => {
                i += 1;
                if i < args.len() {
                    format.clone_from(&args[i]);
                    if !["html", "markdown", "md"].contains(&format.as_str()) {
                        eprintln!("Error: --format must be 'html' or 'markdown'");
                        std::process::exit(1);
                    }
                } else {
                    eprintln!("Error: --format requires an argument (html, markdown)");
                    std::process::exit(1);
                }
            }
            "--open" => open_after = true,
            arg if arg.starts_with('-') => {
                eprintln!("Unknown option: {arg}");
                std::process::exit(1);
            }
            _ => input_paths.push(args[i].clone()),
        }
        i += 1;
    }

    if input_paths.is_empty() {
        eprintln!(
            "Usage: hew doc <file_or_dir> [--output-dir <dir>] [--format html|markdown] [--open]"
        );
        std::process::exit(1);
    }

    // Collect .hew files with fully-qualified module names
    let mut entries: Vec<(std::path::PathBuf, String)> = Vec::new();
    for path_str in &input_paths {
        let path = std::path::Path::new(path_str);
        if path.is_dir() {
            let mut files = Vec::new();
            collect_hew_files(path, &mut files);
            for file in files {
                let name = derive_module_name(&file, path);
                entries.push((file, name));
            }
        } else if path.exists() {
            let name = path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("unknown")
                .to_string();
            entries.push((path.to_path_buf(), name));
        } else {
            eprintln!("Error: {path_str} does not exist");
            std::process::exit(1);
        }
    }

    if entries.is_empty() {
        eprintln!("No .hew files found");
        std::process::exit(1);
    }

    entries.sort_by(|a, b| a.1.cmp(&b.1));

    // Parse and extract docs from each file
    let mut modules = Vec::new();
    let mut had_errors = false;

    for (file_path, module_name) in &entries {
        let source = match std::fs::read_to_string(file_path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("Error reading {}: {e}", file_path.display());
                had_errors = true;
                continue;
            }
        };

        let result = hew_parser::parse(&source);
        if !result.errors.is_empty() {
            eprintln!(
                "Warning: {} has parse errors, documentation may be incomplete",
                file_path.display()
            );
        }

        let module = extract_docs(&result.program, module_name);
        modules.push(module);
    }

    if modules.is_empty() {
        eprintln!("No modules to document");
        std::process::exit(1);
    }

    // Create output directory
    let out = std::path::Path::new(&output_dir);
    if let Err(e) = std::fs::create_dir_all(out) {
        eprintln!("Error creating output directory: {e}");
        std::process::exit(1);
    }

    let use_markdown = format == "markdown" || format == "md";

    if use_markdown {
        // Render Markdown
        let index_md = markdown::render_index(&modules);
        if let Err(e) = std::fs::write(out.join("README.md"), &index_md) {
            eprintln!("Error writing README.md: {e}");
            std::process::exit(1);
        }

        for module in &modules {
            let md = markdown::render_module(module);
            let filename = module_to_filename(&module.name, "md");
            if let Err(e) = std::fs::write(out.join(&filename), &md) {
                eprintln!("Error writing {filename}: {e}");
                had_errors = true;
            }
        }
    } else {
        // Render HTML
        let index_body = render_index(&modules);
        let index_html = wrap_page("Index", &index_body, None);
        if let Err(e) = std::fs::write(out.join("index.html"), &index_html) {
            eprintln!("Error writing index.html: {e}");
            std::process::exit(1);
        }

        for module in &modules {
            let body = render_module(module);
            let filename = module_to_filename(&module.name, "html");
            let escaped_name = template::html_escape(&module.name);
            let breadcrumb = format!(
                "<a href=\"index.html\">Index</a><a href=\"{filename}\">{escaped_name}</a>"
            );
            let html = wrap_page(&module.name, &body, Some(&breadcrumb));
            if let Err(e) = std::fs::write(out.join(&filename), &html) {
                eprintln!("Error writing {filename}: {e}");
                had_errors = true;
            }
        }
    }

    let fmt_label = if use_markdown { "Markdown" } else { "HTML" };
    let count = modules.len();
    eprintln!(
        "Generated {fmt_label} documentation for {count} module{} in {output_dir}/",
        if count == 1 { "" } else { "s" }
    );

    if had_errors {
        std::process::exit(1);
    }

    if open_after && !use_markdown {
        let index_path = out.join("index.html");
        let _ = open_in_browser(&index_path);
    }
}

/// Attempt to open a file in the default browser.
fn open_in_browser(path: &std::path::Path) -> Result<(), String> {
    let path_str = path
        .canonicalize()
        .unwrap_or_else(|_| path.to_path_buf())
        .display()
        .to_string();

    #[cfg(target_os = "linux")]
    let cmd = "xdg-open";
    #[cfg(target_os = "macos")]
    let cmd = "open";
    #[cfg(target_os = "windows")]
    let cmd = "start";
    #[cfg(not(any(target_os = "linux", target_os = "macos", target_os = "windows")))]
    let cmd = "xdg-open";

    std::process::Command::new(cmd)
        .arg(&path_str)
        .spawn()
        .map_err(|e| format!("Failed to open browser: {e}"))?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn module_to_filename_simple() {
        assert_eq!(module_to_filename("std::fs", "html"), "std.fs.html");
    }

    #[test]
    fn module_to_filename_nested() {
        assert_eq!(
            module_to_filename("std::encoding::json", "html"),
            "std.encoding.json.html"
        );
    }

    #[test]
    fn module_to_filename_deep() {
        assert_eq!(
            module_to_filename("std::net::http::http_client", "md"),
            "std.net.http.http_client.md"
        );
    }

    #[test]
    fn derive_name_root_file() {
        let name = derive_module_name(
            std::path::Path::new("std/fs.hew"),
            std::path::Path::new("std"),
        );
        assert_eq!(name, "std::fs");
    }

    #[test]
    fn derive_name_collapsed() {
        let name = derive_module_name(
            std::path::Path::new("std/encoding/json/json.hew"),
            std::path::Path::new("std"),
        );
        assert_eq!(name, "std::encoding::json");
    }

    #[test]
    fn derive_name_not_collapsed() {
        let name = derive_module_name(
            std::path::Path::new("std/net/http/http_client.hew"),
            std::path::Path::new("std"),
        );
        assert_eq!(name, "std::net::http::http_client");
    }

    #[test]
    fn derive_name_single_dir_collapsed() {
        let name = derive_module_name(
            std::path::Path::new("std/crypto/crypto.hew"),
            std::path::Path::new("std"),
        );
        assert_eq!(name, "std::crypto");
    }
}
