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

    // Collect .hew files
    let mut hew_files: Vec<std::path::PathBuf> = Vec::new();
    for path_str in &input_paths {
        let path = std::path::Path::new(path_str);
        if path.is_dir() {
            if let Ok(entries) = std::fs::read_dir(path) {
                for entry in entries.flatten() {
                    let p = entry.path();
                    if p.extension().is_some_and(|e| e == "hew") {
                        hew_files.push(p);
                    }
                }
            }
        } else if path.exists() {
            hew_files.push(path.to_path_buf());
        } else {
            eprintln!("Error: {path_str} does not exist");
            std::process::exit(1);
        }
    }

    if hew_files.is_empty() {
        eprintln!("No .hew files found");
        std::process::exit(1);
    }

    hew_files.sort();

    // Parse and extract docs from each file
    let mut modules = Vec::new();
    let mut had_errors = false;

    for file_path in &hew_files {
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

        let module_name = file_path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown")
            .to_string();

        let module = extract_docs(&result.program, &module_name);
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
            let filename = format!("{}.md", module.name);
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
            let breadcrumb = format!(
                "<a href=\"index.html\">Index</a><a href=\"{name}.html\">{name}</a>",
                name = module.name
            );
            let html = wrap_page(&module.name, &body, Some(&breadcrumb));
            let filename = format!("{}.html", module.name);
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
