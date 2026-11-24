//! Documentation generator for the Hew programming language.
//!
//! This module extracts doc comments from parsed Hew source files and renders
//! them as static HTML pages or Markdown files.

mod config;
mod extract;
pub mod highlight;
pub mod markdown;
mod render;
mod template;

pub use extract::extract_docs;
pub use highlight::highlight;
pub use render::{
    build_reference_index, page_title, render_index, render_module_with_index, render_sidebar,
};
pub use template::{not_found_page, wrap_page_with_sidebar};

fn validate_package_root_name(name: &str) -> Result<String, String> {
    if name.is_empty()
        || name.contains('\0')
        || name.contains('/')
        || name.contains('\\')
        || name.contains(':')
    {
        return Err(format!(
            "package name `{name}` contains invalid path characters"
        ));
    }
    if name.split('.').any(|segment| {
        segment.is_empty() || segment == "." || segment == ".." || {
            let mut components = std::path::Path::new(segment).components();
            !matches!(
                (components.next(), components.next()),
                (Some(std::path::Component::Normal(_)), None)
            )
        }
    }) {
        return Err(format!(
            "package name `{name}` must consist of plain dot-separated segments"
        ));
    }
    let converted = name.replace('.', "::");
    validate_doc_filename(&module_to_filename(&converted, "html"))?;
    Ok(converted)
}

fn validate_doc_filename(filename: &str) -> Result<(), String> {
    if filename.is_empty()
        || filename.contains('\0')
        || filename.contains('/')
        || filename.contains('\\')
        || filename.contains(':')
    {
        return Err(format!(
            "unsafe documentation filename `{filename}`: must be a single plain filename"
        ));
    }
    let mut components = std::path::Path::new(filename).components();
    if !matches!(
        (components.next(), components.next()),
        (Some(std::path::Component::Normal(_)), None)
    ) {
        return Err(format!(
            "unsafe documentation filename `{filename}`: must be a single normal path component"
        ));
    }
    Ok(())
}

fn safe_doc_output_path(
    out: &std::path::Path,
    filename: &str,
) -> Result<std::path::PathBuf, String> {
    validate_doc_filename(filename)?;
    Ok(out.join(filename))
}

fn resolve_root_name(root: &std::path::Path) -> Result<String, String> {
    let manifest_path = root.join("hew.toml");
    if manifest_path.is_file() {
        if let Ok(manifest) = hew_pkg::manifest::parse_manifest(&manifest_path) {
            return validate_package_root_name(&manifest.package.name).map_err(|reason| {
                format!(
                    "invalid package name in {}: {reason}",
                    manifest_path.display()
                )
            });
        }
    }

    if let Some(name) = root.file_name().and_then(|s| s.to_str()) {
        return Ok(name.to_string());
    }

    let resolved = std::fs::canonicalize(root).ok().or_else(|| {
        if root.as_os_str() == "." || root.as_os_str().is_empty() {
            std::env::current_dir().ok()
        } else {
            None
        }
    });
    Ok(resolved
        .as_deref()
        .and_then(std::path::Path::file_name)
        .and_then(|s| s.to_str())
        .unwrap_or("unknown")
        .to_string())
}

/// Derive a module name from a file path relative to a root directory.
///
/// Includes the root directory name as the first component to produce
/// fully-qualified names. If the file stem matches its immediate parent
/// directory, the redundant component is collapsed:
/// `std/encoding/json/json.hew` → `std::encoding::json`.
#[cfg(test)]
fn derive_module_name(file_path: &std::path::Path, root: &std::path::Path) -> String {
    let root_name = resolve_root_name(root).expect("valid test root name");
    derive_module_name_with_root(file_path, root, &root_name)
}

fn derive_module_name_with_root(
    file_path: &std::path::Path,
    root: &std::path::Path,
    root_name: &str,
) -> String {
    let relative = file_path.strip_prefix(root).unwrap_or(file_path);
    let stem = relative
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("unknown");
    let parent = relative.parent().unwrap_or(std::path::Path::new(""));

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
            if matches!(
                p.file_name().and_then(|s| s.to_str()),
                Some(".git" | "target" | ".hew")
            ) {
                continue;
            }
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
pub fn cmd_doc(args: &crate::args::DocArgs) {
    let input_paths: Vec<String> = args.input.iter().map(|p| p.display().to_string()).collect();
    let output_dir = args.output_dir.display().to_string();
    let open_after = args.open;
    let use_markdown = matches!(
        args.format,
        crate::args::DocFormat::Markdown | crate::args::DocFormat::Md
    );
    let mut site_config = match args.site_config.as_deref() {
        Some(path) => config::DocSiteConfig::load(path).unwrap_or_else(|error| {
            eprintln!("Error: {error}");
            std::process::exit(1);
        }),
        None => config::DocSiteConfig::default(),
    };
    if let Some(version) = args.site_version.as_deref() {
        let version = version.trim();
        if version.is_empty() {
            eprintln!("Error: --site-version cannot be empty");
            std::process::exit(1);
        }
        site_config.display_version = version.to_string();
    }

    if input_paths.is_empty() {
        eprintln!(
            "Usage: hew doc <file_or_dir> [--output-dir <dir>] [--format html|markdown] [--site-config <file>] [--site-version <version>] [--open]"
        );
        std::process::exit(1);
    }

    // Collect .hew files with fully-qualified module names
    let mut entries: Vec<(std::path::PathBuf, String)> = Vec::new();
    for path_str in &input_paths {
        let path = std::path::Path::new(path_str);
        if path.is_dir() {
            let root_name = resolve_root_name(path).unwrap_or_else(|error| {
                eprintln!("Error: {error}");
                std::process::exit(1);
            });
            let mut files = Vec::new();
            collect_hew_files(path, &mut files);
            for file in files {
                let name = derive_module_name_with_root(&file, path, &root_name);
                if site_config.includes(&name) {
                    entries.push((file, name));
                }
            }
        } else if path.exists() {
            let name = path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("unknown")
                .to_string();
            if site_config.includes(&name) {
                entries.push((path.to_path_buf(), name));
            }
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
            let mut has_parse_error = false;
            for err in &result.errors {
                match err.severity {
                    hew_parser::Severity::Warning => {
                        eprintln!("Warning: {}: {}", file_path.display(), err.message);
                    }
                    hew_parser::Severity::Error => {
                        eprintln!("Error: {}: {}", file_path.display(), err.message);
                        has_parse_error = true;
                    }
                }
            }
            if has_parse_error {
                had_errors = true;
                continue;
            }
        }

        let module = extract_docs(&result.program, module_name);
        modules.push(module);
    }

    if modules.is_empty() {
        eprintln!("No modules to document");
        std::process::exit(1);
    }

    // Validate every generated output filename before creating or writing to
    // the output directory so a malicious module or package name cannot escape
    // `out`.
    let out = std::path::Path::new(&output_dir);
    let output_ext = if use_markdown { "md" } else { "html" };
    let mut module_outputs = Vec::with_capacity(modules.len());
    for module in &modules {
        let filename = module_to_filename(&module.name, output_ext);
        let output_path = safe_doc_output_path(out, &filename).unwrap_or_else(|error| {
            eprintln!("Error: {error}");
            std::process::exit(1);
        });
        module_outputs.push((filename, output_path));
    }

    // Create output directory
    if let Err(e) = std::fs::create_dir_all(out) {
        eprintln!("Error creating output directory: {e}");
        std::process::exit(1);
    }

    if use_markdown {
        // Render Markdown
        let index_md = markdown::render_index(&modules, &site_config);
        if let Err(e) = std::fs::write(out.join("README.md"), &index_md) {
            eprintln!("Error writing README.md: {e}");
            std::process::exit(1);
        }

        for (module, (filename, output_path)) in modules.iter().zip(&module_outputs) {
            let md = markdown::render_module(module, site_config.is_guide(&module.name));
            if let Err(e) = std::fs::write(output_path, &md) {
                eprintln!("Error writing {filename}: {e}");
                had_errors = true;
            }
        }
    } else {
        // Render HTML
        let reference_index = build_reference_index(&modules);
        let index_body = render_index(&modules, &site_config);
        let index_sidebar = render_sidebar(&modules, &site_config, None);
        let index_html =
            wrap_page_with_sidebar(&site_config, "Index", &index_body, None, &index_sidebar);
        if let Err(e) = std::fs::write(out.join("index.html"), &index_html) {
            eprintln!("Error writing index.html: {e}");
            std::process::exit(1);
        }
        if let Err(e) = std::fs::write(out.join("404.html"), not_found_page(&site_config)) {
            eprintln!("Error writing 404.html: {e}");
            std::process::exit(1);
        }

        for (module, (filename, output_path)) in modules.iter().zip(&module_outputs) {
            let is_guide = site_config.is_guide(&module.name);
            let body = render_module_with_index(module, &reference_index, is_guide);
            let display_name = page_title(module, is_guide);
            let escaped_name = crate::util::html_escape(&display_name);
            let breadcrumb = format!(
                "<a href=\"index.html\">Index</a><a href=\"{filename}\" aria-current=\"page\">{escaped_name}</a>"
            );
            let sidebar = render_sidebar(&modules, &site_config, Some(&module.name));
            let html = wrap_page_with_sidebar(
                &site_config,
                &display_name,
                &body,
                Some(&breadcrumb),
                &sidebar,
            );
            if let Err(e) = std::fs::write(output_path, &html) {
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

#[cfg(target_os = "windows")]
fn strip_windows_verbatim_prefix(path: &std::path::Path) -> std::path::PathBuf {
    use std::os::windows::ffi::{OsStrExt, OsStringExt};

    const fn wide_ascii(byte: u8) -> u16 {
        u16::from_le_bytes([byte, 0])
    }

    const VERBATIM: [u16; 4] = [
        wide_ascii(b'\\'),
        wide_ascii(b'\\'),
        wide_ascii(b'?'),
        wide_ascii(b'\\'),
    ];
    const VERBATIM_UNC: [u16; 8] = [
        wide_ascii(b'\\'),
        wide_ascii(b'\\'),
        wide_ascii(b'?'),
        wide_ascii(b'\\'),
        wide_ascii(b'U'),
        wide_ascii(b'N'),
        wide_ascii(b'C'),
        wide_ascii(b'\\'),
    ];

    let wide: Vec<u16> = path.as_os_str().encode_wide().collect();
    if let Some(rest) = wide.strip_prefix(&VERBATIM_UNC) {
        let mut unc = vec![wide_ascii(b'\\'), wide_ascii(b'\\')];
        unc.extend_from_slice(rest);
        return std::path::PathBuf::from(std::ffi::OsString::from_wide(&unc));
    }
    if let Some(rest) = wide.strip_prefix(&VERBATIM) {
        if rest.len() >= 2 && rest[1] == wide_ascii(b':') {
            return std::path::PathBuf::from(std::ffi::OsString::from_wide(rest));
        }
    }
    path.to_path_buf()
}

fn open_in_browser_command(path: &std::path::Path) -> std::process::Command {
    #[cfg(target_os = "windows")]
    {
        let target = strip_windows_verbatim_prefix(path);
        let mut command = std::process::Command::new("cmd");
        command.args(["/C", "start", ""]).arg(target);
        command
    }
    #[cfg(not(target_os = "windows"))]
    {
        #[cfg(target_os = "macos")]
        let cmd = "open";
        #[cfg(not(target_os = "macos"))]
        let cmd = "xdg-open";

        let mut command = std::process::Command::new(cmd);
        command.arg(path);
        command
    }
}

/// Attempt to open a file in the default browser.
fn open_in_browser(path: &std::path::Path) -> Result<(), String> {
    let resolved = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    open_in_browser_command(&resolved)
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
            module_to_filename("std::net::http::client", "md"),
            "std.net.http.client.md"
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
            std::path::Path::new("std/net/http/client.hew"),
            std::path::Path::new("std"),
        );
        assert_eq!(name, "std::net::http::client");
    }

    #[test]
    fn derive_name_single_dir_collapsed() {
        let name = derive_module_name(
            std::path::Path::new("std/log/log.hew"),
            std::path::Path::new("std"),
        );
        assert_eq!(name, "std::log");
    }

    #[test]
    fn resolve_root_name_handles_dot_and_manifest() {
        let dir = tempfile::tempdir().expect("tempdir");
        let expected_dir = dir
            .path()
            .file_name()
            .and_then(|s| s.to_str())
            .expect("dir name");
        let main_file = dir.path().join("main.hew");
        std::fs::write(&main_file, "fn main() {}\n").expect("write");

        let dot_relative = dir.path().join(".");
        assert_eq!(
            derive_module_name(&dot_relative.join("main.hew"), &dot_relative),
            format!("{expected_dir}::main")
        );

        std::fs::write(
            dir.path().join("hew.toml"),
            "[package]\nname = \"logscope\"\nversion = \"0.1.0\"\n",
        )
        .expect("write manifest");
        assert_eq!(
            derive_module_name(&dot_relative.join("main.hew"), &dot_relative),
            "logscope::main"
        );
        assert_eq!(
            derive_module_name(&dot_relative.join("logscope.hew"), &dot_relative),
            "logscope"
        );
    }

    #[test]
    fn collect_hew_files_skips_ignored_directories() {
        let dir = tempfile::tempdir().expect("tempdir");
        std::fs::create_dir_all(dir.path().join("src")).expect("mkdir src");
        std::fs::create_dir_all(dir.path().join(".git")).expect("mkdir .git");
        std::fs::create_dir_all(dir.path().join("target")).expect("mkdir target");
        std::fs::create_dir_all(dir.path().join(".hew")).expect("mkdir .hew");
        std::fs::write(dir.path().join("src/app.hew"), "fn main() {}\n").expect("write app");
        std::fs::write(dir.path().join(".git/ignored.hew"), "").expect("write .git");
        std::fs::write(dir.path().join("target/ignored.hew"), "").expect("write target");
        std::fs::write(dir.path().join(".hew/ignored.hew"), "").expect("write .hew");

        let mut files = Vec::new();
        collect_hew_files(dir.path(), &mut files);
        assert_eq!(files, vec![dir.path().join("src/app.hew")]);
    }

    #[test]
    fn resolve_root_name_rejects_malicious_package_names() {
        for bad_name in [
            "../../escape",
            "/abs/path",
            "a\\b",
            "..\\..\\escape",
            "C:evil",
            "..",
            ".",
            "foo..bar",
            ".leading",
            "trailing.",
        ] {
            let dir = tempfile::tempdir().expect("tempdir");
            std::fs::write(
                dir.path().join("hew.toml"),
                format!("[package]\nname = {bad_name:?}\nversion = \"0.1.0\"\n"),
            )
            .expect("write manifest");
            let err = resolve_root_name(dir.path())
                .expect_err(&format!("malicious package name {bad_name:?} must fail"));
            assert!(
                err.contains("invalid package name"),
                "unexpected error for {bad_name:?}: {err}"
            );
        }
    }

    #[test]
    fn safe_doc_output_path_rejects_traversal_and_special_filenames() {
        let out = std::path::Path::new("/tmp/hew-doc-out");
        for bad in [
            "../../escape.html",
            "/abs/path.html",
            "a\\b.html",
            "..\\escape.md",
            "C:evil.html",
            "..",
            ".",
            "",
            "bad\0name.html",
            "nested/page.html",
        ] {
            assert!(
                safe_doc_output_path(out, bad).is_err(),
                "unsafe doc filename {bad:?} must be rejected"
            );
        }
        assert_eq!(
            safe_doc_output_path(out, "std.encoding.json.html").expect("safe filename"),
            out.join("std.encoding.json.html")
        );
    }

    #[test]
    fn open_in_browser_command_uses_platform_launcher_and_preserves_path() {
        #[cfg(target_os = "windows")]
        {
            let path = std::path::Path::new(r"\\?\C:\Users\dev\docs\index.html");
            let command = open_in_browser_command(path);
            assert_eq!(command.get_program(), "cmd");
            let args: Vec<_> = command.get_args().collect();
            assert_eq!(args, ["/C", "start", "", r"C:\Users\dev\docs\index.html"]);
        }
        #[cfg(not(target_os = "windows"))]
        {
            let path = std::path::Path::new("/tmp/hew docs/index.html");
            let command = open_in_browser_command(path);
            #[cfg(target_os = "macos")]
            assert_eq!(command.get_program(), "open");
            #[cfg(not(target_os = "macos"))]
            assert_eq!(command.get_program(), "xdg-open");
            let args: Vec<_> = command.get_args().collect();
            assert_eq!(args, [path.as_os_str()]);
        }
    }
}
