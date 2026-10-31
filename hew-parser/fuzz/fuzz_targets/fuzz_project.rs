//! Manifest validation and module resolution over generated project trees.
//!
//! The input is a text archive: each `=== <relative path>` line starts a file
//! of the project and the lines after it are its contents; `=== @link <path>`
//! makes `<path>` a symbolic link to a directory outside the project. Paths
//! that would leave the project, or pass through a link, are dropped, so the
//! harness itself never writes outside the project.
//!
//! Each input becomes a fresh tree, removed after the input runs:
//!
//! ```text
//! <root>/outside/   canary modules beside the project
//! <root>/cwd/       the process working directory, also holding canaries
//! <root>/home/      HEW_HOME, so no account configuration is read
//! <root>/proj/      the generated project
//! ```
//!
//! Properties: manifest parsing, package resolution, manifest validation and
//! the `hew check` frontend never panic; a failed check reports a diagnostic
//! or a message; a resolved package's entry file lies inside the package;
//! without a path dependency, a quoted import or a link, every source the
//! frontend reads lies inside the project or the standard library; and the
//! working directory is never searched (spec §3.5.3).

#![no_main]

mod support;

use std::path::{Component, Path, PathBuf};

use libfuzzer_sys::fuzz_target;

const MAX_FILES: usize = 12;
const MAX_DEPTH: usize = 4;

struct Roots {
    root: PathBuf,
    proj: PathBuf,
    outside: PathBuf,
    cwd: PathBuf,
}

impl Drop for Roots {
    fn drop(&mut self) {
        let _ = std::env::set_current_dir(std::env::temp_dir());
        let _ = std::fs::remove_dir_all(&self.root);
    }
}

fn canaries(dir: &Path) {
    std::fs::create_dir_all(dir.join("canary")).unwrap();
    std::fs::write(dir.join("canary.hew"), "pub fn canary() -> i64 { 1 }\n").unwrap();
    std::fs::write(
        dir.join("canary/canary.hew"),
        "pub fn canary() -> i64 { 2 }\n",
    )
    .unwrap();
    std::fs::write(
        dir.join("hew.toml"),
        "[package]\nname = \"canary\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
}

fn roots() -> Roots {
    let base = std::env::temp_dir().join(format!("hew-fuzz-project-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    std::fs::create_dir_all(&base).unwrap();
    let root = base.canonicalize().unwrap();
    let roots = Roots {
        proj: root.join("proj"),
        outside: root.join("outside"),
        cwd: root.join("cwd"),
        root,
    };
    canaries(&roots.outside);
    canaries(&roots.cwd);
    std::fs::create_dir_all(&roots.proj).unwrap();
    std::fs::create_dir_all(roots.root.join("home")).unwrap();
    // Package configuration and installed packages come from here, never
    // from the account running the fuzzer.
    std::env::set_var("HEW_HOME", roots.root.join("home"));
    std::env::set_current_dir(&roots.cwd).unwrap();
    roots
}

fn std_roots() -> &'static [PathBuf] {
    static ROOTS: std::sync::OnceLock<Vec<PathBuf>> = std::sync::OnceLock::new();
    ROOTS.get_or_init(|| {
        support::module_search_paths()
            .into_iter()
            .map(|p| p.canonicalize().unwrap_or(p))
            .collect()
    })
}

/// A project-relative path with at most `MAX_DEPTH` plain components.
fn relative(spelling: &str) -> Option<PathBuf> {
    let path = Path::new(spelling.trim());
    let mut out = PathBuf::new();
    for component in path.components() {
        match component {
            Component::Normal(part) if !part.is_empty() => out.push(part),
            Component::CurDir => {}
            _ => return None,
        }
    }
    (out.components().count() > 0 && out.components().count() <= MAX_DEPTH).then_some(out)
}

#[derive(Default)]
struct Tree {
    links: usize,
}

/// Whether `relative` below `proj` passes through an existing link.
fn through_link(proj: &Path, relative: &Path) -> bool {
    let mut at = proj.to_path_buf();
    relative.components().any(|component| {
        at.push(component);
        at.symlink_metadata()
            .is_ok_and(|meta| meta.file_type().is_symlink())
    })
}

fn build(proj: &Path, outside: &Path, input: &str) -> Tree {
    let mut tree = Tree::default();
    let mut files = 0;
    let mut pieces = input.split("\n=== ");
    let sections = pieces
        .next()
        .and_then(|first| first.strip_prefix("=== "))
        .into_iter()
        .chain(pieces);
    for section in sections {
        if files == MAX_FILES {
            break;
        }
        let (header, body) = section.split_once('\n').unwrap_or((section, ""));
        // A body runs to the next header; the newline before it is the
        // header's, so put back the body's own final line break.
        let body = &format!("{body}\n");
        let (link, spelling) = match header.strip_prefix("@link ") {
            Some(rest) => (true, rest),
            None => (false, header),
        };
        let Some(path) = relative(spelling) else {
            continue;
        };
        if through_link(proj, &path) {
            continue;
        }
        let path = proj.join(path);
        if let Some(parent) = path.parent() {
            if std::fs::create_dir_all(parent).is_err() {
                continue;
            }
        }
        if path.exists() {
            continue;
        }
        files += 1;
        if link {
            #[cfg(unix)]
            if std::os::unix::fs::symlink(outside, &path).is_ok() {
                tree.links += 1;
            }
        } else {
            let _ = std::fs::write(&path, body);
        }
    }
    tree
}

fn under(path: &Path, root: &Path) -> bool {
    path.canonicalize()
        .unwrap_or_else(|_| path.to_path_buf())
        .starts_with(root)
}

fn quoted_import(source: &str) -> bool {
    source.contains("import \"") || source.contains("import\"")
}

fuzz_target!(|data: &[u8]| {
    let Ok(input) = std::str::from_utf8(data) else {
        return;
    };
    let roots = roots();
    let std_roots = std_roots();
    let tree = build(&roots.proj, &roots.outside, input);

    let manifest_path = roots.proj.join("hew.toml");
    let mut entry = roots.proj.join("main.hew");
    let mut path_dependencies = false;
    if manifest_path.is_file() {
        if let Ok(manifest) = hew_pkg::manifest::parse_manifest(&manifest_path) {
            path_dependencies = !manifest.dependencies.is_empty();
            let _ = hew_pkg::manifest::validate_for_publish(&manifest);
        }
        let _ = hew_pkg::cli::manifest_issues(&roots.proj);
        if let Ok(package) = hew_pkg::project::resolve_package(&roots.proj) {
            let declared = package.entry_path();
            assert!(
                under(&declared, &roots.proj),
                "package entry {} lies outside the package\n--- input ---\n{input}",
                declared.display()
            );
            entry = declared;
        }
    }
    if !entry.is_file() {
        return;
    }

    let options = hew_compile::FrontendOptions {
        project_dir: Some(roots.proj.clone()),
        module_search_paths: Some(support::module_search_paths()),
        ..Default::default()
    };
    let inputs = match hew_compile::check_file_with_state(entry.to_str().unwrap(), &options) {
        Ok((output, _)) => output.inputs,
        Err(failure) => {
            assert!(
                !failure.diagnostics.is_empty() || !failure.message.trim().is_empty(),
                "a failed check reported nothing\n--- input ---\n{input}"
            );
            failure.inputs
        }
    };

    // Quoted paths and links may lead anywhere today; dotted imports stay in
    // the project unless a dependency carries them out (spec 3.5.3).
    let contained = tree.links == 0 && !path_dependencies && !quoted_import(input);
    for read in inputs.iter().filter(|i| i.sha256.is_some()) {
        assert!(
            !under(&read.path, &roots.cwd),
            "module resolution searched the working directory: {}\n--- input ---\n{input}",
            read.path.display()
        );
        if contained && read.dependency_root.is_none() {
            assert!(
                under(&read.path, &roots.proj) || std_roots.iter().any(|r| under(&read.path, r)),
                "module resolution read {} outside the project\n--- input ---\n{input}",
                read.path.display()
            );
        }
    }
});
