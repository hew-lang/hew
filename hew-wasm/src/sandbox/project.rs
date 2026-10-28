//! Named browser documents enter the same frontend as a single editor buffer.

use std::collections::BTreeSet;
use std::path::{Component, Path, PathBuf};

use serde::{Deserialize, Serialize};

use super::{compile_from_semantics, CompileError, CompileOutput, Diagnostic};

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Project {
    entry: String,
    files: Vec<SourceFile>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct SourceFile {
    path: String,
    source: String,
}

/// A compiled project and its entry-inclusive, sorted source dependency closure.
#[derive(Debug, Serialize)]
pub struct ProjectCompileOutput {
    #[serde(flatten)]
    pub output: CompileOutput,
    pub resolved_files: Vec<String>,
}

fn package_path(path: &str) -> bool {
    !path.contains('\\')
        && !path.contains(':')
        && !path
            .split('/')
            .any(|part| part.is_empty() || matches!(part, "." | ".."))
        && path != "std.hew"
        && !path.starts_with("std/")
        && !Path::new(path).components().any(|part| {
            matches!(
                part,
                Component::Prefix(_) | Component::RootDir | Component::ParentDir
            )
        })
}

fn invalid(message: impl Into<String>) -> ProjectCompileOutput {
    ProjectCompileOutput {
        output: CompileOutput {
            diagnostics: vec![Diagnostic::profile_error(0..0, "invalid_project", message)],
            bytecode: None,
        },
        resolved_files: Vec::new(),
    }
}

/// Compile a JSON `{entry, files: [{path, source}]}` project in isolation.
///
/// Files use canonical package-relative paths; the entry is a `.hew` source.
/// Manifests and other UTF-8 inputs use the native frontend's input authority.
/// The compiler's embedded
/// standard library retains its source identity; supplied files cannot replace it.
/// `resolved_files` includes supplied `.hew` sources read by the canonical frontend.
///
/// # Errors
/// Returns an internal error if verified semantics cannot be serialized.
pub fn compile_project_to_sandbox_bytecode(
    request: &str,
    profile: Option<&str>,
) -> Result<ProjectCompileOutput, CompileError> {
    let project: Project = match serde_json::from_str(request) {
        Ok(project) => project,
        Err(error) => return Ok(invalid(format!("invalid project request: {error}"))),
    };
    if !package_path(&project.entry)
        || Path::new(&project.entry)
            .extension()
            .is_none_or(|ext| ext != "hew")
    {
        return Ok(invalid(
            "entry must be a canonical package-relative .hew path outside std/",
        ));
    }
    // Native package membership consumes absolute source identities. This root
    // exists only in the source set; no directory or host file is accessed.
    let root = std::env::current_dir()
        .unwrap_or_else(|_| PathBuf::from("/"))
        .join(".hew-browser-project");
    let mut paths = BTreeSet::new();
    let mut inputs = hew_compile::SourceSet::in_memory();
    for file in &project.files {
        if !package_path(&file.path) {
            return Ok(invalid(format!(
                "invalid project source path: {}",
                file.path
            )));
        }
        if !paths.insert(file.path.clone()) {
            return Ok(invalid(format!(
                "duplicate project source path: {}",
                file.path
            )));
        }
        inputs.insert(root.join(&file.path), &file.source);
    }
    let Some(source) = inputs.get(&root.join(&project.entry)) else {
        return Ok(invalid("project entry is absent from files"));
    };
    let entry = root.join(&project.entry);
    let (mut output, admitted_inputs) = compile_from_semantics(
        source,
        profile,
        &entry.to_string_lossy(),
        hew_compile::FrontendOptions {
            documents: inputs.clone(),
            project_dir: Some(root.clone()),
            ..Default::default()
        },
        &mut Vec::new(),
        None,
    )?;
    for diagnostic in &mut output.diagnostics {
        project_identity(&mut diagnostic.source_module, &root);
        for note in &mut diagnostic.notes {
            project_identity(&mut note.source_module, &root);
        }
    }
    let mut resolved = BTreeSet::from([project.entry]);
    for input in admitted_inputs
        .into_iter()
        .filter(|input| input.sha256.is_some())
    {
        let path = input.path.strip_prefix(&root).unwrap_or(&input.path);
        let path = path.to_string_lossy().replace('\\', "/");
        if Path::new(&path).extension().is_some_and(|ext| ext == "hew") && paths.contains(&path) {
            resolved.insert(path);
        }
    }
    Ok(ProjectCompileOutput {
        output,
        resolved_files: resolved.into_iter().collect(),
    })
}

fn project_identity(identity: &mut Option<String>, root: &Path) {
    if let Some(relative) = identity
        .as_deref()
        .and_then(|path| Path::new(path).strip_prefix(root).ok())
    {
        *identity = Some(relative.to_string_lossy().replace('\\', "/"));
    }
}

/// Compile named browser documents through the canonical source graph.
///
/// # Panics
/// Panics if the compiler output cannot be serialized as JSON.
#[must_use]
#[allow(
    clippy::needless_pass_by_value,
    reason = "wasm-bindgen owns optional JS strings"
)]
#[wasm_bindgen::prelude::wasm_bindgen(js_name = compileProjectToSandboxBytecode)]
pub fn compile_project_to_sandbox_bytecode_js(request: &str, profile: Option<String>) -> String {
    match compile_project_to_sandbox_bytecode(request, profile.as_deref()) {
        Ok(output) => serde_json::to_string(&output).expect("project output is serializable"),
        Err(error) => serde_json::json!({ "error": error.message }).to_string(),
    }
}
