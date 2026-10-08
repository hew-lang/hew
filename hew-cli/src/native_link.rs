//! The files a program is built from and the native code it links.
//!
//! [`ProgramInputs::collect`] walks the type-checked program once: the entry
//! file, every imported module's source and the package that owns each of
//! them (the nearest directory at or above the source holding `hew.toml`).
//! [`link`] then builds each owning package's `[native]` section through
//! [`hew_pkg::native::build`], so a program that compiles a package links its
//! native code without the user passing `--link-lib`, however deep in the
//! import graph the package sits.
//!
//! A Rust `[native]` crate links only when it embeds a byte-identical `libstd`
//! to `libhew.a` (same pinned rustc) and is built `panic = "abort"`; otherwise
//! the final link fails with a duplicate `rust_eh_personality`. A package
//! enforces this with its own `rust-toolchain.toml` + `[profile]`, which
//! `cargo` honours because the build runs in the package's crate directory.

use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use hew_parser::ast::{Item, Program, Spanned};
use hew_pkg::manifest::{self, HewManifest};
use hew_pkg::project::{LOCK_FILE, MANIFEST_FILE};

use crate::target::TargetSpec;

/// A package whose sources the program compiles.
#[derive(Debug)]
pub struct InputPackage {
    /// Directory holding the package's `hew.toml`.
    pub root: PathBuf,
    pub manifest: HewManifest,
}

/// Every non-std source a program reads and the packages that own them.
#[derive(Debug, Default)]
pub struct ProgramInputs {
    /// Hew sources: the entry file first, then each imported module's files.
    pub sources: Vec<PathBuf>,
    /// Owning packages, each once, in first-reached order.
    pub packages: Vec<InputPackage>,
}

impl ProgramInputs {
    /// Collect the sources and owning packages of `program`, whose entry file
    /// is `entry`. Sources below a standard-library root's `std/` are the
    /// toolchain's, not the program's, and are left out.
    ///
    /// # Errors
    ///
    /// Returns the diagnostic when an owning package's `hew.toml` is malformed.
    pub fn collect(program: &Program, entry: &Path, std_roots: &[PathBuf]) -> Result<Self, String> {
        let std_dirs: Vec<PathBuf> = std_roots
            .iter()
            .filter_map(|root| root.join("std").canonicalize().ok())
            .collect();
        let mut walk = Walk {
            std_dirs,
            seen_sources: BTreeSet::new(),
            owners: BTreeMap::new(),
            inputs: Self::default(),
        };
        walk.add_source(entry)?;
        // Shared imports retain one immutable body per module, so the walk is
        // bounded by that body's identity. Recursing per import edge instead
        // expands a module once per path that reaches it, and a diamond import
        // graph then costs one filesystem probe per path rather than per module.
        let mut seen: HashSet<*const Vec<Spanned<Item>>> = HashSet::new();
        let mut queue: Vec<&[Spanned<Item>]> = vec![program.items.as_slice()];
        while let Some(items) = queue.pop() {
            for (item, _span) in items {
                let Item::Import(decl) = item else { continue };
                for source in decl
                    .resolved_source_paths
                    .iter()
                    .chain(&decl.resolved_item_source_paths)
                {
                    walk.add_source(source)?;
                }
                if let Some(resolved) = &decl.resolved_items {
                    if seen.insert(Arc::as_ptr(resolved)) {
                        queue.push(resolved.as_slice());
                    }
                }
            }
        }
        Ok(walk.inputs)
    }

    /// The packages that declare `[native]` code.
    pub fn native_packages(&self) -> impl Iterator<Item = &InputPackage> {
        self.packages
            .iter()
            .filter(|package| package.manifest.native.is_some())
    }

    /// Refuse a wasm build of a program that compiles a `[native]` package:
    /// native archives and objects cannot join a wasm module.
    ///
    /// # Errors
    ///
    /// Names the first package that declares `[native]`.
    pub fn refuse_native_for_wasm(&self) -> Result<(), String> {
        match self.native_packages().next() {
            Some(package) => Err(format!(
                "package `{}` declares [native] code, which cannot be linked into a wasm module",
                package.manifest.package.name
            )),
            None => Ok(()),
        }
    }

    /// Each owning package's manifest and, beside it, its lockfile.
    pub fn manifests(&self) -> impl Iterator<Item = PathBuf> + '_ {
        self.packages.iter().flat_map(|package| {
            let lock = package.root.join(LOCK_FILE);
            std::iter::once(package.root.join(MANIFEST_FILE)).chain(lock.is_file().then_some(lock))
        })
    }
}

struct Walk {
    std_dirs: Vec<PathBuf>,
    seen_sources: BTreeSet<PathBuf>,
    /// Owning package root of each directory already probed.
    owners: BTreeMap<PathBuf, Option<PathBuf>>,
    inputs: ProgramInputs,
}

impl Walk {
    fn add_source(&mut self, source: &Path) -> Result<(), String> {
        let Ok(canonical) = source.canonicalize() else {
            return Ok(());
        };
        if self.std_dirs.iter().any(|dir| canonical.starts_with(dir))
            || !self.seen_sources.insert(canonical.clone())
        {
            return Ok(());
        }
        self.inputs.sources.push(plain(canonical.clone()));
        let Some(dir) = canonical.parent() else {
            return Ok(());
        };
        let Some(root) = self.owner(dir) else {
            return Ok(());
        };
        let root = plain(root);
        if self
            .inputs
            .packages
            .iter()
            .any(|package| package.root == root)
        {
            return Ok(());
        }
        let manifest_path = root.join(MANIFEST_FILE);
        let manifest = manifest::parse_manifest(&manifest_path)
            .map_err(|error| format!("cannot load {}: {error}", manifest_path.display()))?;
        self.inputs.packages.push(InputPackage { root, manifest });
        Ok(())
    }

    /// The nearest directory at or above `dir` holding `hew.toml`.
    fn owner(&mut self, dir: &Path) -> Option<PathBuf> {
        if let Some(owner) = self.owners.get(dir) {
            return owner.clone();
        }
        let owner = if dir.join(MANIFEST_FILE).is_file() {
            Some(dir.to_path_buf())
        } else {
            dir.parent().and_then(|parent| self.owner(parent))
        };
        self.owners.insert(dir.to_path_buf(), owner.clone());
        owner
    }
}

/// `path` without Windows' extended-length `\\?\` prefix, which
/// `canonicalize` adds and which C drivers and Make do not expect. Canonical
/// paths are only the identity key here; tools receive this spelling.
fn plain(path: PathBuf) -> PathBuf {
    if cfg!(windows) {
        let text = path.to_string_lossy();
        if let Some(rest) = text.strip_prefix(r"\\?\UNC\") {
            return PathBuf::from(format!(r"\\{rest}"));
        }
        if let Some(rest) = text.strip_prefix(r"\\?\") {
            return PathBuf::from(rest);
        }
    }
    path
}

/// The native link inputs of a program, in link-line order.
#[derive(Debug, Default)]
pub struct NativeLink {
    /// Package archives and objects.
    inputs: Vec<String>,
    /// Library search paths, system libraries and the C++ runtime.
    link_args: Vec<String>,
    /// Native sources, headers and Rust sources the build read.
    pub dependencies: Vec<PathBuf>,
}

impl NativeLink {
    /// The libraries for the link line: package inputs, then the user's
    /// `--link-lib` entries, then the system libraries both may need.
    pub fn libraries(&self, user: &[String]) -> Vec<String> {
        self.inputs
            .iter()
            .chain(user)
            .chain(&self.link_args)
            .cloned()
            .collect()
    }
}

/// Build the `[native]` code of every package `inputs` owns.
///
/// # Errors
///
/// Returns the first package's build diagnostic.
pub fn link(
    inputs: &ProgramInputs,
    target: &TargetSpec,
    debug: bool,
    opt_level: hew_codegen_rs::OptLevel,
) -> Result<NativeLink, String> {
    let mut out = NativeLink::default();
    let mut packages = inputs.native_packages().peekable();
    if packages.peek().is_none() {
        return Ok(out);
    }
    let toolchain = crate::link::native_toolchain(target, debug, opt_level)?;
    let mut compiles_cxx = false;
    for package in packages {
        let Some(build) = hew_pkg::native::build(&package.root, &package.manifest, &toolchain)?
        else {
            continue;
        };
        for input in &build.inputs {
            out.inputs.push(
                input
                    .to_str()
                    .ok_or_else(|| {
                        format!("native input path is not valid UTF-8: {}", input.display())
                    })?
                    .to_string(),
            );
        }
        // Kept verbatim: pkg-config pairs tokens (`-framework Cocoa`), so
        // dropping a repeated one would split a pair.
        out.link_args.extend(build.link_args);
        compiles_cxx |= build.compiles_cxx;
        out.dependencies.extend(build.dependencies);
    }
    if compiles_cxx {
        out.link_args.extend(
            target
                .native_link_plan()
                .cxx_runtime_libs
                .iter()
                .map(|lib| (*lib).to_string()),
        );
    }
    Ok(out)
}
