//! Package-manager command surface, flattened into the `hew` CLI as native
//! top-level subcommands (`hew add`, `hew install`, `hew publish`, …).

use std::collections::{BTreeMap, BTreeSet};
use std::io::Read as _;
use std::path::{Path, PathBuf};

use clap::Subcommand;

use crate::package_name::{
    invalid_message as invalid_package_name_message, is_valid as is_valid_package_name,
};
use crate::{
    checksum, client, config, credentials, index, lockfile, manifest, package_fs, publish,
    registry, resolver, signing, tarball,
};

/// Package-manager subcommands, flattened into the `hew` CLI's top-level
/// command enum so `hew add`, `hew install`, … parse (and complete) natively.
#[derive(Debug, Subcommand)]
pub enum PkgCommand {
    /// Add a dependency to hew.toml
    // `version` here is the dependency requirement; suppress the auto
    // `--version` flag that `propagate_version` on the `hew` CLI would add.
    #[command(disable_version_flag = true)]
    Add {
        /// Package name
        package: String,
        /// Version requirement
        #[arg(long, default_value = "*")]
        version: String,
        /// Use a named registry from config
        #[arg(long, short = 'r')]
        registry: Option<String>,
        /// Add a dependency from a local directory
        #[arg(long, value_name = "DIR", conflicts_with = "registry")]
        path: Option<PathBuf>,
        /// Print the manifest change without writing hew.toml
        #[arg(long)]
        dry_run: bool,
    },
    /// Install dependencies into .hew/packages/
    Install {
        /// Require an up-to-date lock file
        #[arg(long)]
        locked: bool,
        /// Use a named registry from config
        #[arg(long, short = 'r')]
        registry: Option<String>,
        /// Resolve exclusively from the local package cache
        #[arg(long, conflicts_with = "registry")]
        offline: bool,
    },
    /// Publish this package, or every workspace member, to the registry
    Publish {
        /// Use a named registry from config
        #[arg(long, short = 'r')]
        registry: Option<String>,
        /// Install into the local package registry only; never contacts a
        /// remote registry
        #[arg(long, conflicts_with = "registry")]
        local: bool,
        /// Build and validate the archive without uploading it
        #[arg(long)]
        dry_run: bool,
        /// Publish every member of the enclosing workspace, in dependency
        /// order
        #[arg(long)]
        workspace: bool,
        /// Where upload credentials come from
        #[arg(long, value_enum, default_value_t = PublishAuth::Credentials, conflicts_with = "local")]
        auth: PublishAuth,
    },
    /// Build or publish release-qualified native package archives
    Native {
        #[command(subcommand)]
        action: NativeAction,
    },
    /// List packages in the local registry
    List,
    /// Search packages in the registry
    Search {
        /// Search query
        query: String,
        /// Filter by category
        #[arg(long)]
        category: Option<String>,
        /// Page number (1-based)
        #[arg(long, default_value = "1")]
        page: u32,
        /// Results per page
        #[arg(long, default_value = "20")]
        per_page: u32,
        /// Use a named registry from config
        #[arg(long, short = 'r')]
        registry: Option<String>,
    },
    /// Show package info
    Info {
        /// Package name
        package: String,
        /// Use a named registry from config
        #[arg(long, short = 'r')]
        registry: Option<String>,
    },
    /// Show dependency tree
    Tree,
    /// Update dependencies
    Update {
        /// Package to update (all if omitted)
        package: Option<String>,
    },
    /// Remove a dependency
    Remove {
        /// Package name
        package: String,
    },
    /// Show outdated dependencies
    Outdated,
    /// Log in to the registry via GitHub
    Login,
    /// Log out from the registry
    Logout,
    /// Manage signing keys
    Key {
        #[command(subcommand)]
        action: KeyAction,
    },
    /// Manage namespace ownership
    Namespace {
        #[command(subcommand)]
        action: NamespaceAction,
    },
    /// Yank a published version
    // The positional `version` is the version to yank; suppress the auto
    // `--version` flag that `propagate_version` on the `hew` CLI would add.
    #[command(disable_version_flag = true)]
    Yank {
        /// Version to yank
        version: String,
        /// Reason for yanking
        #[arg(long)]
        reason: Option<String>,
        /// Undo a previous yank
        #[arg(long)]
        undo: bool,
    },
    /// Deprecate a package
    Deprecate {
        /// Package to deprecate (defaults to current project)
        package: Option<String>,
        /// Deprecation message
        #[arg(long)]
        message: Option<String>,
        /// Suggested replacement package
        #[arg(long)]
        successor: Option<String>,
        /// Undo deprecation
        #[arg(long)]
        undo: bool,
    },
    /// Manage local package index
    Index {
        #[command(subcommand)]
        action: IndexAction,
    },
}

/// `hew publish --auth`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
pub enum PublishAuth {
    /// The token from `hew login` and the key from `hew key generate`.
    Credentials,
    /// Trusted publishing: exchange the workflow's OIDC token for a grant
    /// bound to a key that exists only in this process.
    GithubActions,
}

fn init_follow_up_hint(template: manifest::ManifestTemplate, package_name: &str) -> String {
    let source = template.scaffold_source(package_name);
    match template {
        manifest::ManifestTemplate::Lib => format!("Next: `hew check {source}`"),
        manifest::ManifestTemplate::Bin | manifest::ManifestTemplate::Actor => {
            format!("Next: `hew check {source}` then `hew run {source}`")
        }
    }
}

#[derive(Debug, Subcommand)]
pub enum IndexAction {
    /// Sync the local package index from the registry
    Sync,
    /// Resolve a package version from the local index
    // `--version` here is the requirement to resolve; suppress the auto
    // `--version` flag that `propagate_version` on the `hew` CLI would add.
    #[command(disable_version_flag = true)]
    Resolve {
        /// Package name
        package: String,
        /// Version requirement
        #[arg(long, default_value = "*")]
        version: String,
    },
    /// List all versions of a package in the local index
    List {
        /// Package name
        package: String,
    },
}

#[derive(Debug, Subcommand)]
pub enum KeyAction {
    /// Generate a new Ed25519 signing keypair
    Generate,
    /// Register the existing local signing key with the registry
    Register {
        /// Use a named registry from config
        #[arg(long, short = 'r')]
        registry: Option<String>,
    },
    /// List registered signing keys
    List,
    /// Look up a signing key by fingerprint
    Info {
        /// Key fingerprint (e.g. SHA256:...)
        fingerprint: String,
    },
    /// Show the registry's public signing key
    Registry,
}

#[derive(Debug, Subcommand)]
pub enum NamespaceAction {
    /// Register a custom namespace prefix
    Register {
        /// Namespace prefix to claim
        prefix: String,
    },
    /// Show info about a namespace
    Info {
        /// Namespace prefix to look up
        prefix: String,
    },
}

#[derive(Debug, Subcommand)]
pub enum NativeAction {
    /// Build the package's Rust static library for publication
    Build {
        /// Native target triple (defaults to the compiler host)
        #[arg(long)]
        target: Option<String>,
        /// Destination .tar.zst archive
        #[arg(long)]
        output: PathBuf,
    },
    /// Publish an archive beside its already published source package
    Publish {
        archive: PathBuf,
        #[arg(long, short = 'r')]
        registry: Option<String>,
    },
}

/// Execute a package-manager subcommand dispatched from the `hew` CLI.
/// `native_target` is the canonical target selected by the compiler driver.
#[expect(
    clippy::too_many_lines,
    reason = "one match arm per subcommand reads clearest in one place"
)]
pub fn dispatch(
    cmd: &PkgCommand,
    native_target: &str,
    check_generation: impl Fn(&Path) -> Result<(), String>,
) {
    let cfg = load_config_or_exit();
    let registry = registry::Registry::with_root(config::registry_path(&cfg));

    match cmd {
        PkgCommand::Native { action } => {
            if let Err(error) = cmd_native(action, native_target, &check_generation) {
                eprintln!("hew native: {error}");
                std::process::exit(1);
            }
        }
        PkgCommand::Add {
            package,
            version,
            registry: reg,
            path,
            dry_run,
        } => cmd_add(package, version, reg.as_deref(), path.as_deref(), *dry_run),
        PkgCommand::Install {
            locked,
            registry: reg,
            offline,
        } => cmd_install(
            *locked,
            *offline,
            &registry,
            reg.as_deref(),
            &cfg,
            native_target,
        ),
        PkgCommand::Publish {
            registry: reg,
            local,
            dry_run,
            workspace,
            auth,
        } => cmd_publish(
            &registry,
            reg.as_deref(),
            PublishFlags {
                local: *local,
                dry_run: *dry_run,
                workspace: *workspace,
                auth: *auth,
            },
            &check_generation,
        ),
        PkgCommand::List => cmd_list(&registry),
        PkgCommand::Search {
            query,
            category,
            page,
            per_page,
            registry: reg,
        } => cmd_search(
            query,
            category.as_deref(),
            *page,
            *per_page,
            &registry,
            reg.as_deref(),
        ),
        PkgCommand::Info {
            package,
            registry: reg,
        } => cmd_info(package, &registry, reg.as_deref()),
        PkgCommand::Tree => cmd_tree(&registry),
        PkgCommand::Update { package } => cmd_update(package.as_deref(), &registry),
        PkgCommand::Remove { package } => cmd_remove(package),
        PkgCommand::Outdated => cmd_outdated(&registry),
        PkgCommand::Login => cmd_login(),
        PkgCommand::Logout => cmd_logout(),
        PkgCommand::Key { action } => match action {
            KeyAction::Generate => cmd_key_generate(),
            KeyAction::Register { registry: reg } => cmd_key_register(reg.as_deref()),
            KeyAction::List => cmd_key_list(),
            KeyAction::Info { fingerprint } => cmd_key_info(fingerprint),
            KeyAction::Registry => cmd_registry_key(),
        },
        PkgCommand::Namespace { action } => match action {
            NamespaceAction::Register { prefix } => cmd_namespace_register(prefix),
            NamespaceAction::Info { prefix } => cmd_namespace_info(prefix),
        },
        PkgCommand::Yank {
            version,
            reason,
            undo,
        } => cmd_yank(version, reason.as_deref(), *undo),
        PkgCommand::Deprecate {
            package,
            message,
            successor,
            undo,
        } => cmd_deprecate(
            package.as_deref(),
            message.as_deref(),
            successor.as_deref(),
            *undo,
        ),
        PkgCommand::Index { action } => match action {
            IndexAction::Sync => cmd_index_sync(),
            IndexAction::Resolve { package, version } => {
                cmd_index_resolve(package, version);
            }
            IndexAction::List { package } => cmd_index_list(package),
        },
    }
}

fn cmd_native(
    action: &NativeAction,
    target: &str,
    check_generation: &impl Fn(&Path) -> Result<(), String>,
) -> Result<(), String> {
    let root = std::env::current_dir().map_err(|error| error.to_string())?;
    match action {
        NativeAction::Build { output, .. } => {
            check_generation(&root)?;
            crate::native_prebuilt::NativeIdentity::current(target).route()?;
            let archive = crate::native_prebuilt::build(&root, target)?;
            if let Some(parent) = output.parent().filter(|path| !path.as_os_str().is_empty()) {
                std::fs::create_dir_all(parent).map_err(|error| error.to_string())?;
            }
            crate::atomic_fs::write_atomic(output, &archive.data, 0o644)
                .map_err(|error| error.to_string())?;
            println!("Native archive: {}", output.display());
            println!("Checksum: {}", archive.checksum);
        }
        NativeAction::Publish { archive, registry } => {
            let bytes = std::fs::read(archive).map_err(|error| error.to_string())?;
            let descriptor = crate::native_prebuilt::inspect(&bytes, &root.join("target"))?;
            let keypair = signing::load_keypair(&signing::default_key_dir())
                .map_err(|error| error.to_string())?;
            let token =
                resolve_registry_token(&credentials::credentials_path(), registry.as_deref())
                    .map_err(|error| error.to_string())?;
            let client = make_client(registry.as_deref()).with_token(token);
            let checksum = crate::native_prebuilt::publish(
                &client,
                &bytes,
                &descriptor,
                &keypair,
                &root.join("target"),
            )?;
            println!(
                "Published native {}@{} for {}",
                descriptor.name, descriptor.version, descriptor.identity.target
            );
            println!("Checksum: {checksum}");
        }
    }
    Ok(())
}

/// Validate the manifest of the package rooted at `root`, resolving every
/// declared dependency against the configured registry. Returns the list of
/// problems found; an empty list means the manifest is valid.
///
/// This is the manifest half of `hew check` in package mode — the CLI runs it
/// before type-checking the package's entry point.
///
/// # Errors
///
/// Returns a rendered message when the manifest cannot be read or parsed.
pub fn manifest_issues(root: &Path) -> Result<Vec<String>, String> {
    let cfg = load_config_or_exit();
    let registry = registry::Registry::with_root(config::registry_path(&cfg));
    let manifest_path = root.join(crate::project::MANIFEST_FILE);
    let m = manifest::parse_manifest(&manifest_path)
        .map_err(|e| format!("cannot load {}: {e}", manifest_path.display()))?;
    let sources = workspace_sources(resolver::RegistrySources::default_source(), root)?;
    Ok(collect_manifest_issues(&m, root, &registry, &sources))
}

/// Scaffold a manifest-first Hew project in `dir` — the `hew init` back end.
/// The package name is the (canonicalized) directory name with hyphens
/// normalized to underscores.
///
/// Never destructive: errors if `hew.toml` already exists, skips any scaffold
/// source file that already exists, and merges (never replaces) `.gitignore`.
pub fn run_init(dir: &Path, template: manifest::ManifestTemplate) {
    let cfg = load_config_or_exit();
    cmd_init(dir, template, &cfg);
}

/// Look up a named registry in config, exiting with the standard
/// unknown-registry diagnostic if `name` is not configured.
///
/// Shared by [`make_client`] and the publish token-resolution path so a
/// typo'd `--registry NAME` always gets this precise message rather than
/// being misreported by whatever check happens to run first.
fn resolve_named_registry_or_exit(name: &str) -> config::RemoteRegistry {
    let cfg = load_config_or_exit();
    config::get_named_registry(&cfg, name).unwrap_or_else(|| {
        eprintln!("hew: unknown registry '{name}'");
        eprintln!("Configure it in ~/.hew/config.toml under [registries.{name}]");
        std::process::exit(1);
    })
}

/// Build a [`RegistryClient`] for the given named registry (or default).
fn make_client(registry_name: Option<&str>) -> client::RegistryClient {
    match registry_name {
        Some(name) => {
            let remote = resolve_named_registry_or_exit(name);
            let mut c = client::RegistryClient::with_url(remote.api);
            if let Some(mode) = remote.wire_names {
                c = c.with_wire_names(mode);
            }
            if let Some(fallback) = remote.fallback_api {
                c = if let Some(mode) = remote.fallback_wire_names {
                    c.with_fallback_wire_names(fallback, mode)
                } else {
                    c.with_fallback(fallback)
                };
            }
            let cred_path = credentials::credentials_path();
            if let Ok(token) = credentials::get_named_token(&cred_path, name) {
                c = c.with_token(token);
            }
            c
        }

        None => client::RegistryClient::new(),
    }
}

fn registry_sources_or_exit(
    cfg: &config::PkgConfig,
    registry_name: Option<&str>,
) -> resolver::RegistrySources {
    let (default_registry, default_selector) = match registry_name {
        Some(name) => {
            let remote = config::get_named_registry(cfg, name).unwrap_or_else(|| {
                eprintln!("hew: unknown registry '{name}'");
                eprintln!("Configure it in ~/.hew/config.toml under [registries.{name}]");
                std::process::exit(1);
            });
            (
                config::registry_identity(&remote.api),
                Some(name.to_string()),
            )
        }
        None => (config::default_registry_identity(), None),
    };
    resolver::RegistrySources::new(
        default_registry,
        default_selector,
        config::named_registry_identities(cfg),
    )
}

/// Let `sources` resolve the members of the workspace enclosing the package
/// at `package_root`, if there is one, from their directories.
///
/// # Errors
///
/// Returns the rendered workspace error when that workspace cannot be loaded.
fn workspace_sources(
    sources: resolver::RegistrySources,
    package_root: &Path,
) -> Result<resolver::RegistrySources, String> {
    Ok(
        match crate::workspace::find_enclosing(package_root).map_err(|e| e.to_string())? {
            Some(workspace) => sources.with_workspace(&workspace),
            None => sources,
        },
    )
}

/// Resolve the API token required for an authenticated registry call.
///
/// Injecting `cred_path` keeps this unit-testable without `$HOME` mutation.
///
/// # Errors
///
/// Returns [`credentials::CredentialError::NotLoggedIn`] when no token is
/// stored for the target registry (default or named), or
/// [`credentials::CredentialError::Parse`]/[`credentials::CredentialError::Io`]
/// when the credentials file exists but cannot be read.
fn resolve_registry_token(
    cred_path: &Path,
    registry_name: Option<&str>,
) -> Result<String, credentials::CredentialError> {
    match registry_name {
        Some(name) => credentials::get_named_token(cred_path, name),
        None => credentials::get_token(cred_path),
    }
}

fn load_config_or_exit() -> config::PkgConfig {
    config::load_config().unwrap_or_else(|error| {
        eprintln!("hew: {error}");
        std::process::exit(1);
    })
}

fn cmd_init(dir: &Path, template: manifest::ManifestTemplate, cfg: &config::PkgConfig) {
    let directory_name = dir
        .canonicalize()
        .ok()
        .as_deref()
        .and_then(Path::file_name)
        .or_else(|| dir.file_name())
        .and_then(|n| n.to_str())
        .unwrap_or("myproject")
        .to_string();
    let name = directory_name.replace('-', "_");
    let manifest_path = dir.join("hew.toml");
    if manifest_path.exists() {
        eprintln!("hew init: hew.toml already exists");
        std::process::exit(1);
    }
    match manifest::write_manifest_with_template(&manifest_path, &name, template) {
        Ok(()) => {
            // Apply config defaults (author, license) if available.
            if let Ok(mut m) = manifest::parse_manifest(&manifest_path) {
                let mut changed = false;
                if let Some(defaults) = &cfg.defaults {
                    if let Some(author) = &defaults.author {
                        m.package.authors = Some(vec![author.clone()]);
                        changed = true;
                    }
                    if let Some(license) = &defaults.license {
                        m.package.license = Some(license.clone());
                        changed = true;
                    }
                }
                if changed {
                    if let Err(e) = manifest::save_manifest(&manifest_path, &m) {
                        eprintln!("hew init: warning: could not apply defaults: {e}");
                    }
                }
            }

            // Write template source file (skip-if-exists).
            write_template_source(dir, &name, template);

            // Write .gitignore with target/ and .hew/ (merged, never replaced).
            write_init_gitignore(dir);
            let source = template.scaffold_source(&name);
            let follow_up = init_follow_up_hint(template, &name);

            // Parse back to verify and display the confirmed package name.
            match manifest::parse_manifest(&manifest_path) {
                Ok(m) => println!("Created hew.toml: {}", m.summary()),
                Err(e) => {
                    eprintln!("hew init: warning: created manifest may be invalid: {e}");
                    println!("Created hew.toml for project `{name}`");
                }
            }
            println!("Scaffolded {source} and .gitignore");
            println!("{follow_up}");
        }
        Err(e) => {
            eprintln!("hew init: {e}");
            std::process::exit(1);
        }
    }
}

/// Write the template source file for `hew init`.
fn write_template_source(dir: &Path, name: &str, template: manifest::ManifestTemplate) {
    let (filename, content) = match template {
        manifest::ManifestTemplate::Lib => (
            // A package name is a module path. Its final segment names the
            // root module, a single file beside `hew.toml` (`hew.selfqualtype`
            // installs at `hew/selfqualtype/selfqualtype.hew`). Scaffold that
            // exact root so init, install, and import share one convention.
            manifest::package_root_source(name),
            format!("// {name} library\n\npub fn add(a: i32, b: i32) -> i32 {{\n    a + b\n}}\n"),
        ),
        manifest::ManifestTemplate::Actor => (
            "main.hew".to_string(),
            "actor Counter {\n    var count: i32;\n\n    receive fn increment() {\n        count = count + 1;\n        println(count);\n    }\n}\n\nfn main() {\n    let c = spawn Counter { count: 0 };\n    let _ = c.increment();\n    let _ = c.increment();\n    let _ = c.increment();\n}\n"
                .to_string(),
        ),
        manifest::ManifestTemplate::Bin => (
            "main.hew".to_string(),
            format!("fn main() {{\n    println(\"Hello from {name}!\");\n}}\n"),
        ),
    };
    let path = dir.join(&filename);
    if !path.exists() {
        if let Err(e) = std::fs::write(&path, content) {
            eprintln!("warning: could not create {filename}: {e}");
        }
    }
    write_template_test(dir, &filename);
}

/// Write a starter `<stem>_test.hew` beside the scaffolded source file so
/// `hew test` is green immediately after `hew new`/`hew init`, skip-if-exists
/// like every other scaffold file.
///
/// Self-contained (asserts an arithmetic fact rather than calling into the
/// sibling source file): the discovered test file is compiled as its own
/// isolated program (`compile_test`, `hew-cli/src/test_runner/runner.rs`),
/// and peer compilation — a test file calling functions declared in
/// `main.hew` — is D17, tracked separately (V060-FD-6).
fn write_template_test(dir: &Path, source_filename: &str) {
    let Some(stem) = Path::new(source_filename)
        .file_stem()
        .and_then(std::ffi::OsStr::to_str)
    else {
        return;
    };
    let test_filename = format!("{stem}_test.hew");
    let test_path = dir.join(&test_filename);
    if test_path.exists() {
        return;
    }
    let content = "#[test]\nfn scaffold_test() {\n    assert(1 + 1 == 2);\n}\n";
    if let Err(e) = std::fs::write(&test_path, content) {
        eprintln!("warning: could not create {test_filename}: {e}");
    }
}

/// Write a `.gitignore` with `target/` and `.hew/` entries.
fn write_init_gitignore(dir: &Path) {
    ensure_gitignore_entry(dir, ".hew/");
    ensure_gitignore_entry(dir, "target/");
}

/// Append `entry` to `.gitignore` if not already present.
///
/// Never destructive: a `.gitignore` that exists but cannot be read as UTF-8
/// (or fails to read for any reason other than being absent) is left
/// untouched rather than overwritten. Only a genuinely missing file is
/// created from scratch.
fn ensure_gitignore_entry(dir: &Path, entry: &str) {
    let path = dir.join(".gitignore");
    match std::fs::read_to_string(&path) {
        Ok(contents) => {
            if contents.lines().any(|l| l.trim() == entry) {
                return;
            }
            let updated = if contents.ends_with('\n') {
                format!("{contents}{entry}\n")
            } else if contents.is_empty() {
                format!("{entry}\n")
            } else {
                format!("{contents}\n{entry}\n")
            };
            let _ = std::fs::write(&path, updated);
        }
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            let _ = std::fs::write(&path, format!("{entry}\n"));
        }
        Err(e) => {
            eprintln!(
                "warning: could not update {}: {e} (leaving the file unchanged)",
                path.display()
            );
        }
    }
}

fn cmd_add(
    pkg: &str,
    version: &str,
    registry_name: Option<&str>,
    dependency_path: Option<&Path>,
    dry_run: bool,
) {
    if !is_valid_package_name(pkg) {
        eprintln!("hew add: {}", invalid_package_name_message(pkg));
        std::process::exit(1);
    }

    let cwd = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    let manifest_path = cwd.join("hew.toml");
    if !manifest_path.exists() {
        eprintln!("hew add: no hew.toml found in current directory");
        eprintln!("Run `hew init` to create one.");
        std::process::exit(1);
    }

    // Local-path dependencies never contact a registry: the target's own
    // hew.toml is the only source of truth for its package name.
    if let Some(path) = dependency_path {
        let resolved = cwd.join(path);
        let dependency_manifest = resolved.join("hew.toml");
        match manifest::parse_manifest(&dependency_manifest) {
            Ok(dependency) if dependency.package.name == pkg => {
                if dry_run {
                    println!("Would add {pkg} from {} to hew.toml", path.display());
                    return;
                }
                match manifest::add_path_dependency(&manifest_path, pkg, version, path) {
                    Ok(()) => println!("Added {pkg} from {} to hew.toml", path.display()),
                    Err(e) => {
                        eprintln!("hew add: {e}");
                        std::process::exit(1);
                    }
                }
            }
            Ok(dependency) => {
                eprintln!(
                    "hew add: path dependency `{pkg}` at `{}` declares package name `{}`",
                    resolved.display(),
                    dependency.package.name
                );
                std::process::exit(1);
            }
            Err(error) => {
                eprintln!(
                    "hew add: cannot load path dependency `{pkg}` at `{}`: {error}",
                    resolved.display()
                );
                std::process::exit(1);
            }
        }
        return;
    }

    // Registry dependency: refuse an unknown package (or an unreachable
    // registry — fail closed rather than write a dependency nothing can
    // ever resolve) before touching hew.toml.
    let api_client = make_client(registry_name);
    match api_client.get_package(pkg) {
        Ok(entries) if !entries.is_empty() => {}
        Ok(_) => {
            eprintln!("hew add: package `{pkg}` not found in registry");
            std::process::exit(1);
        }
        Err(e) => {
            eprintln!("hew add: {e}");
            std::process::exit(1);
        }
    }

    if dry_run {
        println!("Would add {pkg}@{version} to hew.toml");
        return;
    }

    match manifest::add_dependency(&manifest_path, pkg, version, registry_name) {
        Ok(()) => println!("Added {pkg}@{version} to hew.toml"),
        Err(e) => {
            eprintln!("hew add: {e}");
            std::process::exit(1);
        }
    }
}

fn cmd_install(
    locked: bool,
    offline: bool,
    registry: &registry::Registry,
    registry_name: Option<&str>,
    cfg: &config::PkgConfig,
    native_target: &str,
) {
    let cwd = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    let manifest_path = cwd.join("hew.toml");
    if !manifest_path.exists() {
        eprintln!("hew install: no hew.toml found in current directory");
        std::process::exit(1);
    }
    let m = match manifest::parse_manifest(&manifest_path) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("hew install: {e}");
            std::process::exit(1);
        }
    };
    let registry_sources = workspace_sources(registry_sources_or_exit(cfg, registry_name), &cwd)
        .unwrap_or_else(|error| {
            eprintln!("hew install: {error}");
            std::process::exit(1);
        });
    let mode = InstallMode {
        locked,
        offline,
        write_lock: true,
    };
    if let Err(error) = install_dependencies(
        &cwd,
        &m,
        registry,
        &registry_sources,
        registry_name,
        mode,
        Some(native_target),
    ) {
        eprintln!("hew install: {error}");
        std::process::exit(1);
    }
}

/// How [`install_dependencies`] resolves and what it writes.
#[derive(Debug, Clone, Copy)]
pub(crate) struct InstallMode {
    /// Require and follow an up-to-date `hew.lock`.
    pub locked: bool,
    /// Resolve exclusively from the local package cache.
    pub offline: bool,
    /// Write `hew.lock` and add `.hew/` to `.gitignore`. Publish installs
    /// only into `.hew/packages`, so the packed tree stays as authored.
    pub write_lock: bool,
}

/// Resolve the dependencies of the package `m` rooted at `root` and
/// materialize them in its `.hew/packages`. Workspace members in `sources`
/// link from their directories; registry packages are fetched and verified.
/// With a `native_target`, registry packages with a `[native]` Rust crate
/// also select their signed prebuilt for that target and pin it in
/// `hew.lock`; publish passes `None` because it never links native code.
/// Returns canonical installed roots mapped to their resolved local sources,
/// including copies on platforms that do not materialize packages as links.
///
/// # Errors
///
/// Returns the rendered diagnostic of the first step that fails.
#[expect(
    clippy::too_many_lines,
    reason = "install has many sequential steps that are clearest in one function"
)]
pub(crate) fn install_dependencies(
    root: &Path,
    m: &manifest::HewManifest,
    registry: &registry::Registry,
    registry_sources: &resolver::RegistrySources,
    registry_name: Option<&str>,
    mode: InstallMode,
    native_target: Option<&str>,
) -> Result<BTreeMap<PathBuf, PathBuf>, String> {
    let lock_path = root.join(crate::project::LOCK_FILE);
    // Native pins carry forward only where this install selects prebuilts.
    let previous_lockfile = native_target
        .and_then(|_| lockfile::read_lockfile(&lock_path).ok())
        .filter(|lockfile| lockfile::validate_lockfile(lockfile).is_ok());

    let (locked_lockfile, pinned_locked_registry) = if mode.locked {
        // --locked: read existing lock and validate against manifest.
        let lf = lockfile::read_lockfile(&lock_path).map_err(|error| match error {
            lockfile::LockError::Missing => {
                "--locked requires an hew.lock file, but none was found".to_string()
            }
            error => error.to_string(),
        })?;
        lockfile::validate_lockfile(&lf).map_err(|error| error.to_string())?;
        if lockfile::is_lock_stale_for_registries(
            &lf,
            m,
            registry_sources.default_registry(),
            registry_sources.named(),
            &registry_sources.workspace_lock_paths(root),
        ) {
            return Err(format!(
                "{}\nRun `hew install` without --locked to update it.",
                lockfile::LockError::Stale
            ));
        }
        let pinned = verify_locked_registry_packages(&lf, registry)?;
        (Some(lf), pinned)
    } else {
        (None, BTreeMap::new())
    };

    if m.dependencies.is_empty() && locked_lockfile.is_none() {
        // Write an empty lockfile for consistency.
        let lf = lockfile::LockFile {
            packages: Vec::new(),
        };
        if mode.write_lock {
            if let Err(e) = lockfile::write_lockfile(&lock_path, &lf) {
                eprintln!("hew install: cannot write hew.lock: {e}");
            }
            println!("Nothing to install.");
        }
        return Ok(BTreeMap::new());
    }

    if mode.offline {
        eprintln!(
            "Offline mode: using cached packages from {}; the registry will not be contacted.",
            registry.root().display()
        );
    }

    // Locked resolution uses only the immutable paths verified above. Online
    // resolution admits only versions confirmed by this registry session;
    // offline resolution explicitly admits the local cache.
    let pinned_locked_paths = pinned_locked_registry
        .iter()
        .map(|(identity, package)| (identity.clone(), package.path().to_path_buf()))
        .collect();
    let offline_registry = if mode.offline && locked_lockfile.is_none() {
        pin_offline_registry_packages(registry, registry_sources, registry_name.is_none())?
    } else {
        BTreeMap::new()
    };
    let offline_paths = pinned_registry_paths(&offline_registry);
    let mut confirmed_registry =
        BTreeMap::<(String, String, String), VerifiedRegistryPackage>::new();
    let mut fetch_progress = FetchProgress::default();
    let resolved = loop {
        let result = if locked_lockfile.is_some() {
            resolver::resolve_all_pinned_with_sources(
                m,
                root,
                registry,
                registry_sources,
                &pinned_locked_paths,
            )
        } else if mode.offline {
            resolver::resolve_all_pinned_with_sources(
                m,
                root,
                registry,
                registry_sources,
                &offline_paths,
            )
        } else {
            let pinned_online_paths = pinned_registry_paths(&confirmed_registry);
            resolver::resolve_all_pinned_with_sources(
                m,
                root,
                registry,
                registry_sources,
                &pinned_online_paths,
            )
        };
        match result {
            Ok(resolved) => break resolved,
            Err(resolver::ResolveError::UnresolvableDeps { failures })
                if mode.offline || locked_lockfile.is_some() =>
            {
                use std::fmt::Write as _;
                let mut message = if mode.locked {
                    "locked graph could not resolve:".to_string()
                } else {
                    "offline cache could not resolve:".to_string()
                };
                for failure in failures {
                    let _ = write!(
                        message,
                        "\n  {} [{}] in {}",
                        failure.package,
                        failure.requirements.join(", "),
                        registry.root().display()
                    );
                }
                return Err(message);
            }
            Err(resolver::ResolveError::UnresolvableDeps { failures }) => {
                fetch_progress.begin_round(&failures)?;
                let confirmed_before = confirmed_registry.len();
                fetch_missing_packages(&failures, registry, &mut confirmed_registry)?;
                if confirmed_registry.len() <= confirmed_before {
                    return Err("registry fetch completed without making progress".to_string());
                }
            }
            Err(e) => return Err(e.to_string()),
        }
    };

    if let Some(lockfile) = &locked_lockfile {
        validate_locked_resolution(lockfile, &resolved)?;
    }
    let local_packages = project_packages_path(root);
    std::fs::create_dir_all(&local_packages)
        .map_err(|e| format!("cannot create .hew/packages/: {e}"))?;
    if mode.write_lock {
        ensure_gitignore_entry(root, ".hew/");
    }

    let mut local_sources = BTreeMap::new();

    // Build lockfile entries from resolved versions.
    let mut lock_packages: Vec<lockfile::LockedPackage> = Vec::new();

    for (name, package) in &resolved {
        if !is_valid_package_name(name) {
            return Err(invalid_package_name_message(name));
        }
        let version = &package.version;
        let (target, source, locked_path, locked_registry, pkg_checksum) = match &package.source {
            resolver::PackageSource::Registry {
                registry: registry_id,
            } => {
                if locked_lockfile.is_some() {
                    let pinned = pinned_locked_registry
                        .get(&(registry_id.clone(), name.clone(), version.clone()))
                        .expect("locked registry resolution must retain its verified path");
                    (
                        pinned.path().to_path_buf(),
                        "registry",
                        None,
                        Some(registry_id.clone()),
                        Some(pinned.checksum.clone()),
                    )
                } else if mode.offline {
                    let pinned = offline_registry
                        .get(&(registry_id.clone(), name.clone(), version.clone()))
                        .expect("offline registry resolution must retain its pinned path");
                    (
                        pinned.path().to_path_buf(),
                        "registry",
                        None,
                        Some(registry_id.clone()),
                        Some(pinned.checksum.clone()),
                    )
                } else {
                    let pinned = confirmed_registry
                        .get(&(registry_id.clone(), name.clone(), version.clone()))
                        .expect("online registry resolution must retain its verified path");
                    (
                        pinned.path().to_path_buf(),
                        "registry",
                        None,
                        Some(registry_id.clone()),
                        Some(pinned.checksum.clone()),
                    )
                }
            }
            resolver::PackageSource::Path(path) => (
                path.clone(),
                "path",
                Some(
                    package
                        .direct_path
                        .clone()
                        .unwrap_or_else(|| path.to_string_lossy().into_owned()),
                ),
                None,
                None,
            ),
        };

        let mut native = previous_lockfile
            .as_ref()
            .and_then(|lockfile| {
                lockfile.packages.iter().find(|entry| {
                    entry.name == *name
                        && entry.version == *version
                        && entry.registry == locked_registry
                        && entry.checksum == pkg_checksum
                        && entry.source == source
                })
            })
            .map(|entry| entry.native.clone())
            .unwrap_or_default();
        if let Some(native_target) = native_target.filter(|_| source == "registry" && !mode.offline)
        {
            let manifest = manifest::parse_manifest(&target.join("hew.toml"))
                .map_err(|error| error.to_string())?;
            if manifest
                .native
                .as_ref()
                .and_then(manifest::NativeLib::rust_crate)
                .is_some()
            {
                let artifact = crate::native_prebuilt::install(&target, &manifest, native_target)?;
                let identity = crate::native_prebuilt::NativeIdentity::current(native_target);
                let selected = artifact
                    .as_ref()
                    .map(crate::native_prebuilt::PrebuiltArtifact::lock_pin);
                let previous = native.iter().find(|pin| pin.identity == identity);
                if mode.locked && previous != selected.as_ref() {
                    return Err(
                        "native artifact does not match hew.lock; run without --locked to update it"
                            .to_string(),
                    );
                }
                native.retain(|pin| pin.identity != identity);
                native.extend(selected);
                native.sort_by_key(|pin| pin.identity.route().expect("validated native identity"));
            }
        }

        lock_packages.push(lockfile::LockedPackage {
            name: name.clone(),
            requirement: package.direct_requirement.clone(),
            version: version.clone(),
            checksum: pkg_checksum.clone(),
            signature: None,
            native,
            source: source.to_string(),
            registry: locked_registry,
            path: locked_path,
        });

        // Build the local symlink path: replace :: with / separators.
        let link = crate::paths::package_link(&local_packages, name)?;
        if let Some(parent) = link.parent() {
            std::fs::create_dir_all(parent)
                .map_err(|e| format!("cannot create {}: {e}", parent.display()))?;
        }
        let materialization = match &package.source {
            resolver::PackageSource::Registry { .. } => replace_registry_package_copy_verified(
                &link,
                &target,
                pkg_checksum
                    .as_deref()
                    .expect("verified registry package has a checksum"),
            ),
            resolver::PackageSource::Path(_) => {
                replace_local_package_materialization(&link, &target)
            }
        };
        let materialization = materialization
            .map_err(|e| format!("cannot materialize project package for {name}: {e}"))?;
        if let resolver::PackageSource::Path(source) = &package.source {
            let installed = link.canonicalize().map_err(|error| error.to_string())?;
            local_sources.insert(installed, source.clone());
        }
        println!("  {} {name}@{version}", materialization.verb());
    }

    if locked_lockfile.is_some() {
        println!("Used locked dependency graph");
    } else if mode.write_lock {
        // Skip the write when the resolved graph didn't change: `hew
        // install` (online or `--offline`) always resolves fresh, but
        // rewriting an already-current hew.lock on every run makes it look
        // like a mutating command even when nothing moved (#3233). Compare
        // structurally (both sides sorted by name, matching how
        // `write_lockfile` orders its output) rather than by serialized
        // bytes, so formatting alone never forces a write.
        let mut lf = lockfile::LockFile {
            packages: lock_packages,
        };
        lf.packages.sort_by(|a, b| a.name.cmp(&b.name));
        let unchanged = lockfile::read_lockfile(&lock_path).is_ok_and(|mut existing| {
            existing.packages.sort_by(|a, b| a.name.cmp(&b.name));
            existing == lf
        });
        if unchanged {
            println!("hew.lock is already up to date");
        } else if let Err(e) = lockfile::write_lockfile(&lock_path, &lf) {
            return Err(format!("cannot write hew.lock: {e}"));
        } else {
            println!("Wrote hew.lock");
        }
    }
    Ok(local_sources)
}

#[derive(Debug)]
struct VerifiedRegistryPackage {
    pinned: crate::atomic_fs::PinnedDir,
    checksum: String,
}

impl VerifiedRegistryPackage {
    fn path(&self) -> &Path {
        self.pinned.path()
    }
}

fn pinned_registry_paths(
    packages: &BTreeMap<(String, String, String), VerifiedRegistryPackage>,
) -> BTreeMap<(String, String, String), PathBuf> {
    packages
        .iter()
        .map(|(identity, package)| (identity.clone(), package.path().to_path_buf()))
        .collect()
}

fn pin_offline_registry_packages(
    registry: &registry::Registry,
    sources: &resolver::RegistrySources,
    allow_legacy_default: bool,
) -> Result<BTreeMap<(String, String, String), VerifiedRegistryPackage>, String> {
    let mut identities = sources.named().values().cloned().collect::<BTreeSet<_>>();
    identities.insert(sources.default_registry().to_string());
    let mut pinned = BTreeMap::new();

    for registry_id in identities {
        let include_legacy = allow_legacy_default && registry_id == sources.default_registry();
        let packages = registry
            .try_list_packages_for(&registry_id, include_legacy)
            .map_err(|error| {
                format!("cannot enumerate offline cache for registry `{registry_id}`: {error}")
            })?;
        for package in packages {
            let identity = (
                registry_id.clone(),
                package.name.clone(),
                package.version.clone(),
            );
            if pinned.contains_key(&identity) {
                continue;
            }

            let package_pin = if package.pin.is_generation()
                && package.path.starts_with(registry.source_root(&registry_id))
            {
                package.pin
            } else {
                let slot = registry.package_slot_for(&registry_id, &package.name, &package.version);
                let staged = crate::atomic_fs::StagedDir::new(&slot).map_err(|error| {
                    format!(
                        "cannot stage legacy cache migration for {}@{}: {error}",
                        package.name, package.version
                    )
                })?;
                copy_dir(package.pin.path(), staged.path()).map_err(|error| {
                    format!(
                        "cannot copy legacy cache for {}@{}: {error}",
                        package.name, package.version
                    )
                })?;
                let staged_checksum =
                    checksum::compute_dir_checksum(staged.path()).map_err(|error| {
                        format!(
                            "cannot verify legacy cache copy for {}@{}: {error}",
                            package.name, package.version
                        )
                    })?;
                let published = staged.publish_pinned(&slot).map_err(|error| {
                    format!(
                        "cannot publish migrated cache for {}@{}: {error}",
                        package.name, package.version
                    )
                })?;
                let published_checksum =
                    checksum::compute_dir_checksum(published.path()).map_err(|error| {
                        format!(
                            "cannot verify migrated cache for {}@{}: {error}",
                            package.name, package.version
                        )
                    })?;
                if published_checksum != staged_checksum {
                    return Err(format!(
                        "migrated cache changed before pinning for {}@{}",
                        package.name, package.version
                    ));
                }
                published
            };
            let checksum = checksum::compute_dir_checksum(package_pin.path()).map_err(|error| {
                format!(
                    "cannot verify offline cache for {}@{}: {error}",
                    package.name, package.version
                )
            })?;
            pinned.insert(
                identity,
                VerifiedRegistryPackage {
                    pinned: package_pin,
                    checksum,
                },
            );
        }
    }

    Ok(pinned)
}

fn verify_locked_registry_packages(
    lockfile: &lockfile::LockFile,
    registry: &registry::Registry,
) -> Result<BTreeMap<(String, String, String), VerifiedRegistryPackage>, String> {
    let mut pinned = BTreeMap::new();
    for entry in &lockfile.packages {
        if entry.source != "registry" {
            continue;
        }
        let expected = entry
            .checksum
            .as_deref()
            .expect("validated registry lock entry must have a checksum");
        let registry_id = entry
            .registry
            .as_deref()
            .expect("validated registry lock entry must have an identity");
        let pin = registry
            .pin_package_dir_for_if_present(registry_id, &entry.name, &entry.version)
            .map_err(|error| {
                format!(
                    "cannot resolve locked package {}@{} generation: {error}",
                    entry.name, entry.version
                )
            })?
            .ok_or_else(|| {
                format!(
                    "locked package {}@{} is missing from cache",
                    entry.name, entry.version
                )
            })?;
        let unresolved_path = pin.path().display().to_string();
        let pin = pin.canonicalize().map_err(|error| {
            format!(
                "locked package {}@{} is missing from cache at {}: {error}",
                entry.name, entry.version, unresolved_path
            )
        })?;
        let path = pin.path();
        if !path.join("hew.toml").is_file() {
            return Err(format!(
                "locked package {}@{} is missing from cache at {}",
                entry.name,
                entry.version,
                path.display()
            ));
        }
        let actual = checksum::compute_dir_checksum(path).map_err(|error| {
            format!(
                "cannot verify checksum for {}@{}: {error}",
                entry.name, entry.version
            )
        })?;
        if actual != expected {
            return Err(format!(
                "checksum mismatch for {}@{}\n  expected: {expected}\n  actual:   {actual}",
                entry.name, entry.version
            ));
        }
        pinned.insert(
            (
                registry_id.to_string(),
                entry.name.clone(),
                entry.version.clone(),
            ),
            VerifiedRegistryPackage {
                pinned: pin,
                checksum: actual,
            },
        );
    }
    Ok(pinned)
}

fn validate_locked_resolution(
    lockfile: &lockfile::LockFile,
    resolved: &BTreeMap<String, resolver::ResolvedPackage>,
) -> Result<(), String> {
    let locked = lockfile
        .packages
        .iter()
        .map(|package| (package.name.as_str(), package))
        .collect::<BTreeMap<_, _>>();

    for (name, package) in resolved {
        let entry = locked
            .get(name.as_str())
            .ok_or_else(|| format!("locked dependency graph is missing `{name}`"))?;
        let expected_source = match &package.source {
            resolver::PackageSource::Registry { .. } => "registry",
            resolver::PackageSource::Path(_) => "path",
        };
        if entry.version != package.version || entry.source != expected_source {
            return Err(format!(
                "locked dependency `{name}` is {}@{} but resolution requires {}@{}",
                entry.source, entry.version, expected_source, package.version
            ));
        }
        if let resolver::PackageSource::Registry { registry } = &package.source {
            if entry.registry.as_deref() != Some(registry) {
                return Err(format!(
                    "locked dependency `{name}` registry identity does not match the resolved graph"
                ));
            }
        }
        if let resolver::PackageSource::Path(resolved_path) = &package.source {
            let path_matches = entry.path.as_deref().is_some_and(|locked_path| {
                package.direct_path.as_deref() == Some(locked_path)
                    || Path::new(locked_path)
                        .canonicalize()
                        .is_ok_and(|path| &path == resolved_path)
            });
            if !path_matches {
                return Err(format!(
                    "locked path for dependency `{name}` does not match the resolved graph"
                ));
            }
        }
    }

    if let Some(extra) = lockfile
        .packages
        .iter()
        .find(|entry| !resolved.contains_key(&entry.name))
    {
        return Err(format!(
            "locked dependency graph contains unreachable package `{}`",
            extra.name
        ));
    }
    Ok(())
}

/// Fetch missing packages from the remote registry.
///
/// For each unresolved package in `failures`, queries the remote API
/// for available versions, finds the best match, downloads the tarball,
/// and unpacks it into the global registry.
#[expect(
    clippy::too_many_lines,
    reason = "CLI command handler requires many steps"
)]
fn fetch_missing_packages(
    failures: &[resolver::UnresolvedDep],
    registry: &registry::Registry,
    confirmed_registry: &mut BTreeMap<(String, String, String), VerifiedRegistryPackage>,
) -> Result<(), String> {
    for failure in failures {
        let api_client = make_client(failure.registry_selector.as_deref());
        if api_client.registry_identity() != failure.registry {
            return Err(format!(
                "registry client identity mismatch for `{}`: resolver selected `{}`, client uses `{}`",
                failure.package,
                failure.registry,
                api_client.registry_identity()
            ));
        }
        let registry_id = &failure.registry;
        let name = &failure.package;
        let requirements = &failure.requirements;
        let requirement_summary = requirements.join(", ");

        eprint!("Fetching {name} from registry... ");

        // The registry is authoritative in online mode. A failed request must
        // never admit an unconfirmed cache entry.
        let entries = match api_client.get_package(name) {
            Ok(entries) => entries,
            Err(api_err) => {
                eprintln!("failed");
                let cache_path = registry.source_root(registry_id).join(name);
                return Err(format!(
                    "registry request failed for package `{name}` version `{requirement_summary}` at `{}`: {api_err}; cached package path `{}` was not used (rerun with `hew install --offline` to use cached packages)",
                    api_client.package_url(name),
                    cache_path.display()
                ));
            }
        };

        // Find the best matching version.
        let matched = match resolver::resolve_version_from_entries_with_requirements(
            &entries,
            requirements,
        ) {
            Ok(result) => result,
            Err(e) => {
                eprintln!("failed");
                return Err(e.to_string());
            }
        };
        let Some(resolved) = matched else {
            eprintln!("no matching version");
            return Err(format!(
                "registry `{}` has no version of package `{name}` matching [{requirement_summary}]; cached package path `{}` was not used",
                api_client.package_url(name),
                registry.source_root(registry_id).join(name).display()
            ));
        };
        let version = &resolved.version;

        validate_registry_identity(name, version)?;
        verify_supplied_package_signatures(&api_client, name, &resolved)?;
        if let Some(verified) =
            registry.verified_online_cache_entry(registry_id, name, version, &resolved.checksum)?
        {
            confirmed_registry.insert(
                (registry_id.clone(), name.clone(), version.clone()),
                VerifiedRegistryPackage {
                    pinned: verified.pin,
                    checksum: verified.tree_checksum,
                },
            );
            eprintln!("confirmed cached {name}@{version}");
            continue;
        }

        // Get the download URL from the registry response.
        let Some(ref dl_url) = resolved.dl else {
            eprintln!("failed");
            return Err(format!(
                "registry did not provide a download URL for package `{name}` version `{version}`"
            ));
        };

        // Download the tarball.
        let tarball_data = match api_client.download_package_tarball(name, version, dl_url) {
            Ok(data) => data,
            Err(e) => {
                eprintln!("download failed");
                return Err(format!(
                    "could not download package `{name}` version `{version}`: {e}"
                ));
            }
        };

        // Verify checksum of downloaded tarball.
        let actual_checksum = tarball::checksum_bytes(&tarball_data);
        if actual_checksum != resolved.checksum {
            eprintln!("CHECKSUM MISMATCH");
            eprintln!("  error: tarball integrity check failed for {name}@{version}");
            eprintln!("  expected: {}", resolved.checksum);
            eprintln!("  actual:   {actual_checksum}");
            return Err(format!(
                "tarball integrity check failed for {name}@{version}"
            ));
        }

        validate_downloaded_registry_manifest(&tarball_data, name)?;

        let lock_dir = registry.source_root(registry_id).join(".locks");
        std::fs::create_dir_all(&lock_dir).map_err(|error| {
            format!(
                "could not create package cache lock directory {}: {error}",
                lock_dir.display()
            )
        })?;
        let lock_path = lock_dir.join(format!("{name}-{version}.lock"));
        let lock_file = std::fs::OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(false)
            .open(&lock_path)
            .map_err(|error| {
                format!(
                    "could not open package cache lock {}: {error}",
                    lock_path.display()
                )
            })?;
        loop {
            match lock_file.lock() {
                Ok(()) => break,
                Err(error) if error.kind() == std::io::ErrorKind::Interrupted => {}
                Err(error) => {
                    return Err(format!(
                        "could not acquire package cache lock {}: {error}",
                        lock_path.display()
                    ));
                }
            }
        }
        if let Some(verified) =
            registry.verified_online_cache_entry(registry_id, name, version, &resolved.checksum)?
        {
            confirmed_registry.insert(
                (registry_id.clone(), name.clone(), version.clone()),
                VerifiedRegistryPackage {
                    pinned: verified.pin,
                    checksum: verified.tree_checksum,
                },
            );
            eprintln!("confirmed cached {name}@{version}");
            continue;
        }

        let target = registry.package_slot_for(registry_id, name, version);
        let staged = crate::atomic_fs::StagedDir::new(&target).map_err(|error| {
            format!(
                "could not stage cache entry for {name}@{version} at {}: {error}",
                target.display()
            )
        })?;
        if let Err(e) = tarball::unpack(&tarball_data, staged.path()) {
            eprintln!("unpack failed");
            return Err(format!("could not unpack {name}@{version}: {e}"));
        }
        registry::Registry::write_cache_metadata(
            staged.path(),
            registry_id,
            name,
            version,
            &resolved.checksum,
            &tarball_data,
        )?;
        let pinned = staged.publish_pinned(&target).map_err(|error| {
            format!(
                "could not publish cache entry for {name}@{version} at {}: {error}",
                target.display()
            )
        })?;
        let pinned = pinned.canonicalize().map_err(|error| {
            format!("could not canonicalize published cache entry for {name}@{version}: {error}")
        })?;
        let tree_checksum = checksum::compute_dir_checksum(pinned.path()).map_err(|error| {
            format!("could not verify published cache entry for {name}@{version}: {error}")
        })?;
        confirmed_registry.insert(
            (registry_id.clone(), name.clone(), version.clone()),
            VerifiedRegistryPackage {
                pinned,
                checksum: tree_checksum,
            },
        );
        eprintln!("{name}@{version}");
    }

    Ok(())
}

const MAX_FETCH_ROUNDS: usize = 1024;

#[derive(Debug, Default)]
struct FetchProgress {
    rounds: usize,
    previous_failures: Option<Vec<(String, String, Vec<String>)>>,
}

impl FetchProgress {
    fn begin_round(&mut self, failures: &[resolver::UnresolvedDep]) -> Result<(), String> {
        if self.rounds >= MAX_FETCH_ROUNDS {
            return Err(format!(
                "registry fetch exceeded the finite limit of {MAX_FETCH_ROUNDS} rounds"
            ));
        }
        let signature = failures
            .iter()
            .map(|failure| {
                (
                    failure.registry.clone(),
                    failure.package.clone(),
                    failure.requirements.clone(),
                )
            })
            .collect::<Vec<_>>();
        if self.previous_failures.as_ref() == Some(&signature) {
            return Err("registry fetch repeated without making resolver progress".to_string());
        }
        self.previous_failures = Some(signature);
        self.rounds += 1;
        Ok(())
    }
}

fn validate_downloaded_registry_manifest(data: &[u8], package: &str) -> Result<(), String> {
    let decoder = zstd::Decoder::new(std::io::Cursor::new(data))
        .map_err(|error| format!("cannot inspect archive manifest for `{package}`: {error}"))?;
    let mut archive = tar::Archive::new(decoder);
    let mut manifest_text = None;
    for entry in archive
        .entries()
        .map_err(|error| format!("cannot inspect archive for `{package}`: {error}"))?
    {
        let mut entry =
            entry.map_err(|error| format!("cannot inspect archive for `{package}`: {error}"))?;
        let path = entry
            .path()
            .map_err(|error| format!("cannot inspect archive for `{package}`: {error}"))?;
        let normalized = path
            .components()
            .filter_map(|component| match component {
                std::path::Component::Normal(part) => Some(part),
                std::path::Component::CurDir => None,
                _ => Some(component.as_os_str()),
            })
            .collect::<PathBuf>();
        if normalized != Path::new("hew.toml") {
            continue;
        }
        if manifest_text.is_some() {
            return Err(format!(
                "registry archive for `{package}` contains multiple hew.toml entries"
            ));
        }
        if !entry.header().entry_type().is_file() {
            return Err(format!(
                "registry archive for `{package}` has a non-file hew.toml entry"
            ));
        }
        if entry.size() > 1024 * 1024 {
            return Err(format!(
                "registry archive manifest for `{package}` exceeds 1 MiB"
            ));
        }
        let mut text = String::new();
        entry
            .read_to_string(&mut text)
            .map_err(|error| format!("cannot read archive manifest for `{package}`: {error}"))?;
        manifest_text = Some(text);
    }
    let text = manifest_text
        .ok_or_else(|| format!("registry archive for `{package}` is missing hew.toml"))?;
    let parsed = manifest::parse_manifest_text(&text)
        .map_err(|error| format!("invalid registry manifest for `{package}`: {error}"))?;
    resolver::validate_registry_manifest(package, &parsed).map_err(|error| error.to_string())
}

fn validate_registry_identity(name: &str, version: &str) -> Result<(), String> {
    if !is_valid_package_name(name) {
        return Err(invalid_package_name_message(name));
    }
    semver::Version::parse(version)
        .map(|_| ())
        .map_err(|error| format!("invalid registry version `{version}` for `{name}`: {error}"))
}

/// Verify an Ed25519 signature over a checksum by fetching the public key
/// from the registry.
pub(crate) fn verify_supplied_package_signatures(
    api_client: &client::RegistryClient,
    name: &str,
    entry: &resolver::ResolvedEntry,
) -> Result<(), String> {
    match (entry.sig.is_empty(), entry.key_fp.is_empty()) {
        (true, true) => eprintln!("warning: package {name}@{} is unsigned", entry.version),
        (false, false) => {
            verify_package_signature(api_client, &entry.checksum, &entry.sig, &entry.key_fp)?;
        }
        _ => {
            return Err(format!(
                "incomplete publisher signature for {name}@{}",
                entry.version
            ))
        }
    }
    match (&entry.registry_sig, &entry.published_at) {
        (None, None) => {}
        (Some(signature), Some(timestamp)) => verify_registry_signature(
            api_client,
            &entry.registry_name,
            &entry.version,
            &entry.checksum,
            &entry.sig,
            timestamp,
            signature,
        )?,
        _ => {
            return Err(format!(
                "incomplete registry signature for {name}@{}",
                entry.version
            ))
        }
    }
    Ok(())
}

/// Verify the publisher's checksum signature using the registered public key.
pub(crate) fn verify_package_signature(
    api_client: &client::RegistryClient,
    checksum: &str,
    signature: &str,
    key_fingerprint: &str,
) -> Result<(), String> {
    use base64::Engine as _;

    // Fetch the public key from the registry.
    let key_record = api_client
        .get_public_key(key_fingerprint)
        .map_err(|e| format!("could not fetch signing key {key_fingerprint}: {e}"))?;

    // Decode the base64 public key to raw bytes.
    let pub_bytes = base64::engine::general_purpose::STANDARD
        .decode(&key_record.public_key)
        .map_err(|e| format!("invalid public key encoding: {e}"))?;

    let pub_arr: [u8; 32] = pub_bytes
        .try_into()
        .map_err(|_| "public key is not 32 bytes".to_string())?;

    // Verify the signature.
    signing::verify(checksum.as_bytes(), signature, &pub_arr)
        .map_err(|e| format!("signature invalid: {e}"))
}

/// Verify the registry's counter-signature over a published package.
///
/// Reconstructs the canonical message and verifies against the registry's
/// public key. Returns `Ok(())` on success.
fn verify_registry_signature(
    api_client: &client::RegistryClient,
    name: &str,
    version: &str,
    checksum: &str,
    publisher_sig: &str,
    published_at: &str,
    registry_sig: &str,
) -> Result<(), String> {
    let canonical =
        format!("registry:v1:{name}@{version}:{checksum}:{publisher_sig}:{published_at}");
    verify_registry_message(api_client, &canonical, registry_sig, None)
}

pub(crate) fn verify_registry_message(
    api_client: &client::RegistryClient,
    canonical: &str,
    registry_sig: &str,
    key_fingerprint: Option<&str>,
) -> Result<(), String> {
    use base64::Engine as _;

    let key_resp = api_client
        .get_registry_key()
        .map_err(|e| format!("could not fetch registry key: {e}"))?;
    if key_fingerprint.is_some_and(|fingerprint| fingerprint != key_resp.key_id) {
        return Err("registry signing key fingerprint mismatch".to_string());
    }
    let pub_bytes = base64::engine::general_purpose::STANDARD
        .decode(&key_resp.public_key)
        .map_err(|e| format!("invalid registry public key encoding: {e}"))?;

    let pub_arr: [u8; 32] = pub_bytes
        .try_into()
        .map_err(|_| "registry public key is not 32 bytes".to_string())?;

    signing::verify(canonical.as_bytes(), registry_sig, &pub_arr)
        .map_err(|e| format!("registry signature invalid: {e}"))
}

fn cmd_publish(
    registry: &registry::Registry,
    registry_name: Option<&str>,
    flags: PublishFlags,
    check_generation: &impl Fn(&Path) -> Result<(), String>,
) {
    let fail = |error: &dyn std::fmt::Display| -> ! {
        eprintln!("hew publish: {error}");
        std::process::exit(1);
    };
    let cwd = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));

    // Phase 1, offline: every member validates before anything is checked.
    let (workspace, selected) =
        publish_selection(&cwd, flags.workspace).unwrap_or_else(|e| fail(&e));
    let selected: Vec<&Path> = selected.iter().map(PathBuf::as_path).collect();
    let members = publish::plan(&workspace, &selected).unwrap_or_else(|error| fail(&error));
    if flags.workspace {
        println!(
            "workspace {}: {} package{}",
            workspace.root.display(),
            members.len(),
            if members.len() == 1 { "" } else { "s" }
        );
    }

    // Phase 2: install each member's dependencies (members from their
    // directories), then check and pack it.
    let cfg = load_config_or_exit();
    let sources = registry_sources_or_exit(&cfg, registry_name).with_workspace(&workspace);
    let install = InstallMode {
        locked: false,
        offline: flags.local,
        write_lock: false,
    };
    let packages = pack_publish_members(
        &workspace,
        &members,
        registry,
        &sources,
        registry_name,
        install,
        check_generation,
    )
    .unwrap_or_else(|error| fail(&error));

    if flags.local {
        publish_local_packages(registry, &packages, flags.dry_run).unwrap_or_else(|e| fail(&e));
        return;
    }

    // Phases 3 to 5: query, authenticate if anything uploads, then upload and
    // read back in order. `make_client` validates a named registry first, so a
    // typo'd `--registry NAME` gets the unknown-registry diagnostic rather
    // than "not logged in".
    let mut credentials = publish::Fixed(Some(|| {
        let token = resolve_registry_token(&credentials::credentials_path(), registry_name)
            .map_err(|error| {
                publish::PublishError::Auth(match error {
                    credentials::CredentialError::NotLoggedIn => {
                        "not logged in; run `hew login` to publish to a remote registry\n\
                         Use `hew publish --local` to install this package into the local \
                         registry only."
                            .to_string()
                    }
                    credentials::CredentialError::Parse(_)
                    | credentials::CredentialError::Io(_) => error.to_string(),
                })
            })?;
        Ok(publish::Signer {
            keypair: load_publish_keypair()?,
            token,
        })
    }));
    let mut github_actions;
    let auth: &mut dyn publish::Auth = match flags.auth {
        PublishAuth::Credentials => &mut credentials,
        PublishAuth::GithubActions => {
            github_actions = crate::trusted::GithubActions::new(
                make_client(registry_name),
                crate::trusted::OidcEndpoint::from_env(),
            );
            &mut github_actions
        }
    };
    let mut tally = PublishTally::default();
    let result = publish::publish_all(
        make_client(registry_name),
        &packages,
        flags.dry_run,
        auth,
        &std::thread::sleep,
        &mut |package, outcome| tally.report(package, outcome),
    );
    if flags.workspace {
        tally.summarize(flags.dry_run);
    }
    result.unwrap_or_else(|error| fail(&error));
}

fn pack_publish_members(
    workspace: &crate::workspace::Workspace,
    members: &[&crate::workspace::Member],
    registry: &registry::Registry,
    sources: &resolver::RegistrySources,
    registry_name: Option<&str>,
    install: InstallMode,
    check_generation: &impl Fn(&Path) -> Result<(), String>,
) -> Result<Vec<publish::PackedPackage>, String> {
    let mut packages: Vec<publish::PackedPackage> = Vec::new();
    // Selected dependencies have already been packed in topological order.
    // A single-member publish also checks unselected siblings with the same packer.
    let mut sibling_archives = BTreeMap::new();
    for member in members {
        let local_sources = install_dependencies(
            &member.dir,
            &member.manifest,
            registry,
            sources,
            registry_name,
            install,
            None,
        )
        .map_err(|error| format!("{}: {error}", member.name()))?;
        let package = publish::check_and_pack_with_dependencies(
            &member.dir,
            member.manifest.clone(),
            check_generation,
            &mut |input, dependency_root| {
                let Some(source) = local_sources.get(dependency_root) else {
                    return Ok(());
                };
                let Some(sibling) = workspace.members.iter().find(|item| &item.dir == source) else {
                    return Ok(());
                };
                let archive = if let Some(package) = packages.iter().find(|p| p.name() == sibling.name()) {
                    &package.archive
                } else {
                    match sibling_archives.entry(source.clone()) {
                        std::collections::btree_map::Entry::Occupied(entry) => entry.into_mut(),
                        std::collections::btree_map::Entry::Vacant(entry) => {
                            check_generation(source)?;
                            entry.insert(publish::pack(source, &sibling.manifest).map_err(|error| error.to_string())?)
                        }
                    }
                };
                let relative = input.strip_prefix(dependency_root).map_err(|error| error.to_string())?;
                if !archive.files.iter().any(|file| file == relative) {
                    return Err(format!(
                        "E_PUBLISH_MISSING_INPUT: compilation input `{}` is excluded from workspace dependency `{}`",
                        relative.display(), sibling.name()
                    ));
                }
                Ok(())
            },
        )
        .map_err(|error| error.to_string())?;
        packages.push(package);
    }
    Ok(packages)
}

/// `hew publish` switches that shape the run.
#[derive(Debug, Clone, Copy)]
struct PublishFlags {
    local: bool,
    dry_run: bool,
    workspace: bool,
    auth: PublishAuth,
}

/// The workspace a publish runs against and the member directories it
/// publishes: every member with `--workspace`, otherwise the package in
/// `cwd`, inside its enclosing workspace if it has one.
fn publish_selection(
    cwd: &Path,
    whole_workspace: bool,
) -> Result<(crate::workspace::Workspace, Vec<PathBuf>), String> {
    if whole_workspace {
        let root = crate::workspace::find_root(cwd).ok_or_else(|| {
            format!(
                "no hew.toml declaring [workspace] in {} or any parent directory",
                cwd.display()
            )
        })?;
        let workspace = crate::workspace::load(&root).map_err(|e| e.to_string())?;
        let selected = workspace.members.iter().map(|m| m.dir.clone()).collect();
        return Ok((workspace, selected));
    }
    let manifest_path = cwd.join(crate::project::MANIFEST_FILE);
    if !manifest_path.exists() {
        return Err("no hew.toml found in current directory".to_string());
    }
    manifest::parse_manifest(&manifest_path).map_err(|e| e.to_string())?;
    let workspace = match crate::workspace::find_enclosing(cwd).map_err(|e| e.to_string())? {
        Some(workspace) => workspace,
        None => crate::workspace::Workspace::single(cwd).map_err(|e| e.to_string())?,
    };
    let dir = cwd
        .canonicalize()
        .map_err(|e| format!("{}: {e}", cwd.display()))?;
    Ok((workspace, vec![dir]))
}

/// `--local`: install each package into the local registry only.
fn publish_local_packages(
    registry: &registry::Registry,
    packages: &[publish::PackedPackage],
    dry_run: bool,
) -> Result<(), publish::PublishError> {
    if dry_run {
        for package in packages {
            print_would_publish(package);
        }
        return Ok(());
    }
    let keypair = load_publish_keypair()?;
    for package in packages {
        let published = publish::publish_local_package(
            registry,
            package.name(),
            package.version(),
            &package.archive.data,
        )
        .map_err(|error| publish::PublishError::Invalid(error.to_string()))?;
        println!(
            "Published {}@{} to {} (local registry only — not published to a remote registry)",
            package.name(),
            package.version(),
            published.path().display()
        );
        println!("Checksum: {}", package.archive.checksum);
        println!(
            "Signature: {}",
            keypair.sign(package.archive.checksum.as_bytes())
        );
        println!("Key: {}", keypair.fingerprint());
    }
    Ok(())
}

fn print_would_publish(package: &publish::PackedPackage) {
    println!(
        "Would publish {}@{} ({} bytes)",
        package.name(),
        package.version(),
        package.archive.data.len()
    );
    println!("Checksum: {}", package.archive.checksum);
}

/// Prints each remote outcome as it is decided and counts them.
#[derive(Debug, Default)]
struct PublishTally {
    published: usize,
    skipped: usize,
}

impl PublishTally {
    fn report(&mut self, package: &publish::PackedPackage, outcome: &publish::Outcome) {
        match outcome {
            publish::Outcome::WouldPublish => {
                self.published += 1;
                print_would_publish(package);
            }
            publish::Outcome::Skipped(skip) => {
                self.skipped += 1;
                println!("Skipped {}@{}: {skip}", package.name(), package.version());
            }
            publish::Outcome::Published {
                signature,
                key_fingerprint,
            } => {
                self.published += 1;
                println!(
                    "Published {}@{} to registry",
                    package.name(),
                    package.version()
                );
                println!("Checksum: {}", package.archive.checksum);
                println!("Signature: {signature}");
                println!("Key: {key_fingerprint}");
                println!(
                    "Verified: stored record, publisher and registry signatures, and served archive"
                );
            }
        }
    }

    fn summarize(&self, dry_run: bool) {
        let verb = if dry_run {
            "would publish"
        } else {
            "published"
        };
        println!("{verb} {}, skipped {}", self.published, self.skipped);
    }
}

fn load_publish_keypair() -> Result<signing::KeyPair, publish::PublishError> {
    signing::load_keypair(&signing::default_key_dir()).map_err(|error| {
        publish::PublishError::Auth(match error {
            signing::SignError::NoKey => {
                "no signing key found\nRun `hew key generate` to create one.".to_string()
            }
            error => error.to_string(),
        })
    })
}

fn cmd_list(registry: &registry::Registry) {
    let packages = registry.list_packages();
    if packages.is_empty() {
        println!("No packages installed in global registry.");
    } else {
        println!("Installed packages in {}:", registry.root().display());
        for pkg in &packages {
            println!("  {}@{}  ({})", pkg.name, pkg.version, pkg.path.display());
        }
    }
}

// ── new commands ─────────────────────────────────────────────────────────────

fn cmd_search(
    query: &str,
    category: Option<&str>,
    page: u32,
    per_page: u32,
    registry: &registry::Registry,
    registry_name: Option<&str>,
) {
    // Try remote registry search first.
    let api_client = make_client(registry_name);
    match api_client.search(query, category, page, per_page) {
        Ok(result) => {
            if result.results.is_empty() {
                println!("No packages matching `{query}`.");
            } else {
                println!(
                    "Packages matching `{query}` ({} total, page {}):",
                    result.total, page
                );
                for hit in &result.results {
                    let desc = hit.description.as_deref().unwrap_or("");
                    let dl = hit
                        .downloads
                        .map(|n| format!(" ({n} downloads)"))
                        .unwrap_or_default();
                    let ver = hit.latest_version.as_deref().unwrap_or("?");
                    if desc.is_empty() {
                        println!("  {}@{ver}{dl}", hit.name);
                    } else {
                        println!("  {}@{ver} — {desc}{dl}", hit.name);
                    }
                }
            }
            return;
        }
        Err(e) => {
            eprintln!("warning: remote search failed: {e}");
            eprintln!("Falling back to local search.");
        }
    }

    // Fall back to local registry search.
    let packages = registry.list_packages();
    let query_lower = query.to_lowercase();
    let matches: Vec<_> = packages
        .iter()
        .filter(|p| p.name.to_lowercase().contains(&query_lower))
        .collect();

    if matches.is_empty() {
        println!("No packages matching `{query}`.");
    } else {
        println!("Packages matching `{query}`:");
        for pkg in &matches {
            println!("  {}@{}", pkg.name, pkg.version);
        }
    }
}

/// Print info for `package` from a non-empty set of registry index entries,
/// picking the highest semver version. Shared by `cmd_info`'s named- and
/// default-registry branches so the two print identically apart from the
/// `source:` line.
fn print_remote_package_info(
    package: &str,
    entries: Vec<index::IndexEntry>,
    registry_name: Option<&str>,
) {
    let Some(latest) = entries.into_iter().max_by(|left, right| {
        let left_version =
            semver::Version::parse(&left.vers).unwrap_or_else(|_| semver::Version::new(0, 0, 0));
        let right_version =
            semver::Version::parse(&right.vers).unwrap_or_else(|_| semver::Version::new(0, 0, 0));
        left_version.cmp(&right_version)
    }) else {
        eprintln!("hew info: package `{package}` not found in registry");
        std::process::exit(1);
    };
    println!("{}@{}", latest.name, latest.vers);
    match registry_name {
        Some(name) => println!("  source: registry `{name}`"),
        None => println!("  source: default registry"),
    }
    println!("  checksum: {}", latest.cksum);
    if !latest.deps.is_empty() {
        println!("  dependencies:");
        for dep in latest.deps {
            println!("    {} {}", dep.name, dep.req);
        }
    }
}

fn cmd_info(package: &str, registry: &registry::Registry, registry_name: Option<&str>) {
    if let Some(registry_name) = registry_name {
        let api_client = make_client(Some(registry_name));
        match api_client.get_package(package) {
            Ok(entries) if !entries.is_empty() => {
                print_remote_package_info(package, entries, Some(registry_name));
            }
            Ok(_) => {
                eprintln!("hew info: package `{package}` not found in registry `{registry_name}`");
                std::process::exit(1);
            }
            Err(e) => {
                eprintln!("hew info: {e}");
                std::process::exit(1);
            }
        }
        return;
    }

    // No `-r`: ask the default registry first, mirroring cmd_search's
    // client-first shape, and fall back to the local cache only when the
    // remote lookup itself fails (network, timeout, …) rather than
    // treating "not found remotely" and "registry unreachable" alike.
    let api_client = make_client(None);
    match api_client.get_package(package) {
        Ok(entries) if !entries.is_empty() => {
            print_remote_package_info(package, entries, None);
            return;
        }
        Ok(_) => {
            // Not found on the default registry: fall through to the
            // local cache below before giving up.
        }
        Err(e) => {
            eprintln!("warning: remote lookup failed: {e}");
            eprintln!("Falling back to local cache.");
        }
    }

    let packages = registry.list_packages();
    let versions: Vec<_> = packages.iter().filter(|p| p.name == package).collect();

    if versions.is_empty() {
        eprintln!("hew info: package `{package}` not found in registry");
        std::process::exit(1);
    }

    // Find the latest version by semver.
    let latest = versions
        .iter()
        .filter_map(|p| {
            semver::Version::parse(&p.version)
                .ok()
                .map(|v| (v, &p.path))
        })
        .max_by(|(a, _), (b, _)| a.cmp(b));

    let Some((latest_ver, latest_path)) = latest else {
        let pkg = versions.last().unwrap();
        println!("{package}@{}", pkg.version);
        return;
    };

    let manifest_path = latest_path.join("hew.toml");
    if let Ok(m) = manifest::parse_manifest(&manifest_path) {
        println!("{}", m.package.name);
        println!("  version: {latest_ver}");
        if let Some(desc) = &m.package.description {
            println!("  description: {desc}");
        }
        if let Some(authors) = &m.package.authors {
            if !authors.is_empty() {
                println!("  authors: {}", authors.join(", "));
            }
        }
        if let Some(license) = &m.package.license {
            println!("  license: {license}");
        }
        if !m.dependencies.is_empty() {
            println!("  dependencies:");
            for (dep, spec) in &m.dependencies {
                println!("    {dep} = \"{}\"", spec.version_req());
            }
        }
    } else {
        println!("{package}@{latest_ver}");
    }

    if versions.len() > 1 {
        let mut all_vers: Vec<_> = versions
            .iter()
            .filter_map(|p| semver::Version::parse(&p.version).ok())
            .collect();
        all_vers.sort();
        let ver_strs: Vec<String> = all_vers.iter().map(ToString::to_string).collect();
        println!("  available versions: {}", ver_strs.join(", "));
    }
}

fn cmd_tree(registry: &registry::Registry) {
    let cwd = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    let manifest_path = cwd.join("hew.toml");
    if !manifest_path.exists() {
        eprintln!("hew tree: no hew.toml found in current directory");
        std::process::exit(1);
    }
    let m = match manifest::parse_manifest(&manifest_path) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("hew tree: {e}");
            std::process::exit(1);
        }
    };

    println!("{} v{}", m.package.name, m.package.version);

    if m.dependencies.is_empty() {
        println!("  (no dependencies)");
        return;
    }

    let deps: Vec<_> = m.dependencies.iter().collect();
    for (i, (name, spec)) in deps.iter().enumerate() {
        let is_last = i == deps.len() - 1;
        let prefix = if is_last { "└── " } else { "├── " };
        let child_prefix = if is_last { "    " } else { "│   " };

        let ver_req = spec.version_req();
        let resolved = match resolver::resolve_version(name, ver_req, registry) {
            Ok(resolved) => resolved,
            Err(resolver::ResolveError::NoMatchingVersion { .. }) => ver_req.to_string(),
            Err(e) => {
                eprintln!("hew tree: dependency {name}@{ver_req}: {e}");
                std::process::exit(1);
            }
        };
        println!("{prefix}{name}@{resolved}");

        // Check if this dep itself has dependencies.
        let dep_toml = registry.package_dir(name, &resolved).join("hew.toml");
        if let Ok(dep_m) = manifest::parse_manifest(&dep_toml) {
            let sub_deps: Vec<_> = dep_m.dependencies.iter().collect();
            for (j, (sub_name, sub_spec)) in sub_deps.iter().enumerate() {
                let sub_is_last = j == sub_deps.len() - 1;
                let sub_prefix = if sub_is_last {
                    "└── "
                } else {
                    "├── "
                };
                println!(
                    "{child_prefix}{sub_prefix}{sub_name}@{}",
                    sub_spec.version_req()
                );
            }
        }
    }
}

fn cmd_update(package: Option<&str>, registry: &registry::Registry) {
    let cwd = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    let manifest_path = cwd.join("hew.toml");
    if !manifest_path.exists() {
        eprintln!("hew update: no hew.toml found in current directory");
        std::process::exit(1);
    }
    let mut m = match manifest::parse_manifest(&manifest_path) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("hew update: {e}");
            std::process::exit(1);
        }
    };

    let packages = registry.list_packages();
    let mut updated = Vec::new();

    let deps_to_update: Vec<String> = if let Some(pkg) = package {
        if !m.dependencies.contains_key(pkg) {
            eprintln!("hew update: `{pkg}` is not a dependency");
            std::process::exit(1);
        }
        vec![pkg.to_string()]
    } else {
        m.dependencies.keys().cloned().collect()
    };

    for dep_name in &deps_to_update {
        let latest = find_latest_version(&packages, dep_name);
        if let Some(latest_ver) = latest {
            let old = m
                .dependencies
                .get(dep_name)
                .map_or_else(String::new, |s| s.version_req().to_string());
            if old != latest_ver {
                m.dependencies.insert(
                    dep_name.clone(),
                    manifest::DepSpec::Version(latest_ver.clone()),
                );
                updated.push((dep_name.clone(), old, latest_ver));
            }
        }
    }

    if updated.is_empty() {
        println!("All dependencies are up to date.");
    } else {
        if let Err(e) = manifest::save_manifest(&manifest_path, &m) {
            eprintln!("hew update: {e}");
            std::process::exit(1);
        }
        for (name, old, new) in &updated {
            println!("Updated {name}: {old} \u{2192} {new}");
        }
    }
}

fn cmd_remove(package: &str) {
    let cwd = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    let manifest_path = cwd.join("hew.toml");
    if !manifest_path.exists() {
        eprintln!("hew remove: no hew.toml found in current directory");
        std::process::exit(1);
    }

    match manifest::remove_dependency(&manifest_path, package) {
        Ok(true) => {
            // Remove local project package materialization if it exists.
            let link = match crate::paths::package_link(&project_packages_path(&cwd), package) {
                Ok(link) => link,
                Err(e) => {
                    eprintln!("warning: could not remove local materialization for {package}: {e}");
                    println!("Removed {package} from hew.toml");
                    return;
                }
            };
            if std::fs::symlink_metadata(&link).is_ok() {
                if let Err(e) = remove_path_for_replacement(&link) {
                    eprintln!("warning: could not remove {}: {e}", link.display());
                }
            }
            println!("Removed {package} from hew.toml");
        }
        Ok(false) => {
            eprintln!("hew remove: `{package}` is not a dependency");
            std::process::exit(1);
        }
        Err(e) => {
            eprintln!("hew remove: {e}");
            std::process::exit(1);
        }
    }
}

/// Collect every manifest-level problem: malformed package identity plus any
/// dependency requirement the registry cannot satisfy.
fn collect_manifest_issues(
    m: &manifest::HewManifest,
    root: &Path,
    registry: &registry::Registry,
    sources: &resolver::RegistrySources,
) -> Vec<String> {
    let mut issues = Vec::new();

    if let Err(issue) = crate::package_name::check_importable(&m.package.name) {
        issues.push(issue);
    }

    if semver::Version::parse(&m.package.version).is_err() {
        issues.push(format!(
            "invalid version `{}`: must be valid semver (e.g. 1.0.0)",
            m.package.version
        ));
    }

    if let Err(issue) = manifest::check_compiler_range(&m.package) {
        issues.push(issue);
    }

    if let Err(issue) = crate::native::check_crate_shape(root, m) {
        issues.push(issue);
    }

    for (name, spec) in &m.dependencies {
        let req = spec.version_req();
        if let Err(e) = resolver::resolve_dependency_from_root(
            &m.package.name,
            name,
            spec,
            root,
            registry,
            sources,
        ) {
            issues.push(format!("dependency {name}@{req}: {e}"));
        }
    }

    issues
}

fn cmd_outdated(registry: &registry::Registry) {
    let cwd = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    let manifest_path = cwd.join("hew.toml");
    if !manifest_path.exists() {
        eprintln!("hew outdated: no hew.toml found in current directory");
        std::process::exit(1);
    }
    let m = match manifest::parse_manifest(&manifest_path) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("hew outdated: {e}");
            std::process::exit(1);
        }
    };

    if m.dependencies.is_empty() {
        println!("No dependencies.");
        return;
    }

    let packages = registry.list_packages();

    println!(
        "{:<30} {:<15} {:<15} Status",
        "Package", "Current", "Latest"
    );
    println!("{}", "\u{2500}".repeat(75));

    for (name, spec) in &m.dependencies {
        let current = spec.version_req();
        let latest =
            find_latest_version(&packages, name).unwrap_or_else(|| "(not found)".to_string());
        let status = if latest == current {
            "up-to-date"
        } else {
            "outdated"
        };
        println!("{name:<30} {current:<15} {latest:<15} {status}");
    }
}

// ── new registry commands ────────────────────────────────────────────────────

fn cmd_login() {
    let api_client = client::RegistryClient::new();
    let device = match api_client.login_device() {
        Ok(d) => d,
        Err(e) => {
            eprintln!("hew login: {e}");
            std::process::exit(1);
        }
    };

    println!("Open this URL in your browser:");
    println!("  {}", device.verification_uri);
    println!();
    println!("Enter code: {}", device.user_code);
    println!();
    println!("Waiting for authorization...");

    let interval = std::time::Duration::from_secs(device.interval.max(5));
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(device.expires_in);

    loop {
        std::thread::sleep(interval);
        if std::time::Instant::now() > deadline {
            eprintln!("hew login: authorization timed out");
            std::process::exit(1);
        }

        match api_client.login_token(&device.device_code) {
            Ok(resp) => {
                if let Some(token) = resp.token {
                    let cred_path = credentials::credentials_path();
                    if let Err(e) = credentials::update_default_credentials(
                        &cred_path,
                        credentials::Credentials {
                            token,
                            github_user: resp.github_user.clone(),
                        },
                    ) {
                        eprintln!("hew login: {e}");
                        std::process::exit(1);
                    }
                    let user = resp.github_user.as_deref().unwrap_or("unknown");
                    println!("Logged in as {user}");
                    return;
                }
                if resp.error.as_deref() != Some("authorization_pending") {
                    eprintln!(
                        "hew login: {}",
                        resp.error.unwrap_or_else(|| "unknown error".to_string())
                    );
                    std::process::exit(1);
                }
            }
            Err(e) => {
                eprintln!("hew login: {e}");
                std::process::exit(1);
            }
        }
    }
}

fn cmd_logout() {
    let cred_path = credentials::credentials_path();
    if cred_path.exists() {
        if let Err(e) = std::fs::remove_file(&cred_path) {
            eprintln!("hew logout: {e}");
            std::process::exit(1);
        }
        println!("Logged out.");
    } else {
        println!("Not logged in.");
    }
}

fn cmd_key_generate() {
    let key_dir = signing::default_key_dir();
    let secret_path = key_dir.join("id_ed25519");

    if secret_path.exists() {
        eprintln!(
            "hew key generate: key already exists at {}",
            secret_path.display()
        );
        eprintln!("Run `hew key register` to register it with the registry,");
        eprintln!("or remove it first if you want to regenerate.");
        std::process::exit(1);
    }

    let keypair = signing::KeyPair::generate();
    if let Err(e) = signing::save_keypair(&key_dir, &keypair) {
        eprintln!("hew key generate: {e}");
        std::process::exit(1);
    }

    println!("Generated Ed25519 signing key:");
    println!("  Private: {}", secret_path.display());
    println!("  Public:  {}", key_dir.join("id_ed25519.pub").display());
    println!("  Fingerprint: {}", keypair.fingerprint());

    // Try to register the key with the registry. A generated key is usable
    // for local work without registration, so a failure here is reported and
    // recovered with `hew key register` rather than losing the new key.
    let cred_path = credentials::credentials_path();
    if let Ok(token) = resolve_registry_token(&cred_path, None) {
        match register_signing_key(None, token, &keypair.public_key_base64()) {
            Ok(fp) => println!("Key registered with registry (fingerprint: {fp})"),
            Err(e) => {
                eprintln!("warning: could not register key with registry: {e}");
                eprintln!("Run `hew key register` once the registry is reachable.");
            }
        }
    } else {
        println!("Run `hew login`, then `hew key register`, to register this key.");
    }
}

/// Send a public key to a registry's key endpoint.
///
/// The one path by which a signing key becomes known to a registry: `hew key
/// generate` calls it for a key it just created, `hew key register` calls it
/// for a key that already exists on disk.
fn register_signing_key(
    registry_name: Option<&str>,
    token: String,
    public_key_b64: &str,
) -> Result<String, client::ApiError> {
    let api_client = match registry_name {
        Some(name) => {
            let remote = resolve_named_registry_or_exit(name);
            client::RegistryClient::with_url(remote.api)
        }
        None => client::RegistryClient::new(),
    }
    .with_token(token);
    api_client.register_key(public_key_b64)
}

/// Register the signing key already on disk with the registry.
///
/// Publishing signs with `~/.hew/keys/id_ed25519`, and the registry rejects a
/// signature from a key it does not know. Before this command the only way to
/// register an existing key was to delete it and generate another, which
/// invalidates every signature the old key ever made.
fn cmd_key_register(registry_name: Option<&str>) {
    let key_dir = signing::default_key_dir();
    let keypair = match signing::load_keypair(&key_dir) {
        Ok(kp) => kp,
        Err(e) => {
            eprintln!("hew key register: {e}");
            std::process::exit(1);
        }
    };

    if let Some(name) = registry_name {
        resolve_named_registry_or_exit(name);
    }
    let cred_path = credentials::credentials_path();
    let token = resolve_registry_token(&cred_path, registry_name).unwrap_or_else(|err| {
        match err {
            credentials::CredentialError::NotLoggedIn => {
                eprintln!("hew key register: not logged in");
                eprintln!("Run `hew login` first.");
            }
            credentials::CredentialError::Parse(_) | credentials::CredentialError::Io(_) => {
                eprintln!("hew key register: {err}");
            }
        }
        std::process::exit(1);
    });

    match register_signing_key(registry_name, token, &keypair.public_key_base64()) {
        Ok(fp) => println!("Key registered with registry (fingerprint: {fp})"),
        Err(e) => {
            eprintln!("hew key register: {e}");
            std::process::exit(1);
        }
    }
}

fn cmd_key_list() {
    let key_dir = signing::default_key_dir();
    let pub_path = key_dir.join("id_ed25519.pub");

    if !pub_path.exists() {
        println!("No signing keys found.");
        println!("Run `hew key generate` to create one.");
        return;
    }

    match signing::load_keypair(&key_dir) {
        Ok(kp) => {
            println!("Ed25519 signing key:");
            println!("  Path:        {}", key_dir.display());
            println!("  Fingerprint: {}", kp.fingerprint());
            println!("  Public key:  {}", kp.public_key_base64());
        }
        Err(e) => {
            eprintln!("hew key list: {e}");
            std::process::exit(1);
        }
    }
}

fn cmd_registry_key() {
    let api_client = client::RegistryClient::new();
    match api_client.get_registry_key() {
        Ok(key) => {
            println!("Registry signing key:");
            println!("  Key ID:     {}", key.key_id);
            println!("  Algorithm:  {}", key.algorithm);
            println!("  Public key: {}", key.public_key);
        }
        Err(e) => {
            eprintln!("hew key registry: {e}");
            std::process::exit(1);
        }
    }
}

fn cmd_key_info(fingerprint: &str) {
    let api_client = client::RegistryClient::new();
    match api_client.get_public_key(fingerprint) {
        Ok(key) => {
            println!("Signing key:");
            println!("  Fingerprint:  {}", key.fingerprint);
            println!("  Key type:     {}", key.key_type);
            println!("  Public key:   {}", key.public_key);
            println!("  GitHub user:  {}", key.github_user);
            println!("  GitHub ID:    {}", key.github_id);
        }
        Err(e) => {
            eprintln!("hew key info: {e}");
            std::process::exit(1);
        }
    }
}

fn cmd_namespace_info(prefix: &str) {
    let api_client = client::RegistryClient::new();
    match api_client.get_namespace(prefix) {
        Ok(info) => {
            println!("Namespace `{}`:", info.prefix);
            println!("  Owner:  {}", info.owner);
            println!("  Source: {}", info.source);
        }
        Err(e) => {
            eprintln!("hew namespace info: {e}");
            std::process::exit(1);
        }
    }
}

fn cmd_namespace_register(prefix: &str) {
    let cred_path = credentials::credentials_path();
    let Ok(token) = credentials::get_token(&cred_path) else {
        eprintln!("hew namespace register: not logged in");
        eprintln!("Run `hew login` first.");
        std::process::exit(1);
    };

    let api_client = client::RegistryClient::new().with_token(token);
    match api_client.register_namespace(prefix) {
        Ok(()) => println!("Registered namespace `{prefix}`"),
        Err(e) => {
            eprintln!("hew namespace register: {e}");
            std::process::exit(1);
        }
    }
}

fn cmd_yank(version: &str, reason: Option<&str>, undo: bool) {
    let cwd = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    let manifest_path = cwd.join("hew.toml");
    if !manifest_path.exists() {
        eprintln!("hew yank: no hew.toml found in current directory");
        std::process::exit(1);
    }
    let m = match manifest::parse_manifest(&manifest_path) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("hew yank: {e}");
            std::process::exit(1);
        }
    };

    let cred_path = credentials::credentials_path();
    let Ok(token) = credentials::get_token(&cred_path) else {
        eprintln!("hew yank: not logged in");
        std::process::exit(1);
    };

    let yanked = !undo;
    let api_client = client::RegistryClient::new().with_token(token);
    match api_client.yank(&m.package.name, version, yanked, reason) {
        Ok(()) => {
            if yanked {
                println!("Yanked {}@{}", m.package.name, version);
            } else {
                println!("Unyanked {}@{}", m.package.name, version);
            }
        }
        Err(e) => {
            eprintln!("hew yank: {e}");
            std::process::exit(1);
        }
    }
}

fn cmd_deprecate(
    package: Option<&str>,
    message: Option<&str>,
    successor: Option<&str>,
    undo: bool,
) {
    let name = if let Some(pkg) = package {
        pkg.to_string()
    } else {
        let cwd = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
        let manifest_path = cwd.join("hew.toml");
        if !manifest_path.exists() {
            eprintln!("hew deprecate: no hew.toml found and no package specified");
            std::process::exit(1);
        }
        match manifest::parse_manifest(&manifest_path) {
            Ok(m) => m.package.name,
            Err(e) => {
                eprintln!("hew deprecate: {e}");
                std::process::exit(1);
            }
        }
    };

    let cred_path = credentials::credentials_path();
    let Ok(token) = credentials::get_token(&cred_path) else {
        eprintln!("hew deprecate: not logged in");
        std::process::exit(1);
    };

    let api_client = client::RegistryClient::new().with_token(token);

    if undo {
        match api_client.set_deprecation(&name, false, None, None) {
            Ok(()) => println!("Undid deprecation of {name}"),
            Err(e) => {
                eprintln!("hew deprecate: {e}");
                std::process::exit(1);
            }
        }
    } else {
        match api_client.deprecate(&name, message, successor) {
            Ok(()) => {
                print!("Deprecated {name}");
                if let Some(succ) = successor {
                    print!(" (successor: {succ})");
                }
                println!();
            }
            Err(e) => {
                eprintln!("hew deprecate: {e}");
                std::process::exit(1);
            }
        }
    }
}

// ── index subcommand ────────────────────────────────────────────────────────

fn cmd_index_sync() {
    let index_dir = config::local_index_path();

    if index_dir.join(".git").exists() {
        // Pull latest changes.
        eprintln!("Updating local index...");
        let status = std::process::Command::new("git")
            .args(["pull", "--ff-only"])
            .current_dir(&index_dir)
            .status();
        match status {
            Ok(s) if s.success() => eprintln!("Index updated."),
            Ok(s) => {
                eprintln!(
                    "hew index sync: git pull failed (exit {})",
                    s.code().unwrap_or(-1)
                );
                std::process::exit(1);
            }
            Err(e) => {
                eprintln!("hew index sync: {e}");
                std::process::exit(1);
            }
        }
    } else {
        // Clone the index.
        eprintln!("Cloning registry index...");
        if let Some(parent) = index_dir.parent() {
            let _ = std::fs::create_dir_all(parent);
        }
        let status = std::process::Command::new("git")
            .args([
                "clone",
                "--depth=1",
                config::DEFAULT_REGISTRY_INDEX,
                &index_dir.to_string_lossy(),
            ])
            .status();
        match status {
            Ok(s) if s.success() => eprintln!("Index cloned to {}", index_dir.display()),
            Ok(s) => {
                eprintln!(
                    "hew index sync: git clone failed (exit {})",
                    s.code().unwrap_or(-1)
                );
                std::process::exit(1);
            }
            Err(e) => {
                eprintln!("hew index sync: {e}");
                std::process::exit(1);
            }
        }
    }

    if let Err(e) = index::validate_index_root(&index_dir) {
        eprintln!("hew index sync: {e}");
        std::process::exit(1);
    }
}

fn cmd_index_resolve(package: &str, version_req: &str) {
    let index_dir = config::local_index_path();
    if !index_dir.exists() {
        eprintln!("hew index resolve: local index not found");
        eprintln!("Run `hew index sync` first.");
        std::process::exit(1);
    }

    match index::resolve_from_index(&index_dir, package, version_req) {
        Ok(Some(entry)) => {
            println!("{} v{}", entry.name, entry.vers);
            println!("  checksum: {}", entry.cksum);
            if !entry.deps.is_empty() {
                println!("  dependencies:");
                for dep in &entry.deps {
                    println!("    {} {}", dep.name, dep.req);
                }
            }
        }
        Ok(None) => {
            eprintln!("No version of {package} matches {version_req}");
            std::process::exit(1);
        }
        Err(e) => {
            eprintln!("hew index resolve: {e}");
            std::process::exit(1);
        }
    }
}

fn cmd_index_list(package: &str) {
    let index_dir = config::local_index_path();
    if !index_dir.exists() {
        eprintln!("hew index list: local index not found");
        eprintln!("Run `hew index sync` first.");
        std::process::exit(1);
    }

    // Check for deprecation metadata alongside the package index file.
    let deprecation_path = index_dir.join(index::deprecation_path(package));
    if let Ok(content) = std::fs::read_to_string(&deprecation_path) {
        if let Ok(info) = serde_json::from_str::<index::DeprecationInfo>(&content) {
            if info.deprecated {
                eprint!("warning: {package} is deprecated");
                if let Some(ref msg) = info.message {
                    eprint!(" — {msg}");
                }
                if let Some(ref succ) = info.successor {
                    eprint!(" (use {succ} instead)");
                }
                eprintln!();
            }
        }
    }

    match index::read_index_entries(&index_dir, package) {
        Ok(entries) if entries.is_empty() => {
            eprintln!("No entries found for {package}");
            std::process::exit(1);
        }
        Ok(entries) => {
            println!("{package} — {} version(s):", entries.len());
            for entry in &entries {
                let status = if entry.yanked.is_tombstoned() {
                    " (tombstoned)"
                } else if entry.yanked.is_yanked() {
                    " (yanked)"
                } else {
                    ""
                };
                println!("  v{}{status}", entry.vers);
            }
        }
        Err(e) => {
            eprintln!("hew index list: {e}");
            std::process::exit(1);
        }
    }
}

// ── helpers ──────────────────────────────────────────────────────────────────

fn project_packages_path(cwd: &Path) -> PathBuf {
    cwd.join(".hew").join("packages")
}

#[derive(Debug, Clone, Copy)]
enum LocalPackageMaterialization {
    #[cfg(unix)]
    Linked,
    Copied,
}

impl LocalPackageMaterialization {
    fn verb(self) -> &'static str {
        match self {
            #[cfg(unix)]
            Self::Linked => "linked",
            Self::Copied => "copied",
        }
    }
}

fn replace_local_package_materialization(
    link: &Path,
    target: &Path,
) -> std::io::Result<LocalPackageMaterialization> {
    #[cfg(unix)]
    {
        replace_local_package_link(link, target)?;
        Ok(LocalPackageMaterialization::Linked)
    }
    #[cfg(not(unix))]
    {
        replace_local_package_copy(link, target)?;
        Ok(LocalPackageMaterialization::Copied)
    }
}

fn replace_registry_package_copy_verified(
    destination: &Path,
    source: &Path,
    expected_checksum: &str,
) -> std::io::Result<LocalPackageMaterialization> {
    if !source.is_dir() {
        return Err(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!("{} is not an installed package directory", source.display()),
        ));
    }
    let parent = destination.parent().ok_or_else(|| {
        std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            format!("{} has no parent directory", destination.display()),
        )
    })?;
    std::fs::create_dir_all(parent)?;
    let file_name = destination.file_name().ok_or_else(|| {
        std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            format!("{} has no final path component", destination.display()),
        )
    })?;
    let staged = unique_materialization_path(destination, "copy")?;
    if let Err(error) = copy_dir(source, &staged) {
        let _ = remove_path_for_replacement(&staged);
        return Err(error);
    }
    let staged_checksum = checksum::compute_dir_checksum(&staged)?;
    if staged_checksum != expected_checksum {
        let _ = remove_path_for_replacement(&staged);
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!(
                "copied registry package checksum mismatch for {}",
                file_name.to_string_lossy()
            ),
        ));
    }
    replace_directory_coherently(&staged, destination)?;
    let installed_checksum = checksum::compute_dir_checksum(destination)?;
    if installed_checksum != expected_checksum {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!(
                "materialized registry package checksum mismatch for {}",
                destination.display()
            ),
        ));
    }
    Ok(LocalPackageMaterialization::Copied)
}

fn unique_materialization_path(path: &Path, purpose: &str) -> std::io::Result<PathBuf> {
    let name = path.file_name().ok_or_else(|| {
        std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            format!("{} has no final path component", path.display()),
        )
    })?;
    for counter in 0..100_u32 {
        let candidate = path.with_file_name(format!(
            ".{}.{}-{}-{counter}",
            name.to_string_lossy(),
            purpose,
            std::process::id()
        ));
        match std::fs::symlink_metadata(&candidate) {
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
                return Ok(candidate);
            }
            Ok(_) => {}
            Err(error) => return Err(error),
        }
    }
    Err(std::io::Error::new(
        std::io::ErrorKind::AlreadyExists,
        format!("cannot allocate staging path for {}", path.display()),
    ))
}

fn replace_directory_coherently(staged: &Path, destination: &Path) -> std::io::Result<()> {
    match std::fs::symlink_metadata(destination) {
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
            return std::fs::rename(staged, destination);
        }
        Err(error) => return Err(error),
        Ok(_) => {}
    }

    let backup = unique_materialization_path(destination, "previous")?;
    std::fs::rename(destination, &backup)?;
    if let Err(error) = std::fs::rename(staged, destination) {
        let rollback = std::fs::rename(&backup, destination);
        return match rollback {
            Ok(()) => Err(error),
            Err(rollback_error) => Err(std::io::Error::other(format!(
                "cannot install {}: {error}; rollback also failed: {rollback_error}",
                destination.display()
            ))),
        };
    }
    remove_path_for_replacement(&backup)
}

#[cfg(unix)]
fn replace_local_package_link(link: &Path, target: &Path) -> std::io::Result<()> {
    if let Ok(existing) = std::fs::read_link(link) {
        if existing == target {
            return Ok(());
        }
    } else if std::fs::symlink_metadata(link).is_ok() {
        return Err(std::io::Error::other(format!(
            "{} exists and is not a symlink",
            link.display()
        )));
    }

    crate::atomic_fs::replace_symlink_atomic(link, target)
}

#[cfg_attr(
    unix,
    allow(
        dead_code,
        reason = "portable install fallback is selected on non-Unix and unit-tested on Unix"
    )
)]
fn replace_local_package_copy(link: &Path, target: &Path) -> std::io::Result<()> {
    if !target.is_dir() {
        return Err(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!("{} is not an installed package directory", target.display()),
        ));
    }
    let parent = link.parent().ok_or_else(|| {
        std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            format!("{} has no parent directory", link.display()),
        )
    })?;
    std::fs::create_dir_all(parent)?;

    let file_name = link.file_name().ok_or_else(|| {
        std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            format!("{} has no final path component", link.display()),
        )
    })?;
    let temp = link.with_file_name(format!(
        ".{}.tmp-{}",
        file_name.to_string_lossy(),
        std::process::id()
    ));
    if std::fs::symlink_metadata(&temp).is_ok() {
        remove_path_for_replacement(&temp)?;
    }

    if let Err(err) = copy_dir(target, &temp) {
        let _ = remove_path_for_replacement(&temp);
        return Err(err);
    }
    if std::fs::symlink_metadata(link).is_ok() {
        remove_path_for_replacement(link)?;
    }
    std::fs::rename(temp, link)
}

#[cfg_attr(
    unix,
    allow(
        dead_code,
        reason = "portable install fallback is selected on non-Unix and unit-tested on Unix"
    )
)]
fn remove_path_for_replacement(path: &Path) -> std::io::Result<()> {
    let metadata = std::fs::symlink_metadata(path)?;
    if metadata.file_type().is_symlink() || metadata.is_file() {
        std::fs::remove_file(path)
    } else if metadata.is_dir() {
        std::fs::remove_dir_all(path)
    } else {
        Err(std::io::Error::other(format!(
            "{} is neither a file nor a directory",
            path.display()
        )))
    }
}

/// Find the latest (highest semver) version of `package_name` in the registry.
fn find_latest_version(
    packages: &[registry::InstalledPackage],
    package_name: &str,
) -> Option<String> {
    packages
        .iter()
        .filter(|p| p.name == package_name)
        .filter_map(|p| semver::Version::parse(&p.version).ok())
        .max()
        .map(|v| v.to_string())
}

/// Recursively copy `src` to `dst`, skipping `.git`, `target`, and `.hew`.
///
/// # Errors
///
/// Returns an `std::io::Error` when any file or directory operation fails.
fn copy_dir(src: &Path, dst: &Path) -> std::io::Result<()> {
    std::fs::create_dir_all(dst)?;
    for entry in std::fs::read_dir(src)? {
        let entry = entry?;
        let name = entry.file_name();
        let name_str = name.to_string_lossy();
        if package_fs::is_skipped_package_dir(&name_str) {
            continue;
        }
        let src_path = entry.path();
        let dst_path = dst.join(&name);
        if src_path.is_dir() {
            copy_dir(&src_path, &dst_path)?;
        } else {
            std::fs::copy(&src_path, &dst_path)?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::publish::publish_local_package;

    #[test]
    fn fetch_progress_rejects_repeated_non_progress() {
        let failures = vec![resolver::UnresolvedDep {
            package: "foo".to_string(),
            requirements: vec!["1.0.0".to_string()],
            registry: config::default_registry_identity(),
            registry_selector: None,
        }];
        let mut progress = FetchProgress::default();

        progress.begin_round(&failures).unwrap();
        let error = progress.begin_round(&failures).unwrap_err();

        assert!(error.contains("repeated without making resolver progress"));
        assert_eq!(progress.rounds, 1);
    }

    #[test]
    fn ensure_gitignore_entry_creates_file() {
        let dir = tempfile::tempdir().unwrap();
        ensure_gitignore_entry(dir.path(), ".hew/");
        let contents = std::fs::read_to_string(dir.path().join(".gitignore")).unwrap();
        assert!(contents.contains(".hew/"));
    }

    #[test]
    fn ensure_gitignore_entry_no_duplicate() {
        let dir = tempfile::tempdir().unwrap();
        ensure_gitignore_entry(dir.path(), ".hew/");
        ensure_gitignore_entry(dir.path(), ".hew/");
        let contents = std::fs::read_to_string(dir.path().join(".gitignore")).unwrap();
        assert_eq!(contents.matches(".hew/").count(), 1);
    }

    #[test]
    fn ensure_gitignore_entry_appends_to_existing() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join(".gitignore"), "target/\n").unwrap();
        ensure_gitignore_entry(dir.path(), ".hew/");
        let contents = std::fs::read_to_string(dir.path().join(".gitignore")).unwrap();
        assert!(contents.contains("target/"));
        assert!(contents.contains(".hew/"));
    }

    #[test]
    fn ensure_gitignore_entry_appends_after_missing_trailing_newline() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join(".gitignore"), "target/").unwrap();
        ensure_gitignore_entry(dir.path(), ".hew/");
        let contents = std::fs::read_to_string(dir.path().join(".gitignore")).unwrap();
        assert_eq!(contents, "target/\n.hew/\n");
    }

    #[test]
    fn ensure_gitignore_entry_matches_trimmed_existing_line() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join(".gitignore"), ".hew/   \n").unwrap();
        ensure_gitignore_entry(dir.path(), ".hew/");
        let contents = std::fs::read_to_string(dir.path().join(".gitignore")).unwrap();
        assert_eq!(contents.matches(".hew/").count(), 1);
    }

    #[test]
    fn ensure_gitignore_entry_leaves_non_utf8_file_untouched() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join(".gitignore");
        let original_bytes: &[u8] = b"target/\n\xff\xfe invalid utf8 \xc0\xc0\n";
        std::fs::write(&path, original_bytes).unwrap();
        ensure_gitignore_entry(dir.path(), ".hew/");
        let bytes_after = std::fs::read(&path).unwrap();
        assert_eq!(bytes_after, original_bytes);
    }

    #[test]
    fn copy_dir_copies_files() {
        let src = tempfile::tempdir().unwrap();
        let dst = tempfile::tempdir().unwrap();
        std::fs::write(
            src.path().join("hew.toml"),
            "[package]\nname=\"x\"\nversion=\"1\"\n",
        )
        .unwrap();
        std::fs::write(src.path().join("main.hew"), "// hello").unwrap();
        copy_dir(src.path(), dst.path()).unwrap();
        assert!(dst.path().join("hew.toml").exists());
        assert!(dst.path().join("main.hew").exists());
    }

    #[test]
    fn copy_dir_skips_git_and_target() {
        let src = tempfile::tempdir().unwrap();
        let dst = tempfile::tempdir().unwrap();
        std::fs::create_dir(src.path().join(".git")).unwrap();
        std::fs::create_dir(src.path().join("target")).unwrap();
        std::fs::write(
            src.path().join("hew.toml"),
            "[package]\nname=\"x\"\nversion=\"1\"\n",
        )
        .unwrap();
        copy_dir(src.path(), dst.path()).unwrap();
        assert!(!dst.path().join(".git").exists());
        assert!(!dst.path().join("target").exists());
        assert!(dst.path().join("hew.toml").exists());
    }

    #[test]
    fn replace_local_package_copy_replaces_existing_tree() {
        let src = tempfile::tempdir().unwrap();
        std::fs::write(
            src.path().join("hew.toml"),
            "[package]\nname=\"x\"\nversion=\"0.2.0\"\n",
        )
        .unwrap();
        std::fs::write(src.path().join("x.hew"), "pub fn answer() -> i64 { 2 }").unwrap();

        let project = tempfile::tempdir().unwrap();
        let link = project.path().join(".hew").join("packages").join("x");
        std::fs::create_dir_all(&link).unwrap();
        std::fs::write(link.join("stale.hew"), "old").unwrap();

        replace_local_package_copy(&link, src.path()).unwrap();

        assert_eq!(
            std::fs::read_to_string(link.join("x.hew")).unwrap(),
            "pub fn answer() -> i64 { 2 }"
        );
        assert!(!link.join("stale.hew").exists());
    }

    #[test]
    fn registry_materialization_is_a_verified_copy() {
        let src = tempfile::tempdir().unwrap();
        std::fs::write(
            src.path().join("hew.toml"),
            "[package]\nname=\"x\"\nversion=\"0.2.0\"\n",
        )
        .unwrap();
        std::fs::write(src.path().join("x.hew"), "verified").unwrap();
        let expected = checksum::compute_dir_checksum(src.path()).unwrap();

        let project = tempfile::tempdir().unwrap();
        let destination = project.path().join(".hew/packages/x");
        std::fs::create_dir_all(&destination).unwrap();
        std::fs::write(destination.join("stale.hew"), "old").unwrap();

        replace_registry_package_copy_verified(&destination, src.path(), &expected).unwrap();

        assert!(!std::fs::symlink_metadata(&destination)
            .unwrap()
            .file_type()
            .is_symlink());
        assert_eq!(
            checksum::compute_dir_checksum(&destination).unwrap(),
            expected
        );
        assert!(!destination.join("stale.hew").exists());
    }

    #[test]
    fn registry_copy_mismatch_does_not_mutate_project() {
        let src = tempfile::tempdir().unwrap();
        std::fs::write(
            src.path().join("hew.toml"),
            "[package]\nname=\"x\"\nversion=\"0.2.0\"\n",
        )
        .unwrap();
        let project = tempfile::tempdir().unwrap();
        let destination = project.path().join(".hew/packages/x");
        std::fs::create_dir_all(&destination).unwrap();
        std::fs::write(destination.join("old.hew"), "old").unwrap();

        let error = replace_registry_package_copy_verified(
            &destination,
            src.path(),
            "sha256:0000000000000000000000000000000000000000000000000000000000000000",
        )
        .unwrap_err();

        assert_eq!(error.kind(), std::io::ErrorKind::InvalidData);
        assert_eq!(
            std::fs::read_to_string(destination.join("old.hew")).unwrap(),
            "old"
        );
    }

    #[test]
    fn publish_copies_to_registry() {
        let src = tempfile::tempdir().unwrap();
        std::fs::write(
            src.path().join("hew.toml"),
            "[package]\nname = \"mypkg\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        std::fs::write(src.path().join("main.hew"), "// hello").unwrap();

        let reg_dir = tempfile::tempdir().unwrap();
        let reg = registry::Registry::with_root(reg_dir.path().to_path_buf());

        let m = manifest::parse_manifest(&src.path().join("hew.toml")).unwrap();
        let dest = reg.package_dir(&m.package.name, &m.package.version);
        copy_dir(src.path(), &dest).unwrap();

        assert!(dest.join("hew.toml").exists());
        assert!(dest.join("main.hew").exists());
    }

    #[test]
    #[expect(
        clippy::too_many_lines,
        reason = "security regression keeps verification, concurrent swap, graph, and materialization in one proof"
    )]
    fn locked_resolution_and_materialization_keep_verified_generation_after_pointer_swap() {
        use std::sync::atomic::{AtomicBool, Ordering};
        use std::sync::{Arc, Barrier};

        let root = tempfile::tempdir().unwrap();
        let registry_root = root.path().join("registry");
        let registry = registry::Registry::with_root(registry_root.clone());
        let registry_id = config::default_registry_identity();
        let source_root = registry.source_root(&registry_id);
        let package_root = source_root.join("foo");
        let generation_a = package_root.join(".1.0.0.generation-aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa");
        let generation_b = package_root.join(".1.0.0.generation-bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb");
        let bar = source_root.join("bar/1.0.0");
        std::fs::create_dir_all(&generation_a).unwrap();
        std::fs::create_dir_all(&generation_b).unwrap();
        std::fs::create_dir_all(&bar).unwrap();
        std::fs::write(
            generation_a.join("hew.toml"),
            "[package]\nname = \"foo\"\nversion = \"1.0.0\"\n\n[dependencies]\nbar = \"1.0.0\"\n",
        )
        .unwrap();
        std::fs::write(generation_a.join("foo.hew"), "generation A").unwrap();
        std::fs::write(
            generation_b.join("hew.toml"),
            "[package]\nname = \"foo\"\nversion = \"1.0.0\"\n\n[dependencies]\nbaz = \"1.0.0\"\n",
        )
        .unwrap();
        std::fs::write(generation_b.join("foo.hew"), "generation B").unwrap();
        std::fs::write(
            bar.join("hew.toml"),
            "[package]\nname = \"bar\"\nversion = \"1.0.0\"\n",
        )
        .unwrap();
        std::fs::write(
            package_root.join(".1.0.0.current"),
            format!("{}\n", generation_a.file_name().unwrap().to_string_lossy()),
        )
        .unwrap();
        std::fs::write(
            package_root.join(".1.0.0.generation-aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.lease"),
            [],
        )
        .unwrap();
        std::fs::write(package_root.join(".1.0.0.slot.lock"), []).unwrap();

        let lockfile = lockfile::LockFile {
            packages: vec![
                lockfile::LockedPackage {
                    name: "foo".to_string(),
                    requirement: Some("1.0.0".to_string()),
                    version: "1.0.0".to_string(),
                    checksum: Some(checksum::compute_dir_checksum(&generation_a).unwrap()),
                    signature: None,
                    native: Vec::new(),
                    source: "registry".to_string(),
                    registry: Some(registry_id.clone()),
                    path: None,
                },
                lockfile::LockedPackage {
                    name: "bar".to_string(),
                    requirement: None,
                    version: "1.0.0".to_string(),
                    checksum: Some(checksum::compute_dir_checksum(&bar).unwrap()),
                    signature: None,
                    native: Vec::new(),
                    source: "registry".to_string(),
                    registry: Some(registry_id.clone()),
                    path: None,
                },
            ],
        };
        let pinned = verify_locked_registry_packages(&lockfile, &registry).unwrap();

        let ready = Arc::new(Barrier::new(2));
        let writer_ready = Arc::clone(&ready);
        let stop = Arc::new(AtomicBool::new(false));
        let writer_stop = Arc::clone(&stop);
        let pointer = package_root.join(".1.0.0.current");
        let generation_b_name = generation_b.file_name().unwrap().to_owned();
        let writer = std::thread::spawn(move || {
            let pointer_b = format!("{}\n", generation_b_name.to_string_lossy());
            crate::atomic_fs::write_atomic(&pointer, pointer_b.as_bytes(), 0o644).unwrap();
            writer_ready.wait();
            while !writer_stop.load(Ordering::Relaxed) {
                crate::atomic_fs::write_atomic(&pointer, pointer_b.as_bytes(), 0o644).unwrap();
                std::thread::yield_now();
            }
        });
        ready.wait();
        assert_eq!(
            registry.package_dir_for(&registry_id, "foo", "1.0.0"),
            generation_b
        );

        let project = root.path().join("project");
        std::fs::create_dir_all(&project).unwrap();
        std::fs::write(
            project.join("hew.toml"),
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n[dependencies]\nfoo = \"1.0.0\"\n",
        )
        .unwrap();
        let manifest = manifest::parse_manifest(&project.join("hew.toml")).unwrap();
        let pinned_paths = pinned
            .iter()
            .map(|(identity, package)| (identity.clone(), package.path().to_path_buf()))
            .collect();
        let sources = resolver::RegistrySources::default_source();
        let resolved = resolver::resolve_all_pinned_with_sources(
            &manifest,
            &project,
            &registry,
            &sources,
            &pinned_paths,
        )
        .unwrap();
        assert!(resolved.contains_key("bar"));
        assert!(!resolved.contains_key("baz"));

        let link = project.join(".hew/packages/foo");
        let verified = pinned
            .get(&(registry_id, "foo".to_string(), "1.0.0".to_string()))
            .unwrap();
        replace_registry_package_copy_verified(&link, verified.path(), &verified.checksum).unwrap();
        assert_eq!(
            std::fs::read_to_string(link.join("foo.hew")).unwrap(),
            "generation A"
        );
        stop.store(true, Ordering::Relaxed);
        writer.join().unwrap();
    }

    /// `hew publish --local` followed by `hew install --locked` with a fixed
    /// (hand-written, checked-in-style) lockfile: the locally published
    /// package must resolve directly from `--locked` verification, with no
    /// registry contact and no lockfile rewrite. Regression coverage for
    /// hew-lang/hew#3233 (`publish --local` used to write the legacy
    /// unnamespaced layout, which `--locked`'s per-registry generation
    /// lookup never reads).
    #[test]
    fn publish_local_then_install_locked_round_trips() {
        let root = tempfile::tempdir().unwrap();
        let registry = registry::Registry::with_root(root.path().join("registry"));
        let registry_id = config::default_registry_identity();

        // `hew publish --local`: pack a tiny package and publish it into the
        // local registry only, exactly as `cmd_publish`'s `--local` branch does.
        let pkg_src = tempfile::tempdir().unwrap();
        std::fs::write(
            pkg_src.path().join("hew.toml"),
            "[package]\nname = \"foo\"\nversion = \"1.0.0\"\n",
        )
        .unwrap();
        std::fs::write(
            pkg_src.path().join("foo.hew"),
            "pub fn value() -> i64 { 1 }\n",
        )
        .unwrap();
        let archive = tarball::pack(pkg_src.path(), &[], &[]).unwrap();
        let published = publish_local_package(&registry, "foo", "1.0.0", &archive.data).unwrap();
        assert!(published.path().join("hew.toml").is_file());
        // The publish lands in the namespaced slot `--locked` reads, not the
        // legacy unnamespaced layout.
        assert_eq!(
            published.path(),
            registry.package_dir_for(&registry_id, "foo", "1.0.0")
        );

        // `hew install --locked`: a fixed lockfile naming the just-published
        // package and its default-registry identity.
        let checksum = checksum::compute_dir_checksum(published.path()).unwrap();
        let lockfile = lockfile::LockFile {
            packages: vec![lockfile::LockedPackage {
                name: "foo".to_string(),
                requirement: Some("1.0.0".to_string()),
                version: "1.0.0".to_string(),
                checksum: Some(checksum),
                signature: None,
                native: Vec::new(),
                source: "registry".to_string(),
                registry: Some(registry_id.clone()),
                path: None,
            }],
        };
        let pinned = verify_locked_registry_packages(&lockfile, &registry)
            .expect("--locked must resolve the locally published package directly");

        let project = tempfile::tempdir().unwrap();
        std::fs::write(
            project.path().join("hew.toml"),
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n[dependencies]\nfoo = \"1.0.0\"\n",
        )
        .unwrap();
        let manifest = manifest::parse_manifest(&project.path().join("hew.toml")).unwrap();
        let pinned_paths = pinned
            .iter()
            .map(|(identity, package)| (identity.clone(), package.path().to_path_buf()))
            .collect();
        let sources = resolver::RegistrySources::default_source();
        let resolved = resolver::resolve_all_pinned_with_sources(
            &manifest,
            project.path(),
            &registry,
            &sources,
            &pinned_paths,
        )
        .unwrap();
        assert!(resolved.contains_key("foo"));

        let link = project.path().join(".hew/packages/foo");
        let verified = pinned
            .get(&(registry_id, "foo".to_string(), "1.0.0".to_string()))
            .unwrap();
        replace_registry_package_copy_verified(&link, verified.path(), &verified.checksum).unwrap();
        assert_eq!(
            std::fs::read_to_string(link.join("foo.hew")).unwrap(),
            "pub fn value() -> i64 { 1 }\n"
        );

        // Negative control: the legacy unnamespaced layout (what `--local`
        // used to write before this fix) is a different path than where
        // `--locked` resolves from, and nothing was published there.
        assert!(!registry
            .package_dir("foo", "1.0.0")
            .join("hew.toml")
            .is_file());
        assert_ne!(
            registry.package_dir("foo", "1.0.0"),
            registry.package_dir_for(&config::default_registry_identity(), "foo", "1.0.0")
        );
    }

    /// A local publish must not silently overwrite a package the same slot
    /// already holds from a *verified* registry fetch (one
    /// `Registry::write_cache_metadata` populated) with different content:
    /// that would make a later `--offline`/`--locked` resolution trust a
    /// local test build as if it were the real registry's package. A local
    /// publish over a *prior local* publish (no cache metadata) still
    /// succeeds — that is routine local development, not a collision.
    #[test]
    fn publish_local_refuses_to_shadow_a_verified_registry_fetch() {
        let root = tempfile::tempdir().unwrap();
        let registry = registry::Registry::with_root(root.path().join("registry"));
        let registry_id = config::default_registry_identity();

        let pkg = |value: &str| {
            let src = tempfile::tempdir().unwrap();
            std::fs::write(
                src.path().join("hew.toml"),
                "[package]\nname = \"foo\"\nversion = \"1.0.0\"\n",
            )
            .unwrap();
            std::fs::write(
                src.path().join("foo.hew"),
                format!("pub fn value() -> i64 {{ {value} }}\n"),
            )
            .unwrap();
            let archive = tarball::pack(src.path(), &[], &[]).unwrap();
            (src, archive)
        };

        // Seed the slot as a verified registry fetch would leave it: unpacked
        // content plus the cache metadata sidecar `write_cache_metadata` writes.
        let (_src, real) = pkg("1");
        let slot = registry.package_slot_for(&registry_id, "foo", "1.0.0");
        let staged = crate::atomic_fs::StagedDir::new(&slot).unwrap();
        tarball::unpack(&real.data, staged.path()).unwrap();
        let published = staged.publish_pinned(&slot).unwrap();
        registry::Registry::write_cache_metadata(
            published.path(),
            &registry_id,
            "foo",
            "1.0.0",
            &real.checksum,
            &real.data,
        )
        .unwrap();

        // A local publish with DIFFERENT content for the same name@version
        // must be refused, not silently overwrite the verified fetch.
        let (_src2, different) = pkg("2");
        let error = publish_local_package(&registry, "foo", "1.0.0", &different.data)
            .expect_err("must refuse to shadow a verified registry fetch");
        assert!(
            error.to_string().contains("verified registry fetch"),
            "unexpected error: {error}"
        );
        assert_eq!(
            std::fs::read_to_string(
                registry
                    .package_dir_for(&registry_id, "foo", "1.0.0")
                    .join("foo.hew")
            )
            .unwrap(),
            "pub fn value() -> i64 { 1 }\n",
            "the verified fetch must be untouched after the refused publish"
        );

        // A local publish with the SAME content is idempotent, not a collision.
        let (_src3, same) = pkg("1");
        publish_local_package(&registry, "foo", "1.0.0", &same.data)
            .expect("identical content is not a collision");

        // A local publish over a PRIOR LOCAL publish (no cache metadata) is
        // routine local development and must still succeed even when content
        // changes — matching `tests/package-install/run.sh`'s republish case.
        let bar_pkg = |value: &str| {
            let src = tempfile::tempdir().unwrap();
            std::fs::write(
                src.path().join("hew.toml"),
                "[package]\nname = \"bar\"\nversion = \"1.0.0\"\n",
            )
            .unwrap();
            std::fs::write(
                src.path().join("bar.hew"),
                format!("pub fn value() -> i64 {{ {value} }}\n"),
            )
            .unwrap();
            tarball::pack(src.path(), &[], &[]).unwrap()
        };
        publish_local_package(&registry, "bar", "1.0.0", &bar_pkg("1").data).unwrap();
        publish_local_package(&registry, "bar", "1.0.0", &bar_pkg("2").data)
            .expect("republishing over a prior local publish is not a collision");
        assert_eq!(
            std::fs::read_to_string(
                registry
                    .package_dir_for(&registry_id, "bar", "1.0.0")
                    .join("bar.hew")
            )
            .unwrap(),
            "pub fn value() -> i64 { 2 }\n"
        );
    }

    /// An idempotent same-content local publish over a *verified* registry
    /// fetch must not drop that fetch's cache metadata or retained archive.
    /// `StagedDir::publish_pinned` always swaps in a fresh generation holding
    /// only the unpacked tree, so a naive "checksums match, allow it" guard
    /// still restages and silently loses the metadata sidecar; a later
    /// different-content publish would then find `verified_tree_checksum ==
    /// None` and shadow the package the registry actually served.
    #[test]
    fn publish_local_idempotent_publish_preserves_verified_metadata() {
        let root = tempfile::tempdir().unwrap();
        let registry = registry::Registry::with_root(root.path().join("registry"));
        let registry_id = config::default_registry_identity();

        let pkg = |value: &str| {
            let src = tempfile::tempdir().unwrap();
            std::fs::write(
                src.path().join("hew.toml"),
                "[package]\nname = \"foo\"\nversion = \"1.0.0\"\n",
            )
            .unwrap();
            std::fs::write(
                src.path().join("foo.hew"),
                format!("pub fn value() -> i64 {{ {value} }}\n"),
            )
            .unwrap();
            let archive = tarball::pack(src.path(), &[], &[]).unwrap();
            (src, archive)
        };

        // Seed the slot as a verified registry fetch would leave it: unpacked
        // content plus the cache metadata sidecar `write_cache_metadata` writes.
        let (_src, real) = pkg("1");
        let slot = registry.package_slot_for(&registry_id, "foo", "1.0.0");
        let staged = crate::atomic_fs::StagedDir::new(&slot).unwrap();
        tarball::unpack(&real.data, staged.path()).unwrap();
        let published = staged.publish_pinned(&slot).unwrap();
        registry::Registry::write_cache_metadata(
            published.path(),
            &registry_id,
            "foo",
            "1.0.0",
            &real.checksum,
            &real.data,
        )
        .unwrap();

        // A local publish with the SAME content is idempotent, not a collision.
        let (_src2, same) = pkg("1");
        publish_local_package(&registry, "foo", "1.0.0", &same.data)
            .expect("identical content is not a collision");

        // The idempotent publish must not have dropped the verified fetch's
        // cache metadata: the slot must still read back with the same tree
        // checksum, not silently downgraded to a plain, unverified local
        // publish that a later different-content publish could shadow.
        let expected_tree_checksum = crate::tarball::unpacked_tree_checksum(&real.data).unwrap();
        assert_eq!(
            registry::Registry::verified_tree_checksum(&registry.package_dir_for(
                &registry_id,
                "foo",
                "1.0.0"
            )),
            Some(expected_tree_checksum),
            "idempotent same-content publish must not disturb the verified metadata"
        );
        assert!(
            registry
                .verified_online_cache_entry(&registry_id, "foo", "1.0.0", &real.checksum)
                .unwrap()
                .is_some(),
            "the retained archive must also survive, or the next online install redownloads"
        );

        // With the verified metadata intact, a later local publish with
        // DIFFERENT content must still be refused — the guard the idempotent
        // path above must not have silently disarmed it.
        let (_src3, different_again) = pkg("3");
        let error = publish_local_package(&registry, "foo", "1.0.0", &different_again.data)
            .expect_err(
                "verified metadata surviving the idempotent publish must still guard the slot",
            );
        assert!(
            error.to_string().contains("verified registry fetch"),
            "unexpected error: {error}"
        );
    }

    #[test]
    fn locked_cache_from_wrong_registry_is_rejected() {
        let root = tempfile::tempdir().unwrap();
        let registry = registry::Registry::with_root(root.path().to_path_buf());
        let first = "https://registry-a.example/api/v1";
        let second = "https://registry-b.example/api/v1";
        let package = registry.package_dir_for(first, "foo", "1.0.0");
        std::fs::create_dir_all(&package).unwrap();
        std::fs::write(
            package.join("hew.toml"),
            "[package]\nname = \"foo\"\nversion = \"1.0.0\"\n",
        )
        .unwrap();
        let checksum = checksum::compute_dir_checksum(&package).unwrap();
        let lockfile = lockfile::LockFile {
            packages: vec![lockfile::LockedPackage {
                name: "foo".to_string(),
                requirement: Some("1.0.0".to_string()),
                version: "1.0.0".to_string(),
                checksum: Some(checksum),
                signature: None,
                native: Vec::new(),
                source: "registry".to_string(),
                registry: Some(second.to_string()),
                path: None,
            }],
        };

        let error = verify_locked_registry_packages(&lockfile, &registry).unwrap_err();
        assert!(error.contains("missing from cache"), "{error}");
    }

    #[test]
    fn resolve_registry_token_default_registry_missing_is_not_logged_in() {
        let dir = tempfile::tempdir().unwrap();
        let cred_path = dir.path().join("credentials.toml");
        let result = resolve_registry_token(&cred_path, None);
        assert!(matches!(
            result,
            Err(credentials::CredentialError::NotLoggedIn)
        ));
    }

    #[test]
    fn resolve_registry_token_named_registry_requires_named_token() {
        let dir = tempfile::tempdir().unwrap();
        let cred_path = dir.path().join("credentials.toml");
        credentials::save_credentials(
            &cred_path,
            &credentials::CredentialsFile {
                registry: Some(credentials::Credentials {
                    token: "tok_default".to_string(),
                    github_user: None,
                }),
                registries: None,
            },
        )
        .unwrap();

        let result = resolve_registry_token(&cred_path, Some("testreg"));
        assert!(matches!(
            result,
            Err(credentials::CredentialError::NotLoggedIn)
        ));
    }

    #[test]
    fn resolve_registry_token_named_registry_returns_named_token() {
        let dir = tempfile::tempdir().unwrap();
        let cred_path = dir.path().join("credentials.toml");
        let mut registries = std::collections::BTreeMap::new();
        registries.insert(
            "testreg".to_string(),
            credentials::Credentials {
                token: "tok_testreg".to_string(),
                github_user: None,
            },
        );
        credentials::save_credentials(
            &cred_path,
            &credentials::CredentialsFile {
                registry: None,
                registries: Some(registries),
            },
        )
        .unwrap();

        let token = resolve_registry_token(&cred_path, Some("testreg")).unwrap();
        assert_eq!(token, "tok_testreg");
    }

    /// A corrupt `credentials.toml` must surface as `CredentialError::Parse`,
    /// not be collapsed into `NotLoggedIn` — the two point a user at
    /// different remedies (`hew login` cannot fix a parse error, since
    /// `hew login` also reads through this same file).
    #[test]
    fn resolve_registry_token_corrupt_credentials_file_is_parse_error() {
        let dir = tempfile::tempdir().unwrap();
        let cred_path = dir.path().join("credentials.toml");
        std::fs::write(&cred_path, "this is not valid toml {{{").unwrap();

        let result = resolve_registry_token(&cred_path, None);
        assert!(
            matches!(result, Err(credentials::CredentialError::Parse(_))),
            "expected Parse error, got {result:?}"
        );

        let result = resolve_registry_token(&cred_path, Some("testreg"));
        assert!(
            matches!(result, Err(credentials::CredentialError::Parse(_))),
            "expected Parse error, got {result:?}"
        );
    }

    // ── search tests ────────────────────────────────────────────────────

    fn setup_registry() -> (tempfile::TempDir, registry::Registry) {
        let dir = tempfile::tempdir().unwrap();
        let reg = registry::Registry::with_root(dir.path().to_path_buf());
        (dir, reg)
    }

    fn install_fake(registry: &registry::Registry, name: &str, version: &str) {
        let dir = registry.package_dir(name, version);
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(
            dir.join("hew.toml"),
            format!("[package]\nname = \"{name}\"\nversion = \"{version}\"\n"),
        )
        .unwrap();
    }

    fn install_fake_with_deps(
        registry: &registry::Registry,
        name: &str,
        version: &str,
        deps: &[(&str, &str)],
    ) {
        let dir = registry.package_dir(name, version);
        std::fs::create_dir_all(&dir).unwrap();
        let mut content =
            format!("[package]\nname = \"{name}\"\nversion = \"{version}\"\n\n[dependencies]\n");
        for (dep_name, dep_ver) in deps {
            use std::fmt::Write;
            let _ = writeln!(content, "\"{dep_name}\" = \"{dep_ver}\"");
        }
        std::fs::write(dir.join("hew.toml"), content).unwrap();
    }

    #[test]
    fn is_valid_package_name_accepts_simple() {
        assert!(is_valid_package_name("mypackage"));
        assert!(is_valid_package_name("my_package"));
        assert!(is_valid_package_name("std.net.http"));
    }

    #[test]
    fn is_valid_package_name_rejects_invalid() {
        assert!(!is_valid_package_name(""));
        assert!(!is_valid_package_name("my package"));
        assert!(!is_valid_package_name("my@package"));
        assert!(!is_valid_package_name("my:package")); // single colon
        assert!(!is_valid_package_name("my/package"));
        assert!(!is_valid_package_name("evil.../../../tmp/pwned"));
        assert!(!is_valid_package_name("evil./tmp/pwned"));
    }

    #[test]
    fn find_latest_version_picks_highest() {
        let (_dir, reg) = setup_registry();
        install_fake(&reg, "mypkg", "1.0.0");
        install_fake(&reg, "mypkg", "2.0.0");
        install_fake(&reg, "mypkg", "1.5.0");

        let pkgs = reg.list_packages();
        let latest = find_latest_version(&pkgs, "mypkg");
        assert_eq!(latest.as_deref(), Some("2.0.0"));
    }

    #[test]
    fn find_latest_version_returns_none_for_missing() {
        let (_dir, reg) = setup_registry();
        let pkgs = reg.list_packages();
        assert!(find_latest_version(&pkgs, "nonexistent").is_none());
    }

    #[test]
    fn search_finds_matching_packages() {
        let (_dir, reg) = setup_registry();
        install_fake(&reg, "std.net.http", "1.0.0");
        install_fake(&reg, "std.net.websocket", "1.0.0");
        install_fake(&reg, "ecosystem.db.postgres", "1.0.0");

        let pkgs = reg.list_packages();
        let query_lower = "net";
        let matches: Vec<_> = pkgs
            .iter()
            .filter(|p| p.name.to_lowercase().contains(query_lower))
            .collect();
        assert_eq!(matches.len(), 2);
    }

    #[test]
    fn search_case_insensitive() {
        let (_dir, reg) = setup_registry();
        install_fake(&reg, "MyPackage", "1.0.0");

        let pkgs = reg.list_packages();
        let query_lower = "mypackage";
        let matches: Vec<_> = pkgs
            .iter()
            .filter(|p| p.name.to_lowercase().contains(query_lower))
            .collect();
        assert_eq!(matches.len(), 1);
    }

    #[test]
    fn tree_dep_resolution() {
        let (_dir, reg) = setup_registry();
        install_fake_with_deps(&reg, "std.net.http", "1.0.0", &[("hew.net.tcp", "1.0.0")]);
        install_fake(&reg, "hew.net.tcp", "1.0.0");

        let dep_toml = reg.package_dir("std.net.http", "1.0.0").join("hew.toml");
        let dep_m = manifest::parse_manifest(&dep_toml).unwrap();
        assert_eq!(dep_m.dependencies.len(), 1);
        assert!(dep_m.dependencies.contains_key("hew.net.tcp"));
    }

    #[test]
    fn remove_dependency_from_manifest() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("hew.toml");
        manifest::write_default_manifest(&path, "myproject").unwrap();
        manifest::add_dependency(&path, "std.net.http", "1.0", None).unwrap();

        let removed = manifest::remove_dependency(&path, "std.net.http").unwrap();
        assert!(removed);

        let m = manifest::parse_manifest(&path).unwrap();
        assert!(!m.dependencies.contains_key("std.net.http"));
    }

    #[test]
    fn remove_nonexistent_dep_returns_false() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("hew.toml");
        manifest::write_default_manifest(&path, "myproject").unwrap();

        let removed = manifest::remove_dependency(&path, "nonexistent").unwrap();
        assert!(!removed);
    }

    #[test]
    fn check_valid_manifest() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("hew.toml");
        std::fs::write(
            &path,
            "[package]\nname = \"valid_pkg\"\nversion = \"1.0.0\"\n\n[dependencies]\n",
        )
        .unwrap();
        let m = manifest::parse_manifest(&path).unwrap();

        assert!(is_valid_package_name(&m.package.name));
        assert!(semver::Version::parse(&m.package.version).is_ok());
    }

    #[test]
    fn check_invalid_name() {
        assert!(!is_valid_package_name("bad name!"));
    }

    #[test]
    fn check_invalid_version() {
        assert!(semver::Version::parse("not.semver").is_err());
    }

    #[test]
    fn outdated_detects_old_version() {
        let (_dir, reg) = setup_registry();
        install_fake(&reg, "mypkg", "1.0.0");
        install_fake(&reg, "mypkg", "2.0.0");

        let pkgs = reg.list_packages();
        let latest = find_latest_version(&pkgs, "mypkg").unwrap();
        assert_eq!(latest, "2.0.0");
        assert_ne!("1.0.0", latest);
    }

    #[test]
    fn update_changes_version_in_manifest() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("hew.toml");
        manifest::write_default_manifest(&path, "myproject").unwrap();
        manifest::add_dependency(&path, "mypkg", "1.0.0", None).unwrap();

        let (_reg_dir, reg) = setup_registry();
        install_fake(&reg, "mypkg", "1.0.0");
        install_fake(&reg, "mypkg", "2.0.0");

        let mut m = manifest::parse_manifest(&path).unwrap();
        let pkgs = reg.list_packages();
        if let Some(latest) = find_latest_version(&pkgs, "mypkg") {
            m.dependencies
                .insert("mypkg".to_string(), manifest::DepSpec::Version(latest));
        }
        manifest::save_manifest(&path, &m).unwrap();

        let m = manifest::parse_manifest(&path).unwrap();
        assert_eq!(m.dependencies["mypkg"].version_req(), "2.0.0");
    }

    // ── init template tests ────────────────────────────────────────────

    #[test]
    fn init_bin_creates_main_hew() {
        let dir = tempfile::tempdir().unwrap();
        let manifest_path = dir.path().join("hew.toml");
        manifest::write_manifest_with_template(
            &manifest_path,
            "binproj",
            manifest::ManifestTemplate::Bin,
        )
        .unwrap();
        write_template_source(dir.path(), "binproj", manifest::ManifestTemplate::Bin);
        write_init_gitignore(dir.path());

        assert!(dir.path().join("main.hew").exists());
        let src = std::fs::read_to_string(dir.path().join("main.hew")).unwrap();
        assert!(
            src.contains("fn main()"),
            "main.hew should contain fn main()"
        );
        assert!(
            src.contains("Hello from binproj!"),
            "main.hew should greet with project name"
        );

        let m = manifest::parse_manifest(&manifest_path).unwrap();
        assert_eq!(
            m.package.description.as_deref(),
            Some("A Hew binary project")
        );

        let gi = std::fs::read_to_string(dir.path().join(".gitignore")).unwrap();
        assert!(gi.contains("target/"));
        assert!(gi.contains(".hew/"));
    }

    #[test]
    fn init_lib_creates_canonical_package_module() {
        let dir = tempfile::tempdir().unwrap();
        let manifest_path = dir.path().join("hew.toml");
        manifest::write_manifest_with_template(
            &manifest_path,
            "mylib",
            manifest::ManifestTemplate::Lib,
        )
        .unwrap();
        write_template_source(dir.path(), "mylib", manifest::ManifestTemplate::Lib);
        write_init_gitignore(dir.path());

        assert!(
            !dir.path().join("main.hew").exists(),
            "lib should not create main.hew"
        );
        assert!(dir.path().join("mylib.hew").exists());
        let src = std::fs::read_to_string(dir.path().join("mylib.hew")).unwrap();
        assert!(src.contains("fn add("), "mylib.hew should contain fn add");
        assert!(
            src.contains("// mylib library"),
            "mylib.hew should have library comment"
        );

        let m = manifest::parse_manifest(&manifest_path).unwrap();
        assert_eq!(m.package.description.as_deref(), Some("A Hew library"));

        let gi = std::fs::read_to_string(dir.path().join(".gitignore")).unwrap();
        assert!(gi.contains("target/"));
        assert!(gi.contains(".hew/"));
    }

    #[test]
    fn init_actor_creates_main_hew_with_actor() {
        let dir = tempfile::tempdir().unwrap();
        let manifest_path = dir.path().join("hew.toml");
        manifest::write_manifest_with_template(
            &manifest_path,
            "actorproj",
            manifest::ManifestTemplate::Actor,
        )
        .unwrap();
        write_template_source(dir.path(), "actorproj", manifest::ManifestTemplate::Actor);
        write_init_gitignore(dir.path());

        assert!(dir.path().join("main.hew").exists());
        let src = std::fs::read_to_string(dir.path().join("main.hew")).unwrap();
        assert!(
            src.contains("actor Counter"),
            "should contain actor definition"
        );
        assert!(
            src.contains("var count: i32"),
            "actor state field must use `var` — `receive fn increment` reassigns it, \
             and only a `var` field is mutable outside `init`"
        );
        assert!(
            !src.contains("self."),
            "actor template must not use legacy `self.` syntax"
        );
        assert!(
            src.contains("receive fn increment()"),
            "should contain receive handler"
        );
        assert!(
            src.contains("spawn Counter { count: 0 }"),
            "should contain spawn"
        );

        let m = manifest::parse_manifest(&manifest_path).unwrap();
        assert_eq!(
            m.package.description.as_deref(),
            Some("A Hew actor project")
        );

        let gi = std::fs::read_to_string(dir.path().join(".gitignore")).unwrap();
        assert!(gi.contains("target/"));
        assert!(gi.contains(".hew/"));
    }

    #[test]
    fn init_does_not_overwrite_existing_source() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("main.hew"), "// existing").unwrap();
        write_template_source(dir.path(), "proj", manifest::ManifestTemplate::Bin);
        let src = std::fs::read_to_string(dir.path().join("main.hew")).unwrap();
        assert_eq!(src, "// existing", "should not overwrite existing file");
    }

    #[test]
    fn init_follow_up_hint_matches_template() {
        assert_eq!(
            init_follow_up_hint(manifest::ManifestTemplate::Bin, "proj"),
            "Next: `hew check main.hew` then `hew run main.hew`"
        );
        assert_eq!(
            init_follow_up_hint(manifest::ManifestTemplate::Lib, "proj"),
            "Next: `hew check proj.hew`"
        );
        assert_eq!(
            init_follow_up_hint(manifest::ManifestTemplate::Actor, "proj"),
            "Next: `hew check main.hew` then `hew run main.hew`"
        );
    }

    #[test]
    fn init_gitignore_appends_to_existing_without_entries() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join(".gitignore"), "*.o\n").unwrap();
        write_init_gitignore(dir.path());
        let gi = std::fs::read_to_string(dir.path().join(".gitignore")).unwrap();
        assert!(gi.contains("*.o"), "should keep existing entries");
        assert!(gi.contains(".hew/"), "should add .hew/");
        assert!(gi.contains("target/"), "should add target/");
    }

    // ── remove command tests ───────────────────────────────────────────

    #[test]
    fn remove_cleans_up_symlink_dir() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("hew.toml");
        manifest::write_default_manifest(&path, "myproject").unwrap();
        manifest::add_dependency(&path, "std.net.http", "1.0", None).unwrap();

        // Simulate installed package symlink directory.
        let pkg_dir = dir
            .path()
            .join(".hew")
            .join("packages")
            .join("std.net.http");
        std::fs::create_dir_all(&pkg_dir).unwrap();
        std::fs::write(pkg_dir.join("marker"), "x").unwrap();

        // Remove the dependency from manifest directly.
        let removed = manifest::remove_dependency(&path, "std.net.http").unwrap();
        assert!(removed);
        let m = manifest::parse_manifest(&path).unwrap();
        assert!(!m.dependencies.contains_key("std.net.http"));
    }

    // ── check command tests ────────────────────────────────────────────

    #[test]
    fn downloaded_manifest_naming_native_lib_gets_the_removal_hint() {
        let manifest = "[package]\nname = \"acme.old\"\nversion = \"0.3.0\"\n\n\
                        [native]\ncrate = \".\"\nlib = \"acme_old\"\nkind = \"staticlib\"\n";
        let mut tar = tar::Builder::new(Vec::new());
        let mut header = tar::Header::new_gnu();
        header.set_size(manifest.len() as u64);
        header.set_mode(0o644);
        header.set_cksum();
        tar.append_data(&mut header, "hew.toml", manifest.as_bytes())
            .unwrap();
        let archive = zstd::encode_all(tar.into_inner().unwrap().as_slice(), 3).unwrap();

        let error = validate_downloaded_registry_manifest(&archive, "acme.old").unwrap_err();

        assert!(
            error.contains("E_INVALID_NATIVE: line 7: [native] `lib` is no longer read"),
            "{error}"
        );
    }

    #[test]
    fn check_resolves_workspace_siblings_from_their_directories() {
        let (_registry_dir, registry) = setup_registry();
        let root = tempfile::tempdir().unwrap();
        for (dir, manifest) in [
            ("a", "[package]\nname = \"a\"\nversion = \"1.0.0\"\n"),
            (
                "b",
                "[package]\nname = \"b\"\nversion = \"1.0.0\"\n[dependencies]\n\"a\" = \"1.0.0\"\n",
            ),
        ] {
            std::fs::create_dir_all(root.path().join(dir)).unwrap();
            std::fs::write(root.path().join(dir).join("hew.toml"), manifest).unwrap();
        }
        std::fs::write(
            root.path().join("hew.toml"),
            "[workspace]\nmembers = [\"a\", \"b\"]\n",
        )
        .unwrap();
        let member = root.path().join("b");
        let m = manifest::parse_manifest(&member.join("hew.toml")).unwrap();

        let sources =
            workspace_sources(resolver::RegistrySources::default_source(), &member).unwrap();
        assert!(collect_manifest_issues(&m, &member, &registry, &sources).is_empty());

        // Negative control: outside the workspace, `a` is an unpublished registry package.
        let issues = collect_manifest_issues(
            &m,
            &member,
            &registry,
            &resolver::RegistrySources::default_source(),
        );
        assert_eq!(issues.len(), 1, "{issues:?}");
    }

    #[test]
    fn check_detects_invalid_and_unimportable_names_in_manifest() {
        let (_registry_dir, registry) = setup_registry();
        let issues_for = |name: &str| {
            let dir = tempfile::tempdir().unwrap();
            let path = dir.path().join("hew.toml");
            std::fs::write(
                &path,
                format!("[package]\nname = \"{name}\"\nversion = \"1.0.0\"\n"),
            )
            .unwrap();
            let m = manifest::parse_manifest(&path).unwrap();
            collect_manifest_issues(
                &m,
                dir.path(),
                &registry,
                &resolver::RegistrySources::default_source(),
            )
        };
        assert!(issues_for("bad name!")[0].contains("only lowercase"));
        assert!(issues_for("acme.3d")[0].contains("segment `3d` cannot be written"));
        assert!(issues_for("acme.type").is_empty());
        assert!(issues_for("acme.three_d").is_empty());
    }

    #[test]
    fn check_detects_invalid_version_in_manifest() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("hew.toml");
        std::fs::write(
            &path,
            "[package]\nname = \"good_name\"\nversion = \"notvalid\"\n\n[dependencies]\n",
        )
        .unwrap();
        let m = manifest::parse_manifest(&path).unwrap();
        assert!(is_valid_package_name(&m.package.name));
        assert!(semver::Version::parse(&m.package.version).is_err());
    }

    #[test]
    fn check_valid_manifest_passes() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("hew.toml");
        manifest::write_manifest_with_template(
            &path,
            "valid_project",
            manifest::ManifestTemplate::Bin,
        )
        .unwrap();
        let m = manifest::parse_manifest(&path).unwrap();
        assert!(is_valid_package_name(&m.package.name));
        assert!(semver::Version::parse(&m.package.version).is_ok());
    }
}
