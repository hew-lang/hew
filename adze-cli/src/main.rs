//! `adze` — the Hew package manager CLI.

mod checksum;
mod client;
mod config;
mod credentials;
mod index;
mod lockfile;
mod manifest;
mod registry;
mod resolver;
mod signing;
mod tarball;

use std::path::{Path, PathBuf};

use clap::{Parser, Subcommand};

/// adze — the Hew package manager
#[derive(Debug, Parser)]
#[command(name = "adze", version, about)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// Create a new hew.toml in the current directory
    Init {
        /// Project name (defaults to directory name)
        name: Option<String>,
        /// Create a library project
        #[arg(long)]
        lib: bool,
        /// Create a binary project
        #[arg(long)]
        bin: bool,
        /// Create an actor project
        #[arg(long)]
        actor: bool,
    },
    /// Add a dependency to hew.toml
    Add {
        /// Package name
        package: String,
        /// Version requirement
        #[arg(long, default_value = "*")]
        version: String,
        /// Use a named registry from config
        #[arg(long, short = 'r')]
        registry: Option<String>,
    },
    /// Install dependencies into .adze/packages/
    Install {
        /// Require an up-to-date lock file
        #[arg(long)]
        locked: bool,
        /// Use a named registry from config
        #[arg(long, short = 'r')]
        registry: Option<String>,
    },
    /// Publish this package to the registry
    Publish {
        /// Use a named registry from config
        #[arg(long, short = 'r')]
        registry: Option<String>,
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
    /// Validate manifest
    Check,
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
    /// Show the registry's public signing key
    RegistryKey,
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
    /// Generate shell completion scripts
    Completions {
        /// Shell type: bash, zsh, or fish
        shell: String,
    },
}

#[derive(Debug, Subcommand)]
enum IndexAction {
    /// Sync the local package index from the registry
    Sync,
    /// Resolve a package version from the local index
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
enum KeyAction {
    /// Generate a new Ed25519 signing keypair
    Generate,
    /// List registered signing keys
    List,
    /// Look up a signing key by fingerprint
    Info {
        /// Key fingerprint (e.g. SHA256:...)
        fingerprint: String,
    },
}

#[derive(Debug, Subcommand)]
enum NamespaceAction {
    /// Register a custom namespace prefix
    Register {
        /// Namespace prefix to claim
        prefix: String,
    },
    /// List namespaces you own
    List,
    /// Show info about a namespace
    Info {
        /// Namespace prefix to look up
        prefix: String,
    },
}

fn main() {
    let cli = Cli::parse();

    // Handle commands that don't need config/registry.
    if let Command::Completions { shell } = &cli.command {
        cmd_completions(shell);
        return;
    }

    let cfg = config::load_config();
    let registry = registry::Registry::with_root(config::registry_path(&cfg));

    match cli.command {
        Command::Init {
            name,
            lib,
            bin: _,
            actor,
        } => {
            let template = if lib {
                manifest::ManifestTemplate::Lib
            } else if actor {
                manifest::ManifestTemplate::Actor
            } else {
                manifest::ManifestTemplate::Bin
            };
            cmd_init(name.as_deref(), template, &cfg);
        }
        Command::Add {
            package,
            version,
            registry: reg,
        } => cmd_add(&package, &version, reg.as_deref()),
        Command::Install {
            locked,
            registry: reg,
        } => cmd_install(locked, &registry, reg.as_deref()),
        Command::Publish { registry: reg } => cmd_publish(&registry, reg.as_deref()),
        Command::List => cmd_list(&registry),
        Command::Search {
            query,
            category,
            page,
            per_page,
            registry: reg,
        } => cmd_search(
            &query,
            category.as_deref(),
            page,
            per_page,
            &registry,
            reg.as_deref(),
        ),
        Command::Info {
            package,
            registry: reg,
        } => cmd_info(&package, &registry, reg.as_deref()),
        Command::Tree => cmd_tree(&registry),
        Command::Update { package } => cmd_update(package.as_deref(), &registry),
        Command::Remove { package } => cmd_remove(&package),
        Command::Check => cmd_check(&registry),
        Command::Outdated => cmd_outdated(&registry),
        Command::Login => cmd_login(),
        Command::Logout => cmd_logout(),
        Command::RegistryKey => cmd_registry_key(),
        Command::Key { action } => match action {
            KeyAction::Generate => cmd_key_generate(),
            KeyAction::List => cmd_key_list(),
            KeyAction::Info { fingerprint } => cmd_key_info(&fingerprint),
        },
        Command::Namespace { action } => match action {
            NamespaceAction::Register { prefix } => cmd_namespace_register(&prefix),
            NamespaceAction::List => cmd_namespace_list(),
            NamespaceAction::Info { prefix } => cmd_namespace_info(&prefix),
        },
        Command::Yank {
            version,
            reason,
            undo,
        } => cmd_yank(&version, reason.as_deref(), undo),
        Command::Deprecate {
            package,
            message,
            successor,
            undo,
        } => cmd_deprecate(
            package.as_deref(),
            message.as_deref(),
            successor.as_deref(),
            undo,
        ),
        Command::Index { action } => match action {
            IndexAction::Sync => cmd_index_sync(),
            IndexAction::Resolve { package, version } => {
                cmd_index_resolve(&package, &version);
            }
            IndexAction::List { package } => cmd_index_list(&package),
        },
        Command::Completions { .. } => unreachable!("handled above"),
    }
}

fn cmd_completions(shell: &str) {
    match shell {
        "bash" => print!("{}", include_str!("../../completions/adze.bash")),
        "zsh" => print!("{}", include_str!("../../completions/adze.zsh")),
        "fish" => print!("{}", include_str!("../../completions/adze.fish")),
        other => {
            eprintln!("Unknown shell: {other}");
            eprintln!("Supported shells: bash, zsh, fish");
            std::process::exit(1);
        }
    }
}

/// Build a [`RegistryClient`] for the given named registry (or default).
fn make_client(registry_name: Option<&str>) -> client::RegistryClient {
    match registry_name {
        Some(name) => {
            let cfg = config::load_config();
            let remote = config::get_named_registry(&cfg, name).unwrap_or_else(|| {
                eprintln!("adze: unknown registry '{name}'");
                eprintln!("Configure it in ~/.adze/config.toml under [registries.{name}]");
                std::process::exit(1);
            });
            let mut c = client::RegistryClient::with_url(remote.api);
            let cred_path = credentials::credentials_path();
            if let Ok(token) = credentials::get_named_token(&cred_path, name) {
                c = c.with_token(token);
            }
            c
        }
        None => client::RegistryClient::new(),
    }
}

fn cmd_init(name: Option<&str>, template: manifest::ManifestTemplate, cfg: &config::AdzeConfig) {
    let cwd = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    let name = name.map_or_else(
        || {
            cwd.file_name()
                .and_then(|n| n.to_str())
                .unwrap_or("myproject")
                .to_string()
        },
        str::to_string,
    );
    let manifest_path = cwd.join("hew.toml");
    if manifest_path.exists() {
        eprintln!("adze init: hew.toml already exists");
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
                        eprintln!("adze init: warning: could not apply defaults: {e}");
                    }
                }
            }

            // Write template source file.
            write_template_source(&cwd, &name, template);

            // Write .gitignore with target/ and .adze/.
            write_init_gitignore(&cwd);

            // Parse back to verify and display the confirmed package name.
            match manifest::parse_manifest(&manifest_path) {
                Ok(m) => println!("Created hew.toml: {}", m.summary()),
                Err(e) => {
                    eprintln!("adze init: warning: created manifest may be invalid: {e}");
                    println!("Created hew.toml for project `{name}`");
                }
            }
        }
        Err(e) => {
            eprintln!("adze init: {e}");
            std::process::exit(1);
        }
    }
}

/// Write the template source file for `adze init`.
fn write_template_source(dir: &Path, name: &str, template: manifest::ManifestTemplate) {
    let (filename, content) = match template {
        manifest::ManifestTemplate::Lib => (
            "lib.hew",
            format!("// {name} library\n\nfn add(a: i32, b: i32) -> i32 {{\n    a + b\n}}\n"),
        ),
        manifest::ManifestTemplate::Actor => (
            "main.hew",
            "actor Counter {\n    count: i32;\n\n    receive fn increment() {\n        \
             self.count = self.count + 1;\n        println(self.count);\n    }\n}\n\n\
             fn main() {\n    let c = spawn Counter(count: 0);\n    c.increment();\n    \
             c.increment();\n    c.increment();\n}\n"
                .to_string(),
        ),
        manifest::ManifestTemplate::Bin => (
            "main.hew",
            format!("fn main() {{\n    println(\"Hello from {name}!\");\n}}\n"),
        ),
    };
    let path = dir.join(filename);
    if !path.exists() {
        if let Err(e) = std::fs::write(&path, content) {
            eprintln!("warning: could not create {filename}: {e}");
        }
    }
}

/// Write a `.gitignore` with `target/` and `.adze/` entries.
fn write_init_gitignore(dir: &Path) {
    ensure_gitignore_entry(dir, ".adze/");
    ensure_gitignore_entry(dir, "target/");
}

/// Append `entry` to `.gitignore` if not already present.
fn ensure_gitignore_entry(dir: &Path, entry: &str) {
    let path = dir.join(".gitignore");
    if let Ok(contents) = std::fs::read_to_string(&path) {
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
    } else {
        let _ = std::fs::write(&path, format!("{entry}\n"));
    }
}

fn cmd_add(pkg: &str, version: &str, registry_name: Option<&str>) {
    // Validate the named registry exists (if specified) early.
    if registry_name.is_some() {
        let _ = make_client(registry_name);
    }
    let cwd = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    let manifest_path = cwd.join("hew.toml");
    if !manifest_path.exists() {
        eprintln!("adze add: no hew.toml found in current directory");
        eprintln!("Run `adze init` to create one.");
        std::process::exit(1);
    }
    match manifest::add_dependency(&manifest_path, pkg, version) {
        Ok(()) => println!("Added {pkg}@{version} to hew.toml"),
        Err(e) => {
            eprintln!("adze add: {e}");
            std::process::exit(1);
        }
    }
}

#[expect(
    clippy::too_many_lines,
    reason = "install has many sequential steps that are clearest in one function"
)]
fn cmd_install(locked: bool, registry: &registry::Registry, registry_name: Option<&str>) {
    let cwd = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    let manifest_path = cwd.join("hew.toml");
    if !manifest_path.exists() {
        eprintln!("adze install: no hew.toml found in current directory");
        std::process::exit(1);
    }
    let m = match manifest::parse_manifest(&manifest_path) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("adze install: {e}");
            std::process::exit(1);
        }
    };

    let lock_path = cwd.join("adze.lock");

    if locked {
        // --locked: read existing lock and validate against manifest.
        let lf = match lockfile::read_lockfile(&lock_path) {
            Ok(lf) => lf,
            Err(lockfile::LockError::Missing) => {
                eprintln!("adze install: --locked requires an adze.lock file, but none was found");
                std::process::exit(1);
            }
            Err(e) => {
                eprintln!("adze install: {e}");
                std::process::exit(1);
            }
        };
        if lockfile::is_lock_stale(&lf, &m) {
            let err = lockfile::LockError::Stale;
            eprintln!("adze install: {err}");
            eprintln!("Run `adze install` without --locked to update it.");
            std::process::exit(1);
        }
        // Verify checksums of locked packages.
        for entry in &lf.packages {
            if let Some(expected) = &entry.checksum {
                let pkg_dir = registry.package_dir(&entry.name, &entry.version);
                if pkg_dir.is_dir() {
                    match checksum::compute_dir_checksum(&pkg_dir) {
                        Ok(actual) if actual != *expected => {
                            eprintln!(
                                "adze install: checksum mismatch for {}@{}",
                                entry.name, entry.version
                            );
                            eprintln!("  expected: {expected}");
                            eprintln!("  actual:   {actual}");
                            std::process::exit(1);
                        }
                        Err(e) => {
                            eprintln!(
                                "warning: cannot verify checksum for {}@{}: {e}",
                                entry.name, entry.version
                            );
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    let local_packages = cwd.join(".adze").join("packages");
    if let Err(e) = std::fs::create_dir_all(&local_packages) {
        eprintln!("adze install: cannot create .adze/packages/: {e}");
        std::process::exit(1);
    }

    update_gitignore(&cwd);

    if m.dependencies.is_empty() {
        // Write an empty lockfile for consistency.
        let lf = lockfile::LockFile {
            packages: Vec::new(),
        };
        if let Err(e) = lockfile::write_lockfile(&lock_path, &lf) {
            eprintln!("adze install: cannot write adze.lock: {e}");
        }
        println!("Nothing to install.");
        return;
    }

    // Resolve dependencies to exact versions.
    // First pass: try resolving against locally installed packages.
    let resolved = match resolver::resolve_all(&m, registry) {
        Ok(r) => r,
        Err(resolver::ResolveError::UnresolvableDeps { failures }) => {
            // Second pass: fetch missing packages from the remote registry.
            fetch_missing_packages(&failures, &m, registry, &make_client(registry_name));
            // Re-resolve now that packages are installed.
            match resolver::resolve_all(&m, registry) {
                Ok(r) => r,
                Err(e) => {
                    eprintln!("adze install: {e}");
                    std::process::exit(1);
                }
            }
        }
        Err(e) => {
            eprintln!("adze install: {e}");
            std::process::exit(1);
        }
    };

    // Build lockfile entries from resolved versions.
    let mut lock_packages: Vec<lockfile::LockedPackage> = Vec::new();

    for (name, version) in &resolved {
        let target = registry.package_dir(name, version);
        if !registry.is_installed(name, version) {
            eprintln!("warning: {name}@{version} not found in global registry (~/.adze/packages/)");
        }

        // Compute checksum if the package directory exists.
        let pkg_checksum = if target.is_dir() {
            match checksum::compute_dir_checksum(&target) {
                Ok(cs) => Some(cs),
                Err(e) => {
                    eprintln!("warning: cannot compute checksum for {name}@{version}: {e}");
                    None
                }
            }
        } else {
            None
        };

        lock_packages.push(lockfile::LockedPackage {
            name: name.clone(),
            version: version.clone(),
            checksum: pkg_checksum,
            signature: None,
            source: "registry".to_string(),
        });

        // Build the local symlink path: replace :: with / separators.
        let link = name
            .split("::")
            .fold(local_packages.clone(), |p, seg| p.join(seg));
        if let Some(parent) = link.parent() {
            if let Err(e) = std::fs::create_dir_all(parent) {
                eprintln!("adze install: cannot create {}: {e}", parent.display());
                std::process::exit(1);
            }
        }
        if link.is_symlink() || link.exists() {
            println!("  (already linked) {name}");
            continue;
        }
        #[cfg(unix)]
        {
            if let Err(e) = std::os::unix::fs::symlink(&target, &link) {
                eprintln!("adze install: cannot create symlink for {name}: {e}");
                std::process::exit(1);
            }
            println!("  linked {name}@{version}");
        }
        #[cfg(not(unix))]
        {
            eprintln!("adze install: symlinks not supported on this platform; skipping {name}");
        }
    }

    // Write the lockfile.
    let lf = lockfile::LockFile {
        packages: lock_packages,
    };
    if let Err(e) = lockfile::write_lockfile(&lock_path, &lf) {
        eprintln!("adze install: cannot write adze.lock: {e}");
        std::process::exit(1);
    }
    println!("Wrote adze.lock");
}

/// Fetch missing packages from the remote registry.
///
/// For each `(name, requirement)` in `failures`, queries the remote API
/// for available versions, finds the best match, downloads the tarball,
/// and unpacks it into the global registry.
fn fetch_missing_packages(
    failures: &[(String, String)],
    manifest: &manifest::HewManifest,
    registry: &registry::Registry,
    api_client: &client::RegistryClient,
) {
    for (name, _req_from_error) in failures {
        // Look up the original version requirement from the manifest.
        let Some(dep_spec) = manifest.dependencies.get(name) else {
            continue;
        };
        let requirement = dep_spec.version_req();

        eprint!("Fetching {name} from registry... ");

        // Query the remote for available versions (fall back to local index).
        let entries = match api_client.get_package(name) {
            Ok(entries) => entries,
            Err(api_err) => {
                // Fallback: try local index if synced.
                let index_root = config::local_index_path();
                match index::read_index_entries(&index_root, name) {
                    Ok(entries) if !entries.is_empty() => {
                        eprintln!("(using local index fallback)");
                        entries
                    }
                    _ => {
                        eprintln!("failed");
                        eprintln!("  warning: could not query registry for {name}: {api_err}");
                        continue;
                    }
                }
            }
        };

        // Find the best matching version.
        let matched = match resolver::resolve_version_from_entries(&entries, requirement) {
            Ok(result) => result,
            Err(e) => {
                eprintln!("failed");
                eprintln!("  warning: {e}");
                continue;
            }
        };
        let Some(resolved) = matched else {
            eprintln!("no matching version");
            eprintln!("  warning: no version of {name} matches {requirement}");
            continue;
        };
        let version = &resolved.version;

        // Get the download URL from the registry response.
        let Some(ref dl_url) = resolved.dl else {
            eprintln!("failed");
            eprintln!("  warning: registry did not provide download URL for {name}@{version}");
            continue;
        };

        // Download the tarball.
        let tarball_data = match api_client.download_tarball(dl_url) {
            Ok(data) => data,
            Err(e) => {
                eprintln!("download failed");
                eprintln!("  warning: could not download {name}@{version}: {e}");
                continue;
            }
        };

        // Verify checksum of downloaded tarball.
        let actual_checksum = tarball::checksum_bytes(&tarball_data);
        if actual_checksum != resolved.checksum {
            eprintln!("CHECKSUM MISMATCH");
            eprintln!("  error: tarball integrity check failed for {name}@{version}");
            eprintln!("  expected: {}", resolved.checksum);
            eprintln!("  actual:   {actual_checksum}");
            continue;
        }

        // Verify Ed25519 signature over the checksum.
        if !resolved.sig.is_empty() && !resolved.key_fp.is_empty() {
            match verify_package_signature(
                api_client,
                &resolved.checksum,
                &resolved.sig,
                &resolved.key_fp,
            ) {
                Ok(()) => {}
                Err(msg) => {
                    eprintln!("SIGNATURE VERIFICATION FAILED");
                    eprintln!("  error: {msg}");
                    continue;
                }
            }
        } else {
            eprintln!("warning: package {name}@{version} is unsigned");
        }

        // Verify registry counter-signature if present (warn-only).
        if let Some(ref reg_sig) = resolved.registry_sig {
            if let Some(ref published_at) = resolved.published_at {
                match verify_registry_signature(
                    api_client,
                    name,
                    version,
                    &resolved.checksum,
                    &resolved.sig,
                    published_at,
                    reg_sig,
                ) {
                    Ok(()) => {}
                    Err(msg) => {
                        eprintln!("warning: registry signature verification failed for {name}@{version}: {msg}");
                    }
                }
            }
        }

        // Unpack into the global registry.
        let target = registry.package_dir(name, version);
        if let Err(e) = tarball::unpack(&tarball_data, &target) {
            eprintln!("unpack failed");
            eprintln!("  warning: could not unpack {name}@{version}: {e}");
            continue;
        }

        eprintln!("{name}@{version}");
    }
}

/// Verify an Ed25519 signature over a checksum by fetching the public key
/// from the registry.
fn verify_package_signature(
    api_client: &client::RegistryClient,
    checksum: &str,
    signature: &str,
    key_fingerprint: &str,
) -> Result<(), String> {
    // Fetch the public key from the registry.
    let key_record = api_client
        .get_public_key(key_fingerprint)
        .map_err(|e| format!("could not fetch signing key {key_fingerprint}: {e}"))?;

    // Decode the base64 public key to raw bytes.
    use base64::Engine as _;
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
    let key_resp = api_client
        .get_registry_key()
        .map_err(|e| format!("could not fetch registry key: {e}"))?;

    use base64::Engine as _;
    let pub_bytes = base64::engine::general_purpose::STANDARD
        .decode(&key_resp.public_key)
        .map_err(|e| format!("invalid registry public key encoding: {e}"))?;

    let pub_arr: [u8; 32] = pub_bytes
        .try_into()
        .map_err(|_| "registry public key is not 32 bytes".to_string())?;

    let canonical =
        format!("registry:v1:{name}@{version}:{checksum}:{publisher_sig}:{published_at}");

    signing::verify(canonical.as_bytes(), registry_sig, &pub_arr)
        .map_err(|e| format!("registry signature invalid: {e}"))
}

fn cmd_publish(registry: &registry::Registry, registry_name: Option<&str>) {
    let cwd = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    let manifest_path = cwd.join("hew.toml");
    if !manifest_path.exists() {
        eprintln!("adze publish: no hew.toml found in current directory");
        std::process::exit(1);
    }
    let m = match manifest::parse_manifest(&manifest_path) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("adze publish: {e}");
            std::process::exit(1);
        }
    };

    // Validate required fields for publishing.
    let missing = manifest::validate_for_publish(&m);
    if !missing.is_empty() {
        eprintln!("adze publish: missing required fields:");
        for field in &missing {
            eprintln!("  - {field}");
        }
        std::process::exit(1);
    }

    // Validate package name format.
    if !is_valid_package_name(&m.package.name) {
        eprintln!(
            "adze publish: invalid package name `{}`: only alphanumeric, `_`, and `::` (or `/`) allowed",
            m.package.name
        );
        std::process::exit(1);
    }

    // Validate version is semver.
    if semver::Version::parse(&m.package.version).is_err() {
        eprintln!(
            "adze publish: invalid version `{}`: must be valid semver (e.g. 1.0.0)",
            m.package.version
        );
        std::process::exit(1);
    }

    // Load signing key.
    let key_dir = signing::default_key_dir();
    let keypair = match signing::load_keypair(&key_dir) {
        Ok(kp) => kp,
        Err(signing::SignError::NoKey) => {
            eprintln!("adze publish: no signing key found");
            eprintln!("Run `adze key generate` to create one.");
            std::process::exit(1);
        }
        Err(e) => {
            eprintln!("adze publish: {e}");
            std::process::exit(1);
        }
    };

    // Pack tarball.
    let exclude = m.package.exclude.as_deref().unwrap_or_default();
    let include = m.package.include.as_deref().unwrap_or_default();
    let pack_result = match tarball::pack(&cwd, exclude, include) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("adze publish: {e}");
            std::process::exit(1);
        }
    };

    // Sign the checksum.
    let signature = keypair.sign(pack_result.checksum.as_bytes());
    let fingerprint = keypair.fingerprint();

    // Build deps list for the index entry.
    let deps: Vec<index::IndexDep> = m
        .dependencies
        .iter()
        .map(|(name, spec)| {
            let (optional, features, default_features, dep_registry) = match spec {
                manifest::DepSpec::Version(_) => (false, vec![], true, None),
                manifest::DepSpec::Table(t) => (
                    t.optional.unwrap_or(false),
                    t.features.clone().unwrap_or_default(),
                    t.default_features.unwrap_or(true),
                    t.registry.clone(),
                ),
            };
            index::IndexDep {
                name: name.clone(),
                req: spec.version_req().to_string(),
                features,
                optional,
                default_features,
                registry: dep_registry,
            }
        })
        .collect();

    // Try remote publish first if we have credentials.
    let cred_path = credentials::credentials_path();
    let has_token = if registry_name.is_some() {
        // Named registries get their token via make_client
        true
    } else {
        credentials::get_token(&cred_path).is_ok()
    };
    if has_token {
        let api_client = {
            let mut c = make_client(registry_name);
            if registry_name.is_none() {
                if let Ok(token) = credentials::get_token(&cred_path) {
                    c = c.with_token(token);
                }
            }
            c
        };
        let request = client::PublishRequest {
            metadata: client::PublishMetadata {
                name: m.package.name.clone(),
                vers: m.package.version.clone(),
                description: m.package.description.clone().unwrap_or_default(),
                license: m.package.license.clone().unwrap_or_default(),
                authors: m.package.authors.clone().unwrap_or_default(),
                deps: deps.clone(),
                features: m.features.clone(),
                edition: m.package.edition.clone(),
                hew: m.package.hew.clone(),
                keywords: m.package.keywords.clone(),
                categories: m.package.categories.clone(),
                homepage: m.package.homepage.clone(),
                repository: m.package.repository.clone(),
                documentation: m.package.documentation.clone(),
            },
            checksum: pack_result.checksum.clone(),
            signature: signature.clone(),
            key_fingerprint: fingerprint.clone(),
        };

        match api_client.publish(
            &m.package.name,
            &m.package.version,
            &pack_result.data,
            &request,
        ) {
            Ok(()) => {
                println!(
                    "Published {}@{} to registry",
                    m.package.name, m.package.version
                );
                println!("Checksum: {}", pack_result.checksum);
                println!("Signature: {signature}");
                return;
            }
            Err(e) => {
                eprintln!("adze publish: remote publish failed: {e}");
                eprintln!("Falling back to local publish.");
            }
        }
    }

    // Fall back to local publish.
    let dest = registry.package_dir(&m.package.name, &m.package.version);
    if let Err(e) = copy_dir(&cwd, &dest) {
        eprintln!("adze publish: {e}");
        std::process::exit(1);
    }

    println!(
        "Published {}@{} to {}",
        m.package.name,
        m.package.version,
        dest.display()
    );
    println!("Checksum: {}", pack_result.checksum);
    println!("Signature: {signature}");
    println!("Key: {fingerprint}");
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

fn cmd_info(package: &str, registry: &registry::Registry, _registry_name: Option<&str>) {
    let packages = registry.list_packages();
    let versions: Vec<_> = packages.iter().filter(|p| p.name == package).collect();

    if versions.is_empty() {
        eprintln!("adze info: package `{package}` not found in registry");
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
        eprintln!("adze tree: no hew.toml found in current directory");
        std::process::exit(1);
    }
    let m = match manifest::parse_manifest(&manifest_path) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("adze tree: {e}");
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
        let resolved = resolver::resolve_version(name, ver_req, registry)
            .unwrap_or_else(|_| ver_req.to_string());
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
        eprintln!("adze update: no hew.toml found in current directory");
        std::process::exit(1);
    }
    let mut m = match manifest::parse_manifest(&manifest_path) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("adze update: {e}");
            std::process::exit(1);
        }
    };

    let packages = registry.list_packages();
    let mut updated = Vec::new();

    let deps_to_update: Vec<String> = if let Some(pkg) = package {
        if !m.dependencies.contains_key(pkg) {
            eprintln!("adze update: `{pkg}` is not a dependency");
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
            eprintln!("adze update: {e}");
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
        eprintln!("adze remove: no hew.toml found in current directory");
        std::process::exit(1);
    }

    match manifest::remove_dependency(&manifest_path, package) {
        Ok(true) => {
            // Remove local symlink if it exists.
            let link = package
                .split("::")
                .fold(cwd.join(".adze").join("packages"), |p, seg| p.join(seg));
            if link.is_symlink() || link.exists() {
                let _ = std::fs::remove_file(&link);
            }
            println!("Removed {package} from hew.toml");
        }
        Ok(false) => {
            eprintln!("adze remove: `{package}` is not a dependency");
            std::process::exit(1);
        }
        Err(e) => {
            eprintln!("adze remove: {e}");
            std::process::exit(1);
        }
    }
}

fn cmd_check(registry: &registry::Registry) {
    let cwd = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    let manifest_path = cwd.join("hew.toml");
    if !manifest_path.exists() {
        eprintln!("adze check: no hew.toml found in current directory");
        std::process::exit(1);
    }
    let m = match manifest::parse_manifest(&manifest_path) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("adze check: {e}");
            std::process::exit(1);
        }
    };

    let mut issues = Vec::new();

    if !is_valid_package_name(&m.package.name) {
        issues.push(format!(
            "invalid package name `{}`: only alphanumeric, `_`, and `::` allowed",
            m.package.name
        ));
    }

    if semver::Version::parse(&m.package.version).is_err() {
        issues.push(format!(
            "invalid version `{}`: must be valid semver (e.g. 1.0.0)",
            m.package.version
        ));
    }

    for (name, spec) in &m.dependencies {
        let req = spec.version_req();
        if let Err(e) = resolver::resolve_version(name, req, registry) {
            issues.push(format!("dependency {name}@{req}: {e}"));
        }
    }

    if issues.is_empty() {
        println!("OK: manifest is valid.");
    } else {
        println!("Issues found:");
        for issue in &issues {
            println!("  - {issue}");
        }
        std::process::exit(1);
    }
}

fn cmd_outdated(registry: &registry::Registry) {
    let cwd = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    let manifest_path = cwd.join("hew.toml");
    if !manifest_path.exists() {
        eprintln!("adze outdated: no hew.toml found in current directory");
        std::process::exit(1);
    }
    let m = match manifest::parse_manifest(&manifest_path) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("adze outdated: {e}");
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
            eprintln!("adze login: {e}");
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
            eprintln!("adze login: authorization timed out");
            std::process::exit(1);
        }

        match api_client.login_token(&device.device_code) {
            Ok(resp) => {
                if let Some(token) = resp.token {
                    let cred_path = credentials::credentials_path();
                    let creds = credentials::CredentialsFile {
                        registry: Some(credentials::Credentials {
                            token,
                            github_user: resp.github_user.clone(),
                        }),
                        registries: None,
                    };
                    if let Err(e) = credentials::save_credentials(&cred_path, &creds) {
                        eprintln!("adze login: {e}");
                        std::process::exit(1);
                    }
                    let user = resp.github_user.as_deref().unwrap_or("unknown");
                    println!("Logged in as {user}");
                    return;
                }
                if resp.error.as_deref() != Some("authorization_pending") {
                    eprintln!(
                        "adze login: {}",
                        resp.error.unwrap_or_else(|| "unknown error".to_string())
                    );
                    std::process::exit(1);
                }
            }
            Err(e) => {
                eprintln!("adze login: {e}");
                std::process::exit(1);
            }
        }
    }
}

fn cmd_logout() {
    let cred_path = credentials::credentials_path();
    if cred_path.exists() {
        if let Err(e) = std::fs::remove_file(&cred_path) {
            eprintln!("adze logout: {e}");
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
            "adze key generate: key already exists at {}",
            secret_path.display()
        );
        eprintln!("Remove it first if you want to regenerate.");
        std::process::exit(1);
    }

    let keypair = signing::KeyPair::generate();
    if let Err(e) = signing::save_keypair(&key_dir, &keypair) {
        eprintln!("adze key generate: {e}");
        std::process::exit(1);
    }

    println!("Generated Ed25519 signing key:");
    println!("  Private: {}", secret_path.display());
    println!("  Public:  {}", key_dir.join("id_ed25519.pub").display());
    println!("  Fingerprint: {}", keypair.fingerprint());

    // Try to register the key with the registry.
    let cred_path = credentials::credentials_path();
    if let Ok(token) = credentials::get_token(&cred_path) {
        let api_client = client::RegistryClient::new().with_token(token);
        match api_client.register_key(&keypair.public_key_base64()) {
            Ok(fp) => println!("Key registered with registry (fingerprint: {fp})"),
            Err(e) => eprintln!("warning: could not register key with registry: {e}"),
        }
    } else {
        println!("Run `adze login` and re-run to register this key with the registry.");
    }
}

fn cmd_key_list() {
    let key_dir = signing::default_key_dir();
    let pub_path = key_dir.join("id_ed25519.pub");

    if !pub_path.exists() {
        println!("No signing keys found.");
        println!("Run `adze key generate` to create one.");
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
            eprintln!("adze key list: {e}");
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
            eprintln!("adze registry-key: {e}");
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
            eprintln!("adze key info: {e}");
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
            eprintln!("adze namespace info: {e}");
            std::process::exit(1);
        }
    }
}

fn cmd_namespace_register(prefix: &str) {
    let cred_path = credentials::credentials_path();
    let Ok(token) = credentials::get_token(&cred_path) else {
        eprintln!("adze namespace register: not logged in");
        eprintln!("Run `adze login` first.");
        std::process::exit(1);
    };

    let api_client = client::RegistryClient::new().with_token(token);
    match api_client.register_namespace(prefix) {
        Ok(()) => println!("Registered namespace `{prefix}`"),
        Err(e) => {
            eprintln!("adze namespace register: {e}");
            std::process::exit(1);
        }
    }
}

fn cmd_namespace_list() {
    let cred_path = credentials::credentials_path();
    if credentials::get_token(&cred_path).is_err() {
        eprintln!("adze namespace list: not logged in");
        eprintln!("Run `adze login` first.");
        std::process::exit(1);
    }
    // TODO: implement via API call to list owned namespaces.
    println!("Namespace listing requires registry connection.");
    println!("Your GitHub username is auto-reserved as a namespace.");
}

fn cmd_yank(version: &str, reason: Option<&str>, undo: bool) {
    let cwd = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    let manifest_path = cwd.join("hew.toml");
    if !manifest_path.exists() {
        eprintln!("adze yank: no hew.toml found in current directory");
        std::process::exit(1);
    }
    let m = match manifest::parse_manifest(&manifest_path) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("adze yank: {e}");
            std::process::exit(1);
        }
    };

    let cred_path = credentials::credentials_path();
    let Ok(token) = credentials::get_token(&cred_path) else {
        eprintln!("adze yank: not logged in");
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
            eprintln!("adze yank: {e}");
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
            eprintln!("adze deprecate: no hew.toml found and no package specified");
            std::process::exit(1);
        }
        match manifest::parse_manifest(&manifest_path) {
            Ok(m) => m.package.name,
            Err(e) => {
                eprintln!("adze deprecate: {e}");
                std::process::exit(1);
            }
        }
    };

    let cred_path = credentials::credentials_path();
    let Ok(token) = credentials::get_token(&cred_path) else {
        eprintln!("adze deprecate: not logged in");
        std::process::exit(1);
    };

    let api_client = client::RegistryClient::new().with_token(token);

    if undo {
        // Undo deprecation — pass None for both.
        match api_client.deprecate(&name, None, None) {
            Ok(()) => println!("Undid deprecation of {name}"),
            Err(e) => {
                eprintln!("adze deprecate: {e}");
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
                eprintln!("adze deprecate: {e}");
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
                    "adze index sync: git pull failed (exit {})",
                    s.code().unwrap_or(-1)
                );
                std::process::exit(1);
            }
            Err(e) => {
                eprintln!("adze index sync: {e}");
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
                    "adze index sync: git clone failed (exit {})",
                    s.code().unwrap_or(-1)
                );
                std::process::exit(1);
            }
            Err(e) => {
                eprintln!("adze index sync: {e}");
                std::process::exit(1);
            }
        }
    }
}

fn cmd_index_resolve(package: &str, version_req: &str) {
    let index_dir = config::local_index_path();
    if !index_dir.exists() {
        eprintln!("adze index resolve: local index not found");
        eprintln!("Run `adze index sync` first.");
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
            eprintln!("adze index resolve: {e}");
            std::process::exit(1);
        }
    }
}

fn cmd_index_list(package: &str) {
    let index_dir = config::local_index_path();
    if !index_dir.exists() {
        eprintln!("adze index list: local index not found");
        eprintln!("Run `adze index sync` first.");
        std::process::exit(1);
    }

    // Check for deprecation metadata alongside the package index file.
    let deprecation_path = index_dir
        .join(index::index_path(package))
        .with_extension("deprecated");
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
            eprintln!("adze index list: {e}");
            std::process::exit(1);
        }
    }
}

// ── helpers ──────────────────────────────────────────────────────────────────

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

/// Validate that a package name contains only lowercase alphanumeric, `_`, and `::` or `/` separators.
/// Rejects empty names, empty segments, and names longer than 128 characters.
fn is_valid_package_name(name: &str) -> bool {
    if name.is_empty() || name.len() > 128 {
        return false;
    }
    // Normalize :: to / then validate segments
    let normalized = name.replace("::", "/");
    // After normalizing ::→/, any remaining colons mean a bare single colon
    if normalized.contains(':') {
        return false;
    }
    if normalized.starts_with('/') || normalized.ends_with('/') {
        return false;
    }
    for segment in normalized.split('/') {
        if segment.is_empty() {
            return false;
        }
        if !segment
            .chars()
            .all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '_')
        {
            return false;
        }
    }
    true
}

/// Add `.adze/` to the `.gitignore` in `dir` if not already present.
fn update_gitignore(dir: &Path) {
    const ENTRY: &str = ".adze/";
    let path = dir.join(".gitignore");
    if let Ok(contents) = std::fs::read_to_string(&path) {
        if contents.lines().any(|l| l.trim() == ENTRY) {
            return;
        }
        let updated = if contents.ends_with('\n') {
            format!("{contents}{ENTRY}\n")
        } else if contents.is_empty() {
            format!("{ENTRY}\n")
        } else {
            format!("{contents}\n{ENTRY}\n")
        };
        let _ = std::fs::write(&path, updated);
    } else {
        let _ = std::fs::write(&path, format!("{ENTRY}\n"));
    }
}

/// Recursively copy `src` to `dst`, skipping `.git`, `target`, and `.adze`.
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
        if matches!(name_str.as_ref(), ".git" | "target" | ".adze") {
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

    #[test]
    fn update_gitignore_creates_file() {
        let dir = tempfile::tempdir().unwrap();
        update_gitignore(dir.path());
        let contents = std::fs::read_to_string(dir.path().join(".gitignore")).unwrap();
        assert!(contents.contains(".adze/"));
    }

    #[test]
    fn update_gitignore_no_duplicate() {
        let dir = tempfile::tempdir().unwrap();
        update_gitignore(dir.path());
        update_gitignore(dir.path());
        let contents = std::fs::read_to_string(dir.path().join(".gitignore")).unwrap();
        assert_eq!(contents.matches(".adze/").count(), 1);
    }

    #[test]
    fn update_gitignore_appends_to_existing() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join(".gitignore"), "target/\n").unwrap();
        update_gitignore(dir.path());
        let contents = std::fs::read_to_string(dir.path().join(".gitignore")).unwrap();
        assert!(contents.contains("target/"));
        assert!(contents.contains(".adze/"));
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
            content.push_str(&format!("\"{dep_name}\" = \"{dep_ver}\"\n"));
        }
        std::fs::write(dir.join("hew.toml"), content).unwrap();
    }

    #[test]
    fn is_valid_package_name_accepts_simple() {
        assert!(is_valid_package_name("mypackage"));
        assert!(is_valid_package_name("my_package"));
        assert!(is_valid_package_name("std::net::http"));
    }

    #[test]
    fn is_valid_package_name_rejects_invalid() {
        assert!(!is_valid_package_name(""));
        assert!(!is_valid_package_name("my package"));
        assert!(!is_valid_package_name("my@package"));
        assert!(!is_valid_package_name("my:package")); // single colon
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
        install_fake(&reg, "std::net::http", "1.0.0");
        install_fake(&reg, "std::net::websocket", "1.0.0");
        install_fake(&reg, "ecosystem::db::postgres", "1.0.0");

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
        install_fake_with_deps(
            &reg,
            "std::net::http",
            "1.0.0",
            &[("hew::net::tcp", "1.0.0")],
        );
        install_fake(&reg, "hew::net::tcp", "1.0.0");

        let dep_toml = reg.package_dir("std::net::http", "1.0.0").join("hew.toml");
        let dep_m = manifest::parse_manifest(&dep_toml).unwrap();
        assert_eq!(dep_m.dependencies.len(), 1);
        assert!(dep_m.dependencies.contains_key("hew::net::tcp"));
    }

    #[test]
    fn remove_dependency_from_manifest() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("hew.toml");
        manifest::write_default_manifest(&path, "myproject").unwrap();
        manifest::add_dependency(&path, "std::net::http", "1.0").unwrap();

        let removed = manifest::remove_dependency(&path, "std::net::http").unwrap();
        assert!(removed);

        let m = manifest::parse_manifest(&path).unwrap();
        assert!(!m.dependencies.contains_key("std::net::http"));
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
        manifest::add_dependency(&path, "mypkg", "1.0.0").unwrap();

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
        assert!(gi.contains(".adze/"));
    }

    #[test]
    fn init_lib_creates_lib_hew() {
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
        assert!(dir.path().join("lib.hew").exists());
        let src = std::fs::read_to_string(dir.path().join("lib.hew")).unwrap();
        assert!(src.contains("fn add("), "lib.hew should contain fn add");
        assert!(
            src.contains("// mylib library"),
            "lib.hew should have library comment"
        );

        let m = manifest::parse_manifest(&manifest_path).unwrap();
        assert_eq!(m.package.description.as_deref(), Some("A Hew library"));

        let gi = std::fs::read_to_string(dir.path().join(".gitignore")).unwrap();
        assert!(gi.contains("target/"));
        assert!(gi.contains(".adze/"));
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
            src.contains("receive fn increment()"),
            "should contain receive handler"
        );
        assert!(
            src.contains("spawn Counter(count: 0)"),
            "should contain spawn"
        );

        let m = manifest::parse_manifest(&manifest_path).unwrap();
        assert_eq!(
            m.package.description.as_deref(),
            Some("A Hew actor project")
        );

        let gi = std::fs::read_to_string(dir.path().join(".gitignore")).unwrap();
        assert!(gi.contains("target/"));
        assert!(gi.contains(".adze/"));
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
    fn init_gitignore_appends_to_existing_without_entries() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join(".gitignore"), "*.o\n").unwrap();
        write_init_gitignore(dir.path());
        let gi = std::fs::read_to_string(dir.path().join(".gitignore")).unwrap();
        assert!(gi.contains("*.o"), "should keep existing entries");
        assert!(gi.contains(".adze/"), "should add .adze/");
        assert!(gi.contains("target/"), "should add target/");
    }

    // ── remove command tests ───────────────────────────────────────────

    #[test]
    fn remove_cleans_up_symlink_dir() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("hew.toml");
        manifest::write_default_manifest(&path, "myproject").unwrap();
        manifest::add_dependency(&path, "std::net::http", "1.0").unwrap();

        // Simulate installed package symlink directory.
        let pkg_dir = dir
            .path()
            .join(".adze")
            .join("packages")
            .join("std")
            .join("net")
            .join("http");
        std::fs::create_dir_all(&pkg_dir).unwrap();
        std::fs::write(pkg_dir.join("marker"), "x").unwrap();

        // Remove the dependency from manifest directly.
        let removed = manifest::remove_dependency(&path, "std::net::http").unwrap();
        assert!(removed);
        let m = manifest::parse_manifest(&path).unwrap();
        assert!(!m.dependencies.contains_key("std::net::http"));
    }

    // ── check command tests ────────────────────────────────────────────

    #[test]
    fn check_detects_invalid_name_in_manifest() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("hew.toml");
        std::fs::write(
            &path,
            "[package]\nname = \"bad name!\"\nversion = \"1.0.0\"\n\n[dependencies]\n",
        )
        .unwrap();
        let m = manifest::parse_manifest(&path).unwrap();
        assert!(!is_valid_package_name(&m.package.name));
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
