//! Building a package's `[native]` code into link inputs.
//!
//! [`build`] is the one place a package's native code is built: the Rust crate
//! (through Cargo), the C, C++ and assembly sources (through the C driver Hew links
//! with) and the system libraries (`pkg-config`, `link-libs`, `lib-dirs`). It
//! returns everything the final link needs from that package, plus the files
//! the build read, so `hew build --emit-deps` can name them.
//!
//! The caller supplies the C toolchain ([`NativeToolchain`]) because the driver
//! and its target flags belong to the linker driver, not to the manifest.

use std::collections::BTreeSet;
use std::fmt::Write as _;
use std::path::{Path, PathBuf};
use std::process::Command;

use sha2::{Digest, Sha256};

use crate::manifest::{HewManifest, LinkLib, NativeInputs, NativeOs, RustCrate, SourceLanguage};

/// The symbol prefix the Hew runtime owns. Package C and C++ code may not
/// define symbols under it.
const RESERVED_PREFIX: &str = "hew_";

/// Serial for temporary object names. In-process test compiles build on
/// several threads, so the process id alone does not make a name unique.
static NEXT_TEMPORARY: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

/// How native sources compile for the current target.
#[derive(Debug, Clone)]
pub struct NativeToolchain {
    /// Canonical target triple selected by the compiler driver.
    pub target: String,
    /// Entry package lock used to verify registry native artifacts.
    pub lockfile: Option<PathBuf>,
    /// Selects the `[native.<os>]` table.
    pub os: NativeOs,
    /// The C driver (`clang`, a gcc-compatible `cc`, or `HEW_CC`). It picks
    /// the C/C++ language from the source extension; assembly uses explicit `-x`.
    pub driver: String,
    /// Flags every compile carries: target, profile, PIC, CRT and sanitizer.
    pub flags: Vec<String>,
    /// Object file suffix for the target (`.o` or `.obj`).
    pub object_suffix: &'static str,
}

/// What one package's `[native]` section contributes to the final link.
/// Keep this value alive through linking to retain its cache generations.
#[derive(Debug, Clone, Default)]
pub struct NativeBuild {
    /// Archives and objects, linked before any system library.
    pub inputs: Vec<PathBuf>,
    /// Library search paths and system libraries, linked after every input.
    pub link_args: Vec<String>,
    /// Whether a C++ source was compiled; the link then needs the C++ runtime.
    pub compiles_cxx: bool,
    /// Files the build read: native sources, their headers and Rust sources.
    pub dependencies: Vec<PathBuf>,
    /// Retains immutable native archive generations through the final link.
    prebuilts: Vec<std::sync::Arc<crate::native_prebuilt::PrebuiltArtifact>>,
    source: Option<std::sync::Arc<crate::atomic_fs::PinnedDir>>,
}

/// Build the `[native]` section of the package rooted at `root`.
///
/// Returns `Ok(None)` when the manifest has no `[native]` section.
///
/// # Errors
///
/// Returns the diagnostic text when the Rust crate's toolchain does not match
/// the runtime's (`E_NATIVE_TOOLCHAIN`), a build step fails
/// (`E_NATIVE_COMPILE`, `E_NATIVE_PKG_CONFIG`), or the Rust crate or a C or
/// C++ source of a package outside the `hew.` namespace defines a symbol
/// under the runtime's reserved prefix (`E_RESERVED_NATIVE_SYMBOL`).
pub fn build(
    root: &Path,
    manifest: &HewManifest,
    toolchain: &NativeToolchain,
) -> Result<Option<NativeBuild>, String> {
    let Some(native) = &manifest.native else {
        return Ok(None);
    };
    let package = manifest.package.name.as_str();
    let mut out = NativeBuild::default();
    if let Some(rust) = native.rust_crate() {
        let prebuilt = crate::native_prebuilt::for_package(
            root,
            manifest,
            &toolchain.target,
            toolchain.lockfile.as_deref(),
        )?;
        let (artifact, label) = if let Some(prebuilt) = prebuilt {
            let artifact = prebuilt.path().to_path_buf();
            let label = format!(
                "native prebuilt `{}`",
                artifact.file_name().unwrap_or_default().display()
            );
            out.prebuilts.push(std::sync::Arc::new(prebuilt));
            (artifact, label)
        } else {
            out.source = stage_registry_source(root, manifest)?;
            let source = out.source.as_ref().map_or(root, |source| source.path());
            let mut paths = hew_compile::source_path_policy();
            if paths.prefixes().next().is_some() {
                let display = paths.display(root);
                let display = if display == root {
                    PathBuf::from("packages")
                        .join(package)
                        .join(&manifest.package.version)
                } else {
                    display
                };
                paths.insert(source, &display);
            }
            let (artifact, lib) = build_rust_crate(
                source,
                &rust,
                &embedded_rustc_identity(),
                &toolchain.target,
                &paths,
            )?;
            (artifact, format!("Rust crate `{lib}`"))
        };
        refuse_reserved_symbols(&artifact, &label, package)?;
        out.dependencies.push(artifact.clone());
        out.dependencies.extend(
            cargo_dep_info(&artifact.with_extension("d"))
                .into_iter()
                .filter_map(|path| {
                    let Some(source) = &out.source else {
                        return Some(path);
                    };
                    match path.strip_prefix(source.path()) {
                        Ok(relative) => {
                            let original = root.join(relative);
                            original.is_file().then_some(original)
                        }
                        Err(_) => Some(path),
                    }
                }),
        );
        out.inputs.push(artifact);
    }
    let inputs = native.inputs_for(toolchain.os);
    let compile_flags = pkg_config(package, &inputs.pkg_config, "--cflags")?;
    let link_flags = pkg_config(package, &inputs.pkg_config, "--libs")?;
    if !inputs.sources.is_empty() {
        let sources = SourceSet {
            root,
            package,
            inputs: &inputs,
            pkg_cflags: &compile_flags,
        };
        sources.compile(toolchain, &mut out)?;
    }
    for dir in &inputs.lib_dirs {
        out.link_args
            .push(format!("-L{}", root.join(dir).display()));
    }
    out.link_args.extend(link_flags);
    for lib in &inputs.link_libs {
        out.link_args.push(match LinkLib::of(lib) {
            LinkLib::Name(name) => format!("-l{name}"),
            LinkLib::Path(path) => root.join(path).display().to_string(),
        });
    }
    Ok(Some(out))
}

// ── C, C++ and assembly sources ──────────────────────────────────────────────────────

struct SourceSet<'a> {
    root: &'a Path,
    package: &'a str,
    inputs: &'a NativeInputs,
    pkg_cflags: &'a [String],
}

impl SourceSet<'_> {
    fn compile(&self, toolchain: &NativeToolchain, out: &mut NativeBuild) -> Result<(), String> {
        let c_args = self.args(toolchain, SourceLanguage::C);
        let cxx_args = self.args(toolchain, SourceLanguage::Cxx);
        let asm_args = self.args(toolchain, SourceLanguage::Assembly);
        let asm_cpp_args = self.args(toolchain, SourceLanguage::AssemblyWithCpp);
        let object_dir = self.object_dir(&command_key(
            self.root,
            &toolchain.driver,
            &c_args,
            &cxx_args,
            &asm_args,
            &asm_cpp_args,
        ));
        for source in &self.inputs.sources {
            let language = SourceLanguage::of(source)
                .ok_or_else(|| format!("{source}: not a C, C++ or assembly source"))?;
            let language = match language {
                SourceLanguage::Assembly | SourceLanguage::AssemblyWithCpp => {
                    assembly_language(language, &self.inputs.asflags)
                        .map_err(|e| self.compile_error(source, &e))?
                }
                other => other,
            };
            let path = self.root.join(source);
            if !path.is_file() {
                return Err(format!(
                    "error[E_NATIVE_COMPILE]: package `{}` lists source `{source}` in [native], \
                     but {} does not exist",
                    self.package,
                    path.display()
                ));
            }
            let object = object_dir.join(format!("{source}{}", toolchain.object_suffix));
            let depfile = object_dir.join(format!("{source}.d"));
            let args = match language {
                SourceLanguage::C => &c_args,
                SourceLanguage::Cxx => &cxx_args,
                SourceLanguage::Assembly => &asm_args,
                SourceLanguage::AssemblyWithCpp => &asm_cpp_args,
            };
            // Integrated assemblers cannot report `.include` dependencies.
            // Rebuild assembly rather than reuse an object with hidden inputs.
            let assembly = matches!(
                language,
                SourceLanguage::Assembly | SourceLanguage::AssemblyWithCpp
            );
            if assembly || fresh_dependencies(&object, &depfile).is_none() {
                let debug_object_name = (toolchain.os == NativeOs::Windows
                    && language != SourceLanguage::Assembly
                    && hew_compile::source_path_policy()
                        .prefixes()
                        .next()
                        .is_some())
                .then(|| format!("{source}{}", toolchain.object_suffix));
                compile_one(
                    &toolchain.driver,
                    args,
                    &path,
                    &object,
                    &depfile,
                    language,
                    debug_object_name.as_deref(),
                )
                .map_err(|stderr| self.compile_error(source, &stderr))?;
            }
            refuse_reserved_symbols(&object, &format!("`{source}`"), self.package)?;
            out.dependencies.push(path);
            out.dependencies.extend(read_depfile(&depfile));
            out.compiles_cxx |= language == SourceLanguage::Cxx;
            out.inputs.push(object);
        }
        Ok(())
    }

    /// Objects live under a directory named for the package root and the full
    /// command line, so a changed flag, define, target or profile never reuses
    /// an object, and neither does a copy of the package tree.
    ///
    /// The directory is `target/native/<key>` in the package; a package whose
    /// directory is read-only builds under the system temporary directory
    /// instead.
    fn object_dir(&self, key: &str) -> PathBuf {
        let local = self.root.join("target").join("native").join(key);
        if std::fs::create_dir_all(&local).is_ok() {
            return local;
        }
        std::env::temp_dir().join("hew-native").join(key)
    }

    fn args(&self, toolchain: &NativeToolchain, language: SourceLanguage) -> Vec<String> {
        let mut args = toolchain.flags.clone();
        let paths = hew_compile::source_path_policy();
        for (source, display) in paths.prefixes().rev() {
            args.push(format!(
                "-ffile-prefix-map={}={}",
                source.display(),
                display.display()
            ));
        }
        args.extend_from_slice(self.pkg_cflags);
        for dir in &self.inputs.include_dirs {
            args.push(format!("-I{}", self.root.join(dir).display()));
        }
        if language != SourceLanguage::Assembly {
            args.extend(self.inputs.defines.iter().map(|d| format!("-D{d}")));
        }
        if matches!(
            language,
            SourceLanguage::Assembly | SourceLanguage::AssemblyWithCpp
        ) {
            args.extend([
                "-x".to_string(),
                if language == SourceLanguage::Assembly {
                    "assembler"
                } else {
                    "assembler-with-cpp"
                }
                .to_string(),
            ]);
        }
        args.extend_from_slice(match language {
            SourceLanguage::C => &self.inputs.cflags,
            SourceLanguage::Cxx => &self.inputs.cxxflags,
            SourceLanguage::Assembly | SourceLanguage::AssemblyWithCpp => &self.inputs.asflags,
        });
        if toolchain.os == NativeOs::Windows && paths.prefixes().next().is_some() {
            // COFF timestamps and the raw compiler command are outside prefix
            // mapping. Keep debug records without embedding these build inputs.
            args.push("-mno-incremental-linker-compatible".to_string());
            if language != SourceLanguage::Assembly {
                args.push("-gno-codeview-command-line".to_string());
            }
        }
        args
    }

    fn compile_error(&self, source: &str, stderr: &str) -> String {
        format!(
            "error[E_NATIVE_COMPILE]: cannot compile `{source}` for package `{}`\n{}",
            self.package,
            stderr.trim_end()
        )
    }
}

/// Honour the driver's last explicit language selection, including the GNU
/// joined spelling, before choosing preprocessing and dependency flags.
fn assembly_language(default: SourceLanguage, flags: &[String]) -> Result<SourceLanguage, String> {
    let mut language = default;
    let mut flags = flags.iter();
    while let Some(flag) = flags.next() {
        let Some(joined) = flag.strip_prefix("-x") else {
            continue;
        };
        let name = if joined.is_empty() {
            flags
                .next()
                .map(String::as_str)
                .ok_or("asflags -x requires an assembly language")?
        } else {
            joined
        };
        language = match name {
            "assembler" => SourceLanguage::Assembly,
            "assembler-with-cpp" => SourceLanguage::AssemblyWithCpp,
            _ => {
                return Err(format!(
                    "asflags -x expects assembler or assembler-with-cpp, got {name}"
                ))
            }
        };
    }
    Ok(language)
}

fn command_key(
    root: &Path,
    driver: &str,
    c_args: &[String],
    cxx_args: &[String],
    asm_args: &[String],
    asm_cpp_args: &[String],
) -> String {
    let mut hasher = Sha256::new();
    // Dependency files name sources by absolute path, so an object built in
    // one location must not be reused from another.
    hasher.update(root.as_os_str().as_encoded_bytes());
    hasher.update([0]);
    for part in std::iter::once(driver)
        .chain(c_args.iter().map(String::as_str))
        .chain(std::iter::once("\0cxx"))
        .chain(cxx_args.iter().map(String::as_str))
        .chain(std::iter::once("\0asm"))
        .chain(asm_args.iter().map(String::as_str))
        .chain(std::iter::once("\0asm-cpp"))
        .chain(asm_cpp_args.iter().map(String::as_str))
    {
        hasher.update(part.as_bytes());
        hasher.update([0]);
    }
    hasher.finalize()[..8]
        .iter()
        .fold(String::new(), |mut key, byte| {
            let _ = write!(key, "{byte:02x}");
            key
        })
}

/// Compile `source` to `object`, writing its header dependencies to
/// `depfile`. Both land through a temporary name so a concurrent build of the
/// same package never links a half-written object.
fn compile_one(
    driver: &str,
    args: &[String],
    source: &Path,
    object: &Path,
    depfile: &Path,
    language: SourceLanguage,
    debug_object_name: Option<&str>,
) -> Result<(), String> {
    let parent = object
        .parent()
        .ok_or_else(|| format!("object path has no parent: {}", object.display()))?;
    std::fs::create_dir_all(parent)
        .map_err(|e| format!("cannot create {}: {e}", parent.display()))?;
    let serial = NEXT_TEMPORARY.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    let unique = format!(".{}-{serial}.tmp", std::process::id());
    let object_tmp = append(object, &unique);
    let depfile_tmp = append(depfile, &unique);
    let mut command = Command::new(driver);
    command
        .args(args)
        .arg("-c")
        .arg(source)
        .arg("-o")
        .arg(&object_tmp);
    if let Some(name) = debug_object_name {
        // S_OBJNAME describes the logical object, not its atomic temporary file.
        command
            .arg("-Xclang")
            .arg(format!("-object-file-name={name}"));
    }
    if language != SourceLanguage::Assembly {
        command.arg("-MMD").arg("-MF").arg(&depfile_tmp);
    }
    let output = command
        .output()
        .map_err(|e| format!("cannot run the C compiler `{driver}`: {e}"))?;
    let stderr =
        hew_compile::source_path_policy().display_text(&String::from_utf8_lossy(&output.stderr));
    if !output.status.success() {
        let _ = std::fs::remove_file(&object_tmp);
        let _ = std::fs::remove_file(&depfile_tmp);
        return Err(stderr);
    }
    // Warnings are build output the package asked for (`-Wall` and friends).
    if !stderr.trim().is_empty() {
        eprint!("{stderr}");
    }
    if language != SourceLanguage::Assembly {
        std::fs::rename(&depfile_tmp, depfile)
            .map_err(|e| format!("cannot write {}: {e}", depfile.display()))?;
    }
    std::fs::rename(&object_tmp, object)
        .map_err(|e| format!("cannot write {}: {e}", object.display()))
}

fn append(path: &Path, suffix: &str) -> PathBuf {
    let mut name = path.as_os_str().to_owned();
    name.push(suffix);
    PathBuf::from(name)
}

/// The object's dependencies when it is newer than every one of them; `None`
/// when it must be rebuilt.
fn fresh_dependencies(object: &Path, depfile: &Path) -> Option<Vec<PathBuf>> {
    let built = std::fs::metadata(object).ok()?.modified().ok()?;
    let deps = read_depfile(depfile);
    if deps.is_empty() {
        return None;
    }
    deps.iter()
        .all(|dep| {
            std::fs::metadata(dep)
                .and_then(|meta| meta.modified())
                .is_ok_and(|modified| modified <= built)
        })
        .then_some(deps)
}

fn read_depfile(path: &Path) -> Vec<PathBuf> {
    std::fs::read_to_string(path)
        .map(|text| parse_depfile(&text))
        .unwrap_or_default()
}

/// The prerequisites of a Make-format dependency file, as written by
/// `cc -MMD` and Cargo: `target: dep dep \` with `\ ` escaping a space and
/// `$$` a dollar sign.
fn parse_depfile(text: &str) -> Vec<PathBuf> {
    let joined = text.replace("\\\r\n", " ").replace("\\\n", " ");
    let mut deps = Vec::new();
    for line in joined.lines() {
        // The target ends at the first `:` followed by whitespace or the end
        // of the line, so a Windows drive (`C:\`) stays inside a path.
        let bytes = line.as_bytes();
        let Some(colon) = (0..bytes.len())
            .find(|&i| bytes[i] == b':' && bytes.get(i + 1).is_none_or(u8::is_ascii_whitespace))
        else {
            continue;
        };
        let mut token = String::new();
        let mut chars = line[colon + 1..].chars().peekable();
        while let Some(c) = chars.next() {
            match c {
                '\\' if chars
                    .peek()
                    .is_some_and(|next| *next == ' ' || *next == '#') =>
                {
                    token.push(chars.next().unwrap_or(' '));
                }
                '$' if chars.peek() == Some(&'$') => {
                    chars.next();
                    token.push('$');
                }
                c if c.is_whitespace() => {
                    if !token.is_empty() {
                        deps.push(PathBuf::from(std::mem::take(&mut token)));
                    }
                }
                c => token.push(c),
            }
        }
        if !token.is_empty() {
            deps.push(PathBuf::from(token));
        }
    }
    deps
}

/// Cargo writes `<artifact>.d` beside each library it builds; it lists the
/// crate's Rust sources. A missing file only means no Rust sources reach the
/// dependency file.
fn cargo_dep_info(path: &Path) -> Vec<PathBuf> {
    read_depfile(path)
}

// ── Reserved runtime symbols ───────────────────────────────────────────────

/// Refuse a `hew_` definition in `artifact`, an object file, a Rust static
/// library or a Rust dynamic library. `origin` names what was built, such as
/// `` `bridge.c` `` or ``Rust crate `p_native` ``.
fn refuse_reserved_symbols(artifact: &Path, origin: &str, package: &str) -> Result<(), String> {
    if in_runtime_namespace(package) {
        return Ok(());
    }
    let reserved = reserved_definitions(artifact)?;
    if reserved.is_empty() {
        return Ok(());
    }
    let prefix = symbol_prefix(package);
    let mut message = String::new();
    for symbol in &reserved {
        let renamed = format!("{prefix}{}", &symbol[RESERVED_PREFIX.len()..]);
        let _ = write!(
            message,
            "error[E_RESERVED_NATIVE_SYMBOL]: {origin} in package `{package}` defines \
             `{symbol}`, but the `{RESERVED_PREFIX}` symbol prefix is reserved for the Hew \
             runtime\n  help: rename it to `{renamed}` here and in the `extern \"C\"` block \
             that declares it\n"
        );
    }
    Err(message.trim_end().to_string())
}

/// Whether `package` belongs to the `hew.` ecosystem namespace, whose
/// packages are the runtime's own and may define `hew_` symbols. The older
/// `hew::` spelling of a package name counts too.
fn in_runtime_namespace(package: &str) -> bool {
    package == "hew" || package.starts_with("hew.") || package.starts_with("hew::")
}

/// Global symbols `artifact` defines under the reserved prefix: functions,
/// data, thread-local variables and common symbols, in an object, or in every
/// member of an archive.
fn reserved_definitions(artifact: &Path) -> Result<Vec<String>, String> {
    use object::read::archive::ArchiveFile;
    use object::FileKind;

    let shown = artifact.display();
    let data = std::fs::read(artifact).map_err(|e| format!("cannot read {shown}: {e}"))?;
    let mut reserved = BTreeSet::new();
    if matches!(FileKind::parse(&*data), Ok(FileKind::Archive)) {
        let archive =
            ArchiveFile::parse(&*data).map_err(|e| format!("cannot read archive {shown}: {e}"))?;
        for member in archive.members() {
            let member = member.map_err(|e| format!("cannot read archive {shown}: {e}"))?;
            let bytes = member
                .data(&*data)
                .map_err(|e| format!("cannot read archive {shown}: {e}"))?;
            // Members that are not object files (bitcode, metadata) define
            // no linkable symbols.
            if let Ok(file) = object::File::parse(bytes) {
                collect_reserved(&file, &mut reserved);
            }
        }
    } else {
        let file =
            object::File::parse(&*data).map_err(|e| format!("cannot read object {shown}: {e}"))?;
        collect_reserved(&file, &mut reserved);
    }
    Ok(reserved.into_iter().collect())
}

fn collect_reserved(file: &object::File<'_>, reserved: &mut BTreeSet<String>) {
    use object::{Object as _, ObjectSymbol as _, SymbolKind};

    // Mach-O spells every C symbol with a leading underscore.
    let mangling = match file.format() {
        object::BinaryFormat::MachO => "_",
        _ => "",
    };
    let mut note = |name: &[u8]| {
        let reserved_name = std::str::from_utf8(name)
            .ok()
            .and_then(|name| name.strip_prefix(mangling))
            .filter(|name| name.starts_with(RESERVED_PREFIX));
        if let Some(name) = reserved_name {
            reserved.insert(name.to_string());
        }
    };
    // `is_definition` leaves out thread-local and common symbols, so a
    // definition is any global symbol that is not undefined.
    for symbol in file.symbols().chain(file.dynamic_symbols()) {
        if symbol.is_undefined()
            || !symbol.is_global()
            || matches!(symbol.kind(), SymbolKind::File | SymbolKind::Section)
        {
            continue;
        }
        note(symbol.name_bytes().unwrap_or_default());
    }
    // A PE library lists its symbols only in the export table.
    if let Ok(exports) = file.exports() {
        for export in exports.flatten() {
            if let object::NameOrOrdinal::Name(name) = export.name() {
                note(name);
            }
        }
    }
}

/// The symbol prefix to suggest for `package`: its name with every
/// non-identifier character as `_`, so `meshcore.roles` suggests
/// `meshcore_roles_`.
fn symbol_prefix(package: &str) -> String {
    let spelled: String = package
        .chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() {
                c.to_ascii_lowercase()
            } else {
                '_'
            }
        })
        .collect();
    let prefix = spelled
        .split('_')
        .filter(|part| !part.is_empty())
        .collect::<Vec<_>>();
    if prefix.is_empty() {
        return "pkg_".to_string();
    }
    format!("{}_", prefix.join("_"))
}

// ── pkg-config ─────────────────────────────────────────────────────────────

/// Run `pkg-config <flag> <names>` (or `$PKG_CONFIG`) and split its output.
fn pkg_config(package: &str, names: &[String], flag: &str) -> Result<Vec<String>, String> {
    if names.is_empty() {
        return Ok(Vec::new());
    }
    let program = std::env::var("PKG_CONFIG").unwrap_or_else(|_| "pkg-config".to_string());
    let listed = names.join(", ");
    let output = Command::new(&program)
        .arg(flag)
        .args(names)
        .output()
        .map_err(|error| {
            format!(
                "error[E_NATIVE_PKG_CONFIG]: package `{package}` finds {listed} with pkg-config, \
                 but `{program}` cannot run: {error}\n  help: install pkg-config (or pkgconf), \
                 or name the libraries with `link-libs` and `lib-dirs` in a \
                 [native.<os>] table"
            )
        })?;
    if !output.status.success() {
        return Err(format!(
            "error[E_NATIVE_PKG_CONFIG]: package `{package}` needs {listed}, which pkg-config \
             cannot find\n{}",
            String::from_utf8_lossy(&output.stderr).trim_end()
        ));
    }
    Ok(split_flags(&String::from_utf8_lossy(&output.stdout)))
}

/// Split pkg-config output into arguments: whitespace separates them and `\`
/// escapes the next character.
fn split_flags(text: &str) -> Vec<String> {
    let mut flags = Vec::new();
    let mut flag = String::new();
    let mut chars = text.chars();
    while let Some(c) = chars.next() {
        match c {
            '\\' => flag.extend(chars.next()),
            c if c.is_whitespace() => {
                if !flag.is_empty() {
                    flags.push(std::mem::take(&mut flag));
                }
            }
            c => flag.push(c),
        }
    }
    if !flag.is_empty() {
        flags.push(flag);
    }
    flags
}

// ── Rust crates ────────────────────────────────────────────────────────────

/// The release and host identity of a rustc toolchain.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustcIdentity {
    pub release: String,
    pub host: String,
}

impl std::fmt::Display for RustcIdentity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "rustc {} ({})", self.release, self.host)
    }
}

impl RustcIdentity {
    /// Parse the `<release> <host>` stamp `hew-pkg/build.rs` embeds via
    /// `cargo:rustc-env=HEW_RUNTIME_RUSTC`.
    fn parse_stamp(text: &str) -> Option<Self> {
        let (release, host) = text.trim().split_once(' ')?;
        Some(Self {
            release: release.to_string(),
            host: host.to_string(),
        })
    }

    /// Parse the `release:`/`host:` lines out of full `rustc -vV` output.
    fn parse_verbose(text: &str) -> Option<Self> {
        let field = |name: &str| -> Option<String> {
            let prefix = format!("{name}: ");
            text.lines()
                .find_map(|line| line.strip_prefix(prefix.as_str()))
                .map(str::trim)
                .map(str::to_string)
        };
        Some(Self {
            release: field("release")?,
            host: field("host")?,
        })
    }
}

/// The rustc identity embedded while building the Hew runtime.
///
/// # Panics
///
/// Panics if the build stamp is malformed.
#[must_use]
pub fn embedded_rustc_identity() -> RustcIdentity {
    RustcIdentity::parse_stamp(env!("HEW_RUNTIME_RUSTC")).unwrap_or_else(|| {
        panic!(
            "HEW_RUNTIME_RUSTC={:?} embedded by hew-pkg/build.rs is malformed",
            env!("HEW_RUNTIME_RUSTC")
        )
    })
}

fn stage_registry_source(
    root: &Path,
    manifest: &HewManifest,
) -> Result<Option<std::sync::Arc<crate::atomic_fs::PinnedDir>>, String> {
    let Some(provenance) = crate::registry::source_provenance(root, manifest)? else {
        return Ok(None);
    };
    let slot = root.join("target/native-source");
    let staged = crate::atomic_fs::StagedDir::new(&slot).map_err(|error| error.to_string())?;
    crate::tarball::unpack(&provenance.archive, staged.path())
        .map_err(|error| error.to_string())?;
    let pinned = staged
        .publish_pinned(&slot)
        .map_err(|error| error.to_string())?;
    Ok(Some(std::sync::Arc::new(pinned)))
}

/// Query the rustc selected for a native crate directory.
fn crate_rustc_identity(
    crate_dir: &Path,
    rustc: &std::ffi::OsStr,
) -> Result<RustcIdentity, String> {
    let output = Command::new(rustc)
        .arg("-vV")
        .current_dir(crate_dir)
        .output()
        .map_err(|e| format!("failed to run `rustc -vV`: {e}"))?;
    if !output.status.success() {
        return Err(format!(
            "`rustc -vV` failed: {}",
            String::from_utf8_lossy(&output.stderr)
        ));
    }
    let text = String::from_utf8(output.stdout)
        .map_err(|e| format!("`rustc -vV` produced non-UTF-8 output: {e}"))?;
    RustcIdentity::parse_verbose(&text)
        .ok_or_else(|| format!("could not parse `rustc -vV` output:\n{text}"))
}

/// Library crate types a `[native]` Rust crate may build, in the order the
/// build prefers them. Cargo.toml's `[lib] crate-type` decides which it has.
const LINKABLE_CRATE_TYPES: [&str; 2] = ["staticlib", "cdylib"];

/// Check that the `[native]` Rust crate of the package at `root`, if it
/// declares one, builds a linkable library: its library target's crate types
/// include `staticlib` or `cdylib`. Reads `cargo metadata` without building,
/// so `hew check` and `hew publish` refuse an unlinkable crate up front.
///
/// # Errors
///
/// Returns an `E_INVALID_NATIVE` diagnostic when the crate has no
/// `Cargo.toml`, `cargo metadata` fails, or no target builds a linkable type.
pub fn check_crate_shape(root: &Path, manifest: &HewManifest) -> Result<(), String> {
    let Some(rust) = manifest
        .native
        .as_ref()
        .and_then(crate::manifest::NativeLib::rust_crate)
    else {
        return Ok(());
    };
    let cargo_toml = crate_manifest(root, &rust)?;
    let out = Command::new("cargo")
        .args([
            "metadata",
            "--no-deps",
            "--offline",
            "--format-version",
            "1",
            "--manifest-path",
        ])
        .arg(&cargo_toml)
        .current_dir(cargo_toml.parent().unwrap_or_else(|| Path::new(".")))
        .output()
        .map_err(|e| format!("E_INVALID_NATIVE: failed to run cargo metadata: {e}"))?;
    if !out.status.success() {
        return Err(format!(
            "E_INVALID_NATIVE: cargo metadata failed for [native] crate {}:\n{}",
            cargo_toml.display(),
            String::from_utf8_lossy(&out.stderr).trim_end()
        ));
    }
    let metadata: serde_json::Value = serde_json::from_slice(&out.stdout)
        .map_err(|e| format!("E_INVALID_NATIVE: invalid cargo metadata: {e}"))?;
    let package = metadata["packages"]
        .as_array()
        .into_iter()
        .flatten()
        .find(|package| {
            package["manifest_path"]
                .as_str()
                .is_some_and(|path| same_file(Path::new(path), &cargo_toml))
        })
        .ok_or_else(|| {
            format!(
                "E_INVALID_NATIVE: cargo metadata does not describe {}",
                cargo_toml.display()
            )
        })?;
    let linkable = package["targets"]
        .as_array()
        .into_iter()
        .flatten()
        .any(|target| linkable_crate_type(target).is_some());
    if linkable {
        Ok(())
    } else {
        Err(format!(
            "E_INVALID_NATIVE: [native] crate {} builds no linkable library; add \
             `crate-type = [\"staticlib\"]` (or `\"cdylib\"`) to its [lib] table",
            cargo_toml.display()
        ))
    }
}

/// The preferred linkable crate type of a Cargo target description (from
/// `cargo metadata` or a `compiler-artifact` message), if it has one.
fn linkable_crate_type(target: &serde_json::Value) -> Option<&'static str> {
    let types: Vec<&str> = target["crate_types"]
        .as_array()
        .into_iter()
        .flatten()
        .filter_map(serde_json::Value::as_str)
        .collect();
    LINKABLE_CRATE_TYPES
        .into_iter()
        .find(|linkable| types.contains(linkable))
}

/// The crate's `Cargo.toml`, which must exist.
fn crate_manifest(root: &Path, rust: &RustCrate<'_>) -> Result<PathBuf, String> {
    let crate_dir = root.join(rust.dir);
    let cargo_toml = crate_dir.join("Cargo.toml");
    if cargo_toml.is_file() {
        Ok(cargo_toml)
    } else {
        Err(format!(
            "E_INVALID_NATIVE: [native] crate at {} has no Cargo.toml",
            crate_dir.display()
        ))
    }
}

/// Whether two paths name the same file, comparing canonical forms when both
/// exist.
fn same_file(left: &Path, right: &Path) -> bool {
    match (left.canonicalize(), right.canonicalize()) {
        (Ok(left), Ok(right)) => left == right,
        _ => left == right,
    }
}

/// Whether `file` is the library Cargo produced for `crate_type`: the archive
/// for `staticlib`, the shared library for `cdylib`, never an import library.
pub(crate) fn is_library_file(file: &Path, crate_type: &str) -> bool {
    let extension = |path: &Path| {
        path.extension()
            .and_then(|extension| extension.to_str())
            .map(str::to_ascii_lowercase)
    };
    // `foo.dll.lib` and `foo.dll.a` link against `foo.dll`; they are not it.
    let import_library = file
        .file_stem()
        .and_then(|stem| extension(Path::new(stem)))
        .is_some_and(|stem_extension| stem_extension == "dll");
    match (crate_type, extension(file).as_deref()) {
        ("staticlib", Some("a" | "lib")) => !import_library,
        ("cdylib", Some("so" | "dylib" | "dll")) => true,
        _ => false,
    }
}

/// Build a `[native]` Rust crate with Cargo's non-LTO `release-lib` profile
/// and return the produced library and its target name.
///
/// The artifact is the one Cargo reports: the `compiler-artifact` message for
/// the crate's own library target, its `staticlib` file if it builds one,
/// otherwise its `cdylib`. Cargo.toml is the one authority on the library's
/// name and type.
///
/// `expected` is the rustc identity `libhew.a` was built with: production
/// passes [`embedded_rustc_identity`]; tests pass a deliberately mismatching
/// value.
pub(crate) fn build_rust_crate(
    root: &Path,
    rust: &RustCrate<'_>,
    expected: &RustcIdentity,
    target: &str,
    paths: &hew_compile::SourcePathPolicy,
) -> Result<(PathBuf, String), String> {
    let cargo_toml = crate_manifest(root, rust)?;
    let crate_dir = root.join(rust.dir);

    // Refuse before cargo builds a static library with an incompatible libstd.
    let rustc = std::env::var_os("RUSTC").unwrap_or_else(|| "rustc".into());
    let actual = crate_rustc_identity(&crate_dir, &rustc).map_err(|error| format!(
        "error[E_NATIVE_TOOLCHAIN]: no compatible native prebuilt is available; [native] crate at {} requires {expected}. Install Cargo and run `rustup toolchain install {}`. {error}",
        crate_dir.display(), expected.release,
    ))?;
    if actual != *expected {
        return Err(format!(
            "error[E_NATIVE_TOOLCHAIN]: [native] crate at {} would build with {actual}, \
             but the Hew runtime (libhew.a) was built with {expected}. A mismatched rustc \
             embeds an incompatible libstd, and the final link would fail on a duplicate \
             `rust_eh_personality` symbol. Fix: pin {}/rust-toolchain.toml to release {}.",
            crate_dir.display(),
            crate_dir.display(),
            expected.release,
        ));
    }

    // Run cargo from the crate directory (not just `--manifest-path`) so that
    // rustup resolves the package's `rust-toolchain.toml` by walking up from
    // the crate dir. The native staticlib must be built with the *same* rustc
    // as `libhew.a` so its embedded `libstd` is byte-identical and the linker
    // dedups `rust_eh_personality`; a mismatched toolchain re-introduces a
    // duplicate-symbol link failure. Define `release-lib` on the Cargo
    // command line so standalone packages inherit their own release settings
    // while always disabling LTO for a consumer-linkable archive. Stdout
    // carries Cargo's JSON messages; rendered diagnostics arrive on stderr.
    let mut command = Command::new("cargo");
    command
        .env("RUSTC", &rustc)
        .args([
            "rustc",
            "--lib",
            "--profile",
            "release-lib",
            "--config",
            r#"profile.release-lib.inherits="release""#,
            "--config",
            "profile.release-lib.lto=false",
            "--message-format=json-render-diagnostics",
            "--manifest-path",
        ])
        .arg(&cargo_toml)
        .args(["--target", target])
        .current_dir(&crate_dir)
        .stdin(std::process::Stdio::null());
    if paths.prefixes().next().is_some() {
        // Cargo's effective flags include environment, target and hierarchical
        // config precedence. Preserve them before adding dependency-wide remaps.
        let config = cargo_config2::Config::load_with_options(
            &crate_dir,
            cargo_config2::ResolveOptions::default().rustc(cargo_config2::PathAndArgs::new(&rustc)),
        )
        .map_err(|e| format!("failed to read native Cargo configuration: {e}"))?;
        let mut flags = config
            .rustflags(target)
            .map_err(|e| format!("failed to resolve native Cargo rustflags: {e}"))?
            .unwrap_or_default();
        for (source, display) in paths.prefixes().rev() {
            flags.push(format!(
                "--remap-path-prefix={}={}",
                source.display(),
                display.display()
            ));
        }
        command.env(
            "CARGO_ENCODED_RUSTFLAGS",
            flags
                .encode()
                .map_err(|e| format!("failed to encode native Cargo rustflags: {e}"))?,
        );
    }
    let output = command
        .output()
        .map_err(|e| format!("failed to run cargo: {e}"))?;
    eprint!(
        "{}",
        paths.display_text(&String::from_utf8_lossy(&output.stderr))
    );
    if !output.status.success() {
        return Err(format!(
            "cargo build failed for [native] crate {}",
            crate_dir.display()
        ));
    }
    built_library(&output.stdout, &cargo_toml).ok_or_else(|| {
        format!(
            "E_INVALID_NATIVE: [native] crate {} built no staticlib or cdylib; set \
             `crate-type` in its [lib] table",
            cargo_toml.display()
        )
    })
}

/// The library file and target name from Cargo's JSON messages for the crate
/// whose manifest is `cargo_toml`.
fn built_library(messages: &[u8], cargo_toml: &Path) -> Option<(PathBuf, String)> {
    String::from_utf8_lossy(messages)
        .lines()
        .filter_map(|line| serde_json::from_str::<serde_json::Value>(line).ok())
        .filter(|message| {
            message["reason"] == "compiler-artifact"
                && message["manifest_path"]
                    .as_str()
                    .is_some_and(|path| same_file(Path::new(path), cargo_toml))
        })
        .find_map(|message| {
            let crate_type = linkable_crate_type(&message["target"])?;
            let file = message["filenames"]
                .as_array()?
                .iter()
                .filter_map(serde_json::Value::as_str)
                .map(PathBuf::from)
                .find(|file| is_library_file(file, crate_type))?;
            let name = message["target"]["name"].as_str()?.to_string();
            Some((file, name))
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn crate_with_types(crate_types: &str) -> (tempfile::TempDir, HewManifest) {
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(dir.path().join("native/src")).unwrap();
        std::fs::write(
            dir.path().join("native/Cargo.toml"),
            format!(
                "[workspace]\n\n[package]\nname = \"p\"\nversion = \"0.1.0\"\nedition = \"2021\"\n\
                 [lib]\nname = \"p_native\"\ncrate-type = [{crate_types}]\n"
            ),
        )
        .unwrap();
        std::fs::write(dir.path().join("native/src/lib.rs"), "").unwrap();
        let manifest: HewManifest = toml::from_str(
            "[package]\nname = \"p\"\nversion = \"0.1.0\"\n\n[native]\ncrate = \"native\"\n",
        )
        .unwrap();
        (dir, manifest)
    }

    #[test]
    fn trimmed_coff_objects_are_reproducible_across_roots() {
        use object::{Object as _, ObjectSection as _};

        let dir = tempfile::tempdir().unwrap();
        let roots = [
            dir.path().join("first"),
            dir.path().join("a longer second root"),
        ];
        let manifest: HewManifest = toml::from_str(
            "[package]\nname = \"paths\"\nversion = \"0.1.0\"\n\n[native]\nsources = [\"native/path.c\"]\n\
             cflags = [\"-gcodeview-command-line\", \"-mincremental-linker-compatible\"]\n",
        )
        .unwrap();
        for root in &roots {
            std::fs::create_dir_all(root.join("native")).unwrap();
            std::fs::write(
                root.join("native/path.c"),
                "const char *source_path = __FILE__; int answer(int n) { return n + 40; }\n",
            )
            .unwrap();
        }
        for profile in ["-O0", "-O2"] {
            let mut objects = Vec::new();
            for root in &roots {
                let mut paths = hew_compile::SourcePathPolicy::default();
                paths.insert(root, Path::new("."));
                let _paths = paths.enter();
                let toolchain = NativeToolchain {
                    target: "x86_64-pc-windows-msvc".to_string(),
                    lockfile: None,
                    os: NativeOs::Windows,
                    driver: "clang".to_string(),
                    flags: [
                        "-target",
                        "x86_64-pc-windows-msvc",
                        profile,
                        "-g",
                        "-fms-runtime-lib=dll",
                    ]
                    .map(str::to_string)
                    .to_vec(),
                    object_suffix: ".obj",
                };
                let built = build(root, &manifest, &toolchain).unwrap().unwrap();
                let bytes = std::fs::read(&built.inputs[0]).unwrap();
                let object = object::File::parse(bytes.as_slice()).unwrap();
                assert_eq!(object.format(), object::BinaryFormat::Coff);
                assert!(object.section_by_name(".debug$T").is_some());
                let symbols = object.section_by_name(".debug$S").unwrap();
                let symbols = symbols.data().unwrap();
                assert!(symbols
                    .windows(b"native/path.c.obj".len())
                    .any(|s| s == b"native/path.c.obj"));
                assert_eq!(
                    u32::from_le_bytes(bytes[4..8].try_into().unwrap()),
                    0,
                    "COFF timestamp"
                );
                objects.push(bytes);
            }
            assert!(
                objects[0] == objects[1],
                "{profile}: trimmed COFF object bytes across roots"
            );
        }
    }

    #[test]
    fn crate_shape_requires_a_linkable_library() {
        for linkable in ["\"staticlib\"", "\"cdylib\"", "\"rlib\", \"staticlib\""] {
            let (dir, manifest) = crate_with_types(linkable);
            check_crate_shape(dir.path(), &manifest).unwrap_or_else(|e| panic!("{linkable}: {e}"));
        }
        let (dir, manifest) = crate_with_types("\"rlib\"");
        let error = check_crate_shape(dir.path(), &manifest).unwrap_err();
        assert!(error.starts_with("E_INVALID_NATIVE"), "{error}");
        assert!(error.contains("builds no linkable library"), "{error}");
        assert!(!dir.path().join("native/target").exists(), "metadata only");

        std::fs::remove_file(dir.path().join("native/Cargo.toml")).unwrap();
        let error = check_crate_shape(dir.path(), &manifest).unwrap_err();
        assert!(error.contains("has no Cargo.toml"), "{error}");
    }

    #[test]
    fn built_library_is_the_crate_s_own_linkable_artifact() {
        let dir = tempfile::tempdir().unwrap();
        let cargo_toml = dir.path().join("Cargo.toml");
        std::fs::write(&cargo_toml, "").unwrap();
        let other = dir.path().join("dep/Cargo.toml");
        let message = |manifest: &Path, name: &str, types: &[&str], files: &[&str]| {
            serde_json::json!({
                "reason": "compiler-artifact",
                "manifest_path": manifest,
                "target": { "name": name, "crate_types": types },
                "filenames": files,
            })
            .to_string()
        };
        let messages = [
            r#"{"reason":"build-script-executed"}"#.to_string(),
            message(&other, "dep", &["staticlib"], &["/t/libdep.a"]),
            message(
                &cargo_toml,
                "foo_native",
                &["rlib", "cdylib", "staticlib"],
                &[
                    "/t/libfoo_native.rlib",
                    "/t/foo_native.dll",
                    "/t/foo_native.dll.lib",
                    "/t/foo_native.lib",
                ],
            ),
        ]
        .join("\n");
        assert_eq!(
            built_library(messages.as_bytes(), &cargo_toml),
            Some((PathBuf::from("/t/foo_native.lib"), "foo_native".to_string()))
        );

        let cdylib = message(
            &cargo_toml,
            "foo_native",
            &["cdylib"],
            &["/t/foo_native.dll.lib", "/t/foo_native.dll"],
        );
        assert_eq!(
            built_library(cdylib.as_bytes(), &cargo_toml),
            Some((PathBuf::from("/t/foo_native.dll"), "foo_native".to_string()))
        );

        let rlib_only = message(
            &cargo_toml,
            "foo_native",
            &["rlib"],
            &["/t/libfoo_native.rlib"],
        );
        assert_eq!(built_library(rlib_only.as_bytes(), &cargo_toml), None);
    }

    #[test]
    fn rustc_identity_display_names_release_and_host() {
        let id = RustcIdentity {
            release: "1.82.0".to_string(),
            host: "x86_64-unknown-linux-gnu".to_string(),
        };
        assert_eq!(id.to_string(), "rustc 1.82.0 (x86_64-unknown-linux-gnu)");
    }

    #[test]
    fn rustc_identity_parses_verbose_output() {
        let text = "rustc 1.82.0 (f6e511eec 2024-10-15)\n\
                     binary: rustc\n\
                     commit-hash: f6e511eec5f43ba5e5e2b60eb1a35d4f1a35e97a\n\
                     commit-date: 2024-10-15\n\
                     host: x86_64-unknown-linux-gnu\n\
                     release: 1.82.0\n\
                     LLVM version: 19.1.1\n";
        let id = RustcIdentity::parse_verbose(text).expect("parses");
        assert_eq!(id.release, "1.82.0");
        assert_eq!(id.host, "x86_64-unknown-linux-gnu");
    }

    #[test]
    fn rustc_identity_parse_verbose_rejects_output_missing_fields() {
        assert!(RustcIdentity::parse_verbose("binary: rustc\n").is_none());
    }

    #[test]
    fn build_rust_crate_refuses_mismatched_rustc() {
        let dir = tempfile::tempdir().unwrap();
        let native_dir = dir.path().join("native");
        std::fs::create_dir_all(&native_dir).unwrap();
        std::fs::write(
            native_dir.join("Cargo.toml"),
            "[package]\nname = \"p_native\"\nversion = \"0.1.0\"\nedition = \"2021\"\n\
             [lib]\ncrate-type = [\"staticlib\"]\n",
        )
        .unwrap();

        let mismatching = RustcIdentity {
            release: "0.0.0-does-not-exist".to_string(),
            host: "nowhere".to_string(),
        };
        let rust = RustCrate { dir: "native" };
        let error = build_rust_crate(
            dir.path(),
            &rust,
            &mismatching,
            &embedded_rustc_identity().host,
            &hew_compile::SourcePathPolicy::default(),
        )
        .unwrap_err();

        assert!(
            error.contains("E_NATIVE_TOOLCHAIN"),
            "must carry the toolchain-mismatch code: {error}"
        );
        assert!(
            error.contains("0.0.0-does-not-exist"),
            "must name the expected (libhew.a) rustc: {error}"
        );
        assert!(
            !error.contains("cargo build failed"),
            "must fail before ever invoking cargo: {error}"
        );
    }

    #[test]
    fn depfile_prerequisites_keep_escaped_spaces_and_drive_letters() {
        let deps =
            parse_depfile("out/a.o: src/a.c include/my\\ header.h \\\n  C:\\sdk\\x.h cost$$.h\n");
        assert_eq!(
            deps,
            [
                PathBuf::from("src/a.c"),
                PathBuf::from("include/my header.h"),
                PathBuf::from("C:\\sdk\\x.h"),
                PathBuf::from("cost$.h"),
            ]
        );
    }

    #[test]
    fn only_the_hew_namespace_may_define_runtime_symbols() {
        for exempt in ["hew", "hew.db.sqlite", "hew::testffi"] {
            assert!(in_runtime_namespace(exempt), "{exempt}");
        }
        for reserved in ["acme.hew", "hewn.x", "hewlett", "std.net"] {
            assert!(!in_runtime_namespace(reserved), "{reserved}");
        }
    }

    #[test]
    fn suggested_prefix_comes_from_the_package_name() {
        assert_eq!(symbol_prefix("meshcore.roles"), "meshcore_roles_");
        assert_eq!(symbol_prefix("acme-tls"), "acme_tls_");
        assert_eq!(symbol_prefix("..."), "pkg_");
    }

    #[test]
    fn pkg_config_output_splits_on_unescaped_whitespace() {
        assert_eq!(
            split_flags("-I/opt/my\\ sdk/include -DX=1\n"),
            ["-I/opt/my sdk/include", "-DX=1"]
        );
    }
}
