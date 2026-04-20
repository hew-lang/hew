//! Target-triple parsing and target-driven CLI behavior.

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TargetArch {
    Aarch64,
    X86_64,
    Wasm32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TargetOs {
    Darwin,
    Linux,
    Windows,
    Wasi,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ObjectFormat {
    Coff,
    Elf,
    MachO,
    Wasm,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExecutionTargetKind {
    Native,
    Wasi,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TargetSpec {
    #[cfg(hew_embedded_codegen)]
    requested_triple: Option<String>,
    normalized_triple: String,
    arch: TargetArch,
    os: TargetOs,
    env: Option<String>,
    object_format: ObjectFormat,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExecutionTarget {
    spec: TargetSpec,
    kind: ExecutionTargetKind,
}

/// Platform-specific components of a native link plan, driven entirely by the
/// *target* [`TargetOs`], not by the compile-time host `#[cfg]` world.
///
/// Separating target intent from host availability makes the Darwin/Linux/
/// Windows contracts explicit, independently unit-testable, and correct even
/// when the link plan is inspected in contexts that differ from the running
/// host (e.g. dry-run, documentation, or future cross-OS paths).
///
/// Host-side concerns (which linker binary to invoke, whether lld is installed,
/// whether `dsymutil` is available) are intentionally *not* captured here —
/// those remain runtime checks in `link.rs`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NativeLinkPlan {
    /// Dead-code-elimination flags passed to the linker unconditionally.
    ///
    /// Darwin: `["-Wl,-dead_strip"]`; Linux/ELF: `["-Wl,--gc-sections"]`;
    /// Windows: `[]` (lld-link uses `/OPT:REF` by default).
    pub gc_flags: &'static [&'static str],

    /// Strip flags applied only for release (non-debug) builds.
    ///
    /// Darwin: `["-Wl,-x"]`; Linux/ELF: `["-Wl,--strip-all"]`; Windows: `[]`.
    pub strip_flags: &'static [&'static str],

    /// System libraries required by the Hew runtime on this target.
    pub platform_libs: &'static [&'static str],

    /// `true` when the Darwin SDK path should be anchored via `-isysroot`.
    ///
    /// The path is resolved at link time via `xcrun --show-sdk-path`; failure
    /// is non-fatal (clang falls back to its own SDK search heuristics).
    pub needs_darwin_sdk: bool,

    /// `true` when the Windows DLL CRT override (`/NODEFAULTLIB:libcmt
    /// /DEFAULTLIB:msvcrt`) is required to match the Rust runtime's CRT.
    pub needs_windows_crt_fixup: bool,

    /// `true` when a `dsymutil` pass should follow a successful debug link.
    ///
    /// Set for Darwin targets; whether the tool is actually present on the
    /// host is checked at link time — the step is skipped if unavailable.
    pub needs_dsymutil: bool,
}

impl TargetSpec {
    pub fn from_requested(requested: Option<&str>) -> Result<Self, String> {
        let normalized_triple = match requested {
            Some(triple) => normalize_target_triple(triple)?,
            None => host_triple(),
        };
        let parsed = ParsedTarget::parse(&normalized_triple)?;

        Ok(Self {
            #[cfg(hew_embedded_codegen)]
            requested_triple: requested.map(str::to_owned),
            normalized_triple,
            arch: parsed.arch,
            os: parsed.os,
            env: parsed.env,
            object_format: parsed.object_format,
        })
    }

    pub fn normalized_triple(&self) -> &str {
        &self.normalized_triple
    }

    #[cfg(hew_embedded_codegen)]
    pub fn codegen_triple(&self) -> Option<&str> {
        self.requested_triple
            .as_ref()
            .map(|_| self.normalized_triple())
    }

    pub fn executable_suffix(&self) -> &'static str {
        match self.os {
            TargetOs::Windows => ".exe",
            TargetOs::Wasi => ".wasm",
            TargetOs::Darwin | TargetOs::Linux => "",
        }
    }

    pub fn object_suffix(&self) -> &'static str {
        match self.object_format {
            ObjectFormat::Coff => ".obj",
            ObjectFormat::Elf | ObjectFormat::MachO | ObjectFormat::Wasm => ".o",
        }
    }

    pub fn hew_lib_name(&self) -> &'static str {
        match self.object_format {
            ObjectFormat::Coff => "hew.lib",
            ObjectFormat::Elf | ObjectFormat::MachO | ObjectFormat::Wasm => "libhew.a",
        }
    }

    pub fn is_wasm(&self) -> bool {
        self.os == TargetOs::Wasi
    }

    /// Returns the target-driven native link plan for this target.
    ///
    /// Every field is derived from `self.os` — not from the host `#[cfg]`
    /// world — so the plan is deterministic and unit-testable regardless of
    /// which machine compiled hew.  See [`NativeLinkPlan`] for field
    /// semantics.
    pub fn native_link_plan(&self) -> NativeLinkPlan {
        match self.os {
            TargetOs::Darwin => NativeLinkPlan {
                gc_flags: &["-Wl,-dead_strip"],
                strip_flags: &["-Wl,-x"],
                // CoreFoundation and Security are pulled in by the Hew runtime.
                // `-lpthread` and `-lm` satisfy runtime math and threading
                // references; clang implicitly links libc++ for C++ TLS support.
                platform_libs: &[
                    "-lpthread",
                    "-lm",
                    "-framework",
                    "CoreFoundation",
                    "-framework",
                    "Security",
                ],
                needs_darwin_sdk: true,
                needs_windows_crt_fixup: false,
                needs_dsymutil: true,
            },
            TargetOs::Linux => NativeLinkPlan {
                gc_flags: &["-Wl,--gc-sections"],
                strip_flags: &["-Wl,--strip-all"],
                platform_libs: &["-lpthread", "-lm", "-ldl", "-lrt"],
                needs_darwin_sdk: false,
                needs_windows_crt_fixup: false,
                needs_dsymutil: false,
            },
            TargetOs::Windows => NativeLinkPlan {
                // The Windows linker (lld-link) does not accept ELF-style -Wl
                // gc flags; dead code is trimmed by the linker by default via
                // /OPT:REF.
                gc_flags: &[],
                strip_flags: &[],
                platform_libs: &[
                    // The Rust runtime references `printf` via __declspec(dllimport),
                    // but the UCRT inlines printf; the legacy definitions library
                    // provides the classic __imp_printf symbol.
                    "-llegacy_stdio_definitions",
                    "-lws2_32",
                    "-luserenv",
                    "-lbcrypt",
                    "-lntdll",
                    "-ladvapi32",
                ],
                needs_darwin_sdk: false,
                // Clang defaults to the static CRT (libcmt) but the Rust-compiled
                // runtime uses the DLL CRT (msvcrt).  The CRT linkage must match.
                needs_windows_crt_fixup: true,
                needs_dsymutil: false,
            },
            TargetOs::Wasi => NativeLinkPlan {
                gc_flags: &[],
                strip_flags: &[],
                platform_libs: &[],
                needs_darwin_sdk: false,
                needs_windows_crt_fixup: false,
                needs_dsymutil: false,
            },
        }
    }

    /// Returns the clang-compatible target triple for the linker `-target` flag.
    ///
    /// On Darwin, clang expects a deployment-target form like
    /// `aarch64-apple-macosx13.0` rather than the bare vendor triple
    /// `aarch64-apple-darwin`.  The deployment target is read from the
    /// `MACOSX_DEPLOYMENT_TARGET` environment variable and defaults to `"13.0"`
    /// (macOS 13 / Ventura — the project's stated minimum; see Makefile and
    /// PR #771).
    ///
    /// All other platforms use the normalized triple directly.
    pub fn linker_triple(&self) -> String {
        if self.os == TargetOs::Darwin {
            let arch = match self.arch {
                TargetArch::Aarch64 => "aarch64",
                TargetArch::X86_64 => "x86_64",
                TargetArch::Wasm32 => "wasm32",
            };
            let deployment = std::env::var("MACOSX_DEPLOYMENT_TARGET")
                .ok()
                .filter(|v| !v.is_empty())
                .unwrap_or_else(|| "13.0".to_string());
            format!("{arch}-apple-macosx{deployment}")
        } else {
            self.normalized_triple.clone()
        }
    }

    pub fn can_run_on_host(&self) -> bool {
        !self.is_wasm() && self.matches_host_environment()
    }

    /// Returns the target architecture.
    ///
    /// Used by Linux cross-arch sysroot probing in `link.rs`; only called from
    /// `#[cfg(target_os = "linux")]` code so the warning is suppressed on other
    /// hosts.
    #[allow(
        dead_code,
        reason = "only called from #[cfg(target_os=\"linux\")] code in link.rs; unused on non-Linux hosts"
    )]
    pub fn arch(&self) -> TargetArch {
        self.arch
    }

    /// Returns the target OS.
    ///
    /// Used by Linux cross-arch sysroot probing in `link.rs`; only called from
    /// `#[cfg(target_os = "linux")]` code so the warning is suppressed on other
    /// hosts.
    #[allow(
        dead_code,
        reason = "only called from #[cfg(target_os=\"linux\")] code in link.rs; unused on non-Linux hosts"
    )]
    pub fn os(&self) -> TargetOs {
        self.os
    }

    /// Returns the object file format for this target.
    ///
    /// Used by linker-side helpers that need to distinguish ELF, Mach-O, COFF,
    /// and Wasm targets without pattern-matching the full OS.  Currently
    /// exercised by tests; linker callers in this crate use `native_link_plan`.
    #[allow(
        dead_code,
        reason = "exercised by tests and cfg-gated linker helpers; not all callers are visible to the linter on every host"
    )]
    pub fn object_format(&self) -> ObjectFormat {
        self.object_format
    }

    pub fn can_link_with_host_tools(&self) -> bool {
        self.is_wasm()
            || self.matches_host_environment()
            || self.can_cross_link_on_darwin_host()
            || self.can_cross_link_on_linux_host()
    }

    pub fn cross_target_run_error(&self, verb: &str) -> String {
        format!(
            "Error: cannot {verb} target {} on this host. Cross-target executable {verb} is not \
             supported yet; use `hew build --target {} --emit-obj` instead.",
            self.normalized_triple(),
            self.normalized_triple(),
        )
    }

    pub fn unsupported_native_link_error(&self) -> String {
        format!(
            "Error: target {} can emit objects, but native executable linking is only supported \
             for the host target right now. Use `hew build --target {} --emit-obj` for this \
             prototype lane.",
            self.normalized_triple(),
            self.normalized_triple(),
        )
    }

    fn matches_host_environment(&self) -> bool {
        let host = host_platform();
        self.arch == host.arch && self.os == host.os && self.env == host.env
    }

    fn can_cross_link_on_darwin_host(&self) -> bool {
        let host = host_platform();
        host.os == TargetOs::Darwin
            && self.os == TargetOs::Darwin
            && self.env == host.env
            && self.arch != host.arch
    }

    /// Linux same-OS cross-arch linking is supported by clang/LLD with a
    /// Debian/Ubuntu multiarch sysroot.  Both arches must share the same
    /// runtime ABI environment (e.g. both `gnu` or both `musl`) so that the
    /// pre-built `libhew.a` and runtime ABI contract are compatible.
    fn can_cross_link_on_linux_host(&self) -> bool {
        let host = host_platform();
        host.os == TargetOs::Linux
            && self.os == TargetOs::Linux
            && self.env == host.env
            && self.arch != host.arch
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ParsedTarget {
    arch: TargetArch,
    os: TargetOs,
    env: Option<String>,
    object_format: ObjectFormat,
}

impl ParsedTarget {
    fn parse(triple: &str) -> Result<Self, String> {
        let parts: Vec<_> = triple.split('-').collect();
        let Some(arch_name) = parts.first().copied() else {
            return Err(format!("Error: unsupported target triple: {triple}"));
        };
        let arch = match arch_name {
            "aarch64" => TargetArch::Aarch64,
            "x86_64" => TargetArch::X86_64,
            "wasm32" => TargetArch::Wasm32,
            _ => return Err(format!("Error: unsupported target triple: {triple}")),
        };

        if parts.iter().any(|part| matches!(*part, "wasi" | "wasip1")) {
            return Ok(Self {
                arch,
                os: TargetOs::Wasi,
                env: Some("wasip1".to_string()),
                object_format: ObjectFormat::Wasm,
            });
        }

        if parts.len() < 3 {
            return Err(format!("Error: unsupported target triple: {triple}"));
        }

        if parts.contains(&"darwin") {
            return Ok(Self {
                arch,
                os: TargetOs::Darwin,
                env: None,
                object_format: ObjectFormat::MachO,
            });
        }

        if parts.contains(&"linux") {
            return Ok(Self {
                arch,
                os: TargetOs::Linux,
                env: parse_env(parts.as_slice(), &["gnu", "musl"]),
                object_format: ObjectFormat::Elf,
            });
        }

        if parts.contains(&"windows") {
            return Ok(Self {
                arch,
                os: TargetOs::Windows,
                env: parse_env(parts.as_slice(), &["gnu", "msvc"]),
                object_format: ObjectFormat::Coff,
            });
        }

        Err(format!("Error: unsupported target triple: {triple}"))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct HostPlatform {
    arch: TargetArch,
    os: TargetOs,
    env: Option<String>,
}

fn normalize_target_triple(triple: &str) -> Result<String, String> {
    let normalized = triple.trim().to_ascii_lowercase();
    if normalized.is_empty() {
        return Err("Error: target triple cannot be empty".to_string());
    }

    let normalized = if let Some(rest) = normalized.strip_prefix("arm64-") {
        format!("aarch64-{rest}")
    } else {
        normalized
    };

    if normalized == "wasm32-wasi" {
        Ok("wasm32-wasip1".to_string())
    } else {
        Ok(normalized)
    }
}

fn parse_env(parts: &[&str], known: &[&str]) -> Option<String> {
    parts
        .iter()
        .find(|part| known.contains(part))
        .map(|part| (*part).to_string())
}

fn host_triple() -> String {
    let platform = host_platform();
    match platform.os {
        TargetOs::Darwin => format!("{}-apple-darwin", host_arch_name(platform.arch)),
        TargetOs::Linux => format!(
            "{}-unknown-linux-{}",
            host_arch_name(platform.arch),
            platform.env.as_deref().unwrap_or("gnu")
        ),
        TargetOs::Windows => format!(
            "{}-pc-windows-{}",
            host_arch_name(platform.arch),
            platform.env.as_deref().unwrap_or("msvc")
        ),
        TargetOs::Wasi => "wasm32-wasip1".to_string(),
    }
}

fn host_platform() -> HostPlatform {
    HostPlatform {
        arch: host_arch(),
        os: host_os(),
        env: host_env().map(str::to_owned),
    }
}

fn host_arch_name(arch: TargetArch) -> &'static str {
    match arch {
        TargetArch::Aarch64 => "aarch64",
        TargetArch::X86_64 => "x86_64",
        TargetArch::Wasm32 => "wasm32",
    }
}

fn host_arch() -> TargetArch {
    if cfg!(target_arch = "aarch64") {
        TargetArch::Aarch64
    } else if cfg!(target_arch = "x86_64") {
        TargetArch::X86_64
    } else if cfg!(target_arch = "wasm32") {
        TargetArch::Wasm32
    } else {
        panic!("unsupported host architecture for hew target modeling");
    }
}

fn host_os() -> TargetOs {
    if cfg!(target_os = "macos") {
        TargetOs::Darwin
    } else if cfg!(target_os = "linux") {
        TargetOs::Linux
    } else if cfg!(target_os = "windows") {
        TargetOs::Windows
    } else if cfg!(target_os = "wasi") {
        TargetOs::Wasi
    } else {
        panic!("unsupported host OS for hew target modeling");
    }
}

#[cfg(target_env = "gnu")]
const HOST_ENV: Option<&str> = Some("gnu");

#[cfg(target_env = "msvc")]
const HOST_ENV: Option<&str> = Some("msvc");

#[cfg(target_env = "musl")]
const HOST_ENV: Option<&str> = Some("musl");

#[cfg(not(any(target_env = "gnu", target_env = "msvc", target_env = "musl")))]
const HOST_ENV: Option<&str> = None;

fn host_env() -> Option<&'static str> {
    HOST_ENV
}

impl ExecutionTarget {
    pub fn from_requested(requested: Option<&str>) -> Result<Self, String> {
        let spec = TargetSpec::from_requested(requested)?;
        let kind = if spec.is_wasm() {
            ExecutionTargetKind::Wasi
        } else {
            ExecutionTargetKind::Native
        };
        Ok(Self { spec, kind })
    }

    pub fn normalized_triple(&self) -> &str {
        self.spec.normalized_triple()
    }

    pub fn executable_suffix(&self) -> &'static str {
        self.spec.executable_suffix()
    }

    pub fn is_native(&self) -> bool {
        self.kind == ExecutionTargetKind::Native
    }

    pub fn is_wasi(&self) -> bool {
        self.kind == ExecutionTargetKind::Wasi
    }

    pub fn can_run_on_host(&self) -> bool {
        self.spec.can_run_on_host()
    }

    pub fn cross_target_run_error(&self, verb: &str) -> String {
        self.spec.cross_target_run_error(verb)
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Mutex;

    use super::{ExecutionTarget, TargetSpec};

    // Serialize env-var–mutating tests: `std::env::set_var` / `remove_var` are
    // not thread-safe when multiple tests share the same process.
    static ENV_LOCK: Mutex<()> = Mutex::new(());

    #[test]
    fn normalizes_arm64_apple_darwin() {
        let spec = TargetSpec::from_requested(Some("arm64-apple-darwin")).expect("target");
        assert_eq!(spec.normalized_triple(), "aarch64-apple-darwin");
    }

    #[test]
    fn windows_gnu_uses_windows_suffixes() {
        // The prototype lane uses the GNU Windows ABI for cross-target object
        // emission coverage before widening to additional Windows link modes.
        let spec = TargetSpec::from_requested(Some("x86_64-pc-windows-gnu")).expect("target");
        assert_eq!(spec.executable_suffix(), ".exe");
        assert_eq!(spec.object_suffix(), ".obj");
    }

    #[test]
    fn wasm32_wasi_normalizes_to_wasip1() {
        let spec = TargetSpec::from_requested(Some("wasm32-wasi")).expect("target");
        assert_eq!(spec.normalized_triple(), "wasm32-wasip1");
        assert_eq!(spec.executable_suffix(), ".wasm");
    }

    #[test]
    fn execution_target_classifies_native_targets() {
        let target =
            ExecutionTarget::from_requested(Some("x86_64-unknown-linux-gnu")).expect("target");
        assert!(target.is_native());
        assert!(!target.is_wasi());
    }

    #[test]
    fn execution_target_classifies_wasi_targets() {
        let target = ExecutionTarget::from_requested(Some("wasm32-wasi")).expect("target");
        assert!(target.is_wasi());
        assert!(!target.is_native());
    }

    // ── linker_triple ──────────────────────────────────────────────────

    #[test]
    fn darwin_linker_triple_uses_deployment_target_env() {
        let _guard = ENV_LOCK.lock().unwrap();
        let spec = TargetSpec::from_requested(Some("arm64-apple-darwin")).expect("target");
        std::env::set_var("MACOSX_DEPLOYMENT_TARGET", "14.0");
        let triple = spec.linker_triple();
        std::env::remove_var("MACOSX_DEPLOYMENT_TARGET");
        assert_eq!(triple, "aarch64-apple-macosx14.0");
    }

    #[test]
    fn darwin_linker_triple_defaults_to_13_0_when_env_absent() {
        let _guard = ENV_LOCK.lock().unwrap();
        let spec = TargetSpec::from_requested(Some("aarch64-apple-darwin")).expect("target");
        std::env::remove_var("MACOSX_DEPLOYMENT_TARGET");
        let triple = spec.linker_triple();
        assert_eq!(triple, "aarch64-apple-macosx13.0");
    }

    #[test]
    fn darwin_linker_triple_defaults_to_13_0_when_env_empty() {
        // An explicitly-empty MACOSX_DEPLOYMENT_TARGET (e.g. `export
        // MACOSX_DEPLOYMENT_TARGET=`) must not produce a versionless triple
        // like `aarch64-apple-macosx` that clang rejects.
        let _guard = ENV_LOCK.lock().unwrap();
        let spec = TargetSpec::from_requested(Some("arm64-apple-darwin")).expect("target");
        std::env::set_var("MACOSX_DEPLOYMENT_TARGET", "");
        let triple = spec.linker_triple();
        std::env::remove_var("MACOSX_DEPLOYMENT_TARGET");
        assert_eq!(triple, "aarch64-apple-macosx13.0");
    }

    #[test]
    fn linux_linker_triple_passes_normalized_triple_unchanged() {
        let spec = TargetSpec::from_requested(Some("x86_64-unknown-linux-gnu")).expect("target");
        assert_eq!(spec.linker_triple(), "x86_64-unknown-linux-gnu");
    }

    #[cfg(target_os = "macos")]
    #[test]
    fn darwin_cross_arch_can_link_with_host_tools() {
        let triple = if cfg!(target_arch = "aarch64") {
            "x86_64-apple-darwin"
        } else {
            "aarch64-apple-darwin"
        };
        let spec = TargetSpec::from_requested(Some(triple)).expect("target");
        assert!(spec.can_link_with_host_tools());
        assert!(!spec.can_run_on_host());
    }

    #[cfg(target_os = "macos")]
    #[test]
    fn non_darwin_foreign_targets_still_cannot_link_with_host_tools() {
        let spec = TargetSpec::from_requested(Some("x86_64-unknown-linux-gnu")).expect("target");
        assert!(!spec.can_link_with_host_tools());
    }

    #[cfg(all(target_os = "linux", target_arch = "aarch64"))]
    #[test]
    fn linux_cross_arch_x86_64_can_link_with_host_tools_on_aarch64() {
        // Mirror the host env so the same-OS env-match gate accepts the target.
        let triple = if cfg!(target_env = "musl") {
            "x86_64-unknown-linux-musl"
        } else {
            "x86_64-unknown-linux-gnu"
        };
        let spec = TargetSpec::from_requested(Some(triple)).expect("target");
        assert!(
            spec.can_link_with_host_tools(),
            "{triple} must be linkable from aarch64 Linux host"
        );
        assert!(
            !spec.can_run_on_host(),
            "cross-arch binary must not be runnable on host"
        );
    }

    #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
    #[test]
    fn linux_cross_arch_aarch64_can_link_with_host_tools_on_x86_64() {
        // Mirror the host env so the same-OS env-match gate accepts the target.
        let triple = if cfg!(target_env = "musl") {
            "aarch64-unknown-linux-musl"
        } else {
            "aarch64-unknown-linux-gnu"
        };
        let spec = TargetSpec::from_requested(Some(triple)).expect("target");
        assert!(
            spec.can_link_with_host_tools(),
            "{triple} must be linkable from x86_64 Linux host"
        );
        assert!(
            !spec.can_run_on_host(),
            "cross-arch binary must not be runnable on host"
        );
    }

    /// Linux cross-arch with a mismatched ABI env (gnu vs musl) must be
    /// rejected: the pre-built `libhew.a` and runtime ABI contract are
    /// env-specific.
    #[cfg(target_os = "linux")]
    #[test]
    fn linux_cross_arch_env_mismatch_cannot_link() {
        // Construct a cross-arch target with a different env than the host.
        // On a gnu host we target musl; on a musl host we target gnu.
        let cross_env_triple = if cfg!(target_arch = "x86_64") {
            if cfg!(target_env = "musl") {
                "aarch64-unknown-linux-gnu"
            } else {
                "aarch64-unknown-linux-musl"
            }
        } else {
            // aarch64 host
            if cfg!(target_env = "musl") {
                "x86_64-unknown-linux-gnu"
            } else {
                "x86_64-unknown-linux-musl"
            }
        };
        let spec = TargetSpec::from_requested(Some(cross_env_triple)).expect("target");
        assert!(
            !spec.can_link_with_host_tools(),
            "cross-arch cross-env target {cross_env_triple} must not be linkable (ABI mismatch)"
        );
    }

    /// A completely foreign OS target (Darwin on a Linux host, or vice-versa)
    /// must never pass the link gate regardless of arch.
    #[cfg(target_os = "linux")]
    #[test]
    fn darwin_target_cannot_link_from_linux_host() {
        for triple in ["aarch64-apple-darwin", "x86_64-apple-darwin"] {
            let spec = TargetSpec::from_requested(Some(triple)).expect("target");
            assert!(
                !spec.can_link_with_host_tools(),
                "{triple} must not be linkable from a Linux host"
            );
        }
    }

    // ── native_link_plan ───────────────────────────────────────────────
    //
    // All assertions below are target-driven: they hold on *any* host.

    #[test]
    fn darwin_link_plan_gc_flags() {
        let spec = TargetSpec::from_requested(Some("aarch64-apple-darwin")).expect("target");
        let plan = spec.native_link_plan();
        assert_eq!(plan.gc_flags, &["-Wl,-dead_strip"]);
        assert_eq!(plan.strip_flags, &["-Wl,-x"]);
    }

    #[test]
    fn darwin_link_plan_platform_libs_include_frameworks() {
        let spec = TargetSpec::from_requested(Some("aarch64-apple-darwin")).expect("target");
        let plan = spec.native_link_plan();
        let libs: Vec<_> = plan.platform_libs.to_vec();
        assert!(libs.contains(&"-lpthread"), "expected -lpthread");
        assert!(libs.contains(&"-lm"), "expected -lm");
        assert!(libs.contains(&"CoreFoundation"), "expected CoreFoundation");
        assert!(libs.contains(&"Security"), "expected Security");
        // -framework flags must immediately precede their framework names
        let cf_pos = libs.iter().position(|&l| l == "CoreFoundation").unwrap();
        assert_eq!(libs[cf_pos - 1], "-framework");
        let sec_pos = libs.iter().position(|&l| l == "Security").unwrap();
        assert_eq!(libs[sec_pos - 1], "-framework");
    }

    #[test]
    fn darwin_link_plan_flags() {
        let spec = TargetSpec::from_requested(Some("x86_64-apple-darwin")).expect("target");
        let plan = spec.native_link_plan();
        assert!(plan.needs_darwin_sdk, "Darwin must request SDK anchoring");
        assert!(plan.needs_dsymutil, "Darwin must request dsymutil pass");
        assert!(!plan.needs_windows_crt_fixup);
    }

    #[test]
    fn linux_link_plan_gc_flags() {
        let spec = TargetSpec::from_requested(Some("x86_64-unknown-linux-gnu")).expect("target");
        let plan = spec.native_link_plan();
        assert_eq!(plan.gc_flags, &["-Wl,--gc-sections"]);
        assert_eq!(plan.strip_flags, &["-Wl,--strip-all"]);
    }

    #[test]
    fn linux_link_plan_platform_libs() {
        let spec = TargetSpec::from_requested(Some("aarch64-unknown-linux-gnu")).expect("target");
        let plan = spec.native_link_plan();
        let libs: Vec<_> = plan.platform_libs.to_vec();
        assert!(libs.contains(&"-lpthread"));
        assert!(libs.contains(&"-lm"));
        assert!(libs.contains(&"-ldl"));
        assert!(libs.contains(&"-lrt"));
    }

    #[test]
    fn linux_link_plan_flags() {
        let spec = TargetSpec::from_requested(Some("x86_64-unknown-linux-gnu")).expect("target");
        let plan = spec.native_link_plan();
        assert!(!plan.needs_darwin_sdk);
        assert!(!plan.needs_dsymutil);
        assert!(!plan.needs_windows_crt_fixup);
    }

    #[test]
    fn windows_link_plan_gc_flags_are_empty() {
        let spec = TargetSpec::from_requested(Some("x86_64-pc-windows-gnu")).expect("target");
        let plan = spec.native_link_plan();
        assert!(
            plan.gc_flags.is_empty(),
            "Windows lld-link has no ELF-style gc flags"
        );
        assert!(plan.strip_flags.is_empty());
    }

    #[test]
    fn windows_link_plan_platform_libs() {
        let spec = TargetSpec::from_requested(Some("x86_64-pc-windows-gnu")).expect("target");
        let plan = spec.native_link_plan();
        let libs: Vec<_> = plan.platform_libs.to_vec();
        assert!(libs.contains(&"-lws2_32"));
        assert!(libs.contains(&"-luserenv"));
        assert!(libs.contains(&"-lbcrypt"));
        assert!(libs.contains(&"-lntdll"));
        assert!(libs.contains(&"-ladvapi32"));
    }

    #[test]
    fn windows_link_plan_flags() {
        let spec = TargetSpec::from_requested(Some("x86_64-pc-windows-gnu")).expect("target");
        let plan = spec.native_link_plan();
        assert!(plan.needs_windows_crt_fixup, "Windows needs DLL CRT fixup");
        assert!(!plan.needs_darwin_sdk);
        assert!(!plan.needs_dsymutil);
    }

    #[test]
    fn wasm_link_plan_is_empty() {
        let spec = TargetSpec::from_requested(Some("wasm32-wasi")).expect("target");
        let plan = spec.native_link_plan();
        assert!(plan.gc_flags.is_empty());
        assert!(plan.strip_flags.is_empty());
        assert!(plan.platform_libs.is_empty());
        assert!(!plan.needs_darwin_sdk);
        assert!(!plan.needs_dsymutil);
        assert!(!plan.needs_windows_crt_fixup);
    }
}
