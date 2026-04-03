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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TargetSpec {
    requested_triple: Option<String>,
    normalized_triple: String,
    arch: TargetArch,
    os: TargetOs,
    env: Option<String>,
    object_format: ObjectFormat,
}

impl TargetSpec {
    pub fn from_requested(requested: Option<&str>) -> Result<Self, String> {
        let normalized_triple = match requested {
            Some(triple) => normalize_target_triple(triple)?,
            None => host_triple(),
        };
        let parsed = ParsedTarget::parse(&normalized_triple)?;

        Ok(Self {
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

    pub fn can_run_on_host(&self) -> bool {
        !self.is_wasm() && self.matches_host_environment()
    }

    pub fn can_link_with_host_tools(&self) -> bool {
        self.is_wasm() || self.matches_host_environment()
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

#[cfg(test)]
mod tests {
    use super::TargetSpec;

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
}
