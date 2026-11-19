//! Install advice for the host tools a native link needs.
//!
//! The `hew` binary embeds LLVM, so compiling never needs one installed. The
//! final link does: it runs the system C compiler driver, which supplies the
//! C runtime start files and the C library. When that driver (or, on Windows
//! and for cross targets, clang) is missing, the diagnostic names the one
//! command that installs it on *this* host, not a list for every platform.

/// The package-manager family of the host, for install advice.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum HostFamily {
    /// Debian, Ubuntu and derivatives (`apt`).
    Debian,
    /// Fedora, RHEL, CentOS, Rocky, Alma, Amazon Linux (`dnf`).
    Fedora,
    /// openSUSE and SLES (`zypper`).
    Suse,
    /// Arch Linux and derivatives (`pacman`).
    Arch,
    /// Alpine Linux (`apk`).
    Alpine,
    /// Void Linux (`xbps`).
    Void,
    /// Any other Linux distribution.
    OtherLinux,
    MacOs,
    FreeBsd,
    Windows,
    Other,
}

impl HostFamily {
    /// The family of the machine running `hew`.
    pub(crate) fn host() -> Self {
        if cfg!(target_os = "macos") {
            Self::MacOs
        } else if cfg!(target_os = "freebsd") {
            Self::FreeBsd
        } else if cfg!(target_os = "windows") {
            Self::Windows
        } else if cfg!(target_os = "linux") {
            std::fs::read_to_string("/etc/os-release")
                .or_else(|_| std::fs::read_to_string("/usr/lib/os-release"))
                .map_or(Self::OtherLinux, |text| Self::from_os_release(&text))
        } else {
            Self::Other
        }
    }

    /// Classify a Linux distribution from the text of `os-release(5)`, by its
    /// `ID` first and then each `ID_LIKE` entry, so a derivative (Linux Mint,
    /// Pop!_OS, Rocky) maps to the family whose package manager it uses.
    pub(crate) fn from_os_release(text: &str) -> Self {
        let field = |key: &str| {
            text.lines().find_map(|line| {
                let value = line.strip_prefix(key)?.strip_prefix('=')?;
                Some(value.trim().trim_matches(['"', '\'']).to_ascii_lowercase())
            })
        };
        let id = field("ID").unwrap_or_default();
        let like = field("ID_LIKE").unwrap_or_default();
        std::iter::once(id.as_str())
            .chain(like.split_whitespace())
            .find_map(|name| match name {
                "debian" | "ubuntu" => Some(Self::Debian),
                "fedora" | "rhel" | "centos" | "rocky" | "almalinux" | "amzn" => {
                    Some(Self::Fedora)
                }
                "opensuse" | "opensuse-leap" | "opensuse-tumbleweed" | "suse" | "sles" => {
                    Some(Self::Suse)
                }
                "arch" | "archlinux" | "manjaro" | "endeavouros" => Some(Self::Arch),
                "alpine" => Some(Self::Alpine),
                "void" => Some(Self::Void),
                _ => None,
            })
            .unwrap_or(Self::OtherLinux)
    }

    /// How to install a C compiler driver together with the C library's
    /// development files (start files such as `crt1.o`, and `libc.so`).
    pub(crate) fn c_toolchain_advice(self) -> &'static str {
        match self {
            Self::Debian => "install one with `sudo apt install gcc libc6-dev`",
            Self::Fedora => "install one with `sudo dnf install gcc glibc-devel`",
            Self::Suse => "install one with `sudo zypper install gcc glibc-devel`",
            Self::Arch => "install one with `sudo pacman -S gcc`",
            Self::Alpine => "install one with `sudo apk add build-base`",
            Self::Void => "install one with `sudo xbps-install gcc`",
            Self::MacOs => "install the Xcode Command Line Tools with `xcode-select --install`",
            Self::FreeBsd => {
                "the FreeBSD base system provides `cc`; if this system was installed \
                 without the toolchain, install one with `pkg install llvm`"
            }
            Self::Windows => WINDOWS_TOOLCHAIN_ADVICE,
            Self::OtherLinux | Self::Other => {
                "install `gcc` (or `clang`) and the C library development files with \
                 your system's package manager"
            }
        }
    }

    /// How to install clang, which a cross-target link needs (GCC's `cc`
    /// cannot take a `-target`).
    pub(crate) fn clang_advice(self) -> &'static str {
        match self {
            Self::Debian => "install it with `sudo apt install clang lld`",
            Self::Fedora => "install it with `sudo dnf install clang lld`",
            Self::Suse => "install it with `sudo zypper install clang lld`",
            Self::Arch => "install it with `sudo pacman -S clang lld`",
            Self::Alpine => "install it with `sudo apk add clang lld`",
            Self::Void => "install it with `sudo xbps-install clang lld`",
            Self::MacOs => "install the Xcode Command Line Tools with `xcode-select --install`",
            Self::FreeBsd => "install it with `pkg install llvm`",
            Self::Windows => WINDOWS_TOOLCHAIN_ADVICE,
            Self::OtherLinux | Self::Other => {
                "install `clang` and `lld` with your system's package manager"
            }
        }
    }
}

/// Windows links with LLVM's clang driver and lld-link against the MSVC C
/// runtime and Windows SDK import libraries; lld-link finds those libraries
/// itself, so no developer command prompt is involved.
const WINDOWS_TOOLCHAIN_ADVICE: &str = "install LLVM with `winget install LLVM.LLVM` and put \
     its `bin` directory (usually `C:\\Program Files\\LLVM\\bin`) on PATH, then install \
     Visual Studio Build Tools with the \"Desktop development with C++\" workload and a \
     Windows SDK, which provide the C runtime and system libraries. A developer command \
     prompt (vcvars64.bat) is not needed";

/// The diagnostic for a host with no usable C compiler driver.
pub(crate) fn missing_c_driver(tried: &str, family: HostFamily) -> String {
    format!(
        "Error: {tried}. Hew links native programs with the system C compiler driver; \
         {advice}. To use a specific driver, set HEW_CC to its path.",
        advice = family.c_toolchain_advice(),
    )
}

/// The diagnostic for a cross-target link on a host without clang.
pub(crate) fn missing_cross_clang(triple: &str, family: HostFamily) -> String {
    format!(
        "Error: clang not found. Linking for target {triple} from this host needs clang; \
         {advice}. To use a specific driver, set HEW_CC to its path.",
        advice = family.clang_advice(),
    )
}

/// The diagnostic for a Windows host without `clang.exe`.
pub(crate) fn missing_windows_clang() -> String {
    format!(
        "Error: clang.exe not found on PATH. On Windows, Hew links programs with LLVM's \
         clang and lld-link; {WINDOWS_TOOLCHAIN_ADVICE}. To use a specific driver, set \
         HEW_CC to its path."
    )
}

/// A hint for linker output showing the driver ran but the C library's
/// development files are missing (`crt1.o`, `crti.o`, `-lc`, `-lgcc_s`), which
/// is what a bare `gcc` install without `libc6-dev`/`glibc-devel` produces.
pub(crate) fn missing_c_library_hint(stderr: &str, family: HostFamily) -> Option<String> {
    let missing_start_files = ["crt1.o", "Scrt1.o", "crti.o", "crtbeginS.o", "crtbegin.o"];
    let missing_libs = ["-lc", "-lgcc_s", "-lgcc", "-lm", "-lpthread", "-ldl", "-lrt"];
    let mentions_missing = stderr.lines().any(|line| {
        let lower = line.to_ascii_lowercase();
        let not_found = lower.contains("cannot find")
            || lower.contains("unable to find library")
            || lower.contains("library not found for");
        not_found
            && (missing_start_files.iter().any(|file| line.contains(file))
                || missing_libs.iter().any(|lib| {
                    line.split(|c: char| c.is_whitespace() || c == ':' || c == '\'')
                        .any(|word| word == *lib)
                }))
    });
    mentions_missing.then(|| {
        format!(
            "hint: the C compiler driver ran, but the C library's development files are \
             missing; {advice}.",
            advice = family.c_toolchain_advice()
        )
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn os_release_maps_ids_and_derivatives_to_their_package_manager() {
        let cases = [
            ("ID=ubuntu\nID_LIKE=debian\n", HostFamily::Debian),
            ("ID=linuxmint\nID_LIKE=\"ubuntu debian\"\n", HostFamily::Debian),
            ("ID=fedora\n", HostFamily::Fedora),
            ("ID=\"rocky\"\nID_LIKE=\"rhel centos fedora\"\n", HostFamily::Fedora),
            ("ID=\"opensuse-tumbleweed\"\nID_LIKE=\"opensuse suse\"\n", HostFamily::Suse),
            ("ID=arch\n", HostFamily::Arch),
            ("ID=endeavouros\nID_LIKE=arch\n", HostFamily::Arch),
            ("ID=alpine\n", HostFamily::Alpine),
            ("ID=void\n", HostFamily::Void),
            ("ID=nixos\n", HostFamily::OtherLinux),
            ("", HostFamily::OtherLinux),
        ];
        for (text, expected) in cases {
            assert_eq!(HostFamily::from_os_release(text), expected, "{text:?}");
        }
    }

    #[test]
    fn os_release_id_wins_over_id_like() {
        // A distribution that names itself is classified by its own ID even
        // when it lists another family first in ID_LIKE.
        let text = "ID_LIKE=\"rhel fedora\"\nID=ubuntu\n";
        assert_eq!(HostFamily::from_os_release(text), HostFamily::Debian);
    }

    #[test]
    fn missing_driver_names_one_command_for_this_host() {
        let debian = missing_c_driver("no C compiler found (tried clang, cc)", HostFamily::Debian);
        assert!(debian.contains("tried clang, cc"), "{debian}");
        assert!(debian.contains("sudo apt install gcc libc6-dev"), "{debian}");
        assert!(!debian.contains("dnf"), "only this host's command: {debian}");
        assert!(!debian.contains("xcode-select"), "only this host's command: {debian}");
        assert!(debian.contains("HEW_CC"), "{debian}");

        let mac = missing_c_driver("no C compiler found (tried clang, cc)", HostFamily::MacOs);
        assert!(mac.contains("xcode-select --install"), "{mac}");
        assert!(!mac.contains("apt"), "{mac}");

        let freebsd = missing_c_driver("no C compiler found (tried clang, cc)", HostFamily::FreeBsd);
        assert!(freebsd.contains("base system provides `cc`"), "{freebsd}");
    }

    #[test]
    fn cross_link_advice_installs_clang() {
        let error = missing_cross_clang("aarch64-unknown-linux-gnu", HostFamily::Fedora);
        assert!(error.contains("clang not found"), "{error}");
        assert!(error.contains("aarch64-unknown-linux-gnu"), "{error}");
        assert!(error.contains("sudo dnf install clang lld"), "{error}");
    }

    #[test]
    fn windows_advice_names_tools_without_pinning_an_llvm_release() {
        let error = missing_windows_clang();
        assert!(error.contains("clang.exe"), "{error}");
        assert!(error.contains("lld-link"), "{error}");
        assert!(error.contains("winget install LLVM.LLVM"), "{error}");
        assert!(error.contains("Build Tools"), "{error}");
        assert!(error.contains("Windows SDK"), "{error}");
        assert!(error.contains("PATH"), "{error}");
        assert!(
            !error.contains("22.1") && !error.contains("github.com"),
            "no hard-pinned download: {error}"
        );
    }

    #[test]
    fn missing_c_library_hint_detects_absent_start_files_and_libc() {
        let gnu_ld = "/usr/bin/ld: cannot find Scrt1.o: No such file or directory\n\
                      /usr/bin/ld: cannot find crti.o: No such file or directory";
        let hint = missing_c_library_hint(gnu_ld, HostFamily::Debian).expect("hint");
        assert!(hint.contains("libc6-dev"), "{hint}");

        let lld = "ld.lld: error: unable to find library -lgcc_s";
        assert!(missing_c_library_hint(lld, HostFamily::Fedora).is_some());

        let gnu_lc = "/usr/bin/ld: cannot find -lc: No such file or directory";
        assert!(missing_c_library_hint(gnu_lc, HostFamily::Debian).is_some());
    }

    #[test]
    fn missing_c_library_hint_ignores_unrelated_link_errors() {
        let unrelated = "ld.lld: error: undefined symbol: hew_json_parse\n\
                         /usr/bin/ld: cannot find -lMagickWand-7.Q16";
        assert_eq!(missing_c_library_hint(unrelated, HostFamily::Debian), None);
        // `-lcrypto` merely begins with `-lc`.
        let prefix = "/usr/bin/ld: cannot find -lcrypto: No such file or directory";
        assert_eq!(missing_c_library_hint(prefix, HostFamily::Debian), None);
    }
}
