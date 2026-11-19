//! Name the shared runtime after the exact Hew release.
//!
//! The runtime ABI is not stable across releases, so a program linked against
//! the shared runtime records the release in the name it loads: the ELF
//! `SONAME` `libhew.so.<version>` or the Mach-O install name
//! `@rpath/libhew.<version>.dylib`. Releases install side by side, and a
//! program never loads a runtime it was not built against.
//!
//! Cargo writes the library as plain `libhew.so` / `libhew.dylib`, so the
//! script also links the versioned name to it beside Cargo's output. A program
//! or test linked against the in-tree runtime then resolves the name it records
//! exactly as it does against an installed one.

use std::path::Path;

fn main() {
    let version = std::env::var("CARGO_PKG_VERSION").expect("Cargo supplies the package version");
    let target_os = std::env::var("CARGO_CFG_TARGET_OS").expect("Cargo supplies the target OS");
    let out = std::path::PathBuf::from(std::env::var_os("OUT_DIR").unwrap());
    // Cargo's OUT_DIR is <profile>/build/<package>/out.
    let profile = out.ancestors().nth(3).expect("Cargo profile directory");
    match target_os.as_str() {
        "linux" | "freebsd" => {
            let soname = format!("libhew.so.{version}");
            println!("cargo:rustc-link-arg-cdylib=-Wl,-soname,{soname}");
            link_versioned_name(profile, "libhew.so", &soname);
        }
        "macos" => {
            let name = format!("libhew.{version}.dylib");
            println!("cargo:rustc-link-arg-cdylib=-Wl,-install_name,@rpath/{name}");
            // Mach-O records numeric versions only; a pre-release keeps its
            // release number (0.12.0-rc1 -> 0.12.0). Each release is its own
            // install name, so the compatibility version is the release itself.
            let numeric = version.split(['-', '+']).next().unwrap_or(&version);
            println!("cargo:rustc-link-arg-cdylib=-Wl,-current_version,{numeric}");
            println!("cargo:rustc-link-arg-cdylib=-Wl,-compatibility_version,{numeric}");
            link_versioned_name(profile, "libhew.dylib", &name);
        }
        _ => {}
    }
    if std::env::var("TARGET").unwrap().ends_with("-windows-msvc") {
        // Keep the runtime DLL's PDB beside that image with its own basename;
        // the driver owns the ordinary hew.pdb. Two concurrent links must not
        // share a PDB.
        println!(
            "cargo:rustc-link-arg-cdylib=/PDB:{}",
            profile.join("hew-runtime.pdb").display()
        );
    }
}

/// Point `<profile>/<versioned>` at Cargo's `<profile>/<cargo_name>`, which
/// the link after this script produces. A relative link survives moving the
/// target directory.
#[cfg_attr(
    not(unix),
    allow(
        unused_variables,
        reason = "only Unix hosts link ELF and Mach-O runtimes"
    )
)]
fn link_versioned_name(profile: &Path, cargo_name: &str, versioned: &str) {
    #[cfg(unix)]
    {
        let link = profile.join(versioned);
        if std::fs::read_link(&link).is_ok_and(|current| current == Path::new(cargo_name)) {
            return;
        }
        let _ = std::fs::remove_file(&link);
        if let Err(error) = std::os::unix::fs::symlink(cargo_name, &link) {
            println!(
                "cargo:warning=cannot link {} to {cargo_name}: {error}",
                link.display()
            );
        }
    }
}
