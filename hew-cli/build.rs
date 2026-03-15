use std::process::Command;

fn main() {
    // Git short hash
    if let Ok(output) = Command::new("git")
        .args(["rev-parse", "--short", "HEAD"])
        .output()
    {
        if output.status.success() {
            let hash = String::from_utf8_lossy(&output.stdout).trim().to_string();
            println!("cargo:rustc-env=HEW_GIT_HASH={hash}");
        }
    }

    // Dirty flag
    if let Ok(output) = Command::new("git").args(["status", "--porcelain"]).output() {
        if output.status.success() && !output.stdout.is_empty() {
            println!("cargo:rustc-env=HEW_GIT_DIRTY=true");
        }
    }

    println!("cargo:rerun-if-changed=.git/HEAD");
}
