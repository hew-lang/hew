use std::path::PathBuf;

fn main() {
    if let Err(err) = run() {
        eprintln!("hew-capability-gen: {err}");
        std::process::exit(1);
    }
}

fn run() -> Result<(), String> {
    let mut check = false;
    for arg in std::env::args().skip(1) {
        match arg.as_str() {
            "--check" => check = true,
            "-h" | "--help" => {
                println!("Usage: hew-capability-gen [--check]");
                return Ok(());
            }
            _ => return Err(format!("unknown argument `{arg}`")),
        }
    }

    let root = std::env::var_os("HEW_BUILD_SOURCE_ROOT")
        .map(PathBuf::from)
        .map_or_else(std::env::current_dir, Ok)
        .map_err(|err| format!("resolve invocation source root: {err}"))?;
    let manifest = hew_capability_gen::load_manifest(&root)?;
    if check {
        let stale = hew_capability_gen::stale_outputs(&root, &manifest)?;
        if stale.is_empty() {
            println!("WASM capability outputs are current.");
            return Ok(());
        }
        let paths = stale
            .iter()
            .map(|path| {
                path.strip_prefix(&root)
                    .unwrap_or(path)
                    .display()
                    .to_string()
            })
            .collect::<Vec<_>>()
            .join(", ");
        return Err(format!(
            "generated outputs are stale: {paths}; run `cargo run -p hew-capability-gen`"
        ));
    }

    hew_capability_gen::write_outputs(&root, &manifest)?;
    println!("Generated WASM capability Rust, playground, and matrix tables.");
    Ok(())
}
