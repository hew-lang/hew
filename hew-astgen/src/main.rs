mod codegen;
mod hew_emit;
mod model;
mod parse;
mod special_cases;
mod type_map;

use std::path::PathBuf;

fn generate_reader(ast_source: &str, module_source: &str) -> String {
    let mut types = parse::extract_types(ast_source);
    let module_types = parse::extract_types(module_source);
    types.extend(module_types);

    eprintln!("Extracted {} type definitions:", types.len());
    for t in &types {
        let kind = match t {
            model::TypeDef::SimpleEnum(e) => format!("simple enum ({} variants)", e.variants.len()),
            model::TypeDef::TaggedEnum(e) => {
                format!("tagged enum ({} variants)", e.variants.len())
            }
            model::TypeDef::Struct(s) => format!("struct ({} fields)", s.fields.len()),
        };
        eprintln!("  {} - {}", t.name(), kind);
    }

    let type_map = type_map::TypeMap::new();
    codegen::generate(&types, &type_map)
}

fn generate_hew_exports(
    source: &str,
    module_prefix: &str,
) -> Result<(Vec<hew_emit::ExternFn>, String), hew_emit::HewEmitError> {
    let fns = hew_emit::extract_extern_fns(source)?;
    let hew_code = hew_emit::generate_hew_module(&fns, module_prefix);
    Ok((fns, hew_code))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let mut mode: Option<String> = None;
    let mut ast_path: Option<PathBuf> = None;
    let mut module_path: Option<PathBuf> = None;
    let mut input_path: Option<PathBuf> = None;
    let mut output_path: Option<PathBuf> = None;
    let mut prefix: Option<String> = None;

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--mode" => {
                i += 1;
                mode = Some(args[i].clone());
            }
            "--ast" => {
                i += 1;
                ast_path = Some(PathBuf::from(&args[i]));
            }
            "--module" => {
                i += 1;
                module_path = Some(PathBuf::from(&args[i]));
            }
            "--input" => {
                i += 1;
                input_path = Some(PathBuf::from(&args[i]));
            }
            "--output" => {
                i += 1;
                output_path = Some(PathBuf::from(&args[i]));
            }
            "--prefix" => {
                i += 1;
                prefix = Some(args[i].clone());
            }
            _ => {
                eprintln!("Unknown argument: {}", args[i]);
                std::process::exit(1);
            }
        }
        i += 1;
    }

    let output_path = output_path.expect("--output <path> is required");

    match mode.as_deref() {
        Some("hew") => {
            // --mode hew: emit a Hew module skeleton from a Rust crate lib.rs.
            // Usage: hew-astgen --mode hew --input <lib.rs> --output <out.hew> [--prefix <fn_prefix>]
            let input_path = input_path.expect("--input <path> is required with --mode hew");
            let source = std::fs::read_to_string(&input_path)
                .unwrap_or_else(|e| panic!("Failed to read {}: {e}", input_path.display()));

            let module_prefix = prefix.as_deref().unwrap_or("");
            let (fns, hew_code) =
                generate_hew_exports(&source, module_prefix).unwrap_or_else(|e| {
                    panic!(
                        "Failed to generate Hew exports from {}: {e}",
                        input_path.display()
                    )
                });
            eprintln!(
                "Extracted {} extern fns from {}",
                fns.len(),
                input_path.display()
            );
            for f in &fns {
                eprintln!("  {}", f.name);
            }

            std::fs::write(&output_path, &hew_code)
                .unwrap_or_else(|e| panic!("Failed to write {}: {e}", output_path.display()));

            eprintln!(
                "Generated {} bytes to {}",
                hew_code.len(),
                output_path.display()
            );
        }
        None | Some("cpp") => {
            // Default mode: emit C++ msgpack deserializer from AST definitions.
            // Usage: hew-astgen --ast <ast.rs> --module <module.rs> --output <out.cpp>
            let ast_path = ast_path.expect("--ast <path> is required");
            let module_path = module_path.expect("--module <path> is required");

            let ast_source = std::fs::read_to_string(&ast_path)
                .unwrap_or_else(|e| panic!("Failed to read {}: {e}", ast_path.display()));
            let module_source = std::fs::read_to_string(&module_path)
                .unwrap_or_else(|e| panic!("Failed to read {}: {e}", module_path.display()));

            let cpp_code = generate_reader(&ast_source, &module_source);

            std::fs::write(&output_path, &cpp_code)
                .unwrap_or_else(|e| panic!("Failed to write {}: {e}", output_path.display()));

            eprintln!(
                "Generated {} bytes to {}",
                cpp_code.len(),
                output_path.display()
            );
        }
        Some(other) => {
            eprintln!("Unknown --mode value: {other}. Valid values: hew, cpp");
            std::process::exit(1);
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::PathBuf;

    use super::generate_reader;

    #[test]
    fn generated_reader_matches_checked_in_file() {
        let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let ast_path = manifest_dir.join("../hew-parser/src/ast.rs");
        let module_path = manifest_dir.join("../hew-parser/src/module.rs");
        let output_path = manifest_dir.join("../hew-codegen/src/msgpack_reader.cpp");

        let ast_source = fs::read_to_string(&ast_path)
            .unwrap_or_else(|e| panic!("Failed to read {}: {e}", ast_path.display()));
        let module_source = fs::read_to_string(&module_path)
            .unwrap_or_else(|e| panic!("Failed to read {}: {e}", module_path.display()));
        let checked_in = fs::read_to_string(&output_path)
            .unwrap_or_else(|e| panic!("Failed to read {}: {e}", output_path.display()));

        let generated = generate_reader(&ast_source, &module_source);
        // Normalize CRLF → LF so the comparison works on Windows (git
        // may check out .cpp files with CRLF, but the generator emits LF).
        assert_eq!(
            generated,
            checked_in.replace("\r\n", "\n"),
            "hew-codegen/src/msgpack_reader.cpp is out of date; run `make astgen`"
        );
    }
}
