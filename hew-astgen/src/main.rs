mod codegen;
mod model;
mod parse;
mod special_cases;
mod type_map;

use std::path::PathBuf;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let mut ast_path: Option<PathBuf> = None;
    let mut module_path: Option<PathBuf> = None;
    let mut output_path: Option<PathBuf> = None;

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--ast" => {
                i += 1;
                ast_path = Some(PathBuf::from(&args[i]));
            }
            "--module" => {
                i += 1;
                module_path = Some(PathBuf::from(&args[i]));
            }
            "--output" => {
                i += 1;
                output_path = Some(PathBuf::from(&args[i]));
            }
            _ => {
                eprintln!("Unknown argument: {}", args[i]);
                std::process::exit(1);
            }
        }
        i += 1;
    }

    let ast_path = ast_path.expect("--ast <path> is required");
    let module_path = module_path.expect("--module <path> is required");
    let output_path = output_path.expect("--output <path> is required");

    // Read source files
    let ast_source = std::fs::read_to_string(&ast_path)
        .unwrap_or_else(|e| panic!("Failed to read {}: {e}", ast_path.display()));
    let module_source = std::fs::read_to_string(&module_path)
        .unwrap_or_else(|e| panic!("Failed to read {}: {e}", module_path.display()));

    // Extract types from both files
    let mut types = parse::extract_types(&ast_source);
    let module_types = parse::extract_types(&module_source);
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

    // Generate C++ code
    let type_map = type_map::TypeMap::new();
    let cpp_code = codegen::generate(&types, &type_map);

    // Write output
    std::fs::write(&output_path, &cpp_code)
        .unwrap_or_else(|e| panic!("Failed to write {}: {e}", output_path.display()));

    eprintln!(
        "Generated {} bytes to {}",
        cpp_code.len(),
        output_path.display()
    );
}
