use arbitrary::Unstructured;
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

#[test]
fn generated_iterator_adapters_have_their_trait_module() {
    let program = hew_parity_gen::generate(&mut Unstructured::new(&[0x60, 0x21, 0xfa]), None)
        .expect("generate iterator adapter regression");
    let parsed = hew_parser::parse(&program.source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("workspace root")
        .to_path_buf();
    let checked = Checker::new(ModuleRegistry::new(vec![root])).check_program(&parsed.program);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
}
