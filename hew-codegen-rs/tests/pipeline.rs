// Domain harness for hew-codegen-rs `pipeline` tests.
// Add new pipeline-behaviour tests under `tests/pipeline/`, wire them into this
// file via #[path] — do not create a new top-level tests/*.rs file.

#[path = "pipeline/dwarf_emitted_object.rs"]
mod dwarf_emitted_object;
#[path = "pipeline/dwarf_variable_dies.rs"]
mod dwarf_variable_dies;
#[path = "pipeline/elab_drop_plans.rs"]
mod elab_drop_plans;
#[path = "pipeline/llvm_native_lowering.rs"]
mod llvm_native_lowering;
#[path = "pipeline/monomorph_emit_test.rs"]
mod monomorph_emit_test;
#[path = "pipeline/pipeline_smoke.rs"]
mod pipeline_smoke;
#[path = "pipeline/verify_pipeline.rs"]
mod verify_pipeline;
