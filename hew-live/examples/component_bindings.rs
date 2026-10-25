//! Emit the reference router's C guest bindings from its WIT contract.

fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let directory = std::env::args_os()
        .nth(1)
        .ok_or("usage: component_bindings OUTPUT_DIRECTORY")?;
    hew_live::component::write_guest_bindings(std::path::Path::new(&directory))
}
