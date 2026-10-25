//! Typed component contracts for independently owned portable router instances.

use std::path::Path;

use wit_bindgen_core::{wit_parser::Resolve, Files};

pub mod cache;
mod hew_bindings;
pub mod host;

/// The portable reference service's source contract, shared by guest bindings.
pub const ROUTER_WIT: &str = include_str!("../wit/router.wit");

/// Host bindings retain component async calls instead of blocking the guest.
pub mod bindings {
    wasmtime::component::bindgen!({
        path: "wit",
        world: "router",
        imports: { default: async | store | trappable },
        exports: { default: async | store },
    });
}

/// Generate canonical C bindings, ordinary Hew service functions and the SDK
/// lifecycle adapter, including async callbacks and subtasks.
///
/// # Errors
/// Returns WIT parsing, unsupported service-type, generation or filesystem errors.
pub fn write_guest_bindings(
    directory: &Path,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let mut resolve = Resolve::default();
    let package = resolve.push_str("router.wit", ROUTER_WIT)?;
    let world = resolve.select_world(&[package], Some("router"))?;
    let mut files = Files::default();
    wit_bindgen_c::Opts::default()
        .build()
        .generate(&mut resolve, world, &mut files)?;
    let (hew, services) = hew_bindings::generate(&resolve, world)?;
    files.push("router_services.hew", hew.as_bytes());
    let adapter = include_str!("component/router_adapter.c")
        .replace("/* HEW_WIT_SERVICE_BINDINGS */", &services);
    files.push("router_adapter.c", adapter.as_bytes());
    std::fs::create_dir_all(directory)?;
    for (name, contents) in files.iter() {
        std::fs::write(directory.join(name), contents)?;
    }
    Ok(())
}
