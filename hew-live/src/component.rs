//! Typed component contracts for independently owned portable router instances.

use std::path::Path;

use wit_bindgen_core::{wit_parser::Resolve, Files};

pub mod build;
pub mod cache;
mod hew_bindings;
pub mod host;
pub mod network;
pub mod telemetry;

/// The portable reference service's source contract, shared by guest bindings.
pub const ROUTER_WIT: &str = include_str!("../wit/router.wit");

#[derive(Clone, Copy)]
enum GuestContract {
    Router,
    Telemetry,
}

impl GuestContract {
    fn world(self) -> &'static str {
        match self {
            Self::Router => "router",
            Self::Telemetry => "telemetry",
        }
    }

    fn wit(self) -> &'static str {
        match self {
            Self::Router => ROUTER_WIT,
            Self::Telemetry => include_str!("../wit/telemetry/telemetry.wit"),
        }
    }

    fn adapter_header(self) -> &'static [u8] {
        match self {
            Self::Router => include_bytes!("component/router_contract.h"),
            Self::Telemetry => include_bytes!("component/telemetry_contract.h"),
        }
    }
}

/// Host bindings retain component async calls instead of blocking the guest.
pub mod bindings {
    wasmtime::component::bindgen!({
        path: "wit",
        world: "router",
        additional_derives: [serde::Serialize, serde::Deserialize],
        imports: { default: async | store | trappable },
        exports: { default: async | store },
    });
}

/// Return the ordinary Hew service module generated from the canonical WIT.
/// Editors and portable builds consume this same source, without a disk module.
///
/// # Errors
/// Returns WIT parsing or unsupported service-type diagnostics.
pub fn service_module_source() -> Result<String, Box<dyn std::error::Error + Send + Sync>> {
    service_module_for(GuestContract::Router)
}

fn service_module_for(
    contract: GuestContract,
) -> Result<String, Box<dyn std::error::Error + Send + Sync>> {
    let mut resolve = Resolve::default();
    let package = resolve.push_str("guest.wit", contract.wit())?;
    let world = resolve.select_world(&[package], Some(contract.world()))?;
    Ok(hew_bindings::generate(&resolve, world)?.0)
}

/// Generate canonical C bindings, ordinary Hew service functions and the SDK
/// lifecycle adapter, including async callbacks and subtasks.
///
/// # Errors
/// Returns WIT parsing, unsupported service-type, generation or filesystem errors.
pub fn write_guest_bindings(
    directory: &Path,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    write_guest_bindings_for(directory, GuestContract::Router)
}

fn write_guest_bindings_for(
    directory: &Path,
    contract: GuestContract,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let mut resolve = Resolve::default();
    let package = resolve.push_str("guest.wit", contract.wit())?;
    let world = resolve.select_world(&[package], Some(contract.world()))?;
    let mut files = Files::default();
    wit_bindgen_c::Opts::default()
        .build()
        .generate(&mut resolve, world, &mut files)?;
    let (hew, services) = hew_bindings::generate(&resolve, world)?;
    files.push(
        &format!("{}_services.hew", contract.world()),
        hew.as_bytes(),
    );
    let adapter = include_str!("component/guest_adapter.c")
        .replace("/* HEW_WIT_SERVICE_BINDINGS */", &services);
    files.push(
        &format!("{}_adapter.c", contract.world()),
        adapter.as_bytes(),
    );
    files.push("guest_contract.h", contract.adapter_header());
    write_host_headers(directory)?;
    for (name, contents) in files.iter() {
        std::fs::write(directory.join(name), contents)?;
    }
    Ok(())
}

// Embed the canonical same-build SDK declarations so installed companions do
// not depend on the source checkout used to build them.
fn write_host_headers(directory: &Path) -> Result<(), std::io::Error> {
    std::fs::create_dir_all(directory)?;
    std::fs::write(
        directory.join("hew.h"),
        include_bytes!("../../hew-cabi/include/hew.h"),
    )?;
    std::fs::write(
        directory.join("hew_host.h"),
        include_bytes!("../../hew-cabi/include/hew_host.h"),
    )
}
