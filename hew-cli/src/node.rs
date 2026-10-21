//! Operator access to the same persistent credentials used by node admission.

use crate::args::{NodeCommand, NodeIdentityTransport, NodeSubcommand};

pub(crate) fn cmd_node(args: &NodeCommand) {
    let NodeSubcommand::Identity(args) = &args.command;
    let credential = match args.transport {
        NodeIdentityTransport::Tcp => {
            hew_runtime::encryption::noise_identity_load_or_create(&args.key)
                .map(|identity| hew_runtime::peer_binding::hex_lower(&identity.public()))
        }
        NodeIdentityTransport::QuicMesh => {
            hew_runtime::quic_mesh::mesh_identity_load_or_create(&args.key)
                .map(|identity| hew_runtime::peer_binding::hex_lower(identity.spki()))
                .map_err(|error| error.to_string())
        }
    };
    match credential {
        Ok(credential) => println!("{credential}"),
        Err(error) => {
            eprintln!("node identity: {error}");
            std::process::exit(1);
        }
    }
}
