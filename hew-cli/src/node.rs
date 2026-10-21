//! Operator access to the same persistent credentials used by node admission.

use crate::args::{NodeCommand, NodeIdentityTransport, NodeSubcommand};

pub(crate) fn cmd_node(args: &NodeCommand) {
    match &args.command {
        NodeSubcommand::Identity(args) => identity(args),
        NodeSubcommand::Authority(args) => {
            finish(
                "authority",
                hew_runtime::cluster_admission::authority_identity(&args.key),
            );
        }
        NodeSubcommand::Admit(args) => {
            let result = admit(args);
            if let Err(error) = result {
                eprintln!("node admit: {error}");
                std::process::exit(1);
            }
        }
    }
}

fn finish(command: &str, result: std::io::Result<String>) {
    match result {
        Ok(value) => println!("{value}"),
        Err(error) => {
            eprintln!("node {command}: {error}");
            std::process::exit(1);
        }
    }
}

fn admit(args: &crate::args::NodeAdmissionArgs) -> std::io::Result<()> {
    use hew_runtime::peer_binding::PeerCredential;
    let invalid = || {
        std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            "invalid transport credential",
        )
    };
    if !args.credential.len().is_multiple_of(2)
        || args.credential.is_empty()
        || !args.credential.bytes().all(|byte| byte.is_ascii_hexdigit())
    {
        return Err(invalid());
    }
    let bytes = args
        .credential
        .as_bytes()
        .chunks_exact(2)
        .map(|pair| {
            let text = std::str::from_utf8(pair).map_err(|_| invalid())?;
            u8::from_str_radix(text, 16).map_err(|_| invalid())
        })
        .collect::<Result<Vec<_>, _>>()?;
    let credential = match args.transport {
        NodeIdentityTransport::Tcp => {
            PeerCredential::NoiseKey(bytes.try_into().map_err(|_| invalid())?)
        }
        NodeIdentityTransport::QuicMesh => PeerCredential::Spki(bytes),
    };
    let bytes = hew_runtime::cluster_admission::issue(&args.authority, &credential)?;
    hew_runtime::cluster_admission::write_bundle(&args.output, &bytes)
}

fn identity(args: &crate::args::NodeIdentityArgs) {
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
