//! Credentials shared by native distributed fixture consumers.

/// Install distinct node identities and operator-issued cluster enrolments.
pub fn credentials(mesh: bool, roles: &[&str]) -> tempfile::TempDir {
    use hew_runtime::{cluster_admission, encryption, peer_binding::PeerCredential};
    let kx_dir = tempfile::tempdir().expect("create cluster directory");
    let authority = kx_dir.path().join("authority.key");
    let foreign = kx_dir.path().join("foreign.key");
    cluster_admission::authority_identity(&authority).expect("create authority");
    cluster_admission::authority_identity(&foreign).expect("create foreign authority");
    for role in roles {
        let key = kx_dir.path().join(format!("{role}.key"));
        let credential = if mesh {
            let identity = hew_runtime::quic_mesh::mesh_identity_load_or_create(&key)
                .expect("create mesh identity");
            PeerCredential::Spki(identity.spki().to_vec())
        } else {
            let identity =
                encryption::noise_identity_load_or_create(&key).expect("create Noise identity");
            PeerCredential::NoiseKey(identity.public())
        };
        let text = match &credential {
            PeerCredential::NoiseKey(key) => hew_runtime::peer_binding::hex_lower(key),
            PeerCredential::Spki(spki) => hew_runtime::peer_binding::hex_lower(spki),
        };
        std::fs::write(kx_dir.path().join(format!("{role}.pub")), text)
            .expect("publish public credential");
        for (issuer, suffix) in [(&authority, "enrollment"), (&foreign, "foreign.enrollment")] {
            let bundle = cluster_admission::issue(issuer, &credential).expect("issue enrolment");
            cluster_admission::write_bundle(
                &kx_dir.path().join(format!("{role}.{suffix}")),
                &bundle,
            )
            .expect("install enrolment");
        }
    }
    kx_dir
}
