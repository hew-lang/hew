//! Signed, release-qualified native package archives.

use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

use crate::atomic_fs::{PinnedDir, StagedDir};
use crate::client::{ApiError, RegistryClient};
use crate::manifest::HewManifest;
use crate::native::{artifact_file_name, build_rust_crate, embedded_rustc_identity};
use crate::registry::Registry;

const DESCRIPTOR: &str = "native.json";

/// Compatibility identity shared by the publisher and native linker.
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub struct NativeIdentity {
    pub target: String,
    pub hew: String,
    pub rustc_release: String,
    pub rustc_host: String,
}

impl NativeIdentity {
    #[must_use]
    pub fn current(target: &str) -> Self {
        let rustc = embedded_rustc_identity();
        Self {
            target: target.to_string(),
            hew: hew_compile::VERSION.to_string(),
            rustc_release: rustc.release,
            rustc_host: rustc.host,
        }
    }

    pub(crate) fn route(&self) -> Result<String, String> {
        let parts = [
            &self.target,
            &self.hew,
            &self.rustc_release,
            &self.rustc_host,
        ];
        if parts.iter().any(|part| {
            part.is_empty()
                || matches!(part.as_str(), "." | "..")
                || part.len() > 160
                || !part
                    .bytes()
                    .all(|byte| byte.is_ascii_alphanumeric() || b"-_.+".contains(&byte))
        }) {
            return Err("invalid native artifact compatibility identity".to_string());
        }
        Ok(parts.map(String::as_str).join("/"))
    }
}

/// Package and compatibility claims inside the signed archive.
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub struct NativeDescriptor {
    pub name: String,
    pub version: String,
    pub source_checksum: String,
    pub identity: NativeIdentity,
    pub library: String,
}

/// Publisher and registry proofs for one qualified native archive.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct NativeRecord {
    pub cksum: String,
    pub sig: String,
    pub key_fp: String,
    #[serde(default)]
    pub source_checksum: String,
    #[serde(default)]
    pub registry_sig: String,
    #[serde(default)]
    pub registry_key_fp: String,
    #[serde(default)]
    pub published_at: String,
    pub dl: String,
}

/// Pins the verified archive generation until the linker has read it.
#[derive(Debug)]
pub struct PrebuiltArtifact {
    path: PathBuf,
    locked: crate::lockfile::LockedNative,
    _pin: PinnedDir,
}

impl PrebuiltArtifact {
    #[must_use]
    pub fn path(&self) -> &Path {
        &self.path
    }

    pub(crate) fn lock_pin(&self) -> crate::lockfile::LockedNative {
        self.locked.clone()
    }
}

fn descriptor(root: &Path) -> Result<NativeDescriptor, String> {
    let bytes = std::fs::read(root.join(DESCRIPTOR))
        .map_err(|error| format!("cannot read native artifact descriptor: {error}"))?;
    serde_json::from_slice(&bytes)
        .map_err(|error| format!("invalid native artifact descriptor: {error}"))
}

fn validate_descriptor(
    root: &Path,
    manifest: &HewManifest,
    source_checksum: &str,
    identity: &NativeIdentity,
) -> Result<PathBuf, String> {
    let rust = manifest
        .native
        .as_ref()
        .and_then(crate::manifest::NativeLib::rust_crate)
        .ok_or_else(|| "native artifact requires a Rust staticlib package".to_string())?;
    let expected = NativeDescriptor {
        name: manifest.package.name.clone(),
        version: manifest.package.version.clone(),
        source_checksum: source_checksum.to_string(),
        identity: identity.clone(),
        library: artifact_file_name(rust.lib, rust.kind, &identity.target),
    };
    if rust.kind != "staticlib" || descriptor(root)? != expected {
        return Err("native artifact package, source or runtime ABI identity mismatch".to_string());
    }
    let library = root.join(&expected.library);
    if !library.is_file() {
        return Err("native artifact is missing its static library".to_string());
    }
    Ok(library)
}

/// Fetch a package's exact native artifact; only an absent record permits source build.
///
/// # Errors
///
/// Refuses invalid provenance, damaged archives and failed registry requests.
pub fn for_package(
    root: &Path,
    manifest: &HewManifest,
    target: &str,
    lockfile: Option<&Path>,
) -> Result<Option<PrebuiltArtifact>, String> {
    let locked = locked_package(root, manifest, lockfile)?;
    package_artifact(root, manifest, target, locked.as_ref())
}

pub(crate) fn install(
    root: &Path,
    manifest: &HewManifest,
    target: &str,
) -> Result<Option<PrebuiltArtifact>, String> {
    package_artifact(root, manifest, target, None)
}

fn package_artifact(
    root: &Path,
    manifest: &HewManifest,
    target: &str,
    package: Option<&crate::lockfile::LockedPackage>,
) -> Result<Option<PrebuiltArtifact>, String> {
    let Some((registry, source_checksum, _)) = crate::registry::source_provenance(root, manifest)?
    else {
        if package.is_some() {
            return Err(
                "native registry package is missing verified source provenance".to_string(),
            );
        }
        return Ok(None);
    };
    let identity = NativeIdentity::current(target);
    if package.is_some_and(|package| package.registry.as_deref() != Some(&registry)) {
        return Err("native package registry does not match hew.lock".to_string());
    }
    let locked =
        package.and_then(|package| package.native.iter().find(|pin| pin.identity == identity));
    if locked.is_some_and(|pin| pin.source_checksum != source_checksum) {
        return Err("native artifact source does not match hew.lock".to_string());
    }
    if let Some(locked) = locked {
        let (cache, namespace) = cache_location(&registry, &source_checksum, &identity)?;
        let name = &manifest.package.name;
        let version = &manifest.package.version;
        if let Some(cached) =
            cache.verified_online_cache_entry(&namespace, name, version, &locked.checksum)?
        {
            let path =
                validate_descriptor(cached.pin.path(), manifest, &source_checksum, &identity)?;
            return Ok(Some(PrebuiltArtifact {
                path,
                locked: locked.clone(),
                _pin: cached.pin,
            }));
        }
        if cache
            .pin_package_dir_for_if_present(&namespace, name, version)
            .map_err(|error| format!("native artifact cache integrity check failed: {error}"))?
            .is_some()
        {
            return Err(
                "native artifact cache integrity check failed against hew.lock".to_string(),
            );
        }
    }
    let config = crate::config::load_config().map_err(|error| error.to_string())?;
    let mut client = RegistryClient::with_url(&registry);
    if let Some(remote) = config.registries.as_ref().and_then(|registries| {
        registries
            .values()
            .find(|remote| crate::config::registry_identity(&remote.api) == registry)
    }) {
        if let Some(mode) = remote.wire_names {
            client = client.with_wire_names(mode);
        }
    } else if registry == crate::config::default_registry_identity() {
        client = RegistryClient::new_with_config(&config);
        if client.registry_identity() != registry {
            return Err("native artifact client changed retained registry identity".to_string());
        }
    }
    let artifact = fetch(&client, manifest, &source_checksum, &identity)?;
    if let Some(locked) = locked {
        if artifact.as_ref().map(PrebuiltArtifact::lock_pin).as_ref() != Some(locked) {
            return Err("native artifact does not match hew.lock".to_string());
        }
    }
    Ok(artifact)
}

fn locked_package(
    root: &Path,
    manifest: &HewManifest,
    path: Option<&Path>,
) -> Result<Option<crate::lockfile::LockedPackage>, String> {
    let Some(path) = path.filter(|path| path.is_file()) else {
        return Ok(None);
    };
    let lockfile = crate::lockfile::read_lockfile(path).map_err(|error| error.to_string())?;
    crate::lockfile::validate_lockfile(&lockfile).map_err(|error| error.to_string())?;
    let Some(package) = lockfile.packages.iter().find(|package| {
        package.name == manifest.package.name
            && package.version == manifest.package.version
            && package.source == "registry"
    }) else {
        return Ok(None);
    };
    let checksum =
        crate::checksum::compute_dir_checksum(root).map_err(|error| error.to_string())?;
    if package.checksum.as_deref() != Some(checksum.as_str()) {
        return Err("native package source does not match hew.lock".to_string());
    }
    Ok(Some(package.clone()))
}

fn cache_location(
    registry: &str,
    source_checksum: &str,
    identity: &NativeIdentity,
) -> Result<(Registry, String), String> {
    let cache = Registry::with_root(
        crate::paths::hew_home()
            .join("native")
            .join(identity.route()?),
    );
    Ok((cache, format!("{registry}/{source_checksum}")))
}

pub(crate) fn fetch(
    client: &RegistryClient,
    manifest: &HewManifest,
    source_checksum: &str,
    identity: &NativeIdentity,
) -> Result<Option<PrebuiltArtifact>, String> {
    if manifest
        .native
        .as_ref()
        .and_then(crate::manifest::NativeLib::rust_crate)
        .is_none()
    {
        return Ok(None);
    }
    let record =
        match client.get_native(&manifest.package.name, &manifest.package.version, identity) {
            Ok(record) => record,
            Err(ApiError::Server { status: 404, .. }) => return Ok(None),
            Err(error) => return Err(format!("cannot fetch native artifact record: {error}")),
        };
    if record.sig.is_empty() || record.key_fp.is_empty() {
        return Err("native artifact requires a publisher signature".to_string());
    }
    crate::cli::verify_package_signature(client, &record.cksum, &record.sig, &record.key_fp)?;
    client.verify_native_registry_signature(
        &manifest.package.name,
        &manifest.package.version,
        source_checksum,
        identity,
        &record,
    )?;
    let (cache, namespace) = cache_location(client.registry_identity(), source_checksum, identity)?;
    let name = &manifest.package.name;
    let version = &manifest.package.version;
    let slot = cache.package_slot_for(&namespace, name, version);
    let cached = cache.verified_online_cache_entry(&namespace, name, version, &record.cksum)?;
    let pin = if let Some(cached) = cached {
        cached.pin
    } else {
        if cache
            .pin_package_dir_for_if_present(&namespace, name, version)
            .map_err(|error| format!("native artifact cache integrity check failed: {error}"))?
            .is_some()
        {
            return Err("native artifact cache integrity check failed".to_string());
        }
        let bytes = client
            .download_tarball(&record.dl)
            .map_err(|error| format!("cannot download advertised native artifact: {error}"))?;
        if crate::tarball::checksum_bytes(&bytes) != record.cksum {
            return Err("native artifact integrity check failed".to_string());
        }
        let staged = StagedDir::new(&slot)
            .map_err(|error| format!("cannot stage native artifact: {error}"))?;
        crate::tarball::unpack(&bytes, staged.path()).map_err(|error| error.to_string())?;
        validate_descriptor(staged.path(), manifest, source_checksum, identity)?;
        Registry::write_cache_metadata(
            staged.path(),
            &namespace,
            name,
            version,
            &record.cksum,
            &bytes,
        )?;
        staged
            .publish_pinned(&slot)
            .map_err(|error| format!("cannot publish native artifact cache: {error}"))?
    };
    let path = validate_descriptor(pin.path(), manifest, source_checksum, identity)?;
    Ok(Some(PrebuiltArtifact {
        path,
        locked: crate::lockfile::LockedNative {
            identity: identity.clone(),
            source_checksum: source_checksum.to_string(),
            checksum: record.cksum,
        },
        _pin: pin,
    }))
}

/// Build an archive using the same source snapshot and non-LTO builder as publication.
///
/// # Errors
///
/// Refuses unsupported native kinds, mismatched toolchains and failed builds or packing.
pub fn build(root: &Path, target: &str) -> Result<crate::tarball::PackResult, String> {
    let manifest = crate::manifest::parse_manifest(&root.join("hew.toml"))
        .map_err(|error| error.to_string())?;
    let rust = manifest
        .native
        .as_ref()
        .and_then(crate::manifest::NativeLib::rust_crate)
        .ok_or_else(|| "package has no [native] Rust crate".to_string())?;
    if rust.kind != "staticlib" {
        return Err("native prebuilts require kind = staticlib".to_string());
    }
    let (source_archive, source_checksum) = if let Some((_, checksum, archive)) =
        crate::registry::source_provenance(root, &manifest)?
    {
        (archive, checksum)
    } else {
        let source = crate::tarball::pack(
            root,
            manifest.package.exclude.as_deref().unwrap_or_default(),
            manifest.package.include.as_deref().unwrap_or_default(),
        )
        .map_err(|error| error.to_string())?;
        (source.data, source.checksum)
    };
    let slot = root.join("target/native-package-source");
    let staged = StagedDir::new(&slot).map_err(|error| error.to_string())?;
    crate::tarball::unpack(&source_archive, staged.path()).map_err(|error| error.to_string())?;
    let mut paths = hew_compile::SourcePathPolicy::default();
    let display = PathBuf::from("packages")
        .join(&manifest.package.name)
        .join(&manifest.package.version);
    paths.insert(root, &display);
    paths.insert(staged.path(), &display);
    let artifact = build_rust_crate(
        staged.path(),
        &rust,
        &embedded_rustc_identity(),
        target,
        &paths,
    )?;
    let descriptor = NativeDescriptor {
        name: manifest.package.name.clone(),
        version: manifest.package.version.clone(),
        source_checksum,
        identity: NativeIdentity::current(target),
        library: artifact_file_name(rust.lib, rust.kind, target),
    };
    let files = [
        crate::package_fs::PackageFile {
            path: "hew.toml".to_string(),
            contents: std::fs::read(staged.path().join("hew.toml"))
                .map_err(|error| error.to_string())?,
        },
        crate::package_fs::PackageFile {
            path: DESCRIPTOR.to_string(),
            contents: serde_json::to_vec(&descriptor).map_err(|error| error.to_string())?,
        },
        crate::package_fs::PackageFile {
            path: descriptor.library,
            contents: std::fs::read(artifact).map_err(|error| error.to_string())?,
        },
    ];
    crate::tarball::pack_files(&files).map_err(|error| error.to_string())
}

pub(crate) fn inspect(bytes: &[u8], staging_parent: &Path) -> Result<NativeDescriptor, String> {
    let staged = StagedDir::new(&staging_parent.join("native-inspect"))
        .map_err(|error| error.to_string())?;
    crate::tarball::unpack(bytes, staged.path()).map_err(|error| error.to_string())?;
    let descriptor = descriptor(staged.path())?;
    let manifest = crate::manifest::parse_manifest(&staged.path().join("hew.toml"))
        .map_err(|error| error.to_string())?;
    descriptor.identity.route()?;
    validate_descriptor(
        staged.path(),
        &manifest,
        &descriptor.source_checksum,
        &descriptor.identity,
    )?;
    Ok(descriptor)
}

pub(crate) fn publish(
    client: &RegistryClient,
    bytes: &[u8],
    descriptor: &NativeDescriptor,
    keypair: &crate::signing::KeyPair,
    staging_parent: &Path,
) -> Result<String, String> {
    let record =
        match client.get_native(&descriptor.name, &descriptor.version, &descriptor.identity) {
            Ok(record) => record,
            Err(ApiError::Server { status: 404, .. }) => {
                let checksum = crate::tarball::checksum_bytes(bytes);
                client
                    .publish_native(
                        descriptor,
                        bytes,
                        &keypair.sign(checksum.as_bytes()),
                        &keypair.fingerprint(),
                    )
                    .map_err(|error| error.to_string())?;
                let record = client
                    .get_native(&descriptor.name, &descriptor.version, &descriptor.identity)
                    .map_err(|error| error.to_string())?;
                if record.cksum != checksum || record.key_fp != keypair.fingerprint() {
                    return Err(
                        "published native artifact record does not match submitted bytes/key"
                            .to_string(),
                    );
                }
                record
            }
            Err(error) => return Err(error.to_string()),
        };
    crate::cli::verify_package_signature(client, &record.cksum, &record.sig, &record.key_fp)?;
    let stored = client
        .download_tarball(&record.dl)
        .map_err(|error| error.to_string())?;
    if crate::tarball::checksum_bytes(&stored) != record.cksum
        || inspect(&stored, staging_parent)? != *descriptor
    {
        return Err(
            "published native artifact integrity or source/runtime identity mismatch".to_string(),
        );
    }
    Ok(record.cksum)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn native_rebuild_preserves_verified_source_archive_identity() {
        let source = tempfile::tempdir().unwrap();
        std::fs::create_dir(source.path().join("native")).unwrap();
        std::fs::write(
            source.path().join("hew.toml"),
            "[package]\nname = \"acme.provenance\"\nversion = \"0.1.0\"\nedition = \"2026\"\n[native]\nlib = \"provenance_probe\"\ncrate = \"native\"\n",
        ).unwrap();
        std::fs::write(
            source.path().join("native/Cargo.toml"),
            "[workspace]\n[package]\nname = \"provenance_probe\"\nversion = \"0.1.0\"\nedition = \"2021\"\n[lib]\npath = \"lib.rs\"\ncrate-type = [\"staticlib\"]\n",
        ).unwrap();
        std::fs::write(
            source.path().join("native/lib.rs"),
            "pub fn answer() -> u32 { 42 }\n",
        )
        .unwrap();
        let packed = crate::tarball::pack(source.path(), &[], &[]).unwrap();
        let payload = zstd::decode_all(packed.data.as_slice()).unwrap();
        let published = zstd::encode_all(payload.as_slice(), 22).unwrap();
        let published_checksum = crate::tarball::checksum_bytes(&published);
        let installed = tempfile::tempdir().unwrap();
        crate::tarball::unpack(&published, installed.path()).unwrap();
        Registry::write_cache_metadata(
            installed.path(),
            "https://registry.example",
            "acme.provenance",
            "0.1.0",
            &published_checksum,
            &published,
        )
        .unwrap();
        assert_ne!(
            crate::tarball::pack(installed.path(), &[], &[])
                .unwrap()
                .checksum,
            published_checksum,
        );
        let target = embedded_rustc_identity().host;
        let artifact = build(installed.path(), &target).unwrap();
        let descriptor = inspect(&artifact.data, source.path()).unwrap();
        assert_eq!(descriptor.source_checksum, published_checksum);

        std::fs::write(
            installed.path().join("native/lib.rs"),
            "compile_error!(\"modified registered source must not reach Cargo\");\n",
        )
        .unwrap();
        let error = build(installed.path(), &target).unwrap_err();
        assert!(
            error.contains("package provenance integrity check failed"),
            "{error}"
        );
    }
}
