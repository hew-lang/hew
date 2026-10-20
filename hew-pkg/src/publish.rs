//! Publish core behind `hew publish`.
//!
//! Every phase returns a [`PublishError`] instead of exiting, so the CLI owns
//! process status and the phases can run in-process from tests. The CLI
//! resolves the current directory, configuration, credentials and signing key
//! and passes them in.
//!
//! A remote publish is idempotent: a version the registry already holds with
//! the same content is skipped, and one it holds with different content is
//! [`PublishError::Drift`]. Every upload is read back and verified with the
//! signature checks `hew install` uses, and the served archive is hashed.

use std::fmt;
use std::path::Path;
use std::time::Duration;

use crate::client::ApiError;
use crate::workspace::{self, Workspace};
use crate::{client, index, manifest, native_prebuilt, registry, resolver, signing, tarball};

/// Delays between attempts to download a just-published archive, absorbing
/// CDN and object-store visibility lag after the record is readable.
const READBACK_DOWNLOAD_BACKOFF: [Duration; 3] = [
    Duration::from_secs(1),
    Duration::from_secs(2),
    Duration::from_secs(4),
];

/// A failed publish phase. The CLI prints it after `hew publish: `.
#[derive(Debug)]
pub enum PublishError {
    /// The package is not publishable; nothing touched the network.
    Invalid(String),
    /// No credentials or signing key for an upload; nothing was uploaded.
    Auth(String),
    /// Reading the registry's copy failed, before upload or after a conflict.
    Query(String),
    /// The registry refused or failed the upload.
    Remote(ApiError),
    /// The registry already holds this version with different content.
    Drift {
        name: String,
        version: String,
        local: String,
        registry: String,
    },
    /// The upload was accepted, but the registry's copy does not verify.
    Readback {
        name: String,
        version: String,
        detail: String,
    },
    /// Several members failed the same phase; each is reported.
    Several(Vec<PublishError>),
}

impl PublishError {
    /// One error for `errors`: the error itself when there is one.
    fn combine(mut errors: Vec<Self>) -> Result<(), Self> {
        match errors.len() {
            0 => Ok(()),
            1 => Err(errors.remove(0)),
            _ => Err(Self::Several(errors)),
        }
    }
}

impl fmt::Display for PublishError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Invalid(message) | Self::Auth(message) => f.write_str(message),
            Self::Query(message) => write!(f, "registry query failed: {message}"),
            Self::Remote(error) => write!(f, "remote publish failed: {error}"),
            Self::Drift {
                name,
                version,
                local,
                registry,
            } => write!(
                f,
                "E_PUBLISH_VERSION_DRIFT: {name}@{version} is already published with different \
                 content (local {local}, registry {registry}); bump the version to publish \
                 this tree"
            ),
            Self::Readback {
                name,
                version,
                detail,
            } => write!(
                f,
                "E_PUBLISH_READBACK: {name}@{version} was uploaded, but the registry copy does \
                 not verify: {detail}"
            ),
            Self::Several(errors) => {
                for (index, error) in errors.iter().enumerate() {
                    if index > 0 {
                        f.write_str("\n")?;
                    }
                    error.fmt(f)?;
                }
                Ok(())
            }
        }
    }
}

impl std::error::Error for PublishError {}

/// The credentials an upload needs, resolved only when something uploads.
#[derive(Debug)]
pub struct Signer {
    pub keypair: signing::KeyPair,
    pub token: String,
}

/// Why a version already on the registry needs no upload.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Skip {
    /// The stored archive is byte-identical to the local one.
    Identical,
    /// The archives differ only in compression; the unpacked trees match.
    SameTree,
}

impl fmt::Display for Skip {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Identical => "already published, identical",
            Self::SameTree => "already published, same tree",
        })
    }
}

/// The result of a successful remote publish.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Outcome {
    /// `--dry-run`: the registry lacks this version, so a real run uploads.
    WouldPublish,
    /// The registry already holds this content.
    Skipped(Skip),
    /// Uploaded, then read back and verified.
    Published {
        signature: String,
        key_fingerprint: String,
    },
}

/// A validated, checked and packed package, ready to publish.
#[derive(Debug)]
pub struct PackedPackage {
    pub manifest: manifest::HewManifest,
    pub archive: tarball::PackResult,
}

impl PackedPackage {
    #[must_use]
    pub fn name(&self) -> &str {
        &self.manifest.package.name
    }

    #[must_use]
    pub fn version(&self) -> &str {
        &self.manifest.package.version
    }
}

/// The registry comparison of one prepared source package.
#[derive(Debug)]
pub struct PreparedSource<'a> {
    package: &'a PackedPackage,
    source_checksum: String,
    skip: Option<Skip>,
}

impl PreparedSource<'_> {
    /// The archive identity native attachments must refer to. When an existing
    /// source has the same tree with different compression, this is its stored
    /// checksum, not the newly packed archive's checksum.
    #[must_use]
    pub fn source_checksum(&self) -> &str {
        &self.source_checksum
    }
}

/// A publication whose entire source set has been compared before any upload.
#[derive(Debug)]
pub struct PreparedPublication<'a> {
    sources: Vec<PreparedSource<'a>>,
    natives: Vec<PreparedNative<'a>>,
}

impl<'a> PreparedPublication<'a> {
    /// Read-only source identities selected by the registry comparison.
    #[must_use]
    pub fn sources(&self) -> &[PreparedSource<'a>] {
        &self.sources
    }
}

#[derive(Debug)]
struct PublishedSource {
    checksum: String,
    skip: Skip,
}

/// Compare all sources with the registry, retaining its canonical archive
/// identities. Report every source drift before authentication or uploads.
///
/// # Errors
/// Returns a registry query or source drift error.
pub fn prepare_remote<'a>(
    api_client: &client::RegistryClient,
    packages: &'a [PackedPackage],
) -> Result<PreparedPublication<'a>, PublishError> {
    let mut sources = Vec::new();
    let mut drift = Vec::new();
    for package in packages {
        match compare_with_registry(api_client, package) {
            Ok(published) => sources.push(PreparedSource {
                package,
                source_checksum: published.as_ref().map_or_else(
                    || package.archive.checksum.clone(),
                    |source| source.checksum.clone(),
                ),
                skip: published.map(|source| source.skip),
            }),
            Err(error @ PublishError::Drift { .. }) => drift.push(error),
            Err(error) => return Err(error),
        }
    }
    PublishError::combine(drift)?;
    Ok(PreparedPublication {
        sources,
        natives: Vec::new(),
    })
}

/// An inspected native archive supplied by the publisher. The bytes remain
/// unchanged: a source checksum mismatch never rewrites its descriptor.
#[derive(Debug)]
pub struct NativeArchive {
    descriptor: native_prebuilt::NativeDescriptor,
    data: Vec<u8>,
    checksum: String,
}

impl NativeArchive {
    /// Inspect archive contents and the claimed static library, offline.
    ///
    /// # Errors
    /// Returns an invalid native archive or descriptor error.
    pub fn from_bytes(data: Vec<u8>) -> Result<Self, PublishError> {
        let descriptor = native_prebuilt::inspect(&data, &std::env::temp_dir())
            .map_err(PublishError::Invalid)?;
        let checksum = tarball::checksum_bytes(&data);
        Ok(Self {
            descriptor,
            data,
            checksum,
        })
    }

    #[must_use]
    pub fn descriptor(&self) -> &native_prebuilt::NativeDescriptor {
        &self.descriptor
    }
}

#[derive(Debug)]
struct PreparedNative<'a> {
    archive: &'a NativeArchive,
    source: &'a PackedPackage,
    skip: Option<Skip>,
}

/// An item in the shared source/native publication plan.
#[derive(Clone, Copy, Debug)]
pub enum PublicationItem<'a> {
    Source(&'a PackedPackage),
    Native(&'a NativeArchive),
}

impl PublicationItem<'_> {
    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Source(package) => package.name(),
            Self::Native(archive) => &archive.descriptor.name,
        }
    }

    #[must_use]
    pub fn version(&self) -> &str {
        match self {
            Self::Source(package) => package.version(),
            Self::Native(archive) => &archive.descriptor.version,
        }
    }

    #[must_use]
    pub fn checksum(&self) -> &str {
        match self {
            Self::Source(package) => &package.archive.checksum,
            Self::Native(archive) => &archive.checksum,
        }
    }
}

/// Attach inspected archives to canonical source identities, then discover
/// every requested native identity even when all source versions already exist.
///
/// # Errors
/// Returns invalid membership/source binding, native drift or receipt errors.
pub fn prepare_with_native<'a>(
    api_client: &client::RegistryClient,
    packages: &'a [PackedPackage],
    archives: &'a [NativeArchive],
) -> Result<PreparedPublication<'a>, PublishError> {
    let mut identities = std::collections::BTreeSet::new();
    for archive in archives {
        let descriptor = &archive.descriptor;
        let source = packages
            .iter()
            .find(|source| {
                source.name() == descriptor.name && source.version() == descriptor.version
            })
            .ok_or_else(|| {
                PublishError::Invalid(format!(
                    "native archive {}@{} has no selected source package",
                    descriptor.name, descriptor.version
                ))
            })?;
        if source
            .manifest
            .native
            .as_ref()
            .and_then(manifest::NativeLib::rust_crate)
            .is_none()
        {
            return Err(PublishError::Invalid(format!(
                "{} is not a Rust native package",
                descriptor.name
            )));
        }
        let identity = (
            descriptor.name.clone(),
            descriptor.version.clone(),
            descriptor.identity.route().map_err(PublishError::Invalid)?,
        );
        if !identities.insert(identity) {
            return Err(PublishError::Invalid(format!(
                "duplicate native archive identity for {}@{}",
                descriptor.name, descriptor.version
            )));
        }
    }
    let mut publication = prepare_remote(api_client, packages)?;
    // Validate every descriptor before any native query or authentication.
    for archive in archives {
        let source = publication
            .sources
            .iter()
            .find(|source| {
                source.package.name() == archive.descriptor.name
                    && source.package.version() == archive.descriptor.version
            })
            .expect("membership validated");
        require_native_source(archive, &source.source_checksum)?;
    }
    for archive in archives {
        let source = publication
            .sources
            .iter()
            .find(|source| {
                source.package.name() == archive.descriptor.name
                    && source.package.version() == archive.descriptor.version
            })
            .expect("membership validated");
        publication.natives.push(PreparedNative {
            archive,
            source: source.package,
            skip: compare_native(api_client, archive)?,
        });
    }
    Ok(publication)
}

fn require_native_source(archive: &NativeArchive, expected: &str) -> Result<(), PublishError> {
    if archive.descriptor.source_checksum != expected {
        return Err(PublishError::Invalid(format!(
            "E_PUBLISH_NATIVE_SOURCE: {}@{} native archive refers to {}, but the published source archive is {expected}; rebuild from the verified installed source archive",
            archive.descriptor.name, archive.descriptor.version, archive.descriptor.source_checksum)));
    }
    Ok(())
}

/// Validate, check and pack the package rooted at `root`. Offline.
///
/// # Errors
///
/// Returns [`PublishError::Invalid`] when the manifest, generated sources,
/// frontend check or packed contents do not meet the publish contract.
pub fn prepare(
    root: &Path,
    check_generation: &impl Fn(&Path) -> Result<(), String>,
) -> Result<PackedPackage, PublishError> {
    let manifest_path = root.join("hew.toml");
    if !manifest_path.exists() {
        return Err(PublishError::Invalid(
            "no hew.toml found in current directory".to_string(),
        ));
    }
    let m = manifest::parse_manifest(&manifest_path)
        .map_err(|error| PublishError::Invalid(error.to_string()))?;
    validate(root, &m).map_err(PublishError::Invalid)?;
    check_and_pack(root, m, check_generation)
}

/// Phase 1 for the package rooted at `root`: the publish contract its
/// manifest and its `[native]` crate's Cargo metadata decide. Offline, and
/// run for every member before any member is checked. Native code is not
/// built: a consumer's build does that.
///
/// # Errors
///
/// Returns the diagnostic for the first rule the package breaks.
pub fn validate(root: &Path, m: &manifest::HewManifest) -> Result<(), String> {
    let missing = manifest::validate_for_publish(m);
    if !missing.is_empty() {
        let mut message = format!("{}: missing required fields:", m.package.name);
        for field in &missing {
            message.push_str("\n  - ");
            message.push_str(field);
        }
        return Err(message);
    }
    crate::package_name::check_importable(&m.package.name)?;
    if semver::Version::parse(&m.package.version).is_err() {
        return Err(format!(
            "invalid version `{}`: must be valid semver (e.g. 1.0.0)",
            m.package.version
        ));
    }
    manifest::check_compiler_range(&m.package)?;
    crate::native::check_crate_shape(root, m)
}

/// Phase 1 for a workspace: validate every member in `members`, check their
/// requirements on other members, and order them for publishing.
///
/// # Errors
///
/// Returns every invalid member, or the first requirement or cycle error.
pub fn plan<'w>(
    workspace: &'w Workspace,
    members: &[&Path],
) -> Result<Vec<&'w workspace::Member>, PublishError> {
    let selected = |member: &workspace::Member| members.contains(&member.dir.as_path());
    PublishError::combine(
        workspace
            .members
            .iter()
            .filter(|member| selected(member))
            .filter_map(|member| validate(&member.dir, &member.manifest).err())
            .map(PublishError::Invalid)
            .collect(),
    )?;
    for member in workspace.members.iter().filter(|member| selected(member)) {
        workspace
            .check_member_requirements(member)
            .map_err(|error| PublishError::Invalid(error.to_string()))?;
    }
    Ok(workspace
        .publish_order()
        .map_err(|error| PublishError::Invalid(error.to_string()))?
        .into_iter()
        .filter(|member| selected(member))
        .collect())
}

/// Phase 2 for one validated package: generated sources, the frontend check
/// and packing. Its dependencies must already be in its `.hew/packages`.
///
/// # Errors
///
/// Returns [`PublishError::Invalid`] when generated sources, the frontend
/// check or the packed contents do not meet the publish contract.
pub fn check_and_pack(
    root: &Path,
    m: manifest::HewManifest,
    check_generation: &impl Fn(&Path) -> Result<(), String>,
) -> Result<PackedPackage, PublishError> {
    check_and_pack_with_dependencies(root, m, check_generation, &mut |_, _| Ok(()))
}

pub(crate) fn check_and_pack_with_dependencies(
    root: &Path,
    m: manifest::HewManifest,
    check_generation: &impl Fn(&Path) -> Result<(), String>,
    check_dependency: &mut impl FnMut(&Path, &Path) -> Result<(), String>,
) -> Result<PackedPackage, PublishError> {
    check_generation(root).map_err(PublishError::Invalid)?;

    let entry = m.package.main.as_ref().map_or_else(
        || {
            let library = root.join(manifest::package_root_source(&m.package.name));
            if library.is_file() {
                library
            } else {
                root.join(m.package.main_source())
            }
        },
        |main| root.join(main),
    );
    // The package's own hew.lock pins nothing a consumer resolves, so the
    // check ignores it and the archive leaves it out.
    let checked = hew_compile::check_file(
        &entry.to_string_lossy(),
        &hew_compile::FrontendOptions {
            ignore_lockfile: true,
            ..hew_compile::FrontendOptions::default()
        },
    )
    .map_err(|failure| {
        let mut message = failure.message;
        for diagnostic in failure.diagnostics {
            let text = match diagnostic.kind {
                hew_compile::FrontendDiagnosticKind::Message(error) => error.message,
                hew_compile::FrontendDiagnosticKind::Parse(error) => error.message,
                hew_compile::FrontendDiagnosticKind::Type(error) => error.message,
                hew_compile::FrontendDiagnosticKind::Hir(error) => error.note,
            };
            message.push('\n');
            message.push_str(diagnostic.filename.as_deref().unwrap_or("package"));
            message.push_str(": ");
            message.push_str(&text);
        }
        PublishError::Invalid(message)
    })?;

    let archive = pack(root, &m)?;
    let canonical_root = root.canonicalize().unwrap_or_else(|_| root.to_path_buf());
    for step in &m.generate {
        let generated = crate::generate::output_files(&canonical_root.join(&step.outputs))
            .map_err(PublishError::Invalid)?;
        for path in generated.keys() {
            let relative = Path::new(&step.outputs).join(path);
            if !archive.files.iter().any(|file| file == &relative) {
                return Err(PublishError::Invalid(format!(
                    "E_PUBLISH_MISSING_GENERATED: generated source `{}` is excluded from the package",
                    relative.display()
                )));
            }
        }
    }
    for input in checked.inputs.iter().filter(|input| input.sha256.is_some()) {
        if let Some(dependency_root) = &input.dependency_root {
            check_dependency(&input.path, dependency_root).map_err(PublishError::Invalid)?;
        } else if let Ok(relative) = input.path.strip_prefix(&canonical_root) {
            if !archive.files.iter().any(|file| file == relative) {
                return Err(PublishError::Invalid(format!(
                    "E_PUBLISH_MISSING_INPUT: compilation input `{}` is excluded from the package",
                    relative.display()
                )));
            }
        }
    }

    Ok(PackedPackage {
        manifest: m,
        archive,
    })
}

/// Pack the same source membership used by publish checks and uploads.
pub(crate) fn pack(
    root: &Path,
    m: &manifest::HewManifest,
) -> Result<tarball::PackResult, PublishError> {
    let mut exclude = m.package.exclude.clone().unwrap_or_default();
    exclude.push(crate::project::LOCK_FILE.to_string());
    let include = m.package.include.as_deref().unwrap_or_default();
    tarball::pack(root, &exclude, include).map_err(|error| PublishError::Invalid(error.to_string()))
}

/// The publish request the registry receives for `package`.
fn publish_request(
    package: &PackedPackage,
    signature: String,
    key_fingerprint: String,
) -> client::PublishRequest {
    let m = &package.manifest;
    let deps = m
        .dependencies
        .iter()
        .map(|(name, spec)| {
            let (optional, features, default_features, registry) = match spec {
                manifest::DepSpec::Version(_) => (false, vec![], true, None),
                manifest::DepSpec::Table(t) => (
                    t.optional.unwrap_or(false),
                    t.features.clone().unwrap_or_default(),
                    t.default_features.unwrap_or(true),
                    t.registry.clone(),
                ),
            };
            index::IndexDep {
                name: name.clone(),
                req: spec.version_req().to_string(),
                features,
                optional,
                default_features,
                registry,
            }
        })
        .collect();
    client::PublishRequest {
        metadata: client::PublishMetadata {
            name: m.package.name.clone(),
            vers: m.package.version.clone(),
            description: m.package.description.clone().unwrap_or_default(),
            license: m.package.license.clone().unwrap_or_default(),
            authors: m.package.authors.clone().unwrap_or_default(),
            deps,
            features: m.features.clone(),
            edition: Some(m.package.edition.clone()),
            hew: m.package.hew.clone(),
            keywords: m.package.keywords.clone(),
            categories: m.package.categories.clone(),
            homepage: m.package.homepage.clone(),
            repository: m.package.repository.clone(),
            documentation: m.package.documentation.clone(),
        },
        checksum: package.archive.checksum.clone(),
        signature,
        key_fingerprint,
    }
}

/// Where an upload's credentials come from. [`authenticate`](Auth::authenticate)
/// runs at most once per run, and only when something will upload.
pub trait Auth {
    /// The token and signing key for this run's uploads.
    ///
    /// # Errors
    ///
    /// Returns [`PublishError::Auth`] when no credentials are available.
    fn authenticate(&mut self) -> Result<Signer, PublishError>;

    /// A fresh token for `keypair` after the registry refused the current
    /// one with 401, or `None` when this source cannot renew it.
    ///
    /// # Errors
    ///
    /// Returns [`PublishError::Auth`] when renewal was attempted and failed.
    fn refresh(&mut self, keypair: &signing::KeyPair) -> Result<Option<String>, PublishError> {
        let _ = keypair;
        Ok(None)
    }
}

/// Credentials resolved once by a closure and never renewed.
pub(crate) struct Fixed<F>(pub Option<F>);

impl<F: FnOnce() -> Result<Signer, PublishError>> Auth for Fixed<F> {
    fn authenticate(&mut self) -> Result<Signer, PublishError> {
        (self.0.take().expect("authenticate runs at most once"))()
    }
}

/// Publish `package` to the registry `api_client` addresses.
///
/// Queries the registry first: an existing identical version is skipped and a
/// differing one is [`PublishError::Drift`]. `authenticate` runs only when an
/// upload follows, and never for `dry_run`. A 409 from the upload goes back
/// through the same comparison, so a rerun or concurrent publish converges.
/// After upload, the stored record and served archive are verified.
///
/// # Errors
///
/// Returns the [`PublishError`] of the first phase that fails.
pub fn publish_remote(
    api_client: client::RegistryClient,
    package: &PackedPackage,
    dry_run: bool,
    authenticate: impl FnOnce() -> Result<Signer, PublishError>,
    sleep: &dyn Fn(Duration),
) -> Result<Outcome, PublishError> {
    let mut outcomes = Vec::new();
    publish_all(
        api_client,
        std::slice::from_ref(package),
        dry_run,
        &mut Fixed(Some(authenticate)),
        sleep,
        &mut |_, outcome| outcomes.push(outcome.clone()),
    )?;
    Ok(outcomes.remove(0))
}

/// Phases 3 to 5 for `packages`, already checked, packed and in publish
/// order.
///
/// Every package is compared with the registry first, and all drift is
/// reported before anything uploads. `auth` authenticates only when a package
/// will upload, and never for `dry_run`. Packages then upload and read back
/// one at a time, in order, stopping at the first failure because later
/// packages may depend on it. `report` receives each package's outcome as it
/// is decided.
///
/// # Errors
///
/// Returns the [`PublishError`] of the first phase that fails.
pub fn publish_all(
    api_client: client::RegistryClient,
    packages: &[PackedPackage],
    dry_run: bool,
    auth: &mut dyn Auth,
    sleep: &dyn Fn(Duration),
    report: &mut dyn FnMut(&PackedPackage, &Outcome),
) -> Result<(), PublishError> {
    let publication = prepare_remote(&api_client, packages)?;
    publish_prepared(
        api_client,
        &publication,
        dry_run,
        auth,
        sleep,
        &mut |item, outcome| {
            if let PublicationItem::Source(package) = item {
                report(package, outcome);
            }
        },
    )
}

/// Execute a prepared publication through the one authentication and upload
/// authority. A dry run reports the same plan without acquiring credentials.
///
/// # Errors
/// Returns an authentication, upload or readback error.
pub fn publish_prepared(
    api_client: client::RegistryClient,
    publication: &PreparedPublication<'_>,
    dry_run: bool,
    auth: &mut dyn Auth,
    sleep: &dyn Fn(Duration),
    report: &mut dyn FnMut(PublicationItem<'_>, &Outcome),
) -> Result<(), PublishError> {
    let mut uploads = Vec::new();
    for source in &publication.sources {
        let package = source.package;
        match source.skip {
            Some(skip) => report(PublicationItem::Source(package), &Outcome::Skipped(skip)),
            None if dry_run => report(PublicationItem::Source(package), &Outcome::WouldPublish),
            None => uploads.push(package),
        }
    }
    let mut native_uploads = Vec::new();
    for native in &publication.natives {
        let item = PublicationItem::Native(native.archive);
        match native.skip {
            Some(skip) => report(item, &Outcome::Skipped(skip)),
            None if dry_run => report(item, &Outcome::WouldPublish),
            None => native_uploads.push(native),
        }
    }
    if uploads.is_empty() && native_uploads.is_empty() {
        return Ok(());
    }

    let signer = auth.authenticate()?;
    let mut api_client = api_client.with_token(signer.token);
    for package in uploads {
        let outcome = upload(&mut api_client, package, &signer.keypair, auth, sleep)?;
        report(PublicationItem::Source(package), &outcome);
    }
    // A concurrent source publisher can win with a differently compressed
    // archive after preparation. Check the resulting binding before natives.
    for native in &native_uploads {
        let stored = compare_with_registry(&api_client, native.source)?.ok_or_else(|| {
            PublishError::Query("native source disappeared after publication".into())
        })?;
        require_native_source(native.archive, &stored.checksum)?;
    }
    for native in native_uploads {
        let outcome = upload_native(
            &mut api_client,
            native.archive,
            &signer.keypair,
            auth,
            sleep,
        )?;
        report(PublicationItem::Native(native.archive), &outcome);
    }
    Ok(())
}

/// Phase 5 for one package: sign, upload, and read back.
fn upload(
    api_client: &mut client::RegistryClient,
    package: &PackedPackage,
    keypair: &signing::KeyPair,
    auth: &mut dyn Auth,
    sleep: &dyn Fn(Duration),
) -> Result<Outcome, PublishError> {
    let signature = keypair.sign(package.archive.checksum.as_bytes());
    let key_fingerprint = keypair.fingerprint();
    let request = publish_request(package, signature.clone(), key_fingerprint.clone());
    let put = |api_client: &client::RegistryClient| {
        api_client.publish(
            package.name(),
            package.version(),
            &package.archive.data,
            &request,
        )
    };
    match upload_with_refresh(api_client, keypair, auth, put)? {
        Ok(()) => {}
        Err(conflict @ ApiError::Server { status: 409, .. }) => {
            return match compare_with_registry(api_client, package)? {
                Some(source) => Ok(Outcome::Skipped(source.skip)),
                // The registry reported the version taken but does not serve it.
                None => Err(PublishError::Remote(conflict)),
            };
        }
        Err(error) => return Err(PublishError::Remote(error)),
    }

    read_back(api_client, package, &signature, &key_fingerprint, sleep)?;
    Ok(Outcome::Published {
        signature,
        key_fingerprint,
    })
}

fn upload_with_refresh(
    api_client: &mut client::RegistryClient,
    keypair: &signing::KeyPair,
    auth: &mut dyn Auth,
    put: impl Fn(&client::RegistryClient) -> Result<(), ApiError>,
) -> Result<Result<(), ApiError>, PublishError> {
    let mut result = put(api_client);
    if let Err(ApiError::Server { status: 401, .. }) = result {
        if let Some(token) = auth.refresh(keypair)? {
            api_client.set_token(token);
            result = put(api_client);
        }
    }
    Ok(result)
}

fn compare_native(
    api_client: &client::RegistryClient,
    archive: &NativeArchive,
) -> Result<Option<Skip>, PublishError> {
    let descriptor = &archive.descriptor;
    let record =
        match api_client.get_native(&descriptor.name, &descriptor.version, &descriptor.identity) {
            Ok(record) => record,
            Err(ApiError::Server { status: 404, .. }) => return Ok(None),
            Err(error) => return Err(PublishError::Query(error.to_string())),
        };
    let served = verify_native_record(api_client, archive, &record, &|_| {})?;
    if record.cksum == archive.checksum {
        Ok(Some(Skip::Identical))
    } else if tarball::unpacked_tree_checksum(&served)
        .map_err(|e| PublishError::Query(e.to_string()))?
        == tarball::unpacked_tree_checksum(&archive.data)
            .map_err(|e| PublishError::Invalid(e.to_string()))?
    {
        Ok(Some(Skip::SameTree))
    } else {
        Err(PublishError::Invalid(format!(
            "E_PUBLISH_NATIVE_DRIFT: {}@{} already has different bytes for native identity {}",
            descriptor.name,
            descriptor.version,
            descriptor.identity.route().map_err(PublishError::Invalid)?
        )))
    }
}

fn verify_native_record(
    api_client: &client::RegistryClient,
    archive: &NativeArchive,
    record: &native_prebuilt::NativeRecord,
    sleep: &dyn Fn(Duration),
) -> Result<Vec<u8>, PublishError> {
    let descriptor = &archive.descriptor;
    let fail = |detail: String| PublishError::Readback {
        name: descriptor.name.clone(),
        version: descriptor.version.clone(),
        detail,
    };
    crate::cli::verify_package_signature(api_client, &record.cksum, &record.sig, &record.key_fp)
        .map_err(fail)?;
    api_client
        .verify_native_registry_signature(
            &descriptor.name,
            &descriptor.version,
            &descriptor.source_checksum,
            &descriptor.identity,
            record,
        )
        .map_err(fail)?;
    let mut delays = READBACK_DOWNLOAD_BACKOFF.iter();
    let served = loop {
        match api_client.download_tarball(&record.dl) {
            Ok(bytes) => break bytes,
            Err(error) => match (&error, delays.next()) {
                (ApiError::Server { status: 404, .. }, Some(delay)) => sleep(*delay),
                _ => {
                    return Err(fail(format!(
                        "cannot download served native archive: {error}"
                    )))
                }
            },
        }
    };
    if tarball::checksum_bytes(&served) != record.cksum
        || native_prebuilt::inspect(&served, &std::env::temp_dir()).map_err(fail)? != *descriptor
    {
        return Err(fail(
            "native archive integrity or descriptor mismatch".into(),
        ));
    }
    Ok(served)
}

fn upload_native(
    api_client: &mut client::RegistryClient,
    archive: &NativeArchive,
    keypair: &signing::KeyPair,
    auth: &mut dyn Auth,
    sleep: &dyn Fn(Duration),
) -> Result<Outcome, PublishError> {
    let signature = keypair.sign(archive.checksum.as_bytes());
    let key_fingerprint = keypair.fingerprint();
    let descriptor = &archive.descriptor;
    let put = |api_client: &client::RegistryClient| {
        api_client.publish_native(descriptor, &archive.data, &signature, &key_fingerprint)
    };
    match upload_with_refresh(api_client, keypair, auth, put)? {
        Ok(()) => {}
        Err(conflict @ ApiError::Server { status: 409, .. }) => {
            return match compare_native(api_client, archive)? {
                Some(skip) => Ok(Outcome::Skipped(skip)),
                None => Err(PublishError::Remote(conflict)),
            };
        }
        Err(error) => return Err(PublishError::Remote(error)),
    }
    let record = api_client
        .get_native(&descriptor.name, &descriptor.version, &descriptor.identity)
        .map_err(|error| PublishError::Query(error.to_string()))?;
    if record.cksum != archive.checksum
        || record.sig != signature
        || record.key_fp != key_fingerprint
    {
        return Err(PublishError::Readback {
            name: descriptor.name.clone(),
            version: descriptor.version.clone(),
            detail: "native stored checksum, signature or signing key differs from upload".into(),
        });
    }
    verify_native_record(api_client, archive, &record, sleep)?;
    Ok(Outcome::Published {
        signature,
        key_fingerprint,
    })
}

/// Compare `package` with the registry's copy of the same version.
///
/// `Ok(None)` means the registry lacks the version. A differing archive
/// checksum is resolved by downloading the stored archive, checking that it
/// matches its record, and comparing unpacked trees.
fn compare_with_registry(
    api_client: &client::RegistryClient,
    package: &PackedPackage,
) -> Result<Option<PublishedSource>, PublishError> {
    let (name, version) = (package.name(), package.version());
    let record = match api_client.get_package_version(name, version) {
        Ok(record) => record,
        Err(ApiError::Server { status: 404, .. }) => return Ok(None),
        Err(error) => return Err(PublishError::Query(error.to_string())),
    };
    let local = &package.archive.checksum;
    if record.cksum == *local {
        return Ok(Some(PublishedSource {
            checksum: record.cksum,
            skip: Skip::Identical,
        }));
    }
    let dl = record.dl.as_deref().ok_or_else(|| {
        PublishError::Query(format!(
            "registry record for {name}@{version} has no download URL"
        ))
    })?;
    let stored = api_client
        .download_package_tarball(name, version, dl)
        .map_err(|error| PublishError::Query(error.to_string()))?;
    let served = tarball::checksum_bytes(&stored);
    if served != record.cksum {
        return Err(PublishError::Query(format!(
            "registry archive for {name}@{version} hashes to {served}, but its record says {}",
            record.cksum
        )));
    }
    let tree = |archive: &[u8], whose: &str| {
        tarball::unpacked_tree_checksum(archive).map_err(|error| {
            PublishError::Query(format!(
                "cannot unpack {whose} archive for {name}@{version}: {error}"
            ))
        })
    };
    if tree(&stored, "the registry")? == tree(&package.archive.data, "the local")? {
        Ok(Some(PublishedSource {
            checksum: record.cksum,
            skip: Skip::SameTree,
        }))
    } else {
        Err(PublishError::Drift {
            name: name.to_string(),
            version: version.to_string(),
            local: local.clone(),
            registry: record.cksum,
        })
    }
}

/// Read the stored record back and require that it is the upload: same
/// identity, checksum, key and signature, a valid publisher signature and
/// registry counter-signature, and served bytes that hash to the checksum.
fn read_back(
    api_client: &client::RegistryClient,
    package: &PackedPackage,
    signature: &str,
    key_fingerprint: &str,
    sleep: &dyn Fn(Duration),
) -> Result<(), PublishError> {
    let (name, version) = (package.name(), package.version());
    let fail = |detail: String| PublishError::Readback {
        name: name.to_string(),
        version: version.to_string(),
        detail,
    };
    let mismatch = |field: &str, expected: &str, actual: &str| {
        fail(format!("{field} is {actual}, expected {expected}"))
    };

    let record = api_client
        .get_package_version(name, version)
        .map_err(|error| fail(format!("cannot read the stored record: {error}")))?;
    let local = &package.archive.checksum;
    if record.cksum != *local {
        return Err(mismatch("cksum", local, &record.cksum));
    }
    if record.key_fp != key_fingerprint {
        return Err(mismatch("key_fp", key_fingerprint, &record.key_fp));
    }
    if record.sig != signature {
        return Err(mismatch("sig", signature, &record.sig));
    }
    if record.registry_sig.is_none() || record.published_at.is_none() {
        return Err(fail(
            "the record has no registry counter-signature".to_string(),
        ));
    }
    let entry = resolver::ResolvedEntry {
        registry_name: record
            .registry_name
            .clone()
            .unwrap_or_else(|| record.name.clone()),
        version: record.vers.clone(),
        checksum: record.cksum.clone(),
        dl: record.dl.clone(),
        sig: record.sig.clone(),
        key_fp: record.key_fp.clone(),
        registry_sig: record.registry_sig.clone(),
        published_at: record.published_at.clone(),
    };
    crate::cli::verify_supplied_package_signatures(api_client, name, &entry).map_err(fail)?;

    let dl = record
        .dl
        .as_deref()
        .ok_or_else(|| fail("the record has no download URL".to_string()))?;
    let mut delays = READBACK_DOWNLOAD_BACKOFF.iter();
    let served = loop {
        match api_client.download_package_tarball(name, version, dl) {
            Ok(bytes) => break bytes,
            Err(error) => match (&error, delays.next()) {
                (ApiError::Server { status: 404, .. }, Some(delay)) => sleep(*delay),
                _ => return Err(fail(format!("cannot download the served archive: {error}"))),
            },
        }
    };
    let served_checksum = tarball::checksum_bytes(&served);
    if served_checksum != *local {
        return Err(mismatch("the served archive", local, &served_checksum));
    }
    Ok(())
}

/// Publish a packed archive into the local registry only, under the same
/// namespaced cache slot that `--locked` and `--offline` resolution read
/// from (`Registry::package_slot_for`). `--local` never combines with
/// `--registry` (`conflicts_with` in `PkgCommand::Publish`), so every local
/// publish targets the default registry's identity — see hew-lang/hew#3233.
/// Refuses, rather than silently overwrites, when that slot already holds a
/// *verified* registry fetch (one `write_cache_metadata` populated) whose
/// tree checksum differs: a local test build must never shadow a package a
/// real registry actually served at the same name and version. When the
/// incoming content matches the verified fetch exactly, this is a no-op that
/// leaves the existing generation (and its cache metadata and retained
/// archive) untouched, rather than restaging an identical tree.
pub(crate) fn publish_local_package(
    registry: &registry::Registry,
    name: &str,
    version: &str,
    archive: &[u8],
) -> std::io::Result<crate::atomic_fs::PinnedDir> {
    let identity = crate::config::default_registry_identity();
    let dest = registry.package_slot_for(&identity, name, version);
    let incoming_checksum = tarball::unpacked_tree_checksum(archive)
        .map_err(|error| std::io::Error::other(format!("cannot checksum archive: {error}")))?;
    if let Ok(Some(existing)) = registry.pin_package_dir_for_if_present(&identity, name, version) {
        if let Some(verified_checksum) = registry::Registry::verified_tree_checksum(existing.path())
        {
            if verified_checksum != incoming_checksum {
                return Err(std::io::Error::other(format!(
                    "{name}@{version} is already cached from a verified registry fetch with a \
                     different tree checksum; a local publish must not shadow it (bump the \
                     version to publish a different build)"
                )));
            }
            // Identical content: leave the verified generation in place rather
            // than restaging it. `StagedDir::publish_pinned` always swaps in a
            // fresh generation holding only the unpacked tree, which would
            // drop this generation's cache metadata and retained archive even
            // though nothing actually changed — silently turning the next
            // publish (any content) into an unverified, unguarded overwrite.
            return Ok(existing);
        }
    }
    let staged = crate::atomic_fs::StagedDir::new(&dest)?;
    tarball::unpack(archive, staged.path()).map_err(std::io::Error::other)?;
    staged.publish_pinned(&dest)
}
