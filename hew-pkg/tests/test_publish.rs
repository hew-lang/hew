//! Remote publish against a stateful loopback registry.
//!
//! The registry stores real records: it hashes each uploaded archive,
//! counter-signs the canonical `registry:v1:` message with its own key and
//! serves the publisher key it was given, so read-back runs the same
//! signature checks `hew install` uses. Tamper switches corrupt what it
//! serves after an upload. Every test asserts the requests the registry saw,
//! not internal calls.

use std::collections::BTreeMap;
use std::fs;
use std::path::Path;
use std::process::{Command, Output};
use std::sync::{Arc, Mutex};
use std::thread::{self, JoinHandle};
use std::time::Duration;

use base64::Engine as _;
use clap::Parser;
use hew_pkg::client::RegistryClient;
use hew_pkg::config::WireNames;
use hew_pkg::publish::{self, Outcome, PackedPackage, PublishError, Signer, Skip};
use hew_pkg::signing::KeyPair;
use serde_json::{json, Value};
use tempfile::TempDir;
use tiny_http::{Header, Response, Server};

const NAME: &str = "acme.widget";
const VERSION: &str = "1.0.0";
const CHILD_ARGS: &str = "HEW_PUBLISH_TEST_ARGS";

// ── Loopback registry ───────────────────────────────────────────────────────

/// What the registry corrupts in the record or archive it serves.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Tamper {
    None,
    Cksum,
    KeyFp,
    Name,
    Sig,
    RegistrySig,
    NoRegistrySig,
    Archive,
}

#[derive(Default)]
struct State {
    /// `(wire name, version)` -> stored index entry.
    records: BTreeMap<(String, String), Value>,
    /// `(wire name, version)` -> stored archive bytes.
    archives: BTreeMap<(String, String), Vec<u8>>,
    /// Publisher fingerprint -> base64 public key.
    keys: BTreeMap<String, String>,
    /// `METHOD path` for every request, in arrival order.
    log: Vec<String>,
    tamper: Option<Tamper>,
    /// On the next PUT, store this archive instead (as a concurrent publish
    /// would) and answer 409.
    conflict_with: Option<(Vec<u8>, KeyPairBytes)>,
    /// Answer this many archive downloads with 404 before serving them.
    archive_misses: usize,
    /// Answer every PUT with 500 and store nothing.
    fail_puts: bool,
    /// Refuse uploads after this many have been accepted.
    fail_after_puts: Option<usize>,
    /// Trusted publishing: the `audience` of every OIDC token request.
    oidc_audiences: Vec<String>,
    /// OIDC tokens issued and not yet exchanged; each exchanges once.
    oidc_unused: Vec<String>,
    oidc_issued: usize,
    /// The `public_key` of every grant exchange, in order.
    exchanges: Vec<String>,
    /// Grant tokens the PUT route accepts; empty means any token.
    grants: Vec<String>,
    /// Answer exchanges with a fingerprint for a different key.
    grant_wrong_key: bool,
    /// Revoke every grant after this many accepted PUTs.
    expire_grants_after_puts: Option<usize>,
    accepted_puts: usize,
}

type KeyPairBytes = [u8; 32];

struct MockRegistry {
    origin: String,
    server: Arc<Server>,
    state: Arc<Mutex<State>>,
    registry_key: Arc<KeyPair>,
    worker: Option<JoinHandle<()>>,
    /// The wire grammar clients use against this registry.
    wire_names: WireNames,
}

impl MockRegistry {
    fn start() -> Self {
        Self::start_with(WireNames::Dotted)
    }

    fn start_with(wire_names: WireNames) -> Self {
        let server = Arc::new(Server::http("127.0.0.1:0").unwrap());
        let origin = format!("http://{}", server.server_addr());
        let state = Arc::new(Mutex::new(State::default()));
        let registry_key = Arc::new(KeyPair::generate());
        let worker = {
            let server = Arc::clone(&server);
            let state = Arc::clone(&state);
            let registry_key = Arc::clone(&registry_key);
            let origin = origin.clone();
            thread::spawn(move || {
                while let Ok(mut request) = server.recv() {
                    let method = request.method().as_str().to_owned();
                    let url = request.url().to_owned();
                    let path = url.split('?').next().unwrap().to_owned();
                    let bearer = request
                        .headers()
                        .iter()
                        .find(|header| header.field.equiv("Authorization"))
                        .map(|header| {
                            header
                                .value
                                .as_str()
                                .trim_start_matches("Bearer ")
                                .to_owned()
                        });
                    let mut body = Vec::new();
                    let _ = request.as_reader().read_to_end(&mut body);
                    let (status, reply) = if path == "/oidc" {
                        issue_oidc_token(&state, &url)
                    } else {
                        route(
                            &state,
                            &registry_key,
                            &origin,
                            (&method, &path, bearer.as_deref()),
                            &body,
                        )
                    };
                    let response = Response::from_data(reply)
                        .with_status_code(status)
                        .with_header(
                            Header::from_bytes("Content-Type", "application/json").unwrap(),
                        );
                    let _ = request.respond(response);
                }
            })
        };
        Self {
            origin,
            server,
            state,
            registry_key,
            worker: Some(worker),
            wire_names,
        }
    }

    /// `NAME` as this registry stores and routes it.
    fn wire_name(&self) -> String {
        match self.wire_names {
            WireNames::Dotted => NAME.to_owned(),
            WireNames::Slash => NAME.replace('.', "/"),
        }
    }

    fn api(&self) -> String {
        format!("{}/api/v1", self.origin)
    }

    fn client(&self) -> RegistryClient {
        RegistryClient::with_url(self.api()).with_wire_names(self.wire_names)
    }

    fn register_key(&self, keypair: &KeyPair) {
        self.state
            .lock()
            .unwrap()
            .keys
            .insert(keypair.fingerprint(), keypair.public_key_base64());
    }

    /// Store `archive` as `NAME@VERSION`, exactly as an accepted upload would.
    fn seed(&self, archive: &[u8], publisher: &KeyPair) {
        self.seed_package(&self.wire_name(), VERSION, archive, publisher);
    }

    /// Store `archive` as `name@version`, exactly as an accepted upload would.
    fn seed_package(&self, name: &str, version: &str, archive: &[u8], publisher: &KeyPair) {
        self.register_key(publisher);
        let mut state = self.state.lock().unwrap();
        store(
            &mut state,
            &self.registry_key,
            name,
            version,
            archive,
            publisher,
        );
    }

    /// The `PUT` paths in arrival order.
    fn put_paths(&self) -> Vec<String> {
        self.log()
            .into_iter()
            .filter_map(|line| line.strip_prefix("PUT ").map(ToOwned::to_owned))
            .collect()
    }

    fn stored_archive(&self, name: &str, version: &str) -> Option<Vec<u8>> {
        self.state
            .lock()
            .unwrap()
            .archives
            .get(&(name.to_owned(), version.to_owned()))
            .cloned()
    }

    fn stored_package(&self, name: &str, version: &str) -> Option<Value> {
        self.state
            .lock()
            .unwrap()
            .records
            .get(&(name.to_owned(), version.to_owned()))
            .cloned()
    }

    fn set_tamper(&self, tamper: Tamper) {
        self.state.lock().unwrap().tamper = Some(tamper);
    }

    fn conflict_with(&self, archive: Vec<u8>, publisher: &KeyPair) {
        self.register_key(publisher);
        self.state.lock().unwrap().conflict_with = Some((archive, *publisher.secret_key_bytes()));
    }

    fn miss_archive_downloads(&self, count: usize) {
        self.state.lock().unwrap().archive_misses = count;
    }

    fn log(&self) -> Vec<String> {
        self.state.lock().unwrap().log.clone()
    }

    fn puts(&self) -> usize {
        self.log()
            .iter()
            .filter(|line| line.starts_with("PUT "))
            .count()
    }

    fn stored(&self) -> Option<Value> {
        self.state
            .lock()
            .unwrap()
            .records
            .get(&(self.wire_name(), VERSION.to_owned()))
            .cloned()
    }
}

impl Drop for MockRegistry {
    fn drop(&mut self) {
        self.server.unblock();
        if let Some(worker) = self.worker.take() {
            let _ = worker.join();
        }
    }
}

fn store(
    state: &mut State,
    registry_key: &KeyPair,
    name: &str,
    version: &str,
    archive: &[u8],
    publisher: &KeyPair,
) {
    let cksum = hew_pkg::tarball::checksum_bytes(archive);
    let sig = publisher.sign(cksum.as_bytes());
    insert_record(
        state,
        registry_key,
        (name, version),
        archive.to_vec(),
        &sig,
        &publisher.fingerprint(),
    );
}

/// Store an accepted archive with the record the registry would write.
fn insert_record(
    state: &mut State,
    registry_key: &KeyPair,
    (name, version): (&str, &str),
    archive: Vec<u8>,
    sig: &str,
    key_fp: &str,
) {
    let cksum = hew_pkg::tarball::checksum_bytes(&archive);
    let published_at = "2026-10-18T00:00:00.000Z";
    let registry_sig = registry_key
        .sign(format!("registry:v1:{name}@{version}:{cksum}:{sig}:{published_at}").as_bytes());
    let key = (name.to_owned(), version.to_owned());
    state.records.insert(
        key.clone(),
        json!({
            "name": name,
            "vers": version,
            "deps": [],
            "features": {},
            "cksum": cksum,
            "sig": sig,
            "key_fp": key_fp,
            "yanked": false,
            "registry_sig": registry_sig,
            "registry_key_fp": registry_key.fingerprint(),
            "published_at": published_at,
        }),
    );
    state.archives.insert(key, archive);
}

fn not_found() -> (u16, Vec<u8>) {
    (404, br#"{"error":"not_found"}"#.to_vec())
}

/// Split `/api/v1/packages/{name}/{version}` into its identity.
fn package_version(path: &str) -> Option<(String, String)> {
    let rest = path.strip_prefix("/api/v1/packages/")?;
    let (name, version) = rest.rsplit_once('/')?;
    Some((name.to_owned(), version.to_owned()))
}

/// `GET /oidc?...&audience=...`, the runner's OIDC token endpoint.
fn issue_oidc_token(state: &Mutex<State>, url: &str) -> (u16, Vec<u8>) {
    let mut state = state.lock().unwrap();
    state.log.push("GET /oidc".to_owned());
    let audience = url
        .split_once("&audience=")
        .map(|(_, audience)| percent_decode(audience))
        .unwrap_or_default();
    state.oidc_audiences.push(audience);
    state.oidc_issued += 1;
    let jwt = format!("jwt-{}", state.oidc_issued);
    state.oidc_unused.push(jwt.clone());
    (200, serde_json::to_vec(&json!({ "value": jwt })).unwrap())
}

/// `POST /login/github-actions`, shaped like the registry's exchange: each
/// OIDC token exchanges once, and the supplied key is registered.
fn exchange_grant(state: &mut State, body: &[u8]) -> (u16, Vec<u8>) {
    let request: Value = serde_json::from_slice(body).unwrap();
    let jwt = request["oidc_token"].as_str().unwrap().to_owned();
    let public_key = request["public_key"].as_str().unwrap().to_owned();
    let Some(position) = state.oidc_unused.iter().position(|unused| *unused == jwt) else {
        return (401, br#"{"error":"token already exchanged"}"#.to_vec());
    };
    state.oidc_unused.remove(position);
    let bytes = base64::engine::general_purpose::STANDARD
        .decode(&public_key)
        .unwrap();
    let fingerprint = hew_pkg::signing::compute_fingerprint(&bytes);
    state.keys.insert(fingerprint.clone(), public_key.clone());
    state.exchanges.push(public_key);
    let token = format!("grant-{}", state.exchanges.len());
    state.grants.push(token.clone());
    let key_fingerprint = if state.grant_wrong_key {
        KeyPair::generate().fingerprint()
    } else {
        fingerprint
    };
    (
        200,
        serde_json::to_vec(&json!({
            "token": token,
            "expires_in": 900,
            "identity": "hew-ecosystem",
            "key_fingerprint": key_fingerprint,
        }))
        .unwrap(),
    )
}

fn route(
    state: &Mutex<State>,
    registry_key: &KeyPair,
    origin: &str,
    (method, path, bearer): (&str, &str, Option<&str>),
    body: &[u8],
) -> (u16, Vec<u8>) {
    let mut state = state.lock().unwrap();
    state.log.push(format!("{method} {path}"));
    match method {
        "POST" if path == "/api/v1/login/github-actions" => exchange_grant(&mut state, body),
        "PUT"
            if !state.grants.is_empty()
                && !bearer.is_some_and(|token| state.grants.iter().any(|grant| grant == token)) =>
        {
            (401, br#"{"error":"unauthorized"}"#.to_vec())
        }
        "PUT" => {
            let reply = accept_upload(&mut state, registry_key, path, body);
            if reply.0 == 201 {
                state.accepted_puts += 1;
                if state.expire_grants_after_puts == Some(state.accepted_puts) {
                    state.grants = vec!["expired".to_owned()];
                }
            }
            reply
        }
        "GET" if path == "/api/v1/registry-key" => (
            200,
            serde_json::to_vec(&json!({
                "key_id": "registry",
                "public_key": registry_key.public_key_base64(),
                "algorithm": "ed25519",
            }))
            .unwrap(),
        ),
        "GET" if path.starts_with("/api/v1/keys/") => {
            let fingerprint = percent_decode(path.strip_prefix("/api/v1/keys/").unwrap());
            match state.keys.get(&fingerprint) {
                Some(public_key) => (
                    200,
                    serde_json::to_vec(&json!({
                        "fingerprint": fingerprint,
                        "public_key": public_key,
                        "key_type": "ed25519",
                        "github_user": "publisher",
                        "github_id": 1,
                    }))
                    .unwrap(),
                ),
                None => not_found(),
            }
        }
        "GET" if path.starts_with("/tarballs/") => serve_archive(&mut state, path),
        "GET" => serve_record(&state, registry_key, origin, path),
        _ => (405, br#"{"error":"method"}"#.to_vec()),
    }
}

/// `PUT /packages/{name}/{version}`, shaped like the registry's handler.
fn accept_upload(
    state: &mut State,
    registry_key: &KeyPair,
    path: &str,
    body: &[u8],
) -> (u16, Vec<u8>) {
    let Some((name, version)) = package_version(path) else {
        return not_found();
    };
    if state.fail_puts
        || state
            .fail_after_puts
            .is_some_and(|limit| state.accepted_puts >= limit)
    {
        return (500, br#"{"error":"registry down"}"#.to_vec());
    }
    if let Some((archive, secret)) = state.conflict_with.take() {
        let racer = KeyPair::from_secret_bytes(&secret).unwrap();
        store(state, registry_key, &name, &version, &archive, &racer);
        return (409, br#"{"error":"conflict"}"#.to_vec());
    }
    if state.records.contains_key(&(name.clone(), version.clone())) {
        return (409, br#"{"error":"conflict"}"#.to_vec());
    }
    let envelope: Value = serde_json::from_slice(body).unwrap();
    let request: Value = serde_json::from_str(envelope["metadata"].as_str().unwrap()).unwrap();
    let archive = base64::engine::general_purpose::STANDARD
        .decode(envelope["tarball"].as_str().unwrap())
        .unwrap();
    assert_eq!(
        request["checksum"],
        hew_pkg::tarball::checksum_bytes(&archive).as_str(),
        "client checksum"
    );
    let key_fp = request["key_fingerprint"].as_str().unwrap();
    assert!(state.keys.contains_key(key_fp), "unregistered key");
    let sig = request["signature"].as_str().unwrap();
    insert_record(state, registry_key, (&name, &version), archive, sig, key_fp);
    (201, br#"{"ok":true}"#.to_vec())
}

/// `GET /tarballs/{name}/{version}.tar.zst`, the CDN route records point at.
fn serve_archive(state: &mut State, path: &str) -> (u16, Vec<u8>) {
    let rest = path.strip_prefix("/tarballs/").unwrap();
    let (name, file) = rest.rsplit_once('/').unwrap();
    let version = file.strip_suffix(".tar.zst").unwrap();
    if state.archive_misses > 0 {
        state.archive_misses -= 1;
        return not_found();
    }
    match state.archives.get(&(name.to_owned(), version.to_owned())) {
        Some(archive) => {
            let mut archive = archive.clone();
            if state.tamper == Some(Tamper::Archive) {
                archive.push(0);
            }
            (200, archive)
        }
        None => not_found(),
    }
}

/// `GET /packages/{name}/{version}`, with any tamper switch applied.
fn serve_record(state: &State, registry_key: &KeyPair, origin: &str, path: &str) -> (u16, Vec<u8>) {
    let Some((name, version)) = package_version(path) else {
        return not_found();
    };
    let Some(record) = state.records.get(&(name.clone(), version.clone())) else {
        return not_found();
    };
    let mut record = record.clone();
    record["dl"] = json!(format!("{origin}/tarballs/{name}/{version}.tar.zst"));
    match state.tamper.unwrap_or(Tamper::None) {
        Tamper::Cksum => record["cksum"] = json!(format!("sha256:{}", "0".repeat(64))),
        Tamper::KeyFp => record["key_fp"] = json!(KeyPair::generate().fingerprint()),
        Tamper::Name => record["name"] = json!("acme.other"),
        Tamper::Sig => {
            let sig = record["sig"].as_str().unwrap();
            let flipped = if sig.ends_with('0') { '1' } else { '0' };
            record["sig"] = json!(format!("{}{flipped}", &sig[..sig.len() - 1]));
        }
        Tamper::RegistrySig => {
            record["registry_sig"] = json!(registry_key.sign(b"registry:v1:something else"));
        }
        Tamper::NoRegistrySig => {
            let record = record.as_object_mut().unwrap();
            record.remove("registry_sig");
            record.remove("published_at");
        }
        Tamper::None | Tamper::Archive => {}
    }
    (
        200,
        serde_json::to_vec(&json!({ "metadata": { "name": name }, "version": record })).unwrap(),
    )
}

fn percent_decode(text: &str) -> String {
    let bytes = text.as_bytes();
    let mut out = Vec::new();
    let mut index = 0;
    while index < bytes.len() {
        if bytes[index] == b'%' && index + 2 < bytes.len() {
            let hex = std::str::from_utf8(&bytes[index + 1..index + 3]).unwrap();
            out.push(u8::from_str_radix(hex, 16).unwrap());
            index += 3;
        } else {
            out.push(bytes[index]);
            index += 1;
        }
    }
    String::from_utf8(out).unwrap()
}

// ── Fixtures ────────────────────────────────────────────────────────────────

fn write_package(root: &Path, value: i64) {
    fs::write(
        root.join("hew.toml"),
        format!(
            "[package]\nname = \"{NAME}\"\nversion = \"{VERSION}\"\ndescription = \"widget\"\n\
             license = \"MIT\"\nauthors = [\"a\"]\nedition = \"2026\"\n"
        ),
    )
    .unwrap();
    fs::write(
        root.join("widget.hew"),
        format!("pub fn value() -> i64 {{ {value} }}\n"),
    )
    .unwrap();
}

/// Write a publishable package `name@version` whose root module defines
/// `source`, with `extra` appended to its `[package]` table.
fn write_named_package(root: &Path, name: &str, version: &str, extra: &str, source: &str) {
    fs::create_dir_all(root).unwrap();
    fs::write(
        root.join("hew.toml"),
        format!(
            "[package]\nname = \"{name}\"\nversion = \"{version}\"\ndescription = \"d\"\n\
             license = \"MIT\"\nauthors = [\"a\"]\nedition = \"2026\"\n{extra}"
        ),
    )
    .unwrap();
    let leaf = name.rsplit('.').next().unwrap();
    fs::write(root.join(format!("{leaf}.hew")), source).unwrap();
}

fn packed(value: i64) -> (TempDir, PackedPackage) {
    let dir = tempfile::tempdir().unwrap();
    write_package(dir.path(), value);
    let package = publish::prepare(dir.path(), &|_| Ok(())).unwrap();
    (dir, package)
}

/// The same archive contents recompressed at another zstd level.
fn reencoded(archive: &[u8]) -> Vec<u8> {
    let tar = zstd::decode_all(archive).unwrap();
    let again = zstd::encode_all(tar.as_slice(), 19).unwrap();
    assert_ne!(again, archive, "re-encoding must change the bytes");
    again
}

fn no_sleep(_: Duration) {}

fn run(
    mock: &MockRegistry,
    package: &PackedPackage,
    dry_run: bool,
    publisher: &KeyPair,
) -> Result<Outcome, PublishError> {
    mock.register_key(publisher);
    let secret = *publisher.secret_key_bytes();
    publish::publish_remote(
        mock.client(),
        package,
        dry_run,
        || {
            Ok(Signer {
                keypair: KeyPair::from_secret_bytes(&secret).unwrap(),
                token: "token".to_owned(),
            })
        },
        &no_sleep,
    )
}

fn version_path() -> String {
    format!("/api/v1/packages/{NAME}/{VERSION}")
}

// ── Upload and read-back ────────────────────────────────────────────────────

#[test]
fn upload_is_read_back_and_verified() {
    let mock = MockRegistry::start();
    let (_dir, package) = packed(1);
    let publisher = KeyPair::generate();

    let outcome = run(&mock, &package, false, &publisher).unwrap();

    let stored = mock.stored().expect("record stored");
    assert_eq!(stored["cksum"], package.archive.checksum.as_str());
    assert_eq!(stored["key_fp"], publisher.fingerprint().as_str());
    assert_eq!(
        outcome,
        Outcome::Published {
            signature: stored["sig"].as_str().unwrap().to_owned(),
            key_fingerprint: publisher.fingerprint(),
        }
    );
    let log = mock.log();
    let put = log
        .iter()
        .position(|line| line.starts_with("PUT "))
        .unwrap();
    let after: Vec<_> = log[put + 1..].iter().map(String::as_str).collect();
    assert!(
        after.contains(&format!("GET {}", version_path()).as_str()),
        "{log:?}"
    );
    assert!(
        after
            .iter()
            .any(|line| line.starts_with("GET /api/v1/keys/")),
        "{log:?}"
    );
    assert!(after.contains(&"GET /api/v1/registry-key"), "{log:?}");
    assert!(
        after.contains(&format!("GET /tarballs/{NAME}/{VERSION}.tar.zst").as_str()),
        "{log:?}"
    );
}

#[test]
fn refused_upload_is_a_remote_error_without_readback() {
    let mock = MockRegistry::start();
    let (_dir, package) = packed(1);
    mock.state.lock().unwrap().fail_puts = true;

    let error = run(&mock, &package, false, &KeyPair::generate()).unwrap_err();

    assert!(matches!(error, PublishError::Remote(_)), "{error}");
    assert!(
        error.to_string().starts_with("remote publish failed:"),
        "{error}"
    );
    let log = mock.log();
    assert_eq!(
        log.last().map(String::as_str),
        Some(format!("PUT {}", version_path()).as_str())
    );
}

#[test]
fn slash_wire_names_upload_read_back_and_skip() {
    let mock = MockRegistry::start_with(WireNames::Slash);
    let (_dir, package) = packed(1);
    let publisher = KeyPair::generate();

    let outcome = run(&mock, &package, false, &publisher).unwrap();

    assert!(matches!(outcome, Outcome::Published { .. }), "{outcome:?}");
    let stored = mock.stored().expect("record stored under the slash name");
    assert_eq!(stored["name"], "acme/widget");
    let log = mock.log();
    assert!(
        log.contains(&"PUT /api/v1/packages/acme/widget/1.0.0".to_owned()),
        "{log:?}"
    );
    assert!(
        log.contains(&"GET /tarballs/acme/widget/1.0.0.tar.zst".to_owned()),
        "{log:?}"
    );

    let again = run(&mock, &package, false, &publisher).unwrap();
    assert_eq!(again, Outcome::Skipped(Skip::Identical));
    assert_eq!(mock.puts(), 1);
}

#[test]
fn each_tampered_readback_is_refused() {
    for tamper in [
        Tamper::Cksum,
        Tamper::KeyFp,
        Tamper::Name,
        Tamper::Sig,
        Tamper::RegistrySig,
        Tamper::NoRegistrySig,
        Tamper::Archive,
    ] {
        let mock = MockRegistry::start();
        let (_dir, package) = packed(1);
        let publisher = KeyPair::generate();
        mock.register_key(&publisher);
        let secret = *publisher.secret_key_bytes();
        // Tamper only with what is served after the upload is stored.
        let tamper_after_put = || {
            mock.set_tamper(tamper);
            Ok(Signer {
                keypair: KeyPair::from_secret_bytes(&secret).unwrap(),
                token: "token".to_owned(),
            })
        };
        let error =
            publish::publish_remote(mock.client(), &package, false, tamper_after_put, &no_sleep)
                .unwrap_err();
        assert!(
            matches!(error, PublishError::Readback { .. }),
            "{tamper:?}: {error}"
        );
        assert!(
            error
                .to_string()
                .starts_with("E_PUBLISH_READBACK: acme.widget@1.0.0"),
            "{tamper:?}: {error}"
        );
        assert_eq!(mock.puts(), 1, "{tamper:?}");
    }
}

#[test]
fn readback_retries_a_missing_archive_then_verifies() {
    let mock = MockRegistry::start();
    let (_dir, package) = packed(1);
    let publisher = KeyPair::generate();
    mock.miss_archive_downloads(3);
    let slept = Mutex::new(Vec::new());
    mock.register_key(&publisher);
    let secret = *publisher.secret_key_bytes();

    let outcome = publish::publish_remote(
        mock.client(),
        &package,
        false,
        || {
            Ok(Signer {
                keypair: KeyPair::from_secret_bytes(&secret).unwrap(),
                token: "token".to_owned(),
            })
        },
        &|delay| slept.lock().unwrap().push(delay.as_secs()),
    )
    .unwrap();

    assert!(matches!(outcome, Outcome::Published { .. }));
    assert_eq!(*slept.lock().unwrap(), [1, 2, 4]);
}

#[test]
fn readback_gives_up_after_three_retries() {
    let mock = MockRegistry::start();
    let (_dir, package) = packed(1);
    mock.miss_archive_downloads(4);

    let error = run(&mock, &package, false, &KeyPair::generate()).unwrap_err();

    assert!(matches!(error, PublishError::Readback { .. }), "{error}");
    assert!(error.to_string().contains("404"), "{error}");
}

// ── Idempotence ─────────────────────────────────────────────────────────────

#[test]
fn identical_version_is_skipped_without_upload() {
    let mock = MockRegistry::start();
    let (_dir, package) = packed(1);
    mock.seed(&package.archive.data, &KeyPair::generate());

    let outcome = run(&mock, &package, false, &KeyPair::generate()).unwrap();

    assert_eq!(outcome, Outcome::Skipped(Skip::Identical));
    assert_eq!(mock.puts(), 0, "{:?}", mock.log());
}

#[test]
fn different_content_is_drift_without_upload() {
    let mock = MockRegistry::start();
    let (_dir, package) = packed(1);
    let (_other_dir, other) = packed(2);
    mock.seed(&other.archive.data, &KeyPair::generate());

    let error = run(&mock, &package, false, &KeyPair::generate()).unwrap_err();

    let message = error.to_string();
    assert!(
        message.starts_with("E_PUBLISH_VERSION_DRIFT: acme.widget@1.0.0"),
        "{message}"
    );
    assert!(message.contains(&package.archive.checksum), "{message}");
    assert!(message.contains(&other.archive.checksum), "{message}");
    assert_eq!(mock.puts(), 0, "{:?}", mock.log());
}

#[test]
fn recompressed_identical_tree_is_skipped() {
    let mock = MockRegistry::start();
    let (_dir, package) = packed(1);
    mock.seed(&reencoded(&package.archive.data), &KeyPair::generate());

    let outcome = run(&mock, &package, false, &KeyPair::generate()).unwrap();

    assert_eq!(outcome, Outcome::Skipped(Skip::SameTree));
    assert_eq!(mock.puts(), 0);
}

#[test]
fn recompressed_changed_tree_is_drift() {
    let mock = MockRegistry::start();
    let (_dir, package) = packed(1);
    let (_other_dir, other) = packed(3);
    mock.seed(&reencoded(&other.archive.data), &KeyPair::generate());

    let error = run(&mock, &package, false, &KeyPair::generate()).unwrap_err();

    assert!(matches!(error, PublishError::Drift { .. }), "{error}");
    assert_eq!(mock.puts(), 0);
}

#[test]
fn conflict_with_matching_record_converges_without_second_put() {
    let mock = MockRegistry::start();
    let (_dir, package) = packed(1);
    mock.conflict_with(package.archive.data.clone(), &KeyPair::generate());

    let outcome = run(&mock, &package, false, &KeyPair::generate()).unwrap();

    assert_eq!(outcome, Outcome::Skipped(Skip::Identical));
    assert_eq!(mock.puts(), 1, "{:?}", mock.log());
}

#[test]
fn conflict_with_differing_record_is_drift() {
    let mock = MockRegistry::start();
    let (_dir, package) = packed(1);
    let (_other_dir, other) = packed(2);
    mock.conflict_with(other.archive.data.clone(), &KeyPair::generate());

    let error = run(&mock, &package, false, &KeyPair::generate()).unwrap_err();

    assert!(matches!(error, PublishError::Drift { .. }), "{error}");
    assert_eq!(mock.puts(), 1, "{:?}", mock.log());
}

// ── Dry run ─────────────────────────────────────────────────────────────────

#[test]
fn dry_run_queries_without_authenticating() {
    let mock = MockRegistry::start();
    let (_dir, package) = packed(1);

    let outcome = publish::publish_remote(
        mock.client(),
        &package,
        true,
        || panic!("dry run must not authenticate"),
        &no_sleep,
    )
    .unwrap();

    assert_eq!(outcome, Outcome::WouldPublish);
    assert_eq!(mock.log(), [format!("GET {}", version_path())]);
}

#[test]
fn dry_run_reports_drift() {
    let mock = MockRegistry::start();
    let (_dir, package) = packed(1);
    let (_other_dir, other) = packed(2);
    mock.seed(&other.archive.data, &KeyPair::generate());

    let error = publish::publish_remote(
        mock.client(),
        &package,
        true,
        || panic!("dry run must not authenticate"),
        &no_sleep,
    )
    .unwrap_err();

    assert!(matches!(error, PublishError::Drift { .. }), "{error}");
}

// ── `hew publish` command ───────────────────────────────────────────────────

#[derive(Parser)]
struct FixtureCommand {
    #[command(subcommand)]
    command: hew_pkg::cli::PkgCommand,
}

/// Only the parent tests set the marker. The command reads the process
/// environment and current directory and exits, so it runs out of process.
#[test]
fn publish_command_child() {
    let Ok(arguments) = std::env::var(CHILD_ARGS) else {
        return;
    };
    let arguments: Vec<String> = serde_json::from_str(&arguments).unwrap();
    let command = FixtureCommand::parse_from(std::iter::once("hew".to_owned()).chain(arguments));
    hew_pkg::cli::dispatch(
        &command.command,
        &hew_pkg::native::embedded_rustc_identity().host,
        |_| Ok(()),
    );
}

fn hew_publish(mock: &MockRegistry, project: &Path, hew_home: &Path, arguments: &[&str]) -> Output {
    let arguments: Vec<&str> = std::iter::once("publish")
        .chain(arguments.iter().copied())
        .collect();
    hew_command(mock, project, hew_home, &arguments)
}

/// Run `hew <arguments>` in `project` against `mock`, out of process.
fn hew_command(mock: &MockRegistry, project: &Path, hew_home: &Path, arguments: &[&str]) -> Output {
    Command::new(std::env::current_exe().unwrap())
        .args(["--exact", "publish_command_child", "--nocapture"])
        .env(CHILD_ARGS, serde_json::to_string(&arguments).unwrap())
        .current_dir(project)
        .env("HOME", hew_home)
        .env("HEW_HOME", hew_home)
        .env("HEW_REGISTRY", mock.api())
        .env_remove("ACTIONS_ID_TOKEN_REQUEST_URL")
        .env_remove("ACTIONS_ID_TOKEN_REQUEST_TOKEN")
        .env_remove("USERPROFILE")
        .env_remove("HTTP_PROXY")
        .env_remove("HTTPS_PROXY")
        .env_remove("ALL_PROXY")
        .env_remove("http_proxy")
        .env_remove("https_proxy")
        .env_remove("all_proxy")
        .env("NO_PROXY", "127.0.0.1,localhost")
        .output()
        .unwrap()
}

#[test]
fn dry_run_command_needs_no_key_or_credentials() {
    let mock = MockRegistry::start();
    let project = tempfile::tempdir().unwrap();
    write_package(project.path(), 1);
    let hew_home = tempfile::tempdir().unwrap();

    let output = hew_publish(&mock, project.path(), hew_home.path(), &["--dry-run"]);

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(output.status.success(), "{output:?}");
    assert!(
        stdout.contains("Would publish acme.widget@1.0.0"),
        "{stdout}"
    );
    assert!(!stdout.contains("Signature:"), "{stdout}");
    let log = mock.log();
    assert!(
        !log.is_empty() && log.iter().all(|line| line.starts_with("GET ")),
        "{log:?}"
    );
}

#[test]
fn command_without_credentials_is_not_logged_in_and_uploads_nothing() {
    let mock = MockRegistry::start();
    let project = tempfile::tempdir().unwrap();
    write_package(project.path(), 1);
    let hew_home = tempfile::tempdir().unwrap();

    let output = hew_publish(&mock, project.path(), hew_home.path(), &[]);

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_eq!(output.status.code(), Some(1), "{output:?}");
    assert!(stderr.contains("hew publish: not logged in"), "{stderr}");
    assert_eq!(mock.puts(), 0, "{:?}", mock.log());
}

#[test]
fn command_skips_an_identical_version_without_credentials() {
    let mock = MockRegistry::start();
    let project = tempfile::tempdir().unwrap();
    write_package(project.path(), 1);
    let package = publish::prepare(project.path(), &|_| Ok(())).unwrap();
    mock.seed(&package.archive.data, &KeyPair::generate());
    let hew_home = tempfile::tempdir().unwrap();

    let output = hew_publish(&mock, project.path(), hew_home.path(), &[]);

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(output.status.success(), "{output:?}");
    assert!(
        stdout.contains("Skipped acme.widget@1.0.0: already published, identical"),
        "{stdout}"
    );
    assert_eq!(mock.puts(), 0);
}

#[test]
fn command_publishes_verifies_and_writes_no_local_copy() {
    let mock = MockRegistry::start();
    let project = tempfile::tempdir().unwrap();
    write_package(project.path(), 1);
    let hew_home = tempfile::tempdir().unwrap();
    let publisher = KeyPair::generate();
    hew_pkg::signing::save_keypair(&hew_home.path().join("keys"), &publisher).unwrap();
    fs::write(
        hew_home.path().join("credentials.toml"),
        "[registry]\ntoken = \"token\"\ngithub_user = \"publisher\"\n",
    )
    .unwrap();
    mock.register_key(&publisher);

    let output = hew_publish(&mock, project.path(), hew_home.path(), &[]);

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(output.status.success(), "{output:?}");
    assert!(
        stdout.contains("Published acme.widget@1.0.0 to registry"),
        "{stdout}"
    );
    assert!(stdout.contains("Verified: stored record"), "{stdout}");
    assert_eq!(mock.puts(), 1);
    assert!(
        !hew_home.path().join("packages").exists(),
        "a remote publish must not write a local copy"
    );
}

// ── Names and compiler ranges ───────────────────────────────────────────────

#[test]
fn unimportable_name_is_refused_and_importable_names_pack() {
    let dir = tempfile::tempdir().unwrap();
    write_named_package(
        dir.path(),
        "acme.3d",
        VERSION,
        "",
        "pub fn f() -> i64 { 1 }\n",
    );
    let error = publish::prepare(dir.path(), &|_| Ok(())).unwrap_err();
    assert!(
        error
            .to_string()
            .contains("segment `3d` cannot be written in an import path"),
        "{error}"
    );
    // The parser, the authority behind the refusal, rejects the import too.
    assert!(!hew_parser::parse("import acme.3d;").errors.is_empty());

    for name in ["acme.three_d", "acme.type"] {
        let dir = tempfile::tempdir().unwrap();
        write_named_package(dir.path(), name, VERSION, "", "pub fn f() -> i64 { 1 }\n");
        publish::prepare(dir.path(), &|_| Ok(())).unwrap_or_else(|e| panic!("{name}: {e}"));
        assert!(hew_parser::parse(&format!("import {name};"))
            .errors
            .is_empty());
    }
}

#[test]
fn compiler_range_excluding_this_compiler_is_refused() {
    let dir = tempfile::tempdir().unwrap();
    write_named_package(
        dir.path(),
        NAME,
        VERSION,
        "hew = \">=9.0.0\"\n",
        "pub fn f() -> i64 { 1 }\n",
    );
    let error = publish::prepare(dir.path(), &|_| Ok(())).unwrap_err();
    assert!(
        error
            .to_string()
            .starts_with("E_PKG_COMPILER_RANGE: acme.widget requires hew `>=9.0.0`"),
        "{error}"
    );

    write_named_package(
        dir.path(),
        NAME,
        VERSION,
        "hew = \"0.6 or so\"\n",
        "pub fn f() -> i64 { 1 }\n",
    );
    let error = publish::prepare(dir.path(), &|_| Ok(())).unwrap_err();
    assert!(
        error.to_string().contains("is not a semver range"),
        "{error}"
    );

    // Negative control: a range that admits this compiler packs.
    write_named_package(
        dir.path(),
        NAME,
        VERSION,
        "hew = \"<9.0.0\"\n",
        "pub fn f() -> i64 { 1 }\n",
    );
    publish::prepare(dir.path(), &|_| Ok(())).unwrap();
}

// ── Workspaces ──────────────────────────────────────────────────────────────

/// Write a workspace at `root` listing `members` (directory, name, version,
/// dependency lines, root module source).
fn write_workspace(root: &Path, members: &[(&str, &str, &str, &str, &str)]) {
    let list = members
        .iter()
        .map(|(dir, ..)| format!("\"{dir}\""))
        .collect::<Vec<_>>()
        .join(", ");
    fs::write(
        root.join("hew.toml"),
        format!("[workspace]\nmembers = [{list}]\n"),
    )
    .unwrap();
    for (dir, name, version, deps, source) in members {
        write_named_package(
            &root.join(dir),
            name,
            version,
            &format!("\n[dependencies]\n{deps}"),
            source,
        );
    }
}

#[test]
fn install_in_a_member_links_siblings_and_locks_relative_paths() {
    let mock = MockRegistry::start();
    let root = tempfile::tempdir().unwrap();
    write_workspace(
        root.path(),
        &[
            (
                "libs/a",
                "acme.a",
                "1.0.0",
                "",
                "pub fn one() -> i64 { 1 }\n",
            ),
            (
                "apps/b",
                "acme.b",
                "1.0.0",
                "\"acme.a\" = \"1.0.0\"\n",
                "import acme.a;\npub fn two() -> i64 { a.one() + 1 }\n",
            ),
        ],
    );
    let member = root.path().join("apps/b");
    let hew_home = tempfile::tempdir().unwrap();

    let output = hew_command(&mock, &member, hew_home.path(), &["install"]);
    assert!(output.status.success(), "{output:?}");
    assert!(member.join(".hew/packages/acme/a/a.hew").is_file());
    let lock = fs::read_to_string(member.join("hew.lock")).unwrap();
    assert!(lock.contains("path = \"../../libs/a\""), "{lock}");
    assert!(!lock.contains(&root.path().display().to_string()), "{lock}");

    let output = hew_command(&mock, &member, hew_home.path(), &["install", "--locked"]);
    assert!(output.status.success(), "{output:?}");
    assert!(mock.log().is_empty(), "{:?}", mock.log());

    // Negative control: once `libs/a` is not a member, `acme.a` resolves
    // against a registry again.
    fs::write(
        root.path().join("hew.toml"),
        "[workspace]\nmembers = [\"apps/b\"]\n",
    )
    .unwrap();
    let output = hew_command(&mock, &member, hew_home.path(), &["install"]);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(!output.status.success(), "{output:?}");
    assert!(
        stderr.contains("registry") && stderr.contains("acme.a"),
        "{stderr}"
    );
}

/// A home with a registered signing key and a token, ready to upload.
fn publisher_home(mock: &MockRegistry) -> (TempDir, KeyPair) {
    let hew_home = tempfile::tempdir().unwrap();
    let publisher = KeyPair::generate();
    hew_pkg::signing::save_keypair(&hew_home.path().join("keys"), &publisher).unwrap();
    fs::write(
        hew_home.path().join("credentials.toml"),
        "[registry]\ntoken = \"token\"\ngithub_user = \"publisher\"\n",
    )
    .unwrap();
    mock.register_key(&publisher);
    (hew_home, publisher)
}

const PUB_FN: &str = "pub fn one() -> i64 { 1 }\n";

/// `a`; `b` depending on `b_dep`; `c`.
fn abc_workspace(root: &Path, b_dep: &str) {
    write_workspace(
        root,
        &[
            ("a", "acme.a", "1.0.0", "", PUB_FN),
            (
                "b",
                "acme.b",
                "1.0.0",
                &format!("\"{b_dep}\" = \"1.0.0\"\n"),
                PUB_FN,
            ),
            ("c", "acme.c", "1.0.0", "", PUB_FN),
        ],
    );
}

#[test]
fn workspace_uploads_in_dependency_order_and_verifies_each_record() {
    let mock = MockRegistry::start();
    let root = tempfile::tempdir().unwrap();
    abc_workspace(root.path(), "acme.a");
    let (hew_home, publisher) = publisher_home(&mock);

    let output = hew_publish(&mock, root.path(), hew_home.path(), &["--workspace"]);

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(output.status.success(), "{output:?}");
    assert_eq!(
        mock.put_paths(),
        [
            "/api/v1/packages/acme.a/1.0.0",
            "/api/v1/packages/acme.b/1.0.0",
            "/api/v1/packages/acme.c/1.0.0",
        ]
    );
    for name in ["acme.a", "acme.b", "acme.c"] {
        let record = mock.stored_package(name, "1.0.0").unwrap();
        let archive = mock.stored_archive(name, "1.0.0").unwrap();
        assert_eq!(
            record["cksum"],
            hew_pkg::tarball::checksum_bytes(&archive).as_str()
        );
        assert_eq!(record["key_fp"], publisher.fingerprint().as_str());
        assert!(
            stdout.contains(&format!("Published {name}@1.0.0 to registry")),
            "{stdout}"
        );
    }
    assert!(stdout.contains("published 3, skipped 0"), "{stdout}");

    // Negative control: `b` depending on `c` moves it after `c`.
    let mock = MockRegistry::start();
    let root = tempfile::tempdir().unwrap();
    abc_workspace(root.path(), "acme.c");
    let (hew_home, _) = publisher_home(&mock);
    let output = hew_publish(&mock, root.path(), hew_home.path(), &["--workspace"]);
    assert!(output.status.success(), "{output:?}");
    assert_eq!(
        mock.put_paths(),
        [
            "/api/v1/packages/acme.a/1.0.0",
            "/api/v1/packages/acme.c/1.0.0",
            "/api/v1/packages/acme.b/1.0.0",
        ]
    );
}

#[test]
fn invalid_member_fails_before_any_request() {
    let mock = MockRegistry::start();
    let root = tempfile::tempdir().unwrap();
    abc_workspace(root.path(), "acme.a");
    let manifest = root.path().join("c/hew.toml");
    let text = fs::read_to_string(&manifest).unwrap();
    fs::write(
        &manifest,
        text.replace("description = \"d\"", "description = \"\""),
    )
    .unwrap();
    let hew_home = tempfile::tempdir().unwrap();

    let output = hew_publish(
        &mock,
        root.path(),
        hew_home.path(),
        &["--workspace", "--dry-run"],
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(!output.status.success(), "{output:?}");
    assert!(
        stderr.contains("acme.c: missing required fields"),
        "{stderr}"
    );
    assert!(mock.log().is_empty(), "{:?}", mock.log());

    // Negative control: a valid description reaches the registry.
    fs::write(&manifest, text).unwrap();
    let output = hew_publish(
        &mock,
        root.path(),
        hew_home.path(),
        &["--workspace", "--dry-run"],
    );
    assert!(output.status.success(), "{output:?}");
    assert!(!mock.log().is_empty());
}

#[test]
fn member_cycle_fails_before_any_request() {
    let mock = MockRegistry::start();
    let root = tempfile::tempdir().unwrap();
    write_workspace(
        root.path(),
        &[
            ("a", "acme.a", "1.0.0", "\"acme.b\" = \"1.0.0\"\n", PUB_FN),
            ("b", "acme.b", "1.0.0", "\"acme.a\" = \"1.0.0\"\n", PUB_FN),
        ],
    );
    let hew_home = tempfile::tempdir().unwrap();

    let output = hew_publish(
        &mock,
        root.path(),
        hew_home.path(),
        &["--workspace", "--dry-run"],
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(!output.status.success(), "{output:?}");
    assert!(
        stderr.contains("E_WORKSPACE_CYCLE") && stderr.contains("acme.a -> acme.b -> acme.a"),
        "{stderr}"
    );
    assert!(mock.log().is_empty(), "{:?}", mock.log());
}

#[test]
fn published_members_are_skipped_and_drift_uploads_nothing() {
    let mock = MockRegistry::start();
    let root = tempfile::tempdir().unwrap();
    abc_workspace(root.path(), "acme.a");
    let a = publish::prepare(&root.path().join("a"), &|_| Ok(())).unwrap();
    mock.seed_package("acme.a", "1.0.0", &a.archive.data, &KeyPair::generate());
    let (hew_home, _) = publisher_home(&mock);

    let output = hew_publish(&mock, root.path(), hew_home.path(), &["--workspace"]);

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(output.status.success(), "{output:?}");
    assert!(
        stdout.contains("Skipped acme.a@1.0.0: already published, identical"),
        "{stdout}"
    );
    assert!(stdout.contains("published 2, skipped 1"), "{stdout}");
    assert_eq!(
        mock.put_paths(),
        [
            "/api/v1/packages/acme.b/1.0.0",
            "/api/v1/packages/acme.c/1.0.0"
        ]
    );

    // Negative control: different content under a published version, in two
    // members, is drift for both, and nothing uploads.
    let mock = MockRegistry::start();
    let (_other, other) = packed(7);
    mock.seed_package("acme.a", "1.0.0", &other.archive.data, &KeyPair::generate());
    mock.seed_package("acme.c", "1.0.0", &other.archive.data, &KeyPair::generate());
    let (hew_home, _) = publisher_home(&mock);
    let output = hew_publish(&mock, root.path(), hew_home.path(), &["--workspace"]);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(!output.status.success(), "{output:?}");
    assert!(
        stderr.contains("E_PUBLISH_VERSION_DRIFT: acme.a@1.0.0"),
        "{stderr}"
    );
    assert!(
        stderr.contains("E_PUBLISH_VERSION_DRIFT: acme.c@1.0.0"),
        "{stderr}"
    );
    assert_eq!(mock.puts(), 0, "{:?}", mock.log());
}

#[test]
fn workspace_upload_failure_preserves_the_reported_partial_success() {
    let mock = MockRegistry::start();
    let root = tempfile::tempdir().unwrap();
    abc_workspace(root.path(), "acme.a");
    mock.state.lock().unwrap().fail_after_puts = Some(1);
    let (hew_home, _) = publisher_home(&mock);

    let output = hew_publish(&mock, root.path(), hew_home.path(), &["--workspace"]);
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(!output.status.success(), "{output:?}");
    assert!(stdout.contains("published 1, skipped 0"), "{stdout}");
    assert!(stderr.contains("remote publish failed:"), "{stderr}");
    assert!(!stderr.contains("Nothing was published"), "{stderr}");
    assert_eq!(mock.state.lock().unwrap().accepted_puts, 1);
    assert_eq!(
        mock.put_paths(),
        [
            "/api/v1/packages/acme.a/1.0.0",
            "/api/v1/packages/acme.b/1.0.0"
        ]
    );
}

#[test]
fn workspace_dry_run_needs_no_credentials_and_only_reads() {
    let mock = MockRegistry::start();
    let root = tempfile::tempdir().unwrap();
    abc_workspace(root.path(), "acme.a");
    let hew_home = tempfile::tempdir().unwrap();

    let output = hew_publish(
        &mock,
        root.path(),
        hew_home.path(),
        &["--workspace", "--dry-run"],
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(output.status.success(), "{output:?}");
    assert!(stdout.contains("3 packages"), "{stdout}");
    for name in ["acme.a", "acme.b", "acme.c"] {
        assert!(
            stdout.contains(&format!("Would publish {name}@1.0.0")),
            "{stdout}"
        );
    }
    assert!(stdout.contains("would publish 3, skipped 0"), "{stdout}");
    let log = mock.log();
    assert!(log.iter().all(|line| line.starts_with("GET ")), "{log:?}");
    assert_eq!(fs::read_dir(hew_home.path()).unwrap().count(), 0);

    // Negative control: a real run needs credentials and uploads nothing.
    let output = hew_publish(&mock, root.path(), hew_home.path(), &["--workspace"]);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(!output.status.success(), "{output:?}");
    assert!(stderr.contains("not logged in"), "{stderr}");
    assert_eq!(mock.puts(), 0);
}

#[test]
fn sibling_requirement_mismatch_fails_before_any_request() {
    let mock = MockRegistry::start();
    let root = tempfile::tempdir().unwrap();
    let members = |requirement: &str| {
        write_workspace(
            root.path(),
            &[
                (
                    "a",
                    "acme.a",
                    "0.4.0",
                    "",
                    "pub fn unpublished_helper() -> i64 { 4 }\n",
                ),
                (
                    "b",
                    "acme.b",
                    "0.4.0",
                    &format!("\"acme.a\" = \"{requirement}\"\n"),
                    "import acme.a;\npub fn two() -> i64 { a.unpublished_helper() }\n",
                ),
            ],
        );
    };
    members("0.3.0");
    let hew_home = tempfile::tempdir().unwrap();

    let output = hew_publish(
        &mock,
        root.path(),
        hew_home.path(),
        &["--workspace", "--dry-run"],
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(!output.status.success(), "{output:?}");
    assert!(
        stderr.contains(
            "E_WORKSPACE_REQUIREMENT: acme.b requires acme.a `0.3.0`, but the workspace member \
             acme.a is 0.4.0"
        ),
        "{stderr}"
    );
    assert!(mock.log().is_empty(), "{:?}", mock.log());

    // Negative control: a matching requirement checks `b` against `a`'s
    // directory, where the helper the registry has never seen exists.
    members("0.4.0");
    let output = hew_publish(
        &mock,
        root.path(),
        hew_home.path(),
        &["--workspace", "--dry-run"],
    );
    assert!(output.status.success(), "{output:?}");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("Would publish acme.b@0.4.0"), "{stdout}");
}

#[test]
fn listed_member_without_manifest_fails_and_unlisted_packages_are_ignored() {
    let mock = MockRegistry::start();
    let root = tempfile::tempdir().unwrap();
    abc_workspace(root.path(), "acme.a");
    // Neither a cached package nor a nested fixture is a member.
    write_named_package(
        &root.path().join(".hew/packages/x"),
        "acme.x",
        "1.0.0",
        "",
        PUB_FN,
    );
    write_named_package(
        &root.path().join("tools/fixture"),
        "acme.fixture",
        "1.0.0",
        "",
        PUB_FN,
    );
    let hew_home = tempfile::tempdir().unwrap();

    let output = hew_publish(
        &mock,
        root.path(),
        hew_home.path(),
        &["--workspace", "--dry-run"],
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(output.status.success(), "{output:?}");
    assert!(stdout.contains("3 packages"), "{stdout}");
    assert!(
        !stdout.contains("acme.x") && !stdout.contains("acme.fixture"),
        "{stdout}"
    );

    let text = fs::read_to_string(root.path().join("hew.toml")).unwrap();
    fs::create_dir_all(root.path().join("empty")).unwrap();
    fs::write(
        root.path().join("hew.toml"),
        text.replace("\"c\"]", "\"c\", \"empty\"]"),
    )
    .unwrap();
    let before = mock.log().len();
    let output = hew_publish(
        &mock,
        root.path(),
        hew_home.path(),
        &["--workspace", "--dry-run"],
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(!output.status.success(), "{output:?}");
    assert!(stderr.contains("E_WORKSPACE_MEMBER: `empty`"), "{stderr}");
    assert_eq!(mock.log().len(), before);
}

#[test]
fn workspace_archives_do_not_depend_on_the_checkout_or_change_it() {
    let checksums = |root: &Path| {
        let mock = MockRegistry::start();
        abc_workspace(root, "acme.a");
        let hew_home = tempfile::tempdir().unwrap();
        let before = tree_listing(root);
        let output = hew_publish(&mock, root, hew_home.path(), &["--workspace", "--dry-run"]);
        assert!(output.status.success(), "{output:?}");
        assert_eq!(tree_listing(root), before, "dry run changed the checkout");
        String::from_utf8_lossy(&output.stdout)
            .lines()
            .filter(|line| line.starts_with("Checksum: "))
            .map(ToOwned::to_owned)
            .collect::<Vec<_>>()
    };
    let first = tempfile::tempdir().unwrap();
    let second = tempfile::tempdir().unwrap();
    let nested = second.path().join("deeper/checkout");
    fs::create_dir_all(&nested).unwrap();
    let first = checksums(first.path());
    assert_eq!(first.len(), 3);
    assert_eq!(first, checksums(&nested));
}

/// Every file below `root` except `.hew/`, sorted.
fn tree_listing(root: &Path) -> Vec<String> {
    fn walk(dir: &Path, root: &Path, out: &mut Vec<String>) {
        for entry in fs::read_dir(dir).unwrap() {
            let path = entry.unwrap().path();
            if path.file_name().is_some_and(|name| name == ".hew") {
                continue;
            }
            if path.is_dir() {
                walk(&path, root, out);
            } else {
                out.push(path.strip_prefix(root).unwrap().display().to_string());
            }
        }
    }
    let mut out = Vec::new();
    walk(root, root, &mut out);
    out.sort();
    out
}

#[test]
fn package_command_at_a_workspace_root_points_at_members() {
    let mock = MockRegistry::start();
    let root = tempfile::tempdir().unwrap();
    abc_workspace(root.path(), "acme.a");
    let hew_home = tempfile::tempdir().unwrap();

    let output = hew_publish(&mock, root.path(), hew_home.path(), &["--dry-run"]);

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(!output.status.success(), "{output:?}");
    assert!(
        stderr.contains("run in a member or use `--workspace`"),
        "{stderr}"
    );
}

#[test]
fn single_member_publish_resolves_siblings_locally() {
    let mock = MockRegistry::start();
    let root = tempfile::tempdir().unwrap();
    write_workspace(
        root.path(),
        &[
            ("a", "acme.a", "1.0.0", "", "pub fn fresh() -> i64 { 1 }\n"),
            (
                "b",
                "acme.b",
                "1.0.0",
                "\"acme.a\" = \"1.0.0\"\n",
                "import acme.a;\npub fn two() -> i64 { a.fresh() }\n",
            ),
        ],
    );
    let hew_home = tempfile::tempdir().unwrap();

    let output = hew_publish(
        &mock,
        &root.path().join("b"),
        hew_home.path(),
        &["--dry-run"],
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(output.status.success(), "{output:?}");
    assert!(stdout.contains("Would publish acme.b@1.0.0"), "{stdout}");
    assert!(!stdout.contains("Would publish acme.a"), "{stdout}");
    assert_eq!(mock.log(), ["GET /api/v1/packages/acme.b/1.0.0"]);
}

#[test]
fn publish_checks_the_archive_membership_of_workspace_dependencies() {
    for whole_workspace in [false, true] {
        let mock = MockRegistry::start();
        let root = tempfile::tempdir().unwrap();
        write_workspace(
            root.path(),
            &[
                ("a", "acme.a", "1.0.0", "", "pub fn one() -> i64 { 1 }\n"),
                (
                    "b",
                    "acme.b",
                    "1.0.0",
                    "\"acme.a\" = \"1.0.0\"\n",
                    "import acme.a.extra;\npub fn two() -> i64 { extra.value() }\n",
                ),
            ],
        );
        fs::write(
            root.path().join("a/extra.hew"),
            "pub fn value() -> i64 { 2 }\n",
        )
        .unwrap();
        let hew_home = tempfile::tempdir().unwrap();
        let cwd = if whole_workspace {
            root.path().to_path_buf()
        } else {
            root.path().join("b")
        };
        let flags: &[&str] = if whole_workspace {
            &["--workspace", "--dry-run"]
        } else {
            &["--dry-run"]
        };
        let included = hew_publish(&mock, &cwd, hew_home.path(), flags);
        assert!(included.status.success(), "{included:?}");
        let requests_before = mock.log();
        let manifest = root.path().join("a/hew.toml");
        let contents = fs::read_to_string(&manifest).unwrap();
        fs::write(
            &manifest,
            contents.replace("[package]", "[package]\nexclude = [\"extra.hew\"]"),
        )
        .unwrap();

        let excluded = hew_publish(&mock, &cwd, hew_home.path(), flags);
        let stderr = String::from_utf8_lossy(&excluded.stderr);
        assert!(!excluded.status.success(), "{excluded:?}");
        assert!(stderr.contains("E_PUBLISH_MISSING_INPUT"), "{stderr}");
        assert!(
            stderr.contains("extra.hew") && stderr.contains("acme.a"),
            "{stderr}"
        );
        assert_eq!(
            mock.log(),
            requests_before,
            "invalid archives must fail before querying the registry"
        );
    }
}

// ── Trusted publishing ──────────────────────────────────────────────────────

/// `hew publish --workspace --auth github-actions` with the runner's OIDC
/// endpoint on `mock` and an empty `HEW_HOME`.
fn trusted_publish(mock: &MockRegistry, root: &Path, hew_home: &Path) -> Output {
    let arguments = ["publish", "--workspace", "--auth", "github-actions"];
    Command::new(std::env::current_exe().unwrap())
        .args(["--exact", "publish_command_child", "--nocapture"])
        .env(CHILD_ARGS, serde_json::to_string(&arguments).unwrap())
        .current_dir(root)
        .env("HOME", hew_home)
        .env("HEW_HOME", hew_home)
        .env("HEW_REGISTRY", mock.api())
        .env(
            "ACTIONS_ID_TOKEN_REQUEST_URL",
            format!("{}/oidc?api-version=2.0", mock.origin),
        )
        .env("ACTIONS_ID_TOKEN_REQUEST_TOKEN", "runner-token")
        .env_remove("USERPROFILE")
        .env_remove("HTTP_PROXY")
        .env_remove("HTTPS_PROXY")
        .env_remove("ALL_PROXY")
        .env_remove("http_proxy")
        .env_remove("https_proxy")
        .env_remove("all_proxy")
        .env("NO_PROXY", "127.0.0.1,localhost")
        .output()
        .unwrap()
}

fn two_member_workspace(root: &Path) {
    write_workspace(
        root,
        &[
            ("a", "acme.a", "1.0.0", "", PUB_FN),
            ("b", "acme.b", "1.0.0", "\"acme.a\" = \"1.0.0\"\n", PUB_FN),
        ],
    );
}

#[test]
fn trusted_publishing_exchanges_once_and_writes_no_credentials() {
    let mock = MockRegistry::start();
    let root = tempfile::tempdir().unwrap();
    two_member_workspace(root.path());
    let hew_home = tempfile::tempdir().unwrap();

    let output = trusted_publish(&mock, root.path(), hew_home.path());

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(output.status.success(), "{output:?}");
    assert!(
        stdout.contains("authenticated as hew-ecosystem through GitHub Actions"),
        "{stdout}"
    );
    let state = mock.state.lock().unwrap();
    assert_eq!(state.oidc_audiences, std::slice::from_ref(&mock.origin));
    assert_eq!(state.exchanges.len(), 1);
    let exchanged = base64::engine::general_purpose::STANDARD
        .decode(&state.exchanges[0])
        .unwrap();
    drop(state);
    let exchanged_fingerprint = hew_pkg::signing::compute_fingerprint(&exchanged);
    assert_eq!(mock.puts(), 2);
    for name in ["acme.a", "acme.b"] {
        let record = mock.stored_package(name, "1.0.0").unwrap();
        assert_eq!(record["key_fp"], exchanged_fingerprint.as_str());
        assert!(
            stdout.contains(&format!("Published {name}@1.0.0 to registry")),
            "{stdout}"
        );
    }
    assert!(!hew_home.path().join("credentials.toml").exists());
    assert!(!hew_home.path().join("keys").exists());
}

#[test]
fn trusted_publishing_refuses_a_grant_for_another_key() {
    let mock = MockRegistry::start();
    mock.state.lock().unwrap().grant_wrong_key = true;
    let root = tempfile::tempdir().unwrap();
    two_member_workspace(root.path());
    let hew_home = tempfile::tempdir().unwrap();

    let output = trusted_publish(&mock, root.path(), hew_home.path());

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(!output.status.success(), "{output:?}");
    assert!(stderr.contains("E_AUTH_GRANT_KEY"), "{stderr}");
    assert_eq!(mock.puts(), 0);
}

#[test]
fn trusted_publishing_re_exchanges_for_the_same_key_after_401() {
    let mock = MockRegistry::start();
    mock.state.lock().unwrap().expire_grants_after_puts = Some(1);
    let root = tempfile::tempdir().unwrap();
    two_member_workspace(root.path());
    let hew_home = tempfile::tempdir().unwrap();

    let output = trusted_publish(&mock, root.path(), hew_home.path());

    assert!(output.status.success(), "{output:?}");
    let state = mock.state.lock().unwrap();
    assert_eq!(
        state.oidc_audiences.len(),
        2,
        "a fresh OIDC token per exchange"
    );
    assert_eq!(state.exchanges.len(), 2);
    assert_eq!(
        state.exchanges[0], state.exchanges[1],
        "the same key both times"
    );
    drop(state);
    assert_eq!(
        mock.put_paths(),
        [
            "/api/v1/packages/acme.a/1.0.0",
            "/api/v1/packages/acme.b/1.0.0",
            "/api/v1/packages/acme.b/1.0.0",
        ]
    );
    assert!(mock.stored_package("acme.b", "1.0.0").is_some());
}

#[test]
fn trusted_publishing_outside_a_workflow_names_the_permission() {
    let mock = MockRegistry::start();
    let root = tempfile::tempdir().unwrap();
    two_member_workspace(root.path());
    let hew_home = tempfile::tempdir().unwrap();

    let output = hew_publish(
        &mock,
        root.path(),
        hew_home.path(),
        &["--workspace", "--auth", "github-actions"],
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(!output.status.success(), "{output:?}");
    assert!(stderr.contains("permissions: id-token: write"), "{stderr}");
    assert_eq!(mock.puts(), 0);
}

// ── Native crates ───────────────────────────────────────────────────────────

#[test]
fn unlinkable_native_crate_fails_validation_before_any_request() {
    let mock = MockRegistry::start();
    let root = tempfile::tempdir().unwrap();
    abc_workspace(root.path(), "acme.a");
    let member = root.path().join("c");
    let manifest = fs::read_to_string(member.join("hew.toml")).unwrap();
    fs::write(
        member.join("hew.toml"),
        format!("{manifest}\n[native]\ncrate = \"native\"\n"),
    )
    .unwrap();
    let cargo_toml = |crate_type: &str| {
        fs::create_dir_all(member.join("native/src")).unwrap();
        fs::write(member.join("native/src/lib.rs"), "").unwrap();
        fs::write(
            member.join("native/Cargo.toml"),
            format!(
                "[workspace]\n\n[package]\nname = \"c-native\"\nversion = \"0.1.0\"\n\
                 edition = \"2021\"\n\n[lib]\ncrate-type = [\"{crate_type}\"]\n"
            ),
        )
        .unwrap();
    };
    cargo_toml("rlib");
    let hew_home = tempfile::tempdir().unwrap();

    let output = hew_publish(
        &mock,
        root.path(),
        hew_home.path(),
        &["--workspace", "--dry-run"],
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(!output.status.success(), "{output:?}");
    assert!(stderr.contains("E_INVALID_NATIVE"), "{stderr}");
    assert!(mock.log().is_empty(), "{:?}", mock.log());
    assert!(
        !member.join("native/target").exists(),
        "publish never builds native code"
    );

    // Negative control: a staticlib crate passes validation without building.
    cargo_toml("staticlib");
    let output = hew_publish(
        &mock,
        root.path(),
        hew_home.path(),
        &["--workspace", "--dry-run"],
    );
    assert!(output.status.success(), "{output:?}");
    assert!(
        !member.join("native/target").exists(),
        "publish never builds native code"
    );
}

// ── Member lockfiles ────────────────────────────────────────────────────────

#[test]
fn stale_member_lock_is_ignored_and_left_out_of_the_archive() {
    let mock = MockRegistry::start();
    let root = tempfile::tempdir().unwrap();
    write_workspace(
        root.path(),
        &[
            ("a", "acme.a", "0.4.0", "", PUB_FN),
            (
                "b",
                "acme.b",
                "0.4.0",
                "\"acme.a\" = \"0.4.0\"\n",
                "import acme.a;\npub fn two() -> i64 { a.one() + 1 }\n",
            ),
        ],
    );
    let hew_home = tempfile::tempdir().unwrap();
    let dry_run_checksums = || {
        let output = hew_publish(
            &mock,
            root.path(),
            hew_home.path(),
            &["--workspace", "--dry-run"],
        );
        assert!(output.status.success(), "{output:?}");
        String::from_utf8_lossy(&output.stdout)
            .lines()
            .filter(|line| line.starts_with("Checksum: "))
            .map(ToOwned::to_owned)
            .collect::<Vec<_>>()
    };
    let without_lock = dry_run_checksums();

    // The lock an earlier install left behind pins the sibling's old version.
    let member = root.path().join("b");
    fs::write(
        member.join("hew.lock"),
        "[[package]]\nname = \"acme.a\"\nrequirement = \"0.3.0\"\nversion = \"0.3.0\"\n\
         source = \"path\"\npath = \"../a\"\n",
    )
    .unwrap();

    assert_eq!(
        dry_run_checksums(),
        without_lock,
        "the lock must not reach the archive"
    );
    let package = publish::check_and_pack(
        &member,
        hew_pkg::manifest::parse_manifest(&member.join("hew.toml")).unwrap(),
        &|_| Ok(()),
    )
    .unwrap();
    assert!(
        !package
            .archive
            .files
            .iter()
            .any(|file| file == Path::new("hew.lock")),
        "{:?}",
        package.archive.files
    );

    // Negative controls: the frontend honours the stale lock when not told to
    // ignore it, and packing without publish's exclusion carries it.
    let entry = member.join("b.hew");
    let failure = hew_compile::check_file(
        &entry.to_string_lossy(),
        &hew_compile::FrontendOptions::default(),
    )
    .unwrap_err();
    assert!(
        failure.message.contains("does not match hew.lock"),
        "{}",
        failure.message
    );
    let raw = hew_pkg::tarball::pack(&member, &[], &[]).unwrap();
    assert!(raw.files.iter().any(|file| file == Path::new("hew.lock")));
}

#[test]
fn prepared_plan_retains_the_stored_source_archive_identity() {
    let mock = MockRegistry::start();
    let (_dir, package) = packed(1);
    let stored = reencoded(&package.archive.data);
    let stored_checksum = hew_pkg::tarball::checksum_bytes(&stored);
    mock.seed(&stored, &KeyPair::generate());
    let plan = publish::prepare_remote(&mock.client(), std::slice::from_ref(&package)).unwrap();
    assert_eq!(plan.sources()[0].source_checksum(), stored_checksum);
    assert_ne!(
        plan.sources()[0].source_checksum(),
        package.archive.checksum
    );
    assert_eq!(mock.puts(), 0);
}
