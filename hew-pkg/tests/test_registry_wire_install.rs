//! Signed, loopback-only command tests for registry wire names.
//!
//! The subprocess exercises the same install dispatcher as `hew install` without
//! mutating the test runner's environment. Set `HEW_BIN` to use a freshly built
//! compiler instead; `HEW_WIRE_TEST_NATIVE` additionally checks/runs the consumer.

use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::{Arc, Mutex};
use std::thread::{self, JoinHandle};

use clap::Parser;
use hew_pkg::client::RegistryClient;
use hew_pkg::config::{self, WireNames};
use hew_pkg::registry::Registry;
use hew_pkg::signing::{self, KeyPair};
use hew_pkg::tarball;
use serde_json::{json, Value};
use tempfile::TempDir;
use tiny_http::{Header, Response, Server};

const VERSION: &str = "1.2.3";
const PUBLISHED_AT: &str = "2026-01-02T03:04:05Z";
const CHILD_ARGS: &str = "HEW_REGISTRY_WIRE_TEST_ARGS";

#[derive(Parser)]
struct FixtureCommand {
    #[command(subcommand)]
    command: hew_pkg::cli::PkgCommand,
}

/// Only the parent fixture sets this marker. This child must run out of process:
/// install exits on failure and reads the process environment/current directory.
#[test]
fn registry_wire_command_child() {
    let Ok(arguments) = std::env::var(CHILD_ARGS) else {
        return;
    };
    let arguments: Vec<String> = serde_json::from_str(&arguments).unwrap();
    let command = FixtureCommand::parse_from(std::iter::once("hew".to_owned()).chain(arguments));
    hew_pkg::cli::dispatch(&command.command);
}

#[derive(Clone)]
struct Reply {
    status: u16,
    body: Vec<u8>,
}

struct FixtureServer {
    origin: String,
    server: Arc<Server>,
    routes: Arc<Mutex<BTreeMap<String, Reply>>>,
    requests: Arc<Mutex<Vec<String>>>,
    worker: Option<JoinHandle<()>>,
}

impl FixtureServer {
    fn new() -> Self {
        let server = Arc::new(Server::http("127.0.0.1:0").unwrap());
        let origin = format!("http://{}", server.server_addr());
        let routes = Arc::new(Mutex::new(BTreeMap::<String, Reply>::new()));
        let requests = Arc::new(Mutex::new(Vec::new()));
        let worker_server = Arc::clone(&server);
        let worker_routes = Arc::clone(&routes);
        let worker_requests = Arc::clone(&requests);
        let worker = thread::spawn(move || {
            while let Ok(request) = worker_server.recv() {
                worker_requests
                    .lock()
                    .unwrap()
                    .push(request.url().to_owned());
                let path = request.url().split('?').next().unwrap();
                let reply = worker_routes
                    .lock()
                    .unwrap()
                    .get(path)
                    .cloned()
                    .unwrap_or(Reply {
                        status: 404,
                        body: br#"{"error":"unconfigured fixture route"}"#.to_vec(),
                    });
                let status = if request.method().as_str() == "GET" {
                    reply.status
                } else {
                    405
                };
                let response = Response::from_data(reply.body)
                    .with_status_code(status)
                    .with_header(Header::from_bytes("Content-Type", "application/json").unwrap());
                let _ = request.respond(response);
            }
        });
        Self {
            origin,
            server,
            routes,
            requests,
            worker: Some(worker),
        }
    }

    fn api(&self) -> String {
        format!("{}/api/v1", self.origin)
    }

    fn reply(&self, path: &str, status: u16, body: Vec<u8>) {
        self.routes
            .lock()
            .unwrap()
            .insert(path.to_owned(), Reply { status, body });
    }

    fn json(&self, path: &str, body: &Value) {
        self.reply(path, 200, serde_json::to_vec(body).unwrap());
    }

    fn metadata(&self, name: &str, mode: WireNames, entry: &Value) {
        let name = wire_name(name, mode);
        self.json(
            &format!("/api/v1/packages/{name}"),
            &json!({ "metadata": { "name": name }, "versions": [entry] }),
        );
    }

    fn requests(&self) -> Vec<String> {
        self.requests.lock().unwrap().clone()
    }

    fn clear_requests(&self) {
        self.requests.lock().unwrap().clear();
    }
}

impl Drop for FixtureServer {
    fn drop(&mut self) {
        self.server.unblock();
        self.worker.take().unwrap().join().unwrap();
    }
}

fn wire_name(name: &str, mode: WireNames) -> String {
    match mode {
        WireNames::Dotted => name.to_owned(),
        WireNames::Slash => name.replace('.', "/"),
    }
}

fn encode_path(text: &str) -> String {
    use std::fmt::Write as _;
    let mut encoded = String::new();
    for byte in text.bytes() {
        if byte.is_ascii_alphanumeric() || matches!(byte, b'-' | b'_' | b'.' | b'~') {
            encoded.push(char::from(byte));
        } else {
            write!(&mut encoded, "%{byte:02X}").unwrap();
        }
    }
    encoded
}

struct Signers {
    publisher: KeyPair,
    registry: KeyPair,
}

impl Signers {
    fn new() -> Self {
        Self {
            publisher: KeyPair::generate(),
            registry: KeyPair::generate(),
        }
    }

    fn serve_keys(&self, server: &FixtureServer) {
        server.json(
            &format!(
                "/api/v1/keys/{}",
                encode_path(&self.publisher.fingerprint())
            ),
            &json!({
                "fingerprint": self.publisher.fingerprint(),
                "public_key": self.publisher.public_key_base64(),
                "key_type": "ed25519", "github_user": "fixture", "github_id": 1,
            }),
        );
        server.json("/api/v1/registry-key", &json!({
            "key_id": "fixture-registry", "public_key": self.registry.public_key_base64(), "algorithm": "ed25519",
        }));
    }

    fn countersign(&self, entry: &mut Value, raw_name: &str) {
        let canonical = format!(
            "registry:v1:{raw_name}@{VERSION}:{}:{}:{PUBLISHED_AT}",
            entry["cksum"].as_str().unwrap(),
            entry["sig"].as_str().unwrap()
        );
        entry["registry_sig"] = json!(self.registry.sign(canonical.as_bytes()));
    }
}

struct SignedPackage {
    name: String,
    manifest: String,
    source: String,
    archive: Vec<u8>,
    checksum: String,
    signature: String,
}

impl SignedPackage {
    fn new(signers: &Signers, name: &str, source: &str, manifest_tail: &str) -> Self {
        let directory = tempfile::tempdir().unwrap();
        let manifest = format!("[package]\nname = {name:?}\nversion = {VERSION:?}\nedition = \"2026\"\n{manifest_tail}");
        fs::write(directory.path().join("hew.toml"), &manifest).unwrap();
        fs::write(
            directory
                .path()
                .join(format!("{}.hew", name.rsplit('.').next().unwrap())),
            source,
        )
        .unwrap();
        let packed = tarball::pack(directory.path(), &[], &[]).unwrap();
        let signature = signers.publisher.sign(packed.checksum.as_bytes());
        signing::verify(
            packed.checksum.as_bytes(),
            &signature,
            &signers.publisher.public_key_bytes(),
        )
        .unwrap();
        Self {
            name: name.to_owned(),
            manifest,
            source: source.to_owned(),
            archive: packed.data,
            checksum: packed.checksum,
            signature,
        }
    }

    fn entry(&self, signers: &Signers, mode: WireNames, download: &str) -> Value {
        let raw_name = wire_name(&self.name, mode);
        let mut entry = json!({
            "name": raw_name, "vers": VERSION, "deps": [], "features": {},
            "cksum": self.checksum, "sig": self.signature, "key_fp": signers.publisher.fingerprint(),
            "yanked": false, "edition": "2026", "dl": download,
            "registry_key_fp": "fixture-registry", "published_at": PUBLISHED_AT,
        });
        signers.countersign(&mut entry, &raw_name);
        entry
    }

    fn serve(&self, signers: &Signers, server: &FixtureServer, mode: WireNames) -> Value {
        let download_path = format!("/archives/{}/{VERSION}.tar.zst", self.name);
        server.reply(&download_path, 200, self.archive.clone());
        let entry = self.entry(signers, mode, &format!("{}{download_path}", server.origin));
        server.metadata(&self.name, mode, &entry);
        entry
    }
}

struct Profile {
    directory: TempDir,
    home: PathBuf,
    hew_home: PathBuf,
    cache: PathBuf,
}

impl Profile {
    fn new() -> Self {
        let directory = tempfile::tempdir().unwrap();
        let home = directory.path().join("home");
        let hew_home = home.join(".hew");
        let cache = hew_home.join("packages");
        fs::create_dir_all(&hew_home).unwrap();
        Self {
            directory,
            home,
            hew_home,
            cache,
        }
    }

    fn configure(
        &self,
        registry_settings: &str,
        named: &[(&str, &FixtureServer, Option<WireNames>)],
    ) {
        use std::fmt::Write as _;

        let mut config = format!(
            "[registry]\npath = {:?}\n{registry_settings}\n",
            self.cache.to_str().unwrap()
        );
        for (name, server, mode) in named {
            write!(
                &mut config,
                "\n[registries.{name}]\nindex = \"https://fixture.invalid/index\"\napi = {:?}\n",
                server.api()
            )
            .unwrap();
            if let Some(mode) = mode {
                writeln!(
                    &mut config,
                    "wire-names = {:?}",
                    match mode {
                        WireNames::Dotted => "dotted",
                        WireNames::Slash => "slash",
                    }
                )
                .unwrap();
            }
        }
        fs::write(self.hew_home.join("config.toml"), config).unwrap();
    }

    fn project(&self, label: &str, registry: &str, features: &[&str]) -> PathBuf {
        self.project_for(label, registry, features, "alice.router")
    }

    fn project_for(
        &self,
        label: &str,
        registry: &str,
        features: &[&str],
        package: &str,
    ) -> PathBuf {
        let project = self.directory.path().join(label);
        fs::create_dir_all(&project).unwrap();
        let features = serde_json::to_string(features).unwrap();
        fs::write(project.join("hew.toml"), format!(
            "[package]\nname = \"consumer\"\nversion = \"0.1.0\"\nedition = \"2026\"\n[dependencies]\n{package:?} = {{ version = \"^1.2\", registry = {registry:?}, features = {features}, default-features = false }}\n"
        )).unwrap();
        let module = package.rsplit('.').next().unwrap();
        fs::write(
            project.join("main.hew"),
            format!("import {package};\nfn main() {{ println({module}.answer()); }}\n"),
        )
        .unwrap();
        project
    }

    fn run(&self, project: &Path, arguments: &[&str], primary: Option<&FixtureServer>) -> Output {
        let mut command = if let Some(binary) = std::env::var_os("HEW_BIN") {
            let mut command = Command::new(binary);
            command.args(arguments);
            command
        } else {
            let mut command = Command::new(std::env::current_exe().unwrap());
            command.args(["--exact", "registry_wire_command_child", "--nocapture"]);
            command.env(CHILD_ARGS, serde_json::to_string(arguments).unwrap());
            command
        };
        command
            .current_dir(project)
            .env("HOME", &self.home)
            .env("HEW_HOME", &self.hew_home)
            .env_remove("USERPROFILE")
            .env_remove("HEW_TOKEN")
            .env_remove("HEW_REGISTRY_TOKEN")
            .env_remove("HEW_REGISTRY")
            .env_remove("HTTP_PROXY")
            .env_remove("HTTPS_PROXY")
            .env_remove("ALL_PROXY")
            .env_remove("http_proxy")
            .env_remove("https_proxy")
            .env_remove("all_proxy")
            .env("NO_PROXY", "127.0.0.1,localhost");
        if let Some(primary) = primary {
            command.env("HEW_REGISTRY", primary.api());
        }
        command.output().unwrap()
    }

    fn installed(&self, server: &FixtureServer, package: &SignedPackage) -> PathBuf {
        Registry::with_root(self.cache.clone()).package_dir_for(
            &config::registry_identity(&server.api()),
            &package.name,
            VERSION,
        )
    }
}

fn assert_success(output: &Output) {
    assert!(
        output.status.success(),
        "command failed:\n{}\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

fn assert_failure(output: &Output, diagnostic: &str) {
    assert!(
        !output.status.success(),
        "invalid registry response was admitted"
    );
    assert!(
        String::from_utf8_lossy(&output.stderr)
            .to_lowercase()
            .contains(&diagnostic.to_lowercase()),
        "expected {diagnostic:?} in diagnostic:\n{}\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

fn assert_installed(
    profile: &Profile,
    project: &Path,
    server: &FixtureServer,
    package: &SignedPackage,
) {
    let cache = profile.installed(server, package);
    assert_eq!(
        fs::read(cache.join(".hew-registry-cache.tar.zst")).unwrap(),
        package.archive
    );
    assert_eq!(
        fs::read_to_string(cache.join("hew.toml")).unwrap(),
        package.manifest
    );
    assert_eq!(
        fs::read_to_string(cache.join(format!("{}.hew", package.name.rsplit('.').next().unwrap())))
            .unwrap(),
        package.source
    );
    let metadata: toml::Value =
        toml::from_str(&fs::read_to_string(cache.join(".hew-registry-cache.toml")).unwrap())
            .unwrap();
    assert_eq!(metadata["name"].as_str(), Some(package.name.as_str()));
    assert_eq!(
        metadata["registry"].as_str(),
        Some(config::registry_identity(&server.api()).as_str())
    );
    assert_eq!(
        metadata["registry_checksum"].as_str(),
        Some(package.checksum.as_str())
    );
    let local = package
        .name
        .split('.')
        .fold(project.join(".hew/packages"), |path, component| {
            path.join(component)
        });
    assert_eq!(
        fs::read_to_string(local.join("hew.toml")).unwrap(),
        package.manifest
    );
    let lock: toml::Value =
        toml::from_str(&fs::read_to_string(project.join("hew.lock")).unwrap()).unwrap();
    let locked = lock["package"]
        .as_array()
        .unwrap()
        .iter()
        .find(|entry| entry["name"].as_str() == Some(package.name.as_str()))
        .unwrap();
    assert_eq!(locked["version"].as_str(), Some(VERSION));
    assert_eq!(
        locked["registry"].as_str(),
        Some(config::registry_identity(&server.api()).as_str())
    );
    assert!(
        cache.ancestors().any(|path| path
            .file_name()
            .is_some_and(|name| name == package.name.as_str())),
        "logical dotted cache slot missing: {}",
        cache.display()
    );
}

#[test]
fn signed_slash_install_preserves_authored_bytes_features_and_offline_lock() {
    assert_eq!(
        RegistryClient::with_url(config::DEFAULT_REGISTRY_API).package_url("alice.router"),
        "https://registry.hewpkg.com/api/v1/packages/alice/router"
    );
    let server = FixtureServer::new();
    let signers = Signers::new();
    signers.serve_keys(&server);
    let codec = SignedPackage::new(
        &signers,
        "alice.codec",
        "pub fn answer() -> i32 { 41 }\n",
        "",
    );
    codec.serve(&signers, &server, WireNames::Slash);
    let router = SignedPackage::new(&signers, "alice.router", "import alice.codec;\npub fn answer() -> i32 { codec.answer() + 1 }\n",
        "[dependencies]\n\"alice.codec\" = { version = \"^1.2\", optional = true }\n[features]\nenhanced = [\"alice.codec\"]\n");
    let mut entry = router.serve(&signers, &server, WireNames::Slash);
    entry["deps"] = json!([{ "name": "alice/codec", "req": "^1.2", "features": [], "optional": true, "default_features": true }]);
    entry["features"] = json!({ "enhanced": ["alice/codec"], "local/feature": ["other/label"] });
    server.metadata(&router.name, WireNames::Slash, &entry);
    let profile = Profile::new();
    profile.configure("", &[("signed", &server, Some(WireNames::Slash))]);
    let project = profile.project("consumer", "signed", &["enhanced"]);
    assert_success(&profile.run(&project, &["install"], None));
    assert_installed(&profile, &project, &server, &router);
    assert_installed(&profile, &project, &server, &codec);
    let requests = server.requests();
    assert!(requests.contains(&"/api/v1/packages/alice/router".to_owned()));
    assert!(requests.contains(&"/api/v1/packages/alice/codec".to_owned()));
    assert!(!requests
        .iter()
        .any(|path| path.starts_with("/api/v1/packages/alice.")));
    assert!(requests.contains(&"/api/v1/registry-key".to_owned()));
    let client = RegistryClient::with_url(server.api()).with_wire_names(WireNames::Slash);
    let normalized = client.get_package("alice.router").unwrap();
    assert_eq!(normalized[0].name, "alice.router");
    assert_eq!(normalized[0].deps[0].name, "alice.codec");
    assert_eq!(normalized[0].features["enhanced"], ["alice.codec"]);
    assert_eq!(normalized[0].features["local/feature"], ["other/label"]);
    assert_eq!(normalized[0].sig, router.signature);
    assert_eq!(normalized[0].cksum, router.checksum);
    assert_eq!(normalized[0].registry_name.as_deref(), Some("alice/router"));
    let lock = fs::read(project.join("hew.lock")).unwrap();
    server.clear_requests();
    fs::remove_dir_all(project.join(".hew/packages")).unwrap();
    assert_success(&profile.run(&project, &["install", "--locked", "--offline"], None));
    assert_eq!(fs::read(project.join("hew.lock")).unwrap(), lock);
    assert_installed(&profile, &project, &server, &router);
    assert_installed(&profile, &project, &server, &codec);
    assert!(
        server.requests().is_empty(),
        "locked offline install contacted the registry"
    );
    if std::env::var_os("HEW_BIN").is_some() && std::env::var_os("HEW_WIRE_TEST_NATIVE").is_some() {
        assert_success(&profile.run(&project, &["check", "main.hew"], None));
        let output = profile.run(&project, &["run", "main.hew"], None);
        assert_success(&output);
        assert_eq!(String::from_utf8_lossy(&output.stdout).trim(), "42");
    }
}

#[test]
fn custom_dotted_default_and_slash_opt_in_keep_distinct_signed_sources() {
    let dotted = FixtureServer::new();
    let slash = FixtureServer::new();
    let signers = Signers::new();
    signers.serve_keys(&dotted);
    signers.serve_keys(&slash);
    let first = SignedPackage::new(
        &signers,
        "alice.router",
        "pub fn answer() -> i32 { 11 }\n",
        "",
    );
    let second = SignedPackage::new(
        &signers,
        "alice.router",
        "pub fn answer() -> i32 { 22 }\n",
        "",
    );
    first.serve(&signers, &dotted, WireNames::Dotted);
    second.serve(&signers, &slash, WireNames::Slash);
    let profile = Profile::new();
    profile.configure(
        "",
        &[
            ("dotted", &dotted, None),
            ("slash", &slash, Some(WireNames::Slash)),
        ],
    );
    let first_project = profile.project("first", "dotted", &[]);
    let second_project = profile.project("second", "slash", &[]);
    assert_success(&profile.run(&first_project, &["install"], None));
    assert_success(&profile.run(&second_project, &["install"], None));
    assert_installed(&profile, &first_project, &dotted, &first);
    assert_installed(&profile, &second_project, &slash, &second);
    assert_ne!(
        profile.installed(&dotted, &first),
        profile.installed(&slash, &second)
    );
    assert!(dotted
        .requests()
        .contains(&"/api/v1/packages/alice.router".to_owned()));
    assert!(slash
        .requests()
        .contains(&"/api/v1/packages/alice/router".to_owned()));
    dotted.clear_requests();
    slash.clear_requests();
    for project in [&first_project, &second_project] {
        fs::remove_dir_all(project.join(".hew/packages")).unwrap();
        assert_success(&profile.run(project, &["install", "--locked", "--offline"], None));
    }
    assert_installed(&profile, &first_project, &dotted, &first);
    assert_installed(&profile, &second_project, &slash, &second);
    assert!(dotted.requests().is_empty() && slash.requests().is_empty());

    // The default endpoint's read commands honour HEW_REGISTRY and its explicit
    // wire policy without changing the existing default install source policy.
    profile.configure("", &[]);
    assert_success(&profile.run(&first_project, &["info", "alice.router"], Some(&dotted)));
    assert_eq!(dotted.requests(), ["/api/v1/packages/alice.router"]);
    profile.configure("wire-names = \"slash\"", &[]);
    assert_success(&profile.run(&second_project, &["info", "alice.router"], Some(&slash)));
    assert_eq!(slash.requests(), ["/api/v1/packages/alice/router"]);
}

#[test]
fn signed_metadata_and_cdn_fallback_use_each_endpoints_wire_identity() {
    for (primary_mode, mirror_mode) in [
        (WireNames::Slash, WireNames::Dotted),
        (WireNames::Dotted, WireNames::Slash),
    ] {
        let primary = FixtureServer::new();
        let mirror = FixtureServer::new();
        let cdn = FixtureServer::new();
        let signers = Signers::new();
        signers.serve_keys(&mirror);
        primary.reply(
            &format!(
                "/api/v1/packages/{}",
                wire_name("alice.router", primary_mode)
            ),
            503,
            br#"{"error":"primary unavailable"}"#.to_vec(),
        );
        primary.reply(
            "/api/v1/registry-key",
            503,
            br#"{"error":"primary unavailable"}"#.to_vec(),
        );
        primary.reply(
            &format!(
                "/api/v1/keys/{}",
                encode_path(&signers.publisher.fingerprint())
            ),
            503,
            br#"{"error":"primary unavailable"}"#.to_vec(),
        );
        let router = SignedPackage::new(
            &signers,
            "alice.router",
            "pub fn answer() -> i32 { 42 }\n",
            "",
        );
        let cdn_path = "/objects/content-addressed-router.tar.zst";
        cdn.reply(cdn_path, 503, br#"{"error":"CDN unavailable"}"#.to_vec());
        let entry = router.entry(&signers, mirror_mode, &format!("{}{cdn_path}", cdn.origin));
        mirror.metadata(&router.name, mirror_mode, &entry);
        let archive_path = format!(
            "/packages/{}/{VERSION}.tar.zst",
            wire_name(&router.name, mirror_mode)
        );
        mirror.reply(&archive_path, 200, router.archive.clone());
        let profile = Profile::new();
        profile.configure("", &[("signed", &primary, Some(primary_mode))]);
        // These fields belong to this named registry, rather than the global
        // default. The primary remains the cache/lock authority after fallback.
        let config_path = profile.hew_home.join("config.toml");
        let settings = fs::read_to_string(&config_path).unwrap();
        let mirror_mode_text = match mirror_mode {
            WireNames::Dotted => "dotted",
            WireNames::Slash => "slash",
        };
        fs::write(
            config_path,
            format!(
                "{settings}fallback-api = {:?}\nfallback-wire-names = {mirror_mode_text:?}\n",
                mirror.api()
            ),
        )
        .unwrap();
        let project = profile.project("consumer", "signed", &[]);
        assert_success(&profile.run(&project, &["install"], None));
        assert_installed(&profile, &project, &primary, &router);
        assert_eq!(cdn.requests(), [cdn_path]);
        let mirror_requests = mirror.requests();
        assert!(mirror_requests.contains(&format!(
            "/api/v1/packages/{}",
            wire_name(&router.name, mirror_mode)
        )));
        assert!(mirror_requests.contains(&archive_path));
        assert!(!mirror_requests
            .iter()
            .any(|path| path.contains("/tarballs/")));
        assert!(!mirror_requests.contains(&format!(
            "/packages/{}/{VERSION}.tar.zst",
            wire_name(&router.name, primary_mode)
        )));
        let client = RegistryClient::with_url(primary.api())
            .with_wire_names(primary_mode)
            .with_fallback_wire_names(mirror.api(), mirror_mode);
        let resolved = client.get_package("alice.router").unwrap();
        assert_eq!(
            resolved[0].registry_name.as_deref(),
            Some(wire_name(&router.name, mirror_mode).as_str())
        );
    }
}

#[test]
fn authoritative_not_found_never_tries_mirror_or_another_spelling() {
    let primary = FixtureServer::new();
    let mirror = FixtureServer::new();
    let cdn = FixtureServer::new();
    let signers = Signers::new();
    let router = SignedPackage::new(
        &signers,
        "alice.router",
        "pub fn answer() -> i32 { 42 }\n",
        "",
    );
    router.serve(&signers, &mirror, WireNames::Dotted);
    primary.reply(
        "/api/v1/packages/alice/router",
        404,
        br#"{"error":"authoritative absence"}"#.to_vec(),
    );
    router.serve(&signers, &primary, WireNames::Dotted); // A spelling probe would incorrectly succeed.
    let client = RegistryClient::with_url(primary.api())
        .with_wire_names(WireNames::Slash)
        .with_fallback_wire_names(mirror.api(), WireNames::Dotted);
    assert!(client.get_package("alice.router").is_err());
    assert_eq!(primary.requests(), ["/api/v1/packages/alice/router"]);
    assert!(mirror.requests().is_empty());
    cdn.reply(
        "/object.tar.zst",
        404,
        br#"{"error":"authoritative absence"}"#.to_vec(),
    );
    mirror.reply(
        &format!("/packages/alice.router/{VERSION}.tar.zst"),
        200,
        router.archive.clone(),
    );
    assert!(client
        .download_package_tarball(
            "alice.router",
            VERSION,
            &format!("{}/object.tar.zst", cdn.origin)
        )
        .is_err());
    assert_eq!(cdn.requests(), ["/object.tar.zst"]);
    assert!(mirror.requests().is_empty());
}

#[test]
fn supplied_signature_tampering_refuses_fresh_and_same_checksum_cached_installs() {
    let server = FixtureServer::new();
    let signers = Signers::new();
    signers.serve_keys(&server);
    let router = SignedPackage::new(
        &signers,
        "alice.router",
        "pub fn answer() -> i32 { 42 }\n",
        "",
    );
    let valid = router.serve(&signers, &server, WireNames::Slash);
    let cached = Profile::new();
    cached.configure("", &[("signed", &server, Some(WireNames::Slash))]);
    let cached_project = cached.project("consumer", "signed", &[]);
    assert_success(&cached.run(&cached_project, &["install"], None));
    let cached_archive = cached
        .installed(&server, &router)
        .join(".hew-registry-cache.tar.zst");
    let original_lock = fs::read(cached_project.join("hew.lock")).unwrap();
    let cases = [
        "publisher signature",
        "registry signature",
        "wrong wire canonical",
        "publisher pair",
        "missing publisher signature",
        "registry pair",
        "missing registry signature",
    ];
    for case in cases {
        let mut tampered = valid.clone();
        match case {
            "publisher signature" => {
                tampered["sig"] = json!(signers.publisher.sign(b"a different checksum"));
                signers.countersign(&mut tampered, "alice/router");
            }
            "registry signature" => {
                tampered["registry_sig"] = json!(signers.registry.sign(b"another publication"));
            }
            "wrong wire canonical" => signers.countersign(&mut tampered, "alice.router"),
            "publisher pair" => tampered["key_fp"] = json!(""),
            "missing publisher signature" => {
                tampered["sig"] = json!("");
                signers.countersign(&mut tampered, "alice/router");
            }
            "registry pair" => tampered["published_at"] = Value::Null,
            "missing registry signature" => tampered["registry_sig"] = Value::Null,
            _ => unreachable!(),
        }
        server.metadata(&router.name, WireNames::Slash, &tampered);
        let fresh = Profile::new();
        fresh.configure("", &[("signed", &server, Some(WireNames::Slash))]);
        let fresh_project = fresh.project("consumer", "signed", &[]);
        for (profile, project) in [(&fresh, &fresh_project), (&cached, &cached_project)] {
            server.clear_requests();
            let output = profile.run(project, &["install"], None);
            assert_failure(&output, "signature");
            assert!(
                server
                    .requests()
                    .contains(&"/api/v1/packages/alice/router".to_owned()),
                "{case}: current metadata was not fetched"
            );
            assert!(
                !server
                    .requests()
                    .iter()
                    .any(|path| path.starts_with("/archives/")),
                "{case}: invalid metadata reached archive fetch"
            );
        }
        assert!(
            !fresh.installed(&server, &router).join("hew.toml").exists(),
            "{case}: invalid package entered fresh cache"
        );
        assert!(!fresh_project.join("hew.lock").exists());
        assert!(!fresh_project.join(".hew/packages/alice/router").exists());
        assert_eq!(
            fs::read(&cached_archive).unwrap(),
            router.archive,
            "{case}: existing verified cache was changed"
        );
        assert_eq!(
            fs::read(cached_project.join("hew.lock")).unwrap(),
            original_lock,
            "{case}: failed install changed the lock"
        );
    }
    server.metadata(&router.name, WireNames::Slash, &valid);
    server.clear_requests();
    assert_success(&cached.run(&cached_project, &["install"], None));
    assert!(
        !server
            .requests()
            .iter()
            .any(|path| path.starts_with("/archives/")),
        "valid same-checksum cache should remain reusable"
    );
    assert_installed(&cached, &cached_project, &server, &router);
}

#[test]
fn archive_and_response_identity_tampering_never_materialize_a_package() {
    let server = FixtureServer::new();
    let signers = Signers::new();
    signers.serve_keys(&server);
    let router = SignedPackage::new(
        &signers,
        "alice.router",
        "pub fn answer() -> i32 { 42 }\n",
        "",
    );
    let imposter = SignedPackage::new(
        &signers,
        "alice.imposter",
        "pub fn answer() -> i32 { 99 }\n",
        "",
    );
    let altered = SignedPackage::new(
        &signers,
        "alice.router",
        "pub fn answer() -> i32 { 99 }\n",
        "",
    );
    let valid = router.serve(&signers, &server, WireNames::Slash);
    let cases = [
        "checksum",
        "manifest name",
        "response name",
        "wrong response mode",
        "metadata name",
        "missing metadata",
        "invalid dependency",
    ];
    for case in cases {
        let mut entry = valid.clone();
        let archive = match case {
            "checksum" => altered.archive.clone(),
            "manifest name" => {
                entry["cksum"] = json!(imposter.checksum);
                entry["sig"] = json!(imposter.signature);
                signers.countersign(&mut entry, "alice/router");
                imposter.archive.clone()
            }
            "response name" => {
                entry["name"] = json!("alice/imposter");
                signers.countersign(&mut entry, "alice/imposter");
                router.archive.clone()
            }
            "wrong response mode" => {
                entry["name"] = json!("alice.router");
                signers.countersign(&mut entry, "alice.router");
                router.archive.clone()
            }
            "metadata name" | "missing metadata" => router.archive.clone(),
            "invalid dependency" => {
                entry["deps"] = json!([{ "name": "alice//codec", "req": "^1.2", "features": [], "optional": false, "default_features": true }]);
                router.archive.clone()
            }
            _ => unreachable!(),
        };
        server.metadata(&router.name, WireNames::Slash, &entry);
        if case == "metadata name" {
            server.json(
                "/api/v1/packages/alice/router",
                &json!({ "metadata": { "name": "alice/imposter" }, "versions": [entry] }),
            );
        } else if case == "missing metadata" {
            server.json(
                "/api/v1/packages/alice/router",
                &json!({ "versions": [entry] }),
            );
        }
        server.reply(
            &format!("/archives/alice.router/{VERSION}.tar.zst"),
            200,
            archive,
        );
        let profile = Profile::new();
        profile.configure("", &[("signed", &server, Some(WireNames::Slash))]);
        let project = profile.project("consumer", "signed", &[]);
        let output = profile.run(&project, &["install"], None);
        assert!(
            !output.status.success(),
            "{case}: invalid publication installed:\n{}",
            String::from_utf8_lossy(&output.stderr)
        );
        assert!(
            !profile
                .installed(&server, &router)
                .join("hew.toml")
                .exists(),
            "{case}: cache admission"
        );
        assert!(!project.join("hew.lock").exists(), "{case}: lock admission");
        assert!(
            !project.join(".hew/packages/alice/router").exists(),
            "{case}: consumer materialization"
        );
    }
}

#[test]
fn historical_descriptive_name_preserves_canonical_signed_install() {
    let server = FixtureServer::new();
    let signers = Signers::new();
    signers.serve_keys(&server);
    let stats = SignedPackage::new(
        &signers,
        "hew.math.stats",
        "pub fn answer() -> i32 { 42 }\n",
        "",
    );
    let entry = stats.serve(&signers, &server, WireNames::Slash);
    server.json(
        "/api/v1/packages/hew/math/stats",
        &json!({ "metadata": { "name": "hew::math::stats" }, "versions": [entry] }),
    );
    let profile = Profile::new();
    profile.configure("", &[("signed", &server, Some(WireNames::Slash))]);
    let project = profile.project_for("consumer", "signed", &[], &stats.name);
    assert_success(&profile.run(&project, &["install"], None));
    assert_installed(&profile, &project, &server, &stats);
    assert!(server
        .requests()
        .contains(&"/api/v1/packages/hew/math/stats".to_owned()));
    assert!(!server.requests().iter().any(|path| path.contains("::")));
    let client = RegistryClient::with_url(server.api()).with_wire_names(WireNames::Slash);
    let normalized = client.get_package(&stats.name).unwrap();
    assert_eq!(normalized[0].name, stats.name);
    assert_eq!(
        normalized[0].registry_name.as_deref(),
        Some("hew/math/stats")
    );
    assert_eq!(normalized[0].cksum, stats.checksum);
    assert_eq!(normalized[0].sig, stats.signature);
    assert_eq!(
        normalized[0].registry_sig.as_deref(),
        entry["registry_sig"].as_str()
    );
    if std::env::var_os("HEW_BIN").is_some() && std::env::var_os("HEW_WIRE_TEST_NATIVE").is_some() {
        assert_success(&profile.run(&project, &["check", "main.hew"], None));
        let output = profile.run(&project, &["run", "main.hew"], None);
        assert_success(&output);
        assert_eq!(String::from_utf8_lossy(&output.stdout).trim(), "42");
    }
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "independent signed boundary mutations share one valid publication fixture"
)]
fn historical_descriptive_name_does_not_relax_signed_package_boundaries() {
    let server = FixtureServer::new();
    let signers = Signers::new();
    signers.serve_keys(&server);
    let stats = SignedPackage::new(
        &signers,
        "hew.math.stats",
        "pub fn answer() -> i32 { 42 }\n",
        "",
    );
    let valid = stats.serve(&signers, &server, WireNames::Slash);
    let legacy_manifest = stats.manifest.replacen(
        "name = \"hew.math.stats\"",
        "name = \"hew::math::stats\"",
        1,
    );
    // Pack deliberately invalid authored metadata without asking the production
    // packer to accept it. All other archive paths and content remain valid.
    let mut tar_bytes = Vec::new();
    {
        let mut builder = tar::Builder::new(&mut tar_bytes);
        for (name, data) in [
            ("hew.toml", legacy_manifest.as_bytes()),
            ("stats.hew", stats.source.as_bytes()),
        ] {
            let mut header = tar::Header::new_gnu();
            header.set_size(u64::try_from(data.len()).unwrap());
            header.set_mode(0o644);
            header.set_uid(0);
            header.set_gid(0);
            header.set_mtime(0);
            header.set_cksum();
            builder.append_data(&mut header, name, data).unwrap();
        }
        builder.finish().unwrap();
    }
    let legacy_archive = zstd::stream::encode_all(std::io::Cursor::new(tar_bytes), 3).unwrap();
    let cases = [
        "historical version name",
        "historical dependency name",
        "historical authored name",
        "historical signature identity",
        "mixed descriptive name",
        "wrong descriptive identity",
    ];
    for case in cases {
        let mut entry = valid.clone();
        let mut descriptive_name = "hew::math::stats";
        let mut archive = stats.archive.clone();
        match case {
            "historical version name" => {
                entry["name"] = json!("hew::math::stats");
                signers.countersign(&mut entry, "hew::math::stats");
            }
            "historical dependency name" => {
                entry["deps"] = json!([{ "name": "hew::math::codec", "req": "^1.2", "features": [], "optional": false, "default_features": true }]);
            }
            "historical authored name" => {
                archive.clone_from(&legacy_archive);
                let checksum = tarball::checksum_bytes(&archive);
                entry["cksum"] = json!(checksum);
                entry["sig"] = json!(signers.publisher.sign(checksum.as_bytes()));
                signers.countersign(&mut entry, "hew/math/stats");
            }
            "historical signature identity" => signers.countersign(&mut entry, "hew::math::stats"),
            "mixed descriptive name" => descriptive_name = "hew::math/stats",
            "wrong descriptive identity" => descriptive_name = "hew::math::other",
            _ => unreachable!(),
        }
        server.json(
            "/api/v1/packages/hew/math/stats",
            &json!({ "metadata": { "name": descriptive_name }, "versions": [entry] }),
        );
        server.reply(
            &format!("/archives/hew.math.stats/{VERSION}.tar.zst"),
            200,
            archive,
        );
        server.clear_requests();
        let profile = Profile::new();
        profile.configure("", &[("signed", &server, Some(WireNames::Slash))]);
        let project = profile.project_for("consumer", "signed", &[], &stats.name);
        let output = profile.run(&project, &["install"], None);
        assert!(
            !output.status.success(),
            "{case}: invalid publication installed:\n{}",
            String::from_utf8_lossy(&output.stderr)
        );
        assert!(server
            .requests()
            .contains(&"/api/v1/packages/hew/math/stats".to_owned()));
        if case == "historical authored name" {
            assert!(
                server
                    .requests()
                    .iter()
                    .any(|path| path.starts_with("/archives/")),
                "signed legacy archive must reach the authored-name boundary"
            );
        } else {
            assert!(
                !server
                    .requests()
                    .iter()
                    .any(|path| path.starts_with("/archives/")),
                "{case}: invalid metadata reached archive fetch"
            );
        }
        if case == "historical signature identity" {
            assert_failure(&output, "signature");
        }
        assert!(
            !profile.installed(&server, &stats).join("hew.toml").exists(),
            "{case}: cache admission"
        );
        assert!(!project.join("hew.lock").exists(), "{case}: lock admission");
        assert!(
            !project.join(".hew/packages/hew/math/stats").exists(),
            "{case}: consumer materialization"
        );
    }
}
