mod support;

use std::collections::BTreeMap;
use std::fmt::Write as _;
use std::io::{Read as _, Write as _};
use std::net::{SocketAddr, TcpListener, TcpStream};
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Barrier};
use std::thread::JoinHandle;
use std::time::Duration;

use support::{describe_output, hew_binary};

fn write_manifest(root: &Path, dependencies: &str) {
    std::fs::write(
        root.join("hew.toml"),
        format!(
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\nedition = \"2026\"\n\n[dependencies]\n{dependencies}"
        ),
    )
    .expect("write project manifest");
}

fn write_cached_package(cache: &Path, name: &str, version: &str) -> PathBuf {
    let package = cache.join(name).join(version);
    std::fs::create_dir_all(&package).expect("create cached package");
    std::fs::write(
        package.join("hew.toml"),
        format!("[package]\nname = \"{name}\"\nversion = \"{version}\"\n"),
    )
    .expect("write cached package manifest");
    std::fs::write(
        package.join(format!("{name}.hew")),
        "pub fn answer() -> i64 { 42 }\n",
    )
    .expect("write cached package source");
    package
}

fn write_config(home: &Path, cache: &Path, registry_api: Option<&str>) {
    let hew_home = home.join(".hew");
    std::fs::create_dir_all(&hew_home).expect("create package-manager home");
    let mut config = format!("[registry]\npath = {:?}\n", cache.to_string_lossy());
    if let Some(api) = registry_api {
        write!(
            config,
            "\n[registries.mock]\nindex = \"unused\"\napi = {api:?}\n"
        )
        .expect("format named registry config");
    }
    std::fs::write(hew_home.join("config.toml"), config).expect("write package config");
}

fn run_pkg(home: &Path, project: &Path, args: &[&str]) -> Output {
    Command::new(hew_binary())
        .args(args)
        .current_dir(project)
        .env("HOME", home)
        .env_remove("USERPROFILE")
        .output()
        .expect("run hew package command")
}

fn run_pkg_bounded(home: &Path, project: &Path, args: &[&str], label: &str) -> Output {
    let mut command = Command::new(hew_binary());
    command
        .args(args)
        .current_dir(project)
        .env("HOME", home)
        .env_remove("USERPROFILE");
    support::try_run_bounded_command(command, label, Duration::from_secs(30))
        .expect("package command must terminate")
}

fn snapshot_tree(root: &Path) -> BTreeMap<PathBuf, Vec<u8>> {
    fn visit(root: &Path, path: &Path, snapshot: &mut BTreeMap<PathBuf, Vec<u8>>) {
        let mut entries = std::fs::read_dir(path)
            .unwrap()
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        entries.sort_by_key(std::fs::DirEntry::file_name);
        for entry in entries {
            let entry_path = entry.path();
            let relative = entry_path.strip_prefix(root).unwrap().to_path_buf();
            let metadata = std::fs::symlink_metadata(&entry_path).unwrap();
            if metadata.file_type().is_symlink() {
                snapshot.insert(
                    relative,
                    format!(
                        "symlink:{}",
                        std::fs::read_link(&entry_path).unwrap().display()
                    )
                    .into_bytes(),
                );
            } else if metadata.is_dir() {
                snapshot.insert(relative.clone(), b"directory".to_vec());
                visit(root, &entry_path, snapshot);
            } else {
                snapshot.insert(relative, std::fs::read(entry_path).unwrap());
            }
        }
    }

    let mut snapshot = BTreeMap::new();
    if root.is_dir() {
        visit(root, root, &mut snapshot);
    }
    snapshot
}

fn start_404_registry() -> String {
    let listener = TcpListener::bind("127.0.0.1:0").expect("bind mock registry");
    let address = listener.local_addr().expect("mock registry address");
    std::thread::spawn(move || {
        let (mut stream, _) = listener.accept().expect("accept registry request");
        let mut request = [0_u8; 4096];
        let _ = stream.read(&mut request);
        let body = r#"{"error":"package not found"}"#;
        write!(
            stream,
            "HTTP/1.1 404 Not Found\r\nContent-Type: application/json\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{body}",
            body.len()
        )
        .expect("write registry response");
    });
    format!("http://{address}/api/v1")
}

struct RepairRegistry {
    api_url: String,
    package_requests: Arc<AtomicUsize>,
    download_requests: Arc<AtomicUsize>,
    stop: Arc<AtomicBool>,
    wake_address: SocketAddr,
    handle: Option<JoinHandle<()>>,
}

impl Drop for RepairRegistry {
    fn drop(&mut self) {
        self.stop.store(true, Ordering::Relaxed);
        let _ = TcpStream::connect(self.wake_address);
        if let Some(handle) = self.handle.take() {
            let result = handle.join();
            if !std::thread::panicking() {
                result.expect("join mock registry");
            }
        }
    }
}

fn start_repair_registry(tarball: Vec<u8>, checksum: String) -> RepairRegistry {
    let listener = TcpListener::bind("127.0.0.1:0").expect("bind mock registry");
    let address = listener.local_addr().expect("mock registry address");
    let api_url = format!("http://{address}/api/v1");
    let download_url = format!("http://{address}/packages/foo/0.2.1.tar.zst");
    let package_requests = Arc::new(AtomicUsize::new(0));
    let download_requests = Arc::new(AtomicUsize::new(0));
    let stop = Arc::new(AtomicBool::new(false));
    let thread_package_requests = Arc::clone(&package_requests);
    let thread_download_requests = Arc::clone(&download_requests);
    let thread_stop = Arc::clone(&stop);

    let handle = std::thread::spawn(move || {
        while !thread_stop.load(Ordering::Relaxed) {
            let (mut stream, _) = listener.accept().expect("accept registry request");
            if thread_stop.load(Ordering::Relaxed) {
                break;
            }
            let mut request = [0_u8; 4096];
            let bytes_read = stream.read(&mut request).expect("read registry request");
            let request = String::from_utf8_lossy(&request[..bytes_read]);
            let path = request
                .lines()
                .next()
                .and_then(|line| line.split_whitespace().nth(1))
                .expect("request path");

            if path == "/api/v1/packages/foo" {
                thread_package_requests.fetch_add(1, Ordering::Relaxed);
                let body = serde_json::json!({
                    "versions": [{
                        "name": "foo",
                        "vers": "0.2.1",
                        "cksum": checksum,
                        "sig": "",
                        "key_fp": "",
                        "dl": download_url,
                    }]
                })
                .to_string();
                write!(
                    stream,
                    "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{body}",
                    body.len()
                )
                .expect("write package response");
            } else if path == "/packages/foo/0.2.1.tar.zst" {
                thread_download_requests.fetch_add(1, Ordering::Relaxed);
                write!(
                    stream,
                    "HTTP/1.1 200 OK\r\nContent-Type: application/octet-stream\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
                    tarball.len()
                )
                .expect("write tarball headers");
                stream.write_all(&tarball).expect("write tarball");
            } else {
                let body = r#"{"error":"not found"}"#;
                write!(
                    stream,
                    "HTTP/1.1 404 Not Found\r\nContent-Type: application/json\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{body}",
                    body.len()
                )
                .expect("write not-found response");
            }
        }
    });

    RepairRegistry {
        api_url,
        package_requests,
        download_requests,
        stop,
        wake_address: address,
        handle: Some(handle),
    }
}

fn package_tarball(root: &Path) -> hew_pkg::tarball::PackResult {
    package_tarball_variant(root, "default", "pub fn answer() -> i64 { 42 }\n", None)
}

fn package_tarball_variant(
    root: &Path,
    label: &str,
    source: &str,
    path_dependency: Option<&str>,
) -> hew_pkg::tarball::PackResult {
    let package_source = root.join(format!("foo-source-{label}"));
    std::fs::create_dir_all(&package_source).unwrap();
    let dependency = path_dependency.map_or_else(String::new, |path| {
        format!("\n[dependencies]\nevil = {{ path = {path:?} }}\n")
    });
    std::fs::write(
        package_source.join("hew.toml"),
        format!("[package]\nname = \"foo\"\nversion = \"0.2.1\"\n{dependency}"),
    )
    .unwrap();
    std::fs::write(package_source.join("foo.hew"), source).unwrap();
    hew_pkg::tarball::pack(&package_source, &[], &[]).unwrap()
}

fn assert_registry_path_dependency_rejected(path: &str, label: &str) {
    let root = support::tempdir();
    let home = root.path().join("home");
    let project = root.path().join("app");
    let cache = root.path().join("cache");
    std::fs::create_dir_all(&home).unwrap();
    std::fs::create_dir_all(&project).unwrap();
    write_manifest(&project, "foo = \"0.2.1\"\n");
    let packed = package_tarball_variant(
        root.path(),
        label,
        "pub fn answer() -> i64 { 42 }\n",
        Some(path),
    );
    let registry = start_repair_registry(packed.data, packed.checksum);
    write_config(&home, &cache, Some(&registry.api_url));

    let output = run_pkg_bounded(
        &home,
        &project,
        &["install", "--registry", "mock"],
        "reject registry path dependency",
    );
    assert!(
        !output.status.success(),
        "registry path dependency must fail\n{}",
        describe_output(&output)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("forbidden path dependency"), "{stderr}");
    assert!(stderr.contains(path), "{stderr}");
    assert!(!project.join("hew.lock").exists());
    assert!(!project.join(".hew").exists());
    assert!(!cache.join(".registries").exists());
    assert_eq!(registry.download_requests.load(Ordering::Relaxed), 1);
}

fn write_lockfile(project: &Path, version: &str, checksum: Option<&str>) {
    let checksum = checksum.map_or_else(String::new, |value| format!("checksum = {value:?}\n"));
    let registry = hew_pkg::config::default_registry_identity();
    std::fs::write(
        project.join("hew.lock"),
        format!(
            "[[package]]\nname = \"foo\"\nrequirement = \"0.2.1\"\nversion = {version:?}\nregistry = {registry:?}\n{checksum}"
        ),
    )
    .unwrap();
}

#[test]
fn pkg_rejects_relative_path_dependency_in_registry_archive_without_mutation() {
    assert_registry_path_dependency_rejected("nested", "relative-path");
}

#[test]
fn pkg_rejects_traversal_path_dependency_in_registry_archive_without_mutation() {
    assert_registry_path_dependency_rejected("../../escape", "traversal-path");
}

#[test]
fn pkg_rejects_absolute_path_dependency_in_registry_archive_without_mutation() {
    assert_registry_path_dependency_rejected("/etc/hew-poison", "absolute-path");
}

#[test]
fn pkg_registry_404_refuses_stale_cache() {
    let root = support::tempdir();
    let home = root.path().join("home");
    let project = root.path().join("app");
    let cache = root.path().join("cache");
    std::fs::create_dir_all(&project).unwrap();
    std::fs::create_dir_all(&home).unwrap();
    write_manifest(&project, "foo = \"0.2.1\"\n");
    let cached_path = write_cached_package(&cache, "foo", "0.2.1");
    let registry_api = start_404_registry();
    write_config(&home, &cache, Some(&registry_api));

    let output = run_pkg(&home, &project, &["install", "--registry", "mock"]);
    assert!(
        !output.status.success(),
        "registry miss must fail closed\n{}",
        describe_output(&output)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("foo"), "{stderr}");
    assert!(stderr.contains("0.2.1"), "{stderr}");
    assert!(
        stderr.contains(&format!("{registry_api}/packages/foo")),
        "{stderr}"
    );
    assert!(!stderr.contains(cached_path.to_string_lossy().as_ref()));
    assert!(stderr.contains("was not used"), "{stderr}");
    assert!(stderr.contains("--offline"), "{stderr}");
    assert!(!project.join(".hew/packages/foo").exists());
}

#[test]
fn pkg_offline_uses_cache_and_says_so() {
    let root = support::tempdir();
    let home = root.path().join("home");
    let project = root.path().join("app");
    let cache = root.path().join("cache");
    std::fs::create_dir_all(&project).unwrap();
    std::fs::create_dir_all(&home).unwrap();
    write_manifest(&project, "foo = \"0.2.1\"\n");
    write_cached_package(&cache, "foo", "0.2.1");
    write_config(&home, &cache, None);

    let output = run_pkg(&home, &project, &["install", "--offline"]);
    assert!(
        output.status.success(),
        "offline install should use cache\n{}",
        describe_output(&output)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("Offline mode"), "{stderr}");
    assert!(
        stderr.contains(cache.to_string_lossy().as_ref()),
        "{stderr}"
    );
    assert!(project.join(".hew/packages/foo/foo.hew").is_file());
}

#[test]
fn pkg_online_repairs_incomplete_cache_without_loop() {
    let root = support::tempdir();
    let home = root.path().join("home");
    let project = root.path().join("app");
    let cache = root.path().join("cache");
    std::fs::create_dir_all(&project).unwrap();
    std::fs::create_dir_all(&home).unwrap();
    write_manifest(&project, "foo = \"0.2.1\"\n");
    let packed = package_tarball(root.path());
    let registry = start_repair_registry(packed.data, packed.checksum);
    write_config(&home, &cache, Some(&registry.api_url));

    let incomplete = cache.join("foo").join("0.2.1");
    std::fs::create_dir_all(&incomplete).unwrap();
    std::fs::write(incomplete.join("partial.marker"), "incomplete").unwrap();

    let mut command = Command::new(hew_binary());
    command
        .args(["install", "--registry", "mock"])
        .current_dir(&project)
        .env("HOME", &home)
        .env_remove("USERPROFILE");
    let output = support::try_run_bounded_command(
        command,
        "online install repairs incomplete cache",
        Duration::from_secs(30),
    )
    .expect("online install must terminate");

    assert!(
        output.status.success(),
        "online install should refetch an incomplete cache entry\n{}",
        describe_output(&output)
    );
    assert_eq!(registry.package_requests.load(Ordering::Relaxed), 1);
    assert_eq!(registry.download_requests.load(Ordering::Relaxed), 1);
    let active = hew_pkg::registry::Registry::with_root(cache.clone()).package_dir_for(
        &hew_pkg::config::registry_identity(&registry.api_url),
        "foo",
        "0.2.1",
    );
    assert!(active.join("hew.toml").is_file());
    assert!(active.join("foo.hew").is_file());
    assert!(incomplete.join("partial.marker").exists());
    assert!(project.join(".hew/packages/foo/foo.hew").is_file());
}

#[test]
fn pkg_online_repairs_tampered_manifest_present_cache() {
    let root = support::tempdir();
    let home = root.path().join("home");
    let project = root.path().join("app");
    let cache = root.path().join("cache");
    std::fs::create_dir_all(&project).unwrap();
    std::fs::create_dir_all(&home).unwrap();
    write_manifest(&project, "foo = \"0.2.1\"\n");
    let packed = package_tarball(root.path());
    let registry = start_repair_registry(packed.data, packed.checksum);
    write_config(&home, &cache, Some(&registry.api_url));

    let first = run_pkg_bounded(
        &home,
        &project,
        &["install", "--registry", "mock"],
        "initial verified cache install",
    );
    assert!(
        first.status.success(),
        "initial online install failed\n{}",
        describe_output(&first)
    );
    let cache_registry = hew_pkg::registry::Registry::with_root(cache.clone());
    let registry_id = hew_pkg::config::registry_identity(&registry.api_url);
    let cached = cache_registry.package_dir_for(&registry_id, "foo", "0.2.1");
    std::fs::write(cached.join("foo.hew"), "pub fn answer() -> i64 { 7 }\n").unwrap();

    let output = run_pkg_bounded(
        &home,
        &project,
        &["install", "--registry", "mock"],
        "tampered cache repair",
    );
    assert!(
        output.status.success(),
        "online install should replace an unverified manifest-present cache\n{}",
        describe_output(&output)
    );
    assert_eq!(registry.download_requests.load(Ordering::Relaxed), 2);
    assert_eq!(
        std::fs::read_to_string(
            cache_registry
                .package_dir_for(&registry_id, "foo", "0.2.1")
                .join("foo.hew"),
        )
        .unwrap(),
        "pub fn answer() -> i64 { 42 }\n"
    );
    assert_eq!(
        std::fs::read_to_string(cached.join("foo.hew")).unwrap(),
        "pub fn answer() -> i64 { 7 }\n",
        "published generations are immutable"
    );
    assert!(cache_registry
        .package_dir_for(&registry_id, "foo", "0.2.1")
        .join(".hew-registry-cache.toml")
        .is_file());
}

#[test]
fn pkg_malformed_archive_preserves_untrusted_old_cache() {
    let root = support::tempdir();
    let home = root.path().join("home");
    let project = root.path().join("app");
    let cache = root.path().join("cache");
    std::fs::create_dir_all(&project).unwrap();
    std::fs::create_dir_all(&home).unwrap();
    write_manifest(&project, "foo = \"0.2.1\"\n");
    let cached = write_cached_package(&cache, "foo", "0.2.1");
    let old_source = std::fs::read(cached.join("foo.hew")).unwrap();
    let malformed = b"not a zstd archive".to_vec();
    let registry = start_repair_registry(
        malformed.clone(),
        hew_pkg::tarball::checksum_bytes(&malformed),
    );
    write_config(&home, &cache, Some(&registry.api_url));

    let output = run_pkg_bounded(
        &home,
        &project,
        &["install", "--registry", "mock"],
        "malformed archive replacement",
    );
    assert!(
        !output.status.success(),
        "malformed replacement must fail\n{}",
        describe_output(&output)
    );
    assert_eq!(std::fs::read(cached.join("foo.hew")).unwrap(), old_source);
    assert!(!cached.join(".hew-registry-cache.toml").exists());
    assert!(!project.join(".hew/packages/foo").exists());
}

#[test]
fn pkg_concurrent_repair_downloads_and_publishes_once() {
    let root = support::tempdir();
    let home = root.path().join("home");
    let cache = root.path().join("cache");
    let first_project = root.path().join("first");
    let second_project = root.path().join("second");
    std::fs::create_dir_all(&home).unwrap();
    std::fs::create_dir_all(&first_project).unwrap();
    std::fs::create_dir_all(&second_project).unwrap();
    write_manifest(&first_project, "foo = \"0.2.1\"\n");
    write_manifest(&second_project, "foo = \"0.2.1\"\n");
    write_cached_package(&cache, "foo", "0.2.1");
    let packed = package_tarball(root.path());
    let registry = start_repair_registry(packed.data, packed.checksum);
    write_config(&home, &cache, Some(&registry.api_url));

    let barrier = Arc::new(Barrier::new(2));
    let run_install = |project: PathBuf, barrier: Arc<Barrier>| {
        let home = home.clone();
        std::thread::spawn(move || {
            barrier.wait();
            let mut command = Command::new(hew_binary());
            command
                .args(["install", "--registry", "mock"])
                .current_dir(project)
                .env("HOME", home)
                .env_remove("USERPROFILE");
            support::try_run_bounded_command(
                command,
                "concurrent package cache repair",
                Duration::from_secs(30),
            )
            .expect("concurrent repair must terminate")
        })
    };
    let first = run_install(first_project.clone(), Arc::clone(&barrier));
    let second = run_install(second_project.clone(), barrier);
    let first_output = first.join().unwrap();
    let second_output = second.join().unwrap();

    assert!(
        first_output.status.success(),
        "first concurrent install failed\n{}",
        describe_output(&first_output)
    );
    assert!(
        second_output.status.success(),
        "second concurrent install failed\n{}",
        describe_output(&second_output)
    );
    assert_eq!(registry.package_requests.load(Ordering::Relaxed), 2);
    assert_eq!(registry.download_requests.load(Ordering::Relaxed), 2);
    assert_eq!(
        std::fs::read_to_string(
            hew_pkg::registry::Registry::with_root(cache)
                .package_dir_for(
                    &hew_pkg::config::registry_identity(&registry.api_url),
                    "foo",
                    "0.2.1",
                )
                .join("foo.hew")
        )
        .unwrap(),
        "pub fn answer() -> i64 { 42 }\n"
    );
}

#[test]
fn pkg_online_install_keeps_registry_confirmed_generation_after_pointer_swap() {
    let root = support::tempdir();
    let home = root.path().join("home");
    let cache = root.path().join("cache");
    let seed_project = root.path().join("seed-b");
    let project = root.path().join("install-a");
    std::fs::create_dir_all(&home).unwrap();
    std::fs::create_dir_all(&seed_project).unwrap();
    std::fs::create_dir_all(&project).unwrap();
    write_manifest(&seed_project, "foo = \"0.2.1\"\n");
    write_manifest(&project, "foo = \"0.2.1\"\n");

    let packed_b =
        package_tarball_variant(root.path(), "b", "pub fn answer() -> i64 { 222 }\n", None);
    let checksum_b = packed_b.checksum.clone();
    let registry_b = start_repair_registry(packed_b.data, packed_b.checksum);
    write_config(&home, &cache, Some(&registry_b.api_url));
    let seeded = run_pkg_bounded(
        &home,
        &seed_project,
        &["install", "--registry", "mock"],
        "seed adversarial generation B",
    );
    assert!(
        seeded.status.success(),
        "could not seed generation B\n{}",
        describe_output(&seeded)
    );
    let adversarial_registry_id = hew_pkg::config::registry_identity(&registry_b.api_url);
    drop(registry_b);

    let cache_registry = hew_pkg::registry::Registry::with_root(cache.clone());
    let generation_b = cache_registry.package_dir_for(&adversarial_registry_id, "foo", "0.2.1");
    assert_eq!(
        std::fs::read_to_string(generation_b.join("foo.hew")).unwrap(),
        "pub fn answer() -> i64 { 222 }\n"
    );
    let packed_a =
        package_tarball_variant(root.path(), "a", "pub fn answer() -> i64 { 111 }\n", None);
    assert_ne!(packed_a.checksum, checksum_b);
    let registry_a = start_repair_registry(packed_a.data, packed_a.checksum);
    write_config(&home, &cache, Some(&registry_a.api_url));
    let verified_registry_id = hew_pkg::config::registry_identity(&registry_a.api_url);
    let a_slot = cache_registry.package_dir_for(&verified_registry_id, "foo", "0.2.1");
    let a_package_root = a_slot.parent().unwrap();
    std::fs::create_dir_all(a_package_root).unwrap();
    let adversarial_b = a_package_root.join(".0.2.1.generation-bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb");
    std::fs::create_dir_all(&adversarial_b).unwrap();
    std::fs::copy(
        generation_b.join("hew.toml"),
        adversarial_b.join("hew.toml"),
    )
    .unwrap();
    std::fs::copy(generation_b.join("foo.hew"), adversarial_b.join("foo.hew")).unwrap();
    let generation_b_name = adversarial_b
        .file_name()
        .unwrap()
        .to_string_lossy()
        .into_owned();
    let pointer = a_package_root.join(".0.2.1.current");
    std::fs::write(&pointer, format!("{generation_b_name}\n")).unwrap();

    let stop = Arc::new(AtomicBool::new(false));
    let writer_stop = Arc::clone(&stop);
    let swaps = Arc::new(AtomicUsize::new(0));
    let writer_swaps = Arc::clone(&swaps);
    let writer = std::thread::spawn(move || {
        let pointer_b = format!("{generation_b_name}\n");
        while !writer_stop.load(Ordering::Acquire) {
            if std::fs::read_to_string(&pointer)
                .is_ok_and(|current| current.trim() != generation_b_name)
            {
                std::fs::write(&pointer, pointer_b.as_bytes()).unwrap();
                writer_swaps.fetch_add(1, Ordering::Release);
            }
            std::thread::yield_now();
        }
    });

    let output = run_pkg_bounded(
        &home,
        &project,
        &["install", "--registry", "mock"],
        "online verified-generation pointer swap",
    );
    stop.store(true, Ordering::Release);
    writer.join().unwrap();
    assert!(
        output.status.success(),
        "install of confirmed generation A failed\n{}",
        describe_output(&output)
    );
    assert!(
        swaps.load(Ordering::Acquire) > 0,
        "counterfactual writer never replaced the A pointer with B"
    );
    assert_eq!(registry_a.package_requests.load(Ordering::Relaxed), 1);
    assert_eq!(registry_a.download_requests.load(Ordering::Relaxed), 1);
    assert_eq!(
        std::fs::read_to_string(project.join(".hew/packages/foo/foo.hew")).unwrap(),
        "pub fn answer() -> i64 { 111 }\n",
        "materialization must use the registry-confirmed A generation"
    );
}

#[test]
fn pkg_locked_rejects_traversal_version_before_cache_lookup() {
    let root = support::tempdir();
    let home = root.path().join("home");
    let project = root.path().join("app");
    let cache = root.path().join("cache");
    std::fs::create_dir_all(&project).unwrap();
    std::fs::create_dir_all(&home).unwrap();
    write_manifest(&project, "foo = \"0.2.1\"\n");
    write_config(&home, &cache, None);
    write_lockfile(
        &project,
        "../../escape",
        Some("sha256:0000000000000000000000000000000000000000000000000000000000000000"),
    );

    let output = run_pkg_bounded(
        &home,
        &project,
        &["install", "--locked", "--offline"],
        "traversal lock version",
    );
    assert!(
        !output.status.success(),
        "traversal lock version must fail\n{}",
        describe_output(&output)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("invalid version"), "{stderr}");
    assert!(!root.path().join("escape").exists());
}

#[test]
fn pkg_locked_rejects_missing_registry_checksum() {
    let root = support::tempdir();
    let home = root.path().join("home");
    let project = root.path().join("app");
    let cache = root.path().join("cache");
    std::fs::create_dir_all(&project).unwrap();
    std::fs::create_dir_all(&home).unwrap();
    write_manifest(&project, "foo = \"0.2.1\"\n");
    write_config(&home, &cache, None);
    write_lockfile(&project, "0.2.1", None);

    let output = run_pkg_bounded(
        &home,
        &project,
        &["install", "--locked", "--offline"],
        "missing locked checksum",
    );
    assert!(
        !output.status.success(),
        "missing locked checksum must fail\n{}",
        describe_output(&output)
    );
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("missing its locked checksum"),
        "{}",
        describe_output(&output)
    );
}

#[test]
fn pkg_locked_rejects_missing_cached_package() {
    let root = support::tempdir();
    let home = root.path().join("home");
    let project = root.path().join("app");
    let cache = root.path().join("cache");
    std::fs::create_dir_all(&project).unwrap();
    std::fs::create_dir_all(&home).unwrap();
    write_manifest(&project, "foo = \"0.2.1\"\n");
    write_config(&home, &cache, None);
    write_lockfile(
        &project,
        "0.2.1",
        Some("sha256:0000000000000000000000000000000000000000000000000000000000000000"),
    );

    let output = run_pkg_bounded(
        &home,
        &project,
        &["install", "--locked", "--offline"],
        "missing locked cache",
    );
    assert!(
        !output.status.success(),
        "missing locked cache must fail\n{}",
        describe_output(&output)
    );
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("is missing from cache"),
        "{}",
        describe_output(&output)
    );
}

#[test]
fn pkg_locked_uses_exact_version_and_never_rewrites_lockfile() {
    let root = support::tempdir();
    let home = root.path().join("home");
    let project = root.path().join("app");
    let cache = root.path().join("cache");
    std::fs::create_dir_all(&project).unwrap();
    std::fs::create_dir_all(&home).unwrap();
    write_manifest(&project, "foo = \"0.2.1\"\n");
    write_config(&home, &cache, None);

    let locked_package = write_cached_package(&cache, "foo", "0.2.1");
    std::fs::write(
        locked_package.join("foo.hew"),
        "pub fn selected() -> i64 { 21 }\n",
    )
    .unwrap();
    std::fs::write(
        locked_package.join("hew.toml"),
        "[package]\nname = \"foo\"\nversion = \"0.2.1\"\n\n[dependencies]\nbar = \"^1\"\n",
    )
    .unwrap();
    let locked_bar = write_cached_package(&cache, "bar", "1.0.1");
    std::fs::write(
        locked_bar.join("bar.hew"),
        "pub fn selected() -> i64 { 101 }\n",
    )
    .unwrap();
    let initial = run_pkg_bounded(
        &home,
        &project,
        &["install", "--offline"],
        "create initial lock",
    );
    assert!(initial.status.success(), "{}", describe_output(&initial));
    let generated_lock = std::fs::read_to_string(project.join("hew.lock")).unwrap();
    let lock_contents = generated_lock.replace(
        "requirement = \"0.2.1\"",
        "requirement = \"^0.2\" # retained verbatim by --locked",
    );
    assert_ne!(generated_lock, lock_contents);
    std::fs::write(project.join("hew.lock"), &lock_contents).unwrap();
    write_manifest(&project, "foo = \"^0.2\"\n");

    let newer_package = write_cached_package(&cache, "foo", "0.2.2");
    std::fs::write(
        newer_package.join("foo.hew"),
        "pub fn selected() -> i64 { 22 }\n",
    )
    .unwrap();
    std::fs::write(
        newer_package.join("hew.toml"),
        "[package]\nname = \"foo\"\nversion = \"0.2.2\"\n\n[dependencies]\nbar = \"^1\"\n",
    )
    .unwrap();
    let newer_bar = write_cached_package(&cache, "bar", "1.0.2");
    std::fs::write(
        newer_bar.join("bar.hew"),
        "pub fn selected() -> i64 { 102 }\n",
    )
    .unwrap();
    let materialized = project.join(".hew/packages/foo");
    let metadata = std::fs::symlink_metadata(&materialized).unwrap();
    if metadata.file_type().is_symlink() || metadata.is_file() {
        std::fs::remove_file(&materialized).unwrap();
    } else {
        std::fs::remove_dir_all(&materialized).unwrap();
    }

    let output = run_pkg_bounded(
        &home,
        &project,
        &["install", "--locked", "--offline"],
        "locked exact version",
    );
    assert!(
        output.status.success(),
        "locked exact install failed\n{}",
        describe_output(&output)
    );
    assert_eq!(
        std::fs::read_to_string(project.join("hew.lock")).unwrap(),
        lock_contents,
        "--locked must not rewrite even formatting or comments"
    );
    assert_eq!(
        std::fs::read_to_string(project.join(".hew/packages/foo/foo.hew")).unwrap(),
        "pub fn selected() -> i64 { 21 }\n",
        "a newer compatible cache entry must not supersede the locked version"
    );
    assert_eq!(
        std::fs::read_to_string(project.join(".hew/packages/bar/bar.hew")).unwrap(),
        "pub fn selected() -> i64 { 101 }\n",
        "transitive packages must also use the exact locked graph"
    );
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "one matrix shares setup and exact mutation snapshots across all invalid lock classes"
)]
fn pkg_locked_invalid_graphs_never_contact_registry_or_mutate_project_or_cache() {
    let root = support::tempdir();
    let home = root.path().join("home");
    let project = root.path().join("app");
    let seed_project = root.path().join("seed");
    let cache = root.path().join("cache");
    std::fs::create_dir_all(&project).unwrap();
    std::fs::create_dir_all(&seed_project).unwrap();
    std::fs::create_dir_all(&home).unwrap();
    write_manifest(&project, "foo = \"0.2.1\"\n");
    write_manifest(&seed_project, "baz = \"1.0.0\"\n");
    write_config(&home, &cache, None);

    let foo = write_cached_package(&cache, "foo", "0.2.1");
    std::fs::write(
        foo.join("hew.toml"),
        "[package]\nname = \"foo\"\nversion = \"0.2.1\"\n\n[dependencies]\nbar = \"1.0.0\"\n",
    )
    .unwrap();
    write_cached_package(&cache, "bar", "1.0.0");
    write_cached_package(&cache, "baz", "1.0.0");
    let initial = run_pkg_bounded(
        &home,
        &project,
        &["install", "--offline"],
        "create complete lock",
    );
    assert!(initial.status.success(), "{}", describe_output(&initial));
    let seed = run_pkg_bounded(
        &home,
        &seed_project,
        &["install", "--offline"],
        "create extra package lock entry",
    );
    assert!(seed.status.success(), "{}", describe_output(&seed));

    let complete_lock = std::fs::read_to_string(project.join("hew.lock")).unwrap();
    let entries = complete_lock
        .split("[[package]]")
        .skip(1)
        .map(str::to_string)
        .collect::<Vec<_>>();
    let foo_entry = entries
        .iter()
        .find(|entry| entry.contains("name = \"foo\""))
        .unwrap();
    let transitive_entry = entries
        .iter()
        .find(|entry| entry.contains("name = \"bar\""))
        .unwrap();
    let seed_lock = std::fs::read_to_string(seed_project.join("hew.lock")).unwrap();
    let extra_entry = seed_lock
        .split("[[package]]")
        .skip(1)
        .find(|entry| entry.contains("name = \"baz\""))
        .unwrap();
    let default_registry = hew_pkg::config::default_registry_identity();
    let unsupported_entry = foo_entry.replacen(
        "version = \"0.2.1\"",
        "version = \"0.2.1\"\nsource = \"git\"",
        1,
    );
    let wrong_registry_entry = foo_entry.replacen(
        &format!("registry = {default_registry:?}"),
        "registry = \"https://wrong.example/api/v1\"",
        1,
    );
    let invalid_locks = [
        (
            "incomplete",
            format!(
                "# invalid locked graph\n\n[[package]]{foo_entry}[[package]]{transitive_entry}"
            )
            .replace(&format!("[[package]]{transitive_entry}"), ""),
        ),
        ("extra", format!("{complete_lock}\n[[package]]{extra_entry}")),
        (
            "duplicate",
            format!("{complete_lock}\n[[package]]{foo_entry}"),
        ),
        (
            "unsupported",
            format!(
                "# invalid locked graph\n\n[[package]]{unsupported_entry}[[package]]{transitive_entry}"
            ),
        ),
        (
            "wrong-registry",
            format!(
                "# invalid locked graph\n\n[[package]]{wrong_registry_entry}[[package]]{transitive_entry}"
            ),
        ),
    ];

    std::fs::remove_dir_all(project.join(".hew")).unwrap();
    std::fs::remove_dir_all(seed_project.join(".hew")).unwrap();

    let packed = package_tarball(root.path());
    let registry = start_repair_registry(packed.data, packed.checksum);
    write_config(&home, &cache, Some(&registry.api_url));
    for (case, invalid_lock) in invalid_locks {
        std::fs::write(project.join("hew.lock"), invalid_lock).unwrap();
        let project_before = snapshot_tree(&project);
        let cache_before = snapshot_tree(&cache);
        let output = run_pkg_bounded(
            &home,
            &project,
            &["install", "--locked"],
            &format!("invalid locked graph: {case}"),
        );
        assert!(
            !output.status.success(),
            "{case} locked graph must fail\n{}",
            describe_output(&output)
        );
        assert_eq!(
            snapshot_tree(&project),
            project_before,
            "{case} locked failure mutated the project"
        );
        assert_eq!(
            snapshot_tree(&cache),
            cache_before,
            "{case} locked failure mutated the cache"
        );
    }

    assert_eq!(registry.package_requests.load(Ordering::Relaxed), 0);
    assert_eq!(registry.download_requests.load(Ordering::Relaxed), 0);
}
