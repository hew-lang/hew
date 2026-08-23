mod support;

use std::fmt::Write as _;
use std::io::{Read as _, Write as _};
use std::net::{SocketAddr, TcpListener, TcpStream};
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;
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
    assert!(
        stderr.contains(cached_path.to_string_lossy().as_ref()),
        "{stderr}"
    );
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
    let package_source = root.path().join("foo-source");
    std::fs::create_dir_all(&project).unwrap();
    std::fs::create_dir_all(&home).unwrap();
    std::fs::create_dir_all(&package_source).unwrap();
    write_manifest(&project, "foo = \"0.2.1\"\n");
    std::fs::write(
        package_source.join("hew.toml"),
        "[package]\nname = \"foo\"\nversion = \"0.2.1\"\n",
    )
    .unwrap();
    std::fs::write(
        package_source.join("foo.hew"),
        "pub fn answer() -> i64 { 42 }\n",
    )
    .unwrap();
    let packed = hew_pkg::tarball::pack(&package_source, &[], &[]).unwrap();
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
        Duration::from_secs(10),
    )
    .expect("online install must terminate");

    assert!(
        output.status.success(),
        "online install should refetch an incomplete cache entry\n{}",
        describe_output(&output)
    );
    assert_eq!(registry.package_requests.load(Ordering::Relaxed), 1);
    assert_eq!(registry.download_requests.load(Ordering::Relaxed), 1);
    assert!(incomplete.join("hew.toml").is_file());
    assert!(incomplete.join("foo.hew").is_file());
    assert!(!incomplete.join("partial.marker").exists());
    assert!(project.join(".hew/packages/foo/foo.hew").is_file());
}
