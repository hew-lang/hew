mod support;

use std::fmt::Write as _;
use std::io::{Read as _, Write as _};
use std::net::TcpListener;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

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
