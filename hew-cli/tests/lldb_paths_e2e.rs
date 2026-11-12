//! LLDB receives a single script path even when the installation has spaces.
#![cfg(target_os = "linux")]

mod support;

use std::os::unix::fs::PermissionsExt;
use std::process::Command;
use support::{hew_binary, require_codegen};

#[test]
fn launch_and_attach_import_the_script_from_a_spaced_path() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("hew lldb path ")
        .tempdir()
        .unwrap();
    let install = dir.path().join("install \"quoted\" \\ path");
    let bin = install.join("bin");
    let lib = install.join("lib");
    let scripts = install.join("share/hew");
    std::fs::create_dir_all(&bin).unwrap();
    std::fs::create_dir_all(&lib).unwrap();
    std::fs::create_dir_all(&scripts).unwrap();
    let link_or_copy = |source: &std::path::Path, target: &std::path::Path| {
        if std::fs::hard_link(source, target).is_err() {
            std::fs::copy(source, target).unwrap();
        }
    };
    link_or_copy(&hew_binary(), &bin.join("hew"));
    link_or_copy(
        &hew_testutil::ensure_hew_lib_built().unwrap(),
        &lib.join("libhew.a"),
    );
    std::fs::write(scripts.join("hew-gdb.py"), "# marker\n").unwrap();
    std::fs::write(scripts.join("hew_lldb.py"), "# marker\n").unwrap();
    let fake = dir.path().join("debugger tools");
    std::fs::create_dir(&fake).unwrap();
    std::fs::write(fake.join("gdb"), "#!/bin/sh\nexit 1\n").unwrap();
    std::fs::write(
        fake.join("lldb"),
        r"#!/usr/bin/python3
import os, shlex, sys
if '--version' in sys.argv:
    print('test lldb')
    sys.exit(0)
commands = [sys.argv[i + 1] for i, value in enumerate(sys.argv[:-1]) if value == '-o']
imports = [shlex.split(command) for command in commands if command.startswith('command script import ')]
assert len(imports) == 1 and len(imports[0]) == 4, imports
assert os.path.isfile(imports[0][3]), imports
print('loaded spaced LLDB script')
",
    )
    .unwrap();
    for name in ["gdb", "lldb"] {
        std::fs::set_permissions(fake.join(name), std::fs::Permissions::from_mode(0o755)).unwrap();
    }
    let path = std::env::join_paths(
        std::iter::once(fake).chain(std::env::split_paths(&std::env::var_os("PATH").unwrap())),
    )
    .unwrap();
    let source = dir.path().join("program.hew");
    std::fs::write(&source, "fn main() { println(42); }\n").unwrap();
    for arguments in [vec!["--attach", "4242"], vec![source.to_str().unwrap()]] {
        let mut command = Command::new(bin.join("hew"));
        command
            .arg("debug")
            .args(arguments)
            .env("PATH", &path)
            .env("HEW_STD", concat!(env!("CARGO_MANIFEST_DIR"), "/../std"));
        let output = hew_testutil::run_command_bounded(
            &mut command,
            "LLDB script path",
            std::time::Duration::from_mins(5),
        )
        .unwrap();
        assert!(
            output.status.success(),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );
        assert!(String::from_utf8_lossy(&output.stdout).contains("loaded spaced LLDB script"));
    }
}
