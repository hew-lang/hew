//! A real Hew object is prepared, loaded and called repeatedly in one host.

#![cfg(any(target_os = "linux", target_os = "macos"))]

use std::ffi::c_void;
use std::path::{Path, PathBuf};
use std::process::Command;

use hew_live::native::{prepare, NativeHost};

fn artifact_dir() -> PathBuf {
    std::env::current_exe()
        .unwrap()
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf()
}

fn compile(directory: &Path, source: &str, name: &str, exports: &[&str]) -> PathBuf {
    let compiler = std::env::var_os("HEW_LIVE_COMPILER")
        .map_or_else(|| artifact_dir().join("hew"), PathBuf::from);
    let input = directory.join(format!("{name}.hew"));
    let object = directory.join(format!("{name}.o"));
    std::fs::write(&input, source).unwrap();
    let mut command = Command::new(compiler);
    command.args(["build", "--emit-obj"]);
    for export in exports {
        command.arg("--export-c").arg(export);
    }
    let result = command.arg(&input).arg("-o").arg(&object).output().unwrap();
    assert!(
        result.status.success(),
        "{}",
        String::from_utf8_lossy(&result.stderr)
    );
    object
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "one generation oracle covers relocation, owned calls and retirement"
)]
fn native_values_survive_calls_and_generations_retire_by_reachability() {
    let directory = tempfile::tempdir().unwrap();
    let runtime_dir =
        std::env::var_os("HEW_LIVE_RUNTIME_DIR").map_or_else(artifact_dir, PathBuf::from);
    let runtime_name = if cfg!(target_os = "macos") {
        "libhew.dylib"
    } else {
        "libhew.so"
    };
    let sdk = directory.path().join("sdk-original");
    let library = sdk.join("lib/hew");
    let modules = library.join("live-node");
    std::fs::create_dir_all(&modules).unwrap();
    let runtime = library.join(runtime_name);
    std::fs::copy(runtime_dir.join(runtime_name), &runtime).unwrap();
    let object = compile(
        directory.path(),
        include_str!("counter.hew"),
        "counter",
        &[
            "create=counter_create",
            "read=counter_read",
            "step=counter_step",
        ],
    );
    let module = modules.join("counter.so");
    let manifest = prepare(&object, &runtime, &module, "test-build", "test-target").unwrap();
    let original = std::fs::read(&module).unwrap();
    assert!(prepare(&object, &runtime, &module, "test-build", "test-target").is_err());
    assert_eq!(
        std::fs::read(&module).unwrap(),
        original,
        "preparation cannot overwrite a published generation"
    );
    let duplicate_source = directory.path().join("duplicate.c");
    let duplicate_object = directory.path().join("duplicate.o");
    std::fs::write(&duplicate_source, "void hew_runtime_new(void) {}\n").unwrap();
    assert!(Command::new("cc")
        .args(["-fPIC", "-c"])
        .arg(&duplicate_source)
        .arg("-o")
        .arg(&duplicate_object)
        .status()
        .unwrap()
        .success());
    assert!(prepare(
        &duplicate_object,
        &runtime,
        &directory.path().join("duplicate.so"),
        "test-build",
        "test-target"
    )
    .unwrap_err()
    .to_string()
    .contains("defines runtime symbol"));
    // A prepared SDK must remain usable after its atomic staging rename or
    // installation elsewhere. The original link-time runtime path is gone.
    let moved = directory.path().join("sdk-moved");
    std::fs::rename(&sdk, &moved).unwrap();
    let runtime = moved.join("lib/hew").join(runtime_name);
    let module = moved.join("lib/hew/live-node/counter.so");
    // SAFETY: this test binary links no runtime, and all native code was built
    // by this workspace. The temporary directory is private and immutable.
    let host = unsafe { NativeHost::open(&runtime, "test-build", "test-target") }.unwrap();
    let mut incompatible = manifest.clone();
    incompatible.compiler = "another-build".into();
    // SAFETY: the trusted test artefact is never modified during admission.
    assert!(unsafe { host.load(&module, &incompatible) }.is_err());
    // SAFETY: the generated module has no initialization or finalization effects.
    let first = unsafe { host.load(&module, &manifest) }.unwrap();
    exercise_owned_state(&host, &first);
    // SAFETY: same trusted generation and immutable manifest as above.
    let second = unsafe { host.load(&module, &manifest) }.unwrap();
    assert!(
        std::ptr::eq(first.code_lease(), second.code_lease()),
        "repeated activation reuses the defining-code owner"
    );
    drop(second);
    let other_object = compile(
        directory.path(),
        &include_str!("counter.hew").replace("total: 40", "total: 50"),
        "other",
        &[
            "create=counter_create",
            "read=counter_read",
            "step=counter_step",
        ],
    );
    let other_module = directory.path().join("other.so");
    let other_manifest = prepare(
        &other_object,
        &runtime,
        &other_module,
        "test-build",
        "test-target",
    )
    .unwrap();
    // SAFETY: distinct trusted generation prepared above.
    let second = unsafe { host.load(&other_module, &other_manifest) }.unwrap();
    assert!(!std::ptr::eq(first.code_lease(), second.code_lease()));
    first.depends_on(&second).unwrap();
    second.depends_on(&first).unwrap();
    drop(first);
    assert!(
        host.collect().unwrap().is_empty(),
        "a rooted dependency cycle remains executable"
    );
    drop(second);
    assert_eq!(
        host.collect().unwrap().len(),
        2,
        "an unreachable dependency cycle is reclaimable"
    );
    // The host is reusable, and invocations above perform no compilation.
    // SAFETY: same trusted prepared module.
    let third = unsafe { host.load(&module, &manifest) }.unwrap();
    exercise_typed_owner(&host, third);
}

fn exercise_typed_owner(host: &NativeHost, generation: hew_live::native::Generation) {
    type NewValue = unsafe extern "C" fn(*const hew_cabi::value::HewValueLayout) -> *mut c_void;
    type ForgetValue = unsafe extern "C" fn(*mut c_void);
    let slot = hew_cabi::code::HewCodeLeaseSlot {
        lease: generation.code_lease(),
    };
    let layout = hew_cabi::value::HewValueLayout {
        size: 8,
        align: 8,
        ownership_kind: hew_cabi::value::HewTypeOwnershipKind::Plain,
        clone_fn: None,
        drop_fn: None,
        release_start: None,
        code_owner: &raw const slot,
        managed: core::ptr::null(),
    };
    // SAFETY: this same-build descriptor is pinned by the generation handle
    // until the runtime acquires its own defining-code root.
    let owner = unsafe {
        host.runtime_symbol::<NewValue>(b"hew_host_value_new")
            .unwrap()(&raw const layout)
    };
    drop(generation);
    assert!(
        host.collect().unwrap().is_empty(),
        "a typed owner retains its defining code after the host handle is gone"
    );
    // SAFETY: no semantic value was initialized in the unique owner.
    unsafe {
        host.runtime_symbol::<ForgetValue>(b"hew_host_value_forget")
            .unwrap()(owner);
    };
    assert_eq!(host.collect().unwrap().len(), 1);
}

fn exercise_owned_state(host: &NativeHost, generation: &hew_live::native::Generation) {
    // SAFETY: every pointer follows the compiler-generated header. The generation
    // root remains alive until the opaque value is explicitly released.
    unsafe {
        type Create = unsafe extern "C" fn(*mut *mut c_void, *mut *mut c_void) -> i32;
        type Read = unsafe extern "C" fn(*const c_void, *mut i64, *mut *mut c_void) -> i32;
        type Step =
            unsafe extern "C" fn(*mut *mut c_void, *mut *mut c_void, *mut *mut c_void) -> i32;
        type Release = unsafe extern "C" fn(*mut c_void, *mut *mut c_void) -> i32;
        type RuntimeNew = unsafe extern "C" fn() -> *mut c_void;
        type RuntimeRelease = unsafe extern "C" fn(*mut c_void);
        let runtime_new = host
            .runtime_symbol::<RuntimeNew>(b"hew_runtime_new")
            .unwrap();
        let runtime_release = host
            .runtime_symbol::<RuntimeRelease>(b"hew_runtime_release")
            .unwrap();
        let runtime = runtime_new();
        let create = generation.symbol::<Create>(b"counter_create").unwrap();
        let read = generation.symbol::<Read>(b"counter_read").unwrap();
        let step = generation.symbol::<Step>(b"counter_step").unwrap();
        let release = host
            .runtime_symbol::<Release>(b"hew_host_value_release")
            .unwrap();
        let mut owner = std::ptr::null_mut();
        let mut error = std::ptr::null_mut();
        assert_eq!(create(&raw mut owner, &raw mut error), 0);
        assert!(!owner.is_null());
        for expected in [41, 43, 46] {
            let mut next = std::ptr::null_mut();
            assert_eq!(step(&raw mut owner, &raw mut next, &raw mut error), 0);
            assert!(
                owner.is_null(),
                "consuming calls clear the previous ownership slot"
            );
            owner = next;
            let mut value = 0;
            assert_eq!(read(owner, &raw mut value, &raw mut error), 0);
            assert_eq!(value, expected);
            assert!(host.collect().unwrap().is_empty());
        }
        assert_eq!(release(owner, &raw mut error), 0);
        assert!(error.is_null());
        runtime_release(runtime);
    }
}
