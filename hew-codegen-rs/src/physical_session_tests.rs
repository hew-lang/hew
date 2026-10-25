//! Compiled submissions share slots in a native host with one shared runtime.

use super::*;

#[test]
fn native_session_places_host_retains_mutations_and_fault_occupancy() {
    use std::io::{BufRead, BufReader, Write};
    use std::net::TcpListener;
    use std::process::{Command, Stdio};

    use hew_compile::live::frontend::{InputKind, LiveFrontend};
    use serde_json::{json, Value};

    struct Worker(Option<std::process::Child>);

    impl Drop for Worker {
        fn drop(&mut self) {
            if let Some(child) = &mut self.0 {
                let _ = child.kill();
                let _ = child.wait();
            }
        }
    }

    fn exchange(connection: &mut BufReader<std::net::TcpStream>, request: Value) -> Value {
        serde_json::to_writer(connection.get_mut(), &request).unwrap();
        connection.get_mut().write_all(b"\n").unwrap();
        connection.get_mut().flush().unwrap();
        let mut line = String::new();
        assert!(connection.read_line(&mut line).unwrap() > 0);
        serde_json::from_str(&line).unwrap()
    }

    let directory = tempfile::tempdir().unwrap();
    let binaries = std::env::current_exe()
        .unwrap()
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf();
    let runtime = binaries.join(if cfg!(target_os = "macos") {
        "libhew.dylib"
    } else {
        "libhew.so"
    });
    for (suffix, opt_level) in [("", OptLevel::O0), ("_o2", OptLevel::O2)] {
        let listener = TcpListener::bind((std::net::Ipv4Addr::LOCALHOST, 0)).unwrap();
        let mut child = Worker(Some(
            Command::new(binaries.join("hew-live-host"))
                .arg("--runtime")
                .arg(&runtime)
                .args(["--compiler", "session-test", "--target", "native-test"])
                .arg("--control")
                .arg(listener.local_addr().unwrap().to_string())
                .stdin(Stdio::null())
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn()
                .unwrap(),
        ));
        let (control, _) = listener.accept().unwrap();
        control.set_nodelay(true).unwrap();
        control
            .set_read_timeout(Some(std::time::Duration::from_secs(30)))
            .unwrap();
        let mut connection = BufReader::new(control);
        let mut frontend = LiveFrontend::new(
            directory.path().to_path_buf(),
            hew_compile::FrontendOptions::default(),
        );
        let compiler = hew_compile::Session::new(
            hew_compile::SessionTarget::native(),
            hew_compile::DiagnosticPolicy::default(),
        );
        for (id, (kind, input)) in [
            (InputKind::Declarations, "type Token { value: i64; } fn read_token(value: Token) -> i64 { value.value } #[resource] type Handle { value: i64; } impl Handle { fn close(consume self) { println(self.value); } } macro create { ($name:ident, $n:literal) => items { fn hidden() -> i64 { $n } fn $name() -> i64 { hidden() } } } create!(first, 3) create!(second, 7)"),
            (InputKind::Statements, "println(\"initialize\"); assert(first() == 3); assert(second() == 7); var count: i64 = 40; let text = \"retained\"; let old = Token { value: 7 }; let read = || read_token(old); let handle = Handle { value: 11 };"),
            (InputKind::Statements, "count += 2; panic(\"stop\");"),
            (InputKind::Declarations, "type Token { value: string; } fn read_token(value: Token) -> string { value.value }"),
            (InputKind::Statements, "let recent = Token { value: \"new\" }; assert(count == 42); assert(text == \"retained\"); assert(old.value == 7); assert(read() == 7); assert(read_token(recent) == \"new\"); count += 1;"),
            (InputKind::Statements, "assert(count == 43); assert(recent.value == \"new\"); let handle = Handle { value: 12 };"),
            (InputKind::Expression, "count"),
        ].into_iter().enumerate() {
            let prepared = frontend.prepare(&compiler, input, kind).unwrap();
            let name = format!("input_{id}");
            emit_live(directory.path(), &name, &prepared.live, true, &[(suffix, opt_level)]);
            let module = directory.path().join(format!("{name}{suffix}.so"));
            let manifest = hew_live::native::prepare(
                &directory.path().join(format!("{name}{suffix}.o")),
                &runtime,
                &module,
                "session-test",
                "native-test",
            )
            .unwrap();
            let request =
                json!({ "op": "prepare", "id": id, "module": module, "manifest": manifest });
            assert_eq!(
                exchange(&mut connection, request.clone()),
                json!({ "prepared": id })
            );
            if id == 0 {
                assert!(exchange(&mut connection, request)["error"]
                    .as_str()
                    .unwrap()
                    .contains("already prepared"));
            }
            let result = exchange(&mut connection, json!({ "op": "execute", "id": id, "entry": name, "slot_count": prepared.slot_count() }));
            let execution = &result["execution"];
            if id == 2 {
                assert!(execution["fault"]["message"].as_str().unwrap().contains("stop"), "{result}");
            } else {
                assert!(execution["fault"].is_null(), "{id}: {result}");
            }
            let initialized: Vec<_> = execution["slots"].as_array().unwrap().iter().map(|slot| slot["initialized"].as_bool().unwrap()).collect();
            assert_eq!(exchange(&mut connection, json!({ "op": "inspect" }))["slots"], execution["slots"]);
            let retired = frontend.commit(prepared, &initialized).unwrap();
            if id == 5 {
                assert_eq!(retired.len(), 1);
                let before = exchange(&mut connection, json!({ "op": "inspect" }));
                assert!(exchange(&mut connection, json!({ "op": "release", "slots": [retired[0], u32::MAX] }))["error"].is_string());
                assert_eq!(exchange(&mut connection, json!({ "op": "inspect" })), before);
                let released = exchange(&mut connection, json!({ "op": "release", "slots": retired }));
                assert_eq!(released["cleanup_faults"], json!([]));
                assert_eq!(released["slots"][retired[0] as usize]["initialized"], false);
            } else {
                assert!(retired.is_empty());
            }
        }
        assert!(exchange(
            &mut connection,
            json!({ "op": "execute", "id": 2, "entry": "input_2", "slot_count": 1 })
        )["error"]
            .as_str()
            .unwrap()
            .contains("cannot discard"));
        // The initial descriptor remains executable after its explicit root is
        // retired; the live String slot retains its defining generation.
        assert!(
            exchange(&mut connection, json!({ "op": "retire", "id": 1 }))["collected"]
                .as_array()
                .unwrap()
                .is_empty()
        );
        assert_eq!(
            exchange(&mut connection, json!({ "op": "clear" })),
            json!({ "cleanup_faults": [] })
        );
        assert_eq!(
            exchange(&mut connection, json!({ "op": "inspect" })),
            json!({ "slots": [] })
        );
        assert!(
            !exchange(&mut connection, json!({ "op": "collect" }))["collected"]
                .as_array()
                .unwrap()
                .is_empty()
        );
        assert_eq!(
            exchange(&mut connection, json!({ "op": "quit" })),
            json!({ "cleanup_faults": [] })
        );
        let output = child.0.take().unwrap().wait_with_output().unwrap();
        assert_eq!(
            String::from_utf8_lossy(&output.stdout),
            "initialize\n11\n43\n12\n"
        );
        assert!(
            output.status.success(),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );
    }
}

fn execute(directory: &Path, modules: &[&str]) {
    let runtime = std::env::current_exe()
        .unwrap()
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf();
    for suffix in ["", "_o2"] {
        let executable = directory.join(format!("host{suffix}"));
        let linked = std::process::Command::new("cc")
            .arg(directory.join("host.c"))
            .args(
                modules
                    .iter()
                    .map(|name| directory.join(format!("{name}{suffix}.o"))),
            )
            .arg(runtime.join("libhew.so"))
            .arg(format!("-Wl,-rpath,{}", runtime.display()))
            .arg("-o")
            .arg(&executable)
            .output()
            .unwrap();
        assert!(
            linked.status.success(),
            "{}",
            String::from_utf8_lossy(&linked.stderr)
        );
        let executed = std::process::Command::new(executable).output().unwrap();
        assert!(
            executed.status.success(),
            "{suffix}: {}",
            String::from_utf8_lossy(&executed.stderr)
        );
    }
}

fn compile(directory: &Path, name: &str, source: &str, inputs: &[u32], next: u32) {
    compile_with_ownership(directory, name, source, inputs, next, false);
}

fn compile_with_ownership(
    directory: &Path,
    name: &str,
    source: &str,
    inputs: &[u32],
    next: u32,
    code_owner: bool,
) {
    let mut parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    hew_compile::attach_prelude_std_modules(&mut parsed.program);
    let mut checker =
        hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(Vec::new()));
    let checked = checker.check_program(&parsed.program);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let roots = hew_compile::Session::source_roots(&parsed.program, &checked).unwrap();
    let session = hew_compile::Session::new(
        hew_compile::SessionTarget::native(),
        hew_compile::DiagnosticPolicy::default(),
    );
    let live = session
        .lower_live_program(&parsed.program, &checked, roots[0], inputs, next)
        .unwrap();
    emit_live(
        directory,
        name,
        &live,
        code_owner,
        &[("", OptLevel::O0), ("_o2", OptLevel::O2)],
    );
}

fn emit_live(
    directory: &Path,
    name: &str,
    live: &hew_compile::live::LiveSubmission,
    code_owner: bool,
    optimizations: &[(&str, OptLevel)],
) {
    let triple = native_emission_triple();
    let target = physical_target_for_inventory(
        &triple,
        &hew_mir::physical::physical_type_inventory(&live.output.semantics().module),
    )
    .unwrap();
    let physical = live.output.lower_physical(target).unwrap_or_else(|error| {
        panic!(
            "{name}: {error}\n{:#?}",
            live.output.semantics().module.functions
        )
    });
    let export = HostExport::new(&physical, &[(live.output.compiled_roots()[0], name)]).unwrap();
    let export = if code_owner {
        export.with_live_code_ownership()
    } else {
        export
    };
    for &(suffix, opt_level) in optimizations {
        emit_host_object(
            &export,
            &PhysicalEmitOptions {
                module_name: &format!("{name}{suffix}"),
                out_dir: directory,
                target_triple: Some(&triple),
                opt_level,
                emit_llvm: true,
                address_sanitizer: false,
                debug_source: None,
                debug_display_source: None,
                link_freestanding_wasm: false,
                build_info: None,
            },
        )
        .unwrap();
    }
}

#[test]
fn native_session_places_preserve_mutations_and_untouched_values_on_fault() {
    let directory = tempfile::tempdir().unwrap();
    compile(
        directory.path(),
        "initial",
        "pub fn submission() { var value: i64 = 40; let untouched: i64 = 7; }",
        &[],
        0,
    );
    compile(
        directory.path(),
        "failing",
        "pub fn submission(var value: i64, untouched: i64) { value += 2; panic(\"stop\"); }",
        &[0, 1],
        2,
    );
    compile(
        directory.path(),
        "resume",
        "pub fn submission(var value: i64, untouched: i64) { value += 1; }",
        &[0, 1],
        2,
    );
    let driver = directory.path().join("host.c");
    std::fs::write(
        &driver,
        r#"
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
typedef struct { void **slots; size_t len; } Session;
extern void *hew_runtime_new(void);
extern void hew_runtime_release(void *);
extern void *hew_live_slot_storage(Session *, size_t, const void *);
extern bool hew_live_slot_initialized(const Session *, size_t);
extern int hew_live_slot_release(Session *, size_t, void **);
extern void hew_host_error_release(void *);
extern int initial(void *, void **);
extern int failing(void *, void **);
extern int resume(void *, void **);
int main(void) {
    void *runtime = hew_runtime_new(), *slots[2] = {0}, *error = 0;
    Session session = { slots, 2 };
    assert(initial(&session, &error) == 0 && !error);
    assert(failing(&session, &error) != 0 && error);
    hew_host_error_release(error); error = 0;
    assert(hew_live_slot_initialized(&session, 0));
    assert(hew_live_slot_initialized(&session, 1));
    assert(*(int64_t *)hew_live_slot_storage(&session, 0, 0) == 42);
    assert(*(int64_t *)hew_live_slot_storage(&session, 1, 0) == 7);
    assert(resume(&session, &error) == 0 && !error);
    assert(*(int64_t *)hew_live_slot_storage(&session, 0, 0) == 43);
    assert(hew_live_slot_release(&session, 0, &error) == 0 && !error);
    assert(hew_live_slot_release(&session, 1, &error) == 0 && !error);
    hew_runtime_release(runtime);
}
"#,
    )
    .unwrap();
    execute(directory.path(), &["initial", "failing", "resume"]);
}

#[test]
fn native_session_places_fault_consumes_only_the_resource_actually_moved() {
    const TYPES: &str = r#"
        #[resource] pub type Handle { id: i64; }
        impl Handle {
            fn close(consume self) { unsafe { record_close(self.id); } }
        }
        extern "C" { fn record_close(id: i64); }
    "#;
    let directory = tempfile::tempdir().unwrap();
    compile(directory.path(), "initial", &format!("{TYPES} pub fn submission() {{ let taken = Handle {{ id: 1 }}; let untouched = Handle {{ id: 2 }}; }}"), &[], 0);
    compile(directory.path(), "failing", &format!("{TYPES} pub fn submission(consume taken: Handle, consume untouched: Handle) {{ taken.close(); panic(\"stop\"); }}"), &[0, 1], 2);
    let driver = directory.path().join("host.c");
    std::fs::write(
        &driver,
        r#"
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
typedef struct { void **slots; size_t len; } Session;
extern void *hew_runtime_new(void);
extern void hew_runtime_release(void *);
extern bool hew_live_slot_initialized(const Session *, size_t);
extern int hew_live_slot_release(Session *, size_t, void **);
extern void hew_host_error_release(void *);
extern int initial(void *, void **);
extern int failing(void *, void **);
static int closed = 0;
void record_close(int64_t id) { closed = closed * 10 + (int)id; }
int main(void) {
    void *runtime = hew_runtime_new(), *slots[2] = {0}, *error = 0;
    Session session = { slots, 2 };
    assert(initial(&session, &error) == 0 && !error && closed == 0);
    assert(failing(&session, &error) != 0 && error && closed == 1);
    hew_host_error_release(error); error = 0;
    assert(!hew_live_slot_initialized(&session, 0));
    assert(hew_live_slot_initialized(&session, 1));
    assert(hew_live_slot_release(&session, 0, &error) == 0 && !error && closed == 1);
    assert(hew_live_slot_release(&session, 1, &error) == 0 && !error && closed == 12);
    hew_runtime_release(runtime);
}
"#,
    )
    .unwrap();
    execute(directory.path(), &["initial", "failing"]);
}

#[test]
fn native_session_places_keep_mutable_closure_and_actor_state() {
    const ACTOR: &str = r#"
        actor Count {
            var total: i64;
            receive fn add(value: i64) -> i64 { total += value; total }
        }
    "#;
    let directory = tempfile::tempdir().unwrap();
    compile(
        directory.path(),
        "initial",
        &format!(
            r#"{ACTOR}
        pub fn submission() {{
            var next: fn[var]() -> i64 = {{ let count: i64 = 0; || {{ count += 1; count }} }};
            let counter = spawn Count {{ total: 40 }};
        }}
    "#
        ),
        &[],
        0,
    );
    let step = format!(
        r#"{ACTOR}
        pub fn submission(consume var next: fn[var]() -> i64, counter: Count) {{
            let result = counter.add(next()).expect("reply");
        }}
    "#
    );
    compile(directory.path(), "step", &step, &[0, 1], 2);
    compile(directory.path(), "again", &step, &[0, 1], 3);
    compile(
        directory.path(),
        "finish",
        &format!(
            "{ACTOR} pub fn submission(counter: Count) {{ stop(counter); stopped(counter); }}"
        ),
        &[1],
        4,
    );
    std::fs::write(directory.path().join("host.c"), r#"
#include <assert.h>
#include <stdint.h>
#include <stddef.h>
typedef struct { void **slots; size_t len; } Session;
extern void *hew_runtime_new(void);
extern void hew_runtime_release(void *);
extern void *hew_live_slot_storage(Session *, size_t, const void *);
extern int hew_live_slot_release(Session *, size_t, void **);
extern int initial(void *, void **), step(void *, void **), again(void *, void **), finish(void *, void **);
int main(void) {
    void *runtime = hew_runtime_new(), *slots[4] = {0}, *error = 0;
    Session session = { slots, 4 };
    assert(initial(&session, &error) == 0 && !error);
    assert(step(&session, &error) == 0 && !error);
    assert(*(int64_t *)hew_live_slot_storage(&session, 2, 0) == 41);
    assert(again(&session, &error) == 0 && !error);
    assert(*(int64_t *)hew_live_slot_storage(&session, 3, 0) == 43);
    assert(finish(&session, &error) == 0 && !error);
    for (size_t i = 0; i < 4; ++i) assert(hew_live_slot_release(&session, i, &error) == 0 && !error);
    hew_runtime_release(runtime);
}
"#).unwrap();
    execute(directory.path(), &["initial", "step", "again", "finish"]);
}

#[test]
fn native_session_places_retain_emitted_defining_code() {
    let directory = tempfile::tempdir().unwrap();
    compile_with_ownership(
        directory.path(),
        "initial",
        "pub fn submission() { let value: i64 = 40; let next = || value + 1; let answer = number; } fn number() -> i64 { 42 }",
        &[],
        0,
        true,
    );
    std::fs::write(
        directory.path().join("host.c"),
        r#"
#include <assert.h>
#include <stddef.h>
typedef struct { void **slots; size_t len; } Session;
typedef struct { void *context; void (*retain)(void *); void (*release)(void *); void *try_retain; } Lease;
extern const Lease *HewLiveCodeOwner;
extern void *hew_runtime_new(void);
extern void hew_runtime_release(void *);
extern int hew_live_slot_release(Session *, size_t, void **);
extern int initial(void *, void **);
static void retain(void *context) { ++*(int *)context; }
static void release(void *context) { assert(*(int *)context > 0); --*(int *)context; }
int main(void) {
    int roots = 0;
    Lease lease = { &roots, retain, release };
    HewLiveCodeOwner = &lease;
    void *runtime = hew_runtime_new(), *slots[3] = {0}, *error = 0;
    Session session = { slots, 3 };
    assert(initial(&session, &error) == 0 && !error);
    assert(roots == 5);
    assert(hew_live_slot_release(&session, 0, &error) == 0 && !error);
    assert(roots == 4);
    assert(hew_live_slot_release(&session, 1, &error) == 0 && !error);
    assert(roots == 2);
    assert(hew_live_slot_release(&session, 2, &error) == 0 && !error);
    assert(roots == 0);
    hew_runtime_release(runtime);
}
"#,
    )
    .unwrap();
    execute(directory.path(), &["initial"]);
}

#[test]
fn native_session_places_actor_retains_code_after_host_slot_release() {
    let directory = tempfile::tempdir().unwrap();
    compile_with_ownership(directory.path(), "initial",
        "actor Count { var total: i64; receive fn read() -> i64 { total } } pub fn submission() { let counter = spawn Count { total: 40 }; }",
        &[], 0, true);
    std::fs::write(
        directory.path().join("host.c"),
        r#"
#include <assert.h>
#include <stddef.h>
#include <stdatomic.h>
typedef struct { void **slots; size_t len; } Session;
typedef struct { void *context; void (*retain)(void *); void (*release)(void *); void *try_retain; } Lease;
extern const Lease *HewLiveCodeOwner;
extern void *hew_runtime_new(void);
extern void hew_runtime_release(void *);
extern int hew_live_slot_release(Session *, size_t, void **);
extern int initial(void *, void **);
static void retain(void *context) { atomic_fetch_add((atomic_int *)context, 1); }
static void release(void *context) { assert(atomic_fetch_sub((atomic_int *)context, 1) > 0); }
int main(void) {
    atomic_int roots = 0;
    Lease lease = { &roots, retain, release };
    HewLiveCodeOwner = &lease;
    void *runtime = hew_runtime_new(), *slots[1] = {0}, *error = 0;
    Session session = { slots, 1 };
    assert(initial(&session, &error) == 0 && !error);
    assert(atomic_load(&roots) >= 2);
    assert(hew_live_slot_release(&session, 0, &error) == 0 && !error);
    assert(atomic_load(&roots) >= 1);
    hew_runtime_release(runtime);
    assert(atomic_load(&roots) == 0);
}
"#,
    )
    .unwrap();
    execute(directory.path(), &["initial"]);
}

#[test]
fn native_session_places_queued_message_retains_its_producer_code() {
    let directory = tempfile::tempdir().unwrap();
    let declarations = r#"
extern "C" { fn wait_gate(); }
actor Count {
    receive fn block() { unsafe { wait_gate(); } }
    receive fn tick() {}
}
"#;
    compile_with_ownership(directory.path(), "initial", &format!(
        "{declarations} pub fn submission() {{ let counter = spawn Count; let _ = mailbox(counter, on_full: .Wait).block(); }}"), &[], 0, true);
    compile_with_ownership(directory.path(), "submit", &format!(
        "{declarations} pub fn submission(counter: Count) {{ let _ = mailbox(counter, on_full: .Wait).tick(); }}"), &[0], 1, true);
    std::fs::write(
        directory.path().join("host.c"),
        r#"
#include <assert.h>
#include <stddef.h>
#include <stdatomic.h>
#include <dlfcn.h>
#include <sched.h>
#include <unistd.h>
typedef struct { void **slots; size_t len; } Session;
typedef struct { void *context; void (*retain)(void *); void (*release)(void *); void *try_retain; } Lease;
extern void *hew_runtime_new(void);
extern void hew_runtime_release(void *);
extern int hew_live_slot_release(Session *, size_t, void **);
static atomic_int gate = 0, entered = 0;
void wait_gate(void) {
    atomic_store(&entered, 1);
    while (!atomic_load(&gate)) sched_yield();
}
static void retain(void *context) { atomic_fetch_add((atomic_int *)context, 1); }
static void release(void *context) { assert(atomic_fetch_sub((atomic_int *)context, 1) > 0); }
int main(int argc, char **argv) {
    assert(argc == 3);
    alarm(15);
    atomic_int roots_a = 0, roots_b = 0;
    Lease a = { &roots_a, retain, release }, b = { &roots_b, retain, release };
    void *first = dlopen(argv[1], RTLD_NOW | RTLD_LOCAL);
    void *second = dlopen(argv[2], RTLD_NOW | RTLD_LOCAL);
    assert(first && second);
    *(const Lease **)dlsym(first, "HewLiveCodeOwner") = &a;
    *(const Lease **)dlsym(second, "HewLiveCodeOwner") = &b;
    int (*initial)(void *, void **) = dlsym(first, "initial");
    int (*submit)(void *, void **) = dlsym(second, "submit");
    assert(initial && submit);
    void *runtime = hew_runtime_new(), *slots[1] = {0}, *error = 0;
    Session session = { slots, 1 };
    assert(initial(&session, &error) == 0 && !error);
    while (!atomic_load(&entered)) sched_yield();
    assert(submit(&session, &error) == 0 && !error);
    assert(atomic_load(&roots_b) > 0);
    atomic_store(&gate, 1);
    while (atomic_load(&roots_b)) sched_yield();
    assert(hew_live_slot_release(&session, 0, &error) == 0 && !error);
    hew_runtime_release(runtime);
    assert(atomic_load(&roots_a) == 0 && atomic_load(&roots_b) == 0);
    assert(dlclose(second) == 0 && dlclose(first) == 0);
}
"#,
    )
    .unwrap();
    execute_dynamic(directory.path());
}

fn execute_dynamic(directory: &Path) {
    let runtime = std::env::current_exe()
        .unwrap()
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf();
    for suffix in ["", "_o2"] {
        for name in ["initial", "submit"] {
            let linked = std::process::Command::new("cc")
                .args(["-shared", "-fPIC"])
                .arg(directory.join(format!("{name}{suffix}.o")))
                .arg(runtime.join("libhew.so"))
                .arg(format!("-Wl,-rpath,{}", runtime.display()))
                .arg("-o")
                .arg(directory.join(format!("{name}{suffix}.so")))
                .output()
                .unwrap();
            assert!(
                linked.status.success(),
                "{}",
                String::from_utf8_lossy(&linked.stderr)
            );
        }
        let executable = directory.join(format!("host{suffix}"));
        let linked = std::process::Command::new("cc")
            .args(["-rdynamic", "-ldl"])
            .arg(directory.join("host.c"))
            .arg(runtime.join("libhew.so"))
            .arg(format!("-Wl,-rpath,{}", runtime.display()))
            .arg("-o")
            .arg(&executable)
            .output()
            .unwrap();
        assert!(
            linked.status.success(),
            "{}",
            String::from_utf8_lossy(&linked.stderr)
        );
        let run = std::process::Command::new(executable)
            .arg(directory.join(format!("initial{suffix}.so")))
            .arg(directory.join(format!("submit{suffix}.so")))
            .output()
            .unwrap();
        assert!(
            run.status.success(),
            "{}",
            String::from_utf8_lossy(&run.stderr)
        );
    }
}
