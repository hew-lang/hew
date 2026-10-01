use super::check_source;
use crate::check::effects::SuspensionEffect;

fn suspends(output: &crate::TypeCheckOutput, declaration: &str) -> bool {
    output.suspension_effects.bodies.iter().any(|(body, effect)| {
        matches!(body, crate::check::effects::EffectBody::Declaration(id) if output.defs.path(*id) == declaration)
            && *effect == SuspensionEffect::MaySuspend
    })
}

fn offload_errors(output: &crate::TypeCheckOutput) -> Vec<&str> {
    output
        .errors
        .iter()
        .filter(|error| error.message.contains("[E_OFFLOAD_SIGNATURE]"))
        .map(|error| error.message.as_str())
        .collect()
}

#[test]
fn canonical_io_declarations_publish_suspension_without_symbol_alias_authority() {
    let source = "extern \"C\" { fn hew_stdin_read_line() -> bytes; } pub fn read() -> bytes { unsafe { hew_stdin_read_line() } }";
    let module = ["std".to_string(), "io".to_string()];
    let output = super::check_source_in_canonical_std_module(source, &module);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert!(output.direct_call_targets.values().any(|target| matches!(
        target,
        crate::CallTarget::Runtime(crate::RuntimeCallFamily::AsyncIo(
            crate::runtime_call::AsyncIoOp::StdinReadLine
        ))
    )));
    assert!(suspends(&output, "std.io.read"));
    let user = check_source(source);
    assert!(user.errors.is_empty(), "{:?}", user.errors);
    assert!(user.direct_call_targets.values().all(|target| !matches!(
        target,
        crate::CallTarget::Runtime(crate::RuntimeCallFamily::AsyncIo(_))
    )));
}

#[test]
fn offload_marks_a_user_extern_call_as_suspending_by_declaration() {
    let output = check_source(
        r#"
extern "C" {
    #[offload]
    fn slow_lookup(key: string, attempt: i64) -> string;
    fn fast_lookup(key: string) -> string;
}
fn offloaded() -> string { unsafe { slow_lookup("key", 1) } }
fn direct() -> string { unsafe { fast_lookup("key") } }
fn main() {}
"#,
    );
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    let offloaded: Vec<_> = output
        .direct_call_targets
        .values()
        .filter_map(|target| match target {
            crate::CallTarget::Extern { declaration, .. } => {
                Some(output.extern_contracts.is_offload(*declaration))
            }
            _ => None,
        })
        .collect();
    assert_eq!(offloaded.iter().filter(|offload| **offload).count(), 1);
    assert_eq!(offloaded.iter().filter(|offload| !**offload).count(), 1);
    let root = output
        .defs
        .root_module_path()
        .unwrap_or_default()
        .to_string();
    let path = |name: &str| {
        if root.is_empty() {
            name.to_string()
        } else {
            format!("{root}.{name}")
        }
    };
    assert!(suspends(&output, &path("offloaded")));
    assert!(!suspends(&output, &path("direct")));
}

#[test]
fn offload_refuses_signatures_the_job_cannot_own() {
    let output = check_source(
        r#"
#[opaque]
type Db {}

#[resource]
type Session {
    id: i64;
}

extern "C" {
    #[offload]
    fn query(db: Db, sql: string) -> i32;
    #[offload]
    fn finish(consume session: Session);
    #[offload]
    fn open(path: string) -> Db;
    #[offload]
    fn log_all(format: string ..);
    #[offload]
    fn copy_out(text: string, data: bytes, names: Vec<string>) -> Vec<string>;
}
fn main() {}
"#,
    );
    let errors = offload_errors(&output);
    assert!(
        errors
            .iter()
            .any(|error| error.contains("query") && error.contains("parameter 0")),
        "{errors:#?}"
    );
    assert!(
        errors
            .iter()
            .any(|error| error.contains("finish") && error.contains("`consume`")),
        "{errors:#?}"
    );
    assert!(
        errors
            .iter()
            .any(|error| error.contains("open") && error.contains("returns `Db`")),
        "{errors:#?}"
    );
    assert!(
        errors
            .iter()
            .any(|error| error.contains("log_all") && error.contains("variadic")),
        "{errors:#?}"
    );
    assert!(
        errors.iter().all(|error| !error.contains("copy_out")),
        "{errors:#?}"
    );
}

#[test]
fn the_same_symbol_without_offload_stays_a_direct_call() {
    let output = check_source(
        r#"
extern "C" {
    #[offload]
    fn hew_sleep_ns(ns: i64);
}
fn parked() { unsafe { hew_sleep_ns(1) } }
fn main() {}
"#,
    );
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    let root = output
        .defs
        .root_module_path()
        .unwrap_or_default()
        .to_string();
    let parked = if root.is_empty() {
        "parked".to_string()
    } else {
        format!("{root}.parked")
    };
    assert!(suspends(&output, &parked));
    let plain = check_source(
        r#"
extern "C" {
    fn hew_sleep_ns(ns: i64);
}
fn parked() { unsafe { hew_sleep_ns(1) } }
fn main() {}
"#,
    );
    assert!(plain.errors.is_empty(), "{:?}", plain.errors);
    assert!(!suspends(&plain, &parked));
}
