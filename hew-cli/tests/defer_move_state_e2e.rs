//! End-to-end coverage for move-state replay at defer materialization edges.

mod support;

use std::process::{Command, Output};

use support::{hew_binary, strip_ansi};

fn check_source(name: &str, source: &str) -> Output {
    let fixture = support::tempdir();
    let source_path = fixture.path().join(name);
    std::fs::write(&source_path, source).expect("write Hew fixture");
    Command::new(hew_binary())
        .args(["check", source_path.to_str().expect("UTF-8 fixture path")])
        .current_dir(fixture.path())
        .output()
        .expect("hew check must run")
}

const RESOURCE_DECL: &str = r"
#[resource]
type Conn {
    fd: i64,
}

impl Conn {
    fn id(self) -> i64 {
        self.fd
    }

    fn close(self) {}
}
";

#[test]
fn defer_reads_recheck_unconditional_conditional_and_return_edges() {
    let source = format!(
        r#"{RESOURCE_DECL}
fn unconditional() {{
    let unconditional_conn = Conn {{ fd: 1 }};
    defer println(f"{{unconditional_conn.id()}}");
    unconditional_conn.close();
}}

fn conditional(take: bool) {{
    let conditional_conn = Conn {{ fd: 2 }};
    defer println(f"{{conditional_conn.id()}}");
    if take {{
        conditional_conn.close();
    }}
}}

fn early_return(leave: bool) {{
    let return_conn = Conn {{ fd: 3 }};
    defer println(f"{{return_conn.id()}}");
    if leave {{
        return_conn.close();
        return;
    }}
    println("normal exit keeps return_conn live");
}}

fn main() {{}}
"#
    );
    let output = check_source("defer_moved.hew", &source);
    assert!(!output.status.success(), "moved defer captures must reject");

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    let bindings = ["unconditional_conn", "conditional_conn", "return_conn"];
    let missing_diagnostics: Vec<_> = bindings
        .iter()
        .filter(|name| !stderr.contains(&format!("error: use of moved value `{name}`")))
        .copied()
        .collect();
    assert!(
        missing_diagnostics.is_empty(),
        "missing inline-quality move diagnostics for {missing_diagnostics:?}:\n{stderr}"
    );
    for name in bindings {
        assert!(
            stderr.contains(&format!(
                "duplicate `{name}` with `clone {name}` before the consuming use"
            )),
            "missing inline-quality help for {name}:\n{stderr}"
        );
    }
    assert_eq!(
        stderr.matches("note: value was consumed here").count(),
        3,
        "each materialization failure must point at its own consume edge:\n{stderr}"
    );
    assert!(
        stderr.contains("conditional_conn.close();"),
        "conditional rejection must name the consuming branch:\n{stderr}"
    );
    assert!(
        stderr.contains("return_conn.close();"),
        "return-edge rejection must name its consuming branch:\n{stderr}"
    );
}

#[test]
fn defer_reads_compile_when_every_materialization_edge_is_live() {
    let source = format!(
        r#"{RESOURCE_DECL}
fn live_on_every_edge(leave: bool) {{
    let edge_conn = Conn {{ fd: 1 }};
    defer println(f"{{edge_conn.id()}}");
    if leave {{
        return;
    }}
    println("normal exit");
}}

fn resource_is_not_consumed() {{
    let live_conn = Conn {{ fd: 2 }};
    defer println(f"{{live_conn.id()}}");
    println("body leaves resource live");
}}

fn main() {{
    live_on_every_edge(true);
    live_on_every_edge(false);
    resource_is_not_consumed();
}}
"#
    );
    let output = check_source("defer_live.hew", &source);
    assert!(
        output.status.success(),
        "live defer captures must compile:\n{}",
        strip_ansi(&String::from_utf8_lossy(&output.stderr))
    );
}

#[test]
fn defer_registration_still_rejects_forward_references() {
    let output = check_source(
        "defer_forward.hew",
        r#"fn main() {
    defer println(f"{later}");
    let later = 5;
}
"#,
    );
    assert!(
        !output.status.success(),
        "forward defer capture must reject"
    );

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        stderr.contains("error: undefined variable `later`"),
        "registration must retain lexical scope checking:\n{stderr}"
    );
    assert!(
        !stderr.contains("use of moved value"),
        "forward-reference rejection must not be misreported as a move:\n{stderr}"
    );
}
