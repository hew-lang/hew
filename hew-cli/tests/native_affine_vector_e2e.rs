//! Vector mutations preserve value copies and drain transferred affine owners.

mod support;

use std::process::Command;
use support::{describe_output, hew_binary, require_codegen, run_bounded_command, tempdir};

const GENERATOR: &str = r#"
gen fn held(label: string) -> string {
    defer println(label);
    yield label;
    yield "later";
}
fn started(label: string) -> Generator<string, ()> {
    let values = held(label);
    let _first = await values.next();
    values
}
"#;

fn run_vector(source: &str, check: impl Fn(&str)) {
    require_codegen();
    let directory = tempdir();
    let input = directory.path().join("vectors.hew");
    std::fs::write(&input, source).unwrap();
    for opt in ["0", "2"] {
        let binary =
            hew_testutil::compiled_binary_path(directory.path(), &format!("vectors-{opt}"));
        let mut build = Command::new(hew_binary());
        build
            .arg("build")
            .arg(&input)
            .args(["--opt-level", opt, "-o"])
            .arg(&binary);
        let output = run_bounded_command(build, format!("build affine vector O{opt}"));
        assert!(output.status.success(), "{}", describe_output(&output));
        let output = run_bounded_command(Command::new(binary), format!("run affine vector O{opt}"));
        assert!(output.status.success(), "{}", describe_output(&output));
        assert!(output.stderr.is_empty(), "{}", describe_output(&output));
        check(&String::from_utf8_lossy(&output.stdout));
    }
}

#[test]
fn affine_vector_push_set_clear_and_drop_close_each_owner_once() {
    run_vector(
        &format!(
            "{GENERATOR}{}",
            r#"
fn main() {
    var values: Vec<Generator<string, ()>> = [];
    values.push(await started("first"));
    values.push(await started("second"));
    values.set(0, await started("third"));
    println("set");
    values.clear();
    println("clear");
    println(values.len());
    values.push(await started("dropped"));
}
"#
        ),
        |output| assert_eq!(output, "first\nset\nthird\nsecond\nclear\n0\ndropped\n"),
    );
}

#[test]
fn affine_vector_failed_set_closes_receiver_and_untransferred_element() {
    run_vector(
        &format!(
            "{GENERATOR}{}",
            r#"
fn main() {
    scope {
        var values: Vec<Generator<string, ()>> = [];
        values.push(await started("stored"));
        values.set(99, await started("replacement"));
        println("unreachable");
    } handle failure { println("caught"); };
}
"#
        ),
        |output| {
            let lines = output.lines().collect::<Vec<_>>();
            assert_eq!(lines.len(), 3, "{output}");
            assert_eq!(lines[2], "caught", "{output}");
            assert_eq!(lines.iter().filter(|line| **line == "stored").count(), 1);
            assert_eq!(
                lines.iter().filter(|line| **line == "replacement").count(),
                1
            );
        },
    );
}

#[test]
fn affine_vector_parent_cancellation_closes_elements_before_recovery() {
    run_vector(
        &format!(
            "{GENERATOR}{}",
            r#"
fn main() {
    let item = await started("cancelled owner");
    scope within 50ms {
        var values: Vec<Generator<string, ()>> = [];
        values.push(item);
        await sleep(1s);
        println("unreachable");
    } handle failure { println("caught"); };
}
"#
        ),
        |output| assert_eq!(output, "cancelled owner\ncaught\n"),
    );
}

#[test]
fn ordinary_vector_insertion_and_replacement_preserve_value_copies() {
    run_vector(
        r#"
fn main() {
    let original = "original".to_upper();
    var values: Vec<string> = [];
    values.push(original);
    let independent = values;
    values.set(0, "replacement".to_upper());
    values.clear();
    println(original);
    println(independent[0]);
    println(values.len());
}

"#,
        |output| assert_eq!(output, "ORIGINAL\nORIGINAL\n0\n"),
    );
}

#[test]
fn affine_vector_cleanup_fault_stops_the_mutation_continuation() {
    run_vector(
        r#"
gen fn failing() -> i64 {
    defer panic("element cleanup");
    yield 1;
}
fn main() {
    scope {
        let item = failing();
        let _first = await item.next();
        var values: Vec<Generator<i64, ()>> = [];
        values.push(item);
        values.clear();
        println("unreachable");
    } handle failure {
        match failure {
            .Fault { message } => println(message),
            .Deadline { message } => panic("unexpected deadline"),
        }
        println("caught");
    };
}
"#,
        |output| {
            assert!(output.contains("element cleanup"), "{output}");
            assert!(!output.contains("unreachable"), "{output}");
            assert!(output.ends_with("caught\n"), "{output}");
        },
    );
}

#[test]
fn affine_vector_record_field_mutations_preserve_the_enclosing_owner() {
    run_vector(
        &format!(
            "{GENERATOR}{}",
            r#"
type Holder { values: Vec<Generator<string, ()>>, label: string, }
fn main() {
    var holder = Holder { values: [], label: "holder" };
    holder.values.push(await started("field"));
    holder.values.set(0, await started("replacement"));
    println(holder.label);
    holder.values.clear();
    println(holder.values.len());
}
"#
        ),
        |output| assert_eq!(output, "field\nholder\nreplacement\n0\n"),
    );
}
