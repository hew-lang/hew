//! Functional-update ownership fixtures.
//!
//! `T { field: new, ..base }` fills every field the update leaves unnamed from
//! `base`. Ownership SIR decides how each carried field arrives:
//!
//! * A live base - a binding, a projection or an index of one - is only read.
//!   Every carried field is an independent copy, so the base stays usable and
//!   the same base can feed any number of updates.
//! * A temporary base, or a base with a carried field that has no copy
//!   operation (a closure field, for instance), is consumed. Its carried
//!   fields transfer into the new record and its overridden owned fields are
//!   released at the construction site.
//! * A borrowed base cannot give up a non-copyable field: the update is
//!   refused with `E_OWN_CONSUME_BORROWED` instead of copying what cannot be
//!   copied.
//!
//! The leak-freedom of both forms is pinned by the `record-update-*` cases in
//! `tests/core-acceptance` under ASan/LSan; these fixtures pin the observable
//! source behaviour through `hew run` and `hew check`.

#![cfg(unix)]

mod support;

use std::process::Command;

use support::{hew_binary, repo_root, require_codegen};

/// Run `hew <command> <source>` and return `(success, combined_output)`.
fn hew(command: &str, source: &str) -> (bool, String) {
    let dir = tempfile::tempdir().expect("tempdir");
    let src = dir.path().join("fixture.hew");
    std::fs::write(&src, source).expect("write source");
    let output = Command::new(hew_binary())
        .arg(command)
        .arg(&src)
        .current_dir(repo_root())
        .output()
        .expect("invoke hew");
    let mut combined = String::from_utf8_lossy(&output.stdout).into_owned();
    combined.push_str(&String::from_utf8_lossy(&output.stderr));
    (output.status.success(), combined)
}

fn assert_runs(source: &str, expected_stdout: &str) {
    require_codegen();
    let (ok, out) = hew("run", source);
    assert!(ok, "fixture must run cleanly; got:\n{out}");
    assert_eq!(out, expected_stdout);
}

#[test]
fn live_base_stays_usable_after_any_number_of_updates() {
    assert_runs(
        r#"
type VHolder { items: Vec<i64>, tag: string }
fn main() {
    var init: Vec<i64> = Vec.new(); init.push(1);
    let base = VHolder { items: init, tag: "base" };
    var next1: Vec<i64> = Vec.new(); next1.push(2); next1.push(3);
    let updated1 = VHolder { items: next1, ..base };
    let updated2 = VHolder { tag: "two", ..base };
    let aliased = VHolder { items: base.items, ..base };
    println(f"{updated1.items.len()} {updated1.tag}");
    println(f"{updated2.items.len()} {updated2.tag}");
    println(f"{aliased.items.len()} {aliased.tag}");
    println(f"{base.items.len()} {base.tag}");
}
"#,
        "2 base\n1 two\n1 base\n1 base\n",
    );
}

#[test]
fn projection_and_index_bases_copy_and_leave_their_owner_intact() {
    assert_runs(
        r#"
type Inner { label: string, n: i64 }
type Mid { inner: Inner, k: i64 }
type Outer { inner: Inner, pair: (Inner, i64), items: Vec<Mid>, tag: string }
fn main() {
    var v: Vec<Mid> = Vec.new();
    v.push(Mid { inner: Inner { label: "indexed", n: 7 }, k: 3 });
    let o = Outer {
        inner: Inner { label: "inner", n: 1 },
        pair: (Inner { label: "paired", n: 2 }, 5),
        items: v,
        tag: "outer",
    };
    let t = (Inner { label: "tuple", n: 3 }, 9);
    let a = Inner { label: "a", ..o.inner };
    let b = Inner { label: "b", ..t.0 };
    let c = Inner { label: "c", ..o.pair.0 };
    let d = Inner { label: "d", ..o.items[0].inner };
    println(f"{a.label}:{a.n} {b.label}:{b.n} {c.label}:{c.n} {d.label}:{d.n}");
    println(f"{o.inner.label} {t.0.label} {o.pair.0.label} {o.items[0].inner.label} {o.tag}");
}
"#,
        "a:1 b:3 c:2 d:7\ninner tuple paired indexed outer\n",
    );
}

#[test]
fn temporary_bases_are_consumed_in_every_value_position() {
    assert_runs(
        r#"
type Inner { label: string, n: i64 }
type Outer { inner: Inner, tag: i64 }
fn makeInner() -> Inner { Inner { label: "made", n: 1 } }
fn makeOther() -> Inner { Inner { label: "other", n: 2 } }
fn makeOuter() -> Outer { Outer { inner: Inner { label: "nested", n: 3 }, tag: 9 } }
fn labelled(s: string) -> Inner { Inner { label: s, n: 4 } }
fn main() {
    let flag = true;
    let a = Inner { label: "a", ..makeInner() };
    let b = Inner { label: "b", ..makeOuter().inner };
    let c = Inner { label: "c", ..if flag { makeInner() } else { makeOther() } };
    let d = Inner { label: "d", ..match flag { true => makeOther(), false => makeInner() } };
    let e = Inner { label: "e", ..{ println("seed"); makeInner() } };
    let seed = "q";
    let f = Inner { label: "f", ..labelled(seed) };
    let base = makeInner();
    let moved = base;
    let g = Inner { label: "g", ..moved };
    let build = |s: string| -> Inner {
        let local = labelled(s);
        Inner { label: "h", ..local }
    };
    let h = build("z");
    println(f"{a.label}:{a.n} {b.label}:{b.n} {c.label}:{c.n} {d.label}:{d.n} {e.label}:{e.n}");
    println(f"{f.label}:{f.n} {seed} {g.label}:{g.n} {h.label}:{h.n}");
}
"#,
        "seed\na:1 b:3 c:1 d:2 e:1\nf:4 q g:1 h:4\n",
    );
}

#[test]
fn carried_records_tuples_and_collections_arrive_whole() {
    assert_runs(
        r#"
type Inner { label: string, n: i64 }
type Nested { inner: Inner, tag: string }
type Pairs { pair: (string, (string, i64)), twice: ((string, i64), (string, i64)), tag: string }
type Bag { items: (Vec<string>, i64), record: (Inner, i64), tag: string }
fn nested() -> Nested {
    let b = Nested { inner: Inner { label: "inner", n: 1 }, tag: "x" };
    Nested { tag: "y", ..b }
}
fn pairs() -> Pairs {
    let b = Pairs { pair: ("k", ("m", 7)), twice: (("k1", 1), ("m2", 2)), tag: "x" };
    Pairs { tag: "y", ..b }
}
fn bag() -> Bag {
    var v: Vec<string> = Vec.new();
    v.push("vec");
    let b = Bag { items: (v, 7), record: (Inner { label: "rec", n: 1 }, 7), tag: "x" };
    Bag { tag: "y", ..b }
}
fn main() {
    let n = nested();
    println(f"{n.tag} {n.inner.label} {n.inner.n}");
    let p = pairs();
    let first = p.twice.0;
    let inner_pair = p.pair.1;
    println(f"{p.tag} {p.pair.0} {inner_pair.0} {inner_pair.1} {first.0} {first.1}");
    let b = bag();
    let items = b.items.0;
    match items.get(0) {
        .Some(head) => println(f"{b.tag} {head} {b.record.0.label}"),
        .None => println("empty"),
    }
}
"#,
        "y inner 1\ny k m 7 k1 1\ny vec rec\n",
    );
}

#[test]
fn reassign_loop_idiom_keeps_one_live_owner() {
    assert_runs(
        r"
type VecHolder { items: Vec<i64>, tag: i64 }
fn main() {
    var init: Vec<i64> = Vec.new(); init.push(99);
    var h = VecHolder { items: init, tag: 0 };
    var i: i64 = 0;
    while i < 8 {
        var next: Vec<i64> = Vec.new(); next.push(i);
        h = VecHolder { items: next, ..h };
        i = i + 1;
    }
    println(h.items.len());
}
",
        "1\n",
    );
}

#[test]
fn non_copyable_field_consumes_its_binding_base() {
    assert_runs(
        r#"
type Job { name: string, run: fn() -> string, retries: i64 }
fn make(name: string, answer: string) -> Job {
    Job { name: name, run: || -> string { answer }, retries: 0 }
}
fn renamed(consume job: Job) -> Job {
    Job { name: "renamed", ..job }
}
fn main() {
    let first = Job { retries: 3, ..make("first", "one") };
    let seed = make("seed", "two");
    let second = Job { name: "second", ..seed };
    let third = renamed(make("third", "three"));
    let run_first = first.run;
    let run_second = second.run;
    let run_third = third.run;
    println(f"{first.name}:{first.retries}:{run_first()}");
    println(f"{second.name}:{second.retries}:{run_second()}");
    println(f"{third.name}:{third.retries}:{run_third()}");
}
"#,
        "first:3:one\nsecond:0:two\nrenamed:0:three\n",
    );
}

#[test]
fn borrowed_base_cannot_give_up_a_non_copyable_field() {
    let (ok, out) = hew(
        "check",
        r#"
type Job { name: string, run: fn() -> string }
fn renamed(job: Job) -> Job {
    Job { name: "renamed", ..job }
}
fn main() {
    let job = Job { name: "job", run: || -> string { "answer" } };
    let other = renamed(job);
    println(other.name);
}
"#,
    );
    assert!(
        !ok,
        "a borrowed base must not transfer its closure field:\n{out}"
    );
    assert!(
        out.contains("E_OWN_CONSUME_BORROWED"),
        "expected the borrowed-transfer refusal; got:\n{out}"
    );
}
