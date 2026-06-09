//! Tests for HIR actor declaration lowering. Lane A: structural only —
//! method, receive, and lifecycle-hook bodies are not lowered to `HirBlock`
//! in this slice (see `HirActorDecl` doc comment).

use hew_hir::{lower_program, HirActorDecl, HirItem, HirLifecycleHookKind, ResolutionCtx};
use hew_parser::ast::OverflowPolicy;
use hew_types::TypeCheckOutput;

fn lower(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    lower_program(&parsed.program, &TypeCheckOutput::default(), &ResolutionCtx)
}

fn find_actor<'a>(output: &'a hew_hir::LowerOutput, name: &str) -> &'a HirActorDecl {
    output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Actor(a) if a.name == name => Some(a),
            _ => None,
        })
        .unwrap_or_else(|| panic!("expected actor `{name}` in lowered module"))
}

#[test]
fn actor_decl_lowering_happy_path() {
    // Logger: one state field, one receive handler, no lifecycle hooks, no init.
    let src = r#"
actor Logger {
    let label: string;

    receive fn log(msg: string) {
        println(f"[{label}] {msg}");
    }
}

fn main() {}
"#;
    let output = lower(src);
    assert!(
        output.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        output.diagnostics
    );
    let actor = find_actor(&output, "Logger");
    assert_eq!(actor.state_fields.len(), 1);
    assert_eq!(actor.state_fields[0].name, "label");
    assert_eq!(actor.receive_handlers.len(), 1);
    assert_eq!(actor.receive_handlers[0].name, "log");
    assert_eq!(actor.receive_handlers[0].params.len(), 1);
    assert_eq!(actor.receive_handlers[0].every_ns, None);
    assert!(actor.init.is_none());
    assert!(actor.methods.is_empty());
    assert!(actor.lifecycle_hooks.is_empty());
    assert!(!actor.is_isolated);
    assert_eq!(actor.max_heap_bytes, None);
    assert_eq!(actor.mailbox_capacity, None);
    assert!(actor.overflow_policy.is_none());
    assert!(!actor.cycle_capable);
}

#[test]
fn actor_lifecycle_hooks_attributed_separately() {
    // Service with one regular method and one of each lifecycle hook kind.
    let src = r"
actor Service {
    receive fn handle() {
        process();
    }

    #[on(start)]
    fn boot() {
        warm();
    }

    #[on(stop)]
    fn cleanup() {
        flush();
    }

    #[on(stop)]
    fn drain() {
        finish();
    }

    fn helper() {
        do_work();
    }
}

fn main() {}
";
    let output = lower(src);
    assert!(
        output.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        output.diagnostics
    );
    let actor = find_actor(&output, "Service");

    // One receive handler.
    assert_eq!(actor.receive_handlers.len(), 1);
    assert_eq!(actor.receive_handlers[0].name, "handle");

    // Only `helper` is a plain method; the three `#[on(...)]` fns are hooks.
    let method_names: Vec<&str> = actor.methods.iter().map(|m| m.name.as_str()).collect();
    assert_eq!(
        method_names,
        vec!["helper"],
        "lifecycle-hook fns must not appear in methods; got {method_names:?}"
    );

    // Three lifecycle hooks: Start (boot), Stop (cleanup), Stop (drain).
    assert_eq!(actor.lifecycle_hooks.len(), 3);
    let hook_kinds: Vec<HirLifecycleHookKind> =
        actor.lifecycle_hooks.iter().map(|h| h.kind).collect();
    assert_eq!(
        hook_kinds,
        vec![
            HirLifecycleHookKind::Start,
            HirLifecycleHookKind::Stop,
            HirLifecycleHookKind::Stop,
        ],
        "lifecycle hooks preserve source order"
    );
    assert_eq!(actor.lifecycle_hooks[0].name, "boot");
    assert_eq!(actor.lifecycle_hooks[1].name, "cleanup");
    assert_eq!(actor.lifecycle_hooks[2].name, "drain");
}

#[test]
fn actor_init_and_every_attribute_lower() {
    let src = r"
actor Worker {
    let counter: i64;

    init(start: i64) {
        counter = start;
    }

    #[every(50ms)]
    receive fn tick() {
        work();
    }

    receive fn bump() {
        counter = counter + 1;
    }
}

fn main() {}
";
    let output = lower(src);
    assert!(
        output.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        output.diagnostics
    );
    let actor = find_actor(&output, "Worker");

    // init block present, with one param.
    let init = actor.init.as_ref().expect("init block expected");
    assert_eq!(init.params.len(), 1);
    assert_eq!(init.params[0].name, "start");

    // Two receive handlers; the `tick` one carries every_ns.
    assert_eq!(actor.receive_handlers.len(), 2);
    let tick = actor
        .receive_handlers
        .iter()
        .find(|r| r.name == "tick")
        .expect("tick receive fn expected");
    assert_eq!(
        tick.every_ns,
        Some(50_000_000),
        "#[every(50ms)] lowers to 50_000_000 ns"
    );
    let bump = actor
        .receive_handlers
        .iter()
        .find(|r| r.name == "bump")
        .expect("bump receive fn expected");
    assert_eq!(bump.every_ns, None);
}

#[test]
fn actor_mailbox_and_overflow_lower() {
    let src = r"
actor Bounded {
    mailbox 16
    overflow drop_old

    receive fn ping() {
        ack();
    }
}

fn main() {}
";
    let output = lower(src);
    assert!(
        output.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        output.diagnostics
    );
    let actor = find_actor(&output, "Bounded");
    assert_eq!(actor.mailbox_capacity, Some(16));
    assert_eq!(actor.overflow_policy, Some(OverflowPolicy::DropOld));
}

#[test]
fn actor_max_heap_lower() {
    let src = r"
#[max_heap(64 kb)]
actor Cache {
    receive fn get() {
        lookup();
    }
}

fn main() {}
";
    let output = lower(src);
    assert!(
        output.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        output.diagnostics
    );
    let actor = find_actor(&output, "Cache");
    assert_eq!(actor.max_heap_bytes, Some(64 * 1024));
}
