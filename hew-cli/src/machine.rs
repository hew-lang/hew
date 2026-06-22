//! `hew machine` — Extract and visualize state machines from Hew source files.
//!
//! Usage:
//!   hew machine diagram <file.hew>                      Mermaid state diagram (with HIR checks)
//!   hew machine diagram <file.hew> --format graphviz    Graphviz DOT output
//!   hew machine diagram <file.hew> --format dot         Alias for --format graphviz
//!   hew machine diagram <file.hew> --format json        JSON schema for tooling
//!   hew machine diagram <file.hew> --dot                Alias for --format graphviz (flag shorthand)
//!   hew machine diagram <file.hew> --machine `TrafficLight`  Filter to one machine
//!   hew machine diagram <file.hew> --no-check           Skip HIR static checks
//!   hew machine list <file.hew>                         List all machines with states/events

use hew_hir::{lower_program, HirItem, HirMachineDecl, ResolutionCtx};
use hew_parser::ast::{Item, MachineDecl};
use hew_types::TypeCheckOutput;

use crate::args::{MachineDiagramArgs, MachineFormat};

pub fn cmd_machine(args: &crate::args::MachineCommand) {
    match &args.command {
        crate::args::MachineSubcommand::Diagram(a) => {
            let path = a.input.display().to_string();
            cmd_diagram(&path, a);
        }
        crate::args::MachineSubcommand::List(a) => {
            let path = a.input.display().to_string();
            cmd_list(&path);
        }
    }
}

fn read_source(path: &str) -> String {
    match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading {path}: {e}");
            std::process::exit(1);
        }
    }
}

fn parse_machines(path: &str, source: &str) -> Vec<MachineDecl> {
    let result = hew_parser::parse(source);

    if !result.errors.is_empty() {
        for err in &result.errors {
            eprintln!("{path}: parse error: {err:?}");
        }
        std::process::exit(1);
    }

    result
        .program
        .items
        .into_iter()
        .filter_map(|(item, _)| {
            if let Item::Machine(md) = item {
                Some(md)
            } else {
                None
            }
        })
        .collect()
}

/// Run HIR lowering + static checks. Returns the checked HIR machines on success,
/// or exits the process on failure.
fn check_and_lower(path: &str, source: &str) -> Vec<HirMachineDecl> {
    let result = hew_parser::parse(source);

    if !result.errors.is_empty() {
        for err in &result.errors {
            eprintln!("{path}: parse error: {err:?}");
        }
        std::process::exit(1);
    }

    let lowered = lower_program(
        &result.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );

    if !lowered.diagnostics.is_empty() {
        for diag in &lowered.diagnostics {
            eprintln!("{path}: error: {}", diag.note);
        }
        std::process::exit(1);
    }

    lowered
        .module
        .items
        .into_iter()
        .filter_map(|item| {
            if let HirItem::Machine(m) = item {
                Some(m)
            } else {
                None
            }
        })
        .collect()
}

enum MachineCheckResult {
    Checked(Vec<HirMachineDecl>),
    AstFallback,
}

fn check_machines_or_ast_fallback(
    path: &str,
    source: &str,
    ast_machines: &[MachineDecl],
) -> MachineCheckResult {
    if ast_machines.iter().any(|m| !m.type_params.is_empty()) {
        eprintln!(
            "{path}: advisory: generic machine(s) skipping HIR checks — use --no-check to suppress"
        );
        return MachineCheckResult::AstFallback;
    }

    let hir_machines = check_and_lower(path, source);
    if hir_machines.is_empty() {
        eprintln!("No machines found in {path}");
        std::process::exit(1);
    }

    MachineCheckResult::Checked(hir_machines)
}

fn cmd_list(path: &str) {
    let source = read_source(path);
    let machines = parse_machines(path, &source);

    let _ = check_machines_or_ast_fallback(path, &source, &machines);

    for md in &machines {
        println!("machine {} {{", md.name);
        println!("  States:");
        for state in &md.states {
            if state.fields.is_empty() {
                println!("    {}", state.name);
            } else {
                let fields: Vec<String> =
                    state.fields.iter().map(|(name, _)| name.clone()).collect();
                println!("    {} {{ {} }}", state.name, fields.join(", "));
            }
        }
        println!("  Events:");
        for event in &md.events {
            if event.fields.is_empty() {
                println!("    {}", event.name);
            } else {
                let fields: Vec<String> =
                    event.fields.iter().map(|(name, _)| name.clone()).collect();
                println!("    {} {{ {} }}", event.name, fields.join(", "));
            }
        }
        println!("  Transitions: {}", md.transitions.len());
        if md.has_default {
            println!("  Default: unhandled events stay in current state");
        }
        // fix 3: Emits section in cmd_list.
        if !md.emits.is_empty() {
            println!("  Emits: {}", md.emits.join(", "));
        }
        println!("}}");
        println!();
    }
}

fn cmd_diagram(path: &str, args: &MachineDiagramArgs) {
    let source = read_source(path);

    // Determine output format. `--dot` is a shorthand for `--format graphviz`.
    let format = if args.dot {
        MachineFormat::Graphviz
    } else {
        args.format.clone().unwrap_or(MachineFormat::Mermaid)
    };

    let ast_machines = parse_machines(path, &source);

    if args.check {
        match check_machines_or_ast_fallback(path, &source, &ast_machines) {
            MachineCheckResult::AstFallback => {
                // Fall through to AST rendering below.
            }
            MachineCheckResult::Checked(hir_machines) => {
                // HIR is flat by invariant — composite grouping and the emits
                // manifest live only on the AST. Build both side-tables keyed by
                // machine name, threaded alongside the HIR (same pattern as
                // groups_by_name that already existed for composites).
                let groups_by_name: std::collections::HashMap<
                    &str,
                    &[hew_parser::ast::CompositeGroup],
                > = ast_machines
                    .iter()
                    .map(|m| (m.name.as_str(), m.composite_groups.as_slice()))
                    .collect();
                // fix 3: emits manifest side-table for the HIR path.
                let emits_by_name: std::collections::HashMap<&str, &[String]> = ast_machines
                    .iter()
                    .map(|m| (m.name.as_str(), m.emits.as_slice()))
                    .collect();

                // Filter by --machine if specified.
                let filtered: Vec<&HirMachineDecl> = if let Some(name) = &args.machine_name {
                    let matched: Vec<_> = hir_machines.iter().filter(|m| &m.name == name).collect();
                    if matched.is_empty() {
                        eprintln!("No machine named `{name}` found in {path}");
                        std::process::exit(1);
                    }
                    matched
                } else {
                    hir_machines.iter().collect()
                };

                for machine in filtered {
                    let groups = groups_by_name
                        .get(machine.name.as_str())
                        .copied()
                        .unwrap_or(&[]);
                    let emits = emits_by_name
                        .get(machine.name.as_str())
                        .copied()
                        .unwrap_or(&[]);
                    match format {
                        MachineFormat::Mermaid => print_mermaid_hir(machine, groups, emits),
                        MachineFormat::Graphviz | MachineFormat::Dot => {
                            print_dot_hir(machine, groups, emits);
                        }
                        MachineFormat::Json => print_json_hir(machine, groups, emits),
                    }
                }
                return;
            }
        }
    }

    // AST-only rendering path (--no-check, or generic machines falling through
    // from the check path above).
    if ast_machines.is_empty() {
        eprintln!("No machines found in {path}");
        std::process::exit(1);
    }

    let filtered: Vec<&MachineDecl> = if let Some(name) = &args.machine_name {
        let matched: Vec<_> = ast_machines.iter().filter(|m| &m.name == name).collect();
        if matched.is_empty() {
            eprintln!("No machine named `{name}` found in {path}");
            std::process::exit(1);
        }
        matched
    } else {
        ast_machines.iter().collect()
    };

    for md in filtered {
        match format {
            MachineFormat::Mermaid => print_mermaid(md),
            MachineFormat::Graphviz | MachineFormat::Dot => print_dot(md),
            MachineFormat::Json => print_json_ast(md),
        }
    }
}

// ── HIR-backed renderers (used when --check is active) ──────────────────────

fn print_mermaid_hir(
    machine: &HirMachineDecl,
    groups: &[hew_parser::ast::CompositeGroup],
    emits: &[String],
) {
    print_mermaid_title(&machine.name, &machine.type_params);
    println!("stateDiagram-v2");

    if let Some(first) = machine.states.first() {
        println!("    [*] --> {}", first.name);
    }

    // fix 1: has_default — a note tells readers this is a stay-on-unhandled machine,
    // distinguishing it from a trapping machine where unhandled events are errors.
    if machine.has_default {
        println!("    note right of [*]");
        println!("        unhandled events stay in current state");
        println!("    end note");
    }

    // Emit composite nesting first: a `state Composite { … }` block groups its
    // members with the initial-substate marker. Members rendered inside a block
    // are still valid transition endpoints below. The grouping comes from the
    // AST side-table threaded in by `cmd_diagram` (HIR is flat).
    let composite_members: std::collections::HashSet<&str> = groups
        .iter()
        .flat_map(|g| g.members.iter().map(String::as_str))
        .collect();
    for group in groups {
        println!("    state {} {{", group.name);
        println!("        [*] --> {}", group.initial);
        for member in &group.members {
            println!("        {member}");
        }
        println!("    }}");
    }

    for tr in &machine.transitions {
        if tr.source_state == "_" {
            for state in &machine.states {
                let target = if tr.target_state == "_" {
                    &state.name
                } else {
                    &tr.target_state
                };
                let has_explicit = machine
                    .transitions
                    .iter()
                    .any(|t| t.source_state == state.name && t.event_name == tr.event_name);
                if !has_explicit && target != &state.name {
                    println!("    {} --> {} : {}", state.name, target, tr.event_name);
                }
            }
        } else {
            let target = if tr.target_state == "_" {
                &tr.source_state
            } else {
                &tr.target_state
            };
            // fix 2: reenter — suffix [reenter] on self-transition edge labels so
            // readers can distinguish a no-op self-loop from one that re-runs
            // the entry/exit lifecycle blocks.
            let mut label = if tr.guard.is_some() {
                format!("{} [guard]", tr.event_name)
            } else {
                tr.event_name.clone()
            };
            if tr.reenter {
                label.push_str(" [reenter]");
            }
            println!("    {} --> {} : {}", tr.source_state, target, label);
        }
    }

    // State annotations: entry/exit notes and field names. Members rendered
    // inside a composite block still take their annotations at the top level.
    let _ = &composite_members;
    for state in &machine.states {
        let mut notes: Vec<String> = state.fields.iter().map(|f| f.name.clone()).collect();
        if state.has_entry {
            notes.push("entry".into());
        }
        if state.has_exit {
            notes.push("exit".into());
        }
        if !notes.is_empty() {
            println!("    {} : {}", state.name, notes.join(", "));
        }
    }

    // fix 3: emits manifest — note in the diagram when declared.
    if !emits.is_empty() {
        println!("    note left of [*]");
        println!("        Emits: {}", emits.join(", "));
        println!("    end note");
    }

    println!();
}

fn print_mermaid_title(name: &str, type_params: &[String]) {
    // Mermaid YAML frontmatter title carries the generic-params signature
    // when present (e.g. `Lifecycle<T>`). Omitted entirely for monomorphic
    // machines so existing snapshot tests and consumer pipelines are
    // unaffected.
    if type_params.is_empty() {
        return;
    }

    println!("---");
    println!("title: {}<{}>", name, type_params.join(", "));
    println!("---");
}

fn print_dot_hir(
    machine: &HirMachineDecl,
    groups: &[hew_parser::ast::CompositeGroup],
    emits: &[String],
) {
    println!("digraph {} {{", machine.name);
    println!("    rankdir=LR;");
    println!("    node [shape=circle];");

    // fix 1: has_default — graph label signals stay-on-unhandled semantics.
    if machine.has_default {
        println!(
            "    label=\"{}\\n(unhandled events stay in current state)\";",
            machine.name
        );
        println!("    labelloc=t;");
    }

    // fix 3: emits manifest — tooltip attribute when declared.
    if !emits.is_empty() {
        println!("    tooltip=\"Emits: {}\";", emits.join(", "));
    }

    if let Some(first) = machine.states.first() {
        println!("    __start [shape=point, width=0.2];");
        println!("    __start -> {};", first.name);
    }

    // Composite members render inside a `subgraph cluster_<Composite>` box.
    let member_to_group: std::collections::HashMap<&str, &str> = groups
        .iter()
        .flat_map(|g| g.members.iter().map(move |m| (m.as_str(), g.name.as_str())))
        .collect();

    let node_decl = |state: &hew_hir::HirMachineState| -> String {
        let mut annotations: Vec<String> = state.fields.iter().map(|f| f.name.clone()).collect();
        if state.has_entry {
            annotations.push("entry".into());
        }
        if state.has_exit {
            annotations.push("exit".into());
        }
        if annotations.is_empty() {
            format!("{} [label=\"{}\"];", state.name, state.name)
        } else {
            format!(
                "{} [label=\"{}\\n({})\", shape=Mrecord];",
                state.name,
                state.name,
                annotations.join(", ")
            )
        }
    };

    // Emit clustered composite members first.
    for group in groups {
        println!("    subgraph cluster_{} {{", group.name);
        println!("        label=\"{}\";", group.name);
        for member in &group.members {
            if let Some(state) = machine.states.iter().find(|s| &s.name == member) {
                println!("        {}", node_decl(state));
            }
        }
        println!("    }}");
    }

    // Emit the remaining (non-member) states at the top level.
    for state in &machine.states {
        if member_to_group.contains_key(state.name.as_str()) {
            continue;
        }
        println!("    {}", node_decl(state));
    }

    for tr in &machine.transitions {
        if tr.source_state == "_" {
            for state in &machine.states {
                let target = if tr.target_state == "_" {
                    &state.name
                } else {
                    &tr.target_state
                };
                let has_explicit = machine
                    .transitions
                    .iter()
                    .any(|t| t.source_state == state.name && t.event_name == tr.event_name);
                if !has_explicit && target != &state.name {
                    println!(
                        "    {} -> {} [label=\"{}\"];",
                        state.name, target, tr.event_name
                    );
                }
            }
        } else {
            let target = if tr.target_state == "_" {
                &tr.source_state
            } else {
                &tr.target_state
            };
            // fix 2: reenter — suffix [reenter] on self-transition edge labels.
            let mut label = if tr.guard.is_some() {
                format!("{} [guard]", tr.event_name)
            } else {
                tr.event_name.clone()
            };
            if tr.reenter {
                label.push_str(" [reenter]");
            }
            println!(
                "    {} -> {} [label=\"{}\"];",
                tr.source_state, target, label
            );
        }
    }

    println!("}}");
    println!();
}

#[allow(
    clippy::too_many_lines,
    reason = "sequential field emission; splitting would obscure the schema layout"
)]
fn print_json_hir(
    machine: &HirMachineDecl,
    groups: &[hew_parser::ast::CompositeGroup],
    emits: &[String],
) {
    // Stable JSON schema for tooling. Field order is deterministic.
    print!("{{");
    print!("\"name\":{:?}", machine.name);
    // fix 1: hasDefault — trapping vs stay-on-unhandled machines are distinguishable.
    print!(",\"hasDefault\":{}", machine.has_default);
    // fix 3: emits manifest at the machine level (from the AST side-table).
    print!(",\"emits\":[");
    for (i, e) in emits.iter().enumerate() {
        if i > 0 {
            print!(",");
        }
        print!("{e:?}");
    }
    print!("]");
    // fix 5: typeParams in JSON schema.
    print!(",\"typeParams\":[");
    for (i, p) in machine.type_params.iter().enumerate() {
        if i > 0 {
            print!(",");
        }
        print!("{p:?}");
    }
    print!("]");
    print!(",\"states\":[");
    for (i, state) in machine.states.iter().enumerate() {
        if i > 0 {
            print!(",");
        }
        print!(
            "{{\"name\":{:?},\"hasEntry\":{},\"hasExit\":{}}}",
            state.name, state.has_entry, state.has_exit
        );
    }
    print!("],\"events\":[");
    for (i, event) in machine.events.iter().enumerate() {
        if i > 0 {
            print!(",");
        }
        // fix 5: event payload field names in JSON.
        print!("{{\"name\":{:?},\"fields\":[", event.name);
        for (j, field) in event.fields.iter().enumerate() {
            if j > 0 {
                print!(",");
            }
            print!("{:?}", field.name);
        }
        print!("]}}");
    }
    // fix 5: wildcard expansion in JSON — apply the same expansion + vacuous-self-loop
    // suppression used by the diagram renderers instead of dumping raw `"_"` rows.
    print!("],\"transitions\":[");
    let mut first_tr = true;
    for tr in &machine.transitions {
        if tr.source_state == "_" {
            for state in &machine.states {
                let concrete_target = if tr.target_state == "_" {
                    state.name.as_str()
                } else {
                    tr.target_state.as_str()
                };
                let has_explicit = machine
                    .transitions
                    .iter()
                    .any(|t| t.source_state == state.name && t.event_name == tr.event_name);
                // Suppress wildcard-derived vacuous self-loops (same rule as renderers).
                if !has_explicit && concrete_target != state.name.as_str() {
                    if !first_tr {
                        print!(",");
                    }
                    first_tr = false;
                    print!(
                        "{{\"event\":{:?},\"from\":{:?},\"to\":{:?},\
                         \"selfTransition\":false,\"reenter\":false,\"bodyEmits\":[",
                        tr.event_name, state.name, concrete_target,
                    );
                    for (k, e) in tr.body_emits.iter().enumerate() {
                        if k > 0 {
                            print!(",");
                        }
                        print!("{e:?}");
                    }
                    print!("]}}");
                }
            }
        } else {
            if !first_tr {
                print!(",");
            }
            first_tr = false;
            let concrete_target = if tr.target_state == "_" {
                tr.source_state.as_str()
            } else {
                tr.target_state.as_str()
            };
            // fix 2: reenter field in JSON transition objects.
            print!(
                "{{\"event\":{:?},\"from\":{:?},\"to\":{:?},\
                 \"selfTransition\":{},\"reenter\":{},\"bodyEmits\":[",
                tr.event_name, tr.source_state, concrete_target, tr.is_self_transition, tr.reenter,
            );
            for (k, e) in tr.body_emits.iter().enumerate() {
                if k > 0 {
                    print!(",");
                }
                print!("{e:?}");
            }
            print!("]}}");
        }
    }
    // Composite grouping (from the AST side-table; HIR is flat). Omitted as an
    // empty array for flat machines so the schema stays additive.
    print!("],\"composites\":[");
    for (i, group) in groups.iter().enumerate() {
        if i > 0 {
            print!(",");
        }
        print!(
            "{{\"name\":{:?},\"initial\":{:?},\"members\":[",
            group.name, group.initial
        );
        for (j, member) in group.members.iter().enumerate() {
            if j > 0 {
                print!(",");
            }
            print!("{member:?}");
        }
        print!("]}}");
    }
    println!("]}}");
}

// ── AST-backed renderers (used with --no-check) ──────────────────────────────

fn print_mermaid(md: &MachineDecl) {
    let type_param_names: Vec<String> = md.type_params.iter().map(|p| p.name.clone()).collect();
    print_mermaid_title(&md.name, &type_param_names);
    println!("stateDiagram-v2");

    if let Some(first) = md.states.first() {
        println!("    [*] --> {}", first.name);
    }

    // fix 1: has_default — note signals stay-on-unhandled semantics.
    if md.has_default {
        println!("    note right of [*]");
        println!("        unhandled events stay in current state");
        println!("    end note");
    }

    for trans in &md.transitions {
        if trans.source_state == "_" {
            for state in &md.states {
                let target = if trans.target_state == "_" {
                    &state.name
                } else {
                    &trans.target_state
                };
                let has_explicit = md
                    .transitions
                    .iter()
                    .any(|t| t.source_state == state.name && t.event_name == trans.event_name);
                if !has_explicit && target != &state.name {
                    println!("    {} --> {} : {}", state.name, target, trans.event_name);
                }
            }
        } else {
            let target = if trans.target_state == "_" {
                &trans.source_state
            } else {
                &trans.target_state
            };
            // fix 2: reenter — suffix [reenter] on self-transition edge labels.
            let mut label = if trans.guard.is_some() {
                format!("{} [guard]", trans.event_name)
            } else {
                trans.event_name.clone()
            };
            if trans.reenter {
                label.push_str(" [reenter]");
            }
            println!("    {} --> {} : {}", trans.source_state, target, label);
        }
    }

    for state in &md.states {
        let mut annotations: Vec<String> =
            state.fields.iter().map(|(name, _)| name.clone()).collect();
        if state.entry.is_some() {
            annotations.push("entry".into());
        }
        if state.exit.is_some() {
            annotations.push("exit".into());
        }
        if !annotations.is_empty() {
            println!("    {} : {}", state.name, annotations.join(", "));
        }
    }

    // fix 3: emits manifest note when declared.
    if !md.emits.is_empty() {
        println!("    note left of [*]");
        println!("        Emits: {}", md.emits.join(", "));
        println!("    end note");
    }

    println!();
}

fn print_dot(md: &MachineDecl) {
    println!("digraph {} {{", md.name);
    println!("    rankdir=LR;");
    println!("    node [shape=circle];");

    // fix 1: has_default — graph label signals stay-on-unhandled semantics.
    if md.has_default {
        println!(
            "    label=\"{}\\n(unhandled events stay in current state)\";",
            md.name
        );
        println!("    labelloc=t;");
    }

    // fix 3: emits manifest as a graph tooltip attribute.
    if !md.emits.is_empty() {
        println!("    tooltip=\"Emits: {}\";", md.emits.join(", "));
    }

    if let Some(first) = md.states.first() {
        println!("    __start [shape=point, width=0.2];");
        println!("    __start -> {};", first.name);
    }

    for state in &md.states {
        let mut annotations: Vec<String> =
            state.fields.iter().map(|(name, _)| name.clone()).collect();
        if state.entry.is_some() {
            annotations.push("entry".into());
        }
        if state.exit.is_some() {
            annotations.push("exit".into());
        }
        if annotations.is_empty() {
            println!("    {} [label=\"{}\"];", state.name, state.name);
        } else {
            println!(
                "    {} [label=\"{}\\n({})\", shape=Mrecord];",
                state.name,
                state.name,
                annotations.join(", ")
            );
        }
    }

    for trans in &md.transitions {
        if trans.source_state == "_" {
            for state in &md.states {
                let target = if trans.target_state == "_" {
                    &state.name
                } else {
                    &trans.target_state
                };
                let has_explicit = md
                    .transitions
                    .iter()
                    .any(|t| t.source_state == state.name && t.event_name == trans.event_name);
                if !has_explicit && target != &state.name {
                    println!(
                        "    {} -> {} [label=\"{}\"];",
                        state.name, target, trans.event_name
                    );
                }
            }
        } else {
            let target = if trans.target_state == "_" {
                &trans.source_state
            } else {
                &trans.target_state
            };
            // fix 2: reenter — suffix [reenter] on self-transition edge labels.
            let mut label = if trans.guard.is_some() {
                format!("{} [guard]", trans.event_name)
            } else {
                trans.event_name.clone()
            };
            if trans.reenter {
                label.push_str(" [reenter]");
            }
            println!(
                "    {} -> {} [label=\"{}\"];",
                trans.source_state, target, label
            );
        }
    }

    println!("}}");
    println!();
}

fn print_json_ast(md: &MachineDecl) {
    print!("{{");
    print!("\"name\":{:?}", md.name);
    // fix 1: hasDefault.
    print!(",\"hasDefault\":{}", md.has_default);
    // fix 3: emits manifest.
    print!(",\"emits\":[");
    for (i, e) in md.emits.iter().enumerate() {
        if i > 0 {
            print!(",");
        }
        print!("{e:?}");
    }
    print!("]");
    // fix 5: typeParams in schema.
    print!(",\"typeParams\":[");
    for (i, p) in md.type_params.iter().enumerate() {
        if i > 0 {
            print!(",");
        }
        print!("{:?}", p.name);
    }
    print!("]");
    print!(",\"states\":[");
    for (i, state) in md.states.iter().enumerate() {
        if i > 0 {
            print!(",");
        }
        print!(
            "{{\"name\":{:?},\"hasEntry\":{},\"hasExit\":{}}}",
            state.name,
            state.entry.is_some(),
            state.exit.is_some()
        );
    }
    print!("],\"events\":[");
    for (i, event) in md.events.iter().enumerate() {
        if i > 0 {
            print!(",");
        }
        // fix 5: event payload field names.
        print!("{{\"name\":{:?},\"fields\":[", event.name);
        for (j, (fname, _)) in event.fields.iter().enumerate() {
            if j > 0 {
                print!(",");
            }
            print!("{fname:?}");
        }
        print!("]}}");
    }
    // fix 5: wildcard expansion in AST JSON — same logic as diagram renderers.
    print!("],\"transitions\":[");
    let mut first_tr = true;
    for tr in &md.transitions {
        if tr.source_state == "_" {
            for state in &md.states {
                let concrete_target = if tr.target_state == "_" {
                    state.name.as_str()
                } else {
                    tr.target_state.as_str()
                };
                let has_explicit = md
                    .transitions
                    .iter()
                    .any(|t| t.source_state == state.name && t.event_name == tr.event_name);
                // Suppress wildcard-derived vacuous self-loops.
                if !has_explicit && concrete_target != state.name.as_str() {
                    if !first_tr {
                        print!(",");
                    }
                    first_tr = false;
                    print!(
                        "{{\"event\":{:?},\"from\":{:?},\"to\":{:?},\
                         \"selfTransition\":false,\"reenter\":false}}",
                        tr.event_name, state.name, concrete_target,
                    );
                }
            }
        } else {
            if !first_tr {
                print!(",");
            }
            first_tr = false;
            let concrete_target = if tr.target_state == "_" {
                tr.source_state.as_str()
            } else {
                tr.target_state.as_str()
            };
            let is_self = tr.source_state == concrete_target;
            // fix 2: reenter field in JSON transition objects.
            print!(
                "{{\"event\":{:?},\"from\":{:?},\"to\":{:?},\
                 \"selfTransition\":{},\"reenter\":{}}}",
                tr.event_name, tr.source_state, concrete_target, is_self, tr.reenter
            );
        }
    }
    println!("]}}");
}
