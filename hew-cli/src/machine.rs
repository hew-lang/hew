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

use hew_hir::{lower_program, HirDiagnosticKind, HirItem, HirMachineDecl, ResolutionCtx};
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

    // Filter out NotYetImplemented diagnostics from non-machine items —
    // Lane A only lowers machines; functions are also lowered if present.
    let machine_errors: Vec<_> = lowered
        .diagnostics
        .iter()
        .filter(|d| {
            !matches!(
                &d.kind,
                HirDiagnosticKind::NotYetImplemented { .. }
                    | HirDiagnosticKind::UnresolvedSymbol { .. }
                    | HirDiagnosticKind::ImportMissing { .. }
            )
        })
        .collect();

    if !machine_errors.is_empty() {
        for diag in &machine_errors {
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

fn cmd_list(path: &str) {
    let source = read_source(path);
    let machines = parse_machines(path, &source);

    if machines.is_empty() {
        println!("No machines found in {path}");
        return;
    }

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

    if args.check {
        // Run HIR checks first; exit non-zero on any machine error.
        let hir_machines = check_and_lower(path, &source);

        if hir_machines.is_empty() {
            eprintln!("No machines found in {path}");
            std::process::exit(1);
        }

        // HIR is flat by invariant — composite grouping lives only on the AST.
        // Parse the source again to recover the grouping side-table and render
        // nested boxes over the flat HIR. Keyed by machine name.
        let ast_machines = parse_machines(path, &source);
        let groups_by_name: std::collections::HashMap<&str, &[hew_parser::ast::CompositeGroup]> =
            ast_machines
                .iter()
                .map(|m| (m.name.as_str(), m.composite_groups.as_slice()))
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
            match format {
                MachineFormat::Mermaid => print_mermaid_hir(machine, groups),
                MachineFormat::Graphviz | MachineFormat::Dot => print_dot_hir(machine, groups),
                MachineFormat::Json => print_json_hir(machine, groups),
            }
        }
    } else {
        // --no-check: fall back to AST-only rendering (no static checks).
        let machines = parse_machines(path, &source);

        if machines.is_empty() {
            eprintln!("No machines found in {path}");
            std::process::exit(1);
        }

        let filtered: Vec<&MachineDecl> = if let Some(name) = &args.machine_name {
            let matched: Vec<_> = machines.iter().filter(|m| &m.name == name).collect();
            if matched.is_empty() {
                eprintln!("No machine named `{name}` found in {path}");
                std::process::exit(1);
            }
            matched
        } else {
            machines.iter().collect()
        };

        for md in filtered {
            match format {
                MachineFormat::Mermaid => print_mermaid(md),
                MachineFormat::Graphviz | MachineFormat::Dot => print_dot(md),
                MachineFormat::Json => print_json_ast(md),
            }
        }
    }
}

// ── HIR-backed renderers (used when --check is active) ──────────────────────

fn print_mermaid_hir(machine: &HirMachineDecl, groups: &[hew_parser::ast::CompositeGroup]) {
    print_mermaid_title(&machine.name, &machine.type_params);
    println!("stateDiagram-v2");

    if let Some(first) = machine.states.first() {
        println!("    [*] --> {}", first.name);
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
            let label = if tr.guard.is_some() {
                format!("{} [guard]", tr.event_name)
            } else {
                tr.event_name.clone()
            };
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

fn print_dot_hir(machine: &HirMachineDecl, groups: &[hew_parser::ast::CompositeGroup]) {
    println!("digraph {} {{", machine.name);
    println!("    rankdir=LR;");
    println!("    node [shape=circle];");

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
            let label = if tr.guard.is_some() {
                format!("{} [guard]", tr.event_name)
            } else {
                tr.event_name.clone()
            };
            println!(
                "    {} -> {} [label=\"{}\"];",
                tr.source_state, target, label
            );
        }
    }

    println!("}}");
    println!();
}

fn print_json_hir(machine: &HirMachineDecl, groups: &[hew_parser::ast::CompositeGroup]) {
    // Stable JSON schema for tooling. Field order is deterministic.
    print!("{{");
    print!("\"name\":{:?}", machine.name);
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
        print!("{{\"name\":{:?}}}", event.name);
    }
    print!("],\"transitions\":[");
    for (i, tr) in machine.transitions.iter().enumerate() {
        if i > 0 {
            print!(",");
        }
        print!(
            "{{\"event\":{:?},\"from\":{:?},\"to\":{:?},\"selfTransition\":{}}}",
            tr.event_name, tr.source_state, tr.target_state, tr.is_self_transition
        );
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
            let label = if trans.guard.is_some() {
                format!("{} [guard]", trans.event_name)
            } else {
                trans.event_name.clone()
            };
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

    println!();
}

fn print_dot(md: &MachineDecl) {
    println!("digraph {} {{", md.name);
    println!("    rankdir=LR;");
    println!("    node [shape=circle];");

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
            let label = if trans.guard.is_some() {
                format!("{} [guard]", trans.event_name)
            } else {
                trans.event_name.clone()
            };
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
        print!("{{\"name\":{:?}}}", event.name);
    }
    print!("],\"transitions\":[");
    for (i, tr) in md.transitions.iter().enumerate() {
        if i > 0 {
            print!(",");
        }
        let is_self = tr.source_state == tr.target_state && tr.source_state != "_";
        print!(
            "{{\"event\":{:?},\"from\":{:?},\"to\":{:?},\"selfTransition\":{}}}",
            tr.event_name, tr.source_state, tr.target_state, is_self
        );
    }
    println!("]}}");
}
