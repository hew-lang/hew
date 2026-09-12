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

use hew_compile::FrontendOptions;
use hew_hir::{lower_program, ResolutionCtx};
use hew_parser::ast::{Item, MachineDecl};

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

/// Parse `source`, extracting the machine declarations of this file.
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
        .iter()
        .filter_map(|(item, _)| {
            if let Item::Machine(md) = item {
                Some(md.clone())
            } else {
                None
            }
        })
        .collect()
}

/// Validate through the ordinary frontend, then render authored declarations.
/// Machine syntax is presentation data; executable HIR contains ordinary enums
/// and methods after normalization.
fn check_and_lower(path: &str) -> Vec<MachineDecl> {
    let state = match hew_compile::run_file_frontend_to_typecheck(path, &FrontendOptions::default())
    {
        Ok(state) => state,
        Err(failure) => {
            crate::compile::render_frontend_diagnostics(&failure.diagnostics);
            if failure.diagnostics.is_empty() {
                eprintln!("{path}: error: {}", failure.message);
            }
            std::process::exit(1);
        }
    };
    let Some(tco) = state.typecheck_result.tco.as_ref() else {
        eprintln!("{path}: error: machine checks require a type-checked program");
        std::process::exit(1);
    };
    let lowered = lower_program(
        &state.program,
        tco,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );

    if !lowered.diagnostics.is_empty() {
        for diag in &lowered.diagnostics {
            eprintln!("{path}: error: {}", diag.note);
        }
        std::process::exit(1);
    }

    let mut machines = Vec::new();
    let mut seen = std::collections::HashSet::new();
    if let Some(graph) = &state.program.module_graph {
        for id in &graph.topo_order {
            let Some(module) = graph.modules.get(id) else {
                continue;
            };
            for (ordinal, (item, span)) in module.items.iter().enumerate() {
                let Item::Machine(machine) = item else {
                    continue;
                };
                if *id != graph.root && !machine.visibility.is_pub() {
                    continue;
                }
                let source = graph
                    .item_source(id, ordinal)
                    .or_else(|| module.source_paths.first());
                if seen.insert((source.cloned(), span.clone())) {
                    machines.push(machine.clone());
                }
            }
        }
    } else {
        machines.extend(state.program.items.iter().filter_map(|(item, _)| {
            if let Item::Machine(machine) = item {
                Some(machine.clone())
            } else {
                None
            }
        }));
    }
    machines
}

fn print_list_entry(
    name: &str,
    states: &[(&str, Vec<&str>)],
    events: &[(&str, Vec<&str>)],
    transitions: usize,
    has_default: bool,
    emits: &[hew_parser::ast::MachineEvent],
) {
    println!("machine {name} {{");
    println!("  States:");
    for (state, fields) in states {
        if fields.is_empty() {
            println!("    {state}");
        } else {
            println!("    {} {{ {} }}", state, fields.join(", "));
        }
    }
    println!("  Events:");
    for (event, fields) in events {
        if fields.is_empty() {
            println!("    {event}");
        } else {
            println!("    {} {{ {} }}", event, fields.join(", "));
        }
    }
    println!("  Transitions: {transitions}");
    if has_default {
        println!("  Default: unhandled events stay in current state");
    }
    // fix 3: Emits section in cmd_list.
    if !emits.is_empty() {
        println!(
            "  Emits: {}",
            emits
                .iter()
                .map(|output| output.name.as_str())
                .collect::<Vec<_>>()
                .join(", ")
        );
    }
    println!("}}");
    println!();
}

fn cmd_list(path: &str) {
    let machines = check_and_lower(path);
    if machines.is_empty() {
        eprintln!("No machines found in {path}");
        std::process::exit(1);
    }
    for machine in &machines {
        let states = machine
            .states
            .iter()
            .map(|state| {
                (
                    state.name.as_str(),
                    state.fields.iter().map(|(name, _)| name.as_str()).collect(),
                )
            })
            .collect::<Vec<_>>();
        let events = machine
            .events
            .iter()
            .map(|event| {
                (
                    event.name.as_str(),
                    event.fields.iter().map(|(name, _)| name.as_str()).collect(),
                )
            })
            .collect::<Vec<_>>();
        print_list_entry(
            &machine.name,
            &states,
            &events,
            machine.transitions.len(),
            machine.has_default,
            &machine.emits,
        );
    }
}

fn cmd_diagram(path: &str, args: &MachineDiagramArgs) {
    // Determine output format. `--dot` is a shorthand for `--format graphviz`.
    let format = if args.dot {
        MachineFormat::Graphviz
    } else {
        args.format.clone().unwrap_or(MachineFormat::Mermaid)
    };

    let ast_machines = if args.check {
        check_and_lower(path)
    } else {
        parse_machines(path, &read_source(path))
    };

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

    for (source, rule) in visible_rules(md) {
        let label = rule_label(rule);
        println!("    {} --> {} : {}", source, rule.target_state, label);
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
        println!(
            "        Emits: {}",
            md.emits
                .iter()
                .map(|output| output.name.as_str())
                .collect::<Vec<_>>()
                .join(", ")
        );
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
        println!(
            "    tooltip=\"Emits: {}\";",
            md.emits
                .iter()
                .map(|output| output.name.as_str())
                .collect::<Vec<_>>()
                .join(", ")
        );
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

    for (source, rule) in visible_rules(md) {
        let label = rule_label(rule);
        println!(
            "    {} -> {} [label=\"{}\"];",
            source, rule.target_state, label
        );
    }

    println!("}}");
    println!();
}

fn visible_rules(machine: &MachineDecl) -> Vec<(&str, &hew_parser::ast::MachineTransition)> {
    let mut rules = Vec::new();
    for rule in &machine.transitions {
        if rule.source_state == "_" {
            for state in &machine.states {
                let covered = machine.transitions.iter().any(|specific| {
                    specific.source_state == state.name
                        && specific.event_name == rule.event_name
                        && specific.guard.is_none()
                });
                if !covered {
                    rules.push((state.name.as_str(), rule));
                }
            }
        } else {
            rules.push((rule.source_state.as_str(), rule));
        }
    }
    rules
}

fn rule_label(rule: &hew_parser::ast::MachineTransition) -> String {
    let mut label = rule.event_name.clone();
    if rule.guard.is_some() {
        label.push_str(" [guard]");
    }
    if rule.target_state == "_" {
        label.push_str(" [external]");
    } else if rule.reenter {
        label.push_str(" [reenter]");
    }
    label
}

fn print_json_ast(machine: &MachineDecl) {
    let fields = |fields: &[(String, hew_parser::ast::Spanned<hew_parser::ast::TypeExpr>)]| {
        fields
            .iter()
            .map(|(name, _)| name.clone())
            .collect::<Vec<_>>()
    };
    let value = serde_json::json!({
        "name": machine.name,
        "hasDefault": machine.has_default,
        "emits": machine.emits.iter().map(|output| &output.name).collect::<Vec<_>>(),
        "outputs": machine.emits.iter().map(|output| serde_json::json!({"name": output.name, "fields": fields(&output.fields)})).collect::<Vec<_>>(),
        "typeParams": machine.type_params.iter().map(|param| &param.name).collect::<Vec<_>>(),
        "states": machine.states.iter().map(|state| serde_json::json!({"name": state.name, "fields": fields(&state.fields), "hasEntry": state.entry.is_some(), "hasExit": state.exit.is_some()})).collect::<Vec<_>>(),
        "events": machine.events.iter().map(|event| serde_json::json!({"name": event.name, "fields": fields(&event.fields)})).collect::<Vec<_>>(),
        "transitions": visible_rules(machine).into_iter().map(|(source, rule)| serde_json::json!({
            "event": rule.event_name, "from": source, "to": rule.target_state,
            "selfTransition": source == rule.target_state,
            "guarded": rule.guard.is_some(), "reenter": rule.reenter,
            "external": rule.reenter || rule.target_state != source,
        })).collect::<Vec<_>>(),
        "composites": machine.composite_groups.iter().map(|group| serde_json::json!({"name": group.name, "initial": group.initial, "members": group.members})).collect::<Vec<_>>(),
    });
    println!("{value}");
}
