//! `hew machine` — Extract and visualize state machines from Hew source files.
//!
//! Usage:
//!   hew machine diagram <file.hew>         Output Mermaid state diagram
//!   hew machine diagram <file.hew> --dot   Output Graphviz DOT instead
//!   hew machine list <file.hew>            List all machines with states/events

use hew_parser::ast::{Item, MachineDecl};

pub fn cmd_machine(args: &[String]) {
    if args.is_empty() {
        eprintln!("Usage: hew machine <subcommand> <file.hew>");
        eprintln!();
        eprintln!("Subcommands:");
        eprintln!("  diagram <file.hew>         Output Mermaid state diagram to stdout");
        eprintln!("  diagram <file.hew> --dot   Output Graphviz DOT format");
        eprintln!("  list <file.hew>            List machines with states and events");
        std::process::exit(1);
    }

    match args[0].as_str() {
        "diagram" => {
            if args.len() < 2 {
                eprintln!("Usage: hew machine diagram <file.hew> [--dot]");
                std::process::exit(1);
            }
            let dot = args.iter().any(|a| a == "--dot");
            cmd_diagram(&args[1], dot);
        }
        "list" => {
            if args.len() < 2 {
                eprintln!("Usage: hew machine list <file.hew>");
                std::process::exit(1);
            }
            cmd_list(&args[1]);
        }
        other => {
            eprintln!("Unknown machine subcommand: {other}");
            eprintln!("Try: hew machine diagram <file.hew>");
            std::process::exit(1);
        }
    }
}

fn parse_machines(path: &str) -> Vec<MachineDecl> {
    let source = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading {path}: {e}");
            std::process::exit(1);
        }
    };

    let result = hew_parser::parse(&source);

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

fn cmd_list(path: &str) {
    let machines = parse_machines(path);

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

fn cmd_diagram(path: &str, dot: bool) {
    let machines = parse_machines(path);

    if machines.is_empty() {
        eprintln!("No machines found in {path}");
        std::process::exit(1);
    }

    for md in &machines {
        if dot {
            print_dot(md);
        } else {
            print_mermaid(md);
        }
    }
}

fn print_mermaid(md: &MachineDecl) {
    println!("stateDiagram-v2");

    // Initial state arrow to first declared state
    if let Some(first) = md.states.first() {
        println!("    [*] --> {}", first.name);
    }

    // Transitions
    for trans in &md.transitions {
        if trans.source_state == "_" {
            // Wildcard: applies to all states without explicit handling
            for state in &md.states {
                let target = if trans.target_state == "_" {
                    &state.name
                } else {
                    &trans.target_state
                };
                // Only emit if there's no explicit transition for this (state, event)
                let has_explicit = md
                    .transitions
                    .iter()
                    .any(|t| t.source_state == state.name && t.event_name == trans.event_name);
                if !has_explicit {
                    if target == &state.name {
                        // Self-transition — only show if meaningful
                    } else {
                        println!("    {} --> {} : {}", state.name, target, trans.event_name);
                    }
                }
            }
        } else {
            let target = if trans.target_state == "_" {
                &trans.source_state
            } else {
                &trans.target_state
            };
            // Add guard annotation if present
            let label = if trans.guard.is_some() {
                format!("{} [guard]", trans.event_name)
            } else {
                trans.event_name.clone()
            };
            println!("    {} --> {} : {}", trans.source_state, target, label);
        }
    }

    // State annotations for states with fields
    for state in &md.states {
        if !state.fields.is_empty() {
            let fields: Vec<String> = state.fields.iter().map(|(name, _)| name.clone()).collect();
            println!("    {} : {}", state.name, fields.join(", "));
        }
    }

    println!();
}

fn print_dot(md: &MachineDecl) {
    println!("digraph {} {{", md.name);
    println!("    rankdir=LR;");
    println!("    node [shape=circle];");

    // Initial state
    if let Some(first) = md.states.first() {
        println!("    __start [shape=point, width=0.2];");
        println!("    __start -> {};", first.name);
    }

    // State nodes with fields
    for state in &md.states {
        if state.fields.is_empty() {
            println!("    {} [label=\"{}\"];", state.name, state.name);
        } else {
            let fields: Vec<String> = state.fields.iter().map(|(name, _)| name.clone()).collect();
            println!(
                "    {} [label=\"{}\\n({})\", shape=Mrecord];",
                state.name,
                state.name,
                fields.join(", ")
            );
        }
    }

    // Transitions
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
