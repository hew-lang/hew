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

mod diagram;

struct MachineSource {
    declaration: MachineDecl,
    qualified_name: String,
}

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
fn parse_machines(path: &str, source: &str) -> Vec<MachineSource> {
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
                Some(MachineSource {
                    declaration: md.clone(),
                    qualified_name: md.name.to_string(),
                })
            } else {
                None
            }
        })
        .collect()
}

/// Validate through the ordinary frontend, then render authored declarations.
/// Machine syntax is presentation data; executable HIR contains ordinary enums
/// and methods after normalization.
fn check_and_lower(path: &str) -> Vec<MachineSource> {
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
                    machines.push(MachineSource {
                        declaration: machine.clone(),
                        qualified_name: if *id == graph.root {
                            machine.name.to_string()
                        } else {
                            format!("{}.{}", id.dotted(), machine.name)
                        },
                    });
                }
            }
        }
    } else {
        machines.extend(state.program.items.iter().filter_map(|(item, _)| {
            if let Item::Machine(machine) = item {
                Some(MachineSource {
                    declaration: machine.clone(),
                    qualified_name: machine.name.to_string(),
                })
            } else {
                None
            }
        }));
    }
    machines
}

fn cmd_list(path: &str) {
    let machines = check_and_lower(path);
    if machines.is_empty() {
        eprintln!("No machines found in {path}");
        std::process::exit(1);
    }
    for machine in &machines {
        print!(
            "{}",
            diagram::Chart::new(&machine.declaration, &machine.qualified_name).list()
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

    let filtered: Vec<&MachineSource> = if let Some(name) = &args.machine_name {
        let mut matched: Vec<_> = ast_machines
            .iter()
            .filter(|machine| machine.qualified_name == *name)
            .collect();
        if matched.is_empty() {
            matched = ast_machines
                .iter()
                .filter(|machine| machine.declaration.name.name.as_str() == name)
                .collect();
        }
        if matched.is_empty() {
            eprintln!("No machine named `{name}` found in {path}");
            std::process::exit(1);
        }
        if matched.len() > 1 {
            eprintln!(
                "Machine `{name}` is ambiguous; select one of: {}",
                matched
                    .iter()
                    .map(|machine| machine.qualified_name.as_str())
                    .collect::<Vec<_>>()
                    .join(", ")
            );
            std::process::exit(1);
        }
        matched
    } else {
        ast_machines.iter().collect()
    };

    for source in filtered {
        let chart = diagram::Chart::new(&source.declaration, &source.qualified_name);
        match format {
            MachineFormat::Mermaid => print!("{}", chart.mermaid()),
            MachineFormat::Graphviz | MachineFormat::Dot => print!("{}", chart.dot()),
            MachineFormat::Json => println!("{}", chart.json()),
        }
    }
}
