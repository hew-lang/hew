//! Inspection at existing frontend and HIR boundaries, without downstream lowering.

use std::fmt::Write as _;
use std::path::Path;

use hew_parser::ast::Program;

use crate::args::{CompileArgs, DiagnosticFormat};
use crate::{compile, target};
use hew_types::error::DiagChannel;

pub(crate) fn run(args: &CompileArgs) -> i32 {
    let target = match target::TargetSpec::from_requested(args.target.as_deref()) {
        Ok(target) => target,
        Err(error) => {
            crate::emit_semantic_error("E_TARGET", &error, DiagChannel::User);
            return 2;
        }
    };
    let options = compile::frontend_options(&target, &compile::CompileOptions::default());
    let input = args.input.display().to_string();
    let frontend = if args.dump_ast {
        hew_compile::run_file_frontend_to_expansion(&input, &options)
    } else {
        hew_compile::run_file_frontend_to_typecheck(&input, &options)
    };
    let state = match frontend {
        Ok(state) => state,
        Err(failure) => {
            let channel = compile::render_frontend_diagnostics(&failure.diagnostics)
                .unwrap_or(DiagChannel::User);
            if failure.diagnostics.is_empty() {
                crate::emit_semantic_error("E_FRONTEND", &failure.message, channel);
            }
            return channel.exit_code();
        }
    };
    compile::render_frontend_diagnostics(&state.diagnostics);
    let json = args.format == DiagnosticFormat::Json;
    if args.dump_ast {
        print_dump(&dump_ast(&state.program, &args.input), json);
        return 0;
    }
    let Some(checked) = state.typecheck_result.tco.as_ref() else {
        return crate::emit_semantic_error(
            "E_TYPECHECK_REQUIRED",
            "inspection requires a type-checked program",
            DiagChannel::Internal,
        )
        .exit_code();
    };
    if args.dump_defs {
        print_dump(&hew_types::dump::dump_defs(&checked.defs), json);
        return 0;
    }
    let session = hew_compile::Session::from_frontend_options(
        hew_compile::SessionTarget {
            hir_arch: crate::hir_target_arch(&target),
            ..hew_compile::SessionTarget::native()
        },
        &options,
    );
    let module = match session.lower_hir_program(&state.program, checked) {
        Ok(module) => module,
        Err(error) => {
            return crate::render_session_error(error, &state.program, &state.source, &input)
                .exit_code();
        }
    };
    let analysis = hew_hir::analyze_hir(&module);
    if !analysis.diagnostics().is_empty() {
        let diagnostics = hew_compile::hir_diagnostics_to_frontend(
            &state.program,
            &state.source,
            &input,
            analysis.diagnostics().to_vec(),
            &options.documents,
        );
        return compile::render_frontend_diagnostics(&diagnostics)
            .unwrap_or(DiagChannel::Internal)
            .exit_code();
    }
    print_dump(&dump_hir(&analysis, &state.program, &args.input), json);
    0
}

fn dump_hir(analysis: &hew_hir::HirAnalysis<'_>, program: &Program, input: &Path) -> String {
    let module = analysis.module();
    let mut dump = dump_sources(program, input);
    dump.push_str(&hew_types::dump::dump_defs(&module.defs));
    dump.push_str("hir\n");
    dump.push_str(&hew_hir::dump_hir(module));
    dump.push_str(
        "site sources (byte ranges; checked identities are absent for some generated sites)\n",
    );
    let mut sites: Vec<_> = analysis.site_sources().iter().collect();
    sites.sort_unstable_by_key(|(site, _)| **site);
    for (site, source) in sites {
        write!(
            dump,
            "  {site} module={:?} span={}..{}",
            source.source_module, source.span.start, source.span.end
        )
        .expect("write to string");
        if let Some(checked) = module.checked_sites.get(site) {
            write!(
                dump,
                " checked=file#{}:{}..{}",
                checked.module_idx, checked.start, checked.end
            )
            .expect("write to string");
        }
        dump.push('\n');
    }
    dump.push_str("scope sources\n");
    let mut scopes: Vec<_> = analysis.scope_extents().iter().collect();
    scopes.sort_unstable_by_key(|(scope, _)| **scope);
    for (scope, extent) in scopes {
        writeln!(
            dump,
            "  {scope} parent={:?} module={:?} span={}..{}",
            extent.parent, extent.source_module, extent.span.start, extent.span.end
        )
        .expect("write to string");
    }
    dump
}

fn print_dump(dump: &str, json: bool) {
    // Match the existing SIR/MIR contract: JSON stdout contains diagnostics only.
    if json {
        eprint!("{dump}");
    } else {
        print!("{dump}");
    }
}

fn dump_sources(program: &Program, input: &Path) -> String {
    let mut out = String::from("sources\n");
    let input = input.canonicalize().unwrap_or_else(|_| input.to_path_buf());
    writeln!(out, "  root file#0 path={:?}", input.display()).expect("write to string");
    if let Some(graph) = &program.module_graph {
        let indices = graph.file_span_indices();
        for id in &graph.topo_order {
            if *id == graph.root {
                continue;
            }
            let Some(module) = graph.modules.get(id) else {
                continue;
            };
            writeln!(
                out,
                "  module {:?} sources={:?}",
                id.dotted(),
                module.source_paths
            )
            .expect("write to string");
            // Project the graph's per-item identities once per file, including
            // distinct indices for a file imported through different modules.
            let mut files: std::collections::BTreeSet<_> = module
                .items
                .iter()
                .enumerate()
                .filter_map(|(ordinal, _)| {
                    Some((
                        indices.item_index(id, ordinal)?,
                        graph.item_source(id, ordinal)?,
                    ))
                })
                .collect();
            if let (Some(index), Some(path)) =
                (indices.module_base(id), module.source_paths.first())
            {
                files.insert((index, path));
            }
            for (index, path) in files {
                writeln!(out, "    file#{index} path={:?}", path.display())
                    .expect("write to string");
            }
        }
    }
    out
}

fn dump_ast(program: &Program, input: &Path) -> String {
    let mut out = dump_sources(program, input);
    out.push_str("ast root (expanded imports; before type checking)\n");
    for (ordinal, (item, span)) in program.items.iter().enumerate() {
        writeln!(
            out,
            "item#{ordinal} span={}..{}\n{item:#?}",
            span.start, span.end
        )
        .expect("write to string");
    }
    if let Some(graph) = &program.module_graph {
        for id in &graph.topo_order {
            if *id == graph.root {
                continue;
            }
            let Some(module) = graph.modules.get(id) else {
                continue;
            };
            writeln!(out, "ast module {:?}", id.dotted()).expect("write to string");
            for (ordinal, (item, span)) in module.items.iter().enumerate() {
                writeln!(
                    out,
                    "item#{ordinal} span={}..{}\n{item:#?}",
                    span.start, span.end
                )
                .expect("write to string");
            }
        }
    }
    out
}
