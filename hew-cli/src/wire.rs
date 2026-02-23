//! Wire schema compatibility checks.

use std::collections::BTreeMap;

use hew_parser::ast::{Item, WireDecl, WireDeclKind, WireFieldDecl};

#[derive(Debug, Default)]
struct CompatibilityReport {
    errors: Vec<String>,
    warnings: Vec<String>,
}

impl CompatibilityReport {
    fn print(&self) {
        for error in &self.errors {
            eprintln!("error: {error}");
        }
        for warning in &self.warnings {
            eprintln!("warning: {warning}");
        }
        eprintln!(
            "wire check: {} error(s), {} warning(s)",
            self.errors.len(),
            self.warnings.len()
        );
    }

    fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
}

pub fn cmd_wire(args: &[String]) {
    if args.is_empty() {
        print_wire_usage();
        std::process::exit(1);
    }

    match args[0].as_str() {
        "check" => cmd_wire_check(&args[1..]),
        "help" | "--help" | "-h" => print_wire_usage(),
        other => {
            eprintln!("Unknown wire subcommand: {other}");
            print_wire_usage();
            std::process::exit(1);
        }
    }
}

fn cmd_wire_check(args: &[String]) {
    if args.is_empty() {
        eprintln!("Error: no input file specified");
        print_wire_usage();
        std::process::exit(1);
    }

    let input = &args[0];
    let mut against: Option<&str> = None;
    let mut i = 1;

    while i < args.len() {
        match args[i].as_str() {
            "--against" => {
                i += 1;
                if i >= args.len() {
                    eprintln!("Error: --against requires a schema path");
                    std::process::exit(1);
                }
                against = Some(&args[i]);
            }
            other => {
                eprintln!("Error: unexpected argument '{other}'");
                print_wire_usage();
                std::process::exit(1);
            }
        }
        i += 1;
    }

    let Some(against) = against else {
        eprintln!("Error: missing required option --against <baseline.hew>");
        print_wire_usage();
        std::process::exit(1);
    };

    let report = match run_wire_check(input, against) {
        Ok(report) => report,
        Err(error) => {
            eprintln!("{error}");
            std::process::exit(1);
        }
    };

    report.print();
    if report.has_errors() {
        std::process::exit(1);
    }
}

fn run_wire_check(current_path: &str, baseline_path: &str) -> Result<CompatibilityReport, String> {
    let current_wires = parse_wire_decls(current_path)?;
    let baseline_wires = parse_wire_decls(baseline_path)?;
    Ok(compare_wire_schemas(&current_wires, &baseline_wires))
}

fn parse_wire_decls(path: &str) -> Result<Vec<WireDecl>, String> {
    let source =
        std::fs::read_to_string(path).map_err(|e| format!("Error: cannot read {path}: {e}"))?;
    let parsed = hew_parser::parse(&source);

    if !parsed.errors.is_empty() {
        let details = parsed
            .errors
            .iter()
            .map(|e| format!("{e:?}"))
            .collect::<Vec<_>>()
            .join("\n");
        return Err(format!("Error: cannot parse {path}:\n{details}"));
    }

    Ok(parsed
        .program
        .items
        .into_iter()
        .filter_map(|(item, _)| match item {
            Item::Wire(decl) => Some(decl),
            _ => None,
        })
        .collect())
}

fn compare_wire_schemas(current: &[WireDecl], baseline: &[WireDecl]) -> CompatibilityReport {
    let mut report = CompatibilityReport::default();

    let current_by_name = build_wire_map(current, "current schema", &mut report);
    let baseline_by_name = build_wire_map(baseline, "baseline schema", &mut report);

    for (name, current_decl) in &current_by_name {
        if current_decl.kind != WireDeclKind::Struct {
            continue;
        }

        if let Some(baseline_decl) = baseline_by_name.get(name) {
            if baseline_decl.kind != current_decl.kind {
                report.errors.push(format!(
                    "wire `{name}` changed declaration kind from {:?} to {:?}",
                    baseline_decl.kind, current_decl.kind
                ));
                continue;
            }
            compare_wire_struct(name, current_decl, baseline_decl, &mut report);
        } else {
            warn_new_required_and_deprecated_fields(name, current_decl, &mut report);
        }
    }

    for (name, baseline_decl) in &baseline_by_name {
        if baseline_decl.kind != WireDeclKind::Struct || current_by_name.contains_key(name) {
            continue;
        }
        for field in baseline_decl.fields.iter().filter(|f| !f.is_optional) {
            report.errors.push(format!(
                "removed required field `{name}.{} @{}` (wire type removed)",
                field.name, field.field_number
            ));
        }
    }

    report
}

fn build_wire_map<'a>(
    decls: &'a [WireDecl],
    schema_name: &str,
    report: &mut CompatibilityReport,
) -> BTreeMap<String, &'a WireDecl> {
    let mut by_name: BTreeMap<String, &'a WireDecl> = BTreeMap::new();
    for decl in decls {
        if by_name.insert(decl.name.clone(), decl).is_some() {
            report.errors.push(format!(
                "{schema_name}: duplicate wire declaration `{}`",
                decl.name
            ));
        }
    }
    by_name
}

fn compare_wire_struct(
    wire_name: &str,
    current: &WireDecl,
    baseline: &WireDecl,
    report: &mut CompatibilityReport,
) {
    let current_fields = build_field_map(wire_name, current, "current schema", report);
    let baseline_fields = build_field_map(wire_name, baseline, "baseline schema", report);

    for (number, current_field) in &current_fields {
        if let Some(baseline_field) = baseline_fields.get(number) {
            if current_field.name != baseline_field.name {
                report.errors.push(format!(
                    "reused field number @{number} in `{wire_name}`: `{}` became `{}`",
                    baseline_field.name, current_field.name
                ));
            }
            if field_type_changed(current_field, baseline_field) {
                report.errors.push(format!(
                    "changed field type for `{wire_name}.{} @{number}`: `{}` -> `{}`",
                    current_field.name,
                    describe_field_type(baseline_field),
                    describe_field_type(current_field)
                ));
            }
            if baseline_field.is_optional && !current_field.is_optional {
                report.warnings.push(format!(
                    "new required field `{wire_name}.{} @{number}` has no default",
                    current_field.name
                ));
            }
        } else if !current_field.is_optional {
            report.warnings.push(format!(
                "new required field `{wire_name}.{} @{number}` has no default",
                current_field.name
            ));
        }

        if current_field.is_deprecated {
            report.warnings.push(format!(
                "deprecated field usage: `{wire_name}.{} @{number}`",
                current_field.name
            ));
        }
    }

    for (number, baseline_field) in &baseline_fields {
        if !baseline_field.is_optional && !current_fields.contains_key(number) {
            report.errors.push(format!(
                "removed required field `{wire_name}.{} @{number}`",
                baseline_field.name
            ));
        }
    }
}

fn build_field_map<'a>(
    wire_name: &str,
    decl: &'a WireDecl,
    schema_name: &str,
    report: &mut CompatibilityReport,
) -> BTreeMap<u32, &'a WireFieldDecl> {
    let mut by_number: BTreeMap<u32, &'a WireFieldDecl> = BTreeMap::new();
    for field in &decl.fields {
        if let Some(existing) = by_number.get(&field.field_number) {
            report.errors.push(format!(
                "{schema_name}: wire `{wire_name}` reuses field number @{} for `{}` and `{}`",
                field.field_number, existing.name, field.name
            ));
            continue;
        }
        by_number.insert(field.field_number, field);
    }
    by_number
}

fn field_type_changed(current: &WireFieldDecl, baseline: &WireFieldDecl) -> bool {
    current.ty != baseline.ty || current.is_repeated != baseline.is_repeated
}

fn describe_field_type(field: &WireFieldDecl) -> String {
    if field.is_repeated {
        format!("repeated {}", field.ty)
    } else {
        field.ty.clone()
    }
}

fn warn_new_required_and_deprecated_fields(
    wire_name: &str,
    decl: &WireDecl,
    report: &mut CompatibilityReport,
) {
    for field in &decl.fields {
        if !field.is_optional {
            report.warnings.push(format!(
                "new required field `{wire_name}.{} @{}` has no default",
                field.name, field.field_number
            ));
        }
        if field.is_deprecated {
            report.warnings.push(format!(
                "deprecated field usage: `{wire_name}.{} @{}`",
                field.name, field.field_number
            ));
        }
    }
}

fn print_wire_usage() {
    eprintln!(
        "\
Usage:
  hew wire check <file.hew> --against <baseline.hew>
"
    );
}
