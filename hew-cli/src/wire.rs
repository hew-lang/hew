//! Wire schema compatibility checks.

use std::collections::BTreeMap;

use hew_parser::ast::{
    Item, NamingCase, TypeBodyItem, TypeDeclKind, TypeExpr, VariantDecl, VariantKind,
};

// Private schema types — mirror the information extracted from `Item::TypeDecl` with `#[wire]`.
// These exist so the compatibility logic does not depend on parser AST types directly.

#[derive(Debug, PartialEq)]
enum WireSchemaKind {
    Struct,
    Enum,
}

#[derive(Debug)]
struct WireSchemaField {
    name: String,
    ty: String,
    field_number: u32,
    is_optional: bool,
    is_repeated: bool,
    is_deprecated: bool,
    // Reserved for future cross-schema encoding compatibility checks.
    #[allow(
        dead_code,
        reason = "reserved for future cross-schema encoding compatibility checks"
    )]
    json_name: Option<String>,
    #[allow(
        dead_code,
        reason = "reserved for future cross-schema encoding compatibility checks"
    )]
    yaml_name: Option<String>,
    #[allow(
        dead_code,
        reason = "reserved for future cross-schema encoding compatibility checks"
    )]
    since: Option<u32>,
}

#[derive(Debug)]
struct WireSchema {
    name: String,
    kind: WireSchemaKind,
    fields: Vec<WireSchemaField>,
    variants: Vec<VariantDecl>,
    // Reserved for future cross-schema encoding compatibility checks.
    #[allow(
        dead_code,
        reason = "reserved for future cross-schema encoding compatibility checks"
    )]
    json_case: Option<NamingCase>,
    #[allow(
        dead_code,
        reason = "reserved for future cross-schema encoding compatibility checks"
    )]
    yaml_case: Option<NamingCase>,
}

/// A wire schema with optional version metadata.
struct VersionedWireSchema {
    schema: WireSchema,
    version: Option<u32>,
    min_version: Option<u32>,
    /// Per-field `since` version, keyed by field number.
    field_since: BTreeMap<u32, u32>,
}

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

pub fn cmd_wire(args: &crate::args::WireCommand) {
    match &args.command {
        crate::args::WireSubcommand::Check(check_args) => {
            let input = check_args.input.display().to_string();
            let against = check_args.against.display().to_string();
            let report = match run_wire_check(&input, &against) {
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
    }
}

fn run_wire_check(current_path: &str, baseline_path: &str) -> Result<CompatibilityReport, String> {
    let current_wires = parse_wire_decls(current_path)?;
    let baseline_wires = parse_wire_decls(baseline_path)?;
    Ok(compare_wire_schemas(&current_wires, &baseline_wires))
}

fn parse_wire_decls(path: &str) -> Result<Vec<VersionedWireSchema>, String> {
    let source =
        std::fs::read_to_string(path).map_err(|e| format!("Error: cannot read {path}: {e}"))?;
    let parsed = hew_parser::parse(&source);

    if !parsed.errors.is_empty() {
        let details = parsed
            .errors
            .iter()
            .map(|e| e.message.clone())
            .collect::<Vec<_>>()
            .join("\n");
        return Err(format!("Error: cannot parse {path}:\n{details}"));
    }

    Ok(parsed
        .program
        .items
        .into_iter()
        .filter_map(|(item, _)| match item {
            Item::TypeDecl(td) if td.wire.is_some() => {
                let wire = td.wire.unwrap();
                let kind = match td.kind {
                    TypeDeclKind::Struct => WireSchemaKind::Struct,
                    TypeDeclKind::Enum => WireSchemaKind::Enum,
                };
                // Build a map of field name → type string from the body
                let field_types: std::collections::HashMap<String, String> = td
                    .body
                    .iter()
                    .filter_map(|item| match item {
                        TypeBodyItem::Field { name, ty, .. } => {
                            Some((name.clone(), type_expr_to_string(&ty.0)))
                        }
                        _ => None,
                    })
                    .collect();
                let variants: Vec<VariantDecl> = td
                    .body
                    .iter()
                    .filter_map(|item| match item {
                        TypeBodyItem::Variant(variant) => Some(variant.clone()),
                        _ => None,
                    })
                    .collect();
                let field_since: BTreeMap<u32, u32> = wire
                    .field_meta
                    .iter()
                    .filter_map(|fm| fm.since.map(|s| (fm.field_number, s)))
                    .collect();
                let version = wire.version;
                let min_version = wire.min_version;
                let fields = wire
                    .field_meta
                    .into_iter()
                    .map(|fm| {
                        let ty = field_types.get(&fm.field_name).cloned().unwrap_or_default();
                        WireSchemaField {
                            name: fm.field_name,
                            ty,
                            field_number: fm.field_number,
                            is_optional: fm.is_optional,
                            is_repeated: fm.is_repeated,
                            is_deprecated: fm.is_deprecated,
                            json_name: fm.json_name,
                            yaml_name: fm.yaml_name,
                            since: fm.since,
                        }
                    })
                    .collect();
                Some(VersionedWireSchema {
                    schema: WireSchema {
                        name: td.name,
                        kind,
                        fields,
                        variants,
                        json_case: wire.json_case,
                        yaml_case: wire.yaml_case,
                    },
                    version,
                    min_version,
                    field_since,
                })
            }
            _ => None,
        })
        .collect())
}

fn compare_wire_schemas(
    current: &[VersionedWireSchema],
    baseline: &[VersionedWireSchema],
) -> CompatibilityReport {
    let mut report = CompatibilityReport::default();

    let current_by_name = build_wire_map(current, "current schema", &mut report);
    let baseline_by_name = build_wire_map(baseline, "baseline schema", &mut report);

    for (name, current_vws) in &current_by_name {
        // Report version progression
        if let Some(v) = current_vws.version {
            if let Some(baseline_vws) = baseline_by_name.get(name) {
                if let Some(bv) = baseline_vws.version {
                    if v > bv {
                        report
                            .warnings
                            .push(format!("wire `{name}` version advanced from {bv} to {v}"));
                    }
                }
            }
        }

        // Check min_version vs baseline version
        if let (Some(min_v), Some(baseline_vws)) =
            (current_vws.min_version, baseline_by_name.get(name))
        {
            if let Some(bv) = baseline_vws.version {
                if min_v > bv {
                    report.errors.push(format!(
                        "wire `{name}`: min_version {min_v} is higher than baseline version \
                         {bv} — old clients cannot decode"
                    ));
                }
            }
        }

        if let Some(baseline_vws) = baseline_by_name.get(name) {
            if baseline_vws.schema.kind != current_vws.schema.kind {
                report.errors.push(format!(
                    "wire `{name}` changed declaration kind from {:?} to {:?}",
                    baseline_vws.schema.kind, current_vws.schema.kind
                ));
                continue;
            }
            match current_vws.schema.kind {
                WireSchemaKind::Struct => {
                    compare_wire_struct(name, current_vws, baseline_vws, &mut report);
                }
                WireSchemaKind::Enum => {
                    compare_wire_enum(name, current_vws, baseline_vws, &mut report);
                }
            }
        } else if current_vws.schema.kind == WireSchemaKind::Struct {
            warn_new_required_and_deprecated_fields(name, &current_vws.schema, &mut report);
        }
    }

    for (name, baseline_vws) in &baseline_by_name {
        if current_by_name.contains_key(name) {
            continue;
        }
        match baseline_vws.schema.kind {
            WireSchemaKind::Struct => {
                for field in baseline_vws.schema.fields.iter().filter(|f| !f.is_optional) {
                    report.errors.push(format!(
                        "removed required field `{name}.{} @{}` (wire type removed)",
                        field.name, field.field_number
                    ));
                }
            }
            WireSchemaKind::Enum => {
                report
                    .errors
                    .push(format!("removed wire enum `{name}` (wire type removed)"));
            }
        }
    }

    report
}

fn build_wire_map<'a>(
    decls: &'a [VersionedWireSchema],
    schema_name: &str,
    report: &mut CompatibilityReport,
) -> BTreeMap<String, &'a VersionedWireSchema> {
    let mut by_name: BTreeMap<String, &'a VersionedWireSchema> = BTreeMap::new();
    for vws in decls {
        if by_name.insert(vws.schema.name.clone(), vws).is_some() {
            report.errors.push(format!(
                "{schema_name}: duplicate wire declaration `{}`",
                vws.schema.name
            ));
        }
    }
    by_name
}

fn compare_wire_struct(
    wire_name: &str,
    current: &VersionedWireSchema,
    baseline: &VersionedWireSchema,
    report: &mut CompatibilityReport,
) {
    let current_fields = build_field_map(wire_name, &current.schema, "current schema", report);
    let baseline_fields = build_field_map(wire_name, &baseline.schema, "baseline schema", report);

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
        } else {
            // New field — suppress "no default" warning if it has `since` and
            // that version is newer than the baseline schema version
            let is_expected_new = current
                .field_since
                .get(number)
                .is_some_and(|since| baseline.version.is_some_and(|bv| *since > bv));
            if !current_field.is_optional && !is_expected_new {
                report.warnings.push(format!(
                    "new required field `{wire_name}.{} @{number}` has no default",
                    current_field.name
                ));
            }
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

fn compare_wire_enum(
    wire_name: &str,
    current: &VersionedWireSchema,
    baseline: &VersionedWireSchema,
    report: &mut CompatibilityReport,
) {
    for (index, (current_variant, baseline_variant)) in current
        .schema
        .variants
        .iter()
        .zip(&baseline.schema.variants)
        .enumerate()
    {
        let position = index + 1;
        if current_variant.name != baseline_variant.name {
            report.errors.push(format!(
                "changed variant order for `{wire_name}` at position {position}: `{}` -> `{}`",
                baseline_variant.name, current_variant.name
            ));
            continue;
        }
        compare_wire_enum_variant_payload(wire_name, current_variant, baseline_variant, report);
    }

    if current.schema.variants.len() > baseline.schema.variants.len() {
        for variant in &current.schema.variants[baseline.schema.variants.len()..] {
            report
                .errors
                .push(format!("added variant `{wire_name}::{}`", variant.name));
        }
    } else if baseline.schema.variants.len() > current.schema.variants.len() {
        for variant in &baseline.schema.variants[current.schema.variants.len()..] {
            report
                .errors
                .push(format!("removed variant `{wire_name}::{}`", variant.name));
        }
    }
}

fn compare_wire_enum_variant_payload(
    wire_name: &str,
    current_variant: &VariantDecl,
    baseline_variant: &VariantDecl,
    report: &mut CompatibilityReport,
) {
    match (&current_variant.kind, &baseline_variant.kind) {
        (VariantKind::Unit, VariantKind::Unit) => {}
        (VariantKind::Tuple(current_fields), VariantKind::Tuple(baseline_fields)) => {
            compare_wire_enum_payload_types(
                wire_name,
                &current_variant.name,
                current_fields
                    .iter()
                    .map(|field| type_expr_to_string(&field.0)),
                baseline_fields
                    .iter()
                    .map(|field| type_expr_to_string(&field.0)),
                report,
            );
        }
        (VariantKind::Struct(current_fields), VariantKind::Struct(baseline_fields)) => {
            if current_fields.len() != baseline_fields.len() {
                report.errors.push(format!(
                    "changed payload arity for `{wire_name}::{}`: {} -> {}",
                    current_variant.name,
                    baseline_fields.len(),
                    current_fields.len()
                ));
                return;
            }

            for (index, ((current_name, current_ty), (baseline_name, baseline_ty))) in
                current_fields.iter().zip(baseline_fields).enumerate()
            {
                let payload_position = index + 1;
                if current_name != baseline_name {
                    report.errors.push(format!(
                        "changed payload field name for `{wire_name}::{}` item {payload_position}: `{}` -> `{}`",
                        current_variant.name, baseline_name, current_name
                    ));
                }

                let current_ty = type_expr_to_string(&current_ty.0);
                let baseline_ty = type_expr_to_string(&baseline_ty.0);
                if current_ty != baseline_ty {
                    report.errors.push(format!(
                        "changed payload type for `{wire_name}::{}` item {payload_position}: `{}` -> `{}`",
                        current_variant.name, baseline_ty, current_ty
                    ));
                }
            }
        }
        _ => {
            report.errors.push(format!(
                "changed payload shape for `{wire_name}::{}`",
                current_variant.name
            ));
        }
    }
}

fn build_field_map<'a>(
    wire_name: &str,
    schema: &'a WireSchema,
    schema_name: &str,
    report: &mut CompatibilityReport,
) -> BTreeMap<u32, &'a WireSchemaField> {
    let mut by_number: BTreeMap<u32, &'a WireSchemaField> = BTreeMap::new();
    for field in &schema.fields {
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

fn field_type_changed(current: &WireSchemaField, baseline: &WireSchemaField) -> bool {
    current.ty != baseline.ty || current.is_repeated != baseline.is_repeated
}

fn describe_field_type(field: &WireSchemaField) -> String {
    if field.is_repeated {
        format!("repeated {}", field.ty)
    } else {
        field.ty.clone()
    }
}

fn compare_wire_enum_payload_types(
    wire_name: &str,
    variant_name: &str,
    current_payload: impl Iterator<Item = String>,
    baseline_payload: impl Iterator<Item = String>,
    report: &mut CompatibilityReport,
) {
    let current_payload: Vec<String> = current_payload.collect();
    let baseline_payload: Vec<String> = baseline_payload.collect();

    if current_payload.len() != baseline_payload.len() {
        report.errors.push(format!(
            "changed payload arity for `{wire_name}::{variant_name}`: {} -> {}",
            baseline_payload.len(),
            current_payload.len()
        ));
        return;
    }

    for (index, (current_ty, baseline_ty)) in
        current_payload.iter().zip(&baseline_payload).enumerate()
    {
        if current_ty != baseline_ty {
            let payload_position = index + 1;
            report.errors.push(format!(
                "changed payload type for `{wire_name}::{variant_name}` item {payload_position}: `{baseline_ty}` -> `{current_ty}`"
            ));
        }
    }
}

fn warn_new_required_and_deprecated_fields(
    wire_name: &str,
    schema: &WireSchema,
    report: &mut CompatibilityReport,
) {
    for field in &schema.fields {
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

fn type_expr_to_string(te: &TypeExpr) -> String {
    match te {
        TypeExpr::Named {
            name,
            type_args: Some(args),
        } => {
            let arg_strs: Vec<String> = args.iter().map(|a| type_expr_to_string(&a.0)).collect();
            format!("{name}<{}>", arg_strs.join(", "))
        }
        TypeExpr::Named {
            name,
            type_args: None,
        } => name.clone(),
        TypeExpr::Option(inner) => format!("Option<{}>", type_expr_to_string(&inner.0)),
        TypeExpr::Tuple(items) => {
            let parts: Vec<String> = items.iter().map(|i| type_expr_to_string(&i.0)).collect();
            format!("({})", parts.join(", "))
        }
        _ => String::from("?"),
    }
}
