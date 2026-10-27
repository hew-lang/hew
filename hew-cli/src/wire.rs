//! Wire schema compatibility checks.

use std::collections::BTreeMap;

use hew_parser::ast::{Item, TypeDecl, TypeDeclKind};
use hew_types::{DefTable, ParamHead, ResolvedTy, TypeFactService, TypeHead};

// Snapshots of checked schema facts for comparison across independent compilations.

#[derive(Debug, PartialEq)]
enum WireSchemaKind {
    Struct,
    Enum,
}

#[derive(Debug)]
struct WireSchemaField {
    name: String,
    key: String,
    ty: SchemaType,
    field_number: u32,
    is_optional: bool,
    is_deprecated: bool,
}

#[derive(Debug)]
struct WireSchema {
    name: String,
    kind: WireSchemaKind,
    parameters: usize,
    fields: Vec<WireSchemaField>,
    variants: Vec<WireVariant>,
}

#[derive(Debug)]
struct WireVariant {
    name: String,
    tag: u32,
    payload: WirePayload,
}

#[derive(Debug)]
enum WirePayload {
    Unit,
    Tuple(Vec<SchemaType>),
    Struct(Vec<(String, SchemaType)>),
}

/// Equality uses the declaration's generic parameter positions, so renaming
/// a binder cannot change a schema. Diagnostics show those positions as well.
#[derive(Debug, Clone)]
struct SchemaType {
    spelling: String,
    identity: String,
}

impl SchemaType {
    fn new(ty: &ResolvedTy, parameters: &[ParamHead], defs: &DefTable) -> Self {
        Self {
            spelling: ty.user_facing().to_string(),
            identity: schema_type_identity(ty, parameters, defs),
        }
    }
}

impl PartialEq for SchemaType {
    fn eq(&self, other: &Self) -> bool {
        self.identity == other.identity
    }
}

impl std::fmt::Display for SchemaType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.identity == self.spelling {
            f.write_str(&self.spelling)
        } else {
            write!(f, "{} (schema {})", self.spelling, self.identity)
        }
    }
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
    let mut frontend =
        hew_compile::SourceAnalysisBatch::new(hew_compile::FrontendOptions::default());
    let current_wires = parse_wire_decls(current_path, &mut frontend)?;
    let baseline_wires = parse_wire_decls(baseline_path, &mut frontend)?;
    Ok(compare_wire_schemas(&current_wires, &baseline_wires))
}

fn parse_wire_decls(
    path: &str,
    frontend: &mut hew_compile::SourceAnalysisBatch,
) -> Result<Vec<VersionedWireSchema>, String> {
    let source =
        std::fs::read_to_string(path).map_err(|error| format!("cannot read {path}: {error}"))?;
    let state = frontend.run_source_frontend(&source, path);
    crate::compile::render_frontend_diagnostics(&state.diagnostics);
    if let Some(failure) = state.stopped {
        return Err(format!("cannot check {path}: {}", failure.message));
    }
    let checked = state
        .typecheck_result
        .as_ref()
        .and_then(|result| result.tco.as_ref())
        .ok_or_else(|| format!("Error: wire declarations in {path} have no checked type facts"))?;
    let program = checked
        .normalized_program
        .as_ref()
        .map_or(&state.program, |normalized| &normalized.program);
    let facts = TypeFactService::new(
        checked.type_fact_context.clone(),
        checked.type_facts.clone(),
    );
    program
        .items
        .iter()
        .enumerate()
        .filter_map(|(ordinal, (item, span))| {
            let Item::TypeDecl(declaration) = item else {
                return None;
            };
            declaration.wire.as_ref()?;
            let kind = match declaration.origin {
                hew_parser::ast::DeclarationOrigin::MachineState => {
                    hew_types::DeclarationKind::Machine
                }
                hew_parser::ast::DeclarationOrigin::MachineEventType { .. } => {
                    hew_types::DeclarationKind::MachineEventType
                }
                _ => hew_types::DeclarationKind::Type,
            };
            let occurrence = hew_types::DeclarationOccurrence::new_with_synthetic_ordinal(
                checked.defs.root_module(),
                span,
                ordinal,
                kind,
                0,
            );
            Some(
                checked
                    .defs
                    .nominal(occurrence)
                    .ok_or_else(|| {
                        format!("wire `{}` has no checked declaration", declaration.name)
                    })
                    .and_then(|identity| schema_from_declaration(declaration, identity, &facts)),
            )
        })
        .collect()
}

fn schema_from_declaration(
    td: &TypeDecl,
    identity: hew_types::NominalId,
    facts: &TypeFactService,
) -> Result<VersionedWireSchema, String> {
    let wire = td
        .wire
        .as_ref()
        .ok_or("wire declaration metadata is absent")?;
    let context = facts.context();
    let declaration = context
        .declarations()
        .get(&identity)
        .ok_or("wire declaration has no type facts")?;
    let layout = context
        .serial_layout(identity)
        .ok_or("wire declaration has no checked serialization layout")?;
    let parameters = &declaration.type_params;
    let ty = ResolvedTy::Named {
        head: TypeHead::Nominal(hew_types::NominalHead::new(identity, td.name.name.as_str())),
        args: parameters
            .iter()
            .map(|name| ResolvedTy::TypeParam { name: *name })
            .collect(),
        is_opaque: declaration.is_opaque,
    };
    let kind = match td.kind {
        TypeDeclKind::Struct => WireSchemaKind::Struct,
        TypeDeclKind::Enum => WireSchemaKind::Enum,
    };
    let mut fields = Vec::new();
    let mut variants = Vec::new();
    match kind {
        WireSchemaKind::Struct => {
            let (_, checked_fields) = facts.record_fields(&ty)?;
            if checked_fields.len() != layout.members.len() {
                return Err("wire field types disagree with their checked layout".into());
            }
            for ((name, ty), member) in checked_fields.iter().zip(&layout.members) {
                if member.flags & hew_codec::Member::SKIP != 0 {
                    continue;
                }
                let field_number = u32::try_from(member.tag)
                    .map_err(|_| "wire field tag exceeds its declaration range")?;
                fields.push(WireSchemaField {
                    name: name.clone(),
                    key: member.key.clone(),
                    ty: SchemaType::new(ty, parameters, facts.defs()),
                    field_number,
                    is_optional: member.flags & hew_codec::Member::ACCEPT_ABSENT != 0,
                    is_deprecated: wire
                        .field_meta
                        .iter()
                        .any(|meta| meta.field_number == field_number && meta.is_deprecated),
                });
            }
        }
        WireSchemaKind::Enum => {
            variants = schema_variants(&ty, layout, parameters, facts)?;
        }
    }
    Ok(VersionedWireSchema {
        schema: WireSchema {
            name: td.name.to_string(),
            kind,
            parameters: parameters.len(),
            fields,
            variants,
        },
        version: wire.version,
        min_version: wire.min_version,
        field_since: wire
            .field_meta
            .iter()
            .filter_map(|meta| meta.since.map(|since| (meta.field_number, since)))
            .collect(),
    })
}

fn schema_variants(
    ty: &ResolvedTy,
    layout: &hew_types::data_shape::SerialLayout,
    parameters: &[ParamHead],
    facts: &TypeFactService,
) -> Result<Vec<WireVariant>, String> {
    let mut variants = Vec::new();
    let checked_variants = facts.data_variants(ty)?;
    if checked_variants.len() != layout.members.len() {
        return Err("wire variant types disagree with their checked layout".into());
    }
    for (variant, member) in checked_variants.iter().zip(&layout.members) {
        let payload = if variant.named {
            if variant.fields.len() != member.fields.len() {
                return Err("wire variant fields disagree with their checked layout".into());
            }
            WirePayload::Struct(
                variant
                    .fields
                    .iter()
                    .zip(&member.fields)
                    .map(|((_, ty), field)| {
                        (
                            field.key.clone(),
                            SchemaType::new(ty, parameters, facts.defs()),
                        )
                    })
                    .collect(),
            )
        } else if member.flags & hew_codec::Member::PAYLOAD != 0 {
            WirePayload::Tuple(
                variant
                    .fields
                    .iter()
                    .map(|(_, ty)| SchemaType::new(ty, parameters, facts.defs()))
                    .collect(),
            )
        } else {
            WirePayload::Unit
        };
        variants.push(WireVariant {
            name: member.key.clone(),
            tag: u32::try_from(member.tag)
                .map_err(|_| "wire variant tag exceeds its declaration range")?,
            payload,
        });
    }
    Ok(variants)
}

fn compare_wire_schemas(
    current: &[VersionedWireSchema],
    baseline: &[VersionedWireSchema],
) -> CompatibilityReport {
    let mut report = CompatibilityReport::default();

    let current_by_name = build_wire_map(current);
    let baseline_by_name = build_wire_map(baseline);

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
            if baseline_vws.schema.parameters != current_vws.schema.parameters {
                report.errors.push(format!(
                    "wire `{name}` changed generic parameter count: {} -> {}",
                    baseline_vws.schema.parameters, current_vws.schema.parameters
                ));
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

fn build_wire_map(decls: &[VersionedWireSchema]) -> BTreeMap<&str, &VersionedWireSchema> {
    decls
        .iter()
        .map(|decl| (decl.schema.name.as_str(), decl))
        .collect()
}

fn compare_wire_struct(
    wire_name: &str,
    current: &VersionedWireSchema,
    baseline: &VersionedWireSchema,
    report: &mut CompatibilityReport,
) {
    let current_fields = build_field_map(&current.schema);
    let baseline_fields = build_field_map(&baseline.schema);

    for (number, current_field) in &current_fields {
        if let Some(baseline_field) = baseline_fields.get(number) {
            if current_field.key != baseline_field.key {
                report.errors.push(format!(
                    "reused field number @{number} in `{wire_name}`: `{}` became `{}`",
                    baseline_field.key, current_field.key
                ));
            }
            if current_field.ty != baseline_field.ty {
                report.errors.push(format!(
                    "changed field type for `{wire_name}.{} @{number}`: `{}` -> `{}`",
                    current_field.name, baseline_field.ty, current_field.ty
                ));
            }
            if baseline_field.is_optional != current_field.is_optional {
                let baseline_presence = if baseline_field.is_optional {
                    "optional"
                } else {
                    "required"
                };
                let current_presence = if current_field.is_optional {
                    "optional"
                } else {
                    "required"
                };
                report.errors.push(format!(
                    "changed field presence for `{wire_name}.{} @{number}`: \
                     `{baseline_presence}` -> `{current_presence}`",
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

/// Variants match by their stable `@N` tag, so reordering is compatible;
/// a removed, added or renamed tag is not (text formats key by name).
fn compare_wire_enum(
    wire_name: &str,
    current: &VersionedWireSchema,
    baseline: &VersionedWireSchema,
    report: &mut CompatibilityReport,
) {
    for baseline_variant in &baseline.schema.variants {
        let tag = baseline_variant.tag;
        let Some(current_variant) = current
            .schema
            .variants
            .iter()
            .find(|variant| variant.tag == baseline_variant.tag)
        else {
            report.errors.push(format!(
                "removed variant `{wire_name}.{}` @{tag}",
                baseline_variant.name
            ));
            continue;
        };
        if current_variant.name != baseline_variant.name {
            report.errors.push(format!(
                "renamed variant @{tag} of `{wire_name}`: `{}` -> `{}`",
                baseline_variant.name, current_variant.name
            ));
            continue;
        }
        compare_wire_enum_variant_payload(wire_name, current_variant, baseline_variant, report);
    }
    for variant in &current.schema.variants {
        if !baseline
            .schema
            .variants
            .iter()
            .any(|baseline_variant| baseline_variant.tag == variant.tag)
        {
            report.errors.push(format!(
                "added variant `{wire_name}.{}` @{}",
                variant.name, variant.tag
            ));
        }
    }
}

fn compare_wire_enum_variant_payload(
    wire_name: &str,
    current_variant: &WireVariant,
    baseline_variant: &WireVariant,
    report: &mut CompatibilityReport,
) {
    match (&current_variant.payload, &baseline_variant.payload) {
        (WirePayload::Unit, WirePayload::Unit) => {}
        (WirePayload::Tuple(current), WirePayload::Tuple(baseline)) => {
            compare_wire_enum_payload_types(
                wire_name,
                &current_variant.name,
                current.iter().cloned(),
                baseline.iter().cloned(),
                report,
            );
        }
        (WirePayload::Struct(current), WirePayload::Struct(baseline)) => {
            if current.len() != baseline.len() {
                report.errors.push(format!(
                    "changed payload arity for `{wire_name}.{}`: {} -> {}",
                    current_variant.name,
                    baseline.len(),
                    current.len()
                ));
                return;
            }
            for (index, ((current_name, current_ty), (baseline_name, baseline_ty))) in
                current.iter().zip(baseline).enumerate()
            {
                let position = index + 1;
                if current_name != baseline_name {
                    report.errors.push(format!("changed payload field name for `{wire_name}.{}` item {position}: `{baseline_name}` -> `{current_name}`", current_variant.name));
                }
                if current_ty != baseline_ty {
                    report.errors.push(format!("changed payload type for `{wire_name}.{}` item {position}: `{baseline_ty}` -> `{current_ty}`", current_variant.name));
                }
            }
        }
        _ => report.errors.push(format!(
            "changed payload shape for `{wire_name}.{}`",
            current_variant.name
        )),
    }
}

fn build_field_map(schema: &WireSchema) -> BTreeMap<u32, &WireSchemaField> {
    schema
        .fields
        .iter()
        .map(|field| (field.field_number, field))
        .collect()
}

fn compare_wire_enum_payload_types(
    wire_name: &str,
    variant_name: &str,
    current_payload: impl Iterator<Item = SchemaType>,
    baseline_payload: impl Iterator<Item = SchemaType>,
    report: &mut CompatibilityReport,
) {
    let current_payload: Vec<SchemaType> = current_payload.collect();
    let baseline_payload: Vec<SchemaType> = baseline_payload.collect();

    if current_payload.len() != baseline_payload.len() {
        report.errors.push(format!(
            "changed payload arity for `{wire_name}.{variant_name}`: {} -> {}",
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
                "changed payload type for `{wire_name}.{variant_name}` item {payload_position}: `{baseline_ty}` -> `{current_ty}`"
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

fn schema_type_identity(ty: &ResolvedTy, parameters: &[ParamHead], defs: &DefTable) -> String {
    match ty {
        ResolvedTy::TypeParam { name } => parameters
            .iter()
            .position(|parameter| parameter == name)
            .map_or_else(
                || ty.user_facing().to_string(),
                |position| format!("$parameter{position}"),
            ),
        ResolvedTy::Named { head, args, .. } => {
            let name = match head {
                TypeHead::Nominal(identity) => defs.path(identity.id.declaration()).to_string(),
                _ => ResolvedTy::Named {
                    head: *head,
                    args: Vec::new(),
                    is_opaque: false,
                }
                .user_facing()
                .to_string(),
            };
            if args.is_empty() {
                name
            } else {
                format!(
                    "{name}<{}>",
                    args.iter()
                        .map(|arg| schema_type_identity(arg, parameters, defs))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
        ResolvedTy::Tuple(items) => format!(
            "({})",
            items
                .iter()
                .map(|item| schema_type_identity(item, parameters, defs))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        ResolvedTy::Array(element, size) => format!(
            "[{}; {size}]",
            schema_type_identity(element, parameters, defs)
        ),
        _ => ty.user_facing().to_string(),
    }
}
