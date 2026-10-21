//! Internal protocol bindings consume the ordinary checker's closed data plans.
use std::collections::BTreeMap;
use std::fmt::Write as _;

use hew_types::data_plan::{DataKind, DataPlan};
use hew_types::{module_registry::ModuleRegistry, Checker, ResolvedTy, TypeFactService};

const SOURCE: &str = "hew-runtime/protocol/node_controls.hew";
const RUST: &str = "hew-runtime/src/node_protocol_generated.rs";
const CDDL: &str = "hew-runtime/schemas/node-controls.generated.cddl";
const ROOTS: &[&str] = &[
    "ServiceOffer",
    "MonitorDown",
    "RegistryGossip",
    "SwimControl",
    "ClusterEnrollment",
    "ClusterMember",
    "NodeHandshake",
];

pub fn run(args: &[String]) -> crate::Result<()> {
    let [mode] = args else {
        return Err("node-protocol requires --write or --check".into());
    };
    if mode != "--write" && mode != "--check" {
        return Err("node-protocol requires --write or --check".into());
    }
    let root = crate::workspace_root()?;
    let source = std::fs::read_to_string(root.join(SOURCE)).map_err(|error| error.to_string())?;
    let parsed = hew_parser::parse(&source);
    if !parsed.errors.is_empty() {
        return Err(format!("{SOURCE}: {:?}", parsed.errors));
    }
    let output = Checker::new(ModuleRegistry::new(Vec::new())).check_program(&parsed.program);
    if !output.errors.is_empty() {
        return Err(format!("{SOURCE}: {:?}", output.errors));
    }
    let service = TypeFactService::new(output.type_fact_context, output.type_facts);
    let mut plans = BTreeMap::new();
    for name in ROOTS {
        let ty = ResolvedTy::named_path(service.defs(), name, vec![]);
        plans.extend(service.data_plans(&ty, false)?.plans);
    }
    let generator = Generator {
        service: &service,
        plans: &plans,
    };
    let (rust, cddl) = generator.generate()?;
    // Use the supported Rust formatter, rather than a second formatting policy.
    let mut formatter = std::process::Command::new("rustfmt")
        .args(["--edition", "2021", "--emit", "stdout"])
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .spawn()
        .map_err(|error| error.to_string())?;
    std::io::Write::write_all(
        formatter
            .stdin
            .take()
            .as_mut()
            .ok_or("rustfmt stdin unavailable")?,
        rust.as_bytes(),
    )
    .map_err(|error| error.to_string())?;
    let output = formatter
        .wait_with_output()
        .map_err(|error| error.to_string())?;
    if !output.status.success() {
        return Err("generated protocol bindings failed rustfmt".into());
    }
    for (path, content) in [(RUST, output.stdout), (CDDL, cddl.into_bytes())] {
        if mode == "--write" {
            if std::fs::read(root.join(path)).ok().as_ref() != Some(&content) {
                std::fs::write(root.join(path), content).map_err(|error| error.to_string())?;
            }
        } else if std::fs::read(root.join(path)).map_err(|error| error.to_string())? != content {
            return Err(format!("{path} is stale; run make node-protocol"));
        }
    }
    Ok(())
}

struct Generator<'a> {
    service: &'a TypeFactService,
    plans: &'a BTreeMap<ResolvedTy, DataPlan<(), ()>>,
}

fn cddl_name(name: &str) -> String {
    let mut result = String::new();
    for (index, ch) in name.chars().enumerate() {
        if ch.is_uppercase() && index != 0 {
            result.push('-');
        }
        result.extend(ch.to_lowercase());
    }
    result
}

impl Generator<'_> {
    fn kind(&self, ty: &ResolvedTy) -> crate::Result<&DataKind<(), ()>> {
        self.plans
            .get(ty)
            .map(|plan| &plan.kind)
            .ok_or_else(|| format!("missing checked child plan: {ty}"))
    }

    fn rust_type(&self, ty: &ResolvedTy) -> crate::Result<String> {
        Ok(match self.kind(ty)? {
            DataKind::Scalar => match ty {
                ResolvedTy::String => "String".into(),
                ResolvedTy::Bytes => "Vec<u8>".into(),
                ResolvedTy::U8
                | ResolvedTy::U16
                | ResolvedTy::U32
                | ResolvedTy::U64
                | ResolvedTy::I32 => ty.to_string(),
                _ => return Err(format!("protocol scalar is not supported: {ty}")),
            },
            DataKind::Record { table: Some(_), .. } => ty.to_string(),
            DataKind::Vector(value) => format!("Vec<{}>", self.rust_type(value)?),
            DataKind::Option { value, .. } => format!("Option<{}>", self.rust_type(value)?),
            _ => return Err(format!("protocol data shape is not supported: {ty}")),
        })
    }

    fn encode(&self, ty: &ResolvedTy, value: &str) -> crate::Result<String> {
        Ok(match self.kind(ty)? {
            DataKind::Scalar => match ty {
                ResolvedTy::String => format!("sink.str({value});"),
                ResolvedTy::Bytes => format!("sink.bytes({value});"),
                ResolvedTy::U8 | ResolvedTy::U16 | ResolvedTy::U32 => {
                    format!("sink.u64(u64::from(*{value}));")
                }
                ResolvedTy::U64 => format!("sink.u64(*{value});"),
                ResolvedTy::I32 => format!("sink.i64(i64::from(*{value}));"),
                _ => return Err(format!("protocol scalar is not supported: {ty}")),
            },
            DataKind::Record { .. } => format!("({value}).write(sink);"),
            DataKind::Vector(child) => format!(
                "sink.seq_begin(({value}).len()); for value in {value} {{ {} }} sink.seq_end();",
                self.encode(child, "value")?
            ),
            DataKind::Option { value: child, .. } => format!(
                "match {value} {{ Some(value) => {{ {} }} None => sink.null(), }}",
                self.encode(child, "value")?
            ),
            _ => return Err(format!("protocol data shape is not supported: {ty}")),
        })
    }

    fn decode(&self, ty: &ResolvedTy) -> crate::Result<String> {
        Ok(match self.kind(ty)? {
            DataKind::Scalar => match ty {
                ResolvedTy::String => "source.read_str()?".into(),
                ResolvedTy::Bytes => "source.read_bytes()?".into(),
                ResolvedTy::U64 => "source.read_u64(u64::MAX)?".into(),
                ResolvedTy::U8 | ResolvedTy::U16 | ResolvedTy::U32 => format!("{ty}::try_from(source.read_u64(u64::from({ty}::MAX))?).expect(\"checked integer width\")"),
                ResolvedTy::I32 => "i32::try_from(source.read_i64(i64::from(i32::MIN), i64::from(i32::MAX))?).expect(\"checked integer width\")".into(),
                _ => return Err(format!("protocol scalar is not supported: {ty}")),
            },
            DataKind::Record { .. } => format!("{ty}::read(source)?"),
            DataKind::Vector(child) => format!("{{ let len = source.seq_begin()?; let mut values = Vec::with_capacity(len); while source.seq_next()? {{ values.push({}); }} values }}", self.decode(child)?),
            DataKind::Option { value, .. } => format!("if source.is_null()? {{ None }} else {{ Some({}) }}", self.decode(value)?),
            _ => return Err(format!("protocol data shape is not supported: {ty}")),
        })
    }

    fn cddl_type(&self, ty: &ResolvedTy) -> crate::Result<String> {
        Ok(match self.kind(ty)? {
            DataKind::Scalar => match ty {
                ResolvedTy::String => "tstr".into(),
                ResolvedTy::Bytes => "bstr".into(),
                ResolvedTy::U8 => "0..255".into(),
                ResolvedTy::U16 => "0..65535".into(),
                ResolvedTy::U32 => "0..4294967295".into(),
                ResolvedTy::U64 => "uint".into(),
                ResolvedTy::I32 => "-2147483648..2147483647".into(),
                _ => return Err(format!("protocol scalar is not supported: {ty}")),
            },
            DataKind::Record { .. } => cddl_name(&ty.to_string()),
            DataKind::Vector(child) => format!("[* {}]", self.cddl_type(child)?),
            DataKind::Option { value, .. } => format!("({}) / nil", self.cddl_type(value)?),
            _ => return Err(format!("protocol data shape is not supported: {ty}")),
        })
    }

    fn generate(&self) -> crate::Result<(String, String)> {
        let mut rust = String::from("// Generated from hew-runtime/protocol/node_controls.hew; run make node-protocol.\n// Checked data plans own structure; runtime adapters own semantic validation.\nuse hew_codec::{DecodeError, Format, Member, Sink, Source, Table};\n");
        let mut cddl = String::from("; Generated from hew-runtime/protocol/node_controls.hew; run make node-protocol.\n; Structural types only. Runtime owners enforce identity and resource policy.\n");
        for (ty, plan) in self.plans {
            let DataKind::Record {
                table: Some(table),
                fields,
                ..
            } = &plan.kind
            else {
                continue;
            };
            let name = self.rust_type(ty)?;
            let (_, names) = self.service.record_fields(ty)?;
            if names.len() != fields.len() || table.members.len() != fields.len() || !table.tagged {
                return Err(format!(
                    "protocol record has inconsistent checked fields: {ty}"
                ));
            }
            writeln!(
                rust,
                "#[derive(Debug, Clone, PartialEq, Eq)] pub(crate) struct {name} {{"
            )
            .unwrap();
            for ((field, _), child) in names.iter().zip(fields) {
                writeln!(rust, "pub(crate) {field}: {},", self.rust_type(child)?).unwrap();
            }
            writeln!(rust, "}}\nimpl {name} {{").unwrap();
            if ROOTS.contains(&name.as_str()) {
                writeln!(rust, "pub(crate) fn encode(&self, format: Format) -> Vec<u8> {{ let mut sink = Sink::new(format); self.write(&mut sink); sink.finish() }}").unwrap();
                writeln!(rust, "pub(crate) fn decode(format: Format, bytes: &[u8]) -> Result<Self, DecodeError> {{ let mut source = Source::new(format, bytes)?.reject_unknown_fields(); Self::read(&mut source) }}").unwrap();
            }
            let members = table
                .members
                .iter()
                .map(|member| {
                    format!(
                        "Member::new({:?}, {}, {})",
                        member.key, member.tag, member.flags
                    )
                })
                .collect::<Vec<_>>()
                .join(",");
            writeln!(rust, "const MEMBERS: &'static [Member<'static>] = &[{members}]; fn write(&self, sink: &mut Sink<'_>) {{ sink.record_begin(Table::new(Self::MEMBERS, true));").unwrap();
            for (index, ((field, _), child)) in names.iter().zip(fields).enumerate() {
                writeln!(
                    rust,
                    "sink.field({index}); let value = &self.{field}; {}",
                    self.encode(child, "value")?
                )
                .unwrap();
            }
            writeln!(rust, "sink.record_end(); }}").unwrap();
            writeln!(rust, "fn read(source: &mut Source<'_>) -> Result<Self, DecodeError> {{ source.record_begin(Table::new(Self::MEMBERS, true))?;").unwrap();
            for (field, _) in &names {
                writeln!(rust, "let mut {field} = None;").unwrap();
            }
            writeln!(
                rust,
                "while let Some(index) = source.record_next()? {{ match index {{"
            )
            .unwrap();
            for (index, ((field, _), child)) in names.iter().zip(fields).enumerate() {
                writeln!(
                    rust,
                    "{index} => {{ {field} = Some({}); }},",
                    self.decode(child)?
                )
                .unwrap();
            }
            writeln!(
                rust,
                "_ => unreachable!(\"checked record index\"), }} }} Ok(Self {{"
            )
            .unwrap();
            for (field, _) in &names {
                writeln!(rust, "{field}: {field}.expect(\"checked required field\"),").unwrap();
            }
            writeln!(rust, "}}) }} }}").unwrap();
            writeln!(cddl, "{} = {{", cddl_name(&name)).unwrap();
            for (member, child) in table.members.iter().zip(fields) {
                if member.flags != 0 {
                    return Err(format!(
                        "protocol field flags unsupported: {ty}.{}",
                        member.key
                    ));
                }
                writeln!(
                    cddl,
                    "    {} => {}, ; {}",
                    member.tag,
                    self.cddl_type(child)?,
                    member.key
                )
                .unwrap();
            }
            writeln!(cddl, "}}\n").unwrap();
        }
        Ok((rust, cddl))
    }
}
