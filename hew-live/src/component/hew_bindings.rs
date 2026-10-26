//! Ordinary Hew service wrappers and their canonical-ABI carrier adapters.

use std::collections::HashMap;
use std::fmt::Write;

use wit_bindgen_core::wit_parser::{
    Function, InterfaceId, Resolve, Type, TypeDefKind, TypeId, TypeOwner, WorldId, WorldItem,
    WorldKey,
};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error + Send + Sync>>;

struct Bindings<'a> {
    resolve: &'a Resolve,
    world: &'a str,
    interface: InterfaceId,
    key: &'a WorldKey,
    names: HashMap<TypeId, String>,
}

struct Leaf {
    field: String,
    name: String,
    ty: Type,
}

pub(super) fn generate(resolve: &Resolve, world: WorldId) -> Result<(String, String)> {
    let world = &resolve.worlds[world];
    let (key, interface) = world
        .imports
        .iter()
        .find_map(|(key, item)| match item {
            WorldItem::Interface { id, .. }
                if resolve.interfaces[*id].name.as_deref() == Some("services") =>
            {
                Some((key, *id))
            }
            _ => None,
        })
        .ok_or("router world must import its services interface")?;
    let mut generator = Bindings {
        resolve,
        world: &world.name,
        interface,
        key,
        names: HashMap::new(),
    };
    let mut hew = String::from(
        "// Generated from the component's WIT service contract.\nimport std.stream;\n\n",
    );
    for (name, id) in &resolve.interfaces[interface].types {
        let id = generator.dealias(*id);
        if matches!(resolve.types[id].kind, TypeDefKind::Record(_)) {
            generator.names.insert(id, pascal(name));
        }
    }
    for id in resolve.interfaces[interface].types.values() {
        let id = generator.dealias(*id);
        if let TypeDefKind::Record(record) = &resolve.types[id].kind {
            writeln!(hew, "pub type {} {{", generator.names[&id])?;
            for field in &record.fields {
                writeln!(
                    hew,
                    "    {}: {};",
                    ident(&field.name),
                    generator.hew_type(field.ty)?
                )?;
            }
            hew.push_str("}\n\n");
        }
    }
    let mut c = String::new();
    for function in resolve.interfaces[interface].functions.values() {
        generator.function(function, &mut hew, &mut c)?;
    }
    Ok((hew, c))
}

pub(super) fn wire_records(resolve: &Resolve, world: WorldId) -> Result<String> {
    let (interface, events) = resolve
        .interfaces
        .iter()
        .find(|(_, interface)| interface.name.as_deref() == Some("events"))
        .ok_or("router contract must declare events")?;
    let key = WorldKey::Interface(interface);
    let mut generator = Bindings {
        resolve,
        world: &resolve.worlds[world].name,
        interface,
        key: &key,
        names: HashMap::new(),
    };
    for (name, id) in &events.types {
        generator.names.insert(generator.dealias(*id), pascal(name));
    }
    let mut source = String::from("// Generated from the component's WIT event records.\n");
    for id in events.types.values() {
        let id = generator.dealias(*id);
        let TypeDefKind::Record(record) = &resolve.types[id].kind else {
            return Err("node event contracts require WIT records".into());
        };
        writeln!(source, "\n#[wire]\npub type {} {{", generator.names[&id])?;
        for (index, field) in record.fields.iter().enumerate() {
            writeln!(
                source,
                "    {}: {} @{};",
                ident(&field.name),
                generator.hew_type(field.ty)?,
                index + 1
            )?;
        }
        source.push_str("}\n");
    }
    Ok(source)
}

fn ident(name: &str) -> String {
    wit_bindgen_c::to_c_ident(name)
}

fn pascal(name: &str) -> String {
    name.split('-')
        .map(|part| {
            let mut letters = part.chars();
            letters.next().map_or_else(String::new, |first| {
                first.to_uppercase().chain(letters).collect()
            })
        })
        .collect()
}

impl Bindings<'_> {
    fn dealias(&self, mut id: TypeId) -> TypeId {
        while let TypeDefKind::Type(Type::Id(next)) = self.resolve.types[id].kind {
            id = next;
        }
        id
    }

    fn scalar(&self, ty: Type) -> Result<(&'static str, &'static str)> {
        Ok(match ty {
            Type::String => ("string", "const HewText *"),
            Type::Bool => ("bool", "bool"),
            Type::U8 => ("u8", "uint8_t"),
            Type::S8 => ("i8", "int8_t"),
            Type::U16 => ("u16", "uint16_t"),
            Type::S16 => ("i16", "int16_t"),
            Type::U32 => ("u32", "uint32_t"),
            Type::S32 => ("i32", "int32_t"),
            Type::U64 => ("u64", "uint64_t"),
            Type::S64 => ("i64", "int64_t"),
            Type::F32 => ("f32", "float"),
            Type::F64 => ("f64", "double"),
            Type::Id(id) => match self.resolve.types[self.dealias(id)].kind {
                TypeDefKind::Type(inner) => return self.scalar(inner),
                _ => return Err("service result payload requires a scalar WIT type".into()),
            },
            _ => return Err("this WIT scalar has no Hew service binding".into()),
        })
    }

    fn scalar_type(&self, ty: Type) -> Result<Type> {
        if let Type::Id(id) = ty {
            if let TypeDefKind::Type(inner) = self.resolve.types[self.dealias(id)].kind {
                return self.scalar_type(inner);
            }
        }
        self.scalar(ty)?;
        Ok(ty)
    }

    fn hew_type(&self, ty: Type) -> Result<String> {
        if let Type::Id(id) = ty {
            if let Some(name) = self.names.get(&self.dealias(id)) {
                return Ok(name.clone());
            }
        }
        Ok(self.scalar(ty)?.0.into())
    }

    fn c_type(&self, ty: Type) -> Result<String> {
        let Type::Id(id) = ty else {
            return Ok(if ty == Type::String {
                format!("{}_string_t", self.world)
            } else {
                self.scalar(ty)?.1.into()
            });
        };
        let (info, encoded) = wit_bindgen_c::gen_type_name(self.resolve, id);
        let namespace = if matches!(
            info,
            wit_bindgen_c::CTypeNameInfo::Anonymous { is_prim: true }
        ) {
            self.world.into()
        } else {
            let owner = match self.resolve.types[id].owner {
                TypeOwner::Interface(owner) => Some(owner),
                TypeOwner::World(_) => None,
                TypeOwner::None => Some(self.interface),
            };
            let key = owner.map(WorldKey::Interface);
            let interface = owner.zip(key.as_ref());
            wit_bindgen_c::owner_namespace(
                interface,
                true,
                self.world.into(),
                self.resolve,
                id,
                &HashMap::new(),
            )
        };
        Ok(format!("{namespace}_{encoded}_t"))
    }

    fn leaves(&self, ty: Type, field: String, name: String, leaves: &mut Vec<Leaf>) -> Result<()> {
        if let Type::Id(id) = ty {
            if let TypeDefKind::Record(record) = &self.resolve.types[self.dealias(id)].kind {
                for child in &record.fields {
                    self.leaves(
                        child.ty,
                        format!("{field}.{}", ident(&child.name)),
                        format!("{name}_{}", ident(&child.name)),
                        leaves,
                    )?;
                }
                return Ok(());
            }
        }
        let ty = self.scalar_type(ty)?;
        leaves.push(Leaf { field, name, ty });
        Ok(())
    }

    #[allow(
        clippy::too_many_lines,
        reason = "one WIT function emits its paired typed Hew and C carrier contract"
    )]
    fn function(&self, function: &Function, hew: &mut String, c: &mut String) -> Result<()> {
        if !function.kind.is_async() {
            return Err("service imports must be asynchronous".into());
        }
        let Some(Type::Id(result)) = function.result else {
            return Err("service imports must return Result".into());
        };
        let TypeDefKind::Result(result_shape) = &self.resolve.types[self.dealias(result)].kind
        else {
            return Err("service imports must return Result".into());
        };
        if result_shape.err != Some(Type::String) {
            return Err("service errors must be WIT strings".into());
        }
        let (optional, value) = match result_shape.ok {
            Some(Type::Id(id)) => match self.resolve.types[self.dealias(id)].kind {
                TypeDefKind::Option(value) => (true, Some(value)),
                _ => (false, result_shape.ok),
            },
            value => (false, value),
        };
        let value = value.map(|value| self.scalar_type(value)).transpose()?;
        let name = ident(&function.name);
        let bridge = format!("hew_component_import_{name}");
        let pending = format!("Pending_{name}");
        let c_function = wit_bindgen_c::c_func_name(
            true,
            self.resolve,
            self.world,
            Some(self.key),
            function,
            &HashMap::new(),
        );
        let result_c = self.c_type(Type::Id(result))?;
        let result_free = format!("{}_free", result_c.trim_end_matches("_t"));
        let mut leaves = Vec::new();
        for param in &function.params {
            self.leaves(
                param.ty,
                ident(&param.name),
                ident(&param.name),
                &mut leaves,
            )?;
        }
        let mut extern_params = leaves
            .iter()
            .map(|leaf| Ok(format!("{}: {}", leaf.name, self.scalar(leaf.ty)?.0)))
            .collect::<Result<Vec<_>>>()?;
        extern_params.push("consume status: Sink<i64>".into());
        if let Some(value) = value {
            extern_params.push(format!("consume ok: Sink<{}>", self.scalar(value)?.0));
        }
        extern_params.push("consume error: Sink<string>".into());
        writeln!(
            hew,
            "extern \"C\" {{\n    fn {bridge}({});\n}}\n",
            extern_params.join(", ")
        )?;
        let params = function
            .params
            .iter()
            .map(|param| {
                Ok(format!(
                    "{}: {}",
                    ident(&param.name),
                    self.hew_type(param.ty)?
                ))
            })
            .collect::<Result<Vec<_>>>()?;
        let value_type = value
            .map(|value| self.scalar(value).map(|pair| pair.0.to_owned()))
            .transpose()?
            .unwrap_or_else(|| "()".into());
        let output = if optional {
            format!("Option<{value_type}>")
        } else {
            value_type.clone()
        };
        writeln!(
            hew,
            "pub fn {name}({}) -> Result<{output}, string> {{",
            params.join(", ")
        )?;
        writeln!(hew, "    let (status, status_input): (Sink<i64>, Stream<i64>) = stream.pipe(1).expect(\"component status pipe\");")?;
        if value.is_some() {
            writeln!(hew, "    let (ok, ok_input): (Sink<{value_type}>, Stream<{value_type}>) = stream.pipe(1).expect(\"component value pipe\");")?;
        }
        writeln!(hew, "    let (error, error_input): (Sink<string>, Stream<string>) = stream.pipe(1).expect(\"component error pipe\");")?;
        let mut args = leaves
            .iter()
            .map(|leaf| leaf.field.clone())
            .collect::<Vec<_>>();
        args.push("status".into());
        if value.is_some() {
            args.push("ok".into());
        }
        args.push("error".into());
        writeln!(hew, "    unsafe {{ {bridge}({}); }};", args.join(", "))?;
        writeln!(
            hew,
            "    match status_input.recv().expect(\"component reply\") {{"
        )?;
        let received = if value.is_some() {
            "ok_input.recv().expect(\"component value\")"
        } else {
            "()"
        };
        if optional {
            writeln!(
                hew,
                "        0 => .Ok(.None),\n        1 => .Ok(.Some({received})),"
            )?;
        } else {
            writeln!(hew, "        0 => .Ok({received}),")?;
        }
        hew.push_str(
            "        _ => .Err(error_input.recv().expect(\"component error\")),\n    }\n}\n\n",
        );

        writeln!(c, "typedef struct {pending} {{\n    Pending base;")?;
        for param in &function.params {
            writeln!(c, "    {} {};", self.c_type(param.ty)?, ident(&param.name))?;
        }
        writeln!(c, "    {result_c} result;\n}} {pending};")?;
        writeln!(c, "static void settle_{name}(Pending *base, bool cancelled) {{\n    {pending} *pending = ({pending} *)base;\n    if (!cancelled) {{\n        int64_t status = 0;\n        if (pending->result.is_err) {{\n            status = -1;")?;
        write_value(c, Type::String, "base->error", "pending->result.val.err")?;
        c.push_str("        } else {\n");
        if let Some(value) = value {
            if optional {
                c.push_str("            if (pending->result.val.ok.is_some) {\n                status = 1;\n");
                write_value(c, value, "base->ok", "pending->result.val.ok.val")?;
                c.push_str("            }\n");
            } else {
                write_value(c, value, "base->ok", "pending->result.val.ok")?;
            }
        }
        writeln!(c, "        }}\n        hew_sink_write(base->status, &status, sizeof(status));\n        {result_free}(&pending->result);\n    }}")?;
        for leaf in &leaves {
            if leaf.ty == Type::String {
                writeln!(c, "    router_string_free(&pending->{});", leaf.field)?;
            }
        }
        c.push_str("}\n");
        let mut c_params = leaves
            .iter()
            .map(|leaf| Ok(format!("{} {}", self.scalar(leaf.ty)?.1, leaf.name)))
            .collect::<Result<Vec<_>>>()?;
        c_params.push("void *status".into());
        if value.is_some() {
            c_params.push("void *ok".into());
        }
        c_params.push("void *error".into());
        writeln!(c, "void {bridge}({}) {{\n    Task *task = router_context_get_0();\n    assert(task != NULL);\n    {pending} *pending = calloc(1, sizeof(*pending));\n    assert(pending != NULL);\n    pending->base.status = status;\n    pending->base.error = error;\n    pending->base.settle = settle_{name};", c_params.join(", "))?;
        if value.is_some() {
            c.push_str("    pending->base.ok = ok;\n");
        }
        for leaf in &leaves {
            let input = if leaf.ty == Type::String {
                format!("copy_text({})", leaf.name)
            } else {
                leaf.name.clone()
            };
            writeln!(c, "    pending->{} = {input};", leaf.field)?;
        }
        let mut args = function
            .params
            .iter()
            .map(|param| format!("pending->{}", ident(&param.name)))
            .collect::<Vec<_>>();
        args.push("&pending->result".into());
        writeln!(
            c,
            "    pending->base.next = task->pending;\n    task->pending = &pending->base;\n    await_pending(task, &pending->base, {c_function}({}));\n}}\n",
            args.join(", ")
        )?;
        Ok(())
    }
}

fn write_value(c: &mut String, ty: Type, sink: &str, value: &str) -> Result<()> {
    if ty == Type::String {
        writeln!(c, "            {{ HewText *text = hew_text({value}); hew_sink_write_string({sink}, text); hew_host_text_release(text); }}")?;
    } else {
        writeln!(
            c,
            "            hew_sink_write({sink}, &{value}, sizeof({value}));"
        )?;
    }
    Ok(())
}
