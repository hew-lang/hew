//! Export primitive syntax categories from the checker's type identities.

use hew_types::builtin_type::BuiltinType;
use hew_types::ty::PRIMITIVE_ALIASES;
use hew_types::Ty;
use serde_json::Value;

pub fn run(args: &[String]) -> crate::Result<()> {
    let [mode] = args else {
        return Err("syntax-data requires --write or --check".to_string());
    };
    if mode != "--write" && mode != "--check" {
        return Err("syntax-data requires --write or --check".to_string());
    }
    let path = crate::workspace_root()?.join("docs/syntax-data.json");
    let current = crate::read_json(&path)?;
    let mut generated = current.clone();
    let mut integers = Vec::new();
    let mut floats = Vec::new();
    let mut primitives = Vec::new();
    for (name, _) in PRIMITIVE_ALIASES {
        let ty = Ty::from_name(name).ok_or_else(|| format!("unknown primitive {name}"))?;
        if matches!(ty, Ty::Unit | Ty::Never) {
            continue;
        }
        let category = if ty.is_integer() {
            &mut integers
        } else if ty.is_float() {
            &mut floats
        } else {
            &mut primitives
        };
        category.push(Value::from(*name));
    }
    // These source primitives have named checker identities, rather than
    // scalar Ty variants. Their spelling remains owned by BuiltinType.
    primitives.extend(
        [BuiltinType::Instant, BuiltinType::Timestamp].map(|ty| Value::from(ty.canonical_name())),
    );
    generated["types"]["integer"] = integers.into();
    generated["types"]["float"] = floats.into();
    generated["types"]["primitive"] = primitives.into();
    if mode == "--write" {
        crate::write_json_if_changed(&path, &generated)?;
    } else if current != generated {
        return Err("primitive syntax metadata is stale; run make syntax-data".to_string());
    }
    Ok(())
}
