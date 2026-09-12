//! Generate runtime method contracts from parsed stdlib declarations.

use std::collections::BTreeMap;
use std::fmt::Write as _;
use std::fs;
use std::path::Path;

use hew_parser::ast::{AttributeArg, FnDecl, Item, TypeExpr};

struct GeneratedMethod {
    rust: String,
    direct: bool,
    symbol: String,
    classification: String,
    ownership: String,
}

pub fn generate(repo: &Path, out: &Path) -> String {
    let mut rows = BTreeMap::new();
    for (module, path) in [
        ("std.builtins", "std/builtins.hew"),
        ("std.net", "std/net/net.hew"),
        ("std.net.tls", "std/net/tls/tls.hew"),
        ("std.net.websocket", "std/net/websocket/websocket.hew"),
    ] {
        let path = repo.join(path);
        println!("cargo:rerun-if-changed={}", path.display());
        let source = fs::read_to_string(&path).expect("read runtime declaration source");
        let parsed = hew_parser::parse(&source);
        assert!(
            parsed.errors.is_empty(),
            "{}: {:?}",
            path.display(),
            parsed.errors
        );
        for (item, _) in parsed.program.items {
            let Item::Impl(decl) = item else { continue };
            let TypeExpr::Named { name, .. } = decl.target_type.0 else {
                continue;
            };
            for method in decl.methods {
                if let Some((family, row)) = render_method(module, &name, &method) {
                    assert!(
                        rows.insert(family, row).is_none(),
                        "duplicate runtime family"
                    );
                }
            }
        }
    }
    let mut generated = String::from("// Generated from #[runtime] stdlib declarations.\nuse super::{RuntimeArgumentContract as A, RuntimeArgumentEffect as E, RuntimeValueKind as K, RuntimeResultEffect as R, RuntimeSemanticContract, RuntimeStaging, RuntimeCallAbiShape, RuntimePhysicalForm, RuntimeCReturn, IoHandleKind};\n");
    for (family, row) in &rows {
        writeln!(
            generated,
            "pub(super) const {family}: {} = {};",
            if row.direct {
                "DeclaredDirectRuntimeMethod"
            } else {
                "DeclaredRuntimeMethod"
            },
            row.rust
        )
        .unwrap();
    }
    generated.push_str("pub static DECLARED_RUNTIME_METHODS: &[DeclaredRuntimeMethod] = &[\n");
    for (family, row) in &rows {
        if !row.direct {
            writeln!(generated, "{family},").unwrap();
        }
    }
    generated.push_str("];\n");
    generated.push_str(
        "pub static DECLARED_DIRECT_RUNTIME_METHODS: &[DeclaredDirectRuntimeMethod] = &[\n",
    );
    for (family, row) in &rows {
        if row.direct {
            writeln!(generated, "{family},").unwrap();
        }
    }
    generated.push_str("];\n");
    fs::write(out.join("declared_runtime_methods.rs"), generated)
        .expect("write generated runtime declarations");
    let mut exports =
        String::from("# Generated from #[runtime] stdlib declarations; do not edit.\n");
    let mut tiers: BTreeMap<&str, Vec<&str>> = BTreeMap::new();
    for row in rows.values() {
        tiers
            .entry(&row.classification)
            .or_default()
            .push(&row.symbol);
    }
    for (tier, mut symbols) in tiers {
        symbols.sort_unstable();
        writeln!(exports, "{tier} = [").unwrap();
        for symbol in symbols {
            writeln!(exports, "  {symbol:?},").unwrap();
        }
        exports.push_str("]\n\n");
    }
    for row in rows.values() {
        exports.push_str(&row.ownership);
    }
    let exports = format!("{}\n", exports.trim_end());
    fs::write(out.join("declared_runtime_exports.toml"), &exports)
        .expect("write generated runtime export contracts");
    exports
}

fn render_method(
    module: &str,
    receiver: &str,
    method: &FnDecl,
) -> Option<(String, GeneratedMethod)> {
    let mut attrs = method
        .attributes
        .iter()
        .filter(|attr| attr.name == "runtime");
    let attr = attrs.next()?;
    assert!(attrs.next().is_none(), "duplicate runtime contract");
    let mut fields = BTreeMap::new();
    for arg in &attr.args {
        let AttributeArg::KeyValue { key, value } = arg else {
            panic!("runtime contract requires named fields");
        };
        assert!(
            fields.insert(key.as_str(), value.as_str()).is_none(),
            "duplicate runtime field {key}"
        );
    }
    let family = take(&mut fields, "family");
    assert!(
        family.chars().all(|c| c.is_ascii_alphanumeric()),
        "invalid runtime family"
    );
    let lowering = take(&mut fields, "lowering");
    if lowering == "direct" {
        return Some(render_direct(module, receiver, method, family, fields));
    }
    assert_eq!(lowering, "actor_ingress");
    let symbol = take(&mut fields, "symbol");
    let classification = take(&mut fields, "classification");
    assert!(
        matches!(classification, "non-declarable" | "non-declarable-stdlib"),
        "actor callbacks are compiler-only runtime exports"
    );
    assert_eq!(take(&mut fields, "target"), "native");
    let data = take(&mut fields, "data");
    let close = take(&mut fields, "close");
    let receiver_kind = match take(&mut fields, "receiver") {
        "connection" => "K::IoHandle(IoHandleKind::Connection)".to_string(),
        "opaque" => format!("K::NamedOpaque({:?})", format!("{module}.{receiver}")),
        other => panic!("unknown runtime receiver {other}"),
    };
    let result = match take(&mut fields, "result") {
        "discard_status" => "DeclaredRuntimeResult::DiscardStatus".to_string(),
        "status_result" => {
            let error_type = take(&mut fields, "error_type");
            let error_variant = take(&mut fields, "error_variant");
            format!("DeclaredRuntimeResult::StatusResult {{ error_type: {error_type:?}, error_variant: {error_variant:?} }}")
        }
        other => panic!("unknown runtime result {other}"),
    };
    assert!(fields.is_empty(), "unknown runtime fields: {fields:?}");
    let consumes = method.consumes_self || method.params.first().is_some_and(|p| p.is_consume);
    let effect = if consumes { "Move" } else { "Borrow" };
    let declaration = format!("{module}.{receiver}::{}", method.name);
    let row = format!("DeclaredRuntimeMethod {{ module: {module:?}, declaration: {declaration:?}, family: RuntimeCallFamily::{family}, data_handler: {data:?}, close_handler: {close:?}, consumes_receiver: {consumes}, result: {result}, row: RuntimeOpRow {{ symbol: {symbol:?}, contract: Some(RuntimeSemanticContract {{ arguments: &[A {{ ty: {receiver_kind}, effect: E::{effect} }}, A {{ ty: K::ActorHandle, effect: E::Borrow }}, A {{ ty: K::ConstBytePointer, effect: E::Copy }}, A {{ ty: K::ConstBytePointer, effect: E::Copy }}], result: R::BitCopy(K::I32), failures: &[] }}), staging: RuntimeStaging::Declared, abi_shape: RuntimeCallAbiShape::Other, physical: RuntimePhysicalForm::Direct, c_return: RuntimeCReturn::Storage }} }}");
    let ownership_effect = if consumes { "consume" } else { "borrow" };
    let nominal = format!("{module}.{receiver}");
    let ownership = format!("[[ownership.contracts]]\nsymbol = {symbol:?}\nresult = \"none\"\nparams = [{ownership_effect:?}, \"borrow\", \"borrow\", \"borrow\"]\nresource-param-types = [{nominal:?}, \"\", \"\", \"\"]\nrelease-symbol = \"\"\ndischarge-depth = \"none\"\n\n");
    Some((
        family.to_ascii_uppercase(),
        GeneratedMethod {
            rust: row,
            direct: false,
            symbol: symbol.to_string(),
            classification: classification.to_string(),
            ownership,
        },
    ))
}

fn take<'a>(fields: &mut BTreeMap<&str, &'a str>, key: &str) -> &'a str {
    fields
        .remove(key)
        .unwrap_or_else(|| panic!("missing runtime contract field {key}"))
}

fn scalar(ty: &TypeExpr) -> &'static str {
    let TypeExpr::Named { name, type_args } = ty else {
        panic!("direct runtime type must be a scalar");
    };
    assert!(
        type_args.is_none(),
        "direct runtime scalar cannot have type arguments"
    );
    match name.as_str() {
        "i64" => "I64",
        "duration" => "Duration",
        "instant" => "Instant",
        "bool" => "Bool",
        other => panic!("unsupported direct runtime scalar {other}"),
    }
}

fn render_direct(
    module: &str,
    receiver: &str,
    method: &FnDecl,
    family: &str,
    mut fields: BTreeMap<&str, &str>,
) -> (String, GeneratedMethod) {
    let classification = take(&mut fields, "classification");
    assert!(
        matches!(classification, "stable" | "stable-stdlib"),
        "invalid direct export classification"
    );
    let target = take(&mut fields, "target");
    assert_eq!(target, "native", "unsupported direct runtime target");
    let c_return = take(&mut fields, "c_return");
    assert!(fields.is_empty(), "unknown runtime fields: {fields:?}");
    let mut symbols = method
        .attributes
        .iter()
        .filter(|a| a.name == "extern_symbol");
    let attr = symbols
        .next()
        .expect("direct runtime declaration requires extern_symbol");
    assert!(
        symbols.next().is_none() && attr.args.len() == 1,
        "invalid direct extern_symbol"
    );
    let AttributeArg::Positional(symbol) = &attr.args[0] else {
        panic!("extern_symbol requires one symbol");
    };
    assert!(
        method.type_params.is_none() && !method.is_generator && !method.consumes_self,
        "direct scalar runtime declaration cannot be generic, consuming or a generator"
    );
    let params: Vec<_> = method
        .params
        .iter()
        .map(|p| {
            assert!(!p.is_consume, "direct scalar parameter cannot consume");
            scalar(&p.ty.0)
        })
        .collect();
    let result = scalar(
        &method
            .return_type
            .as_ref()
            .expect("direct runtime declaration requires result")
            .0,
    );
    let c_return = match (c_return, result) {
        ("storage", "Bool") => panic!("direct bool result requires explicit truth conversion"),
        ("storage", _) => "Storage",
        ("truth_i32", "Bool") => "TruthI32",
        _ => panic!("invalid direct runtime C return conversion"),
    };
    let receiver_param = method
        .params
        .first()
        .is_some_and(|p| matches!(&p.ty.0, TypeExpr::Named {name, ..} if name == receiver));
    let logical_params = &params[usize::from(receiver_param)..];
    let signature_key = format!("{receiver}::{}", method.name);
    let logical = logical_params
        .iter()
        .map(|p| format!("super::CanonicalExternTy::{p}"))
        .collect::<Vec<_>>()
        .join(",");
    let physical = params
        .iter()
        .map(|p| format!("super::CanonicalExternTy::{p}"))
        .collect::<Vec<_>>()
        .join(",");
    let arguments = params
        .iter()
        .map(|p| {
            format!(
                "A {{ ty: K::{}, effect: E::Copy }}",
                if *p == "Instant" { "I64" } else { p }
            )
        })
        .collect::<Vec<_>>()
        .join(",");
    let result_kind = if result == "Instant" { "I64" } else { result };
    let rust = format!("DeclaredDirectRuntimeMethod {{ signature: super::CanonicalStdlibExternSignature {{ module: {module:?}, signature_key: {signature_key:?}, symbol: {symbol:?}, family: Some(RuntimeCallFamily::{family}), params: &[{logical}], result: super::CanonicalExternTy::{result} }}, params: &[{physical}], target: DeclaredRuntimeTarget::Native, row: RuntimeOpRow {{ symbol: {symbol:?}, contract: Some(RuntimeSemanticContract {{ arguments: &[{arguments}], result: R::BitCopy(K::{result_kind}), failures: &[] }}), staging: RuntimeStaging::Declared, abi_shape: RuntimeCallAbiShape::Other, physical: RuntimePhysicalForm::Direct, c_return: RuntimeCReturn::{c_return} }} }}");
    (
        family.to_ascii_uppercase(),
        GeneratedMethod {
            rust,
            direct: true,
            symbol: symbol.clone(),
            classification: classification.to_owned(),
            ownership: String::new(),
        },
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn direct(source: &str) -> GeneratedMethod {
        let parsed = hew_parser::parse(source);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let Item::Impl(decl) = &parsed.program.items[0].0 else {
            panic!("impl");
        };
        render_method("std.builtins", "duration", &decl.methods[0])
            .unwrap()
            .1
    }

    #[test]
    #[should_panic(expected = "missing runtime contract field c_return")]
    fn direct_contract_requires_c_return_fact() {
        direct(
            r#"impl duration {
            #[runtime(family = "DurationIsZero", lowering = "direct", target = "native", classification = "stable")]
            #[extern_symbol(hew_duration_is_zero)]
            fn is_zero(d: duration) -> bool { false }
        }"#,
        );
    }

    #[test]
    #[should_panic(expected = "direct bool result requires explicit truth conversion")]
    fn direct_bool_cannot_guess_its_c_width() {
        direct(
            r#"impl duration {
            #[runtime(family = "DurationIsZero", lowering = "direct", target = "native", classification = "stable", c_return = "storage")]
            #[extern_symbol(hew_duration_is_zero)]
            fn is_zero(d: duration) -> bool { false }
        }"#,
        );
    }

    #[test]
    #[should_panic(expected = "direct runtime declaration requires extern_symbol")]
    fn direct_contract_requires_declaration_symbol() {
        direct(
            r#"impl duration {
            #[runtime(family = "DurationIsZero", lowering = "direct", target = "native", classification = "stable", c_return = "truth_i32")]
            fn is_zero(d: duration) -> bool { false }
        }"#,
        );
    }

    #[test]
    #[should_panic(expected = "unsupported direct runtime scalar string")]
    fn direct_scalar_rejects_managed_parameter() {
        direct(
            r#"impl duration {
            #[runtime(family = "DurationIsZero", lowering = "direct", target = "native", classification = "stable", c_return = "truth_i32")]
            #[extern_symbol(hew_duration_is_zero)]
            fn is_zero(d: string) -> bool { false }
        }"#,
        );
    }
}
