//! Generate runtime method contracts from parsed stdlib declarations.

use std::collections::BTreeMap;
use std::fmt::Write as _;
use std::fs;
use std::path::Path;

use hew_parser::ast::{AttributeArg, FnDecl, Item, TypeExpr};

struct GeneratedMethod {
    rust: String,
    symbol: String,
    classification: String,
    ownership: String,
}

pub fn generate(repo: &Path, out: &Path) -> String {
    let mut rows = BTreeMap::new();
    for (module, path) in [
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
            "pub(super) const {family}: DeclaredRuntimeMethod = {};",
            row.rust
        )
        .unwrap();
    }
    generated.push_str("pub static DECLARED_RUNTIME_METHODS: &[DeclaredRuntimeMethod] = &[\n");
    for family in rows.keys() {
        writeln!(generated, "{family},").unwrap();
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
    let symbol = take(&mut fields, "symbol");
    let classification = take(&mut fields, "classification");
    assert!(
        matches!(classification, "non-declarable" | "non-declarable-stdlib"),
        "actor callbacks are compiler-only runtime exports"
    );
    assert_eq!(take(&mut fields, "lowering"), "actor_ingress");
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
