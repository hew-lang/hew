//! Walk a parsed Hew AST and extract documentation items.

use hew_parser::ast::{
    Expr, Item, Literal, Program, TypeBodyItem, TypeDeclKind, UnaryOp, VariantKind, Visibility,
};

/// Documentation for a single module (source file).
#[derive(Debug, Clone)]
pub struct DocModule {
    /// Module name (derived from the file name).
    pub name: String,
    /// Module-level documentation from `//!` comments.
    pub doc: Option<String>,
    /// Documented `pub` functions.
    pub functions: Vec<DocFunction>,
    /// Documented `pub` types (structs, enums).
    pub types: Vec<DocType>,
    /// Documented `pub` actors.
    pub actors: Vec<DocActor>,
    /// Documented traits. Emitted regardless of visibility because the
    /// stdlib declares method-syntax traits without `pub` even when they
    /// describe methods on a `pub` opaque type (e.g. `ValueMethods` on
    /// `json::Value`).
    pub traits: Vec<DocTrait>,
    /// Documented `pub` constants.
    pub consts: Vec<DocConst>,
    /// Documented `pub` type aliases.
    pub type_aliases: Vec<DocTypeAlias>,
}

/// A documented function.
#[derive(Debug, Clone)]
pub struct DocFunction {
    pub name: String,
    pub signature: String,
    pub doc: Option<String>,
}

/// A documented struct field or actor field.
#[derive(Debug, Clone)]
pub struct DocField {
    pub name: String,
    pub ty: String,
    pub doc: Option<String>,
}

/// A documented enum variant.
#[derive(Debug, Clone)]
pub struct DocEnumVariant {
    pub name: String,
    /// Variant payload shape, suitable for appending after the name:
    /// empty for unit, `(T1, T2)` for tuple, ` { a: T, b: U }` for struct.
    pub shape: String,
    pub doc: Option<String>,
}

/// A documented type (struct or enum).
#[derive(Debug, Clone)]
pub struct DocType {
    pub name: String,
    /// `"struct"` or `"enum"`.
    pub kind: &'static str,
    /// Struct fields (empty for enums).
    pub fields: Vec<DocField>,
    /// Enum variants (empty for structs).
    pub variants: Vec<DocEnumVariant>,
    pub doc: Option<String>,
}

/// A documented method (trait method or actor receive handler).
#[derive(Debug, Clone)]
pub struct DocMethod {
    pub name: String,
    pub signature: String,
    pub doc: Option<String>,
}

/// A documented actor.
#[derive(Debug, Clone)]
pub struct DocActor {
    pub name: String,
    pub fields: Vec<DocField>,
    pub handlers: Vec<DocMethod>,
    pub doc: Option<String>,
}

/// A documented trait.
#[derive(Debug, Clone)]
pub struct DocTrait {
    pub name: String,
    pub methods: Vec<DocMethod>,
    pub doc: Option<String>,
}

/// A documented constant.
#[derive(Debug, Clone)]
pub struct DocConst {
    pub name: String,
    pub ty: String,
    /// Best-effort string form of the const value. Literals are rendered
    /// verbatim; non-literal expressions render as `…`.
    pub value: String,
    /// Rendered visibility prefix (`pub `, `pub(package) `, `pub(super) `),
    /// trailing space included so renderers can paste it before `const`.
    pub visibility: String,
    pub doc: Option<String>,
}

/// A documented type alias.
#[derive(Debug, Clone)]
pub struct DocTypeAlias {
    pub name: String,
    pub ty: String,
    pub visibility: String,
    pub doc: Option<String>,
}

/// Format a type expression back to a human-readable string.
fn format_type(ty: &hew_parser::ast::TypeExpr) -> String {
    use hew_parser::ast::TypeExpr;
    match ty {
        TypeExpr::Named { name, type_args } => {
            if let Some(args) = type_args {
                let arg_strs: Vec<String> = args.iter().map(|(t, _)| format_type(t)).collect();
                format!("{name}<{}>", arg_strs.join(", "))
            } else {
                name.clone()
            }
        }
        TypeExpr::Array { element, size } => {
            format!("[{}; {size}]", format_type(&element.0))
        }
        TypeExpr::Slice(inner) => {
            format!("[{}]", format_type(&inner.0))
        }
        TypeExpr::Tuple(elems) => {
            let strs: Vec<String> = elems.iter().map(|(t, _)| format_type(t)).collect();
            format!("({})", strs.join(", "))
        }
        TypeExpr::Function {
            params,
            return_type,
        } => {
            let param_strs: Vec<String> = params.iter().map(|(t, _)| format_type(t)).collect();
            format!(
                "fn({}) -> {}",
                param_strs.join(", "),
                format_type(&return_type.0)
            )
        }
        TypeExpr::Pointer {
            is_mutable,
            pointee,
        } => {
            let prefix = if *is_mutable { "&mut " } else { "&" };
            format!("{prefix}{}", format_type(&pointee.0))
        }
        TypeExpr::Option(inner) => format!("{}?", format_type(&inner.0)),
        TypeExpr::Result { ok, err } => {
            format!("Result<{}, {}>", format_type(&ok.0), format_type(&err.0))
        }
        TypeExpr::TraitObject(bounds) => {
            let parts: Vec<String> = bounds
                .iter()
                .map(|b| {
                    let args = b.type_args.as_ref().map_or(String::new(), |args| {
                        let strs: Vec<String> = args.iter().map(|(t, _)| format_type(t)).collect();
                        format!("<{}>", strs.join(", "))
                    });
                    format!("{}{args}", b.name)
                })
                .collect();
            if parts.len() == 1 {
                format!("dyn {}", parts[0])
            } else {
                format!("dyn ({})", parts.join(" + "))
            }
        }
        TypeExpr::Infer => "_".to_string(),
    }
}

/// Best-effort literal-expression rendering for const values. Non-literal
/// expressions render as `…` — full expression formatting is not needed for
/// the stdlib reference.
fn format_literal(expr: &Expr) -> String {
    match expr {
        Expr::Literal(lit) => format_lit(lit),
        // Handle common unary-negated literals so constants like
        // `pub const NEG_ONE: i32 = -1` render as `-1` rather than `…`.
        Expr::Unary { op, operand } => {
            let prefix = match op {
                UnaryOp::Negate => "-",
                UnaryOp::Not => "!",
                UnaryOp::BitNot => "~",
            };
            if let Expr::Literal(lit) = &operand.0 {
                format!("{prefix}{}", format_lit(lit))
            } else {
                "…".to_string()
            }
        }
        _ => "…".to_string(),
    }
}

fn format_lit(lit: &Literal) -> String {
    match lit {
        Literal::Integer { value, .. } => value.to_string(),
        Literal::Float(v) => v.to_string(),
        Literal::Bool(b) => b.to_string(),
        Literal::String(s) => format!("\"{}\"", escape_string_contents(s)),
        Literal::Char(c) => format!("'{}'", escape_char_contents(*c)),
        Literal::Duration(ns) => format!("{ns}ns"),
    }
}

/// Escape a string's contents for display inside `"..."`.
fn escape_string_contents(s: &str) -> String {
    use std::fmt::Write;
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\0' => out.push_str("\\0"),
            c if c.is_control() => {
                let _ = write!(out, "\\u{{{:x}}}", c as u32);
            }
            c => out.push(c),
        }
    }
    out
}

/// Escape a char's contents for display inside `'...'`.
fn escape_char_contents(c: char) -> String {
    match c {
        '\'' => "\\'".to_string(),
        '\\' => "\\\\".to_string(),
        '\n' => "\\n".to_string(),
        '\r' => "\\r".to_string(),
        '\t' => "\\t".to_string(),
        '\0' => "\\0".to_string(),
        c if c.is_control() => format!("\\u{{{:x}}}", c as u32),
        c => c.to_string(),
    }
}

/// Render a visibility prefix with a trailing space, or empty for private.
fn visibility_prefix(v: Visibility) -> &'static str {
    match v {
        Visibility::Private => "",
        Visibility::Pub => "pub ",
        Visibility::PubPackage => "pub(package) ",
        Visibility::PubSuper => "pub(super) ",
    }
}

/// Build a function signature string from a function declaration.
fn build_fn_signature(f: &hew_parser::ast::FnDecl) -> String {
    let mut sig = String::new();
    sig.push_str(visibility_prefix(f.visibility));
    if f.is_async {
        sig.push_str("async ");
    }
    if f.is_generator {
        sig.push_str("gen ");
    }
    sig.push_str("fn ");
    sig.push_str(&f.name);
    sig.push('(');
    let params: Vec<String> = f
        .params
        .iter()
        .map(|p| {
            let ty_str = format_type(&p.ty.0);
            if p.is_mutable {
                format!("var {}: {ty_str}", p.name)
            } else {
                format!("{}: {ty_str}", p.name)
            }
        })
        .collect();
    sig.push_str(&params.join(", "));
    sig.push(')');
    if let Some(ret) = &f.return_type {
        sig.push_str(" -> ");
        sig.push_str(&format_type(&ret.0));
    }
    sig
}

/// Build a receive handler signature string.
fn build_receive_signature(r: &hew_parser::ast::ReceiveFnDecl) -> String {
    let mut sig = String::from("receive ");
    if r.is_generator {
        sig.push_str("gen fn ");
    } else {
        sig.push_str("fn ");
    }
    sig.push_str(&r.name);
    sig.push('(');
    let params: Vec<String> = r
        .params
        .iter()
        .map(|p| {
            let ty_str = format_type(&p.ty.0);
            format!("{}: {ty_str}", p.name)
        })
        .collect();
    sig.push_str(&params.join(", "));
    sig.push(')');
    if let Some(ret) = &r.return_type {
        sig.push_str(" -> ");
        sig.push_str(&format_type(&ret.0));
    }
    sig
}

/// Build a trait method signature string.
fn build_trait_method_signature(m: &hew_parser::ast::TraitMethod) -> String {
    let mut sig = String::from("fn ");
    sig.push_str(&m.name);
    sig.push('(');
    let params: Vec<String> = m
        .params
        .iter()
        .map(|p| format!("{}: {}", p.name, format_type(&p.ty.0)))
        .collect();
    sig.push_str(&params.join(", "));
    sig.push(')');
    if let Some(ret) = &m.return_type {
        sig.push_str(" -> ");
        sig.push_str(&format_type(&ret.0));
    }
    sig
}

/// Render the payload shape of an enum variant as a string suffix.
fn format_variant_shape(kind: &VariantKind) -> String {
    match kind {
        VariantKind::Unit => String::new(),
        VariantKind::Tuple(types) => {
            let strs: Vec<String> = types.iter().map(|(t, _)| format_type(t)).collect();
            format!("({})", strs.join(", "))
        }
        VariantKind::Struct(fields) => {
            let strs: Vec<String> = fields
                .iter()
                .map(|(name, ty)| format!("{name}: {}", format_type(&ty.0)))
                .collect();
            format!(" {{ {} }}", strs.join(", "))
        }
    }
}

fn extract_struct_fields(body: &[TypeBodyItem]) -> Vec<DocField> {
    body.iter()
        .filter_map(|item| {
            if let TypeBodyItem::Field {
                name,
                ty,
                doc_comment,
                ..
            } = item
            {
                Some(DocField {
                    name: name.clone(),
                    ty: format_type(&ty.0),
                    doc: doc_comment.clone(),
                })
            } else {
                None
            }
        })
        .collect()
}

fn extract_enum_variants(body: &[TypeBodyItem]) -> Vec<DocEnumVariant> {
    body.iter()
        .filter_map(|item| {
            if let TypeBodyItem::Variant(v) = item {
                Some(DocEnumVariant {
                    name: v.name.clone(),
                    shape: format_variant_shape(&v.kind),
                    doc: v.doc_comment.clone(),
                })
            } else {
                None
            }
        })
        .collect()
}

/// Extract documentation items from a parsed Hew program.
///
/// Functions, types, actors, consts, and type aliases are filtered to the
/// `pub` surface so the generated site matches the public API. Traits are
/// emitted regardless of visibility — the stdlib uses method-syntax traits
/// without `pub` on `pub` opaque types (e.g. `ValueMethods` on
/// `json::Value`); filtering those would drop method docs users rely on.
#[must_use]
#[expect(
    clippy::too_many_lines,
    reason = "sequential match over each Item variant"
)]
pub fn extract_docs(program: &Program, module_name: &str) -> DocModule {
    let mut functions = Vec::new();
    let mut types = Vec::new();
    let mut actors = Vec::new();
    let mut traits = Vec::new();
    let mut consts = Vec::new();
    let mut type_aliases = Vec::new();

    for (item, _span) in &program.items {
        match item {
            Item::Function(f) if f.visibility.is_pub() => {
                functions.push(DocFunction {
                    name: f.name.clone(),
                    signature: build_fn_signature(f),
                    doc: f.doc_comment.clone(),
                });
            }
            Item::TypeDecl(t) if t.visibility.is_pub() => {
                let kind = match t.kind {
                    TypeDeclKind::Struct => "struct",
                    TypeDeclKind::Enum => "enum",
                };
                types.push(DocType {
                    name: t.name.clone(),
                    kind,
                    fields: extract_struct_fields(&t.body),
                    variants: extract_enum_variants(&t.body),
                    doc: t.doc_comment.clone(),
                });
            }
            Item::Actor(a) if a.visibility.is_pub() => {
                let fields: Vec<DocField> = a
                    .fields
                    .iter()
                    .map(|f| DocField {
                        name: f.name.clone(),
                        ty: format_type(&f.ty.0),
                        doc: f.doc_comment.clone(),
                    })
                    .collect();
                let handlers: Vec<DocMethod> = a
                    .receive_fns
                    .iter()
                    .map(|r| DocMethod {
                        name: r.name.clone(),
                        signature: build_receive_signature(r),
                        doc: r.doc_comment.clone(),
                    })
                    .collect();
                actors.push(DocActor {
                    name: a.name.clone(),
                    fields,
                    handlers,
                    doc: a.doc_comment.clone(),
                });
            }
            // Traits are emitted regardless of visibility: the stdlib pattern
            // declares method-syntax traits without `pub` even when they
            // describe methods on a `pub` opaque type (e.g. `ValueMethods`
            // on `json::Value`). A strict `pub`-only filter would hide those
            // methods entirely.
            Item::Trait(t) => {
                let methods: Vec<DocMethod> = t
                    .items
                    .iter()
                    .filter_map(|item| {
                        if let hew_parser::ast::TraitItem::Method(m) = item {
                            Some(DocMethod {
                                name: m.name.clone(),
                                signature: build_trait_method_signature(m),
                                doc: m.doc_comment.clone(),
                            })
                        } else {
                            None
                        }
                    })
                    .collect();
                traits.push(DocTrait {
                    name: t.name.clone(),
                    methods,
                    doc: t.doc_comment.clone(),
                });
            }
            Item::Const(c) if c.visibility.is_pub() => {
                consts.push(DocConst {
                    name: c.name.clone(),
                    ty: format_type(&c.ty.0),
                    value: format_literal(&c.value.0),
                    visibility: visibility_prefix(c.visibility).to_string(),
                    doc: c.doc_comment.clone(),
                });
            }
            Item::TypeAlias(ta) if ta.visibility.is_pub() => {
                type_aliases.push(DocTypeAlias {
                    name: ta.name.clone(),
                    ty: format_type(&ta.ty.0),
                    visibility: visibility_prefix(ta.visibility).to_string(),
                    doc: ta.doc_comment.clone(),
                });
            }
            // Private items and non-documentable item kinds fall through.
            Item::Function(_)
            | Item::TypeDecl(_)
            | Item::Actor(_)
            | Item::Const(_)
            | Item::TypeAlias(_)
            | Item::Import(_)
            | Item::Impl(_)
            | Item::Wire(_)
            | Item::ExternBlock(_)
            | Item::Supervisor(_)
            | Item::Machine(_) => {}
        }
    }

    DocModule {
        name: module_name.to_string(),
        doc: program.module_doc.clone(),
        functions,
        types,
        actors,
        traits,
        consts,
        type_aliases,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extract_function_docs() {
        let source = r"/// Adds two numbers.
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}
";
        let result = hew_parser::parse(source);
        assert!(result.errors.is_empty());
        let module = extract_docs(&result.program, "test");
        assert_eq!(module.functions.len(), 1);
        assert_eq!(module.functions[0].name, "add");
        assert_eq!(
            module.functions[0].doc.as_deref(),
            Some("Adds two numbers.")
        );
        assert!(module.functions[0]
            .signature
            .contains("fn add(a: i32, b: i32) -> i32"));
    }

    #[test]
    fn extract_module_doc() {
        let source = r"//! Module docs here.
//! Second line.

pub fn foo() {}
";
        let result = hew_parser::parse(source);
        assert!(result.errors.is_empty());
        let module = extract_docs(&result.program, "test");
        assert_eq!(
            module.doc.as_deref(),
            Some("Module docs here.\nSecond line.")
        );
    }

    #[test]
    fn extract_struct_docs_with_field_docs() {
        let source = r"/// A point in space.
pub type Point {
    /// x coordinate.
    x: i32;
    /// y coordinate.
    y: i32;
}
";
        let result = hew_parser::parse(source);
        assert!(result.errors.is_empty());
        let module = extract_docs(&result.program, "test");
        assert_eq!(module.types.len(), 1);
        assert_eq!(module.types[0].name, "Point");
        assert_eq!(module.types[0].kind, "struct");
        assert_eq!(module.types[0].doc.as_deref(), Some("A point in space."));
        assert_eq!(module.types[0].fields.len(), 2);
        assert_eq!(module.types[0].fields[0].name, "x");
        assert_eq!(
            module.types[0].fields[0].doc.as_deref(),
            Some("x coordinate.")
        );
    }

    #[test]
    fn extract_enum_variants_with_docs() {
        let source = r"pub enum Error {
    /// The thing that went wrong.
    Invalid(String);
    /// No payload.
    Empty;
}
";
        let result = hew_parser::parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let module = extract_docs(&result.program, "test");
        assert_eq!(module.types.len(), 1);
        let t = &module.types[0];
        assert_eq!(t.kind, "enum");
        assert_eq!(t.variants.len(), 2);
        assert_eq!(t.variants[0].name, "Invalid");
        assert_eq!(t.variants[0].shape, "(String)");
        assert_eq!(
            t.variants[0].doc.as_deref(),
            Some("The thing that went wrong.")
        );
        assert_eq!(t.variants[1].name, "Empty");
        assert_eq!(t.variants[1].shape, "");
    }

    #[test]
    fn extract_trait_method_docs() {
        let source = r"pub trait T {
    /// Do the thing.
    fn do_it(x: i32) -> i32;
}
";
        let result = hew_parser::parse(source);
        assert!(result.errors.is_empty());
        let module = extract_docs(&result.program, "test");
        assert_eq!(module.traits.len(), 1);
        assert_eq!(module.traits[0].methods.len(), 1);
        assert_eq!(module.traits[0].methods[0].name, "do_it");
        assert_eq!(
            module.traits[0].methods[0].doc.as_deref(),
            Some("Do the thing.")
        );
    }

    #[test]
    fn extract_actor_docs_with_member_docs() {
        let source = r"/// A simple counter actor.
pub actor Counter {
    /// How many so far.
    let count: i32;
    /// Bump the counter.
    receive fn increment() {
        self.count = self.count + 1;
    }
}
";
        let result = hew_parser::parse(source);
        assert!(result.errors.is_empty());
        let module = extract_docs(&result.program, "test");
        assert_eq!(module.actors.len(), 1);
        assert_eq!(module.actors[0].name, "Counter");
        assert_eq!(module.actors[0].fields.len(), 1);
        assert_eq!(
            module.actors[0].fields[0].doc.as_deref(),
            Some("How many so far.")
        );
        assert_eq!(module.actors[0].handlers.len(), 1);
        assert_eq!(
            module.actors[0].handlers[0].doc.as_deref(),
            Some("Bump the counter.")
        );
    }

    #[test]
    fn format_literal_escapes_embedded_quotes_and_controls() {
        assert_eq!(escape_string_contents("a\"b\\c"), "a\\\"b\\\\c");
        assert_eq!(escape_string_contents("line1\nline2"), "line1\\nline2");
        assert_eq!(escape_char_contents('\''), "\\'");
        assert_eq!(escape_char_contents('\n'), "\\n");
        assert_eq!(escape_char_contents('a'), "a");
    }

    #[test]
    fn extract_const_and_type_alias() {
        let source = r"/// The answer.
pub const ANSWER: i32 = 42;

/// A user id.
pub type UserId = i64;
";
        let result = hew_parser::parse(source);
        assert!(result.errors.is_empty());
        let module = extract_docs(&result.program, "test");
        assert_eq!(module.consts.len(), 1);
        assert_eq!(module.consts[0].name, "ANSWER");
        assert_eq!(module.consts[0].value, "42");
        assert_eq!(module.consts[0].doc.as_deref(), Some("The answer."));
        assert_eq!(module.type_aliases.len(), 1);
        assert_eq!(module.type_aliases[0].name, "UserId");
        assert_eq!(module.type_aliases[0].doc.as_deref(), Some("A user id."));
    }

    #[test]
    fn private_items_are_filtered() {
        let source = r"pub fn public_fn() {}
fn private_fn() {}
pub const PUBLIC_C: i32 = 1;
const PRIVATE_C: i32 = 2;
";
        let result = hew_parser::parse(source);
        assert!(result.errors.is_empty());
        let module = extract_docs(&result.program, "test");
        assert_eq!(module.functions.len(), 1);
        assert_eq!(module.functions[0].name, "public_fn");
        assert_eq!(module.consts.len(), 1);
        assert_eq!(module.consts[0].name, "PUBLIC_C");
    }

    #[test]
    fn pub_package_and_pub_super_are_emitted() {
        let source = r"pub(package) fn pp_fn() {}
pub(super) fn ps_fn() {}
pub(package) const PP_C: i32 = 1;
";
        let result = hew_parser::parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let module = extract_docs(&result.program, "test");
        assert_eq!(module.functions.len(), 2);
        let names: Vec<&str> = module.functions.iter().map(|f| f.name.as_str()).collect();
        assert!(names.contains(&"pp_fn"));
        assert!(names.contains(&"ps_fn"));
        // Signature carries the actual visibility, not a hardcoded `pub `.
        let pp_sig = &module
            .functions
            .iter()
            .find(|f| f.name == "pp_fn")
            .unwrap()
            .signature;
        assert!(pp_sig.starts_with("pub(package) "), "sig: {pp_sig}");
        assert_eq!(module.consts.len(), 1);
        assert_eq!(module.consts[0].visibility, "pub(package) ");
    }

    #[test]
    fn negative_literal_const_renders_with_sign() {
        let source = "pub const NEG_ONE: i32 = -1;\n";
        let result = hew_parser::parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let module = extract_docs(&result.program, "test");
        assert_eq!(module.consts.len(), 1);
        assert_eq!(module.consts[0].value, "-1");
    }

    #[test]
    fn receive_fn_signature_includes_fn_keyword() {
        let source = r"pub actor A {
    /// Handle the tick.
    receive fn tick() {}
}
";
        let result = hew_parser::parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let module = extract_docs(&result.program, "test");
        assert_eq!(module.actors.len(), 1);
        assert_eq!(module.actors[0].handlers.len(), 1);
        assert!(
            module.actors[0].handlers[0]
                .signature
                .starts_with("receive fn tick"),
            "signature was {:?}",
            module.actors[0].handlers[0].signature,
        );
    }

    #[test]
    fn no_doc_comment_is_none() {
        let source = "pub fn bare() {}";
        let result = hew_parser::parse(source);
        assert!(result.errors.is_empty());
        let module = extract_docs(&result.program, "test");
        assert_eq!(module.functions.len(), 1);
        assert!(module.functions[0].doc.is_none());
    }
}
