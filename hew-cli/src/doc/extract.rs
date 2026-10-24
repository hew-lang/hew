//! Walk a parsed Hew AST and extract documentation items.

use std::collections::HashMap;

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
    /// Documented `pub` state machines.
    pub machines: Vec<DocMachine>,
    /// Documented traits. Emitted regardless of visibility because the
    /// stdlib declares method-syntax traits without `pub` even when they
    /// describe methods on a `pub` opaque type (e.g. `ValueMethods` on
    /// `json::Value`).
    pub traits: Vec<DocTrait>,
    /// Documented inherent implementation blocks.
    pub impls: Vec<DocImpl>,
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

/// A named state, input event, or output variant in a machine declaration.
#[derive(Debug, Clone)]
pub struct DocMachineMember {
    pub name: String,
    pub shape: String,
}

/// A documented state machine declaration.
#[derive(Debug, Clone)]
pub struct DocMachine {
    pub name: String,
    pub signature: String,
    pub states: Vec<DocMachineMember>,
    pub events: Vec<DocMachineMember>,
    pub outputs: Vec<DocMachineMember>,
}

/// A documented trait.
#[derive(Debug, Clone)]
pub struct DocTrait {
    pub name: String,
    pub methods: Vec<DocMethod>,
    pub doc: Option<String>,
    /// Whether the trait itself is part of the public API. Private method
    /// traits are retained so their methods can be attached to public types.
    pub is_public: bool,
    /// Types that implement this trait in the same module.
    pub implementation_targets: Vec<String>,
}

/// A documented inherent implementation block.
#[derive(Debug, Clone)]
pub struct DocImpl {
    pub target: String,
    pub signature: String,
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
    /// Rendered visibility prefix (`pub `, `package `, or `""`),
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
        TypeExpr::QualifiedAssocPath(path) => format!(
            "<{} as {}>.{}",
            format_type(&path.base.0),
            path.trait_path,
            path.members
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(".")
        ),
        TypeExpr::Named {
            path: named_path,
            type_args,
        } => {
            let name = &named_path.to_string(); // TRANSITION(P1): deleted by A1 commit 2
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
            ..
        } => {
            let param_strs: Vec<String> = params.iter().map(format_callable_parameter).collect();
            format!(
                "fn({}) -> {}",
                param_strs.join(", "),
                format_type(&return_type.0)
            )
        }
        TypeExpr::ActorFn {
            params,
            return_type,
        } => {
            let param_strs: Vec<String> = params.iter().map(|(t, _)| format_type(t)).collect();
            format!(
                "actor({}) -> {}",
                param_strs.join(", "),
                format_type(&return_type.0)
            )
        }
        TypeExpr::Pointer {
            is_mutable,
            pointee,
        } => {
            let prefix = if *is_mutable { "*mut " } else { "*const " };
            format!("{prefix}{}", format_type(&pointee.0))
        }
        TypeExpr::Option(inner) => format!("{}?", format_type(&inner.0)),
        TypeExpr::Fallible { success, error } => format!(
            "{} fails {}",
            format_type(&success.0),
            format_type(&error.0)
        ),
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
                    format!("{}{args}", b.path) // TRANSITION(P1): deleted by A1 commit 2
                })
                .collect();
            if parts.len() == 1 {
                format!("dyn {}", parts[0])
            } else {
                format!("dyn ({})", parts.join(" + "))
            }
        }
        TypeExpr::Infer => "_".to_string(),
        TypeExpr::Borrow(inner) => format!("&{}", format_type(&inner.0)),
    }
}

fn format_callable_parameter(
    parameter: &hew_parser::ast::CallableParameter<
        hew_parser::ast::Spanned<hew_parser::ast::TypeExpr>,
    >,
) -> String {
    let prefix = if parameter.ownership == hew_parser::ast::ParameterOwnership::Consume {
        "consume "
    } else {
        ""
    };
    format!("{prefix}{}", format_type(&parameter.ty.0))
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
                UnaryOp::RawDeref => "*",
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
        Visibility::Package => "package ",
    }
}

fn format_trait_bound(bound: &hew_parser::ast::TraitBound) -> String {
    let mut out = bound.path.to_string();
    let mut args = bound
        .type_args
        .as_ref()
        .map(|args| {
            args.iter()
                .map(|(ty, _)| format_type(ty))
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();
    args.extend(
        bound
            .assoc_type_bindings
            .iter()
            .map(|binding| format!("{} = {}", binding.name, format_type(&binding.ty.0))),
    );
    if !args.is_empty() {
        out.push('<');
        out.push_str(&args.join(", "));
        out.push('>');
    }
    out
}

fn format_type_params(type_params: Option<&[hew_parser::ast::TypeParam]>) -> String {
    let Some(type_params) = type_params else {
        return String::new();
    };
    if type_params.is_empty() {
        return String::new();
    }
    let params = type_params
        .iter()
        .map(|param| {
            if param.bounds.is_empty() {
                param.name.to_string()
            } else {
                format!(
                    "{}: {}",
                    param.name,
                    param
                        .bounds
                        .iter()
                        .map(format_trait_bound)
                        .collect::<Vec<_>>()
                        .join(" + ")
                )
            }
        })
        .collect::<Vec<_>>();
    format!("<{}>", params.join(", "))
}

fn format_where_clause(where_clause: Option<&hew_parser::ast::WhereClause>) -> String {
    let Some(where_clause) = where_clause else {
        return String::new();
    };
    if where_clause.predicates.is_empty() {
        return String::new();
    }
    let predicates = where_clause
        .predicates
        .iter()
        .map(|predicate| {
            format!(
                "{}: {}",
                format_type(&predicate.ty.0),
                predicate
                    .bounds
                    .iter()
                    .map(format_trait_bound)
                    .collect::<Vec<_>>()
                    .join(" + ")
            )
        })
        .collect::<Vec<_>>();
    format!(" where {}", predicates.join(", "))
}

fn format_param(param: &hew_parser::ast::Param, consumes_receiver: bool) -> String {
    let prefix = if param.is_consume || param.is_receiver && consumes_receiver {
        "consume "
    } else if param.is_mutable {
        "var "
    } else {
        ""
    };
    let mut rendered = if param.is_receiver {
        format!("{prefix}self")
    } else {
        format!("{prefix}{}: {}", param.name, format_type(&param.ty.0))
    };
    if let Some((default, _)) = &param.default {
        rendered.push_str(" = ");
        rendered.push_str(&format_literal(default));
    }
    rendered
}

/// Build a function signature string from a function declaration.
fn build_fn_signature(f: &hew_parser::ast::FnDecl) -> String {
    let mut sig = String::new();
    sig.push_str(visibility_prefix(f.visibility));
    if f.is_generator {
        sig.push_str("gen ");
    }
    sig.push_str("fn ");
    sig.push_str(f.name.name.as_str());
    sig.push_str(&format_type_params(f.type_params.as_deref()));
    sig.push('(');
    let params: Vec<String> = f
        .params
        .iter()
        .map(|param| format_param(param, f.consumes_self))
        .collect();
    sig.push_str(&params.join(", "));
    sig.push(')');
    if let Some(ret) = &f.return_type {
        sig.push_str(" -> ");
        sig.push_str(&format_type(&ret.0));
    }
    sig.push_str(&format_where_clause(f.where_clause.as_ref()));
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
    sig.push_str(r.name.name.as_str());
    sig.push('(');
    let params: Vec<String> = r
        .params
        .iter()
        .map(|param| format_param(param, false))
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
    sig.push_str(m.name.name.as_str());
    sig.push_str(&format_type_params(m.type_params.as_deref()));
    sig.push('(');
    let params: Vec<String> = m
        .params
        .iter()
        .map(|param| format_param(param, m.consumes_self))
        .collect();
    sig.push_str(&params.join(", "));
    sig.push(')');
    if let Some(ret) = &m.return_type {
        sig.push_str(" -> ");
        sig.push_str(&format_type(&ret.0));
    }
    sig.push_str(&format_where_clause(m.where_clause.as_ref()));
    sig
}

fn build_impl_signature(i: &hew_parser::ast::ImplDecl) -> String {
    format!(
        "impl{} {}{}",
        format_type_params(i.type_params.as_deref()),
        format_type(&i.target_type.0),
        format_where_clause(i.where_clause.as_ref())
    )
}

fn build_machine_signature(machine: &hew_parser::ast::MachineDecl) -> String {
    let mut params = machine
        .type_params
        .iter()
        .map(|param| {
            if param.bounds.is_empty() {
                param.name.to_string()
            } else {
                format!(
                    "{}: {}",
                    param.name,
                    param
                        .bounds
                        .iter()
                        .map(format_trait_bound)
                        .collect::<Vec<_>>()
                        .join(" + ")
                )
            }
        })
        .collect::<Vec<_>>();
    params.extend(machine.const_params.iter().map(|param| {
        param.default.map_or_else(
            || format!("const {}: usize", param.name),
            |default| format!("const {}: usize = {default}", param.name),
        )
    }));
    let generics = if params.is_empty() {
        String::new()
    } else {
        format!("<{}>", params.join(", "))
    };
    format!(
        "{}machine {}{}{}",
        visibility_prefix(machine.visibility),
        machine.name,
        generics,
        format_where_clause(machine.where_clause.as_ref())
    )
}

fn machine_member(
    name: hew_parser::ast::Ident,
    fields: &[(
        hew_parser::ast::Ident,
        hew_parser::ast::Spanned<hew_parser::ast::TypeExpr>,
    )],
) -> DocMachineMember {
    let shape = if fields.is_empty() {
        String::new()
    } else {
        format!(
            " {{ {} }}",
            fields
                .iter()
                .map(|(field, ty)| format!("{field}: {}", format_type(&ty.0)))
                .collect::<Vec<_>>()
                .join(", ")
        )
    };
    DocMachineMember {
        name: name.to_string(),
        shape,
    }
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
                    name: name.to_string(),
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
                    name: v.name.to_string(),
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
    let mut trait_implementations: HashMap<String, Vec<String>> = HashMap::new();
    for (item, _) in &program.items {
        let Item::Impl(implementation) = item else {
            continue;
        };
        let Some(trait_bound) = &implementation.trait_bound else {
            continue;
        };
        let Some((trait_name, _)) = trait_bound.path.segments.last() else {
            continue;
        };
        trait_implementations
            .entry(trait_name.to_string())
            .or_default()
            .push(format_type(&implementation.target_type.0));
    }

    let mut functions = Vec::new();
    let mut types = Vec::new();
    let mut actors = Vec::new();
    let mut machines = Vec::new();
    let mut traits = Vec::new();
    let mut impls = Vec::new();
    let mut consts = Vec::new();
    let mut type_aliases = Vec::new();

    for (item, _span) in &program.items {
        match item {
            Item::Function(f) if f.visibility.is_pub() => {
                functions.push(DocFunction {
                    name: f.name.to_string(),
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
                    name: t.name.to_string(),
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
                        name: f.name.to_string(),
                        ty: format_type(&f.ty.0),
                        doc: f.doc_comment.clone(),
                    })
                    .collect();
                let handlers: Vec<DocMethod> = a
                    .receive_fns
                    .iter()
                    .map(|r| DocMethod {
                        name: r.name.to_string(),
                        signature: build_receive_signature(r),
                        doc: r.doc_comment.clone(),
                    })
                    .collect();
                actors.push(DocActor {
                    name: a.name.to_string(),
                    fields,
                    handlers,
                    doc: a.doc_comment.clone(),
                });
            }
            Item::Machine(machine) if machine.visibility.is_pub() => {
                machines.push(DocMachine {
                    name: machine.name.to_string(),
                    signature: build_machine_signature(machine),
                    states: machine
                        .states
                        .iter()
                        .map(|state| machine_member(state.name, &state.fields))
                        .collect(),
                    events: machine
                        .events
                        .iter()
                        .map(|event| machine_member(event.name, &event.fields))
                        .collect(),
                    outputs: machine
                        .emits
                        .iter()
                        .map(|output| machine_member(output.name, &output.fields))
                        .collect(),
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
                                name: m.name.to_string(),
                                signature: build_trait_method_signature(m),
                                doc: m.doc_comment.clone(),
                            })
                        } else {
                            None
                        }
                    })
                    .collect();
                traits.push(DocTrait {
                    name: t.name.to_string(),
                    methods,
                    doc: t.doc_comment.clone(),
                    is_public: t.visibility.is_pub(),
                    implementation_targets: trait_implementations
                        .remove(t.name.name.as_str())
                        .unwrap_or_default(),
                });
            }
            Item::Impl(i) if i.trait_bound.is_none() => {
                let methods = i
                    .methods
                    .iter()
                    .filter(|method| method.visibility.is_pub() || method.doc_comment.is_some())
                    .map(|method| DocMethod {
                        name: method.name.to_string(),
                        signature: build_fn_signature(method),
                        doc: method.doc_comment.clone(),
                    })
                    .collect::<Vec<_>>();
                if !methods.is_empty() {
                    impls.push(DocImpl {
                        target: format_type(&i.target_type.0),
                        signature: build_impl_signature(i),
                        methods,
                        doc: i.doc_comment.clone(),
                    });
                }
            }
            Item::Const(c) if c.visibility.is_pub() => {
                consts.push(DocConst {
                    name: c.name.to_string(),
                    ty: format_type(&c.ty.0),
                    value: format_literal(&c.value.0),
                    visibility: visibility_prefix(c.visibility).to_string(),
                    doc: c.doc_comment.clone(),
                });
            }
            Item::TypeAlias(ta) if ta.visibility.is_pub() => {
                type_aliases.push(DocTypeAlias {
                    name: ta.name.to_string(),
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
            | Item::ExternBlock(_)
            | Item::Supervisor(_)
            | Item::Machine(_)
            | Item::Macro(_)
            | Item::MacroCall(_)
            | Item::Record(_) => {} // doc renderer has no record section type yet
        }
    }

    DocModule {
        name: module_name.to_string(),
        doc: program.module_doc.clone(),
        functions,
        types,
        actors,
        machines,
        traits,
        impls,
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
    fn package_visibility_items_are_emitted() {
        let source = r"package fn pkg_fn() {}
pub fn pub_fn() {}
package const PKG_C: i32 = 1;
";
        let result = hew_parser::parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let module = extract_docs(&result.program, "test");
        assert_eq!(module.functions.len(), 2);
        let names: Vec<&str> = module.functions.iter().map(|f| f.name.as_str()).collect();
        assert!(names.contains(&"pkg_fn"));
        assert!(names.contains(&"pub_fn"));
        // Signature carries the actual visibility keyword.
        let pkg_sig = &module
            .functions
            .iter()
            .find(|f| f.name == "pkg_fn")
            .unwrap()
            .signature;
        assert!(pkg_sig.starts_with("package "), "sig: {pkg_sig}");
        assert_eq!(module.consts.len(), 1);
        assert_eq!(module.consts[0].visibility, "package ");
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

    #[test]
    fn signatures_preserve_generics_ownership_defaults_and_bounds() {
        let source = r#"pub fn transform<T, U: Display>(consume item: T, limit: i64 = 2) -> U where T: Clone {
    panic("not called")
}
"#;
        let result = hew_parser::parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let module = extract_docs(&result.program, "test");
        assert_eq!(
            module.functions[0].signature,
            "pub fn transform<T, U: Display>(consume item: T, limit: i64 = 2) -> U where T: Clone"
        );
    }

    #[test]
    fn extracts_documented_inherent_methods() {
        let source = r"impl<T> Option<T> {
    /// Return the contained value.
    pub fn expect(consume self, reason: string) -> T { panic(reason) }
}
";
        let result = hew_parser::parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let module = extract_docs(&result.program, "test");
        assert_eq!(module.impls.len(), 1);
        assert_eq!(module.impls[0].target, "Option<T>");
        assert_eq!(module.impls[0].signature, "impl<T> Option<T>");
        assert_eq!(module.impls[0].methods.len(), 1);
        assert!(module.impls[0].methods[0]
            .signature
            .contains("expect(consume self, reason: string)"));
    }

    #[test]
    fn extracts_public_machine_surface() {
        let source = r"pub machine Door<const LIMIT: usize = 3> {
    events { Open { by: string; } Close; }
    emits { Notice { text: string; } }
    state Shut;
    state Ajar { by: string; }
    on Open: Shut => Ajar { by: event.by }
    on Close: Ajar => Shut;
    default { state }
}
";
        let result = hew_parser::parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let module = extract_docs(&result.program, "test");
        assert_eq!(module.machines.len(), 1);
        let machine = &module.machines[0];
        assert_eq!(
            machine.signature,
            "pub machine Door<const LIMIT: usize = 3>"
        );
        assert_eq!(machine.states[1].shape, " { by: string }");
        assert_eq!(machine.events[0].shape, " { by: string }");
        assert_eq!(machine.outputs[0].name, "Notice");
    }
}
