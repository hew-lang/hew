//! Walk a parsed Hew AST and extract documentation items.

use hew_parser::ast::{Item, Program, TypeDeclKind};

/// Documentation for a single module (source file).
#[derive(Debug, Clone)]
pub struct DocModule {
    /// Module name (derived from the file name).
    pub name: String,
    /// Module-level documentation from `//!` comments.
    pub doc: Option<String>,
    /// Documented functions.
    pub functions: Vec<DocFunction>,
    /// Documented types (structs, enums).
    pub types: Vec<DocType>,
    /// Documented actors.
    pub actors: Vec<DocActor>,
    /// Documented traits.
    pub traits: Vec<DocTrait>,
}

/// A documented function.
#[derive(Debug, Clone)]
pub struct DocFunction {
    /// Function name.
    pub name: String,
    /// Reconstructed signature string.
    pub signature: String,
    /// Doc comment content.
    pub doc: Option<String>,
}

/// A documented type (struct or enum).
#[derive(Debug, Clone)]
pub struct DocType {
    /// Type name.
    pub name: String,
    /// `"struct"` or `"enum"`.
    pub kind: &'static str,
    /// Field names and type strings (structs only).
    pub fields: Vec<(String, String)>,
    /// Doc comment content.
    pub doc: Option<String>,
}

/// A documented actor.
#[derive(Debug, Clone)]
pub struct DocActor {
    /// Actor name.
    pub name: String,
    /// Actor field names and type strings.
    pub fields: Vec<(String, String)>,
    /// Receive handler names and signatures.
    pub handlers: Vec<(String, String)>,
    /// Doc comment content.
    pub doc: Option<String>,
}

/// A documented trait.
#[derive(Debug, Clone)]
pub struct DocTrait {
    /// Trait name.
    pub name: String,
    /// Method names and signatures.
    pub methods: Vec<(String, String)>,
    /// Doc comment content.
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

/// Build a function signature string from a function declaration.
fn build_fn_signature(f: &hew_parser::ast::FnDecl) -> String {
    let mut sig = String::new();
    match f.visibility {
        hew_parser::ast::Visibility::Private => {}
        hew_parser::ast::Visibility::Pub => sig.push_str("pub "),
        hew_parser::ast::Visibility::PubPackage => sig.push_str("pub(package) "),
        hew_parser::ast::Visibility::PubSuper => sig.push_str("pub(super) "),
    }
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

/// Extract documentation items from a parsed Hew program.
#[must_use]
#[expect(
    clippy::too_many_lines,
    reason = "sequential extraction from each item variant"
)]
pub fn extract_docs(program: &Program, module_name: &str) -> DocModule {
    let mut functions = Vec::new();
    let mut types = Vec::new();
    let mut actors = Vec::new();
    let mut traits = Vec::new();

    for (item, _span) in &program.items {
        match item {
            Item::Function(f) => {
                functions.push(DocFunction {
                    name: f.name.clone(),
                    signature: build_fn_signature(f),
                    doc: f.doc_comment.clone(),
                });
            }
            Item::TypeDecl(t) => {
                let kind = match t.kind {
                    TypeDeclKind::Struct => "struct",
                    TypeDeclKind::Enum => "enum",
                };
                let fields: Vec<(String, String)> = t
                    .body
                    .iter()
                    .filter_map(|item| {
                        if let hew_parser::ast::TypeBodyItem::Field { name, ty } = item {
                            Some((name.clone(), format_type(&ty.0)))
                        } else {
                            None
                        }
                    })
                    .collect();
                types.push(DocType {
                    name: t.name.clone(),
                    kind,
                    fields,
                    doc: t.doc_comment.clone(),
                });
            }
            Item::Actor(a) => {
                let fields: Vec<(String, String)> = a
                    .fields
                    .iter()
                    .map(|f| (f.name.clone(), format_type(&f.ty.0)))
                    .collect();
                let handlers: Vec<(String, String)> = a
                    .receive_fns
                    .iter()
                    .map(|r| (r.name.clone(), build_receive_signature(r)))
                    .collect();
                actors.push(DocActor {
                    name: a.name.clone(),
                    fields,
                    handlers,
                    doc: a.doc_comment.clone(),
                });
            }
            Item::Trait(t) => {
                let methods: Vec<(String, String)> = t
                    .items
                    .iter()
                    .filter_map(|item| {
                        if let hew_parser::ast::TraitItem::Method(m) = item {
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
                            Some((m.name.clone(), sig))
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
            Item::Import(_)
            | Item::Const(_)
            | Item::TypeAlias(_)
            | Item::Impl(_)
            | Item::Wire(_)
            | Item::ExternBlock(_)
            | Item::Supervisor(_) => {}
        }
    }

    DocModule {
        name: module_name.to_string(),
        doc: program.module_doc.clone(),
        functions,
        types,
        actors,
        traits,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extract_function_docs() {
        let source = r#"/// Adds two numbers.
fn add(a: i32, b: i32) -> i32 {
    a + b
}
"#;
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
        let source = r#"//! Module docs here.
//! Second line.

fn foo() {}
"#;
        let result = hew_parser::parse(source);
        assert!(result.errors.is_empty());
        let module = extract_docs(&result.program, "test");
        assert_eq!(
            module.doc.as_deref(),
            Some("Module docs here.\nSecond line.")
        );
    }

    #[test]
    fn extract_struct_docs() {
        let source = r#"/// A point in space.
type Point {
    x: i32;
    y: i32;
}
"#;
        let result = hew_parser::parse(source);
        assert!(result.errors.is_empty());
        let module = extract_docs(&result.program, "test");
        assert_eq!(module.types.len(), 1);
        assert_eq!(module.types[0].name, "Point");
        assert_eq!(module.types[0].kind, "struct");
        assert_eq!(module.types[0].doc.as_deref(), Some("A point in space."));
        assert_eq!(module.types[0].fields.len(), 2);
    }

    #[test]
    fn extract_actor_docs() {
        let source = r#"/// A simple counter actor.
actor Counter {
    count: i32;
    receive fn increment() {
        self.count = self.count + 1;
    }
}
"#;
        let result = hew_parser::parse(source);
        assert!(result.errors.is_empty());
        let module = extract_docs(&result.program, "test");
        assert_eq!(module.actors.len(), 1);
        assert_eq!(module.actors[0].name, "Counter");
        assert_eq!(
            module.actors[0].doc.as_deref(),
            Some("A simple counter actor.")
        );
    }

    #[test]
    fn no_doc_comment_is_none() {
        let source = "fn bare() {}";
        let result = hew_parser::parse(source);
        assert!(result.errors.is_empty());
        let module = extract_docs(&result.program, "test");
        assert_eq!(module.functions.len(), 1);
        assert!(module.functions[0].doc.is_none());
    }
}
