//! Render [`DocModule`] items to Markdown.

use std::fmt::Write;

use super::extract::{DocActor, DocFunction, DocModule, DocTrait, DocType};

/// Render a function item as Markdown.
fn render_function(f: &DocFunction) -> String {
    let mut out = format!("### `{}`\n\n", f.signature);
    if let Some(doc) = &f.doc {
        out.push_str(doc);
        out.push_str("\n\n");
    }
    out
}

/// Render a type item as Markdown.
fn render_type(t: &DocType) -> String {
    let label = if t.kind == "struct" { "Struct" } else { "Enum" };
    let mut out = format!("### {label} `{}`\n\n", t.name);
    if let Some(doc) = &t.doc {
        out.push_str(doc);
        out.push_str("\n\n");
    }
    if !t.fields.is_empty() {
        out.push_str("**Fields:**\n\n");
        for (name, ty) in &t.fields {
            let _ = writeln!(out, "- `{name}: {ty}`");
        }
        out.push('\n');
    }
    out
}

/// Render an actor item as Markdown.
fn render_actor(a: &DocActor) -> String {
    let mut out = format!("### Actor `{}`\n\n", a.name);
    if let Some(doc) = &a.doc {
        out.push_str(doc);
        out.push_str("\n\n");
    }
    if !a.fields.is_empty() {
        out.push_str("**Fields:**\n\n");
        for (name, ty) in &a.fields {
            let _ = writeln!(out, "- `{name}: {ty}`");
        }
        out.push('\n');
    }
    if !a.handlers.is_empty() {
        out.push_str("**Handlers:**\n\n");
        for (_name, sig) in &a.handlers {
            let _ = writeln!(out, "- `{sig}`");
        }
        out.push('\n');
    }
    out
}

/// Render a trait item as Markdown.
fn render_trait(t: &DocTrait) -> String {
    let mut out = format!("### Trait `{}`\n\n", t.name);
    if let Some(doc) = &t.doc {
        out.push_str(doc);
        out.push_str("\n\n");
    }
    if !t.methods.is_empty() {
        out.push_str("**Methods:**\n\n");
        for (_name, sig) in &t.methods {
            let _ = writeln!(out, "- `{sig}`");
        }
        out.push('\n');
    }
    out
}

/// Render a full module page as Markdown.
#[must_use]
pub fn render_module(module: &DocModule) -> String {
    let mut out = format!("# Module `{}`\n\n", module.name);

    if let Some(doc) = &module.doc {
        out.push_str(doc);
        out.push_str("\n\n");
    }

    if !module.functions.is_empty() {
        out.push_str("## Functions\n\n");
        for f in &module.functions {
            out.push_str(&render_function(f));
        }
    }

    if !module.types.is_empty() {
        out.push_str("## Types\n\n");
        for t in &module.types {
            out.push_str(&render_type(t));
        }
    }

    if !module.actors.is_empty() {
        out.push_str("## Actors\n\n");
        for a in &module.actors {
            out.push_str(&render_actor(a));
        }
    }

    if !module.traits.is_empty() {
        out.push_str("## Traits\n\n");
        for t in &module.traits {
            out.push_str(&render_trait(t));
        }
    }

    out
}

/// Render an index page listing all documented modules as Markdown.
#[must_use]
pub fn render_index(modules: &[DocModule]) -> String {
    let mut out = String::from("# Hew Documentation\n\n");
    for m in modules {
        let _ = write!(out, "- [{}]({}.md)", m.name, m.name);
        if let Some(doc) = &m.doc {
            if let Some(first_line) = doc.lines().next() {
                let _ = write!(out, " — {first_line}");
            }
        }
        out.push('\n');
    }
    out
}

#[cfg(test)]
mod tests {
    use super::super::extract::extract_docs;
    use super::*;

    #[test]
    fn render_function_markdown() {
        let source = "/// Adds numbers.\nfn add(a: i32, b: i32) -> i32 {\n    a + b\n}\n";
        let result = hew_parser::parse(source);
        let module = extract_docs(&result.program, "math");
        let md = render_module(&module);
        assert!(md.contains("# Module `math`"));
        assert!(md.contains("## Functions"));
        assert!(md.contains("### `fn add(a: i32, b: i32) -> i32`"));
        assert!(md.contains("Adds numbers."));
    }

    #[test]
    fn render_index_markdown() {
        let module = DocModule {
            name: "math".to_string(),
            doc: Some("Math utilities.".to_string()),
            functions: vec![],
            types: vec![],
            actors: vec![],
            traits: vec![],
        };
        let md = render_index(&[module]);
        assert!(md.contains("[math](math.md)"));
        assert!(md.contains("Math utilities."));
    }
}
