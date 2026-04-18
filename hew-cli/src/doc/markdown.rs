//! Render [`DocModule`] items to Markdown.

use std::fmt::Write;

use super::extract::{
    DocActor, DocConst, DocField, DocFunction, DocMethod, DocModule, DocTrait, DocType,
    DocTypeAlias,
};

fn render_fields_md(fields: &[DocField]) -> String {
    if fields.is_empty() {
        return String::new();
    }
    let mut out = String::from("**Fields:**\n\n");
    for f in fields {
        let _ = writeln!(out, "- `{}: {}`", f.name, f.ty);
        if let Some(doc) = &f.doc {
            for line in doc.lines() {
                let _ = writeln!(out, "  {line}");
            }
        }
    }
    out.push('\n');
    out
}

fn render_methods_md(heading: &str, methods: &[DocMethod]) -> String {
    if methods.is_empty() {
        return String::new();
    }
    let mut out = format!("**{heading}:**\n\n");
    for m in methods {
        let _ = writeln!(out, "- `{}`", m.signature);
        if let Some(doc) = &m.doc {
            for line in doc.lines() {
                let _ = writeln!(out, "  {line}");
            }
        }
    }
    out.push('\n');
    out
}

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
    out.push_str(&render_fields_md(&t.fields));
    if !t.variants.is_empty() {
        out.push_str("**Variants:**\n\n");
        for v in &t.variants {
            let _ = writeln!(out, "- `{}{}`", v.name, v.shape);
            if let Some(doc) = &v.doc {
                for line in doc.lines() {
                    let _ = writeln!(out, "  {line}");
                }
            }
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
    out.push_str(&render_fields_md(&a.fields));
    out.push_str(&render_methods_md("Handlers", &a.handlers));
    out
}

/// Render a trait item as Markdown.
fn render_trait(t: &DocTrait) -> String {
    let mut out = format!("### Trait `{}`\n\n", t.name);
    if let Some(doc) = &t.doc {
        out.push_str(doc);
        out.push_str("\n\n");
    }
    out.push_str(&render_methods_md("Methods", &t.methods));
    out
}

/// Render a constant item as Markdown.
fn render_const(c: &DocConst) -> String {
    let mut out = format!(
        "### `{}const {}: {} = {}`\n\n",
        c.visibility, c.name, c.ty, c.value,
    );
    if let Some(doc) = &c.doc {
        out.push_str(doc);
        out.push_str("\n\n");
    }
    out
}

/// Render a type-alias item as Markdown.
fn render_type_alias(ta: &DocTypeAlias) -> String {
    let mut out = format!("### `{}type {} = {}`\n\n", ta.visibility, ta.name, ta.ty);
    if let Some(doc) = &ta.doc {
        out.push_str(doc);
        out.push_str("\n\n");
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

    if !module.consts.is_empty() {
        out.push_str("## Constants\n\n");
        for c in &module.consts {
            out.push_str(&render_const(c));
        }
    }

    if !module.type_aliases.is_empty() {
        out.push_str("## Type aliases\n\n");
        for ta in &module.type_aliases {
            out.push_str(&render_type_alias(ta));
        }
    }

    out
}

/// Render an index page listing all documented modules as Markdown, grouped by section.
#[must_use]
pub fn render_index(modules: &[DocModule]) -> String {
    let mut out = String::from("# Hew Standard Library\n\n");

    let mut root_mods: Vec<&DocModule> = Vec::new();
    let mut groups: std::collections::BTreeMap<String, Vec<&DocModule>> =
        std::collections::BTreeMap::new();

    for m in modules {
        let after_root = m.name.find("::").map_or("", |i| &m.name[i + 2..]);
        if after_root.contains("::") {
            let category = after_root.split("::").next().unwrap_or(after_root);
            groups.entry(category.to_string()).or_default().push(m);
        } else {
            root_mods.push(m);
        }
    }

    for m in &root_mods {
        let filename = super::module_to_filename(&m.name, "md");
        let _ = write!(out, "- [`{}`]({})", m.name, filename);
        if let Some(doc) = &m.doc {
            if let Some(first_line) = doc.lines().next() {
                let _ = write!(out, " — {first_line}");
            }
        }
        out.push('\n');
    }

    for (category, mods) in &groups {
        let _ = writeln!(out, "\n## {category}\n");
        for m in mods {
            let filename = super::module_to_filename(&m.name, "md");
            let _ = write!(out, "- [`{}`]({})", m.name, filename);
            if let Some(doc) = &m.doc {
                if let Some(first_line) = doc.lines().next() {
                    let _ = write!(out, " — {first_line}");
                }
            }
            out.push('\n');
        }
    }

    out
}

#[cfg(test)]
mod tests {
    use super::super::extract::extract_docs;
    use super::*;

    #[test]
    fn render_function_markdown() {
        let source = "/// Adds numbers.\npub fn add(a: i32, b: i32) -> i32 {\n    a + b\n}\n";
        let result = hew_parser::parse(source);
        let module = extract_docs(&result.program, "math");
        let md = render_module(&module);
        assert!(md.contains("# Module `math`"));
        assert!(md.contains("## Functions"));
        assert!(md.contains("### `pub fn add(a: i32, b: i32) -> i32`"));
        assert!(md.contains("Adds numbers."));
    }

    #[test]
    fn render_index_markdown() {
        let module = DocModule {
            name: "std::math".to_string(),
            doc: Some("Math utilities.".to_string()),
            functions: vec![],
            types: vec![],
            actors: vec![],
            traits: vec![],
            consts: vec![],
            type_aliases: vec![],
        };
        let md = render_index(&[module]);
        assert!(md.contains("[`std::math`](std.math.md)"));
        assert!(md.contains("Math utilities."));
    }
}
