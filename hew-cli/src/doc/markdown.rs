//! Render [`DocModule`] items to Markdown.

use std::fmt::Write;

use super::config::DocSiteConfig;
use super::extract::{
    DocActor, DocConst, DocField, DocFunction, DocImpl, DocMachine, DocMachineMember, DocMethod,
    DocModule, DocTrait, DocType, DocTypeAlias,
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
fn implementation_target_name(target: &str) -> &str {
    target
        .split('<')
        .next()
        .unwrap_or(target)
        .rsplit(['.', ':'])
        .find(|part| !part.is_empty())
        .unwrap_or(target)
        .trim()
}

fn render_type(t: &DocType, module_traits: &[DocTrait]) -> String {
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
    let methods = module_traits
        .iter()
        .filter(|tr| {
            tr.implementation_targets
                .iter()
                .any(|target| implementation_target_name(target) == t.name.as_str())
        })
        .flat_map(|tr| tr.methods.iter().cloned())
        .collect::<Vec<_>>();
    out.push_str(&render_methods_md("Methods", &methods));
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

fn render_machine_members_md(heading: &str, members: &[DocMachineMember]) -> String {
    if members.is_empty() {
        return String::new();
    }
    let mut out = format!("**{heading}:**\n\n");
    for member in members {
        let _ = writeln!(out, "- `{}{}`", member.name, member.shape);
    }
    out.push('\n');
    out
}

fn render_machine(machine: &DocMachine) -> String {
    let mut out = format!(
        "### Machine `{}`\n\n`{}`\n\n",
        machine.name, machine.signature
    );
    out.push_str(&render_machine_members_md("States", &machine.states));
    out.push_str(&render_machine_members_md("Events", &machine.events));
    out.push_str(&render_machine_members_md("Outputs", &machine.outputs));
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

fn render_impl(i: &DocImpl) -> String {
    let mut out = format!("### `{}`\n\n", i.signature);
    if let Some(doc) = &i.doc {
        out.push_str(doc);
        out.push_str("\n\n");
    }
    out.push_str(&render_methods_md("Methods", &i.methods));
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
pub fn render_module(module: &DocModule, is_guide: bool) -> String {
    let mut out = if is_guide {
        format!("# {}\n\n", super::render::page_title(module, true))
    } else {
        format!("# Module `{}`\n\n", module.name.replace("::", "."))
    };

    if let Some(doc) = &module.doc {
        if is_guide {
            let remainder = doc
                .lines()
                .skip_while(|line| !line.trim().starts_with("# "))
                .skip(1)
                .collect::<Vec<_>>()
                .join("\n");
            out.push_str(&remainder);
        } else {
            out.push_str(doc);
        }
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
            out.push_str(&render_type(t, &module.traits));
        }
    }

    if !module.actors.is_empty() {
        out.push_str("## Actors\n\n");
        for a in &module.actors {
            out.push_str(&render_actor(a));
        }
    }

    if !module.machines.is_empty() {
        out.push_str("## Machines\n\n");
        for machine in &module.machines {
            out.push_str(&render_machine(machine));
        }
    }

    if module.traits.iter().any(|t| t.is_public) {
        out.push_str("## Traits\n\n");
        for t in module.traits.iter().filter(|t| t.is_public) {
            out.push_str(&render_trait(t));
        }
    }

    if !module.impls.is_empty() {
        out.push_str("## Inherent methods\n\n");
        for i in &module.impls {
            out.push_str(&render_impl(i));
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
pub fn render_index(modules: &[DocModule], config: &DocSiteConfig) -> String {
    let mut out = format!(
        "# {}\n\n{}\n\n",
        config.index_title, config.index_description
    );

    let mut language_mods: Vec<&DocModule> = Vec::new();
    let mut root_mods: Vec<&DocModule> = Vec::new();
    let mut groups: std::collections::BTreeMap<String, Vec<&DocModule>> =
        std::collections::BTreeMap::new();

    for m in modules {
        if config.is_guide(&m.name) {
            language_mods.push(m);
            continue;
        }
        let after_root = m.name.find("::").map_or("", |i| &m.name[i + 2..]);
        if after_root.contains("::") {
            let category = after_root.split("::").next().unwrap_or(after_root);
            groups.entry(category.to_string()).or_default().push(m);
        } else {
            root_mods.push(m);
        }
    }
    language_mods.sort_by_key(|module| (config.guide_rank(&module.name), &module.name));

    if !language_mods.is_empty() {
        let _ = writeln!(out, "## {}\n", config.guide_title);
        if config.guide_groups.is_empty() {
            for m in &language_mods {
                render_index_entry_md(&mut out, m, true, config);
            }
        } else {
            for group in &config.guide_groups {
                let _ = writeln!(out, "### {}\n", group.title);
                for guide in &group.guides {
                    if let Some(module) = language_mods
                        .iter()
                        .find(|module| module.name == guide.module)
                    {
                        render_index_entry_md(&mut out, module, true, config);
                    }
                }
                out.push('\n');
            }
        }
        out.push('\n');
    }

    let _ = writeln!(out, "## {}\n", config.module_title);
    for m in &root_mods {
        render_index_entry_md(&mut out, m, false, config);
    }

    for (category, mods) in &groups {
        let _ = writeln!(out, "\n### {category}\n");
        for m in mods {
            render_index_entry_md(&mut out, m, false, config);
        }
    }

    out
}

fn render_index_entry_md(
    out: &mut String,
    module: &DocModule,
    is_guide: bool,
    config: &DocSiteConfig,
) {
    let filename = super::module_to_filename(&module.name, "md");
    let guide = is_guide.then(|| config.guide_entry(&module.name)).flatten();
    if let Some(guide) = guide {
        let _ = write!(out, "- [{}]({filename})", guide.label);
    } else if is_guide {
        let _ = write!(
            out,
            "- [{}]({filename})",
            super::render::page_title(module, true)
        );
    } else {
        let _ = write!(out, "- [`{}`]({filename})", module.name.replace("::", "."));
    }
    let summary = guide.map_or_else(
        || module_summary(module.doc.as_deref()),
        |guide| guide.summary.clone(),
    );
    if !summary.is_empty() {
        let _ = write!(out, " — {summary}");
    }
    out.push('\n');
}

fn module_summary(doc: Option<&str>) -> String {
    let Some(doc) = doc else {
        return String::new();
    };
    let mut lines = doc.lines().map(str::trim).peekable();
    while lines
        .peek()
        .is_some_and(|line| line.is_empty() || line.starts_with('#'))
    {
        lines.next();
    }
    lines
        .take_while(|line| !line.is_empty())
        .collect::<Vec<_>>()
        .join(" ")
}

#[cfg(test)]
mod tests {
    use super::super::config::{GuideEntry, GuideGroup};
    use super::super::extract::extract_docs;
    use super::*;

    #[test]
    fn render_function_markdown() {
        let source = "/// Adds numbers.\npub fn add(a: i32, b: i32) -> i32 {\n    a + b\n}\n";
        let result = hew_parser::parse(source);
        let module = extract_docs(&result.program, "math");
        let md = render_module(&module, false);
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
            machines: vec![],
            traits: vec![],
            impls: vec![],
            consts: vec![],
            type_aliases: vec![],
        };
        let md = render_index(&[module], &DocSiteConfig::default());
        assert!(md.contains("[`std.math`](std.math.md)"));
        assert!(md.contains("Math utilities."));
    }

    #[test]
    fn render_index_markdown_uses_reader_facing_guide_groups() {
        let module = DocModule {
            name: "language::basics".to_string(),
            doc: Some("# Language basics\n\nA long technical introduction.".to_string()),
            functions: vec![],
            types: vec![],
            actors: vec![],
            machines: vec![],
            traits: vec![],
            impls: vec![],
            consts: vec![],
            type_aliases: vec![],
        };
        let config = DocSiteConfig {
            guide_title: "Learn Hew".to_string(),
            guide_groups: vec![GuideGroup {
                title: "Start here".to_string(),
                guides: vec![GuideEntry {
                    module: "language::basics".to_string(),
                    label: "Basics".to_string(),
                    summary: "Write a first program.".to_string(),
                }],
            }],
            ..DocSiteConfig::default()
        };

        let markdown = render_index(&[module], &config);
        assert!(markdown.contains("## Learn Hew"));
        assert!(markdown.contains("### Start here"));
        assert!(markdown.contains("[Basics](language.basics.md) — Write a first program."));
        assert!(!markdown.contains("A long technical introduction."));
    }
}
