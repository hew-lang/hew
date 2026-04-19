//! Render [`DocModule`] items to HTML fragments.

use pulldown_cmark::{CodeBlockKind, Event, HeadingLevel, Options, Parser, Tag, TagEnd};
use std::collections::HashMap;

use super::extract::{
    DocActor, DocConst, DocField, DocFunction, DocMethod, DocModule, DocTrait, DocType,
    DocTypeAlias,
};
use super::highlight;
use super::template::{highlight_signature, html_escape};

/// Shift a pulldown-cmark heading level by `offset`, clamping to 1–6.
fn shift_level(level: HeadingLevel, offset: u8) -> HeadingLevel {
    let n = level as u8;
    let shifted = (n + offset).min(6);
    match shifted {
        1 => HeadingLevel::H1,
        2 => HeadingLevel::H2,
        3 => HeadingLevel::H3,
        4 => HeadingLevel::H4,
        5 => HeadingLevel::H5,
        _ => HeadingLevel::H6,
    }
}

/// Convert a Markdown doc comment to an HTML fragment.
///
/// `heading_offset` is added to every heading level so that `# Examples`
/// inside a doc comment (which would normally emit `<h1>`) becomes `<h4>`
/// when the item heading is `<h3>` (offset = 3).  Pass `0` for no shift.
///
/// Code blocks with language `hew` (or no language) are highlighted using
/// the lexer-based Shiki-compatible highlighter.
fn markdown_to_html(md: &str, heading_offset: u8) -> String {
    let opts =
        Options::ENABLE_TABLES | Options::ENABLE_STRIKETHROUGH | Options::ENABLE_HEADING_ATTRIBUTES;
    let parser = Parser::new_ext(md, opts);

    let mut html = String::new();
    let mut in_code_block = false;
    let mut code_lang = String::new();
    let mut code_buf = String::new();

    for event in parser {
        let event = match event {
            // Shift heading start
            Event::Start(Tag::Heading {
                level,
                id,
                classes,
                attrs,
            }) => {
                let new_level = shift_level(level, heading_offset);
                Event::Start(Tag::Heading {
                    level: new_level,
                    id,
                    classes,
                    attrs,
                })
            }
            // Shift heading end
            Event::End(TagEnd::Heading(level)) => {
                let new_level = shift_level(level, heading_offset);
                Event::End(TagEnd::Heading(new_level))
            }
            other => other,
        };

        match event {
            Event::Start(Tag::CodeBlock(kind)) => {
                in_code_block = true;
                code_buf.clear();
                code_lang = match kind {
                    CodeBlockKind::Fenced(lang) => lang.to_string(),
                    CodeBlockKind::Indented => String::new(),
                };
            }
            Event::End(TagEnd::CodeBlock) => {
                in_code_block = false;
                if code_lang.is_empty() || code_lang == "hew" {
                    html.push_str(&highlight::highlight(&code_buf));
                } else {
                    html.push_str("<pre><code>");
                    html.push_str(&html_escape(&code_buf));
                    html.push_str("</code></pre>");
                }
            }
            Event::Text(text) if in_code_block => {
                code_buf.push_str(&text);
            }
            _ => {
                if !in_code_block {
                    let single = std::iter::once(event);
                    pulldown_cmark::html::push_html(&mut html, single);
                }
            }
        }
    }

    html
}

fn push_doc(out: &mut String, doc: Option<&String>) {
    if let Some(d) = doc {
        out.push_str("<div class=\"doc\">");
        // Items are rendered under <h3>; shift doc `#` → <h4> (offset 3).
        out.push_str(&markdown_to_html(d, 3));
        out.push_str("</div>\n");
    }
}

/// Render an inline doc fragment (used inside dl entries and method lists).
fn render_inline_doc(doc: Option<&String>) -> String {
    if let Some(d) = doc {
        // Method/field docs sit inside an <h4> context; shift doc `#` → <h5> (offset 4).
        format!(
            "<div class=\"inline-doc\">{}</div>\n",
            markdown_to_html(d, 4)
        )
    } else {
        String::new()
    }
}

fn render_fields_dl(fields: &[DocField]) -> String {
    if fields.is_empty() {
        return String::new();
    }
    let mut out = String::from("<h4>Fields</h4>\n<dl class=\"fields\">\n");
    for field in fields {
        out.push_str("<dt>");
        out.push_str(&html_escape(&field.name));
        out.push_str(": <span class=\"ty\">");
        out.push_str(&html_escape(&field.ty));
        out.push_str("</span></dt>\n");
        if let Some(doc) = &field.doc {
            out.push_str("<dd>");
            out.push_str(&markdown_to_html(doc, 4));
            out.push_str("</dd>\n");
        }
    }
    out.push_str("</dl>\n");
    out
}

/// Map from bare type name to its intra-page anchor href (`#type.Foo`).
///
/// Used by `link_signature_types` to wrap type-coloured tokens in signature
/// HTML with clickable anchors. Only same-module types are indexed; bare names
/// are always unique within one module so no disambiguation is needed.
type TypeIndex = HashMap<String, String>;

/// Build a type index for the current module: maps bare type name → anchor href.
///
/// Types (`#type.Foo`), actors (`#actor.Foo`), and traits (`#trait.Foo`) are
/// all indexed. Same-module only; inter-module links are out of scope for this
/// feature — following cross-module refs would require a multi-module index not
/// available at per-module render time.
fn build_type_index(module: &DocModule) -> TypeIndex {
    let mut index = TypeIndex::new();
    for t in &module.types {
        index.insert(t.name.clone(), format!("#type.{}", t.name));
    }
    for a in &module.actors {
        index.insert(a.name.clone(), format!("#actor.{}", a.name));
    }
    for t in &module.traits {
        index.insert(t.name.clone(), format!("#trait.{}", t.name));
    }
    index
}

/// Post-process a highlighted signature HTML string, wrapping type-coloured
/// tokens with intra-page anchor links when the type appears in `index`.
///
/// The highlight step emits `<span style="color:#2dd4bf">TypeName</span>` for
/// `PascalCase` identifiers and built-in type names. This function scans for
/// those spans and replaces them with
/// `<a href="..."><span ...>TypeName</span></a>` when the unescaped content
/// matches an indexed type name.
fn link_signature_types(highlighted: &str, index: &TypeIndex) -> String {
    // The colour emitted by highlight_signature / token_color for type tokens.
    const TYPE_COLOR: &str = "#2dd4bf";
    let open_tag = format!("<span style=\"color:{TYPE_COLOR}\">");

    if index.is_empty() {
        return highlighted.to_string();
    }

    let mut out = String::with_capacity(highlighted.len() + index.len() * 30);
    let mut rest = highlighted;

    while let Some(pos) = rest.find(&open_tag) {
        out.push_str(&rest[..pos]);
        let after_open = &rest[pos + open_tag.len()..];
        // Find the matching </span>
        if let Some(end) = after_open.find("</span>") {
            let inner = &after_open[..end];
            // Reverse HTML-escape to get the plain type name for index lookup
            let name = inner
                .replace("&amp;", "&")
                .replace("&lt;", "<")
                .replace("&gt;", ">")
                .replace("&quot;", "\"");
            if let Some(href) = index.get(&name) {
                out.push_str("<a href=\"");
                out.push_str(href);
                out.push_str("\">");
                out.push_str(&open_tag);
                out.push_str(inner);
                out.push_str("</span></a>");
            } else {
                // Not in index — emit unchanged
                out.push_str(&open_tag);
                out.push_str(inner);
                out.push_str("</span>");
            }
            rest = &after_open[end + "</span>".len()..];
        } else {
            // Malformed span — emit as-is
            out.push_str(&rest[pos..]);
            return out;
        }
    }

    out.push_str(rest);
    out
}

/// Render a list of methods or handlers under a named heading. `owner` is
/// the parent item's name (trait or actor), used to namespace each
/// method's HTML `id` so two traits on the same page can expose methods
/// with overlapping names without colliding on the anchor.
fn render_methods_list(
    heading: &str,
    owner: &str,
    methods: &[DocMethod],
    index: &TypeIndex,
) -> String {
    if methods.is_empty() {
        return String::new();
    }
    let mut out = format!("<h4>{}</h4>\n", html_escape(heading));
    for m in methods {
        out.push_str("<div class=\"method\" id=\"method.");
        out.push_str(&html_escape(owner));
        out.push('.');
        out.push_str(&html_escape(&m.name));
        out.push_str("\">\n");
        out.push_str("<span class=\"sig\">");
        let highlighted = highlight_signature(&m.signature);
        out.push_str(&link_signature_types(&highlighted, index));
        out.push_str("</span>\n");
        out.push_str(&render_inline_doc(m.doc.as_ref()));
        out.push_str("</div>\n");
    }
    out
}

/// Render a function item.
fn render_function(f: &DocFunction, index: &TypeIndex) -> String {
    let mut out = String::from("<div class=\"item\" id=\"fn.");
    out.push_str(&html_escape(&f.name));
    out.push_str("\">\n");
    out.push_str("<h3>Function <code>");
    out.push_str(&html_escape(&f.name));
    out.push_str("</code></h3>\n");
    out.push_str("<span class=\"sig\">");
    let highlighted = highlight_signature(&f.signature);
    out.push_str(&link_signature_types(&highlighted, index));
    out.push_str("</span>\n");
    push_doc(&mut out, f.doc.as_ref());
    out.push_str("</div>\n");
    out
}

/// Extract the type name of the first parameter from a formatted trait method
/// signature string such as `fn foo(val: Value, key: String) -> i32`.
///
/// Returns `None` when the parameter list is empty.  Used by
/// `traits_for_type` to decide whether a trait "belongs to" a given type.
fn first_param_type(sig: &str) -> Option<&str> {
    // Skip past 'fn name(' to the parameter list.
    let after_paren = sig.find('(').map(|i| &sig[i + 1..])?;
    // First param may be "name: Type" or just "Type".
    let (_, ty_part) = after_paren.split_once(':').unwrap_or(("", after_paren));
    // Take the first identifier token (stops at whitespace/punct).
    let ty_name = ty_part
        .trim_start()
        .split(|c: char| !c.is_alphanumeric() && c != '_')
        .next()?;
    if ty_name.is_empty() {
        None
    } else {
        Some(ty_name)
    }
}

/// Return the subset of `traits` whose methods have `type_name` as the type
/// of their first parameter.
///
/// This is the same-module heuristic used to surface trait methods directly
/// on an opaque type's entry.  Only same-module traits are considered; no
/// cross-module lookup is performed.
fn traits_for_type<'a>(traits: &'a [DocTrait], type_name: &str) -> Vec<&'a DocTrait> {
    traits
        .iter()
        .filter(|tr| {
            tr.methods
                .iter()
                .any(|m| first_param_type(&m.signature) == Some(type_name))
        })
        .collect()
}

/// Render a type item (struct or enum).
///
/// When same-module traits expose methods that take this type as their first
/// parameter, a "Methods" block is rendered on the type's entry so users do
/// not need to navigate to a separate `Trait FooMethods` section.
fn render_type(t: &DocType, index: &TypeIndex, module_traits: &[DocTrait]) -> String {
    let mut out = String::from("<div class=\"item\" id=\"type.");
    out.push_str(&html_escape(&t.name));
    out.push_str("\">\n");
    out.push_str("<h3>");
    out.push_str(if t.kind == "struct" { "Struct" } else { "Enum" });
    out.push_str(" <code>");
    out.push_str(&html_escape(&t.name));
    out.push_str("</code></h3>\n");
    push_doc(&mut out, t.doc.as_ref());
    out.push_str(&render_fields_dl(&t.fields));
    if !t.variants.is_empty() {
        out.push_str("<h4>Variants</h4>\n<dl class=\"variants\">\n");
        for v in &t.variants {
            out.push_str("<dt><code>");
            out.push_str(&html_escape(&v.name));
            if !v.shape.is_empty() {
                out.push_str(&html_escape(&v.shape));
            }
            out.push_str("</code></dt>\n");
            if let Some(doc) = &v.doc {
                out.push_str("<dd>");
                out.push_str(&markdown_to_html(doc, 4));
                out.push_str("</dd>\n");
            }
        }
        out.push_str("</dl>\n");
    }

    // Surface methods from same-module traits whose first parameter is this type.
    let implementors = traits_for_type(module_traits, &t.name);
    if !implementors.is_empty() {
        out.push_str("<h4>Methods</h4>\n");
        for tr in implementors {
            let applicable: Vec<&DocMethod> = tr
                .methods
                .iter()
                .filter(|m| first_param_type(&m.signature) == Some(t.name.as_str()))
                .collect();
            if !applicable.is_empty() {
                out.push_str("<p class=\"trait-ref\">via <a href=\"#trait.");
                out.push_str(&html_escape(&tr.name));
                out.push_str("\"><code>");
                out.push_str(&html_escape(&tr.name));
                out.push_str("</code></a></p>\n");
                for m in applicable {
                    out.push_str("<div class=\"method\" id=\"method.");
                    out.push_str(&html_escape(&tr.name));
                    out.push('.');
                    out.push_str(&html_escape(&m.name));
                    out.push_str("\">\n<span class=\"sig\">");
                    let highlighted = highlight_signature(&m.signature);
                    out.push_str(&link_signature_types(&highlighted, index));
                    out.push_str("</span>\n");
                    out.push_str(&render_inline_doc(m.doc.as_ref()));
                    out.push_str("</div>\n");
                }
            }
        }
    }

    out.push_str("</div>\n");
    out
}

/// Render an actor item.
fn render_actor(a: &DocActor, index: &TypeIndex) -> String {
    let mut out = String::from("<div class=\"item\" id=\"actor.");
    out.push_str(&html_escape(&a.name));
    out.push_str("\">\n");
    out.push_str("<h3>Actor <code>");
    out.push_str(&html_escape(&a.name));
    out.push_str("</code></h3>\n");
    push_doc(&mut out, a.doc.as_ref());
    out.push_str(&render_fields_dl(&a.fields));
    out.push_str(&render_methods_list(
        "Handlers",
        &a.name,
        &a.handlers,
        index,
    ));
    out.push_str("</div>\n");
    out
}

/// Render a trait item.
fn render_trait(t: &DocTrait, index: &TypeIndex) -> String {
    let mut out = String::from("<div class=\"item\" id=\"trait.");
    out.push_str(&html_escape(&t.name));
    out.push_str("\">\n");
    out.push_str("<h3>Trait <code>");
    out.push_str(&html_escape(&t.name));
    out.push_str("</code></h3>\n");
    push_doc(&mut out, t.doc.as_ref());
    out.push_str(&render_methods_list("Methods", &t.name, &t.methods, index));
    out.push_str("</div>\n");
    out
}

/// Render a constant item.
fn render_const(c: &DocConst) -> String {
    let mut out = String::from("<div class=\"item\" id=\"const.");
    out.push_str(&html_escape(&c.name));
    out.push_str("\">\n");
    out.push_str("<h3>Constant <code>");
    out.push_str(&html_escape(&c.name));
    out.push_str("</code></h3>\n");
    let sig = format!("{}const {}: {} = {}", c.visibility, c.name, c.ty, c.value);
    out.push_str("<span class=\"sig\">");
    out.push_str(&highlight_signature(&sig));
    out.push_str("</span>\n");
    push_doc(&mut out, c.doc.as_ref());
    out.push_str("</div>\n");
    out
}

/// Render a type-alias item.
fn render_type_alias(ta: &DocTypeAlias) -> String {
    let mut out = String::from("<div class=\"item\" id=\"typealias.");
    out.push_str(&html_escape(&ta.name));
    out.push_str("\">\n");
    out.push_str("<h3>Type alias <code>");
    out.push_str(&html_escape(&ta.name));
    out.push_str("</code></h3>\n");
    let sig = format!("{}type {} = {}", ta.visibility, ta.name, ta.ty);
    out.push_str("<span class=\"sig\">");
    out.push_str(&highlight_signature(&sig));
    out.push_str("</span>\n");
    push_doc(&mut out, ta.doc.as_ref());
    out.push_str("</div>\n");
    out
}

/// Render a full module page body (without the outer HTML wrapper).
#[must_use]
#[expect(
    clippy::too_many_lines,
    reason = "sequential rendering of each item kind"
)]
pub fn render_module(module: &DocModule) -> String {
    let mut body = String::new();

    body.push_str("<h1>Module <code>");
    body.push_str(&html_escape(&module.name));
    body.push_str("</code></h1>\n");

    // Module-level docs are under <h1>; shift doc `#` → <h2> (offset 1).
    if let Some(d) = &module.doc {
        body.push_str("<div class=\"doc\">");
        body.push_str(&markdown_to_html(d, 1));
        body.push_str("</div>\n");
    }

    // Build the intra-page type index for cross-linking in signatures.
    let index = build_type_index(module);

    // Table of contents
    let has_items = !module.functions.is_empty()
        || !module.types.is_empty()
        || !module.actors.is_empty()
        || !module.traits.is_empty()
        || !module.consts.is_empty()
        || !module.type_aliases.is_empty();
    if has_items {
        body.push_str("<h2>Contents</h2>\n<ul>\n");
        for f in &module.functions {
            body.push_str("<li><a href=\"#fn.");
            body.push_str(&html_escape(&f.name));
            body.push_str("\">fn ");
            body.push_str(&html_escape(&f.name));
            body.push_str("</a></li>\n");
        }
        for t in &module.types {
            body.push_str("<li><a href=\"#type.");
            body.push_str(&html_escape(&t.name));
            body.push_str("\">");
            body.push_str(t.kind);
            body.push(' ');
            body.push_str(&html_escape(&t.name));
            body.push_str("</a></li>\n");
        }
        for a in &module.actors {
            body.push_str("<li><a href=\"#actor.");
            body.push_str(&html_escape(&a.name));
            body.push_str("\">actor ");
            body.push_str(&html_escape(&a.name));
            body.push_str("</a></li>\n");
        }
        for t in &module.traits {
            body.push_str("<li><a href=\"#trait.");
            body.push_str(&html_escape(&t.name));
            body.push_str("\">trait ");
            body.push_str(&html_escape(&t.name));
            body.push_str("</a></li>\n");
        }
        for c in &module.consts {
            body.push_str("<li><a href=\"#const.");
            body.push_str(&html_escape(&c.name));
            body.push_str("\">const ");
            body.push_str(&html_escape(&c.name));
            body.push_str("</a></li>\n");
        }
        for ta in &module.type_aliases {
            body.push_str("<li><a href=\"#typealias.");
            body.push_str(&html_escape(&ta.name));
            body.push_str("\">type ");
            body.push_str(&html_escape(&ta.name));
            body.push_str("</a></li>\n");
        }
        body.push_str("</ul>\n");
    }

    if !module.functions.is_empty() {
        body.push_str("<h2>Functions</h2>\n");
        for f in &module.functions {
            body.push_str(&render_function(f, &index));
        }
    }

    if !module.types.is_empty() {
        body.push_str("<h2>Types</h2>\n");
        for t in &module.types {
            body.push_str(&render_type(t, &index, &module.traits));
        }
    }

    if !module.actors.is_empty() {
        body.push_str("<h2>Actors</h2>\n");
        for a in &module.actors {
            body.push_str(&render_actor(a, &index));
        }
    }

    if !module.traits.is_empty() {
        body.push_str("<h2>Traits</h2>\n");
        for t in &module.traits {
            body.push_str(&render_trait(t, &index));
        }
    }

    if !module.consts.is_empty() {
        body.push_str("<h2>Constants</h2>\n");
        for c in &module.consts {
            body.push_str(&render_const(c));
        }
    }

    if !module.type_aliases.is_empty() {
        body.push_str("<h2>Type aliases</h2>\n");
        for ta in &module.type_aliases {
            body.push_str(&render_type_alias(ta));
        }
    }

    body
}

/// Render an index page listing all documented modules, grouped by section.
#[must_use]
pub fn render_index(modules: &[DocModule]) -> String {
    let mut body = String::from("<h1>Hew Standard Library</h1>\n");

    // Group: root-level modules (single :: component after prefix) first,
    // then by the second path component (e.g., "encoding", "crypto").
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

    if !root_mods.is_empty() {
        body.push_str("<ul class=\"module-list\">\n");
        for m in &root_mods {
            render_index_entry(&mut body, m);
        }
        body.push_str("</ul>\n");
    }

    for (category, mods) in &groups {
        body.push_str("<div class=\"section-heading\">");
        body.push_str(&html_escape(category));
        body.push_str("</div>\n<ul class=\"module-list\">\n");
        for m in mods {
            render_index_entry(&mut body, m);
        }
        body.push_str("</ul>\n");
    }

    body
}

fn render_index_entry(body: &mut String, m: &DocModule) {
    let filename = super::module_to_filename(&m.name, "html");
    body.push_str("<li><a href=\"");
    body.push_str(&html_escape(&filename));
    body.push_str("\"><code>");
    body.push_str(&html_escape(&m.name));
    body.push_str("</code></a>");
    if let Some(doc) = &m.doc {
        if let Some(first_line) = doc.lines().next() {
            body.push_str("<span class=\"module-desc\"> — ");
            body.push_str(&html_escape(first_line));
            body.push_str("</span>");
        }
    }
    body.push_str("</li>\n");
}

#[cfg(test)]
mod tests {
    use super::super::extract::extract_docs;
    use super::*;

    #[test]
    fn render_contains_function() {
        let source = r"/// Adds numbers.
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}
";
        let result = hew_parser::parse(source);
        let module = extract_docs(&result.program, "math");
        let html = render_module(&module);
        assert!(html.contains("fn add"));
        assert!(html.contains("Adds numbers."));
    }

    #[test]
    fn render_index_links() {
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
        let html = render_index(&[module]);
        assert!(html.contains("std.math.html"));
        assert!(html.contains("std::math"));
        assert!(html.contains("Math utilities."));
    }

    #[test]
    fn render_enum_variants_with_docs() {
        let source = r"pub enum E {
    /// First variant.
    A;
    /// Second variant with payload.
    B(int);
}
";
        let result = hew_parser::parse(source);
        let module = extract_docs(&result.program, "test");
        let html = render_module(&module);
        assert!(html.contains("Variants"));
        assert!(html.contains("First variant."));
        assert!(html.contains("Second variant with payload."));
        assert!(html.contains("B(int)"));
    }

    #[test]
    fn render_trait_method_docs() {
        let source = r"pub trait T {
    /// Do the thing.
    fn do_it();
}
";
        let result = hew_parser::parse(source);
        let module = extract_docs(&result.program, "test");
        let html = render_module(&module);
        assert!(html.contains("Do the thing."));
        assert!(html.contains("do_it"));
        assert!(html.contains("method.T.do_it"));
    }

    #[test]
    fn method_ids_are_namespaced_by_owner() {
        let source = r"pub trait A { fn shared(); }
pub trait B { fn shared(); }
";
        let result = hew_parser::parse(source);
        let module = extract_docs(&result.program, "test");
        let html = render_module(&module);
        assert!(html.contains("method.A.shared"));
        assert!(html.contains("method.B.shared"));
    }

    #[test]
    fn render_const_and_type_alias() {
        let source = r"/// The answer.
pub const ANSWER: i32 = 42;

/// A user id.
pub type UserId = i64;
";
        let result = hew_parser::parse(source);
        let module = extract_docs(&result.program, "test");
        let html = render_module(&module);
        assert!(html.contains("<h2>Constants</h2>"));
        assert!(html.contains("ANSWER"));
        assert!(html.contains("42"));
        assert!(html.contains("<h2>Type aliases</h2>"));
        assert!(html.contains("UserId"));
    }

    #[test]
    fn private_fn_omitted_from_html() {
        let source = "pub fn public() {}\nfn private_helper() {}\n";
        let result = hew_parser::parse(source);
        let module = extract_docs(&result.program, "test");
        let html = render_module(&module);
        assert!(html.contains("public"));
        assert!(!html.contains("private_helper"));
    }

    #[test]
    fn markdown_renders_code_blocks() {
        let md = "Some text.\n\n```\nlet x = 1;\n```\n";
        let html = markdown_to_html(md, 0);
        assert!(html.contains("<code>"));
    }

    // ── Feature 1: Markdown header demotion ──────────────────────────────────

    /// Doc `#` inside an item (h3 context) must not produce <h1>.
    #[test]
    fn doc_heading_demoted_under_item() {
        let source = r"/// # Examples
///
/// Some text.
pub fn foo() {}
";
        let result = hew_parser::parse(source);
        let module = extract_docs(&result.program, "test");
        let html = render_module(&module);
        // # Examples → <h4> (offset 3), never <h1>
        assert!(
            !html.contains("<h1>Examples</h1>"),
            "h1 should not appear inside item doc"
        );
        assert!(
            html.contains("<h4>Examples</h4>"),
            "# Examples inside item doc must become <h4>"
        );
    }

    /// Doc `##` inside an item must become <h5>.
    #[test]
    fn doc_subheading_demoted_under_item() {
        let source = r"/// ## Sub
pub fn bar() {}
";
        let result = hew_parser::parse(source);
        let module = extract_docs(&result.program, "test");
        let html = render_module(&module);
        assert!(
            html.contains("<h5>Sub</h5>"),
            "## Sub inside item doc must become <h5>"
        );
    }

    /// Module-level `#` doc heading must become <h2> (one below the page <h1>).
    #[test]
    fn module_doc_heading_demoted() {
        let source = "//! # Overview\n\npub fn x() {}\n";
        let result = hew_parser::parse(source);
        let module = extract_docs(&result.program, "test");
        let html = render_module(&module);
        // Module docs get offset 1: # → <h2>
        assert!(
            !html.contains("<h1>Overview</h1>"),
            "module-doc # must not produce h1"
        );
        assert!(
            html.contains("<h2>Overview</h2>"),
            "module-doc # must become <h2>"
        );
    }

    // ── Feature 2: Cross-link types in signatures ─────────────────────────────

    /// Types defined in the same module are hyperlinked in function signatures.
    #[test]
    fn signature_types_are_linked() {
        let source = "pub type Value {}\npub fn stringify(val: Value) -> String;\n";
        let result = hew_parser::parse(source);
        let module = extract_docs(&result.program, "test");
        let html = render_module(&module);
        assert!(
            html.contains("href=\"#type.Value\""),
            "Value in signature should link to #type.Value"
        );
    }

    /// Types not in the module are not given a (broken) link.
    #[test]
    fn unknown_type_not_linked() {
        let source = "pub fn foo(x: i32) -> i32 {}\n";
        let result = hew_parser::parse(source);
        let module = extract_docs(&result.program, "test");
        let html = render_module(&module);
        // i32 is a built-in type coloured TY, but it's not in the module index
        assert!(
            !html.contains("href=\"#type.i32\""),
            "built-in i32 must not get a broken link"
        );
    }

    /// Types in trait method signatures are also linked.
    #[test]
    fn trait_method_signature_types_linked() {
        let source = "pub type Foo {}\ntrait FooMethods { fn get(v: Foo) -> Foo; }\n";
        let result = hew_parser::parse(source);
        let module = extract_docs(&result.program, "test");
        let html = render_module(&module);
        // Both the param and return type should be linked
        assert!(
            html.contains("href=\"#type.Foo\""),
            "Foo in trait method signature must be linked"
        );
    }

    // ── Feature 3: Methods on opaque types ────────────────────────────────────

    /// A `Methods` block appears on a type's entry when a same-module trait
    /// takes that type as the first parameter of its methods.
    #[test]
    fn opaque_type_surfaces_trait_methods() {
        let source = r"/// An opaque value.
pub type Value {}
/// Methods on Value.
trait ValueMethods {
    /// Serialize to JSON.
    fn stringify(val: Value) -> String;
    /// Extract integer.
    fn get_int(val: Value) -> int;
}
";
        let result = hew_parser::parse(source);
        let module = extract_docs(&result.program, "test");
        let html = render_module(&module);

        let type_pos = html
            .find("id=\"type.Value\"")
            .expect("type.Value anchor missing");
        let type_section = &html[type_pos..];
        assert!(
            type_section.contains("<h4>Methods</h4>"),
            "Methods heading must appear on the Value type entry"
        );
        assert!(
            type_section.contains("stringify"),
            "stringify must be listed"
        );
        assert!(type_section.contains("get_int"), "get_int must be listed");
        assert!(
            type_section.contains("href=\"#trait.ValueMethods\""),
            "back-link to ValueMethods trait must be present"
        );
    }

    /// A trait whose methods do NOT take the type as first param does not
    /// produce a Methods block on that type.
    #[test]
    fn unrelated_trait_not_surfaced_on_type() {
        let source = r"pub type Foo {}
trait Other {
    fn helper(x: i32) -> i32;
}
";
        let result = hew_parser::parse(source);
        let module = extract_docs(&result.program, "test");
        let html = render_module(&module);

        let type_pos = html
            .find("id=\"type.Foo\"")
            .expect("type.Foo anchor missing");
        // The next top-level </div> closes the type item.
        let end = html[type_pos..]
            .find("</div>")
            .unwrap_or(html.len() - type_pos);
        let type_div = &html[type_pos..type_pos + end];
        assert!(
            !type_div.contains("<h4>Methods</h4>"),
            "unrelated trait must not produce a Methods block on Foo"
        );
    }

    /// `first_param_type` correctly extracts the type name from various signatures.
    #[test]
    fn first_param_type_extraction() {
        assert_eq!(
            first_param_type("fn foo(val: Value) -> String"),
            Some("Value")
        );
        assert_eq!(
            first_param_type("fn bar(x: i32, y: i32) -> i32"),
            Some("i32")
        );
        assert_eq!(first_param_type("fn baz()"), None);
        assert_eq!(
            first_param_type("fn qux(obj: Result) -> bool"),
            Some("Result")
        );
    }
}
