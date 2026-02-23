//! Render [`DocModule`] items to HTML fragments.

use pulldown_cmark::{CodeBlockKind, Event, Options, Parser, Tag, TagEnd};

use super::extract::{DocActor, DocFunction, DocModule, DocTrait, DocType};
use super::highlight;
use super::template::{highlight_signature, html_escape};

/// Convert a Markdown doc comment to an HTML fragment.
///
/// Code blocks with language `hew` (or no language) are highlighted using
/// the lexer-based Shiki-compatible highlighter.
fn markdown_to_html(md: &str) -> String {
    let opts =
        Options::ENABLE_TABLES | Options::ENABLE_STRIKETHROUGH | Options::ENABLE_HEADING_ATTRIBUTES;
    let parser = Parser::new_ext(md, opts);

    let mut html = String::new();
    let mut in_code_block = false;
    let mut code_lang = String::new();
    let mut code_buf = String::new();

    for event in parser {
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
                // Apply Hew highlighting for hew blocks or untagged blocks
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
                    // Let pulldown-cmark handle non-code events
                    let single = std::iter::once(event);
                    pulldown_cmark::html::push_html(&mut html, single);
                }
            }
        }
    }

    html
}

/// Render a function item.
fn render_function(f: &DocFunction) -> String {
    let mut out = String::from("<div class=\"item\" id=\"fn.");
    out.push_str(&html_escape(&f.name));
    out.push_str("\">\n");
    out.push_str("<h3>Function <code>");
    out.push_str(&html_escape(&f.name));
    out.push_str("</code></h3>\n");
    out.push_str("<span class=\"sig\">");
    out.push_str(&highlight_signature(&f.signature));
    out.push_str("</span>\n");
    if let Some(doc) = &f.doc {
        out.push_str("<div class=\"doc\">");
        out.push_str(&markdown_to_html(doc));
        out.push_str("</div>\n");
    }
    out.push_str("</div>\n");
    out
}

/// Render a type item (struct or enum).
fn render_type(t: &DocType) -> String {
    let mut out = String::from("<div class=\"item\" id=\"type.");
    out.push_str(&html_escape(&t.name));
    out.push_str("\">\n");
    out.push_str("<h3>");
    out.push_str(if t.kind == "struct" { "Struct" } else { "Enum" });
    out.push_str(" <code>");
    out.push_str(&html_escape(&t.name));
    out.push_str("</code></h3>\n");
    if let Some(doc) = &t.doc {
        out.push_str("<div class=\"doc\">");
        out.push_str(&markdown_to_html(doc));
        out.push_str("</div>\n");
    }
    if !t.fields.is_empty() {
        out.push_str("<h4>Fields</h4>\n<dl class=\"fields\">\n");
        for (name, ty) in &t.fields {
            out.push_str("<dt>");
            out.push_str(&html_escape(name));
            out.push_str(": <span class=\"ty\">");
            out.push_str(&html_escape(ty));
            out.push_str("</span></dt>\n");
        }
        out.push_str("</dl>\n");
    }
    out.push_str("</div>\n");
    out
}

/// Render an actor item.
fn render_actor(a: &DocActor) -> String {
    let mut out = String::from("<div class=\"item\" id=\"actor.");
    out.push_str(&html_escape(&a.name));
    out.push_str("\">\n");
    out.push_str("<h3>Actor <code>");
    out.push_str(&html_escape(&a.name));
    out.push_str("</code></h3>\n");
    if let Some(doc) = &a.doc {
        out.push_str("<div class=\"doc\">");
        out.push_str(&markdown_to_html(doc));
        out.push_str("</div>\n");
    }
    if !a.fields.is_empty() {
        out.push_str("<h4>Fields</h4>\n<dl class=\"fields\">\n");
        for (name, ty) in &a.fields {
            out.push_str("<dt>");
            out.push_str(&html_escape(name));
            out.push_str(": <span class=\"ty\">");
            out.push_str(&html_escape(ty));
            out.push_str("</span></dt>\n");
        }
        out.push_str("</dl>\n");
    }
    if !a.handlers.is_empty() {
        out.push_str("<h4>Handlers</h4>\n");
        for (_name, sig) in &a.handlers {
            out.push_str("<span class=\"sig\">");
            out.push_str(&highlight_signature(sig));
            out.push_str("</span>\n");
        }
    }
    out.push_str("</div>\n");
    out
}

/// Render a trait item.
fn render_trait(t: &DocTrait) -> String {
    let mut out = String::from("<div class=\"item\" id=\"trait.");
    out.push_str(&html_escape(&t.name));
    out.push_str("\">\n");
    out.push_str("<h3>Trait <code>");
    out.push_str(&html_escape(&t.name));
    out.push_str("</code></h3>\n");
    if let Some(doc) = &t.doc {
        out.push_str("<div class=\"doc\">");
        out.push_str(&markdown_to_html(doc));
        out.push_str("</div>\n");
    }
    if !t.methods.is_empty() {
        out.push_str("<h4>Methods</h4>\n");
        for (_name, sig) in &t.methods {
            out.push_str("<span class=\"sig\">");
            out.push_str(&highlight_signature(sig));
            out.push_str("</span>\n");
        }
    }
    out.push_str("</div>\n");
    out
}

/// Render a full module page body (without the outer HTML wrapper).
#[must_use]
pub fn render_module(module: &DocModule) -> String {
    let mut body = String::new();

    body.push_str("<h1>Module <code>");
    body.push_str(&html_escape(&module.name));
    body.push_str("</code></h1>\n");

    if let Some(doc) = &module.doc {
        body.push_str("<div class=\"doc\">");
        body.push_str(&markdown_to_html(doc));
        body.push_str("</div>\n");
    }

    // Table of contents
    let has_items = !module.functions.is_empty()
        || !module.types.is_empty()
        || !module.actors.is_empty()
        || !module.traits.is_empty();
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
        body.push_str("</ul>\n");
    }

    if !module.functions.is_empty() {
        body.push_str("<h2>Functions</h2>\n");
        for f in &module.functions {
            body.push_str(&render_function(f));
        }
    }

    if !module.types.is_empty() {
        body.push_str("<h2>Types</h2>\n");
        for t in &module.types {
            body.push_str(&render_type(t));
        }
    }

    if !module.actors.is_empty() {
        body.push_str("<h2>Actors</h2>\n");
        for a in &module.actors {
            body.push_str(&render_actor(a));
        }
    }

    if !module.traits.is_empty() {
        body.push_str("<h2>Traits</h2>\n");
        for t in &module.traits {
            body.push_str(&render_trait(t));
        }
    }

    body
}

/// Render an index page listing all documented modules.
#[must_use]
pub fn render_index(modules: &[DocModule]) -> String {
    let mut body = String::from("<h1>Hew Documentation</h1>\n");
    body.push_str("<ul class=\"module-list\">\n");
    for m in modules {
        body.push_str("<li><a href=\"");
        body.push_str(&html_escape(&m.name));
        body.push_str(".html\">");
        body.push_str(&html_escape(&m.name));
        body.push_str("</a>");
        if let Some(doc) = &m.doc {
            // Show first line of doc as description
            if let Some(first_line) = doc.lines().next() {
                body.push_str(" — ");
                body.push_str(&html_escape(first_line));
            }
        }
        body.push_str("</li>\n");
    }
    body.push_str("</ul>\n");
    body
}

#[cfg(test)]
mod tests {
    use super::super::extract::extract_docs;
    use super::*;

    #[test]
    fn render_contains_function() {
        let source = r#"/// Adds numbers.
fn add(a: i32, b: i32) -> i32 {
    a + b
}
"#;
        let result = hew_parser::parse(source);
        let module = extract_docs(&result.program, "math");
        let html = render_module(&module);
        assert!(html.contains("fn add"));
        assert!(html.contains("Adds numbers."));
    }

    #[test]
    fn render_index_links() {
        let module = DocModule {
            name: "math".to_string(),
            doc: Some("Math utilities.".to_string()),
            functions: vec![],
            types: vec![],
            actors: vec![],
            traits: vec![],
        };
        let html = render_index(&[module]);
        assert!(html.contains("math.html"));
        assert!(html.contains("Math utilities."));
    }

    #[test]
    fn markdown_renders_code_blocks() {
        let md = "Some text.\n\n```\nlet x = 1;\n```\n";
        let html = markdown_to_html(md);
        assert!(html.contains("<code>"));
    }
}
