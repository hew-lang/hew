//! Minimal HTML template with embedded CSS for generated documentation.

/// Hew logo SVG (inline, matches hew.sh).
const LOGO_SVG: &str = r##"<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 200 200" class="logo-icon"><defs><linearGradient id="b" x1="0" x2="0" y1="1" y2="0"><stop offset="0%" stop-color="#1e3a8a"/><stop offset="100%" stop-color="#38bdf8"/></linearGradient><linearGradient id="a" x1="0" x2="0" y1="1" y2="0"><stop offset="0%" stop-color="#1e3a8a" stop-opacity=".6"/><stop offset="100%" stop-color="#38bdf8" stop-opacity=".85"/></linearGradient></defs><path fill="url(#a)" d="M95 78 30 48l28 60z"/><path fill="url(#b)" d="M95 85 50 62l18 50z"/><path fill="url(#a)" d="m105 78 65-30-28 60z"/><path fill="url(#b)" d="m105 85 45-23-18 50z"/><path fill="#fff" d="m52 72 16-7-3 13-17 4zm96 0-16-7 3 13 17 4z" opacity=".6"/><path fill="url(#b)" d="m100 48 18 67-18 57-18-57z"/><path fill="#fff" d="m100 68 10 42-10 45-10-45z" opacity=".8"/><path fill="#38bdf8" d="m100 15 8 37-8-10-8 10z"/><path fill="#1e293b" d="m95 62 5 13 5-13z"/><path fill="url(#a)" d="m90 148-18 37 28-13z"/><path fill="url(#a)" d="m110 148 18 37-28-13z"/><circle cx="93" cy="55" r="2.5" fill="#0f172a"/><circle cx="107" cy="55" r="2.5" fill="#0f172a"/></svg>"##;

/// Embedded CSS styles for the documentation pages.
///
/// The colour palette matches the Shiki `dark-plus` theme as configured on the
/// Hew website (`hew.sh`), with the same design-system overrides applied in the
/// website's `CodeBlock.astro` component.
const CSS: &str = r#"
@import url('https://fonts.googleapis.com/css2?family=Gentium+Plus:wght@700&family=Inter:wght@300;400;500;600;700&family=JetBrains+Mono:wght@400;500;700&display=swap');
:root {
    --bg: #0d1117;
    --fg: #e2e8f0;
    --code-bg: #111827;
    --border: #30363d;
    --accent: #60a5fa;
    --accent-light: #1e293b;
    --kw: #22d3ee;
    --ty: #2dd4bf;
    --fn-color: #facc15;
    --str: #4ade80;
    --num: #fb923c;
    --comment: #8b949e;
    --op: #94a3b8;
    --muted: #6b7280;
}
* { margin: 0; padding: 0; box-sizing: border-box; }
body {
    font-family: Inter, 'Inter Fallback', system-ui, sans-serif;
    line-height: 1.6;
    color: var(--fg);
    background: var(--bg);
    max-width: 56rem;
    margin: 0 auto;
    padding: 0 1.5rem 2rem;
}
/* Site header */
.site-header {
    display: flex;
    align-items: center;
    justify-content: space-between;
    padding: 1rem 0;
    margin-bottom: 2rem;
    border-bottom: 1px solid var(--border);
}
.site-brand {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    text-decoration: none;
    color: var(--accent);
}
.site-brand:hover { text-decoration: none; }
.logo-icon { width: 1.75rem; height: 1.75rem; }
.site-brand-name {
    font-family: 'Gentium Plus', Georgia, serif;
    font-weight: 700;
    font-size: 1.25rem;
    letter-spacing: -0.01em;
}
.site-brand-tag {
    font-size: 0.8rem;
    color: var(--muted);
    margin-left: 0.25rem;
    font-weight: 400;
}
.site-nav {
    display: flex;
    gap: 1.5rem;
    align-items: center;
}
.site-nav a {
    color: var(--muted);
    font-size: 0.875rem;
    text-decoration: none;
    transition: color 0.15s;
}
.site-nav a:hover { color: var(--fg); text-decoration: none; }
/* Content */
h1 { font-size: 2rem; margin-bottom: 0.5rem; }
h2 { font-size: 1.5rem; margin: 2rem 0 0.75rem; border-bottom: 1px solid var(--border); padding-bottom: 0.25rem; }
h3 { font-size: 1.15rem; margin: 1.25rem 0 0.5rem; }
a { color: var(--accent); text-decoration: none; }
a:hover { text-decoration: underline; }
code, pre {
    font-family: "JetBrains Mono", "Fira Code", "Cascadia Code", monospace;
    font-size: 0.9em;
}
code { background: var(--code-bg); padding: 0.1em 0.35em; border-radius: 3px; }
pre { background: var(--code-bg); padding: 1em; border-radius: 6px; overflow-x: auto; margin: 0.75rem 0; }
pre code { background: none; padding: 0; }
pre.shiki { border: 1px solid var(--border); }
pre.shiki code { counter-reset: line; display: flex; flex-direction: column; }
pre.shiki .line { display: block; }
pre.shiki .line::before {
    counter-increment: line;
    content: counter(line);
    display: inline-block;
    width: 2.5em;
    margin-right: 1em;
    text-align: right;
    color: #4b5563;
    user-select: none;
    font-size: 0.8em;
}
.sig { font-weight: 600; font-family: monospace; font-size: 1rem; background: var(--accent-light); padding: 0.5em 0.75em; border-radius: 6px; border-left: 3px solid var(--accent); margin-bottom: 0.5rem; display: block; }
.doc { margin: 0.5rem 0 1.5rem 0; }
.fields { margin: 0.5rem 0 0.75rem 1rem; }
.fields dt { font-family: monospace; font-weight: 600; }
.fields dd { margin-left: 1rem; margin-bottom: 0.25rem; color: var(--comment); }
.item { margin-bottom: 2rem; }
/* Module index */
.module-list { list-style: none; }
.module-list li { margin: 0.4rem 0; display: flex; align-items: baseline; gap: 0.5rem; }
.module-list li a code { font-size: 0.95em; font-weight: 500; }
.module-list li .module-desc { color: var(--comment); font-size: 0.9em; }
.section-heading { font-size: 1.1rem; font-weight: 600; color: var(--muted); text-transform: capitalize; margin: 1.75rem 0 0.5rem; padding-bottom: 0.25rem; border-bottom: 1px solid var(--border); }
nav.breadcrumb { margin-bottom: 1.5rem; font-size: 0.95em; }
nav.breadcrumb a + a::before { content: " · "; color: var(--comment); }
footer { margin-top: 3rem; padding-top: 1rem; border-top: 1px solid var(--border); font-size: 0.85em; color: var(--comment); display: flex; justify-content: space-between; align-items: center; }
footer a { color: var(--comment); }
footer a:hover { color: var(--accent); }
"#;

/// Wrap HTML body content in a full page with the documentation stylesheet.
#[must_use]
pub fn wrap_page(title: &str, body: &str, breadcrumb: Option<&str>) -> String {
    let nav = breadcrumb.map_or(String::new(), |bc| {
        format!("<nav class=\"breadcrumb\">{bc}</nav>\n")
    });
    let header = format!(
        r#"<header class="site-header">
<a href="https://hew.sh" class="site-brand">{LOGO_SVG}<span class="site-brand-name">Hew</span><span class="site-brand-tag">Standard Library</span></a>
<nav class="site-nav">
<a href="index.html">API Reference</a>
<a href="https://hew.sh/learn">Learn</a>
<a href="https://hew.sh/playground">Playground</a>
<a href="https://hew.sh/docs">Docs</a>
<a href="https://github.com/hew-lang/hew">GitHub ↗</a>
</nav>
</header>"#
    );
    format!(
        r#"<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>{title} — Hew Standard Library</title>
<style>{CSS}</style>
</head>
<body>
{header}
{nav}{body}
<footer><span>Generated by <code>hew doc</code></span><a href="https://hew.sh">hew.sh</a></footer>
</body>
</html>
"#
    )
}

/// Apply syntax highlighting to a code string using the hew-lexer.
///
/// Wraps known Hew tokens in coloured `<span>` elements matching the website's
/// Shiki theme.
#[must_use]
pub fn highlight_signature(sig: &str) -> String {
    super::highlight::highlight_signature(sig)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn wrap_page_contains_title() {
        let html = wrap_page("TestMod", "<p>hello</p>", None);
        assert!(html.contains("<title>TestMod — Hew Standard Library</title>"));
        assert!(html.contains("<p>hello</p>"));
    }

    #[test]
    fn highlight_keywords() {
        let out = highlight_signature("pub fn add(a: i32)");
        assert!(out.contains("pub</span>"));
        assert!(out.contains("fn</span>"));
    }
}
