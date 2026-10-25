//! Minimal HTML template with embedded CSS for generated documentation.

use std::fmt::Write;

use crate::util::html_escape;

use super::config::DocSiteConfig;

/// Embedded CSS styles for the documentation pages.
///
/// The colour palette follows the Shiki `dark-plus` syntax theme.
const CSS: &str = r#"
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
    --muted: #9ca3af;
}
* { margin: 0; padding: 0; box-sizing: border-box; }
body {
    font-family: system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
    line-height: 1.6;
    color: var(--fg);
    background: var(--bg);
    margin: 0;
}
.skip-link { position: absolute; left: -9999px; top: 0; }
.skip-link:focus { left: 1rem; top: 0.5rem; z-index: 10; background: var(--fg); color: var(--bg); padding: 0.5rem 0.75rem; border-radius: 4px; }
/* Site header */
.site-header {
    position: sticky;
    top: 0;
    z-index: 20;
    display: flex;
    align-items: center;
    gap: 1.25rem;
    min-height: 4rem;
    padding: 0.65rem max(1.25rem, calc((100vw - 92rem) / 2));
    background: color-mix(in srgb, var(--bg) 94%, transparent);
    backdrop-filter: blur(12px);
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
.site-brand-name {
    font-family: Georgia, "Times New Roman", serif;
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
    gap: 1.1rem;
    align-items: center;
    margin-left: auto;
}
.site-nav a {
    color: var(--muted);
    font-size: 0.875rem;
    text-decoration: none;
    transition: color 0.15s;
}
.site-nav a:hover { color: var(--fg); text-decoration: none; }
.site-actions { display: flex; align-items: center; gap: 0.5rem; }
.site-actions:empty { display: none; }
.sidebar-toggle, .sidebar-close {
    border: 1px solid var(--border);
    border-radius: 5px;
    padding: 0.42rem 0.65rem;
    color: var(--fg);
    background: var(--code-bg);
    font: inherit;
    cursor: pointer;
}
.sidebar-toggle { display: none; }
.sidebar-close { display: none; }
/* Documentation shell */
.docs-shell {
    display: grid;
    grid-template-columns: 17rem minmax(0, 56rem);
    gap: clamp(2rem, 4vw, 4rem);
    width: min(100%, 92rem);
    margin: 0 auto;
    padding: 1.5rem;
}
.docs-reader { min-width: 0; }
.docs-sidebar {
    position: sticky;
    top: 5rem;
    align-self: start;
    max-height: calc(100vh - 6rem);
    overflow-y: auto;
    padding: 0 1rem 2rem 0;
    scrollbar-width: thin;
}
.sidebar-header, .mobile-site-nav { display: none; }
.docs-navigation ul, .docs-outline ul { list-style: none; }
.docs-navigation a, .docs-outline a {
    display: block;
    border-left: 2px solid transparent;
    padding: 0.22rem 0.5rem;
    color: var(--muted);
    font-size: 0.86rem;
    line-height: 1.35;
    text-decoration: none;
}
.docs-navigation a:hover, .docs-outline a:hover { color: var(--fg); text-decoration: none; }
.docs-navigation a[aria-current="page"], .docs-outline a[aria-current="location"] {
    border-left-color: var(--accent);
    color: var(--fg);
    background: color-mix(in srgb, var(--accent-light) 55%, transparent);
}
.docs-home { margin-bottom: 1.1rem; font-weight: 650; }
.sidebar-section + .sidebar-section { margin-top: 1.25rem; }
.sidebar-title, .outline-title {
    margin: 0 0 0.35rem;
    color: var(--fg);
    font-size: 0.78rem;
    font-weight: 700;
    letter-spacing: 0.05em;
    text-transform: uppercase;
}
.sidebar-module-group { margin: 0.2rem 0; }
.sidebar-module-group summary {
    padding: 0.25rem 0.5rem;
    color: var(--muted);
    font-size: 0.86rem;
    cursor: pointer;
}
.sidebar-module-group[open] > summary { color: var(--fg); }
.sidebar-module-group ul { margin: 0.1rem 0 0.45rem 0.45rem; }
.docs-outline { margin-top: 1.5rem; padding-top: 1rem; border-top: 1px solid var(--border); }
.docs-outline[hidden] { display: none; }
.docs-backdrop { display: none; }
/* Content */
h1 { font-size: 2rem; margin-bottom: 0.5rem; overflow-wrap: anywhere; }
h2 { font-size: 1.5rem; margin: 2rem 0 0.75rem; border-bottom: 1px solid var(--border); padding-bottom: 0.25rem; }
h3 { font-size: 1.15rem; margin: 1.25rem 0 0.5rem; }
h1 code, h2 code, h3 code, h4 code, .module-list code {
    overflow-wrap: anywhere;
    word-break: break-word;
}
p, .doc ul, .doc ol, .doc blockquote, .doc table { margin: 0.75rem 0; }
.doc ul, .doc ol { padding-left: 1.5rem; }
.doc blockquote { border-left: 3px solid var(--border); padding-left: 1rem; color: var(--comment); }
.doc table { width: 100%; border-collapse: collapse; display: block; overflow-x: auto; }
.doc th, .doc td { border: 1px solid var(--border); padding: 0.45rem 0.65rem; text-align: left; }
.doc th { background: var(--code-bg); }
a { color: var(--accent); text-decoration: none; }
a:hover { text-decoration: underline; }
code, pre {
    font-family: ui-monospace, "Cascadia Code", "Fira Code", monospace;
    font-size: 0.9em;
}
code { background: var(--code-bg); padding: 0.1em 0.35em; border-radius: 3px; overflow-wrap: anywhere; word-break: break-word; }
pre { background: var(--code-bg); padding: 1em; border-radius: 6px; overflow-x: auto; margin: 0.75rem 0; }
pre code { background: none; padding: 0; overflow-wrap: normal; word-break: normal; }
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
.sig { font-weight: 600; font-family: monospace; font-size: 1rem; background: var(--accent-light); padding: 0.5em 0.75em; border-radius: 6px; border-left: 3px solid var(--accent); margin-top: 0; margin-bottom: 0.5rem; display: block; max-width: 100%; overflow-x: auto; overflow-wrap: anywhere; word-break: break-word; }
.doc { margin: 0.5rem 0 1.5rem 0; }
.fields { margin: 0.5rem 0 0.75rem 1rem; }
.fields dt { font-family: monospace; font-weight: 600; }
.fields dd { margin-left: 1rem; margin-bottom: 0.25rem; color: var(--comment); }
.item { margin-bottom: 2rem; }
/* Module index */
.module-list { list-style: none; margin: 0.25rem 0 1.25rem; }
.module-list li {
    display: grid;
    grid-template-columns: minmax(11rem, 15rem) minmax(0, 1fr);
    align-items: baseline;
    column-gap: 1.25rem;
    padding: 0.5rem 0;
}
.module-list li + li { border-top: 1px solid color-mix(in srgb, var(--border) 65%, transparent); }
.module-list li > a { font-weight: 600; line-height: 1.35; }
.module-list li a code { font-size: 0.95em; font-weight: 500; }
.module-list li .module-desc { color: var(--comment); font-size: 0.9em; line-height: 1.5; }
.guide-group { margin-top: 1.5rem; }
.guide-group h3 { margin-bottom: 0.25rem; color: var(--fg); }
.index-nav { display: flex; gap: 1rem; margin: 0.75rem 0 1.25rem; font-size: 0.9rem; }
.index-nav a + a::before { content: "·"; color: var(--comment); margin-right: 1rem; }
.section-heading { font-size: 1.1rem; font-weight: 600; color: var(--muted); text-transform: capitalize; margin: 1.75rem 0 0.5rem; padding-bottom: 0.25rem; border-bottom: 1px solid var(--border); }
.search-label { display: block; font-weight: 600; margin-top: 1.5rem; }
.module-search { width: 100%; margin: 0.4rem 0 0.25rem; padding: 0.65rem 0.75rem; border: 1px solid var(--border); border-radius: 6px; background: var(--code-bg); color: var(--fg); font: inherit; }
.module-search:focus { outline: 2px solid var(--accent); outline-offset: 2px; }
.search-status { min-height: 1.5em; color: var(--comment); font-size: 0.9rem; }
.module-entry[hidden], .doc-section[hidden], .section-heading[hidden] { display: none; }
nav.breadcrumb { margin-bottom: 1.5rem; font-size: 0.95em; }
nav.breadcrumb a + a::before { content: " · "; color: var(--comment); }
footer { margin-top: 3rem; padding: 1rem 0 2rem; border-top: 1px solid var(--border); font-size: 0.85em; color: var(--comment); display: flex; justify-content: space-between; align-items: center; }
footer a { color: var(--comment); }
footer a:hover { color: var(--accent); }
@media (max-width: 64rem) {
    .site-header { padding: 0.65rem 1rem; }
    .site-nav { display: none; }
    .site-actions { margin-left: auto; }
    .sidebar-toggle { display: inline-flex; }
    .docs-shell { display: block; padding: 1.25rem 1rem; }
    .docs-sidebar {
        position: fixed;
        inset: 0 auto 0 0;
        z-index: 40;
        width: min(19rem, calc(100vw - 3rem));
        max-height: none;
        padding: 1rem;
        background: var(--bg);
        border-right: 1px solid var(--border);
        transform: translateX(-105%);
        visibility: hidden;
        transition: transform 0.18s ease, visibility 0s linear 0.18s;
    }
    body.sidebar-open { overflow: hidden; }
    body.sidebar-open .docs-sidebar {
        transform: translateX(0);
        visibility: visible;
        transition-delay: 0s;
    }
    .docs-backdrop {
        position: fixed;
        inset: 0;
        z-index: 30;
        background: rgb(0 0 0 / 0.58);
    }
    body.sidebar-open .docs-backdrop { display: block; }
    .sidebar-header {
        display: flex;
        align-items: center;
        justify-content: space-between;
        margin-bottom: 0.75rem;
        padding-bottom: 0.75rem;
        border-bottom: 1px solid var(--border);
        font-weight: 700;
    }
    .sidebar-close { display: inline-flex; }
    .mobile-site-nav {
        display: flex;
        flex-wrap: wrap;
        gap: 0.35rem 0.8rem;
        margin-bottom: 1.25rem;
    }
    .mobile-site-nav a { color: var(--muted); font-size: 0.84rem; }
}
@media (max-width: 44rem) {
    .site-header { gap: 0.55rem; min-height: 3.5rem; padding: 0.5rem 1rem; }
    .site-brand { gap: 0.3rem; }
    .site-brand-name { font-size: 1.1rem; }
    .site-brand-tag { font-size: 0.72rem; }
    .sidebar-toggle, .sidebar-close { padding: 0.36rem 0.55rem; font-size: 0.82rem; }
    .site-brand-tag { display: inline; }
    .docs-shell { padding-top: 1rem; }
    .module-list li { grid-template-columns: minmax(0, 1fr); row-gap: 0.2rem; padding: 0.65rem 0; }
    .module-desc { display: block; }
}
"#;

const SHELL_SCRIPT: &str = r"
const menuButton = document.querySelector('.sidebar-toggle');
const closeButton = document.querySelector('.sidebar-close');
const backdrop = document.querySelector('.docs-backdrop');
const sidebar = document.getElementById('docs-sidebar');
const mobileSidebar = matchMedia('(max-width: 64rem)');
let sidebarReturnFocus = null;
const setSidebarAvailability = (hidden) => {
  if (!sidebar) return;
  sidebar.inert = hidden;
  sidebar.setAttribute('aria-hidden', hidden ? 'true' : 'false');
};
const syncSidebar = () => {
  if (!mobileSidebar.matches) {
    document.body.classList.remove('sidebar-open');
    menuButton?.setAttribute('aria-expanded', 'false');
    setSidebarAvailability(false);
    sidebarReturnFocus = null;
    return;
  }
  setSidebarAvailability(!document.body.classList.contains('sidebar-open'));
};
const closeSidebar = () => {
  if (!document.body.classList.contains('sidebar-open')) return;
  document.body.classList.remove('sidebar-open');
  menuButton?.setAttribute('aria-expanded', 'false');
  const returnFocus = sidebarReturnFocus;
  sidebarReturnFocus = null;
  returnFocus?.focus();
  setSidebarAvailability(true);
};
const openSidebar = () => {
  if (!mobileSidebar.matches) return;
  sidebarReturnFocus = document.activeElement;
  setSidebarAvailability(false);
  document.body.classList.add('sidebar-open');
  menuButton?.setAttribute('aria-expanded', 'true');
  closeButton?.focus();
};
menuButton?.addEventListener('click', openSidebar);
closeButton?.addEventListener('click', closeSidebar);
backdrop?.addEventListener('click', closeSidebar);
sidebar?.addEventListener('click', (event) => {
  if (event.target.closest('a') && mobileSidebar.matches) closeSidebar();
});
document.addEventListener('keydown', (event) => {
  const open = mobileSidebar.matches && document.body.classList.contains('sidebar-open');
  if (event.key === 'Escape' && open) {
    event.preventDefault();
    closeSidebar();
    return;
  }
  if (event.key === 'Tab' && open && sidebar) {
    const focusable = [...sidebar.querySelectorAll('a[href], button:not([disabled]), summary')]
      .filter((element) => element.getClientRects().length > 0);
    if (!focusable.length) return;
    const first = focusable[0];
    const last = focusable[focusable.length - 1];
    if (event.shiftKey && (document.activeElement === first || !sidebar.contains(document.activeElement))) {
      event.preventDefault();
      last.focus();
    } else if (!event.shiftKey && (document.activeElement === last || !sidebar.contains(document.activeElement))) {
      event.preventDefault();
      first.focus();
    }
  }
});
mobileSidebar.addEventListener?.('change', syncSidebar);
syncSidebar();

const outline = document.querySelector('.docs-outline');
const outlineList = document.getElementById('docs-outline-list');
const headings = [...document.querySelectorAll('#main-content h2')];
if (outline && outlineList && headings.length > 1) {
  const usedIds = new Set();
  for (const heading of headings) {
    let id = heading.id || heading.textContent.trim().toLowerCase()
      .replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '') || 'section';
    const base = id;
    let suffix = 2;
    while (usedIds.has(id) || (document.getElementById(id) && document.getElementById(id) !== heading)) {
      id = base + '-' + suffix;
      suffix += 1;
    }
    usedIds.add(id);
    heading.id = id;
    const item = document.createElement('li');
    const link = document.createElement('a');
    link.href = '#' + id;
    link.textContent = heading.textContent.trim();
    item.append(link);
    outlineList.append(item);
  }
  outline.hidden = false;
  const links = [...outlineList.querySelectorAll('a')];
  let scheduled = false;
  const updateOutline = () => {
    scheduled = false;
    let active = 0;
    for (let index = 0; index < headings.length; index += 1) {
      if (headings[index].getBoundingClientRect().top <= 130) active = index;
    }
    links.forEach((link, index) => {
      if (index === active) link.setAttribute('aria-current', 'location');
      else link.removeAttribute('aria-current');
    });
  };
  addEventListener('scroll', () => {
    if (!scheduled) {
      scheduled = true;
      requestAnimationFrame(updateOutline);
    }
  }, { passive: true });
  updateOutline();
}
";

/// Wrap HTML body content with the full documentation shell and sidebar.
#[must_use]
pub fn wrap_page_with_sidebar(
    config: &DocSiteConfig,
    title: &str,
    body: &str,
    breadcrumb: Option<&str>,
    sidebar: &str,
) -> String {
    let nav = breadcrumb.map_or(String::new(), |bc| {
        format!("<nav class=\"breadcrumb\" aria-label=\"Breadcrumb\">{bc}</nav>\n")
    });
    let reference_current = if breadcrumb.is_none() {
        " aria-current=\"page\""
    } else {
        ""
    };
    let header = render_header(config, reference_current);
    let mobile_links = render_mobile_links(config);
    let footer = render_footer(config);
    let title = html_escape(title);
    let site_title = html_escape(&config.site_title);
    let sidebar_brand = html_escape(&config.brand);
    format!(
        r##"<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>{title} — {site_title}</title>
<style>{CSS}</style>
</head>
<body>
<a class="skip-link" href="#main-content">Skip to content</a>
{header}
<button class="docs-backdrop" type="button" tabindex="-1" aria-label="Close navigation"></button>
<div class="docs-shell">
<aside class="docs-sidebar" id="docs-sidebar" aria-label="Documentation navigation">
<div class="sidebar-header"><span>{sidebar_brand} documentation</span><button class="sidebar-close" type="button">Close</button></div>
{mobile_links}
{sidebar}
<nav class="docs-outline" aria-label="On this page" hidden>
<p class="outline-title">On this page</p>
<ul id="docs-outline-list"></ul>
</nav>
</aside>
<div class="docs-reader">
{nav}<main id="main-content">{body}</main>
<footer>{footer}</footer>
</div>
</div>
<script>{SHELL_SCRIPT}
const search = document.getElementById('module-search');
if (search) {{
  const entries = [...document.querySelectorAll('.module-entry')];
  const status = document.getElementById('search-status');
  const update = () => {{
    const query = search.value.trim().toLowerCase();
    let visible = 0;
    for (const entry of entries) {{
      const match = !query || entry.dataset.search.toLowerCase().includes(query);
      entry.hidden = !match;
      if (match) visible += 1;
    }}
    for (const section of document.querySelectorAll('.doc-section')) {{
      section.hidden = !section.querySelector('.module-entry:not([hidden])');
    }}
    for (const heading of document.querySelectorAll('.section-heading')) {{
      const list = heading.nextElementSibling;
      heading.hidden = !list || !list.querySelector('.module-entry:not([hidden])');
    }}
    status.textContent = query ? `${{visible}} result${{visible === 1 ? '' : 's'}}` : '';
  }};
  search.addEventListener('input', update);
}}
</script>
</body>
</html>
"##
    )
}

fn render_header(config: &DocSiteConfig, reference_current: &str) -> String {
    let mut header = format!(
        "<header class=\"site-header\">\n<a href=\"index.html\" class=\"site-brand\"><span class=\"site-brand-name\">{}</span>",
        html_escape(&config.brand)
    );
    if !config.brand_tag.is_empty() {
        let _ = write!(
            header,
            "<span class=\"site-brand-tag\">{}</span>",
            html_escape(&config.expand_version(&config.brand_tag))
        );
    }
    header.push_str("</a>\n<nav class=\"site-nav\" aria-label=\"Documentation\">\n");
    let _ = writeln!(
        header,
        "<a href=\"index.html\"{reference_current}>Reference</a>"
    );
    for link in &config.nav_links {
        let _ = writeln!(
            header,
            "<a href=\"{}\">{}</a>",
            html_escape(&link.url),
            html_escape(&link.label)
        );
    }
    header.push_str(
        "</nav>\n<div class=\"site-actions\" data-docs-actions></div>\n<button class=\"sidebar-toggle\" type=\"button\" aria-controls=\"docs-sidebar\" aria-expanded=\"false\">Menu</button>\n</header>",
    );
    header
}

fn render_mobile_links(config: &DocSiteConfig) -> String {
    let mut links = String::from("<nav class=\"mobile-site-nav\" aria-label=\"Project links\">\n");
    for link in &config.nav_links {
        let _ = writeln!(
            links,
            "<a href=\"{}\">{}</a>",
            html_escape(&link.url),
            html_escape(&link.label)
        );
    }
    links.push_str("</nav>");
    links
}

fn render_footer(config: &DocSiteConfig) -> String {
    let mut footer = format!(
        "<span>{}</span>",
        html_escape(&config.expand_version(&config.footer_text))
    );
    if let (Some(label), Some(url)) = (&config.footer_link_label, &config.footer_link_url) {
        let _ = write!(
            footer,
            "<a href=\"{}\">{}</a>",
            html_escape(url),
            html_escape(label)
        );
    }
    footer
}

/// Render a root-level static-hosting not-found document.
#[must_use]
pub fn not_found_page(config: &DocSiteConfig) -> String {
    format!(
        r#"<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Not found — {}</title>
<style>body{{font-family:system-ui,sans-serif;max-width:42rem;margin:5rem auto;padding:0 1rem;background:#0d1117;color:#e2e8f0}}a{{color:#60a5fa}}</style>
</head>
<body><h1>Page not found</h1><p>The requested documentation page does not exist.</p><p><a href="/">Return to the documentation index</a></p></body>
</html>
"#,
        html_escape(&config.site_title)
    )
}

/// Apply syntax highlighting to a code string using the hew-lexer.
///
/// Wraps known Hew tokens in coloured `<span>` elements using the documentation
/// theme.
#[must_use]
pub fn highlight_signature(sig: &str) -> String {
    super::highlight::highlight_signature(sig)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn wrap_page_contains_title() {
        let config = DocSiteConfig::default();
        let html = wrap_page_with_sidebar(&config, "TestMod", "<p>hello</p>", None, "");
        assert!(html.contains("<title>TestMod — Documentation</title>"));
        assert!(html.contains("<main id=\"main-content\">"));
        assert!(html.contains("<p>hello</p>"));
        assert!(!html.contains("fonts.googleapis.com"));
        assert!(html.contains("href=\"index.html\" aria-current=\"page\">Reference"));
    }

    #[test]
    fn child_page_does_not_mark_reference_as_current() {
        let config = DocSiteConfig::default();
        let breadcrumb = "<a href=\"index.html\">Index</a><a aria-current=\"page\">std.example</a>";
        let html =
            wrap_page_with_sidebar(&config, "std.example", "<p>hello</p>", Some(breadcrumb), "");
        assert!(html.contains("<a href=\"index.html\">Reference</a>"));
        assert!(!html.contains("href=\"index.html\" aria-current=\"page\">Reference"));
        assert!(html.contains("<a aria-current=\"page\">std.example</a>"));
    }

    #[test]
    fn site_links_come_from_configuration() {
        let config = DocSiteConfig {
            site_title: "Example API".to_string(),
            brand: "Example".to_string(),
            nav_links: vec![super::super::config::NavLink {
                label: "Project".to_string(),
                url: "https://example.com".to_string(),
            }],
            ..DocSiteConfig::default()
        };
        let html = wrap_page_with_sidebar(&config, "Index", "", None, "");
        assert!(html.contains("<title>Index — Example API</title>"));
        assert!(html.contains("https://example.com"));
        assert!(!html.contains("hew.sh"));
    }

    #[test]
    fn mobile_sidebar_is_hidden_and_keyboard_contained() {
        let html = wrap_page_with_sidebar(&DocSiteConfig::default(), "Index", "", None, "");
        assert!(html.contains("visibility: hidden;"));
        assert!(html.contains("sidebar.inert = hidden;"));
        assert!(html.contains("event.key === 'Tab' && open"));
        assert!(html.contains("mobileSidebar.addEventListener?.('change', syncSidebar)"));
    }

    #[test]
    fn highlight_keywords() {
        let out = highlight_signature("pub fn add(a: i32)");
        assert!(out.contains("pub</span>"));
        assert!(out.contains("fn</span>"));
    }
}
