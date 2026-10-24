#!/usr/bin/env python3
"""Add docs.hew.sh publishing metadata to generic `hew doc` output."""

from __future__ import annotations

import argparse
import html
import json
import re
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import urljoin, urlparse

SEARCH_HEAD = """<link rel="stylesheet" href="/pagefind/pagefind-component-ui.css">
<script type="module" src="/pagefind/pagefind-component-ui.js"></script>
<style>
:root {
  --pf-text: #e2e8f0;
  --pf-text-secondary: #9ca3af;
  --pf-text-muted: #9ca3af;
  --pf-background: #111827;
  --pf-border: #30363d;
  --pf-border-focus: #60a5fa;
  --pf-hover: #1e293b;
  --pf-mark: #facc15;
  --pf-font: system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
  --pf-searchbox-max-width: 56rem;
}
.docs-search { margin: -1rem 0 1.5rem; position: relative; z-index: 2; }
.docs-search pagefind-searchbox { display: block; width: 100%; }
.ai-docs-search { margin-top: 0.75rem; }
.ai-docs-search form { display: grid; gap: 0.5rem; }
.ai-docs-search textarea { min-height: 5rem; resize: vertical; padding: 0.6rem; color: #e2e8f0; background: #111827; border: 1px solid #30363d; border-radius: 6px; font: inherit; }
.ai-docs-search button { width: fit-content; padding: 0.5rem 0.8rem; color: #0d1117; background: #60a5fa; border: 0; border-radius: 6px; font: inherit; font-weight: 600; }
.ai-docs-answer[hidden] { display: none; }
</style>"""


LOCAL_SEARCH = """<section class="docs-search" aria-label="Search documentation" data-pagefind-ignore>
<pagefind-config bundle-path="/pagefind/" base-url="/"></pagefind-config>
<pagefind-searchbox show-sub-results placeholder="Search guides, APIs, and examples"></pagefind-searchbox>
</section>"""


def ai_search(endpoint: str) -> str:
    encoded_endpoint = json.dumps(endpoint)
    return f"""<details class="ai-docs-search" data-pagefind-ignore>
<summary>Ask the documentation with AI</summary>
<p>This optional search sends only the submitted question to the documentation search service.</p>
<form id="ai-docs-form">
<label for="ai-docs-question">Question</label>
<textarea id="ai-docs-question" maxlength="800" required></textarea>
<button type="submit">Ask documentation</button>
<p id="ai-docs-status" role="status" aria-live="polite"></p>
</form>
<div id="ai-docs-answer" class="ai-docs-answer" hidden>
<p id="ai-docs-answer-text"></p>
<ul id="ai-docs-citations"></ul>
</div>
</details>
<script type="module" data-pagefind-ignore>
const form = document.getElementById('ai-docs-form');
if (form) {{
  form.addEventListener('submit', async (event) => {{
    event.preventDefault();
    const question = document.getElementById('ai-docs-question').value.trim();
    if (!question) return;
    const status = document.getElementById('ai-docs-status');
    const answer = document.getElementById('ai-docs-answer');
    status.textContent = 'Searching the documentation…';
    answer.hidden = true;
    try {{
      const response = await fetch({encoded_endpoint}, {{
        method: 'POST',
        headers: {{'content-type': 'application/json'}},
        body: JSON.stringify({{question}}),
      }});
      if (!response.ok) throw new Error('AI documentation search is unavailable.');
      const result = await response.json();
      document.getElementById('ai-docs-answer-text').textContent = result.answer;
      const citations = document.getElementById('ai-docs-citations');
      citations.replaceChildren(...(result.citations || []).map((url) => {{
        const item = document.createElement('li');
        const link = document.createElement('a');
        link.href = url;
        link.textContent = url;
        item.append(link);
        return item;
      }}));
      answer.hidden = false;
      status.textContent = '';
    }} catch (error) {{
      status.textContent = error.message;
    }}
  }});
}}
</script>"""


class FirstParagraph(HTMLParser):
    def __init__(self) -> None:
        super().__init__()
        self.in_main = False
        self.in_paragraph = False
        self.parts: list[str] = []
        self.done = False

    def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        attributes = dict(attrs)
        if tag == "main" and attributes.get("id") == "main-content":
            self.in_main = True
        elif self.in_main and tag == "p" and not self.done:
            self.in_paragraph = True

    def handle_endtag(self, tag: str) -> None:
        if tag == "p" and self.in_paragraph:
            self.in_paragraph = False
            self.done = True
        elif tag == "main":
            self.in_main = False

    def handle_data(self, data: str) -> None:
        if self.in_paragraph:
            self.parts.append(data)

    def description(self) -> str:
        text = " ".join("".join(self.parts).split())
        if len(text) <= 240:
            return text
        shortened = text[:237].rsplit(" ", 1)[0]
        return f"{shortened}…"


def page_title(source: str) -> str:
    match = re.search(r"<title>(.*?)</title>", source, flags=re.DOTALL)
    if not match:
        return "Hew documentation"
    return html.unescape(" ".join(match.group(1).split()))


def page_description(source: str) -> str:
    parser = FirstParagraph()
    parser.feed(source)
    return (
        parser.description()
        or "Language guides and standard-library API documentation for Hew."
    )


def metadata(title: str, description: str, canonical: str, *, indexed: bool) -> str:
    escaped_title = html.escape(title, quote=True)
    escaped_description = html.escape(description, quote=True)
    robots = "index,follow" if indexed else "noindex,nofollow"
    lines = [
        '<meta name="docs-site-assembly" content="docs.hew.sh">',
        f'<meta name="description" content="{escaped_description}">',
        f'<meta name="robots" content="{robots}">',
    ]
    if indexed:
        escaped_canonical = html.escape(canonical, quote=True)
        lines.extend(
            [
                f'<link rel="canonical" href="{escaped_canonical}">',
                '<meta property="og:type" content="website">',
                f'<meta property="og:title" content="{escaped_title}">',
                f'<meta property="og:description" content="{escaped_description}">',
                f'<meta property="og:url" content="{escaped_canonical}">',
            ]
        )
    return "\n".join(lines)


def canonical_url(base_url: str, filename: str) -> str:
    if filename == "index.html":
        return base_url
    return urljoin(base_url, filename)


def replace_required(source: str, old: str, new: str, page: Path) -> str:
    if old not in source:
        raise ValueError(f"{page} has no publishing insertion point for {old!r}")
    return source.replace(old, new, 1)


def assemble(
    site: Path, base_url: str, version: str, ai_search_endpoint: str | None = None
) -> None:
    parsed = urlparse(base_url)
    if parsed.scheme != "https" or not parsed.netloc:
        raise ValueError("--base-url must be an absolute https URL")
    base_url = base_url.rstrip("/") + "/"
    if ai_search_endpoint is not None and (
        not ai_search_endpoint.startswith("/") or ai_search_endpoint.startswith("//")
    ):
        raise ValueError("--ai-search-endpoint must be a same-origin absolute path")

    pages = sorted(site.glob("*.html"))
    if not pages or not (site / "index.html").is_file():
        raise ValueError(f"{site} does not contain generated HTML documentation")

    indexed_urls: list[str] = []
    for page in pages:
        source = page.read_text(encoding="utf-8")
        if 'name="docs-site-assembly"' in source:
            raise ValueError(f"{page} has already been assembled")
        indexed = page.name != "404.html"
        canonical = canonical_url(base_url, page.name)
        head = metadata(
            page_title(source), page_description(source), canonical, indexed=indexed
        )
        marker = '<meta name="viewport" content="width=device-width, initial-scale=1">'
        if marker not in source:
            raise ValueError(f"{page} has no viewport metadata insertion point")
        source = replace_required(source, marker, f"{marker}\n{head}", page)
        if indexed:
            source = replace_required(
                source, "<style>", f"{SEARCH_HEAD}\n<style>", page
            )
            source = replace_required(
                source,
                '<header class="site-header">',
                '<header class="site-header" data-pagefind-ignore>',
                page,
            )
            source = replace_required(
                source, "</header>", f"</header>\n{LOCAL_SEARCH}", page
            )
            source = replace_required(
                source,
                '<main id="main-content">',
                '<main id="main-content" data-pagefind-body>',
                page,
            )
            source = replace_required(
                source, "<footer>", "<footer data-pagefind-ignore>", page
            )
            if ai_search_endpoint is not None:
                source = replace_required(
                    source,
                    "</main>",
                    f"{ai_search(ai_search_endpoint)}\n</main>",
                    page,
                )
        page.write_text(source, encoding="utf-8")
        if indexed:
            indexed_urls.append(canonical)

    sitemap = [
        '<?xml version="1.0" encoding="UTF-8"?>',
        '<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">',
    ]
    sitemap.extend(
        f"  <url><loc>{html.escape(url)}</loc></url>" for url in indexed_urls
    )
    sitemap.append("</urlset>")
    (site / "sitemap.xml").write_text("\n".join(sitemap) + "\n", encoding="utf-8")
    (site / "robots.txt").write_text(
        f"User-agent: *\nAllow: /\nSitemap: {urljoin(base_url, 'sitemap.xml')}\n",
        encoding="utf-8",
    )

    released = version if version.startswith("v") else f"v{version}"
    (site / "llms.txt").write_text(
        f"""# Hew Documentation

> Language guides and standard-library API reference for Hew {released}.

The pages on this site describe one Hew release. Use the language guides for
syntax and semantics, and the standard-library pages for exact public APIs.

## Start here

- [Language basics]({urljoin(base_url, "language.basics.html")})
- [Core concepts]({urljoin(base_url, "language.concepts.html")})
- [Ownership and sending]({urljoin(base_url, "language.ownership.html")})
- [Actors]({urljoin(base_url, "language.actors.html")})
- [Structured concurrency]({urljoin(base_url, "language.concurrency.html")})
- [Target capabilities]({urljoin(base_url, "language.targets.html")})

## Reference

- [Complete documentation index]({base_url})
- [Hew source repository](https://github.com/hew-lang/hew)
""",
        encoding="utf-8",
    )
    (site / "agents.txt").write_text(
        f"""Hew documentation release: {released}
Canonical index: {base_url}
Language guide: {urljoin(base_url, "language.basics.html")}
Core concepts: {urljoin(base_url, "language.concepts.html")}
Standard-library reference: {base_url}#modules
Machine-readable guide: {urljoin(base_url, "llms.txt")}
Source repository: https://github.com/hew-lang/hew

The documentation is release-specific. Language-guide pages define Hew syntax
and semantics; std.* pages describe the public standard-library API shipped in
the same release.
""",
        encoding="utf-8",
    )


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("site", type=Path)
    parser.add_argument("--base-url", required=True)
    parser.add_argument("--version", required=True)
    parser.add_argument("--ai-search-endpoint")
    args = parser.parse_args()
    assemble(args.site, args.base_url, args.version, args.ai_search_endpoint)


if __name__ == "__main__":
    main()
