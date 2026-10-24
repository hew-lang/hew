# Optional documentation AI search

Fast documentation search is static and always available through the Pagefind
index published with docs.hew.sh. The Worker in this directory is a separate,
optional adapter for Cloudflare AI Search. The release workflow does not deploy
it and does not enable its UI.

Cloudflare setup, billing approval, and the runtime switch are all required
before activation:

1. Confirm that `docs.hew.sh` and the AI Search instance use the same
   Cloudflare account.
2. Approve the AI Search, indexing, storage, and generation budget. Cloudflare
   AI Search billing begins November 1, 2026. Cloudflare's published AI Search
   controls do not currently provide a dollar-denominated hard stop spanning
   ingestion, storage, queries, and generation. Do not enable this integration
   under a strict monthly-cap requirement unless the account has an additional
   enforceable billing control that covers every component.
3. Create the website-backed instance:

   ```sh
   wrangler ai-search create docs-hew-sh-ai-search --type web-crawler --source docs.hew.sh
   ```

4. Copy `wrangler.toml.example` to the deployment configuration. Keep
   `AI_SEARCH_ENABLED = "false"` while validating the instance and Worker.
5. Deploy the Worker on the same origin at `/api/docs-search`, with suitable
   application-side rate limiting and durable usage accounting. Those controls
   can bound public query traffic, but they are not an account-wide spending
   cap: crawler ingestion, storage, and the configured generation service also
   contribute to cost. The Worker calls the single-instance `DOCS_AI_SEARCH`
   binding; it never exposes a service token or provider endpoint to the
   browser.
6. After the runtime flag is enabled, pass
   `--ai-search-endpoint /api/docs-search` to
   `scripts/assemble-docs-site.py` to include the optional UI. Ordinary local
   search keystrokes use Pagefind only and are never sent to this endpoint.

The Worker returns 404 before calling the binding unless the runtime flag is
exactly `true`. Disabling or omitting the AI deployment therefore cannot break
the static site or local search.

Current API references:

- [Cloudflare AI Search Workers binding](https://developers.cloudflare.com/ai-search/api/search/workers-binding/)
- [Wrangler setup](https://developers.cloudflare.com/ai-search/get-started/wrangler/)
- [Limits and pricing](https://developers.cloudflare.com/ai-search/platform/limits-pricing/)
