# Optional documentation AI search

Fast documentation search is static and always available through the Pagefind
index published with docs.hew.sh. The Worker in this directory is a separate,
optional adapter for Cloudflare AI Search. The release workflow does not deploy
it and does not enable its UI.

Cloudflare setup and the runtime switch are both required before activation:

1. Confirm that `docs.hew.sh` and the AI Search instance use the same
   Cloudflare account.
2. Use the dedicated `docs-hew-sh` AI Gateway for generation. Keep its US$10
   fixed-month spend limit and ten-request-per-minute limit enabled, require
   authenticated Gateway and provider requests, and keep retries, Gateway
   caching, and request logging off. The spend limit is not an all-in
   transactional hard cap: enforcement can lag, and AI Search ingestion,
   storage, and retrieval have their own allowances and billing. The Worker
   limits below provide an independent bound on public use.
3. Use the website-backed `docs-hew-sh` instance. If it must be recreated,
   create it only after the site containing `sitemap.xml` is live, and restrict
   indexed items to the documentation root and generated HTML pages:

   ```sh
   wrangler ai-search create docs-hew-sh \
     --type web-crawler \
     --source https://docs.hew.sh \
     --parse-type sitemap \
     --include-items / \
     --include-items '/*.html' \
     --max-num-results 5
   ```

4. Keep the source paused while the live site lacks the generated sitemap.
   After the static documentation is published, change the parse type from any
   temporary discovery setting to **Sitemap**, then verify a 24-hour sync,
   256-token chunks with 10% overlap, and no image indexing, OCR, or browser
   rendering before resuming it. The generated sitemap contains at most 500
   canonical HTML URLs and gives each one the source revision date in
   `lastmod`. Review the initial crawl before enabling the Worker. Leave the AI
   Search public endpoint disabled; the bound Worker is the only public query
   path.
5. Review `wrangler.toml` before deployment. Its committed
   `AI_SEARCH_ENABLED = "false"` keeps an ordinary deployment fail-closed. The
   Worker retrieves five excerpts through the
   `DOCS_AI_SEARCH` instance binding, then calls
   `@cf/meta/llama-3.2-1b-instruct` through the `AI` binding with
   `gateway.id = "docs-hew-sh"` and Gateway caching disabled. Generation
   therefore cannot silently bypass the dedicated Gateway.
6. Deploy the Worker on the same origin at `/api/docs-search`. Its single
   Durable Object serializes reservations and enforces 500 queries per UTC
   month, 60 per hour globally, five per client per minute, and a
   1,000,000-reported-token monthly stop. A response without provider usage is
   conservatively charged 4,096 tokens. Requests are limited to one
   512-character question in a 2 KiB JSON body; retrieved context is capped at
   12,000 characters, retrieval is capped at five chunks, query rewriting,
   reranking, and similarity caching are off, and generation is capped at 512
   tokens. Returned answers are capped at 4,096 characters. Raising any of
   these limits requires a code change. After the initial crawl and a disabled
   Worker dry run pass, enable the runtime route explicitly:

   ```sh
   npx --yes wrangler@4.147.0 deploy \
     --config docs/ai-search/wrangler.toml \
     --var AI_SEARCH_ENABLED:true
   ```

7. Rebuild the stable site with
   `DOCS_AI_SEARCH_ENDPOINT=/api/docs-search` and deploy that artifact to add
   the optional UI. Ordinary local search keystrokes use Pagefind only and are
   never sent to the AI endpoint.

The public Worker exposes no instance-management, crawling, upload, history,
model-provider, or arbitrary-URL operation. It hashes the connecting address
before storing a short rate-limit window, accepts same-origin requests only,
filters citations to docs.hew.sh, and treats retrieved text as reference data
rather than instructions. It returns 404 before touching the guard or billable
bindings unless the runtime flag is exactly `true`. Disabling or omitting the
AI deployment therefore cannot break the static site or local search.

Current API references:

- [Cloudflare AI Search Workers binding](https://developers.cloudflare.com/ai-search/api/search/workers-binding/)
- [Workers AI Gateway binding](https://developers.cloudflare.com/ai-gateway/usage/worker-binding-methods/)
- [Llama 3.2 1B model and request limits](https://developers.cloudflare.com/workers-ai/models/llama-3.2-1b-instruct/)
- [Wrangler setup](https://developers.cloudflare.com/ai-search/get-started/wrangler/)
- [Limits and pricing](https://developers.cloudflare.com/ai-search/platform/limits-pricing/)
