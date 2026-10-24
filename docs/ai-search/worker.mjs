const json = (value, status = 200) =>
  Response.json(value, {
    status,
    headers: {
      "cache-control": "no-store",
      "x-content-type-options": "nosniff",
    },
  });

export default {
  async fetch(request, env) {
    const url = new URL(request.url);
    if (url.pathname !== "/api/docs-search") {
      return new Response("Not found", { status: 404 });
    }

    // Check the runtime flag before touching the billable binding. A deployed
    // but disabled Worker therefore has no hidden AI-search route.
    if (env.AI_SEARCH_ENABLED !== "true") {
      return new Response("Not found", { status: 404 });
    }
    if (request.method !== "POST") {
      return new Response("Method not allowed", {
        status: 405,
        headers: { allow: "POST" },
      });
    }

    const origin = request.headers.get("origin");
    if (origin && origin !== url.origin) {
      return json({ error: "Cross-origin requests are not accepted." }, 403);
    }
    const contentLength = Number(request.headers.get("content-length") || 0);
    if (contentLength > 2048) {
      return json({ error: "Question is too large." }, 413);
    }

    let body;
    try {
      body = await request.json();
    } catch {
      return json({ error: "Expected a JSON request." }, 400);
    }
    const question = typeof body.question === "string" ? body.question.trim() : "";
    if (!question || question.length > 800) {
      return json({ error: "Question must contain 1 to 800 characters." }, 400);
    }
    if (!env.DOCS_AI_SEARCH) {
      return json({ error: "Documentation AI search is not configured." }, 503);
    }

    try {
      const result = await env.DOCS_AI_SEARCH.chatCompletions({
        messages: [{ role: "user", content: question }],
        ai_search_options: {
          retrieval: { max_num_results: 5 },
          query_rewrite: { enabled: false },
        },
      });
      const citations = [
        ...new Set(
          (result.chunks || [])
            .map((chunk) => chunk?.item?.key)
            .filter(
              (key) =>
                typeof key === "string" &&
                (key.startsWith("https://docs.hew.sh/") || key.startsWith("/")),
            ),
        ),
      ].slice(0, 5);
      return json({
        answer: result.choices?.[0]?.message?.content || "No answer was returned.",
        citations,
      });
    } catch {
      return json({ error: "Documentation AI search is temporarily unavailable." }, 502);
    }
  },
};
