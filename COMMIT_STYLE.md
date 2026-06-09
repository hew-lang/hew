# Commit Style

This repo uses Conventional Commits with a strict house style for voice.
The goal is a history that scans cleanly, reads consistently, and explains
behavioural intent before implementation detail.

## Required Header Shape

Every non-merge commit should use:

```text
type(scope): imperative summary
```

Examples:

- `feat(parser): add entry/exit blocks for machine declarations`
- `fix(types): reject Int alias with i64 migration hint`
- `docs(spec): document scope blocks as Unit-typed`

Rules:

- Use one of: `feat`, `fix`, `refactor`, `docs`, `test`, `build`, `ci`,
  `chore`, `perf`, `revert`.
- Include a scope when there is a clear subsystem or boundary.
- Start the summary with an imperative verb: `add`, `remove`, `lower`,
  `rename`, `document`, `regenerate`, `tighten`, `stabilize`.
- Keep the first line under 72 characters when possible.
- Do not end the summary with a period.
- Keep the summary concrete. Name the semantic change, not just the files.

## Voice Rules

Preferred voice:

```text
type(scope): imperative action + concrete object
```

Good:

- `feat(mir): add Place variants for Duplex handles`
- `fix(parser): suppress <- removal error cascade`
- `docs(spec): mark C FFI section as deferred`

Avoid noun-led or label-led summaries when an imperative form is clearer:

- Prefer `feat(mir): add Place variants for Duplex handles`
  over `feat(mir): Place variants for Duplex handles`
- Prefer `docs(spec): document scope blocks as Unit-typed`
  over `docs(spec): scope blocks evaluate to Unit`

Also avoid vague summaries such as `update`, `misc cleanup`, or
`address review comments` unless the commit is genuinely that broad.

## Body Rules

Use the body to explain why the change exists and what semantic boundary it
touches.

Rules:

- Start with a short rationale-first paragraph.
- Explain the behavioural change before listing mechanics.
- Use bullets only when they clarify a multi-part change.
- Keep file lists secondary to semantics.
- If the change is breaking, use a `BREAKING CHANGE:` footer.

Preferred opening patterns:

- `The parser now ...`
- `This change ...`
- `A previous implementation ...`
- `When ...`

Less consistent patterns to avoid mixing arbitrarily across the branch:

- fragment-only openings such as `Diagnostics for ...`
- purely mechanical openings such as `Files changed: ...`

## Merge Commits

For final branch history, prefer rebased or squashed commits over raw
`merge ...` subjects. If a merge commit must remain, keep it rare and make
its subject intentional.

See [CONTRIBUTING.md](CONTRIBUTING.md) for the surrounding contribution
workflow and review expectations.
