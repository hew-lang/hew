# CI information and script safety

- Corrected the local agent build instructions against current Make targets before repository edits.
- Remote Linux CI now validates requests before synchronization, addresses the captured commit without force updates, and retains unique worktrees for inspection. Local Git integration coverage verifies dirty-work preservation, spaced paths and remote failure propagation.
- Report finalization now replaces its own prior outputs on retries, preserves unrelated files, and rejects using the raw report directory as output. Regression coverage exercises a failed run followed by success.
- Shell lint now covers every tracked shell script and uses its declared sh/bash dialect. Broader coverage found and fixed an unquoted benchmark cleanup trap; discovery, syntax failures and POSIX/Bash distinction pass through the existing harness.
- Downstream synchronization is now an explicit local Make command, outside hosted shards and preflight. The help entry explains its scope and the comment names HEW_SYNC_PARENT for worktree users.
- Split lint into Rust, source/ABI contracts and Hew formatting; moved tooling behaviour tests to their own required Linux dependency and compiler measurements to a named Linux step. Compiled-Hew report processing no longer depends on unrelated gate success.
- Workflow validation, tooling tests, full lint (including JSON Clippy and Hew formatting), and compiler measurements pass. The preflight dependency comparison removes only downstream-check and adds grouping targets. Remote-helper behaviour was exercised with local Git and an SSH stand-in on Linux; native macOS and Windows execution was not run locally.

- Reviewed hosted PR results: Rust lint and Hew formatting pass independently of the newly exposed shell-lint failure; tooling tests pass. Linux shard 1 has the same template fixture type mismatch as the base run.
- Standardized CI and local development on ShellCheck 0.11.0, removed the older-version compatibility suppression, refreshed README prerequisites, and added an opt-in make check-requirements command. Normal build, lint and test graphs do not invoke it.
- Fixed real shell issues exposed during review: package staging no longer swallows copy failures, and the distributed example uses a portable temporary directory for binaries and logs. Cleanup passed after both forced compilation failure and a successful stubbed run with spaced paths.
