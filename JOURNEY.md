# CI information and script safety

- Corrected the local agent build instructions against current Make targets before repository edits.
- Remote Linux CI now validates requests before synchronization, addresses the captured commit without force updates, and retains unique worktrees for inspection. Local Git integration coverage verifies dirty-work preservation, spaced paths and remote failure propagation.
- Report finalization now replaces its own prior outputs on retries, preserves unrelated files, and rejects using the raw report directory as output. Regression coverage exercises a failed run followed by success.
- Shell lint now covers every tracked shell script and uses its declared sh/bash dialect. Broader coverage found and fixed an unquoted benchmark cleanup trap; discovery, syntax failures and POSIX/Bash distinction pass through the existing harness.
