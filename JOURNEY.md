# CI information and script safety

- Corrected the local agent build instructions against current Make targets before repository edits.
- Remote Linux CI now validates requests before synchronization, addresses the captured commit without force updates, and retains unique worktrees for inspection. Local Git integration coverage verifies dirty-work preservation, spaced paths and remote failure propagation.
