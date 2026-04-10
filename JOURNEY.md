# Journey

## 2026-04-09 — refactor/checker-collection-capability-dedup

- Chose to centralise the repeated structural `Ty` recursion in `hew-types/src/check/admissibility.rs` behind one helper that dispatches back into the existing Vec, HashSet, and HashMap validators at the named-collection boundary.
- Kept the lane checker-only on purpose: the duplication lived in admissibility checking, the behaviour contract already belonged to the checker, and widening the change into serializer or codegen would have mixed cleanup with unrelated ownership and lowering concerns.
- Preserved the existing aggregate validation shape by still running the concrete HashMap, HashSet, and Vec passes separately at the top level, because short-circuiting through one shared pass hid the more specific nested collection diagnostic that current behaviour expects.
