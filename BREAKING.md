# Hew — Breaking Changes

A running log of breaking changes to Hew's compiler internals, runtime ABI,
and standard library, by release.

v0.6.0 is the surface freeze and working front door on the current lowerer.
v0.7.0 is the final ladder release, with the legacy lowerer deleted.

## Distributed identity and remote PIDs in v0.6.0-rc1

- Node identity is derived from the stable authenticated Noise key or TLS SPKI.
  Operators persist keys with `Node::load_keys` and exchange
  `Node::identity_key()` output.
- `Node::allow_peer(route_slot, credential_hex)` binds a credential to a
  receiver-local non-zero route slot. Slot `0` is reserved; route slots are not
  identity and may differ between nodes.
- `RemotePid<T>` carries the complete key-derived node identity, actor slot, and
  durable session incarnation. It is allocation-free and cannot be forged from
  scalar PID components.
- Same-key restarts advance the session incarnation. Previously captured remote
  PIDs then fail with `StaleRef`; callers must perform a fresh
  `Node::lookup`.
- Registry names are discovery aliases. Repointing or unregistering a name does
  not mutate an already issued PID.
- Monitor termination is delivered through `#[on(down)]` with
  `DownNotification`; `MonitorRef::id()` provides exact correlation.
- Distributed nodes, remote messaging, and cross-node monitor/link operations
  are native-only and are rejected for wasm32 targets.

See [`docs/specs/HEW-DIST-SPEC.md`](docs/specs/HEW-DIST-SPEC.md) for the
normative protocol.

---

## `HewChildSpec` layout change in v0.6.0-rc1

`HewChildSpec` is a public `#[repr(C)]` struct that C and ahead-of-time
embedders construct directly. It has **grown by one trailing field**, so its
size changed. This is a hard ABI break: there is no version tag in the struct
and no length prefix, so a caller that passes the previous shape leaves the
runtime reading whatever follows the allocation as a function pointer.

**Old shape (tail):**

```c
struct HewChildSpec {
    /* ... leading fields unchanged ... */
    void  *config;
    size_t config_size;
    void (*message_drop_fn)(int32_t, void *, size_t);   /* last field */
};
```

**New shape (tail):**

```c
struct HewChildSpec {
    /* ... leading fields unchanged ... */
    void  *config;
    size_t config_size;
    void (*message_drop_fn)(int32_t, void *, size_t);
    void (*sys_dispatch)(HewActor *, int32_t, void *, size_t);  /* NEW, last */
};
```

**Why:** `sys_dispatch` is the child actor's system-message entry point, the
function that receives its `#[on(exit)]` and `#[on(down)]` deliveries. It is
carried in the spec rather than installed afterwards because the first
supervised spawn happens inside `hew_supervisor_add_child_spec`, before any
setter could run, and because each restart re-registers from the spec. Setting
it post-hoc gave the initial incarnation a different system surface from every
restart.

**Migration:**

- Rebuild against the new header. Field order matters; `sys_dispatch` is last.
- A child actor with no `#[on(exit)]` and no `#[on(down)]` hook sets
  `sys_dispatch = NULL`, which reproduces the previous behaviour exactly.
- A child actor with either hook sets it to that actor's
  `__hew_actor_sys_dispatch_<Actor>` symbol.
- Zero-initialise the whole struct (`memset` or `= {0}`) before filling it, so a
  future trailing field cannot repeat this as a silent read of stack garbage.

Hew source needs no migration: `hew_supervisor_add_child_spec` is no longer
user-declarable and the compiler emits the spec literal, whose codegen mirror
(`hew_child_spec_struct_type`) appends the matching slot.

---

## Public API and language surface

Public language semantics, standard-library APIs, and wire-protocol surfaces
follow their normal deprecation and stability policy. See `CHANGELOG.md` for
user-visible changes.

---

## Removed in v0.5

### `std::collections::hashset` module

- **Removed:** the Hew-source module `std::collections::hashset` and its
  opaque `HashSet` handle API (`hashset.new()`, `insert_int`,
  `insert_string`, `contains_int`, `contains_string`, `remove_int`,
  `remove_string`, `clear`, and `free`).
- **Replacement:** use built-in `HashSet<T>` directly, for example
  `HashSet::<i64>::new()` or `HashSet::<String>::new()`, with `.insert()`,
  `.contains()`, `.remove()`, `.len()`, and `.is_empty()`.
- **Behaviour change:** built-in `HashSet<T>` releases through RAII; callers
  do not call `free()`.

### `HewScope` runtime substrate + `hew_scope_*` C ABI

- **Removed:** the `hew-runtime::scope` module (`HewScope`, `hew_scope_new`,
  `hew_scope_create`, `hew_scope_destroy`, `hew_scope_free`, `hew_scope_spawn`,
  `hew_scope_cancel`, `hew_scope_is_cancelled`, `hew_scope_wait_all`).
- **Removed:** `hew-runtime-testkit::TestScope` (RAII wrapper around the
  legacy ABI).
- **Replacement:** `HewTaskScope` (`hew_task_scope_*`) is the canonical
  structured-concurrency substrate; `scope { … }` source syntax is preserved
  unchanged and now lowers exclusively to `hew_task_scope_*`.
- **Behaviour change (looser):** the legacy `MAX_ACTORS=64` ceiling
  (`HEW_SCOPE_MAX_ACTORS`) is gone; `HewTaskScope` is unbounded.
- **Rationale:** A244 substrate-first / A250 one-canonical-name.
  No compat shim per the pre-1.0 policy.
