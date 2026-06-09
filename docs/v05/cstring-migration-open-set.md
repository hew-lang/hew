# C-string allocator migration — OPEN set (S1 work order)

Status: **scaffolding complete (S0)** — this document is the authoritative
checklist for the atomic C-string allocator flip (**S1**). It is produced by
stage S0 (`feat/cstring-free-site-classification`), which is behaviour-identical:
every site below is annotated in-source with a greppable marker, but no
allocation or free call has been rewired yet.

## The flip

cabi gains a 16-byte header on every Hew-string allocation so that
`free_cstring` can recover the true base pointer and validate it
(`hew-cabi/src/cabi.rs`: `malloc_cstring` / `alloc_cstring` / `free_cstring`,
`validate_cstring_header`). S1 makes the header live by:

1. Flipping the **single shared producer** `malloc_cstring`
   (`hew-cabi/src/cabi.rs:20`, marked `CSTRING-ALLOC: str-open`). Because
   `str_to_malloc` calls `malloc_cstring`, this one change makes all ~80 std
   `-> string` externs header-aware with zero per-crate edits.
2. Migrating each **raw-malloc / strdup string producer** that bypasses
   `malloc_cstring` (enumerated below) to `alloc_cstring`.
3. Routing every **`str-open` free** to `free_cstring`, and leaving every
   **`libc-bytes` / `container-elem-P2b` / `struct`** free on `libc::free`.

### The bidirectional safety rule

S1 is correct iff, after the flip:

- every pointer that reaches the header-aware path (`free_cstring`,
  `hew_string_drop`) was produced by a header-aware allocator, and
- every header-aware pointer is freed only through the header-aware path.

A single misclassification corrupts the heap: passing a headerless pointer to
`free_cstring` frees `ptr - 16`; passing a header-aware pointer to `libc::free`
frees `ptr` while the real allocation began at `ptr - 16`.

## Marker vocabulary

| Marker | Meaning | S1 action |
| --- | --- | --- |
| `CSTRING-ALLOC: str-open` | raw String producer bypassing `malloc_cstring` | migrate to `alloc_cstring` |
| `CSTRING-ALLOC: libc-bytes` | opaque byte-buffer producer | leave on `libc::malloc` |
| `CSTRING-FREE: str-open` | frees a header-aware Hew String | route to `free_cstring` |
| `CSTRING-FREE: container-elem-P2b` | headerless `strdup` container element | leave on `libc::free` (P2b) |
| `CSTRING-FREE: libc-bytes` | opaque byte buffer | leave on `libc::free` |
| `CSTRING-FREE: struct` | non-string wrapper struct | leave on `libc::free` |

Find every site: `rg 'CSTRING-(ALLOC|FREE):' hew-cabi/src hew-runtime/src std`.

## Producers S1 MUST migrate individually

These do **not** flow through `malloc_cstring`, so the shared flip does not
reach them. Each returns a `*mut c_char` that is later dropped by
`hew_string_drop` or freed by a `str-open` site.

| Producer | Site | Note |
| --- | --- | --- |
| `malloc_cstring` (shared) | `hew-cabi/src/cabi.rs:20` | the one flip that covers all `str_to_malloc` callers |
| `hew_string_concat` | `hew-runtime/src/string.rs:48` | **plan-omitted** raw malloc |
| `hew_string_replace` | `hew-runtime/src/string.rs:438` | raw malloc |
| `hew_char_to_string` | `hew-runtime/src/string.rs:495` | raw malloc |
| `hew_vec_join_str` | `hew-runtime/src/string.rs:754` | raw malloc |
| `hew_string_to_lowercase` | `hew-runtime/src/string.rs:803` | raw malloc |
| `hew_string_to_uppercase` | `hew-runtime/src/string.rs:827` | raw malloc |
| `hew_string_from_char` | `hew-runtime/src/string.rs:892` | raw malloc |
| `hew_string_repeat` | `hew-runtime/src/string.rs:925` | raw malloc |
| `hew_string_clone` | `hew-runtime/src/string.rs:1133` | raw malloc |
| `hew_stream_collect_string` | `hew-runtime/src/stream.rs:1736` | **plan-omitted** raw malloc |
| `hew_proto_msg_get_string` | `std/encoding/protobuf/src/lib.rs:620` | **plan-omitted**; crate imports only `malloc_bytes` |
| `hew_ws_message_text` | `std/net/websocket/src/lib.rs:1199` | raw malloc, returned to Hew |
| websocket reader `str_ptr` | `std/net/websocket/src/lib.rs:731` | raw malloc, callback arg |
| `raw_http_strdup` | `std/net/http/src/client.rs:85` | `libc::strdup`; http/client produces strings via BOTH this and `str_to_malloc` |
| `hew_read_file` | `hew-runtime/src/io_time.rs:46` | **re-sweep addition** raw malloc; user-visible `stable`-tier `-> string`; was mis-bucketed as a non-string subsystem |
| `hew_bytes_to_string` | `hew-runtime/src/bytes.rs:548,566` | **re-sweep addition** raw malloc; the canonical `bytes -> string` (W4.039), bound `-> string` in `std/io.hew`, `std/net/net.hew`, `std/net/quic/quic.hew`; was mis-bucketed under non-string `bytes.rs` |
| `bytes_to_cstr` (channel recv) | `hew-runtime/src/channel_common.rs:7` | **re-sweep addition** raw malloc; backs `hew_channel_recv`/`hew_channel_try_recv` (native + wasm), bound `recv -> Option<string>` in `std/channel/channel.hew`; was mis-bucketed under non-string `channel.rs` |

> The `StringMapFn` closure output (`hew-runtime/src/stream.rs:632`) is a Hew
> `(string) -> string` result; it inherits the header from whatever the closure
> returns (already header-aware via the shared flip) — no producer edit needed,
> but its free is `str-open`.

## Plan corrections (S1-critical)

1. **`hew-runtime/src/string.rs:746,771` are NOT `str-open`.** The plan listed
   them as str-open consumer temps. They free `hew_vec_get_str` output, which is
   a **headerless `libc::strdup`** — classified `container-elem-P2b`. Routing
   them to `free_cstring` in S1 would free `ptr - 16` and corrupt the heap.
2. **Raw String producers the plan omitted:** `hew_string_concat`
   (`string.rs:48`), `hew_stream_collect_string` (`stream.rs:1736`),
   `hew_proto_msg_get_string` (protobuf `:620`), websocket text/reader.
3. **`hew_cron_free_string` is a 4th `*_string_free`** alongside json/xml/yaml.
4. **`hew_vec_pop_str` is NOT MIR-allowlisted** (only `hew_vec_get_str` is, at
   `hew-mir/.../runtime_symbols.rs:400`) — a stronger container/string drop-domain
   separation than the plan assumed. Guarded by the canary test below.
5. **`hew_msgpack_free` doc overstates scope** (`std/encoding/msgpack/src/lib.rs:344`):
   its comment says it accepts any `hew_msgpack_*` pointer, but `to_json` strings
   drop via `hew_string_drop`, not here. Tighten the doc in S1 (cf. the
   `validate_cstring_header` precision fix already landed in S0).
6. **`hew_http_request_body_string`** (`std/net/http/src/server.rs:541`) frees
   its intermediate `malloc_bytes` buffer (`:555`, `libc-bytes`) after copying
   into a `malloc_cstring` result string — the free **stays `libc::free`**.

## str-open free ledger (route to `free_cstring` in S1)

### Runtime
- `hew-runtime/src/string.rs`: 608, 623, 661, 680, 1115 (`hew_string_drop` — the
  universal consumer; keep the `is_static_string` skip BEFORE `free_cstring`),
  1478, 1936 (tests).
- `hew-runtime/src/stream.rs`: 632, 3357, 3378.
- `hew-runtime/src/process.rs`: 458, 462.
- `hew-runtime/src/env.rs`: 622, 846, 890, 925.
- `hew-runtime/src/file_io.rs`: 503 (note: stale `strdup` SAFETY comment — the
  producer is actually `str_to_malloc`).
- `hew-runtime/src/io_time.rs`: 973, 1052 (**re-sweep addition** — frees
  `hew_read_file` raw-malloc string output).
- `hew-runtime/src/bytes.rs`: 1016, 1097 (**re-sweep addition** — frees
  `hew_bytes_to_string` raw-malloc string output). NB: `bytes.rs:266` is the
  refcounted byte-buffer free and STAYS `libc::free` (libc-bytes).
- `hew-runtime/src/channel.rs`: 391, 414, 474, 551 (**re-sweep addition** —
  tests freeing `hew_channel_recv` string output from `bytes_to_cstr`).
- `hew-runtime/src/channel_wasm.rs`: 696, 792 (**re-sweep addition** — same,
  wasm channel recv).

### std (73 sites)
- jwt 410; password 116; uuid 66; json 406; xml 558; yaml 503; url 249; smtp 455;
  datetime 248; cron 255/312/451; regex 288/364/392; toml 655/811/826;
  ipnet 227/245; dns 113/182; markdown 106/155; tls 537.
- net/quic 1761 (**re-sweep / review addition** — `take_string` test helper frees
  `str_to_malloc` output via a crate-local bare `free` extern, not `libc::free`;
  this is why the original `libc::free`-keyed sweep missed it).
- http/client: **all 24** (498, 510, 515, 1029, 1041, 1059, 1081, 1097, 1116,
  1216, 1233, 1248, 1260, 1279, 1359, 1361, 1388, 1425, 1427, 1673, 1693, 1871,
  1899, 1934).
- http/server: 1071, 1388, 1390, 1457, 1459, 1552.
- protobuf: 722, 785, 790, 875, 1461.
- msgpack: 490, 514, 538, 636, 769, 783, 855, 878, 900, 923.
- websocket: 750, 1578.

## Stay on `libc::free` (do NOT touch in S1)

### container-elem-P2b (headerless strdup elements)
- `hew-runtime/src/string.rs`: 746, 771.
- `hew-runtime/src/process.rs`: 50 (`free_c_string`).
- `hew-runtime/src/vec.rs`: 866, 1007, 1676, 2236 (`pop_str` test), 2931
  (`get_str` test).

### libc-bytes (opaque byte buffers)
- `hew-runtime/src/stream.rs`: byte item buffers — 1411, 1445, 2002, 2321, 2572,
  2585, 2613, 2682, 2702, 2723, 2743, 2757, 2776, 2823, 2842, 3479, 3729, 3955
  (producers: 1104, 1145, 1378, 2092).
- `hew-runtime/src/string.rs`: 1986 (test).
- `hew-runtime/src/vec.rs`: 1052, 1104 (backing arrays).
- std: compress 398; tls 553; http/server 555, 1635, 2080, 2173, 2228;
  protobuf 724, 756, 795, 827, 879, 881, 1303, 1361, 1384, 1407, 1431, 1470;
  msgpack 344 (`hew_msgpack_free`); websocket 1227 (`message.data`).

### struct (non-string wrappers)
- `hew-runtime/src/vec.rs`: 1054, 1106.

## Non-string subsystems (verified — STAY `libc::free`, no inline annotation)

These files carry no **owned `-> string`** returns: they free internal byte
buffers, structs, or borrowed/internally-owned C strings that never reach
`hew_string_drop`. Documented at subsystem granularity rather than per-site (a
non-annotated site correctly stays `libc::free` in S1 — the danger is a missing
`str-open`, and that set is exhaustive above).

The discriminator applied in the re-sweep: a runtime export typed
`-> *mut c_char` that Hew owns as a `string` is `str-open` (its result reaches
`hew_string_drop`); a `-> *const c_char` borrow, or a C string kept inside a
struct and freed by that struct's own `*_free`, STAYS `libc::free`.

- `actor.rs` — state wrappers, reply payloads, id buffers.
- `mailbox.rs` / `mailbox_wasm.rs` — envelopes.
- `reply_channel.rs` / `reply_channel_wasm.rs`, `scheduler_wasm.rs` — reply payloads.
- `supervisor.rs` — strdup pool / child names (internal struct fields, freed by
  the supervisor's own teardown — not returned as owned Hew strings) + malloc structs.
- `generator.rs` — value-payload buffers.
- `encryption.rs` — byte buffers.
- `result.rs` — `error_msg` is `libc::strdup`'d into `HewResult` and freed by
  `hew_result_free` (`result.rs:322`); the only getter
  (`hew_result_error_msg:246`) returns a `*const c_char` **borrow**, so it never
  reaches `hew_string_drop`. STAYS.
- `hew_node.rs` — reply payloads + `bind_addr` strdup (internal copy, freed
  internally — not an owned Hew-string return).
- `alloc_tracker.rs`, `lambda_actor.rs`, `task_scope.rs`,
  `iter.rs`, `hashset.rs` (1 struct free) — structs / buffers. `hashmap.rs` has
  0 real free calls.

> **Re-sweep correction (S1-critical).** Earlier drafts listed `channel.rs` /
> `channel_wasm.rs`, `bytes.rs`, and `io_time.rs` here as "stay". That was
> **wrong**: each owns a raw-malloc `-> string` producer whose result reaches
> `hew_string_drop`, so leaving them on `libc::free` while the shared drop becomes
> header-aware would corrupt the heap. They are now in the producer table and the
> str-open ledger above. The flawed reasoning — "string payloads are `libc::malloc`
> copies, not `str_to_malloc`, so they stay" — is invalid: provenance via
> `str_to_malloc` is sufficient but not necessary; what matters is whether the
> pointer reaches `hew_string_drop`/a `str-open` free.

## Drop-domain canary (regression guard)

`hew-mir/tests/cstring_container_domain_canary.rs` (landed in S0) guards the
container/string drop-domain separation that keeps `container-elem-P2b` sites
from ever reaching the header-aware path:

- `string_element_vec_accessors_are_allowlisted_but_guarded` — asserts
  `hew_vec_get_str` IS MIR-allowlisted while `hew_vec_pop_str` is NOT.
- `vec_string_index_never_emits_headerless_string_accessor` — lowers
  `fn get_first(xs: vec<string>) -> string { xs[0] }` and asserts no
  `hew_vec_get_str` / `hew_vec_pop_str` runtime call is emitted, plus a
  `NotYetImplemented` "element type for xs[i]" diagnostic. The invariant lives in
  `hew-mir/.../lower.rs` `lower_vec_index`: the element-type dispatch has no
  `ResolvedTy::String` arm, so a String element falls through to
  `NotYetImplemented` rather than emitting a headerless accessor.

## S1 exit gate

- [ ] Flip `malloc_cstring` (cabi:20) + migrate all 17 individual producers above
      (plus the shared `malloc_cstring` flip itself).
- [ ] Route all 100 `str-open` frees to `free_cstring`.
- [ ] Leave all `container-elem-P2b` / `libc-bytes` / `struct` frees untouched.
- [ ] Tighten `hew_msgpack_free` doc scope.
- [ ] Canary stays green; full suite green; ASan/valgrind clean over the std FFI
      round-trip tests.
