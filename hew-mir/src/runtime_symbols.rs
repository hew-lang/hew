//! Runtime-ABI symbol admission for `Instr::CallRuntimeAbi`.
//!
//! `Instr::CallRuntimeAbi` carries a `String` symbol naming a `hew_*`
//! C-ABI entry in `hew-runtime/`. Accepting any string at construction
//! time would invite typos that survive to link-time (or worse, silently
//! route to a wrong runtime entry). Per HEW-SPEC §3.7
//! boundary-fail-closed and LESSONS row P0 `boundary-fail-closed` (49),
//! the producer validates every symbol BEFORE the `Instr` enters the
//! `BasicBlock::instructions` stream; an unrecognised symbol surfaces as a
//! hard `MirDiagnostic` so the failure lands at MIR construction, not at
//! codegen link-time.
//!
//! This is a deliberately curated subset of the runtime symbols classified
//! `stable` or `codegen-stable` in `scripts/jit-symbol-classification.toml`,
//! plus seven synthetic spellings with no `hew-runtime` export. The build
//! script checks that every runtime-backed entry remains in one of those TOML
//! tiers, but the broader TOML classification never widens this fail-closed
//! admission boundary. Adding an emitter symbol therefore requires an explicit
//! allowlist decision; runtime-backed additions must also already be classified
//! in the TOML, while synthetic additions need a documented non-runtime
//! lowering rationale.
//!
//! The ownership-contract table below (`callee_ownership_contract`) is a
//! separate, broader positive-membership authority over callee spellings
//! (it also covers `Terminator::Call` builtin spellings) and is
//! deliberately not folded into the admission predicate.

#[cfg(test)]
use hew_types::runtime_call::{all_runtime_call_families, is_pre_staged_family, RuntimeCallFamily};

/// Runtime-exported symbols that `Instr::CallRuntimeAbi` may name.
///
/// This fail-closed list is intentionally narrower than the TOML's complete
/// stable/codegen-stable runtime surface. Keep it lexicographically sorted;
/// `build.rs` verifies that every entry remains classified in one of those two
/// TOML tiers.
const RUNTIME_BACKED_MIR_SYMBOLS: &[&str] = &[
    "hew_actor_ask",
    "hew_actor_ask_with_channel",
    "hew_actor_cooperate",
    "hew_actor_demonitor",
    "hew_actor_link",
    "hew_actor_monitor",
    "hew_actor_self",
    "hew_actor_send_by_id",
    "hew_actor_spawn",
    "hew_actor_unlink",
    "hew_auto_mutex_alloc",
    "hew_auto_mutex_free",
    "hew_auto_mutex_lock",
    "hew_auto_mutex_unlock",
    "hew_bytes_append",
    "hew_bytes_clear",
    "hew_bytes_contains",
    "hew_bytes_index",
    "hew_bytes_is_empty",
    "hew_bytes_len",
    "hew_bytes_pop",
    "hew_bytes_push",
    "hew_bytes_set",
    "hew_bytes_slice",
    "hew_cancel_token_is_requested",
    "hew_cancel_token_release",
    "hew_cancel_token_retain",
    "hew_duplex_clone",
    "hew_duplex_close",
    "hew_duplex_close_half",
    "hew_duplex_pair",
    "hew_duplex_payload_free",
    "hew_duplex_recv",
    "hew_duplex_recv_half",
    "hew_duplex_send",
    "hew_duplex_send_half",
    "hew_duplex_try_recv",
    "hew_duplex_try_send",
    "hew_duration_abs",
    "hew_duration_hours",
    "hew_duration_is_zero",
    "hew_duration_micros",
    "hew_duration_millis",
    "hew_duration_mins",
    "hew_duration_nanos",
    "hew_duration_secs",
    "hew_dyn_box_alloc",
    "hew_dyn_box_free",
    "hew_hashmap_contains_key_layout",
    "hew_hashmap_free_layout",
    "hew_hashmap_get_layout",
    "hew_hashmap_insert_layout",
    "hew_hashmap_len_layout",
    "hew_hashmap_new_with_layout",
    "hew_hashmap_remove_layout",
    "hew_hashset_contains_layout",
    "hew_hashset_free_layout",
    "hew_hashset_insert_layout",
    "hew_hashset_is_empty_layout",
    "hew_hashset_len_layout",
    "hew_hashset_new_with_layout",
    "hew_hashset_remove_layout",
    "hew_instant_duration_since",
    "hew_instant_elapsed",
    "hew_instant_now",
    "hew_lambda_actor_ask",
    "hew_lambda_actor_clone",
    "hew_lambda_actor_downgrade",
    "hew_lambda_actor_new",
    "hew_lambda_actor_release",
    "hew_lambda_actor_send",
    "hew_lambda_actor_weak_clone",
    "hew_lambda_actor_weak_drop",
    "hew_lambda_actor_weak_send",
    "hew_lambda_body_alloc_reply_buf",
    "hew_lambda_drain_all",
    "hew_metric_counter_add",
    "hew_metric_counter_inc",
    "hew_metric_counter_register",
    "hew_metric_gauge_add",
    "hew_metric_gauge_dec",
    "hew_metric_gauge_inc",
    "hew_metric_gauge_register",
    "hew_metric_gauge_set",
    "hew_metric_histogram_record",
    "hew_metric_histogram_register",
    "hew_metric_histogram_register_simple",
    "hew_metric_vec_register",
    "hew_metric_vec_with",
    "hew_node_link_remote",
    "hew_node_monitor",
    "hew_node_monitor_recv",
    "hew_observe_barrier",
    "hew_observe_read_u64",
    "hew_observe_scrape",
    "hew_observe_series",
    "hew_rc_new",
    "hew_recv_half_recv",
    "hew_recv_half_try_recv",
    "hew_reply_channel_cancel",
    "hew_reply_channel_free",
    "hew_reply_channel_new",
    "hew_reply_payload_free",
    "hew_reply_wait",
    "hew_select_first",
    "hew_send_half_send",
    "hew_send_half_try_send",
    "hew_string_char_at",
    "hew_string_char_at_utf8",
    "hew_string_char_count",
    "hew_string_concat",
    "hew_string_find",
    "hew_string_index",
    "hew_string_slice_codepoints",
    "hew_supervisor_child_get",
    "hew_supervisor_nested_get",
    "hew_supervisor_pool_child_get",
    "hew_supervisor_pool_len",
    "hew_supervisor_restart_await_blocking",
    "hew_supervisor_stop",
    "hew_task_await_blocking",
    "hew_task_complete_threaded",
    "hew_task_completion_observe",
    "hew_task_completion_unobserve",
    "hew_task_free",
    "hew_task_get_env",
    "hew_task_get_error",
    "hew_task_get_result",
    "hew_task_new",
    "hew_task_scope_cancel_after_ns",
    "hew_task_scope_destroy",
    "hew_task_scope_join_all",
    "hew_task_scope_new",
    "hew_task_scope_set_current",
    "hew_task_scope_spawn",
    "hew_task_set_env",
    "hew_task_set_result",
    "hew_task_spawn_thread",
    "hew_vec_get_bool",
    "hew_vec_get_f32",
    "hew_vec_get_f64",
    "hew_vec_get_i16",
    "hew_vec_get_i32",
    "hew_vec_get_i64",
    "hew_vec_get_i8",
    "hew_vec_get_layout",
    "hew_vec_get_owned",
    "hew_vec_get_ptr",
    "hew_vec_get_str",
    "hew_vec_get_u16",
    "hew_vec_get_u8",
    "hew_vec_len",
    "hew_vec_slice_range_bytesize",
    "hew_vec_slice_range_f64",
    "hew_vec_slice_range_i32",
    "hew_vec_slice_range_i64",
    "hew_vec_slice_range_layout",
    "hew_vec_slice_range_owned",
    "hew_vec_slice_range_ptr",
    "hew_vec_slice_range_str",
    "hew_vtable_dispatch_panic_on_oob",
];

/// Compiler-recognised MIR emitter spellings with no `hew-runtime` export.
///
/// Keep this list lexicographically sorted. Each entry is admitted only because
/// codegen or a separately linked stdlib component implements the operation.
pub const SYNTHETIC_MIR_SYMBOLS: &[&str] = &[
    // Codegen bounds-checks the bytes triple and materialises `Option<u8>`.
    "hew_bytes_get",
    // The regex stdlib extracts captures; `hew-runtime` exports no such symbol.
    "hew_regex_capture",
    // The regex stdlib compiles module literals; `hew-runtime` exports no such symbol.
    "hew_regex_compile",
    // The regex stdlib frees capture buffers; `hew-runtime` exports no such symbol.
    "hew_regex_free_capture",
    // Codegen loads the compiled literal handle directly from the module table.
    "hew_regex_handle",
    // The regex stdlib performs the match; `hew-runtime` exports no such symbol.
    "hew_regex_match",
    // Codegen bounds-checks codepoints and materialises `Option<char>`.
    "hew_string_get",
];

/// Complete fail-closed MIR emitter authority:
/// runtime-backed symbols union synthetic compiler spellings.
const MIR_EMITTER_RUNTIME_SYMBOLS: &[&[&str]] =
    &[RUNTIME_BACKED_MIR_SYMBOLS, SYNTHETIC_MIR_SYMBOLS];

/// Error returned when a `RuntimeCall` is constructed with a symbol that
/// is not a recognised runtime-ABI entry.
///
/// Carrying the rejected symbol string lets callers emit diagnostics that
/// name the exact offender (`MirDiagnosticKind::NotYetImplemented`,
/// codegen assertions, unit-test assertions).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnknownRuntimeSymbol(pub String);

impl std::fmt::Display for UnknownRuntimeSymbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "symbol `{}` is not in the MIR runtime-ABI emitter allowlist",
            self.0
        )
    }
}

/// Return `true` if `symbol` is a runtime-ABI entry an
/// `Instr::CallRuntimeAbi` may name.
#[must_use]
pub fn is_known_runtime_symbol(symbol: &str) -> bool {
    MIR_EMITTER_RUNTIME_SYMBOLS
        .iter()
        .any(|symbols| symbols.binary_search(&symbol).is_ok())
}

/// Enumerate every runtime-ABI symbol an `Instr::CallRuntimeAbi` may name,
/// sorted lexicographically for stable diffs.
#[must_use]
pub fn known_runtime_symbols() -> Vec<&'static str> {
    let mut symbols: Vec<&'static str> = MIR_EMITTER_RUNTIME_SYMBOLS
        .iter()
        .flat_map(|symbols| symbols.iter().copied())
        .collect();
    symbols.sort_unstable();
    symbols
}

/// Candidate-scoped receiver-borrow scans that may use a callee's arg[0]
/// exemption.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ReceiverScanSet {
    vec: bool,
    collection: bool,
    bytes: bool,
}

impl ReceiverScanSet {
    /// Owned-Vec local collection scans.
    pub const VEC: Self = Self::new(true, false, false);
    /// HashMap/HashSet/string/bytes collection-handle scans.
    pub const COLLECTION: Self = Self::new(false, true, false);
    /// Bytes triple sole-owner scans.
    pub const BYTES: Self = Self::new(false, false, true);
    /// The polymorphic `hew_vec_len` receiver is observed by Vec and bytes scans.
    pub const VEC_BYTES: Self = Self::new(true, false, true);

    #[must_use]
    pub const fn new(vec: bool, collection: bool, bytes: bool) -> Self {
        Self {
            vec,
            collection,
            bytes,
        }
    }

    #[must_use]
    pub const fn contains_vec(self) -> bool {
        self.vec
    }

    #[must_use]
    pub const fn contains_collection(self) -> bool {
        self.collection
    }

    #[must_use]
    pub const fn contains_bytes(self) -> bool {
        self.bytes
    }
}

/// Ownership contract for the receiver and tail operands of a runtime callee.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ReceiverOwnership {
    /// arg[0] is borrowed in place; arg[1..] still escape.
    BorrowsReceiver { scans: ReceiverScanSet },
    /// Owned-Vec element stores copy in the element. Collection-local scans
    /// exempt every operand; composite binder scans exempt only arg[0].
    VecCopyInElementStore,
    /// Bytes append borrows every unpacked bytes operand.
    BytesAllArgsBorrow,
    /// Every operand escapes.
    Escapes,
}

/// Ownership contract for string operands.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum StringArgsOwnership {
    /// String operands are read or copied; caller keeps the drop obligation.
    BorrowingUse,
    /// Output sinks read the string operand without retaining it.
    PrintSink,
    /// String operands escape.
    Escaping,
}

/// Ownership contract for a callee result.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ResultOwnership {
    /// Fresh or refcount-retained string; caller owes one `hew_string_drop`.
    FreshOwnedString,
    /// Fresh bytes allocation; caller owes the matching bytes release.
    FreshOwnedBytes,
    /// Result is borrowed and carries no caller-owned drop obligation.
    Borrowed,
    /// Result borrows storage owned by arg[0].
    InteriorAliasOfReceiver,
    /// No drop obligation is tracked by this contract.
    Untracked,
}

/// One typed ownership contract per compiler-known callee identity.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CalleeOwnershipContract {
    pub receiver: ReceiverOwnership,
    pub string_args: StringArgsOwnership,
    pub result: ResultOwnership,
}

impl CalleeOwnershipContract {
    pub const FAIL_CLOSED: Self = Self {
        receiver: ReceiverOwnership::Escapes,
        string_args: StringArgsOwnership::Escaping,
        result: ResultOwnership::Untracked,
    };

    #[must_use]
    pub const fn new(
        receiver: ReceiverOwnership,
        string_args: StringArgsOwnership,
        result: ResultOwnership,
    ) -> Self {
        Self {
            receiver,
            string_args,
            result,
        }
    }

    #[must_use]
    pub const fn borrows_vec_receiver(self) -> bool {
        match self.receiver {
            ReceiverOwnership::BorrowsReceiver { scans } => scans.contains_vec(),
            ReceiverOwnership::VecCopyInElementStore => true,
            ReceiverOwnership::BytesAllArgsBorrow | ReceiverOwnership::Escapes => false,
        }
    }

    #[must_use]
    pub const fn borrows_collection_receiver(self) -> bool {
        match self.receiver {
            ReceiverOwnership::BorrowsReceiver { scans } => scans.contains_collection(),
            ReceiverOwnership::VecCopyInElementStore
            | ReceiverOwnership::BytesAllArgsBorrow
            | ReceiverOwnership::Escapes => false,
        }
    }

    #[must_use]
    pub const fn borrows_collection_binder_receiver(self) -> bool {
        self.borrows_vec_receiver() || self.borrows_collection_receiver()
    }

    #[must_use]
    pub const fn borrows_bytes_receiver(self) -> bool {
        match self.receiver {
            ReceiverOwnership::BorrowsReceiver { scans } => scans.contains_bytes(),
            ReceiverOwnership::VecCopyInElementStore
            | ReceiverOwnership::BytesAllArgsBorrow
            | ReceiverOwnership::Escapes => false,
        }
    }

    #[must_use]
    pub const fn is_vec_copy_in_element_store(self) -> bool {
        matches!(self.receiver, ReceiverOwnership::VecCopyInElementStore)
    }

    #[must_use]
    pub const fn borrows_all_bytes_args(self) -> bool {
        matches!(self.receiver, ReceiverOwnership::BytesAllArgsBorrow)
    }

    #[must_use]
    pub const fn borrows_string_use(self) -> bool {
        matches!(self.string_args, StringArgsOwnership::BorrowingUse)
    }

    #[must_use]
    pub const fn borrows_string_call_args(self) -> bool {
        matches!(
            self.string_args,
            StringArgsOwnership::BorrowingUse | StringArgsOwnership::PrintSink
        )
    }

    #[must_use]
    pub const fn produces_fresh_owned_string(self) -> bool {
        matches!(self.result, ResultOwnership::FreshOwnedString)
    }

    #[must_use]
    pub const fn produces_fresh_owned_bytes(self) -> bool {
        matches!(self.result, ResultOwnership::FreshOwnedBytes)
    }

    #[must_use]
    pub const fn returns_receiver_interior_alias(self) -> bool {
        matches!(
            self.result,
            ResultOwnership::Borrowed | ResultOwnership::InteriorAliasOfReceiver
        )
    }
}

impl Default for CalleeOwnershipContract {
    fn default() -> Self {
        Self::FAIL_CLOSED
    }
}

/// Return the ownership contract for a runtime callee spelling.
///
/// Unknown callees are explicit fail-closed contracts: operands escape and the
/// result carries no tracked drop obligation.
#[expect(
    clippy::too_many_lines,
    reason = "single flat positive-membership symbol table; one authority, never split"
)]
#[must_use]
pub fn callee_ownership_contract(callee: &str) -> CalleeOwnershipContract {
    use ReceiverOwnership::{BorrowsReceiver, BytesAllArgsBorrow, Escapes, VecCopyInElementStore};
    use ResultOwnership::{Borrowed, FreshOwnedBytes, FreshOwnedString, Untracked};
    use StringArgsOwnership::{BorrowingUse, Escaping, PrintSink};

    match callee {
        // Bytes append borrows the receiver and the unpacked source triple.
        "hew_bytes_append" => CalleeOwnershipContract::new(BytesAllArgsBorrow, Escaping, Untracked),

        // Bytes receiver reads and in-place mutations leave arg[0] owned by the
        // caller and hand back no tracked result.
        "hew_bytes_clear" | "hew_bytes_contains" | "hew_bytes_index" | "hew_bytes_is_empty"
        | "hew_bytes_len" | "hew_bytes_pop" | "hew_bytes_push" | "hew_bytes_set" => {
            CalleeOwnershipContract::new(
                BorrowsReceiver {
                    scans: ReceiverScanSet::BYTES,
                },
                Escaping,
                Untracked,
            )
        }

        // `hew_bytes_to_string` borrows its bytes-triple arg[0] (the source
        // buffer stays owned by the caller) but its RESULT is a fresh, header-
        // aware `+1` string: `alloc_cstring_from_str` allocates on BOTH the
        // empty and non-empty paths (hew-runtime/src/bytes.rs), and the result
        // reaches `hew_string_drop`/`free_cstring`. So it is a fresh-owned-string
        // producer, identical in ownership shape to the `Vec<string>` getter
        // `hew_vec_get_str`: the caller owes exactly one balancing drop. The MIR
        // `read_string` lowering (`await conn.read_string()`, non-deadline path)
        // emits this call with a string destination; the deadline path converts
        // codegen-side and SKIPS this call, so the MIR result is always a genuine
        // sole owner with no other drop site (no double-free risk).
        "hew_bytes_to_string" => CalleeOwnershipContract::new(
            BorrowsReceiver {
                scans: ReceiverScanSet::BYTES,
            },
            Escaping,
            FreshOwnedString,
        ),

        // `hew_bytes_slice(ptr, offset, len, start, end) -> BytesTriple`
        // borrows its receiver for the scan but hands back a fresh rc==1
        // handle: the non-empty path bumps the shared buffer's refcount
        // (`hew_bytes_clone_ref`) so the slice owner drops independently, and
        // the empty path returns a null/0/0 triple whose `hew_bytes_drop` is a
        // no-op. Either way the caller owes exactly one bytes release, so the
        // result is `FreshOwnedBytes` — this is what lets a transient
        // `b.slice(..).len()` temp earn its drop instead of leaking.
        "hew_bytes_slice" => CalleeOwnershipContract::new(
            BorrowsReceiver {
                scans: ReceiverScanSet::BYTES,
            },
            Escaping,
            FreshOwnedBytes,
        ),

        // The polymorphic Vec length symbol is a receiver borrow for both the
        // Vec-local and bytes-local scans.
        "hew_vec_len" => CalleeOwnershipContract::new(
            BorrowsReceiver {
                scans: ReceiverScanSet::VEC_BYTES,
            },
            Escaping,
            Untracked,
        ),

        // Collection receiver reads and in-place mutations borrow arg[0]; tail
        // operands remain ordinary escapes.
        "hew_bytes_get"
        | "hew_hashmap_clear_layout"
        | "hew_hashmap_clone_layout"
        | "hew_hashmap_contains_key_layout"
        | "hew_hashmap_get_clone_layout"
        | "hew_hashmap_get_layout"
        | "hew_hashmap_insert_layout"
        | "hew_hashmap_keys_layout"
        | "hew_hashmap_len_layout"
        | "hew_hashmap_remove_layout"
        | "hew_hashmap_remove_take_layout"
        | "hew_hashmap_values_layout"
        | "hew_hashset_clear_layout"
        | "hew_hashset_clone_layout"
        | "hew_hashset_contains_layout"
        | "hew_hashset_insert_layout"
        | "hew_hashset_is_empty_layout"
        | "hew_hashset_len_layout"
        | "hew_hashset_remove_layout"
        | "hew_hashset_to_vec_layout"
        | "hew_string_get" => CalleeOwnershipContract::new(
            BorrowsReceiver {
                scans: ReceiverScanSet::COLLECTION,
            },
            Escaping,
            Untracked,
        ),

        // Owned-Vec copy-in stores clone the element into the destination slot.
        "hew_vec_push_owned" | "hew_vec_set_owned" => {
            CalleeOwnershipContract::new(VecCopyInElementStore, Escaping, Untracked)
        }

        // Vec string element stores borrow the receiver and copy the string
        // argument; the caller keeps the string drop obligation.
        "hew_vec_push_str" | "hew_vec_set_str" => CalleeOwnershipContract::new(
            BorrowsReceiver {
                scans: ReceiverScanSet::VEC,
            },
            BorrowingUse,
            Untracked,
        ),

        // Vec string element handoffs — the getter (`get`, clone-out) and the
        // move-out ops (`pop`/`remove`) all hand the caller a +1 owned string
        // that must be balanced with exactly one `hew_string_drop`. The getter
        // retains a clone (the vec keeps its copy); the move-out ops transfer
        // the sole owner (the element leaves the vec via pop's length decrement
        // or remove's tail shift, so there is no double owner). Both shapes
        // share the identical ownership contract — the caller owes one drop.
        // (Scalar/ptr `pop`/`remove` classes stay `Untracked`: no heap to drop.)
        "hew_vec_get_str" | "hew_vec_pop_str" | "hew_vec_remove_at_str" => {
            CalleeOwnershipContract::new(
                BorrowsReceiver {
                    scans: ReceiverScanSet::VEC,
                },
                Escaping,
                FreshOwnedString,
            )
        }

        // Vec owned-element getters return aliases into the receiver storage.
        "hew_vec_get_owned" | "hew_vec_get_ptr" => CalleeOwnershipContract::new(
            BorrowsReceiver {
                scans: ReceiverScanSet::VEC,
            },
            Escaping,
            Borrowed,
        ),

        // Vec receivers are borrowed in place; element and range operands keep
        // the default escaping behaviour unless a narrower row above applies.
        "hew_vec_append"
        | "hew_vec_append_layout"
        | "hew_vec_clear"
        | "hew_vec_clear_layout"
        | "hew_vec_clone"
        | "hew_vec_clone_layout"
        | "hew_vec_clone_owned"
        | "hew_vec_contains_f64"
        | "hew_vec_contains_i32"
        | "hew_vec_contains_i64"
        | "hew_vec_contains_owned"
        | "hew_vec_contains_str"
        | "hew_vec_contains_thunk"
        | "hew_vec_get_bool"
        | "hew_vec_get_clone"
        | "hew_vec_get_f32"
        | "hew_vec_get_f64"
        | "hew_vec_get_i16"
        | "hew_vec_get_i32"
        | "hew_vec_get_i64"
        | "hew_vec_get_i8"
        | "hew_vec_get_layout"
        | "hew_vec_get_u16"
        | "hew_vec_get_u8"
        | "hew_vec_is_empty"
        | "hew_vec_join_str"
        | "hew_vec_pop_bool"
        | "hew_vec_pop_f32"
        | "hew_vec_pop_f64"
        | "hew_vec_pop_i16"
        | "hew_vec_pop_i32"
        | "hew_vec_pop_i64"
        | "hew_vec_pop_i8"
        | "hew_vec_pop_layout"
        | "hew_vec_pop_owned"
        | "hew_vec_pop_ptr"
        | "hew_vec_pop_u16"
        | "hew_vec_pop_u8"
        | "hew_vec_push_bool"
        | "hew_vec_push_f32"
        | "hew_vec_push_f64"
        | "hew_vec_push_i16"
        | "hew_vec_push_i32"
        | "hew_vec_push_i64"
        | "hew_vec_push_i8"
        | "hew_vec_push_layout"
        | "hew_vec_push_owned_move"
        | "hew_vec_push_ptr"
        | "hew_vec_push_u16"
        | "hew_vec_push_u8"
        | "hew_vec_remove_at_bool"
        | "hew_vec_remove_at_f32"
        | "hew_vec_remove_at_f64"
        | "hew_vec_remove_at_i16"
        | "hew_vec_remove_at_i32"
        | "hew_vec_remove_at_i64"
        | "hew_vec_remove_at_i8"
        | "hew_vec_remove_at_layout"
        | "hew_vec_remove_at_owned"
        | "hew_vec_remove_at_ptr"
        | "hew_vec_remove_at_u16"
        | "hew_vec_remove_at_u8"
        | "hew_vec_set_bool"
        | "hew_vec_set_f32"
        | "hew_vec_set_f64"
        | "hew_vec_set_i16"
        | "hew_vec_set_i32"
        | "hew_vec_set_i64"
        | "hew_vec_set_i8"
        | "hew_vec_set_layout"
        | "hew_vec_set_ptr"
        | "hew_vec_set_u16"
        | "hew_vec_set_u8"
        | "hew_vec_slice_range_bytesize"
        | "hew_vec_slice_range_f64"
        | "hew_vec_slice_range_i32"
        | "hew_vec_slice_range_i64"
        | "hew_vec_slice_range_layout"
        | "hew_vec_slice_range_owned"
        | "hew_vec_slice_range_ptr"
        | "hew_vec_slice_range_str" => CalleeOwnershipContract::new(
            BorrowsReceiver {
                scans: ReceiverScanSet::VEC,
            },
            Escaping,
            Untracked,
        ),

        // String transforms borrow their inputs and produce a fresh or +1-owned
        // string result. `hew-runtime/src/string.rs` either allocates a new
        // refcounted buffer at rc=1 or bumps an existing string's refcount; the
        // caller owns exactly one balancing `hew_string_drop`.
        //
        // `string_concat` (no `hew_` prefix) is the `stdlib_catalog` presentation
        // name f-string interpolation lowering calls through (`build_catalog_call`,
        // `hew-hir/src/lower.rs::lower_interpolated_string`) — it reaches MIR as
        // a `Terminator::Call { callee: "string_concat", .. }`, never rewritten to
        // the `hew_string_concat` c-symbol before this lookup (that rewrite is a
        // codegen-time `BuiltinLinkage::RuntimeFfiShim` concern, per
        // `hew-hir/src/stdlib_catalog.rs`). Every sibling catalog presentation
        // name reachable as a literal MIR callee (`to_string_i64`, `println_str`,
        // …) is already dual-listed against its c-symbol below; `string_concat`
        // was the one missing entry, so both the concat call's own fresh-owned
        // result AND the `to_string_*` temp feeding it as a borrowed argument
        // fell through to `FAIL_CLOSED` (`Untracked` / `Escaping`) and leaked —
        // `collect_nested_fresh_string_temp_drops` never admitted either. The
        // runtime behaviour is byte-identical to `hew_string_concat` (the shim
        // calls it directly), so the contract must be too.
        "hew_string_clone"
        | "hew_string_concat"
        | "hew_string_repeat"
        | "hew_string_replace"
        | "hew_string_slice"
        | "hew_string_slice_codepoints"
        | "hew_string_to_lowercase"
        | "hew_string_to_uppercase"
        | "hew_string_trim"
        | "string_concat" => CalleeOwnershipContract::new(Escapes, BorrowingUse, FreshOwnedString),

        // String inspectors and container producers borrow their string input
        // without handing back a tracked string result. Scalar/bytes/Vec
        // inspectors read only; `hew_vec_push_str` stores an independent string
        // copy so the caller keeps the source string's drop obligation.
        "hew_string_char_at"
        | "hew_string_char_at_utf8"
        | "hew_string_char_count"
        | "hew_string_chars"
        | "hew_string_contains"
        | "hew_string_ends_with"
        | "hew_string_find"
        | "hew_string_index"
        | "hew_string_is_alpha"
        | "hew_string_is_alphanumeric"
        | "hew_string_is_digit"
        | "hew_string_is_empty"
        | "hew_string_length"
        | "hew_string_lines"
        | "hew_string_split"
        | "hew_string_starts_with"
        | "hew_string_to_bytes" => CalleeOwnershipContract::new(Escapes, BorrowingUse, Untracked),

        // Scalar and catalog display producers allocate a fresh string result.
        // The `to_string_*` catalog spellings are the MIR presentation for
        // f-string display dispatch and map to the `hew_*_to_string` runtime
        // allocation entries.
        "hew_bool_to_string"
        | "hew_char_to_string"
        | "hew_float_to_string"
        | "hew_i64_to_string"
        | "hew_int_to_string"
        | "hew_node_api_identity_key"
        | "hew_string_from_char"
        | "hew_u64_to_string"
        | "hew_uint_to_string"
        | "to_string_bool"
        | "to_string_char"
        | "to_string_f64"
        | "to_string_i32"
        | "to_string_i64"
        | "to_string_u16"
        | "to_string_u32"
        | "to_string_u64"
        | "to_string_u8" => CalleeOwnershipContract::new(Escapes, Escaping, FreshOwnedString),

        // Print sinks borrow their string operands for output.
        "print" | "print_str" | "println" | "println_str" => {
            CalleeOwnershipContract::new(Escapes, PrintSink, Untracked)
        }

        _ => CalleeOwnershipContract::FAIL_CLOSED,
    }
}

#[cfg(test)]
const TOML_RESULT_CONSISTENCY: &[(&str, &str, ResultOwnership)] = &[
    (
        "hew_bytes_to_string",
        "fresh",
        ResultOwnership::FreshOwnedString,
    ),
    ("hew_vec_get_clone", "fresh", ResultOwnership::Untracked),
    ("hew_vec_get_owned", "borrowed", ResultOwnership::Borrowed),
    (
        "hew_vec_get_str",
        "retained",
        ResultOwnership::FreshOwnedString,
    ),
];

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeSet;

    const CONTRACT_SYMBOLS: &[&str] = &[
        "hew_bool_to_string",
        "hew_bytes_append",
        "hew_bytes_clear",
        "hew_bytes_contains",
        "hew_bytes_get",
        "hew_bytes_index",
        "hew_bytes_is_empty",
        "hew_bytes_len",
        "hew_bytes_pop",
        "hew_bytes_push",
        "hew_bytes_set",
        "hew_bytes_slice",
        "hew_bytes_to_string",
        "hew_char_to_string",
        "hew_float_to_string",
        "hew_hashmap_clear_layout",
        "hew_hashmap_clone_layout",
        "hew_hashmap_contains_key_layout",
        "hew_hashmap_get_clone_layout",
        "hew_hashmap_get_layout",
        "hew_hashmap_insert_layout",
        "hew_hashmap_keys_layout",
        "hew_hashmap_len_layout",
        "hew_hashmap_remove_layout",
        "hew_hashmap_remove_take_layout",
        "hew_hashmap_values_layout",
        "hew_hashset_clear_layout",
        "hew_hashset_clone_layout",
        "hew_hashset_contains_layout",
        "hew_hashset_insert_layout",
        "hew_hashset_is_empty_layout",
        "hew_hashset_len_layout",
        "hew_hashset_remove_layout",
        "hew_hashset_to_vec_layout",
        "hew_i64_to_string",
        "hew_int_to_string",
        "hew_node_api_identity_key",
        "hew_string_char_at",
        "hew_string_char_at_utf8",
        "hew_string_char_count",
        "hew_string_chars",
        "hew_string_clone",
        "hew_string_concat",
        "hew_string_contains",
        "hew_string_ends_with",
        "hew_string_find",
        "hew_string_from_char",
        "hew_string_get",
        "hew_string_index",
        "hew_string_is_alpha",
        "hew_string_is_alphanumeric",
        "hew_string_is_digit",
        "hew_string_is_empty",
        "hew_string_length",
        "hew_string_lines",
        "hew_string_repeat",
        "hew_string_replace",
        "hew_string_slice",
        "hew_string_slice_codepoints",
        "hew_string_split",
        "hew_string_starts_with",
        "hew_string_to_bytes",
        "hew_string_to_lowercase",
        "hew_string_to_uppercase",
        "hew_string_trim",
        "hew_u64_to_string",
        "hew_uint_to_string",
        "hew_vec_append",
        "hew_vec_append_layout",
        "hew_vec_clear",
        "hew_vec_clear_layout",
        "hew_vec_clone",
        "hew_vec_clone_layout",
        "hew_vec_clone_owned",
        "hew_vec_contains_f64",
        "hew_vec_contains_i32",
        "hew_vec_contains_i64",
        "hew_vec_contains_owned",
        "hew_vec_contains_str",
        "hew_vec_contains_thunk",
        "hew_vec_get_bool",
        "hew_vec_get_clone",
        "hew_vec_get_f32",
        "hew_vec_get_f64",
        "hew_vec_get_i16",
        "hew_vec_get_i32",
        "hew_vec_get_i64",
        "hew_vec_get_i8",
        "hew_vec_get_layout",
        "hew_vec_get_owned",
        "hew_vec_get_ptr",
        "hew_vec_get_str",
        "hew_vec_get_u16",
        "hew_vec_get_u8",
        "hew_vec_is_empty",
        "hew_vec_join_str",
        "hew_vec_len",
        "hew_vec_pop_bool",
        "hew_vec_pop_f32",
        "hew_vec_pop_f64",
        "hew_vec_pop_i16",
        "hew_vec_pop_i32",
        "hew_vec_pop_i64",
        "hew_vec_pop_i8",
        "hew_vec_pop_layout",
        "hew_vec_pop_owned",
        "hew_vec_pop_ptr",
        "hew_vec_pop_str",
        "hew_vec_pop_u16",
        "hew_vec_pop_u8",
        "hew_vec_push_bool",
        "hew_vec_push_f32",
        "hew_vec_push_f64",
        "hew_vec_push_i16",
        "hew_vec_push_i32",
        "hew_vec_push_i64",
        "hew_vec_push_i8",
        "hew_vec_push_layout",
        "hew_vec_push_owned",
        "hew_vec_push_owned_move",
        "hew_vec_push_ptr",
        "hew_vec_push_str",
        "hew_vec_push_u16",
        "hew_vec_push_u8",
        "hew_vec_remove_at_bool",
        "hew_vec_remove_at_f32",
        "hew_vec_remove_at_f64",
        "hew_vec_remove_at_i16",
        "hew_vec_remove_at_i32",
        "hew_vec_remove_at_i64",
        "hew_vec_remove_at_i8",
        "hew_vec_remove_at_layout",
        "hew_vec_remove_at_owned",
        "hew_vec_remove_at_ptr",
        "hew_vec_remove_at_str",
        "hew_vec_remove_at_u16",
        "hew_vec_remove_at_u8",
        "hew_vec_set_bool",
        "hew_vec_set_f32",
        "hew_vec_set_f64",
        "hew_vec_set_i16",
        "hew_vec_set_i32",
        "hew_vec_set_i64",
        "hew_vec_set_i8",
        "hew_vec_set_layout",
        "hew_vec_set_owned",
        "hew_vec_set_ptr",
        "hew_vec_set_str",
        "hew_vec_set_u16",
        "hew_vec_set_u8",
        "hew_vec_slice_range_bytesize",
        "hew_vec_slice_range_f64",
        "hew_vec_slice_range_i32",
        "hew_vec_slice_range_i64",
        "hew_vec_slice_range_layout",
        "hew_vec_slice_range_owned",
        "hew_vec_slice_range_ptr",
        "hew_vec_slice_range_str",
        "print",
        "print_str",
        "println",
        "println_str",
        "to_string_bool",
        "to_string_char",
        "to_string_f64",
        "to_string_i32",
        "to_string_i64",
        "to_string_u16",
        "to_string_u32",
        "to_string_u64",
        "to_string_u8",
    ];

    #[test]
    fn known_substrate_symbols_recognised() {
        // Every explicitly allowlisted symbol must round-trip through the
        // admission predicate. `known_runtime_symbols()` is sorted, so this
        // also pins the stable-diff ordering.
        let listed = known_runtime_symbols();
        assert!(listed.windows(2).all(|w| w[0] < w[1]), "not sorted/unique");
        for sym in listed {
            assert!(
                is_known_runtime_symbol(sym),
                "allowlisted symbol {sym} should be recognised",
            );
        }
    }

    #[test]
    fn pre_staged_families_are_not_admitted() {
        // The admission boundary IS the routing boundary: every family maps
        // to exactly one admission verdict, and a pre-staged family (routed
        // via `Terminator::Call`) must never be admitted onto the
        // `Instr::CallRuntimeAbi` path — admitting it would flip
        // `runtime_symbol_for_call_expr`'s producer routing.
        for family in all_runtime_call_families() {
            let admitted = is_known_runtime_symbol(family.c_symbol());
            assert_eq!(
                admitted,
                !is_pre_staged_family(family),
                "family {family:?} ({}) admission disagrees with its routing class",
                family.c_symbol(),
            );
        }
    }

    #[test]
    fn unknown_symbol_rejected() {
        // A symbol the substrate does not emit must NOT be
        // recognised — typo-class bugs need to fail closed.
        assert!(!is_known_runtime_symbol("hew_duplex_sned"));
        assert!(!is_known_runtime_symbol("hew_duplex_send_t"));
        assert!(!is_known_runtime_symbol(""));
        assert!(!is_known_runtime_symbol("printf"));
    }

    #[test]
    fn substrate_quartet_present() {
        // Spot-check the four most load-bearing entries.
        // (These are the symbols a slice-5 lowering of Duplex
        // send/recv/close/pair lands first.)
        assert!(is_known_runtime_symbol("hew_duplex_pair"));
        assert!(is_known_runtime_symbol("hew_duplex_send"));
        assert!(is_known_runtime_symbol("hew_duplex_recv"));
        assert!(is_known_runtime_symbol("hew_duplex_close"));
    }

    #[test]
    fn callee_ownership_contract_symbols_are_unique_positive_rows() {
        let unique = CONTRACT_SYMBOLS.iter().copied().collect::<BTreeSet<_>>();
        assert_eq!(CONTRACT_SYMBOLS.len(), 171);
        assert_eq!(
            unique.len(),
            CONTRACT_SYMBOLS.len(),
            "contract symbol inventory contains duplicates"
        );
        for symbol in CONTRACT_SYMBOLS {
            assert_ne!(
                callee_ownership_contract(symbol),
                CalleeOwnershipContract::FAIL_CLOSED,
                "{symbol} should have a positive ownership contract row",
            );
        }
    }

    #[test]
    fn unknown_callee_ownership_contract_fails_closed() {
        assert_eq!(
            callee_ownership_contract("hew_nope"),
            CalleeOwnershipContract::FAIL_CLOSED
        );
    }

    #[test]
    fn toml_checked_result_contracts_match_hand_table() {
        for (symbol, _toml_result, expected) in TOML_RESULT_CONSISTENCY {
            assert_eq!(
                callee_ownership_contract(symbol).result,
                *expected,
                "{symbol} diverges from its TOML-checked result contract",
            );
        }
    }

    #[test]
    fn hew_bytes_to_string_is_a_fresh_owned_string_producer_that_borrows_its_source() {
        // Issue #2354/#2332: `hew_bytes_to_string` allocates a fresh header-aware
        // string on BOTH the empty and non-empty paths
        // (hew-runtime/src/bytes.rs `alloc_cstring_from_str`), so its RESULT is a
        // `+1` owner the caller must release exactly once — never zero (leak),
        // never twice (double-free). The MIR `read_string` lowering
        // (`await conn.read_string()`, non-deadline path) emits this call with a
        // string destination and no other drop site, so the widened admission is
        // sound in both directions.
        let contract = callee_ownership_contract("hew_bytes_to_string");
        assert!(
            contract.produces_fresh_owned_string(),
            "hew_bytes_to_string must be classified as a fresh-owned-string producer",
        );
        // The SOURCE bytes-triple arg[0] is only borrowed (the caller keeps the
        // buffer's drop obligation): the fresh RESULT and the borrowed SOURCE are
        // independent, so admitting the result never double-accounts the input.
        assert!(
            contract.borrows_bytes_receiver(),
            "hew_bytes_to_string must still borrow its bytes-triple source arg",
        );
        assert_eq!(contract.result, ResultOwnership::FreshOwnedString);
    }

    #[test]
    fn borrow_marked_contracts_do_not_consume_catalogued_receivers() {
        // The runtime-call catalog covers only its closed subset of callee
        // spellings. Within that subset, no consuming receiver may be marked as
        // borrowed by this contract.
        for symbol in CONTRACT_SYMBOLS {
            let Some(family) = RuntimeCallFamily::from_c_symbol(symbol) else {
                continue;
            };
            assert!(
                !family.consumes_receiver()
                    || matches!(
                        callee_ownership_contract(symbol).receiver,
                        ReceiverOwnership::Escapes
                    ),
                "{symbol} consumes its receiver and must not be borrow-marked",
            );
        }
    }

    #[test]
    fn release_spelling_runtime_symbols_fail_closed() {
        // Spelling-based tripwire for the emitter surface outside the closed
        // runtime-call catalog: release-like names default to no borrow/result
        // ownership contract unless deliberately classified elsewhere.
        let release_symbols = known_runtime_symbols()
            .into_iter()
            .filter(|symbol| {
                ["drop", "free", "close", "release", "destroy", "dispose"]
                    .iter()
                    .any(|needle| symbol.contains(needle))
            })
            .collect::<Vec<_>>();
        assert_eq!(
            release_symbols,
            vec![
                "hew_auto_mutex_free",
                "hew_cancel_token_release",
                "hew_duplex_close",
                "hew_duplex_close_half",
                "hew_duplex_payload_free",
                "hew_dyn_box_free",
                "hew_hashmap_free_layout",
                "hew_hashset_free_layout",
                "hew_lambda_actor_release",
                "hew_lambda_actor_weak_drop",
                "hew_regex_free_capture",
                "hew_reply_channel_free",
                "hew_reply_payload_free",
                "hew_task_free",
                "hew_task_scope_destroy",
            ],
        );
        for symbol in release_symbols {
            assert_eq!(
                callee_ownership_contract(symbol),
                CalleeOwnershipContract::FAIL_CLOSED,
                "{symbol} should keep the fail-closed ownership contract",
            );
        }
    }

    #[test]
    fn task_abi_symbols_present() {
        // Phase 2 substrate for scope{}/spawn/await (inventory rows 2/3/4).
        // Each of these must be recognised before the MIR producer arms can
        // be wired in lower.rs. The canonical structured-concurrency surface
        // is `hew_task_scope_*` (W2.006); legacy `hew_scope_*` has been
        // removed.
        assert!(is_known_runtime_symbol("hew_task_scope_spawn"));
        assert!(is_known_runtime_symbol("hew_task_new"));
        assert!(is_known_runtime_symbol("hew_task_spawn_thread"));
        assert!(is_known_runtime_symbol("hew_task_await_blocking"));
        assert!(is_known_runtime_symbol("hew_task_get_result"));
        assert!(is_known_runtime_symbol("hew_task_free"));
    }
}
