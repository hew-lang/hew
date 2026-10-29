#!/usr/bin/env python3
"""Require a counterexample for one implementation defect per new guarantee."""

from pathlib import Path
import argparse
import shutil
import subprocess
import tempfile
from run import command

ROOT = Path(__file__).resolve().parents[2]
PROOFS = Path(__file__).resolve().parent
# name, feature, source, defect, replacement, optional harness filter
MUTATIONS = [
    (
        "one_shot_custody",
        "one_shot",
        "timer_actor/protocol.rs",
        "self.phase = Phase::Fired;",
        "self.phase = Phase::Scheduled;",
        "one_shot_inductive_custody",
    ),
    (
        "one_shot_duplicate_start",
        "one_shot",
        "timer_actor/protocol.rs",
        "Phase::Scheduled | Phase::Started | Phase::Cancelled | Phase::Overdue => false,",
        "Phase::Started => true, Phase::Scheduled | Phase::Cancelled | Phase::Overdue => false,",
        "one_shot_inductive_custody",
    ),
    (
        "one_shot_reopen",
        "one_shot",
        "timer_actor/protocol.rs",
        "Phase::Fired => Phase::Disarmed,",
        "Phase::Fired | Phase::Started => Phase::Disarmed,",
        "one_shot_inductive_custody",
    ),
    (
        "one_shot_lost_fault",
        "one_shot",
        "timer_actor/protocol.rs",
        "self.0.discarded();",
        "// Deliberately lose the unconsumed delivery claim.",
        "one_shot_firing_cancel",
    ),
    (
        "periodic_initial_publish",
        "periodic_loom",
        "timer_periodic/entry.rs",
        "    let wheel_handle = unsafe {\n        timer_wheel_schedule_precise_handle(\n            tw,\n            crate::timer_deadline::Deadline::from_nanos(next_fire_ns),\n            periodic_timer_cb,\n            data,\n        )\n    };",
        "    drop(pending);\n    let wheel_handle = unsafe {\n        timer_wheel_schedule_precise_handle(\n            tw,\n            crate::timer_deadline::Deadline::from_nanos(next_fire_ns),\n            periodic_timer_cb,\n            data,\n        )\n    };\n    pending = ctx.pending.lock().unwrap_or_else(std::sync::PoisonError::into_inner);",
        "initial_fire",
    ),
    (
        "periodic_rearm_cancel",
        "periodic_loom",
        "timer_periodic/entry.rs",
        "if ctx.cancelled.load(Ordering::SeqCst) {\n        drop(pending);",
        "if false && ctx.cancelled.load(Ordering::SeqCst) {\n        drop(pending);",
        "cancel_fire",
    ),
    (
        "periodic_flight_claim",
        "periodic_loom",
        "timer_periodic/entry.rs",
        "ctx.in_flight.store(true, Ordering::SeqCst);",
        "// Deliberately fail to publish callback actor access.",
        "cancel_fire",
    ),
    (
        "periodic_flight_drain",
        "periodic_loom",
        "timer_periodic/entry.rs",
        "while self.in_flight.load(Ordering::SeqCst) {",
        "while false && self.in_flight.load(Ordering::SeqCst) {",
        "cancel_fire",
    ),
    (
        "periodic_cancel_publication",
        "periodic_loom",
        "timer_periodic/entry.rs",
        "self.cancelled.store(true, Ordering::SeqCst);",
        "self.cancelled.store(false, Ordering::SeqCst);",
        "cancel_fire",
    ),
    (
        "periodic_context_release",
        "periodic_loom",
        "timer_periodic/entry.rs",
        "            drop(unsafe { Arc::from_raw(data.cast::<PeriodicCtx>()) });",
        "            let _ = data; // Deliberately retain the wheel reference.",
        "double_cancel",
    ),
    (
        "periodic_skip_slots",
        "periodic_loom",
        "timer_periodic/entry.rs",
        "if next_fire_ns < cursor_ns {",
        "if false && next_fire_ns < cursor_ns {",
        "late_phase",
    ),
    (
        "periodic_phase",
        "periodic_loom",
        "timer_periodic/entry.rs",
        "next_fire_ns = next_fire_ns.saturating_add(ctx.interval_ns.saturating_mul(skipped_slots));",
        "next_fire_ns = cursor_ns.saturating_add(ctx.interval_ns);",
        "late_phase",
    ),
    (
        "reactor_wake",
        "reactor_loom",
        "reactor_wait.rs",
        "        wake();",
        "        drop(wake);",
        "earlier_insert",
    ),
    (
        "reactor_recheck",
        "reactor_loom",
        "reactor_wait.rs",
        "let again = driver.next_deadline();",
        "let again = None;",
        "empty_insert",
    ),
    (
        "reactor_saturation",
        "reactor_loom",
        "reactor_wait.rs",
        " || limit == u64::MAX - 1",
        "",
        "saturated_insert",
    ),
    (
        "reactor_idle_spin",
        "reactor_loom",
        "reactor_wait.rs",
        "return deadline.map(|d| d.remaining(driver.now()));",
        "return Some(deadline.map_or(Duration::ZERO, |d| d.remaining(driver.now())));",
        "idle_block",
    ),
    (
        "reactor_rounding",
        "reactor_loom",
        "reactor_wait.rs",
        "d.remaining(driver.now())",
        "Duration::from_millis(d.remaining(driver.now()).as_millis() as u64)",
        "exact_wait",
    ),
    (
        "reactor_shutdown_claim",
        "reactor_loom",
        "reactor_wait.rs",
        "if !driver.stopping() {",
        "if true {",
        "stop",
    ),
    (
        "reactor_retry_errors",
        "reactor_loom",
        "reactor_wait.rs",
        "error.kind() == io::ErrorKind::Interrupted",
        "true",
        "poll_error",
    ),
    (
        "reactor_fatal_interrupt",
        "reactor_loom",
        "reactor_wait.rs",
        "error.kind() == io::ErrorKind::Interrupted",
        "false",
        "interrupt",
    ),
    (
        "reactor_awake",
        "reactor_loom",
        "reactor_wait.rs",
        "sleep_until.store(AWAKE, Ordering::SeqCst);",
        "// Deliberately leave the old park publication.",
        "idle_block",
    ),
    (
        "reactor_publication",
        "reactor_loom",
        "reactor_wait.rs",
        "sleep_until.store(sleep_limit(deadline), Ordering::SeqCst);",
        "sleep_until.store(AWAKE, Ordering::SeqCst);",
        "earlier_insert",
    ),
    (
        "reactor_insert_hook",
        "reactor_loom",
        "timer_wheel.rs",
        "notify_inserted(tw, deadline.saturated_nanos().min(u64::MAX - 1));",
        "// Deliberately omit the process-wheel insert hook.",
        "earlier_insert",
    ),
    (
        "deadline_rounding",
        "failure_properties",
        "timer_deadline.rs",
        "fraction_ns: (fraction % 1_000_000) as u32",
        "fraction_ns: 0",
        "generated_deadlines",
    ),
    (
        "deadline_overflow",
        "failure_properties",
        "timer_deadline.rs",
        "duration.as_secs().checked_mul(1_000)?",
        "duration.as_secs().wrapping_mul(1_000)",
        "generated_deadlines",
    ),
    (
        "fault_shutdown_gap",
        "fault_loom",
        "exit_status/state.rs",
        "self.publication_in_flight() || self.open_count() > 0",
        "self.open_count() > 0 || self.publication_in_flight()",
        None,
    ),
    (
        "channel_lost_message",
        "channel_loom",
        "channel_core.rs",
        "inner.queue.push_back(envelope.into());",
        "drop(envelope);",
        "handoff",
    ),
    (
        "channel_capacity",
        "channel_loom",
        "channel_core.rs",
        "if inner.queue.len() >= inner.capacity {\n                inner",
        "if false && inner.queue.len() >= inner.capacity {\n                inner",
        "backpressure",
    ),
    (
        "channel_cancel_release",
        "channel_loom",
        "channel_core.rs",
        "if let Some(item) = w.item.take() {\n                Self::drop_envelope(layout.as_ref(), item);\n            }\n        }\n    }\n\n    // ── Close edges",
        "if let Some(item) = w.item.take() {\n                drop(item);\n            }\n        }\n    }\n\n    // ── Close edges",
        "detach_promotion",
    ),
    (
        "channel_fault_eof",
        "channel_loom",
        "channel_core.rs",
        "None if inner.sink_fault => return (3, None),",
        "None if inner.sink_fault => return (2, None),",
        "fault_drain",
    ),
    (
        "channel_close_wake",
        "channel_loom",
        "channel_core.rs",
        "Self::wake_native(native_producers);\n        for w in wakes",
        "drop(native_producers);\n        for w in wakes",
        "close_send",
    ),
    (
        "channel_premature_eof",
        "channel_loom",
        "channel_core.rs",
        "if inner.sink_handles > 0 {",
        "if false && inner.sink_handles > 0 {",
        "last_sink",
    ),
    (
        "channel_observer",
        "channel_loom",
        "channel_core.rs",
        "let previous = inner.native_consumer.replace(NativeRegistration::Observe({",
        "let previous = Some(NativeRegistration::Observe({",
        "observation",
    ),
    (
        "fault_id_wrap",
        "fault_records",
        "exit_status/state.rs",
        "        let Ok(id) = self\n            .next_id\n            .fetch_update(Ordering::AcqRel, Ordering::Acquire, |id| {\n                (id != 0).then(|| id.checked_add(1)).flatten()\n            })\n        else {\n            self.record_unrecovered();\n            return FaultRecord::NONE;\n        };\n",
        "        let id = self.next_id.fetch_add(1, Ordering::AcqRel);\n",
        "fault_record_ids",
    ),
    (
        "fault_provisional_recovery",
        "fault_records",
        "exit_status/state.rs",
        "matches!(self, Self::Handled | Self::Unrecovered)",
        "true",
        "fault_rulings_preserve",
    ),
    (
        "fault_table_overflow",
        "fault_records",
        "exit_status/state.rs",
        "        self.record_unrecovered();\n        FaultRecord::NONE",
        "        FaultRecord::NONE",
        "fault_table_exhaustion",
    ),
    (
        "fault_publication_gap",
        "fault_loom",
        "exit_status/state.rs",
        "self.publication_in_flight()\n            || self.terminal.load(Ordering::Acquire)\n            || self.open_count() > 0",
        "self.terminal.load(Ordering::Acquire)\n            || self.open_count() > 0\n            || self.publication_in_flight()",
        None,
    ),
    (
        "fault_terminal_erased",
        "fault_loom",
        "exit_status/state.rs",
        "if !record.is_some() || !ruling.settles() {",
        "if ruling == FaultRuling::Handled { self.terminal.store(false, Ordering::Release); }\n        if !record.is_some() || !ruling.settles() {",
        None,
    ),
    (
        "restart_budget_fixed_32",
        "restart",
        "supervisor/restart_history.rs",
        "        while self.0.len() >= limit {",
        "        let limit = limit.min(32);\n        while self.0.len() >= limit {",
        "restart_budget_above_32",
    ),
    (
        "restart_spent_resurrection",
        "failure_properties",
        "supervisor/restart_policy.rs",
        ".filter_map(|(role, spent)| (!spent).then_some(role))",
        ".filter_map(|(role, spent)| Some(role))",
        "generated_restart_groups",
    ),
    (
        "channel_callback_lock",
        "channel_loom",
        "channel_core.rs",
        "            inner.queue.push_back(envelope.into());\n            consumer_wake = inner.consumer.take();\n            native_consumer = inner\n                .native_consumer\n                .take()\n                .and_then(NativeRegistration::into_waker);\n",
        "            inner.queue.push_back(envelope.into());\n            consumer_wake = inner.consumer.take();\n            native_consumer = inner\n                .native_consumer\n                .take()\n                .and_then(NativeRegistration::into_waker);\n            if let Some(wake) = native_consumer.as_ref() { wake.wake(); }\n",
        "reentrant",
    ),
    (
        "executor_resume_admission",
        "executor",
        "coro_exec.rs",
        "cas_tag(a, ContTag::Parked, ContTag::Resuming)",
        "cas_tag(a, ContTag::Done, ContTag::Resuming)",
        "executor_raw_tag_admission",
    ),
    (
        "executor_checked_cleanup",
        "executor",
        "coro_exec.rs",
        "if !a.checked_invocation.load(Ordering::Acquire).is_null() {",
        "if false && !a.checked_invocation.load(Ordering::Acquire).is_null() {",
        "executor_teardown_respects_checked_owner",
    ),
    (
        "executor_dangling_slot",
        "executor",
        "coro_exec.rs",
        "let handle = a.suspended_cont.swap(ptr::null_mut(), Ordering::AcqRel);\n    // SAFETY: we won the single `… -> Destroyed` CAS",
        "let handle = a.suspended_cont.load(Ordering::Acquire);\n    // SAFETY: we won the single `… -> Destroyed` CAS",
        "executor_teardown_respects_checked_owner",
    ),
    (
        "executor_crash_cleanup",
        "executor",
        "coro_exec.rs",
        "unsafe { cont::hew_cont_frame_free(handle) };",
        "unsafe { cont::hew_cont_destroy(handle) };",
        "executor_crash_frees_allocation",
    ),
    (
        "executor_rearm_live_slot",
        "executor",
        "coro_exec.rs",
        "if !a.suspended_cont.load(Ordering::Acquire).is_null() {",
        "if false && !a.suspended_cont.load(Ordering::Acquire).is_null() {",
        "executor_raw_tag_admission",
    ),
    (
        "executor_relaxed_publication",
        "executor_loom",
        "coro_exec.rs",
        "        Ordering::AcqRel,\n        Ordering::Acquire,",
        "        Ordering::Relaxed,\n        Ordering::Relaxed,",
        "two_resumes",
    ),
    (
        "waker",
        "waker",
        "waker_target.rs",
        "unsafe { Self::retain(&self.0) }",
        "Self(self.0)",
        None,
    ),
    (
        "cancellation",
        "cancellation",
        "cancel_state.rs",
        ".compare_exchange(0, requested_reason, Ordering::AcqRel, Ordering::Acquire)",
        ".swap(requested_reason, Ordering::AcqRel)",
        None,
    ),
    ("await", "await", "await_state.rs", ".is_err()", ".is_ok()", None),
    (
        "timer",
        "timer",
        "timer_entry.rs",
        "cur == entry && unsafe { (*cur).generation } == generation",
        "cur == entry",
        "core_timer_removal_preserves_sibling_links",
    ),
    (
        "arena_extent",
        "arena",
        "arena_layout.rs",
        "let end = start.checked_add(size)?;",
        "let end = start.wrapping_add(size);",
        "arena_layout_and_quota",
    ),
    (
        "arena_overlap",
        "arena",
        "arena.rs",
        "chunk.base.add(extent.start)",
        "chunk.base",
        "arena_allocation_is_disjoint",
    ),
    (
        "arena_failed_charge",
        "arena",
        "arena.rs",
        "let mut index = self.current_chunk;",
        "self.used = new_used; let mut index = self.current_chunk;",
        "arena_mapping_failure",
    ),
    (
        "wheel_generation",
        "timer",
        "timer_wheel.rs",
        "w.next_generation = w.next_generation.wrapping_add(1);",
        "w.next_generation = 1;",
        "wheel_registration_preserves",
    ),
    (
        "wheel_bucket",
        "timer",
        "timer_wheel.rs",
        "let slot = (deadline / L0_MS) as usize & (L0_SIZE - 1);",
        "let slot = ((deadline / L0_MS) as usize + 1) & (L0_SIZE - 1);",
        "wheel_registration_preserves",
    ),
    (
        "wheel_removal",
        "await_race",
        "timer_wheel.rs",
        "remove_entry(&mut w, entry, generation)",
        "ptr::null_mut()",
        None,
    ),
    (
        "wheel_firing",
        "sleep_teardown",
        "timer_wheel.rs",
        "fire_expired_entries(expired)",
        "0",
        None,
    ),
    (
        "cancellation_delivery",
        "cancellation_delivery",
        "cancel_token_wake.rs",
        "if cancelled {",
        "if false && cancelled {",
        None,
    ),
    (
        "await_race",
        "await_race",
        "await_cancel.rs",
        "unsafe { cancel_timer(reg) };",
        "// Deliberately leave the deadline attached.",
        None,
    ),
    (
        "sleep_teardown",
        "sleep_teardown",
        "coro_sleep.rs",
        "CoroStatus::Cancelled as i32,\n            Ordering::AcqRel",
        "CoroStatus::Pending as i32,\n            Ordering::AcqRel",
        None,
    ),
    (
        "semaphore_handoff",
        "semaphore",
        "semaphore.rs",
        "if previous == ACQUIRED {",
        "if false && previous == ACQUIRED {",
        None,
    ),
    (
        "semaphore_stranded_waiter",
        "semaphore",
        "semaphore.rs",
        "if waiter.status.load(Ordering::Acquire) == PENDING {\n            waiter.status.store(ACQUIRED, Ordering::Release);",
        "if false && waiter.status.load(Ordering::Acquire) == PENDING {\n            waiter.status.store(ACQUIRED, Ordering::Release);",
        None,
    ),
    (
        "semaphore_timeout",
        "semaphore",
        "semaphore.rs",
        "if waiter.status.load(Ordering::Acquire) == PENDING {\n            waiter.status.store(TIMED_OUT, Ordering::Release);",
        "if waiter.status.load(Ordering::Acquire) == PENDING {\n            waiter.status.store(ACQUIRED, Ordering::Release);",
        None,
    ),
    (
        "supervisor_admission",
        "supervisor",
        "lifetime/pin_state.rs",
        "state & CLOSING != 0 || state & PINS == PINS",
        "state & PINS == PINS",
        "supervisor_pin_count",
    ),
    (
        "supervisor_drain",
        "supervisor",
        "lifetime/pin_state.rs",
        "self.access.load(Ordering::Acquire) == CLOSING",
        "self.access.load(Ordering::Acquire) & CLOSING != 0",
        None,
    ),
    (
        "supervisor_drain_notification",
        "supervisor",
        "lifetime/pin_state.rs",
        "previous & CLOSING != 0 && previous & PINS == 1",
        "false",
        "final_supervisor_pin",
    ),
    (
        "managed_quota",
        "managed_budget",
        "owned_value_budget/budget_limit.rs",
        "live.checked_add(reserved)",
        "live.checked_add(0)",
        "managed_quota",
    ),
    (
        "managed_acquisition",
        "managed_budget",
        "owned_value_budget/account.rs",
        "self.owner.release(&self.storage);",
        "// Deliberately leak the abandoned graph.",
        None,
    ),
    (
        "managed_reservation",
        "managed_budget",
        "owned_value_budget/account.rs",
        "self.owner.ledger().reserved -= self.bytes;",
        "self.owner.ledger().reserved -= 0;",
        None,
    ),
    (
        "heap_shrink",
        "heap",
        "mem.rs",
        "unsafe { hew_dealloc(ptr, old_size, align) };",
        "// Deliberately retain the allocation at size zero.",
        "general_heap_realloc",
    ),
    (
        "heap_relocated_owner",
        "heap",
        "mem.rs",
        "    new_ptr\n",
        "    ptr\n",
        "general_heap_realloc",
    ),
    (
        "read_slot_publication",
        "read_slot",
        "read_slot.rs",
        "if s.status\n        .compare_exchange(\n            ReadStatus::Pending as i32,",
        "if s.status\n        .compare_exchange(\n            s.status.load(Ordering::Acquire),",
        None,
    ),
    (
        "read_slot_lifetime",
        "read_slot",
        "read_slot.rs",
        "if prev != 1 {",
        "if prev != 2 {",
        "read_slot_teardown",
    ),
    (
        "root_readiness",
        "root_driver",
        "coro_root/poll.rs",
        "if !ready {",
        "if false {",
        None,
    ),
    (
        "root_spurious_abort",
        "root_driver",
        "coro_root/poll.rs",
        "if !ready {\n            return None;",
        "if !ready {\n            std::process::abort();",
        None,
    ),
    (
        "root_completed_resume",
        "root_driver",
        "coro_root/poll.rs",
        "if frame.is_null() || unsafe { hew_cont_done(*frame) } {",
        "if frame.is_null() {",
        None,
    ),
    (
        "root_yielded",
        "root_driver",
        "coro_root/poll.rs",
        "if unsafe { hew_coro_state_status(state) } == CoroStatus::Yielded as i32 {",
        "if false {",
        None,
    ),
    (
        "root_destroy",
        "root_driver",
        "coro_root/poll.rs",
        "unsafe { hew_cont_destroy(*frame) };",
        "// Deliberately skip destruction.",
        None,
    ),
    (
        "root_unfinished_destroy",
        "root_driver",
        "coro_root/poll.rs",
        "if !unsafe { hew_cont_done(*frame) } {",
        "if false {",
        None,
    ),
    (
        "root_clear",
        "root_driver",
        "coro_root/poll.rs",
        "*frame = std::ptr::null_mut();",
        "// Deliberately keep the dangling handle.",
        None,
    ),
    (
        "root_after_free",
        "root_driver",
        "coro_root/poll.rs",
        "unsafe { hew_cont_destroy(*frame) };",
        "unsafe { hew_cont_destroy(*frame) }; unsafe { hew_cont_done(*frame) };",
        None,
    ),
    (
        "root_result",
        "root_driver",
        "coro_root/poll.rs",
        "Some(unsafe { hew_coro_state_private_status(state) })",
        "Some(unsafe { hew_coro_state_status(state) })",
        None,
    ),
    (
        "vector_growth",
        "vector",
        "vec.rs",
        "new_cap.checked_mul(2)?",
        "new_cap.saturating_mul(2)",
        "vector_capacity",
    ),
    (
        "vector_extent",
        "vector",
        "vec.rs",
        "let size = capacity\n            .checked_mul((*v).elem_size)\n            .unwrap_or_else(|| allocation_size_overflow());",
        "let size = capacity.wrapping_mul((*v).elem_size);",
        "vector_buffer_layout",
    ),
    (
        "vector_cow",
        "vector",
        "vec.rs",
        "if !(*v).data.is_null() && (*buffer_owner(v)).load(Ordering::Acquire) != 1 {",
        "if false && !(*v).data.is_null() && (*buffer_owner(v)).load(Ordering::Acquire) != 1 {",
        "vector_shared_set",
    ),
    (
        "vector_spurious_abort",
        "vector",
        "vec.rs",
        'pub unsafe extern "C" fn hew_vec_make_unique(v: *mut HewVec) {',
        'pub unsafe extern "C" fn hew_vec_make_unique(v: *mut HewVec) { unsafe { abort_vector() };',
        "vector_shared_set",
    ),
    (
        "vector_interior",
        "vector",
        "vec.rs",
        "offset.map_or(source, |offset| (*v).data.add(offset).cast())",
        "source",
        "vector_interior_inputs",
    ),
    (
        "vector_remove",
        "vector",
        "vec.rs",
        "((*v).len - index - 1) * width,",
        "0, // Deliberately omit the suffix shift.",
        "vector_cow_mutations",
    ),
    (
        "vector_get",
        "vector",
        "vec.rs",
        "        match (*v).elem_kind {\n            ElemKind::String => {\n                let raw = (*v).data.cast::<*const HewString>().add(idx).read();",
        "        let idx = 0;\n        match (*v).elem_kind {\n            ElemKind::String => {\n                let raw = (*v).data.cast::<*const HewString>().add(idx).read();",
        "vector_get_clone",
    ),
    (
        "vector_index_narrowing",
        "vector",
        "vec/bounds.rs",
        "W::try_from(index)",
        "W::try_from(index as u32 as i64)",
        "vector_index_u32",
    ),
    (
        "vector_range_narrowing",
        "vector",
        "vec/bounds.rs",
        "W::try_from(end)",
        "W::try_from(end as u32 as i64)",
        "vector_range_u32",
    ),
    (
        "vector_miss_writes_output",
        "vector",
        "vec.rs",
        "let Some(idx) = checked_index(index, (*v).len) else {\n            return false;",
        "let Some(idx) = checked_index(index, (*v).len) else {\n            out.cast::<u8>().write(1);\n            return false;",
        "vector_failed_owned",
    ),
    (
        "vector_truncate_narrowing",
        "vector",
        "vec.rs",
        "checked_index(new_len, old_len)",
        "checked_index(new_len as u32 as i64, old_len)",
        "vector_truncate_checks_entire",
    ),
    (
        "vector_copy",
        "vector",
        "vec.rs",
        "let status = clone_fn.map_or(0, |clone_fn| {\n            clone_fn(data, dst.cast::<core::ffi::c_void>())\n        });",
        "let status = 0;",
        "vector_owned_copy_move",
    ),
    (
        "vector_move",
        "vector",
        "vec.rs",
        "// source's heap is now owned by the Vec; the source is dead.\n        core::ptr::copy_nonoverlapping(data.cast::<u8>(), dst, elem_size);",
        "// source's heap is now owned by the Vec; the source is dead.\n        core::ptr::copy_nonoverlapping(data.cast::<u8>(), dst, elem_size);\n        if let Some(clone) = layout.clone_fn { clone(data, dst.cast()); }",
        "vector_owned_copy_move",
    ),
    (
        "vector_pop",
        "vector",
        "vec.rs",
        "// Move out: byte-copy to `out`, leaving no live copy in the buffer (the",
        "if let Some(drop) = layout.drop_fn { drop(src.cast()); }\n        // Move out: byte-copy to `out`, leaving no live copy in the buffer (the",
        "vector_owned_copy_move",
    ),
    (
        "vector_clear",
        "vector",
        "vec.rs",
        "(*v).len = new_len;\n        HewReleaseCursor::join(cursors)",
        "(*v).len = new_len;\n        drop(cursors);\n        ptr::null_mut()",
        "vector_owned_self_overwrite",
    ),
    (
        "vector_displaced_identity",
        "vector",
        "vec.rs",
        "let displaced = HewReleaseCursor::detached(slot.cast(), layout);",
        "let displaced = HewReleaseCursor::detached(scratch.cast(), layout);",
        "vector_owned_self_overwrite",
    ),
]
FILES = [
    "timer_actor/protocol.rs",
    "timer_periodic/entry.rs",
    "reactor_wait.rs",
    "timer_deadline.rs",
    "waker_target.rs",
    "ready_state.rs",
    "cancel_state.rs",
    "cancel_token.rs",
    "cancel_token_wake.rs",
    "await_state.rs",
    "await_cancel.rs",
    "timer_entry.rs",
    "timer_wheel.rs",
    "arena.rs",
    "arena_layout.rs",
    "coro_sleep.rs",
    "request_deadline.rs",
    "semaphore.rs",
    "read_slot.rs",
    "bytes_value.rs",
    "mem.rs",
    "lifetime/pin_state.rs",
    "owned_value_budget/account.rs",
    "owned_value_budget/budget_limit.rs",
    "coro_root/poll.rs",
    "vec.rs",
    "vec/managed.rs",
    "vec/bounds.rs",
    "coro_exec.rs",
    "internal/cont_tag.rs",
    "channel_core.rs",
    "channel_core/item.rs",
    "channel_envelope.rs",
    "exit_status/state.rs",
    "supervisor/restart_policy.rs",
    "supervisor/restart_history.rs",
]
CONCURRENT = {
    "cancellation_delivery",
    "await_race",
    "sleep_teardown",
    "semaphore",
    "supervisor",
    "managed_budget",
    "read_slot",
}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--mutation", choices=[mutation[0] for mutation in MUTATIONS])
    parser.add_argument(
        "--feature", choices=sorted({mutation[1] for mutation in MUTATIONS})
    )
    selected = parser.parse_args()
    mutations = [
        m
        for m in MUTATIONS
        if (not selected.mutation or m[0] == selected.mutation)
        and (not selected.feature or m[1] == selected.feature)
    ]
    if not mutations:
        raise SystemExit("No implementation defects selected")
    logs = PROOFS / "target/proof-logs"
    logs.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix="hew-protocol-mutants-") as directory:
        folder = Path(directory)
        runtime = folder / "hew-runtime/src"
        runtime.mkdir(parents=True)
        for name in FILES:
            (runtime / name).parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(ROOT / "hew-runtime/src" / name, runtime / name)
        proof = folder / "proofs/runtime_protocols"
        shutil.copytree(PROOFS / "src", proof / "src")
        shutil.copytree(PROOFS / "examples", proof / "examples")
        for name in ["Cargo.toml", "Cargo.lock", "build.rs"]:
            shutil.copy2(PROOFS / name, proof / name)
        manifest = proof / "Cargo.toml"
        manifest.write_text(
            manifest.read_text().replace(
                'path = "../../hew-cabi"', f'path = "{ROOT / "hew-cabi"}"'
            )
        )
        for name, feature, filename, before, after, harness in mutations:
            source = (ROOT / "hew-runtime/src" / filename).read_text()
            if source.count(before) != 1:
                raise SystemExit(f"Mutation site changed; review {feature}: {before!r}")
            (runtime / filename).write_text(source.replace(before, after, 1))
            if feature == "periodic_loom":
                args = [
                    "cargo",
                    "run",
                    "--manifest-path",
                    str(proof / "Cargo.toml"),
                    "--locked",
                    "--features",
                    feature,
                    "--bin",
                    "periodic",
                    "--",
                    harness,
                ]
            elif feature == "reactor_loom":
                args = [
                    "cargo",
                    "run",
                    "--manifest-path",
                    str(proof / "Cargo.toml"),
                    "--locked",
                    "--features",
                    feature,
                    "--bin",
                    "reactor",
                    "--",
                    harness,
                ]
            elif feature in ("channel_loom", "fault_loom"):
                args = [
                    "cargo",
                    "run",
                    "--manifest-path",
                    str(proof / "Cargo.toml"),
                    "--locked",
                    "--features",
                    feature,
                    "--bin",
                    "channels" if feature == "channel_loom" else "faults",
                ]
                if harness:
                    args += ["--", harness]
            elif feature == "failure_properties":
                args = [
                    "cargo",
                    "test",
                    "--manifest-path",
                    str(proof / "Cargo.toml"),
                    "--locked",
                    "--features",
                    feature,
                    "--lib",
                    harness or "failure_properties",
                ]
            elif feature == "executor_loom":
                args = [
                    "cargo",
                    "run",
                    "--manifest-path",
                    str(proof / "Cargo.toml"),
                    "--locked",
                    "--features",
                    "executor_loom",
                    "--bin",
                    "executor",
                    "--",
                    harness,
                ]
            elif feature in CONCURRENT and harness is None:
                args = [
                    "cargo",
                    "run",
                    "--manifest-path",
                    str(proof / "Cargo.toml"),
                    "--locked",
                    "--features",
                    "loom",
                    "--bin",
                    "protocols",
                    "--",
                    feature,
                ]
            else:
                args = command(proof, feature, playback=True)
                if harness and feature != "read_slot":
                    if "--harness" in args:
                        index = args.index("--harness")
                        args[index + 1] = harness
                    else:
                        args += ["--harness", harness]
            result = subprocess.run(args, text=True, capture_output=True)
            (logs / f"mutant-{name}.log").write_text(result.stdout + result.stderr)
            if feature == "periodic_loom":
                detected = (
                    result.returncode != 0
                    and "panicked at" in result.stderr
                    and "periodic:" in result.stderr
                )
            elif feature == "reactor_loom":
                detected = (
                    result.returncode != 0
                    and "panicked at" in result.stderr
                    and "reactor:" in result.stderr
                )
            elif feature in ("channel_loom", "fault_loom"):
                detected = (
                    result.returncode != 0
                    and "panicked at" in result.stderr
                    and (
                        "channel:" in result.stderr
                        or "fault:" in result.stderr
                        or (
                            name == "channel_callback_lock"
                            and "deadlock" in result.stderr
                        )
                    )
                )
            elif feature == "failure_properties":
                detected = (
                    result.returncode != 0
                    and "minimal failing input:" in result.stdout
                    and "Test failed:" in result.stdout
                )
            elif feature == "executor_loom":
                detected = (
                    result.returncode != 0
                    and "panicked at" in result.stderr
                    and "Causality violation:" in result.stderr
                    and "UnsafeCell" in result.stderr
                )
            elif feature in CONCURRENT and harness is None:
                detected = (
                    result.returncode != 0
                    and "panicked at" in result.stderr
                    and any(
                        prefix + ":" in result.stderr
                        for prefix in ("protocol", "lifecycle")
                    )
                )
            else:
                named_failure = any(
                    f'Failed Checks: "{prefix}:' in result.stdout
                    for prefix in (
                        "protocol",
                        "arena",
                        "wheel",
                        "lifecycle",
                        "heap",
                        "root",
                        "vector",
                        "executor",
                        "fault",
                        "restart",
                        "one-shot",
                    )
                )
                # This witness frees a retained object. Its expected violation
                # is Kani's built-in memory check, not a user assertion after UB.
                use_after_free = name in ("read_slot_lifetime", "vector_interior") and (
                    "Failed Checks: dereference failure: deallocated dynamic object"
                    in result.stdout
                    or (
                        name == "vector_interior"
                        and "Failed Checks: memcpy source region readable"
                        in result.stdout
                    )
                )
                detected = (
                    result.returncode == 1
                    and (named_failure or use_after_free)
                    and "kani::concrete_playback_run" in result.stdout
                )
            if not detected:
                print(result.stdout)
                print(result.stderr)
                raise SystemExit(
                    f"Expected a protocol counterexample for {name}; see {logs}"
                )
            (runtime / filename).write_text(source)
            print(
                f"Rejected {name} implementation defect with a counterexample",
                flush=True,
            )


if __name__ == "__main__":
    main()
