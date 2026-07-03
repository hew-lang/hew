// Domain harness for hew-mir `diagnostics` tests.
// Add new diagnostics-behaviour tests under `tests/diagnostics/`, wire them into this
// file via #[path] — do not create a new top-level tests/*.rs file.

#[path = "diagnostics/const_descriptor.rs"]
mod const_descriptor;
#[path = "diagnostics/div_shift_trap.rs"]
mod div_shift_trap;
#[path = "diagnostics/dump_filecheck.rs"]
mod dump_filecheck;
#[path = "diagnostics/instr_spans_line_table.rs"]
mod instr_spans_line_table;
#[path = "diagnostics/liveness.rs"]
mod liveness;
#[path = "diagnostics/local_names_side_table.rs"]
mod local_names_side_table;
#[path = "diagnostics/overflow_trap.rs"]
mod overflow_trap;
#[path = "diagnostics/runtime_call_allowlist.rs"]
mod runtime_call_allowlist;
#[path = "diagnostics/trap_terminator.rs"]
mod trap_terminator;
#[path = "diagnostics/vec_index_oob.rs"]
mod vec_index_oob;
