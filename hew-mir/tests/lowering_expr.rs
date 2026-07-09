// Domain harness for hew-mir `lowering_expr` tests.
// Add new lowering_expr-behaviour tests under `tests/lowering_expr/`, wire them into this
// file via #[path] — do not create a new top-level tests/*.rs file.

#[path = "lowering_expr/binop_bitwise_logical.rs"]
mod binop_bitwise_logical;
#[path = "lowering_expr/bytes_literal_lowering.rs"]
mod bytes_literal_lowering;
#[path = "lowering_expr/conditional_move_drop.rs"]
mod conditional_move_drop;
#[path = "lowering_expr/cstring_container_domain_canary.rs"]
mod cstring_container_domain_canary;
#[path = "lowering_expr/elaborate.rs"]
mod elaborate;
#[path = "lowering_expr/forawait_loopvar_release.rs"]
mod forawait_loopvar_release;
#[path = "lowering_expr/gen_block_mir_lowering.rs"]
mod gen_block_mir_lowering;
#[path = "lowering_expr/generic_record_layout_test.rs"]
mod generic_record_layout_test;
#[path = "lowering_expr/hashmap_hashset_local_drop.rs"]
mod hashmap_hashset_local_drop;
#[path = "lowering_expr/identity_lowering.rs"]
mod identity_lowering;
#[path = "lowering_expr/lambda_captures.rs"]
mod lambda_captures;
#[path = "lowering_expr/literal_misc.rs"]
mod literal_misc;
#[path = "lowering_expr/loop_break_continue.rs"]
mod loop_break_continue;
#[path = "lowering_expr/machine_dispatch_fixtures.rs"]
mod machine_dispatch_fixtures;
#[path = "lowering_expr/machine_mir.rs"]
mod machine_mir;
#[path = "lowering_expr/match_call_scrutinee_drop.rs"]
mod match_call_scrutinee_drop;
#[path = "lowering_expr/match_literal_string.rs"]
mod match_literal_string;
#[path = "lowering_expr/match_project.rs"]
mod match_project;
#[path = "lowering_expr/monomorph_lowering_test.rs"]
mod monomorph_lowering_test;
#[path = "lowering_expr/numeric_cast_lowering.rs"]
mod numeric_cast_lowering;
#[path = "lowering_expr/owned_string_temp_drop_canary.rs"]
mod owned_string_temp_drop_canary;
#[path = "lowering_expr/plain_vec_local_drop.rs"]
mod plain_vec_local_drop;
#[path = "lowering_expr/polymorphic_mir_typeparam.rs"]
mod polymorphic_mir_typeparam;
#[path = "lowering_expr/regex_predicate_lowering.rs"]
mod regex_predicate_lowering;
#[path = "lowering_expr/runtime_abi_instr.rs"]
mod runtime_abi_instr;
#[path = "lowering_expr/select_arm_dataflow.rs"]
mod select_arm_dataflow;
#[path = "lowering_expr/send_alias_mode.rs"]
mod send_alias_mode;
#[path = "lowering_expr/state_clone_classification.rs"]
mod state_clone_classification;
#[path = "lowering_expr/tuple_construct_lowering.rs"]
mod tuple_construct_lowering;
#[path = "lowering_expr/tuple_index.rs"]
mod tuple_index;
#[path = "lowering_expr/unary_lowering.rs"]
mod unary_lowering;
#[path = "lowering_expr/vec_slice_range.rs"]
mod vec_slice_range;
#[path = "lowering_expr/vertical.rs"]
mod vertical;
#[path = "lowering_expr/wrapping_lowering.rs"]
mod wrapping_lowering;
