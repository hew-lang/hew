// Domain harness for hew-mir `lowering_expr` tests.
// Add new lowering_expr-behaviour tests under `tests/lowering_expr/`, wire them into this
// file via #[path] — do not create a new top-level tests/*.rs file.

#[path = "lowering_expr/actor_handler_bytes_ownership_authority.rs"]
mod actor_handler_bytes_ownership_authority;
#[path = "lowering_expr/anonymous_owned_temp_drop.rs"]
mod anonymous_owned_temp_drop;
#[path = "lowering_expr/audited_extern_argument_provenance.rs"]
mod audited_extern_argument_provenance;
#[path = "lowering_expr/audited_extern_result_provenance.rs"]
mod audited_extern_result_provenance;
#[path = "lowering_expr/binder_shape_release_sweep.rs"]
mod binder_shape_release_sweep;
#[path = "lowering_expr/binop_bitwise_logical.rs"]
mod binop_bitwise_logical;
#[path = "lowering_expr/borrowed_forwarder_retained_owner_class.rs"]
mod borrowed_forwarder_retained_owner_class;
#[path = "lowering_expr/bytes_literal_lowering.rs"]
mod bytes_literal_lowering;
#[path = "lowering_expr/bytes_retained_local_share.rs"]
mod bytes_retained_local_share;
#[path = "lowering_expr/closure_await_fail_closed.rs"]
mod closure_await_fail_closed;
#[path = "lowering_expr/closure_env_ownership.rs"]
mod closure_env_ownership;
#[path = "lowering_expr/conditional_ask_return_ownership.rs"]
mod conditional_ask_return_ownership;
#[path = "lowering_expr/conditional_move_drop.rs"]
mod conditional_move_drop;
#[path = "lowering_expr/contextual_resource_binder_drop.rs"]
mod contextual_resource_binder_drop;
#[path = "lowering_expr/cstring_container_domain_canary.rs"]
mod cstring_container_domain_canary;
#[path = "lowering_expr/declared_release_adoption.rs"]
mod declared_release_adoption;
#[path = "lowering_expr/elaborate.rs"]
mod elaborate;
#[path = "lowering_expr/extern_wrapper_result_opacity.rs"]
mod extern_wrapper_result_opacity;

#[path = "lowering_expr/divergent_selection_transfer.rs"]
mod divergent_selection_transfer;
#[path = "lowering_expr/edge_owner_facts.rs"]
mod edge_owner_facts;
#[path = "lowering_expr/for_in_hash_projection_types.rs"]
mod for_in_hash_projection_types;
#[path = "lowering_expr/forawait_loopvar_release.rs"]
mod forawait_loopvar_release;
#[path = "lowering_expr/fresh_vec_projection_owner.rs"]
mod fresh_vec_projection_owner;
#[path = "lowering_expr/funcupdate_field_override_release.rs"]
mod funcupdate_field_override_release;
#[path = "lowering_expr/gen_block_mir_lowering.rs"]
mod gen_block_mir_lowering;
#[path = "lowering_expr/generic_record_layout_test.rs"]
mod generic_record_layout_test;
#[path = "lowering_expr/hashmap_hashset_local_drop.rs"]
mod hashmap_hashset_local_drop;
#[path = "lowering_expr/http_ws_string_temp_drop_canary.rs"]
mod http_ws_string_temp_drop_canary;
#[path = "lowering_expr/identity_lowering.rs"]
mod identity_lowering;
#[path = "lowering_expr/lambda_captures.rs"]
mod lambda_captures;
#[path = "lowering_expr/last_borrowed_release_multiplicity.rs"]
mod last_borrowed_release_multiplicity;
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
#[path = "lowering_expr/nested_tuple_alias_rebind.rs"]
mod nested_tuple_alias_rebind;
#[path = "lowering_expr/numeric_cast_lowering.rs"]
mod numeric_cast_lowering;
#[path = "lowering_expr/observe_string_temp_drop_canary.rs"]
mod observe_string_temp_drop_canary;
#[path = "lowering_expr/owned_string_temp_drop_canary.rs"]
mod owned_string_temp_drop_canary;
#[path = "lowering_expr/owner_mint_warrant_seams.rs"]
mod owner_mint_warrant_seams;
#[path = "lowering_expr/plain_vec_local_drop.rs"]
mod plain_vec_local_drop;
#[path = "lowering_expr/polymorphic_mir_typeparam.rs"]
mod polymorphic_mir_typeparam;
#[path = "lowering_expr/quic_string_temp_drop_canary.rs"]
mod quic_string_temp_drop_canary;
#[path = "lowering_expr/range_integer_normalization.rs"]
mod range_integer_normalization;
#[path = "lowering_expr/rc_weak_lowering.rs"]
mod rc_weak_lowering;
#[path = "lowering_expr/regex_predicate_lowering.rs"]
mod regex_predicate_lowering;
#[path = "lowering_expr/returned_member_cancel_drop.rs"]
mod returned_member_cancel_drop;
#[path = "lowering_expr/runtime_abi_instr.rs"]
mod runtime_abi_instr;
#[path = "lowering_expr/select_arm_dataflow.rs"]
mod select_arm_dataflow;
#[path = "lowering_expr/select_arm_owned_registration.rs"]
mod select_arm_owned_registration;
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
#[path = "lowering_expr/while_let_reassign_owner.rs"]
mod while_let_reassign_owner;
#[path = "lowering_expr/while_let_skipped_owned.rs"]
mod while_let_skipped_owned;
#[path = "lowering_expr/wrapping_lowering.rs"]
mod wrapping_lowering;
