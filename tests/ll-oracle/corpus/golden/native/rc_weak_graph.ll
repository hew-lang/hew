; ModuleID = 'rc_weak_graph'
source_filename = "rc_weak_graph"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "aarch64-apple-macosx13.0"

%"Option$$Weak$lNode$g" = type { i8, [1 x i64] }
%Node = type { ptr, %"Option$$Weak$lNode$g" }
%"Option$$Rc$lNode$g" = type { i8, [1 x i64] }

@str_lit = private unnamed_addr constant [5 x i8] c"root\00", align 1
@str_lit.1 = private unnamed_addr constant [12 x i8] c"replacement\00", align 1
@str_lit.2 = private unnamed_addr constant [3 x i8] c"ns\00", align 1

declare void @hew_sleep_ns(i64)

declare void @hew_sleep_until_ns(i64)

declare void @hew_exit(i64)

declare void @hew_panic_msg(ptr)

declare void @hew_assert(i8)

declare void @hew_print_value(i8, i64, i1)

declare ptr @hew_int_to_string(i32)

declare ptr @hew_i64_to_string(i64)

declare ptr @hew_u8_to_string(i8)

declare ptr @hew_uint_to_string(i32)

declare ptr @hew_u64_to_string(i64)

declare ptr @hew_float_to_string(double)

declare ptr @hew_bool_to_string(i8)

declare ptr @hew_char_to_string(i32)

declare ptr @hew_string_clone(ptr)

declare ptr @hew_string_concat(ptr, ptr)

declare void @hew_assert_eq_i8(i8, i8)

declare void @hew_assert_eq_i16(i16, i16)

declare void @hew_assert_eq_i32(i32, i32)

declare void @hew_assert_eq_i64(i64, i64)

declare void @hew_assert_eq_isize(i64, i64)

declare void @hew_assert_eq_u8(i8, i8)

declare void @hew_assert_eq_u16(i16, i16)

declare void @hew_assert_eq_u32(i32, i32)

declare void @hew_assert_eq_u64(i64, i64)

declare void @hew_assert_eq_usize(i64, i64)

declare void @hew_assert_eq_str(ptr, ptr)

declare void @hew_assert_eq_f64(double, double)

declare void @hew_assert_eq_bool(i8, i8)

declare void @hew_assert_ne_i8(i8, i8)

declare void @hew_assert_ne_i16(i16, i16)

declare void @hew_assert_ne_i32(i32, i32)

declare void @hew_assert_ne_i64(i64, i64)

declare void @hew_assert_ne_isize(i64, i64)

declare void @hew_assert_ne_u8(i8, i8)

declare void @hew_assert_ne_u16(i16, i16)

declare void @hew_assert_ne_u32(i32, i32)

declare void @hew_assert_ne_u64(i64, i64)

declare void @hew_assert_ne_usize(i64, i64)

declare void @hew_assert_ne_str(ptr, ptr)

declare void @hew_assert_ne_f64(double, double)

declare void @hew_assert_ne_bool(i8, i8)

declare i32 @hew_string_length(ptr)

declare i64 @hew_vec_len(ptr)

declare i64 @hew_duration_nanos(i64)

declare i64 @hew_duration_micros(i64)

declare i64 @hew_duration_millis(i64)

declare i64 @hew_duration_secs(i64)

declare i64 @hew_duration_mins(i64)

declare i64 @hew_duration_hours(i64)

declare i64 @hew_duration_abs(i64)

declare i32 @hew_duration_is_zero(i64)

declare i64 @hew_instant_now()

declare i64 @hew_instant_elapsed(i64)

declare i64 @hew_instant_duration_since(i64, i64)

declare i8 @hew_string_starts_with(ptr, ptr)

declare i8 @hew_string_ends_with(ptr, ptr)

declare i8 @hew_string_contains(ptr, ptr)

declare i8 @hew_string_is_empty(ptr)

declare i8 @hew_string_is_digit(ptr)

declare i8 @hew_string_is_alpha(ptr)

declare i8 @hew_string_is_alphanumeric(ptr)

declare ptr @hew_string_trim(ptr)

declare ptr @hew_string_to_lowercase(ptr)

declare ptr @hew_string_to_uppercase(ptr)

declare [2 x i64] @hew_string_to_bytes(ptr)

declare ptr @hew_string_replace(ptr, ptr, ptr)

declare ptr @hew_string_split(ptr, ptr)

declare ptr @hew_string_lines(ptr)

declare ptr @hew_string_slice(ptr, i64, i64)

declare ptr @hew_string_repeat(ptr, i64)

declare ptr @hew_string_chars(ptr)

declare i32 @hew_string_char_count(ptr)

declare void @hew_vec_push_bool(ptr, i1)

declare void @hew_vec_push_i8(ptr, i8)

declare void @hew_vec_push_u8(ptr, i8)

declare void @hew_vec_push_i16(ptr, i16)

declare void @hew_vec_push_u16(ptr, i16)

declare void @hew_vec_push_i32(ptr, i32)

declare void @hew_vec_push_i64(ptr, i64)

declare void @hew_vec_push_f64(ptr, double)

declare void @hew_vec_push_f32(ptr, float)

declare void @hew_vec_push_str(ptr, ptr)

declare void @hew_vec_push_ptr(ptr, ptr)

declare i1 @hew_vec_pop_bool(ptr)

declare i8 @hew_vec_pop_i8(ptr)

declare i8 @hew_vec_pop_u8(ptr)

declare i16 @hew_vec_pop_i16(ptr)

declare i16 @hew_vec_pop_u16(ptr)

declare i32 @hew_vec_pop_i32(ptr)

declare i64 @hew_vec_pop_i64(ptr)

declare double @hew_vec_pop_f64(ptr)

declare float @hew_vec_pop_f32(ptr)

declare ptr @hew_vec_pop_str(ptr)

declare ptr @hew_vec_pop_ptr(ptr)

declare i1 @hew_vec_get_bool(ptr, i64)

declare i8 @hew_vec_get_i8(ptr, i64)

declare i8 @hew_vec_get_u8(ptr, i64)

declare i16 @hew_vec_get_i16(ptr, i64)

declare i16 @hew_vec_get_u16(ptr, i64)

declare i32 @hew_vec_get_i32(ptr, i64)

declare i64 @hew_vec_get_i64(ptr, i64)

declare double @hew_vec_get_f64(ptr, i64)

declare float @hew_vec_get_f32(ptr, i64)

declare ptr @hew_vec_get_str(ptr, i64)

declare ptr @hew_vec_get_ptr(ptr, i64)

declare void @hew_vec_set_bool(ptr, i64, i1)

declare void @hew_vec_set_i8(ptr, i64, i8)

declare void @hew_vec_set_u8(ptr, i64, i8)

declare void @hew_vec_set_i16(ptr, i64, i16)

declare void @hew_vec_set_u16(ptr, i64, i16)

declare void @hew_vec_set_i32(ptr, i64, i32)

declare void @hew_vec_set_i64(ptr, i64, i64)

declare void @hew_vec_set_f64(ptr, i64, double)

declare void @hew_vec_set_f32(ptr, i64, float)

declare void @hew_vec_set_str(ptr, i64, ptr)

declare void @hew_vec_set_ptr(ptr, i64, ptr)

declare i8 @hew_vec_is_empty(ptr)

declare void @hew_vec_clear(ptr)

declare i8 @hew_vec_contains_i32(ptr, i32)

declare i8 @hew_vec_contains_i64(ptr, i64)

declare i8 @hew_vec_contains_f64(ptr, double)

declare i8 @hew_vec_contains_str(ptr, ptr)

declare ptr @hew_bytes_to_string(ptr)

declare void @hew_vec_append(ptr, ptr)

declare i1 @hew_vec_remove_at_bool(ptr, i64)

declare i8 @hew_vec_remove_at_i8(ptr, i64)

declare i8 @hew_vec_remove_at_u8(ptr, i64)

declare i16 @hew_vec_remove_at_i16(ptr, i64)

declare i16 @hew_vec_remove_at_u16(ptr, i64)

declare i32 @hew_vec_remove_at_i32(ptr, i64)

declare i64 @hew_vec_remove_at_i64(ptr, i64)

declare float @hew_vec_remove_at_f32(ptr, i64)

declare double @hew_vec_remove_at_f64(ptr, i64)

declare ptr @hew_vec_remove_at_str(ptr, i64)

declare ptr @hew_vec_remove_at_ptr(ptr, i64)

declare ptr @hew_vec_clone(ptr)

declare ptr @hew_vec_join_str(ptr, ptr)

declare void @hew_random_seed(i64)

declare double @hew_random_random()

declare double @hew_random_gauss(double, double)

declare i64 @hew_random_randint(i64, i64)

declare void @hew_random_shuffle_i64(ptr)

declare i64 @hew_random_choices_vec(ptr, double, i64)

declare void @hew_node_api_set_transport(ptr)

declare void @hew_node_api_start(ptr)

declare void @hew_node_api_connect(ptr)

declare void @hew_node_api_shutdown()

declare void @hew_node_api_load_keys(ptr)

declare void @hew_node_api_allow_peer(i16, ptr)

declare ptr @hew_node_api_identity_key()

declare i64 @hew_actor_pid(ptr)

declare i32 @hew_node_api_register_by_pid(ptr, i64)

declare ptr @hew_stream_channel(i64)

declare ptr @hew_stream_pair_sink(ptr)

declare ptr @hew_stream_pair_stream(ptr)

declare void @hew_stream_pair_free(ptr)

declare void @hew_sink_close(ptr)

declare i32 @hew_sink_peer_closed(ptr)

declare void @hew_actor_gen_sink_register(ptr, ptr)

declare void @hew_actor_gen_sink_complete(ptr, ptr)

define internal ptr @build() {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca %"Option$$Weak$lNode$g", align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca %Node, align 8
  %local_4 = alloca ptr, align 8
  %local_5 = alloca ptr, align 8
  %local_6 = alloca i64, align 8
  %local_7 = alloca ptr, align 8
  %local_8 = alloca ptr, align 8
  %local_9 = alloca i64, align 8
  %local_10 = alloca ptr, align 8
  %local_11 = alloca %"Option$$Weak$lNode$g", align 8
  %local_12 = alloca ptr, align 8
  %local_13 = alloca i64, align 8
  %local_14 = alloca %Node, align 8
  %local_15 = alloca i8, align 1
  %helper_crash_cleanup_token_5 = alloca i64, align 8
  store i64 0, ptr %helper_crash_cleanup_token_5, align 8
  %helper_crash_cleanup_active_5 = alloca i1, align 1
  store i1 false, ptr %helper_crash_cleanup_active_5, align 1
  store i64 -1, ptr %local_6, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  store ptr @str_lit, ptr %local_0, align 8
  store i64 1, ptr %local_2, align 8
  %machine_tag_ptr = getelementptr inbounds nuw %"Option$$Weak$lNode$g", ptr %local_1, i32 0, i32 0
  %move_iN_load_wide = load i64, ptr %local_2, align 8
  %move_iN_trunc = trunc i64 %move_iN_load_wide to i8
  store i8 %move_iN_trunc, ptr %machine_tag_ptr, align 1
  %field_0_init_ptr = getelementptr inbounds nuw %Node, ptr %local_3, i32 0, i32 0
  %field_0_init_src = load ptr, ptr %local_0, align 8
  store ptr %field_0_init_src, ptr %field_0_init_ptr, align 8
  %field_1_init_ptr = getelementptr inbounds nuw %Node, ptr %local_3, i32 0, i32 1
  %field_1_init_src = load %"Option$$Weak$lNode$g", ptr %local_1, align 8
  store %"Option$$Weak$lNode$g" %field_1_init_src, ptr %field_1_init_ptr, align 8
  %rc_new = call ptr @hew_rc_new(ptr %local_3, i64 24, i64 8, ptr @__hew_rc_payload_drop_Node)
  store ptr %rc_new, ptr %local_4, align 8
  %helper_crash_cleanup_was_active = load i1, ptr %helper_crash_cleanup_active_5, align 1
  br i1 %helper_crash_cleanup_was_active, label %helper_crash_cleanup_deactivate, label %helper_crash_cleanup_deactivate_merge

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0

helper_crash_cleanup_deactivate:                  ; preds = %bb0
  %helper_crash_cleanup_token = load i64, ptr %helper_crash_cleanup_token_5, align 8
  %helper_crash_cleanup_deactivate_call = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token)
  br i1 %helper_crash_cleanup_deactivate_call, label %helper_crash_cleanup_deactivate_accepted, label %helper_crash_cleanup_deactivate_rejected

helper_crash_cleanup_deactivate_merge:            ; preds = %helper_crash_cleanup_deactivate_accepted, %bb0
  %move_load = load ptr, ptr %local_4, align 8
  store ptr %move_load, ptr %local_5, align 8
  store i64 0, ptr %local_6, align 8
  %helper_crash_cleanup_arm_guard = load i64, ptr %local_6, align 8
  %helper_crash_cleanup_arm_guard_live = icmp eq i64 %helper_crash_cleanup_arm_guard, 0
  br i1 %helper_crash_cleanup_arm_guard_live, label %helper_crash_cleanup_guard_live, label %helper_crash_cleanup_guard_merge

helper_crash_cleanup_deactivate_accepted:         ; preds = %helper_crash_cleanup_deactivate
  store i1 false, ptr %helper_crash_cleanup_active_5, align 1
  br label %helper_crash_cleanup_deactivate_merge

helper_crash_cleanup_deactivate_rejected:         ; preds = %helper_crash_cleanup_deactivate
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_guard_live:                  ; preds = %helper_crash_cleanup_deactivate_merge
  %helper_crash_cleanup_prior_token = load i64, ptr %helper_crash_cleanup_token_5, align 8
  %arm_typed_crash_cleanup = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token, ptr %local_5, i64 8, i64 8, ptr @__hew_frame_cleanup_193e762a1871d870, i32 1, i32 0)
  %frame_cleanup_arm_failed = icmp eq i64 %arm_typed_crash_cleanup, -1
  br i1 %frame_cleanup_arm_failed, label %frame_cleanup_rejected, label %frame_cleanup_registered

helper_crash_cleanup_guard_merge:                 ; preds = %frame_cleanup_registered, %helper_crash_cleanup_deactivate_merge
  %rc_borrow_handle = load ptr, ptr %local_5, align 8
  %rc_handle_result = call ptr @hew_rc_downgrade(ptr %rc_borrow_handle)
  store ptr %rc_handle_result, ptr %local_7, align 8
  %move_load1 = load ptr, ptr %local_7, align 8
  store ptr %move_load1, ptr %local_8, align 8
  store i64 0, ptr %local_9, align 8
  store ptr @str_lit.1, ptr %local_10, align 8
  %rc_borrow_handle2 = load ptr, ptr %local_8, align 8
  %rc_handle_result3 = call ptr @hew_weak_clone_rc(ptr %rc_borrow_handle2)
  store ptr %rc_handle_result3, ptr %local_12, align 8
  store i64 0, ptr %local_13, align 8
  %machine_tag_ptr4 = getelementptr inbounds nuw %"Option$$Weak$lNode$g", ptr %local_11, i32 0, i32 0
  %move_iN_load_wide5 = load i64, ptr %local_13, align 8
  %move_iN_trunc6 = trunc i64 %move_iN_load_wide5 to i8
  store i8 %move_iN_trunc6, ptr %machine_tag_ptr4, align 1
  %machine_payload_ptr = getelementptr inbounds nuw %"Option$$Weak$lNode$g", ptr %local_11, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { ptr }, ptr %machine_payload_ptr, i32 0, i32 0
  %move_load7 = load ptr, ptr %local_12, align 8
  store ptr %move_load7, ptr %machine_variant_field_ptr, align 8
  %field_0_init_ptr8 = getelementptr inbounds nuw %Node, ptr %local_14, i32 0, i32 0
  %field_0_init_src9 = load ptr, ptr %local_10, align 8
  store ptr %field_0_init_src9, ptr %field_0_init_ptr8, align 8
  %field_1_init_ptr10 = getelementptr inbounds nuw %Node, ptr %local_14, i32 0, i32 1
  %field_1_init_src11 = load %"Option$$Weak$lNode$g", ptr %local_11, align 8
  store %"Option$$Weak$lNode$g" %field_1_init_src11, ptr %field_1_init_ptr10, align 8
  %rc_set_handle = load ptr, ptr %local_5, align 8
  call void @hew_rc_set(ptr %rc_set_handle, ptr %local_14)
  store i8 0, ptr %local_15, align 1
  store i64 1, ptr %local_9, align 8
  %move_load12 = load ptr, ptr %local_8, align 8
  store ptr %move_load12, ptr %return_slot, align 8
  %resource_drop_flag = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed = icmp eq i64 %resource_drop_flag, 0
  br i1 %resource_drop_not_consumed, label %resource_drop_live_only, label %resource_drop_merge

frame_cleanup_registered:                         ; preds = %helper_crash_cleanup_guard_live
  store i64 %arm_typed_crash_cleanup, ptr %helper_crash_cleanup_token_5, align 8
  store i1 true, ptr %helper_crash_cleanup_active_5, align 1
  br label %helper_crash_cleanup_guard_merge

frame_cleanup_rejected:                           ; preds = %helper_crash_cleanup_guard_live
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only:                          ; preds = %helper_crash_cleanup_guard_merge
  %helper_crash_cleanup_drop_active = load i1, ptr %helper_crash_cleanup_active_5, align 1
  br i1 %helper_crash_cleanup_drop_active, label %helper_crash_cleanup_retire, label %helper_crash_cleanup_retire_merge

resource_drop_merge:                              ; preds = %helper_crash_cleanup_retire_merge, %helper_crash_cleanup_guard_merge
  %helper_crash_cleanup_return_token_5 = load i64, ptr %helper_crash_cleanup_token_5, align 8
  %helper_crash_cleanup_return_has_token_5 = icmp ne i64 %helper_crash_cleanup_return_token_5, 0
  br i1 %helper_crash_cleanup_return_has_token_5, label %helper_crash_cleanup_return_retire_5, label %helper_crash_cleanup_return_merge_5

helper_crash_cleanup_retire:                      ; preds = %resource_drop_live_only
  %helper_crash_cleanup_retire_token = load i64, ptr %helper_crash_cleanup_token_5, align 8
  %helper_crash_cleanup_retire_call = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token)
  br i1 %helper_crash_cleanup_retire_call, label %helper_crash_cleanup_retire_accepted, label %helper_crash_cleanup_retire_rejected

helper_crash_cleanup_retire_merge:                ; preds = %helper_crash_cleanup_retire_accepted, %resource_drop_live_only
  %ref_drop_handle = load ptr, ptr %local_5, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle)
  store ptr null, ptr %local_5, align 8
  br label %resource_drop_merge

helper_crash_cleanup_retire_accepted:             ; preds = %helper_crash_cleanup_retire
  store i64 0, ptr %helper_crash_cleanup_token_5, align 8
  store i1 false, ptr %helper_crash_cleanup_active_5, align 1
  br label %helper_crash_cleanup_retire_merge

helper_crash_cleanup_retire_rejected:             ; preds = %helper_crash_cleanup_retire
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_return_merge_5:              ; preds = %helper_crash_cleanup_return_retire_5_accepted, %resource_drop_merge
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

helper_crash_cleanup_return_retire_5:             ; preds = %resource_drop_merge
  %helper_crash_cleanup_return_retire_5_call = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_return_token_5)
  br i1 %helper_crash_cleanup_return_retire_5_call, label %helper_crash_cleanup_return_retire_5_accepted, label %helper_crash_cleanup_return_retire_5_rejected

helper_crash_cleanup_return_retire_5_accepted:    ; preds = %helper_crash_cleanup_return_retire_5
  store i64 0, ptr %helper_crash_cleanup_token_5, align 8
  store i1 false, ptr %helper_crash_cleanup_active_5, align 1
  br label %helper_crash_cleanup_return_merge_5

helper_crash_cleanup_return_retire_5_rejected:    ; preds = %helper_crash_cleanup_return_retire_5
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable
}

define i64 @main() {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca %"Option$$Rc$lNode$g", align 8
  %local_5 = alloca i64, align 8
  %local_6 = alloca i64, align 8
  %local_7 = alloca i8, align 1
  %local_8 = alloca i64, align 8
  %local_9 = alloca i8, align 1
  %local_10 = alloca ptr, align 8
  %local_11 = alloca i64, align 8
  %local_12 = alloca i64, align 8
  %helper_crash_cleanup_token_1 = alloca i64, align 8
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_active_1 = alloca i1, align 1
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  %helper_crash_cleanup_token_4 = alloca i64, align 8
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_active_4 = alloca i1, align 1
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  store i64 -1, ptr %local_2, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_result = call ptr @build()
  store ptr %call_result, ptr %local_0, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %helper_crash_cleanup_was_active = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_was_active, label %helper_crash_cleanup_deactivate, label %helper_crash_cleanup_deactivate_merge

bb2:                                              ; preds = %after_cooperate80, %after_cooperate52
  %helper_crash_cleanup_drop_active = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active, label %helper_crash_cleanup_retire, label %helper_crash_cleanup_retire_merge

bb3:                                              ; preds = %frame_cleanup_registered11
  %machine_payload_ptr31 = getelementptr inbounds nuw %"Option$$Rc$lNode$g", ptr %local_4, i32 0, i32 1
  %machine_variant_field_ptr32 = getelementptr inbounds nuw { ptr }, ptr %machine_payload_ptr31, i32 0, i32 0
  %move_load33 = load ptr, ptr %machine_variant_field_ptr32, align 8
  store ptr %move_load33, ptr %local_10, align 8
  %helper_crash_cleanup_was_active34 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_was_active34, label %helper_crash_cleanup_deactivate35, label %helper_crash_cleanup_deactivate_merge36

bb4:                                              ; preds = %bb6
  store i64 1, ptr %local_12, align 8
  %move_load76 = load i64, ptr %local_12, align 8
  store i64 %move_load76, ptr %local_3, align 8
  %hew_actor_cooperate77 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel78 = icmp eq i32 %hew_actor_cooperate77, 2
  br i1 %hew_cooperate_is_cancel78, label %cancel_exit79, label %after_cooperate80

bb5:                                              ; preds = %bb6
  %helper_crash_cleanup_drop_active104 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active104, label %helper_crash_cleanup_retire105, label %helper_crash_cleanup_retire_merge106

bb6:                                              ; preds = %frame_cleanup_registered11
  store i64 1, ptr %local_8, align 8
  %cmp_lhs123 = load i64, ptr %local_5, align 8
  %cmp_rhs124 = load i64, ptr %local_8, align 8
  %cmp_bit125 = icmp eq i64 %cmp_lhs123, %cmp_rhs124
  %cmp_zext126 = zext i1 %cmp_bit125 to i8
  store i8 %cmp_zext126, ptr %local_9, align 1
  %cond_load127 = load i8, ptr %local_9, align 1
  %cond_nz128 = icmp ne i8 %cond_load127, 0
  br i1 %cond_nz128, label %bb4, label %bb5

cancel_exit:                                      ; preds = %entry
  %hew_runtime_exit_status_call = call i32 @hew_runtime_exit_status()
  %hew_runtime_faulted = icmp ne i32 %hew_runtime_exit_status_call, 0
  br i1 %hew_runtime_faulted, label %hew_exit_status_failed, label %hew_exit_status_continue

after_cooperate:                                  ; preds = %entry
  br label %bb0

hew_exit_status_failed:                           ; preds = %cancel_exit
  call void @hew_exit(i64 1)
  br label %hew_exit_status_continue

hew_exit_status_continue:                         ; preds = %hew_exit_status_failed, %cancel_exit
  ret i64 0

helper_crash_cleanup_deactivate:                  ; preds = %bb1
  %helper_crash_cleanup_token = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_deactivate_call = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token)
  br i1 %helper_crash_cleanup_deactivate_call, label %helper_crash_cleanup_deactivate_accepted, label %helper_crash_cleanup_deactivate_rejected

helper_crash_cleanup_deactivate_merge:            ; preds = %helper_crash_cleanup_deactivate_accepted, %bb1
  %move_load = load ptr, ptr %local_0, align 8
  store ptr %move_load, ptr %local_1, align 8
  store i64 0, ptr %local_2, align 8
  %helper_crash_cleanup_arm_guard = load i64, ptr %local_2, align 8
  %helper_crash_cleanup_arm_guard_live = icmp eq i64 %helper_crash_cleanup_arm_guard, 0
  br i1 %helper_crash_cleanup_arm_guard_live, label %helper_crash_cleanup_guard_live, label %helper_crash_cleanup_guard_merge

helper_crash_cleanup_deactivate_accepted:         ; preds = %helper_crash_cleanup_deactivate
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_deactivate_merge

helper_crash_cleanup_deactivate_rejected:         ; preds = %helper_crash_cleanup_deactivate
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_guard_live:                  ; preds = %helper_crash_cleanup_deactivate_merge
  %helper_crash_cleanup_prior_token = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %arm_typed_crash_cleanup = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token, ptr %local_1, i64 8, i64 8, ptr @__hew_frame_cleanup_5357928e581d821b, i32 1, i32 0)
  %frame_cleanup_arm_failed = icmp eq i64 %arm_typed_crash_cleanup, -1
  br i1 %frame_cleanup_arm_failed, label %frame_cleanup_rejected, label %frame_cleanup_registered

helper_crash_cleanup_guard_merge:                 ; preds = %frame_cleanup_registered, %helper_crash_cleanup_deactivate_merge
  %helper_crash_cleanup_was_active1 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_was_active1, label %helper_crash_cleanup_deactivate2, label %helper_crash_cleanup_deactivate_merge3

frame_cleanup_registered:                         ; preds = %helper_crash_cleanup_guard_live
  store i64 %arm_typed_crash_cleanup, ptr %helper_crash_cleanup_token_1, align 8
  store i1 true, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_guard_merge

frame_cleanup_rejected:                           ; preds = %helper_crash_cleanup_guard_live
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_deactivate2:                 ; preds = %helper_crash_cleanup_guard_merge
  %helper_crash_cleanup_token4 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_deactivate_call5 = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token4)
  br i1 %helper_crash_cleanup_deactivate_call5, label %helper_crash_cleanup_deactivate_accepted6, label %helper_crash_cleanup_deactivate_rejected7

helper_crash_cleanup_deactivate_merge3:           ; preds = %helper_crash_cleanup_deactivate_accepted6, %helper_crash_cleanup_guard_merge
  %weak_upgrade_handle = load ptr, ptr %local_1, align 8
  %weak_upgrade = call ptr @hew_weak_upgrade_rc(ptr %weak_upgrade_handle)
  %machine_tag_ptr = getelementptr inbounds nuw %"Option$$Rc$lNode$g", ptr %local_4, i32 0, i32 0
  %machine_payload_ptr = getelementptr inbounds nuw %"Option$$Rc$lNode$g", ptr %local_4, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { ptr }, ptr %machine_payload_ptr, i32 0, i32 0
  %weak_upgrade_is_none = icmp eq ptr %weak_upgrade, null
  br i1 %weak_upgrade_is_none, label %weak_upgrade_none, label %weak_upgrade_some

helper_crash_cleanup_deactivate_accepted6:        ; preds = %helper_crash_cleanup_deactivate2
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_deactivate_merge3

helper_crash_cleanup_deactivate_rejected7:        ; preds = %helper_crash_cleanup_deactivate2
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

weak_upgrade_some:                                ; preds = %helper_crash_cleanup_deactivate_merge3
  store i8 0, ptr %machine_tag_ptr, align 1
  store ptr %weak_upgrade, ptr %machine_variant_field_ptr, align 8
  br label %weak_upgrade_cont

weak_upgrade_none:                                ; preds = %helper_crash_cleanup_deactivate_merge3
  store i8 1, ptr %machine_tag_ptr, align 1
  store ptr null, ptr %machine_variant_field_ptr, align 8
  br label %weak_upgrade_cont

weak_upgrade_cont:                                ; preds = %weak_upgrade_none, %weak_upgrade_some
  %helper_crash_cleanup_prior_token8 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %arm_typed_crash_cleanup9 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token8, ptr %local_4, i64 16, i64 8, ptr @__hew_frame_cleanup_efca22a2c2039eb3, i32 1, i32 0)
  %frame_cleanup_arm_failed10 = icmp eq i64 %arm_typed_crash_cleanup9, -1
  br i1 %frame_cleanup_arm_failed10, label %frame_cleanup_rejected12, label %frame_cleanup_registered11

frame_cleanup_registered11:                       ; preds = %weak_upgrade_cont
  store i64 %arm_typed_crash_cleanup9, ptr %helper_crash_cleanup_token_4, align 8
  store i1 true, ptr %helper_crash_cleanup_active_4, align 1
  %machine_tag_ptr13 = getelementptr inbounds nuw %"Option$$Rc$lNode$g", ptr %local_4, i32 0, i32 0
  %move_iN_load = load i8, ptr %machine_tag_ptr13, align 1
  %move_iN_zext = zext i8 %move_iN_load to i64
  store i64 %move_iN_zext, ptr %local_5, align 8
  store i64 0, ptr %local_6, align 8
  %cmp_lhs = load i64, ptr %local_5, align 8
  %cmp_rhs = load i64, ptr %local_6, align 8
  %cmp_bit = icmp eq i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_7, align 1
  %cond_load = load i8, ptr %local_7, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb3, label %bb6

frame_cleanup_rejected12:                         ; preds = %weak_upgrade_cont
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire:                      ; preds = %bb2
  %helper_crash_cleanup_retire_token = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token)
  br i1 %helper_crash_cleanup_retire_call, label %helper_crash_cleanup_retire_accepted, label %helper_crash_cleanup_retire_rejected

helper_crash_cleanup_retire_merge:                ; preds = %helper_crash_cleanup_retire_accepted, %bb2
  call void @"__hew_enum_drop_inplace_Option$$Rc$lNode$g"(ptr %local_4)
  store %"Option$$Rc$lNode$g" zeroinitializer, ptr %local_4, align 8
  %move_load14 = load i64, ptr %local_3, align 8
  store i64 %move_load14, ptr %return_slot, align 8
  %helper_crash_cleanup_drop_active15 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active15, label %helper_crash_cleanup_retire16, label %helper_crash_cleanup_retire_merge17

helper_crash_cleanup_retire_accepted:             ; preds = %helper_crash_cleanup_retire
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge

helper_crash_cleanup_retire_rejected:             ; preds = %helper_crash_cleanup_retire
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire16:                    ; preds = %helper_crash_cleanup_retire_merge
  %helper_crash_cleanup_retire_token18 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call19 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token18)
  br i1 %helper_crash_cleanup_retire_call19, label %helper_crash_cleanup_retire_accepted20, label %helper_crash_cleanup_retire_rejected21

helper_crash_cleanup_retire_merge17:              ; preds = %helper_crash_cleanup_retire_accepted20, %helper_crash_cleanup_retire_merge
  call void @"__hew_enum_drop_inplace_Option$$Rc$lNode$g"(ptr %local_4)
  %resource_drop_flag = load i64, ptr %local_2, align 8
  %resource_drop_not_consumed = icmp eq i64 %resource_drop_flag, 0
  br i1 %resource_drop_not_consumed, label %resource_drop_live_only, label %resource_drop_merge

helper_crash_cleanup_retire_accepted20:           ; preds = %helper_crash_cleanup_retire16
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge17

helper_crash_cleanup_retire_rejected21:           ; preds = %helper_crash_cleanup_retire16
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only:                          ; preds = %helper_crash_cleanup_retire_merge17
  %helper_crash_cleanup_drop_active22 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active22, label %helper_crash_cleanup_retire23, label %helper_crash_cleanup_retire_merge24

resource_drop_merge:                              ; preds = %helper_crash_cleanup_retire_merge24, %helper_crash_cleanup_retire_merge17
  %helper_crash_cleanup_return_token_1 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_return_has_token_1 = icmp ne i64 %helper_crash_cleanup_return_token_1, 0
  br i1 %helper_crash_cleanup_return_has_token_1, label %helper_crash_cleanup_return_retire_1, label %helper_crash_cleanup_return_merge_1

helper_crash_cleanup_retire23:                    ; preds = %resource_drop_live_only
  %helper_crash_cleanup_retire_token25 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call26 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token25)
  br i1 %helper_crash_cleanup_retire_call26, label %helper_crash_cleanup_retire_accepted27, label %helper_crash_cleanup_retire_rejected28

helper_crash_cleanup_retire_merge24:              ; preds = %helper_crash_cleanup_retire_accepted27, %resource_drop_live_only
  %ref_drop_handle = load ptr, ptr %local_1, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle)
  store ptr null, ptr %local_1, align 8
  br label %resource_drop_merge

helper_crash_cleanup_retire_accepted27:           ; preds = %helper_crash_cleanup_retire23
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge24

helper_crash_cleanup_retire_rejected28:           ; preds = %helper_crash_cleanup_retire23
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_return_merge_1:              ; preds = %helper_crash_cleanup_return_retire_1_accepted, %resource_drop_merge
  %helper_crash_cleanup_return_token_4 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_return_has_token_4 = icmp ne i64 %helper_crash_cleanup_return_token_4, 0
  br i1 %helper_crash_cleanup_return_has_token_4, label %helper_crash_cleanup_return_retire_4, label %helper_crash_cleanup_return_merge_4

helper_crash_cleanup_return_retire_1:             ; preds = %resource_drop_merge
  %helper_crash_cleanup_return_retire_1_call = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_return_token_1)
  br i1 %helper_crash_cleanup_return_retire_1_call, label %helper_crash_cleanup_return_retire_1_accepted, label %helper_crash_cleanup_return_retire_1_rejected

helper_crash_cleanup_return_retire_1_accepted:    ; preds = %helper_crash_cleanup_return_retire_1
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_return_merge_1

helper_crash_cleanup_return_retire_1_rejected:    ; preds = %helper_crash_cleanup_return_retire_1
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_return_merge_4:              ; preds = %helper_crash_cleanup_return_retire_4_accepted, %helper_crash_cleanup_return_merge_1
  %hew_lambda_drain_all_call = call i32 @hew_lambda_drain_all(i64 0)
  %hew_lambda_drain_failed = icmp ne i32 %hew_lambda_drain_all_call, 0
  %hew_runtime_exit_status_call29 = call i32 @hew_runtime_exit_status()
  %hew_runtime_faulted30 = icmp ne i32 %hew_runtime_exit_status_call29, 0
  %hew_exit_any_failed = or i1 %hew_lambda_drain_failed, %hew_runtime_faulted30
  br i1 %hew_exit_any_failed, label %hew_shutdown_exit_failed, label %hew_shutdown_exit_continue

helper_crash_cleanup_return_retire_4:             ; preds = %helper_crash_cleanup_return_merge_1
  %helper_crash_cleanup_return_retire_4_call = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_return_token_4)
  br i1 %helper_crash_cleanup_return_retire_4_call, label %helper_crash_cleanup_return_retire_4_accepted, label %helper_crash_cleanup_return_retire_4_rejected

helper_crash_cleanup_return_retire_4_accepted:    ; preds = %helper_crash_cleanup_return_retire_4
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_return_merge_4

helper_crash_cleanup_return_retire_4_rejected:    ; preds = %helper_crash_cleanup_return_retire_4
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

hew_shutdown_exit_failed:                         ; preds = %helper_crash_cleanup_return_merge_4
  %hew_exit_user_code = load i64, ptr %return_slot, align 8
  %hew_exit_user_code_set = icmp ne i64 %hew_exit_user_code, 0
  %hew_exit_status_code = select i1 %hew_exit_user_code_set, i64 %hew_exit_user_code, i64 1
  call void @hew_exit(i64 %hew_exit_status_code)
  br label %hew_shutdown_exit_continue

hew_shutdown_exit_continue:                       ; preds = %hew_shutdown_exit_failed, %helper_crash_cleanup_return_merge_4
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val

helper_crash_cleanup_deactivate35:                ; preds = %bb3
  %helper_crash_cleanup_token37 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_deactivate_call38 = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token37)
  br i1 %helper_crash_cleanup_deactivate_call38, label %helper_crash_cleanup_deactivate_accepted39, label %helper_crash_cleanup_deactivate_rejected40

helper_crash_cleanup_deactivate_merge36:          ; preds = %helper_crash_cleanup_deactivate_accepted39, %bb3
  %machine_payload_ptr41 = getelementptr inbounds nuw %"Option$$Rc$lNode$g", ptr %local_4, i32 0, i32 1
  %machine_variant_field_ptr42 = getelementptr inbounds nuw { ptr }, ptr %machine_payload_ptr41, i32 0, i32 0
  store ptr null, ptr %machine_variant_field_ptr42, align 8
  %helper_crash_cleanup_prior_token43 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %arm_typed_crash_cleanup44 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token43, ptr %local_4, i64 16, i64 8, ptr @__hew_frame_cleanup_efca22a2c2039eb3, i32 1, i32 0)
  %frame_cleanup_arm_failed45 = icmp eq i64 %arm_typed_crash_cleanup44, -1
  br i1 %frame_cleanup_arm_failed45, label %frame_cleanup_rejected47, label %frame_cleanup_registered46

helper_crash_cleanup_deactivate_accepted39:       ; preds = %helper_crash_cleanup_deactivate35
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_deactivate_merge36

helper_crash_cleanup_deactivate_rejected40:       ; preds = %helper_crash_cleanup_deactivate35
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

frame_cleanup_registered46:                       ; preds = %helper_crash_cleanup_deactivate_merge36
  store i64 %arm_typed_crash_cleanup44, ptr %helper_crash_cleanup_token_4, align 8
  store i1 true, ptr %helper_crash_cleanup_active_4, align 1
  store i64 9000, ptr %local_11, align 8
  %move_load48 = load i64, ptr %local_11, align 8
  store i64 %move_load48, ptr %local_3, align 8
  %"hew_rc_drop drop" = load ptr, ptr %local_10, align 8
  call void @hew_rc_drop(ptr %"hew_rc_drop drop")
  store ptr null, ptr %local_10, align 8
  %hew_actor_cooperate49 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel50 = icmp eq i32 %hew_actor_cooperate49, 2
  br i1 %hew_cooperate_is_cancel50, label %cancel_exit51, label %after_cooperate52

frame_cleanup_rejected47:                         ; preds = %helper_crash_cleanup_deactivate_merge36
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

cancel_exit51:                                    ; preds = %frame_cleanup_registered46
  %helper_crash_cleanup_drop_active53 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active53, label %helper_crash_cleanup_retire54, label %helper_crash_cleanup_retire_merge55

after_cooperate52:                                ; preds = %frame_cleanup_registered46
  br label %bb2

helper_crash_cleanup_retire54:                    ; preds = %cancel_exit51
  %helper_crash_cleanup_retire_token56 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call57 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token56)
  br i1 %helper_crash_cleanup_retire_call57, label %helper_crash_cleanup_retire_accepted58, label %helper_crash_cleanup_retire_rejected59

helper_crash_cleanup_retire_merge55:              ; preds = %helper_crash_cleanup_retire_accepted58, %cancel_exit51
  call void @"__hew_enum_drop_inplace_Option$$Rc$lNode$g"(ptr %local_4)
  %resource_drop_flag60 = load i64, ptr %local_2, align 8
  %resource_drop_not_consumed61 = icmp eq i64 %resource_drop_flag60, 0
  br i1 %resource_drop_not_consumed61, label %resource_drop_live_only62, label %resource_drop_merge63

helper_crash_cleanup_retire_accepted58:           ; preds = %helper_crash_cleanup_retire54
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge55

helper_crash_cleanup_retire_rejected59:           ; preds = %helper_crash_cleanup_retire54
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only62:                        ; preds = %helper_crash_cleanup_retire_merge55
  %helper_crash_cleanup_drop_active64 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active64, label %helper_crash_cleanup_retire65, label %helper_crash_cleanup_retire_merge66

resource_drop_merge63:                            ; preds = %helper_crash_cleanup_retire_merge66, %helper_crash_cleanup_retire_merge55
  %hew_runtime_exit_status_call72 = call i32 @hew_runtime_exit_status()
  %hew_runtime_faulted73 = icmp ne i32 %hew_runtime_exit_status_call72, 0
  br i1 %hew_runtime_faulted73, label %hew_exit_status_failed74, label %hew_exit_status_continue75

helper_crash_cleanup_retire65:                    ; preds = %resource_drop_live_only62
  %helper_crash_cleanup_retire_token67 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call68 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token67)
  br i1 %helper_crash_cleanup_retire_call68, label %helper_crash_cleanup_retire_accepted69, label %helper_crash_cleanup_retire_rejected70

helper_crash_cleanup_retire_merge66:              ; preds = %helper_crash_cleanup_retire_accepted69, %resource_drop_live_only62
  %ref_drop_handle71 = load ptr, ptr %local_1, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle71)
  store ptr null, ptr %local_1, align 8
  br label %resource_drop_merge63

helper_crash_cleanup_retire_accepted69:           ; preds = %helper_crash_cleanup_retire65
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge66

helper_crash_cleanup_retire_rejected70:           ; preds = %helper_crash_cleanup_retire65
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

hew_exit_status_failed74:                         ; preds = %resource_drop_merge63
  call void @hew_exit(i64 1)
  br label %hew_exit_status_continue75

hew_exit_status_continue75:                       ; preds = %hew_exit_status_failed74, %resource_drop_merge63
  ret i64 0

cancel_exit79:                                    ; preds = %bb4
  %helper_crash_cleanup_drop_active81 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active81, label %helper_crash_cleanup_retire82, label %helper_crash_cleanup_retire_merge83

after_cooperate80:                                ; preds = %bb4
  br label %bb2

helper_crash_cleanup_retire82:                    ; preds = %cancel_exit79
  %helper_crash_cleanup_retire_token84 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call85 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token84)
  br i1 %helper_crash_cleanup_retire_call85, label %helper_crash_cleanup_retire_accepted86, label %helper_crash_cleanup_retire_rejected87

helper_crash_cleanup_retire_merge83:              ; preds = %helper_crash_cleanup_retire_accepted86, %cancel_exit79
  call void @"__hew_enum_drop_inplace_Option$$Rc$lNode$g"(ptr %local_4)
  %resource_drop_flag88 = load i64, ptr %local_2, align 8
  %resource_drop_not_consumed89 = icmp eq i64 %resource_drop_flag88, 0
  br i1 %resource_drop_not_consumed89, label %resource_drop_live_only90, label %resource_drop_merge91

helper_crash_cleanup_retire_accepted86:           ; preds = %helper_crash_cleanup_retire82
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge83

helper_crash_cleanup_retire_rejected87:           ; preds = %helper_crash_cleanup_retire82
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only90:                        ; preds = %helper_crash_cleanup_retire_merge83
  %helper_crash_cleanup_drop_active92 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active92, label %helper_crash_cleanup_retire93, label %helper_crash_cleanup_retire_merge94

resource_drop_merge91:                            ; preds = %helper_crash_cleanup_retire_merge94, %helper_crash_cleanup_retire_merge83
  %hew_runtime_exit_status_call100 = call i32 @hew_runtime_exit_status()
  %hew_runtime_faulted101 = icmp ne i32 %hew_runtime_exit_status_call100, 0
  br i1 %hew_runtime_faulted101, label %hew_exit_status_failed102, label %hew_exit_status_continue103

helper_crash_cleanup_retire93:                    ; preds = %resource_drop_live_only90
  %helper_crash_cleanup_retire_token95 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call96 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token95)
  br i1 %helper_crash_cleanup_retire_call96, label %helper_crash_cleanup_retire_accepted97, label %helper_crash_cleanup_retire_rejected98

helper_crash_cleanup_retire_merge94:              ; preds = %helper_crash_cleanup_retire_accepted97, %resource_drop_live_only90
  %ref_drop_handle99 = load ptr, ptr %local_1, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle99)
  store ptr null, ptr %local_1, align 8
  br label %resource_drop_merge91

helper_crash_cleanup_retire_accepted97:           ; preds = %helper_crash_cleanup_retire93
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge94

helper_crash_cleanup_retire_rejected98:           ; preds = %helper_crash_cleanup_retire93
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

hew_exit_status_failed102:                        ; preds = %resource_drop_merge91
  call void @hew_exit(i64 1)
  br label %hew_exit_status_continue103

hew_exit_status_continue103:                      ; preds = %hew_exit_status_failed102, %resource_drop_merge91
  ret i64 0

helper_crash_cleanup_retire105:                   ; preds = %bb5
  %helper_crash_cleanup_retire_token107 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call108 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token107)
  br i1 %helper_crash_cleanup_retire_call108, label %helper_crash_cleanup_retire_accepted109, label %helper_crash_cleanup_retire_rejected110

helper_crash_cleanup_retire_merge106:             ; preds = %helper_crash_cleanup_retire_accepted109, %bb5
  call void @"__hew_enum_drop_inplace_Option$$Rc$lNode$g"(ptr %local_4)
  %resource_drop_flag111 = load i64, ptr %local_2, align 8
  %resource_drop_not_consumed112 = icmp eq i64 %resource_drop_flag111, 0
  br i1 %resource_drop_not_consumed112, label %resource_drop_live_only113, label %resource_drop_merge114

helper_crash_cleanup_retire_accepted109:          ; preds = %helper_crash_cleanup_retire105
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge106

helper_crash_cleanup_retire_rejected110:          ; preds = %helper_crash_cleanup_retire105
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only113:                       ; preds = %helper_crash_cleanup_retire_merge106
  %helper_crash_cleanup_drop_active115 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active115, label %helper_crash_cleanup_retire116, label %helper_crash_cleanup_retire_merge117

resource_drop_merge114:                           ; preds = %helper_crash_cleanup_retire_merge117, %helper_crash_cleanup_retire_merge106
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire116:                   ; preds = %resource_drop_live_only113
  %helper_crash_cleanup_retire_token118 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call119 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token118)
  br i1 %helper_crash_cleanup_retire_call119, label %helper_crash_cleanup_retire_accepted120, label %helper_crash_cleanup_retire_rejected121

helper_crash_cleanup_retire_merge117:             ; preds = %helper_crash_cleanup_retire_accepted120, %resource_drop_live_only113
  %ref_drop_handle122 = load ptr, ptr %local_1, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle122)
  store ptr null, ptr %local_1, align 8
  br label %resource_drop_merge114

helper_crash_cleanup_retire_accepted120:          ; preds = %helper_crash_cleanup_retire116
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge117

helper_crash_cleanup_retire_rejected121:          ; preds = %helper_crash_cleanup_retire116
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable
}

define internal ptr @"i8::fmt"(i8 %0) {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca i8, align 1
  %local_1 = alloca i32, align 4
  %local_2 = alloca ptr, align 8
  store i8 %0, ptr %local_0, align 1
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %cast_int_src = load i8, ptr %local_0, align 1
  %cast_int_sext = sext i8 %cast_int_src to i32
  store i32 %cast_int_sext, ptr %local_1, align 4
  %call_arg = load i32, ptr %local_1, align 4
  %call_result = call ptr @hew_int_to_string(i32 %call_arg)
  store ptr %call_result, ptr %local_2, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_2, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"i16::fmt"(i16 %0) {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca i16, align 2
  %local_1 = alloca i32, align 4
  %local_2 = alloca ptr, align 8
  store i16 %0, ptr %local_0, align 2
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %cast_int_src = load i16, ptr %local_0, align 2
  %cast_int_sext = sext i16 %cast_int_src to i32
  store i32 %cast_int_sext, ptr %local_1, align 4
  %call_arg = load i32, ptr %local_1, align 4
  %call_result = call ptr @hew_int_to_string(i32 %call_arg)
  store ptr %call_result, ptr %local_2, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_2, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"i32::fmt"(i32 %0) {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca i32, align 4
  %local_1 = alloca ptr, align 8
  store i32 %0, ptr %local_0, align 4
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load i32, ptr %local_0, align 4
  %call_result = call ptr @hew_int_to_string(i32 %call_arg)
  store ptr %call_result, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"i64::fmt"(i64 %0) {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca i64, align 8
  %local_1 = alloca ptr, align 8
  store i64 %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load i64, ptr %local_0, align 8
  %call_result = call ptr @hew_i64_to_string(i64 %call_arg)
  store ptr %call_result, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"u8::fmt"(i8 %0) {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca i8, align 1
  %local_1 = alloca ptr, align 8
  store i8 %0, ptr %local_0, align 1
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load i8, ptr %local_0, align 1
  %call_result = call ptr @hew_u8_to_string(i8 %call_arg)
  store ptr %call_result, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"u16::fmt"(i16 %0) {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca i16, align 2
  %local_1 = alloca ptr, align 8
  store i16 %0, ptr %local_0, align 2
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load i16, ptr %local_0, align 2
  %ffi_zext = zext i16 %call_arg to i32
  %call_result = call ptr @hew_uint_to_string(i32 %ffi_zext)
  store ptr %call_result, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"u32::fmt"(i32 %0) {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca i32, align 4
  %local_1 = alloca ptr, align 8
  store i32 %0, ptr %local_0, align 4
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load i32, ptr %local_0, align 4
  %call_result = call ptr @hew_uint_to_string(i32 %call_arg)
  store ptr %call_result, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"u64::fmt"(i64 %0) {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca i64, align 8
  %local_1 = alloca ptr, align 8
  store i64 %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load i64, ptr %local_0, align 8
  %call_result = call ptr @hew_u64_to_string(i64 %call_arg)
  store ptr %call_result, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"isize::fmt"(i64 %0) {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca i64, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca ptr, align 8
  store i64 %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %cast_int_src = load i64, ptr %local_0, align 8
  store i64 %cast_int_src, ptr %local_1, align 8
  %call_arg = load i64, ptr %local_1, align 8
  %call_result = call ptr @hew_i64_to_string(i64 %call_arg)
  store ptr %call_result, ptr %local_2, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_2, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"usize::fmt"(i64 %0) {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca i64, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca ptr, align 8
  store i64 %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %cast_int_src = load i64, ptr %local_0, align 8
  store i64 %cast_int_src, ptr %local_1, align 8
  %call_arg = load i64, ptr %local_1, align 8
  %call_result = call ptr @hew_u64_to_string(i64 %call_arg)
  store ptr %call_result, ptr %local_2, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_2, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"bool::fmt"(i8 %0) {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca i8, align 1
  %local_1 = alloca ptr, align 8
  store i8 %0, ptr %local_0, align 1
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load i8, ptr %local_0, align 1
  %call_result = call ptr @hew_bool_to_string(i8 %call_arg)
  store ptr %call_result, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"char::fmt"(i32 %0) {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca i32, align 4
  %local_1 = alloca ptr, align 8
  store i32 %0, ptr %local_0, align 4
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load i32, ptr %local_0, align 4
  %call_result = call ptr @hew_char_to_string(i32 %call_arg)
  store ptr %call_result, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"f64::fmt"(double %0) {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca double, align 8
  %local_1 = alloca ptr, align 8
  store double %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load double, ptr %local_0, align 8
  %call_result = call ptr @hew_float_to_string(double %call_arg)
  store ptr %call_result, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"f32::fmt"(float %0) {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca float, align 4
  %local_1 = alloca double, align 8
  %local_2 = alloca ptr, align 8
  store float %0, ptr %local_0, align 4
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %cast_float_src = load float, ptr %local_0, align 4
  %cast_float_ext = fpext float %cast_float_src to double
  store double %cast_float_ext, ptr %local_1, align 8
  %call_arg = load double, ptr %local_1, align 8
  %call_result = call ptr @hew_float_to_string(double %call_arg)
  store ptr %call_result, ptr %local_2, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_2, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"string::fmt"(ptr %0) {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca ptr, align 8
  store ptr %0, ptr %local_0, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  %mir_share_string_load = load ptr, ptr %local_0, align 8
  %mir_share_string_retain = call ptr @hew_string_clone(ptr %mir_share_string_load)
  %move_load = load ptr, ptr %local_0, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val
}

define internal i64 @"duration::from_nanos"(i64 %0) {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca i64, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i8, align 1
  store i64 %0, ptr %local_0, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  store i64 1, ptr %local_1, align 8
  %checked_lhs = load i64, ptr %local_0, align 8
  %checked_rhs = load i64, ptr %local_1, align 8
  %with_overflow = call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_2, align 8
  store i8 %checked_overflow_widen, ptr %local_3, align 1
  %cond_load = load i8, ptr %local_3, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb2:                                              ; preds = %bb0
  %move_load = load i64, ptr %local_2, align 8
  store i64 %move_load, ptr %return_slot, align 8
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val
}

define internal i64 @"duration::from_micros"(i64 %0) {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca i64, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i8, align 1
  store i64 %0, ptr %local_0, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  store i64 1000, ptr %local_1, align 8
  %checked_lhs = load i64, ptr %local_0, align 8
  %checked_rhs = load i64, ptr %local_1, align 8
  %with_overflow = call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_2, align 8
  store i8 %checked_overflow_widen, ptr %local_3, align 1
  %cond_load = load i8, ptr %local_3, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb2:                                              ; preds = %bb0
  %move_load = load i64, ptr %local_2, align 8
  store i64 %move_load, ptr %return_slot, align 8
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val
}

define internal i64 @"duration::from_millis"(i64 %0) {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca i64, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i8, align 1
  store i64 %0, ptr %local_0, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  store i64 1000000, ptr %local_1, align 8
  %checked_lhs = load i64, ptr %local_0, align 8
  %checked_rhs = load i64, ptr %local_1, align 8
  %with_overflow = call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_2, align 8
  store i8 %checked_overflow_widen, ptr %local_3, align 1
  %cond_load = load i8, ptr %local_3, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb2:                                              ; preds = %bb0
  %move_load = load i64, ptr %local_2, align 8
  store i64 %move_load, ptr %return_slot, align 8
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val
}

define internal i64 @"duration::from_secs"(i64 %0) {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca i64, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i8, align 1
  store i64 %0, ptr %local_0, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  store i64 1000000000, ptr %local_1, align 8
  %checked_lhs = load i64, ptr %local_0, align 8
  %checked_rhs = load i64, ptr %local_1, align 8
  %with_overflow = call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_2, align 8
  store i8 %checked_overflow_widen, ptr %local_3, align 1
  %cond_load = load i8, ptr %local_3, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb2:                                              ; preds = %bb0
  %move_load = load i64, ptr %local_2, align 8
  store i64 %move_load, ptr %return_slot, align 8
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val
}

define internal ptr @"duration::fmt"(i64 %0) {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca i64, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca ptr, align 8
  %local_3 = alloca ptr, align 8
  %local_4 = alloca ptr, align 8
  store i64 %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load i64, ptr %local_0, align 8
  %call_result = call i64 @hew_duration_nanos(i64 %call_arg)
  store i64 %call_result, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_arg1 = load i64, ptr %local_1, align 8
  %call_result2 = call ptr @hew_i64_to_string(i64 %call_arg1)
  store ptr %call_result2, ptr %local_2, align 8
  br label %bb2

bb2:                                              ; preds = %bb1
  store ptr @str_lit.2, ptr %local_3, align 8
  %"hew_string_concat arg0" = load ptr, ptr %local_2, align 8
  %"hew_string_concat arg1" = load ptr, ptr %local_3, align 8
  %hew_string_concat_call = call ptr @hew_string_concat(ptr %"hew_string_concat arg0", ptr %"hew_string_concat arg1")
  store ptr %hew_string_concat_call, ptr %local_4, align 8
  %"hew_string_drop drop" = load ptr, ptr %local_2, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop")
  store ptr null, ptr %local_2, align 8
  %move_load = load ptr, ptr %local_4, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal i32 @"__hew_enum_clone_inplace_Option$$Rc$lNode$g"(ptr %0, ptr %1) {
entry:
  %enum_clone_tag_ptr = getelementptr inbounds nuw %"Option$$Rc$lNode$g", ptr %0, i32 0, i32 0
  %enum_clone_tag = load i8, ptr %enum_clone_tag_ptr, align 1
  switch i8 %enum_clone_tag, label %tag_oob_trap [
    i8 0, label %enum_clone_variant_0
    i8 1, label %enum_clone_variant_1
  ]

success:                                          ; preds = %enum_clone_variant_1, %enum_clone_v0_store_0
  ret i32 0

fail:                                             ; preds = %enum_clone_v0_rb_0
  ret i32 1

tag_oob_trap:                                     ; preds = %entry
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

enum_clone_variant_0:                             ; preds = %entry
  %enum_clone_src_payload_0 = getelementptr inbounds nuw %"Option$$Rc$lNode$g", ptr %0, i32 0, i32 1
  %enum_clone_dst_payload_0 = getelementptr inbounds nuw %"Option$$Rc$lNode$g", ptr %1, i32 0, i32 1
  br label %enum_clone_v0_step_0

enum_clone_variant_1:                             ; preds = %entry
  br label %success

enum_clone_v0_step_0:                             ; preds = %enum_clone_variant_0
  %src_f0_ptr = getelementptr inbounds nuw { ptr }, ptr %enum_clone_src_payload_0, i32 0, i32 0
  %src_f0 = load ptr, ptr %src_f0_ptr, align 8
  %clone_helper_f0 = call ptr @hew_rc_clone(ptr %src_f0)
  %cloned_f0_int = ptrtoint ptr %clone_helper_f0 to i64
  %cloned_f0_null = icmp eq i64 %cloned_f0_int, 0
  br i1 %cloned_f0_null, label %enum_clone_v0_rb_0, label %enum_clone_v0_store_0

enum_clone_v0_store_0:                            ; preds = %enum_clone_v0_step_0
  %dst_f0_ptr = getelementptr inbounds nuw { ptr }, ptr %enum_clone_dst_payload_0, i32 0, i32 0
  store ptr %clone_helper_f0, ptr %dst_f0_ptr, align 8
  br label %success

enum_clone_v0_rb_0:                               ; preds = %enum_clone_v0_step_0
  br label %fail
}

declare void @hew_trap_with_code(i32)

; Function Attrs: cold noreturn nounwind memory(inaccessiblemem: write)
declare void @llvm.trap() #0

declare ptr @hew_rc_clone(ptr)

define internal void @"__hew_enum_drop_inplace_Option$$Rc$lNode$g"(ptr %0) {
entry:
  %enum_drop_tag_ptr = getelementptr inbounds nuw %"Option$$Rc$lNode$g", ptr %0, i32 0, i32 0
  %enum_drop_tag = load i8, ptr %enum_drop_tag_ptr, align 1
  switch i8 %enum_drop_tag, label %tag_oob_trap [
    i8 0, label %enum_drop_variant_0
    i8 1, label %enum_drop_variant_1
  ]

done:                                             ; preds = %enum_drop_variant_1, %enum_drop_variant_0
  ret void

tag_oob_trap:                                     ; preds = %entry
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

enum_drop_variant_0:                              ; preds = %entry
  %enum_drop_payload_0 = getelementptr inbounds nuw %"Option$$Rc$lNode$g", ptr %0, i32 0, i32 1
  %drop_rc_f0_ptr = getelementptr inbounds nuw { ptr }, ptr %enum_drop_payload_0, i32 0, i32 0
  %drop_rc_f0 = load ptr, ptr %drop_rc_f0_ptr, align 8
  call void @hew_rc_drop(ptr %drop_rc_f0)
  store ptr null, ptr %drop_rc_f0_ptr, align 8
  br label %done

enum_drop_variant_1:                              ; preds = %entry
  %enum_drop_payload_1 = getelementptr inbounds nuw %"Option$$Rc$lNode$g", ptr %0, i32 0, i32 1
  br label %done
}

declare void @hew_rc_drop(ptr)

define internal void @"__hew_enum_overwrite_release_Option$$Rc$lNode$g"(ptr %0, ptr %1) {
entry:
  %ow_slot_0 = alloca ptr, align 8
  store ptr null, ptr %ow_slot_0, align 8
  %"ow_new_d0_Option$$Rc$lNode$g_tag_ptr" = getelementptr inbounds nuw %"Option$$Rc$lNode$g", ptr %1, i32 0, i32 0
  %"ow_new_d0_Option$$Rc$lNode$g_tag" = load i8, ptr %"ow_new_d0_Option$$Rc$lNode$g_tag_ptr", align 1
  switch i8 %"ow_new_d0_Option$$Rc$lNode$g_tag", label %"ow_new_d0_Option$$Rc$lNode$g_tag_oob" [
    i8 0, label %"ow_new_d0_Option$$Rc$lNode$g_v0"
    i8 1, label %"ow_new_d0_Option$$Rc$lNode$g_v1"
  ]

"ow_new_d0_Option$$Rc$lNode$g_merge":             ; preds = %"ow_new_d0_Option$$Rc$lNode$g_v1", %"ow_new_d0_Option$$Rc$lNode$g_v0"
  %"ow_old_d0_Option$$Rc$lNode$g_tag_ptr" = getelementptr inbounds nuw %"Option$$Rc$lNode$g", ptr %0, i32 0, i32 0
  %"ow_old_d0_Option$$Rc$lNode$g_tag" = load i8, ptr %"ow_old_d0_Option$$Rc$lNode$g_tag_ptr", align 1
  switch i8 %"ow_old_d0_Option$$Rc$lNode$g_tag", label %"ow_old_d0_Option$$Rc$lNode$g_tag_oob" [
    i8 0, label %"ow_old_d0_Option$$Rc$lNode$g_v0"
    i8 1, label %"ow_old_d0_Option$$Rc$lNode$g_v1"
  ]

"ow_new_d0_Option$$Rc$lNode$g_tag_oob":           ; preds = %entry
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

"ow_new_d0_Option$$Rc$lNode$g_v0":                ; preds = %entry
  %"ow_new_d0_Option$$Rc$lNode$g_v0_payload" = getelementptr inbounds nuw %"Option$$Rc$lNode$g", ptr %1, i32 0, i32 1
  %ow_new_d0_f0_ptr = getelementptr inbounds nuw { ptr }, ptr %"ow_new_d0_Option$$Rc$lNode$g_v0_payload", i32 0, i32 0
  %ow_new_d0_f0_leaf = load ptr, ptr %ow_new_d0_f0_ptr, align 8
  store ptr %ow_new_d0_f0_leaf, ptr %ow_slot_0, align 8
  br label %"ow_new_d0_Option$$Rc$lNode$g_merge"

"ow_new_d0_Option$$Rc$lNode$g_v1":                ; preds = %entry
  %"ow_new_d0_Option$$Rc$lNode$g_v1_payload" = getelementptr inbounds nuw %"Option$$Rc$lNode$g", ptr %1, i32 0, i32 1
  br label %"ow_new_d0_Option$$Rc$lNode$g_merge"

"ow_old_d0_Option$$Rc$lNode$g_merge":             ; preds = %"ow_old_d0_Option$$Rc$lNode$g_v1", %"ow_old_d0_Option$$Rc$lNode$g_v0"
  call void @"__hew_enum_drop_inplace_Option$$Rc$lNode$g"(ptr %0)
  ret void

"ow_old_d0_Option$$Rc$lNode$g_tag_oob":           ; preds = %"ow_new_d0_Option$$Rc$lNode$g_merge"
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

"ow_old_d0_Option$$Rc$lNode$g_v0":                ; preds = %"ow_new_d0_Option$$Rc$lNode$g_merge"
  %"ow_old_d0_Option$$Rc$lNode$g_v0_payload" = getelementptr inbounds nuw %"Option$$Rc$lNode$g", ptr %0, i32 0, i32 1
  %ow_old_d0_f0_ptr = getelementptr inbounds nuw { ptr }, ptr %"ow_old_d0_Option$$Rc$lNode$g_v0_payload", i32 0, i32 0
  %ow_old_d0_f0_val = load ptr, ptr %ow_old_d0_f0_ptr, align 8
  %ow_old_d0_f0_int = ptrtoint ptr %ow_old_d0_f0_val to i64
  %ow_old_d0_f0_cmp0_leaf = load ptr, ptr %ow_slot_0, align 8
  %ow_old_d0_f0_cmp0_int = ptrtoint ptr %ow_old_d0_f0_cmp0_leaf to i64
  %ow_old_d0_f0_cmp0_eq = icmp eq i64 %ow_old_d0_f0_int, %ow_old_d0_f0_cmp0_int
  %ow_old_d0_f0_matched0 = or i1 false, %ow_old_d0_f0_cmp0_eq
  %ow_old_d0_f0_neutralized = select i1 %ow_old_d0_f0_matched0, ptr null, ptr %ow_old_d0_f0_val
  store ptr %ow_old_d0_f0_neutralized, ptr %ow_old_d0_f0_ptr, align 8
  br label %"ow_old_d0_Option$$Rc$lNode$g_merge"

"ow_old_d0_Option$$Rc$lNode$g_v1":                ; preds = %"ow_new_d0_Option$$Rc$lNode$g_merge"
  %"ow_old_d0_Option$$Rc$lNode$g_v1_payload" = getelementptr inbounds nuw %"Option$$Rc$lNode$g", ptr %0, i32 0, i32 1
  br label %"ow_old_d0_Option$$Rc$lNode$g_merge"
}

declare i32 @hew_actor_cooperate()

define internal void @__hew_rc_payload_drop_Node(ptr %0) {
entry:
  call void @__hew_record_drop_inplace_Node(ptr %0)
  ret void
}

define internal void @__hew_record_drop_inplace_Node(ptr %0) {
entry:
  %rec_int = ptrtoint ptr %0 to i64
  %rec_is_null = icmp eq i64 %rec_int, 0
  br i1 %rec_is_null, label %done, label %do_drop

do_drop:                                          ; preds = %entry
  %drop_f1_ptr = getelementptr inbounds nuw %Node, ptr %0, i32 0, i32 1
  call void @"__hew_enum_drop_inplace_Option$$Weak$lNode$g"(ptr %drop_f1_ptr)
  %drop_f0_ptr = getelementptr inbounds nuw %Node, ptr %0, i32 0, i32 0
  %drop_f0 = load ptr, ptr %drop_f0_ptr, align 8
  call void @hew_string_drop(ptr %drop_f0)
  br label %done

done:                                             ; preds = %do_drop, %entry
  ret void
}

define internal void @"__hew_enum_drop_inplace_Option$$Weak$lNode$g"(ptr %0) {
entry:
  %enum_drop_tag_ptr = getelementptr inbounds nuw %"Option$$Weak$lNode$g", ptr %0, i32 0, i32 0
  %enum_drop_tag = load i8, ptr %enum_drop_tag_ptr, align 1
  switch i8 %enum_drop_tag, label %tag_oob_trap [
    i8 0, label %enum_drop_variant_0
    i8 1, label %enum_drop_variant_1
  ]

done:                                             ; preds = %enum_drop_variant_1, %enum_drop_variant_0
  ret void

tag_oob_trap:                                     ; preds = %entry
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

enum_drop_variant_0:                              ; preds = %entry
  %enum_drop_payload_0 = getelementptr inbounds nuw %"Option$$Weak$lNode$g", ptr %0, i32 0, i32 1
  %drop_rc_f0_ptr = getelementptr inbounds nuw { ptr }, ptr %enum_drop_payload_0, i32 0, i32 0
  %drop_rc_f0 = load ptr, ptr %drop_rc_f0_ptr, align 8
  call void @hew_weak_drop_rc(ptr %drop_rc_f0)
  store ptr null, ptr %drop_rc_f0_ptr, align 8
  br label %done

enum_drop_variant_1:                              ; preds = %entry
  %enum_drop_payload_1 = getelementptr inbounds nuw %"Option$$Weak$lNode$g", ptr %0, i32 0, i32 1
  br label %done
}

declare void @hew_weak_drop_rc(ptr)

declare void @hew_string_drop(ptr)

declare ptr @hew_rc_new(ptr, i64, i64, ptr)

declare i1 @hew_cont_crash_cleanup_deactivate(i64)

define internal void @__hew_frame_cleanup_193e762a1871d870(ptr %0) {
entry:
  %ref_drop_handle = load ptr, ptr %0, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle)
  store ptr null, ptr %0, align 8
  ret void
}

declare i64 @hew_cont_crash_cleanup_arm(i64, ptr, i64, i64, ptr, i32, i32)

declare ptr @hew_rc_downgrade(ptr)

declare ptr @hew_weak_clone_rc(ptr)

declare void @hew_rc_set(ptr, ptr)

declare i1 @hew_cont_crash_cleanup_retire(i64)

declare i32 @hew_runtime_exit_status()

define internal void @__hew_frame_cleanup_5357928e581d821b(ptr %0) {
entry:
  %ref_drop_handle = load ptr, ptr %0, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle)
  store ptr null, ptr %0, align 8
  ret void
}

declare ptr @hew_weak_upgrade_rc(ptr)

define internal void @__hew_frame_cleanup_efca22a2c2039eb3(ptr %0) {
entry:
  call void @"__hew_enum_drop_inplace_Option$$Rc$lNode$g"(ptr %0)
  ret void
}

declare i32 @hew_lambda_drain_all(i64)

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.smul.with.overflow.i64(i64, i64) #1

attributes #0 = { cold noreturn nounwind memory(inaccessiblemem: write) }
attributes #1 = { nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none) }
