; ModuleID = 'rc_weak_graph'
source_filename = "rc_weak_graph"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "aarch64-apple-macosx13.0"

%"Option$$Weak$lNode$g" = type { i8, [1 x i64] }
%Node = type { ptr, %"Option$$Weak$lNode$g" }
%"Option$$Rc$lNode$g" = type { i8, [1 x i64] }
%CrashInfo = type { i64, ptr }

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

declare void @hew_assert_eq_i64(i64, i64)

declare void @hew_assert_eq_u8(i8, i8)

declare void @hew_assert_eq_str(ptr, ptr)

declare void @hew_assert_eq_f64(double, double)

declare void @hew_assert_eq_bool(i8, i8)

declare void @hew_assert_ne_i64(i64, i64)

declare void @hew_assert_ne_u8(i8, i8)

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
  %local_12 = alloca i64, align 8
  %local_13 = alloca ptr, align 8
  %local_14 = alloca %Node, align 8
  %local_15 = alloca i8, align 1
  br label %bb0

bb0:                                              ; preds = %entry
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
  %move_load = load ptr, ptr %local_4, align 8
  store ptr %move_load, ptr %local_5, align 8
  store i64 0, ptr %local_6, align 8
  %rc_borrow_handle = load ptr, ptr %local_5, align 8
  %rc_handle_result = call ptr @hew_rc_downgrade(ptr %rc_borrow_handle)
  store ptr %rc_handle_result, ptr %local_7, align 8
  %move_load1 = load ptr, ptr %local_7, align 8
  store ptr %move_load1, ptr %local_8, align 8
  store i64 0, ptr %local_9, align 8
  store ptr @str_lit.1, ptr %local_10, align 8
  store i64 0, ptr %local_12, align 8
  %machine_tag_ptr2 = getelementptr inbounds nuw %"Option$$Weak$lNode$g", ptr %local_11, i32 0, i32 0
  %move_iN_load_wide3 = load i64, ptr %local_12, align 8
  %move_iN_trunc4 = trunc i64 %move_iN_load_wide3 to i8
  store i8 %move_iN_trunc4, ptr %machine_tag_ptr2, align 1
  %rc_borrow_handle5 = load ptr, ptr %local_8, align 8
  %rc_handle_result6 = call ptr @hew_weak_clone_rc(ptr %rc_borrow_handle5)
  store ptr %rc_handle_result6, ptr %local_13, align 8
  %machine_payload_ptr = getelementptr inbounds nuw %"Option$$Weak$lNode$g", ptr %local_11, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { ptr }, ptr %machine_payload_ptr, i32 0, i32 0
  %move_load7 = load ptr, ptr %local_13, align 8
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
  %move_load12 = load ptr, ptr %local_8, align 8
  store ptr %move_load12, ptr %return_slot, align 8
  %resource_drop_flag = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed = icmp eq i64 %resource_drop_flag, 0
  br i1 %resource_drop_not_consumed, label %resource_drop_live_only, label %resource_drop_merge

resource_drop_live_only:                          ; preds = %bb0
  %ref_drop_handle = load ptr, ptr %local_5, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle)
  store ptr null, ptr %local_5, align 8
  br label %resource_drop_merge

resource_drop_merge:                              ; preds = %resource_drop_live_only, %bb0
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val
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
  %local_10 = alloca i64, align 8
  %local_11 = alloca i64, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_result = call ptr @build()
  store ptr %call_result, ptr %local_0, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_0, align 8
  store ptr %move_load, ptr %local_1, align 8
  store i64 0, ptr %local_2, align 8
  %weak_upgrade_handle = load ptr, ptr %local_1, align 8
  %weak_upgrade = call ptr @hew_weak_upgrade_rc(ptr %weak_upgrade_handle)
  %machine_tag_ptr = getelementptr inbounds nuw %"Option$$Rc$lNode$g", ptr %local_4, i32 0, i32 0
  %machine_payload_ptr = getelementptr inbounds nuw %"Option$$Rc$lNode$g", ptr %local_4, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { ptr }, ptr %machine_payload_ptr, i32 0, i32 0
  %weak_upgrade_is_none = icmp eq ptr %weak_upgrade, null
  br i1 %weak_upgrade_is_none, label %weak_upgrade_none, label %weak_upgrade_some

bb2:                                              ; preds = %after_cooperate17, %after_cooperate7
  call void @"__hew_enum_drop_inplace_Option$$Rc$lNode$g"(ptr %local_4)
  store %"Option$$Rc$lNode$g" zeroinitializer, ptr %local_4, align 8
  %move_load2 = load i64, ptr %local_3, align 8
  store i64 %move_load2, ptr %return_slot, align 8
  call void @"__hew_enum_drop_inplace_Option$$Rc$lNode$g"(ptr %local_4)
  %resource_drop_flag = load i64, ptr %local_2, align 8
  %resource_drop_not_consumed = icmp eq i64 %resource_drop_flag, 0
  br i1 %resource_drop_not_consumed, label %resource_drop_live_only, label %resource_drop_merge

bb3:                                              ; preds = %weak_upgrade_cont
  store i64 9000, ptr %local_10, align 8
  %move_load3 = load i64, ptr %local_10, align 8
  store i64 %move_load3, ptr %local_3, align 8
  %hew_actor_cooperate4 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel5 = icmp eq i32 %hew_actor_cooperate4, 2
  br i1 %hew_cooperate_is_cancel5, label %cancel_exit6, label %after_cooperate7

bb4:                                              ; preds = %bb6
  store i64 1, ptr %local_11, align 8
  %move_load13 = load i64, ptr %local_11, align 8
  store i64 %move_load13, ptr %local_3, align 8
  %hew_actor_cooperate14 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel15 = icmp eq i32 %hew_actor_cooperate14, 2
  br i1 %hew_cooperate_is_cancel15, label %cancel_exit16, label %after_cooperate17

bb5:                                              ; preds = %bb6
  call void @"__hew_enum_drop_inplace_Option$$Rc$lNode$g"(ptr %local_4)
  %resource_drop_flag23 = load i64, ptr %local_2, align 8
  %resource_drop_not_consumed24 = icmp eq i64 %resource_drop_flag23, 0
  br i1 %resource_drop_not_consumed24, label %resource_drop_live_only25, label %resource_drop_merge26

bb6:                                              ; preds = %weak_upgrade_cont
  store i64 1, ptr %local_8, align 8
  %cmp_lhs28 = load i64, ptr %local_5, align 8
  %cmp_rhs29 = load i64, ptr %local_8, align 8
  %cmp_bit30 = icmp eq i64 %cmp_lhs28, %cmp_rhs29
  %cmp_zext31 = zext i1 %cmp_bit30 to i8
  store i8 %cmp_zext31, ptr %local_9, align 1
  %cond_load32 = load i8, ptr %local_9, align 1
  %cond_nz33 = icmp ne i8 %cond_load32, 0
  br i1 %cond_nz33, label %bb4, label %bb5

cancel_exit:                                      ; preds = %entry
  ret i64 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

weak_upgrade_some:                                ; preds = %bb1
  store i8 0, ptr %machine_tag_ptr, align 1
  store ptr %weak_upgrade, ptr %machine_variant_field_ptr, align 8
  br label %weak_upgrade_cont

weak_upgrade_none:                                ; preds = %bb1
  store i8 1, ptr %machine_tag_ptr, align 1
  store ptr null, ptr %machine_variant_field_ptr, align 8
  br label %weak_upgrade_cont

weak_upgrade_cont:                                ; preds = %weak_upgrade_none, %weak_upgrade_some
  %machine_tag_ptr1 = getelementptr inbounds nuw %"Option$$Rc$lNode$g", ptr %local_4, i32 0, i32 0
  %move_iN_load = load i8, ptr %machine_tag_ptr1, align 1
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

resource_drop_live_only:                          ; preds = %bb2
  %ref_drop_handle = load ptr, ptr %local_1, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle)
  store ptr null, ptr %local_1, align 8
  br label %resource_drop_merge

resource_drop_merge:                              ; preds = %resource_drop_live_only, %bb2
  %hew_lambda_drain_all_call = call i32 @hew_lambda_drain_all(i64 0)
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val

cancel_exit6:                                     ; preds = %bb3
  call void @"__hew_enum_drop_inplace_Option$$Rc$lNode$g"(ptr %local_4)
  %resource_drop_flag8 = load i64, ptr %local_2, align 8
  %resource_drop_not_consumed9 = icmp eq i64 %resource_drop_flag8, 0
  br i1 %resource_drop_not_consumed9, label %resource_drop_live_only10, label %resource_drop_merge11

after_cooperate7:                                 ; preds = %bb3
  br label %bb2

resource_drop_live_only10:                        ; preds = %cancel_exit6
  %ref_drop_handle12 = load ptr, ptr %local_1, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle12)
  store ptr null, ptr %local_1, align 8
  br label %resource_drop_merge11

resource_drop_merge11:                            ; preds = %resource_drop_live_only10, %cancel_exit6
  ret i64 0

cancel_exit16:                                    ; preds = %bb4
  call void @"__hew_enum_drop_inplace_Option$$Rc$lNode$g"(ptr %local_4)
  %resource_drop_flag18 = load i64, ptr %local_2, align 8
  %resource_drop_not_consumed19 = icmp eq i64 %resource_drop_flag18, 0
  br i1 %resource_drop_not_consumed19, label %resource_drop_live_only20, label %resource_drop_merge21

after_cooperate17:                                ; preds = %bb4
  br label %bb2

resource_drop_live_only20:                        ; preds = %cancel_exit16
  %ref_drop_handle22 = load ptr, ptr %local_1, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle22)
  store ptr null, ptr %local_1, align 8
  br label %resource_drop_merge21

resource_drop_merge21:                            ; preds = %resource_drop_live_only20, %cancel_exit16
  ret i64 0

resource_drop_live_only25:                        ; preds = %bb5
  %ref_drop_handle27 = load ptr, ptr %local_1, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle27)
  store ptr null, ptr %local_1, align 8
  br label %resource_drop_merge26

resource_drop_merge26:                            ; preds = %resource_drop_live_only25, %bb5
  call void @hew_trap_with_code(i32 208)
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
  %hew_duration_nanos = load i64, ptr %local_0, align 8
  %hew_duration_nanos_call = call i64 @hew_duration_nanos(i64 %hew_duration_nanos)
  store i64 %hew_duration_nanos_call, ptr %local_1, align 8
  %call_arg = load i64, ptr %local_1, align 8
  %call_result = call ptr @hew_i64_to_string(i64 %call_arg)
  store ptr %call_result, ptr %local_2, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
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

define internal i32 @__hew_record_clone_inplace_CrashInfo(ptr %0, ptr %1) {
entry:
  br label %step_0_clone

success:                                          ; preds = %step_0_store
  ret i32 0

fail:                                             ; preds = %rb_step_0
  ret i32 1

rb_step_0:                                        ; preds = %step_0_clone
  br label %fail

step_0_store:                                     ; preds = %step_0_clone
  %dst_f1_ptr = getelementptr inbounds nuw %CrashInfo, ptr %1, i32 0, i32 1
  store ptr %clone_helper_f1, ptr %dst_f1_ptr, align 8
  br label %success

step_0_clone:                                     ; preds = %entry
  %src_f1_ptr = getelementptr inbounds nuw %CrashInfo, ptr %0, i32 0, i32 1
  %src_f1 = load ptr, ptr %src_f1_ptr, align 8
  %clone_helper_f1 = call ptr @hew_string_clone(ptr %src_f1)
  %cloned_f1_int = ptrtoint ptr %clone_helper_f1 to i64
  %cloned_f1_null = icmp eq i64 %cloned_f1_int, 0
  br i1 %cloned_f1_null, label %rb_step_0, label %step_0_store
}

define internal void @__hew_record_drop_inplace_CrashInfo(ptr %0) {
entry:
  %rec_int = ptrtoint ptr %0 to i64
  %rec_is_null = icmp eq i64 %rec_int, 0
  br i1 %rec_is_null, label %done, label %do_drop

do_drop:                                          ; preds = %entry
  %drop_f1_ptr = getelementptr inbounds nuw %CrashInfo, ptr %0, i32 0, i32 1
  %drop_f1 = load ptr, ptr %drop_f1_ptr, align 8
  call void @hew_string_drop(ptr %drop_f1)
  br label %done

done:                                             ; preds = %do_drop, %entry
  ret void
}

declare void @hew_string_drop(ptr)

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

define internal void @__hew_record_overwrite_release_CrashInfo(ptr %0, ptr %1) {
entry:
  %ow_slot_0 = alloca ptr, align 8
  store ptr null, ptr %ow_slot_0, align 8
  %ow_new_d0_f1_ptr = getelementptr inbounds nuw %CrashInfo, ptr %1, i32 0, i32 1
  %ow_new_d0_f1_leaf = load ptr, ptr %ow_new_d0_f1_ptr, align 8
  store ptr %ow_new_d0_f1_leaf, ptr %ow_slot_0, align 8
  %ow_old_d0_f1_ptr = getelementptr inbounds nuw %CrashInfo, ptr %0, i32 0, i32 1
  %ow_old_d0_f1_val = load ptr, ptr %ow_old_d0_f1_ptr, align 8
  %ow_old_d0_f1_int = ptrtoint ptr %ow_old_d0_f1_val to i64
  %ow_old_d0_f1_cmp0_leaf = load ptr, ptr %ow_slot_0, align 8
  %ow_old_d0_f1_cmp0_int = ptrtoint ptr %ow_old_d0_f1_cmp0_leaf to i64
  %ow_old_d0_f1_cmp0_eq = icmp eq i64 %ow_old_d0_f1_int, %ow_old_d0_f1_cmp0_int
  %ow_old_d0_f1_matched0 = or i1 false, %ow_old_d0_f1_cmp0_eq
  %ow_old_d0_f1_neutralized = select i1 %ow_old_d0_f1_matched0, ptr null, ptr %ow_old_d0_f1_val
  store ptr %ow_old_d0_f1_neutralized, ptr %ow_old_d0_f1_ptr, align 8
  call void @__hew_record_drop_inplace_CrashInfo(ptr %0)
  ret void
}

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

declare ptr @hew_rc_new(ptr, i64, i64, ptr)

declare ptr @hew_rc_downgrade(ptr)

declare ptr @hew_weak_clone_rc(ptr)

declare void @hew_rc_set(ptr, ptr)

declare i32 @hew_actor_cooperate()

declare ptr @hew_weak_upgrade_rc(ptr)

declare i32 @hew_lambda_drain_all(i64)

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.smul.with.overflow.i64(i64, i64) #1

attributes #0 = { cold noreturn nounwind memory(inaccessiblemem: write) }
attributes #1 = { nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none) }
