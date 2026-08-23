; ModuleID = 'rc_weak_lifecycle'
source_filename = "rc_weak_lifecycle"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "aarch64-apple-macosx13.0"

%"Option$$Rc$li64$g" = type { i8, [1 x i64] }

@str_lit = private unnamed_addr constant [3 x i8] c"ns\00", align 1

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

define internal i64 @live() {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca i64, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca ptr, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca ptr, align 8
  %local_5 = alloca ptr, align 8
  %local_6 = alloca i64, align 8
  %local_7 = alloca ptr, align 8
  %local_8 = alloca ptr, align 8
  %local_9 = alloca i64, align 8
  %local_10 = alloca ptr, align 8
  %local_11 = alloca ptr, align 8
  %local_12 = alloca i64, align 8
  %local_13 = alloca i64, align 8
  %local_14 = alloca i64, align 8
  %local_15 = alloca i64, align 8
  %local_16 = alloca i8, align 1
  %local_17 = alloca i64, align 8
  %local_18 = alloca i64, align 8
  %local_19 = alloca i8, align 1
  %local_20 = alloca i64, align 8
  %local_21 = alloca i64, align 8
  %local_22 = alloca i8, align 1
  %local_23 = alloca i64, align 8
  %local_24 = alloca i64, align 8
  %local_25 = alloca i64, align 8
  %local_26 = alloca i64, align 8
  %local_27 = alloca i64, align 8
  %local_28 = alloca i64, align 8
  %local_29 = alloca %"Option$$Rc$li64$g", align 8
  %local_30 = alloca i64, align 8
  %local_31 = alloca i64, align 8
  %local_32 = alloca i8, align 1
  %local_33 = alloca i64, align 8
  %local_34 = alloca i8, align 1
  %local_35 = alloca ptr, align 8
  %local_36 = alloca i64, align 8
  %local_37 = alloca i8, align 1
  %local_38 = alloca i64, align 8
  %local_39 = alloca i64, align 8
  %local_40 = alloca i64, align 8
  %local_41 = alloca i8, align 1
  %local_42 = alloca i64, align 8
  %local_43 = alloca i8, align 1
  %local_44 = alloca i64, align 8
  %local_45 = alloca i64, align 8
  %local_46 = alloca i8, align 1
  %local_47 = alloca i64, align 8
  %local_48 = alloca i8, align 1
  %local_49 = alloca i64, align 8
  %local_50 = alloca i64, align 8
  %helper_crash_cleanup_token_2 = alloca i64, align 8
  store i64 0, ptr %helper_crash_cleanup_token_2, align 8
  %helper_crash_cleanup_active_2 = alloca i1, align 1
  store i1 false, ptr %helper_crash_cleanup_active_2, align 1
  %helper_crash_cleanup_token_5 = alloca i64, align 8
  store i64 0, ptr %helper_crash_cleanup_token_5, align 8
  %helper_crash_cleanup_active_5 = alloca i1, align 1
  store i1 false, ptr %helper_crash_cleanup_active_5, align 1
  %helper_crash_cleanup_token_8 = alloca i64, align 8
  store i64 0, ptr %helper_crash_cleanup_token_8, align 8
  %helper_crash_cleanup_active_8 = alloca i1, align 1
  store i1 false, ptr %helper_crash_cleanup_active_8, align 1
  %helper_crash_cleanup_token_11 = alloca i64, align 8
  store i64 0, ptr %helper_crash_cleanup_token_11, align 8
  %helper_crash_cleanup_active_11 = alloca i1, align 1
  store i1 false, ptr %helper_crash_cleanup_active_11, align 1
  %helper_crash_cleanup_token_29 = alloca i64, align 8
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_active_29 = alloca i1, align 1
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  store i64 -1, ptr %local_3, align 8
  store i64 -1, ptr %local_6, align 8
  store i64 -1, ptr %local_9, align 8
  store i64 -1, ptr %local_12, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  store i64 7, ptr %local_0, align 8
  %rc_new = call ptr @hew_rc_new(ptr %local_0, i64 8, i64 8, ptr null)
  store ptr %rc_new, ptr %local_1, align 8
  %helper_crash_cleanup_was_active = load i1, ptr %helper_crash_cleanup_active_2, align 1
  br i1 %helper_crash_cleanup_was_active, label %helper_crash_cleanup_deactivate, label %helper_crash_cleanup_deactivate_merge

bb1:                                              ; preds = %helper_crash_cleanup_guard_merge50
  %resource_drop_flag = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed = icmp eq i64 %resource_drop_flag, 0
  br i1 %resource_drop_not_consumed, label %resource_drop_live_only, label %resource_drop_merge

bb2:                                              ; preds = %helper_crash_cleanup_guard_merge50
  %rc_count_handle92 = load ptr, ptr %local_2, align 8
  %rc_count93 = call i64 @hew_rc_weak_count(ptr %rc_count_handle92)
  store i64 %rc_count93, ptr %local_17, align 8
  %checked_lhs94 = load i64, ptr %local_15, align 8
  %checked_rhs95 = load i64, ptr %local_17, align 8
  %with_overflow96 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs94, i64 %checked_rhs95)
  %checked_result97 = extractvalue { i64, i1 } %with_overflow96, 0
  %checked_overflow98 = extractvalue { i64, i1 } %with_overflow96, 1
  %checked_overflow_widen99 = zext i1 %checked_overflow98 to i8
  store i64 %checked_result97, ptr %local_18, align 8
  store i8 %checked_overflow_widen99, ptr %local_19, align 1
  %cond_load100 = load i8, ptr %local_19, align 1
  %cond_nz101 = icmp ne i8 %cond_load100, 0
  br i1 %cond_nz101, label %bb3, label %bb4

bb3:                                              ; preds = %bb2
  %resource_drop_flag102 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed103 = icmp eq i64 %resource_drop_flag102, 0
  br i1 %resource_drop_not_consumed103, label %resource_drop_live_only104, label %resource_drop_merge105

bb4:                                              ; preds = %bb2
  %move_load150 = load i64, ptr %local_18, align 8
  store i64 %move_load150, ptr %local_20, align 8
  %rc_is_unique_handle = load ptr, ptr %local_2, align 8
  %rc_is_unique = call i32 @hew_rc_is_unique(ptr %rc_is_unique_handle)
  %rc_unique_bit = icmp ne i32 %rc_is_unique, 0
  %rc_unique_bool = zext i1 %rc_unique_bit to i8
  store i8 %rc_unique_bool, ptr %local_22, align 1
  %cond_load151 = load i8, ptr %local_22, align 1
  %cond_nz152 = icmp ne i8 %cond_load151, 0
  br i1 %cond_nz152, label %bb5, label %bb6

bb5:                                              ; preds = %bb4
  store i64 90, ptr %local_23, align 8
  %move_load153 = load i64, ptr %local_23, align 8
  store i64 %move_load153, ptr %local_24, align 8
  %move_load154 = load i64, ptr %local_24, align 8
  store i64 %move_load154, ptr %local_21, align 8
  br label %bb7

bb6:                                              ; preds = %bb4
  store i64 1, ptr %local_25, align 8
  %move_load155 = load i64, ptr %local_25, align 8
  store i64 %move_load155, ptr %local_26, align 8
  %move_load156 = load i64, ptr %local_26, align 8
  store i64 %move_load156, ptr %local_21, align 8
  br label %bb7

bb7:                                              ; preds = %bb6, %bb5
  %move_load157 = load i64, ptr %local_21, align 8
  store i64 %move_load157, ptr %local_27, align 8
  %helper_crash_cleanup_was_active158 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_was_active158, label %helper_crash_cleanup_deactivate159, label %helper_crash_cleanup_deactivate_merge160

bb8:                                              ; preds = %after_cooperate619, %after_cooperate253
  %helper_crash_cleanup_drop_active173 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_drop_active173, label %helper_crash_cleanup_retire174, label %helper_crash_cleanup_retire_merge175

bb9:                                              ; preds = %frame_cleanup_registered168
  %machine_payload_ptr236 = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %local_29, i32 0, i32 1
  %machine_variant_field_ptr237 = getelementptr inbounds nuw { ptr }, ptr %machine_payload_ptr236, i32 0, i32 0
  %move_load238 = load ptr, ptr %machine_variant_field_ptr237, align 8
  store ptr %move_load238, ptr %local_35, align 8
  store i64 9, ptr %local_36, align 8
  %rc_set_handle = load ptr, ptr %local_2, align 8
  call void @hew_rc_set(ptr %rc_set_handle, ptr %local_36)
  store i8 0, ptr %local_37, align 1
  %rc_count_handle239 = load ptr, ptr %local_35, align 8
  %rc_count240 = call i64 @hew_rc_strong_count(ptr %rc_count_handle239)
  store i64 %rc_count240, ptr %local_38, align 8
  store i64 10, ptr %local_39, align 8
  %checked_lhs241 = load i64, ptr %local_38, align 8
  %checked_rhs242 = load i64, ptr %local_39, align 8
  %with_overflow243 = call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %checked_lhs241, i64 %checked_rhs242)
  %checked_result244 = extractvalue { i64, i1 } %with_overflow243, 0
  %checked_overflow245 = extractvalue { i64, i1 } %with_overflow243, 1
  %checked_overflow_widen246 = zext i1 %checked_overflow245 to i8
  store i64 %checked_result244, ptr %local_40, align 8
  store i8 %checked_overflow_widen246, ptr %local_41, align 1
  %cond_load247 = load i8, ptr %local_41, align 1
  %cond_nz248 = icmp ne i8 %cond_load247, 0
  br i1 %cond_nz248, label %bb13, label %bb14

bb10:                                             ; preds = %bb12
  store i64 9000, ptr %local_50, align 8
  %move_load249 = load i64, ptr %local_50, align 8
  store i64 %move_load249, ptr %local_28, align 8
  %hew_actor_cooperate250 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel251 = icmp eq i32 %hew_actor_cooperate250, 2
  br i1 %hew_cooperate_is_cancel251, label %cancel_exit252, label %after_cooperate253

bb11:                                             ; preds = %bb12
  %helper_crash_cleanup_drop_active309 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_drop_active309, label %helper_crash_cleanup_retire310, label %helper_crash_cleanup_retire_merge311

bb12:                                             ; preds = %frame_cleanup_registered168
  store i64 1, ptr %local_33, align 8
  %cmp_lhs364 = load i64, ptr %local_30, align 8
  %cmp_rhs365 = load i64, ptr %local_33, align 8
  %cmp_bit366 = icmp eq i64 %cmp_lhs364, %cmp_rhs365
  %cmp_zext367 = zext i1 %cmp_bit366 to i8
  store i8 %cmp_zext367, ptr %local_34, align 1
  %cond_load368 = load i8, ptr %local_34, align 1
  %cond_nz369 = icmp ne i8 %cond_load368, 0
  br i1 %cond_nz369, label %bb10, label %bb11

bb13:                                             ; preds = %bb9
  %helper_crash_cleanup_drop_active370 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_drop_active370, label %helper_crash_cleanup_retire371, label %helper_crash_cleanup_retire_merge372

bb14:                                             ; preds = %bb9
  %checked_lhs425 = load i64, ptr %local_20, align 8
  %checked_rhs426 = load i64, ptr %local_40, align 8
  %with_overflow427 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs425, i64 %checked_rhs426)
  %checked_result428 = extractvalue { i64, i1 } %with_overflow427, 0
  %checked_overflow429 = extractvalue { i64, i1 } %with_overflow427, 1
  %checked_overflow_widen430 = zext i1 %checked_overflow429 to i8
  store i64 %checked_result428, ptr %local_42, align 8
  store i8 %checked_overflow_widen430, ptr %local_43, align 1
  %cond_load431 = load i8, ptr %local_43, align 1
  %cond_nz432 = icmp ne i8 %cond_load431, 0
  br i1 %cond_nz432, label %bb15, label %bb16

bb15:                                             ; preds = %bb14
  %helper_crash_cleanup_drop_active433 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_drop_active433, label %helper_crash_cleanup_retire434, label %helper_crash_cleanup_retire_merge435

bb16:                                             ; preds = %bb14
  %rc_get_handle = load ptr, ptr %local_5, align 8
  %rc_get_data = call ptr @hew_rc_get(ptr %rc_get_handle)
  %rc_get_value = load i64, ptr %rc_get_data, align 8
  store i64 %rc_get_value, ptr %local_44, align 8
  %checked_lhs488 = load i64, ptr %local_42, align 8
  %checked_rhs489 = load i64, ptr %local_44, align 8
  %with_overflow490 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs488, i64 %checked_rhs489)
  %checked_result491 = extractvalue { i64, i1 } %with_overflow490, 0
  %checked_overflow492 = extractvalue { i64, i1 } %with_overflow490, 1
  %checked_overflow_widen493 = zext i1 %checked_overflow492 to i8
  store i64 %checked_result491, ptr %local_45, align 8
  store i8 %checked_overflow_widen493, ptr %local_46, align 1
  %cond_load494 = load i8, ptr %local_46, align 1
  %cond_nz495 = icmp ne i8 %cond_load494, 0
  br i1 %cond_nz495, label %bb17, label %bb18

bb17:                                             ; preds = %bb16
  %helper_crash_cleanup_drop_active496 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_drop_active496, label %helper_crash_cleanup_retire497, label %helper_crash_cleanup_retire_merge498

bb18:                                             ; preds = %bb16
  %checked_lhs551 = load i64, ptr %local_45, align 8
  %checked_rhs552 = load i64, ptr %local_27, align 8
  %with_overflow553 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs551, i64 %checked_rhs552)
  %checked_result554 = extractvalue { i64, i1 } %with_overflow553, 0
  %checked_overflow555 = extractvalue { i64, i1 } %with_overflow553, 1
  %checked_overflow_widen556 = zext i1 %checked_overflow555 to i8
  store i64 %checked_result554, ptr %local_47, align 8
  store i8 %checked_overflow_widen556, ptr %local_48, align 1
  %cond_load557 = load i8, ptr %local_48, align 1
  %cond_nz558 = icmp ne i8 %cond_load557, 0
  br i1 %cond_nz558, label %bb19, label %bb20

bb19:                                             ; preds = %bb18
  %helper_crash_cleanup_drop_active559 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_drop_active559, label %helper_crash_cleanup_retire560, label %helper_crash_cleanup_retire_merge561

bb20:                                             ; preds = %bb18
  %move_load614 = load i64, ptr %local_47, align 8
  store i64 %move_load614, ptr %local_49, align 8
  %move_load615 = load i64, ptr %local_49, align 8
  store i64 %move_load615, ptr %local_28, align 8
  %hew_actor_cooperate616 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel617 = icmp eq i32 %hew_actor_cooperate616, 2
  br i1 %hew_cooperate_is_cancel617, label %cancel_exit618, label %after_cooperate619

cancel_exit:                                      ; preds = %entry
  ret i64 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

helper_crash_cleanup_deactivate:                  ; preds = %bb0
  %helper_crash_cleanup_token = load i64, ptr %helper_crash_cleanup_token_2, align 8
  %helper_crash_cleanup_deactivate_call = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token)
  br i1 %helper_crash_cleanup_deactivate_call, label %helper_crash_cleanup_deactivate_accepted, label %helper_crash_cleanup_deactivate_rejected

helper_crash_cleanup_deactivate_merge:            ; preds = %helper_crash_cleanup_deactivate_accepted, %bb0
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %local_2, align 8
  store i64 0, ptr %local_3, align 8
  %helper_crash_cleanup_arm_guard = load i64, ptr %local_3, align 8
  %helper_crash_cleanup_arm_guard_live = icmp eq i64 %helper_crash_cleanup_arm_guard, 0
  br i1 %helper_crash_cleanup_arm_guard_live, label %helper_crash_cleanup_guard_live, label %helper_crash_cleanup_guard_merge

helper_crash_cleanup_deactivate_accepted:         ; preds = %helper_crash_cleanup_deactivate
  store i1 false, ptr %helper_crash_cleanup_active_2, align 1
  br label %helper_crash_cleanup_deactivate_merge

helper_crash_cleanup_deactivate_rejected:         ; preds = %helper_crash_cleanup_deactivate
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_guard_live:                  ; preds = %helper_crash_cleanup_deactivate_merge
  %helper_crash_cleanup_prior_token = load i64, ptr %helper_crash_cleanup_token_2, align 8
  %arm_typed_crash_cleanup = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token, ptr %local_2, i64 8, i64 8, ptr @__hew_frame_cleanup_445350923e96a538, i32 1, i32 0)
  %frame_cleanup_arm_failed = icmp eq i64 %arm_typed_crash_cleanup, -1
  br i1 %frame_cleanup_arm_failed, label %frame_cleanup_rejected, label %frame_cleanup_registered

helper_crash_cleanup_guard_merge:                 ; preds = %frame_cleanup_registered, %helper_crash_cleanup_deactivate_merge
  %rc_borrow_handle = load ptr, ptr %local_2, align 8
  %rc_handle_result = call ptr @hew_rc_clone(ptr %rc_borrow_handle)
  store ptr %rc_handle_result, ptr %local_4, align 8
  %helper_crash_cleanup_was_active1 = load i1, ptr %helper_crash_cleanup_active_5, align 1
  br i1 %helper_crash_cleanup_was_active1, label %helper_crash_cleanup_deactivate2, label %helper_crash_cleanup_deactivate_merge3

frame_cleanup_registered:                         ; preds = %helper_crash_cleanup_guard_live
  store i64 %arm_typed_crash_cleanup, ptr %helper_crash_cleanup_token_2, align 8
  store i1 true, ptr %helper_crash_cleanup_active_2, align 1
  br label %helper_crash_cleanup_guard_merge

frame_cleanup_rejected:                           ; preds = %helper_crash_cleanup_guard_live
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_deactivate2:                 ; preds = %helper_crash_cleanup_guard_merge
  %helper_crash_cleanup_token4 = load i64, ptr %helper_crash_cleanup_token_5, align 8
  %helper_crash_cleanup_deactivate_call5 = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token4)
  br i1 %helper_crash_cleanup_deactivate_call5, label %helper_crash_cleanup_deactivate_accepted6, label %helper_crash_cleanup_deactivate_rejected7

helper_crash_cleanup_deactivate_merge3:           ; preds = %helper_crash_cleanup_deactivate_accepted6, %helper_crash_cleanup_guard_merge
  %move_load8 = load ptr, ptr %local_4, align 8
  store ptr %move_load8, ptr %local_5, align 8
  store i64 0, ptr %local_6, align 8
  %helper_crash_cleanup_arm_guard9 = load i64, ptr %local_6, align 8
  %helper_crash_cleanup_arm_guard_live10 = icmp eq i64 %helper_crash_cleanup_arm_guard9, 0
  br i1 %helper_crash_cleanup_arm_guard_live10, label %helper_crash_cleanup_guard_live11, label %helper_crash_cleanup_guard_merge12

helper_crash_cleanup_deactivate_accepted6:        ; preds = %helper_crash_cleanup_deactivate2
  store i1 false, ptr %helper_crash_cleanup_active_5, align 1
  br label %helper_crash_cleanup_deactivate_merge3

helper_crash_cleanup_deactivate_rejected7:        ; preds = %helper_crash_cleanup_deactivate2
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_guard_live11:                ; preds = %helper_crash_cleanup_deactivate_merge3
  %helper_crash_cleanup_prior_token13 = load i64, ptr %helper_crash_cleanup_token_5, align 8
  %arm_typed_crash_cleanup14 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token13, ptr %local_5, i64 8, i64 8, ptr @__hew_frame_cleanup_445350923e96a538, i32 1, i32 0)
  %frame_cleanup_arm_failed15 = icmp eq i64 %arm_typed_crash_cleanup14, -1
  br i1 %frame_cleanup_arm_failed15, label %frame_cleanup_rejected17, label %frame_cleanup_registered16

helper_crash_cleanup_guard_merge12:               ; preds = %frame_cleanup_registered16, %helper_crash_cleanup_deactivate_merge3
  %rc_borrow_handle18 = load ptr, ptr %local_2, align 8
  %rc_handle_result19 = call ptr @hew_rc_downgrade(ptr %rc_borrow_handle18)
  store ptr %rc_handle_result19, ptr %local_7, align 8
  %helper_crash_cleanup_was_active20 = load i1, ptr %helper_crash_cleanup_active_8, align 1
  br i1 %helper_crash_cleanup_was_active20, label %helper_crash_cleanup_deactivate21, label %helper_crash_cleanup_deactivate_merge22

frame_cleanup_registered16:                       ; preds = %helper_crash_cleanup_guard_live11
  store i64 %arm_typed_crash_cleanup14, ptr %helper_crash_cleanup_token_5, align 8
  store i1 true, ptr %helper_crash_cleanup_active_5, align 1
  br label %helper_crash_cleanup_guard_merge12

frame_cleanup_rejected17:                         ; preds = %helper_crash_cleanup_guard_live11
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_deactivate21:                ; preds = %helper_crash_cleanup_guard_merge12
  %helper_crash_cleanup_token23 = load i64, ptr %helper_crash_cleanup_token_8, align 8
  %helper_crash_cleanup_deactivate_call24 = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token23)
  br i1 %helper_crash_cleanup_deactivate_call24, label %helper_crash_cleanup_deactivate_accepted25, label %helper_crash_cleanup_deactivate_rejected26

helper_crash_cleanup_deactivate_merge22:          ; preds = %helper_crash_cleanup_deactivate_accepted25, %helper_crash_cleanup_guard_merge12
  %move_load27 = load ptr, ptr %local_7, align 8
  store ptr %move_load27, ptr %local_8, align 8
  store i64 0, ptr %local_9, align 8
  %helper_crash_cleanup_arm_guard28 = load i64, ptr %local_9, align 8
  %helper_crash_cleanup_arm_guard_live29 = icmp eq i64 %helper_crash_cleanup_arm_guard28, 0
  br i1 %helper_crash_cleanup_arm_guard_live29, label %helper_crash_cleanup_guard_live30, label %helper_crash_cleanup_guard_merge31

helper_crash_cleanup_deactivate_accepted25:       ; preds = %helper_crash_cleanup_deactivate21
  store i1 false, ptr %helper_crash_cleanup_active_8, align 1
  br label %helper_crash_cleanup_deactivate_merge22

helper_crash_cleanup_deactivate_rejected26:       ; preds = %helper_crash_cleanup_deactivate21
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_guard_live30:                ; preds = %helper_crash_cleanup_deactivate_merge22
  %helper_crash_cleanup_prior_token32 = load i64, ptr %helper_crash_cleanup_token_8, align 8
  %arm_typed_crash_cleanup33 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token32, ptr %local_8, i64 8, i64 8, ptr @__hew_frame_cleanup_7b793acd5045b9cb, i32 1, i32 0)
  %frame_cleanup_arm_failed34 = icmp eq i64 %arm_typed_crash_cleanup33, -1
  br i1 %frame_cleanup_arm_failed34, label %frame_cleanup_rejected36, label %frame_cleanup_registered35

helper_crash_cleanup_guard_merge31:               ; preds = %frame_cleanup_registered35, %helper_crash_cleanup_deactivate_merge22
  %rc_borrow_handle37 = load ptr, ptr %local_8, align 8
  %rc_handle_result38 = call ptr @hew_weak_clone_rc(ptr %rc_borrow_handle37)
  store ptr %rc_handle_result38, ptr %local_10, align 8
  %helper_crash_cleanup_was_active39 = load i1, ptr %helper_crash_cleanup_active_11, align 1
  br i1 %helper_crash_cleanup_was_active39, label %helper_crash_cleanup_deactivate40, label %helper_crash_cleanup_deactivate_merge41

frame_cleanup_registered35:                       ; preds = %helper_crash_cleanup_guard_live30
  store i64 %arm_typed_crash_cleanup33, ptr %helper_crash_cleanup_token_8, align 8
  store i1 true, ptr %helper_crash_cleanup_active_8, align 1
  br label %helper_crash_cleanup_guard_merge31

frame_cleanup_rejected36:                         ; preds = %helper_crash_cleanup_guard_live30
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_deactivate40:                ; preds = %helper_crash_cleanup_guard_merge31
  %helper_crash_cleanup_token42 = load i64, ptr %helper_crash_cleanup_token_11, align 8
  %helper_crash_cleanup_deactivate_call43 = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token42)
  br i1 %helper_crash_cleanup_deactivate_call43, label %helper_crash_cleanup_deactivate_accepted44, label %helper_crash_cleanup_deactivate_rejected45

helper_crash_cleanup_deactivate_merge41:          ; preds = %helper_crash_cleanup_deactivate_accepted44, %helper_crash_cleanup_guard_merge31
  %move_load46 = load ptr, ptr %local_10, align 8
  store ptr %move_load46, ptr %local_11, align 8
  store i64 0, ptr %local_12, align 8
  %helper_crash_cleanup_arm_guard47 = load i64, ptr %local_12, align 8
  %helper_crash_cleanup_arm_guard_live48 = icmp eq i64 %helper_crash_cleanup_arm_guard47, 0
  br i1 %helper_crash_cleanup_arm_guard_live48, label %helper_crash_cleanup_guard_live49, label %helper_crash_cleanup_guard_merge50

helper_crash_cleanup_deactivate_accepted44:       ; preds = %helper_crash_cleanup_deactivate40
  store i1 false, ptr %helper_crash_cleanup_active_11, align 1
  br label %helper_crash_cleanup_deactivate_merge41

helper_crash_cleanup_deactivate_rejected45:       ; preds = %helper_crash_cleanup_deactivate40
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_guard_live49:                ; preds = %helper_crash_cleanup_deactivate_merge41
  %helper_crash_cleanup_prior_token51 = load i64, ptr %helper_crash_cleanup_token_11, align 8
  %arm_typed_crash_cleanup52 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token51, ptr %local_11, i64 8, i64 8, ptr @__hew_frame_cleanup_7b793acd5045b9cb, i32 1, i32 0)
  %frame_cleanup_arm_failed53 = icmp eq i64 %arm_typed_crash_cleanup52, -1
  br i1 %frame_cleanup_arm_failed53, label %frame_cleanup_rejected55, label %frame_cleanup_registered54

helper_crash_cleanup_guard_merge50:               ; preds = %frame_cleanup_registered54, %helper_crash_cleanup_deactivate_merge41
  %rc_count_handle = load ptr, ptr %local_2, align 8
  %rc_count = call i64 @hew_rc_strong_count(ptr %rc_count_handle)
  store i64 %rc_count, ptr %local_13, align 8
  store i64 100, ptr %local_14, align 8
  %checked_lhs = load i64, ptr %local_13, align 8
  %checked_rhs = load i64, ptr %local_14, align 8
  %with_overflow = call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_15, align 8
  store i8 %checked_overflow_widen, ptr %local_16, align 1
  %cond_load = load i8, ptr %local_16, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb1, label %bb2

frame_cleanup_registered54:                       ; preds = %helper_crash_cleanup_guard_live49
  store i64 %arm_typed_crash_cleanup52, ptr %helper_crash_cleanup_token_11, align 8
  store i1 true, ptr %helper_crash_cleanup_active_11, align 1
  br label %helper_crash_cleanup_guard_merge50

frame_cleanup_rejected55:                         ; preds = %helper_crash_cleanup_guard_live49
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only:                          ; preds = %bb1
  %helper_crash_cleanup_drop_active = load i1, ptr %helper_crash_cleanup_active_11, align 1
  br i1 %helper_crash_cleanup_drop_active, label %helper_crash_cleanup_retire, label %helper_crash_cleanup_retire_merge

resource_drop_merge:                              ; preds = %helper_crash_cleanup_retire_merge, %bb1
  %resource_drop_flag56 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed57 = icmp eq i64 %resource_drop_flag56, 0
  br i1 %resource_drop_not_consumed57, label %resource_drop_live_only58, label %resource_drop_merge59

helper_crash_cleanup_retire:                      ; preds = %resource_drop_live_only
  %helper_crash_cleanup_retire_token = load i64, ptr %helper_crash_cleanup_token_11, align 8
  %helper_crash_cleanup_retire_call = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token)
  br i1 %helper_crash_cleanup_retire_call, label %helper_crash_cleanup_retire_accepted, label %helper_crash_cleanup_retire_rejected

helper_crash_cleanup_retire_merge:                ; preds = %helper_crash_cleanup_retire_accepted, %resource_drop_live_only
  %ref_drop_handle = load ptr, ptr %local_11, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle)
  store ptr null, ptr %local_11, align 8
  br label %resource_drop_merge

helper_crash_cleanup_retire_accepted:             ; preds = %helper_crash_cleanup_retire
  store i64 0, ptr %helper_crash_cleanup_token_11, align 8
  store i1 false, ptr %helper_crash_cleanup_active_11, align 1
  br label %helper_crash_cleanup_retire_merge

helper_crash_cleanup_retire_rejected:             ; preds = %helper_crash_cleanup_retire
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only58:                        ; preds = %resource_drop_merge
  %helper_crash_cleanup_drop_active60 = load i1, ptr %helper_crash_cleanup_active_8, align 1
  br i1 %helper_crash_cleanup_drop_active60, label %helper_crash_cleanup_retire61, label %helper_crash_cleanup_retire_merge62

resource_drop_merge59:                            ; preds = %helper_crash_cleanup_retire_merge62, %resource_drop_merge
  %resource_drop_flag68 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed69 = icmp eq i64 %resource_drop_flag68, 0
  br i1 %resource_drop_not_consumed69, label %resource_drop_live_only70, label %resource_drop_merge71

helper_crash_cleanup_retire61:                    ; preds = %resource_drop_live_only58
  %helper_crash_cleanup_retire_token63 = load i64, ptr %helper_crash_cleanup_token_8, align 8
  %helper_crash_cleanup_retire_call64 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token63)
  br i1 %helper_crash_cleanup_retire_call64, label %helper_crash_cleanup_retire_accepted65, label %helper_crash_cleanup_retire_rejected66

helper_crash_cleanup_retire_merge62:              ; preds = %helper_crash_cleanup_retire_accepted65, %resource_drop_live_only58
  %ref_drop_handle67 = load ptr, ptr %local_8, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle67)
  store ptr null, ptr %local_8, align 8
  br label %resource_drop_merge59

helper_crash_cleanup_retire_accepted65:           ; preds = %helper_crash_cleanup_retire61
  store i64 0, ptr %helper_crash_cleanup_token_8, align 8
  store i1 false, ptr %helper_crash_cleanup_active_8, align 1
  br label %helper_crash_cleanup_retire_merge62

helper_crash_cleanup_retire_rejected66:           ; preds = %helper_crash_cleanup_retire61
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only70:                        ; preds = %resource_drop_merge59
  %helper_crash_cleanup_drop_active72 = load i1, ptr %helper_crash_cleanup_active_5, align 1
  br i1 %helper_crash_cleanup_drop_active72, label %helper_crash_cleanup_retire73, label %helper_crash_cleanup_retire_merge74

resource_drop_merge71:                            ; preds = %helper_crash_cleanup_retire_merge74, %resource_drop_merge59
  %resource_drop_flag80 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed81 = icmp eq i64 %resource_drop_flag80, 0
  br i1 %resource_drop_not_consumed81, label %resource_drop_live_only82, label %resource_drop_merge83

helper_crash_cleanup_retire73:                    ; preds = %resource_drop_live_only70
  %helper_crash_cleanup_retire_token75 = load i64, ptr %helper_crash_cleanup_token_5, align 8
  %helper_crash_cleanup_retire_call76 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token75)
  br i1 %helper_crash_cleanup_retire_call76, label %helper_crash_cleanup_retire_accepted77, label %helper_crash_cleanup_retire_rejected78

helper_crash_cleanup_retire_merge74:              ; preds = %helper_crash_cleanup_retire_accepted77, %resource_drop_live_only70
  %ref_drop_handle79 = load ptr, ptr %local_5, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle79)
  store ptr null, ptr %local_5, align 8
  br label %resource_drop_merge71

helper_crash_cleanup_retire_accepted77:           ; preds = %helper_crash_cleanup_retire73
  store i64 0, ptr %helper_crash_cleanup_token_5, align 8
  store i1 false, ptr %helper_crash_cleanup_active_5, align 1
  br label %helper_crash_cleanup_retire_merge74

helper_crash_cleanup_retire_rejected78:           ; preds = %helper_crash_cleanup_retire73
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only82:                        ; preds = %resource_drop_merge71
  %helper_crash_cleanup_drop_active84 = load i1, ptr %helper_crash_cleanup_active_2, align 1
  br i1 %helper_crash_cleanup_drop_active84, label %helper_crash_cleanup_retire85, label %helper_crash_cleanup_retire_merge86

resource_drop_merge83:                            ; preds = %helper_crash_cleanup_retire_merge86, %resource_drop_merge71
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire85:                    ; preds = %resource_drop_live_only82
  %helper_crash_cleanup_retire_token87 = load i64, ptr %helper_crash_cleanup_token_2, align 8
  %helper_crash_cleanup_retire_call88 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token87)
  br i1 %helper_crash_cleanup_retire_call88, label %helper_crash_cleanup_retire_accepted89, label %helper_crash_cleanup_retire_rejected90

helper_crash_cleanup_retire_merge86:              ; preds = %helper_crash_cleanup_retire_accepted89, %resource_drop_live_only82
  %ref_drop_handle91 = load ptr, ptr %local_2, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle91)
  store ptr null, ptr %local_2, align 8
  br label %resource_drop_merge83

helper_crash_cleanup_retire_accepted89:           ; preds = %helper_crash_cleanup_retire85
  store i64 0, ptr %helper_crash_cleanup_token_2, align 8
  store i1 false, ptr %helper_crash_cleanup_active_2, align 1
  br label %helper_crash_cleanup_retire_merge86

helper_crash_cleanup_retire_rejected90:           ; preds = %helper_crash_cleanup_retire85
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only104:                       ; preds = %bb3
  %helper_crash_cleanup_drop_active106 = load i1, ptr %helper_crash_cleanup_active_11, align 1
  br i1 %helper_crash_cleanup_drop_active106, label %helper_crash_cleanup_retire107, label %helper_crash_cleanup_retire_merge108

resource_drop_merge105:                           ; preds = %helper_crash_cleanup_retire_merge108, %bb3
  %resource_drop_flag114 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed115 = icmp eq i64 %resource_drop_flag114, 0
  br i1 %resource_drop_not_consumed115, label %resource_drop_live_only116, label %resource_drop_merge117

helper_crash_cleanup_retire107:                   ; preds = %resource_drop_live_only104
  %helper_crash_cleanup_retire_token109 = load i64, ptr %helper_crash_cleanup_token_11, align 8
  %helper_crash_cleanup_retire_call110 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token109)
  br i1 %helper_crash_cleanup_retire_call110, label %helper_crash_cleanup_retire_accepted111, label %helper_crash_cleanup_retire_rejected112

helper_crash_cleanup_retire_merge108:             ; preds = %helper_crash_cleanup_retire_accepted111, %resource_drop_live_only104
  %ref_drop_handle113 = load ptr, ptr %local_11, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle113)
  store ptr null, ptr %local_11, align 8
  br label %resource_drop_merge105

helper_crash_cleanup_retire_accepted111:          ; preds = %helper_crash_cleanup_retire107
  store i64 0, ptr %helper_crash_cleanup_token_11, align 8
  store i1 false, ptr %helper_crash_cleanup_active_11, align 1
  br label %helper_crash_cleanup_retire_merge108

helper_crash_cleanup_retire_rejected112:          ; preds = %helper_crash_cleanup_retire107
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only116:                       ; preds = %resource_drop_merge105
  %helper_crash_cleanup_drop_active118 = load i1, ptr %helper_crash_cleanup_active_8, align 1
  br i1 %helper_crash_cleanup_drop_active118, label %helper_crash_cleanup_retire119, label %helper_crash_cleanup_retire_merge120

resource_drop_merge117:                           ; preds = %helper_crash_cleanup_retire_merge120, %resource_drop_merge105
  %resource_drop_flag126 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed127 = icmp eq i64 %resource_drop_flag126, 0
  br i1 %resource_drop_not_consumed127, label %resource_drop_live_only128, label %resource_drop_merge129

helper_crash_cleanup_retire119:                   ; preds = %resource_drop_live_only116
  %helper_crash_cleanup_retire_token121 = load i64, ptr %helper_crash_cleanup_token_8, align 8
  %helper_crash_cleanup_retire_call122 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token121)
  br i1 %helper_crash_cleanup_retire_call122, label %helper_crash_cleanup_retire_accepted123, label %helper_crash_cleanup_retire_rejected124

helper_crash_cleanup_retire_merge120:             ; preds = %helper_crash_cleanup_retire_accepted123, %resource_drop_live_only116
  %ref_drop_handle125 = load ptr, ptr %local_8, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle125)
  store ptr null, ptr %local_8, align 8
  br label %resource_drop_merge117

helper_crash_cleanup_retire_accepted123:          ; preds = %helper_crash_cleanup_retire119
  store i64 0, ptr %helper_crash_cleanup_token_8, align 8
  store i1 false, ptr %helper_crash_cleanup_active_8, align 1
  br label %helper_crash_cleanup_retire_merge120

helper_crash_cleanup_retire_rejected124:          ; preds = %helper_crash_cleanup_retire119
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only128:                       ; preds = %resource_drop_merge117
  %helper_crash_cleanup_drop_active130 = load i1, ptr %helper_crash_cleanup_active_5, align 1
  br i1 %helper_crash_cleanup_drop_active130, label %helper_crash_cleanup_retire131, label %helper_crash_cleanup_retire_merge132

resource_drop_merge129:                           ; preds = %helper_crash_cleanup_retire_merge132, %resource_drop_merge117
  %resource_drop_flag138 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed139 = icmp eq i64 %resource_drop_flag138, 0
  br i1 %resource_drop_not_consumed139, label %resource_drop_live_only140, label %resource_drop_merge141

helper_crash_cleanup_retire131:                   ; preds = %resource_drop_live_only128
  %helper_crash_cleanup_retire_token133 = load i64, ptr %helper_crash_cleanup_token_5, align 8
  %helper_crash_cleanup_retire_call134 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token133)
  br i1 %helper_crash_cleanup_retire_call134, label %helper_crash_cleanup_retire_accepted135, label %helper_crash_cleanup_retire_rejected136

helper_crash_cleanup_retire_merge132:             ; preds = %helper_crash_cleanup_retire_accepted135, %resource_drop_live_only128
  %ref_drop_handle137 = load ptr, ptr %local_5, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle137)
  store ptr null, ptr %local_5, align 8
  br label %resource_drop_merge129

helper_crash_cleanup_retire_accepted135:          ; preds = %helper_crash_cleanup_retire131
  store i64 0, ptr %helper_crash_cleanup_token_5, align 8
  store i1 false, ptr %helper_crash_cleanup_active_5, align 1
  br label %helper_crash_cleanup_retire_merge132

helper_crash_cleanup_retire_rejected136:          ; preds = %helper_crash_cleanup_retire131
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only140:                       ; preds = %resource_drop_merge129
  %helper_crash_cleanup_drop_active142 = load i1, ptr %helper_crash_cleanup_active_2, align 1
  br i1 %helper_crash_cleanup_drop_active142, label %helper_crash_cleanup_retire143, label %helper_crash_cleanup_retire_merge144

resource_drop_merge141:                           ; preds = %helper_crash_cleanup_retire_merge144, %resource_drop_merge129
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire143:                   ; preds = %resource_drop_live_only140
  %helper_crash_cleanup_retire_token145 = load i64, ptr %helper_crash_cleanup_token_2, align 8
  %helper_crash_cleanup_retire_call146 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token145)
  br i1 %helper_crash_cleanup_retire_call146, label %helper_crash_cleanup_retire_accepted147, label %helper_crash_cleanup_retire_rejected148

helper_crash_cleanup_retire_merge144:             ; preds = %helper_crash_cleanup_retire_accepted147, %resource_drop_live_only140
  %ref_drop_handle149 = load ptr, ptr %local_2, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle149)
  store ptr null, ptr %local_2, align 8
  br label %resource_drop_merge141

helper_crash_cleanup_retire_accepted147:          ; preds = %helper_crash_cleanup_retire143
  store i64 0, ptr %helper_crash_cleanup_token_2, align 8
  store i1 false, ptr %helper_crash_cleanup_active_2, align 1
  br label %helper_crash_cleanup_retire_merge144

helper_crash_cleanup_retire_rejected148:          ; preds = %helper_crash_cleanup_retire143
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_deactivate159:               ; preds = %bb7
  %helper_crash_cleanup_token161 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_deactivate_call162 = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token161)
  br i1 %helper_crash_cleanup_deactivate_call162, label %helper_crash_cleanup_deactivate_accepted163, label %helper_crash_cleanup_deactivate_rejected164

helper_crash_cleanup_deactivate_merge160:         ; preds = %helper_crash_cleanup_deactivate_accepted163, %bb7
  %weak_upgrade_handle = load ptr, ptr %local_8, align 8
  %weak_upgrade = call ptr @hew_weak_upgrade_rc(ptr %weak_upgrade_handle)
  %machine_tag_ptr = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %local_29, i32 0, i32 0
  %machine_payload_ptr = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %local_29, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { ptr }, ptr %machine_payload_ptr, i32 0, i32 0
  %weak_upgrade_is_none = icmp eq ptr %weak_upgrade, null
  br i1 %weak_upgrade_is_none, label %weak_upgrade_none, label %weak_upgrade_some

helper_crash_cleanup_deactivate_accepted163:      ; preds = %helper_crash_cleanup_deactivate159
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_deactivate_merge160

helper_crash_cleanup_deactivate_rejected164:      ; preds = %helper_crash_cleanup_deactivate159
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

weak_upgrade_some:                                ; preds = %helper_crash_cleanup_deactivate_merge160
  store i8 0, ptr %machine_tag_ptr, align 1
  store ptr %weak_upgrade, ptr %machine_variant_field_ptr, align 8
  br label %weak_upgrade_cont

weak_upgrade_none:                                ; preds = %helper_crash_cleanup_deactivate_merge160
  store i8 1, ptr %machine_tag_ptr, align 1
  store ptr null, ptr %machine_variant_field_ptr, align 8
  br label %weak_upgrade_cont

weak_upgrade_cont:                                ; preds = %weak_upgrade_none, %weak_upgrade_some
  %helper_crash_cleanup_prior_token165 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %arm_typed_crash_cleanup166 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token165, ptr %local_29, i64 16, i64 8, ptr @__hew_frame_cleanup_45375aadfceee3dc, i32 1, i32 0)
  %frame_cleanup_arm_failed167 = icmp eq i64 %arm_typed_crash_cleanup166, -1
  br i1 %frame_cleanup_arm_failed167, label %frame_cleanup_rejected169, label %frame_cleanup_registered168

frame_cleanup_registered168:                      ; preds = %weak_upgrade_cont
  store i64 %arm_typed_crash_cleanup166, ptr %helper_crash_cleanup_token_29, align 8
  store i1 true, ptr %helper_crash_cleanup_active_29, align 1
  %machine_tag_ptr170 = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %local_29, i32 0, i32 0
  %move_iN_load = load i8, ptr %machine_tag_ptr170, align 1
  %move_iN_zext = zext i8 %move_iN_load to i64
  store i64 %move_iN_zext, ptr %local_30, align 8
  store i64 0, ptr %local_31, align 8
  %cmp_lhs = load i64, ptr %local_30, align 8
  %cmp_rhs = load i64, ptr %local_31, align 8
  %cmp_bit = icmp eq i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_32, align 1
  %cond_load171 = load i8, ptr %local_32, align 1
  %cond_nz172 = icmp ne i8 %cond_load171, 0
  br i1 %cond_nz172, label %bb9, label %bb12

frame_cleanup_rejected169:                        ; preds = %weak_upgrade_cont
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire174:                   ; preds = %bb8
  %helper_crash_cleanup_retire_token176 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_retire_call177 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token176)
  br i1 %helper_crash_cleanup_retire_call177, label %helper_crash_cleanup_retire_accepted178, label %helper_crash_cleanup_retire_rejected179

helper_crash_cleanup_retire_merge175:             ; preds = %helper_crash_cleanup_retire_accepted178, %bb8
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_29)
  store %"Option$$Rc$li64$g" zeroinitializer, ptr %local_29, align 8
  %move_load180 = load i64, ptr %local_28, align 8
  store i64 %move_load180, ptr %return_slot, align 8
  %helper_crash_cleanup_drop_active181 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_drop_active181, label %helper_crash_cleanup_retire182, label %helper_crash_cleanup_retire_merge183

helper_crash_cleanup_retire_accepted178:          ; preds = %helper_crash_cleanup_retire174
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_retire_merge175

helper_crash_cleanup_retire_rejected179:          ; preds = %helper_crash_cleanup_retire174
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire182:                   ; preds = %helper_crash_cleanup_retire_merge175
  %helper_crash_cleanup_retire_token184 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_retire_call185 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token184)
  br i1 %helper_crash_cleanup_retire_call185, label %helper_crash_cleanup_retire_accepted186, label %helper_crash_cleanup_retire_rejected187

helper_crash_cleanup_retire_merge183:             ; preds = %helper_crash_cleanup_retire_accepted186, %helper_crash_cleanup_retire_merge175
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_29)
  %resource_drop_flag188 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed189 = icmp eq i64 %resource_drop_flag188, 0
  br i1 %resource_drop_not_consumed189, label %resource_drop_live_only190, label %resource_drop_merge191

helper_crash_cleanup_retire_accepted186:          ; preds = %helper_crash_cleanup_retire182
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_retire_merge183

helper_crash_cleanup_retire_rejected187:          ; preds = %helper_crash_cleanup_retire182
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only190:                       ; preds = %helper_crash_cleanup_retire_merge183
  %helper_crash_cleanup_drop_active192 = load i1, ptr %helper_crash_cleanup_active_11, align 1
  br i1 %helper_crash_cleanup_drop_active192, label %helper_crash_cleanup_retire193, label %helper_crash_cleanup_retire_merge194

resource_drop_merge191:                           ; preds = %helper_crash_cleanup_retire_merge194, %helper_crash_cleanup_retire_merge183
  %resource_drop_flag200 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed201 = icmp eq i64 %resource_drop_flag200, 0
  br i1 %resource_drop_not_consumed201, label %resource_drop_live_only202, label %resource_drop_merge203

helper_crash_cleanup_retire193:                   ; preds = %resource_drop_live_only190
  %helper_crash_cleanup_retire_token195 = load i64, ptr %helper_crash_cleanup_token_11, align 8
  %helper_crash_cleanup_retire_call196 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token195)
  br i1 %helper_crash_cleanup_retire_call196, label %helper_crash_cleanup_retire_accepted197, label %helper_crash_cleanup_retire_rejected198

helper_crash_cleanup_retire_merge194:             ; preds = %helper_crash_cleanup_retire_accepted197, %resource_drop_live_only190
  %ref_drop_handle199 = load ptr, ptr %local_11, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle199)
  store ptr null, ptr %local_11, align 8
  br label %resource_drop_merge191

helper_crash_cleanup_retire_accepted197:          ; preds = %helper_crash_cleanup_retire193
  store i64 0, ptr %helper_crash_cleanup_token_11, align 8
  store i1 false, ptr %helper_crash_cleanup_active_11, align 1
  br label %helper_crash_cleanup_retire_merge194

helper_crash_cleanup_retire_rejected198:          ; preds = %helper_crash_cleanup_retire193
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only202:                       ; preds = %resource_drop_merge191
  %helper_crash_cleanup_drop_active204 = load i1, ptr %helper_crash_cleanup_active_8, align 1
  br i1 %helper_crash_cleanup_drop_active204, label %helper_crash_cleanup_retire205, label %helper_crash_cleanup_retire_merge206

resource_drop_merge203:                           ; preds = %helper_crash_cleanup_retire_merge206, %resource_drop_merge191
  %resource_drop_flag212 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed213 = icmp eq i64 %resource_drop_flag212, 0
  br i1 %resource_drop_not_consumed213, label %resource_drop_live_only214, label %resource_drop_merge215

helper_crash_cleanup_retire205:                   ; preds = %resource_drop_live_only202
  %helper_crash_cleanup_retire_token207 = load i64, ptr %helper_crash_cleanup_token_8, align 8
  %helper_crash_cleanup_retire_call208 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token207)
  br i1 %helper_crash_cleanup_retire_call208, label %helper_crash_cleanup_retire_accepted209, label %helper_crash_cleanup_retire_rejected210

helper_crash_cleanup_retire_merge206:             ; preds = %helper_crash_cleanup_retire_accepted209, %resource_drop_live_only202
  %ref_drop_handle211 = load ptr, ptr %local_8, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle211)
  store ptr null, ptr %local_8, align 8
  br label %resource_drop_merge203

helper_crash_cleanup_retire_accepted209:          ; preds = %helper_crash_cleanup_retire205
  store i64 0, ptr %helper_crash_cleanup_token_8, align 8
  store i1 false, ptr %helper_crash_cleanup_active_8, align 1
  br label %helper_crash_cleanup_retire_merge206

helper_crash_cleanup_retire_rejected210:          ; preds = %helper_crash_cleanup_retire205
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only214:                       ; preds = %resource_drop_merge203
  %helper_crash_cleanup_drop_active216 = load i1, ptr %helper_crash_cleanup_active_5, align 1
  br i1 %helper_crash_cleanup_drop_active216, label %helper_crash_cleanup_retire217, label %helper_crash_cleanup_retire_merge218

resource_drop_merge215:                           ; preds = %helper_crash_cleanup_retire_merge218, %resource_drop_merge203
  %resource_drop_flag224 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed225 = icmp eq i64 %resource_drop_flag224, 0
  br i1 %resource_drop_not_consumed225, label %resource_drop_live_only226, label %resource_drop_merge227

helper_crash_cleanup_retire217:                   ; preds = %resource_drop_live_only214
  %helper_crash_cleanup_retire_token219 = load i64, ptr %helper_crash_cleanup_token_5, align 8
  %helper_crash_cleanup_retire_call220 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token219)
  br i1 %helper_crash_cleanup_retire_call220, label %helper_crash_cleanup_retire_accepted221, label %helper_crash_cleanup_retire_rejected222

helper_crash_cleanup_retire_merge218:             ; preds = %helper_crash_cleanup_retire_accepted221, %resource_drop_live_only214
  %ref_drop_handle223 = load ptr, ptr %local_5, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle223)
  store ptr null, ptr %local_5, align 8
  br label %resource_drop_merge215

helper_crash_cleanup_retire_accepted221:          ; preds = %helper_crash_cleanup_retire217
  store i64 0, ptr %helper_crash_cleanup_token_5, align 8
  store i1 false, ptr %helper_crash_cleanup_active_5, align 1
  br label %helper_crash_cleanup_retire_merge218

helper_crash_cleanup_retire_rejected222:          ; preds = %helper_crash_cleanup_retire217
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only226:                       ; preds = %resource_drop_merge215
  %helper_crash_cleanup_drop_active228 = load i1, ptr %helper_crash_cleanup_active_2, align 1
  br i1 %helper_crash_cleanup_drop_active228, label %helper_crash_cleanup_retire229, label %helper_crash_cleanup_retire_merge230

resource_drop_merge227:                           ; preds = %helper_crash_cleanup_retire_merge230, %resource_drop_merge215
  %helper_crash_cleanup_return_token_2 = load i64, ptr %helper_crash_cleanup_token_2, align 8
  %helper_crash_cleanup_return_has_token_2 = icmp ne i64 %helper_crash_cleanup_return_token_2, 0
  br i1 %helper_crash_cleanup_return_has_token_2, label %helper_crash_cleanup_return_retire_2, label %helper_crash_cleanup_return_merge_2

helper_crash_cleanup_retire229:                   ; preds = %resource_drop_live_only226
  %helper_crash_cleanup_retire_token231 = load i64, ptr %helper_crash_cleanup_token_2, align 8
  %helper_crash_cleanup_retire_call232 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token231)
  br i1 %helper_crash_cleanup_retire_call232, label %helper_crash_cleanup_retire_accepted233, label %helper_crash_cleanup_retire_rejected234

helper_crash_cleanup_retire_merge230:             ; preds = %helper_crash_cleanup_retire_accepted233, %resource_drop_live_only226
  %ref_drop_handle235 = load ptr, ptr %local_2, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle235)
  store ptr null, ptr %local_2, align 8
  br label %resource_drop_merge227

helper_crash_cleanup_retire_accepted233:          ; preds = %helper_crash_cleanup_retire229
  store i64 0, ptr %helper_crash_cleanup_token_2, align 8
  store i1 false, ptr %helper_crash_cleanup_active_2, align 1
  br label %helper_crash_cleanup_retire_merge230

helper_crash_cleanup_retire_rejected234:          ; preds = %helper_crash_cleanup_retire229
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_return_merge_2:              ; preds = %helper_crash_cleanup_return_retire_2_accepted, %resource_drop_merge227
  %helper_crash_cleanup_return_token_5 = load i64, ptr %helper_crash_cleanup_token_5, align 8
  %helper_crash_cleanup_return_has_token_5 = icmp ne i64 %helper_crash_cleanup_return_token_5, 0
  br i1 %helper_crash_cleanup_return_has_token_5, label %helper_crash_cleanup_return_retire_5, label %helper_crash_cleanup_return_merge_5

helper_crash_cleanup_return_retire_2:             ; preds = %resource_drop_merge227
  %helper_crash_cleanup_return_retire_2_call = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_return_token_2)
  br i1 %helper_crash_cleanup_return_retire_2_call, label %helper_crash_cleanup_return_retire_2_accepted, label %helper_crash_cleanup_return_retire_2_rejected

helper_crash_cleanup_return_retire_2_accepted:    ; preds = %helper_crash_cleanup_return_retire_2
  store i64 0, ptr %helper_crash_cleanup_token_2, align 8
  store i1 false, ptr %helper_crash_cleanup_active_2, align 1
  br label %helper_crash_cleanup_return_merge_2

helper_crash_cleanup_return_retire_2_rejected:    ; preds = %helper_crash_cleanup_return_retire_2
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_return_merge_5:              ; preds = %helper_crash_cleanup_return_retire_5_accepted, %helper_crash_cleanup_return_merge_2
  %helper_crash_cleanup_return_token_8 = load i64, ptr %helper_crash_cleanup_token_8, align 8
  %helper_crash_cleanup_return_has_token_8 = icmp ne i64 %helper_crash_cleanup_return_token_8, 0
  br i1 %helper_crash_cleanup_return_has_token_8, label %helper_crash_cleanup_return_retire_8, label %helper_crash_cleanup_return_merge_8

helper_crash_cleanup_return_retire_5:             ; preds = %helper_crash_cleanup_return_merge_2
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

helper_crash_cleanup_return_merge_8:              ; preds = %helper_crash_cleanup_return_retire_8_accepted, %helper_crash_cleanup_return_merge_5
  %helper_crash_cleanup_return_token_11 = load i64, ptr %helper_crash_cleanup_token_11, align 8
  %helper_crash_cleanup_return_has_token_11 = icmp ne i64 %helper_crash_cleanup_return_token_11, 0
  br i1 %helper_crash_cleanup_return_has_token_11, label %helper_crash_cleanup_return_retire_11, label %helper_crash_cleanup_return_merge_11

helper_crash_cleanup_return_retire_8:             ; preds = %helper_crash_cleanup_return_merge_5
  %helper_crash_cleanup_return_retire_8_call = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_return_token_8)
  br i1 %helper_crash_cleanup_return_retire_8_call, label %helper_crash_cleanup_return_retire_8_accepted, label %helper_crash_cleanup_return_retire_8_rejected

helper_crash_cleanup_return_retire_8_accepted:    ; preds = %helper_crash_cleanup_return_retire_8
  store i64 0, ptr %helper_crash_cleanup_token_8, align 8
  store i1 false, ptr %helper_crash_cleanup_active_8, align 1
  br label %helper_crash_cleanup_return_merge_8

helper_crash_cleanup_return_retire_8_rejected:    ; preds = %helper_crash_cleanup_return_retire_8
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_return_merge_11:             ; preds = %helper_crash_cleanup_return_retire_11_accepted, %helper_crash_cleanup_return_merge_8
  %helper_crash_cleanup_return_token_29 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_return_has_token_29 = icmp ne i64 %helper_crash_cleanup_return_token_29, 0
  br i1 %helper_crash_cleanup_return_has_token_29, label %helper_crash_cleanup_return_retire_29, label %helper_crash_cleanup_return_merge_29

helper_crash_cleanup_return_retire_11:            ; preds = %helper_crash_cleanup_return_merge_8
  %helper_crash_cleanup_return_retire_11_call = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_return_token_11)
  br i1 %helper_crash_cleanup_return_retire_11_call, label %helper_crash_cleanup_return_retire_11_accepted, label %helper_crash_cleanup_return_retire_11_rejected

helper_crash_cleanup_return_retire_11_accepted:   ; preds = %helper_crash_cleanup_return_retire_11
  store i64 0, ptr %helper_crash_cleanup_token_11, align 8
  store i1 false, ptr %helper_crash_cleanup_active_11, align 1
  br label %helper_crash_cleanup_return_merge_11

helper_crash_cleanup_return_retire_11_rejected:   ; preds = %helper_crash_cleanup_return_retire_11
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_return_merge_29:             ; preds = %helper_crash_cleanup_return_retire_29_accepted, %helper_crash_cleanup_return_merge_11
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val

helper_crash_cleanup_return_retire_29:            ; preds = %helper_crash_cleanup_return_merge_11
  %helper_crash_cleanup_return_retire_29_call = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_return_token_29)
  br i1 %helper_crash_cleanup_return_retire_29_call, label %helper_crash_cleanup_return_retire_29_accepted, label %helper_crash_cleanup_return_retire_29_rejected

helper_crash_cleanup_return_retire_29_accepted:   ; preds = %helper_crash_cleanup_return_retire_29
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_return_merge_29

helper_crash_cleanup_return_retire_29_rejected:   ; preds = %helper_crash_cleanup_return_retire_29
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

cancel_exit252:                                   ; preds = %bb10
  %helper_crash_cleanup_drop_active254 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_drop_active254, label %helper_crash_cleanup_retire255, label %helper_crash_cleanup_retire_merge256

after_cooperate253:                               ; preds = %bb10
  br label %bb8

helper_crash_cleanup_retire255:                   ; preds = %cancel_exit252
  %helper_crash_cleanup_retire_token257 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_retire_call258 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token257)
  br i1 %helper_crash_cleanup_retire_call258, label %helper_crash_cleanup_retire_accepted259, label %helper_crash_cleanup_retire_rejected260

helper_crash_cleanup_retire_merge256:             ; preds = %helper_crash_cleanup_retire_accepted259, %cancel_exit252
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_29)
  %resource_drop_flag261 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed262 = icmp eq i64 %resource_drop_flag261, 0
  br i1 %resource_drop_not_consumed262, label %resource_drop_live_only263, label %resource_drop_merge264

helper_crash_cleanup_retire_accepted259:          ; preds = %helper_crash_cleanup_retire255
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_retire_merge256

helper_crash_cleanup_retire_rejected260:          ; preds = %helper_crash_cleanup_retire255
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only263:                       ; preds = %helper_crash_cleanup_retire_merge256
  %helper_crash_cleanup_drop_active265 = load i1, ptr %helper_crash_cleanup_active_11, align 1
  br i1 %helper_crash_cleanup_drop_active265, label %helper_crash_cleanup_retire266, label %helper_crash_cleanup_retire_merge267

resource_drop_merge264:                           ; preds = %helper_crash_cleanup_retire_merge267, %helper_crash_cleanup_retire_merge256
  %resource_drop_flag273 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed274 = icmp eq i64 %resource_drop_flag273, 0
  br i1 %resource_drop_not_consumed274, label %resource_drop_live_only275, label %resource_drop_merge276

helper_crash_cleanup_retire266:                   ; preds = %resource_drop_live_only263
  %helper_crash_cleanup_retire_token268 = load i64, ptr %helper_crash_cleanup_token_11, align 8
  %helper_crash_cleanup_retire_call269 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token268)
  br i1 %helper_crash_cleanup_retire_call269, label %helper_crash_cleanup_retire_accepted270, label %helper_crash_cleanup_retire_rejected271

helper_crash_cleanup_retire_merge267:             ; preds = %helper_crash_cleanup_retire_accepted270, %resource_drop_live_only263
  %ref_drop_handle272 = load ptr, ptr %local_11, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle272)
  store ptr null, ptr %local_11, align 8
  br label %resource_drop_merge264

helper_crash_cleanup_retire_accepted270:          ; preds = %helper_crash_cleanup_retire266
  store i64 0, ptr %helper_crash_cleanup_token_11, align 8
  store i1 false, ptr %helper_crash_cleanup_active_11, align 1
  br label %helper_crash_cleanup_retire_merge267

helper_crash_cleanup_retire_rejected271:          ; preds = %helper_crash_cleanup_retire266
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only275:                       ; preds = %resource_drop_merge264
  %helper_crash_cleanup_drop_active277 = load i1, ptr %helper_crash_cleanup_active_8, align 1
  br i1 %helper_crash_cleanup_drop_active277, label %helper_crash_cleanup_retire278, label %helper_crash_cleanup_retire_merge279

resource_drop_merge276:                           ; preds = %helper_crash_cleanup_retire_merge279, %resource_drop_merge264
  %resource_drop_flag285 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed286 = icmp eq i64 %resource_drop_flag285, 0
  br i1 %resource_drop_not_consumed286, label %resource_drop_live_only287, label %resource_drop_merge288

helper_crash_cleanup_retire278:                   ; preds = %resource_drop_live_only275
  %helper_crash_cleanup_retire_token280 = load i64, ptr %helper_crash_cleanup_token_8, align 8
  %helper_crash_cleanup_retire_call281 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token280)
  br i1 %helper_crash_cleanup_retire_call281, label %helper_crash_cleanup_retire_accepted282, label %helper_crash_cleanup_retire_rejected283

helper_crash_cleanup_retire_merge279:             ; preds = %helper_crash_cleanup_retire_accepted282, %resource_drop_live_only275
  %ref_drop_handle284 = load ptr, ptr %local_8, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle284)
  store ptr null, ptr %local_8, align 8
  br label %resource_drop_merge276

helper_crash_cleanup_retire_accepted282:          ; preds = %helper_crash_cleanup_retire278
  store i64 0, ptr %helper_crash_cleanup_token_8, align 8
  store i1 false, ptr %helper_crash_cleanup_active_8, align 1
  br label %helper_crash_cleanup_retire_merge279

helper_crash_cleanup_retire_rejected283:          ; preds = %helper_crash_cleanup_retire278
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only287:                       ; preds = %resource_drop_merge276
  %helper_crash_cleanup_drop_active289 = load i1, ptr %helper_crash_cleanup_active_5, align 1
  br i1 %helper_crash_cleanup_drop_active289, label %helper_crash_cleanup_retire290, label %helper_crash_cleanup_retire_merge291

resource_drop_merge288:                           ; preds = %helper_crash_cleanup_retire_merge291, %resource_drop_merge276
  %resource_drop_flag297 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed298 = icmp eq i64 %resource_drop_flag297, 0
  br i1 %resource_drop_not_consumed298, label %resource_drop_live_only299, label %resource_drop_merge300

helper_crash_cleanup_retire290:                   ; preds = %resource_drop_live_only287
  %helper_crash_cleanup_retire_token292 = load i64, ptr %helper_crash_cleanup_token_5, align 8
  %helper_crash_cleanup_retire_call293 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token292)
  br i1 %helper_crash_cleanup_retire_call293, label %helper_crash_cleanup_retire_accepted294, label %helper_crash_cleanup_retire_rejected295

helper_crash_cleanup_retire_merge291:             ; preds = %helper_crash_cleanup_retire_accepted294, %resource_drop_live_only287
  %ref_drop_handle296 = load ptr, ptr %local_5, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle296)
  store ptr null, ptr %local_5, align 8
  br label %resource_drop_merge288

helper_crash_cleanup_retire_accepted294:          ; preds = %helper_crash_cleanup_retire290
  store i64 0, ptr %helper_crash_cleanup_token_5, align 8
  store i1 false, ptr %helper_crash_cleanup_active_5, align 1
  br label %helper_crash_cleanup_retire_merge291

helper_crash_cleanup_retire_rejected295:          ; preds = %helper_crash_cleanup_retire290
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only299:                       ; preds = %resource_drop_merge288
  %helper_crash_cleanup_drop_active301 = load i1, ptr %helper_crash_cleanup_active_2, align 1
  br i1 %helper_crash_cleanup_drop_active301, label %helper_crash_cleanup_retire302, label %helper_crash_cleanup_retire_merge303

resource_drop_merge300:                           ; preds = %helper_crash_cleanup_retire_merge303, %resource_drop_merge288
  ret i64 0

helper_crash_cleanup_retire302:                   ; preds = %resource_drop_live_only299
  %helper_crash_cleanup_retire_token304 = load i64, ptr %helper_crash_cleanup_token_2, align 8
  %helper_crash_cleanup_retire_call305 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token304)
  br i1 %helper_crash_cleanup_retire_call305, label %helper_crash_cleanup_retire_accepted306, label %helper_crash_cleanup_retire_rejected307

helper_crash_cleanup_retire_merge303:             ; preds = %helper_crash_cleanup_retire_accepted306, %resource_drop_live_only299
  %ref_drop_handle308 = load ptr, ptr %local_2, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle308)
  store ptr null, ptr %local_2, align 8
  br label %resource_drop_merge300

helper_crash_cleanup_retire_accepted306:          ; preds = %helper_crash_cleanup_retire302
  store i64 0, ptr %helper_crash_cleanup_token_2, align 8
  store i1 false, ptr %helper_crash_cleanup_active_2, align 1
  br label %helper_crash_cleanup_retire_merge303

helper_crash_cleanup_retire_rejected307:          ; preds = %helper_crash_cleanup_retire302
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire310:                   ; preds = %bb11
  %helper_crash_cleanup_retire_token312 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_retire_call313 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token312)
  br i1 %helper_crash_cleanup_retire_call313, label %helper_crash_cleanup_retire_accepted314, label %helper_crash_cleanup_retire_rejected315

helper_crash_cleanup_retire_merge311:             ; preds = %helper_crash_cleanup_retire_accepted314, %bb11
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_29)
  %resource_drop_flag316 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed317 = icmp eq i64 %resource_drop_flag316, 0
  br i1 %resource_drop_not_consumed317, label %resource_drop_live_only318, label %resource_drop_merge319

helper_crash_cleanup_retire_accepted314:          ; preds = %helper_crash_cleanup_retire310
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_retire_merge311

helper_crash_cleanup_retire_rejected315:          ; preds = %helper_crash_cleanup_retire310
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only318:                       ; preds = %helper_crash_cleanup_retire_merge311
  %helper_crash_cleanup_drop_active320 = load i1, ptr %helper_crash_cleanup_active_11, align 1
  br i1 %helper_crash_cleanup_drop_active320, label %helper_crash_cleanup_retire321, label %helper_crash_cleanup_retire_merge322

resource_drop_merge319:                           ; preds = %helper_crash_cleanup_retire_merge322, %helper_crash_cleanup_retire_merge311
  %resource_drop_flag328 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed329 = icmp eq i64 %resource_drop_flag328, 0
  br i1 %resource_drop_not_consumed329, label %resource_drop_live_only330, label %resource_drop_merge331

helper_crash_cleanup_retire321:                   ; preds = %resource_drop_live_only318
  %helper_crash_cleanup_retire_token323 = load i64, ptr %helper_crash_cleanup_token_11, align 8
  %helper_crash_cleanup_retire_call324 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token323)
  br i1 %helper_crash_cleanup_retire_call324, label %helper_crash_cleanup_retire_accepted325, label %helper_crash_cleanup_retire_rejected326

helper_crash_cleanup_retire_merge322:             ; preds = %helper_crash_cleanup_retire_accepted325, %resource_drop_live_only318
  %ref_drop_handle327 = load ptr, ptr %local_11, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle327)
  store ptr null, ptr %local_11, align 8
  br label %resource_drop_merge319

helper_crash_cleanup_retire_accepted325:          ; preds = %helper_crash_cleanup_retire321
  store i64 0, ptr %helper_crash_cleanup_token_11, align 8
  store i1 false, ptr %helper_crash_cleanup_active_11, align 1
  br label %helper_crash_cleanup_retire_merge322

helper_crash_cleanup_retire_rejected326:          ; preds = %helper_crash_cleanup_retire321
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only330:                       ; preds = %resource_drop_merge319
  %helper_crash_cleanup_drop_active332 = load i1, ptr %helper_crash_cleanup_active_8, align 1
  br i1 %helper_crash_cleanup_drop_active332, label %helper_crash_cleanup_retire333, label %helper_crash_cleanup_retire_merge334

resource_drop_merge331:                           ; preds = %helper_crash_cleanup_retire_merge334, %resource_drop_merge319
  %resource_drop_flag340 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed341 = icmp eq i64 %resource_drop_flag340, 0
  br i1 %resource_drop_not_consumed341, label %resource_drop_live_only342, label %resource_drop_merge343

helper_crash_cleanup_retire333:                   ; preds = %resource_drop_live_only330
  %helper_crash_cleanup_retire_token335 = load i64, ptr %helper_crash_cleanup_token_8, align 8
  %helper_crash_cleanup_retire_call336 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token335)
  br i1 %helper_crash_cleanup_retire_call336, label %helper_crash_cleanup_retire_accepted337, label %helper_crash_cleanup_retire_rejected338

helper_crash_cleanup_retire_merge334:             ; preds = %helper_crash_cleanup_retire_accepted337, %resource_drop_live_only330
  %ref_drop_handle339 = load ptr, ptr %local_8, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle339)
  store ptr null, ptr %local_8, align 8
  br label %resource_drop_merge331

helper_crash_cleanup_retire_accepted337:          ; preds = %helper_crash_cleanup_retire333
  store i64 0, ptr %helper_crash_cleanup_token_8, align 8
  store i1 false, ptr %helper_crash_cleanup_active_8, align 1
  br label %helper_crash_cleanup_retire_merge334

helper_crash_cleanup_retire_rejected338:          ; preds = %helper_crash_cleanup_retire333
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only342:                       ; preds = %resource_drop_merge331
  %helper_crash_cleanup_drop_active344 = load i1, ptr %helper_crash_cleanup_active_5, align 1
  br i1 %helper_crash_cleanup_drop_active344, label %helper_crash_cleanup_retire345, label %helper_crash_cleanup_retire_merge346

resource_drop_merge343:                           ; preds = %helper_crash_cleanup_retire_merge346, %resource_drop_merge331
  %resource_drop_flag352 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed353 = icmp eq i64 %resource_drop_flag352, 0
  br i1 %resource_drop_not_consumed353, label %resource_drop_live_only354, label %resource_drop_merge355

helper_crash_cleanup_retire345:                   ; preds = %resource_drop_live_only342
  %helper_crash_cleanup_retire_token347 = load i64, ptr %helper_crash_cleanup_token_5, align 8
  %helper_crash_cleanup_retire_call348 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token347)
  br i1 %helper_crash_cleanup_retire_call348, label %helper_crash_cleanup_retire_accepted349, label %helper_crash_cleanup_retire_rejected350

helper_crash_cleanup_retire_merge346:             ; preds = %helper_crash_cleanup_retire_accepted349, %resource_drop_live_only342
  %ref_drop_handle351 = load ptr, ptr %local_5, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle351)
  store ptr null, ptr %local_5, align 8
  br label %resource_drop_merge343

helper_crash_cleanup_retire_accepted349:          ; preds = %helper_crash_cleanup_retire345
  store i64 0, ptr %helper_crash_cleanup_token_5, align 8
  store i1 false, ptr %helper_crash_cleanup_active_5, align 1
  br label %helper_crash_cleanup_retire_merge346

helper_crash_cleanup_retire_rejected350:          ; preds = %helper_crash_cleanup_retire345
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only354:                       ; preds = %resource_drop_merge343
  %helper_crash_cleanup_drop_active356 = load i1, ptr %helper_crash_cleanup_active_2, align 1
  br i1 %helper_crash_cleanup_drop_active356, label %helper_crash_cleanup_retire357, label %helper_crash_cleanup_retire_merge358

resource_drop_merge355:                           ; preds = %helper_crash_cleanup_retire_merge358, %resource_drop_merge343
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire357:                   ; preds = %resource_drop_live_only354
  %helper_crash_cleanup_retire_token359 = load i64, ptr %helper_crash_cleanup_token_2, align 8
  %helper_crash_cleanup_retire_call360 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token359)
  br i1 %helper_crash_cleanup_retire_call360, label %helper_crash_cleanup_retire_accepted361, label %helper_crash_cleanup_retire_rejected362

helper_crash_cleanup_retire_merge358:             ; preds = %helper_crash_cleanup_retire_accepted361, %resource_drop_live_only354
  %ref_drop_handle363 = load ptr, ptr %local_2, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle363)
  store ptr null, ptr %local_2, align 8
  br label %resource_drop_merge355

helper_crash_cleanup_retire_accepted361:          ; preds = %helper_crash_cleanup_retire357
  store i64 0, ptr %helper_crash_cleanup_token_2, align 8
  store i1 false, ptr %helper_crash_cleanup_active_2, align 1
  br label %helper_crash_cleanup_retire_merge358

helper_crash_cleanup_retire_rejected362:          ; preds = %helper_crash_cleanup_retire357
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire371:                   ; preds = %bb13
  %helper_crash_cleanup_retire_token373 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_retire_call374 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token373)
  br i1 %helper_crash_cleanup_retire_call374, label %helper_crash_cleanup_retire_accepted375, label %helper_crash_cleanup_retire_rejected376

helper_crash_cleanup_retire_merge372:             ; preds = %helper_crash_cleanup_retire_accepted375, %bb13
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_29)
  %resource_drop_flag377 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed378 = icmp eq i64 %resource_drop_flag377, 0
  br i1 %resource_drop_not_consumed378, label %resource_drop_live_only379, label %resource_drop_merge380

helper_crash_cleanup_retire_accepted375:          ; preds = %helper_crash_cleanup_retire371
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_retire_merge372

helper_crash_cleanup_retire_rejected376:          ; preds = %helper_crash_cleanup_retire371
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only379:                       ; preds = %helper_crash_cleanup_retire_merge372
  %helper_crash_cleanup_drop_active381 = load i1, ptr %helper_crash_cleanup_active_11, align 1
  br i1 %helper_crash_cleanup_drop_active381, label %helper_crash_cleanup_retire382, label %helper_crash_cleanup_retire_merge383

resource_drop_merge380:                           ; preds = %helper_crash_cleanup_retire_merge383, %helper_crash_cleanup_retire_merge372
  %resource_drop_flag389 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed390 = icmp eq i64 %resource_drop_flag389, 0
  br i1 %resource_drop_not_consumed390, label %resource_drop_live_only391, label %resource_drop_merge392

helper_crash_cleanup_retire382:                   ; preds = %resource_drop_live_only379
  %helper_crash_cleanup_retire_token384 = load i64, ptr %helper_crash_cleanup_token_11, align 8
  %helper_crash_cleanup_retire_call385 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token384)
  br i1 %helper_crash_cleanup_retire_call385, label %helper_crash_cleanup_retire_accepted386, label %helper_crash_cleanup_retire_rejected387

helper_crash_cleanup_retire_merge383:             ; preds = %helper_crash_cleanup_retire_accepted386, %resource_drop_live_only379
  %ref_drop_handle388 = load ptr, ptr %local_11, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle388)
  store ptr null, ptr %local_11, align 8
  br label %resource_drop_merge380

helper_crash_cleanup_retire_accepted386:          ; preds = %helper_crash_cleanup_retire382
  store i64 0, ptr %helper_crash_cleanup_token_11, align 8
  store i1 false, ptr %helper_crash_cleanup_active_11, align 1
  br label %helper_crash_cleanup_retire_merge383

helper_crash_cleanup_retire_rejected387:          ; preds = %helper_crash_cleanup_retire382
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only391:                       ; preds = %resource_drop_merge380
  %helper_crash_cleanup_drop_active393 = load i1, ptr %helper_crash_cleanup_active_8, align 1
  br i1 %helper_crash_cleanup_drop_active393, label %helper_crash_cleanup_retire394, label %helper_crash_cleanup_retire_merge395

resource_drop_merge392:                           ; preds = %helper_crash_cleanup_retire_merge395, %resource_drop_merge380
  %resource_drop_flag401 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed402 = icmp eq i64 %resource_drop_flag401, 0
  br i1 %resource_drop_not_consumed402, label %resource_drop_live_only403, label %resource_drop_merge404

helper_crash_cleanup_retire394:                   ; preds = %resource_drop_live_only391
  %helper_crash_cleanup_retire_token396 = load i64, ptr %helper_crash_cleanup_token_8, align 8
  %helper_crash_cleanup_retire_call397 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token396)
  br i1 %helper_crash_cleanup_retire_call397, label %helper_crash_cleanup_retire_accepted398, label %helper_crash_cleanup_retire_rejected399

helper_crash_cleanup_retire_merge395:             ; preds = %helper_crash_cleanup_retire_accepted398, %resource_drop_live_only391
  %ref_drop_handle400 = load ptr, ptr %local_8, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle400)
  store ptr null, ptr %local_8, align 8
  br label %resource_drop_merge392

helper_crash_cleanup_retire_accepted398:          ; preds = %helper_crash_cleanup_retire394
  store i64 0, ptr %helper_crash_cleanup_token_8, align 8
  store i1 false, ptr %helper_crash_cleanup_active_8, align 1
  br label %helper_crash_cleanup_retire_merge395

helper_crash_cleanup_retire_rejected399:          ; preds = %helper_crash_cleanup_retire394
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only403:                       ; preds = %resource_drop_merge392
  %helper_crash_cleanup_drop_active405 = load i1, ptr %helper_crash_cleanup_active_5, align 1
  br i1 %helper_crash_cleanup_drop_active405, label %helper_crash_cleanup_retire406, label %helper_crash_cleanup_retire_merge407

resource_drop_merge404:                           ; preds = %helper_crash_cleanup_retire_merge407, %resource_drop_merge392
  %resource_drop_flag413 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed414 = icmp eq i64 %resource_drop_flag413, 0
  br i1 %resource_drop_not_consumed414, label %resource_drop_live_only415, label %resource_drop_merge416

helper_crash_cleanup_retire406:                   ; preds = %resource_drop_live_only403
  %helper_crash_cleanup_retire_token408 = load i64, ptr %helper_crash_cleanup_token_5, align 8
  %helper_crash_cleanup_retire_call409 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token408)
  br i1 %helper_crash_cleanup_retire_call409, label %helper_crash_cleanup_retire_accepted410, label %helper_crash_cleanup_retire_rejected411

helper_crash_cleanup_retire_merge407:             ; preds = %helper_crash_cleanup_retire_accepted410, %resource_drop_live_only403
  %ref_drop_handle412 = load ptr, ptr %local_5, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle412)
  store ptr null, ptr %local_5, align 8
  br label %resource_drop_merge404

helper_crash_cleanup_retire_accepted410:          ; preds = %helper_crash_cleanup_retire406
  store i64 0, ptr %helper_crash_cleanup_token_5, align 8
  store i1 false, ptr %helper_crash_cleanup_active_5, align 1
  br label %helper_crash_cleanup_retire_merge407

helper_crash_cleanup_retire_rejected411:          ; preds = %helper_crash_cleanup_retire406
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only415:                       ; preds = %resource_drop_merge404
  %helper_crash_cleanup_drop_active417 = load i1, ptr %helper_crash_cleanup_active_2, align 1
  br i1 %helper_crash_cleanup_drop_active417, label %helper_crash_cleanup_retire418, label %helper_crash_cleanup_retire_merge419

resource_drop_merge416:                           ; preds = %helper_crash_cleanup_retire_merge419, %resource_drop_merge404
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire418:                   ; preds = %resource_drop_live_only415
  %helper_crash_cleanup_retire_token420 = load i64, ptr %helper_crash_cleanup_token_2, align 8
  %helper_crash_cleanup_retire_call421 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token420)
  br i1 %helper_crash_cleanup_retire_call421, label %helper_crash_cleanup_retire_accepted422, label %helper_crash_cleanup_retire_rejected423

helper_crash_cleanup_retire_merge419:             ; preds = %helper_crash_cleanup_retire_accepted422, %resource_drop_live_only415
  %ref_drop_handle424 = load ptr, ptr %local_2, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle424)
  store ptr null, ptr %local_2, align 8
  br label %resource_drop_merge416

helper_crash_cleanup_retire_accepted422:          ; preds = %helper_crash_cleanup_retire418
  store i64 0, ptr %helper_crash_cleanup_token_2, align 8
  store i1 false, ptr %helper_crash_cleanup_active_2, align 1
  br label %helper_crash_cleanup_retire_merge419

helper_crash_cleanup_retire_rejected423:          ; preds = %helper_crash_cleanup_retire418
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire434:                   ; preds = %bb15
  %helper_crash_cleanup_retire_token436 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_retire_call437 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token436)
  br i1 %helper_crash_cleanup_retire_call437, label %helper_crash_cleanup_retire_accepted438, label %helper_crash_cleanup_retire_rejected439

helper_crash_cleanup_retire_merge435:             ; preds = %helper_crash_cleanup_retire_accepted438, %bb15
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_29)
  %resource_drop_flag440 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed441 = icmp eq i64 %resource_drop_flag440, 0
  br i1 %resource_drop_not_consumed441, label %resource_drop_live_only442, label %resource_drop_merge443

helper_crash_cleanup_retire_accepted438:          ; preds = %helper_crash_cleanup_retire434
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_retire_merge435

helper_crash_cleanup_retire_rejected439:          ; preds = %helper_crash_cleanup_retire434
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only442:                       ; preds = %helper_crash_cleanup_retire_merge435
  %helper_crash_cleanup_drop_active444 = load i1, ptr %helper_crash_cleanup_active_11, align 1
  br i1 %helper_crash_cleanup_drop_active444, label %helper_crash_cleanup_retire445, label %helper_crash_cleanup_retire_merge446

resource_drop_merge443:                           ; preds = %helper_crash_cleanup_retire_merge446, %helper_crash_cleanup_retire_merge435
  %resource_drop_flag452 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed453 = icmp eq i64 %resource_drop_flag452, 0
  br i1 %resource_drop_not_consumed453, label %resource_drop_live_only454, label %resource_drop_merge455

helper_crash_cleanup_retire445:                   ; preds = %resource_drop_live_only442
  %helper_crash_cleanup_retire_token447 = load i64, ptr %helper_crash_cleanup_token_11, align 8
  %helper_crash_cleanup_retire_call448 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token447)
  br i1 %helper_crash_cleanup_retire_call448, label %helper_crash_cleanup_retire_accepted449, label %helper_crash_cleanup_retire_rejected450

helper_crash_cleanup_retire_merge446:             ; preds = %helper_crash_cleanup_retire_accepted449, %resource_drop_live_only442
  %ref_drop_handle451 = load ptr, ptr %local_11, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle451)
  store ptr null, ptr %local_11, align 8
  br label %resource_drop_merge443

helper_crash_cleanup_retire_accepted449:          ; preds = %helper_crash_cleanup_retire445
  store i64 0, ptr %helper_crash_cleanup_token_11, align 8
  store i1 false, ptr %helper_crash_cleanup_active_11, align 1
  br label %helper_crash_cleanup_retire_merge446

helper_crash_cleanup_retire_rejected450:          ; preds = %helper_crash_cleanup_retire445
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only454:                       ; preds = %resource_drop_merge443
  %helper_crash_cleanup_drop_active456 = load i1, ptr %helper_crash_cleanup_active_8, align 1
  br i1 %helper_crash_cleanup_drop_active456, label %helper_crash_cleanup_retire457, label %helper_crash_cleanup_retire_merge458

resource_drop_merge455:                           ; preds = %helper_crash_cleanup_retire_merge458, %resource_drop_merge443
  %resource_drop_flag464 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed465 = icmp eq i64 %resource_drop_flag464, 0
  br i1 %resource_drop_not_consumed465, label %resource_drop_live_only466, label %resource_drop_merge467

helper_crash_cleanup_retire457:                   ; preds = %resource_drop_live_only454
  %helper_crash_cleanup_retire_token459 = load i64, ptr %helper_crash_cleanup_token_8, align 8
  %helper_crash_cleanup_retire_call460 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token459)
  br i1 %helper_crash_cleanup_retire_call460, label %helper_crash_cleanup_retire_accepted461, label %helper_crash_cleanup_retire_rejected462

helper_crash_cleanup_retire_merge458:             ; preds = %helper_crash_cleanup_retire_accepted461, %resource_drop_live_only454
  %ref_drop_handle463 = load ptr, ptr %local_8, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle463)
  store ptr null, ptr %local_8, align 8
  br label %resource_drop_merge455

helper_crash_cleanup_retire_accepted461:          ; preds = %helper_crash_cleanup_retire457
  store i64 0, ptr %helper_crash_cleanup_token_8, align 8
  store i1 false, ptr %helper_crash_cleanup_active_8, align 1
  br label %helper_crash_cleanup_retire_merge458

helper_crash_cleanup_retire_rejected462:          ; preds = %helper_crash_cleanup_retire457
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only466:                       ; preds = %resource_drop_merge455
  %helper_crash_cleanup_drop_active468 = load i1, ptr %helper_crash_cleanup_active_5, align 1
  br i1 %helper_crash_cleanup_drop_active468, label %helper_crash_cleanup_retire469, label %helper_crash_cleanup_retire_merge470

resource_drop_merge467:                           ; preds = %helper_crash_cleanup_retire_merge470, %resource_drop_merge455
  %resource_drop_flag476 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed477 = icmp eq i64 %resource_drop_flag476, 0
  br i1 %resource_drop_not_consumed477, label %resource_drop_live_only478, label %resource_drop_merge479

helper_crash_cleanup_retire469:                   ; preds = %resource_drop_live_only466
  %helper_crash_cleanup_retire_token471 = load i64, ptr %helper_crash_cleanup_token_5, align 8
  %helper_crash_cleanup_retire_call472 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token471)
  br i1 %helper_crash_cleanup_retire_call472, label %helper_crash_cleanup_retire_accepted473, label %helper_crash_cleanup_retire_rejected474

helper_crash_cleanup_retire_merge470:             ; preds = %helper_crash_cleanup_retire_accepted473, %resource_drop_live_only466
  %ref_drop_handle475 = load ptr, ptr %local_5, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle475)
  store ptr null, ptr %local_5, align 8
  br label %resource_drop_merge467

helper_crash_cleanup_retire_accepted473:          ; preds = %helper_crash_cleanup_retire469
  store i64 0, ptr %helper_crash_cleanup_token_5, align 8
  store i1 false, ptr %helper_crash_cleanup_active_5, align 1
  br label %helper_crash_cleanup_retire_merge470

helper_crash_cleanup_retire_rejected474:          ; preds = %helper_crash_cleanup_retire469
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only478:                       ; preds = %resource_drop_merge467
  %helper_crash_cleanup_drop_active480 = load i1, ptr %helper_crash_cleanup_active_2, align 1
  br i1 %helper_crash_cleanup_drop_active480, label %helper_crash_cleanup_retire481, label %helper_crash_cleanup_retire_merge482

resource_drop_merge479:                           ; preds = %helper_crash_cleanup_retire_merge482, %resource_drop_merge467
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire481:                   ; preds = %resource_drop_live_only478
  %helper_crash_cleanup_retire_token483 = load i64, ptr %helper_crash_cleanup_token_2, align 8
  %helper_crash_cleanup_retire_call484 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token483)
  br i1 %helper_crash_cleanup_retire_call484, label %helper_crash_cleanup_retire_accepted485, label %helper_crash_cleanup_retire_rejected486

helper_crash_cleanup_retire_merge482:             ; preds = %helper_crash_cleanup_retire_accepted485, %resource_drop_live_only478
  %ref_drop_handle487 = load ptr, ptr %local_2, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle487)
  store ptr null, ptr %local_2, align 8
  br label %resource_drop_merge479

helper_crash_cleanup_retire_accepted485:          ; preds = %helper_crash_cleanup_retire481
  store i64 0, ptr %helper_crash_cleanup_token_2, align 8
  store i1 false, ptr %helper_crash_cleanup_active_2, align 1
  br label %helper_crash_cleanup_retire_merge482

helper_crash_cleanup_retire_rejected486:          ; preds = %helper_crash_cleanup_retire481
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire497:                   ; preds = %bb17
  %helper_crash_cleanup_retire_token499 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_retire_call500 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token499)
  br i1 %helper_crash_cleanup_retire_call500, label %helper_crash_cleanup_retire_accepted501, label %helper_crash_cleanup_retire_rejected502

helper_crash_cleanup_retire_merge498:             ; preds = %helper_crash_cleanup_retire_accepted501, %bb17
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_29)
  %resource_drop_flag503 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed504 = icmp eq i64 %resource_drop_flag503, 0
  br i1 %resource_drop_not_consumed504, label %resource_drop_live_only505, label %resource_drop_merge506

helper_crash_cleanup_retire_accepted501:          ; preds = %helper_crash_cleanup_retire497
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_retire_merge498

helper_crash_cleanup_retire_rejected502:          ; preds = %helper_crash_cleanup_retire497
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only505:                       ; preds = %helper_crash_cleanup_retire_merge498
  %helper_crash_cleanup_drop_active507 = load i1, ptr %helper_crash_cleanup_active_11, align 1
  br i1 %helper_crash_cleanup_drop_active507, label %helper_crash_cleanup_retire508, label %helper_crash_cleanup_retire_merge509

resource_drop_merge506:                           ; preds = %helper_crash_cleanup_retire_merge509, %helper_crash_cleanup_retire_merge498
  %resource_drop_flag515 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed516 = icmp eq i64 %resource_drop_flag515, 0
  br i1 %resource_drop_not_consumed516, label %resource_drop_live_only517, label %resource_drop_merge518

helper_crash_cleanup_retire508:                   ; preds = %resource_drop_live_only505
  %helper_crash_cleanup_retire_token510 = load i64, ptr %helper_crash_cleanup_token_11, align 8
  %helper_crash_cleanup_retire_call511 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token510)
  br i1 %helper_crash_cleanup_retire_call511, label %helper_crash_cleanup_retire_accepted512, label %helper_crash_cleanup_retire_rejected513

helper_crash_cleanup_retire_merge509:             ; preds = %helper_crash_cleanup_retire_accepted512, %resource_drop_live_only505
  %ref_drop_handle514 = load ptr, ptr %local_11, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle514)
  store ptr null, ptr %local_11, align 8
  br label %resource_drop_merge506

helper_crash_cleanup_retire_accepted512:          ; preds = %helper_crash_cleanup_retire508
  store i64 0, ptr %helper_crash_cleanup_token_11, align 8
  store i1 false, ptr %helper_crash_cleanup_active_11, align 1
  br label %helper_crash_cleanup_retire_merge509

helper_crash_cleanup_retire_rejected513:          ; preds = %helper_crash_cleanup_retire508
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only517:                       ; preds = %resource_drop_merge506
  %helper_crash_cleanup_drop_active519 = load i1, ptr %helper_crash_cleanup_active_8, align 1
  br i1 %helper_crash_cleanup_drop_active519, label %helper_crash_cleanup_retire520, label %helper_crash_cleanup_retire_merge521

resource_drop_merge518:                           ; preds = %helper_crash_cleanup_retire_merge521, %resource_drop_merge506
  %resource_drop_flag527 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed528 = icmp eq i64 %resource_drop_flag527, 0
  br i1 %resource_drop_not_consumed528, label %resource_drop_live_only529, label %resource_drop_merge530

helper_crash_cleanup_retire520:                   ; preds = %resource_drop_live_only517
  %helper_crash_cleanup_retire_token522 = load i64, ptr %helper_crash_cleanup_token_8, align 8
  %helper_crash_cleanup_retire_call523 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token522)
  br i1 %helper_crash_cleanup_retire_call523, label %helper_crash_cleanup_retire_accepted524, label %helper_crash_cleanup_retire_rejected525

helper_crash_cleanup_retire_merge521:             ; preds = %helper_crash_cleanup_retire_accepted524, %resource_drop_live_only517
  %ref_drop_handle526 = load ptr, ptr %local_8, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle526)
  store ptr null, ptr %local_8, align 8
  br label %resource_drop_merge518

helper_crash_cleanup_retire_accepted524:          ; preds = %helper_crash_cleanup_retire520
  store i64 0, ptr %helper_crash_cleanup_token_8, align 8
  store i1 false, ptr %helper_crash_cleanup_active_8, align 1
  br label %helper_crash_cleanup_retire_merge521

helper_crash_cleanup_retire_rejected525:          ; preds = %helper_crash_cleanup_retire520
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only529:                       ; preds = %resource_drop_merge518
  %helper_crash_cleanup_drop_active531 = load i1, ptr %helper_crash_cleanup_active_5, align 1
  br i1 %helper_crash_cleanup_drop_active531, label %helper_crash_cleanup_retire532, label %helper_crash_cleanup_retire_merge533

resource_drop_merge530:                           ; preds = %helper_crash_cleanup_retire_merge533, %resource_drop_merge518
  %resource_drop_flag539 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed540 = icmp eq i64 %resource_drop_flag539, 0
  br i1 %resource_drop_not_consumed540, label %resource_drop_live_only541, label %resource_drop_merge542

helper_crash_cleanup_retire532:                   ; preds = %resource_drop_live_only529
  %helper_crash_cleanup_retire_token534 = load i64, ptr %helper_crash_cleanup_token_5, align 8
  %helper_crash_cleanup_retire_call535 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token534)
  br i1 %helper_crash_cleanup_retire_call535, label %helper_crash_cleanup_retire_accepted536, label %helper_crash_cleanup_retire_rejected537

helper_crash_cleanup_retire_merge533:             ; preds = %helper_crash_cleanup_retire_accepted536, %resource_drop_live_only529
  %ref_drop_handle538 = load ptr, ptr %local_5, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle538)
  store ptr null, ptr %local_5, align 8
  br label %resource_drop_merge530

helper_crash_cleanup_retire_accepted536:          ; preds = %helper_crash_cleanup_retire532
  store i64 0, ptr %helper_crash_cleanup_token_5, align 8
  store i1 false, ptr %helper_crash_cleanup_active_5, align 1
  br label %helper_crash_cleanup_retire_merge533

helper_crash_cleanup_retire_rejected537:          ; preds = %helper_crash_cleanup_retire532
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only541:                       ; preds = %resource_drop_merge530
  %helper_crash_cleanup_drop_active543 = load i1, ptr %helper_crash_cleanup_active_2, align 1
  br i1 %helper_crash_cleanup_drop_active543, label %helper_crash_cleanup_retire544, label %helper_crash_cleanup_retire_merge545

resource_drop_merge542:                           ; preds = %helper_crash_cleanup_retire_merge545, %resource_drop_merge530
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire544:                   ; preds = %resource_drop_live_only541
  %helper_crash_cleanup_retire_token546 = load i64, ptr %helper_crash_cleanup_token_2, align 8
  %helper_crash_cleanup_retire_call547 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token546)
  br i1 %helper_crash_cleanup_retire_call547, label %helper_crash_cleanup_retire_accepted548, label %helper_crash_cleanup_retire_rejected549

helper_crash_cleanup_retire_merge545:             ; preds = %helper_crash_cleanup_retire_accepted548, %resource_drop_live_only541
  %ref_drop_handle550 = load ptr, ptr %local_2, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle550)
  store ptr null, ptr %local_2, align 8
  br label %resource_drop_merge542

helper_crash_cleanup_retire_accepted548:          ; preds = %helper_crash_cleanup_retire544
  store i64 0, ptr %helper_crash_cleanup_token_2, align 8
  store i1 false, ptr %helper_crash_cleanup_active_2, align 1
  br label %helper_crash_cleanup_retire_merge545

helper_crash_cleanup_retire_rejected549:          ; preds = %helper_crash_cleanup_retire544
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire560:                   ; preds = %bb19
  %helper_crash_cleanup_retire_token562 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_retire_call563 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token562)
  br i1 %helper_crash_cleanup_retire_call563, label %helper_crash_cleanup_retire_accepted564, label %helper_crash_cleanup_retire_rejected565

helper_crash_cleanup_retire_merge561:             ; preds = %helper_crash_cleanup_retire_accepted564, %bb19
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_29)
  %resource_drop_flag566 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed567 = icmp eq i64 %resource_drop_flag566, 0
  br i1 %resource_drop_not_consumed567, label %resource_drop_live_only568, label %resource_drop_merge569

helper_crash_cleanup_retire_accepted564:          ; preds = %helper_crash_cleanup_retire560
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_retire_merge561

helper_crash_cleanup_retire_rejected565:          ; preds = %helper_crash_cleanup_retire560
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only568:                       ; preds = %helper_crash_cleanup_retire_merge561
  %helper_crash_cleanup_drop_active570 = load i1, ptr %helper_crash_cleanup_active_11, align 1
  br i1 %helper_crash_cleanup_drop_active570, label %helper_crash_cleanup_retire571, label %helper_crash_cleanup_retire_merge572

resource_drop_merge569:                           ; preds = %helper_crash_cleanup_retire_merge572, %helper_crash_cleanup_retire_merge561
  %resource_drop_flag578 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed579 = icmp eq i64 %resource_drop_flag578, 0
  br i1 %resource_drop_not_consumed579, label %resource_drop_live_only580, label %resource_drop_merge581

helper_crash_cleanup_retire571:                   ; preds = %resource_drop_live_only568
  %helper_crash_cleanup_retire_token573 = load i64, ptr %helper_crash_cleanup_token_11, align 8
  %helper_crash_cleanup_retire_call574 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token573)
  br i1 %helper_crash_cleanup_retire_call574, label %helper_crash_cleanup_retire_accepted575, label %helper_crash_cleanup_retire_rejected576

helper_crash_cleanup_retire_merge572:             ; preds = %helper_crash_cleanup_retire_accepted575, %resource_drop_live_only568
  %ref_drop_handle577 = load ptr, ptr %local_11, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle577)
  store ptr null, ptr %local_11, align 8
  br label %resource_drop_merge569

helper_crash_cleanup_retire_accepted575:          ; preds = %helper_crash_cleanup_retire571
  store i64 0, ptr %helper_crash_cleanup_token_11, align 8
  store i1 false, ptr %helper_crash_cleanup_active_11, align 1
  br label %helper_crash_cleanup_retire_merge572

helper_crash_cleanup_retire_rejected576:          ; preds = %helper_crash_cleanup_retire571
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only580:                       ; preds = %resource_drop_merge569
  %helper_crash_cleanup_drop_active582 = load i1, ptr %helper_crash_cleanup_active_8, align 1
  br i1 %helper_crash_cleanup_drop_active582, label %helper_crash_cleanup_retire583, label %helper_crash_cleanup_retire_merge584

resource_drop_merge581:                           ; preds = %helper_crash_cleanup_retire_merge584, %resource_drop_merge569
  %resource_drop_flag590 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed591 = icmp eq i64 %resource_drop_flag590, 0
  br i1 %resource_drop_not_consumed591, label %resource_drop_live_only592, label %resource_drop_merge593

helper_crash_cleanup_retire583:                   ; preds = %resource_drop_live_only580
  %helper_crash_cleanup_retire_token585 = load i64, ptr %helper_crash_cleanup_token_8, align 8
  %helper_crash_cleanup_retire_call586 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token585)
  br i1 %helper_crash_cleanup_retire_call586, label %helper_crash_cleanup_retire_accepted587, label %helper_crash_cleanup_retire_rejected588

helper_crash_cleanup_retire_merge584:             ; preds = %helper_crash_cleanup_retire_accepted587, %resource_drop_live_only580
  %ref_drop_handle589 = load ptr, ptr %local_8, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle589)
  store ptr null, ptr %local_8, align 8
  br label %resource_drop_merge581

helper_crash_cleanup_retire_accepted587:          ; preds = %helper_crash_cleanup_retire583
  store i64 0, ptr %helper_crash_cleanup_token_8, align 8
  store i1 false, ptr %helper_crash_cleanup_active_8, align 1
  br label %helper_crash_cleanup_retire_merge584

helper_crash_cleanup_retire_rejected588:          ; preds = %helper_crash_cleanup_retire583
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only592:                       ; preds = %resource_drop_merge581
  %helper_crash_cleanup_drop_active594 = load i1, ptr %helper_crash_cleanup_active_5, align 1
  br i1 %helper_crash_cleanup_drop_active594, label %helper_crash_cleanup_retire595, label %helper_crash_cleanup_retire_merge596

resource_drop_merge593:                           ; preds = %helper_crash_cleanup_retire_merge596, %resource_drop_merge581
  %resource_drop_flag602 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed603 = icmp eq i64 %resource_drop_flag602, 0
  br i1 %resource_drop_not_consumed603, label %resource_drop_live_only604, label %resource_drop_merge605

helper_crash_cleanup_retire595:                   ; preds = %resource_drop_live_only592
  %helper_crash_cleanup_retire_token597 = load i64, ptr %helper_crash_cleanup_token_5, align 8
  %helper_crash_cleanup_retire_call598 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token597)
  br i1 %helper_crash_cleanup_retire_call598, label %helper_crash_cleanup_retire_accepted599, label %helper_crash_cleanup_retire_rejected600

helper_crash_cleanup_retire_merge596:             ; preds = %helper_crash_cleanup_retire_accepted599, %resource_drop_live_only592
  %ref_drop_handle601 = load ptr, ptr %local_5, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle601)
  store ptr null, ptr %local_5, align 8
  br label %resource_drop_merge593

helper_crash_cleanup_retire_accepted599:          ; preds = %helper_crash_cleanup_retire595
  store i64 0, ptr %helper_crash_cleanup_token_5, align 8
  store i1 false, ptr %helper_crash_cleanup_active_5, align 1
  br label %helper_crash_cleanup_retire_merge596

helper_crash_cleanup_retire_rejected600:          ; preds = %helper_crash_cleanup_retire595
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only604:                       ; preds = %resource_drop_merge593
  %helper_crash_cleanup_drop_active606 = load i1, ptr %helper_crash_cleanup_active_2, align 1
  br i1 %helper_crash_cleanup_drop_active606, label %helper_crash_cleanup_retire607, label %helper_crash_cleanup_retire_merge608

resource_drop_merge605:                           ; preds = %helper_crash_cleanup_retire_merge608, %resource_drop_merge593
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire607:                   ; preds = %resource_drop_live_only604
  %helper_crash_cleanup_retire_token609 = load i64, ptr %helper_crash_cleanup_token_2, align 8
  %helper_crash_cleanup_retire_call610 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token609)
  br i1 %helper_crash_cleanup_retire_call610, label %helper_crash_cleanup_retire_accepted611, label %helper_crash_cleanup_retire_rejected612

helper_crash_cleanup_retire_merge608:             ; preds = %helper_crash_cleanup_retire_accepted611, %resource_drop_live_only604
  %ref_drop_handle613 = load ptr, ptr %local_2, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle613)
  store ptr null, ptr %local_2, align 8
  br label %resource_drop_merge605

helper_crash_cleanup_retire_accepted611:          ; preds = %helper_crash_cleanup_retire607
  store i64 0, ptr %helper_crash_cleanup_token_2, align 8
  store i1 false, ptr %helper_crash_cleanup_active_2, align 1
  br label %helper_crash_cleanup_retire_merge608

helper_crash_cleanup_retire_rejected612:          ; preds = %helper_crash_cleanup_retire607
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

cancel_exit618:                                   ; preds = %bb20
  %helper_crash_cleanup_drop_active620 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_drop_active620, label %helper_crash_cleanup_retire621, label %helper_crash_cleanup_retire_merge622

after_cooperate619:                               ; preds = %bb20
  br label %bb8

helper_crash_cleanup_retire621:                   ; preds = %cancel_exit618
  %helper_crash_cleanup_retire_token623 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_retire_call624 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token623)
  br i1 %helper_crash_cleanup_retire_call624, label %helper_crash_cleanup_retire_accepted625, label %helper_crash_cleanup_retire_rejected626

helper_crash_cleanup_retire_merge622:             ; preds = %helper_crash_cleanup_retire_accepted625, %cancel_exit618
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_29)
  %resource_drop_flag627 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed628 = icmp eq i64 %resource_drop_flag627, 0
  br i1 %resource_drop_not_consumed628, label %resource_drop_live_only629, label %resource_drop_merge630

helper_crash_cleanup_retire_accepted625:          ; preds = %helper_crash_cleanup_retire621
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_retire_merge622

helper_crash_cleanup_retire_rejected626:          ; preds = %helper_crash_cleanup_retire621
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only629:                       ; preds = %helper_crash_cleanup_retire_merge622
  %helper_crash_cleanup_drop_active631 = load i1, ptr %helper_crash_cleanup_active_11, align 1
  br i1 %helper_crash_cleanup_drop_active631, label %helper_crash_cleanup_retire632, label %helper_crash_cleanup_retire_merge633

resource_drop_merge630:                           ; preds = %helper_crash_cleanup_retire_merge633, %helper_crash_cleanup_retire_merge622
  %resource_drop_flag639 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed640 = icmp eq i64 %resource_drop_flag639, 0
  br i1 %resource_drop_not_consumed640, label %resource_drop_live_only641, label %resource_drop_merge642

helper_crash_cleanup_retire632:                   ; preds = %resource_drop_live_only629
  %helper_crash_cleanup_retire_token634 = load i64, ptr %helper_crash_cleanup_token_11, align 8
  %helper_crash_cleanup_retire_call635 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token634)
  br i1 %helper_crash_cleanup_retire_call635, label %helper_crash_cleanup_retire_accepted636, label %helper_crash_cleanup_retire_rejected637

helper_crash_cleanup_retire_merge633:             ; preds = %helper_crash_cleanup_retire_accepted636, %resource_drop_live_only629
  %ref_drop_handle638 = load ptr, ptr %local_11, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle638)
  store ptr null, ptr %local_11, align 8
  br label %resource_drop_merge630

helper_crash_cleanup_retire_accepted636:          ; preds = %helper_crash_cleanup_retire632
  store i64 0, ptr %helper_crash_cleanup_token_11, align 8
  store i1 false, ptr %helper_crash_cleanup_active_11, align 1
  br label %helper_crash_cleanup_retire_merge633

helper_crash_cleanup_retire_rejected637:          ; preds = %helper_crash_cleanup_retire632
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only641:                       ; preds = %resource_drop_merge630
  %helper_crash_cleanup_drop_active643 = load i1, ptr %helper_crash_cleanup_active_8, align 1
  br i1 %helper_crash_cleanup_drop_active643, label %helper_crash_cleanup_retire644, label %helper_crash_cleanup_retire_merge645

resource_drop_merge642:                           ; preds = %helper_crash_cleanup_retire_merge645, %resource_drop_merge630
  %resource_drop_flag651 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed652 = icmp eq i64 %resource_drop_flag651, 0
  br i1 %resource_drop_not_consumed652, label %resource_drop_live_only653, label %resource_drop_merge654

helper_crash_cleanup_retire644:                   ; preds = %resource_drop_live_only641
  %helper_crash_cleanup_retire_token646 = load i64, ptr %helper_crash_cleanup_token_8, align 8
  %helper_crash_cleanup_retire_call647 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token646)
  br i1 %helper_crash_cleanup_retire_call647, label %helper_crash_cleanup_retire_accepted648, label %helper_crash_cleanup_retire_rejected649

helper_crash_cleanup_retire_merge645:             ; preds = %helper_crash_cleanup_retire_accepted648, %resource_drop_live_only641
  %ref_drop_handle650 = load ptr, ptr %local_8, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle650)
  store ptr null, ptr %local_8, align 8
  br label %resource_drop_merge642

helper_crash_cleanup_retire_accepted648:          ; preds = %helper_crash_cleanup_retire644
  store i64 0, ptr %helper_crash_cleanup_token_8, align 8
  store i1 false, ptr %helper_crash_cleanup_active_8, align 1
  br label %helper_crash_cleanup_retire_merge645

helper_crash_cleanup_retire_rejected649:          ; preds = %helper_crash_cleanup_retire644
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only653:                       ; preds = %resource_drop_merge642
  %helper_crash_cleanup_drop_active655 = load i1, ptr %helper_crash_cleanup_active_5, align 1
  br i1 %helper_crash_cleanup_drop_active655, label %helper_crash_cleanup_retire656, label %helper_crash_cleanup_retire_merge657

resource_drop_merge654:                           ; preds = %helper_crash_cleanup_retire_merge657, %resource_drop_merge642
  %resource_drop_flag663 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed664 = icmp eq i64 %resource_drop_flag663, 0
  br i1 %resource_drop_not_consumed664, label %resource_drop_live_only665, label %resource_drop_merge666

helper_crash_cleanup_retire656:                   ; preds = %resource_drop_live_only653
  %helper_crash_cleanup_retire_token658 = load i64, ptr %helper_crash_cleanup_token_5, align 8
  %helper_crash_cleanup_retire_call659 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token658)
  br i1 %helper_crash_cleanup_retire_call659, label %helper_crash_cleanup_retire_accepted660, label %helper_crash_cleanup_retire_rejected661

helper_crash_cleanup_retire_merge657:             ; preds = %helper_crash_cleanup_retire_accepted660, %resource_drop_live_only653
  %ref_drop_handle662 = load ptr, ptr %local_5, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle662)
  store ptr null, ptr %local_5, align 8
  br label %resource_drop_merge654

helper_crash_cleanup_retire_accepted660:          ; preds = %helper_crash_cleanup_retire656
  store i64 0, ptr %helper_crash_cleanup_token_5, align 8
  store i1 false, ptr %helper_crash_cleanup_active_5, align 1
  br label %helper_crash_cleanup_retire_merge657

helper_crash_cleanup_retire_rejected661:          ; preds = %helper_crash_cleanup_retire656
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only665:                       ; preds = %resource_drop_merge654
  %helper_crash_cleanup_drop_active667 = load i1, ptr %helper_crash_cleanup_active_2, align 1
  br i1 %helper_crash_cleanup_drop_active667, label %helper_crash_cleanup_retire668, label %helper_crash_cleanup_retire_merge669

resource_drop_merge666:                           ; preds = %helper_crash_cleanup_retire_merge669, %resource_drop_merge654
  ret i64 0

helper_crash_cleanup_retire668:                   ; preds = %resource_drop_live_only665
  %helper_crash_cleanup_retire_token670 = load i64, ptr %helper_crash_cleanup_token_2, align 8
  %helper_crash_cleanup_retire_call671 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token670)
  br i1 %helper_crash_cleanup_retire_call671, label %helper_crash_cleanup_retire_accepted672, label %helper_crash_cleanup_retire_rejected673

helper_crash_cleanup_retire_merge669:             ; preds = %helper_crash_cleanup_retire_accepted672, %resource_drop_live_only665
  %ref_drop_handle674 = load ptr, ptr %local_2, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle674)
  store ptr null, ptr %local_2, align 8
  br label %resource_drop_merge666

helper_crash_cleanup_retire_accepted672:          ; preds = %helper_crash_cleanup_retire668
  store i64 0, ptr %helper_crash_cleanup_token_2, align 8
  store i1 false, ptr %helper_crash_cleanup_active_2, align 1
  br label %helper_crash_cleanup_retire_merge669

helper_crash_cleanup_retire_rejected673:          ; preds = %helper_crash_cleanup_retire668
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable
}

define internal ptr @expired() {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca i64, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca ptr, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca ptr, align 8
  %helper_crash_cleanup_token_2 = alloca i64, align 8
  store i64 0, ptr %helper_crash_cleanup_token_2, align 8
  %helper_crash_cleanup_active_2 = alloca i1, align 1
  store i1 false, ptr %helper_crash_cleanup_active_2, align 1
  store i64 -1, ptr %local_3, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  store i64 1, ptr %local_0, align 8
  %rc_new = call ptr @hew_rc_new(ptr %local_0, i64 8, i64 8, ptr null)
  store ptr %rc_new, ptr %local_1, align 8
  %helper_crash_cleanup_was_active = load i1, ptr %helper_crash_cleanup_active_2, align 1
  br i1 %helper_crash_cleanup_was_active, label %helper_crash_cleanup_deactivate, label %helper_crash_cleanup_deactivate_merge

helper_crash_cleanup_deactivate:                  ; preds = %bb0
  %helper_crash_cleanup_token = load i64, ptr %helper_crash_cleanup_token_2, align 8
  %helper_crash_cleanup_deactivate_call = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token)
  br i1 %helper_crash_cleanup_deactivate_call, label %helper_crash_cleanup_deactivate_accepted, label %helper_crash_cleanup_deactivate_rejected

helper_crash_cleanup_deactivate_merge:            ; preds = %helper_crash_cleanup_deactivate_accepted, %bb0
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %local_2, align 8
  store i64 0, ptr %local_3, align 8
  %helper_crash_cleanup_arm_guard = load i64, ptr %local_3, align 8
  %helper_crash_cleanup_arm_guard_live = icmp eq i64 %helper_crash_cleanup_arm_guard, 0
  br i1 %helper_crash_cleanup_arm_guard_live, label %helper_crash_cleanup_guard_live, label %helper_crash_cleanup_guard_merge

helper_crash_cleanup_deactivate_accepted:         ; preds = %helper_crash_cleanup_deactivate
  store i1 false, ptr %helper_crash_cleanup_active_2, align 1
  br label %helper_crash_cleanup_deactivate_merge

helper_crash_cleanup_deactivate_rejected:         ; preds = %helper_crash_cleanup_deactivate
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_guard_live:                  ; preds = %helper_crash_cleanup_deactivate_merge
  %helper_crash_cleanup_prior_token = load i64, ptr %helper_crash_cleanup_token_2, align 8
  %arm_typed_crash_cleanup = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token, ptr %local_2, i64 8, i64 8, ptr @__hew_frame_cleanup_445350923e96a538, i32 1, i32 0)
  %frame_cleanup_arm_failed = icmp eq i64 %arm_typed_crash_cleanup, -1
  br i1 %frame_cleanup_arm_failed, label %frame_cleanup_rejected, label %frame_cleanup_registered

helper_crash_cleanup_guard_merge:                 ; preds = %frame_cleanup_registered, %helper_crash_cleanup_deactivate_merge
  %rc_borrow_handle = load ptr, ptr %local_2, align 8
  %rc_handle_result = call ptr @hew_rc_downgrade(ptr %rc_borrow_handle)
  store ptr %rc_handle_result, ptr %local_4, align 8
  %move_load1 = load ptr, ptr %local_4, align 8
  store ptr %move_load1, ptr %return_slot, align 8
  %resource_drop_flag = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed = icmp eq i64 %resource_drop_flag, 0
  br i1 %resource_drop_not_consumed, label %resource_drop_live_only, label %resource_drop_merge

frame_cleanup_registered:                         ; preds = %helper_crash_cleanup_guard_live
  store i64 %arm_typed_crash_cleanup, ptr %helper_crash_cleanup_token_2, align 8
  store i1 true, ptr %helper_crash_cleanup_active_2, align 1
  br label %helper_crash_cleanup_guard_merge

frame_cleanup_rejected:                           ; preds = %helper_crash_cleanup_guard_live
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only:                          ; preds = %helper_crash_cleanup_guard_merge
  %helper_crash_cleanup_drop_active = load i1, ptr %helper_crash_cleanup_active_2, align 1
  br i1 %helper_crash_cleanup_drop_active, label %helper_crash_cleanup_retire, label %helper_crash_cleanup_retire_merge

resource_drop_merge:                              ; preds = %helper_crash_cleanup_retire_merge, %helper_crash_cleanup_guard_merge
  %helper_crash_cleanup_return_token_2 = load i64, ptr %helper_crash_cleanup_token_2, align 8
  %helper_crash_cleanup_return_has_token_2 = icmp ne i64 %helper_crash_cleanup_return_token_2, 0
  br i1 %helper_crash_cleanup_return_has_token_2, label %helper_crash_cleanup_return_retire_2, label %helper_crash_cleanup_return_merge_2

helper_crash_cleanup_retire:                      ; preds = %resource_drop_live_only
  %helper_crash_cleanup_retire_token = load i64, ptr %helper_crash_cleanup_token_2, align 8
  %helper_crash_cleanup_retire_call = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token)
  br i1 %helper_crash_cleanup_retire_call, label %helper_crash_cleanup_retire_accepted, label %helper_crash_cleanup_retire_rejected

helper_crash_cleanup_retire_merge:                ; preds = %helper_crash_cleanup_retire_accepted, %resource_drop_live_only
  %ref_drop_handle = load ptr, ptr %local_2, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle)
  store ptr null, ptr %local_2, align 8
  br label %resource_drop_merge

helper_crash_cleanup_retire_accepted:             ; preds = %helper_crash_cleanup_retire
  store i64 0, ptr %helper_crash_cleanup_token_2, align 8
  store i1 false, ptr %helper_crash_cleanup_active_2, align 1
  br label %helper_crash_cleanup_retire_merge

helper_crash_cleanup_retire_rejected:             ; preds = %helper_crash_cleanup_retire
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_return_merge_2:              ; preds = %helper_crash_cleanup_return_retire_2_accepted, %resource_drop_merge
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

helper_crash_cleanup_return_retire_2:             ; preds = %resource_drop_merge
  %helper_crash_cleanup_return_retire_2_call = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_return_token_2)
  br i1 %helper_crash_cleanup_return_retire_2_call, label %helper_crash_cleanup_return_retire_2_accepted, label %helper_crash_cleanup_return_retire_2_rejected

helper_crash_cleanup_return_retire_2_accepted:    ; preds = %helper_crash_cleanup_return_retire_2
  store i64 0, ptr %helper_crash_cleanup_token_2, align 8
  store i1 false, ptr %helper_crash_cleanup_active_2, align 1
  br label %helper_crash_cleanup_return_merge_2

helper_crash_cleanup_return_retire_2_rejected:    ; preds = %helper_crash_cleanup_return_retire_2
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
  %local_4 = alloca %"Option$$Rc$li64$g", align 8
  %local_5 = alloca i64, align 8
  %local_6 = alloca i64, align 8
  %local_7 = alloca i8, align 1
  %local_8 = alloca i64, align 8
  %local_9 = alloca i8, align 1
  %local_10 = alloca ptr, align 8
  %local_11 = alloca i64, align 8
  %local_12 = alloca i64, align 8
  %local_13 = alloca i64, align 8
  %local_14 = alloca i64, align 8
  %local_15 = alloca i64, align 8
  %local_16 = alloca i8, align 1
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
  %call_result = call ptr @expired()
  store ptr %call_result, ptr %local_0, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %helper_crash_cleanup_was_active = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_was_active, label %helper_crash_cleanup_deactivate, label %helper_crash_cleanup_deactivate_merge

bb2:                                              ; preds = %after_cooperate60, %after_cooperate37
  %helper_crash_cleanup_drop_active = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active, label %helper_crash_cleanup_retire, label %helper_crash_cleanup_retire_merge

bb3:                                              ; preds = %frame_cleanup_registered11
  %machine_payload_ptr16 = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %local_4, i32 0, i32 1
  %machine_variant_field_ptr17 = getelementptr inbounds nuw { ptr }, ptr %machine_payload_ptr16, i32 0, i32 0
  %move_load18 = load ptr, ptr %machine_variant_field_ptr17, align 8
  store ptr %move_load18, ptr %local_10, align 8
  %helper_crash_cleanup_was_active19 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_was_active19, label %helper_crash_cleanup_deactivate20, label %helper_crash_cleanup_deactivate_merge21

bb4:                                              ; preds = %bb6
  store i64 1, ptr %local_12, align 8
  %move_load56 = load i64, ptr %local_12, align 8
  store i64 %move_load56, ptr %local_3, align 8
  %hew_actor_cooperate57 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel58 = icmp eq i32 %hew_actor_cooperate57, 2
  br i1 %hew_cooperate_is_cancel58, label %cancel_exit59, label %after_cooperate60

bb5:                                              ; preds = %bb6
  %helper_crash_cleanup_drop_active84 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active84, label %helper_crash_cleanup_retire85, label %helper_crash_cleanup_retire_merge86

bb6:                                              ; preds = %frame_cleanup_registered11
  store i64 1, ptr %local_8, align 8
  %cmp_lhs103 = load i64, ptr %local_5, align 8
  %cmp_rhs104 = load i64, ptr %local_8, align 8
  %cmp_bit105 = icmp eq i64 %cmp_lhs103, %cmp_rhs104
  %cmp_zext106 = zext i1 %cmp_bit105 to i8
  store i8 %cmp_zext106, ptr %local_9, align 1
  %cond_load107 = load i8, ptr %local_9, align 1
  %cond_nz108 = icmp ne i8 %cond_load107, 0
  br i1 %cond_nz108, label %bb4, label %bb5

bb7:                                              ; preds = %helper_crash_cleanup_retire_merge
  %checked_lhs = load i64, ptr %local_14, align 8
  %checked_rhs = load i64, ptr %local_13, align 8
  %with_overflow = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_15, align 8
  store i8 %checked_overflow_widen, ptr %local_16, align 1
  %cond_load109 = load i8, ptr %local_16, align 1
  %cond_nz110 = icmp ne i8 %cond_load109, 0
  br i1 %cond_nz110, label %bb8, label %bb9

bb8:                                              ; preds = %bb7
  %helper_crash_cleanup_drop_active111 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active111, label %helper_crash_cleanup_retire112, label %helper_crash_cleanup_retire_merge113

bb9:                                              ; preds = %bb7
  %move_load130 = load i64, ptr %local_15, align 8
  store i64 %move_load130, ptr %return_slot, align 8
  %helper_crash_cleanup_drop_active131 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active131, label %helper_crash_cleanup_retire132, label %helper_crash_cleanup_retire_merge133

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
  %arm_typed_crash_cleanup = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token, ptr %local_1, i64 8, i64 8, ptr @__hew_frame_cleanup_7b793acd5045b9cb, i32 1, i32 0)
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
  %machine_tag_ptr = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %local_4, i32 0, i32 0
  %machine_payload_ptr = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %local_4, i32 0, i32 1
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
  %arm_typed_crash_cleanup9 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token8, ptr %local_4, i64 16, i64 8, ptr @__hew_frame_cleanup_45375aadfceee3dc, i32 1, i32 0)
  %frame_cleanup_arm_failed10 = icmp eq i64 %arm_typed_crash_cleanup9, -1
  br i1 %frame_cleanup_arm_failed10, label %frame_cleanup_rejected12, label %frame_cleanup_registered11

frame_cleanup_registered11:                       ; preds = %weak_upgrade_cont
  store i64 %arm_typed_crash_cleanup9, ptr %helper_crash_cleanup_token_4, align 8
  store i1 true, ptr %helper_crash_cleanup_active_4, align 1
  %machine_tag_ptr13 = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %local_4, i32 0, i32 0
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
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_4)
  store %"Option$$Rc$li64$g" zeroinitializer, ptr %local_4, align 8
  %move_load14 = load i64, ptr %local_3, align 8
  store i64 %move_load14, ptr %local_13, align 8
  %call_result15 = call i64 @live()
  store i64 %call_result15, ptr %local_14, align 8
  br label %bb7

helper_crash_cleanup_retire_accepted:             ; preds = %helper_crash_cleanup_retire
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge

helper_crash_cleanup_retire_rejected:             ; preds = %helper_crash_cleanup_retire
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_deactivate20:                ; preds = %bb3
  %helper_crash_cleanup_token22 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_deactivate_call23 = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token22)
  br i1 %helper_crash_cleanup_deactivate_call23, label %helper_crash_cleanup_deactivate_accepted24, label %helper_crash_cleanup_deactivate_rejected25

helper_crash_cleanup_deactivate_merge21:          ; preds = %helper_crash_cleanup_deactivate_accepted24, %bb3
  %machine_payload_ptr26 = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %local_4, i32 0, i32 1
  %machine_variant_field_ptr27 = getelementptr inbounds nuw { ptr }, ptr %machine_payload_ptr26, i32 0, i32 0
  store ptr null, ptr %machine_variant_field_ptr27, align 8
  %helper_crash_cleanup_prior_token28 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %arm_typed_crash_cleanup29 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token28, ptr %local_4, i64 16, i64 8, ptr @__hew_frame_cleanup_45375aadfceee3dc, i32 1, i32 0)
  %frame_cleanup_arm_failed30 = icmp eq i64 %arm_typed_crash_cleanup29, -1
  br i1 %frame_cleanup_arm_failed30, label %frame_cleanup_rejected32, label %frame_cleanup_registered31

helper_crash_cleanup_deactivate_accepted24:       ; preds = %helper_crash_cleanup_deactivate20
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_deactivate_merge21

helper_crash_cleanup_deactivate_rejected25:       ; preds = %helper_crash_cleanup_deactivate20
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

frame_cleanup_registered31:                       ; preds = %helper_crash_cleanup_deactivate_merge21
  store i64 %arm_typed_crash_cleanup29, ptr %helper_crash_cleanup_token_4, align 8
  store i1 true, ptr %helper_crash_cleanup_active_4, align 1
  store i64 9000, ptr %local_11, align 8
  %move_load33 = load i64, ptr %local_11, align 8
  store i64 %move_load33, ptr %local_3, align 8
  %"hew_rc_drop drop" = load ptr, ptr %local_10, align 8
  call void @hew_rc_drop(ptr %"hew_rc_drop drop")
  store ptr null, ptr %local_10, align 8
  %hew_actor_cooperate34 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel35 = icmp eq i32 %hew_actor_cooperate34, 2
  br i1 %hew_cooperate_is_cancel35, label %cancel_exit36, label %after_cooperate37

frame_cleanup_rejected32:                         ; preds = %helper_crash_cleanup_deactivate_merge21
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

cancel_exit36:                                    ; preds = %frame_cleanup_registered31
  %helper_crash_cleanup_drop_active38 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active38, label %helper_crash_cleanup_retire39, label %helper_crash_cleanup_retire_merge40

after_cooperate37:                                ; preds = %frame_cleanup_registered31
  br label %bb2

helper_crash_cleanup_retire39:                    ; preds = %cancel_exit36
  %helper_crash_cleanup_retire_token41 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call42 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token41)
  br i1 %helper_crash_cleanup_retire_call42, label %helper_crash_cleanup_retire_accepted43, label %helper_crash_cleanup_retire_rejected44

helper_crash_cleanup_retire_merge40:              ; preds = %helper_crash_cleanup_retire_accepted43, %cancel_exit36
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_4)
  %resource_drop_flag = load i64, ptr %local_2, align 8
  %resource_drop_not_consumed = icmp eq i64 %resource_drop_flag, 0
  br i1 %resource_drop_not_consumed, label %resource_drop_live_only, label %resource_drop_merge

helper_crash_cleanup_retire_accepted43:           ; preds = %helper_crash_cleanup_retire39
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge40

helper_crash_cleanup_retire_rejected44:           ; preds = %helper_crash_cleanup_retire39
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only:                          ; preds = %helper_crash_cleanup_retire_merge40
  %helper_crash_cleanup_drop_active45 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active45, label %helper_crash_cleanup_retire46, label %helper_crash_cleanup_retire_merge47

resource_drop_merge:                              ; preds = %helper_crash_cleanup_retire_merge47, %helper_crash_cleanup_retire_merge40
  %hew_runtime_exit_status_call52 = call i32 @hew_runtime_exit_status()
  %hew_runtime_faulted53 = icmp ne i32 %hew_runtime_exit_status_call52, 0
  br i1 %hew_runtime_faulted53, label %hew_exit_status_failed54, label %hew_exit_status_continue55

helper_crash_cleanup_retire46:                    ; preds = %resource_drop_live_only
  %helper_crash_cleanup_retire_token48 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call49 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token48)
  br i1 %helper_crash_cleanup_retire_call49, label %helper_crash_cleanup_retire_accepted50, label %helper_crash_cleanup_retire_rejected51

helper_crash_cleanup_retire_merge47:              ; preds = %helper_crash_cleanup_retire_accepted50, %resource_drop_live_only
  %ref_drop_handle = load ptr, ptr %local_1, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle)
  store ptr null, ptr %local_1, align 8
  br label %resource_drop_merge

helper_crash_cleanup_retire_accepted50:           ; preds = %helper_crash_cleanup_retire46
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge47

helper_crash_cleanup_retire_rejected51:           ; preds = %helper_crash_cleanup_retire46
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

hew_exit_status_failed54:                         ; preds = %resource_drop_merge
  call void @hew_exit(i64 1)
  br label %hew_exit_status_continue55

hew_exit_status_continue55:                       ; preds = %hew_exit_status_failed54, %resource_drop_merge
  ret i64 0

cancel_exit59:                                    ; preds = %bb4
  %helper_crash_cleanup_drop_active61 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active61, label %helper_crash_cleanup_retire62, label %helper_crash_cleanup_retire_merge63

after_cooperate60:                                ; preds = %bb4
  br label %bb2

helper_crash_cleanup_retire62:                    ; preds = %cancel_exit59
  %helper_crash_cleanup_retire_token64 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call65 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token64)
  br i1 %helper_crash_cleanup_retire_call65, label %helper_crash_cleanup_retire_accepted66, label %helper_crash_cleanup_retire_rejected67

helper_crash_cleanup_retire_merge63:              ; preds = %helper_crash_cleanup_retire_accepted66, %cancel_exit59
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_4)
  %resource_drop_flag68 = load i64, ptr %local_2, align 8
  %resource_drop_not_consumed69 = icmp eq i64 %resource_drop_flag68, 0
  br i1 %resource_drop_not_consumed69, label %resource_drop_live_only70, label %resource_drop_merge71

helper_crash_cleanup_retire_accepted66:           ; preds = %helper_crash_cleanup_retire62
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge63

helper_crash_cleanup_retire_rejected67:           ; preds = %helper_crash_cleanup_retire62
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only70:                        ; preds = %helper_crash_cleanup_retire_merge63
  %helper_crash_cleanup_drop_active72 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active72, label %helper_crash_cleanup_retire73, label %helper_crash_cleanup_retire_merge74

resource_drop_merge71:                            ; preds = %helper_crash_cleanup_retire_merge74, %helper_crash_cleanup_retire_merge63
  %hew_runtime_exit_status_call80 = call i32 @hew_runtime_exit_status()
  %hew_runtime_faulted81 = icmp ne i32 %hew_runtime_exit_status_call80, 0
  br i1 %hew_runtime_faulted81, label %hew_exit_status_failed82, label %hew_exit_status_continue83

helper_crash_cleanup_retire73:                    ; preds = %resource_drop_live_only70
  %helper_crash_cleanup_retire_token75 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call76 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token75)
  br i1 %helper_crash_cleanup_retire_call76, label %helper_crash_cleanup_retire_accepted77, label %helper_crash_cleanup_retire_rejected78

helper_crash_cleanup_retire_merge74:              ; preds = %helper_crash_cleanup_retire_accepted77, %resource_drop_live_only70
  %ref_drop_handle79 = load ptr, ptr %local_1, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle79)
  store ptr null, ptr %local_1, align 8
  br label %resource_drop_merge71

helper_crash_cleanup_retire_accepted77:           ; preds = %helper_crash_cleanup_retire73
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge74

helper_crash_cleanup_retire_rejected78:           ; preds = %helper_crash_cleanup_retire73
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

hew_exit_status_failed82:                         ; preds = %resource_drop_merge71
  call void @hew_exit(i64 1)
  br label %hew_exit_status_continue83

hew_exit_status_continue83:                       ; preds = %hew_exit_status_failed82, %resource_drop_merge71
  ret i64 0

helper_crash_cleanup_retire85:                    ; preds = %bb5
  %helper_crash_cleanup_retire_token87 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call88 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token87)
  br i1 %helper_crash_cleanup_retire_call88, label %helper_crash_cleanup_retire_accepted89, label %helper_crash_cleanup_retire_rejected90

helper_crash_cleanup_retire_merge86:              ; preds = %helper_crash_cleanup_retire_accepted89, %bb5
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_4)
  %resource_drop_flag91 = load i64, ptr %local_2, align 8
  %resource_drop_not_consumed92 = icmp eq i64 %resource_drop_flag91, 0
  br i1 %resource_drop_not_consumed92, label %resource_drop_live_only93, label %resource_drop_merge94

helper_crash_cleanup_retire_accepted89:           ; preds = %helper_crash_cleanup_retire85
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge86

helper_crash_cleanup_retire_rejected90:           ; preds = %helper_crash_cleanup_retire85
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only93:                        ; preds = %helper_crash_cleanup_retire_merge86
  %helper_crash_cleanup_drop_active95 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active95, label %helper_crash_cleanup_retire96, label %helper_crash_cleanup_retire_merge97

resource_drop_merge94:                            ; preds = %helper_crash_cleanup_retire_merge97, %helper_crash_cleanup_retire_merge86
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire96:                    ; preds = %resource_drop_live_only93
  %helper_crash_cleanup_retire_token98 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call99 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token98)
  br i1 %helper_crash_cleanup_retire_call99, label %helper_crash_cleanup_retire_accepted100, label %helper_crash_cleanup_retire_rejected101

helper_crash_cleanup_retire_merge97:              ; preds = %helper_crash_cleanup_retire_accepted100, %resource_drop_live_only93
  %ref_drop_handle102 = load ptr, ptr %local_1, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle102)
  store ptr null, ptr %local_1, align 8
  br label %resource_drop_merge94

helper_crash_cleanup_retire_accepted100:          ; preds = %helper_crash_cleanup_retire96
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge97

helper_crash_cleanup_retire_rejected101:          ; preds = %helper_crash_cleanup_retire96
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire112:                   ; preds = %bb8
  %helper_crash_cleanup_retire_token114 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call115 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token114)
  br i1 %helper_crash_cleanup_retire_call115, label %helper_crash_cleanup_retire_accepted116, label %helper_crash_cleanup_retire_rejected117

helper_crash_cleanup_retire_merge113:             ; preds = %helper_crash_cleanup_retire_accepted116, %bb8
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_4)
  %resource_drop_flag118 = load i64, ptr %local_2, align 8
  %resource_drop_not_consumed119 = icmp eq i64 %resource_drop_flag118, 0
  br i1 %resource_drop_not_consumed119, label %resource_drop_live_only120, label %resource_drop_merge121

helper_crash_cleanup_retire_accepted116:          ; preds = %helper_crash_cleanup_retire112
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge113

helper_crash_cleanup_retire_rejected117:          ; preds = %helper_crash_cleanup_retire112
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only120:                       ; preds = %helper_crash_cleanup_retire_merge113
  %helper_crash_cleanup_drop_active122 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active122, label %helper_crash_cleanup_retire123, label %helper_crash_cleanup_retire_merge124

resource_drop_merge121:                           ; preds = %helper_crash_cleanup_retire_merge124, %helper_crash_cleanup_retire_merge113
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire123:                   ; preds = %resource_drop_live_only120
  %helper_crash_cleanup_retire_token125 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call126 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token125)
  br i1 %helper_crash_cleanup_retire_call126, label %helper_crash_cleanup_retire_accepted127, label %helper_crash_cleanup_retire_rejected128

helper_crash_cleanup_retire_merge124:             ; preds = %helper_crash_cleanup_retire_accepted127, %resource_drop_live_only120
  %ref_drop_handle129 = load ptr, ptr %local_1, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle129)
  store ptr null, ptr %local_1, align 8
  br label %resource_drop_merge121

helper_crash_cleanup_retire_accepted127:          ; preds = %helper_crash_cleanup_retire123
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge124

helper_crash_cleanup_retire_rejected128:          ; preds = %helper_crash_cleanup_retire123
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire132:                   ; preds = %bb9
  %helper_crash_cleanup_retire_token134 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call135 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token134)
  br i1 %helper_crash_cleanup_retire_call135, label %helper_crash_cleanup_retire_accepted136, label %helper_crash_cleanup_retire_rejected137

helper_crash_cleanup_retire_merge133:             ; preds = %helper_crash_cleanup_retire_accepted136, %bb9
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_4)
  %resource_drop_flag138 = load i64, ptr %local_2, align 8
  %resource_drop_not_consumed139 = icmp eq i64 %resource_drop_flag138, 0
  br i1 %resource_drop_not_consumed139, label %resource_drop_live_only140, label %resource_drop_merge141

helper_crash_cleanup_retire_accepted136:          ; preds = %helper_crash_cleanup_retire132
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge133

helper_crash_cleanup_retire_rejected137:          ; preds = %helper_crash_cleanup_retire132
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only140:                       ; preds = %helper_crash_cleanup_retire_merge133
  %helper_crash_cleanup_drop_active142 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active142, label %helper_crash_cleanup_retire143, label %helper_crash_cleanup_retire_merge144

resource_drop_merge141:                           ; preds = %helper_crash_cleanup_retire_merge144, %helper_crash_cleanup_retire_merge133
  %helper_crash_cleanup_return_token_1 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_return_has_token_1 = icmp ne i64 %helper_crash_cleanup_return_token_1, 0
  br i1 %helper_crash_cleanup_return_has_token_1, label %helper_crash_cleanup_return_retire_1, label %helper_crash_cleanup_return_merge_1

helper_crash_cleanup_retire143:                   ; preds = %resource_drop_live_only140
  %helper_crash_cleanup_retire_token145 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call146 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token145)
  br i1 %helper_crash_cleanup_retire_call146, label %helper_crash_cleanup_retire_accepted147, label %helper_crash_cleanup_retire_rejected148

helper_crash_cleanup_retire_merge144:             ; preds = %helper_crash_cleanup_retire_accepted147, %resource_drop_live_only140
  %ref_drop_handle149 = load ptr, ptr %local_1, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle149)
  store ptr null, ptr %local_1, align 8
  br label %resource_drop_merge141

helper_crash_cleanup_retire_accepted147:          ; preds = %helper_crash_cleanup_retire143
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge144

helper_crash_cleanup_retire_rejected148:          ; preds = %helper_crash_cleanup_retire143
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_return_merge_1:              ; preds = %helper_crash_cleanup_return_retire_1_accepted, %resource_drop_merge141
  %helper_crash_cleanup_return_token_4 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_return_has_token_4 = icmp ne i64 %helper_crash_cleanup_return_token_4, 0
  br i1 %helper_crash_cleanup_return_has_token_4, label %helper_crash_cleanup_return_retire_4, label %helper_crash_cleanup_return_merge_4

helper_crash_cleanup_return_retire_1:             ; preds = %resource_drop_merge141
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
  %hew_runtime_exit_status_call150 = call i32 @hew_runtime_exit_status()
  %hew_runtime_faulted151 = icmp ne i32 %hew_runtime_exit_status_call150, 0
  %hew_exit_any_failed = or i1 %hew_lambda_drain_failed, %hew_runtime_faulted151
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
  store ptr @str_lit, ptr %local_3, align 8
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

define internal i32 @"__hew_enum_clone_inplace_Option$$Rc$li64$g"(ptr %0, ptr %1) {
entry:
  %enum_clone_tag_ptr = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %0, i32 0, i32 0
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
  %enum_clone_src_payload_0 = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %0, i32 0, i32 1
  %enum_clone_dst_payload_0 = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %1, i32 0, i32 1
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

define internal void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %0) {
entry:
  %enum_drop_tag_ptr = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %0, i32 0, i32 0
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
  %enum_drop_payload_0 = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %0, i32 0, i32 1
  %drop_rc_f0_ptr = getelementptr inbounds nuw { ptr }, ptr %enum_drop_payload_0, i32 0, i32 0
  %drop_rc_f0 = load ptr, ptr %drop_rc_f0_ptr, align 8
  call void @hew_rc_drop(ptr %drop_rc_f0)
  store ptr null, ptr %drop_rc_f0_ptr, align 8
  br label %done

enum_drop_variant_1:                              ; preds = %entry
  %enum_drop_payload_1 = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %0, i32 0, i32 1
  br label %done
}

declare void @hew_rc_drop(ptr)

define internal void @"__hew_enum_overwrite_release_Option$$Rc$li64$g"(ptr %0, ptr %1) {
entry:
  %ow_slot_0 = alloca ptr, align 8
  store ptr null, ptr %ow_slot_0, align 8
  %"ow_new_d0_Option$$Rc$li64$g_tag_ptr" = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %1, i32 0, i32 0
  %"ow_new_d0_Option$$Rc$li64$g_tag" = load i8, ptr %"ow_new_d0_Option$$Rc$li64$g_tag_ptr", align 1
  switch i8 %"ow_new_d0_Option$$Rc$li64$g_tag", label %"ow_new_d0_Option$$Rc$li64$g_tag_oob" [
    i8 0, label %"ow_new_d0_Option$$Rc$li64$g_v0"
    i8 1, label %"ow_new_d0_Option$$Rc$li64$g_v1"
  ]

"ow_new_d0_Option$$Rc$li64$g_merge":              ; preds = %"ow_new_d0_Option$$Rc$li64$g_v1", %"ow_new_d0_Option$$Rc$li64$g_v0"
  %"ow_old_d0_Option$$Rc$li64$g_tag_ptr" = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %0, i32 0, i32 0
  %"ow_old_d0_Option$$Rc$li64$g_tag" = load i8, ptr %"ow_old_d0_Option$$Rc$li64$g_tag_ptr", align 1
  switch i8 %"ow_old_d0_Option$$Rc$li64$g_tag", label %"ow_old_d0_Option$$Rc$li64$g_tag_oob" [
    i8 0, label %"ow_old_d0_Option$$Rc$li64$g_v0"
    i8 1, label %"ow_old_d0_Option$$Rc$li64$g_v1"
  ]

"ow_new_d0_Option$$Rc$li64$g_tag_oob":            ; preds = %entry
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

"ow_new_d0_Option$$Rc$li64$g_v0":                 ; preds = %entry
  %"ow_new_d0_Option$$Rc$li64$g_v0_payload" = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %1, i32 0, i32 1
  %ow_new_d0_f0_ptr = getelementptr inbounds nuw { ptr }, ptr %"ow_new_d0_Option$$Rc$li64$g_v0_payload", i32 0, i32 0
  %ow_new_d0_f0_leaf = load ptr, ptr %ow_new_d0_f0_ptr, align 8
  store ptr %ow_new_d0_f0_leaf, ptr %ow_slot_0, align 8
  br label %"ow_new_d0_Option$$Rc$li64$g_merge"

"ow_new_d0_Option$$Rc$li64$g_v1":                 ; preds = %entry
  %"ow_new_d0_Option$$Rc$li64$g_v1_payload" = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %1, i32 0, i32 1
  br label %"ow_new_d0_Option$$Rc$li64$g_merge"

"ow_old_d0_Option$$Rc$li64$g_merge":              ; preds = %"ow_old_d0_Option$$Rc$li64$g_v1", %"ow_old_d0_Option$$Rc$li64$g_v0"
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %0)
  ret void

"ow_old_d0_Option$$Rc$li64$g_tag_oob":            ; preds = %"ow_new_d0_Option$$Rc$li64$g_merge"
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

"ow_old_d0_Option$$Rc$li64$g_v0":                 ; preds = %"ow_new_d0_Option$$Rc$li64$g_merge"
  %"ow_old_d0_Option$$Rc$li64$g_v0_payload" = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %0, i32 0, i32 1
  %ow_old_d0_f0_ptr = getelementptr inbounds nuw { ptr }, ptr %"ow_old_d0_Option$$Rc$li64$g_v0_payload", i32 0, i32 0
  %ow_old_d0_f0_val = load ptr, ptr %ow_old_d0_f0_ptr, align 8
  %ow_old_d0_f0_int = ptrtoint ptr %ow_old_d0_f0_val to i64
  %ow_old_d0_f0_cmp0_leaf = load ptr, ptr %ow_slot_0, align 8
  %ow_old_d0_f0_cmp0_int = ptrtoint ptr %ow_old_d0_f0_cmp0_leaf to i64
  %ow_old_d0_f0_cmp0_eq = icmp eq i64 %ow_old_d0_f0_int, %ow_old_d0_f0_cmp0_int
  %ow_old_d0_f0_matched0 = or i1 false, %ow_old_d0_f0_cmp0_eq
  %ow_old_d0_f0_neutralized = select i1 %ow_old_d0_f0_matched0, ptr null, ptr %ow_old_d0_f0_val
  store ptr %ow_old_d0_f0_neutralized, ptr %ow_old_d0_f0_ptr, align 8
  br label %"ow_old_d0_Option$$Rc$li64$g_merge"

"ow_old_d0_Option$$Rc$li64$g_v1":                 ; preds = %"ow_new_d0_Option$$Rc$li64$g_merge"
  %"ow_old_d0_Option$$Rc$li64$g_v1_payload" = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %0, i32 0, i32 1
  br label %"ow_old_d0_Option$$Rc$li64$g_merge"
}

declare i32 @hew_actor_cooperate()

declare ptr @hew_rc_new(ptr, i64, i64, ptr)

declare i1 @hew_cont_crash_cleanup_deactivate(i64)

define internal void @__hew_frame_cleanup_445350923e96a538(ptr %0) {
entry:
  %ref_drop_handle = load ptr, ptr %0, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle)
  store ptr null, ptr %0, align 8
  ret void
}

declare i64 @hew_cont_crash_cleanup_arm(i64, ptr, i64, i64, ptr, i32, i32)

declare ptr @hew_rc_downgrade(ptr)

define internal void @__hew_frame_cleanup_7b793acd5045b9cb(ptr %0) {
entry:
  %ref_drop_handle = load ptr, ptr %0, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle)
  store ptr null, ptr %0, align 8
  ret void
}

declare void @hew_weak_drop_rc(ptr)

declare ptr @hew_weak_clone_rc(ptr)

declare i64 @hew_rc_strong_count(ptr)

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.smul.with.overflow.i64(i64, i64) #1

declare i1 @hew_cont_crash_cleanup_retire(i64)

declare i64 @hew_rc_weak_count(ptr)

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64) #1

declare i32 @hew_rc_is_unique(ptr)

declare ptr @hew_weak_upgrade_rc(ptr)

define internal void @__hew_frame_cleanup_45375aadfceee3dc(ptr %0) {
entry:
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %0)
  ret void
}

declare void @hew_rc_set(ptr, ptr)

declare ptr @hew_rc_get(ptr)

declare i32 @hew_runtime_exit_status()

declare i32 @hew_lambda_drain_all(i64)

declare void @hew_string_drop(ptr)

attributes #0 = { cold noreturn nounwind memory(inaccessiblemem: write) }
attributes #1 = { nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none) }
