; ModuleID = 'rc_weak_lifecycle'
source_filename = "rc_weak_lifecycle"
target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

%"Option$$Rc$li64$g" = type { i8, [1 x i32] }

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

declare void @hew_string_to_bytes(ptr noalias sret({ ptr, i32, i32 }), ptr)

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
  %local_1 = alloca ptr, align 4
  %local_2 = alloca ptr, align 4
  %local_3 = alloca i64, align 8
  %local_4 = alloca ptr, align 4
  %local_5 = alloca ptr, align 4
  %local_6 = alloca i64, align 8
  %local_7 = alloca ptr, align 4
  %local_8 = alloca ptr, align 4
  %local_9 = alloca i64, align 8
  %local_10 = alloca ptr, align 4
  %local_11 = alloca ptr, align 4
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
  %local_35 = alloca ptr, align 4
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
  %rc_new = call ptr @hew_rc_new(ptr %local_0, i32 8, i32 8, ptr null)
  store ptr %rc_new, ptr %local_1, align 4
  %helper_crash_cleanup_was_active = load i1, ptr %helper_crash_cleanup_active_2, align 1
  br i1 %helper_crash_cleanup_was_active, label %helper_crash_cleanup_deactivate, label %helper_crash_cleanup_deactivate_merge

bb1:                                              ; preds = %helper_crash_cleanup_guard_merge50
  %resource_drop_flag = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed = icmp eq i64 %resource_drop_flag, 0
  br i1 %resource_drop_not_consumed, label %resource_drop_live_only, label %resource_drop_merge

bb2:                                              ; preds = %helper_crash_cleanup_guard_merge50
  %rc_count_handle92 = load ptr, ptr %local_2, align 4
  %rc_count93 = call i32 @hew_rc_weak_count(ptr %rc_count_handle92)
  %ffi_zext94 = zext i32 %rc_count93 to i64
  store i64 %ffi_zext94, ptr %local_17, align 8
  %checked_lhs95 = load i64, ptr %local_15, align 8
  %checked_rhs96 = load i64, ptr %local_17, align 8
  %with_overflow97 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs95, i64 %checked_rhs96)
  %checked_result98 = extractvalue { i64, i1 } %with_overflow97, 0
  %checked_overflow99 = extractvalue { i64, i1 } %with_overflow97, 1
  %checked_overflow_widen100 = zext i1 %checked_overflow99 to i8
  store i64 %checked_result98, ptr %local_18, align 8
  store i8 %checked_overflow_widen100, ptr %local_19, align 1
  %cond_load101 = load i8, ptr %local_19, align 1
  %cond_nz102 = icmp ne i8 %cond_load101, 0
  br i1 %cond_nz102, label %bb3, label %bb4

bb3:                                              ; preds = %bb2
  %resource_drop_flag103 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed104 = icmp eq i64 %resource_drop_flag103, 0
  br i1 %resource_drop_not_consumed104, label %resource_drop_live_only105, label %resource_drop_merge106

bb4:                                              ; preds = %bb2
  %move_load151 = load i64, ptr %local_18, align 8
  store i64 %move_load151, ptr %local_20, align 8
  %rc_is_unique_handle = load ptr, ptr %local_2, align 4
  %rc_is_unique = call i32 @hew_rc_is_unique(ptr %rc_is_unique_handle)
  %rc_unique_bit = icmp ne i32 %rc_is_unique, 0
  %rc_unique_bool = zext i1 %rc_unique_bit to i8
  store i8 %rc_unique_bool, ptr %local_22, align 1
  %cond_load152 = load i8, ptr %local_22, align 1
  %cond_nz153 = icmp ne i8 %cond_load152, 0
  br i1 %cond_nz153, label %bb5, label %bb6

bb5:                                              ; preds = %bb4
  store i64 90, ptr %local_23, align 8
  %move_load154 = load i64, ptr %local_23, align 8
  store i64 %move_load154, ptr %local_24, align 8
  %move_load155 = load i64, ptr %local_24, align 8
  store i64 %move_load155, ptr %local_21, align 8
  br label %bb7

bb6:                                              ; preds = %bb4
  store i64 1, ptr %local_25, align 8
  %move_load156 = load i64, ptr %local_25, align 8
  store i64 %move_load156, ptr %local_26, align 8
  %move_load157 = load i64, ptr %local_26, align 8
  store i64 %move_load157, ptr %local_21, align 8
  br label %bb7

bb7:                                              ; preds = %bb6, %bb5
  %move_load158 = load i64, ptr %local_21, align 8
  store i64 %move_load158, ptr %local_27, align 8
  %helper_crash_cleanup_was_active159 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_was_active159, label %helper_crash_cleanup_deactivate160, label %helper_crash_cleanup_deactivate_merge161

bb8:                                              ; preds = %after_cooperate621, %after_cooperate255
  %helper_crash_cleanup_drop_active174 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_drop_active174, label %helper_crash_cleanup_retire175, label %helper_crash_cleanup_retire_merge176

bb9:                                              ; preds = %frame_cleanup_registered169
  %machine_payload_ptr237 = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %local_29, i32 0, i32 1
  %machine_variant_field_ptr238 = getelementptr inbounds nuw { ptr }, ptr %machine_payload_ptr237, i32 0, i32 0
  %move_load239 = load ptr, ptr %machine_variant_field_ptr238, align 4
  store ptr %move_load239, ptr %local_35, align 4
  store i64 9, ptr %local_36, align 8
  %rc_set_handle = load ptr, ptr %local_2, align 4
  call void @hew_rc_set(ptr %rc_set_handle, ptr %local_36)
  store i8 0, ptr %local_37, align 1
  %rc_count_handle240 = load ptr, ptr %local_35, align 4
  %rc_count241 = call i32 @hew_rc_strong_count(ptr %rc_count_handle240)
  %ffi_zext242 = zext i32 %rc_count241 to i64
  store i64 %ffi_zext242, ptr %local_38, align 8
  store i64 10, ptr %local_39, align 8
  %checked_lhs243 = load i64, ptr %local_38, align 8
  %checked_rhs244 = load i64, ptr %local_39, align 8
  %with_overflow245 = call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %checked_lhs243, i64 %checked_rhs244)
  %checked_result246 = extractvalue { i64, i1 } %with_overflow245, 0
  %checked_overflow247 = extractvalue { i64, i1 } %with_overflow245, 1
  %checked_overflow_widen248 = zext i1 %checked_overflow247 to i8
  store i64 %checked_result246, ptr %local_40, align 8
  store i8 %checked_overflow_widen248, ptr %local_41, align 1
  %cond_load249 = load i8, ptr %local_41, align 1
  %cond_nz250 = icmp ne i8 %cond_load249, 0
  br i1 %cond_nz250, label %bb13, label %bb14

bb10:                                             ; preds = %bb12
  store i64 9000, ptr %local_50, align 8
  %move_load251 = load i64, ptr %local_50, align 8
  store i64 %move_load251, ptr %local_28, align 8
  %hew_actor_cooperate252 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel253 = icmp eq i32 %hew_actor_cooperate252, 2
  br i1 %hew_cooperate_is_cancel253, label %cancel_exit254, label %after_cooperate255

bb11:                                             ; preds = %bb12
  %helper_crash_cleanup_drop_active311 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_drop_active311, label %helper_crash_cleanup_retire312, label %helper_crash_cleanup_retire_merge313

bb12:                                             ; preds = %frame_cleanup_registered169
  store i64 1, ptr %local_33, align 8
  %cmp_lhs366 = load i64, ptr %local_30, align 8
  %cmp_rhs367 = load i64, ptr %local_33, align 8
  %cmp_bit368 = icmp eq i64 %cmp_lhs366, %cmp_rhs367
  %cmp_zext369 = zext i1 %cmp_bit368 to i8
  store i8 %cmp_zext369, ptr %local_34, align 1
  %cond_load370 = load i8, ptr %local_34, align 1
  %cond_nz371 = icmp ne i8 %cond_load370, 0
  br i1 %cond_nz371, label %bb10, label %bb11

bb13:                                             ; preds = %bb9
  %helper_crash_cleanup_drop_active372 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_drop_active372, label %helper_crash_cleanup_retire373, label %helper_crash_cleanup_retire_merge374

bb14:                                             ; preds = %bb9
  %checked_lhs427 = load i64, ptr %local_20, align 8
  %checked_rhs428 = load i64, ptr %local_40, align 8
  %with_overflow429 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs427, i64 %checked_rhs428)
  %checked_result430 = extractvalue { i64, i1 } %with_overflow429, 0
  %checked_overflow431 = extractvalue { i64, i1 } %with_overflow429, 1
  %checked_overflow_widen432 = zext i1 %checked_overflow431 to i8
  store i64 %checked_result430, ptr %local_42, align 8
  store i8 %checked_overflow_widen432, ptr %local_43, align 1
  %cond_load433 = load i8, ptr %local_43, align 1
  %cond_nz434 = icmp ne i8 %cond_load433, 0
  br i1 %cond_nz434, label %bb15, label %bb16

bb15:                                             ; preds = %bb14
  %helper_crash_cleanup_drop_active435 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_drop_active435, label %helper_crash_cleanup_retire436, label %helper_crash_cleanup_retire_merge437

bb16:                                             ; preds = %bb14
  %rc_get_handle = load ptr, ptr %local_5, align 4
  %rc_get_data = call ptr @hew_rc_get(ptr %rc_get_handle)
  %rc_get_value = load i64, ptr %rc_get_data, align 8
  store i64 %rc_get_value, ptr %local_44, align 8
  %checked_lhs490 = load i64, ptr %local_42, align 8
  %checked_rhs491 = load i64, ptr %local_44, align 8
  %with_overflow492 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs490, i64 %checked_rhs491)
  %checked_result493 = extractvalue { i64, i1 } %with_overflow492, 0
  %checked_overflow494 = extractvalue { i64, i1 } %with_overflow492, 1
  %checked_overflow_widen495 = zext i1 %checked_overflow494 to i8
  store i64 %checked_result493, ptr %local_45, align 8
  store i8 %checked_overflow_widen495, ptr %local_46, align 1
  %cond_load496 = load i8, ptr %local_46, align 1
  %cond_nz497 = icmp ne i8 %cond_load496, 0
  br i1 %cond_nz497, label %bb17, label %bb18

bb17:                                             ; preds = %bb16
  %helper_crash_cleanup_drop_active498 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_drop_active498, label %helper_crash_cleanup_retire499, label %helper_crash_cleanup_retire_merge500

bb18:                                             ; preds = %bb16
  %checked_lhs553 = load i64, ptr %local_45, align 8
  %checked_rhs554 = load i64, ptr %local_27, align 8
  %with_overflow555 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs553, i64 %checked_rhs554)
  %checked_result556 = extractvalue { i64, i1 } %with_overflow555, 0
  %checked_overflow557 = extractvalue { i64, i1 } %with_overflow555, 1
  %checked_overflow_widen558 = zext i1 %checked_overflow557 to i8
  store i64 %checked_result556, ptr %local_47, align 8
  store i8 %checked_overflow_widen558, ptr %local_48, align 1
  %cond_load559 = load i8, ptr %local_48, align 1
  %cond_nz560 = icmp ne i8 %cond_load559, 0
  br i1 %cond_nz560, label %bb19, label %bb20

bb19:                                             ; preds = %bb18
  %helper_crash_cleanup_drop_active561 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_drop_active561, label %helper_crash_cleanup_retire562, label %helper_crash_cleanup_retire_merge563

bb20:                                             ; preds = %bb18
  %move_load616 = load i64, ptr %local_47, align 8
  store i64 %move_load616, ptr %local_49, align 8
  %move_load617 = load i64, ptr %local_49, align 8
  store i64 %move_load617, ptr %local_28, align 8
  %hew_actor_cooperate618 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel619 = icmp eq i32 %hew_actor_cooperate618, 2
  br i1 %hew_cooperate_is_cancel619, label %cancel_exit620, label %after_cooperate621

cancel_exit:                                      ; preds = %entry
  ret i64 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

helper_crash_cleanup_deactivate:                  ; preds = %bb0
  %helper_crash_cleanup_token = load i64, ptr %helper_crash_cleanup_token_2, align 8
  %helper_crash_cleanup_deactivate_call = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token)
  br i1 %helper_crash_cleanup_deactivate_call, label %helper_crash_cleanup_deactivate_accepted, label %helper_crash_cleanup_deactivate_rejected

helper_crash_cleanup_deactivate_merge:            ; preds = %helper_crash_cleanup_deactivate_accepted, %bb0
  %move_load = load ptr, ptr %local_1, align 4
  store ptr %move_load, ptr %local_2, align 4
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
  %arm_typed_crash_cleanup = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token, ptr %local_2, i64 4, i64 4, ptr @__hew_frame_cleanup_445350923e96a538, i32 1, i32 0)
  %frame_cleanup_arm_failed = icmp eq i64 %arm_typed_crash_cleanup, -1
  br i1 %frame_cleanup_arm_failed, label %frame_cleanup_rejected, label %frame_cleanup_registered

helper_crash_cleanup_guard_merge:                 ; preds = %frame_cleanup_registered, %helper_crash_cleanup_deactivate_merge
  %rc_borrow_handle = load ptr, ptr %local_2, align 4
  %rc_handle_result = call ptr @hew_rc_clone(ptr %rc_borrow_handle)
  store ptr %rc_handle_result, ptr %local_4, align 4
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
  %move_load8 = load ptr, ptr %local_4, align 4
  store ptr %move_load8, ptr %local_5, align 4
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
  %arm_typed_crash_cleanup14 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token13, ptr %local_5, i64 4, i64 4, ptr @__hew_frame_cleanup_445350923e96a538, i32 1, i32 0)
  %frame_cleanup_arm_failed15 = icmp eq i64 %arm_typed_crash_cleanup14, -1
  br i1 %frame_cleanup_arm_failed15, label %frame_cleanup_rejected17, label %frame_cleanup_registered16

helper_crash_cleanup_guard_merge12:               ; preds = %frame_cleanup_registered16, %helper_crash_cleanup_deactivate_merge3
  %rc_borrow_handle18 = load ptr, ptr %local_2, align 4
  %rc_handle_result19 = call ptr @hew_rc_downgrade(ptr %rc_borrow_handle18)
  store ptr %rc_handle_result19, ptr %local_7, align 4
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
  %move_load27 = load ptr, ptr %local_7, align 4
  store ptr %move_load27, ptr %local_8, align 4
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
  %arm_typed_crash_cleanup33 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token32, ptr %local_8, i64 4, i64 4, ptr @__hew_frame_cleanup_7b793acd5045b9cb, i32 1, i32 0)
  %frame_cleanup_arm_failed34 = icmp eq i64 %arm_typed_crash_cleanup33, -1
  br i1 %frame_cleanup_arm_failed34, label %frame_cleanup_rejected36, label %frame_cleanup_registered35

helper_crash_cleanup_guard_merge31:               ; preds = %frame_cleanup_registered35, %helper_crash_cleanup_deactivate_merge22
  %rc_borrow_handle37 = load ptr, ptr %local_8, align 4
  %rc_handle_result38 = call ptr @hew_weak_clone_rc(ptr %rc_borrow_handle37)
  store ptr %rc_handle_result38, ptr %local_10, align 4
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
  %move_load46 = load ptr, ptr %local_10, align 4
  store ptr %move_load46, ptr %local_11, align 4
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
  %arm_typed_crash_cleanup52 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token51, ptr %local_11, i64 4, i64 4, ptr @__hew_frame_cleanup_7b793acd5045b9cb, i32 1, i32 0)
  %frame_cleanup_arm_failed53 = icmp eq i64 %arm_typed_crash_cleanup52, -1
  br i1 %frame_cleanup_arm_failed53, label %frame_cleanup_rejected55, label %frame_cleanup_registered54

helper_crash_cleanup_guard_merge50:               ; preds = %frame_cleanup_registered54, %helper_crash_cleanup_deactivate_merge41
  %rc_count_handle = load ptr, ptr %local_2, align 4
  %rc_count = call i32 @hew_rc_strong_count(ptr %rc_count_handle)
  %ffi_zext = zext i32 %rc_count to i64
  store i64 %ffi_zext, ptr %local_13, align 8
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
  %ref_drop_handle = load ptr, ptr %local_11, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle)
  store ptr null, ptr %local_11, align 4
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
  %ref_drop_handle67 = load ptr, ptr %local_8, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle67)
  store ptr null, ptr %local_8, align 4
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
  %ref_drop_handle79 = load ptr, ptr %local_5, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle79)
  store ptr null, ptr %local_5, align 4
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
  %ref_drop_handle91 = load ptr, ptr %local_2, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle91)
  store ptr null, ptr %local_2, align 4
  br label %resource_drop_merge83

helper_crash_cleanup_retire_accepted89:           ; preds = %helper_crash_cleanup_retire85
  store i64 0, ptr %helper_crash_cleanup_token_2, align 8
  store i1 false, ptr %helper_crash_cleanup_active_2, align 1
  br label %helper_crash_cleanup_retire_merge86

helper_crash_cleanup_retire_rejected90:           ; preds = %helper_crash_cleanup_retire85
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only105:                       ; preds = %bb3
  %helper_crash_cleanup_drop_active107 = load i1, ptr %helper_crash_cleanup_active_11, align 1
  br i1 %helper_crash_cleanup_drop_active107, label %helper_crash_cleanup_retire108, label %helper_crash_cleanup_retire_merge109

resource_drop_merge106:                           ; preds = %helper_crash_cleanup_retire_merge109, %bb3
  %resource_drop_flag115 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed116 = icmp eq i64 %resource_drop_flag115, 0
  br i1 %resource_drop_not_consumed116, label %resource_drop_live_only117, label %resource_drop_merge118

helper_crash_cleanup_retire108:                   ; preds = %resource_drop_live_only105
  %helper_crash_cleanup_retire_token110 = load i64, ptr %helper_crash_cleanup_token_11, align 8
  %helper_crash_cleanup_retire_call111 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token110)
  br i1 %helper_crash_cleanup_retire_call111, label %helper_crash_cleanup_retire_accepted112, label %helper_crash_cleanup_retire_rejected113

helper_crash_cleanup_retire_merge109:             ; preds = %helper_crash_cleanup_retire_accepted112, %resource_drop_live_only105
  %ref_drop_handle114 = load ptr, ptr %local_11, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle114)
  store ptr null, ptr %local_11, align 4
  br label %resource_drop_merge106

helper_crash_cleanup_retire_accepted112:          ; preds = %helper_crash_cleanup_retire108
  store i64 0, ptr %helper_crash_cleanup_token_11, align 8
  store i1 false, ptr %helper_crash_cleanup_active_11, align 1
  br label %helper_crash_cleanup_retire_merge109

helper_crash_cleanup_retire_rejected113:          ; preds = %helper_crash_cleanup_retire108
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only117:                       ; preds = %resource_drop_merge106
  %helper_crash_cleanup_drop_active119 = load i1, ptr %helper_crash_cleanup_active_8, align 1
  br i1 %helper_crash_cleanup_drop_active119, label %helper_crash_cleanup_retire120, label %helper_crash_cleanup_retire_merge121

resource_drop_merge118:                           ; preds = %helper_crash_cleanup_retire_merge121, %resource_drop_merge106
  %resource_drop_flag127 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed128 = icmp eq i64 %resource_drop_flag127, 0
  br i1 %resource_drop_not_consumed128, label %resource_drop_live_only129, label %resource_drop_merge130

helper_crash_cleanup_retire120:                   ; preds = %resource_drop_live_only117
  %helper_crash_cleanup_retire_token122 = load i64, ptr %helper_crash_cleanup_token_8, align 8
  %helper_crash_cleanup_retire_call123 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token122)
  br i1 %helper_crash_cleanup_retire_call123, label %helper_crash_cleanup_retire_accepted124, label %helper_crash_cleanup_retire_rejected125

helper_crash_cleanup_retire_merge121:             ; preds = %helper_crash_cleanup_retire_accepted124, %resource_drop_live_only117
  %ref_drop_handle126 = load ptr, ptr %local_8, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle126)
  store ptr null, ptr %local_8, align 4
  br label %resource_drop_merge118

helper_crash_cleanup_retire_accepted124:          ; preds = %helper_crash_cleanup_retire120
  store i64 0, ptr %helper_crash_cleanup_token_8, align 8
  store i1 false, ptr %helper_crash_cleanup_active_8, align 1
  br label %helper_crash_cleanup_retire_merge121

helper_crash_cleanup_retire_rejected125:          ; preds = %helper_crash_cleanup_retire120
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only129:                       ; preds = %resource_drop_merge118
  %helper_crash_cleanup_drop_active131 = load i1, ptr %helper_crash_cleanup_active_5, align 1
  br i1 %helper_crash_cleanup_drop_active131, label %helper_crash_cleanup_retire132, label %helper_crash_cleanup_retire_merge133

resource_drop_merge130:                           ; preds = %helper_crash_cleanup_retire_merge133, %resource_drop_merge118
  %resource_drop_flag139 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed140 = icmp eq i64 %resource_drop_flag139, 0
  br i1 %resource_drop_not_consumed140, label %resource_drop_live_only141, label %resource_drop_merge142

helper_crash_cleanup_retire132:                   ; preds = %resource_drop_live_only129
  %helper_crash_cleanup_retire_token134 = load i64, ptr %helper_crash_cleanup_token_5, align 8
  %helper_crash_cleanup_retire_call135 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token134)
  br i1 %helper_crash_cleanup_retire_call135, label %helper_crash_cleanup_retire_accepted136, label %helper_crash_cleanup_retire_rejected137

helper_crash_cleanup_retire_merge133:             ; preds = %helper_crash_cleanup_retire_accepted136, %resource_drop_live_only129
  %ref_drop_handle138 = load ptr, ptr %local_5, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle138)
  store ptr null, ptr %local_5, align 4
  br label %resource_drop_merge130

helper_crash_cleanup_retire_accepted136:          ; preds = %helper_crash_cleanup_retire132
  store i64 0, ptr %helper_crash_cleanup_token_5, align 8
  store i1 false, ptr %helper_crash_cleanup_active_5, align 1
  br label %helper_crash_cleanup_retire_merge133

helper_crash_cleanup_retire_rejected137:          ; preds = %helper_crash_cleanup_retire132
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only141:                       ; preds = %resource_drop_merge130
  %helper_crash_cleanup_drop_active143 = load i1, ptr %helper_crash_cleanup_active_2, align 1
  br i1 %helper_crash_cleanup_drop_active143, label %helper_crash_cleanup_retire144, label %helper_crash_cleanup_retire_merge145

resource_drop_merge142:                           ; preds = %helper_crash_cleanup_retire_merge145, %resource_drop_merge130
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire144:                   ; preds = %resource_drop_live_only141
  %helper_crash_cleanup_retire_token146 = load i64, ptr %helper_crash_cleanup_token_2, align 8
  %helper_crash_cleanup_retire_call147 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token146)
  br i1 %helper_crash_cleanup_retire_call147, label %helper_crash_cleanup_retire_accepted148, label %helper_crash_cleanup_retire_rejected149

helper_crash_cleanup_retire_merge145:             ; preds = %helper_crash_cleanup_retire_accepted148, %resource_drop_live_only141
  %ref_drop_handle150 = load ptr, ptr %local_2, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle150)
  store ptr null, ptr %local_2, align 4
  br label %resource_drop_merge142

helper_crash_cleanup_retire_accepted148:          ; preds = %helper_crash_cleanup_retire144
  store i64 0, ptr %helper_crash_cleanup_token_2, align 8
  store i1 false, ptr %helper_crash_cleanup_active_2, align 1
  br label %helper_crash_cleanup_retire_merge145

helper_crash_cleanup_retire_rejected149:          ; preds = %helper_crash_cleanup_retire144
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_deactivate160:               ; preds = %bb7
  %helper_crash_cleanup_token162 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_deactivate_call163 = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token162)
  br i1 %helper_crash_cleanup_deactivate_call163, label %helper_crash_cleanup_deactivate_accepted164, label %helper_crash_cleanup_deactivate_rejected165

helper_crash_cleanup_deactivate_merge161:         ; preds = %helper_crash_cleanup_deactivate_accepted164, %bb7
  %weak_upgrade_handle = load ptr, ptr %local_8, align 4
  %weak_upgrade = call ptr @hew_weak_upgrade_rc(ptr %weak_upgrade_handle)
  %machine_tag_ptr = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %local_29, i32 0, i32 0
  %machine_payload_ptr = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %local_29, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { ptr }, ptr %machine_payload_ptr, i32 0, i32 0
  %weak_upgrade_is_none = icmp eq ptr %weak_upgrade, null
  br i1 %weak_upgrade_is_none, label %weak_upgrade_none, label %weak_upgrade_some

helper_crash_cleanup_deactivate_accepted164:      ; preds = %helper_crash_cleanup_deactivate160
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_deactivate_merge161

helper_crash_cleanup_deactivate_rejected165:      ; preds = %helper_crash_cleanup_deactivate160
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

weak_upgrade_some:                                ; preds = %helper_crash_cleanup_deactivate_merge161
  store i8 0, ptr %machine_tag_ptr, align 1
  store ptr %weak_upgrade, ptr %machine_variant_field_ptr, align 4
  br label %weak_upgrade_cont

weak_upgrade_none:                                ; preds = %helper_crash_cleanup_deactivate_merge161
  store i8 1, ptr %machine_tag_ptr, align 1
  store ptr null, ptr %machine_variant_field_ptr, align 4
  br label %weak_upgrade_cont

weak_upgrade_cont:                                ; preds = %weak_upgrade_none, %weak_upgrade_some
  %helper_crash_cleanup_prior_token166 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %arm_typed_crash_cleanup167 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token166, ptr %local_29, i64 8, i64 4, ptr @__hew_frame_cleanup_063d4439fd0b791f, i32 1, i32 0)
  %frame_cleanup_arm_failed168 = icmp eq i64 %arm_typed_crash_cleanup167, -1
  br i1 %frame_cleanup_arm_failed168, label %frame_cleanup_rejected170, label %frame_cleanup_registered169

frame_cleanup_registered169:                      ; preds = %weak_upgrade_cont
  store i64 %arm_typed_crash_cleanup167, ptr %helper_crash_cleanup_token_29, align 8
  store i1 true, ptr %helper_crash_cleanup_active_29, align 1
  %machine_tag_ptr171 = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %local_29, i32 0, i32 0
  %move_iN_load = load i8, ptr %machine_tag_ptr171, align 1
  %move_iN_zext = zext i8 %move_iN_load to i64
  store i64 %move_iN_zext, ptr %local_30, align 8
  store i64 0, ptr %local_31, align 8
  %cmp_lhs = load i64, ptr %local_30, align 8
  %cmp_rhs = load i64, ptr %local_31, align 8
  %cmp_bit = icmp eq i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_32, align 1
  %cond_load172 = load i8, ptr %local_32, align 1
  %cond_nz173 = icmp ne i8 %cond_load172, 0
  br i1 %cond_nz173, label %bb9, label %bb12

frame_cleanup_rejected170:                        ; preds = %weak_upgrade_cont
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire175:                   ; preds = %bb8
  %helper_crash_cleanup_retire_token177 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_retire_call178 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token177)
  br i1 %helper_crash_cleanup_retire_call178, label %helper_crash_cleanup_retire_accepted179, label %helper_crash_cleanup_retire_rejected180

helper_crash_cleanup_retire_merge176:             ; preds = %helper_crash_cleanup_retire_accepted179, %bb8
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_29)
  store %"Option$$Rc$li64$g" zeroinitializer, ptr %local_29, align 4
  %move_load181 = load i64, ptr %local_28, align 8
  store i64 %move_load181, ptr %return_slot, align 8
  %helper_crash_cleanup_drop_active182 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_drop_active182, label %helper_crash_cleanup_retire183, label %helper_crash_cleanup_retire_merge184

helper_crash_cleanup_retire_accepted179:          ; preds = %helper_crash_cleanup_retire175
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_retire_merge176

helper_crash_cleanup_retire_rejected180:          ; preds = %helper_crash_cleanup_retire175
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire183:                   ; preds = %helper_crash_cleanup_retire_merge176
  %helper_crash_cleanup_retire_token185 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_retire_call186 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token185)
  br i1 %helper_crash_cleanup_retire_call186, label %helper_crash_cleanup_retire_accepted187, label %helper_crash_cleanup_retire_rejected188

helper_crash_cleanup_retire_merge184:             ; preds = %helper_crash_cleanup_retire_accepted187, %helper_crash_cleanup_retire_merge176
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_29)
  %resource_drop_flag189 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed190 = icmp eq i64 %resource_drop_flag189, 0
  br i1 %resource_drop_not_consumed190, label %resource_drop_live_only191, label %resource_drop_merge192

helper_crash_cleanup_retire_accepted187:          ; preds = %helper_crash_cleanup_retire183
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_retire_merge184

helper_crash_cleanup_retire_rejected188:          ; preds = %helper_crash_cleanup_retire183
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only191:                       ; preds = %helper_crash_cleanup_retire_merge184
  %helper_crash_cleanup_drop_active193 = load i1, ptr %helper_crash_cleanup_active_11, align 1
  br i1 %helper_crash_cleanup_drop_active193, label %helper_crash_cleanup_retire194, label %helper_crash_cleanup_retire_merge195

resource_drop_merge192:                           ; preds = %helper_crash_cleanup_retire_merge195, %helper_crash_cleanup_retire_merge184
  %resource_drop_flag201 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed202 = icmp eq i64 %resource_drop_flag201, 0
  br i1 %resource_drop_not_consumed202, label %resource_drop_live_only203, label %resource_drop_merge204

helper_crash_cleanup_retire194:                   ; preds = %resource_drop_live_only191
  %helper_crash_cleanup_retire_token196 = load i64, ptr %helper_crash_cleanup_token_11, align 8
  %helper_crash_cleanup_retire_call197 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token196)
  br i1 %helper_crash_cleanup_retire_call197, label %helper_crash_cleanup_retire_accepted198, label %helper_crash_cleanup_retire_rejected199

helper_crash_cleanup_retire_merge195:             ; preds = %helper_crash_cleanup_retire_accepted198, %resource_drop_live_only191
  %ref_drop_handle200 = load ptr, ptr %local_11, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle200)
  store ptr null, ptr %local_11, align 4
  br label %resource_drop_merge192

helper_crash_cleanup_retire_accepted198:          ; preds = %helper_crash_cleanup_retire194
  store i64 0, ptr %helper_crash_cleanup_token_11, align 8
  store i1 false, ptr %helper_crash_cleanup_active_11, align 1
  br label %helper_crash_cleanup_retire_merge195

helper_crash_cleanup_retire_rejected199:          ; preds = %helper_crash_cleanup_retire194
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only203:                       ; preds = %resource_drop_merge192
  %helper_crash_cleanup_drop_active205 = load i1, ptr %helper_crash_cleanup_active_8, align 1
  br i1 %helper_crash_cleanup_drop_active205, label %helper_crash_cleanup_retire206, label %helper_crash_cleanup_retire_merge207

resource_drop_merge204:                           ; preds = %helper_crash_cleanup_retire_merge207, %resource_drop_merge192
  %resource_drop_flag213 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed214 = icmp eq i64 %resource_drop_flag213, 0
  br i1 %resource_drop_not_consumed214, label %resource_drop_live_only215, label %resource_drop_merge216

helper_crash_cleanup_retire206:                   ; preds = %resource_drop_live_only203
  %helper_crash_cleanup_retire_token208 = load i64, ptr %helper_crash_cleanup_token_8, align 8
  %helper_crash_cleanup_retire_call209 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token208)
  br i1 %helper_crash_cleanup_retire_call209, label %helper_crash_cleanup_retire_accepted210, label %helper_crash_cleanup_retire_rejected211

helper_crash_cleanup_retire_merge207:             ; preds = %helper_crash_cleanup_retire_accepted210, %resource_drop_live_only203
  %ref_drop_handle212 = load ptr, ptr %local_8, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle212)
  store ptr null, ptr %local_8, align 4
  br label %resource_drop_merge204

helper_crash_cleanup_retire_accepted210:          ; preds = %helper_crash_cleanup_retire206
  store i64 0, ptr %helper_crash_cleanup_token_8, align 8
  store i1 false, ptr %helper_crash_cleanup_active_8, align 1
  br label %helper_crash_cleanup_retire_merge207

helper_crash_cleanup_retire_rejected211:          ; preds = %helper_crash_cleanup_retire206
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only215:                       ; preds = %resource_drop_merge204
  %helper_crash_cleanup_drop_active217 = load i1, ptr %helper_crash_cleanup_active_5, align 1
  br i1 %helper_crash_cleanup_drop_active217, label %helper_crash_cleanup_retire218, label %helper_crash_cleanup_retire_merge219

resource_drop_merge216:                           ; preds = %helper_crash_cleanup_retire_merge219, %resource_drop_merge204
  %resource_drop_flag225 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed226 = icmp eq i64 %resource_drop_flag225, 0
  br i1 %resource_drop_not_consumed226, label %resource_drop_live_only227, label %resource_drop_merge228

helper_crash_cleanup_retire218:                   ; preds = %resource_drop_live_only215
  %helper_crash_cleanup_retire_token220 = load i64, ptr %helper_crash_cleanup_token_5, align 8
  %helper_crash_cleanup_retire_call221 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token220)
  br i1 %helper_crash_cleanup_retire_call221, label %helper_crash_cleanup_retire_accepted222, label %helper_crash_cleanup_retire_rejected223

helper_crash_cleanup_retire_merge219:             ; preds = %helper_crash_cleanup_retire_accepted222, %resource_drop_live_only215
  %ref_drop_handle224 = load ptr, ptr %local_5, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle224)
  store ptr null, ptr %local_5, align 4
  br label %resource_drop_merge216

helper_crash_cleanup_retire_accepted222:          ; preds = %helper_crash_cleanup_retire218
  store i64 0, ptr %helper_crash_cleanup_token_5, align 8
  store i1 false, ptr %helper_crash_cleanup_active_5, align 1
  br label %helper_crash_cleanup_retire_merge219

helper_crash_cleanup_retire_rejected223:          ; preds = %helper_crash_cleanup_retire218
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only227:                       ; preds = %resource_drop_merge216
  %helper_crash_cleanup_drop_active229 = load i1, ptr %helper_crash_cleanup_active_2, align 1
  br i1 %helper_crash_cleanup_drop_active229, label %helper_crash_cleanup_retire230, label %helper_crash_cleanup_retire_merge231

resource_drop_merge228:                           ; preds = %helper_crash_cleanup_retire_merge231, %resource_drop_merge216
  %helper_crash_cleanup_return_token_2 = load i64, ptr %helper_crash_cleanup_token_2, align 8
  %helper_crash_cleanup_return_has_token_2 = icmp ne i64 %helper_crash_cleanup_return_token_2, 0
  br i1 %helper_crash_cleanup_return_has_token_2, label %helper_crash_cleanup_return_retire_2, label %helper_crash_cleanup_return_merge_2

helper_crash_cleanup_retire230:                   ; preds = %resource_drop_live_only227
  %helper_crash_cleanup_retire_token232 = load i64, ptr %helper_crash_cleanup_token_2, align 8
  %helper_crash_cleanup_retire_call233 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token232)
  br i1 %helper_crash_cleanup_retire_call233, label %helper_crash_cleanup_retire_accepted234, label %helper_crash_cleanup_retire_rejected235

helper_crash_cleanup_retire_merge231:             ; preds = %helper_crash_cleanup_retire_accepted234, %resource_drop_live_only227
  %ref_drop_handle236 = load ptr, ptr %local_2, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle236)
  store ptr null, ptr %local_2, align 4
  br label %resource_drop_merge228

helper_crash_cleanup_retire_accepted234:          ; preds = %helper_crash_cleanup_retire230
  store i64 0, ptr %helper_crash_cleanup_token_2, align 8
  store i1 false, ptr %helper_crash_cleanup_active_2, align 1
  br label %helper_crash_cleanup_retire_merge231

helper_crash_cleanup_retire_rejected235:          ; preds = %helper_crash_cleanup_retire230
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_return_merge_2:              ; preds = %helper_crash_cleanup_return_retire_2_accepted, %resource_drop_merge228
  %helper_crash_cleanup_return_token_5 = load i64, ptr %helper_crash_cleanup_token_5, align 8
  %helper_crash_cleanup_return_has_token_5 = icmp ne i64 %helper_crash_cleanup_return_token_5, 0
  br i1 %helper_crash_cleanup_return_has_token_5, label %helper_crash_cleanup_return_retire_5, label %helper_crash_cleanup_return_merge_5

helper_crash_cleanup_return_retire_2:             ; preds = %resource_drop_merge228
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

cancel_exit254:                                   ; preds = %bb10
  %helper_crash_cleanup_drop_active256 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_drop_active256, label %helper_crash_cleanup_retire257, label %helper_crash_cleanup_retire_merge258

after_cooperate255:                               ; preds = %bb10
  br label %bb8

helper_crash_cleanup_retire257:                   ; preds = %cancel_exit254
  %helper_crash_cleanup_retire_token259 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_retire_call260 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token259)
  br i1 %helper_crash_cleanup_retire_call260, label %helper_crash_cleanup_retire_accepted261, label %helper_crash_cleanup_retire_rejected262

helper_crash_cleanup_retire_merge258:             ; preds = %helper_crash_cleanup_retire_accepted261, %cancel_exit254
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_29)
  %resource_drop_flag263 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed264 = icmp eq i64 %resource_drop_flag263, 0
  br i1 %resource_drop_not_consumed264, label %resource_drop_live_only265, label %resource_drop_merge266

helper_crash_cleanup_retire_accepted261:          ; preds = %helper_crash_cleanup_retire257
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_retire_merge258

helper_crash_cleanup_retire_rejected262:          ; preds = %helper_crash_cleanup_retire257
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only265:                       ; preds = %helper_crash_cleanup_retire_merge258
  %helper_crash_cleanup_drop_active267 = load i1, ptr %helper_crash_cleanup_active_11, align 1
  br i1 %helper_crash_cleanup_drop_active267, label %helper_crash_cleanup_retire268, label %helper_crash_cleanup_retire_merge269

resource_drop_merge266:                           ; preds = %helper_crash_cleanup_retire_merge269, %helper_crash_cleanup_retire_merge258
  %resource_drop_flag275 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed276 = icmp eq i64 %resource_drop_flag275, 0
  br i1 %resource_drop_not_consumed276, label %resource_drop_live_only277, label %resource_drop_merge278

helper_crash_cleanup_retire268:                   ; preds = %resource_drop_live_only265
  %helper_crash_cleanup_retire_token270 = load i64, ptr %helper_crash_cleanup_token_11, align 8
  %helper_crash_cleanup_retire_call271 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token270)
  br i1 %helper_crash_cleanup_retire_call271, label %helper_crash_cleanup_retire_accepted272, label %helper_crash_cleanup_retire_rejected273

helper_crash_cleanup_retire_merge269:             ; preds = %helper_crash_cleanup_retire_accepted272, %resource_drop_live_only265
  %ref_drop_handle274 = load ptr, ptr %local_11, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle274)
  store ptr null, ptr %local_11, align 4
  br label %resource_drop_merge266

helper_crash_cleanup_retire_accepted272:          ; preds = %helper_crash_cleanup_retire268
  store i64 0, ptr %helper_crash_cleanup_token_11, align 8
  store i1 false, ptr %helper_crash_cleanup_active_11, align 1
  br label %helper_crash_cleanup_retire_merge269

helper_crash_cleanup_retire_rejected273:          ; preds = %helper_crash_cleanup_retire268
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only277:                       ; preds = %resource_drop_merge266
  %helper_crash_cleanup_drop_active279 = load i1, ptr %helper_crash_cleanup_active_8, align 1
  br i1 %helper_crash_cleanup_drop_active279, label %helper_crash_cleanup_retire280, label %helper_crash_cleanup_retire_merge281

resource_drop_merge278:                           ; preds = %helper_crash_cleanup_retire_merge281, %resource_drop_merge266
  %resource_drop_flag287 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed288 = icmp eq i64 %resource_drop_flag287, 0
  br i1 %resource_drop_not_consumed288, label %resource_drop_live_only289, label %resource_drop_merge290

helper_crash_cleanup_retire280:                   ; preds = %resource_drop_live_only277
  %helper_crash_cleanup_retire_token282 = load i64, ptr %helper_crash_cleanup_token_8, align 8
  %helper_crash_cleanup_retire_call283 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token282)
  br i1 %helper_crash_cleanup_retire_call283, label %helper_crash_cleanup_retire_accepted284, label %helper_crash_cleanup_retire_rejected285

helper_crash_cleanup_retire_merge281:             ; preds = %helper_crash_cleanup_retire_accepted284, %resource_drop_live_only277
  %ref_drop_handle286 = load ptr, ptr %local_8, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle286)
  store ptr null, ptr %local_8, align 4
  br label %resource_drop_merge278

helper_crash_cleanup_retire_accepted284:          ; preds = %helper_crash_cleanup_retire280
  store i64 0, ptr %helper_crash_cleanup_token_8, align 8
  store i1 false, ptr %helper_crash_cleanup_active_8, align 1
  br label %helper_crash_cleanup_retire_merge281

helper_crash_cleanup_retire_rejected285:          ; preds = %helper_crash_cleanup_retire280
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only289:                       ; preds = %resource_drop_merge278
  %helper_crash_cleanup_drop_active291 = load i1, ptr %helper_crash_cleanup_active_5, align 1
  br i1 %helper_crash_cleanup_drop_active291, label %helper_crash_cleanup_retire292, label %helper_crash_cleanup_retire_merge293

resource_drop_merge290:                           ; preds = %helper_crash_cleanup_retire_merge293, %resource_drop_merge278
  %resource_drop_flag299 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed300 = icmp eq i64 %resource_drop_flag299, 0
  br i1 %resource_drop_not_consumed300, label %resource_drop_live_only301, label %resource_drop_merge302

helper_crash_cleanup_retire292:                   ; preds = %resource_drop_live_only289
  %helper_crash_cleanup_retire_token294 = load i64, ptr %helper_crash_cleanup_token_5, align 8
  %helper_crash_cleanup_retire_call295 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token294)
  br i1 %helper_crash_cleanup_retire_call295, label %helper_crash_cleanup_retire_accepted296, label %helper_crash_cleanup_retire_rejected297

helper_crash_cleanup_retire_merge293:             ; preds = %helper_crash_cleanup_retire_accepted296, %resource_drop_live_only289
  %ref_drop_handle298 = load ptr, ptr %local_5, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle298)
  store ptr null, ptr %local_5, align 4
  br label %resource_drop_merge290

helper_crash_cleanup_retire_accepted296:          ; preds = %helper_crash_cleanup_retire292
  store i64 0, ptr %helper_crash_cleanup_token_5, align 8
  store i1 false, ptr %helper_crash_cleanup_active_5, align 1
  br label %helper_crash_cleanup_retire_merge293

helper_crash_cleanup_retire_rejected297:          ; preds = %helper_crash_cleanup_retire292
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only301:                       ; preds = %resource_drop_merge290
  %helper_crash_cleanup_drop_active303 = load i1, ptr %helper_crash_cleanup_active_2, align 1
  br i1 %helper_crash_cleanup_drop_active303, label %helper_crash_cleanup_retire304, label %helper_crash_cleanup_retire_merge305

resource_drop_merge302:                           ; preds = %helper_crash_cleanup_retire_merge305, %resource_drop_merge290
  ret i64 0

helper_crash_cleanup_retire304:                   ; preds = %resource_drop_live_only301
  %helper_crash_cleanup_retire_token306 = load i64, ptr %helper_crash_cleanup_token_2, align 8
  %helper_crash_cleanup_retire_call307 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token306)
  br i1 %helper_crash_cleanup_retire_call307, label %helper_crash_cleanup_retire_accepted308, label %helper_crash_cleanup_retire_rejected309

helper_crash_cleanup_retire_merge305:             ; preds = %helper_crash_cleanup_retire_accepted308, %resource_drop_live_only301
  %ref_drop_handle310 = load ptr, ptr %local_2, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle310)
  store ptr null, ptr %local_2, align 4
  br label %resource_drop_merge302

helper_crash_cleanup_retire_accepted308:          ; preds = %helper_crash_cleanup_retire304
  store i64 0, ptr %helper_crash_cleanup_token_2, align 8
  store i1 false, ptr %helper_crash_cleanup_active_2, align 1
  br label %helper_crash_cleanup_retire_merge305

helper_crash_cleanup_retire_rejected309:          ; preds = %helper_crash_cleanup_retire304
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire312:                   ; preds = %bb11
  %helper_crash_cleanup_retire_token314 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_retire_call315 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token314)
  br i1 %helper_crash_cleanup_retire_call315, label %helper_crash_cleanup_retire_accepted316, label %helper_crash_cleanup_retire_rejected317

helper_crash_cleanup_retire_merge313:             ; preds = %helper_crash_cleanup_retire_accepted316, %bb11
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_29)
  %resource_drop_flag318 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed319 = icmp eq i64 %resource_drop_flag318, 0
  br i1 %resource_drop_not_consumed319, label %resource_drop_live_only320, label %resource_drop_merge321

helper_crash_cleanup_retire_accepted316:          ; preds = %helper_crash_cleanup_retire312
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_retire_merge313

helper_crash_cleanup_retire_rejected317:          ; preds = %helper_crash_cleanup_retire312
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only320:                       ; preds = %helper_crash_cleanup_retire_merge313
  %helper_crash_cleanup_drop_active322 = load i1, ptr %helper_crash_cleanup_active_11, align 1
  br i1 %helper_crash_cleanup_drop_active322, label %helper_crash_cleanup_retire323, label %helper_crash_cleanup_retire_merge324

resource_drop_merge321:                           ; preds = %helper_crash_cleanup_retire_merge324, %helper_crash_cleanup_retire_merge313
  %resource_drop_flag330 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed331 = icmp eq i64 %resource_drop_flag330, 0
  br i1 %resource_drop_not_consumed331, label %resource_drop_live_only332, label %resource_drop_merge333

helper_crash_cleanup_retire323:                   ; preds = %resource_drop_live_only320
  %helper_crash_cleanup_retire_token325 = load i64, ptr %helper_crash_cleanup_token_11, align 8
  %helper_crash_cleanup_retire_call326 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token325)
  br i1 %helper_crash_cleanup_retire_call326, label %helper_crash_cleanup_retire_accepted327, label %helper_crash_cleanup_retire_rejected328

helper_crash_cleanup_retire_merge324:             ; preds = %helper_crash_cleanup_retire_accepted327, %resource_drop_live_only320
  %ref_drop_handle329 = load ptr, ptr %local_11, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle329)
  store ptr null, ptr %local_11, align 4
  br label %resource_drop_merge321

helper_crash_cleanup_retire_accepted327:          ; preds = %helper_crash_cleanup_retire323
  store i64 0, ptr %helper_crash_cleanup_token_11, align 8
  store i1 false, ptr %helper_crash_cleanup_active_11, align 1
  br label %helper_crash_cleanup_retire_merge324

helper_crash_cleanup_retire_rejected328:          ; preds = %helper_crash_cleanup_retire323
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only332:                       ; preds = %resource_drop_merge321
  %helper_crash_cleanup_drop_active334 = load i1, ptr %helper_crash_cleanup_active_8, align 1
  br i1 %helper_crash_cleanup_drop_active334, label %helper_crash_cleanup_retire335, label %helper_crash_cleanup_retire_merge336

resource_drop_merge333:                           ; preds = %helper_crash_cleanup_retire_merge336, %resource_drop_merge321
  %resource_drop_flag342 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed343 = icmp eq i64 %resource_drop_flag342, 0
  br i1 %resource_drop_not_consumed343, label %resource_drop_live_only344, label %resource_drop_merge345

helper_crash_cleanup_retire335:                   ; preds = %resource_drop_live_only332
  %helper_crash_cleanup_retire_token337 = load i64, ptr %helper_crash_cleanup_token_8, align 8
  %helper_crash_cleanup_retire_call338 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token337)
  br i1 %helper_crash_cleanup_retire_call338, label %helper_crash_cleanup_retire_accepted339, label %helper_crash_cleanup_retire_rejected340

helper_crash_cleanup_retire_merge336:             ; preds = %helper_crash_cleanup_retire_accepted339, %resource_drop_live_only332
  %ref_drop_handle341 = load ptr, ptr %local_8, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle341)
  store ptr null, ptr %local_8, align 4
  br label %resource_drop_merge333

helper_crash_cleanup_retire_accepted339:          ; preds = %helper_crash_cleanup_retire335
  store i64 0, ptr %helper_crash_cleanup_token_8, align 8
  store i1 false, ptr %helper_crash_cleanup_active_8, align 1
  br label %helper_crash_cleanup_retire_merge336

helper_crash_cleanup_retire_rejected340:          ; preds = %helper_crash_cleanup_retire335
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only344:                       ; preds = %resource_drop_merge333
  %helper_crash_cleanup_drop_active346 = load i1, ptr %helper_crash_cleanup_active_5, align 1
  br i1 %helper_crash_cleanup_drop_active346, label %helper_crash_cleanup_retire347, label %helper_crash_cleanup_retire_merge348

resource_drop_merge345:                           ; preds = %helper_crash_cleanup_retire_merge348, %resource_drop_merge333
  %resource_drop_flag354 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed355 = icmp eq i64 %resource_drop_flag354, 0
  br i1 %resource_drop_not_consumed355, label %resource_drop_live_only356, label %resource_drop_merge357

helper_crash_cleanup_retire347:                   ; preds = %resource_drop_live_only344
  %helper_crash_cleanup_retire_token349 = load i64, ptr %helper_crash_cleanup_token_5, align 8
  %helper_crash_cleanup_retire_call350 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token349)
  br i1 %helper_crash_cleanup_retire_call350, label %helper_crash_cleanup_retire_accepted351, label %helper_crash_cleanup_retire_rejected352

helper_crash_cleanup_retire_merge348:             ; preds = %helper_crash_cleanup_retire_accepted351, %resource_drop_live_only344
  %ref_drop_handle353 = load ptr, ptr %local_5, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle353)
  store ptr null, ptr %local_5, align 4
  br label %resource_drop_merge345

helper_crash_cleanup_retire_accepted351:          ; preds = %helper_crash_cleanup_retire347
  store i64 0, ptr %helper_crash_cleanup_token_5, align 8
  store i1 false, ptr %helper_crash_cleanup_active_5, align 1
  br label %helper_crash_cleanup_retire_merge348

helper_crash_cleanup_retire_rejected352:          ; preds = %helper_crash_cleanup_retire347
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only356:                       ; preds = %resource_drop_merge345
  %helper_crash_cleanup_drop_active358 = load i1, ptr %helper_crash_cleanup_active_2, align 1
  br i1 %helper_crash_cleanup_drop_active358, label %helper_crash_cleanup_retire359, label %helper_crash_cleanup_retire_merge360

resource_drop_merge357:                           ; preds = %helper_crash_cleanup_retire_merge360, %resource_drop_merge345
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire359:                   ; preds = %resource_drop_live_only356
  %helper_crash_cleanup_retire_token361 = load i64, ptr %helper_crash_cleanup_token_2, align 8
  %helper_crash_cleanup_retire_call362 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token361)
  br i1 %helper_crash_cleanup_retire_call362, label %helper_crash_cleanup_retire_accepted363, label %helper_crash_cleanup_retire_rejected364

helper_crash_cleanup_retire_merge360:             ; preds = %helper_crash_cleanup_retire_accepted363, %resource_drop_live_only356
  %ref_drop_handle365 = load ptr, ptr %local_2, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle365)
  store ptr null, ptr %local_2, align 4
  br label %resource_drop_merge357

helper_crash_cleanup_retire_accepted363:          ; preds = %helper_crash_cleanup_retire359
  store i64 0, ptr %helper_crash_cleanup_token_2, align 8
  store i1 false, ptr %helper_crash_cleanup_active_2, align 1
  br label %helper_crash_cleanup_retire_merge360

helper_crash_cleanup_retire_rejected364:          ; preds = %helper_crash_cleanup_retire359
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire373:                   ; preds = %bb13
  %helper_crash_cleanup_retire_token375 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_retire_call376 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token375)
  br i1 %helper_crash_cleanup_retire_call376, label %helper_crash_cleanup_retire_accepted377, label %helper_crash_cleanup_retire_rejected378

helper_crash_cleanup_retire_merge374:             ; preds = %helper_crash_cleanup_retire_accepted377, %bb13
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_29)
  %resource_drop_flag379 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed380 = icmp eq i64 %resource_drop_flag379, 0
  br i1 %resource_drop_not_consumed380, label %resource_drop_live_only381, label %resource_drop_merge382

helper_crash_cleanup_retire_accepted377:          ; preds = %helper_crash_cleanup_retire373
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_retire_merge374

helper_crash_cleanup_retire_rejected378:          ; preds = %helper_crash_cleanup_retire373
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only381:                       ; preds = %helper_crash_cleanup_retire_merge374
  %helper_crash_cleanup_drop_active383 = load i1, ptr %helper_crash_cleanup_active_11, align 1
  br i1 %helper_crash_cleanup_drop_active383, label %helper_crash_cleanup_retire384, label %helper_crash_cleanup_retire_merge385

resource_drop_merge382:                           ; preds = %helper_crash_cleanup_retire_merge385, %helper_crash_cleanup_retire_merge374
  %resource_drop_flag391 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed392 = icmp eq i64 %resource_drop_flag391, 0
  br i1 %resource_drop_not_consumed392, label %resource_drop_live_only393, label %resource_drop_merge394

helper_crash_cleanup_retire384:                   ; preds = %resource_drop_live_only381
  %helper_crash_cleanup_retire_token386 = load i64, ptr %helper_crash_cleanup_token_11, align 8
  %helper_crash_cleanup_retire_call387 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token386)
  br i1 %helper_crash_cleanup_retire_call387, label %helper_crash_cleanup_retire_accepted388, label %helper_crash_cleanup_retire_rejected389

helper_crash_cleanup_retire_merge385:             ; preds = %helper_crash_cleanup_retire_accepted388, %resource_drop_live_only381
  %ref_drop_handle390 = load ptr, ptr %local_11, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle390)
  store ptr null, ptr %local_11, align 4
  br label %resource_drop_merge382

helper_crash_cleanup_retire_accepted388:          ; preds = %helper_crash_cleanup_retire384
  store i64 0, ptr %helper_crash_cleanup_token_11, align 8
  store i1 false, ptr %helper_crash_cleanup_active_11, align 1
  br label %helper_crash_cleanup_retire_merge385

helper_crash_cleanup_retire_rejected389:          ; preds = %helper_crash_cleanup_retire384
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only393:                       ; preds = %resource_drop_merge382
  %helper_crash_cleanup_drop_active395 = load i1, ptr %helper_crash_cleanup_active_8, align 1
  br i1 %helper_crash_cleanup_drop_active395, label %helper_crash_cleanup_retire396, label %helper_crash_cleanup_retire_merge397

resource_drop_merge394:                           ; preds = %helper_crash_cleanup_retire_merge397, %resource_drop_merge382
  %resource_drop_flag403 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed404 = icmp eq i64 %resource_drop_flag403, 0
  br i1 %resource_drop_not_consumed404, label %resource_drop_live_only405, label %resource_drop_merge406

helper_crash_cleanup_retire396:                   ; preds = %resource_drop_live_only393
  %helper_crash_cleanup_retire_token398 = load i64, ptr %helper_crash_cleanup_token_8, align 8
  %helper_crash_cleanup_retire_call399 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token398)
  br i1 %helper_crash_cleanup_retire_call399, label %helper_crash_cleanup_retire_accepted400, label %helper_crash_cleanup_retire_rejected401

helper_crash_cleanup_retire_merge397:             ; preds = %helper_crash_cleanup_retire_accepted400, %resource_drop_live_only393
  %ref_drop_handle402 = load ptr, ptr %local_8, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle402)
  store ptr null, ptr %local_8, align 4
  br label %resource_drop_merge394

helper_crash_cleanup_retire_accepted400:          ; preds = %helper_crash_cleanup_retire396
  store i64 0, ptr %helper_crash_cleanup_token_8, align 8
  store i1 false, ptr %helper_crash_cleanup_active_8, align 1
  br label %helper_crash_cleanup_retire_merge397

helper_crash_cleanup_retire_rejected401:          ; preds = %helper_crash_cleanup_retire396
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only405:                       ; preds = %resource_drop_merge394
  %helper_crash_cleanup_drop_active407 = load i1, ptr %helper_crash_cleanup_active_5, align 1
  br i1 %helper_crash_cleanup_drop_active407, label %helper_crash_cleanup_retire408, label %helper_crash_cleanup_retire_merge409

resource_drop_merge406:                           ; preds = %helper_crash_cleanup_retire_merge409, %resource_drop_merge394
  %resource_drop_flag415 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed416 = icmp eq i64 %resource_drop_flag415, 0
  br i1 %resource_drop_not_consumed416, label %resource_drop_live_only417, label %resource_drop_merge418

helper_crash_cleanup_retire408:                   ; preds = %resource_drop_live_only405
  %helper_crash_cleanup_retire_token410 = load i64, ptr %helper_crash_cleanup_token_5, align 8
  %helper_crash_cleanup_retire_call411 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token410)
  br i1 %helper_crash_cleanup_retire_call411, label %helper_crash_cleanup_retire_accepted412, label %helper_crash_cleanup_retire_rejected413

helper_crash_cleanup_retire_merge409:             ; preds = %helper_crash_cleanup_retire_accepted412, %resource_drop_live_only405
  %ref_drop_handle414 = load ptr, ptr %local_5, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle414)
  store ptr null, ptr %local_5, align 4
  br label %resource_drop_merge406

helper_crash_cleanup_retire_accepted412:          ; preds = %helper_crash_cleanup_retire408
  store i64 0, ptr %helper_crash_cleanup_token_5, align 8
  store i1 false, ptr %helper_crash_cleanup_active_5, align 1
  br label %helper_crash_cleanup_retire_merge409

helper_crash_cleanup_retire_rejected413:          ; preds = %helper_crash_cleanup_retire408
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only417:                       ; preds = %resource_drop_merge406
  %helper_crash_cleanup_drop_active419 = load i1, ptr %helper_crash_cleanup_active_2, align 1
  br i1 %helper_crash_cleanup_drop_active419, label %helper_crash_cleanup_retire420, label %helper_crash_cleanup_retire_merge421

resource_drop_merge418:                           ; preds = %helper_crash_cleanup_retire_merge421, %resource_drop_merge406
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire420:                   ; preds = %resource_drop_live_only417
  %helper_crash_cleanup_retire_token422 = load i64, ptr %helper_crash_cleanup_token_2, align 8
  %helper_crash_cleanup_retire_call423 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token422)
  br i1 %helper_crash_cleanup_retire_call423, label %helper_crash_cleanup_retire_accepted424, label %helper_crash_cleanup_retire_rejected425

helper_crash_cleanup_retire_merge421:             ; preds = %helper_crash_cleanup_retire_accepted424, %resource_drop_live_only417
  %ref_drop_handle426 = load ptr, ptr %local_2, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle426)
  store ptr null, ptr %local_2, align 4
  br label %resource_drop_merge418

helper_crash_cleanup_retire_accepted424:          ; preds = %helper_crash_cleanup_retire420
  store i64 0, ptr %helper_crash_cleanup_token_2, align 8
  store i1 false, ptr %helper_crash_cleanup_active_2, align 1
  br label %helper_crash_cleanup_retire_merge421

helper_crash_cleanup_retire_rejected425:          ; preds = %helper_crash_cleanup_retire420
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire436:                   ; preds = %bb15
  %helper_crash_cleanup_retire_token438 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_retire_call439 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token438)
  br i1 %helper_crash_cleanup_retire_call439, label %helper_crash_cleanup_retire_accepted440, label %helper_crash_cleanup_retire_rejected441

helper_crash_cleanup_retire_merge437:             ; preds = %helper_crash_cleanup_retire_accepted440, %bb15
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_29)
  %resource_drop_flag442 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed443 = icmp eq i64 %resource_drop_flag442, 0
  br i1 %resource_drop_not_consumed443, label %resource_drop_live_only444, label %resource_drop_merge445

helper_crash_cleanup_retire_accepted440:          ; preds = %helper_crash_cleanup_retire436
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_retire_merge437

helper_crash_cleanup_retire_rejected441:          ; preds = %helper_crash_cleanup_retire436
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only444:                       ; preds = %helper_crash_cleanup_retire_merge437
  %helper_crash_cleanup_drop_active446 = load i1, ptr %helper_crash_cleanup_active_11, align 1
  br i1 %helper_crash_cleanup_drop_active446, label %helper_crash_cleanup_retire447, label %helper_crash_cleanup_retire_merge448

resource_drop_merge445:                           ; preds = %helper_crash_cleanup_retire_merge448, %helper_crash_cleanup_retire_merge437
  %resource_drop_flag454 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed455 = icmp eq i64 %resource_drop_flag454, 0
  br i1 %resource_drop_not_consumed455, label %resource_drop_live_only456, label %resource_drop_merge457

helper_crash_cleanup_retire447:                   ; preds = %resource_drop_live_only444
  %helper_crash_cleanup_retire_token449 = load i64, ptr %helper_crash_cleanup_token_11, align 8
  %helper_crash_cleanup_retire_call450 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token449)
  br i1 %helper_crash_cleanup_retire_call450, label %helper_crash_cleanup_retire_accepted451, label %helper_crash_cleanup_retire_rejected452

helper_crash_cleanup_retire_merge448:             ; preds = %helper_crash_cleanup_retire_accepted451, %resource_drop_live_only444
  %ref_drop_handle453 = load ptr, ptr %local_11, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle453)
  store ptr null, ptr %local_11, align 4
  br label %resource_drop_merge445

helper_crash_cleanup_retire_accepted451:          ; preds = %helper_crash_cleanup_retire447
  store i64 0, ptr %helper_crash_cleanup_token_11, align 8
  store i1 false, ptr %helper_crash_cleanup_active_11, align 1
  br label %helper_crash_cleanup_retire_merge448

helper_crash_cleanup_retire_rejected452:          ; preds = %helper_crash_cleanup_retire447
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only456:                       ; preds = %resource_drop_merge445
  %helper_crash_cleanup_drop_active458 = load i1, ptr %helper_crash_cleanup_active_8, align 1
  br i1 %helper_crash_cleanup_drop_active458, label %helper_crash_cleanup_retire459, label %helper_crash_cleanup_retire_merge460

resource_drop_merge457:                           ; preds = %helper_crash_cleanup_retire_merge460, %resource_drop_merge445
  %resource_drop_flag466 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed467 = icmp eq i64 %resource_drop_flag466, 0
  br i1 %resource_drop_not_consumed467, label %resource_drop_live_only468, label %resource_drop_merge469

helper_crash_cleanup_retire459:                   ; preds = %resource_drop_live_only456
  %helper_crash_cleanup_retire_token461 = load i64, ptr %helper_crash_cleanup_token_8, align 8
  %helper_crash_cleanup_retire_call462 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token461)
  br i1 %helper_crash_cleanup_retire_call462, label %helper_crash_cleanup_retire_accepted463, label %helper_crash_cleanup_retire_rejected464

helper_crash_cleanup_retire_merge460:             ; preds = %helper_crash_cleanup_retire_accepted463, %resource_drop_live_only456
  %ref_drop_handle465 = load ptr, ptr %local_8, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle465)
  store ptr null, ptr %local_8, align 4
  br label %resource_drop_merge457

helper_crash_cleanup_retire_accepted463:          ; preds = %helper_crash_cleanup_retire459
  store i64 0, ptr %helper_crash_cleanup_token_8, align 8
  store i1 false, ptr %helper_crash_cleanup_active_8, align 1
  br label %helper_crash_cleanup_retire_merge460

helper_crash_cleanup_retire_rejected464:          ; preds = %helper_crash_cleanup_retire459
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only468:                       ; preds = %resource_drop_merge457
  %helper_crash_cleanup_drop_active470 = load i1, ptr %helper_crash_cleanup_active_5, align 1
  br i1 %helper_crash_cleanup_drop_active470, label %helper_crash_cleanup_retire471, label %helper_crash_cleanup_retire_merge472

resource_drop_merge469:                           ; preds = %helper_crash_cleanup_retire_merge472, %resource_drop_merge457
  %resource_drop_flag478 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed479 = icmp eq i64 %resource_drop_flag478, 0
  br i1 %resource_drop_not_consumed479, label %resource_drop_live_only480, label %resource_drop_merge481

helper_crash_cleanup_retire471:                   ; preds = %resource_drop_live_only468
  %helper_crash_cleanup_retire_token473 = load i64, ptr %helper_crash_cleanup_token_5, align 8
  %helper_crash_cleanup_retire_call474 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token473)
  br i1 %helper_crash_cleanup_retire_call474, label %helper_crash_cleanup_retire_accepted475, label %helper_crash_cleanup_retire_rejected476

helper_crash_cleanup_retire_merge472:             ; preds = %helper_crash_cleanup_retire_accepted475, %resource_drop_live_only468
  %ref_drop_handle477 = load ptr, ptr %local_5, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle477)
  store ptr null, ptr %local_5, align 4
  br label %resource_drop_merge469

helper_crash_cleanup_retire_accepted475:          ; preds = %helper_crash_cleanup_retire471
  store i64 0, ptr %helper_crash_cleanup_token_5, align 8
  store i1 false, ptr %helper_crash_cleanup_active_5, align 1
  br label %helper_crash_cleanup_retire_merge472

helper_crash_cleanup_retire_rejected476:          ; preds = %helper_crash_cleanup_retire471
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only480:                       ; preds = %resource_drop_merge469
  %helper_crash_cleanup_drop_active482 = load i1, ptr %helper_crash_cleanup_active_2, align 1
  br i1 %helper_crash_cleanup_drop_active482, label %helper_crash_cleanup_retire483, label %helper_crash_cleanup_retire_merge484

resource_drop_merge481:                           ; preds = %helper_crash_cleanup_retire_merge484, %resource_drop_merge469
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire483:                   ; preds = %resource_drop_live_only480
  %helper_crash_cleanup_retire_token485 = load i64, ptr %helper_crash_cleanup_token_2, align 8
  %helper_crash_cleanup_retire_call486 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token485)
  br i1 %helper_crash_cleanup_retire_call486, label %helper_crash_cleanup_retire_accepted487, label %helper_crash_cleanup_retire_rejected488

helper_crash_cleanup_retire_merge484:             ; preds = %helper_crash_cleanup_retire_accepted487, %resource_drop_live_only480
  %ref_drop_handle489 = load ptr, ptr %local_2, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle489)
  store ptr null, ptr %local_2, align 4
  br label %resource_drop_merge481

helper_crash_cleanup_retire_accepted487:          ; preds = %helper_crash_cleanup_retire483
  store i64 0, ptr %helper_crash_cleanup_token_2, align 8
  store i1 false, ptr %helper_crash_cleanup_active_2, align 1
  br label %helper_crash_cleanup_retire_merge484

helper_crash_cleanup_retire_rejected488:          ; preds = %helper_crash_cleanup_retire483
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire499:                   ; preds = %bb17
  %helper_crash_cleanup_retire_token501 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_retire_call502 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token501)
  br i1 %helper_crash_cleanup_retire_call502, label %helper_crash_cleanup_retire_accepted503, label %helper_crash_cleanup_retire_rejected504

helper_crash_cleanup_retire_merge500:             ; preds = %helper_crash_cleanup_retire_accepted503, %bb17
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_29)
  %resource_drop_flag505 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed506 = icmp eq i64 %resource_drop_flag505, 0
  br i1 %resource_drop_not_consumed506, label %resource_drop_live_only507, label %resource_drop_merge508

helper_crash_cleanup_retire_accepted503:          ; preds = %helper_crash_cleanup_retire499
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_retire_merge500

helper_crash_cleanup_retire_rejected504:          ; preds = %helper_crash_cleanup_retire499
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only507:                       ; preds = %helper_crash_cleanup_retire_merge500
  %helper_crash_cleanup_drop_active509 = load i1, ptr %helper_crash_cleanup_active_11, align 1
  br i1 %helper_crash_cleanup_drop_active509, label %helper_crash_cleanup_retire510, label %helper_crash_cleanup_retire_merge511

resource_drop_merge508:                           ; preds = %helper_crash_cleanup_retire_merge511, %helper_crash_cleanup_retire_merge500
  %resource_drop_flag517 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed518 = icmp eq i64 %resource_drop_flag517, 0
  br i1 %resource_drop_not_consumed518, label %resource_drop_live_only519, label %resource_drop_merge520

helper_crash_cleanup_retire510:                   ; preds = %resource_drop_live_only507
  %helper_crash_cleanup_retire_token512 = load i64, ptr %helper_crash_cleanup_token_11, align 8
  %helper_crash_cleanup_retire_call513 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token512)
  br i1 %helper_crash_cleanup_retire_call513, label %helper_crash_cleanup_retire_accepted514, label %helper_crash_cleanup_retire_rejected515

helper_crash_cleanup_retire_merge511:             ; preds = %helper_crash_cleanup_retire_accepted514, %resource_drop_live_only507
  %ref_drop_handle516 = load ptr, ptr %local_11, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle516)
  store ptr null, ptr %local_11, align 4
  br label %resource_drop_merge508

helper_crash_cleanup_retire_accepted514:          ; preds = %helper_crash_cleanup_retire510
  store i64 0, ptr %helper_crash_cleanup_token_11, align 8
  store i1 false, ptr %helper_crash_cleanup_active_11, align 1
  br label %helper_crash_cleanup_retire_merge511

helper_crash_cleanup_retire_rejected515:          ; preds = %helper_crash_cleanup_retire510
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only519:                       ; preds = %resource_drop_merge508
  %helper_crash_cleanup_drop_active521 = load i1, ptr %helper_crash_cleanup_active_8, align 1
  br i1 %helper_crash_cleanup_drop_active521, label %helper_crash_cleanup_retire522, label %helper_crash_cleanup_retire_merge523

resource_drop_merge520:                           ; preds = %helper_crash_cleanup_retire_merge523, %resource_drop_merge508
  %resource_drop_flag529 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed530 = icmp eq i64 %resource_drop_flag529, 0
  br i1 %resource_drop_not_consumed530, label %resource_drop_live_only531, label %resource_drop_merge532

helper_crash_cleanup_retire522:                   ; preds = %resource_drop_live_only519
  %helper_crash_cleanup_retire_token524 = load i64, ptr %helper_crash_cleanup_token_8, align 8
  %helper_crash_cleanup_retire_call525 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token524)
  br i1 %helper_crash_cleanup_retire_call525, label %helper_crash_cleanup_retire_accepted526, label %helper_crash_cleanup_retire_rejected527

helper_crash_cleanup_retire_merge523:             ; preds = %helper_crash_cleanup_retire_accepted526, %resource_drop_live_only519
  %ref_drop_handle528 = load ptr, ptr %local_8, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle528)
  store ptr null, ptr %local_8, align 4
  br label %resource_drop_merge520

helper_crash_cleanup_retire_accepted526:          ; preds = %helper_crash_cleanup_retire522
  store i64 0, ptr %helper_crash_cleanup_token_8, align 8
  store i1 false, ptr %helper_crash_cleanup_active_8, align 1
  br label %helper_crash_cleanup_retire_merge523

helper_crash_cleanup_retire_rejected527:          ; preds = %helper_crash_cleanup_retire522
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only531:                       ; preds = %resource_drop_merge520
  %helper_crash_cleanup_drop_active533 = load i1, ptr %helper_crash_cleanup_active_5, align 1
  br i1 %helper_crash_cleanup_drop_active533, label %helper_crash_cleanup_retire534, label %helper_crash_cleanup_retire_merge535

resource_drop_merge532:                           ; preds = %helper_crash_cleanup_retire_merge535, %resource_drop_merge520
  %resource_drop_flag541 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed542 = icmp eq i64 %resource_drop_flag541, 0
  br i1 %resource_drop_not_consumed542, label %resource_drop_live_only543, label %resource_drop_merge544

helper_crash_cleanup_retire534:                   ; preds = %resource_drop_live_only531
  %helper_crash_cleanup_retire_token536 = load i64, ptr %helper_crash_cleanup_token_5, align 8
  %helper_crash_cleanup_retire_call537 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token536)
  br i1 %helper_crash_cleanup_retire_call537, label %helper_crash_cleanup_retire_accepted538, label %helper_crash_cleanup_retire_rejected539

helper_crash_cleanup_retire_merge535:             ; preds = %helper_crash_cleanup_retire_accepted538, %resource_drop_live_only531
  %ref_drop_handle540 = load ptr, ptr %local_5, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle540)
  store ptr null, ptr %local_5, align 4
  br label %resource_drop_merge532

helper_crash_cleanup_retire_accepted538:          ; preds = %helper_crash_cleanup_retire534
  store i64 0, ptr %helper_crash_cleanup_token_5, align 8
  store i1 false, ptr %helper_crash_cleanup_active_5, align 1
  br label %helper_crash_cleanup_retire_merge535

helper_crash_cleanup_retire_rejected539:          ; preds = %helper_crash_cleanup_retire534
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only543:                       ; preds = %resource_drop_merge532
  %helper_crash_cleanup_drop_active545 = load i1, ptr %helper_crash_cleanup_active_2, align 1
  br i1 %helper_crash_cleanup_drop_active545, label %helper_crash_cleanup_retire546, label %helper_crash_cleanup_retire_merge547

resource_drop_merge544:                           ; preds = %helper_crash_cleanup_retire_merge547, %resource_drop_merge532
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire546:                   ; preds = %resource_drop_live_only543
  %helper_crash_cleanup_retire_token548 = load i64, ptr %helper_crash_cleanup_token_2, align 8
  %helper_crash_cleanup_retire_call549 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token548)
  br i1 %helper_crash_cleanup_retire_call549, label %helper_crash_cleanup_retire_accepted550, label %helper_crash_cleanup_retire_rejected551

helper_crash_cleanup_retire_merge547:             ; preds = %helper_crash_cleanup_retire_accepted550, %resource_drop_live_only543
  %ref_drop_handle552 = load ptr, ptr %local_2, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle552)
  store ptr null, ptr %local_2, align 4
  br label %resource_drop_merge544

helper_crash_cleanup_retire_accepted550:          ; preds = %helper_crash_cleanup_retire546
  store i64 0, ptr %helper_crash_cleanup_token_2, align 8
  store i1 false, ptr %helper_crash_cleanup_active_2, align 1
  br label %helper_crash_cleanup_retire_merge547

helper_crash_cleanup_retire_rejected551:          ; preds = %helper_crash_cleanup_retire546
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire562:                   ; preds = %bb19
  %helper_crash_cleanup_retire_token564 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_retire_call565 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token564)
  br i1 %helper_crash_cleanup_retire_call565, label %helper_crash_cleanup_retire_accepted566, label %helper_crash_cleanup_retire_rejected567

helper_crash_cleanup_retire_merge563:             ; preds = %helper_crash_cleanup_retire_accepted566, %bb19
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_29)
  %resource_drop_flag568 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed569 = icmp eq i64 %resource_drop_flag568, 0
  br i1 %resource_drop_not_consumed569, label %resource_drop_live_only570, label %resource_drop_merge571

helper_crash_cleanup_retire_accepted566:          ; preds = %helper_crash_cleanup_retire562
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_retire_merge563

helper_crash_cleanup_retire_rejected567:          ; preds = %helper_crash_cleanup_retire562
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only570:                       ; preds = %helper_crash_cleanup_retire_merge563
  %helper_crash_cleanup_drop_active572 = load i1, ptr %helper_crash_cleanup_active_11, align 1
  br i1 %helper_crash_cleanup_drop_active572, label %helper_crash_cleanup_retire573, label %helper_crash_cleanup_retire_merge574

resource_drop_merge571:                           ; preds = %helper_crash_cleanup_retire_merge574, %helper_crash_cleanup_retire_merge563
  %resource_drop_flag580 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed581 = icmp eq i64 %resource_drop_flag580, 0
  br i1 %resource_drop_not_consumed581, label %resource_drop_live_only582, label %resource_drop_merge583

helper_crash_cleanup_retire573:                   ; preds = %resource_drop_live_only570
  %helper_crash_cleanup_retire_token575 = load i64, ptr %helper_crash_cleanup_token_11, align 8
  %helper_crash_cleanup_retire_call576 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token575)
  br i1 %helper_crash_cleanup_retire_call576, label %helper_crash_cleanup_retire_accepted577, label %helper_crash_cleanup_retire_rejected578

helper_crash_cleanup_retire_merge574:             ; preds = %helper_crash_cleanup_retire_accepted577, %resource_drop_live_only570
  %ref_drop_handle579 = load ptr, ptr %local_11, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle579)
  store ptr null, ptr %local_11, align 4
  br label %resource_drop_merge571

helper_crash_cleanup_retire_accepted577:          ; preds = %helper_crash_cleanup_retire573
  store i64 0, ptr %helper_crash_cleanup_token_11, align 8
  store i1 false, ptr %helper_crash_cleanup_active_11, align 1
  br label %helper_crash_cleanup_retire_merge574

helper_crash_cleanup_retire_rejected578:          ; preds = %helper_crash_cleanup_retire573
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only582:                       ; preds = %resource_drop_merge571
  %helper_crash_cleanup_drop_active584 = load i1, ptr %helper_crash_cleanup_active_8, align 1
  br i1 %helper_crash_cleanup_drop_active584, label %helper_crash_cleanup_retire585, label %helper_crash_cleanup_retire_merge586

resource_drop_merge583:                           ; preds = %helper_crash_cleanup_retire_merge586, %resource_drop_merge571
  %resource_drop_flag592 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed593 = icmp eq i64 %resource_drop_flag592, 0
  br i1 %resource_drop_not_consumed593, label %resource_drop_live_only594, label %resource_drop_merge595

helper_crash_cleanup_retire585:                   ; preds = %resource_drop_live_only582
  %helper_crash_cleanup_retire_token587 = load i64, ptr %helper_crash_cleanup_token_8, align 8
  %helper_crash_cleanup_retire_call588 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token587)
  br i1 %helper_crash_cleanup_retire_call588, label %helper_crash_cleanup_retire_accepted589, label %helper_crash_cleanup_retire_rejected590

helper_crash_cleanup_retire_merge586:             ; preds = %helper_crash_cleanup_retire_accepted589, %resource_drop_live_only582
  %ref_drop_handle591 = load ptr, ptr %local_8, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle591)
  store ptr null, ptr %local_8, align 4
  br label %resource_drop_merge583

helper_crash_cleanup_retire_accepted589:          ; preds = %helper_crash_cleanup_retire585
  store i64 0, ptr %helper_crash_cleanup_token_8, align 8
  store i1 false, ptr %helper_crash_cleanup_active_8, align 1
  br label %helper_crash_cleanup_retire_merge586

helper_crash_cleanup_retire_rejected590:          ; preds = %helper_crash_cleanup_retire585
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only594:                       ; preds = %resource_drop_merge583
  %helper_crash_cleanup_drop_active596 = load i1, ptr %helper_crash_cleanup_active_5, align 1
  br i1 %helper_crash_cleanup_drop_active596, label %helper_crash_cleanup_retire597, label %helper_crash_cleanup_retire_merge598

resource_drop_merge595:                           ; preds = %helper_crash_cleanup_retire_merge598, %resource_drop_merge583
  %resource_drop_flag604 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed605 = icmp eq i64 %resource_drop_flag604, 0
  br i1 %resource_drop_not_consumed605, label %resource_drop_live_only606, label %resource_drop_merge607

helper_crash_cleanup_retire597:                   ; preds = %resource_drop_live_only594
  %helper_crash_cleanup_retire_token599 = load i64, ptr %helper_crash_cleanup_token_5, align 8
  %helper_crash_cleanup_retire_call600 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token599)
  br i1 %helper_crash_cleanup_retire_call600, label %helper_crash_cleanup_retire_accepted601, label %helper_crash_cleanup_retire_rejected602

helper_crash_cleanup_retire_merge598:             ; preds = %helper_crash_cleanup_retire_accepted601, %resource_drop_live_only594
  %ref_drop_handle603 = load ptr, ptr %local_5, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle603)
  store ptr null, ptr %local_5, align 4
  br label %resource_drop_merge595

helper_crash_cleanup_retire_accepted601:          ; preds = %helper_crash_cleanup_retire597
  store i64 0, ptr %helper_crash_cleanup_token_5, align 8
  store i1 false, ptr %helper_crash_cleanup_active_5, align 1
  br label %helper_crash_cleanup_retire_merge598

helper_crash_cleanup_retire_rejected602:          ; preds = %helper_crash_cleanup_retire597
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only606:                       ; preds = %resource_drop_merge595
  %helper_crash_cleanup_drop_active608 = load i1, ptr %helper_crash_cleanup_active_2, align 1
  br i1 %helper_crash_cleanup_drop_active608, label %helper_crash_cleanup_retire609, label %helper_crash_cleanup_retire_merge610

resource_drop_merge607:                           ; preds = %helper_crash_cleanup_retire_merge610, %resource_drop_merge595
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire609:                   ; preds = %resource_drop_live_only606
  %helper_crash_cleanup_retire_token611 = load i64, ptr %helper_crash_cleanup_token_2, align 8
  %helper_crash_cleanup_retire_call612 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token611)
  br i1 %helper_crash_cleanup_retire_call612, label %helper_crash_cleanup_retire_accepted613, label %helper_crash_cleanup_retire_rejected614

helper_crash_cleanup_retire_merge610:             ; preds = %helper_crash_cleanup_retire_accepted613, %resource_drop_live_only606
  %ref_drop_handle615 = load ptr, ptr %local_2, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle615)
  store ptr null, ptr %local_2, align 4
  br label %resource_drop_merge607

helper_crash_cleanup_retire_accepted613:          ; preds = %helper_crash_cleanup_retire609
  store i64 0, ptr %helper_crash_cleanup_token_2, align 8
  store i1 false, ptr %helper_crash_cleanup_active_2, align 1
  br label %helper_crash_cleanup_retire_merge610

helper_crash_cleanup_retire_rejected614:          ; preds = %helper_crash_cleanup_retire609
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

cancel_exit620:                                   ; preds = %bb20
  %helper_crash_cleanup_drop_active622 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_drop_active622, label %helper_crash_cleanup_retire623, label %helper_crash_cleanup_retire_merge624

after_cooperate621:                               ; preds = %bb20
  br label %bb8

helper_crash_cleanup_retire623:                   ; preds = %cancel_exit620
  %helper_crash_cleanup_retire_token625 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_retire_call626 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token625)
  br i1 %helper_crash_cleanup_retire_call626, label %helper_crash_cleanup_retire_accepted627, label %helper_crash_cleanup_retire_rejected628

helper_crash_cleanup_retire_merge624:             ; preds = %helper_crash_cleanup_retire_accepted627, %cancel_exit620
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_29)
  %resource_drop_flag629 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed630 = icmp eq i64 %resource_drop_flag629, 0
  br i1 %resource_drop_not_consumed630, label %resource_drop_live_only631, label %resource_drop_merge632

helper_crash_cleanup_retire_accepted627:          ; preds = %helper_crash_cleanup_retire623
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_retire_merge624

helper_crash_cleanup_retire_rejected628:          ; preds = %helper_crash_cleanup_retire623
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only631:                       ; preds = %helper_crash_cleanup_retire_merge624
  %helper_crash_cleanup_drop_active633 = load i1, ptr %helper_crash_cleanup_active_11, align 1
  br i1 %helper_crash_cleanup_drop_active633, label %helper_crash_cleanup_retire634, label %helper_crash_cleanup_retire_merge635

resource_drop_merge632:                           ; preds = %helper_crash_cleanup_retire_merge635, %helper_crash_cleanup_retire_merge624
  %resource_drop_flag641 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed642 = icmp eq i64 %resource_drop_flag641, 0
  br i1 %resource_drop_not_consumed642, label %resource_drop_live_only643, label %resource_drop_merge644

helper_crash_cleanup_retire634:                   ; preds = %resource_drop_live_only631
  %helper_crash_cleanup_retire_token636 = load i64, ptr %helper_crash_cleanup_token_11, align 8
  %helper_crash_cleanup_retire_call637 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token636)
  br i1 %helper_crash_cleanup_retire_call637, label %helper_crash_cleanup_retire_accepted638, label %helper_crash_cleanup_retire_rejected639

helper_crash_cleanup_retire_merge635:             ; preds = %helper_crash_cleanup_retire_accepted638, %resource_drop_live_only631
  %ref_drop_handle640 = load ptr, ptr %local_11, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle640)
  store ptr null, ptr %local_11, align 4
  br label %resource_drop_merge632

helper_crash_cleanup_retire_accepted638:          ; preds = %helper_crash_cleanup_retire634
  store i64 0, ptr %helper_crash_cleanup_token_11, align 8
  store i1 false, ptr %helper_crash_cleanup_active_11, align 1
  br label %helper_crash_cleanup_retire_merge635

helper_crash_cleanup_retire_rejected639:          ; preds = %helper_crash_cleanup_retire634
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only643:                       ; preds = %resource_drop_merge632
  %helper_crash_cleanup_drop_active645 = load i1, ptr %helper_crash_cleanup_active_8, align 1
  br i1 %helper_crash_cleanup_drop_active645, label %helper_crash_cleanup_retire646, label %helper_crash_cleanup_retire_merge647

resource_drop_merge644:                           ; preds = %helper_crash_cleanup_retire_merge647, %resource_drop_merge632
  %resource_drop_flag653 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed654 = icmp eq i64 %resource_drop_flag653, 0
  br i1 %resource_drop_not_consumed654, label %resource_drop_live_only655, label %resource_drop_merge656

helper_crash_cleanup_retire646:                   ; preds = %resource_drop_live_only643
  %helper_crash_cleanup_retire_token648 = load i64, ptr %helper_crash_cleanup_token_8, align 8
  %helper_crash_cleanup_retire_call649 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token648)
  br i1 %helper_crash_cleanup_retire_call649, label %helper_crash_cleanup_retire_accepted650, label %helper_crash_cleanup_retire_rejected651

helper_crash_cleanup_retire_merge647:             ; preds = %helper_crash_cleanup_retire_accepted650, %resource_drop_live_only643
  %ref_drop_handle652 = load ptr, ptr %local_8, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle652)
  store ptr null, ptr %local_8, align 4
  br label %resource_drop_merge644

helper_crash_cleanup_retire_accepted650:          ; preds = %helper_crash_cleanup_retire646
  store i64 0, ptr %helper_crash_cleanup_token_8, align 8
  store i1 false, ptr %helper_crash_cleanup_active_8, align 1
  br label %helper_crash_cleanup_retire_merge647

helper_crash_cleanup_retire_rejected651:          ; preds = %helper_crash_cleanup_retire646
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only655:                       ; preds = %resource_drop_merge644
  %helper_crash_cleanup_drop_active657 = load i1, ptr %helper_crash_cleanup_active_5, align 1
  br i1 %helper_crash_cleanup_drop_active657, label %helper_crash_cleanup_retire658, label %helper_crash_cleanup_retire_merge659

resource_drop_merge656:                           ; preds = %helper_crash_cleanup_retire_merge659, %resource_drop_merge644
  %resource_drop_flag665 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed666 = icmp eq i64 %resource_drop_flag665, 0
  br i1 %resource_drop_not_consumed666, label %resource_drop_live_only667, label %resource_drop_merge668

helper_crash_cleanup_retire658:                   ; preds = %resource_drop_live_only655
  %helper_crash_cleanup_retire_token660 = load i64, ptr %helper_crash_cleanup_token_5, align 8
  %helper_crash_cleanup_retire_call661 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token660)
  br i1 %helper_crash_cleanup_retire_call661, label %helper_crash_cleanup_retire_accepted662, label %helper_crash_cleanup_retire_rejected663

helper_crash_cleanup_retire_merge659:             ; preds = %helper_crash_cleanup_retire_accepted662, %resource_drop_live_only655
  %ref_drop_handle664 = load ptr, ptr %local_5, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle664)
  store ptr null, ptr %local_5, align 4
  br label %resource_drop_merge656

helper_crash_cleanup_retire_accepted662:          ; preds = %helper_crash_cleanup_retire658
  store i64 0, ptr %helper_crash_cleanup_token_5, align 8
  store i1 false, ptr %helper_crash_cleanup_active_5, align 1
  br label %helper_crash_cleanup_retire_merge659

helper_crash_cleanup_retire_rejected663:          ; preds = %helper_crash_cleanup_retire658
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only667:                       ; preds = %resource_drop_merge656
  %helper_crash_cleanup_drop_active669 = load i1, ptr %helper_crash_cleanup_active_2, align 1
  br i1 %helper_crash_cleanup_drop_active669, label %helper_crash_cleanup_retire670, label %helper_crash_cleanup_retire_merge671

resource_drop_merge668:                           ; preds = %helper_crash_cleanup_retire_merge671, %resource_drop_merge656
  ret i64 0

helper_crash_cleanup_retire670:                   ; preds = %resource_drop_live_only667
  %helper_crash_cleanup_retire_token672 = load i64, ptr %helper_crash_cleanup_token_2, align 8
  %helper_crash_cleanup_retire_call673 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token672)
  br i1 %helper_crash_cleanup_retire_call673, label %helper_crash_cleanup_retire_accepted674, label %helper_crash_cleanup_retire_rejected675

helper_crash_cleanup_retire_merge671:             ; preds = %helper_crash_cleanup_retire_accepted674, %resource_drop_live_only667
  %ref_drop_handle676 = load ptr, ptr %local_2, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle676)
  store ptr null, ptr %local_2, align 4
  br label %resource_drop_merge668

helper_crash_cleanup_retire_accepted674:          ; preds = %helper_crash_cleanup_retire670
  store i64 0, ptr %helper_crash_cleanup_token_2, align 8
  store i1 false, ptr %helper_crash_cleanup_active_2, align 1
  br label %helper_crash_cleanup_retire_merge671

helper_crash_cleanup_retire_rejected675:          ; preds = %helper_crash_cleanup_retire670
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable
}

define internal ptr @expired() {
entry:
  %return_slot = alloca ptr, align 4
  %local_0 = alloca i64, align 8
  %local_1 = alloca ptr, align 4
  %local_2 = alloca ptr, align 4
  %local_3 = alloca i64, align 8
  %local_4 = alloca ptr, align 4
  %helper_crash_cleanup_token_2 = alloca i64, align 8
  store i64 0, ptr %helper_crash_cleanup_token_2, align 8
  %helper_crash_cleanup_active_2 = alloca i1, align 1
  store i1 false, ptr %helper_crash_cleanup_active_2, align 1
  store i64 -1, ptr %local_3, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  store i64 1, ptr %local_0, align 8
  %rc_new = call ptr @hew_rc_new(ptr %local_0, i32 8, i32 8, ptr null)
  store ptr %rc_new, ptr %local_1, align 4
  %helper_crash_cleanup_was_active = load i1, ptr %helper_crash_cleanup_active_2, align 1
  br i1 %helper_crash_cleanup_was_active, label %helper_crash_cleanup_deactivate, label %helper_crash_cleanup_deactivate_merge

helper_crash_cleanup_deactivate:                  ; preds = %bb0
  %helper_crash_cleanup_token = load i64, ptr %helper_crash_cleanup_token_2, align 8
  %helper_crash_cleanup_deactivate_call = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token)
  br i1 %helper_crash_cleanup_deactivate_call, label %helper_crash_cleanup_deactivate_accepted, label %helper_crash_cleanup_deactivate_rejected

helper_crash_cleanup_deactivate_merge:            ; preds = %helper_crash_cleanup_deactivate_accepted, %bb0
  %move_load = load ptr, ptr %local_1, align 4
  store ptr %move_load, ptr %local_2, align 4
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
  %arm_typed_crash_cleanup = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token, ptr %local_2, i64 4, i64 4, ptr @__hew_frame_cleanup_445350923e96a538, i32 1, i32 0)
  %frame_cleanup_arm_failed = icmp eq i64 %arm_typed_crash_cleanup, -1
  br i1 %frame_cleanup_arm_failed, label %frame_cleanup_rejected, label %frame_cleanup_registered

helper_crash_cleanup_guard_merge:                 ; preds = %frame_cleanup_registered, %helper_crash_cleanup_deactivate_merge
  %rc_borrow_handle = load ptr, ptr %local_2, align 4
  %rc_handle_result = call ptr @hew_rc_downgrade(ptr %rc_borrow_handle)
  store ptr %rc_handle_result, ptr %local_4, align 4
  %move_load1 = load ptr, ptr %local_4, align 4
  store ptr %move_load1, ptr %return_slot, align 4
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
  %ref_drop_handle = load ptr, ptr %local_2, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle)
  store ptr null, ptr %local_2, align 4
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
  %ret_val = load ptr, ptr %return_slot, align 4
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

define i64 @__original_main() {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca ptr, align 4
  %local_1 = alloca ptr, align 4
  %local_2 = alloca i64, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca %"Option$$Rc$li64$g", align 8
  %local_5 = alloca i64, align 8
  %local_6 = alloca i64, align 8
  %local_7 = alloca i8, align 1
  %local_8 = alloca i64, align 8
  %local_9 = alloca i8, align 1
  %local_10 = alloca i64, align 8
  %local_11 = alloca i64, align 8
  %local_12 = alloca i64, align 8
  %local_13 = alloca i64, align 8
  %local_14 = alloca i64, align 8
  %local_15 = alloca i8, align 1
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
  store ptr %call_result, ptr %local_0, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %helper_crash_cleanup_was_active = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_was_active, label %helper_crash_cleanup_deactivate, label %helper_crash_cleanup_deactivate_merge

bb2:                                              ; preds = %after_cooperate39, %after_cooperate20
  %helper_crash_cleanup_drop_active = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active, label %helper_crash_cleanup_retire, label %helper_crash_cleanup_retire_merge

bb3:                                              ; preds = %frame_cleanup_registered11
  store i64 9000, ptr %local_10, align 8
  %move_load16 = load i64, ptr %local_10, align 8
  store i64 %move_load16, ptr %local_3, align 8
  %hew_actor_cooperate17 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel18 = icmp eq i32 %hew_actor_cooperate17, 2
  br i1 %hew_cooperate_is_cancel18, label %cancel_exit19, label %after_cooperate20

bb4:                                              ; preds = %bb6
  store i64 1, ptr %local_11, align 8
  %move_load35 = load i64, ptr %local_11, align 8
  store i64 %move_load35, ptr %local_3, align 8
  %hew_actor_cooperate36 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel37 = icmp eq i32 %hew_actor_cooperate36, 2
  br i1 %hew_cooperate_is_cancel37, label %cancel_exit38, label %after_cooperate39

bb5:                                              ; preds = %bb6
  %helper_crash_cleanup_drop_active59 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active59, label %helper_crash_cleanup_retire60, label %helper_crash_cleanup_retire_merge61

bb6:                                              ; preds = %frame_cleanup_registered11
  store i64 1, ptr %local_8, align 8
  %cmp_lhs78 = load i64, ptr %local_5, align 8
  %cmp_rhs79 = load i64, ptr %local_8, align 8
  %cmp_bit80 = icmp eq i64 %cmp_lhs78, %cmp_rhs79
  %cmp_zext81 = zext i1 %cmp_bit80 to i8
  store i8 %cmp_zext81, ptr %local_9, align 1
  %cond_load82 = load i8, ptr %local_9, align 1
  %cond_nz83 = icmp ne i8 %cond_load82, 0
  br i1 %cond_nz83, label %bb4, label %bb5

bb7:                                              ; preds = %helper_crash_cleanup_retire_merge
  %checked_lhs = load i64, ptr %local_13, align 8
  %checked_rhs = load i64, ptr %local_12, align 8
  %with_overflow = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_14, align 8
  store i8 %checked_overflow_widen, ptr %local_15, align 1
  %cond_load84 = load i8, ptr %local_15, align 1
  %cond_nz85 = icmp ne i8 %cond_load84, 0
  br i1 %cond_nz85, label %bb8, label %bb9

bb8:                                              ; preds = %bb7
  %helper_crash_cleanup_drop_active86 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active86, label %helper_crash_cleanup_retire87, label %helper_crash_cleanup_retire_merge88

bb9:                                              ; preds = %bb7
  %move_load105 = load i64, ptr %local_14, align 8
  store i64 %move_load105, ptr %return_slot, align 8
  %helper_crash_cleanup_drop_active106 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active106, label %helper_crash_cleanup_retire107, label %helper_crash_cleanup_retire_merge108

cancel_exit:                                      ; preds = %entry
  ret i64 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

helper_crash_cleanup_deactivate:                  ; preds = %bb1
  %helper_crash_cleanup_token = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_deactivate_call = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token)
  br i1 %helper_crash_cleanup_deactivate_call, label %helper_crash_cleanup_deactivate_accepted, label %helper_crash_cleanup_deactivate_rejected

helper_crash_cleanup_deactivate_merge:            ; preds = %helper_crash_cleanup_deactivate_accepted, %bb1
  %move_load = load ptr, ptr %local_0, align 4
  store ptr %move_load, ptr %local_1, align 4
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
  %arm_typed_crash_cleanup = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token, ptr %local_1, i64 4, i64 4, ptr @__hew_frame_cleanup_7b793acd5045b9cb, i32 1, i32 0)
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
  %weak_upgrade_handle = load ptr, ptr %local_1, align 4
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
  store ptr %weak_upgrade, ptr %machine_variant_field_ptr, align 4
  br label %weak_upgrade_cont

weak_upgrade_none:                                ; preds = %helper_crash_cleanup_deactivate_merge3
  store i8 1, ptr %machine_tag_ptr, align 1
  store ptr null, ptr %machine_variant_field_ptr, align 4
  br label %weak_upgrade_cont

weak_upgrade_cont:                                ; preds = %weak_upgrade_none, %weak_upgrade_some
  %helper_crash_cleanup_prior_token8 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %arm_typed_crash_cleanup9 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token8, ptr %local_4, i64 8, i64 4, ptr @__hew_frame_cleanup_063d4439fd0b791f, i32 1, i32 0)
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
  store %"Option$$Rc$li64$g" zeroinitializer, ptr %local_4, align 4
  %move_load14 = load i64, ptr %local_3, align 8
  store i64 %move_load14, ptr %local_12, align 8
  %call_result15 = call i64 @live()
  store i64 %call_result15, ptr %local_13, align 8
  br label %bb7

helper_crash_cleanup_retire_accepted:             ; preds = %helper_crash_cleanup_retire
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge

helper_crash_cleanup_retire_rejected:             ; preds = %helper_crash_cleanup_retire
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

cancel_exit19:                                    ; preds = %bb3
  %helper_crash_cleanup_drop_active21 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active21, label %helper_crash_cleanup_retire22, label %helper_crash_cleanup_retire_merge23

after_cooperate20:                                ; preds = %bb3
  br label %bb2

helper_crash_cleanup_retire22:                    ; preds = %cancel_exit19
  %helper_crash_cleanup_retire_token24 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call25 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token24)
  br i1 %helper_crash_cleanup_retire_call25, label %helper_crash_cleanup_retire_accepted26, label %helper_crash_cleanup_retire_rejected27

helper_crash_cleanup_retire_merge23:              ; preds = %helper_crash_cleanup_retire_accepted26, %cancel_exit19
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_4)
  %resource_drop_flag = load i64, ptr %local_2, align 8
  %resource_drop_not_consumed = icmp eq i64 %resource_drop_flag, 0
  br i1 %resource_drop_not_consumed, label %resource_drop_live_only, label %resource_drop_merge

helper_crash_cleanup_retire_accepted26:           ; preds = %helper_crash_cleanup_retire22
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge23

helper_crash_cleanup_retire_rejected27:           ; preds = %helper_crash_cleanup_retire22
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only:                          ; preds = %helper_crash_cleanup_retire_merge23
  %helper_crash_cleanup_drop_active28 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active28, label %helper_crash_cleanup_retire29, label %helper_crash_cleanup_retire_merge30

resource_drop_merge:                              ; preds = %helper_crash_cleanup_retire_merge30, %helper_crash_cleanup_retire_merge23
  ret i64 0

helper_crash_cleanup_retire29:                    ; preds = %resource_drop_live_only
  %helper_crash_cleanup_retire_token31 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call32 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token31)
  br i1 %helper_crash_cleanup_retire_call32, label %helper_crash_cleanup_retire_accepted33, label %helper_crash_cleanup_retire_rejected34

helper_crash_cleanup_retire_merge30:              ; preds = %helper_crash_cleanup_retire_accepted33, %resource_drop_live_only
  %ref_drop_handle = load ptr, ptr %local_1, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle)
  store ptr null, ptr %local_1, align 4
  br label %resource_drop_merge

helper_crash_cleanup_retire_accepted33:           ; preds = %helper_crash_cleanup_retire29
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge30

helper_crash_cleanup_retire_rejected34:           ; preds = %helper_crash_cleanup_retire29
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

cancel_exit38:                                    ; preds = %bb4
  %helper_crash_cleanup_drop_active40 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active40, label %helper_crash_cleanup_retire41, label %helper_crash_cleanup_retire_merge42

after_cooperate39:                                ; preds = %bb4
  br label %bb2

helper_crash_cleanup_retire41:                    ; preds = %cancel_exit38
  %helper_crash_cleanup_retire_token43 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call44 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token43)
  br i1 %helper_crash_cleanup_retire_call44, label %helper_crash_cleanup_retire_accepted45, label %helper_crash_cleanup_retire_rejected46

helper_crash_cleanup_retire_merge42:              ; preds = %helper_crash_cleanup_retire_accepted45, %cancel_exit38
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_4)
  %resource_drop_flag47 = load i64, ptr %local_2, align 8
  %resource_drop_not_consumed48 = icmp eq i64 %resource_drop_flag47, 0
  br i1 %resource_drop_not_consumed48, label %resource_drop_live_only49, label %resource_drop_merge50

helper_crash_cleanup_retire_accepted45:           ; preds = %helper_crash_cleanup_retire41
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge42

helper_crash_cleanup_retire_rejected46:           ; preds = %helper_crash_cleanup_retire41
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only49:                        ; preds = %helper_crash_cleanup_retire_merge42
  %helper_crash_cleanup_drop_active51 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active51, label %helper_crash_cleanup_retire52, label %helper_crash_cleanup_retire_merge53

resource_drop_merge50:                            ; preds = %helper_crash_cleanup_retire_merge53, %helper_crash_cleanup_retire_merge42
  ret i64 0

helper_crash_cleanup_retire52:                    ; preds = %resource_drop_live_only49
  %helper_crash_cleanup_retire_token54 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call55 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token54)
  br i1 %helper_crash_cleanup_retire_call55, label %helper_crash_cleanup_retire_accepted56, label %helper_crash_cleanup_retire_rejected57

helper_crash_cleanup_retire_merge53:              ; preds = %helper_crash_cleanup_retire_accepted56, %resource_drop_live_only49
  %ref_drop_handle58 = load ptr, ptr %local_1, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle58)
  store ptr null, ptr %local_1, align 4
  br label %resource_drop_merge50

helper_crash_cleanup_retire_accepted56:           ; preds = %helper_crash_cleanup_retire52
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge53

helper_crash_cleanup_retire_rejected57:           ; preds = %helper_crash_cleanup_retire52
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire60:                    ; preds = %bb5
  %helper_crash_cleanup_retire_token62 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call63 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token62)
  br i1 %helper_crash_cleanup_retire_call63, label %helper_crash_cleanup_retire_accepted64, label %helper_crash_cleanup_retire_rejected65

helper_crash_cleanup_retire_merge61:              ; preds = %helper_crash_cleanup_retire_accepted64, %bb5
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_4)
  %resource_drop_flag66 = load i64, ptr %local_2, align 8
  %resource_drop_not_consumed67 = icmp eq i64 %resource_drop_flag66, 0
  br i1 %resource_drop_not_consumed67, label %resource_drop_live_only68, label %resource_drop_merge69

helper_crash_cleanup_retire_accepted64:           ; preds = %helper_crash_cleanup_retire60
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge61

helper_crash_cleanup_retire_rejected65:           ; preds = %helper_crash_cleanup_retire60
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only68:                        ; preds = %helper_crash_cleanup_retire_merge61
  %helper_crash_cleanup_drop_active70 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active70, label %helper_crash_cleanup_retire71, label %helper_crash_cleanup_retire_merge72

resource_drop_merge69:                            ; preds = %helper_crash_cleanup_retire_merge72, %helper_crash_cleanup_retire_merge61
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire71:                    ; preds = %resource_drop_live_only68
  %helper_crash_cleanup_retire_token73 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call74 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token73)
  br i1 %helper_crash_cleanup_retire_call74, label %helper_crash_cleanup_retire_accepted75, label %helper_crash_cleanup_retire_rejected76

helper_crash_cleanup_retire_merge72:              ; preds = %helper_crash_cleanup_retire_accepted75, %resource_drop_live_only68
  %ref_drop_handle77 = load ptr, ptr %local_1, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle77)
  store ptr null, ptr %local_1, align 4
  br label %resource_drop_merge69

helper_crash_cleanup_retire_accepted75:           ; preds = %helper_crash_cleanup_retire71
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge72

helper_crash_cleanup_retire_rejected76:           ; preds = %helper_crash_cleanup_retire71
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire87:                    ; preds = %bb8
  %helper_crash_cleanup_retire_token89 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call90 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token89)
  br i1 %helper_crash_cleanup_retire_call90, label %helper_crash_cleanup_retire_accepted91, label %helper_crash_cleanup_retire_rejected92

helper_crash_cleanup_retire_merge88:              ; preds = %helper_crash_cleanup_retire_accepted91, %bb8
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_4)
  %resource_drop_flag93 = load i64, ptr %local_2, align 8
  %resource_drop_not_consumed94 = icmp eq i64 %resource_drop_flag93, 0
  br i1 %resource_drop_not_consumed94, label %resource_drop_live_only95, label %resource_drop_merge96

helper_crash_cleanup_retire_accepted91:           ; preds = %helper_crash_cleanup_retire87
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge88

helper_crash_cleanup_retire_rejected92:           ; preds = %helper_crash_cleanup_retire87
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only95:                        ; preds = %helper_crash_cleanup_retire_merge88
  %helper_crash_cleanup_drop_active97 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active97, label %helper_crash_cleanup_retire98, label %helper_crash_cleanup_retire_merge99

resource_drop_merge96:                            ; preds = %helper_crash_cleanup_retire_merge99, %helper_crash_cleanup_retire_merge88
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire98:                    ; preds = %resource_drop_live_only95
  %helper_crash_cleanup_retire_token100 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call101 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token100)
  br i1 %helper_crash_cleanup_retire_call101, label %helper_crash_cleanup_retire_accepted102, label %helper_crash_cleanup_retire_rejected103

helper_crash_cleanup_retire_merge99:              ; preds = %helper_crash_cleanup_retire_accepted102, %resource_drop_live_only95
  %ref_drop_handle104 = load ptr, ptr %local_1, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle104)
  store ptr null, ptr %local_1, align 4
  br label %resource_drop_merge96

helper_crash_cleanup_retire_accepted102:          ; preds = %helper_crash_cleanup_retire98
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge99

helper_crash_cleanup_retire_rejected103:          ; preds = %helper_crash_cleanup_retire98
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire107:                   ; preds = %bb9
  %helper_crash_cleanup_retire_token109 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call110 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token109)
  br i1 %helper_crash_cleanup_retire_call110, label %helper_crash_cleanup_retire_accepted111, label %helper_crash_cleanup_retire_rejected112

helper_crash_cleanup_retire_merge108:             ; preds = %helper_crash_cleanup_retire_accepted111, %bb9
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_4)
  %resource_drop_flag113 = load i64, ptr %local_2, align 8
  %resource_drop_not_consumed114 = icmp eq i64 %resource_drop_flag113, 0
  br i1 %resource_drop_not_consumed114, label %resource_drop_live_only115, label %resource_drop_merge116

helper_crash_cleanup_retire_accepted111:          ; preds = %helper_crash_cleanup_retire107
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge108

helper_crash_cleanup_retire_rejected112:          ; preds = %helper_crash_cleanup_retire107
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

resource_drop_live_only115:                       ; preds = %helper_crash_cleanup_retire_merge108
  %helper_crash_cleanup_drop_active117 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active117, label %helper_crash_cleanup_retire118, label %helper_crash_cleanup_retire_merge119

resource_drop_merge116:                           ; preds = %helper_crash_cleanup_retire_merge119, %helper_crash_cleanup_retire_merge108
  %helper_crash_cleanup_return_token_1 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_return_has_token_1 = icmp ne i64 %helper_crash_cleanup_return_token_1, 0
  br i1 %helper_crash_cleanup_return_has_token_1, label %helper_crash_cleanup_return_retire_1, label %helper_crash_cleanup_return_merge_1

helper_crash_cleanup_retire118:                   ; preds = %resource_drop_live_only115
  %helper_crash_cleanup_retire_token120 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call121 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token120)
  br i1 %helper_crash_cleanup_retire_call121, label %helper_crash_cleanup_retire_accepted122, label %helper_crash_cleanup_retire_rejected123

helper_crash_cleanup_retire_merge119:             ; preds = %helper_crash_cleanup_retire_accepted122, %resource_drop_live_only115
  %ref_drop_handle124 = load ptr, ptr %local_1, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle124)
  store ptr null, ptr %local_1, align 4
  br label %resource_drop_merge116

helper_crash_cleanup_retire_accepted122:          ; preds = %helper_crash_cleanup_retire118
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge119

helper_crash_cleanup_retire_rejected123:          ; preds = %helper_crash_cleanup_retire118
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_return_merge_1:              ; preds = %helper_crash_cleanup_return_retire_1_accepted, %resource_drop_merge116
  %helper_crash_cleanup_return_token_4 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_return_has_token_4 = icmp ne i64 %helper_crash_cleanup_return_token_4, 0
  br i1 %helper_crash_cleanup_return_has_token_4, label %helper_crash_cleanup_return_retire_4, label %helper_crash_cleanup_return_merge_4

helper_crash_cleanup_return_retire_1:             ; preds = %resource_drop_merge116
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
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val

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
}

define internal ptr @"i8::fmt"(i8 %0) {
entry:
  %return_slot = alloca ptr, align 4
  %local_0 = alloca i8, align 1
  %local_1 = alloca i32, align 4
  %local_2 = alloca ptr, align 4
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
  store ptr %call_result, ptr %local_2, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_2, align 4
  store ptr %move_load, ptr %return_slot, align 4
  %ret_val = load ptr, ptr %return_slot, align 4
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"i16::fmt"(i16 %0) {
entry:
  %return_slot = alloca ptr, align 4
  %local_0 = alloca i16, align 2
  %local_1 = alloca i32, align 4
  %local_2 = alloca ptr, align 4
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
  store ptr %call_result, ptr %local_2, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_2, align 4
  store ptr %move_load, ptr %return_slot, align 4
  %ret_val = load ptr, ptr %return_slot, align 4
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"i32::fmt"(i32 %0) {
entry:
  %return_slot = alloca ptr, align 4
  %local_0 = alloca i32, align 4
  %local_1 = alloca ptr, align 4
  store i32 %0, ptr %local_0, align 4
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load i32, ptr %local_0, align 4
  %call_result = call ptr @hew_int_to_string(i32 %call_arg)
  store ptr %call_result, ptr %local_1, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 4
  store ptr %move_load, ptr %return_slot, align 4
  %ret_val = load ptr, ptr %return_slot, align 4
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"i64::fmt"(i64 %0) {
entry:
  %return_slot = alloca ptr, align 4
  %local_0 = alloca i64, align 8
  %local_1 = alloca ptr, align 4
  store i64 %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load i64, ptr %local_0, align 8
  %call_result = call ptr @hew_i64_to_string(i64 %call_arg)
  store ptr %call_result, ptr %local_1, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 4
  store ptr %move_load, ptr %return_slot, align 4
  %ret_val = load ptr, ptr %return_slot, align 4
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"u8::fmt"(i8 %0) {
entry:
  %return_slot = alloca ptr, align 4
  %local_0 = alloca i8, align 1
  %local_1 = alloca ptr, align 4
  store i8 %0, ptr %local_0, align 1
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load i8, ptr %local_0, align 1
  %call_result = call ptr @hew_u8_to_string(i8 %call_arg)
  store ptr %call_result, ptr %local_1, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 4
  store ptr %move_load, ptr %return_slot, align 4
  %ret_val = load ptr, ptr %return_slot, align 4
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"u16::fmt"(i16 %0) {
entry:
  %return_slot = alloca ptr, align 4
  %local_0 = alloca i16, align 2
  %local_1 = alloca ptr, align 4
  store i16 %0, ptr %local_0, align 2
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load i16, ptr %local_0, align 2
  %ffi_zext = zext i16 %call_arg to i32
  %call_result = call ptr @hew_uint_to_string(i32 %ffi_zext)
  store ptr %call_result, ptr %local_1, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 4
  store ptr %move_load, ptr %return_slot, align 4
  %ret_val = load ptr, ptr %return_slot, align 4
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"u32::fmt"(i32 %0) {
entry:
  %return_slot = alloca ptr, align 4
  %local_0 = alloca i32, align 4
  %local_1 = alloca ptr, align 4
  store i32 %0, ptr %local_0, align 4
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load i32, ptr %local_0, align 4
  %call_result = call ptr @hew_uint_to_string(i32 %call_arg)
  store ptr %call_result, ptr %local_1, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 4
  store ptr %move_load, ptr %return_slot, align 4
  %ret_val = load ptr, ptr %return_slot, align 4
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"u64::fmt"(i64 %0) {
entry:
  %return_slot = alloca ptr, align 4
  %local_0 = alloca i64, align 8
  %local_1 = alloca ptr, align 4
  store i64 %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load i64, ptr %local_0, align 8
  %call_result = call ptr @hew_u64_to_string(i64 %call_arg)
  store ptr %call_result, ptr %local_1, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 4
  store ptr %move_load, ptr %return_slot, align 4
  %ret_val = load ptr, ptr %return_slot, align 4
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"isize::fmt"(i32 %0) {
entry:
  %return_slot = alloca ptr, align 4
  %local_0 = alloca i32, align 4
  %local_1 = alloca i64, align 8
  %local_2 = alloca ptr, align 4
  store i32 %0, ptr %local_0, align 4
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %cast_int_src = load i32, ptr %local_0, align 4
  %cast_int_sext = sext i32 %cast_int_src to i64
  store i64 %cast_int_sext, ptr %local_1, align 8
  %call_arg = load i64, ptr %local_1, align 8
  %call_result = call ptr @hew_i64_to_string(i64 %call_arg)
  store ptr %call_result, ptr %local_2, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_2, align 4
  store ptr %move_load, ptr %return_slot, align 4
  %ret_val = load ptr, ptr %return_slot, align 4
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"usize::fmt"(i32 %0) {
entry:
  %return_slot = alloca ptr, align 4
  %local_0 = alloca i32, align 4
  %local_1 = alloca i64, align 8
  %local_2 = alloca ptr, align 4
  store i32 %0, ptr %local_0, align 4
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %cast_int_src = load i32, ptr %local_0, align 4
  %cast_int_zext = zext i32 %cast_int_src to i64
  store i64 %cast_int_zext, ptr %local_1, align 8
  %call_arg = load i64, ptr %local_1, align 8
  %call_result = call ptr @hew_u64_to_string(i64 %call_arg)
  store ptr %call_result, ptr %local_2, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_2, align 4
  store ptr %move_load, ptr %return_slot, align 4
  %ret_val = load ptr, ptr %return_slot, align 4
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"bool::fmt"(i8 %0) {
entry:
  %return_slot = alloca ptr, align 4
  %local_0 = alloca i8, align 1
  %local_1 = alloca ptr, align 4
  store i8 %0, ptr %local_0, align 1
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load i8, ptr %local_0, align 1
  %call_result = call ptr @hew_bool_to_string(i8 %call_arg)
  store ptr %call_result, ptr %local_1, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 4
  store ptr %move_load, ptr %return_slot, align 4
  %ret_val = load ptr, ptr %return_slot, align 4
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"char::fmt"(i32 %0) {
entry:
  %return_slot = alloca ptr, align 4
  %local_0 = alloca i32, align 4
  %local_1 = alloca ptr, align 4
  store i32 %0, ptr %local_0, align 4
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load i32, ptr %local_0, align 4
  %call_result = call ptr @hew_char_to_string(i32 %call_arg)
  store ptr %call_result, ptr %local_1, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 4
  store ptr %move_load, ptr %return_slot, align 4
  %ret_val = load ptr, ptr %return_slot, align 4
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"f64::fmt"(double %0) {
entry:
  %return_slot = alloca ptr, align 4
  %local_0 = alloca double, align 8
  %local_1 = alloca ptr, align 4
  store double %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load double, ptr %local_0, align 8
  %call_result = call ptr @hew_float_to_string(double %call_arg)
  store ptr %call_result, ptr %local_1, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 4
  store ptr %move_load, ptr %return_slot, align 4
  %ret_val = load ptr, ptr %return_slot, align 4
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"f32::fmt"(float %0) {
entry:
  %return_slot = alloca ptr, align 4
  %local_0 = alloca float, align 4
  %local_1 = alloca double, align 8
  %local_2 = alloca ptr, align 4
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
  store ptr %call_result, ptr %local_2, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_2, align 4
  store ptr %move_load, ptr %return_slot, align 4
  %ret_val = load ptr, ptr %return_slot, align 4
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"string::fmt"(ptr %0) {
entry:
  %return_slot = alloca ptr, align 4
  %local_0 = alloca ptr, align 4
  store ptr %0, ptr %local_0, align 4
  br label %bb0

bb0:                                              ; preds = %entry
  %mir_share_string_load = load ptr, ptr %local_0, align 4
  %mir_share_string_retain = call ptr @hew_string_clone(ptr %mir_share_string_load)
  %move_load = load ptr, ptr %local_0, align 4
  store ptr %move_load, ptr %return_slot, align 4
  %ret_val = load ptr, ptr %return_slot, align 4
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
  %return_slot = alloca ptr, align 4
  %local_0 = alloca i64, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca ptr, align 4
  %local_3 = alloca ptr, align 4
  %local_4 = alloca ptr, align 4
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
  store ptr %call_result2, ptr %local_2, align 4
  br label %bb2

bb2:                                              ; preds = %bb1
  store ptr @str_lit, ptr %local_3, align 4
  %"hew_string_concat arg0" = load ptr, ptr %local_2, align 4
  %"hew_string_concat arg1" = load ptr, ptr %local_3, align 4
  %hew_string_concat_call = call ptr @hew_string_concat(ptr %"hew_string_concat arg0", ptr %"hew_string_concat arg1")
  store ptr %hew_string_concat_call, ptr %local_4, align 4
  %"hew_string_drop drop" = load ptr, ptr %local_2, align 4
  call void @hew_string_drop(ptr %"hew_string_drop drop")
  store ptr null, ptr %local_2, align 4
  %move_load = load ptr, ptr %local_4, align 4
  store ptr %move_load, ptr %return_slot, align 4
  %ret_val = load ptr, ptr %return_slot, align 4
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define i64 @main() {
entry:
  %__original_main_call = call i64 @__original_main()
  ret i64 %__original_main_call
}

define i32 @__hew_wasi_main() {
entry:
  %hew_source_main_call = call i64 @__original_main()
  %wasi_exit_trunc = trunc i64 %hew_source_main_call to i32
  ret i32 %wasi_exit_trunc
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
  %src_f0 = load ptr, ptr %src_f0_ptr, align 4
  %clone_helper_f0 = call ptr @hew_rc_clone(ptr %src_f0)
  %cloned_f0_int = ptrtoint ptr %clone_helper_f0 to i64
  %cloned_f0_null = icmp eq i64 %cloned_f0_int, 0
  br i1 %cloned_f0_null, label %enum_clone_v0_rb_0, label %enum_clone_v0_store_0

enum_clone_v0_store_0:                            ; preds = %enum_clone_v0_step_0
  %dst_f0_ptr = getelementptr inbounds nuw { ptr }, ptr %enum_clone_dst_payload_0, i32 0, i32 0
  store ptr %clone_helper_f0, ptr %dst_f0_ptr, align 4
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
  %drop_rc_f0 = load ptr, ptr %drop_rc_f0_ptr, align 4
  call void @hew_rc_drop(ptr %drop_rc_f0)
  store ptr null, ptr %drop_rc_f0_ptr, align 4
  br label %done

enum_drop_variant_1:                              ; preds = %entry
  %enum_drop_payload_1 = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %0, i32 0, i32 1
  br label %done
}

declare void @hew_rc_drop(ptr)

define internal void @"__hew_enum_overwrite_release_Option$$Rc$li64$g"(ptr %0, ptr %1) {
entry:
  %ow_slot_0 = alloca ptr, align 4
  store ptr null, ptr %ow_slot_0, align 4
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
  %ow_new_d0_f0_leaf = load ptr, ptr %ow_new_d0_f0_ptr, align 4
  store ptr %ow_new_d0_f0_leaf, ptr %ow_slot_0, align 4
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
  %ow_old_d0_f0_val = load ptr, ptr %ow_old_d0_f0_ptr, align 4
  %ow_old_d0_f0_int = ptrtoint ptr %ow_old_d0_f0_val to i64
  %ow_old_d0_f0_cmp0_leaf = load ptr, ptr %ow_slot_0, align 4
  %ow_old_d0_f0_cmp0_int = ptrtoint ptr %ow_old_d0_f0_cmp0_leaf to i64
  %ow_old_d0_f0_cmp0_eq = icmp eq i64 %ow_old_d0_f0_int, %ow_old_d0_f0_cmp0_int
  %ow_old_d0_f0_matched0 = or i1 false, %ow_old_d0_f0_cmp0_eq
  %ow_old_d0_f0_neutralized = select i1 %ow_old_d0_f0_matched0, ptr null, ptr %ow_old_d0_f0_val
  store ptr %ow_old_d0_f0_neutralized, ptr %ow_old_d0_f0_ptr, align 4
  br label %"ow_old_d0_Option$$Rc$li64$g_merge"

"ow_old_d0_Option$$Rc$li64$g_v1":                 ; preds = %"ow_new_d0_Option$$Rc$li64$g_merge"
  %"ow_old_d0_Option$$Rc$li64$g_v1_payload" = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %0, i32 0, i32 1
  br label %"ow_old_d0_Option$$Rc$li64$g_merge"
}

declare i32 @hew_actor_cooperate()

declare ptr @hew_rc_new(ptr, i32, i32, ptr)

declare i1 @hew_cont_crash_cleanup_deactivate(i64)

define internal void @__hew_frame_cleanup_445350923e96a538(ptr %0) {
entry:
  %ref_drop_handle = load ptr, ptr %0, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle)
  store ptr null, ptr %0, align 4
  ret void
}

declare i64 @hew_cont_crash_cleanup_arm(i64, ptr, i64, i64, ptr, i32, i32)

declare ptr @hew_rc_downgrade(ptr)

define internal void @__hew_frame_cleanup_7b793acd5045b9cb(ptr %0) {
entry:
  %ref_drop_handle = load ptr, ptr %0, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle)
  store ptr null, ptr %0, align 4
  ret void
}

declare void @hew_weak_drop_rc(ptr)

declare ptr @hew_weak_clone_rc(ptr)

declare i32 @hew_rc_strong_count(ptr)

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.smul.with.overflow.i64(i64, i64) #1

declare i1 @hew_cont_crash_cleanup_retire(i64)

declare i32 @hew_rc_weak_count(ptr)

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64) #1

declare i32 @hew_rc_is_unique(ptr)

declare ptr @hew_weak_upgrade_rc(ptr)

define internal void @__hew_frame_cleanup_063d4439fd0b791f(ptr %0) {
entry:
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %0)
  ret void
}

declare void @hew_rc_set(ptr, ptr)

declare ptr @hew_rc_get(ptr)

declare void @hew_string_drop(ptr)

attributes #0 = { cold noreturn nounwind memory(inaccessiblemem: write) }
attributes #1 = { nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none) }
