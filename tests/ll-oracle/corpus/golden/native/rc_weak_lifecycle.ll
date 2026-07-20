; ModuleID = 'rc_weak_lifecycle'
source_filename = "rc_weak_lifecycle"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "aarch64-apple-macosx13.0"

%"Option$$Rc$li64$g" = type { i8, [1 x i64] }
%CrashInfo = type { i64, ptr }

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

declare i64 @hew_remote_pid_from_raw(i64, i64)

declare i64 @hew_node_api_lookup(ptr)

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
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  store i64 7, ptr %local_0, align 8
  %rc_new = call ptr @hew_rc_new(ptr %local_0, i64 8, i64 8, ptr null)
  store ptr %rc_new, ptr %local_1, align 8
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %local_2, align 8
  store i64 0, ptr %local_3, align 8
  %rc_borrow_handle = load ptr, ptr %local_2, align 8
  %rc_handle_result = call ptr @hew_rc_clone(ptr %rc_borrow_handle)
  store ptr %rc_handle_result, ptr %local_4, align 8
  %move_load1 = load ptr, ptr %local_4, align 8
  store ptr %move_load1, ptr %local_5, align 8
  store i64 0, ptr %local_6, align 8
  %rc_borrow_handle2 = load ptr, ptr %local_2, align 8
  %rc_handle_result3 = call ptr @hew_rc_downgrade(ptr %rc_borrow_handle2)
  store ptr %rc_handle_result3, ptr %local_7, align 8
  %move_load4 = load ptr, ptr %local_7, align 8
  store ptr %move_load4, ptr %local_8, align 8
  store i64 0, ptr %local_9, align 8
  %rc_borrow_handle5 = load ptr, ptr %local_8, align 8
  %rc_handle_result6 = call ptr @hew_weak_clone_rc(ptr %rc_borrow_handle5)
  store ptr %rc_handle_result6, ptr %local_10, align 8
  %move_load7 = load ptr, ptr %local_10, align 8
  store ptr %move_load7, ptr %local_11, align 8
  store i64 0, ptr %local_12, align 8
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

bb1:                                              ; preds = %bb0
  %resource_drop_flag = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed = icmp eq i64 %resource_drop_flag, 0
  br i1 %resource_drop_not_consumed, label %resource_drop_live_only, label %resource_drop_merge

bb2:                                              ; preds = %bb0
  %rc_count_handle23 = load ptr, ptr %local_2, align 8
  %rc_count24 = call i64 @hew_rc_weak_count(ptr %rc_count_handle23)
  store i64 %rc_count24, ptr %local_17, align 8
  %checked_lhs25 = load i64, ptr %local_15, align 8
  %checked_rhs26 = load i64, ptr %local_17, align 8
  %with_overflow27 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs25, i64 %checked_rhs26)
  %checked_result28 = extractvalue { i64, i1 } %with_overflow27, 0
  %checked_overflow29 = extractvalue { i64, i1 } %with_overflow27, 1
  %checked_overflow_widen30 = zext i1 %checked_overflow29 to i8
  store i64 %checked_result28, ptr %local_18, align 8
  store i8 %checked_overflow_widen30, ptr %local_19, align 1
  %cond_load31 = load i8, ptr %local_19, align 1
  %cond_nz32 = icmp ne i8 %cond_load31, 0
  br i1 %cond_nz32, label %bb3, label %bb4

bb3:                                              ; preds = %bb2
  %resource_drop_flag33 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed34 = icmp eq i64 %resource_drop_flag33, 0
  br i1 %resource_drop_not_consumed34, label %resource_drop_live_only35, label %resource_drop_merge36

bb4:                                              ; preds = %bb2
  %move_load53 = load i64, ptr %local_18, align 8
  store i64 %move_load53, ptr %local_20, align 8
  %rc_is_unique_handle = load ptr, ptr %local_2, align 8
  %rc_is_unique = call i32 @hew_rc_is_unique(ptr %rc_is_unique_handle)
  %rc_unique_bit = icmp ne i32 %rc_is_unique, 0
  %rc_unique_bool = zext i1 %rc_unique_bit to i8
  store i8 %rc_unique_bool, ptr %local_22, align 1
  %cond_load54 = load i8, ptr %local_22, align 1
  %cond_nz55 = icmp ne i8 %cond_load54, 0
  br i1 %cond_nz55, label %bb5, label %bb6

bb5:                                              ; preds = %bb4
  store i64 90, ptr %local_23, align 8
  %move_load56 = load i64, ptr %local_23, align 8
  store i64 %move_load56, ptr %local_24, align 8
  %move_load57 = load i64, ptr %local_24, align 8
  store i64 %move_load57, ptr %local_21, align 8
  br label %bb7

bb6:                                              ; preds = %bb4
  store i64 1, ptr %local_25, align 8
  %move_load58 = load i64, ptr %local_25, align 8
  store i64 %move_load58, ptr %local_26, align 8
  %move_load59 = load i64, ptr %local_26, align 8
  store i64 %move_load59, ptr %local_21, align 8
  br label %bb7

bb7:                                              ; preds = %bb6, %bb5
  %move_load60 = load i64, ptr %local_21, align 8
  store i64 %move_load60, ptr %local_27, align 8
  %weak_upgrade_handle = load ptr, ptr %local_8, align 8
  %weak_upgrade = call ptr @hew_weak_upgrade_rc(ptr %weak_upgrade_handle)
  %machine_tag_ptr = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %local_29, i32 0, i32 0
  %machine_payload_ptr = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %local_29, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { ptr }, ptr %machine_payload_ptr, i32 0, i32 0
  %weak_upgrade_is_none = icmp eq ptr %weak_upgrade, null
  br i1 %weak_upgrade_is_none, label %weak_upgrade_none, label %weak_upgrade_some

bb8:                                              ; preds = %after_cooperate262, %after_cooperate102
  %move_load64 = load i64, ptr %local_28, align 8
  store i64 %move_load64, ptr %return_slot, align 8
  %resource_drop_flag65 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed66 = icmp eq i64 %resource_drop_flag65, 0
  br i1 %resource_drop_not_consumed66, label %resource_drop_live_only67, label %resource_drop_merge68

bb9:                                              ; preds = %weak_upgrade_cont
  %machine_payload_ptr85 = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %local_29, i32 0, i32 1
  %machine_variant_field_ptr86 = getelementptr inbounds nuw { ptr }, ptr %machine_payload_ptr85, i32 0, i32 0
  %move_load87 = load ptr, ptr %machine_variant_field_ptr86, align 8
  store ptr %move_load87, ptr %local_35, align 8
  store i64 9, ptr %local_36, align 8
  %rc_set_handle = load ptr, ptr %local_2, align 8
  call void @hew_rc_set(ptr %rc_set_handle, ptr %local_36)
  store i8 0, ptr %local_37, align 1
  %rc_count_handle88 = load ptr, ptr %local_35, align 8
  %rc_count89 = call i64 @hew_rc_strong_count(ptr %rc_count_handle88)
  store i64 %rc_count89, ptr %local_38, align 8
  store i64 10, ptr %local_39, align 8
  %checked_lhs90 = load i64, ptr %local_38, align 8
  %checked_rhs91 = load i64, ptr %local_39, align 8
  %with_overflow92 = call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %checked_lhs90, i64 %checked_rhs91)
  %checked_result93 = extractvalue { i64, i1 } %with_overflow92, 0
  %checked_overflow94 = extractvalue { i64, i1 } %with_overflow92, 1
  %checked_overflow_widen95 = zext i1 %checked_overflow94 to i8
  store i64 %checked_result93, ptr %local_40, align 8
  store i8 %checked_overflow_widen95, ptr %local_41, align 1
  %cond_load96 = load i8, ptr %local_41, align 1
  %cond_nz97 = icmp ne i8 %cond_load96, 0
  br i1 %cond_nz97, label %bb13, label %bb14

bb10:                                             ; preds = %bb12
  store i64 9000, ptr %local_50, align 8
  %move_load98 = load i64, ptr %local_50, align 8
  store i64 %move_load98, ptr %local_28, align 8
  %hew_actor_cooperate99 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel100 = icmp eq i32 %hew_actor_cooperate99, 2
  br i1 %hew_cooperate_is_cancel100, label %cancel_exit101, label %after_cooperate102

bb11:                                             ; preds = %bb12
  %resource_drop_flag123 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed124 = icmp eq i64 %resource_drop_flag123, 0
  br i1 %resource_drop_not_consumed124, label %resource_drop_live_only125, label %resource_drop_merge126

bb12:                                             ; preds = %weak_upgrade_cont
  store i64 1, ptr %local_33, align 8
  %cmp_lhs143 = load i64, ptr %local_30, align 8
  %cmp_rhs144 = load i64, ptr %local_33, align 8
  %cmp_bit145 = icmp eq i64 %cmp_lhs143, %cmp_rhs144
  %cmp_zext146 = zext i1 %cmp_bit145 to i8
  store i8 %cmp_zext146, ptr %local_34, align 1
  %cond_load147 = load i8, ptr %local_34, align 1
  %cond_nz148 = icmp ne i8 %cond_load147, 0
  br i1 %cond_nz148, label %bb10, label %bb11

bb13:                                             ; preds = %bb9
  %ref_drop_handle149 = load ptr, ptr %local_35, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle149)
  store ptr null, ptr %local_35, align 8
  %resource_drop_flag150 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed151 = icmp eq i64 %resource_drop_flag150, 0
  br i1 %resource_drop_not_consumed151, label %resource_drop_live_only152, label %resource_drop_merge153

bb14:                                             ; preds = %bb9
  %checked_lhs170 = load i64, ptr %local_20, align 8
  %checked_rhs171 = load i64, ptr %local_40, align 8
  %with_overflow172 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs170, i64 %checked_rhs171)
  %checked_result173 = extractvalue { i64, i1 } %with_overflow172, 0
  %checked_overflow174 = extractvalue { i64, i1 } %with_overflow172, 1
  %checked_overflow_widen175 = zext i1 %checked_overflow174 to i8
  store i64 %checked_result173, ptr %local_42, align 8
  store i8 %checked_overflow_widen175, ptr %local_43, align 1
  %cond_load176 = load i8, ptr %local_43, align 1
  %cond_nz177 = icmp ne i8 %cond_load176, 0
  br i1 %cond_nz177, label %bb15, label %bb16

bb15:                                             ; preds = %bb14
  %ref_drop_handle178 = load ptr, ptr %local_35, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle178)
  store ptr null, ptr %local_35, align 8
  %resource_drop_flag179 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed180 = icmp eq i64 %resource_drop_flag179, 0
  br i1 %resource_drop_not_consumed180, label %resource_drop_live_only181, label %resource_drop_merge182

bb16:                                             ; preds = %bb14
  %rc_get_handle = load ptr, ptr %local_5, align 8
  %rc_get_data = call ptr @hew_rc_get(ptr %rc_get_handle)
  %rc_get_value = load i64, ptr %rc_get_data, align 8
  store i64 %rc_get_value, ptr %local_44, align 8
  %checked_lhs199 = load i64, ptr %local_42, align 8
  %checked_rhs200 = load i64, ptr %local_44, align 8
  %with_overflow201 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs199, i64 %checked_rhs200)
  %checked_result202 = extractvalue { i64, i1 } %with_overflow201, 0
  %checked_overflow203 = extractvalue { i64, i1 } %with_overflow201, 1
  %checked_overflow_widen204 = zext i1 %checked_overflow203 to i8
  store i64 %checked_result202, ptr %local_45, align 8
  store i8 %checked_overflow_widen204, ptr %local_46, align 1
  %cond_load205 = load i8, ptr %local_46, align 1
  %cond_nz206 = icmp ne i8 %cond_load205, 0
  br i1 %cond_nz206, label %bb17, label %bb18

bb17:                                             ; preds = %bb16
  %ref_drop_handle207 = load ptr, ptr %local_35, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle207)
  store ptr null, ptr %local_35, align 8
  %resource_drop_flag208 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed209 = icmp eq i64 %resource_drop_flag208, 0
  br i1 %resource_drop_not_consumed209, label %resource_drop_live_only210, label %resource_drop_merge211

bb18:                                             ; preds = %bb16
  %checked_lhs228 = load i64, ptr %local_45, align 8
  %checked_rhs229 = load i64, ptr %local_27, align 8
  %with_overflow230 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs228, i64 %checked_rhs229)
  %checked_result231 = extractvalue { i64, i1 } %with_overflow230, 0
  %checked_overflow232 = extractvalue { i64, i1 } %with_overflow230, 1
  %checked_overflow_widen233 = zext i1 %checked_overflow232 to i8
  store i64 %checked_result231, ptr %local_47, align 8
  store i8 %checked_overflow_widen233, ptr %local_48, align 1
  %cond_load234 = load i8, ptr %local_48, align 1
  %cond_nz235 = icmp ne i8 %cond_load234, 0
  br i1 %cond_nz235, label %bb19, label %bb20

bb19:                                             ; preds = %bb18
  %ref_drop_handle236 = load ptr, ptr %local_35, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle236)
  store ptr null, ptr %local_35, align 8
  %resource_drop_flag237 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed238 = icmp eq i64 %resource_drop_flag237, 0
  br i1 %resource_drop_not_consumed238, label %resource_drop_live_only239, label %resource_drop_merge240

bb20:                                             ; preds = %bb18
  %move_load257 = load i64, ptr %local_47, align 8
  store i64 %move_load257, ptr %local_49, align 8
  %move_load258 = load i64, ptr %local_49, align 8
  store i64 %move_load258, ptr %local_28, align 8
  %hew_actor_cooperate259 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel260 = icmp eq i32 %hew_actor_cooperate259, 2
  br i1 %hew_cooperate_is_cancel260, label %cancel_exit261, label %after_cooperate262

cancel_exit:                                      ; preds = %entry
  ret i64 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

resource_drop_live_only:                          ; preds = %bb1
  %ref_drop_handle = load ptr, ptr %local_11, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle)
  store ptr null, ptr %local_11, align 8
  br label %resource_drop_merge

resource_drop_merge:                              ; preds = %resource_drop_live_only, %bb1
  %resource_drop_flag8 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed9 = icmp eq i64 %resource_drop_flag8, 0
  br i1 %resource_drop_not_consumed9, label %resource_drop_live_only10, label %resource_drop_merge11

resource_drop_live_only10:                        ; preds = %resource_drop_merge
  %ref_drop_handle12 = load ptr, ptr %local_8, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle12)
  store ptr null, ptr %local_8, align 8
  br label %resource_drop_merge11

resource_drop_merge11:                            ; preds = %resource_drop_live_only10, %resource_drop_merge
  %resource_drop_flag13 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed14 = icmp eq i64 %resource_drop_flag13, 0
  br i1 %resource_drop_not_consumed14, label %resource_drop_live_only15, label %resource_drop_merge16

resource_drop_live_only15:                        ; preds = %resource_drop_merge11
  %ref_drop_handle17 = load ptr, ptr %local_5, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle17)
  store ptr null, ptr %local_5, align 8
  br label %resource_drop_merge16

resource_drop_merge16:                            ; preds = %resource_drop_live_only15, %resource_drop_merge11
  %resource_drop_flag18 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed19 = icmp eq i64 %resource_drop_flag18, 0
  br i1 %resource_drop_not_consumed19, label %resource_drop_live_only20, label %resource_drop_merge21

resource_drop_live_only20:                        ; preds = %resource_drop_merge16
  %ref_drop_handle22 = load ptr, ptr %local_2, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle22)
  store ptr null, ptr %local_2, align 8
  br label %resource_drop_merge21

resource_drop_merge21:                            ; preds = %resource_drop_live_only20, %resource_drop_merge16
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

resource_drop_live_only35:                        ; preds = %bb3
  %ref_drop_handle37 = load ptr, ptr %local_11, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle37)
  store ptr null, ptr %local_11, align 8
  br label %resource_drop_merge36

resource_drop_merge36:                            ; preds = %resource_drop_live_only35, %bb3
  %resource_drop_flag38 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed39 = icmp eq i64 %resource_drop_flag38, 0
  br i1 %resource_drop_not_consumed39, label %resource_drop_live_only40, label %resource_drop_merge41

resource_drop_live_only40:                        ; preds = %resource_drop_merge36
  %ref_drop_handle42 = load ptr, ptr %local_8, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle42)
  store ptr null, ptr %local_8, align 8
  br label %resource_drop_merge41

resource_drop_merge41:                            ; preds = %resource_drop_live_only40, %resource_drop_merge36
  %resource_drop_flag43 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed44 = icmp eq i64 %resource_drop_flag43, 0
  br i1 %resource_drop_not_consumed44, label %resource_drop_live_only45, label %resource_drop_merge46

resource_drop_live_only45:                        ; preds = %resource_drop_merge41
  %ref_drop_handle47 = load ptr, ptr %local_5, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle47)
  store ptr null, ptr %local_5, align 8
  br label %resource_drop_merge46

resource_drop_merge46:                            ; preds = %resource_drop_live_only45, %resource_drop_merge41
  %resource_drop_flag48 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed49 = icmp eq i64 %resource_drop_flag48, 0
  br i1 %resource_drop_not_consumed49, label %resource_drop_live_only50, label %resource_drop_merge51

resource_drop_live_only50:                        ; preds = %resource_drop_merge46
  %ref_drop_handle52 = load ptr, ptr %local_2, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle52)
  store ptr null, ptr %local_2, align 8
  br label %resource_drop_merge51

resource_drop_merge51:                            ; preds = %resource_drop_live_only50, %resource_drop_merge46
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

weak_upgrade_some:                                ; preds = %bb7
  store i8 0, ptr %machine_tag_ptr, align 1
  store ptr %weak_upgrade, ptr %machine_variant_field_ptr, align 8
  br label %weak_upgrade_cont

weak_upgrade_none:                                ; preds = %bb7
  store i8 1, ptr %machine_tag_ptr, align 1
  store ptr null, ptr %machine_variant_field_ptr, align 8
  br label %weak_upgrade_cont

weak_upgrade_cont:                                ; preds = %weak_upgrade_none, %weak_upgrade_some
  %machine_tag_ptr61 = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %local_29, i32 0, i32 0
  %move_iN_load = load i8, ptr %machine_tag_ptr61, align 1
  %move_iN_zext = zext i8 %move_iN_load to i64
  store i64 %move_iN_zext, ptr %local_30, align 8
  store i64 0, ptr %local_31, align 8
  %cmp_lhs = load i64, ptr %local_30, align 8
  %cmp_rhs = load i64, ptr %local_31, align 8
  %cmp_bit = icmp eq i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_32, align 1
  %cond_load62 = load i8, ptr %local_32, align 1
  %cond_nz63 = icmp ne i8 %cond_load62, 0
  br i1 %cond_nz63, label %bb9, label %bb12

resource_drop_live_only67:                        ; preds = %bb8
  %ref_drop_handle69 = load ptr, ptr %local_11, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle69)
  store ptr null, ptr %local_11, align 8
  br label %resource_drop_merge68

resource_drop_merge68:                            ; preds = %resource_drop_live_only67, %bb8
  %resource_drop_flag70 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed71 = icmp eq i64 %resource_drop_flag70, 0
  br i1 %resource_drop_not_consumed71, label %resource_drop_live_only72, label %resource_drop_merge73

resource_drop_live_only72:                        ; preds = %resource_drop_merge68
  %ref_drop_handle74 = load ptr, ptr %local_8, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle74)
  store ptr null, ptr %local_8, align 8
  br label %resource_drop_merge73

resource_drop_merge73:                            ; preds = %resource_drop_live_only72, %resource_drop_merge68
  %resource_drop_flag75 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed76 = icmp eq i64 %resource_drop_flag75, 0
  br i1 %resource_drop_not_consumed76, label %resource_drop_live_only77, label %resource_drop_merge78

resource_drop_live_only77:                        ; preds = %resource_drop_merge73
  %ref_drop_handle79 = load ptr, ptr %local_5, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle79)
  store ptr null, ptr %local_5, align 8
  br label %resource_drop_merge78

resource_drop_merge78:                            ; preds = %resource_drop_live_only77, %resource_drop_merge73
  %resource_drop_flag80 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed81 = icmp eq i64 %resource_drop_flag80, 0
  br i1 %resource_drop_not_consumed81, label %resource_drop_live_only82, label %resource_drop_merge83

resource_drop_live_only82:                        ; preds = %resource_drop_merge78
  %ref_drop_handle84 = load ptr, ptr %local_2, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle84)
  store ptr null, ptr %local_2, align 8
  br label %resource_drop_merge83

resource_drop_merge83:                            ; preds = %resource_drop_live_only82, %resource_drop_merge78
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val

cancel_exit101:                                   ; preds = %bb10
  %resource_drop_flag103 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed104 = icmp eq i64 %resource_drop_flag103, 0
  br i1 %resource_drop_not_consumed104, label %resource_drop_live_only105, label %resource_drop_merge106

after_cooperate102:                               ; preds = %bb10
  br label %bb8

resource_drop_live_only105:                       ; preds = %cancel_exit101
  %ref_drop_handle107 = load ptr, ptr %local_11, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle107)
  store ptr null, ptr %local_11, align 8
  br label %resource_drop_merge106

resource_drop_merge106:                           ; preds = %resource_drop_live_only105, %cancel_exit101
  %resource_drop_flag108 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed109 = icmp eq i64 %resource_drop_flag108, 0
  br i1 %resource_drop_not_consumed109, label %resource_drop_live_only110, label %resource_drop_merge111

resource_drop_live_only110:                       ; preds = %resource_drop_merge106
  %ref_drop_handle112 = load ptr, ptr %local_8, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle112)
  store ptr null, ptr %local_8, align 8
  br label %resource_drop_merge111

resource_drop_merge111:                           ; preds = %resource_drop_live_only110, %resource_drop_merge106
  %resource_drop_flag113 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed114 = icmp eq i64 %resource_drop_flag113, 0
  br i1 %resource_drop_not_consumed114, label %resource_drop_live_only115, label %resource_drop_merge116

resource_drop_live_only115:                       ; preds = %resource_drop_merge111
  %ref_drop_handle117 = load ptr, ptr %local_5, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle117)
  store ptr null, ptr %local_5, align 8
  br label %resource_drop_merge116

resource_drop_merge116:                           ; preds = %resource_drop_live_only115, %resource_drop_merge111
  %resource_drop_flag118 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed119 = icmp eq i64 %resource_drop_flag118, 0
  br i1 %resource_drop_not_consumed119, label %resource_drop_live_only120, label %resource_drop_merge121

resource_drop_live_only120:                       ; preds = %resource_drop_merge116
  %ref_drop_handle122 = load ptr, ptr %local_2, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle122)
  store ptr null, ptr %local_2, align 8
  br label %resource_drop_merge121

resource_drop_merge121:                           ; preds = %resource_drop_live_only120, %resource_drop_merge116
  ret i64 0

resource_drop_live_only125:                       ; preds = %bb11
  %ref_drop_handle127 = load ptr, ptr %local_11, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle127)
  store ptr null, ptr %local_11, align 8
  br label %resource_drop_merge126

resource_drop_merge126:                           ; preds = %resource_drop_live_only125, %bb11
  %resource_drop_flag128 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed129 = icmp eq i64 %resource_drop_flag128, 0
  br i1 %resource_drop_not_consumed129, label %resource_drop_live_only130, label %resource_drop_merge131

resource_drop_live_only130:                       ; preds = %resource_drop_merge126
  %ref_drop_handle132 = load ptr, ptr %local_8, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle132)
  store ptr null, ptr %local_8, align 8
  br label %resource_drop_merge131

resource_drop_merge131:                           ; preds = %resource_drop_live_only130, %resource_drop_merge126
  %resource_drop_flag133 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed134 = icmp eq i64 %resource_drop_flag133, 0
  br i1 %resource_drop_not_consumed134, label %resource_drop_live_only135, label %resource_drop_merge136

resource_drop_live_only135:                       ; preds = %resource_drop_merge131
  %ref_drop_handle137 = load ptr, ptr %local_5, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle137)
  store ptr null, ptr %local_5, align 8
  br label %resource_drop_merge136

resource_drop_merge136:                           ; preds = %resource_drop_live_only135, %resource_drop_merge131
  %resource_drop_flag138 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed139 = icmp eq i64 %resource_drop_flag138, 0
  br i1 %resource_drop_not_consumed139, label %resource_drop_live_only140, label %resource_drop_merge141

resource_drop_live_only140:                       ; preds = %resource_drop_merge136
  %ref_drop_handle142 = load ptr, ptr %local_2, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle142)
  store ptr null, ptr %local_2, align 8
  br label %resource_drop_merge141

resource_drop_merge141:                           ; preds = %resource_drop_live_only140, %resource_drop_merge136
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

resource_drop_live_only152:                       ; preds = %bb13
  %ref_drop_handle154 = load ptr, ptr %local_11, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle154)
  store ptr null, ptr %local_11, align 8
  br label %resource_drop_merge153

resource_drop_merge153:                           ; preds = %resource_drop_live_only152, %bb13
  %resource_drop_flag155 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed156 = icmp eq i64 %resource_drop_flag155, 0
  br i1 %resource_drop_not_consumed156, label %resource_drop_live_only157, label %resource_drop_merge158

resource_drop_live_only157:                       ; preds = %resource_drop_merge153
  %ref_drop_handle159 = load ptr, ptr %local_8, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle159)
  store ptr null, ptr %local_8, align 8
  br label %resource_drop_merge158

resource_drop_merge158:                           ; preds = %resource_drop_live_only157, %resource_drop_merge153
  %resource_drop_flag160 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed161 = icmp eq i64 %resource_drop_flag160, 0
  br i1 %resource_drop_not_consumed161, label %resource_drop_live_only162, label %resource_drop_merge163

resource_drop_live_only162:                       ; preds = %resource_drop_merge158
  %ref_drop_handle164 = load ptr, ptr %local_5, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle164)
  store ptr null, ptr %local_5, align 8
  br label %resource_drop_merge163

resource_drop_merge163:                           ; preds = %resource_drop_live_only162, %resource_drop_merge158
  %resource_drop_flag165 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed166 = icmp eq i64 %resource_drop_flag165, 0
  br i1 %resource_drop_not_consumed166, label %resource_drop_live_only167, label %resource_drop_merge168

resource_drop_live_only167:                       ; preds = %resource_drop_merge163
  %ref_drop_handle169 = load ptr, ptr %local_2, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle169)
  store ptr null, ptr %local_2, align 8
  br label %resource_drop_merge168

resource_drop_merge168:                           ; preds = %resource_drop_live_only167, %resource_drop_merge163
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

resource_drop_live_only181:                       ; preds = %bb15
  %ref_drop_handle183 = load ptr, ptr %local_11, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle183)
  store ptr null, ptr %local_11, align 8
  br label %resource_drop_merge182

resource_drop_merge182:                           ; preds = %resource_drop_live_only181, %bb15
  %resource_drop_flag184 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed185 = icmp eq i64 %resource_drop_flag184, 0
  br i1 %resource_drop_not_consumed185, label %resource_drop_live_only186, label %resource_drop_merge187

resource_drop_live_only186:                       ; preds = %resource_drop_merge182
  %ref_drop_handle188 = load ptr, ptr %local_8, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle188)
  store ptr null, ptr %local_8, align 8
  br label %resource_drop_merge187

resource_drop_merge187:                           ; preds = %resource_drop_live_only186, %resource_drop_merge182
  %resource_drop_flag189 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed190 = icmp eq i64 %resource_drop_flag189, 0
  br i1 %resource_drop_not_consumed190, label %resource_drop_live_only191, label %resource_drop_merge192

resource_drop_live_only191:                       ; preds = %resource_drop_merge187
  %ref_drop_handle193 = load ptr, ptr %local_5, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle193)
  store ptr null, ptr %local_5, align 8
  br label %resource_drop_merge192

resource_drop_merge192:                           ; preds = %resource_drop_live_only191, %resource_drop_merge187
  %resource_drop_flag194 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed195 = icmp eq i64 %resource_drop_flag194, 0
  br i1 %resource_drop_not_consumed195, label %resource_drop_live_only196, label %resource_drop_merge197

resource_drop_live_only196:                       ; preds = %resource_drop_merge192
  %ref_drop_handle198 = load ptr, ptr %local_2, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle198)
  store ptr null, ptr %local_2, align 8
  br label %resource_drop_merge197

resource_drop_merge197:                           ; preds = %resource_drop_live_only196, %resource_drop_merge192
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

resource_drop_live_only210:                       ; preds = %bb17
  %ref_drop_handle212 = load ptr, ptr %local_11, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle212)
  store ptr null, ptr %local_11, align 8
  br label %resource_drop_merge211

resource_drop_merge211:                           ; preds = %resource_drop_live_only210, %bb17
  %resource_drop_flag213 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed214 = icmp eq i64 %resource_drop_flag213, 0
  br i1 %resource_drop_not_consumed214, label %resource_drop_live_only215, label %resource_drop_merge216

resource_drop_live_only215:                       ; preds = %resource_drop_merge211
  %ref_drop_handle217 = load ptr, ptr %local_8, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle217)
  store ptr null, ptr %local_8, align 8
  br label %resource_drop_merge216

resource_drop_merge216:                           ; preds = %resource_drop_live_only215, %resource_drop_merge211
  %resource_drop_flag218 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed219 = icmp eq i64 %resource_drop_flag218, 0
  br i1 %resource_drop_not_consumed219, label %resource_drop_live_only220, label %resource_drop_merge221

resource_drop_live_only220:                       ; preds = %resource_drop_merge216
  %ref_drop_handle222 = load ptr, ptr %local_5, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle222)
  store ptr null, ptr %local_5, align 8
  br label %resource_drop_merge221

resource_drop_merge221:                           ; preds = %resource_drop_live_only220, %resource_drop_merge216
  %resource_drop_flag223 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed224 = icmp eq i64 %resource_drop_flag223, 0
  br i1 %resource_drop_not_consumed224, label %resource_drop_live_only225, label %resource_drop_merge226

resource_drop_live_only225:                       ; preds = %resource_drop_merge221
  %ref_drop_handle227 = load ptr, ptr %local_2, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle227)
  store ptr null, ptr %local_2, align 8
  br label %resource_drop_merge226

resource_drop_merge226:                           ; preds = %resource_drop_live_only225, %resource_drop_merge221
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

resource_drop_live_only239:                       ; preds = %bb19
  %ref_drop_handle241 = load ptr, ptr %local_11, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle241)
  store ptr null, ptr %local_11, align 8
  br label %resource_drop_merge240

resource_drop_merge240:                           ; preds = %resource_drop_live_only239, %bb19
  %resource_drop_flag242 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed243 = icmp eq i64 %resource_drop_flag242, 0
  br i1 %resource_drop_not_consumed243, label %resource_drop_live_only244, label %resource_drop_merge245

resource_drop_live_only244:                       ; preds = %resource_drop_merge240
  %ref_drop_handle246 = load ptr, ptr %local_8, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle246)
  store ptr null, ptr %local_8, align 8
  br label %resource_drop_merge245

resource_drop_merge245:                           ; preds = %resource_drop_live_only244, %resource_drop_merge240
  %resource_drop_flag247 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed248 = icmp eq i64 %resource_drop_flag247, 0
  br i1 %resource_drop_not_consumed248, label %resource_drop_live_only249, label %resource_drop_merge250

resource_drop_live_only249:                       ; preds = %resource_drop_merge245
  %ref_drop_handle251 = load ptr, ptr %local_5, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle251)
  store ptr null, ptr %local_5, align 8
  br label %resource_drop_merge250

resource_drop_merge250:                           ; preds = %resource_drop_live_only249, %resource_drop_merge245
  %resource_drop_flag252 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed253 = icmp eq i64 %resource_drop_flag252, 0
  br i1 %resource_drop_not_consumed253, label %resource_drop_live_only254, label %resource_drop_merge255

resource_drop_live_only254:                       ; preds = %resource_drop_merge250
  %ref_drop_handle256 = load ptr, ptr %local_2, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle256)
  store ptr null, ptr %local_2, align 8
  br label %resource_drop_merge255

resource_drop_merge255:                           ; preds = %resource_drop_live_only254, %resource_drop_merge250
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

cancel_exit261:                                   ; preds = %bb20
  %ref_drop_handle263 = load ptr, ptr %local_35, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle263)
  store ptr null, ptr %local_35, align 8
  %resource_drop_flag264 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed265 = icmp eq i64 %resource_drop_flag264, 0
  br i1 %resource_drop_not_consumed265, label %resource_drop_live_only266, label %resource_drop_merge267

after_cooperate262:                               ; preds = %bb20
  br label %bb8

resource_drop_live_only266:                       ; preds = %cancel_exit261
  %ref_drop_handle268 = load ptr, ptr %local_11, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle268)
  store ptr null, ptr %local_11, align 8
  br label %resource_drop_merge267

resource_drop_merge267:                           ; preds = %resource_drop_live_only266, %cancel_exit261
  %resource_drop_flag269 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed270 = icmp eq i64 %resource_drop_flag269, 0
  br i1 %resource_drop_not_consumed270, label %resource_drop_live_only271, label %resource_drop_merge272

resource_drop_live_only271:                       ; preds = %resource_drop_merge267
  %ref_drop_handle273 = load ptr, ptr %local_8, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle273)
  store ptr null, ptr %local_8, align 8
  br label %resource_drop_merge272

resource_drop_merge272:                           ; preds = %resource_drop_live_only271, %resource_drop_merge267
  %resource_drop_flag274 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed275 = icmp eq i64 %resource_drop_flag274, 0
  br i1 %resource_drop_not_consumed275, label %resource_drop_live_only276, label %resource_drop_merge277

resource_drop_live_only276:                       ; preds = %resource_drop_merge272
  %ref_drop_handle278 = load ptr, ptr %local_5, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle278)
  store ptr null, ptr %local_5, align 8
  br label %resource_drop_merge277

resource_drop_merge277:                           ; preds = %resource_drop_live_only276, %resource_drop_merge272
  %resource_drop_flag279 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed280 = icmp eq i64 %resource_drop_flag279, 0
  br i1 %resource_drop_not_consumed280, label %resource_drop_live_only281, label %resource_drop_merge282

resource_drop_live_only281:                       ; preds = %resource_drop_merge277
  %ref_drop_handle283 = load ptr, ptr %local_2, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle283)
  store ptr null, ptr %local_2, align 8
  br label %resource_drop_merge282

resource_drop_merge282:                           ; preds = %resource_drop_live_only281, %resource_drop_merge277
  ret i64 0
}

define internal ptr @expired() {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca i64, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca ptr, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca ptr, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  store i64 1, ptr %local_0, align 8
  %rc_new = call ptr @hew_rc_new(ptr %local_0, i64 8, i64 8, ptr null)
  store ptr %rc_new, ptr %local_1, align 8
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %local_2, align 8
  store i64 0, ptr %local_3, align 8
  %rc_borrow_handle = load ptr, ptr %local_2, align 8
  %rc_handle_result = call ptr @hew_rc_downgrade(ptr %rc_borrow_handle)
  store ptr %rc_handle_result, ptr %local_4, align 8
  %move_load1 = load ptr, ptr %local_4, align 8
  store ptr %move_load1, ptr %return_slot, align 8
  %resource_drop_flag = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed = icmp eq i64 %resource_drop_flag, 0
  br i1 %resource_drop_not_consumed, label %resource_drop_live_only, label %resource_drop_merge

resource_drop_live_only:                          ; preds = %bb0
  %ref_drop_handle = load ptr, ptr %local_2, align 8
  call void @hew_rc_drop(ptr %ref_drop_handle)
  store ptr null, ptr %local_2, align 8
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
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_result = call ptr @expired()
  store ptr %call_result, ptr %local_0, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_0, align 8
  store ptr %move_load, ptr %local_1, align 8
  store i64 0, ptr %local_2, align 8
  %weak_upgrade_handle = load ptr, ptr %local_1, align 8
  %weak_upgrade = call ptr @hew_weak_upgrade_rc(ptr %weak_upgrade_handle)
  %machine_tag_ptr = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %local_4, i32 0, i32 0
  %machine_payload_ptr = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %local_4, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { ptr }, ptr %machine_payload_ptr, i32 0, i32 0
  %weak_upgrade_is_none = icmp eq ptr %weak_upgrade, null
  br i1 %weak_upgrade_is_none, label %weak_upgrade_none, label %weak_upgrade_some

bb2:                                              ; preds = %after_cooperate13, %after_cooperate8
  %move_load2 = load i64, ptr %local_3, align 8
  store i64 %move_load2, ptr %local_12, align 8
  %call_result3 = call i64 @live()
  store i64 %call_result3, ptr %local_13, align 8
  br label %bb7

bb3:                                              ; preds = %weak_upgrade_cont
  store i64 9000, ptr %local_10, align 8
  %move_load4 = load i64, ptr %local_10, align 8
  store i64 %move_load4, ptr %local_3, align 8
  %hew_actor_cooperate5 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel6 = icmp eq i32 %hew_actor_cooperate5, 2
  br i1 %hew_cooperate_is_cancel6, label %cancel_exit7, label %after_cooperate8

bb4:                                              ; preds = %bb6
  store i64 1, ptr %local_11, align 8
  %move_load9 = load i64, ptr %local_11, align 8
  store i64 %move_load9, ptr %local_3, align 8
  %hew_actor_cooperate10 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel11 = icmp eq i32 %hew_actor_cooperate10, 2
  br i1 %hew_cooperate_is_cancel11, label %cancel_exit12, label %after_cooperate13

bb5:                                              ; preds = %bb6
  %resource_drop_flag19 = load i64, ptr %local_2, align 8
  %resource_drop_not_consumed20 = icmp eq i64 %resource_drop_flag19, 0
  br i1 %resource_drop_not_consumed20, label %resource_drop_live_only21, label %resource_drop_merge22

bb6:                                              ; preds = %weak_upgrade_cont
  store i64 1, ptr %local_8, align 8
  %cmp_lhs24 = load i64, ptr %local_5, align 8
  %cmp_rhs25 = load i64, ptr %local_8, align 8
  %cmp_bit26 = icmp eq i64 %cmp_lhs24, %cmp_rhs25
  %cmp_zext27 = zext i1 %cmp_bit26 to i8
  store i8 %cmp_zext27, ptr %local_9, align 1
  %cond_load28 = load i8, ptr %local_9, align 1
  %cond_nz29 = icmp ne i8 %cond_load28, 0
  br i1 %cond_nz29, label %bb4, label %bb5

bb7:                                              ; preds = %bb2
  %checked_lhs = load i64, ptr %local_13, align 8
  %checked_rhs = load i64, ptr %local_12, align 8
  %with_overflow = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_14, align 8
  store i8 %checked_overflow_widen, ptr %local_15, align 1
  %cond_load30 = load i8, ptr %local_15, align 1
  %cond_nz31 = icmp ne i8 %cond_load30, 0
  br i1 %cond_nz31, label %bb8, label %bb9

bb8:                                              ; preds = %bb7
  %resource_drop_flag32 = load i64, ptr %local_2, align 8
  %resource_drop_not_consumed33 = icmp eq i64 %resource_drop_flag32, 0
  br i1 %resource_drop_not_consumed33, label %resource_drop_live_only34, label %resource_drop_merge35

bb9:                                              ; preds = %bb7
  %move_load37 = load i64, ptr %local_14, align 8
  store i64 %move_load37, ptr %return_slot, align 8
  %resource_drop_flag38 = load i64, ptr %local_2, align 8
  %resource_drop_not_consumed39 = icmp eq i64 %resource_drop_flag38, 0
  br i1 %resource_drop_not_consumed39, label %resource_drop_live_only40, label %resource_drop_merge41

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
  %machine_tag_ptr1 = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %local_4, i32 0, i32 0
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

cancel_exit7:                                     ; preds = %bb3
  %resource_drop_flag = load i64, ptr %local_2, align 8
  %resource_drop_not_consumed = icmp eq i64 %resource_drop_flag, 0
  br i1 %resource_drop_not_consumed, label %resource_drop_live_only, label %resource_drop_merge

after_cooperate8:                                 ; preds = %bb3
  br label %bb2

resource_drop_live_only:                          ; preds = %cancel_exit7
  %ref_drop_handle = load ptr, ptr %local_1, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle)
  store ptr null, ptr %local_1, align 8
  br label %resource_drop_merge

resource_drop_merge:                              ; preds = %resource_drop_live_only, %cancel_exit7
  ret i64 0

cancel_exit12:                                    ; preds = %bb4
  %resource_drop_flag14 = load i64, ptr %local_2, align 8
  %resource_drop_not_consumed15 = icmp eq i64 %resource_drop_flag14, 0
  br i1 %resource_drop_not_consumed15, label %resource_drop_live_only16, label %resource_drop_merge17

after_cooperate13:                                ; preds = %bb4
  br label %bb2

resource_drop_live_only16:                        ; preds = %cancel_exit12
  %ref_drop_handle18 = load ptr, ptr %local_1, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle18)
  store ptr null, ptr %local_1, align 8
  br label %resource_drop_merge17

resource_drop_merge17:                            ; preds = %resource_drop_live_only16, %cancel_exit12
  ret i64 0

resource_drop_live_only21:                        ; preds = %bb5
  %ref_drop_handle23 = load ptr, ptr %local_1, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle23)
  store ptr null, ptr %local_1, align 8
  br label %resource_drop_merge22

resource_drop_merge22:                            ; preds = %resource_drop_live_only21, %bb5
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

resource_drop_live_only34:                        ; preds = %bb8
  %ref_drop_handle36 = load ptr, ptr %local_1, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle36)
  store ptr null, ptr %local_1, align 8
  br label %resource_drop_merge35

resource_drop_merge35:                            ; preds = %resource_drop_live_only34, %bb8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

resource_drop_live_only40:                        ; preds = %bb9
  %ref_drop_handle42 = load ptr, ptr %local_1, align 8
  call void @hew_weak_drop_rc(ptr %ref_drop_handle42)
  store ptr null, ptr %local_1, align 8
  br label %resource_drop_merge41

resource_drop_merge41:                            ; preds = %resource_drop_live_only40, %bb9
  %hew_lambda_drain_all_call = call i32 @hew_lambda_drain_all(i64 0)
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
  %hew_duration_nanos = load i64, ptr %local_0, align 8
  %hew_duration_nanos_call = call i64 @hew_duration_nanos(i64 %hew_duration_nanos)
  store i64 %hew_duration_nanos_call, ptr %local_1, align 8
  %call_arg = load i64, ptr %local_1, align 8
  %call_result = call ptr @hew_i64_to_string(i64 %call_arg)
  store ptr %call_result, ptr %local_2, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
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

declare i32 @hew_actor_cooperate()

declare ptr @hew_rc_new(ptr, i64, i64, ptr)

declare ptr @hew_rc_clone(ptr)

declare ptr @hew_rc_downgrade(ptr)

declare ptr @hew_weak_clone_rc(ptr)

declare i64 @hew_rc_strong_count(ptr)

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.smul.with.overflow.i64(i64, i64) #0

declare void @hew_weak_drop_rc(ptr)

declare void @hew_rc_drop(ptr)

declare void @hew_trap_with_code(i32)

; Function Attrs: cold noreturn nounwind memory(inaccessiblemem: write)
declare void @llvm.trap() #1

declare i64 @hew_rc_weak_count(ptr)

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64) #0

declare i32 @hew_rc_is_unique(ptr)

declare ptr @hew_weak_upgrade_rc(ptr)

declare void @hew_rc_set(ptr, ptr)

declare ptr @hew_rc_get(ptr)

declare i32 @hew_lambda_drain_all(i64)

attributes #0 = { nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none) }
attributes #1 = { cold noreturn nounwind memory(inaccessiblemem: write) }
