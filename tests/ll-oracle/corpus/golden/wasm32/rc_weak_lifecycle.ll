; ModuleID = 'rc_weak_lifecycle'
source_filename = "rc_weak_lifecycle"
target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

%"Option$$Rc$li64$g" = type { i8, [1 x i32] }
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
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  store i64 7, ptr %local_0, align 8
  %rc_new = call ptr @hew_rc_new(ptr %local_0, i32 8, i32 8, ptr null)
  store ptr %rc_new, ptr %local_1, align 4
  %move_load = load ptr, ptr %local_1, align 4
  store ptr %move_load, ptr %local_2, align 4
  store i64 0, ptr %local_3, align 8
  %rc_borrow_handle = load ptr, ptr %local_2, align 4
  %rc_handle_result = call ptr @hew_rc_clone(ptr %rc_borrow_handle)
  store ptr %rc_handle_result, ptr %local_4, align 4
  %move_load1 = load ptr, ptr %local_4, align 4
  store ptr %move_load1, ptr %local_5, align 4
  store i64 0, ptr %local_6, align 8
  %rc_borrow_handle2 = load ptr, ptr %local_2, align 4
  %rc_handle_result3 = call ptr @hew_rc_downgrade(ptr %rc_borrow_handle2)
  store ptr %rc_handle_result3, ptr %local_7, align 4
  %move_load4 = load ptr, ptr %local_7, align 4
  store ptr %move_load4, ptr %local_8, align 4
  store i64 0, ptr %local_9, align 8
  %rc_borrow_handle5 = load ptr, ptr %local_8, align 4
  %rc_handle_result6 = call ptr @hew_weak_clone_rc(ptr %rc_borrow_handle5)
  store ptr %rc_handle_result6, ptr %local_10, align 4
  %move_load7 = load ptr, ptr %local_10, align 4
  store ptr %move_load7, ptr %local_11, align 4
  store i64 0, ptr %local_12, align 8
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

bb1:                                              ; preds = %bb0
  %resource_drop_flag = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed = icmp eq i64 %resource_drop_flag, 0
  br i1 %resource_drop_not_consumed, label %resource_drop_live_only, label %resource_drop_merge

bb2:                                              ; preds = %bb0
  %rc_count_handle23 = load ptr, ptr %local_2, align 4
  %rc_count24 = call i32 @hew_rc_weak_count(ptr %rc_count_handle23)
  %ffi_zext25 = zext i32 %rc_count24 to i64
  store i64 %ffi_zext25, ptr %local_17, align 8
  %checked_lhs26 = load i64, ptr %local_15, align 8
  %checked_rhs27 = load i64, ptr %local_17, align 8
  %with_overflow28 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs26, i64 %checked_rhs27)
  %checked_result29 = extractvalue { i64, i1 } %with_overflow28, 0
  %checked_overflow30 = extractvalue { i64, i1 } %with_overflow28, 1
  %checked_overflow_widen31 = zext i1 %checked_overflow30 to i8
  store i64 %checked_result29, ptr %local_18, align 8
  store i8 %checked_overflow_widen31, ptr %local_19, align 1
  %cond_load32 = load i8, ptr %local_19, align 1
  %cond_nz33 = icmp ne i8 %cond_load32, 0
  br i1 %cond_nz33, label %bb3, label %bb4

bb3:                                              ; preds = %bb2
  %resource_drop_flag34 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed35 = icmp eq i64 %resource_drop_flag34, 0
  br i1 %resource_drop_not_consumed35, label %resource_drop_live_only36, label %resource_drop_merge37

bb4:                                              ; preds = %bb2
  %move_load54 = load i64, ptr %local_18, align 8
  store i64 %move_load54, ptr %local_20, align 8
  %rc_is_unique_handle = load ptr, ptr %local_2, align 4
  %rc_is_unique = call i32 @hew_rc_is_unique(ptr %rc_is_unique_handle)
  %rc_unique_bit = icmp ne i32 %rc_is_unique, 0
  %rc_unique_bool = zext i1 %rc_unique_bit to i8
  store i8 %rc_unique_bool, ptr %local_22, align 1
  %cond_load55 = load i8, ptr %local_22, align 1
  %cond_nz56 = icmp ne i8 %cond_load55, 0
  br i1 %cond_nz56, label %bb5, label %bb6

bb5:                                              ; preds = %bb4
  store i64 90, ptr %local_23, align 8
  %move_load57 = load i64, ptr %local_23, align 8
  store i64 %move_load57, ptr %local_24, align 8
  %move_load58 = load i64, ptr %local_24, align 8
  store i64 %move_load58, ptr %local_21, align 8
  br label %bb7

bb6:                                              ; preds = %bb4
  store i64 1, ptr %local_25, align 8
  %move_load59 = load i64, ptr %local_25, align 8
  store i64 %move_load59, ptr %local_26, align 8
  %move_load60 = load i64, ptr %local_26, align 8
  store i64 %move_load60, ptr %local_21, align 8
  br label %bb7

bb7:                                              ; preds = %bb6, %bb5
  %move_load61 = load i64, ptr %local_21, align 8
  store i64 %move_load61, ptr %local_27, align 8
  %weak_upgrade_handle = load ptr, ptr %local_8, align 4
  %weak_upgrade = call ptr @hew_weak_upgrade_rc(ptr %weak_upgrade_handle)
  %machine_tag_ptr = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %local_29, i32 0, i32 0
  %machine_payload_ptr = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %local_29, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { ptr }, ptr %machine_payload_ptr, i32 0, i32 0
  %weak_upgrade_is_none = icmp eq ptr %weak_upgrade, null
  br i1 %weak_upgrade_is_none, label %weak_upgrade_none, label %weak_upgrade_some

bb8:                                              ; preds = %after_cooperate260, %after_cooperate104
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_29)
  store %"Option$$Rc$li64$g" zeroinitializer, ptr %local_29, align 4
  %move_load65 = load i64, ptr %local_28, align 8
  store i64 %move_load65, ptr %return_slot, align 8
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_29)
  %resource_drop_flag66 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed67 = icmp eq i64 %resource_drop_flag66, 0
  br i1 %resource_drop_not_consumed67, label %resource_drop_live_only68, label %resource_drop_merge69

bb9:                                              ; preds = %weak_upgrade_cont
  %machine_payload_ptr86 = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %local_29, i32 0, i32 1
  %machine_variant_field_ptr87 = getelementptr inbounds nuw { ptr }, ptr %machine_payload_ptr86, i32 0, i32 0
  %move_load88 = load ptr, ptr %machine_variant_field_ptr87, align 4
  store ptr %move_load88, ptr %local_35, align 4
  store i64 9, ptr %local_36, align 8
  %rc_set_handle = load ptr, ptr %local_2, align 4
  call void @hew_rc_set(ptr %rc_set_handle, ptr %local_36)
  store i8 0, ptr %local_37, align 1
  %rc_count_handle89 = load ptr, ptr %local_35, align 4
  %rc_count90 = call i32 @hew_rc_strong_count(ptr %rc_count_handle89)
  %ffi_zext91 = zext i32 %rc_count90 to i64
  store i64 %ffi_zext91, ptr %local_38, align 8
  store i64 10, ptr %local_39, align 8
  %checked_lhs92 = load i64, ptr %local_38, align 8
  %checked_rhs93 = load i64, ptr %local_39, align 8
  %with_overflow94 = call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %checked_lhs92, i64 %checked_rhs93)
  %checked_result95 = extractvalue { i64, i1 } %with_overflow94, 0
  %checked_overflow96 = extractvalue { i64, i1 } %with_overflow94, 1
  %checked_overflow_widen97 = zext i1 %checked_overflow96 to i8
  store i64 %checked_result95, ptr %local_40, align 8
  store i8 %checked_overflow_widen97, ptr %local_41, align 1
  %cond_load98 = load i8, ptr %local_41, align 1
  %cond_nz99 = icmp ne i8 %cond_load98, 0
  br i1 %cond_nz99, label %bb13, label %bb14

bb10:                                             ; preds = %bb12
  store i64 9000, ptr %local_50, align 8
  %move_load100 = load i64, ptr %local_50, align 8
  store i64 %move_load100, ptr %local_28, align 8
  %hew_actor_cooperate101 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel102 = icmp eq i32 %hew_actor_cooperate101, 2
  br i1 %hew_cooperate_is_cancel102, label %cancel_exit103, label %after_cooperate104

bb11:                                             ; preds = %bb12
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_29)
  %resource_drop_flag125 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed126 = icmp eq i64 %resource_drop_flag125, 0
  br i1 %resource_drop_not_consumed126, label %resource_drop_live_only127, label %resource_drop_merge128

bb12:                                             ; preds = %weak_upgrade_cont
  store i64 1, ptr %local_33, align 8
  %cmp_lhs145 = load i64, ptr %local_30, align 8
  %cmp_rhs146 = load i64, ptr %local_33, align 8
  %cmp_bit147 = icmp eq i64 %cmp_lhs145, %cmp_rhs146
  %cmp_zext148 = zext i1 %cmp_bit147 to i8
  store i8 %cmp_zext148, ptr %local_34, align 1
  %cond_load149 = load i8, ptr %local_34, align 1
  %cond_nz150 = icmp ne i8 %cond_load149, 0
  br i1 %cond_nz150, label %bb10, label %bb11

bb13:                                             ; preds = %bb9
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_29)
  %resource_drop_flag151 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed152 = icmp eq i64 %resource_drop_flag151, 0
  br i1 %resource_drop_not_consumed152, label %resource_drop_live_only153, label %resource_drop_merge154

bb14:                                             ; preds = %bb9
  %checked_lhs171 = load i64, ptr %local_20, align 8
  %checked_rhs172 = load i64, ptr %local_40, align 8
  %with_overflow173 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs171, i64 %checked_rhs172)
  %checked_result174 = extractvalue { i64, i1 } %with_overflow173, 0
  %checked_overflow175 = extractvalue { i64, i1 } %with_overflow173, 1
  %checked_overflow_widen176 = zext i1 %checked_overflow175 to i8
  store i64 %checked_result174, ptr %local_42, align 8
  store i8 %checked_overflow_widen176, ptr %local_43, align 1
  %cond_load177 = load i8, ptr %local_43, align 1
  %cond_nz178 = icmp ne i8 %cond_load177, 0
  br i1 %cond_nz178, label %bb15, label %bb16

bb15:                                             ; preds = %bb14
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_29)
  %resource_drop_flag179 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed180 = icmp eq i64 %resource_drop_flag179, 0
  br i1 %resource_drop_not_consumed180, label %resource_drop_live_only181, label %resource_drop_merge182

bb16:                                             ; preds = %bb14
  %rc_get_handle = load ptr, ptr %local_5, align 4
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
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_29)
  %resource_drop_flag207 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed208 = icmp eq i64 %resource_drop_flag207, 0
  br i1 %resource_drop_not_consumed208, label %resource_drop_live_only209, label %resource_drop_merge210

bb18:                                             ; preds = %bb16
  %checked_lhs227 = load i64, ptr %local_45, align 8
  %checked_rhs228 = load i64, ptr %local_27, align 8
  %with_overflow229 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs227, i64 %checked_rhs228)
  %checked_result230 = extractvalue { i64, i1 } %with_overflow229, 0
  %checked_overflow231 = extractvalue { i64, i1 } %with_overflow229, 1
  %checked_overflow_widen232 = zext i1 %checked_overflow231 to i8
  store i64 %checked_result230, ptr %local_47, align 8
  store i8 %checked_overflow_widen232, ptr %local_48, align 1
  %cond_load233 = load i8, ptr %local_48, align 1
  %cond_nz234 = icmp ne i8 %cond_load233, 0
  br i1 %cond_nz234, label %bb19, label %bb20

bb19:                                             ; preds = %bb18
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_29)
  %resource_drop_flag235 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed236 = icmp eq i64 %resource_drop_flag235, 0
  br i1 %resource_drop_not_consumed236, label %resource_drop_live_only237, label %resource_drop_merge238

bb20:                                             ; preds = %bb18
  %move_load255 = load i64, ptr %local_47, align 8
  store i64 %move_load255, ptr %local_49, align 8
  %move_load256 = load i64, ptr %local_49, align 8
  store i64 %move_load256, ptr %local_28, align 8
  %hew_actor_cooperate257 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel258 = icmp eq i32 %hew_actor_cooperate257, 2
  br i1 %hew_cooperate_is_cancel258, label %cancel_exit259, label %after_cooperate260

cancel_exit:                                      ; preds = %entry
  ret i64 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

resource_drop_live_only:                          ; preds = %bb1
  %ref_drop_handle = load ptr, ptr %local_11, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle)
  store ptr null, ptr %local_11, align 4
  br label %resource_drop_merge

resource_drop_merge:                              ; preds = %resource_drop_live_only, %bb1
  %resource_drop_flag8 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed9 = icmp eq i64 %resource_drop_flag8, 0
  br i1 %resource_drop_not_consumed9, label %resource_drop_live_only10, label %resource_drop_merge11

resource_drop_live_only10:                        ; preds = %resource_drop_merge
  %ref_drop_handle12 = load ptr, ptr %local_8, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle12)
  store ptr null, ptr %local_8, align 4
  br label %resource_drop_merge11

resource_drop_merge11:                            ; preds = %resource_drop_live_only10, %resource_drop_merge
  %resource_drop_flag13 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed14 = icmp eq i64 %resource_drop_flag13, 0
  br i1 %resource_drop_not_consumed14, label %resource_drop_live_only15, label %resource_drop_merge16

resource_drop_live_only15:                        ; preds = %resource_drop_merge11
  %ref_drop_handle17 = load ptr, ptr %local_5, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle17)
  store ptr null, ptr %local_5, align 4
  br label %resource_drop_merge16

resource_drop_merge16:                            ; preds = %resource_drop_live_only15, %resource_drop_merge11
  %resource_drop_flag18 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed19 = icmp eq i64 %resource_drop_flag18, 0
  br i1 %resource_drop_not_consumed19, label %resource_drop_live_only20, label %resource_drop_merge21

resource_drop_live_only20:                        ; preds = %resource_drop_merge16
  %ref_drop_handle22 = load ptr, ptr %local_2, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle22)
  store ptr null, ptr %local_2, align 4
  br label %resource_drop_merge21

resource_drop_merge21:                            ; preds = %resource_drop_live_only20, %resource_drop_merge16
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

resource_drop_live_only36:                        ; preds = %bb3
  %ref_drop_handle38 = load ptr, ptr %local_11, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle38)
  store ptr null, ptr %local_11, align 4
  br label %resource_drop_merge37

resource_drop_merge37:                            ; preds = %resource_drop_live_only36, %bb3
  %resource_drop_flag39 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed40 = icmp eq i64 %resource_drop_flag39, 0
  br i1 %resource_drop_not_consumed40, label %resource_drop_live_only41, label %resource_drop_merge42

resource_drop_live_only41:                        ; preds = %resource_drop_merge37
  %ref_drop_handle43 = load ptr, ptr %local_8, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle43)
  store ptr null, ptr %local_8, align 4
  br label %resource_drop_merge42

resource_drop_merge42:                            ; preds = %resource_drop_live_only41, %resource_drop_merge37
  %resource_drop_flag44 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed45 = icmp eq i64 %resource_drop_flag44, 0
  br i1 %resource_drop_not_consumed45, label %resource_drop_live_only46, label %resource_drop_merge47

resource_drop_live_only46:                        ; preds = %resource_drop_merge42
  %ref_drop_handle48 = load ptr, ptr %local_5, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle48)
  store ptr null, ptr %local_5, align 4
  br label %resource_drop_merge47

resource_drop_merge47:                            ; preds = %resource_drop_live_only46, %resource_drop_merge42
  %resource_drop_flag49 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed50 = icmp eq i64 %resource_drop_flag49, 0
  br i1 %resource_drop_not_consumed50, label %resource_drop_live_only51, label %resource_drop_merge52

resource_drop_live_only51:                        ; preds = %resource_drop_merge47
  %ref_drop_handle53 = load ptr, ptr %local_2, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle53)
  store ptr null, ptr %local_2, align 4
  br label %resource_drop_merge52

resource_drop_merge52:                            ; preds = %resource_drop_live_only51, %resource_drop_merge47
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

weak_upgrade_some:                                ; preds = %bb7
  store i8 0, ptr %machine_tag_ptr, align 1
  store ptr %weak_upgrade, ptr %machine_variant_field_ptr, align 4
  br label %weak_upgrade_cont

weak_upgrade_none:                                ; preds = %bb7
  store i8 1, ptr %machine_tag_ptr, align 1
  store ptr null, ptr %machine_variant_field_ptr, align 4
  br label %weak_upgrade_cont

weak_upgrade_cont:                                ; preds = %weak_upgrade_none, %weak_upgrade_some
  %machine_tag_ptr62 = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %local_29, i32 0, i32 0
  %move_iN_load = load i8, ptr %machine_tag_ptr62, align 1
  %move_iN_zext = zext i8 %move_iN_load to i64
  store i64 %move_iN_zext, ptr %local_30, align 8
  store i64 0, ptr %local_31, align 8
  %cmp_lhs = load i64, ptr %local_30, align 8
  %cmp_rhs = load i64, ptr %local_31, align 8
  %cmp_bit = icmp eq i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_32, align 1
  %cond_load63 = load i8, ptr %local_32, align 1
  %cond_nz64 = icmp ne i8 %cond_load63, 0
  br i1 %cond_nz64, label %bb9, label %bb12

resource_drop_live_only68:                        ; preds = %bb8
  %ref_drop_handle70 = load ptr, ptr %local_11, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle70)
  store ptr null, ptr %local_11, align 4
  br label %resource_drop_merge69

resource_drop_merge69:                            ; preds = %resource_drop_live_only68, %bb8
  %resource_drop_flag71 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed72 = icmp eq i64 %resource_drop_flag71, 0
  br i1 %resource_drop_not_consumed72, label %resource_drop_live_only73, label %resource_drop_merge74

resource_drop_live_only73:                        ; preds = %resource_drop_merge69
  %ref_drop_handle75 = load ptr, ptr %local_8, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle75)
  store ptr null, ptr %local_8, align 4
  br label %resource_drop_merge74

resource_drop_merge74:                            ; preds = %resource_drop_live_only73, %resource_drop_merge69
  %resource_drop_flag76 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed77 = icmp eq i64 %resource_drop_flag76, 0
  br i1 %resource_drop_not_consumed77, label %resource_drop_live_only78, label %resource_drop_merge79

resource_drop_live_only78:                        ; preds = %resource_drop_merge74
  %ref_drop_handle80 = load ptr, ptr %local_5, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle80)
  store ptr null, ptr %local_5, align 4
  br label %resource_drop_merge79

resource_drop_merge79:                            ; preds = %resource_drop_live_only78, %resource_drop_merge74
  %resource_drop_flag81 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed82 = icmp eq i64 %resource_drop_flag81, 0
  br i1 %resource_drop_not_consumed82, label %resource_drop_live_only83, label %resource_drop_merge84

resource_drop_live_only83:                        ; preds = %resource_drop_merge79
  %ref_drop_handle85 = load ptr, ptr %local_2, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle85)
  store ptr null, ptr %local_2, align 4
  br label %resource_drop_merge84

resource_drop_merge84:                            ; preds = %resource_drop_live_only83, %resource_drop_merge79
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val

cancel_exit103:                                   ; preds = %bb10
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_29)
  %resource_drop_flag105 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed106 = icmp eq i64 %resource_drop_flag105, 0
  br i1 %resource_drop_not_consumed106, label %resource_drop_live_only107, label %resource_drop_merge108

after_cooperate104:                               ; preds = %bb10
  br label %bb8

resource_drop_live_only107:                       ; preds = %cancel_exit103
  %ref_drop_handle109 = load ptr, ptr %local_11, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle109)
  store ptr null, ptr %local_11, align 4
  br label %resource_drop_merge108

resource_drop_merge108:                           ; preds = %resource_drop_live_only107, %cancel_exit103
  %resource_drop_flag110 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed111 = icmp eq i64 %resource_drop_flag110, 0
  br i1 %resource_drop_not_consumed111, label %resource_drop_live_only112, label %resource_drop_merge113

resource_drop_live_only112:                       ; preds = %resource_drop_merge108
  %ref_drop_handle114 = load ptr, ptr %local_8, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle114)
  store ptr null, ptr %local_8, align 4
  br label %resource_drop_merge113

resource_drop_merge113:                           ; preds = %resource_drop_live_only112, %resource_drop_merge108
  %resource_drop_flag115 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed116 = icmp eq i64 %resource_drop_flag115, 0
  br i1 %resource_drop_not_consumed116, label %resource_drop_live_only117, label %resource_drop_merge118

resource_drop_live_only117:                       ; preds = %resource_drop_merge113
  %ref_drop_handle119 = load ptr, ptr %local_5, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle119)
  store ptr null, ptr %local_5, align 4
  br label %resource_drop_merge118

resource_drop_merge118:                           ; preds = %resource_drop_live_only117, %resource_drop_merge113
  %resource_drop_flag120 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed121 = icmp eq i64 %resource_drop_flag120, 0
  br i1 %resource_drop_not_consumed121, label %resource_drop_live_only122, label %resource_drop_merge123

resource_drop_live_only122:                       ; preds = %resource_drop_merge118
  %ref_drop_handle124 = load ptr, ptr %local_2, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle124)
  store ptr null, ptr %local_2, align 4
  br label %resource_drop_merge123

resource_drop_merge123:                           ; preds = %resource_drop_live_only122, %resource_drop_merge118
  ret i64 0

resource_drop_live_only127:                       ; preds = %bb11
  %ref_drop_handle129 = load ptr, ptr %local_11, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle129)
  store ptr null, ptr %local_11, align 4
  br label %resource_drop_merge128

resource_drop_merge128:                           ; preds = %resource_drop_live_only127, %bb11
  %resource_drop_flag130 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed131 = icmp eq i64 %resource_drop_flag130, 0
  br i1 %resource_drop_not_consumed131, label %resource_drop_live_only132, label %resource_drop_merge133

resource_drop_live_only132:                       ; preds = %resource_drop_merge128
  %ref_drop_handle134 = load ptr, ptr %local_8, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle134)
  store ptr null, ptr %local_8, align 4
  br label %resource_drop_merge133

resource_drop_merge133:                           ; preds = %resource_drop_live_only132, %resource_drop_merge128
  %resource_drop_flag135 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed136 = icmp eq i64 %resource_drop_flag135, 0
  br i1 %resource_drop_not_consumed136, label %resource_drop_live_only137, label %resource_drop_merge138

resource_drop_live_only137:                       ; preds = %resource_drop_merge133
  %ref_drop_handle139 = load ptr, ptr %local_5, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle139)
  store ptr null, ptr %local_5, align 4
  br label %resource_drop_merge138

resource_drop_merge138:                           ; preds = %resource_drop_live_only137, %resource_drop_merge133
  %resource_drop_flag140 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed141 = icmp eq i64 %resource_drop_flag140, 0
  br i1 %resource_drop_not_consumed141, label %resource_drop_live_only142, label %resource_drop_merge143

resource_drop_live_only142:                       ; preds = %resource_drop_merge138
  %ref_drop_handle144 = load ptr, ptr %local_2, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle144)
  store ptr null, ptr %local_2, align 4
  br label %resource_drop_merge143

resource_drop_merge143:                           ; preds = %resource_drop_live_only142, %resource_drop_merge138
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

resource_drop_live_only153:                       ; preds = %bb13
  %ref_drop_handle155 = load ptr, ptr %local_11, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle155)
  store ptr null, ptr %local_11, align 4
  br label %resource_drop_merge154

resource_drop_merge154:                           ; preds = %resource_drop_live_only153, %bb13
  %resource_drop_flag156 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed157 = icmp eq i64 %resource_drop_flag156, 0
  br i1 %resource_drop_not_consumed157, label %resource_drop_live_only158, label %resource_drop_merge159

resource_drop_live_only158:                       ; preds = %resource_drop_merge154
  %ref_drop_handle160 = load ptr, ptr %local_8, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle160)
  store ptr null, ptr %local_8, align 4
  br label %resource_drop_merge159

resource_drop_merge159:                           ; preds = %resource_drop_live_only158, %resource_drop_merge154
  %resource_drop_flag161 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed162 = icmp eq i64 %resource_drop_flag161, 0
  br i1 %resource_drop_not_consumed162, label %resource_drop_live_only163, label %resource_drop_merge164

resource_drop_live_only163:                       ; preds = %resource_drop_merge159
  %ref_drop_handle165 = load ptr, ptr %local_5, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle165)
  store ptr null, ptr %local_5, align 4
  br label %resource_drop_merge164

resource_drop_merge164:                           ; preds = %resource_drop_live_only163, %resource_drop_merge159
  %resource_drop_flag166 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed167 = icmp eq i64 %resource_drop_flag166, 0
  br i1 %resource_drop_not_consumed167, label %resource_drop_live_only168, label %resource_drop_merge169

resource_drop_live_only168:                       ; preds = %resource_drop_merge164
  %ref_drop_handle170 = load ptr, ptr %local_2, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle170)
  store ptr null, ptr %local_2, align 4
  br label %resource_drop_merge169

resource_drop_merge169:                           ; preds = %resource_drop_live_only168, %resource_drop_merge164
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

resource_drop_live_only181:                       ; preds = %bb15
  %ref_drop_handle183 = load ptr, ptr %local_11, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle183)
  store ptr null, ptr %local_11, align 4
  br label %resource_drop_merge182

resource_drop_merge182:                           ; preds = %resource_drop_live_only181, %bb15
  %resource_drop_flag184 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed185 = icmp eq i64 %resource_drop_flag184, 0
  br i1 %resource_drop_not_consumed185, label %resource_drop_live_only186, label %resource_drop_merge187

resource_drop_live_only186:                       ; preds = %resource_drop_merge182
  %ref_drop_handle188 = load ptr, ptr %local_8, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle188)
  store ptr null, ptr %local_8, align 4
  br label %resource_drop_merge187

resource_drop_merge187:                           ; preds = %resource_drop_live_only186, %resource_drop_merge182
  %resource_drop_flag189 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed190 = icmp eq i64 %resource_drop_flag189, 0
  br i1 %resource_drop_not_consumed190, label %resource_drop_live_only191, label %resource_drop_merge192

resource_drop_live_only191:                       ; preds = %resource_drop_merge187
  %ref_drop_handle193 = load ptr, ptr %local_5, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle193)
  store ptr null, ptr %local_5, align 4
  br label %resource_drop_merge192

resource_drop_merge192:                           ; preds = %resource_drop_live_only191, %resource_drop_merge187
  %resource_drop_flag194 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed195 = icmp eq i64 %resource_drop_flag194, 0
  br i1 %resource_drop_not_consumed195, label %resource_drop_live_only196, label %resource_drop_merge197

resource_drop_live_only196:                       ; preds = %resource_drop_merge192
  %ref_drop_handle198 = load ptr, ptr %local_2, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle198)
  store ptr null, ptr %local_2, align 4
  br label %resource_drop_merge197

resource_drop_merge197:                           ; preds = %resource_drop_live_only196, %resource_drop_merge192
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

resource_drop_live_only209:                       ; preds = %bb17
  %ref_drop_handle211 = load ptr, ptr %local_11, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle211)
  store ptr null, ptr %local_11, align 4
  br label %resource_drop_merge210

resource_drop_merge210:                           ; preds = %resource_drop_live_only209, %bb17
  %resource_drop_flag212 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed213 = icmp eq i64 %resource_drop_flag212, 0
  br i1 %resource_drop_not_consumed213, label %resource_drop_live_only214, label %resource_drop_merge215

resource_drop_live_only214:                       ; preds = %resource_drop_merge210
  %ref_drop_handle216 = load ptr, ptr %local_8, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle216)
  store ptr null, ptr %local_8, align 4
  br label %resource_drop_merge215

resource_drop_merge215:                           ; preds = %resource_drop_live_only214, %resource_drop_merge210
  %resource_drop_flag217 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed218 = icmp eq i64 %resource_drop_flag217, 0
  br i1 %resource_drop_not_consumed218, label %resource_drop_live_only219, label %resource_drop_merge220

resource_drop_live_only219:                       ; preds = %resource_drop_merge215
  %ref_drop_handle221 = load ptr, ptr %local_5, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle221)
  store ptr null, ptr %local_5, align 4
  br label %resource_drop_merge220

resource_drop_merge220:                           ; preds = %resource_drop_live_only219, %resource_drop_merge215
  %resource_drop_flag222 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed223 = icmp eq i64 %resource_drop_flag222, 0
  br i1 %resource_drop_not_consumed223, label %resource_drop_live_only224, label %resource_drop_merge225

resource_drop_live_only224:                       ; preds = %resource_drop_merge220
  %ref_drop_handle226 = load ptr, ptr %local_2, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle226)
  store ptr null, ptr %local_2, align 4
  br label %resource_drop_merge225

resource_drop_merge225:                           ; preds = %resource_drop_live_only224, %resource_drop_merge220
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

resource_drop_live_only237:                       ; preds = %bb19
  %ref_drop_handle239 = load ptr, ptr %local_11, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle239)
  store ptr null, ptr %local_11, align 4
  br label %resource_drop_merge238

resource_drop_merge238:                           ; preds = %resource_drop_live_only237, %bb19
  %resource_drop_flag240 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed241 = icmp eq i64 %resource_drop_flag240, 0
  br i1 %resource_drop_not_consumed241, label %resource_drop_live_only242, label %resource_drop_merge243

resource_drop_live_only242:                       ; preds = %resource_drop_merge238
  %ref_drop_handle244 = load ptr, ptr %local_8, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle244)
  store ptr null, ptr %local_8, align 4
  br label %resource_drop_merge243

resource_drop_merge243:                           ; preds = %resource_drop_live_only242, %resource_drop_merge238
  %resource_drop_flag245 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed246 = icmp eq i64 %resource_drop_flag245, 0
  br i1 %resource_drop_not_consumed246, label %resource_drop_live_only247, label %resource_drop_merge248

resource_drop_live_only247:                       ; preds = %resource_drop_merge243
  %ref_drop_handle249 = load ptr, ptr %local_5, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle249)
  store ptr null, ptr %local_5, align 4
  br label %resource_drop_merge248

resource_drop_merge248:                           ; preds = %resource_drop_live_only247, %resource_drop_merge243
  %resource_drop_flag250 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed251 = icmp eq i64 %resource_drop_flag250, 0
  br i1 %resource_drop_not_consumed251, label %resource_drop_live_only252, label %resource_drop_merge253

resource_drop_live_only252:                       ; preds = %resource_drop_merge248
  %ref_drop_handle254 = load ptr, ptr %local_2, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle254)
  store ptr null, ptr %local_2, align 4
  br label %resource_drop_merge253

resource_drop_merge253:                           ; preds = %resource_drop_live_only252, %resource_drop_merge248
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

cancel_exit259:                                   ; preds = %bb20
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_29)
  %resource_drop_flag261 = load i64, ptr %local_12, align 8
  %resource_drop_not_consumed262 = icmp eq i64 %resource_drop_flag261, 0
  br i1 %resource_drop_not_consumed262, label %resource_drop_live_only263, label %resource_drop_merge264

after_cooperate260:                               ; preds = %bb20
  br label %bb8

resource_drop_live_only263:                       ; preds = %cancel_exit259
  %ref_drop_handle265 = load ptr, ptr %local_11, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle265)
  store ptr null, ptr %local_11, align 4
  br label %resource_drop_merge264

resource_drop_merge264:                           ; preds = %resource_drop_live_only263, %cancel_exit259
  %resource_drop_flag266 = load i64, ptr %local_9, align 8
  %resource_drop_not_consumed267 = icmp eq i64 %resource_drop_flag266, 0
  br i1 %resource_drop_not_consumed267, label %resource_drop_live_only268, label %resource_drop_merge269

resource_drop_live_only268:                       ; preds = %resource_drop_merge264
  %ref_drop_handle270 = load ptr, ptr %local_8, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle270)
  store ptr null, ptr %local_8, align 4
  br label %resource_drop_merge269

resource_drop_merge269:                           ; preds = %resource_drop_live_only268, %resource_drop_merge264
  %resource_drop_flag271 = load i64, ptr %local_6, align 8
  %resource_drop_not_consumed272 = icmp eq i64 %resource_drop_flag271, 0
  br i1 %resource_drop_not_consumed272, label %resource_drop_live_only273, label %resource_drop_merge274

resource_drop_live_only273:                       ; preds = %resource_drop_merge269
  %ref_drop_handle275 = load ptr, ptr %local_5, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle275)
  store ptr null, ptr %local_5, align 4
  br label %resource_drop_merge274

resource_drop_merge274:                           ; preds = %resource_drop_live_only273, %resource_drop_merge269
  %resource_drop_flag276 = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed277 = icmp eq i64 %resource_drop_flag276, 0
  br i1 %resource_drop_not_consumed277, label %resource_drop_live_only278, label %resource_drop_merge279

resource_drop_live_only278:                       ; preds = %resource_drop_merge274
  %ref_drop_handle280 = load ptr, ptr %local_2, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle280)
  store ptr null, ptr %local_2, align 4
  br label %resource_drop_merge279

resource_drop_merge279:                           ; preds = %resource_drop_live_only278, %resource_drop_merge274
  ret i64 0
}

define internal ptr @expired() {
entry:
  %return_slot = alloca ptr, align 4
  %local_0 = alloca i64, align 8
  %local_1 = alloca ptr, align 4
  %local_2 = alloca ptr, align 4
  %local_3 = alloca i64, align 8
  %local_4 = alloca ptr, align 4
  br label %bb0

bb0:                                              ; preds = %entry
  store i64 1, ptr %local_0, align 8
  %rc_new = call ptr @hew_rc_new(ptr %local_0, i32 8, i32 8, ptr null)
  store ptr %rc_new, ptr %local_1, align 4
  %move_load = load ptr, ptr %local_1, align 4
  store ptr %move_load, ptr %local_2, align 4
  store i64 0, ptr %local_3, align 8
  %rc_borrow_handle = load ptr, ptr %local_2, align 4
  %rc_handle_result = call ptr @hew_rc_downgrade(ptr %rc_borrow_handle)
  store ptr %rc_handle_result, ptr %local_4, align 4
  %move_load1 = load ptr, ptr %local_4, align 4
  store ptr %move_load1, ptr %return_slot, align 4
  %resource_drop_flag = load i64, ptr %local_3, align 8
  %resource_drop_not_consumed = icmp eq i64 %resource_drop_flag, 0
  br i1 %resource_drop_not_consumed, label %resource_drop_live_only, label %resource_drop_merge

resource_drop_live_only:                          ; preds = %bb0
  %ref_drop_handle = load ptr, ptr %local_2, align 4
  call void @hew_rc_drop(ptr %ref_drop_handle)
  store ptr null, ptr %local_2, align 4
  br label %resource_drop_merge

resource_drop_merge:                              ; preds = %resource_drop_live_only, %bb0
  %ret_val = load ptr, ptr %return_slot, align 4
  ret ptr %ret_val
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
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_result = call ptr @expired()
  store ptr %call_result, ptr %local_0, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_0, align 4
  store ptr %move_load, ptr %local_1, align 4
  store i64 0, ptr %local_2, align 8
  %weak_upgrade_handle = load ptr, ptr %local_1, align 4
  %weak_upgrade = call ptr @hew_weak_upgrade_rc(ptr %weak_upgrade_handle)
  %machine_tag_ptr = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %local_4, i32 0, i32 0
  %machine_payload_ptr = getelementptr inbounds nuw %"Option$$Rc$li64$g", ptr %local_4, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { ptr }, ptr %machine_payload_ptr, i32 0, i32 0
  %weak_upgrade_is_none = icmp eq ptr %weak_upgrade, null
  br i1 %weak_upgrade_is_none, label %weak_upgrade_none, label %weak_upgrade_some

bb2:                                              ; preds = %after_cooperate13, %after_cooperate8
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_4)
  store %"Option$$Rc$li64$g" zeroinitializer, ptr %local_4, align 4
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
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_4)
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
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_4)
  %resource_drop_flag32 = load i64, ptr %local_2, align 8
  %resource_drop_not_consumed33 = icmp eq i64 %resource_drop_flag32, 0
  br i1 %resource_drop_not_consumed33, label %resource_drop_live_only34, label %resource_drop_merge35

bb9:                                              ; preds = %bb7
  %move_load37 = load i64, ptr %local_14, align 8
  store i64 %move_load37, ptr %return_slot, align 8
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_4)
  %resource_drop_flag38 = load i64, ptr %local_2, align 8
  %resource_drop_not_consumed39 = icmp eq i64 %resource_drop_flag38, 0
  br i1 %resource_drop_not_consumed39, label %resource_drop_live_only40, label %resource_drop_merge41

cancel_exit:                                      ; preds = %entry
  ret i64 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

weak_upgrade_some:                                ; preds = %bb1
  store i8 0, ptr %machine_tag_ptr, align 1
  store ptr %weak_upgrade, ptr %machine_variant_field_ptr, align 4
  br label %weak_upgrade_cont

weak_upgrade_none:                                ; preds = %bb1
  store i8 1, ptr %machine_tag_ptr, align 1
  store ptr null, ptr %machine_variant_field_ptr, align 4
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
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_4)
  %resource_drop_flag = load i64, ptr %local_2, align 8
  %resource_drop_not_consumed = icmp eq i64 %resource_drop_flag, 0
  br i1 %resource_drop_not_consumed, label %resource_drop_live_only, label %resource_drop_merge

after_cooperate8:                                 ; preds = %bb3
  br label %bb2

resource_drop_live_only:                          ; preds = %cancel_exit7
  %ref_drop_handle = load ptr, ptr %local_1, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle)
  store ptr null, ptr %local_1, align 4
  br label %resource_drop_merge

resource_drop_merge:                              ; preds = %resource_drop_live_only, %cancel_exit7
  ret i64 0

cancel_exit12:                                    ; preds = %bb4
  call void @"__hew_enum_drop_inplace_Option$$Rc$li64$g"(ptr %local_4)
  %resource_drop_flag14 = load i64, ptr %local_2, align 8
  %resource_drop_not_consumed15 = icmp eq i64 %resource_drop_flag14, 0
  br i1 %resource_drop_not_consumed15, label %resource_drop_live_only16, label %resource_drop_merge17

after_cooperate13:                                ; preds = %bb4
  br label %bb2

resource_drop_live_only16:                        ; preds = %cancel_exit12
  %ref_drop_handle18 = load ptr, ptr %local_1, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle18)
  store ptr null, ptr %local_1, align 4
  br label %resource_drop_merge17

resource_drop_merge17:                            ; preds = %resource_drop_live_only16, %cancel_exit12
  ret i64 0

resource_drop_live_only21:                        ; preds = %bb5
  %ref_drop_handle23 = load ptr, ptr %local_1, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle23)
  store ptr null, ptr %local_1, align 4
  br label %resource_drop_merge22

resource_drop_merge22:                            ; preds = %resource_drop_live_only21, %bb5
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

resource_drop_live_only34:                        ; preds = %bb8
  %ref_drop_handle36 = load ptr, ptr %local_1, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle36)
  store ptr null, ptr %local_1, align 4
  br label %resource_drop_merge35

resource_drop_merge35:                            ; preds = %resource_drop_live_only34, %bb8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

resource_drop_live_only40:                        ; preds = %bb9
  %ref_drop_handle42 = load ptr, ptr %local_1, align 4
  call void @hew_weak_drop_rc(ptr %ref_drop_handle42)
  store ptr null, ptr %local_1, align 4
  br label %resource_drop_merge41

resource_drop_merge41:                            ; preds = %resource_drop_live_only40, %bb9
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val
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
  %hew_duration_nanos = load i64, ptr %local_0, align 8
  %hew_duration_nanos_call = call i64 @hew_duration_nanos(i64 %hew_duration_nanos)
  store i64 %hew_duration_nanos_call, ptr %local_1, align 8
  %call_arg = load i64, ptr %local_1, align 8
  %call_result = call ptr @hew_i64_to_string(i64 %call_arg)
  store ptr %call_result, ptr %local_2, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
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
  store ptr %clone_helper_f1, ptr %dst_f1_ptr, align 4
  br label %success

step_0_clone:                                     ; preds = %entry
  %src_f1_ptr = getelementptr inbounds nuw %CrashInfo, ptr %0, i32 0, i32 1
  %src_f1 = load ptr, ptr %src_f1_ptr, align 4
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
  %drop_f1 = load ptr, ptr %drop_f1_ptr, align 4
  call void @hew_string_drop(ptr %drop_f1)
  br label %done

done:                                             ; preds = %do_drop, %entry
  ret void
}

declare void @hew_string_drop(ptr)

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

define internal void @__hew_record_overwrite_release_CrashInfo(ptr %0, ptr %1) {
entry:
  %ow_slot_0 = alloca ptr, align 4
  store ptr null, ptr %ow_slot_0, align 4
  %ow_new_d0_f1_ptr = getelementptr inbounds nuw %CrashInfo, ptr %1, i32 0, i32 1
  %ow_new_d0_f1_leaf = load ptr, ptr %ow_new_d0_f1_ptr, align 4
  store ptr %ow_new_d0_f1_leaf, ptr %ow_slot_0, align 4
  %ow_old_d0_f1_ptr = getelementptr inbounds nuw %CrashInfo, ptr %0, i32 0, i32 1
  %ow_old_d0_f1_val = load ptr, ptr %ow_old_d0_f1_ptr, align 4
  %ow_old_d0_f1_int = ptrtoint ptr %ow_old_d0_f1_val to i64
  %ow_old_d0_f1_cmp0_leaf = load ptr, ptr %ow_slot_0, align 4
  %ow_old_d0_f1_cmp0_int = ptrtoint ptr %ow_old_d0_f1_cmp0_leaf to i64
  %ow_old_d0_f1_cmp0_eq = icmp eq i64 %ow_old_d0_f1_int, %ow_old_d0_f1_cmp0_int
  %ow_old_d0_f1_matched0 = or i1 false, %ow_old_d0_f1_cmp0_eq
  %ow_old_d0_f1_neutralized = select i1 %ow_old_d0_f1_matched0, ptr null, ptr %ow_old_d0_f1_val
  store ptr %ow_old_d0_f1_neutralized, ptr %ow_old_d0_f1_ptr, align 4
  call void @__hew_record_drop_inplace_CrashInfo(ptr %0)
  ret void
}

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

declare ptr @hew_rc_downgrade(ptr)

declare ptr @hew_weak_clone_rc(ptr)

declare i32 @hew_rc_strong_count(ptr)

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.smul.with.overflow.i64(i64, i64) #1

declare void @hew_weak_drop_rc(ptr)

declare i32 @hew_rc_weak_count(ptr)

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64) #1

declare i32 @hew_rc_is_unique(ptr)

declare ptr @hew_weak_upgrade_rc(ptr)

declare void @hew_rc_set(ptr, ptr)

declare ptr @hew_rc_get(ptr)

attributes #0 = { cold noreturn nounwind memory(inaccessiblemem: write) }
attributes #1 = { nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none) }
