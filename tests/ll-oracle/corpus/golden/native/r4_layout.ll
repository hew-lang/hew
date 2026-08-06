; ModuleID = 'r4_layout'
source_filename = "r4_layout"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "aarch64-apple-macosx13.0"

%Point = type { i64, i64 }
%Color = type { i8, [1 x i8] }
%"Option$$Point" = type { i8, [2 x i64] }

@__hew_layout_new_16_8_plain = private constant { i64, i64, i8 } { i64 16, i64 8, i8 0 }
@__hew_layout_push_16_8_plain = private constant { i64, i64, i8 } { i64 16, i64 8, i8 0 }
@__hew_layout_get_16_8_plain = private constant { i64, i64, i8 } { i64 16, i64 8, i8 0 }
@hew_layout_key_string = external constant i8
@__hew_map_value_layout_16_8_plain = private constant { i64, i64, i8, ptr, ptr } { i64 16, i64 8, i8 0, ptr null, ptr null }
@str_lit = private unnamed_addr constant [7 x i8] c"origin\00", align 1
@str_lit.1 = private unnamed_addr constant [7 x i8] c"origin\00", align 1
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

define internal %Point @make_point(i64 %0, i64 %1) {
entry:
  %return_slot = alloca %Point, align 8
  %local_0 = alloca i64, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca %Point, align 8
  store i64 %0, ptr %local_0, align 8
  store i64 %1, ptr %local_1, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  %field_0_init_ptr = getelementptr inbounds nuw %Point, ptr %local_2, i32 0, i32 0
  %field_0_init_src = load i64, ptr %local_0, align 8
  store i64 %field_0_init_src, ptr %field_0_init_ptr, align 8
  %field_1_init_ptr = getelementptr inbounds nuw %Point, ptr %local_2, i32 0, i32 1
  %field_1_init_src = load i64, ptr %local_1, align 8
  store i64 %field_1_init_src, ptr %field_1_init_ptr, align 8
  %move_load = load %Point, ptr %local_2, align 8
  store %Point %move_load, ptr %return_slot, align 8
  %ret_val = load %Point, ptr %return_slot, align 8
  ret %Point %ret_val
}

define internal i64 @color_code(%Color %0) {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca %Color, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i64, align 8
  %local_5 = alloca i8, align 1
  %local_6 = alloca i64, align 8
  %local_7 = alloca i8, align 1
  %local_8 = alloca i64, align 8
  %local_9 = alloca i8, align 1
  %local_10 = alloca i64, align 8
  %local_11 = alloca i64, align 8
  %local_12 = alloca i64, align 8
  store %Color %0, ptr %local_0, align 1
  store i64 0, ptr %local_1, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  store i64 0, ptr %local_1, align 8
  %machine_tag_ptr = getelementptr inbounds nuw %Color, ptr %local_0, i32 0, i32 0
  %move_iN_load = load i8, ptr %machine_tag_ptr, align 1
  %move_iN_zext = zext i8 %move_iN_load to i64
  store i64 %move_iN_zext, ptr %local_3, align 8
  store i64 0, ptr %local_4, align 8
  %cmp_lhs = load i64, ptr %local_3, align 8
  %cmp_rhs = load i64, ptr %local_4, align 8
  %cmp_bit = icmp eq i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_5, align 1
  %cond_load = load i8, ptr %local_5, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb2, label %bb6

bb1:                                              ; preds = %after_cooperate27, %after_cooperate18, %after_cooperate9
  %move_load = load i64, ptr %local_2, align 8
  store i64 %move_load, ptr %return_slot, align 8
  %carrier_drop_flag1 = load i64, ptr %local_1, align 8
  %carrier_drop_live2 = icmp eq i64 %carrier_drop_flag1, 0
  br i1 %carrier_drop_live2, label %carrier_drop_live_only3, label %carrier_drop_merge4

bb2:                                              ; preds = %bb0
  store i64 0, ptr %local_10, align 8
  %move_load5 = load i64, ptr %local_10, align 8
  store i64 %move_load5, ptr %local_2, align 8
  %hew_actor_cooperate6 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel7 = icmp eq i32 %hew_actor_cooperate6, 2
  br i1 %hew_cooperate_is_cancel7, label %cancel_exit8, label %after_cooperate9

bb3:                                              ; preds = %bb6
  store i64 1, ptr %local_11, align 8
  %move_load14 = load i64, ptr %local_11, align 8
  store i64 %move_load14, ptr %local_2, align 8
  %hew_actor_cooperate15 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel16 = icmp eq i32 %hew_actor_cooperate15, 2
  br i1 %hew_cooperate_is_cancel16, label %cancel_exit17, label %after_cooperate18

bb4:                                              ; preds = %bb7
  store i64 2, ptr %local_12, align 8
  %move_load23 = load i64, ptr %local_12, align 8
  store i64 %move_load23, ptr %local_2, align 8
  %hew_actor_cooperate24 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel25 = icmp eq i32 %hew_actor_cooperate24, 2
  br i1 %hew_cooperate_is_cancel25, label %cancel_exit26, label %after_cooperate27

bb5:                                              ; preds = %bb7
  %carrier_drop_flag32 = load i64, ptr %local_1, align 8
  %carrier_drop_live33 = icmp eq i64 %carrier_drop_flag32, 0
  br i1 %carrier_drop_live33, label %carrier_drop_live_only34, label %carrier_drop_merge35

bb6:                                              ; preds = %bb0
  store i64 1, ptr %local_6, align 8
  %cmp_lhs36 = load i64, ptr %local_3, align 8
  %cmp_rhs37 = load i64, ptr %local_6, align 8
  %cmp_bit38 = icmp eq i64 %cmp_lhs36, %cmp_rhs37
  %cmp_zext39 = zext i1 %cmp_bit38 to i8
  store i8 %cmp_zext39, ptr %local_7, align 1
  %cond_load40 = load i8, ptr %local_7, align 1
  %cond_nz41 = icmp ne i8 %cond_load40, 0
  br i1 %cond_nz41, label %bb3, label %bb7

bb7:                                              ; preds = %bb6
  store i64 2, ptr %local_8, align 8
  %cmp_lhs42 = load i64, ptr %local_3, align 8
  %cmp_rhs43 = load i64, ptr %local_8, align 8
  %cmp_bit44 = icmp eq i64 %cmp_lhs42, %cmp_rhs43
  %cmp_zext45 = zext i1 %cmp_bit44 to i8
  store i8 %cmp_zext45, ptr %local_9, align 1
  %cond_load46 = load i8, ptr %local_9, align 1
  %cond_nz47 = icmp ne i8 %cond_load46, 0
  br i1 %cond_nz47, label %bb4, label %bb5

cancel_exit:                                      ; preds = %entry
  %carrier_drop_flag = load i64, ptr %local_1, align 8
  %carrier_drop_live = icmp eq i64 %carrier_drop_flag, 0
  br i1 %carrier_drop_live, label %carrier_drop_live_only, label %carrier_drop_merge

after_cooperate:                                  ; preds = %entry
  br label %bb0

carrier_drop_live_only:                           ; preds = %cancel_exit
  call void @__hew_enum_drop_inplace_Color(ptr %local_0)
  br label %carrier_drop_merge

carrier_drop_merge:                               ; preds = %carrier_drop_live_only, %cancel_exit
  ret i64 0

carrier_drop_live_only3:                          ; preds = %bb1
  call void @__hew_enum_drop_inplace_Color(ptr %local_0)
  br label %carrier_drop_merge4

carrier_drop_merge4:                              ; preds = %carrier_drop_live_only3, %bb1
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val

cancel_exit8:                                     ; preds = %bb2
  %carrier_drop_flag10 = load i64, ptr %local_1, align 8
  %carrier_drop_live11 = icmp eq i64 %carrier_drop_flag10, 0
  br i1 %carrier_drop_live11, label %carrier_drop_live_only12, label %carrier_drop_merge13

after_cooperate9:                                 ; preds = %bb2
  br label %bb1

carrier_drop_live_only12:                         ; preds = %cancel_exit8
  call void @__hew_enum_drop_inplace_Color(ptr %local_0)
  br label %carrier_drop_merge13

carrier_drop_merge13:                             ; preds = %carrier_drop_live_only12, %cancel_exit8
  ret i64 0

cancel_exit17:                                    ; preds = %bb3
  %carrier_drop_flag19 = load i64, ptr %local_1, align 8
  %carrier_drop_live20 = icmp eq i64 %carrier_drop_flag19, 0
  br i1 %carrier_drop_live20, label %carrier_drop_live_only21, label %carrier_drop_merge22

after_cooperate18:                                ; preds = %bb3
  br label %bb1

carrier_drop_live_only21:                         ; preds = %cancel_exit17
  call void @__hew_enum_drop_inplace_Color(ptr %local_0)
  br label %carrier_drop_merge22

carrier_drop_merge22:                             ; preds = %carrier_drop_live_only21, %cancel_exit17
  ret i64 0

cancel_exit26:                                    ; preds = %bb4
  %carrier_drop_flag28 = load i64, ptr %local_1, align 8
  %carrier_drop_live29 = icmp eq i64 %carrier_drop_flag28, 0
  br i1 %carrier_drop_live29, label %carrier_drop_live_only30, label %carrier_drop_merge31

after_cooperate27:                                ; preds = %bb4
  br label %bb1

carrier_drop_live_only30:                         ; preds = %cancel_exit26
  call void @__hew_enum_drop_inplace_Color(ptr %local_0)
  br label %carrier_drop_merge31

carrier_drop_merge31:                             ; preds = %carrier_drop_live_only30, %cancel_exit26
  ret i64 0

carrier_drop_live_only34:                         ; preds = %bb5
  call void @__hew_enum_drop_inplace_Color(ptr %local_0)
  br label %carrier_drop_merge35

carrier_drop_merge35:                             ; preds = %carrier_drop_live_only34, %bb5
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable
}

define i64 @main() {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca i64, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca %Point, align 8
  %local_3 = alloca %Point, align 8
  %local_4 = alloca i64, align 8
  %local_5 = alloca i64, align 8
  %local_6 = alloca i64, align 8
  %local_7 = alloca i8, align 1
  %local_8 = alloca ptr, align 8
  %local_9 = alloca ptr, align 8
  %local_10 = alloca i64, align 8
  %local_11 = alloca i64, align 8
  %local_12 = alloca %Point, align 8
  %local_13 = alloca i64, align 8
  %local_14 = alloca i64, align 8
  %local_15 = alloca %Point, align 8
  %local_16 = alloca i64, align 8
  %local_17 = alloca i64, align 8
  %local_18 = alloca i8, align 1
  %local_19 = alloca %Point, align 8
  %local_20 = alloca %Point, align 8
  %local_21 = alloca i64, align 8
  %local_22 = alloca ptr, align 8
  %local_23 = alloca ptr, align 8
  %local_24 = alloca ptr, align 8
  %local_25 = alloca i64, align 8
  %local_26 = alloca i64, align 8
  %local_27 = alloca %Point, align 8
  %local_28 = alloca i8, align 1
  %local_29 = alloca ptr, align 8
  %local_30 = alloca %"Option$$Point", align 8
  %local_31 = alloca i64, align 8
  %local_32 = alloca i64, align 8
  %local_33 = alloca i8, align 1
  %local_34 = alloca i64, align 8
  %local_35 = alloca i8, align 1
  %local_36 = alloca %Point, align 8
  %local_37 = alloca i64, align 8
  %local_38 = alloca i64, align 8
  %local_39 = alloca i64, align 8
  %local_40 = alloca i8, align 1
  %local_41 = alloca i64, align 8
  %local_42 = alloca %Color, align 8
  %local_43 = alloca i64, align 8
  %local_44 = alloca i64, align 8
  %local_45 = alloca i64, align 8
  %local_46 = alloca %Color, align 8
  %helper_crash_cleanup_token_9 = alloca i64, align 8
  store i64 0, ptr %helper_crash_cleanup_token_9, align 8
  %helper_crash_cleanup_active_9 = alloca i1, align 1
  store i1 false, ptr %helper_crash_cleanup_active_9, align 1
  %helper_crash_cleanup_token_23 = alloca i64, align 8
  store i64 0, ptr %helper_crash_cleanup_token_23, align 8
  %helper_crash_cleanup_active_23 = alloca i1, align 1
  store i1 false, ptr %helper_crash_cleanup_active_23, align 1
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  store i64 3, ptr %local_0, align 8
  store i64 4, ptr %local_1, align 8
  %call_arg = load i64, ptr %local_0, align 8
  %call_arg1 = load i64, ptr %local_1, align 8
  %call_result = call %Point @make_point(i64 %call_arg, i64 %call_arg1)
  store %Point %call_result, ptr %local_2, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load %Point, ptr %local_2, align 8
  store %Point %move_load, ptr %local_3, align 8
  %field_0_load_ptr = getelementptr inbounds nuw %Point, ptr %local_3, i32 0, i32 0
  %field_0_load = load i64, ptr %field_0_load_ptr, align 8
  store i64 %field_0_load, ptr %local_4, align 8
  %field_1_load_ptr = getelementptr inbounds nuw %Point, ptr %local_3, i32 0, i32 1
  %field_1_load = load i64, ptr %field_1_load_ptr, align 8
  store i64 %field_1_load, ptr %local_5, align 8
  %checked_lhs = load i64, ptr %local_4, align 8
  %checked_rhs = load i64, ptr %local_5, align 8
  %with_overflow = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_6, align 8
  store i8 %checked_overflow_widen, ptr %local_7, align 1
  %cond_load = load i8, ptr %local_7, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb3:                                              ; preds = %bb1
  %print_arg = load i64, ptr %local_6, align 8
  call void @hew_print_value(i8 1, i64 %print_arg, i1 true)
  br label %bb4

bb4:                                              ; preds = %bb3
  %hew_vec_new_with_layout_call = call ptr @hew_vec_new_with_layout(ptr @__hew_layout_new_16_8_plain)
  store ptr %hew_vec_new_with_layout_call, ptr %local_8, align 8
  br label %bb5

bb5:                                              ; preds = %bb4
  %helper_crash_cleanup_was_active = load i1, ptr %helper_crash_cleanup_active_9, align 1
  br i1 %helper_crash_cleanup_was_active, label %helper_crash_cleanup_deactivate, label %helper_crash_cleanup_deactivate_merge

bb6:                                              ; preds = %frame_cleanup_registered
  store i64 5, ptr %local_13, align 8
  store i64 5, ptr %local_14, align 8
  %field_0_init_ptr3 = getelementptr inbounds nuw %Point, ptr %local_15, i32 0, i32 0
  %field_0_init_src4 = load i64, ptr %local_13, align 8
  store i64 %field_0_init_src4, ptr %field_0_init_ptr3, align 8
  %field_1_init_ptr5 = getelementptr inbounds nuw %Point, ptr %local_15, i32 0, i32 1
  %field_1_init_src6 = load i64, ptr %local_14, align 8
  store i64 %field_1_init_src6, ptr %field_1_init_ptr5, align 8
  %"hew_vec_push_layout arg07" = load ptr, ptr %local_9, align 8
  call void @hew_vec_push_layout(ptr %"hew_vec_push_layout arg07", ptr %local_15, ptr @__hew_layout_push_16_8_plain)
  br label %bb7

bb7:                                              ; preds = %bb6
  store i64 0, ptr %local_16, align 8
  %"hew_vec_len arg0" = load ptr, ptr %local_9, align 8
  %hew_vec_len_call = call i64 @hew_vec_len(ptr %"hew_vec_len arg0")
  store i64 %hew_vec_len_call, ptr %local_17, align 8
  %cmp_lhs = load i64, ptr %local_16, align 8
  %cmp_rhs = load i64, ptr %local_17, align 8
  %cmp_bit = icmp uge i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_18, align 1
  %cond_load8 = load i8, ptr %local_18, align 1
  %cond_nz9 = icmp ne i8 %cond_load8, 0
  br i1 %cond_nz9, label %bb8, label %bb9

bb8:                                              ; preds = %bb7
  %helper_crash_cleanup_drop_active = load i1, ptr %helper_crash_cleanup_active_9, align 1
  br i1 %helper_crash_cleanup_drop_active, label %helper_crash_cleanup_retire, label %helper_crash_cleanup_retire_merge

bb9:                                              ; preds = %bb7
  %"hew_vec_get_layout arg0" = load ptr, ptr %local_9, align 8
  %"hew_vec_get_layout arg1" = load i64, ptr %local_16, align 8
  %hew_vec_get_layout_call = call ptr @hew_vec_get_layout(ptr %"hew_vec_get_layout arg0", i64 %"hew_vec_get_layout arg1", ptr @__hew_layout_get_16_8_plain)
  %hew_vec_get_layout_load = load %Point, ptr %hew_vec_get_layout_call, align 8
  store %Point %hew_vec_get_layout_load, ptr %local_19, align 8
  %move_load10 = load %Point, ptr %local_19, align 8
  store %Point %move_load10, ptr %local_20, align 8
  %field_0_load_ptr11 = getelementptr inbounds nuw %Point, ptr %local_20, i32 0, i32 0
  %field_0_load12 = load i64, ptr %field_0_load_ptr11, align 8
  store i64 %field_0_load12, ptr %local_21, align 8
  %print_arg13 = load i64, ptr %local_21, align 8
  call void @hew_print_value(i8 1, i64 %print_arg13, i1 true)
  br label %bb10

bb10:                                             ; preds = %bb9
  %hew_hashmap_new_with_layout_call = call ptr @hew_hashmap_new_with_layout(ptr @hew_layout_key_string, ptr @__hew_map_value_layout_16_8_plain)
  store ptr %hew_hashmap_new_with_layout_call, ptr %local_22, align 8
  br label %bb11

bb11:                                             ; preds = %bb10
  %helper_crash_cleanup_was_active14 = load i1, ptr %helper_crash_cleanup_active_23, align 1
  br i1 %helper_crash_cleanup_was_active14, label %helper_crash_cleanup_deactivate15, label %helper_crash_cleanup_deactivate_merge16

bb12:                                             ; preds = %insert_overwrite_key_cont
  store ptr @str_lit.1, ptr %local_29, align 8
  %"hew_hashmap_get_layout arg0" = load ptr, ptr %local_23, align 8
  %machine_payload_ptr = getelementptr inbounds nuw %"Option$$Point", ptr %local_30, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { %Point }, ptr %machine_payload_ptr, i32 0, i32 0
  %hew_hashmap_get_clone_layout_call = call i1 @hew_hashmap_get_clone_layout(ptr %"hew_hashmap_get_layout arg0", ptr %local_29, ptr %machine_variant_field_ptr)
  br i1 %hew_hashmap_get_clone_layout_call, label %hashmap_get_some, label %hashmap_get_none

bb13:                                             ; preds = %hashmap_get_initialized
  %machine_tag_ptr32 = getelementptr inbounds nuw %"Option$$Point", ptr %local_30, i32 0, i32 0
  %move_iN_load = load i8, ptr %machine_tag_ptr32, align 1
  %move_iN_zext = zext i8 %move_iN_load to i64
  store i64 %move_iN_zext, ptr %local_31, align 8
  store i64 0, ptr %local_32, align 8
  %cmp_lhs33 = load i64, ptr %local_31, align 8
  %cmp_rhs34 = load i64, ptr %local_32, align 8
  %cmp_bit35 = icmp eq i64 %cmp_lhs33, %cmp_rhs34
  %cmp_zext36 = zext i1 %cmp_bit35 to i8
  store i8 %cmp_zext36, ptr %local_33, align 1
  %cond_load37 = load i8, ptr %local_33, align 1
  %cond_nz38 = icmp ne i8 %cond_load37, 0
  br i1 %cond_nz38, label %bb15, label %bb18

bb14:                                             ; preds = %after_cooperate120, %after_cooperate100
  store i64 1, ptr %local_43, align 8
  %machine_tag_ptr39 = getelementptr inbounds nuw %Color, ptr %local_42, i32 0, i32 0
  %move_iN_load_wide = load i64, ptr %local_43, align 8
  %move_iN_trunc = trunc i64 %move_iN_load_wide to i8
  store i8 %move_iN_trunc, ptr %machine_tag_ptr39, align 1
  %move_load40 = load %Color, ptr %local_42, align 1
  store %Color %move_load40, ptr %local_46, align 1
  store %Color zeroinitializer, ptr %local_42, align 1
  %call_arg41 = load %Color, ptr %local_46, align 1
  %call_result42 = call i64 @color_code(%Color %call_arg41)
  store i64 %call_result42, ptr %local_44, align 8
  br label %bb23

bb15:                                             ; preds = %bb13
  %machine_payload_ptr43 = getelementptr inbounds nuw %"Option$$Point", ptr %local_30, i32 0, i32 1
  %machine_variant_field_ptr44 = getelementptr inbounds nuw { %Point }, ptr %machine_payload_ptr43, i32 0, i32 0
  %move_load45 = load %Point, ptr %machine_variant_field_ptr44, align 8
  store %Point %move_load45, ptr %local_36, align 8
  %field_0_load_ptr46 = getelementptr inbounds nuw %Point, ptr %local_36, i32 0, i32 0
  %field_0_load47 = load i64, ptr %field_0_load_ptr46, align 8
  store i64 %field_0_load47, ptr %local_37, align 8
  %field_1_load_ptr48 = getelementptr inbounds nuw %Point, ptr %local_36, i32 0, i32 1
  %field_1_load49 = load i64, ptr %field_1_load_ptr48, align 8
  store i64 %field_1_load49, ptr %local_38, align 8
  %checked_lhs50 = load i64, ptr %local_37, align 8
  %checked_rhs51 = load i64, ptr %local_38, align 8
  %with_overflow52 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs50, i64 %checked_rhs51)
  %checked_result53 = extractvalue { i64, i1 } %with_overflow52, 0
  %checked_overflow54 = extractvalue { i64, i1 } %with_overflow52, 1
  %checked_overflow_widen55 = zext i1 %checked_overflow54 to i8
  store i64 %checked_result53, ptr %local_39, align 8
  store i8 %checked_overflow_widen55, ptr %local_40, align 1
  %cond_load56 = load i8, ptr %local_40, align 1
  %cond_nz57 = icmp ne i8 %cond_load56, 0
  br i1 %cond_nz57, label %bb19, label %bb20

bb16:                                             ; preds = %bb18
  store i64 -1, ptr %local_41, align 8
  %print_arg58 = load i64, ptr %local_41, align 8
  call void @hew_print_value(i8 1, i64 %print_arg58, i1 true)
  br label %bb22

bb17:                                             ; preds = %bb18
  %helper_crash_cleanup_drop_active59 = load i1, ptr %helper_crash_cleanup_active_23, align 1
  br i1 %helper_crash_cleanup_drop_active59, label %helper_crash_cleanup_retire60, label %helper_crash_cleanup_retire_merge61

bb18:                                             ; preds = %bb13
  store i64 1, ptr %local_34, align 8
  %cmp_lhs74 = load i64, ptr %local_31, align 8
  %cmp_rhs75 = load i64, ptr %local_34, align 8
  %cmp_bit76 = icmp eq i64 %cmp_lhs74, %cmp_rhs75
  %cmp_zext77 = zext i1 %cmp_bit76 to i8
  store i8 %cmp_zext77, ptr %local_35, align 1
  %cond_load78 = load i8, ptr %local_35, align 1
  %cond_nz79 = icmp ne i8 %cond_load78, 0
  br i1 %cond_nz79, label %bb16, label %bb17

bb19:                                             ; preds = %bb15
  %helper_crash_cleanup_drop_active80 = load i1, ptr %helper_crash_cleanup_active_23, align 1
  br i1 %helper_crash_cleanup_drop_active80, label %helper_crash_cleanup_retire81, label %helper_crash_cleanup_retire_merge82

bb20:                                             ; preds = %bb15
  %print_arg96 = load i64, ptr %local_39, align 8
  call void @hew_print_value(i8 1, i64 %print_arg96, i1 true)
  br label %bb21

bb21:                                             ; preds = %bb20
  %hew_actor_cooperate97 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel98 = icmp eq i32 %hew_actor_cooperate97, 2
  br i1 %hew_cooperate_is_cancel98, label %cancel_exit99, label %after_cooperate100

bb22:                                             ; preds = %bb16
  %hew_actor_cooperate117 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel118 = icmp eq i32 %hew_actor_cooperate117, 2
  br i1 %hew_cooperate_is_cancel118, label %cancel_exit119, label %after_cooperate120

bb23:                                             ; preds = %bb14
  %print_arg137 = load i64, ptr %local_44, align 8
  call void @hew_print_value(i8 1, i64 %print_arg137, i1 true)
  br label %bb24

bb24:                                             ; preds = %bb23
  store i64 0, ptr %local_45, align 8
  %move_load138 = load i64, ptr %local_45, align 8
  store i64 %move_load138, ptr %return_slot, align 8
  %helper_crash_cleanup_drop_active139 = load i1, ptr %helper_crash_cleanup_active_23, align 1
  br i1 %helper_crash_cleanup_drop_active139, label %helper_crash_cleanup_retire140, label %helper_crash_cleanup_retire_merge141

cancel_exit:                                      ; preds = %entry
  ret i64 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

helper_crash_cleanup_deactivate:                  ; preds = %bb5
  %helper_crash_cleanup_token = load i64, ptr %helper_crash_cleanup_token_9, align 8
  %helper_crash_cleanup_deactivate_call = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token)
  br i1 %helper_crash_cleanup_deactivate_call, label %helper_crash_cleanup_deactivate_accepted, label %helper_crash_cleanup_deactivate_rejected

helper_crash_cleanup_deactivate_merge:            ; preds = %helper_crash_cleanup_deactivate_accepted, %bb5
  %move_load2 = load ptr, ptr %local_8, align 8
  store ptr %move_load2, ptr %local_9, align 8
  %helper_crash_cleanup_prior_token = load i64, ptr %helper_crash_cleanup_token_9, align 8
  %arm_typed_crash_cleanup = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token, ptr %local_9, i64 8, i64 8, ptr @__hew_frame_cleanup_0, i32 1, i32 0)
  %frame_cleanup_arm_failed = icmp eq i64 %arm_typed_crash_cleanup, -1
  br i1 %frame_cleanup_arm_failed, label %frame_cleanup_rejected, label %frame_cleanup_registered

helper_crash_cleanup_deactivate_accepted:         ; preds = %helper_crash_cleanup_deactivate
  store i1 false, ptr %helper_crash_cleanup_active_9, align 1
  br label %helper_crash_cleanup_deactivate_merge

helper_crash_cleanup_deactivate_rejected:         ; preds = %helper_crash_cleanup_deactivate
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

frame_cleanup_registered:                         ; preds = %helper_crash_cleanup_deactivate_merge
  store i64 %arm_typed_crash_cleanup, ptr %helper_crash_cleanup_token_9, align 8
  store i1 true, ptr %helper_crash_cleanup_active_9, align 1
  store i64 10, ptr %local_10, align 8
  store i64 20, ptr %local_11, align 8
  %field_0_init_ptr = getelementptr inbounds nuw %Point, ptr %local_12, i32 0, i32 0
  %field_0_init_src = load i64, ptr %local_10, align 8
  store i64 %field_0_init_src, ptr %field_0_init_ptr, align 8
  %field_1_init_ptr = getelementptr inbounds nuw %Point, ptr %local_12, i32 0, i32 1
  %field_1_init_src = load i64, ptr %local_11, align 8
  store i64 %field_1_init_src, ptr %field_1_init_ptr, align 8
  %"hew_vec_push_layout arg0" = load ptr, ptr %local_9, align 8
  call void @hew_vec_push_layout(ptr %"hew_vec_push_layout arg0", ptr %local_12, ptr @__hew_layout_push_16_8_plain)
  br label %bb6

frame_cleanup_rejected:                           ; preds = %helper_crash_cleanup_deactivate_merge
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire:                      ; preds = %bb8
  %helper_crash_cleanup_retire_token = load i64, ptr %helper_crash_cleanup_token_9, align 8
  %helper_crash_cleanup_retire_call = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token)
  br i1 %helper_crash_cleanup_retire_call, label %helper_crash_cleanup_retire_accepted, label %helper_crash_cleanup_retire_rejected

helper_crash_cleanup_retire_merge:                ; preds = %helper_crash_cleanup_retire_accepted, %bb8
  %"hew_vec_free drop" = load ptr, ptr %local_9, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop")
  store ptr null, ptr %local_9, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire_accepted:             ; preds = %helper_crash_cleanup_retire
  store i64 0, ptr %helper_crash_cleanup_token_9, align 8
  store i1 false, ptr %helper_crash_cleanup_active_9, align 1
  br label %helper_crash_cleanup_retire_merge

helper_crash_cleanup_retire_rejected:             ; preds = %helper_crash_cleanup_retire
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_deactivate15:                ; preds = %bb11
  %helper_crash_cleanup_token17 = load i64, ptr %helper_crash_cleanup_token_23, align 8
  %helper_crash_cleanup_deactivate_call18 = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token17)
  br i1 %helper_crash_cleanup_deactivate_call18, label %helper_crash_cleanup_deactivate_accepted19, label %helper_crash_cleanup_deactivate_rejected20

helper_crash_cleanup_deactivate_merge16:          ; preds = %helper_crash_cleanup_deactivate_accepted19, %bb11
  %move_load21 = load ptr, ptr %local_22, align 8
  store ptr %move_load21, ptr %local_23, align 8
  %helper_crash_cleanup_prior_token22 = load i64, ptr %helper_crash_cleanup_token_23, align 8
  %arm_typed_crash_cleanup23 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token22, ptr %local_23, i64 8, i64 8, ptr @__hew_frame_cleanup_1, i32 1, i32 0)
  %frame_cleanup_arm_failed24 = icmp eq i64 %arm_typed_crash_cleanup23, -1
  br i1 %frame_cleanup_arm_failed24, label %frame_cleanup_rejected26, label %frame_cleanup_registered25

helper_crash_cleanup_deactivate_accepted19:       ; preds = %helper_crash_cleanup_deactivate15
  store i1 false, ptr %helper_crash_cleanup_active_23, align 1
  br label %helper_crash_cleanup_deactivate_merge16

helper_crash_cleanup_deactivate_rejected20:       ; preds = %helper_crash_cleanup_deactivate15
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

frame_cleanup_registered25:                       ; preds = %helper_crash_cleanup_deactivate_merge16
  store i64 %arm_typed_crash_cleanup23, ptr %helper_crash_cleanup_token_23, align 8
  store i1 true, ptr %helper_crash_cleanup_active_23, align 1
  store ptr @str_lit, ptr %local_24, align 8
  store i64 0, ptr %local_25, align 8
  store i64 0, ptr %local_26, align 8
  %field_0_init_ptr27 = getelementptr inbounds nuw %Point, ptr %local_27, i32 0, i32 0
  %field_0_init_src28 = load i64, ptr %local_25, align 8
  store i64 %field_0_init_src28, ptr %field_0_init_ptr27, align 8
  %field_1_init_ptr29 = getelementptr inbounds nuw %Point, ptr %local_27, i32 0, i32 1
  %field_1_init_src30 = load i64, ptr %local_26, align 8
  store i64 %field_1_init_src30, ptr %field_1_init_ptr29, align 8
  %"hew_hashmap_insert_layout arg0" = load ptr, ptr %local_23, align 8
  %hew_hashmap_insert_layout_call = call i1 @hew_hashmap_insert_layout(ptr %"hew_hashmap_insert_layout arg0", ptr %local_24, ptr %local_27)
  %insert_existed = icmp eq i1 %hew_hashmap_insert_layout_call, false
  br i1 %insert_existed, label %insert_overwrite_key_release, label %insert_overwrite_key_cont

frame_cleanup_rejected26:                         ; preds = %helper_crash_cleanup_deactivate_merge16
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

insert_overwrite_key_release:                     ; preds = %frame_cleanup_registered25
  %"hew_hashmap_insert_layout overwrite key" = load ptr, ptr %local_24, align 8
  call void @hew_string_drop(ptr %"hew_hashmap_insert_layout overwrite key")
  br label %insert_overwrite_key_cont

insert_overwrite_key_cont:                        ; preds = %insert_overwrite_key_release, %frame_cleanup_registered25
  br label %bb12

hashmap_get_none:                                 ; preds = %bb12
  %machine_tag_ptr = getelementptr inbounds nuw %"Option$$Point", ptr %local_30, i32 0, i32 0
  store i8 1, ptr %machine_tag_ptr, align 1
  br label %hashmap_get_initialized

hashmap_get_some:                                 ; preds = %bb12
  %machine_tag_ptr31 = getelementptr inbounds nuw %"Option$$Point", ptr %local_30, i32 0, i32 0
  store i8 0, ptr %machine_tag_ptr31, align 1
  br label %hashmap_get_initialized

hashmap_get_initialized:                          ; preds = %hashmap_get_some, %hashmap_get_none
  br label %bb13

helper_crash_cleanup_retire60:                    ; preds = %bb17
  %helper_crash_cleanup_retire_token62 = load i64, ptr %helper_crash_cleanup_token_23, align 8
  %helper_crash_cleanup_retire_call63 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token62)
  br i1 %helper_crash_cleanup_retire_call63, label %helper_crash_cleanup_retire_accepted64, label %helper_crash_cleanup_retire_rejected65

helper_crash_cleanup_retire_merge61:              ; preds = %helper_crash_cleanup_retire_accepted64, %bb17
  %"hew_hashmap_free_layout drop" = load ptr, ptr %local_23, align 8
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop")
  store ptr null, ptr %local_23, align 8
  %helper_crash_cleanup_drop_active66 = load i1, ptr %helper_crash_cleanup_active_9, align 1
  br i1 %helper_crash_cleanup_drop_active66, label %helper_crash_cleanup_retire67, label %helper_crash_cleanup_retire_merge68

helper_crash_cleanup_retire_accepted64:           ; preds = %helper_crash_cleanup_retire60
  store i64 0, ptr %helper_crash_cleanup_token_23, align 8
  store i1 false, ptr %helper_crash_cleanup_active_23, align 1
  br label %helper_crash_cleanup_retire_merge61

helper_crash_cleanup_retire_rejected65:           ; preds = %helper_crash_cleanup_retire60
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire67:                    ; preds = %helper_crash_cleanup_retire_merge61
  %helper_crash_cleanup_retire_token69 = load i64, ptr %helper_crash_cleanup_token_9, align 8
  %helper_crash_cleanup_retire_call70 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token69)
  br i1 %helper_crash_cleanup_retire_call70, label %helper_crash_cleanup_retire_accepted71, label %helper_crash_cleanup_retire_rejected72

helper_crash_cleanup_retire_merge68:              ; preds = %helper_crash_cleanup_retire_accepted71, %helper_crash_cleanup_retire_merge61
  %"hew_vec_free drop73" = load ptr, ptr %local_9, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop73")
  store ptr null, ptr %local_9, align 8
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire_accepted71:           ; preds = %helper_crash_cleanup_retire67
  store i64 0, ptr %helper_crash_cleanup_token_9, align 8
  store i1 false, ptr %helper_crash_cleanup_active_9, align 1
  br label %helper_crash_cleanup_retire_merge68

helper_crash_cleanup_retire_rejected72:           ; preds = %helper_crash_cleanup_retire67
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire81:                    ; preds = %bb19
  %helper_crash_cleanup_retire_token83 = load i64, ptr %helper_crash_cleanup_token_23, align 8
  %helper_crash_cleanup_retire_call84 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token83)
  br i1 %helper_crash_cleanup_retire_call84, label %helper_crash_cleanup_retire_accepted85, label %helper_crash_cleanup_retire_rejected86

helper_crash_cleanup_retire_merge82:              ; preds = %helper_crash_cleanup_retire_accepted85, %bb19
  %"hew_hashmap_free_layout drop87" = load ptr, ptr %local_23, align 8
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop87")
  store ptr null, ptr %local_23, align 8
  %helper_crash_cleanup_drop_active88 = load i1, ptr %helper_crash_cleanup_active_9, align 1
  br i1 %helper_crash_cleanup_drop_active88, label %helper_crash_cleanup_retire89, label %helper_crash_cleanup_retire_merge90

helper_crash_cleanup_retire_accepted85:           ; preds = %helper_crash_cleanup_retire81
  store i64 0, ptr %helper_crash_cleanup_token_23, align 8
  store i1 false, ptr %helper_crash_cleanup_active_23, align 1
  br label %helper_crash_cleanup_retire_merge82

helper_crash_cleanup_retire_rejected86:           ; preds = %helper_crash_cleanup_retire81
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire89:                    ; preds = %helper_crash_cleanup_retire_merge82
  %helper_crash_cleanup_retire_token91 = load i64, ptr %helper_crash_cleanup_token_9, align 8
  %helper_crash_cleanup_retire_call92 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token91)
  br i1 %helper_crash_cleanup_retire_call92, label %helper_crash_cleanup_retire_accepted93, label %helper_crash_cleanup_retire_rejected94

helper_crash_cleanup_retire_merge90:              ; preds = %helper_crash_cleanup_retire_accepted93, %helper_crash_cleanup_retire_merge82
  %"hew_vec_free drop95" = load ptr, ptr %local_9, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop95")
  store ptr null, ptr %local_9, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire_accepted93:           ; preds = %helper_crash_cleanup_retire89
  store i64 0, ptr %helper_crash_cleanup_token_9, align 8
  store i1 false, ptr %helper_crash_cleanup_active_9, align 1
  br label %helper_crash_cleanup_retire_merge90

helper_crash_cleanup_retire_rejected94:           ; preds = %helper_crash_cleanup_retire89
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

cancel_exit99:                                    ; preds = %bb21
  %helper_crash_cleanup_drop_active101 = load i1, ptr %helper_crash_cleanup_active_23, align 1
  br i1 %helper_crash_cleanup_drop_active101, label %helper_crash_cleanup_retire102, label %helper_crash_cleanup_retire_merge103

after_cooperate100:                               ; preds = %bb21
  br label %bb14

helper_crash_cleanup_retire102:                   ; preds = %cancel_exit99
  %helper_crash_cleanup_retire_token104 = load i64, ptr %helper_crash_cleanup_token_23, align 8
  %helper_crash_cleanup_retire_call105 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token104)
  br i1 %helper_crash_cleanup_retire_call105, label %helper_crash_cleanup_retire_accepted106, label %helper_crash_cleanup_retire_rejected107

helper_crash_cleanup_retire_merge103:             ; preds = %helper_crash_cleanup_retire_accepted106, %cancel_exit99
  %"hew_hashmap_free_layout drop108" = load ptr, ptr %local_23, align 8
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop108")
  store ptr null, ptr %local_23, align 8
  %helper_crash_cleanup_drop_active109 = load i1, ptr %helper_crash_cleanup_active_9, align 1
  br i1 %helper_crash_cleanup_drop_active109, label %helper_crash_cleanup_retire110, label %helper_crash_cleanup_retire_merge111

helper_crash_cleanup_retire_accepted106:          ; preds = %helper_crash_cleanup_retire102
  store i64 0, ptr %helper_crash_cleanup_token_23, align 8
  store i1 false, ptr %helper_crash_cleanup_active_23, align 1
  br label %helper_crash_cleanup_retire_merge103

helper_crash_cleanup_retire_rejected107:          ; preds = %helper_crash_cleanup_retire102
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire110:                   ; preds = %helper_crash_cleanup_retire_merge103
  %helper_crash_cleanup_retire_token112 = load i64, ptr %helper_crash_cleanup_token_9, align 8
  %helper_crash_cleanup_retire_call113 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token112)
  br i1 %helper_crash_cleanup_retire_call113, label %helper_crash_cleanup_retire_accepted114, label %helper_crash_cleanup_retire_rejected115

helper_crash_cleanup_retire_merge111:             ; preds = %helper_crash_cleanup_retire_accepted114, %helper_crash_cleanup_retire_merge103
  %"hew_vec_free drop116" = load ptr, ptr %local_9, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop116")
  store ptr null, ptr %local_9, align 8
  ret i64 0

helper_crash_cleanup_retire_accepted114:          ; preds = %helper_crash_cleanup_retire110
  store i64 0, ptr %helper_crash_cleanup_token_9, align 8
  store i1 false, ptr %helper_crash_cleanup_active_9, align 1
  br label %helper_crash_cleanup_retire_merge111

helper_crash_cleanup_retire_rejected115:          ; preds = %helper_crash_cleanup_retire110
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

cancel_exit119:                                   ; preds = %bb22
  %helper_crash_cleanup_drop_active121 = load i1, ptr %helper_crash_cleanup_active_23, align 1
  br i1 %helper_crash_cleanup_drop_active121, label %helper_crash_cleanup_retire122, label %helper_crash_cleanup_retire_merge123

after_cooperate120:                               ; preds = %bb22
  br label %bb14

helper_crash_cleanup_retire122:                   ; preds = %cancel_exit119
  %helper_crash_cleanup_retire_token124 = load i64, ptr %helper_crash_cleanup_token_23, align 8
  %helper_crash_cleanup_retire_call125 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token124)
  br i1 %helper_crash_cleanup_retire_call125, label %helper_crash_cleanup_retire_accepted126, label %helper_crash_cleanup_retire_rejected127

helper_crash_cleanup_retire_merge123:             ; preds = %helper_crash_cleanup_retire_accepted126, %cancel_exit119
  %"hew_hashmap_free_layout drop128" = load ptr, ptr %local_23, align 8
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop128")
  store ptr null, ptr %local_23, align 8
  %helper_crash_cleanup_drop_active129 = load i1, ptr %helper_crash_cleanup_active_9, align 1
  br i1 %helper_crash_cleanup_drop_active129, label %helper_crash_cleanup_retire130, label %helper_crash_cleanup_retire_merge131

helper_crash_cleanup_retire_accepted126:          ; preds = %helper_crash_cleanup_retire122
  store i64 0, ptr %helper_crash_cleanup_token_23, align 8
  store i1 false, ptr %helper_crash_cleanup_active_23, align 1
  br label %helper_crash_cleanup_retire_merge123

helper_crash_cleanup_retire_rejected127:          ; preds = %helper_crash_cleanup_retire122
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire130:                   ; preds = %helper_crash_cleanup_retire_merge123
  %helper_crash_cleanup_retire_token132 = load i64, ptr %helper_crash_cleanup_token_9, align 8
  %helper_crash_cleanup_retire_call133 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token132)
  br i1 %helper_crash_cleanup_retire_call133, label %helper_crash_cleanup_retire_accepted134, label %helper_crash_cleanup_retire_rejected135

helper_crash_cleanup_retire_merge131:             ; preds = %helper_crash_cleanup_retire_accepted134, %helper_crash_cleanup_retire_merge123
  %"hew_vec_free drop136" = load ptr, ptr %local_9, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop136")
  store ptr null, ptr %local_9, align 8
  ret i64 0

helper_crash_cleanup_retire_accepted134:          ; preds = %helper_crash_cleanup_retire130
  store i64 0, ptr %helper_crash_cleanup_token_9, align 8
  store i1 false, ptr %helper_crash_cleanup_active_9, align 1
  br label %helper_crash_cleanup_retire_merge131

helper_crash_cleanup_retire_rejected135:          ; preds = %helper_crash_cleanup_retire130
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire140:                   ; preds = %bb24
  %helper_crash_cleanup_retire_token142 = load i64, ptr %helper_crash_cleanup_token_23, align 8
  %helper_crash_cleanup_retire_call143 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token142)
  br i1 %helper_crash_cleanup_retire_call143, label %helper_crash_cleanup_retire_accepted144, label %helper_crash_cleanup_retire_rejected145

helper_crash_cleanup_retire_merge141:             ; preds = %helper_crash_cleanup_retire_accepted144, %bb24
  %"hew_hashmap_free_layout drop146" = load ptr, ptr %local_23, align 8
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop146")
  store ptr null, ptr %local_23, align 8
  %helper_crash_cleanup_drop_active147 = load i1, ptr %helper_crash_cleanup_active_9, align 1
  br i1 %helper_crash_cleanup_drop_active147, label %helper_crash_cleanup_retire148, label %helper_crash_cleanup_retire_merge149

helper_crash_cleanup_retire_accepted144:          ; preds = %helper_crash_cleanup_retire140
  store i64 0, ptr %helper_crash_cleanup_token_23, align 8
  store i1 false, ptr %helper_crash_cleanup_active_23, align 1
  br label %helper_crash_cleanup_retire_merge141

helper_crash_cleanup_retire_rejected145:          ; preds = %helper_crash_cleanup_retire140
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire148:                   ; preds = %helper_crash_cleanup_retire_merge141
  %helper_crash_cleanup_retire_token150 = load i64, ptr %helper_crash_cleanup_token_9, align 8
  %helper_crash_cleanup_retire_call151 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token150)
  br i1 %helper_crash_cleanup_retire_call151, label %helper_crash_cleanup_retire_accepted152, label %helper_crash_cleanup_retire_rejected153

helper_crash_cleanup_retire_merge149:             ; preds = %helper_crash_cleanup_retire_accepted152, %helper_crash_cleanup_retire_merge141
  %"hew_vec_free drop154" = load ptr, ptr %local_9, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop154")
  store ptr null, ptr %local_9, align 8
  %helper_crash_cleanup_return_token_9 = load i64, ptr %helper_crash_cleanup_token_9, align 8
  %helper_crash_cleanup_return_has_token_9 = icmp ne i64 %helper_crash_cleanup_return_token_9, 0
  br i1 %helper_crash_cleanup_return_has_token_9, label %helper_crash_cleanup_return_retire_9, label %helper_crash_cleanup_return_merge_9

helper_crash_cleanup_retire_accepted152:          ; preds = %helper_crash_cleanup_retire148
  store i64 0, ptr %helper_crash_cleanup_token_9, align 8
  store i1 false, ptr %helper_crash_cleanup_active_9, align 1
  br label %helper_crash_cleanup_retire_merge149

helper_crash_cleanup_retire_rejected153:          ; preds = %helper_crash_cleanup_retire148
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_return_merge_9:              ; preds = %helper_crash_cleanup_return_retire_9_accepted, %helper_crash_cleanup_retire_merge149
  %helper_crash_cleanup_return_token_23 = load i64, ptr %helper_crash_cleanup_token_23, align 8
  %helper_crash_cleanup_return_has_token_23 = icmp ne i64 %helper_crash_cleanup_return_token_23, 0
  br i1 %helper_crash_cleanup_return_has_token_23, label %helper_crash_cleanup_return_retire_23, label %helper_crash_cleanup_return_merge_23

helper_crash_cleanup_return_retire_9:             ; preds = %helper_crash_cleanup_retire_merge149
  %helper_crash_cleanup_return_retire_9_call = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_return_token_9)
  br i1 %helper_crash_cleanup_return_retire_9_call, label %helper_crash_cleanup_return_retire_9_accepted, label %helper_crash_cleanup_return_retire_9_rejected

helper_crash_cleanup_return_retire_9_accepted:    ; preds = %helper_crash_cleanup_return_retire_9
  store i64 0, ptr %helper_crash_cleanup_token_9, align 8
  store i1 false, ptr %helper_crash_cleanup_active_9, align 1
  br label %helper_crash_cleanup_return_merge_9

helper_crash_cleanup_return_retire_9_rejected:    ; preds = %helper_crash_cleanup_return_retire_9
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_return_merge_23:             ; preds = %helper_crash_cleanup_return_retire_23_accepted, %helper_crash_cleanup_return_merge_9
  %hew_lambda_drain_all_call = call i32 @hew_lambda_drain_all(i64 0)
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val

helper_crash_cleanup_return_retire_23:            ; preds = %helper_crash_cleanup_return_merge_9
  %helper_crash_cleanup_return_retire_23_call = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_return_token_23)
  br i1 %helper_crash_cleanup_return_retire_23_call, label %helper_crash_cleanup_return_retire_23_accepted, label %helper_crash_cleanup_return_retire_23_rejected

helper_crash_cleanup_return_retire_23_accepted:   ; preds = %helper_crash_cleanup_return_retire_23
  store i64 0, ptr %helper_crash_cleanup_token_23, align 8
  store i1 false, ptr %helper_crash_cleanup_active_23, align 1
  br label %helper_crash_cleanup_return_merge_23

helper_crash_cleanup_return_retire_23_rejected:   ; preds = %helper_crash_cleanup_return_retire_23
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

define internal i32 @__hew_record_clone_inplace_Point(ptr %0, ptr %1) {
entry:
  br label %success

success:                                          ; preds = %entry
  ret i32 0

fail:                                             ; No predecessors!
  ret i32 1
}

define internal void @__hew_record_drop_inplace_Point(ptr %0) {
entry:
  %rec_int = ptrtoint ptr %0 to i64
  %rec_is_null = icmp eq i64 %rec_int, 0
  br i1 %rec_is_null, label %done, label %do_drop

do_drop:                                          ; preds = %entry
  br label %done

done:                                             ; preds = %do_drop, %entry
  ret void
}

define internal void @__hew_record_overwrite_release_Point(ptr %0, ptr %1) {
entry:
  call void @__hew_record_drop_inplace_Point(ptr %0)
  ret void
}

declare i32 @hew_actor_cooperate()

define internal void @__hew_enum_drop_inplace_Color(ptr %0) {
entry:
  %enum_drop_tag_ptr = getelementptr inbounds nuw %Color, ptr %0, i32 0, i32 0
  %enum_drop_tag = load i8, ptr %enum_drop_tag_ptr, align 1
  switch i8 %enum_drop_tag, label %tag_oob_trap [
    i8 0, label %enum_drop_variant_0
    i8 1, label %enum_drop_variant_1
    i8 2, label %enum_drop_variant_2
  ]

done:                                             ; preds = %enum_drop_variant_2, %enum_drop_variant_1, %enum_drop_variant_0
  ret void

tag_oob_trap:                                     ; preds = %entry
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

enum_drop_variant_0:                              ; preds = %entry
  %enum_drop_payload_0 = getelementptr inbounds nuw %Color, ptr %0, i32 0, i32 1
  br label %done

enum_drop_variant_1:                              ; preds = %entry
  %enum_drop_payload_1 = getelementptr inbounds nuw %Color, ptr %0, i32 0, i32 1
  br label %done

enum_drop_variant_2:                              ; preds = %entry
  %enum_drop_payload_2 = getelementptr inbounds nuw %Color, ptr %0, i32 0, i32 1
  br label %done
}

declare void @hew_trap_with_code(i32)

; Function Attrs: cold noreturn nounwind memory(inaccessiblemem: write)
declare void @llvm.trap() #0

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64) #1

declare ptr @hew_vec_new_with_layout(ptr)

declare i1 @hew_cont_crash_cleanup_deactivate(i64)

define internal void @__hew_frame_cleanup_0(ptr %0) {
entry:
  %"hew_vec_free drop" = load ptr, ptr %0, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop")
  store ptr null, ptr %0, align 8
  ret void
}

declare void @hew_vec_free(ptr)

declare i64 @hew_cont_crash_cleanup_arm(i64, ptr, i64, i64, ptr, i32, i32)

declare void @hew_vec_push_layout(ptr, ptr, ptr)

declare i1 @hew_cont_crash_cleanup_retire(i64)

declare ptr @hew_vec_get_layout(ptr, i64, ptr)

declare ptr @hew_hashmap_new_with_layout(ptr, ptr)

define internal void @__hew_frame_cleanup_1(ptr %0) {
entry:
  %"hew_hashmap_free_layout drop" = load ptr, ptr %0, align 8
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop")
  store ptr null, ptr %0, align 8
  ret void
}

declare void @hew_hashmap_free_layout(ptr)

declare i1 @hew_hashmap_insert_layout(ptr, ptr, ptr)

declare void @hew_string_drop(ptr)

declare i1 @hew_hashmap_get_clone_layout(ptr, ptr, ptr)

declare i32 @hew_lambda_drain_all(i64)

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.smul.with.overflow.i64(i64, i64) #1

attributes #0 = { cold noreturn nounwind memory(inaccessiblemem: write) }
attributes #1 = { nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none) }
