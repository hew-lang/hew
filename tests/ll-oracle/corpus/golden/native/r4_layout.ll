; ModuleID = 'r4_layout'
source_filename = "r4_layout"

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

declare void @hew_sleep_ms(i64)

declare void @hew_exit(i64)

declare void @hew_panic_msg(ptr)

declare void @hew_assert(i8)

declare void @hew_print_value(i8, i64, i1)

declare ptr @hew_int_to_string(i32)

declare ptr @hew_i64_to_string(i64)

declare ptr @hew_u8_to_string(i8)

declare ptr @hew_uint_to_string(i16)

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

declare void @hew_string_to_bytes_raw(ptr, ptr)

declare ptr @hew_string_replace(ptr, ptr, ptr)

declare ptr @hew_string_split(ptr, ptr)

declare ptr @hew_string_lines(ptr)

declare i32 @hew_string_find(ptr, ptr)

declare i32 @hew_string_index_of_start(ptr, ptr)

declare ptr @hew_string_slice(ptr, i64, i64)

declare ptr @hew_string_repeat(ptr, i64)

declare i32 @hew_string_char_at(ptr, i32)

declare ptr @hew_string_chars(ptr)

declare i32 @hew_string_char_count(ptr)

declare i32 @hew_string_char_at_utf8(ptr, i32)

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

declare void @hew_vec_remove_at(ptr, i64)

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

declare i64 @hew_actor_pid(ptr)

declare i32 @hew_node_api_register_by_pid(ptr, i64)

declare i64 @hew_remote_pid_from_raw(i64, i64)

declare i64 @hew_node_api_lookup(ptr)

define internal %Point @make_point(i64 %0, i64 %1) {
entry:
  %return_slot = alloca %Point, align 8
  %local_0 = alloca i64, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca %Point, align 8
  store i64 %0, ptr %local_0, align 4
  store i64 %1, ptr %local_1, align 4
  br label %bb0

bb0:                                              ; preds = %entry
  %field_0_init_ptr = getelementptr inbounds nuw %Point, ptr %local_2, i32 0, i32 0
  %field_0_init_src = load i64, ptr %local_0, align 4
  store i64 %field_0_init_src, ptr %field_0_init_ptr, align 4
  %field_1_init_ptr = getelementptr inbounds nuw %Point, ptr %local_2, i32 0, i32 1
  %field_1_init_src = load i64, ptr %local_1, align 4
  store i64 %field_1_init_src, ptr %field_1_init_ptr, align 4
  %move_load = load %Point, ptr %local_2, align 4
  store %Point %move_load, ptr %return_slot, align 4
  %ret_val = load %Point, ptr %return_slot, align 4
  ret %Point %ret_val
}

define internal i64 @color_code(%Color %0) {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca %Color, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i8, align 1
  %local_5 = alloca i64, align 8
  %local_6 = alloca i8, align 1
  %local_7 = alloca i64, align 8
  %local_8 = alloca i8, align 1
  %local_9 = alloca i64, align 8
  %local_10 = alloca i64, align 8
  %local_11 = alloca i64, align 8
  store %Color %0, ptr %local_0, align 1
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %machine_tag_ptr = getelementptr inbounds nuw %Color, ptr %local_0, i32 0, i32 0
  %move_iN_load = load i8, ptr %machine_tag_ptr, align 1
  %move_iN_zext = zext i8 %move_iN_load to i64
  store i64 %move_iN_zext, ptr %local_2, align 4
  store i64 0, ptr %local_3, align 4
  %cmp_lhs = load i64, ptr %local_2, align 4
  %cmp_rhs = load i64, ptr %local_3, align 4
  %cmp_bit = icmp eq i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_4, align 1
  %cond_load = load i8, ptr %local_4, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb2, label %bb6

bb1:                                              ; preds = %after_cooperate15, %after_cooperate10, %after_cooperate5
  %move_load = load i64, ptr %local_1, align 4
  store i64 %move_load, ptr %return_slot, align 4
  %ret_val = load i64, ptr %return_slot, align 4
  ret i64 %ret_val

bb2:                                              ; preds = %bb0
  store i64 0, ptr %local_9, align 4
  %move_load1 = load i64, ptr %local_9, align 4
  store i64 %move_load1, ptr %local_1, align 4
  %hew_actor_cooperate2 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel3 = icmp eq i32 %hew_actor_cooperate2, 2
  br i1 %hew_cooperate_is_cancel3, label %cancel_exit4, label %after_cooperate5

bb3:                                              ; preds = %bb6
  store i64 1, ptr %local_10, align 4
  %move_load6 = load i64, ptr %local_10, align 4
  store i64 %move_load6, ptr %local_1, align 4
  %hew_actor_cooperate7 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel8 = icmp eq i32 %hew_actor_cooperate7, 2
  br i1 %hew_cooperate_is_cancel8, label %cancel_exit9, label %after_cooperate10

bb4:                                              ; preds = %bb7
  store i64 2, ptr %local_11, align 4
  %move_load11 = load i64, ptr %local_11, align 4
  store i64 %move_load11, ptr %local_1, align 4
  %hew_actor_cooperate12 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel13 = icmp eq i32 %hew_actor_cooperate12, 2
  br i1 %hew_cooperate_is_cancel13, label %cancel_exit14, label %after_cooperate15

bb5:                                              ; preds = %bb7
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

bb6:                                              ; preds = %bb0
  store i64 1, ptr %local_5, align 4
  %cmp_lhs16 = load i64, ptr %local_2, align 4
  %cmp_rhs17 = load i64, ptr %local_5, align 4
  %cmp_bit18 = icmp eq i64 %cmp_lhs16, %cmp_rhs17
  %cmp_zext19 = zext i1 %cmp_bit18 to i8
  store i8 %cmp_zext19, ptr %local_6, align 1
  %cond_load20 = load i8, ptr %local_6, align 1
  %cond_nz21 = icmp ne i8 %cond_load20, 0
  br i1 %cond_nz21, label %bb3, label %bb7

bb7:                                              ; preds = %bb6
  store i64 2, ptr %local_7, align 4
  %cmp_lhs22 = load i64, ptr %local_2, align 4
  %cmp_rhs23 = load i64, ptr %local_7, align 4
  %cmp_bit24 = icmp eq i64 %cmp_lhs22, %cmp_rhs23
  %cmp_zext25 = zext i1 %cmp_bit24 to i8
  store i8 %cmp_zext25, ptr %local_8, align 1
  %cond_load26 = load i8, ptr %local_8, align 1
  %cond_nz27 = icmp ne i8 %cond_load26, 0
  br i1 %cond_nz27, label %bb4, label %bb5

cancel_exit:                                      ; preds = %entry
  ret i64 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit4:                                     ; preds = %bb2
  ret i64 0

after_cooperate5:                                 ; preds = %bb2
  br label %bb1

cancel_exit9:                                     ; preds = %bb3
  ret i64 0

after_cooperate10:                                ; preds = %bb3
  br label %bb1

cancel_exit14:                                    ; preds = %bb4
  ret i64 0

after_cooperate15:                                ; preds = %bb4
  br label %bb1
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
  %local_42 = alloca i64, align 8
  %local_43 = alloca i8, align 1
  %local_44 = alloca %Color, align 8
  %local_45 = alloca i64, align 8
  %local_46 = alloca i64, align 8
  %local_47 = alloca i64, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  store i64 3, ptr %local_0, align 4
  store i64 4, ptr %local_1, align 4
  %call_arg = load i64, ptr %local_0, align 4
  %call_arg1 = load i64, ptr %local_1, align 4
  %call_result = call %Point @make_point(i64 %call_arg, i64 %call_arg1)
  store %Point %call_result, ptr %local_2, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load %Point, ptr %local_2, align 4
  store %Point %move_load, ptr %local_3, align 4
  %field_0_load_ptr = getelementptr inbounds nuw %Point, ptr %local_3, i32 0, i32 0
  %field_0_load = load i64, ptr %field_0_load_ptr, align 4
  store i64 %field_0_load, ptr %local_4, align 4
  %field_1_load_ptr = getelementptr inbounds nuw %Point, ptr %local_3, i32 0, i32 1
  %field_1_load = load i64, ptr %field_1_load_ptr, align 4
  store i64 %field_1_load, ptr %local_5, align 4
  %checked_lhs = load i64, ptr %local_4, align 4
  %checked_rhs = load i64, ptr %local_5, align 4
  %with_overflow = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_6, align 4
  store i8 %checked_overflow_widen, ptr %local_7, align 1
  %cond_load = load i8, ptr %local_7, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb3:                                              ; preds = %bb1
  %print_arg = load i64, ptr %local_6, align 4
  call void @hew_print_value(i8 1, i64 %print_arg, i1 true)
  br label %bb4

bb4:                                              ; preds = %bb3
  %hew_vec_new_with_layout_call = call ptr @hew_vec_new_with_layout(ptr @__hew_layout_new_16_8_plain)
  store ptr %hew_vec_new_with_layout_call, ptr %local_8, align 8
  br label %bb5

bb5:                                              ; preds = %bb4
  %move_load2 = load ptr, ptr %local_8, align 8
  store ptr %move_load2, ptr %local_9, align 8
  store i64 10, ptr %local_10, align 4
  store i64 20, ptr %local_11, align 4
  %field_0_init_ptr = getelementptr inbounds nuw %Point, ptr %local_12, i32 0, i32 0
  %field_0_init_src = load i64, ptr %local_10, align 4
  store i64 %field_0_init_src, ptr %field_0_init_ptr, align 4
  %field_1_init_ptr = getelementptr inbounds nuw %Point, ptr %local_12, i32 0, i32 1
  %field_1_init_src = load i64, ptr %local_11, align 4
  store i64 %field_1_init_src, ptr %field_1_init_ptr, align 4
  %"hew_vec_push_layout arg0" = load ptr, ptr %local_9, align 8
  call void @hew_vec_push_layout(ptr %"hew_vec_push_layout arg0", ptr %local_12, ptr @__hew_layout_push_16_8_plain)
  br label %bb6

bb6:                                              ; preds = %bb5
  store i64 5, ptr %local_13, align 4
  store i64 5, ptr %local_14, align 4
  %field_0_init_ptr3 = getelementptr inbounds nuw %Point, ptr %local_15, i32 0, i32 0
  %field_0_init_src4 = load i64, ptr %local_13, align 4
  store i64 %field_0_init_src4, ptr %field_0_init_ptr3, align 4
  %field_1_init_ptr5 = getelementptr inbounds nuw %Point, ptr %local_15, i32 0, i32 1
  %field_1_init_src6 = load i64, ptr %local_14, align 4
  store i64 %field_1_init_src6, ptr %field_1_init_ptr5, align 4
  %"hew_vec_push_layout arg07" = load ptr, ptr %local_9, align 8
  call void @hew_vec_push_layout(ptr %"hew_vec_push_layout arg07", ptr %local_15, ptr @__hew_layout_push_16_8_plain)
  br label %bb7

bb7:                                              ; preds = %bb6
  store i64 0, ptr %local_16, align 4
  %"hew_vec_len arg0" = load ptr, ptr %local_9, align 8
  %hew_vec_len_call = call i64 @hew_vec_len(ptr %"hew_vec_len arg0")
  store i64 %hew_vec_len_call, ptr %local_17, align 4
  %cmp_lhs = load i64, ptr %local_16, align 4
  %cmp_rhs = load i64, ptr %local_17, align 4
  %cmp_bit = icmp uge i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_18, align 1
  %cond_load8 = load i8, ptr %local_18, align 1
  %cond_nz9 = icmp ne i8 %cond_load8, 0
  br i1 %cond_nz9, label %bb8, label %bb9

bb8:                                              ; preds = %bb7
  %"hew_vec_free drop" = load ptr, ptr %local_9, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop")
  store ptr null, ptr %local_9, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb9:                                              ; preds = %bb7
  %"hew_vec_get_layout arg0" = load ptr, ptr %local_9, align 8
  %"hew_vec_get_layout arg1" = load i64, ptr %local_16, align 4
  %hew_vec_get_layout_call = call ptr @hew_vec_get_layout(ptr %"hew_vec_get_layout arg0", i64 %"hew_vec_get_layout arg1", ptr @__hew_layout_get_16_8_plain)
  %hew_vec_get_layout_load = load %Point, ptr %hew_vec_get_layout_call, align 4
  store %Point %hew_vec_get_layout_load, ptr %local_19, align 4
  %move_load10 = load %Point, ptr %local_19, align 4
  store %Point %move_load10, ptr %local_20, align 4
  %field_0_load_ptr11 = getelementptr inbounds nuw %Point, ptr %local_20, i32 0, i32 0
  %field_0_load12 = load i64, ptr %field_0_load_ptr11, align 4
  store i64 %field_0_load12, ptr %local_21, align 4
  %print_arg13 = load i64, ptr %local_21, align 4
  call void @hew_print_value(i8 1, i64 %print_arg13, i1 true)
  br label %bb10

bb10:                                             ; preds = %bb9
  %hew_hashmap_new_with_layout_call = call ptr @hew_hashmap_new_with_layout(ptr @hew_layout_key_string, ptr @__hew_map_value_layout_16_8_plain)
  store ptr %hew_hashmap_new_with_layout_call, ptr %local_22, align 8
  br label %bb11

bb11:                                             ; preds = %bb10
  %move_load14 = load ptr, ptr %local_22, align 8
  store ptr %move_load14, ptr %local_23, align 8
  store ptr @str_lit, ptr %local_24, align 8
  store i64 0, ptr %local_25, align 4
  store i64 0, ptr %local_26, align 4
  %field_0_init_ptr15 = getelementptr inbounds nuw %Point, ptr %local_27, i32 0, i32 0
  %field_0_init_src16 = load i64, ptr %local_25, align 4
  store i64 %field_0_init_src16, ptr %field_0_init_ptr15, align 4
  %field_1_init_ptr17 = getelementptr inbounds nuw %Point, ptr %local_27, i32 0, i32 1
  %field_1_init_src18 = load i64, ptr %local_26, align 4
  store i64 %field_1_init_src18, ptr %field_1_init_ptr17, align 4
  %"hew_hashmap_insert_layout arg0" = load ptr, ptr %local_23, align 8
  %hew_hashmap_insert_layout_call = call i1 @hew_hashmap_insert_layout(ptr %"hew_hashmap_insert_layout arg0", ptr %local_24, ptr %local_27)
  br label %bb12

bb12:                                             ; preds = %bb11
  store ptr @str_lit.1, ptr %local_29, align 8
  %"hew_hashmap_get_layout arg0" = load ptr, ptr %local_23, align 8
  %hew_hashmap_get_layout_call = call ptr @hew_hashmap_get_layout(ptr %"hew_hashmap_get_layout arg0", ptr %local_29)
  %hashmap_get_is_some = icmp ne ptr %hew_hashmap_get_layout_call, null
  br i1 %hashmap_get_is_some, label %hashmap_get_some, label %hashmap_get_none

bb13:                                             ; preds = %hashmap_get_some, %hashmap_get_none
  %machine_tag_ptr20 = getelementptr inbounds nuw %"Option$$Point", ptr %local_30, i32 0, i32 0
  %move_iN_load = load i8, ptr %machine_tag_ptr20, align 1
  %move_iN_zext = zext i8 %move_iN_load to i64
  store i64 %move_iN_zext, ptr %local_31, align 4
  store i64 0, ptr %local_32, align 4
  %cmp_lhs21 = load i64, ptr %local_31, align 4
  %cmp_rhs22 = load i64, ptr %local_32, align 4
  %cmp_bit23 = icmp eq i64 %cmp_lhs21, %cmp_rhs22
  %cmp_zext24 = zext i1 %cmp_bit23 to i8
  store i8 %cmp_zext24, ptr %local_33, align 1
  %cond_load25 = load i8, ptr %local_33, align 1
  %cond_nz26 = icmp ne i8 %cond_load25, 0
  br i1 %cond_nz26, label %bb15, label %bb18

bb14:                                             ; preds = %after_cooperate69, %after_cooperate60
  store i64 1, ptr %local_45, align 4
  %machine_tag_ptr27 = getelementptr inbounds nuw %Color, ptr %local_44, i32 0, i32 0
  %move_iN_load_wide = load i64, ptr %local_45, align 4
  %move_iN_trunc = trunc i64 %move_iN_load_wide to i8
  store i8 %move_iN_trunc, ptr %machine_tag_ptr27, align 1
  %call_arg28 = load %Color, ptr %local_44, align 1
  %call_result29 = call i64 @color_code(%Color %call_arg28)
  store i64 %call_result29, ptr %local_46, align 4
  br label %bb25

bb15:                                             ; preds = %bb13
  %machine_payload_ptr30 = getelementptr inbounds nuw %"Option$$Point", ptr %local_30, i32 0, i32 1
  %machine_variant_field_ptr31 = getelementptr inbounds nuw { %Point }, ptr %machine_payload_ptr30, i32 0, i32 0
  %move_load32 = load %Point, ptr %machine_variant_field_ptr31, align 4
  store %Point %move_load32, ptr %local_36, align 4
  %field_0_load_ptr33 = getelementptr inbounds nuw %Point, ptr %local_36, i32 0, i32 0
  %field_0_load34 = load i64, ptr %field_0_load_ptr33, align 4
  store i64 %field_0_load34, ptr %local_37, align 4
  %field_1_load_ptr35 = getelementptr inbounds nuw %Point, ptr %local_36, i32 0, i32 1
  %field_1_load36 = load i64, ptr %field_1_load_ptr35, align 4
  store i64 %field_1_load36, ptr %local_38, align 4
  %checked_lhs37 = load i64, ptr %local_37, align 4
  %checked_rhs38 = load i64, ptr %local_38, align 4
  %with_overflow39 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs37, i64 %checked_rhs38)
  %checked_result40 = extractvalue { i64, i1 } %with_overflow39, 0
  %checked_overflow41 = extractvalue { i64, i1 } %with_overflow39, 1
  %checked_overflow_widen42 = zext i1 %checked_overflow41 to i8
  store i64 %checked_result40, ptr %local_39, align 4
  store i8 %checked_overflow_widen42, ptr %local_40, align 1
  %cond_load43 = load i8, ptr %local_40, align 1
  %cond_nz44 = icmp ne i8 %cond_load43, 0
  br i1 %cond_nz44, label %bb19, label %bb20

bb16:                                             ; preds = %bb18
  store i64 1, ptr %local_41, align 4
  %ineg_operand = load i64, ptr %local_41, align 4
  %neg_with_overflow = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 0, i64 %ineg_operand)
  %neg_result = extractvalue { i64, i1 } %neg_with_overflow, 0
  %neg_overflow = extractvalue { i64, i1 } %neg_with_overflow, 1
  %neg_overflow_widen = zext i1 %neg_overflow to i8
  store i64 %neg_result, ptr %local_42, align 4
  store i8 %neg_overflow_widen, ptr %local_43, align 1
  %cond_load45 = load i8, ptr %local_43, align 1
  %cond_nz46 = icmp ne i8 %cond_load45, 0
  br i1 %cond_nz46, label %bb22, label %bb23

bb17:                                             ; preds = %bb18
  %"hew_hashmap_free_layout drop" = load ptr, ptr %local_23, align 8
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop")
  store ptr null, ptr %local_23, align 8
  %"hew_vec_free drop47" = load ptr, ptr %local_9, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop47")
  store ptr null, ptr %local_9, align 8
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

bb18:                                             ; preds = %bb13
  store i64 1, ptr %local_34, align 4
  %cmp_lhs48 = load i64, ptr %local_31, align 4
  %cmp_rhs49 = load i64, ptr %local_34, align 4
  %cmp_bit50 = icmp eq i64 %cmp_lhs48, %cmp_rhs49
  %cmp_zext51 = zext i1 %cmp_bit50 to i8
  store i8 %cmp_zext51, ptr %local_35, align 1
  %cond_load52 = load i8, ptr %local_35, align 1
  %cond_nz53 = icmp ne i8 %cond_load52, 0
  br i1 %cond_nz53, label %bb16, label %bb17

bb19:                                             ; preds = %bb15
  %"hew_hashmap_free_layout drop54" = load ptr, ptr %local_23, align 8
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop54")
  store ptr null, ptr %local_23, align 8
  %"hew_vec_free drop55" = load ptr, ptr %local_9, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop55")
  store ptr null, ptr %local_9, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb20:                                             ; preds = %bb15
  %print_arg56 = load i64, ptr %local_39, align 4
  call void @hew_print_value(i8 1, i64 %print_arg56, i1 true)
  br label %bb21

bb21:                                             ; preds = %bb20
  %hew_actor_cooperate57 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel58 = icmp eq i32 %hew_actor_cooperate57, 2
  br i1 %hew_cooperate_is_cancel58, label %cancel_exit59, label %after_cooperate60

bb22:                                             ; preds = %bb16
  %"hew_hashmap_free_layout drop63" = load ptr, ptr %local_23, align 8
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop63")
  store ptr null, ptr %local_23, align 8
  %"hew_vec_free drop64" = load ptr, ptr %local_9, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop64")
  store ptr null, ptr %local_9, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb23:                                             ; preds = %bb16
  %print_arg65 = load i64, ptr %local_42, align 4
  call void @hew_print_value(i8 1, i64 %print_arg65, i1 true)
  br label %bb24

bb24:                                             ; preds = %bb23
  %hew_actor_cooperate66 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel67 = icmp eq i32 %hew_actor_cooperate66, 2
  br i1 %hew_cooperate_is_cancel67, label %cancel_exit68, label %after_cooperate69

bb25:                                             ; preds = %bb14
  %print_arg72 = load i64, ptr %local_46, align 4
  call void @hew_print_value(i8 1, i64 %print_arg72, i1 true)
  br label %bb26

bb26:                                             ; preds = %bb25
  store i64 0, ptr %local_47, align 4
  %move_load73 = load i64, ptr %local_47, align 4
  store i64 %move_load73, ptr %return_slot, align 4
  %"hew_hashmap_free_layout drop74" = load ptr, ptr %local_23, align 8
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop74")
  store ptr null, ptr %local_23, align 8
  %"hew_vec_free drop75" = load ptr, ptr %local_9, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop75")
  store ptr null, ptr %local_9, align 8
  %hew_lambda_drain_all_call = call i32 @hew_lambda_drain_all(i64 0)
  %ret_val = load i64, ptr %return_slot, align 4
  ret i64 %ret_val

cancel_exit:                                      ; preds = %entry
  ret i64 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

hashmap_get_none:                                 ; preds = %bb12
  %machine_tag_ptr = getelementptr inbounds nuw %"Option$$Point", ptr %local_30, i32 0, i32 0
  store i8 1, ptr %machine_tag_ptr, align 1
  br label %bb13

hashmap_get_some:                                 ; preds = %bb12
  %machine_tag_ptr19 = getelementptr inbounds nuw %"Option$$Point", ptr %local_30, i32 0, i32 0
  store i8 0, ptr %machine_tag_ptr19, align 1
  %machine_payload_ptr = getelementptr inbounds nuw %"Option$$Point", ptr %local_30, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { %Point }, ptr %machine_payload_ptr, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %machine_variant_field_ptr, ptr align 8 %hew_hashmap_get_layout_call, i64 16, i1 false)
  br label %bb13

cancel_exit59:                                    ; preds = %bb21
  %"hew_hashmap_free_layout drop61" = load ptr, ptr %local_23, align 8
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop61")
  store ptr null, ptr %local_23, align 8
  %"hew_vec_free drop62" = load ptr, ptr %local_9, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop62")
  store ptr null, ptr %local_9, align 8
  ret i64 0

after_cooperate60:                                ; preds = %bb21
  br label %bb14

cancel_exit68:                                    ; preds = %bb24
  %"hew_hashmap_free_layout drop70" = load ptr, ptr %local_23, align 8
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop70")
  store ptr null, ptr %local_23, align 8
  %"hew_vec_free drop71" = load ptr, ptr %local_9, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop71")
  store ptr null, ptr %local_9, align 8
  ret i64 0

after_cooperate69:                                ; preds = %bb24
  br label %bb14
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
  store i64 %0, ptr %local_0, align 4
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load i64, ptr %local_0, align 4
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
  %ffi_arg_trunc = trunc i32 %call_arg to i16
  %call_result = call ptr @hew_uint_to_string(i16 %ffi_arg_trunc)
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
  store i64 %0, ptr %local_0, align 4
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load i64, ptr %local_0, align 4
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

define internal ptr @"string::fmt"(ptr %0) {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca ptr, align 8
  store ptr %0, ptr %local_0, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  %move_load = load ptr, ptr %local_0, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val
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

declare void @hew_trap_with_code(i32)

; Function Attrs: cold noreturn nounwind memory(inaccessiblemem: write)
declare void @llvm.trap() #0

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64) #1

declare ptr @hew_vec_new_with_layout(ptr)

declare void @hew_vec_push_layout(ptr, ptr, ptr)

declare void @hew_vec_free(ptr)

declare ptr @hew_vec_get_layout(ptr, i64, ptr)

declare ptr @hew_hashmap_new_with_layout(ptr, ptr)

declare i1 @hew_hashmap_insert_layout(ptr, ptr, ptr)

declare ptr @hew_hashmap_get_layout(ptr, ptr)

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias writeonly captures(none), ptr noalias readonly captures(none), i64, i1 immarg) #2

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.ssub.with.overflow.i64(i64, i64) #1

declare void @hew_hashmap_free_layout(ptr)

declare i32 @hew_lambda_drain_all(i64)

attributes #0 = { cold noreturn nounwind memory(inaccessiblemem: write) }
attributes #1 = { nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none) }
attributes #2 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
