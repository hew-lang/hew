; ModuleID = 'r4_runtime_abi'
source_filename = "r4_runtime_abi"

%"Option$$i64" = type { i8, [1 x i64] }

@hew_layout_key_string = external constant i8
@hew_layout_val_i64 = external constant i8
@str_lit = private unnamed_addr constant [2 x i8] c"a\00", align 1
@str_lit.1 = private unnamed_addr constant [2 x i8] c"b\00", align 1
@str_lit.2 = private unnamed_addr constant [2 x i8] c"a\00", align 1
@str_lit.3 = private unnamed_addr constant [2 x i8] c"b\00", align 1
@hew_layout_key_i64 = external constant i8
@str_lit.4 = private unnamed_addr constant [6 x i8] c"hello\00", align 1
@str_lit.5 = private unnamed_addr constant [7 x i8] c" world\00", align 1


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

define i8 @main() {
entry:
  %return_slot = alloca i8, align 1
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i64, align 8
  %local_5 = alloca i64, align 8
  %local_6 = alloca i64, align 8
  %local_7 = alloca i64, align 8
  %local_8 = alloca i64, align 8
  %local_9 = alloca i64, align 8
  %local_10 = alloca i64, align 8
  %local_11 = alloca ptr, align 8
  %local_12 = alloca ptr, align 8
  %local_13 = alloca ptr, align 8
  %local_14 = alloca i64, align 8
  %local_15 = alloca ptr, align 8
  %local_16 = alloca i64, align 8
  %local_17 = alloca i64, align 8
  %local_18 = alloca i8, align 1
  %local_19 = alloca ptr, align 8
  %local_20 = alloca %"Option$$i64", align 8
  %local_21 = alloca i64, align 8
  %local_22 = alloca i64, align 8
  %local_23 = alloca i8, align 1
  %local_24 = alloca i64, align 8
  %local_25 = alloca i8, align 1
  %local_26 = alloca i64, align 8
  %local_27 = alloca i64, align 8
  %local_28 = alloca i64, align 8
  %local_29 = alloca i8, align 1
  %local_30 = alloca ptr, align 8
  %local_31 = alloca i8, align 1
  %local_32 = alloca i8, align 1
  %local_33 = alloca ptr, align 8
  %local_34 = alloca ptr, align 8
  %local_35 = alloca i64, align 8
  %local_36 = alloca i8, align 1
  %local_37 = alloca i64, align 8
  %local_38 = alloca i8, align 1
  %local_39 = alloca i64, align 8
  %local_40 = alloca i64, align 8
  %local_41 = alloca i8, align 1
  %local_42 = alloca ptr, align 8
  %local_43 = alloca ptr, align 8
  %local_44 = alloca ptr, align 8
  %local_45 = alloca ptr, align 8
  %local_46 = alloca ptr, align 8
  %local_47 = alloca ptr, align 8
  %local_48 = alloca i64, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %hew_vec_new_i64_call = call ptr @hew_vec_new_i64()
  store ptr %hew_vec_new_i64_call, ptr %local_0, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_0, align 8
  store ptr %move_load, ptr %local_1, align 8
  store i64 1, ptr %local_2, align 4
  %call_arg = load ptr, ptr %local_1, align 8
  %call_arg1 = load i64, ptr %local_2, align 4
  call void @hew_vec_push_i64(ptr %call_arg, i64 %call_arg1)
  br label %bb2

bb2:                                              ; preds = %bb1
  store i64 2, ptr %local_3, align 4
  %call_arg2 = load ptr, ptr %local_1, align 8
  %call_arg3 = load i64, ptr %local_3, align 4
  call void @hew_vec_push_i64(ptr %call_arg2, i64 %call_arg3)
  br label %bb3

bb3:                                              ; preds = %bb2
  store i64 3, ptr %local_4, align 4
  %call_arg4 = load ptr, ptr %local_1, align 8
  %call_arg5 = load i64, ptr %local_4, align 4
  call void @hew_vec_push_i64(ptr %call_arg4, i64 %call_arg5)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_arg6 = load ptr, ptr %local_1, align 8
  %call_result = call i64 @hew_vec_len(ptr %call_arg6)
  store i64 %call_result, ptr %local_5, align 4
  br label %bb5

bb5:                                              ; preds = %bb4
  %print_arg = load i64, ptr %local_5, align 4
  call void @hew_print_value(i8 1, i64 %print_arg, i1 true)
  br label %bb6

bb6:                                              ; preds = %bb5
  store i64 0, ptr %local_6, align 4
  %call_arg7 = load ptr, ptr %local_1, align 8
  %call_arg8 = load i64, ptr %local_6, align 4
  %call_result9 = call i64 @hew_vec_get_i64(ptr %call_arg7, i64 %call_arg8)
  store i64 %call_result9, ptr %local_7, align 4
  br label %bb7

bb7:                                              ; preds = %bb6
  %move_load10 = load i64, ptr %local_7, align 4
  store i64 %move_load10, ptr %local_8, align 4
  %print_arg11 = load i64, ptr %local_8, align 4
  call void @hew_print_value(i8 1, i64 %print_arg11, i1 true)
  br label %bb8

bb8:                                              ; preds = %bb7
  %call_arg12 = load ptr, ptr %local_1, align 8
  %call_result13 = call i64 @hew_vec_pop_i64(ptr %call_arg12)
  store i64 %call_result13, ptr %local_9, align 4
  br label %bb9

bb9:                                              ; preds = %bb8
  %move_load14 = load i64, ptr %local_9, align 4
  store i64 %move_load14, ptr %local_10, align 4
  %print_arg15 = load i64, ptr %local_10, align 4
  call void @hew_print_value(i8 1, i64 %print_arg15, i1 true)
  br label %bb10

bb10:                                             ; preds = %bb9
  %hew_hashmap_new_with_layout_call = call ptr @hew_hashmap_new_with_layout(ptr @hew_layout_key_string, ptr @hew_layout_val_i64)
  store ptr %hew_hashmap_new_with_layout_call, ptr %local_11, align 8
  br label %bb11

bb11:                                             ; preds = %bb10
  %move_load16 = load ptr, ptr %local_11, align 8
  store ptr %move_load16, ptr %local_12, align 8
  store ptr @str_lit, ptr %local_13, align 8
  store i64 10, ptr %local_14, align 4
  %"hew_hashmap_insert_layout arg0" = load ptr, ptr %local_12, align 8
  %hew_hashmap_insert_layout_call = call i1 @hew_hashmap_insert_layout(ptr %"hew_hashmap_insert_layout arg0", ptr %local_13, ptr %local_14)
  br label %bb12

bb12:                                             ; preds = %bb11
  store ptr @str_lit.1, ptr %local_15, align 8
  store i64 20, ptr %local_16, align 4
  %"hew_hashmap_insert_layout arg017" = load ptr, ptr %local_12, align 8
  %hew_hashmap_insert_layout_call18 = call i1 @hew_hashmap_insert_layout(ptr %"hew_hashmap_insert_layout arg017", ptr %local_15, ptr %local_16)
  br label %bb13

bb13:                                             ; preds = %bb12
  %"hew_hashmap_len_layout arg0" = load ptr, ptr %local_12, align 8
  %hew_hashmap_len_layout_call = call i64 @hew_hashmap_len_layout(ptr %"hew_hashmap_len_layout arg0")
  store i64 %hew_hashmap_len_layout_call, ptr %local_17, align 4
  br label %bb14

bb14:                                             ; preds = %bb13
  %print_arg19 = load i64, ptr %local_17, align 4
  call void @hew_print_value(i8 1, i64 %print_arg19, i1 true)
  br label %bb15

bb15:                                             ; preds = %bb14
  store ptr @str_lit.2, ptr %local_19, align 8
  %"hew_hashmap_get_layout arg0" = load ptr, ptr %local_12, align 8
  %hew_hashmap_get_layout_call = call ptr @hew_hashmap_get_layout(ptr %"hew_hashmap_get_layout arg0", ptr %local_19)
  %hashmap_get_is_some = icmp ne ptr %hew_hashmap_get_layout_call, null
  br i1 %hashmap_get_is_some, label %hashmap_get_some, label %hashmap_get_none

bb16:                                             ; preds = %hashmap_get_some, %hashmap_get_none
  %machine_tag_ptr21 = getelementptr inbounds nuw %"Option$$i64", ptr %local_20, i32 0, i32 0
  %move_iN_load = load i8, ptr %machine_tag_ptr21, align 1
  %move_iN_zext = zext i8 %move_iN_load to i64
  store i64 %move_iN_zext, ptr %local_21, align 4
  store i64 0, ptr %local_22, align 4
  %cmp_lhs = load i64, ptr %local_21, align 4
  %cmp_rhs = load i64, ptr %local_22, align 4
  %cmp_bit = icmp eq i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_23, align 1
  %cond_load = load i8, ptr %local_23, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb18, label %bb21

bb17:                                             ; preds = %after_cooperate46, %after_cooperate37
  store ptr @str_lit.3, ptr %local_30, align 8
  %"hew_hashmap_remove_layout arg0" = load ptr, ptr %local_12, align 8
  %hew_hashmap_remove_layout_call = call i1 @hew_hashmap_remove_layout(ptr %"hew_hashmap_remove_layout arg0", ptr %local_30)
  %hashmap_remove_bool_zext_i8 = zext i1 %hew_hashmap_remove_layout_call to i8
  store i8 %hashmap_remove_bool_zext_i8, ptr %local_31, align 1
  br label %bb26

bb18:                                             ; preds = %bb16
  %machine_payload_ptr22 = getelementptr inbounds nuw %"Option$$i64", ptr %local_20, i32 0, i32 1
  %machine_variant_field_ptr23 = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr22, i32 0, i32 0
  %move_load24 = load i64, ptr %machine_variant_field_ptr23, align 4
  store i64 %move_load24, ptr %local_26, align 4
  %print_arg25 = load i64, ptr %local_26, align 4
  call void @hew_print_value(i8 1, i64 %print_arg25, i1 true)
  br label %bb22

bb19:                                             ; preds = %bb21
  store i64 1, ptr %local_27, align 4
  %ineg_operand = load i64, ptr %local_27, align 4
  %neg_with_overflow = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 0, i64 %ineg_operand)
  %neg_result = extractvalue { i64, i1 } %neg_with_overflow, 0
  %neg_overflow = extractvalue { i64, i1 } %neg_with_overflow, 1
  %neg_overflow_widen = zext i1 %neg_overflow to i8
  store i64 %neg_result, ptr %local_28, align 4
  store i8 %neg_overflow_widen, ptr %local_29, align 1
  %cond_load26 = load i8, ptr %local_29, align 1
  %cond_nz27 = icmp ne i8 %cond_load26, 0
  br i1 %cond_nz27, label %bb23, label %bb24

bb20:                                             ; preds = %bb21
  %"hew_hashmap_free_layout drop" = load ptr, ptr %local_12, align 8
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop")
  store ptr null, ptr %local_12, align 8
  %"hew_vec_free drop" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

bb21:                                             ; preds = %bb16
  store i64 1, ptr %local_24, align 4
  %cmp_lhs28 = load i64, ptr %local_21, align 4
  %cmp_rhs29 = load i64, ptr %local_24, align 4
  %cmp_bit30 = icmp eq i64 %cmp_lhs28, %cmp_rhs29
  %cmp_zext31 = zext i1 %cmp_bit30 to i8
  store i8 %cmp_zext31, ptr %local_25, align 1
  %cond_load32 = load i8, ptr %local_25, align 1
  %cond_nz33 = icmp ne i8 %cond_load32, 0
  br i1 %cond_nz33, label %bb19, label %bb20

bb22:                                             ; preds = %bb18
  %hew_actor_cooperate34 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel35 = icmp eq i32 %hew_actor_cooperate34, 2
  br i1 %hew_cooperate_is_cancel35, label %cancel_exit36, label %after_cooperate37

bb23:                                             ; preds = %bb19
  %"hew_hashmap_free_layout drop40" = load ptr, ptr %local_12, align 8
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop40")
  store ptr null, ptr %local_12, align 8
  %"hew_vec_free drop41" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop41")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb24:                                             ; preds = %bb19
  %print_arg42 = load i64, ptr %local_28, align 4
  call void @hew_print_value(i8 1, i64 %print_arg42, i1 true)
  br label %bb25

bb25:                                             ; preds = %bb24
  %hew_actor_cooperate43 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel44 = icmp eq i32 %hew_actor_cooperate43, 2
  br i1 %hew_cooperate_is_cancel44, label %cancel_exit45, label %after_cooperate46

bb26:                                             ; preds = %bb17
  %move_load49 = load i8, ptr %local_31, align 1
  store i8 %move_load49, ptr %local_32, align 1
  %print_arg50 = load i8, ptr %local_32, align 1
  %print_narrow_bits = zext i8 %print_arg50 to i64
  call void @hew_print_value(i8 3, i64 %print_narrow_bits, i1 true)
  br label %bb27

bb27:                                             ; preds = %bb26
  %hew_hashset_new_with_layout_call = call ptr @hew_hashset_new_with_layout(ptr @hew_layout_key_i64)
  store ptr %hew_hashset_new_with_layout_call, ptr %local_33, align 8
  br label %bb28

bb28:                                             ; preds = %bb27
  %move_load51 = load ptr, ptr %local_33, align 8
  store ptr %move_load51, ptr %local_34, align 8
  store i64 42, ptr %local_35, align 4
  %"hew_hashset_insert_layout arg0" = load ptr, ptr %local_34, align 8
  %hew_hashset_insert_layout_call = call i1 @hew_hashset_insert_layout(ptr %"hew_hashset_insert_layout arg0", ptr %local_35)
  %hashset_insert_bool_zext_i8 = zext i1 %hew_hashset_insert_layout_call to i8
  store i8 %hashset_insert_bool_zext_i8, ptr %local_36, align 1
  br label %bb29

bb29:                                             ; preds = %bb28
  store i64 7, ptr %local_37, align 4
  %"hew_hashset_insert_layout arg052" = load ptr, ptr %local_34, align 8
  %hew_hashset_insert_layout_call53 = call i1 @hew_hashset_insert_layout(ptr %"hew_hashset_insert_layout arg052", ptr %local_37)
  %hashset_insert_bool_zext_i854 = zext i1 %hew_hashset_insert_layout_call53 to i8
  store i8 %hashset_insert_bool_zext_i854, ptr %local_38, align 1
  br label %bb30

bb30:                                             ; preds = %bb29
  %"hew_hashset_len_layout arg0" = load ptr, ptr %local_34, align 8
  %hew_hashset_len_layout_call = call i64 @hew_hashset_len_layout(ptr %"hew_hashset_len_layout arg0")
  store i64 %hew_hashset_len_layout_call, ptr %local_39, align 4
  br label %bb31

bb31:                                             ; preds = %bb30
  %print_arg55 = load i64, ptr %local_39, align 4
  call void @hew_print_value(i8 1, i64 %print_arg55, i1 true)
  br label %bb32

bb32:                                             ; preds = %bb31
  store i64 42, ptr %local_40, align 4
  %"hew_hashset_contains_layout arg0" = load ptr, ptr %local_34, align 8
  %hew_hashset_contains_layout_call = call i1 @hew_hashset_contains_layout(ptr %"hew_hashset_contains_layout arg0", ptr %local_40)
  %hashset_contains_bool_zext_i8 = zext i1 %hew_hashset_contains_layout_call to i8
  store i8 %hashset_contains_bool_zext_i8, ptr %local_41, align 1
  br label %bb33

bb33:                                             ; preds = %bb32
  %print_arg56 = load i8, ptr %local_41, align 1
  %print_narrow_bits57 = zext i8 %print_arg56 to i64
  call void @hew_print_value(i8 3, i64 %print_narrow_bits57, i1 true)
  br label %bb34

bb34:                                             ; preds = %bb33
  store ptr @str_lit.4, ptr %local_42, align 8
  %move_load58 = load ptr, ptr %local_42, align 8
  store ptr %move_load58, ptr %local_43, align 8
  store ptr @str_lit.5, ptr %local_44, align 8
  %move_load59 = load ptr, ptr %local_44, align 8
  store ptr %move_load59, ptr %local_45, align 8
  %"hew_string_concat arg0" = load ptr, ptr %local_43, align 8
  %"hew_string_concat arg1" = load ptr, ptr %local_45, align 8
  %hew_string_concat_call = call ptr @hew_string_concat(ptr %"hew_string_concat arg0", ptr %"hew_string_concat arg1")
  store ptr %hew_string_concat_call, ptr %local_46, align 8
  %move_load60 = load ptr, ptr %local_46, align 8
  store ptr %move_load60, ptr %local_47, align 8
  %print_arg61 = load ptr, ptr %local_47, align 8
  %print_str_bits = ptrtoint ptr %print_arg61 to i64
  call void @hew_print_value(i8 4, i64 %print_str_bits, i1 true)
  br label %bb35

bb35:                                             ; preds = %bb34
  %call_arg62 = load ptr, ptr %local_47, align 8
  %call_result63 = call i32 @hew_string_length(ptr %call_arg62)
  %ffi_sext = sext i32 %call_result63 to i64
  store i64 %ffi_sext, ptr %local_48, align 4
  br label %bb36

bb36:                                             ; preds = %bb35
  %print_arg64 = load i64, ptr %local_48, align 4
  call void @hew_print_value(i8 1, i64 %print_arg64, i1 true)
  br label %bb37

bb37:                                             ; preds = %bb36
  %"hew_string_drop drop" = load ptr, ptr %local_47, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop")
  store ptr null, ptr %local_47, align 8
  %"hew_hashset_free_layout drop" = load ptr, ptr %local_34, align 8
  call void @hew_hashset_free_layout(ptr %"hew_hashset_free_layout drop")
  store ptr null, ptr %local_34, align 8
  %"hew_hashmap_free_layout drop65" = load ptr, ptr %local_12, align 8
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop65")
  store ptr null, ptr %local_12, align 8
  %"hew_vec_free drop66" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop66")
  store ptr null, ptr %local_1, align 8
  %hew_lambda_drain_all_call = call i32 @hew_lambda_drain_all(i64 0)
  ret i8 0

cancel_exit:                                      ; preds = %entry
  ret i8 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

hashmap_get_none:                                 ; preds = %bb15
  %machine_tag_ptr = getelementptr inbounds nuw %"Option$$i64", ptr %local_20, i32 0, i32 0
  store i8 1, ptr %machine_tag_ptr, align 1
  br label %bb16

hashmap_get_some:                                 ; preds = %bb15
  %machine_tag_ptr20 = getelementptr inbounds nuw %"Option$$i64", ptr %local_20, i32 0, i32 0
  store i8 0, ptr %machine_tag_ptr20, align 1
  %machine_payload_ptr = getelementptr inbounds nuw %"Option$$i64", ptr %local_20, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %machine_variant_field_ptr, ptr align 8 %hew_hashmap_get_layout_call, i64 8, i1 false)
  br label %bb16

cancel_exit36:                                    ; preds = %bb22
  %"hew_hashmap_free_layout drop38" = load ptr, ptr %local_12, align 8
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop38")
  store ptr null, ptr %local_12, align 8
  %"hew_vec_free drop39" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop39")
  store ptr null, ptr %local_1, align 8
  ret i8 0

after_cooperate37:                                ; preds = %bb22
  br label %bb17

cancel_exit45:                                    ; preds = %bb25
  %"hew_hashmap_free_layout drop47" = load ptr, ptr %local_12, align 8
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop47")
  store ptr null, ptr %local_12, align 8
  %"hew_vec_free drop48" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop48")
  store ptr null, ptr %local_1, align 8
  ret i8 0

after_cooperate46:                                ; preds = %bb25
  br label %bb17
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

declare i32 @hew_actor_cooperate()

declare ptr @hew_vec_new_i64()

declare ptr @hew_hashmap_new_with_layout(ptr, ptr)

declare i1 @hew_hashmap_insert_layout(ptr, ptr, ptr)

declare i64 @hew_hashmap_len_layout(ptr)

declare ptr @hew_hashmap_get_layout(ptr, ptr)

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias writeonly captures(none), ptr noalias readonly captures(none), i64, i1 immarg) #0

declare i1 @hew_hashmap_remove_layout(ptr, ptr)

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.ssub.with.overflow.i64(i64, i64) #1

declare void @hew_hashmap_free_layout(ptr)

declare void @hew_vec_free(ptr)

declare void @hew_trap_with_code(i32)

; Function Attrs: cold noreturn nounwind memory(inaccessiblemem: write)
declare void @llvm.trap() #2

declare ptr @hew_hashset_new_with_layout(ptr)

declare i1 @hew_hashset_insert_layout(ptr, ptr)

declare i64 @hew_hashset_len_layout(ptr)

declare i1 @hew_hashset_contains_layout(ptr, ptr)

declare void @hew_string_drop(ptr)

declare void @hew_hashset_free_layout(ptr)

declare i32 @hew_lambda_drain_all(i64)

attributes #0 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
attributes #1 = { nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none) }
attributes #2 = { cold noreturn nounwind memory(inaccessiblemem: write) }
