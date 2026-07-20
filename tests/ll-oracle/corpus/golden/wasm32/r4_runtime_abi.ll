; ModuleID = 'r4_runtime_abi'
source_filename = "r4_runtime_abi"
target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

%"Option$$i64" = type { i8, [1 x i64] }
%CrashInfo = type { i64, ptr }

@hew_layout_key_string = external constant i8
@hew_layout_val_i64 = external constant i8
@str_lit = private unnamed_addr constant [2 x i8] c"a\00", align 1
@str_lit.1 = private unnamed_addr constant [2 x i8] c"b\00", align 1
@str_lit.2 = private unnamed_addr constant [2 x i8] c"a\00", align 1
@str_lit.3 = private unnamed_addr constant [2 x i8] c"b\00", align 1
@hew_layout_key_i64 = external constant i8
@str_lit.4 = private unnamed_addr constant [6 x i8] c"hello\00", align 1
@str_lit.5 = private unnamed_addr constant [7 x i8] c" world\00", align 1
@str_lit.6 = private unnamed_addr constant [3 x i8] c"ns\00", align 1

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

define i8 @__original_main() {
entry:
  %return_slot = alloca i8, align 1
  %local_0 = alloca ptr, align 4
  %local_1 = alloca ptr, align 4
  %local_2 = alloca i64, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i64, align 8
  %local_5 = alloca i64, align 8
  %local_6 = alloca i64, align 8
  %local_7 = alloca %"Option$$i64", align 8
  %local_8 = alloca %"Option$$i64", align 8
  %local_9 = alloca i64, align 8
  %local_10 = alloca i64, align 8
  %local_11 = alloca i64, align 8
  %local_12 = alloca i8, align 1
  %local_13 = alloca i64, align 8
  %local_14 = alloca i8, align 1
  %local_15 = alloca i64, align 8
  %local_16 = alloca i64, align 8
  %local_17 = alloca i64, align 8
  %local_18 = alloca i64, align 8
  %local_19 = alloca ptr, align 4
  %local_20 = alloca ptr, align 4
  %local_21 = alloca ptr, align 4
  %local_22 = alloca i64, align 8
  %local_23 = alloca ptr, align 4
  %local_24 = alloca i64, align 8
  %local_25 = alloca i64, align 8
  %local_26 = alloca i8, align 1
  %local_27 = alloca ptr, align 4
  %local_28 = alloca %"Option$$i64", align 8
  %local_29 = alloca i64, align 8
  %local_30 = alloca i64, align 8
  %local_31 = alloca i8, align 1
  %local_32 = alloca i64, align 8
  %local_33 = alloca i8, align 1
  %local_34 = alloca i64, align 8
  %local_35 = alloca i64, align 8
  %local_36 = alloca i8, align 1
  %local_37 = alloca ptr, align 4
  %local_38 = alloca %"Option$$i64", align 8
  %local_39 = alloca i64, align 8
  %local_40 = alloca i64, align 8
  %local_41 = alloca i8, align 1
  %local_42 = alloca i64, align 8
  %local_43 = alloca i8, align 1
  %local_44 = alloca i8, align 1
  %local_45 = alloca i8, align 1
  %local_46 = alloca i8, align 1
  %local_47 = alloca ptr, align 4
  %local_48 = alloca ptr, align 4
  %local_49 = alloca i64, align 8
  %local_50 = alloca i8, align 1
  %local_51 = alloca i64, align 8
  %local_52 = alloca i8, align 1
  %local_53 = alloca i64, align 8
  %local_54 = alloca i64, align 8
  %local_55 = alloca i8, align 1
  %local_56 = alloca ptr, align 4
  %local_57 = alloca ptr, align 4
  %local_58 = alloca ptr, align 4
  %local_59 = alloca ptr, align 4
  %local_60 = alloca ptr, align 4
  %local_61 = alloca ptr, align 4
  %local_62 = alloca i64, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %hew_vec_new_i64_call = call ptr @hew_vec_new_i64()
  store ptr %hew_vec_new_i64_call, ptr %local_0, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_0, align 4
  store ptr %move_load, ptr %local_1, align 4
  store i64 1, ptr %local_2, align 8
  %call_arg = load ptr, ptr %local_1, align 4
  %call_arg1 = load i64, ptr %local_2, align 8
  call void @hew_vec_push_i64(ptr %call_arg, i64 %call_arg1)
  br label %bb2

bb2:                                              ; preds = %bb1
  store i64 2, ptr %local_3, align 8
  %call_arg2 = load ptr, ptr %local_1, align 4
  %call_arg3 = load i64, ptr %local_3, align 8
  call void @hew_vec_push_i64(ptr %call_arg2, i64 %call_arg3)
  br label %bb3

bb3:                                              ; preds = %bb2
  store i64 3, ptr %local_4, align 8
  %call_arg4 = load ptr, ptr %local_1, align 4
  %call_arg5 = load i64, ptr %local_4, align 8
  call void @hew_vec_push_i64(ptr %call_arg4, i64 %call_arg5)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_arg6 = load ptr, ptr %local_1, align 4
  %call_result = call i64 @hew_vec_len(ptr %call_arg6)
  store i64 %call_result, ptr %local_5, align 8
  br label %bb5

bb5:                                              ; preds = %bb4
  %print_arg = load i64, ptr %local_5, align 8
  call void @hew_print_value(i8 1, i64 %print_arg, i1 true)
  br label %bb6

bb6:                                              ; preds = %bb5
  store i64 0, ptr %local_6, align 8
  %"hew_vec_get_clone arg0" = load ptr, ptr %local_1, align 4
  %"hew_vec_get_clone index" = load i64, ptr %local_6, align 8
  %machine_payload_ptr = getelementptr inbounds nuw %"Option$$i64", ptr %local_7, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr, i32 0, i32 0
  %hew_vec_get_clone_call = call i1 @hew_vec_get_clone(ptr %"hew_vec_get_clone arg0", i64 %"hew_vec_get_clone index", ptr %machine_variant_field_ptr)
  br i1 %hew_vec_get_clone_call, label %vec_get_some, label %vec_get_none

bb7:                                              ; preds = %vec_get_some, %vec_get_none
  %move_load8 = load %"Option$$i64", ptr %local_7, align 8
  store %"Option$$i64" %move_load8, ptr %local_8, align 8
  %machine_tag_ptr9 = getelementptr inbounds nuw %"Option$$i64", ptr %local_8, i32 0, i32 0
  %move_iN_load = load i8, ptr %machine_tag_ptr9, align 1
  %move_iN_zext = zext i8 %move_iN_load to i64
  store i64 %move_iN_zext, ptr %local_10, align 8
  store i64 0, ptr %local_11, align 8
  %cmp_lhs = load i64, ptr %local_10, align 8
  %cmp_rhs = load i64, ptr %local_11, align 8
  %cmp_bit = icmp eq i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_12, align 1
  %cond_load = load i8, ptr %local_12, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb9, label %bb12

bb8:                                              ; preds = %after_cooperate23, %after_cooperate18
  %print_arg10 = load i64, ptr %local_9, align 8
  call void @hew_print_value(i8 1, i64 %print_arg10, i1 true)
  br label %bb13

bb9:                                              ; preds = %bb7
  %machine_payload_ptr11 = getelementptr inbounds nuw %"Option$$i64", ptr %local_8, i32 0, i32 1
  %machine_variant_field_ptr12 = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr11, i32 0, i32 0
  %move_load13 = load i64, ptr %machine_variant_field_ptr12, align 8
  store i64 %move_load13, ptr %local_15, align 8
  %move_load14 = load i64, ptr %local_15, align 8
  store i64 %move_load14, ptr %local_9, align 8
  %hew_actor_cooperate15 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel16 = icmp eq i32 %hew_actor_cooperate15, 2
  br i1 %hew_cooperate_is_cancel16, label %cancel_exit17, label %after_cooperate18

bb10:                                             ; preds = %bb12
  store i64 -1, ptr %local_16, align 8
  %move_load19 = load i64, ptr %local_16, align 8
  store i64 %move_load19, ptr %local_9, align 8
  %hew_actor_cooperate20 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel21 = icmp eq i32 %hew_actor_cooperate20, 2
  br i1 %hew_cooperate_is_cancel21, label %cancel_exit22, label %after_cooperate23

bb11:                                             ; preds = %bb12
  %"hew_vec_free drop25" = load ptr, ptr %local_1, align 4
  call void @hew_vec_free(ptr %"hew_vec_free drop25")
  store ptr null, ptr %local_1, align 4
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

bb12:                                             ; preds = %bb7
  store i64 1, ptr %local_13, align 8
  %cmp_lhs26 = load i64, ptr %local_10, align 8
  %cmp_rhs27 = load i64, ptr %local_13, align 8
  %cmp_bit28 = icmp eq i64 %cmp_lhs26, %cmp_rhs27
  %cmp_zext29 = zext i1 %cmp_bit28 to i8
  store i8 %cmp_zext29, ptr %local_14, align 1
  %cond_load30 = load i8, ptr %local_14, align 1
  %cond_nz31 = icmp ne i8 %cond_load30, 0
  br i1 %cond_nz31, label %bb10, label %bb11

bb13:                                             ; preds = %bb8
  %call_arg32 = load ptr, ptr %local_1, align 4
  %call_result33 = call i64 @hew_vec_pop_i64(ptr %call_arg32)
  store i64 %call_result33, ptr %local_17, align 8
  br label %bb14

bb14:                                             ; preds = %bb13
  %move_load34 = load i64, ptr %local_17, align 8
  store i64 %move_load34, ptr %local_18, align 8
  %print_arg35 = load i64, ptr %local_18, align 8
  call void @hew_print_value(i8 1, i64 %print_arg35, i1 true)
  br label %bb15

bb15:                                             ; preds = %bb14
  %hew_hashmap_new_with_layout_call = call ptr @hew_hashmap_new_with_layout(ptr @hew_layout_key_string, ptr @hew_layout_val_i64)
  store ptr %hew_hashmap_new_with_layout_call, ptr %local_19, align 4
  br label %bb16

bb16:                                             ; preds = %bb15
  %move_load36 = load ptr, ptr %local_19, align 4
  store ptr %move_load36, ptr %local_20, align 4
  store ptr @str_lit, ptr %local_21, align 4
  store i64 10, ptr %local_22, align 8
  %"hew_hashmap_insert_layout arg0" = load ptr, ptr %local_20, align 4
  %hew_hashmap_insert_layout_call = call i1 @hew_hashmap_insert_layout(ptr %"hew_hashmap_insert_layout arg0", ptr %local_21, ptr %local_22)
  %insert_existed = icmp eq i1 %hew_hashmap_insert_layout_call, false
  br i1 %insert_existed, label %insert_overwrite_key_release, label %insert_overwrite_key_cont

bb17:                                             ; preds = %insert_overwrite_key_cont
  store ptr @str_lit.1, ptr %local_23, align 4
  store i64 20, ptr %local_24, align 8
  %"hew_hashmap_insert_layout arg037" = load ptr, ptr %local_20, align 4
  %hew_hashmap_insert_layout_call38 = call i1 @hew_hashmap_insert_layout(ptr %"hew_hashmap_insert_layout arg037", ptr %local_23, ptr %local_24)
  %insert_existed41 = icmp eq i1 %hew_hashmap_insert_layout_call38, false
  br i1 %insert_existed41, label %insert_overwrite_key_release39, label %insert_overwrite_key_cont40

bb18:                                             ; preds = %insert_overwrite_key_cont40
  %"hew_hashmap_len_layout arg0" = load ptr, ptr %local_20, align 4
  %hew_hashmap_len_layout_call = call i64 @hew_hashmap_len_layout(ptr %"hew_hashmap_len_layout arg0")
  store i64 %hew_hashmap_len_layout_call, ptr %local_25, align 8
  br label %bb19

bb19:                                             ; preds = %bb18
  %print_arg43 = load i64, ptr %local_25, align 8
  call void @hew_print_value(i8 1, i64 %print_arg43, i1 true)
  br label %bb20

bb20:                                             ; preds = %bb19
  store ptr @str_lit.2, ptr %local_27, align 4
  %"hew_hashmap_get_layout arg0" = load ptr, ptr %local_20, align 4
  %machine_payload_ptr44 = getelementptr inbounds nuw %"Option$$i64", ptr %local_28, i32 0, i32 1
  %machine_variant_field_ptr45 = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr44, i32 0, i32 0
  %hew_hashmap_get_clone_layout_call = call i1 @hew_hashmap_get_clone_layout(ptr %"hew_hashmap_get_layout arg0", ptr %local_27, ptr %machine_variant_field_ptr45)
  br i1 %hew_hashmap_get_clone_layout_call, label %hashmap_get_some, label %hashmap_get_none

bb21:                                             ; preds = %hashmap_get_some, %hashmap_get_none
  %machine_tag_ptr48 = getelementptr inbounds nuw %"Option$$i64", ptr %local_28, i32 0, i32 0
  %move_iN_load49 = load i8, ptr %machine_tag_ptr48, align 1
  %move_iN_zext50 = zext i8 %move_iN_load49 to i64
  store i64 %move_iN_zext50, ptr %local_29, align 8
  store i64 0, ptr %local_30, align 8
  %cmp_lhs51 = load i64, ptr %local_29, align 8
  %cmp_rhs52 = load i64, ptr %local_30, align 8
  %cmp_bit53 = icmp eq i64 %cmp_lhs51, %cmp_rhs52
  %cmp_zext54 = zext i1 %cmp_bit53 to i8
  store i8 %cmp_zext54, ptr %local_31, align 1
  %cond_load55 = load i8, ptr %local_31, align 1
  %cond_nz56 = icmp ne i8 %cond_load55, 0
  br i1 %cond_nz56, label %bb23, label %bb26

bb22:                                             ; preds = %after_cooperate82, %after_cooperate76
  store ptr @str_lit.3, ptr %local_37, align 4
  %"hew_hashmap_remove_take_layout arg0" = load ptr, ptr %local_20, align 4
  %machine_payload_ptr57 = getelementptr inbounds nuw %"Option$$i64", ptr %local_38, i32 0, i32 1
  %machine_variant_field_ptr58 = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr57, i32 0, i32 0
  %hew_hashmap_remove_take_layout_call = call i1 @hew_hashmap_remove_take_layout(ptr %"hew_hashmap_remove_take_layout arg0", ptr %local_37, ptr %machine_variant_field_ptr58)
  br i1 %hew_hashmap_remove_take_layout_call, label %hashmap_remove_some, label %hashmap_remove_none

bb23:                                             ; preds = %bb21
  %machine_payload_ptr61 = getelementptr inbounds nuw %"Option$$i64", ptr %local_28, i32 0, i32 1
  %machine_variant_field_ptr62 = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr61, i32 0, i32 0
  %move_load63 = load i64, ptr %machine_variant_field_ptr62, align 8
  store i64 %move_load63, ptr %local_34, align 8
  %print_arg64 = load i64, ptr %local_34, align 8
  call void @hew_print_value(i8 1, i64 %print_arg64, i1 true)
  br label %bb27

bb24:                                             ; preds = %bb26
  store i64 -1, ptr %local_35, align 8
  %print_arg65 = load i64, ptr %local_35, align 8
  call void @hew_print_value(i8 1, i64 %print_arg65, i1 true)
  br label %bb28

bb25:                                             ; preds = %bb26
  %"hew_hashmap_free_layout drop" = load ptr, ptr %local_20, align 4
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop")
  store ptr null, ptr %local_20, align 4
  %"hew_vec_free drop66" = load ptr, ptr %local_1, align 4
  call void @hew_vec_free(ptr %"hew_vec_free drop66")
  store ptr null, ptr %local_1, align 4
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

bb26:                                             ; preds = %bb21
  store i64 1, ptr %local_32, align 8
  %cmp_lhs67 = load i64, ptr %local_29, align 8
  %cmp_rhs68 = load i64, ptr %local_32, align 8
  %cmp_bit69 = icmp eq i64 %cmp_lhs67, %cmp_rhs68
  %cmp_zext70 = zext i1 %cmp_bit69 to i8
  store i8 %cmp_zext70, ptr %local_33, align 1
  %cond_load71 = load i8, ptr %local_33, align 1
  %cond_nz72 = icmp ne i8 %cond_load71, 0
  br i1 %cond_nz72, label %bb24, label %bb25

bb27:                                             ; preds = %bb23
  %hew_actor_cooperate73 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel74 = icmp eq i32 %hew_actor_cooperate73, 2
  br i1 %hew_cooperate_is_cancel74, label %cancel_exit75, label %after_cooperate76

bb28:                                             ; preds = %bb24
  %hew_actor_cooperate79 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel80 = icmp eq i32 %hew_actor_cooperate79, 2
  br i1 %hew_cooperate_is_cancel80, label %cancel_exit81, label %after_cooperate82

bb29:                                             ; preds = %hashmap_remove_some, %hashmap_remove_none
  %machine_tag_ptr85 = getelementptr inbounds nuw %"Option$$i64", ptr %local_38, i32 0, i32 0
  %move_iN_load86 = load i8, ptr %machine_tag_ptr85, align 1
  %move_iN_zext87 = zext i8 %move_iN_load86 to i64
  store i64 %move_iN_zext87, ptr %local_39, align 8
  store i64 0, ptr %local_40, align 8
  %cmp_lhs88 = load i64, ptr %local_39, align 8
  %cmp_rhs89 = load i64, ptr %local_40, align 8
  %cmp_bit90 = icmp eq i64 %cmp_lhs88, %cmp_rhs89
  %cmp_zext91 = zext i1 %cmp_bit90 to i8
  store i8 %cmp_zext91, ptr %local_41, align 1
  %cond_load92 = load i8, ptr %local_41, align 1
  %cond_nz93 = icmp ne i8 %cond_load92, 0
  br i1 %cond_nz93, label %bb31, label %bb34

bb30:                                             ; preds = %after_cooperate107, %after_cooperate100
  %move_load94 = load i8, ptr %local_36, align 1
  store i8 %move_load94, ptr %local_46, align 1
  %print_arg95 = load i8, ptr %local_46, align 1
  %print_narrow_bits = zext i8 %print_arg95 to i64
  call void @hew_print_value(i8 3, i64 %print_narrow_bits, i1 true)
  br label %bb35

bb31:                                             ; preds = %bb29
  store i8 1, ptr %local_44, align 1
  %move_load96 = load i8, ptr %local_44, align 1
  store i8 %move_load96, ptr %local_36, align 1
  %hew_actor_cooperate97 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel98 = icmp eq i32 %hew_actor_cooperate97, 2
  br i1 %hew_cooperate_is_cancel98, label %cancel_exit99, label %after_cooperate100

bb32:                                             ; preds = %bb34
  store i8 0, ptr %local_45, align 1
  %move_load103 = load i8, ptr %local_45, align 1
  store i8 %move_load103, ptr %local_36, align 1
  %hew_actor_cooperate104 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel105 = icmp eq i32 %hew_actor_cooperate104, 2
  br i1 %hew_cooperate_is_cancel105, label %cancel_exit106, label %after_cooperate107

bb33:                                             ; preds = %bb34
  %"hew_hashmap_free_layout drop110" = load ptr, ptr %local_20, align 4
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop110")
  store ptr null, ptr %local_20, align 4
  %"hew_vec_free drop111" = load ptr, ptr %local_1, align 4
  call void @hew_vec_free(ptr %"hew_vec_free drop111")
  store ptr null, ptr %local_1, align 4
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

bb34:                                             ; preds = %bb29
  store i64 1, ptr %local_42, align 8
  %cmp_lhs112 = load i64, ptr %local_39, align 8
  %cmp_rhs113 = load i64, ptr %local_42, align 8
  %cmp_bit114 = icmp eq i64 %cmp_lhs112, %cmp_rhs113
  %cmp_zext115 = zext i1 %cmp_bit114 to i8
  store i8 %cmp_zext115, ptr %local_43, align 1
  %cond_load116 = load i8, ptr %local_43, align 1
  %cond_nz117 = icmp ne i8 %cond_load116, 0
  br i1 %cond_nz117, label %bb32, label %bb33

bb35:                                             ; preds = %bb30
  %hew_hashset_new_with_layout_call = call ptr @hew_hashset_new_with_layout(ptr @hew_layout_key_i64)
  store ptr %hew_hashset_new_with_layout_call, ptr %local_47, align 4
  br label %bb36

bb36:                                             ; preds = %bb35
  %move_load118 = load ptr, ptr %local_47, align 4
  store ptr %move_load118, ptr %local_48, align 4
  store i64 42, ptr %local_49, align 8
  %"hew_hashset_insert_layout arg0" = load ptr, ptr %local_48, align 4
  %hew_hashset_insert_layout_call = call i1 @hew_hashset_insert_layout(ptr %"hew_hashset_insert_layout arg0", ptr %local_49)
  %hashset_insert_bool_zext_i8 = zext i1 %hew_hashset_insert_layout_call to i8
  store i8 %hashset_insert_bool_zext_i8, ptr %local_50, align 1
  br label %bb37

bb37:                                             ; preds = %bb36
  store i64 7, ptr %local_51, align 8
  %"hew_hashset_insert_layout arg0119" = load ptr, ptr %local_48, align 4
  %hew_hashset_insert_layout_call120 = call i1 @hew_hashset_insert_layout(ptr %"hew_hashset_insert_layout arg0119", ptr %local_51)
  %hashset_insert_bool_zext_i8121 = zext i1 %hew_hashset_insert_layout_call120 to i8
  store i8 %hashset_insert_bool_zext_i8121, ptr %local_52, align 1
  br label %bb38

bb38:                                             ; preds = %bb37
  %"hew_hashset_len_layout arg0" = load ptr, ptr %local_48, align 4
  %hew_hashset_len_layout_call = call i64 @hew_hashset_len_layout(ptr %"hew_hashset_len_layout arg0")
  store i64 %hew_hashset_len_layout_call, ptr %local_53, align 8
  br label %bb39

bb39:                                             ; preds = %bb38
  %print_arg122 = load i64, ptr %local_53, align 8
  call void @hew_print_value(i8 1, i64 %print_arg122, i1 true)
  br label %bb40

bb40:                                             ; preds = %bb39
  store i64 42, ptr %local_54, align 8
  %"hew_hashset_contains_layout arg0" = load ptr, ptr %local_48, align 4
  %hew_hashset_contains_layout_call = call i1 @hew_hashset_contains_layout(ptr %"hew_hashset_contains_layout arg0", ptr %local_54)
  %hashset_contains_bool_zext_i8 = zext i1 %hew_hashset_contains_layout_call to i8
  store i8 %hashset_contains_bool_zext_i8, ptr %local_55, align 1
  br label %bb41

bb41:                                             ; preds = %bb40
  %print_arg123 = load i8, ptr %local_55, align 1
  %print_narrow_bits124 = zext i8 %print_arg123 to i64
  call void @hew_print_value(i8 3, i64 %print_narrow_bits124, i1 true)
  br label %bb42

bb42:                                             ; preds = %bb41
  store ptr @str_lit.4, ptr %local_56, align 4
  %move_load125 = load ptr, ptr %local_56, align 4
  store ptr %move_load125, ptr %local_57, align 4
  store ptr @str_lit.5, ptr %local_58, align 4
  %move_load126 = load ptr, ptr %local_58, align 4
  store ptr %move_load126, ptr %local_59, align 4
  %"hew_string_concat arg0" = load ptr, ptr %local_57, align 4
  %"hew_string_concat arg1" = load ptr, ptr %local_59, align 4
  %hew_string_concat_call = call ptr @hew_string_concat(ptr %"hew_string_concat arg0", ptr %"hew_string_concat arg1")
  store ptr %hew_string_concat_call, ptr %local_60, align 4
  %move_load127 = load ptr, ptr %local_60, align 4
  store ptr %move_load127, ptr %local_61, align 4
  %print_arg128 = load ptr, ptr %local_61, align 4
  %print_str_bits = ptrtoint ptr %print_arg128 to i64
  call void @hew_print_value(i8 4, i64 %print_str_bits, i1 true)
  br label %bb43

bb43:                                             ; preds = %bb42
  %call_arg129 = load ptr, ptr %local_61, align 4
  %call_result130 = call i32 @hew_string_length(ptr %call_arg129)
  %ffi_sext = sext i32 %call_result130 to i64
  store i64 %ffi_sext, ptr %local_62, align 8
  br label %bb44

bb44:                                             ; preds = %bb43
  %print_arg131 = load i64, ptr %local_62, align 8
  call void @hew_print_value(i8 1, i64 %print_arg131, i1 true)
  br label %bb45

bb45:                                             ; preds = %bb44
  %"hew_string_drop drop" = load ptr, ptr %local_61, align 4
  call void @hew_string_drop(ptr %"hew_string_drop drop")
  store ptr null, ptr %local_61, align 4
  %"hew_string_drop drop132" = load ptr, ptr %local_59, align 4
  call void @hew_string_drop(ptr %"hew_string_drop drop132")
  store ptr null, ptr %local_59, align 4
  %"hew_string_drop drop133" = load ptr, ptr %local_57, align 4
  call void @hew_string_drop(ptr %"hew_string_drop drop133")
  store ptr null, ptr %local_57, align 4
  %"hew_hashset_free_layout drop" = load ptr, ptr %local_48, align 4
  call void @hew_hashset_free_layout(ptr %"hew_hashset_free_layout drop")
  store ptr null, ptr %local_48, align 4
  %"hew_hashmap_free_layout drop134" = load ptr, ptr %local_20, align 4
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop134")
  store ptr null, ptr %local_20, align 4
  %"hew_vec_free drop135" = load ptr, ptr %local_1, align 4
  call void @hew_vec_free(ptr %"hew_vec_free drop135")
  store ptr null, ptr %local_1, align 4
  ret i8 0

cancel_exit:                                      ; preds = %entry
  ret i8 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

vec_get_none:                                     ; preds = %bb6
  %machine_tag_ptr = getelementptr inbounds nuw %"Option$$i64", ptr %local_7, i32 0, i32 0
  store i8 1, ptr %machine_tag_ptr, align 1
  br label %bb7

vec_get_some:                                     ; preds = %bb6
  %machine_tag_ptr7 = getelementptr inbounds nuw %"Option$$i64", ptr %local_7, i32 0, i32 0
  store i8 0, ptr %machine_tag_ptr7, align 1
  br label %bb7

cancel_exit17:                                    ; preds = %bb9
  %"hew_vec_free drop" = load ptr, ptr %local_1, align 4
  call void @hew_vec_free(ptr %"hew_vec_free drop")
  store ptr null, ptr %local_1, align 4
  ret i8 0

after_cooperate18:                                ; preds = %bb9
  br label %bb8

cancel_exit22:                                    ; preds = %bb10
  %"hew_vec_free drop24" = load ptr, ptr %local_1, align 4
  call void @hew_vec_free(ptr %"hew_vec_free drop24")
  store ptr null, ptr %local_1, align 4
  ret i8 0

after_cooperate23:                                ; preds = %bb10
  br label %bb8

insert_overwrite_key_release:                     ; preds = %bb16
  %"hew_hashmap_insert_layout overwrite key" = load ptr, ptr %local_21, align 4
  call void @hew_string_drop(ptr %"hew_hashmap_insert_layout overwrite key")
  br label %insert_overwrite_key_cont

insert_overwrite_key_cont:                        ; preds = %insert_overwrite_key_release, %bb16
  br label %bb17

insert_overwrite_key_release39:                   ; preds = %bb17
  %"hew_hashmap_insert_layout overwrite key42" = load ptr, ptr %local_23, align 4
  call void @hew_string_drop(ptr %"hew_hashmap_insert_layout overwrite key42")
  br label %insert_overwrite_key_cont40

insert_overwrite_key_cont40:                      ; preds = %insert_overwrite_key_release39, %bb17
  br label %bb18

hashmap_get_none:                                 ; preds = %bb20
  %machine_tag_ptr46 = getelementptr inbounds nuw %"Option$$i64", ptr %local_28, i32 0, i32 0
  store i8 1, ptr %machine_tag_ptr46, align 1
  br label %bb21

hashmap_get_some:                                 ; preds = %bb20
  %machine_tag_ptr47 = getelementptr inbounds nuw %"Option$$i64", ptr %local_28, i32 0, i32 0
  store i8 0, ptr %machine_tag_ptr47, align 1
  br label %bb21

hashmap_remove_none:                              ; preds = %bb22
  %machine_tag_ptr59 = getelementptr inbounds nuw %"Option$$i64", ptr %local_38, i32 0, i32 0
  store i8 1, ptr %machine_tag_ptr59, align 1
  br label %bb29

hashmap_remove_some:                              ; preds = %bb22
  %machine_tag_ptr60 = getelementptr inbounds nuw %"Option$$i64", ptr %local_38, i32 0, i32 0
  store i8 0, ptr %machine_tag_ptr60, align 1
  br label %bb29

cancel_exit75:                                    ; preds = %bb27
  %"hew_hashmap_free_layout drop77" = load ptr, ptr %local_20, align 4
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop77")
  store ptr null, ptr %local_20, align 4
  %"hew_vec_free drop78" = load ptr, ptr %local_1, align 4
  call void @hew_vec_free(ptr %"hew_vec_free drop78")
  store ptr null, ptr %local_1, align 4
  ret i8 0

after_cooperate76:                                ; preds = %bb27
  br label %bb22

cancel_exit81:                                    ; preds = %bb28
  %"hew_hashmap_free_layout drop83" = load ptr, ptr %local_20, align 4
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop83")
  store ptr null, ptr %local_20, align 4
  %"hew_vec_free drop84" = load ptr, ptr %local_1, align 4
  call void @hew_vec_free(ptr %"hew_vec_free drop84")
  store ptr null, ptr %local_1, align 4
  ret i8 0

after_cooperate82:                                ; preds = %bb28
  br label %bb22

cancel_exit99:                                    ; preds = %bb31
  %"hew_hashmap_free_layout drop101" = load ptr, ptr %local_20, align 4
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop101")
  store ptr null, ptr %local_20, align 4
  %"hew_vec_free drop102" = load ptr, ptr %local_1, align 4
  call void @hew_vec_free(ptr %"hew_vec_free drop102")
  store ptr null, ptr %local_1, align 4
  ret i8 0

after_cooperate100:                               ; preds = %bb31
  br label %bb30

cancel_exit106:                                   ; preds = %bb32
  %"hew_hashmap_free_layout drop108" = load ptr, ptr %local_20, align 4
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop108")
  store ptr null, ptr %local_20, align 4
  %"hew_vec_free drop109" = load ptr, ptr %local_1, align 4
  call void @hew_vec_free(ptr %"hew_vec_free drop109")
  store ptr null, ptr %local_1, align 4
  ret i8 0

after_cooperate107:                               ; preds = %bb32
  br label %bb30
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
  store ptr @str_lit.6, ptr %local_3, align 4
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

define i8 @main() {
entry:
  %__original_main_call = call i8 @__original_main()
  ret i8 %__original_main_call
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

declare i32 @hew_actor_cooperate()

declare ptr @hew_vec_new_i64()

declare i1 @hew_vec_get_clone(ptr, i64, ptr)

declare void @hew_vec_free(ptr)

declare void @hew_trap_with_code(i32)

; Function Attrs: cold noreturn nounwind memory(inaccessiblemem: write)
declare void @llvm.trap() #0

declare ptr @hew_hashmap_new_with_layout(ptr, ptr)

declare i1 @hew_hashmap_insert_layout(ptr, ptr, ptr)

declare i64 @hew_hashmap_len_layout(ptr)

declare i1 @hew_hashmap_get_clone_layout(ptr, ptr, ptr)

declare i1 @hew_hashmap_remove_take_layout(ptr, ptr, ptr)

declare void @hew_hashmap_free_layout(ptr)

declare ptr @hew_hashset_new_with_layout(ptr)

declare i1 @hew_hashset_insert_layout(ptr, ptr)

declare i64 @hew_hashset_len_layout(ptr)

declare i1 @hew_hashset_contains_layout(ptr, ptr)

declare void @hew_hashset_free_layout(ptr)

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.smul.with.overflow.i64(i64, i64) #1

attributes #0 = { cold noreturn nounwind memory(inaccessiblemem: write) }
attributes #1 = { nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none) }
