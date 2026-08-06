; ModuleID = 'r4_runtime_abi'
source_filename = "r4_runtime_abi"
target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

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
  %helper_crash_cleanup_token_1 = alloca i64, align 8
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_active_1 = alloca i1, align 1
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  %helper_crash_cleanup_token_20 = alloca i64, align 8
  store i64 0, ptr %helper_crash_cleanup_token_20, align 8
  %helper_crash_cleanup_active_20 = alloca i1, align 1
  store i1 false, ptr %helper_crash_cleanup_active_20, align 1
  %helper_crash_cleanup_token_48 = alloca i64, align 8
  store i64 0, ptr %helper_crash_cleanup_token_48, align 8
  %helper_crash_cleanup_active_48 = alloca i1, align 1
  store i1 false, ptr %helper_crash_cleanup_active_48, align 1
  %helper_crash_cleanup_token_57 = alloca i64, align 8
  store i64 0, ptr %helper_crash_cleanup_token_57, align 8
  %helper_crash_cleanup_active_57 = alloca i1, align 1
  store i1 false, ptr %helper_crash_cleanup_active_57, align 1
  %helper_crash_cleanup_token_59 = alloca i64, align 8
  store i64 0, ptr %helper_crash_cleanup_token_59, align 8
  %helper_crash_cleanup_active_59 = alloca i1, align 1
  store i1 false, ptr %helper_crash_cleanup_active_59, align 1
  %helper_crash_cleanup_token_61 = alloca i64, align 8
  store i64 0, ptr %helper_crash_cleanup_token_61, align 8
  %helper_crash_cleanup_active_61 = alloca i1, align 1
  store i1 false, ptr %helper_crash_cleanup_active_61, align 1
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %hew_vec_new_i64_call = call ptr @hew_vec_new_i64()
  store ptr %hew_vec_new_i64_call, ptr %local_0, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %helper_crash_cleanup_was_active = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_was_active, label %helper_crash_cleanup_deactivate, label %helper_crash_cleanup_deactivate_merge

bb2:                                              ; preds = %frame_cleanup_registered
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
  %"hew_vec_len arg0" = load ptr, ptr %local_1, align 4
  %hew_vec_len_call = call i64 @hew_vec_len(ptr %"hew_vec_len arg0")
  store i64 %hew_vec_len_call, ptr %local_5, align 8
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

bb7:                                              ; preds = %vec_get_initialized
  %move_load7 = load %"Option$$i64", ptr %local_7, align 8
  store %"Option$$i64" %move_load7, ptr %local_8, align 8
  %machine_tag_ptr8 = getelementptr inbounds nuw %"Option$$i64", ptr %local_8, i32 0, i32 0
  %move_iN_load = load i8, ptr %machine_tag_ptr8, align 1
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

bb8:                                              ; preds = %after_cooperate22, %after_cooperate17
  %print_arg9 = load i64, ptr %local_9, align 8
  call void @hew_print_value(i8 1, i64 %print_arg9, i1 true)
  br label %bb13

bb9:                                              ; preds = %bb7
  %machine_payload_ptr10 = getelementptr inbounds nuw %"Option$$i64", ptr %local_8, i32 0, i32 1
  %machine_variant_field_ptr11 = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr10, i32 0, i32 0
  %move_load12 = load i64, ptr %machine_variant_field_ptr11, align 8
  store i64 %move_load12, ptr %local_15, align 8
  %move_load13 = load i64, ptr %local_15, align 8
  store i64 %move_load13, ptr %local_9, align 8
  %hew_actor_cooperate14 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel15 = icmp eq i32 %hew_actor_cooperate14, 2
  br i1 %hew_cooperate_is_cancel15, label %cancel_exit16, label %after_cooperate17

bb10:                                             ; preds = %bb12
  store i64 -1, ptr %local_16, align 8
  %move_load18 = load i64, ptr %local_16, align 8
  store i64 %move_load18, ptr %local_9, align 8
  %hew_actor_cooperate19 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel20 = icmp eq i32 %hew_actor_cooperate19, 2
  br i1 %hew_cooperate_is_cancel20, label %cancel_exit21, label %after_cooperate22

bb11:                                             ; preds = %bb12
  %helper_crash_cleanup_drop_active31 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active31, label %helper_crash_cleanup_retire32, label %helper_crash_cleanup_retire_merge33

bb12:                                             ; preds = %bb7
  store i64 1, ptr %local_13, align 8
  %cmp_lhs39 = load i64, ptr %local_10, align 8
  %cmp_rhs40 = load i64, ptr %local_13, align 8
  %cmp_bit41 = icmp eq i64 %cmp_lhs39, %cmp_rhs40
  %cmp_zext42 = zext i1 %cmp_bit41 to i8
  store i8 %cmp_zext42, ptr %local_14, align 1
  %cond_load43 = load i8, ptr %local_14, align 1
  %cond_nz44 = icmp ne i8 %cond_load43, 0
  br i1 %cond_nz44, label %bb10, label %bb11

bb13:                                             ; preds = %bb8
  %call_arg45 = load ptr, ptr %local_1, align 4
  %call_result = call i64 @hew_vec_pop_i64(ptr %call_arg45)
  store i64 %call_result, ptr %local_17, align 8
  br label %bb14

bb14:                                             ; preds = %bb13
  %move_load46 = load i64, ptr %local_17, align 8
  store i64 %move_load46, ptr %local_18, align 8
  %print_arg47 = load i64, ptr %local_18, align 8
  call void @hew_print_value(i8 1, i64 %print_arg47, i1 true)
  br label %bb15

bb15:                                             ; preds = %bb14
  %hew_hashmap_new_with_layout_call = call ptr @hew_hashmap_new_with_layout(ptr @hew_layout_key_string, ptr @hew_layout_val_i64)
  store ptr %hew_hashmap_new_with_layout_call, ptr %local_19, align 4
  br label %bb16

bb16:                                             ; preds = %bb15
  %helper_crash_cleanup_was_active48 = load i1, ptr %helper_crash_cleanup_active_20, align 1
  br i1 %helper_crash_cleanup_was_active48, label %helper_crash_cleanup_deactivate49, label %helper_crash_cleanup_deactivate_merge50

bb17:                                             ; preds = %insert_overwrite_key_cont
  store ptr @str_lit.1, ptr %local_23, align 4
  store i64 20, ptr %local_24, align 8
  %"hew_hashmap_insert_layout arg061" = load ptr, ptr %local_20, align 4
  %hew_hashmap_insert_layout_call62 = call i1 @hew_hashmap_insert_layout(ptr %"hew_hashmap_insert_layout arg061", ptr %local_23, ptr %local_24)
  %insert_existed65 = icmp eq i1 %hew_hashmap_insert_layout_call62, false
  br i1 %insert_existed65, label %insert_overwrite_key_release63, label %insert_overwrite_key_cont64

bb18:                                             ; preds = %insert_overwrite_key_cont64
  %"hew_hashmap_len_layout arg0" = load ptr, ptr %local_20, align 4
  %hew_hashmap_len_layout_call = call i64 @hew_hashmap_len_layout(ptr %"hew_hashmap_len_layout arg0")
  store i64 %hew_hashmap_len_layout_call, ptr %local_25, align 8
  br label %bb19

bb19:                                             ; preds = %bb18
  %print_arg67 = load i64, ptr %local_25, align 8
  call void @hew_print_value(i8 1, i64 %print_arg67, i1 true)
  br label %bb20

bb20:                                             ; preds = %bb19
  store ptr @str_lit.2, ptr %local_27, align 4
  %"hew_hashmap_get_layout arg0" = load ptr, ptr %local_20, align 4
  %machine_payload_ptr68 = getelementptr inbounds nuw %"Option$$i64", ptr %local_28, i32 0, i32 1
  %machine_variant_field_ptr69 = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr68, i32 0, i32 0
  %hew_hashmap_get_clone_layout_call = call i1 @hew_hashmap_get_clone_layout(ptr %"hew_hashmap_get_layout arg0", ptr %local_27, ptr %machine_variant_field_ptr69)
  br i1 %hew_hashmap_get_clone_layout_call, label %hashmap_get_some, label %hashmap_get_none

bb21:                                             ; preds = %hashmap_get_initialized
  %machine_tag_ptr72 = getelementptr inbounds nuw %"Option$$i64", ptr %local_28, i32 0, i32 0
  %move_iN_load73 = load i8, ptr %machine_tag_ptr72, align 1
  %move_iN_zext74 = zext i8 %move_iN_load73 to i64
  store i64 %move_iN_zext74, ptr %local_29, align 8
  store i64 0, ptr %local_30, align 8
  %cmp_lhs75 = load i64, ptr %local_29, align 8
  %cmp_rhs76 = load i64, ptr %local_30, align 8
  %cmp_bit77 = icmp eq i64 %cmp_lhs75, %cmp_rhs76
  %cmp_zext78 = zext i1 %cmp_bit77 to i8
  store i8 %cmp_zext78, ptr %local_31, align 1
  %cond_load79 = load i8, ptr %local_31, align 1
  %cond_nz80 = icmp ne i8 %cond_load79, 0
  br i1 %cond_nz80, label %bb23, label %bb26

bb22:                                             ; preds = %after_cooperate134, %after_cooperate114
  store ptr @str_lit.3, ptr %local_37, align 4
  %"hew_hashmap_remove_take_layout arg0" = load ptr, ptr %local_20, align 4
  %machine_payload_ptr81 = getelementptr inbounds nuw %"Option$$i64", ptr %local_38, i32 0, i32 1
  %machine_variant_field_ptr82 = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr81, i32 0, i32 0
  %hew_hashmap_remove_take_layout_call = call i1 @hew_hashmap_remove_take_layout(ptr %"hew_hashmap_remove_take_layout arg0", ptr %local_37, ptr %machine_variant_field_ptr82)
  br i1 %hew_hashmap_remove_take_layout_call, label %hashmap_remove_some, label %hashmap_remove_none

bb23:                                             ; preds = %bb21
  %machine_payload_ptr85 = getelementptr inbounds nuw %"Option$$i64", ptr %local_28, i32 0, i32 1
  %machine_variant_field_ptr86 = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr85, i32 0, i32 0
  %move_load87 = load i64, ptr %machine_variant_field_ptr86, align 8
  store i64 %move_load87, ptr %local_34, align 8
  %print_arg88 = load i64, ptr %local_34, align 8
  call void @hew_print_value(i8 1, i64 %print_arg88, i1 true)
  br label %bb27

bb24:                                             ; preds = %bb26
  store i64 -1, ptr %local_35, align 8
  %print_arg89 = load i64, ptr %local_35, align 8
  call void @hew_print_value(i8 1, i64 %print_arg89, i1 true)
  br label %bb28

bb25:                                             ; preds = %bb26
  %helper_crash_cleanup_drop_active90 = load i1, ptr %helper_crash_cleanup_active_20, align 1
  br i1 %helper_crash_cleanup_drop_active90, label %helper_crash_cleanup_retire91, label %helper_crash_cleanup_retire_merge92

bb26:                                             ; preds = %bb21
  store i64 1, ptr %local_32, align 8
  %cmp_lhs105 = load i64, ptr %local_29, align 8
  %cmp_rhs106 = load i64, ptr %local_32, align 8
  %cmp_bit107 = icmp eq i64 %cmp_lhs105, %cmp_rhs106
  %cmp_zext108 = zext i1 %cmp_bit107 to i8
  store i8 %cmp_zext108, ptr %local_33, align 1
  %cond_load109 = load i8, ptr %local_33, align 1
  %cond_nz110 = icmp ne i8 %cond_load109, 0
  br i1 %cond_nz110, label %bb24, label %bb25

bb27:                                             ; preds = %bb23
  %hew_actor_cooperate111 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel112 = icmp eq i32 %hew_actor_cooperate111, 2
  br i1 %hew_cooperate_is_cancel112, label %cancel_exit113, label %after_cooperate114

bb28:                                             ; preds = %bb24
  %hew_actor_cooperate131 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel132 = icmp eq i32 %hew_actor_cooperate131, 2
  br i1 %hew_cooperate_is_cancel132, label %cancel_exit133, label %after_cooperate134

bb29:                                             ; preds = %hashmap_remove_initialized
  %machine_tag_ptr151 = getelementptr inbounds nuw %"Option$$i64", ptr %local_38, i32 0, i32 0
  %move_iN_load152 = load i8, ptr %machine_tag_ptr151, align 1
  %move_iN_zext153 = zext i8 %move_iN_load152 to i64
  store i64 %move_iN_zext153, ptr %local_39, align 8
  store i64 0, ptr %local_40, align 8
  %cmp_lhs154 = load i64, ptr %local_39, align 8
  %cmp_rhs155 = load i64, ptr %local_40, align 8
  %cmp_bit156 = icmp eq i64 %cmp_lhs154, %cmp_rhs155
  %cmp_zext157 = zext i1 %cmp_bit156 to i8
  store i8 %cmp_zext157, ptr %local_41, align 1
  %cond_load158 = load i8, ptr %local_41, align 1
  %cond_nz159 = icmp ne i8 %cond_load158, 0
  br i1 %cond_nz159, label %bb31, label %bb34

bb30:                                             ; preds = %after_cooperate187, %after_cooperate166
  %move_load160 = load i8, ptr %local_36, align 1
  store i8 %move_load160, ptr %local_46, align 1
  %print_arg161 = load i8, ptr %local_46, align 1
  %print_narrow_bits = zext i8 %print_arg161 to i64
  call void @hew_print_value(i8 3, i64 %print_narrow_bits, i1 true)
  br label %bb35

bb31:                                             ; preds = %bb29
  store i8 1, ptr %local_44, align 1
  %move_load162 = load i8, ptr %local_44, align 1
  store i8 %move_load162, ptr %local_36, align 1
  %hew_actor_cooperate163 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel164 = icmp eq i32 %hew_actor_cooperate163, 2
  br i1 %hew_cooperate_is_cancel164, label %cancel_exit165, label %after_cooperate166

bb32:                                             ; preds = %bb34
  store i8 0, ptr %local_45, align 1
  %move_load183 = load i8, ptr %local_45, align 1
  store i8 %move_load183, ptr %local_36, align 1
  %hew_actor_cooperate184 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel185 = icmp eq i32 %hew_actor_cooperate184, 2
  br i1 %hew_cooperate_is_cancel185, label %cancel_exit186, label %after_cooperate187

bb33:                                             ; preds = %bb34
  %helper_crash_cleanup_drop_active204 = load i1, ptr %helper_crash_cleanup_active_20, align 1
  br i1 %helper_crash_cleanup_drop_active204, label %helper_crash_cleanup_retire205, label %helper_crash_cleanup_retire_merge206

bb34:                                             ; preds = %bb29
  store i64 1, ptr %local_42, align 8
  %cmp_lhs220 = load i64, ptr %local_39, align 8
  %cmp_rhs221 = load i64, ptr %local_42, align 8
  %cmp_bit222 = icmp eq i64 %cmp_lhs220, %cmp_rhs221
  %cmp_zext223 = zext i1 %cmp_bit222 to i8
  store i8 %cmp_zext223, ptr %local_43, align 1
  %cond_load224 = load i8, ptr %local_43, align 1
  %cond_nz225 = icmp ne i8 %cond_load224, 0
  br i1 %cond_nz225, label %bb32, label %bb33

bb35:                                             ; preds = %bb30
  %hew_hashset_new_with_layout_call = call ptr @hew_hashset_new_with_layout(ptr @hew_layout_key_i64)
  store ptr %hew_hashset_new_with_layout_call, ptr %local_47, align 4
  br label %bb36

bb36:                                             ; preds = %bb35
  %helper_crash_cleanup_was_active226 = load i1, ptr %helper_crash_cleanup_active_48, align 1
  br i1 %helper_crash_cleanup_was_active226, label %helper_crash_cleanup_deactivate227, label %helper_crash_cleanup_deactivate_merge228

bb37:                                             ; preds = %frame_cleanup_registered237
  store i64 7, ptr %local_51, align 8
  %"hew_hashset_insert_layout arg0239" = load ptr, ptr %local_48, align 4
  %hew_hashset_insert_layout_call240 = call i1 @hew_hashset_insert_layout(ptr %"hew_hashset_insert_layout arg0239", ptr %local_51)
  %hashset_insert_bool_zext_i8241 = zext i1 %hew_hashset_insert_layout_call240 to i8
  store i8 %hashset_insert_bool_zext_i8241, ptr %local_52, align 1
  br label %bb38

bb38:                                             ; preds = %bb37
  %"hew_hashset_len_layout arg0" = load ptr, ptr %local_48, align 4
  %hew_hashset_len_layout_call = call i64 @hew_hashset_len_layout(ptr %"hew_hashset_len_layout arg0")
  store i64 %hew_hashset_len_layout_call, ptr %local_53, align 8
  br label %bb39

bb39:                                             ; preds = %bb38
  %print_arg242 = load i64, ptr %local_53, align 8
  call void @hew_print_value(i8 1, i64 %print_arg242, i1 true)
  br label %bb40

bb40:                                             ; preds = %bb39
  store i64 42, ptr %local_54, align 8
  %"hew_hashset_contains_layout arg0" = load ptr, ptr %local_48, align 4
  %hew_hashset_contains_layout_call = call i1 @hew_hashset_contains_layout(ptr %"hew_hashset_contains_layout arg0", ptr %local_54)
  %hashset_contains_bool_zext_i8 = zext i1 %hew_hashset_contains_layout_call to i8
  store i8 %hashset_contains_bool_zext_i8, ptr %local_55, align 1
  br label %bb41

bb41:                                             ; preds = %bb40
  %print_arg243 = load i8, ptr %local_55, align 1
  %print_narrow_bits244 = zext i8 %print_arg243 to i64
  call void @hew_print_value(i8 3, i64 %print_narrow_bits244, i1 true)
  br label %bb42

bb42:                                             ; preds = %bb41
  store ptr @str_lit.4, ptr %local_56, align 4
  %helper_crash_cleanup_was_active245 = load i1, ptr %helper_crash_cleanup_active_57, align 1
  br i1 %helper_crash_cleanup_was_active245, label %helper_crash_cleanup_deactivate246, label %helper_crash_cleanup_deactivate_merge247

bb43:                                             ; preds = %frame_cleanup_registered282
  %call_arg285 = load ptr, ptr %local_61, align 4
  %call_result286 = call i32 @hew_string_length(ptr %call_arg285)
  %ffi_sext = sext i32 %call_result286 to i64
  store i64 %ffi_sext, ptr %local_62, align 8
  br label %bb44

bb44:                                             ; preds = %bb43
  %print_arg287 = load i64, ptr %local_62, align 8
  call void @hew_print_value(i8 1, i64 %print_arg287, i1 true)
  br label %bb45

bb45:                                             ; preds = %bb44
  %helper_crash_cleanup_drop_active288 = load i1, ptr %helper_crash_cleanup_active_61, align 1
  br i1 %helper_crash_cleanup_drop_active288, label %helper_crash_cleanup_retire289, label %helper_crash_cleanup_retire_merge290

cancel_exit:                                      ; preds = %entry
  ret i8 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

helper_crash_cleanup_deactivate:                  ; preds = %bb1
  %helper_crash_cleanup_token = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_deactivate_call = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token)
  br i1 %helper_crash_cleanup_deactivate_call, label %helper_crash_cleanup_deactivate_accepted, label %helper_crash_cleanup_deactivate_rejected

helper_crash_cleanup_deactivate_merge:            ; preds = %helper_crash_cleanup_deactivate_accepted, %bb1
  %move_load = load ptr, ptr %local_0, align 4
  store ptr %move_load, ptr %local_1, align 4
  %helper_crash_cleanup_prior_token = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %arm_typed_crash_cleanup = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token, ptr %local_1, i64 4, i64 4, ptr @__hew_frame_cleanup_7fdeddf79806b8a4, i32 1, i32 0)
  %frame_cleanup_arm_failed = icmp eq i64 %arm_typed_crash_cleanup, -1
  br i1 %frame_cleanup_arm_failed, label %frame_cleanup_rejected, label %frame_cleanup_registered

helper_crash_cleanup_deactivate_accepted:         ; preds = %helper_crash_cleanup_deactivate
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_deactivate_merge

helper_crash_cleanup_deactivate_rejected:         ; preds = %helper_crash_cleanup_deactivate
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

frame_cleanup_registered:                         ; preds = %helper_crash_cleanup_deactivate_merge
  store i64 %arm_typed_crash_cleanup, ptr %helper_crash_cleanup_token_1, align 8
  store i1 true, ptr %helper_crash_cleanup_active_1, align 1
  store i64 1, ptr %local_2, align 8
  %call_arg = load ptr, ptr %local_1, align 4
  %call_arg1 = load i64, ptr %local_2, align 8
  call void @hew_vec_push_i64(ptr %call_arg, i64 %call_arg1)
  br label %bb2

frame_cleanup_rejected:                           ; preds = %helper_crash_cleanup_deactivate_merge
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

vec_get_none:                                     ; preds = %bb6
  %machine_tag_ptr = getelementptr inbounds nuw %"Option$$i64", ptr %local_7, i32 0, i32 0
  store i8 1, ptr %machine_tag_ptr, align 1
  br label %vec_get_initialized

vec_get_some:                                     ; preds = %bb6
  %machine_tag_ptr6 = getelementptr inbounds nuw %"Option$$i64", ptr %local_7, i32 0, i32 0
  store i8 0, ptr %machine_tag_ptr6, align 1
  br label %vec_get_initialized

vec_get_initialized:                              ; preds = %vec_get_some, %vec_get_none
  br label %bb7

cancel_exit16:                                    ; preds = %bb9
  %helper_crash_cleanup_drop_active = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active, label %helper_crash_cleanup_retire, label %helper_crash_cleanup_retire_merge

after_cooperate17:                                ; preds = %bb9
  br label %bb8

helper_crash_cleanup_retire:                      ; preds = %cancel_exit16
  %helper_crash_cleanup_retire_token = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token)
  br i1 %helper_crash_cleanup_retire_call, label %helper_crash_cleanup_retire_accepted, label %helper_crash_cleanup_retire_rejected

helper_crash_cleanup_retire_merge:                ; preds = %helper_crash_cleanup_retire_accepted, %cancel_exit16
  %"hew_vec_free drop" = load ptr, ptr %local_1, align 4
  call void @hew_vec_free(ptr %"hew_vec_free drop")
  store ptr null, ptr %local_1, align 4
  ret i8 0

helper_crash_cleanup_retire_accepted:             ; preds = %helper_crash_cleanup_retire
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge

helper_crash_cleanup_retire_rejected:             ; preds = %helper_crash_cleanup_retire
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

cancel_exit21:                                    ; preds = %bb10
  %helper_crash_cleanup_drop_active23 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active23, label %helper_crash_cleanup_retire24, label %helper_crash_cleanup_retire_merge25

after_cooperate22:                                ; preds = %bb10
  br label %bb8

helper_crash_cleanup_retire24:                    ; preds = %cancel_exit21
  %helper_crash_cleanup_retire_token26 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call27 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token26)
  br i1 %helper_crash_cleanup_retire_call27, label %helper_crash_cleanup_retire_accepted28, label %helper_crash_cleanup_retire_rejected29

helper_crash_cleanup_retire_merge25:              ; preds = %helper_crash_cleanup_retire_accepted28, %cancel_exit21
  %"hew_vec_free drop30" = load ptr, ptr %local_1, align 4
  call void @hew_vec_free(ptr %"hew_vec_free drop30")
  store ptr null, ptr %local_1, align 4
  ret i8 0

helper_crash_cleanup_retire_accepted28:           ; preds = %helper_crash_cleanup_retire24
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge25

helper_crash_cleanup_retire_rejected29:           ; preds = %helper_crash_cleanup_retire24
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire32:                    ; preds = %bb11
  %helper_crash_cleanup_retire_token34 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call35 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token34)
  br i1 %helper_crash_cleanup_retire_call35, label %helper_crash_cleanup_retire_accepted36, label %helper_crash_cleanup_retire_rejected37

helper_crash_cleanup_retire_merge33:              ; preds = %helper_crash_cleanup_retire_accepted36, %bb11
  %"hew_vec_free drop38" = load ptr, ptr %local_1, align 4
  call void @hew_vec_free(ptr %"hew_vec_free drop38")
  store ptr null, ptr %local_1, align 4
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire_accepted36:           ; preds = %helper_crash_cleanup_retire32
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge33

helper_crash_cleanup_retire_rejected37:           ; preds = %helper_crash_cleanup_retire32
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_deactivate49:                ; preds = %bb16
  %helper_crash_cleanup_token51 = load i64, ptr %helper_crash_cleanup_token_20, align 8
  %helper_crash_cleanup_deactivate_call52 = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token51)
  br i1 %helper_crash_cleanup_deactivate_call52, label %helper_crash_cleanup_deactivate_accepted53, label %helper_crash_cleanup_deactivate_rejected54

helper_crash_cleanup_deactivate_merge50:          ; preds = %helper_crash_cleanup_deactivate_accepted53, %bb16
  %move_load55 = load ptr, ptr %local_19, align 4
  store ptr %move_load55, ptr %local_20, align 4
  %helper_crash_cleanup_prior_token56 = load i64, ptr %helper_crash_cleanup_token_20, align 8
  %arm_typed_crash_cleanup57 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token56, ptr %local_20, i64 4, i64 4, ptr @__hew_frame_cleanup_01796e325d15e37f, i32 1, i32 0)
  %frame_cleanup_arm_failed58 = icmp eq i64 %arm_typed_crash_cleanup57, -1
  br i1 %frame_cleanup_arm_failed58, label %frame_cleanup_rejected60, label %frame_cleanup_registered59

helper_crash_cleanup_deactivate_accepted53:       ; preds = %helper_crash_cleanup_deactivate49
  store i1 false, ptr %helper_crash_cleanup_active_20, align 1
  br label %helper_crash_cleanup_deactivate_merge50

helper_crash_cleanup_deactivate_rejected54:       ; preds = %helper_crash_cleanup_deactivate49
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

frame_cleanup_registered59:                       ; preds = %helper_crash_cleanup_deactivate_merge50
  store i64 %arm_typed_crash_cleanup57, ptr %helper_crash_cleanup_token_20, align 8
  store i1 true, ptr %helper_crash_cleanup_active_20, align 1
  store ptr @str_lit, ptr %local_21, align 4
  store i64 10, ptr %local_22, align 8
  %"hew_hashmap_insert_layout arg0" = load ptr, ptr %local_20, align 4
  %hew_hashmap_insert_layout_call = call i1 @hew_hashmap_insert_layout(ptr %"hew_hashmap_insert_layout arg0", ptr %local_21, ptr %local_22)
  %insert_existed = icmp eq i1 %hew_hashmap_insert_layout_call, false
  br i1 %insert_existed, label %insert_overwrite_key_release, label %insert_overwrite_key_cont

frame_cleanup_rejected60:                         ; preds = %helper_crash_cleanup_deactivate_merge50
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

insert_overwrite_key_release:                     ; preds = %frame_cleanup_registered59
  %"hew_hashmap_insert_layout overwrite key" = load ptr, ptr %local_21, align 4
  call void @hew_string_drop(ptr %"hew_hashmap_insert_layout overwrite key")
  br label %insert_overwrite_key_cont

insert_overwrite_key_cont:                        ; preds = %insert_overwrite_key_release, %frame_cleanup_registered59
  br label %bb17

insert_overwrite_key_release63:                   ; preds = %bb17
  %"hew_hashmap_insert_layout overwrite key66" = load ptr, ptr %local_23, align 4
  call void @hew_string_drop(ptr %"hew_hashmap_insert_layout overwrite key66")
  br label %insert_overwrite_key_cont64

insert_overwrite_key_cont64:                      ; preds = %insert_overwrite_key_release63, %bb17
  br label %bb18

hashmap_get_none:                                 ; preds = %bb20
  %machine_tag_ptr70 = getelementptr inbounds nuw %"Option$$i64", ptr %local_28, i32 0, i32 0
  store i8 1, ptr %machine_tag_ptr70, align 1
  br label %hashmap_get_initialized

hashmap_get_some:                                 ; preds = %bb20
  %machine_tag_ptr71 = getelementptr inbounds nuw %"Option$$i64", ptr %local_28, i32 0, i32 0
  store i8 0, ptr %machine_tag_ptr71, align 1
  br label %hashmap_get_initialized

hashmap_get_initialized:                          ; preds = %hashmap_get_some, %hashmap_get_none
  br label %bb21

hashmap_remove_none:                              ; preds = %bb22
  %machine_tag_ptr83 = getelementptr inbounds nuw %"Option$$i64", ptr %local_38, i32 0, i32 0
  store i8 1, ptr %machine_tag_ptr83, align 1
  br label %hashmap_remove_initialized

hashmap_remove_some:                              ; preds = %bb22
  %machine_tag_ptr84 = getelementptr inbounds nuw %"Option$$i64", ptr %local_38, i32 0, i32 0
  store i8 0, ptr %machine_tag_ptr84, align 1
  br label %hashmap_remove_initialized

hashmap_remove_initialized:                       ; preds = %hashmap_remove_some, %hashmap_remove_none
  br label %bb29

helper_crash_cleanup_retire91:                    ; preds = %bb25
  %helper_crash_cleanup_retire_token93 = load i64, ptr %helper_crash_cleanup_token_20, align 8
  %helper_crash_cleanup_retire_call94 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token93)
  br i1 %helper_crash_cleanup_retire_call94, label %helper_crash_cleanup_retire_accepted95, label %helper_crash_cleanup_retire_rejected96

helper_crash_cleanup_retire_merge92:              ; preds = %helper_crash_cleanup_retire_accepted95, %bb25
  %"hew_hashmap_free_layout drop" = load ptr, ptr %local_20, align 4
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop")
  store ptr null, ptr %local_20, align 4
  %helper_crash_cleanup_drop_active97 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active97, label %helper_crash_cleanup_retire98, label %helper_crash_cleanup_retire_merge99

helper_crash_cleanup_retire_accepted95:           ; preds = %helper_crash_cleanup_retire91
  store i64 0, ptr %helper_crash_cleanup_token_20, align 8
  store i1 false, ptr %helper_crash_cleanup_active_20, align 1
  br label %helper_crash_cleanup_retire_merge92

helper_crash_cleanup_retire_rejected96:           ; preds = %helper_crash_cleanup_retire91
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire98:                    ; preds = %helper_crash_cleanup_retire_merge92
  %helper_crash_cleanup_retire_token100 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call101 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token100)
  br i1 %helper_crash_cleanup_retire_call101, label %helper_crash_cleanup_retire_accepted102, label %helper_crash_cleanup_retire_rejected103

helper_crash_cleanup_retire_merge99:              ; preds = %helper_crash_cleanup_retire_accepted102, %helper_crash_cleanup_retire_merge92
  %"hew_vec_free drop104" = load ptr, ptr %local_1, align 4
  call void @hew_vec_free(ptr %"hew_vec_free drop104")
  store ptr null, ptr %local_1, align 4
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire_accepted102:          ; preds = %helper_crash_cleanup_retire98
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge99

helper_crash_cleanup_retire_rejected103:          ; preds = %helper_crash_cleanup_retire98
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

cancel_exit113:                                   ; preds = %bb27
  %helper_crash_cleanup_drop_active115 = load i1, ptr %helper_crash_cleanup_active_20, align 1
  br i1 %helper_crash_cleanup_drop_active115, label %helper_crash_cleanup_retire116, label %helper_crash_cleanup_retire_merge117

after_cooperate114:                               ; preds = %bb27
  br label %bb22

helper_crash_cleanup_retire116:                   ; preds = %cancel_exit113
  %helper_crash_cleanup_retire_token118 = load i64, ptr %helper_crash_cleanup_token_20, align 8
  %helper_crash_cleanup_retire_call119 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token118)
  br i1 %helper_crash_cleanup_retire_call119, label %helper_crash_cleanup_retire_accepted120, label %helper_crash_cleanup_retire_rejected121

helper_crash_cleanup_retire_merge117:             ; preds = %helper_crash_cleanup_retire_accepted120, %cancel_exit113
  %"hew_hashmap_free_layout drop122" = load ptr, ptr %local_20, align 4
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop122")
  store ptr null, ptr %local_20, align 4
  %helper_crash_cleanup_drop_active123 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active123, label %helper_crash_cleanup_retire124, label %helper_crash_cleanup_retire_merge125

helper_crash_cleanup_retire_accepted120:          ; preds = %helper_crash_cleanup_retire116
  store i64 0, ptr %helper_crash_cleanup_token_20, align 8
  store i1 false, ptr %helper_crash_cleanup_active_20, align 1
  br label %helper_crash_cleanup_retire_merge117

helper_crash_cleanup_retire_rejected121:          ; preds = %helper_crash_cleanup_retire116
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire124:                   ; preds = %helper_crash_cleanup_retire_merge117
  %helper_crash_cleanup_retire_token126 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call127 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token126)
  br i1 %helper_crash_cleanup_retire_call127, label %helper_crash_cleanup_retire_accepted128, label %helper_crash_cleanup_retire_rejected129

helper_crash_cleanup_retire_merge125:             ; preds = %helper_crash_cleanup_retire_accepted128, %helper_crash_cleanup_retire_merge117
  %"hew_vec_free drop130" = load ptr, ptr %local_1, align 4
  call void @hew_vec_free(ptr %"hew_vec_free drop130")
  store ptr null, ptr %local_1, align 4
  ret i8 0

helper_crash_cleanup_retire_accepted128:          ; preds = %helper_crash_cleanup_retire124
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge125

helper_crash_cleanup_retire_rejected129:          ; preds = %helper_crash_cleanup_retire124
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

cancel_exit133:                                   ; preds = %bb28
  %helper_crash_cleanup_drop_active135 = load i1, ptr %helper_crash_cleanup_active_20, align 1
  br i1 %helper_crash_cleanup_drop_active135, label %helper_crash_cleanup_retire136, label %helper_crash_cleanup_retire_merge137

after_cooperate134:                               ; preds = %bb28
  br label %bb22

helper_crash_cleanup_retire136:                   ; preds = %cancel_exit133
  %helper_crash_cleanup_retire_token138 = load i64, ptr %helper_crash_cleanup_token_20, align 8
  %helper_crash_cleanup_retire_call139 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token138)
  br i1 %helper_crash_cleanup_retire_call139, label %helper_crash_cleanup_retire_accepted140, label %helper_crash_cleanup_retire_rejected141

helper_crash_cleanup_retire_merge137:             ; preds = %helper_crash_cleanup_retire_accepted140, %cancel_exit133
  %"hew_hashmap_free_layout drop142" = load ptr, ptr %local_20, align 4
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop142")
  store ptr null, ptr %local_20, align 4
  %helper_crash_cleanup_drop_active143 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active143, label %helper_crash_cleanup_retire144, label %helper_crash_cleanup_retire_merge145

helper_crash_cleanup_retire_accepted140:          ; preds = %helper_crash_cleanup_retire136
  store i64 0, ptr %helper_crash_cleanup_token_20, align 8
  store i1 false, ptr %helper_crash_cleanup_active_20, align 1
  br label %helper_crash_cleanup_retire_merge137

helper_crash_cleanup_retire_rejected141:          ; preds = %helper_crash_cleanup_retire136
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire144:                   ; preds = %helper_crash_cleanup_retire_merge137
  %helper_crash_cleanup_retire_token146 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call147 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token146)
  br i1 %helper_crash_cleanup_retire_call147, label %helper_crash_cleanup_retire_accepted148, label %helper_crash_cleanup_retire_rejected149

helper_crash_cleanup_retire_merge145:             ; preds = %helper_crash_cleanup_retire_accepted148, %helper_crash_cleanup_retire_merge137
  %"hew_vec_free drop150" = load ptr, ptr %local_1, align 4
  call void @hew_vec_free(ptr %"hew_vec_free drop150")
  store ptr null, ptr %local_1, align 4
  ret i8 0

helper_crash_cleanup_retire_accepted148:          ; preds = %helper_crash_cleanup_retire144
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge145

helper_crash_cleanup_retire_rejected149:          ; preds = %helper_crash_cleanup_retire144
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

cancel_exit165:                                   ; preds = %bb31
  %helper_crash_cleanup_drop_active167 = load i1, ptr %helper_crash_cleanup_active_20, align 1
  br i1 %helper_crash_cleanup_drop_active167, label %helper_crash_cleanup_retire168, label %helper_crash_cleanup_retire_merge169

after_cooperate166:                               ; preds = %bb31
  br label %bb30

helper_crash_cleanup_retire168:                   ; preds = %cancel_exit165
  %helper_crash_cleanup_retire_token170 = load i64, ptr %helper_crash_cleanup_token_20, align 8
  %helper_crash_cleanup_retire_call171 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token170)
  br i1 %helper_crash_cleanup_retire_call171, label %helper_crash_cleanup_retire_accepted172, label %helper_crash_cleanup_retire_rejected173

helper_crash_cleanup_retire_merge169:             ; preds = %helper_crash_cleanup_retire_accepted172, %cancel_exit165
  %"hew_hashmap_free_layout drop174" = load ptr, ptr %local_20, align 4
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop174")
  store ptr null, ptr %local_20, align 4
  %helper_crash_cleanup_drop_active175 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active175, label %helper_crash_cleanup_retire176, label %helper_crash_cleanup_retire_merge177

helper_crash_cleanup_retire_accepted172:          ; preds = %helper_crash_cleanup_retire168
  store i64 0, ptr %helper_crash_cleanup_token_20, align 8
  store i1 false, ptr %helper_crash_cleanup_active_20, align 1
  br label %helper_crash_cleanup_retire_merge169

helper_crash_cleanup_retire_rejected173:          ; preds = %helper_crash_cleanup_retire168
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire176:                   ; preds = %helper_crash_cleanup_retire_merge169
  %helper_crash_cleanup_retire_token178 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call179 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token178)
  br i1 %helper_crash_cleanup_retire_call179, label %helper_crash_cleanup_retire_accepted180, label %helper_crash_cleanup_retire_rejected181

helper_crash_cleanup_retire_merge177:             ; preds = %helper_crash_cleanup_retire_accepted180, %helper_crash_cleanup_retire_merge169
  %"hew_vec_free drop182" = load ptr, ptr %local_1, align 4
  call void @hew_vec_free(ptr %"hew_vec_free drop182")
  store ptr null, ptr %local_1, align 4
  ret i8 0

helper_crash_cleanup_retire_accepted180:          ; preds = %helper_crash_cleanup_retire176
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge177

helper_crash_cleanup_retire_rejected181:          ; preds = %helper_crash_cleanup_retire176
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

cancel_exit186:                                   ; preds = %bb32
  %helper_crash_cleanup_drop_active188 = load i1, ptr %helper_crash_cleanup_active_20, align 1
  br i1 %helper_crash_cleanup_drop_active188, label %helper_crash_cleanup_retire189, label %helper_crash_cleanup_retire_merge190

after_cooperate187:                               ; preds = %bb32
  br label %bb30

helper_crash_cleanup_retire189:                   ; preds = %cancel_exit186
  %helper_crash_cleanup_retire_token191 = load i64, ptr %helper_crash_cleanup_token_20, align 8
  %helper_crash_cleanup_retire_call192 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token191)
  br i1 %helper_crash_cleanup_retire_call192, label %helper_crash_cleanup_retire_accepted193, label %helper_crash_cleanup_retire_rejected194

helper_crash_cleanup_retire_merge190:             ; preds = %helper_crash_cleanup_retire_accepted193, %cancel_exit186
  %"hew_hashmap_free_layout drop195" = load ptr, ptr %local_20, align 4
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop195")
  store ptr null, ptr %local_20, align 4
  %helper_crash_cleanup_drop_active196 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active196, label %helper_crash_cleanup_retire197, label %helper_crash_cleanup_retire_merge198

helper_crash_cleanup_retire_accepted193:          ; preds = %helper_crash_cleanup_retire189
  store i64 0, ptr %helper_crash_cleanup_token_20, align 8
  store i1 false, ptr %helper_crash_cleanup_active_20, align 1
  br label %helper_crash_cleanup_retire_merge190

helper_crash_cleanup_retire_rejected194:          ; preds = %helper_crash_cleanup_retire189
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire197:                   ; preds = %helper_crash_cleanup_retire_merge190
  %helper_crash_cleanup_retire_token199 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call200 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token199)
  br i1 %helper_crash_cleanup_retire_call200, label %helper_crash_cleanup_retire_accepted201, label %helper_crash_cleanup_retire_rejected202

helper_crash_cleanup_retire_merge198:             ; preds = %helper_crash_cleanup_retire_accepted201, %helper_crash_cleanup_retire_merge190
  %"hew_vec_free drop203" = load ptr, ptr %local_1, align 4
  call void @hew_vec_free(ptr %"hew_vec_free drop203")
  store ptr null, ptr %local_1, align 4
  ret i8 0

helper_crash_cleanup_retire_accepted201:          ; preds = %helper_crash_cleanup_retire197
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge198

helper_crash_cleanup_retire_rejected202:          ; preds = %helper_crash_cleanup_retire197
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire205:                   ; preds = %bb33
  %helper_crash_cleanup_retire_token207 = load i64, ptr %helper_crash_cleanup_token_20, align 8
  %helper_crash_cleanup_retire_call208 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token207)
  br i1 %helper_crash_cleanup_retire_call208, label %helper_crash_cleanup_retire_accepted209, label %helper_crash_cleanup_retire_rejected210

helper_crash_cleanup_retire_merge206:             ; preds = %helper_crash_cleanup_retire_accepted209, %bb33
  %"hew_hashmap_free_layout drop211" = load ptr, ptr %local_20, align 4
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop211")
  store ptr null, ptr %local_20, align 4
  %helper_crash_cleanup_drop_active212 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active212, label %helper_crash_cleanup_retire213, label %helper_crash_cleanup_retire_merge214

helper_crash_cleanup_retire_accepted209:          ; preds = %helper_crash_cleanup_retire205
  store i64 0, ptr %helper_crash_cleanup_token_20, align 8
  store i1 false, ptr %helper_crash_cleanup_active_20, align 1
  br label %helper_crash_cleanup_retire_merge206

helper_crash_cleanup_retire_rejected210:          ; preds = %helper_crash_cleanup_retire205
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire213:                   ; preds = %helper_crash_cleanup_retire_merge206
  %helper_crash_cleanup_retire_token215 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call216 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token215)
  br i1 %helper_crash_cleanup_retire_call216, label %helper_crash_cleanup_retire_accepted217, label %helper_crash_cleanup_retire_rejected218

helper_crash_cleanup_retire_merge214:             ; preds = %helper_crash_cleanup_retire_accepted217, %helper_crash_cleanup_retire_merge206
  %"hew_vec_free drop219" = load ptr, ptr %local_1, align 4
  call void @hew_vec_free(ptr %"hew_vec_free drop219")
  store ptr null, ptr %local_1, align 4
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire_accepted217:          ; preds = %helper_crash_cleanup_retire213
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge214

helper_crash_cleanup_retire_rejected218:          ; preds = %helper_crash_cleanup_retire213
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_deactivate227:               ; preds = %bb36
  %helper_crash_cleanup_token229 = load i64, ptr %helper_crash_cleanup_token_48, align 8
  %helper_crash_cleanup_deactivate_call230 = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token229)
  br i1 %helper_crash_cleanup_deactivate_call230, label %helper_crash_cleanup_deactivate_accepted231, label %helper_crash_cleanup_deactivate_rejected232

helper_crash_cleanup_deactivate_merge228:         ; preds = %helper_crash_cleanup_deactivate_accepted231, %bb36
  %move_load233 = load ptr, ptr %local_47, align 4
  store ptr %move_load233, ptr %local_48, align 4
  %helper_crash_cleanup_prior_token234 = load i64, ptr %helper_crash_cleanup_token_48, align 8
  %arm_typed_crash_cleanup235 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token234, ptr %local_48, i64 4, i64 4, ptr @__hew_frame_cleanup_5110292a480a33ee, i32 1, i32 0)
  %frame_cleanup_arm_failed236 = icmp eq i64 %arm_typed_crash_cleanup235, -1
  br i1 %frame_cleanup_arm_failed236, label %frame_cleanup_rejected238, label %frame_cleanup_registered237

helper_crash_cleanup_deactivate_accepted231:      ; preds = %helper_crash_cleanup_deactivate227
  store i1 false, ptr %helper_crash_cleanup_active_48, align 1
  br label %helper_crash_cleanup_deactivate_merge228

helper_crash_cleanup_deactivate_rejected232:      ; preds = %helper_crash_cleanup_deactivate227
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

frame_cleanup_registered237:                      ; preds = %helper_crash_cleanup_deactivate_merge228
  store i64 %arm_typed_crash_cleanup235, ptr %helper_crash_cleanup_token_48, align 8
  store i1 true, ptr %helper_crash_cleanup_active_48, align 1
  store i64 42, ptr %local_49, align 8
  %"hew_hashset_insert_layout arg0" = load ptr, ptr %local_48, align 4
  %hew_hashset_insert_layout_call = call i1 @hew_hashset_insert_layout(ptr %"hew_hashset_insert_layout arg0", ptr %local_49)
  %hashset_insert_bool_zext_i8 = zext i1 %hew_hashset_insert_layout_call to i8
  store i8 %hashset_insert_bool_zext_i8, ptr %local_50, align 1
  br label %bb37

frame_cleanup_rejected238:                        ; preds = %helper_crash_cleanup_deactivate_merge228
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_deactivate246:               ; preds = %bb42
  %helper_crash_cleanup_token248 = load i64, ptr %helper_crash_cleanup_token_57, align 8
  %helper_crash_cleanup_deactivate_call249 = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token248)
  br i1 %helper_crash_cleanup_deactivate_call249, label %helper_crash_cleanup_deactivate_accepted250, label %helper_crash_cleanup_deactivate_rejected251

helper_crash_cleanup_deactivate_merge247:         ; preds = %helper_crash_cleanup_deactivate_accepted250, %bb42
  %move_load252 = load ptr, ptr %local_56, align 4
  store ptr %move_load252, ptr %local_57, align 4
  %helper_crash_cleanup_prior_token253 = load i64, ptr %helper_crash_cleanup_token_57, align 8
  %arm_typed_crash_cleanup254 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token253, ptr %local_57, i64 4, i64 4, ptr @__hew_frame_cleanup_6e3157b8b1632579, i32 1, i32 0)
  %frame_cleanup_arm_failed255 = icmp eq i64 %arm_typed_crash_cleanup254, -1
  br i1 %frame_cleanup_arm_failed255, label %frame_cleanup_rejected257, label %frame_cleanup_registered256

helper_crash_cleanup_deactivate_accepted250:      ; preds = %helper_crash_cleanup_deactivate246
  store i1 false, ptr %helper_crash_cleanup_active_57, align 1
  br label %helper_crash_cleanup_deactivate_merge247

helper_crash_cleanup_deactivate_rejected251:      ; preds = %helper_crash_cleanup_deactivate246
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

frame_cleanup_registered256:                      ; preds = %helper_crash_cleanup_deactivate_merge247
  store i64 %arm_typed_crash_cleanup254, ptr %helper_crash_cleanup_token_57, align 8
  store i1 true, ptr %helper_crash_cleanup_active_57, align 1
  store ptr @str_lit.5, ptr %local_58, align 4
  %helper_crash_cleanup_was_active258 = load i1, ptr %helper_crash_cleanup_active_59, align 1
  br i1 %helper_crash_cleanup_was_active258, label %helper_crash_cleanup_deactivate259, label %helper_crash_cleanup_deactivate_merge260

frame_cleanup_rejected257:                        ; preds = %helper_crash_cleanup_deactivate_merge247
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_deactivate259:               ; preds = %frame_cleanup_registered256
  %helper_crash_cleanup_token261 = load i64, ptr %helper_crash_cleanup_token_59, align 8
  %helper_crash_cleanup_deactivate_call262 = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token261)
  br i1 %helper_crash_cleanup_deactivate_call262, label %helper_crash_cleanup_deactivate_accepted263, label %helper_crash_cleanup_deactivate_rejected264

helper_crash_cleanup_deactivate_merge260:         ; preds = %helper_crash_cleanup_deactivate_accepted263, %frame_cleanup_registered256
  %move_load265 = load ptr, ptr %local_58, align 4
  store ptr %move_load265, ptr %local_59, align 4
  %helper_crash_cleanup_prior_token266 = load i64, ptr %helper_crash_cleanup_token_59, align 8
  %arm_typed_crash_cleanup267 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token266, ptr %local_59, i64 4, i64 4, ptr @__hew_frame_cleanup_6e3157b8b1632579, i32 1, i32 0)
  %frame_cleanup_arm_failed268 = icmp eq i64 %arm_typed_crash_cleanup267, -1
  br i1 %frame_cleanup_arm_failed268, label %frame_cleanup_rejected270, label %frame_cleanup_registered269

helper_crash_cleanup_deactivate_accepted263:      ; preds = %helper_crash_cleanup_deactivate259
  store i1 false, ptr %helper_crash_cleanup_active_59, align 1
  br label %helper_crash_cleanup_deactivate_merge260

helper_crash_cleanup_deactivate_rejected264:      ; preds = %helper_crash_cleanup_deactivate259
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

frame_cleanup_registered269:                      ; preds = %helper_crash_cleanup_deactivate_merge260
  store i64 %arm_typed_crash_cleanup267, ptr %helper_crash_cleanup_token_59, align 8
  store i1 true, ptr %helper_crash_cleanup_active_59, align 1
  %"hew_string_concat arg0" = load ptr, ptr %local_57, align 4
  %"hew_string_concat arg1" = load ptr, ptr %local_59, align 4
  %hew_string_concat_call = call ptr @hew_string_concat(ptr %"hew_string_concat arg0", ptr %"hew_string_concat arg1")
  store ptr %hew_string_concat_call, ptr %local_60, align 4
  %helper_crash_cleanup_was_active271 = load i1, ptr %helper_crash_cleanup_active_61, align 1
  br i1 %helper_crash_cleanup_was_active271, label %helper_crash_cleanup_deactivate272, label %helper_crash_cleanup_deactivate_merge273

frame_cleanup_rejected270:                        ; preds = %helper_crash_cleanup_deactivate_merge260
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_deactivate272:               ; preds = %frame_cleanup_registered269
  %helper_crash_cleanup_token274 = load i64, ptr %helper_crash_cleanup_token_61, align 8
  %helper_crash_cleanup_deactivate_call275 = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token274)
  br i1 %helper_crash_cleanup_deactivate_call275, label %helper_crash_cleanup_deactivate_accepted276, label %helper_crash_cleanup_deactivate_rejected277

helper_crash_cleanup_deactivate_merge273:         ; preds = %helper_crash_cleanup_deactivate_accepted276, %frame_cleanup_registered269
  %move_load278 = load ptr, ptr %local_60, align 4
  store ptr %move_load278, ptr %local_61, align 4
  %helper_crash_cleanup_prior_token279 = load i64, ptr %helper_crash_cleanup_token_61, align 8
  %arm_typed_crash_cleanup280 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token279, ptr %local_61, i64 4, i64 4, ptr @__hew_frame_cleanup_6e3157b8b1632579, i32 1, i32 0)
  %frame_cleanup_arm_failed281 = icmp eq i64 %arm_typed_crash_cleanup280, -1
  br i1 %frame_cleanup_arm_failed281, label %frame_cleanup_rejected283, label %frame_cleanup_registered282

helper_crash_cleanup_deactivate_accepted276:      ; preds = %helper_crash_cleanup_deactivate272
  store i1 false, ptr %helper_crash_cleanup_active_61, align 1
  br label %helper_crash_cleanup_deactivate_merge273

helper_crash_cleanup_deactivate_rejected277:      ; preds = %helper_crash_cleanup_deactivate272
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

frame_cleanup_registered282:                      ; preds = %helper_crash_cleanup_deactivate_merge273
  store i64 %arm_typed_crash_cleanup280, ptr %helper_crash_cleanup_token_61, align 8
  store i1 true, ptr %helper_crash_cleanup_active_61, align 1
  %print_arg284 = load ptr, ptr %local_61, align 4
  %print_str_bits = ptrtoint ptr %print_arg284 to i64
  call void @hew_print_value(i8 4, i64 %print_str_bits, i1 true)
  br label %bb43

frame_cleanup_rejected283:                        ; preds = %helper_crash_cleanup_deactivate_merge273
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire289:                   ; preds = %bb45
  %helper_crash_cleanup_retire_token291 = load i64, ptr %helper_crash_cleanup_token_61, align 8
  %helper_crash_cleanup_retire_call292 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token291)
  br i1 %helper_crash_cleanup_retire_call292, label %helper_crash_cleanup_retire_accepted293, label %helper_crash_cleanup_retire_rejected294

helper_crash_cleanup_retire_merge290:             ; preds = %helper_crash_cleanup_retire_accepted293, %bb45
  %"hew_string_drop drop" = load ptr, ptr %local_61, align 4
  call void @hew_string_drop(ptr %"hew_string_drop drop")
  store ptr null, ptr %local_61, align 4
  %helper_crash_cleanup_drop_active295 = load i1, ptr %helper_crash_cleanup_active_59, align 1
  br i1 %helper_crash_cleanup_drop_active295, label %helper_crash_cleanup_retire296, label %helper_crash_cleanup_retire_merge297

helper_crash_cleanup_retire_accepted293:          ; preds = %helper_crash_cleanup_retire289
  store i64 0, ptr %helper_crash_cleanup_token_61, align 8
  store i1 false, ptr %helper_crash_cleanup_active_61, align 1
  br label %helper_crash_cleanup_retire_merge290

helper_crash_cleanup_retire_rejected294:          ; preds = %helper_crash_cleanup_retire289
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire296:                   ; preds = %helper_crash_cleanup_retire_merge290
  %helper_crash_cleanup_retire_token298 = load i64, ptr %helper_crash_cleanup_token_59, align 8
  %helper_crash_cleanup_retire_call299 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token298)
  br i1 %helper_crash_cleanup_retire_call299, label %helper_crash_cleanup_retire_accepted300, label %helper_crash_cleanup_retire_rejected301

helper_crash_cleanup_retire_merge297:             ; preds = %helper_crash_cleanup_retire_accepted300, %helper_crash_cleanup_retire_merge290
  %"hew_string_drop drop302" = load ptr, ptr %local_59, align 4
  call void @hew_string_drop(ptr %"hew_string_drop drop302")
  store ptr null, ptr %local_59, align 4
  %helper_crash_cleanup_drop_active303 = load i1, ptr %helper_crash_cleanup_active_57, align 1
  br i1 %helper_crash_cleanup_drop_active303, label %helper_crash_cleanup_retire304, label %helper_crash_cleanup_retire_merge305

helper_crash_cleanup_retire_accepted300:          ; preds = %helper_crash_cleanup_retire296
  store i64 0, ptr %helper_crash_cleanup_token_59, align 8
  store i1 false, ptr %helper_crash_cleanup_active_59, align 1
  br label %helper_crash_cleanup_retire_merge297

helper_crash_cleanup_retire_rejected301:          ; preds = %helper_crash_cleanup_retire296
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire304:                   ; preds = %helper_crash_cleanup_retire_merge297
  %helper_crash_cleanup_retire_token306 = load i64, ptr %helper_crash_cleanup_token_57, align 8
  %helper_crash_cleanup_retire_call307 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token306)
  br i1 %helper_crash_cleanup_retire_call307, label %helper_crash_cleanup_retire_accepted308, label %helper_crash_cleanup_retire_rejected309

helper_crash_cleanup_retire_merge305:             ; preds = %helper_crash_cleanup_retire_accepted308, %helper_crash_cleanup_retire_merge297
  %"hew_string_drop drop310" = load ptr, ptr %local_57, align 4
  call void @hew_string_drop(ptr %"hew_string_drop drop310")
  store ptr null, ptr %local_57, align 4
  %helper_crash_cleanup_drop_active311 = load i1, ptr %helper_crash_cleanup_active_48, align 1
  br i1 %helper_crash_cleanup_drop_active311, label %helper_crash_cleanup_retire312, label %helper_crash_cleanup_retire_merge313

helper_crash_cleanup_retire_accepted308:          ; preds = %helper_crash_cleanup_retire304
  store i64 0, ptr %helper_crash_cleanup_token_57, align 8
  store i1 false, ptr %helper_crash_cleanup_active_57, align 1
  br label %helper_crash_cleanup_retire_merge305

helper_crash_cleanup_retire_rejected309:          ; preds = %helper_crash_cleanup_retire304
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire312:                   ; preds = %helper_crash_cleanup_retire_merge305
  %helper_crash_cleanup_retire_token314 = load i64, ptr %helper_crash_cleanup_token_48, align 8
  %helper_crash_cleanup_retire_call315 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token314)
  br i1 %helper_crash_cleanup_retire_call315, label %helper_crash_cleanup_retire_accepted316, label %helper_crash_cleanup_retire_rejected317

helper_crash_cleanup_retire_merge313:             ; preds = %helper_crash_cleanup_retire_accepted316, %helper_crash_cleanup_retire_merge305
  %"hew_hashset_free_layout drop" = load ptr, ptr %local_48, align 4
  call void @hew_hashset_free_layout(ptr %"hew_hashset_free_layout drop")
  store ptr null, ptr %local_48, align 4
  %helper_crash_cleanup_drop_active318 = load i1, ptr %helper_crash_cleanup_active_20, align 1
  br i1 %helper_crash_cleanup_drop_active318, label %helper_crash_cleanup_retire319, label %helper_crash_cleanup_retire_merge320

helper_crash_cleanup_retire_accepted316:          ; preds = %helper_crash_cleanup_retire312
  store i64 0, ptr %helper_crash_cleanup_token_48, align 8
  store i1 false, ptr %helper_crash_cleanup_active_48, align 1
  br label %helper_crash_cleanup_retire_merge313

helper_crash_cleanup_retire_rejected317:          ; preds = %helper_crash_cleanup_retire312
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire319:                   ; preds = %helper_crash_cleanup_retire_merge313
  %helper_crash_cleanup_retire_token321 = load i64, ptr %helper_crash_cleanup_token_20, align 8
  %helper_crash_cleanup_retire_call322 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token321)
  br i1 %helper_crash_cleanup_retire_call322, label %helper_crash_cleanup_retire_accepted323, label %helper_crash_cleanup_retire_rejected324

helper_crash_cleanup_retire_merge320:             ; preds = %helper_crash_cleanup_retire_accepted323, %helper_crash_cleanup_retire_merge313
  %"hew_hashmap_free_layout drop325" = load ptr, ptr %local_20, align 4
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop325")
  store ptr null, ptr %local_20, align 4
  %helper_crash_cleanup_drop_active326 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active326, label %helper_crash_cleanup_retire327, label %helper_crash_cleanup_retire_merge328

helper_crash_cleanup_retire_accepted323:          ; preds = %helper_crash_cleanup_retire319
  store i64 0, ptr %helper_crash_cleanup_token_20, align 8
  store i1 false, ptr %helper_crash_cleanup_active_20, align 1
  br label %helper_crash_cleanup_retire_merge320

helper_crash_cleanup_retire_rejected324:          ; preds = %helper_crash_cleanup_retire319
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire327:                   ; preds = %helper_crash_cleanup_retire_merge320
  %helper_crash_cleanup_retire_token329 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call330 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token329)
  br i1 %helper_crash_cleanup_retire_call330, label %helper_crash_cleanup_retire_accepted331, label %helper_crash_cleanup_retire_rejected332

helper_crash_cleanup_retire_merge328:             ; preds = %helper_crash_cleanup_retire_accepted331, %helper_crash_cleanup_retire_merge320
  %"hew_vec_free drop333" = load ptr, ptr %local_1, align 4
  call void @hew_vec_free(ptr %"hew_vec_free drop333")
  store ptr null, ptr %local_1, align 4
  %helper_crash_cleanup_return_token_1 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_return_has_token_1 = icmp ne i64 %helper_crash_cleanup_return_token_1, 0
  br i1 %helper_crash_cleanup_return_has_token_1, label %helper_crash_cleanup_return_retire_1, label %helper_crash_cleanup_return_merge_1

helper_crash_cleanup_retire_accepted331:          ; preds = %helper_crash_cleanup_retire327
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge328

helper_crash_cleanup_retire_rejected332:          ; preds = %helper_crash_cleanup_retire327
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_return_merge_1:              ; preds = %helper_crash_cleanup_return_retire_1_accepted, %helper_crash_cleanup_retire_merge328
  %helper_crash_cleanup_return_token_20 = load i64, ptr %helper_crash_cleanup_token_20, align 8
  %helper_crash_cleanup_return_has_token_20 = icmp ne i64 %helper_crash_cleanup_return_token_20, 0
  br i1 %helper_crash_cleanup_return_has_token_20, label %helper_crash_cleanup_return_retire_20, label %helper_crash_cleanup_return_merge_20

helper_crash_cleanup_return_retire_1:             ; preds = %helper_crash_cleanup_retire_merge328
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

helper_crash_cleanup_return_merge_20:             ; preds = %helper_crash_cleanup_return_retire_20_accepted, %helper_crash_cleanup_return_merge_1
  %helper_crash_cleanup_return_token_48 = load i64, ptr %helper_crash_cleanup_token_48, align 8
  %helper_crash_cleanup_return_has_token_48 = icmp ne i64 %helper_crash_cleanup_return_token_48, 0
  br i1 %helper_crash_cleanup_return_has_token_48, label %helper_crash_cleanup_return_retire_48, label %helper_crash_cleanup_return_merge_48

helper_crash_cleanup_return_retire_20:            ; preds = %helper_crash_cleanup_return_merge_1
  %helper_crash_cleanup_return_retire_20_call = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_return_token_20)
  br i1 %helper_crash_cleanup_return_retire_20_call, label %helper_crash_cleanup_return_retire_20_accepted, label %helper_crash_cleanup_return_retire_20_rejected

helper_crash_cleanup_return_retire_20_accepted:   ; preds = %helper_crash_cleanup_return_retire_20
  store i64 0, ptr %helper_crash_cleanup_token_20, align 8
  store i1 false, ptr %helper_crash_cleanup_active_20, align 1
  br label %helper_crash_cleanup_return_merge_20

helper_crash_cleanup_return_retire_20_rejected:   ; preds = %helper_crash_cleanup_return_retire_20
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_return_merge_48:             ; preds = %helper_crash_cleanup_return_retire_48_accepted, %helper_crash_cleanup_return_merge_20
  %helper_crash_cleanup_return_token_57 = load i64, ptr %helper_crash_cleanup_token_57, align 8
  %helper_crash_cleanup_return_has_token_57 = icmp ne i64 %helper_crash_cleanup_return_token_57, 0
  br i1 %helper_crash_cleanup_return_has_token_57, label %helper_crash_cleanup_return_retire_57, label %helper_crash_cleanup_return_merge_57

helper_crash_cleanup_return_retire_48:            ; preds = %helper_crash_cleanup_return_merge_20
  %helper_crash_cleanup_return_retire_48_call = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_return_token_48)
  br i1 %helper_crash_cleanup_return_retire_48_call, label %helper_crash_cleanup_return_retire_48_accepted, label %helper_crash_cleanup_return_retire_48_rejected

helper_crash_cleanup_return_retire_48_accepted:   ; preds = %helper_crash_cleanup_return_retire_48
  store i64 0, ptr %helper_crash_cleanup_token_48, align 8
  store i1 false, ptr %helper_crash_cleanup_active_48, align 1
  br label %helper_crash_cleanup_return_merge_48

helper_crash_cleanup_return_retire_48_rejected:   ; preds = %helper_crash_cleanup_return_retire_48
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_return_merge_57:             ; preds = %helper_crash_cleanup_return_retire_57_accepted, %helper_crash_cleanup_return_merge_48
  %helper_crash_cleanup_return_token_59 = load i64, ptr %helper_crash_cleanup_token_59, align 8
  %helper_crash_cleanup_return_has_token_59 = icmp ne i64 %helper_crash_cleanup_return_token_59, 0
  br i1 %helper_crash_cleanup_return_has_token_59, label %helper_crash_cleanup_return_retire_59, label %helper_crash_cleanup_return_merge_59

helper_crash_cleanup_return_retire_57:            ; preds = %helper_crash_cleanup_return_merge_48
  %helper_crash_cleanup_return_retire_57_call = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_return_token_57)
  br i1 %helper_crash_cleanup_return_retire_57_call, label %helper_crash_cleanup_return_retire_57_accepted, label %helper_crash_cleanup_return_retire_57_rejected

helper_crash_cleanup_return_retire_57_accepted:   ; preds = %helper_crash_cleanup_return_retire_57
  store i64 0, ptr %helper_crash_cleanup_token_57, align 8
  store i1 false, ptr %helper_crash_cleanup_active_57, align 1
  br label %helper_crash_cleanup_return_merge_57

helper_crash_cleanup_return_retire_57_rejected:   ; preds = %helper_crash_cleanup_return_retire_57
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_return_merge_59:             ; preds = %helper_crash_cleanup_return_retire_59_accepted, %helper_crash_cleanup_return_merge_57
  %helper_crash_cleanup_return_token_61 = load i64, ptr %helper_crash_cleanup_token_61, align 8
  %helper_crash_cleanup_return_has_token_61 = icmp ne i64 %helper_crash_cleanup_return_token_61, 0
  br i1 %helper_crash_cleanup_return_has_token_61, label %helper_crash_cleanup_return_retire_61, label %helper_crash_cleanup_return_merge_61

helper_crash_cleanup_return_retire_59:            ; preds = %helper_crash_cleanup_return_merge_57
  %helper_crash_cleanup_return_retire_59_call = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_return_token_59)
  br i1 %helper_crash_cleanup_return_retire_59_call, label %helper_crash_cleanup_return_retire_59_accepted, label %helper_crash_cleanup_return_retire_59_rejected

helper_crash_cleanup_return_retire_59_accepted:   ; preds = %helper_crash_cleanup_return_retire_59
  store i64 0, ptr %helper_crash_cleanup_token_59, align 8
  store i1 false, ptr %helper_crash_cleanup_active_59, align 1
  br label %helper_crash_cleanup_return_merge_59

helper_crash_cleanup_return_retire_59_rejected:   ; preds = %helper_crash_cleanup_return_retire_59
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_return_merge_61:             ; preds = %helper_crash_cleanup_return_retire_61_accepted, %helper_crash_cleanup_return_merge_59
  ret i8 0

helper_crash_cleanup_return_retire_61:            ; preds = %helper_crash_cleanup_return_merge_59
  %helper_crash_cleanup_return_retire_61_call = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_return_token_61)
  br i1 %helper_crash_cleanup_return_retire_61_call, label %helper_crash_cleanup_return_retire_61_accepted, label %helper_crash_cleanup_return_retire_61_rejected

helper_crash_cleanup_return_retire_61_accepted:   ; preds = %helper_crash_cleanup_return_retire_61
  store i64 0, ptr %helper_crash_cleanup_token_61, align 8
  store i1 false, ptr %helper_crash_cleanup_active_61, align 1
  br label %helper_crash_cleanup_return_merge_61

helper_crash_cleanup_return_retire_61_rejected:   ; preds = %helper_crash_cleanup_return_retire_61
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

define i32 @__hew_wasi_main() {
entry:
  %hew_source_main_call = call i8 @__original_main()
  ret i32 0
}

declare i32 @hew_actor_cooperate()

declare ptr @hew_vec_new_i64()

declare i1 @hew_cont_crash_cleanup_deactivate(i64)

declare void @hew_trap_with_code(i32)

; Function Attrs: cold noreturn nounwind memory(inaccessiblemem: write)
declare void @llvm.trap() #0

define internal void @__hew_frame_cleanup_7fdeddf79806b8a4(ptr %0) {
entry:
  %"hew_vec_free drop" = load ptr, ptr %0, align 4
  call void @hew_vec_free(ptr %"hew_vec_free drop")
  store ptr null, ptr %0, align 4
  ret void
}

declare void @hew_vec_free(ptr)

declare i64 @hew_cont_crash_cleanup_arm(i64, ptr, i64, i64, ptr, i32, i32)

declare i1 @hew_vec_get_clone(ptr, i64, ptr)

declare i1 @hew_cont_crash_cleanup_retire(i64)

declare ptr @hew_hashmap_new_with_layout(ptr, ptr)

define internal void @__hew_frame_cleanup_01796e325d15e37f(ptr %0) {
entry:
  %"hew_hashmap_free_layout drop" = load ptr, ptr %0, align 4
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop")
  store ptr null, ptr %0, align 4
  ret void
}

declare void @hew_hashmap_free_layout(ptr)

declare i1 @hew_hashmap_insert_layout(ptr, ptr, ptr)

declare void @hew_string_drop(ptr)

declare i64 @hew_hashmap_len_layout(ptr)

declare i1 @hew_hashmap_get_clone_layout(ptr, ptr, ptr)

declare i1 @hew_hashmap_remove_take_layout(ptr, ptr, ptr)

declare ptr @hew_hashset_new_with_layout(ptr)

define internal void @__hew_frame_cleanup_5110292a480a33ee(ptr %0) {
entry:
  %"hew_hashset_free_layout drop" = load ptr, ptr %0, align 4
  call void @hew_hashset_free_layout(ptr %"hew_hashset_free_layout drop")
  store ptr null, ptr %0, align 4
  ret void
}

declare void @hew_hashset_free_layout(ptr)

declare i1 @hew_hashset_insert_layout(ptr, ptr)

declare i64 @hew_hashset_len_layout(ptr)

declare i1 @hew_hashset_contains_layout(ptr, ptr)

define internal void @__hew_frame_cleanup_6e3157b8b1632579(ptr %0) {
entry:
  %"hew_string_drop drop" = load ptr, ptr %0, align 4
  call void @hew_string_drop(ptr %"hew_string_drop drop")
  store ptr null, ptr %0, align 4
  ret void
}

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.smul.with.overflow.i64(i64, i64) #1

attributes #0 = { cold noreturn nounwind memory(inaccessiblemem: write) }
attributes #1 = { nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none) }
