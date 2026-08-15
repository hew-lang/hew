; ModuleID = 'r4_runtime_abi'
source_filename = "r4_runtime_abi"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "aarch64-apple-macosx13.0"

%"Option$$i64" = type { i8, [1 x i64] }

@hew_layout_key_string = external constant i8
@str_lit = private unnamed_addr constant [4 x i8] c"col\00", align 1
@str_lit.1 = private unnamed_addr constant [4 x i8] c"our\00", align 1
@str_lit.2 = private unnamed_addr constant [5 x i8] c"flav\00", align 1
@str_lit.3 = private unnamed_addr constant [4 x i8] c"our\00", align 1
@hew_layout_val_i64 = external constant i8
@str_lit.4 = private unnamed_addr constant [2 x i8] c"a\00", align 1
@str_lit.5 = private unnamed_addr constant [2 x i8] c"b\00", align 1
@str_lit.6 = private unnamed_addr constant [2 x i8] c"a\00", align 1
@str_lit.7 = private unnamed_addr constant [2 x i8] c"b\00", align 1
@hew_layout_key_i64 = external constant i8
@str_lit.8 = private unnamed_addr constant [6 x i8] c"hello\00", align 1
@str_lit.9 = private unnamed_addr constant [7 x i8] c" world\00", align 1
@str_lit.10 = private unnamed_addr constant [3 x i8] c"ns\00", align 1

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

define internal i8 @add_member(ptr %0, ptr %1) {
entry:
  %return_slot = alloca i8, align 1
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca i8, align 1
  store ptr %0, ptr %local_0, align 8
  store ptr %1, ptr %local_1, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %mir_share_string_load = load ptr, ptr %local_1, align 8
  %mir_share_string_retain = call ptr @hew_string_clone(ptr %mir_share_string_load)
  %"hew_hashset_insert_layout arg0" = load ptr, ptr %local_0, align 8
  %hew_hashset_insert_layout_call = call i1 @hew_hashset_insert_layout(ptr %"hew_hashset_insert_layout arg0", ptr %local_1)
  %hashset_insert_bool_zext_i8 = zext i1 %hew_hashset_insert_layout_call to i8
  store i8 %hashset_insert_bool_zext_i8, ptr %local_2, align 1
  %insert_existed = icmp eq i1 %hew_hashset_insert_layout_call, false
  br i1 %insert_existed, label %insert_overwrite_key_release, label %insert_overwrite_key_cont

bb1:                                              ; preds = %insert_overwrite_key_cont
  ret i8 0

cancel_exit:                                      ; preds = %entry
  ret i8 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

insert_overwrite_key_release:                     ; preds = %bb0
  %"hew_hashset_insert_layout overwrite key" = load ptr, ptr %local_1, align 8
  call void @hew_string_drop(ptr %"hew_hashset_insert_layout overwrite key")
  br label %insert_overwrite_key_cont

insert_overwrite_key_cont:                        ; preds = %insert_overwrite_key_release, %bb0
  br label %bb1
}

define internal i8 @add_renamed(ptr %0, ptr %1) {
entry:
  %return_slot = alloca i8, align 1
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca ptr, align 8
  %local_3 = alloca i8, align 1
  store ptr %0, ptr %local_0, align 8
  store ptr %1, ptr %local_1, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_1, align 8
  %call_result = call ptr @hew_string_to_uppercase(ptr %call_arg)
  store ptr %call_result, ptr %local_2, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_2, align 8
  store ptr %move_load, ptr %local_1, align 8
  %"hew_hashset_insert_layout arg0" = load ptr, ptr %local_0, align 8
  %hew_hashset_insert_layout_call = call i1 @hew_hashset_insert_layout(ptr %"hew_hashset_insert_layout arg0", ptr %local_1)
  %hashset_insert_bool_zext_i8 = zext i1 %hew_hashset_insert_layout_call to i8
  store i8 %hashset_insert_bool_zext_i8, ptr %local_3, align 1
  %insert_existed = icmp eq i1 %hew_hashset_insert_layout_call, false
  br i1 %insert_existed, label %insert_overwrite_key_release, label %insert_overwrite_key_cont

bb2:                                              ; preds = %insert_overwrite_key_cont
  ret i8 0

cancel_exit:                                      ; preds = %entry
  ret i8 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

insert_overwrite_key_release:                     ; preds = %bb1
  %"hew_hashset_insert_layout overwrite key" = load ptr, ptr %local_1, align 8
  call void @hew_string_drop(ptr %"hew_hashset_insert_layout overwrite key")
  br label %insert_overwrite_key_cont

insert_overwrite_key_cont:                        ; preds = %insert_overwrite_key_release, %bb1
  br label %bb2
}

define i8 @main() {
entry:
  %return_slot = alloca i8, align 1
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca ptr, align 8
  %local_3 = alloca ptr, align 8
  %local_4 = alloca ptr, align 8
  %local_5 = alloca ptr, align 8
  %local_6 = alloca ptr, align 8
  %local_7 = alloca ptr, align 8
  %local_8 = alloca i64, align 8
  %local_9 = alloca ptr, align 8
  %local_10 = alloca ptr, align 8
  %local_11 = alloca i64, align 8
  %local_12 = alloca i64, align 8
  %local_13 = alloca i64, align 8
  %local_14 = alloca i64, align 8
  %local_15 = alloca i64, align 8
  %local_16 = alloca %"Option$$i64", align 8
  %local_17 = alloca %"Option$$i64", align 8
  %local_18 = alloca i64, align 8
  %local_19 = alloca i64, align 8
  %local_20 = alloca i64, align 8
  %local_21 = alloca i8, align 1
  %local_22 = alloca i64, align 8
  %local_23 = alloca i8, align 1
  %local_24 = alloca i64, align 8
  %local_25 = alloca i64, align 8
  %local_26 = alloca i64, align 8
  %local_27 = alloca i64, align 8
  %local_28 = alloca ptr, align 8
  %local_29 = alloca ptr, align 8
  %local_30 = alloca ptr, align 8
  %local_31 = alloca i64, align 8
  %local_32 = alloca ptr, align 8
  %local_33 = alloca i64, align 8
  %local_34 = alloca i64, align 8
  %local_35 = alloca i8, align 1
  %local_36 = alloca ptr, align 8
  %local_37 = alloca %"Option$$i64", align 8
  %local_38 = alloca i64, align 8
  %local_39 = alloca i64, align 8
  %local_40 = alloca i8, align 1
  %local_41 = alloca i64, align 8
  %local_42 = alloca i8, align 1
  %local_43 = alloca i64, align 8
  %local_44 = alloca i64, align 8
  %local_45 = alloca i8, align 1
  %local_46 = alloca ptr, align 8
  %local_47 = alloca %"Option$$i64", align 8
  %local_48 = alloca i64, align 8
  %local_49 = alloca i64, align 8
  %local_50 = alloca i8, align 1
  %local_51 = alloca i64, align 8
  %local_52 = alloca i8, align 1
  %local_53 = alloca i8, align 1
  %local_54 = alloca i8, align 1
  %local_55 = alloca i8, align 1
  %local_56 = alloca ptr, align 8
  %local_57 = alloca ptr, align 8
  %local_58 = alloca i64, align 8
  %local_59 = alloca i8, align 1
  %local_60 = alloca i64, align 8
  %local_61 = alloca i8, align 1
  %local_62 = alloca i64, align 8
  %local_63 = alloca i64, align 8
  %local_64 = alloca i8, align 1
  %local_65 = alloca ptr, align 8
  %local_66 = alloca ptr, align 8
  %local_67 = alloca ptr, align 8
  %local_68 = alloca ptr, align 8
  %local_69 = alloca ptr, align 8
  %local_70 = alloca ptr, align 8
  %local_71 = alloca i64, align 8
  %helper_crash_cleanup_token_1 = alloca i64, align 8
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_active_1 = alloca i1, align 1
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  %helper_crash_cleanup_token_4 = alloca i64, align 8
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_active_4 = alloca i1, align 1
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  %helper_crash_cleanup_token_7 = alloca i64, align 8
  store i64 0, ptr %helper_crash_cleanup_token_7, align 8
  %helper_crash_cleanup_active_7 = alloca i1, align 1
  store i1 false, ptr %helper_crash_cleanup_active_7, align 1
  %helper_crash_cleanup_token_10 = alloca i64, align 8
  store i64 0, ptr %helper_crash_cleanup_token_10, align 8
  %helper_crash_cleanup_active_10 = alloca i1, align 1
  store i1 false, ptr %helper_crash_cleanup_active_10, align 1
  %helper_crash_cleanup_token_29 = alloca i64, align 8
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_active_29 = alloca i1, align 1
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  %helper_crash_cleanup_token_57 = alloca i64, align 8
  store i64 0, ptr %helper_crash_cleanup_token_57, align 8
  %helper_crash_cleanup_active_57 = alloca i1, align 1
  store i1 false, ptr %helper_crash_cleanup_active_57, align 1
  %helper_crash_cleanup_token_66 = alloca i64, align 8
  store i64 0, ptr %helper_crash_cleanup_token_66, align 8
  %helper_crash_cleanup_active_66 = alloca i1, align 1
  store i1 false, ptr %helper_crash_cleanup_active_66, align 1
  %helper_crash_cleanup_token_68 = alloca i64, align 8
  store i64 0, ptr %helper_crash_cleanup_token_68, align 8
  %helper_crash_cleanup_active_68 = alloca i1, align 1
  store i1 false, ptr %helper_crash_cleanup_active_68, align 1
  %helper_crash_cleanup_token_70 = alloca i64, align 8
  store i64 0, ptr %helper_crash_cleanup_token_70, align 8
  %helper_crash_cleanup_active_70 = alloca i1, align 1
  store i1 false, ptr %helper_crash_cleanup_active_70, align 1
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %hew_hashset_new_with_layout_call = call ptr @hew_hashset_new_with_layout(ptr @hew_layout_key_string)
  store ptr %hew_hashset_new_with_layout_call, ptr %local_0, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %helper_crash_cleanup_was_active = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_was_active, label %helper_crash_cleanup_deactivate, label %helper_crash_cleanup_deactivate_merge

bb2:                                              ; preds = %frame_cleanup_registered11
  store ptr @str_lit.2, ptr %local_5, align 8
  store ptr @str_lit.3, ptr %local_6, align 8
  %helper_crash_cleanup_was_active14 = load i1, ptr %helper_crash_cleanup_active_7, align 1
  br i1 %helper_crash_cleanup_was_active14, label %helper_crash_cleanup_deactivate15, label %helper_crash_cleanup_deactivate_merge16

bb3:                                              ; preds = %frame_cleanup_registered27
  %"hew_hashset_len_layout arg0" = load ptr, ptr %local_1, align 8
  %hew_hashset_len_layout_call = call i64 @hew_hashset_len_layout(ptr %"hew_hashset_len_layout arg0")
  store i64 %hew_hashset_len_layout_call, ptr %local_8, align 8
  br label %bb4

bb4:                                              ; preds = %bb3
  %print_arg = load i64, ptr %local_8, align 8
  call void @hew_print_value(i8 1, i64 %print_arg, i1 true)
  br label %bb5

bb5:                                              ; preds = %bb4
  %hew_vec_new_i64_call = call ptr @hew_vec_new_i64()
  store ptr %hew_vec_new_i64_call, ptr %local_9, align 8
  br label %bb6

bb6:                                              ; preds = %bb5
  %helper_crash_cleanup_was_active32 = load i1, ptr %helper_crash_cleanup_active_10, align 1
  br i1 %helper_crash_cleanup_was_active32, label %helper_crash_cleanup_deactivate33, label %helper_crash_cleanup_deactivate_merge34

bb7:                                              ; preds = %frame_cleanup_registered43
  store i64 2, ptr %local_12, align 8
  %call_arg47 = load ptr, ptr %local_10, align 8
  %call_arg48 = load i64, ptr %local_12, align 8
  call void @hew_vec_push_i64(ptr %call_arg47, i64 %call_arg48)
  br label %bb8

bb8:                                              ; preds = %bb7
  store i64 3, ptr %local_13, align 8
  %call_arg49 = load ptr, ptr %local_10, align 8
  %call_arg50 = load i64, ptr %local_13, align 8
  call void @hew_vec_push_i64(ptr %call_arg49, i64 %call_arg50)
  br label %bb9

bb9:                                              ; preds = %bb8
  %"hew_vec_len arg0" = load ptr, ptr %local_10, align 8
  %hew_vec_len_call = call i64 @hew_vec_len(ptr %"hew_vec_len arg0")
  store i64 %hew_vec_len_call, ptr %local_14, align 8
  br label %bb10

bb10:                                             ; preds = %bb9
  %print_arg51 = load i64, ptr %local_14, align 8
  call void @hew_print_value(i8 1, i64 %print_arg51, i1 true)
  br label %bb11

bb11:                                             ; preds = %bb10
  store i64 0, ptr %local_15, align 8
  %"hew_vec_get_clone arg0" = load ptr, ptr %local_10, align 8
  %"hew_vec_get_clone index" = load i64, ptr %local_15, align 8
  %machine_payload_ptr = getelementptr inbounds nuw %"Option$$i64", ptr %local_16, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr, i32 0, i32 0
  %hew_vec_get_clone_call = call i1 @hew_vec_get_clone(ptr %"hew_vec_get_clone arg0", i64 %"hew_vec_get_clone index", ptr %machine_variant_field_ptr)
  br i1 %hew_vec_get_clone_call, label %vec_get_some, label %vec_get_none

bb12:                                             ; preds = %vec_get_initialized
  %move_load53 = load %"Option$$i64", ptr %local_16, align 8
  store %"Option$$i64" %move_load53, ptr %local_17, align 8
  %machine_tag_ptr54 = getelementptr inbounds nuw %"Option$$i64", ptr %local_17, i32 0, i32 0
  %move_iN_load = load i8, ptr %machine_tag_ptr54, align 1
  %move_iN_zext = zext i8 %move_iN_load to i64
  store i64 %move_iN_zext, ptr %local_19, align 8
  store i64 0, ptr %local_20, align 8
  %cmp_lhs = load i64, ptr %local_19, align 8
  %cmp_rhs = load i64, ptr %local_20, align 8
  %cmp_bit = icmp eq i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_21, align 1
  %cond_load = load i8, ptr %local_21, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb14, label %bb17

bb13:                                             ; preds = %after_cooperate90, %after_cooperate63
  %print_arg55 = load i64, ptr %local_18, align 8
  call void @hew_print_value(i8 1, i64 %print_arg55, i1 true)
  br label %bb18

bb14:                                             ; preds = %bb12
  %machine_payload_ptr56 = getelementptr inbounds nuw %"Option$$i64", ptr %local_17, i32 0, i32 1
  %machine_variant_field_ptr57 = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr56, i32 0, i32 0
  %move_load58 = load i64, ptr %machine_variant_field_ptr57, align 8
  store i64 %move_load58, ptr %local_24, align 8
  %move_load59 = load i64, ptr %local_24, align 8
  store i64 %move_load59, ptr %local_18, align 8
  %hew_actor_cooperate60 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel61 = icmp eq i32 %hew_actor_cooperate60, 2
  br i1 %hew_cooperate_is_cancel61, label %cancel_exit62, label %after_cooperate63

bb15:                                             ; preds = %bb17
  store i64 -1, ptr %local_25, align 8
  %move_load86 = load i64, ptr %local_25, align 8
  store i64 %move_load86, ptr %local_18, align 8
  %hew_actor_cooperate87 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel88 = icmp eq i32 %hew_actor_cooperate87, 2
  br i1 %hew_cooperate_is_cancel88, label %cancel_exit89, label %after_cooperate90

bb16:                                             ; preds = %bb17
  %helper_crash_cleanup_drop_active123 = load i1, ptr %helper_crash_cleanup_active_10, align 1
  br i1 %helper_crash_cleanup_drop_active123, label %helper_crash_cleanup_retire124, label %helper_crash_cleanup_retire_merge125

bb17:                                             ; preds = %bb12
  store i64 1, ptr %local_22, align 8
  %cmp_lhs155 = load i64, ptr %local_19, align 8
  %cmp_rhs156 = load i64, ptr %local_22, align 8
  %cmp_bit157 = icmp eq i64 %cmp_lhs155, %cmp_rhs156
  %cmp_zext158 = zext i1 %cmp_bit157 to i8
  store i8 %cmp_zext158, ptr %local_23, align 1
  %cond_load159 = load i8, ptr %local_23, align 1
  %cond_nz160 = icmp ne i8 %cond_load159, 0
  br i1 %cond_nz160, label %bb15, label %bb16

bb18:                                             ; preds = %bb13
  %call_arg161 = load ptr, ptr %local_10, align 8
  %call_result162 = call i64 @hew_vec_pop_i64(ptr %call_arg161)
  store i64 %call_result162, ptr %local_26, align 8
  br label %bb19

bb19:                                             ; preds = %bb18
  %move_load163 = load i64, ptr %local_26, align 8
  store i64 %move_load163, ptr %local_27, align 8
  %print_arg164 = load i64, ptr %local_27, align 8
  call void @hew_print_value(i8 1, i64 %print_arg164, i1 true)
  br label %bb20

bb20:                                             ; preds = %bb19
  %hew_hashmap_new_with_layout_call = call ptr @hew_hashmap_new_with_layout(ptr @hew_layout_key_string, ptr @hew_layout_val_i64)
  store ptr %hew_hashmap_new_with_layout_call, ptr %local_28, align 8
  br label %bb21

bb21:                                             ; preds = %bb20
  %helper_crash_cleanup_was_active165 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_was_active165, label %helper_crash_cleanup_deactivate166, label %helper_crash_cleanup_deactivate_merge167

bb22:                                             ; preds = %insert_overwrite_key_cont
  store ptr @str_lit.5, ptr %local_32, align 8
  store i64 20, ptr %local_33, align 8
  %"hew_hashmap_insert_layout arg0178" = load ptr, ptr %local_29, align 8
  %hew_hashmap_insert_layout_call179 = call i1 @hew_hashmap_insert_layout(ptr %"hew_hashmap_insert_layout arg0178", ptr %local_32, ptr %local_33)
  %insert_existed182 = icmp eq i1 %hew_hashmap_insert_layout_call179, false
  br i1 %insert_existed182, label %insert_overwrite_key_release180, label %insert_overwrite_key_cont181

bb23:                                             ; preds = %insert_overwrite_key_cont181
  %"hew_hashmap_len_layout arg0" = load ptr, ptr %local_29, align 8
  %hew_hashmap_len_layout_call = call i64 @hew_hashmap_len_layout(ptr %"hew_hashmap_len_layout arg0")
  store i64 %hew_hashmap_len_layout_call, ptr %local_34, align 8
  br label %bb24

bb24:                                             ; preds = %bb23
  %print_arg184 = load i64, ptr %local_34, align 8
  call void @hew_print_value(i8 1, i64 %print_arg184, i1 true)
  br label %bb25

bb25:                                             ; preds = %bb24
  store ptr @str_lit.6, ptr %local_36, align 8
  %"hew_hashmap_get_layout arg0" = load ptr, ptr %local_29, align 8
  %machine_payload_ptr185 = getelementptr inbounds nuw %"Option$$i64", ptr %local_37, i32 0, i32 1
  %machine_variant_field_ptr186 = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr185, i32 0, i32 0
  %hew_hashmap_get_clone_layout_call = call i1 @hew_hashmap_get_clone_layout(ptr %"hew_hashmap_get_layout arg0", ptr %local_36, ptr %machine_variant_field_ptr186)
  br i1 %hew_hashmap_get_clone_layout_call, label %hashmap_get_some, label %hashmap_get_none

bb26:                                             ; preds = %hashmap_get_initialized
  %machine_tag_ptr189 = getelementptr inbounds nuw %"Option$$i64", ptr %local_37, i32 0, i32 0
  %move_iN_load190 = load i8, ptr %machine_tag_ptr189, align 1
  %move_iN_zext191 = zext i8 %move_iN_load190 to i64
  store i64 %move_iN_zext191, ptr %local_38, align 8
  store i64 0, ptr %local_39, align 8
  %cmp_lhs192 = load i64, ptr %local_38, align 8
  %cmp_rhs193 = load i64, ptr %local_39, align 8
  %cmp_bit194 = icmp eq i64 %cmp_lhs192, %cmp_rhs193
  %cmp_zext195 = zext i1 %cmp_bit194 to i8
  store i8 %cmp_zext195, ptr %local_40, align 1
  %cond_load196 = load i8, ptr %local_40, align 1
  %cond_nz197 = icmp ne i8 %cond_load196, 0
  br i1 %cond_nz197, label %bb28, label %bb31

bb27:                                             ; preds = %after_cooperate299, %after_cooperate255
  store ptr @str_lit.7, ptr %local_46, align 8
  %"hew_hashmap_remove_take_layout arg0" = load ptr, ptr %local_29, align 8
  %machine_payload_ptr198 = getelementptr inbounds nuw %"Option$$i64", ptr %local_47, i32 0, i32 1
  %machine_variant_field_ptr199 = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr198, i32 0, i32 0
  %hew_hashmap_remove_take_layout_call = call i1 @hew_hashmap_remove_take_layout(ptr %"hew_hashmap_remove_take_layout arg0", ptr %local_46, ptr %machine_variant_field_ptr199)
  br i1 %hew_hashmap_remove_take_layout_call, label %hashmap_remove_some, label %hashmap_remove_none

bb28:                                             ; preds = %bb26
  %machine_payload_ptr202 = getelementptr inbounds nuw %"Option$$i64", ptr %local_37, i32 0, i32 1
  %machine_variant_field_ptr203 = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr202, i32 0, i32 0
  %move_load204 = load i64, ptr %machine_variant_field_ptr203, align 8
  store i64 %move_load204, ptr %local_43, align 8
  %print_arg205 = load i64, ptr %local_43, align 8
  call void @hew_print_value(i8 1, i64 %print_arg205, i1 true)
  br label %bb32

bb29:                                             ; preds = %bb31
  store i64 -1, ptr %local_44, align 8
  %print_arg206 = load i64, ptr %local_44, align 8
  call void @hew_print_value(i8 1, i64 %print_arg206, i1 true)
  br label %bb33

bb30:                                             ; preds = %bb31
  %helper_crash_cleanup_drop_active207 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_drop_active207, label %helper_crash_cleanup_retire208, label %helper_crash_cleanup_retire_merge209

bb31:                                             ; preds = %bb26
  store i64 1, ptr %local_41, align 8
  %cmp_lhs246 = load i64, ptr %local_38, align 8
  %cmp_rhs247 = load i64, ptr %local_41, align 8
  %cmp_bit248 = icmp eq i64 %cmp_lhs246, %cmp_rhs247
  %cmp_zext249 = zext i1 %cmp_bit248 to i8
  store i8 %cmp_zext249, ptr %local_42, align 1
  %cond_load250 = load i8, ptr %local_42, align 1
  %cond_nz251 = icmp ne i8 %cond_load250, 0
  br i1 %cond_nz251, label %bb29, label %bb30

bb32:                                             ; preds = %bb28
  %hew_actor_cooperate252 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel253 = icmp eq i32 %hew_actor_cooperate252, 2
  br i1 %hew_cooperate_is_cancel253, label %cancel_exit254, label %after_cooperate255

bb33:                                             ; preds = %bb29
  %hew_actor_cooperate296 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel297 = icmp eq i32 %hew_actor_cooperate296, 2
  br i1 %hew_cooperate_is_cancel297, label %cancel_exit298, label %after_cooperate299

bb34:                                             ; preds = %hashmap_remove_initialized
  %machine_tag_ptr340 = getelementptr inbounds nuw %"Option$$i64", ptr %local_47, i32 0, i32 0
  %move_iN_load341 = load i8, ptr %machine_tag_ptr340, align 1
  %move_iN_zext342 = zext i8 %move_iN_load341 to i64
  store i64 %move_iN_zext342, ptr %local_48, align 8
  store i64 0, ptr %local_49, align 8
  %cmp_lhs343 = load i64, ptr %local_48, align 8
  %cmp_rhs344 = load i64, ptr %local_49, align 8
  %cmp_bit345 = icmp eq i64 %cmp_lhs343, %cmp_rhs344
  %cmp_zext346 = zext i1 %cmp_bit345 to i8
  store i8 %cmp_zext346, ptr %local_50, align 1
  %cond_load347 = load i8, ptr %local_50, align 1
  %cond_nz348 = icmp ne i8 %cond_load347, 0
  br i1 %cond_nz348, label %bb36, label %bb39

bb35:                                             ; preds = %after_cooperate400, %after_cooperate355
  %move_load349 = load i8, ptr %local_45, align 1
  store i8 %move_load349, ptr %local_55, align 1
  %print_arg350 = load i8, ptr %local_55, align 1
  %print_narrow_bits = zext i8 %print_arg350 to i64
  call void @hew_print_value(i8 3, i64 %print_narrow_bits, i1 true)
  br label %bb40

bb36:                                             ; preds = %bb34
  store i8 1, ptr %local_53, align 1
  %move_load351 = load i8, ptr %local_53, align 1
  store i8 %move_load351, ptr %local_45, align 1
  %hew_actor_cooperate352 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel353 = icmp eq i32 %hew_actor_cooperate352, 2
  br i1 %hew_cooperate_is_cancel353, label %cancel_exit354, label %after_cooperate355

bb37:                                             ; preds = %bb39
  store i8 0, ptr %local_54, align 1
  %move_load396 = load i8, ptr %local_54, align 1
  store i8 %move_load396, ptr %local_45, align 1
  %hew_actor_cooperate397 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel398 = icmp eq i32 %hew_actor_cooperate397, 2
  br i1 %hew_cooperate_is_cancel398, label %cancel_exit399, label %after_cooperate400

bb38:                                             ; preds = %bb39
  %helper_crash_cleanup_drop_active441 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_drop_active441, label %helper_crash_cleanup_retire442, label %helper_crash_cleanup_retire_merge443

bb39:                                             ; preds = %bb34
  store i64 1, ptr %local_51, align 8
  %cmp_lhs481 = load i64, ptr %local_48, align 8
  %cmp_rhs482 = load i64, ptr %local_51, align 8
  %cmp_bit483 = icmp eq i64 %cmp_lhs481, %cmp_rhs482
  %cmp_zext484 = zext i1 %cmp_bit483 to i8
  store i8 %cmp_zext484, ptr %local_52, align 1
  %cond_load485 = load i8, ptr %local_52, align 1
  %cond_nz486 = icmp ne i8 %cond_load485, 0
  br i1 %cond_nz486, label %bb37, label %bb38

bb40:                                             ; preds = %bb35
  %hew_hashset_new_with_layout_call487 = call ptr @hew_hashset_new_with_layout(ptr @hew_layout_key_i64)
  store ptr %hew_hashset_new_with_layout_call487, ptr %local_56, align 8
  br label %bb41

bb41:                                             ; preds = %bb40
  %helper_crash_cleanup_was_active488 = load i1, ptr %helper_crash_cleanup_active_57, align 1
  br i1 %helper_crash_cleanup_was_active488, label %helper_crash_cleanup_deactivate489, label %helper_crash_cleanup_deactivate_merge490

bb42:                                             ; preds = %frame_cleanup_registered499
  store i64 7, ptr %local_60, align 8
  %"hew_hashset_insert_layout arg0501" = load ptr, ptr %local_57, align 8
  %hew_hashset_insert_layout_call502 = call i1 @hew_hashset_insert_layout(ptr %"hew_hashset_insert_layout arg0501", ptr %local_60)
  %hashset_insert_bool_zext_i8503 = zext i1 %hew_hashset_insert_layout_call502 to i8
  store i8 %hashset_insert_bool_zext_i8503, ptr %local_61, align 1
  br label %bb43

bb43:                                             ; preds = %bb42
  %"hew_hashset_len_layout arg0504" = load ptr, ptr %local_57, align 8
  %hew_hashset_len_layout_call505 = call i64 @hew_hashset_len_layout(ptr %"hew_hashset_len_layout arg0504")
  store i64 %hew_hashset_len_layout_call505, ptr %local_62, align 8
  br label %bb44

bb44:                                             ; preds = %bb43
  %print_arg506 = load i64, ptr %local_62, align 8
  call void @hew_print_value(i8 1, i64 %print_arg506, i1 true)
  br label %bb45

bb45:                                             ; preds = %bb44
  store i64 42, ptr %local_63, align 8
  %"hew_hashset_contains_layout arg0" = load ptr, ptr %local_57, align 8
  %hew_hashset_contains_layout_call = call i1 @hew_hashset_contains_layout(ptr %"hew_hashset_contains_layout arg0", ptr %local_63)
  %hashset_contains_bool_zext_i8 = zext i1 %hew_hashset_contains_layout_call to i8
  store i8 %hashset_contains_bool_zext_i8, ptr %local_64, align 1
  br label %bb46

bb46:                                             ; preds = %bb45
  %print_arg507 = load i8, ptr %local_64, align 1
  %print_narrow_bits508 = zext i8 %print_arg507 to i64
  call void @hew_print_value(i8 3, i64 %print_narrow_bits508, i1 true)
  br label %bb47

bb47:                                             ; preds = %bb46
  store ptr @str_lit.8, ptr %local_65, align 8
  %helper_crash_cleanup_was_active509 = load i1, ptr %helper_crash_cleanup_active_66, align 1
  br i1 %helper_crash_cleanup_was_active509, label %helper_crash_cleanup_deactivate510, label %helper_crash_cleanup_deactivate_merge511

bb48:                                             ; preds = %frame_cleanup_registered549
  %call_arg552 = load ptr, ptr %local_70, align 8
  %call_result553 = call i32 @hew_string_length(ptr %call_arg552)
  %ffi_sext = sext i32 %call_result553 to i64
  store i64 %ffi_sext, ptr %local_71, align 8
  br label %bb49

bb49:                                             ; preds = %bb48
  %print_arg554 = load i64, ptr %local_71, align 8
  call void @hew_print_value(i8 1, i64 %print_arg554, i1 true)
  br label %bb50

bb50:                                             ; preds = %bb49
  %helper_crash_cleanup_drop_active555 = load i1, ptr %helper_crash_cleanup_active_70, align 1
  br i1 %helper_crash_cleanup_drop_active555, label %helper_crash_cleanup_retire556, label %helper_crash_cleanup_retire_merge557

cancel_exit:                                      ; preds = %entry
  ret i8 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

helper_crash_cleanup_deactivate:                  ; preds = %bb1
  %helper_crash_cleanup_token = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_deactivate_call = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token)
  br i1 %helper_crash_cleanup_deactivate_call, label %helper_crash_cleanup_deactivate_accepted, label %helper_crash_cleanup_deactivate_rejected

helper_crash_cleanup_deactivate_merge:            ; preds = %helper_crash_cleanup_deactivate_accepted, %bb1
  %move_load = load ptr, ptr %local_0, align 8
  store ptr %move_load, ptr %local_1, align 8
  %helper_crash_cleanup_prior_token = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %arm_typed_crash_cleanup = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token, ptr %local_1, i64 8, i64 8, ptr @__hew_frame_cleanup_b53461ca9fc57f84, i32 1, i32 0)
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
  store ptr @str_lit, ptr %local_2, align 8
  store ptr @str_lit.1, ptr %local_3, align 8
  %helper_crash_cleanup_was_active1 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_was_active1, label %helper_crash_cleanup_deactivate2, label %helper_crash_cleanup_deactivate_merge3

frame_cleanup_rejected:                           ; preds = %helper_crash_cleanup_deactivate_merge
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_deactivate2:                 ; preds = %frame_cleanup_registered
  %helper_crash_cleanup_token4 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_deactivate_call5 = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token4)
  br i1 %helper_crash_cleanup_deactivate_call5, label %helper_crash_cleanup_deactivate_accepted6, label %helper_crash_cleanup_deactivate_rejected7

helper_crash_cleanup_deactivate_merge3:           ; preds = %helper_crash_cleanup_deactivate_accepted6, %frame_cleanup_registered
  %"hew_string_concat arg0" = load ptr, ptr %local_2, align 8
  %"hew_string_concat arg1" = load ptr, ptr %local_3, align 8
  %hew_string_concat_call = call ptr @hew_string_concat(ptr %"hew_string_concat arg0", ptr %"hew_string_concat arg1")
  store ptr %hew_string_concat_call, ptr %local_4, align 8
  %helper_crash_cleanup_prior_token8 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %arm_typed_crash_cleanup9 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token8, ptr %local_4, i64 8, i64 8, ptr @__hew_frame_cleanup_6e3157b8b1632579, i32 1, i32 0)
  %frame_cleanup_arm_failed10 = icmp eq i64 %arm_typed_crash_cleanup9, -1
  br i1 %frame_cleanup_arm_failed10, label %frame_cleanup_rejected12, label %frame_cleanup_registered11

helper_crash_cleanup_deactivate_accepted6:        ; preds = %helper_crash_cleanup_deactivate2
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_deactivate_merge3

helper_crash_cleanup_deactivate_rejected7:        ; preds = %helper_crash_cleanup_deactivate2
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

frame_cleanup_registered11:                       ; preds = %helper_crash_cleanup_deactivate_merge3
  store i64 %arm_typed_crash_cleanup9, ptr %helper_crash_cleanup_token_4, align 8
  store i1 true, ptr %helper_crash_cleanup_active_4, align 1
  %call_arg = load ptr, ptr %local_1, align 8
  %call_arg13 = load ptr, ptr %local_4, align 8
  %call_result = call i8 @add_member(ptr %call_arg, ptr %call_arg13)
  br label %bb2

frame_cleanup_rejected12:                         ; preds = %helper_crash_cleanup_deactivate_merge3
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_deactivate15:                ; preds = %bb2
  %helper_crash_cleanup_token17 = load i64, ptr %helper_crash_cleanup_token_7, align 8
  %helper_crash_cleanup_deactivate_call18 = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token17)
  br i1 %helper_crash_cleanup_deactivate_call18, label %helper_crash_cleanup_deactivate_accepted19, label %helper_crash_cleanup_deactivate_rejected20

helper_crash_cleanup_deactivate_merge16:          ; preds = %helper_crash_cleanup_deactivate_accepted19, %bb2
  %"hew_string_concat arg021" = load ptr, ptr %local_5, align 8
  %"hew_string_concat arg122" = load ptr, ptr %local_6, align 8
  %hew_string_concat_call23 = call ptr @hew_string_concat(ptr %"hew_string_concat arg021", ptr %"hew_string_concat arg122")
  store ptr %hew_string_concat_call23, ptr %local_7, align 8
  %helper_crash_cleanup_prior_token24 = load i64, ptr %helper_crash_cleanup_token_7, align 8
  %arm_typed_crash_cleanup25 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token24, ptr %local_7, i64 8, i64 8, ptr @__hew_frame_cleanup_6e3157b8b1632579, i32 1, i32 0)
  %frame_cleanup_arm_failed26 = icmp eq i64 %arm_typed_crash_cleanup25, -1
  br i1 %frame_cleanup_arm_failed26, label %frame_cleanup_rejected28, label %frame_cleanup_registered27

helper_crash_cleanup_deactivate_accepted19:       ; preds = %helper_crash_cleanup_deactivate15
  store i1 false, ptr %helper_crash_cleanup_active_7, align 1
  br label %helper_crash_cleanup_deactivate_merge16

helper_crash_cleanup_deactivate_rejected20:       ; preds = %helper_crash_cleanup_deactivate15
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

frame_cleanup_registered27:                       ; preds = %helper_crash_cleanup_deactivate_merge16
  store i64 %arm_typed_crash_cleanup25, ptr %helper_crash_cleanup_token_7, align 8
  store i1 true, ptr %helper_crash_cleanup_active_7, align 1
  %call_arg29 = load ptr, ptr %local_1, align 8
  %call_arg30 = load ptr, ptr %local_7, align 8
  %call_result31 = call i8 @add_renamed(ptr %call_arg29, ptr %call_arg30)
  br label %bb3

frame_cleanup_rejected28:                         ; preds = %helper_crash_cleanup_deactivate_merge16
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_deactivate33:                ; preds = %bb6
  %helper_crash_cleanup_token35 = load i64, ptr %helper_crash_cleanup_token_10, align 8
  %helper_crash_cleanup_deactivate_call36 = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token35)
  br i1 %helper_crash_cleanup_deactivate_call36, label %helper_crash_cleanup_deactivate_accepted37, label %helper_crash_cleanup_deactivate_rejected38

helper_crash_cleanup_deactivate_merge34:          ; preds = %helper_crash_cleanup_deactivate_accepted37, %bb6
  %move_load39 = load ptr, ptr %local_9, align 8
  store ptr %move_load39, ptr %local_10, align 8
  %helper_crash_cleanup_prior_token40 = load i64, ptr %helper_crash_cleanup_token_10, align 8
  %arm_typed_crash_cleanup41 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token40, ptr %local_10, i64 8, i64 8, ptr @__hew_frame_cleanup_7fdeddf79806b8a4, i32 1, i32 0)
  %frame_cleanup_arm_failed42 = icmp eq i64 %arm_typed_crash_cleanup41, -1
  br i1 %frame_cleanup_arm_failed42, label %frame_cleanup_rejected44, label %frame_cleanup_registered43

helper_crash_cleanup_deactivate_accepted37:       ; preds = %helper_crash_cleanup_deactivate33
  store i1 false, ptr %helper_crash_cleanup_active_10, align 1
  br label %helper_crash_cleanup_deactivate_merge34

helper_crash_cleanup_deactivate_rejected38:       ; preds = %helper_crash_cleanup_deactivate33
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

frame_cleanup_registered43:                       ; preds = %helper_crash_cleanup_deactivate_merge34
  store i64 %arm_typed_crash_cleanup41, ptr %helper_crash_cleanup_token_10, align 8
  store i1 true, ptr %helper_crash_cleanup_active_10, align 1
  store i64 1, ptr %local_11, align 8
  %call_arg45 = load ptr, ptr %local_10, align 8
  %call_arg46 = load i64, ptr %local_11, align 8
  call void @hew_vec_push_i64(ptr %call_arg45, i64 %call_arg46)
  br label %bb7

frame_cleanup_rejected44:                         ; preds = %helper_crash_cleanup_deactivate_merge34
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

vec_get_none:                                     ; preds = %bb11
  %machine_tag_ptr = getelementptr inbounds nuw %"Option$$i64", ptr %local_16, i32 0, i32 0
  store i8 1, ptr %machine_tag_ptr, align 1
  br label %vec_get_initialized

vec_get_some:                                     ; preds = %bb11
  %machine_tag_ptr52 = getelementptr inbounds nuw %"Option$$i64", ptr %local_16, i32 0, i32 0
  store i8 0, ptr %machine_tag_ptr52, align 1
  br label %vec_get_initialized

vec_get_initialized:                              ; preds = %vec_get_some, %vec_get_none
  br label %bb12

cancel_exit62:                                    ; preds = %bb14
  %helper_crash_cleanup_drop_active = load i1, ptr %helper_crash_cleanup_active_10, align 1
  br i1 %helper_crash_cleanup_drop_active, label %helper_crash_cleanup_retire, label %helper_crash_cleanup_retire_merge

after_cooperate63:                                ; preds = %bb14
  br label %bb13

helper_crash_cleanup_retire:                      ; preds = %cancel_exit62
  %helper_crash_cleanup_retire_token = load i64, ptr %helper_crash_cleanup_token_10, align 8
  %helper_crash_cleanup_retire_call = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token)
  br i1 %helper_crash_cleanup_retire_call, label %helper_crash_cleanup_retire_accepted, label %helper_crash_cleanup_retire_rejected

helper_crash_cleanup_retire_merge:                ; preds = %helper_crash_cleanup_retire_accepted, %cancel_exit62
  %"hew_vec_free drop" = load ptr, ptr %local_10, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop")
  store ptr null, ptr %local_10, align 8
  %helper_crash_cleanup_drop_active64 = load i1, ptr %helper_crash_cleanup_active_7, align 1
  br i1 %helper_crash_cleanup_drop_active64, label %helper_crash_cleanup_retire65, label %helper_crash_cleanup_retire_merge66

helper_crash_cleanup_retire_accepted:             ; preds = %helper_crash_cleanup_retire
  store i64 0, ptr %helper_crash_cleanup_token_10, align 8
  store i1 false, ptr %helper_crash_cleanup_active_10, align 1
  br label %helper_crash_cleanup_retire_merge

helper_crash_cleanup_retire_rejected:             ; preds = %helper_crash_cleanup_retire
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire65:                    ; preds = %helper_crash_cleanup_retire_merge
  %helper_crash_cleanup_retire_token67 = load i64, ptr %helper_crash_cleanup_token_7, align 8
  %helper_crash_cleanup_retire_call68 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token67)
  br i1 %helper_crash_cleanup_retire_call68, label %helper_crash_cleanup_retire_accepted69, label %helper_crash_cleanup_retire_rejected70

helper_crash_cleanup_retire_merge66:              ; preds = %helper_crash_cleanup_retire_accepted69, %helper_crash_cleanup_retire_merge
  %"hew_string_drop drop" = load ptr, ptr %local_7, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop")
  store ptr null, ptr %local_7, align 8
  %helper_crash_cleanup_drop_active71 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active71, label %helper_crash_cleanup_retire72, label %helper_crash_cleanup_retire_merge73

helper_crash_cleanup_retire_accepted69:           ; preds = %helper_crash_cleanup_retire65
  store i64 0, ptr %helper_crash_cleanup_token_7, align 8
  store i1 false, ptr %helper_crash_cleanup_active_7, align 1
  br label %helper_crash_cleanup_retire_merge66

helper_crash_cleanup_retire_rejected70:           ; preds = %helper_crash_cleanup_retire65
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire72:                    ; preds = %helper_crash_cleanup_retire_merge66
  %helper_crash_cleanup_retire_token74 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call75 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token74)
  br i1 %helper_crash_cleanup_retire_call75, label %helper_crash_cleanup_retire_accepted76, label %helper_crash_cleanup_retire_rejected77

helper_crash_cleanup_retire_merge73:              ; preds = %helper_crash_cleanup_retire_accepted76, %helper_crash_cleanup_retire_merge66
  %"hew_string_drop drop78" = load ptr, ptr %local_4, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop78")
  store ptr null, ptr %local_4, align 8
  %helper_crash_cleanup_drop_active79 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active79, label %helper_crash_cleanup_retire80, label %helper_crash_cleanup_retire_merge81

helper_crash_cleanup_retire_accepted76:           ; preds = %helper_crash_cleanup_retire72
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge73

helper_crash_cleanup_retire_rejected77:           ; preds = %helper_crash_cleanup_retire72
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire80:                    ; preds = %helper_crash_cleanup_retire_merge73
  %helper_crash_cleanup_retire_token82 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call83 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token82)
  br i1 %helper_crash_cleanup_retire_call83, label %helper_crash_cleanup_retire_accepted84, label %helper_crash_cleanup_retire_rejected85

helper_crash_cleanup_retire_merge81:              ; preds = %helper_crash_cleanup_retire_accepted84, %helper_crash_cleanup_retire_merge73
  %"hew_hashset_free_layout drop" = load ptr, ptr %local_1, align 8
  call void @hew_hashset_free_layout(ptr %"hew_hashset_free_layout drop")
  store ptr null, ptr %local_1, align 8
  ret i8 0

helper_crash_cleanup_retire_accepted84:           ; preds = %helper_crash_cleanup_retire80
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge81

helper_crash_cleanup_retire_rejected85:           ; preds = %helper_crash_cleanup_retire80
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

cancel_exit89:                                    ; preds = %bb15
  %helper_crash_cleanup_drop_active91 = load i1, ptr %helper_crash_cleanup_active_10, align 1
  br i1 %helper_crash_cleanup_drop_active91, label %helper_crash_cleanup_retire92, label %helper_crash_cleanup_retire_merge93

after_cooperate90:                                ; preds = %bb15
  br label %bb13

helper_crash_cleanup_retire92:                    ; preds = %cancel_exit89
  %helper_crash_cleanup_retire_token94 = load i64, ptr %helper_crash_cleanup_token_10, align 8
  %helper_crash_cleanup_retire_call95 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token94)
  br i1 %helper_crash_cleanup_retire_call95, label %helper_crash_cleanup_retire_accepted96, label %helper_crash_cleanup_retire_rejected97

helper_crash_cleanup_retire_merge93:              ; preds = %helper_crash_cleanup_retire_accepted96, %cancel_exit89
  %"hew_vec_free drop98" = load ptr, ptr %local_10, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop98")
  store ptr null, ptr %local_10, align 8
  %helper_crash_cleanup_drop_active99 = load i1, ptr %helper_crash_cleanup_active_7, align 1
  br i1 %helper_crash_cleanup_drop_active99, label %helper_crash_cleanup_retire100, label %helper_crash_cleanup_retire_merge101

helper_crash_cleanup_retire_accepted96:           ; preds = %helper_crash_cleanup_retire92
  store i64 0, ptr %helper_crash_cleanup_token_10, align 8
  store i1 false, ptr %helper_crash_cleanup_active_10, align 1
  br label %helper_crash_cleanup_retire_merge93

helper_crash_cleanup_retire_rejected97:           ; preds = %helper_crash_cleanup_retire92
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire100:                   ; preds = %helper_crash_cleanup_retire_merge93
  %helper_crash_cleanup_retire_token102 = load i64, ptr %helper_crash_cleanup_token_7, align 8
  %helper_crash_cleanup_retire_call103 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token102)
  br i1 %helper_crash_cleanup_retire_call103, label %helper_crash_cleanup_retire_accepted104, label %helper_crash_cleanup_retire_rejected105

helper_crash_cleanup_retire_merge101:             ; preds = %helper_crash_cleanup_retire_accepted104, %helper_crash_cleanup_retire_merge93
  %"hew_string_drop drop106" = load ptr, ptr %local_7, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop106")
  store ptr null, ptr %local_7, align 8
  %helper_crash_cleanup_drop_active107 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active107, label %helper_crash_cleanup_retire108, label %helper_crash_cleanup_retire_merge109

helper_crash_cleanup_retire_accepted104:          ; preds = %helper_crash_cleanup_retire100
  store i64 0, ptr %helper_crash_cleanup_token_7, align 8
  store i1 false, ptr %helper_crash_cleanup_active_7, align 1
  br label %helper_crash_cleanup_retire_merge101

helper_crash_cleanup_retire_rejected105:          ; preds = %helper_crash_cleanup_retire100
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire108:                   ; preds = %helper_crash_cleanup_retire_merge101
  %helper_crash_cleanup_retire_token110 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call111 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token110)
  br i1 %helper_crash_cleanup_retire_call111, label %helper_crash_cleanup_retire_accepted112, label %helper_crash_cleanup_retire_rejected113

helper_crash_cleanup_retire_merge109:             ; preds = %helper_crash_cleanup_retire_accepted112, %helper_crash_cleanup_retire_merge101
  %"hew_string_drop drop114" = load ptr, ptr %local_4, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop114")
  store ptr null, ptr %local_4, align 8
  %helper_crash_cleanup_drop_active115 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active115, label %helper_crash_cleanup_retire116, label %helper_crash_cleanup_retire_merge117

helper_crash_cleanup_retire_accepted112:          ; preds = %helper_crash_cleanup_retire108
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge109

helper_crash_cleanup_retire_rejected113:          ; preds = %helper_crash_cleanup_retire108
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire116:                   ; preds = %helper_crash_cleanup_retire_merge109
  %helper_crash_cleanup_retire_token118 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call119 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token118)
  br i1 %helper_crash_cleanup_retire_call119, label %helper_crash_cleanup_retire_accepted120, label %helper_crash_cleanup_retire_rejected121

helper_crash_cleanup_retire_merge117:             ; preds = %helper_crash_cleanup_retire_accepted120, %helper_crash_cleanup_retire_merge109
  %"hew_hashset_free_layout drop122" = load ptr, ptr %local_1, align 8
  call void @hew_hashset_free_layout(ptr %"hew_hashset_free_layout drop122")
  store ptr null, ptr %local_1, align 8
  ret i8 0

helper_crash_cleanup_retire_accepted120:          ; preds = %helper_crash_cleanup_retire116
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge117

helper_crash_cleanup_retire_rejected121:          ; preds = %helper_crash_cleanup_retire116
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire124:                   ; preds = %bb16
  %helper_crash_cleanup_retire_token126 = load i64, ptr %helper_crash_cleanup_token_10, align 8
  %helper_crash_cleanup_retire_call127 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token126)
  br i1 %helper_crash_cleanup_retire_call127, label %helper_crash_cleanup_retire_accepted128, label %helper_crash_cleanup_retire_rejected129

helper_crash_cleanup_retire_merge125:             ; preds = %helper_crash_cleanup_retire_accepted128, %bb16
  %"hew_vec_free drop130" = load ptr, ptr %local_10, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop130")
  store ptr null, ptr %local_10, align 8
  %helper_crash_cleanup_drop_active131 = load i1, ptr %helper_crash_cleanup_active_7, align 1
  br i1 %helper_crash_cleanup_drop_active131, label %helper_crash_cleanup_retire132, label %helper_crash_cleanup_retire_merge133

helper_crash_cleanup_retire_accepted128:          ; preds = %helper_crash_cleanup_retire124
  store i64 0, ptr %helper_crash_cleanup_token_10, align 8
  store i1 false, ptr %helper_crash_cleanup_active_10, align 1
  br label %helper_crash_cleanup_retire_merge125

helper_crash_cleanup_retire_rejected129:          ; preds = %helper_crash_cleanup_retire124
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire132:                   ; preds = %helper_crash_cleanup_retire_merge125
  %helper_crash_cleanup_retire_token134 = load i64, ptr %helper_crash_cleanup_token_7, align 8
  %helper_crash_cleanup_retire_call135 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token134)
  br i1 %helper_crash_cleanup_retire_call135, label %helper_crash_cleanup_retire_accepted136, label %helper_crash_cleanup_retire_rejected137

helper_crash_cleanup_retire_merge133:             ; preds = %helper_crash_cleanup_retire_accepted136, %helper_crash_cleanup_retire_merge125
  %"hew_string_drop drop138" = load ptr, ptr %local_7, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop138")
  store ptr null, ptr %local_7, align 8
  %helper_crash_cleanup_drop_active139 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active139, label %helper_crash_cleanup_retire140, label %helper_crash_cleanup_retire_merge141

helper_crash_cleanup_retire_accepted136:          ; preds = %helper_crash_cleanup_retire132
  store i64 0, ptr %helper_crash_cleanup_token_7, align 8
  store i1 false, ptr %helper_crash_cleanup_active_7, align 1
  br label %helper_crash_cleanup_retire_merge133

helper_crash_cleanup_retire_rejected137:          ; preds = %helper_crash_cleanup_retire132
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire140:                   ; preds = %helper_crash_cleanup_retire_merge133
  %helper_crash_cleanup_retire_token142 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call143 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token142)
  br i1 %helper_crash_cleanup_retire_call143, label %helper_crash_cleanup_retire_accepted144, label %helper_crash_cleanup_retire_rejected145

helper_crash_cleanup_retire_merge141:             ; preds = %helper_crash_cleanup_retire_accepted144, %helper_crash_cleanup_retire_merge133
  %"hew_string_drop drop146" = load ptr, ptr %local_4, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop146")
  store ptr null, ptr %local_4, align 8
  %helper_crash_cleanup_drop_active147 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active147, label %helper_crash_cleanup_retire148, label %helper_crash_cleanup_retire_merge149

helper_crash_cleanup_retire_accepted144:          ; preds = %helper_crash_cleanup_retire140
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge141

helper_crash_cleanup_retire_rejected145:          ; preds = %helper_crash_cleanup_retire140
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire148:                   ; preds = %helper_crash_cleanup_retire_merge141
  %helper_crash_cleanup_retire_token150 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call151 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token150)
  br i1 %helper_crash_cleanup_retire_call151, label %helper_crash_cleanup_retire_accepted152, label %helper_crash_cleanup_retire_rejected153

helper_crash_cleanup_retire_merge149:             ; preds = %helper_crash_cleanup_retire_accepted152, %helper_crash_cleanup_retire_merge141
  %"hew_hashset_free_layout drop154" = load ptr, ptr %local_1, align 8
  call void @hew_hashset_free_layout(ptr %"hew_hashset_free_layout drop154")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire_accepted152:          ; preds = %helper_crash_cleanup_retire148
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge149

helper_crash_cleanup_retire_rejected153:          ; preds = %helper_crash_cleanup_retire148
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_deactivate166:               ; preds = %bb21
  %helper_crash_cleanup_token168 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_deactivate_call169 = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token168)
  br i1 %helper_crash_cleanup_deactivate_call169, label %helper_crash_cleanup_deactivate_accepted170, label %helper_crash_cleanup_deactivate_rejected171

helper_crash_cleanup_deactivate_merge167:         ; preds = %helper_crash_cleanup_deactivate_accepted170, %bb21
  %move_load172 = load ptr, ptr %local_28, align 8
  store ptr %move_load172, ptr %local_29, align 8
  %helper_crash_cleanup_prior_token173 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %arm_typed_crash_cleanup174 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token173, ptr %local_29, i64 8, i64 8, ptr @__hew_frame_cleanup_01796e325d15e37f, i32 1, i32 0)
  %frame_cleanup_arm_failed175 = icmp eq i64 %arm_typed_crash_cleanup174, -1
  br i1 %frame_cleanup_arm_failed175, label %frame_cleanup_rejected177, label %frame_cleanup_registered176

helper_crash_cleanup_deactivate_accepted170:      ; preds = %helper_crash_cleanup_deactivate166
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_deactivate_merge167

helper_crash_cleanup_deactivate_rejected171:      ; preds = %helper_crash_cleanup_deactivate166
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

frame_cleanup_registered176:                      ; preds = %helper_crash_cleanup_deactivate_merge167
  store i64 %arm_typed_crash_cleanup174, ptr %helper_crash_cleanup_token_29, align 8
  store i1 true, ptr %helper_crash_cleanup_active_29, align 1
  store ptr @str_lit.4, ptr %local_30, align 8
  store i64 10, ptr %local_31, align 8
  %"hew_hashmap_insert_layout arg0" = load ptr, ptr %local_29, align 8
  %hew_hashmap_insert_layout_call = call i1 @hew_hashmap_insert_layout(ptr %"hew_hashmap_insert_layout arg0", ptr %local_30, ptr %local_31)
  %insert_existed = icmp eq i1 %hew_hashmap_insert_layout_call, false
  br i1 %insert_existed, label %insert_overwrite_key_release, label %insert_overwrite_key_cont

frame_cleanup_rejected177:                        ; preds = %helper_crash_cleanup_deactivate_merge167
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

insert_overwrite_key_release:                     ; preds = %frame_cleanup_registered176
  %"hew_hashmap_insert_layout overwrite key" = load ptr, ptr %local_30, align 8
  call void @hew_string_drop(ptr %"hew_hashmap_insert_layout overwrite key")
  br label %insert_overwrite_key_cont

insert_overwrite_key_cont:                        ; preds = %insert_overwrite_key_release, %frame_cleanup_registered176
  br label %bb22

insert_overwrite_key_release180:                  ; preds = %bb22
  %"hew_hashmap_insert_layout overwrite key183" = load ptr, ptr %local_32, align 8
  call void @hew_string_drop(ptr %"hew_hashmap_insert_layout overwrite key183")
  br label %insert_overwrite_key_cont181

insert_overwrite_key_cont181:                     ; preds = %insert_overwrite_key_release180, %bb22
  br label %bb23

hashmap_get_none:                                 ; preds = %bb25
  %machine_tag_ptr187 = getelementptr inbounds nuw %"Option$$i64", ptr %local_37, i32 0, i32 0
  store i8 1, ptr %machine_tag_ptr187, align 1
  br label %hashmap_get_initialized

hashmap_get_some:                                 ; preds = %bb25
  %machine_tag_ptr188 = getelementptr inbounds nuw %"Option$$i64", ptr %local_37, i32 0, i32 0
  store i8 0, ptr %machine_tag_ptr188, align 1
  br label %hashmap_get_initialized

hashmap_get_initialized:                          ; preds = %hashmap_get_some, %hashmap_get_none
  br label %bb26

hashmap_remove_none:                              ; preds = %bb27
  %machine_tag_ptr200 = getelementptr inbounds nuw %"Option$$i64", ptr %local_47, i32 0, i32 0
  store i8 1, ptr %machine_tag_ptr200, align 1
  br label %hashmap_remove_initialized

hashmap_remove_some:                              ; preds = %bb27
  %machine_tag_ptr201 = getelementptr inbounds nuw %"Option$$i64", ptr %local_47, i32 0, i32 0
  store i8 0, ptr %machine_tag_ptr201, align 1
  br label %hashmap_remove_initialized

hashmap_remove_initialized:                       ; preds = %hashmap_remove_some, %hashmap_remove_none
  br label %bb34

helper_crash_cleanup_retire208:                   ; preds = %bb30
  %helper_crash_cleanup_retire_token210 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_retire_call211 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token210)
  br i1 %helper_crash_cleanup_retire_call211, label %helper_crash_cleanup_retire_accepted212, label %helper_crash_cleanup_retire_rejected213

helper_crash_cleanup_retire_merge209:             ; preds = %helper_crash_cleanup_retire_accepted212, %bb30
  %"hew_hashmap_free_layout drop" = load ptr, ptr %local_29, align 8
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop")
  store ptr null, ptr %local_29, align 8
  %helper_crash_cleanup_drop_active214 = load i1, ptr %helper_crash_cleanup_active_10, align 1
  br i1 %helper_crash_cleanup_drop_active214, label %helper_crash_cleanup_retire215, label %helper_crash_cleanup_retire_merge216

helper_crash_cleanup_retire_accepted212:          ; preds = %helper_crash_cleanup_retire208
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_retire_merge209

helper_crash_cleanup_retire_rejected213:          ; preds = %helper_crash_cleanup_retire208
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire215:                   ; preds = %helper_crash_cleanup_retire_merge209
  %helper_crash_cleanup_retire_token217 = load i64, ptr %helper_crash_cleanup_token_10, align 8
  %helper_crash_cleanup_retire_call218 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token217)
  br i1 %helper_crash_cleanup_retire_call218, label %helper_crash_cleanup_retire_accepted219, label %helper_crash_cleanup_retire_rejected220

helper_crash_cleanup_retire_merge216:             ; preds = %helper_crash_cleanup_retire_accepted219, %helper_crash_cleanup_retire_merge209
  %"hew_vec_free drop221" = load ptr, ptr %local_10, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop221")
  store ptr null, ptr %local_10, align 8
  %helper_crash_cleanup_drop_active222 = load i1, ptr %helper_crash_cleanup_active_7, align 1
  br i1 %helper_crash_cleanup_drop_active222, label %helper_crash_cleanup_retire223, label %helper_crash_cleanup_retire_merge224

helper_crash_cleanup_retire_accepted219:          ; preds = %helper_crash_cleanup_retire215
  store i64 0, ptr %helper_crash_cleanup_token_10, align 8
  store i1 false, ptr %helper_crash_cleanup_active_10, align 1
  br label %helper_crash_cleanup_retire_merge216

helper_crash_cleanup_retire_rejected220:          ; preds = %helper_crash_cleanup_retire215
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire223:                   ; preds = %helper_crash_cleanup_retire_merge216
  %helper_crash_cleanup_retire_token225 = load i64, ptr %helper_crash_cleanup_token_7, align 8
  %helper_crash_cleanup_retire_call226 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token225)
  br i1 %helper_crash_cleanup_retire_call226, label %helper_crash_cleanup_retire_accepted227, label %helper_crash_cleanup_retire_rejected228

helper_crash_cleanup_retire_merge224:             ; preds = %helper_crash_cleanup_retire_accepted227, %helper_crash_cleanup_retire_merge216
  %"hew_string_drop drop229" = load ptr, ptr %local_7, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop229")
  store ptr null, ptr %local_7, align 8
  %helper_crash_cleanup_drop_active230 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active230, label %helper_crash_cleanup_retire231, label %helper_crash_cleanup_retire_merge232

helper_crash_cleanup_retire_accepted227:          ; preds = %helper_crash_cleanup_retire223
  store i64 0, ptr %helper_crash_cleanup_token_7, align 8
  store i1 false, ptr %helper_crash_cleanup_active_7, align 1
  br label %helper_crash_cleanup_retire_merge224

helper_crash_cleanup_retire_rejected228:          ; preds = %helper_crash_cleanup_retire223
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire231:                   ; preds = %helper_crash_cleanup_retire_merge224
  %helper_crash_cleanup_retire_token233 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call234 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token233)
  br i1 %helper_crash_cleanup_retire_call234, label %helper_crash_cleanup_retire_accepted235, label %helper_crash_cleanup_retire_rejected236

helper_crash_cleanup_retire_merge232:             ; preds = %helper_crash_cleanup_retire_accepted235, %helper_crash_cleanup_retire_merge224
  %"hew_string_drop drop237" = load ptr, ptr %local_4, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop237")
  store ptr null, ptr %local_4, align 8
  %helper_crash_cleanup_drop_active238 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active238, label %helper_crash_cleanup_retire239, label %helper_crash_cleanup_retire_merge240

helper_crash_cleanup_retire_accepted235:          ; preds = %helper_crash_cleanup_retire231
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge232

helper_crash_cleanup_retire_rejected236:          ; preds = %helper_crash_cleanup_retire231
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire239:                   ; preds = %helper_crash_cleanup_retire_merge232
  %helper_crash_cleanup_retire_token241 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call242 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token241)
  br i1 %helper_crash_cleanup_retire_call242, label %helper_crash_cleanup_retire_accepted243, label %helper_crash_cleanup_retire_rejected244

helper_crash_cleanup_retire_merge240:             ; preds = %helper_crash_cleanup_retire_accepted243, %helper_crash_cleanup_retire_merge232
  %"hew_hashset_free_layout drop245" = load ptr, ptr %local_1, align 8
  call void @hew_hashset_free_layout(ptr %"hew_hashset_free_layout drop245")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire_accepted243:          ; preds = %helper_crash_cleanup_retire239
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge240

helper_crash_cleanup_retire_rejected244:          ; preds = %helper_crash_cleanup_retire239
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

cancel_exit254:                                   ; preds = %bb32
  %helper_crash_cleanup_drop_active256 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_drop_active256, label %helper_crash_cleanup_retire257, label %helper_crash_cleanup_retire_merge258

after_cooperate255:                               ; preds = %bb32
  br label %bb27

helper_crash_cleanup_retire257:                   ; preds = %cancel_exit254
  %helper_crash_cleanup_retire_token259 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_retire_call260 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token259)
  br i1 %helper_crash_cleanup_retire_call260, label %helper_crash_cleanup_retire_accepted261, label %helper_crash_cleanup_retire_rejected262

helper_crash_cleanup_retire_merge258:             ; preds = %helper_crash_cleanup_retire_accepted261, %cancel_exit254
  %"hew_hashmap_free_layout drop263" = load ptr, ptr %local_29, align 8
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop263")
  store ptr null, ptr %local_29, align 8
  %helper_crash_cleanup_drop_active264 = load i1, ptr %helper_crash_cleanup_active_10, align 1
  br i1 %helper_crash_cleanup_drop_active264, label %helper_crash_cleanup_retire265, label %helper_crash_cleanup_retire_merge266

helper_crash_cleanup_retire_accepted261:          ; preds = %helper_crash_cleanup_retire257
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_retire_merge258

helper_crash_cleanup_retire_rejected262:          ; preds = %helper_crash_cleanup_retire257
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire265:                   ; preds = %helper_crash_cleanup_retire_merge258
  %helper_crash_cleanup_retire_token267 = load i64, ptr %helper_crash_cleanup_token_10, align 8
  %helper_crash_cleanup_retire_call268 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token267)
  br i1 %helper_crash_cleanup_retire_call268, label %helper_crash_cleanup_retire_accepted269, label %helper_crash_cleanup_retire_rejected270

helper_crash_cleanup_retire_merge266:             ; preds = %helper_crash_cleanup_retire_accepted269, %helper_crash_cleanup_retire_merge258
  %"hew_vec_free drop271" = load ptr, ptr %local_10, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop271")
  store ptr null, ptr %local_10, align 8
  %helper_crash_cleanup_drop_active272 = load i1, ptr %helper_crash_cleanup_active_7, align 1
  br i1 %helper_crash_cleanup_drop_active272, label %helper_crash_cleanup_retire273, label %helper_crash_cleanup_retire_merge274

helper_crash_cleanup_retire_accepted269:          ; preds = %helper_crash_cleanup_retire265
  store i64 0, ptr %helper_crash_cleanup_token_10, align 8
  store i1 false, ptr %helper_crash_cleanup_active_10, align 1
  br label %helper_crash_cleanup_retire_merge266

helper_crash_cleanup_retire_rejected270:          ; preds = %helper_crash_cleanup_retire265
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire273:                   ; preds = %helper_crash_cleanup_retire_merge266
  %helper_crash_cleanup_retire_token275 = load i64, ptr %helper_crash_cleanup_token_7, align 8
  %helper_crash_cleanup_retire_call276 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token275)
  br i1 %helper_crash_cleanup_retire_call276, label %helper_crash_cleanup_retire_accepted277, label %helper_crash_cleanup_retire_rejected278

helper_crash_cleanup_retire_merge274:             ; preds = %helper_crash_cleanup_retire_accepted277, %helper_crash_cleanup_retire_merge266
  %"hew_string_drop drop279" = load ptr, ptr %local_7, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop279")
  store ptr null, ptr %local_7, align 8
  %helper_crash_cleanup_drop_active280 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active280, label %helper_crash_cleanup_retire281, label %helper_crash_cleanup_retire_merge282

helper_crash_cleanup_retire_accepted277:          ; preds = %helper_crash_cleanup_retire273
  store i64 0, ptr %helper_crash_cleanup_token_7, align 8
  store i1 false, ptr %helper_crash_cleanup_active_7, align 1
  br label %helper_crash_cleanup_retire_merge274

helper_crash_cleanup_retire_rejected278:          ; preds = %helper_crash_cleanup_retire273
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire281:                   ; preds = %helper_crash_cleanup_retire_merge274
  %helper_crash_cleanup_retire_token283 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call284 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token283)
  br i1 %helper_crash_cleanup_retire_call284, label %helper_crash_cleanup_retire_accepted285, label %helper_crash_cleanup_retire_rejected286

helper_crash_cleanup_retire_merge282:             ; preds = %helper_crash_cleanup_retire_accepted285, %helper_crash_cleanup_retire_merge274
  %"hew_string_drop drop287" = load ptr, ptr %local_4, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop287")
  store ptr null, ptr %local_4, align 8
  %helper_crash_cleanup_drop_active288 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active288, label %helper_crash_cleanup_retire289, label %helper_crash_cleanup_retire_merge290

helper_crash_cleanup_retire_accepted285:          ; preds = %helper_crash_cleanup_retire281
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge282

helper_crash_cleanup_retire_rejected286:          ; preds = %helper_crash_cleanup_retire281
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire289:                   ; preds = %helper_crash_cleanup_retire_merge282
  %helper_crash_cleanup_retire_token291 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call292 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token291)
  br i1 %helper_crash_cleanup_retire_call292, label %helper_crash_cleanup_retire_accepted293, label %helper_crash_cleanup_retire_rejected294

helper_crash_cleanup_retire_merge290:             ; preds = %helper_crash_cleanup_retire_accepted293, %helper_crash_cleanup_retire_merge282
  %"hew_hashset_free_layout drop295" = load ptr, ptr %local_1, align 8
  call void @hew_hashset_free_layout(ptr %"hew_hashset_free_layout drop295")
  store ptr null, ptr %local_1, align 8
  ret i8 0

helper_crash_cleanup_retire_accepted293:          ; preds = %helper_crash_cleanup_retire289
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge290

helper_crash_cleanup_retire_rejected294:          ; preds = %helper_crash_cleanup_retire289
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

cancel_exit298:                                   ; preds = %bb33
  %helper_crash_cleanup_drop_active300 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_drop_active300, label %helper_crash_cleanup_retire301, label %helper_crash_cleanup_retire_merge302

after_cooperate299:                               ; preds = %bb33
  br label %bb27

helper_crash_cleanup_retire301:                   ; preds = %cancel_exit298
  %helper_crash_cleanup_retire_token303 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_retire_call304 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token303)
  br i1 %helper_crash_cleanup_retire_call304, label %helper_crash_cleanup_retire_accepted305, label %helper_crash_cleanup_retire_rejected306

helper_crash_cleanup_retire_merge302:             ; preds = %helper_crash_cleanup_retire_accepted305, %cancel_exit298
  %"hew_hashmap_free_layout drop307" = load ptr, ptr %local_29, align 8
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop307")
  store ptr null, ptr %local_29, align 8
  %helper_crash_cleanup_drop_active308 = load i1, ptr %helper_crash_cleanup_active_10, align 1
  br i1 %helper_crash_cleanup_drop_active308, label %helper_crash_cleanup_retire309, label %helper_crash_cleanup_retire_merge310

helper_crash_cleanup_retire_accepted305:          ; preds = %helper_crash_cleanup_retire301
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_retire_merge302

helper_crash_cleanup_retire_rejected306:          ; preds = %helper_crash_cleanup_retire301
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire309:                   ; preds = %helper_crash_cleanup_retire_merge302
  %helper_crash_cleanup_retire_token311 = load i64, ptr %helper_crash_cleanup_token_10, align 8
  %helper_crash_cleanup_retire_call312 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token311)
  br i1 %helper_crash_cleanup_retire_call312, label %helper_crash_cleanup_retire_accepted313, label %helper_crash_cleanup_retire_rejected314

helper_crash_cleanup_retire_merge310:             ; preds = %helper_crash_cleanup_retire_accepted313, %helper_crash_cleanup_retire_merge302
  %"hew_vec_free drop315" = load ptr, ptr %local_10, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop315")
  store ptr null, ptr %local_10, align 8
  %helper_crash_cleanup_drop_active316 = load i1, ptr %helper_crash_cleanup_active_7, align 1
  br i1 %helper_crash_cleanup_drop_active316, label %helper_crash_cleanup_retire317, label %helper_crash_cleanup_retire_merge318

helper_crash_cleanup_retire_accepted313:          ; preds = %helper_crash_cleanup_retire309
  store i64 0, ptr %helper_crash_cleanup_token_10, align 8
  store i1 false, ptr %helper_crash_cleanup_active_10, align 1
  br label %helper_crash_cleanup_retire_merge310

helper_crash_cleanup_retire_rejected314:          ; preds = %helper_crash_cleanup_retire309
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire317:                   ; preds = %helper_crash_cleanup_retire_merge310
  %helper_crash_cleanup_retire_token319 = load i64, ptr %helper_crash_cleanup_token_7, align 8
  %helper_crash_cleanup_retire_call320 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token319)
  br i1 %helper_crash_cleanup_retire_call320, label %helper_crash_cleanup_retire_accepted321, label %helper_crash_cleanup_retire_rejected322

helper_crash_cleanup_retire_merge318:             ; preds = %helper_crash_cleanup_retire_accepted321, %helper_crash_cleanup_retire_merge310
  %"hew_string_drop drop323" = load ptr, ptr %local_7, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop323")
  store ptr null, ptr %local_7, align 8
  %helper_crash_cleanup_drop_active324 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active324, label %helper_crash_cleanup_retire325, label %helper_crash_cleanup_retire_merge326

helper_crash_cleanup_retire_accepted321:          ; preds = %helper_crash_cleanup_retire317
  store i64 0, ptr %helper_crash_cleanup_token_7, align 8
  store i1 false, ptr %helper_crash_cleanup_active_7, align 1
  br label %helper_crash_cleanup_retire_merge318

helper_crash_cleanup_retire_rejected322:          ; preds = %helper_crash_cleanup_retire317
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire325:                   ; preds = %helper_crash_cleanup_retire_merge318
  %helper_crash_cleanup_retire_token327 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call328 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token327)
  br i1 %helper_crash_cleanup_retire_call328, label %helper_crash_cleanup_retire_accepted329, label %helper_crash_cleanup_retire_rejected330

helper_crash_cleanup_retire_merge326:             ; preds = %helper_crash_cleanup_retire_accepted329, %helper_crash_cleanup_retire_merge318
  %"hew_string_drop drop331" = load ptr, ptr %local_4, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop331")
  store ptr null, ptr %local_4, align 8
  %helper_crash_cleanup_drop_active332 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active332, label %helper_crash_cleanup_retire333, label %helper_crash_cleanup_retire_merge334

helper_crash_cleanup_retire_accepted329:          ; preds = %helper_crash_cleanup_retire325
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge326

helper_crash_cleanup_retire_rejected330:          ; preds = %helper_crash_cleanup_retire325
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire333:                   ; preds = %helper_crash_cleanup_retire_merge326
  %helper_crash_cleanup_retire_token335 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call336 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token335)
  br i1 %helper_crash_cleanup_retire_call336, label %helper_crash_cleanup_retire_accepted337, label %helper_crash_cleanup_retire_rejected338

helper_crash_cleanup_retire_merge334:             ; preds = %helper_crash_cleanup_retire_accepted337, %helper_crash_cleanup_retire_merge326
  %"hew_hashset_free_layout drop339" = load ptr, ptr %local_1, align 8
  call void @hew_hashset_free_layout(ptr %"hew_hashset_free_layout drop339")
  store ptr null, ptr %local_1, align 8
  ret i8 0

helper_crash_cleanup_retire_accepted337:          ; preds = %helper_crash_cleanup_retire333
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge334

helper_crash_cleanup_retire_rejected338:          ; preds = %helper_crash_cleanup_retire333
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

cancel_exit354:                                   ; preds = %bb36
  %helper_crash_cleanup_drop_active356 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_drop_active356, label %helper_crash_cleanup_retire357, label %helper_crash_cleanup_retire_merge358

after_cooperate355:                               ; preds = %bb36
  br label %bb35

helper_crash_cleanup_retire357:                   ; preds = %cancel_exit354
  %helper_crash_cleanup_retire_token359 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_retire_call360 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token359)
  br i1 %helper_crash_cleanup_retire_call360, label %helper_crash_cleanup_retire_accepted361, label %helper_crash_cleanup_retire_rejected362

helper_crash_cleanup_retire_merge358:             ; preds = %helper_crash_cleanup_retire_accepted361, %cancel_exit354
  %"hew_hashmap_free_layout drop363" = load ptr, ptr %local_29, align 8
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop363")
  store ptr null, ptr %local_29, align 8
  %helper_crash_cleanup_drop_active364 = load i1, ptr %helper_crash_cleanup_active_10, align 1
  br i1 %helper_crash_cleanup_drop_active364, label %helper_crash_cleanup_retire365, label %helper_crash_cleanup_retire_merge366

helper_crash_cleanup_retire_accepted361:          ; preds = %helper_crash_cleanup_retire357
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_retire_merge358

helper_crash_cleanup_retire_rejected362:          ; preds = %helper_crash_cleanup_retire357
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire365:                   ; preds = %helper_crash_cleanup_retire_merge358
  %helper_crash_cleanup_retire_token367 = load i64, ptr %helper_crash_cleanup_token_10, align 8
  %helper_crash_cleanup_retire_call368 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token367)
  br i1 %helper_crash_cleanup_retire_call368, label %helper_crash_cleanup_retire_accepted369, label %helper_crash_cleanup_retire_rejected370

helper_crash_cleanup_retire_merge366:             ; preds = %helper_crash_cleanup_retire_accepted369, %helper_crash_cleanup_retire_merge358
  %"hew_vec_free drop371" = load ptr, ptr %local_10, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop371")
  store ptr null, ptr %local_10, align 8
  %helper_crash_cleanup_drop_active372 = load i1, ptr %helper_crash_cleanup_active_7, align 1
  br i1 %helper_crash_cleanup_drop_active372, label %helper_crash_cleanup_retire373, label %helper_crash_cleanup_retire_merge374

helper_crash_cleanup_retire_accepted369:          ; preds = %helper_crash_cleanup_retire365
  store i64 0, ptr %helper_crash_cleanup_token_10, align 8
  store i1 false, ptr %helper_crash_cleanup_active_10, align 1
  br label %helper_crash_cleanup_retire_merge366

helper_crash_cleanup_retire_rejected370:          ; preds = %helper_crash_cleanup_retire365
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire373:                   ; preds = %helper_crash_cleanup_retire_merge366
  %helper_crash_cleanup_retire_token375 = load i64, ptr %helper_crash_cleanup_token_7, align 8
  %helper_crash_cleanup_retire_call376 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token375)
  br i1 %helper_crash_cleanup_retire_call376, label %helper_crash_cleanup_retire_accepted377, label %helper_crash_cleanup_retire_rejected378

helper_crash_cleanup_retire_merge374:             ; preds = %helper_crash_cleanup_retire_accepted377, %helper_crash_cleanup_retire_merge366
  %"hew_string_drop drop379" = load ptr, ptr %local_7, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop379")
  store ptr null, ptr %local_7, align 8
  %helper_crash_cleanup_drop_active380 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active380, label %helper_crash_cleanup_retire381, label %helper_crash_cleanup_retire_merge382

helper_crash_cleanup_retire_accepted377:          ; preds = %helper_crash_cleanup_retire373
  store i64 0, ptr %helper_crash_cleanup_token_7, align 8
  store i1 false, ptr %helper_crash_cleanup_active_7, align 1
  br label %helper_crash_cleanup_retire_merge374

helper_crash_cleanup_retire_rejected378:          ; preds = %helper_crash_cleanup_retire373
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire381:                   ; preds = %helper_crash_cleanup_retire_merge374
  %helper_crash_cleanup_retire_token383 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call384 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token383)
  br i1 %helper_crash_cleanup_retire_call384, label %helper_crash_cleanup_retire_accepted385, label %helper_crash_cleanup_retire_rejected386

helper_crash_cleanup_retire_merge382:             ; preds = %helper_crash_cleanup_retire_accepted385, %helper_crash_cleanup_retire_merge374
  %"hew_string_drop drop387" = load ptr, ptr %local_4, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop387")
  store ptr null, ptr %local_4, align 8
  %helper_crash_cleanup_drop_active388 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active388, label %helper_crash_cleanup_retire389, label %helper_crash_cleanup_retire_merge390

helper_crash_cleanup_retire_accepted385:          ; preds = %helper_crash_cleanup_retire381
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge382

helper_crash_cleanup_retire_rejected386:          ; preds = %helper_crash_cleanup_retire381
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire389:                   ; preds = %helper_crash_cleanup_retire_merge382
  %helper_crash_cleanup_retire_token391 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call392 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token391)
  br i1 %helper_crash_cleanup_retire_call392, label %helper_crash_cleanup_retire_accepted393, label %helper_crash_cleanup_retire_rejected394

helper_crash_cleanup_retire_merge390:             ; preds = %helper_crash_cleanup_retire_accepted393, %helper_crash_cleanup_retire_merge382
  %"hew_hashset_free_layout drop395" = load ptr, ptr %local_1, align 8
  call void @hew_hashset_free_layout(ptr %"hew_hashset_free_layout drop395")
  store ptr null, ptr %local_1, align 8
  ret i8 0

helper_crash_cleanup_retire_accepted393:          ; preds = %helper_crash_cleanup_retire389
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge390

helper_crash_cleanup_retire_rejected394:          ; preds = %helper_crash_cleanup_retire389
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

cancel_exit399:                                   ; preds = %bb37
  %helper_crash_cleanup_drop_active401 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_drop_active401, label %helper_crash_cleanup_retire402, label %helper_crash_cleanup_retire_merge403

after_cooperate400:                               ; preds = %bb37
  br label %bb35

helper_crash_cleanup_retire402:                   ; preds = %cancel_exit399
  %helper_crash_cleanup_retire_token404 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_retire_call405 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token404)
  br i1 %helper_crash_cleanup_retire_call405, label %helper_crash_cleanup_retire_accepted406, label %helper_crash_cleanup_retire_rejected407

helper_crash_cleanup_retire_merge403:             ; preds = %helper_crash_cleanup_retire_accepted406, %cancel_exit399
  %"hew_hashmap_free_layout drop408" = load ptr, ptr %local_29, align 8
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop408")
  store ptr null, ptr %local_29, align 8
  %helper_crash_cleanup_drop_active409 = load i1, ptr %helper_crash_cleanup_active_10, align 1
  br i1 %helper_crash_cleanup_drop_active409, label %helper_crash_cleanup_retire410, label %helper_crash_cleanup_retire_merge411

helper_crash_cleanup_retire_accepted406:          ; preds = %helper_crash_cleanup_retire402
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_retire_merge403

helper_crash_cleanup_retire_rejected407:          ; preds = %helper_crash_cleanup_retire402
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire410:                   ; preds = %helper_crash_cleanup_retire_merge403
  %helper_crash_cleanup_retire_token412 = load i64, ptr %helper_crash_cleanup_token_10, align 8
  %helper_crash_cleanup_retire_call413 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token412)
  br i1 %helper_crash_cleanup_retire_call413, label %helper_crash_cleanup_retire_accepted414, label %helper_crash_cleanup_retire_rejected415

helper_crash_cleanup_retire_merge411:             ; preds = %helper_crash_cleanup_retire_accepted414, %helper_crash_cleanup_retire_merge403
  %"hew_vec_free drop416" = load ptr, ptr %local_10, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop416")
  store ptr null, ptr %local_10, align 8
  %helper_crash_cleanup_drop_active417 = load i1, ptr %helper_crash_cleanup_active_7, align 1
  br i1 %helper_crash_cleanup_drop_active417, label %helper_crash_cleanup_retire418, label %helper_crash_cleanup_retire_merge419

helper_crash_cleanup_retire_accepted414:          ; preds = %helper_crash_cleanup_retire410
  store i64 0, ptr %helper_crash_cleanup_token_10, align 8
  store i1 false, ptr %helper_crash_cleanup_active_10, align 1
  br label %helper_crash_cleanup_retire_merge411

helper_crash_cleanup_retire_rejected415:          ; preds = %helper_crash_cleanup_retire410
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire418:                   ; preds = %helper_crash_cleanup_retire_merge411
  %helper_crash_cleanup_retire_token420 = load i64, ptr %helper_crash_cleanup_token_7, align 8
  %helper_crash_cleanup_retire_call421 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token420)
  br i1 %helper_crash_cleanup_retire_call421, label %helper_crash_cleanup_retire_accepted422, label %helper_crash_cleanup_retire_rejected423

helper_crash_cleanup_retire_merge419:             ; preds = %helper_crash_cleanup_retire_accepted422, %helper_crash_cleanup_retire_merge411
  %"hew_string_drop drop424" = load ptr, ptr %local_7, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop424")
  store ptr null, ptr %local_7, align 8
  %helper_crash_cleanup_drop_active425 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active425, label %helper_crash_cleanup_retire426, label %helper_crash_cleanup_retire_merge427

helper_crash_cleanup_retire_accepted422:          ; preds = %helper_crash_cleanup_retire418
  store i64 0, ptr %helper_crash_cleanup_token_7, align 8
  store i1 false, ptr %helper_crash_cleanup_active_7, align 1
  br label %helper_crash_cleanup_retire_merge419

helper_crash_cleanup_retire_rejected423:          ; preds = %helper_crash_cleanup_retire418
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire426:                   ; preds = %helper_crash_cleanup_retire_merge419
  %helper_crash_cleanup_retire_token428 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call429 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token428)
  br i1 %helper_crash_cleanup_retire_call429, label %helper_crash_cleanup_retire_accepted430, label %helper_crash_cleanup_retire_rejected431

helper_crash_cleanup_retire_merge427:             ; preds = %helper_crash_cleanup_retire_accepted430, %helper_crash_cleanup_retire_merge419
  %"hew_string_drop drop432" = load ptr, ptr %local_4, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop432")
  store ptr null, ptr %local_4, align 8
  %helper_crash_cleanup_drop_active433 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active433, label %helper_crash_cleanup_retire434, label %helper_crash_cleanup_retire_merge435

helper_crash_cleanup_retire_accepted430:          ; preds = %helper_crash_cleanup_retire426
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge427

helper_crash_cleanup_retire_rejected431:          ; preds = %helper_crash_cleanup_retire426
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire434:                   ; preds = %helper_crash_cleanup_retire_merge427
  %helper_crash_cleanup_retire_token436 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call437 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token436)
  br i1 %helper_crash_cleanup_retire_call437, label %helper_crash_cleanup_retire_accepted438, label %helper_crash_cleanup_retire_rejected439

helper_crash_cleanup_retire_merge435:             ; preds = %helper_crash_cleanup_retire_accepted438, %helper_crash_cleanup_retire_merge427
  %"hew_hashset_free_layout drop440" = load ptr, ptr %local_1, align 8
  call void @hew_hashset_free_layout(ptr %"hew_hashset_free_layout drop440")
  store ptr null, ptr %local_1, align 8
  ret i8 0

helper_crash_cleanup_retire_accepted438:          ; preds = %helper_crash_cleanup_retire434
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge435

helper_crash_cleanup_retire_rejected439:          ; preds = %helper_crash_cleanup_retire434
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire442:                   ; preds = %bb38
  %helper_crash_cleanup_retire_token444 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_retire_call445 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token444)
  br i1 %helper_crash_cleanup_retire_call445, label %helper_crash_cleanup_retire_accepted446, label %helper_crash_cleanup_retire_rejected447

helper_crash_cleanup_retire_merge443:             ; preds = %helper_crash_cleanup_retire_accepted446, %bb38
  %"hew_hashmap_free_layout drop448" = load ptr, ptr %local_29, align 8
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop448")
  store ptr null, ptr %local_29, align 8
  %helper_crash_cleanup_drop_active449 = load i1, ptr %helper_crash_cleanup_active_10, align 1
  br i1 %helper_crash_cleanup_drop_active449, label %helper_crash_cleanup_retire450, label %helper_crash_cleanup_retire_merge451

helper_crash_cleanup_retire_accepted446:          ; preds = %helper_crash_cleanup_retire442
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_retire_merge443

helper_crash_cleanup_retire_rejected447:          ; preds = %helper_crash_cleanup_retire442
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire450:                   ; preds = %helper_crash_cleanup_retire_merge443
  %helper_crash_cleanup_retire_token452 = load i64, ptr %helper_crash_cleanup_token_10, align 8
  %helper_crash_cleanup_retire_call453 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token452)
  br i1 %helper_crash_cleanup_retire_call453, label %helper_crash_cleanup_retire_accepted454, label %helper_crash_cleanup_retire_rejected455

helper_crash_cleanup_retire_merge451:             ; preds = %helper_crash_cleanup_retire_accepted454, %helper_crash_cleanup_retire_merge443
  %"hew_vec_free drop456" = load ptr, ptr %local_10, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop456")
  store ptr null, ptr %local_10, align 8
  %helper_crash_cleanup_drop_active457 = load i1, ptr %helper_crash_cleanup_active_7, align 1
  br i1 %helper_crash_cleanup_drop_active457, label %helper_crash_cleanup_retire458, label %helper_crash_cleanup_retire_merge459

helper_crash_cleanup_retire_accepted454:          ; preds = %helper_crash_cleanup_retire450
  store i64 0, ptr %helper_crash_cleanup_token_10, align 8
  store i1 false, ptr %helper_crash_cleanup_active_10, align 1
  br label %helper_crash_cleanup_retire_merge451

helper_crash_cleanup_retire_rejected455:          ; preds = %helper_crash_cleanup_retire450
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire458:                   ; preds = %helper_crash_cleanup_retire_merge451
  %helper_crash_cleanup_retire_token460 = load i64, ptr %helper_crash_cleanup_token_7, align 8
  %helper_crash_cleanup_retire_call461 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token460)
  br i1 %helper_crash_cleanup_retire_call461, label %helper_crash_cleanup_retire_accepted462, label %helper_crash_cleanup_retire_rejected463

helper_crash_cleanup_retire_merge459:             ; preds = %helper_crash_cleanup_retire_accepted462, %helper_crash_cleanup_retire_merge451
  %"hew_string_drop drop464" = load ptr, ptr %local_7, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop464")
  store ptr null, ptr %local_7, align 8
  %helper_crash_cleanup_drop_active465 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active465, label %helper_crash_cleanup_retire466, label %helper_crash_cleanup_retire_merge467

helper_crash_cleanup_retire_accepted462:          ; preds = %helper_crash_cleanup_retire458
  store i64 0, ptr %helper_crash_cleanup_token_7, align 8
  store i1 false, ptr %helper_crash_cleanup_active_7, align 1
  br label %helper_crash_cleanup_retire_merge459

helper_crash_cleanup_retire_rejected463:          ; preds = %helper_crash_cleanup_retire458
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire466:                   ; preds = %helper_crash_cleanup_retire_merge459
  %helper_crash_cleanup_retire_token468 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call469 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token468)
  br i1 %helper_crash_cleanup_retire_call469, label %helper_crash_cleanup_retire_accepted470, label %helper_crash_cleanup_retire_rejected471

helper_crash_cleanup_retire_merge467:             ; preds = %helper_crash_cleanup_retire_accepted470, %helper_crash_cleanup_retire_merge459
  %"hew_string_drop drop472" = load ptr, ptr %local_4, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop472")
  store ptr null, ptr %local_4, align 8
  %helper_crash_cleanup_drop_active473 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active473, label %helper_crash_cleanup_retire474, label %helper_crash_cleanup_retire_merge475

helper_crash_cleanup_retire_accepted470:          ; preds = %helper_crash_cleanup_retire466
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge467

helper_crash_cleanup_retire_rejected471:          ; preds = %helper_crash_cleanup_retire466
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire474:                   ; preds = %helper_crash_cleanup_retire_merge467
  %helper_crash_cleanup_retire_token476 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call477 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token476)
  br i1 %helper_crash_cleanup_retire_call477, label %helper_crash_cleanup_retire_accepted478, label %helper_crash_cleanup_retire_rejected479

helper_crash_cleanup_retire_merge475:             ; preds = %helper_crash_cleanup_retire_accepted478, %helper_crash_cleanup_retire_merge467
  %"hew_hashset_free_layout drop480" = load ptr, ptr %local_1, align 8
  call void @hew_hashset_free_layout(ptr %"hew_hashset_free_layout drop480")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire_accepted478:          ; preds = %helper_crash_cleanup_retire474
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge475

helper_crash_cleanup_retire_rejected479:          ; preds = %helper_crash_cleanup_retire474
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_deactivate489:               ; preds = %bb41
  %helper_crash_cleanup_token491 = load i64, ptr %helper_crash_cleanup_token_57, align 8
  %helper_crash_cleanup_deactivate_call492 = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token491)
  br i1 %helper_crash_cleanup_deactivate_call492, label %helper_crash_cleanup_deactivate_accepted493, label %helper_crash_cleanup_deactivate_rejected494

helper_crash_cleanup_deactivate_merge490:         ; preds = %helper_crash_cleanup_deactivate_accepted493, %bb41
  %move_load495 = load ptr, ptr %local_56, align 8
  store ptr %move_load495, ptr %local_57, align 8
  %helper_crash_cleanup_prior_token496 = load i64, ptr %helper_crash_cleanup_token_57, align 8
  %arm_typed_crash_cleanup497 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token496, ptr %local_57, i64 8, i64 8, ptr @__hew_frame_cleanup_5110292a480a33ee, i32 1, i32 0)
  %frame_cleanup_arm_failed498 = icmp eq i64 %arm_typed_crash_cleanup497, -1
  br i1 %frame_cleanup_arm_failed498, label %frame_cleanup_rejected500, label %frame_cleanup_registered499

helper_crash_cleanup_deactivate_accepted493:      ; preds = %helper_crash_cleanup_deactivate489
  store i1 false, ptr %helper_crash_cleanup_active_57, align 1
  br label %helper_crash_cleanup_deactivate_merge490

helper_crash_cleanup_deactivate_rejected494:      ; preds = %helper_crash_cleanup_deactivate489
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

frame_cleanup_registered499:                      ; preds = %helper_crash_cleanup_deactivate_merge490
  store i64 %arm_typed_crash_cleanup497, ptr %helper_crash_cleanup_token_57, align 8
  store i1 true, ptr %helper_crash_cleanup_active_57, align 1
  store i64 42, ptr %local_58, align 8
  %"hew_hashset_insert_layout arg0" = load ptr, ptr %local_57, align 8
  %hew_hashset_insert_layout_call = call i1 @hew_hashset_insert_layout(ptr %"hew_hashset_insert_layout arg0", ptr %local_58)
  %hashset_insert_bool_zext_i8 = zext i1 %hew_hashset_insert_layout_call to i8
  store i8 %hashset_insert_bool_zext_i8, ptr %local_59, align 1
  br label %bb42

frame_cleanup_rejected500:                        ; preds = %helper_crash_cleanup_deactivate_merge490
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_deactivate510:               ; preds = %bb47
  %helper_crash_cleanup_token512 = load i64, ptr %helper_crash_cleanup_token_66, align 8
  %helper_crash_cleanup_deactivate_call513 = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token512)
  br i1 %helper_crash_cleanup_deactivate_call513, label %helper_crash_cleanup_deactivate_accepted514, label %helper_crash_cleanup_deactivate_rejected515

helper_crash_cleanup_deactivate_merge511:         ; preds = %helper_crash_cleanup_deactivate_accepted514, %bb47
  %move_load516 = load ptr, ptr %local_65, align 8
  store ptr %move_load516, ptr %local_66, align 8
  %helper_crash_cleanup_prior_token517 = load i64, ptr %helper_crash_cleanup_token_66, align 8
  %arm_typed_crash_cleanup518 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token517, ptr %local_66, i64 8, i64 8, ptr @__hew_frame_cleanup_6e3157b8b1632579, i32 1, i32 0)
  %frame_cleanup_arm_failed519 = icmp eq i64 %arm_typed_crash_cleanup518, -1
  br i1 %frame_cleanup_arm_failed519, label %frame_cleanup_rejected521, label %frame_cleanup_registered520

helper_crash_cleanup_deactivate_accepted514:      ; preds = %helper_crash_cleanup_deactivate510
  store i1 false, ptr %helper_crash_cleanup_active_66, align 1
  br label %helper_crash_cleanup_deactivate_merge511

helper_crash_cleanup_deactivate_rejected515:      ; preds = %helper_crash_cleanup_deactivate510
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

frame_cleanup_registered520:                      ; preds = %helper_crash_cleanup_deactivate_merge511
  store i64 %arm_typed_crash_cleanup518, ptr %helper_crash_cleanup_token_66, align 8
  store i1 true, ptr %helper_crash_cleanup_active_66, align 1
  store ptr @str_lit.9, ptr %local_67, align 8
  %helper_crash_cleanup_was_active522 = load i1, ptr %helper_crash_cleanup_active_68, align 1
  br i1 %helper_crash_cleanup_was_active522, label %helper_crash_cleanup_deactivate523, label %helper_crash_cleanup_deactivate_merge524

frame_cleanup_rejected521:                        ; preds = %helper_crash_cleanup_deactivate_merge511
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_deactivate523:               ; preds = %frame_cleanup_registered520
  %helper_crash_cleanup_token525 = load i64, ptr %helper_crash_cleanup_token_68, align 8
  %helper_crash_cleanup_deactivate_call526 = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token525)
  br i1 %helper_crash_cleanup_deactivate_call526, label %helper_crash_cleanup_deactivate_accepted527, label %helper_crash_cleanup_deactivate_rejected528

helper_crash_cleanup_deactivate_merge524:         ; preds = %helper_crash_cleanup_deactivate_accepted527, %frame_cleanup_registered520
  %move_load529 = load ptr, ptr %local_67, align 8
  store ptr %move_load529, ptr %local_68, align 8
  %helper_crash_cleanup_prior_token530 = load i64, ptr %helper_crash_cleanup_token_68, align 8
  %arm_typed_crash_cleanup531 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token530, ptr %local_68, i64 8, i64 8, ptr @__hew_frame_cleanup_6e3157b8b1632579, i32 1, i32 0)
  %frame_cleanup_arm_failed532 = icmp eq i64 %arm_typed_crash_cleanup531, -1
  br i1 %frame_cleanup_arm_failed532, label %frame_cleanup_rejected534, label %frame_cleanup_registered533

helper_crash_cleanup_deactivate_accepted527:      ; preds = %helper_crash_cleanup_deactivate523
  store i1 false, ptr %helper_crash_cleanup_active_68, align 1
  br label %helper_crash_cleanup_deactivate_merge524

helper_crash_cleanup_deactivate_rejected528:      ; preds = %helper_crash_cleanup_deactivate523
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

frame_cleanup_registered533:                      ; preds = %helper_crash_cleanup_deactivate_merge524
  store i64 %arm_typed_crash_cleanup531, ptr %helper_crash_cleanup_token_68, align 8
  store i1 true, ptr %helper_crash_cleanup_active_68, align 1
  %"hew_string_concat arg0535" = load ptr, ptr %local_66, align 8
  %"hew_string_concat arg1536" = load ptr, ptr %local_68, align 8
  %hew_string_concat_call537 = call ptr @hew_string_concat(ptr %"hew_string_concat arg0535", ptr %"hew_string_concat arg1536")
  store ptr %hew_string_concat_call537, ptr %local_69, align 8
  %helper_crash_cleanup_was_active538 = load i1, ptr %helper_crash_cleanup_active_70, align 1
  br i1 %helper_crash_cleanup_was_active538, label %helper_crash_cleanup_deactivate539, label %helper_crash_cleanup_deactivate_merge540

frame_cleanup_rejected534:                        ; preds = %helper_crash_cleanup_deactivate_merge524
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_deactivate539:               ; preds = %frame_cleanup_registered533
  %helper_crash_cleanup_token541 = load i64, ptr %helper_crash_cleanup_token_70, align 8
  %helper_crash_cleanup_deactivate_call542 = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token541)
  br i1 %helper_crash_cleanup_deactivate_call542, label %helper_crash_cleanup_deactivate_accepted543, label %helper_crash_cleanup_deactivate_rejected544

helper_crash_cleanup_deactivate_merge540:         ; preds = %helper_crash_cleanup_deactivate_accepted543, %frame_cleanup_registered533
  %move_load545 = load ptr, ptr %local_69, align 8
  store ptr %move_load545, ptr %local_70, align 8
  %helper_crash_cleanup_prior_token546 = load i64, ptr %helper_crash_cleanup_token_70, align 8
  %arm_typed_crash_cleanup547 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token546, ptr %local_70, i64 8, i64 8, ptr @__hew_frame_cleanup_6e3157b8b1632579, i32 1, i32 0)
  %frame_cleanup_arm_failed548 = icmp eq i64 %arm_typed_crash_cleanup547, -1
  br i1 %frame_cleanup_arm_failed548, label %frame_cleanup_rejected550, label %frame_cleanup_registered549

helper_crash_cleanup_deactivate_accepted543:      ; preds = %helper_crash_cleanup_deactivate539
  store i1 false, ptr %helper_crash_cleanup_active_70, align 1
  br label %helper_crash_cleanup_deactivate_merge540

helper_crash_cleanup_deactivate_rejected544:      ; preds = %helper_crash_cleanup_deactivate539
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

frame_cleanup_registered549:                      ; preds = %helper_crash_cleanup_deactivate_merge540
  store i64 %arm_typed_crash_cleanup547, ptr %helper_crash_cleanup_token_70, align 8
  store i1 true, ptr %helper_crash_cleanup_active_70, align 1
  %print_arg551 = load ptr, ptr %local_70, align 8
  %print_str_bits = ptrtoint ptr %print_arg551 to i64
  call void @hew_print_value(i8 4, i64 %print_str_bits, i1 true)
  br label %bb48

frame_cleanup_rejected550:                        ; preds = %helper_crash_cleanup_deactivate_merge540
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire556:                   ; preds = %bb50
  %helper_crash_cleanup_retire_token558 = load i64, ptr %helper_crash_cleanup_token_70, align 8
  %helper_crash_cleanup_retire_call559 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token558)
  br i1 %helper_crash_cleanup_retire_call559, label %helper_crash_cleanup_retire_accepted560, label %helper_crash_cleanup_retire_rejected561

helper_crash_cleanup_retire_merge557:             ; preds = %helper_crash_cleanup_retire_accepted560, %bb50
  %"hew_string_drop drop562" = load ptr, ptr %local_70, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop562")
  store ptr null, ptr %local_70, align 8
  %helper_crash_cleanup_drop_active563 = load i1, ptr %helper_crash_cleanup_active_68, align 1
  br i1 %helper_crash_cleanup_drop_active563, label %helper_crash_cleanup_retire564, label %helper_crash_cleanup_retire_merge565

helper_crash_cleanup_retire_accepted560:          ; preds = %helper_crash_cleanup_retire556
  store i64 0, ptr %helper_crash_cleanup_token_70, align 8
  store i1 false, ptr %helper_crash_cleanup_active_70, align 1
  br label %helper_crash_cleanup_retire_merge557

helper_crash_cleanup_retire_rejected561:          ; preds = %helper_crash_cleanup_retire556
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire564:                   ; preds = %helper_crash_cleanup_retire_merge557
  %helper_crash_cleanup_retire_token566 = load i64, ptr %helper_crash_cleanup_token_68, align 8
  %helper_crash_cleanup_retire_call567 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token566)
  br i1 %helper_crash_cleanup_retire_call567, label %helper_crash_cleanup_retire_accepted568, label %helper_crash_cleanup_retire_rejected569

helper_crash_cleanup_retire_merge565:             ; preds = %helper_crash_cleanup_retire_accepted568, %helper_crash_cleanup_retire_merge557
  %"hew_string_drop drop570" = load ptr, ptr %local_68, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop570")
  store ptr null, ptr %local_68, align 8
  %helper_crash_cleanup_drop_active571 = load i1, ptr %helper_crash_cleanup_active_66, align 1
  br i1 %helper_crash_cleanup_drop_active571, label %helper_crash_cleanup_retire572, label %helper_crash_cleanup_retire_merge573

helper_crash_cleanup_retire_accepted568:          ; preds = %helper_crash_cleanup_retire564
  store i64 0, ptr %helper_crash_cleanup_token_68, align 8
  store i1 false, ptr %helper_crash_cleanup_active_68, align 1
  br label %helper_crash_cleanup_retire_merge565

helper_crash_cleanup_retire_rejected569:          ; preds = %helper_crash_cleanup_retire564
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire572:                   ; preds = %helper_crash_cleanup_retire_merge565
  %helper_crash_cleanup_retire_token574 = load i64, ptr %helper_crash_cleanup_token_66, align 8
  %helper_crash_cleanup_retire_call575 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token574)
  br i1 %helper_crash_cleanup_retire_call575, label %helper_crash_cleanup_retire_accepted576, label %helper_crash_cleanup_retire_rejected577

helper_crash_cleanup_retire_merge573:             ; preds = %helper_crash_cleanup_retire_accepted576, %helper_crash_cleanup_retire_merge565
  %"hew_string_drop drop578" = load ptr, ptr %local_66, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop578")
  store ptr null, ptr %local_66, align 8
  %helper_crash_cleanup_drop_active579 = load i1, ptr %helper_crash_cleanup_active_57, align 1
  br i1 %helper_crash_cleanup_drop_active579, label %helper_crash_cleanup_retire580, label %helper_crash_cleanup_retire_merge581

helper_crash_cleanup_retire_accepted576:          ; preds = %helper_crash_cleanup_retire572
  store i64 0, ptr %helper_crash_cleanup_token_66, align 8
  store i1 false, ptr %helper_crash_cleanup_active_66, align 1
  br label %helper_crash_cleanup_retire_merge573

helper_crash_cleanup_retire_rejected577:          ; preds = %helper_crash_cleanup_retire572
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire580:                   ; preds = %helper_crash_cleanup_retire_merge573
  %helper_crash_cleanup_retire_token582 = load i64, ptr %helper_crash_cleanup_token_57, align 8
  %helper_crash_cleanup_retire_call583 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token582)
  br i1 %helper_crash_cleanup_retire_call583, label %helper_crash_cleanup_retire_accepted584, label %helper_crash_cleanup_retire_rejected585

helper_crash_cleanup_retire_merge581:             ; preds = %helper_crash_cleanup_retire_accepted584, %helper_crash_cleanup_retire_merge573
  %"hew_hashset_free_layout drop586" = load ptr, ptr %local_57, align 8
  call void @hew_hashset_free_layout(ptr %"hew_hashset_free_layout drop586")
  store ptr null, ptr %local_57, align 8
  %helper_crash_cleanup_drop_active587 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_drop_active587, label %helper_crash_cleanup_retire588, label %helper_crash_cleanup_retire_merge589

helper_crash_cleanup_retire_accepted584:          ; preds = %helper_crash_cleanup_retire580
  store i64 0, ptr %helper_crash_cleanup_token_57, align 8
  store i1 false, ptr %helper_crash_cleanup_active_57, align 1
  br label %helper_crash_cleanup_retire_merge581

helper_crash_cleanup_retire_rejected585:          ; preds = %helper_crash_cleanup_retire580
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire588:                   ; preds = %helper_crash_cleanup_retire_merge581
  %helper_crash_cleanup_retire_token590 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_retire_call591 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token590)
  br i1 %helper_crash_cleanup_retire_call591, label %helper_crash_cleanup_retire_accepted592, label %helper_crash_cleanup_retire_rejected593

helper_crash_cleanup_retire_merge589:             ; preds = %helper_crash_cleanup_retire_accepted592, %helper_crash_cleanup_retire_merge581
  %"hew_hashmap_free_layout drop594" = load ptr, ptr %local_29, align 8
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop594")
  store ptr null, ptr %local_29, align 8
  %helper_crash_cleanup_drop_active595 = load i1, ptr %helper_crash_cleanup_active_10, align 1
  br i1 %helper_crash_cleanup_drop_active595, label %helper_crash_cleanup_retire596, label %helper_crash_cleanup_retire_merge597

helper_crash_cleanup_retire_accepted592:          ; preds = %helper_crash_cleanup_retire588
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_retire_merge589

helper_crash_cleanup_retire_rejected593:          ; preds = %helper_crash_cleanup_retire588
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire596:                   ; preds = %helper_crash_cleanup_retire_merge589
  %helper_crash_cleanup_retire_token598 = load i64, ptr %helper_crash_cleanup_token_10, align 8
  %helper_crash_cleanup_retire_call599 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token598)
  br i1 %helper_crash_cleanup_retire_call599, label %helper_crash_cleanup_retire_accepted600, label %helper_crash_cleanup_retire_rejected601

helper_crash_cleanup_retire_merge597:             ; preds = %helper_crash_cleanup_retire_accepted600, %helper_crash_cleanup_retire_merge589
  %"hew_vec_free drop602" = load ptr, ptr %local_10, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop602")
  store ptr null, ptr %local_10, align 8
  %helper_crash_cleanup_drop_active603 = load i1, ptr %helper_crash_cleanup_active_7, align 1
  br i1 %helper_crash_cleanup_drop_active603, label %helper_crash_cleanup_retire604, label %helper_crash_cleanup_retire_merge605

helper_crash_cleanup_retire_accepted600:          ; preds = %helper_crash_cleanup_retire596
  store i64 0, ptr %helper_crash_cleanup_token_10, align 8
  store i1 false, ptr %helper_crash_cleanup_active_10, align 1
  br label %helper_crash_cleanup_retire_merge597

helper_crash_cleanup_retire_rejected601:          ; preds = %helper_crash_cleanup_retire596
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire604:                   ; preds = %helper_crash_cleanup_retire_merge597
  %helper_crash_cleanup_retire_token606 = load i64, ptr %helper_crash_cleanup_token_7, align 8
  %helper_crash_cleanup_retire_call607 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token606)
  br i1 %helper_crash_cleanup_retire_call607, label %helper_crash_cleanup_retire_accepted608, label %helper_crash_cleanup_retire_rejected609

helper_crash_cleanup_retire_merge605:             ; preds = %helper_crash_cleanup_retire_accepted608, %helper_crash_cleanup_retire_merge597
  %"hew_string_drop drop610" = load ptr, ptr %local_7, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop610")
  store ptr null, ptr %local_7, align 8
  %helper_crash_cleanup_drop_active611 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active611, label %helper_crash_cleanup_retire612, label %helper_crash_cleanup_retire_merge613

helper_crash_cleanup_retire_accepted608:          ; preds = %helper_crash_cleanup_retire604
  store i64 0, ptr %helper_crash_cleanup_token_7, align 8
  store i1 false, ptr %helper_crash_cleanup_active_7, align 1
  br label %helper_crash_cleanup_retire_merge605

helper_crash_cleanup_retire_rejected609:          ; preds = %helper_crash_cleanup_retire604
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire612:                   ; preds = %helper_crash_cleanup_retire_merge605
  %helper_crash_cleanup_retire_token614 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call615 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token614)
  br i1 %helper_crash_cleanup_retire_call615, label %helper_crash_cleanup_retire_accepted616, label %helper_crash_cleanup_retire_rejected617

helper_crash_cleanup_retire_merge613:             ; preds = %helper_crash_cleanup_retire_accepted616, %helper_crash_cleanup_retire_merge605
  %"hew_string_drop drop618" = load ptr, ptr %local_4, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop618")
  store ptr null, ptr %local_4, align 8
  %helper_crash_cleanup_drop_active619 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active619, label %helper_crash_cleanup_retire620, label %helper_crash_cleanup_retire_merge621

helper_crash_cleanup_retire_accepted616:          ; preds = %helper_crash_cleanup_retire612
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge613

helper_crash_cleanup_retire_rejected617:          ; preds = %helper_crash_cleanup_retire612
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire620:                   ; preds = %helper_crash_cleanup_retire_merge613
  %helper_crash_cleanup_retire_token622 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call623 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token622)
  br i1 %helper_crash_cleanup_retire_call623, label %helper_crash_cleanup_retire_accepted624, label %helper_crash_cleanup_retire_rejected625

helper_crash_cleanup_retire_merge621:             ; preds = %helper_crash_cleanup_retire_accepted624, %helper_crash_cleanup_retire_merge613
  %"hew_hashset_free_layout drop626" = load ptr, ptr %local_1, align 8
  call void @hew_hashset_free_layout(ptr %"hew_hashset_free_layout drop626")
  store ptr null, ptr %local_1, align 8
  %helper_crash_cleanup_return_token_1 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_return_has_token_1 = icmp ne i64 %helper_crash_cleanup_return_token_1, 0
  br i1 %helper_crash_cleanup_return_has_token_1, label %helper_crash_cleanup_return_retire_1, label %helper_crash_cleanup_return_merge_1

helper_crash_cleanup_retire_accepted624:          ; preds = %helper_crash_cleanup_retire620
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge621

helper_crash_cleanup_retire_rejected625:          ; preds = %helper_crash_cleanup_retire620
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_return_merge_1:              ; preds = %helper_crash_cleanup_return_retire_1_accepted, %helper_crash_cleanup_retire_merge621
  %helper_crash_cleanup_return_token_4 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_return_has_token_4 = icmp ne i64 %helper_crash_cleanup_return_token_4, 0
  br i1 %helper_crash_cleanup_return_has_token_4, label %helper_crash_cleanup_return_retire_4, label %helper_crash_cleanup_return_merge_4

helper_crash_cleanup_return_retire_1:             ; preds = %helper_crash_cleanup_retire_merge621
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
  %helper_crash_cleanup_return_token_7 = load i64, ptr %helper_crash_cleanup_token_7, align 8
  %helper_crash_cleanup_return_has_token_7 = icmp ne i64 %helper_crash_cleanup_return_token_7, 0
  br i1 %helper_crash_cleanup_return_has_token_7, label %helper_crash_cleanup_return_retire_7, label %helper_crash_cleanup_return_merge_7

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

helper_crash_cleanup_return_merge_7:              ; preds = %helper_crash_cleanup_return_retire_7_accepted, %helper_crash_cleanup_return_merge_4
  %helper_crash_cleanup_return_token_10 = load i64, ptr %helper_crash_cleanup_token_10, align 8
  %helper_crash_cleanup_return_has_token_10 = icmp ne i64 %helper_crash_cleanup_return_token_10, 0
  br i1 %helper_crash_cleanup_return_has_token_10, label %helper_crash_cleanup_return_retire_10, label %helper_crash_cleanup_return_merge_10

helper_crash_cleanup_return_retire_7:             ; preds = %helper_crash_cleanup_return_merge_4
  %helper_crash_cleanup_return_retire_7_call = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_return_token_7)
  br i1 %helper_crash_cleanup_return_retire_7_call, label %helper_crash_cleanup_return_retire_7_accepted, label %helper_crash_cleanup_return_retire_7_rejected

helper_crash_cleanup_return_retire_7_accepted:    ; preds = %helper_crash_cleanup_return_retire_7
  store i64 0, ptr %helper_crash_cleanup_token_7, align 8
  store i1 false, ptr %helper_crash_cleanup_active_7, align 1
  br label %helper_crash_cleanup_return_merge_7

helper_crash_cleanup_return_retire_7_rejected:    ; preds = %helper_crash_cleanup_return_retire_7
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_return_merge_10:             ; preds = %helper_crash_cleanup_return_retire_10_accepted, %helper_crash_cleanup_return_merge_7
  %helper_crash_cleanup_return_token_29 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_return_has_token_29 = icmp ne i64 %helper_crash_cleanup_return_token_29, 0
  br i1 %helper_crash_cleanup_return_has_token_29, label %helper_crash_cleanup_return_retire_29, label %helper_crash_cleanup_return_merge_29

helper_crash_cleanup_return_retire_10:            ; preds = %helper_crash_cleanup_return_merge_7
  %helper_crash_cleanup_return_retire_10_call = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_return_token_10)
  br i1 %helper_crash_cleanup_return_retire_10_call, label %helper_crash_cleanup_return_retire_10_accepted, label %helper_crash_cleanup_return_retire_10_rejected

helper_crash_cleanup_return_retire_10_accepted:   ; preds = %helper_crash_cleanup_return_retire_10
  store i64 0, ptr %helper_crash_cleanup_token_10, align 8
  store i1 false, ptr %helper_crash_cleanup_active_10, align 1
  br label %helper_crash_cleanup_return_merge_10

helper_crash_cleanup_return_retire_10_rejected:   ; preds = %helper_crash_cleanup_return_retire_10
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_return_merge_29:             ; preds = %helper_crash_cleanup_return_retire_29_accepted, %helper_crash_cleanup_return_merge_10
  %helper_crash_cleanup_return_token_57 = load i64, ptr %helper_crash_cleanup_token_57, align 8
  %helper_crash_cleanup_return_has_token_57 = icmp ne i64 %helper_crash_cleanup_return_token_57, 0
  br i1 %helper_crash_cleanup_return_has_token_57, label %helper_crash_cleanup_return_retire_57, label %helper_crash_cleanup_return_merge_57

helper_crash_cleanup_return_retire_29:            ; preds = %helper_crash_cleanup_return_merge_10
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

helper_crash_cleanup_return_merge_57:             ; preds = %helper_crash_cleanup_return_retire_57_accepted, %helper_crash_cleanup_return_merge_29
  %helper_crash_cleanup_return_token_66 = load i64, ptr %helper_crash_cleanup_token_66, align 8
  %helper_crash_cleanup_return_has_token_66 = icmp ne i64 %helper_crash_cleanup_return_token_66, 0
  br i1 %helper_crash_cleanup_return_has_token_66, label %helper_crash_cleanup_return_retire_66, label %helper_crash_cleanup_return_merge_66

helper_crash_cleanup_return_retire_57:            ; preds = %helper_crash_cleanup_return_merge_29
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

helper_crash_cleanup_return_merge_66:             ; preds = %helper_crash_cleanup_return_retire_66_accepted, %helper_crash_cleanup_return_merge_57
  %helper_crash_cleanup_return_token_68 = load i64, ptr %helper_crash_cleanup_token_68, align 8
  %helper_crash_cleanup_return_has_token_68 = icmp ne i64 %helper_crash_cleanup_return_token_68, 0
  br i1 %helper_crash_cleanup_return_has_token_68, label %helper_crash_cleanup_return_retire_68, label %helper_crash_cleanup_return_merge_68

helper_crash_cleanup_return_retire_66:            ; preds = %helper_crash_cleanup_return_merge_57
  %helper_crash_cleanup_return_retire_66_call = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_return_token_66)
  br i1 %helper_crash_cleanup_return_retire_66_call, label %helper_crash_cleanup_return_retire_66_accepted, label %helper_crash_cleanup_return_retire_66_rejected

helper_crash_cleanup_return_retire_66_accepted:   ; preds = %helper_crash_cleanup_return_retire_66
  store i64 0, ptr %helper_crash_cleanup_token_66, align 8
  store i1 false, ptr %helper_crash_cleanup_active_66, align 1
  br label %helper_crash_cleanup_return_merge_66

helper_crash_cleanup_return_retire_66_rejected:   ; preds = %helper_crash_cleanup_return_retire_66
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_return_merge_68:             ; preds = %helper_crash_cleanup_return_retire_68_accepted, %helper_crash_cleanup_return_merge_66
  %helper_crash_cleanup_return_token_70 = load i64, ptr %helper_crash_cleanup_token_70, align 8
  %helper_crash_cleanup_return_has_token_70 = icmp ne i64 %helper_crash_cleanup_return_token_70, 0
  br i1 %helper_crash_cleanup_return_has_token_70, label %helper_crash_cleanup_return_retire_70, label %helper_crash_cleanup_return_merge_70

helper_crash_cleanup_return_retire_68:            ; preds = %helper_crash_cleanup_return_merge_66
  %helper_crash_cleanup_return_retire_68_call = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_return_token_68)
  br i1 %helper_crash_cleanup_return_retire_68_call, label %helper_crash_cleanup_return_retire_68_accepted, label %helper_crash_cleanup_return_retire_68_rejected

helper_crash_cleanup_return_retire_68_accepted:   ; preds = %helper_crash_cleanup_return_retire_68
  store i64 0, ptr %helper_crash_cleanup_token_68, align 8
  store i1 false, ptr %helper_crash_cleanup_active_68, align 1
  br label %helper_crash_cleanup_return_merge_68

helper_crash_cleanup_return_retire_68_rejected:   ; preds = %helper_crash_cleanup_return_retire_68
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_return_merge_70:             ; preds = %helper_crash_cleanup_return_retire_70_accepted, %helper_crash_cleanup_return_merge_68
  %hew_lambda_drain_all_call = call i32 @hew_lambda_drain_all(i64 0)
  %hew_lambda_drain_failed = icmp ne i32 %hew_lambda_drain_all_call, 0
  br i1 %hew_lambda_drain_failed, label %hew_shutdown_exit_failed, label %hew_shutdown_exit_continue

helper_crash_cleanup_return_retire_70:            ; preds = %helper_crash_cleanup_return_merge_68
  %helper_crash_cleanup_return_retire_70_call = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_return_token_70)
  br i1 %helper_crash_cleanup_return_retire_70_call, label %helper_crash_cleanup_return_retire_70_accepted, label %helper_crash_cleanup_return_retire_70_rejected

helper_crash_cleanup_return_retire_70_accepted:   ; preds = %helper_crash_cleanup_return_retire_70
  store i64 0, ptr %helper_crash_cleanup_token_70, align 8
  store i1 false, ptr %helper_crash_cleanup_active_70, align 1
  br label %helper_crash_cleanup_return_merge_70

helper_crash_cleanup_return_retire_70_rejected:   ; preds = %helper_crash_cleanup_return_retire_70
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

hew_shutdown_exit_failed:                         ; preds = %helper_crash_cleanup_return_merge_70
  call void @hew_exit(i64 1)
  br label %hew_shutdown_exit_continue

hew_shutdown_exit_continue:                       ; preds = %hew_shutdown_exit_failed, %helper_crash_cleanup_return_merge_70
  ret i8 0
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
  store ptr @str_lit.10, ptr %local_3, align 8
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

declare i32 @hew_actor_cooperate()

declare i1 @hew_hashset_insert_layout(ptr, ptr)

declare void @hew_string_drop(ptr)

declare ptr @hew_hashset_new_with_layout(ptr)

declare i1 @hew_cont_crash_cleanup_deactivate(i64)

declare void @hew_trap_with_code(i32)

; Function Attrs: cold noreturn nounwind memory(inaccessiblemem: write)
declare void @llvm.trap() #0

define internal void @__hew_frame_cleanup_b53461ca9fc57f84(ptr %0) {
entry:
  %"hew_hashset_free_layout drop" = load ptr, ptr %0, align 8
  call void @hew_hashset_free_layout(ptr %"hew_hashset_free_layout drop")
  store ptr null, ptr %0, align 8
  ret void
}

declare void @hew_hashset_free_layout(ptr)

declare i64 @hew_cont_crash_cleanup_arm(i64, ptr, i64, i64, ptr, i32, i32)

define internal void @__hew_frame_cleanup_6e3157b8b1632579(ptr %0) {
entry:
  %"hew_string_drop drop" = load ptr, ptr %0, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop")
  store ptr null, ptr %0, align 8
  ret void
}

declare i64 @hew_hashset_len_layout(ptr)

declare ptr @hew_vec_new_i64()

define internal void @__hew_frame_cleanup_7fdeddf79806b8a4(ptr %0) {
entry:
  %"hew_vec_free drop" = load ptr, ptr %0, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop")
  store ptr null, ptr %0, align 8
  ret void
}

declare void @hew_vec_free(ptr)

declare i1 @hew_vec_get_clone(ptr, i64, ptr)

declare i1 @hew_cont_crash_cleanup_retire(i64)

declare ptr @hew_hashmap_new_with_layout(ptr, ptr)

define internal void @__hew_frame_cleanup_01796e325d15e37f(ptr %0) {
entry:
  %"hew_hashmap_free_layout drop" = load ptr, ptr %0, align 8
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop")
  store ptr null, ptr %0, align 8
  ret void
}

declare void @hew_hashmap_free_layout(ptr)

declare i1 @hew_hashmap_insert_layout(ptr, ptr, ptr)

declare i64 @hew_hashmap_len_layout(ptr)

declare i1 @hew_hashmap_get_clone_layout(ptr, ptr, ptr)

declare i1 @hew_hashmap_remove_take_layout(ptr, ptr, ptr)

define internal void @__hew_frame_cleanup_5110292a480a33ee(ptr %0) {
entry:
  %"hew_hashset_free_layout drop" = load ptr, ptr %0, align 8
  call void @hew_hashset_free_layout(ptr %"hew_hashset_free_layout drop")
  store ptr null, ptr %0, align 8
  ret void
}

declare i1 @hew_hashset_contains_layout(ptr, ptr)

declare i32 @hew_lambda_drain_all(i64)

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.smul.with.overflow.i64(i64, i64) #1

attributes #0 = { cold noreturn nounwind memory(inaccessiblemem: write) }
attributes #1 = { nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none) }
