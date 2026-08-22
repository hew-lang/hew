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

bb13:                                             ; preds = %after_cooperate94, %after_cooperate63
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
  %move_load90 = load i64, ptr %local_25, align 8
  store i64 %move_load90, ptr %local_18, align 8
  %hew_actor_cooperate91 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel92 = icmp eq i32 %hew_actor_cooperate91, 2
  br i1 %hew_cooperate_is_cancel92, label %cancel_exit93, label %after_cooperate94

bb16:                                             ; preds = %bb17
  %helper_crash_cleanup_drop_active131 = load i1, ptr %helper_crash_cleanup_active_10, align 1
  br i1 %helper_crash_cleanup_drop_active131, label %helper_crash_cleanup_retire132, label %helper_crash_cleanup_retire_merge133

bb17:                                             ; preds = %bb12
  store i64 1, ptr %local_22, align 8
  %cmp_lhs163 = load i64, ptr %local_19, align 8
  %cmp_rhs164 = load i64, ptr %local_22, align 8
  %cmp_bit165 = icmp eq i64 %cmp_lhs163, %cmp_rhs164
  %cmp_zext166 = zext i1 %cmp_bit165 to i8
  store i8 %cmp_zext166, ptr %local_23, align 1
  %cond_load167 = load i8, ptr %local_23, align 1
  %cond_nz168 = icmp ne i8 %cond_load167, 0
  br i1 %cond_nz168, label %bb15, label %bb16

bb18:                                             ; preds = %bb13
  %call_arg169 = load ptr, ptr %local_10, align 8
  %call_result170 = call i64 @hew_vec_pop_i64(ptr %call_arg169)
  store i64 %call_result170, ptr %local_26, align 8
  br label %bb19

bb19:                                             ; preds = %bb18
  %move_load171 = load i64, ptr %local_26, align 8
  store i64 %move_load171, ptr %local_27, align 8
  %print_arg172 = load i64, ptr %local_27, align 8
  call void @hew_print_value(i8 1, i64 %print_arg172, i1 true)
  br label %bb20

bb20:                                             ; preds = %bb19
  %hew_hashmap_new_with_layout_call = call ptr @hew_hashmap_new_with_layout(ptr @hew_layout_key_string, ptr @hew_layout_val_i64)
  store ptr %hew_hashmap_new_with_layout_call, ptr %local_28, align 8
  br label %bb21

bb21:                                             ; preds = %bb20
  %helper_crash_cleanup_was_active173 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_was_active173, label %helper_crash_cleanup_deactivate174, label %helper_crash_cleanup_deactivate_merge175

bb22:                                             ; preds = %insert_overwrite_key_cont
  store ptr @str_lit.5, ptr %local_32, align 8
  store i64 20, ptr %local_33, align 8
  %"hew_hashmap_insert_layout arg0186" = load ptr, ptr %local_29, align 8
  %hew_hashmap_insert_layout_call187 = call i1 @hew_hashmap_insert_layout(ptr %"hew_hashmap_insert_layout arg0186", ptr %local_32, ptr %local_33)
  %insert_existed190 = icmp eq i1 %hew_hashmap_insert_layout_call187, false
  br i1 %insert_existed190, label %insert_overwrite_key_release188, label %insert_overwrite_key_cont189

bb23:                                             ; preds = %insert_overwrite_key_cont189
  %"hew_hashmap_len_layout arg0" = load ptr, ptr %local_29, align 8
  %hew_hashmap_len_layout_call = call i64 @hew_hashmap_len_layout(ptr %"hew_hashmap_len_layout arg0")
  store i64 %hew_hashmap_len_layout_call, ptr %local_34, align 8
  br label %bb24

bb24:                                             ; preds = %bb23
  %print_arg192 = load i64, ptr %local_34, align 8
  call void @hew_print_value(i8 1, i64 %print_arg192, i1 true)
  br label %bb25

bb25:                                             ; preds = %bb24
  store ptr @str_lit.6, ptr %local_36, align 8
  %"hew_hashmap_get_layout arg0" = load ptr, ptr %local_29, align 8
  %machine_payload_ptr193 = getelementptr inbounds nuw %"Option$$i64", ptr %local_37, i32 0, i32 1
  %machine_variant_field_ptr194 = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr193, i32 0, i32 0
  %hew_hashmap_get_clone_layout_call = call i1 @hew_hashmap_get_clone_layout(ptr %"hew_hashmap_get_layout arg0", ptr %local_36, ptr %machine_variant_field_ptr194)
  br i1 %hew_hashmap_get_clone_layout_call, label %hashmap_get_some, label %hashmap_get_none

bb26:                                             ; preds = %hashmap_get_initialized
  %machine_tag_ptr197 = getelementptr inbounds nuw %"Option$$i64", ptr %local_37, i32 0, i32 0
  %move_iN_load198 = load i8, ptr %machine_tag_ptr197, align 1
  %move_iN_zext199 = zext i8 %move_iN_load198 to i64
  store i64 %move_iN_zext199, ptr %local_38, align 8
  store i64 0, ptr %local_39, align 8
  %cmp_lhs200 = load i64, ptr %local_38, align 8
  %cmp_rhs201 = load i64, ptr %local_39, align 8
  %cmp_bit202 = icmp eq i64 %cmp_lhs200, %cmp_rhs201
  %cmp_zext203 = zext i1 %cmp_bit202 to i8
  store i8 %cmp_zext203, ptr %local_40, align 1
  %cond_load204 = load i8, ptr %local_40, align 1
  %cond_nz205 = icmp ne i8 %cond_load204, 0
  br i1 %cond_nz205, label %bb28, label %bb31

bb27:                                             ; preds = %after_cooperate311, %after_cooperate263
  store ptr @str_lit.7, ptr %local_46, align 8
  %"hew_hashmap_remove_take_layout arg0" = load ptr, ptr %local_29, align 8
  %machine_payload_ptr206 = getelementptr inbounds nuw %"Option$$i64", ptr %local_47, i32 0, i32 1
  %machine_variant_field_ptr207 = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr206, i32 0, i32 0
  %hew_hashmap_remove_take_layout_call = call i1 @hew_hashmap_remove_take_layout(ptr %"hew_hashmap_remove_take_layout arg0", ptr %local_46, ptr %machine_variant_field_ptr207)
  br i1 %hew_hashmap_remove_take_layout_call, label %hashmap_remove_some, label %hashmap_remove_none

bb28:                                             ; preds = %bb26
  %machine_payload_ptr210 = getelementptr inbounds nuw %"Option$$i64", ptr %local_37, i32 0, i32 1
  %machine_variant_field_ptr211 = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr210, i32 0, i32 0
  %move_load212 = load i64, ptr %machine_variant_field_ptr211, align 8
  store i64 %move_load212, ptr %local_43, align 8
  %print_arg213 = load i64, ptr %local_43, align 8
  call void @hew_print_value(i8 1, i64 %print_arg213, i1 true)
  br label %bb32

bb29:                                             ; preds = %bb31
  store i64 -1, ptr %local_44, align 8
  %print_arg214 = load i64, ptr %local_44, align 8
  call void @hew_print_value(i8 1, i64 %print_arg214, i1 true)
  br label %bb33

bb30:                                             ; preds = %bb31
  %helper_crash_cleanup_drop_active215 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_drop_active215, label %helper_crash_cleanup_retire216, label %helper_crash_cleanup_retire_merge217

bb31:                                             ; preds = %bb26
  store i64 1, ptr %local_41, align 8
  %cmp_lhs254 = load i64, ptr %local_38, align 8
  %cmp_rhs255 = load i64, ptr %local_41, align 8
  %cmp_bit256 = icmp eq i64 %cmp_lhs254, %cmp_rhs255
  %cmp_zext257 = zext i1 %cmp_bit256 to i8
  store i8 %cmp_zext257, ptr %local_42, align 1
  %cond_load258 = load i8, ptr %local_42, align 1
  %cond_nz259 = icmp ne i8 %cond_load258, 0
  br i1 %cond_nz259, label %bb29, label %bb30

bb32:                                             ; preds = %bb28
  %hew_actor_cooperate260 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel261 = icmp eq i32 %hew_actor_cooperate260, 2
  br i1 %hew_cooperate_is_cancel261, label %cancel_exit262, label %after_cooperate263

bb33:                                             ; preds = %bb29
  %hew_actor_cooperate308 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel309 = icmp eq i32 %hew_actor_cooperate308, 2
  br i1 %hew_cooperate_is_cancel309, label %cancel_exit310, label %after_cooperate311

bb34:                                             ; preds = %hashmap_remove_initialized
  %machine_tag_ptr356 = getelementptr inbounds nuw %"Option$$i64", ptr %local_47, i32 0, i32 0
  %move_iN_load357 = load i8, ptr %machine_tag_ptr356, align 1
  %move_iN_zext358 = zext i8 %move_iN_load357 to i64
  store i64 %move_iN_zext358, ptr %local_48, align 8
  store i64 0, ptr %local_49, align 8
  %cmp_lhs359 = load i64, ptr %local_48, align 8
  %cmp_rhs360 = load i64, ptr %local_49, align 8
  %cmp_bit361 = icmp eq i64 %cmp_lhs359, %cmp_rhs360
  %cmp_zext362 = zext i1 %cmp_bit361 to i8
  store i8 %cmp_zext362, ptr %local_50, align 1
  %cond_load363 = load i8, ptr %local_50, align 1
  %cond_nz364 = icmp ne i8 %cond_load363, 0
  br i1 %cond_nz364, label %bb36, label %bb39

bb35:                                             ; preds = %after_cooperate420, %after_cooperate371
  %move_load365 = load i8, ptr %local_45, align 1
  store i8 %move_load365, ptr %local_55, align 1
  %print_arg366 = load i8, ptr %local_55, align 1
  %print_narrow_bits = zext i8 %print_arg366 to i64
  call void @hew_print_value(i8 3, i64 %print_narrow_bits, i1 true)
  br label %bb40

bb36:                                             ; preds = %bb34
  store i8 1, ptr %local_53, align 1
  %move_load367 = load i8, ptr %local_53, align 1
  store i8 %move_load367, ptr %local_45, align 1
  %hew_actor_cooperate368 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel369 = icmp eq i32 %hew_actor_cooperate368, 2
  br i1 %hew_cooperate_is_cancel369, label %cancel_exit370, label %after_cooperate371

bb37:                                             ; preds = %bb39
  store i8 0, ptr %local_54, align 1
  %move_load416 = load i8, ptr %local_54, align 1
  store i8 %move_load416, ptr %local_45, align 1
  %hew_actor_cooperate417 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel418 = icmp eq i32 %hew_actor_cooperate417, 2
  br i1 %hew_cooperate_is_cancel418, label %cancel_exit419, label %after_cooperate420

bb38:                                             ; preds = %bb39
  %helper_crash_cleanup_drop_active465 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_drop_active465, label %helper_crash_cleanup_retire466, label %helper_crash_cleanup_retire_merge467

bb39:                                             ; preds = %bb34
  store i64 1, ptr %local_51, align 8
  %cmp_lhs505 = load i64, ptr %local_48, align 8
  %cmp_rhs506 = load i64, ptr %local_51, align 8
  %cmp_bit507 = icmp eq i64 %cmp_lhs505, %cmp_rhs506
  %cmp_zext508 = zext i1 %cmp_bit507 to i8
  store i8 %cmp_zext508, ptr %local_52, align 1
  %cond_load509 = load i8, ptr %local_52, align 1
  %cond_nz510 = icmp ne i8 %cond_load509, 0
  br i1 %cond_nz510, label %bb37, label %bb38

bb40:                                             ; preds = %bb35
  %hew_hashset_new_with_layout_call511 = call ptr @hew_hashset_new_with_layout(ptr @hew_layout_key_i64)
  store ptr %hew_hashset_new_with_layout_call511, ptr %local_56, align 8
  br label %bb41

bb41:                                             ; preds = %bb40
  %helper_crash_cleanup_was_active512 = load i1, ptr %helper_crash_cleanup_active_57, align 1
  br i1 %helper_crash_cleanup_was_active512, label %helper_crash_cleanup_deactivate513, label %helper_crash_cleanup_deactivate_merge514

bb42:                                             ; preds = %frame_cleanup_registered523
  store i64 7, ptr %local_60, align 8
  %"hew_hashset_insert_layout arg0525" = load ptr, ptr %local_57, align 8
  %hew_hashset_insert_layout_call526 = call i1 @hew_hashset_insert_layout(ptr %"hew_hashset_insert_layout arg0525", ptr %local_60)
  %hashset_insert_bool_zext_i8527 = zext i1 %hew_hashset_insert_layout_call526 to i8
  store i8 %hashset_insert_bool_zext_i8527, ptr %local_61, align 1
  br label %bb43

bb43:                                             ; preds = %bb42
  %"hew_hashset_len_layout arg0528" = load ptr, ptr %local_57, align 8
  %hew_hashset_len_layout_call529 = call i64 @hew_hashset_len_layout(ptr %"hew_hashset_len_layout arg0528")
  store i64 %hew_hashset_len_layout_call529, ptr %local_62, align 8
  br label %bb44

bb44:                                             ; preds = %bb43
  %print_arg530 = load i64, ptr %local_62, align 8
  call void @hew_print_value(i8 1, i64 %print_arg530, i1 true)
  br label %bb45

bb45:                                             ; preds = %bb44
  store i64 42, ptr %local_63, align 8
  %"hew_hashset_contains_layout arg0" = load ptr, ptr %local_57, align 8
  %hew_hashset_contains_layout_call = call i1 @hew_hashset_contains_layout(ptr %"hew_hashset_contains_layout arg0", ptr %local_63)
  %hashset_contains_bool_zext_i8 = zext i1 %hew_hashset_contains_layout_call to i8
  store i8 %hashset_contains_bool_zext_i8, ptr %local_64, align 1
  br label %bb46

bb46:                                             ; preds = %bb45
  %print_arg531 = load i8, ptr %local_64, align 1
  %print_narrow_bits532 = zext i8 %print_arg531 to i64
  call void @hew_print_value(i8 3, i64 %print_narrow_bits532, i1 true)
  br label %bb47

bb47:                                             ; preds = %bb46
  store ptr @str_lit.8, ptr %local_65, align 8
  %helper_crash_cleanup_was_active533 = load i1, ptr %helper_crash_cleanup_active_66, align 1
  br i1 %helper_crash_cleanup_was_active533, label %helper_crash_cleanup_deactivate534, label %helper_crash_cleanup_deactivate_merge535

bb48:                                             ; preds = %frame_cleanup_registered573
  %call_arg576 = load ptr, ptr %local_70, align 8
  %call_result577 = call i32 @hew_string_length(ptr %call_arg576)
  %ffi_sext = sext i32 %call_result577 to i64
  store i64 %ffi_sext, ptr %local_71, align 8
  br label %bb49

bb49:                                             ; preds = %bb48
  %print_arg578 = load i64, ptr %local_71, align 8
  call void @hew_print_value(i8 1, i64 %print_arg578, i1 true)
  br label %bb50

bb50:                                             ; preds = %bb49
  %helper_crash_cleanup_drop_active579 = load i1, ptr %helper_crash_cleanup_active_70, align 1
  br i1 %helper_crash_cleanup_drop_active579, label %helper_crash_cleanup_retire580, label %helper_crash_cleanup_retire_merge581

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
  ret i8 0

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
  %hew_runtime_exit_status_call86 = call i32 @hew_runtime_exit_status()
  %hew_runtime_faulted87 = icmp ne i32 %hew_runtime_exit_status_call86, 0
  br i1 %hew_runtime_faulted87, label %hew_exit_status_failed88, label %hew_exit_status_continue89

helper_crash_cleanup_retire_accepted84:           ; preds = %helper_crash_cleanup_retire80
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge81

helper_crash_cleanup_retire_rejected85:           ; preds = %helper_crash_cleanup_retire80
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

hew_exit_status_failed88:                         ; preds = %helper_crash_cleanup_retire_merge81
  call void @hew_exit(i64 1)
  br label %hew_exit_status_continue89

hew_exit_status_continue89:                       ; preds = %hew_exit_status_failed88, %helper_crash_cleanup_retire_merge81
  ret i8 0

cancel_exit93:                                    ; preds = %bb15
  %helper_crash_cleanup_drop_active95 = load i1, ptr %helper_crash_cleanup_active_10, align 1
  br i1 %helper_crash_cleanup_drop_active95, label %helper_crash_cleanup_retire96, label %helper_crash_cleanup_retire_merge97

after_cooperate94:                                ; preds = %bb15
  br label %bb13

helper_crash_cleanup_retire96:                    ; preds = %cancel_exit93
  %helper_crash_cleanup_retire_token98 = load i64, ptr %helper_crash_cleanup_token_10, align 8
  %helper_crash_cleanup_retire_call99 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token98)
  br i1 %helper_crash_cleanup_retire_call99, label %helper_crash_cleanup_retire_accepted100, label %helper_crash_cleanup_retire_rejected101

helper_crash_cleanup_retire_merge97:              ; preds = %helper_crash_cleanup_retire_accepted100, %cancel_exit93
  %"hew_vec_free drop102" = load ptr, ptr %local_10, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop102")
  store ptr null, ptr %local_10, align 8
  %helper_crash_cleanup_drop_active103 = load i1, ptr %helper_crash_cleanup_active_7, align 1
  br i1 %helper_crash_cleanup_drop_active103, label %helper_crash_cleanup_retire104, label %helper_crash_cleanup_retire_merge105

helper_crash_cleanup_retire_accepted100:          ; preds = %helper_crash_cleanup_retire96
  store i64 0, ptr %helper_crash_cleanup_token_10, align 8
  store i1 false, ptr %helper_crash_cleanup_active_10, align 1
  br label %helper_crash_cleanup_retire_merge97

helper_crash_cleanup_retire_rejected101:          ; preds = %helper_crash_cleanup_retire96
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire104:                   ; preds = %helper_crash_cleanup_retire_merge97
  %helper_crash_cleanup_retire_token106 = load i64, ptr %helper_crash_cleanup_token_7, align 8
  %helper_crash_cleanup_retire_call107 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token106)
  br i1 %helper_crash_cleanup_retire_call107, label %helper_crash_cleanup_retire_accepted108, label %helper_crash_cleanup_retire_rejected109

helper_crash_cleanup_retire_merge105:             ; preds = %helper_crash_cleanup_retire_accepted108, %helper_crash_cleanup_retire_merge97
  %"hew_string_drop drop110" = load ptr, ptr %local_7, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop110")
  store ptr null, ptr %local_7, align 8
  %helper_crash_cleanup_drop_active111 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active111, label %helper_crash_cleanup_retire112, label %helper_crash_cleanup_retire_merge113

helper_crash_cleanup_retire_accepted108:          ; preds = %helper_crash_cleanup_retire104
  store i64 0, ptr %helper_crash_cleanup_token_7, align 8
  store i1 false, ptr %helper_crash_cleanup_active_7, align 1
  br label %helper_crash_cleanup_retire_merge105

helper_crash_cleanup_retire_rejected109:          ; preds = %helper_crash_cleanup_retire104
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire112:                   ; preds = %helper_crash_cleanup_retire_merge105
  %helper_crash_cleanup_retire_token114 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call115 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token114)
  br i1 %helper_crash_cleanup_retire_call115, label %helper_crash_cleanup_retire_accepted116, label %helper_crash_cleanup_retire_rejected117

helper_crash_cleanup_retire_merge113:             ; preds = %helper_crash_cleanup_retire_accepted116, %helper_crash_cleanup_retire_merge105
  %"hew_string_drop drop118" = load ptr, ptr %local_4, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop118")
  store ptr null, ptr %local_4, align 8
  %helper_crash_cleanup_drop_active119 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active119, label %helper_crash_cleanup_retire120, label %helper_crash_cleanup_retire_merge121

helper_crash_cleanup_retire_accepted116:          ; preds = %helper_crash_cleanup_retire112
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge113

helper_crash_cleanup_retire_rejected117:          ; preds = %helper_crash_cleanup_retire112
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire120:                   ; preds = %helper_crash_cleanup_retire_merge113
  %helper_crash_cleanup_retire_token122 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call123 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token122)
  br i1 %helper_crash_cleanup_retire_call123, label %helper_crash_cleanup_retire_accepted124, label %helper_crash_cleanup_retire_rejected125

helper_crash_cleanup_retire_merge121:             ; preds = %helper_crash_cleanup_retire_accepted124, %helper_crash_cleanup_retire_merge113
  %"hew_hashset_free_layout drop126" = load ptr, ptr %local_1, align 8
  call void @hew_hashset_free_layout(ptr %"hew_hashset_free_layout drop126")
  store ptr null, ptr %local_1, align 8
  %hew_runtime_exit_status_call127 = call i32 @hew_runtime_exit_status()
  %hew_runtime_faulted128 = icmp ne i32 %hew_runtime_exit_status_call127, 0
  br i1 %hew_runtime_faulted128, label %hew_exit_status_failed129, label %hew_exit_status_continue130

helper_crash_cleanup_retire_accepted124:          ; preds = %helper_crash_cleanup_retire120
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge121

helper_crash_cleanup_retire_rejected125:          ; preds = %helper_crash_cleanup_retire120
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

hew_exit_status_failed129:                        ; preds = %helper_crash_cleanup_retire_merge121
  call void @hew_exit(i64 1)
  br label %hew_exit_status_continue130

hew_exit_status_continue130:                      ; preds = %hew_exit_status_failed129, %helper_crash_cleanup_retire_merge121
  ret i8 0

helper_crash_cleanup_retire132:                   ; preds = %bb16
  %helper_crash_cleanup_retire_token134 = load i64, ptr %helper_crash_cleanup_token_10, align 8
  %helper_crash_cleanup_retire_call135 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token134)
  br i1 %helper_crash_cleanup_retire_call135, label %helper_crash_cleanup_retire_accepted136, label %helper_crash_cleanup_retire_rejected137

helper_crash_cleanup_retire_merge133:             ; preds = %helper_crash_cleanup_retire_accepted136, %bb16
  %"hew_vec_free drop138" = load ptr, ptr %local_10, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop138")
  store ptr null, ptr %local_10, align 8
  %helper_crash_cleanup_drop_active139 = load i1, ptr %helper_crash_cleanup_active_7, align 1
  br i1 %helper_crash_cleanup_drop_active139, label %helper_crash_cleanup_retire140, label %helper_crash_cleanup_retire_merge141

helper_crash_cleanup_retire_accepted136:          ; preds = %helper_crash_cleanup_retire132
  store i64 0, ptr %helper_crash_cleanup_token_10, align 8
  store i1 false, ptr %helper_crash_cleanup_active_10, align 1
  br label %helper_crash_cleanup_retire_merge133

helper_crash_cleanup_retire_rejected137:          ; preds = %helper_crash_cleanup_retire132
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire140:                   ; preds = %helper_crash_cleanup_retire_merge133
  %helper_crash_cleanup_retire_token142 = load i64, ptr %helper_crash_cleanup_token_7, align 8
  %helper_crash_cleanup_retire_call143 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token142)
  br i1 %helper_crash_cleanup_retire_call143, label %helper_crash_cleanup_retire_accepted144, label %helper_crash_cleanup_retire_rejected145

helper_crash_cleanup_retire_merge141:             ; preds = %helper_crash_cleanup_retire_accepted144, %helper_crash_cleanup_retire_merge133
  %"hew_string_drop drop146" = load ptr, ptr %local_7, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop146")
  store ptr null, ptr %local_7, align 8
  %helper_crash_cleanup_drop_active147 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active147, label %helper_crash_cleanup_retire148, label %helper_crash_cleanup_retire_merge149

helper_crash_cleanup_retire_accepted144:          ; preds = %helper_crash_cleanup_retire140
  store i64 0, ptr %helper_crash_cleanup_token_7, align 8
  store i1 false, ptr %helper_crash_cleanup_active_7, align 1
  br label %helper_crash_cleanup_retire_merge141

helper_crash_cleanup_retire_rejected145:          ; preds = %helper_crash_cleanup_retire140
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire148:                   ; preds = %helper_crash_cleanup_retire_merge141
  %helper_crash_cleanup_retire_token150 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call151 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token150)
  br i1 %helper_crash_cleanup_retire_call151, label %helper_crash_cleanup_retire_accepted152, label %helper_crash_cleanup_retire_rejected153

helper_crash_cleanup_retire_merge149:             ; preds = %helper_crash_cleanup_retire_accepted152, %helper_crash_cleanup_retire_merge141
  %"hew_string_drop drop154" = load ptr, ptr %local_4, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop154")
  store ptr null, ptr %local_4, align 8
  %helper_crash_cleanup_drop_active155 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active155, label %helper_crash_cleanup_retire156, label %helper_crash_cleanup_retire_merge157

helper_crash_cleanup_retire_accepted152:          ; preds = %helper_crash_cleanup_retire148
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge149

helper_crash_cleanup_retire_rejected153:          ; preds = %helper_crash_cleanup_retire148
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire156:                   ; preds = %helper_crash_cleanup_retire_merge149
  %helper_crash_cleanup_retire_token158 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call159 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token158)
  br i1 %helper_crash_cleanup_retire_call159, label %helper_crash_cleanup_retire_accepted160, label %helper_crash_cleanup_retire_rejected161

helper_crash_cleanup_retire_merge157:             ; preds = %helper_crash_cleanup_retire_accepted160, %helper_crash_cleanup_retire_merge149
  %"hew_hashset_free_layout drop162" = load ptr, ptr %local_1, align 8
  call void @hew_hashset_free_layout(ptr %"hew_hashset_free_layout drop162")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire_accepted160:          ; preds = %helper_crash_cleanup_retire156
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge157

helper_crash_cleanup_retire_rejected161:          ; preds = %helper_crash_cleanup_retire156
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_deactivate174:               ; preds = %bb21
  %helper_crash_cleanup_token176 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_deactivate_call177 = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token176)
  br i1 %helper_crash_cleanup_deactivate_call177, label %helper_crash_cleanup_deactivate_accepted178, label %helper_crash_cleanup_deactivate_rejected179

helper_crash_cleanup_deactivate_merge175:         ; preds = %helper_crash_cleanup_deactivate_accepted178, %bb21
  %move_load180 = load ptr, ptr %local_28, align 8
  store ptr %move_load180, ptr %local_29, align 8
  %helper_crash_cleanup_prior_token181 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %arm_typed_crash_cleanup182 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token181, ptr %local_29, i64 8, i64 8, ptr @__hew_frame_cleanup_01796e325d15e37f, i32 1, i32 0)
  %frame_cleanup_arm_failed183 = icmp eq i64 %arm_typed_crash_cleanup182, -1
  br i1 %frame_cleanup_arm_failed183, label %frame_cleanup_rejected185, label %frame_cleanup_registered184

helper_crash_cleanup_deactivate_accepted178:      ; preds = %helper_crash_cleanup_deactivate174
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_deactivate_merge175

helper_crash_cleanup_deactivate_rejected179:      ; preds = %helper_crash_cleanup_deactivate174
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

frame_cleanup_registered184:                      ; preds = %helper_crash_cleanup_deactivate_merge175
  store i64 %arm_typed_crash_cleanup182, ptr %helper_crash_cleanup_token_29, align 8
  store i1 true, ptr %helper_crash_cleanup_active_29, align 1
  store ptr @str_lit.4, ptr %local_30, align 8
  store i64 10, ptr %local_31, align 8
  %"hew_hashmap_insert_layout arg0" = load ptr, ptr %local_29, align 8
  %hew_hashmap_insert_layout_call = call i1 @hew_hashmap_insert_layout(ptr %"hew_hashmap_insert_layout arg0", ptr %local_30, ptr %local_31)
  %insert_existed = icmp eq i1 %hew_hashmap_insert_layout_call, false
  br i1 %insert_existed, label %insert_overwrite_key_release, label %insert_overwrite_key_cont

frame_cleanup_rejected185:                        ; preds = %helper_crash_cleanup_deactivate_merge175
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

insert_overwrite_key_release:                     ; preds = %frame_cleanup_registered184
  %"hew_hashmap_insert_layout overwrite key" = load ptr, ptr %local_30, align 8
  call void @hew_string_drop(ptr %"hew_hashmap_insert_layout overwrite key")
  br label %insert_overwrite_key_cont

insert_overwrite_key_cont:                        ; preds = %insert_overwrite_key_release, %frame_cleanup_registered184
  br label %bb22

insert_overwrite_key_release188:                  ; preds = %bb22
  %"hew_hashmap_insert_layout overwrite key191" = load ptr, ptr %local_32, align 8
  call void @hew_string_drop(ptr %"hew_hashmap_insert_layout overwrite key191")
  br label %insert_overwrite_key_cont189

insert_overwrite_key_cont189:                     ; preds = %insert_overwrite_key_release188, %bb22
  br label %bb23

hashmap_get_none:                                 ; preds = %bb25
  %machine_tag_ptr195 = getelementptr inbounds nuw %"Option$$i64", ptr %local_37, i32 0, i32 0
  store i8 1, ptr %machine_tag_ptr195, align 1
  br label %hashmap_get_initialized

hashmap_get_some:                                 ; preds = %bb25
  %machine_tag_ptr196 = getelementptr inbounds nuw %"Option$$i64", ptr %local_37, i32 0, i32 0
  store i8 0, ptr %machine_tag_ptr196, align 1
  br label %hashmap_get_initialized

hashmap_get_initialized:                          ; preds = %hashmap_get_some, %hashmap_get_none
  br label %bb26

hashmap_remove_none:                              ; preds = %bb27
  %machine_tag_ptr208 = getelementptr inbounds nuw %"Option$$i64", ptr %local_47, i32 0, i32 0
  store i8 1, ptr %machine_tag_ptr208, align 1
  br label %hashmap_remove_initialized

hashmap_remove_some:                              ; preds = %bb27
  %machine_tag_ptr209 = getelementptr inbounds nuw %"Option$$i64", ptr %local_47, i32 0, i32 0
  store i8 0, ptr %machine_tag_ptr209, align 1
  br label %hashmap_remove_initialized

hashmap_remove_initialized:                       ; preds = %hashmap_remove_some, %hashmap_remove_none
  br label %bb34

helper_crash_cleanup_retire216:                   ; preds = %bb30
  %helper_crash_cleanup_retire_token218 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_retire_call219 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token218)
  br i1 %helper_crash_cleanup_retire_call219, label %helper_crash_cleanup_retire_accepted220, label %helper_crash_cleanup_retire_rejected221

helper_crash_cleanup_retire_merge217:             ; preds = %helper_crash_cleanup_retire_accepted220, %bb30
  %"hew_hashmap_free_layout drop" = load ptr, ptr %local_29, align 8
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop")
  store ptr null, ptr %local_29, align 8
  %helper_crash_cleanup_drop_active222 = load i1, ptr %helper_crash_cleanup_active_10, align 1
  br i1 %helper_crash_cleanup_drop_active222, label %helper_crash_cleanup_retire223, label %helper_crash_cleanup_retire_merge224

helper_crash_cleanup_retire_accepted220:          ; preds = %helper_crash_cleanup_retire216
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_retire_merge217

helper_crash_cleanup_retire_rejected221:          ; preds = %helper_crash_cleanup_retire216
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire223:                   ; preds = %helper_crash_cleanup_retire_merge217
  %helper_crash_cleanup_retire_token225 = load i64, ptr %helper_crash_cleanup_token_10, align 8
  %helper_crash_cleanup_retire_call226 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token225)
  br i1 %helper_crash_cleanup_retire_call226, label %helper_crash_cleanup_retire_accepted227, label %helper_crash_cleanup_retire_rejected228

helper_crash_cleanup_retire_merge224:             ; preds = %helper_crash_cleanup_retire_accepted227, %helper_crash_cleanup_retire_merge217
  %"hew_vec_free drop229" = load ptr, ptr %local_10, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop229")
  store ptr null, ptr %local_10, align 8
  %helper_crash_cleanup_drop_active230 = load i1, ptr %helper_crash_cleanup_active_7, align 1
  br i1 %helper_crash_cleanup_drop_active230, label %helper_crash_cleanup_retire231, label %helper_crash_cleanup_retire_merge232

helper_crash_cleanup_retire_accepted227:          ; preds = %helper_crash_cleanup_retire223
  store i64 0, ptr %helper_crash_cleanup_token_10, align 8
  store i1 false, ptr %helper_crash_cleanup_active_10, align 1
  br label %helper_crash_cleanup_retire_merge224

helper_crash_cleanup_retire_rejected228:          ; preds = %helper_crash_cleanup_retire223
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire231:                   ; preds = %helper_crash_cleanup_retire_merge224
  %helper_crash_cleanup_retire_token233 = load i64, ptr %helper_crash_cleanup_token_7, align 8
  %helper_crash_cleanup_retire_call234 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token233)
  br i1 %helper_crash_cleanup_retire_call234, label %helper_crash_cleanup_retire_accepted235, label %helper_crash_cleanup_retire_rejected236

helper_crash_cleanup_retire_merge232:             ; preds = %helper_crash_cleanup_retire_accepted235, %helper_crash_cleanup_retire_merge224
  %"hew_string_drop drop237" = load ptr, ptr %local_7, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop237")
  store ptr null, ptr %local_7, align 8
  %helper_crash_cleanup_drop_active238 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active238, label %helper_crash_cleanup_retire239, label %helper_crash_cleanup_retire_merge240

helper_crash_cleanup_retire_accepted235:          ; preds = %helper_crash_cleanup_retire231
  store i64 0, ptr %helper_crash_cleanup_token_7, align 8
  store i1 false, ptr %helper_crash_cleanup_active_7, align 1
  br label %helper_crash_cleanup_retire_merge232

helper_crash_cleanup_retire_rejected236:          ; preds = %helper_crash_cleanup_retire231
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire239:                   ; preds = %helper_crash_cleanup_retire_merge232
  %helper_crash_cleanup_retire_token241 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call242 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token241)
  br i1 %helper_crash_cleanup_retire_call242, label %helper_crash_cleanup_retire_accepted243, label %helper_crash_cleanup_retire_rejected244

helper_crash_cleanup_retire_merge240:             ; preds = %helper_crash_cleanup_retire_accepted243, %helper_crash_cleanup_retire_merge232
  %"hew_string_drop drop245" = load ptr, ptr %local_4, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop245")
  store ptr null, ptr %local_4, align 8
  %helper_crash_cleanup_drop_active246 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active246, label %helper_crash_cleanup_retire247, label %helper_crash_cleanup_retire_merge248

helper_crash_cleanup_retire_accepted243:          ; preds = %helper_crash_cleanup_retire239
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge240

helper_crash_cleanup_retire_rejected244:          ; preds = %helper_crash_cleanup_retire239
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire247:                   ; preds = %helper_crash_cleanup_retire_merge240
  %helper_crash_cleanup_retire_token249 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call250 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token249)
  br i1 %helper_crash_cleanup_retire_call250, label %helper_crash_cleanup_retire_accepted251, label %helper_crash_cleanup_retire_rejected252

helper_crash_cleanup_retire_merge248:             ; preds = %helper_crash_cleanup_retire_accepted251, %helper_crash_cleanup_retire_merge240
  %"hew_hashset_free_layout drop253" = load ptr, ptr %local_1, align 8
  call void @hew_hashset_free_layout(ptr %"hew_hashset_free_layout drop253")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire_accepted251:          ; preds = %helper_crash_cleanup_retire247
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge248

helper_crash_cleanup_retire_rejected252:          ; preds = %helper_crash_cleanup_retire247
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

cancel_exit262:                                   ; preds = %bb32
  %helper_crash_cleanup_drop_active264 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_drop_active264, label %helper_crash_cleanup_retire265, label %helper_crash_cleanup_retire_merge266

after_cooperate263:                               ; preds = %bb32
  br label %bb27

helper_crash_cleanup_retire265:                   ; preds = %cancel_exit262
  %helper_crash_cleanup_retire_token267 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_retire_call268 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token267)
  br i1 %helper_crash_cleanup_retire_call268, label %helper_crash_cleanup_retire_accepted269, label %helper_crash_cleanup_retire_rejected270

helper_crash_cleanup_retire_merge266:             ; preds = %helper_crash_cleanup_retire_accepted269, %cancel_exit262
  %"hew_hashmap_free_layout drop271" = load ptr, ptr %local_29, align 8
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop271")
  store ptr null, ptr %local_29, align 8
  %helper_crash_cleanup_drop_active272 = load i1, ptr %helper_crash_cleanup_active_10, align 1
  br i1 %helper_crash_cleanup_drop_active272, label %helper_crash_cleanup_retire273, label %helper_crash_cleanup_retire_merge274

helper_crash_cleanup_retire_accepted269:          ; preds = %helper_crash_cleanup_retire265
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_retire_merge266

helper_crash_cleanup_retire_rejected270:          ; preds = %helper_crash_cleanup_retire265
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire273:                   ; preds = %helper_crash_cleanup_retire_merge266
  %helper_crash_cleanup_retire_token275 = load i64, ptr %helper_crash_cleanup_token_10, align 8
  %helper_crash_cleanup_retire_call276 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token275)
  br i1 %helper_crash_cleanup_retire_call276, label %helper_crash_cleanup_retire_accepted277, label %helper_crash_cleanup_retire_rejected278

helper_crash_cleanup_retire_merge274:             ; preds = %helper_crash_cleanup_retire_accepted277, %helper_crash_cleanup_retire_merge266
  %"hew_vec_free drop279" = load ptr, ptr %local_10, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop279")
  store ptr null, ptr %local_10, align 8
  %helper_crash_cleanup_drop_active280 = load i1, ptr %helper_crash_cleanup_active_7, align 1
  br i1 %helper_crash_cleanup_drop_active280, label %helper_crash_cleanup_retire281, label %helper_crash_cleanup_retire_merge282

helper_crash_cleanup_retire_accepted277:          ; preds = %helper_crash_cleanup_retire273
  store i64 0, ptr %helper_crash_cleanup_token_10, align 8
  store i1 false, ptr %helper_crash_cleanup_active_10, align 1
  br label %helper_crash_cleanup_retire_merge274

helper_crash_cleanup_retire_rejected278:          ; preds = %helper_crash_cleanup_retire273
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire281:                   ; preds = %helper_crash_cleanup_retire_merge274
  %helper_crash_cleanup_retire_token283 = load i64, ptr %helper_crash_cleanup_token_7, align 8
  %helper_crash_cleanup_retire_call284 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token283)
  br i1 %helper_crash_cleanup_retire_call284, label %helper_crash_cleanup_retire_accepted285, label %helper_crash_cleanup_retire_rejected286

helper_crash_cleanup_retire_merge282:             ; preds = %helper_crash_cleanup_retire_accepted285, %helper_crash_cleanup_retire_merge274
  %"hew_string_drop drop287" = load ptr, ptr %local_7, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop287")
  store ptr null, ptr %local_7, align 8
  %helper_crash_cleanup_drop_active288 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active288, label %helper_crash_cleanup_retire289, label %helper_crash_cleanup_retire_merge290

helper_crash_cleanup_retire_accepted285:          ; preds = %helper_crash_cleanup_retire281
  store i64 0, ptr %helper_crash_cleanup_token_7, align 8
  store i1 false, ptr %helper_crash_cleanup_active_7, align 1
  br label %helper_crash_cleanup_retire_merge282

helper_crash_cleanup_retire_rejected286:          ; preds = %helper_crash_cleanup_retire281
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire289:                   ; preds = %helper_crash_cleanup_retire_merge282
  %helper_crash_cleanup_retire_token291 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call292 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token291)
  br i1 %helper_crash_cleanup_retire_call292, label %helper_crash_cleanup_retire_accepted293, label %helper_crash_cleanup_retire_rejected294

helper_crash_cleanup_retire_merge290:             ; preds = %helper_crash_cleanup_retire_accepted293, %helper_crash_cleanup_retire_merge282
  %"hew_string_drop drop295" = load ptr, ptr %local_4, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop295")
  store ptr null, ptr %local_4, align 8
  %helper_crash_cleanup_drop_active296 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active296, label %helper_crash_cleanup_retire297, label %helper_crash_cleanup_retire_merge298

helper_crash_cleanup_retire_accepted293:          ; preds = %helper_crash_cleanup_retire289
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge290

helper_crash_cleanup_retire_rejected294:          ; preds = %helper_crash_cleanup_retire289
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire297:                   ; preds = %helper_crash_cleanup_retire_merge290
  %helper_crash_cleanup_retire_token299 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call300 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token299)
  br i1 %helper_crash_cleanup_retire_call300, label %helper_crash_cleanup_retire_accepted301, label %helper_crash_cleanup_retire_rejected302

helper_crash_cleanup_retire_merge298:             ; preds = %helper_crash_cleanup_retire_accepted301, %helper_crash_cleanup_retire_merge290
  %"hew_hashset_free_layout drop303" = load ptr, ptr %local_1, align 8
  call void @hew_hashset_free_layout(ptr %"hew_hashset_free_layout drop303")
  store ptr null, ptr %local_1, align 8
  %hew_runtime_exit_status_call304 = call i32 @hew_runtime_exit_status()
  %hew_runtime_faulted305 = icmp ne i32 %hew_runtime_exit_status_call304, 0
  br i1 %hew_runtime_faulted305, label %hew_exit_status_failed306, label %hew_exit_status_continue307

helper_crash_cleanup_retire_accepted301:          ; preds = %helper_crash_cleanup_retire297
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge298

helper_crash_cleanup_retire_rejected302:          ; preds = %helper_crash_cleanup_retire297
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

hew_exit_status_failed306:                        ; preds = %helper_crash_cleanup_retire_merge298
  call void @hew_exit(i64 1)
  br label %hew_exit_status_continue307

hew_exit_status_continue307:                      ; preds = %hew_exit_status_failed306, %helper_crash_cleanup_retire_merge298
  ret i8 0

cancel_exit310:                                   ; preds = %bb33
  %helper_crash_cleanup_drop_active312 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_drop_active312, label %helper_crash_cleanup_retire313, label %helper_crash_cleanup_retire_merge314

after_cooperate311:                               ; preds = %bb33
  br label %bb27

helper_crash_cleanup_retire313:                   ; preds = %cancel_exit310
  %helper_crash_cleanup_retire_token315 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_retire_call316 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token315)
  br i1 %helper_crash_cleanup_retire_call316, label %helper_crash_cleanup_retire_accepted317, label %helper_crash_cleanup_retire_rejected318

helper_crash_cleanup_retire_merge314:             ; preds = %helper_crash_cleanup_retire_accepted317, %cancel_exit310
  %"hew_hashmap_free_layout drop319" = load ptr, ptr %local_29, align 8
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop319")
  store ptr null, ptr %local_29, align 8
  %helper_crash_cleanup_drop_active320 = load i1, ptr %helper_crash_cleanup_active_10, align 1
  br i1 %helper_crash_cleanup_drop_active320, label %helper_crash_cleanup_retire321, label %helper_crash_cleanup_retire_merge322

helper_crash_cleanup_retire_accepted317:          ; preds = %helper_crash_cleanup_retire313
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_retire_merge314

helper_crash_cleanup_retire_rejected318:          ; preds = %helper_crash_cleanup_retire313
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire321:                   ; preds = %helper_crash_cleanup_retire_merge314
  %helper_crash_cleanup_retire_token323 = load i64, ptr %helper_crash_cleanup_token_10, align 8
  %helper_crash_cleanup_retire_call324 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token323)
  br i1 %helper_crash_cleanup_retire_call324, label %helper_crash_cleanup_retire_accepted325, label %helper_crash_cleanup_retire_rejected326

helper_crash_cleanup_retire_merge322:             ; preds = %helper_crash_cleanup_retire_accepted325, %helper_crash_cleanup_retire_merge314
  %"hew_vec_free drop327" = load ptr, ptr %local_10, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop327")
  store ptr null, ptr %local_10, align 8
  %helper_crash_cleanup_drop_active328 = load i1, ptr %helper_crash_cleanup_active_7, align 1
  br i1 %helper_crash_cleanup_drop_active328, label %helper_crash_cleanup_retire329, label %helper_crash_cleanup_retire_merge330

helper_crash_cleanup_retire_accepted325:          ; preds = %helper_crash_cleanup_retire321
  store i64 0, ptr %helper_crash_cleanup_token_10, align 8
  store i1 false, ptr %helper_crash_cleanup_active_10, align 1
  br label %helper_crash_cleanup_retire_merge322

helper_crash_cleanup_retire_rejected326:          ; preds = %helper_crash_cleanup_retire321
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire329:                   ; preds = %helper_crash_cleanup_retire_merge322
  %helper_crash_cleanup_retire_token331 = load i64, ptr %helper_crash_cleanup_token_7, align 8
  %helper_crash_cleanup_retire_call332 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token331)
  br i1 %helper_crash_cleanup_retire_call332, label %helper_crash_cleanup_retire_accepted333, label %helper_crash_cleanup_retire_rejected334

helper_crash_cleanup_retire_merge330:             ; preds = %helper_crash_cleanup_retire_accepted333, %helper_crash_cleanup_retire_merge322
  %"hew_string_drop drop335" = load ptr, ptr %local_7, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop335")
  store ptr null, ptr %local_7, align 8
  %helper_crash_cleanup_drop_active336 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active336, label %helper_crash_cleanup_retire337, label %helper_crash_cleanup_retire_merge338

helper_crash_cleanup_retire_accepted333:          ; preds = %helper_crash_cleanup_retire329
  store i64 0, ptr %helper_crash_cleanup_token_7, align 8
  store i1 false, ptr %helper_crash_cleanup_active_7, align 1
  br label %helper_crash_cleanup_retire_merge330

helper_crash_cleanup_retire_rejected334:          ; preds = %helper_crash_cleanup_retire329
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire337:                   ; preds = %helper_crash_cleanup_retire_merge330
  %helper_crash_cleanup_retire_token339 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call340 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token339)
  br i1 %helper_crash_cleanup_retire_call340, label %helper_crash_cleanup_retire_accepted341, label %helper_crash_cleanup_retire_rejected342

helper_crash_cleanup_retire_merge338:             ; preds = %helper_crash_cleanup_retire_accepted341, %helper_crash_cleanup_retire_merge330
  %"hew_string_drop drop343" = load ptr, ptr %local_4, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop343")
  store ptr null, ptr %local_4, align 8
  %helper_crash_cleanup_drop_active344 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active344, label %helper_crash_cleanup_retire345, label %helper_crash_cleanup_retire_merge346

helper_crash_cleanup_retire_accepted341:          ; preds = %helper_crash_cleanup_retire337
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge338

helper_crash_cleanup_retire_rejected342:          ; preds = %helper_crash_cleanup_retire337
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire345:                   ; preds = %helper_crash_cleanup_retire_merge338
  %helper_crash_cleanup_retire_token347 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call348 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token347)
  br i1 %helper_crash_cleanup_retire_call348, label %helper_crash_cleanup_retire_accepted349, label %helper_crash_cleanup_retire_rejected350

helper_crash_cleanup_retire_merge346:             ; preds = %helper_crash_cleanup_retire_accepted349, %helper_crash_cleanup_retire_merge338
  %"hew_hashset_free_layout drop351" = load ptr, ptr %local_1, align 8
  call void @hew_hashset_free_layout(ptr %"hew_hashset_free_layout drop351")
  store ptr null, ptr %local_1, align 8
  %hew_runtime_exit_status_call352 = call i32 @hew_runtime_exit_status()
  %hew_runtime_faulted353 = icmp ne i32 %hew_runtime_exit_status_call352, 0
  br i1 %hew_runtime_faulted353, label %hew_exit_status_failed354, label %hew_exit_status_continue355

helper_crash_cleanup_retire_accepted349:          ; preds = %helper_crash_cleanup_retire345
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge346

helper_crash_cleanup_retire_rejected350:          ; preds = %helper_crash_cleanup_retire345
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

hew_exit_status_failed354:                        ; preds = %helper_crash_cleanup_retire_merge346
  call void @hew_exit(i64 1)
  br label %hew_exit_status_continue355

hew_exit_status_continue355:                      ; preds = %hew_exit_status_failed354, %helper_crash_cleanup_retire_merge346
  ret i8 0

cancel_exit370:                                   ; preds = %bb36
  %helper_crash_cleanup_drop_active372 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_drop_active372, label %helper_crash_cleanup_retire373, label %helper_crash_cleanup_retire_merge374

after_cooperate371:                               ; preds = %bb36
  br label %bb35

helper_crash_cleanup_retire373:                   ; preds = %cancel_exit370
  %helper_crash_cleanup_retire_token375 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_retire_call376 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token375)
  br i1 %helper_crash_cleanup_retire_call376, label %helper_crash_cleanup_retire_accepted377, label %helper_crash_cleanup_retire_rejected378

helper_crash_cleanup_retire_merge374:             ; preds = %helper_crash_cleanup_retire_accepted377, %cancel_exit370
  %"hew_hashmap_free_layout drop379" = load ptr, ptr %local_29, align 8
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop379")
  store ptr null, ptr %local_29, align 8
  %helper_crash_cleanup_drop_active380 = load i1, ptr %helper_crash_cleanup_active_10, align 1
  br i1 %helper_crash_cleanup_drop_active380, label %helper_crash_cleanup_retire381, label %helper_crash_cleanup_retire_merge382

helper_crash_cleanup_retire_accepted377:          ; preds = %helper_crash_cleanup_retire373
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_retire_merge374

helper_crash_cleanup_retire_rejected378:          ; preds = %helper_crash_cleanup_retire373
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire381:                   ; preds = %helper_crash_cleanup_retire_merge374
  %helper_crash_cleanup_retire_token383 = load i64, ptr %helper_crash_cleanup_token_10, align 8
  %helper_crash_cleanup_retire_call384 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token383)
  br i1 %helper_crash_cleanup_retire_call384, label %helper_crash_cleanup_retire_accepted385, label %helper_crash_cleanup_retire_rejected386

helper_crash_cleanup_retire_merge382:             ; preds = %helper_crash_cleanup_retire_accepted385, %helper_crash_cleanup_retire_merge374
  %"hew_vec_free drop387" = load ptr, ptr %local_10, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop387")
  store ptr null, ptr %local_10, align 8
  %helper_crash_cleanup_drop_active388 = load i1, ptr %helper_crash_cleanup_active_7, align 1
  br i1 %helper_crash_cleanup_drop_active388, label %helper_crash_cleanup_retire389, label %helper_crash_cleanup_retire_merge390

helper_crash_cleanup_retire_accepted385:          ; preds = %helper_crash_cleanup_retire381
  store i64 0, ptr %helper_crash_cleanup_token_10, align 8
  store i1 false, ptr %helper_crash_cleanup_active_10, align 1
  br label %helper_crash_cleanup_retire_merge382

helper_crash_cleanup_retire_rejected386:          ; preds = %helper_crash_cleanup_retire381
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire389:                   ; preds = %helper_crash_cleanup_retire_merge382
  %helper_crash_cleanup_retire_token391 = load i64, ptr %helper_crash_cleanup_token_7, align 8
  %helper_crash_cleanup_retire_call392 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token391)
  br i1 %helper_crash_cleanup_retire_call392, label %helper_crash_cleanup_retire_accepted393, label %helper_crash_cleanup_retire_rejected394

helper_crash_cleanup_retire_merge390:             ; preds = %helper_crash_cleanup_retire_accepted393, %helper_crash_cleanup_retire_merge382
  %"hew_string_drop drop395" = load ptr, ptr %local_7, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop395")
  store ptr null, ptr %local_7, align 8
  %helper_crash_cleanup_drop_active396 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active396, label %helper_crash_cleanup_retire397, label %helper_crash_cleanup_retire_merge398

helper_crash_cleanup_retire_accepted393:          ; preds = %helper_crash_cleanup_retire389
  store i64 0, ptr %helper_crash_cleanup_token_7, align 8
  store i1 false, ptr %helper_crash_cleanup_active_7, align 1
  br label %helper_crash_cleanup_retire_merge390

helper_crash_cleanup_retire_rejected394:          ; preds = %helper_crash_cleanup_retire389
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire397:                   ; preds = %helper_crash_cleanup_retire_merge390
  %helper_crash_cleanup_retire_token399 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call400 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token399)
  br i1 %helper_crash_cleanup_retire_call400, label %helper_crash_cleanup_retire_accepted401, label %helper_crash_cleanup_retire_rejected402

helper_crash_cleanup_retire_merge398:             ; preds = %helper_crash_cleanup_retire_accepted401, %helper_crash_cleanup_retire_merge390
  %"hew_string_drop drop403" = load ptr, ptr %local_4, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop403")
  store ptr null, ptr %local_4, align 8
  %helper_crash_cleanup_drop_active404 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active404, label %helper_crash_cleanup_retire405, label %helper_crash_cleanup_retire_merge406

helper_crash_cleanup_retire_accepted401:          ; preds = %helper_crash_cleanup_retire397
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge398

helper_crash_cleanup_retire_rejected402:          ; preds = %helper_crash_cleanup_retire397
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire405:                   ; preds = %helper_crash_cleanup_retire_merge398
  %helper_crash_cleanup_retire_token407 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call408 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token407)
  br i1 %helper_crash_cleanup_retire_call408, label %helper_crash_cleanup_retire_accepted409, label %helper_crash_cleanup_retire_rejected410

helper_crash_cleanup_retire_merge406:             ; preds = %helper_crash_cleanup_retire_accepted409, %helper_crash_cleanup_retire_merge398
  %"hew_hashset_free_layout drop411" = load ptr, ptr %local_1, align 8
  call void @hew_hashset_free_layout(ptr %"hew_hashset_free_layout drop411")
  store ptr null, ptr %local_1, align 8
  %hew_runtime_exit_status_call412 = call i32 @hew_runtime_exit_status()
  %hew_runtime_faulted413 = icmp ne i32 %hew_runtime_exit_status_call412, 0
  br i1 %hew_runtime_faulted413, label %hew_exit_status_failed414, label %hew_exit_status_continue415

helper_crash_cleanup_retire_accepted409:          ; preds = %helper_crash_cleanup_retire405
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge406

helper_crash_cleanup_retire_rejected410:          ; preds = %helper_crash_cleanup_retire405
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

hew_exit_status_failed414:                        ; preds = %helper_crash_cleanup_retire_merge406
  call void @hew_exit(i64 1)
  br label %hew_exit_status_continue415

hew_exit_status_continue415:                      ; preds = %hew_exit_status_failed414, %helper_crash_cleanup_retire_merge406
  ret i8 0

cancel_exit419:                                   ; preds = %bb37
  %helper_crash_cleanup_drop_active421 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_drop_active421, label %helper_crash_cleanup_retire422, label %helper_crash_cleanup_retire_merge423

after_cooperate420:                               ; preds = %bb37
  br label %bb35

helper_crash_cleanup_retire422:                   ; preds = %cancel_exit419
  %helper_crash_cleanup_retire_token424 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_retire_call425 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token424)
  br i1 %helper_crash_cleanup_retire_call425, label %helper_crash_cleanup_retire_accepted426, label %helper_crash_cleanup_retire_rejected427

helper_crash_cleanup_retire_merge423:             ; preds = %helper_crash_cleanup_retire_accepted426, %cancel_exit419
  %"hew_hashmap_free_layout drop428" = load ptr, ptr %local_29, align 8
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop428")
  store ptr null, ptr %local_29, align 8
  %helper_crash_cleanup_drop_active429 = load i1, ptr %helper_crash_cleanup_active_10, align 1
  br i1 %helper_crash_cleanup_drop_active429, label %helper_crash_cleanup_retire430, label %helper_crash_cleanup_retire_merge431

helper_crash_cleanup_retire_accepted426:          ; preds = %helper_crash_cleanup_retire422
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_retire_merge423

helper_crash_cleanup_retire_rejected427:          ; preds = %helper_crash_cleanup_retire422
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire430:                   ; preds = %helper_crash_cleanup_retire_merge423
  %helper_crash_cleanup_retire_token432 = load i64, ptr %helper_crash_cleanup_token_10, align 8
  %helper_crash_cleanup_retire_call433 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token432)
  br i1 %helper_crash_cleanup_retire_call433, label %helper_crash_cleanup_retire_accepted434, label %helper_crash_cleanup_retire_rejected435

helper_crash_cleanup_retire_merge431:             ; preds = %helper_crash_cleanup_retire_accepted434, %helper_crash_cleanup_retire_merge423
  %"hew_vec_free drop436" = load ptr, ptr %local_10, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop436")
  store ptr null, ptr %local_10, align 8
  %helper_crash_cleanup_drop_active437 = load i1, ptr %helper_crash_cleanup_active_7, align 1
  br i1 %helper_crash_cleanup_drop_active437, label %helper_crash_cleanup_retire438, label %helper_crash_cleanup_retire_merge439

helper_crash_cleanup_retire_accepted434:          ; preds = %helper_crash_cleanup_retire430
  store i64 0, ptr %helper_crash_cleanup_token_10, align 8
  store i1 false, ptr %helper_crash_cleanup_active_10, align 1
  br label %helper_crash_cleanup_retire_merge431

helper_crash_cleanup_retire_rejected435:          ; preds = %helper_crash_cleanup_retire430
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire438:                   ; preds = %helper_crash_cleanup_retire_merge431
  %helper_crash_cleanup_retire_token440 = load i64, ptr %helper_crash_cleanup_token_7, align 8
  %helper_crash_cleanup_retire_call441 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token440)
  br i1 %helper_crash_cleanup_retire_call441, label %helper_crash_cleanup_retire_accepted442, label %helper_crash_cleanup_retire_rejected443

helper_crash_cleanup_retire_merge439:             ; preds = %helper_crash_cleanup_retire_accepted442, %helper_crash_cleanup_retire_merge431
  %"hew_string_drop drop444" = load ptr, ptr %local_7, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop444")
  store ptr null, ptr %local_7, align 8
  %helper_crash_cleanup_drop_active445 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active445, label %helper_crash_cleanup_retire446, label %helper_crash_cleanup_retire_merge447

helper_crash_cleanup_retire_accepted442:          ; preds = %helper_crash_cleanup_retire438
  store i64 0, ptr %helper_crash_cleanup_token_7, align 8
  store i1 false, ptr %helper_crash_cleanup_active_7, align 1
  br label %helper_crash_cleanup_retire_merge439

helper_crash_cleanup_retire_rejected443:          ; preds = %helper_crash_cleanup_retire438
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire446:                   ; preds = %helper_crash_cleanup_retire_merge439
  %helper_crash_cleanup_retire_token448 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call449 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token448)
  br i1 %helper_crash_cleanup_retire_call449, label %helper_crash_cleanup_retire_accepted450, label %helper_crash_cleanup_retire_rejected451

helper_crash_cleanup_retire_merge447:             ; preds = %helper_crash_cleanup_retire_accepted450, %helper_crash_cleanup_retire_merge439
  %"hew_string_drop drop452" = load ptr, ptr %local_4, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop452")
  store ptr null, ptr %local_4, align 8
  %helper_crash_cleanup_drop_active453 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active453, label %helper_crash_cleanup_retire454, label %helper_crash_cleanup_retire_merge455

helper_crash_cleanup_retire_accepted450:          ; preds = %helper_crash_cleanup_retire446
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge447

helper_crash_cleanup_retire_rejected451:          ; preds = %helper_crash_cleanup_retire446
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire454:                   ; preds = %helper_crash_cleanup_retire_merge447
  %helper_crash_cleanup_retire_token456 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call457 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token456)
  br i1 %helper_crash_cleanup_retire_call457, label %helper_crash_cleanup_retire_accepted458, label %helper_crash_cleanup_retire_rejected459

helper_crash_cleanup_retire_merge455:             ; preds = %helper_crash_cleanup_retire_accepted458, %helper_crash_cleanup_retire_merge447
  %"hew_hashset_free_layout drop460" = load ptr, ptr %local_1, align 8
  call void @hew_hashset_free_layout(ptr %"hew_hashset_free_layout drop460")
  store ptr null, ptr %local_1, align 8
  %hew_runtime_exit_status_call461 = call i32 @hew_runtime_exit_status()
  %hew_runtime_faulted462 = icmp ne i32 %hew_runtime_exit_status_call461, 0
  br i1 %hew_runtime_faulted462, label %hew_exit_status_failed463, label %hew_exit_status_continue464

helper_crash_cleanup_retire_accepted458:          ; preds = %helper_crash_cleanup_retire454
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge455

helper_crash_cleanup_retire_rejected459:          ; preds = %helper_crash_cleanup_retire454
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

hew_exit_status_failed463:                        ; preds = %helper_crash_cleanup_retire_merge455
  call void @hew_exit(i64 1)
  br label %hew_exit_status_continue464

hew_exit_status_continue464:                      ; preds = %hew_exit_status_failed463, %helper_crash_cleanup_retire_merge455
  ret i8 0

helper_crash_cleanup_retire466:                   ; preds = %bb38
  %helper_crash_cleanup_retire_token468 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_retire_call469 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token468)
  br i1 %helper_crash_cleanup_retire_call469, label %helper_crash_cleanup_retire_accepted470, label %helper_crash_cleanup_retire_rejected471

helper_crash_cleanup_retire_merge467:             ; preds = %helper_crash_cleanup_retire_accepted470, %bb38
  %"hew_hashmap_free_layout drop472" = load ptr, ptr %local_29, align 8
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop472")
  store ptr null, ptr %local_29, align 8
  %helper_crash_cleanup_drop_active473 = load i1, ptr %helper_crash_cleanup_active_10, align 1
  br i1 %helper_crash_cleanup_drop_active473, label %helper_crash_cleanup_retire474, label %helper_crash_cleanup_retire_merge475

helper_crash_cleanup_retire_accepted470:          ; preds = %helper_crash_cleanup_retire466
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_retire_merge467

helper_crash_cleanup_retire_rejected471:          ; preds = %helper_crash_cleanup_retire466
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire474:                   ; preds = %helper_crash_cleanup_retire_merge467
  %helper_crash_cleanup_retire_token476 = load i64, ptr %helper_crash_cleanup_token_10, align 8
  %helper_crash_cleanup_retire_call477 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token476)
  br i1 %helper_crash_cleanup_retire_call477, label %helper_crash_cleanup_retire_accepted478, label %helper_crash_cleanup_retire_rejected479

helper_crash_cleanup_retire_merge475:             ; preds = %helper_crash_cleanup_retire_accepted478, %helper_crash_cleanup_retire_merge467
  %"hew_vec_free drop480" = load ptr, ptr %local_10, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop480")
  store ptr null, ptr %local_10, align 8
  %helper_crash_cleanup_drop_active481 = load i1, ptr %helper_crash_cleanup_active_7, align 1
  br i1 %helper_crash_cleanup_drop_active481, label %helper_crash_cleanup_retire482, label %helper_crash_cleanup_retire_merge483

helper_crash_cleanup_retire_accepted478:          ; preds = %helper_crash_cleanup_retire474
  store i64 0, ptr %helper_crash_cleanup_token_10, align 8
  store i1 false, ptr %helper_crash_cleanup_active_10, align 1
  br label %helper_crash_cleanup_retire_merge475

helper_crash_cleanup_retire_rejected479:          ; preds = %helper_crash_cleanup_retire474
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire482:                   ; preds = %helper_crash_cleanup_retire_merge475
  %helper_crash_cleanup_retire_token484 = load i64, ptr %helper_crash_cleanup_token_7, align 8
  %helper_crash_cleanup_retire_call485 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token484)
  br i1 %helper_crash_cleanup_retire_call485, label %helper_crash_cleanup_retire_accepted486, label %helper_crash_cleanup_retire_rejected487

helper_crash_cleanup_retire_merge483:             ; preds = %helper_crash_cleanup_retire_accepted486, %helper_crash_cleanup_retire_merge475
  %"hew_string_drop drop488" = load ptr, ptr %local_7, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop488")
  store ptr null, ptr %local_7, align 8
  %helper_crash_cleanup_drop_active489 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active489, label %helper_crash_cleanup_retire490, label %helper_crash_cleanup_retire_merge491

helper_crash_cleanup_retire_accepted486:          ; preds = %helper_crash_cleanup_retire482
  store i64 0, ptr %helper_crash_cleanup_token_7, align 8
  store i1 false, ptr %helper_crash_cleanup_active_7, align 1
  br label %helper_crash_cleanup_retire_merge483

helper_crash_cleanup_retire_rejected487:          ; preds = %helper_crash_cleanup_retire482
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire490:                   ; preds = %helper_crash_cleanup_retire_merge483
  %helper_crash_cleanup_retire_token492 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call493 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token492)
  br i1 %helper_crash_cleanup_retire_call493, label %helper_crash_cleanup_retire_accepted494, label %helper_crash_cleanup_retire_rejected495

helper_crash_cleanup_retire_merge491:             ; preds = %helper_crash_cleanup_retire_accepted494, %helper_crash_cleanup_retire_merge483
  %"hew_string_drop drop496" = load ptr, ptr %local_4, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop496")
  store ptr null, ptr %local_4, align 8
  %helper_crash_cleanup_drop_active497 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active497, label %helper_crash_cleanup_retire498, label %helper_crash_cleanup_retire_merge499

helper_crash_cleanup_retire_accepted494:          ; preds = %helper_crash_cleanup_retire490
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge491

helper_crash_cleanup_retire_rejected495:          ; preds = %helper_crash_cleanup_retire490
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire498:                   ; preds = %helper_crash_cleanup_retire_merge491
  %helper_crash_cleanup_retire_token500 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call501 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token500)
  br i1 %helper_crash_cleanup_retire_call501, label %helper_crash_cleanup_retire_accepted502, label %helper_crash_cleanup_retire_rejected503

helper_crash_cleanup_retire_merge499:             ; preds = %helper_crash_cleanup_retire_accepted502, %helper_crash_cleanup_retire_merge491
  %"hew_hashset_free_layout drop504" = load ptr, ptr %local_1, align 8
  call void @hew_hashset_free_layout(ptr %"hew_hashset_free_layout drop504")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire_accepted502:          ; preds = %helper_crash_cleanup_retire498
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge499

helper_crash_cleanup_retire_rejected503:          ; preds = %helper_crash_cleanup_retire498
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_deactivate513:               ; preds = %bb41
  %helper_crash_cleanup_token515 = load i64, ptr %helper_crash_cleanup_token_57, align 8
  %helper_crash_cleanup_deactivate_call516 = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token515)
  br i1 %helper_crash_cleanup_deactivate_call516, label %helper_crash_cleanup_deactivate_accepted517, label %helper_crash_cleanup_deactivate_rejected518

helper_crash_cleanup_deactivate_merge514:         ; preds = %helper_crash_cleanup_deactivate_accepted517, %bb41
  %move_load519 = load ptr, ptr %local_56, align 8
  store ptr %move_load519, ptr %local_57, align 8
  %helper_crash_cleanup_prior_token520 = load i64, ptr %helper_crash_cleanup_token_57, align 8
  %arm_typed_crash_cleanup521 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token520, ptr %local_57, i64 8, i64 8, ptr @__hew_frame_cleanup_5110292a480a33ee, i32 1, i32 0)
  %frame_cleanup_arm_failed522 = icmp eq i64 %arm_typed_crash_cleanup521, -1
  br i1 %frame_cleanup_arm_failed522, label %frame_cleanup_rejected524, label %frame_cleanup_registered523

helper_crash_cleanup_deactivate_accepted517:      ; preds = %helper_crash_cleanup_deactivate513
  store i1 false, ptr %helper_crash_cleanup_active_57, align 1
  br label %helper_crash_cleanup_deactivate_merge514

helper_crash_cleanup_deactivate_rejected518:      ; preds = %helper_crash_cleanup_deactivate513
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

frame_cleanup_registered523:                      ; preds = %helper_crash_cleanup_deactivate_merge514
  store i64 %arm_typed_crash_cleanup521, ptr %helper_crash_cleanup_token_57, align 8
  store i1 true, ptr %helper_crash_cleanup_active_57, align 1
  store i64 42, ptr %local_58, align 8
  %"hew_hashset_insert_layout arg0" = load ptr, ptr %local_57, align 8
  %hew_hashset_insert_layout_call = call i1 @hew_hashset_insert_layout(ptr %"hew_hashset_insert_layout arg0", ptr %local_58)
  %hashset_insert_bool_zext_i8 = zext i1 %hew_hashset_insert_layout_call to i8
  store i8 %hashset_insert_bool_zext_i8, ptr %local_59, align 1
  br label %bb42

frame_cleanup_rejected524:                        ; preds = %helper_crash_cleanup_deactivate_merge514
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_deactivate534:               ; preds = %bb47
  %helper_crash_cleanup_token536 = load i64, ptr %helper_crash_cleanup_token_66, align 8
  %helper_crash_cleanup_deactivate_call537 = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token536)
  br i1 %helper_crash_cleanup_deactivate_call537, label %helper_crash_cleanup_deactivate_accepted538, label %helper_crash_cleanup_deactivate_rejected539

helper_crash_cleanup_deactivate_merge535:         ; preds = %helper_crash_cleanup_deactivate_accepted538, %bb47
  %move_load540 = load ptr, ptr %local_65, align 8
  store ptr %move_load540, ptr %local_66, align 8
  %helper_crash_cleanup_prior_token541 = load i64, ptr %helper_crash_cleanup_token_66, align 8
  %arm_typed_crash_cleanup542 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token541, ptr %local_66, i64 8, i64 8, ptr @__hew_frame_cleanup_6e3157b8b1632579, i32 1, i32 0)
  %frame_cleanup_arm_failed543 = icmp eq i64 %arm_typed_crash_cleanup542, -1
  br i1 %frame_cleanup_arm_failed543, label %frame_cleanup_rejected545, label %frame_cleanup_registered544

helper_crash_cleanup_deactivate_accepted538:      ; preds = %helper_crash_cleanup_deactivate534
  store i1 false, ptr %helper_crash_cleanup_active_66, align 1
  br label %helper_crash_cleanup_deactivate_merge535

helper_crash_cleanup_deactivate_rejected539:      ; preds = %helper_crash_cleanup_deactivate534
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

frame_cleanup_registered544:                      ; preds = %helper_crash_cleanup_deactivate_merge535
  store i64 %arm_typed_crash_cleanup542, ptr %helper_crash_cleanup_token_66, align 8
  store i1 true, ptr %helper_crash_cleanup_active_66, align 1
  store ptr @str_lit.9, ptr %local_67, align 8
  %helper_crash_cleanup_was_active546 = load i1, ptr %helper_crash_cleanup_active_68, align 1
  br i1 %helper_crash_cleanup_was_active546, label %helper_crash_cleanup_deactivate547, label %helper_crash_cleanup_deactivate_merge548

frame_cleanup_rejected545:                        ; preds = %helper_crash_cleanup_deactivate_merge535
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_deactivate547:               ; preds = %frame_cleanup_registered544
  %helper_crash_cleanup_token549 = load i64, ptr %helper_crash_cleanup_token_68, align 8
  %helper_crash_cleanup_deactivate_call550 = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token549)
  br i1 %helper_crash_cleanup_deactivate_call550, label %helper_crash_cleanup_deactivate_accepted551, label %helper_crash_cleanup_deactivate_rejected552

helper_crash_cleanup_deactivate_merge548:         ; preds = %helper_crash_cleanup_deactivate_accepted551, %frame_cleanup_registered544
  %move_load553 = load ptr, ptr %local_67, align 8
  store ptr %move_load553, ptr %local_68, align 8
  %helper_crash_cleanup_prior_token554 = load i64, ptr %helper_crash_cleanup_token_68, align 8
  %arm_typed_crash_cleanup555 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token554, ptr %local_68, i64 8, i64 8, ptr @__hew_frame_cleanup_6e3157b8b1632579, i32 1, i32 0)
  %frame_cleanup_arm_failed556 = icmp eq i64 %arm_typed_crash_cleanup555, -1
  br i1 %frame_cleanup_arm_failed556, label %frame_cleanup_rejected558, label %frame_cleanup_registered557

helper_crash_cleanup_deactivate_accepted551:      ; preds = %helper_crash_cleanup_deactivate547
  store i1 false, ptr %helper_crash_cleanup_active_68, align 1
  br label %helper_crash_cleanup_deactivate_merge548

helper_crash_cleanup_deactivate_rejected552:      ; preds = %helper_crash_cleanup_deactivate547
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

frame_cleanup_registered557:                      ; preds = %helper_crash_cleanup_deactivate_merge548
  store i64 %arm_typed_crash_cleanup555, ptr %helper_crash_cleanup_token_68, align 8
  store i1 true, ptr %helper_crash_cleanup_active_68, align 1
  %"hew_string_concat arg0559" = load ptr, ptr %local_66, align 8
  %"hew_string_concat arg1560" = load ptr, ptr %local_68, align 8
  %hew_string_concat_call561 = call ptr @hew_string_concat(ptr %"hew_string_concat arg0559", ptr %"hew_string_concat arg1560")
  store ptr %hew_string_concat_call561, ptr %local_69, align 8
  %helper_crash_cleanup_was_active562 = load i1, ptr %helper_crash_cleanup_active_70, align 1
  br i1 %helper_crash_cleanup_was_active562, label %helper_crash_cleanup_deactivate563, label %helper_crash_cleanup_deactivate_merge564

frame_cleanup_rejected558:                        ; preds = %helper_crash_cleanup_deactivate_merge548
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_deactivate563:               ; preds = %frame_cleanup_registered557
  %helper_crash_cleanup_token565 = load i64, ptr %helper_crash_cleanup_token_70, align 8
  %helper_crash_cleanup_deactivate_call566 = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token565)
  br i1 %helper_crash_cleanup_deactivate_call566, label %helper_crash_cleanup_deactivate_accepted567, label %helper_crash_cleanup_deactivate_rejected568

helper_crash_cleanup_deactivate_merge564:         ; preds = %helper_crash_cleanup_deactivate_accepted567, %frame_cleanup_registered557
  %move_load569 = load ptr, ptr %local_69, align 8
  store ptr %move_load569, ptr %local_70, align 8
  %helper_crash_cleanup_prior_token570 = load i64, ptr %helper_crash_cleanup_token_70, align 8
  %arm_typed_crash_cleanup571 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token570, ptr %local_70, i64 8, i64 8, ptr @__hew_frame_cleanup_6e3157b8b1632579, i32 1, i32 0)
  %frame_cleanup_arm_failed572 = icmp eq i64 %arm_typed_crash_cleanup571, -1
  br i1 %frame_cleanup_arm_failed572, label %frame_cleanup_rejected574, label %frame_cleanup_registered573

helper_crash_cleanup_deactivate_accepted567:      ; preds = %helper_crash_cleanup_deactivate563
  store i1 false, ptr %helper_crash_cleanup_active_70, align 1
  br label %helper_crash_cleanup_deactivate_merge564

helper_crash_cleanup_deactivate_rejected568:      ; preds = %helper_crash_cleanup_deactivate563
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

frame_cleanup_registered573:                      ; preds = %helper_crash_cleanup_deactivate_merge564
  store i64 %arm_typed_crash_cleanup571, ptr %helper_crash_cleanup_token_70, align 8
  store i1 true, ptr %helper_crash_cleanup_active_70, align 1
  %print_arg575 = load ptr, ptr %local_70, align 8
  %print_str_bits = ptrtoint ptr %print_arg575 to i64
  call void @hew_print_value(i8 4, i64 %print_str_bits, i1 true)
  br label %bb48

frame_cleanup_rejected574:                        ; preds = %helper_crash_cleanup_deactivate_merge564
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire580:                   ; preds = %bb50
  %helper_crash_cleanup_retire_token582 = load i64, ptr %helper_crash_cleanup_token_70, align 8
  %helper_crash_cleanup_retire_call583 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token582)
  br i1 %helper_crash_cleanup_retire_call583, label %helper_crash_cleanup_retire_accepted584, label %helper_crash_cleanup_retire_rejected585

helper_crash_cleanup_retire_merge581:             ; preds = %helper_crash_cleanup_retire_accepted584, %bb50
  %"hew_string_drop drop586" = load ptr, ptr %local_70, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop586")
  store ptr null, ptr %local_70, align 8
  %helper_crash_cleanup_drop_active587 = load i1, ptr %helper_crash_cleanup_active_68, align 1
  br i1 %helper_crash_cleanup_drop_active587, label %helper_crash_cleanup_retire588, label %helper_crash_cleanup_retire_merge589

helper_crash_cleanup_retire_accepted584:          ; preds = %helper_crash_cleanup_retire580
  store i64 0, ptr %helper_crash_cleanup_token_70, align 8
  store i1 false, ptr %helper_crash_cleanup_active_70, align 1
  br label %helper_crash_cleanup_retire_merge581

helper_crash_cleanup_retire_rejected585:          ; preds = %helper_crash_cleanup_retire580
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire588:                   ; preds = %helper_crash_cleanup_retire_merge581
  %helper_crash_cleanup_retire_token590 = load i64, ptr %helper_crash_cleanup_token_68, align 8
  %helper_crash_cleanup_retire_call591 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token590)
  br i1 %helper_crash_cleanup_retire_call591, label %helper_crash_cleanup_retire_accepted592, label %helper_crash_cleanup_retire_rejected593

helper_crash_cleanup_retire_merge589:             ; preds = %helper_crash_cleanup_retire_accepted592, %helper_crash_cleanup_retire_merge581
  %"hew_string_drop drop594" = load ptr, ptr %local_68, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop594")
  store ptr null, ptr %local_68, align 8
  %helper_crash_cleanup_drop_active595 = load i1, ptr %helper_crash_cleanup_active_66, align 1
  br i1 %helper_crash_cleanup_drop_active595, label %helper_crash_cleanup_retire596, label %helper_crash_cleanup_retire_merge597

helper_crash_cleanup_retire_accepted592:          ; preds = %helper_crash_cleanup_retire588
  store i64 0, ptr %helper_crash_cleanup_token_68, align 8
  store i1 false, ptr %helper_crash_cleanup_active_68, align 1
  br label %helper_crash_cleanup_retire_merge589

helper_crash_cleanup_retire_rejected593:          ; preds = %helper_crash_cleanup_retire588
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire596:                   ; preds = %helper_crash_cleanup_retire_merge589
  %helper_crash_cleanup_retire_token598 = load i64, ptr %helper_crash_cleanup_token_66, align 8
  %helper_crash_cleanup_retire_call599 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token598)
  br i1 %helper_crash_cleanup_retire_call599, label %helper_crash_cleanup_retire_accepted600, label %helper_crash_cleanup_retire_rejected601

helper_crash_cleanup_retire_merge597:             ; preds = %helper_crash_cleanup_retire_accepted600, %helper_crash_cleanup_retire_merge589
  %"hew_string_drop drop602" = load ptr, ptr %local_66, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop602")
  store ptr null, ptr %local_66, align 8
  %helper_crash_cleanup_drop_active603 = load i1, ptr %helper_crash_cleanup_active_57, align 1
  br i1 %helper_crash_cleanup_drop_active603, label %helper_crash_cleanup_retire604, label %helper_crash_cleanup_retire_merge605

helper_crash_cleanup_retire_accepted600:          ; preds = %helper_crash_cleanup_retire596
  store i64 0, ptr %helper_crash_cleanup_token_66, align 8
  store i1 false, ptr %helper_crash_cleanup_active_66, align 1
  br label %helper_crash_cleanup_retire_merge597

helper_crash_cleanup_retire_rejected601:          ; preds = %helper_crash_cleanup_retire596
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire604:                   ; preds = %helper_crash_cleanup_retire_merge597
  %helper_crash_cleanup_retire_token606 = load i64, ptr %helper_crash_cleanup_token_57, align 8
  %helper_crash_cleanup_retire_call607 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token606)
  br i1 %helper_crash_cleanup_retire_call607, label %helper_crash_cleanup_retire_accepted608, label %helper_crash_cleanup_retire_rejected609

helper_crash_cleanup_retire_merge605:             ; preds = %helper_crash_cleanup_retire_accepted608, %helper_crash_cleanup_retire_merge597
  %"hew_hashset_free_layout drop610" = load ptr, ptr %local_57, align 8
  call void @hew_hashset_free_layout(ptr %"hew_hashset_free_layout drop610")
  store ptr null, ptr %local_57, align 8
  %helper_crash_cleanup_drop_active611 = load i1, ptr %helper_crash_cleanup_active_29, align 1
  br i1 %helper_crash_cleanup_drop_active611, label %helper_crash_cleanup_retire612, label %helper_crash_cleanup_retire_merge613

helper_crash_cleanup_retire_accepted608:          ; preds = %helper_crash_cleanup_retire604
  store i64 0, ptr %helper_crash_cleanup_token_57, align 8
  store i1 false, ptr %helper_crash_cleanup_active_57, align 1
  br label %helper_crash_cleanup_retire_merge605

helper_crash_cleanup_retire_rejected609:          ; preds = %helper_crash_cleanup_retire604
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire612:                   ; preds = %helper_crash_cleanup_retire_merge605
  %helper_crash_cleanup_retire_token614 = load i64, ptr %helper_crash_cleanup_token_29, align 8
  %helper_crash_cleanup_retire_call615 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token614)
  br i1 %helper_crash_cleanup_retire_call615, label %helper_crash_cleanup_retire_accepted616, label %helper_crash_cleanup_retire_rejected617

helper_crash_cleanup_retire_merge613:             ; preds = %helper_crash_cleanup_retire_accepted616, %helper_crash_cleanup_retire_merge605
  %"hew_hashmap_free_layout drop618" = load ptr, ptr %local_29, align 8
  call void @hew_hashmap_free_layout(ptr %"hew_hashmap_free_layout drop618")
  store ptr null, ptr %local_29, align 8
  %helper_crash_cleanup_drop_active619 = load i1, ptr %helper_crash_cleanup_active_10, align 1
  br i1 %helper_crash_cleanup_drop_active619, label %helper_crash_cleanup_retire620, label %helper_crash_cleanup_retire_merge621

helper_crash_cleanup_retire_accepted616:          ; preds = %helper_crash_cleanup_retire612
  store i64 0, ptr %helper_crash_cleanup_token_29, align 8
  store i1 false, ptr %helper_crash_cleanup_active_29, align 1
  br label %helper_crash_cleanup_retire_merge613

helper_crash_cleanup_retire_rejected617:          ; preds = %helper_crash_cleanup_retire612
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire620:                   ; preds = %helper_crash_cleanup_retire_merge613
  %helper_crash_cleanup_retire_token622 = load i64, ptr %helper_crash_cleanup_token_10, align 8
  %helper_crash_cleanup_retire_call623 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token622)
  br i1 %helper_crash_cleanup_retire_call623, label %helper_crash_cleanup_retire_accepted624, label %helper_crash_cleanup_retire_rejected625

helper_crash_cleanup_retire_merge621:             ; preds = %helper_crash_cleanup_retire_accepted624, %helper_crash_cleanup_retire_merge613
  %"hew_vec_free drop626" = load ptr, ptr %local_10, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop626")
  store ptr null, ptr %local_10, align 8
  %helper_crash_cleanup_drop_active627 = load i1, ptr %helper_crash_cleanup_active_7, align 1
  br i1 %helper_crash_cleanup_drop_active627, label %helper_crash_cleanup_retire628, label %helper_crash_cleanup_retire_merge629

helper_crash_cleanup_retire_accepted624:          ; preds = %helper_crash_cleanup_retire620
  store i64 0, ptr %helper_crash_cleanup_token_10, align 8
  store i1 false, ptr %helper_crash_cleanup_active_10, align 1
  br label %helper_crash_cleanup_retire_merge621

helper_crash_cleanup_retire_rejected625:          ; preds = %helper_crash_cleanup_retire620
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire628:                   ; preds = %helper_crash_cleanup_retire_merge621
  %helper_crash_cleanup_retire_token630 = load i64, ptr %helper_crash_cleanup_token_7, align 8
  %helper_crash_cleanup_retire_call631 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token630)
  br i1 %helper_crash_cleanup_retire_call631, label %helper_crash_cleanup_retire_accepted632, label %helper_crash_cleanup_retire_rejected633

helper_crash_cleanup_retire_merge629:             ; preds = %helper_crash_cleanup_retire_accepted632, %helper_crash_cleanup_retire_merge621
  %"hew_string_drop drop634" = load ptr, ptr %local_7, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop634")
  store ptr null, ptr %local_7, align 8
  %helper_crash_cleanup_drop_active635 = load i1, ptr %helper_crash_cleanup_active_4, align 1
  br i1 %helper_crash_cleanup_drop_active635, label %helper_crash_cleanup_retire636, label %helper_crash_cleanup_retire_merge637

helper_crash_cleanup_retire_accepted632:          ; preds = %helper_crash_cleanup_retire628
  store i64 0, ptr %helper_crash_cleanup_token_7, align 8
  store i1 false, ptr %helper_crash_cleanup_active_7, align 1
  br label %helper_crash_cleanup_retire_merge629

helper_crash_cleanup_retire_rejected633:          ; preds = %helper_crash_cleanup_retire628
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire636:                   ; preds = %helper_crash_cleanup_retire_merge629
  %helper_crash_cleanup_retire_token638 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_retire_call639 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token638)
  br i1 %helper_crash_cleanup_retire_call639, label %helper_crash_cleanup_retire_accepted640, label %helper_crash_cleanup_retire_rejected641

helper_crash_cleanup_retire_merge637:             ; preds = %helper_crash_cleanup_retire_accepted640, %helper_crash_cleanup_retire_merge629
  %"hew_string_drop drop642" = load ptr, ptr %local_4, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop642")
  store ptr null, ptr %local_4, align 8
  %helper_crash_cleanup_drop_active643 = load i1, ptr %helper_crash_cleanup_active_1, align 1
  br i1 %helper_crash_cleanup_drop_active643, label %helper_crash_cleanup_retire644, label %helper_crash_cleanup_retire_merge645

helper_crash_cleanup_retire_accepted640:          ; preds = %helper_crash_cleanup_retire636
  store i64 0, ptr %helper_crash_cleanup_token_4, align 8
  store i1 false, ptr %helper_crash_cleanup_active_4, align 1
  br label %helper_crash_cleanup_retire_merge637

helper_crash_cleanup_retire_rejected641:          ; preds = %helper_crash_cleanup_retire636
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire644:                   ; preds = %helper_crash_cleanup_retire_merge637
  %helper_crash_cleanup_retire_token646 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_retire_call647 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token646)
  br i1 %helper_crash_cleanup_retire_call647, label %helper_crash_cleanup_retire_accepted648, label %helper_crash_cleanup_retire_rejected649

helper_crash_cleanup_retire_merge645:             ; preds = %helper_crash_cleanup_retire_accepted648, %helper_crash_cleanup_retire_merge637
  %"hew_hashset_free_layout drop650" = load ptr, ptr %local_1, align 8
  call void @hew_hashset_free_layout(ptr %"hew_hashset_free_layout drop650")
  store ptr null, ptr %local_1, align 8
  %helper_crash_cleanup_return_token_1 = load i64, ptr %helper_crash_cleanup_token_1, align 8
  %helper_crash_cleanup_return_has_token_1 = icmp ne i64 %helper_crash_cleanup_return_token_1, 0
  br i1 %helper_crash_cleanup_return_has_token_1, label %helper_crash_cleanup_return_retire_1, label %helper_crash_cleanup_return_merge_1

helper_crash_cleanup_retire_accepted648:          ; preds = %helper_crash_cleanup_retire644
  store i64 0, ptr %helper_crash_cleanup_token_1, align 8
  store i1 false, ptr %helper_crash_cleanup_active_1, align 1
  br label %helper_crash_cleanup_retire_merge645

helper_crash_cleanup_retire_rejected649:          ; preds = %helper_crash_cleanup_retire644
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_return_merge_1:              ; preds = %helper_crash_cleanup_return_retire_1_accepted, %helper_crash_cleanup_retire_merge645
  %helper_crash_cleanup_return_token_4 = load i64, ptr %helper_crash_cleanup_token_4, align 8
  %helper_crash_cleanup_return_has_token_4 = icmp ne i64 %helper_crash_cleanup_return_token_4, 0
  br i1 %helper_crash_cleanup_return_has_token_4, label %helper_crash_cleanup_return_retire_4, label %helper_crash_cleanup_return_merge_4

helper_crash_cleanup_return_retire_1:             ; preds = %helper_crash_cleanup_retire_merge645
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
  %hew_runtime_exit_status_call651 = call i32 @hew_runtime_exit_status()
  %hew_runtime_faulted652 = icmp ne i32 %hew_runtime_exit_status_call651, 0
  %hew_exit_any_failed = or i1 %hew_lambda_drain_failed, %hew_runtime_faulted652
  br i1 %hew_exit_any_failed, label %hew_shutdown_exit_failed, label %hew_shutdown_exit_continue

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

declare i32 @hew_runtime_exit_status()

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
