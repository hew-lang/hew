; ModuleID = 'mir_lifo_drop_plan'
source_filename = "mir_lifo_drop_plan"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "aarch64-apple-macosx13.0"

@str_lit = private unnamed_addr constant [4 x i8] c"one\00", align 1
@str_lit.1 = private unnamed_addr constant [2 x i8] c"1\00", align 1
@str_lit.2 = private unnamed_addr constant [4 x i8] c"two\00", align 1
@str_lit.3 = private unnamed_addr constant [2 x i8] c"2\00", align 1
@str_lit.4 = private unnamed_addr constant [6 x i8] c"three\00", align 1
@str_lit.5 = private unnamed_addr constant [2 x i8] c"3\00", align 1
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

define internal i64 @measure(ptr %0) {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca i64, align 8
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result = call i32 @hew_string_length(ptr %call_arg)
  %ffi_sext = sext i32 %call_result to i64
  store i64 %ffi_sext, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load i64, ptr %local_1, align 8
  store i64 %move_load, ptr %return_slot, align 8
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val

cancel_exit:                                      ; preds = %entry
  ret i64 0

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define i64 @main() {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca ptr, align 8
  %local_3 = alloca ptr, align 8
  %local_4 = alloca ptr, align 8
  %local_5 = alloca ptr, align 8
  %local_6 = alloca ptr, align 8
  %local_7 = alloca ptr, align 8
  %local_8 = alloca ptr, align 8
  %local_9 = alloca ptr, align 8
  %local_10 = alloca ptr, align 8
  %local_11 = alloca ptr, align 8
  %local_12 = alloca i64, align 8
  %local_13 = alloca i64, align 8
  %local_14 = alloca i64, align 8
  %local_15 = alloca i8, align 1
  %local_16 = alloca i64, align 8
  %local_17 = alloca i64, align 8
  %local_18 = alloca i8, align 1
  %local_19 = alloca i64, align 8
  %helper_crash_cleanup_token_3 = alloca i64, align 8
  store i64 0, ptr %helper_crash_cleanup_token_3, align 8
  %helper_crash_cleanup_active_3 = alloca i1, align 1
  store i1 false, ptr %helper_crash_cleanup_active_3, align 1
  %helper_crash_cleanup_token_7 = alloca i64, align 8
  store i64 0, ptr %helper_crash_cleanup_token_7, align 8
  %helper_crash_cleanup_active_7 = alloca i1, align 1
  store i1 false, ptr %helper_crash_cleanup_active_7, align 1
  %helper_crash_cleanup_token_11 = alloca i64, align 8
  store i64 0, ptr %helper_crash_cleanup_token_11, align 8
  %helper_crash_cleanup_active_11 = alloca i1, align 1
  store i1 false, ptr %helper_crash_cleanup_active_11, align 1
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  store ptr @str_lit, ptr %local_0, align 8
  store ptr @str_lit.1, ptr %local_1, align 8
  %"hew_string_concat arg0" = load ptr, ptr %local_0, align 8
  %"hew_string_concat arg1" = load ptr, ptr %local_1, align 8
  %hew_string_concat_call = call ptr @hew_string_concat(ptr %"hew_string_concat arg0", ptr %"hew_string_concat arg1")
  store ptr %hew_string_concat_call, ptr %local_2, align 8
  %helper_crash_cleanup_was_active = load i1, ptr %helper_crash_cleanup_active_3, align 1
  br i1 %helper_crash_cleanup_was_active, label %helper_crash_cleanup_deactivate, label %helper_crash_cleanup_deactivate_merge

bb1:                                              ; preds = %frame_cleanup_registered31
  %call_arg33 = load ptr, ptr %local_7, align 8
  %call_result34 = call i64 @measure(ptr %call_arg33)
  store i64 %call_result34, ptr %local_13, align 8
  br label %bb2

bb2:                                              ; preds = %bb1
  %checked_lhs = load i64, ptr %local_12, align 8
  %checked_rhs = load i64, ptr %local_13, align 8
  %with_overflow = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_14, align 8
  store i8 %checked_overflow_widen, ptr %local_15, align 1
  %cond_load = load i8, ptr %local_15, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb3, label %bb4

bb3:                                              ; preds = %bb2
  %helper_crash_cleanup_drop_active = load i1, ptr %helper_crash_cleanup_active_11, align 1
  br i1 %helper_crash_cleanup_drop_active, label %helper_crash_cleanup_retire, label %helper_crash_cleanup_retire_merge

bb4:                                              ; preds = %bb2
  %call_arg51 = load ptr, ptr %local_11, align 8
  %call_result52 = call i64 @measure(ptr %call_arg51)
  store i64 %call_result52, ptr %local_16, align 8
  br label %bb5

bb5:                                              ; preds = %bb4
  %checked_lhs53 = load i64, ptr %local_14, align 8
  %checked_rhs54 = load i64, ptr %local_16, align 8
  %with_overflow55 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs53, i64 %checked_rhs54)
  %checked_result56 = extractvalue { i64, i1 } %with_overflow55, 0
  %checked_overflow57 = extractvalue { i64, i1 } %with_overflow55, 1
  %checked_overflow_widen58 = zext i1 %checked_overflow57 to i8
  store i64 %checked_result56, ptr %local_17, align 8
  store i8 %checked_overflow_widen58, ptr %local_18, align 1
  %cond_load59 = load i8, ptr %local_18, align 1
  %cond_nz60 = icmp ne i8 %cond_load59, 0
  br i1 %cond_nz60, label %bb6, label %bb7

bb6:                                              ; preds = %bb5
  %helper_crash_cleanup_drop_active61 = load i1, ptr %helper_crash_cleanup_active_11, align 1
  br i1 %helper_crash_cleanup_drop_active61, label %helper_crash_cleanup_retire62, label %helper_crash_cleanup_retire_merge63

bb7:                                              ; preds = %bb5
  %move_load85 = load i64, ptr %local_17, align 8
  store i64 %move_load85, ptr %local_19, align 8
  %print_arg = load i64, ptr %local_19, align 8
  call void @hew_print_value(i8 1, i64 %print_arg, i1 true)
  br label %bb8

bb8:                                              ; preds = %bb7
  %move_load86 = load i64, ptr %local_19, align 8
  store i64 %move_load86, ptr %return_slot, align 8
  %helper_crash_cleanup_drop_active87 = load i1, ptr %helper_crash_cleanup_active_11, align 1
  br i1 %helper_crash_cleanup_drop_active87, label %helper_crash_cleanup_retire88, label %helper_crash_cleanup_retire_merge89

cancel_exit:                                      ; preds = %entry
  ret i64 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

helper_crash_cleanup_deactivate:                  ; preds = %bb0
  %helper_crash_cleanup_token = load i64, ptr %helper_crash_cleanup_token_3, align 8
  %helper_crash_cleanup_deactivate_call = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token)
  br i1 %helper_crash_cleanup_deactivate_call, label %helper_crash_cleanup_deactivate_accepted, label %helper_crash_cleanup_deactivate_rejected

helper_crash_cleanup_deactivate_merge:            ; preds = %helper_crash_cleanup_deactivate_accepted, %bb0
  %move_load = load ptr, ptr %local_2, align 8
  store ptr %move_load, ptr %local_3, align 8
  %helper_crash_cleanup_prior_token = load i64, ptr %helper_crash_cleanup_token_3, align 8
  %arm_typed_crash_cleanup = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token, ptr %local_3, i64 8, i64 8, ptr @__hew_frame_cleanup_6e3157b8b1632579, i32 1, i32 0)
  %frame_cleanup_arm_failed = icmp eq i64 %arm_typed_crash_cleanup, -1
  br i1 %frame_cleanup_arm_failed, label %frame_cleanup_rejected, label %frame_cleanup_registered

helper_crash_cleanup_deactivate_accepted:         ; preds = %helper_crash_cleanup_deactivate
  store i1 false, ptr %helper_crash_cleanup_active_3, align 1
  br label %helper_crash_cleanup_deactivate_merge

helper_crash_cleanup_deactivate_rejected:         ; preds = %helper_crash_cleanup_deactivate
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

frame_cleanup_registered:                         ; preds = %helper_crash_cleanup_deactivate_merge
  store i64 %arm_typed_crash_cleanup, ptr %helper_crash_cleanup_token_3, align 8
  store i1 true, ptr %helper_crash_cleanup_active_3, align 1
  store ptr @str_lit.2, ptr %local_4, align 8
  store ptr @str_lit.3, ptr %local_5, align 8
  %"hew_string_concat arg01" = load ptr, ptr %local_4, align 8
  %"hew_string_concat arg12" = load ptr, ptr %local_5, align 8
  %hew_string_concat_call3 = call ptr @hew_string_concat(ptr %"hew_string_concat arg01", ptr %"hew_string_concat arg12")
  store ptr %hew_string_concat_call3, ptr %local_6, align 8
  %helper_crash_cleanup_was_active4 = load i1, ptr %helper_crash_cleanup_active_7, align 1
  br i1 %helper_crash_cleanup_was_active4, label %helper_crash_cleanup_deactivate5, label %helper_crash_cleanup_deactivate_merge6

frame_cleanup_rejected:                           ; preds = %helper_crash_cleanup_deactivate_merge
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_deactivate5:                 ; preds = %frame_cleanup_registered
  %helper_crash_cleanup_token7 = load i64, ptr %helper_crash_cleanup_token_7, align 8
  %helper_crash_cleanup_deactivate_call8 = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token7)
  br i1 %helper_crash_cleanup_deactivate_call8, label %helper_crash_cleanup_deactivate_accepted9, label %helper_crash_cleanup_deactivate_rejected10

helper_crash_cleanup_deactivate_merge6:           ; preds = %helper_crash_cleanup_deactivate_accepted9, %frame_cleanup_registered
  %move_load11 = load ptr, ptr %local_6, align 8
  store ptr %move_load11, ptr %local_7, align 8
  %helper_crash_cleanup_prior_token12 = load i64, ptr %helper_crash_cleanup_token_7, align 8
  %arm_typed_crash_cleanup13 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token12, ptr %local_7, i64 8, i64 8, ptr @__hew_frame_cleanup_6e3157b8b1632579, i32 1, i32 0)
  %frame_cleanup_arm_failed14 = icmp eq i64 %arm_typed_crash_cleanup13, -1
  br i1 %frame_cleanup_arm_failed14, label %frame_cleanup_rejected16, label %frame_cleanup_registered15

helper_crash_cleanup_deactivate_accepted9:        ; preds = %helper_crash_cleanup_deactivate5
  store i1 false, ptr %helper_crash_cleanup_active_7, align 1
  br label %helper_crash_cleanup_deactivate_merge6

helper_crash_cleanup_deactivate_rejected10:       ; preds = %helper_crash_cleanup_deactivate5
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

frame_cleanup_registered15:                       ; preds = %helper_crash_cleanup_deactivate_merge6
  store i64 %arm_typed_crash_cleanup13, ptr %helper_crash_cleanup_token_7, align 8
  store i1 true, ptr %helper_crash_cleanup_active_7, align 1
  store ptr @str_lit.4, ptr %local_8, align 8
  store ptr @str_lit.5, ptr %local_9, align 8
  %"hew_string_concat arg017" = load ptr, ptr %local_8, align 8
  %"hew_string_concat arg118" = load ptr, ptr %local_9, align 8
  %hew_string_concat_call19 = call ptr @hew_string_concat(ptr %"hew_string_concat arg017", ptr %"hew_string_concat arg118")
  store ptr %hew_string_concat_call19, ptr %local_10, align 8
  %helper_crash_cleanup_was_active20 = load i1, ptr %helper_crash_cleanup_active_11, align 1
  br i1 %helper_crash_cleanup_was_active20, label %helper_crash_cleanup_deactivate21, label %helper_crash_cleanup_deactivate_merge22

frame_cleanup_rejected16:                         ; preds = %helper_crash_cleanup_deactivate_merge6
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_deactivate21:                ; preds = %frame_cleanup_registered15
  %helper_crash_cleanup_token23 = load i64, ptr %helper_crash_cleanup_token_11, align 8
  %helper_crash_cleanup_deactivate_call24 = call i1 @hew_cont_crash_cleanup_deactivate(i64 %helper_crash_cleanup_token23)
  br i1 %helper_crash_cleanup_deactivate_call24, label %helper_crash_cleanup_deactivate_accepted25, label %helper_crash_cleanup_deactivate_rejected26

helper_crash_cleanup_deactivate_merge22:          ; preds = %helper_crash_cleanup_deactivate_accepted25, %frame_cleanup_registered15
  %move_load27 = load ptr, ptr %local_10, align 8
  store ptr %move_load27, ptr %local_11, align 8
  %helper_crash_cleanup_prior_token28 = load i64, ptr %helper_crash_cleanup_token_11, align 8
  %arm_typed_crash_cleanup29 = call i64 @hew_cont_crash_cleanup_arm(i64 %helper_crash_cleanup_prior_token28, ptr %local_11, i64 8, i64 8, ptr @__hew_frame_cleanup_6e3157b8b1632579, i32 1, i32 0)
  %frame_cleanup_arm_failed30 = icmp eq i64 %arm_typed_crash_cleanup29, -1
  br i1 %frame_cleanup_arm_failed30, label %frame_cleanup_rejected32, label %frame_cleanup_registered31

helper_crash_cleanup_deactivate_accepted25:       ; preds = %helper_crash_cleanup_deactivate21
  store i1 false, ptr %helper_crash_cleanup_active_11, align 1
  br label %helper_crash_cleanup_deactivate_merge22

helper_crash_cleanup_deactivate_rejected26:       ; preds = %helper_crash_cleanup_deactivate21
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

frame_cleanup_registered31:                       ; preds = %helper_crash_cleanup_deactivate_merge22
  store i64 %arm_typed_crash_cleanup29, ptr %helper_crash_cleanup_token_11, align 8
  store i1 true, ptr %helper_crash_cleanup_active_11, align 1
  %call_arg = load ptr, ptr %local_3, align 8
  %call_result = call i64 @measure(ptr %call_arg)
  store i64 %call_result, ptr %local_12, align 8
  br label %bb1

frame_cleanup_rejected32:                         ; preds = %helper_crash_cleanup_deactivate_merge22
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire:                      ; preds = %bb3
  %helper_crash_cleanup_retire_token = load i64, ptr %helper_crash_cleanup_token_11, align 8
  %helper_crash_cleanup_retire_call = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token)
  br i1 %helper_crash_cleanup_retire_call, label %helper_crash_cleanup_retire_accepted, label %helper_crash_cleanup_retire_rejected

helper_crash_cleanup_retire_merge:                ; preds = %helper_crash_cleanup_retire_accepted, %bb3
  %"hew_string_drop drop" = load ptr, ptr %local_11, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop")
  store ptr null, ptr %local_11, align 8
  %helper_crash_cleanup_drop_active35 = load i1, ptr %helper_crash_cleanup_active_7, align 1
  br i1 %helper_crash_cleanup_drop_active35, label %helper_crash_cleanup_retire36, label %helper_crash_cleanup_retire_merge37

helper_crash_cleanup_retire_accepted:             ; preds = %helper_crash_cleanup_retire
  store i64 0, ptr %helper_crash_cleanup_token_11, align 8
  store i1 false, ptr %helper_crash_cleanup_active_11, align 1
  br label %helper_crash_cleanup_retire_merge

helper_crash_cleanup_retire_rejected:             ; preds = %helper_crash_cleanup_retire
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire36:                    ; preds = %helper_crash_cleanup_retire_merge
  %helper_crash_cleanup_retire_token38 = load i64, ptr %helper_crash_cleanup_token_7, align 8
  %helper_crash_cleanup_retire_call39 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token38)
  br i1 %helper_crash_cleanup_retire_call39, label %helper_crash_cleanup_retire_accepted40, label %helper_crash_cleanup_retire_rejected41

helper_crash_cleanup_retire_merge37:              ; preds = %helper_crash_cleanup_retire_accepted40, %helper_crash_cleanup_retire_merge
  %"hew_string_drop drop42" = load ptr, ptr %local_7, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop42")
  store ptr null, ptr %local_7, align 8
  %helper_crash_cleanup_drop_active43 = load i1, ptr %helper_crash_cleanup_active_3, align 1
  br i1 %helper_crash_cleanup_drop_active43, label %helper_crash_cleanup_retire44, label %helper_crash_cleanup_retire_merge45

helper_crash_cleanup_retire_accepted40:           ; preds = %helper_crash_cleanup_retire36
  store i64 0, ptr %helper_crash_cleanup_token_7, align 8
  store i1 false, ptr %helper_crash_cleanup_active_7, align 1
  br label %helper_crash_cleanup_retire_merge37

helper_crash_cleanup_retire_rejected41:           ; preds = %helper_crash_cleanup_retire36
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire44:                    ; preds = %helper_crash_cleanup_retire_merge37
  %helper_crash_cleanup_retire_token46 = load i64, ptr %helper_crash_cleanup_token_3, align 8
  %helper_crash_cleanup_retire_call47 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token46)
  br i1 %helper_crash_cleanup_retire_call47, label %helper_crash_cleanup_retire_accepted48, label %helper_crash_cleanup_retire_rejected49

helper_crash_cleanup_retire_merge45:              ; preds = %helper_crash_cleanup_retire_accepted48, %helper_crash_cleanup_retire_merge37
  %"hew_string_drop drop50" = load ptr, ptr %local_3, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop50")
  store ptr null, ptr %local_3, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire_accepted48:           ; preds = %helper_crash_cleanup_retire44
  store i64 0, ptr %helper_crash_cleanup_token_3, align 8
  store i1 false, ptr %helper_crash_cleanup_active_3, align 1
  br label %helper_crash_cleanup_retire_merge45

helper_crash_cleanup_retire_rejected49:           ; preds = %helper_crash_cleanup_retire44
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire62:                    ; preds = %bb6
  %helper_crash_cleanup_retire_token64 = load i64, ptr %helper_crash_cleanup_token_11, align 8
  %helper_crash_cleanup_retire_call65 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token64)
  br i1 %helper_crash_cleanup_retire_call65, label %helper_crash_cleanup_retire_accepted66, label %helper_crash_cleanup_retire_rejected67

helper_crash_cleanup_retire_merge63:              ; preds = %helper_crash_cleanup_retire_accepted66, %bb6
  %"hew_string_drop drop68" = load ptr, ptr %local_11, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop68")
  store ptr null, ptr %local_11, align 8
  %helper_crash_cleanup_drop_active69 = load i1, ptr %helper_crash_cleanup_active_7, align 1
  br i1 %helper_crash_cleanup_drop_active69, label %helper_crash_cleanup_retire70, label %helper_crash_cleanup_retire_merge71

helper_crash_cleanup_retire_accepted66:           ; preds = %helper_crash_cleanup_retire62
  store i64 0, ptr %helper_crash_cleanup_token_11, align 8
  store i1 false, ptr %helper_crash_cleanup_active_11, align 1
  br label %helper_crash_cleanup_retire_merge63

helper_crash_cleanup_retire_rejected67:           ; preds = %helper_crash_cleanup_retire62
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire70:                    ; preds = %helper_crash_cleanup_retire_merge63
  %helper_crash_cleanup_retire_token72 = load i64, ptr %helper_crash_cleanup_token_7, align 8
  %helper_crash_cleanup_retire_call73 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token72)
  br i1 %helper_crash_cleanup_retire_call73, label %helper_crash_cleanup_retire_accepted74, label %helper_crash_cleanup_retire_rejected75

helper_crash_cleanup_retire_merge71:              ; preds = %helper_crash_cleanup_retire_accepted74, %helper_crash_cleanup_retire_merge63
  %"hew_string_drop drop76" = load ptr, ptr %local_7, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop76")
  store ptr null, ptr %local_7, align 8
  %helper_crash_cleanup_drop_active77 = load i1, ptr %helper_crash_cleanup_active_3, align 1
  br i1 %helper_crash_cleanup_drop_active77, label %helper_crash_cleanup_retire78, label %helper_crash_cleanup_retire_merge79

helper_crash_cleanup_retire_accepted74:           ; preds = %helper_crash_cleanup_retire70
  store i64 0, ptr %helper_crash_cleanup_token_7, align 8
  store i1 false, ptr %helper_crash_cleanup_active_7, align 1
  br label %helper_crash_cleanup_retire_merge71

helper_crash_cleanup_retire_rejected75:           ; preds = %helper_crash_cleanup_retire70
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire78:                    ; preds = %helper_crash_cleanup_retire_merge71
  %helper_crash_cleanup_retire_token80 = load i64, ptr %helper_crash_cleanup_token_3, align 8
  %helper_crash_cleanup_retire_call81 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token80)
  br i1 %helper_crash_cleanup_retire_call81, label %helper_crash_cleanup_retire_accepted82, label %helper_crash_cleanup_retire_rejected83

helper_crash_cleanup_retire_merge79:              ; preds = %helper_crash_cleanup_retire_accepted82, %helper_crash_cleanup_retire_merge71
  %"hew_string_drop drop84" = load ptr, ptr %local_3, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop84")
  store ptr null, ptr %local_3, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire_accepted82:           ; preds = %helper_crash_cleanup_retire78
  store i64 0, ptr %helper_crash_cleanup_token_3, align 8
  store i1 false, ptr %helper_crash_cleanup_active_3, align 1
  br label %helper_crash_cleanup_retire_merge79

helper_crash_cleanup_retire_rejected83:           ; preds = %helper_crash_cleanup_retire78
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire88:                    ; preds = %bb8
  %helper_crash_cleanup_retire_token90 = load i64, ptr %helper_crash_cleanup_token_11, align 8
  %helper_crash_cleanup_retire_call91 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token90)
  br i1 %helper_crash_cleanup_retire_call91, label %helper_crash_cleanup_retire_accepted92, label %helper_crash_cleanup_retire_rejected93

helper_crash_cleanup_retire_merge89:              ; preds = %helper_crash_cleanup_retire_accepted92, %bb8
  %"hew_string_drop drop94" = load ptr, ptr %local_11, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop94")
  store ptr null, ptr %local_11, align 8
  %helper_crash_cleanup_drop_active95 = load i1, ptr %helper_crash_cleanup_active_7, align 1
  br i1 %helper_crash_cleanup_drop_active95, label %helper_crash_cleanup_retire96, label %helper_crash_cleanup_retire_merge97

helper_crash_cleanup_retire_accepted92:           ; preds = %helper_crash_cleanup_retire88
  store i64 0, ptr %helper_crash_cleanup_token_11, align 8
  store i1 false, ptr %helper_crash_cleanup_active_11, align 1
  br label %helper_crash_cleanup_retire_merge89

helper_crash_cleanup_retire_rejected93:           ; preds = %helper_crash_cleanup_retire88
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire96:                    ; preds = %helper_crash_cleanup_retire_merge89
  %helper_crash_cleanup_retire_token98 = load i64, ptr %helper_crash_cleanup_token_7, align 8
  %helper_crash_cleanup_retire_call99 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token98)
  br i1 %helper_crash_cleanup_retire_call99, label %helper_crash_cleanup_retire_accepted100, label %helper_crash_cleanup_retire_rejected101

helper_crash_cleanup_retire_merge97:              ; preds = %helper_crash_cleanup_retire_accepted100, %helper_crash_cleanup_retire_merge89
  %"hew_string_drop drop102" = load ptr, ptr %local_7, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop102")
  store ptr null, ptr %local_7, align 8
  %helper_crash_cleanup_drop_active103 = load i1, ptr %helper_crash_cleanup_active_3, align 1
  br i1 %helper_crash_cleanup_drop_active103, label %helper_crash_cleanup_retire104, label %helper_crash_cleanup_retire_merge105

helper_crash_cleanup_retire_accepted100:          ; preds = %helper_crash_cleanup_retire96
  store i64 0, ptr %helper_crash_cleanup_token_7, align 8
  store i1 false, ptr %helper_crash_cleanup_active_7, align 1
  br label %helper_crash_cleanup_retire_merge97

helper_crash_cleanup_retire_rejected101:          ; preds = %helper_crash_cleanup_retire96
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_retire104:                   ; preds = %helper_crash_cleanup_retire_merge97
  %helper_crash_cleanup_retire_token106 = load i64, ptr %helper_crash_cleanup_token_3, align 8
  %helper_crash_cleanup_retire_call107 = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_retire_token106)
  br i1 %helper_crash_cleanup_retire_call107, label %helper_crash_cleanup_retire_accepted108, label %helper_crash_cleanup_retire_rejected109

helper_crash_cleanup_retire_merge105:             ; preds = %helper_crash_cleanup_retire_accepted108, %helper_crash_cleanup_retire_merge97
  %"hew_string_drop drop110" = load ptr, ptr %local_3, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop110")
  store ptr null, ptr %local_3, align 8
  %helper_crash_cleanup_return_token_3 = load i64, ptr %helper_crash_cleanup_token_3, align 8
  %helper_crash_cleanup_return_has_token_3 = icmp ne i64 %helper_crash_cleanup_return_token_3, 0
  br i1 %helper_crash_cleanup_return_has_token_3, label %helper_crash_cleanup_return_retire_3, label %helper_crash_cleanup_return_merge_3

helper_crash_cleanup_retire_accepted108:          ; preds = %helper_crash_cleanup_retire104
  store i64 0, ptr %helper_crash_cleanup_token_3, align 8
  store i1 false, ptr %helper_crash_cleanup_active_3, align 1
  br label %helper_crash_cleanup_retire_merge105

helper_crash_cleanup_retire_rejected109:          ; preds = %helper_crash_cleanup_retire104
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_return_merge_3:              ; preds = %helper_crash_cleanup_return_retire_3_accepted, %helper_crash_cleanup_retire_merge105
  %helper_crash_cleanup_return_token_7 = load i64, ptr %helper_crash_cleanup_token_7, align 8
  %helper_crash_cleanup_return_has_token_7 = icmp ne i64 %helper_crash_cleanup_return_token_7, 0
  br i1 %helper_crash_cleanup_return_has_token_7, label %helper_crash_cleanup_return_retire_7, label %helper_crash_cleanup_return_merge_7

helper_crash_cleanup_return_retire_3:             ; preds = %helper_crash_cleanup_retire_merge105
  %helper_crash_cleanup_return_retire_3_call = call i1 @hew_cont_crash_cleanup_retire(i64 %helper_crash_cleanup_return_token_3)
  br i1 %helper_crash_cleanup_return_retire_3_call, label %helper_crash_cleanup_return_retire_3_accepted, label %helper_crash_cleanup_return_retire_3_rejected

helper_crash_cleanup_return_retire_3_accepted:    ; preds = %helper_crash_cleanup_return_retire_3
  store i64 0, ptr %helper_crash_cleanup_token_3, align 8
  store i1 false, ptr %helper_crash_cleanup_active_3, align 1
  br label %helper_crash_cleanup_return_merge_3

helper_crash_cleanup_return_retire_3_rejected:    ; preds = %helper_crash_cleanup_return_retire_3
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

helper_crash_cleanup_return_merge_7:              ; preds = %helper_crash_cleanup_return_retire_7_accepted, %helper_crash_cleanup_return_merge_3
  %helper_crash_cleanup_return_token_11 = load i64, ptr %helper_crash_cleanup_token_11, align 8
  %helper_crash_cleanup_return_has_token_11 = icmp ne i64 %helper_crash_cleanup_return_token_11, 0
  br i1 %helper_crash_cleanup_return_has_token_11, label %helper_crash_cleanup_return_retire_11, label %helper_crash_cleanup_return_merge_11

helper_crash_cleanup_return_retire_7:             ; preds = %helper_crash_cleanup_return_merge_3
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

helper_crash_cleanup_return_merge_11:             ; preds = %helper_crash_cleanup_return_retire_11_accepted, %helper_crash_cleanup_return_merge_7
  %hew_lambda_drain_all_call = call i32 @hew_lambda_drain_all(i64 0)
  %hew_lambda_drain_failed = icmp ne i32 %hew_lambda_drain_all_call, 0
  br i1 %hew_lambda_drain_failed, label %hew_shutdown_exit_failed, label %hew_shutdown_exit_continue

helper_crash_cleanup_return_retire_11:            ; preds = %helper_crash_cleanup_return_merge_7
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

hew_shutdown_exit_failed:                         ; preds = %helper_crash_cleanup_return_merge_11
  call void @hew_exit(i64 1)
  br label %hew_shutdown_exit_continue

hew_shutdown_exit_continue:                       ; preds = %hew_shutdown_exit_failed, %helper_crash_cleanup_return_merge_11
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
  store ptr @str_lit.6, ptr %local_3, align 8
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

declare i1 @hew_cont_crash_cleanup_deactivate(i64)

declare void @hew_trap_with_code(i32)

; Function Attrs: cold noreturn nounwind memory(inaccessiblemem: write)
declare void @llvm.trap() #0

define internal void @__hew_frame_cleanup_6e3157b8b1632579(ptr %0) {
entry:
  %"hew_string_drop drop" = load ptr, ptr %0, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop")
  store ptr null, ptr %0, align 8
  ret void
}

declare void @hew_string_drop(ptr)

declare i64 @hew_cont_crash_cleanup_arm(i64, ptr, i64, i64, ptr, i32, i32)

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64) #1

declare i1 @hew_cont_crash_cleanup_retire(i64)

declare i32 @hew_lambda_drain_all(i64)

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.smul.with.overflow.i64(i64, i64) #1

attributes #0 = { cold noreturn nounwind memory(inaccessiblemem: write) }
attributes #1 = { nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none) }
