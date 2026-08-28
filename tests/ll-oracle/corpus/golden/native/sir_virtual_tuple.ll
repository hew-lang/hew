; ModuleID = 'sir_virtual_tuple'
source_filename = "sir_virtual_tuple"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "aarch64-apple-macosx13.0"

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

define internal i64 @pair_second(i64 %0, i64 %1) {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca i64, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca { i64, i64 }, align 8
  %local_3 = alloca { i64, i64 }, align 8
  %local_4 = alloca i64, align 8
  store i64 %0, ptr %local_0, align 8
  store i64 %1, ptr %local_1, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  %tuple_elem_0_load = load i64, ptr %local_0, align 8
  %tuple_elem_0_gep = getelementptr inbounds nuw { i64, i64 }, ptr %local_2, i32 0, i32 0
  store i64 %tuple_elem_0_load, ptr %tuple_elem_0_gep, align 8
  %tuple_elem_1_load = load i64, ptr %local_1, align 8
  %tuple_elem_1_gep = getelementptr inbounds nuw { i64, i64 }, ptr %local_2, i32 0, i32 1
  store i64 %tuple_elem_1_load, ptr %tuple_elem_1_gep, align 8
  %move_load = load { i64, i64 }, ptr %local_2, align 8
  store { i64, i64 } %move_load, ptr %local_3, align 8
  %tuple_1_load_ptr = getelementptr inbounds nuw { i64, i64 }, ptr %local_3, i32 0, i32 1
  %tuple_1_load = load i64, ptr %tuple_1_load_ptr, align 8
  store i64 %tuple_1_load, ptr %local_4, align 8
  %move_load1 = load i64, ptr %local_4, align 8
  store i64 %move_load1, ptr %return_slot, align 8
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val
}

define i64 @main() personality ptr @rust_eh_personality {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca i64, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i64, align 8
  %local_5 = alloca i8, align 1
  %local_6 = alloca i64, align 8
  %local_7 = alloca i64, align 8
  %local_8 = alloca i64, align 8
  %local_9 = alloca i64, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  store i64 0, ptr %local_1, align 8
  store i64 42, ptr %local_2, align 8
  %call_arg = load i64, ptr %local_1, align 8
  %call_arg1 = load i64, ptr %local_2, align 8
  %call_result = invoke i64 @pair_second(i64 %call_arg, i64 %call_arg1)
          to label %invoke.cont unwind label %invoke.cleanup

bb1:                                              ; preds = %invoke.cont
  store i64 42, ptr %local_4, align 8
  %cmp_lhs = load i64, ptr %local_3, align 8
  %cmp_rhs = load i64, ptr %local_4, align 8
  %cmp_bit = icmp eq i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_5, align 1
  %cond_load = load i8, ptr %local_5, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  store i64 0, ptr %local_6, align 8
  %move_load = load i64, ptr %local_6, align 8
  store i64 %move_load, ptr %local_7, align 8
  %move_load2 = load i64, ptr %local_7, align 8
  store i64 %move_load2, ptr %local_0, align 8
  br label %bb4

bb3:                                              ; preds = %bb1
  store i64 1, ptr %local_8, align 8
  %move_load3 = load i64, ptr %local_8, align 8
  store i64 %move_load3, ptr %local_9, align 8
  %move_load4 = load i64, ptr %local_9, align 8
  store i64 %move_load4, ptr %local_0, align 8
  br label %bb4

bb4:                                              ; preds = %bb3, %bb2
  %move_load5 = load i64, ptr %local_0, align 8
  store i64 %move_load5, ptr %return_slot, align 8
  %hew_lambda_drain_all_call = call i32 @hew_lambda_drain_all(i64 0)
  %hew_lambda_drain_failed = icmp ne i32 %hew_lambda_drain_all_call, 0
  %hew_runtime_exit_status_call6 = call i32 @hew_runtime_exit_status()
  %hew_runtime_faulted7 = icmp ne i32 %hew_runtime_exit_status_call6, 0
  %hew_exit_any_failed = or i1 %hew_lambda_drain_failed, %hew_runtime_faulted7
  br i1 %hew_exit_any_failed, label %hew_shutdown_exit_failed, label %hew_shutdown_exit_continue

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
  ret i64 0

invoke.cont:                                      ; preds = %bb0
  store i64 %call_result, ptr %local_3, align 8
  br label %bb1

invoke.cleanup:                                   ; preds = %bb0
  %hew.exception = landingpad { ptr, i32 }
          cleanup
  resume { ptr, i32 } %hew.exception

hew_shutdown_exit_failed:                         ; preds = %bb4
  %hew_exit_user_code = load i64, ptr %return_slot, align 8
  %hew_exit_user_code_set = icmp ne i64 %hew_exit_user_code, 0
  %hew_exit_status_code = select i1 %hew_exit_user_code_set, i64 %hew_exit_user_code, i64 1
  call void @hew_exit(i64 %hew_exit_status_code)
  br label %hew_shutdown_exit_continue

hew_shutdown_exit_continue:                       ; preds = %hew_shutdown_exit_failed, %bb4
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val
}

define internal ptr @"i8::fmt"(i8 %0) personality ptr @rust_eh_personality {
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
  %call_result = invoke ptr @hew_int_to_string(i32 %call_arg)
          to label %invoke.cont unwind label %invoke.cleanup

bb1:                                              ; preds = %invoke.cont
  %move_load = load ptr, ptr %local_2, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0

invoke.cont:                                      ; preds = %bb0
  store ptr %call_result, ptr %local_2, align 8
  br label %bb1

invoke.cleanup:                                   ; preds = %bb0
  %hew.exception = landingpad { ptr, i32 }
          cleanup
  resume { ptr, i32 } %hew.exception
}

define internal ptr @"i16::fmt"(i16 %0) personality ptr @rust_eh_personality {
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
  %call_result = invoke ptr @hew_int_to_string(i32 %call_arg)
          to label %invoke.cont unwind label %invoke.cleanup

bb1:                                              ; preds = %invoke.cont
  %move_load = load ptr, ptr %local_2, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0

invoke.cont:                                      ; preds = %bb0
  store ptr %call_result, ptr %local_2, align 8
  br label %bb1

invoke.cleanup:                                   ; preds = %bb0
  %hew.exception = landingpad { ptr, i32 }
          cleanup
  resume { ptr, i32 } %hew.exception
}

define internal ptr @"i32::fmt"(i32 %0) personality ptr @rust_eh_personality {
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
  %call_result = invoke ptr @hew_int_to_string(i32 %call_arg)
          to label %invoke.cont unwind label %invoke.cleanup

bb1:                                              ; preds = %invoke.cont
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0

invoke.cont:                                      ; preds = %bb0
  store ptr %call_result, ptr %local_1, align 8
  br label %bb1

invoke.cleanup:                                   ; preds = %bb0
  %hew.exception = landingpad { ptr, i32 }
          cleanup
  resume { ptr, i32 } %hew.exception
}

define internal ptr @"i64::fmt"(i64 %0) personality ptr @rust_eh_personality {
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
  %call_result = invoke ptr @hew_i64_to_string(i64 %call_arg)
          to label %invoke.cont unwind label %invoke.cleanup

bb1:                                              ; preds = %invoke.cont
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0

invoke.cont:                                      ; preds = %bb0
  store ptr %call_result, ptr %local_1, align 8
  br label %bb1

invoke.cleanup:                                   ; preds = %bb0
  %hew.exception = landingpad { ptr, i32 }
          cleanup
  resume { ptr, i32 } %hew.exception
}

define internal ptr @"u8::fmt"(i8 %0) personality ptr @rust_eh_personality {
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
  %call_result = invoke ptr @hew_u8_to_string(i8 %call_arg)
          to label %invoke.cont unwind label %invoke.cleanup

bb1:                                              ; preds = %invoke.cont
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0

invoke.cont:                                      ; preds = %bb0
  store ptr %call_result, ptr %local_1, align 8
  br label %bb1

invoke.cleanup:                                   ; preds = %bb0
  %hew.exception = landingpad { ptr, i32 }
          cleanup
  resume { ptr, i32 } %hew.exception
}

define internal ptr @"u16::fmt"(i16 %0) personality ptr @rust_eh_personality {
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
  %call_result = invoke ptr @hew_uint_to_string(i32 %ffi_zext)
          to label %invoke.cont unwind label %invoke.cleanup

bb1:                                              ; preds = %invoke.cont
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0

invoke.cont:                                      ; preds = %bb0
  store ptr %call_result, ptr %local_1, align 8
  br label %bb1

invoke.cleanup:                                   ; preds = %bb0
  %hew.exception = landingpad { ptr, i32 }
          cleanup
  resume { ptr, i32 } %hew.exception
}

define internal ptr @"u32::fmt"(i32 %0) personality ptr @rust_eh_personality {
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
  %call_result = invoke ptr @hew_uint_to_string(i32 %call_arg)
          to label %invoke.cont unwind label %invoke.cleanup

bb1:                                              ; preds = %invoke.cont
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0

invoke.cont:                                      ; preds = %bb0
  store ptr %call_result, ptr %local_1, align 8
  br label %bb1

invoke.cleanup:                                   ; preds = %bb0
  %hew.exception = landingpad { ptr, i32 }
          cleanup
  resume { ptr, i32 } %hew.exception
}

define internal ptr @"u64::fmt"(i64 %0) personality ptr @rust_eh_personality {
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
  %call_result = invoke ptr @hew_u64_to_string(i64 %call_arg)
          to label %invoke.cont unwind label %invoke.cleanup

bb1:                                              ; preds = %invoke.cont
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0

invoke.cont:                                      ; preds = %bb0
  store ptr %call_result, ptr %local_1, align 8
  br label %bb1

invoke.cleanup:                                   ; preds = %bb0
  %hew.exception = landingpad { ptr, i32 }
          cleanup
  resume { ptr, i32 } %hew.exception
}

define internal ptr @"isize::fmt"(i64 %0) personality ptr @rust_eh_personality {
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
  %call_result = invoke ptr @hew_i64_to_string(i64 %call_arg)
          to label %invoke.cont unwind label %invoke.cleanup

bb1:                                              ; preds = %invoke.cont
  %move_load = load ptr, ptr %local_2, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0

invoke.cont:                                      ; preds = %bb0
  store ptr %call_result, ptr %local_2, align 8
  br label %bb1

invoke.cleanup:                                   ; preds = %bb0
  %hew.exception = landingpad { ptr, i32 }
          cleanup
  resume { ptr, i32 } %hew.exception
}

define internal ptr @"usize::fmt"(i64 %0) personality ptr @rust_eh_personality {
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
  %call_result = invoke ptr @hew_u64_to_string(i64 %call_arg)
          to label %invoke.cont unwind label %invoke.cleanup

bb1:                                              ; preds = %invoke.cont
  %move_load = load ptr, ptr %local_2, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0

invoke.cont:                                      ; preds = %bb0
  store ptr %call_result, ptr %local_2, align 8
  br label %bb1

invoke.cleanup:                                   ; preds = %bb0
  %hew.exception = landingpad { ptr, i32 }
          cleanup
  resume { ptr, i32 } %hew.exception
}

define internal ptr @"bool::fmt"(i8 %0) personality ptr @rust_eh_personality {
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
  %call_result = invoke ptr @hew_bool_to_string(i8 %call_arg)
          to label %invoke.cont unwind label %invoke.cleanup

bb1:                                              ; preds = %invoke.cont
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0

invoke.cont:                                      ; preds = %bb0
  store ptr %call_result, ptr %local_1, align 8
  br label %bb1

invoke.cleanup:                                   ; preds = %bb0
  %hew.exception = landingpad { ptr, i32 }
          cleanup
  resume { ptr, i32 } %hew.exception
}

define internal ptr @"char::fmt"(i32 %0) personality ptr @rust_eh_personality {
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
  %call_result = invoke ptr @hew_char_to_string(i32 %call_arg)
          to label %invoke.cont unwind label %invoke.cleanup

bb1:                                              ; preds = %invoke.cont
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0

invoke.cont:                                      ; preds = %bb0
  store ptr %call_result, ptr %local_1, align 8
  br label %bb1

invoke.cleanup:                                   ; preds = %bb0
  %hew.exception = landingpad { ptr, i32 }
          cleanup
  resume { ptr, i32 } %hew.exception
}

define internal ptr @"f64::fmt"(double %0) personality ptr @rust_eh_personality {
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
  %call_result = invoke ptr @hew_float_to_string(double %call_arg)
          to label %invoke.cont unwind label %invoke.cleanup

bb1:                                              ; preds = %invoke.cont
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0

invoke.cont:                                      ; preds = %bb0
  store ptr %call_result, ptr %local_1, align 8
  br label %bb1

invoke.cleanup:                                   ; preds = %bb0
  %hew.exception = landingpad { ptr, i32 }
          cleanup
  resume { ptr, i32 } %hew.exception
}

define internal ptr @"f32::fmt"(float %0) personality ptr @rust_eh_personality {
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
  %call_result = invoke ptr @hew_float_to_string(double %call_arg)
          to label %invoke.cont unwind label %invoke.cleanup

bb1:                                              ; preds = %invoke.cont
  %move_load = load ptr, ptr %local_2, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0

invoke.cont:                                      ; preds = %bb0
  store ptr %call_result, ptr %local_2, align 8
  br label %bb1

invoke.cleanup:                                   ; preds = %bb0
  %hew.exception = landingpad { ptr, i32 }
          cleanup
  resume { ptr, i32 } %hew.exception
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

define internal ptr @"duration::fmt"(i64 %0) personality ptr @rust_eh_personality {
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
  %call_result = invoke i64 @hew_duration_nanos(i64 %call_arg)
          to label %invoke.cont unwind label %invoke.cleanup

bb1:                                              ; preds = %invoke.cont
  %call_arg1 = load i64, ptr %local_1, align 8
  %call_result4 = invoke ptr @hew_i64_to_string(i64 %call_arg1)
          to label %invoke.cont2 unwind label %invoke.cleanup3

bb2:                                              ; preds = %invoke.cont2
  store ptr @str_lit, ptr %local_3, align 8
  %"hew_string_concat arg0" = load ptr, ptr %local_2, align 8
  %"hew_string_concat arg1" = load ptr, ptr %local_3, align 8
  %hew_string_concat_call = invoke ptr @hew_string_concat(ptr %"hew_string_concat arg0", ptr %"hew_string_concat arg1")
          to label %invoke.cont6 unwind label %invoke.cleanup7

bb3:                                              ; preds = %invoke.cont6
  %"hew_string_drop drop9" = load ptr, ptr %local_2, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop9")
  store ptr null, ptr %local_2, align 8
  %move_load = load ptr, ptr %local_4, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0

invoke.cont:                                      ; preds = %bb0
  store i64 %call_result, ptr %local_1, align 8
  br label %bb1

invoke.cleanup:                                   ; preds = %bb0
  %hew.exception = landingpad { ptr, i32 }
          cleanup
  resume { ptr, i32 } %hew.exception

invoke.cont2:                                     ; preds = %bb1
  store ptr %call_result4, ptr %local_2, align 8
  br label %bb2

invoke.cleanup3:                                  ; preds = %bb1
  %hew.exception5 = landingpad { ptr, i32 }
          cleanup
  resume { ptr, i32 } %hew.exception5

invoke.cont6:                                     ; preds = %bb2
  store ptr %hew_string_concat_call, ptr %local_4, align 8
  br label %bb3

invoke.cleanup7:                                  ; preds = %bb2
  %hew.exception8 = landingpad { ptr, i32 }
          cleanup
  %"hew_string_drop drop" = load ptr, ptr %local_2, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop")
  store ptr null, ptr %local_2, align 8
  resume { ptr, i32 } %hew.exception8
}

declare i32 @hew_actor_cooperate()

declare i32 @hew_runtime_exit_status()

declare i32 @rust_eh_personality(...)

declare i32 @hew_lambda_drain_all(i64)

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.smul.with.overflow.i64(i64, i64) #0

declare void @hew_trap_with_code(i32)

; Function Attrs: cold noreturn nounwind memory(inaccessiblemem: write)
declare void @llvm.trap() #1

declare void @hew_string_drop(ptr)

attributes #0 = { nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none) }
attributes #1 = { cold noreturn nounwind memory(inaccessiblemem: write) }
