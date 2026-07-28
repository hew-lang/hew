; ModuleID = 'r4_thunks'
source_filename = "r4_thunks"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "aarch64-apple-macosx13.0"

%__hew_closure_env_make_adder_0 = type { i64 }
%__hew_closure_env_make_multiplier_0 = type { i64 }
%__hew_closure_env_main_0 = type {}
%__hew_closure_env_main_1 = type { i64, i64 }
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

define internal i64 @apply({ ptr, ptr } %0, i64 %1) {
entry:
  %return_slot = alloca i64, align 8
  %closure_call_fallback_ctx = alloca [16 x i64], align 8
  store [16 x i64] zeroinitializer, ptr %closure_call_fallback_ctx, align 8
  %local_0 = alloca { ptr, ptr }, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  store { ptr, ptr } %0, ptr %local_0, align 8
  store i64 %1, ptr %local_1, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  %closure_pair_load = load { ptr, ptr }, ptr %local_0, align 8
  %closure_fn_extract = extractvalue { ptr, ptr } %closure_pair_load, 0
  %closure_env_extract = extractvalue { ptr, ptr } %closure_pair_load, 1
  %closure_call_arg = load i64, ptr %local_1, align 8
  %closure_call_result = call i64 %closure_fn_extract(ptr %closure_call_fallback_ctx, ptr %closure_env_extract, i64 %closure_call_arg)
  store i64 %closure_call_result, ptr %local_2, align 8
  %move_load = load i64, ptr %local_2, align 8
  store i64 %move_load, ptr %return_slot, align 8
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val
}

define internal i64 @apply_twice({ ptr, ptr } %0, i64 %1) {
entry:
  %return_slot = alloca i64, align 8
  %closure_call_fallback_ctx = alloca [16 x i64], align 8
  store [16 x i64] zeroinitializer, ptr %closure_call_fallback_ctx, align 8
  %local_0 = alloca { ptr, ptr }, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i64, align 8
  store { ptr, ptr } %0, ptr %local_0, align 8
  store i64 %1, ptr %local_1, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  %closure_pair_load = load { ptr, ptr }, ptr %local_0, align 8
  %closure_fn_extract = extractvalue { ptr, ptr } %closure_pair_load, 0
  %closure_env_extract = extractvalue { ptr, ptr } %closure_pair_load, 1
  %closure_call_arg = load i64, ptr %local_1, align 8
  %closure_call_result = call i64 %closure_fn_extract(ptr %closure_call_fallback_ctx, ptr %closure_env_extract, i64 %closure_call_arg)
  store i64 %closure_call_result, ptr %local_2, align 8
  %closure_pair_load1 = load { ptr, ptr }, ptr %local_0, align 8
  %closure_fn_extract2 = extractvalue { ptr, ptr } %closure_pair_load1, 0
  %closure_env_extract3 = extractvalue { ptr, ptr } %closure_pair_load1, 1
  %closure_call_arg4 = load i64, ptr %local_2, align 8
  %closure_call_result5 = call i64 %closure_fn_extract2(ptr %closure_call_fallback_ctx, ptr %closure_env_extract3, i64 %closure_call_arg4)
  store i64 %closure_call_result5, ptr %local_3, align 8
  %move_load = load i64, ptr %local_3, align 8
  store i64 %move_load, ptr %return_slot, align 8
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val
}

define internal { ptr, ptr } @make_adder(i64 %0) {
entry:
  %return_slot = alloca { ptr, ptr }, align 8
  %local_0 = alloca i64, align 8
  %local_1 = alloca %__hew_closure_env_make_adder_0, align 8
  %local_2 = alloca { ptr, ptr }, align 8
  store i64 %0, ptr %local_0, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  %field_0_init_ptr = getelementptr inbounds nuw %__hew_closure_env_make_adder_0, ptr %local_1, i32 0, i32 0
  %field_0_init_src = load i64, ptr %local_0, align 8
  store i64 %field_0_init_src, ptr %field_0_init_ptr, align 8
  %closure_env_box = call ptr @hew_dyn_box_alloc(i64 16, i64 8)
  store ptr @__hew_closure_env_free___hew_closure_invoke_make_adder_0, ptr %closure_env_box, align 8
  %closure_env_data = getelementptr inbounds i8, ptr %closure_env_box, i64 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %closure_env_data, ptr align 8 %local_1, i64 8, i1 false)
  %closure_fn_ptr = getelementptr inbounds nuw { ptr, ptr }, ptr %local_2, i32 0, i32 0
  %closure_env_ptr = getelementptr inbounds nuw { ptr, ptr }, ptr %local_2, i32 0, i32 1
  store ptr @__hew_closure_invoke_make_adder_0, ptr %closure_fn_ptr, align 8
  store ptr %closure_env_data, ptr %closure_env_ptr, align 8
  %move_load = load { ptr, ptr }, ptr %local_2, align 8
  store { ptr, ptr } %move_load, ptr %return_slot, align 8
  %ret_val = load { ptr, ptr }, ptr %return_slot, align 8
  ret { ptr, ptr } %ret_val
}

define internal i64 @__hew_closure_invoke_make_adder_0(ptr %0, ptr %1, i64 %2) {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i8, align 1
  store ptr %1, ptr %local_0, align 8
  store i64 %2, ptr %local_1, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  %closure_env_ptr_load = load ptr, ptr %local_0, align 8
  %closure_capture_ptr = getelementptr inbounds nuw %__hew_closure_env_make_adder_0, ptr %closure_env_ptr_load, i32 0, i32 0
  %closure_capture_load = load i64, ptr %closure_capture_ptr, align 8
  store i64 %closure_capture_load, ptr %local_2, align 8
  %checked_lhs = load i64, ptr %local_1, align 8
  %checked_rhs = load i64, ptr %local_2, align 8
  %with_overflow = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_3, align 8
  store i8 %checked_overflow_widen, ptr %local_4, align 1
  %cond_load = load i8, ptr %local_4, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb2:                                              ; preds = %bb0
  %move_load = load i64, ptr %local_3, align 8
  store i64 %move_load, ptr %return_slot, align 8
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val
}

define internal { ptr, ptr } @make_multiplier(i64 %0) {
entry:
  %return_slot = alloca { ptr, ptr }, align 8
  %local_0 = alloca i64, align 8
  %local_1 = alloca %__hew_closure_env_make_multiplier_0, align 8
  %local_2 = alloca { ptr, ptr }, align 8
  store i64 %0, ptr %local_0, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  %field_0_init_ptr = getelementptr inbounds nuw %__hew_closure_env_make_multiplier_0, ptr %local_1, i32 0, i32 0
  %field_0_init_src = load i64, ptr %local_0, align 8
  store i64 %field_0_init_src, ptr %field_0_init_ptr, align 8
  %closure_env_box = call ptr @hew_dyn_box_alloc(i64 16, i64 8)
  store ptr @__hew_closure_env_free___hew_closure_invoke_make_multiplier_0, ptr %closure_env_box, align 8
  %closure_env_data = getelementptr inbounds i8, ptr %closure_env_box, i64 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %closure_env_data, ptr align 8 %local_1, i64 8, i1 false)
  %closure_fn_ptr = getelementptr inbounds nuw { ptr, ptr }, ptr %local_2, i32 0, i32 0
  %closure_env_ptr = getelementptr inbounds nuw { ptr, ptr }, ptr %local_2, i32 0, i32 1
  store ptr @__hew_closure_invoke_make_multiplier_0, ptr %closure_fn_ptr, align 8
  store ptr %closure_env_data, ptr %closure_env_ptr, align 8
  %move_load = load { ptr, ptr }, ptr %local_2, align 8
  store { ptr, ptr } %move_load, ptr %return_slot, align 8
  %ret_val = load { ptr, ptr }, ptr %return_slot, align 8
  ret { ptr, ptr } %ret_val
}

define internal i64 @__hew_closure_invoke_make_multiplier_0(ptr %0, ptr %1, i64 %2) {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i8, align 1
  store ptr %1, ptr %local_0, align 8
  store i64 %2, ptr %local_1, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  %closure_env_ptr_load = load ptr, ptr %local_0, align 8
  %closure_capture_ptr = getelementptr inbounds nuw %__hew_closure_env_make_multiplier_0, ptr %closure_env_ptr_load, i32 0, i32 0
  %closure_capture_load = load i64, ptr %closure_capture_ptr, align 8
  store i64 %closure_capture_load, ptr %local_2, align 8
  %checked_lhs = load i64, ptr %local_1, align 8
  %checked_rhs = load i64, ptr %local_2, align 8
  %with_overflow = call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_3, align 8
  store i8 %checked_overflow_widen, ptr %local_4, align 1
  %cond_load = load i8, ptr %local_4, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb2:                                              ; preds = %bb0
  %move_load = load i64, ptr %local_3, align 8
  store i64 %move_load, ptr %return_slot, align 8
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val
}

define i8 @main() {
entry:
  %return_slot = alloca i8, align 1
  %closure_call_fallback_ctx = alloca [16 x i64], align 8
  store [16 x i64] zeroinitializer, ptr %closure_call_fallback_ctx, align 8
  %local_0 = alloca %__hew_closure_env_main_0, align 8
  %local_1 = alloca { ptr, ptr }, align 8
  %local_2 = alloca { ptr, ptr }, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i64, align 8
  %local_5 = alloca i64, align 8
  %local_6 = alloca i64, align 8
  %local_7 = alloca i64, align 8
  %local_8 = alloca { ptr, ptr }, align 8
  %local_9 = alloca { ptr, ptr }, align 8
  %local_10 = alloca i64, align 8
  %local_11 = alloca i64, align 8
  %local_12 = alloca i64, align 8
  %local_13 = alloca { ptr, ptr }, align 8
  %local_14 = alloca { ptr, ptr }, align 8
  %local_15 = alloca i64, align 8
  %local_16 = alloca i64, align 8
  %local_17 = alloca i64, align 8
  %local_18 = alloca i64, align 8
  %local_19 = alloca i64, align 8
  %local_20 = alloca i64, align 8
  %local_21 = alloca %__hew_closure_env_main_1, align 8
  %local_22 = alloca { ptr, ptr }, align 8
  %local_23 = alloca { ptr, ptr }, align 8
  %local_24 = alloca i64, align 8
  %local_25 = alloca i64, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %closure_fn_ptr = getelementptr inbounds nuw { ptr, ptr }, ptr %local_1, i32 0, i32 0
  %closure_env_ptr = getelementptr inbounds nuw { ptr, ptr }, ptr %local_1, i32 0, i32 1
  store ptr @__hew_closure_invoke_main_0, ptr %closure_fn_ptr, align 8
  store ptr null, ptr %closure_env_ptr, align 8
  %move_load = load { ptr, ptr }, ptr %local_1, align 8
  store { ptr, ptr } %move_load, ptr %local_2, align 8
  store i64 21, ptr %local_3, align 8
  %call_arg = load { ptr, ptr }, ptr %local_2, align 8
  %call_arg1 = load i64, ptr %local_3, align 8
  %call_result = call i64 @apply({ ptr, ptr } %call_arg, i64 %call_arg1)
  store i64 %call_result, ptr %local_4, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %print_arg = load i64, ptr %local_4, align 8
  call void @hew_print_value(i8 1, i64 %print_arg, i1 true)
  br label %bb2

bb2:                                              ; preds = %bb1
  store i64 3, ptr %local_5, align 8
  %call_arg2 = load { ptr, ptr }, ptr %local_2, align 8
  %call_arg3 = load i64, ptr %local_5, align 8
  %call_result4 = call i64 @apply_twice({ ptr, ptr } %call_arg2, i64 %call_arg3)
  store i64 %call_result4, ptr %local_6, align 8
  br label %bb3

bb3:                                              ; preds = %bb2
  %print_arg5 = load i64, ptr %local_6, align 8
  call void @hew_print_value(i8 1, i64 %print_arg5, i1 true)
  br label %bb4

bb4:                                              ; preds = %bb3
  store i64 10, ptr %local_7, align 8
  %call_arg6 = load i64, ptr %local_7, align 8
  %call_result7 = call { ptr, ptr } @make_adder(i64 %call_arg6)
  store { ptr, ptr } %call_result7, ptr %local_8, align 8
  br label %bb5

bb5:                                              ; preds = %bb4
  %move_load8 = load { ptr, ptr }, ptr %local_8, align 8
  store { ptr, ptr } %move_load8, ptr %local_9, align 8
  store i64 32, ptr %local_10, align 8
  %closure_pair_load = load { ptr, ptr }, ptr %local_9, align 8
  %closure_fn_extract = extractvalue { ptr, ptr } %closure_pair_load, 0
  %closure_env_extract = extractvalue { ptr, ptr } %closure_pair_load, 1
  %closure_call_arg = load i64, ptr %local_10, align 8
  %closure_call_result = call i64 %closure_fn_extract(ptr %closure_call_fallback_ctx, ptr %closure_env_extract, i64 %closure_call_arg)
  store i64 %closure_call_result, ptr %local_11, align 8
  %print_arg9 = load i64, ptr %local_11, align 8
  call void @hew_print_value(i8 1, i64 %print_arg9, i1 true)
  br label %bb6

bb6:                                              ; preds = %bb5
  store i64 3, ptr %local_12, align 8
  %call_arg10 = load i64, ptr %local_12, align 8
  %call_result11 = call { ptr, ptr } @make_multiplier(i64 %call_arg10)
  store { ptr, ptr } %call_result11, ptr %local_13, align 8
  br label %bb7

bb7:                                              ; preds = %bb6
  %move_load12 = load { ptr, ptr }, ptr %local_13, align 8
  store { ptr, ptr } %move_load12, ptr %local_14, align 8
  store i64 7, ptr %local_15, align 8
  %call_arg13 = load { ptr, ptr }, ptr %local_14, align 8
  %call_arg14 = load i64, ptr %local_15, align 8
  %call_result15 = call i64 @apply({ ptr, ptr } %call_arg13, i64 %call_arg14)
  store i64 %call_result15, ptr %local_16, align 8
  br label %bb8

bb8:                                              ; preds = %bb7
  %print_arg16 = load i64, ptr %local_16, align 8
  call void @hew_print_value(i8 1, i64 %print_arg16, i1 true)
  br label %bb9

bb9:                                              ; preds = %bb8
  store i64 100, ptr %local_17, align 8
  %move_load17 = load i64, ptr %local_17, align 8
  store i64 %move_load17, ptr %local_18, align 8
  store i64 5, ptr %local_19, align 8
  %move_load18 = load i64, ptr %local_19, align 8
  store i64 %move_load18, ptr %local_20, align 8
  %field_0_init_ptr = getelementptr inbounds nuw %__hew_closure_env_main_1, ptr %local_21, i32 0, i32 0
  %field_0_init_src = load i64, ptr %local_18, align 8
  store i64 %field_0_init_src, ptr %field_0_init_ptr, align 8
  %field_1_init_ptr = getelementptr inbounds nuw %__hew_closure_env_main_1, ptr %local_21, i32 0, i32 1
  %field_1_init_src = load i64, ptr %local_20, align 8
  store i64 %field_1_init_src, ptr %field_1_init_ptr, align 8
  %closure_fn_ptr19 = getelementptr inbounds nuw { ptr, ptr }, ptr %local_22, i32 0, i32 0
  %closure_env_ptr20 = getelementptr inbounds nuw { ptr, ptr }, ptr %local_22, i32 0, i32 1
  store ptr @__hew_closure_invoke_main_1, ptr %closure_fn_ptr19, align 8
  store ptr %local_21, ptr %closure_env_ptr20, align 8
  %move_load21 = load { ptr, ptr }, ptr %local_22, align 8
  store { ptr, ptr } %move_load21, ptr %local_23, align 8
  store i64 4, ptr %local_24, align 8
  %closure_pair_load22 = load { ptr, ptr }, ptr %local_23, align 8
  %closure_fn_extract23 = extractvalue { ptr, ptr } %closure_pair_load22, 0
  %closure_env_extract24 = extractvalue { ptr, ptr } %closure_pair_load22, 1
  %closure_call_arg25 = load i64, ptr %local_24, align 8
  %closure_call_result26 = call i64 %closure_fn_extract23(ptr %closure_call_fallback_ctx, ptr %closure_env_extract24, i64 %closure_call_arg25)
  store i64 %closure_call_result26, ptr %local_25, align 8
  %print_arg27 = load i64, ptr %local_25, align 8
  call void @hew_print_value(i8 1, i64 %print_arg27, i1 true)
  br label %bb10

bb10:                                             ; preds = %bb9
  %closure_drop_env_slot = getelementptr inbounds nuw { ptr, ptr }, ptr %local_9, i32 0, i32 1
  %closure_drop_env = load ptr, ptr %closure_drop_env_slot, align 8
  %closure_drop_env_is_null = icmp eq ptr %closure_drop_env, null
  br i1 %closure_drop_env_is_null, label %closure_drop_closure_env_drop_cont, label %closure_drop_closure_env_free

cancel_exit:                                      ; preds = %entry
  ret i8 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

closure_drop_closure_env_free:                    ; preds = %bb10
  %closure_drop_env_thunk_slot = getelementptr inbounds i8, ptr %closure_drop_env, i64 -8
  %closure_drop_env_free_thunk = load ptr, ptr %closure_drop_env_thunk_slot, align 8
  call void %closure_drop_env_free_thunk(ptr %closure_drop_env)
  store ptr null, ptr %closure_drop_env_slot, align 8
  br label %closure_drop_closure_env_drop_cont

closure_drop_closure_env_drop_cont:               ; preds = %closure_drop_closure_env_free, %bb10
  %hew_lambda_drain_all_call = call i32 @hew_lambda_drain_all(i64 0)
  ret i8 0
}

define internal i64 @__hew_closure_invoke_main_0(ptr %0, ptr %1, i64 %2) {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i8, align 1
  store ptr %1, ptr %local_0, align 8
  store i64 %2, ptr %local_1, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  store i64 2, ptr %local_2, align 8
  %checked_lhs = load i64, ptr %local_1, align 8
  %checked_rhs = load i64, ptr %local_2, align 8
  %with_overflow = call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_3, align 8
  store i8 %checked_overflow_widen, ptr %local_4, align 1
  %cond_load = load i8, ptr %local_4, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb2:                                              ; preds = %bb0
  %move_load = load i64, ptr %local_3, align 8
  store i64 %move_load, ptr %return_slot, align 8
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val
}

define internal i64 @__hew_closure_invoke_main_1(ptr %0, ptr %1, i64 %2) {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i64, align 8
  %local_5 = alloca i8, align 1
  %local_6 = alloca i64, align 8
  %local_7 = alloca i8, align 1
  store ptr %1, ptr %local_0, align 8
  store i64 %2, ptr %local_1, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  %closure_env_ptr_load = load ptr, ptr %local_0, align 8
  %closure_capture_ptr = getelementptr inbounds nuw %__hew_closure_env_main_1, ptr %closure_env_ptr_load, i32 0, i32 0
  %closure_capture_load = load i64, ptr %closure_capture_ptr, align 8
  store i64 %closure_capture_load, ptr %local_2, align 8
  %closure_env_ptr_load1 = load ptr, ptr %local_0, align 8
  %closure_capture_ptr2 = getelementptr inbounds nuw %__hew_closure_env_main_1, ptr %closure_env_ptr_load1, i32 0, i32 1
  %closure_capture_load3 = load i64, ptr %closure_capture_ptr2, align 8
  store i64 %closure_capture_load3, ptr %local_3, align 8
  %checked_lhs = load i64, ptr %local_1, align 8
  %checked_rhs = load i64, ptr %local_3, align 8
  %with_overflow = call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_4, align 8
  store i8 %checked_overflow_widen, ptr %local_5, align 1
  %cond_load = load i8, ptr %local_5, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb2:                                              ; preds = %bb0
  %checked_lhs4 = load i64, ptr %local_2, align 8
  %checked_rhs5 = load i64, ptr %local_4, align 8
  %with_overflow6 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs4, i64 %checked_rhs5)
  %checked_result7 = extractvalue { i64, i1 } %with_overflow6, 0
  %checked_overflow8 = extractvalue { i64, i1 } %with_overflow6, 1
  %checked_overflow_widen9 = zext i1 %checked_overflow8 to i8
  store i64 %checked_result7, ptr %local_6, align 8
  store i8 %checked_overflow_widen9, ptr %local_7, align 1
  %cond_load10 = load i8, ptr %local_7, align 1
  %cond_nz11 = icmp ne i8 %cond_load10, 0
  br i1 %cond_nz11, label %bb3, label %bb4

bb3:                                              ; preds = %bb2
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb4:                                              ; preds = %bb2
  %move_load = load i64, ptr %local_6, align 8
  store i64 %move_load, ptr %return_slot, align 8
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
  %hew_duration_nanos = load i64, ptr %local_0, align 8
  %hew_duration_nanos_call = call i64 @hew_duration_nanos(i64 %hew_duration_nanos)
  store i64 %hew_duration_nanos_call, ptr %local_1, align 8
  %call_arg = load i64, ptr %local_1, align 8
  %call_result = call ptr @hew_i64_to_string(i64 %call_arg)
  store ptr %call_result, ptr %local_2, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  store ptr @str_lit, ptr %local_3, align 8
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
  store ptr %clone_helper_f1, ptr %dst_f1_ptr, align 8
  br label %success

step_0_clone:                                     ; preds = %entry
  %src_f1_ptr = getelementptr inbounds nuw %CrashInfo, ptr %0, i32 0, i32 1
  %src_f1 = load ptr, ptr %src_f1_ptr, align 8
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
  %drop_f1 = load ptr, ptr %drop_f1_ptr, align 8
  call void @hew_string_drop(ptr %drop_f1)
  br label %done

done:                                             ; preds = %do_drop, %entry
  ret void
}

declare void @hew_string_drop(ptr)

define internal void @__hew_record_overwrite_release_CrashInfo(ptr %0, ptr %1) {
entry:
  %ow_slot_0 = alloca ptr, align 8
  store ptr null, ptr %ow_slot_0, align 8
  %ow_new_d0_f1_ptr = getelementptr inbounds nuw %CrashInfo, ptr %1, i32 0, i32 1
  %ow_new_d0_f1_leaf = load ptr, ptr %ow_new_d0_f1_ptr, align 8
  store ptr %ow_new_d0_f1_leaf, ptr %ow_slot_0, align 8
  %ow_old_d0_f1_ptr = getelementptr inbounds nuw %CrashInfo, ptr %0, i32 0, i32 1
  %ow_old_d0_f1_val = load ptr, ptr %ow_old_d0_f1_ptr, align 8
  %ow_old_d0_f1_int = ptrtoint ptr %ow_old_d0_f1_val to i64
  %ow_old_d0_f1_cmp0_leaf = load ptr, ptr %ow_slot_0, align 8
  %ow_old_d0_f1_cmp0_int = ptrtoint ptr %ow_old_d0_f1_cmp0_leaf to i64
  %ow_old_d0_f1_cmp0_eq = icmp eq i64 %ow_old_d0_f1_int, %ow_old_d0_f1_cmp0_int
  %ow_old_d0_f1_matched0 = or i1 false, %ow_old_d0_f1_cmp0_eq
  %ow_old_d0_f1_neutralized = select i1 %ow_old_d0_f1_matched0, ptr null, ptr %ow_old_d0_f1_val
  store ptr %ow_old_d0_f1_neutralized, ptr %ow_old_d0_f1_ptr, align 8
  call void @__hew_record_drop_inplace_CrashInfo(ptr %0)
  ret void
}

declare ptr @hew_dyn_box_alloc(i64, i64)

declare void @hew_dyn_box_free(ptr, i64, i64)

define private void @__hew_closure_env_free___hew_closure_invoke_make_adder_0(ptr %0) {
entry:
  %env_box_base = getelementptr inbounds i8, ptr %0, i64 -8
  call void @hew_dyn_box_free(ptr %env_box_base, i64 16, i64 8)
  ret void
}

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias writeonly captures(none), ptr noalias readonly captures(none), i64, i1 immarg) #0

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64) #1

declare void @hew_trap_with_code(i32)

; Function Attrs: cold noreturn nounwind memory(inaccessiblemem: write)
declare void @llvm.trap() #2

define private void @__hew_closure_env_free___hew_closure_invoke_make_multiplier_0(ptr %0) {
entry:
  %env_box_base = getelementptr inbounds i8, ptr %0, i64 -8
  call void @hew_dyn_box_free(ptr %env_box_base, i64 16, i64 8)
  ret void
}

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.smul.with.overflow.i64(i64, i64) #1

declare i32 @hew_actor_cooperate()

declare i32 @hew_lambda_drain_all(i64)

attributes #0 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
attributes #1 = { nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none) }
attributes #2 = { cold noreturn nounwind memory(inaccessiblemem: write) }
