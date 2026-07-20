; ModuleID = 'owned_vec_cross_function_release'
source_filename = "owned_vec_cross_function_release"
target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

%HeapRow = type { ptr, i64 }
%__hew_gen_env_batches_0 = type { i64 }
%"Option$$Vec$lHeapRow$g" = type { i8, [1 x i32] }
%Holder = type { ptr, i64 }
%CrashInfo = type { i64, ptr }

@__hew_vec_elem_layout_rec_HeapRow_16_8 = private constant { i32, i32, i8, ptr, ptr } { i32 16, i32 8, i8 2, ptr @__hew_record_clone_inplace_HeapRow, ptr @__hew_record_drop_inplace_HeapRow }
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

define internal ptr @make_rows(i64 %0) {
entry:
  %return_slot = alloca ptr, align 4
  %local_0 = alloca i64, align 8
  %local_1 = alloca ptr, align 4
  %local_2 = alloca ptr, align 4
  %local_3 = alloca ptr, align 4
  %local_4 = alloca ptr, align 4
  %local_5 = alloca i64, align 8
  %local_6 = alloca i64, align 8
  %local_7 = alloca i8, align 1
  %local_8 = alloca ptr, align 4
  %local_9 = alloca %HeapRow, align 8
  %local_10 = alloca ptr, align 4
  store i64 %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %hew_vec_new_with_elem_layout_call = call ptr @hew_vec_new_with_elem_layout(ptr @__hew_vec_elem_layout_rec_HeapRow_16_8)
  store ptr %hew_vec_new_with_elem_layout_call, ptr %local_1, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 4
  store ptr %move_load, ptr %local_2, align 4
  %hew_vec_new_i64_call = call ptr @hew_vec_new_i64()
  store ptr %hew_vec_new_i64_call, ptr %local_3, align 4
  br label %bb2

bb2:                                              ; preds = %bb1
  %move_load1 = load ptr, ptr %local_3, align 4
  store ptr %move_load1, ptr %local_4, align 4
  %call_arg = load ptr, ptr %local_4, align 4
  %call_arg2 = load i64, ptr %local_0, align 8
  call void @hew_vec_push_i64(ptr %call_arg, i64 %call_arg2)
  br label %bb3

bb3:                                              ; preds = %bb2
  store i64 1, ptr %local_5, align 8
  %checked_lhs = load i64, ptr %local_0, align 8
  %checked_rhs = load i64, ptr %local_5, align 8
  %with_overflow = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_6, align 8
  store i8 %checked_overflow_widen, ptr %local_7, align 1
  %cond_load = load i8, ptr %local_7, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb4, label %bb5

bb4:                                              ; preds = %bb3
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb5:                                              ; preds = %bb3
  %call_arg3 = load ptr, ptr %local_4, align 4
  %call_arg4 = load i64, ptr %local_6, align 8
  call void @hew_vec_push_i64(ptr %call_arg3, i64 %call_arg4)
  br label %bb6

bb6:                                              ; preds = %bb5
  %move_load5 = load ptr, ptr %local_4, align 4
  store ptr %move_load5, ptr %local_8, align 4
  %field_0_init_ptr = getelementptr inbounds nuw %HeapRow, ptr %local_9, i32 0, i32 0
  %field_0_init_src = load ptr, ptr %local_8, align 4
  store ptr %field_0_init_src, ptr %field_0_init_ptr, align 4
  %field_1_init_ptr = getelementptr inbounds nuw %HeapRow, ptr %local_9, i32 0, i32 1
  %field_1_init_src = load i64, ptr %local_0, align 8
  store i64 %field_1_init_src, ptr %field_1_init_ptr, align 8
  %"hew_vec_push_owned_move arg0" = load ptr, ptr %local_2, align 4
  call void @hew_vec_push_owned_move(ptr %"hew_vec_push_owned_move arg0", ptr %local_9)
  br label %bb7

bb7:                                              ; preds = %bb6
  %move_load6 = load ptr, ptr %local_2, align 4
  store ptr %move_load6, ptr %local_10, align 4
  %move_load7 = load ptr, ptr %local_10, align 4
  store ptr %move_load7, ptr %return_slot, align 4
  %ret_val = load ptr, ptr %return_slot, align 4
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @batches(i64 %0) {
entry:
  %return_slot = alloca ptr, align 4
  %local_0 = alloca i64, align 8
  %local_1 = alloca ptr, align 4
  %local_2 = alloca %__hew_gen_env_batches_0, align 8
  store i64 %0, ptr %local_0, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  %field_0_init_ptr = getelementptr inbounds nuw %__hew_gen_env_batches_0, ptr %local_2, i32 0, i32 0
  %field_0_init_src = load i64, ptr %local_0, align 8
  store i64 %field_0_init_src, ptr %field_0_init_ptr, align 8
  %gen_companion_alloc = call ptr @hew_cont_frame_alloc(i64 ptrtoint (ptr getelementptr ({ ptr, ptr, ptr, i8, i8, ptr }, ptr null, i32 1) to i64))
  %gen_companion_started_ptr = getelementptr inbounds nuw { ptr, ptr, ptr, i8, i8, ptr }, ptr %gen_companion_alloc, i32 0, i32 3
  store i8 0, ptr %gen_companion_started_ptr, align 1
  %gen_companion_out_drop_thunk_ptr = getelementptr inbounds nuw { ptr, ptr, ptr, i8, i8, ptr }, ptr %gen_companion_alloc, i32 0, i32 2
  store ptr @__hew_gen_out_drop___hew_gen_body_batches_0, ptr %gen_companion_out_drop_thunk_ptr, align 4
  %gen_companion_env_ptr = getelementptr inbounds nuw { ptr, ptr, ptr, i8, i8, ptr }, ptr %gen_companion_alloc, i32 0, i32 1
  %gen_companion_out_ptr = getelementptr inbounds nuw { ptr, ptr, ptr, i8, i8, ptr }, ptr %gen_companion_alloc, i32 0, i32 5
  %gen_env_alloc = call ptr @hew_cont_frame_alloc(i64 ptrtoint (ptr getelementptr (%__hew_gen_env_batches_0, ptr null, i32 1) to i64))
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %gen_env_alloc, ptr align 1 %local_2, i64 ptrtoint (ptr getelementptr (%__hew_gen_env_batches_0, ptr null, i32 1) to i64), i1 false)
  store ptr %gen_env_alloc, ptr %gen_companion_env_ptr, align 4
  %gen_body_ramp_call = call ptr @__hew_gen_body_batches_0(ptr %gen_companion_out_ptr, ptr %gen_env_alloc)
  %gen_companion_handle_ptr = getelementptr inbounds nuw { ptr, ptr, ptr, i8, i8, ptr }, ptr %gen_companion_alloc, i32 0, i32 0
  store ptr %gen_body_ramp_call, ptr %gen_companion_handle_ptr, align 4
  %gen_companion_ramp_done = call i1 @hew_cont_done(ptr %gen_body_ramp_call)
  %gen_companion_pending_bit = icmp eq i1 %gen_companion_ramp_done, false
  %gen_companion_pending_i8 = zext i1 %gen_companion_pending_bit to i8
  %gen_companion_pending_ptr = getelementptr inbounds nuw { ptr, ptr, ptr, i8, i8, ptr }, ptr %gen_companion_alloc, i32 0, i32 4
  store i8 %gen_companion_pending_i8, ptr %gen_companion_pending_ptr, align 1
  store ptr %gen_companion_alloc, ptr %local_1, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 4
  store ptr %move_load, ptr %return_slot, align 4
  %ret_val = load ptr, ptr %return_slot, align 4
  ret ptr %ret_val
}

; Function Attrs: presplitcoroutine
define internal ptr @__hew_gen_body_batches_0(ptr %0, ptr %1) #0 {
entry:
  %coro.id = call token @llvm.coro.id(i32 0, ptr null, ptr null, ptr null)
  %coro.need.alloc = call i1 @llvm.coro.alloc(token %coro.id)
  br i1 %coro.need.alloc, label %dyn.alloc, label %coro.begin

dyn.alloc:                                        ; preds = %entry
  %coro.size = call i64 @llvm.coro.size.i64()
  %coro.frame = call ptr @hew_cont_frame_alloc(i64 %coro.size)
  br label %coro.begin

coro.begin:                                       ; preds = %dyn.alloc, %entry
  %coro.mem = phi ptr [ null, %entry ], [ %coro.frame, %dyn.alloc ]
  %coro.handle = call ptr @llvm.coro.begin(token %coro.id, ptr %coro.mem)
  br label %alloca.prologue

coro.suspend.return:                              ; preds = %coro.dyn.free, %coro.cleanup, %coro.final.suspend, %bb7
  call void @llvm.coro.end(ptr %coro.handle, i1 false, token none)
  ret ptr %coro.handle

coro.cleanup:                                     ; preds = %coro.final.suspend, %coro.final.suspend, %gen_yield_abandon
  %coro.freemem = call ptr @llvm.coro.free(token %coro.id, ptr %coro.handle)
  %coro.freemem.isnull = icmp eq ptr %coro.freemem, null
  br i1 %coro.freemem.isnull, label %coro.suspend.return, label %coro.dyn.free

coro.final.suspend:                               ; preds = %bb4
  %coro.final.save = call token @llvm.coro.save(ptr %coro.handle)
  %coro.final.s = call i8 @llvm.coro.suspend(token %coro.final.save, i1 true)
  switch i8 %coro.final.s, label %coro.suspend.return [
    i8 0, label %coro.cleanup
    i8 1, label %coro.cleanup
  ]

alloca.prologue:                                  ; preds = %coro.begin
  %return_slot = alloca i8, align 1
  %local_0 = alloca ptr, align 4
  %local_1 = alloca ptr, align 4
  %local_2 = alloca i64, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i64, align 8
  %local_5 = alloca i64, align 8
  %local_6 = alloca i64, align 8
  %local_7 = alloca i8, align 1
  %local_8 = alloca i64, align 8
  %local_9 = alloca i64, align 8
  %local_10 = alloca i8, align 1
  %local_11 = alloca ptr, align 4
  %local_12 = alloca i8, align 1
  store ptr %0, ptr %local_0, align 4
  store ptr %1, ptr %local_1, align 4
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  store i64 1, ptr %local_4, align 8
  %move_load = load i64, ptr %local_4, align 8
  store i64 %move_load, ptr %local_5, align 8
  store i64 0, ptr %local_6, align 8
  %cmp_lhs = load i64, ptr %local_5, align 8
  %cmp_rhs = load i64, ptr %local_6, align 8
  %cmp_bit = icmp sle i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_7, align 1
  %cond_load = load i8, ptr %local_7, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb5, label %bb6

bb1:                                              ; preds = %after_cooperate14, %bb3
  %cmp_lhs1 = load i64, ptr %local_2, align 8
  %cmp_rhs2 = load i64, ptr %local_3, align 8
  %cmp_bit3 = icmp slt i64 %cmp_lhs1, %cmp_rhs2
  %cmp_zext4 = zext i1 %cmp_bit3 to i8
  store i8 %cmp_zext4, ptr %local_10, align 1
  %cond_load5 = load i8, ptr %local_10, align 1
  %cond_nz6 = icmp ne i8 %cond_load5, 0
  br i1 %cond_nz6, label %bb2, label %bb4

bb2:                                              ; preds = %bb1
  %call_arg = load i64, ptr %local_2, align 8
  %call_result = call ptr @make_rows(i64 %call_arg)
  store ptr %call_result, ptr %local_11, align 4
  br label %bb7

bb3:                                              ; preds = %after_cooperate18
  %checked_lhs = load i64, ptr %local_2, align 8
  %checked_rhs = load i64, ptr %local_5, align 8
  %with_overflow = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_2, align 8
  store i8 %checked_overflow_widen, ptr %local_12, align 1
  %cond_load7 = load i8, ptr %local_12, align 1
  %cond_nz8 = icmp ne i8 %cond_load7, 0
  br i1 %cond_nz8, label %bb9, label %bb1

bb4:                                              ; preds = %bb1
  br label %coro.final.suspend

bb5:                                              ; preds = %bb0
  call void @hew_trap_with_code(i32 202)
  call void @llvm.trap()
  unreachable

bb6:                                              ; preds = %bb0
  store i64 0, ptr %local_8, align 8
  %closure_env_ptr_load = load ptr, ptr %local_1, align 4
  %closure_capture_ptr = getelementptr inbounds nuw %__hew_gen_env_batches_0, ptr %closure_env_ptr_load, i32 0, i32 0
  %closure_capture_load = load i64, ptr %closure_capture_ptr, align 8
  store i64 %closure_capture_load, ptr %local_9, align 8
  %move_load9 = load i64, ptr %local_8, align 8
  store i64 %move_load9, ptr %local_2, align 8
  %move_load10 = load i64, ptr %local_9, align 8
  store i64 %move_load10, ptr %local_3, align 8
  %hew_actor_cooperate11 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel12 = icmp eq i32 %hew_actor_cooperate11, 2
  br i1 %hew_cooperate_is_cancel12, label %cancel_exit13, label %after_cooperate14

bb7:                                              ; preds = %bb2
  %gen_out_ptr = load ptr, ptr %local_0, align 4
  %gen_yield_value = load ptr, ptr %local_11, align 4
  store ptr %gen_yield_value, ptr %gen_out_ptr, align 4
  %gen_yield.save = call token @llvm.coro.save(ptr %coro.handle)
  %gen_yield.s = call i8 @llvm.coro.suspend(token %gen_yield.save, i1 false)
  switch i8 %gen_yield.s, label %coro.suspend.return [
    i8 0, label %bb8
    i8 1, label %gen_yield_abandon
  ]

bb8:                                              ; preds = %bb7
  %hew_actor_cooperate15 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel16 = icmp eq i32 %hew_actor_cooperate15, 2
  br i1 %hew_cooperate_is_cancel16, label %cancel_exit17, label %after_cooperate18

bb9:                                              ; preds = %bb3
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

cancel_exit:                                      ; preds = %alloca.prologue
  ret ptr %coro.handle

after_cooperate:                                  ; preds = %alloca.prologue
  br label %bb0

cancel_exit13:                                    ; preds = %bb6
  ret ptr %coro.handle

after_cooperate14:                                ; preds = %bb6
  br label %bb1

gen_yield_abandon:                                ; preds = %bb7
  br label %coro.cleanup

cancel_exit17:                                    ; preds = %bb8
  ret ptr %coro.handle

after_cooperate18:                                ; preds = %bb8
  br label %bb3

coro.dyn.free:                                    ; preds = %coro.cleanup
  call void @hew_cont_frame_free(ptr %coro.freemem)
  br label %coro.suspend.return
}

define internal i64 @consume_generator(i64 %0) {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca i64, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca ptr, align 4
  %local_4 = alloca ptr, align 4
  %local_5 = alloca i8, align 1
  %local_6 = alloca %"Option$$Vec$lHeapRow$g", align 8
  %local_7 = alloca i64, align 8
  %local_8 = alloca i64, align 8
  %local_9 = alloca i8, align 1
  %local_10 = alloca i64, align 8
  %local_11 = alloca i8, align 1
  %local_12 = alloca ptr, align 4
  %local_13 = alloca i64, align 8
  %local_14 = alloca i64, align 8
  %local_15 = alloca i8, align 1
  store i64 %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  store i64 0, ptr %local_1, align 8
  %move_load = load i64, ptr %local_1, align 8
  store i64 %move_load, ptr %local_2, align 8
  %call_arg = load i64, ptr %local_0, align 8
  %call_result = call ptr @batches(i64 %call_arg)
  store ptr %call_result, ptr %local_3, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load1 = load ptr, ptr %local_3, align 4
  store ptr %move_load1, ptr %local_4, align 4
  br label %bb2

bb2:                                              ; preds = %after_cooperate7, %bb1
  %gen_next_companion = load ptr, ptr %local_4, align 4
  %gen_next_handle_ptr = getelementptr inbounds nuw { ptr, ptr, ptr, i8, i8, ptr }, ptr %gen_next_companion, i32 0, i32 0
  %gen_next_handle = load ptr, ptr %gen_next_handle_ptr, align 4
  %gen_next_pending_ptr = getelementptr inbounds nuw { ptr, ptr, ptr, i8, i8, ptr }, ptr %gen_next_companion, i32 0, i32 4
  %gen_next_started_ptr = getelementptr inbounds nuw { ptr, ptr, ptr, i8, i8, ptr }, ptr %gen_next_companion, i32 0, i32 3
  %gen_next_started = load i8, ptr %gen_next_started_ptr, align 1
  %gen_next_started_set = icmp ne i8 %gen_next_started, 0
  %machine_tag_ptr = getelementptr inbounds nuw %"Option$$Vec$lHeapRow$g", ptr %local_6, i32 0, i32 0
  %machine_payload_ptr = getelementptr inbounds nuw %"Option$$Vec$lHeapRow$g", ptr %local_6, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { ptr }, ptr %machine_payload_ptr, i32 0, i32 0
  br i1 %gen_next_started_set, label %gen_next_resume, label %gen_next_check_done

bb3:                                              ; preds = %after_cooperate16
  %"hew_gen_coro_destroy drop" = load ptr, ptr %local_4, align 4
  call void @hew_gen_coro_destroy(ptr %"hew_gen_coro_destroy drop")
  store ptr null, ptr %local_4, align 4
  %move_load3 = load i64, ptr %local_2, align 8
  store i64 %move_load3, ptr %return_slot, align 8
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val

bb4:                                              ; preds = %after_cooperate31, %after_cooperate27
  %hew_actor_cooperate4 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel5 = icmp eq i32 %hew_actor_cooperate4, 2
  br i1 %hew_cooperate_is_cancel5, label %cancel_exit6, label %after_cooperate7

bb5:                                              ; preds = %gen_next_cont
  %machine_payload_ptr8 = getelementptr inbounds nuw %"Option$$Vec$lHeapRow$g", ptr %local_6, i32 0, i32 1
  %machine_variant_field_ptr9 = getelementptr inbounds nuw { ptr }, ptr %machine_payload_ptr8, i32 0, i32 0
  %move_load10 = load ptr, ptr %machine_variant_field_ptr9, align 4
  store ptr %move_load10, ptr %local_12, align 4
  store i64 1, ptr %local_13, align 8
  %checked_lhs = load i64, ptr %local_2, align 8
  %checked_rhs = load i64, ptr %local_13, align 8
  %with_overflow = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_14, align 8
  store i8 %checked_overflow_widen, ptr %local_15, align 1
  %cond_load11 = load i8, ptr %local_15, align 1
  %cond_nz12 = icmp ne i8 %cond_load11, 0
  br i1 %cond_nz12, label %bb9, label %bb10

bb6:                                              ; preds = %bb8
  %hew_actor_cooperate13 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel14 = icmp eq i32 %hew_actor_cooperate13, 2
  br i1 %hew_cooperate_is_cancel14, label %cancel_exit15, label %after_cooperate16

bb7:                                              ; preds = %bb8
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

bb8:                                              ; preds = %gen_next_cont
  store i64 1, ptr %local_10, align 8
  %cmp_lhs17 = load i64, ptr %local_7, align 8
  %cmp_rhs18 = load i64, ptr %local_10, align 8
  %cmp_bit19 = icmp eq i64 %cmp_lhs17, %cmp_rhs18
  %cmp_zext20 = zext i1 %cmp_bit19 to i8
  store i8 %cmp_zext20, ptr %local_11, align 1
  %cond_load21 = load i8, ptr %local_11, align 1
  %cond_nz22 = icmp ne i8 %cond_load21, 0
  br i1 %cond_nz22, label %bb6, label %bb7

bb9:                                              ; preds = %bb5
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb10:                                             ; preds = %bb5
  %move_load23 = load i64, ptr %local_14, align 8
  store i64 %move_load23, ptr %local_2, align 8
  %"hew_vec_free_owned drop" = load ptr, ptr %local_12, align 4
  call void @hew_vec_free_owned(ptr %"hew_vec_free_owned drop")
  store ptr null, ptr %local_12, align 4
  %hew_actor_cooperate24 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel25 = icmp eq i32 %hew_actor_cooperate24, 2
  br i1 %hew_cooperate_is_cancel25, label %cancel_exit26, label %after_cooperate27

bb11:                                             ; No predecessors!
  %hew_actor_cooperate28 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel29 = icmp eq i32 %hew_actor_cooperate28, 2
  br i1 %hew_cooperate_is_cancel29, label %cancel_exit30, label %after_cooperate31

cancel_exit:                                      ; preds = %entry
  ret i64 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

gen_next_resume:                                  ; preds = %bb2
  call void @hew_cont_resume(ptr %gen_next_handle)
  br label %gen_next_check_done

gen_next_check_done:                              ; preds = %gen_next_resume, %bb2
  %hew_cont_done_call = call i1 @hew_cont_done(ptr %gen_next_handle)
  br i1 %hew_cont_done_call, label %gen_next_none, label %gen_next_some

gen_next_none:                                    ; preds = %gen_next_check_done
  store i8 1, ptr %machine_tag_ptr, align 1
  store i8 0, ptr %gen_next_pending_ptr, align 1
  br label %gen_next_cont

gen_next_some:                                    ; preds = %gen_next_check_done
  %gen_next_out_ptr = getelementptr inbounds nuw { ptr, ptr, ptr, i8, i8, ptr }, ptr %gen_next_companion, i32 0, i32 5
  %gen_next_value = load ptr, ptr %gen_next_out_ptr, align 4
  store i8 0, ptr %machine_tag_ptr, align 1
  store ptr %gen_next_value, ptr %machine_variant_field_ptr, align 4
  store i8 0, ptr %gen_next_pending_ptr, align 1
  store i8 1, ptr %gen_next_started_ptr, align 1
  br label %gen_next_cont

gen_next_cont:                                    ; preds = %gen_next_some, %gen_next_none
  %machine_tag_ptr2 = getelementptr inbounds nuw %"Option$$Vec$lHeapRow$g", ptr %local_6, i32 0, i32 0
  %move_iN_load = load i8, ptr %machine_tag_ptr2, align 1
  %move_iN_zext = zext i8 %move_iN_load to i64
  store i64 %move_iN_zext, ptr %local_7, align 8
  store i64 0, ptr %local_8, align 8
  %cmp_lhs = load i64, ptr %local_7, align 8
  %cmp_rhs = load i64, ptr %local_8, align 8
  %cmp_bit = icmp eq i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_9, align 1
  %cond_load = load i8, ptr %local_9, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb5, label %bb8

cancel_exit6:                                     ; preds = %bb4
  ret i64 0

after_cooperate7:                                 ; preds = %bb4
  br label %bb2

cancel_exit15:                                    ; preds = %bb6
  ret i64 0

after_cooperate16:                                ; preds = %bb6
  br label %bb3

cancel_exit26:                                    ; preds = %bb10
  ret i64 0

after_cooperate27:                                ; preds = %bb10
  br label %bb4

cancel_exit30:                                    ; preds = %bb11
  ret i64 0

after_cooperate31:                                ; preds = %bb11
  br label %bb4
}

define internal %Holder @replace_holder(%Holder %0, %Holder %1) {
entry:
  %return_slot = alloca %Holder, align 8
  %local_0 = alloca %Holder, align 8
  %local_1 = alloca %Holder, align 8
  %local_2 = alloca %Holder, align 8
  store %Holder %0, ptr %local_0, align 8
  store %Holder %1, ptr %local_1, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  %move_load = load %Holder, ptr %local_0, align 8
  store %Holder %move_load, ptr %local_2, align 8
  %rfd_0_gep = getelementptr inbounds nuw %Holder, ptr %local_2, i32 0, i32 0
  %rfd_0_raw_load = load ptr, ptr %rfd_0_gep, align 4
  call void @hew_vec_free_owned(ptr %rfd_0_raw_load)
  store ptr null, ptr %rfd_0_gep, align 4
  %move_load1 = load %Holder, ptr %local_1, align 8
  store %Holder %move_load1, ptr %local_2, align 8
  %move_load2 = load %Holder, ptr %local_2, align 8
  store %Holder %move_load2, ptr %return_slot, align 8
  %ret_val = load %Holder, ptr %return_slot, align 8
  ret %Holder %ret_val
}

define internal i64 @reassign_field(i64 %0) {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca i64, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca ptr, align 4
  %local_3 = alloca i64, align 8
  %local_4 = alloca %Holder, align 8
  %local_5 = alloca %Holder, align 8
  %local_6 = alloca i64, align 8
  %local_7 = alloca i64, align 8
  %local_8 = alloca i64, align 8
  %local_9 = alloca i64, align 8
  %local_10 = alloca i64, align 8
  %local_11 = alloca i8, align 1
  %local_12 = alloca i64, align 8
  %local_13 = alloca i8, align 1
  %local_14 = alloca ptr, align 4
  %local_15 = alloca i64, align 8
  %local_16 = alloca i64, align 8
  %local_17 = alloca i8, align 1
  %local_18 = alloca %Holder, align 8
  %local_19 = alloca %Holder, align 8
  %local_20 = alloca %Holder, align 8
  %local_21 = alloca i8, align 1
  %local_22 = alloca i64, align 8
  store i64 %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  store i64 0, ptr %local_1, align 8
  %call_arg = load i64, ptr %local_1, align 8
  %call_result = call ptr @make_rows(i64 %call_arg)
  store ptr %call_result, ptr %local_2, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  store i64 0, ptr %local_3, align 8
  %field_0_init_ptr = getelementptr inbounds nuw %Holder, ptr %local_4, i32 0, i32 0
  %field_0_init_src = load ptr, ptr %local_2, align 4
  store ptr %field_0_init_src, ptr %field_0_init_ptr, align 4
  %field_1_init_ptr = getelementptr inbounds nuw %Holder, ptr %local_4, i32 0, i32 1
  %field_1_init_src = load i64, ptr %local_3, align 8
  store i64 %field_1_init_src, ptr %field_1_init_ptr, align 8
  %move_load = load %Holder, ptr %local_4, align 8
  store %Holder %move_load, ptr %local_5, align 8
  store i64 1, ptr %local_8, align 8
  %move_load1 = load i64, ptr %local_8, align 8
  store i64 %move_load1, ptr %local_9, align 8
  store i64 0, ptr %local_10, align 8
  %cmp_lhs = load i64, ptr %local_9, align 8
  %cmp_rhs = load i64, ptr %local_10, align 8
  %cmp_bit = icmp sle i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_11, align 1
  %cond_load = load i8, ptr %local_11, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb6, label %bb7

bb2:                                              ; preds = %after_cooperate18, %bb4
  %cmp_lhs2 = load i64, ptr %local_6, align 8
  %cmp_rhs3 = load i64, ptr %local_7, align 8
  %cmp_bit4 = icmp slt i64 %cmp_lhs2, %cmp_rhs3
  %cmp_zext5 = zext i1 %cmp_bit4 to i8
  store i8 %cmp_zext5, ptr %local_13, align 1
  %cond_load6 = load i8, ptr %local_13, align 1
  %cond_nz7 = icmp ne i8 %cond_load6, 0
  br i1 %cond_nz7, label %bb3, label %bb5

bb3:                                              ; preds = %bb2
  %call_arg8 = load i64, ptr %local_6, align 8
  %call_result9 = call ptr @make_rows(i64 %call_arg8)
  store ptr %call_result9, ptr %local_14, align 4
  br label %bb8

bb4:                                              ; preds = %after_cooperate39
  %checked_lhs = load i64, ptr %local_6, align 8
  %checked_rhs = load i64, ptr %local_9, align 8
  %with_overflow = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_6, align 8
  store i8 %checked_overflow_widen, ptr %local_21, align 1
  %cond_load10 = load i8, ptr %local_21, align 1
  %cond_nz11 = icmp ne i8 %cond_load10, 0
  br i1 %cond_nz11, label %bb12, label %bb2

bb5:                                              ; preds = %bb2
  %field_1_load_ptr = getelementptr inbounds nuw %Holder, ptr %local_5, i32 0, i32 1
  %field_1_load = load i64, ptr %field_1_load_ptr, align 8
  store i64 %field_1_load, ptr %local_22, align 8
  %move_load12 = load i64, ptr %local_22, align 8
  store i64 %move_load12, ptr %return_slot, align 8
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val

bb6:                                              ; preds = %bb1
  call void @hew_trap_with_code(i32 202)
  call void @llvm.trap()
  unreachable

bb7:                                              ; preds = %bb1
  store i64 0, ptr %local_12, align 8
  %move_load13 = load i64, ptr %local_12, align 8
  store i64 %move_load13, ptr %local_6, align 8
  %move_load14 = load i64, ptr %local_0, align 8
  store i64 %move_load14, ptr %local_7, align 8
  %hew_actor_cooperate15 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel16 = icmp eq i32 %hew_actor_cooperate15, 2
  br i1 %hew_cooperate_is_cancel16, label %cancel_exit17, label %after_cooperate18

bb8:                                              ; preds = %bb3
  store i64 1, ptr %local_15, align 8
  %checked_lhs19 = load i64, ptr %local_6, align 8
  %checked_rhs20 = load i64, ptr %local_15, align 8
  %with_overflow21 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs19, i64 %checked_rhs20)
  %checked_result22 = extractvalue { i64, i1 } %with_overflow21, 0
  %checked_overflow23 = extractvalue { i64, i1 } %with_overflow21, 1
  %checked_overflow_widen24 = zext i1 %checked_overflow23 to i8
  store i64 %checked_result22, ptr %local_16, align 8
  store i8 %checked_overflow_widen24, ptr %local_17, align 1
  %cond_load25 = load i8, ptr %local_17, align 1
  %cond_nz26 = icmp ne i8 %cond_load25, 0
  br i1 %cond_nz26, label %bb9, label %bb10

bb9:                                              ; preds = %bb8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb10:                                             ; preds = %bb8
  %field_0_init_ptr27 = getelementptr inbounds nuw %Holder, ptr %local_18, i32 0, i32 0
  %field_0_init_src28 = load ptr, ptr %local_14, align 4
  store ptr %field_0_init_src28, ptr %field_0_init_ptr27, align 4
  %field_1_init_ptr29 = getelementptr inbounds nuw %Holder, ptr %local_18, i32 0, i32 1
  %field_1_init_src30 = load i64, ptr %local_16, align 8
  store i64 %field_1_init_src30, ptr %field_1_init_ptr29, align 8
  %move_load31 = load %Holder, ptr %local_18, align 8
  store %Holder %move_load31, ptr %local_19, align 8
  %call_arg32 = load %Holder, ptr %local_5, align 8
  %call_arg33 = load %Holder, ptr %local_19, align 8
  %call_result34 = call %Holder @replace_holder(%Holder %call_arg32, %Holder %call_arg33)
  store %Holder %call_result34, ptr %local_20, align 8
  br label %bb11

bb11:                                             ; preds = %bb10
  %move_load35 = load %Holder, ptr %local_20, align 8
  store %Holder %move_load35, ptr %local_5, align 8
  %hew_actor_cooperate36 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel37 = icmp eq i32 %hew_actor_cooperate36, 2
  br i1 %hew_cooperate_is_cancel37, label %cancel_exit38, label %after_cooperate39

bb12:                                             ; preds = %bb4
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

cancel_exit:                                      ; preds = %entry
  ret i64 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit17:                                    ; preds = %bb7
  ret i64 0

after_cooperate18:                                ; preds = %bb7
  br label %bb2

cancel_exit38:                                    ; preds = %bb11
  ret i64 0

after_cooperate39:                                ; preds = %bb11
  br label %bb4
}

define i64 @__original_main() {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca i64, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i64, align 8
  %local_5 = alloca i8, align 1
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  store i64 2, ptr %local_0, align 8
  %call_arg = load i64, ptr %local_0, align 8
  %call_result = call i64 @consume_generator(i64 %call_arg)
  store i64 %call_result, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  store i64 2, ptr %local_2, align 8
  %call_arg1 = load i64, ptr %local_2, align 8
  %call_result2 = call i64 @reassign_field(i64 %call_arg1)
  store i64 %call_result2, ptr %local_3, align 8
  br label %bb2

bb2:                                              ; preds = %bb1
  %checked_lhs = load i64, ptr %local_1, align 8
  %checked_rhs = load i64, ptr %local_3, align 8
  %with_overflow = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_4, align 8
  store i8 %checked_overflow_widen, ptr %local_5, align 1
  %cond_load = load i8, ptr %local_5, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb3, label %bb4

bb3:                                              ; preds = %bb2
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb4:                                              ; preds = %bb2
  %move_load = load i64, ptr %local_4, align 8
  store i64 %move_load, ptr %return_slot, align 8
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val

cancel_exit:                                      ; preds = %entry
  ret i64 0

after_cooperate:                                  ; preds = %entry
  br label %bb0
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
  store ptr @str_lit, ptr %local_3, align 4
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

define i64 @main() {
entry:
  %__original_main_call = call i64 @__original_main()
  ret i64 %__original_main_call
}

define internal i32 @__hew_record_clone_inplace_HeapRow(ptr %0, ptr %1) {
entry:
  br label %step_0_clone

success:                                          ; preds = %step_0_store
  ret i32 0

fail:                                             ; preds = %rb_step_0
  ret i32 1

rb_step_0:                                        ; preds = %step_0_clone
  br label %fail

step_0_store:                                     ; preds = %step_0_clone
  %dst_f0_ptr = getelementptr inbounds nuw %HeapRow, ptr %1, i32 0, i32 0
  store ptr %clone_helper_f0, ptr %dst_f0_ptr, align 4
  br label %success

step_0_clone:                                     ; preds = %entry
  %src_f0_ptr = getelementptr inbounds nuw %HeapRow, ptr %0, i32 0, i32 0
  %src_f0 = load ptr, ptr %src_f0_ptr, align 4
  %clone_helper_f0 = call ptr @hew_vec_clone_owned(ptr %src_f0)
  %cloned_f0_int = ptrtoint ptr %clone_helper_f0 to i64
  %cloned_f0_null = icmp eq i64 %cloned_f0_int, 0
  br i1 %cloned_f0_null, label %rb_step_0, label %step_0_store
}

declare ptr @hew_vec_clone_owned(ptr)

define internal void @__hew_record_drop_inplace_HeapRow(ptr %0) {
entry:
  %rec_int = ptrtoint ptr %0 to i64
  %rec_is_null = icmp eq i64 %rec_int, 0
  br i1 %rec_is_null, label %done, label %do_drop

do_drop:                                          ; preds = %entry
  %drop_f0_ptr = getelementptr inbounds nuw %HeapRow, ptr %0, i32 0, i32 0
  %drop_f0 = load ptr, ptr %drop_f0_ptr, align 4
  call void @hew_vec_free_owned(ptr %drop_f0)
  br label %done

done:                                             ; preds = %do_drop, %entry
  ret void
}

declare void @hew_vec_free_owned(ptr)

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

define internal void @__hew_record_overwrite_release_HeapRow(ptr %0, ptr %1) {
entry:
  %ow_slot_0 = alloca ptr, align 4
  store ptr null, ptr %ow_slot_0, align 4
  %ow_new_d0_f0_ptr = getelementptr inbounds nuw %HeapRow, ptr %1, i32 0, i32 0
  %ow_new_d0_f0_leaf = load ptr, ptr %ow_new_d0_f0_ptr, align 4
  store ptr %ow_new_d0_f0_leaf, ptr %ow_slot_0, align 4
  %ow_old_d0_f0_ptr = getelementptr inbounds nuw %HeapRow, ptr %0, i32 0, i32 0
  %ow_old_d0_f0_val = load ptr, ptr %ow_old_d0_f0_ptr, align 4
  %ow_old_d0_f0_int = ptrtoint ptr %ow_old_d0_f0_val to i64
  %ow_old_d0_f0_cmp0_leaf = load ptr, ptr %ow_slot_0, align 4
  %ow_old_d0_f0_cmp0_int = ptrtoint ptr %ow_old_d0_f0_cmp0_leaf to i64
  %ow_old_d0_f0_cmp0_eq = icmp eq i64 %ow_old_d0_f0_int, %ow_old_d0_f0_cmp0_int
  %ow_old_d0_f0_matched0 = or i1 false, %ow_old_d0_f0_cmp0_eq
  %ow_old_d0_f0_neutralized = select i1 %ow_old_d0_f0_matched0, ptr null, ptr %ow_old_d0_f0_val
  store ptr %ow_old_d0_f0_neutralized, ptr %ow_old_d0_f0_ptr, align 4
  call void @__hew_record_drop_inplace_HeapRow(ptr %0)
  ret void
}

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

declare ptr @hew_vec_new_with_elem_layout(ptr)

declare ptr @hew_vec_new_i64()

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64) #1

declare void @hew_trap_with_code(i32)

; Function Attrs: cold noreturn nounwind memory(inaccessiblemem: write)
declare void @llvm.trap() #2

declare void @hew_vec_push_owned_move(ptr, ptr)

declare ptr @hew_cont_frame_alloc(i64)

define internal void @__hew_gen_out_drop___hew_gen_body_batches_0(ptr %0) {
entry:
  %gen_out_drop_value_ptr = getelementptr inbounds nuw { ptr, ptr, ptr, i8, i8, ptr }, ptr %0, i32 0, i32 5
  %gen_out_drop_hew_vec_free_owned_hdl = load ptr, ptr %gen_out_drop_value_ptr, align 4
  call void @hew_vec_free_owned(ptr %gen_out_drop_hew_vec_free_owned_hdl)
  store ptr null, ptr %gen_out_drop_value_ptr, align 4
  ret void
}

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias writeonly captures(none), ptr noalias readonly captures(none), i64, i1 immarg) #3

declare i1 @hew_cont_done(ptr)

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: read)
declare token @llvm.coro.id(i32, ptr readnone, ptr readonly captures(none), ptr) #4

; Function Attrs: nounwind
declare i1 @llvm.coro.alloc(token) #5

; Function Attrs: nounwind memory(none)
declare i64 @llvm.coro.size.i64() #6

; Function Attrs: nounwind
declare ptr @llvm.coro.begin(token, ptr writeonly) #5

; Function Attrs: nounwind
declare i8 @llvm.coro.suspend(token, i1) #5

; Function Attrs: nomerge nounwind
declare token @llvm.coro.save(ptr) #7

; Function Attrs: nounwind memory(argmem: read)
declare ptr @llvm.coro.free(token, ptr readonly captures(none)) #8

declare void @hew_cont_frame_free(ptr)

; Function Attrs: nounwind
declare void @llvm.coro.end(ptr, i1, token) #5

declare void @hew_cont_resume(ptr)

define void @hew_gen_coro_destroy(ptr %0) {
entry:
  %gen_destroy_null = icmp eq ptr %0, null
  br i1 %gen_destroy_null, label %done, label %do_destroy

do_destroy:                                       ; preds = %entry
  %gen_destroy_handle = load ptr, ptr %0, align 4
  %gen_destroy_handle_null = icmp eq ptr %gen_destroy_handle, null
  br i1 %gen_destroy_handle_null, label %after_frame, label %destroy_frame

pending_check:                                    ; preds = %after_frame
  %gen_destroy_pending_slot = getelementptr inbounds i8, ptr %0, i64 13
  %gen_destroy_pending = load i8, ptr %gen_destroy_pending_slot, align 1
  %gen_destroy_pending_live = icmp ne i8 %gen_destroy_pending, 0
  br i1 %gen_destroy_pending_live, label %out_drop_call, label %free_companion

out_drop_call:                                    ; preds = %pending_check
  %gen_destroy_thunk_slot = getelementptr inbounds i8, ptr %0, i64 8
  %gen_destroy_thunk = load ptr, ptr %gen_destroy_thunk_slot, align 4
  %gen_destroy_thunk_null = icmp eq ptr %gen_destroy_thunk, null
  br i1 %gen_destroy_thunk_null, label %free_companion, label %out_drop_invoke

free_companion:                                   ; preds = %out_drop_invoke, %out_drop_call, %pending_check
  call void @hew_cont_frame_free(ptr %0)
  br label %done

done:                                             ; preds = %free_companion, %entry
  ret void

destroy_frame:                                    ; preds = %do_destroy
  call void @llvm.coro.destroy(ptr %gen_destroy_handle)
  br label %after_frame

after_frame:                                      ; preds = %destroy_frame, %do_destroy
  %gen_destroy_env_slot = getelementptr inbounds i8, ptr %0, i64 4
  %gen_destroy_env = load ptr, ptr %gen_destroy_env_slot, align 4
  call void @hew_cont_frame_free(ptr %gen_destroy_env)
  br label %pending_check

out_drop_invoke:                                  ; preds = %out_drop_call
  call void %gen_destroy_thunk(ptr %0)
  br label %free_companion
}

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.smul.with.overflow.i64(i64, i64) #1

declare void @llvm.coro.destroy(ptr)

attributes #0 = { presplitcoroutine }
attributes #1 = { nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none) }
attributes #2 = { cold noreturn nounwind memory(inaccessiblemem: write) }
attributes #3 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
attributes #4 = { nocallback nofree nosync nounwind willreturn memory(argmem: read) }
attributes #5 = { nounwind }
attributes #6 = { nounwind memory(none) }
attributes #7 = { nomerge nounwind }
attributes #8 = { nounwind memory(argmem: read) }
