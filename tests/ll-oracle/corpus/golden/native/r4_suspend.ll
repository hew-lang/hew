; ModuleID = 'r4_suspend'
source_filename = "r4_suspend"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "aarch64-apple-macosx13.0"

%Adder = type { i64 }
%"Result$$i64$AskError" = type { i8, [1 x i64] }
%AskError = type { i8, [1 x i8] }
%CrashInfo = type { i64, ptr }

@str_actor_meta_name_Adder = private unnamed_addr constant [6 x i8] c"Adder\00", align 1
@str_actor_meta_handler_Adder_0_set_base = private unnamed_addr constant [9 x i8] c"set_base\00", align 1
@str_actor_meta_handler_Adder_1_add = private unnamed_addr constant [4 x i8] c"add\00", align 1
@str_actor_type_name_Adder = private unnamed_addr constant [6 x i8] c"Adder\00", align 1
@str_actor_handler_name_Adder_0_set_base = private unnamed_addr constant [16 x i8] c"Adder::set_base\00", align 1
@str_actor_handler_name_Adder_1_add = private unnamed_addr constant [11 x i8] c"Adder::add\00", align 1
@str_lit = private unnamed_addr constant [3 x i8] c"ns\00", align 1
@llvm.global_ctors = appending global [1 x { i32, ptr, ptr }] [{ i32, ptr, ptr } { i32 65535, ptr @hew_module_init_actor_codecs, ptr null }]

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

declare i64 @hew_remote_pid_from_raw(i64, i64)

declare i64 @hew_node_api_lookup(ptr)

declare ptr @hew_stream_channel(i64)

declare ptr @hew_stream_pair_sink(ptr)

declare ptr @hew_stream_pair_stream(ptr)

declare void @hew_stream_pair_free(ptr)

declare void @hew_sink_close(ptr)

declare i32 @hew_sink_peer_closed(ptr)

declare void @hew_actor_gen_sink_register(ptr, ptr)

declare void @hew_actor_gen_sink_complete(ptr, ptr)

define internal i8 @Adder__recv__set_base(ptr %0, i64 %1, i32 %2) {
entry:
  %return_slot = alloca i8, align 1
  %local_0 = alloca i64, align 8
  store i64 %1, ptr %local_0, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  %ctx_actor_ptr_slot = getelementptr i8, ptr %0, i64 0
  %ctx_actor_ptr = load ptr, ptr %ctx_actor_ptr_slot, align 8
  %actor_state_slot = getelementptr i8, ptr %ctx_actor_ptr, i64 16
  %actor_state_ptr = load ptr, ptr %actor_state_slot, align 8
  %actor_state_field_0_ptr = getelementptr inbounds nuw %Adder, ptr %actor_state_ptr, i32 0, i32 0
  %actor_state_field_0_src = load i64, ptr %local_0, align 8
  store i64 %actor_state_field_0_src, ptr %actor_state_field_0_ptr, align 8
  ret i8 0
}

define internal i64 @Adder__recv__add(ptr %0, i64 %1, i32 %2) {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca i64, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i8, align 1
  store i64 %1, ptr %local_0, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  %ctx_actor_ptr_slot = getelementptr i8, ptr %0, i64 0
  %ctx_actor_ptr = load ptr, ptr %ctx_actor_ptr_slot, align 8
  %actor_state_slot = getelementptr i8, ptr %ctx_actor_ptr, i64 16
  %actor_state_ptr = load ptr, ptr %actor_state_slot, align 8
  %actor_state_field_0_ptr = getelementptr inbounds nuw %Adder, ptr %actor_state_ptr, i32 0, i32 0
  %actor_state_field_0 = load i64, ptr %actor_state_field_0_ptr, align 8
  store i64 %actor_state_field_0, ptr %local_1, align 8
  %checked_lhs = load i64, ptr %local_1, align 8
  %checked_rhs = load i64, ptr %local_0, align 8
  %with_overflow = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
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

define i8 @main() {
entry:
  %return_slot = alloca i8, align 1
  %local_0 = alloca %Adder, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca ptr, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i64, align 8
  %local_5 = alloca %"Result$$i64$AskError", align 8
  %local_6 = alloca i64, align 8
  %local_7 = alloca %AskError, align 8
  %local_8 = alloca %"Result$$i64$AskError", align 8
  %local_9 = alloca i8, align 1
  %local_10 = alloca i64, align 8
  %local_11 = alloca i64, align 8
  %local_12 = alloca i8, align 1
  %local_13 = alloca i64, align 8
  %local_14 = alloca i8, align 1
  %local_15 = alloca i64, align 8
  %local_16 = alloca i64, align 8
  %hew_sched_init_entry_call = call i32 @hew_sched_init()
  br label %bb0

bb0:                                              ; preds = %entry
  store i64 10, ptr %local_1, align 8
  %field_0_init_ptr = getelementptr inbounds nuw %Adder, ptr %local_0, i32 0, i32 0
  %field_0_init_src = load i64, ptr %local_1, align 8
  store i64 %field_0_init_src, ptr %field_0_init_ptr, align 8
  %hew_sched_init_call = call i32 @hew_sched_init()
  %actor_meta_handlers_Adder = alloca [2 x { ptr, i32, i32, ptr, ptr, i32 }], align 8
  %actor_meta_handler_0 = getelementptr [2 x { ptr, i32, i32, ptr, ptr, i32 }], ptr %actor_meta_handlers_Adder, i32 0, i32 0
  %actor_meta_handler_0_f0 = getelementptr inbounds nuw { ptr, i32, i32, ptr, ptr, i32 }, ptr %actor_meta_handler_0, i32 0, i32 0
  store ptr @str_actor_meta_handler_Adder_0_set_base, ptr %actor_meta_handler_0_f0, align 8
  %actor_meta_handler_0_f1 = getelementptr inbounds nuw { ptr, i32, i32, ptr, ptr, i32 }, ptr %actor_meta_handler_0, i32 0, i32 1
  store i32 1995638644, ptr %actor_meta_handler_0_f1, align 4
  %actor_meta_handler_0_f2 = getelementptr inbounds nuw { ptr, i32, i32, ptr, ptr, i32 }, ptr %actor_meta_handler_0, i32 0, i32 2
  store i32 0, ptr %actor_meta_handler_0_f2, align 4
  %actor_meta_handler_0_f3 = getelementptr inbounds nuw { ptr, i32, i32, ptr, ptr, i32 }, ptr %actor_meta_handler_0, i32 0, i32 3
  store ptr null, ptr %actor_meta_handler_0_f3, align 8
  %actor_meta_handler_0_f4 = getelementptr inbounds nuw { ptr, i32, i32, ptr, ptr, i32 }, ptr %actor_meta_handler_0, i32 0, i32 4
  store ptr null, ptr %actor_meta_handler_0_f4, align 8
  %actor_meta_handler_0_f5 = getelementptr inbounds nuw { ptr, i32, i32, ptr, ptr, i32 }, ptr %actor_meta_handler_0, i32 0, i32 5
  store i32 0, ptr %actor_meta_handler_0_f5, align 4
  %actor_meta_handler_1 = getelementptr [2 x { ptr, i32, i32, ptr, ptr, i32 }], ptr %actor_meta_handlers_Adder, i32 0, i32 1
  %actor_meta_handler_1_f0 = getelementptr inbounds nuw { ptr, i32, i32, ptr, ptr, i32 }, ptr %actor_meta_handler_1, i32 0, i32 0
  store ptr @str_actor_meta_handler_Adder_1_add, ptr %actor_meta_handler_1_f0, align 8
  %actor_meta_handler_1_f1 = getelementptr inbounds nuw { ptr, i32, i32, ptr, ptr, i32 }, ptr %actor_meta_handler_1, i32 0, i32 1
  store i32 311929158, ptr %actor_meta_handler_1_f1, align 4
  %actor_meta_handler_1_f2 = getelementptr inbounds nuw { ptr, i32, i32, ptr, ptr, i32 }, ptr %actor_meta_handler_1, i32 0, i32 2
  store i32 0, ptr %actor_meta_handler_1_f2, align 4
  %actor_meta_handler_1_f3 = getelementptr inbounds nuw { ptr, i32, i32, ptr, ptr, i32 }, ptr %actor_meta_handler_1, i32 0, i32 3
  store ptr null, ptr %actor_meta_handler_1_f3, align 8
  %actor_meta_handler_1_f4 = getelementptr inbounds nuw { ptr, i32, i32, ptr, ptr, i32 }, ptr %actor_meta_handler_1, i32 0, i32 4
  store ptr null, ptr %actor_meta_handler_1_f4, align 8
  %actor_meta_handler_1_f5 = getelementptr inbounds nuw { ptr, i32, i32, ptr, ptr, i32 }, ptr %actor_meta_handler_1, i32 0, i32 5
  store i32 0, ptr %actor_meta_handler_1_f5, align 4
  %actor_meta_handlers_ptr = getelementptr [2 x { ptr, i32, i32, ptr, ptr, i32 }], ptr %actor_meta_handlers_Adder, i32 0, i32 0
  %actor_meta_Adder = alloca { ptr, i32, ptr }, align 8
  %actor_meta_f0 = getelementptr inbounds nuw { ptr, i32, ptr }, ptr %actor_meta_Adder, i32 0, i32 0
  store ptr @str_actor_meta_name_Adder, ptr %actor_meta_f0, align 8
  %actor_meta_f1 = getelementptr inbounds nuw { ptr, i32, ptr }, ptr %actor_meta_Adder, i32 0, i32 1
  store i32 2, ptr %actor_meta_f1, align 4
  %actor_meta_f2 = getelementptr inbounds nuw { ptr, i32, ptr }, ptr %actor_meta_Adder, i32 0, i32 2
  store ptr %actor_meta_handlers_ptr, ptr %actor_meta_f2, align 8
  call void @hew_wasm_register_actor_meta(ptr %actor_meta_Adder)
  call void @hew_actor_register_type(ptr @__hew_actor_dispatch_Adder, ptr @str_actor_type_name_Adder)
  call void @hew_register_handler_name(ptr @__hew_actor_dispatch_Adder, i32 1995638644, ptr @str_actor_handler_name_Adder_0_set_base)
  call void @hew_register_handler_name(ptr @__hew_actor_dispatch_Adder, i32 311929158, ptr @str_actor_handler_name_Adder_1_add)
  %hew_actor_spawn_call = call ptr @hew_actor_spawn(ptr %local_0, i64 ptrtoint (ptr getelementptr (%Adder, ptr null, i32 1) to i64), ptr @__hew_actor_dispatch_Adder)
  call void @hew_actor_set_state_drop(ptr %hew_actor_spawn_call, ptr @__hew_state_drop_Adder)
  call void @hew_actor_set_state_clone(ptr %hew_actor_spawn_call, ptr @__hew_state_clone_Adder)
  store ptr %hew_actor_spawn_call, ptr %local_2, align 8
  store i64 100, ptr %local_3, align 8
  %"actor_send receiver" = load ptr, ptr %local_2, align 8
  %actor_id_slot = getelementptr i8, ptr %"actor_send receiver", i64 8
  %actor_id = load i64, ptr %actor_id_slot, align 8
  %hew_actor_send_by_id_call = call i32 @hew_actor_send_by_id(i64 %actor_id, ptr null, i32 1995638644, ptr %local_3, i64 ptrtoint (ptr getelementptr (i64, ptr null, i32 1) to i64))
  %send_not_ok = icmp ne i32 %hew_actor_send_by_id_call, 0
  br i1 %send_not_ok, label %actor_send_fail, label %bb1

bb1:                                              ; preds = %bb0
  store i64 42, ptr %local_4, align 8
  %"actor_ask receiver" = load ptr, ptr %local_2, align 8
  %hew_actor_ask_call = call ptr @hew_actor_ask(ptr %"actor_ask receiver", i32 311929158, ptr %local_4, i64 ptrtoint (ptr getelementptr (i64, ptr null, i32 1) to i64))
  %actor_ask_reply_is_null = icmp eq ptr %hew_actor_ask_call, null
  br i1 %actor_ask_reply_is_null, label %actor_ask_reply_err, label %actor_ask_reply_ok

bb2:                                              ; preds = %actor_ask_reply_err, %actor_ask_reply_ok
  %move_load = load %"Result$$i64$AskError", ptr %local_5, align 8
  store %"Result$$i64$AskError" %move_load, ptr %local_8, align 8
  %machine_tag_ptr5 = getelementptr inbounds nuw %"Result$$i64$AskError", ptr %local_8, i32 0, i32 0
  %move_iN_load = load i8, ptr %machine_tag_ptr5, align 1
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
  br i1 %cond_nz, label %bb4, label %bb7

bb3:                                              ; preds = %after_cooperate20, %after_cooperate
  %move_load6 = load i8, ptr %local_9, align 1
  store i8 %move_load6, ptr %return_slot, align 1
  call void @hew_shutdown_initiate_implicit(i64 0)
  %hew_shutdown_wait_call = call i32 @hew_shutdown_wait()
  %hew_lambda_drain_all_call = call i32 @hew_lambda_drain_all(i64 0)
  call void @hew_runtime_cleanup_after_main()
  ret i8 0

bb4:                                              ; preds = %bb2
  %machine_payload_ptr7 = getelementptr inbounds nuw %"Result$$i64$AskError", ptr %local_8, i32 0, i32 1
  %machine_variant_field_ptr8 = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr7, i32 0, i32 0
  %move_load9 = load i64, ptr %machine_variant_field_ptr8, align 8
  store i64 %move_load9, ptr %local_15, align 8
  %print_arg = load i64, ptr %local_15, align 8
  call void @hew_print_value(i8 1, i64 %print_arg, i1 true)
  br label %bb8

bb5:                                              ; preds = %bb7
  store i64 -1, ptr %local_16, align 8
  %print_arg10 = load i64, ptr %local_16, align 8
  call void @hew_print_value(i8 1, i64 %print_arg10, i1 true)
  br label %bb9

bb6:                                              ; preds = %bb7
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

bb7:                                              ; preds = %bb2
  store i64 1, ptr %local_13, align 8
  %cmp_lhs11 = load i64, ptr %local_10, align 8
  %cmp_rhs12 = load i64, ptr %local_13, align 8
  %cmp_bit13 = icmp eq i64 %cmp_lhs11, %cmp_rhs12
  %cmp_zext14 = zext i1 %cmp_bit13 to i8
  store i8 %cmp_zext14, ptr %local_14, align 1
  %cond_load15 = load i8, ptr %local_14, align 1
  %cond_nz16 = icmp ne i8 %cond_load15, 0
  br i1 %cond_nz16, label %bb5, label %bb6

bb8:                                              ; preds = %bb4
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb9:                                              ; preds = %bb5
  %hew_actor_cooperate17 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel18 = icmp eq i32 %hew_actor_cooperate17, 2
  br i1 %hew_cooperate_is_cancel18, label %cancel_exit19, label %after_cooperate20

actor_send_fail:                                  ; preds = %bb0
  call void @hew_trap_with_code(i32 206)
  call void @llvm.trap()
  unreachable

actor_ask_reply_ok:                               ; preds = %bb1
  %actor_ask_reply_value = load i64, ptr %hew_actor_ask_call, align 8
  store i64 %actor_ask_reply_value, ptr %local_6, align 8
  %machine_tag_ptr = getelementptr inbounds nuw %"Result$$i64$AskError", ptr %local_5, i32 0, i32 0
  store i8 0, ptr %machine_tag_ptr, align 1
  %machine_payload_ptr = getelementptr inbounds nuw %"Result$$i64$AskError", ptr %local_5, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr, i32 0, i32 0
  %emit_result_ok_v0_f0_src = load i64, ptr %local_6, align 8
  store i64 %emit_result_ok_v0_f0_src, ptr %machine_variant_field_ptr, align 8
  call void @free(ptr %hew_actor_ask_call)
  br label %bb2

actor_ask_reply_err:                              ; preds = %bb1
  %hew_actor_ask_take_last_error_call = call i32 @hew_actor_ask_take_last_error()
  %machine_tag_ptr1 = getelementptr inbounds nuw %AskError, ptr %local_7, i32 0, i32 0
  %ask_err_trunc = trunc i32 %hew_actor_ask_take_last_error_call to i8
  store i8 %ask_err_trunc, ptr %machine_tag_ptr1, align 1
  %machine_tag_ptr2 = getelementptr inbounds nuw %"Result$$i64$AskError", ptr %local_5, i32 0, i32 0
  store i8 1, ptr %machine_tag_ptr2, align 1
  %machine_payload_ptr3 = getelementptr inbounds nuw %"Result$$i64$AskError", ptr %local_5, i32 0, i32 1
  %machine_variant_field_ptr4 = getelementptr inbounds nuw { %AskError }, ptr %machine_payload_ptr3, i32 0, i32 0
  %emit_result_err_v1_f0_src = load %AskError, ptr %local_7, align 1
  store %AskError %emit_result_err_v1_f0_src, ptr %machine_variant_field_ptr4, align 1
  br label %bb2

cancel_exit:                                      ; preds = %bb8
  ret i8 0

after_cooperate:                                  ; preds = %bb8
  br label %bb3

cancel_exit19:                                    ; preds = %bb9
  ret i8 0

after_cooperate20:                                ; preds = %bb9
  br label %bb3
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

define internal ptr @__hew_actor_dispatch_Adder(ptr %0, ptr %1, i32 %2, ptr %3, i64 %4, i32 %5) {
entry:
  %dispatch_is_borrow = icmp ne i32 %5, 0
  br i1 %dispatch_is_borrow, label %borrow_src, label %copy_src

unknown_msg_type:                                 ; preds = %payload_src
  call void @llvm.trap()
  unreachable

dispatch_done:                                    ; preds = %msg_sys_exit_unhandled, %msg_311929158, %msg_1995638644
  %dispatch_suspend_handle = phi ptr [ null, %msg_1995638644 ], [ null, %msg_311929158 ], [ null, %msg_sys_exit_unhandled ]
  ret ptr %dispatch_suspend_handle

borrow_src:                                       ; preds = %entry
  %envelope_payload_ptr = call ptr @hew_msg_envelope_payload_ptr(ptr %3)
  %borrow_payload_is_null = icmp eq ptr %envelope_payload_ptr, null
  br i1 %borrow_payload_is_null, label %borrow_payload_null, label %borrow_payload_ok

copy_src:                                         ; preds = %entry
  br label %payload_src

payload_src:                                      ; preds = %copy_src, %borrow_payload_ok
  %payload_src_ptr = phi ptr [ %envelope_payload_ptr, %borrow_payload_ok ], [ %3, %copy_src ]
  switch i32 %2, label %unknown_msg_type [
    i32 1995638644, label %msg_1995638644
    i32 311929158, label %msg_311929158
    i32 103, label %msg_sys_exit_unhandled
  ]

borrow_payload_null:                              ; preds = %borrow_src
  call void @hew_panic()
  unreachable

borrow_payload_ok:                                ; preds = %borrow_src
  br label %payload_src

msg_1995638644:                                   ; preds = %payload_src
  %msg_arg_0 = load i64, ptr %payload_src_ptr, align 8
  %call_set_base = call i8 @Adder__recv__set_base(ptr %0, i64 %msg_arg_0, i32 %5)
  br label %dispatch_done

msg_311929158:                                    ; preds = %payload_src
  %msg_arg_01 = load i64, ptr %payload_src_ptr, align 8
  %call_add = call i64 @Adder__recv__add(ptr %0, i64 %msg_arg_01, i32 %5)
  %hew_get_reply_channel_call = call ptr @hew_get_reply_channel()
  %actor_reply_slot = alloca i64, align 8
  store i64 %call_add, ptr %actor_reply_slot, align 8
  %hew_reply_call = call i1 @hew_reply(ptr %hew_get_reply_channel_call, ptr %actor_reply_slot, i64 ptrtoint (ptr getelementptr (i64, ptr null, i32 1) to i64))
  br label %dispatch_done

msg_sys_exit_unhandled:                           ; preds = %payload_src
  %exit_reason_ptr = getelementptr inbounds nuw { i64, i32, i32 }, ptr %payload_src_ptr, i32 0, i32 1
  %exit_reason = load i32, ptr %exit_reason_ptr, align 4
  call void @hew_actor_exit_unhandled(i32 %exit_reason)
  br label %dispatch_done
}

declare ptr @hew_msg_envelope_payload_ptr(ptr)

declare void @hew_panic()

declare ptr @hew_get_reply_channel()

declare i1 @hew_reply(ptr, ptr, i64)

declare void @hew_actor_exit_unhandled(i32)

; Function Attrs: cold noreturn nounwind memory(inaccessiblemem: write)
declare void @llvm.trap() #0

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

define ptr @__hew_state_clone_Adder(ptr %0) {
entry:
  %src_as_int = ptrtoint ptr %0 to i64
  %src_is_null = icmp eq i64 %src_as_int, 0
  br i1 %src_is_null, label %ret_null, label %alloc

ret_null:                                         ; preds = %alloc, %entry
  ret ptr null

alloc:                                            ; preds = %entry
  %state_wrapper = call ptr @malloc(i64 8)
  %dst_as_int = ptrtoint ptr %state_wrapper to i64
  %dst_is_null = icmp eq i64 %dst_as_int, 0
  br i1 %dst_is_null, label %ret_null, label %memcpy_wholesale

memcpy_wholesale:                                 ; preds = %alloc
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %state_wrapper, ptr align 8 %0, i64 8, i1 false)
  br label %success

success:                                          ; preds = %memcpy_wholesale
  ret ptr %state_wrapper
}

declare ptr @malloc(i64)

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias writeonly captures(none), ptr noalias readonly captures(none), i64, i1 immarg) #1

define void @__hew_state_drop_Adder(ptr %0) {
entry:
  %state_int = ptrtoint ptr %0 to i64
  %state_is_null = icmp eq i64 %state_int, 0
  br i1 %state_is_null, label %done, label %do_drop

do_drop:                                          ; preds = %entry
  br label %done

done:                                             ; preds = %do_drop, %entry
  ret void
}

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64) #2

declare void @hew_trap_with_code(i32)

declare i32 @hew_sched_init()

declare void @hew_wasm_register_actor_meta(ptr)

declare void @hew_actor_register_type(ptr, ptr)

declare void @hew_register_handler_name(ptr, i32, ptr)

declare ptr @hew_actor_spawn(ptr, i64, ptr)

declare void @hew_actor_set_state_drop(ptr, ptr)

declare void @hew_actor_set_state_clone(ptr, ptr)

declare i32 @hew_actor_send_by_id(i64, ptr, i32, ptr, i64)

declare ptr @hew_actor_ask(ptr, i32, ptr, i64)

declare void @free(ptr)

declare i32 @hew_actor_ask_take_last_error()

declare void @hew_shutdown_initiate_implicit(i64)

declare i32 @hew_shutdown_wait()

declare i32 @hew_lambda_drain_all(i64)

declare void @hew_runtime_cleanup_after_main()

declare i32 @hew_actor_cooperate()

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.smul.with.overflow.i64(i64, i64) #2

define internal ptr @__hew_cbor_serialize_i64(ptr %0, ptr %1) {
entry:
  %cbor_ser_buf = call ptr @hew_cbor_ser_new()
  %cbor_ser_scalar = load i64, ptr %0, align 8
  call void @hew_cbor_ser_i64(ptr %cbor_ser_buf, i64 %cbor_ser_scalar)
  %cbor_ser_finish = call ptr @hew_cbor_ser_finish(ptr %cbor_ser_buf, ptr %1)
  ret ptr %cbor_ser_finish
}

define internal ptr @__hew_cbor_deserialize_i64(ptr %0, i64 %1, ptr %2) {
entry:
  %cbor_de_reader = call ptr @hew_cbor_de_new(ptr %0, i64 %1)
  store i64 ptrtoint (ptr getelementptr (i64, ptr null, i32 1) to i64), ptr %2, align 8
  %cbor_de_value = call ptr @malloc(i64 ptrtoint (ptr getelementptr (i64, ptr null, i32 1) to i64))
  %cbor_dst_as_int = ptrtoint ptr %cbor_de_value to i64
  %cbor_dst_is_null = icmp eq i64 %cbor_dst_as_int, 0
  br i1 %cbor_dst_is_null, label %cbor_de_oom, label %cbor_de_alloc_ok

cbor_de_oom:                                      ; preds = %entry
  call void @hew_cbor_de_free(ptr %cbor_de_reader)
  ret ptr null

cbor_de_alloc_ok:                                 ; preds = %entry
  %cbor_de_zero = call ptr @memset(ptr %cbor_de_value, i32 0, i64 ptrtoint (ptr getelementptr (i64, ptr null, i32 1) to i64))
  %cbor_de_int = call i64 @hew_cbor_de_int_checked(ptr %cbor_de_reader, i32 64, i32 1)
  store i64 %cbor_de_int, ptr %cbor_de_value, align 8
  %cbor_de_failed = call i32 @hew_cbor_de_failed(ptr %cbor_de_reader)
  call void @hew_cbor_de_free(ptr %cbor_de_reader)
  %cbor_de_is_failed = icmp ne i32 %cbor_de_failed, 0
  br i1 %cbor_de_is_failed, label %cbor_de_fail, label %cbor_de_ok

cbor_de_ok:                                       ; preds = %cbor_de_alloc_ok
  ret ptr %cbor_de_value

cbor_de_fail:                                     ; preds = %cbor_de_alloc_ok
  call void @free(ptr %cbor_de_value)
  ret ptr null
}

declare ptr @hew_cbor_ser_new()

declare void @hew_cbor_ser_i64(ptr, i64)

declare ptr @hew_cbor_ser_finish(ptr, ptr)

declare ptr @hew_cbor_de_new(ptr, i64)

declare void @hew_cbor_de_free(ptr)

declare ptr @memset(ptr, i32, i64)

declare i64 @hew_cbor_de_int_checked(ptr, i32, i32)

declare i32 @hew_cbor_de_failed(ptr)

define private void @hew_module_init_actor_codecs() {
entry:
  call void @hew_xnode_register_codec(ptr @__hew_actor_dispatch_Adder, i32 1995638644, ptr @__hew_cbor_serialize_i64, ptr @__hew_cbor_deserialize_i64)
  call void @hew_xnode_register_codec(ptr @__hew_actor_dispatch_Adder, i32 311929158, ptr @__hew_cbor_serialize_i64, ptr @__hew_cbor_deserialize_i64)
  call void @hew_xnode_register_reply_codec(ptr @__hew_actor_dispatch_Adder, i32 311929158, ptr @__hew_cbor_serialize_i64, ptr @__hew_cbor_deserialize_i64)
  ret void
}

declare void @hew_xnode_register_codec(ptr, i32, ptr, ptr)

declare void @hew_xnode_register_reply_codec(ptr, i32, ptr, ptr)

attributes #0 = { cold noreturn nounwind memory(inaccessiblemem: write) }
attributes #1 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
attributes #2 = { nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none) }
