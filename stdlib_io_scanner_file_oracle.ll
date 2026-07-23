; ModuleID = 'stdlib_io_scanner_file_oracle'
source_filename = "stdlib_io_scanner_file_oracle"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "aarch64-apple-macosx13.0"

%"Option$$string" = type { i8, [1 x i64] }
%"Result$$Scanner$IoError" = type { i8, [4 x i64] }
%Scanner = type { ptr, i64, ptr, i8, i8, %SplitMode }
%SplitMode = type { i8, [1 x i8] }
%IoError = type { i8, [1 x i64] }
%"Result$$string$IoError" = type { i8, [2 x i64] }
%"Result$$i64$IoError" = type { i8, [2 x i64] }
%"Result$$bytes$IoError" = type { i8, [2 x i64] }
%"Result$$Vec$lstring$g$IoError" = type { i8, [2 x i64] }
%"Option$$u8" = type { i8, [1 x i8] }
%"Option$$i64" = type { i8, [1 x i64] }
%CrashInfo = type { i64, ptr }

@str_lit = private unnamed_addr constant [6 x i8] c"<eof>\00", align 1
@str_lit.1 = private unnamed_addr constant [40 x i8] c".tmp/stdlib-io-scanner-oracle-input.txt\00", align 1
@str_lit.2 = private unnamed_addr constant [11 x i8] c"red\0A\0Ablue\0A\00", align 1
@str_lit.3 = private unnamed_addr constant [10 x i8] c"file_err=\00", align 1
@str_lit.4 = private unnamed_addr constant [7 x i8] c"first=\00", align 1
@str_lit.5 = private unnamed_addr constant [7 x i8] c"blank=\00", align 1
@str_lit.6 = private unnamed_addr constant [7 x i8] c"third=\00", align 1
@str_lit.7 = private unnamed_addr constant [5 x i8] c"eof=\00", align 1
@str_lit.8 = private unnamed_addr constant [46 x i8] c".tmp/stdlib-io-scanner-definitely-missing.txt\00", align 1
@str_lit.9 = private unnamed_addr constant [22 x i8] c"missing=unexpected-ok\00", align 1
@str_lit.10 = private unnamed_addr constant [9 x i8] c"missing=\00", align 1
@str_lit.11 = private unnamed_addr constant [10 x i8] c"NotFound(\00", align 1
@str_lit.12 = private unnamed_addr constant [18 x i8] c"PermissionDenied(\00", align 1
@str_lit.13 = private unnamed_addr constant [15 x i8] c"AlreadyExists(\00", align 1
@str_lit.14 = private unnamed_addr constant [10 x i8] c"TimedOut(\00", align 1
@str_lit.15 = private unnamed_addr constant [11 x i8] c"Cancelled(\00", align 1
@str_lit.16 = private unnamed_addr constant [7 x i8] c"Other(\00", align 1
@str_lit.17 = private unnamed_addr constant [2 x i8] c")\00", align 1
@str_lit.18 = private unnamed_addr constant [2 x i8] c")\00", align 1
@str_lit.19 = private unnamed_addr constant [2 x i8] c")\00", align 1
@str_lit.20 = private unnamed_addr constant [2 x i8] c")\00", align 1
@str_lit.21 = private unnamed_addr constant [2 x i8] c")\00", align 1
@str_lit.22 = private unnamed_addr constant [2 x i8] c")\00", align 1
@str_lit.23 = private unnamed_addr constant [21 x i8] c"fs.read failed for `\00", align 1
@str_lit.24 = private unnamed_addr constant [21 x i8] c"fs.read failed for `\00", align 1
@str_lit.25 = private unnamed_addr constant [4 x i8] c"`: \00", align 1
@str_lit.26 = private unnamed_addr constant [4 x i8] c"`: \00", align 1
@str_lit.27 = private unnamed_addr constant [1 x i8] zeroinitializer, align 1
@str_lit.28 = private unnamed_addr constant [1 x i8] zeroinitializer, align 1
@str_lit.29 = private unnamed_addr constant [1 x i8] zeroinitializer, align 1
@str_lit.30 = private unnamed_addr constant [1 x i8] zeroinitializer, align 1
@str_lit.31 = private unnamed_addr constant [1 x i8] zeroinitializer, align 1
@str_lit.32 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1
@str_lit.33 = private unnamed_addr constant [2 x i8] c"\0D\00", align 1
@str_lit.34 = private unnamed_addr constant [2 x i8] c" \00", align 1
@str_lit.35 = private unnamed_addr constant [2 x i8] c"\09\00", align 1
@str_lit.36 = private unnamed_addr constant [2 x i8] c"\0D\00", align 1
@str_lit.37 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1
@str_lit.38 = private unnamed_addr constant [1 x i8] zeroinitializer, align 1
@str_lit.39 = private unnamed_addr constant [3 x i8] c"ns\00", align 1

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

declare ptr @hew_file_read(ptr)

declare i32 @hew_file_write(ptr, ptr)

declare i32 @hew_file_append(ptr, ptr)

declare i8 @hew_file_exists(ptr)

declare i32 @hew_file_delete(ptr)

declare i64 @hew_file_size(ptr)

declare [2 x i64] @hew_file_read_bytes(ptr)

declare i32 @hew_file_write_bytes(ptr, ptr)

declare i32 @hew_fs_mkdir(ptr)

declare i32 @hew_fs_mkdir_all(ptr)

declare ptr @hew_fs_list_dir(ptr)

declare i32 @hew_fs_rename(ptr, ptr)

declare i32 @hew_fs_copy(ptr, ptr)

declare i8 @hew_fs_is_dir(ptr)

declare ptr @hew_stream_from_file_read(ptr)

declare i8 @hew_stream_is_valid(ptr)

declare ptr @hew_stream_last_error()

declare i32 @hew_stream_last_errno()

declare i32 @hew_stream_last_error_kind()

declare ptr @hew_stream_collect_string(ptr)

declare ptr @hew_io_read_line()

declare void @hew_io_write(ptr)

declare void @hew_io_write_err(ptr)

declare ptr @hew_io_read_all()

define internal i8 @print_line(ptr %0, %"Option$$string" %1) {
entry:
  %return_slot = alloca i8, align 1
  %local_0 = alloca ptr, align 8
  %local_1 = alloca %"Option$$string", align 8
  %local_2 = alloca i8, align 1
  %local_3 = alloca i64, align 8
  %local_4 = alloca i64, align 8
  %local_5 = alloca i8, align 1
  %local_6 = alloca i64, align 8
  %local_7 = alloca i8, align 1
  %local_8 = alloca ptr, align 8
  %local_9 = alloca ptr, align 8
  %local_10 = alloca ptr, align 8
  %local_11 = alloca ptr, align 8
  store ptr %0, ptr %local_0, align 8
  store %"Option$$string" %1, ptr %local_1, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %machine_tag_ptr = getelementptr inbounds nuw %"Option$$string", ptr %local_1, i32 0, i32 0
  %move_iN_load = load i8, ptr %machine_tag_ptr, align 1
  %move_iN_zext = zext i8 %move_iN_load to i64
  store i64 %move_iN_zext, ptr %local_3, align 8
  store i64 0, ptr %local_4, align 8
  %cmp_lhs = load i64, ptr %local_3, align 8
  %cmp_rhs = load i64, ptr %local_4, align 8
  %cmp_bit = icmp eq i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_5, align 1
  %cond_load = load i8, ptr %local_5, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb2, label %bb5

bb1:                                              ; preds = %after_cooperate21, %after_cooperate16
  %move_load = load i8, ptr %local_2, align 1
  store i8 %move_load, ptr %return_slot, align 1
  call void @"__hew_enum_drop_inplace_Option$$string"(ptr %local_1)
  ret i8 0

bb2:                                              ; preds = %bb0
  %machine_payload_ptr = getelementptr inbounds nuw %"Option$$string", ptr %local_1, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { ptr }, ptr %machine_payload_ptr, i32 0, i32 0
  %move_load1 = load ptr, ptr %machine_variant_field_ptr, align 8
  store ptr %move_load1, ptr %local_8, align 8
  %"hew_string_concat arg0" = load ptr, ptr %local_0, align 8
  %"hew_string_concat arg1" = load ptr, ptr %local_8, align 8
  %hew_string_concat_call = call ptr @hew_string_concat(ptr %"hew_string_concat arg0", ptr %"hew_string_concat arg1")
  store ptr %hew_string_concat_call, ptr %local_9, align 8
  %print_arg = load ptr, ptr %local_9, align 8
  %print_str_bits = ptrtoint ptr %print_arg to i64
  call void @hew_print_value(i8 4, i64 %print_str_bits, i1 true)
  br label %bb6

bb3:                                              ; preds = %bb5
  store ptr @str_lit, ptr %local_10, align 8
  %"hew_string_concat arg02" = load ptr, ptr %local_0, align 8
  %"hew_string_concat arg13" = load ptr, ptr %local_10, align 8
  %hew_string_concat_call4 = call ptr @hew_string_concat(ptr %"hew_string_concat arg02", ptr %"hew_string_concat arg13")
  store ptr %hew_string_concat_call4, ptr %local_11, align 8
  %print_arg5 = load ptr, ptr %local_11, align 8
  %print_str_bits6 = ptrtoint ptr %print_arg5 to i64
  call void @hew_print_value(i8 4, i64 %print_str_bits6, i1 true)
  br label %bb7

bb4:                                              ; preds = %bb5
  call void @"__hew_enum_drop_inplace_Option$$string"(ptr %local_1)
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

bb5:                                              ; preds = %bb0
  store i64 1, ptr %local_6, align 8
  %cmp_lhs7 = load i64, ptr %local_3, align 8
  %cmp_rhs8 = load i64, ptr %local_6, align 8
  %cmp_bit9 = icmp eq i64 %cmp_lhs7, %cmp_rhs8
  %cmp_zext10 = zext i1 %cmp_bit9 to i8
  store i8 %cmp_zext10, ptr %local_7, align 1
  %cond_load11 = load i8, ptr %local_7, align 1
  %cond_nz12 = icmp ne i8 %cond_load11, 0
  br i1 %cond_nz12, label %bb3, label %bb4

bb6:                                              ; preds = %bb2
  %"hew_string_drop drop" = load ptr, ptr %local_9, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop")
  store ptr null, ptr %local_9, align 8
  %hew_actor_cooperate13 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel14 = icmp eq i32 %hew_actor_cooperate13, 2
  br i1 %hew_cooperate_is_cancel14, label %cancel_exit15, label %after_cooperate16

bb7:                                              ; preds = %bb3
  %"hew_string_drop drop17" = load ptr, ptr %local_11, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop17")
  store ptr null, ptr %local_11, align 8
  %hew_actor_cooperate18 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel19 = icmp eq i32 %hew_actor_cooperate18, 2
  br i1 %hew_cooperate_is_cancel19, label %cancel_exit20, label %after_cooperate21

cancel_exit:                                      ; preds = %entry
  call void @"__hew_enum_drop_inplace_Option$$string"(ptr %local_1)
  ret i8 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit15:                                    ; preds = %bb6
  call void @"__hew_enum_drop_inplace_Option$$string"(ptr %local_1)
  ret i8 0

after_cooperate16:                                ; preds = %bb6
  br label %bb1

cancel_exit20:                                    ; preds = %bb7
  call void @"__hew_enum_drop_inplace_Option$$string"(ptr %local_1)
  ret i8 0

after_cooperate21:                                ; preds = %bb7
  br label %bb1
}

define i8 @main() {
entry:
  %return_slot = alloca i8, align 1
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca ptr, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i8, align 1
  %local_5 = alloca %"Result$$Scanner$IoError", align 8
  %local_6 = alloca i64, align 8
  %local_7 = alloca i64, align 8
  %local_8 = alloca i8, align 1
  %local_9 = alloca i64, align 8
  %local_10 = alloca i8, align 1
  %local_11 = alloca %Scanner, align 8
  %local_12 = alloca %Scanner, align 8
  %local_13 = alloca { %Scanner, %"Option$$string" }, align 8
  %local_14 = alloca { %Scanner, %"Option$$string" }, align 8
  %local_15 = alloca %Scanner, align 8
  %local_16 = alloca %Scanner, align 8
  %local_17 = alloca %"Option$$string", align 8
  %local_18 = alloca %"Option$$string", align 8
  %local_19 = alloca { %Scanner, %"Option$$string" }, align 8
  %local_20 = alloca { %Scanner, %"Option$$string" }, align 8
  %local_21 = alloca %Scanner, align 8
  %local_22 = alloca %Scanner, align 8
  %local_23 = alloca %"Option$$string", align 8
  %local_24 = alloca %"Option$$string", align 8
  %local_25 = alloca { %Scanner, %"Option$$string" }, align 8
  %local_26 = alloca { %Scanner, %"Option$$string" }, align 8
  %local_27 = alloca %Scanner, align 8
  %local_28 = alloca %Scanner, align 8
  %local_29 = alloca %"Option$$string", align 8
  %local_30 = alloca %"Option$$string", align 8
  %local_31 = alloca { %Scanner, %"Option$$string" }, align 8
  %local_32 = alloca { %Scanner, %"Option$$string" }, align 8
  %local_33 = alloca %Scanner, align 8
  %local_34 = alloca %Scanner, align 8
  %local_35 = alloca %"Option$$string", align 8
  %local_36 = alloca %"Option$$string", align 8
  %local_37 = alloca ptr, align 8
  %local_38 = alloca ptr, align 8
  %local_39 = alloca ptr, align 8
  %local_40 = alloca ptr, align 8
  %local_41 = alloca %IoError, align 8
  %local_42 = alloca ptr, align 8
  %local_43 = alloca ptr, align 8
  %local_44 = alloca ptr, align 8
  %local_45 = alloca i64, align 8
  %local_46 = alloca i8, align 1
  %local_47 = alloca ptr, align 8
  %local_48 = alloca %"Result$$Scanner$IoError", align 8
  %local_49 = alloca i64, align 8
  %local_50 = alloca i64, align 8
  %local_51 = alloca i8, align 1
  %local_52 = alloca i64, align 8
  %local_53 = alloca i8, align 1
  %local_54 = alloca ptr, align 8
  %local_55 = alloca %IoError, align 8
  %local_56 = alloca ptr, align 8
  %local_57 = alloca ptr, align 8
  %local_58 = alloca ptr, align 8
  %local_59 = alloca %Scanner, align 8
  %local_60 = alloca %IoError, align 8
  %local_61 = alloca %Scanner, align 8
  %local_62 = alloca %Scanner, align 8
  %local_63 = alloca %Scanner, align 8
  %local_64 = alloca %"Option$$string", align 8
  %local_65 = alloca %"Option$$string", align 8
  %local_66 = alloca %"Option$$string", align 8
  %local_67 = alloca %"Option$$string", align 8
  %local_68 = alloca %IoError, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  store ptr @str_lit.1, ptr %local_0, align 8
  %move_load = load ptr, ptr %local_0, align 8
  store ptr %move_load, ptr %local_1, align 8
  store ptr @str_lit.2, ptr %local_2, align 8
  %call_arg = load ptr, ptr %local_1, align 8
  %call_arg1 = load ptr, ptr %local_2, align 8
  %call_result = call i32 @hew_file_write(ptr %call_arg, ptr %call_arg1)
  %ffi_sext = sext i32 %call_result to i64
  store i64 %ffi_sext, ptr %local_3, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_arg2 = load ptr, ptr %local_1, align 8
  %call_result3 = call %"Result$$Scanner$IoError" @"scanner$from_file"(ptr %call_arg2)
  store %"Result$$Scanner$IoError" %call_result3, ptr %local_5, align 8
  br label %bb2

bb2:                                              ; preds = %bb1
  %machine_tag_ptr = getelementptr inbounds nuw %"Result$$Scanner$IoError", ptr %local_5, i32 0, i32 0
  %move_iN_load = load i8, ptr %machine_tag_ptr, align 1
  %move_iN_zext = zext i8 %move_iN_load to i64
  store i64 %move_iN_zext, ptr %local_6, align 8
  store i64 0, ptr %local_7, align 8
  %cmp_lhs = load i64, ptr %local_6, align 8
  %cmp_rhs = load i64, ptr %local_7, align 8
  %cmp_bit = icmp eq i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_8, align 1
  %cond_load = load i8, ptr %local_8, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb4, label %bb7

bb3:                                              ; preds = %after_cooperate107, %after_cooperate100
  %call_arg4 = load ptr, ptr %local_1, align 8
  %call_result5 = call i32 @hew_file_delete(ptr %call_arg4)
  %ffi_sext6 = sext i32 %call_result5 to i64
  store i64 %ffi_sext6, ptr %local_45, align 8
  br label %bb18

bb4:                                              ; preds = %bb2
  %machine_payload_ptr = getelementptr inbounds nuw %"Result$$Scanner$IoError", ptr %local_5, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { %Scanner }, ptr %machine_payload_ptr, i32 0, i32 0
  %move_load7 = load %Scanner, ptr %machine_variant_field_ptr, align 8
  store %Scanner %move_load7, ptr %local_11, align 8
  %machine_payload_ptr8 = getelementptr inbounds nuw %"Result$$Scanner$IoError", ptr %local_5, i32 0, i32 1
  %machine_variant_field_ptr9 = getelementptr inbounds nuw { %Scanner }, ptr %machine_payload_ptr8, i32 0, i32 0
  store %Scanner zeroinitializer, ptr %machine_variant_field_ptr9, align 8
  %move_load10 = load %Scanner, ptr %local_11, align 8
  store %Scanner %move_load10, ptr %local_12, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %local_59, ptr align 8 %local_12, i64 32, i1 false)
  %record_clone_Scanner = call i32 @__hew_record_clone_inplace_Scanner(ptr %local_12, ptr %local_59)
  %record_clone_Scanner_failed = icmp ne i32 %record_clone_Scanner, 0
  br i1 %record_clone_Scanner_failed, label %clone_trap, label %clone_ok

bb5:                                              ; preds = %bb7
  %machine_payload_ptr13 = getelementptr inbounds nuw %"Result$$Scanner$IoError", ptr %local_5, i32 0, i32 1
  %machine_variant_field_ptr14 = getelementptr inbounds nuw { %IoError }, ptr %machine_payload_ptr13, i32 0, i32 0
  %move_load15 = load %IoError, ptr %machine_variant_field_ptr14, align 8
  store %IoError %move_load15, ptr %local_41, align 8
  store ptr @str_lit.3, ptr %local_42, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %local_60, ptr align 8 %local_41, i64 16, i1 false)
  %enum_clone_IoError = call i32 @__hew_enum_clone_inplace_IoError(ptr %local_41, ptr %local_60)
  %enum_clone_IoError_failed = icmp ne i32 %enum_clone_IoError, 0
  br i1 %enum_clone_IoError_failed, label %clone_trap17, label %clone_ok16

bb6:                                              ; preds = %bb7
  %"hew_string_drop drop" = load ptr, ptr %local_1, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

bb7:                                              ; preds = %bb2
  store i64 1, ptr %local_9, align 8
  %cmp_lhs20 = load i64, ptr %local_6, align 8
  %cmp_rhs21 = load i64, ptr %local_9, align 8
  %cmp_bit22 = icmp eq i64 %cmp_lhs20, %cmp_rhs21
  %cmp_zext23 = zext i1 %cmp_bit22 to i8
  store i8 %cmp_zext23, ptr %local_10, align 1
  %cond_load24 = load i8, ptr %local_10, align 1
  %cond_nz25 = icmp ne i8 %cond_load24, 0
  br i1 %cond_nz25, label %bb5, label %bb6

bb8:                                              ; preds = %clone_ok
  %move_load26 = load { %Scanner, %"Option$$string" }, ptr %local_13, align 8
  store { %Scanner, %"Option$$string" } %move_load26, ptr %local_14, align 8
  %tuple_0_load_ptr = getelementptr inbounds nuw { %Scanner, %"Option$$string" }, ptr %local_14, i32 0, i32 0
  %tuple_0_load = load %Scanner, ptr %tuple_0_load_ptr, align 8
  store %Scanner %tuple_0_load, ptr %local_15, align 8
  %move_load27 = load %Scanner, ptr %local_15, align 8
  store %Scanner %move_load27, ptr %local_16, align 8
  %tuple_1_load_ptr = getelementptr inbounds nuw { %Scanner, %"Option$$string" }, ptr %local_14, i32 0, i32 1
  %tuple_1_load = load %"Option$$string", ptr %tuple_1_load_ptr, align 8
  store %"Option$$string" %tuple_1_load, ptr %local_17, align 8
  %move_load28 = load %"Option$$string", ptr %local_17, align 8
  store %"Option$$string" %move_load28, ptr %local_18, align 8
  %move_load29 = load %Scanner, ptr %local_16, align 8
  store %Scanner %move_load29, ptr %local_12, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %local_61, ptr align 8 %local_12, i64 32, i1 false)
  %record_clone_Scanner30 = call i32 @__hew_record_clone_inplace_Scanner(ptr %local_12, ptr %local_61)
  %record_clone_Scanner_failed31 = icmp ne i32 %record_clone_Scanner30, 0
  br i1 %record_clone_Scanner_failed31, label %clone_trap33, label %clone_ok32

bb9:                                              ; preds = %clone_ok32
  %move_load36 = load { %Scanner, %"Option$$string" }, ptr %local_19, align 8
  store { %Scanner, %"Option$$string" } %move_load36, ptr %local_20, align 8
  %tuple_0_load_ptr37 = getelementptr inbounds nuw { %Scanner, %"Option$$string" }, ptr %local_20, i32 0, i32 0
  %tuple_0_load38 = load %Scanner, ptr %tuple_0_load_ptr37, align 8
  store %Scanner %tuple_0_load38, ptr %local_21, align 8
  %move_load39 = load %Scanner, ptr %local_21, align 8
  store %Scanner %move_load39, ptr %local_22, align 8
  %tuple_1_load_ptr40 = getelementptr inbounds nuw { %Scanner, %"Option$$string" }, ptr %local_20, i32 0, i32 1
  %tuple_1_load41 = load %"Option$$string", ptr %tuple_1_load_ptr40, align 8
  store %"Option$$string" %tuple_1_load41, ptr %local_23, align 8
  %move_load42 = load %"Option$$string", ptr %local_23, align 8
  store %"Option$$string" %move_load42, ptr %local_24, align 8
  %move_load43 = load %Scanner, ptr %local_22, align 8
  store %Scanner %move_load43, ptr %local_12, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %local_62, ptr align 8 %local_12, i64 32, i1 false)
  %record_clone_Scanner44 = call i32 @__hew_record_clone_inplace_Scanner(ptr %local_12, ptr %local_62)
  %record_clone_Scanner_failed45 = icmp ne i32 %record_clone_Scanner44, 0
  br i1 %record_clone_Scanner_failed45, label %clone_trap47, label %clone_ok46

bb10:                                             ; preds = %clone_ok46
  %move_load50 = load { %Scanner, %"Option$$string" }, ptr %local_25, align 8
  store { %Scanner, %"Option$$string" } %move_load50, ptr %local_26, align 8
  %tuple_0_load_ptr51 = getelementptr inbounds nuw { %Scanner, %"Option$$string" }, ptr %local_26, i32 0, i32 0
  %tuple_0_load52 = load %Scanner, ptr %tuple_0_load_ptr51, align 8
  store %Scanner %tuple_0_load52, ptr %local_27, align 8
  %move_load53 = load %Scanner, ptr %local_27, align 8
  store %Scanner %move_load53, ptr %local_28, align 8
  %tuple_1_load_ptr54 = getelementptr inbounds nuw { %Scanner, %"Option$$string" }, ptr %local_26, i32 0, i32 1
  %tuple_1_load55 = load %"Option$$string", ptr %tuple_1_load_ptr54, align 8
  store %"Option$$string" %tuple_1_load55, ptr %local_29, align 8
  %move_load56 = load %"Option$$string", ptr %local_29, align 8
  store %"Option$$string" %move_load56, ptr %local_30, align 8
  %move_load57 = load %Scanner, ptr %local_28, align 8
  store %Scanner %move_load57, ptr %local_12, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %local_63, ptr align 8 %local_12, i64 32, i1 false)
  %record_clone_Scanner58 = call i32 @__hew_record_clone_inplace_Scanner(ptr %local_12, ptr %local_63)
  %record_clone_Scanner_failed59 = icmp ne i32 %record_clone_Scanner58, 0
  br i1 %record_clone_Scanner_failed59, label %clone_trap61, label %clone_ok60

bb11:                                             ; preds = %clone_ok60
  %move_load64 = load { %Scanner, %"Option$$string" }, ptr %local_31, align 8
  store { %Scanner, %"Option$$string" } %move_load64, ptr %local_32, align 8
  %tuple_0_load_ptr65 = getelementptr inbounds nuw { %Scanner, %"Option$$string" }, ptr %local_32, i32 0, i32 0
  %tuple_0_load66 = load %Scanner, ptr %tuple_0_load_ptr65, align 8
  store %Scanner %tuple_0_load66, ptr %local_33, align 8
  %move_load67 = load %Scanner, ptr %local_33, align 8
  store %Scanner %move_load67, ptr %local_34, align 8
  %tuple_1_load_ptr68 = getelementptr inbounds nuw { %Scanner, %"Option$$string" }, ptr %local_32, i32 0, i32 1
  %tuple_1_load69 = load %"Option$$string", ptr %tuple_1_load_ptr68, align 8
  store %"Option$$string" %tuple_1_load69, ptr %local_35, align 8
  %move_load70 = load %"Option$$string", ptr %local_35, align 8
  store %"Option$$string" %move_load70, ptr %local_36, align 8
  store ptr @str_lit.4, ptr %local_37, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %local_64, ptr align 8 %local_18, i64 16, i1 false)
  %"enum_clone_Option$$string" = call i32 @"__hew_enum_clone_inplace_Option$$string"(ptr %local_18, ptr %local_64)
  %"enum_clone_Option$$string_failed" = icmp ne i32 %"enum_clone_Option$$string", 0
  br i1 %"enum_clone_Option$$string_failed", label %clone_trap72, label %clone_ok71

bb12:                                             ; preds = %clone_ok71
  store ptr @str_lit.5, ptr %local_38, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %local_65, ptr align 8 %local_24, i64 16, i1 false)
  %"enum_clone_Option$$string76" = call i32 @"__hew_enum_clone_inplace_Option$$string"(ptr %local_24, ptr %local_65)
  %"enum_clone_Option$$string_failed77" = icmp ne i32 %"enum_clone_Option$$string76", 0
  br i1 %"enum_clone_Option$$string_failed77", label %clone_trap79, label %clone_ok78

bb13:                                             ; preds = %clone_ok78
  store ptr @str_lit.6, ptr %local_39, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %local_66, ptr align 8 %local_30, i64 16, i1 false)
  %"enum_clone_Option$$string83" = call i32 @"__hew_enum_clone_inplace_Option$$string"(ptr %local_30, ptr %local_66)
  %"enum_clone_Option$$string_failed84" = icmp ne i32 %"enum_clone_Option$$string83", 0
  br i1 %"enum_clone_Option$$string_failed84", label %clone_trap86, label %clone_ok85

bb14:                                             ; preds = %clone_ok85
  store ptr @str_lit.7, ptr %local_40, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %local_67, ptr align 8 %local_36, i64 16, i1 false)
  %"enum_clone_Option$$string90" = call i32 @"__hew_enum_clone_inplace_Option$$string"(ptr %local_36, ptr %local_67)
  %"enum_clone_Option$$string_failed91" = icmp ne i32 %"enum_clone_Option$$string90", 0
  br i1 %"enum_clone_Option$$string_failed91", label %clone_trap93, label %clone_ok92

bb15:                                             ; preds = %clone_ok92
  %hew_actor_cooperate97 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel98 = icmp eq i32 %hew_actor_cooperate97, 2
  br i1 %hew_cooperate_is_cancel98, label %cancel_exit99, label %after_cooperate100

bb16:                                             ; preds = %clone_ok16
  %"hew_string_concat arg0" = load ptr, ptr %local_42, align 8
  %"hew_string_concat arg1" = load ptr, ptr %local_43, align 8
  %hew_string_concat_call = call ptr @hew_string_concat(ptr %"hew_string_concat arg0", ptr %"hew_string_concat arg1")
  store ptr %hew_string_concat_call, ptr %local_44, align 8
  %"hew_string_drop drop102" = load ptr, ptr %local_43, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop102")
  store ptr null, ptr %local_43, align 8
  %print_arg = load ptr, ptr %local_44, align 8
  %print_str_bits = ptrtoint ptr %print_arg to i64
  call void @hew_print_value(i8 4, i64 %print_str_bits, i1 true)
  br label %bb17

bb17:                                             ; preds = %bb16
  %"hew_string_drop drop103" = load ptr, ptr %local_44, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop103")
  store ptr null, ptr %local_44, align 8
  %hew_actor_cooperate104 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel105 = icmp eq i32 %hew_actor_cooperate104, 2
  br i1 %hew_cooperate_is_cancel105, label %cancel_exit106, label %after_cooperate107

bb18:                                             ; preds = %bb3
  store ptr @str_lit.8, ptr %local_47, align 8
  %call_arg109 = load ptr, ptr %local_47, align 8
  %call_result110 = call %"Result$$Scanner$IoError" @"scanner$from_file"(ptr %call_arg109)
  store %"Result$$Scanner$IoError" %call_result110, ptr %local_48, align 8
  br label %bb19

bb19:                                             ; preds = %bb18
  %machine_tag_ptr111 = getelementptr inbounds nuw %"Result$$Scanner$IoError", ptr %local_48, i32 0, i32 0
  %move_iN_load112 = load i8, ptr %machine_tag_ptr111, align 1
  %move_iN_zext113 = zext i8 %move_iN_load112 to i64
  store i64 %move_iN_zext113, ptr %local_49, align 8
  store i64 0, ptr %local_50, align 8
  %cmp_lhs114 = load i64, ptr %local_49, align 8
  %cmp_rhs115 = load i64, ptr %local_50, align 8
  %cmp_bit116 = icmp eq i64 %cmp_lhs114, %cmp_rhs115
  %cmp_zext117 = zext i1 %cmp_bit116 to i8
  store i8 %cmp_zext117, ptr %local_51, align 1
  %cond_load118 = load i8, ptr %local_51, align 1
  %cond_nz119 = icmp ne i8 %cond_load118, 0
  br i1 %cond_nz119, label %bb21, label %bb24

bb20:                                             ; preds = %after_cooperate155, %after_cooperate143
  %move_load120 = load i8, ptr %local_46, align 1
  store i8 %move_load120, ptr %return_slot, align 1
  %"hew_string_drop drop121" = load ptr, ptr %local_1, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop121")
  store ptr null, ptr %local_1, align 8
  %hew_lambda_drain_all_call = call i32 @hew_lambda_drain_all(i64 0)
  ret i8 0

bb21:                                             ; preds = %bb19
  store ptr @str_lit.9, ptr %local_54, align 8
  %print_arg122 = load ptr, ptr %local_54, align 8
  %print_str_bits123 = ptrtoint ptr %print_arg122 to i64
  call void @hew_print_value(i8 4, i64 %print_str_bits123, i1 true)
  br label %bb25

bb22:                                             ; preds = %bb24
  %machine_payload_ptr124 = getelementptr inbounds nuw %"Result$$Scanner$IoError", ptr %local_48, i32 0, i32 1
  %machine_variant_field_ptr125 = getelementptr inbounds nuw { %IoError }, ptr %machine_payload_ptr124, i32 0, i32 0
  %move_load126 = load %IoError, ptr %machine_variant_field_ptr125, align 8
  store %IoError %move_load126, ptr %local_55, align 8
  store ptr @str_lit.10, ptr %local_56, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %local_68, ptr align 8 %local_55, i64 16, i1 false)
  %enum_clone_IoError127 = call i32 @__hew_enum_clone_inplace_IoError(ptr %local_55, ptr %local_68)
  %enum_clone_IoError_failed128 = icmp ne i32 %enum_clone_IoError127, 0
  br i1 %enum_clone_IoError_failed128, label %clone_trap130, label %clone_ok129

bb23:                                             ; preds = %bb24
  %"hew_string_drop drop133" = load ptr, ptr %local_1, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop133")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

bb24:                                             ; preds = %bb19
  store i64 1, ptr %local_52, align 8
  %cmp_lhs134 = load i64, ptr %local_49, align 8
  %cmp_rhs135 = load i64, ptr %local_52, align 8
  %cmp_bit136 = icmp eq i64 %cmp_lhs134, %cmp_rhs135
  %cmp_zext137 = zext i1 %cmp_bit136 to i8
  store i8 %cmp_zext137, ptr %local_53, align 1
  %cond_load138 = load i8, ptr %local_53, align 1
  %cond_nz139 = icmp ne i8 %cond_load138, 0
  br i1 %cond_nz139, label %bb22, label %bb23

bb25:                                             ; preds = %bb21
  %hew_actor_cooperate140 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel141 = icmp eq i32 %hew_actor_cooperate140, 2
  br i1 %hew_cooperate_is_cancel141, label %cancel_exit142, label %after_cooperate143

bb26:                                             ; preds = %clone_ok129
  %"hew_string_concat arg0145" = load ptr, ptr %local_56, align 8
  %"hew_string_concat arg1146" = load ptr, ptr %local_57, align 8
  %hew_string_concat_call147 = call ptr @hew_string_concat(ptr %"hew_string_concat arg0145", ptr %"hew_string_concat arg1146")
  store ptr %hew_string_concat_call147, ptr %local_58, align 8
  %"hew_string_drop drop148" = load ptr, ptr %local_57, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop148")
  store ptr null, ptr %local_57, align 8
  %print_arg149 = load ptr, ptr %local_58, align 8
  %print_str_bits150 = ptrtoint ptr %print_arg149 to i64
  call void @hew_print_value(i8 4, i64 %print_str_bits150, i1 true)
  br label %bb27

bb27:                                             ; preds = %bb26
  %"hew_string_drop drop151" = load ptr, ptr %local_58, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop151")
  store ptr null, ptr %local_58, align 8
  %hew_actor_cooperate152 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel153 = icmp eq i32 %hew_actor_cooperate152, 2
  br i1 %hew_cooperate_is_cancel153, label %cancel_exit154, label %after_cooperate155

cancel_exit:                                      ; preds = %entry
  ret i8 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

clone_ok:                                         ; preds = %bb4
  %call_arg11 = load %Scanner, ptr %local_59, align 8
  %call_result12 = call { %Scanner, %"Option$$string" } @"scanner$next_line"(%Scanner %call_arg11)
  store { %Scanner, %"Option$$string" } %call_result12, ptr %local_13, align 8
  br label %bb8

clone_trap:                                       ; preds = %bb4
  call void @llvm.trap()
  unreachable

clone_ok16:                                       ; preds = %bb5
  %call_arg18 = load %IoError, ptr %local_60, align 8
  %call_result19 = call ptr @"fs$io_error_message"(%IoError %call_arg18)
  store ptr %call_result19, ptr %local_43, align 8
  br label %bb16

clone_trap17:                                     ; preds = %bb5
  call void @llvm.trap()
  unreachable

clone_ok32:                                       ; preds = %bb8
  %call_arg34 = load %Scanner, ptr %local_61, align 8
  %call_result35 = call { %Scanner, %"Option$$string" } @"scanner$next_line"(%Scanner %call_arg34)
  store { %Scanner, %"Option$$string" } %call_result35, ptr %local_19, align 8
  br label %bb9

clone_trap33:                                     ; preds = %bb8
  call void @llvm.trap()
  unreachable

clone_ok46:                                       ; preds = %bb9
  %call_arg48 = load %Scanner, ptr %local_62, align 8
  %call_result49 = call { %Scanner, %"Option$$string" } @"scanner$next_line"(%Scanner %call_arg48)
  store { %Scanner, %"Option$$string" } %call_result49, ptr %local_25, align 8
  br label %bb10

clone_trap47:                                     ; preds = %bb9
  call void @llvm.trap()
  unreachable

clone_ok60:                                       ; preds = %bb10
  %call_arg62 = load %Scanner, ptr %local_63, align 8
  %call_result63 = call { %Scanner, %"Option$$string" } @"scanner$next_line"(%Scanner %call_arg62)
  store { %Scanner, %"Option$$string" } %call_result63, ptr %local_31, align 8
  br label %bb11

clone_trap61:                                     ; preds = %bb10
  call void @llvm.trap()
  unreachable

clone_ok71:                                       ; preds = %bb11
  %call_arg73 = load ptr, ptr %local_37, align 8
  %call_arg74 = load %"Option$$string", ptr %local_64, align 8
  %call_result75 = call i8 @print_line(ptr %call_arg73, %"Option$$string" %call_arg74)
  br label %bb12

clone_trap72:                                     ; preds = %bb11
  call void @llvm.trap()
  unreachable

clone_ok78:                                       ; preds = %bb12
  %call_arg80 = load ptr, ptr %local_38, align 8
  %call_arg81 = load %"Option$$string", ptr %local_65, align 8
  %call_result82 = call i8 @print_line(ptr %call_arg80, %"Option$$string" %call_arg81)
  br label %bb13

clone_trap79:                                     ; preds = %bb12
  call void @llvm.trap()
  unreachable

clone_ok85:                                       ; preds = %bb13
  %call_arg87 = load ptr, ptr %local_39, align 8
  %call_arg88 = load %"Option$$string", ptr %local_66, align 8
  %call_result89 = call i8 @print_line(ptr %call_arg87, %"Option$$string" %call_arg88)
  br label %bb14

clone_trap86:                                     ; preds = %bb13
  call void @llvm.trap()
  unreachable

clone_ok92:                                       ; preds = %bb14
  %call_arg94 = load ptr, ptr %local_40, align 8
  %call_arg95 = load %"Option$$string", ptr %local_67, align 8
  %call_result96 = call i8 @print_line(ptr %call_arg94, %"Option$$string" %call_arg95)
  br label %bb15

clone_trap93:                                     ; preds = %bb14
  call void @llvm.trap()
  unreachable

cancel_exit99:                                    ; preds = %bb15
  %"hew_string_drop drop101" = load ptr, ptr %local_1, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop101")
  store ptr null, ptr %local_1, align 8
  ret i8 0

after_cooperate100:                               ; preds = %bb15
  br label %bb3

cancel_exit106:                                   ; preds = %bb17
  %"hew_string_drop drop108" = load ptr, ptr %local_1, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop108")
  store ptr null, ptr %local_1, align 8
  ret i8 0

after_cooperate107:                               ; preds = %bb17
  br label %bb3

clone_ok129:                                      ; preds = %bb22
  %call_arg131 = load %IoError, ptr %local_68, align 8
  %call_result132 = call ptr @"fs$io_error_message"(%IoError %call_arg131)
  store ptr %call_result132, ptr %local_57, align 8
  br label %bb26

clone_trap130:                                    ; preds = %bb22
  call void @llvm.trap()
  unreachable

cancel_exit142:                                   ; preds = %bb25
  %"hew_string_drop drop144" = load ptr, ptr %local_1, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop144")
  store ptr null, ptr %local_1, align 8
  ret i8 0

after_cooperate143:                               ; preds = %bb25
  br label %bb20

cancel_exit154:                                   ; preds = %bb27
  %"hew_string_drop drop156" = load ptr, ptr %local_1, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop156")
  store ptr null, ptr %local_1, align 8
  ret i8 0

after_cooperate155:                               ; preds = %bb27
  br label %bb20
}

define internal %IoError @"fs$io_error_from_errno"(i64 %0) {
entry:
  %return_slot = alloca %IoError, align 8
  %local_0 = alloca i64, align 8
  %local_1 = alloca %IoError, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i8, align 1
  %local_4 = alloca %IoError, align 8
  %local_5 = alloca i64, align 8
  %local_6 = alloca %IoError, align 8
  %local_7 = alloca %IoError, align 8
  %local_8 = alloca i64, align 8
  %local_9 = alloca i8, align 1
  %local_10 = alloca %IoError, align 8
  %local_11 = alloca i64, align 8
  %local_12 = alloca %IoError, align 8
  %local_13 = alloca %IoError, align 8
  %local_14 = alloca i8, align 1
  %local_15 = alloca i64, align 8
  %local_16 = alloca i8, align 1
  %local_17 = alloca i64, align 8
  %local_18 = alloca i8, align 1
  %local_19 = alloca %IoError, align 8
  %local_20 = alloca i64, align 8
  %local_21 = alloca %IoError, align 8
  %local_22 = alloca %IoError, align 8
  %local_23 = alloca i8, align 1
  %local_24 = alloca i64, align 8
  %local_25 = alloca i8, align 1
  %local_26 = alloca i64, align 8
  %local_27 = alloca i8, align 1
  %local_28 = alloca %IoError, align 8
  %local_29 = alloca i64, align 8
  %local_30 = alloca %IoError, align 8
  %local_31 = alloca %IoError, align 8
  %local_32 = alloca i8, align 1
  %local_33 = alloca i64, align 8
  %local_34 = alloca i8, align 1
  %local_35 = alloca i64, align 8
  %local_36 = alloca i8, align 1
  %local_37 = alloca %IoError, align 8
  %local_38 = alloca i64, align 8
  %local_39 = alloca %IoError, align 8
  %local_40 = alloca %IoError, align 8
  %local_41 = alloca i64, align 8
  %local_42 = alloca %IoError, align 8
  store i64 %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  store i64 2, ptr %local_2, align 8
  %cmp_lhs = load i64, ptr %local_0, align 8
  %cmp_rhs = load i64, ptr %local_2, align 8
  %cmp_bit = icmp eq i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_3, align 1
  %cond_load = load i8, ptr %local_3, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  store i64 0, ptr %local_5, align 8
  %machine_tag_ptr = getelementptr inbounds nuw %IoError, ptr %local_4, i32 0, i32 0
  %move_iN_load_wide = load i64, ptr %local_5, align 8
  %move_iN_trunc = trunc i64 %move_iN_load_wide to i8
  store i8 %move_iN_trunc, ptr %machine_tag_ptr, align 1
  %machine_payload_ptr = getelementptr inbounds nuw %IoError, ptr %local_4, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr, i32 0, i32 0
  %move_load = load i64, ptr %local_0, align 8
  store i64 %move_load, ptr %machine_variant_field_ptr, align 8
  %move_load1 = load %IoError, ptr %local_4, align 8
  store %IoError %move_load1, ptr %local_6, align 8
  %move_load2 = load %IoError, ptr %local_6, align 8
  store %IoError %move_load2, ptr %local_1, align 8
  br label %bb3

bb2:                                              ; preds = %bb0
  store i64 13, ptr %local_8, align 8
  %cmp_lhs3 = load i64, ptr %local_0, align 8
  %cmp_rhs4 = load i64, ptr %local_8, align 8
  %cmp_bit5 = icmp eq i64 %cmp_lhs3, %cmp_rhs4
  %cmp_zext6 = zext i1 %cmp_bit5 to i8
  store i8 %cmp_zext6, ptr %local_9, align 1
  %cond_load7 = load i8, ptr %local_9, align 1
  %cond_nz8 = icmp ne i8 %cond_load7, 0
  br i1 %cond_nz8, label %bb4, label %bb5

bb3:                                              ; preds = %after_cooperate28, %bb1
  %move_load9 = load %IoError, ptr %local_1, align 8
  store %IoError %move_load9, ptr %return_slot, align 8
  %ret_val = load %IoError, ptr %return_slot, align 8
  ret %IoError %ret_val

bb4:                                              ; preds = %bb2
  store i64 1, ptr %local_11, align 8
  %machine_tag_ptr10 = getelementptr inbounds nuw %IoError, ptr %local_10, i32 0, i32 0
  %move_iN_load_wide11 = load i64, ptr %local_11, align 8
  %move_iN_trunc12 = trunc i64 %move_iN_load_wide11 to i8
  store i8 %move_iN_trunc12, ptr %machine_tag_ptr10, align 1
  %machine_payload_ptr13 = getelementptr inbounds nuw %IoError, ptr %local_10, i32 0, i32 1
  %machine_variant_field_ptr14 = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr13, i32 0, i32 0
  %move_load15 = load i64, ptr %local_0, align 8
  store i64 %move_load15, ptr %machine_variant_field_ptr14, align 8
  %move_load16 = load %IoError, ptr %local_10, align 8
  store %IoError %move_load16, ptr %local_12, align 8
  %move_load17 = load %IoError, ptr %local_12, align 8
  store %IoError %move_load17, ptr %local_7, align 8
  br label %bb6

bb5:                                              ; preds = %bb2
  store i8 1, ptr %local_14, align 1
  store i64 17, ptr %local_15, align 8
  %cmp_lhs18 = load i64, ptr %local_0, align 8
  %cmp_rhs19 = load i64, ptr %local_15, align 8
  %cmp_bit20 = icmp eq i64 %cmp_lhs18, %cmp_rhs19
  %cmp_zext21 = zext i1 %cmp_bit20 to i8
  store i8 %cmp_zext21, ptr %local_16, align 1
  %cond_load22 = load i8, ptr %local_16, align 1
  %cond_nz23 = icmp ne i8 %cond_load22, 0
  br i1 %cond_nz23, label %bb8, label %bb7

bb6:                                              ; preds = %after_cooperate54, %bb4
  %move_load24 = load %IoError, ptr %local_7, align 8
  store %IoError %move_load24, ptr %local_1, align 8
  %hew_actor_cooperate25 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel26 = icmp eq i32 %hew_actor_cooperate25, 2
  br i1 %hew_cooperate_is_cancel26, label %cancel_exit27, label %after_cooperate28

bb7:                                              ; preds = %bb5
  store i64 183, ptr %local_17, align 8
  %cmp_lhs29 = load i64, ptr %local_0, align 8
  %cmp_rhs30 = load i64, ptr %local_17, align 8
  %cmp_bit31 = icmp eq i64 %cmp_lhs29, %cmp_rhs30
  %cmp_zext32 = zext i1 %cmp_bit31 to i8
  store i8 %cmp_zext32, ptr %local_18, align 1
  %move_load33 = load i8, ptr %local_18, align 1
  store i8 %move_load33, ptr %local_14, align 1
  br label %bb8

bb8:                                              ; preds = %bb7, %bb5
  %cond_load34 = load i8, ptr %local_14, align 1
  %cond_nz35 = icmp ne i8 %cond_load34, 0
  br i1 %cond_nz35, label %bb9, label %bb10

bb9:                                              ; preds = %bb8
  store i64 2, ptr %local_20, align 8
  %machine_tag_ptr36 = getelementptr inbounds nuw %IoError, ptr %local_19, i32 0, i32 0
  %move_iN_load_wide37 = load i64, ptr %local_20, align 8
  %move_iN_trunc38 = trunc i64 %move_iN_load_wide37 to i8
  store i8 %move_iN_trunc38, ptr %machine_tag_ptr36, align 1
  %machine_payload_ptr39 = getelementptr inbounds nuw %IoError, ptr %local_19, i32 0, i32 1
  %machine_variant_field_ptr40 = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr39, i32 0, i32 0
  %move_load41 = load i64, ptr %local_0, align 8
  store i64 %move_load41, ptr %machine_variant_field_ptr40, align 8
  %move_load42 = load %IoError, ptr %local_19, align 8
  store %IoError %move_load42, ptr %local_21, align 8
  %move_load43 = load %IoError, ptr %local_21, align 8
  store %IoError %move_load43, ptr %local_13, align 8
  br label %bb11

bb10:                                             ; preds = %bb8
  store i8 1, ptr %local_23, align 1
  store i64 110, ptr %local_24, align 8
  %cmp_lhs44 = load i64, ptr %local_0, align 8
  %cmp_rhs45 = load i64, ptr %local_24, align 8
  %cmp_bit46 = icmp eq i64 %cmp_lhs44, %cmp_rhs45
  %cmp_zext47 = zext i1 %cmp_bit46 to i8
  store i8 %cmp_zext47, ptr %local_25, align 1
  %cond_load48 = load i8, ptr %local_25, align 1
  %cond_nz49 = icmp ne i8 %cond_load48, 0
  br i1 %cond_nz49, label %bb13, label %bb12

bb11:                                             ; preds = %after_cooperate80, %bb9
  %move_load50 = load %IoError, ptr %local_13, align 8
  store %IoError %move_load50, ptr %local_7, align 8
  %hew_actor_cooperate51 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel52 = icmp eq i32 %hew_actor_cooperate51, 2
  br i1 %hew_cooperate_is_cancel52, label %cancel_exit53, label %after_cooperate54

bb12:                                             ; preds = %bb10
  store i64 60, ptr %local_26, align 8
  %cmp_lhs55 = load i64, ptr %local_0, align 8
  %cmp_rhs56 = load i64, ptr %local_26, align 8
  %cmp_bit57 = icmp eq i64 %cmp_lhs55, %cmp_rhs56
  %cmp_zext58 = zext i1 %cmp_bit57 to i8
  store i8 %cmp_zext58, ptr %local_27, align 1
  %move_load59 = load i8, ptr %local_27, align 1
  store i8 %move_load59, ptr %local_23, align 1
  br label %bb13

bb13:                                             ; preds = %bb12, %bb10
  %cond_load60 = load i8, ptr %local_23, align 1
  %cond_nz61 = icmp ne i8 %cond_load60, 0
  br i1 %cond_nz61, label %bb14, label %bb15

bb14:                                             ; preds = %bb13
  store i64 3, ptr %local_29, align 8
  %machine_tag_ptr62 = getelementptr inbounds nuw %IoError, ptr %local_28, i32 0, i32 0
  %move_iN_load_wide63 = load i64, ptr %local_29, align 8
  %move_iN_trunc64 = trunc i64 %move_iN_load_wide63 to i8
  store i8 %move_iN_trunc64, ptr %machine_tag_ptr62, align 1
  %machine_payload_ptr65 = getelementptr inbounds nuw %IoError, ptr %local_28, i32 0, i32 1
  %machine_variant_field_ptr66 = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr65, i32 0, i32 0
  %move_load67 = load i64, ptr %local_0, align 8
  store i64 %move_load67, ptr %machine_variant_field_ptr66, align 8
  %move_load68 = load %IoError, ptr %local_28, align 8
  store %IoError %move_load68, ptr %local_30, align 8
  %move_load69 = load %IoError, ptr %local_30, align 8
  store %IoError %move_load69, ptr %local_22, align 8
  br label %bb16

bb15:                                             ; preds = %bb13
  store i8 1, ptr %local_32, align 1
  store i64 125, ptr %local_33, align 8
  %cmp_lhs70 = load i64, ptr %local_0, align 8
  %cmp_rhs71 = load i64, ptr %local_33, align 8
  %cmp_bit72 = icmp eq i64 %cmp_lhs70, %cmp_rhs71
  %cmp_zext73 = zext i1 %cmp_bit72 to i8
  store i8 %cmp_zext73, ptr %local_34, align 1
  %cond_load74 = load i8, ptr %local_34, align 1
  %cond_nz75 = icmp ne i8 %cond_load74, 0
  br i1 %cond_nz75, label %bb18, label %bb17

bb16:                                             ; preds = %after_cooperate108, %bb14
  %move_load76 = load %IoError, ptr %local_22, align 8
  store %IoError %move_load76, ptr %local_13, align 8
  %hew_actor_cooperate77 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel78 = icmp eq i32 %hew_actor_cooperate77, 2
  br i1 %hew_cooperate_is_cancel78, label %cancel_exit79, label %after_cooperate80

bb17:                                             ; preds = %bb15
  store i64 89, ptr %local_35, align 8
  %cmp_lhs81 = load i64, ptr %local_0, align 8
  %cmp_rhs82 = load i64, ptr %local_35, align 8
  %cmp_bit83 = icmp eq i64 %cmp_lhs81, %cmp_rhs82
  %cmp_zext84 = zext i1 %cmp_bit83 to i8
  store i8 %cmp_zext84, ptr %local_36, align 1
  %move_load85 = load i8, ptr %local_36, align 1
  store i8 %move_load85, ptr %local_32, align 1
  br label %bb18

bb18:                                             ; preds = %bb17, %bb15
  %cond_load86 = load i8, ptr %local_32, align 1
  %cond_nz87 = icmp ne i8 %cond_load86, 0
  br i1 %cond_nz87, label %bb19, label %bb20

bb19:                                             ; preds = %bb18
  store i64 4, ptr %local_38, align 8
  %machine_tag_ptr88 = getelementptr inbounds nuw %IoError, ptr %local_37, i32 0, i32 0
  %move_iN_load_wide89 = load i64, ptr %local_38, align 8
  %move_iN_trunc90 = trunc i64 %move_iN_load_wide89 to i8
  store i8 %move_iN_trunc90, ptr %machine_tag_ptr88, align 1
  %machine_payload_ptr91 = getelementptr inbounds nuw %IoError, ptr %local_37, i32 0, i32 1
  %machine_variant_field_ptr92 = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr91, i32 0, i32 0
  %move_load93 = load i64, ptr %local_0, align 8
  store i64 %move_load93, ptr %machine_variant_field_ptr92, align 8
  %move_load94 = load %IoError, ptr %local_37, align 8
  store %IoError %move_load94, ptr %local_39, align 8
  %move_load95 = load %IoError, ptr %local_39, align 8
  store %IoError %move_load95, ptr %local_31, align 8
  br label %bb21

bb20:                                             ; preds = %bb18
  store i64 5, ptr %local_41, align 8
  %machine_tag_ptr96 = getelementptr inbounds nuw %IoError, ptr %local_40, i32 0, i32 0
  %move_iN_load_wide97 = load i64, ptr %local_41, align 8
  %move_iN_trunc98 = trunc i64 %move_iN_load_wide97 to i8
  store i8 %move_iN_trunc98, ptr %machine_tag_ptr96, align 1
  %machine_payload_ptr99 = getelementptr inbounds nuw %IoError, ptr %local_40, i32 0, i32 1
  %machine_variant_field_ptr100 = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr99, i32 0, i32 0
  %move_load101 = load i64, ptr %local_0, align 8
  store i64 %move_load101, ptr %machine_variant_field_ptr100, align 8
  %move_load102 = load %IoError, ptr %local_40, align 8
  store %IoError %move_load102, ptr %local_42, align 8
  %move_load103 = load %IoError, ptr %local_42, align 8
  store %IoError %move_load103, ptr %local_31, align 8
  br label %bb21

bb21:                                             ; preds = %bb20, %bb19
  %move_load104 = load %IoError, ptr %local_31, align 8
  store %IoError %move_load104, ptr %local_22, align 8
  %hew_actor_cooperate105 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel106 = icmp eq i32 %hew_actor_cooperate105, 2
  br i1 %hew_cooperate_is_cancel106, label %cancel_exit107, label %after_cooperate108

cancel_exit:                                      ; preds = %entry
  ret %IoError zeroinitializer

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit27:                                    ; preds = %bb6
  ret %IoError zeroinitializer

after_cooperate28:                                ; preds = %bb6
  br label %bb3

cancel_exit53:                                    ; preds = %bb11
  ret %IoError zeroinitializer

after_cooperate54:                                ; preds = %bb11
  br label %bb6

cancel_exit79:                                    ; preds = %bb16
  ret %IoError zeroinitializer

after_cooperate80:                                ; preds = %bb16
  br label %bb11

cancel_exit107:                                   ; preds = %bb21
  ret %IoError zeroinitializer

after_cooperate108:                               ; preds = %bb21
  br label %bb16
}

define internal %IoError @"fs$io_error_from_kind"(i64 %0, i64 %1) {
entry:
  %return_slot = alloca %IoError, align 8
  %local_0 = alloca i64, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca %IoError, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i8, align 1
  %local_5 = alloca %IoError, align 8
  %local_6 = alloca i64, align 8
  %local_7 = alloca %IoError, align 8
  %local_8 = alloca %IoError, align 8
  %local_9 = alloca i64, align 8
  %local_10 = alloca i8, align 1
  %local_11 = alloca %IoError, align 8
  %local_12 = alloca i64, align 8
  %local_13 = alloca %IoError, align 8
  %local_14 = alloca %IoError, align 8
  %local_15 = alloca i64, align 8
  %local_16 = alloca i8, align 1
  %local_17 = alloca %IoError, align 8
  %local_18 = alloca i64, align 8
  %local_19 = alloca %IoError, align 8
  %local_20 = alloca %IoError, align 8
  %local_21 = alloca i64, align 8
  %local_22 = alloca i8, align 1
  %local_23 = alloca %IoError, align 8
  %local_24 = alloca i64, align 8
  %local_25 = alloca %IoError, align 8
  %local_26 = alloca %IoError, align 8
  %local_27 = alloca %IoError, align 8
  store i64 %0, ptr %local_0, align 8
  store i64 %1, ptr %local_1, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  store i64 1, ptr %local_3, align 8
  %cmp_lhs = load i64, ptr %local_0, align 8
  %cmp_rhs = load i64, ptr %local_3, align 8
  %cmp_bit = icmp eq i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_4, align 1
  %cond_load = load i8, ptr %local_4, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  store i64 0, ptr %local_6, align 8
  %machine_tag_ptr = getelementptr inbounds nuw %IoError, ptr %local_5, i32 0, i32 0
  %move_iN_load_wide = load i64, ptr %local_6, align 8
  %move_iN_trunc = trunc i64 %move_iN_load_wide to i8
  store i8 %move_iN_trunc, ptr %machine_tag_ptr, align 1
  %machine_payload_ptr = getelementptr inbounds nuw %IoError, ptr %local_5, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr, i32 0, i32 0
  %move_load = load i64, ptr %local_1, align 8
  store i64 %move_load, ptr %machine_variant_field_ptr, align 8
  %move_load1 = load %IoError, ptr %local_5, align 8
  store %IoError %move_load1, ptr %local_7, align 8
  %move_load2 = load %IoError, ptr %local_7, align 8
  store %IoError %move_load2, ptr %local_2, align 8
  br label %bb3

bb2:                                              ; preds = %bb0
  store i64 2, ptr %local_9, align 8
  %cmp_lhs3 = load i64, ptr %local_0, align 8
  %cmp_rhs4 = load i64, ptr %local_9, align 8
  %cmp_bit5 = icmp eq i64 %cmp_lhs3, %cmp_rhs4
  %cmp_zext6 = zext i1 %cmp_bit5 to i8
  store i8 %cmp_zext6, ptr %local_10, align 1
  %cond_load7 = load i8, ptr %local_10, align 1
  %cond_nz8 = icmp ne i8 %cond_load7, 0
  br i1 %cond_nz8, label %bb4, label %bb5

bb3:                                              ; preds = %after_cooperate28, %bb1
  %move_load9 = load %IoError, ptr %local_2, align 8
  store %IoError %move_load9, ptr %return_slot, align 8
  %ret_val = load %IoError, ptr %return_slot, align 8
  ret %IoError %ret_val

bb4:                                              ; preds = %bb2
  store i64 1, ptr %local_12, align 8
  %machine_tag_ptr10 = getelementptr inbounds nuw %IoError, ptr %local_11, i32 0, i32 0
  %move_iN_load_wide11 = load i64, ptr %local_12, align 8
  %move_iN_trunc12 = trunc i64 %move_iN_load_wide11 to i8
  store i8 %move_iN_trunc12, ptr %machine_tag_ptr10, align 1
  %machine_payload_ptr13 = getelementptr inbounds nuw %IoError, ptr %local_11, i32 0, i32 1
  %machine_variant_field_ptr14 = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr13, i32 0, i32 0
  %move_load15 = load i64, ptr %local_1, align 8
  store i64 %move_load15, ptr %machine_variant_field_ptr14, align 8
  %move_load16 = load %IoError, ptr %local_11, align 8
  store %IoError %move_load16, ptr %local_13, align 8
  %move_load17 = load %IoError, ptr %local_13, align 8
  store %IoError %move_load17, ptr %local_8, align 8
  br label %bb6

bb5:                                              ; preds = %bb2
  store i64 3, ptr %local_15, align 8
  %cmp_lhs18 = load i64, ptr %local_0, align 8
  %cmp_rhs19 = load i64, ptr %local_15, align 8
  %cmp_bit20 = icmp eq i64 %cmp_lhs18, %cmp_rhs19
  %cmp_zext21 = zext i1 %cmp_bit20 to i8
  store i8 %cmp_zext21, ptr %local_16, align 1
  %cond_load22 = load i8, ptr %local_16, align 1
  %cond_nz23 = icmp ne i8 %cond_load22, 0
  br i1 %cond_nz23, label %bb7, label %bb8

bb6:                                              ; preds = %after_cooperate47, %bb4
  %move_load24 = load %IoError, ptr %local_8, align 8
  store %IoError %move_load24, ptr %local_2, align 8
  %hew_actor_cooperate25 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel26 = icmp eq i32 %hew_actor_cooperate25, 2
  br i1 %hew_cooperate_is_cancel26, label %cancel_exit27, label %after_cooperate28

bb7:                                              ; preds = %bb5
  store i64 2, ptr %local_18, align 8
  %machine_tag_ptr29 = getelementptr inbounds nuw %IoError, ptr %local_17, i32 0, i32 0
  %move_iN_load_wide30 = load i64, ptr %local_18, align 8
  %move_iN_trunc31 = trunc i64 %move_iN_load_wide30 to i8
  store i8 %move_iN_trunc31, ptr %machine_tag_ptr29, align 1
  %machine_payload_ptr32 = getelementptr inbounds nuw %IoError, ptr %local_17, i32 0, i32 1
  %machine_variant_field_ptr33 = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr32, i32 0, i32 0
  %move_load34 = load i64, ptr %local_1, align 8
  store i64 %move_load34, ptr %machine_variant_field_ptr33, align 8
  %move_load35 = load %IoError, ptr %local_17, align 8
  store %IoError %move_load35, ptr %local_19, align 8
  %move_load36 = load %IoError, ptr %local_19, align 8
  store %IoError %move_load36, ptr %local_14, align 8
  br label %bb9

bb8:                                              ; preds = %bb5
  store i64 4, ptr %local_21, align 8
  %cmp_lhs37 = load i64, ptr %local_0, align 8
  %cmp_rhs38 = load i64, ptr %local_21, align 8
  %cmp_bit39 = icmp eq i64 %cmp_lhs37, %cmp_rhs38
  %cmp_zext40 = zext i1 %cmp_bit39 to i8
  store i8 %cmp_zext40, ptr %local_22, align 1
  %cond_load41 = load i8, ptr %local_22, align 1
  %cond_nz42 = icmp ne i8 %cond_load41, 0
  br i1 %cond_nz42, label %bb10, label %bb11

bb9:                                              ; preds = %after_cooperate60, %bb7
  %move_load43 = load %IoError, ptr %local_14, align 8
  store %IoError %move_load43, ptr %local_8, align 8
  %hew_actor_cooperate44 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel45 = icmp eq i32 %hew_actor_cooperate44, 2
  br i1 %hew_cooperate_is_cancel45, label %cancel_exit46, label %after_cooperate47

bb10:                                             ; preds = %bb8
  store i64 3, ptr %local_24, align 8
  %machine_tag_ptr48 = getelementptr inbounds nuw %IoError, ptr %local_23, i32 0, i32 0
  %move_iN_load_wide49 = load i64, ptr %local_24, align 8
  %move_iN_trunc50 = trunc i64 %move_iN_load_wide49 to i8
  store i8 %move_iN_trunc50, ptr %machine_tag_ptr48, align 1
  %machine_payload_ptr51 = getelementptr inbounds nuw %IoError, ptr %local_23, i32 0, i32 1
  %machine_variant_field_ptr52 = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr51, i32 0, i32 0
  %move_load53 = load i64, ptr %local_1, align 8
  store i64 %move_load53, ptr %machine_variant_field_ptr52, align 8
  %move_load54 = load %IoError, ptr %local_23, align 8
  store %IoError %move_load54, ptr %local_25, align 8
  %move_load55 = load %IoError, ptr %local_25, align 8
  store %IoError %move_load55, ptr %local_20, align 8
  br label %bb12

bb11:                                             ; preds = %bb8
  %call_arg = load i64, ptr %local_1, align 8
  %call_result = call %IoError @"fs$io_error_from_errno"(i64 %call_arg)
  store %IoError %call_result, ptr %local_26, align 8
  br label %bb13

bb12:                                             ; preds = %after_cooperate66, %bb10
  %move_load56 = load %IoError, ptr %local_20, align 8
  store %IoError %move_load56, ptr %local_14, align 8
  %hew_actor_cooperate57 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel58 = icmp eq i32 %hew_actor_cooperate57, 2
  br i1 %hew_cooperate_is_cancel58, label %cancel_exit59, label %after_cooperate60

bb13:                                             ; preds = %bb11
  %move_load61 = load %IoError, ptr %local_26, align 8
  store %IoError %move_load61, ptr %local_27, align 8
  %move_load62 = load %IoError, ptr %local_27, align 8
  store %IoError %move_load62, ptr %local_20, align 8
  %hew_actor_cooperate63 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel64 = icmp eq i32 %hew_actor_cooperate63, 2
  br i1 %hew_cooperate_is_cancel64, label %cancel_exit65, label %after_cooperate66

cancel_exit:                                      ; preds = %entry
  ret %IoError zeroinitializer

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit27:                                    ; preds = %bb6
  ret %IoError zeroinitializer

after_cooperate28:                                ; preds = %bb6
  br label %bb3

cancel_exit46:                                    ; preds = %bb9
  ret %IoError zeroinitializer

after_cooperate47:                                ; preds = %bb9
  br label %bb6

cancel_exit59:                                    ; preds = %bb12
  ret %IoError zeroinitializer

after_cooperate60:                                ; preds = %bb12
  br label %bb9

cancel_exit65:                                    ; preds = %bb13
  ret %IoError zeroinitializer

after_cooperate66:                                ; preds = %bb13
  br label %bb12
}

define internal ptr @"fs$io_error_message"(%IoError %0) {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca %IoError, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i8, align 1
  %local_5 = alloca i64, align 8
  %local_6 = alloca i8, align 1
  %local_7 = alloca i64, align 8
  %local_8 = alloca i8, align 1
  %local_9 = alloca i64, align 8
  %local_10 = alloca i8, align 1
  %local_11 = alloca i64, align 8
  %local_12 = alloca i8, align 1
  %local_13 = alloca i64, align 8
  %local_14 = alloca i8, align 1
  %local_15 = alloca i64, align 8
  %local_16 = alloca ptr, align 8
  %local_17 = alloca ptr, align 8
  %local_18 = alloca ptr, align 8
  %local_19 = alloca ptr, align 8
  %local_20 = alloca ptr, align 8
  %local_21 = alloca i64, align 8
  %local_22 = alloca ptr, align 8
  %local_23 = alloca ptr, align 8
  %local_24 = alloca ptr, align 8
  %local_25 = alloca ptr, align 8
  %local_26 = alloca ptr, align 8
  %local_27 = alloca i64, align 8
  %local_28 = alloca ptr, align 8
  %local_29 = alloca ptr, align 8
  %local_30 = alloca ptr, align 8
  %local_31 = alloca ptr, align 8
  %local_32 = alloca ptr, align 8
  %local_33 = alloca i64, align 8
  %local_34 = alloca ptr, align 8
  %local_35 = alloca ptr, align 8
  %local_36 = alloca ptr, align 8
  %local_37 = alloca ptr, align 8
  %local_38 = alloca ptr, align 8
  %local_39 = alloca i64, align 8
  %local_40 = alloca ptr, align 8
  %local_41 = alloca ptr, align 8
  %local_42 = alloca ptr, align 8
  %local_43 = alloca ptr, align 8
  %local_44 = alloca ptr, align 8
  %local_45 = alloca i64, align 8
  %local_46 = alloca ptr, align 8
  %local_47 = alloca ptr, align 8
  %local_48 = alloca ptr, align 8
  %local_49 = alloca ptr, align 8
  %local_50 = alloca ptr, align 8
  store %IoError %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %machine_tag_ptr = getelementptr inbounds nuw %IoError, ptr %local_0, i32 0, i32 0
  %move_iN_load = load i8, ptr %machine_tag_ptr, align 1
  %move_iN_zext = zext i8 %move_iN_load to i64
  store i64 %move_iN_zext, ptr %local_2, align 8
  store i64 0, ptr %local_3, align 8
  %cmp_lhs = load i64, ptr %local_2, align 8
  %cmp_rhs = load i64, ptr %local_3, align 8
  %cmp_bit = icmp eq i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_4, align 1
  %cond_load = load i8, ptr %local_4, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb2, label %bb9

bb1:                                              ; preds = %after_cooperate133, %after_cooperate120, %after_cooperate107, %after_cooperate94, %after_cooperate81, %after_cooperate68
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %return_slot, align 8
  call void @__hew_enum_drop_inplace_IoError(ptr %local_0)
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

bb2:                                              ; preds = %bb0
  %machine_payload_ptr = getelementptr inbounds nuw %IoError, ptr %local_0, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr, i32 0, i32 0
  %move_load1 = load i64, ptr %machine_variant_field_ptr, align 8
  store i64 %move_load1, ptr %local_15, align 8
  store ptr @str_lit.11, ptr %local_16, align 8
  %call_arg = load i64, ptr %local_15, align 8
  %call_result = call ptr @hew_i64_to_string(i64 %call_arg)
  store ptr %call_result, ptr %local_17, align 8
  br label %bb14

bb3:                                              ; preds = %bb9
  %machine_payload_ptr2 = getelementptr inbounds nuw %IoError, ptr %local_0, i32 0, i32 1
  %machine_variant_field_ptr3 = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr2, i32 0, i32 0
  %move_load4 = load i64, ptr %machine_variant_field_ptr3, align 8
  store i64 %move_load4, ptr %local_21, align 8
  store ptr @str_lit.12, ptr %local_22, align 8
  %call_arg5 = load i64, ptr %local_21, align 8
  %call_result6 = call ptr @hew_i64_to_string(i64 %call_arg5)
  store ptr %call_result6, ptr %local_23, align 8
  br label %bb17

bb4:                                              ; preds = %bb10
  %machine_payload_ptr7 = getelementptr inbounds nuw %IoError, ptr %local_0, i32 0, i32 1
  %machine_variant_field_ptr8 = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr7, i32 0, i32 0
  %move_load9 = load i64, ptr %machine_variant_field_ptr8, align 8
  store i64 %move_load9, ptr %local_27, align 8
  store ptr @str_lit.13, ptr %local_28, align 8
  %call_arg10 = load i64, ptr %local_27, align 8
  %call_result11 = call ptr @hew_i64_to_string(i64 %call_arg10)
  store ptr %call_result11, ptr %local_29, align 8
  br label %bb20

bb5:                                              ; preds = %bb11
  %machine_payload_ptr12 = getelementptr inbounds nuw %IoError, ptr %local_0, i32 0, i32 1
  %machine_variant_field_ptr13 = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr12, i32 0, i32 0
  %move_load14 = load i64, ptr %machine_variant_field_ptr13, align 8
  store i64 %move_load14, ptr %local_33, align 8
  store ptr @str_lit.14, ptr %local_34, align 8
  %call_arg15 = load i64, ptr %local_33, align 8
  %call_result16 = call ptr @hew_i64_to_string(i64 %call_arg15)
  store ptr %call_result16, ptr %local_35, align 8
  br label %bb23

bb6:                                              ; preds = %bb12
  %machine_payload_ptr17 = getelementptr inbounds nuw %IoError, ptr %local_0, i32 0, i32 1
  %machine_variant_field_ptr18 = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr17, i32 0, i32 0
  %move_load19 = load i64, ptr %machine_variant_field_ptr18, align 8
  store i64 %move_load19, ptr %local_39, align 8
  store ptr @str_lit.15, ptr %local_40, align 8
  %call_arg20 = load i64, ptr %local_39, align 8
  %call_result21 = call ptr @hew_i64_to_string(i64 %call_arg20)
  store ptr %call_result21, ptr %local_41, align 8
  br label %bb26

bb7:                                              ; preds = %bb13
  %machine_payload_ptr22 = getelementptr inbounds nuw %IoError, ptr %local_0, i32 0, i32 1
  %machine_variant_field_ptr23 = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr22, i32 0, i32 0
  %move_load24 = load i64, ptr %machine_variant_field_ptr23, align 8
  store i64 %move_load24, ptr %local_45, align 8
  store ptr @str_lit.16, ptr %local_46, align 8
  %call_arg25 = load i64, ptr %local_45, align 8
  %call_result26 = call ptr @hew_i64_to_string(i64 %call_arg25)
  store ptr %call_result26, ptr %local_47, align 8
  br label %bb29

bb8:                                              ; preds = %bb13
  call void @__hew_enum_drop_inplace_IoError(ptr %local_0)
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

bb9:                                              ; preds = %bb0
  store i64 1, ptr %local_5, align 8
  %cmp_lhs27 = load i64, ptr %local_2, align 8
  %cmp_rhs28 = load i64, ptr %local_5, align 8
  %cmp_bit29 = icmp eq i64 %cmp_lhs27, %cmp_rhs28
  %cmp_zext30 = zext i1 %cmp_bit29 to i8
  store i8 %cmp_zext30, ptr %local_6, align 1
  %cond_load31 = load i8, ptr %local_6, align 1
  %cond_nz32 = icmp ne i8 %cond_load31, 0
  br i1 %cond_nz32, label %bb3, label %bb10

bb10:                                             ; preds = %bb9
  store i64 2, ptr %local_7, align 8
  %cmp_lhs33 = load i64, ptr %local_2, align 8
  %cmp_rhs34 = load i64, ptr %local_7, align 8
  %cmp_bit35 = icmp eq i64 %cmp_lhs33, %cmp_rhs34
  %cmp_zext36 = zext i1 %cmp_bit35 to i8
  store i8 %cmp_zext36, ptr %local_8, align 1
  %cond_load37 = load i8, ptr %local_8, align 1
  %cond_nz38 = icmp ne i8 %cond_load37, 0
  br i1 %cond_nz38, label %bb4, label %bb11

bb11:                                             ; preds = %bb10
  store i64 3, ptr %local_9, align 8
  %cmp_lhs39 = load i64, ptr %local_2, align 8
  %cmp_rhs40 = load i64, ptr %local_9, align 8
  %cmp_bit41 = icmp eq i64 %cmp_lhs39, %cmp_rhs40
  %cmp_zext42 = zext i1 %cmp_bit41 to i8
  store i8 %cmp_zext42, ptr %local_10, align 1
  %cond_load43 = load i8, ptr %local_10, align 1
  %cond_nz44 = icmp ne i8 %cond_load43, 0
  br i1 %cond_nz44, label %bb5, label %bb12

bb12:                                             ; preds = %bb11
  store i64 4, ptr %local_11, align 8
  %cmp_lhs45 = load i64, ptr %local_2, align 8
  %cmp_rhs46 = load i64, ptr %local_11, align 8
  %cmp_bit47 = icmp eq i64 %cmp_lhs45, %cmp_rhs46
  %cmp_zext48 = zext i1 %cmp_bit47 to i8
  store i8 %cmp_zext48, ptr %local_12, align 1
  %cond_load49 = load i8, ptr %local_12, align 1
  %cond_nz50 = icmp ne i8 %cond_load49, 0
  br i1 %cond_nz50, label %bb6, label %bb13

bb13:                                             ; preds = %bb12
  store i64 5, ptr %local_13, align 8
  %cmp_lhs51 = load i64, ptr %local_2, align 8
  %cmp_rhs52 = load i64, ptr %local_13, align 8
  %cmp_bit53 = icmp eq i64 %cmp_lhs51, %cmp_rhs52
  %cmp_zext54 = zext i1 %cmp_bit53 to i8
  store i8 %cmp_zext54, ptr %local_14, align 1
  %cond_load55 = load i8, ptr %local_14, align 1
  %cond_nz56 = icmp ne i8 %cond_load55, 0
  br i1 %cond_nz56, label %bb7, label %bb8

bb14:                                             ; preds = %bb2
  %call_arg57 = load ptr, ptr %local_16, align 8
  %call_arg58 = load ptr, ptr %local_17, align 8
  %call_result59 = call ptr @hew_string_concat(ptr %call_arg57, ptr %call_arg58)
  store ptr %call_result59, ptr %local_18, align 8
  br label %bb15

bb15:                                             ; preds = %bb14
  %"hew_string_drop drop" = load ptr, ptr %local_17, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop")
  store ptr null, ptr %local_17, align 8
  store ptr @str_lit.17, ptr %local_19, align 8
  %call_arg60 = load ptr, ptr %local_18, align 8
  %call_arg61 = load ptr, ptr %local_19, align 8
  %call_result62 = call ptr @hew_string_concat(ptr %call_arg60, ptr %call_arg61)
  store ptr %call_result62, ptr %local_20, align 8
  br label %bb16

bb16:                                             ; preds = %bb15
  %"hew_string_drop drop63" = load ptr, ptr %local_18, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop63")
  store ptr null, ptr %local_18, align 8
  %move_load64 = load ptr, ptr %local_20, align 8
  store ptr %move_load64, ptr %local_1, align 8
  %hew_actor_cooperate65 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel66 = icmp eq i32 %hew_actor_cooperate65, 2
  br i1 %hew_cooperate_is_cancel66, label %cancel_exit67, label %after_cooperate68

bb17:                                             ; preds = %bb3
  %call_arg69 = load ptr, ptr %local_22, align 8
  %call_arg70 = load ptr, ptr %local_23, align 8
  %call_result71 = call ptr @hew_string_concat(ptr %call_arg69, ptr %call_arg70)
  store ptr %call_result71, ptr %local_24, align 8
  br label %bb18

bb18:                                             ; preds = %bb17
  %"hew_string_drop drop72" = load ptr, ptr %local_23, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop72")
  store ptr null, ptr %local_23, align 8
  store ptr @str_lit.18, ptr %local_25, align 8
  %call_arg73 = load ptr, ptr %local_24, align 8
  %call_arg74 = load ptr, ptr %local_25, align 8
  %call_result75 = call ptr @hew_string_concat(ptr %call_arg73, ptr %call_arg74)
  store ptr %call_result75, ptr %local_26, align 8
  br label %bb19

bb19:                                             ; preds = %bb18
  %"hew_string_drop drop76" = load ptr, ptr %local_24, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop76")
  store ptr null, ptr %local_24, align 8
  %move_load77 = load ptr, ptr %local_26, align 8
  store ptr %move_load77, ptr %local_1, align 8
  %hew_actor_cooperate78 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel79 = icmp eq i32 %hew_actor_cooperate78, 2
  br i1 %hew_cooperate_is_cancel79, label %cancel_exit80, label %after_cooperate81

bb20:                                             ; preds = %bb4
  %call_arg82 = load ptr, ptr %local_28, align 8
  %call_arg83 = load ptr, ptr %local_29, align 8
  %call_result84 = call ptr @hew_string_concat(ptr %call_arg82, ptr %call_arg83)
  store ptr %call_result84, ptr %local_30, align 8
  br label %bb21

bb21:                                             ; preds = %bb20
  %"hew_string_drop drop85" = load ptr, ptr %local_29, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop85")
  store ptr null, ptr %local_29, align 8
  store ptr @str_lit.19, ptr %local_31, align 8
  %call_arg86 = load ptr, ptr %local_30, align 8
  %call_arg87 = load ptr, ptr %local_31, align 8
  %call_result88 = call ptr @hew_string_concat(ptr %call_arg86, ptr %call_arg87)
  store ptr %call_result88, ptr %local_32, align 8
  br label %bb22

bb22:                                             ; preds = %bb21
  %"hew_string_drop drop89" = load ptr, ptr %local_30, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop89")
  store ptr null, ptr %local_30, align 8
  %move_load90 = load ptr, ptr %local_32, align 8
  store ptr %move_load90, ptr %local_1, align 8
  %hew_actor_cooperate91 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel92 = icmp eq i32 %hew_actor_cooperate91, 2
  br i1 %hew_cooperate_is_cancel92, label %cancel_exit93, label %after_cooperate94

bb23:                                             ; preds = %bb5
  %call_arg95 = load ptr, ptr %local_34, align 8
  %call_arg96 = load ptr, ptr %local_35, align 8
  %call_result97 = call ptr @hew_string_concat(ptr %call_arg95, ptr %call_arg96)
  store ptr %call_result97, ptr %local_36, align 8
  br label %bb24

bb24:                                             ; preds = %bb23
  %"hew_string_drop drop98" = load ptr, ptr %local_35, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop98")
  store ptr null, ptr %local_35, align 8
  store ptr @str_lit.20, ptr %local_37, align 8
  %call_arg99 = load ptr, ptr %local_36, align 8
  %call_arg100 = load ptr, ptr %local_37, align 8
  %call_result101 = call ptr @hew_string_concat(ptr %call_arg99, ptr %call_arg100)
  store ptr %call_result101, ptr %local_38, align 8
  br label %bb25

bb25:                                             ; preds = %bb24
  %"hew_string_drop drop102" = load ptr, ptr %local_36, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop102")
  store ptr null, ptr %local_36, align 8
  %move_load103 = load ptr, ptr %local_38, align 8
  store ptr %move_load103, ptr %local_1, align 8
  %hew_actor_cooperate104 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel105 = icmp eq i32 %hew_actor_cooperate104, 2
  br i1 %hew_cooperate_is_cancel105, label %cancel_exit106, label %after_cooperate107

bb26:                                             ; preds = %bb6
  %call_arg108 = load ptr, ptr %local_40, align 8
  %call_arg109 = load ptr, ptr %local_41, align 8
  %call_result110 = call ptr @hew_string_concat(ptr %call_arg108, ptr %call_arg109)
  store ptr %call_result110, ptr %local_42, align 8
  br label %bb27

bb27:                                             ; preds = %bb26
  %"hew_string_drop drop111" = load ptr, ptr %local_41, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop111")
  store ptr null, ptr %local_41, align 8
  store ptr @str_lit.21, ptr %local_43, align 8
  %call_arg112 = load ptr, ptr %local_42, align 8
  %call_arg113 = load ptr, ptr %local_43, align 8
  %call_result114 = call ptr @hew_string_concat(ptr %call_arg112, ptr %call_arg113)
  store ptr %call_result114, ptr %local_44, align 8
  br label %bb28

bb28:                                             ; preds = %bb27
  %"hew_string_drop drop115" = load ptr, ptr %local_42, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop115")
  store ptr null, ptr %local_42, align 8
  %move_load116 = load ptr, ptr %local_44, align 8
  store ptr %move_load116, ptr %local_1, align 8
  %hew_actor_cooperate117 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel118 = icmp eq i32 %hew_actor_cooperate117, 2
  br i1 %hew_cooperate_is_cancel118, label %cancel_exit119, label %after_cooperate120

bb29:                                             ; preds = %bb7
  %call_arg121 = load ptr, ptr %local_46, align 8
  %call_arg122 = load ptr, ptr %local_47, align 8
  %call_result123 = call ptr @hew_string_concat(ptr %call_arg121, ptr %call_arg122)
  store ptr %call_result123, ptr %local_48, align 8
  br label %bb30

bb30:                                             ; preds = %bb29
  %"hew_string_drop drop124" = load ptr, ptr %local_47, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop124")
  store ptr null, ptr %local_47, align 8
  store ptr @str_lit.22, ptr %local_49, align 8
  %call_arg125 = load ptr, ptr %local_48, align 8
  %call_arg126 = load ptr, ptr %local_49, align 8
  %call_result127 = call ptr @hew_string_concat(ptr %call_arg125, ptr %call_arg126)
  store ptr %call_result127, ptr %local_50, align 8
  br label %bb31

bb31:                                             ; preds = %bb30
  %"hew_string_drop drop128" = load ptr, ptr %local_48, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop128")
  store ptr null, ptr %local_48, align 8
  %move_load129 = load ptr, ptr %local_50, align 8
  store ptr %move_load129, ptr %local_1, align 8
  %hew_actor_cooperate130 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel131 = icmp eq i32 %hew_actor_cooperate130, 2
  br i1 %hew_cooperate_is_cancel131, label %cancel_exit132, label %after_cooperate133

cancel_exit:                                      ; preds = %entry
  call void @__hew_enum_drop_inplace_IoError(ptr %local_0)
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit67:                                    ; preds = %bb16
  call void @__hew_enum_drop_inplace_IoError(ptr %local_0)
  ret ptr null

after_cooperate68:                                ; preds = %bb16
  br label %bb1

cancel_exit80:                                    ; preds = %bb19
  call void @__hew_enum_drop_inplace_IoError(ptr %local_0)
  ret ptr null

after_cooperate81:                                ; preds = %bb19
  br label %bb1

cancel_exit93:                                    ; preds = %bb22
  call void @__hew_enum_drop_inplace_IoError(ptr %local_0)
  ret ptr null

after_cooperate94:                                ; preds = %bb22
  br label %bb1

cancel_exit106:                                   ; preds = %bb25
  call void @__hew_enum_drop_inplace_IoError(ptr %local_0)
  ret ptr null

after_cooperate107:                               ; preds = %bb25
  br label %bb1

cancel_exit119:                                   ; preds = %bb28
  call void @__hew_enum_drop_inplace_IoError(ptr %local_0)
  ret ptr null

after_cooperate120:                               ; preds = %bb28
  br label %bb1

cancel_exit132:                                   ; preds = %bb31
  call void @__hew_enum_drop_inplace_IoError(ptr %local_0)
  ret ptr null

after_cooperate133:                               ; preds = %bb31
  br label %bb1
}

define internal %IoError @"fs$io_error_timed_out"() {
entry:
  %return_slot = alloca %IoError, align 8
  %local_0 = alloca %IoError, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  store i64 3, ptr %local_1, align 8
  %machine_tag_ptr = getelementptr inbounds nuw %IoError, ptr %local_0, i32 0, i32 0
  %move_iN_load_wide = load i64, ptr %local_1, align 8
  %move_iN_trunc = trunc i64 %move_iN_load_wide to i8
  store i8 %move_iN_trunc, ptr %machine_tag_ptr, align 1
  store i64 0, ptr %local_2, align 8
  %machine_payload_ptr = getelementptr inbounds nuw %IoError, ptr %local_0, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr, i32 0, i32 0
  %move_load = load i64, ptr %local_2, align 8
  store i64 %move_load, ptr %machine_variant_field_ptr, align 8
  %move_load1 = load %IoError, ptr %local_0, align 8
  store %IoError %move_load1, ptr %return_slot, align 8
  %ret_val = load %IoError, ptr %return_slot, align 8
  ret %IoError %ret_val
}

define internal %IoError @"fs$io_error_cancelled"() {
entry:
  %return_slot = alloca %IoError, align 8
  %local_0 = alloca %IoError, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  store i64 4, ptr %local_1, align 8
  %machine_tag_ptr = getelementptr inbounds nuw %IoError, ptr %local_0, i32 0, i32 0
  %move_iN_load_wide = load i64, ptr %local_1, align 8
  %move_iN_trunc = trunc i64 %move_iN_load_wide to i8
  store i8 %move_iN_trunc, ptr %machine_tag_ptr, align 1
  store i64 0, ptr %local_2, align 8
  %machine_payload_ptr = getelementptr inbounds nuw %IoError, ptr %local_0, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr, i32 0, i32 0
  %move_load = load i64, ptr %local_2, align 8
  store i64 %move_load, ptr %machine_variant_field_ptr, align 8
  %move_load1 = load %IoError, ptr %local_0, align 8
  store %IoError %move_load1, ptr %return_slot, align 8
  %ret_val = load %IoError, ptr %return_slot, align 8
  ret %IoError %ret_val
}

define internal %IoError @"fs$last_io_error"() {
entry:
  %return_slot = alloca %IoError, align 8
  %local_0 = alloca i32, align 4
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i32, align 4
  %local_4 = alloca i64, align 8
  %local_5 = alloca i64, align 8
  %local_6 = alloca ptr, align 8
  %local_7 = alloca %IoError, align 8
  %local_8 = alloca i8, align 1
  %local_9 = alloca i64, align 8
  %local_10 = alloca i8, align 1
  %local_11 = alloca i64, align 8
  %local_12 = alloca i8, align 1
  %local_13 = alloca %IoError, align 8
  %local_14 = alloca i64, align 8
  %local_15 = alloca i64, align 8
  %local_16 = alloca %IoError, align 8
  %local_17 = alloca %IoError, align 8
  %local_18 = alloca %IoError, align 8
  %local_19 = alloca %IoError, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_result = call i32 @hew_stream_last_error_kind()
  store i32 %call_result, ptr %local_0, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %cast_int_src = load i32, ptr %local_0, align 4
  %cast_int_sext = sext i32 %cast_int_src to i64
  store i64 %cast_int_sext, ptr %local_1, align 8
  %move_load = load i64, ptr %local_1, align 8
  store i64 %move_load, ptr %local_2, align 8
  %call_result1 = call i32 @hew_stream_last_errno()
  store i32 %call_result1, ptr %local_3, align 4
  br label %bb2

bb2:                                              ; preds = %bb1
  %cast_int_src2 = load i32, ptr %local_3, align 4
  %cast_int_sext3 = sext i32 %cast_int_src2 to i64
  store i64 %cast_int_sext3, ptr %local_4, align 8
  %move_load4 = load i64, ptr %local_4, align 8
  store i64 %move_load4, ptr %local_5, align 8
  %call_result5 = call ptr @hew_stream_last_error()
  store ptr %call_result5, ptr %local_6, align 8
  br label %bb3

bb3:                                              ; preds = %bb2
  %"hew_string_drop drop" = load ptr, ptr %local_6, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop")
  store ptr null, ptr %local_6, align 8
  store i8 0, ptr %local_8, align 1
  store i64 0, ptr %local_9, align 8
  %cmp_lhs = load i64, ptr %local_5, align 8
  %cmp_rhs = load i64, ptr %local_9, align 8
  %cmp_bit = icmp eq i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_10, align 1
  %cond_load = load i8, ptr %local_10, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb4, label %bb5

bb4:                                              ; preds = %bb3
  store i64 0, ptr %local_11, align 8
  %cmp_lhs6 = load i64, ptr %local_2, align 8
  %cmp_rhs7 = load i64, ptr %local_11, align 8
  %cmp_bit8 = icmp eq i64 %cmp_lhs6, %cmp_rhs7
  %cmp_zext9 = zext i1 %cmp_bit8 to i8
  store i8 %cmp_zext9, ptr %local_12, align 1
  %move_load10 = load i8, ptr %local_12, align 1
  store i8 %move_load10, ptr %local_8, align 1
  br label %bb5

bb5:                                              ; preds = %bb4, %bb3
  %cond_load11 = load i8, ptr %local_8, align 1
  %cond_nz12 = icmp ne i8 %cond_load11, 0
  br i1 %cond_nz12, label %bb6, label %bb7

bb6:                                              ; preds = %bb5
  store i64 5, ptr %local_14, align 8
  %machine_tag_ptr = getelementptr inbounds nuw %IoError, ptr %local_13, i32 0, i32 0
  %move_iN_load_wide = load i64, ptr %local_14, align 8
  %move_iN_trunc = trunc i64 %move_iN_load_wide to i8
  store i8 %move_iN_trunc, ptr %machine_tag_ptr, align 1
  store i64 5, ptr %local_15, align 8
  %machine_payload_ptr = getelementptr inbounds nuw %IoError, ptr %local_13, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr, i32 0, i32 0
  %move_load13 = load i64, ptr %local_15, align 8
  store i64 %move_load13, ptr %machine_variant_field_ptr, align 8
  %move_load14 = load %IoError, ptr %local_13, align 8
  store %IoError %move_load14, ptr %local_16, align 8
  %move_load15 = load %IoError, ptr %local_16, align 8
  store %IoError %move_load15, ptr %local_7, align 8
  br label %bb8

bb7:                                              ; preds = %bb5
  %call_arg = load i64, ptr %local_2, align 8
  %call_arg16 = load i64, ptr %local_5, align 8
  %call_result17 = call %IoError @"fs$io_error_from_kind"(i64 %call_arg, i64 %call_arg16)
  store %IoError %call_result17, ptr %local_17, align 8
  br label %bb9

bb8:                                              ; preds = %after_cooperate25, %bb6
  %move_load18 = load %IoError, ptr %local_7, align 8
  store %IoError %move_load18, ptr %local_19, align 8
  %move_load19 = load %IoError, ptr %local_19, align 8
  store %IoError %move_load19, ptr %return_slot, align 8
  %ret_val = load %IoError, ptr %return_slot, align 8
  ret %IoError %ret_val

bb9:                                              ; preds = %bb7
  %move_load20 = load %IoError, ptr %local_17, align 8
  store %IoError %move_load20, ptr %local_18, align 8
  %move_load21 = load %IoError, ptr %local_18, align 8
  store %IoError %move_load21, ptr %local_7, align 8
  %hew_actor_cooperate22 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel23 = icmp eq i32 %hew_actor_cooperate22, 2
  br i1 %hew_cooperate_is_cancel23, label %cancel_exit24, label %after_cooperate25

cancel_exit:                                      ; preds = %entry
  ret %IoError zeroinitializer

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit24:                                    ; preds = %bb9
  ret %IoError zeroinitializer

after_cooperate25:                                ; preds = %bb9
  br label %bb8
}

define internal ptr @"fs$read"(ptr %0) {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca ptr, align 8
  %local_3 = alloca i32, align 4
  %local_4 = alloca i32, align 4
  %local_5 = alloca i8, align 1
  %local_6 = alloca i32, align 4
  %local_7 = alloca i8, align 1
  %local_8 = alloca ptr, align 8
  %local_9 = alloca ptr, align 8
  %local_10 = alloca i8, align 1
  %local_11 = alloca i64, align 8
  %local_12 = alloca i64, align 8
  %local_13 = alloca i8, align 1
  %local_14 = alloca ptr, align 8
  %local_15 = alloca ptr, align 8
  %local_16 = alloca ptr, align 8
  %local_17 = alloca ptr, align 8
  %local_18 = alloca ptr, align 8
  %local_19 = alloca i64, align 8
  %local_20 = alloca %IoError, align 8
  %local_21 = alloca ptr, align 8
  %local_22 = alloca ptr, align 8
  %local_23 = alloca ptr, align 8
  %local_24 = alloca ptr, align 8
  %local_25 = alloca ptr, align 8
  %local_26 = alloca ptr, align 8
  %local_27 = alloca ptr, align 8
  %local_28 = alloca ptr, align 8
  %local_29 = alloca ptr, align 8
  %local_30 = alloca ptr, align 8
  %local_31 = alloca i8, align 1
  %local_32 = alloca ptr, align 8
  %local_33 = alloca %IoError, align 8
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result = call ptr @hew_file_read(ptr %call_arg)
  store ptr %call_result, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %local_2, align 8
  %call_result1 = call i32 @hew_stream_last_errno()
  store i32 %call_result1, ptr %local_3, align 4
  br label %bb2

bb2:                                              ; preds = %bb1
  %move_load2 = load i32, ptr %local_3, align 4
  store i32 %move_load2, ptr %local_4, align 4
  store i32 0, ptr %local_6, align 4
  %cmp_lhs = load i32, ptr %local_4, align 4
  %cmp_rhs = load i32, ptr %local_6, align 4
  %cmp_bit = icmp ne i32 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_7, align 1
  %cond_load = load i8, ptr %local_7, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb3, label %bb4

bb3:                                              ; preds = %bb2
  %call_result3 = call ptr @hew_stream_last_error()
  store ptr %call_result3, ptr %local_8, align 8
  br label %bb6

bb4:                                              ; preds = %bb2
  br label %bb5

bb5:                                              ; preds = %after_cooperate24, %bb4
  %move_load4 = load ptr, ptr %local_2, align 8
  store ptr %move_load4, ptr %local_32, align 8
  %move_load5 = load ptr, ptr %local_32, align 8
  store ptr %move_load5, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

bb6:                                              ; preds = %bb3
  %move_load6 = load ptr, ptr %local_8, align 8
  store ptr %move_load6, ptr %local_9, align 8
  %call_arg7 = load ptr, ptr %local_9, align 8
  %call_result8 = call i32 @hew_string_length(ptr %call_arg7)
  %ffi_sext = sext i32 %call_result8 to i64
  store i64 %ffi_sext, ptr %local_11, align 8
  br label %bb7

bb7:                                              ; preds = %bb6
  store i64 0, ptr %local_12, align 8
  %cmp_lhs9 = load i64, ptr %local_11, align 8
  %cmp_rhs10 = load i64, ptr %local_12, align 8
  %cmp_bit11 = icmp eq i64 %cmp_lhs9, %cmp_rhs10
  %cmp_zext12 = zext i1 %cmp_bit11 to i8
  store i8 %cmp_zext12, ptr %local_13, align 1
  %cond_load13 = load i8, ptr %local_13, align 1
  %cond_nz14 = icmp ne i8 %cond_load13, 0
  br i1 %cond_nz14, label %bb8, label %bb9

bb8:                                              ; preds = %bb7
  store ptr @str_lit.23, ptr %local_14, align 8
  %call_arg15 = load ptr, ptr %local_0, align 8
  %call_result16 = call ptr @"string::fmt"(ptr %call_arg15)
  store ptr %call_result16, ptr %local_15, align 8
  br label %bb11

bb9:                                              ; preds = %bb7
  store ptr @str_lit.24, ptr %local_24, align 8
  %call_arg17 = load ptr, ptr %local_0, align 8
  %call_result18 = call ptr @"string::fmt"(ptr %call_arg17)
  store ptr %call_result18, ptr %local_25, align 8
  br label %bb19

bb10:                                             ; preds = %after_cooperate73, %after_cooperate50
  %move_load19 = load i8, ptr %local_10, align 1
  store i8 %move_load19, ptr %local_31, align 1
  %move_load20 = load i8, ptr %local_31, align 1
  store i8 %move_load20, ptr %local_5, align 1
  %hew_actor_cooperate21 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel22 = icmp eq i32 %hew_actor_cooperate21, 2
  br i1 %hew_cooperate_is_cancel22, label %cancel_exit23, label %after_cooperate24

bb11:                                             ; preds = %bb8
  %call_arg26 = load ptr, ptr %local_14, align 8
  %call_arg27 = load ptr, ptr %local_15, align 8
  %call_result28 = call ptr @hew_string_concat(ptr %call_arg26, ptr %call_arg27)
  store ptr %call_result28, ptr %local_16, align 8
  br label %bb12

bb12:                                             ; preds = %bb11
  %"hew_string_drop drop29" = load ptr, ptr %local_15, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop29")
  store ptr null, ptr %local_15, align 8
  store ptr @str_lit.25, ptr %local_17, align 8
  %call_arg30 = load ptr, ptr %local_16, align 8
  %call_arg31 = load ptr, ptr %local_17, align 8
  %call_result32 = call ptr @hew_string_concat(ptr %call_arg30, ptr %call_arg31)
  store ptr %call_result32, ptr %local_18, align 8
  br label %bb13

bb13:                                             ; preds = %bb12
  %"hew_string_drop drop33" = load ptr, ptr %local_16, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop33")
  store ptr null, ptr %local_16, align 8
  %cast_int_src = load i32, ptr %local_4, align 4
  %cast_int_sext = sext i32 %cast_int_src to i64
  store i64 %cast_int_sext, ptr %local_19, align 8
  %call_arg34 = load i64, ptr %local_19, align 8
  %call_result35 = call %IoError @"fs$io_error_from_errno"(i64 %call_arg34)
  store %IoError %call_result35, ptr %local_20, align 8
  br label %bb14

bb14:                                             ; preds = %bb13
  %move_load36 = load %IoError, ptr %local_20, align 8
  store %IoError %move_load36, ptr %local_33, align 8
  store %IoError zeroinitializer, ptr %local_20, align 8
  %call_arg37 = load %IoError, ptr %local_33, align 8
  %call_result38 = call ptr @"fs$io_error_message"(%IoError %call_arg37)
  store ptr %call_result38, ptr %local_21, align 8
  br label %bb15

bb15:                                             ; preds = %bb14
  %call_arg39 = load ptr, ptr %local_21, align 8
  %call_result40 = call ptr @"string::fmt"(ptr %call_arg39)
  store ptr %call_result40, ptr %local_22, align 8
  br label %bb16

bb16:                                             ; preds = %bb15
  %call_arg41 = load ptr, ptr %local_18, align 8
  %call_arg42 = load ptr, ptr %local_22, align 8
  %call_result43 = call ptr @hew_string_concat(ptr %call_arg41, ptr %call_arg42)
  store ptr %call_result43, ptr %local_23, align 8
  br label %bb17

bb17:                                             ; preds = %bb16
  %"hew_string_drop drop44" = load ptr, ptr %local_22, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop44")
  store ptr null, ptr %local_22, align 8
  %"hew_string_drop drop45" = load ptr, ptr %local_18, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop45")
  store ptr null, ptr %local_18, align 8
  %call_arg46 = load ptr, ptr %local_23, align 8
  call void @hew_panic_msg(ptr %call_arg46)
  br label %bb18

bb18:                                             ; preds = %bb17
  %hew_actor_cooperate47 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel48 = icmp eq i32 %hew_actor_cooperate47, 2
  br i1 %hew_cooperate_is_cancel48, label %cancel_exit49, label %after_cooperate50

bb19:                                             ; preds = %bb9
  %call_arg54 = load ptr, ptr %local_24, align 8
  %call_arg55 = load ptr, ptr %local_25, align 8
  %call_result56 = call ptr @hew_string_concat(ptr %call_arg54, ptr %call_arg55)
  store ptr %call_result56, ptr %local_26, align 8
  br label %bb20

bb20:                                             ; preds = %bb19
  %"hew_string_drop drop57" = load ptr, ptr %local_25, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop57")
  store ptr null, ptr %local_25, align 8
  store ptr @str_lit.26, ptr %local_27, align 8
  %call_arg58 = load ptr, ptr %local_26, align 8
  %call_arg59 = load ptr, ptr %local_27, align 8
  %call_result60 = call ptr @hew_string_concat(ptr %call_arg58, ptr %call_arg59)
  store ptr %call_result60, ptr %local_28, align 8
  br label %bb21

bb21:                                             ; preds = %bb20
  %"hew_string_drop drop61" = load ptr, ptr %local_26, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop61")
  store ptr null, ptr %local_26, align 8
  %call_arg62 = load ptr, ptr %local_9, align 8
  %call_result63 = call ptr @"string::fmt"(ptr %call_arg62)
  store ptr %call_result63, ptr %local_29, align 8
  br label %bb22

bb22:                                             ; preds = %bb21
  %call_arg64 = load ptr, ptr %local_28, align 8
  %call_arg65 = load ptr, ptr %local_29, align 8
  %call_result66 = call ptr @hew_string_concat(ptr %call_arg64, ptr %call_arg65)
  store ptr %call_result66, ptr %local_30, align 8
  br label %bb23

bb23:                                             ; preds = %bb22
  %"hew_string_drop drop67" = load ptr, ptr %local_29, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop67")
  store ptr null, ptr %local_29, align 8
  %"hew_string_drop drop68" = load ptr, ptr %local_28, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop68")
  store ptr null, ptr %local_28, align 8
  %call_arg69 = load ptr, ptr %local_30, align 8
  call void @hew_panic_msg(ptr %call_arg69)
  br label %bb24

bb24:                                             ; preds = %bb23
  %hew_actor_cooperate70 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel71 = icmp eq i32 %hew_actor_cooperate70, 2
  br i1 %hew_cooperate_is_cancel71, label %cancel_exit72, label %after_cooperate73

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit23:                                    ; preds = %bb10
  %"hew_string_drop drop" = load ptr, ptr %local_9, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop")
  store ptr null, ptr %local_9, align 8
  ret ptr null

after_cooperate24:                                ; preds = %bb10
  %"hew_string_drop drop25" = load ptr, ptr %local_9, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop25")
  store ptr null, ptr %local_9, align 8
  br label %bb5

cancel_exit49:                                    ; preds = %bb18
  %"hew_string_drop drop51" = load ptr, ptr %local_23, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop51")
  store ptr null, ptr %local_23, align 8
  %"hew_string_drop drop52" = load ptr, ptr %local_9, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop52")
  store ptr null, ptr %local_9, align 8
  ret ptr null

after_cooperate50:                                ; preds = %bb18
  %"hew_string_drop drop53" = load ptr, ptr %local_23, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop53")
  store ptr null, ptr %local_23, align 8
  br label %bb10

cancel_exit72:                                    ; preds = %bb24
  %"hew_string_drop drop74" = load ptr, ptr %local_30, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop74")
  store ptr null, ptr %local_30, align 8
  %"hew_string_drop drop75" = load ptr, ptr %local_9, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop75")
  store ptr null, ptr %local_9, align 8
  ret ptr null

after_cooperate73:                                ; preds = %bb24
  %"hew_string_drop drop76" = load ptr, ptr %local_30, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop76")
  store ptr null, ptr %local_30, align 8
  br label %bb10
}

define internal %"Result$$string$IoError" @"fs$try_read"(ptr %0) {
entry:
  %return_slot = alloca %"Result$$string$IoError", align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca ptr, align 8
  %local_3 = alloca %"Result$$string$IoError", align 8
  %local_4 = alloca i8, align 1
  %local_5 = alloca ptr, align 8
  %local_6 = alloca ptr, align 8
  %local_7 = alloca i32, align 4
  %local_8 = alloca i64, align 8
  %local_9 = alloca i64, align 8
  %local_10 = alloca i32, align 4
  %local_11 = alloca i32, align 4
  %local_12 = alloca ptr, align 8
  %local_13 = alloca %"Result$$string$IoError", align 8
  %local_14 = alloca i32, align 4
  %local_15 = alloca i8, align 1
  %local_16 = alloca %"Result$$string$IoError", align 8
  %local_17 = alloca i64, align 8
  %local_18 = alloca %"Result$$string$IoError", align 8
  %local_19 = alloca %"Result$$string$IoError", align 8
  %local_20 = alloca i64, align 8
  %local_21 = alloca i64, align 8
  %local_22 = alloca %IoError, align 8
  %local_23 = alloca %"Result$$string$IoError", align 8
  %local_24 = alloca %"Result$$string$IoError", align 8
  %local_25 = alloca %"Result$$string$IoError", align 8
  %local_26 = alloca i64, align 8
  %local_27 = alloca %IoError, align 8
  %local_28 = alloca %"Result$$string$IoError", align 8
  %local_29 = alloca %"Result$$string$IoError", align 8
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result = call ptr @hew_stream_from_file_read(ptr %call_arg)
  store ptr %call_result, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %local_2, align 8
  %call_arg1 = load ptr, ptr %local_2, align 8
  %call_result2 = call i8 @hew_stream_is_valid(ptr %call_arg1)
  store i8 %call_result2, ptr %local_4, align 1
  br label %bb2

bb2:                                              ; preds = %bb1
  %cond_load = load i8, ptr %local_4, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb3, label %bb4

bb3:                                              ; preds = %bb2
  %call_arg3 = load ptr, ptr %local_2, align 8
  %call_result4 = call ptr @hew_stream_collect_string(ptr %call_arg3)
  store ptr %call_result4, ptr %local_5, align 8
  br label %bb6

bb4:                                              ; preds = %bb2
  store i64 1, ptr %local_26, align 8
  %machine_tag_ptr = getelementptr inbounds nuw %"Result$$string$IoError", ptr %local_25, i32 0, i32 0
  %move_iN_load_wide = load i64, ptr %local_26, align 8
  %move_iN_trunc = trunc i64 %move_iN_load_wide to i8
  store i8 %move_iN_trunc, ptr %machine_tag_ptr, align 1
  %call_result5 = call %IoError @"fs$last_io_error"()
  store %IoError %call_result5, ptr %local_27, align 8
  br label %bb14

bb5:                                              ; preds = %after_cooperate53, %after_cooperate35
  %move_load6 = load %"Result$$string$IoError", ptr %local_3, align 8
  store %"Result$$string$IoError" %move_load6, ptr %local_29, align 8
  %move_load7 = load %"Result$$string$IoError", ptr %local_29, align 8
  store %"Result$$string$IoError" %move_load7, ptr %return_slot, align 8
  %ret_val = load %"Result$$string$IoError", ptr %return_slot, align 8
  ret %"Result$$string$IoError" %ret_val

bb6:                                              ; preds = %bb3
  %move_load8 = load ptr, ptr %local_5, align 8
  store ptr %move_load8, ptr %local_6, align 8
  %call_result9 = call i32 @hew_stream_last_error_kind()
  store i32 %call_result9, ptr %local_7, align 4
  br label %bb7

bb7:                                              ; preds = %bb6
  %cast_int_src = load i32, ptr %local_7, align 4
  %cast_int_sext = sext i32 %cast_int_src to i64
  store i64 %cast_int_sext, ptr %local_8, align 8
  %move_load10 = load i64, ptr %local_8, align 8
  store i64 %move_load10, ptr %local_9, align 8
  %call_result11 = call i32 @hew_stream_last_errno()
  store i32 %call_result11, ptr %local_10, align 4
  br label %bb8

bb8:                                              ; preds = %bb7
  %move_load12 = load i32, ptr %local_10, align 4
  store i32 %move_load12, ptr %local_11, align 4
  %call_result13 = call ptr @hew_stream_last_error()
  store ptr %call_result13, ptr %local_12, align 8
  br label %bb9

bb9:                                              ; preds = %bb8
  %"hew_string_drop drop" = load ptr, ptr %local_12, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop")
  store ptr null, ptr %local_12, align 8
  store i32 0, ptr %local_14, align 4
  %cmp_lhs = load i32, ptr %local_11, align 4
  %cmp_rhs = load i32, ptr %local_14, align 4
  %cmp_bit = icmp eq i32 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_15, align 1
  %cond_load14 = load i8, ptr %local_15, align 1
  %cond_nz15 = icmp ne i8 %cond_load14, 0
  br i1 %cond_nz15, label %bb10, label %bb11

bb10:                                             ; preds = %bb9
  store i64 0, ptr %local_17, align 8
  %machine_tag_ptr16 = getelementptr inbounds nuw %"Result$$string$IoError", ptr %local_16, i32 0, i32 0
  %move_iN_load_wide17 = load i64, ptr %local_17, align 8
  %move_iN_trunc18 = trunc i64 %move_iN_load_wide17 to i8
  store i8 %move_iN_trunc18, ptr %machine_tag_ptr16, align 1
  %machine_payload_ptr = getelementptr inbounds nuw %"Result$$string$IoError", ptr %local_16, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { ptr }, ptr %machine_payload_ptr, i32 0, i32 0
  %move_load19 = load ptr, ptr %local_6, align 8
  store ptr %move_load19, ptr %machine_variant_field_ptr, align 8
  %move_load20 = load %"Result$$string$IoError", ptr %local_16, align 8
  store %"Result$$string$IoError" %move_load20, ptr %local_18, align 8
  %move_load21 = load %"Result$$string$IoError", ptr %local_18, align 8
  store %"Result$$string$IoError" %move_load21, ptr %local_13, align 8
  br label %bb12

bb11:                                             ; preds = %bb9
  store i64 1, ptr %local_20, align 8
  %machine_tag_ptr22 = getelementptr inbounds nuw %"Result$$string$IoError", ptr %local_19, i32 0, i32 0
  %move_iN_load_wide23 = load i64, ptr %local_20, align 8
  %move_iN_trunc24 = trunc i64 %move_iN_load_wide23 to i8
  store i8 %move_iN_trunc24, ptr %machine_tag_ptr22, align 1
  %cast_int_src25 = load i32, ptr %local_11, align 4
  %cast_int_sext26 = sext i32 %cast_int_src25 to i64
  store i64 %cast_int_sext26, ptr %local_21, align 8
  %call_arg27 = load i64, ptr %local_9, align 8
  %call_arg28 = load i64, ptr %local_21, align 8
  %call_result29 = call %IoError @"fs$io_error_from_kind"(i64 %call_arg27, i64 %call_arg28)
  store %IoError %call_result29, ptr %local_22, align 8
  br label %bb13

bb12:                                             ; preds = %after_cooperate44, %bb10
  %move_load30 = load %"Result$$string$IoError", ptr %local_13, align 8
  store %"Result$$string$IoError" %move_load30, ptr %local_24, align 8
  %move_load31 = load %"Result$$string$IoError", ptr %local_24, align 8
  store %"Result$$string$IoError" %move_load31, ptr %local_3, align 8
  %hew_actor_cooperate32 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel33 = icmp eq i32 %hew_actor_cooperate32, 2
  br i1 %hew_cooperate_is_cancel33, label %cancel_exit34, label %after_cooperate35

bb13:                                             ; preds = %bb11
  %machine_payload_ptr36 = getelementptr inbounds nuw %"Result$$string$IoError", ptr %local_19, i32 0, i32 1
  %machine_variant_field_ptr37 = getelementptr inbounds nuw { %IoError }, ptr %machine_payload_ptr36, i32 0, i32 0
  %move_load38 = load %IoError, ptr %local_22, align 8
  store %IoError %move_load38, ptr %machine_variant_field_ptr37, align 8
  %move_load39 = load %"Result$$string$IoError", ptr %local_19, align 8
  store %"Result$$string$IoError" %move_load39, ptr %local_23, align 8
  %move_load40 = load %"Result$$string$IoError", ptr %local_23, align 8
  store %"Result$$string$IoError" %move_load40, ptr %local_13, align 8
  %hew_actor_cooperate41 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel42 = icmp eq i32 %hew_actor_cooperate41, 2
  br i1 %hew_cooperate_is_cancel42, label %cancel_exit43, label %after_cooperate44

bb14:                                             ; preds = %bb4
  %machine_payload_ptr45 = getelementptr inbounds nuw %"Result$$string$IoError", ptr %local_25, i32 0, i32 1
  %machine_variant_field_ptr46 = getelementptr inbounds nuw { %IoError }, ptr %machine_payload_ptr45, i32 0, i32 0
  %move_load47 = load %IoError, ptr %local_27, align 8
  store %IoError %move_load47, ptr %machine_variant_field_ptr46, align 8
  %move_load48 = load %"Result$$string$IoError", ptr %local_25, align 8
  store %"Result$$string$IoError" %move_load48, ptr %local_28, align 8
  %move_load49 = load %"Result$$string$IoError", ptr %local_28, align 8
  store %"Result$$string$IoError" %move_load49, ptr %local_3, align 8
  %hew_actor_cooperate50 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel51 = icmp eq i32 %hew_actor_cooperate50, 2
  br i1 %hew_cooperate_is_cancel51, label %cancel_exit52, label %after_cooperate53

cancel_exit:                                      ; preds = %entry
  ret %"Result$$string$IoError" zeroinitializer

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit34:                                    ; preds = %bb12
  ret %"Result$$string$IoError" zeroinitializer

after_cooperate35:                                ; preds = %bb12
  br label %bb5

cancel_exit43:                                    ; preds = %bb13
  ret %"Result$$string$IoError" zeroinitializer

after_cooperate44:                                ; preds = %bb13
  br label %bb12

cancel_exit52:                                    ; preds = %bb14
  ret %"Result$$string$IoError" zeroinitializer

after_cooperate53:                                ; preds = %bb14
  br label %bb5
}

define internal i64 @"fs$write"(ptr %0, ptr %1) {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca i32, align 4
  %local_3 = alloca i64, align 8
  %local_4 = alloca i64, align 8
  store ptr %0, ptr %local_0, align 8
  store ptr %1, ptr %local_1, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_arg1 = load ptr, ptr %local_1, align 8
  %call_result = call i32 @hew_file_write(ptr %call_arg, ptr %call_arg1)
  store i32 %call_result, ptr %local_2, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %cast_int_src = load i32, ptr %local_2, align 4
  %cast_int_sext = sext i32 %cast_int_src to i64
  store i64 %cast_int_sext, ptr %local_3, align 8
  %move_load = load i64, ptr %local_3, align 8
  store i64 %move_load, ptr %local_4, align 8
  %move_load2 = load i64, ptr %local_4, align 8
  store i64 %move_load2, ptr %return_slot, align 8
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val

cancel_exit:                                      ; preds = %entry
  ret i64 0

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal %"Result$$i64$IoError" @"fs$try_write"(ptr %0, ptr %1) {
entry:
  %return_slot = alloca %"Result$$i64$IoError", align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca %"Result$$i64$IoError", align 8
  %local_3 = alloca i32, align 4
  %local_4 = alloca i32, align 4
  %local_5 = alloca i32, align 4
  %local_6 = alloca i8, align 1
  %local_7 = alloca %"Result$$i64$IoError", align 8
  %local_8 = alloca i64, align 8
  %local_9 = alloca i64, align 8
  %local_10 = alloca %"Result$$i64$IoError", align 8
  %local_11 = alloca i64, align 8
  %local_12 = alloca %IoError, align 8
  store ptr %0, ptr %local_0, align 8
  store ptr %1, ptr %local_1, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_arg1 = load ptr, ptr %local_1, align 8
  %call_result = call i32 @hew_file_write(ptr %call_arg, ptr %call_arg1)
  store i32 %call_result, ptr %local_3, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load i32, ptr %local_3, align 4
  store i32 %move_load, ptr %local_4, align 4
  br label %bb3

bb2:                                              ; preds = %after_cooperate20, %after_cooperate12
  %move_load2 = load %"Result$$i64$IoError", ptr %local_2, align 8
  store %"Result$$i64$IoError" %move_load2, ptr %return_slot, align 8
  %ret_val = load %"Result$$i64$IoError", ptr %return_slot, align 8
  ret %"Result$$i64$IoError" %ret_val

bb3:                                              ; preds = %bb1
  store i32 0, ptr %local_5, align 4
  %cmp_lhs = load i32, ptr %local_4, align 4
  %cmp_rhs = load i32, ptr %local_5, align 4
  %cmp_bit = icmp eq i32 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_6, align 1
  %cond_load = load i8, ptr %local_6, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb6, label %bb4

bb4:                                              ; preds = %bb3
  store i64 1, ptr %local_11, align 8
  %machine_tag_ptr = getelementptr inbounds nuw %"Result$$i64$IoError", ptr %local_10, i32 0, i32 0
  %move_iN_load_wide = load i64, ptr %local_11, align 8
  %move_iN_trunc = trunc i64 %move_iN_load_wide to i8
  store i8 %move_iN_trunc, ptr %machine_tag_ptr, align 1
  %call_result3 = call %IoError @"fs$last_io_error"()
  store %IoError %call_result3, ptr %local_12, align 8
  br label %bb7

bb5:                                              ; No predecessors!
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

bb6:                                              ; preds = %bb3
  store i64 0, ptr %local_8, align 8
  %machine_tag_ptr4 = getelementptr inbounds nuw %"Result$$i64$IoError", ptr %local_7, i32 0, i32 0
  %move_iN_load_wide5 = load i64, ptr %local_8, align 8
  %move_iN_trunc6 = trunc i64 %move_iN_load_wide5 to i8
  store i8 %move_iN_trunc6, ptr %machine_tag_ptr4, align 1
  store i64 0, ptr %local_9, align 8
  %machine_payload_ptr = getelementptr inbounds nuw %"Result$$i64$IoError", ptr %local_7, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr, i32 0, i32 0
  %move_load7 = load i64, ptr %local_9, align 8
  store i64 %move_load7, ptr %machine_variant_field_ptr, align 8
  %move_load8 = load %"Result$$i64$IoError", ptr %local_7, align 8
  store %"Result$$i64$IoError" %move_load8, ptr %local_2, align 8
  %hew_actor_cooperate9 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel10 = icmp eq i32 %hew_actor_cooperate9, 2
  br i1 %hew_cooperate_is_cancel10, label %cancel_exit11, label %after_cooperate12

bb7:                                              ; preds = %bb4
  %machine_payload_ptr13 = getelementptr inbounds nuw %"Result$$i64$IoError", ptr %local_10, i32 0, i32 1
  %machine_variant_field_ptr14 = getelementptr inbounds nuw { %IoError }, ptr %machine_payload_ptr13, i32 0, i32 0
  %move_load15 = load %IoError, ptr %local_12, align 8
  store %IoError %move_load15, ptr %machine_variant_field_ptr14, align 8
  %move_load16 = load %"Result$$i64$IoError", ptr %local_10, align 8
  store %"Result$$i64$IoError" %move_load16, ptr %local_2, align 8
  %hew_actor_cooperate17 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel18 = icmp eq i32 %hew_actor_cooperate17, 2
  br i1 %hew_cooperate_is_cancel18, label %cancel_exit19, label %after_cooperate20

cancel_exit:                                      ; preds = %entry
  ret %"Result$$i64$IoError" zeroinitializer

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit11:                                    ; preds = %bb6
  ret %"Result$$i64$IoError" zeroinitializer

after_cooperate12:                                ; preds = %bb6
  br label %bb2

cancel_exit19:                                    ; preds = %bb7
  ret %"Result$$i64$IoError" zeroinitializer

after_cooperate20:                                ; preds = %bb7
  br label %bb2
}

define internal i64 @"fs$append"(ptr %0, ptr %1) {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca i32, align 4
  %local_3 = alloca i64, align 8
  %local_4 = alloca i64, align 8
  store ptr %0, ptr %local_0, align 8
  store ptr %1, ptr %local_1, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_arg1 = load ptr, ptr %local_1, align 8
  %call_result = call i32 @hew_file_append(ptr %call_arg, ptr %call_arg1)
  store i32 %call_result, ptr %local_2, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %cast_int_src = load i32, ptr %local_2, align 4
  %cast_int_sext = sext i32 %cast_int_src to i64
  store i64 %cast_int_sext, ptr %local_3, align 8
  %move_load = load i64, ptr %local_3, align 8
  store i64 %move_load, ptr %local_4, align 8
  %move_load2 = load i64, ptr %local_4, align 8
  store i64 %move_load2, ptr %return_slot, align 8
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val

cancel_exit:                                      ; preds = %entry
  ret i64 0

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal %"Result$$i64$IoError" @"fs$try_append"(ptr %0, ptr %1) {
entry:
  %return_slot = alloca %"Result$$i64$IoError", align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca %"Result$$i64$IoError", align 8
  %local_3 = alloca i32, align 4
  %local_4 = alloca i32, align 4
  %local_5 = alloca i32, align 4
  %local_6 = alloca i8, align 1
  %local_7 = alloca %"Result$$i64$IoError", align 8
  %local_8 = alloca i64, align 8
  %local_9 = alloca i64, align 8
  %local_10 = alloca %"Result$$i64$IoError", align 8
  %local_11 = alloca i64, align 8
  %local_12 = alloca %IoError, align 8
  store ptr %0, ptr %local_0, align 8
  store ptr %1, ptr %local_1, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_arg1 = load ptr, ptr %local_1, align 8
  %call_result = call i32 @hew_file_append(ptr %call_arg, ptr %call_arg1)
  store i32 %call_result, ptr %local_3, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load i32, ptr %local_3, align 4
  store i32 %move_load, ptr %local_4, align 4
  br label %bb3

bb2:                                              ; preds = %after_cooperate20, %after_cooperate12
  %move_load2 = load %"Result$$i64$IoError", ptr %local_2, align 8
  store %"Result$$i64$IoError" %move_load2, ptr %return_slot, align 8
  %ret_val = load %"Result$$i64$IoError", ptr %return_slot, align 8
  ret %"Result$$i64$IoError" %ret_val

bb3:                                              ; preds = %bb1
  store i32 0, ptr %local_5, align 4
  %cmp_lhs = load i32, ptr %local_4, align 4
  %cmp_rhs = load i32, ptr %local_5, align 4
  %cmp_bit = icmp eq i32 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_6, align 1
  %cond_load = load i8, ptr %local_6, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb6, label %bb4

bb4:                                              ; preds = %bb3
  store i64 1, ptr %local_11, align 8
  %machine_tag_ptr = getelementptr inbounds nuw %"Result$$i64$IoError", ptr %local_10, i32 0, i32 0
  %move_iN_load_wide = load i64, ptr %local_11, align 8
  %move_iN_trunc = trunc i64 %move_iN_load_wide to i8
  store i8 %move_iN_trunc, ptr %machine_tag_ptr, align 1
  %call_result3 = call %IoError @"fs$last_io_error"()
  store %IoError %call_result3, ptr %local_12, align 8
  br label %bb7

bb5:                                              ; No predecessors!
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

bb6:                                              ; preds = %bb3
  store i64 0, ptr %local_8, align 8
  %machine_tag_ptr4 = getelementptr inbounds nuw %"Result$$i64$IoError", ptr %local_7, i32 0, i32 0
  %move_iN_load_wide5 = load i64, ptr %local_8, align 8
  %move_iN_trunc6 = trunc i64 %move_iN_load_wide5 to i8
  store i8 %move_iN_trunc6, ptr %machine_tag_ptr4, align 1
  store i64 0, ptr %local_9, align 8
  %machine_payload_ptr = getelementptr inbounds nuw %"Result$$i64$IoError", ptr %local_7, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr, i32 0, i32 0
  %move_load7 = load i64, ptr %local_9, align 8
  store i64 %move_load7, ptr %machine_variant_field_ptr, align 8
  %move_load8 = load %"Result$$i64$IoError", ptr %local_7, align 8
  store %"Result$$i64$IoError" %move_load8, ptr %local_2, align 8
  %hew_actor_cooperate9 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel10 = icmp eq i32 %hew_actor_cooperate9, 2
  br i1 %hew_cooperate_is_cancel10, label %cancel_exit11, label %after_cooperate12

bb7:                                              ; preds = %bb4
  %machine_payload_ptr13 = getelementptr inbounds nuw %"Result$$i64$IoError", ptr %local_10, i32 0, i32 1
  %machine_variant_field_ptr14 = getelementptr inbounds nuw { %IoError }, ptr %machine_payload_ptr13, i32 0, i32 0
  %move_load15 = load %IoError, ptr %local_12, align 8
  store %IoError %move_load15, ptr %machine_variant_field_ptr14, align 8
  %move_load16 = load %"Result$$i64$IoError", ptr %local_10, align 8
  store %"Result$$i64$IoError" %move_load16, ptr %local_2, align 8
  %hew_actor_cooperate17 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel18 = icmp eq i32 %hew_actor_cooperate17, 2
  br i1 %hew_cooperate_is_cancel18, label %cancel_exit19, label %after_cooperate20

cancel_exit:                                      ; preds = %entry
  ret %"Result$$i64$IoError" zeroinitializer

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit11:                                    ; preds = %bb6
  ret %"Result$$i64$IoError" zeroinitializer

after_cooperate12:                                ; preds = %bb6
  br label %bb2

cancel_exit19:                                    ; preds = %bb7
  ret %"Result$$i64$IoError" zeroinitializer

after_cooperate20:                                ; preds = %bb7
  br label %bb2
}

define internal i8 @"fs$exists"(ptr %0) {
entry:
  %return_slot = alloca i8, align 1
  %local_0 = alloca ptr, align 8
  %local_1 = alloca i8, align 1
  %local_2 = alloca i8, align 1
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result = call i8 @hew_file_exists(ptr %call_arg)
  store i8 %call_result, ptr %local_1, align 1
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load i8, ptr %local_1, align 1
  store i8 %move_load, ptr %local_2, align 1
  %move_load1 = load i8, ptr %local_2, align 1
  store i8 %move_load1, ptr %return_slot, align 1
  %ret_val = load i8, ptr %return_slot, align 1
  ret i8 %ret_val

cancel_exit:                                      ; preds = %entry
  ret i8 0

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal i64 @"fs$delete"(ptr %0) {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca i32, align 4
  %local_2 = alloca i64, align 8
  %local_3 = alloca i64, align 8
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result = call i32 @hew_file_delete(ptr %call_arg)
  store i32 %call_result, ptr %local_1, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %cast_int_src = load i32, ptr %local_1, align 4
  %cast_int_sext = sext i32 %cast_int_src to i64
  store i64 %cast_int_sext, ptr %local_2, align 8
  %move_load = load i64, ptr %local_2, align 8
  store i64 %move_load, ptr %local_3, align 8
  %move_load1 = load i64, ptr %local_3, align 8
  store i64 %move_load1, ptr %return_slot, align 8
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val

cancel_exit:                                      ; preds = %entry
  ret i64 0

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal %"Result$$i64$IoError" @"fs$try_delete"(ptr %0) {
entry:
  %return_slot = alloca %"Result$$i64$IoError", align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca %"Result$$i64$IoError", align 8
  %local_2 = alloca i32, align 4
  %local_3 = alloca i32, align 4
  %local_4 = alloca i32, align 4
  %local_5 = alloca i8, align 1
  %local_6 = alloca %"Result$$i64$IoError", align 8
  %local_7 = alloca i64, align 8
  %local_8 = alloca i64, align 8
  %local_9 = alloca %"Result$$i64$IoError", align 8
  %local_10 = alloca i64, align 8
  %local_11 = alloca %IoError, align 8
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result = call i32 @hew_file_delete(ptr %call_arg)
  store i32 %call_result, ptr %local_2, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load i32, ptr %local_2, align 4
  store i32 %move_load, ptr %local_3, align 4
  br label %bb3

bb2:                                              ; preds = %after_cooperate19, %after_cooperate11
  %move_load1 = load %"Result$$i64$IoError", ptr %local_1, align 8
  store %"Result$$i64$IoError" %move_load1, ptr %return_slot, align 8
  %ret_val = load %"Result$$i64$IoError", ptr %return_slot, align 8
  ret %"Result$$i64$IoError" %ret_val

bb3:                                              ; preds = %bb1
  store i32 0, ptr %local_4, align 4
  %cmp_lhs = load i32, ptr %local_3, align 4
  %cmp_rhs = load i32, ptr %local_4, align 4
  %cmp_bit = icmp eq i32 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_5, align 1
  %cond_load = load i8, ptr %local_5, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb6, label %bb4

bb4:                                              ; preds = %bb3
  store i64 1, ptr %local_10, align 8
  %machine_tag_ptr = getelementptr inbounds nuw %"Result$$i64$IoError", ptr %local_9, i32 0, i32 0
  %move_iN_load_wide = load i64, ptr %local_10, align 8
  %move_iN_trunc = trunc i64 %move_iN_load_wide to i8
  store i8 %move_iN_trunc, ptr %machine_tag_ptr, align 1
  %call_result2 = call %IoError @"fs$last_io_error"()
  store %IoError %call_result2, ptr %local_11, align 8
  br label %bb7

bb5:                                              ; No predecessors!
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

bb6:                                              ; preds = %bb3
  store i64 0, ptr %local_7, align 8
  %machine_tag_ptr3 = getelementptr inbounds nuw %"Result$$i64$IoError", ptr %local_6, i32 0, i32 0
  %move_iN_load_wide4 = load i64, ptr %local_7, align 8
  %move_iN_trunc5 = trunc i64 %move_iN_load_wide4 to i8
  store i8 %move_iN_trunc5, ptr %machine_tag_ptr3, align 1
  store i64 0, ptr %local_8, align 8
  %machine_payload_ptr = getelementptr inbounds nuw %"Result$$i64$IoError", ptr %local_6, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr, i32 0, i32 0
  %move_load6 = load i64, ptr %local_8, align 8
  store i64 %move_load6, ptr %machine_variant_field_ptr, align 8
  %move_load7 = load %"Result$$i64$IoError", ptr %local_6, align 8
  store %"Result$$i64$IoError" %move_load7, ptr %local_1, align 8
  %hew_actor_cooperate8 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel9 = icmp eq i32 %hew_actor_cooperate8, 2
  br i1 %hew_cooperate_is_cancel9, label %cancel_exit10, label %after_cooperate11

bb7:                                              ; preds = %bb4
  %machine_payload_ptr12 = getelementptr inbounds nuw %"Result$$i64$IoError", ptr %local_9, i32 0, i32 1
  %machine_variant_field_ptr13 = getelementptr inbounds nuw { %IoError }, ptr %machine_payload_ptr12, i32 0, i32 0
  %move_load14 = load %IoError, ptr %local_11, align 8
  store %IoError %move_load14, ptr %machine_variant_field_ptr13, align 8
  %move_load15 = load %"Result$$i64$IoError", ptr %local_9, align 8
  store %"Result$$i64$IoError" %move_load15, ptr %local_1, align 8
  %hew_actor_cooperate16 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel17 = icmp eq i32 %hew_actor_cooperate16, 2
  br i1 %hew_cooperate_is_cancel17, label %cancel_exit18, label %after_cooperate19

cancel_exit:                                      ; preds = %entry
  ret %"Result$$i64$IoError" zeroinitializer

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit10:                                    ; preds = %bb6
  ret %"Result$$i64$IoError" zeroinitializer

after_cooperate11:                                ; preds = %bb6
  br label %bb2

cancel_exit18:                                    ; preds = %bb7
  ret %"Result$$i64$IoError" zeroinitializer

after_cooperate19:                                ; preds = %bb7
  br label %bb2
}

define internal i64 @"fs$size"(ptr %0) {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result = call i64 @hew_file_size(ptr %call_arg)
  store i64 %call_result, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load i64, ptr %local_1, align 8
  store i64 %move_load, ptr %local_2, align 8
  %move_load1 = load i64, ptr %local_2, align 8
  store i64 %move_load1, ptr %return_slot, align 8
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val

cancel_exit:                                      ; preds = %entry
  ret i64 0

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal { ptr, i32, i32 } @"fs$read_bytes"(ptr %0) {
entry:
  %return_slot = alloca { ptr, i32, i32 }, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca { ptr, i32, i32 }, align 8
  %local_2 = alloca { ptr, i32, i32 }, align 8
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result = call [2 x i64] @hew_file_read_bytes(ptr %call_arg)
  store [2 x i64] %call_result, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load { ptr, i32, i32 }, ptr %local_1, align 8
  store { ptr, i32, i32 } %move_load, ptr %local_2, align 8
  %move_load1 = load { ptr, i32, i32 }, ptr %local_2, align 8
  store { ptr, i32, i32 } %move_load1, ptr %return_slot, align 8
  %ret_val = load { ptr, i32, i32 }, ptr %return_slot, align 8
  ret { ptr, i32, i32 } %ret_val

cancel_exit:                                      ; preds = %entry
  ret { ptr, i32, i32 } zeroinitializer

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal %"Result$$bytes$IoError" @"fs$try_read_bytes"(ptr %0) {
entry:
  %return_slot = alloca %"Result$$bytes$IoError", align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca { ptr, i32, i32 }, align 8
  %local_2 = alloca { ptr, i32, i32 }, align 8
  %local_3 = alloca i32, align 4
  %local_4 = alloca i64, align 8
  %local_5 = alloca i64, align 8
  %local_6 = alloca i32, align 4
  %local_7 = alloca i32, align 4
  %local_8 = alloca %"Result$$bytes$IoError", align 8
  %local_9 = alloca i32, align 4
  %local_10 = alloca i8, align 1
  %local_11 = alloca ptr, align 8
  %local_12 = alloca %"Result$$bytes$IoError", align 8
  %local_13 = alloca i64, align 8
  %local_14 = alloca i64, align 8
  %local_15 = alloca %IoError, align 8
  %local_16 = alloca %"Result$$bytes$IoError", align 8
  %local_17 = alloca %"Result$$bytes$IoError", align 8
  %local_18 = alloca i64, align 8
  %local_19 = alloca %"Result$$bytes$IoError", align 8
  %local_20 = alloca %"Result$$bytes$IoError", align 8
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result = call [2 x i64] @hew_file_read_bytes(ptr %call_arg)
  store [2 x i64] %call_result, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load { ptr, i32, i32 }, ptr %local_1, align 8
  store { ptr, i32, i32 } %move_load, ptr %local_2, align 8
  %call_result1 = call i32 @hew_stream_last_error_kind()
  store i32 %call_result1, ptr %local_3, align 4
  br label %bb2

bb2:                                              ; preds = %bb1
  %cast_int_src = load i32, ptr %local_3, align 4
  %cast_int_sext = sext i32 %cast_int_src to i64
  store i64 %cast_int_sext, ptr %local_4, align 8
  %move_load2 = load i64, ptr %local_4, align 8
  store i64 %move_load2, ptr %local_5, align 8
  %call_result3 = call i32 @hew_stream_last_errno()
  store i32 %call_result3, ptr %local_6, align 4
  br label %bb3

bb3:                                              ; preds = %bb2
  %move_load4 = load i32, ptr %local_6, align 4
  store i32 %move_load4, ptr %local_7, align 4
  store i32 0, ptr %local_9, align 4
  %cmp_lhs = load i32, ptr %local_7, align 4
  %cmp_rhs = load i32, ptr %local_9, align 4
  %cmp_bit = icmp ne i32 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_10, align 1
  %cond_load = load i8, ptr %local_10, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb4, label %bb5

bb4:                                              ; preds = %bb3
  %call_result5 = call ptr @hew_stream_last_error()
  store ptr %call_result5, ptr %local_11, align 8
  br label %bb7

bb5:                                              ; preds = %bb3
  store i64 0, ptr %local_18, align 8
  %machine_tag_ptr = getelementptr inbounds nuw %"Result$$bytes$IoError", ptr %local_17, i32 0, i32 0
  %move_iN_load_wide = load i64, ptr %local_18, align 8
  %move_iN_trunc = trunc i64 %move_iN_load_wide to i8
  store i8 %move_iN_trunc, ptr %machine_tag_ptr, align 1
  %machine_payload_ptr = getelementptr inbounds nuw %"Result$$bytes$IoError", ptr %local_17, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { { ptr, i32, i32 } }, ptr %machine_payload_ptr, i32 0, i32 0
  %move_load6 = load { ptr, i32, i32 }, ptr %local_2, align 8
  store { ptr, i32, i32 } %move_load6, ptr %machine_variant_field_ptr, align 8
  %move_load7 = load %"Result$$bytes$IoError", ptr %local_17, align 8
  store %"Result$$bytes$IoError" %move_load7, ptr %local_19, align 8
  %move_load8 = load %"Result$$bytes$IoError", ptr %local_19, align 8
  store %"Result$$bytes$IoError" %move_load8, ptr %local_8, align 8
  br label %bb6

bb6:                                              ; preds = %after_cooperate27, %bb5
  %move_load9 = load %"Result$$bytes$IoError", ptr %local_8, align 8
  store %"Result$$bytes$IoError" %move_load9, ptr %local_20, align 8
  %move_load10 = load %"Result$$bytes$IoError", ptr %local_20, align 8
  store %"Result$$bytes$IoError" %move_load10, ptr %return_slot, align 8
  %ret_val = load %"Result$$bytes$IoError", ptr %return_slot, align 8
  ret %"Result$$bytes$IoError" %ret_val

bb7:                                              ; preds = %bb4
  %"hew_string_drop drop" = load ptr, ptr %local_11, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop")
  store ptr null, ptr %local_11, align 8
  store i64 1, ptr %local_13, align 8
  %machine_tag_ptr11 = getelementptr inbounds nuw %"Result$$bytes$IoError", ptr %local_12, i32 0, i32 0
  %move_iN_load_wide12 = load i64, ptr %local_13, align 8
  %move_iN_trunc13 = trunc i64 %move_iN_load_wide12 to i8
  store i8 %move_iN_trunc13, ptr %machine_tag_ptr11, align 1
  %cast_int_src14 = load i32, ptr %local_7, align 4
  %cast_int_sext15 = sext i32 %cast_int_src14 to i64
  store i64 %cast_int_sext15, ptr %local_14, align 8
  %call_arg16 = load i64, ptr %local_5, align 8
  %call_arg17 = load i64, ptr %local_14, align 8
  %call_result18 = call %IoError @"fs$io_error_from_kind"(i64 %call_arg16, i64 %call_arg17)
  store %IoError %call_result18, ptr %local_15, align 8
  br label %bb8

bb8:                                              ; preds = %bb7
  %machine_payload_ptr19 = getelementptr inbounds nuw %"Result$$bytes$IoError", ptr %local_12, i32 0, i32 1
  %machine_variant_field_ptr20 = getelementptr inbounds nuw { %IoError }, ptr %machine_payload_ptr19, i32 0, i32 0
  %move_load21 = load %IoError, ptr %local_15, align 8
  store %IoError %move_load21, ptr %machine_variant_field_ptr20, align 8
  %move_load22 = load %"Result$$bytes$IoError", ptr %local_12, align 8
  store %"Result$$bytes$IoError" %move_load22, ptr %local_16, align 8
  %move_load23 = load %"Result$$bytes$IoError", ptr %local_16, align 8
  store %"Result$$bytes$IoError" %move_load23, ptr %local_8, align 8
  %hew_actor_cooperate24 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel25 = icmp eq i32 %hew_actor_cooperate24, 2
  br i1 %hew_cooperate_is_cancel25, label %cancel_exit26, label %after_cooperate27

cancel_exit:                                      ; preds = %entry
  ret %"Result$$bytes$IoError" zeroinitializer

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit26:                                    ; preds = %bb8
  ret %"Result$$bytes$IoError" zeroinitializer

after_cooperate27:                                ; preds = %bb8
  br label %bb6
}

define internal i64 @"fs$write_bytes"(ptr %0, ptr %1) {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca { ptr, i32, i32 }, align 8
  %local_2 = alloca i32, align 4
  %local_3 = alloca i64, align 8
  %local_4 = alloca i64, align 8
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result = call i32 @hew_file_write_bytes(ptr %call_arg, ptr %1)
  store i32 %call_result, ptr %local_2, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %cast_int_src = load i32, ptr %local_2, align 4
  %cast_int_sext = sext i32 %cast_int_src to i64
  store i64 %cast_int_sext, ptr %local_3, align 8
  %move_load = load i64, ptr %local_3, align 8
  store i64 %move_load, ptr %local_4, align 8
  %move_load1 = load i64, ptr %local_4, align 8
  store i64 %move_load1, ptr %return_slot, align 8
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val

cancel_exit:                                      ; preds = %entry
  ret i64 0

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal %"Result$$i64$IoError" @"fs$try_write_bytes"(ptr %0, ptr %1) {
entry:
  %return_slot = alloca %"Result$$i64$IoError", align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca { ptr, i32, i32 }, align 8
  %local_2 = alloca %"Result$$i64$IoError", align 8
  %local_3 = alloca i32, align 4
  %local_4 = alloca i32, align 4
  %local_5 = alloca i32, align 4
  %local_6 = alloca i8, align 1
  %local_7 = alloca %"Result$$i64$IoError", align 8
  %local_8 = alloca i64, align 8
  %local_9 = alloca i64, align 8
  %local_10 = alloca %"Result$$i64$IoError", align 8
  %local_11 = alloca i64, align 8
  %local_12 = alloca %IoError, align 8
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result = call i32 @hew_file_write_bytes(ptr %call_arg, ptr %1)
  store i32 %call_result, ptr %local_3, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load i32, ptr %local_3, align 4
  store i32 %move_load, ptr %local_4, align 4
  br label %bb3

bb2:                                              ; preds = %after_cooperate19, %after_cooperate11
  %move_load1 = load %"Result$$i64$IoError", ptr %local_2, align 8
  store %"Result$$i64$IoError" %move_load1, ptr %return_slot, align 8
  %ret_val = load %"Result$$i64$IoError", ptr %return_slot, align 8
  ret %"Result$$i64$IoError" %ret_val

bb3:                                              ; preds = %bb1
  store i32 0, ptr %local_5, align 4
  %cmp_lhs = load i32, ptr %local_4, align 4
  %cmp_rhs = load i32, ptr %local_5, align 4
  %cmp_bit = icmp eq i32 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_6, align 1
  %cond_load = load i8, ptr %local_6, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb6, label %bb4

bb4:                                              ; preds = %bb3
  store i64 1, ptr %local_11, align 8
  %machine_tag_ptr = getelementptr inbounds nuw %"Result$$i64$IoError", ptr %local_10, i32 0, i32 0
  %move_iN_load_wide = load i64, ptr %local_11, align 8
  %move_iN_trunc = trunc i64 %move_iN_load_wide to i8
  store i8 %move_iN_trunc, ptr %machine_tag_ptr, align 1
  %call_result2 = call %IoError @"fs$last_io_error"()
  store %IoError %call_result2, ptr %local_12, align 8
  br label %bb7

bb5:                                              ; No predecessors!
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

bb6:                                              ; preds = %bb3
  store i64 0, ptr %local_8, align 8
  %machine_tag_ptr3 = getelementptr inbounds nuw %"Result$$i64$IoError", ptr %local_7, i32 0, i32 0
  %move_iN_load_wide4 = load i64, ptr %local_8, align 8
  %move_iN_trunc5 = trunc i64 %move_iN_load_wide4 to i8
  store i8 %move_iN_trunc5, ptr %machine_tag_ptr3, align 1
  store i64 0, ptr %local_9, align 8
  %machine_payload_ptr = getelementptr inbounds nuw %"Result$$i64$IoError", ptr %local_7, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr, i32 0, i32 0
  %move_load6 = load i64, ptr %local_9, align 8
  store i64 %move_load6, ptr %machine_variant_field_ptr, align 8
  %move_load7 = load %"Result$$i64$IoError", ptr %local_7, align 8
  store %"Result$$i64$IoError" %move_load7, ptr %local_2, align 8
  %hew_actor_cooperate8 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel9 = icmp eq i32 %hew_actor_cooperate8, 2
  br i1 %hew_cooperate_is_cancel9, label %cancel_exit10, label %after_cooperate11

bb7:                                              ; preds = %bb4
  %machine_payload_ptr12 = getelementptr inbounds nuw %"Result$$i64$IoError", ptr %local_10, i32 0, i32 1
  %machine_variant_field_ptr13 = getelementptr inbounds nuw { %IoError }, ptr %machine_payload_ptr12, i32 0, i32 0
  %move_load14 = load %IoError, ptr %local_12, align 8
  store %IoError %move_load14, ptr %machine_variant_field_ptr13, align 8
  %move_load15 = load %"Result$$i64$IoError", ptr %local_10, align 8
  store %"Result$$i64$IoError" %move_load15, ptr %local_2, align 8
  %hew_actor_cooperate16 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel17 = icmp eq i32 %hew_actor_cooperate16, 2
  br i1 %hew_cooperate_is_cancel17, label %cancel_exit18, label %after_cooperate19

cancel_exit:                                      ; preds = %entry
  ret %"Result$$i64$IoError" zeroinitializer

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit10:                                    ; preds = %bb6
  ret %"Result$$i64$IoError" zeroinitializer

after_cooperate11:                                ; preds = %bb6
  br label %bb2

cancel_exit18:                                    ; preds = %bb7
  ret %"Result$$i64$IoError" zeroinitializer

after_cooperate19:                                ; preds = %bb7
  br label %bb2
}

define internal i64 @"fs$mkdir"(ptr %0) {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca i32, align 4
  %local_2 = alloca i64, align 8
  %local_3 = alloca i64, align 8
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result = call i32 @hew_fs_mkdir(ptr %call_arg)
  store i32 %call_result, ptr %local_1, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %cast_int_src = load i32, ptr %local_1, align 4
  %cast_int_sext = sext i32 %cast_int_src to i64
  store i64 %cast_int_sext, ptr %local_2, align 8
  %move_load = load i64, ptr %local_2, align 8
  store i64 %move_load, ptr %local_3, align 8
  %move_load1 = load i64, ptr %local_3, align 8
  store i64 %move_load1, ptr %return_slot, align 8
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val

cancel_exit:                                      ; preds = %entry
  ret i64 0

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal %"Result$$i64$IoError" @"fs$try_mkdir"(ptr %0) {
entry:
  %return_slot = alloca %"Result$$i64$IoError", align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca %"Result$$i64$IoError", align 8
  %local_2 = alloca i32, align 4
  %local_3 = alloca i32, align 4
  %local_4 = alloca i32, align 4
  %local_5 = alloca i8, align 1
  %local_6 = alloca %"Result$$i64$IoError", align 8
  %local_7 = alloca i64, align 8
  %local_8 = alloca i64, align 8
  %local_9 = alloca %"Result$$i64$IoError", align 8
  %local_10 = alloca i64, align 8
  %local_11 = alloca %IoError, align 8
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result = call i32 @hew_fs_mkdir(ptr %call_arg)
  store i32 %call_result, ptr %local_2, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load i32, ptr %local_2, align 4
  store i32 %move_load, ptr %local_3, align 4
  br label %bb3

bb2:                                              ; preds = %after_cooperate19, %after_cooperate11
  %move_load1 = load %"Result$$i64$IoError", ptr %local_1, align 8
  store %"Result$$i64$IoError" %move_load1, ptr %return_slot, align 8
  %ret_val = load %"Result$$i64$IoError", ptr %return_slot, align 8
  ret %"Result$$i64$IoError" %ret_val

bb3:                                              ; preds = %bb1
  store i32 0, ptr %local_4, align 4
  %cmp_lhs = load i32, ptr %local_3, align 4
  %cmp_rhs = load i32, ptr %local_4, align 4
  %cmp_bit = icmp eq i32 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_5, align 1
  %cond_load = load i8, ptr %local_5, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb6, label %bb4

bb4:                                              ; preds = %bb3
  store i64 1, ptr %local_10, align 8
  %machine_tag_ptr = getelementptr inbounds nuw %"Result$$i64$IoError", ptr %local_9, i32 0, i32 0
  %move_iN_load_wide = load i64, ptr %local_10, align 8
  %move_iN_trunc = trunc i64 %move_iN_load_wide to i8
  store i8 %move_iN_trunc, ptr %machine_tag_ptr, align 1
  %call_result2 = call %IoError @"fs$last_io_error"()
  store %IoError %call_result2, ptr %local_11, align 8
  br label %bb7

bb5:                                              ; No predecessors!
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

bb6:                                              ; preds = %bb3
  store i64 0, ptr %local_7, align 8
  %machine_tag_ptr3 = getelementptr inbounds nuw %"Result$$i64$IoError", ptr %local_6, i32 0, i32 0
  %move_iN_load_wide4 = load i64, ptr %local_7, align 8
  %move_iN_trunc5 = trunc i64 %move_iN_load_wide4 to i8
  store i8 %move_iN_trunc5, ptr %machine_tag_ptr3, align 1
  store i64 0, ptr %local_8, align 8
  %machine_payload_ptr = getelementptr inbounds nuw %"Result$$i64$IoError", ptr %local_6, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr, i32 0, i32 0
  %move_load6 = load i64, ptr %local_8, align 8
  store i64 %move_load6, ptr %machine_variant_field_ptr, align 8
  %move_load7 = load %"Result$$i64$IoError", ptr %local_6, align 8
  store %"Result$$i64$IoError" %move_load7, ptr %local_1, align 8
  %hew_actor_cooperate8 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel9 = icmp eq i32 %hew_actor_cooperate8, 2
  br i1 %hew_cooperate_is_cancel9, label %cancel_exit10, label %after_cooperate11

bb7:                                              ; preds = %bb4
  %machine_payload_ptr12 = getelementptr inbounds nuw %"Result$$i64$IoError", ptr %local_9, i32 0, i32 1
  %machine_variant_field_ptr13 = getelementptr inbounds nuw { %IoError }, ptr %machine_payload_ptr12, i32 0, i32 0
  %move_load14 = load %IoError, ptr %local_11, align 8
  store %IoError %move_load14, ptr %machine_variant_field_ptr13, align 8
  %move_load15 = load %"Result$$i64$IoError", ptr %local_9, align 8
  store %"Result$$i64$IoError" %move_load15, ptr %local_1, align 8
  %hew_actor_cooperate16 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel17 = icmp eq i32 %hew_actor_cooperate16, 2
  br i1 %hew_cooperate_is_cancel17, label %cancel_exit18, label %after_cooperate19

cancel_exit:                                      ; preds = %entry
  ret %"Result$$i64$IoError" zeroinitializer

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit10:                                    ; preds = %bb6
  ret %"Result$$i64$IoError" zeroinitializer

after_cooperate11:                                ; preds = %bb6
  br label %bb2

cancel_exit18:                                    ; preds = %bb7
  ret %"Result$$i64$IoError" zeroinitializer

after_cooperate19:                                ; preds = %bb7
  br label %bb2
}

define internal i64 @"fs$mkdir_all"(ptr %0) {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca i32, align 4
  %local_2 = alloca i64, align 8
  %local_3 = alloca i64, align 8
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result = call i32 @hew_fs_mkdir_all(ptr %call_arg)
  store i32 %call_result, ptr %local_1, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %cast_int_src = load i32, ptr %local_1, align 4
  %cast_int_sext = sext i32 %cast_int_src to i64
  store i64 %cast_int_sext, ptr %local_2, align 8
  %move_load = load i64, ptr %local_2, align 8
  store i64 %move_load, ptr %local_3, align 8
  %move_load1 = load i64, ptr %local_3, align 8
  store i64 %move_load1, ptr %return_slot, align 8
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val

cancel_exit:                                      ; preds = %entry
  ret i64 0

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal %"Result$$i64$IoError" @"fs$try_mkdir_all"(ptr %0) {
entry:
  %return_slot = alloca %"Result$$i64$IoError", align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca %"Result$$i64$IoError", align 8
  %local_2 = alloca i32, align 4
  %local_3 = alloca i32, align 4
  %local_4 = alloca i32, align 4
  %local_5 = alloca i8, align 1
  %local_6 = alloca %"Result$$i64$IoError", align 8
  %local_7 = alloca i64, align 8
  %local_8 = alloca i64, align 8
  %local_9 = alloca %"Result$$i64$IoError", align 8
  %local_10 = alloca i64, align 8
  %local_11 = alloca %IoError, align 8
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result = call i32 @hew_fs_mkdir_all(ptr %call_arg)
  store i32 %call_result, ptr %local_2, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load i32, ptr %local_2, align 4
  store i32 %move_load, ptr %local_3, align 4
  br label %bb3

bb2:                                              ; preds = %after_cooperate19, %after_cooperate11
  %move_load1 = load %"Result$$i64$IoError", ptr %local_1, align 8
  store %"Result$$i64$IoError" %move_load1, ptr %return_slot, align 8
  %ret_val = load %"Result$$i64$IoError", ptr %return_slot, align 8
  ret %"Result$$i64$IoError" %ret_val

bb3:                                              ; preds = %bb1
  store i32 0, ptr %local_4, align 4
  %cmp_lhs = load i32, ptr %local_3, align 4
  %cmp_rhs = load i32, ptr %local_4, align 4
  %cmp_bit = icmp eq i32 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_5, align 1
  %cond_load = load i8, ptr %local_5, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb6, label %bb4

bb4:                                              ; preds = %bb3
  store i64 1, ptr %local_10, align 8
  %machine_tag_ptr = getelementptr inbounds nuw %"Result$$i64$IoError", ptr %local_9, i32 0, i32 0
  %move_iN_load_wide = load i64, ptr %local_10, align 8
  %move_iN_trunc = trunc i64 %move_iN_load_wide to i8
  store i8 %move_iN_trunc, ptr %machine_tag_ptr, align 1
  %call_result2 = call %IoError @"fs$last_io_error"()
  store %IoError %call_result2, ptr %local_11, align 8
  br label %bb7

bb5:                                              ; No predecessors!
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

bb6:                                              ; preds = %bb3
  store i64 0, ptr %local_7, align 8
  %machine_tag_ptr3 = getelementptr inbounds nuw %"Result$$i64$IoError", ptr %local_6, i32 0, i32 0
  %move_iN_load_wide4 = load i64, ptr %local_7, align 8
  %move_iN_trunc5 = trunc i64 %move_iN_load_wide4 to i8
  store i8 %move_iN_trunc5, ptr %machine_tag_ptr3, align 1
  store i64 0, ptr %local_8, align 8
  %machine_payload_ptr = getelementptr inbounds nuw %"Result$$i64$IoError", ptr %local_6, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr, i32 0, i32 0
  %move_load6 = load i64, ptr %local_8, align 8
  store i64 %move_load6, ptr %machine_variant_field_ptr, align 8
  %move_load7 = load %"Result$$i64$IoError", ptr %local_6, align 8
  store %"Result$$i64$IoError" %move_load7, ptr %local_1, align 8
  %hew_actor_cooperate8 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel9 = icmp eq i32 %hew_actor_cooperate8, 2
  br i1 %hew_cooperate_is_cancel9, label %cancel_exit10, label %after_cooperate11

bb7:                                              ; preds = %bb4
  %machine_payload_ptr12 = getelementptr inbounds nuw %"Result$$i64$IoError", ptr %local_9, i32 0, i32 1
  %machine_variant_field_ptr13 = getelementptr inbounds nuw { %IoError }, ptr %machine_payload_ptr12, i32 0, i32 0
  %move_load14 = load %IoError, ptr %local_11, align 8
  store %IoError %move_load14, ptr %machine_variant_field_ptr13, align 8
  %move_load15 = load %"Result$$i64$IoError", ptr %local_9, align 8
  store %"Result$$i64$IoError" %move_load15, ptr %local_1, align 8
  %hew_actor_cooperate16 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel17 = icmp eq i32 %hew_actor_cooperate16, 2
  br i1 %hew_cooperate_is_cancel17, label %cancel_exit18, label %after_cooperate19

cancel_exit:                                      ; preds = %entry
  ret %"Result$$i64$IoError" zeroinitializer

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit10:                                    ; preds = %bb6
  ret %"Result$$i64$IoError" zeroinitializer

after_cooperate11:                                ; preds = %bb6
  br label %bb2

cancel_exit18:                                    ; preds = %bb7
  ret %"Result$$i64$IoError" zeroinitializer

after_cooperate19:                                ; preds = %bb7
  br label %bb2
}

define internal ptr @"fs$list_dir"(ptr %0) {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca ptr, align 8
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result = call ptr @hew_fs_list_dir(ptr %call_arg)
  store ptr %call_result, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %local_2, align 8
  %move_load1 = load ptr, ptr %local_2, align 8
  store ptr %move_load1, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal %"Result$$Vec$lstring$g$IoError" @"fs$try_list_dir"(ptr %0) {
entry:
  %return_slot = alloca %"Result$$Vec$lstring$g$IoError", align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca ptr, align 8
  %local_3 = alloca i32, align 4
  %local_4 = alloca i64, align 8
  %local_5 = alloca i64, align 8
  %local_6 = alloca i32, align 4
  %local_7 = alloca i32, align 4
  %local_8 = alloca ptr, align 8
  %local_9 = alloca %"Result$$Vec$lstring$g$IoError", align 8
  %local_10 = alloca i32, align 4
  %local_11 = alloca i8, align 1
  %local_12 = alloca %"Result$$Vec$lstring$g$IoError", align 8
  %local_13 = alloca i64, align 8
  %local_14 = alloca %"Result$$Vec$lstring$g$IoError", align 8
  %local_15 = alloca %"Result$$Vec$lstring$g$IoError", align 8
  %local_16 = alloca i64, align 8
  %local_17 = alloca i64, align 8
  %local_18 = alloca %IoError, align 8
  %local_19 = alloca %"Result$$Vec$lstring$g$IoError", align 8
  %local_20 = alloca %"Result$$Vec$lstring$g$IoError", align 8
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result = call ptr @hew_fs_list_dir(ptr %call_arg)
  store ptr %call_result, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %local_2, align 8
  %call_result1 = call i32 @hew_stream_last_error_kind()
  store i32 %call_result1, ptr %local_3, align 4
  br label %bb2

bb2:                                              ; preds = %bb1
  %cast_int_src = load i32, ptr %local_3, align 4
  %cast_int_sext = sext i32 %cast_int_src to i64
  store i64 %cast_int_sext, ptr %local_4, align 8
  %move_load2 = load i64, ptr %local_4, align 8
  store i64 %move_load2, ptr %local_5, align 8
  %call_result3 = call i32 @hew_stream_last_errno()
  store i32 %call_result3, ptr %local_6, align 4
  br label %bb3

bb3:                                              ; preds = %bb2
  %move_load4 = load i32, ptr %local_6, align 4
  store i32 %move_load4, ptr %local_7, align 4
  %call_result5 = call ptr @hew_stream_last_error()
  store ptr %call_result5, ptr %local_8, align 8
  br label %bb4

bb4:                                              ; preds = %bb3
  %"hew_string_drop drop" = load ptr, ptr %local_8, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop")
  store ptr null, ptr %local_8, align 8
  store i32 0, ptr %local_10, align 4
  %cmp_lhs = load i32, ptr %local_7, align 4
  %cmp_rhs = load i32, ptr %local_10, align 4
  %cmp_bit = icmp eq i32 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_11, align 1
  %cond_load = load i8, ptr %local_11, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb5, label %bb6

bb5:                                              ; preds = %bb4
  store i64 0, ptr %local_13, align 8
  %machine_tag_ptr = getelementptr inbounds nuw %"Result$$Vec$lstring$g$IoError", ptr %local_12, i32 0, i32 0
  %move_iN_load_wide = load i64, ptr %local_13, align 8
  %move_iN_trunc = trunc i64 %move_iN_load_wide to i8
  store i8 %move_iN_trunc, ptr %machine_tag_ptr, align 1
  %machine_payload_ptr = getelementptr inbounds nuw %"Result$$Vec$lstring$g$IoError", ptr %local_12, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { ptr }, ptr %machine_payload_ptr, i32 0, i32 0
  %move_load6 = load ptr, ptr %local_2, align 8
  store ptr %move_load6, ptr %machine_variant_field_ptr, align 8
  %move_load7 = load %"Result$$Vec$lstring$g$IoError", ptr %local_12, align 8
  store %"Result$$Vec$lstring$g$IoError" %move_load7, ptr %local_14, align 8
  %move_load8 = load %"Result$$Vec$lstring$g$IoError", ptr %local_14, align 8
  store %"Result$$Vec$lstring$g$IoError" %move_load8, ptr %local_9, align 8
  br label %bb7

bb6:                                              ; preds = %bb4
  store i64 1, ptr %local_16, align 8
  %machine_tag_ptr9 = getelementptr inbounds nuw %"Result$$Vec$lstring$g$IoError", ptr %local_15, i32 0, i32 0
  %move_iN_load_wide10 = load i64, ptr %local_16, align 8
  %move_iN_trunc11 = trunc i64 %move_iN_load_wide10 to i8
  store i8 %move_iN_trunc11, ptr %machine_tag_ptr9, align 1
  %cast_int_src12 = load i32, ptr %local_7, align 4
  %cast_int_sext13 = sext i32 %cast_int_src12 to i64
  store i64 %cast_int_sext13, ptr %local_17, align 8
  %call_arg14 = load i64, ptr %local_5, align 8
  %call_arg15 = load i64, ptr %local_17, align 8
  %call_result16 = call %IoError @"fs$io_error_from_kind"(i64 %call_arg14, i64 %call_arg15)
  store %IoError %call_result16, ptr %local_18, align 8
  br label %bb8

bb7:                                              ; preds = %after_cooperate27, %bb5
  %move_load17 = load %"Result$$Vec$lstring$g$IoError", ptr %local_9, align 8
  store %"Result$$Vec$lstring$g$IoError" %move_load17, ptr %local_20, align 8
  %move_load18 = load %"Result$$Vec$lstring$g$IoError", ptr %local_20, align 8
  store %"Result$$Vec$lstring$g$IoError" %move_load18, ptr %return_slot, align 8
  %ret_val = load %"Result$$Vec$lstring$g$IoError", ptr %return_slot, align 8
  ret %"Result$$Vec$lstring$g$IoError" %ret_val

bb8:                                              ; preds = %bb6
  %machine_payload_ptr19 = getelementptr inbounds nuw %"Result$$Vec$lstring$g$IoError", ptr %local_15, i32 0, i32 1
  %machine_variant_field_ptr20 = getelementptr inbounds nuw { %IoError }, ptr %machine_payload_ptr19, i32 0, i32 0
  %move_load21 = load %IoError, ptr %local_18, align 8
  store %IoError %move_load21, ptr %machine_variant_field_ptr20, align 8
  %move_load22 = load %"Result$$Vec$lstring$g$IoError", ptr %local_15, align 8
  store %"Result$$Vec$lstring$g$IoError" %move_load22, ptr %local_19, align 8
  %move_load23 = load %"Result$$Vec$lstring$g$IoError", ptr %local_19, align 8
  store %"Result$$Vec$lstring$g$IoError" %move_load23, ptr %local_9, align 8
  %hew_actor_cooperate24 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel25 = icmp eq i32 %hew_actor_cooperate24, 2
  br i1 %hew_cooperate_is_cancel25, label %cancel_exit26, label %after_cooperate27

cancel_exit:                                      ; preds = %entry
  ret %"Result$$Vec$lstring$g$IoError" zeroinitializer

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit26:                                    ; preds = %bb8
  ret %"Result$$Vec$lstring$g$IoError" zeroinitializer

after_cooperate27:                                ; preds = %bb8
  br label %bb7
}

define internal i64 @"fs$rename"(ptr %0, ptr %1) {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca i32, align 4
  %local_3 = alloca i64, align 8
  %local_4 = alloca i64, align 8
  store ptr %0, ptr %local_0, align 8
  store ptr %1, ptr %local_1, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_arg1 = load ptr, ptr %local_1, align 8
  %call_result = call i32 @hew_fs_rename(ptr %call_arg, ptr %call_arg1)
  store i32 %call_result, ptr %local_2, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %cast_int_src = load i32, ptr %local_2, align 4
  %cast_int_sext = sext i32 %cast_int_src to i64
  store i64 %cast_int_sext, ptr %local_3, align 8
  %move_load = load i64, ptr %local_3, align 8
  store i64 %move_load, ptr %local_4, align 8
  %move_load2 = load i64, ptr %local_4, align 8
  store i64 %move_load2, ptr %return_slot, align 8
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val

cancel_exit:                                      ; preds = %entry
  ret i64 0

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal %"Result$$i64$IoError" @"fs$try_rename"(ptr %0, ptr %1) {
entry:
  %return_slot = alloca %"Result$$i64$IoError", align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca %"Result$$i64$IoError", align 8
  %local_3 = alloca i32, align 4
  %local_4 = alloca i32, align 4
  %local_5 = alloca i32, align 4
  %local_6 = alloca i8, align 1
  %local_7 = alloca %"Result$$i64$IoError", align 8
  %local_8 = alloca i64, align 8
  %local_9 = alloca i64, align 8
  %local_10 = alloca %"Result$$i64$IoError", align 8
  %local_11 = alloca i64, align 8
  %local_12 = alloca %IoError, align 8
  store ptr %0, ptr %local_0, align 8
  store ptr %1, ptr %local_1, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_arg1 = load ptr, ptr %local_1, align 8
  %call_result = call i32 @hew_fs_rename(ptr %call_arg, ptr %call_arg1)
  store i32 %call_result, ptr %local_3, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load i32, ptr %local_3, align 4
  store i32 %move_load, ptr %local_4, align 4
  br label %bb3

bb2:                                              ; preds = %after_cooperate20, %after_cooperate12
  %move_load2 = load %"Result$$i64$IoError", ptr %local_2, align 8
  store %"Result$$i64$IoError" %move_load2, ptr %return_slot, align 8
  %ret_val = load %"Result$$i64$IoError", ptr %return_slot, align 8
  ret %"Result$$i64$IoError" %ret_val

bb3:                                              ; preds = %bb1
  store i32 0, ptr %local_5, align 4
  %cmp_lhs = load i32, ptr %local_4, align 4
  %cmp_rhs = load i32, ptr %local_5, align 4
  %cmp_bit = icmp eq i32 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_6, align 1
  %cond_load = load i8, ptr %local_6, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb6, label %bb4

bb4:                                              ; preds = %bb3
  store i64 1, ptr %local_11, align 8
  %machine_tag_ptr = getelementptr inbounds nuw %"Result$$i64$IoError", ptr %local_10, i32 0, i32 0
  %move_iN_load_wide = load i64, ptr %local_11, align 8
  %move_iN_trunc = trunc i64 %move_iN_load_wide to i8
  store i8 %move_iN_trunc, ptr %machine_tag_ptr, align 1
  %call_result3 = call %IoError @"fs$last_io_error"()
  store %IoError %call_result3, ptr %local_12, align 8
  br label %bb7

bb5:                                              ; No predecessors!
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

bb6:                                              ; preds = %bb3
  store i64 0, ptr %local_8, align 8
  %machine_tag_ptr4 = getelementptr inbounds nuw %"Result$$i64$IoError", ptr %local_7, i32 0, i32 0
  %move_iN_load_wide5 = load i64, ptr %local_8, align 8
  %move_iN_trunc6 = trunc i64 %move_iN_load_wide5 to i8
  store i8 %move_iN_trunc6, ptr %machine_tag_ptr4, align 1
  store i64 0, ptr %local_9, align 8
  %machine_payload_ptr = getelementptr inbounds nuw %"Result$$i64$IoError", ptr %local_7, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr, i32 0, i32 0
  %move_load7 = load i64, ptr %local_9, align 8
  store i64 %move_load7, ptr %machine_variant_field_ptr, align 8
  %move_load8 = load %"Result$$i64$IoError", ptr %local_7, align 8
  store %"Result$$i64$IoError" %move_load8, ptr %local_2, align 8
  %hew_actor_cooperate9 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel10 = icmp eq i32 %hew_actor_cooperate9, 2
  br i1 %hew_cooperate_is_cancel10, label %cancel_exit11, label %after_cooperate12

bb7:                                              ; preds = %bb4
  %machine_payload_ptr13 = getelementptr inbounds nuw %"Result$$i64$IoError", ptr %local_10, i32 0, i32 1
  %machine_variant_field_ptr14 = getelementptr inbounds nuw { %IoError }, ptr %machine_payload_ptr13, i32 0, i32 0
  %move_load15 = load %IoError, ptr %local_12, align 8
  store %IoError %move_load15, ptr %machine_variant_field_ptr14, align 8
  %move_load16 = load %"Result$$i64$IoError", ptr %local_10, align 8
  store %"Result$$i64$IoError" %move_load16, ptr %local_2, align 8
  %hew_actor_cooperate17 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel18 = icmp eq i32 %hew_actor_cooperate17, 2
  br i1 %hew_cooperate_is_cancel18, label %cancel_exit19, label %after_cooperate20

cancel_exit:                                      ; preds = %entry
  ret %"Result$$i64$IoError" zeroinitializer

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit11:                                    ; preds = %bb6
  ret %"Result$$i64$IoError" zeroinitializer

after_cooperate12:                                ; preds = %bb6
  br label %bb2

cancel_exit19:                                    ; preds = %bb7
  ret %"Result$$i64$IoError" zeroinitializer

after_cooperate20:                                ; preds = %bb7
  br label %bb2
}

define internal i64 @"fs$copy"(ptr %0, ptr %1) {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca i32, align 4
  %local_3 = alloca i64, align 8
  %local_4 = alloca i64, align 8
  store ptr %0, ptr %local_0, align 8
  store ptr %1, ptr %local_1, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_arg1 = load ptr, ptr %local_1, align 8
  %call_result = call i32 @hew_fs_copy(ptr %call_arg, ptr %call_arg1)
  store i32 %call_result, ptr %local_2, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %cast_int_src = load i32, ptr %local_2, align 4
  %cast_int_sext = sext i32 %cast_int_src to i64
  store i64 %cast_int_sext, ptr %local_3, align 8
  %move_load = load i64, ptr %local_3, align 8
  store i64 %move_load, ptr %local_4, align 8
  %move_load2 = load i64, ptr %local_4, align 8
  store i64 %move_load2, ptr %return_slot, align 8
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val

cancel_exit:                                      ; preds = %entry
  ret i64 0

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal %"Result$$i64$IoError" @"fs$try_copy"(ptr %0, ptr %1) {
entry:
  %return_slot = alloca %"Result$$i64$IoError", align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca %"Result$$i64$IoError", align 8
  %local_3 = alloca i32, align 4
  %local_4 = alloca i32, align 4
  %local_5 = alloca i32, align 4
  %local_6 = alloca i8, align 1
  %local_7 = alloca %"Result$$i64$IoError", align 8
  %local_8 = alloca i64, align 8
  %local_9 = alloca i64, align 8
  %local_10 = alloca %"Result$$i64$IoError", align 8
  %local_11 = alloca i64, align 8
  %local_12 = alloca %IoError, align 8
  store ptr %0, ptr %local_0, align 8
  store ptr %1, ptr %local_1, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_arg1 = load ptr, ptr %local_1, align 8
  %call_result = call i32 @hew_fs_copy(ptr %call_arg, ptr %call_arg1)
  store i32 %call_result, ptr %local_3, align 4
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load i32, ptr %local_3, align 4
  store i32 %move_load, ptr %local_4, align 4
  br label %bb3

bb2:                                              ; preds = %after_cooperate20, %after_cooperate12
  %move_load2 = load %"Result$$i64$IoError", ptr %local_2, align 8
  store %"Result$$i64$IoError" %move_load2, ptr %return_slot, align 8
  %ret_val = load %"Result$$i64$IoError", ptr %return_slot, align 8
  ret %"Result$$i64$IoError" %ret_val

bb3:                                              ; preds = %bb1
  store i32 0, ptr %local_5, align 4
  %cmp_lhs = load i32, ptr %local_4, align 4
  %cmp_rhs = load i32, ptr %local_5, align 4
  %cmp_bit = icmp eq i32 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_6, align 1
  %cond_load = load i8, ptr %local_6, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb6, label %bb4

bb4:                                              ; preds = %bb3
  store i64 1, ptr %local_11, align 8
  %machine_tag_ptr = getelementptr inbounds nuw %"Result$$i64$IoError", ptr %local_10, i32 0, i32 0
  %move_iN_load_wide = load i64, ptr %local_11, align 8
  %move_iN_trunc = trunc i64 %move_iN_load_wide to i8
  store i8 %move_iN_trunc, ptr %machine_tag_ptr, align 1
  %call_result3 = call %IoError @"fs$last_io_error"()
  store %IoError %call_result3, ptr %local_12, align 8
  br label %bb7

bb5:                                              ; No predecessors!
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

bb6:                                              ; preds = %bb3
  store i64 0, ptr %local_8, align 8
  %machine_tag_ptr4 = getelementptr inbounds nuw %"Result$$i64$IoError", ptr %local_7, i32 0, i32 0
  %move_iN_load_wide5 = load i64, ptr %local_8, align 8
  %move_iN_trunc6 = trunc i64 %move_iN_load_wide5 to i8
  store i8 %move_iN_trunc6, ptr %machine_tag_ptr4, align 1
  store i64 0, ptr %local_9, align 8
  %machine_payload_ptr = getelementptr inbounds nuw %"Result$$i64$IoError", ptr %local_7, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr, i32 0, i32 0
  %move_load7 = load i64, ptr %local_9, align 8
  store i64 %move_load7, ptr %machine_variant_field_ptr, align 8
  %move_load8 = load %"Result$$i64$IoError", ptr %local_7, align 8
  store %"Result$$i64$IoError" %move_load8, ptr %local_2, align 8
  %hew_actor_cooperate9 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel10 = icmp eq i32 %hew_actor_cooperate9, 2
  br i1 %hew_cooperate_is_cancel10, label %cancel_exit11, label %after_cooperate12

bb7:                                              ; preds = %bb4
  %machine_payload_ptr13 = getelementptr inbounds nuw %"Result$$i64$IoError", ptr %local_10, i32 0, i32 1
  %machine_variant_field_ptr14 = getelementptr inbounds nuw { %IoError }, ptr %machine_payload_ptr13, i32 0, i32 0
  %move_load15 = load %IoError, ptr %local_12, align 8
  store %IoError %move_load15, ptr %machine_variant_field_ptr14, align 8
  %move_load16 = load %"Result$$i64$IoError", ptr %local_10, align 8
  store %"Result$$i64$IoError" %move_load16, ptr %local_2, align 8
  %hew_actor_cooperate17 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel18 = icmp eq i32 %hew_actor_cooperate17, 2
  br i1 %hew_cooperate_is_cancel18, label %cancel_exit19, label %after_cooperate20

cancel_exit:                                      ; preds = %entry
  ret %"Result$$i64$IoError" zeroinitializer

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit11:                                    ; preds = %bb6
  ret %"Result$$i64$IoError" zeroinitializer

after_cooperate12:                                ; preds = %bb6
  br label %bb2

cancel_exit19:                                    ; preds = %bb7
  ret %"Result$$i64$IoError" zeroinitializer

after_cooperate20:                                ; preds = %bb7
  br label %bb2
}

define internal i8 @"fs$is_dir"(ptr %0) {
entry:
  %return_slot = alloca i8, align 1
  %local_0 = alloca ptr, align 8
  %local_1 = alloca i8, align 1
  %local_2 = alloca i8, align 1
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result = call i8 @hew_fs_is_dir(ptr %call_arg)
  store i8 %call_result, ptr %local_1, align 1
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load i8, ptr %local_1, align 1
  store i8 %move_load, ptr %local_2, align 1
  %move_load1 = load i8, ptr %local_2, align 1
  store i8 %move_load1, ptr %return_slot, align 1
  %ret_val = load i8, ptr %return_slot, align 1
  ret i8 %ret_val

cancel_exit:                                      ; preds = %entry
  ret i8 0

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"io$read_line"() {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_result = call ptr @hew_io_read_line()
  store ptr %call_result, ptr %local_0, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_0, align 8
  store ptr %move_load, ptr %local_1, align 8
  %move_load1 = load ptr, ptr %local_1, align 8
  store ptr %move_load1, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal i8 @"io$write"(ptr %0) {
entry:
  %return_slot = alloca i8, align 1
  %local_0 = alloca ptr, align 8
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  call void @hew_io_write(ptr %call_arg)
  br label %bb1

bb1:                                              ; preds = %bb0
  ret i8 0

cancel_exit:                                      ; preds = %entry
  ret i8 0

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal i8 @"io$write_err"(ptr %0) {
entry:
  %return_slot = alloca i8, align 1
  %local_0 = alloca ptr, align 8
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  call void @hew_io_write_err(ptr %call_arg)
  br label %bb1

bb1:                                              ; preds = %bb0
  ret i8 0

cancel_exit:                                      ; preds = %entry
  ret i8 0

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"io$read_all"() {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_result = call ptr @hew_io_read_all()
  store ptr %call_result, ptr %local_0, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_0, align 8
  store ptr %move_load, ptr %local_1, align 8
  %move_load1 = load ptr, ptr %local_1, align 8
  store ptr %move_load1, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal i8 @"bytes::push"(ptr %0, i8 %1) {
entry:
  %return_slot = alloca i8, align 1
  %local_0 = alloca { ptr, i32, i32 }, align 8
  %local_1 = alloca i8, align 1
  store i8 %1, ptr %local_1, align 1
  br label %bb0

bb0:                                              ; preds = %entry
  ret i8 0
}

define internal i8 @"bytes::pop"(ptr %0) {
entry:
  %return_slot = alloca i8, align 1
  %local_0 = alloca { ptr, i32, i32 }, align 8
  %local_1 = alloca i8, align 1
  br label %bb0

bb0:                                              ; preds = %entry
  store i8 0, ptr %local_1, align 1
  %move_load = load i8, ptr %local_1, align 1
  store i8 %move_load, ptr %return_slot, align 1
  %ret_val = load i8, ptr %return_slot, align 1
  ret i8 %ret_val
}

define internal i64 @"bytes::len"(ptr %0) {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca { ptr, i32, i32 }, align 8
  %local_1 = alloca i64, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  store i64 0, ptr %local_1, align 8
  %move_load = load i64, ptr %local_1, align 8
  store i64 %move_load, ptr %return_slot, align 8
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val
}

define internal %"Option$$u8" @"bytes::get"(ptr %0, i64 %1) {
entry:
  %return_slot = alloca %"Option$$u8", align 8
  %local_0 = alloca { ptr, i32, i32 }, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca %"Option$$u8", align 8
  %local_3 = alloca i64, align 8
  store i64 %1, ptr %local_1, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  store i64 1, ptr %local_3, align 8
  %machine_tag_ptr = getelementptr inbounds nuw %"Option$$u8", ptr %local_2, i32 0, i32 0
  %move_iN_load_wide = load i64, ptr %local_3, align 8
  %move_iN_trunc = trunc i64 %move_iN_load_wide to i8
  store i8 %move_iN_trunc, ptr %machine_tag_ptr, align 1
  %move_load = load %"Option$$u8", ptr %local_2, align 1
  store %"Option$$u8" %move_load, ptr %return_slot, align 1
  %ret_val = load %"Option$$u8", ptr %return_slot, align 1
  ret %"Option$$u8" %ret_val
}

define internal i8 @"bytes::set"(ptr %0, i64 %1, i8 %2) {
entry:
  %return_slot = alloca i8, align 1
  %local_0 = alloca { ptr, i32, i32 }, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i8, align 1
  store i64 %1, ptr %local_1, align 8
  store i8 %2, ptr %local_2, align 1
  br label %bb0

bb0:                                              ; preds = %entry
  ret i8 0
}

define internal i8 @"bytes::is_empty"(ptr %0) {
entry:
  %return_slot = alloca i8, align 1
  %local_0 = alloca { ptr, i32, i32 }, align 8
  %local_1 = alloca i8, align 1
  br label %bb0

bb0:                                              ; preds = %entry
  store i8 0, ptr %local_1, align 1
  %move_load = load i8, ptr %local_1, align 1
  store i8 %move_load, ptr %return_slot, align 1
  %ret_val = load i8, ptr %return_slot, align 1
  ret i8 %ret_val
}

define internal i8 @"bytes::clear"(ptr %0) {
entry:
  %return_slot = alloca i8, align 1
  %local_0 = alloca { ptr, i32, i32 }, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  ret i8 0
}

define internal i8 @"bytes::contains"(ptr %0, i8 %1) {
entry:
  %return_slot = alloca i8, align 1
  %local_0 = alloca { ptr, i32, i32 }, align 8
  %local_1 = alloca i8, align 1
  %local_2 = alloca i8, align 1
  store i8 %1, ptr %local_1, align 1
  br label %bb0

bb0:                                              ; preds = %entry
  store i8 0, ptr %local_2, align 1
  %move_load = load i8, ptr %local_2, align 1
  store i8 %move_load, ptr %return_slot, align 1
  %ret_val = load i8, ptr %return_slot, align 1
  ret i8 %ret_val
}

define internal ptr @"bytes::to_string"(ptr %0) {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca { ptr, i32, i32 }, align 8
  %local_1 = alloca ptr, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  store ptr @str_lit.27, ptr %local_1, align 8
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val
}

define internal i8 @"bytes::append"(ptr %0, ptr %1) {
entry:
  %return_slot = alloca i8, align 1
  %local_0 = alloca { ptr, i32, i32 }, align 8
  %local_1 = alloca { ptr, i32, i32 }, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  ret i8 0
}

define internal %Scanner @"scanner$from_string"(ptr %0) {
entry:
  %return_slot = alloca %Scanner, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca ptr, align 8
  %local_3 = alloca i8, align 1
  %local_4 = alloca i8, align 1
  %local_5 = alloca %SplitMode, align 8
  %local_6 = alloca i64, align 8
  %local_7 = alloca %Scanner, align 8
  store ptr %0, ptr %local_0, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  store i64 0, ptr %local_1, align 8
  store ptr @str_lit.28, ptr %local_2, align 8
  store i8 0, ptr %local_3, align 1
  store i8 0, ptr %local_4, align 1
  store i64 0, ptr %local_6, align 8
  %machine_tag_ptr = getelementptr inbounds nuw %SplitMode, ptr %local_5, i32 0, i32 0
  %move_iN_load_wide = load i64, ptr %local_6, align 8
  %move_iN_trunc = trunc i64 %move_iN_load_wide to i8
  store i8 %move_iN_trunc, ptr %machine_tag_ptr, align 1
  %mir_share_string_load = load ptr, ptr %local_0, align 8
  %mir_share_string_retain = call ptr @hew_string_clone(ptr %mir_share_string_load)
  %field_0_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_7, i32 0, i32 0
  %field_0_init_src = load ptr, ptr %local_0, align 8
  store ptr %field_0_init_src, ptr %field_0_init_ptr, align 8
  %field_1_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_7, i32 0, i32 1
  %field_1_init_src = load i64, ptr %local_1, align 8
  store i64 %field_1_init_src, ptr %field_1_init_ptr, align 8
  %field_2_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_7, i32 0, i32 2
  %field_2_init_src = load ptr, ptr %local_2, align 8
  store ptr %field_2_init_src, ptr %field_2_init_ptr, align 8
  %field_3_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_7, i32 0, i32 3
  %field_3_init_src = load i8, ptr %local_3, align 1
  store i8 %field_3_init_src, ptr %field_3_init_ptr, align 1
  %field_4_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_7, i32 0, i32 4
  %field_4_init_src = load i8, ptr %local_4, align 1
  store i8 %field_4_init_src, ptr %field_4_init_ptr, align 1
  %field_5_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_7, i32 0, i32 5
  %field_5_init_src = load %SplitMode, ptr %local_5, align 1
  store %SplitMode %field_5_init_src, ptr %field_5_init_ptr, align 1
  %move_load = load %Scanner, ptr %local_7, align 8
  store %Scanner %move_load, ptr %return_slot, align 8
  %ret_val = load %Scanner, ptr %return_slot, align 8
  ret %Scanner %ret_val
}

define internal %Scanner @"scanner$from_stdin"() {
entry:
  %return_slot = alloca %Scanner, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca %Scanner, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_result = call ptr @hew_io_read_all()
  store ptr %call_result, ptr %local_0, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result1 = call %Scanner @"scanner$from_string"(ptr %call_arg)
  store %Scanner %call_result1, ptr %local_1, align 8
  br label %bb2

bb2:                                              ; preds = %bb1
  %move_load = load %Scanner, ptr %local_1, align 8
  store %Scanner %move_load, ptr %return_slot, align 8
  %ret_val = load %Scanner, ptr %return_slot, align 8
  ret %Scanner %ret_val

cancel_exit:                                      ; preds = %entry
  ret %Scanner zeroinitializer

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal %"Result$$Scanner$IoError" @"scanner$from_file"(ptr %0) {
entry:
  %return_slot = alloca %"Result$$Scanner$IoError", align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca %"Result$$Scanner$IoError", align 8
  %local_2 = alloca %"Result$$string$IoError", align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i64, align 8
  %local_5 = alloca i8, align 1
  %local_6 = alloca i64, align 8
  %local_7 = alloca i8, align 1
  %local_8 = alloca ptr, align 8
  %local_9 = alloca %"Result$$Scanner$IoError", align 8
  %local_10 = alloca i64, align 8
  %local_11 = alloca i64, align 8
  %local_12 = alloca ptr, align 8
  %local_13 = alloca i8, align 1
  %local_14 = alloca i8, align 1
  %local_15 = alloca %SplitMode, align 8
  %local_16 = alloca i64, align 8
  %local_17 = alloca %Scanner, align 8
  %local_18 = alloca %IoError, align 8
  %local_19 = alloca %"Result$$Scanner$IoError", align 8
  %local_20 = alloca i64, align 8
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result = call %"Result$$string$IoError" @"fs$try_read"(ptr %call_arg)
  store %"Result$$string$IoError" %call_result, ptr %local_2, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %machine_tag_ptr = getelementptr inbounds nuw %"Result$$string$IoError", ptr %local_2, i32 0, i32 0
  %move_iN_load = load i8, ptr %machine_tag_ptr, align 1
  %move_iN_zext = zext i8 %move_iN_load to i64
  store i64 %move_iN_zext, ptr %local_3, align 8
  store i64 0, ptr %local_4, align 8
  %cmp_lhs = load i64, ptr %local_3, align 8
  %cmp_rhs = load i64, ptr %local_4, align 8
  %cmp_bit = icmp eq i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_5, align 1
  %cond_load = load i8, ptr %local_5, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb3, label %bb6

bb2:                                              ; preds = %after_cooperate27, %after_cooperate13
  %move_load = load %"Result$$Scanner$IoError", ptr %local_1, align 8
  store %"Result$$Scanner$IoError" %move_load, ptr %return_slot, align 8
  %ret_val = load %"Result$$Scanner$IoError", ptr %return_slot, align 8
  ret %"Result$$Scanner$IoError" %ret_val

bb3:                                              ; preds = %bb1
  %machine_payload_ptr = getelementptr inbounds nuw %"Result$$string$IoError", ptr %local_2, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { ptr }, ptr %machine_payload_ptr, i32 0, i32 0
  %move_load1 = load ptr, ptr %machine_variant_field_ptr, align 8
  store ptr %move_load1, ptr %local_8, align 8
  store i64 0, ptr %local_10, align 8
  %machine_tag_ptr2 = getelementptr inbounds nuw %"Result$$Scanner$IoError", ptr %local_9, i32 0, i32 0
  %move_iN_load_wide = load i64, ptr %local_10, align 8
  %move_iN_trunc = trunc i64 %move_iN_load_wide to i8
  store i8 %move_iN_trunc, ptr %machine_tag_ptr2, align 1
  store i64 0, ptr %local_11, align 8
  store ptr @str_lit.29, ptr %local_12, align 8
  store i8 0, ptr %local_13, align 1
  store i8 0, ptr %local_14, align 1
  store i64 0, ptr %local_16, align 8
  %machine_tag_ptr3 = getelementptr inbounds nuw %SplitMode, ptr %local_15, i32 0, i32 0
  %move_iN_load_wide4 = load i64, ptr %local_16, align 8
  %move_iN_trunc5 = trunc i64 %move_iN_load_wide4 to i8
  store i8 %move_iN_trunc5, ptr %machine_tag_ptr3, align 1
  %field_0_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_17, i32 0, i32 0
  %field_0_init_src = load ptr, ptr %local_8, align 8
  store ptr %field_0_init_src, ptr %field_0_init_ptr, align 8
  %field_1_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_17, i32 0, i32 1
  %field_1_init_src = load i64, ptr %local_11, align 8
  store i64 %field_1_init_src, ptr %field_1_init_ptr, align 8
  %field_2_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_17, i32 0, i32 2
  %field_2_init_src = load ptr, ptr %local_12, align 8
  store ptr %field_2_init_src, ptr %field_2_init_ptr, align 8
  %field_3_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_17, i32 0, i32 3
  %field_3_init_src = load i8, ptr %local_13, align 1
  store i8 %field_3_init_src, ptr %field_3_init_ptr, align 1
  %field_4_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_17, i32 0, i32 4
  %field_4_init_src = load i8, ptr %local_14, align 1
  store i8 %field_4_init_src, ptr %field_4_init_ptr, align 1
  %field_5_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_17, i32 0, i32 5
  %field_5_init_src = load %SplitMode, ptr %local_15, align 1
  store %SplitMode %field_5_init_src, ptr %field_5_init_ptr, align 1
  %machine_payload_ptr6 = getelementptr inbounds nuw %"Result$$Scanner$IoError", ptr %local_9, i32 0, i32 1
  %machine_variant_field_ptr7 = getelementptr inbounds nuw { %Scanner }, ptr %machine_payload_ptr6, i32 0, i32 0
  %move_load8 = load %Scanner, ptr %local_17, align 8
  store %Scanner %move_load8, ptr %machine_variant_field_ptr7, align 8
  %move_load9 = load %"Result$$Scanner$IoError", ptr %local_9, align 8
  store %"Result$$Scanner$IoError" %move_load9, ptr %local_1, align 8
  %hew_actor_cooperate10 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel11 = icmp eq i32 %hew_actor_cooperate10, 2
  br i1 %hew_cooperate_is_cancel11, label %cancel_exit12, label %after_cooperate13

bb4:                                              ; preds = %bb6
  %machine_payload_ptr14 = getelementptr inbounds nuw %"Result$$string$IoError", ptr %local_2, i32 0, i32 1
  %machine_variant_field_ptr15 = getelementptr inbounds nuw { %IoError }, ptr %machine_payload_ptr14, i32 0, i32 0
  %move_load16 = load %IoError, ptr %machine_variant_field_ptr15, align 8
  store %IoError %move_load16, ptr %local_18, align 8
  store i64 1, ptr %local_20, align 8
  %machine_tag_ptr17 = getelementptr inbounds nuw %"Result$$Scanner$IoError", ptr %local_19, i32 0, i32 0
  %move_iN_load_wide18 = load i64, ptr %local_20, align 8
  %move_iN_trunc19 = trunc i64 %move_iN_load_wide18 to i8
  store i8 %move_iN_trunc19, ptr %machine_tag_ptr17, align 1
  %machine_payload_ptr20 = getelementptr inbounds nuw %"Result$$Scanner$IoError", ptr %local_19, i32 0, i32 1
  %machine_variant_field_ptr21 = getelementptr inbounds nuw { %IoError }, ptr %machine_payload_ptr20, i32 0, i32 0
  %move_load22 = load %IoError, ptr %local_18, align 8
  store %IoError %move_load22, ptr %machine_variant_field_ptr21, align 8
  %move_load23 = load %"Result$$Scanner$IoError", ptr %local_19, align 8
  store %"Result$$Scanner$IoError" %move_load23, ptr %local_1, align 8
  %hew_actor_cooperate24 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel25 = icmp eq i32 %hew_actor_cooperate24, 2
  br i1 %hew_cooperate_is_cancel25, label %cancel_exit26, label %after_cooperate27

bb5:                                              ; preds = %bb6
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

bb6:                                              ; preds = %bb1
  store i64 1, ptr %local_6, align 8
  %cmp_lhs28 = load i64, ptr %local_3, align 8
  %cmp_rhs29 = load i64, ptr %local_6, align 8
  %cmp_bit30 = icmp eq i64 %cmp_lhs28, %cmp_rhs29
  %cmp_zext31 = zext i1 %cmp_bit30 to i8
  store i8 %cmp_zext31, ptr %local_7, align 1
  %cond_load32 = load i8, ptr %local_7, align 1
  %cond_nz33 = icmp ne i8 %cond_load32, 0
  br i1 %cond_nz33, label %bb4, label %bb5

cancel_exit:                                      ; preds = %entry
  ret %"Result$$Scanner$IoError" zeroinitializer

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit12:                                    ; preds = %bb3
  ret %"Result$$Scanner$IoError" zeroinitializer

after_cooperate13:                                ; preds = %bb3
  br label %bb2

cancel_exit26:                                    ; preds = %bb4
  ret %"Result$$Scanner$IoError" zeroinitializer

after_cooperate27:                                ; preds = %bb4
  br label %bb2
}

define internal %Scanner @"scanner$with_split"(%Scanner %0, %SplitMode %1) {
entry:
  %return_slot = alloca %Scanner, align 8
  %local_0 = alloca %Scanner, align 8
  %local_1 = alloca %SplitMode, align 8
  %local_2 = alloca ptr, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca ptr, align 8
  %local_5 = alloca i8, align 1
  %local_6 = alloca i8, align 1
  %local_7 = alloca %SplitMode, align 8
  %local_8 = alloca %Scanner, align 8
  store %Scanner %0, ptr %local_0, align 8
  store %SplitMode %1, ptr %local_1, align 1
  br label %bb0

bb0:                                              ; preds = %entry
  %field_0_load_ptr = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 0
  %field_0_load = load ptr, ptr %field_0_load_ptr, align 8
  %field_0_str_retain = call ptr @hew_string_clone(ptr %field_0_load)
  store ptr %field_0_str_retain, ptr %local_2, align 8
  store i64 0, ptr %local_3, align 8
  store ptr @str_lit.30, ptr %local_4, align 8
  store i8 0, ptr %local_5, align 1
  store i8 0, ptr %local_6, align 1
  %move_load = load %SplitMode, ptr %local_1, align 1
  store %SplitMode %move_load, ptr %local_7, align 1
  store %SplitMode zeroinitializer, ptr %local_1, align 1
  %field_0_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_8, i32 0, i32 0
  %field_0_init_src = load ptr, ptr %local_2, align 8
  store ptr %field_0_init_src, ptr %field_0_init_ptr, align 8
  %field_1_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_8, i32 0, i32 1
  %field_1_init_src = load i64, ptr %local_3, align 8
  store i64 %field_1_init_src, ptr %field_1_init_ptr, align 8
  %field_2_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_8, i32 0, i32 2
  %field_2_init_src = load ptr, ptr %local_4, align 8
  store ptr %field_2_init_src, ptr %field_2_init_ptr, align 8
  %field_3_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_8, i32 0, i32 3
  %field_3_init_src = load i8, ptr %local_5, align 1
  store i8 %field_3_init_src, ptr %field_3_init_ptr, align 1
  %field_4_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_8, i32 0, i32 4
  %field_4_init_src = load i8, ptr %local_6, align 1
  store i8 %field_4_init_src, ptr %field_4_init_ptr, align 1
  %field_5_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_8, i32 0, i32 5
  %field_5_init_src = load %SplitMode, ptr %local_7, align 1
  store %SplitMode %field_5_init_src, ptr %field_5_init_ptr, align 1
  %move_load1 = load %Scanner, ptr %local_8, align 8
  store %Scanner %move_load1, ptr %return_slot, align 8
  call void @__hew_enum_drop_inplace_SplitMode(ptr %local_1)
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  %ret_val = load %Scanner, ptr %return_slot, align 8
  ret %Scanner %ret_val
}

define internal %Scanner @"scanner$scan"(%Scanner %0) {
entry:
  %return_slot = alloca %Scanner, align 8
  %local_0 = alloca %Scanner, align 8
  %local_1 = alloca %Scanner, align 8
  %local_2 = alloca %SplitMode, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i64, align 8
  %local_5 = alloca i8, align 1
  %local_6 = alloca i64, align 8
  %local_7 = alloca i8, align 1
  %local_8 = alloca %Scanner, align 8
  %local_9 = alloca %Scanner, align 8
  %local_10 = alloca %Scanner, align 8
  %local_11 = alloca %Scanner, align 8
  store %Scanner %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %field_5_load_ptr = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 5
  %field_5_load = load %SplitMode, ptr %field_5_load_ptr, align 1
  store %SplitMode %field_5_load, ptr %local_2, align 1
  %machine_tag_ptr = getelementptr inbounds nuw %SplitMode, ptr %local_2, i32 0, i32 0
  %move_iN_load = load i8, ptr %machine_tag_ptr, align 1
  %move_iN_zext = zext i8 %move_iN_load to i64
  store i64 %move_iN_zext, ptr %local_3, align 8
  store i64 0, ptr %local_4, align 8
  %cmp_lhs = load i64, ptr %local_3, align 8
  %cmp_rhs = load i64, ptr %local_4, align 8
  %cmp_bit = icmp eq i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_5, align 1
  %cond_load = load i8, ptr %local_5, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb2, label %bb5

bb1:                                              ; preds = %after_cooperate22, %after_cooperate17
  %move_load = load %Scanner, ptr %local_1, align 8
  store %Scanner %move_load, ptr %return_slot, align 8
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  %ret_val = load %Scanner, ptr %return_slot, align 8
  ret %Scanner %ret_val

bb2:                                              ; preds = %bb0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %local_10, ptr align 8 %local_0, i64 32, i1 false)
  %record_clone_Scanner = call i32 @__hew_record_clone_inplace_Scanner(ptr %local_0, ptr %local_10)
  %record_clone_Scanner_failed = icmp ne i32 %record_clone_Scanner, 0
  br i1 %record_clone_Scanner_failed, label %clone_trap, label %clone_ok

bb3:                                              ; preds = %bb5
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %local_11, ptr align 8 %local_0, i64 32, i1 false)
  %record_clone_Scanner1 = call i32 @__hew_record_clone_inplace_Scanner(ptr %local_0, ptr %local_11)
  %record_clone_Scanner_failed2 = icmp ne i32 %record_clone_Scanner1, 0
  br i1 %record_clone_Scanner_failed2, label %clone_trap4, label %clone_ok3

bb4:                                              ; preds = %bb5
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

bb5:                                              ; preds = %bb0
  store i64 1, ptr %local_6, align 8
  %cmp_lhs7 = load i64, ptr %local_3, align 8
  %cmp_rhs8 = load i64, ptr %local_6, align 8
  %cmp_bit9 = icmp eq i64 %cmp_lhs7, %cmp_rhs8
  %cmp_zext10 = zext i1 %cmp_bit9 to i8
  store i8 %cmp_zext10, ptr %local_7, align 1
  %cond_load11 = load i8, ptr %local_7, align 1
  %cond_nz12 = icmp ne i8 %cond_load11, 0
  br i1 %cond_nz12, label %bb3, label %bb4

bb6:                                              ; preds = %clone_ok
  %move_load13 = load %Scanner, ptr %local_8, align 8
  store %Scanner %move_load13, ptr %local_1, align 8
  %hew_actor_cooperate14 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel15 = icmp eq i32 %hew_actor_cooperate14, 2
  br i1 %hew_cooperate_is_cancel15, label %cancel_exit16, label %after_cooperate17

bb7:                                              ; preds = %clone_ok3
  %move_load18 = load %Scanner, ptr %local_9, align 8
  store %Scanner %move_load18, ptr %local_1, align 8
  %hew_actor_cooperate19 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel20 = icmp eq i32 %hew_actor_cooperate19, 2
  br i1 %hew_cooperate_is_cancel20, label %cancel_exit21, label %after_cooperate22

cancel_exit:                                      ; preds = %entry
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  ret %Scanner zeroinitializer

after_cooperate:                                  ; preds = %entry
  br label %bb0

clone_ok:                                         ; preds = %bb2
  %call_arg = load %Scanner, ptr %local_10, align 8
  %call_result = call %Scanner @"scanner$scan_line"(%Scanner %call_arg)
  store %Scanner %call_result, ptr %local_8, align 8
  br label %bb6

clone_trap:                                       ; preds = %bb2
  call void @llvm.trap()
  unreachable

clone_ok3:                                        ; preds = %bb3
  %call_arg5 = load %Scanner, ptr %local_11, align 8
  %call_result6 = call %Scanner @"scanner$scan_word"(%Scanner %call_arg5)
  store %Scanner %call_result6, ptr %local_9, align 8
  br label %bb7

clone_trap4:                                      ; preds = %bb3
  call void @llvm.trap()
  unreachable

cancel_exit16:                                    ; preds = %bb6
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  ret %Scanner zeroinitializer

after_cooperate17:                                ; preds = %bb6
  br label %bb1

cancel_exit21:                                    ; preds = %bb7
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  ret %Scanner zeroinitializer

after_cooperate22:                                ; preds = %bb7
  br label %bb1
}

define internal { %Scanner, %"Option$$string" } @"scanner$next_line"(%Scanner %0) {
entry:
  %return_slot = alloca { %Scanner, %"Option$$string" }, align 8
  %local_0 = alloca %Scanner, align 8
  %local_1 = alloca %SplitMode, align 8
  %local_2 = alloca %SplitMode, align 8
  %local_3 = alloca ptr, align 8
  %local_4 = alloca i64, align 8
  %local_5 = alloca ptr, align 8
  %local_6 = alloca i8, align 1
  %local_7 = alloca i8, align 1
  %local_8 = alloca %SplitMode, align 8
  %local_9 = alloca i64, align 8
  %local_10 = alloca %Scanner, align 8
  %local_11 = alloca %Scanner, align 8
  %local_12 = alloca %Scanner, align 8
  %local_13 = alloca %Scanner, align 8
  %local_14 = alloca ptr, align 8
  %local_15 = alloca i64, align 8
  %local_16 = alloca ptr, align 8
  %local_17 = alloca i8, align 1
  %local_18 = alloca i8, align 1
  %local_19 = alloca %Scanner, align 8
  %local_20 = alloca %Scanner, align 8
  %local_21 = alloca { %Scanner, %"Option$$string" }, align 8
  %local_22 = alloca i8, align 1
  %local_23 = alloca ptr, align 8
  %local_24 = alloca ptr, align 8
  %local_25 = alloca %"Option$$string", align 8
  %local_26 = alloca i64, align 8
  %local_27 = alloca { %Scanner, %"Option$$string" }, align 8
  %local_28 = alloca { %Scanner, %"Option$$string" }, align 8
  %local_29 = alloca %"Option$$string", align 8
  %local_30 = alloca i64, align 8
  %local_31 = alloca { %Scanner, %"Option$$string" }, align 8
  %local_32 = alloca { %Scanner, %"Option$$string" }, align 8
  %local_33 = alloca %Scanner, align 8
  store %Scanner %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %field_5_load_ptr = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 5
  %field_5_load = load %SplitMode, ptr %field_5_load_ptr, align 1
  store %SplitMode %field_5_load, ptr %local_1, align 1
  %carrier_path_d0_f5_ptr = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 5
  store %SplitMode zeroinitializer, ptr %carrier_path_d0_f5_ptr, align 1
  %move_load = load %SplitMode, ptr %local_1, align 1
  store %SplitMode %move_load, ptr %local_2, align 1
  %field_0_load_ptr = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 0
  %field_0_load = load ptr, ptr %field_0_load_ptr, align 8
  %field_0_str_retain = call ptr @hew_string_clone(ptr %field_0_load)
  store ptr %field_0_str_retain, ptr %local_3, align 8
  %field_1_load_ptr = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 1
  %field_1_load = load i64, ptr %field_1_load_ptr, align 8
  store i64 %field_1_load, ptr %local_4, align 8
  %field_2_load_ptr = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 2
  %field_2_load = load ptr, ptr %field_2_load_ptr, align 8
  %field_2_str_retain = call ptr @hew_string_clone(ptr %field_2_load)
  store ptr %field_2_str_retain, ptr %local_5, align 8
  %field_3_load_ptr = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 3
  %field_3_load = load i8, ptr %field_3_load_ptr, align 1
  store i8 %field_3_load, ptr %local_6, align 1
  %field_4_load_ptr = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 4
  %field_4_load = load i8, ptr %field_4_load_ptr, align 1
  store i8 %field_4_load, ptr %local_7, align 1
  store i64 0, ptr %local_9, align 8
  %machine_tag_ptr = getelementptr inbounds nuw %SplitMode, ptr %local_8, i32 0, i32 0
  %move_iN_load_wide = load i64, ptr %local_9, align 8
  %move_iN_trunc = trunc i64 %move_iN_load_wide to i8
  store i8 %move_iN_trunc, ptr %machine_tag_ptr, align 1
  %field_0_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_10, i32 0, i32 0
  %field_0_init_src = load ptr, ptr %local_3, align 8
  store ptr %field_0_init_src, ptr %field_0_init_ptr, align 8
  %field_1_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_10, i32 0, i32 1
  %field_1_init_src = load i64, ptr %local_4, align 8
  store i64 %field_1_init_src, ptr %field_1_init_ptr, align 8
  %field_2_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_10, i32 0, i32 2
  %field_2_init_src = load ptr, ptr %local_5, align 8
  store ptr %field_2_init_src, ptr %field_2_init_ptr, align 8
  %field_3_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_10, i32 0, i32 3
  %field_3_init_src = load i8, ptr %local_6, align 1
  store i8 %field_3_init_src, ptr %field_3_init_ptr, align 1
  %field_4_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_10, i32 0, i32 4
  %field_4_init_src = load i8, ptr %local_7, align 1
  store i8 %field_4_init_src, ptr %field_4_init_ptr, align 1
  %field_5_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_10, i32 0, i32 5
  %field_5_init_src = load %SplitMode, ptr %local_8, align 1
  store %SplitMode %field_5_init_src, ptr %field_5_init_ptr, align 1
  %move_load1 = load %Scanner, ptr %local_10, align 8
  store %Scanner %move_load1, ptr %local_11, align 8
  %move_load2 = load %Scanner, ptr %local_11, align 8
  store %Scanner %move_load2, ptr %local_33, align 8
  store %Scanner zeroinitializer, ptr %local_11, align 8
  %call_arg = load %Scanner, ptr %local_33, align 8
  %call_result = call %Scanner @"scanner$scan"(%Scanner %call_arg)
  store %Scanner %call_result, ptr %local_12, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load3 = load %Scanner, ptr %local_12, align 8
  store %Scanner %move_load3, ptr %local_13, align 8
  %field_0_load_ptr4 = getelementptr inbounds nuw %Scanner, ptr %local_13, i32 0, i32 0
  %field_0_load5 = load ptr, ptr %field_0_load_ptr4, align 8
  %field_0_str_retain6 = call ptr @hew_string_clone(ptr %field_0_load5)
  store ptr %field_0_str_retain6, ptr %local_14, align 8
  %field_1_load_ptr7 = getelementptr inbounds nuw %Scanner, ptr %local_13, i32 0, i32 1
  %field_1_load8 = load i64, ptr %field_1_load_ptr7, align 8
  store i64 %field_1_load8, ptr %local_15, align 8
  %field_2_load_ptr9 = getelementptr inbounds nuw %Scanner, ptr %local_13, i32 0, i32 2
  %field_2_load10 = load ptr, ptr %field_2_load_ptr9, align 8
  %field_2_str_retain11 = call ptr @hew_string_clone(ptr %field_2_load10)
  store ptr %field_2_str_retain11, ptr %local_16, align 8
  %field_3_load_ptr12 = getelementptr inbounds nuw %Scanner, ptr %local_13, i32 0, i32 3
  %field_3_load13 = load i8, ptr %field_3_load_ptr12, align 1
  store i8 %field_3_load13, ptr %local_17, align 1
  %field_4_load_ptr14 = getelementptr inbounds nuw %Scanner, ptr %local_13, i32 0, i32 4
  %field_4_load15 = load i8, ptr %field_4_load_ptr14, align 1
  store i8 %field_4_load15, ptr %local_18, align 1
  %field_0_init_ptr16 = getelementptr inbounds nuw %Scanner, ptr %local_19, i32 0, i32 0
  %field_0_init_src17 = load ptr, ptr %local_14, align 8
  store ptr %field_0_init_src17, ptr %field_0_init_ptr16, align 8
  %field_1_init_ptr18 = getelementptr inbounds nuw %Scanner, ptr %local_19, i32 0, i32 1
  %field_1_init_src19 = load i64, ptr %local_15, align 8
  store i64 %field_1_init_src19, ptr %field_1_init_ptr18, align 8
  %field_2_init_ptr20 = getelementptr inbounds nuw %Scanner, ptr %local_19, i32 0, i32 2
  %field_2_init_src21 = load ptr, ptr %local_16, align 8
  store ptr %field_2_init_src21, ptr %field_2_init_ptr20, align 8
  %field_3_init_ptr22 = getelementptr inbounds nuw %Scanner, ptr %local_19, i32 0, i32 3
  %field_3_init_src23 = load i8, ptr %local_17, align 1
  store i8 %field_3_init_src23, ptr %field_3_init_ptr22, align 1
  %field_4_init_ptr24 = getelementptr inbounds nuw %Scanner, ptr %local_19, i32 0, i32 4
  %field_4_init_src25 = load i8, ptr %local_18, align 1
  store i8 %field_4_init_src25, ptr %field_4_init_ptr24, align 1
  %field_5_init_ptr26 = getelementptr inbounds nuw %Scanner, ptr %local_19, i32 0, i32 5
  %field_5_init_src27 = load %SplitMode, ptr %local_2, align 1
  store %SplitMode %field_5_init_src27, ptr %field_5_init_ptr26, align 1
  %move_load28 = load %Scanner, ptr %local_19, align 8
  store %Scanner %move_load28, ptr %local_20, align 8
  %call_arg29 = load %Scanner, ptr %local_13, align 8
  %call_result30 = call i8 @"scanner$has_next"(%Scanner %call_arg29)
  store i8 %call_result30, ptr %local_22, align 1
  br label %bb2

bb2:                                              ; preds = %bb1
  %cond_load = load i8, ptr %local_22, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb3, label %bb4

bb3:                                              ; preds = %bb2
  %call_arg31 = load %Scanner, ptr %local_13, align 8
  %call_result32 = call ptr @"scanner$text"(%Scanner %call_arg31)
  store ptr %call_result32, ptr %local_23, align 8
  br label %bb6

bb4:                                              ; preds = %bb2
  store i64 1, ptr %local_30, align 8
  %machine_tag_ptr33 = getelementptr inbounds nuw %"Option$$string", ptr %local_29, i32 0, i32 0
  %move_iN_load_wide34 = load i64, ptr %local_30, align 8
  %move_iN_trunc35 = trunc i64 %move_iN_load_wide34 to i8
  store i8 %move_iN_trunc35, ptr %machine_tag_ptr33, align 1
  %tuple_elem_0_load = load %Scanner, ptr %local_20, align 8
  %tuple_elem_0_gep = getelementptr inbounds nuw { %Scanner, %"Option$$string" }, ptr %local_31, i32 0, i32 0
  store %Scanner %tuple_elem_0_load, ptr %tuple_elem_0_gep, align 8
  %tuple_elem_1_load = load %"Option$$string", ptr %local_29, align 8
  %tuple_elem_1_gep = getelementptr inbounds nuw { %Scanner, %"Option$$string" }, ptr %local_31, i32 0, i32 1
  store %"Option$$string" %tuple_elem_1_load, ptr %tuple_elem_1_gep, align 8
  %move_load36 = load { %Scanner, %"Option$$string" }, ptr %local_31, align 8
  store { %Scanner, %"Option$$string" } %move_load36, ptr %local_32, align 8
  %move_load37 = load { %Scanner, %"Option$$string" }, ptr %local_32, align 8
  store { %Scanner, %"Option$$string" } %move_load37, ptr %local_21, align 8
  br label %bb5

bb5:                                              ; preds = %after_cooperate53, %bb4
  %move_load38 = load { %Scanner, %"Option$$string" }, ptr %local_21, align 8
  store { %Scanner, %"Option$$string" } %move_load38, ptr %return_slot, align 8
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  %ret_val = load { %Scanner, %"Option$$string" }, ptr %return_slot, align 8
  ret { %Scanner, %"Option$$string" } %ret_val

bb6:                                              ; preds = %bb3
  %move_load39 = load ptr, ptr %local_23, align 8
  store ptr %move_load39, ptr %local_24, align 8
  store i64 0, ptr %local_26, align 8
  %machine_tag_ptr40 = getelementptr inbounds nuw %"Option$$string", ptr %local_25, i32 0, i32 0
  %move_iN_load_wide41 = load i64, ptr %local_26, align 8
  %move_iN_trunc42 = trunc i64 %move_iN_load_wide41 to i8
  store i8 %move_iN_trunc42, ptr %machine_tag_ptr40, align 1
  %machine_payload_ptr = getelementptr inbounds nuw %"Option$$string", ptr %local_25, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { ptr }, ptr %machine_payload_ptr, i32 0, i32 0
  %move_load43 = load ptr, ptr %local_24, align 8
  store ptr %move_load43, ptr %machine_variant_field_ptr, align 8
  %tuple_elem_0_load44 = load %Scanner, ptr %local_20, align 8
  %tuple_elem_0_gep45 = getelementptr inbounds nuw { %Scanner, %"Option$$string" }, ptr %local_27, i32 0, i32 0
  store %Scanner %tuple_elem_0_load44, ptr %tuple_elem_0_gep45, align 8
  %tuple_elem_1_load46 = load %"Option$$string", ptr %local_25, align 8
  %tuple_elem_1_gep47 = getelementptr inbounds nuw { %Scanner, %"Option$$string" }, ptr %local_27, i32 0, i32 1
  store %"Option$$string" %tuple_elem_1_load46, ptr %tuple_elem_1_gep47, align 8
  %move_load48 = load { %Scanner, %"Option$$string" }, ptr %local_27, align 8
  store { %Scanner, %"Option$$string" } %move_load48, ptr %local_28, align 8
  %move_load49 = load { %Scanner, %"Option$$string" }, ptr %local_28, align 8
  store { %Scanner, %"Option$$string" } %move_load49, ptr %local_21, align 8
  %hew_actor_cooperate50 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel51 = icmp eq i32 %hew_actor_cooperate50, 2
  br i1 %hew_cooperate_is_cancel51, label %cancel_exit52, label %after_cooperate53

cancel_exit:                                      ; preds = %entry
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  ret { %Scanner, %"Option$$string" } zeroinitializer

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit52:                                    ; preds = %bb6
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  ret { %Scanner, %"Option$$string" } zeroinitializer

after_cooperate53:                                ; preds = %bb6
  br label %bb5
}

define internal i8 @"scanner$has_next"(%Scanner %0) {
entry:
  %return_slot = alloca i8, align 1
  %local_0 = alloca %Scanner, align 8
  %local_1 = alloca i8, align 1
  store %Scanner %0, ptr %local_0, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  %field_3_load_ptr = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 3
  %field_3_load = load i8, ptr %field_3_load_ptr, align 1
  store i8 %field_3_load, ptr %local_1, align 1
  %move_load = load i8, ptr %local_1, align 1
  store i8 %move_load, ptr %return_slot, align 1
  %ret_val = load i8, ptr %return_slot, align 1
  ret i8 %ret_val
}

define internal ptr @"scanner$text"(%Scanner %0) {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca %Scanner, align 8
  %local_1 = alloca ptr, align 8
  store %Scanner %0, ptr %local_0, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  %field_2_load_ptr = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 2
  %field_2_load = load ptr, ptr %field_2_load_ptr, align 8
  %field_2_str_retain = call ptr @hew_string_clone(ptr %field_2_load)
  store ptr %field_2_str_retain, ptr %local_1, align 8
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val
}

define internal ptr @"scanner$lines"(ptr %0) {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca ptr, align 8
  %local_3 = alloca i8, align 1
  %local_4 = alloca i64, align 8
  %local_5 = alloca i64, align 8
  %local_6 = alloca i8, align 1
  %local_7 = alloca %Scanner, align 8
  %local_8 = alloca %Scanner, align 8
  %local_9 = alloca %Scanner, align 8
  %local_10 = alloca i8, align 1
  %local_11 = alloca ptr, align 8
  %local_12 = alloca %Scanner, align 8
  %local_13 = alloca %Scanner, align 8
  %local_14 = alloca %Scanner, align 8
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %hew_vec_new_str_call = call ptr @hew_vec_new_str()
  store ptr %hew_vec_new_str_call, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %local_2, align 8
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result = call i32 @hew_string_length(ptr %call_arg)
  %ffi_sext = sext i32 %call_result to i64
  store i64 %ffi_sext, ptr %local_4, align 8
  br label %bb2

bb2:                                              ; preds = %bb1
  store i64 0, ptr %local_5, align 8
  %cmp_lhs = load i64, ptr %local_4, align 8
  %cmp_rhs = load i64, ptr %local_5, align 8
  %cmp_bit = icmp eq i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_6, align 1
  %cond_load = load i8, ptr %local_6, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb3, label %bb4

bb3:                                              ; preds = %bb2
  %move_load1 = load ptr, ptr %local_2, align 8
  store ptr %move_load1, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

bb4:                                              ; preds = %bb2
  br label %bb5

bb5:                                              ; preds = %after_cooperate7, %bb4
  %call_arg2 = load ptr, ptr %local_0, align 8
  %call_result3 = call %Scanner @"scanner$from_string"(ptr %call_arg2)
  store %Scanner %call_result3, ptr %local_7, align 8
  br label %bb7

bb6:                                              ; No predecessors!
  %hew_actor_cooperate4 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel5 = icmp eq i32 %hew_actor_cooperate4, 2
  br i1 %hew_cooperate_is_cancel5, label %cancel_exit6, label %after_cooperate7

bb7:                                              ; preds = %bb5
  %move_load8 = load %Scanner, ptr %local_7, align 8
  store %Scanner %move_load8, ptr %local_8, align 8
  %move_load9 = load %Scanner, ptr %local_8, align 8
  store %Scanner %move_load9, ptr %local_13, align 8
  store %Scanner zeroinitializer, ptr %local_8, align 8
  %call_arg10 = load %Scanner, ptr %local_13, align 8
  %call_result11 = call %Scanner @"scanner$scan"(%Scanner %call_arg10)
  store %Scanner %call_result11, ptr %local_9, align 8
  br label %bb8

bb8:                                              ; preds = %bb7
  %move_load12 = load %Scanner, ptr %local_9, align 8
  store %Scanner %move_load12, ptr %local_8, align 8
  br label %bb9

bb9:                                              ; preds = %after_cooperate28, %bb8
  %call_arg13 = load %Scanner, ptr %local_8, align 8
  %call_result14 = call i8 @"scanner$has_next"(%Scanner %call_arg13)
  store i8 %call_result14, ptr %local_10, align 1
  br label %bb12

bb10:                                             ; preds = %bb12
  %field_2_load_ptr = getelementptr inbounds nuw %Scanner, ptr %local_8, i32 0, i32 2
  %field_2_load = load ptr, ptr %field_2_load_ptr, align 8
  %field_2_str_retain = call ptr @hew_string_clone(ptr %field_2_load)
  store ptr %field_2_str_retain, ptr %local_11, align 8
  %call_arg15 = load ptr, ptr %local_2, align 8
  %call_arg16 = load ptr, ptr %local_11, align 8
  call void @hew_vec_push_str(ptr %call_arg15, ptr %call_arg16)
  br label %bb13

bb11:                                             ; preds = %bb12
  %move_load17 = load ptr, ptr %local_2, align 8
  store ptr %move_load17, ptr %return_slot, align 8
  %ret_val18 = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val18

bb12:                                             ; preds = %bb9
  %cond_load19 = load i8, ptr %local_10, align 1
  %cond_nz20 = icmp ne i8 %cond_load19, 0
  br i1 %cond_nz20, label %bb10, label %bb11

bb13:                                             ; preds = %bb10
  %"hew_string_drop drop" = load ptr, ptr %local_11, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop")
  store ptr null, ptr %local_11, align 8
  %move_load21 = load %Scanner, ptr %local_8, align 8
  store %Scanner %move_load21, ptr %local_14, align 8
  store %Scanner zeroinitializer, ptr %local_8, align 8
  %call_arg22 = load %Scanner, ptr %local_14, align 8
  %call_result23 = call %Scanner @"scanner$scan"(%Scanner %call_arg22)
  store %Scanner %call_result23, ptr %local_12, align 8
  br label %bb14

bb14:                                             ; preds = %bb13
  %move_load24 = load %Scanner, ptr %local_12, align 8
  store %Scanner %move_load24, ptr %local_8, align 8
  %hew_actor_cooperate25 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel26 = icmp eq i32 %hew_actor_cooperate25, 2
  br i1 %hew_cooperate_is_cancel26, label %cancel_exit27, label %after_cooperate28

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit6:                                     ; preds = %bb6
  ret ptr null

after_cooperate7:                                 ; preds = %bb6
  br label %bb5

cancel_exit27:                                    ; preds = %bb14
  ret ptr null

after_cooperate28:                                ; preds = %bb14
  br label %bb9
}

define internal ptr @"scanner$words"(ptr %0) {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca ptr, align 8
  %local_3 = alloca i8, align 1
  %local_4 = alloca i64, align 8
  %local_5 = alloca i64, align 8
  %local_6 = alloca i8, align 1
  %local_7 = alloca %Scanner, align 8
  %local_8 = alloca %Scanner, align 8
  %local_9 = alloca %SplitMode, align 8
  %local_10 = alloca i64, align 8
  %local_11 = alloca %Scanner, align 8
  %local_12 = alloca %Scanner, align 8
  %local_13 = alloca i8, align 1
  %local_14 = alloca ptr, align 8
  %local_15 = alloca %Scanner, align 8
  %local_16 = alloca %Scanner, align 8
  %local_17 = alloca %SplitMode, align 8
  %local_18 = alloca %Scanner, align 8
  %local_19 = alloca %Scanner, align 8
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %hew_vec_new_str_call = call ptr @hew_vec_new_str()
  store ptr %hew_vec_new_str_call, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %local_2, align 8
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result = call i32 @hew_string_length(ptr %call_arg)
  %ffi_sext = sext i32 %call_result to i64
  store i64 %ffi_sext, ptr %local_4, align 8
  br label %bb2

bb2:                                              ; preds = %bb1
  store i64 0, ptr %local_5, align 8
  %cmp_lhs = load i64, ptr %local_4, align 8
  %cmp_rhs = load i64, ptr %local_5, align 8
  %cmp_bit = icmp eq i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_6, align 1
  %cond_load = load i8, ptr %local_6, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb3, label %bb4

bb3:                                              ; preds = %bb2
  %move_load1 = load ptr, ptr %local_2, align 8
  store ptr %move_load1, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

bb4:                                              ; preds = %bb2
  br label %bb5

bb5:                                              ; preds = %after_cooperate7, %bb4
  %call_arg2 = load ptr, ptr %local_0, align 8
  %call_result3 = call %Scanner @"scanner$from_string"(ptr %call_arg2)
  store %Scanner %call_result3, ptr %local_7, align 8
  br label %bb7

bb6:                                              ; No predecessors!
  %hew_actor_cooperate4 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel5 = icmp eq i32 %hew_actor_cooperate4, 2
  br i1 %hew_cooperate_is_cancel5, label %cancel_exit6, label %after_cooperate7

bb7:                                              ; preds = %bb5
  %move_load8 = load %Scanner, ptr %local_7, align 8
  store %Scanner %move_load8, ptr %local_8, align 8
  store i64 1, ptr %local_10, align 8
  %machine_tag_ptr = getelementptr inbounds nuw %SplitMode, ptr %local_9, i32 0, i32 0
  %move_iN_load_wide = load i64, ptr %local_10, align 8
  %move_iN_trunc = trunc i64 %move_iN_load_wide to i8
  store i8 %move_iN_trunc, ptr %machine_tag_ptr, align 1
  %move_load9 = load %Scanner, ptr %local_8, align 8
  store %Scanner %move_load9, ptr %local_16, align 8
  store %Scanner zeroinitializer, ptr %local_8, align 8
  %move_load10 = load %SplitMode, ptr %local_9, align 1
  store %SplitMode %move_load10, ptr %local_17, align 1
  store %SplitMode zeroinitializer, ptr %local_9, align 1
  %call_arg11 = load %Scanner, ptr %local_16, align 8
  %call_arg12 = load %SplitMode, ptr %local_17, align 1
  %call_result13 = call %Scanner @"scanner$with_split"(%Scanner %call_arg11, %SplitMode %call_arg12)
  store %Scanner %call_result13, ptr %local_11, align 8
  br label %bb8

bb8:                                              ; preds = %bb7
  %move_load14 = load %Scanner, ptr %local_11, align 8
  store %Scanner %move_load14, ptr %local_8, align 8
  %move_load15 = load %Scanner, ptr %local_8, align 8
  store %Scanner %move_load15, ptr %local_18, align 8
  store %Scanner zeroinitializer, ptr %local_8, align 8
  %call_arg16 = load %Scanner, ptr %local_18, align 8
  %call_result17 = call %Scanner @"scanner$scan"(%Scanner %call_arg16)
  store %Scanner %call_result17, ptr %local_12, align 8
  br label %bb9

bb9:                                              ; preds = %bb8
  %move_load18 = load %Scanner, ptr %local_12, align 8
  store %Scanner %move_load18, ptr %local_8, align 8
  br label %bb10

bb10:                                             ; preds = %after_cooperate34, %bb9
  %call_arg19 = load %Scanner, ptr %local_8, align 8
  %call_result20 = call i8 @"scanner$has_next"(%Scanner %call_arg19)
  store i8 %call_result20, ptr %local_13, align 1
  br label %bb13

bb11:                                             ; preds = %bb13
  %field_2_load_ptr = getelementptr inbounds nuw %Scanner, ptr %local_8, i32 0, i32 2
  %field_2_load = load ptr, ptr %field_2_load_ptr, align 8
  %field_2_str_retain = call ptr @hew_string_clone(ptr %field_2_load)
  store ptr %field_2_str_retain, ptr %local_14, align 8
  %call_arg21 = load ptr, ptr %local_2, align 8
  %call_arg22 = load ptr, ptr %local_14, align 8
  call void @hew_vec_push_str(ptr %call_arg21, ptr %call_arg22)
  br label %bb14

bb12:                                             ; preds = %bb13
  %move_load23 = load ptr, ptr %local_2, align 8
  store ptr %move_load23, ptr %return_slot, align 8
  %ret_val24 = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val24

bb13:                                             ; preds = %bb10
  %cond_load25 = load i8, ptr %local_13, align 1
  %cond_nz26 = icmp ne i8 %cond_load25, 0
  br i1 %cond_nz26, label %bb11, label %bb12

bb14:                                             ; preds = %bb11
  %"hew_string_drop drop" = load ptr, ptr %local_14, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop")
  store ptr null, ptr %local_14, align 8
  %move_load27 = load %Scanner, ptr %local_8, align 8
  store %Scanner %move_load27, ptr %local_19, align 8
  store %Scanner zeroinitializer, ptr %local_8, align 8
  %call_arg28 = load %Scanner, ptr %local_19, align 8
  %call_result29 = call %Scanner @"scanner$scan"(%Scanner %call_arg28)
  store %Scanner %call_result29, ptr %local_15, align 8
  br label %bb15

bb15:                                             ; preds = %bb14
  %move_load30 = load %Scanner, ptr %local_15, align 8
  store %Scanner %move_load30, ptr %local_8, align 8
  %hew_actor_cooperate31 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel32 = icmp eq i32 %hew_actor_cooperate31, 2
  br i1 %hew_cooperate_is_cancel32, label %cancel_exit33, label %after_cooperate34

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit6:                                     ; preds = %bb6
  ret ptr null

after_cooperate7:                                 ; preds = %bb6
  br label %bb5

cancel_exit33:                                    ; preds = %bb15
  ret ptr null

after_cooperate34:                                ; preds = %bb15
  br label %bb10
}

define internal ptr @"scanner$collect"(%Scanner %0) {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca %Scanner, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca ptr, align 8
  %local_3 = alloca i8, align 1
  %local_4 = alloca i8, align 1
  %local_5 = alloca ptr, align 8
  %local_6 = alloca %Scanner, align 8
  %local_7 = alloca %Scanner, align 8
  %local_8 = alloca %Scanner, align 8
  %local_9 = alloca i8, align 1
  %local_10 = alloca ptr, align 8
  %local_11 = alloca %Scanner, align 8
  %local_12 = alloca %Scanner, align 8
  %local_13 = alloca %Scanner, align 8
  store %Scanner %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %hew_vec_new_str_call = call ptr @hew_vec_new_str()
  store ptr %hew_vec_new_str_call, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %local_2, align 8
  %call_arg = load %Scanner, ptr %local_0, align 8
  %call_result = call i8 @"scanner$has_next"(%Scanner %call_arg)
  store i8 %call_result, ptr %local_4, align 1
  br label %bb2

bb2:                                              ; preds = %bb1
  %cond_load = load i8, ptr %local_4, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb3, label %bb4

bb3:                                              ; preds = %bb2
  %field_2_load_ptr = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 2
  %field_2_load = load ptr, ptr %field_2_load_ptr, align 8
  %field_2_str_retain = call ptr @hew_string_clone(ptr %field_2_load)
  store ptr %field_2_str_retain, ptr %local_5, align 8
  %call_arg1 = load ptr, ptr %local_2, align 8
  %call_arg2 = load ptr, ptr %local_5, align 8
  call void @hew_vec_push_str(ptr %call_arg1, ptr %call_arg2)
  br label %bb6

bb4:                                              ; preds = %bb2
  br label %bb5

bb5:                                              ; preds = %after_cooperate11, %bb4
  %move_load3 = load %Scanner, ptr %local_0, align 8
  store %Scanner %move_load3, ptr %local_6, align 8
  store %Scanner zeroinitializer, ptr %local_0, align 8
  %move_load4 = load %Scanner, ptr %local_6, align 8
  store %Scanner %move_load4, ptr %local_7, align 8
  %move_load5 = load %Scanner, ptr %local_7, align 8
  store %Scanner %move_load5, ptr %local_12, align 8
  store %Scanner zeroinitializer, ptr %local_7, align 8
  %call_arg6 = load %Scanner, ptr %local_12, align 8
  %call_result7 = call %Scanner @"scanner$scan"(%Scanner %call_arg6)
  store %Scanner %call_result7, ptr %local_8, align 8
  br label %bb7

bb6:                                              ; preds = %bb3
  %"hew_string_drop drop" = load ptr, ptr %local_5, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop")
  store ptr null, ptr %local_5, align 8
  %hew_actor_cooperate8 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel9 = icmp eq i32 %hew_actor_cooperate8, 2
  br i1 %hew_cooperate_is_cancel9, label %cancel_exit10, label %after_cooperate11

bb7:                                              ; preds = %bb5
  %move_load12 = load %Scanner, ptr %local_8, align 8
  store %Scanner %move_load12, ptr %local_7, align 8
  br label %bb8

bb8:                                              ; preds = %after_cooperate31, %bb7
  %call_arg13 = load %Scanner, ptr %local_7, align 8
  %call_result14 = call i8 @"scanner$has_next"(%Scanner %call_arg13)
  store i8 %call_result14, ptr %local_9, align 1
  br label %bb11

bb9:                                              ; preds = %bb11
  %field_2_load_ptr15 = getelementptr inbounds nuw %Scanner, ptr %local_7, i32 0, i32 2
  %field_2_load16 = load ptr, ptr %field_2_load_ptr15, align 8
  %field_2_str_retain17 = call ptr @hew_string_clone(ptr %field_2_load16)
  store ptr %field_2_str_retain17, ptr %local_10, align 8
  %call_arg18 = load ptr, ptr %local_2, align 8
  %call_arg19 = load ptr, ptr %local_10, align 8
  call void @hew_vec_push_str(ptr %call_arg18, ptr %call_arg19)
  br label %bb12

bb10:                                             ; preds = %bb11
  %move_load20 = load ptr, ptr %local_2, align 8
  store ptr %move_load20, ptr %return_slot, align 8
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

bb11:                                             ; preds = %bb8
  %cond_load21 = load i8, ptr %local_9, align 1
  %cond_nz22 = icmp ne i8 %cond_load21, 0
  br i1 %cond_nz22, label %bb9, label %bb10

bb12:                                             ; preds = %bb9
  %"hew_string_drop drop23" = load ptr, ptr %local_10, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop23")
  store ptr null, ptr %local_10, align 8
  %move_load24 = load %Scanner, ptr %local_7, align 8
  store %Scanner %move_load24, ptr %local_13, align 8
  store %Scanner zeroinitializer, ptr %local_7, align 8
  %call_arg25 = load %Scanner, ptr %local_13, align 8
  %call_result26 = call %Scanner @"scanner$scan"(%Scanner %call_arg25)
  store %Scanner %call_result26, ptr %local_11, align 8
  br label %bb13

bb13:                                             ; preds = %bb12
  %move_load27 = load %Scanner, ptr %local_11, align 8
  store %Scanner %move_load27, ptr %local_7, align 8
  %hew_actor_cooperate28 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel29 = icmp eq i32 %hew_actor_cooperate28, 2
  br i1 %hew_cooperate_is_cancel29, label %cancel_exit30, label %after_cooperate31

cancel_exit:                                      ; preds = %entry
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit10:                                    ; preds = %bb6
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  ret ptr null

after_cooperate11:                                ; preds = %bb6
  br label %bb5

cancel_exit30:                                    ; preds = %bb13
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  ret ptr null

after_cooperate31:                                ; preds = %bb13
  br label %bb8
}

define internal %Scanner @"scanner$scan_line"(%Scanner %0) {
entry:
  %return_slot = alloca %Scanner, align 8
  %local_0 = alloca %Scanner, align 8
  %local_1 = alloca i8, align 1
  %local_2 = alloca i8, align 1
  %local_3 = alloca %Scanner, align 8
  %local_4 = alloca ptr, align 8
  %local_5 = alloca i64, align 8
  %local_6 = alloca i64, align 8
  %local_7 = alloca i64, align 8
  %local_8 = alloca i64, align 8
  %local_9 = alloca i8, align 1
  %local_10 = alloca i8, align 1
  %local_11 = alloca ptr, align 8
  %local_12 = alloca ptr, align 8
  %local_13 = alloca i8, align 1
  %local_14 = alloca i8, align 1
  %local_15 = alloca %SplitMode, align 8
  %local_16 = alloca %Scanner, align 8
  %local_17 = alloca ptr, align 8
  %local_18 = alloca ptr, align 8
  %local_19 = alloca ptr, align 8
  %local_20 = alloca ptr, align 8
  %local_21 = alloca %"Option$$i64", align 8
  %local_22 = alloca %"Option$$i64", align 8
  %local_23 = alloca %Scanner, align 8
  %local_24 = alloca i8, align 1
  %local_25 = alloca i64, align 8
  %local_26 = alloca i64, align 8
  %local_27 = alloca i8, align 1
  %local_28 = alloca i64, align 8
  %local_29 = alloca i8, align 1
  %local_30 = alloca i8, align 1
  %local_31 = alloca i8, align 1
  %local_32 = alloca ptr, align 8
  %local_33 = alloca ptr, align 8
  %local_34 = alloca ptr, align 8
  %local_35 = alloca ptr, align 8
  %local_36 = alloca i8, align 1
  %local_37 = alloca i8, align 1
  %local_38 = alloca %SplitMode, align 8
  %local_39 = alloca %Scanner, align 8
  %local_40 = alloca %Scanner, align 8
  %local_41 = alloca i64, align 8
  %local_42 = alloca i64, align 8
  %local_43 = alloca i64, align 8
  %local_44 = alloca i8, align 1
  %local_45 = alloca i64, align 8
  %local_46 = alloca i8, align 1
  %local_47 = alloca i64, align 8
  %local_48 = alloca i64, align 8
  %local_49 = alloca i64, align 8
  %local_50 = alloca i8, align 1
  %local_51 = alloca i64, align 8
  %local_52 = alloca i64, align 8
  %local_53 = alloca i8, align 1
  %local_54 = alloca i8, align 1
  %local_55 = alloca ptr, align 8
  %local_56 = alloca i64, align 8
  %local_57 = alloca i64, align 8
  %local_58 = alloca i8, align 1
  %local_59 = alloca ptr, align 8
  %local_60 = alloca ptr, align 8
  %local_61 = alloca i8, align 1
  %local_62 = alloca i64, align 8
  %local_63 = alloca i64, align 8
  %local_64 = alloca i8, align 1
  %local_65 = alloca i64, align 8
  %local_66 = alloca i64, align 8
  %local_67 = alloca i64, align 8
  %local_68 = alloca ptr, align 8
  %local_69 = alloca ptr, align 8
  %local_70 = alloca ptr, align 8
  %local_71 = alloca ptr, align 8
  %local_72 = alloca i64, align 8
  %local_73 = alloca i64, align 8
  %local_74 = alloca i8, align 1
  %local_75 = alloca i8, align 1
  %local_76 = alloca i8, align 1
  %local_77 = alloca %SplitMode, align 8
  %local_78 = alloca %Scanner, align 8
  %local_79 = alloca %Scanner, align 8
  store %Scanner %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %field_4_load_ptr = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 4
  %field_4_load = load i8, ptr %field_4_load_ptr, align 1
  store i8 %field_4_load, ptr %local_2, align 1
  %cond_load = load i8, ptr %local_2, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  %move_load = load %Scanner, ptr %local_0, align 8
  store %Scanner %move_load, ptr %local_3, align 8
  store %Scanner zeroinitializer, ptr %local_0, align 8
  %move_load1 = load %Scanner, ptr %local_3, align 8
  store %Scanner %move_load1, ptr %return_slot, align 8
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  %ret_val = load %Scanner, ptr %return_slot, align 8
  ret %Scanner %ret_val

bb2:                                              ; preds = %bb0
  br label %bb3

bb3:                                              ; preds = %after_cooperate5, %bb2
  %field_0_load_ptr = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 0
  %field_0_load = load ptr, ptr %field_0_load_ptr, align 8
  %field_0_str_retain = call ptr @hew_string_clone(ptr %field_0_load)
  store ptr %field_0_str_retain, ptr %local_4, align 8
  %call_arg = load ptr, ptr %local_4, align 8
  %call_result = call i32 @hew_string_length(ptr %call_arg)
  %ffi_sext = sext i32 %call_result to i64
  store i64 %ffi_sext, ptr %local_5, align 8
  br label %bb5

bb4:                                              ; No predecessors!
  %hew_actor_cooperate2 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel3 = icmp eq i32 %hew_actor_cooperate2, 2
  br i1 %hew_cooperate_is_cancel3, label %cancel_exit4, label %after_cooperate5

bb5:                                              ; preds = %bb3
  %"hew_string_drop drop" = load ptr, ptr %local_4, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop")
  store ptr null, ptr %local_4, align 8
  %move_load6 = load i64, ptr %local_5, align 8
  store i64 %move_load6, ptr %local_6, align 8
  %field_1_load_ptr = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 1
  %field_1_load = load i64, ptr %field_1_load_ptr, align 8
  store i64 %field_1_load, ptr %local_7, align 8
  %move_load7 = load i64, ptr %local_7, align 8
  store i64 %move_load7, ptr %local_8, align 8
  %cmp_lhs = load i64, ptr %local_8, align 8
  %cmp_rhs = load i64, ptr %local_6, align 8
  %cmp_bit = icmp sge i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_10, align 1
  %cond_load8 = load i8, ptr %local_10, align 1
  %cond_nz9 = icmp ne i8 %cond_load8, 0
  br i1 %cond_nz9, label %bb6, label %bb7

bb6:                                              ; preds = %bb5
  %field_0_load_ptr10 = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 0
  %field_0_load11 = load ptr, ptr %field_0_load_ptr10, align 8
  %field_0_str_retain12 = call ptr @hew_string_clone(ptr %field_0_load11)
  store ptr %field_0_str_retain12, ptr %local_11, align 8
  store ptr @str_lit.31, ptr %local_12, align 8
  store i8 0, ptr %local_13, align 1
  store i8 1, ptr %local_14, align 1
  %field_5_load_ptr = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 5
  %field_5_load = load %SplitMode, ptr %field_5_load_ptr, align 1
  store %SplitMode %field_5_load, ptr %local_15, align 1
  %field_0_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_16, i32 0, i32 0
  %field_0_init_src = load ptr, ptr %local_11, align 8
  store ptr %field_0_init_src, ptr %field_0_init_ptr, align 8
  %field_1_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_16, i32 0, i32 1
  %field_1_init_src = load i64, ptr %local_8, align 8
  store i64 %field_1_init_src, ptr %field_1_init_ptr, align 8
  %field_2_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_16, i32 0, i32 2
  %field_2_init_src = load ptr, ptr %local_12, align 8
  store ptr %field_2_init_src, ptr %field_2_init_ptr, align 8
  %field_3_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_16, i32 0, i32 3
  %field_3_init_src = load i8, ptr %local_13, align 1
  store i8 %field_3_init_src, ptr %field_3_init_ptr, align 1
  %field_4_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_16, i32 0, i32 4
  %field_4_init_src = load i8, ptr %local_14, align 1
  store i8 %field_4_init_src, ptr %field_4_init_ptr, align 1
  %field_5_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_16, i32 0, i32 5
  %field_5_init_src = load %SplitMode, ptr %local_15, align 1
  store %SplitMode %field_5_init_src, ptr %field_5_init_ptr, align 1
  %move_load13 = load %Scanner, ptr %local_16, align 8
  store %Scanner %move_load13, ptr %return_slot, align 8
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  %ret_val14 = load %Scanner, ptr %return_slot, align 8
  ret %Scanner %ret_val14

bb7:                                              ; preds = %bb5
  br label %bb8

bb8:                                              ; preds = %after_cooperate25, %bb7
  %field_0_load_ptr15 = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 0
  %field_0_load16 = load ptr, ptr %field_0_load_ptr15, align 8
  %field_0_str_retain17 = call ptr @hew_string_clone(ptr %field_0_load16)
  store ptr %field_0_str_retain17, ptr %local_17, align 8
  %call_arg18 = load ptr, ptr %local_17, align 8
  %call_arg19 = load i64, ptr %local_8, align 8
  %call_arg20 = load i64, ptr %local_6, align 8
  %call_result21 = call ptr @hew_string_slice(ptr %call_arg18, i64 %call_arg19, i64 %call_arg20)
  store ptr %call_result21, ptr %local_18, align 8
  br label %bb10

bb9:                                              ; No predecessors!
  %hew_actor_cooperate22 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel23 = icmp eq i32 %hew_actor_cooperate22, 2
  br i1 %hew_cooperate_is_cancel23, label %cancel_exit24, label %after_cooperate25

bb10:                                             ; preds = %bb8
  %"hew_string_drop drop26" = load ptr, ptr %local_17, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop26")
  store ptr null, ptr %local_17, align 8
  %move_load27 = load ptr, ptr %local_18, align 8
  store ptr %move_load27, ptr %local_19, align 8
  store ptr @str_lit.32, ptr %local_20, align 8
  %"hew_string_find arg0" = load ptr, ptr %local_19, align 8
  %"hew_string_find arg1" = load ptr, ptr %local_20, align 8
  %hew_string_find_call = call i32 @hew_string_find(ptr %"hew_string_find arg0", ptr %"hew_string_find arg1")
  %string_sentinel_is_some = icmp sge i32 %hew_string_find_call, 0
  br i1 %string_sentinel_is_some, label %string_sentinel_some, label %string_sentinel_none

bb11:                                             ; preds = %string_sentinel_none, %string_sentinel_some
  %move_load29 = load %"Option$$i64", ptr %local_21, align 8
  store %"Option$$i64" %move_load29, ptr %local_22, align 8
  %machine_tag_ptr30 = getelementptr inbounds nuw %"Option$$i64", ptr %local_22, i32 0, i32 0
  %move_iN_load = load i8, ptr %machine_tag_ptr30, align 1
  %move_iN_zext = zext i8 %move_iN_load to i64
  store i64 %move_iN_zext, ptr %local_25, align 8
  store i64 0, ptr %local_26, align 8
  %cmp_lhs31 = load i64, ptr %local_25, align 8
  %cmp_rhs32 = load i64, ptr %local_26, align 8
  %cmp_bit33 = icmp eq i64 %cmp_lhs31, %cmp_rhs32
  %cmp_zext34 = zext i1 %cmp_bit33 to i8
  store i8 %cmp_zext34, ptr %local_27, align 1
  %cond_load35 = load i8, ptr %local_27, align 1
  %cond_nz36 = icmp ne i8 %cond_load35, 0
  br i1 %cond_nz36, label %bb13, label %bb16

bb12:                                             ; preds = %after_cooperate49, %after_cooperate43
  %cond_load37 = load i8, ptr %local_24, align 1
  %cond_nz38 = icmp ne i8 %cond_load37, 0
  br i1 %cond_nz38, label %bb17, label %bb18

bb13:                                             ; preds = %bb11
  store i8 0, ptr %local_30, align 1
  %move_load39 = load i8, ptr %local_30, align 1
  store i8 %move_load39, ptr %local_24, align 1
  %hew_actor_cooperate40 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel41 = icmp eq i32 %hew_actor_cooperate40, 2
  br i1 %hew_cooperate_is_cancel41, label %cancel_exit42, label %after_cooperate43

bb14:                                             ; preds = %bb16
  store i8 1, ptr %local_31, align 1
  %move_load45 = load i8, ptr %local_31, align 1
  store i8 %move_load45, ptr %local_24, align 1
  %hew_actor_cooperate46 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel47 = icmp eq i32 %hew_actor_cooperate46, 2
  br i1 %hew_cooperate_is_cancel47, label %cancel_exit48, label %after_cooperate49

bb15:                                             ; preds = %bb16
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  %"hew_string_drop drop51" = load ptr, ptr %local_19, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop51")
  store ptr null, ptr %local_19, align 8
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

bb16:                                             ; preds = %bb11
  store i64 1, ptr %local_28, align 8
  %cmp_lhs52 = load i64, ptr %local_25, align 8
  %cmp_rhs53 = load i64, ptr %local_28, align 8
  %cmp_bit54 = icmp eq i64 %cmp_lhs52, %cmp_rhs53
  %cmp_zext55 = zext i1 %cmp_bit54 to i8
  store i8 %cmp_zext55, ptr %local_29, align 1
  %cond_load56 = load i8, ptr %local_29, align 1
  %cond_nz57 = icmp ne i8 %cond_load56, 0
  br i1 %cond_nz57, label %bb14, label %bb15

bb17:                                             ; preds = %bb12
  %field_0_load_ptr58 = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 0
  %field_0_load59 = load ptr, ptr %field_0_load_ptr58, align 8
  %field_0_str_retain60 = call ptr @hew_string_clone(ptr %field_0_load59)
  store ptr %field_0_str_retain60, ptr %local_32, align 8
  %call_arg61 = load ptr, ptr %local_32, align 8
  %call_arg62 = load i64, ptr %local_8, align 8
  %call_arg63 = load i64, ptr %local_6, align 8
  %call_result64 = call ptr @hew_string_slice(ptr %call_arg61, i64 %call_arg62, i64 %call_arg63)
  store ptr %call_result64, ptr %local_33, align 8
  br label %bb20

bb18:                                             ; preds = %bb12
  %machine_tag_ptr65 = getelementptr inbounds nuw %"Option$$i64", ptr %local_22, i32 0, i32 0
  %move_iN_load66 = load i8, ptr %machine_tag_ptr65, align 1
  %move_iN_zext67 = zext i8 %move_iN_load66 to i64
  store i64 %move_iN_zext67, ptr %local_42, align 8
  store i64 0, ptr %local_43, align 8
  %cmp_lhs68 = load i64, ptr %local_42, align 8
  %cmp_rhs69 = load i64, ptr %local_43, align 8
  %cmp_bit70 = icmp eq i64 %cmp_lhs68, %cmp_rhs69
  %cmp_zext71 = zext i1 %cmp_bit70 to i8
  store i8 %cmp_zext71, ptr %local_44, align 1
  %cond_load72 = load i8, ptr %local_44, align 1
  %cond_nz73 = icmp ne i8 %cond_load72, 0
  br i1 %cond_nz73, label %bb22, label %bb25

bb19:                                             ; preds = %after_cooperate219, %after_cooperate101
  %move_load74 = load %Scanner, ptr %local_23, align 8
  store %Scanner %move_load74, ptr %return_slot, align 8
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  %"hew_string_drop drop75" = load ptr, ptr %local_19, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop75")
  store ptr null, ptr %local_19, align 8
  %ret_val76 = load %Scanner, ptr %return_slot, align 8
  ret %Scanner %ret_val76

bb20:                                             ; preds = %bb17
  %"hew_string_drop drop77" = load ptr, ptr %local_32, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop77")
  store ptr null, ptr %local_32, align 8
  %move_load78 = load ptr, ptr %local_33, align 8
  store ptr %move_load78, ptr %local_34, align 8
  %field_0_load_ptr79 = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 0
  %field_0_load80 = load ptr, ptr %field_0_load_ptr79, align 8
  %field_0_str_retain81 = call ptr @hew_string_clone(ptr %field_0_load80)
  store ptr %field_0_str_retain81, ptr %local_35, align 8
  store i8 1, ptr %local_36, align 1
  store i8 0, ptr %local_37, align 1
  %field_5_load_ptr82 = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 5
  %field_5_load83 = load %SplitMode, ptr %field_5_load_ptr82, align 1
  store %SplitMode %field_5_load83, ptr %local_38, align 1
  %field_0_init_ptr84 = getelementptr inbounds nuw %Scanner, ptr %local_39, i32 0, i32 0
  %field_0_init_src85 = load ptr, ptr %local_35, align 8
  store ptr %field_0_init_src85, ptr %field_0_init_ptr84, align 8
  %field_1_init_ptr86 = getelementptr inbounds nuw %Scanner, ptr %local_39, i32 0, i32 1
  %field_1_init_src87 = load i64, ptr %local_6, align 8
  store i64 %field_1_init_src87, ptr %field_1_init_ptr86, align 8
  %field_2_init_ptr88 = getelementptr inbounds nuw %Scanner, ptr %local_39, i32 0, i32 2
  %field_2_init_src89 = load ptr, ptr %local_34, align 8
  store ptr %field_2_init_src89, ptr %field_2_init_ptr88, align 8
  %field_3_init_ptr90 = getelementptr inbounds nuw %Scanner, ptr %local_39, i32 0, i32 3
  %field_3_init_src91 = load i8, ptr %local_36, align 1
  store i8 %field_3_init_src91, ptr %field_3_init_ptr90, align 1
  %field_4_init_ptr92 = getelementptr inbounds nuw %Scanner, ptr %local_39, i32 0, i32 4
  %field_4_init_src93 = load i8, ptr %local_37, align 1
  store i8 %field_4_init_src93, ptr %field_4_init_ptr92, align 1
  %field_5_init_ptr94 = getelementptr inbounds nuw %Scanner, ptr %local_39, i32 0, i32 5
  %field_5_init_src95 = load %SplitMode, ptr %local_38, align 1
  store %SplitMode %field_5_init_src95, ptr %field_5_init_ptr94, align 1
  %move_load96 = load %Scanner, ptr %local_39, align 8
  store %Scanner %move_load96, ptr %local_40, align 8
  %move_load97 = load %Scanner, ptr %local_40, align 8
  store %Scanner %move_load97, ptr %local_23, align 8
  %hew_actor_cooperate98 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel99 = icmp eq i32 %hew_actor_cooperate98, 2
  br i1 %hew_cooperate_is_cancel99, label %cancel_exit100, label %after_cooperate101

bb21:                                             ; preds = %after_cooperate118, %after_cooperate112
  %checked_lhs = load i64, ptr %local_8, align 8
  %checked_rhs = load i64, ptr %local_41, align 8
  %with_overflow = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_49, align 8
  store i8 %checked_overflow_widen, ptr %local_50, align 1
  %cond_load103 = load i8, ptr %local_50, align 1
  %cond_nz104 = icmp ne i8 %cond_load103, 0
  br i1 %cond_nz104, label %bb26, label %bb27

bb22:                                             ; preds = %bb18
  %machine_payload_ptr105 = getelementptr inbounds nuw %"Option$$i64", ptr %local_22, i32 0, i32 1
  %machine_variant_field_ptr106 = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr105, i32 0, i32 0
  %move_load107 = load i64, ptr %machine_variant_field_ptr106, align 8
  store i64 %move_load107, ptr %local_47, align 8
  %move_load108 = load i64, ptr %local_47, align 8
  store i64 %move_load108, ptr %local_41, align 8
  %hew_actor_cooperate109 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel110 = icmp eq i32 %hew_actor_cooperate109, 2
  br i1 %hew_cooperate_is_cancel110, label %cancel_exit111, label %after_cooperate112

bb23:                                             ; preds = %bb25
  store i64 0, ptr %local_48, align 8
  %move_load114 = load i64, ptr %local_48, align 8
  store i64 %move_load114, ptr %local_41, align 8
  %hew_actor_cooperate115 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel116 = icmp eq i32 %hew_actor_cooperate115, 2
  br i1 %hew_cooperate_is_cancel116, label %cancel_exit117, label %after_cooperate118

bb24:                                             ; preds = %bb25
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  %"hew_string_drop drop120" = load ptr, ptr %local_19, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop120")
  store ptr null, ptr %local_19, align 8
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

bb25:                                             ; preds = %bb18
  store i64 1, ptr %local_45, align 8
  %cmp_lhs121 = load i64, ptr %local_42, align 8
  %cmp_rhs122 = load i64, ptr %local_45, align 8
  %cmp_bit123 = icmp eq i64 %cmp_lhs121, %cmp_rhs122
  %cmp_zext124 = zext i1 %cmp_bit123 to i8
  store i8 %cmp_zext124, ptr %local_46, align 1
  %cond_load125 = load i8, ptr %local_46, align 1
  %cond_nz126 = icmp ne i8 %cond_load125, 0
  br i1 %cond_nz126, label %bb23, label %bb24

bb26:                                             ; preds = %bb21
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  %"hew_string_drop drop127" = load ptr, ptr %local_19, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop127")
  store ptr null, ptr %local_19, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb27:                                             ; preds = %bb21
  %move_load128 = load i64, ptr %local_49, align 8
  store i64 %move_load128, ptr %local_51, align 8
  store i8 0, ptr %local_53, align 1
  %cmp_lhs129 = load i64, ptr %local_51, align 8
  %cmp_rhs130 = load i64, ptr %local_8, align 8
  %cmp_bit131 = icmp sgt i64 %cmp_lhs129, %cmp_rhs130
  %cmp_zext132 = zext i1 %cmp_bit131 to i8
  store i8 %cmp_zext132, ptr %local_54, align 1
  %cond_load133 = load i8, ptr %local_54, align 1
  %cond_nz134 = icmp ne i8 %cond_load133, 0
  br i1 %cond_nz134, label %bb28, label %bb29

bb28:                                             ; preds = %bb27
  %field_0_load_ptr135 = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 0
  %field_0_load136 = load ptr, ptr %field_0_load_ptr135, align 8
  %field_0_str_retain137 = call ptr @hew_string_clone(ptr %field_0_load136)
  store ptr %field_0_str_retain137, ptr %local_55, align 8
  store i64 1, ptr %local_56, align 8
  %checked_lhs138 = load i64, ptr %local_51, align 8
  %checked_rhs139 = load i64, ptr %local_56, align 8
  %with_overflow140 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %checked_lhs138, i64 %checked_rhs139)
  %checked_result141 = extractvalue { i64, i1 } %with_overflow140, 0
  %checked_overflow142 = extractvalue { i64, i1 } %with_overflow140, 1
  %checked_overflow_widen143 = zext i1 %checked_overflow142 to i8
  store i64 %checked_result141, ptr %local_57, align 8
  store i8 %checked_overflow_widen143, ptr %local_58, align 1
  %cond_load144 = load i8, ptr %local_58, align 1
  %cond_nz145 = icmp ne i8 %cond_load144, 0
  br i1 %cond_nz145, label %bb30, label %bb31

bb29:                                             ; preds = %after_cooperate158, %bb27
  %cond_load146 = load i8, ptr %local_53, align 1
  %cond_nz147 = icmp ne i8 %cond_load146, 0
  br i1 %cond_nz147, label %bb33, label %bb34

bb30:                                             ; preds = %bb28
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  %"hew_string_drop drop148" = load ptr, ptr %local_19, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop148")
  store ptr null, ptr %local_19, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb31:                                             ; preds = %bb28
  %call_arg149 = load ptr, ptr %local_55, align 8
  %call_arg150 = load i64, ptr %local_57, align 8
  %call_arg151 = load i64, ptr %local_51, align 8
  %call_result152 = call ptr @hew_string_slice(ptr %call_arg149, i64 %call_arg150, i64 %call_arg151)
  store ptr %call_result152, ptr %local_59, align 8
  br label %bb32

bb32:                                             ; preds = %bb31
  store ptr @str_lit.33, ptr %local_60, align 8
  %string_cmp_lhs = load ptr, ptr %local_59, align 8
  %string_cmp_rhs = load ptr, ptr %local_60, align 8
  %hew_string_equals = call i32 @hew_string_equals(ptr %string_cmp_lhs, ptr %string_cmp_rhs)
  %string_cmp_bit = icmp ne i32 %hew_string_equals, 0
  %string_cmp_zext = zext i1 %string_cmp_bit to i8
  store i8 %string_cmp_zext, ptr %local_61, align 1
  %"hew_string_drop drop153" = load ptr, ptr %local_59, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop153")
  store ptr null, ptr %local_59, align 8
  %move_load154 = load i8, ptr %local_61, align 1
  store i8 %move_load154, ptr %local_53, align 1
  %hew_actor_cooperate155 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel156 = icmp eq i32 %hew_actor_cooperate155, 2
  br i1 %hew_cooperate_is_cancel156, label %cancel_exit157, label %after_cooperate158

bb33:                                             ; preds = %bb29
  store i64 1, ptr %local_62, align 8
  %checked_lhs160 = load i64, ptr %local_51, align 8
  %checked_rhs161 = load i64, ptr %local_62, align 8
  %with_overflow162 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %checked_lhs160, i64 %checked_rhs161)
  %checked_result163 = extractvalue { i64, i1 } %with_overflow162, 0
  %checked_overflow164 = extractvalue { i64, i1 } %with_overflow162, 1
  %checked_overflow_widen165 = zext i1 %checked_overflow164 to i8
  store i64 %checked_result163, ptr %local_63, align 8
  store i8 %checked_overflow_widen165, ptr %local_64, align 1
  %cond_load166 = load i8, ptr %local_64, align 1
  %cond_nz167 = icmp ne i8 %cond_load166, 0
  br i1 %cond_nz167, label %bb36, label %bb37

bb34:                                             ; preds = %bb29
  %move_load168 = load i64, ptr %local_51, align 8
  store i64 %move_load168, ptr %local_66, align 8
  %move_load169 = load i64, ptr %local_66, align 8
  store i64 %move_load169, ptr %local_52, align 8
  br label %bb35

bb35:                                             ; preds = %after_cooperate184, %bb34
  %move_load170 = load i64, ptr %local_52, align 8
  store i64 %move_load170, ptr %local_67, align 8
  %field_0_load_ptr171 = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 0
  %field_0_load172 = load ptr, ptr %field_0_load_ptr171, align 8
  %field_0_str_retain173 = call ptr @hew_string_clone(ptr %field_0_load172)
  store ptr %field_0_str_retain173, ptr %local_68, align 8
  %call_arg174 = load ptr, ptr %local_68, align 8
  %call_arg175 = load i64, ptr %local_8, align 8
  %call_arg176 = load i64, ptr %local_67, align 8
  %call_result177 = call ptr @hew_string_slice(ptr %call_arg174, i64 %call_arg175, i64 %call_arg176)
  store ptr %call_result177, ptr %local_69, align 8
  br label %bb38

bb36:                                             ; preds = %bb33
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  %"hew_string_drop drop178" = load ptr, ptr %local_19, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop178")
  store ptr null, ptr %local_19, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb37:                                             ; preds = %bb33
  %move_load179 = load i64, ptr %local_63, align 8
  store i64 %move_load179, ptr %local_65, align 8
  %move_load180 = load i64, ptr %local_65, align 8
  store i64 %move_load180, ptr %local_52, align 8
  %hew_actor_cooperate181 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel182 = icmp eq i32 %hew_actor_cooperate181, 2
  br i1 %hew_cooperate_is_cancel182, label %cancel_exit183, label %after_cooperate184

bb38:                                             ; preds = %bb35
  %"hew_string_drop drop186" = load ptr, ptr %local_68, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop186")
  store ptr null, ptr %local_68, align 8
  %move_load187 = load ptr, ptr %local_69, align 8
  store ptr %move_load187, ptr %local_70, align 8
  %field_0_load_ptr188 = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 0
  %field_0_load189 = load ptr, ptr %field_0_load_ptr188, align 8
  %field_0_str_retain190 = call ptr @hew_string_clone(ptr %field_0_load189)
  store ptr %field_0_str_retain190, ptr %local_71, align 8
  store i64 1, ptr %local_72, align 8
  %checked_lhs191 = load i64, ptr %local_51, align 8
  %checked_rhs192 = load i64, ptr %local_72, align 8
  %with_overflow193 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs191, i64 %checked_rhs192)
  %checked_result194 = extractvalue { i64, i1 } %with_overflow193, 0
  %checked_overflow195 = extractvalue { i64, i1 } %with_overflow193, 1
  %checked_overflow_widen196 = zext i1 %checked_overflow195 to i8
  store i64 %checked_result194, ptr %local_73, align 8
  store i8 %checked_overflow_widen196, ptr %local_74, align 1
  %cond_load197 = load i8, ptr %local_74, align 1
  %cond_nz198 = icmp ne i8 %cond_load197, 0
  br i1 %cond_nz198, label %bb39, label %bb40

bb39:                                             ; preds = %bb38
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  %"hew_string_drop drop199" = load ptr, ptr %local_19, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop199")
  store ptr null, ptr %local_19, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb40:                                             ; preds = %bb38
  store i8 1, ptr %local_75, align 1
  store i8 0, ptr %local_76, align 1
  %field_5_load_ptr200 = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 5
  %field_5_load201 = load %SplitMode, ptr %field_5_load_ptr200, align 1
  store %SplitMode %field_5_load201, ptr %local_77, align 1
  %field_0_init_ptr202 = getelementptr inbounds nuw %Scanner, ptr %local_78, i32 0, i32 0
  %field_0_init_src203 = load ptr, ptr %local_71, align 8
  store ptr %field_0_init_src203, ptr %field_0_init_ptr202, align 8
  %field_1_init_ptr204 = getelementptr inbounds nuw %Scanner, ptr %local_78, i32 0, i32 1
  %field_1_init_src205 = load i64, ptr %local_73, align 8
  store i64 %field_1_init_src205, ptr %field_1_init_ptr204, align 8
  %field_2_init_ptr206 = getelementptr inbounds nuw %Scanner, ptr %local_78, i32 0, i32 2
  %field_2_init_src207 = load ptr, ptr %local_70, align 8
  store ptr %field_2_init_src207, ptr %field_2_init_ptr206, align 8
  %field_3_init_ptr208 = getelementptr inbounds nuw %Scanner, ptr %local_78, i32 0, i32 3
  %field_3_init_src209 = load i8, ptr %local_75, align 1
  store i8 %field_3_init_src209, ptr %field_3_init_ptr208, align 1
  %field_4_init_ptr210 = getelementptr inbounds nuw %Scanner, ptr %local_78, i32 0, i32 4
  %field_4_init_src211 = load i8, ptr %local_76, align 1
  store i8 %field_4_init_src211, ptr %field_4_init_ptr210, align 1
  %field_5_init_ptr212 = getelementptr inbounds nuw %Scanner, ptr %local_78, i32 0, i32 5
  %field_5_init_src213 = load %SplitMode, ptr %local_77, align 1
  store %SplitMode %field_5_init_src213, ptr %field_5_init_ptr212, align 1
  %move_load214 = load %Scanner, ptr %local_78, align 8
  store %Scanner %move_load214, ptr %local_79, align 8
  %move_load215 = load %Scanner, ptr %local_79, align 8
  store %Scanner %move_load215, ptr %local_23, align 8
  %hew_actor_cooperate216 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel217 = icmp eq i32 %hew_actor_cooperate216, 2
  br i1 %hew_cooperate_is_cancel217, label %cancel_exit218, label %after_cooperate219

cancel_exit:                                      ; preds = %entry
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  ret %Scanner zeroinitializer

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit4:                                     ; preds = %bb4
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  ret %Scanner zeroinitializer

after_cooperate5:                                 ; preds = %bb4
  br label %bb3

cancel_exit24:                                    ; preds = %bb9
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  ret %Scanner zeroinitializer

after_cooperate25:                                ; preds = %bb9
  br label %bb8

string_sentinel_some:                             ; preds = %bb10
  %machine_payload_ptr = getelementptr inbounds nuw %"Option$$i64", ptr %local_21, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { i64 }, ptr %machine_payload_ptr, i32 0, i32 0
  %string_sentinel_payload = zext i32 %hew_string_find_call to i64
  store i64 %string_sentinel_payload, ptr %machine_variant_field_ptr, align 8
  %machine_tag_ptr = getelementptr inbounds nuw %"Option$$i64", ptr %local_21, i32 0, i32 0
  store i8 0, ptr %machine_tag_ptr, align 1
  br label %bb11

string_sentinel_none:                             ; preds = %bb10
  %machine_tag_ptr28 = getelementptr inbounds nuw %"Option$$i64", ptr %local_21, i32 0, i32 0
  store i8 1, ptr %machine_tag_ptr28, align 1
  br label %bb11

cancel_exit42:                                    ; preds = %bb13
  %"hew_string_drop drop44" = load ptr, ptr %local_19, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop44")
  store ptr null, ptr %local_19, align 8
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  ret %Scanner zeroinitializer

after_cooperate43:                                ; preds = %bb13
  br label %bb12

cancel_exit48:                                    ; preds = %bb14
  %"hew_string_drop drop50" = load ptr, ptr %local_19, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop50")
  store ptr null, ptr %local_19, align 8
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  ret %Scanner zeroinitializer

after_cooperate49:                                ; preds = %bb14
  br label %bb12

cancel_exit100:                                   ; preds = %bb20
  %"hew_string_drop drop102" = load ptr, ptr %local_19, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop102")
  store ptr null, ptr %local_19, align 8
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  ret %Scanner zeroinitializer

after_cooperate101:                               ; preds = %bb20
  br label %bb19

cancel_exit111:                                   ; preds = %bb22
  %"hew_string_drop drop113" = load ptr, ptr %local_19, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop113")
  store ptr null, ptr %local_19, align 8
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  ret %Scanner zeroinitializer

after_cooperate112:                               ; preds = %bb22
  br label %bb21

cancel_exit117:                                   ; preds = %bb23
  %"hew_string_drop drop119" = load ptr, ptr %local_19, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop119")
  store ptr null, ptr %local_19, align 8
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  ret %Scanner zeroinitializer

after_cooperate118:                               ; preds = %bb23
  br label %bb21

cancel_exit157:                                   ; preds = %bb32
  %"hew_string_drop drop159" = load ptr, ptr %local_19, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop159")
  store ptr null, ptr %local_19, align 8
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  ret %Scanner zeroinitializer

after_cooperate158:                               ; preds = %bb32
  br label %bb29

cancel_exit183:                                   ; preds = %bb37
  %"hew_string_drop drop185" = load ptr, ptr %local_19, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop185")
  store ptr null, ptr %local_19, align 8
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  ret %Scanner zeroinitializer

after_cooperate184:                               ; preds = %bb37
  br label %bb35

cancel_exit218:                                   ; preds = %bb40
  %"hew_string_drop drop220" = load ptr, ptr %local_19, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop220")
  store ptr null, ptr %local_19, align 8
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  ret %Scanner zeroinitializer

after_cooperate219:                               ; preds = %bb40
  br label %bb19
}

define internal i8 @"scanner$is_ascii_whitespace"(ptr %0) {
entry:
  %return_slot = alloca i8, align 1
  %local_0 = alloca ptr, align 8
  %local_1 = alloca i8, align 1
  %local_2 = alloca i8, align 1
  %local_3 = alloca i8, align 1
  %local_4 = alloca ptr, align 8
  %local_5 = alloca i8, align 1
  %local_6 = alloca ptr, align 8
  %local_7 = alloca i8, align 1
  %local_8 = alloca ptr, align 8
  %local_9 = alloca i8, align 1
  %local_10 = alloca ptr, align 8
  %local_11 = alloca i8, align 1
  store ptr %0, ptr %local_0, align 8
  br label %bb0

bb0:                                              ; preds = %entry
  store i8 1, ptr %local_1, align 1
  store i8 1, ptr %local_2, align 1
  store i8 1, ptr %local_3, align 1
  store ptr @str_lit.34, ptr %local_4, align 8
  %string_cmp_lhs = load ptr, ptr %local_0, align 8
  %string_cmp_rhs = load ptr, ptr %local_4, align 8
  %hew_string_equals = call i32 @hew_string_equals(ptr %string_cmp_lhs, ptr %string_cmp_rhs)
  %string_cmp_bit = icmp ne i32 %hew_string_equals, 0
  %string_cmp_zext = zext i1 %string_cmp_bit to i8
  store i8 %string_cmp_zext, ptr %local_5, align 1
  %cond_load = load i8, ptr %local_5, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb2, label %bb1

bb1:                                              ; preds = %bb0
  store ptr @str_lit.35, ptr %local_6, align 8
  %string_cmp_lhs1 = load ptr, ptr %local_0, align 8
  %string_cmp_rhs2 = load ptr, ptr %local_6, align 8
  %hew_string_equals3 = call i32 @hew_string_equals(ptr %string_cmp_lhs1, ptr %string_cmp_rhs2)
  %string_cmp_bit4 = icmp ne i32 %hew_string_equals3, 0
  %string_cmp_zext5 = zext i1 %string_cmp_bit4 to i8
  store i8 %string_cmp_zext5, ptr %local_7, align 1
  %move_load = load i8, ptr %local_7, align 1
  store i8 %move_load, ptr %local_3, align 1
  br label %bb2

bb2:                                              ; preds = %bb1, %bb0
  %cond_load6 = load i8, ptr %local_3, align 1
  %cond_nz7 = icmp ne i8 %cond_load6, 0
  br i1 %cond_nz7, label %bb4, label %bb3

bb3:                                              ; preds = %bb2
  store ptr @str_lit.36, ptr %local_8, align 8
  %string_cmp_lhs8 = load ptr, ptr %local_0, align 8
  %string_cmp_rhs9 = load ptr, ptr %local_8, align 8
  %hew_string_equals10 = call i32 @hew_string_equals(ptr %string_cmp_lhs8, ptr %string_cmp_rhs9)
  %string_cmp_bit11 = icmp ne i32 %hew_string_equals10, 0
  %string_cmp_zext12 = zext i1 %string_cmp_bit11 to i8
  store i8 %string_cmp_zext12, ptr %local_9, align 1
  %move_load13 = load i8, ptr %local_9, align 1
  store i8 %move_load13, ptr %local_2, align 1
  br label %bb4

bb4:                                              ; preds = %bb3, %bb2
  %cond_load14 = load i8, ptr %local_2, align 1
  %cond_nz15 = icmp ne i8 %cond_load14, 0
  br i1 %cond_nz15, label %bb6, label %bb5

bb5:                                              ; preds = %bb4
  store ptr @str_lit.37, ptr %local_10, align 8
  %string_cmp_lhs16 = load ptr, ptr %local_0, align 8
  %string_cmp_rhs17 = load ptr, ptr %local_10, align 8
  %hew_string_equals18 = call i32 @hew_string_equals(ptr %string_cmp_lhs16, ptr %string_cmp_rhs17)
  %string_cmp_bit19 = icmp ne i32 %hew_string_equals18, 0
  %string_cmp_zext20 = zext i1 %string_cmp_bit19 to i8
  store i8 %string_cmp_zext20, ptr %local_11, align 1
  %move_load21 = load i8, ptr %local_11, align 1
  store i8 %move_load21, ptr %local_1, align 1
  br label %bb6

bb6:                                              ; preds = %bb5, %bb4
  %move_load22 = load i8, ptr %local_1, align 1
  store i8 %move_load22, ptr %return_slot, align 1
  %ret_val = load i8, ptr %return_slot, align 1
  ret i8 %ret_val
}

define internal %Scanner @"scanner$scan_word"(%Scanner %0) {
entry:
  %return_slot = alloca %Scanner, align 8
  %local_0 = alloca %Scanner, align 8
  %local_1 = alloca i8, align 1
  %local_2 = alloca i8, align 1
  %local_3 = alloca %Scanner, align 8
  %local_4 = alloca ptr, align 8
  %local_5 = alloca i64, align 8
  %local_6 = alloca i64, align 8
  %local_7 = alloca i64, align 8
  %local_8 = alloca i64, align 8
  %local_9 = alloca i8, align 1
  %local_10 = alloca i8, align 1
  %local_11 = alloca ptr, align 8
  %local_12 = alloca i64, align 8
  %local_13 = alloca i64, align 8
  %local_14 = alloca i8, align 1
  %local_15 = alloca ptr, align 8
  %local_16 = alloca i8, align 1
  %local_17 = alloca i64, align 8
  %local_18 = alloca i64, align 8
  %local_19 = alloca i8, align 1
  %local_20 = alloca i8, align 1
  %local_21 = alloca i8, align 1
  %local_22 = alloca ptr, align 8
  %local_23 = alloca ptr, align 8
  %local_24 = alloca i8, align 1
  %local_25 = alloca i8, align 1
  %local_26 = alloca %SplitMode, align 8
  %local_27 = alloca %Scanner, align 8
  %local_28 = alloca i64, align 8
  %local_29 = alloca i8, align 1
  %local_30 = alloca i8, align 1
  %local_31 = alloca ptr, align 8
  %local_32 = alloca i64, align 8
  %local_33 = alloca i64, align 8
  %local_34 = alloca i8, align 1
  %local_35 = alloca ptr, align 8
  %local_36 = alloca i8, align 1
  %local_37 = alloca i8, align 1
  %local_38 = alloca i64, align 8
  %local_39 = alloca i64, align 8
  %local_40 = alloca i8, align 1
  %local_41 = alloca ptr, align 8
  %local_42 = alloca ptr, align 8
  %local_43 = alloca ptr, align 8
  %local_44 = alloca ptr, align 8
  %local_45 = alloca i8, align 1
  %local_46 = alloca i8, align 1
  %local_47 = alloca %SplitMode, align 8
  %local_48 = alloca %Scanner, align 8
  store %Scanner %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %field_4_load_ptr = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 4
  %field_4_load = load i8, ptr %field_4_load_ptr, align 1
  store i8 %field_4_load, ptr %local_2, align 1
  %cond_load = load i8, ptr %local_2, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  %move_load = load %Scanner, ptr %local_0, align 8
  store %Scanner %move_load, ptr %local_3, align 8
  store %Scanner zeroinitializer, ptr %local_0, align 8
  %move_load1 = load %Scanner, ptr %local_3, align 8
  store %Scanner %move_load1, ptr %return_slot, align 8
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  %ret_val = load %Scanner, ptr %return_slot, align 8
  ret %Scanner %ret_val

bb2:                                              ; preds = %bb0
  br label %bb3

bb3:                                              ; preds = %after_cooperate5, %bb2
  %field_0_load_ptr = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 0
  %field_0_load = load ptr, ptr %field_0_load_ptr, align 8
  %field_0_str_retain = call ptr @hew_string_clone(ptr %field_0_load)
  store ptr %field_0_str_retain, ptr %local_4, align 8
  %call_arg = load ptr, ptr %local_4, align 8
  %call_result = call i32 @hew_string_length(ptr %call_arg)
  %ffi_sext = sext i32 %call_result to i64
  store i64 %ffi_sext, ptr %local_5, align 8
  br label %bb5

bb4:                                              ; No predecessors!
  %hew_actor_cooperate2 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel3 = icmp eq i32 %hew_actor_cooperate2, 2
  br i1 %hew_cooperate_is_cancel3, label %cancel_exit4, label %after_cooperate5

bb5:                                              ; preds = %bb3
  %"hew_string_drop drop" = load ptr, ptr %local_4, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop")
  store ptr null, ptr %local_4, align 8
  %move_load6 = load i64, ptr %local_5, align 8
  store i64 %move_load6, ptr %local_6, align 8
  %field_1_load_ptr = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 1
  %field_1_load = load i64, ptr %field_1_load_ptr, align 8
  store i64 %field_1_load, ptr %local_7, align 8
  %move_load7 = load i64, ptr %local_7, align 8
  store i64 %move_load7, ptr %local_8, align 8
  br label %bb6

bb6:                                              ; preds = %after_cooperate48, %bb5
  store i8 0, ptr %local_9, align 1
  %cmp_lhs = load i64, ptr %local_8, align 8
  %cmp_rhs = load i64, ptr %local_6, align 8
  %cmp_bit = icmp slt i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_10, align 1
  %cond_load8 = load i8, ptr %local_10, align 1
  %cond_nz9 = icmp ne i8 %cond_load8, 0
  br i1 %cond_nz9, label %bb9, label %bb10

bb7:                                              ; preds = %bb10
  store i64 1, ptr %local_17, align 8
  %checked_lhs = load i64, ptr %local_8, align 8
  %checked_rhs = load i64, ptr %local_17, align 8
  %with_overflow = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_18, align 8
  store i8 %checked_overflow_widen, ptr %local_19, align 1
  %cond_load10 = load i8, ptr %local_19, align 1
  %cond_nz11 = icmp ne i8 %cond_load10, 0
  br i1 %cond_nz11, label %bb15, label %bb16

bb8:                                              ; preds = %bb10
  %cmp_lhs12 = load i64, ptr %local_8, align 8
  %cmp_rhs13 = load i64, ptr %local_6, align 8
  %cmp_bit14 = icmp sge i64 %cmp_lhs12, %cmp_rhs13
  %cmp_zext15 = zext i1 %cmp_bit14 to i8
  store i8 %cmp_zext15, ptr %local_21, align 1
  %cond_load16 = load i8, ptr %local_21, align 1
  %cond_nz17 = icmp ne i8 %cond_load16, 0
  br i1 %cond_nz17, label %bb17, label %bb18

bb9:                                              ; preds = %bb6
  %field_0_load_ptr18 = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 0
  %field_0_load19 = load ptr, ptr %field_0_load_ptr18, align 8
  %field_0_str_retain20 = call ptr @hew_string_clone(ptr %field_0_load19)
  store ptr %field_0_str_retain20, ptr %local_11, align 8
  store i64 1, ptr %local_12, align 8
  %checked_lhs21 = load i64, ptr %local_8, align 8
  %checked_rhs22 = load i64, ptr %local_12, align 8
  %with_overflow23 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs21, i64 %checked_rhs22)
  %checked_result24 = extractvalue { i64, i1 } %with_overflow23, 0
  %checked_overflow25 = extractvalue { i64, i1 } %with_overflow23, 1
  %checked_overflow_widen26 = zext i1 %checked_overflow25 to i8
  store i64 %checked_result24, ptr %local_13, align 8
  store i8 %checked_overflow_widen26, ptr %local_14, align 1
  %cond_load27 = load i8, ptr %local_14, align 1
  %cond_nz28 = icmp ne i8 %cond_load27, 0
  br i1 %cond_nz28, label %bb11, label %bb12

bb10:                                             ; preds = %after_cooperate41, %bb6
  %cond_load29 = load i8, ptr %local_9, align 1
  %cond_nz30 = icmp ne i8 %cond_load29, 0
  br i1 %cond_nz30, label %bb7, label %bb8

bb11:                                             ; preds = %bb9
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb12:                                             ; preds = %bb9
  %call_arg31 = load ptr, ptr %local_11, align 8
  %call_arg32 = load i64, ptr %local_8, align 8
  %call_arg33 = load i64, ptr %local_13, align 8
  %call_result34 = call ptr @hew_string_slice(ptr %call_arg31, i64 %call_arg32, i64 %call_arg33)
  store ptr %call_result34, ptr %local_15, align 8
  br label %bb13

bb13:                                             ; preds = %bb12
  %call_arg35 = load ptr, ptr %local_15, align 8
  %call_result36 = call i8 @"scanner$is_ascii_whitespace"(ptr %call_arg35)
  store i8 %call_result36, ptr %local_16, align 1
  br label %bb14

bb14:                                             ; preds = %bb13
  %move_load37 = load i8, ptr %local_16, align 1
  store i8 %move_load37, ptr %local_9, align 1
  %hew_actor_cooperate38 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel39 = icmp eq i32 %hew_actor_cooperate38, 2
  br i1 %hew_cooperate_is_cancel39, label %cancel_exit40, label %after_cooperate41

bb15:                                             ; preds = %bb7
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb16:                                             ; preds = %bb7
  %move_load44 = load i64, ptr %local_18, align 8
  store i64 %move_load44, ptr %local_8, align 8
  %hew_actor_cooperate45 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel46 = icmp eq i32 %hew_actor_cooperate45, 2
  br i1 %hew_cooperate_is_cancel46, label %cancel_exit47, label %after_cooperate48

bb17:                                             ; preds = %bb8
  %field_0_load_ptr49 = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 0
  %field_0_load50 = load ptr, ptr %field_0_load_ptr49, align 8
  %field_0_str_retain51 = call ptr @hew_string_clone(ptr %field_0_load50)
  store ptr %field_0_str_retain51, ptr %local_22, align 8
  store ptr @str_lit.38, ptr %local_23, align 8
  store i8 0, ptr %local_24, align 1
  store i8 1, ptr %local_25, align 1
  %field_5_load_ptr = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 5
  %field_5_load = load %SplitMode, ptr %field_5_load_ptr, align 1
  store %SplitMode %field_5_load, ptr %local_26, align 1
  %field_0_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_27, i32 0, i32 0
  %field_0_init_src = load ptr, ptr %local_22, align 8
  store ptr %field_0_init_src, ptr %field_0_init_ptr, align 8
  %field_1_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_27, i32 0, i32 1
  %field_1_init_src = load i64, ptr %local_8, align 8
  store i64 %field_1_init_src, ptr %field_1_init_ptr, align 8
  %field_2_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_27, i32 0, i32 2
  %field_2_init_src = load ptr, ptr %local_23, align 8
  store ptr %field_2_init_src, ptr %field_2_init_ptr, align 8
  %field_3_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_27, i32 0, i32 3
  %field_3_init_src = load i8, ptr %local_24, align 1
  store i8 %field_3_init_src, ptr %field_3_init_ptr, align 1
  %field_4_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_27, i32 0, i32 4
  %field_4_init_src = load i8, ptr %local_25, align 1
  store i8 %field_4_init_src, ptr %field_4_init_ptr, align 1
  %field_5_init_ptr = getelementptr inbounds nuw %Scanner, ptr %local_27, i32 0, i32 5
  %field_5_init_src = load %SplitMode, ptr %local_26, align 1
  store %SplitMode %field_5_init_src, ptr %field_5_init_ptr, align 1
  %move_load52 = load %Scanner, ptr %local_27, align 8
  store %Scanner %move_load52, ptr %return_slot, align 8
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  %ret_val53 = load %Scanner, ptr %return_slot, align 8
  ret %Scanner %ret_val53

bb18:                                             ; preds = %bb8
  br label %bb19

bb19:                                             ; preds = %after_cooperate58, %bb18
  %move_load54 = load i64, ptr %local_8, align 8
  store i64 %move_load54, ptr %local_28, align 8
  br label %bb21

bb20:                                             ; No predecessors!
  %hew_actor_cooperate55 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel56 = icmp eq i32 %hew_actor_cooperate55, 2
  br i1 %hew_cooperate_is_cancel56, label %cancel_exit57, label %after_cooperate58

bb21:                                             ; preds = %after_cooperate110, %bb19
  store i8 0, ptr %local_29, align 1
  %cmp_lhs59 = load i64, ptr %local_28, align 8
  %cmp_rhs60 = load i64, ptr %local_6, align 8
  %cmp_bit61 = icmp slt i64 %cmp_lhs59, %cmp_rhs60
  %cmp_zext62 = zext i1 %cmp_bit61 to i8
  store i8 %cmp_zext62, ptr %local_30, align 1
  %cond_load63 = load i8, ptr %local_30, align 1
  %cond_nz64 = icmp ne i8 %cond_load63, 0
  br i1 %cond_nz64, label %bb24, label %bb25

bb22:                                             ; preds = %bb25
  store i64 1, ptr %local_38, align 8
  %checked_lhs65 = load i64, ptr %local_28, align 8
  %checked_rhs66 = load i64, ptr %local_38, align 8
  %with_overflow67 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs65, i64 %checked_rhs66)
  %checked_result68 = extractvalue { i64, i1 } %with_overflow67, 0
  %checked_overflow69 = extractvalue { i64, i1 } %with_overflow67, 1
  %checked_overflow_widen70 = zext i1 %checked_overflow69 to i8
  store i64 %checked_result68, ptr %local_39, align 8
  store i8 %checked_overflow_widen70, ptr %local_40, align 1
  %cond_load71 = load i8, ptr %local_40, align 1
  %cond_nz72 = icmp ne i8 %cond_load71, 0
  br i1 %cond_nz72, label %bb30, label %bb31

bb23:                                             ; preds = %bb25
  %field_0_load_ptr73 = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 0
  %field_0_load74 = load ptr, ptr %field_0_load_ptr73, align 8
  %field_0_str_retain75 = call ptr @hew_string_clone(ptr %field_0_load74)
  store ptr %field_0_str_retain75, ptr %local_41, align 8
  %call_arg76 = load ptr, ptr %local_41, align 8
  %call_arg77 = load i64, ptr %local_8, align 8
  %call_arg78 = load i64, ptr %local_28, align 8
  %call_result79 = call ptr @hew_string_slice(ptr %call_arg76, i64 %call_arg77, i64 %call_arg78)
  store ptr %call_result79, ptr %local_42, align 8
  br label %bb32

bb24:                                             ; preds = %bb21
  %field_0_load_ptr80 = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 0
  %field_0_load81 = load ptr, ptr %field_0_load_ptr80, align 8
  %field_0_str_retain82 = call ptr @hew_string_clone(ptr %field_0_load81)
  store ptr %field_0_str_retain82, ptr %local_31, align 8
  store i64 1, ptr %local_32, align 8
  %checked_lhs83 = load i64, ptr %local_28, align 8
  %checked_rhs84 = load i64, ptr %local_32, align 8
  %with_overflow85 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs83, i64 %checked_rhs84)
  %checked_result86 = extractvalue { i64, i1 } %with_overflow85, 0
  %checked_overflow87 = extractvalue { i64, i1 } %with_overflow85, 1
  %checked_overflow_widen88 = zext i1 %checked_overflow87 to i8
  store i64 %checked_result86, ptr %local_33, align 8
  store i8 %checked_overflow_widen88, ptr %local_34, align 1
  %cond_load89 = load i8, ptr %local_34, align 1
  %cond_nz90 = icmp ne i8 %cond_load89, 0
  br i1 %cond_nz90, label %bb26, label %bb27

bb25:                                             ; preds = %after_cooperate103, %bb21
  %cond_load91 = load i8, ptr %local_29, align 1
  %cond_nz92 = icmp ne i8 %cond_load91, 0
  br i1 %cond_nz92, label %bb22, label %bb23

bb26:                                             ; preds = %bb24
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb27:                                             ; preds = %bb24
  %call_arg93 = load ptr, ptr %local_31, align 8
  %call_arg94 = load i64, ptr %local_28, align 8
  %call_arg95 = load i64, ptr %local_33, align 8
  %call_result96 = call ptr @hew_string_slice(ptr %call_arg93, i64 %call_arg94, i64 %call_arg95)
  store ptr %call_result96, ptr %local_35, align 8
  br label %bb28

bb28:                                             ; preds = %bb27
  %call_arg97 = load ptr, ptr %local_35, align 8
  %call_result98 = call i8 @"scanner$is_ascii_whitespace"(ptr %call_arg97)
  store i8 %call_result98, ptr %local_36, align 1
  br label %bb29

bb29:                                             ; preds = %bb28
  %boolnot_operand = load i8, ptr %local_36, align 1
  %boolnot = icmp eq i8 %boolnot_operand, 0
  %boolnot_widen = zext i1 %boolnot to i8
  store i8 %boolnot_widen, ptr %local_37, align 1
  %move_load99 = load i8, ptr %local_37, align 1
  store i8 %move_load99, ptr %local_29, align 1
  %hew_actor_cooperate100 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel101 = icmp eq i32 %hew_actor_cooperate100, 2
  br i1 %hew_cooperate_is_cancel101, label %cancel_exit102, label %after_cooperate103

bb30:                                             ; preds = %bb22
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb31:                                             ; preds = %bb22
  %move_load106 = load i64, ptr %local_39, align 8
  store i64 %move_load106, ptr %local_28, align 8
  %hew_actor_cooperate107 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel108 = icmp eq i32 %hew_actor_cooperate107, 2
  br i1 %hew_cooperate_is_cancel108, label %cancel_exit109, label %after_cooperate110

bb32:                                             ; preds = %bb23
  %"hew_string_drop drop111" = load ptr, ptr %local_41, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop111")
  store ptr null, ptr %local_41, align 8
  %move_load112 = load ptr, ptr %local_42, align 8
  store ptr %move_load112, ptr %local_43, align 8
  %field_0_load_ptr113 = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 0
  %field_0_load114 = load ptr, ptr %field_0_load_ptr113, align 8
  %field_0_str_retain115 = call ptr @hew_string_clone(ptr %field_0_load114)
  store ptr %field_0_str_retain115, ptr %local_44, align 8
  store i8 1, ptr %local_45, align 1
  store i8 0, ptr %local_46, align 1
  %field_5_load_ptr116 = getelementptr inbounds nuw %Scanner, ptr %local_0, i32 0, i32 5
  %field_5_load117 = load %SplitMode, ptr %field_5_load_ptr116, align 1
  store %SplitMode %field_5_load117, ptr %local_47, align 1
  %field_0_init_ptr118 = getelementptr inbounds nuw %Scanner, ptr %local_48, i32 0, i32 0
  %field_0_init_src119 = load ptr, ptr %local_44, align 8
  store ptr %field_0_init_src119, ptr %field_0_init_ptr118, align 8
  %field_1_init_ptr120 = getelementptr inbounds nuw %Scanner, ptr %local_48, i32 0, i32 1
  %field_1_init_src121 = load i64, ptr %local_28, align 8
  store i64 %field_1_init_src121, ptr %field_1_init_ptr120, align 8
  %field_2_init_ptr122 = getelementptr inbounds nuw %Scanner, ptr %local_48, i32 0, i32 2
  %field_2_init_src123 = load ptr, ptr %local_43, align 8
  store ptr %field_2_init_src123, ptr %field_2_init_ptr122, align 8
  %field_3_init_ptr124 = getelementptr inbounds nuw %Scanner, ptr %local_48, i32 0, i32 3
  %field_3_init_src125 = load i8, ptr %local_45, align 1
  store i8 %field_3_init_src125, ptr %field_3_init_ptr124, align 1
  %field_4_init_ptr126 = getelementptr inbounds nuw %Scanner, ptr %local_48, i32 0, i32 4
  %field_4_init_src127 = load i8, ptr %local_46, align 1
  store i8 %field_4_init_src127, ptr %field_4_init_ptr126, align 1
  %field_5_init_ptr128 = getelementptr inbounds nuw %Scanner, ptr %local_48, i32 0, i32 5
  %field_5_init_src129 = load %SplitMode, ptr %local_47, align 1
  store %SplitMode %field_5_init_src129, ptr %field_5_init_ptr128, align 1
  %move_load130 = load %Scanner, ptr %local_48, align 8
  store %Scanner %move_load130, ptr %return_slot, align 8
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  %ret_val131 = load %Scanner, ptr %return_slot, align 8
  ret %Scanner %ret_val131

cancel_exit:                                      ; preds = %entry
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  ret %Scanner zeroinitializer

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit4:                                     ; preds = %bb4
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  ret %Scanner zeroinitializer

after_cooperate5:                                 ; preds = %bb4
  br label %bb3

cancel_exit40:                                    ; preds = %bb14
  %"hew_string_drop drop42" = load ptr, ptr %local_15, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop42")
  store ptr null, ptr %local_15, align 8
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  ret %Scanner zeroinitializer

after_cooperate41:                                ; preds = %bb14
  %"hew_string_drop drop43" = load ptr, ptr %local_15, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop43")
  store ptr null, ptr %local_15, align 8
  br label %bb10

cancel_exit47:                                    ; preds = %bb16
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  ret %Scanner zeroinitializer

after_cooperate48:                                ; preds = %bb16
  br label %bb6

cancel_exit57:                                    ; preds = %bb20
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  ret %Scanner zeroinitializer

after_cooperate58:                                ; preds = %bb20
  br label %bb19

cancel_exit102:                                   ; preds = %bb29
  %"hew_string_drop drop104" = load ptr, ptr %local_35, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop104")
  store ptr null, ptr %local_35, align 8
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  ret %Scanner zeroinitializer

after_cooperate103:                               ; preds = %bb29
  %"hew_string_drop drop105" = load ptr, ptr %local_35, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop105")
  store ptr null, ptr %local_35, align 8
  br label %bb25

cancel_exit109:                                   ; preds = %bb31
  call void @__hew_record_drop_inplace_Scanner(ptr %local_0)
  ret %Scanner zeroinitializer

after_cooperate110:                               ; preds = %bb31
  br label %bb21
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
  store ptr @str_lit.39, ptr %local_3, align 8
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

define internal i32 @__hew_record_clone_inplace_Scanner(ptr %0, ptr %1) {
entry:
  br label %step_0_clone

success:                                          ; preds = %step_2_store
  ret i32 0

fail:                                             ; preds = %rb_step_2, %rb_step_1, %rb_step_0
  ret i32 1

rb_step_0:                                        ; preds = %step_0_clone
  br label %fail

rb_step_1:                                        ; preds = %step_1_clone
  %drop_f0_ptr = getelementptr inbounds nuw %Scanner, ptr %1, i32 0, i32 0
  %drop_f0 = load ptr, ptr %drop_f0_ptr, align 8
  call void @hew_string_drop(ptr %drop_f0)
  br label %fail

rb_step_2:                                        ; preds = %step_2_clone
  %drop_f2_ptr = getelementptr inbounds nuw %Scanner, ptr %1, i32 0, i32 2
  %drop_f2 = load ptr, ptr %drop_f2_ptr, align 8
  call void @hew_string_drop(ptr %drop_f2)
  %drop_f0_ptr1 = getelementptr inbounds nuw %Scanner, ptr %1, i32 0, i32 0
  %drop_f02 = load ptr, ptr %drop_f0_ptr1, align 8
  call void @hew_string_drop(ptr %drop_f02)
  br label %fail

step_0_store:                                     ; preds = %step_0_clone
  %dst_f0_ptr = getelementptr inbounds nuw %Scanner, ptr %1, i32 0, i32 0
  store ptr %clone_helper_f0, ptr %dst_f0_ptr, align 8
  br label %step_1_clone

step_1_store:                                     ; preds = %step_1_clone
  %dst_f2_ptr = getelementptr inbounds nuw %Scanner, ptr %1, i32 0, i32 2
  store ptr %clone_helper_f2, ptr %dst_f2_ptr, align 8
  br label %step_2_clone

step_2_store:                                     ; preds = %step_2_clone
  br label %success

step_0_clone:                                     ; preds = %entry
  %src_f0_ptr = getelementptr inbounds nuw %Scanner, ptr %0, i32 0, i32 0
  %src_f0 = load ptr, ptr %src_f0_ptr, align 8
  %clone_helper_f0 = call ptr @hew_string_clone(ptr %src_f0)
  %cloned_f0_int = ptrtoint ptr %clone_helper_f0 to i64
  %cloned_f0_null = icmp eq i64 %cloned_f0_int, 0
  br i1 %cloned_f0_null, label %rb_step_0, label %step_0_store

step_1_clone:                                     ; preds = %step_0_store
  %src_f2_ptr = getelementptr inbounds nuw %Scanner, ptr %0, i32 0, i32 2
  %src_f2 = load ptr, ptr %src_f2_ptr, align 8
  %clone_helper_f2 = call ptr @hew_string_clone(ptr %src_f2)
  %cloned_f2_int = ptrtoint ptr %clone_helper_f2 to i64
  %cloned_f2_null = icmp eq i64 %cloned_f2_int, 0
  br i1 %cloned_f2_null, label %rb_step_1, label %step_1_store

step_2_clone:                                     ; preds = %step_1_store
  %src_f5_ptr = getelementptr inbounds nuw %Scanner, ptr %0, i32 0, i32 5
  %dst_f5_ptr = getelementptr inbounds nuw %Scanner, ptr %1, i32 0, i32 5
  %enum_clone_inplace_f5 = call i32 @__hew_enum_clone_inplace_SplitMode(ptr %src_f5_ptr, ptr %dst_f5_ptr)
  %enum_clone_failed_f5 = icmp ne i32 %enum_clone_inplace_f5, 0
  br i1 %enum_clone_failed_f5, label %rb_step_2, label %step_2_store
}

define internal i32 @__hew_enum_clone_inplace_SplitMode(ptr %0, ptr %1) {
entry:
  %enum_clone_tag_ptr = getelementptr inbounds nuw %SplitMode, ptr %0, i32 0, i32 0
  %enum_clone_tag = load i8, ptr %enum_clone_tag_ptr, align 1
  switch i8 %enum_clone_tag, label %tag_oob_trap [
    i8 0, label %enum_clone_variant_0
    i8 1, label %enum_clone_variant_1
  ]

success:                                          ; preds = %enum_clone_variant_1, %enum_clone_variant_0
  ret i32 0

fail:                                             ; No predecessors!
  ret i32 1

tag_oob_trap:                                     ; preds = %entry
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

enum_clone_variant_0:                             ; preds = %entry
  br label %success

enum_clone_variant_1:                             ; preds = %entry
  br label %success
}

declare void @hew_string_drop(ptr)

define internal void @__hew_record_drop_inplace_Scanner(ptr %0) {
entry:
  %rec_int = ptrtoint ptr %0 to i64
  %rec_is_null = icmp eq i64 %rec_int, 0
  br i1 %rec_is_null, label %done, label %do_drop

do_drop:                                          ; preds = %entry
  %drop_f5_ptr = getelementptr inbounds nuw %Scanner, ptr %0, i32 0, i32 5
  call void @__hew_enum_drop_inplace_SplitMode(ptr %drop_f5_ptr)
  %drop_f2_ptr = getelementptr inbounds nuw %Scanner, ptr %0, i32 0, i32 2
  %drop_f2 = load ptr, ptr %drop_f2_ptr, align 8
  call void @hew_string_drop(ptr %drop_f2)
  %drop_f0_ptr = getelementptr inbounds nuw %Scanner, ptr %0, i32 0, i32 0
  %drop_f0 = load ptr, ptr %drop_f0_ptr, align 8
  call void @hew_string_drop(ptr %drop_f0)
  br label %done

done:                                             ; preds = %do_drop, %entry
  ret void
}

define internal void @__hew_enum_drop_inplace_SplitMode(ptr %0) {
entry:
  %enum_drop_tag_ptr = getelementptr inbounds nuw %SplitMode, ptr %0, i32 0, i32 0
  %enum_drop_tag = load i8, ptr %enum_drop_tag_ptr, align 1
  switch i8 %enum_drop_tag, label %tag_oob_trap [
    i8 0, label %enum_drop_variant_0
    i8 1, label %enum_drop_variant_1
  ]

done:                                             ; preds = %enum_drop_variant_1, %enum_drop_variant_0
  ret void

tag_oob_trap:                                     ; preds = %entry
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

enum_drop_variant_0:                              ; preds = %entry
  %enum_drop_payload_0 = getelementptr inbounds nuw %SplitMode, ptr %0, i32 0, i32 1
  br label %done

enum_drop_variant_1:                              ; preds = %entry
  %enum_drop_payload_1 = getelementptr inbounds nuw %SplitMode, ptr %0, i32 0, i32 1
  br label %done
}

declare void @hew_trap_with_code(i32)

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

define internal i32 @__hew_enum_clone_inplace_IoError(ptr %0, ptr %1) {
entry:
  %enum_clone_tag_ptr = getelementptr inbounds nuw %IoError, ptr %0, i32 0, i32 0
  %enum_clone_tag = load i8, ptr %enum_clone_tag_ptr, align 1
  switch i8 %enum_clone_tag, label %tag_oob_trap [
    i8 0, label %enum_clone_variant_0
    i8 1, label %enum_clone_variant_1
    i8 2, label %enum_clone_variant_2
    i8 3, label %enum_clone_variant_3
    i8 4, label %enum_clone_variant_4
    i8 5, label %enum_clone_variant_5
  ]

success:                                          ; preds = %enum_clone_variant_5, %enum_clone_variant_4, %enum_clone_variant_3, %enum_clone_variant_2, %enum_clone_variant_1, %enum_clone_variant_0
  ret i32 0

fail:                                             ; No predecessors!
  ret i32 1

tag_oob_trap:                                     ; preds = %entry
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

enum_clone_variant_0:                             ; preds = %entry
  br label %success

enum_clone_variant_1:                             ; preds = %entry
  br label %success

enum_clone_variant_2:                             ; preds = %entry
  br label %success

enum_clone_variant_3:                             ; preds = %entry
  br label %success

enum_clone_variant_4:                             ; preds = %entry
  br label %success

enum_clone_variant_5:                             ; preds = %entry
  br label %success
}

define internal void @__hew_enum_drop_inplace_IoError(ptr %0) {
entry:
  %enum_drop_tag_ptr = getelementptr inbounds nuw %IoError, ptr %0, i32 0, i32 0
  %enum_drop_tag = load i8, ptr %enum_drop_tag_ptr, align 1
  switch i8 %enum_drop_tag, label %tag_oob_trap [
    i8 0, label %enum_drop_variant_0
    i8 1, label %enum_drop_variant_1
    i8 2, label %enum_drop_variant_2
    i8 3, label %enum_drop_variant_3
    i8 4, label %enum_drop_variant_4
    i8 5, label %enum_drop_variant_5
  ]

done:                                             ; preds = %enum_drop_variant_5, %enum_drop_variant_4, %enum_drop_variant_3, %enum_drop_variant_2, %enum_drop_variant_1, %enum_drop_variant_0
  ret void

tag_oob_trap:                                     ; preds = %entry
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

enum_drop_variant_0:                              ; preds = %entry
  %enum_drop_payload_0 = getelementptr inbounds nuw %IoError, ptr %0, i32 0, i32 1
  br label %done

enum_drop_variant_1:                              ; preds = %entry
  %enum_drop_payload_1 = getelementptr inbounds nuw %IoError, ptr %0, i32 0, i32 1
  br label %done

enum_drop_variant_2:                              ; preds = %entry
  %enum_drop_payload_2 = getelementptr inbounds nuw %IoError, ptr %0, i32 0, i32 1
  br label %done

enum_drop_variant_3:                              ; preds = %entry
  %enum_drop_payload_3 = getelementptr inbounds nuw %IoError, ptr %0, i32 0, i32 1
  br label %done

enum_drop_variant_4:                              ; preds = %entry
  %enum_drop_payload_4 = getelementptr inbounds nuw %IoError, ptr %0, i32 0, i32 1
  br label %done

enum_drop_variant_5:                              ; preds = %entry
  %enum_drop_payload_5 = getelementptr inbounds nuw %IoError, ptr %0, i32 0, i32 1
  br label %done
}

define internal i32 @"__hew_enum_clone_inplace_Option$$string"(ptr %0, ptr %1) {
entry:
  %enum_clone_tag_ptr = getelementptr inbounds nuw %"Option$$string", ptr %0, i32 0, i32 0
  %enum_clone_tag = load i8, ptr %enum_clone_tag_ptr, align 1
  switch i8 %enum_clone_tag, label %tag_oob_trap [
    i8 0, label %enum_clone_variant_0
    i8 1, label %enum_clone_variant_1
  ]

success:                                          ; preds = %enum_clone_variant_1, %enum_clone_v0_store_0
  ret i32 0

fail:                                             ; preds = %enum_clone_v0_rb_0
  ret i32 1

tag_oob_trap:                                     ; preds = %entry
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

enum_clone_variant_0:                             ; preds = %entry
  %enum_clone_src_payload_0 = getelementptr inbounds nuw %"Option$$string", ptr %0, i32 0, i32 1
  %enum_clone_dst_payload_0 = getelementptr inbounds nuw %"Option$$string", ptr %1, i32 0, i32 1
  br label %enum_clone_v0_step_0

enum_clone_variant_1:                             ; preds = %entry
  br label %success

enum_clone_v0_step_0:                             ; preds = %enum_clone_variant_0
  %src_f0_ptr = getelementptr inbounds nuw { ptr }, ptr %enum_clone_src_payload_0, i32 0, i32 0
  %src_f0 = load ptr, ptr %src_f0_ptr, align 8
  %clone_helper_f0 = call ptr @hew_string_clone(ptr %src_f0)
  %cloned_f0_int = ptrtoint ptr %clone_helper_f0 to i64
  %cloned_f0_null = icmp eq i64 %cloned_f0_int, 0
  br i1 %cloned_f0_null, label %enum_clone_v0_rb_0, label %enum_clone_v0_store_0

enum_clone_v0_store_0:                            ; preds = %enum_clone_v0_step_0
  %dst_f0_ptr = getelementptr inbounds nuw { ptr }, ptr %enum_clone_dst_payload_0, i32 0, i32 0
  store ptr %clone_helper_f0, ptr %dst_f0_ptr, align 8
  br label %success

enum_clone_v0_rb_0:                               ; preds = %enum_clone_v0_step_0
  br label %fail
}

define internal void @"__hew_enum_drop_inplace_Option$$string"(ptr %0) {
entry:
  %enum_drop_tag_ptr = getelementptr inbounds nuw %"Option$$string", ptr %0, i32 0, i32 0
  %enum_drop_tag = load i8, ptr %enum_drop_tag_ptr, align 1
  switch i8 %enum_drop_tag, label %tag_oob_trap [
    i8 0, label %enum_drop_variant_0
    i8 1, label %enum_drop_variant_1
  ]

done:                                             ; preds = %enum_drop_variant_1, %enum_drop_variant_0
  ret void

tag_oob_trap:                                     ; preds = %entry
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

enum_drop_variant_0:                              ; preds = %entry
  %enum_drop_payload_0 = getelementptr inbounds nuw %"Option$$string", ptr %0, i32 0, i32 1
  %drop_f0_ptr = getelementptr inbounds nuw { ptr }, ptr %enum_drop_payload_0, i32 0, i32 0
  %drop_f0 = load ptr, ptr %drop_f0_ptr, align 8
  call void @hew_string_drop(ptr %drop_f0)
  br label %done

enum_drop_variant_1:                              ; preds = %entry
  %enum_drop_payload_1 = getelementptr inbounds nuw %"Option$$string", ptr %0, i32 0, i32 1
  br label %done
}

define internal void @__hew_record_overwrite_release_Scanner(ptr %0, ptr %1) {
entry:
  %ow_slot_0 = alloca ptr, align 8
  store ptr null, ptr %ow_slot_0, align 8
  %ow_slot_1 = alloca ptr, align 8
  store ptr null, ptr %ow_slot_1, align 8
  %ow_new_d0_f0_ptr = getelementptr inbounds nuw %Scanner, ptr %1, i32 0, i32 0
  %ow_new_d0_f0_leaf = load ptr, ptr %ow_new_d0_f0_ptr, align 8
  store ptr %ow_new_d0_f0_leaf, ptr %ow_slot_0, align 8
  %ow_new_d0_f2_ptr = getelementptr inbounds nuw %Scanner, ptr %1, i32 0, i32 2
  %ow_new_d0_f2_leaf = load ptr, ptr %ow_new_d0_f2_ptr, align 8
  store ptr %ow_new_d0_f2_leaf, ptr %ow_slot_1, align 8
  %ow_new_d0_f5_enum_ptr = getelementptr inbounds nuw %Scanner, ptr %1, i32 0, i32 5
  %ow_old_d0_f0_ptr = getelementptr inbounds nuw %Scanner, ptr %0, i32 0, i32 0
  %ow_old_d0_f0_val = load ptr, ptr %ow_old_d0_f0_ptr, align 8
  %ow_old_d0_f0_int = ptrtoint ptr %ow_old_d0_f0_val to i64
  %ow_old_d0_f0_cmp0_leaf = load ptr, ptr %ow_slot_0, align 8
  %ow_old_d0_f0_cmp0_int = ptrtoint ptr %ow_old_d0_f0_cmp0_leaf to i64
  %ow_old_d0_f0_cmp0_eq = icmp eq i64 %ow_old_d0_f0_int, %ow_old_d0_f0_cmp0_int
  %ow_old_d0_f0_matched0 = or i1 false, %ow_old_d0_f0_cmp0_eq
  %ow_old_d0_f0_cmp1_leaf = load ptr, ptr %ow_slot_1, align 8
  %ow_old_d0_f0_cmp1_int = ptrtoint ptr %ow_old_d0_f0_cmp1_leaf to i64
  %ow_old_d0_f0_cmp1_eq = icmp eq i64 %ow_old_d0_f0_int, %ow_old_d0_f0_cmp1_int
  %ow_old_d0_f0_matched1 = or i1 %ow_old_d0_f0_matched0, %ow_old_d0_f0_cmp1_eq
  %ow_old_d0_f0_neutralized = select i1 %ow_old_d0_f0_matched1, ptr null, ptr %ow_old_d0_f0_val
  store ptr %ow_old_d0_f0_neutralized, ptr %ow_old_d0_f0_ptr, align 8
  %ow_old_d0_f2_ptr = getelementptr inbounds nuw %Scanner, ptr %0, i32 0, i32 2
  %ow_old_d0_f2_val = load ptr, ptr %ow_old_d0_f2_ptr, align 8
  %ow_old_d0_f2_int = ptrtoint ptr %ow_old_d0_f2_val to i64
  %ow_old_d0_f2_cmp0_leaf = load ptr, ptr %ow_slot_0, align 8
  %ow_old_d0_f2_cmp0_int = ptrtoint ptr %ow_old_d0_f2_cmp0_leaf to i64
  %ow_old_d0_f2_cmp0_eq = icmp eq i64 %ow_old_d0_f2_int, %ow_old_d0_f2_cmp0_int
  %ow_old_d0_f2_matched0 = or i1 false, %ow_old_d0_f2_cmp0_eq
  %ow_old_d0_f2_cmp1_leaf = load ptr, ptr %ow_slot_1, align 8
  %ow_old_d0_f2_cmp1_int = ptrtoint ptr %ow_old_d0_f2_cmp1_leaf to i64
  %ow_old_d0_f2_cmp1_eq = icmp eq i64 %ow_old_d0_f2_int, %ow_old_d0_f2_cmp1_int
  %ow_old_d0_f2_matched1 = or i1 %ow_old_d0_f2_matched0, %ow_old_d0_f2_cmp1_eq
  %ow_old_d0_f2_neutralized = select i1 %ow_old_d0_f2_matched1, ptr null, ptr %ow_old_d0_f2_val
  store ptr %ow_old_d0_f2_neutralized, ptr %ow_old_d0_f2_ptr, align 8
  %ow_old_d0_f5_enum_ptr = getelementptr inbounds nuw %Scanner, ptr %0, i32 0, i32 5
  call void @__hew_record_drop_inplace_Scanner(ptr %0)
  ret void
}

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

define internal void @__hew_enum_overwrite_release_IoError(ptr %0, ptr %1) {
entry:
  call void @__hew_enum_drop_inplace_IoError(ptr %0)
  ret void
}

define internal void @"__hew_enum_overwrite_release_Option$$string"(ptr %0, ptr %1) {
entry:
  %ow_slot_0 = alloca ptr, align 8
  store ptr null, ptr %ow_slot_0, align 8
  %"ow_new_d0_Option$$string_tag_ptr" = getelementptr inbounds nuw %"Option$$string", ptr %1, i32 0, i32 0
  %"ow_new_d0_Option$$string_tag" = load i8, ptr %"ow_new_d0_Option$$string_tag_ptr", align 1
  switch i8 %"ow_new_d0_Option$$string_tag", label %"ow_new_d0_Option$$string_tag_oob" [
    i8 0, label %"ow_new_d0_Option$$string_v0"
    i8 1, label %"ow_new_d0_Option$$string_v1"
  ]

"ow_new_d0_Option$$string_merge":                 ; preds = %"ow_new_d0_Option$$string_v1", %"ow_new_d0_Option$$string_v0"
  %"ow_old_d0_Option$$string_tag_ptr" = getelementptr inbounds nuw %"Option$$string", ptr %0, i32 0, i32 0
  %"ow_old_d0_Option$$string_tag" = load i8, ptr %"ow_old_d0_Option$$string_tag_ptr", align 1
  switch i8 %"ow_old_d0_Option$$string_tag", label %"ow_old_d0_Option$$string_tag_oob" [
    i8 0, label %"ow_old_d0_Option$$string_v0"
    i8 1, label %"ow_old_d0_Option$$string_v1"
  ]

"ow_new_d0_Option$$string_tag_oob":               ; preds = %entry
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

"ow_new_d0_Option$$string_v0":                    ; preds = %entry
  %"ow_new_d0_Option$$string_v0_payload" = getelementptr inbounds nuw %"Option$$string", ptr %1, i32 0, i32 1
  %ow_new_d0_f0_ptr = getelementptr inbounds nuw { ptr }, ptr %"ow_new_d0_Option$$string_v0_payload", i32 0, i32 0
  %ow_new_d0_f0_leaf = load ptr, ptr %ow_new_d0_f0_ptr, align 8
  store ptr %ow_new_d0_f0_leaf, ptr %ow_slot_0, align 8
  br label %"ow_new_d0_Option$$string_merge"

"ow_new_d0_Option$$string_v1":                    ; preds = %entry
  %"ow_new_d0_Option$$string_v1_payload" = getelementptr inbounds nuw %"Option$$string", ptr %1, i32 0, i32 1
  br label %"ow_new_d0_Option$$string_merge"

"ow_old_d0_Option$$string_merge":                 ; preds = %"ow_old_d0_Option$$string_v1", %"ow_old_d0_Option$$string_v0"
  call void @"__hew_enum_drop_inplace_Option$$string"(ptr %0)
  ret void

"ow_old_d0_Option$$string_tag_oob":               ; preds = %"ow_new_d0_Option$$string_merge"
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

"ow_old_d0_Option$$string_v0":                    ; preds = %"ow_new_d0_Option$$string_merge"
  %"ow_old_d0_Option$$string_v0_payload" = getelementptr inbounds nuw %"Option$$string", ptr %0, i32 0, i32 1
  %ow_old_d0_f0_ptr = getelementptr inbounds nuw { ptr }, ptr %"ow_old_d0_Option$$string_v0_payload", i32 0, i32 0
  %ow_old_d0_f0_val = load ptr, ptr %ow_old_d0_f0_ptr, align 8
  %ow_old_d0_f0_int = ptrtoint ptr %ow_old_d0_f0_val to i64
  %ow_old_d0_f0_cmp0_leaf = load ptr, ptr %ow_slot_0, align 8
  %ow_old_d0_f0_cmp0_int = ptrtoint ptr %ow_old_d0_f0_cmp0_leaf to i64
  %ow_old_d0_f0_cmp0_eq = icmp eq i64 %ow_old_d0_f0_int, %ow_old_d0_f0_cmp0_int
  %ow_old_d0_f0_matched0 = or i1 false, %ow_old_d0_f0_cmp0_eq
  %ow_old_d0_f0_neutralized = select i1 %ow_old_d0_f0_matched0, ptr null, ptr %ow_old_d0_f0_val
  store ptr %ow_old_d0_f0_neutralized, ptr %ow_old_d0_f0_ptr, align 8
  br label %"ow_old_d0_Option$$string_merge"

"ow_old_d0_Option$$string_v1":                    ; preds = %"ow_new_d0_Option$$string_merge"
  %"ow_old_d0_Option$$string_v1_payload" = getelementptr inbounds nuw %"Option$$string", ptr %0, i32 0, i32 1
  br label %"ow_old_d0_Option$$string_merge"
}

define internal void @__hew_enum_overwrite_release_SplitMode(ptr %0, ptr %1) {
entry:
  call void @__hew_enum_drop_inplace_SplitMode(ptr %0)
  ret void
}

declare i32 @hew_actor_cooperate()

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias writeonly captures(none), ptr noalias readonly captures(none), i64, i1 immarg) #1

declare i32 @hew_lambda_drain_all(i64)

declare ptr @hew_vec_new_str()

declare i32 @hew_string_find(ptr, ptr)

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64) #2

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.ssub.with.overflow.i64(i64, i64) #2

declare i32 @hew_string_equals(ptr, ptr)

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.smul.with.overflow.i64(i64, i64) #2

attributes #0 = { cold noreturn nounwind memory(inaccessiblemem: write) }
attributes #1 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
attributes #2 = { nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none) }
