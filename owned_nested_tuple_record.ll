; ModuleID = 'owned_nested_tuple_record'
source_filename = "owned_nested_tuple_record"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "aarch64-apple-macosx13.0"

%Rec = type { ptr }
%"Option$$tuple$xtuple$xRec$ci64$g$cbool$g" = type { i8, [3 x i64] }
%CrashInfo = type { i64, ptr }

@"__hew_vec_elem_layout_tup_tuple_tuple$xRec$ci64$g_bool_24_8" = private constant { i64, i64, i8, ptr, ptr } { i64 24, i64 8, i8 2, ptr @"__hew_tuple_clone_inplace_tuple_tuple$xRec$ci64$g_bool", ptr @"__hew_tuple_drop_inplace_tuple_tuple$xRec$ci64$g_bool" }
@str_lit = private unnamed_addr constant [3 x i8] c"hi\00", align 1
@str_lit.1 = private unnamed_addr constant [4 x i8] c"bye\00", align 1
@str_lit.2 = private unnamed_addr constant [8 x i8] c"changed\00", align 1
@str_lit.3 = private unnamed_addr constant [4 x i8] c"oob\00", align 1
@str_lit.4 = private unnamed_addr constant [4 x i8] c"oob\00", align 1
@str_lit.5 = private unnamed_addr constant [2 x i8] c":\00", align 1
@str_lit.6 = private unnamed_addr constant [2 x i8] c":\00", align 1
@str_lit.7 = private unnamed_addr constant [2 x i8] c":\00", align 1
@str_lit.8 = private unnamed_addr constant [2 x i8] c":\00", align 1
@str_lit.9 = private unnamed_addr constant [2 x i8] c":\00", align 1
@str_lit.10 = private unnamed_addr constant [2 x i8] c":\00", align 1
@str_lit.11 = private unnamed_addr constant [3 x i8] c"ns\00", align 1

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

define i8 @main() {
entry:
  %return_slot = alloca i8, align 1
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca ptr, align 8
  %local_3 = alloca %Rec, align 8
  %local_4 = alloca i64, align 8
  %local_5 = alloca { %Rec, i64 }, align 8
  %local_6 = alloca i8, align 1
  %local_7 = alloca { { %Rec, i64 }, i8 }, align 8
  %local_8 = alloca ptr, align 8
  %local_9 = alloca %Rec, align 8
  %local_10 = alloca i64, align 8
  %local_11 = alloca { %Rec, i64 }, align 8
  %local_12 = alloca i8, align 1
  %local_13 = alloca { { %Rec, i64 }, i8 }, align 8
  %local_14 = alloca ptr, align 8
  %local_15 = alloca ptr, align 8
  %local_16 = alloca i64, align 8
  %local_17 = alloca ptr, align 8
  %local_18 = alloca %Rec, align 8
  %local_19 = alloca i64, align 8
  %local_20 = alloca { %Rec, i64 }, align 8
  %local_21 = alloca i8, align 1
  %local_22 = alloca { { %Rec, i64 }, i8 }, align 8
  %local_23 = alloca { { %Rec, i64 }, i8 }, align 8
  %local_24 = alloca i64, align 8
  %local_25 = alloca %"Option$$tuple$xtuple$xRec$ci64$g$cbool$g", align 8
  %local_26 = alloca i64, align 8
  %local_27 = alloca i64, align 8
  %local_28 = alloca i8, align 1
  %local_29 = alloca i64, align 8
  %local_30 = alloca i8, align 1
  %local_31 = alloca { { %Rec, i64 }, i8 }, align 8
  %local_32 = alloca ptr, align 8
  %local_33 = alloca { { %Rec, i64 }, i8 }, align 8
  %local_34 = alloca { { %Rec, i64 }, i8 }, align 8
  %local_35 = alloca i64, align 8
  %local_36 = alloca %"Option$$tuple$xtuple$xRec$ci64$g$cbool$g", align 8
  %local_37 = alloca i64, align 8
  %local_38 = alloca i64, align 8
  %local_39 = alloca i8, align 1
  %local_40 = alloca i64, align 8
  %local_41 = alloca i8, align 1
  %local_42 = alloca { { %Rec, i64 }, i8 }, align 8
  %local_43 = alloca ptr, align 8
  %local_44 = alloca { { %Rec, i64 }, i8 }, align 8
  %local_45 = alloca { { %Rec, i64 }, i8 }, align 8
  %local_46 = alloca { %Rec, i64 }, align 8
  %local_47 = alloca { %Rec, i64 }, align 8
  %local_48 = alloca i8, align 1
  %local_49 = alloca i8, align 1
  %local_50 = alloca { %Rec, i64 }, align 8
  %local_51 = alloca %Rec, align 8
  %local_52 = alloca %Rec, align 8
  %local_53 = alloca i64, align 8
  %local_54 = alloca i64, align 8
  %local_55 = alloca { { %Rec, i64 }, i8 }, align 8
  %local_56 = alloca { %Rec, i64 }, align 8
  %local_57 = alloca { %Rec, i64 }, align 8
  %local_58 = alloca i8, align 1
  %local_59 = alloca i8, align 1
  %local_60 = alloca { %Rec, i64 }, align 8
  %local_61 = alloca %Rec, align 8
  %local_62 = alloca %Rec, align 8
  %local_63 = alloca i64, align 8
  %local_64 = alloca i64, align 8
  %local_65 = alloca i64, align 8
  %local_66 = alloca ptr, align 8
  %local_67 = alloca ptr, align 8
  %local_68 = alloca ptr, align 8
  %local_69 = alloca ptr, align 8
  %local_70 = alloca ptr, align 8
  %local_71 = alloca ptr, align 8
  %local_72 = alloca ptr, align 8
  %local_73 = alloca ptr, align 8
  %local_74 = alloca ptr, align 8
  %local_75 = alloca ptr, align 8
  %local_76 = alloca ptr, align 8
  %local_77 = alloca ptr, align 8
  %local_78 = alloca ptr, align 8
  %local_79 = alloca ptr, align 8
  %local_80 = alloca ptr, align 8
  %local_81 = alloca ptr, align 8
  %local_82 = alloca ptr, align 8
  %local_83 = alloca ptr, align 8
  %local_84 = alloca ptr, align 8
  %local_85 = alloca ptr, align 8
  %local_86 = alloca ptr, align 8
  %local_87 = alloca ptr, align 8
  %local_88 = alloca ptr, align 8
  %local_89 = alloca ptr, align 8
  %local_90 = alloca ptr, align 8
  %local_91 = alloca ptr, align 8
  %local_92 = alloca ptr, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %hew_vec_new_with_elem_layout_call = call ptr @hew_vec_new_with_elem_layout(ptr @"__hew_vec_elem_layout_tup_tuple_tuple$xRec$ci64$g_bool_24_8")
  store ptr %hew_vec_new_with_elem_layout_call, ptr %local_0, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_0, align 8
  store ptr %move_load, ptr %local_1, align 8
  store ptr @str_lit, ptr %local_2, align 8
  %field_0_init_ptr = getelementptr inbounds nuw %Rec, ptr %local_3, i32 0, i32 0
  %field_0_init_src = load ptr, ptr %local_2, align 8
  store ptr %field_0_init_src, ptr %field_0_init_ptr, align 8
  store i64 21, ptr %local_4, align 8
  %tuple_elem_0_load = load %Rec, ptr %local_3, align 8
  %tuple_elem_0_gep = getelementptr inbounds nuw { %Rec, i64 }, ptr %local_5, i32 0, i32 0
  store %Rec %tuple_elem_0_load, ptr %tuple_elem_0_gep, align 8
  %tuple_elem_1_load = load i64, ptr %local_4, align 8
  %tuple_elem_1_gep = getelementptr inbounds nuw { %Rec, i64 }, ptr %local_5, i32 0, i32 1
  store i64 %tuple_elem_1_load, ptr %tuple_elem_1_gep, align 8
  store i8 1, ptr %local_6, align 1
  %tuple_elem_0_load1 = load { %Rec, i64 }, ptr %local_5, align 8
  %tuple_elem_0_gep2 = getelementptr inbounds nuw { { %Rec, i64 }, i8 }, ptr %local_7, i32 0, i32 0
  store { %Rec, i64 } %tuple_elem_0_load1, ptr %tuple_elem_0_gep2, align 8
  %tuple_elem_1_load3 = load i8, ptr %local_6, align 1
  %tuple_elem_1_gep4 = getelementptr inbounds nuw { { %Rec, i64 }, i8 }, ptr %local_7, i32 0, i32 1
  store i8 %tuple_elem_1_load3, ptr %tuple_elem_1_gep4, align 1
  %"hew_vec_push_owned_move arg0" = load ptr, ptr %local_1, align 8
  call void @hew_vec_push_owned_move(ptr %"hew_vec_push_owned_move arg0", ptr %local_7)
  br label %bb2

bb2:                                              ; preds = %bb1
  store ptr @str_lit.1, ptr %local_8, align 8
  %field_0_init_ptr5 = getelementptr inbounds nuw %Rec, ptr %local_9, i32 0, i32 0
  %field_0_init_src6 = load ptr, ptr %local_8, align 8
  store ptr %field_0_init_src6, ptr %field_0_init_ptr5, align 8
  store i64 22, ptr %local_10, align 8
  %tuple_elem_0_load7 = load %Rec, ptr %local_9, align 8
  %tuple_elem_0_gep8 = getelementptr inbounds nuw { %Rec, i64 }, ptr %local_11, i32 0, i32 0
  store %Rec %tuple_elem_0_load7, ptr %tuple_elem_0_gep8, align 8
  %tuple_elem_1_load9 = load i64, ptr %local_10, align 8
  %tuple_elem_1_gep10 = getelementptr inbounds nuw { %Rec, i64 }, ptr %local_11, i32 0, i32 1
  store i64 %tuple_elem_1_load9, ptr %tuple_elem_1_gep10, align 8
  store i8 0, ptr %local_12, align 1
  %tuple_elem_0_load11 = load { %Rec, i64 }, ptr %local_11, align 8
  %tuple_elem_0_gep12 = getelementptr inbounds nuw { { %Rec, i64 }, i8 }, ptr %local_13, i32 0, i32 0
  store { %Rec, i64 } %tuple_elem_0_load11, ptr %tuple_elem_0_gep12, align 8
  %tuple_elem_1_load13 = load i8, ptr %local_12, align 1
  %tuple_elem_1_gep14 = getelementptr inbounds nuw { { %Rec, i64 }, i8 }, ptr %local_13, i32 0, i32 1
  store i8 %tuple_elem_1_load13, ptr %tuple_elem_1_gep14, align 1
  %"hew_vec_push_owned_move arg015" = load ptr, ptr %local_1, align 8
  call void @hew_vec_push_owned_move(ptr %"hew_vec_push_owned_move arg015", ptr %local_13)
  br label %bb3

bb3:                                              ; preds = %bb2
  %"hew_vec_clone_owned arg0" = load ptr, ptr %local_1, align 8
  %hew_vec_clone_owned_call = call ptr @hew_vec_clone_owned(ptr %"hew_vec_clone_owned arg0")
  store ptr %hew_vec_clone_owned_call, ptr %local_14, align 8
  br label %bb4

bb4:                                              ; preds = %bb3
  %move_load16 = load ptr, ptr %local_14, align 8
  store ptr %move_load16, ptr %local_15, align 8
  store i64 0, ptr %local_16, align 8
  store ptr @str_lit.2, ptr %local_17, align 8
  %field_0_init_ptr17 = getelementptr inbounds nuw %Rec, ptr %local_18, i32 0, i32 0
  %field_0_init_src18 = load ptr, ptr %local_17, align 8
  store ptr %field_0_init_src18, ptr %field_0_init_ptr17, align 8
  store i64 99, ptr %local_19, align 8
  %tuple_elem_0_load19 = load %Rec, ptr %local_18, align 8
  %tuple_elem_0_gep20 = getelementptr inbounds nuw { %Rec, i64 }, ptr %local_20, i32 0, i32 0
  store %Rec %tuple_elem_0_load19, ptr %tuple_elem_0_gep20, align 8
  %tuple_elem_1_load21 = load i64, ptr %local_19, align 8
  %tuple_elem_1_gep22 = getelementptr inbounds nuw { %Rec, i64 }, ptr %local_20, i32 0, i32 1
  store i64 %tuple_elem_1_load21, ptr %tuple_elem_1_gep22, align 8
  store i8 1, ptr %local_21, align 1
  %tuple_elem_0_load23 = load { %Rec, i64 }, ptr %local_20, align 8
  %tuple_elem_0_gep24 = getelementptr inbounds nuw { { %Rec, i64 }, i8 }, ptr %local_22, i32 0, i32 0
  store { %Rec, i64 } %tuple_elem_0_load23, ptr %tuple_elem_0_gep24, align 8
  %tuple_elem_1_load25 = load i8, ptr %local_21, align 1
  %tuple_elem_1_gep26 = getelementptr inbounds nuw { { %Rec, i64 }, i8 }, ptr %local_22, i32 0, i32 1
  store i8 %tuple_elem_1_load25, ptr %tuple_elem_1_gep26, align 1
  %"hew_vec_set_owned_move arg0" = load ptr, ptr %local_1, align 8
  %hew_vec_set_owned_arg1 = load i64, ptr %local_16, align 8
  call void @hew_vec_set_owned_move(ptr %"hew_vec_set_owned_move arg0", i64 %hew_vec_set_owned_arg1, ptr %local_22)
  br label %bb5

bb5:                                              ; preds = %bb4
  store i64 0, ptr %local_24, align 8
  %"hew_vec_get_clone arg0" = load ptr, ptr %local_15, align 8
  %"hew_vec_get_clone index" = load i64, ptr %local_24, align 8
  %machine_payload_ptr = getelementptr inbounds nuw %"Option$$tuple$xtuple$xRec$ci64$g$cbool$g", ptr %local_25, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { { { %Rec, i64 }, i8 } }, ptr %machine_payload_ptr, i32 0, i32 0
  %hew_vec_get_clone_call = call i1 @hew_vec_get_clone(ptr %"hew_vec_get_clone arg0", i64 %"hew_vec_get_clone index", ptr %machine_variant_field_ptr)
  br i1 %hew_vec_get_clone_call, label %vec_get_some, label %vec_get_none

bb6:                                              ; preds = %vec_get_some, %vec_get_none
  %machine_tag_ptr28 = getelementptr inbounds nuw %"Option$$tuple$xtuple$xRec$ci64$g$cbool$g", ptr %local_25, i32 0, i32 0
  %move_iN_load = load i8, ptr %machine_tag_ptr28, align 1
  %move_iN_zext = zext i8 %move_iN_load to i64
  store i64 %move_iN_zext, ptr %local_26, align 8
  store i64 0, ptr %local_27, align 8
  %cmp_lhs = load i64, ptr %local_26, align 8
  %cmp_rhs = load i64, ptr %local_27, align 8
  %cmp_bit = icmp eq i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_28, align 1
  %cond_load = load i8, ptr %local_28, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb8, label %bb11

bb7:                                              ; preds = %after_cooperate61, %after_cooperate46
  %move_load29 = load { { %Rec, i64 }, i8 }, ptr %local_23, align 8
  store { { %Rec, i64 }, i8 } %move_load29, ptr %local_33, align 8
  store i64 1, ptr %local_35, align 8
  %"hew_vec_get_clone arg030" = load ptr, ptr %local_15, align 8
  %"hew_vec_get_clone index31" = load i64, ptr %local_35, align 8
  %machine_payload_ptr32 = getelementptr inbounds nuw %"Option$$tuple$xtuple$xRec$ci64$g$cbool$g", ptr %local_36, i32 0, i32 1
  %machine_variant_field_ptr33 = getelementptr inbounds nuw { { { %Rec, i64 }, i8 } }, ptr %machine_payload_ptr32, i32 0, i32 0
  %hew_vec_get_clone_call34 = call i1 @hew_vec_get_clone(ptr %"hew_vec_get_clone arg030", i64 %"hew_vec_get_clone index31", ptr %machine_variant_field_ptr33)
  br i1 %hew_vec_get_clone_call34, label %vec_get_some36, label %vec_get_none35

bb8:                                              ; preds = %bb6
  %machine_payload_ptr39 = getelementptr inbounds nuw %"Option$$tuple$xtuple$xRec$ci64$g$cbool$g", ptr %local_25, i32 0, i32 1
  %machine_variant_field_ptr40 = getelementptr inbounds nuw { { { %Rec, i64 }, i8 } }, ptr %machine_payload_ptr39, i32 0, i32 0
  %move_load41 = load { { %Rec, i64 }, i8 }, ptr %machine_variant_field_ptr40, align 8
  store { { %Rec, i64 }, i8 } %move_load41, ptr %local_31, align 8
  %move_load42 = load { { %Rec, i64 }, i8 }, ptr %local_31, align 8
  store { { %Rec, i64 }, i8 } %move_load42, ptr %local_23, align 8
  %hew_actor_cooperate43 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel44 = icmp eq i32 %hew_actor_cooperate43, 2
  br i1 %hew_cooperate_is_cancel44, label %cancel_exit45, label %after_cooperate46

bb9:                                              ; preds = %bb11
  store ptr @str_lit.3, ptr %local_32, align 8
  %print_arg = load ptr, ptr %local_32, align 8
  %print_str_bits = ptrtoint ptr %print_arg to i64
  call void @hew_print_value(i8 4, i64 %print_str_bits, i1 true)
  br label %bb12

bb10:                                             ; preds = %bb11
  %"hew_vec_free_owned drop48" = load ptr, ptr %local_15, align 8
  call void @hew_vec_free_owned(ptr %"hew_vec_free_owned drop48")
  store ptr null, ptr %local_15, align 8
  %"hew_vec_free_owned drop49" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free_owned(ptr %"hew_vec_free_owned drop49")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

bb11:                                             ; preds = %bb6
  store i64 1, ptr %local_29, align 8
  %cmp_lhs50 = load i64, ptr %local_26, align 8
  %cmp_rhs51 = load i64, ptr %local_29, align 8
  %cmp_bit52 = icmp eq i64 %cmp_lhs50, %cmp_rhs51
  %cmp_zext53 = zext i1 %cmp_bit52 to i8
  store i8 %cmp_zext53, ptr %local_30, align 1
  %cond_load54 = load i8, ptr %local_30, align 1
  %cond_nz55 = icmp ne i8 %cond_load54, 0
  br i1 %cond_nz55, label %bb9, label %bb10

bb12:                                             ; preds = %bb9
  %"hew_vec_free_owned drop56" = load ptr, ptr %local_15, align 8
  call void @hew_vec_free_owned(ptr %"hew_vec_free_owned drop56")
  store ptr null, ptr %local_15, align 8
  %"hew_vec_free_owned drop57" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free_owned(ptr %"hew_vec_free_owned drop57")
  store ptr null, ptr %local_1, align 8
  %hew_lambda_drain_all_call = call i32 @hew_lambda_drain_all(i64 0)
  ret i8 0

bb13:                                             ; No predecessors!
  %hew_actor_cooperate58 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel59 = icmp eq i32 %hew_actor_cooperate58, 2
  br i1 %hew_cooperate_is_cancel59, label %cancel_exit60, label %after_cooperate61

bb14:                                             ; preds = %vec_get_some36, %vec_get_none35
  %machine_tag_ptr62 = getelementptr inbounds nuw %"Option$$tuple$xtuple$xRec$ci64$g$cbool$g", ptr %local_36, i32 0, i32 0
  %move_iN_load63 = load i8, ptr %machine_tag_ptr62, align 1
  %move_iN_zext64 = zext i8 %move_iN_load63 to i64
  store i64 %move_iN_zext64, ptr %local_37, align 8
  store i64 0, ptr %local_38, align 8
  %cmp_lhs65 = load i64, ptr %local_37, align 8
  %cmp_rhs66 = load i64, ptr %local_38, align 8
  %cmp_bit67 = icmp eq i64 %cmp_lhs65, %cmp_rhs66
  %cmp_zext68 = zext i1 %cmp_bit67 to i8
  store i8 %cmp_zext68, ptr %local_39, align 1
  %cond_load69 = load i8, ptr %local_39, align 1
  %cond_nz70 = icmp ne i8 %cond_load69, 0
  br i1 %cond_nz70, label %bb16, label %bb19

bb15:                                             ; preds = %after_cooperate122, %after_cooperate103
  %move_load71 = load { { %Rec, i64 }, i8 }, ptr %local_34, align 8
  store { { %Rec, i64 }, i8 } %move_load71, ptr %local_44, align 8
  %move_load72 = load { { %Rec, i64 }, i8 }, ptr %local_33, align 8
  store { { %Rec, i64 }, i8 } %move_load72, ptr %local_45, align 8
  %tuple_0_load_ptr = getelementptr inbounds nuw { { %Rec, i64 }, i8 }, ptr %local_45, i32 0, i32 0
  %tuple_0_load = load { %Rec, i64 }, ptr %tuple_0_load_ptr, align 8
  store { %Rec, i64 } %tuple_0_load, ptr %local_46, align 8
  %move_load73 = load { %Rec, i64 }, ptr %local_46, align 8
  store { %Rec, i64 } %move_load73, ptr %local_47, align 8
  %tuple_1_load_ptr = getelementptr inbounds nuw { { %Rec, i64 }, i8 }, ptr %local_45, i32 0, i32 1
  %tuple_1_load = load i8, ptr %tuple_1_load_ptr, align 1
  store i8 %tuple_1_load, ptr %local_48, align 1
  %move_load74 = load i8, ptr %local_48, align 1
  store i8 %move_load74, ptr %local_49, align 1
  %move_load75 = load { %Rec, i64 }, ptr %local_47, align 8
  store { %Rec, i64 } %move_load75, ptr %local_50, align 8
  %tuple_0_load_ptr76 = getelementptr inbounds nuw { %Rec, i64 }, ptr %local_50, i32 0, i32 0
  %tuple_0_load77 = load %Rec, ptr %tuple_0_load_ptr76, align 8
  store %Rec %tuple_0_load77, ptr %local_51, align 8
  %move_load78 = load %Rec, ptr %local_51, align 8
  store %Rec %move_load78, ptr %local_52, align 8
  %tuple_1_load_ptr79 = getelementptr inbounds nuw { %Rec, i64 }, ptr %local_50, i32 0, i32 1
  %tuple_1_load80 = load i64, ptr %tuple_1_load_ptr79, align 8
  store i64 %tuple_1_load80, ptr %local_53, align 8
  %move_load81 = load i64, ptr %local_53, align 8
  store i64 %move_load81, ptr %local_54, align 8
  %move_load82 = load { { %Rec, i64 }, i8 }, ptr %local_44, align 8
  store { { %Rec, i64 }, i8 } %move_load82, ptr %local_55, align 8
  %tuple_0_load_ptr83 = getelementptr inbounds nuw { { %Rec, i64 }, i8 }, ptr %local_55, i32 0, i32 0
  %tuple_0_load84 = load { %Rec, i64 }, ptr %tuple_0_load_ptr83, align 8
  store { %Rec, i64 } %tuple_0_load84, ptr %local_56, align 8
  %move_load85 = load { %Rec, i64 }, ptr %local_56, align 8
  store { %Rec, i64 } %move_load85, ptr %local_57, align 8
  %tuple_1_load_ptr86 = getelementptr inbounds nuw { { %Rec, i64 }, i8 }, ptr %local_55, i32 0, i32 1
  %tuple_1_load87 = load i8, ptr %tuple_1_load_ptr86, align 1
  store i8 %tuple_1_load87, ptr %local_58, align 1
  %move_load88 = load i8, ptr %local_58, align 1
  store i8 %move_load88, ptr %local_59, align 1
  %move_load89 = load { %Rec, i64 }, ptr %local_57, align 8
  store { %Rec, i64 } %move_load89, ptr %local_60, align 8
  %tuple_0_load_ptr90 = getelementptr inbounds nuw { %Rec, i64 }, ptr %local_60, i32 0, i32 0
  %tuple_0_load91 = load %Rec, ptr %tuple_0_load_ptr90, align 8
  store %Rec %tuple_0_load91, ptr %local_61, align 8
  %move_load92 = load %Rec, ptr %local_61, align 8
  store %Rec %move_load92, ptr %local_62, align 8
  %tuple_1_load_ptr93 = getelementptr inbounds nuw { %Rec, i64 }, ptr %local_60, i32 0, i32 1
  %tuple_1_load94 = load i64, ptr %tuple_1_load_ptr93, align 8
  store i64 %tuple_1_load94, ptr %local_63, align 8
  %move_load95 = load i64, ptr %local_63, align 8
  store i64 %move_load95, ptr %local_64, align 8
  %call_arg = load ptr, ptr %local_15, align 8
  %call_result = call i64 @hew_vec_len(ptr %call_arg)
  store i64 %call_result, ptr %local_65, align 8
  br label %bb22

bb16:                                             ; preds = %bb14
  %machine_payload_ptr96 = getelementptr inbounds nuw %"Option$$tuple$xtuple$xRec$ci64$g$cbool$g", ptr %local_36, i32 0, i32 1
  %machine_variant_field_ptr97 = getelementptr inbounds nuw { { { %Rec, i64 }, i8 } }, ptr %machine_payload_ptr96, i32 0, i32 0
  %move_load98 = load { { %Rec, i64 }, i8 }, ptr %machine_variant_field_ptr97, align 8
  store { { %Rec, i64 }, i8 } %move_load98, ptr %local_42, align 8
  %move_load99 = load { { %Rec, i64 }, i8 }, ptr %local_42, align 8
  store { { %Rec, i64 }, i8 } %move_load99, ptr %local_34, align 8
  %hew_actor_cooperate100 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel101 = icmp eq i32 %hew_actor_cooperate100, 2
  br i1 %hew_cooperate_is_cancel101, label %cancel_exit102, label %after_cooperate103

bb17:                                             ; preds = %bb19
  store ptr @str_lit.4, ptr %local_43, align 8
  %print_arg106 = load ptr, ptr %local_43, align 8
  %print_str_bits107 = ptrtoint ptr %print_arg106 to i64
  call void @hew_print_value(i8 4, i64 %print_str_bits107, i1 true)
  br label %bb20

bb18:                                             ; preds = %bb19
  %"hew_vec_free_owned drop108" = load ptr, ptr %local_15, align 8
  call void @hew_vec_free_owned(ptr %"hew_vec_free_owned drop108")
  store ptr null, ptr %local_15, align 8
  %"hew_vec_free_owned drop109" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free_owned(ptr %"hew_vec_free_owned drop109")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

bb19:                                             ; preds = %bb14
  store i64 1, ptr %local_40, align 8
  %cmp_lhs110 = load i64, ptr %local_37, align 8
  %cmp_rhs111 = load i64, ptr %local_40, align 8
  %cmp_bit112 = icmp eq i64 %cmp_lhs110, %cmp_rhs111
  %cmp_zext113 = zext i1 %cmp_bit112 to i8
  store i8 %cmp_zext113, ptr %local_41, align 1
  %cond_load114 = load i8, ptr %local_41, align 1
  %cond_nz115 = icmp ne i8 %cond_load114, 0
  br i1 %cond_nz115, label %bb17, label %bb18

bb20:                                             ; preds = %bb17
  %"hew_vec_free_owned drop116" = load ptr, ptr %local_15, align 8
  call void @hew_vec_free_owned(ptr %"hew_vec_free_owned drop116")
  store ptr null, ptr %local_15, align 8
  %"hew_vec_free_owned drop117" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free_owned(ptr %"hew_vec_free_owned drop117")
  store ptr null, ptr %local_1, align 8
  %hew_lambda_drain_all_call118 = call i32 @hew_lambda_drain_all(i64 0)
  ret i8 0

bb21:                                             ; No predecessors!
  %hew_actor_cooperate119 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel120 = icmp eq i32 %hew_actor_cooperate119, 2
  br i1 %hew_cooperate_is_cancel120, label %cancel_exit121, label %after_cooperate122

bb22:                                             ; preds = %bb15
  %call_arg123 = load i64, ptr %local_65, align 8
  %call_result124 = call ptr @hew_i64_to_string(i64 %call_arg123)
  store ptr %call_result124, ptr %local_66, align 8
  br label %bb23

bb23:                                             ; preds = %bb22
  store ptr @str_lit.5, ptr %local_67, align 8
  %call_arg125 = load ptr, ptr %local_66, align 8
  %call_arg126 = load ptr, ptr %local_67, align 8
  %call_result127 = call ptr @hew_string_concat(ptr %call_arg125, ptr %call_arg126)
  store ptr %call_result127, ptr %local_68, align 8
  br label %bb24

bb24:                                             ; preds = %bb23
  %"hew_string_drop drop" = load ptr, ptr %local_66, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop")
  store ptr null, ptr %local_66, align 8
  %field_0_load_ptr = getelementptr inbounds nuw %Rec, ptr %local_52, i32 0, i32 0
  %field_0_load = load ptr, ptr %field_0_load_ptr, align 8
  %field_0_str_retain = call ptr @hew_string_clone(ptr %field_0_load)
  store ptr %field_0_str_retain, ptr %local_69, align 8
  %call_arg128 = load ptr, ptr %local_69, align 8
  %call_result129 = call ptr @"string::fmt"(ptr %call_arg128)
  store ptr %call_result129, ptr %local_70, align 8
  br label %bb25

bb25:                                             ; preds = %bb24
  %call_arg130 = load ptr, ptr %local_68, align 8
  %call_arg131 = load ptr, ptr %local_70, align 8
  %call_result132 = call ptr @hew_string_concat(ptr %call_arg130, ptr %call_arg131)
  store ptr %call_result132, ptr %local_71, align 8
  br label %bb26

bb26:                                             ; preds = %bb25
  %"hew_string_drop drop133" = load ptr, ptr %local_70, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop133")
  store ptr null, ptr %local_70, align 8
  %"hew_string_drop drop134" = load ptr, ptr %local_68, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop134")
  store ptr null, ptr %local_68, align 8
  store ptr @str_lit.6, ptr %local_72, align 8
  %call_arg135 = load ptr, ptr %local_71, align 8
  %call_arg136 = load ptr, ptr %local_72, align 8
  %call_result137 = call ptr @hew_string_concat(ptr %call_arg135, ptr %call_arg136)
  store ptr %call_result137, ptr %local_73, align 8
  br label %bb27

bb27:                                             ; preds = %bb26
  %"hew_string_drop drop138" = load ptr, ptr %local_71, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop138")
  store ptr null, ptr %local_71, align 8
  %call_arg139 = load i64, ptr %local_54, align 8
  %call_result140 = call ptr @hew_i64_to_string(i64 %call_arg139)
  store ptr %call_result140, ptr %local_74, align 8
  br label %bb28

bb28:                                             ; preds = %bb27
  %call_arg141 = load ptr, ptr %local_73, align 8
  %call_arg142 = load ptr, ptr %local_74, align 8
  %call_result143 = call ptr @hew_string_concat(ptr %call_arg141, ptr %call_arg142)
  store ptr %call_result143, ptr %local_75, align 8
  br label %bb29

bb29:                                             ; preds = %bb28
  %"hew_string_drop drop144" = load ptr, ptr %local_74, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop144")
  store ptr null, ptr %local_74, align 8
  %"hew_string_drop drop145" = load ptr, ptr %local_73, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop145")
  store ptr null, ptr %local_73, align 8
  store ptr @str_lit.7, ptr %local_76, align 8
  %call_arg146 = load ptr, ptr %local_75, align 8
  %call_arg147 = load ptr, ptr %local_76, align 8
  %call_result148 = call ptr @hew_string_concat(ptr %call_arg146, ptr %call_arg147)
  store ptr %call_result148, ptr %local_77, align 8
  br label %bb30

bb30:                                             ; preds = %bb29
  %"hew_string_drop drop149" = load ptr, ptr %local_75, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop149")
  store ptr null, ptr %local_75, align 8
  %call_arg150 = load i8, ptr %local_49, align 1
  %call_result151 = call ptr @hew_bool_to_string(i8 %call_arg150)
  store ptr %call_result151, ptr %local_78, align 8
  br label %bb31

bb31:                                             ; preds = %bb30
  %call_arg152 = load ptr, ptr %local_77, align 8
  %call_arg153 = load ptr, ptr %local_78, align 8
  %call_result154 = call ptr @hew_string_concat(ptr %call_arg152, ptr %call_arg153)
  store ptr %call_result154, ptr %local_79, align 8
  br label %bb32

bb32:                                             ; preds = %bb31
  %"hew_string_drop drop155" = load ptr, ptr %local_78, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop155")
  store ptr null, ptr %local_78, align 8
  %"hew_string_drop drop156" = load ptr, ptr %local_77, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop156")
  store ptr null, ptr %local_77, align 8
  store ptr @str_lit.8, ptr %local_80, align 8
  %call_arg157 = load ptr, ptr %local_79, align 8
  %call_arg158 = load ptr, ptr %local_80, align 8
  %call_result159 = call ptr @hew_string_concat(ptr %call_arg157, ptr %call_arg158)
  store ptr %call_result159, ptr %local_81, align 8
  br label %bb33

bb33:                                             ; preds = %bb32
  %"hew_string_drop drop160" = load ptr, ptr %local_79, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop160")
  store ptr null, ptr %local_79, align 8
  %field_0_load_ptr161 = getelementptr inbounds nuw %Rec, ptr %local_62, i32 0, i32 0
  %field_0_load162 = load ptr, ptr %field_0_load_ptr161, align 8
  %field_0_str_retain163 = call ptr @hew_string_clone(ptr %field_0_load162)
  store ptr %field_0_str_retain163, ptr %local_82, align 8
  %call_arg164 = load ptr, ptr %local_82, align 8
  %call_result165 = call ptr @"string::fmt"(ptr %call_arg164)
  store ptr %call_result165, ptr %local_83, align 8
  br label %bb34

bb34:                                             ; preds = %bb33
  %call_arg166 = load ptr, ptr %local_81, align 8
  %call_arg167 = load ptr, ptr %local_83, align 8
  %call_result168 = call ptr @hew_string_concat(ptr %call_arg166, ptr %call_arg167)
  store ptr %call_result168, ptr %local_84, align 8
  br label %bb35

bb35:                                             ; preds = %bb34
  %"hew_string_drop drop169" = load ptr, ptr %local_83, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop169")
  store ptr null, ptr %local_83, align 8
  %"hew_string_drop drop170" = load ptr, ptr %local_81, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop170")
  store ptr null, ptr %local_81, align 8
  store ptr @str_lit.9, ptr %local_85, align 8
  %call_arg171 = load ptr, ptr %local_84, align 8
  %call_arg172 = load ptr, ptr %local_85, align 8
  %call_result173 = call ptr @hew_string_concat(ptr %call_arg171, ptr %call_arg172)
  store ptr %call_result173, ptr %local_86, align 8
  br label %bb36

bb36:                                             ; preds = %bb35
  %"hew_string_drop drop174" = load ptr, ptr %local_84, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop174")
  store ptr null, ptr %local_84, align 8
  %call_arg175 = load i64, ptr %local_64, align 8
  %call_result176 = call ptr @hew_i64_to_string(i64 %call_arg175)
  store ptr %call_result176, ptr %local_87, align 8
  br label %bb37

bb37:                                             ; preds = %bb36
  %call_arg177 = load ptr, ptr %local_86, align 8
  %call_arg178 = load ptr, ptr %local_87, align 8
  %call_result179 = call ptr @hew_string_concat(ptr %call_arg177, ptr %call_arg178)
  store ptr %call_result179, ptr %local_88, align 8
  br label %bb38

bb38:                                             ; preds = %bb37
  %"hew_string_drop drop180" = load ptr, ptr %local_87, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop180")
  store ptr null, ptr %local_87, align 8
  %"hew_string_drop drop181" = load ptr, ptr %local_86, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop181")
  store ptr null, ptr %local_86, align 8
  store ptr @str_lit.10, ptr %local_89, align 8
  %call_arg182 = load ptr, ptr %local_88, align 8
  %call_arg183 = load ptr, ptr %local_89, align 8
  %call_result184 = call ptr @hew_string_concat(ptr %call_arg182, ptr %call_arg183)
  store ptr %call_result184, ptr %local_90, align 8
  br label %bb39

bb39:                                             ; preds = %bb38
  %"hew_string_drop drop185" = load ptr, ptr %local_88, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop185")
  store ptr null, ptr %local_88, align 8
  %call_arg186 = load i8, ptr %local_59, align 1
  %call_result187 = call ptr @hew_bool_to_string(i8 %call_arg186)
  store ptr %call_result187, ptr %local_91, align 8
  br label %bb40

bb40:                                             ; preds = %bb39
  %call_arg188 = load ptr, ptr %local_90, align 8
  %call_arg189 = load ptr, ptr %local_91, align 8
  %call_result190 = call ptr @hew_string_concat(ptr %call_arg188, ptr %call_arg189)
  store ptr %call_result190, ptr %local_92, align 8
  br label %bb41

bb41:                                             ; preds = %bb40
  %"hew_string_drop drop191" = load ptr, ptr %local_91, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop191")
  store ptr null, ptr %local_91, align 8
  %"hew_string_drop drop192" = load ptr, ptr %local_90, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop192")
  store ptr null, ptr %local_90, align 8
  %print_arg193 = load ptr, ptr %local_92, align 8
  %print_str_bits194 = ptrtoint ptr %print_arg193 to i64
  call void @hew_print_value(i8 4, i64 %print_str_bits194, i1 true)
  br label %bb42

bb42:                                             ; preds = %bb41
  %"hew_string_drop drop195" = load ptr, ptr %local_92, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop195")
  store ptr null, ptr %local_92, align 8
  %"hew_vec_free_owned drop196" = load ptr, ptr %local_15, align 8
  call void @hew_vec_free_owned(ptr %"hew_vec_free_owned drop196")
  store ptr null, ptr %local_15, align 8
  %"hew_vec_free_owned drop197" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free_owned(ptr %"hew_vec_free_owned drop197")
  store ptr null, ptr %local_1, align 8
  %hew_lambda_drain_all_call198 = call i32 @hew_lambda_drain_all(i64 0)
  ret i8 0

cancel_exit:                                      ; preds = %entry
  ret i8 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

vec_get_none:                                     ; preds = %bb5
  %machine_tag_ptr = getelementptr inbounds nuw %"Option$$tuple$xtuple$xRec$ci64$g$cbool$g", ptr %local_25, i32 0, i32 0
  store i8 1, ptr %machine_tag_ptr, align 1
  br label %bb6

vec_get_some:                                     ; preds = %bb5
  %machine_tag_ptr27 = getelementptr inbounds nuw %"Option$$tuple$xtuple$xRec$ci64$g$cbool$g", ptr %local_25, i32 0, i32 0
  store i8 0, ptr %machine_tag_ptr27, align 1
  br label %bb6

vec_get_none35:                                   ; preds = %bb7
  %machine_tag_ptr37 = getelementptr inbounds nuw %"Option$$tuple$xtuple$xRec$ci64$g$cbool$g", ptr %local_36, i32 0, i32 0
  store i8 1, ptr %machine_tag_ptr37, align 1
  br label %bb14

vec_get_some36:                                   ; preds = %bb7
  %machine_tag_ptr38 = getelementptr inbounds nuw %"Option$$tuple$xtuple$xRec$ci64$g$cbool$g", ptr %local_36, i32 0, i32 0
  store i8 0, ptr %machine_tag_ptr38, align 1
  br label %bb14

cancel_exit45:                                    ; preds = %bb8
  %"hew_vec_free_owned drop" = load ptr, ptr %local_15, align 8
  call void @hew_vec_free_owned(ptr %"hew_vec_free_owned drop")
  store ptr null, ptr %local_15, align 8
  %"hew_vec_free_owned drop47" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free_owned(ptr %"hew_vec_free_owned drop47")
  store ptr null, ptr %local_1, align 8
  ret i8 0

after_cooperate46:                                ; preds = %bb8
  br label %bb7

cancel_exit60:                                    ; preds = %bb13
  ret i8 0

after_cooperate61:                                ; preds = %bb13
  br label %bb7

cancel_exit102:                                   ; preds = %bb16
  %"hew_vec_free_owned drop104" = load ptr, ptr %local_15, align 8
  call void @hew_vec_free_owned(ptr %"hew_vec_free_owned drop104")
  store ptr null, ptr %local_15, align 8
  %"hew_vec_free_owned drop105" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free_owned(ptr %"hew_vec_free_owned drop105")
  store ptr null, ptr %local_1, align 8
  ret i8 0

after_cooperate103:                               ; preds = %bb16
  br label %bb15

cancel_exit121:                                   ; preds = %bb21
  ret i8 0

after_cooperate122:                               ; preds = %bb21
  br label %bb15
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
  store ptr @str_lit.11, ptr %local_3, align 8
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

define internal i32 @__hew_record_clone_inplace_Rec(ptr %0, ptr %1) {
entry:
  br label %step_0_clone

success:                                          ; preds = %step_0_store
  ret i32 0

fail:                                             ; preds = %rb_step_0
  ret i32 1

rb_step_0:                                        ; preds = %step_0_clone
  br label %fail

step_0_store:                                     ; preds = %step_0_clone
  %dst_f0_ptr = getelementptr inbounds nuw %Rec, ptr %1, i32 0, i32 0
  store ptr %clone_helper_f0, ptr %dst_f0_ptr, align 8
  br label %success

step_0_clone:                                     ; preds = %entry
  %src_f0_ptr = getelementptr inbounds nuw %Rec, ptr %0, i32 0, i32 0
  %src_f0 = load ptr, ptr %src_f0_ptr, align 8
  %clone_helper_f0 = call ptr @hew_string_clone(ptr %src_f0)
  %cloned_f0_int = ptrtoint ptr %clone_helper_f0 to i64
  %cloned_f0_null = icmp eq i64 %cloned_f0_int, 0
  br i1 %cloned_f0_null, label %rb_step_0, label %step_0_store
}

define internal void @__hew_record_drop_inplace_Rec(ptr %0) {
entry:
  %rec_int = ptrtoint ptr %0 to i64
  %rec_is_null = icmp eq i64 %rec_int, 0
  br i1 %rec_is_null, label %done, label %do_drop

do_drop:                                          ; preds = %entry
  %drop_f0_ptr = getelementptr inbounds nuw %Rec, ptr %0, i32 0, i32 0
  %drop_f0 = load ptr, ptr %drop_f0_ptr, align 8
  call void @hew_string_drop(ptr %drop_f0)
  br label %done

done:                                             ; preds = %do_drop, %entry
  ret void
}

declare void @hew_string_drop(ptr)

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

define internal void @__hew_record_overwrite_release_Rec(ptr %0, ptr %1) {
entry:
  %ow_slot_0 = alloca ptr, align 8
  store ptr null, ptr %ow_slot_0, align 8
  %ow_new_d0_f0_ptr = getelementptr inbounds nuw %Rec, ptr %1, i32 0, i32 0
  %ow_new_d0_f0_leaf = load ptr, ptr %ow_new_d0_f0_ptr, align 8
  store ptr %ow_new_d0_f0_leaf, ptr %ow_slot_0, align 8
  %ow_old_d0_f0_ptr = getelementptr inbounds nuw %Rec, ptr %0, i32 0, i32 0
  %ow_old_d0_f0_val = load ptr, ptr %ow_old_d0_f0_ptr, align 8
  %ow_old_d0_f0_int = ptrtoint ptr %ow_old_d0_f0_val to i64
  %ow_old_d0_f0_cmp0_leaf = load ptr, ptr %ow_slot_0, align 8
  %ow_old_d0_f0_cmp0_int = ptrtoint ptr %ow_old_d0_f0_cmp0_leaf to i64
  %ow_old_d0_f0_cmp0_eq = icmp eq i64 %ow_old_d0_f0_int, %ow_old_d0_f0_cmp0_int
  %ow_old_d0_f0_matched0 = or i1 false, %ow_old_d0_f0_cmp0_eq
  %ow_old_d0_f0_neutralized = select i1 %ow_old_d0_f0_matched0, ptr null, ptr %ow_old_d0_f0_val
  store ptr %ow_old_d0_f0_neutralized, ptr %ow_old_d0_f0_ptr, align 8
  call void @__hew_record_drop_inplace_Rec(ptr %0)
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

declare i32 @hew_actor_cooperate()

declare ptr @hew_vec_new_with_elem_layout(ptr)

define internal i32 @"__hew_tuple_clone_inplace_tuple_tuple$xRec$ci64$g_bool"(ptr %0, ptr %1) {
entry:
  br label %step_0_clone

success:                                          ; preds = %step_0_store
  ret i32 0

fail:                                             ; preds = %rb_step_0
  ret i32 1

rb_step_0:                                        ; preds = %step_0_clone
  br label %fail

step_0_store:                                     ; preds = %step_0_clone
  br label %success

step_0_clone:                                     ; preds = %entry
  %src_f0_tuple_ptr = getelementptr inbounds nuw { { %Rec, i64 }, i8 }, ptr %0, i32 0, i32 0
  %dst_f0_tuple_ptr = getelementptr inbounds nuw { { %Rec, i64 }, i8 }, ptr %1, i32 0, i32 0
  %tuple_clone_inplace_f0 = call i32 @__hew_tuple_clone_inplace_kind_tuple_record_Rec_bit8(ptr %src_f0_tuple_ptr, ptr %dst_f0_tuple_ptr)
  %tuple_clone_failed_f0 = icmp ne i32 %tuple_clone_inplace_f0, 0
  br i1 %tuple_clone_failed_f0, label %rb_step_0, label %step_0_store
}

define internal i32 @__hew_tuple_clone_inplace_kind_tuple_record_Rec_bit8(ptr %0, ptr %1) {
entry:
  br label %step_0_clone

success:                                          ; preds = %step_0_store
  ret i32 0

fail:                                             ; preds = %rb_step_0
  ret i32 1

rb_step_0:                                        ; preds = %step_0_clone
  br label %fail

step_0_store:                                     ; preds = %step_0_clone
  br label %success

step_0_clone:                                     ; preds = %entry
  %src_f0_ptr = getelementptr inbounds nuw { %Rec, i64 }, ptr %0, i32 0, i32 0
  %dst_f0_ptr = getelementptr inbounds nuw { %Rec, i64 }, ptr %1, i32 0, i32 0
  %record_clone_inplace_f0 = call i32 @__hew_record_clone_inplace_Rec(ptr %src_f0_ptr, ptr %dst_f0_ptr)
  %record_clone_failed_f0 = icmp ne i32 %record_clone_inplace_f0, 0
  br i1 %record_clone_failed_f0, label %rb_step_0, label %step_0_store
}

define internal void @__hew_tuple_drop_inplace_kind_tuple_record_Rec_bit8(ptr %0) {
entry:
  %rec_int = ptrtoint ptr %0 to i64
  %rec_is_null = icmp eq i64 %rec_int, 0
  br i1 %rec_is_null, label %done, label %do_drop

do_drop:                                          ; preds = %entry
  %drop_f0_ptr = getelementptr inbounds nuw { %Rec, i64 }, ptr %0, i32 0, i32 0
  call void @__hew_record_drop_inplace_Rec(ptr %drop_f0_ptr)
  br label %done

done:                                             ; preds = %do_drop, %entry
  ret void
}

define internal void @"__hew_tuple_drop_inplace_tuple_tuple$xRec$ci64$g_bool"(ptr %0) {
entry:
  %rec_int = ptrtoint ptr %0 to i64
  %rec_is_null = icmp eq i64 %rec_int, 0
  br i1 %rec_is_null, label %done, label %do_drop

do_drop:                                          ; preds = %entry
  %drop_f0_tuple_ptr = getelementptr inbounds nuw { { %Rec, i64 }, i8 }, ptr %0, i32 0, i32 0
  call void @__hew_tuple_drop_inplace_kind_tuple_record_Rec_bit8(ptr %drop_f0_tuple_ptr)
  br label %done

done:                                             ; preds = %do_drop, %entry
  ret void
}

declare void @hew_vec_push_owned_move(ptr, ptr)

declare ptr @hew_vec_clone_owned(ptr)

declare void @hew_vec_set_owned_move(ptr, i64, ptr)

declare i1 @hew_vec_get_clone(ptr, i64, ptr)

declare void @hew_vec_free_owned(ptr)

declare void @hew_trap_with_code(i32)

; Function Attrs: cold noreturn nounwind memory(inaccessiblemem: write)
declare void @llvm.trap() #0

declare i32 @hew_lambda_drain_all(i64)

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.smul.with.overflow.i64(i64, i64) #1

attributes #0 = { cold noreturn nounwind memory(inaccessiblemem: write) }
attributes #1 = { nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none) }
