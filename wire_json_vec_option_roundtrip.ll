; ModuleID = 'wire_json_vec_option_roundtrip'
source_filename = "wire_json_vec_option_roundtrip"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "aarch64-apple-macosx13.0"

%"Option$$string" = type { i8, [1 x i64] }
%Bag = type { ptr, %"Option$$string" }
%"Result$$Bag$string" = type { i8, [3 x i64] }
%CrashInfo = type { i64, ptr }

@__hew_wire_desc_json_Bag = private unnamed_addr constant [125 x i8] c"{\22k\22:\22struct\22,\22f\22:[{\22t\22:1,\22n\22:\22items\22,\22d\22:{\22k\22:\22vec\22,\22e\22:{\22k\22:\22i64\22}}},{\22t\22:2,\22n\22:\22label\22,\22d\22:{\22k\22:\22opt\22,\22e\22:{\22k\22:\22str\22}}}]}\00"
@__hew_wire_desc_yaml_Bag = private unnamed_addr constant [125 x i8] c"{\22k\22:\22struct\22,\22f\22:[{\22t\22:1,\22n\22:\22items\22,\22d\22:{\22k\22:\22vec\22,\22e\22:{\22k\22:\22i64\22}}},{\22t\22:2,\22n\22:\22label\22,\22d\22:{\22k\22:\22opt\22,\22e\22:{\22k\22:\22str\22}}}]}\00"
@str_lit = private unnamed_addr constant [4 x i8] c"tag\00", align 1
@__hew_wire_text_msg_wire_value_does_not_match_the_target_type = private unnamed_addr constant [42 x i8] c"wire value does not match the target type\00"
@str_lit.1 = private unnamed_addr constant [4 x i8] c"tag\00", align 1
@str_lit.2 = private unnamed_addr constant [3 x i8] c"ns\00", align 1

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

define i64 @main() {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i64, align 8
  %local_5 = alloca %"Option$$string", align 8
  %local_6 = alloca i64, align 8
  %local_7 = alloca ptr, align 8
  %local_8 = alloca %Bag, align 8
  %local_9 = alloca %Bag, align 8
  %local_10 = alloca ptr, align 8
  %local_11 = alloca ptr, align 8
  %local_12 = alloca i64, align 8
  %local_13 = alloca %"Result$$Bag$string", align 8
  %local_14 = alloca i64, align 8
  %local_15 = alloca i64, align 8
  %local_16 = alloca i8, align 1
  %local_17 = alloca i64, align 8
  %local_18 = alloca i8, align 1
  %local_19 = alloca %Bag, align 8
  %local_20 = alloca i8, align 1
  %local_21 = alloca ptr, align 8
  %local_22 = alloca i64, align 8
  %local_23 = alloca i64, align 8
  %local_24 = alloca i8, align 1
  %local_25 = alloca i64, align 8
  %local_26 = alloca i8, align 1
  %local_27 = alloca %"Option$$string", align 8
  %local_28 = alloca i64, align 8
  %local_29 = alloca i64, align 8
  %local_30 = alloca i8, align 1
  %local_31 = alloca i64, align 8
  %local_32 = alloca i8, align 1
  %local_33 = alloca ptr, align 8
  %local_34 = alloca i8, align 1
  %local_35 = alloca ptr, align 8
  %local_36 = alloca i8, align 1
  %local_37 = alloca i64, align 8
  %local_38 = alloca i8, align 1
  %local_39 = alloca i64, align 8
  %local_40 = alloca ptr, align 8
  %local_41 = alloca i64, align 8
  %local_42 = alloca i64, align 8
  %local_43 = alloca i8, align 1
  %local_44 = alloca i64, align 8
  %local_45 = alloca ptr, align 8
  %local_46 = alloca i64, align 8
  %local_47 = alloca i64, align 8
  %local_48 = alloca i8, align 1
  %local_49 = alloca i64, align 8
  %local_50 = alloca i64, align 8
  %local_51 = alloca i8, align 1
  %local_52 = alloca ptr, align 8
  %local_53 = alloca i64, align 8
  %local_54 = alloca i64, align 8
  %local_55 = alloca i8, align 1
  %local_56 = alloca i64, align 8
  %local_57 = alloca i64, align 8
  %local_58 = alloca i8, align 1
  %local_59 = alloca i64, align 8
  %local_60 = alloca i64, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %hew_vec_new_i64_call = call ptr @hew_vec_new_i64()
  store ptr %hew_vec_new_i64_call, ptr %local_0, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_0, align 8
  store ptr %move_load, ptr %local_1, align 8
  store i64 10, ptr %local_2, align 8
  %call_arg = load ptr, ptr %local_1, align 8
  %call_arg1 = load i64, ptr %local_2, align 8
  call void @hew_vec_push_i64(ptr %call_arg, i64 %call_arg1)
  br label %bb2

bb2:                                              ; preds = %bb1
  store i64 20, ptr %local_3, align 8
  %call_arg2 = load ptr, ptr %local_1, align 8
  %call_arg3 = load i64, ptr %local_3, align 8
  call void @hew_vec_push_i64(ptr %call_arg2, i64 %call_arg3)
  br label %bb3

bb3:                                              ; preds = %bb2
  store i64 12, ptr %local_4, align 8
  %call_arg4 = load ptr, ptr %local_1, align 8
  %call_arg5 = load i64, ptr %local_4, align 8
  call void @hew_vec_push_i64(ptr %call_arg4, i64 %call_arg5)
  br label %bb4

bb4:                                              ; preds = %bb3
  store i64 0, ptr %local_6, align 8
  %machine_tag_ptr = getelementptr inbounds nuw %"Option$$string", ptr %local_5, i32 0, i32 0
  %move_iN_load_wide = load i64, ptr %local_6, align 8
  %move_iN_trunc = trunc i64 %move_iN_load_wide to i8
  store i8 %move_iN_trunc, ptr %machine_tag_ptr, align 1
  store ptr @str_lit, ptr %local_7, align 8
  %machine_payload_ptr = getelementptr inbounds nuw %"Option$$string", ptr %local_5, i32 0, i32 1
  %machine_variant_field_ptr = getelementptr inbounds nuw { ptr }, ptr %machine_payload_ptr, i32 0, i32 0
  %move_load6 = load ptr, ptr %local_7, align 8
  store ptr %move_load6, ptr %machine_variant_field_ptr, align 8
  %field_0_init_ptr = getelementptr inbounds nuw %Bag, ptr %local_8, i32 0, i32 0
  %field_0_init_src = load ptr, ptr %local_1, align 8
  store ptr %field_0_init_src, ptr %field_0_init_ptr, align 8
  %field_1_init_ptr = getelementptr inbounds nuw %Bag, ptr %local_8, i32 0, i32 1
  %field_1_init_src = load %"Option$$string", ptr %local_5, align 8
  store %"Option$$string" %field_1_init_src, ptr %field_1_init_ptr, align 8
  %move_load7 = load %Bag, ptr %local_8, align 8
  store %Bag %move_load7, ptr %local_9, align 8
  %wire_text_ser_len = alloca i64, align 8
  %wire_text_ser_cbor = call ptr @__hew_cbor_serialize_Bag(ptr %local_9, ptr %wire_text_ser_len)
  %wire_text_ser_cbor_len = load i64, ptr %wire_text_ser_len, align 8
  %wire_text_ser_text = call ptr @hew_wire_cbor_to_text(ptr %wire_text_ser_cbor, i64 %wire_text_ser_cbor_len, ptr @__hew_wire_desc_json_Bag, i32 0)
  call void @hew_ser_free_bytes(ptr %wire_text_ser_cbor)
  %wire_text_ser_is_null = icmp eq ptr %wire_text_ser_text, null
  br i1 %wire_text_ser_is_null, label %wire_text_ser_fail, label %wire_text_ser_ok

bb5:                                              ; preds = %after_cooperate149, %after_cooperate28
  %move_load19 = load i64, ptr %local_12, align 8
  store i64 %move_load19, ptr %return_slot, align 8
  %"hew_string_drop drop" = load ptr, ptr %local_11, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop")
  store ptr null, ptr %local_11, align 8
  %hew_lambda_drain_all_call = call i32 @hew_lambda_drain_all(i64 0)
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val

bb6:                                              ; preds = %wire_text_de_join
  %machine_payload_ptr20 = getelementptr inbounds nuw %"Result$$Bag$string", ptr %local_13, i32 0, i32 1
  %machine_variant_field_ptr21 = getelementptr inbounds nuw { %Bag }, ptr %machine_payload_ptr20, i32 0, i32 0
  %move_load22 = load %Bag, ptr %machine_variant_field_ptr21, align 8
  store %Bag %move_load22, ptr %local_19, align 8
  %field_0_load_ptr = getelementptr inbounds nuw %Bag, ptr %local_19, i32 0, i32 0
  %field_0_load = load ptr, ptr %field_0_load_ptr, align 8
  store ptr %field_0_load, ptr %local_21, align 8
  %call_arg23 = load ptr, ptr %local_21, align 8
  %call_result = call i64 @hew_vec_len(ptr %call_arg23)
  store i64 %call_result, ptr %local_22, align 8
  br label %bb10

bb7:                                              ; preds = %bb9
  store i64 4, ptr %local_60, align 8
  %move_load24 = load i64, ptr %local_60, align 8
  store i64 %move_load24, ptr %local_12, align 8
  %hew_actor_cooperate25 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel26 = icmp eq i32 %hew_actor_cooperate25, 2
  br i1 %hew_cooperate_is_cancel26, label %cancel_exit27, label %after_cooperate28

bb8:                                              ; preds = %bb9
  %"hew_string_drop drop30" = load ptr, ptr %local_11, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop30")
  store ptr null, ptr %local_11, align 8
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

bb9:                                              ; preds = %wire_text_de_join
  store i64 1, ptr %local_17, align 8
  %cmp_lhs31 = load i64, ptr %local_14, align 8
  %cmp_rhs32 = load i64, ptr %local_17, align 8
  %cmp_bit33 = icmp eq i64 %cmp_lhs31, %cmp_rhs32
  %cmp_zext34 = zext i1 %cmp_bit33 to i8
  store i8 %cmp_zext34, ptr %local_18, align 1
  %cond_load35 = load i8, ptr %local_18, align 1
  %cond_nz36 = icmp ne i8 %cond_load35, 0
  br i1 %cond_nz36, label %bb7, label %bb8

bb10:                                             ; preds = %bb6
  store i64 3, ptr %local_23, align 8
  %cmp_lhs37 = load i64, ptr %local_22, align 8
  %cmp_rhs38 = load i64, ptr %local_23, align 8
  %cmp_bit39 = icmp ne i64 %cmp_lhs37, %cmp_rhs38
  %cmp_zext40 = zext i1 %cmp_bit39 to i8
  store i8 %cmp_zext40, ptr %local_24, align 1
  %cond_load41 = load i8, ptr %local_24, align 1
  %cond_nz42 = icmp ne i8 %cond_load41, 0
  br i1 %cond_nz42, label %bb11, label %bb12

bb11:                                             ; preds = %bb10
  store i64 1, ptr %local_25, align 8
  %move_load43 = load i64, ptr %local_25, align 8
  store i64 %move_load43, ptr %return_slot, align 8
  %"hew_string_drop drop44" = load ptr, ptr %local_11, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop44")
  store ptr null, ptr %local_11, align 8
  %hew_lambda_drain_all_call45 = call i32 @hew_lambda_drain_all(i64 0)
  %ret_val46 = load i64, ptr %return_slot, align 8
  ret i64 %ret_val46

bb12:                                             ; preds = %bb10
  br label %bb13

bb13:                                             ; preds = %after_cooperate59, %bb12
  %field_1_load_ptr = getelementptr inbounds nuw %Bag, ptr %local_19, i32 0, i32 1
  %field_1_load = load %"Option$$string", ptr %field_1_load_ptr, align 8
  store %"Option$$string" %field_1_load, ptr %local_27, align 8
  %machine_tag_ptr47 = getelementptr inbounds nuw %"Option$$string", ptr %local_27, i32 0, i32 0
  %move_iN_load48 = load i8, ptr %machine_tag_ptr47, align 1
  %move_iN_zext49 = zext i8 %move_iN_load48 to i64
  store i64 %move_iN_zext49, ptr %local_28, align 8
  store i64 0, ptr %local_29, align 8
  %cmp_lhs50 = load i64, ptr %local_28, align 8
  %cmp_rhs51 = load i64, ptr %local_29, align 8
  %cmp_bit52 = icmp eq i64 %cmp_lhs50, %cmp_rhs51
  %cmp_zext53 = zext i1 %cmp_bit52 to i8
  store i8 %cmp_zext53, ptr %local_30, align 1
  %cond_load54 = load i8, ptr %local_30, align 1
  %cond_nz55 = icmp ne i8 %cond_load54, 0
  br i1 %cond_nz55, label %bb16, label %bb19

bb14:                                             ; No predecessors!
  %hew_actor_cooperate56 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel57 = icmp eq i32 %hew_actor_cooperate56, 2
  br i1 %hew_cooperate_is_cancel57, label %cancel_exit58, label %after_cooperate59

bb15:                                             ; preds = %after_cooperate102, %after_cooperate93
  %field_0_load_ptr60 = getelementptr inbounds nuw %Bag, ptr %local_19, i32 0, i32 0
  %field_0_load61 = load ptr, ptr %field_0_load_ptr60, align 8
  store ptr %field_0_load61, ptr %local_40, align 8
  store i64 0, ptr %local_41, align 8
  %"hew_vec_len arg0" = load ptr, ptr %local_40, align 8
  %hew_vec_len_call = call i64 @hew_vec_len(ptr %"hew_vec_len arg0")
  store i64 %hew_vec_len_call, ptr %local_42, align 8
  %cmp_lhs62 = load i64, ptr %local_41, align 8
  %cmp_rhs63 = load i64, ptr %local_42, align 8
  %cmp_bit64 = icmp uge i64 %cmp_lhs62, %cmp_rhs63
  %cmp_zext65 = zext i1 %cmp_bit64 to i8
  store i8 %cmp_zext65, ptr %local_43, align 1
  %cond_load66 = load i8, ptr %local_43, align 1
  %cond_nz67 = icmp ne i8 %cond_load66, 0
  br i1 %cond_nz67, label %bb25, label %bb26

bb16:                                             ; preds = %bb13
  %machine_payload_ptr68 = getelementptr inbounds nuw %"Option$$string", ptr %local_27, i32 0, i32 1
  %machine_variant_field_ptr69 = getelementptr inbounds nuw { ptr }, ptr %machine_payload_ptr68, i32 0, i32 0
  %move_load70 = load ptr, ptr %machine_variant_field_ptr69, align 8
  store ptr %move_load70, ptr %local_33, align 8
  store ptr @str_lit.1, ptr %local_35, align 8
  %string_cmp_lhs = load ptr, ptr %local_33, align 8
  %string_cmp_rhs = load ptr, ptr %local_35, align 8
  %hew_string_equals = call i32 @hew_string_equals(ptr %string_cmp_lhs, ptr %string_cmp_rhs)
  %string_cmp_bit = icmp eq i32 %hew_string_equals, 0
  %string_cmp_zext = zext i1 %string_cmp_bit to i8
  store i8 %string_cmp_zext, ptr %local_36, align 1
  %cond_load71 = load i8, ptr %local_36, align 1
  %cond_nz72 = icmp ne i8 %cond_load71, 0
  br i1 %cond_nz72, label %bb20, label %bb21

bb17:                                             ; preds = %bb19
  store i64 3, ptr %local_39, align 8
  %move_load73 = load i64, ptr %local_39, align 8
  store i64 %move_load73, ptr %return_slot, align 8
  %"hew_string_drop drop74" = load ptr, ptr %local_11, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop74")
  store ptr null, ptr %local_11, align 8
  %hew_lambda_drain_all_call75 = call i32 @hew_lambda_drain_all(i64 0)
  %ret_val76 = load i64, ptr %return_slot, align 8
  ret i64 %ret_val76

bb18:                                             ; preds = %bb19
  %"hew_string_drop drop77" = load ptr, ptr %local_11, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop77")
  store ptr null, ptr %local_11, align 8
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

bb19:                                             ; preds = %bb13
  store i64 1, ptr %local_31, align 8
  %cmp_lhs78 = load i64, ptr %local_28, align 8
  %cmp_rhs79 = load i64, ptr %local_31, align 8
  %cmp_bit80 = icmp eq i64 %cmp_lhs78, %cmp_rhs79
  %cmp_zext81 = zext i1 %cmp_bit80 to i8
  store i8 %cmp_zext81, ptr %local_32, align 1
  %cond_load82 = load i8, ptr %local_32, align 1
  %cond_nz83 = icmp ne i8 %cond_load82, 0
  br i1 %cond_nz83, label %bb17, label %bb18

bb20:                                             ; preds = %bb16
  store i64 2, ptr %local_37, align 8
  %move_load84 = load i64, ptr %local_37, align 8
  store i64 %move_load84, ptr %return_slot, align 8
  %"hew_string_drop drop85" = load ptr, ptr %local_11, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop85")
  store ptr null, ptr %local_11, align 8
  %hew_lambda_drain_all_call86 = call i32 @hew_lambda_drain_all(i64 0)
  %ret_val87 = load i64, ptr %return_slot, align 8
  ret i64 %ret_val87

bb21:                                             ; preds = %bb16
  br label %bb22

bb22:                                             ; preds = %after_cooperate98, %bb21
  %move_load88 = load i8, ptr %local_34, align 1
  store i8 %move_load88, ptr %local_38, align 1
  %move_load89 = load i8, ptr %local_38, align 1
  store i8 %move_load89, ptr %local_26, align 1
  %hew_actor_cooperate90 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel91 = icmp eq i32 %hew_actor_cooperate90, 2
  br i1 %hew_cooperate_is_cancel91, label %cancel_exit92, label %after_cooperate93

bb23:                                             ; No predecessors!
  %hew_actor_cooperate95 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel96 = icmp eq i32 %hew_actor_cooperate95, 2
  br i1 %hew_cooperate_is_cancel96, label %cancel_exit97, label %after_cooperate98

bb24:                                             ; No predecessors!
  %hew_actor_cooperate99 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel100 = icmp eq i32 %hew_actor_cooperate99, 2
  br i1 %hew_cooperate_is_cancel100, label %cancel_exit101, label %after_cooperate102

bb25:                                             ; preds = %bb15
  %"hew_string_drop drop103" = load ptr, ptr %local_11, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop103")
  store ptr null, ptr %local_11, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb26:                                             ; preds = %bb15
  %"hew_vec_get_i64 arg0" = load ptr, ptr %local_40, align 8
  %"hew_vec_get_i64 arg1" = load i64, ptr %local_41, align 8
  %hew_vec_get_i64_call = call i64 @hew_vec_get_i64(ptr %"hew_vec_get_i64 arg0", i64 %"hew_vec_get_i64 arg1")
  store i64 %hew_vec_get_i64_call, ptr %local_44, align 8
  %field_0_load_ptr104 = getelementptr inbounds nuw %Bag, ptr %local_19, i32 0, i32 0
  %field_0_load105 = load ptr, ptr %field_0_load_ptr104, align 8
  store ptr %field_0_load105, ptr %local_45, align 8
  store i64 1, ptr %local_46, align 8
  %"hew_vec_len arg0106" = load ptr, ptr %local_45, align 8
  %hew_vec_len_call107 = call i64 @hew_vec_len(ptr %"hew_vec_len arg0106")
  store i64 %hew_vec_len_call107, ptr %local_47, align 8
  %cmp_lhs108 = load i64, ptr %local_46, align 8
  %cmp_rhs109 = load i64, ptr %local_47, align 8
  %cmp_bit110 = icmp uge i64 %cmp_lhs108, %cmp_rhs109
  %cmp_zext111 = zext i1 %cmp_bit110 to i8
  store i8 %cmp_zext111, ptr %local_48, align 1
  %cond_load112 = load i8, ptr %local_48, align 1
  %cond_nz113 = icmp ne i8 %cond_load112, 0
  br i1 %cond_nz113, label %bb27, label %bb28

bb27:                                             ; preds = %bb26
  %"hew_string_drop drop114" = load ptr, ptr %local_11, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop114")
  store ptr null, ptr %local_11, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb28:                                             ; preds = %bb26
  %"hew_vec_get_i64 arg0115" = load ptr, ptr %local_45, align 8
  %"hew_vec_get_i64 arg1116" = load i64, ptr %local_46, align 8
  %hew_vec_get_i64_call117 = call i64 @hew_vec_get_i64(ptr %"hew_vec_get_i64 arg0115", i64 %"hew_vec_get_i64 arg1116")
  store i64 %hew_vec_get_i64_call117, ptr %local_49, align 8
  %checked_lhs = load i64, ptr %local_44, align 8
  %checked_rhs = load i64, ptr %local_49, align 8
  %with_overflow = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_50, align 8
  store i8 %checked_overflow_widen, ptr %local_51, align 1
  %cond_load118 = load i8, ptr %local_51, align 1
  %cond_nz119 = icmp ne i8 %cond_load118, 0
  br i1 %cond_nz119, label %bb29, label %bb30

bb29:                                             ; preds = %bb28
  %"hew_string_drop drop120" = load ptr, ptr %local_11, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop120")
  store ptr null, ptr %local_11, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb30:                                             ; preds = %bb28
  %field_0_load_ptr121 = getelementptr inbounds nuw %Bag, ptr %local_19, i32 0, i32 0
  %field_0_load122 = load ptr, ptr %field_0_load_ptr121, align 8
  store ptr %field_0_load122, ptr %local_52, align 8
  store i64 2, ptr %local_53, align 8
  %"hew_vec_len arg0123" = load ptr, ptr %local_52, align 8
  %hew_vec_len_call124 = call i64 @hew_vec_len(ptr %"hew_vec_len arg0123")
  store i64 %hew_vec_len_call124, ptr %local_54, align 8
  %cmp_lhs125 = load i64, ptr %local_53, align 8
  %cmp_rhs126 = load i64, ptr %local_54, align 8
  %cmp_bit127 = icmp uge i64 %cmp_lhs125, %cmp_rhs126
  %cmp_zext128 = zext i1 %cmp_bit127 to i8
  store i8 %cmp_zext128, ptr %local_55, align 1
  %cond_load129 = load i8, ptr %local_55, align 1
  %cond_nz130 = icmp ne i8 %cond_load129, 0
  br i1 %cond_nz130, label %bb31, label %bb32

bb31:                                             ; preds = %bb30
  %"hew_string_drop drop131" = load ptr, ptr %local_11, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop131")
  store ptr null, ptr %local_11, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb32:                                             ; preds = %bb30
  %"hew_vec_get_i64 arg0132" = load ptr, ptr %local_52, align 8
  %"hew_vec_get_i64 arg1133" = load i64, ptr %local_53, align 8
  %hew_vec_get_i64_call134 = call i64 @hew_vec_get_i64(ptr %"hew_vec_get_i64 arg0132", i64 %"hew_vec_get_i64 arg1133")
  store i64 %hew_vec_get_i64_call134, ptr %local_56, align 8
  %checked_lhs135 = load i64, ptr %local_50, align 8
  %checked_rhs136 = load i64, ptr %local_56, align 8
  %with_overflow137 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs135, i64 %checked_rhs136)
  %checked_result138 = extractvalue { i64, i1 } %with_overflow137, 0
  %checked_overflow139 = extractvalue { i64, i1 } %with_overflow137, 1
  %checked_overflow_widen140 = zext i1 %checked_overflow139 to i8
  store i64 %checked_result138, ptr %local_57, align 8
  store i8 %checked_overflow_widen140, ptr %local_58, align 1
  %cond_load141 = load i8, ptr %local_58, align 1
  %cond_nz142 = icmp ne i8 %cond_load141, 0
  br i1 %cond_nz142, label %bb33, label %bb34

bb33:                                             ; preds = %bb32
  %"hew_string_drop drop143" = load ptr, ptr %local_11, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop143")
  store ptr null, ptr %local_11, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb34:                                             ; preds = %bb32
  %move_load144 = load i64, ptr %local_57, align 8
  store i64 %move_load144, ptr %local_59, align 8
  %move_load145 = load i64, ptr %local_59, align 8
  store i64 %move_load145, ptr %local_12, align 8
  %hew_actor_cooperate146 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel147 = icmp eq i32 %hew_actor_cooperate146, 2
  br i1 %hew_cooperate_is_cancel147, label %cancel_exit148, label %after_cooperate149

cancel_exit:                                      ; preds = %entry
  ret i64 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

wire_text_ser_fail:                               ; preds = %bb4
  call void @hew_trap_with_code(i32 210)
  call void @llvm.trap()
  unreachable

wire_text_ser_ok:                                 ; preds = %bb4
  store ptr %wire_text_ser_text, ptr %local_10, align 8
  %move_load8 = load ptr, ptr %local_10, align 8
  store ptr %move_load8, ptr %local_11, align 8
  %wire_text_de_text = load ptr, ptr %local_11, align 8
  %wire_text_de_len = alloca i64, align 8
  %wire_text_de_err = alloca ptr, align 8
  store ptr null, ptr %wire_text_de_err, align 8
  %wire_text_de_cbor = call ptr @hew_wire_text_to_cbor(ptr %wire_text_de_text, ptr @__hew_wire_desc_json_Bag, i32 0, ptr %wire_text_de_len, ptr %wire_text_de_err)
  %wire_text_de_parse_failed = icmp eq ptr %wire_text_de_cbor, null
  br i1 %wire_text_de_parse_failed, label %wire_text_de_parse_err, label %wire_text_de_decode

wire_text_de_parse_err:                           ; preds = %wire_text_ser_ok
  %wire_text_de_err_str = load ptr, ptr %wire_text_de_err, align 8
  %machine_tag_ptr9 = getelementptr inbounds nuw %"Result$$Bag$string", ptr %local_13, i32 0, i32 0
  store i8 1, ptr %machine_tag_ptr9, align 1
  %machine_payload_ptr10 = getelementptr inbounds nuw %"Result$$Bag$string", ptr %local_13, i32 0, i32 1
  %machine_variant_field_ptr11 = getelementptr inbounds nuw { ptr }, ptr %machine_payload_ptr10, i32 0, i32 0
  store ptr %wire_text_de_err_str, ptr %machine_variant_field_ptr11, align 8
  br label %wire_text_de_join

wire_text_de_decode:                              ; preds = %wire_text_ser_ok
  %wire_text_de_struct_size = alloca i64, align 8
  %wire_text_de_cbor_len = load i64, ptr %wire_text_de_len, align 8
  %wire_text_de_value = call ptr @__hew_cbor_deserialize_Bag(ptr %wire_text_de_cbor, i64 %wire_text_de_cbor_len, ptr %wire_text_de_struct_size)
  call void @hew_ser_free_bytes(ptr %wire_text_de_cbor)
  %wire_text_de_decode_failed = icmp eq ptr %wire_text_de_value, null
  br i1 %wire_text_de_decode_failed, label %wire_text_de_decode_err, label %wire_text_de_decode_ok

wire_text_de_join:                                ; preds = %wire_text_de_decode_ok, %wire_text_de_decode_err, %wire_text_de_parse_err
  %machine_tag_ptr18 = getelementptr inbounds nuw %"Result$$Bag$string", ptr %local_13, i32 0, i32 0
  %move_iN_load = load i8, ptr %machine_tag_ptr18, align 1
  %move_iN_zext = zext i8 %move_iN_load to i64
  store i64 %move_iN_zext, ptr %local_14, align 8
  store i64 0, ptr %local_15, align 8
  %cmp_lhs = load i64, ptr %local_14, align 8
  %cmp_rhs = load i64, ptr %local_15, align 8
  %cmp_bit = icmp eq i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_16, align 1
  %cond_load = load i8, ptr %local_16, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb6, label %bb9

wire_text_de_decode_err:                          ; preds = %wire_text_de_decode
  %machine_tag_ptr12 = getelementptr inbounds nuw %"Result$$Bag$string", ptr %local_13, i32 0, i32 0
  store i8 1, ptr %machine_tag_ptr12, align 1
  %machine_payload_ptr13 = getelementptr inbounds nuw %"Result$$Bag$string", ptr %local_13, i32 0, i32 1
  %machine_variant_field_ptr14 = getelementptr inbounds nuw { ptr }, ptr %machine_payload_ptr13, i32 0, i32 0
  store ptr @__hew_wire_text_msg_wire_value_does_not_match_the_target_type, ptr %machine_variant_field_ptr14, align 8
  br label %wire_text_de_join

wire_text_de_decode_ok:                           ; preds = %wire_text_de_decode
  %machine_payload_ptr15 = getelementptr inbounds nuw %"Result$$Bag$string", ptr %local_13, i32 0, i32 1
  %machine_variant_field_ptr16 = getelementptr inbounds nuw { %Bag }, ptr %machine_payload_ptr15, i32 0, i32 0
  %wire_text_de_value_load = load %Bag, ptr %wire_text_de_value, align 8
  %machine_tag_ptr17 = getelementptr inbounds nuw %"Result$$Bag$string", ptr %local_13, i32 0, i32 0
  store i8 0, ptr %machine_tag_ptr17, align 1
  store %Bag %wire_text_de_value_load, ptr %machine_variant_field_ptr16, align 8
  call void @free(ptr %wire_text_de_value)
  br label %wire_text_de_join

cancel_exit27:                                    ; preds = %bb7
  %"hew_string_drop drop29" = load ptr, ptr %local_11, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop29")
  store ptr null, ptr %local_11, align 8
  ret i64 0

after_cooperate28:                                ; preds = %bb7
  br label %bb5

cancel_exit58:                                    ; preds = %bb14
  ret i64 0

after_cooperate59:                                ; preds = %bb14
  br label %bb13

cancel_exit92:                                    ; preds = %bb22
  %"hew_string_drop drop94" = load ptr, ptr %local_11, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop94")
  store ptr null, ptr %local_11, align 8
  ret i64 0

after_cooperate93:                                ; preds = %bb22
  br label %bb15

cancel_exit97:                                    ; preds = %bb23
  ret i64 0

after_cooperate98:                                ; preds = %bb23
  br label %bb22

cancel_exit101:                                   ; preds = %bb24
  ret i64 0

after_cooperate102:                               ; preds = %bb24
  br label %bb15

cancel_exit148:                                   ; preds = %bb34
  %"hew_string_drop drop150" = load ptr, ptr %local_11, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop150")
  store ptr null, ptr %local_11, align 8
  ret i64 0

after_cooperate149:                               ; preds = %bb34
  br label %bb5
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
  store ptr @str_lit.2, ptr %local_3, align 8
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

define internal i32 @__hew_record_clone_inplace_Bag(ptr %0, ptr %1) {
entry:
  br label %step_0_clone

success:                                          ; preds = %step_1_store
  ret i32 0

fail:                                             ; preds = %rb_step_1, %rb_step_0
  ret i32 1

rb_step_0:                                        ; preds = %step_0_clone
  br label %fail

rb_step_1:                                        ; preds = %step_1_clone
  %drop_f0_ptr = getelementptr inbounds nuw %Bag, ptr %1, i32 0, i32 0
  %drop_f0 = load ptr, ptr %drop_f0_ptr, align 8
  call void @hew_vec_free_owned(ptr %drop_f0)
  br label %fail

step_0_store:                                     ; preds = %step_0_clone
  %dst_f0_ptr = getelementptr inbounds nuw %Bag, ptr %1, i32 0, i32 0
  store ptr %clone_helper_f0, ptr %dst_f0_ptr, align 8
  br label %step_1_clone

step_1_store:                                     ; preds = %step_1_clone
  br label %success

step_0_clone:                                     ; preds = %entry
  %src_f0_ptr = getelementptr inbounds nuw %Bag, ptr %0, i32 0, i32 0
  %src_f0 = load ptr, ptr %src_f0_ptr, align 8
  %clone_helper_f0 = call ptr @hew_vec_clone_owned(ptr %src_f0)
  %cloned_f0_int = ptrtoint ptr %clone_helper_f0 to i64
  %cloned_f0_null = icmp eq i64 %cloned_f0_int, 0
  br i1 %cloned_f0_null, label %rb_step_0, label %step_0_store

step_1_clone:                                     ; preds = %step_0_store
  %src_f1_ptr = getelementptr inbounds nuw %Bag, ptr %0, i32 0, i32 1
  %dst_f1_ptr = getelementptr inbounds nuw %Bag, ptr %1, i32 0, i32 1
  %enum_clone_inplace_f1 = call i32 @"__hew_enum_clone_inplace_Option$$string"(ptr %src_f1_ptr, ptr %dst_f1_ptr)
  %enum_clone_failed_f1 = icmp ne i32 %enum_clone_inplace_f1, 0
  br i1 %enum_clone_failed_f1, label %rb_step_1, label %step_1_store
}

declare ptr @hew_vec_clone_owned(ptr)

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

declare void @hew_vec_free_owned(ptr)

define internal void @__hew_record_drop_inplace_Bag(ptr %0) {
entry:
  %rec_int = ptrtoint ptr %0 to i64
  %rec_is_null = icmp eq i64 %rec_int, 0
  br i1 %rec_is_null, label %done, label %do_drop

do_drop:                                          ; preds = %entry
  %drop_f1_ptr = getelementptr inbounds nuw %Bag, ptr %0, i32 0, i32 1
  call void @"__hew_enum_drop_inplace_Option$$string"(ptr %drop_f1_ptr)
  %drop_f0_ptr = getelementptr inbounds nuw %Bag, ptr %0, i32 0, i32 0
  %drop_f0 = load ptr, ptr %drop_f0_ptr, align 8
  call void @hew_vec_free_owned(ptr %drop_f0)
  br label %done

done:                                             ; preds = %do_drop, %entry
  ret void
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

declare void @hew_trap_with_code(i32)

; Function Attrs: cold noreturn nounwind memory(inaccessiblemem: write)
declare void @llvm.trap() #0

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

define internal void @__hew_record_overwrite_release_Bag(ptr %0, ptr %1) {
entry:
  %ow_slot_0 = alloca ptr, align 8
  store ptr null, ptr %ow_slot_0, align 8
  %ow_slot_1 = alloca ptr, align 8
  store ptr null, ptr %ow_slot_1, align 8
  %ow_new_d0_f0_ptr = getelementptr inbounds nuw %Bag, ptr %1, i32 0, i32 0
  %ow_new_d0_f0_leaf = load ptr, ptr %ow_new_d0_f0_ptr, align 8
  store ptr %ow_new_d0_f0_leaf, ptr %ow_slot_0, align 8
  %ow_new_d0_f1_enum_ptr = getelementptr inbounds nuw %Bag, ptr %1, i32 0, i32 1
  %"ow_new_d1_Option$$string_tag_ptr" = getelementptr inbounds nuw %"Option$$string", ptr %ow_new_d0_f1_enum_ptr, i32 0, i32 0
  %"ow_new_d1_Option$$string_tag" = load i8, ptr %"ow_new_d1_Option$$string_tag_ptr", align 1
  switch i8 %"ow_new_d1_Option$$string_tag", label %"ow_new_d1_Option$$string_tag_oob" [
    i8 0, label %"ow_new_d1_Option$$string_v0"
    i8 1, label %"ow_new_d1_Option$$string_v1"
  ]

"ow_new_d1_Option$$string_merge":                 ; preds = %"ow_new_d1_Option$$string_v1", %"ow_new_d1_Option$$string_v0"
  %ow_old_d0_f0_ptr = getelementptr inbounds nuw %Bag, ptr %0, i32 0, i32 0
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
  %ow_old_d0_f1_enum_ptr = getelementptr inbounds nuw %Bag, ptr %0, i32 0, i32 1
  %"ow_old_d1_Option$$string_tag_ptr" = getelementptr inbounds nuw %"Option$$string", ptr %ow_old_d0_f1_enum_ptr, i32 0, i32 0
  %"ow_old_d1_Option$$string_tag" = load i8, ptr %"ow_old_d1_Option$$string_tag_ptr", align 1
  switch i8 %"ow_old_d1_Option$$string_tag", label %"ow_old_d1_Option$$string_tag_oob" [
    i8 0, label %"ow_old_d1_Option$$string_v0"
    i8 1, label %"ow_old_d1_Option$$string_v1"
  ]

"ow_new_d1_Option$$string_tag_oob":               ; preds = %entry
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

"ow_new_d1_Option$$string_v0":                    ; preds = %entry
  %"ow_new_d1_Option$$string_v0_payload" = getelementptr inbounds nuw %"Option$$string", ptr %ow_new_d0_f1_enum_ptr, i32 0, i32 1
  %ow_new_d1_f0_ptr = getelementptr inbounds nuw { ptr }, ptr %"ow_new_d1_Option$$string_v0_payload", i32 0, i32 0
  %ow_new_d1_f0_leaf = load ptr, ptr %ow_new_d1_f0_ptr, align 8
  store ptr %ow_new_d1_f0_leaf, ptr %ow_slot_1, align 8
  br label %"ow_new_d1_Option$$string_merge"

"ow_new_d1_Option$$string_v1":                    ; preds = %entry
  %"ow_new_d1_Option$$string_v1_payload" = getelementptr inbounds nuw %"Option$$string", ptr %ow_new_d0_f1_enum_ptr, i32 0, i32 1
  br label %"ow_new_d1_Option$$string_merge"

"ow_old_d1_Option$$string_merge":                 ; preds = %"ow_old_d1_Option$$string_v1", %"ow_old_d1_Option$$string_v0"
  call void @__hew_record_drop_inplace_Bag(ptr %0)
  ret void

"ow_old_d1_Option$$string_tag_oob":               ; preds = %"ow_new_d1_Option$$string_merge"
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

"ow_old_d1_Option$$string_v0":                    ; preds = %"ow_new_d1_Option$$string_merge"
  %"ow_old_d1_Option$$string_v0_payload" = getelementptr inbounds nuw %"Option$$string", ptr %ow_old_d0_f1_enum_ptr, i32 0, i32 1
  %ow_old_d1_f0_ptr = getelementptr inbounds nuw { ptr }, ptr %"ow_old_d1_Option$$string_v0_payload", i32 0, i32 0
  %ow_old_d1_f0_val = load ptr, ptr %ow_old_d1_f0_ptr, align 8
  %ow_old_d1_f0_int = ptrtoint ptr %ow_old_d1_f0_val to i64
  %ow_old_d1_f0_cmp0_leaf = load ptr, ptr %ow_slot_0, align 8
  %ow_old_d1_f0_cmp0_int = ptrtoint ptr %ow_old_d1_f0_cmp0_leaf to i64
  %ow_old_d1_f0_cmp0_eq = icmp eq i64 %ow_old_d1_f0_int, %ow_old_d1_f0_cmp0_int
  %ow_old_d1_f0_matched0 = or i1 false, %ow_old_d1_f0_cmp0_eq
  %ow_old_d1_f0_cmp1_leaf = load ptr, ptr %ow_slot_1, align 8
  %ow_old_d1_f0_cmp1_int = ptrtoint ptr %ow_old_d1_f0_cmp1_leaf to i64
  %ow_old_d1_f0_cmp1_eq = icmp eq i64 %ow_old_d1_f0_int, %ow_old_d1_f0_cmp1_int
  %ow_old_d1_f0_matched1 = or i1 %ow_old_d1_f0_matched0, %ow_old_d1_f0_cmp1_eq
  %ow_old_d1_f0_neutralized = select i1 %ow_old_d1_f0_matched1, ptr null, ptr %ow_old_d1_f0_val
  store ptr %ow_old_d1_f0_neutralized, ptr %ow_old_d1_f0_ptr, align 8
  br label %"ow_old_d1_Option$$string_merge"

"ow_old_d1_Option$$string_v1":                    ; preds = %"ow_new_d1_Option$$string_merge"
  %"ow_old_d1_Option$$string_v1_payload" = getelementptr inbounds nuw %"Option$$string", ptr %ow_old_d0_f1_enum_ptr, i32 0, i32 1
  br label %"ow_old_d1_Option$$string_merge"
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

declare i32 @hew_actor_cooperate()

declare ptr @hew_vec_new_i64()

define internal ptr @__hew_cbor_serialize_Bag(ptr %0, ptr %1) {
entry:
  %cbor_ser_buf = call ptr @hew_cbor_ser_new()
  call void @hew_cbor_ser_begin_map(ptr %cbor_ser_buf)
  call void @hew_cbor_ser_key_u64(ptr %cbor_ser_buf, i64 1)
  %cbor_ser_field_0 = getelementptr inbounds nuw %Bag, ptr %0, i32 0, i32 0
  %cbor_ser_vec_ptr = load ptr, ptr %cbor_ser_field_0, align 8
  %cbor_ser_vec_len = call i64 @hew_vec_len(ptr %cbor_ser_vec_ptr)
  call void @hew_cbor_ser_begin_array(ptr %cbor_ser_buf)
  %cbor_ser_vec_i = alloca i64, align 8
  store i64 0, ptr %cbor_ser_vec_i, align 8
  br label %cbor_ser_vec_head

cbor_ser_vec_head:                                ; preds = %cbor_ser_vec_body, %entry
  %cbor_ser_vec_i_cur = load i64, ptr %cbor_ser_vec_i, align 8
  %cbor_ser_vec_cmp = icmp slt i64 %cbor_ser_vec_i_cur, %cbor_ser_vec_len
  br i1 %cbor_ser_vec_cmp, label %cbor_ser_vec_body, label %cbor_ser_vec_done

cbor_ser_vec_body:                                ; preds = %cbor_ser_vec_head
  %cbor_ser_vec_get = call ptr @hew_vec_get_generic(ptr %cbor_ser_vec_ptr, i64 %cbor_ser_vec_i_cur)
  %cbor_ser_scalar = load i64, ptr %cbor_ser_vec_get, align 8
  call void @hew_cbor_ser_i64(ptr %cbor_ser_buf, i64 %cbor_ser_scalar)
  %cbor_ser_vec_inc = add i64 %cbor_ser_vec_i_cur, 1
  store i64 %cbor_ser_vec_inc, ptr %cbor_ser_vec_i, align 8
  br label %cbor_ser_vec_head

cbor_ser_vec_done:                                ; preds = %cbor_ser_vec_head
  call void @hew_cbor_ser_end_array(ptr %cbor_ser_buf)
  call void @hew_cbor_ser_key_u64(ptr %cbor_ser_buf, i64 2)
  %cbor_ser_field_1 = getelementptr inbounds nuw %Bag, ptr %0, i32 0, i32 1
  %cbor_ser_opt_tag_ptr = getelementptr inbounds nuw %"Option$$string", ptr %cbor_ser_field_1, i32 0, i32 0
  %cbor_ser_opt_tag = load i8, ptr %cbor_ser_opt_tag_ptr, align 1
  %cbor_ser_opt_is_some = icmp eq i8 %cbor_ser_opt_tag, 0
  br i1 %cbor_ser_opt_is_some, label %cbor_ser_opt_some, label %cbor_ser_opt_none

cbor_ser_opt_some:                                ; preds = %cbor_ser_vec_done
  %cbor_ser_opt_payload = getelementptr inbounds nuw %"Option$$string", ptr %cbor_ser_field_1, i32 0, i32 1
  %cbor_ser_opt_field0 = getelementptr inbounds nuw { ptr }, ptr %cbor_ser_opt_payload, i32 0, i32 0
  %cbor_ser_string_ptr = load ptr, ptr %cbor_ser_opt_field0, align 8
  call void @hew_cbor_ser_string(ptr %cbor_ser_buf, ptr %cbor_ser_string_ptr)
  br label %cbor_ser_opt_cont

cbor_ser_opt_none:                                ; preds = %cbor_ser_vec_done
  call void @hew_cbor_ser_null(ptr %cbor_ser_buf)
  br label %cbor_ser_opt_cont

cbor_ser_opt_cont:                                ; preds = %cbor_ser_opt_none, %cbor_ser_opt_some
  call void @hew_cbor_ser_end_map(ptr %cbor_ser_buf)
  %cbor_ser_finish = call ptr @hew_cbor_ser_finish(ptr %cbor_ser_buf, ptr %1)
  ret ptr %cbor_ser_finish
}

declare ptr @hew_wire_cbor_to_text(ptr, i64, ptr, i32)

declare void @hew_ser_free_bytes(ptr)

declare ptr @hew_wire_text_to_cbor(ptr, ptr, i32, ptr, ptr)

define internal ptr @__hew_cbor_deserialize_Bag(ptr %0, i64 %1, ptr %2) {
entry:
  %cbor_de_reader = call ptr @hew_cbor_de_new(ptr %0, i64 %1)
  store i64 ptrtoint (ptr getelementptr (%Bag, ptr null, i32 1) to i64), ptr %2, align 8
  %cbor_de_value = call ptr @malloc(i64 ptrtoint (ptr getelementptr (%Bag, ptr null, i32 1) to i64))
  %cbor_dst_as_int = ptrtoint ptr %cbor_de_value to i64
  %cbor_dst_is_null = icmp eq i64 %cbor_dst_as_int, 0
  br i1 %cbor_dst_is_null, label %cbor_de_oom, label %cbor_de_alloc_ok

cbor_de_oom:                                      ; preds = %entry
  call void @hew_cbor_de_free(ptr %cbor_de_reader)
  ret ptr null

cbor_de_alloc_ok:                                 ; preds = %entry
  %cbor_de_zero = call ptr @memset(ptr %cbor_de_value, i32 0, i64 ptrtoint (ptr getelementptr (%Bag, ptr null, i32 1) to i64))
  call void @hew_cbor_de_enter_map(ptr %cbor_de_reader)
  call void @hew_cbor_de_select_key(ptr %cbor_de_reader, i64 1)
  %cbor_de_field_0 = getelementptr inbounds nuw %Bag, ptr %cbor_de_value, i32 0, i32 0
  %cbor_de_vec_new = call ptr @hew_vec_new_with_elem_size(i64 ptrtoint (ptr getelementptr (i64, ptr null, i32 1) to i64))
  store ptr %cbor_de_vec_new, ptr %cbor_de_field_0, align 8
  call void @hew_cbor_de_enter_array(ptr %cbor_de_reader)
  br label %cbor_de_vec_head

cbor_de_vec_head:                                 ; preds = %cbor_de_vec_body, %cbor_de_alloc_ok
  %cbor_de_vec_next = call i32 @hew_cbor_de_array_next(ptr %cbor_de_reader)
  %cbor_de_vec_has = icmp ne i32 %cbor_de_vec_next, 0
  br i1 %cbor_de_vec_has, label %cbor_de_vec_body, label %cbor_de_vec_done

cbor_de_vec_body:                                 ; preds = %cbor_de_vec_head
  %cbor_de_vec_elem = alloca i64, align 8
  store i64 0, ptr %cbor_de_vec_elem, align 8
  %cbor_de_int = call i64 @hew_cbor_de_int_checked(ptr %cbor_de_reader, i32 64, i32 1)
  store i64 %cbor_de_int, ptr %cbor_de_vec_elem, align 8
  call void @hew_vec_push_generic(ptr %cbor_de_vec_new, ptr %cbor_de_vec_elem)
  br label %cbor_de_vec_head

cbor_de_vec_done:                                 ; preds = %cbor_de_vec_head
  call void @hew_cbor_de_exit_array(ptr %cbor_de_reader)
  call void @hew_cbor_de_select_key(ptr %cbor_de_reader, i64 2)
  %cbor_de_field_1 = getelementptr inbounds nuw %Bag, ptr %cbor_de_value, i32 0, i32 1
  %cbor_de_opt_is_null = call i32 @hew_cbor_de_is_null(ptr %cbor_de_reader)
  %cbor_de_opt_nullc = icmp ne i32 %cbor_de_opt_is_null, 0
  br i1 %cbor_de_opt_nullc, label %cbor_de_opt_none, label %cbor_de_opt_some

cbor_de_opt_none:                                 ; preds = %cbor_de_vec_done
  call void @hew_cbor_de_skip(ptr %cbor_de_reader)
  %cbor_de_opt_none_tag = getelementptr inbounds nuw %"Option$$string", ptr %cbor_de_field_1, i32 0, i32 0
  store i8 1, ptr %cbor_de_opt_none_tag, align 1
  br label %cbor_de_opt_cont

cbor_de_opt_some:                                 ; preds = %cbor_de_vec_done
  %cbor_de_opt_some_tag = getelementptr inbounds nuw %"Option$$string", ptr %cbor_de_field_1, i32 0, i32 0
  store i8 0, ptr %cbor_de_opt_some_tag, align 1
  %cbor_de_opt_payload = getelementptr inbounds nuw %"Option$$string", ptr %cbor_de_field_1, i32 0, i32 1
  %cbor_de_opt_field0 = getelementptr inbounds nuw { ptr }, ptr %cbor_de_opt_payload, i32 0, i32 0
  %cbor_de_string = call ptr @hew_cbor_de_string(ptr %cbor_de_reader)
  store ptr %cbor_de_string, ptr %cbor_de_opt_field0, align 8
  br label %cbor_de_opt_cont

cbor_de_opt_cont:                                 ; preds = %cbor_de_opt_some, %cbor_de_opt_none
  call void @hew_cbor_de_exit_map(ptr %cbor_de_reader)
  %cbor_de_failed = call i32 @hew_cbor_de_failed(ptr %cbor_de_reader)
  call void @hew_cbor_de_free(ptr %cbor_de_reader)
  %cbor_de_is_failed = icmp ne i32 %cbor_de_failed, 0
  br i1 %cbor_de_is_failed, label %cbor_de_fail, label %cbor_de_ok

cbor_de_ok:                                       ; preds = %cbor_de_opt_cont
  ret ptr %cbor_de_value

cbor_de_fail:                                     ; preds = %cbor_de_opt_cont
  call void @__hew_record_drop_inplace_Bag(ptr %cbor_de_value)
  call void @free(ptr %cbor_de_value)
  ret ptr null
}

declare void @free(ptr)

declare i32 @hew_lambda_drain_all(i64)

declare i32 @hew_string_equals(ptr, ptr)

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64) #1

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.smul.with.overflow.i64(i64, i64) #1

declare ptr @hew_cbor_ser_new()

declare void @hew_cbor_ser_begin_map(ptr)

declare void @hew_cbor_ser_key_u64(ptr, i64)

declare void @hew_cbor_ser_begin_array(ptr)

declare ptr @hew_vec_get_generic(ptr, i64)

declare void @hew_cbor_ser_i64(ptr, i64)

declare void @hew_cbor_ser_end_array(ptr)

declare void @hew_cbor_ser_string(ptr, ptr)

declare void @hew_cbor_ser_null(ptr)

declare void @hew_cbor_ser_end_map(ptr)

declare ptr @hew_cbor_ser_finish(ptr, ptr)

declare ptr @hew_cbor_de_new(ptr, i64)

declare ptr @malloc(i64)

declare void @hew_cbor_de_free(ptr)

declare ptr @memset(ptr, i32, i64)

declare void @hew_cbor_de_enter_map(ptr)

declare void @hew_cbor_de_select_key(ptr, i64)

declare ptr @hew_vec_new_with_elem_size(i64)

declare void @hew_cbor_de_enter_array(ptr)

declare i32 @hew_cbor_de_array_next(ptr)

declare i64 @hew_cbor_de_int_checked(ptr, i32, i32)

declare void @hew_vec_push_generic(ptr, ptr)

declare void @hew_cbor_de_exit_array(ptr)

declare i32 @hew_cbor_de_is_null(ptr)

declare void @hew_cbor_de_skip(ptr)

declare ptr @hew_cbor_de_string(ptr)

declare void @hew_cbor_de_exit_map(ptr)

declare i32 @hew_cbor_de_failed(ptr)

attributes #0 = { cold noreturn nounwind memory(inaccessiblemem: write) }
attributes #1 = { nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none) }
