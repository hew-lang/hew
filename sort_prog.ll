; ModuleID = 'sort_prog'
source_filename = "sort_prog"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "aarch64-apple-macosx13.0"

%CrashInfo = type { i64, ptr }

@str_lit = private unnamed_addr constant [6 x i8] c"alpha\00", align 1
@str_lit.1 = private unnamed_addr constant [6 x i8] c"hotel\00", align 1
@str_lit.2 = private unnamed_addr constant [5 x i8] c"golf\00", align 1
@str_lit.3 = private unnamed_addr constant [8 x i8] c"foxtrot\00", align 1
@str_lit.4 = private unnamed_addr constant [5 x i8] c"echo\00", align 1
@str_lit.5 = private unnamed_addr constant [6 x i8] c"delta\00", align 1
@str_lit.6 = private unnamed_addr constant [8 x i8] c"charlie\00", align 1
@str_lit.7 = private unnamed_addr constant [6 x i8] c"bravo\00", align 1
@str_lit.8 = private unnamed_addr constant [6 x i8] c"alpha\00", align 1
@str_lit.9 = private unnamed_addr constant [6 x i8] c"bravo\00", align 1
@str_lit.10 = private unnamed_addr constant [8 x i8] c"charlie\00", align 1
@str_lit.11 = private unnamed_addr constant [6 x i8] c"delta\00", align 1
@str_lit.12 = private unnamed_addr constant [5 x i8] c"echo\00", align 1
@str_lit.13 = private unnamed_addr constant [8 x i8] c"foxtrot\00", align 1
@str_lit.14 = private unnamed_addr constant [5 x i8] c"golf\00", align 1
@str_lit.15 = private unnamed_addr constant [6 x i8] c"hotel\00", align 1
@str_lit.16 = private unnamed_addr constant [6 x i8] c"hotel\00", align 1
@str_lit.17 = private unnamed_addr constant [6 x i8] c"alpha\00", align 1
@str_lit.18 = private unnamed_addr constant [6 x i8] c"hotel\00", align 1
@str_lit.19 = private unnamed_addr constant [6 x i8] c"alpha\00", align 1
@str_lit.20 = private unnamed_addr constant [6 x i8] c"bravo\00", align 1
@str_lit.21 = private unnamed_addr constant [8 x i8] c"charlie\00", align 1
@str_lit.22 = private unnamed_addr constant [6 x i8] c"alpha\00", align 1
@str_lit.23 = private unnamed_addr constant [6 x i8] c"bravo\00", align 1
@str_lit.24 = private unnamed_addr constant [8 x i8] c"charlie\00", align 1
@str_lit.25 = private unnamed_addr constant [6 x i8] c"alpha\00", align 1
@str_lit.26 = private unnamed_addr constant [16 x i8] c"sort-authority=\00", align 1
@str_lit.27 = private unnamed_addr constant [16 x i8] c"sort-authority=\00", align 1
@str_lit.28 = private unnamed_addr constant [3 x i8] c"ns\00", align 1

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

declare ptr @hew_sort_floats(ptr)

define internal i64 @check_ints() {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i64, align 8
  %local_5 = alloca i64, align 8
  %local_6 = alloca i64, align 8
  %local_7 = alloca i8, align 1
  %local_8 = alloca i64, align 8
  %local_9 = alloca i64, align 8
  %local_10 = alloca i8, align 1
  %local_11 = alloca i64, align 8
  %local_12 = alloca i64, align 8
  %local_13 = alloca i8, align 1
  %local_14 = alloca i8, align 1
  %local_15 = alloca { ptr, i64 }, align 8
  %local_16 = alloca { ptr, i64 }, align 8
  %local_17 = alloca ptr, align 8
  %local_18 = alloca ptr, align 8
  %local_19 = alloca i64, align 8
  %local_20 = alloca i64, align 8
  %local_21 = alloca i8, align 1
  %local_22 = alloca i64, align 8
  %local_23 = alloca i64, align 8
  %local_24 = alloca i64, align 8
  %local_25 = alloca i8, align 1
  %local_26 = alloca i8, align 1
  %local_27 = alloca i64, align 8
  %local_28 = alloca i64, align 8
  %local_29 = alloca i64, align 8
  %local_30 = alloca i64, align 8
  %local_31 = alloca i64, align 8
  %local_32 = alloca i64, align 8
  %local_33 = alloca i8, align 1
  %local_34 = alloca i64, align 8
  %local_35 = alloca i64, align 8
  %local_36 = alloca i8, align 1
  %local_37 = alloca i8, align 1
  %local_38 = alloca i64, align 8
  %local_39 = alloca i8, align 1
  %local_40 = alloca i64, align 8
  %local_41 = alloca i8, align 1
  %local_42 = alloca i64, align 8
  %local_43 = alloca i8, align 1
  %local_44 = alloca i64, align 8
  %local_45 = alloca i8, align 1
  %local_46 = alloca i64, align 8
  %local_47 = alloca i64, align 8
  %local_48 = alloca i64, align 8
  %local_49 = alloca i8, align 1
  %local_50 = alloca i8, align 1
  %local_51 = alloca i64, align 8
  %local_52 = alloca i8, align 1
  %local_53 = alloca ptr, align 8
  %local_54 = alloca ptr, align 8
  %local_55 = alloca i64, align 8
  %local_56 = alloca i64, align 8
  %local_57 = alloca i64, align 8
  %local_58 = alloca i64, align 8
  %local_59 = alloca i64, align 8
  %local_60 = alloca i8, align 1
  %local_61 = alloca i64, align 8
  %local_62 = alloca i64, align 8
  %local_63 = alloca i8, align 1
  %local_64 = alloca i64, align 8
  %local_65 = alloca i64, align 8
  %local_66 = alloca i8, align 1
  %local_67 = alloca i64, align 8
  %local_68 = alloca i64, align 8
  %local_69 = alloca i64, align 8
  %local_70 = alloca i8, align 1
  %local_71 = alloca i64, align 8
  %local_72 = alloca i8, align 1
  %local_73 = alloca i64, align 8
  %local_74 = alloca i8, align 1
  %local_75 = alloca i8, align 1
  %local_76 = alloca ptr, align 8
  %local_77 = alloca ptr, align 8
  %local_78 = alloca i64, align 8
  %local_79 = alloca i64, align 8
  %local_80 = alloca i64, align 8
  %local_81 = alloca i64, align 8
  %local_82 = alloca i64, align 8
  %local_83 = alloca i64, align 8
  %local_84 = alloca i64, align 8
  %local_85 = alloca i64, align 8
  %local_86 = alloca i64, align 8
  %local_87 = alloca i64, align 8
  %local_88 = alloca i64, align 8
  %local_89 = alloca i64, align 8
  %local_90 = alloca i64, align 8
  %local_91 = alloca i8, align 1
  %local_92 = alloca i64, align 8
  %local_93 = alloca i64, align 8
  %local_94 = alloca i8, align 1
  %local_95 = alloca i8, align 1
  %local_96 = alloca i8, align 1
  %local_97 = alloca i64, align 8
  %local_98 = alloca i8, align 1
  %local_99 = alloca i64, align 8
  %local_100 = alloca i64, align 8
  %local_101 = alloca i8, align 1
  %local_102 = alloca i64, align 8
  %local_103 = alloca i8, align 1
  %local_104 = alloca i64, align 8
  %local_105 = alloca i64, align 8
  %local_106 = alloca i8, align 1
  %local_107 = alloca i64, align 8
  %local_108 = alloca i8, align 1
  %local_109 = alloca i64, align 8
  %local_110 = alloca i64, align 8
  %local_111 = alloca i8, align 1
  %local_112 = alloca i64, align 8
  %local_113 = alloca i64, align 8
  %local_114 = alloca i8, align 1
  %local_115 = alloca i64, align 8
  %local_116 = alloca i8, align 1
  %local_117 = alloca i64, align 8
  %local_118 = alloca i64, align 8
  %local_119 = alloca i8, align 1
  %local_120 = alloca i64, align 8
  %local_121 = alloca i8, align 1
  %local_122 = alloca i64, align 8
  %local_123 = alloca i64, align 8
  %local_124 = alloca i8, align 1
  %local_125 = alloca i64, align 8
  %local_126 = alloca i64, align 8
  %local_127 = alloca i8, align 1
  %local_128 = alloca i64, align 8
  %local_129 = alloca i8, align 1
  %local_130 = alloca i64, align 8
  %local_131 = alloca i8, align 1
  %local_132 = alloca i64, align 8
  %local_133 = alloca i64, align 8
  %local_134 = alloca i8, align 1
  %local_135 = alloca i64, align 8
  %local_136 = alloca i64, align 8
  %local_137 = alloca i8, align 1
  %local_138 = alloca i64, align 8
  %local_139 = alloca i8, align 1
  %local_140 = alloca i8, align 1
  %local_141 = alloca i64, align 8
  %local_142 = alloca i8, align 1
  %local_143 = alloca i64, align 8
  %local_144 = alloca i64, align 8
  %local_145 = alloca i64, align 8
  %local_146 = alloca i8, align 1
  %local_147 = alloca i64, align 8
  %local_148 = alloca i64, align 8
  %local_149 = alloca i64, align 8
  %local_150 = alloca i8, align 1
  %local_151 = alloca i64, align 8
  %local_152 = alloca i8, align 1
  %local_153 = alloca i64, align 8
  %local_154 = alloca i8, align 1
  %local_155 = alloca i8, align 1
  %local_156 = alloca i64, align 8
  %local_157 = alloca i8, align 1
  %local_158 = alloca i8, align 1
  %local_159 = alloca i8, align 1
  %local_160 = alloca i8, align 1
  %local_161 = alloca i8, align 1
  %local_162 = alloca i64, align 8
  %local_163 = alloca ptr, align 8
  %local_164 = alloca ptr, align 8
  %local_165 = alloca i64, align 8
  %local_166 = alloca i64, align 8
  %local_167 = alloca i64, align 8
  %local_168 = alloca i64, align 8
  %local_169 = alloca i64, align 8
  %local_170 = alloca i8, align 1
  %local_171 = alloca i64, align 8
  %local_172 = alloca i64, align 8
  %local_173 = alloca i8, align 1
  %local_174 = alloca i64, align 8
  %local_175 = alloca i8, align 1
  %local_176 = alloca ptr, align 8
  %local_177 = alloca ptr, align 8
  %local_178 = alloca i64, align 8
  %local_179 = alloca i64, align 8
  %local_180 = alloca i64, align 8
  %local_181 = alloca i64, align 8
  %local_182 = alloca i64, align 8
  %local_183 = alloca i8, align 1
  %local_184 = alloca i64, align 8
  %local_185 = alloca i64, align 8
  %local_186 = alloca i8, align 1
  %local_187 = alloca i8, align 1
  %local_188 = alloca i8, align 1
  %local_189 = alloca i64, align 8
  %local_190 = alloca i8, align 1
  %local_191 = alloca i64, align 8
  %local_192 = alloca i64, align 8
  %local_193 = alloca i8, align 1
  %local_194 = alloca i64, align 8
  %local_195 = alloca i8, align 1
  %local_196 = alloca i64, align 8
  %local_197 = alloca i64, align 8
  %local_198 = alloca i8, align 1
  %local_199 = alloca i64, align 8
  %local_200 = alloca i8, align 1
  %local_201 = alloca i64, align 8
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
  store i64 1, ptr %local_4, align 8
  %move_load1 = load i64, ptr %local_4, align 8
  store i64 %move_load1, ptr %local_5, align 8
  store i64 0, ptr %local_6, align 8
  %cmp_lhs = load i64, ptr %local_5, align 8
  %cmp_rhs = load i64, ptr %local_6, align 8
  %cmp_bit = icmp sle i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_7, align 1
  %cond_load = load i8, ptr %local_7, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb6, label %bb7

bb2:                                              ; preds = %after_cooperate23, %bb4
  %cmp_lhs2 = load i64, ptr %local_2, align 8
  %cmp_rhs3 = load i64, ptr %local_3, align 8
  %cmp_bit4 = icmp slt i64 %cmp_lhs2, %cmp_rhs3
  %cmp_zext5 = zext i1 %cmp_bit4 to i8
  store i8 %cmp_zext5, ptr %local_10, align 1
  %cond_load6 = load i8, ptr %local_10, align 1
  %cond_nz7 = icmp ne i8 %cond_load6, 0
  br i1 %cond_nz7, label %bb3, label %bb5

bb3:                                              ; preds = %bb2
  store i64 1023, ptr %local_11, align 8
  %checked_lhs = load i64, ptr %local_11, align 8
  %checked_rhs = load i64, ptr %local_2, align 8
  %with_overflow = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_12, align 8
  store i8 %checked_overflow_widen, ptr %local_13, align 1
  %cond_load8 = load i8, ptr %local_13, align 1
  %cond_nz9 = icmp ne i8 %cond_load8, 0
  br i1 %cond_nz9, label %bb8, label %bb9

bb4:                                              ; preds = %after_cooperate31
  %checked_lhs10 = load i64, ptr %local_2, align 8
  %checked_rhs11 = load i64, ptr %local_5, align 8
  %with_overflow12 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs10, i64 %checked_rhs11)
  %checked_result13 = extractvalue { i64, i1 } %with_overflow12, 0
  %checked_overflow14 = extractvalue { i64, i1 } %with_overflow12, 1
  %checked_overflow_widen15 = zext i1 %checked_overflow14 to i8
  store i64 %checked_result13, ptr %local_2, align 8
  store i8 %checked_overflow_widen15, ptr %local_14, align 1
  %cond_load16 = load i8, ptr %local_14, align 1
  %cond_nz17 = icmp ne i8 %cond_load16, 0
  br i1 %cond_nz17, label %bb11, label %bb2

bb5:                                              ; preds = %bb2
  %call_arg = load ptr, ptr %local_1, align 8
  %call_result = call { ptr, i64 } @"sort$sort_ints_counted"(ptr %call_arg)
  store { ptr, i64 } %call_result, ptr %local_15, align 8
  br label %bb12

bb6:                                              ; preds = %bb1
  %"hew_vec_free drop" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 202)
  call void @llvm.trap()
  unreachable

bb7:                                              ; preds = %bb1
  store i64 0, ptr %local_8, align 8
  store i64 1024, ptr %local_9, align 8
  %move_load18 = load i64, ptr %local_8, align 8
  store i64 %move_load18, ptr %local_2, align 8
  %move_load19 = load i64, ptr %local_9, align 8
  store i64 %move_load19, ptr %local_3, align 8
  %hew_actor_cooperate20 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel21 = icmp eq i32 %hew_actor_cooperate20, 2
  br i1 %hew_cooperate_is_cancel21, label %cancel_exit22, label %after_cooperate23

bb8:                                              ; preds = %bb3
  %"hew_vec_free drop25" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop25")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb9:                                              ; preds = %bb3
  %call_arg26 = load ptr, ptr %local_1, align 8
  %call_arg27 = load i64, ptr %local_12, align 8
  call void @hew_vec_push_i64(ptr %call_arg26, i64 %call_arg27)
  br label %bb10

bb10:                                             ; preds = %bb9
  %hew_actor_cooperate28 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel29 = icmp eq i32 %hew_actor_cooperate28, 2
  br i1 %hew_cooperate_is_cancel29, label %cancel_exit30, label %after_cooperate31

bb11:                                             ; preds = %bb4
  %"hew_vec_free drop33" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop33")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb12:                                             ; preds = %bb5
  %move_load34 = load { ptr, i64 }, ptr %local_15, align 8
  store { ptr, i64 } %move_load34, ptr %local_16, align 8
  %tuple_0_load_ptr = getelementptr inbounds nuw { ptr, i64 }, ptr %local_16, i32 0, i32 0
  %tuple_0_load = load ptr, ptr %tuple_0_load_ptr, align 8
  store ptr %tuple_0_load, ptr %local_17, align 8
  %move_load35 = load ptr, ptr %local_17, align 8
  store ptr %move_load35, ptr %local_18, align 8
  %tuple_1_load_ptr = getelementptr inbounds nuw { ptr, i64 }, ptr %local_16, i32 0, i32 1
  %tuple_1_load = load i64, ptr %tuple_1_load_ptr, align 8
  store i64 %tuple_1_load, ptr %local_19, align 8
  %move_load36 = load i64, ptr %local_19, align 8
  store i64 %move_load36, ptr %local_20, align 8
  store i64 1024, ptr %local_22, align 8
  store i64 10, ptr %local_23, align 8
  %checked_lhs37 = load i64, ptr %local_22, align 8
  %checked_rhs38 = load i64, ptr %local_23, align 8
  %with_overflow39 = call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %checked_lhs37, i64 %checked_rhs38)
  %checked_result40 = extractvalue { i64, i1 } %with_overflow39, 0
  %checked_overflow41 = extractvalue { i64, i1 } %with_overflow39, 1
  %checked_overflow_widen42 = zext i1 %checked_overflow41 to i8
  store i64 %checked_result40, ptr %local_24, align 8
  store i8 %checked_overflow_widen42, ptr %local_25, align 1
  %cond_load43 = load i8, ptr %local_25, align 1
  %cond_nz44 = icmp ne i8 %cond_load43, 0
  br i1 %cond_nz44, label %bb13, label %bb14

bb13:                                             ; preds = %bb12
  %"hew_vec_free drop45" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop45")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb14:                                             ; preds = %bb12
  %cmp_lhs46 = load i64, ptr %local_20, align 8
  %cmp_rhs47 = load i64, ptr %local_24, align 8
  %cmp_bit48 = icmp sgt i64 %cmp_lhs46, %cmp_rhs47
  %cmp_zext49 = zext i1 %cmp_bit48 to i8
  store i8 %cmp_zext49, ptr %local_26, align 1
  %cond_load50 = load i8, ptr %local_26, align 1
  %cond_nz51 = icmp ne i8 %cond_load50, 0
  br i1 %cond_nz51, label %bb15, label %bb16

bb15:                                             ; preds = %bb14
  store i64 10, ptr %local_27, align 8
  %move_load52 = load i64, ptr %local_27, align 8
  store i64 %move_load52, ptr %return_slot, align 8
  %"hew_vec_free drop53" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop53")
  store ptr null, ptr %local_1, align 8
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val

bb16:                                             ; preds = %bb14
  br label %bb17

bb17:                                             ; preds = %after_cooperate64, %bb16
  store i64 1, ptr %local_30, align 8
  %move_load54 = load i64, ptr %local_30, align 8
  store i64 %move_load54, ptr %local_31, align 8
  store i64 0, ptr %local_32, align 8
  %cmp_lhs55 = load i64, ptr %local_31, align 8
  %cmp_rhs56 = load i64, ptr %local_32, align 8
  %cmp_bit57 = icmp sle i64 %cmp_lhs55, %cmp_rhs56
  %cmp_zext58 = zext i1 %cmp_bit57 to i8
  store i8 %cmp_zext58, ptr %local_33, align 1
  %cond_load59 = load i8, ptr %local_33, align 1
  %cond_nz60 = icmp ne i8 %cond_load59, 0
  br i1 %cond_nz60, label %bb23, label %bb24

bb18:                                             ; No predecessors!
  %hew_actor_cooperate61 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel62 = icmp eq i32 %hew_actor_cooperate61, 2
  br i1 %hew_cooperate_is_cancel62, label %cancel_exit63, label %after_cooperate64

bb19:                                             ; preds = %after_cooperate92, %bb21
  %cmp_lhs65 = load i64, ptr %local_28, align 8
  %cmp_rhs66 = load i64, ptr %local_29, align 8
  %cmp_bit67 = icmp slt i64 %cmp_lhs65, %cmp_rhs66
  %cmp_zext68 = zext i1 %cmp_bit67 to i8
  store i8 %cmp_zext68, ptr %local_36, align 1
  %cond_load69 = load i8, ptr %local_36, align 1
  %cond_nz70 = icmp ne i8 %cond_load69, 0
  br i1 %cond_nz70, label %bb20, label %bb22

bb20:                                             ; preds = %bb19
  %"hew_vec_len arg0" = load ptr, ptr %local_18, align 8
  %hew_vec_len_call = call i64 @hew_vec_len(ptr %"hew_vec_len arg0")
  store i64 %hew_vec_len_call, ptr %local_38, align 8
  %cmp_lhs71 = load i64, ptr %local_28, align 8
  %cmp_rhs72 = load i64, ptr %local_38, align 8
  %cmp_bit73 = icmp uge i64 %cmp_lhs71, %cmp_rhs72
  %cmp_zext74 = zext i1 %cmp_bit73 to i8
  store i8 %cmp_zext74, ptr %local_39, align 1
  %cond_load75 = load i8, ptr %local_39, align 1
  %cond_nz76 = icmp ne i8 %cond_load75, 0
  br i1 %cond_nz76, label %bb25, label %bb26

bb21:                                             ; preds = %after_cooperate141
  %checked_lhs77 = load i64, ptr %local_28, align 8
  %checked_rhs78 = load i64, ptr %local_31, align 8
  %with_overflow79 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs77, i64 %checked_rhs78)
  %checked_result80 = extractvalue { i64, i1 } %with_overflow79, 0
  %checked_overflow81 = extractvalue { i64, i1 } %with_overflow79, 1
  %checked_overflow_widen82 = zext i1 %checked_overflow81 to i8
  store i64 %checked_result80, ptr %local_28, align 8
  store i8 %checked_overflow_widen82, ptr %local_52, align 1
  %cond_load83 = load i8, ptr %local_52, align 1
  %cond_nz84 = icmp ne i8 %cond_load83, 0
  br i1 %cond_nz84, label %bb39, label %bb19

bb22:                                             ; preds = %bb19
  %hew_vec_new_i64_call85 = call ptr @hew_vec_new_i64()
  store ptr %hew_vec_new_i64_call85, ptr %local_53, align 8
  br label %bb40

bb23:                                             ; preds = %bb17
  %"hew_vec_free drop86" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop86")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 202)
  call void @llvm.trap()
  unreachable

bb24:                                             ; preds = %bb17
  store i64 0, ptr %local_34, align 8
  store i64 1024, ptr %local_35, align 8
  %move_load87 = load i64, ptr %local_34, align 8
  store i64 %move_load87, ptr %local_28, align 8
  %move_load88 = load i64, ptr %local_35, align 8
  store i64 %move_load88, ptr %local_29, align 8
  %hew_actor_cooperate89 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel90 = icmp eq i32 %hew_actor_cooperate89, 2
  br i1 %hew_cooperate_is_cancel90, label %cancel_exit91, label %after_cooperate92

bb25:                                             ; preds = %bb20
  %"hew_vec_free drop94" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop94")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb26:                                             ; preds = %bb20
  %"hew_vec_get_i64 arg0" = load ptr, ptr %local_18, align 8
  %"hew_vec_get_i64 arg1" = load i64, ptr %local_28, align 8
  %hew_vec_get_i64_call = call i64 @hew_vec_get_i64(ptr %"hew_vec_get_i64 arg0", i64 %"hew_vec_get_i64 arg1")
  store i64 %hew_vec_get_i64_call, ptr %local_40, align 8
  %cmp_lhs95 = load i64, ptr %local_40, align 8
  %cmp_rhs96 = load i64, ptr %local_28, align 8
  %cmp_bit97 = icmp ne i64 %cmp_lhs95, %cmp_rhs96
  %cmp_zext98 = zext i1 %cmp_bit97 to i8
  store i8 %cmp_zext98, ptr %local_41, align 1
  %cond_load99 = load i8, ptr %local_41, align 1
  %cond_nz100 = icmp ne i8 %cond_load99, 0
  br i1 %cond_nz100, label %bb27, label %bb28

bb27:                                             ; preds = %bb26
  store i64 11, ptr %local_42, align 8
  %move_load101 = load i64, ptr %local_42, align 8
  store i64 %move_load101, ptr %return_slot, align 8
  %"hew_vec_free drop102" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop102")
  store ptr null, ptr %local_1, align 8
  %ret_val103 = load i64, ptr %return_slot, align 8
  ret i64 %ret_val103

bb28:                                             ; preds = %bb26
  br label %bb29

bb29:                                             ; preds = %after_cooperate115, %bb28
  %"hew_vec_len arg0104" = load ptr, ptr %local_1, align 8
  %hew_vec_len_call105 = call i64 @hew_vec_len(ptr %"hew_vec_len arg0104")
  store i64 %hew_vec_len_call105, ptr %local_44, align 8
  %cmp_lhs106 = load i64, ptr %local_28, align 8
  %cmp_rhs107 = load i64, ptr %local_44, align 8
  %cmp_bit108 = icmp uge i64 %cmp_lhs106, %cmp_rhs107
  %cmp_zext109 = zext i1 %cmp_bit108 to i8
  store i8 %cmp_zext109, ptr %local_45, align 1
  %cond_load110 = load i8, ptr %local_45, align 1
  %cond_nz111 = icmp ne i8 %cond_load110, 0
  br i1 %cond_nz111, label %bb31, label %bb32

bb30:                                             ; No predecessors!
  %hew_actor_cooperate112 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel113 = icmp eq i32 %hew_actor_cooperate112, 2
  br i1 %hew_cooperate_is_cancel113, label %cancel_exit114, label %after_cooperate115

bb31:                                             ; preds = %bb29
  %"hew_vec_free drop116" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop116")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb32:                                             ; preds = %bb29
  %"hew_vec_get_i64 arg0117" = load ptr, ptr %local_1, align 8
  %"hew_vec_get_i64 arg1118" = load i64, ptr %local_28, align 8
  %hew_vec_get_i64_call119 = call i64 @hew_vec_get_i64(ptr %"hew_vec_get_i64 arg0117", i64 %"hew_vec_get_i64 arg1118")
  store i64 %hew_vec_get_i64_call119, ptr %local_46, align 8
  store i64 1023, ptr %local_47, align 8
  %checked_lhs120 = load i64, ptr %local_47, align 8
  %checked_rhs121 = load i64, ptr %local_28, align 8
  %with_overflow122 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %checked_lhs120, i64 %checked_rhs121)
  %checked_result123 = extractvalue { i64, i1 } %with_overflow122, 0
  %checked_overflow124 = extractvalue { i64, i1 } %with_overflow122, 1
  %checked_overflow_widen125 = zext i1 %checked_overflow124 to i8
  store i64 %checked_result123, ptr %local_48, align 8
  store i8 %checked_overflow_widen125, ptr %local_49, align 1
  %cond_load126 = load i8, ptr %local_49, align 1
  %cond_nz127 = icmp ne i8 %cond_load126, 0
  br i1 %cond_nz127, label %bb33, label %bb34

bb33:                                             ; preds = %bb32
  %"hew_vec_free drop128" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop128")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb34:                                             ; preds = %bb32
  %cmp_lhs129 = load i64, ptr %local_46, align 8
  %cmp_rhs130 = load i64, ptr %local_48, align 8
  %cmp_bit131 = icmp ne i64 %cmp_lhs129, %cmp_rhs130
  %cmp_zext132 = zext i1 %cmp_bit131 to i8
  store i8 %cmp_zext132, ptr %local_50, align 1
  %cond_load133 = load i8, ptr %local_50, align 1
  %cond_nz134 = icmp ne i8 %cond_load133, 0
  br i1 %cond_nz134, label %bb35, label %bb36

bb35:                                             ; preds = %bb34
  store i64 12, ptr %local_51, align 8
  %move_load135 = load i64, ptr %local_51, align 8
  store i64 %move_load135, ptr %return_slot, align 8
  %"hew_vec_free drop136" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop136")
  store ptr null, ptr %local_1, align 8
  %ret_val137 = load i64, ptr %return_slot, align 8
  ret i64 %ret_val137

bb36:                                             ; preds = %bb34
  br label %bb37

bb37:                                             ; preds = %after_cooperate146, %bb36
  %hew_actor_cooperate138 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel139 = icmp eq i32 %hew_actor_cooperate138, 2
  br i1 %hew_cooperate_is_cancel139, label %cancel_exit140, label %after_cooperate141

bb38:                                             ; No predecessors!
  %hew_actor_cooperate143 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel144 = icmp eq i32 %hew_actor_cooperate143, 2
  br i1 %hew_cooperate_is_cancel144, label %cancel_exit145, label %after_cooperate146

bb39:                                             ; preds = %bb21
  %"hew_vec_free drop147" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop147")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb40:                                             ; preds = %bb22
  %move_load148 = load ptr, ptr %local_53, align 8
  store ptr %move_load148, ptr %local_54, align 8
  store i64 1, ptr %local_57, align 8
  %move_load149 = load i64, ptr %local_57, align 8
  store i64 %move_load149, ptr %local_58, align 8
  store i64 0, ptr %local_59, align 8
  %cmp_lhs150 = load i64, ptr %local_58, align 8
  %cmp_rhs151 = load i64, ptr %local_59, align 8
  %cmp_bit152 = icmp sle i64 %cmp_lhs150, %cmp_rhs151
  %cmp_zext153 = zext i1 %cmp_bit152 to i8
  store i8 %cmp_zext153, ptr %local_60, align 1
  %cond_load154 = load i8, ptr %local_60, align 1
  %cond_nz155 = icmp ne i8 %cond_load154, 0
  br i1 %cond_nz155, label %bb45, label %bb46

bb41:                                             ; preds = %after_cooperate187, %bb43
  %cmp_lhs156 = load i64, ptr %local_55, align 8
  %cmp_rhs157 = load i64, ptr %local_56, align 8
  %cmp_bit158 = icmp slt i64 %cmp_lhs156, %cmp_rhs157
  %cmp_zext159 = zext i1 %cmp_bit158 to i8
  store i8 %cmp_zext159, ptr %local_63, align 1
  %cond_load160 = load i8, ptr %local_63, align 1
  %cond_nz161 = icmp ne i8 %cond_load160, 0
  br i1 %cond_nz161, label %bb42, label %bb44

bb42:                                             ; preds = %bb41
  store i64 7, ptr %local_64, align 8
  %checked_lhs162 = load i64, ptr %local_55, align 8
  %checked_rhs163 = load i64, ptr %local_64, align 8
  %with_overflow164 = call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %checked_lhs162, i64 %checked_rhs163)
  %checked_result165 = extractvalue { i64, i1 } %with_overflow164, 0
  %checked_overflow166 = extractvalue { i64, i1 } %with_overflow164, 1
  %checked_overflow_widen167 = zext i1 %checked_overflow166 to i8
  store i64 %checked_result165, ptr %local_65, align 8
  store i8 %checked_overflow_widen167, ptr %local_66, align 1
  %cond_load168 = load i8, ptr %local_66, align 1
  %cond_nz169 = icmp ne i8 %cond_load168, 0
  br i1 %cond_nz169, label %bb47, label %bb48

bb43:                                             ; preds = %after_cooperate219
  %checked_lhs170 = load i64, ptr %local_55, align 8
  %checked_rhs171 = load i64, ptr %local_58, align 8
  %with_overflow172 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs170, i64 %checked_rhs171)
  %checked_result173 = extractvalue { i64, i1 } %with_overflow172, 0
  %checked_overflow174 = extractvalue { i64, i1 } %with_overflow172, 1
  %checked_overflow_widen175 = zext i1 %checked_overflow174 to i8
  store i64 %checked_result173, ptr %local_55, align 8
  store i8 %checked_overflow_widen175, ptr %local_75, align 1
  %cond_load176 = load i8, ptr %local_75, align 1
  %cond_nz177 = icmp ne i8 %cond_load176, 0
  br i1 %cond_nz177, label %bb55, label %bb41

bb44:                                             ; preds = %bb41
  %call_arg178 = load ptr, ptr %local_54, align 8
  %call_result179 = call ptr @"sort$sort_ints"(ptr %call_arg178)
  store ptr %call_result179, ptr %local_76, align 8
  br label %bb56

bb45:                                             ; preds = %bb40
  %"hew_vec_free drop180" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop180")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop181" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop181")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 202)
  call void @llvm.trap()
  unreachable

bb46:                                             ; preds = %bb40
  store i64 0, ptr %local_61, align 8
  store i64 17, ptr %local_62, align 8
  %move_load182 = load i64, ptr %local_61, align 8
  store i64 %move_load182, ptr %local_55, align 8
  %move_load183 = load i64, ptr %local_62, align 8
  store i64 %move_load183, ptr %local_56, align 8
  %hew_actor_cooperate184 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel185 = icmp eq i32 %hew_actor_cooperate184, 2
  br i1 %hew_cooperate_is_cancel185, label %cancel_exit186, label %after_cooperate187

bb47:                                             ; preds = %bb42
  %"hew_vec_free drop190" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop190")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop191" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop191")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb48:                                             ; preds = %bb42
  store i64 5, ptr %local_67, align 8
  store i64 0, ptr %local_69, align 8
  %cmp_lhs192 = load i64, ptr %local_67, align 8
  %cmp_rhs193 = load i64, ptr %local_69, align 8
  %cmp_bit194 = icmp eq i64 %cmp_lhs192, %cmp_rhs193
  %cmp_zext195 = zext i1 %cmp_bit194 to i8
  store i8 %cmp_zext195, ptr %local_70, align 1
  %cond_load196 = load i8, ptr %local_70, align 1
  %cond_nz197 = icmp ne i8 %cond_load196, 0
  br i1 %cond_nz197, label %bb49, label %bb50

bb49:                                             ; preds = %bb48
  %"hew_vec_free drop198" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop198")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop199" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop199")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 202)
  call void @llvm.trap()
  unreachable

bb50:                                             ; preds = %bb48
  store i64 -9223372036854775808, ptr %local_71, align 8
  %cmp_lhs200 = load i64, ptr %local_65, align 8
  %cmp_rhs201 = load i64, ptr %local_71, align 8
  %cmp_bit202 = icmp eq i64 %cmp_lhs200, %cmp_rhs201
  %cmp_zext203 = zext i1 %cmp_bit202 to i8
  store i8 %cmp_zext203, ptr %local_72, align 1
  %cond_load204 = load i8, ptr %local_72, align 1
  %cond_nz205 = icmp ne i8 %cond_load204, 0
  br i1 %cond_nz205, label %bb51, label %bb52

bb51:                                             ; preds = %bb50
  store i64 -1, ptr %local_73, align 8
  %cmp_lhs206 = load i64, ptr %local_67, align 8
  %cmp_rhs207 = load i64, ptr %local_73, align 8
  %cmp_bit208 = icmp eq i64 %cmp_lhs206, %cmp_rhs207
  %cmp_zext209 = zext i1 %cmp_bit208 to i8
  store i8 %cmp_zext209, ptr %local_74, align 1
  %cond_load210 = load i8, ptr %local_74, align 1
  %cond_nz211 = icmp ne i8 %cond_load210, 0
  br i1 %cond_nz211, label %bb53, label %bb52

bb52:                                             ; preds = %bb51, %bb50
  %div_lhs = load i64, ptr %local_65, align 8
  %div_rhs = load i64, ptr %local_67, align 8
  %srem = srem i64 %div_lhs, %div_rhs
  store i64 %srem, ptr %local_68, align 8
  %call_arg212 = load ptr, ptr %local_54, align 8
  %call_arg213 = load i64, ptr %local_68, align 8
  call void @hew_vec_push_i64(ptr %call_arg212, i64 %call_arg213)
  br label %bb54

bb53:                                             ; preds = %bb51
  %"hew_vec_free drop214" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop214")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop215" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop215")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 203)
  call void @llvm.trap()
  unreachable

bb54:                                             ; preds = %bb52
  %hew_actor_cooperate216 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel217 = icmp eq i32 %hew_actor_cooperate216, 2
  br i1 %hew_cooperate_is_cancel217, label %cancel_exit218, label %after_cooperate219

bb55:                                             ; preds = %bb43
  %"hew_vec_free drop222" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop222")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop223" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop223")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb56:                                             ; preds = %bb44
  %move_load224 = load ptr, ptr %local_76, align 8
  store ptr %move_load224, ptr %local_77, align 8
  store i64 0, ptr %local_78, align 8
  %move_load225 = load i64, ptr %local_78, align 8
  store i64 %move_load225, ptr %local_79, align 8
  store i64 0, ptr %local_80, align 8
  %move_load226 = load i64, ptr %local_80, align 8
  store i64 %move_load226, ptr %local_81, align 8
  store i64 0, ptr %local_82, align 8
  %move_load227 = load i64, ptr %local_82, align 8
  store i64 %move_load227, ptr %local_83, align 8
  store i64 0, ptr %local_84, align 8
  %move_load228 = load i64, ptr %local_84, align 8
  store i64 %move_load228, ptr %local_85, align 8
  store i64 1, ptr %local_88, align 8
  %move_load229 = load i64, ptr %local_88, align 8
  store i64 %move_load229, ptr %local_89, align 8
  store i64 0, ptr %local_90, align 8
  %cmp_lhs230 = load i64, ptr %local_89, align 8
  %cmp_rhs231 = load i64, ptr %local_90, align 8
  %cmp_bit232 = icmp sle i64 %cmp_lhs230, %cmp_rhs231
  %cmp_zext233 = zext i1 %cmp_bit232 to i8
  store i8 %cmp_zext233, ptr %local_91, align 1
  %cond_load234 = load i8, ptr %local_91, align 1
  %cond_nz235 = icmp ne i8 %cond_load234, 0
  br i1 %cond_nz235, label %bb61, label %bb62

bb57:                                             ; preds = %after_cooperate270, %bb59
  %cmp_lhs236 = load i64, ptr %local_86, align 8
  %cmp_rhs237 = load i64, ptr %local_87, align 8
  %cmp_bit238 = icmp slt i64 %cmp_lhs236, %cmp_rhs237
  %cmp_zext239 = zext i1 %cmp_bit238 to i8
  store i8 %cmp_zext239, ptr %local_94, align 1
  %cond_load240 = load i8, ptr %local_94, align 1
  %cond_nz241 = icmp ne i8 %cond_load240, 0
  br i1 %cond_nz241, label %bb58, label %bb60

bb58:                                             ; preds = %bb57
  store i8 0, ptr %local_96, align 1
  store i64 0, ptr %local_97, align 8
  %cmp_lhs242 = load i64, ptr %local_86, align 8
  %cmp_rhs243 = load i64, ptr %local_97, align 8
  %cmp_bit244 = icmp sgt i64 %cmp_lhs242, %cmp_rhs243
  %cmp_zext245 = zext i1 %cmp_bit244 to i8
  store i8 %cmp_zext245, ptr %local_98, align 1
  %cond_load246 = load i8, ptr %local_98, align 1
  %cond_nz247 = icmp ne i8 %cond_load246, 0
  br i1 %cond_nz247, label %bb63, label %bb64

bb59:                                             ; preds = %after_cooperate556
  %checked_lhs248 = load i64, ptr %local_86, align 8
  %checked_rhs249 = load i64, ptr %local_89, align 8
  %with_overflow250 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs248, i64 %checked_rhs249)
  %checked_result251 = extractvalue { i64, i1 } %with_overflow250, 0
  %checked_overflow252 = extractvalue { i64, i1 } %with_overflow250, 1
  %checked_overflow_widen253 = zext i1 %checked_overflow252 to i8
  store i64 %checked_result251, ptr %local_86, align 8
  store i8 %checked_overflow_widen253, ptr %local_157, align 1
  %cond_load254 = load i8, ptr %local_157, align 1
  %cond_nz255 = icmp ne i8 %cond_load254, 0
  br i1 %cond_nz255, label %bb112, label %bb57

bb60:                                             ; preds = %bb57
  store i8 1, ptr %local_159, align 1
  %cmp_lhs256 = load i64, ptr %local_79, align 8
  %cmp_rhs257 = load i64, ptr %local_81, align 8
  %cmp_bit258 = icmp ne i64 %cmp_lhs256, %cmp_rhs257
  %cmp_zext259 = zext i1 %cmp_bit258 to i8
  store i8 %cmp_zext259, ptr %local_160, align 1
  %cond_load260 = load i8, ptr %local_160, align 1
  %cond_nz261 = icmp ne i8 %cond_load260, 0
  br i1 %cond_nz261, label %bb114, label %bb113

bb61:                                             ; preds = %bb56
  %"hew_vec_free drop262" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop262")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop263" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop263")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop264" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop264")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 202)
  call void @llvm.trap()
  unreachable

bb62:                                             ; preds = %bb56
  store i64 0, ptr %local_92, align 8
  store i64 17, ptr %local_93, align 8
  %move_load265 = load i64, ptr %local_92, align 8
  store i64 %move_load265, ptr %local_86, align 8
  %move_load266 = load i64, ptr %local_93, align 8
  store i64 %move_load266, ptr %local_87, align 8
  %hew_actor_cooperate267 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel268 = icmp eq i32 %hew_actor_cooperate267, 2
  br i1 %hew_cooperate_is_cancel268, label %cancel_exit269, label %after_cooperate270

bb63:                                             ; preds = %bb58
  store i64 1, ptr %local_99, align 8
  %checked_lhs274 = load i64, ptr %local_86, align 8
  %checked_rhs275 = load i64, ptr %local_99, align 8
  %with_overflow276 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %checked_lhs274, i64 %checked_rhs275)
  %checked_result277 = extractvalue { i64, i1 } %with_overflow276, 0
  %checked_overflow278 = extractvalue { i64, i1 } %with_overflow276, 1
  %checked_overflow_widen279 = zext i1 %checked_overflow278 to i8
  store i64 %checked_result277, ptr %local_100, align 8
  store i8 %checked_overflow_widen279, ptr %local_101, align 1
  %cond_load280 = load i8, ptr %local_101, align 1
  %cond_nz281 = icmp ne i8 %cond_load280, 0
  br i1 %cond_nz281, label %bb65, label %bb66

bb64:                                             ; preds = %after_cooperate323, %bb58
  %cond_load282 = load i8, ptr %local_96, align 1
  %cond_nz283 = icmp ne i8 %cond_load282, 0
  br i1 %cond_nz283, label %bb71, label %bb72

bb65:                                             ; preds = %bb63
  %"hew_vec_free drop284" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop284")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop285" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop285")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop286" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop286")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb66:                                             ; preds = %bb63
  %"hew_vec_len arg0287" = load ptr, ptr %local_77, align 8
  %hew_vec_len_call288 = call i64 @hew_vec_len(ptr %"hew_vec_len arg0287")
  store i64 %hew_vec_len_call288, ptr %local_102, align 8
  %cmp_lhs289 = load i64, ptr %local_100, align 8
  %cmp_rhs290 = load i64, ptr %local_102, align 8
  %cmp_bit291 = icmp uge i64 %cmp_lhs289, %cmp_rhs290
  %cmp_zext292 = zext i1 %cmp_bit291 to i8
  store i8 %cmp_zext292, ptr %local_103, align 1
  %cond_load293 = load i8, ptr %local_103, align 1
  %cond_nz294 = icmp ne i8 %cond_load293, 0
  br i1 %cond_nz294, label %bb67, label %bb68

bb67:                                             ; preds = %bb66
  %"hew_vec_free drop295" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop295")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop296" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop296")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop297" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop297")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb68:                                             ; preds = %bb66
  %"hew_vec_get_i64 arg0298" = load ptr, ptr %local_77, align 8
  %"hew_vec_get_i64 arg1299" = load i64, ptr %local_100, align 8
  %hew_vec_get_i64_call300 = call i64 @hew_vec_get_i64(ptr %"hew_vec_get_i64 arg0298", i64 %"hew_vec_get_i64 arg1299")
  store i64 %hew_vec_get_i64_call300, ptr %local_104, align 8
  %"hew_vec_len arg0301" = load ptr, ptr %local_77, align 8
  %hew_vec_len_call302 = call i64 @hew_vec_len(ptr %"hew_vec_len arg0301")
  store i64 %hew_vec_len_call302, ptr %local_105, align 8
  %cmp_lhs303 = load i64, ptr %local_86, align 8
  %cmp_rhs304 = load i64, ptr %local_105, align 8
  %cmp_bit305 = icmp uge i64 %cmp_lhs303, %cmp_rhs304
  %cmp_zext306 = zext i1 %cmp_bit305 to i8
  store i8 %cmp_zext306, ptr %local_106, align 1
  %cond_load307 = load i8, ptr %local_106, align 1
  %cond_nz308 = icmp ne i8 %cond_load307, 0
  br i1 %cond_nz308, label %bb69, label %bb70

bb69:                                             ; preds = %bb68
  %"hew_vec_free drop309" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop309")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop310" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop310")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop311" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop311")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb70:                                             ; preds = %bb68
  %"hew_vec_get_i64 arg0312" = load ptr, ptr %local_77, align 8
  %"hew_vec_get_i64 arg1313" = load i64, ptr %local_86, align 8
  %hew_vec_get_i64_call314 = call i64 @hew_vec_get_i64(ptr %"hew_vec_get_i64 arg0312", i64 %"hew_vec_get_i64 arg1313")
  store i64 %hew_vec_get_i64_call314, ptr %local_107, align 8
  %cmp_lhs315 = load i64, ptr %local_104, align 8
  %cmp_rhs316 = load i64, ptr %local_107, align 8
  %cmp_bit317 = icmp sgt i64 %cmp_lhs315, %cmp_rhs316
  %cmp_zext318 = zext i1 %cmp_bit317 to i8
  store i8 %cmp_zext318, ptr %local_108, align 1
  %move_load319 = load i8, ptr %local_108, align 1
  store i8 %move_load319, ptr %local_96, align 1
  %hew_actor_cooperate320 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel321 = icmp eq i32 %hew_actor_cooperate320, 2
  br i1 %hew_cooperate_is_cancel321, label %cancel_exit322, label %after_cooperate323

bb71:                                             ; preds = %bb64
  store i64 13, ptr %local_109, align 8
  %move_load327 = load i64, ptr %local_109, align 8
  store i64 %move_load327, ptr %return_slot, align 8
  %"hew_vec_free drop328" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop328")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop329" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop329")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop330" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop330")
  store ptr null, ptr %local_1, align 8
  %ret_val331 = load i64, ptr %return_slot, align 8
  ret i64 %ret_val331

bb72:                                             ; preds = %bb64
  br label %bb73

bb73:                                             ; preds = %after_cooperate343, %bb72
  %"hew_vec_len arg0332" = load ptr, ptr %local_54, align 8
  %hew_vec_len_call333 = call i64 @hew_vec_len(ptr %"hew_vec_len arg0332")
  store i64 %hew_vec_len_call333, ptr %local_110, align 8
  %cmp_lhs334 = load i64, ptr %local_86, align 8
  %cmp_rhs335 = load i64, ptr %local_110, align 8
  %cmp_bit336 = icmp uge i64 %cmp_lhs334, %cmp_rhs335
  %cmp_zext337 = zext i1 %cmp_bit336 to i8
  store i8 %cmp_zext337, ptr %local_111, align 1
  %cond_load338 = load i8, ptr %local_111, align 1
  %cond_nz339 = icmp ne i8 %cond_load338, 0
  br i1 %cond_nz339, label %bb75, label %bb76

bb74:                                             ; No predecessors!
  %hew_actor_cooperate340 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel341 = icmp eq i32 %hew_actor_cooperate340, 2
  br i1 %hew_cooperate_is_cancel341, label %cancel_exit342, label %after_cooperate343

bb75:                                             ; preds = %bb73
  %"hew_vec_free drop344" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop344")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop345" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop345")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop346" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop346")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb76:                                             ; preds = %bb73
  %"hew_vec_get_i64 arg0347" = load ptr, ptr %local_54, align 8
  %"hew_vec_get_i64 arg1348" = load i64, ptr %local_86, align 8
  %hew_vec_get_i64_call349 = call i64 @hew_vec_get_i64(ptr %"hew_vec_get_i64 arg0347", i64 %"hew_vec_get_i64 arg1348")
  store i64 %hew_vec_get_i64_call349, ptr %local_112, align 8
  %checked_lhs350 = load i64, ptr %local_79, align 8
  %checked_rhs351 = load i64, ptr %local_112, align 8
  %with_overflow352 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs350, i64 %checked_rhs351)
  %checked_result353 = extractvalue { i64, i1 } %with_overflow352, 0
  %checked_overflow354 = extractvalue { i64, i1 } %with_overflow352, 1
  %checked_overflow_widen355 = zext i1 %checked_overflow354 to i8
  store i64 %checked_result353, ptr %local_113, align 8
  store i8 %checked_overflow_widen355, ptr %local_114, align 1
  %cond_load356 = load i8, ptr %local_114, align 1
  %cond_nz357 = icmp ne i8 %cond_load356, 0
  br i1 %cond_nz357, label %bb77, label %bb78

bb77:                                             ; preds = %bb76
  %"hew_vec_free drop358" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop358")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop359" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop359")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop360" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop360")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb78:                                             ; preds = %bb76
  %move_load361 = load i64, ptr %local_113, align 8
  store i64 %move_load361, ptr %local_79, align 8
  %"hew_vec_len arg0362" = load ptr, ptr %local_77, align 8
  %hew_vec_len_call363 = call i64 @hew_vec_len(ptr %"hew_vec_len arg0362")
  store i64 %hew_vec_len_call363, ptr %local_115, align 8
  %cmp_lhs364 = load i64, ptr %local_86, align 8
  %cmp_rhs365 = load i64, ptr %local_115, align 8
  %cmp_bit366 = icmp uge i64 %cmp_lhs364, %cmp_rhs365
  %cmp_zext367 = zext i1 %cmp_bit366 to i8
  store i8 %cmp_zext367, ptr %local_116, align 1
  %cond_load368 = load i8, ptr %local_116, align 1
  %cond_nz369 = icmp ne i8 %cond_load368, 0
  br i1 %cond_nz369, label %bb79, label %bb80

bb79:                                             ; preds = %bb78
  %"hew_vec_free drop370" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop370")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop371" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop371")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop372" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop372")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb80:                                             ; preds = %bb78
  %"hew_vec_get_i64 arg0373" = load ptr, ptr %local_77, align 8
  %"hew_vec_get_i64 arg1374" = load i64, ptr %local_86, align 8
  %hew_vec_get_i64_call375 = call i64 @hew_vec_get_i64(ptr %"hew_vec_get_i64 arg0373", i64 %"hew_vec_get_i64 arg1374")
  store i64 %hew_vec_get_i64_call375, ptr %local_117, align 8
  %checked_lhs376 = load i64, ptr %local_81, align 8
  %checked_rhs377 = load i64, ptr %local_117, align 8
  %with_overflow378 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs376, i64 %checked_rhs377)
  %checked_result379 = extractvalue { i64, i1 } %with_overflow378, 0
  %checked_overflow380 = extractvalue { i64, i1 } %with_overflow378, 1
  %checked_overflow_widen381 = zext i1 %checked_overflow380 to i8
  store i64 %checked_result379, ptr %local_118, align 8
  store i8 %checked_overflow_widen381, ptr %local_119, align 1
  %cond_load382 = load i8, ptr %local_119, align 1
  %cond_nz383 = icmp ne i8 %cond_load382, 0
  br i1 %cond_nz383, label %bb81, label %bb82

bb81:                                             ; preds = %bb80
  %"hew_vec_free drop384" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop384")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop385" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop385")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop386" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop386")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb82:                                             ; preds = %bb80
  %move_load387 = load i64, ptr %local_118, align 8
  store i64 %move_load387, ptr %local_81, align 8
  %"hew_vec_len arg0388" = load ptr, ptr %local_54, align 8
  %hew_vec_len_call389 = call i64 @hew_vec_len(ptr %"hew_vec_len arg0388")
  store i64 %hew_vec_len_call389, ptr %local_120, align 8
  %cmp_lhs390 = load i64, ptr %local_86, align 8
  %cmp_rhs391 = load i64, ptr %local_120, align 8
  %cmp_bit392 = icmp uge i64 %cmp_lhs390, %cmp_rhs391
  %cmp_zext393 = zext i1 %cmp_bit392 to i8
  store i8 %cmp_zext393, ptr %local_121, align 1
  %cond_load394 = load i8, ptr %local_121, align 1
  %cond_nz395 = icmp ne i8 %cond_load394, 0
  br i1 %cond_nz395, label %bb83, label %bb84

bb83:                                             ; preds = %bb82
  %"hew_vec_free drop396" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop396")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop397" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop397")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop398" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop398")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb84:                                             ; preds = %bb82
  %"hew_vec_get_i64 arg0399" = load ptr, ptr %local_54, align 8
  %"hew_vec_get_i64 arg1400" = load i64, ptr %local_86, align 8
  %hew_vec_get_i64_call401 = call i64 @hew_vec_get_i64(ptr %"hew_vec_get_i64 arg0399", i64 %"hew_vec_get_i64 arg1400")
  store i64 %hew_vec_get_i64_call401, ptr %local_122, align 8
  %"hew_vec_len arg0402" = load ptr, ptr %local_54, align 8
  %hew_vec_len_call403 = call i64 @hew_vec_len(ptr %"hew_vec_len arg0402")
  store i64 %hew_vec_len_call403, ptr %local_123, align 8
  %cmp_lhs404 = load i64, ptr %local_86, align 8
  %cmp_rhs405 = load i64, ptr %local_123, align 8
  %cmp_bit406 = icmp uge i64 %cmp_lhs404, %cmp_rhs405
  %cmp_zext407 = zext i1 %cmp_bit406 to i8
  store i8 %cmp_zext407, ptr %local_124, align 1
  %cond_load408 = load i8, ptr %local_124, align 1
  %cond_nz409 = icmp ne i8 %cond_load408, 0
  br i1 %cond_nz409, label %bb85, label %bb86

bb85:                                             ; preds = %bb84
  %"hew_vec_free drop410" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop410")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop411" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop411")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop412" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop412")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb86:                                             ; preds = %bb84
  %"hew_vec_get_i64 arg0413" = load ptr, ptr %local_54, align 8
  %"hew_vec_get_i64 arg1414" = load i64, ptr %local_86, align 8
  %hew_vec_get_i64_call415 = call i64 @hew_vec_get_i64(ptr %"hew_vec_get_i64 arg0413", i64 %"hew_vec_get_i64 arg1414")
  store i64 %hew_vec_get_i64_call415, ptr %local_125, align 8
  %checked_lhs416 = load i64, ptr %local_122, align 8
  %checked_rhs417 = load i64, ptr %local_125, align 8
  %with_overflow418 = call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %checked_lhs416, i64 %checked_rhs417)
  %checked_result419 = extractvalue { i64, i1 } %with_overflow418, 0
  %checked_overflow420 = extractvalue { i64, i1 } %with_overflow418, 1
  %checked_overflow_widen421 = zext i1 %checked_overflow420 to i8
  store i64 %checked_result419, ptr %local_126, align 8
  store i8 %checked_overflow_widen421, ptr %local_127, align 1
  %cond_load422 = load i8, ptr %local_127, align 1
  %cond_nz423 = icmp ne i8 %cond_load422, 0
  br i1 %cond_nz423, label %bb87, label %bb88

bb87:                                             ; preds = %bb86
  %"hew_vec_free drop424" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop424")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop425" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop425")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop426" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop426")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb88:                                             ; preds = %bb86
  %checked_lhs427 = load i64, ptr %local_83, align 8
  %checked_rhs428 = load i64, ptr %local_126, align 8
  %with_overflow429 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs427, i64 %checked_rhs428)
  %checked_result430 = extractvalue { i64, i1 } %with_overflow429, 0
  %checked_overflow431 = extractvalue { i64, i1 } %with_overflow429, 1
  %checked_overflow_widen432 = zext i1 %checked_overflow431 to i8
  store i64 %checked_result430, ptr %local_128, align 8
  store i8 %checked_overflow_widen432, ptr %local_129, align 1
  %cond_load433 = load i8, ptr %local_129, align 1
  %cond_nz434 = icmp ne i8 %cond_load433, 0
  br i1 %cond_nz434, label %bb89, label %bb90

bb89:                                             ; preds = %bb88
  %"hew_vec_free drop435" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop435")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop436" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop436")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop437" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop437")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb90:                                             ; preds = %bb88
  %move_load438 = load i64, ptr %local_128, align 8
  store i64 %move_load438, ptr %local_83, align 8
  %"hew_vec_len arg0439" = load ptr, ptr %local_77, align 8
  %hew_vec_len_call440 = call i64 @hew_vec_len(ptr %"hew_vec_len arg0439")
  store i64 %hew_vec_len_call440, ptr %local_130, align 8
  %cmp_lhs441 = load i64, ptr %local_86, align 8
  %cmp_rhs442 = load i64, ptr %local_130, align 8
  %cmp_bit443 = icmp uge i64 %cmp_lhs441, %cmp_rhs442
  %cmp_zext444 = zext i1 %cmp_bit443 to i8
  store i8 %cmp_zext444, ptr %local_131, align 1
  %cond_load445 = load i8, ptr %local_131, align 1
  %cond_nz446 = icmp ne i8 %cond_load445, 0
  br i1 %cond_nz446, label %bb91, label %bb92

bb91:                                             ; preds = %bb90
  %"hew_vec_free drop447" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop447")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop448" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop448")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop449" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop449")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb92:                                             ; preds = %bb90
  %"hew_vec_get_i64 arg0450" = load ptr, ptr %local_77, align 8
  %"hew_vec_get_i64 arg1451" = load i64, ptr %local_86, align 8
  %hew_vec_get_i64_call452 = call i64 @hew_vec_get_i64(ptr %"hew_vec_get_i64 arg0450", i64 %"hew_vec_get_i64 arg1451")
  store i64 %hew_vec_get_i64_call452, ptr %local_132, align 8
  %"hew_vec_len arg0453" = load ptr, ptr %local_77, align 8
  %hew_vec_len_call454 = call i64 @hew_vec_len(ptr %"hew_vec_len arg0453")
  store i64 %hew_vec_len_call454, ptr %local_133, align 8
  %cmp_lhs455 = load i64, ptr %local_86, align 8
  %cmp_rhs456 = load i64, ptr %local_133, align 8
  %cmp_bit457 = icmp uge i64 %cmp_lhs455, %cmp_rhs456
  %cmp_zext458 = zext i1 %cmp_bit457 to i8
  store i8 %cmp_zext458, ptr %local_134, align 1
  %cond_load459 = load i8, ptr %local_134, align 1
  %cond_nz460 = icmp ne i8 %cond_load459, 0
  br i1 %cond_nz460, label %bb93, label %bb94

bb93:                                             ; preds = %bb92
  %"hew_vec_free drop461" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop461")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop462" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop462")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop463" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop463")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb94:                                             ; preds = %bb92
  %"hew_vec_get_i64 arg0464" = load ptr, ptr %local_77, align 8
  %"hew_vec_get_i64 arg1465" = load i64, ptr %local_86, align 8
  %hew_vec_get_i64_call466 = call i64 @hew_vec_get_i64(ptr %"hew_vec_get_i64 arg0464", i64 %"hew_vec_get_i64 arg1465")
  store i64 %hew_vec_get_i64_call466, ptr %local_135, align 8
  %checked_lhs467 = load i64, ptr %local_132, align 8
  %checked_rhs468 = load i64, ptr %local_135, align 8
  %with_overflow469 = call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %checked_lhs467, i64 %checked_rhs468)
  %checked_result470 = extractvalue { i64, i1 } %with_overflow469, 0
  %checked_overflow471 = extractvalue { i64, i1 } %with_overflow469, 1
  %checked_overflow_widen472 = zext i1 %checked_overflow471 to i8
  store i64 %checked_result470, ptr %local_136, align 8
  store i8 %checked_overflow_widen472, ptr %local_137, align 1
  %cond_load473 = load i8, ptr %local_137, align 1
  %cond_nz474 = icmp ne i8 %cond_load473, 0
  br i1 %cond_nz474, label %bb95, label %bb96

bb95:                                             ; preds = %bb94
  %"hew_vec_free drop475" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop475")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop476" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop476")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop477" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop477")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb96:                                             ; preds = %bb94
  %checked_lhs478 = load i64, ptr %local_85, align 8
  %checked_rhs479 = load i64, ptr %local_136, align 8
  %with_overflow480 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs478, i64 %checked_rhs479)
  %checked_result481 = extractvalue { i64, i1 } %with_overflow480, 0
  %checked_overflow482 = extractvalue { i64, i1 } %with_overflow480, 1
  %checked_overflow_widen483 = zext i1 %checked_overflow482 to i8
  store i64 %checked_result481, ptr %local_138, align 8
  store i8 %checked_overflow_widen483, ptr %local_139, align 1
  %cond_load484 = load i8, ptr %local_139, align 1
  %cond_nz485 = icmp ne i8 %cond_load484, 0
  br i1 %cond_nz485, label %bb97, label %bb98

bb97:                                             ; preds = %bb96
  %"hew_vec_free drop486" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop486")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop487" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop487")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop488" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop488")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb98:                                             ; preds = %bb96
  %move_load489 = load i64, ptr %local_138, align 8
  store i64 %move_load489, ptr %local_85, align 8
  %"hew_vec_len arg0490" = load ptr, ptr %local_54, align 8
  %hew_vec_len_call491 = call i64 @hew_vec_len(ptr %"hew_vec_len arg0490")
  store i64 %hew_vec_len_call491, ptr %local_141, align 8
  %cmp_lhs492 = load i64, ptr %local_86, align 8
  %cmp_rhs493 = load i64, ptr %local_141, align 8
  %cmp_bit494 = icmp uge i64 %cmp_lhs492, %cmp_rhs493
  %cmp_zext495 = zext i1 %cmp_bit494 to i8
  store i8 %cmp_zext495, ptr %local_142, align 1
  %cond_load496 = load i8, ptr %local_142, align 1
  %cond_nz497 = icmp ne i8 %cond_load496, 0
  br i1 %cond_nz497, label %bb99, label %bb100

bb99:                                             ; preds = %bb98
  %"hew_vec_free drop498" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop498")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop499" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop499")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop500" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop500")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb100:                                            ; preds = %bb98
  %"hew_vec_get_i64 arg0501" = load ptr, ptr %local_54, align 8
  %"hew_vec_get_i64 arg1502" = load i64, ptr %local_86, align 8
  %hew_vec_get_i64_call503 = call i64 @hew_vec_get_i64(ptr %"hew_vec_get_i64 arg0501", i64 %"hew_vec_get_i64 arg1502")
  store i64 %hew_vec_get_i64_call503, ptr %local_143, align 8
  store i64 7, ptr %local_144, align 8
  %checked_lhs504 = load i64, ptr %local_86, align 8
  %checked_rhs505 = load i64, ptr %local_144, align 8
  %with_overflow506 = call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %checked_lhs504, i64 %checked_rhs505)
  %checked_result507 = extractvalue { i64, i1 } %with_overflow506, 0
  %checked_overflow508 = extractvalue { i64, i1 } %with_overflow506, 1
  %checked_overflow_widen509 = zext i1 %checked_overflow508 to i8
  store i64 %checked_result507, ptr %local_145, align 8
  store i8 %checked_overflow_widen509, ptr %local_146, align 1
  %cond_load510 = load i8, ptr %local_146, align 1
  %cond_nz511 = icmp ne i8 %cond_load510, 0
  br i1 %cond_nz511, label %bb101, label %bb102

bb101:                                            ; preds = %bb100
  %"hew_vec_free drop512" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop512")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop513" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop513")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop514" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop514")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb102:                                            ; preds = %bb100
  store i64 5, ptr %local_147, align 8
  store i64 0, ptr %local_149, align 8
  %cmp_lhs515 = load i64, ptr %local_147, align 8
  %cmp_rhs516 = load i64, ptr %local_149, align 8
  %cmp_bit517 = icmp eq i64 %cmp_lhs515, %cmp_rhs516
  %cmp_zext518 = zext i1 %cmp_bit517 to i8
  store i8 %cmp_zext518, ptr %local_150, align 1
  %cond_load519 = load i8, ptr %local_150, align 1
  %cond_nz520 = icmp ne i8 %cond_load519, 0
  br i1 %cond_nz520, label %bb103, label %bb104

bb103:                                            ; preds = %bb102
  %"hew_vec_free drop521" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop521")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop522" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop522")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop523" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop523")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 202)
  call void @llvm.trap()
  unreachable

bb104:                                            ; preds = %bb102
  store i64 -9223372036854775808, ptr %local_151, align 8
  %cmp_lhs524 = load i64, ptr %local_145, align 8
  %cmp_rhs525 = load i64, ptr %local_151, align 8
  %cmp_bit526 = icmp eq i64 %cmp_lhs524, %cmp_rhs525
  %cmp_zext527 = zext i1 %cmp_bit526 to i8
  store i8 %cmp_zext527, ptr %local_152, align 1
  %cond_load528 = load i8, ptr %local_152, align 1
  %cond_nz529 = icmp ne i8 %cond_load528, 0
  br i1 %cond_nz529, label %bb105, label %bb106

bb105:                                            ; preds = %bb104
  store i64 -1, ptr %local_153, align 8
  %cmp_lhs530 = load i64, ptr %local_147, align 8
  %cmp_rhs531 = load i64, ptr %local_153, align 8
  %cmp_bit532 = icmp eq i64 %cmp_lhs530, %cmp_rhs531
  %cmp_zext533 = zext i1 %cmp_bit532 to i8
  store i8 %cmp_zext533, ptr %local_154, align 1
  %cond_load534 = load i8, ptr %local_154, align 1
  %cond_nz535 = icmp ne i8 %cond_load534, 0
  br i1 %cond_nz535, label %bb107, label %bb106

bb106:                                            ; preds = %bb105, %bb104
  %div_lhs536 = load i64, ptr %local_145, align 8
  %div_rhs537 = load i64, ptr %local_147, align 8
  %srem538 = srem i64 %div_lhs536, %div_rhs537
  store i64 %srem538, ptr %local_148, align 8
  %cmp_lhs539 = load i64, ptr %local_143, align 8
  %cmp_rhs540 = load i64, ptr %local_148, align 8
  %cmp_bit541 = icmp ne i64 %cmp_lhs539, %cmp_rhs540
  %cmp_zext542 = zext i1 %cmp_bit541 to i8
  store i8 %cmp_zext542, ptr %local_155, align 1
  %cond_load543 = load i8, ptr %local_155, align 1
  %cond_nz544 = icmp ne i8 %cond_load543, 0
  br i1 %cond_nz544, label %bb108, label %bb109

bb107:                                            ; preds = %bb105
  %"hew_vec_free drop545" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop545")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop546" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop546")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop547" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop547")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 203)
  call void @llvm.trap()
  unreachable

bb108:                                            ; preds = %bb106
  store i64 14, ptr %local_156, align 8
  %move_load548 = load i64, ptr %local_156, align 8
  store i64 %move_load548, ptr %return_slot, align 8
  %"hew_vec_free drop549" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop549")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop550" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop550")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop551" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop551")
  store ptr null, ptr %local_1, align 8
  %ret_val552 = load i64, ptr %return_slot, align 8
  ret i64 %ret_val552

bb109:                                            ; preds = %bb106
  br label %bb110

bb110:                                            ; preds = %after_cooperate563, %bb109
  %hew_actor_cooperate553 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel554 = icmp eq i32 %hew_actor_cooperate553, 2
  br i1 %hew_cooperate_is_cancel554, label %cancel_exit555, label %after_cooperate556

bb111:                                            ; No predecessors!
  %hew_actor_cooperate560 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel561 = icmp eq i32 %hew_actor_cooperate560, 2
  br i1 %hew_cooperate_is_cancel561, label %cancel_exit562, label %after_cooperate563

bb112:                                            ; preds = %bb59
  %"hew_vec_free drop564" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop564")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop565" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop565")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop566" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop566")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb113:                                            ; preds = %bb60
  %cmp_lhs567 = load i64, ptr %local_83, align 8
  %cmp_rhs568 = load i64, ptr %local_85, align 8
  %cmp_bit569 = icmp ne i64 %cmp_lhs567, %cmp_rhs568
  %cmp_zext570 = zext i1 %cmp_bit569 to i8
  store i8 %cmp_zext570, ptr %local_161, align 1
  %move_load571 = load i8, ptr %local_161, align 1
  store i8 %move_load571, ptr %local_159, align 1
  br label %bb114

bb114:                                            ; preds = %bb113, %bb60
  %cond_load572 = load i8, ptr %local_159, align 1
  %cond_nz573 = icmp ne i8 %cond_load572, 0
  br i1 %cond_nz573, label %bb115, label %bb116

bb115:                                            ; preds = %bb114
  store i64 15, ptr %local_162, align 8
  %move_load574 = load i64, ptr %local_162, align 8
  store i64 %move_load574, ptr %return_slot, align 8
  %"hew_vec_free drop575" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop575")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop576" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop576")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop577" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop577")
  store ptr null, ptr %local_1, align 8
  %ret_val578 = load i64, ptr %return_slot, align 8
  ret i64 %ret_val578

bb116:                                            ; preds = %bb114
  br label %bb117

bb117:                                            ; preds = %after_cooperate583, %bb116
  %hew_vec_new_i64_call579 = call ptr @hew_vec_new_i64()
  store ptr %hew_vec_new_i64_call579, ptr %local_163, align 8
  br label %bb119

bb118:                                            ; No predecessors!
  %hew_actor_cooperate580 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel581 = icmp eq i32 %hew_actor_cooperate580, 2
  br i1 %hew_cooperate_is_cancel581, label %cancel_exit582, label %after_cooperate583

bb119:                                            ; preds = %bb117
  %move_load584 = load ptr, ptr %local_163, align 8
  store ptr %move_load584, ptr %local_164, align 8
  store i64 1, ptr %local_167, align 8
  %move_load585 = load i64, ptr %local_167, align 8
  store i64 %move_load585, ptr %local_168, align 8
  store i64 0, ptr %local_169, align 8
  %cmp_lhs586 = load i64, ptr %local_168, align 8
  %cmp_rhs587 = load i64, ptr %local_169, align 8
  %cmp_bit588 = icmp sle i64 %cmp_lhs586, %cmp_rhs587
  %cmp_zext589 = zext i1 %cmp_bit588 to i8
  store i8 %cmp_zext589, ptr %local_170, align 1
  %cond_load590 = load i8, ptr %local_170, align 1
  %cond_nz591 = icmp ne i8 %cond_load590, 0
  br i1 %cond_nz591, label %bb124, label %bb125

bb120:                                            ; preds = %after_cooperate619, %bb122
  %cmp_lhs592 = load i64, ptr %local_165, align 8
  %cmp_rhs593 = load i64, ptr %local_166, align 8
  %cmp_bit594 = icmp slt i64 %cmp_lhs592, %cmp_rhs593
  %cmp_zext595 = zext i1 %cmp_bit594 to i8
  store i8 %cmp_zext595, ptr %local_173, align 1
  %cond_load596 = load i8, ptr %local_173, align 1
  %cond_nz597 = icmp ne i8 %cond_load596, 0
  br i1 %cond_nz597, label %bb121, label %bb123

bb121:                                            ; preds = %bb120
  store i64 7, ptr %local_174, align 8
  %call_arg598 = load ptr, ptr %local_164, align 8
  %call_arg599 = load i64, ptr %local_174, align 8
  call void @hew_vec_push_i64(ptr %call_arg598, i64 %call_arg599)
  br label %bb126

bb122:                                            ; preds = %after_cooperate627
  %checked_lhs600 = load i64, ptr %local_165, align 8
  %checked_rhs601 = load i64, ptr %local_168, align 8
  %with_overflow602 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs600, i64 %checked_rhs601)
  %checked_result603 = extractvalue { i64, i1 } %with_overflow602, 0
  %checked_overflow604 = extractvalue { i64, i1 } %with_overflow602, 1
  %checked_overflow_widen605 = zext i1 %checked_overflow604 to i8
  store i64 %checked_result603, ptr %local_165, align 8
  store i8 %checked_overflow_widen605, ptr %local_175, align 1
  %cond_load606 = load i8, ptr %local_175, align 1
  %cond_nz607 = icmp ne i8 %cond_load606, 0
  br i1 %cond_nz607, label %bb127, label %bb120

bb123:                                            ; preds = %bb120
  %call_arg608 = load ptr, ptr %local_164, align 8
  %call_result609 = call ptr @"sort$sort_ints"(ptr %call_arg608)
  store ptr %call_result609, ptr %local_176, align 8
  br label %bb128

bb124:                                            ; preds = %bb119
  %"hew_vec_free drop610" = load ptr, ptr %local_164, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop610")
  store ptr null, ptr %local_164, align 8
  %"hew_vec_free drop611" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop611")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop612" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop612")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop613" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop613")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 202)
  call void @llvm.trap()
  unreachable

bb125:                                            ; preds = %bb119
  store i64 0, ptr %local_171, align 8
  store i64 33, ptr %local_172, align 8
  %move_load614 = load i64, ptr %local_171, align 8
  store i64 %move_load614, ptr %local_165, align 8
  %move_load615 = load i64, ptr %local_172, align 8
  store i64 %move_load615, ptr %local_166, align 8
  %hew_actor_cooperate616 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel617 = icmp eq i32 %hew_actor_cooperate616, 2
  br i1 %hew_cooperate_is_cancel617, label %cancel_exit618, label %after_cooperate619

bb126:                                            ; preds = %bb121
  %hew_actor_cooperate624 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel625 = icmp eq i32 %hew_actor_cooperate624, 2
  br i1 %hew_cooperate_is_cancel625, label %cancel_exit626, label %after_cooperate627

bb127:                                            ; preds = %bb122
  %"hew_vec_free drop632" = load ptr, ptr %local_164, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop632")
  store ptr null, ptr %local_164, align 8
  %"hew_vec_free drop633" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop633")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop634" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop634")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop635" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop635")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb128:                                            ; preds = %bb123
  %move_load636 = load ptr, ptr %local_176, align 8
  store ptr %move_load636, ptr %local_177, align 8
  store i64 1, ptr %local_180, align 8
  %move_load637 = load i64, ptr %local_180, align 8
  store i64 %move_load637, ptr %local_181, align 8
  store i64 0, ptr %local_182, align 8
  %cmp_lhs638 = load i64, ptr %local_181, align 8
  %cmp_rhs639 = load i64, ptr %local_182, align 8
  %cmp_bit640 = icmp sle i64 %cmp_lhs638, %cmp_rhs639
  %cmp_zext641 = zext i1 %cmp_bit640 to i8
  store i8 %cmp_zext641, ptr %local_183, align 1
  %cond_load642 = load i8, ptr %local_183, align 1
  %cond_nz643 = icmp ne i8 %cond_load642, 0
  br i1 %cond_nz643, label %bb133, label %bb134

bb129:                                            ; preds = %after_cooperate683, %bb131
  %cmp_lhs644 = load i64, ptr %local_178, align 8
  %cmp_rhs645 = load i64, ptr %local_179, align 8
  %cmp_bit646 = icmp slt i64 %cmp_lhs644, %cmp_rhs645
  %cmp_zext647 = zext i1 %cmp_bit646 to i8
  store i8 %cmp_zext647, ptr %local_186, align 1
  %cond_load648 = load i8, ptr %local_186, align 1
  %cond_nz649 = icmp ne i8 %cond_load648, 0
  br i1 %cond_nz649, label %bb130, label %bb132

bb130:                                            ; preds = %bb129
  store i8 1, ptr %local_188, align 1
  %"hew_vec_len arg0650" = load ptr, ptr %local_177, align 8
  %hew_vec_len_call651 = call i64 @hew_vec_len(ptr %"hew_vec_len arg0650")
  store i64 %hew_vec_len_call651, ptr %local_189, align 8
  %cmp_lhs652 = load i64, ptr %local_178, align 8
  %cmp_rhs653 = load i64, ptr %local_189, align 8
  %cmp_bit654 = icmp uge i64 %cmp_lhs652, %cmp_rhs653
  %cmp_zext655 = zext i1 %cmp_bit654 to i8
  store i8 %cmp_zext655, ptr %local_190, align 1
  %cond_load656 = load i8, ptr %local_190, align 1
  %cond_nz657 = icmp ne i8 %cond_load656, 0
  br i1 %cond_nz657, label %bb135, label %bb136

bb131:                                            ; preds = %after_cooperate745
  %checked_lhs658 = load i64, ptr %local_178, align 8
  %checked_rhs659 = load i64, ptr %local_181, align 8
  %with_overflow660 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs658, i64 %checked_rhs659)
  %checked_result661 = extractvalue { i64, i1 } %with_overflow660, 0
  %checked_overflow662 = extractvalue { i64, i1 } %with_overflow660, 1
  %checked_overflow_widen663 = zext i1 %checked_overflow662 to i8
  store i64 %checked_result661, ptr %local_178, align 8
  store i8 %checked_overflow_widen663, ptr %local_200, align 1
  %cond_load664 = load i8, ptr %local_200, align 1
  %cond_nz665 = icmp ne i8 %cond_load664, 0
  br i1 %cond_nz665, label %bb145, label %bb129

bb132:                                            ; preds = %bb129
  store i64 0, ptr %local_201, align 8
  %move_load666 = load i64, ptr %local_201, align 8
  store i64 %move_load666, ptr %return_slot, align 8
  %"hew_vec_free drop667" = load ptr, ptr %local_177, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop667")
  store ptr null, ptr %local_177, align 8
  %"hew_vec_free drop668" = load ptr, ptr %local_164, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop668")
  store ptr null, ptr %local_164, align 8
  %"hew_vec_free drop669" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop669")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop670" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop670")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop671" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop671")
  store ptr null, ptr %local_1, align 8
  %ret_val672 = load i64, ptr %return_slot, align 8
  ret i64 %ret_val672

bb133:                                            ; preds = %bb128
  %"hew_vec_free drop673" = load ptr, ptr %local_177, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop673")
  store ptr null, ptr %local_177, align 8
  %"hew_vec_free drop674" = load ptr, ptr %local_164, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop674")
  store ptr null, ptr %local_164, align 8
  %"hew_vec_free drop675" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop675")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop676" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop676")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop677" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop677")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 202)
  call void @llvm.trap()
  unreachable

bb134:                                            ; preds = %bb128
  store i64 0, ptr %local_184, align 8
  store i64 33, ptr %local_185, align 8
  %move_load678 = load i64, ptr %local_184, align 8
  store i64 %move_load678, ptr %local_178, align 8
  %move_load679 = load i64, ptr %local_185, align 8
  store i64 %move_load679, ptr %local_179, align 8
  %hew_actor_cooperate680 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel681 = icmp eq i32 %hew_actor_cooperate680, 2
  br i1 %hew_cooperate_is_cancel681, label %cancel_exit682, label %after_cooperate683

bb135:                                            ; preds = %bb130
  %"hew_vec_free drop689" = load ptr, ptr %local_177, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop689")
  store ptr null, ptr %local_177, align 8
  %"hew_vec_free drop690" = load ptr, ptr %local_164, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop690")
  store ptr null, ptr %local_164, align 8
  %"hew_vec_free drop691" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop691")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop692" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop692")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop693" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop693")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb136:                                            ; preds = %bb130
  %"hew_vec_get_i64 arg0694" = load ptr, ptr %local_177, align 8
  %"hew_vec_get_i64 arg1695" = load i64, ptr %local_178, align 8
  %hew_vec_get_i64_call696 = call i64 @hew_vec_get_i64(ptr %"hew_vec_get_i64 arg0694", i64 %"hew_vec_get_i64 arg1695")
  store i64 %hew_vec_get_i64_call696, ptr %local_191, align 8
  store i64 7, ptr %local_192, align 8
  %cmp_lhs697 = load i64, ptr %local_191, align 8
  %cmp_rhs698 = load i64, ptr %local_192, align 8
  %cmp_bit699 = icmp ne i64 %cmp_lhs697, %cmp_rhs698
  %cmp_zext700 = zext i1 %cmp_bit699 to i8
  store i8 %cmp_zext700, ptr %local_193, align 1
  %cond_load701 = load i8, ptr %local_193, align 1
  %cond_nz702 = icmp ne i8 %cond_load701, 0
  br i1 %cond_nz702, label %bb138, label %bb137

bb137:                                            ; preds = %bb136
  %"hew_vec_len arg0703" = load ptr, ptr %local_164, align 8
  %hew_vec_len_call704 = call i64 @hew_vec_len(ptr %"hew_vec_len arg0703")
  store i64 %hew_vec_len_call704, ptr %local_194, align 8
  %cmp_lhs705 = load i64, ptr %local_178, align 8
  %cmp_rhs706 = load i64, ptr %local_194, align 8
  %cmp_bit707 = icmp uge i64 %cmp_lhs705, %cmp_rhs706
  %cmp_zext708 = zext i1 %cmp_bit707 to i8
  store i8 %cmp_zext708, ptr %local_195, align 1
  %cond_load709 = load i8, ptr %local_195, align 1
  %cond_nz710 = icmp ne i8 %cond_load709, 0
  br i1 %cond_nz710, label %bb139, label %bb140

bb138:                                            ; preds = %after_cooperate729, %bb136
  %cond_load711 = load i8, ptr %local_188, align 1
  %cond_nz712 = icmp ne i8 %cond_load711, 0
  br i1 %cond_nz712, label %bb141, label %bb142

bb139:                                            ; preds = %bb137
  %"hew_vec_free drop713" = load ptr, ptr %local_177, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop713")
  store ptr null, ptr %local_177, align 8
  %"hew_vec_free drop714" = load ptr, ptr %local_164, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop714")
  store ptr null, ptr %local_164, align 8
  %"hew_vec_free drop715" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop715")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop716" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop716")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop717" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop717")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb140:                                            ; preds = %bb137
  %"hew_vec_get_i64 arg0718" = load ptr, ptr %local_164, align 8
  %"hew_vec_get_i64 arg1719" = load i64, ptr %local_178, align 8
  %hew_vec_get_i64_call720 = call i64 @hew_vec_get_i64(ptr %"hew_vec_get_i64 arg0718", i64 %"hew_vec_get_i64 arg1719")
  store i64 %hew_vec_get_i64_call720, ptr %local_196, align 8
  store i64 7, ptr %local_197, align 8
  %cmp_lhs721 = load i64, ptr %local_196, align 8
  %cmp_rhs722 = load i64, ptr %local_197, align 8
  %cmp_bit723 = icmp ne i64 %cmp_lhs721, %cmp_rhs722
  %cmp_zext724 = zext i1 %cmp_bit723 to i8
  store i8 %cmp_zext724, ptr %local_198, align 1
  %move_load725 = load i8, ptr %local_198, align 1
  store i8 %move_load725, ptr %local_188, align 1
  %hew_actor_cooperate726 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel727 = icmp eq i32 %hew_actor_cooperate726, 2
  br i1 %hew_cooperate_is_cancel727, label %cancel_exit728, label %after_cooperate729

bb141:                                            ; preds = %bb138
  store i64 16, ptr %local_199, align 8
  %move_load735 = load i64, ptr %local_199, align 8
  store i64 %move_load735, ptr %return_slot, align 8
  %"hew_vec_free drop736" = load ptr, ptr %local_177, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop736")
  store ptr null, ptr %local_177, align 8
  %"hew_vec_free drop737" = load ptr, ptr %local_164, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop737")
  store ptr null, ptr %local_164, align 8
  %"hew_vec_free drop738" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop738")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop739" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop739")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop740" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop740")
  store ptr null, ptr %local_1, align 8
  %ret_val741 = load i64, ptr %return_slot, align 8
  ret i64 %ret_val741

bb142:                                            ; preds = %bb138
  br label %bb143

bb143:                                            ; preds = %after_cooperate754, %bb142
  %hew_actor_cooperate742 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel743 = icmp eq i32 %hew_actor_cooperate742, 2
  br i1 %hew_cooperate_is_cancel743, label %cancel_exit744, label %after_cooperate745

bb144:                                            ; No predecessors!
  %hew_actor_cooperate751 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel752 = icmp eq i32 %hew_actor_cooperate751, 2
  br i1 %hew_cooperate_is_cancel752, label %cancel_exit753, label %after_cooperate754

bb145:                                            ; preds = %bb131
  %"hew_vec_free drop755" = load ptr, ptr %local_177, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop755")
  store ptr null, ptr %local_177, align 8
  %"hew_vec_free drop756" = load ptr, ptr %local_164, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop756")
  store ptr null, ptr %local_164, align 8
  %"hew_vec_free drop757" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop757")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop758" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop758")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop759" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop759")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

cancel_exit:                                      ; preds = %entry
  ret i64 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit22:                                    ; preds = %bb7
  %"hew_vec_free drop24" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop24")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate23:                                ; preds = %bb7
  br label %bb2

cancel_exit30:                                    ; preds = %bb10
  %"hew_vec_free drop32" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop32")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate31:                                ; preds = %bb10
  br label %bb4

cancel_exit63:                                    ; preds = %bb18
  ret i64 0

after_cooperate64:                                ; preds = %bb18
  br label %bb17

cancel_exit91:                                    ; preds = %bb24
  %"hew_vec_free drop93" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop93")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate92:                                ; preds = %bb24
  br label %bb19

cancel_exit114:                                   ; preds = %bb30
  ret i64 0

after_cooperate115:                               ; preds = %bb30
  br label %bb29

cancel_exit140:                                   ; preds = %bb37
  %"hew_vec_free drop142" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop142")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate141:                               ; preds = %bb37
  br label %bb21

cancel_exit145:                                   ; preds = %bb38
  ret i64 0

after_cooperate146:                               ; preds = %bb38
  br label %bb37

cancel_exit186:                                   ; preds = %bb46
  %"hew_vec_free drop188" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop188")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop189" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop189")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate187:                               ; preds = %bb46
  br label %bb41

cancel_exit218:                                   ; preds = %bb54
  %"hew_vec_free drop220" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop220")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop221" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop221")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate219:                               ; preds = %bb54
  br label %bb43

cancel_exit269:                                   ; preds = %bb62
  %"hew_vec_free drop271" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop271")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop272" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop272")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop273" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop273")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate270:                               ; preds = %bb62
  br label %bb57

cancel_exit322:                                   ; preds = %bb70
  %"hew_vec_free drop324" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop324")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop325" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop325")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop326" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop326")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate323:                               ; preds = %bb70
  br label %bb64

cancel_exit342:                                   ; preds = %bb74
  ret i64 0

after_cooperate343:                               ; preds = %bb74
  br label %bb73

cancel_exit555:                                   ; preds = %bb110
  %"hew_vec_free drop557" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop557")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop558" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop558")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop559" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop559")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate556:                               ; preds = %bb110
  br label %bb59

cancel_exit562:                                   ; preds = %bb111
  ret i64 0

after_cooperate563:                               ; preds = %bb111
  br label %bb110

cancel_exit582:                                   ; preds = %bb118
  ret i64 0

after_cooperate583:                               ; preds = %bb118
  br label %bb117

cancel_exit618:                                   ; preds = %bb125
  %"hew_vec_free drop620" = load ptr, ptr %local_164, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop620")
  store ptr null, ptr %local_164, align 8
  %"hew_vec_free drop621" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop621")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop622" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop622")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop623" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop623")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate619:                               ; preds = %bb125
  br label %bb120

cancel_exit626:                                   ; preds = %bb126
  %"hew_vec_free drop628" = load ptr, ptr %local_164, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop628")
  store ptr null, ptr %local_164, align 8
  %"hew_vec_free drop629" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop629")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop630" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop630")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop631" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop631")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate627:                               ; preds = %bb126
  br label %bb122

cancel_exit682:                                   ; preds = %bb134
  %"hew_vec_free drop684" = load ptr, ptr %local_177, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop684")
  store ptr null, ptr %local_177, align 8
  %"hew_vec_free drop685" = load ptr, ptr %local_164, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop685")
  store ptr null, ptr %local_164, align 8
  %"hew_vec_free drop686" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop686")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop687" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop687")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop688" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop688")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate683:                               ; preds = %bb134
  br label %bb129

cancel_exit728:                                   ; preds = %bb140
  %"hew_vec_free drop730" = load ptr, ptr %local_177, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop730")
  store ptr null, ptr %local_177, align 8
  %"hew_vec_free drop731" = load ptr, ptr %local_164, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop731")
  store ptr null, ptr %local_164, align 8
  %"hew_vec_free drop732" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop732")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop733" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop733")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop734" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop734")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate729:                               ; preds = %bb140
  br label %bb138

cancel_exit744:                                   ; preds = %bb143
  %"hew_vec_free drop746" = load ptr, ptr %local_177, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop746")
  store ptr null, ptr %local_177, align 8
  %"hew_vec_free drop747" = load ptr, ptr %local_164, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop747")
  store ptr null, ptr %local_164, align 8
  %"hew_vec_free drop748" = load ptr, ptr %local_77, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop748")
  store ptr null, ptr %local_77, align 8
  %"hew_vec_free drop749" = load ptr, ptr %local_54, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop749")
  store ptr null, ptr %local_54, align 8
  %"hew_vec_free drop750" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop750")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate745:                               ; preds = %bb143
  br label %bb131

cancel_exit753:                                   ; preds = %bb144
  ret i64 0

after_cooperate754:                               ; preds = %bb144
  br label %bb143
}

define internal ptr @reverse_key(i64 %0) {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca i64, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i64, align 8
  %local_5 = alloca i8, align 1
  %local_6 = alloca i64, align 8
  %local_7 = alloca i8, align 1
  %local_8 = alloca i64, align 8
  %local_9 = alloca i8, align 1
  %local_10 = alloca i64, align 8
  %local_11 = alloca i8, align 1
  %local_12 = alloca ptr, align 8
  %local_13 = alloca i64, align 8
  %local_14 = alloca i8, align 1
  %local_15 = alloca ptr, align 8
  %local_16 = alloca i64, align 8
  %local_17 = alloca i8, align 1
  %local_18 = alloca ptr, align 8
  %local_19 = alloca i64, align 8
  %local_20 = alloca i8, align 1
  %local_21 = alloca ptr, align 8
  %local_22 = alloca i64, align 8
  %local_23 = alloca i8, align 1
  %local_24 = alloca ptr, align 8
  %local_25 = alloca i64, align 8
  %local_26 = alloca i8, align 1
  %local_27 = alloca ptr, align 8
  %local_28 = alloca i64, align 8
  %local_29 = alloca i8, align 1
  %local_30 = alloca ptr, align 8
  %local_31 = alloca ptr, align 8
  store i64 %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  store i64 8, ptr %local_2, align 8
  store i64 0, ptr %local_4, align 8
  %cmp_lhs = load i64, ptr %local_2, align 8
  %cmp_rhs = load i64, ptr %local_4, align 8
  %cmp_bit = icmp eq i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_5, align 1
  %cond_load = load i8, ptr %local_5, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  call void @hew_trap_with_code(i32 202)
  call void @llvm.trap()
  unreachable

bb2:                                              ; preds = %bb0
  store i64 -9223372036854775808, ptr %local_6, align 8
  %cmp_lhs1 = load i64, ptr %local_0, align 8
  %cmp_rhs2 = load i64, ptr %local_6, align 8
  %cmp_bit3 = icmp eq i64 %cmp_lhs1, %cmp_rhs2
  %cmp_zext4 = zext i1 %cmp_bit3 to i8
  store i8 %cmp_zext4, ptr %local_7, align 1
  %cond_load5 = load i8, ptr %local_7, align 1
  %cond_nz6 = icmp ne i8 %cond_load5, 0
  br i1 %cond_nz6, label %bb3, label %bb4

bb3:                                              ; preds = %bb2
  store i64 -1, ptr %local_8, align 8
  %cmp_lhs7 = load i64, ptr %local_2, align 8
  %cmp_rhs8 = load i64, ptr %local_8, align 8
  %cmp_bit9 = icmp eq i64 %cmp_lhs7, %cmp_rhs8
  %cmp_zext10 = zext i1 %cmp_bit9 to i8
  store i8 %cmp_zext10, ptr %local_9, align 1
  %cond_load11 = load i8, ptr %local_9, align 1
  %cond_nz12 = icmp ne i8 %cond_load11, 0
  br i1 %cond_nz12, label %bb5, label %bb4

bb4:                                              ; preds = %bb3, %bb2
  %div_lhs = load i64, ptr %local_0, align 8
  %div_rhs = load i64, ptr %local_2, align 8
  %srem = srem i64 %div_lhs, %div_rhs
  store i64 %srem, ptr %local_3, align 8
  br label %bb7

bb5:                                              ; preds = %bb3
  call void @hew_trap_with_code(i32 203)
  call void @llvm.trap()
  unreachable

bb6:                                              ; preds = %after_cooperate94, %after_cooperate89, %after_cooperate84, %after_cooperate79, %after_cooperate74, %after_cooperate69, %after_cooperate64, %after_cooperate59
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

bb7:                                              ; preds = %bb4
  store i64 0, ptr %local_10, align 8
  %cmp_lhs13 = load i64, ptr %local_3, align 8
  %cmp_rhs14 = load i64, ptr %local_10, align 8
  %cmp_bit15 = icmp eq i64 %cmp_lhs13, %cmp_rhs14
  %cmp_zext16 = zext i1 %cmp_bit15 to i8
  store i8 %cmp_zext16, ptr %local_11, align 1
  %cond_load17 = load i8, ptr %local_11, align 1
  %cond_nz18 = icmp ne i8 %cond_load17, 0
  br i1 %cond_nz18, label %bb16, label %bb8

bb8:                                              ; preds = %bb7
  store i64 1, ptr %local_13, align 8
  %cmp_lhs19 = load i64, ptr %local_3, align 8
  %cmp_rhs20 = load i64, ptr %local_13, align 8
  %cmp_bit21 = icmp eq i64 %cmp_lhs19, %cmp_rhs20
  %cmp_zext22 = zext i1 %cmp_bit21 to i8
  store i8 %cmp_zext22, ptr %local_14, align 1
  %cond_load23 = load i8, ptr %local_14, align 1
  %cond_nz24 = icmp ne i8 %cond_load23, 0
  br i1 %cond_nz24, label %bb17, label %bb9

bb9:                                              ; preds = %bb8
  store i64 2, ptr %local_16, align 8
  %cmp_lhs25 = load i64, ptr %local_3, align 8
  %cmp_rhs26 = load i64, ptr %local_16, align 8
  %cmp_bit27 = icmp eq i64 %cmp_lhs25, %cmp_rhs26
  %cmp_zext28 = zext i1 %cmp_bit27 to i8
  store i8 %cmp_zext28, ptr %local_17, align 1
  %cond_load29 = load i8, ptr %local_17, align 1
  %cond_nz30 = icmp ne i8 %cond_load29, 0
  br i1 %cond_nz30, label %bb18, label %bb10

bb10:                                             ; preds = %bb9
  store i64 3, ptr %local_19, align 8
  %cmp_lhs31 = load i64, ptr %local_3, align 8
  %cmp_rhs32 = load i64, ptr %local_19, align 8
  %cmp_bit33 = icmp eq i64 %cmp_lhs31, %cmp_rhs32
  %cmp_zext34 = zext i1 %cmp_bit33 to i8
  store i8 %cmp_zext34, ptr %local_20, align 1
  %cond_load35 = load i8, ptr %local_20, align 1
  %cond_nz36 = icmp ne i8 %cond_load35, 0
  br i1 %cond_nz36, label %bb19, label %bb11

bb11:                                             ; preds = %bb10
  store i64 4, ptr %local_22, align 8
  %cmp_lhs37 = load i64, ptr %local_3, align 8
  %cmp_rhs38 = load i64, ptr %local_22, align 8
  %cmp_bit39 = icmp eq i64 %cmp_lhs37, %cmp_rhs38
  %cmp_zext40 = zext i1 %cmp_bit39 to i8
  store i8 %cmp_zext40, ptr %local_23, align 1
  %cond_load41 = load i8, ptr %local_23, align 1
  %cond_nz42 = icmp ne i8 %cond_load41, 0
  br i1 %cond_nz42, label %bb20, label %bb12

bb12:                                             ; preds = %bb11
  store i64 5, ptr %local_25, align 8
  %cmp_lhs43 = load i64, ptr %local_3, align 8
  %cmp_rhs44 = load i64, ptr %local_25, align 8
  %cmp_bit45 = icmp eq i64 %cmp_lhs43, %cmp_rhs44
  %cmp_zext46 = zext i1 %cmp_bit45 to i8
  store i8 %cmp_zext46, ptr %local_26, align 1
  %cond_load47 = load i8, ptr %local_26, align 1
  %cond_nz48 = icmp ne i8 %cond_load47, 0
  br i1 %cond_nz48, label %bb21, label %bb13

bb13:                                             ; preds = %bb12
  store i64 6, ptr %local_28, align 8
  %cmp_lhs49 = load i64, ptr %local_3, align 8
  %cmp_rhs50 = load i64, ptr %local_28, align 8
  %cmp_bit51 = icmp eq i64 %cmp_lhs49, %cmp_rhs50
  %cmp_zext52 = zext i1 %cmp_bit51 to i8
  store i8 %cmp_zext52, ptr %local_29, align 1
  %cond_load53 = load i8, ptr %local_29, align 1
  %cond_nz54 = icmp ne i8 %cond_load53, 0
  br i1 %cond_nz54, label %bb22, label %bb14

bb14:                                             ; preds = %bb13
  store ptr @str_lit, ptr %local_31, align 8
  %move_load55 = load ptr, ptr %local_31, align 8
  store ptr %move_load55, ptr %local_1, align 8
  %hew_actor_cooperate56 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel57 = icmp eq i32 %hew_actor_cooperate56, 2
  br i1 %hew_cooperate_is_cancel57, label %cancel_exit58, label %after_cooperate59

bb15:                                             ; No predecessors!
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

bb16:                                             ; preds = %bb7
  store ptr @str_lit.1, ptr %local_12, align 8
  %move_load60 = load ptr, ptr %local_12, align 8
  store ptr %move_load60, ptr %local_1, align 8
  %hew_actor_cooperate61 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel62 = icmp eq i32 %hew_actor_cooperate61, 2
  br i1 %hew_cooperate_is_cancel62, label %cancel_exit63, label %after_cooperate64

bb17:                                             ; preds = %bb8
  store ptr @str_lit.2, ptr %local_15, align 8
  %move_load65 = load ptr, ptr %local_15, align 8
  store ptr %move_load65, ptr %local_1, align 8
  %hew_actor_cooperate66 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel67 = icmp eq i32 %hew_actor_cooperate66, 2
  br i1 %hew_cooperate_is_cancel67, label %cancel_exit68, label %after_cooperate69

bb18:                                             ; preds = %bb9
  store ptr @str_lit.3, ptr %local_18, align 8
  %move_load70 = load ptr, ptr %local_18, align 8
  store ptr %move_load70, ptr %local_1, align 8
  %hew_actor_cooperate71 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel72 = icmp eq i32 %hew_actor_cooperate71, 2
  br i1 %hew_cooperate_is_cancel72, label %cancel_exit73, label %after_cooperate74

bb19:                                             ; preds = %bb10
  store ptr @str_lit.4, ptr %local_21, align 8
  %move_load75 = load ptr, ptr %local_21, align 8
  store ptr %move_load75, ptr %local_1, align 8
  %hew_actor_cooperate76 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel77 = icmp eq i32 %hew_actor_cooperate76, 2
  br i1 %hew_cooperate_is_cancel77, label %cancel_exit78, label %after_cooperate79

bb20:                                             ; preds = %bb11
  store ptr @str_lit.5, ptr %local_24, align 8
  %move_load80 = load ptr, ptr %local_24, align 8
  store ptr %move_load80, ptr %local_1, align 8
  %hew_actor_cooperate81 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel82 = icmp eq i32 %hew_actor_cooperate81, 2
  br i1 %hew_cooperate_is_cancel82, label %cancel_exit83, label %after_cooperate84

bb21:                                             ; preds = %bb12
  store ptr @str_lit.6, ptr %local_27, align 8
  %move_load85 = load ptr, ptr %local_27, align 8
  store ptr %move_load85, ptr %local_1, align 8
  %hew_actor_cooperate86 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel87 = icmp eq i32 %hew_actor_cooperate86, 2
  br i1 %hew_cooperate_is_cancel87, label %cancel_exit88, label %after_cooperate89

bb22:                                             ; preds = %bb13
  store ptr @str_lit.7, ptr %local_30, align 8
  %move_load90 = load ptr, ptr %local_30, align 8
  store ptr %move_load90, ptr %local_1, align 8
  %hew_actor_cooperate91 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel92 = icmp eq i32 %hew_actor_cooperate91, 2
  br i1 %hew_cooperate_is_cancel92, label %cancel_exit93, label %after_cooperate94

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit58:                                    ; preds = %bb14
  ret ptr null

after_cooperate59:                                ; preds = %bb14
  br label %bb6

cancel_exit63:                                    ; preds = %bb16
  ret ptr null

after_cooperate64:                                ; preds = %bb16
  br label %bb6

cancel_exit68:                                    ; preds = %bb17
  ret ptr null

after_cooperate69:                                ; preds = %bb17
  br label %bb6

cancel_exit73:                                    ; preds = %bb18
  ret ptr null

after_cooperate74:                                ; preds = %bb18
  br label %bb6

cancel_exit78:                                    ; preds = %bb19
  ret ptr null

after_cooperate79:                                ; preds = %bb19
  br label %bb6

cancel_exit83:                                    ; preds = %bb20
  ret ptr null

after_cooperate84:                                ; preds = %bb20
  br label %bb6

cancel_exit88:                                    ; preds = %bb21
  ret ptr null

after_cooperate89:                                ; preds = %bb21
  br label %bb6

cancel_exit93:                                    ; preds = %bb22
  ret ptr null

after_cooperate94:                                ; preds = %bb22
  br label %bb6
}

define internal i64 @check_strings() {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i64, align 8
  %local_5 = alloca i64, align 8
  %local_6 = alloca i64, align 8
  %local_7 = alloca i8, align 1
  %local_8 = alloca i64, align 8
  %local_9 = alloca i64, align 8
  %local_10 = alloca i8, align 1
  %local_11 = alloca ptr, align 8
  %local_12 = alloca i8, align 1
  %local_13 = alloca { ptr, i64 }, align 8
  %local_14 = alloca { ptr, i64 }, align 8
  %local_15 = alloca ptr, align 8
  %local_16 = alloca ptr, align 8
  %local_17 = alloca i64, align 8
  %local_18 = alloca i64, align 8
  %local_19 = alloca i8, align 1
  %local_20 = alloca i64, align 8
  %local_21 = alloca i64, align 8
  %local_22 = alloca i64, align 8
  %local_23 = alloca i8, align 1
  %local_24 = alloca i8, align 1
  %local_25 = alloca i64, align 8
  %local_26 = alloca i64, align 8
  %local_27 = alloca i64, align 8
  %local_28 = alloca i64, align 8
  %local_29 = alloca i64, align 8
  %local_30 = alloca i64, align 8
  %local_31 = alloca i8, align 1
  %local_32 = alloca i64, align 8
  %local_33 = alloca i64, align 8
  %local_34 = alloca i8, align 1
  %local_35 = alloca i8, align 1
  %local_36 = alloca i64, align 8
  %local_37 = alloca i64, align 8
  %local_38 = alloca i8, align 1
  %local_39 = alloca i64, align 8
  %local_40 = alloca i8, align 1
  %local_41 = alloca ptr, align 8
  %local_42 = alloca i64, align 8
  %local_43 = alloca i8, align 1
  %local_44 = alloca ptr, align 8
  %local_45 = alloca i8, align 1
  %local_46 = alloca i64, align 8
  %local_47 = alloca i8, align 1
  %local_48 = alloca i64, align 8
  %local_49 = alloca i64, align 8
  %local_50 = alloca i64, align 8
  %local_51 = alloca i64, align 8
  %local_52 = alloca i64, align 8
  %local_53 = alloca i8, align 1
  %local_54 = alloca i64, align 8
  %local_55 = alloca i64, align 8
  %local_56 = alloca i8, align 1
  %local_57 = alloca i8, align 1
  %local_58 = alloca i64, align 8
  %local_59 = alloca i8, align 1
  %local_60 = alloca ptr, align 8
  %local_61 = alloca ptr, align 8
  %local_62 = alloca i8, align 1
  %local_63 = alloca i64, align 8
  %local_64 = alloca i8, align 1
  %local_65 = alloca i64, align 8
  %local_66 = alloca i64, align 8
  %local_67 = alloca i8, align 1
  %local_68 = alloca i64, align 8
  %local_69 = alloca i8, align 1
  %local_70 = alloca ptr, align 8
  %local_71 = alloca ptr, align 8
  %local_72 = alloca i8, align 1
  %local_73 = alloca i64, align 8
  %local_74 = alloca i8, align 1
  %local_75 = alloca i64, align 8
  %local_76 = alloca i64, align 8
  %local_77 = alloca i8, align 1
  %local_78 = alloca i64, align 8
  %local_79 = alloca i8, align 1
  %local_80 = alloca ptr, align 8
  %local_81 = alloca ptr, align 8
  %local_82 = alloca i8, align 1
  %local_83 = alloca i64, align 8
  %local_84 = alloca i8, align 1
  %local_85 = alloca i64, align 8
  %local_86 = alloca i64, align 8
  %local_87 = alloca i8, align 1
  %local_88 = alloca i64, align 8
  %local_89 = alloca i8, align 1
  %local_90 = alloca ptr, align 8
  %local_91 = alloca ptr, align 8
  %local_92 = alloca i8, align 1
  %local_93 = alloca i64, align 8
  %local_94 = alloca i8, align 1
  %local_95 = alloca i64, align 8
  %local_96 = alloca i64, align 8
  %local_97 = alloca i8, align 1
  %local_98 = alloca i64, align 8
  %local_99 = alloca i8, align 1
  %local_100 = alloca ptr, align 8
  %local_101 = alloca ptr, align 8
  %local_102 = alloca i8, align 1
  %local_103 = alloca i64, align 8
  %local_104 = alloca i8, align 1
  %local_105 = alloca i64, align 8
  %local_106 = alloca i64, align 8
  %local_107 = alloca i8, align 1
  %local_108 = alloca i64, align 8
  %local_109 = alloca i8, align 1
  %local_110 = alloca ptr, align 8
  %local_111 = alloca ptr, align 8
  %local_112 = alloca i8, align 1
  %local_113 = alloca i64, align 8
  %local_114 = alloca i8, align 1
  %local_115 = alloca i64, align 8
  %local_116 = alloca i64, align 8
  %local_117 = alloca i8, align 1
  %local_118 = alloca i64, align 8
  %local_119 = alloca i8, align 1
  %local_120 = alloca ptr, align 8
  %local_121 = alloca ptr, align 8
  %local_122 = alloca i8, align 1
  %local_123 = alloca i64, align 8
  %local_124 = alloca i8, align 1
  %local_125 = alloca i64, align 8
  %local_126 = alloca i64, align 8
  %local_127 = alloca i8, align 1
  %local_128 = alloca i64, align 8
  %local_129 = alloca i8, align 1
  %local_130 = alloca ptr, align 8
  %local_131 = alloca ptr, align 8
  %local_132 = alloca i8, align 1
  %local_133 = alloca i64, align 8
  %local_134 = alloca i8, align 1
  %local_135 = alloca i8, align 1
  %local_136 = alloca i8, align 1
  %local_137 = alloca i64, align 8
  %local_138 = alloca i64, align 8
  %local_139 = alloca i8, align 1
  %local_140 = alloca ptr, align 8
  %local_141 = alloca ptr, align 8
  %local_142 = alloca i8, align 1
  %local_143 = alloca i64, align 8
  %local_144 = alloca i64, align 8
  %local_145 = alloca i8, align 1
  %local_146 = alloca ptr, align 8
  %local_147 = alloca ptr, align 8
  %local_148 = alloca i8, align 1
  %local_149 = alloca i64, align 8
  %local_150 = alloca i8, align 1
  %local_151 = alloca i8, align 1
  %local_152 = alloca i64, align 8
  %local_153 = alloca i64, align 8
  %local_154 = alloca i8, align 1
  %local_155 = alloca ptr, align 8
  %local_156 = alloca ptr, align 8
  %local_157 = alloca i8, align 1
  %local_158 = alloca i64, align 8
  %local_159 = alloca i64, align 8
  %local_160 = alloca i8, align 1
  %local_161 = alloca ptr, align 8
  %local_162 = alloca ptr, align 8
  %local_163 = alloca i8, align 1
  %local_164 = alloca i64, align 8
  %local_165 = alloca ptr, align 8
  %local_166 = alloca ptr, align 8
  %local_167 = alloca i64, align 8
  %local_168 = alloca i64, align 8
  %local_169 = alloca i64, align 8
  %local_170 = alloca i64, align 8
  %local_171 = alloca i64, align 8
  %local_172 = alloca i8, align 1
  %local_173 = alloca i64, align 8
  %local_174 = alloca i64, align 8
  %local_175 = alloca i8, align 1
  %local_176 = alloca ptr, align 8
  %local_177 = alloca i64, align 8
  %local_178 = alloca i64, align 8
  %local_179 = alloca i64, align 8
  %local_180 = alloca i8, align 1
  %local_181 = alloca i64, align 8
  %local_182 = alloca i8, align 1
  %local_183 = alloca i64, align 8
  %local_184 = alloca i8, align 1
  %local_185 = alloca i64, align 8
  %local_186 = alloca i8, align 1
  %local_187 = alloca ptr, align 8
  %local_188 = alloca i64, align 8
  %local_189 = alloca i8, align 1
  %local_190 = alloca ptr, align 8
  %local_191 = alloca ptr, align 8
  %local_192 = alloca ptr, align 8
  %local_193 = alloca i8, align 1
  %local_194 = alloca ptr, align 8
  %local_195 = alloca ptr, align 8
  %local_196 = alloca i64, align 8
  %local_197 = alloca i64, align 8
  %local_198 = alloca i64, align 8
  %local_199 = alloca i64, align 8
  %local_200 = alloca i64, align 8
  %local_201 = alloca i8, align 1
  %local_202 = alloca i64, align 8
  %local_203 = alloca i64, align 8
  %local_204 = alloca i8, align 1
  %local_205 = alloca i8, align 1
  %local_206 = alloca i64, align 8
  %local_207 = alloca i64, align 8
  %local_208 = alloca i8, align 1
  %local_209 = alloca i64, align 8
  %local_210 = alloca i8, align 1
  %local_211 = alloca ptr, align 8
  %local_212 = alloca i64, align 8
  %local_213 = alloca i8, align 1
  %local_214 = alloca ptr, align 8
  %local_215 = alloca i8, align 1
  %local_216 = alloca i64, align 8
  %local_217 = alloca i8, align 1
  %local_218 = alloca i64, align 8
  %local_219 = alloca i64, align 8
  %local_220 = alloca i64, align 8
  %local_221 = alloca i64, align 8
  %local_222 = alloca i64, align 8
  %local_223 = alloca i8, align 1
  %local_224 = alloca i64, align 8
  %local_225 = alloca i64, align 8
  %local_226 = alloca i8, align 1
  %local_227 = alloca ptr, align 8
  %local_228 = alloca i64, align 8
  %local_229 = alloca i64, align 8
  %local_230 = alloca i64, align 8
  %local_231 = alloca i8, align 1
  %local_232 = alloca i64, align 8
  %local_233 = alloca i8, align 1
  %local_234 = alloca i64, align 8
  %local_235 = alloca i8, align 1
  %local_236 = alloca i64, align 8
  %local_237 = alloca i8, align 1
  %local_238 = alloca ptr, align 8
  %local_239 = alloca i64, align 8
  %local_240 = alloca i8, align 1
  %local_241 = alloca ptr, align 8
  %local_242 = alloca ptr, align 8
  %local_243 = alloca ptr, align 8
  %local_244 = alloca i8, align 1
  %local_245 = alloca i64, align 8
  %local_246 = alloca i8, align 1
  %local_247 = alloca ptr, align 8
  %local_248 = alloca i8, align 1
  %local_249 = alloca i64, align 8
  %local_250 = alloca i8, align 1
  %local_251 = alloca i64, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %hew_vec_new_str_call = call ptr @hew_vec_new_str()
  store ptr %hew_vec_new_str_call, ptr %local_0, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_0, align 8
  store ptr %move_load, ptr %local_1, align 8
  store i64 1, ptr %local_4, align 8
  %move_load1 = load i64, ptr %local_4, align 8
  store i64 %move_load1, ptr %local_5, align 8
  store i64 0, ptr %local_6, align 8
  %cmp_lhs = load i64, ptr %local_5, align 8
  %cmp_rhs = load i64, ptr %local_6, align 8
  %cmp_bit = icmp sle i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_7, align 1
  %cond_load = load i8, ptr %local_7, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb6, label %bb7

bb2:                                              ; preds = %after_cooperate17, %bb4
  %cmp_lhs2 = load i64, ptr %local_2, align 8
  %cmp_rhs3 = load i64, ptr %local_3, align 8
  %cmp_bit4 = icmp slt i64 %cmp_lhs2, %cmp_rhs3
  %cmp_zext5 = zext i1 %cmp_bit4 to i8
  store i8 %cmp_zext5, ptr %local_10, align 1
  %cond_load6 = load i8, ptr %local_10, align 1
  %cond_nz7 = icmp ne i8 %cond_load6, 0
  br i1 %cond_nz7, label %bb3, label %bb5

bb3:                                              ; preds = %bb2
  %call_arg = load i64, ptr %local_2, align 8
  %call_result = call ptr @reverse_key(i64 %call_arg)
  store ptr %call_result, ptr %local_11, align 8
  br label %bb8

bb4:                                              ; preds = %after_cooperate24
  %checked_lhs = load i64, ptr %local_2, align 8
  %checked_rhs = load i64, ptr %local_5, align 8
  %with_overflow = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_2, align 8
  store i8 %checked_overflow_widen, ptr %local_12, align 1
  %cond_load8 = load i8, ptr %local_12, align 1
  %cond_nz9 = icmp ne i8 %cond_load8, 0
  br i1 %cond_nz9, label %bb10, label %bb2

bb5:                                              ; preds = %bb2
  %call_arg10 = load ptr, ptr %local_1, align 8
  %call_result11 = call { ptr, i64 } @"sort$sort_strings_counted"(ptr %call_arg10)
  store { ptr, i64 } %call_result11, ptr %local_13, align 8
  br label %bb11

bb6:                                              ; preds = %bb1
  %"hew_vec_free drop" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 202)
  call void @llvm.trap()
  unreachable

bb7:                                              ; preds = %bb1
  store i64 0, ptr %local_8, align 8
  store i64 256, ptr %local_9, align 8
  %move_load12 = load i64, ptr %local_8, align 8
  store i64 %move_load12, ptr %local_2, align 8
  %move_load13 = load i64, ptr %local_9, align 8
  store i64 %move_load13, ptr %local_3, align 8
  %hew_actor_cooperate14 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel15 = icmp eq i32 %hew_actor_cooperate14, 2
  br i1 %hew_cooperate_is_cancel15, label %cancel_exit16, label %after_cooperate17

bb8:                                              ; preds = %bb3
  %call_arg19 = load ptr, ptr %local_1, align 8
  %call_arg20 = load ptr, ptr %local_11, align 8
  call void @hew_vec_push_str(ptr %call_arg19, ptr %call_arg20)
  br label %bb9

bb9:                                              ; preds = %bb8
  %"hew_string_drop drop" = load ptr, ptr %local_11, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop")
  store ptr null, ptr %local_11, align 8
  %hew_actor_cooperate21 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel22 = icmp eq i32 %hew_actor_cooperate21, 2
  br i1 %hew_cooperate_is_cancel22, label %cancel_exit23, label %after_cooperate24

bb10:                                             ; preds = %bb4
  %"hew_vec_free drop26" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop26")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb11:                                             ; preds = %bb5
  %move_load27 = load { ptr, i64 }, ptr %local_13, align 8
  store { ptr, i64 } %move_load27, ptr %local_14, align 8
  %tuple_0_load_ptr = getelementptr inbounds nuw { ptr, i64 }, ptr %local_14, i32 0, i32 0
  %tuple_0_load = load ptr, ptr %tuple_0_load_ptr, align 8
  store ptr %tuple_0_load, ptr %local_15, align 8
  %move_load28 = load ptr, ptr %local_15, align 8
  store ptr %move_load28, ptr %local_16, align 8
  %tuple_1_load_ptr = getelementptr inbounds nuw { ptr, i64 }, ptr %local_14, i32 0, i32 1
  %tuple_1_load = load i64, ptr %tuple_1_load_ptr, align 8
  store i64 %tuple_1_load, ptr %local_17, align 8
  %move_load29 = load i64, ptr %local_17, align 8
  store i64 %move_load29, ptr %local_18, align 8
  store i64 256, ptr %local_20, align 8
  store i64 8, ptr %local_21, align 8
  %checked_lhs30 = load i64, ptr %local_20, align 8
  %checked_rhs31 = load i64, ptr %local_21, align 8
  %with_overflow32 = call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %checked_lhs30, i64 %checked_rhs31)
  %checked_result33 = extractvalue { i64, i1 } %with_overflow32, 0
  %checked_overflow34 = extractvalue { i64, i1 } %with_overflow32, 1
  %checked_overflow_widen35 = zext i1 %checked_overflow34 to i8
  store i64 %checked_result33, ptr %local_22, align 8
  store i8 %checked_overflow_widen35, ptr %local_23, align 1
  %cond_load36 = load i8, ptr %local_23, align 1
  %cond_nz37 = icmp ne i8 %cond_load36, 0
  br i1 %cond_nz37, label %bb12, label %bb13

bb12:                                             ; preds = %bb11
  %"hew_vec_free drop38" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop38")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb13:                                             ; preds = %bb11
  %cmp_lhs39 = load i64, ptr %local_18, align 8
  %cmp_rhs40 = load i64, ptr %local_22, align 8
  %cmp_bit41 = icmp sgt i64 %cmp_lhs39, %cmp_rhs40
  %cmp_zext42 = zext i1 %cmp_bit41 to i8
  store i8 %cmp_zext42, ptr %local_24, align 1
  %cond_load43 = load i8, ptr %local_24, align 1
  %cond_nz44 = icmp ne i8 %cond_load43, 0
  br i1 %cond_nz44, label %bb14, label %bb15

bb14:                                             ; preds = %bb13
  store i64 20, ptr %local_25, align 8
  %move_load45 = load i64, ptr %local_25, align 8
  store i64 %move_load45, ptr %return_slot, align 8
  %"hew_vec_free drop46" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop46")
  store ptr null, ptr %local_1, align 8
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val

bb15:                                             ; preds = %bb13
  br label %bb16

bb16:                                             ; preds = %after_cooperate57, %bb15
  store i64 1, ptr %local_28, align 8
  %move_load47 = load i64, ptr %local_28, align 8
  store i64 %move_load47, ptr %local_29, align 8
  store i64 0, ptr %local_30, align 8
  %cmp_lhs48 = load i64, ptr %local_29, align 8
  %cmp_rhs49 = load i64, ptr %local_30, align 8
  %cmp_bit50 = icmp sle i64 %cmp_lhs48, %cmp_rhs49
  %cmp_zext51 = zext i1 %cmp_bit50 to i8
  store i8 %cmp_zext51, ptr %local_31, align 1
  %cond_load52 = load i8, ptr %local_31, align 1
  %cond_nz53 = icmp ne i8 %cond_load52, 0
  br i1 %cond_nz53, label %bb22, label %bb23

bb17:                                             ; No predecessors!
  %hew_actor_cooperate54 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel55 = icmp eq i32 %hew_actor_cooperate54, 2
  br i1 %hew_cooperate_is_cancel55, label %cancel_exit56, label %after_cooperate57

bb18:                                             ; preds = %after_cooperate93, %bb20
  %cmp_lhs58 = load i64, ptr %local_26, align 8
  %cmp_rhs59 = load i64, ptr %local_27, align 8
  %cmp_bit60 = icmp slt i64 %cmp_lhs58, %cmp_rhs59
  %cmp_zext61 = zext i1 %cmp_bit60 to i8
  store i8 %cmp_zext61, ptr %local_34, align 1
  %cond_load62 = load i8, ptr %local_34, align 1
  %cond_nz63 = icmp ne i8 %cond_load62, 0
  br i1 %cond_nz63, label %bb19, label %bb21

bb19:                                             ; preds = %bb18
  store i64 1, ptr %local_36, align 8
  %checked_lhs64 = load i64, ptr %local_26, align 8
  %checked_rhs65 = load i64, ptr %local_36, align 8
  %with_overflow66 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %checked_lhs64, i64 %checked_rhs65)
  %checked_result67 = extractvalue { i64, i1 } %with_overflow66, 0
  %checked_overflow68 = extractvalue { i64, i1 } %with_overflow66, 1
  %checked_overflow_widen69 = zext i1 %checked_overflow68 to i8
  store i64 %checked_result67, ptr %local_37, align 8
  store i8 %checked_overflow_widen69, ptr %local_38, align 1
  %cond_load70 = load i8, ptr %local_38, align 1
  %cond_nz71 = icmp ne i8 %cond_load70, 0
  br i1 %cond_nz71, label %bb24, label %bb25

bb20:                                             ; preds = %after_cooperate124
  %checked_lhs72 = load i64, ptr %local_26, align 8
  %checked_rhs73 = load i64, ptr %local_29, align 8
  %with_overflow74 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs72, i64 %checked_rhs73)
  %checked_result75 = extractvalue { i64, i1 } %with_overflow74, 0
  %checked_overflow76 = extractvalue { i64, i1 } %with_overflow74, 1
  %checked_overflow_widen77 = zext i1 %checked_overflow76 to i8
  store i64 %checked_result75, ptr %local_26, align 8
  store i8 %checked_overflow_widen77, ptr %local_47, align 1
  %cond_load78 = load i8, ptr %local_47, align 1
  %cond_nz79 = icmp ne i8 %cond_load78, 0
  br i1 %cond_nz79, label %bb34, label %bb18

bb21:                                             ; preds = %bb18
  store i64 1, ptr %local_50, align 8
  %move_load80 = load i64, ptr %local_50, align 8
  store i64 %move_load80, ptr %local_51, align 8
  store i64 0, ptr %local_52, align 8
  %cmp_lhs81 = load i64, ptr %local_51, align 8
  %cmp_rhs82 = load i64, ptr %local_52, align 8
  %cmp_bit83 = icmp sle i64 %cmp_lhs81, %cmp_rhs82
  %cmp_zext84 = zext i1 %cmp_bit83 to i8
  store i8 %cmp_zext84, ptr %local_53, align 1
  %cond_load85 = load i8, ptr %local_53, align 1
  %cond_nz86 = icmp ne i8 %cond_load85, 0
  br i1 %cond_nz86, label %bb39, label %bb40

bb22:                                             ; preds = %bb16
  %"hew_vec_free drop87" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop87")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 202)
  call void @llvm.trap()
  unreachable

bb23:                                             ; preds = %bb16
  store i64 1, ptr %local_32, align 8
  store i64 256, ptr %local_33, align 8
  %move_load88 = load i64, ptr %local_32, align 8
  store i64 %move_load88, ptr %local_26, align 8
  %move_load89 = load i64, ptr %local_33, align 8
  store i64 %move_load89, ptr %local_27, align 8
  %hew_actor_cooperate90 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel91 = icmp eq i32 %hew_actor_cooperate90, 2
  br i1 %hew_cooperate_is_cancel91, label %cancel_exit92, label %after_cooperate93

bb24:                                             ; preds = %bb19
  %"hew_vec_free drop95" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop95")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb25:                                             ; preds = %bb19
  %"hew_vec_len arg0" = load ptr, ptr %local_16, align 8
  %hew_vec_len_call = call i64 @hew_vec_len(ptr %"hew_vec_len arg0")
  store i64 %hew_vec_len_call, ptr %local_39, align 8
  %cmp_lhs96 = load i64, ptr %local_37, align 8
  %cmp_rhs97 = load i64, ptr %local_39, align 8
  %cmp_bit98 = icmp uge i64 %cmp_lhs96, %cmp_rhs97
  %cmp_zext99 = zext i1 %cmp_bit98 to i8
  store i8 %cmp_zext99, ptr %local_40, align 1
  %cond_load100 = load i8, ptr %local_40, align 1
  %cond_nz101 = icmp ne i8 %cond_load100, 0
  br i1 %cond_nz101, label %bb26, label %bb27

bb26:                                             ; preds = %bb25
  %"hew_vec_free drop102" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop102")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb27:                                             ; preds = %bb25
  %"hew_vec_get_str arg0" = load ptr, ptr %local_16, align 8
  %"hew_vec_get_str arg1" = load i64, ptr %local_37, align 8
  %hew_vec_get_str_call = call ptr @hew_vec_get_str(ptr %"hew_vec_get_str arg0", i64 %"hew_vec_get_str arg1")
  store ptr %hew_vec_get_str_call, ptr %local_41, align 8
  %"hew_vec_len arg0103" = load ptr, ptr %local_16, align 8
  %hew_vec_len_call104 = call i64 @hew_vec_len(ptr %"hew_vec_len arg0103")
  store i64 %hew_vec_len_call104, ptr %local_42, align 8
  %cmp_lhs105 = load i64, ptr %local_26, align 8
  %cmp_rhs106 = load i64, ptr %local_42, align 8
  %cmp_bit107 = icmp uge i64 %cmp_lhs105, %cmp_rhs106
  %cmp_zext108 = zext i1 %cmp_bit107 to i8
  store i8 %cmp_zext108, ptr %local_43, align 1
  %cond_load109 = load i8, ptr %local_43, align 1
  %cond_nz110 = icmp ne i8 %cond_load109, 0
  br i1 %cond_nz110, label %bb28, label %bb29

bb28:                                             ; preds = %bb27
  %"hew_vec_free drop111" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop111")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb29:                                             ; preds = %bb27
  %"hew_vec_get_str arg0112" = load ptr, ptr %local_16, align 8
  %"hew_vec_get_str arg1113" = load i64, ptr %local_26, align 8
  %hew_vec_get_str_call114 = call ptr @hew_vec_get_str(ptr %"hew_vec_get_str arg0112", i64 %"hew_vec_get_str arg1113")
  store ptr %hew_vec_get_str_call114, ptr %local_44, align 8
  %string_ord_lhs = load ptr, ptr %local_41, align 8
  %string_ord_rhs = load ptr, ptr %local_44, align 8
  %hew_string_compare = call i32 @hew_string_compare(ptr %string_ord_lhs, ptr %string_ord_rhs)
  %string_ord_bit = icmp sgt i32 %hew_string_compare, 0
  %string_ord_zext = zext i1 %string_ord_bit to i8
  store i8 %string_ord_zext, ptr %local_45, align 1
  %"hew_string_drop drop115" = load ptr, ptr %local_44, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop115")
  store ptr null, ptr %local_44, align 8
  %cond_load116 = load i8, ptr %local_45, align 1
  %cond_nz117 = icmp ne i8 %cond_load116, 0
  br i1 %cond_nz117, label %bb30, label %bb31

bb30:                                             ; preds = %bb29
  store i64 21, ptr %local_46, align 8
  %move_load118 = load i64, ptr %local_46, align 8
  store i64 %move_load118, ptr %return_slot, align 8
  %"hew_vec_free drop119" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop119")
  store ptr null, ptr %local_1, align 8
  %ret_val120 = load i64, ptr %return_slot, align 8
  ret i64 %ret_val120

bb31:                                             ; preds = %bb29
  br label %bb32

bb32:                                             ; preds = %after_cooperate129, %bb31
  %hew_actor_cooperate121 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel122 = icmp eq i32 %hew_actor_cooperate121, 2
  br i1 %hew_cooperate_is_cancel122, label %cancel_exit123, label %after_cooperate124

bb33:                                             ; No predecessors!
  %hew_actor_cooperate126 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel127 = icmp eq i32 %hew_actor_cooperate126, 2
  br i1 %hew_cooperate_is_cancel127, label %cancel_exit128, label %after_cooperate129

bb34:                                             ; preds = %bb20
  %"hew_vec_free drop130" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop130")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb35:                                             ; preds = %after_cooperate167, %bb37
  %cmp_lhs131 = load i64, ptr %local_48, align 8
  %cmp_rhs132 = load i64, ptr %local_49, align 8
  %cmp_bit133 = icmp slt i64 %cmp_lhs131, %cmp_rhs132
  %cmp_zext134 = zext i1 %cmp_bit133 to i8
  store i8 %cmp_zext134, ptr %local_56, align 1
  %cond_load135 = load i8, ptr %local_56, align 1
  %cond_nz136 = icmp ne i8 %cond_load135, 0
  br i1 %cond_nz136, label %bb36, label %bb38

bb36:                                             ; preds = %bb35
  %"hew_vec_len arg0137" = load ptr, ptr %local_16, align 8
  %hew_vec_len_call138 = call i64 @hew_vec_len(ptr %"hew_vec_len arg0137")
  store i64 %hew_vec_len_call138, ptr %local_58, align 8
  %cmp_lhs139 = load i64, ptr %local_48, align 8
  %cmp_rhs140 = load i64, ptr %local_58, align 8
  %cmp_bit141 = icmp uge i64 %cmp_lhs139, %cmp_rhs140
  %cmp_zext142 = zext i1 %cmp_bit141 to i8
  store i8 %cmp_zext142, ptr %local_59, align 1
  %cond_load143 = load i8, ptr %local_59, align 1
  %cond_nz144 = icmp ne i8 %cond_load143, 0
  br i1 %cond_nz144, label %bb41, label %bb42

bb37:                                             ; preds = %after_cooperate434
  %checked_lhs145 = load i64, ptr %local_48, align 8
  %checked_rhs146 = load i64, ptr %local_51, align 8
  %with_overflow147 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs145, i64 %checked_rhs146)
  %checked_result148 = extractvalue { i64, i1 } %with_overflow147, 0
  %checked_overflow149 = extractvalue { i64, i1 } %with_overflow147, 1
  %checked_overflow_widen150 = zext i1 %checked_overflow149 to i8
  store i64 %checked_result148, ptr %local_48, align 8
  store i8 %checked_overflow_widen150, ptr %local_134, align 1
  %cond_load151 = load i8, ptr %local_134, align 1
  %cond_nz152 = icmp ne i8 %cond_load151, 0
  br i1 %cond_nz152, label %bb103, label %bb35

bb38:                                             ; preds = %bb35
  store i8 1, ptr %local_136, align 1
  store i64 0, ptr %local_137, align 8
  %"hew_vec_len arg0153" = load ptr, ptr %local_1, align 8
  %hew_vec_len_call154 = call i64 @hew_vec_len(ptr %"hew_vec_len arg0153")
  store i64 %hew_vec_len_call154, ptr %local_138, align 8
  %cmp_lhs155 = load i64, ptr %local_137, align 8
  %cmp_rhs156 = load i64, ptr %local_138, align 8
  %cmp_bit157 = icmp uge i64 %cmp_lhs155, %cmp_rhs156
  %cmp_zext158 = zext i1 %cmp_bit157 to i8
  store i8 %cmp_zext158, ptr %local_139, align 1
  %cond_load159 = load i8, ptr %local_139, align 1
  %cond_nz160 = icmp ne i8 %cond_load159, 0
  br i1 %cond_nz160, label %bb104, label %bb105

bb39:                                             ; preds = %bb21
  %"hew_vec_free drop161" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop161")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 202)
  call void @llvm.trap()
  unreachable

bb40:                                             ; preds = %bb21
  store i64 0, ptr %local_54, align 8
  store i64 32, ptr %local_55, align 8
  %move_load162 = load i64, ptr %local_54, align 8
  store i64 %move_load162, ptr %local_48, align 8
  %move_load163 = load i64, ptr %local_55, align 8
  store i64 %move_load163, ptr %local_49, align 8
  %hew_actor_cooperate164 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel165 = icmp eq i32 %hew_actor_cooperate164, 2
  br i1 %hew_cooperate_is_cancel165, label %cancel_exit166, label %after_cooperate167

bb41:                                             ; preds = %bb36
  %"hew_vec_free drop169" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop169")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb42:                                             ; preds = %bb36
  %"hew_vec_get_str arg0170" = load ptr, ptr %local_16, align 8
  %"hew_vec_get_str arg1171" = load i64, ptr %local_48, align 8
  %hew_vec_get_str_call172 = call ptr @hew_vec_get_str(ptr %"hew_vec_get_str arg0170", i64 %"hew_vec_get_str arg1171")
  store ptr %hew_vec_get_str_call172, ptr %local_60, align 8
  store ptr @str_lit.8, ptr %local_61, align 8
  %string_cmp_lhs = load ptr, ptr %local_60, align 8
  %string_cmp_rhs = load ptr, ptr %local_61, align 8
  %hew_string_equals = call i32 @hew_string_equals(ptr %string_cmp_lhs, ptr %string_cmp_rhs)
  %string_cmp_bit = icmp eq i32 %hew_string_equals, 0
  %string_cmp_zext = zext i1 %string_cmp_bit to i8
  store i8 %string_cmp_zext, ptr %local_62, align 1
  %"hew_string_drop drop173" = load ptr, ptr %local_60, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop173")
  store ptr null, ptr %local_60, align 8
  %cond_load174 = load i8, ptr %local_62, align 1
  %cond_nz175 = icmp ne i8 %cond_load174, 0
  br i1 %cond_nz175, label %bb43, label %bb44

bb43:                                             ; preds = %bb42
  store i64 22, ptr %local_63, align 8
  %move_load176 = load i64, ptr %local_63, align 8
  store i64 %move_load176, ptr %return_slot, align 8
  %"hew_vec_free drop177" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop177")
  store ptr null, ptr %local_1, align 8
  %ret_val178 = load i64, ptr %return_slot, align 8
  ret i64 %ret_val178

bb44:                                             ; preds = %bb42
  br label %bb45

bb45:                                             ; preds = %after_cooperate190, %bb44
  store i64 32, ptr %local_65, align 8
  %checked_lhs179 = load i64, ptr %local_65, align 8
  %checked_rhs180 = load i64, ptr %local_48, align 8
  %with_overflow181 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs179, i64 %checked_rhs180)
  %checked_result182 = extractvalue { i64, i1 } %with_overflow181, 0
  %checked_overflow183 = extractvalue { i64, i1 } %with_overflow181, 1
  %checked_overflow_widen184 = zext i1 %checked_overflow183 to i8
  store i64 %checked_result182, ptr %local_66, align 8
  store i8 %checked_overflow_widen184, ptr %local_67, align 1
  %cond_load185 = load i8, ptr %local_67, align 1
  %cond_nz186 = icmp ne i8 %cond_load185, 0
  br i1 %cond_nz186, label %bb47, label %bb48

bb46:                                             ; No predecessors!
  %hew_actor_cooperate187 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel188 = icmp eq i32 %hew_actor_cooperate187, 2
  br i1 %hew_cooperate_is_cancel188, label %cancel_exit189, label %after_cooperate190

bb47:                                             ; preds = %bb45
  %"hew_vec_free drop191" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop191")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb48:                                             ; preds = %bb45
  %"hew_vec_len arg0192" = load ptr, ptr %local_16, align 8
  %hew_vec_len_call193 = call i64 @hew_vec_len(ptr %"hew_vec_len arg0192")
  store i64 %hew_vec_len_call193, ptr %local_68, align 8
  %cmp_lhs194 = load i64, ptr %local_66, align 8
  %cmp_rhs195 = load i64, ptr %local_68, align 8
  %cmp_bit196 = icmp uge i64 %cmp_lhs194, %cmp_rhs195
  %cmp_zext197 = zext i1 %cmp_bit196 to i8
  store i8 %cmp_zext197, ptr %local_69, align 1
  %cond_load198 = load i8, ptr %local_69, align 1
  %cond_nz199 = icmp ne i8 %cond_load198, 0
  br i1 %cond_nz199, label %bb49, label %bb50

bb49:                                             ; preds = %bb48
  %"hew_vec_free drop200" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop200")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb50:                                             ; preds = %bb48
  %"hew_vec_get_str arg0201" = load ptr, ptr %local_16, align 8
  %"hew_vec_get_str arg1202" = load i64, ptr %local_66, align 8
  %hew_vec_get_str_call203 = call ptr @hew_vec_get_str(ptr %"hew_vec_get_str arg0201", i64 %"hew_vec_get_str arg1202")
  store ptr %hew_vec_get_str_call203, ptr %local_70, align 8
  store ptr @str_lit.9, ptr %local_71, align 8
  %string_cmp_lhs204 = load ptr, ptr %local_70, align 8
  %string_cmp_rhs205 = load ptr, ptr %local_71, align 8
  %hew_string_equals206 = call i32 @hew_string_equals(ptr %string_cmp_lhs204, ptr %string_cmp_rhs205)
  %string_cmp_bit207 = icmp eq i32 %hew_string_equals206, 0
  %string_cmp_zext208 = zext i1 %string_cmp_bit207 to i8
  store i8 %string_cmp_zext208, ptr %local_72, align 1
  %"hew_string_drop drop209" = load ptr, ptr %local_70, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop209")
  store ptr null, ptr %local_70, align 8
  %cond_load210 = load i8, ptr %local_72, align 1
  %cond_nz211 = icmp ne i8 %cond_load210, 0
  br i1 %cond_nz211, label %bb51, label %bb52

bb51:                                             ; preds = %bb50
  store i64 23, ptr %local_73, align 8
  %move_load212 = load i64, ptr %local_73, align 8
  store i64 %move_load212, ptr %return_slot, align 8
  %"hew_vec_free drop213" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop213")
  store ptr null, ptr %local_1, align 8
  %ret_val214 = load i64, ptr %return_slot, align 8
  ret i64 %ret_val214

bb52:                                             ; preds = %bb50
  br label %bb53

bb53:                                             ; preds = %after_cooperate226, %bb52
  store i64 64, ptr %local_75, align 8
  %checked_lhs215 = load i64, ptr %local_75, align 8
  %checked_rhs216 = load i64, ptr %local_48, align 8
  %with_overflow217 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs215, i64 %checked_rhs216)
  %checked_result218 = extractvalue { i64, i1 } %with_overflow217, 0
  %checked_overflow219 = extractvalue { i64, i1 } %with_overflow217, 1
  %checked_overflow_widen220 = zext i1 %checked_overflow219 to i8
  store i64 %checked_result218, ptr %local_76, align 8
  store i8 %checked_overflow_widen220, ptr %local_77, align 1
  %cond_load221 = load i8, ptr %local_77, align 1
  %cond_nz222 = icmp ne i8 %cond_load221, 0
  br i1 %cond_nz222, label %bb55, label %bb56

bb54:                                             ; No predecessors!
  %hew_actor_cooperate223 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel224 = icmp eq i32 %hew_actor_cooperate223, 2
  br i1 %hew_cooperate_is_cancel224, label %cancel_exit225, label %after_cooperate226

bb55:                                             ; preds = %bb53
  %"hew_vec_free drop227" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop227")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb56:                                             ; preds = %bb53
  %"hew_vec_len arg0228" = load ptr, ptr %local_16, align 8
  %hew_vec_len_call229 = call i64 @hew_vec_len(ptr %"hew_vec_len arg0228")
  store i64 %hew_vec_len_call229, ptr %local_78, align 8
  %cmp_lhs230 = load i64, ptr %local_76, align 8
  %cmp_rhs231 = load i64, ptr %local_78, align 8
  %cmp_bit232 = icmp uge i64 %cmp_lhs230, %cmp_rhs231
  %cmp_zext233 = zext i1 %cmp_bit232 to i8
  store i8 %cmp_zext233, ptr %local_79, align 1
  %cond_load234 = load i8, ptr %local_79, align 1
  %cond_nz235 = icmp ne i8 %cond_load234, 0
  br i1 %cond_nz235, label %bb57, label %bb58

bb57:                                             ; preds = %bb56
  %"hew_vec_free drop236" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop236")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb58:                                             ; preds = %bb56
  %"hew_vec_get_str arg0237" = load ptr, ptr %local_16, align 8
  %"hew_vec_get_str arg1238" = load i64, ptr %local_76, align 8
  %hew_vec_get_str_call239 = call ptr @hew_vec_get_str(ptr %"hew_vec_get_str arg0237", i64 %"hew_vec_get_str arg1238")
  store ptr %hew_vec_get_str_call239, ptr %local_80, align 8
  store ptr @str_lit.10, ptr %local_81, align 8
  %string_cmp_lhs240 = load ptr, ptr %local_80, align 8
  %string_cmp_rhs241 = load ptr, ptr %local_81, align 8
  %hew_string_equals242 = call i32 @hew_string_equals(ptr %string_cmp_lhs240, ptr %string_cmp_rhs241)
  %string_cmp_bit243 = icmp eq i32 %hew_string_equals242, 0
  %string_cmp_zext244 = zext i1 %string_cmp_bit243 to i8
  store i8 %string_cmp_zext244, ptr %local_82, align 1
  %"hew_string_drop drop245" = load ptr, ptr %local_80, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop245")
  store ptr null, ptr %local_80, align 8
  %cond_load246 = load i8, ptr %local_82, align 1
  %cond_nz247 = icmp ne i8 %cond_load246, 0
  br i1 %cond_nz247, label %bb59, label %bb60

bb59:                                             ; preds = %bb58
  store i64 24, ptr %local_83, align 8
  %move_load248 = load i64, ptr %local_83, align 8
  store i64 %move_load248, ptr %return_slot, align 8
  %"hew_vec_free drop249" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop249")
  store ptr null, ptr %local_1, align 8
  %ret_val250 = load i64, ptr %return_slot, align 8
  ret i64 %ret_val250

bb60:                                             ; preds = %bb58
  br label %bb61

bb61:                                             ; preds = %after_cooperate262, %bb60
  store i64 96, ptr %local_85, align 8
  %checked_lhs251 = load i64, ptr %local_85, align 8
  %checked_rhs252 = load i64, ptr %local_48, align 8
  %with_overflow253 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs251, i64 %checked_rhs252)
  %checked_result254 = extractvalue { i64, i1 } %with_overflow253, 0
  %checked_overflow255 = extractvalue { i64, i1 } %with_overflow253, 1
  %checked_overflow_widen256 = zext i1 %checked_overflow255 to i8
  store i64 %checked_result254, ptr %local_86, align 8
  store i8 %checked_overflow_widen256, ptr %local_87, align 1
  %cond_load257 = load i8, ptr %local_87, align 1
  %cond_nz258 = icmp ne i8 %cond_load257, 0
  br i1 %cond_nz258, label %bb63, label %bb64

bb62:                                             ; No predecessors!
  %hew_actor_cooperate259 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel260 = icmp eq i32 %hew_actor_cooperate259, 2
  br i1 %hew_cooperate_is_cancel260, label %cancel_exit261, label %after_cooperate262

bb63:                                             ; preds = %bb61
  %"hew_vec_free drop263" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop263")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb64:                                             ; preds = %bb61
  %"hew_vec_len arg0264" = load ptr, ptr %local_16, align 8
  %hew_vec_len_call265 = call i64 @hew_vec_len(ptr %"hew_vec_len arg0264")
  store i64 %hew_vec_len_call265, ptr %local_88, align 8
  %cmp_lhs266 = load i64, ptr %local_86, align 8
  %cmp_rhs267 = load i64, ptr %local_88, align 8
  %cmp_bit268 = icmp uge i64 %cmp_lhs266, %cmp_rhs267
  %cmp_zext269 = zext i1 %cmp_bit268 to i8
  store i8 %cmp_zext269, ptr %local_89, align 1
  %cond_load270 = load i8, ptr %local_89, align 1
  %cond_nz271 = icmp ne i8 %cond_load270, 0
  br i1 %cond_nz271, label %bb65, label %bb66

bb65:                                             ; preds = %bb64
  %"hew_vec_free drop272" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop272")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb66:                                             ; preds = %bb64
  %"hew_vec_get_str arg0273" = load ptr, ptr %local_16, align 8
  %"hew_vec_get_str arg1274" = load i64, ptr %local_86, align 8
  %hew_vec_get_str_call275 = call ptr @hew_vec_get_str(ptr %"hew_vec_get_str arg0273", i64 %"hew_vec_get_str arg1274")
  store ptr %hew_vec_get_str_call275, ptr %local_90, align 8
  store ptr @str_lit.11, ptr %local_91, align 8
  %string_cmp_lhs276 = load ptr, ptr %local_90, align 8
  %string_cmp_rhs277 = load ptr, ptr %local_91, align 8
  %hew_string_equals278 = call i32 @hew_string_equals(ptr %string_cmp_lhs276, ptr %string_cmp_rhs277)
  %string_cmp_bit279 = icmp eq i32 %hew_string_equals278, 0
  %string_cmp_zext280 = zext i1 %string_cmp_bit279 to i8
  store i8 %string_cmp_zext280, ptr %local_92, align 1
  %"hew_string_drop drop281" = load ptr, ptr %local_90, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop281")
  store ptr null, ptr %local_90, align 8
  %cond_load282 = load i8, ptr %local_92, align 1
  %cond_nz283 = icmp ne i8 %cond_load282, 0
  br i1 %cond_nz283, label %bb67, label %bb68

bb67:                                             ; preds = %bb66
  store i64 25, ptr %local_93, align 8
  %move_load284 = load i64, ptr %local_93, align 8
  store i64 %move_load284, ptr %return_slot, align 8
  %"hew_vec_free drop285" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop285")
  store ptr null, ptr %local_1, align 8
  %ret_val286 = load i64, ptr %return_slot, align 8
  ret i64 %ret_val286

bb68:                                             ; preds = %bb66
  br label %bb69

bb69:                                             ; preds = %after_cooperate298, %bb68
  store i64 128, ptr %local_95, align 8
  %checked_lhs287 = load i64, ptr %local_95, align 8
  %checked_rhs288 = load i64, ptr %local_48, align 8
  %with_overflow289 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs287, i64 %checked_rhs288)
  %checked_result290 = extractvalue { i64, i1 } %with_overflow289, 0
  %checked_overflow291 = extractvalue { i64, i1 } %with_overflow289, 1
  %checked_overflow_widen292 = zext i1 %checked_overflow291 to i8
  store i64 %checked_result290, ptr %local_96, align 8
  store i8 %checked_overflow_widen292, ptr %local_97, align 1
  %cond_load293 = load i8, ptr %local_97, align 1
  %cond_nz294 = icmp ne i8 %cond_load293, 0
  br i1 %cond_nz294, label %bb71, label %bb72

bb70:                                             ; No predecessors!
  %hew_actor_cooperate295 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel296 = icmp eq i32 %hew_actor_cooperate295, 2
  br i1 %hew_cooperate_is_cancel296, label %cancel_exit297, label %after_cooperate298

bb71:                                             ; preds = %bb69
  %"hew_vec_free drop299" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop299")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb72:                                             ; preds = %bb69
  %"hew_vec_len arg0300" = load ptr, ptr %local_16, align 8
  %hew_vec_len_call301 = call i64 @hew_vec_len(ptr %"hew_vec_len arg0300")
  store i64 %hew_vec_len_call301, ptr %local_98, align 8
  %cmp_lhs302 = load i64, ptr %local_96, align 8
  %cmp_rhs303 = load i64, ptr %local_98, align 8
  %cmp_bit304 = icmp uge i64 %cmp_lhs302, %cmp_rhs303
  %cmp_zext305 = zext i1 %cmp_bit304 to i8
  store i8 %cmp_zext305, ptr %local_99, align 1
  %cond_load306 = load i8, ptr %local_99, align 1
  %cond_nz307 = icmp ne i8 %cond_load306, 0
  br i1 %cond_nz307, label %bb73, label %bb74

bb73:                                             ; preds = %bb72
  %"hew_vec_free drop308" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop308")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb74:                                             ; preds = %bb72
  %"hew_vec_get_str arg0309" = load ptr, ptr %local_16, align 8
  %"hew_vec_get_str arg1310" = load i64, ptr %local_96, align 8
  %hew_vec_get_str_call311 = call ptr @hew_vec_get_str(ptr %"hew_vec_get_str arg0309", i64 %"hew_vec_get_str arg1310")
  store ptr %hew_vec_get_str_call311, ptr %local_100, align 8
  store ptr @str_lit.12, ptr %local_101, align 8
  %string_cmp_lhs312 = load ptr, ptr %local_100, align 8
  %string_cmp_rhs313 = load ptr, ptr %local_101, align 8
  %hew_string_equals314 = call i32 @hew_string_equals(ptr %string_cmp_lhs312, ptr %string_cmp_rhs313)
  %string_cmp_bit315 = icmp eq i32 %hew_string_equals314, 0
  %string_cmp_zext316 = zext i1 %string_cmp_bit315 to i8
  store i8 %string_cmp_zext316, ptr %local_102, align 1
  %"hew_string_drop drop317" = load ptr, ptr %local_100, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop317")
  store ptr null, ptr %local_100, align 8
  %cond_load318 = load i8, ptr %local_102, align 1
  %cond_nz319 = icmp ne i8 %cond_load318, 0
  br i1 %cond_nz319, label %bb75, label %bb76

bb75:                                             ; preds = %bb74
  store i64 26, ptr %local_103, align 8
  %move_load320 = load i64, ptr %local_103, align 8
  store i64 %move_load320, ptr %return_slot, align 8
  %"hew_vec_free drop321" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop321")
  store ptr null, ptr %local_1, align 8
  %ret_val322 = load i64, ptr %return_slot, align 8
  ret i64 %ret_val322

bb76:                                             ; preds = %bb74
  br label %bb77

bb77:                                             ; preds = %after_cooperate334, %bb76
  store i64 160, ptr %local_105, align 8
  %checked_lhs323 = load i64, ptr %local_105, align 8
  %checked_rhs324 = load i64, ptr %local_48, align 8
  %with_overflow325 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs323, i64 %checked_rhs324)
  %checked_result326 = extractvalue { i64, i1 } %with_overflow325, 0
  %checked_overflow327 = extractvalue { i64, i1 } %with_overflow325, 1
  %checked_overflow_widen328 = zext i1 %checked_overflow327 to i8
  store i64 %checked_result326, ptr %local_106, align 8
  store i8 %checked_overflow_widen328, ptr %local_107, align 1
  %cond_load329 = load i8, ptr %local_107, align 1
  %cond_nz330 = icmp ne i8 %cond_load329, 0
  br i1 %cond_nz330, label %bb79, label %bb80

bb78:                                             ; No predecessors!
  %hew_actor_cooperate331 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel332 = icmp eq i32 %hew_actor_cooperate331, 2
  br i1 %hew_cooperate_is_cancel332, label %cancel_exit333, label %after_cooperate334

bb79:                                             ; preds = %bb77
  %"hew_vec_free drop335" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop335")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb80:                                             ; preds = %bb77
  %"hew_vec_len arg0336" = load ptr, ptr %local_16, align 8
  %hew_vec_len_call337 = call i64 @hew_vec_len(ptr %"hew_vec_len arg0336")
  store i64 %hew_vec_len_call337, ptr %local_108, align 8
  %cmp_lhs338 = load i64, ptr %local_106, align 8
  %cmp_rhs339 = load i64, ptr %local_108, align 8
  %cmp_bit340 = icmp uge i64 %cmp_lhs338, %cmp_rhs339
  %cmp_zext341 = zext i1 %cmp_bit340 to i8
  store i8 %cmp_zext341, ptr %local_109, align 1
  %cond_load342 = load i8, ptr %local_109, align 1
  %cond_nz343 = icmp ne i8 %cond_load342, 0
  br i1 %cond_nz343, label %bb81, label %bb82

bb81:                                             ; preds = %bb80
  %"hew_vec_free drop344" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop344")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb82:                                             ; preds = %bb80
  %"hew_vec_get_str arg0345" = load ptr, ptr %local_16, align 8
  %"hew_vec_get_str arg1346" = load i64, ptr %local_106, align 8
  %hew_vec_get_str_call347 = call ptr @hew_vec_get_str(ptr %"hew_vec_get_str arg0345", i64 %"hew_vec_get_str arg1346")
  store ptr %hew_vec_get_str_call347, ptr %local_110, align 8
  store ptr @str_lit.13, ptr %local_111, align 8
  %string_cmp_lhs348 = load ptr, ptr %local_110, align 8
  %string_cmp_rhs349 = load ptr, ptr %local_111, align 8
  %hew_string_equals350 = call i32 @hew_string_equals(ptr %string_cmp_lhs348, ptr %string_cmp_rhs349)
  %string_cmp_bit351 = icmp eq i32 %hew_string_equals350, 0
  %string_cmp_zext352 = zext i1 %string_cmp_bit351 to i8
  store i8 %string_cmp_zext352, ptr %local_112, align 1
  %"hew_string_drop drop353" = load ptr, ptr %local_110, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop353")
  store ptr null, ptr %local_110, align 8
  %cond_load354 = load i8, ptr %local_112, align 1
  %cond_nz355 = icmp ne i8 %cond_load354, 0
  br i1 %cond_nz355, label %bb83, label %bb84

bb83:                                             ; preds = %bb82
  store i64 27, ptr %local_113, align 8
  %move_load356 = load i64, ptr %local_113, align 8
  store i64 %move_load356, ptr %return_slot, align 8
  %"hew_vec_free drop357" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop357")
  store ptr null, ptr %local_1, align 8
  %ret_val358 = load i64, ptr %return_slot, align 8
  ret i64 %ret_val358

bb84:                                             ; preds = %bb82
  br label %bb85

bb85:                                             ; preds = %after_cooperate370, %bb84
  store i64 192, ptr %local_115, align 8
  %checked_lhs359 = load i64, ptr %local_115, align 8
  %checked_rhs360 = load i64, ptr %local_48, align 8
  %with_overflow361 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs359, i64 %checked_rhs360)
  %checked_result362 = extractvalue { i64, i1 } %with_overflow361, 0
  %checked_overflow363 = extractvalue { i64, i1 } %with_overflow361, 1
  %checked_overflow_widen364 = zext i1 %checked_overflow363 to i8
  store i64 %checked_result362, ptr %local_116, align 8
  store i8 %checked_overflow_widen364, ptr %local_117, align 1
  %cond_load365 = load i8, ptr %local_117, align 1
  %cond_nz366 = icmp ne i8 %cond_load365, 0
  br i1 %cond_nz366, label %bb87, label %bb88

bb86:                                             ; No predecessors!
  %hew_actor_cooperate367 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel368 = icmp eq i32 %hew_actor_cooperate367, 2
  br i1 %hew_cooperate_is_cancel368, label %cancel_exit369, label %after_cooperate370

bb87:                                             ; preds = %bb85
  %"hew_vec_free drop371" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop371")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb88:                                             ; preds = %bb85
  %"hew_vec_len arg0372" = load ptr, ptr %local_16, align 8
  %hew_vec_len_call373 = call i64 @hew_vec_len(ptr %"hew_vec_len arg0372")
  store i64 %hew_vec_len_call373, ptr %local_118, align 8
  %cmp_lhs374 = load i64, ptr %local_116, align 8
  %cmp_rhs375 = load i64, ptr %local_118, align 8
  %cmp_bit376 = icmp uge i64 %cmp_lhs374, %cmp_rhs375
  %cmp_zext377 = zext i1 %cmp_bit376 to i8
  store i8 %cmp_zext377, ptr %local_119, align 1
  %cond_load378 = load i8, ptr %local_119, align 1
  %cond_nz379 = icmp ne i8 %cond_load378, 0
  br i1 %cond_nz379, label %bb89, label %bb90

bb89:                                             ; preds = %bb88
  %"hew_vec_free drop380" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop380")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb90:                                             ; preds = %bb88
  %"hew_vec_get_str arg0381" = load ptr, ptr %local_16, align 8
  %"hew_vec_get_str arg1382" = load i64, ptr %local_116, align 8
  %hew_vec_get_str_call383 = call ptr @hew_vec_get_str(ptr %"hew_vec_get_str arg0381", i64 %"hew_vec_get_str arg1382")
  store ptr %hew_vec_get_str_call383, ptr %local_120, align 8
  store ptr @str_lit.14, ptr %local_121, align 8
  %string_cmp_lhs384 = load ptr, ptr %local_120, align 8
  %string_cmp_rhs385 = load ptr, ptr %local_121, align 8
  %hew_string_equals386 = call i32 @hew_string_equals(ptr %string_cmp_lhs384, ptr %string_cmp_rhs385)
  %string_cmp_bit387 = icmp eq i32 %hew_string_equals386, 0
  %string_cmp_zext388 = zext i1 %string_cmp_bit387 to i8
  store i8 %string_cmp_zext388, ptr %local_122, align 1
  %"hew_string_drop drop389" = load ptr, ptr %local_120, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop389")
  store ptr null, ptr %local_120, align 8
  %cond_load390 = load i8, ptr %local_122, align 1
  %cond_nz391 = icmp ne i8 %cond_load390, 0
  br i1 %cond_nz391, label %bb91, label %bb92

bb91:                                             ; preds = %bb90
  store i64 28, ptr %local_123, align 8
  %move_load392 = load i64, ptr %local_123, align 8
  store i64 %move_load392, ptr %return_slot, align 8
  %"hew_vec_free drop393" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop393")
  store ptr null, ptr %local_1, align 8
  %ret_val394 = load i64, ptr %return_slot, align 8
  ret i64 %ret_val394

bb92:                                             ; preds = %bb90
  br label %bb93

bb93:                                             ; preds = %after_cooperate406, %bb92
  store i64 224, ptr %local_125, align 8
  %checked_lhs395 = load i64, ptr %local_125, align 8
  %checked_rhs396 = load i64, ptr %local_48, align 8
  %with_overflow397 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs395, i64 %checked_rhs396)
  %checked_result398 = extractvalue { i64, i1 } %with_overflow397, 0
  %checked_overflow399 = extractvalue { i64, i1 } %with_overflow397, 1
  %checked_overflow_widen400 = zext i1 %checked_overflow399 to i8
  store i64 %checked_result398, ptr %local_126, align 8
  store i8 %checked_overflow_widen400, ptr %local_127, align 1
  %cond_load401 = load i8, ptr %local_127, align 1
  %cond_nz402 = icmp ne i8 %cond_load401, 0
  br i1 %cond_nz402, label %bb95, label %bb96

bb94:                                             ; No predecessors!
  %hew_actor_cooperate403 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel404 = icmp eq i32 %hew_actor_cooperate403, 2
  br i1 %hew_cooperate_is_cancel404, label %cancel_exit405, label %after_cooperate406

bb95:                                             ; preds = %bb93
  %"hew_vec_free drop407" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop407")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb96:                                             ; preds = %bb93
  %"hew_vec_len arg0408" = load ptr, ptr %local_16, align 8
  %hew_vec_len_call409 = call i64 @hew_vec_len(ptr %"hew_vec_len arg0408")
  store i64 %hew_vec_len_call409, ptr %local_128, align 8
  %cmp_lhs410 = load i64, ptr %local_126, align 8
  %cmp_rhs411 = load i64, ptr %local_128, align 8
  %cmp_bit412 = icmp uge i64 %cmp_lhs410, %cmp_rhs411
  %cmp_zext413 = zext i1 %cmp_bit412 to i8
  store i8 %cmp_zext413, ptr %local_129, align 1
  %cond_load414 = load i8, ptr %local_129, align 1
  %cond_nz415 = icmp ne i8 %cond_load414, 0
  br i1 %cond_nz415, label %bb97, label %bb98

bb97:                                             ; preds = %bb96
  %"hew_vec_free drop416" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop416")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb98:                                             ; preds = %bb96
  %"hew_vec_get_str arg0417" = load ptr, ptr %local_16, align 8
  %"hew_vec_get_str arg1418" = load i64, ptr %local_126, align 8
  %hew_vec_get_str_call419 = call ptr @hew_vec_get_str(ptr %"hew_vec_get_str arg0417", i64 %"hew_vec_get_str arg1418")
  store ptr %hew_vec_get_str_call419, ptr %local_130, align 8
  store ptr @str_lit.15, ptr %local_131, align 8
  %string_cmp_lhs420 = load ptr, ptr %local_130, align 8
  %string_cmp_rhs421 = load ptr, ptr %local_131, align 8
  %hew_string_equals422 = call i32 @hew_string_equals(ptr %string_cmp_lhs420, ptr %string_cmp_rhs421)
  %string_cmp_bit423 = icmp eq i32 %hew_string_equals422, 0
  %string_cmp_zext424 = zext i1 %string_cmp_bit423 to i8
  store i8 %string_cmp_zext424, ptr %local_132, align 1
  %"hew_string_drop drop425" = load ptr, ptr %local_130, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop425")
  store ptr null, ptr %local_130, align 8
  %cond_load426 = load i8, ptr %local_132, align 1
  %cond_nz427 = icmp ne i8 %cond_load426, 0
  br i1 %cond_nz427, label %bb99, label %bb100

bb99:                                             ; preds = %bb98
  store i64 29, ptr %local_133, align 8
  %move_load428 = load i64, ptr %local_133, align 8
  store i64 %move_load428, ptr %return_slot, align 8
  %"hew_vec_free drop429" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop429")
  store ptr null, ptr %local_1, align 8
  %ret_val430 = load i64, ptr %return_slot, align 8
  ret i64 %ret_val430

bb100:                                            ; preds = %bb98
  br label %bb101

bb101:                                            ; preds = %after_cooperate439, %bb100
  %hew_actor_cooperate431 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel432 = icmp eq i32 %hew_actor_cooperate431, 2
  br i1 %hew_cooperate_is_cancel432, label %cancel_exit433, label %after_cooperate434

bb102:                                            ; No predecessors!
  %hew_actor_cooperate436 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel437 = icmp eq i32 %hew_actor_cooperate436, 2
  br i1 %hew_cooperate_is_cancel437, label %cancel_exit438, label %after_cooperate439

bb103:                                            ; preds = %bb37
  %"hew_vec_free drop440" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop440")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb104:                                            ; preds = %bb38
  %"hew_vec_free drop441" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop441")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb105:                                            ; preds = %bb38
  %"hew_vec_get_str arg0442" = load ptr, ptr %local_1, align 8
  %"hew_vec_get_str arg1443" = load i64, ptr %local_137, align 8
  %hew_vec_get_str_call444 = call ptr @hew_vec_get_str(ptr %"hew_vec_get_str arg0442", i64 %"hew_vec_get_str arg1443")
  store ptr %hew_vec_get_str_call444, ptr %local_140, align 8
  store ptr @str_lit.16, ptr %local_141, align 8
  %string_cmp_lhs445 = load ptr, ptr %local_140, align 8
  %string_cmp_rhs446 = load ptr, ptr %local_141, align 8
  %hew_string_equals447 = call i32 @hew_string_equals(ptr %string_cmp_lhs445, ptr %string_cmp_rhs446)
  %string_cmp_bit448 = icmp eq i32 %hew_string_equals447, 0
  %string_cmp_zext449 = zext i1 %string_cmp_bit448 to i8
  store i8 %string_cmp_zext449, ptr %local_142, align 1
  %"hew_string_drop drop450" = load ptr, ptr %local_140, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop450")
  store ptr null, ptr %local_140, align 8
  %cond_load451 = load i8, ptr %local_142, align 1
  %cond_nz452 = icmp ne i8 %cond_load451, 0
  br i1 %cond_nz452, label %bb107, label %bb106

bb106:                                            ; preds = %bb105
  store i64 7, ptr %local_143, align 8
  %"hew_vec_len arg0453" = load ptr, ptr %local_1, align 8
  %hew_vec_len_call454 = call i64 @hew_vec_len(ptr %"hew_vec_len arg0453")
  store i64 %hew_vec_len_call454, ptr %local_144, align 8
  %cmp_lhs455 = load i64, ptr %local_143, align 8
  %cmp_rhs456 = load i64, ptr %local_144, align 8
  %cmp_bit457 = icmp uge i64 %cmp_lhs455, %cmp_rhs456
  %cmp_zext458 = zext i1 %cmp_bit457 to i8
  store i8 %cmp_zext458, ptr %local_145, align 1
  %cond_load459 = load i8, ptr %local_145, align 1
  %cond_nz460 = icmp ne i8 %cond_load459, 0
  br i1 %cond_nz460, label %bb108, label %bb109

bb107:                                            ; preds = %after_cooperate477, %bb105
  %cond_load461 = load i8, ptr %local_136, align 1
  %cond_nz462 = icmp ne i8 %cond_load461, 0
  br i1 %cond_nz462, label %bb110, label %bb111

bb108:                                            ; preds = %bb106
  %"hew_vec_free drop463" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop463")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb109:                                            ; preds = %bb106
  %"hew_vec_get_str arg0464" = load ptr, ptr %local_1, align 8
  %"hew_vec_get_str arg1465" = load i64, ptr %local_143, align 8
  %hew_vec_get_str_call466 = call ptr @hew_vec_get_str(ptr %"hew_vec_get_str arg0464", i64 %"hew_vec_get_str arg1465")
  store ptr %hew_vec_get_str_call466, ptr %local_146, align 8
  store ptr @str_lit.17, ptr %local_147, align 8
  %string_cmp_lhs467 = load ptr, ptr %local_146, align 8
  %string_cmp_rhs468 = load ptr, ptr %local_147, align 8
  %hew_string_equals469 = call i32 @hew_string_equals(ptr %string_cmp_lhs467, ptr %string_cmp_rhs468)
  %string_cmp_bit470 = icmp eq i32 %hew_string_equals469, 0
  %string_cmp_zext471 = zext i1 %string_cmp_bit470 to i8
  store i8 %string_cmp_zext471, ptr %local_148, align 1
  %"hew_string_drop drop472" = load ptr, ptr %local_146, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop472")
  store ptr null, ptr %local_146, align 8
  %move_load473 = load i8, ptr %local_148, align 1
  store i8 %move_load473, ptr %local_136, align 1
  %hew_actor_cooperate474 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel475 = icmp eq i32 %hew_actor_cooperate474, 2
  br i1 %hew_cooperate_is_cancel475, label %cancel_exit476, label %after_cooperate477

bb110:                                            ; preds = %bb107
  store i64 30, ptr %local_149, align 8
  %move_load479 = load i64, ptr %local_149, align 8
  store i64 %move_load479, ptr %return_slot, align 8
  %"hew_vec_free drop480" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop480")
  store ptr null, ptr %local_1, align 8
  %ret_val481 = load i64, ptr %return_slot, align 8
  ret i64 %ret_val481

bb111:                                            ; preds = %bb107
  br label %bb112

bb112:                                            ; preds = %after_cooperate493, %bb111
  store i8 1, ptr %local_151, align 1
  store i64 248, ptr %local_152, align 8
  %"hew_vec_len arg0482" = load ptr, ptr %local_1, align 8
  %hew_vec_len_call483 = call i64 @hew_vec_len(ptr %"hew_vec_len arg0482")
  store i64 %hew_vec_len_call483, ptr %local_153, align 8
  %cmp_lhs484 = load i64, ptr %local_152, align 8
  %cmp_rhs485 = load i64, ptr %local_153, align 8
  %cmp_bit486 = icmp uge i64 %cmp_lhs484, %cmp_rhs485
  %cmp_zext487 = zext i1 %cmp_bit486 to i8
  store i8 %cmp_zext487, ptr %local_154, align 1
  %cond_load488 = load i8, ptr %local_154, align 1
  %cond_nz489 = icmp ne i8 %cond_load488, 0
  br i1 %cond_nz489, label %bb114, label %bb115

bb113:                                            ; No predecessors!
  %hew_actor_cooperate490 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel491 = icmp eq i32 %hew_actor_cooperate490, 2
  br i1 %hew_cooperate_is_cancel491, label %cancel_exit492, label %after_cooperate493

bb114:                                            ; preds = %bb112
  %"hew_vec_free drop494" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop494")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb115:                                            ; preds = %bb112
  %"hew_vec_get_str arg0495" = load ptr, ptr %local_1, align 8
  %"hew_vec_get_str arg1496" = load i64, ptr %local_152, align 8
  %hew_vec_get_str_call497 = call ptr @hew_vec_get_str(ptr %"hew_vec_get_str arg0495", i64 %"hew_vec_get_str arg1496")
  store ptr %hew_vec_get_str_call497, ptr %local_155, align 8
  store ptr @str_lit.18, ptr %local_156, align 8
  %string_cmp_lhs498 = load ptr, ptr %local_155, align 8
  %string_cmp_rhs499 = load ptr, ptr %local_156, align 8
  %hew_string_equals500 = call i32 @hew_string_equals(ptr %string_cmp_lhs498, ptr %string_cmp_rhs499)
  %string_cmp_bit501 = icmp eq i32 %hew_string_equals500, 0
  %string_cmp_zext502 = zext i1 %string_cmp_bit501 to i8
  store i8 %string_cmp_zext502, ptr %local_157, align 1
  %"hew_string_drop drop503" = load ptr, ptr %local_155, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop503")
  store ptr null, ptr %local_155, align 8
  %cond_load504 = load i8, ptr %local_157, align 1
  %cond_nz505 = icmp ne i8 %cond_load504, 0
  br i1 %cond_nz505, label %bb117, label %bb116

bb116:                                            ; preds = %bb115
  store i64 255, ptr %local_158, align 8
  %"hew_vec_len arg0506" = load ptr, ptr %local_1, align 8
  %hew_vec_len_call507 = call i64 @hew_vec_len(ptr %"hew_vec_len arg0506")
  store i64 %hew_vec_len_call507, ptr %local_159, align 8
  %cmp_lhs508 = load i64, ptr %local_158, align 8
  %cmp_rhs509 = load i64, ptr %local_159, align 8
  %cmp_bit510 = icmp uge i64 %cmp_lhs508, %cmp_rhs509
  %cmp_zext511 = zext i1 %cmp_bit510 to i8
  store i8 %cmp_zext511, ptr %local_160, align 1
  %cond_load512 = load i8, ptr %local_160, align 1
  %cond_nz513 = icmp ne i8 %cond_load512, 0
  br i1 %cond_nz513, label %bb118, label %bb119

bb117:                                            ; preds = %after_cooperate530, %bb115
  %cond_load514 = load i8, ptr %local_151, align 1
  %cond_nz515 = icmp ne i8 %cond_load514, 0
  br i1 %cond_nz515, label %bb120, label %bb121

bb118:                                            ; preds = %bb116
  %"hew_vec_free drop516" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop516")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb119:                                            ; preds = %bb116
  %"hew_vec_get_str arg0517" = load ptr, ptr %local_1, align 8
  %"hew_vec_get_str arg1518" = load i64, ptr %local_158, align 8
  %hew_vec_get_str_call519 = call ptr @hew_vec_get_str(ptr %"hew_vec_get_str arg0517", i64 %"hew_vec_get_str arg1518")
  store ptr %hew_vec_get_str_call519, ptr %local_161, align 8
  store ptr @str_lit.19, ptr %local_162, align 8
  %string_cmp_lhs520 = load ptr, ptr %local_161, align 8
  %string_cmp_rhs521 = load ptr, ptr %local_162, align 8
  %hew_string_equals522 = call i32 @hew_string_equals(ptr %string_cmp_lhs520, ptr %string_cmp_rhs521)
  %string_cmp_bit523 = icmp eq i32 %hew_string_equals522, 0
  %string_cmp_zext524 = zext i1 %string_cmp_bit523 to i8
  store i8 %string_cmp_zext524, ptr %local_163, align 1
  %"hew_string_drop drop525" = load ptr, ptr %local_161, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop525")
  store ptr null, ptr %local_161, align 8
  %move_load526 = load i8, ptr %local_163, align 1
  store i8 %move_load526, ptr %local_151, align 1
  %hew_actor_cooperate527 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel528 = icmp eq i32 %hew_actor_cooperate527, 2
  br i1 %hew_cooperate_is_cancel528, label %cancel_exit529, label %after_cooperate530

bb120:                                            ; preds = %bb117
  store i64 31, ptr %local_164, align 8
  %move_load532 = load i64, ptr %local_164, align 8
  store i64 %move_load532, ptr %return_slot, align 8
  %"hew_vec_free drop533" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop533")
  store ptr null, ptr %local_1, align 8
  %ret_val534 = load i64, ptr %return_slot, align 8
  ret i64 %ret_val534

bb121:                                            ; preds = %bb117
  br label %bb122

bb122:                                            ; preds = %after_cooperate539, %bb121
  %hew_vec_new_str_call535 = call ptr @hew_vec_new_str()
  store ptr %hew_vec_new_str_call535, ptr %local_165, align 8
  br label %bb124

bb123:                                            ; No predecessors!
  %hew_actor_cooperate536 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel537 = icmp eq i32 %hew_actor_cooperate536, 2
  br i1 %hew_cooperate_is_cancel537, label %cancel_exit538, label %after_cooperate539

bb124:                                            ; preds = %bb122
  %move_load540 = load ptr, ptr %local_165, align 8
  store ptr %move_load540, ptr %local_166, align 8
  store i64 1, ptr %local_169, align 8
  %move_load541 = load i64, ptr %local_169, align 8
  store i64 %move_load541, ptr %local_170, align 8
  store i64 0, ptr %local_171, align 8
  %cmp_lhs542 = load i64, ptr %local_170, align 8
  %cmp_rhs543 = load i64, ptr %local_171, align 8
  %cmp_bit544 = icmp sle i64 %cmp_lhs542, %cmp_rhs543
  %cmp_zext545 = zext i1 %cmp_bit544 to i8
  store i8 %cmp_zext545, ptr %local_172, align 1
  %cond_load546 = load i8, ptr %local_172, align 1
  %cond_nz547 = icmp ne i8 %cond_load546, 0
  br i1 %cond_nz547, label %bb129, label %bb130

bb125:                                            ; preds = %after_cooperate577, %bb127
  %cmp_lhs548 = load i64, ptr %local_167, align 8
  %cmp_rhs549 = load i64, ptr %local_168, align 8
  %cmp_bit550 = icmp slt i64 %cmp_lhs548, %cmp_rhs549
  %cmp_zext551 = zext i1 %cmp_bit550 to i8
  store i8 %cmp_zext551, ptr %local_175, align 1
  %cond_load552 = load i8, ptr %local_175, align 1
  %cond_nz553 = icmp ne i8 %cond_load552, 0
  br i1 %cond_nz553, label %bb126, label %bb128

bb126:                                            ; preds = %bb125
  store i64 3, ptr %local_177, align 8
  store i64 0, ptr %local_179, align 8
  %cmp_lhs554 = load i64, ptr %local_177, align 8
  %cmp_rhs555 = load i64, ptr %local_179, align 8
  %cmp_bit556 = icmp eq i64 %cmp_lhs554, %cmp_rhs555
  %cmp_zext557 = zext i1 %cmp_bit556 to i8
  store i8 %cmp_zext557, ptr %local_180, align 1
  %cond_load558 = load i8, ptr %local_180, align 1
  %cond_nz559 = icmp ne i8 %cond_load558, 0
  br i1 %cond_nz559, label %bb131, label %bb132

bb127:                                            ; preds = %after_cooperate635
  %checked_lhs560 = load i64, ptr %local_167, align 8
  %checked_rhs561 = load i64, ptr %local_170, align 8
  %with_overflow562 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs560, i64 %checked_rhs561)
  %checked_result563 = extractvalue { i64, i1 } %with_overflow562, 0
  %checked_overflow564 = extractvalue { i64, i1 } %with_overflow562, 1
  %checked_overflow_widen565 = zext i1 %checked_overflow564 to i8
  store i64 %checked_result563, ptr %local_167, align 8
  store i8 %checked_overflow_widen565, ptr %local_193, align 1
  %cond_load566 = load i8, ptr %local_193, align 1
  %cond_nz567 = icmp ne i8 %cond_load566, 0
  br i1 %cond_nz567, label %bb144, label %bb125

bb128:                                            ; preds = %bb125
  %call_arg568 = load ptr, ptr %local_166, align 8
  %call_result569 = call ptr @"sort$sort_strings"(ptr %call_arg568)
  store ptr %call_result569, ptr %local_194, align 8
  br label %bb145

bb129:                                            ; preds = %bb124
  %"hew_vec_free drop570" = load ptr, ptr %local_166, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop570")
  store ptr null, ptr %local_166, align 8
  %"hew_vec_free drop571" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop571")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 202)
  call void @llvm.trap()
  unreachable

bb130:                                            ; preds = %bb124
  store i64 0, ptr %local_173, align 8
  store i64 17, ptr %local_174, align 8
  %move_load572 = load i64, ptr %local_173, align 8
  store i64 %move_load572, ptr %local_167, align 8
  %move_load573 = load i64, ptr %local_174, align 8
  store i64 %move_load573, ptr %local_168, align 8
  %hew_actor_cooperate574 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel575 = icmp eq i32 %hew_actor_cooperate574, 2
  br i1 %hew_cooperate_is_cancel575, label %cancel_exit576, label %after_cooperate577

bb131:                                            ; preds = %bb126
  %"hew_vec_free drop580" = load ptr, ptr %local_166, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop580")
  store ptr null, ptr %local_166, align 8
  %"hew_vec_free drop581" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop581")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 202)
  call void @llvm.trap()
  unreachable

bb132:                                            ; preds = %bb126
  store i64 -9223372036854775808, ptr %local_181, align 8
  %cmp_lhs582 = load i64, ptr %local_167, align 8
  %cmp_rhs583 = load i64, ptr %local_181, align 8
  %cmp_bit584 = icmp eq i64 %cmp_lhs582, %cmp_rhs583
  %cmp_zext585 = zext i1 %cmp_bit584 to i8
  store i8 %cmp_zext585, ptr %local_182, align 1
  %cond_load586 = load i8, ptr %local_182, align 1
  %cond_nz587 = icmp ne i8 %cond_load586, 0
  br i1 %cond_nz587, label %bb133, label %bb134

bb133:                                            ; preds = %bb132
  store i64 -1, ptr %local_183, align 8
  %cmp_lhs588 = load i64, ptr %local_177, align 8
  %cmp_rhs589 = load i64, ptr %local_183, align 8
  %cmp_bit590 = icmp eq i64 %cmp_lhs588, %cmp_rhs589
  %cmp_zext591 = zext i1 %cmp_bit590 to i8
  store i8 %cmp_zext591, ptr %local_184, align 1
  %cond_load592 = load i8, ptr %local_184, align 1
  %cond_nz593 = icmp ne i8 %cond_load592, 0
  br i1 %cond_nz593, label %bb135, label %bb134

bb134:                                            ; preds = %bb133, %bb132
  %div_lhs = load i64, ptr %local_167, align 8
  %div_rhs = load i64, ptr %local_177, align 8
  %srem = srem i64 %div_lhs, %div_rhs
  store i64 %srem, ptr %local_178, align 8
  br label %bb137

bb135:                                            ; preds = %bb133
  %"hew_vec_free drop594" = load ptr, ptr %local_166, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop594")
  store ptr null, ptr %local_166, align 8
  %"hew_vec_free drop595" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop595")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 203)
  call void @llvm.trap()
  unreachable

bb136:                                            ; preds = %after_cooperate629, %after_cooperate622, %after_cooperate615
  %move_load596 = load ptr, ptr %local_176, align 8
  store ptr %move_load596, ptr %local_192, align 8
  %call_arg597 = load ptr, ptr %local_166, align 8
  %call_arg598 = load ptr, ptr %local_192, align 8
  call void @hew_vec_push_str(ptr %call_arg597, ptr %call_arg598)
  br label %bb143

bb137:                                            ; preds = %bb134
  store i64 0, ptr %local_185, align 8
  %cmp_lhs599 = load i64, ptr %local_178, align 8
  %cmp_rhs600 = load i64, ptr %local_185, align 8
  %cmp_bit601 = icmp eq i64 %cmp_lhs599, %cmp_rhs600
  %cmp_zext602 = zext i1 %cmp_bit601 to i8
  store i8 %cmp_zext602, ptr %local_186, align 1
  %cond_load603 = load i8, ptr %local_186, align 1
  %cond_nz604 = icmp ne i8 %cond_load603, 0
  br i1 %cond_nz604, label %bb141, label %bb138

bb138:                                            ; preds = %bb137
  store i64 1, ptr %local_188, align 8
  %cmp_lhs605 = load i64, ptr %local_178, align 8
  %cmp_rhs606 = load i64, ptr %local_188, align 8
  %cmp_bit607 = icmp eq i64 %cmp_lhs605, %cmp_rhs606
  %cmp_zext608 = zext i1 %cmp_bit607 to i8
  store i8 %cmp_zext608, ptr %local_189, align 1
  %cond_load609 = load i8, ptr %local_189, align 1
  %cond_nz610 = icmp ne i8 %cond_load609, 0
  br i1 %cond_nz610, label %bb142, label %bb139

bb139:                                            ; preds = %bb138
  store ptr @str_lit.20, ptr %local_191, align 8
  %move_load611 = load ptr, ptr %local_191, align 8
  store ptr %move_load611, ptr %local_176, align 8
  %hew_actor_cooperate612 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel613 = icmp eq i32 %hew_actor_cooperate612, 2
  br i1 %hew_cooperate_is_cancel613, label %cancel_exit614, label %after_cooperate615

bb140:                                            ; No predecessors!
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

bb141:                                            ; preds = %bb137
  store ptr @str_lit.21, ptr %local_187, align 8
  %move_load618 = load ptr, ptr %local_187, align 8
  store ptr %move_load618, ptr %local_176, align 8
  %hew_actor_cooperate619 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel620 = icmp eq i32 %hew_actor_cooperate619, 2
  br i1 %hew_cooperate_is_cancel620, label %cancel_exit621, label %after_cooperate622

bb142:                                            ; preds = %bb138
  store ptr @str_lit.22, ptr %local_190, align 8
  %move_load625 = load ptr, ptr %local_190, align 8
  store ptr %move_load625, ptr %local_176, align 8
  %hew_actor_cooperate626 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel627 = icmp eq i32 %hew_actor_cooperate626, 2
  br i1 %hew_cooperate_is_cancel627, label %cancel_exit628, label %after_cooperate629

bb143:                                            ; preds = %bb136
  %hew_actor_cooperate632 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel633 = icmp eq i32 %hew_actor_cooperate632, 2
  br i1 %hew_cooperate_is_cancel633, label %cancel_exit634, label %after_cooperate635

bb144:                                            ; preds = %bb127
  %"hew_string_drop drop640" = load ptr, ptr %local_192, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop640")
  store ptr null, ptr %local_192, align 8
  %"hew_vec_free drop641" = load ptr, ptr %local_166, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop641")
  store ptr null, ptr %local_166, align 8
  %"hew_vec_free drop642" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop642")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb145:                                            ; preds = %bb128
  %move_load643 = load ptr, ptr %local_194, align 8
  store ptr %move_load643, ptr %local_195, align 8
  store i64 1, ptr %local_198, align 8
  %move_load644 = load i64, ptr %local_198, align 8
  store i64 %move_load644, ptr %local_199, align 8
  store i64 0, ptr %local_200, align 8
  %cmp_lhs645 = load i64, ptr %local_199, align 8
  %cmp_rhs646 = load i64, ptr %local_200, align 8
  %cmp_bit647 = icmp sle i64 %cmp_lhs645, %cmp_rhs646
  %cmp_zext648 = zext i1 %cmp_bit647 to i8
  store i8 %cmp_zext648, ptr %local_201, align 1
  %cond_load649 = load i8, ptr %local_201, align 1
  %cond_nz650 = icmp ne i8 %cond_load649, 0
  br i1 %cond_nz650, label %bb150, label %bb151

bb146:                                            ; preds = %after_cooperate688, %bb148
  %cmp_lhs651 = load i64, ptr %local_196, align 8
  %cmp_rhs652 = load i64, ptr %local_197, align 8
  %cmp_bit653 = icmp slt i64 %cmp_lhs651, %cmp_rhs652
  %cmp_zext654 = zext i1 %cmp_bit653 to i8
  store i8 %cmp_zext654, ptr %local_204, align 1
  %cond_load655 = load i8, ptr %local_204, align 1
  %cond_nz656 = icmp ne i8 %cond_load655, 0
  br i1 %cond_nz656, label %bb147, label %bb149

bb147:                                            ; preds = %bb146
  store i64 1, ptr %local_206, align 8
  %checked_lhs657 = load i64, ptr %local_196, align 8
  %checked_rhs658 = load i64, ptr %local_206, align 8
  %with_overflow659 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %checked_lhs657, i64 %checked_rhs658)
  %checked_result660 = extractvalue { i64, i1 } %with_overflow659, 0
  %checked_overflow661 = extractvalue { i64, i1 } %with_overflow659, 1
  %checked_overflow_widen662 = zext i1 %checked_overflow661 to i8
  store i64 %checked_result660, ptr %local_207, align 8
  store i8 %checked_overflow_widen662, ptr %local_208, align 1
  %cond_load663 = load i8, ptr %local_208, align 1
  %cond_nz664 = icmp ne i8 %cond_load663, 0
  br i1 %cond_nz664, label %bb152, label %bb153

bb148:                                            ; preds = %after_cooperate739
  %checked_lhs665 = load i64, ptr %local_196, align 8
  %checked_rhs666 = load i64, ptr %local_199, align 8
  %with_overflow667 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs665, i64 %checked_rhs666)
  %checked_result668 = extractvalue { i64, i1 } %with_overflow667, 0
  %checked_overflow669 = extractvalue { i64, i1 } %with_overflow667, 1
  %checked_overflow_widen670 = zext i1 %checked_overflow669 to i8
  store i64 %checked_result668, ptr %local_196, align 8
  store i8 %checked_overflow_widen670, ptr %local_217, align 1
  %cond_load671 = load i8, ptr %local_217, align 1
  %cond_nz672 = icmp ne i8 %cond_load671, 0
  br i1 %cond_nz672, label %bb162, label %bb146

bb149:                                            ; preds = %bb146
  store i64 1, ptr %local_220, align 8
  %move_load673 = load i64, ptr %local_220, align 8
  store i64 %move_load673, ptr %local_221, align 8
  store i64 0, ptr %local_222, align 8
  %cmp_lhs674 = load i64, ptr %local_221, align 8
  %cmp_rhs675 = load i64, ptr %local_222, align 8
  %cmp_bit676 = icmp sle i64 %cmp_lhs674, %cmp_rhs675
  %cmp_zext677 = zext i1 %cmp_bit676 to i8
  store i8 %cmp_zext677, ptr %local_223, align 1
  %cond_load678 = load i8, ptr %local_223, align 1
  %cond_nz679 = icmp ne i8 %cond_load678, 0
  br i1 %cond_nz679, label %bb167, label %bb168

bb150:                                            ; preds = %bb145
  %"hew_vec_free drop680" = load ptr, ptr %local_195, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop680")
  store ptr null, ptr %local_195, align 8
  %"hew_vec_free drop681" = load ptr, ptr %local_166, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop681")
  store ptr null, ptr %local_166, align 8
  %"hew_vec_free drop682" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop682")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 202)
  call void @llvm.trap()
  unreachable

bb151:                                            ; preds = %bb145
  store i64 1, ptr %local_202, align 8
  store i64 17, ptr %local_203, align 8
  %move_load683 = load i64, ptr %local_202, align 8
  store i64 %move_load683, ptr %local_196, align 8
  %move_load684 = load i64, ptr %local_203, align 8
  store i64 %move_load684, ptr %local_197, align 8
  %hew_actor_cooperate685 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel686 = icmp eq i32 %hew_actor_cooperate685, 2
  br i1 %hew_cooperate_is_cancel686, label %cancel_exit687, label %after_cooperate688

bb152:                                            ; preds = %bb147
  %"hew_vec_free drop692" = load ptr, ptr %local_195, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop692")
  store ptr null, ptr %local_195, align 8
  %"hew_vec_free drop693" = load ptr, ptr %local_166, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop693")
  store ptr null, ptr %local_166, align 8
  %"hew_vec_free drop694" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop694")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb153:                                            ; preds = %bb147
  %"hew_vec_len arg0695" = load ptr, ptr %local_195, align 8
  %hew_vec_len_call696 = call i64 @hew_vec_len(ptr %"hew_vec_len arg0695")
  store i64 %hew_vec_len_call696, ptr %local_209, align 8
  %cmp_lhs697 = load i64, ptr %local_207, align 8
  %cmp_rhs698 = load i64, ptr %local_209, align 8
  %cmp_bit699 = icmp uge i64 %cmp_lhs697, %cmp_rhs698
  %cmp_zext700 = zext i1 %cmp_bit699 to i8
  store i8 %cmp_zext700, ptr %local_210, align 1
  %cond_load701 = load i8, ptr %local_210, align 1
  %cond_nz702 = icmp ne i8 %cond_load701, 0
  br i1 %cond_nz702, label %bb154, label %bb155

bb154:                                            ; preds = %bb153
  %"hew_vec_free drop703" = load ptr, ptr %local_195, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop703")
  store ptr null, ptr %local_195, align 8
  %"hew_vec_free drop704" = load ptr, ptr %local_166, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop704")
  store ptr null, ptr %local_166, align 8
  %"hew_vec_free drop705" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop705")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb155:                                            ; preds = %bb153
  %"hew_vec_get_str arg0706" = load ptr, ptr %local_195, align 8
  %"hew_vec_get_str arg1707" = load i64, ptr %local_207, align 8
  %hew_vec_get_str_call708 = call ptr @hew_vec_get_str(ptr %"hew_vec_get_str arg0706", i64 %"hew_vec_get_str arg1707")
  store ptr %hew_vec_get_str_call708, ptr %local_211, align 8
  %"hew_vec_len arg0709" = load ptr, ptr %local_195, align 8
  %hew_vec_len_call710 = call i64 @hew_vec_len(ptr %"hew_vec_len arg0709")
  store i64 %hew_vec_len_call710, ptr %local_212, align 8
  %cmp_lhs711 = load i64, ptr %local_196, align 8
  %cmp_rhs712 = load i64, ptr %local_212, align 8
  %cmp_bit713 = icmp uge i64 %cmp_lhs711, %cmp_rhs712
  %cmp_zext714 = zext i1 %cmp_bit713 to i8
  store i8 %cmp_zext714, ptr %local_213, align 1
  %cond_load715 = load i8, ptr %local_213, align 1
  %cond_nz716 = icmp ne i8 %cond_load715, 0
  br i1 %cond_nz716, label %bb156, label %bb157

bb156:                                            ; preds = %bb155
  %"hew_vec_free drop717" = load ptr, ptr %local_195, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop717")
  store ptr null, ptr %local_195, align 8
  %"hew_vec_free drop718" = load ptr, ptr %local_166, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop718")
  store ptr null, ptr %local_166, align 8
  %"hew_vec_free drop719" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop719")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb157:                                            ; preds = %bb155
  %"hew_vec_get_str arg0720" = load ptr, ptr %local_195, align 8
  %"hew_vec_get_str arg1721" = load i64, ptr %local_196, align 8
  %hew_vec_get_str_call722 = call ptr @hew_vec_get_str(ptr %"hew_vec_get_str arg0720", i64 %"hew_vec_get_str arg1721")
  store ptr %hew_vec_get_str_call722, ptr %local_214, align 8
  %string_ord_lhs723 = load ptr, ptr %local_211, align 8
  %string_ord_rhs724 = load ptr, ptr %local_214, align 8
  %hew_string_compare725 = call i32 @hew_string_compare(ptr %string_ord_lhs723, ptr %string_ord_rhs724)
  %string_ord_bit726 = icmp sgt i32 %hew_string_compare725, 0
  %string_ord_zext727 = zext i1 %string_ord_bit726 to i8
  store i8 %string_ord_zext727, ptr %local_215, align 1
  %"hew_string_drop drop728" = load ptr, ptr %local_214, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop728")
  store ptr null, ptr %local_214, align 8
  %cond_load729 = load i8, ptr %local_215, align 1
  %cond_nz730 = icmp ne i8 %cond_load729, 0
  br i1 %cond_nz730, label %bb158, label %bb159

bb158:                                            ; preds = %bb157
  store i64 32, ptr %local_216, align 8
  %move_load731 = load i64, ptr %local_216, align 8
  store i64 %move_load731, ptr %return_slot, align 8
  %"hew_vec_free drop732" = load ptr, ptr %local_195, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop732")
  store ptr null, ptr %local_195, align 8
  %"hew_vec_free drop733" = load ptr, ptr %local_166, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop733")
  store ptr null, ptr %local_166, align 8
  %"hew_vec_free drop734" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop734")
  store ptr null, ptr %local_1, align 8
  %ret_val735 = load i64, ptr %return_slot, align 8
  ret i64 %ret_val735

bb159:                                            ; preds = %bb157
  br label %bb160

bb160:                                            ; preds = %after_cooperate746, %bb159
  %hew_actor_cooperate736 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel737 = icmp eq i32 %hew_actor_cooperate736, 2
  br i1 %hew_cooperate_is_cancel737, label %cancel_exit738, label %after_cooperate739

bb161:                                            ; No predecessors!
  %hew_actor_cooperate743 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel744 = icmp eq i32 %hew_actor_cooperate743, 2
  br i1 %hew_cooperate_is_cancel744, label %cancel_exit745, label %after_cooperate746

bb162:                                            ; preds = %bb148
  %"hew_vec_free drop747" = load ptr, ptr %local_195, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop747")
  store ptr null, ptr %local_195, align 8
  %"hew_vec_free drop748" = load ptr, ptr %local_166, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop748")
  store ptr null, ptr %local_166, align 8
  %"hew_vec_free drop749" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop749")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb163:                                            ; preds = %after_cooperate783, %bb165
  %cmp_lhs750 = load i64, ptr %local_218, align 8
  %cmp_rhs751 = load i64, ptr %local_219, align 8
  %cmp_bit752 = icmp slt i64 %cmp_lhs750, %cmp_rhs751
  %cmp_zext753 = zext i1 %cmp_bit752 to i8
  store i8 %cmp_zext753, ptr %local_226, align 1
  %cond_load754 = load i8, ptr %local_226, align 1
  %cond_nz755 = icmp ne i8 %cond_load754, 0
  br i1 %cond_nz755, label %bb164, label %bb166

bb164:                                            ; preds = %bb163
  store i64 3, ptr %local_228, align 8
  store i64 0, ptr %local_230, align 8
  %cmp_lhs756 = load i64, ptr %local_228, align 8
  %cmp_rhs757 = load i64, ptr %local_230, align 8
  %cmp_bit758 = icmp eq i64 %cmp_lhs756, %cmp_rhs757
  %cmp_zext759 = zext i1 %cmp_bit758 to i8
  store i8 %cmp_zext759, ptr %local_231, align 1
  %cond_load760 = load i8, ptr %local_231, align 1
  %cond_nz761 = icmp ne i8 %cond_load760, 0
  br i1 %cond_nz761, label %bb169, label %bb170

bb165:                                            ; preds = %after_cooperate877
  %checked_lhs762 = load i64, ptr %local_218, align 8
  %checked_rhs763 = load i64, ptr %local_221, align 8
  %with_overflow764 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs762, i64 %checked_rhs763)
  %checked_result765 = extractvalue { i64, i1 } %with_overflow764, 0
  %checked_overflow766 = extractvalue { i64, i1 } %with_overflow764, 1
  %checked_overflow_widen767 = zext i1 %checked_overflow766 to i8
  store i64 %checked_result765, ptr %local_218, align 8
  store i8 %checked_overflow_widen767, ptr %local_250, align 1
  %cond_load768 = load i8, ptr %local_250, align 1
  %cond_nz769 = icmp ne i8 %cond_load768, 0
  br i1 %cond_nz769, label %bb187, label %bb163

bb166:                                            ; preds = %bb163
  store i64 0, ptr %local_251, align 8
  %move_load770 = load i64, ptr %local_251, align 8
  store i64 %move_load770, ptr %return_slot, align 8
  %"hew_vec_free drop771" = load ptr, ptr %local_195, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop771")
  store ptr null, ptr %local_195, align 8
  %"hew_vec_free drop772" = load ptr, ptr %local_166, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop772")
  store ptr null, ptr %local_166, align 8
  %"hew_vec_free drop773" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop773")
  store ptr null, ptr %local_1, align 8
  %ret_val774 = load i64, ptr %return_slot, align 8
  ret i64 %ret_val774

bb167:                                            ; preds = %bb149
  %"hew_vec_free drop775" = load ptr, ptr %local_195, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop775")
  store ptr null, ptr %local_195, align 8
  %"hew_vec_free drop776" = load ptr, ptr %local_166, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop776")
  store ptr null, ptr %local_166, align 8
  %"hew_vec_free drop777" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop777")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 202)
  call void @llvm.trap()
  unreachable

bb168:                                            ; preds = %bb149
  store i64 0, ptr %local_224, align 8
  store i64 17, ptr %local_225, align 8
  %move_load778 = load i64, ptr %local_224, align 8
  store i64 %move_load778, ptr %local_218, align 8
  %move_load779 = load i64, ptr %local_225, align 8
  store i64 %move_load779, ptr %local_219, align 8
  %hew_actor_cooperate780 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel781 = icmp eq i32 %hew_actor_cooperate780, 2
  br i1 %hew_cooperate_is_cancel781, label %cancel_exit782, label %after_cooperate783

bb169:                                            ; preds = %bb164
  %"hew_vec_free drop787" = load ptr, ptr %local_195, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop787")
  store ptr null, ptr %local_195, align 8
  %"hew_vec_free drop788" = load ptr, ptr %local_166, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop788")
  store ptr null, ptr %local_166, align 8
  %"hew_vec_free drop789" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop789")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 202)
  call void @llvm.trap()
  unreachable

bb170:                                            ; preds = %bb164
  store i64 -9223372036854775808, ptr %local_232, align 8
  %cmp_lhs790 = load i64, ptr %local_218, align 8
  %cmp_rhs791 = load i64, ptr %local_232, align 8
  %cmp_bit792 = icmp eq i64 %cmp_lhs790, %cmp_rhs791
  %cmp_zext793 = zext i1 %cmp_bit792 to i8
  store i8 %cmp_zext793, ptr %local_233, align 1
  %cond_load794 = load i8, ptr %local_233, align 1
  %cond_nz795 = icmp ne i8 %cond_load794, 0
  br i1 %cond_nz795, label %bb171, label %bb172

bb171:                                            ; preds = %bb170
  store i64 -1, ptr %local_234, align 8
  %cmp_lhs796 = load i64, ptr %local_228, align 8
  %cmp_rhs797 = load i64, ptr %local_234, align 8
  %cmp_bit798 = icmp eq i64 %cmp_lhs796, %cmp_rhs797
  %cmp_zext799 = zext i1 %cmp_bit798 to i8
  store i8 %cmp_zext799, ptr %local_235, align 1
  %cond_load800 = load i8, ptr %local_235, align 1
  %cond_nz801 = icmp ne i8 %cond_load800, 0
  br i1 %cond_nz801, label %bb173, label %bb172

bb172:                                            ; preds = %bb171, %bb170
  %div_lhs802 = load i64, ptr %local_218, align 8
  %div_rhs803 = load i64, ptr %local_228, align 8
  %srem804 = srem i64 %div_lhs802, %div_rhs803
  store i64 %srem804, ptr %local_229, align 8
  br label %bb175

bb173:                                            ; preds = %bb171
  %"hew_vec_free drop805" = load ptr, ptr %local_195, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop805")
  store ptr null, ptr %local_195, align 8
  %"hew_vec_free drop806" = load ptr, ptr %local_166, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop806")
  store ptr null, ptr %local_166, align 8
  %"hew_vec_free drop807" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop807")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 203)
  call void @llvm.trap()
  unreachable

bb174:                                            ; preds = %after_cooperate849, %after_cooperate841, %after_cooperate833
  %move_load808 = load ptr, ptr %local_227, align 8
  store ptr %move_load808, ptr %local_243, align 8
  %"hew_vec_len arg0809" = load ptr, ptr %local_166, align 8
  %hew_vec_len_call810 = call i64 @hew_vec_len(ptr %"hew_vec_len arg0809")
  store i64 %hew_vec_len_call810, ptr %local_245, align 8
  %cmp_lhs811 = load i64, ptr %local_218, align 8
  %cmp_rhs812 = load i64, ptr %local_245, align 8
  %cmp_bit813 = icmp uge i64 %cmp_lhs811, %cmp_rhs812
  %cmp_zext814 = zext i1 %cmp_bit813 to i8
  store i8 %cmp_zext814, ptr %local_246, align 1
  %cond_load815 = load i8, ptr %local_246, align 1
  %cond_nz816 = icmp ne i8 %cond_load815, 0
  br i1 %cond_nz816, label %bb181, label %bb182

bb175:                                            ; preds = %bb172
  store i64 0, ptr %local_236, align 8
  %cmp_lhs817 = load i64, ptr %local_229, align 8
  %cmp_rhs818 = load i64, ptr %local_236, align 8
  %cmp_bit819 = icmp eq i64 %cmp_lhs817, %cmp_rhs818
  %cmp_zext820 = zext i1 %cmp_bit819 to i8
  store i8 %cmp_zext820, ptr %local_237, align 1
  %cond_load821 = load i8, ptr %local_237, align 1
  %cond_nz822 = icmp ne i8 %cond_load821, 0
  br i1 %cond_nz822, label %bb179, label %bb176

bb176:                                            ; preds = %bb175
  store i64 1, ptr %local_239, align 8
  %cmp_lhs823 = load i64, ptr %local_229, align 8
  %cmp_rhs824 = load i64, ptr %local_239, align 8
  %cmp_bit825 = icmp eq i64 %cmp_lhs823, %cmp_rhs824
  %cmp_zext826 = zext i1 %cmp_bit825 to i8
  store i8 %cmp_zext826, ptr %local_240, align 1
  %cond_load827 = load i8, ptr %local_240, align 1
  %cond_nz828 = icmp ne i8 %cond_load827, 0
  br i1 %cond_nz828, label %bb180, label %bb177

bb177:                                            ; preds = %bb176
  store ptr @str_lit.23, ptr %local_242, align 8
  %move_load829 = load ptr, ptr %local_242, align 8
  store ptr %move_load829, ptr %local_227, align 8
  %hew_actor_cooperate830 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel831 = icmp eq i32 %hew_actor_cooperate830, 2
  br i1 %hew_cooperate_is_cancel831, label %cancel_exit832, label %after_cooperate833

bb178:                                            ; No predecessors!
  call void @hew_trap_with_code(i32 208)
  call void @llvm.trap()
  unreachable

bb179:                                            ; preds = %bb175
  store ptr @str_lit.24, ptr %local_238, align 8
  %move_load837 = load ptr, ptr %local_238, align 8
  store ptr %move_load837, ptr %local_227, align 8
  %hew_actor_cooperate838 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel839 = icmp eq i32 %hew_actor_cooperate838, 2
  br i1 %hew_cooperate_is_cancel839, label %cancel_exit840, label %after_cooperate841

bb180:                                            ; preds = %bb176
  store ptr @str_lit.25, ptr %local_241, align 8
  %move_load845 = load ptr, ptr %local_241, align 8
  store ptr %move_load845, ptr %local_227, align 8
  %hew_actor_cooperate846 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel847 = icmp eq i32 %hew_actor_cooperate846, 2
  br i1 %hew_cooperate_is_cancel847, label %cancel_exit848, label %after_cooperate849

bb181:                                            ; preds = %bb174
  %"hew_string_drop drop853" = load ptr, ptr %local_243, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop853")
  store ptr null, ptr %local_243, align 8
  %"hew_vec_free drop854" = load ptr, ptr %local_195, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop854")
  store ptr null, ptr %local_195, align 8
  %"hew_vec_free drop855" = load ptr, ptr %local_166, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop855")
  store ptr null, ptr %local_166, align 8
  %"hew_vec_free drop856" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop856")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb182:                                            ; preds = %bb174
  %"hew_vec_get_str arg0857" = load ptr, ptr %local_166, align 8
  %"hew_vec_get_str arg1858" = load i64, ptr %local_218, align 8
  %hew_vec_get_str_call859 = call ptr @hew_vec_get_str(ptr %"hew_vec_get_str arg0857", i64 %"hew_vec_get_str arg1858")
  store ptr %hew_vec_get_str_call859, ptr %local_247, align 8
  %string_cmp_lhs860 = load ptr, ptr %local_247, align 8
  %string_cmp_rhs861 = load ptr, ptr %local_243, align 8
  %hew_string_equals862 = call i32 @hew_string_equals(ptr %string_cmp_lhs860, ptr %string_cmp_rhs861)
  %string_cmp_bit863 = icmp eq i32 %hew_string_equals862, 0
  %string_cmp_zext864 = zext i1 %string_cmp_bit863 to i8
  store i8 %string_cmp_zext864, ptr %local_248, align 1
  %"hew_string_drop drop865" = load ptr, ptr %local_247, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop865")
  store ptr null, ptr %local_247, align 8
  %cond_load866 = load i8, ptr %local_248, align 1
  %cond_nz867 = icmp ne i8 %cond_load866, 0
  br i1 %cond_nz867, label %bb183, label %bb184

bb183:                                            ; preds = %bb182
  store i64 33, ptr %local_249, align 8
  %move_load868 = load i64, ptr %local_249, align 8
  store i64 %move_load868, ptr %return_slot, align 8
  %"hew_string_drop drop869" = load ptr, ptr %local_243, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop869")
  store ptr null, ptr %local_243, align 8
  %"hew_vec_free drop870" = load ptr, ptr %local_195, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop870")
  store ptr null, ptr %local_195, align 8
  %"hew_vec_free drop871" = load ptr, ptr %local_166, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop871")
  store ptr null, ptr %local_166, align 8
  %"hew_vec_free drop872" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop872")
  store ptr null, ptr %local_1, align 8
  %ret_val873 = load i64, ptr %return_slot, align 8
  ret i64 %ret_val873

bb184:                                            ; preds = %bb182
  br label %bb185

bb185:                                            ; preds = %after_cooperate886, %bb184
  %hew_actor_cooperate874 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel875 = icmp eq i32 %hew_actor_cooperate874, 2
  br i1 %hew_cooperate_is_cancel875, label %cancel_exit876, label %after_cooperate877

bb186:                                            ; No predecessors!
  %hew_actor_cooperate883 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel884 = icmp eq i32 %hew_actor_cooperate883, 2
  br i1 %hew_cooperate_is_cancel884, label %cancel_exit885, label %after_cooperate886

bb187:                                            ; preds = %bb165
  %"hew_string_drop drop887" = load ptr, ptr %local_243, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop887")
  store ptr null, ptr %local_243, align 8
  %"hew_vec_free drop888" = load ptr, ptr %local_195, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop888")
  store ptr null, ptr %local_195, align 8
  %"hew_vec_free drop889" = load ptr, ptr %local_166, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop889")
  store ptr null, ptr %local_166, align 8
  %"hew_vec_free drop890" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop890")
  store ptr null, ptr %local_1, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

cancel_exit:                                      ; preds = %entry
  ret i64 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit16:                                    ; preds = %bb7
  %"hew_vec_free drop18" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop18")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate17:                                ; preds = %bb7
  br label %bb2

cancel_exit23:                                    ; preds = %bb9
  %"hew_vec_free drop25" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop25")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate24:                                ; preds = %bb9
  br label %bb4

cancel_exit56:                                    ; preds = %bb17
  ret i64 0

after_cooperate57:                                ; preds = %bb17
  br label %bb16

cancel_exit92:                                    ; preds = %bb23
  %"hew_vec_free drop94" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop94")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate93:                                ; preds = %bb23
  br label %bb18

cancel_exit123:                                   ; preds = %bb32
  %"hew_vec_free drop125" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop125")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate124:                               ; preds = %bb32
  br label %bb20

cancel_exit128:                                   ; preds = %bb33
  ret i64 0

after_cooperate129:                               ; preds = %bb33
  br label %bb32

cancel_exit166:                                   ; preds = %bb40
  %"hew_vec_free drop168" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop168")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate167:                               ; preds = %bb40
  br label %bb35

cancel_exit189:                                   ; preds = %bb46
  ret i64 0

after_cooperate190:                               ; preds = %bb46
  br label %bb45

cancel_exit225:                                   ; preds = %bb54
  ret i64 0

after_cooperate226:                               ; preds = %bb54
  br label %bb53

cancel_exit261:                                   ; preds = %bb62
  ret i64 0

after_cooperate262:                               ; preds = %bb62
  br label %bb61

cancel_exit297:                                   ; preds = %bb70
  ret i64 0

after_cooperate298:                               ; preds = %bb70
  br label %bb69

cancel_exit333:                                   ; preds = %bb78
  ret i64 0

after_cooperate334:                               ; preds = %bb78
  br label %bb77

cancel_exit369:                                   ; preds = %bb86
  ret i64 0

after_cooperate370:                               ; preds = %bb86
  br label %bb85

cancel_exit405:                                   ; preds = %bb94
  ret i64 0

after_cooperate406:                               ; preds = %bb94
  br label %bb93

cancel_exit433:                                   ; preds = %bb101
  %"hew_vec_free drop435" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop435")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate434:                               ; preds = %bb101
  br label %bb37

cancel_exit438:                                   ; preds = %bb102
  ret i64 0

after_cooperate439:                               ; preds = %bb102
  br label %bb101

cancel_exit476:                                   ; preds = %bb109
  %"hew_vec_free drop478" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop478")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate477:                               ; preds = %bb109
  br label %bb107

cancel_exit492:                                   ; preds = %bb113
  ret i64 0

after_cooperate493:                               ; preds = %bb113
  br label %bb112

cancel_exit529:                                   ; preds = %bb119
  %"hew_vec_free drop531" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop531")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate530:                               ; preds = %bb119
  br label %bb117

cancel_exit538:                                   ; preds = %bb123
  ret i64 0

after_cooperate539:                               ; preds = %bb123
  br label %bb122

cancel_exit576:                                   ; preds = %bb130
  %"hew_vec_free drop578" = load ptr, ptr %local_166, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop578")
  store ptr null, ptr %local_166, align 8
  %"hew_vec_free drop579" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop579")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate577:                               ; preds = %bb130
  br label %bb125

cancel_exit614:                                   ; preds = %bb139
  %"hew_vec_free drop616" = load ptr, ptr %local_166, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop616")
  store ptr null, ptr %local_166, align 8
  %"hew_vec_free drop617" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop617")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate615:                               ; preds = %bb139
  br label %bb136

cancel_exit621:                                   ; preds = %bb141
  %"hew_vec_free drop623" = load ptr, ptr %local_166, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop623")
  store ptr null, ptr %local_166, align 8
  %"hew_vec_free drop624" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop624")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate622:                               ; preds = %bb141
  br label %bb136

cancel_exit628:                                   ; preds = %bb142
  %"hew_vec_free drop630" = load ptr, ptr %local_166, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop630")
  store ptr null, ptr %local_166, align 8
  %"hew_vec_free drop631" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop631")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate629:                               ; preds = %bb142
  br label %bb136

cancel_exit634:                                   ; preds = %bb143
  %"hew_string_drop drop636" = load ptr, ptr %local_192, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop636")
  store ptr null, ptr %local_192, align 8
  %"hew_vec_free drop637" = load ptr, ptr %local_166, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop637")
  store ptr null, ptr %local_166, align 8
  %"hew_vec_free drop638" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop638")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate635:                               ; preds = %bb143
  %"hew_string_drop drop639" = load ptr, ptr %local_192, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop639")
  store ptr null, ptr %local_192, align 8
  br label %bb127

cancel_exit687:                                   ; preds = %bb151
  %"hew_vec_free drop689" = load ptr, ptr %local_195, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop689")
  store ptr null, ptr %local_195, align 8
  %"hew_vec_free drop690" = load ptr, ptr %local_166, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop690")
  store ptr null, ptr %local_166, align 8
  %"hew_vec_free drop691" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop691")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate688:                               ; preds = %bb151
  br label %bb146

cancel_exit738:                                   ; preds = %bb160
  %"hew_vec_free drop740" = load ptr, ptr %local_195, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop740")
  store ptr null, ptr %local_195, align 8
  %"hew_vec_free drop741" = load ptr, ptr %local_166, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop741")
  store ptr null, ptr %local_166, align 8
  %"hew_vec_free drop742" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop742")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate739:                               ; preds = %bb160
  br label %bb148

cancel_exit745:                                   ; preds = %bb161
  ret i64 0

after_cooperate746:                               ; preds = %bb161
  br label %bb160

cancel_exit782:                                   ; preds = %bb168
  %"hew_vec_free drop784" = load ptr, ptr %local_195, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop784")
  store ptr null, ptr %local_195, align 8
  %"hew_vec_free drop785" = load ptr, ptr %local_166, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop785")
  store ptr null, ptr %local_166, align 8
  %"hew_vec_free drop786" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop786")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate783:                               ; preds = %bb168
  br label %bb163

cancel_exit832:                                   ; preds = %bb177
  %"hew_vec_free drop834" = load ptr, ptr %local_195, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop834")
  store ptr null, ptr %local_195, align 8
  %"hew_vec_free drop835" = load ptr, ptr %local_166, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop835")
  store ptr null, ptr %local_166, align 8
  %"hew_vec_free drop836" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop836")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate833:                               ; preds = %bb177
  br label %bb174

cancel_exit840:                                   ; preds = %bb179
  %"hew_vec_free drop842" = load ptr, ptr %local_195, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop842")
  store ptr null, ptr %local_195, align 8
  %"hew_vec_free drop843" = load ptr, ptr %local_166, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop843")
  store ptr null, ptr %local_166, align 8
  %"hew_vec_free drop844" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop844")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate841:                               ; preds = %bb179
  br label %bb174

cancel_exit848:                                   ; preds = %bb180
  %"hew_vec_free drop850" = load ptr, ptr %local_195, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop850")
  store ptr null, ptr %local_195, align 8
  %"hew_vec_free drop851" = load ptr, ptr %local_166, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop851")
  store ptr null, ptr %local_166, align 8
  %"hew_vec_free drop852" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop852")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate849:                               ; preds = %bb180
  br label %bb174

cancel_exit876:                                   ; preds = %bb185
  %"hew_string_drop drop878" = load ptr, ptr %local_243, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop878")
  store ptr null, ptr %local_243, align 8
  %"hew_vec_free drop879" = load ptr, ptr %local_195, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop879")
  store ptr null, ptr %local_195, align 8
  %"hew_vec_free drop880" = load ptr, ptr %local_166, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop880")
  store ptr null, ptr %local_166, align 8
  %"hew_vec_free drop881" = load ptr, ptr %local_1, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop881")
  store ptr null, ptr %local_1, align 8
  ret i64 0

after_cooperate877:                               ; preds = %bb185
  %"hew_string_drop drop882" = load ptr, ptr %local_243, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop882")
  store ptr null, ptr %local_243, align 8
  br label %bb165

cancel_exit885:                                   ; preds = %bb186
  ret i64 0

after_cooperate886:                               ; preds = %bb186
  br label %bb185
}

define i8 @main() {
entry:
  %return_slot = alloca i8, align 1
  %local_0 = alloca i64, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i8, align 1
  %local_3 = alloca i64, align 8
  %local_4 = alloca i8, align 1
  %local_5 = alloca ptr, align 8
  %local_6 = alloca ptr, align 8
  %local_7 = alloca ptr, align 8
  %local_8 = alloca i64, align 8
  %local_9 = alloca i64, align 8
  %local_10 = alloca ptr, align 8
  %local_11 = alloca ptr, align 8
  %local_12 = alloca ptr, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_result = call i64 @check_ints()
  store i64 %call_result, ptr %local_0, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load i64, ptr %local_0, align 8
  store i64 %move_load, ptr %local_1, align 8
  store i64 0, ptr %local_3, align 8
  %cmp_lhs = load i64, ptr %local_1, align 8
  %cmp_rhs = load i64, ptr %local_3, align 8
  %cmp_bit = icmp ne i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_4, align 1
  %cond_load = load i8, ptr %local_4, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  store ptr @str_lit.26, ptr %local_5, align 8
  %call_arg = load i64, ptr %local_1, align 8
  %call_result1 = call ptr @hew_i64_to_string(i64 %call_arg)
  store ptr %call_result1, ptr %local_6, align 8
  br label %bb5

bb3:                                              ; preds = %bb1
  br label %bb4

bb4:                                              ; preds = %after_cooperate10, %bb3
  %call_result2 = call i64 @check_strings()
  store i64 %call_result2, ptr %local_8, align 8
  br label %bb9

bb5:                                              ; preds = %bb2
  %call_arg3 = load ptr, ptr %local_5, align 8
  %call_arg4 = load ptr, ptr %local_6, align 8
  %call_result5 = call ptr @hew_string_concat(ptr %call_arg3, ptr %call_arg4)
  store ptr %call_result5, ptr %local_7, align 8
  br label %bb6

bb6:                                              ; preds = %bb5
  %"hew_string_drop drop" = load ptr, ptr %local_6, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop")
  store ptr null, ptr %local_6, align 8
  %print_arg = load ptr, ptr %local_7, align 8
  %print_str_bits = ptrtoint ptr %print_arg to i64
  call void @hew_print_value(i8 4, i64 %print_str_bits, i1 true)
  br label %bb7

bb7:                                              ; preds = %bb6
  %"hew_string_drop drop6" = load ptr, ptr %local_7, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop6")
  store ptr null, ptr %local_7, align 8
  %hew_lambda_drain_all_call = call i32 @hew_lambda_drain_all(i64 0)
  ret i8 0

bb8:                                              ; No predecessors!
  %hew_actor_cooperate7 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel8 = icmp eq i32 %hew_actor_cooperate7, 2
  br i1 %hew_cooperate_is_cancel8, label %cancel_exit9, label %after_cooperate10

bb9:                                              ; preds = %bb4
  %move_load11 = load i64, ptr %local_8, align 8
  store i64 %move_load11, ptr %local_9, align 8
  store ptr @str_lit.27, ptr %local_10, align 8
  %call_arg12 = load i64, ptr %local_9, align 8
  %call_result13 = call ptr @hew_i64_to_string(i64 %call_arg12)
  store ptr %call_result13, ptr %local_11, align 8
  br label %bb10

bb10:                                             ; preds = %bb9
  %call_arg14 = load ptr, ptr %local_10, align 8
  %call_arg15 = load ptr, ptr %local_11, align 8
  %call_result16 = call ptr @hew_string_concat(ptr %call_arg14, ptr %call_arg15)
  store ptr %call_result16, ptr %local_12, align 8
  br label %bb11

bb11:                                             ; preds = %bb10
  %"hew_string_drop drop17" = load ptr, ptr %local_11, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop17")
  store ptr null, ptr %local_11, align 8
  %print_arg18 = load ptr, ptr %local_12, align 8
  %print_str_bits19 = ptrtoint ptr %print_arg18 to i64
  call void @hew_print_value(i8 4, i64 %print_str_bits19, i1 true)
  br label %bb12

bb12:                                             ; preds = %bb11
  %"hew_string_drop drop20" = load ptr, ptr %local_12, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop20")
  store ptr null, ptr %local_12, align 8
  %hew_lambda_drain_all_call21 = call i32 @hew_lambda_drain_all(i64 0)
  ret i8 0

cancel_exit:                                      ; preds = %entry
  ret i8 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit9:                                     ; preds = %bb8
  ret i8 0

after_cooperate10:                                ; preds = %bb8
  br label %bb4
}

define internal ptr @"sort$copy_ints"(ptr %0) {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca ptr, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i64, align 8
  %local_5 = alloca i64, align 8
  %local_6 = alloca i64, align 8
  %local_7 = alloca i64, align 8
  %local_8 = alloca i8, align 1
  %local_9 = alloca i64, align 8
  %local_10 = alloca i64, align 8
  %local_11 = alloca i8, align 1
  %local_12 = alloca i64, align 8
  %local_13 = alloca i8, align 1
  %local_14 = alloca i64, align 8
  %local_15 = alloca i8, align 1
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %hew_vec_new_i64_call = call ptr @hew_vec_new_i64()
  store ptr %hew_vec_new_i64_call, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %local_2, align 8
  store i64 1, ptr %local_5, align 8
  %move_load1 = load i64, ptr %local_5, align 8
  store i64 %move_load1, ptr %local_6, align 8
  store i64 0, ptr %local_7, align 8
  %cmp_lhs = load i64, ptr %local_6, align 8
  %cmp_rhs = load i64, ptr %local_7, align 8
  %cmp_bit = icmp sle i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_8, align 1
  %cond_load = load i8, ptr %local_8, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb6, label %bb7

bb2:                                              ; preds = %after_cooperate22, %bb4
  %cmp_lhs2 = load i64, ptr %local_3, align 8
  %cmp_rhs3 = load i64, ptr %local_4, align 8
  %cmp_bit4 = icmp slt i64 %cmp_lhs2, %cmp_rhs3
  %cmp_zext5 = zext i1 %cmp_bit4 to i8
  store i8 %cmp_zext5, ptr %local_11, align 1
  %cond_load6 = load i8, ptr %local_11, align 1
  %cond_nz7 = icmp ne i8 %cond_load6, 0
  br i1 %cond_nz7, label %bb3, label %bb5

bb3:                                              ; preds = %bb2
  %"hew_vec_len arg0" = load ptr, ptr %local_0, align 8
  %hew_vec_len_call = call i64 @hew_vec_len(ptr %"hew_vec_len arg0")
  store i64 %hew_vec_len_call, ptr %local_12, align 8
  %cmp_lhs8 = load i64, ptr %local_3, align 8
  %cmp_rhs9 = load i64, ptr %local_12, align 8
  %cmp_bit10 = icmp uge i64 %cmp_lhs8, %cmp_rhs9
  %cmp_zext11 = zext i1 %cmp_bit10 to i8
  store i8 %cmp_zext11, ptr %local_13, align 1
  %cond_load12 = load i8, ptr %local_13, align 1
  %cond_nz13 = icmp ne i8 %cond_load12, 0
  br i1 %cond_nz13, label %bb9, label %bb10

bb4:                                              ; preds = %after_cooperate28
  %checked_lhs = load i64, ptr %local_3, align 8
  %checked_rhs = load i64, ptr %local_6, align 8
  %with_overflow = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_3, align 8
  store i8 %checked_overflow_widen, ptr %local_15, align 1
  %cond_load14 = load i8, ptr %local_15, align 1
  %cond_nz15 = icmp ne i8 %cond_load14, 0
  br i1 %cond_nz15, label %bb12, label %bb2

bb5:                                              ; preds = %bb2
  %move_load16 = load ptr, ptr %local_2, align 8
  store ptr %move_load16, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

bb6:                                              ; preds = %bb1
  call void @hew_trap_with_code(i32 202)
  call void @llvm.trap()
  unreachable

bb7:                                              ; preds = %bb1
  store i64 0, ptr %local_9, align 8
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result = call i64 @hew_vec_len(ptr %call_arg)
  store i64 %call_result, ptr %local_10, align 8
  br label %bb8

bb8:                                              ; preds = %bb7
  %move_load17 = load i64, ptr %local_9, align 8
  store i64 %move_load17, ptr %local_3, align 8
  %move_load18 = load i64, ptr %local_10, align 8
  store i64 %move_load18, ptr %local_4, align 8
  %hew_actor_cooperate19 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel20 = icmp eq i32 %hew_actor_cooperate19, 2
  br i1 %hew_cooperate_is_cancel20, label %cancel_exit21, label %after_cooperate22

bb9:                                              ; preds = %bb3
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb10:                                             ; preds = %bb3
  %"hew_vec_get_i64 arg0" = load ptr, ptr %local_0, align 8
  %"hew_vec_get_i64 arg1" = load i64, ptr %local_3, align 8
  %hew_vec_get_i64_call = call i64 @hew_vec_get_i64(ptr %"hew_vec_get_i64 arg0", i64 %"hew_vec_get_i64 arg1")
  store i64 %hew_vec_get_i64_call, ptr %local_14, align 8
  %call_arg23 = load ptr, ptr %local_2, align 8
  %call_arg24 = load i64, ptr %local_14, align 8
  call void @hew_vec_push_i64(ptr %call_arg23, i64 %call_arg24)
  br label %bb11

bb11:                                             ; preds = %bb10
  %hew_actor_cooperate25 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel26 = icmp eq i32 %hew_actor_cooperate25, 2
  br i1 %hew_cooperate_is_cancel26, label %cancel_exit27, label %after_cooperate28

bb12:                                             ; preds = %bb4
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit21:                                    ; preds = %bb8
  ret ptr null

after_cooperate22:                                ; preds = %bb8
  br label %bb2

cancel_exit27:                                    ; preds = %bb11
  ret ptr null

after_cooperate28:                                ; preds = %bb11
  br label %bb4
}

define internal ptr @"sort$copy_strings"(ptr %0) {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result = call ptr @hew_vec_clone(ptr %call_arg)
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

define internal ptr @"sort$copy_floats"(ptr %0) {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca ptr, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i64, align 8
  %local_5 = alloca i64, align 8
  %local_6 = alloca i64, align 8
  %local_7 = alloca i64, align 8
  %local_8 = alloca i8, align 1
  %local_9 = alloca i64, align 8
  %local_10 = alloca i64, align 8
  %local_11 = alloca i8, align 1
  %local_12 = alloca i64, align 8
  %local_13 = alloca i8, align 1
  %local_14 = alloca double, align 8
  %local_15 = alloca i8, align 1
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %hew_vec_new_f64_call = call ptr @hew_vec_new_f64()
  store ptr %hew_vec_new_f64_call, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %local_2, align 8
  store i64 1, ptr %local_5, align 8
  %move_load1 = load i64, ptr %local_5, align 8
  store i64 %move_load1, ptr %local_6, align 8
  store i64 0, ptr %local_7, align 8
  %cmp_lhs = load i64, ptr %local_6, align 8
  %cmp_rhs = load i64, ptr %local_7, align 8
  %cmp_bit = icmp sle i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_8, align 1
  %cond_load = load i8, ptr %local_8, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb6, label %bb7

bb2:                                              ; preds = %after_cooperate22, %bb4
  %cmp_lhs2 = load i64, ptr %local_3, align 8
  %cmp_rhs3 = load i64, ptr %local_4, align 8
  %cmp_bit4 = icmp slt i64 %cmp_lhs2, %cmp_rhs3
  %cmp_zext5 = zext i1 %cmp_bit4 to i8
  store i8 %cmp_zext5, ptr %local_11, align 1
  %cond_load6 = load i8, ptr %local_11, align 1
  %cond_nz7 = icmp ne i8 %cond_load6, 0
  br i1 %cond_nz7, label %bb3, label %bb5

bb3:                                              ; preds = %bb2
  %"hew_vec_len arg0" = load ptr, ptr %local_0, align 8
  %hew_vec_len_call = call i64 @hew_vec_len(ptr %"hew_vec_len arg0")
  store i64 %hew_vec_len_call, ptr %local_12, align 8
  %cmp_lhs8 = load i64, ptr %local_3, align 8
  %cmp_rhs9 = load i64, ptr %local_12, align 8
  %cmp_bit10 = icmp uge i64 %cmp_lhs8, %cmp_rhs9
  %cmp_zext11 = zext i1 %cmp_bit10 to i8
  store i8 %cmp_zext11, ptr %local_13, align 1
  %cond_load12 = load i8, ptr %local_13, align 1
  %cond_nz13 = icmp ne i8 %cond_load12, 0
  br i1 %cond_nz13, label %bb9, label %bb10

bb4:                                              ; preds = %after_cooperate28
  %checked_lhs = load i64, ptr %local_3, align 8
  %checked_rhs = load i64, ptr %local_6, align 8
  %with_overflow = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_3, align 8
  store i8 %checked_overflow_widen, ptr %local_15, align 1
  %cond_load14 = load i8, ptr %local_15, align 1
  %cond_nz15 = icmp ne i8 %cond_load14, 0
  br i1 %cond_nz15, label %bb12, label %bb2

bb5:                                              ; preds = %bb2
  %move_load16 = load ptr, ptr %local_2, align 8
  store ptr %move_load16, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

bb6:                                              ; preds = %bb1
  call void @hew_trap_with_code(i32 202)
  call void @llvm.trap()
  unreachable

bb7:                                              ; preds = %bb1
  store i64 0, ptr %local_9, align 8
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result = call i64 @hew_vec_len(ptr %call_arg)
  store i64 %call_result, ptr %local_10, align 8
  br label %bb8

bb8:                                              ; preds = %bb7
  %move_load17 = load i64, ptr %local_9, align 8
  store i64 %move_load17, ptr %local_3, align 8
  %move_load18 = load i64, ptr %local_10, align 8
  store i64 %move_load18, ptr %local_4, align 8
  %hew_actor_cooperate19 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel20 = icmp eq i32 %hew_actor_cooperate19, 2
  br i1 %hew_cooperate_is_cancel20, label %cancel_exit21, label %after_cooperate22

bb9:                                              ; preds = %bb3
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb10:                                             ; preds = %bb3
  %"hew_vec_get_f64 arg0" = load ptr, ptr %local_0, align 8
  %"hew_vec_get_f64 arg1" = load i64, ptr %local_3, align 8
  %hew_vec_get_f64_call = call double @hew_vec_get_f64(ptr %"hew_vec_get_f64 arg0", i64 %"hew_vec_get_f64 arg1")
  store double %hew_vec_get_f64_call, ptr %local_14, align 8
  %call_arg23 = load ptr, ptr %local_2, align 8
  %call_arg24 = load double, ptr %local_14, align 8
  call void @hew_vec_push_f64(ptr %call_arg23, double %call_arg24)
  br label %bb11

bb11:                                             ; preds = %bb10
  %hew_actor_cooperate25 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel26 = icmp eq i32 %hew_actor_cooperate25, 2
  br i1 %hew_cooperate_is_cancel26, label %cancel_exit27, label %after_cooperate28

bb12:                                             ; preds = %bb4
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit21:                                    ; preds = %bb8
  ret ptr null

after_cooperate22:                                ; preds = %bb8
  br label %bb2

cancel_exit27:                                    ; preds = %bb11
  ret ptr null

after_cooperate28:                                ; preds = %bb11
  br label %bb4
}

define internal i8 @"sort$swap_ints"(ptr %0, i64 %1, i64 %2) {
entry:
  %return_slot = alloca i8, align 1
  %local_0 = alloca ptr, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i8, align 1
  %local_5 = alloca i64, align 8
  %local_6 = alloca i64, align 8
  %local_7 = alloca i64, align 8
  %local_8 = alloca i8, align 1
  %local_9 = alloca i64, align 8
  store ptr %0, ptr %local_0, align 8
  store i64 %1, ptr %local_1, align 8
  store i64 %2, ptr %local_2, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %"hew_vec_len arg0" = load ptr, ptr %local_0, align 8
  %hew_vec_len_call = call i64 @hew_vec_len(ptr %"hew_vec_len arg0")
  store i64 %hew_vec_len_call, ptr %local_3, align 8
  %cmp_lhs = load i64, ptr %local_1, align 8
  %cmp_rhs = load i64, ptr %local_3, align 8
  %cmp_bit = icmp uge i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_4, align 1
  %cond_load = load i8, ptr %local_4, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb2:                                              ; preds = %bb0
  %"hew_vec_get_i64 arg0" = load ptr, ptr %local_0, align 8
  %"hew_vec_get_i64 arg1" = load i64, ptr %local_1, align 8
  %hew_vec_get_i64_call = call i64 @hew_vec_get_i64(ptr %"hew_vec_get_i64 arg0", i64 %"hew_vec_get_i64 arg1")
  store i64 %hew_vec_get_i64_call, ptr %local_5, align 8
  %move_load = load i64, ptr %local_5, align 8
  store i64 %move_load, ptr %local_6, align 8
  %"hew_vec_len arg01" = load ptr, ptr %local_0, align 8
  %hew_vec_len_call2 = call i64 @hew_vec_len(ptr %"hew_vec_len arg01")
  store i64 %hew_vec_len_call2, ptr %local_7, align 8
  %cmp_lhs3 = load i64, ptr %local_2, align 8
  %cmp_rhs4 = load i64, ptr %local_7, align 8
  %cmp_bit5 = icmp uge i64 %cmp_lhs3, %cmp_rhs4
  %cmp_zext6 = zext i1 %cmp_bit5 to i8
  store i8 %cmp_zext6, ptr %local_8, align 1
  %cond_load7 = load i8, ptr %local_8, align 1
  %cond_nz8 = icmp ne i8 %cond_load7, 0
  br i1 %cond_nz8, label %bb3, label %bb4

bb3:                                              ; preds = %bb2
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb4:                                              ; preds = %bb2
  %"hew_vec_get_i64 arg09" = load ptr, ptr %local_0, align 8
  %"hew_vec_get_i64 arg110" = load i64, ptr %local_2, align 8
  %hew_vec_get_i64_call11 = call i64 @hew_vec_get_i64(ptr %"hew_vec_get_i64 arg09", i64 %"hew_vec_get_i64 arg110")
  store i64 %hew_vec_get_i64_call11, ptr %local_9, align 8
  %call_arg = load ptr, ptr %local_0, align 8
  %call_arg12 = load i64, ptr %local_1, align 8
  %call_arg13 = load i64, ptr %local_9, align 8
  call void @hew_vec_set_i64(ptr %call_arg, i64 %call_arg12, i64 %call_arg13)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_arg14 = load ptr, ptr %local_0, align 8
  %call_arg15 = load i64, ptr %local_2, align 8
  %call_arg16 = load i64, ptr %local_6, align 8
  call void @hew_vec_set_i64(ptr %call_arg14, i64 %call_arg15, i64 %call_arg16)
  br label %bb6

bb6:                                              ; preds = %bb5
  ret i8 0

cancel_exit:                                      ; preds = %entry
  ret i8 0

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal i8 @"sort$swap_strings"(ptr %0, i64 %1, i64 %2) {
entry:
  %return_slot = alloca i8, align 1
  %local_0 = alloca ptr, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i8, align 1
  %local_5 = alloca ptr, align 8
  %local_6 = alloca ptr, align 8
  %local_7 = alloca i64, align 8
  %local_8 = alloca i8, align 1
  %local_9 = alloca ptr, align 8
  store ptr %0, ptr %local_0, align 8
  store i64 %1, ptr %local_1, align 8
  store i64 %2, ptr %local_2, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %"hew_vec_len arg0" = load ptr, ptr %local_0, align 8
  %hew_vec_len_call = call i64 @hew_vec_len(ptr %"hew_vec_len arg0")
  store i64 %hew_vec_len_call, ptr %local_3, align 8
  %cmp_lhs = load i64, ptr %local_1, align 8
  %cmp_rhs = load i64, ptr %local_3, align 8
  %cmp_bit = icmp uge i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_4, align 1
  %cond_load = load i8, ptr %local_4, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb2:                                              ; preds = %bb0
  %"hew_vec_get_str arg0" = load ptr, ptr %local_0, align 8
  %"hew_vec_get_str arg1" = load i64, ptr %local_1, align 8
  %hew_vec_get_str_call = call ptr @hew_vec_get_str(ptr %"hew_vec_get_str arg0", i64 %"hew_vec_get_str arg1")
  store ptr %hew_vec_get_str_call, ptr %local_5, align 8
  %move_load = load ptr, ptr %local_5, align 8
  store ptr %move_load, ptr %local_6, align 8
  %"hew_vec_len arg01" = load ptr, ptr %local_0, align 8
  %hew_vec_len_call2 = call i64 @hew_vec_len(ptr %"hew_vec_len arg01")
  store i64 %hew_vec_len_call2, ptr %local_7, align 8
  %cmp_lhs3 = load i64, ptr %local_2, align 8
  %cmp_rhs4 = load i64, ptr %local_7, align 8
  %cmp_bit5 = icmp uge i64 %cmp_lhs3, %cmp_rhs4
  %cmp_zext6 = zext i1 %cmp_bit5 to i8
  store i8 %cmp_zext6, ptr %local_8, align 1
  %cond_load7 = load i8, ptr %local_8, align 1
  %cond_nz8 = icmp ne i8 %cond_load7, 0
  br i1 %cond_nz8, label %bb3, label %bb4

bb3:                                              ; preds = %bb2
  %"hew_string_drop drop" = load ptr, ptr %local_6, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop")
  store ptr null, ptr %local_6, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb4:                                              ; preds = %bb2
  %"hew_vec_get_str arg09" = load ptr, ptr %local_0, align 8
  %"hew_vec_get_str arg110" = load i64, ptr %local_2, align 8
  %hew_vec_get_str_call11 = call ptr @hew_vec_get_str(ptr %"hew_vec_get_str arg09", i64 %"hew_vec_get_str arg110")
  store ptr %hew_vec_get_str_call11, ptr %local_9, align 8
  %call_arg = load ptr, ptr %local_0, align 8
  %call_arg12 = load i64, ptr %local_1, align 8
  %call_arg13 = load ptr, ptr %local_9, align 8
  call void @hew_vec_set_str(ptr %call_arg, i64 %call_arg12, ptr %call_arg13)
  br label %bb5

bb5:                                              ; preds = %bb4
  %"hew_string_drop drop14" = load ptr, ptr %local_9, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop14")
  store ptr null, ptr %local_9, align 8
  %call_arg15 = load ptr, ptr %local_0, align 8
  %call_arg16 = load i64, ptr %local_2, align 8
  %call_arg17 = load ptr, ptr %local_6, align 8
  call void @hew_vec_set_str(ptr %call_arg15, i64 %call_arg16, ptr %call_arg17)
  br label %bb6

bb6:                                              ; preds = %bb5
  %"hew_string_drop drop18" = load ptr, ptr %local_6, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop18")
  store ptr null, ptr %local_6, align 8
  ret i8 0

cancel_exit:                                      ; preds = %entry
  ret i8 0

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal i8 @"sort$swap_floats"(ptr %0, i64 %1, i64 %2) {
entry:
  %return_slot = alloca i8, align 1
  %local_0 = alloca ptr, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i8, align 1
  %local_5 = alloca double, align 8
  %local_6 = alloca double, align 8
  %local_7 = alloca i64, align 8
  %local_8 = alloca i8, align 1
  %local_9 = alloca double, align 8
  store ptr %0, ptr %local_0, align 8
  store i64 %1, ptr %local_1, align 8
  store i64 %2, ptr %local_2, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %"hew_vec_len arg0" = load ptr, ptr %local_0, align 8
  %hew_vec_len_call = call i64 @hew_vec_len(ptr %"hew_vec_len arg0")
  store i64 %hew_vec_len_call, ptr %local_3, align 8
  %cmp_lhs = load i64, ptr %local_1, align 8
  %cmp_rhs = load i64, ptr %local_3, align 8
  %cmp_bit = icmp uge i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_4, align 1
  %cond_load = load i8, ptr %local_4, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb2:                                              ; preds = %bb0
  %"hew_vec_get_f64 arg0" = load ptr, ptr %local_0, align 8
  %"hew_vec_get_f64 arg1" = load i64, ptr %local_1, align 8
  %hew_vec_get_f64_call = call double @hew_vec_get_f64(ptr %"hew_vec_get_f64 arg0", i64 %"hew_vec_get_f64 arg1")
  store double %hew_vec_get_f64_call, ptr %local_5, align 8
  %move_load = load double, ptr %local_5, align 8
  store double %move_load, ptr %local_6, align 8
  %"hew_vec_len arg01" = load ptr, ptr %local_0, align 8
  %hew_vec_len_call2 = call i64 @hew_vec_len(ptr %"hew_vec_len arg01")
  store i64 %hew_vec_len_call2, ptr %local_7, align 8
  %cmp_lhs3 = load i64, ptr %local_2, align 8
  %cmp_rhs4 = load i64, ptr %local_7, align 8
  %cmp_bit5 = icmp uge i64 %cmp_lhs3, %cmp_rhs4
  %cmp_zext6 = zext i1 %cmp_bit5 to i8
  store i8 %cmp_zext6, ptr %local_8, align 1
  %cond_load7 = load i8, ptr %local_8, align 1
  %cond_nz8 = icmp ne i8 %cond_load7, 0
  br i1 %cond_nz8, label %bb3, label %bb4

bb3:                                              ; preds = %bb2
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb4:                                              ; preds = %bb2
  %"hew_vec_get_f64 arg09" = load ptr, ptr %local_0, align 8
  %"hew_vec_get_f64 arg110" = load i64, ptr %local_2, align 8
  %hew_vec_get_f64_call11 = call double @hew_vec_get_f64(ptr %"hew_vec_get_f64 arg09", i64 %"hew_vec_get_f64 arg110")
  store double %hew_vec_get_f64_call11, ptr %local_9, align 8
  %call_arg = load ptr, ptr %local_0, align 8
  %call_arg12 = load i64, ptr %local_1, align 8
  %call_arg13 = load double, ptr %local_9, align 8
  call void @hew_vec_set_f64(ptr %call_arg, i64 %call_arg12, double %call_arg13)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_arg14 = load ptr, ptr %local_0, align 8
  %call_arg15 = load i64, ptr %local_2, align 8
  %call_arg16 = load double, ptr %local_6, align 8
  call void @hew_vec_set_f64(ptr %call_arg14, i64 %call_arg15, double %call_arg16)
  br label %bb6

bb6:                                              ; preds = %bb5
  ret i8 0

cancel_exit:                                      ; preds = %entry
  ret i8 0

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal i64 @"sort$merge_int_run"(ptr %0, ptr %1, i64 %2, i64 %3, i64 %4) {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i64, align 8
  %local_5 = alloca i64, align 8
  %local_6 = alloca i64, align 8
  %local_7 = alloca i64, align 8
  %local_8 = alloca i64, align 8
  %local_9 = alloca i64, align 8
  %local_10 = alloca i8, align 1
  %local_11 = alloca i8, align 1
  %local_12 = alloca i8, align 1
  %local_13 = alloca i64, align 8
  %local_14 = alloca i64, align 8
  %local_15 = alloca i8, align 1
  %local_16 = alloca i64, align 8
  %local_17 = alloca i8, align 1
  %local_18 = alloca i64, align 8
  %local_19 = alloca i64, align 8
  %local_20 = alloca i64, align 8
  %local_21 = alloca i8, align 1
  %local_22 = alloca i64, align 8
  %local_23 = alloca i64, align 8
  %local_24 = alloca i8, align 1
  %local_25 = alloca i8, align 1
  %local_26 = alloca i64, align 8
  %local_27 = alloca i64, align 8
  %local_28 = alloca i8, align 1
  %local_29 = alloca i64, align 8
  %local_30 = alloca i64, align 8
  %local_31 = alloca i8, align 1
  %local_32 = alloca i64, align 8
  %local_33 = alloca i64, align 8
  %local_34 = alloca i8, align 1
  %local_35 = alloca i8, align 1
  %local_36 = alloca i64, align 8
  %local_37 = alloca i8, align 1
  %local_38 = alloca i64, align 8
  %local_39 = alloca i64, align 8
  %local_40 = alloca i64, align 8
  %local_41 = alloca i64, align 8
  %local_42 = alloca i8, align 1
  %local_43 = alloca i64, align 8
  %local_44 = alloca i64, align 8
  %local_45 = alloca i8, align 1
  %local_46 = alloca i8, align 1
  %local_47 = alloca i64, align 8
  %local_48 = alloca i8, align 1
  %local_49 = alloca i64, align 8
  %local_50 = alloca i64, align 8
  %local_51 = alloca i64, align 8
  %local_52 = alloca i64, align 8
  %local_53 = alloca i8, align 1
  %local_54 = alloca i64, align 8
  %local_55 = alloca i64, align 8
  %local_56 = alloca i8, align 1
  store ptr %0, ptr %local_0, align 8
  store ptr %1, ptr %local_1, align 8
  store i64 %2, ptr %local_2, align 8
  store i64 %3, ptr %local_3, align 8
  store i64 %4, ptr %local_4, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %move_load = load i64, ptr %local_2, align 8
  store i64 %move_load, ptr %local_5, align 8
  %move_load1 = load i64, ptr %local_3, align 8
  store i64 %move_load1, ptr %local_6, align 8
  %move_load2 = load i64, ptr %local_2, align 8
  store i64 %move_load2, ptr %local_7, align 8
  store i64 0, ptr %local_8, align 8
  %move_load3 = load i64, ptr %local_8, align 8
  store i64 %move_load3, ptr %local_9, align 8
  br label %bb1

bb1:                                              ; preds = %after_cooperate82, %bb0
  store i8 0, ptr %local_10, align 1
  %cmp_lhs = load i64, ptr %local_5, align 8
  %cmp_rhs = load i64, ptr %local_3, align 8
  %cmp_bit = icmp slt i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_11, align 1
  %cond_load = load i8, ptr %local_11, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb4, label %bb5

bb2:                                              ; preds = %bb5
  store i64 1, ptr %local_13, align 8
  %checked_lhs = load i64, ptr %local_9, align 8
  %checked_rhs = load i64, ptr %local_13, align 8
  %with_overflow = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_14, align 8
  store i8 %checked_overflow_widen, ptr %local_15, align 1
  %cond_load4 = load i8, ptr %local_15, align 1
  %cond_nz5 = icmp ne i8 %cond_load4, 0
  br i1 %cond_nz5, label %bb6, label %bb7

bb3:                                              ; preds = %bb5
  br label %bb23

bb4:                                              ; preds = %bb1
  %cmp_lhs6 = load i64, ptr %local_6, align 8
  %cmp_rhs7 = load i64, ptr %local_4, align 8
  %cmp_bit8 = icmp slt i64 %cmp_lhs6, %cmp_rhs7
  %cmp_zext9 = zext i1 %cmp_bit8 to i8
  store i8 %cmp_zext9, ptr %local_12, align 1
  %move_load10 = load i8, ptr %local_12, align 1
  store i8 %move_load10, ptr %local_10, align 1
  br label %bb5

bb5:                                              ; preds = %bb4, %bb1
  %cond_load11 = load i8, ptr %local_10, align 1
  %cond_nz12 = icmp ne i8 %cond_load11, 0
  br i1 %cond_nz12, label %bb2, label %bb3

bb6:                                              ; preds = %bb2
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb7:                                              ; preds = %bb2
  %move_load13 = load i64, ptr %local_14, align 8
  store i64 %move_load13, ptr %local_9, align 8
  %"hew_vec_len arg0" = load ptr, ptr %local_0, align 8
  %hew_vec_len_call = call i64 @hew_vec_len(ptr %"hew_vec_len arg0")
  store i64 %hew_vec_len_call, ptr %local_16, align 8
  %cmp_lhs14 = load i64, ptr %local_5, align 8
  %cmp_rhs15 = load i64, ptr %local_16, align 8
  %cmp_bit16 = icmp uge i64 %cmp_lhs14, %cmp_rhs15
  %cmp_zext17 = zext i1 %cmp_bit16 to i8
  store i8 %cmp_zext17, ptr %local_17, align 1
  %cond_load18 = load i8, ptr %local_17, align 1
  %cond_nz19 = icmp ne i8 %cond_load18, 0
  br i1 %cond_nz19, label %bb8, label %bb9

bb8:                                              ; preds = %bb7
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb9:                                              ; preds = %bb7
  %"hew_vec_get_i64 arg0" = load ptr, ptr %local_0, align 8
  %"hew_vec_get_i64 arg1" = load i64, ptr %local_5, align 8
  %hew_vec_get_i64_call = call i64 @hew_vec_get_i64(ptr %"hew_vec_get_i64 arg0", i64 %"hew_vec_get_i64 arg1")
  store i64 %hew_vec_get_i64_call, ptr %local_18, align 8
  %move_load20 = load i64, ptr %local_18, align 8
  store i64 %move_load20, ptr %local_19, align 8
  %"hew_vec_len arg021" = load ptr, ptr %local_0, align 8
  %hew_vec_len_call22 = call i64 @hew_vec_len(ptr %"hew_vec_len arg021")
  store i64 %hew_vec_len_call22, ptr %local_20, align 8
  %cmp_lhs23 = load i64, ptr %local_6, align 8
  %cmp_rhs24 = load i64, ptr %local_20, align 8
  %cmp_bit25 = icmp uge i64 %cmp_lhs23, %cmp_rhs24
  %cmp_zext26 = zext i1 %cmp_bit25 to i8
  store i8 %cmp_zext26, ptr %local_21, align 1
  %cond_load27 = load i8, ptr %local_21, align 1
  %cond_nz28 = icmp ne i8 %cond_load27, 0
  br i1 %cond_nz28, label %bb10, label %bb11

bb10:                                             ; preds = %bb9
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb11:                                             ; preds = %bb9
  %"hew_vec_get_i64 arg029" = load ptr, ptr %local_0, align 8
  %"hew_vec_get_i64 arg130" = load i64, ptr %local_6, align 8
  %hew_vec_get_i64_call31 = call i64 @hew_vec_get_i64(ptr %"hew_vec_get_i64 arg029", i64 %"hew_vec_get_i64 arg130")
  store i64 %hew_vec_get_i64_call31, ptr %local_22, align 8
  %move_load32 = load i64, ptr %local_22, align 8
  store i64 %move_load32, ptr %local_23, align 8
  %cmp_lhs33 = load i64, ptr %local_23, align 8
  %cmp_rhs34 = load i64, ptr %local_19, align 8
  %cmp_bit35 = icmp slt i64 %cmp_lhs33, %cmp_rhs34
  %cmp_zext36 = zext i1 %cmp_bit35 to i8
  store i8 %cmp_zext36, ptr %local_25, align 1
  %cond_load37 = load i8, ptr %local_25, align 1
  %cond_nz38 = icmp ne i8 %cond_load37, 0
  br i1 %cond_nz38, label %bb12, label %bb13

bb12:                                             ; preds = %bb11
  %call_arg = load ptr, ptr %local_1, align 8
  %call_arg39 = load i64, ptr %local_7, align 8
  %call_arg40 = load i64, ptr %local_23, align 8
  call void @hew_vec_set_i64(ptr %call_arg, i64 %call_arg39, i64 %call_arg40)
  br label %bb15

bb13:                                             ; preds = %bb11
  %call_arg41 = load ptr, ptr %local_1, align 8
  %call_arg42 = load i64, ptr %local_7, align 8
  %call_arg43 = load i64, ptr %local_19, align 8
  call void @hew_vec_set_i64(ptr %call_arg41, i64 %call_arg42, i64 %call_arg43)
  br label %bb18

bb14:                                             ; preds = %after_cooperate77, %after_cooperate64
  store i64 1, ptr %local_32, align 8
  %checked_lhs44 = load i64, ptr %local_7, align 8
  %checked_rhs45 = load i64, ptr %local_32, align 8
  %with_overflow46 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs44, i64 %checked_rhs45)
  %checked_result47 = extractvalue { i64, i1 } %with_overflow46, 0
  %checked_overflow48 = extractvalue { i64, i1 } %with_overflow46, 1
  %checked_overflow_widen49 = zext i1 %checked_overflow48 to i8
  store i64 %checked_result47, ptr %local_33, align 8
  store i8 %checked_overflow_widen49, ptr %local_34, align 1
  %cond_load50 = load i8, ptr %local_34, align 1
  %cond_nz51 = icmp ne i8 %cond_load50, 0
  br i1 %cond_nz51, label %bb21, label %bb22

bb15:                                             ; preds = %bb12
  store i64 1, ptr %local_26, align 8
  %checked_lhs52 = load i64, ptr %local_6, align 8
  %checked_rhs53 = load i64, ptr %local_26, align 8
  %with_overflow54 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs52, i64 %checked_rhs53)
  %checked_result55 = extractvalue { i64, i1 } %with_overflow54, 0
  %checked_overflow56 = extractvalue { i64, i1 } %with_overflow54, 1
  %checked_overflow_widen57 = zext i1 %checked_overflow56 to i8
  store i64 %checked_result55, ptr %local_27, align 8
  store i8 %checked_overflow_widen57, ptr %local_28, align 1
  %cond_load58 = load i8, ptr %local_28, align 1
  %cond_nz59 = icmp ne i8 %cond_load58, 0
  br i1 %cond_nz59, label %bb16, label %bb17

bb16:                                             ; preds = %bb15
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb17:                                             ; preds = %bb15
  %move_load60 = load i64, ptr %local_27, align 8
  store i64 %move_load60, ptr %local_6, align 8
  %hew_actor_cooperate61 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel62 = icmp eq i32 %hew_actor_cooperate61, 2
  br i1 %hew_cooperate_is_cancel62, label %cancel_exit63, label %after_cooperate64

bb18:                                             ; preds = %bb13
  store i64 1, ptr %local_29, align 8
  %checked_lhs65 = load i64, ptr %local_5, align 8
  %checked_rhs66 = load i64, ptr %local_29, align 8
  %with_overflow67 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs65, i64 %checked_rhs66)
  %checked_result68 = extractvalue { i64, i1 } %with_overflow67, 0
  %checked_overflow69 = extractvalue { i64, i1 } %with_overflow67, 1
  %checked_overflow_widen70 = zext i1 %checked_overflow69 to i8
  store i64 %checked_result68, ptr %local_30, align 8
  store i8 %checked_overflow_widen70, ptr %local_31, align 1
  %cond_load71 = load i8, ptr %local_31, align 1
  %cond_nz72 = icmp ne i8 %cond_load71, 0
  br i1 %cond_nz72, label %bb19, label %bb20

bb19:                                             ; preds = %bb18
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb20:                                             ; preds = %bb18
  %move_load73 = load i64, ptr %local_30, align 8
  store i64 %move_load73, ptr %local_5, align 8
  %hew_actor_cooperate74 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel75 = icmp eq i32 %hew_actor_cooperate74, 2
  br i1 %hew_cooperate_is_cancel75, label %cancel_exit76, label %after_cooperate77

bb21:                                             ; preds = %bb14
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb22:                                             ; preds = %bb14
  %move_load78 = load i64, ptr %local_33, align 8
  store i64 %move_load78, ptr %local_7, align 8
  %hew_actor_cooperate79 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel80 = icmp eq i32 %hew_actor_cooperate79, 2
  br i1 %hew_cooperate_is_cancel80, label %cancel_exit81, label %after_cooperate82

bb23:                                             ; preds = %after_cooperate125, %bb3
  %cmp_lhs83 = load i64, ptr %local_5, align 8
  %cmp_rhs84 = load i64, ptr %local_3, align 8
  %cmp_bit85 = icmp slt i64 %cmp_lhs83, %cmp_rhs84
  %cmp_zext86 = zext i1 %cmp_bit85 to i8
  store i8 %cmp_zext86, ptr %local_35, align 1
  %cond_load87 = load i8, ptr %local_35, align 1
  %cond_nz88 = icmp ne i8 %cond_load87, 0
  br i1 %cond_nz88, label %bb24, label %bb25

bb24:                                             ; preds = %bb23
  %"hew_vec_len arg089" = load ptr, ptr %local_0, align 8
  %hew_vec_len_call90 = call i64 @hew_vec_len(ptr %"hew_vec_len arg089")
  store i64 %hew_vec_len_call90, ptr %local_36, align 8
  %cmp_lhs91 = load i64, ptr %local_5, align 8
  %cmp_rhs92 = load i64, ptr %local_36, align 8
  %cmp_bit93 = icmp uge i64 %cmp_lhs91, %cmp_rhs92
  %cmp_zext94 = zext i1 %cmp_bit93 to i8
  store i8 %cmp_zext94, ptr %local_37, align 1
  %cond_load95 = load i8, ptr %local_37, align 1
  %cond_nz96 = icmp ne i8 %cond_load95, 0
  br i1 %cond_nz96, label %bb26, label %bb27

bb25:                                             ; preds = %bb23
  br label %bb33

bb26:                                             ; preds = %bb24
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb27:                                             ; preds = %bb24
  %"hew_vec_get_i64 arg097" = load ptr, ptr %local_0, align 8
  %"hew_vec_get_i64 arg198" = load i64, ptr %local_5, align 8
  %hew_vec_get_i64_call99 = call i64 @hew_vec_get_i64(ptr %"hew_vec_get_i64 arg097", i64 %"hew_vec_get_i64 arg198")
  store i64 %hew_vec_get_i64_call99, ptr %local_38, align 8
  %move_load100 = load i64, ptr %local_38, align 8
  store i64 %move_load100, ptr %local_39, align 8
  %call_arg101 = load ptr, ptr %local_1, align 8
  %call_arg102 = load i64, ptr %local_7, align 8
  %call_arg103 = load i64, ptr %local_39, align 8
  call void @hew_vec_set_i64(ptr %call_arg101, i64 %call_arg102, i64 %call_arg103)
  br label %bb28

bb28:                                             ; preds = %bb27
  store i64 1, ptr %local_40, align 8
  %checked_lhs104 = load i64, ptr %local_5, align 8
  %checked_rhs105 = load i64, ptr %local_40, align 8
  %with_overflow106 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs104, i64 %checked_rhs105)
  %checked_result107 = extractvalue { i64, i1 } %with_overflow106, 0
  %checked_overflow108 = extractvalue { i64, i1 } %with_overflow106, 1
  %checked_overflow_widen109 = zext i1 %checked_overflow108 to i8
  store i64 %checked_result107, ptr %local_41, align 8
  store i8 %checked_overflow_widen109, ptr %local_42, align 1
  %cond_load110 = load i8, ptr %local_42, align 1
  %cond_nz111 = icmp ne i8 %cond_load110, 0
  br i1 %cond_nz111, label %bb29, label %bb30

bb29:                                             ; preds = %bb28
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb30:                                             ; preds = %bb28
  %move_load112 = load i64, ptr %local_41, align 8
  store i64 %move_load112, ptr %local_5, align 8
  store i64 1, ptr %local_43, align 8
  %checked_lhs113 = load i64, ptr %local_7, align 8
  %checked_rhs114 = load i64, ptr %local_43, align 8
  %with_overflow115 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs113, i64 %checked_rhs114)
  %checked_result116 = extractvalue { i64, i1 } %with_overflow115, 0
  %checked_overflow117 = extractvalue { i64, i1 } %with_overflow115, 1
  %checked_overflow_widen118 = zext i1 %checked_overflow117 to i8
  store i64 %checked_result116, ptr %local_44, align 8
  store i8 %checked_overflow_widen118, ptr %local_45, align 1
  %cond_load119 = load i8, ptr %local_45, align 1
  %cond_nz120 = icmp ne i8 %cond_load119, 0
  br i1 %cond_nz120, label %bb31, label %bb32

bb31:                                             ; preds = %bb30
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb32:                                             ; preds = %bb30
  %move_load121 = load i64, ptr %local_44, align 8
  store i64 %move_load121, ptr %local_7, align 8
  %hew_actor_cooperate122 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel123 = icmp eq i32 %hew_actor_cooperate122, 2
  br i1 %hew_cooperate_is_cancel123, label %cancel_exit124, label %after_cooperate125

bb33:                                             ; preds = %after_cooperate169, %bb25
  %cmp_lhs126 = load i64, ptr %local_6, align 8
  %cmp_rhs127 = load i64, ptr %local_4, align 8
  %cmp_bit128 = icmp slt i64 %cmp_lhs126, %cmp_rhs127
  %cmp_zext129 = zext i1 %cmp_bit128 to i8
  store i8 %cmp_zext129, ptr %local_46, align 1
  %cond_load130 = load i8, ptr %local_46, align 1
  %cond_nz131 = icmp ne i8 %cond_load130, 0
  br i1 %cond_nz131, label %bb34, label %bb35

bb34:                                             ; preds = %bb33
  %"hew_vec_len arg0132" = load ptr, ptr %local_0, align 8
  %hew_vec_len_call133 = call i64 @hew_vec_len(ptr %"hew_vec_len arg0132")
  store i64 %hew_vec_len_call133, ptr %local_47, align 8
  %cmp_lhs134 = load i64, ptr %local_6, align 8
  %cmp_rhs135 = load i64, ptr %local_47, align 8
  %cmp_bit136 = icmp uge i64 %cmp_lhs134, %cmp_rhs135
  %cmp_zext137 = zext i1 %cmp_bit136 to i8
  store i8 %cmp_zext137, ptr %local_48, align 1
  %cond_load138 = load i8, ptr %local_48, align 1
  %cond_nz139 = icmp ne i8 %cond_load138, 0
  br i1 %cond_nz139, label %bb36, label %bb37

bb35:                                             ; preds = %bb33
  %move_load140 = load i64, ptr %local_9, align 8
  store i64 %move_load140, ptr %return_slot, align 8
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val

bb36:                                             ; preds = %bb34
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb37:                                             ; preds = %bb34
  %"hew_vec_get_i64 arg0141" = load ptr, ptr %local_0, align 8
  %"hew_vec_get_i64 arg1142" = load i64, ptr %local_6, align 8
  %hew_vec_get_i64_call143 = call i64 @hew_vec_get_i64(ptr %"hew_vec_get_i64 arg0141", i64 %"hew_vec_get_i64 arg1142")
  store i64 %hew_vec_get_i64_call143, ptr %local_49, align 8
  %move_load144 = load i64, ptr %local_49, align 8
  store i64 %move_load144, ptr %local_50, align 8
  %call_arg145 = load ptr, ptr %local_1, align 8
  %call_arg146 = load i64, ptr %local_7, align 8
  %call_arg147 = load i64, ptr %local_50, align 8
  call void @hew_vec_set_i64(ptr %call_arg145, i64 %call_arg146, i64 %call_arg147)
  br label %bb38

bb38:                                             ; preds = %bb37
  store i64 1, ptr %local_51, align 8
  %checked_lhs148 = load i64, ptr %local_6, align 8
  %checked_rhs149 = load i64, ptr %local_51, align 8
  %with_overflow150 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs148, i64 %checked_rhs149)
  %checked_result151 = extractvalue { i64, i1 } %with_overflow150, 0
  %checked_overflow152 = extractvalue { i64, i1 } %with_overflow150, 1
  %checked_overflow_widen153 = zext i1 %checked_overflow152 to i8
  store i64 %checked_result151, ptr %local_52, align 8
  store i8 %checked_overflow_widen153, ptr %local_53, align 1
  %cond_load154 = load i8, ptr %local_53, align 1
  %cond_nz155 = icmp ne i8 %cond_load154, 0
  br i1 %cond_nz155, label %bb39, label %bb40

bb39:                                             ; preds = %bb38
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb40:                                             ; preds = %bb38
  %move_load156 = load i64, ptr %local_52, align 8
  store i64 %move_load156, ptr %local_6, align 8
  store i64 1, ptr %local_54, align 8
  %checked_lhs157 = load i64, ptr %local_7, align 8
  %checked_rhs158 = load i64, ptr %local_54, align 8
  %with_overflow159 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs157, i64 %checked_rhs158)
  %checked_result160 = extractvalue { i64, i1 } %with_overflow159, 0
  %checked_overflow161 = extractvalue { i64, i1 } %with_overflow159, 1
  %checked_overflow_widen162 = zext i1 %checked_overflow161 to i8
  store i64 %checked_result160, ptr %local_55, align 8
  store i8 %checked_overflow_widen162, ptr %local_56, align 1
  %cond_load163 = load i8, ptr %local_56, align 1
  %cond_nz164 = icmp ne i8 %cond_load163, 0
  br i1 %cond_nz164, label %bb41, label %bb42

bb41:                                             ; preds = %bb40
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb42:                                             ; preds = %bb40
  %move_load165 = load i64, ptr %local_55, align 8
  store i64 %move_load165, ptr %local_7, align 8
  %hew_actor_cooperate166 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel167 = icmp eq i32 %hew_actor_cooperate166, 2
  br i1 %hew_cooperate_is_cancel167, label %cancel_exit168, label %after_cooperate169

cancel_exit:                                      ; preds = %entry
  ret i64 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit63:                                    ; preds = %bb17
  ret i64 0

after_cooperate64:                                ; preds = %bb17
  br label %bb14

cancel_exit76:                                    ; preds = %bb20
  ret i64 0

after_cooperate77:                                ; preds = %bb20
  br label %bb14

cancel_exit81:                                    ; preds = %bb22
  ret i64 0

after_cooperate82:                                ; preds = %bb22
  br label %bb1

cancel_exit124:                                   ; preds = %bb32
  ret i64 0

after_cooperate125:                               ; preds = %bb32
  br label %bb23

cancel_exit168:                                   ; preds = %bb42
  ret i64 0

after_cooperate169:                               ; preds = %bb42
  br label %bb33
}

define internal i64 @"sort$merge_string_run"(ptr %0, ptr %1, i64 %2, i64 %3, i64 %4) {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i64, align 8
  %local_5 = alloca i64, align 8
  %local_6 = alloca i64, align 8
  %local_7 = alloca i64, align 8
  %local_8 = alloca i64, align 8
  %local_9 = alloca i64, align 8
  %local_10 = alloca i8, align 1
  %local_11 = alloca i8, align 1
  %local_12 = alloca i8, align 1
  %local_13 = alloca i64, align 8
  %local_14 = alloca i64, align 8
  %local_15 = alloca i8, align 1
  %local_16 = alloca i64, align 8
  %local_17 = alloca i8, align 1
  %local_18 = alloca ptr, align 8
  %local_19 = alloca ptr, align 8
  %local_20 = alloca i64, align 8
  %local_21 = alloca i8, align 1
  %local_22 = alloca ptr, align 8
  %local_23 = alloca ptr, align 8
  %local_24 = alloca i8, align 1
  %local_25 = alloca i8, align 1
  %local_26 = alloca i64, align 8
  %local_27 = alloca i64, align 8
  %local_28 = alloca i8, align 1
  %local_29 = alloca i64, align 8
  %local_30 = alloca i64, align 8
  %local_31 = alloca i8, align 1
  %local_32 = alloca i64, align 8
  %local_33 = alloca i64, align 8
  %local_34 = alloca i8, align 1
  %local_35 = alloca i8, align 1
  %local_36 = alloca i64, align 8
  %local_37 = alloca i8, align 1
  %local_38 = alloca ptr, align 8
  %local_39 = alloca ptr, align 8
  %local_40 = alloca i64, align 8
  %local_41 = alloca i64, align 8
  %local_42 = alloca i8, align 1
  %local_43 = alloca i64, align 8
  %local_44 = alloca i64, align 8
  %local_45 = alloca i8, align 1
  %local_46 = alloca i8, align 1
  %local_47 = alloca i64, align 8
  %local_48 = alloca i8, align 1
  %local_49 = alloca ptr, align 8
  %local_50 = alloca ptr, align 8
  %local_51 = alloca i64, align 8
  %local_52 = alloca i64, align 8
  %local_53 = alloca i8, align 1
  %local_54 = alloca i64, align 8
  %local_55 = alloca i64, align 8
  %local_56 = alloca i8, align 1
  store ptr %0, ptr %local_0, align 8
  store ptr %1, ptr %local_1, align 8
  store i64 %2, ptr %local_2, align 8
  store i64 %3, ptr %local_3, align 8
  store i64 %4, ptr %local_4, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %move_load = load i64, ptr %local_2, align 8
  store i64 %move_load, ptr %local_5, align 8
  %move_load1 = load i64, ptr %local_3, align 8
  store i64 %move_load1, ptr %local_6, align 8
  %move_load2 = load i64, ptr %local_2, align 8
  store i64 %move_load2, ptr %local_7, align 8
  store i64 0, ptr %local_8, align 8
  %move_load3 = load i64, ptr %local_8, align 8
  store i64 %move_load3, ptr %local_9, align 8
  br label %bb1

bb1:                                              ; preds = %after_cooperate88, %bb0
  store i8 0, ptr %local_10, align 1
  %cmp_lhs = load i64, ptr %local_5, align 8
  %cmp_rhs = load i64, ptr %local_3, align 8
  %cmp_bit = icmp slt i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_11, align 1
  %cond_load = load i8, ptr %local_11, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb4, label %bb5

bb2:                                              ; preds = %bb5
  store i64 1, ptr %local_13, align 8
  %checked_lhs = load i64, ptr %local_9, align 8
  %checked_rhs = load i64, ptr %local_13, align 8
  %with_overflow = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_14, align 8
  store i8 %checked_overflow_widen, ptr %local_15, align 1
  %cond_load4 = load i8, ptr %local_15, align 1
  %cond_nz5 = icmp ne i8 %cond_load4, 0
  br i1 %cond_nz5, label %bb6, label %bb7

bb3:                                              ; preds = %bb5
  br label %bb23

bb4:                                              ; preds = %bb1
  %cmp_lhs6 = load i64, ptr %local_6, align 8
  %cmp_rhs7 = load i64, ptr %local_4, align 8
  %cmp_bit8 = icmp slt i64 %cmp_lhs6, %cmp_rhs7
  %cmp_zext9 = zext i1 %cmp_bit8 to i8
  store i8 %cmp_zext9, ptr %local_12, align 1
  %move_load10 = load i8, ptr %local_12, align 1
  store i8 %move_load10, ptr %local_10, align 1
  br label %bb5

bb5:                                              ; preds = %bb4, %bb1
  %cond_load11 = load i8, ptr %local_10, align 1
  %cond_nz12 = icmp ne i8 %cond_load11, 0
  br i1 %cond_nz12, label %bb2, label %bb3

bb6:                                              ; preds = %bb2
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb7:                                              ; preds = %bb2
  %move_load13 = load i64, ptr %local_14, align 8
  store i64 %move_load13, ptr %local_9, align 8
  %"hew_vec_len arg0" = load ptr, ptr %local_0, align 8
  %hew_vec_len_call = call i64 @hew_vec_len(ptr %"hew_vec_len arg0")
  store i64 %hew_vec_len_call, ptr %local_16, align 8
  %cmp_lhs14 = load i64, ptr %local_5, align 8
  %cmp_rhs15 = load i64, ptr %local_16, align 8
  %cmp_bit16 = icmp uge i64 %cmp_lhs14, %cmp_rhs15
  %cmp_zext17 = zext i1 %cmp_bit16 to i8
  store i8 %cmp_zext17, ptr %local_17, align 1
  %cond_load18 = load i8, ptr %local_17, align 1
  %cond_nz19 = icmp ne i8 %cond_load18, 0
  br i1 %cond_nz19, label %bb8, label %bb9

bb8:                                              ; preds = %bb7
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb9:                                              ; preds = %bb7
  %"hew_vec_get_str arg0" = load ptr, ptr %local_0, align 8
  %"hew_vec_get_str arg1" = load i64, ptr %local_5, align 8
  %hew_vec_get_str_call = call ptr @hew_vec_get_str(ptr %"hew_vec_get_str arg0", i64 %"hew_vec_get_str arg1")
  store ptr %hew_vec_get_str_call, ptr %local_18, align 8
  %move_load20 = load ptr, ptr %local_18, align 8
  store ptr %move_load20, ptr %local_19, align 8
  %"hew_vec_len arg021" = load ptr, ptr %local_0, align 8
  %hew_vec_len_call22 = call i64 @hew_vec_len(ptr %"hew_vec_len arg021")
  store i64 %hew_vec_len_call22, ptr %local_20, align 8
  %cmp_lhs23 = load i64, ptr %local_6, align 8
  %cmp_rhs24 = load i64, ptr %local_20, align 8
  %cmp_bit25 = icmp uge i64 %cmp_lhs23, %cmp_rhs24
  %cmp_zext26 = zext i1 %cmp_bit25 to i8
  store i8 %cmp_zext26, ptr %local_21, align 1
  %cond_load27 = load i8, ptr %local_21, align 1
  %cond_nz28 = icmp ne i8 %cond_load27, 0
  br i1 %cond_nz28, label %bb10, label %bb11

bb10:                                             ; preds = %bb9
  %"hew_string_drop drop" = load ptr, ptr %local_19, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop")
  store ptr null, ptr %local_19, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb11:                                             ; preds = %bb9
  %"hew_vec_get_str arg029" = load ptr, ptr %local_0, align 8
  %"hew_vec_get_str arg130" = load i64, ptr %local_6, align 8
  %hew_vec_get_str_call31 = call ptr @hew_vec_get_str(ptr %"hew_vec_get_str arg029", i64 %"hew_vec_get_str arg130")
  store ptr %hew_vec_get_str_call31, ptr %local_22, align 8
  %move_load32 = load ptr, ptr %local_22, align 8
  store ptr %move_load32, ptr %local_23, align 8
  %string_ord_lhs = load ptr, ptr %local_23, align 8
  %string_ord_rhs = load ptr, ptr %local_19, align 8
  %hew_string_compare = call i32 @hew_string_compare(ptr %string_ord_lhs, ptr %string_ord_rhs)
  %string_ord_bit = icmp slt i32 %hew_string_compare, 0
  %string_ord_zext = zext i1 %string_ord_bit to i8
  store i8 %string_ord_zext, ptr %local_25, align 1
  %cond_load33 = load i8, ptr %local_25, align 1
  %cond_nz34 = icmp ne i8 %cond_load33, 0
  br i1 %cond_nz34, label %bb12, label %bb13

bb12:                                             ; preds = %bb11
  %call_arg = load ptr, ptr %local_1, align 8
  %call_arg35 = load i64, ptr %local_7, align 8
  %call_arg36 = load ptr, ptr %local_23, align 8
  call void @hew_vec_set_str(ptr %call_arg, i64 %call_arg35, ptr %call_arg36)
  br label %bb15

bb13:                                             ; preds = %bb11
  %call_arg37 = load ptr, ptr %local_1, align 8
  %call_arg38 = load i64, ptr %local_7, align 8
  %call_arg39 = load ptr, ptr %local_19, align 8
  call void @hew_vec_set_str(ptr %call_arg37, i64 %call_arg38, ptr %call_arg39)
  br label %bb18

bb14:                                             ; preds = %after_cooperate79, %after_cooperate62
  store i64 1, ptr %local_32, align 8
  %checked_lhs40 = load i64, ptr %local_7, align 8
  %checked_rhs41 = load i64, ptr %local_32, align 8
  %with_overflow42 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs40, i64 %checked_rhs41)
  %checked_result43 = extractvalue { i64, i1 } %with_overflow42, 0
  %checked_overflow44 = extractvalue { i64, i1 } %with_overflow42, 1
  %checked_overflow_widen45 = zext i1 %checked_overflow44 to i8
  store i64 %checked_result43, ptr %local_33, align 8
  store i8 %checked_overflow_widen45, ptr %local_34, align 1
  %cond_load46 = load i8, ptr %local_34, align 1
  %cond_nz47 = icmp ne i8 %cond_load46, 0
  br i1 %cond_nz47, label %bb21, label %bb22

bb15:                                             ; preds = %bb12
  store i64 1, ptr %local_26, align 8
  %checked_lhs48 = load i64, ptr %local_6, align 8
  %checked_rhs49 = load i64, ptr %local_26, align 8
  %with_overflow50 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs48, i64 %checked_rhs49)
  %checked_result51 = extractvalue { i64, i1 } %with_overflow50, 0
  %checked_overflow52 = extractvalue { i64, i1 } %with_overflow50, 1
  %checked_overflow_widen53 = zext i1 %checked_overflow52 to i8
  store i64 %checked_result51, ptr %local_27, align 8
  store i8 %checked_overflow_widen53, ptr %local_28, align 1
  %cond_load54 = load i8, ptr %local_28, align 1
  %cond_nz55 = icmp ne i8 %cond_load54, 0
  br i1 %cond_nz55, label %bb16, label %bb17

bb16:                                             ; preds = %bb15
  %"hew_string_drop drop56" = load ptr, ptr %local_23, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop56")
  store ptr null, ptr %local_23, align 8
  %"hew_string_drop drop57" = load ptr, ptr %local_19, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop57")
  store ptr null, ptr %local_19, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb17:                                             ; preds = %bb15
  %move_load58 = load i64, ptr %local_27, align 8
  store i64 %move_load58, ptr %local_6, align 8
  %hew_actor_cooperate59 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel60 = icmp eq i32 %hew_actor_cooperate59, 2
  br i1 %hew_cooperate_is_cancel60, label %cancel_exit61, label %after_cooperate62

bb18:                                             ; preds = %bb13
  store i64 1, ptr %local_29, align 8
  %checked_lhs65 = load i64, ptr %local_5, align 8
  %checked_rhs66 = load i64, ptr %local_29, align 8
  %with_overflow67 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs65, i64 %checked_rhs66)
  %checked_result68 = extractvalue { i64, i1 } %with_overflow67, 0
  %checked_overflow69 = extractvalue { i64, i1 } %with_overflow67, 1
  %checked_overflow_widen70 = zext i1 %checked_overflow69 to i8
  store i64 %checked_result68, ptr %local_30, align 8
  store i8 %checked_overflow_widen70, ptr %local_31, align 1
  %cond_load71 = load i8, ptr %local_31, align 1
  %cond_nz72 = icmp ne i8 %cond_load71, 0
  br i1 %cond_nz72, label %bb19, label %bb20

bb19:                                             ; preds = %bb18
  %"hew_string_drop drop73" = load ptr, ptr %local_23, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop73")
  store ptr null, ptr %local_23, align 8
  %"hew_string_drop drop74" = load ptr, ptr %local_19, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop74")
  store ptr null, ptr %local_19, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb20:                                             ; preds = %bb18
  %move_load75 = load i64, ptr %local_30, align 8
  store i64 %move_load75, ptr %local_5, align 8
  %hew_actor_cooperate76 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel77 = icmp eq i32 %hew_actor_cooperate76, 2
  br i1 %hew_cooperate_is_cancel77, label %cancel_exit78, label %after_cooperate79

bb21:                                             ; preds = %bb14
  %"hew_string_drop drop82" = load ptr, ptr %local_23, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop82")
  store ptr null, ptr %local_23, align 8
  %"hew_string_drop drop83" = load ptr, ptr %local_19, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop83")
  store ptr null, ptr %local_19, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb22:                                             ; preds = %bb14
  %move_load84 = load i64, ptr %local_33, align 8
  store i64 %move_load84, ptr %local_7, align 8
  %hew_actor_cooperate85 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel86 = icmp eq i32 %hew_actor_cooperate85, 2
  br i1 %hew_cooperate_is_cancel86, label %cancel_exit87, label %after_cooperate88

bb23:                                             ; preds = %after_cooperate137, %bb3
  %cmp_lhs93 = load i64, ptr %local_5, align 8
  %cmp_rhs94 = load i64, ptr %local_3, align 8
  %cmp_bit95 = icmp slt i64 %cmp_lhs93, %cmp_rhs94
  %cmp_zext96 = zext i1 %cmp_bit95 to i8
  store i8 %cmp_zext96, ptr %local_35, align 1
  %cond_load97 = load i8, ptr %local_35, align 1
  %cond_nz98 = icmp ne i8 %cond_load97, 0
  br i1 %cond_nz98, label %bb24, label %bb25

bb24:                                             ; preds = %bb23
  %"hew_vec_len arg099" = load ptr, ptr %local_0, align 8
  %hew_vec_len_call100 = call i64 @hew_vec_len(ptr %"hew_vec_len arg099")
  store i64 %hew_vec_len_call100, ptr %local_36, align 8
  %cmp_lhs101 = load i64, ptr %local_5, align 8
  %cmp_rhs102 = load i64, ptr %local_36, align 8
  %cmp_bit103 = icmp uge i64 %cmp_lhs101, %cmp_rhs102
  %cmp_zext104 = zext i1 %cmp_bit103 to i8
  store i8 %cmp_zext104, ptr %local_37, align 1
  %cond_load105 = load i8, ptr %local_37, align 1
  %cond_nz106 = icmp ne i8 %cond_load105, 0
  br i1 %cond_nz106, label %bb26, label %bb27

bb25:                                             ; preds = %bb23
  br label %bb33

bb26:                                             ; preds = %bb24
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb27:                                             ; preds = %bb24
  %"hew_vec_get_str arg0107" = load ptr, ptr %local_0, align 8
  %"hew_vec_get_str arg1108" = load i64, ptr %local_5, align 8
  %hew_vec_get_str_call109 = call ptr @hew_vec_get_str(ptr %"hew_vec_get_str arg0107", i64 %"hew_vec_get_str arg1108")
  store ptr %hew_vec_get_str_call109, ptr %local_38, align 8
  %move_load110 = load ptr, ptr %local_38, align 8
  store ptr %move_load110, ptr %local_39, align 8
  %call_arg111 = load ptr, ptr %local_1, align 8
  %call_arg112 = load i64, ptr %local_7, align 8
  %call_arg113 = load ptr, ptr %local_39, align 8
  call void @hew_vec_set_str(ptr %call_arg111, i64 %call_arg112, ptr %call_arg113)
  br label %bb28

bb28:                                             ; preds = %bb27
  store i64 1, ptr %local_40, align 8
  %checked_lhs114 = load i64, ptr %local_5, align 8
  %checked_rhs115 = load i64, ptr %local_40, align 8
  %with_overflow116 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs114, i64 %checked_rhs115)
  %checked_result117 = extractvalue { i64, i1 } %with_overflow116, 0
  %checked_overflow118 = extractvalue { i64, i1 } %with_overflow116, 1
  %checked_overflow_widen119 = zext i1 %checked_overflow118 to i8
  store i64 %checked_result117, ptr %local_41, align 8
  store i8 %checked_overflow_widen119, ptr %local_42, align 1
  %cond_load120 = load i8, ptr %local_42, align 1
  %cond_nz121 = icmp ne i8 %cond_load120, 0
  br i1 %cond_nz121, label %bb29, label %bb30

bb29:                                             ; preds = %bb28
  %"hew_string_drop drop122" = load ptr, ptr %local_39, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop122")
  store ptr null, ptr %local_39, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb30:                                             ; preds = %bb28
  %move_load123 = load i64, ptr %local_41, align 8
  store i64 %move_load123, ptr %local_5, align 8
  store i64 1, ptr %local_43, align 8
  %checked_lhs124 = load i64, ptr %local_7, align 8
  %checked_rhs125 = load i64, ptr %local_43, align 8
  %with_overflow126 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs124, i64 %checked_rhs125)
  %checked_result127 = extractvalue { i64, i1 } %with_overflow126, 0
  %checked_overflow128 = extractvalue { i64, i1 } %with_overflow126, 1
  %checked_overflow_widen129 = zext i1 %checked_overflow128 to i8
  store i64 %checked_result127, ptr %local_44, align 8
  store i8 %checked_overflow_widen129, ptr %local_45, align 1
  %cond_load130 = load i8, ptr %local_45, align 1
  %cond_nz131 = icmp ne i8 %cond_load130, 0
  br i1 %cond_nz131, label %bb31, label %bb32

bb31:                                             ; preds = %bb30
  %"hew_string_drop drop132" = load ptr, ptr %local_39, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop132")
  store ptr null, ptr %local_39, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb32:                                             ; preds = %bb30
  %move_load133 = load i64, ptr %local_44, align 8
  store i64 %move_load133, ptr %local_7, align 8
  %hew_actor_cooperate134 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel135 = icmp eq i32 %hew_actor_cooperate134, 2
  br i1 %hew_cooperate_is_cancel135, label %cancel_exit136, label %after_cooperate137

bb33:                                             ; preds = %after_cooperate185, %bb25
  %cmp_lhs140 = load i64, ptr %local_6, align 8
  %cmp_rhs141 = load i64, ptr %local_4, align 8
  %cmp_bit142 = icmp slt i64 %cmp_lhs140, %cmp_rhs141
  %cmp_zext143 = zext i1 %cmp_bit142 to i8
  store i8 %cmp_zext143, ptr %local_46, align 1
  %cond_load144 = load i8, ptr %local_46, align 1
  %cond_nz145 = icmp ne i8 %cond_load144, 0
  br i1 %cond_nz145, label %bb34, label %bb35

bb34:                                             ; preds = %bb33
  %"hew_vec_len arg0146" = load ptr, ptr %local_0, align 8
  %hew_vec_len_call147 = call i64 @hew_vec_len(ptr %"hew_vec_len arg0146")
  store i64 %hew_vec_len_call147, ptr %local_47, align 8
  %cmp_lhs148 = load i64, ptr %local_6, align 8
  %cmp_rhs149 = load i64, ptr %local_47, align 8
  %cmp_bit150 = icmp uge i64 %cmp_lhs148, %cmp_rhs149
  %cmp_zext151 = zext i1 %cmp_bit150 to i8
  store i8 %cmp_zext151, ptr %local_48, align 1
  %cond_load152 = load i8, ptr %local_48, align 1
  %cond_nz153 = icmp ne i8 %cond_load152, 0
  br i1 %cond_nz153, label %bb36, label %bb37

bb35:                                             ; preds = %bb33
  %move_load154 = load i64, ptr %local_9, align 8
  store i64 %move_load154, ptr %return_slot, align 8
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val

bb36:                                             ; preds = %bb34
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb37:                                             ; preds = %bb34
  %"hew_vec_get_str arg0155" = load ptr, ptr %local_0, align 8
  %"hew_vec_get_str arg1156" = load i64, ptr %local_6, align 8
  %hew_vec_get_str_call157 = call ptr @hew_vec_get_str(ptr %"hew_vec_get_str arg0155", i64 %"hew_vec_get_str arg1156")
  store ptr %hew_vec_get_str_call157, ptr %local_49, align 8
  %move_load158 = load ptr, ptr %local_49, align 8
  store ptr %move_load158, ptr %local_50, align 8
  %call_arg159 = load ptr, ptr %local_1, align 8
  %call_arg160 = load i64, ptr %local_7, align 8
  %call_arg161 = load ptr, ptr %local_50, align 8
  call void @hew_vec_set_str(ptr %call_arg159, i64 %call_arg160, ptr %call_arg161)
  br label %bb38

bb38:                                             ; preds = %bb37
  store i64 1, ptr %local_51, align 8
  %checked_lhs162 = load i64, ptr %local_6, align 8
  %checked_rhs163 = load i64, ptr %local_51, align 8
  %with_overflow164 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs162, i64 %checked_rhs163)
  %checked_result165 = extractvalue { i64, i1 } %with_overflow164, 0
  %checked_overflow166 = extractvalue { i64, i1 } %with_overflow164, 1
  %checked_overflow_widen167 = zext i1 %checked_overflow166 to i8
  store i64 %checked_result165, ptr %local_52, align 8
  store i8 %checked_overflow_widen167, ptr %local_53, align 1
  %cond_load168 = load i8, ptr %local_53, align 1
  %cond_nz169 = icmp ne i8 %cond_load168, 0
  br i1 %cond_nz169, label %bb39, label %bb40

bb39:                                             ; preds = %bb38
  %"hew_string_drop drop170" = load ptr, ptr %local_50, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop170")
  store ptr null, ptr %local_50, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb40:                                             ; preds = %bb38
  %move_load171 = load i64, ptr %local_52, align 8
  store i64 %move_load171, ptr %local_6, align 8
  store i64 1, ptr %local_54, align 8
  %checked_lhs172 = load i64, ptr %local_7, align 8
  %checked_rhs173 = load i64, ptr %local_54, align 8
  %with_overflow174 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs172, i64 %checked_rhs173)
  %checked_result175 = extractvalue { i64, i1 } %with_overflow174, 0
  %checked_overflow176 = extractvalue { i64, i1 } %with_overflow174, 1
  %checked_overflow_widen177 = zext i1 %checked_overflow176 to i8
  store i64 %checked_result175, ptr %local_55, align 8
  store i8 %checked_overflow_widen177, ptr %local_56, align 1
  %cond_load178 = load i8, ptr %local_56, align 1
  %cond_nz179 = icmp ne i8 %cond_load178, 0
  br i1 %cond_nz179, label %bb41, label %bb42

bb41:                                             ; preds = %bb40
  %"hew_string_drop drop180" = load ptr, ptr %local_50, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop180")
  store ptr null, ptr %local_50, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb42:                                             ; preds = %bb40
  %move_load181 = load i64, ptr %local_55, align 8
  store i64 %move_load181, ptr %local_7, align 8
  %hew_actor_cooperate182 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel183 = icmp eq i32 %hew_actor_cooperate182, 2
  br i1 %hew_cooperate_is_cancel183, label %cancel_exit184, label %after_cooperate185

cancel_exit:                                      ; preds = %entry
  ret i64 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit61:                                    ; preds = %bb17
  %"hew_string_drop drop63" = load ptr, ptr %local_23, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop63")
  store ptr null, ptr %local_23, align 8
  %"hew_string_drop drop64" = load ptr, ptr %local_19, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop64")
  store ptr null, ptr %local_19, align 8
  ret i64 0

after_cooperate62:                                ; preds = %bb17
  br label %bb14

cancel_exit78:                                    ; preds = %bb20
  %"hew_string_drop drop80" = load ptr, ptr %local_23, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop80")
  store ptr null, ptr %local_23, align 8
  %"hew_string_drop drop81" = load ptr, ptr %local_19, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop81")
  store ptr null, ptr %local_19, align 8
  ret i64 0

after_cooperate79:                                ; preds = %bb20
  br label %bb14

cancel_exit87:                                    ; preds = %bb22
  %"hew_string_drop drop89" = load ptr, ptr %local_23, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop89")
  store ptr null, ptr %local_23, align 8
  %"hew_string_drop drop90" = load ptr, ptr %local_19, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop90")
  store ptr null, ptr %local_19, align 8
  ret i64 0

after_cooperate88:                                ; preds = %bb22
  %"hew_string_drop drop91" = load ptr, ptr %local_23, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop91")
  store ptr null, ptr %local_23, align 8
  %"hew_string_drop drop92" = load ptr, ptr %local_19, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop92")
  store ptr null, ptr %local_19, align 8
  br label %bb1

cancel_exit136:                                   ; preds = %bb32
  %"hew_string_drop drop138" = load ptr, ptr %local_39, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop138")
  store ptr null, ptr %local_39, align 8
  ret i64 0

after_cooperate137:                               ; preds = %bb32
  %"hew_string_drop drop139" = load ptr, ptr %local_39, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop139")
  store ptr null, ptr %local_39, align 8
  br label %bb23

cancel_exit184:                                   ; preds = %bb42
  %"hew_string_drop drop186" = load ptr, ptr %local_50, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop186")
  store ptr null, ptr %local_50, align 8
  ret i64 0

after_cooperate185:                               ; preds = %bb42
  %"hew_string_drop drop187" = load ptr, ptr %local_50, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop187")
  store ptr null, ptr %local_50, align 8
  br label %bb33
}

define internal i64 @"sort$sort_ints_in_place"(ptr %0) {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i8, align 1
  %local_4 = alloca i64, align 8
  %local_5 = alloca i8, align 1
  %local_6 = alloca i64, align 8
  %local_7 = alloca ptr, align 8
  %local_8 = alloca ptr, align 8
  %local_9 = alloca i64, align 8
  %local_10 = alloca i64, align 8
  %local_11 = alloca i64, align 8
  %local_12 = alloca i64, align 8
  %local_13 = alloca i8, align 1
  %local_14 = alloca i64, align 8
  %local_15 = alloca i64, align 8
  %local_16 = alloca i8, align 1
  %local_17 = alloca i64, align 8
  %local_18 = alloca i64, align 8
  %local_19 = alloca i8, align 1
  %local_20 = alloca i8, align 1
  %local_21 = alloca i64, align 8
  %local_22 = alloca i64, align 8
  %local_23 = alloca i8, align 1
  %local_24 = alloca i64, align 8
  %local_25 = alloca i64, align 8
  %local_26 = alloca i64, align 8
  %local_27 = alloca i8, align 1
  %local_28 = alloca i64, align 8
  %local_29 = alloca i64, align 8
  %local_30 = alloca i64, align 8
  %local_31 = alloca i8, align 1
  %local_32 = alloca i8, align 1
  %local_33 = alloca i64, align 8
  %local_34 = alloca i64, align 8
  %local_35 = alloca i8, align 1
  %local_36 = alloca i64, align 8
  %local_37 = alloca i64, align 8
  %local_38 = alloca i64, align 8
  %local_39 = alloca i8, align 1
  %local_40 = alloca i64, align 8
  %local_41 = alloca i64, align 8
  %local_42 = alloca i64, align 8
  %local_43 = alloca i8, align 1
  %local_44 = alloca i64, align 8
  %local_45 = alloca i64, align 8
  %local_46 = alloca i64, align 8
  %local_47 = alloca i64, align 8
  %local_48 = alloca i64, align 8
  %local_49 = alloca i8, align 1
  %local_50 = alloca i64, align 8
  %local_51 = alloca i8, align 1
  %local_52 = alloca i64, align 8
  %local_53 = alloca i8, align 1
  %local_54 = alloca i64, align 8
  %local_55 = alloca i64, align 8
  %local_56 = alloca i8, align 1
  %local_57 = alloca i64, align 8
  %local_58 = alloca i64, align 8
  %local_59 = alloca i64, align 8
  %local_60 = alloca i64, align 8
  %local_61 = alloca i8, align 1
  %local_62 = alloca i64, align 8
  %local_63 = alloca i8, align 1
  %local_64 = alloca i64, align 8
  %local_65 = alloca i8, align 1
  %local_66 = alloca i8, align 1
  %local_67 = alloca i64, align 8
  %local_68 = alloca i64, align 8
  %local_69 = alloca i64, align 8
  %local_70 = alloca i8, align 1
  %local_71 = alloca i64, align 8
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result = call i64 @hew_vec_len(ptr %call_arg)
  store i64 %call_result, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load i64, ptr %local_1, align 8
  store i64 %move_load, ptr %local_2, align 8
  store i64 2, ptr %local_4, align 8
  %cmp_lhs = load i64, ptr %local_2, align 8
  %cmp_rhs = load i64, ptr %local_4, align 8
  %cmp_bit = icmp slt i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_5, align 1
  %cond_load = load i8, ptr %local_5, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  store i64 0, ptr %local_6, align 8
  %move_load1 = load i64, ptr %local_6, align 8
  store i64 %move_load1, ptr %return_slot, align 8
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val

bb3:                                              ; preds = %bb1
  br label %bb4

bb4:                                              ; preds = %after_cooperate7, %bb3
  %call_arg2 = load ptr, ptr %local_0, align 8
  %call_result3 = call ptr @"sort$copy_ints"(ptr %call_arg2)
  store ptr %call_result3, ptr %local_7, align 8
  br label %bb6

bb5:                                              ; No predecessors!
  %hew_actor_cooperate4 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel5 = icmp eq i32 %hew_actor_cooperate4, 2
  br i1 %hew_cooperate_is_cancel5, label %cancel_exit6, label %after_cooperate7

bb6:                                              ; preds = %bb4
  %move_load8 = load ptr, ptr %local_7, align 8
  store ptr %move_load8, ptr %local_8, align 8
  store i64 1, ptr %local_9, align 8
  %move_load9 = load i64, ptr %local_9, align 8
  store i64 %move_load9, ptr %local_10, align 8
  store i64 0, ptr %local_11, align 8
  %move_load10 = load i64, ptr %local_11, align 8
  store i64 %move_load10, ptr %local_12, align 8
  br label %bb7

bb7:                                              ; preds = %after_cooperate216, %bb6
  %cmp_lhs11 = load i64, ptr %local_10, align 8
  %cmp_rhs12 = load i64, ptr %local_2, align 8
  %cmp_bit13 = icmp slt i64 %cmp_lhs11, %cmp_rhs12
  %cmp_zext14 = zext i1 %cmp_bit13 to i8
  store i8 %cmp_zext14, ptr %local_13, align 1
  %cond_load15 = load i8, ptr %local_13, align 1
  %cond_nz16 = icmp ne i8 %cond_load15, 0
  br i1 %cond_nz16, label %bb8, label %bb9

bb8:                                              ; preds = %bb7
  store i64 0, ptr %local_14, align 8
  %move_load17 = load i64, ptr %local_14, align 8
  store i64 %move_load17, ptr %local_15, align 8
  br label %bb10

bb9:                                              ; preds = %bb7
  %move_load18 = load i64, ptr %local_12, align 8
  store i64 %move_load18, ptr %return_slot, align 8
  %"hew_vec_free drop" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop")
  store ptr null, ptr %local_8, align 8
  %ret_val19 = load i64, ptr %return_slot, align 8
  ret i64 %ret_val19

bb10:                                             ; preds = %after_cooperate135, %bb8
  %cmp_lhs20 = load i64, ptr %local_15, align 8
  %cmp_rhs21 = load i64, ptr %local_2, align 8
  %cmp_bit22 = icmp slt i64 %cmp_lhs20, %cmp_rhs21
  %cmp_zext23 = zext i1 %cmp_bit22 to i8
  store i8 %cmp_zext23, ptr %local_16, align 1
  %cond_load24 = load i8, ptr %local_16, align 1
  %cond_nz25 = icmp ne i8 %cond_load24, 0
  br i1 %cond_nz25, label %bb11, label %bb12

bb11:                                             ; preds = %bb10
  %checked_lhs = load i64, ptr %local_2, align 8
  %checked_rhs = load i64, ptr %local_15, align 8
  %with_overflow = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_18, align 8
  store i8 %checked_overflow_widen, ptr %local_19, align 1
  %cond_load26 = load i8, ptr %local_19, align 1
  %cond_nz27 = icmp ne i8 %cond_load26, 0
  br i1 %cond_nz27, label %bb13, label %bb14

bb12:                                             ; preds = %bb10
  store i64 1, ptr %local_46, align 8
  %move_load28 = load i64, ptr %local_46, align 8
  store i64 %move_load28, ptr %local_47, align 8
  store i64 0, ptr %local_48, align 8
  %cmp_lhs29 = load i64, ptr %local_47, align 8
  %cmp_rhs30 = load i64, ptr %local_48, align 8
  %cmp_bit31 = icmp sle i64 %cmp_lhs29, %cmp_rhs30
  %cmp_zext32 = zext i1 %cmp_bit31 to i8
  store i8 %cmp_zext32, ptr %local_49, align 1
  %cond_load33 = load i8, ptr %local_49, align 1
  %cond_nz34 = icmp ne i8 %cond_load33, 0
  br i1 %cond_nz34, label %bb38, label %bb39

bb13:                                             ; preds = %bb11
  %"hew_vec_free drop35" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop35")
  store ptr null, ptr %local_8, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb14:                                             ; preds = %bb11
  %cmp_lhs36 = load i64, ptr %local_10, align 8
  %cmp_rhs37 = load i64, ptr %local_18, align 8
  %cmp_bit38 = icmp slt i64 %cmp_lhs36, %cmp_rhs37
  %cmp_zext39 = zext i1 %cmp_bit38 to i8
  store i8 %cmp_zext39, ptr %local_20, align 1
  %cond_load40 = load i8, ptr %local_20, align 1
  %cond_nz41 = icmp ne i8 %cond_load40, 0
  br i1 %cond_nz41, label %bb15, label %bb16

bb15:                                             ; preds = %bb14
  %move_load42 = load i64, ptr %local_10, align 8
  store i64 %move_load42, ptr %local_21, align 8
  %move_load43 = load i64, ptr %local_21, align 8
  store i64 %move_load43, ptr %local_17, align 8
  br label %bb17

bb16:                                             ; preds = %bb14
  %checked_lhs44 = load i64, ptr %local_2, align 8
  %checked_rhs45 = load i64, ptr %local_15, align 8
  %with_overflow46 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %checked_lhs44, i64 %checked_rhs45)
  %checked_result47 = extractvalue { i64, i1 } %with_overflow46, 0
  %checked_overflow48 = extractvalue { i64, i1 } %with_overflow46, 1
  %checked_overflow_widen49 = zext i1 %checked_overflow48 to i8
  store i64 %checked_result47, ptr %local_22, align 8
  store i8 %checked_overflow_widen49, ptr %local_23, align 1
  %cond_load50 = load i8, ptr %local_23, align 1
  %cond_nz51 = icmp ne i8 %cond_load50, 0
  br i1 %cond_nz51, label %bb18, label %bb19

bb17:                                             ; preds = %after_cooperate67, %bb15
  %move_load52 = load i64, ptr %local_17, align 8
  store i64 %move_load52, ptr %local_25, align 8
  %checked_lhs53 = load i64, ptr %local_15, align 8
  %checked_rhs54 = load i64, ptr %local_25, align 8
  %with_overflow55 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs53, i64 %checked_rhs54)
  %checked_result56 = extractvalue { i64, i1 } %with_overflow55, 0
  %checked_overflow57 = extractvalue { i64, i1 } %with_overflow55, 1
  %checked_overflow_widen58 = zext i1 %checked_overflow57 to i8
  store i64 %checked_result56, ptr %local_26, align 8
  store i8 %checked_overflow_widen58, ptr %local_27, align 1
  %cond_load59 = load i8, ptr %local_27, align 1
  %cond_nz60 = icmp ne i8 %cond_load59, 0
  br i1 %cond_nz60, label %bb20, label %bb21

bb18:                                             ; preds = %bb16
  %"hew_vec_free drop61" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop61")
  store ptr null, ptr %local_8, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb19:                                             ; preds = %bb16
  %move_load62 = load i64, ptr %local_22, align 8
  store i64 %move_load62, ptr %local_24, align 8
  %move_load63 = load i64, ptr %local_24, align 8
  store i64 %move_load63, ptr %local_17, align 8
  %hew_actor_cooperate64 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel65 = icmp eq i32 %hew_actor_cooperate64, 2
  br i1 %hew_cooperate_is_cancel65, label %cancel_exit66, label %after_cooperate67

bb20:                                             ; preds = %bb17
  %"hew_vec_free drop69" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop69")
  store ptr null, ptr %local_8, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb21:                                             ; preds = %bb17
  %move_load70 = load i64, ptr %local_26, align 8
  store i64 %move_load70, ptr %local_28, align 8
  %checked_lhs71 = load i64, ptr %local_2, align 8
  %checked_rhs72 = load i64, ptr %local_28, align 8
  %with_overflow73 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %checked_lhs71, i64 %checked_rhs72)
  %checked_result74 = extractvalue { i64, i1 } %with_overflow73, 0
  %checked_overflow75 = extractvalue { i64, i1 } %with_overflow73, 1
  %checked_overflow_widen76 = zext i1 %checked_overflow75 to i8
  store i64 %checked_result74, ptr %local_30, align 8
  store i8 %checked_overflow_widen76, ptr %local_31, align 1
  %cond_load77 = load i8, ptr %local_31, align 1
  %cond_nz78 = icmp ne i8 %cond_load77, 0
  br i1 %cond_nz78, label %bb22, label %bb23

bb22:                                             ; preds = %bb21
  %"hew_vec_free drop79" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop79")
  store ptr null, ptr %local_8, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb23:                                             ; preds = %bb21
  %cmp_lhs80 = load i64, ptr %local_10, align 8
  %cmp_rhs81 = load i64, ptr %local_30, align 8
  %cmp_bit82 = icmp slt i64 %cmp_lhs80, %cmp_rhs81
  %cmp_zext83 = zext i1 %cmp_bit82 to i8
  store i8 %cmp_zext83, ptr %local_32, align 1
  %cond_load84 = load i8, ptr %local_32, align 1
  %cond_nz85 = icmp ne i8 %cond_load84, 0
  br i1 %cond_nz85, label %bb24, label %bb25

bb24:                                             ; preds = %bb23
  %move_load86 = load i64, ptr %local_10, align 8
  store i64 %move_load86, ptr %local_33, align 8
  %move_load87 = load i64, ptr %local_33, align 8
  store i64 %move_load87, ptr %local_29, align 8
  br label %bb26

bb25:                                             ; preds = %bb23
  %checked_lhs88 = load i64, ptr %local_2, align 8
  %checked_rhs89 = load i64, ptr %local_28, align 8
  %with_overflow90 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %checked_lhs88, i64 %checked_rhs89)
  %checked_result91 = extractvalue { i64, i1 } %with_overflow90, 0
  %checked_overflow92 = extractvalue { i64, i1 } %with_overflow90, 1
  %checked_overflow_widen93 = zext i1 %checked_overflow92 to i8
  store i64 %checked_result91, ptr %local_34, align 8
  store i8 %checked_overflow_widen93, ptr %local_35, align 1
  %cond_load94 = load i8, ptr %local_35, align 1
  %cond_nz95 = icmp ne i8 %cond_load94, 0
  br i1 %cond_nz95, label %bb27, label %bb28

bb26:                                             ; preds = %after_cooperate111, %bb24
  %move_load96 = load i64, ptr %local_29, align 8
  store i64 %move_load96, ptr %local_37, align 8
  %checked_lhs97 = load i64, ptr %local_28, align 8
  %checked_rhs98 = load i64, ptr %local_37, align 8
  %with_overflow99 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs97, i64 %checked_rhs98)
  %checked_result100 = extractvalue { i64, i1 } %with_overflow99, 0
  %checked_overflow101 = extractvalue { i64, i1 } %with_overflow99, 1
  %checked_overflow_widen102 = zext i1 %checked_overflow101 to i8
  store i64 %checked_result100, ptr %local_38, align 8
  store i8 %checked_overflow_widen102, ptr %local_39, align 1
  %cond_load103 = load i8, ptr %local_39, align 1
  %cond_nz104 = icmp ne i8 %cond_load103, 0
  br i1 %cond_nz104, label %bb29, label %bb30

bb27:                                             ; preds = %bb25
  %"hew_vec_free drop105" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop105")
  store ptr null, ptr %local_8, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb28:                                             ; preds = %bb25
  %move_load106 = load i64, ptr %local_34, align 8
  store i64 %move_load106, ptr %local_36, align 8
  %move_load107 = load i64, ptr %local_36, align 8
  store i64 %move_load107, ptr %local_29, align 8
  %hew_actor_cooperate108 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel109 = icmp eq i32 %hew_actor_cooperate108, 2
  br i1 %hew_cooperate_is_cancel109, label %cancel_exit110, label %after_cooperate111

bb29:                                             ; preds = %bb26
  %"hew_vec_free drop113" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop113")
  store ptr null, ptr %local_8, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb30:                                             ; preds = %bb26
  %move_load114 = load i64, ptr %local_38, align 8
  store i64 %move_load114, ptr %local_40, align 8
  %call_arg115 = load ptr, ptr %local_0, align 8
  %call_arg116 = load ptr, ptr %local_8, align 8
  %call_arg117 = load i64, ptr %local_15, align 8
  %call_arg118 = load i64, ptr %local_28, align 8
  %call_arg119 = load i64, ptr %local_40, align 8
  %call_result120 = call i64 @"sort$merge_int_run"(ptr %call_arg115, ptr %call_arg116, i64 %call_arg117, i64 %call_arg118, i64 %call_arg119)
  store i64 %call_result120, ptr %local_41, align 8
  br label %bb31

bb31:                                             ; preds = %bb30
  %checked_lhs121 = load i64, ptr %local_12, align 8
  %checked_rhs122 = load i64, ptr %local_41, align 8
  %with_overflow123 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs121, i64 %checked_rhs122)
  %checked_result124 = extractvalue { i64, i1 } %with_overflow123, 0
  %checked_overflow125 = extractvalue { i64, i1 } %with_overflow123, 1
  %checked_overflow_widen126 = zext i1 %checked_overflow125 to i8
  store i64 %checked_result124, ptr %local_42, align 8
  store i8 %checked_overflow_widen126, ptr %local_43, align 1
  %cond_load127 = load i8, ptr %local_43, align 1
  %cond_nz128 = icmp ne i8 %cond_load127, 0
  br i1 %cond_nz128, label %bb32, label %bb33

bb32:                                             ; preds = %bb31
  %"hew_vec_free drop129" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop129")
  store ptr null, ptr %local_8, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb33:                                             ; preds = %bb31
  %move_load130 = load i64, ptr %local_42, align 8
  store i64 %move_load130, ptr %local_12, align 8
  %move_load131 = load i64, ptr %local_40, align 8
  store i64 %move_load131, ptr %local_15, align 8
  %hew_actor_cooperate132 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel133 = icmp eq i32 %hew_actor_cooperate132, 2
  br i1 %hew_cooperate_is_cancel133, label %cancel_exit134, label %after_cooperate135

bb34:                                             ; preds = %after_cooperate169, %bb36
  %cmp_lhs137 = load i64, ptr %local_44, align 8
  %cmp_rhs138 = load i64, ptr %local_45, align 8
  %cmp_bit139 = icmp slt i64 %cmp_lhs137, %cmp_rhs138
  %cmp_zext140 = zext i1 %cmp_bit139 to i8
  store i8 %cmp_zext140, ptr %local_51, align 1
  %cond_load141 = load i8, ptr %local_51, align 1
  %cond_nz142 = icmp ne i8 %cond_load141, 0
  br i1 %cond_nz142, label %bb35, label %bb37

bb35:                                             ; preds = %bb34
  %"hew_vec_len arg0" = load ptr, ptr %local_8, align 8
  %hew_vec_len_call = call i64 @hew_vec_len(ptr %"hew_vec_len arg0")
  store i64 %hew_vec_len_call, ptr %local_52, align 8
  %cmp_lhs143 = load i64, ptr %local_44, align 8
  %cmp_rhs144 = load i64, ptr %local_52, align 8
  %cmp_bit145 = icmp uge i64 %cmp_lhs143, %cmp_rhs144
  %cmp_zext146 = zext i1 %cmp_bit145 to i8
  store i8 %cmp_zext146, ptr %local_53, align 1
  %cond_load147 = load i8, ptr %local_53, align 1
  %cond_nz148 = icmp ne i8 %cond_load147, 0
  br i1 %cond_nz148, label %bb40, label %bb41

bb36:                                             ; preds = %after_cooperate179
  %checked_lhs149 = load i64, ptr %local_44, align 8
  %checked_rhs150 = load i64, ptr %local_47, align 8
  %with_overflow151 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs149, i64 %checked_rhs150)
  %checked_result152 = extractvalue { i64, i1 } %with_overflow151, 0
  %checked_overflow153 = extractvalue { i64, i1 } %with_overflow151, 1
  %checked_overflow_widen154 = zext i1 %checked_overflow153 to i8
  store i64 %checked_result152, ptr %local_44, align 8
  store i8 %checked_overflow_widen154, ptr %local_56, align 1
  %cond_load155 = load i8, ptr %local_56, align 1
  %cond_nz156 = icmp ne i8 %cond_load155, 0
  br i1 %cond_nz156, label %bb43, label %bb34

bb37:                                             ; preds = %bb34
  store i64 2, ptr %local_58, align 8
  store i64 0, ptr %local_60, align 8
  %cmp_lhs157 = load i64, ptr %local_58, align 8
  %cmp_rhs158 = load i64, ptr %local_60, align 8
  %cmp_bit159 = icmp eq i64 %cmp_lhs157, %cmp_rhs158
  %cmp_zext160 = zext i1 %cmp_bit159 to i8
  store i8 %cmp_zext160, ptr %local_61, align 1
  %cond_load161 = load i8, ptr %local_61, align 1
  %cond_nz162 = icmp ne i8 %cond_load161, 0
  br i1 %cond_nz162, label %bb44, label %bb45

bb38:                                             ; preds = %bb12
  %"hew_vec_free drop163" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop163")
  store ptr null, ptr %local_8, align 8
  call void @hew_trap_with_code(i32 202)
  call void @llvm.trap()
  unreachable

bb39:                                             ; preds = %bb12
  store i64 0, ptr %local_50, align 8
  %move_load164 = load i64, ptr %local_50, align 8
  store i64 %move_load164, ptr %local_44, align 8
  %move_load165 = load i64, ptr %local_2, align 8
  store i64 %move_load165, ptr %local_45, align 8
  %hew_actor_cooperate166 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel167 = icmp eq i32 %hew_actor_cooperate166, 2
  br i1 %hew_cooperate_is_cancel167, label %cancel_exit168, label %after_cooperate169

bb40:                                             ; preds = %bb35
  %"hew_vec_free drop171" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop171")
  store ptr null, ptr %local_8, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb41:                                             ; preds = %bb35
  %"hew_vec_get_i64 arg0" = load ptr, ptr %local_8, align 8
  %"hew_vec_get_i64 arg1" = load i64, ptr %local_44, align 8
  %hew_vec_get_i64_call = call i64 @hew_vec_get_i64(ptr %"hew_vec_get_i64 arg0", i64 %"hew_vec_get_i64 arg1")
  store i64 %hew_vec_get_i64_call, ptr %local_54, align 8
  %move_load172 = load i64, ptr %local_54, align 8
  store i64 %move_load172, ptr %local_55, align 8
  %call_arg173 = load ptr, ptr %local_0, align 8
  %call_arg174 = load i64, ptr %local_44, align 8
  %call_arg175 = load i64, ptr %local_55, align 8
  call void @hew_vec_set_i64(ptr %call_arg173, i64 %call_arg174, i64 %call_arg175)
  br label %bb42

bb42:                                             ; preds = %bb41
  %hew_actor_cooperate176 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel177 = icmp eq i32 %hew_actor_cooperate176, 2
  br i1 %hew_cooperate_is_cancel177, label %cancel_exit178, label %after_cooperate179

bb43:                                             ; preds = %bb36
  %"hew_vec_free drop181" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop181")
  store ptr null, ptr %local_8, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb44:                                             ; preds = %bb37
  %"hew_vec_free drop182" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop182")
  store ptr null, ptr %local_8, align 8
  call void @hew_trap_with_code(i32 202)
  call void @llvm.trap()
  unreachable

bb45:                                             ; preds = %bb37
  store i64 -9223372036854775808, ptr %local_62, align 8
  %cmp_lhs183 = load i64, ptr %local_2, align 8
  %cmp_rhs184 = load i64, ptr %local_62, align 8
  %cmp_bit185 = icmp eq i64 %cmp_lhs183, %cmp_rhs184
  %cmp_zext186 = zext i1 %cmp_bit185 to i8
  store i8 %cmp_zext186, ptr %local_63, align 1
  %cond_load187 = load i8, ptr %local_63, align 1
  %cond_nz188 = icmp ne i8 %cond_load187, 0
  br i1 %cond_nz188, label %bb46, label %bb47

bb46:                                             ; preds = %bb45
  store i64 -1, ptr %local_64, align 8
  %cmp_lhs189 = load i64, ptr %local_58, align 8
  %cmp_rhs190 = load i64, ptr %local_64, align 8
  %cmp_bit191 = icmp eq i64 %cmp_lhs189, %cmp_rhs190
  %cmp_zext192 = zext i1 %cmp_bit191 to i8
  store i8 %cmp_zext192, ptr %local_65, align 1
  %cond_load193 = load i8, ptr %local_65, align 1
  %cond_nz194 = icmp ne i8 %cond_load193, 0
  br i1 %cond_nz194, label %bb48, label %bb47

bb47:                                             ; preds = %bb46, %bb45
  %div_lhs = load i64, ptr %local_2, align 8
  %div_rhs = load i64, ptr %local_58, align 8
  %sdiv = sdiv i64 %div_lhs, %div_rhs
  store i64 %sdiv, ptr %local_59, align 8
  %cmp_lhs195 = load i64, ptr %local_10, align 8
  %cmp_rhs196 = load i64, ptr %local_59, align 8
  %cmp_bit197 = icmp sgt i64 %cmp_lhs195, %cmp_rhs196
  %cmp_zext198 = zext i1 %cmp_bit197 to i8
  store i8 %cmp_zext198, ptr %local_66, align 1
  %cond_load199 = load i8, ptr %local_66, align 1
  %cond_nz200 = icmp ne i8 %cond_load199, 0
  br i1 %cond_nz200, label %bb49, label %bb50

bb48:                                             ; preds = %bb46
  %"hew_vec_free drop201" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop201")
  store ptr null, ptr %local_8, align 8
  call void @hew_trap_with_code(i32 203)
  call void @llvm.trap()
  unreachable

bb49:                                             ; preds = %bb47
  %move_load202 = load i64, ptr %local_2, align 8
  store i64 %move_load202, ptr %local_67, align 8
  %move_load203 = load i64, ptr %local_67, align 8
  store i64 %move_load203, ptr %local_57, align 8
  br label %bb51

bb50:                                             ; preds = %bb47
  store i64 2, ptr %local_68, align 8
  %checked_lhs204 = load i64, ptr %local_10, align 8
  %checked_rhs205 = load i64, ptr %local_68, align 8
  %with_overflow206 = call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %checked_lhs204, i64 %checked_rhs205)
  %checked_result207 = extractvalue { i64, i1 } %with_overflow206, 0
  %checked_overflow208 = extractvalue { i64, i1 } %with_overflow206, 1
  %checked_overflow_widen209 = zext i1 %checked_overflow208 to i8
  store i64 %checked_result207, ptr %local_69, align 8
  store i8 %checked_overflow_widen209, ptr %local_70, align 1
  %cond_load210 = load i8, ptr %local_70, align 1
  %cond_nz211 = icmp ne i8 %cond_load210, 0
  br i1 %cond_nz211, label %bb52, label %bb53

bb51:                                             ; preds = %after_cooperate224, %bb49
  %move_load212 = load i64, ptr %local_57, align 8
  store i64 %move_load212, ptr %local_10, align 8
  %hew_actor_cooperate213 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel214 = icmp eq i32 %hew_actor_cooperate213, 2
  br i1 %hew_cooperate_is_cancel214, label %cancel_exit215, label %after_cooperate216

bb52:                                             ; preds = %bb50
  %"hew_vec_free drop218" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop218")
  store ptr null, ptr %local_8, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb53:                                             ; preds = %bb50
  %move_load219 = load i64, ptr %local_69, align 8
  store i64 %move_load219, ptr %local_71, align 8
  %move_load220 = load i64, ptr %local_71, align 8
  store i64 %move_load220, ptr %local_57, align 8
  %hew_actor_cooperate221 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel222 = icmp eq i32 %hew_actor_cooperate221, 2
  br i1 %hew_cooperate_is_cancel222, label %cancel_exit223, label %after_cooperate224

cancel_exit:                                      ; preds = %entry
  ret i64 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit6:                                     ; preds = %bb5
  ret i64 0

after_cooperate7:                                 ; preds = %bb5
  br label %bb4

cancel_exit66:                                    ; preds = %bb19
  %"hew_vec_free drop68" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop68")
  store ptr null, ptr %local_8, align 8
  ret i64 0

after_cooperate67:                                ; preds = %bb19
  br label %bb17

cancel_exit110:                                   ; preds = %bb28
  %"hew_vec_free drop112" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop112")
  store ptr null, ptr %local_8, align 8
  ret i64 0

after_cooperate111:                               ; preds = %bb28
  br label %bb26

cancel_exit134:                                   ; preds = %bb33
  %"hew_vec_free drop136" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop136")
  store ptr null, ptr %local_8, align 8
  ret i64 0

after_cooperate135:                               ; preds = %bb33
  br label %bb10

cancel_exit168:                                   ; preds = %bb39
  %"hew_vec_free drop170" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop170")
  store ptr null, ptr %local_8, align 8
  ret i64 0

after_cooperate169:                               ; preds = %bb39
  br label %bb34

cancel_exit178:                                   ; preds = %bb42
  %"hew_vec_free drop180" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop180")
  store ptr null, ptr %local_8, align 8
  ret i64 0

after_cooperate179:                               ; preds = %bb42
  br label %bb36

cancel_exit215:                                   ; preds = %bb51
  %"hew_vec_free drop217" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop217")
  store ptr null, ptr %local_8, align 8
  ret i64 0

after_cooperate216:                               ; preds = %bb51
  br label %bb7

cancel_exit223:                                   ; preds = %bb53
  %"hew_vec_free drop225" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop225")
  store ptr null, ptr %local_8, align 8
  ret i64 0

after_cooperate224:                               ; preds = %bb53
  br label %bb51
}

define internal i64 @"sort$sort_strings_in_place"(ptr %0) {
entry:
  %return_slot = alloca i64, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i8, align 1
  %local_4 = alloca i64, align 8
  %local_5 = alloca i8, align 1
  %local_6 = alloca i64, align 8
  %local_7 = alloca ptr, align 8
  %local_8 = alloca ptr, align 8
  %local_9 = alloca i64, align 8
  %local_10 = alloca i64, align 8
  %local_11 = alloca i64, align 8
  %local_12 = alloca i64, align 8
  %local_13 = alloca i8, align 1
  %local_14 = alloca i64, align 8
  %local_15 = alloca i64, align 8
  %local_16 = alloca i8, align 1
  %local_17 = alloca i64, align 8
  %local_18 = alloca i64, align 8
  %local_19 = alloca i8, align 1
  %local_20 = alloca i8, align 1
  %local_21 = alloca i64, align 8
  %local_22 = alloca i64, align 8
  %local_23 = alloca i8, align 1
  %local_24 = alloca i64, align 8
  %local_25 = alloca i64, align 8
  %local_26 = alloca i64, align 8
  %local_27 = alloca i8, align 1
  %local_28 = alloca i64, align 8
  %local_29 = alloca i64, align 8
  %local_30 = alloca i64, align 8
  %local_31 = alloca i8, align 1
  %local_32 = alloca i8, align 1
  %local_33 = alloca i64, align 8
  %local_34 = alloca i64, align 8
  %local_35 = alloca i8, align 1
  %local_36 = alloca i64, align 8
  %local_37 = alloca i64, align 8
  %local_38 = alloca i64, align 8
  %local_39 = alloca i8, align 1
  %local_40 = alloca i64, align 8
  %local_41 = alloca i64, align 8
  %local_42 = alloca i64, align 8
  %local_43 = alloca i8, align 1
  %local_44 = alloca i64, align 8
  %local_45 = alloca i64, align 8
  %local_46 = alloca i64, align 8
  %local_47 = alloca i64, align 8
  %local_48 = alloca i64, align 8
  %local_49 = alloca i8, align 1
  %local_50 = alloca i64, align 8
  %local_51 = alloca i8, align 1
  %local_52 = alloca i64, align 8
  %local_53 = alloca i8, align 1
  %local_54 = alloca ptr, align 8
  %local_55 = alloca ptr, align 8
  %local_56 = alloca i8, align 1
  %local_57 = alloca i64, align 8
  %local_58 = alloca i64, align 8
  %local_59 = alloca i64, align 8
  %local_60 = alloca i64, align 8
  %local_61 = alloca i8, align 1
  %local_62 = alloca i64, align 8
  %local_63 = alloca i8, align 1
  %local_64 = alloca i64, align 8
  %local_65 = alloca i8, align 1
  %local_66 = alloca i8, align 1
  %local_67 = alloca i64, align 8
  %local_68 = alloca i64, align 8
  %local_69 = alloca i64, align 8
  %local_70 = alloca i8, align 1
  %local_71 = alloca i64, align 8
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result = call i64 @hew_vec_len(ptr %call_arg)
  store i64 %call_result, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load i64, ptr %local_1, align 8
  store i64 %move_load, ptr %local_2, align 8
  store i64 2, ptr %local_4, align 8
  %cmp_lhs = load i64, ptr %local_2, align 8
  %cmp_rhs = load i64, ptr %local_4, align 8
  %cmp_bit = icmp slt i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_5, align 1
  %cond_load = load i8, ptr %local_5, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  store i64 0, ptr %local_6, align 8
  %move_load1 = load i64, ptr %local_6, align 8
  store i64 %move_load1, ptr %return_slot, align 8
  %ret_val = load i64, ptr %return_slot, align 8
  ret i64 %ret_val

bb3:                                              ; preds = %bb1
  br label %bb4

bb4:                                              ; preds = %after_cooperate7, %bb3
  %call_arg2 = load ptr, ptr %local_0, align 8
  %call_result3 = call ptr @"sort$copy_strings"(ptr %call_arg2)
  store ptr %call_result3, ptr %local_7, align 8
  br label %bb6

bb5:                                              ; No predecessors!
  %hew_actor_cooperate4 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel5 = icmp eq i32 %hew_actor_cooperate4, 2
  br i1 %hew_cooperate_is_cancel5, label %cancel_exit6, label %after_cooperate7

bb6:                                              ; preds = %bb4
  %move_load8 = load ptr, ptr %local_7, align 8
  store ptr %move_load8, ptr %local_8, align 8
  store i64 1, ptr %local_9, align 8
  %move_load9 = load i64, ptr %local_9, align 8
  store i64 %move_load9, ptr %local_10, align 8
  store i64 0, ptr %local_11, align 8
  %move_load10 = load i64, ptr %local_11, align 8
  store i64 %move_load10, ptr %local_12, align 8
  br label %bb7

bb7:                                              ; preds = %after_cooperate218, %bb6
  %cmp_lhs11 = load i64, ptr %local_10, align 8
  %cmp_rhs12 = load i64, ptr %local_2, align 8
  %cmp_bit13 = icmp slt i64 %cmp_lhs11, %cmp_rhs12
  %cmp_zext14 = zext i1 %cmp_bit13 to i8
  store i8 %cmp_zext14, ptr %local_13, align 1
  %cond_load15 = load i8, ptr %local_13, align 1
  %cond_nz16 = icmp ne i8 %cond_load15, 0
  br i1 %cond_nz16, label %bb8, label %bb9

bb8:                                              ; preds = %bb7
  store i64 0, ptr %local_14, align 8
  %move_load17 = load i64, ptr %local_14, align 8
  store i64 %move_load17, ptr %local_15, align 8
  br label %bb10

bb9:                                              ; preds = %bb7
  %move_load18 = load i64, ptr %local_12, align 8
  store i64 %move_load18, ptr %return_slot, align 8
  %"hew_vec_free drop" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop")
  store ptr null, ptr %local_8, align 8
  %ret_val19 = load i64, ptr %return_slot, align 8
  ret i64 %ret_val19

bb10:                                             ; preds = %after_cooperate135, %bb8
  %cmp_lhs20 = load i64, ptr %local_15, align 8
  %cmp_rhs21 = load i64, ptr %local_2, align 8
  %cmp_bit22 = icmp slt i64 %cmp_lhs20, %cmp_rhs21
  %cmp_zext23 = zext i1 %cmp_bit22 to i8
  store i8 %cmp_zext23, ptr %local_16, align 1
  %cond_load24 = load i8, ptr %local_16, align 1
  %cond_nz25 = icmp ne i8 %cond_load24, 0
  br i1 %cond_nz25, label %bb11, label %bb12

bb11:                                             ; preds = %bb10
  %checked_lhs = load i64, ptr %local_2, align 8
  %checked_rhs = load i64, ptr %local_15, align 8
  %with_overflow = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_18, align 8
  store i8 %checked_overflow_widen, ptr %local_19, align 1
  %cond_load26 = load i8, ptr %local_19, align 1
  %cond_nz27 = icmp ne i8 %cond_load26, 0
  br i1 %cond_nz27, label %bb13, label %bb14

bb12:                                             ; preds = %bb10
  store i64 1, ptr %local_46, align 8
  %move_load28 = load i64, ptr %local_46, align 8
  store i64 %move_load28, ptr %local_47, align 8
  store i64 0, ptr %local_48, align 8
  %cmp_lhs29 = load i64, ptr %local_47, align 8
  %cmp_rhs30 = load i64, ptr %local_48, align 8
  %cmp_bit31 = icmp sle i64 %cmp_lhs29, %cmp_rhs30
  %cmp_zext32 = zext i1 %cmp_bit31 to i8
  store i8 %cmp_zext32, ptr %local_49, align 1
  %cond_load33 = load i8, ptr %local_49, align 1
  %cond_nz34 = icmp ne i8 %cond_load33, 0
  br i1 %cond_nz34, label %bb38, label %bb39

bb13:                                             ; preds = %bb11
  %"hew_vec_free drop35" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop35")
  store ptr null, ptr %local_8, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb14:                                             ; preds = %bb11
  %cmp_lhs36 = load i64, ptr %local_10, align 8
  %cmp_rhs37 = load i64, ptr %local_18, align 8
  %cmp_bit38 = icmp slt i64 %cmp_lhs36, %cmp_rhs37
  %cmp_zext39 = zext i1 %cmp_bit38 to i8
  store i8 %cmp_zext39, ptr %local_20, align 1
  %cond_load40 = load i8, ptr %local_20, align 1
  %cond_nz41 = icmp ne i8 %cond_load40, 0
  br i1 %cond_nz41, label %bb15, label %bb16

bb15:                                             ; preds = %bb14
  %move_load42 = load i64, ptr %local_10, align 8
  store i64 %move_load42, ptr %local_21, align 8
  %move_load43 = load i64, ptr %local_21, align 8
  store i64 %move_load43, ptr %local_17, align 8
  br label %bb17

bb16:                                             ; preds = %bb14
  %checked_lhs44 = load i64, ptr %local_2, align 8
  %checked_rhs45 = load i64, ptr %local_15, align 8
  %with_overflow46 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %checked_lhs44, i64 %checked_rhs45)
  %checked_result47 = extractvalue { i64, i1 } %with_overflow46, 0
  %checked_overflow48 = extractvalue { i64, i1 } %with_overflow46, 1
  %checked_overflow_widen49 = zext i1 %checked_overflow48 to i8
  store i64 %checked_result47, ptr %local_22, align 8
  store i8 %checked_overflow_widen49, ptr %local_23, align 1
  %cond_load50 = load i8, ptr %local_23, align 1
  %cond_nz51 = icmp ne i8 %cond_load50, 0
  br i1 %cond_nz51, label %bb18, label %bb19

bb17:                                             ; preds = %after_cooperate67, %bb15
  %move_load52 = load i64, ptr %local_17, align 8
  store i64 %move_load52, ptr %local_25, align 8
  %checked_lhs53 = load i64, ptr %local_15, align 8
  %checked_rhs54 = load i64, ptr %local_25, align 8
  %with_overflow55 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs53, i64 %checked_rhs54)
  %checked_result56 = extractvalue { i64, i1 } %with_overflow55, 0
  %checked_overflow57 = extractvalue { i64, i1 } %with_overflow55, 1
  %checked_overflow_widen58 = zext i1 %checked_overflow57 to i8
  store i64 %checked_result56, ptr %local_26, align 8
  store i8 %checked_overflow_widen58, ptr %local_27, align 1
  %cond_load59 = load i8, ptr %local_27, align 1
  %cond_nz60 = icmp ne i8 %cond_load59, 0
  br i1 %cond_nz60, label %bb20, label %bb21

bb18:                                             ; preds = %bb16
  %"hew_vec_free drop61" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop61")
  store ptr null, ptr %local_8, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb19:                                             ; preds = %bb16
  %move_load62 = load i64, ptr %local_22, align 8
  store i64 %move_load62, ptr %local_24, align 8
  %move_load63 = load i64, ptr %local_24, align 8
  store i64 %move_load63, ptr %local_17, align 8
  %hew_actor_cooperate64 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel65 = icmp eq i32 %hew_actor_cooperate64, 2
  br i1 %hew_cooperate_is_cancel65, label %cancel_exit66, label %after_cooperate67

bb20:                                             ; preds = %bb17
  %"hew_vec_free drop69" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop69")
  store ptr null, ptr %local_8, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb21:                                             ; preds = %bb17
  %move_load70 = load i64, ptr %local_26, align 8
  store i64 %move_load70, ptr %local_28, align 8
  %checked_lhs71 = load i64, ptr %local_2, align 8
  %checked_rhs72 = load i64, ptr %local_28, align 8
  %with_overflow73 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %checked_lhs71, i64 %checked_rhs72)
  %checked_result74 = extractvalue { i64, i1 } %with_overflow73, 0
  %checked_overflow75 = extractvalue { i64, i1 } %with_overflow73, 1
  %checked_overflow_widen76 = zext i1 %checked_overflow75 to i8
  store i64 %checked_result74, ptr %local_30, align 8
  store i8 %checked_overflow_widen76, ptr %local_31, align 1
  %cond_load77 = load i8, ptr %local_31, align 1
  %cond_nz78 = icmp ne i8 %cond_load77, 0
  br i1 %cond_nz78, label %bb22, label %bb23

bb22:                                             ; preds = %bb21
  %"hew_vec_free drop79" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop79")
  store ptr null, ptr %local_8, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb23:                                             ; preds = %bb21
  %cmp_lhs80 = load i64, ptr %local_10, align 8
  %cmp_rhs81 = load i64, ptr %local_30, align 8
  %cmp_bit82 = icmp slt i64 %cmp_lhs80, %cmp_rhs81
  %cmp_zext83 = zext i1 %cmp_bit82 to i8
  store i8 %cmp_zext83, ptr %local_32, align 1
  %cond_load84 = load i8, ptr %local_32, align 1
  %cond_nz85 = icmp ne i8 %cond_load84, 0
  br i1 %cond_nz85, label %bb24, label %bb25

bb24:                                             ; preds = %bb23
  %move_load86 = load i64, ptr %local_10, align 8
  store i64 %move_load86, ptr %local_33, align 8
  %move_load87 = load i64, ptr %local_33, align 8
  store i64 %move_load87, ptr %local_29, align 8
  br label %bb26

bb25:                                             ; preds = %bb23
  %checked_lhs88 = load i64, ptr %local_2, align 8
  %checked_rhs89 = load i64, ptr %local_28, align 8
  %with_overflow90 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %checked_lhs88, i64 %checked_rhs89)
  %checked_result91 = extractvalue { i64, i1 } %with_overflow90, 0
  %checked_overflow92 = extractvalue { i64, i1 } %with_overflow90, 1
  %checked_overflow_widen93 = zext i1 %checked_overflow92 to i8
  store i64 %checked_result91, ptr %local_34, align 8
  store i8 %checked_overflow_widen93, ptr %local_35, align 1
  %cond_load94 = load i8, ptr %local_35, align 1
  %cond_nz95 = icmp ne i8 %cond_load94, 0
  br i1 %cond_nz95, label %bb27, label %bb28

bb26:                                             ; preds = %after_cooperate111, %bb24
  %move_load96 = load i64, ptr %local_29, align 8
  store i64 %move_load96, ptr %local_37, align 8
  %checked_lhs97 = load i64, ptr %local_28, align 8
  %checked_rhs98 = load i64, ptr %local_37, align 8
  %with_overflow99 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs97, i64 %checked_rhs98)
  %checked_result100 = extractvalue { i64, i1 } %with_overflow99, 0
  %checked_overflow101 = extractvalue { i64, i1 } %with_overflow99, 1
  %checked_overflow_widen102 = zext i1 %checked_overflow101 to i8
  store i64 %checked_result100, ptr %local_38, align 8
  store i8 %checked_overflow_widen102, ptr %local_39, align 1
  %cond_load103 = load i8, ptr %local_39, align 1
  %cond_nz104 = icmp ne i8 %cond_load103, 0
  br i1 %cond_nz104, label %bb29, label %bb30

bb27:                                             ; preds = %bb25
  %"hew_vec_free drop105" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop105")
  store ptr null, ptr %local_8, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb28:                                             ; preds = %bb25
  %move_load106 = load i64, ptr %local_34, align 8
  store i64 %move_load106, ptr %local_36, align 8
  %move_load107 = load i64, ptr %local_36, align 8
  store i64 %move_load107, ptr %local_29, align 8
  %hew_actor_cooperate108 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel109 = icmp eq i32 %hew_actor_cooperate108, 2
  br i1 %hew_cooperate_is_cancel109, label %cancel_exit110, label %after_cooperate111

bb29:                                             ; preds = %bb26
  %"hew_vec_free drop113" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop113")
  store ptr null, ptr %local_8, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb30:                                             ; preds = %bb26
  %move_load114 = load i64, ptr %local_38, align 8
  store i64 %move_load114, ptr %local_40, align 8
  %call_arg115 = load ptr, ptr %local_0, align 8
  %call_arg116 = load ptr, ptr %local_8, align 8
  %call_arg117 = load i64, ptr %local_15, align 8
  %call_arg118 = load i64, ptr %local_28, align 8
  %call_arg119 = load i64, ptr %local_40, align 8
  %call_result120 = call i64 @"sort$merge_string_run"(ptr %call_arg115, ptr %call_arg116, i64 %call_arg117, i64 %call_arg118, i64 %call_arg119)
  store i64 %call_result120, ptr %local_41, align 8
  br label %bb31

bb31:                                             ; preds = %bb30
  %checked_lhs121 = load i64, ptr %local_12, align 8
  %checked_rhs122 = load i64, ptr %local_41, align 8
  %with_overflow123 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs121, i64 %checked_rhs122)
  %checked_result124 = extractvalue { i64, i1 } %with_overflow123, 0
  %checked_overflow125 = extractvalue { i64, i1 } %with_overflow123, 1
  %checked_overflow_widen126 = zext i1 %checked_overflow125 to i8
  store i64 %checked_result124, ptr %local_42, align 8
  store i8 %checked_overflow_widen126, ptr %local_43, align 1
  %cond_load127 = load i8, ptr %local_43, align 1
  %cond_nz128 = icmp ne i8 %cond_load127, 0
  br i1 %cond_nz128, label %bb32, label %bb33

bb32:                                             ; preds = %bb31
  %"hew_vec_free drop129" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop129")
  store ptr null, ptr %local_8, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb33:                                             ; preds = %bb31
  %move_load130 = load i64, ptr %local_42, align 8
  store i64 %move_load130, ptr %local_12, align 8
  %move_load131 = load i64, ptr %local_40, align 8
  store i64 %move_load131, ptr %local_15, align 8
  %hew_actor_cooperate132 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel133 = icmp eq i32 %hew_actor_cooperate132, 2
  br i1 %hew_cooperate_is_cancel133, label %cancel_exit134, label %after_cooperate135

bb34:                                             ; preds = %after_cooperate169, %bb36
  %cmp_lhs137 = load i64, ptr %local_44, align 8
  %cmp_rhs138 = load i64, ptr %local_45, align 8
  %cmp_bit139 = icmp slt i64 %cmp_lhs137, %cmp_rhs138
  %cmp_zext140 = zext i1 %cmp_bit139 to i8
  store i8 %cmp_zext140, ptr %local_51, align 1
  %cond_load141 = load i8, ptr %local_51, align 1
  %cond_nz142 = icmp ne i8 %cond_load141, 0
  br i1 %cond_nz142, label %bb35, label %bb37

bb35:                                             ; preds = %bb34
  %"hew_vec_len arg0" = load ptr, ptr %local_8, align 8
  %hew_vec_len_call = call i64 @hew_vec_len(ptr %"hew_vec_len arg0")
  store i64 %hew_vec_len_call, ptr %local_52, align 8
  %cmp_lhs143 = load i64, ptr %local_44, align 8
  %cmp_rhs144 = load i64, ptr %local_52, align 8
  %cmp_bit145 = icmp uge i64 %cmp_lhs143, %cmp_rhs144
  %cmp_zext146 = zext i1 %cmp_bit145 to i8
  store i8 %cmp_zext146, ptr %local_53, align 1
  %cond_load147 = load i8, ptr %local_53, align 1
  %cond_nz148 = icmp ne i8 %cond_load147, 0
  br i1 %cond_nz148, label %bb40, label %bb41

bb36:                                             ; preds = %after_cooperate179
  %checked_lhs149 = load i64, ptr %local_44, align 8
  %checked_rhs150 = load i64, ptr %local_47, align 8
  %with_overflow151 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs149, i64 %checked_rhs150)
  %checked_result152 = extractvalue { i64, i1 } %with_overflow151, 0
  %checked_overflow153 = extractvalue { i64, i1 } %with_overflow151, 1
  %checked_overflow_widen154 = zext i1 %checked_overflow153 to i8
  store i64 %checked_result152, ptr %local_44, align 8
  store i8 %checked_overflow_widen154, ptr %local_56, align 1
  %cond_load155 = load i8, ptr %local_56, align 1
  %cond_nz156 = icmp ne i8 %cond_load155, 0
  br i1 %cond_nz156, label %bb43, label %bb34

bb37:                                             ; preds = %bb34
  store i64 2, ptr %local_58, align 8
  store i64 0, ptr %local_60, align 8
  %cmp_lhs157 = load i64, ptr %local_58, align 8
  %cmp_rhs158 = load i64, ptr %local_60, align 8
  %cmp_bit159 = icmp eq i64 %cmp_lhs157, %cmp_rhs158
  %cmp_zext160 = zext i1 %cmp_bit159 to i8
  store i8 %cmp_zext160, ptr %local_61, align 1
  %cond_load161 = load i8, ptr %local_61, align 1
  %cond_nz162 = icmp ne i8 %cond_load161, 0
  br i1 %cond_nz162, label %bb44, label %bb45

bb38:                                             ; preds = %bb12
  %"hew_vec_free drop163" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop163")
  store ptr null, ptr %local_8, align 8
  call void @hew_trap_with_code(i32 202)
  call void @llvm.trap()
  unreachable

bb39:                                             ; preds = %bb12
  store i64 0, ptr %local_50, align 8
  %move_load164 = load i64, ptr %local_50, align 8
  store i64 %move_load164, ptr %local_44, align 8
  %move_load165 = load i64, ptr %local_2, align 8
  store i64 %move_load165, ptr %local_45, align 8
  %hew_actor_cooperate166 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel167 = icmp eq i32 %hew_actor_cooperate166, 2
  br i1 %hew_cooperate_is_cancel167, label %cancel_exit168, label %after_cooperate169

bb40:                                             ; preds = %bb35
  %"hew_vec_free drop171" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop171")
  store ptr null, ptr %local_8, align 8
  call void @hew_trap_with_code(i32 205)
  call void @llvm.trap()
  unreachable

bb41:                                             ; preds = %bb35
  %"hew_vec_get_str arg0" = load ptr, ptr %local_8, align 8
  %"hew_vec_get_str arg1" = load i64, ptr %local_44, align 8
  %hew_vec_get_str_call = call ptr @hew_vec_get_str(ptr %"hew_vec_get_str arg0", i64 %"hew_vec_get_str arg1")
  store ptr %hew_vec_get_str_call, ptr %local_54, align 8
  %move_load172 = load ptr, ptr %local_54, align 8
  store ptr %move_load172, ptr %local_55, align 8
  %call_arg173 = load ptr, ptr %local_0, align 8
  %call_arg174 = load i64, ptr %local_44, align 8
  %call_arg175 = load ptr, ptr %local_55, align 8
  call void @hew_vec_set_str(ptr %call_arg173, i64 %call_arg174, ptr %call_arg175)
  br label %bb42

bb42:                                             ; preds = %bb41
  %hew_actor_cooperate176 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel177 = icmp eq i32 %hew_actor_cooperate176, 2
  br i1 %hew_cooperate_is_cancel177, label %cancel_exit178, label %after_cooperate179

bb43:                                             ; preds = %bb36
  %"hew_string_drop drop182" = load ptr, ptr %local_55, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop182")
  store ptr null, ptr %local_55, align 8
  %"hew_vec_free drop183" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop183")
  store ptr null, ptr %local_8, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb44:                                             ; preds = %bb37
  %"hew_vec_free drop184" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop184")
  store ptr null, ptr %local_8, align 8
  call void @hew_trap_with_code(i32 202)
  call void @llvm.trap()
  unreachable

bb45:                                             ; preds = %bb37
  store i64 -9223372036854775808, ptr %local_62, align 8
  %cmp_lhs185 = load i64, ptr %local_2, align 8
  %cmp_rhs186 = load i64, ptr %local_62, align 8
  %cmp_bit187 = icmp eq i64 %cmp_lhs185, %cmp_rhs186
  %cmp_zext188 = zext i1 %cmp_bit187 to i8
  store i8 %cmp_zext188, ptr %local_63, align 1
  %cond_load189 = load i8, ptr %local_63, align 1
  %cond_nz190 = icmp ne i8 %cond_load189, 0
  br i1 %cond_nz190, label %bb46, label %bb47

bb46:                                             ; preds = %bb45
  store i64 -1, ptr %local_64, align 8
  %cmp_lhs191 = load i64, ptr %local_58, align 8
  %cmp_rhs192 = load i64, ptr %local_64, align 8
  %cmp_bit193 = icmp eq i64 %cmp_lhs191, %cmp_rhs192
  %cmp_zext194 = zext i1 %cmp_bit193 to i8
  store i8 %cmp_zext194, ptr %local_65, align 1
  %cond_load195 = load i8, ptr %local_65, align 1
  %cond_nz196 = icmp ne i8 %cond_load195, 0
  br i1 %cond_nz196, label %bb48, label %bb47

bb47:                                             ; preds = %bb46, %bb45
  %div_lhs = load i64, ptr %local_2, align 8
  %div_rhs = load i64, ptr %local_58, align 8
  %sdiv = sdiv i64 %div_lhs, %div_rhs
  store i64 %sdiv, ptr %local_59, align 8
  %cmp_lhs197 = load i64, ptr %local_10, align 8
  %cmp_rhs198 = load i64, ptr %local_59, align 8
  %cmp_bit199 = icmp sgt i64 %cmp_lhs197, %cmp_rhs198
  %cmp_zext200 = zext i1 %cmp_bit199 to i8
  store i8 %cmp_zext200, ptr %local_66, align 1
  %cond_load201 = load i8, ptr %local_66, align 1
  %cond_nz202 = icmp ne i8 %cond_load201, 0
  br i1 %cond_nz202, label %bb49, label %bb50

bb48:                                             ; preds = %bb46
  %"hew_vec_free drop203" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop203")
  store ptr null, ptr %local_8, align 8
  call void @hew_trap_with_code(i32 203)
  call void @llvm.trap()
  unreachable

bb49:                                             ; preds = %bb47
  %move_load204 = load i64, ptr %local_2, align 8
  store i64 %move_load204, ptr %local_67, align 8
  %move_load205 = load i64, ptr %local_67, align 8
  store i64 %move_load205, ptr %local_57, align 8
  br label %bb51

bb50:                                             ; preds = %bb47
  store i64 2, ptr %local_68, align 8
  %checked_lhs206 = load i64, ptr %local_10, align 8
  %checked_rhs207 = load i64, ptr %local_68, align 8
  %with_overflow208 = call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %checked_lhs206, i64 %checked_rhs207)
  %checked_result209 = extractvalue { i64, i1 } %with_overflow208, 0
  %checked_overflow210 = extractvalue { i64, i1 } %with_overflow208, 1
  %checked_overflow_widen211 = zext i1 %checked_overflow210 to i8
  store i64 %checked_result209, ptr %local_69, align 8
  store i8 %checked_overflow_widen211, ptr %local_70, align 1
  %cond_load212 = load i8, ptr %local_70, align 1
  %cond_nz213 = icmp ne i8 %cond_load212, 0
  br i1 %cond_nz213, label %bb52, label %bb53

bb51:                                             ; preds = %after_cooperate226, %bb49
  %move_load214 = load i64, ptr %local_57, align 8
  store i64 %move_load214, ptr %local_10, align 8
  %hew_actor_cooperate215 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel216 = icmp eq i32 %hew_actor_cooperate215, 2
  br i1 %hew_cooperate_is_cancel216, label %cancel_exit217, label %after_cooperate218

bb52:                                             ; preds = %bb50
  %"hew_vec_free drop220" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop220")
  store ptr null, ptr %local_8, align 8
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb53:                                             ; preds = %bb50
  %move_load221 = load i64, ptr %local_69, align 8
  store i64 %move_load221, ptr %local_71, align 8
  %move_load222 = load i64, ptr %local_71, align 8
  store i64 %move_load222, ptr %local_57, align 8
  %hew_actor_cooperate223 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel224 = icmp eq i32 %hew_actor_cooperate223, 2
  br i1 %hew_cooperate_is_cancel224, label %cancel_exit225, label %after_cooperate226

cancel_exit:                                      ; preds = %entry
  ret i64 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit6:                                     ; preds = %bb5
  ret i64 0

after_cooperate7:                                 ; preds = %bb5
  br label %bb4

cancel_exit66:                                    ; preds = %bb19
  %"hew_vec_free drop68" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop68")
  store ptr null, ptr %local_8, align 8
  ret i64 0

after_cooperate67:                                ; preds = %bb19
  br label %bb17

cancel_exit110:                                   ; preds = %bb28
  %"hew_vec_free drop112" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop112")
  store ptr null, ptr %local_8, align 8
  ret i64 0

after_cooperate111:                               ; preds = %bb28
  br label %bb26

cancel_exit134:                                   ; preds = %bb33
  %"hew_vec_free drop136" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop136")
  store ptr null, ptr %local_8, align 8
  ret i64 0

after_cooperate135:                               ; preds = %bb33
  br label %bb10

cancel_exit168:                                   ; preds = %bb39
  %"hew_vec_free drop170" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop170")
  store ptr null, ptr %local_8, align 8
  ret i64 0

after_cooperate169:                               ; preds = %bb39
  br label %bb34

cancel_exit178:                                   ; preds = %bb42
  %"hew_string_drop drop" = load ptr, ptr %local_55, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop")
  store ptr null, ptr %local_55, align 8
  %"hew_vec_free drop180" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop180")
  store ptr null, ptr %local_8, align 8
  ret i64 0

after_cooperate179:                               ; preds = %bb42
  %"hew_string_drop drop181" = load ptr, ptr %local_55, align 8
  call void @hew_string_drop(ptr %"hew_string_drop drop181")
  store ptr null, ptr %local_55, align 8
  br label %bb36

cancel_exit217:                                   ; preds = %bb51
  %"hew_vec_free drop219" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop219")
  store ptr null, ptr %local_8, align 8
  ret i64 0

after_cooperate218:                               ; preds = %bb51
  br label %bb7

cancel_exit225:                                   ; preds = %bb53
  %"hew_vec_free drop227" = load ptr, ptr %local_8, align 8
  call void @hew_vec_free(ptr %"hew_vec_free drop227")
  store ptr null, ptr %local_8, align 8
  ret i64 0

after_cooperate226:                               ; preds = %bb53
  br label %bb51
}

define internal i8 @"sort$reverse_ints_in_place"(ptr %0) {
entry:
  %return_slot = alloca i8, align 1
  %local_0 = alloca ptr, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i8, align 1
  %local_4 = alloca i64, align 8
  %local_5 = alloca i8, align 1
  %local_6 = alloca i64, align 8
  %local_7 = alloca i64, align 8
  %local_8 = alloca i64, align 8
  %local_9 = alloca i64, align 8
  %local_10 = alloca i8, align 1
  %local_11 = alloca i64, align 8
  %local_12 = alloca i8, align 1
  %local_13 = alloca i64, align 8
  %local_14 = alloca i64, align 8
  %local_15 = alloca i8, align 1
  %local_16 = alloca i64, align 8
  %local_17 = alloca i64, align 8
  %local_18 = alloca i8, align 1
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result = call i64 @hew_vec_len(ptr %call_arg)
  store i64 %call_result, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load i64, ptr %local_1, align 8
  store i64 %move_load, ptr %local_2, align 8
  store i64 2, ptr %local_4, align 8
  %cmp_lhs = load i64, ptr %local_2, align 8
  %cmp_rhs = load i64, ptr %local_4, align 8
  %cmp_bit = icmp slt i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_5, align 1
  %cond_load = load i8, ptr %local_5, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  ret i8 0

bb3:                                              ; preds = %bb1
  br label %bb4

bb4:                                              ; preds = %after_cooperate7, %bb3
  store i64 0, ptr %local_6, align 8
  %move_load1 = load i64, ptr %local_6, align 8
  store i64 %move_load1, ptr %local_7, align 8
  store i64 1, ptr %local_8, align 8
  %checked_lhs = load i64, ptr %local_2, align 8
  %checked_rhs = load i64, ptr %local_8, align 8
  %with_overflow = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_9, align 8
  store i8 %checked_overflow_widen, ptr %local_10, align 1
  %cond_load2 = load i8, ptr %local_10, align 1
  %cond_nz3 = icmp ne i8 %cond_load2, 0
  br i1 %cond_nz3, label %bb6, label %bb7

bb5:                                              ; No predecessors!
  %hew_actor_cooperate4 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel5 = icmp eq i32 %hew_actor_cooperate4, 2
  br i1 %hew_cooperate_is_cancel5, label %cancel_exit6, label %after_cooperate7

bb6:                                              ; preds = %bb4
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb7:                                              ; preds = %bb4
  %move_load8 = load i64, ptr %local_9, align 8
  store i64 %move_load8, ptr %local_11, align 8
  br label %bb8

bb8:                                              ; preds = %after_cooperate40, %bb7
  %cmp_lhs9 = load i64, ptr %local_7, align 8
  %cmp_rhs10 = load i64, ptr %local_11, align 8
  %cmp_bit11 = icmp slt i64 %cmp_lhs9, %cmp_rhs10
  %cmp_zext12 = zext i1 %cmp_bit11 to i8
  store i8 %cmp_zext12, ptr %local_12, align 1
  %cond_load13 = load i8, ptr %local_12, align 1
  %cond_nz14 = icmp ne i8 %cond_load13, 0
  br i1 %cond_nz14, label %bb9, label %bb10

bb9:                                              ; preds = %bb8
  %call_arg15 = load ptr, ptr %local_0, align 8
  %call_arg16 = load i64, ptr %local_7, align 8
  %call_arg17 = load i64, ptr %local_11, align 8
  %call_result18 = call i8 @"sort$swap_ints"(ptr %call_arg15, i64 %call_arg16, i64 %call_arg17)
  br label %bb11

bb10:                                             ; preds = %bb8
  ret i8 0

bb11:                                             ; preds = %bb9
  store i64 1, ptr %local_13, align 8
  %checked_lhs19 = load i64, ptr %local_7, align 8
  %checked_rhs20 = load i64, ptr %local_13, align 8
  %with_overflow21 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs19, i64 %checked_rhs20)
  %checked_result22 = extractvalue { i64, i1 } %with_overflow21, 0
  %checked_overflow23 = extractvalue { i64, i1 } %with_overflow21, 1
  %checked_overflow_widen24 = zext i1 %checked_overflow23 to i8
  store i64 %checked_result22, ptr %local_14, align 8
  store i8 %checked_overflow_widen24, ptr %local_15, align 1
  %cond_load25 = load i8, ptr %local_15, align 1
  %cond_nz26 = icmp ne i8 %cond_load25, 0
  br i1 %cond_nz26, label %bb12, label %bb13

bb12:                                             ; preds = %bb11
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb13:                                             ; preds = %bb11
  %move_load27 = load i64, ptr %local_14, align 8
  store i64 %move_load27, ptr %local_7, align 8
  store i64 1, ptr %local_16, align 8
  %checked_lhs28 = load i64, ptr %local_11, align 8
  %checked_rhs29 = load i64, ptr %local_16, align 8
  %with_overflow30 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %checked_lhs28, i64 %checked_rhs29)
  %checked_result31 = extractvalue { i64, i1 } %with_overflow30, 0
  %checked_overflow32 = extractvalue { i64, i1 } %with_overflow30, 1
  %checked_overflow_widen33 = zext i1 %checked_overflow32 to i8
  store i64 %checked_result31, ptr %local_17, align 8
  store i8 %checked_overflow_widen33, ptr %local_18, align 1
  %cond_load34 = load i8, ptr %local_18, align 1
  %cond_nz35 = icmp ne i8 %cond_load34, 0
  br i1 %cond_nz35, label %bb14, label %bb15

bb14:                                             ; preds = %bb13
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb15:                                             ; preds = %bb13
  %move_load36 = load i64, ptr %local_17, align 8
  store i64 %move_load36, ptr %local_11, align 8
  %hew_actor_cooperate37 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel38 = icmp eq i32 %hew_actor_cooperate37, 2
  br i1 %hew_cooperate_is_cancel38, label %cancel_exit39, label %after_cooperate40

cancel_exit:                                      ; preds = %entry
  ret i8 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit6:                                     ; preds = %bb5
  ret i8 0

after_cooperate7:                                 ; preds = %bb5
  br label %bb4

cancel_exit39:                                    ; preds = %bb15
  ret i8 0

after_cooperate40:                                ; preds = %bb15
  br label %bb8
}

define internal i8 @"sort$reverse_strings_in_place"(ptr %0) {
entry:
  %return_slot = alloca i8, align 1
  %local_0 = alloca ptr, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i8, align 1
  %local_4 = alloca i64, align 8
  %local_5 = alloca i8, align 1
  %local_6 = alloca i64, align 8
  %local_7 = alloca i64, align 8
  %local_8 = alloca i64, align 8
  %local_9 = alloca i64, align 8
  %local_10 = alloca i8, align 1
  %local_11 = alloca i64, align 8
  %local_12 = alloca i8, align 1
  %local_13 = alloca i64, align 8
  %local_14 = alloca i64, align 8
  %local_15 = alloca i8, align 1
  %local_16 = alloca i64, align 8
  %local_17 = alloca i64, align 8
  %local_18 = alloca i8, align 1
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result = call i64 @hew_vec_len(ptr %call_arg)
  store i64 %call_result, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load i64, ptr %local_1, align 8
  store i64 %move_load, ptr %local_2, align 8
  store i64 2, ptr %local_4, align 8
  %cmp_lhs = load i64, ptr %local_2, align 8
  %cmp_rhs = load i64, ptr %local_4, align 8
  %cmp_bit = icmp slt i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_5, align 1
  %cond_load = load i8, ptr %local_5, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  ret i8 0

bb3:                                              ; preds = %bb1
  br label %bb4

bb4:                                              ; preds = %after_cooperate7, %bb3
  store i64 0, ptr %local_6, align 8
  %move_load1 = load i64, ptr %local_6, align 8
  store i64 %move_load1, ptr %local_7, align 8
  store i64 1, ptr %local_8, align 8
  %checked_lhs = load i64, ptr %local_2, align 8
  %checked_rhs = load i64, ptr %local_8, align 8
  %with_overflow = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_9, align 8
  store i8 %checked_overflow_widen, ptr %local_10, align 1
  %cond_load2 = load i8, ptr %local_10, align 1
  %cond_nz3 = icmp ne i8 %cond_load2, 0
  br i1 %cond_nz3, label %bb6, label %bb7

bb5:                                              ; No predecessors!
  %hew_actor_cooperate4 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel5 = icmp eq i32 %hew_actor_cooperate4, 2
  br i1 %hew_cooperate_is_cancel5, label %cancel_exit6, label %after_cooperate7

bb6:                                              ; preds = %bb4
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb7:                                              ; preds = %bb4
  %move_load8 = load i64, ptr %local_9, align 8
  store i64 %move_load8, ptr %local_11, align 8
  br label %bb8

bb8:                                              ; preds = %after_cooperate40, %bb7
  %cmp_lhs9 = load i64, ptr %local_7, align 8
  %cmp_rhs10 = load i64, ptr %local_11, align 8
  %cmp_bit11 = icmp slt i64 %cmp_lhs9, %cmp_rhs10
  %cmp_zext12 = zext i1 %cmp_bit11 to i8
  store i8 %cmp_zext12, ptr %local_12, align 1
  %cond_load13 = load i8, ptr %local_12, align 1
  %cond_nz14 = icmp ne i8 %cond_load13, 0
  br i1 %cond_nz14, label %bb9, label %bb10

bb9:                                              ; preds = %bb8
  %call_arg15 = load ptr, ptr %local_0, align 8
  %call_arg16 = load i64, ptr %local_7, align 8
  %call_arg17 = load i64, ptr %local_11, align 8
  %call_result18 = call i8 @"sort$swap_strings"(ptr %call_arg15, i64 %call_arg16, i64 %call_arg17)
  br label %bb11

bb10:                                             ; preds = %bb8
  ret i8 0

bb11:                                             ; preds = %bb9
  store i64 1, ptr %local_13, align 8
  %checked_lhs19 = load i64, ptr %local_7, align 8
  %checked_rhs20 = load i64, ptr %local_13, align 8
  %with_overflow21 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs19, i64 %checked_rhs20)
  %checked_result22 = extractvalue { i64, i1 } %with_overflow21, 0
  %checked_overflow23 = extractvalue { i64, i1 } %with_overflow21, 1
  %checked_overflow_widen24 = zext i1 %checked_overflow23 to i8
  store i64 %checked_result22, ptr %local_14, align 8
  store i8 %checked_overflow_widen24, ptr %local_15, align 1
  %cond_load25 = load i8, ptr %local_15, align 1
  %cond_nz26 = icmp ne i8 %cond_load25, 0
  br i1 %cond_nz26, label %bb12, label %bb13

bb12:                                             ; preds = %bb11
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb13:                                             ; preds = %bb11
  %move_load27 = load i64, ptr %local_14, align 8
  store i64 %move_load27, ptr %local_7, align 8
  store i64 1, ptr %local_16, align 8
  %checked_lhs28 = load i64, ptr %local_11, align 8
  %checked_rhs29 = load i64, ptr %local_16, align 8
  %with_overflow30 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %checked_lhs28, i64 %checked_rhs29)
  %checked_result31 = extractvalue { i64, i1 } %with_overflow30, 0
  %checked_overflow32 = extractvalue { i64, i1 } %with_overflow30, 1
  %checked_overflow_widen33 = zext i1 %checked_overflow32 to i8
  store i64 %checked_result31, ptr %local_17, align 8
  store i8 %checked_overflow_widen33, ptr %local_18, align 1
  %cond_load34 = load i8, ptr %local_18, align 1
  %cond_nz35 = icmp ne i8 %cond_load34, 0
  br i1 %cond_nz35, label %bb14, label %bb15

bb14:                                             ; preds = %bb13
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb15:                                             ; preds = %bb13
  %move_load36 = load i64, ptr %local_17, align 8
  store i64 %move_load36, ptr %local_11, align 8
  %hew_actor_cooperate37 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel38 = icmp eq i32 %hew_actor_cooperate37, 2
  br i1 %hew_cooperate_is_cancel38, label %cancel_exit39, label %after_cooperate40

cancel_exit:                                      ; preds = %entry
  ret i8 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit6:                                     ; preds = %bb5
  ret i8 0

after_cooperate7:                                 ; preds = %bb5
  br label %bb4

cancel_exit39:                                    ; preds = %bb15
  ret i8 0

after_cooperate40:                                ; preds = %bb15
  br label %bb8
}

define internal i8 @"sort$reverse_floats_in_place"(ptr %0) {
entry:
  %return_slot = alloca i8, align 1
  %local_0 = alloca ptr, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i8, align 1
  %local_4 = alloca i64, align 8
  %local_5 = alloca i8, align 1
  %local_6 = alloca i64, align 8
  %local_7 = alloca i64, align 8
  %local_8 = alloca i64, align 8
  %local_9 = alloca i64, align 8
  %local_10 = alloca i8, align 1
  %local_11 = alloca i64, align 8
  %local_12 = alloca i8, align 1
  %local_13 = alloca i64, align 8
  %local_14 = alloca i64, align 8
  %local_15 = alloca i8, align 1
  %local_16 = alloca i64, align 8
  %local_17 = alloca i64, align 8
  %local_18 = alloca i8, align 1
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result = call i64 @hew_vec_len(ptr %call_arg)
  store i64 %call_result, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load i64, ptr %local_1, align 8
  store i64 %move_load, ptr %local_2, align 8
  store i64 2, ptr %local_4, align 8
  %cmp_lhs = load i64, ptr %local_2, align 8
  %cmp_rhs = load i64, ptr %local_4, align 8
  %cmp_bit = icmp slt i64 %cmp_lhs, %cmp_rhs
  %cmp_zext = zext i1 %cmp_bit to i8
  store i8 %cmp_zext, ptr %local_5, align 1
  %cond_load = load i8, ptr %local_5, align 1
  %cond_nz = icmp ne i8 %cond_load, 0
  br i1 %cond_nz, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  ret i8 0

bb3:                                              ; preds = %bb1
  br label %bb4

bb4:                                              ; preds = %after_cooperate7, %bb3
  store i64 0, ptr %local_6, align 8
  %move_load1 = load i64, ptr %local_6, align 8
  store i64 %move_load1, ptr %local_7, align 8
  store i64 1, ptr %local_8, align 8
  %checked_lhs = load i64, ptr %local_2, align 8
  %checked_rhs = load i64, ptr %local_8, align 8
  %with_overflow = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %checked_lhs, i64 %checked_rhs)
  %checked_result = extractvalue { i64, i1 } %with_overflow, 0
  %checked_overflow = extractvalue { i64, i1 } %with_overflow, 1
  %checked_overflow_widen = zext i1 %checked_overflow to i8
  store i64 %checked_result, ptr %local_9, align 8
  store i8 %checked_overflow_widen, ptr %local_10, align 1
  %cond_load2 = load i8, ptr %local_10, align 1
  %cond_nz3 = icmp ne i8 %cond_load2, 0
  br i1 %cond_nz3, label %bb6, label %bb7

bb5:                                              ; No predecessors!
  %hew_actor_cooperate4 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel5 = icmp eq i32 %hew_actor_cooperate4, 2
  br i1 %hew_cooperate_is_cancel5, label %cancel_exit6, label %after_cooperate7

bb6:                                              ; preds = %bb4
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb7:                                              ; preds = %bb4
  %move_load8 = load i64, ptr %local_9, align 8
  store i64 %move_load8, ptr %local_11, align 8
  br label %bb8

bb8:                                              ; preds = %after_cooperate40, %bb7
  %cmp_lhs9 = load i64, ptr %local_7, align 8
  %cmp_rhs10 = load i64, ptr %local_11, align 8
  %cmp_bit11 = icmp slt i64 %cmp_lhs9, %cmp_rhs10
  %cmp_zext12 = zext i1 %cmp_bit11 to i8
  store i8 %cmp_zext12, ptr %local_12, align 1
  %cond_load13 = load i8, ptr %local_12, align 1
  %cond_nz14 = icmp ne i8 %cond_load13, 0
  br i1 %cond_nz14, label %bb9, label %bb10

bb9:                                              ; preds = %bb8
  %call_arg15 = load ptr, ptr %local_0, align 8
  %call_arg16 = load i64, ptr %local_7, align 8
  %call_arg17 = load i64, ptr %local_11, align 8
  %call_result18 = call i8 @"sort$swap_floats"(ptr %call_arg15, i64 %call_arg16, i64 %call_arg17)
  br label %bb11

bb10:                                             ; preds = %bb8
  ret i8 0

bb11:                                             ; preds = %bb9
  store i64 1, ptr %local_13, align 8
  %checked_lhs19 = load i64, ptr %local_7, align 8
  %checked_rhs20 = load i64, ptr %local_13, align 8
  %with_overflow21 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %checked_lhs19, i64 %checked_rhs20)
  %checked_result22 = extractvalue { i64, i1 } %with_overflow21, 0
  %checked_overflow23 = extractvalue { i64, i1 } %with_overflow21, 1
  %checked_overflow_widen24 = zext i1 %checked_overflow23 to i8
  store i64 %checked_result22, ptr %local_14, align 8
  store i8 %checked_overflow_widen24, ptr %local_15, align 1
  %cond_load25 = load i8, ptr %local_15, align 1
  %cond_nz26 = icmp ne i8 %cond_load25, 0
  br i1 %cond_nz26, label %bb12, label %bb13

bb12:                                             ; preds = %bb11
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb13:                                             ; preds = %bb11
  %move_load27 = load i64, ptr %local_14, align 8
  store i64 %move_load27, ptr %local_7, align 8
  store i64 1, ptr %local_16, align 8
  %checked_lhs28 = load i64, ptr %local_11, align 8
  %checked_rhs29 = load i64, ptr %local_16, align 8
  %with_overflow30 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %checked_lhs28, i64 %checked_rhs29)
  %checked_result31 = extractvalue { i64, i1 } %with_overflow30, 0
  %checked_overflow32 = extractvalue { i64, i1 } %with_overflow30, 1
  %checked_overflow_widen33 = zext i1 %checked_overflow32 to i8
  store i64 %checked_result31, ptr %local_17, align 8
  store i8 %checked_overflow_widen33, ptr %local_18, align 1
  %cond_load34 = load i8, ptr %local_18, align 1
  %cond_nz35 = icmp ne i8 %cond_load34, 0
  br i1 %cond_nz35, label %bb14, label %bb15

bb14:                                             ; preds = %bb13
  call void @hew_trap_with_code(i32 201)
  call void @llvm.trap()
  unreachable

bb15:                                             ; preds = %bb13
  %move_load36 = load i64, ptr %local_17, align 8
  store i64 %move_load36, ptr %local_11, align 8
  %hew_actor_cooperate37 = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel38 = icmp eq i32 %hew_actor_cooperate37, 2
  br i1 %hew_cooperate_is_cancel38, label %cancel_exit39, label %after_cooperate40

cancel_exit:                                      ; preds = %entry
  ret i8 0

after_cooperate:                                  ; preds = %entry
  br label %bb0

cancel_exit6:                                     ; preds = %bb5
  ret i8 0

after_cooperate7:                                 ; preds = %bb5
  br label %bb4

cancel_exit39:                                    ; preds = %bb15
  ret i8 0

after_cooperate40:                                ; preds = %bb15
  br label %bb8
}

define internal ptr @"sort$sort_ints"(ptr %0) {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca ptr, align 8
  %local_3 = alloca i64, align 8
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result = call ptr @"sort$copy_ints"(ptr %call_arg)
  store ptr %call_result, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %local_2, align 8
  %call_arg1 = load ptr, ptr %local_2, align 8
  %call_result2 = call i64 @"sort$sort_ints_in_place"(ptr %call_arg1)
  store i64 %call_result2, ptr %local_3, align 8
  br label %bb2

bb2:                                              ; preds = %bb1
  %move_load3 = load ptr, ptr %local_2, align 8
  store ptr %move_load3, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal { ptr, i64 } @"sort$sort_ints_counted"(ptr %0) {
entry:
  %return_slot = alloca { ptr, i64 }, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca ptr, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i64, align 8
  %local_5 = alloca { ptr, i64 }, align 8
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result = call ptr @"sort$copy_ints"(ptr %call_arg)
  store ptr %call_result, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %local_2, align 8
  %call_arg1 = load ptr, ptr %local_2, align 8
  %call_result2 = call i64 @"sort$sort_ints_in_place"(ptr %call_arg1)
  store i64 %call_result2, ptr %local_3, align 8
  br label %bb2

bb2:                                              ; preds = %bb1
  %move_load3 = load i64, ptr %local_3, align 8
  store i64 %move_load3, ptr %local_4, align 8
  %tuple_elem_0_load = load ptr, ptr %local_2, align 8
  %tuple_elem_0_gep = getelementptr inbounds nuw { ptr, i64 }, ptr %local_5, i32 0, i32 0
  store ptr %tuple_elem_0_load, ptr %tuple_elem_0_gep, align 8
  %tuple_elem_1_load = load i64, ptr %local_4, align 8
  %tuple_elem_1_gep = getelementptr inbounds nuw { ptr, i64 }, ptr %local_5, i32 0, i32 1
  store i64 %tuple_elem_1_load, ptr %tuple_elem_1_gep, align 8
  %move_load4 = load { ptr, i64 }, ptr %local_5, align 8
  store { ptr, i64 } %move_load4, ptr %return_slot, align 8
  %ret_val = load { ptr, i64 }, ptr %return_slot, align 8
  ret { ptr, i64 } %ret_val

cancel_exit:                                      ; preds = %entry
  ret { ptr, i64 } zeroinitializer

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"sort$sort_strings"(ptr %0) {
entry:
  %return_slot = alloca ptr, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca ptr, align 8
  %local_3 = alloca i64, align 8
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result = call ptr @"sort$copy_strings"(ptr %call_arg)
  store ptr %call_result, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %local_2, align 8
  %call_arg1 = load ptr, ptr %local_2, align 8
  %call_result2 = call i64 @"sort$sort_strings_in_place"(ptr %call_arg1)
  store i64 %call_result2, ptr %local_3, align 8
  br label %bb2

bb2:                                              ; preds = %bb1
  %move_load3 = load ptr, ptr %local_2, align 8
  store ptr %move_load3, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal { ptr, i64 } @"sort$sort_strings_counted"(ptr %0) {
entry:
  %return_slot = alloca { ptr, i64 }, align 8
  %local_0 = alloca ptr, align 8
  %local_1 = alloca ptr, align 8
  %local_2 = alloca ptr, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i64, align 8
  %local_5 = alloca { ptr, i64 }, align 8
  store ptr %0, ptr %local_0, align 8
  %hew_actor_cooperate = call i32 @hew_actor_cooperate()
  %hew_cooperate_is_cancel = icmp eq i32 %hew_actor_cooperate, 2
  br i1 %hew_cooperate_is_cancel, label %cancel_exit, label %after_cooperate

bb0:                                              ; preds = %after_cooperate
  %call_arg = load ptr, ptr %local_0, align 8
  %call_result = call ptr @"sort$copy_strings"(ptr %call_arg)
  store ptr %call_result, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %local_2, align 8
  %call_arg1 = load ptr, ptr %local_2, align 8
  %call_result2 = call i64 @"sort$sort_strings_in_place"(ptr %call_arg1)
  store i64 %call_result2, ptr %local_3, align 8
  br label %bb2

bb2:                                              ; preds = %bb1
  %move_load3 = load i64, ptr %local_3, align 8
  store i64 %move_load3, ptr %local_4, align 8
  %tuple_elem_0_load = load ptr, ptr %local_2, align 8
  %tuple_elem_0_gep = getelementptr inbounds nuw { ptr, i64 }, ptr %local_5, i32 0, i32 0
  store ptr %tuple_elem_0_load, ptr %tuple_elem_0_gep, align 8
  %tuple_elem_1_load = load i64, ptr %local_4, align 8
  %tuple_elem_1_gep = getelementptr inbounds nuw { ptr, i64 }, ptr %local_5, i32 0, i32 1
  store i64 %tuple_elem_1_load, ptr %tuple_elem_1_gep, align 8
  %move_load4 = load { ptr, i64 }, ptr %local_5, align 8
  store { ptr, i64 } %move_load4, ptr %return_slot, align 8
  %ret_val = load { ptr, i64 }, ptr %return_slot, align 8
  ret { ptr, i64 } %ret_val

cancel_exit:                                      ; preds = %entry
  ret { ptr, i64 } zeroinitializer

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"sort$sort_floats"(ptr %0) {
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
  %call_result = call ptr @hew_sort_floats(ptr %call_arg)
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

define internal ptr @"sort$reverse_ints"(ptr %0) {
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
  %call_result = call ptr @"sort$copy_ints"(ptr %call_arg)
  store ptr %call_result, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %local_2, align 8
  %call_arg1 = load ptr, ptr %local_2, align 8
  %call_result2 = call i8 @"sort$reverse_ints_in_place"(ptr %call_arg1)
  br label %bb2

bb2:                                              ; preds = %bb1
  %move_load3 = load ptr, ptr %local_2, align 8
  store ptr %move_load3, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"sort$reverse_strings"(ptr %0) {
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
  %call_result = call ptr @"sort$copy_strings"(ptr %call_arg)
  store ptr %call_result, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %local_2, align 8
  %call_arg1 = load ptr, ptr %local_2, align 8
  %call_result2 = call i8 @"sort$reverse_strings_in_place"(ptr %call_arg1)
  br label %bb2

bb2:                                              ; preds = %bb1
  %move_load3 = load ptr, ptr %local_2, align 8
  store ptr %move_load3, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
}

define internal ptr @"sort$reverse_floats"(ptr %0) {
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
  %call_result = call ptr @"sort$copy_floats"(ptr %call_arg)
  store ptr %call_result, ptr %local_1, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %move_load = load ptr, ptr %local_1, align 8
  store ptr %move_load, ptr %local_2, align 8
  %call_arg1 = load ptr, ptr %local_2, align 8
  %call_result2 = call i8 @"sort$reverse_floats_in_place"(ptr %call_arg1)
  br label %bb2

bb2:                                              ; preds = %bb1
  %move_load3 = load ptr, ptr %local_2, align 8
  store ptr %move_load3, ptr %return_slot, align 8
  %ret_val = load ptr, ptr %return_slot, align 8
  ret ptr %ret_val

cancel_exit:                                      ; preds = %entry
  ret ptr null

after_cooperate:                                  ; preds = %entry
  br label %bb0
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
  store ptr @str_lit.28, ptr %local_3, align 8
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

declare i32 @hew_actor_cooperate()

declare ptr @hew_vec_new_i64()

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.ssub.with.overflow.i64(i64, i64) #0

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64) #0

declare void @hew_vec_free(ptr)

declare void @hew_trap_with_code(i32)

; Function Attrs: cold noreturn nounwind memory(inaccessiblemem: write)
declare void @llvm.trap() #1

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.smul.with.overflow.i64(i64, i64) #0

declare ptr @hew_vec_new_str()

declare i32 @hew_string_compare(ptr, ptr)

declare i32 @hew_string_equals(ptr, ptr)

declare i32 @hew_lambda_drain_all(i64)

declare ptr @hew_vec_new_f64()

attributes #0 = { nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none) }
attributes #1 = { cold noreturn nounwind memory(inaccessiblemem: write) }
