// Real format-specific runtime bindings for the shared execution cases.
use super::*;

// Link the public C exports even though their Rust implementation modules are private.
macro_rules! checked_imports {
    ($prefix:literal) => {
        unsafe extern "C" {
            #[link_name = concat!($prefix, "_clone_checked")]
            fn checked_clone(
                value: *const c_void,
                layout: *const hew_cabi::value::HewValueLayout,
                status: *mut i32,
            ) -> *mut c_void;
            #[link_name = concat!($prefix, "_parse_checked")]
            fn checked_parse(
                text: *const c_void,
                layout: *const hew_cabi::value::HewValueLayout,
                status: *mut i32,
            ) -> *mut c_void;
            #[link_name = concat!($prefix, "_last_error_checked")]
            fn checked_last_error(
                layout: *const hew_cabi::value::HewValueLayout,
                status: *mut i32,
            ) -> *mut c_void;
            #[link_name = concat!($prefix, "_stringify_checked")]
            fn checked_stringify(
                value: *const c_void,
                layout: *const hew_cabi::value::HewValueLayout,
                status: *mut i32,
            ) -> *mut c_void;
            #[link_name = concat!($prefix, "_get_string_checked")]
            fn checked_get_string(
                value: *const c_void,
                layout: *const hew_cabi::value::HewValueLayout,
                status: *mut i32,
            ) -> *mut c_void;
            #[link_name = concat!($prefix, "_get_field_checked")]
            fn checked_get_field(
                value: *const c_void,
                key: *const c_void,
                layout: *const hew_cabi::value::HewValueLayout,
                status: *mut i32,
            ) -> *mut c_void;
            #[link_name = concat!($prefix, "_array_get_checked")]
            fn checked_array_get(
                value: *const c_void,
                index: i32,
                layout: *const hew_cabi::value::HewValueLayout,
                status: *mut i32,
            ) -> *mut c_void;
            #[link_name = concat!($prefix, "_object_keys_checked")]
            fn checked_object_keys(
                value: *const c_void,
                layout: *const hew_cabi::value::HewValueLayout,
                status: *mut i32,
            ) -> *mut c_void;
            #[link_name = concat!($prefix, "_object_new_checked")]
            fn checked_object_new(
                layout: *const hew_cabi::value::HewValueLayout,
                status: *mut i32,
            ) -> *mut c_void;
            #[link_name = concat!($prefix, "_array_new_checked")]
            fn checked_array_new(
                layout: *const hew_cabi::value::HewValueLayout,
                status: *mut i32,
            ) -> *mut c_void;
            #[link_name = concat!($prefix, "_from_bool_checked")]
            fn checked_from_bool(
                value: i32,
                layout: *const hew_cabi::value::HewValueLayout,
                status: *mut i32,
            ) -> *mut c_void;
            #[link_name = concat!($prefix, "_from_int_checked")]
            fn checked_from_int(
                value: i64,
                layout: *const hew_cabi::value::HewValueLayout,
                status: *mut i32,
            ) -> *mut c_void;
            #[link_name = concat!($prefix, "_from_u64_checked")]
            fn checked_from_u64(
                value: u64,
                layout: *const hew_cabi::value::HewValueLayout,
                status: *mut i32,
            ) -> *mut c_void;
            #[link_name = concat!($prefix, "_from_float_checked")]
            fn checked_from_float(
                value: f64,
                layout: *const hew_cabi::value::HewValueLayout,
                status: *mut i32,
            ) -> *mut c_void;
            #[link_name = concat!($prefix, "_from_string_checked")]
            fn checked_from_string(
                value: *const c_void,
                layout: *const hew_cabi::value::HewValueLayout,
                status: *mut i32,
            ) -> *mut c_void;
            #[link_name = concat!($prefix, "_from_null_checked")]
            fn checked_from_null(
                layout: *const hew_cabi::value::HewValueLayout,
                status: *mut i32,
            ) -> *mut c_void;
            #[link_name = concat!($prefix, "_object_set_checked")]
            fn checked_object_set(
                value: *mut c_void,
                key: *const c_void,
                child: *mut c_void,
                status: *mut i32,
            );
            #[link_name = concat!($prefix, "_array_push_checked")]
            fn checked_array_push(value: *mut c_void, child: *mut c_void, status: *mut i32);
            #[link_name = concat!($prefix, "_free_managed")]
            fn managed_free(value: *mut c_void);
        }
    };
}

mod json {
    use super::*;
    const FORMAT: EncodingFormat = EncodingFormat::Json;
    use hew_std::json::{
        hew_json_array_get as raw_array_get, hew_json_array_len as raw_array_len,
        hew_json_clone as raw_clone, hew_json_eq as raw_eq, hew_json_free as raw_free,
        hew_json_from_u64 as raw_from_u64, hew_json_get_bool as raw_get_bool,
        hew_json_get_float as raw_get_float, hew_json_get_int as raw_get_int,
        hew_json_get_u64 as raw_get_u64, hew_json_parse as raw_parse, hew_json_type as raw_type,
        HewJsonValue as Value,
    };
    checked_imports!("hew_json");
    include!("physical_encoding_runtime_cases.rs");
}

mod yaml {
    use super::*;
    const FORMAT: EncodingFormat = EncodingFormat::Yaml;
    use hew_std::yaml::{
        hew_yaml_array_get as raw_array_get, hew_yaml_array_len as raw_array_len,
        hew_yaml_clone as raw_clone, hew_yaml_eq as raw_eq, hew_yaml_free as raw_free,
        hew_yaml_from_u64 as raw_from_u64, hew_yaml_get_bool as raw_get_bool,
        hew_yaml_get_float as raw_get_float, hew_yaml_get_int as raw_get_int,
        hew_yaml_get_u64 as raw_get_u64, hew_yaml_parse as raw_parse, hew_yaml_type as raw_type,
        HewYamlValue as Value,
    };
    checked_imports!("hew_yaml");
    include!("physical_encoding_runtime_cases.rs");
}
