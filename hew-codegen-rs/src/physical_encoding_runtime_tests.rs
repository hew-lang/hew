// Real format-specific runtime bindings for the shared execution cases.
use super::*;

mod json {
    use super::*;
    const FORMAT: EncodingFormat = EncodingFormat::Json;
    use hew_std::json::{
        hew_json_array_get as raw_array_get, hew_json_array_len as raw_array_len,
        hew_json_array_new as raw_array_new, hew_json_array_push as raw_array_push,
        hew_json_clone as raw_clone, hew_json_eq as raw_eq, hew_json_free as raw_free,
        hew_json_from_bool as raw_from_bool, hew_json_from_float as raw_from_float,
        hew_json_from_int as raw_from_int, hew_json_from_null as raw_from_null,
        hew_json_from_string as raw_from_string, hew_json_from_u64 as raw_from_u64,
        hew_json_get_bool as raw_get_bool, hew_json_get_field as raw_get_field,
        hew_json_get_float as raw_get_float, hew_json_get_int as raw_get_int,
        hew_json_get_string as raw_get_string, hew_json_get_u64 as raw_get_u64,
        hew_json_last_error as raw_last_error, hew_json_object_new as raw_object_new,
        hew_json_object_set as raw_object_set, hew_json_parse as raw_parse,
        hew_json_stringify as raw_stringify, hew_json_type as raw_type, HewJsonValue as Value,
    };
    fn bind_runtime(llvm: &Module<'_>, engine: &ExecutionEngine<'_>) {
        if let Some(function) = llvm.get_function("hew_json_clone") {
            engine.add_global_mapping(&function, trace_clone as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_json_free") {
            engine.add_global_mapping(&function, trace_free as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_json_parse") {
            engine.add_global_mapping(&function, raw_parse as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_json_eq") {
            engine.add_global_mapping(&function, raw_eq as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_json_object_set") {
            engine.add_global_mapping(&function, trace_object_set as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_json_array_push") {
            engine.add_global_mapping(&function, trace_array_push as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_json_get_field") {
            engine.add_global_mapping(&function, raw_get_field as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_json_array_get") {
            engine.add_global_mapping(&function, raw_array_get as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_json_get_u64") {
            engine.add_global_mapping(&function, raw_get_u64 as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_json_get_int") {
            engine.add_global_mapping(&function, raw_get_int as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_json_get_float") {
            engine.add_global_mapping(&function, raw_get_float as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_json_get_bool") {
            engine.add_global_mapping(&function, raw_get_bool as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_json_type") {
            engine.add_global_mapping(&function, raw_type as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_json_array_len") {
            engine.add_global_mapping(&function, raw_array_len as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_json_from_u64") {
            engine.add_global_mapping(&function, raw_from_u64 as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_json_from_int") {
            engine.add_global_mapping(&function, raw_from_int as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_json_from_float") {
            engine.add_global_mapping(&function, raw_from_float as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_json_from_bool") {
            engine.add_global_mapping(&function, raw_from_bool as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_json_from_null") {
            engine.add_global_mapping(&function, raw_from_null as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_json_from_string") {
            engine.add_global_mapping(&function, raw_from_string as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_json_get_string") {
            engine.add_global_mapping(&function, raw_get_string as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_json_stringify") {
            engine.add_global_mapping(&function, raw_stringify as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_json_last_error") {
            engine.add_global_mapping(&function, raw_last_error as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_json_object_new") {
            engine.add_global_mapping(&function, raw_object_new as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_json_array_new") {
            engine.add_global_mapping(&function, raw_array_new as *const () as usize);
        }
    }
    include!("physical_encoding_runtime_cases.rs");
}

mod yaml {
    use super::*;
    const FORMAT: EncodingFormat = EncodingFormat::Yaml;
    use hew_std::yaml::{
        hew_yaml_array_get as raw_array_get, hew_yaml_array_len as raw_array_len,
        hew_yaml_array_new as raw_array_new, hew_yaml_array_push as raw_array_push,
        hew_yaml_clone as raw_clone, hew_yaml_eq as raw_eq, hew_yaml_free as raw_free,
        hew_yaml_from_bool as raw_from_bool, hew_yaml_from_float as raw_from_float,
        hew_yaml_from_int as raw_from_int, hew_yaml_from_null as raw_from_null,
        hew_yaml_from_string as raw_from_string, hew_yaml_from_u64 as raw_from_u64,
        hew_yaml_get_bool as raw_get_bool, hew_yaml_get_field as raw_get_field,
        hew_yaml_get_float as raw_get_float, hew_yaml_get_int as raw_get_int,
        hew_yaml_get_string as raw_get_string, hew_yaml_get_u64 as raw_get_u64,
        hew_yaml_last_error as raw_last_error, hew_yaml_object_new as raw_object_new,
        hew_yaml_object_set as raw_object_set, hew_yaml_parse as raw_parse,
        hew_yaml_stringify as raw_stringify, hew_yaml_type as raw_type, HewYamlValue as Value,
    };
    fn bind_runtime(llvm: &Module<'_>, engine: &ExecutionEngine<'_>) {
        if let Some(function) = llvm.get_function("hew_yaml_clone") {
            engine.add_global_mapping(&function, trace_clone as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_yaml_free") {
            engine.add_global_mapping(&function, trace_free as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_yaml_parse") {
            engine.add_global_mapping(&function, raw_parse as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_yaml_eq") {
            engine.add_global_mapping(&function, raw_eq as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_yaml_object_set") {
            engine.add_global_mapping(&function, trace_object_set as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_yaml_array_push") {
            engine.add_global_mapping(&function, trace_array_push as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_yaml_get_field") {
            engine.add_global_mapping(&function, raw_get_field as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_yaml_array_get") {
            engine.add_global_mapping(&function, raw_array_get as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_yaml_get_u64") {
            engine.add_global_mapping(&function, raw_get_u64 as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_yaml_get_int") {
            engine.add_global_mapping(&function, raw_get_int as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_yaml_get_float") {
            engine.add_global_mapping(&function, raw_get_float as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_yaml_get_bool") {
            engine.add_global_mapping(&function, raw_get_bool as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_yaml_type") {
            engine.add_global_mapping(&function, raw_type as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_yaml_array_len") {
            engine.add_global_mapping(&function, raw_array_len as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_yaml_from_u64") {
            engine.add_global_mapping(&function, raw_from_u64 as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_yaml_from_int") {
            engine.add_global_mapping(&function, raw_from_int as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_yaml_from_float") {
            engine.add_global_mapping(&function, raw_from_float as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_yaml_from_bool") {
            engine.add_global_mapping(&function, raw_from_bool as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_yaml_from_null") {
            engine.add_global_mapping(&function, raw_from_null as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_yaml_from_string") {
            engine.add_global_mapping(&function, raw_from_string as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_yaml_get_string") {
            engine.add_global_mapping(&function, raw_get_string as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_yaml_stringify") {
            engine.add_global_mapping(&function, raw_stringify as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_yaml_last_error") {
            engine.add_global_mapping(&function, raw_last_error as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_yaml_object_new") {
            engine.add_global_mapping(&function, raw_object_new as *const () as usize);
        }
        if let Some(function) = llvm.get_function("hew_yaml_array_new") {
            engine.add_global_mapping(&function, raw_array_new as *const () as usize);
        }
    }
    include!("physical_encoding_runtime_cases.rs");
}
