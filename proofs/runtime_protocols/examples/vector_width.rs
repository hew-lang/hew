//! Machine witness for the same production vector module checked by Kani.
use hew_runtime_protocol_verification::vec::*;
use std::ffi::c_void;
unsafe fn vector() -> *mut HewVec {
    let v = hew_vec_new();
    for value in [11, 22, 33] {
        hew_vec_push_i32(v, value);
    }
    v
}
fn main() {
    let indices = [
        -1,
        -4_294_967_296,
        -4_294_967_295,
        3,
        4_294_967_296,
        4_294_967_297,
        i64::MAX,
    ];
    let mut bad = Vec::new();
    let mut checks = 0;
    unsafe {
        for index in indices {
            for op in 0..4 {
                let v = vector();
                let mut out = -99i32;
                let changed = match op {
                    0 => hew_vec_get_clone(v, index, (&raw mut out).cast::<c_void>()),
                    1 => hew_vec_borrow_owned(v, index, (&raw mut out).cast::<c_void>()),
                    2 => hew_vec_take_owned(v, index, (&raw mut out).cast::<c_void>()),
                    _ => {
                        hew_vec_truncate(v, index);
                        false
                    }
                };
                checks += 1;
                if changed || out != -99 || (*v).len != 3 {
                    bad.push(format!(
                        "\"op={op},index={index},found={changed},out={out},len={}\"",
                        (*v).len
                    ));
                }
                if (*v).len == 3 {
                    for (i, value) in [11, 22, 33].into_iter().enumerate() {
                        checks += 1;
                        if hew_vec_get_i32(v, i as i64) != value {
                            bad.push(format!("\"op={op},index={index},source_changed={i}\""));
                        }
                    }
                }
                hew_vec_free(v);
            }
        }
        let v = vector();
        let mut out = 0i32;
        checks += 1;
        if !hew_vec_get_clone(v, 1, (&raw mut out).cast()) || out != 22 {
            bad.push("\"valid_get_failed\"".into());
        }
        hew_vec_free(v);
    }
    println!(
        "{{\"pointer_bits\":{},\"checks\":{checks},\"violations\":[{}]}}",
        usize::BITS,
        bad.join(",")
    );
}
