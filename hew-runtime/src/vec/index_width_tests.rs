use super::*;

#[test]
fn invalid_abi_indices_preserve_elements_and_outputs() {
    let indices = [
        -1,
        i64::MIN,
        -4_294_967_296,
        -4_294_967_295,
        3,
        4_294_967_296,
        4_294_967_297,
        i64::MAX,
    ];
    // SAFETY: headers come from the runtime, slots hold i32 values, and each
    // vector is freed exactly once after its contents have been inspected.
    unsafe {
        for index in indices {
            for operation in 0..5 {
                let vector = hew_vec_new();
                for value in [11, 22, 33] {
                    hew_vec_push_i32(vector, value);
                }
                let mut output = -99i32;
                let mut status = -99;
                let found = match operation {
                    0 => hew_vec_get_clone(vector, index, (&raw mut output).cast()),
                    1 => hew_vec_borrow_owned(vector, index, (&raw mut output).cast()),
                    2 => hew_vec_take_owned(vector, index, (&raw mut output).cast()),
                    3 => {
                        hew_vec_truncate(vector, index);
                        false
                    }
                    _ => hew_vec_get_clone_managed(
                        vector,
                        index,
                        (&raw mut output).cast(),
                        core::ptr::null(),
                        &raw mut status,
                    ),
                };
                assert!(!found, "operation {operation} accepted index {index}");
                assert_eq!(
                    output, -99,
                    "operation {operation} overwrote output at {index}"
                );
                assert_eq!(
                    hew_vec_len(vector),
                    3,
                    "operation {operation} changed length at {index}"
                );
                assert_eq!(status, if operation == 4 { 0 } else { -99 });
                for (element, expected) in [11, 22, 33].into_iter().enumerate() {
                    assert_eq!(
                        hew_vec_get_i32(vector, i64::try_from(element).unwrap()),
                        expected,
                        "operation {operation} changed element {element} at index {index}"
                    );
                }
                hew_vec_free(vector);
            }
        }
        let vector = hew_vec_new();
        hew_vec_push_i32(vector, 22);
        let mut output = -99i32;
        assert!(hew_vec_get_clone(vector, 0, (&raw mut output).cast()));
        assert_eq!(output, 22);
        hew_vec_free(vector);
    }
}
