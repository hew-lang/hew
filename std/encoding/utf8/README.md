# UTF-8

`std.encoding.utf8` defines `Utf8Error`, an ordinary value describing invalid
UTF-8 input. `valid_up_to` and `error_len` are byte counts, not string indices.
An absent `error_len` identifies an incomplete trailing sequence.
