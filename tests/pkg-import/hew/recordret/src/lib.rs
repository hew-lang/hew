//! Native FFI backing for the `hew::recordret` §0 falsification package.
//!
//! Every symbol returns a `#[repr(C)]` record by value with DISTINCT non-zero
//! sentinel values in every field, so a mis-placed return register, a split
//! eightbyte, or an unset indirect-result pointer (the #2399 sret bug) is
//! observable field-by-field rather than hidden behind a zero that garbage
//! could accidentally match.
//!
//! The corpus spans the aggregate-return classes the ABI classifier decides:
//!   - `Big`      24 B  → Indirect (sret) on every target — the headline class.
//!   - `BigMixed` 24 B  → Indirect, taking `(i32, i64)` params to pin the
//!                        sret param-index shift in the caller's width loop.
//!   - `Packed`   16 B  → RegisterPair with a packed second eightbyte.
//!   - `Mid`      16 B  → RegisterPair, one field per eightbyte.
//!   - `Small`     8 B  → Direct multi-field (C packs into one register).
//!   - `One`       8 B  → Direct single-field — the control that must stay green.
//!
//! Zero dependencies so the staticlib builds offline, exactly like an
//! out-of-tree ecosystem package.

/// 24-byte, three-eightbyte aggregate → Indirect (sret) on SysV/AAPCS/MSVC.
#[repr(C)]
pub struct Big {
    a: i64,
    b: i64,
    c: i64,
}

/// Same 24-byte Indirect shape, but produced from `(i32, i64)` arguments so a
/// fixture can prove both params reach the callee at the correct width and
/// position AFTER the hidden sret pointer shifts the argument list by one.
#[repr(C)]
pub struct BigMixed {
    a: i64,
    b: i64,
    c: i64,
}

/// 16-byte aggregate whose second eightbyte packs two i32s → RegisterPair
/// (SysV/AAPCS), Indirect on MSVC. The `{i64,i32,i32}` shape whose bare LLVM
/// struct return the bytes migration proved can split into three registers.
#[repr(C)]
pub struct Packed {
    p: i64,
    x: i32,
    y: i32,
}

/// 16-byte aggregate, one field per eightbyte → RegisterPair. The
/// `CronNextResult` shape.
#[repr(C)]
pub struct Mid {
    s: i32,
    t: i64,
}

/// 8-byte, single-eightbyte, multi-field aggregate → Direct. The C ABI packs
/// both i32s into one register; a bare LLVM `{i32,i32}` return may split them.
/// The `TlsWriteFfiResult` shape.
#[repr(C)]
pub struct Small {
    a: i32,
    b: i32,
}

/// 8-byte single-field aggregate → Direct. The ecosystem-handle shape
/// (`type X { h: i64 }`). The control: correct today and must stay correct.
#[repr(C)]
pub struct One {
    h: i64,
}

/// Big: three distinct non-zero i64 sentinels.
#[no_mangle]
pub extern "C" fn hew_recordret_big() -> Big {
    Big {
        a: 1_111_111_111,
        b: 2_222_222_222,
        c: 3_333_333_333,
    }
}

/// BigMixed: `a = hi`, `b = lo`, `c = hi + lo`. `hi` exceeds 2^32 so a
/// truncated-to-i32 width reconciliation (the sret param-shift bug) corrupts
/// `a`/`c` observably; swapping `lo`/`hi` (a param-index bug) corrupts `a`/`b`.
#[no_mangle]
pub extern "C" fn hew_recordret_big_mixed(lo: i32, hi: i64) -> BigMixed {
    let lo64 = i64::from(lo);
    BigMixed {
        a: hi,
        b: lo64,
        c: hi + lo64,
    }
}

/// Packed: distinct non-zero fields across the packed second eightbyte.
#[no_mangle]
pub extern "C" fn hew_recordret_packed() -> Packed {
    Packed {
        p: 9_999_999_999,
        x: 444,
        y: 555,
    }
}

/// Mid: distinct non-zero fields, one per eightbyte.
#[no_mangle]
pub extern "C" fn hew_recordret_mid() -> Mid {
    Mid {
        s: 666,
        t: 7_777_777_777,
    }
}

/// Small: distinct non-zero i32s the C ABI packs into one register.
#[no_mangle]
pub extern "C" fn hew_recordret_small() -> Small {
    Small { a: 1234, b: 5678 }
}

/// One: the single-field control.
#[no_mangle]
pub extern "C" fn hew_recordret_one() -> One {
    One { h: 8_888_888_888 }
}
