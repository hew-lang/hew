//! Per-type schema tables: the only runtime copy of a record's or enum's
//! member keys, tags and presence rules.
//!
//! Both types are `#[repr(C)]` so the compiler can emit them as read-only
//! globals the runtime reads in place: a string is a pointer and a byte
//! length, a slice a pointer and an element count, both pointer-sized.

use core::marker::PhantomData;

/// One record field or enum variant.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct Member<'a> {
    key_ptr: *const u8,
    key_len: usize,
    /// The stable `@N` tag, used by binary formats when the table is tagged.
    pub tag: u64,
    /// `Member::*` flag bits.
    pub flags: u32,
    _key: PhantomData<&'a str>,
}

impl<'a> Member<'a> {
    /// Decode: an absent key reads as `null` (a plain `Option` field, or a
    /// `#[wire]` `optional` field).
    pub const ACCEPT_ABSENT: u32 = 1;
    /// Encode: a `null` value omits the key (a `#[wire]` `optional` field).
    pub const OMIT_NULL: u32 = 2;
    /// Never encoded; on decode the key is ignored and the field reads as
    /// `null` (`#[serial(skip)]`).
    pub const SKIP: u32 = 4;
    /// A variant with a payload; without it the variant is a unit variant.
    pub const PAYLOAD: u32 = 8;

    /// `key` is the text key every text format uses, and binary formats use
    /// when the table is not tagged.
    #[must_use]
    pub const fn new(key: &'a str, tag: u64, flags: u32) -> Self {
        Self {
            key_ptr: key.as_ptr(),
            key_len: key.len(),
            tag,
            flags,
            _key: PhantomData,
        }
    }

    #[must_use]
    pub fn key(&self) -> &'a str {
        // SAFETY: a `Member` is built by `new` from a `&'a str`, or emitted by
        // the compiler with the same layout over UTF-8 bytes that live as long
        // as the program.
        unsafe {
            core::str::from_utf8_unchecked(core::slice::from_raw_parts(self.key_ptr, self.key_len))
        }
    }

    #[must_use]
    pub fn has(&self, flag: u32) -> bool {
        self.flags & flag != 0
    }
}

/// A record's fields or an enum's variants, in declaration order. Member
/// indices in events are positions in `members`.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct Table<'a> {
    members_ptr: *const Member<'a>,
    members_len: usize,
    /// A `#[wire]` type: binary formats use member tags instead of keys.
    pub tagged: bool,
    _members: PhantomData<&'a [Member<'a>]>,
}

impl<'a> Table<'a> {
    #[must_use]
    pub const fn new(members: &'a [Member<'a>], tagged: bool) -> Self {
        Self {
            members_ptr: members.as_ptr(),
            members_len: members.len(),
            tagged,
            _members: PhantomData,
        }
    }

    #[must_use]
    pub fn members(&self) -> &'a [Member<'a>] {
        // SAFETY: as for `Member::key`: built by `new` from a slice, or emitted
        // by the compiler as a static array of `members_len` members.
        unsafe { core::slice::from_raw_parts(self.members_ptr, self.members_len) }
    }
}

// SAFETY: tables are immutable views of immutable data.
unsafe impl Send for Member<'_> {}
// SAFETY: as above.
unsafe impl Sync for Member<'_> {}
// SAFETY: as above.
unsafe impl Send for Table<'_> {}
// SAFETY: as above.
unsafe impl Sync for Table<'_> {}

// The compiler emits these layouts: pointer-sized strings and counts, a u64
// tag and a u32 flag word per member, and a one-byte tagged flag per table.
const _: () = {
    let word = core::mem::size_of::<usize>();
    assert!(core::mem::size_of::<Member<'_>>() == 2 * word + 8 + 8);
    assert!(core::mem::offset_of!(Member<'_>, tag) == 2 * word);
    assert!(core::mem::offset_of!(Member<'_>, flags) == 2 * word + 8);
    assert!(core::mem::offset_of!(Table<'_>, tagged) == 2 * word);
};
