//! Per-type schema tables: the only runtime copy of a record's or enum's
//! member keys, tags and presence rules.

/// One record field or enum variant.
#[derive(Debug, Clone, Copy)]
pub struct Member<'a> {
    /// The text key used by every text format, and by binary formats when the
    /// table is not tagged.
    pub key: &'a str,
    /// The stable `@N` tag, used by binary formats when the table is tagged.
    pub tag: u64,
    /// `Member::*` flag bits.
    pub flags: u32,
}

impl Member<'_> {
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

    #[must_use]
    pub fn has(&self, flag: u32) -> bool {
        self.flags & flag != 0
    }
}

/// A record's fields or an enum's variants, in declaration order. Member
/// indices in events are positions in `members`.
#[derive(Debug, Clone, Copy)]
pub struct Table<'a> {
    /// The record or enum name, for diagnostics.
    pub name: &'a str,
    pub members: &'a [Member<'a>],
    /// A `#[wire]` type: binary formats use member tags instead of keys.
    pub tagged: bool,
}
