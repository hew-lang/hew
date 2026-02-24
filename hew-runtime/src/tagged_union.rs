#![expect(
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss,
    reason = "bit-preserving payload conversions for C-ABI tagged unions"
)]
pub(crate) const TAG_VARIANT_0: i32 = 0;
pub(crate) const TAG_VARIANT_1: i32 = 1;
pub(crate) const fn is_variant_0(tag: i32) -> bool {
    tag == TAG_VARIANT_0
}
pub(crate) const fn is_variant_1(tag: i32) -> bool {
    tag == TAG_VARIANT_1
}
pub(crate) fn encode_i32(value: i32) -> u64 {
    value as u64
}
pub(crate) fn encode_i64(value: i64) -> u64 {
    value as u64
}
pub(crate) fn encode_f64(value: f64) -> u64 {
    value.to_bits()
}
pub(crate) fn encode_ptr<T>(value: *mut T) -> u64 {
    value as u64
}
pub(crate) fn decode_i32(value: u64) -> i32 {
    value as i32
}
pub(crate) fn decode_i64(value: u64) -> i64 {
    value as i64
}
pub(crate) fn decode_f64(value: u64) -> f64 {
    f64::from_bits(value)
}
pub(crate) fn decode_mut_ptr<T>(value: u64) -> *mut T {
    value as *mut T
}
pub(crate) fn decode_const_ptr<T>(value: u64) -> *const T {
    value as *const T
}
