//! Hew runtime: `std::text::regex` module.
//!
//! Provides regular expression matching and replacement for compiled Hew
//! programs. All returned strings are allocated with `libc::malloc` so callers
//! can free them with `libc::free`.
use hew_cabi::{
    cabi::{cstr_to_str, str_to_malloc},
    vec::{hew_vec_new_str, hew_vec_push_str, HewVec},
};
use std::ffi::c_char;

/// Opaque handle wrapping a compiled [`regex::Regex`].
///
/// Created by [`hew_regex_new`], freed by [`hew_regex_free`].
#[derive(Debug)]
pub struct HewRegex {
    inner: regex::Regex,
}

/// Compile a regular expression pattern.
///
/// Returns a heap-allocated [`HewRegex`], or null if the pattern is invalid.
/// The caller must free it with [`hew_regex_free`].
///
/// # Safety
///
/// `pattern` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_regex_new(pattern: *const c_char) -> *mut HewRegex {
    // SAFETY: caller guarantees pattern is a valid NUL-terminated C string.
    let Some(pat) = (unsafe { cstr_to_str(pattern) }) else {
        return std::ptr::null_mut();
    };
    match regex::Regex::new(pat) {
        Ok(re) => Box::into_raw(Box::new(HewRegex { inner: re })),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Return true if `re` is a non-null regex handle.
#[no_mangle]
pub extern "C" fn hew_regex_is_valid(re: *const HewRegex) -> bool {
    !re.is_null()
}

/// Test whether `text` matches the compiled regex.
///
/// Returns `true` if the text matches, `false` otherwise.
///
/// # Safety
///
/// - `re` must be a valid pointer returned by [`hew_regex_new`].
/// - `text` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_regex_is_match(re: *const HewRegex, text: *const c_char) -> bool {
    if re.is_null() {
        return false;
    }
    // SAFETY: re is a valid HewRegex pointer per caller contract.
    let regex = unsafe { &*re };
    // SAFETY: text is a valid NUL-terminated C string per caller contract.
    let Some(text_str) = (unsafe { cstr_to_str(text) }) else {
        return false;
    };
    regex.inner.is_match(text_str)
}

/// Find the first match of the compiled regex in `text`.
///
/// Returns a `malloc`-allocated, NUL-terminated C string containing the
/// matched substring, or the canonical empty string (a non-null, zero-length
/// buffer) when there is no match. The caller must free the returned string
/// with `libc::free`.
///
/// The user-facing signature is `fn find(..) -> string` (not `Option<string>`),
/// documented as returning "the matched substring, or an empty string if no
/// match". Returning a raw null on no-match violated that contract at the string
/// equality boundary: `hew_string_length`/`hew_string_is_empty` treat null as
/// empty, but `hew_string_equals(null, "")` returns 0 (null and a non-null empty
/// buffer are unequal), so the idiomatic `pat.find(s) == ""` no-match check
/// silently reported a match. We normalize no-match to the canonical empty
/// string here at the FFI boundary so the returned value compares `== ""`.
///
/// # Safety
///
/// - `re` must be a valid pointer returned by [`hew_regex_new`].
/// - `text` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_regex_find(re: *const HewRegex, text: *const c_char) -> *mut c_char {
    if re.is_null() {
        return str_to_malloc("");
    }
    // SAFETY: re is a valid HewRegex pointer per caller contract.
    let regex = unsafe { &*re };
    // SAFETY: text is a valid NUL-terminated C string per caller contract.
    let Some(text_str) = (unsafe { cstr_to_str(text) }) else {
        return str_to_malloc("");
    };
    match regex.inner.find(text_str) {
        Some(m) => str_to_malloc(m.as_str()),
        None => str_to_malloc(""),
    }
}

unsafe fn new_string_vec() -> *mut HewVec {
    // SAFETY: runtime allocates a new Vec<string> handle owned by the caller.
    unsafe { hew_vec_new_str() }
}

unsafe fn push_string(vec: *mut HewVec, value: &str) {
    let cstr = str_to_malloc(value);
    if cstr.is_null() {
        return;
    }
    // SAFETY: `vec` is a live Vec<string> and `cstr` is a valid NUL-terminated string.
    unsafe { hew_vec_push_str(vec, cstr) };
    // SAFETY: `hew_vec_push_str` copies string contents into the vector.
    unsafe { hew_cabi::cabi::free_cstring(cstr) };
}

unsafe fn push_optional_capture(vec: *mut HewVec, capture: Option<regex::Match<'_>>) {
    // SAFETY: `vec` is a live Vec<string>; missing optional captures use Go-style "".
    unsafe { push_string(vec, capture.map_or("", |m| m.as_str())) };
}

/// Find every non-overlapping match of the compiled regex in `text`.
///
/// Returns a newly allocated `Vec<string>` containing all matches. The caller
/// owns the vector. Returns an empty vector for no matches, or null on invalid
/// input.
///
/// # Safety
///
/// - `re` must be a valid pointer returned by [`hew_regex_new`].
/// - `text` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_regex_find_all(
    re: *const HewRegex,
    text: *const c_char,
) -> *mut HewVec {
    if re.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: re is a valid HewRegex pointer per caller contract.
    let regex = unsafe { &*re };
    // SAFETY: text is a valid NUL-terminated C string per caller contract.
    let Some(text_str) = (unsafe { cstr_to_str(text) }) else {
        return std::ptr::null_mut();
    };
    // SAFETY: allocates a new Vec<string> owned by the caller.
    let out = unsafe { new_string_vec() };
    for m in regex.inner.find_iter(text_str) {
        // SAFETY: `out` is a live Vec<string>.
        unsafe { push_string(out, m.as_str()) };
    }
    out
}

/// Return one indexed capture from the first match.
///
/// Group 0 is the whole match. The returned `Vec<string>` has one element when
/// the requested capture is present, otherwise zero elements. This preserves
/// `Option<string>` semantics for empty-string captures.
///
/// # Safety
///
/// - `re` must be a valid pointer returned by [`hew_regex_new`].
/// - `text` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_regex_capture_index_one(
    re: *const HewRegex,
    text: *const c_char,
    group: i64,
) -> *mut HewVec {
    // SAFETY: allocates a new Vec<string> owned by the caller.
    let out = unsafe { new_string_vec() };
    if re.is_null() {
        return out;
    }
    let Ok(idx) = usize::try_from(group) else {
        return out;
    };
    // SAFETY: re is a valid HewRegex pointer per caller contract.
    let regex = unsafe { &*re };
    // SAFETY: text is a valid NUL-terminated C string per caller contract.
    let Some(text_str) = (unsafe { cstr_to_str(text) }) else {
        return out;
    };
    let Some(caps) = regex.inner.captures(text_str) else {
        return out;
    };
    if let Some(m) = caps.get(idx) {
        // SAFETY: `out` is a live Vec<string>.
        unsafe { push_string(out, m.as_str()) };
    }
    out
}

/// Return one named capture from the first match.
///
/// The returned `Vec<string>` has one element when the requested capture is
/// present, otherwise zero elements.
///
/// # Safety
///
/// - `re` must be a valid pointer returned by [`hew_regex_new`].
/// - `text` and `name` must be valid NUL-terminated C strings.
#[no_mangle]
pub unsafe extern "C" fn hew_regex_capture_name_one(
    re: *const HewRegex,
    text: *const c_char,
    name: *const c_char,
) -> *mut HewVec {
    // SAFETY: allocates a new Vec<string> owned by the caller.
    let out = unsafe { new_string_vec() };
    if re.is_null() {
        return out;
    }
    // SAFETY: re is a valid HewRegex pointer per caller contract.
    let regex = unsafe { &*re };
    // SAFETY: text is a valid NUL-terminated C string per caller contract.
    let Some(text_str) = (unsafe { cstr_to_str(text) }) else {
        return out;
    };
    // SAFETY: name is a valid NUL-terminated C string per caller contract.
    let Some(name_str) = (unsafe { cstr_to_str(name) }) else {
        return out;
    };
    let Some(caps) = regex.inner.captures(text_str) else {
        return out;
    };
    if let Some(m) = caps.name(name_str) {
        // SAFETY: `out` is a live Vec<string>.
        unsafe { push_string(out, m.as_str()) };
    }
    out
}

/// Return first-match submatches in row-major flat form.
///
/// The returned vector contains group 0 followed by capture groups. Missing
/// optional groups are represented as an empty string. No match returns an empty
/// vector.
///
/// # Safety
///
/// - `re` must be a valid pointer returned by [`hew_regex_new`].
/// - `text` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_regex_captures_flat(
    re: *const HewRegex,
    text: *const c_char,
) -> *mut HewVec {
    // SAFETY: allocates a new Vec<string> owned by the caller.
    let out = unsafe { new_string_vec() };
    if re.is_null() {
        return out;
    }
    // SAFETY: re is a valid HewRegex pointer per caller contract.
    let regex = unsafe { &*re };
    // SAFETY: text is a valid NUL-terminated C string per caller contract.
    let Some(text_str) = (unsafe { cstr_to_str(text) }) else {
        return out;
    };
    let Some(caps) = regex.inner.captures(text_str) else {
        return out;
    };
    for idx in 0..caps.len() {
        // SAFETY: `out` is a live Vec<string>.
        unsafe { push_optional_capture(out, caps.get(idx)) };
    }
    out
}

/// Return all matches' submatches in row-major flat form.
///
/// Each row has [`hew_regex_capture_width`] entries: group 0 followed by capture
/// groups. Missing optional groups are represented as an empty string.
///
/// # Safety
///
/// - `re` must be a valid pointer returned by [`hew_regex_new`].
/// - `text` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_regex_find_all_submatch_flat(
    re: *const HewRegex,
    text: *const c_char,
) -> *mut HewVec {
    // SAFETY: allocates a new Vec<string> owned by the caller.
    let out = unsafe { new_string_vec() };
    if re.is_null() {
        return out;
    }
    // SAFETY: re is a valid HewRegex pointer per caller contract.
    let regex = unsafe { &*re };
    // SAFETY: text is a valid NUL-terminated C string per caller contract.
    let Some(text_str) = (unsafe { cstr_to_str(text) }) else {
        return out;
    };
    for caps in regex.inner.captures_iter(text_str) {
        for idx in 0..caps.len() {
            // SAFETY: `out` is a live Vec<string>.
            unsafe { push_optional_capture(out, caps.get(idx)) };
        }
    }
    out
}

/// Return the number of submatch groups per match row, including group 0.
///
/// # Safety
///
/// `re` must be a valid pointer returned by [`hew_regex_new`].
#[no_mangle]
#[expect(
    clippy::cast_possible_wrap,
    reason = "regex capture count fits in i64 for Hew programs"
)]
pub unsafe extern "C" fn hew_regex_capture_width(re: *const HewRegex) -> i64 {
    if re.is_null() {
        return 0;
    }
    // SAFETY: re is a valid HewRegex pointer per caller contract.
    let regex = unsafe { &*re };
    regex.inner.captures_len() as i64
}

/// Replace all matches of the compiled regex in `text` with `replacement`.
///
/// Returns a `malloc`-allocated, NUL-terminated C string. The caller must free
/// it with `libc::free`. On an invalid handle or non-UTF-8 input it returns the
/// canonical empty string (a non-null, zero-length buffer) rather than raw null,
/// so the `-> string` result always compares correctly against a string literal
/// (see the equality-boundary note on [`hew_regex_find`]).
///
/// # Safety
///
/// - `re` must be a valid pointer returned by [`hew_regex_new`].
/// - `text` and `replacement` must be valid NUL-terminated C strings.
#[no_mangle]
pub unsafe extern "C" fn hew_regex_replace(
    re: *const HewRegex,
    text: *const c_char,
    replacement: *const c_char,
) -> *mut c_char {
    if re.is_null() {
        return str_to_malloc("");
    }
    // SAFETY: re is a valid HewRegex pointer per caller contract.
    let regex = unsafe { &*re };
    // SAFETY: text is a valid NUL-terminated C string per caller contract.
    let Some(text_str) = (unsafe { cstr_to_str(text) }) else {
        return str_to_malloc("");
    };
    // SAFETY: replacement is a valid NUL-terminated C string per caller contract.
    let Some(repl_str) = (unsafe { cstr_to_str(replacement) }) else {
        return str_to_malloc("");
    };
    str_to_malloc(&regex.inner.replace_all(text_str, repl_str))
}

// ── Codegen-emitted runtime ABI ──────────────────────────────────────────────
//
// These three functions are called ONLY by compiler-generated code (from
// regex-literal match-arm lowering). They are distinct from the user-callable
// `hew_regex_new` / `hew_regex_is_match` / `hew_regex_find` family above.
//
// Naming convention:
//   hew_regex_compile — compile once at module init (called by @llvm.global_ctors)
//   hew_regex_match   — check a match (called per match arm, returns i32 for C ABI)
//   hew_regex_capture — extract a named capture by 0-based index (called per capture binding)
//
// WHY separate from hew_regex_new/hew_regex_is_match: the codegen substrate
// uses a module-level global handle array indexed by literal_id; `hew_regex_compile`
// populates that array at init time. The user-callable family operates on
// user-allocated handles. Keeping them separate avoids coupling the compiler's
// internal representation to the public stdlib API.
// WHEN-OBSOLETE: if the compiler gains a Place::RegexHandle primitive, the
// literal_id indirection in hew_regex_match/hew_regex_capture could be replaced
// by direct handle passing; hew_regex_compile would still be needed for init.
// WHAT: hew-codegen-rs emits calls from lower_call_runtime_abi arms (slice 5).

/// Compile a regex pattern for use by compiler-generated match arms.
///
/// Called once per regex literal at module init time (from the function
/// registered in `@llvm.global_ctors`). Returns a heap-allocated
/// [`HewRegex`] on success, or null if the pattern is invalid. Since
/// patterns are validated by the type-checker before codegen, a null return
/// indicates an internal invariant violation; codegen traps fail-closed.
///
/// The returned handle is stored in the module-level global handle array and
/// is never freed (module-lifetime). Callers must NOT pass it to
/// [`hew_regex_free`].
///
/// # Safety
///
/// `pattern` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_regex_compile(pattern: *const c_char) -> *mut HewRegex {
    // SAFETY: caller guarantees pattern is a valid NUL-terminated C string.
    let Some(pat) = (unsafe { cstr_to_str(pattern) }) else {
        return std::ptr::null_mut();
    };
    match regex::Regex::new(pat) {
        Ok(re) => Box::into_raw(Box::new(HewRegex { inner: re })),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Test whether `text` matches a compiled regex, identified by handle.
///
/// Returns `1` if `text` matches, `0` otherwise. Returns `0` on any null
/// argument. Returns `i32` (not `bool`) to match the C ABI convention used
/// by other predicate-returning runtime entries and because MIR emits an
/// `IntCmp(NotEq, result, 0i32)` immediately after the call.
///
/// Called by compiler-generated code for each regex match arm. The `re`
/// pointer is loaded from the module-level global handle array by an LLVM GEP
/// in the caller; it is the codegen's responsibility to pass a valid,
/// non-null handle.
///
/// # Safety
///
/// - `re` must be a valid, non-null pointer returned by [`hew_regex_compile`].
/// - `text` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_regex_match(re: *const HewRegex, text: *const c_char) -> i32 {
    if re.is_null() {
        return 0;
    }
    // SAFETY: re is a valid HewRegex pointer per caller contract.
    let regex = unsafe { &*re };
    // SAFETY: text is a valid NUL-terminated C string per caller contract.
    let Some(text_str) = (unsafe { cstr_to_str(text) }) else {
        return 0;
    };
    i32::from(regex.inner.is_match(text_str))
}

/// Extract a capture group by its 1-based regex group index from a successful match.
///
/// Called by compiler-generated code for each named capture binding in a
/// regex match arm, after [`hew_regex_match`] has confirmed a match.
/// `capture_idx` is the 1-based regex group position (group 0 is the whole
/// match; group 1 is the first capture group regardless of whether it is
/// named). The compiler resolves name→group-index at type-check time and
/// stores the real group position in the HIR, so this function receives the
/// actual group slot even when unnamed positional groups precede named ones.
///
/// Returns a `malloc`-allocated, NUL-terminated C string containing the
/// capture content, or null if the group did not participate in the match
/// (non-participating optional group). Callers must free with
/// [`hew_regex_free_capture`].
///
/// WHY by group index not by name: MIR stores the group index as a `ConstI64`
/// to avoid passing string constants through the ABI at every call site.
/// The compiler resolves name→group-index at type-check time (checker side,
/// `PatternKind::Regex { captures: Vec<(String, u32)> }`).
///
/// # Safety
///
/// - `re` must be a valid, non-null pointer returned by [`hew_regex_compile`].
/// - `text` must be a valid NUL-terminated C string.
/// - `capture_idx` must be a valid 1-based group index into the pattern's capture groups.
#[no_mangle]
pub unsafe extern "C" fn hew_regex_capture(
    re: *const HewRegex,
    text: *const c_char,
    capture_idx: i64,
) -> *mut c_char {
    if re.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: re is a valid HewRegex pointer per caller contract.
    let regex = unsafe { &*re };
    // SAFETY: text is a valid NUL-terminated C string per caller contract.
    let Some(text_str) = (unsafe { cstr_to_str(text) }) else {
        return std::ptr::null_mut();
    };
    let Ok(idx) = usize::try_from(capture_idx) else {
        return std::ptr::null_mut();
    };
    // `capture_idx` is the 1-based regex group index (group 0 is the whole
    // match). We use `caps.get(idx)` directly — no +1 offset — because the
    // compiler now stores the real group position, not the named-only ordinal.
    // This correctly handles patterns where unnamed groups precede named ones:
    // e.g. `(foo)(?P<bar>bar)` → group 1 is `foo` (unnamed), group 2 is `bar`
    // (named); the compiler passes idx=2 for `bar` so `caps.get(2)` is correct.
    let Some(caps) = regex.inner.captures(text_str) else {
        return std::ptr::null_mut();
    };
    let capture_str = match caps.get(idx) {
        Some(m) => m.as_str(),
        None => return std::ptr::null_mut(),
    };
    str_to_malloc(capture_str)
}

/// Free a capture string returned by [`hew_regex_capture`].
///
/// `hew_regex_capture` returns a `malloc`-allocated, NUL-terminated C string.
/// Compiler-generated code calls this function at arm-body exit (after the
/// capture value has been used) and on the null-fail paths (when earlier
/// captures in a multi-capture arm were allocated but a later one was null).
///
/// Passing a null pointer is a no-op (mirrors `libc::free` semantics).
///
/// WHY a wrapper rather than calling `libc::free` directly: isolates the
/// allocation ABI from the codegen side so the underlying allocator can change
/// without touching codegen. WHEN-OBSOLETE: if MIR gains a typed `CStringDrop`
/// instruction that maps directly to the allocator's free, this wrapper becomes
/// dead code. WHAT: add `Instr::CStringDrop { src: Place }` to hew-mir and wire
/// codegen to the system allocator's free directly.
///
/// # Safety
///
/// - `ptr` must be null or a pointer returned by [`hew_regex_capture`].
/// - After this call, `ptr` must not be used again.
#[no_mangle]
pub unsafe extern "C" fn hew_regex_free_capture(ptr: *mut c_char) {
    if ptr.is_null() {
        return;
    }
    // SAFETY: ptr is either null (checked above) or a valid malloc-owned pointer
    // returned by hew_regex_capture (which calls str_to_malloc → CString::into_raw
    // → a malloc allocation). Freeing it here releases the allocation.
    unsafe { hew_cabi::cabi::free_cstring(ptr) } // CSTRING-FREE: str-open (frees hew_regex_capture = str_to_malloc)
}

/// Clone a compiled [`HewRegex`].
///
/// Returns a heap-allocated copy that is independent of the original.
/// Both handles must be freed with [`hew_regex_free`].
///
/// # Safety
///
/// `re` must be a valid pointer returned by [`hew_regex_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_regex_clone(re: *const HewRegex) -> *mut HewRegex {
    if re.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: re is a valid HewRegex pointer per caller contract.
    let src = unsafe { &*re };
    Box::into_raw(Box::new(HewRegex {
        inner: src.inner.clone(),
    }))
}

/// Free a compiled [`HewRegex`] previously returned by [`hew_regex_new`].
///
/// # Safety
///
/// `re` must be a pointer previously returned by [`hew_regex_new`], and must
/// not have been freed already.
#[no_mangle]
pub unsafe extern "C" fn hew_regex_free(re: *mut HewRegex) {
    if re.is_null() {
        return;
    }
    // SAFETY: re was allocated with Box::into_raw in hew_regex_new.
    drop(unsafe { Box::from_raw(re) });
}

#[cfg(test)]
mod tests {
    use super::*;
    use hew_cabi::vec::{hew_vec_free, hew_vec_get_str, hew_vec_len};
    use std::ffi::{CStr, CString};

    unsafe fn vec_to_strings(vec: *mut HewVec) -> Vec<String> {
        assert!(!vec.is_null());
        // SAFETY: vec is a valid HewVec returned by the regex FFI under test.
        let len = unsafe { hew_vec_len(vec) };
        let mut out = Vec::new();
        for i in 0..len {
            // SAFETY: i is in bounds for vec.
            let ptr = unsafe { hew_vec_get_str(vec, i) };
            assert!(!ptr.is_null());
            // SAFETY: hew_vec_get_str returns a valid borrowed C string pointer.
            let s = unsafe { CStr::from_ptr(ptr) }.to_str().unwrap().to_owned();
            out.push(s);
        }
        out
    }

    #[test]
    fn test_regex_is_match() {
        let pattern = CString::new(r"\d+").unwrap();
        // SAFETY: pattern is a valid NUL-terminated C string.
        let re = unsafe { hew_regex_new(pattern.as_ptr()) };
        assert!(!re.is_null());

        let text_yes = CString::new("abc123def").unwrap();
        let text_no = CString::new("abcdef").unwrap();
        // SAFETY: re and text pointers are valid.
        assert!(unsafe { hew_regex_is_match(re, text_yes.as_ptr()) });
        // SAFETY: re and text pointers are valid.
        assert!(!unsafe { hew_regex_is_match(re, text_no.as_ptr()) });

        // SAFETY: re was returned by hew_regex_new.
        unsafe { hew_regex_free(re) };
    }

    #[test]
    fn test_regex_find() {
        let pattern = CString::new(r"[a-z]+").unwrap();
        // SAFETY: pattern is a valid NUL-terminated C string.
        let re = unsafe { hew_regex_new(pattern.as_ptr()) };
        assert!(!re.is_null());

        let text = CString::new("123hello456").unwrap();
        // SAFETY: re and text pointers are valid.
        let matched = unsafe { hew_regex_find(re, text.as_ptr()) };
        assert!(!matched.is_null());
        // SAFETY: matched was allocated by hew_regex_find.
        let matched_str = unsafe { CStr::from_ptr(matched) }.to_str().unwrap();
        assert_eq!(matched_str, "hello");
        // SAFETY: matched was allocated with libc::malloc.
        unsafe { hew_cabi::cabi::free_cstring(matched) }; // CSTRING-FREE: str-open (test str_to_malloc match)

        // Test no match: returns the canonical empty string (non-null, len 0),
        // NOT raw null, so `pat.find(s) == ""` works (hew_string_equals treats a
        // null and a non-null empty buffer as unequal).
        let text_no = CString::new("123456").unwrap();
        // SAFETY: re and text pointers are valid.
        let no_match = unsafe { hew_regex_find(re, text_no.as_ptr()) };
        assert!(!no_match.is_null());
        // SAFETY: no_match was allocated by hew_regex_find.
        let no_match_str = unsafe { CStr::from_ptr(no_match) }.to_str().unwrap();
        assert_eq!(no_match_str, "");
        // SAFETY: no_match was allocated with libc::malloc.
        unsafe { hew_cabi::cabi::free_cstring(no_match) }; // CSTRING-FREE: str-open (test str_to_malloc match)

        // SAFETY: re was returned by hew_regex_new.
        unsafe { hew_regex_free(re) };
    }

    #[test]
    fn find_no_match_returns_canonical_empty_not_null() {
        // Regression for the no-match-vs-empty equality-boundary bug: find must
        // return a non-null, zero-length, NUL-terminated buffer (the canonical
        // empty string) on no match, so `hew_regex_find(..) == ""` holds. A raw
        // null return would make hew_string_equals(null, "") report 0 (unequal)
        // even though hew_string_length/hew_string_is_empty treat null as empty.
        let pattern = CString::new(r"[0-9]+").unwrap();
        // SAFETY: pattern is a valid NUL-terminated C string.
        let re = unsafe { hew_regex_new(pattern.as_ptr()) };
        assert!(!re.is_null());

        let text = CString::new("abc").unwrap();
        // SAFETY: re and text pointers are valid.
        let got = unsafe { hew_regex_find(re, text.as_ptr()) };
        assert!(!got.is_null(), "no-match must not return null");
        // SAFETY: got is a valid NUL-terminated buffer from hew_regex_find.
        assert_eq!(unsafe { *got }, 0, "no-match buffer must be zero-length");
        // SAFETY: got was allocated with libc::malloc.
        unsafe { hew_cabi::cabi::free_cstring(got) }; // CSTRING-FREE: str-open (test str_to_malloc match)

        // A null handle also returns the canonical empty, never null.
        // SAFETY: passing a null handle is the documented invalid-handle path.
        let from_null = unsafe { hew_regex_find(std::ptr::null(), text.as_ptr()) };
        assert!(!from_null.is_null());
        // SAFETY: from_null is a valid buffer from hew_regex_find.
        assert_eq!(unsafe { *from_null }, 0);
        // SAFETY: from_null was allocated with libc::malloc.
        unsafe { hew_cabi::cabi::free_cstring(from_null) }; // CSTRING-FREE: str-open (test str_to_malloc match)

        // SAFETY: re was returned by hew_regex_new.
        unsafe { hew_regex_free(re) };
    }

    #[test]
    fn replace_invalid_handle_returns_canonical_empty_not_null() {
        // The `-> string` replace seam also normalizes its error paths to the
        // canonical empty string rather than raw null.
        let text = CString::new("abc").unwrap();
        let repl = CString::new("X").unwrap();
        // SAFETY: a null handle is the documented invalid-handle path.
        let got = unsafe { hew_regex_replace(std::ptr::null(), text.as_ptr(), repl.as_ptr()) };
        assert!(!got.is_null());
        // SAFETY: got is a valid NUL-terminated buffer from hew_regex_replace.
        assert_eq!(unsafe { *got }, 0);
        // SAFETY: got was allocated with libc::malloc.
        unsafe { hew_cabi::cabi::free_cstring(got) }; // CSTRING-FREE: str-open (test str_to_malloc match)
    }

    #[test]
    fn find_all_returns_all_non_overlapping_matches() {
        let pattern = CString::new(r"[a-z]+").unwrap();
        // SAFETY: pattern is a valid NUL-terminated C string.
        let re = unsafe { hew_regex_new(pattern.as_ptr()) };
        assert!(!re.is_null());

        let text = CString::new("12ab34cd").unwrap();
        // SAFETY: re and text pointers are valid.
        let matches = unsafe { hew_regex_find_all(re, text.as_ptr()) };
        // SAFETY: matches is a valid Vec<string> returned by hew_regex_find_all.
        assert_eq!(unsafe { vec_to_strings(matches) }, ["ab", "cd"]);
        // SAFETY: matches was allocated by hew_vec_new_str.
        unsafe { hew_vec_free(matches) };

        let text_no = CString::new("1234").unwrap();
        // SAFETY: re and text pointers are valid.
        let empty = unsafe { hew_regex_find_all(re, text_no.as_ptr()) };
        // SAFETY: empty is a valid Vec<string> returned by hew_regex_find_all.
        assert_eq!(unsafe { hew_vec_len(empty) }, 0);
        // SAFETY: empty was allocated by hew_vec_new_str.
        unsafe { hew_vec_free(empty) };

        // SAFETY: re was returned by hew_regex_new.
        unsafe { hew_regex_free(re) };
    }

    #[test]
    fn capture_index_one_preserves_empty_capture_and_absence() {
        let pattern = CString::new(r"([a-z]*)([0-9]+)").unwrap();
        // SAFETY: pattern is a valid NUL-terminated C string.
        let re = unsafe { hew_regex_new(pattern.as_ptr()) };
        assert!(!re.is_null());

        let text = CString::new("123").unwrap();
        // SAFETY: re and text pointers are valid; group 0 is the whole match.
        let whole = unsafe { hew_regex_capture_index_one(re, text.as_ptr(), 0) };
        // SAFETY: whole is a valid Vec<string> returned by the regex FFI.
        assert_eq!(unsafe { vec_to_strings(whole) }, ["123"]);
        // SAFETY: whole was allocated by hew_vec_new_str.
        unsafe { hew_vec_free(whole) };

        // SAFETY: re and text pointers are valid; group 1 captures an empty string.
        let empty_capture = unsafe { hew_regex_capture_index_one(re, text.as_ptr(), 1) };
        // SAFETY: empty_capture is a valid Vec<string> returned by the regex FFI.
        assert_eq!(unsafe { vec_to_strings(empty_capture) }, [""]);
        // SAFETY: empty_capture was allocated by hew_vec_new_str.
        unsafe { hew_vec_free(empty_capture) };

        // SAFETY: re and text pointers are valid; group 99 is absent.
        let missing = unsafe { hew_regex_capture_index_one(re, text.as_ptr(), 99) };
        // SAFETY: missing is a valid Vec<string> returned by the regex FFI.
        assert_eq!(unsafe { hew_vec_len(missing) }, 0);
        // SAFETY: missing was allocated by hew_vec_new_str.
        unsafe { hew_vec_free(missing) };

        // SAFETY: re was returned by hew_regex_new.
        unsafe { hew_regex_free(re) };
    }

    #[test]
    fn capture_name_one_returns_named_capture() {
        let pattern = CString::new(r"(?P<word>[a-z]+)-(?P<num>[0-9]+)").unwrap();
        // SAFETY: pattern is a valid NUL-terminated C string.
        let re = unsafe { hew_regex_new(pattern.as_ptr()) };
        assert!(!re.is_null());

        let text = CString::new("abc-123").unwrap();
        let name = CString::new("num").unwrap();
        // SAFETY: re, text, and name pointers are valid.
        let capture = unsafe { hew_regex_capture_name_one(re, text.as_ptr(), name.as_ptr()) };
        // SAFETY: capture is a valid Vec<string> returned by the regex FFI.
        assert_eq!(unsafe { vec_to_strings(capture) }, ["123"]);
        // SAFETY: capture was allocated by hew_vec_new_str.
        unsafe { hew_vec_free(capture) };

        let missing_name = CString::new("missing").unwrap();
        // SAFETY: re, text, and name pointers are valid.
        let missing =
            unsafe { hew_regex_capture_name_one(re, text.as_ptr(), missing_name.as_ptr()) };
        // SAFETY: missing is a valid Vec<string> returned by the regex FFI.
        assert_eq!(unsafe { hew_vec_len(missing) }, 0);
        // SAFETY: missing was allocated by hew_vec_new_str.
        unsafe { hew_vec_free(missing) };

        // SAFETY: re was returned by hew_regex_new.
        unsafe { hew_regex_free(re) };
    }

    #[test]
    fn captures_flat_returns_first_match_row() {
        let pattern = CString::new(r"([a-z]+)-([0-9]+)?").unwrap();
        // SAFETY: pattern is a valid NUL-terminated C string.
        let re = unsafe { hew_regex_new(pattern.as_ptr()) };
        assert!(!re.is_null());

        // SAFETY: re is valid.
        assert_eq!(unsafe { hew_regex_capture_width(re) }, 3);

        let text = CString::new("abc-").unwrap();
        // SAFETY: re and text pointers are valid.
        let captures = unsafe { hew_regex_captures_flat(re, text.as_ptr()) };
        // SAFETY: captures is a valid Vec<string> returned by the regex FFI.
        assert_eq!(unsafe { vec_to_strings(captures) }, ["abc-", "abc", ""]);
        // SAFETY: captures was allocated by hew_vec_new_str.
        unsafe { hew_vec_free(captures) };

        let text_no = CString::new("123").unwrap();
        // SAFETY: re and text pointers are valid.
        let no_match = unsafe { hew_regex_captures_flat(re, text_no.as_ptr()) };
        // SAFETY: no_match is a valid Vec<string> returned by the regex FFI.
        assert_eq!(unsafe { hew_vec_len(no_match) }, 0);
        // SAFETY: no_match was allocated by hew_vec_new_str.
        unsafe { hew_vec_free(no_match) };

        // SAFETY: re was returned by hew_regex_new.
        unsafe { hew_regex_free(re) };
    }

    #[test]
    fn find_all_submatch_flat_returns_row_major_groups() {
        let pattern = CString::new(r"([a-z]+)([0-9]+)").unwrap();
        // SAFETY: pattern is a valid NUL-terminated C string.
        let re = unsafe { hew_regex_new(pattern.as_ptr()) };
        assert!(!re.is_null());

        let text = CString::new("a1 b22").unwrap();
        // SAFETY: re and text pointers are valid.
        let captures = unsafe { hew_regex_find_all_submatch_flat(re, text.as_ptr()) };
        assert_eq!(
            // SAFETY: captures is a valid Vec<string> returned by the regex FFI.
            unsafe { vec_to_strings(captures) },
            ["a1", "a", "1", "b22", "b", "22"]
        );
        // SAFETY: captures was allocated by hew_vec_new_str.
        unsafe { hew_vec_free(captures) };

        // SAFETY: re was returned by hew_regex_new.
        unsafe { hew_regex_free(re) };
    }

    #[test]
    fn test_regex_replace() {
        let pattern = CString::new(r"\d+").unwrap();
        // SAFETY: pattern is a valid NUL-terminated C string.
        let re = unsafe { hew_regex_new(pattern.as_ptr()) };
        assert!(!re.is_null());

        let text = CString::new("a1b2c3").unwrap();
        let repl = CString::new("X").unwrap();
        // SAFETY: re, text, and repl pointers are valid.
        let result = unsafe { hew_regex_replace(re, text.as_ptr(), repl.as_ptr()) };
        assert!(!result.is_null());
        // SAFETY: result was allocated by hew_regex_replace.
        let result_str = unsafe { CStr::from_ptr(result) }.to_str().unwrap();
        assert_eq!(result_str, "aXbXcX");
        // SAFETY: result was allocated with libc::malloc.
        unsafe { hew_cabi::cabi::free_cstring(result) }; // CSTRING-FREE: str-open (test str_to_malloc result)

        // SAFETY: re was returned by hew_regex_new.
        unsafe { hew_regex_free(re) };
    }

    #[test]
    fn test_regex_null_safety() {
        // SAFETY: Testing null pointer handling.
        assert!(unsafe { hew_regex_new(std::ptr::null()) }.is_null());
        assert!(!hew_regex_is_valid(std::ptr::null()));
        assert!(
            // SAFETY: Testing null pointer handling.
            !unsafe { hew_regex_is_match(std::ptr::null(), std::ptr::null()) },
        );
        // SAFETY: Testing null pointer handling — should not crash.
        unsafe { hew_regex_free(std::ptr::null_mut()) };
    }

    #[test]
    fn codegen_compile_valid_pattern_returns_non_null() {
        let pattern = CString::new(r"(?P<year>\d{4})-(?P<month>\d{2})").unwrap();
        // SAFETY: pattern is a valid NUL-terminated C string.
        let re = unsafe { hew_regex_compile(pattern.as_ptr()) };
        assert!(
            !re.is_null(),
            "hew_regex_compile must return non-null for a valid pattern"
        );
        // Leak intentionally: module-lifetime handles are not freed.
        // In production the handle lives for the process lifetime in the
        // global handle array. In tests we accept the leak via Box::from_raw.
        // SAFETY: re is a valid non-null pointer returned by hew_regex_compile.
        let _ = unsafe { Box::from_raw(re) };
    }

    #[test]
    fn codegen_compile_invalid_pattern_returns_null() {
        let pattern = CString::new(r"(?P<bad").unwrap();
        // SAFETY: pattern is a valid NUL-terminated C string.
        let re = unsafe { hew_regex_compile(pattern.as_ptr()) };
        assert!(
            re.is_null(),
            "hew_regex_compile must return null for an invalid pattern"
        );
    }

    #[test]
    fn codegen_match_returns_one_on_match_zero_on_no_match() {
        let pattern = CString::new(r"\d+").unwrap();
        // SAFETY: pattern is a valid NUL-terminated C string.
        let re = unsafe { hew_regex_compile(pattern.as_ptr()) };
        assert!(!re.is_null());

        let text_yes = CString::new("abc123").unwrap();
        let text_no = CString::new("abcdef").unwrap();
        // SAFETY: re and text pointers are valid.
        assert_eq!(unsafe { hew_regex_match(re, text_yes.as_ptr()) }, 1);
        // SAFETY: re and text pointers are valid.
        assert_eq!(unsafe { hew_regex_match(re, text_no.as_ptr()) }, 0);

        // SAFETY: re is a valid non-null pointer returned by hew_regex_compile.
        let _ = unsafe { Box::from_raw(re) };
    }

    #[test]
    fn codegen_match_null_re_returns_zero() {
        let text = CString::new("hello").unwrap();
        assert_eq!(
            // SAFETY: Testing null pointer handling — null re must return 0.
            unsafe { hew_regex_match(std::ptr::null(), text.as_ptr()) },
            0
        );
    }

    #[test]
    fn codegen_capture_by_index_returns_matched_group() {
        let pattern = CString::new(r"(?P<year>\d{4})-(?P<month>\d{2})").unwrap();
        // SAFETY: pattern is a valid NUL-terminated C string.
        let re = unsafe { hew_regex_compile(pattern.as_ptr()) };
        assert!(!re.is_null());

        let text = CString::new("date: 2024-03 end").unwrap();
        // capture_idx is the 1-based regex group index (group 0 is the whole match).
        // Pattern `(?P<year>...)` is group 1, `(?P<month>...)` is group 2.
        // The compiler resolves name→group-index at check time and passes the real
        // group index; passing 1 here matches the value MIR would emit for `year`.
        // SAFETY: re and text pointers are valid; capture_idx is in-bounds.
        let year_raw = unsafe { hew_regex_capture(re, text.as_ptr(), 1) };
        assert!(
            !year_raw.is_null(),
            "capture idx 1 (year, group 1) must match"
        );
        // SAFETY: year_raw was allocated by hew_regex_capture with malloc.
        let year_val = unsafe { CStr::from_ptr(year_raw) }.to_str().unwrap();
        assert_eq!(year_val, "2024");
        // SAFETY: year_raw was allocated by hew_regex_capture; free via the wrapper.
        unsafe { hew_regex_free_capture(year_raw) };

        // SAFETY: re and text pointers are valid; capture_idx is in-bounds.
        let month_raw = unsafe { hew_regex_capture(re, text.as_ptr(), 2) };
        assert!(
            !month_raw.is_null(),
            "capture idx 2 (month, group 2) must match"
        );
        // SAFETY: month_raw was allocated by hew_regex_capture with malloc.
        let month_val = unsafe { CStr::from_ptr(month_raw) }.to_str().unwrap();
        assert_eq!(month_val, "03");
        // SAFETY: month_raw was allocated by hew_regex_capture; free via the wrapper.
        unsafe { hew_regex_free_capture(month_raw) };

        // SAFETY: re is a valid non-null pointer returned by hew_regex_compile.
        let _ = unsafe { Box::from_raw(re) };
    }

    /// A pattern with an unnamed positional group BEFORE a named group: verifies
    /// the real group index lookup is used (not the named-only ordinal).
    /// Pattern `(foo)(?P<bar>bar)` → group 1 = `foo` (unnamed), group 2 = `bar` (named).
    /// The compiler emits `capture_idx=2` for the `bar` binding.
    #[test]
    fn codegen_capture_unnamed_before_named_uses_real_group_index() {
        let pattern = CString::new(r"(foo)(?P<bar>bar)").unwrap();
        // SAFETY: pattern is a valid NUL-terminated C string.
        let re = unsafe { hew_regex_compile(pattern.as_ptr()) };
        assert!(!re.is_null());

        let text = CString::new("foobar").unwrap();
        // Group 2 is the named capture `bar`. The old code used idx+1=1 which
        // returns `foo` (wrong). The corrected code passes idx=2 → `bar`.
        // SAFETY: re and text pointers are valid.
        // SAFETY: re and text pointers are valid; capture_idx 2 is in-bounds.
        let bar_raw = unsafe { hew_regex_capture(re, text.as_ptr(), 2) };
        assert!(
            !bar_raw.is_null(),
            "capture at real group index 2 (bar) must match"
        );
        // SAFETY: bar_raw is a valid NUL-terminated C string allocated by hew_regex_capture.
        let bar_val = unsafe { CStr::from_ptr(bar_raw) }.to_str().unwrap();
        assert_eq!(bar_val, "bar", "group 2 must be 'bar', not 'foo'");
        // SAFETY: bar_raw was allocated by hew_regex_capture; free via the wrapper.
        unsafe { hew_regex_free_capture(bar_raw) };

        // Also verify group 1 (unnamed `foo`) is accessible by real index.
        // SAFETY: re and text pointers are valid; capture_idx 1 is in-bounds.
        let foo_raw = unsafe { hew_regex_capture(re, text.as_ptr(), 1) };
        assert!(
            !foo_raw.is_null(),
            "group 1 (unnamed foo) must be accessible"
        );
        // SAFETY: foo_raw is a valid NUL-terminated C string allocated by hew_regex_capture.
        let foo_val = unsafe { CStr::from_ptr(foo_raw) }.to_str().unwrap();
        assert_eq!(foo_val, "foo");
        // SAFETY: foo_raw was allocated by hew_regex_capture; free via the wrapper.
        unsafe { hew_regex_free_capture(foo_raw) };

        // SAFETY: re is a valid non-null pointer returned by hew_regex_compile.
        let _ = unsafe { Box::from_raw(re) };
    }

    #[test]
    fn codegen_capture_out_of_bounds_returns_null() {
        let pattern = CString::new(r"(?P<word>\w+)").unwrap();
        // SAFETY: pattern is a valid NUL-terminated C string.
        let re = unsafe { hew_regex_compile(pattern.as_ptr()) };
        assert!(!re.is_null());

        let text = CString::new("hello").unwrap();
        // capture_idx=5 is beyond the single named group — must return null.
        // SAFETY: re and text pointers are valid.
        let oob_ptr = unsafe { hew_regex_capture(re, text.as_ptr(), 5) };
        assert!(
            oob_ptr.is_null(),
            "out-of-bounds capture index must return null"
        );

        // SAFETY: re is a valid non-null pointer returned by hew_regex_compile.
        let _ = unsafe { Box::from_raw(re) };
    }
}
