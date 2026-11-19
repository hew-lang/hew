//! Source declarations with ordinary C linkage and value layout.

/// Source-authorized external symbols and C-layout records, keyed by checked identity.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct SourceCAbi {
    pub exports: std::collections::BTreeMap<crate::DefId, String>,
    pub records: std::collections::BTreeSet<crate::NominalId>,
}

const C_KEYWORDS: &str = "alignas alignof and and_eq asm atomic_cancel atomic_commit atomic_noexcept auto bitand bitor bool break case catch char char8_t char16_t char32_t class compl concept const consteval constexpr constinit const_cast continue co_await co_return co_yield decltype default delete do double dynamic_cast else enum explicit export extern false float for friend goto if inline int long mutable namespace new noexcept not not_eq nullptr operator or or_eq private protected public register reinterpret_cast requires restrict return short signed sizeof static static_assert static_cast struct switch template this thread_local throw true try typedef typeid typename typeof typeof_unqual union unsigned using virtual void volatile wchar_t while xor xor_eq";

/// Whether this name is available to a Hew function at a C/C++ link boundary.
#[must_use]
pub fn is_c_export_identifier(symbol: &str) -> bool {
    symbol
        .as_bytes()
        .first()
        .is_some_and(u8::is_ascii_alphabetic)
        && symbol
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || byte == b'_')
        && !symbol.starts_with("HEW_")
        && ![
            "main",
            "HewText",
            "HewJson",
            "HewError",
            "HewRuntime",
            "HewWaker",
            "HewLiveCodeOwner",
            "HewLiveInitialize",
            "HewLiveFinalize",
        ]
        .contains(&symbol)
        && !C_KEYWORDS
            .split_ascii_whitespace()
            .any(|word| word == symbol)
        && !crate::jit_symbols::is_classified_hew_ffi_symbol(symbol)
}
