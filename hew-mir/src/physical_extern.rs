//! Target classification of declared C results, separate from value storage.

use super::{PhysicalError, PhysicalRepr, PhysicalTarget, ResolvedTy};

/// How a declared C result initializes its ordinary physical result storage.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PhysicalExternResultAbi {
    /// Scalars, pointers and void use their ordinary storage representation.
    Direct,
    /// The C ABI returns the byte triple in this register carrier. Its memory
    /// representation starts with the complete byte triple, including offset.
    BytesCoerce(PhysicalRepr),
    /// The caller supplies byte-triple storage as the first `sret` parameter.
    BytesIndirect,
}

impl PhysicalTarget {
    /// Classify a C result using this target's measured scalar layouts.
    ///
    /// The runtime's `BytesTriple` is `{ptr, u32, u32}` in memory. System V x64
    /// returns its two integer eightbytes, AAPCS64 returns two general-purpose
    /// registers, and Win64 passes a hidden result pointer. LLVM aggregate
    /// storage types alone do not express these C return conventions.
    ///
    /// # Errors
    /// Refuses a byte result when its layout or target ABI is not realized.
    pub fn extern_result_abi(
        &self,
        result: &ResolvedTy,
    ) -> Result<PhysicalExternResultAbi, PhysicalError> {
        if *result != ResolvedTy::Bytes {
            return Ok(PhysicalExternResultAbi::Direct);
        }
        let unsupported = || {
            PhysicalError::new(format!(
                "C byte-triple result ABI is not realized for target `{}`",
                self.triple
            ))
        };
        let layout = self.layout(result).ok_or_else(unsupported)?;
        let PhysicalRepr::Struct(fields) = &layout.repr else {
            return Err(unsupported());
        };
        let [pointer, offset, len] = fields.as_slice() else {
            return Err(unsupported());
        };
        if pointer.repr != PhysicalRepr::Pointer
            || offset.repr != (PhysicalRepr::Integer { bits: 32 })
            || len != offset
        {
            return Err(unsupported());
        }
        let arch = self.triple.split('-').next().ok_or_else(unsupported)?;
        let windows = self
            .triple
            .split('-')
            .any(|part| matches!(part, "windows" | "win32" | "mingw32" | "cygwin" | "uefi"));
        let word = || {
            self.layout(&ResolvedTy::U64)
                .cloned()
                .ok_or_else(unsupported)
        };
        Ok(match (arch, pointer.size) {
            ("x86_64", 8) if windows => PhysicalExternResultAbi::BytesIndirect,
            ("x86_64", 8) => PhysicalExternResultAbi::BytesCoerce(PhysicalRepr::Struct(vec![
                pointer.clone(),
                word()?,
            ])),
            ("aarch64" | "aarch64_be" | "arm64" | "arm64e", 8) => {
                PhysicalExternResultAbi::BytesCoerce(PhysicalRepr::Array {
                    element: Box::new(word()?),
                    len: 2,
                })
            }
            _ => return Err(unsupported()),
        })
    }
}
