//! Target classification of declared C results, separate from value storage.

use super::{PhysicalError, PhysicalLayout, PhysicalRepr, PhysicalTarget, ResolvedTy};

/// How a declared C result initializes its ordinary physical result storage.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PhysicalExternResultAbi {
    /// Scalars, pointers and homogeneous floating aggregates use their storage representation.
    Direct,
    /// The result arrives in this register carrier and is reinterpreted in memory.
    Coerce(PhysicalRepr),
    /// The caller supplies result storage as the first `sret` parameter.
    Indirect,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum RegisterClass {
    Integer,
    Sse,
}

#[derive(Default)]
struct Eightbyte {
    class: Option<RegisterClass>,
    used: u16,
    pointer: Option<PhysicalLayout>,
    wide_float: bool,
}

impl PhysicalTarget {
    /// Classify a declared C result from its target-measured field layouts.
    ///
    /// System V x64 merges fields into integer/SSE eightbytes; Win64 returns
    /// only 1/2/4/8-byte records directly; AAPCS64 additionally recognizes
    /// homogeneous floating aggregates. Larger ordinary aggregates use `sret`.
    /// Bytes is the ordinary `{ptr, u32, u32}` aggregate under the same rules.
    ///
    /// # Errors
    /// Refuses aggregate results whose target ABI is not implemented.
    pub fn extern_result_abi(
        &self,
        result: &ResolvedTy,
    ) -> Result<PhysicalExternResultAbi, PhysicalError> {
        if *result == ResolvedTy::Never {
            return Ok(PhysicalExternResultAbi::Direct);
        }
        let layout = self.layout(result).ok_or_else(|| {
            PhysicalError::new(format!("C result has no target layout: {result:?}"))
        })?;
        if !matches!(
            layout.repr,
            PhysicalRepr::Struct(_) | PhysicalRepr::Array { .. }
        ) || layout.size == 0
        {
            return Ok(PhysicalExternResultAbi::Direct);
        }
        let arch = self.triple.split('-').next().unwrap_or_default();
        let windows = self
            .triple
            .split('-')
            .any(|part| matches!(part, "windows" | "win32" | "mingw32" | "cygwin" | "uefi"));
        match arch {
            "x86_64" if windows => {
                if matches!(layout.size, 1 | 2 | 4 | 8) {
                    Ok(PhysicalExternResultAbi::Coerce(
                        self.integer_carrier(layout.size)?.repr,
                    ))
                } else {
                    Ok(PhysicalExternResultAbi::Indirect)
                }
            }
            "x86_64" if !self.triple.contains("gnux32") => self.sysv_result(layout),
            "aarch64" | "aarch64_be" | "arm64" | "arm64e" => {
                if homogeneous_float(layout).is_some() {
                    return Ok(PhysicalExternResultAbi::Direct);
                }
                if layout.size > 16 {
                    return Ok(PhysicalExternResultAbi::Indirect);
                }
                let repr = if layout.size <= 8 {
                    // Big-endian AAPCS64 left-aligns a small aggregate in x0.
                    self.integer_carrier(if self.data_layout.starts_with('E') {
                        8
                    } else {
                        layout.size
                    })?
                    .repr
                } else {
                    PhysicalRepr::Array {
                        element: Box::new(self.integer_carrier(8)?),
                        len: 2,
                    }
                };
                Ok(PhysicalExternResultAbi::Coerce(repr))
            }
            _ => Err(PhysicalError::new(format!(
                "C aggregate result ABI is not realized for target `{}`",
                self.triple
            ))),
        }
    }

    fn integer_carrier(&self, bytes: u64) -> Result<PhysicalLayout, PhysicalError> {
        let ty = match bytes {
            1 => ResolvedTy::U8,
            2 => ResolvedTy::U16,
            3..=4 => ResolvedTy::U32,
            5..=8 => ResolvedTy::U64,
            _ => {
                return Err(PhysicalError::new(
                    "C integer register carrier exceeds eight bytes",
                ));
            }
        };
        let mut layout = self.layout(&ty).cloned().ok_or_else(|| {
            PhysicalError::new("C integer register carrier lacks its target layout")
        })?;
        layout.repr = PhysicalRepr::Integer {
            bits: u16::try_from(bytes * 8).expect("at most one eightbyte"),
        };
        Ok(layout)
    }

    fn sysv_result(
        &self,
        layout: &PhysicalLayout,
    ) -> Result<PhysicalExternResultAbi, PhysicalError> {
        if layout.size > 16 {
            return Ok(PhysicalExternResultAbi::Indirect);
        }
        let mut chunks: [Eightbyte; 2] = Default::default();
        classify_eightbytes(layout, 0, &mut chunks)?;
        let mut registers = Vec::new();
        for chunk in chunks.into_iter().filter(|chunk| chunk.class.is_some()) {
            registers.push(match chunk.class {
                Some(RegisterClass::Integer) => match chunk.pointer {
                    Some(pointer) => pointer,
                    None => self.integer_carrier(u64::from(chunk.used))?,
                },
                Some(RegisterClass::Sse) => {
                    let ty = if chunk.used <= 4 {
                        ResolvedTy::F32
                    } else {
                        ResolvedTy::F64
                    };
                    let mut carrier = self.layout(&ty).cloned().ok_or_else(|| {
                        PhysicalError::new("C SSE register carrier lacks its target layout")
                    })?;
                    if chunk.used > 4 && !chunk.wide_float {
                        let element = self.layout(&ResolvedTy::F32).cloned().ok_or_else(|| {
                            PhysicalError::new("C SSE register carrier lacks its f32 layout")
                        })?;
                        carrier.repr = PhysicalRepr::Vector {
                            element: Box::new(element),
                            len: 2,
                        };
                    }
                    carrier
                }
                None => unreachable!("empty eightbytes were filtered"),
            });
        }
        Ok(PhysicalExternResultAbi::Coerce(if registers.len() == 1 {
            registers.remove(0).repr
        } else {
            PhysicalRepr::Struct(registers)
        }))
    }
}

/// Classify leaves at their measured natural offsets, merging INTEGER over SSE.
fn classify_eightbytes(
    layout: &PhysicalLayout,
    offset: u64,
    chunks: &mut [Eightbyte; 2],
) -> Result<(), PhysicalError> {
    match &layout.repr {
        PhysicalRepr::Struct(fields) => {
            let mut cursor = offset;
            for field in fields {
                let align = u64::from(field.align);
                cursor = cursor.div_ceil(align) * align;
                classify_eightbytes(field, cursor, chunks)?;
                cursor += field.size;
            }
        }
        PhysicalRepr::Array { element, len } => {
            for index in 0..*len {
                classify_eightbytes(element, offset + u64::from(index) * element.size, chunks)?;
            }
        }
        PhysicalRepr::Unit => {}
        _ if layout.size == 0 => {}
        repr => {
            let class = if matches!(repr, PhysicalRepr::Float { .. }) {
                RegisterClass::Sse
            } else {
                RegisterClass::Integer
            };
            let end = offset + layout.size;
            for index in offset / 8..=(end - 1) / 8 {
                let chunk_index = usize::try_from(index).map_err(|_| {
                    PhysicalError::new("C aggregate register index exceeds addressable size")
                })?;
                let chunk = chunks.get_mut(chunk_index).ok_or_else(|| {
                    PhysicalError::new("C aggregate leaf exceeds its register classification")
                })?;
                chunk.wide_float |= *repr == (PhysicalRepr::Float { bits: 64 });
                chunk.pointer = if chunk.class.is_none()
                    && offset == index * 8
                    && *repr == PhysicalRepr::Pointer
                    && layout.size == 8
                {
                    Some(layout.clone())
                } else {
                    None
                };
                chunk.class = Some(if chunk.class == Some(RegisterClass::Integer) {
                    RegisterClass::Integer
                } else {
                    class
                });
                chunk.used = chunk.used.max(
                    u16::try_from(end.min((index + 1) * 8) - index * 8).expect("one eightbyte"),
                );
            }
        }
    }
    Ok(())
}

/// AAPCS64 homogeneous aggregates contain one to four identical floating leaves.
fn homogeneous_float(layout: &PhysicalLayout) -> Option<(u16, u32)> {
    match &layout.repr {
        PhysicalRepr::Float {
            bits: bits @ (32 | 64),
        } => Some((*bits, 1)),
        PhysicalRepr::Array { element, len } => {
            let (bits, count) = homogeneous_float(element)?;
            let count = count.checked_mul(*len)?;
            (1..=4).contains(&count).then_some((bits, count))
        }
        PhysicalRepr::Struct(fields) => {
            let mut result = None;
            for field in fields.iter().filter(|field| field.size != 0) {
                let (bits, count) = homogeneous_float(field)?;
                let (prior_bits, prior_count) = result.unwrap_or((bits, 0));
                if prior_bits != bits || prior_count + count > 4 {
                    return None;
                }
                result = Some((bits, prior_count + count));
            }
            result
        }
        _ => None,
    }
}
