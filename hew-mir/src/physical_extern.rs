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
    /// homogeneous floating aggregates. The WebAssembly C ABI returns a
    /// single-element aggregate as that element and everything else through
    /// `sret`. Larger ordinary aggregates use `sret`.
    /// Bytes is the ordinary `{ptr, u32, u32}` aggregate under the same rules.
    ///
    /// # Errors
    /// Refuses aggregate results whose target ABI is not implemented.
    pub fn extern_result_abi(
        &self,
        result: &ResolvedTy,
    ) -> Result<PhysicalExternResultAbi, PhysicalError> {
        // C `_Bool` uses an i1 register carrier; Hew value storage is a byte.
        if *result == ResolvedTy::Bool {
            return Ok(PhysicalExternResultAbi::Coerce(PhysicalRepr::Integer {
                bits: 1,
            }));
        }
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
            // The WebAssembly C ABI (clang's `WebAssemblyABIInfo`, matched by
            // rustc's wasm call convention): an aggregate whose only non-empty
            // member is a scalar returns as that scalar; every other aggregate
            // returns through `sret`.
            "wasm32" | "wasm64" => single_element(layout)
                .map_or(Ok(PhysicalExternResultAbi::Indirect), |element| {
                    Ok(PhysicalExternResultAbi::Coerce(element.repr.clone()))
                }),
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
/// The one scalar an aggregate wraps, or `None` when it wraps none or several.
///
/// Padding disqualifies the aggregate: the wrapper and the scalar must occupy
/// the same bytes, or returning the scalar would drop some of them.
fn single_element(layout: &PhysicalLayout) -> Option<&PhysicalLayout> {
    match &layout.repr {
        PhysicalRepr::Integer { .. } | PhysicalRepr::Float { .. } | PhysicalRepr::Pointer => {
            Some(layout)
        }
        PhysicalRepr::Array { element, len: 1 } => {
            (element.size == layout.size).then(|| single_element(element))?
        }
        PhysicalRepr::Struct(fields) => {
            let mut occupied = fields.iter().filter(|field| field.size != 0);
            let only = occupied.next()?;
            if occupied.next().is_some() || only.size != layout.size {
                return None;
            }
            single_element(only)
        }
        _ => None,
    }
}

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

/// One source parameter's register or memory presentation at the C boundary.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CParameterAbi {
    /// The pre-existing scalar, pointer or runtime adapter.
    Direct,
    /// One register carrier overlays the record's storage.
    Coerce {
        repr: PhysicalRepr,
        stack_align: Option<u32>,
    },
    /// System V assigns each occupied eightbyte an individual parameter.
    Expand(Vec<PhysicalLayout>),
    /// The caller supplies a private copy. Some targets pass that copy on the stack.
    Indirect { byval_align: Option<u32> },
}

impl PhysicalTarget {
    /// Classify C parameters in order, accounting for the hidden result pointer
    /// and the System V rule that a whole record spills when either register
    /// class lacks room. Only explicit C records change legacy runtime adapters.
    ///
    /// # Errors
    /// An explicit record requires a measured layout and a supported C ABI.
    pub fn c_parameters<'a>(
        &self,
        types: impl IntoIterator<Item = &'a ResolvedTy>,
        records: &std::collections::BTreeSet<hew_types::NominalId>,
        result: &PhysicalExternResultAbi,
    ) -> Result<Vec<CParameterAbi>, PhysicalError> {
        let arch = self.triple.split('-').next().unwrap_or_default();
        let sysv = arch == "x86_64"
            && !self
                .triple
                .split('-')
                .any(|p| matches!(p, "windows" | "win32" | "mingw32" | "cygwin" | "uefi"));
        let aapcs = matches!(arch, "aarch64" | "aarch64_be" | "arm64" | "arm64e");
        let mut registers = SysvRegisters {
            integers: 6 - usize::from(matches!(result, PhysicalExternResultAbi::Indirect)),
            floats: 8,
        };
        let mut parameters = Vec::new();
        for ty in types {
            let marked = matches!(ty, ResolvedTy::Named { head: hew_types::TypeHead::Nominal(head), .. } if records.contains(&head.id));
            if !marked {
                if sysv {
                    if matches!(ty, ResolvedTy::F32 | ResolvedTy::F64) {
                        registers.floats = registers.floats.saturating_sub(1);
                    } else {
                        registers.integers = registers.integers.saturating_sub(1);
                    }
                }
                parameters.push(CParameterAbi::Direct);
                continue;
            }
            let layout = self
                .layout(ty)
                .ok_or_else(|| PhysicalError::new("C record parameter lacks a target layout"))?;
            let byval_align = if sysv {
                Some(layout.align.max(8))
            } else if arch.starts_with("wasm") {
                Some(layout.align)
            } else {
                None
            };
            let parameter = match self.extern_result_abi(ty)? {
                PhysicalExternResultAbi::Indirect => CParameterAbi::Indirect { byval_align },
                PhysicalExternResultAbi::Coerce(repr) if sysv => {
                    registers.record(repr, byval_align)
                }
                PhysicalExternResultAbi::Coerce(_) if aapcs && layout.size <= 8 => {
                    // AAPCS64 rounds a non-homogeneous composite argument up to a word.
                    CParameterAbi::Coerce {
                        repr: self.integer_carrier(8)?.repr,
                        stack_align: None,
                    }
                }
                PhysicalExternResultAbi::Coerce(repr) => CParameterAbi::Coerce {
                    repr,
                    stack_align: None,
                },
                PhysicalExternResultAbi::Direct => {
                    let (bits, len) = homogeneous_float(layout)
                        .ok_or_else(|| PhysicalError::new("expected a homogeneous C record"))?;
                    let scalar = if bits == 32 {
                        ResolvedTy::F32
                    } else {
                        ResolvedTy::F64
                    };
                    let element = self.layout(&scalar).ok_or_else(|| {
                        PhysicalError::new("C floating carrier lacks its target layout")
                    })?;
                    CParameterAbi::Coerce {
                        repr: PhysicalRepr::Array {
                            element: Box::new(element.clone()),
                            len,
                        },
                        // The base PCS rounds a spilled HFA up to a stack word.
                        // Darwin and Windows retain its natural alignment.
                        stack_align: (!self.triple.contains("apple")
                            && !self.triple.contains("windows"))
                        .then_some(8),
                    }
                }
            };
            parameters.push(parameter);
        }
        Ok(parameters)
    }
}

/// Remaining System V argument registers at this point in the signature.
struct SysvRegisters {
    integers: usize,
    floats: usize,
}

impl SysvRegisters {
    fn record(&mut self, repr: PhysicalRepr, byval_align: Option<u32>) -> CParameterAbi {
        let floating = |repr: &PhysicalRepr| {
            matches!(
                repr,
                PhysicalRepr::Float { .. } | PhysicalRepr::Vector { .. }
            )
        };
        let (integers, floats) = match &repr {
            PhysicalRepr::Struct(parts) => {
                let floats = parts.iter().filter(|p| floating(&p.repr)).count();
                (parts.len() - floats, floats)
            }
            repr if floating(repr) => (0, 1),
            _ => (1, 0),
        };
        if integers > self.integers || floats > self.floats {
            return CParameterAbi::Indirect { byval_align };
        }
        self.integers -= integers;
        self.floats -= floats;
        match repr {
            PhysicalRepr::Struct(parts) => CParameterAbi::Expand(parts),
            repr => CParameterAbi::Coerce {
                repr,
                stack_align: None,
            },
        }
    }
}
