//! Type lowering for `hew build -g`: how each storage type is described to a
//! debugger.

use super::DebugEmitter;

use hew_mir::physical::{PhysicalDebugVariant, PhysicalTarget};
use hew_mir::{PhysicalLayout, PhysicalRepr};
use hew_types::ResolvedTy;
use inkwell::debug_info::{AsDIScope, DIFile, DIFlags, DIFlagsConstants, DIType, DebugInfoBuilder};
use inkwell::AddressSpace;

const DW_ATE_BOOLEAN: u32 = 0x02;
const DW_ATE_FLOAT: u32 = 0x04;
const DW_ATE_SIGNED: u32 = 0x05;
const DW_ATE_UNSIGNED: u32 = 0x07;
const DW_ATE_UNSIGNED_CHAR: u32 = 0x08;

impl<'ctx> DebugEmitter<'ctx> {
    /// The DIE for one storage type, memoized by its structural key.
    pub(super) fn resolve_type(
        &self,
        ty: &ResolvedTy,
        layout: &PhysicalLayout,
        target: &PhysicalTarget,
    ) -> Option<DIType<'ctx>> {
        let key = format!("{ty:?}|{:?}", layout.repr);
        if let Some(cached) = self.types.borrow().get(&key) {
            return Some(*cached);
        }
        let resolved = self.build_type(ty, layout, target)?;
        self.types.borrow_mut().insert(key, resolved);
        Some(resolved)
    }

    fn build_type(
        &self,
        ty: &ResolvedTy,
        layout: &PhysicalLayout,
        target: &PhysicalTarget,
    ) -> Option<DIType<'ctx>> {
        let bits = layout.size.checked_mul(8)?;
        if let Some((name, encoding)) = named_scalar(ty) {
            return self
                .builder
                .create_basic_type(name, bits, encoding, DIFlags::ZERO)
                .ok()
                .map(|basic| basic.as_type());
        }
        // An enum's own storage is a `{ tag, payload }` carrier, but describing
        // it that way hands a debugger every variant's bytes at once. The
        // variant part names the tag as the selector so only the active case
        // renders. An indirect enum's local is a pointer and falls through.
        //
        // CodeView has no variant-part record. LLVM lowers the part to an
        // unnamed `LF_NESTTYPE` it cannot resolve, so a `.debug$T` consumer
        // sees `Status` with no members at all - strictly less than the tag
        // and payload it can read from the carrier. Describe the carrier on
        // those targets and leave the variant part to DWARF.
        if !self.codeview {
            if let Some(variants) = self.enums.get(ty) {
                if let Some(described) = self.build_enum(ty, layout, variants, target) {
                    return Some(described);
                }
            }
        }
        match &layout.repr {
            PhysicalRepr::Unit => None,
            PhysicalRepr::Integer { bits: width } => self
                .builder
                .create_basic_type(
                    &format!("u{width}"),
                    u64::from(*width),
                    DW_ATE_UNSIGNED,
                    DIFlags::ZERO,
                )
                .ok()
                .map(|basic| basic.as_type()),
            PhysicalRepr::Float { bits: width } => self
                .builder
                .create_basic_type(
                    &format!("f{width}"),
                    u64::from(*width),
                    DW_ATE_FLOAT,
                    DIFlags::ZERO,
                )
                .ok()
                .map(|basic| basic.as_type()),
            PhysicalRepr::Pointer if *ty == ResolvedTy::String => self.build_string(layout),
            // Other reference handles remain opaque to the debugger.
            PhysicalRepr::Pointer => {
                let byte = self
                    .builder
                    .create_basic_type("u8", 8, DW_ATE_UNSIGNED_CHAR, DIFlags::ZERO)
                    .unwrap_or_else(|_| unreachable!("a basic type named `u8` is never empty"));
                Some(
                    self.builder
                        .create_pointer_type(
                            "",
                            byte.as_type(),
                            bits,
                            layout.align * 8,
                            AddressSpace::default(),
                        )
                        .as_type(),
                )
            }
            PhysicalRepr::Array { element, len } | PhysicalRepr::Vector { element, len } => {
                let inner = self.build_type(&ResolvedTy::Unit, element, target)?;
                Some(
                    self.builder
                        .create_array_type(
                            inner,
                            bits,
                            layout.align * 8,
                            &[std::ops::Range {
                                start: 0,
                                end: i64::from(*len),
                            }],
                        )
                        .as_type(),
                )
            }
            PhysicalRepr::Struct(fields) => self.build_struct(ty, layout, fields, target),
        }
    }

    /// Describe the canonical managed allocation without turning its handle
    /// into a C string. Debugger summaries read the length and trailing bytes.
    fn build_string(&self, layout: &PhysicalLayout) -> Option<DIType<'ctx>> {
        let header_size = hew_cabi::string::string_debug_header_size(layout.size)?;
        let length = self
            .builder
            .create_basic_type("usize", layout.size * 8, DW_ATE_UNSIGNED, DIFlags::ZERO)
            .unwrap_or_else(|_| unreachable!("a basic type named `usize` is never empty"))
            .as_type();
        let byte = self
            .builder
            .create_basic_type("u8", 8, DW_ATE_UNSIGNED_CHAR, DIFlags::ZERO)
            .unwrap_or_else(|_| unreachable!("a basic type named `u8` is never empty"))
            .as_type();
        let data = self
            .builder
            .create_array_type(byte, 0, 8, &[std::ops::Range { start: 0, end: 0 }])
            .as_type();
        let members = [
            self.builder
                .create_member_type(
                    self.file.as_debug_info_scope(),
                    "byte_len",
                    self.file,
                    0,
                    layout.size * 8,
                    layout.align * 8,
                    hew_cabi::string::STRING_DEBUG_BYTE_LEN_OFFSET as u64 * 8,
                    DIFlags::ZERO,
                    length,
                )
                .as_type(),
            self.builder
                .create_member_type(
                    self.file.as_debug_info_scope(),
                    "data",
                    self.file,
                    0,
                    0,
                    8,
                    header_size * 8,
                    DIFlags::ZERO,
                    data,
                )
                .as_type(),
        ];
        let header = self.builder.create_struct_type(
            self.file.as_debug_info_scope(),
            "hew_string",
            self.file,
            0,
            header_size * 8,
            layout.align * 8,
            DIFlags::ZERO,
            None,
            &members,
            0,
            None,
            "hew_string",
        );
        Some(
            self.builder
                .create_pointer_type(
                    "",
                    header.as_type(),
                    layout.size * 8,
                    layout.align * 8,
                    AddressSpace::default(),
                )
                .as_type(),
        )
    }

    fn build_struct(
        &self,
        ty: &ResolvedTy,
        layout: &PhysicalLayout,
        fields: &[PhysicalLayout],
        target: &PhysicalTarget,
    ) -> Option<DIType<'ctx>> {
        // A declared record names its members from source. Every other struct
        // here is a carrier this stage synthesized - a tuple, a closure
        // environment, an enum's tag-and-payload pair - and has no source name
        // to give, so it keeps positional members.
        let named = self
            .records
            .get(ty)
            .filter(|rows| rows.len() == fields.len());
        let mut offset = 0u64;
        let mut members = Vec::with_capacity(fields.len());
        for (index, field) in fields.iter().enumerate() {
            let align = u64::from(field.align).max(1);
            offset = offset.div_ceil(align) * align;
            let (name, member_ty) = match named {
                Some(rows) => (
                    rows[index].name.clone(),
                    self.resolve_type(&rows[index].ty, field, target)?,
                ),
                None => (
                    if *ty == ResolvedTy::Bytes {
                        ["ptr", "offset", "len"].get(index)?.to_string()
                    } else {
                        format!("f{index}")
                    },
                    self.build_type(&ResolvedTy::Unit, field, target)?,
                ),
            };
            members.push(
                self.builder
                    .create_member_type(
                        self.file.as_debug_info_scope(),
                        &name,
                        self.file,
                        0,
                        field.size.checked_mul(8)?,
                        field.align * 8,
                        offset * 8,
                        DIFlags::ZERO,
                        member_ty,
                    )
                    .as_type(),
            );
            offset += field.size;
        }
        Some(
            self.builder
                .create_struct_type(
                    self.file.as_debug_info_scope(),
                    &type_name(ty),
                    self.file,
                    0,
                    layout.size.checked_mul(8)?,
                    layout.align * 8,
                    DIFlags::ZERO,
                    None,
                    &members,
                    0,
                    None,
                    &type_name(ty),
                )
                .as_type(),
        )
    }

    /// Describe one direct enum as a structure whose only element is a
    /// `DW_TAG_variant_part`: the tag becomes the part's discriminant and each
    /// case a `DW_TAG_variant` guarded by its tag value, so a debugger reads
    /// only the active payload.
    ///
    /// Returns `None` for an indirect enum, whose local holds a pointer the
    /// caller describes instead.
    fn build_enum(
        &self,
        ty: &ResolvedTy,
        layout: &PhysicalLayout,
        variants: &[PhysicalDebugVariant],
        target: &PhysicalTarget,
    ) -> Option<DIType<'ctx>> {
        let PhysicalRepr::Struct(carrier) = &layout.repr else {
            return None;
        };
        let [tag, payload] = carrier.as_slice() else {
            return None;
        };
        let cases = target.variant_layout(ty)?;
        if cases.is_indirect || cases.variants.len() != variants.len() {
            return None;
        }
        let bits = layout.size.checked_mul(8)?;
        let align_bits = layout.align * 8;
        let tag_bits = tag.size.checked_mul(8)?;
        let tag_align = u64::from(payload.align).max(1);
        let payload_offset_bits = tag.size.div_ceil(tag_align) * tag_align * 8;
        let Ok(tag_width) = u32::try_from(tag_bits) else {
            return None;
        };
        let Ok(tag_int) = self
            .ctx
            .custom_width_int_type(std::num::NonZeroU32::new(tag_width)?)
        else {
            return None;
        };

        let enumerators: Vec<_> = variants
            .iter()
            .enumerate()
            .map(|(index, variant)| {
                self.builder.create_enumerator(
                    &variant.name,
                    i64::try_from(index).unwrap_or(i64::MAX),
                    true,
                )
            })
            .collect();
        let Ok(underlying) = self.builder.create_basic_type(
            &format!("u{tag_bits}"),
            tag_bits,
            DW_ATE_UNSIGNED,
            DIFlags::ZERO,
        ) else {
            return None;
        };
        let underlying = underlying.as_type();
        let tag_di = self
            .builder
            .create_enumeration_type(
                self.file.as_debug_info_scope(),
                &format!("{}.Tag", type_name(ty)),
                self.file,
                0,
                tag_bits,
                tag.align * 8,
                &enumerators,
                underlying,
            )
            .as_type();
        // Unnamed and artificial: this member exists to be pointed at by
        // `DW_AT_discr`, not to be printed as a field of the enum.
        let discriminator = self.builder.create_member_type(
            self.file.as_debug_info_scope(),
            "",
            self.file,
            0,
            tag_bits,
            tag.align * 8,
            0,
            DIFlags::ARTIFICIAL,
            tag_di,
        );

        let mut members = Vec::with_capacity(variants.len());
        for (index, variant) in variants.iter().enumerate() {
            let PhysicalRepr::Struct(field_layouts) = &cases.variants[index].repr else {
                return None;
            };
            if field_layouts.len() != variant.fields.len() {
                return None;
            }
            // A variant's payload members sit at their offset within the whole
            // enum, so the case struct spans the enum and a debugger reads the
            // payload where the tag left it.
            let mut offset = 0u64;
            let mut fields = Vec::with_capacity(field_layouts.len());
            for (field, field_layout) in variant.fields.iter().zip(field_layouts) {
                let align = u64::from(field_layout.align).max(1);
                offset = offset.div_ceil(align) * align;
                let member_ty = self.resolve_type(&field.ty, field_layout, target)?;
                fields.push(
                    self.builder
                        .create_member_type(
                            self.file.as_debug_info_scope(),
                            &field.name,
                            self.file,
                            0,
                            field_layout.size.checked_mul(8)?,
                            field_layout.align * 8,
                            payload_offset_bits + offset * 8,
                            DIFlags::ZERO,
                            member_ty,
                        )
                        .as_type(),
                );
                offset += field_layout.size;
            }
            let unique = format!("{}.{}", type_name(ty), variant.name);
            let case = self
                .builder
                .create_struct_type(
                    self.file.as_debug_info_scope(),
                    &variant.name,
                    self.file,
                    0,
                    bits,
                    align_bits,
                    DIFlags::ZERO,
                    None,
                    &fields,
                    0,
                    None,
                    &unique,
                )
                .as_type();
            let Ok(tag_value) = u64::try_from(index) else {
                return None;
            };
            let discriminant = tag_int.const_int(tag_value, false);
            members.push(create_variant_member(
                &self.builder,
                self.file,
                &variant.name,
                bits,
                align_bits,
                discriminant,
                case,
            )?);
        }

        // Build the part before the placeholder: a placeholder left inside a
        // composite with nothing to replace it is a dangling temporary at
        // `finalize`.
        let part = create_variant_part(
            &self.builder,
            self.file,
            bits,
            align_bits,
            discriminator,
            &members,
        )?;
        // SAFETY: the placeholder is replaced below, before `finalize` runs.
        let placeholder = unsafe { self.builder.create_placeholder_derived_type(self.ctx) };
        let composite = self.builder.create_struct_type(
            self.file.as_debug_info_scope(),
            &type_name(ty),
            self.file,
            0,
            bits,
            align_bits,
            DIFlags::ZERO,
            None,
            &[placeholder.as_type()],
            0,
            None,
            &type_name(ty),
        );
        // SAFETY: both nodes belong to this builder's context, and the call
        // deletes the temporary once its uses point at the variant part.
        unsafe {
            inkwell::llvm_sys::debuginfo::LLVMMetadataReplaceAllUsesWith(
                placeholder.as_type().as_mut_ptr(),
                part,
            );
        }
        Some(composite.as_type())
    }
}

/// One `DW_TAG_variant`, guarded by the tag value that selects it.
///
/// LLVM's C API has no entry point for `DIBuilder::createVariantMemberType`;
/// `physical_debug_shim.cpp` exposes exactly this one.
fn create_variant_member<'ctx>(
    builder: &DebugInfoBuilder<'ctx>,
    file: DIFile<'ctx>,
    name: &str,
    size_in_bits: u64,
    align_in_bits: u32,
    discriminant: inkwell::values::IntValue<'ctx>,
    ty: DIType<'ctx>,
) -> Option<inkwell::llvm_sys::prelude::LLVMMetadataRef> {
    use inkwell::values::AsValueRef;

    unsafe extern "C" {
        fn hewLLVMDIBuilderCreateVariantMemberType(
            builder: inkwell::llvm_sys::prelude::LLVMDIBuilderRef,
            scope: inkwell::llvm_sys::prelude::LLVMMetadataRef,
            name: *const std::ffi::c_char,
            name_len: usize,
            file: inkwell::llvm_sys::prelude::LLVMMetadataRef,
            size_in_bits: u64,
            align_in_bits: u32,
            offset_in_bits: u64,
            discriminant: inkwell::llvm_sys::prelude::LLVMValueRef,
            ty: inkwell::llvm_sys::prelude::LLVMMetadataRef,
        ) -> inkwell::llvm_sys::prelude::LLVMMetadataRef;
    }

    // SAFETY: every wrapper belongs to this builder's live context, and the
    // shim borrows `name` only for the duration of the call.
    let metadata = unsafe {
        hewLLVMDIBuilderCreateVariantMemberType(
            builder.as_mut_ptr(),
            file.as_debug_info_scope().as_mut_ptr(),
            name.as_ptr().cast(),
            name.len(),
            file.as_mut_ptr(),
            size_in_bits,
            align_in_bits,
            0,
            discriminant.as_value_ref(),
            ty.as_mut_ptr(),
        )
    };
    (!metadata.is_null()).then_some(metadata)
}

/// The `DW_TAG_variant_part` holding every case, with `discriminator` as its
/// selector. The second entry point LLVM's C API does not provide.
fn create_variant_part<'ctx>(
    builder: &DebugInfoBuilder<'ctx>,
    file: DIFile<'ctx>,
    size_in_bits: u64,
    align_in_bits: u32,
    discriminator: inkwell::debug_info::DIDerivedType<'ctx>,
    elements: &[inkwell::llvm_sys::prelude::LLVMMetadataRef],
) -> Option<inkwell::llvm_sys::prelude::LLVMMetadataRef> {
    unsafe extern "C" {
        fn hewLLVMDIBuilderCreateVariantPart(
            builder: inkwell::llvm_sys::prelude::LLVMDIBuilderRef,
            scope: inkwell::llvm_sys::prelude::LLVMMetadataRef,
            file: inkwell::llvm_sys::prelude::LLVMMetadataRef,
            size_in_bits: u64,
            align_in_bits: u32,
            discriminator: inkwell::llvm_sys::prelude::LLVMMetadataRef,
            elements: *const inkwell::llvm_sys::prelude::LLVMMetadataRef,
            element_count: u32,
        ) -> inkwell::llvm_sys::prelude::LLVMMetadataRef;
    }

    let Ok(count) = u32::try_from(elements.len()) else {
        return None;
    };
    // SAFETY: every metadata node belongs to this builder's context, and LLVM
    // copies the pointer slice into an MDTuple during the call.
    let metadata = unsafe {
        hewLLVMDIBuilderCreateVariantPart(
            builder.as_mut_ptr(),
            file.as_debug_info_scope().as_mut_ptr(),
            file.as_mut_ptr(),
            size_in_bits,
            align_in_bits,
            discriminator.as_type().as_mut_ptr(),
            elements.as_ptr(),
            count,
        )
    };
    (!metadata.is_null()).then_some(metadata)
}

/// The source spelling and DWARF encoding of a scalar Hew type.
fn named_scalar(ty: &ResolvedTy) -> Option<(&'static str, u32)> {
    Some(match ty {
        ResolvedTy::I8 => ("i8", DW_ATE_SIGNED),
        ResolvedTy::I16 => ("i16", DW_ATE_SIGNED),
        ResolvedTy::I32 => ("i32", DW_ATE_SIGNED),
        ResolvedTy::I64 => ("i64", DW_ATE_SIGNED),
        ResolvedTy::Isize => ("isize", DW_ATE_SIGNED),
        ResolvedTy::U8 => ("u8", DW_ATE_UNSIGNED),
        ResolvedTy::U16 => ("u16", DW_ATE_UNSIGNED),
        ResolvedTy::U32 => ("u32", DW_ATE_UNSIGNED),
        ResolvedTy::U64 => ("u64", DW_ATE_UNSIGNED),
        ResolvedTy::Usize => ("usize", DW_ATE_UNSIGNED),
        ResolvedTy::F32 => ("f32", DW_ATE_FLOAT),
        ResolvedTy::F64 => ("f64", DW_ATE_FLOAT),
        ResolvedTy::Bool => ("bool", DW_ATE_BOOLEAN),
        ResolvedTy::Char => ("char", DW_ATE_UNSIGNED_CHAR),
        ResolvedTy::Duration => ("duration", DW_ATE_SIGNED),
        _ => return None,
    })
}

fn type_name(ty: &ResolvedTy) -> String {
    match ty {
        ResolvedTy::Named { head, .. } => head.registry_key().to_string(),
        ResolvedTy::Tuple(_) => "tuple".to_owned(),
        ResolvedTy::Bytes => "bytes".to_owned(),
        other => format!("{other:?}"),
    }
}
