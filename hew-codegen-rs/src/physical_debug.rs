//! DWARF/CodeView metadata for `hew build -g`.
//!
//! Every name, span and lexical scope here is a physical MIR fact projected
//! from SIR ([`hew_mir::physical::PhysicalDebug`]). This module decides only
//! how those facts are spelled as LLVM debug metadata.

use std::cell::RefCell;
use std::collections::HashMap;
use std::path::Path;

use hew_mir::physical::SemDebugScope;
use hew_mir::physical::{
    PhysicalDebug, PhysicalDebugField, PhysicalDebugFunction, PhysicalDebugLocal,
    PhysicalDebugVariant, PhysicalTarget,
};
use hew_mir::{PhysicalFunction, PhysicalLayout, PhysicalRepr};
use hew_types::ResolvedTy;
use inkwell::context::Context;
use inkwell::debug_info::{
    AsDIScope, DICompileUnit, DIFile, DIFlags, DIFlagsConstants, DILexicalBlock, DILocalVariable,
    DILocation, DIScope, DISubprogram, DIType, DWARFEmissionKind, DWARFSourceLanguage,
    DebugInfoBuilder,
};
use inkwell::module::{FlagBehavior, Module};
use inkwell::values::{FunctionValue, PointerValue};
use inkwell::AddressSpace;

const DW_ATE_BOOLEAN: u32 = 0x02;
const DW_ATE_FLOAT: u32 = 0x04;
const DW_ATE_SIGNED: u32 = 0x05;
const DW_ATE_UNSIGNED: u32 = 0x07;
const DW_ATE_UNSIGNED_CHAR: u32 = 0x08;

/// The source text a `-g` build attributes its metadata to.
#[derive(Debug, Clone, Copy)]
pub struct DebugSource<'a> {
    pub path: &'a Path,
    pub text: &'a str,
}

/// Byte offset → one-based line and column.
struct LineIndex {
    starts: Vec<usize>,
    len: usize,
}

impl LineIndex {
    fn new(text: &str) -> Self {
        let mut starts = vec![0usize];
        starts.extend(
            text.char_indices()
                .filter(|(_, ch)| *ch == '\n')
                .map(|(offset, _)| offset + 1),
        );
        Self {
            starts,
            len: text.len(),
        }
    }

    fn contains(&self, offset: u32) -> bool {
        (offset as usize) < self.len
    }

    /// One-based line of `offset`, or 0 when it falls outside the source.
    fn line(&self, offset: u32) -> u32 {
        if !self.contains(offset) {
            return 0;
        }
        let index = self
            .starts
            .partition_point(|start| *start <= offset as usize);
        u32::try_from(index).unwrap_or(u32::MAX)
    }

    fn column(&self, offset: u32) -> u32 {
        let line = self.line(offset);
        if line == 0 {
            return 0;
        }
        let start = self.starts[line as usize - 1];
        u32::try_from(offset as usize - start + 1).unwrap_or(u32::MAX)
    }
}

/// Per-module debug metadata state.
pub(super) struct DebugEmitter<'ctx> {
    ctx: &'ctx Context,
    builder: DebugInfoBuilder<'ctx>,
    file: DIFile<'ctx>,
    lines: LineIndex,
    /// Whether this target's debug records are CodeView rather than DWARF.
    codeview: bool,
    /// Root-unit lexical blocks, in HIR scope order.
    scopes: Vec<SemDebugScope>,
    /// Source field names of each concrete record, in declaration order.
    records: HashMap<ResolvedTy, Vec<PhysicalDebugField>>,
    /// Source variant names of each concrete enum, in tag order.
    enums: HashMap<ResolvedTy, Vec<PhysicalDebugVariant>>,
    types: RefCell<HashMap<String, DIType<'ctx>>>,
    _unit: DICompileUnit<'ctx>,
}

/// One body's subprogram and its lexical block tree.
pub(super) struct FunctionDebug<'ctx> {
    subprogram: DISubprogram<'ctx>,
    blocks: HashMap<u32, DILexicalBlock<'ctx>>,
    /// `(start, end, scope)` sorted innermost-first, so the first containing
    /// entry is the tightest scope around a byte.
    ranges: Vec<(u32, u32, u32)>,
}

impl<'ctx> DebugEmitter<'ctx> {
    /// Create the compile unit and the module flags the backend needs.
    pub(super) fn new(
        ctx: &'ctx Context,
        llvm: &Module<'ctx>,
        triple: &str,
        source: DebugSource<'_>,
        debug: &PhysicalDebug,
    ) -> Self {
        let file_name = source.path.file_name().map_or_else(
            || "module.hew".to_owned(),
            |name| name.to_string_lossy().into_owned(),
        );
        let directory = source
            .path
            .parent()
            .filter(|parent| !parent.as_os_str().is_empty())
            .map_or_else(
                || ".".to_owned(),
                |parent| parent.to_string_lossy().into_owned(),
            );
        let (builder, unit) = llvm.create_debug_info_builder(
            /* allow_unresolved */ true,
            // Hew's algebraic enums use Rust's `DW_TAG_variant_part` shape and
            // lldb only enables its active-variant renderer for a Rust compile
            // unit. WHEN OBSOLETE: a registered `DW_LANG_Hew` that lldb and gdb
            // recognize. WHAT THE REAL FIX IS: upstream that language code plus
            // a Hew-aware formatter, then name it here.
            DWARFSourceLanguage::Rust,
            &file_name,
            &directory,
            concat!("hew ", env!("CARGO_PKG_VERSION")),
            /* is_optimized */ false,
            /* compiler flags */ "",
            /* runtime_ver */ 0,
            /* split_name */ "",
            DWARFEmissionKind::Full,
            /* dwo_id */ 0,
            /* split_debug_inlining */ false,
            /* debug_info_for_profiling */ false,
            /* sysroot */ "",
            /* sdk */ "",
        );
        // The verifier strips all debug info when this flag does not equal the
        // current `DEBUG_METADATA_VERSION`.
        llvm.add_basic_value_flag(
            "Debug Info Version",
            FlagBehavior::Warning,
            ctx.i32_type().const_int(3, false),
        );
        // MSVC-environment targets need this flag for the backend to write
        // CodeView records (`.debug$S`/`.debug$T`) that the linker turns into a
        // PDB.
        let codeview = triple.contains("windows-msvc") || triple.ends_with("-msvc");
        if codeview {
            llvm.add_basic_value_flag(
                "CodeView",
                FlagBehavior::Warning,
                ctx.i32_type().const_int(1, false),
            );
        }
        let file = unit.get_file();
        Self {
            ctx,
            builder,
            file,
            lines: LineIndex::new(source.text),
            codeview,
            scopes: debug.scopes.clone(),
            records: debug.records.clone().into_iter().collect(),
            enums: debug.enums.clone().into_iter().collect(),
            types: RefCell::new(HashMap::new()),
            _unit: unit,
        }
    }

    /// Resolve forward references. Must run before module verification.
    pub(super) fn finalize(&self) {
        self.builder.finalize();
    }

    /// Create the subprogram for one body and its lexical block tree.
    pub(super) fn function(
        &self,
        value: FunctionValue<'ctx>,
        symbol: &str,
        attribution: &PhysicalDebugFunction,
    ) -> FunctionDebug<'ctx> {
        let line = self.lines.line(attribution.decl);
        let subroutine = self
            .builder
            .create_subroutine_type(self.file, None, &[], DIFlags::ZERO);
        let subprogram = self.builder.create_function(
            self.file.as_debug_info_scope(),
            &attribution.name,
            Some(symbol),
            self.file,
            line,
            subroutine,
            /* is_local_to_unit */ false,
            /* is_definition */ true,
            line,
            DIFlags::ZERO,
            /* is_optimized */ false,
        );
        value.set_subprogram(subprogram);
        let mut function = FunctionDebug {
            subprogram,
            blocks: HashMap::new(),
            ranges: Vec::new(),
        };
        self.build_lexical_blocks(&mut function, attribution);
        function
    }

    /// Build the `DILexicalBlock` tree for the scopes inside this body.
    ///
    /// The body's own block is the outermost scope inside the declaration, so a
    /// scope whose parent lies outside the body roots at the subprogram.
    fn build_lexical_blocks(
        &self,
        function: &mut FunctionDebug<'ctx>,
        attribution: &PhysicalDebugFunction,
    ) {
        let owned: Vec<&SemDebugScope> = self
            .scopes
            .iter()
            .filter(|scope| scope.start >= attribution.decl && scope.start < attribution.end)
            .collect();
        let mut remaining = owned;
        loop {
            let mut pending = Vec::new();
            let mut progressed = false;
            for scope in remaining {
                let parent = match scope
                    .parent
                    .filter(|parent| self.owns(attribution, *parent))
                {
                    None => Some(function.subprogram.as_debug_info_scope()),
                    Some(parent) => function
                        .blocks
                        .get(&parent)
                        .map(|block| block.as_debug_info_scope()),
                };
                let Some(parent) = parent else {
                    pending.push(scope);
                    continue;
                };
                let block = self.builder.create_lexical_block(
                    parent,
                    self.file,
                    self.lines.line(scope.start),
                    self.lines.column(scope.start),
                );
                function.blocks.insert(scope.id, block);
                function.ranges.push((scope.start, scope.end, scope.id));
                progressed = true;
            }
            if pending.is_empty() || !progressed {
                break;
            }
            remaining = pending;
        }
        function
            .ranges
            .sort_by_key(|(start, end, _)| end.saturating_sub(*start));
    }

    /// A debug location in the innermost scope containing `offset`.
    pub(super) fn location(
        &self,
        ctx: &'ctx Context,
        function: &FunctionDebug<'ctx>,
        offset: u32,
    ) -> DILocation<'ctx> {
        let scope = function.scope_for(offset);
        self.builder.create_debug_location(
            ctx,
            self.lines.line(offset),
            self.lines.column(offset),
            scope,
            None,
        )
    }

    /// A debug location at an exact scope and line.
    fn location_in(&self, ctx: &'ctx Context, scope: DIScope<'ctx>, line: u32) -> DILocation<'ctx> {
        self.builder
            .create_debug_location(ctx, line, 0, scope, None)
    }

    /// A debug location at the body's declaration, for prologue instructions.
    pub(super) fn declaration_location(
        &self,
        ctx: &'ctx Context,
        function: &FunctionDebug<'ctx>,
        decl: u32,
    ) -> DILocation<'ctx> {
        self.builder.create_debug_location(
            ctx,
            self.lines.line(decl),
            0,
            function.subprogram.as_debug_info_scope(),
            None,
        )
    }

    /// Create the variable DIE for one named local slot.
    ///
    /// Returns `None` when the storage's type has no describable shape; a
    /// missing DIE is honest, a guessed one is not.
    pub(super) fn local_variable(
        &self,
        function: &FunctionDebug<'ctx>,
        local: &PhysicalDebugLocal,
        ty: &ResolvedTy,
        layout: &PhysicalLayout,
        target: &PhysicalTarget,
    ) -> Option<(DILocalVariable<'ctx>, DIScope<'ctx>, u32)> {
        let PhysicalDebugLocal {
            name,
            decl,
            parameter,
        } = local;
        let (decl, parameter) = (*decl, *parameter);
        let di_type = self.resolve_type(ty, layout, target)?;
        if let Some(index) = parameter {
            let line = self.lines.line(decl);
            let scope = function.subprogram.as_debug_info_scope();
            let variable = self.builder.create_parameter_variable(
                scope,
                name,
                index,
                self.file,
                line,
                di_type,
                /* always_preserve */ true,
                DIFlags::ZERO,
            );
            return Some((variable, scope, line));
        }
        let scope = function.scope_for(decl);
        let line = self.lines.line(decl);
        let variable = self.builder.create_auto_variable(
            scope,
            name,
            self.file,
            line,
            di_type,
            /* always_preserve */ true,
            DIFlags::ZERO,
            /* align_in_bits */ 0,
        );
        Some((variable, scope, line))
    }

    /// Attach a whole-scope `llvm.dbg.declare` for a slot.
    pub(super) fn declare(
        &self,
        slot: PointerValue<'ctx>,
        variable: DILocalVariable<'ctx>,
        location: DILocation<'ctx>,
        block: inkwell::basic_block::BasicBlock<'ctx>,
    ) {
        let expression = self.builder.create_expression(vec![]);
        // WHY a raw call: on LLVM 19+ `LLVMDIBuilderInsertDeclareAtEnd` returns
        // a `DbgRecord`, and inkwell 0.9's safe wrapper casts that to an
        // `InstructionValue` whose `is_instruction()` assertion then panics even
        // though the record was inserted. The record API inkwell itself wraps
        // for this LLVM version is the correct one to call.
        use inkwell::llvm_sys::debuginfo::LLVMDIBuilderInsertDeclareRecordAtEnd;
        use inkwell::values::AsValueRef;
        // SAFETY: every pointer comes from a live inkwell wrapper bound to this
        // module's context; the call inserts a record and returns a handle we
        // discard.
        unsafe {
            LLVMDIBuilderInsertDeclareRecordAtEnd(
                self.builder.as_mut_ptr(),
                slot.as_value_ref(),
                variable.as_mut_ptr(),
                expression.as_mut_ptr(),
                location.as_mut_ptr(),
                block.as_mut_ptr(),
            );
        }
    }

    /// Whether `scope` is one of the lexical blocks inside this body.
    fn owns(&self, attribution: &PhysicalDebugFunction, scope: u32) -> bool {
        self.scopes
            .iter()
            .find(|candidate| candidate.id == scope)
            .is_some_and(|candidate| {
                candidate.start >= attribution.decl && candidate.start < attribution.end
            })
    }

    /// The DIE for one storage type, memoized by its structural key.
    fn resolve_type(
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
            // Every Hew reference handle is one machine pointer to bytes the
            // runtime owns. Describing the pointee would claim a layout this
            // stage does not have.
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
                    format!("f{index}"),
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

impl<'ctx> FunctionDebug<'ctx> {
    /// The innermost lexical block containing `offset`, else the subprogram.
    fn scope_for(&self, offset: u32) -> DIScope<'ctx> {
        self.ranges
            .iter()
            .find(|(start, end, _)| offset >= *start && offset < *end)
            .and_then(|(_, _, id)| self.blocks.get(id))
            .map_or_else(
                || self.subprogram.as_debug_info_scope(),
                |block| block.as_debug_info_scope(),
            )
    }
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
        ResolvedTy::Named { name, .. } => name.clone(),
        ResolvedTy::Tuple(_) => "tuple".to_owned(),
        other => format!("{other:?}"),
    }
}

/// Emit the variable DIE and whole-scope `llvm.dbg.declare` for every named
/// source local this body allocates.
pub(super) struct PendingLocal<'ctx> {
    slot: PointerValue<'ctx>,
    variable: DILocalVariable<'ctx>,
    location: DILocation<'ctx>,
}

/// A suspend-carrying body cannot take `optnone`, so its slot stores are free
/// to lag their source line and a prologue declare would let the debugger print
/// stale bits as if they were the local. Those body locals are returned instead
/// and resolved by [`resolve_coroutine_locals`] once the stores exist.
#[allow(
    clippy::too_many_arguments,
    reason = "debug emission threads the same explicit borrows as the body lowering"
)]
pub(super) fn declare_locals<'ctx>(
    ctx: &'ctx Context,
    emitter: &DebugEmitter<'ctx>,
    function_debug: &FunctionDebug<'ctx>,
    attribution: &PhysicalDebugFunction,
    function: &PhysicalFunction,
    target: &PhysicalTarget,
    slots: &[PointerValue<'ctx>],
    prologue: inkwell::basic_block::BasicBlock<'ctx>,
    resumable: bool,
) -> Vec<PendingLocal<'ctx>> {
    let mut pending = Vec::new();
    for (id, local) in &attribution.locals {
        let Some(storage) = function.storage.get(id.0 as usize) else {
            continue;
        };
        let Some(slot) = slots.get(id.0 as usize) else {
            continue;
        };
        let Some((variable, scope, line)) =
            emitter.local_variable(function_debug, local, &storage.ty, &storage.layout, target)
        else {
            continue;
        };
        let location = emitter.location_in(ctx, scope, line);
        if resumable && local.parameter.is_none() {
            pending.push(PendingLocal {
                slot: *slot,
                variable,
                location,
            });
            continue;
        }
        emitter.declare(*slot, variable, location, prologue);
    }
    pending
}

/// Keep a `-g` body's slots faithfully inspectable.
///
/// `optnone` (which the verifier pairs with `noinline`) stops instruction
/// selection from sinking a slot store past the line that wrote it, so a
/// breakpoint reads what the source says. A suspend-carrying body cannot take
/// it: `CoroSplit` has to run, and honest post-suspend locations are that
/// path's own problem.
pub(super) fn pin_for_inspection(ctx: &Context, value: FunctionValue<'_>, resumable: bool) {
    use inkwell::attributes::{Attribute, AttributeLoc};
    let names: &[&str] = if resumable {
        &["noinline"]
    } else {
        &["noinline", "optnone"]
    };
    for name in names {
        let kind = Attribute::get_named_enum_kind_id(name);
        if kind != 0 {
            value.add_attribute(AttributeLoc::Function, ctx.create_enum_attribute(kind, 0));
        }
    }
}

/// Lay a `-g` body's blocks out in execution order.
///
/// Physical block ids follow the order SIR created them, which puts a fault or
/// cleanup block ahead of the normal continuation that shares its source line.
/// A debugger resolving `file:line` takes the lowest address for that line in
/// the function, so the breakpoint lands on a block the run never enters.
/// Reverse postorder from the entry puts the executed path first.
pub(super) fn order_blocks_for_inspection(value: FunctionValue<'_>) {
    use inkwell::llvm_sys::core::{
        LLVMGetBasicBlockTerminator, LLVMGetNumSuccessors, LLVMGetSuccessor,
    };

    let blocks = value.get_basic_blocks();
    if blocks.len() < 2 {
        return;
    }
    let address = |index: usize| blocks[index].as_mut_ptr() as usize;
    let mut order: Vec<usize> = Vec::with_capacity(blocks.len());
    let mut visited = vec![false; blocks.len()];
    let mut stack = vec![0usize];
    while let Some(index) = stack.pop() {
        if visited[index] {
            continue;
        }
        visited[index] = true;
        order.push(index);
        // SAFETY: the block belongs to a fully built function, so its
        // terminator and successor list are live for this read.
        let successors: Vec<usize> = unsafe {
            let terminator = LLVMGetBasicBlockTerminator(blocks[index].as_mut_ptr());
            if terminator.is_null() {
                Vec::new()
            } else {
                (0..LLVMGetNumSuccessors(terminator))
                    .filter_map(|slot| {
                        let successor = LLVMGetSuccessor(terminator, slot) as usize;
                        (0..blocks.len()).find(|candidate| address(*candidate) == successor)
                    })
                    .collect()
            }
        };
        // Pushed in reverse so the first successor is laid out first.
        for successor in successors.into_iter().rev() {
            stack.push(successor);
        }
    }
    // Unreachable blocks keep their relative order at the end.
    order.extend((0..blocks.len()).filter(|index| !visited[*index]));
    for pair in order.windows(2) {
        blocks[pair[1]].move_after(blocks[pair[0]]).ok();
    }
}

/// Give each suspend-carrying body local an honest location.
///
/// Per local, read from the finished pre-`CoroSplit` IR:
///
/// - Every direct store precedes every suspend point: keep the prologue
///   declare. `CoroSplit` rewrites it onto the coroutine frame, which is what
///   makes a pre-suspend local readable after a resume.
/// - A store is reachable at or after a suspend point: anchor a `dbg.value` of
///   the stored value after each store instead. The variable's location list
///   then begins where the assignment actually executes; before it the
///   debugger reports the local unavailable rather than reading stale bits.
/// - The slot has a user this pass cannot read as a plain load or store (a
///   field-wise write through a GEP, a memcpy, the address escaping into a
///   call): keep the declare, because value anchoring would miss that write
///   and leave the last anchor confidently wrong.
pub(super) fn resolve_coroutine_locals<'ctx>(
    emitter: &DebugEmitter<'ctx>,
    llvm: &Module<'ctx>,
    value: FunctionValue<'ctx>,
    prologue: inkwell::basic_block::BasicBlock<'ctx>,
    pending: &[PendingLocal<'ctx>],
) {
    use inkwell::llvm_sys::core::{
        LLVMGetBasicBlockTerminator, LLVMGetCalledValue, LLVMGetFirstUse, LLVMGetNextUse,
        LLVMGetNumSuccessors, LLVMGetSuccessor, LLVMGetUser,
    };
    use inkwell::llvm_sys::debuginfo::LLVMDIBuilderInsertDbgValueRecordBefore;
    use inkwell::llvm_sys::prelude::LLVMBasicBlockRef;
    use inkwell::values::{AsValueRef, BasicValueEnum, InstructionOpcode, InstructionValue};

    if pending.is_empty() {
        return;
    }
    let expression = emitter.builder.create_expression(vec![]);
    // `DW_OP_deref, DW_OP_stack_value`: the variable's value IS the slot's
    // contents, restated after every store. A bare trailing `DW_OP_deref`
    // described a location instead, and LLVM appends it to the slot's own
    // address on the ramp copy, so a debugger read the variable out of
    // whatever address the slot's contents spell.
    let through_slot = emitter.builder.create_expression(vec![0x06, 0x9f]);
    let suspend = llvm
        .get_function("llvm.coro.suspend")
        .map(|function| function.as_value_ref());

    // Blocks that can execute at or after a suspend: the transitive successors
    // of every block holding a `llvm.coro.suspend`. A suspend block's own
    // earlier stores stay pre-suspend; it joins the set only through a back
    // edge, which errs toward honest absence.
    let mut post_suspend: Vec<usize> = Vec::new();
    let mut work: Vec<LLVMBasicBlockRef> = Vec::new();
    if let Some(suspend) = suspend {
        for block in value.get_basic_blocks() {
            let mut cursor = block.get_first_instruction();
            while let Some(instruction) = cursor {
                // SAFETY: a live call instruction of this module; the call only
                // reads its callee operand.
                if instruction.get_opcode() == InstructionOpcode::Call
                    && unsafe { LLVMGetCalledValue(instruction.as_value_ref()) } == suspend
                {
                    if let Some(terminator) = block.get_terminator() {
                        // SAFETY: read-only successor iteration over this
                        // function's CFG.
                        unsafe {
                            for index in 0..LLVMGetNumSuccessors(terminator.as_value_ref()) {
                                work.push(LLVMGetSuccessor(terminator.as_value_ref(), index));
                            }
                        }
                    }
                    break;
                }
                cursor = instruction.get_next_instruction();
            }
        }
    }
    while let Some(block) = work.pop() {
        if post_suspend.contains(&(block as usize)) {
            continue;
        }
        post_suspend.push(block as usize);
        // SAFETY: the block belongs to this function; a null terminator is
        // guarded.
        unsafe {
            let terminator = LLVMGetBasicBlockTerminator(block);
            if !terminator.is_null() {
                for index in 0..LLVMGetNumSuccessors(terminator) {
                    work.push(LLVMGetSuccessor(terminator, index));
                }
            }
        }
    }

    for local in pending {
        let slot = local.slot.as_value_ref();
        let mut stores: Vec<InstructionValue<'ctx>> = Vec::new();
        let mut any_post_suspend = false;
        let mut opaque_user = false;
        // SAFETY: read-only def-use iteration over live IR.
        let mut next_use = unsafe { LLVMGetFirstUse(slot) };
        while !next_use.is_null() {
            // SAFETY: the use handle is live and owned by the module.
            let user = unsafe { InstructionValue::new(LLVMGetUser(next_use)) };
            match user.get_opcode() {
                InstructionOpcode::Load => {}
                // Matched only when the slot is the pointer operand: storing
                // the slot's own address into memory is an escape.
                InstructionOpcode::Store
                    if user
                        .get_operand(1)
                        .and_then(|operand| operand.value())
                        .is_some_and(|pointer| pointer.as_value_ref() == slot) =>
                {
                    any_post_suspend |= user
                        .get_parent()
                        .is_some_and(|block| post_suspend.contains(&(block.as_mut_ptr() as usize)));
                    stores.push(user);
                }
                _ => {
                    opaque_user = true;
                    break;
                }
            }
            // SAFETY: iteration over the same live use list.
            next_use = unsafe { LLVMGetNextUse(next_use) };
        }
        if opaque_user || stores.is_empty() || !any_post_suspend {
            emitter.declare(local.slot, local.variable, local.location, prologue);
            continue;
        }
        for store in stores {
            // A store is never a terminator, so a successor normally exists;
            // without one the anchor is simply skipped.
            let (Some(next), Some(stored)) = (
                store.get_next_instruction(),
                store.get_operand(0).and_then(|operand| operand.value()),
            ) else {
                continue;
            };
            // A constant-null pointer store is the release-and-null interior
            // state of a reassignment, never a value the source can observe:
            // Hew has no null. Anchoring undef ends the location list there, so
            // the local reads unavailable until the replacement's range begins.
            // Integer zero stays a real anchor.
            let (anchor, anchor_expression) = match stored {
                BasicValueEnum::PointerValue(pointer) if pointer.is_null() => {
                    (pointer.get_type().get_undef().as_value_ref(), expression)
                }
                // Anchor the slot, not the stored SSA value. `CoroSplit` maps a
                // spilled value onto whichever frame word held it, and that word
                // is reused — a release-and-null of the source slot then reads
                // back as the variable. The variable's own storage is the one
                // address that stays true until the next assignment.
                _ => (local.slot.as_value_ref(), through_slot),
            };
            // SAFETY: every wrapper belongs to this module's context and
            // builder; the call inserts one record and returns a handle we
            // discard.
            unsafe {
                LLVMDIBuilderInsertDbgValueRecordBefore(
                    emitter.builder.as_mut_ptr(),
                    anchor,
                    local.variable.as_mut_ptr(),
                    anchor_expression.as_mut_ptr(),
                    local.location.as_mut_ptr(),
                    next.as_value_ref(),
                );
            }
        }
    }
}
