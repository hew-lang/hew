//! DWARF/CodeView metadata for `hew build -g`.
//!
//! Every name, span and lexical scope here is a physical MIR fact projected
//! from SIR ([`hew_mir::physical::PhysicalDebug`]). This module decides only
//! how those facts are spelled as LLVM debug metadata.

use std::cell::RefCell;
use std::collections::HashMap;
use std::path::Path;

use hew_mir::physical::SemDebugScope;
use hew_mir::physical::{PhysicalDebug, PhysicalDebugFunction};
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
    builder: DebugInfoBuilder<'ctx>,
    file: DIFile<'ctx>,
    lines: LineIndex,
    /// Root-unit lexical blocks, in HIR scope order.
    scopes: Vec<SemDebugScope>,
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
        // PDB. The DIE graph itself is format-neutral.
        if triple.contains("windows-msvc") || triple.ends_with("-msvc") {
            llvm.add_basic_value_flag(
                "CodeView",
                FlagBehavior::Warning,
                ctx.i32_type().const_int(1, false),
            );
        }
        let file = unit.get_file();
        Self {
            builder,
            file,
            lines: LineIndex::new(source.text),
            scopes: debug.scopes.clone(),
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
        name: &str,
        decl: u32,
        parameter: Option<u32>,
        ty: &ResolvedTy,
        layout: &PhysicalLayout,
    ) -> Option<(DILocalVariable<'ctx>, DIScope<'ctx>, u32)> {
        let di_type = self.resolve_type(ty, layout)?;
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
    fn resolve_type(&self, ty: &ResolvedTy, layout: &PhysicalLayout) -> Option<DIType<'ctx>> {
        let key = format!("{ty:?}|{:?}", layout.repr);
        if let Some(cached) = self.types.borrow().get(&key) {
            return Some(*cached);
        }
        let resolved = self.build_type(ty, layout)?;
        self.types.borrow_mut().insert(key, resolved);
        Some(resolved)
    }

    fn build_type(&self, ty: &ResolvedTy, layout: &PhysicalLayout) -> Option<DIType<'ctx>> {
        let bits = layout.size.checked_mul(8)?;
        if let Some((name, encoding)) = named_scalar(ty) {
            return self
                .builder
                .create_basic_type(name, bits, encoding, DIFlags::ZERO)
                .ok()
                .map(|basic| basic.as_type());
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
                    .ok()?;
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
                let inner = self.build_type(&ResolvedTy::Unit, element)?;
                Some(
                    self.builder
                        .create_array_type(inner, bits, layout.align * 8, &[0..i64::from(*len)])
                        .as_type(),
                )
            }
            PhysicalRepr::Struct(fields) => self.build_struct(ty, layout, fields),
        }
    }

    fn build_struct(
        &self,
        ty: &ResolvedTy,
        layout: &PhysicalLayout,
        fields: &[PhysicalLayout],
    ) -> Option<DIType<'ctx>> {
        let mut offset = 0u64;
        let mut members = Vec::with_capacity(fields.len());
        for (index, field) in fields.iter().enumerate() {
            let align = u64::from(field.align).max(1);
            offset = offset.div_ceil(align) * align;
            let member_ty = self.build_type(&ResolvedTy::Unit, field)?;
            members.push(
                self.builder
                    .create_member_type(
                        self.file.as_debug_info_scope(),
                        &format!("f{index}"),
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
pub(super) fn declare_locals<'ctx>(
    ctx: &'ctx Context,
    emitter: &DebugEmitter<'ctx>,
    function_debug: &FunctionDebug<'ctx>,
    attribution: &PhysicalDebugFunction,
    function: &PhysicalFunction,
    slots: &[PointerValue<'ctx>],
    prologue: inkwell::basic_block::BasicBlock<'ctx>,
) {
    for (id, local) in &attribution.locals {
        let Some(storage) = function.storage.get(id.0 as usize) else {
            continue;
        };
        let Some(slot) = slots.get(id.0 as usize) else {
            continue;
        };
        let Some((variable, scope, line)) = emitter.local_variable(
            function_debug,
            &local.name,
            local.decl,
            local.parameter,
            &storage.ty,
            &storage.layout,
        ) else {
            continue;
        };
        emitter.declare(
            *slot,
            variable,
            emitter.location_in(ctx, scope, line),
            prologue,
        );
    }
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
