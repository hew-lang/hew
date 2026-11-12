//! DWARF/CodeView metadata for `hew build -g`.
//!
//! Every name, span and lexical scope here is a physical MIR fact projected
//! from SIR ([`hew_mir::physical::PhysicalDebug`]). This module decides only
//! how those facts are spelled as LLVM debug metadata.

#[path = "physical_debug/types.rs"]
mod types;

use super::StorageId;
use std::cell::{Cell, RefCell};
use std::collections::{BTreeMap, HashMap};
use std::path::Path;

use hew_mir::physical::debug_names::DisplayName;
use hew_mir::physical::CallableId;
use hew_mir::physical::SemDebugScope;
use hew_mir::physical::{
    PhysicalCallable, PhysicalDebug, PhysicalDebugField, PhysicalDebugFunction, PhysicalDebugLocal,
    PhysicalDebugVariant, PhysicalTarget,
};
use hew_mir::{PhysicalFunction, PhysicalLayout};
use hew_types::ResolvedTy;
use inkwell::context::Context;
use inkwell::debug_info::{
    AsDIScope, DICompileUnit, DIFile, DIFlags, DIFlagsConstants, DILexicalBlock, DILocalVariable,
    DILocation, DIScope, DISubprogram, DIType, DWARFEmissionKind, DWARFSourceLanguage,
    DebugInfoBuilder,
};
use inkwell::module::{FlagBehavior, Module};
use inkwell::values::{AsValueRef, FunctionValue, PointerValue};

/// The source text a `-g` build attributes its metadata to.
#[derive(Debug, Clone, Copy)]
pub struct DebugSource<'a> {
    /// The source path as it should appear in debug information, after any
    /// caller-selected path remapping.
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
    /// Source field names of each concrete record, in declaration order.
    records: HashMap<ResolvedTy, Vec<PhysicalDebugField>>,
    /// Source variant names of each concrete enum, in tag order.
    enums: HashMap<ResolvedTy, Vec<PhysicalDebugVariant>>,
    /// The description of each storage type, built once per type.
    types: RefCell<BTreeMap<ResolvedTy, DIType<'ctx>>>,
    /// Composites created so far, numbering their unique identifiers.
    composites: Cell<u64>,
    /// Display name of every callable, from the display-name authority.
    names: BTreeMap<CallableId, DisplayName>,
    /// The module is compiled with optimization, so locations may be partial.
    optimized: bool,
    _unit: DICompileUnit<'ctx>,
}

/// One body's subprogram and its lexical block tree.
pub(super) struct FunctionDebug<'ctx> {
    subprogram: DISubprogram<'ctx>,
    blocks: HashMap<u32, DILexicalBlock<'ctx>>,
    /// `(start, end, scope)` sorted innermost-first, so the first containing
    /// entry is the tightest scope around a byte.
    ranges: Vec<(u32, u32, u32)>,
    /// Locals of a resumable body, which a debugger sees only while their
    /// slot holds a value; see [`DebugEmitter::bind_local`].
    locals: RefCell<HashMap<StorageId, LocalSlot<'ctx>>>,
}

/// A source local whose location is a value range over its slot.
struct LocalSlot<'ctx> {
    variable: DILocalVariable<'ctx>,
    location: DILocation<'ctx>,
    slot: PointerValue<'ctx>,
}

impl<'ctx> DebugEmitter<'ctx> {
    /// Create the compile unit and the module flags the backend needs.
    ///
    /// The unit names the source by its file name in its canonical absolute
    /// directory, so a debugger started anywhere finds the text, and records
    /// the text's MD5 so it can tell a stale copy from the one compiled.
    pub(super) fn new(
        ctx: &'ctx Context,
        llvm: &Module<'ctx>,
        triple: &str,
        source: DebugSource<'_>,
        debug: &PhysicalDebug,
        optimized: bool,
    ) -> Self {
        let file_name = source.path.file_name().map_or_else(
            || "module.hew".to_owned(),
            |name| name.to_string_lossy().into_owned(),
        );
        let directory = source_directory(source.path);
        let (builder, unit) = llvm.create_debug_info_builder(
            // Every type is complete when it is created; nothing is left for
            // `finalize` to resolve.
            /* allow_unresolved */
            false,
            // Hew has no registered DWARF language code. Every type here is a
            // C shape (typedefs, tagged structs, unions, enumerations), so a C
            // unit gives gdb, lldb and the Windows debuggers their full C
            // expression evaluator and renders those shapes without a plugin.
            DWARFSourceLanguage::C11,
            &file_name,
            &directory,
            concat!("hew ", env!("CARGO_PKG_VERSION")),
            optimized,
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
        // PDB. Every other target writes DWARF 5, whose line table carries the
        // source checksum and column.
        let codeview = triple.contains("windows-msvc") || triple.ends_with("-msvc");
        let (format, version) = if codeview {
            ("CodeView", 1)
        } else {
            ("Dwarf Version", 5)
        };
        llvm.add_basic_value_flag(
            format,
            FlagBehavior::Warning,
            ctx.i32_type().const_int(version, false),
        );
        let file = checksummed_file(&builder, unit, &file_name, &directory, source.text);
        Self {
            builder,
            file,
            lines: LineIndex::new(source.text),
            scopes: debug.scopes.clone(),
            records: debug.records.clone().into_iter().collect(),
            enums: debug.enums.clone().into_iter().collect(),
            types: RefCell::new(BTreeMap::new()),
            composites: Cell::new(0),
            names: debug.names.clone(),
            optimized,
            _unit: unit,
        }
    }

    /// Resolve forward references. Must run before module verification.
    pub(super) fn finalize(&self) {
        self.builder.finalize();
    }

    /// Create the subprogram for one body and its lexical block tree.
    ///
    /// The subprogram carries the callable's display name and no linkage
    /// name: lldb indexes a subprogram that has one by that name alone, and a
    /// Hew symbol does not demangle to the name a person types. The
    /// subroutine type lists the source parameters and result, so a debugger
    /// can print a call's arguments.
    pub(super) fn function(
        &self,
        value: FunctionValue<'ctx>,
        callable: &PhysicalCallable,
        attribution: &PhysicalDebugFunction,
        target: &PhysicalTarget,
    ) -> FunctionDebug<'ctx> {
        let display = self.names.get(&callable.id);
        let artificial = display.is_none_or(|name| name.artificial);
        let line = if artificial {
            0
        } else {
            self.lines.line(attribution.decl)
        };
        let params: Vec<DIType<'ctx>> = callable
            .params
            .iter()
            .filter_map(|param| self.resolve_type(&param.ty, &param.layout, target))
            .collect();
        // No result type: a Hew body returns its fault status in the return
        // register and its value through an out pointer, so a declared result
        // would make `finish` print the status as the value.
        let subroutine =
            self.builder
                .create_subroutine_type(self.file, None, &params, DIFlags::PROTOTYPED);
        let flags = if artificial {
            DIFlags::PROTOTYPED | DIFlags::ARTIFICIAL
        } else {
            DIFlags::PROTOTYPED
        };
        let subprogram = self.builder.create_function(
            self.file.as_debug_info_scope(),
            display.map_or(callable.symbol.as_str(), |name| name.text.as_str()),
            Some(""),
            self.file,
            line,
            subroutine,
            /* is_local_to_unit */ false,
            /* is_definition */ true,
            // The declaration line is the scope line, so the function's first
            // row is its declaration, as in a compiled C function, and the
            // argument spills sit on it. gdb, which analyses the frame setup
            // itself for a producer it does not know, then skips to the next
            // line row and stops with the parameters stored. LLVM ends the
            // prologue on the first spill, so lldb stops there too.
            line,
            flags,
            self.optimized,
        );
        value.set_subprogram(subprogram);
        let mut function = FunctionDebug {
            subprogram,
            blocks: HashMap::new(),
            ranges: Vec::new(),
            locals: RefCell::new(HashMap::new()),
        };
        self.build_lexical_blocks(&mut function, attribution);
        function
    }

    /// Describe a compiler-generated function with no source of its own.
    ///
    /// The subprogram is artificial and every instruction sits on line 0, so a
    /// debugger steps through it without stopping and never resolves a source
    /// name to it. `name` is a spelling from the display-name authority.
    pub(super) fn shim(&self, ctx: &'ctx Context, value: FunctionValue<'ctx>, name: &str) {
        let subroutine =
            self.builder
                .create_subroutine_type(self.file, None, &[], DIFlags::PROTOTYPED);
        let subprogram = self.builder.create_function(
            self.file.as_debug_info_scope(),
            name,
            Some(""),
            self.file,
            0,
            subroutine,
            /* is_local_to_unit */ false,
            /* is_definition */ true,
            0,
            DIFlags::PROTOTYPED | DIFlags::ARTIFICIAL,
            self.optimized,
        );
        value.set_subprogram(subprogram);
        let location =
            self.builder
                .create_debug_location(ctx, 0, 0, subprogram.as_debug_info_scope(), None);
        for block in value.get_basic_blocks() {
            for instruction in block.get_instructions() {
                // SAFETY: the instruction and location belong to this live
                // context; the call only replaces the instruction's `!dbg`.
                unsafe {
                    inkwell::llvm_sys::debuginfo::LLVMInstructionSetDebugLoc(
                        instruction.as_value_ref(),
                        location.as_mut_ptr(),
                    );
                }
            }
        }
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

    /// Whether the unit describes optimized code.
    pub(super) fn optimized(&self) -> bool {
        self.optimized
    }

    /// A debug location at an exact scope and line.
    fn location_in(&self, ctx: &'ctx Context, scope: DIScope<'ctx>, line: u32) -> DILocation<'ctx> {
        self.builder
            .create_debug_location(ctx, line, 0, scope, None)
    }

    /// Line 0 in the body's own scope: code no source line owns.
    ///
    /// The prologue (frame setup, parameter spills, storage zeroing), cleanup,
    /// and fault exits carry it. A debugger then places `prologue_end` on the
    /// first body statement, after every parameter is stored, and never
    /// attributes compiler-generated code to a source line, so a line keeps
    /// one statement address and `next` stops on it once.
    pub(super) fn unattributed_location(
        &self,
        ctx: &'ctx Context,
        function: &FunctionDebug<'ctx>,
    ) -> DILocation<'ctx> {
        self.builder.create_debug_location(
            ctx,
            0,
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
        insert_declare(&self.builder, slot, variable, location, block);
    }

    /// Say that `id` holds its value from here: its location is the slot's
    /// memory until a later record ends it. The record names the slot, not a
    /// copy, so the slot stays the one home of the value.
    pub(super) fn bind_local(
        &self,
        function: &FunctionDebug<'ctx>,
        id: StorageId,
        block: inkwell::basic_block::BasicBlock<'ctx>,
    ) {
        if let Some(local) = function.locals.borrow().get(&id) {
            self.value_record(local.slot.into(), local, block, true);
        }
    }

    /// Say that `id` holds no value from here: a debugger reports it
    /// unavailable instead of reading whatever the slot last held.
    pub(super) fn kill_local(
        &self,
        ctx: &'ctx Context,
        function: &FunctionDebug<'ctx>,
        id: StorageId,
        block: inkwell::basic_block::BasicBlock<'ctx>,
    ) {
        if let Some(local) = function.locals.borrow().get(&id) {
            self.value_record(ctx.i8_type().get_undef().into(), local, block, false);
        }
    }

    /// Describe parameter `id` by its incoming argument from the first
    /// instruction of the body. A coroutine ramp reaches its frame only after
    /// the frame is allocated, so until the record that names the frame slot
    /// the argument itself is the only place the value is.
    pub(super) fn bind_argument(
        &self,
        function: &FunctionDebug<'ctx>,
        id: StorageId,
        argument: inkwell::values::BasicValueEnum<'ctx>,
        indirect: bool,
        block: inkwell::basic_block::BasicBlock<'ctx>,
    ) {
        use inkwell::llvm_sys::debuginfo::{
            LLVMDIBuilderInsertDbgValueRecordAtEnd, LLVMDIBuilderInsertDbgValueRecordBefore,
        };
        let Some(local) = function.locals.borrow().get(&id).map(|local| LocalSlot {
            variable: local.variable,
            location: local.location,
            slot: local.slot,
        }) else {
            return;
        };
        let expression =
            self.builder
                .create_expression(if indirect { vec![DW_OP_DEREF] } else { vec![] });
        // SAFETY: the value, metadata and block belong to this live context.
        unsafe {
            if let Some(first) = block.get_first_instruction() {
                LLVMDIBuilderInsertDbgValueRecordBefore(
                    self.builder.as_mut_ptr(),
                    argument.as_value_ref(),
                    local.variable.as_mut_ptr(),
                    expression.as_mut_ptr(),
                    local.location.as_mut_ptr(),
                    first.as_value_ref(),
                );
            } else {
                LLVMDIBuilderInsertDbgValueRecordAtEnd(
                    self.builder.as_mut_ptr(),
                    argument.as_value_ref(),
                    local.variable.as_mut_ptr(),
                    expression.as_mut_ptr(),
                    local.location.as_mut_ptr(),
                    block.as_mut_ptr(),
                );
            }
        }
    }

    fn value_record(
        &self,
        value: inkwell::values::BasicValueEnum<'ctx>,
        local: &LocalSlot<'ctx>,
        block: inkwell::basic_block::BasicBlock<'ctx>,
        indirect: bool,
    ) {
        use inkwell::llvm_sys::debuginfo::{
            LLVMDIBuilderInsertDbgValueRecordAtEnd, LLVMDIBuilderInsertDbgValueRecordBefore,
        };
        let expression =
            self.builder
                .create_expression(if indirect { vec![DW_OP_DEREF] } else { vec![] });
        // SAFETY: the value, metadata and block belong to this live context.
        unsafe {
            if let Some(terminator) = block.get_terminator() {
                LLVMDIBuilderInsertDbgValueRecordBefore(
                    self.builder.as_mut_ptr(),
                    value.as_value_ref(),
                    local.variable.as_mut_ptr(),
                    expression.as_mut_ptr(),
                    local.location.as_mut_ptr(),
                    terminator.as_value_ref(),
                );
            } else {
                LLVMDIBuilderInsertDbgValueRecordAtEnd(
                    self.builder.as_mut_ptr(),
                    value.as_value_ref(),
                    local.variable.as_mut_ptr(),
                    expression.as_mut_ptr(),
                    local.location.as_mut_ptr(),
                    block.as_mut_ptr(),
                );
            }
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
}

/// The first source point execution reaches from `block`, following
/// unconditional edges and safepoints' normal edges.
pub(super) fn first_site_from(
    function: &PhysicalFunction,
    attribution: &PhysicalDebugFunction,
    block: hew_mir::physical::BlockId,
) -> Option<u32> {
    use hew_mir::physical::PhysicalTerminator;
    let mut target = block;
    let mut seen = std::collections::BTreeSet::new();
    while seen.insert(target) {
        if let Some((_, offset)) = attribution
            .sites
            .range((target, 0)..=(target, u32::MAX))
            .next()
        {
            return Some(*offset);
        }
        target = match &function
            .blocks
            .iter()
            .find(|candidate| candidate.id == target)?
            .terminator
        {
            PhysicalTerminator::Goto(edge) | PhysicalTerminator::Safepoint { normal: edge, .. } => {
                edge.target
            }
            _ => return None,
        };
    }
    None
}

/// Whether `machine` generates optimized code, which the compile unit and
/// subprograms state so a debugger expects partial variable locations.
pub(super) fn optimizes(machine: &inkwell::targets::TargetMachine) -> bool {
    unsafe extern "C" {
        fn hewLLVMTargetMachineOptimizes(
            machine: inkwell::llvm_sys::target_machine::LLVMTargetMachineRef,
        ) -> bool;
    }
    // SAFETY: the machine is live for the call, which only reads its level.
    unsafe { hewLLVMTargetMachineOptimizes(machine.as_mut_ptr()) }
}

/// Insert a local's debug record without treating it as an LLVM instruction.
pub(super) fn insert_declare<'ctx>(
    builder: &DebugInfoBuilder<'ctx>,
    slot: PointerValue<'ctx>,
    variable: DILocalVariable<'ctx>,
    location: DILocation<'ctx>,
    block: inkwell::basic_block::BasicBlock<'ctx>,
) {
    let expression = builder.create_expression(vec![]);
    // LLVM 19+ returns a DbgRecord, but inkwell 0.9 casts it to an
    // InstructionValue and can panic. The record API returns the right type.
    use inkwell::llvm_sys::debuginfo::LLVMDIBuilderInsertDeclareRecordAtEnd;
    // SAFETY: all handles belong to this live context, and the returned
    // record is owned by the block.
    unsafe {
        LLVMDIBuilderInsertDeclareRecordAtEnd(
            builder.as_mut_ptr(),
            slot.as_value_ref(),
            variable.as_mut_ptr(),
            expression.as_mut_ptr(),
            location.as_mut_ptr(),
            block.as_mut_ptr(),
        );
    }
}

/// The source directory in the caller's presentation namespace.
fn source_directory(path: &Path) -> String {
    path.parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."))
        .to_string_lossy()
        .into_owned()
}

/// The compile unit's file, carrying the MD5 of the text compiled. The shim
/// installs it as the unit's own file, and every function and type uses the
/// same node, so the line table has one checksummed file entry.
///
/// LLVM's C API creates a file without a checksum; `physical_debug_shim.cpp`
/// provides that one step.
fn checksummed_file<'ctx>(
    builder: &DebugInfoBuilder<'ctx>,
    unit: DICompileUnit<'ctx>,
    name: &str,
    directory: &str,
    text: &str,
) -> DIFile<'ctx> {
    use inkwell::llvm_sys::prelude::{LLVMDIBuilderRef, LLVMMetadataRef};
    unsafe extern "C" {
        fn hewLLVMDICompileUnitChecksumFile(
            builder: LLVMDIBuilderRef,
            unit: LLVMMetadataRef,
            name: *const std::ffi::c_char,
            name_len: usize,
            directory: *const std::ffi::c_char,
            directory_len: usize,
            text: *const std::ffi::c_char,
            text_len: usize,
        ) -> LLVMMetadataRef;
    }
    // SAFETY: the builder and unit belong to this live context; the shim
    // borrows the strings only for the call.
    let file = unsafe {
        hewLLVMDICompileUnitChecksumFile(
            builder.as_mut_ptr(),
            unit.as_mut_ptr(),
            name.as_ptr().cast(),
            name.len(),
            directory.as_ptr().cast(),
            directory.len(),
            text.as_ptr().cast(),
            text.len(),
        )
    };
    if file.is_null() {
        return unit.get_file();
    }
    // SAFETY: inkwell offers no constructor for a `DIFile` from a node made
    // outside it. `DIFile` is a `Copy` wrapper whose only sized field is the
    // metadata reference (the transmute checks the sizes), and `file` is a
    // live `DIFile` node of this context.
    unsafe { std::mem::transmute::<LLVMMetadataRef, DIFile<'ctx>>(file) }
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

/// `DW_OP_deref`: read the value at the address the record names.
const DW_OP_DEREF: i64 = 0x06;

/// The storage of one body whose locals are described.
pub(super) struct BodyLocals<'a, 'ctx> {
    pub attribution: &'a PhysicalDebugFunction,
    pub function: &'a PhysicalFunction,
    pub target: &'a PhysicalTarget,
    /// One slot per storage entry, indexed by local id.
    pub slots: &'a [PointerValue<'ctx>],
    /// The entry block the declarations are placed in.
    pub prologue: inkwell::basic_block::BasicBlock<'ctx>,
    /// The body is a coroutine. Its slots are written on one side of a
    /// suspension and read on the other, so a local is described only over
    /// the stretches that follow a store to it.
    pub resumable: bool,
}

/// Describe every named local of one body by its storage slot.
///
/// The slot is the variable's single home on every path, so one
/// `dbg.declare` covers the whole body: mem2reg and SROA turn it into value
/// locations under optimization. A coroutine body is different: a local
/// there is honest only after its store, so it gets value records over the
/// slot instead, bound where the slot is written and ended where it is
/// cleared; coroutine splitting rewrites them to the frame field the slot
/// moves into.
pub(super) fn declare_locals<'ctx>(
    ctx: &'ctx Context,
    emitter: &DebugEmitter<'ctx>,
    function_debug: &FunctionDebug<'ctx>,
    body: &BodyLocals<'_, 'ctx>,
) {
    let BodyLocals {
        attribution,
        function,
        target,
        slots,
        prologue,
        resumable,
    } = *body;
    for (id, local) in &attribution.locals {
        let (Some(storage), Some(slot)) = (
            function.storage.get(id.0 as usize),
            slots.get(id.0 as usize),
        ) else {
            continue;
        };
        let Some((variable, scope, line)) =
            emitter.local_variable(function_debug, local, &storage.ty, &storage.layout, target)
        else {
            continue;
        };
        let location = emitter.location_in(ctx, scope, line);
        if resumable {
            function_debug.locals.borrow_mut().insert(
                *id,
                LocalSlot {
                    variable,
                    location,
                    slot: *slot,
                },
            );
        } else {
            emitter.declare(*slot, variable, location, prologue);
        }
    }
}
