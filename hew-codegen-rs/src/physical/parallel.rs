//! Parallel ELF object generation after whole-module optimization.

mod resources;

use std::collections::{BTreeSet, HashMap};
use std::path::{Path, PathBuf};
use std::time::Instant;

use hew_debug_abi::SymbolSlots;
use inkwell::context::Context;
use inkwell::llvm_sys::prelude::LLVMModuleRef;
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::Module;
use inkwell::targets::{FileType, TargetMachine};
use inkwell::values::FunctionValue;
use object::{Object, ObjectSection, ObjectSymbol, RelocationFlags, RelocationTarget, SymbolKind};

use crate::llvm::{
    target_machine_for_triple_with_opt_level, CodegenError, CodegenResult, LlvmResultExt, OptLevel,
};

const PARTITIONS: usize = 8;
const MIN_FUNCTIONS: usize = 64;
const MIN_BLOCKS: u64 = 32 * 1024;
const LOCAL_PREFIX: &str = "__hew_split_local_";

pub(super) struct Emission {
    snapshot: MemoryBuffer<'static>,
    workers: usize,
    objcopy: &'static str,
}

impl Emission {
    pub(super) fn prepare(module: &Module, functions: &[FunctionValue]) -> Option<Self> {
        if functions.len() < MIN_FUNCTIONS
            || functions
                .iter()
                .map(|function| u64::from(function.count_basic_blocks()))
                .sum::<u64>()
                < MIN_BLOCKS
        {
            return None;
        }
        // SAFETY: this query reads how the linked LLVM library was built.
        if unsafe { inkwell::llvm_sys::core::LLVMIsMultithreaded() } == 0 {
            return None;
        }
        let budget = resources::Budget::read()?;
        if !tool_available("ld.lld") {
            return None;
        }
        let objcopy = ["llvm-objcopy", "objcopy"]
            .into_iter()
            .find(|tool| tool_available(tool))?;
        let snapshot = module.write_bitcode_to_memory();
        let workers = budget.workers(snapshot.get_size())?;
        Some(Self {
            snapshot,
            workers,
            objcopy,
        })
    }
}

fn tool_available(name: &str) -> bool {
    std::process::Command::new(name)
        .arg("--version")
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .is_ok_and(|status| status.success())
}

unsafe extern "C" {
    fn hewLLVMSplitObjectModule(
        module: LLVMModuleRef,
        count: u32,
        slots: *const u32,
        rows: usize,
        local_prefix: *const u8,
        prefix_size: usize,
        output: *mut std::ffi::c_void,
        take: unsafe extern "C" fn(*mut std::ffi::c_void, u32, *const u8, usize),
    ) -> bool;
}

unsafe extern "C" fn copy_partition(
    output: *mut std::ffi::c_void,
    index: u32,
    data: *const u8,
    len: usize,
) {
    // SAFETY: split() owns the destination until the synchronous callback returns.
    let partitions = unsafe { &mut *output.cast::<[Option<Vec<u8>>; PARTITIONS]>() };
    if let Some(part) = partitions.get_mut(index as usize) {
        // SAFETY: LLVM keeps this bitcode buffer alive during the callback.
        *part = Some(unsafe { std::slice::from_raw_parts(data, len) }.to_vec());
    }
}

fn split(snapshot: &MemoryBuffer, slots: &[SymbolSlots]) -> CodegenResult<Vec<Vec<u8>>> {
    let context = Context::create();
    let module = Module::parse_bitcode_from_buffer(snapshot, &context)
        .llvm_ctx("parse snapshot for object partitioning")?;
    let indices = slots
        .iter()
        .flat_map(|slot| [slot.start / 4, slot.end / 4])
        .map(u32::try_from)
        .collect::<Result<Vec<_>, _>>()
        .map_err(|_| {
            CodegenError::FailClosed("image symbol table exceeds 32-bit offsets".into())
        })?;
    let mut parts: [Option<Vec<u8>>; PARTITIONS] = std::array::from_fn(|_| None);
    // SAFETY: the LLVM module is isolated from callers and the callback and
    // array are valid for this synchronous call. The bridge may mutate it.
    let accepted = unsafe {
        hewLLVMSplitObjectModule(
            module.as_mut_ptr(),
            PARTITIONS as u32,
            indices.as_ptr(),
            slots.len(),
            LOCAL_PREFIX.as_ptr(),
            LOCAL_PREFIX.len(),
            (&mut parts as *mut [Option<Vec<u8>>; PARTITIONS]).cast(),
            copy_partition,
        )
    };
    if accepted {
        if let Some(parts) = parts.into_iter().collect() {
            return Ok(parts);
        }
    }
    Err(CodegenError::FailClosed(
        "LLVM could not partition the image table into text runs".into(),
    ))
}

// The relocatable link yields one .text run. Validate the actual machine
// function ranges before allowing a split build to replace ordinary output.
fn validate_text_ranges(path: &Path, slots: &[SymbolSlots]) -> CodegenResult<()> {
    let invalid =
        || CodegenError::FailClosed("split native object changed a machine function range".into());
    let bytes = std::fs::read(path)?;
    let object = object::File::parse(bytes.as_slice())
        .llvm_ctx("read the split native object's text symbols")?;
    if object.symbols().any(|symbol| {
        (symbol.is_global() || symbol.is_weak())
            && symbol
                .name()
                .is_ok_and(|name| name.starts_with(LOCAL_PREFIX))
    }) {
        return Err(CodegenError::FailClosed(
            "split native object exposes a private symbol".into(),
        ));
    }
    let text = object.section_by_name(".text").ok_or_else(invalid)?.index();
    // Unlabelled helpers may live in a different section, such as LLVM's
    // cold fault stub. Both bounds of every labelled function still have
    // to resolve within the joined text run, where their order is fixed.
    let function_starts: BTreeSet<_> = object
        .symbols()
        .filter(|symbol| {
            symbol.kind() == SymbolKind::Text
                && symbol.section_index() == Some(text)
                && symbol.size() != 0
        })
        .map(|symbol| symbol.address())
        .collect();
    let table = object.symbol_by_name("__hew_symtab").ok_or_else(invalid)?;
    let section = object
        .section_by_index(table.section_index().ok_or_else(invalid)?)
        .llvm_ctx("read the split native object's image table section")?;
    let table_offset = table
        .address()
        .checked_sub(section.address())
        .ok_or_else(invalid)?;
    let fixups: HashMap<_, _> = section.relocations().collect();
    let find_bound = |slot: usize| -> CodegenResult<(u64, u64)> {
        let offset = u64::try_from(slot).map_err(|_| invalid())?;
        let location = table_offset.checked_add(offset).ok_or_else(invalid)?;
        let relocation = fixups.get(&location).ok_or_else(invalid)?;
        if relocation.flags()
            != (RelocationFlags::Elf {
                r_type: object::elf::R_X86_64_PC32,
            })
            || i64::try_from(offset).ok() != Some(relocation.addend())
        {
            return Err(invalid());
        }
        let RelocationTarget::Symbol(symbol) = relocation.target() else {
            return Err(invalid());
        };
        let symbol = object
            .symbol_by_index(symbol)
            .llvm_ctx("read an image range symbol")?;
        if symbol.kind() != SymbolKind::Text
            || symbol.section_index() != Some(text)
            || symbol.size() == 0
        {
            return Err(invalid());
        }
        Ok((symbol.address(), symbol.size()))
    };
    for slots in slots {
        let (start, size) = find_bound(slots.start)?;
        let (end, _) = find_bound(slots.end)?;
        if start.checked_add(size).is_none_or(|limit| limit > end)
            || function_starts
                .range((std::ops::Bound::Excluded(start), std::ops::Bound::Unbounded))
                .next()
                .copied()
                != Some(end)
        {
            return Err(invalid());
        }
    }
    Ok(())
}

fn emit_parts(
    parts: &[Vec<u8>],
    triple: &str,
    paths: &[PathBuf],
    workers: usize,
) -> CodegenResult<()> {
    let chunk_size = parts.len().div_ceil(workers);
    std::thread::scope(|scope| -> CodegenResult<()> {
        let handles = parts
            .chunks(chunk_size)
            .zip(paths.chunks(chunk_size))
            .map(|(partition_group, path_group)| {
                std::thread::Builder::new().spawn_scoped(scope, move || -> CodegenResult<()> {
                    let machine = target_machine_for_triple_with_opt_level(triple, OptLevel::O2)?;
                    for (bytes, path) in partition_group.iter().zip(path_group) {
                        let context = Context::create();
                        let buffer = MemoryBuffer::create_from_memory_range_copy(bytes, "part");
                        let module = Module::parse_bitcode_from_buffer(&buffer, &context)
                            .llvm_ctx("parse a split native partition")?;
                        machine
                            .write_to_file(&module, FileType::Object, path)
                            .llvm_ctx("write a split native object")?;
                    }
                    Ok(())
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        for handle in handles {
            handle.join().map_err(|_| {
                CodegenError::FailClosed("split native object worker panicked".into())
            })??;
        }
        Ok(())
    })
}

fn measure(phase: &str, start: Instant) {
    if std::env::var_os("HEW_MEASURE_TIMINGS").is_some() {
        eprintln!(
            "hew measure: {phase} {:.3} ms",
            start.elapsed().as_secs_f64() * 1_000.0
        );
    }
}

impl Emission {
    pub(super) fn emit_with_fallback(
        self,
        slots: &[SymbolSlots],
        machine: &TargetMachine,
        triple: &str,
        path: &Path,
    ) -> CodegenResult<()> {
        if let Err(error) = self.emit(slots, triple, path) {
            if std::env::var_os("HEW_MEASURE_TIMINGS").is_some() {
                eprintln!("hew measure: parallel native emission fallback ({error})");
            }
            let context = Context::create();
            let module = Module::parse_bitcode_from_buffer(&self.snapshot, &context)
                .llvm_ctx("parse the monolithic native object fallback")?;
            machine
                .write_to_file(&module, FileType::Object, path)
                .llvm_ctx("write the monolithic native object fallback")?;
        }
        Ok(())
    }

    fn emit(&self, slots: &[SymbolSlots], triple: &str, path: &Path) -> CodegenResult<()> {
        let start = Instant::now();
        let parts = split(&self.snapshot, slots)?;
        measure("split native bitcode", start);
        let dir = tempfile::Builder::new().prefix("hew-codegen-").tempdir()?;
        let paths: Vec<_> = (0..parts.len())
            .map(|index| dir.path().join(format!("part-{index}.o")))
            .collect();
        let start = Instant::now();
        emit_parts(&parts, triple, &paths, self.workers)?;
        measure("split native object emission", start);
        drop(parts);

        let start = Instant::now();
        let joined = dir.path().join("joined.o");
        run_tool(
            std::process::Command::new("ld.lld")
                .arg("-r")
                .arg("-o")
                .arg(&joined)
                .args(&paths),
            "join split objects",
        )?;
        run_tool(
            std::process::Command::new(self.objcopy)
                .arg("--wildcard")
                .arg(format!("--localize-symbol={LOCAL_PREFIX}*"))
                .arg(&joined),
            "restore private object symbols",
        )?;
        validate_text_ranges(&joined, slots)?;
        std::fs::copy(joined, path)?;
        measure("join split native objects", start);
        Ok(())
    }
}

fn run_tool(tool: &mut std::process::Command, operation: &str) -> CodegenResult<()> {
    let output = tool.output()?;
    if !output.status.success() {
        return Err(CodegenError::Link(format!(
            "{operation}: {}",
            String::from_utf8_lossy(&output.stderr)
        )));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn split_object_keeps_private_symbols_local_at_the_native_link() {
        let Some(objcopy) = ["llvm-objcopy", "objcopy"]
            .into_iter()
            .find(|tool| tool_available(tool))
        else {
            eprintln!("skipped: no objcopy on PATH");
            return;
        };
        if !tool_available("ld.lld") {
            eprintln!("skipped: no ld.lld on PATH");
            return;
        }
        let triple = "x86_64-unknown-linux-gnu";
        let machine = target_machine_for_triple_with_opt_level(triple, OptLevel::O2).unwrap();
        let context = Context::create();
        let parse = |name, ir: &[u8]| {
            let buffer = MemoryBuffer::create_from_memory_range_copy(ir, name);
            let module = context.create_module_from_ir(buffer).unwrap();
            module.set_triple(&machine.get_triple());
            module.set_data_layout(&machine.get_target_data().get_data_layout());
            module
        };
        let module = parse("image", br#"
            @__hew_symtab = constant [2 x i32] [
              i32 trunc (i64 sub (i64 ptrtoint (ptr @entry to i64), i64 ptrtoint (ptr @__hew_symtab to i64)) to i32),
              i32 trunc (i64 sub (i64 ptrtoint (ptr @helper to i64), i64 ptrtoint (ptr @__hew_symtab to i64)) to i32)
            ]
            define i32 @entry(i32 %n) {
              %value = call i32 @helper(i32 %n)
              ret i32 %value
            }
            define internal i32 @helper(i32 %n) noinline {
              %value = add i32 %n, 42
              ret i32 %value
            }
        "#);
        let promoted = format!("{LOCAL_PREFIX}0");
        let foreign = parse(
            "native",
            format!(
                "define i32 @{promoted}() {{ ret i32 17 }}\ndefine i32 @helper() {{ ret i32 24 }}"
            )
            .as_bytes(),
        );
        let dir = tempfile::tempdir().unwrap();
        let program = dir.path().join("program.o");
        let native = dir.path().join("native.o");
        let linked = dir.path().join("linked.o");
        let emission = Emission {
            snapshot: module.write_bitcode_to_memory(),
            workers: 2,
            objcopy,
        };
        // Deliberately exercise split emission even on a memory-limited test
        // host. A monolithic fallback cannot satisfy this regression test.
        emission
            .emit(&[SymbolSlots { start: 0, end: 4 }], triple, &program)
            .unwrap();
        machine
            .write_to_file(&foreign, FileType::Object, &native)
            .unwrap();
        run_tool(
            std::process::Command::new("ld.lld")
                .arg("-r")
                .arg("-o")
                .arg(&linked)
                .arg(&program)
                .arg(&native),
            "link the compiled image with a separate native object",
        )
        .unwrap();
        let bytes = std::fs::read(linked).unwrap();
        let object = object::File::parse(bytes.as_slice()).unwrap();
        let matching: Vec<_> = object
            .symbols()
            .filter(|symbol| symbol.name().is_ok_and(|name| name == promoted))
            .collect();
        assert_eq!(matching.len(), 2);
        assert_eq!(
            matching.iter().filter(|symbol| symbol.is_global()).count(),
            1
        );
    }
}
