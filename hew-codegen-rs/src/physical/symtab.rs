//! Tables every native Hew image carries, and the gdb auto-load entry.
//!
//! The layout of `__hew_file_table`, `__hew_site_table`, `__hew_symtab` and
//! `__hew_has_debug_info` belongs to `hew-debug-abi`; this module only places
//! the bytes it writes into the object.

use std::collections::BTreeMap;
use std::path::Path;

use hew_debug_abi::{
    FileRecord, SiteEntry, SymbolRecord, TableWriter, FILE_TABLE_SYMBOL, HAS_DEBUG_INFO_SYMBOL,
    NO_FILE, SITE_TABLE_SYMBOL, SYMTAB_SYMBOL,
};
use hew_mir::physical::{CallableId, PhysicalDebug};
use hew_mir::PhysicalModule;
use inkwell::attributes::AttributeLoc;
use inkwell::module::{Linkage, Module};
use inkwell::values::{AsValueRef, BasicValue, FunctionValue, GlobalValue, IntValue};

use super::build_info::retain;
use super::{CodegenError, CodegenResult};

/// Function attribute naming the callable a body realizes. Coroutine
/// splitting copies it to the resume, destroy and cleanup parts, so the
/// symtab names every part after the source function.
const CALLABLE_ATTRIBUTE: &str = "hew-callable";

/// Mark `function` as the body of `callable` for the symtab.
pub(super) fn tag_callable(function: FunctionValue<'_>, callable: CallableId) {
    let ctx = function.get_type().get_context();
    function.add_attribute(
        AttributeLoc::Function,
        ctx.create_string_attribute(CALLABLE_ATTRIBUTE, &callable.0.to_string()),
    );
}

/// The callable a defined function realizes, when a Hew body tags it. A tag
/// is only ever written by [`tag_callable`], so a malformed one is a codegen
/// defect.
fn tagged_callable(function: FunctionValue<'_>) -> Option<CallableId> {
    let attribute = function.get_string_attribute(AttributeLoc::Function, CALLABLE_ATTRIBUTE)?;
    let value = attribute.get_string_value().to_string_lossy();
    match value.parse() {
        Ok(id) => Some(CallableId(id)),
        Err(_) => unreachable!("tag_callable writes a decimal callable id, found {value:?}"),
    }
}

/// The table-local id of each file in the module's file table: the files in
/// span-index order, numbered from zero.
fn table_file_ids(debug: &PhysicalDebug) -> BTreeMap<u32, u32> {
    debug
        .files
        .keys()
        .enumerate()
        .map(|(index, file)| (*file, u32::try_from(index).unwrap_or(NO_FILE)))
        .collect()
}

/// How a table is visible to the linker. An executable's tables are global so
/// the runtime's weak references bind to them. A library object (a host
/// export, an embedded Hew component) keeps its tables local: several such
/// objects can share one image, and only the executable's own tables describe
/// the process.
fn table_linkage(physical: &PhysicalModule) -> Linkage {
    if physical.entry_callable.is_some() || !physical.test_entries.is_empty() {
        Linkage::External
    } else {
        Linkage::Internal
    }
}

/// A read-only table; `llvm.used` keeps it through optimization.
fn add_table<'ctx>(
    module: &Module<'ctx>,
    name: &str,
    bytes: &[u8],
    linkage: Linkage,
) -> GlobalValue<'ctx> {
    let ctx = module.get_context();
    let data = ctx.const_string(bytes, false);
    let global = module.add_global(data.get_type(), None, name);
    global.set_initializer(&data);
    global.set_constant(true);
    global.set_alignment(4);
    global.set_linkage(linkage);
    retain(module, global);
    global
}

/// Emit `__hew_file_table`, `__hew_site_table` and `__hew_has_debug_info`.
/// They are pure data, present in every build.
pub(super) fn emit_source_tables(module: &Module<'_>, physical: &PhysicalModule, has_debug: bool) {
    let debug = &physical.debug;
    let records: Vec<FileRecord<'_>> = debug
        .files
        .values()
        .map(|file| FileRecord {
            name: &file.display,
            line_starts: &file.line_starts,
            md5: file.md5,
        })
        .collect();
    let linkage = table_linkage(physical);
    add_table(
        module,
        FILE_TABLE_SYMBOL,
        &TableWriter::files(&records),
        linkage,
    );
    let file_ids = table_file_ids(debug);
    let sites: Vec<SiteEntry> = debug
        .fault_sites
        .iter()
        .map(|site| SiteEntry {
            file_id: file_ids.get(&site.file).copied().unwrap_or(NO_FILE),
            byte_offset: site.offset,
            column: debug
                .files
                .get(&site.file)
                .and_then(|file| file.column(site.offset))
                .unwrap_or(0),
        })
        .collect();
    add_table(
        module,
        SITE_TABLE_SYMBOL,
        &TableWriter::sites(&sites),
        linkage,
    );
    let ctx = module.get_context();
    // The symbol table is assembled after optimization, but fault sites need
    // its address during lowering. Replace this declaration with the final
    // table once optimized callable boundaries are known.
    if !physical.target.triple.starts_with("wasm") {
        module.add_global(ctx.i8_type(), None, SYMTAB_SYMBOL);
    }
    let flag = module.add_global(ctx.i8_type(), None, HAS_DEBUG_INFO_SYMBOL);
    flag.set_initializer(&ctx.i8_type().const_int(u64::from(has_debug), false));
    flag.set_constant(true);
    flag.set_linkage(linkage);
    retain(module, flag);
}

/// Emit `__hew_symtab` from the optimized module: one entry per surviving
/// function a Hew callable tags, in emission order. Runs after the pass
/// pipeline so inlined bodies are gone and coroutine parts exist.
///
/// Each entry's `start` is the function's distance from the table and its
/// `end` the next emitted function's, both PC-relative fixups. A trailing
/// marker function bounds the last entry.
pub(super) fn emit_symtab(module: &Module<'_>, physical: &PhysicalModule) -> CodegenResult<()> {
    let ctx = module.get_context();
    let marker = module.add_function(
        "__hew_shim_text_end",
        ctx.void_type().fn_type(&[], false),
        Some(Linkage::Internal),
    );
    let builder = ctx.create_builder();
    builder.position_at_end(ctx.append_basic_block(marker, "entry"));
    builder
        .build_return(None)
        .map_err(|error| CodegenError::Llvm(format!("symtab end marker: {error}")))?;
    let defined: Vec<FunctionValue<'_>> = module
        .get_functions()
        .filter(|function| function.count_basic_blocks() > 0)
        .collect();
    let file_ids = table_file_ids(&physical.debug);
    let mut records = Vec::new();
    let mut bounds = Vec::new();
    for (index, function) in defined.iter().enumerate() {
        let Some(callable) = tagged_callable(*function) else {
            continue;
        };
        let Some(name) = physical
            .debug
            .names
            .get(&callable)
            .filter(|name| !name.artificial)
        else {
            continue;
        };
        let (file_id, decl_line) = physical
            .debug
            .functions
            .get(&callable)
            .and_then(|attribution| {
                let file = physical.debug.files.get(&attribution.file)?;
                Some((
                    *file_ids.get(&attribution.file)?,
                    file.line(attribution.decl),
                ))
            })
            .unwrap_or((NO_FILE, 0));
        records.push(SymbolRecord {
            name: &name.text,
            file_id,
            decl_line,
        });
        // The marker is defined last, so every function has a successor.
        bounds.push((*function, defined[index + 1]));
    }
    let (bytes, slots) = TableWriter::symtab(&records);
    let i32_type = ctx.i32_type();
    let i64_type = ctx.i64_type();
    let word_count = bytes.len() / 4;
    let array_type = i32_type
        .array_type(u32::try_from(word_count).map_err(|_| {
            CodegenError::FailClosed("symtab exceeds the 32-bit table size".into())
        })?);
    let declaration = module.get_global(SYMTAB_SYMBOL);
    if let Some(declaration) = declaration {
        declaration.set_name("__hew_symtab_pending");
    }
    let table = module.add_global(array_type, None, SYMTAB_SYMBOL);
    if let Some(declaration) = declaration {
        declaration
            .as_pointer_value()
            .replace_all_uses_with(table.as_pointer_value());
        // SAFETY: all uses have been redirected to the completed table.
        unsafe { declaration.delete() };
    }
    let base = table.as_pointer_value().const_to_int(i64_type);
    let mut words: Vec<IntValue<'_>> = bytes
        .chunks_exact(4)
        .map(|word| {
            i32_type.const_int(
                u64::from(u32::from_le_bytes([word[0], word[1], word[2], word[3]])),
                false,
            )
        })
        .collect();
    for (slot, (function, next)) in slots.iter().zip(&bounds) {
        let start = function
            .as_global_value()
            .as_pointer_value()
            .const_to_int(i64_type);
        let end = next
            .as_global_value()
            .as_pointer_value()
            .const_to_int(i64_type);
        words[slot.start / 4] = start.const_sub(base).const_truncate(i32_type);
        words[slot.end / 4] = end.const_sub(base).const_truncate(i32_type);
    }
    table.set_initializer(&i32_type.const_array(&words).as_basic_value_enum());
    table.set_constant(true);
    table.set_alignment(4);
    table.set_linkage(table_linkage(physical));
    retain(module, table);
    Ok(())
}

/// gdb's `.debug_gdb_scripts` entry kind for an inline Python script.
const GDB_SCRIPT_PYTHON_TEXT: u8 = 4;

/// The inline loader gdb runs when it opens an image built with `-g`: it
/// sources the toolchain's `hew-gdb.py` when that file exists.
fn gdb_loader_script(script_dir: &Path) -> Vec<u8> {
    let script = script_dir.join("hew-gdb.py");
    // A Python string literal: escape what would end it early.
    let mut literal = String::new();
    for ch in script.to_string_lossy().chars() {
        match ch {
            '\\' => literal.push_str("\\\\"),
            '\'' => literal.push_str("\\'"),
            '\n' | '\0' => {}
            other => literal.push(other),
        }
    }
    let mut entry = vec![GDB_SCRIPT_PYTHON_TEXT];
    entry.extend_from_slice(b"hew-gdb-loader\n");
    entry.extend_from_slice(
        format!(
            "import os\nhew_script = '{literal}'\nif os.path.exists(hew_script):\n    gdb.execute('source ' + hew_script)\n"
        )
        .as_bytes(),
    );
    entry.push(0);
    entry
}

/// Name the toolchain's gdb extensions in a `.debug_gdb_scripts` section so
/// gdb loads them without a manual `source`. ELF only: gdb reads this section
/// from ELF images, and other formats have their own loaders.
pub(super) fn emit_gdb_script_section(module: &Module<'_>, script_dir: &Path, triple: &str) {
    if triple.contains("apple") || triple.contains("windows") || triple.starts_with("wasm") {
        return;
    }
    let ctx = module.get_context();
    let entry = gdb_loader_script(script_dir);
    let bytes = ctx.const_string(&entry, false);
    let global = module.add_global(bytes.get_type(), None, "__hew_gdb_script");
    global.set_initializer(&bytes);
    global.set_constant(true);
    global.set_linkage(Linkage::Internal);
    global.set_alignment(1);
    // SAFETY: the global and the static section string are live for this call.
    unsafe {
        inkwell::llvm_sys::core::LLVMSetSection(
            global.as_value_ref(),
            c".debug_gdb_scripts".as_ptr(),
        );
    }
    retain(module, global);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn gdb_loader_sources_the_script_only_when_it_exists() {
        let entry = gdb_loader_script(Path::new("/opt/hew/share/hew"));
        assert_eq!(entry[0], GDB_SCRIPT_PYTHON_TEXT);
        assert_eq!(entry.last(), Some(&0));
        let text = std::str::from_utf8(&entry[1..entry.len() - 1]).unwrap();
        assert!(text.starts_with("hew-gdb-loader\n"));
        let expected = if cfg!(windows) {
            r"hew_script = '/opt/hew/share/hew\\hew-gdb.py'"
        } else {
            "hew_script = '/opt/hew/share/hew/hew-gdb.py'"
        };
        assert!(text.contains(expected), "{text}");
        assert!(text.contains("if os.path.exists(hew_script):"));
        assert!(!text.contains('\0'));
    }

    #[test]
    fn gdb_loader_escapes_quotes_in_the_script_path() {
        let entry = gdb_loader_script(Path::new("/tmp/it's"));
        let text = std::str::from_utf8(&entry[1..entry.len() - 1]).unwrap();
        let expected = if cfg!(windows) {
            r"hew_script = '/tmp/it\'s\\hew-gdb.py'"
        } else {
            r"hew_script = '/tmp/it\'s/hew-gdb.py'"
        };
        assert!(text.contains(expected), "{text}");
    }
}
