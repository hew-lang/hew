//! Compiler provenance embedded in every native executable.
//!
//! The global carries a fixed greppable prefix so `strings <binary>` proves
//! which compiler built it. It lives in its own section and in `llvm.used`, so
//! optimization and linker dead-stripping keep it.

use inkwell::module::{Linkage, Module};
use inkwell::values::{AsValueRef, BasicValue, GlobalValue};
use inkwell::AddressSpace;

/// Prefix `strings` and the acceptance test search for.
pub const BUILD_INFO_PREFIX: &str = "HEW-BUILD-INFO: ";

/// The record text for a compiler version built for `triple`.
#[must_use]
pub fn build_info_text(version: &str, triple: &str) -> String {
    format!("{BUILD_INFO_PREFIX}hew {version} {triple}")
}

fn section_for(triple: &str) -> &'static std::ffi::CStr {
    if triple.contains("apple") {
        c"__DATA,__hewbuildinfo"
    } else if triple.contains("windows") {
        // COFF section names keep eight characters in a linked image.
        c".hewinfo"
    } else {
        c".hewbuildinfo"
    }
}

/// Emit the read-only provenance string and keep it alive through `llvm.used`.
///
pub fn emit_build_info<'ctx>(module: &Module<'ctx>, version: &str, triple: &str) {
    let ctx = module.get_context();
    let text = ctx.const_string(build_info_text(version, triple).as_bytes(), true);
    let global = module.add_global(text.get_type(), None, "hew.build.info");
    global.set_initializer(&text);
    global.set_constant(true);
    global.set_linkage(Linkage::Internal);
    // Inkwell's `set_section` rewrites Mach-O names for the host; the section
    // spelling must follow the module's target when cross-compiling.
    // SAFETY: the global and the static section string are live for this call.
    unsafe {
        inkwell::llvm_sys::core::LLVMSetSection(
            global.as_value_ref(),
            section_for(triple).as_ptr(),
        );
    }
    retain(module, global);
}

/// Add `global` to the module's single `llvm.used` array, preserving entries
/// other emitters already registered (a second `llvm.used` would be renamed and
/// ignored by LLVM).
fn retain<'ctx>(module: &Module<'ctx>, global: GlobalValue<'ctx>) {
    let pointer = module.get_context().ptr_type(AddressSpace::default());
    let mut entries = Vec::new();
    if let Some(existing) = module.get_global("llvm.used") {
        if let Some(initializer) = existing.get_initializer() {
            let array = initializer.as_value_ref();
            // SAFETY: `llvm.used` is a constant array whose operands are its
            // elements; the index stays below the operand count.
            unsafe {
                for index in 0..inkwell::llvm_sys::core::LLVMGetNumOperands(array) {
                    let element =
                        inkwell::llvm_sys::core::LLVMGetOperand(array, index.cast_unsigned());
                    entries.push(inkwell::values::PointerValue::new(element));
                }
            }
        }
        // SAFETY: nothing else references the old `llvm.used` array.
        unsafe { existing.delete() };
    }
    entries.push(global.as_pointer_value());
    let used = pointer.const_array(&entries);
    let retained = module.add_global(used.get_type(), None, "llvm.used");
    retained.set_initializer(&used.as_basic_value_enum());
    retained.set_linkage(Linkage::Appending);
    // SAFETY: the global and the static section string are live for this call.
    unsafe {
        inkwell::llvm_sys::core::LLVMSetSection(retained.as_value_ref(), c"llvm.metadata".as_ptr());
    }
}
