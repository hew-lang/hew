//===- debug_info.h - DWARF debug info emission -----------------*- C++ -*-===//
//
// Attaches DWARF debug metadata (DICompileUnit, DISubprogram, etc.) to an
// LLVM module produced by MLIR-to-LLVM-IR translation. Without this wrapper,
// LLVM strips the raw debug locations that translateModuleToLLVMIR preserves
// from MLIR FileLineColLoc metadata.
//
//===----------------------------------------------------------------------===//

#ifndef HEW_DEBUG_INFO_H
#define HEW_DEBUG_INFO_H

#include "llvm/IR/Module.h"

#include <string>
#include <vector>

namespace hew {

/// Attach DWARF debug info to an LLVM module produced by MLIR translation.
///
/// Creates a DICompileUnit, DIFile, and DISubprograms for every non-declaration
/// function in the module. Instructions that already have debug locations (from
/// MLIR FileLineColLoc) are re-scoped under the correct DISubprogram; those
/// without locations get a default location at the function's start line.
///
/// After this call the module carries complete DWARF metadata and LLVM will
/// emit .debug_info / .debug_line sections when writing an object file.
void emitDebugInfo(llvm::Module &module, const std::string &sourcePath,
                   const std::vector<size_t> &lineMap);

} // namespace hew

#endif // HEW_DEBUG_INFO_H
