//===- debug_info.cpp - DWARF debug info emission --------------------------===//
//
// Creates proper DWARF metadata (DICompileUnit, DISubprogram, etc.) around
// the raw debug locations that MLIR's translateModuleToLLVMIR preserves from
// FileLineColLoc metadata. Without this, LLVM strips those locations because
// they aren't wrapped in a valid debug info hierarchy.
//
//===----------------------------------------------------------------------===//

#include "hew/debug_info.h"

#include "llvm/BinaryFormat/Dwarf.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

#include <filesystem>

namespace hew {

void emitDebugInfo(llvm::Module &module, const std::string &sourcePath,
                   const std::vector<size_t> &lineMap) {
  llvm::DIBuilder dib(module);

  // Split source path into directory and filename.
  std::filesystem::path p(sourcePath);
  std::string directory = p.parent_path().string();
  std::string filename = p.filename().string();

  // Create the file and compile-unit metadata.
  llvm::DIFile *diFile = dib.createFile(filename, directory);
  // The compile unit must exist for DWARF emission even though we don't
  // reference the pointer directly — DIBuilder owns it and attaches it to
  // the module metadata graph.
  (void)dib.createCompileUnit(
      llvm::dwarf::DW_LANG_C, // closest match; DWARF has no Hew language code
      diFile,
      "hew",      // producer
      false,      // isOptimized — debug builds are -O0
      "",         // flags
      0           // runtime version
  );

  // A generic subroutine type — void function with no parameter types.
  // Good enough for line-level stepping; richer types come later.
  llvm::DISubroutineType *funcTy =
      dib.createSubroutineType(dib.getOrCreateTypeArray({}));

  // Walk every function and attach DISubprogram + per-instruction locations.
  for (llvm::Function &fn : module) {
    if (fn.isDeclaration())
      continue;

    // Determine the function's start line from the first instruction that
    // already carries a debug location (set by MLIR translation).
    unsigned startLine = 1;
    for (const llvm::BasicBlock &bb : fn) {
      for (const llvm::Instruction &inst : bb) {
        if (const auto &dl = inst.getDebugLoc()) {
          startLine = dl.getLine();
          goto found_line; // break out of nested loops
        }
      }
    }
  found_line:

    // Create the subprogram.
    llvm::DISubprogram *sp = dib.createFunction(
        diFile,           // scope — the file
        fn.getName(),     // name
        fn.getName(),     // linkage name
        diFile,           // file
        startLine,        // line number
        funcTy,           // subroutine type
        startLine,        // scope line (same as start)
        llvm::DINode::FlagPrototyped,
        llvm::DISubprogram::SPFlagDefinition);

    fn.setSubprogram(sp);

    // Re-scope every instruction's debug location under this subprogram.
    for (llvm::BasicBlock &bb : fn) {
      for (llvm::Instruction &inst : bb) {
        if (const auto &dl = inst.getDebugLoc()) {
          // Instruction already has a location from MLIR — keep line/col
          // but re-scope it under our new subprogram.
          auto *newLoc = llvm::DILocation::get(
              module.getContext(), dl.getLine(), dl.getCol(), sp);
          inst.setDebugLoc(llvm::DebugLoc(newLoc));
        } else {
          // No location — assign the function's start line so the debugger
          // doesn't lose track.
          auto *defaultLoc = llvm::DILocation::get(
              module.getContext(), startLine, 0, sp);
          inst.setDebugLoc(llvm::DebugLoc(defaultLoc));
        }
      }
    }
  }

  // Finalize the debug info — builds the DWARF CU tree and attaches it.
  dib.finalize();

  // Set module-level DWARF flags so the backend emits .debug_info sections.
  // Use Warning behavior: if a flag already exists, keep it (no error).
  module.addModuleFlag(llvm::Module::Warning, "Dwarf Version", 4);
  module.addModuleFlag(llvm::Module::Warning, "Debug Info Version",
                       llvm::DEBUG_METADATA_VERSION);
}

} // namespace hew
