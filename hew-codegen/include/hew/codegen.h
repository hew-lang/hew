//===- codegen.h - Hew MLIR-to-native codegen pipeline ----------*- C++ -*-===//
//
// Public API for lowering Hew MLIR modules to LLVM IR, emitting object files,
// linking with libhew_rt, and optionally executing the result.
//
//===----------------------------------------------------------------------===//

#ifndef HEW_CODEGEN_H
#define HEW_CODEGEN_H

#include "mlir/IR/BuiltinOps.h"
#include "mlir/IR/MLIRContext.h"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

#include <memory>
#include <string>

namespace hew {

struct CodegenOptions {
  bool emit_llvm_ir = false;   // --emit-llvm: dump LLVM IR to stdout
  bool emit_object = false;    // --emit-obj: only compile to .o
  bool debug_info = false;     // --debug: compile at -O0 for debugger use
  std::string output_path;     // -o: output file path
  std::string runtime_lib_dir; // directory containing libhew_rt.a
  std::string target_triple;   // --target=<triple>: cross-compilation target
  int opt_level = 0;           // -O0, -O1, -O2
};

class Codegen {
public:
  explicit Codegen(mlir::MLIRContext &context);

  /// Full pipeline: MLIR module -> executable (or object file).
  /// Returns 0 on success, non-zero on error.
  int compile(mlir::ModuleOp module, const CodegenOptions &opts);

  /// Lower the MLIR module all the way to LLVM IR.
  /// When \p skipOptimization is true, the LLVM -O2 pass pipeline is skipped
  /// so that variables and control flow remain intact for debugger use.
  /// Returns nullptr on failure.
  std::unique_ptr<llvm::Module> lowerToLLVMIR(mlir::ModuleOp module, llvm::LLVMContext &llvmContext,
                                              bool skipOptimization = false);

private:
  mlir::MLIRContext &context;

  /// Lower hew dialect ops to func/arith/llvm ops.
  mlir::LogicalResult lowerHewDialect(mlir::ModuleOp module);

  /// Lower func/arith/scf/memref/cf dialects to the LLVM dialect.
  mlir::LogicalResult lowerToLLVMDialect(mlir::ModuleOp module);

  /// Emit an object file from an LLVM module.
  /// Returns 0 on success.
  int emitObjectFile(llvm::Module &module, const std::string &path,
                     const std::string &targetTriple);

  /// Link the object file with libhew_rt to produce an executable.
  /// Returns 0 on success.
  int linkExecutable(const std::string &objectPath, const std::string &outputPath,
                     const std::string &runtimeLibDir, const std::string &targetTriple);
};

} // namespace hew

#endif // HEW_CODEGEN_H
