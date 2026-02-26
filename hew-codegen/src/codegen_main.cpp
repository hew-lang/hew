//===- codegen_main.cpp - hew-codegen: AST → object file ------------------===//
//
// Standalone codegen tool. Reads a msgpack-encoded AST from stdin or a file,
// AST from stdin or a file, runs MLIR generation and LLVM codegen, and emits
// an object file (or dumps MLIR / LLVM IR).
//
// This is the "pure codegen" entry point — it does NOT invoke any Rust
// frontend, link with a runtime library, or execute the result.
//
//===----------------------------------------------------------------------===//

#include "hew/codegen.h"
#include "hew/mlir/HewDialect.h"
#include "hew/mlir/HewOps.h"
#include "hew/mlir/MLIRGen.h"
#include "hew/msgpack_reader.h"

#include "mlir/Dialect/Arith/IR/Arith.h"
#include "mlir/Dialect/ControlFlow/IR/ControlFlowOps.h"
#include "mlir/Dialect/Func/IR/FuncOps.h"
#include "mlir/Dialect/LLVMIR/LLVMDialect.h"
#include "mlir/Dialect/MemRef/IR/MemRef.h"
#include "mlir/Dialect/SCF/IR/SCF.h"
#include "mlir/IR/MLIRContext.h"
#include "mlir/IR/Verifier.h"

#include <cstdlib>
#include <fstream>
#include <iostream>
#include <iterator>
#include <string>
#include <vector>

#ifdef _WIN32
#include <fcntl.h>
#include <io.h>
#endif

namespace {

struct Options {
  std::string input_file;
  std::string output_path;
  bool emit_mlir = false;
  bool emit_llvm = false;
  bool emit_object = false;
  bool input_json = false;
  bool debug_info = false;
  std::string target_triple;
};

void printUsage() {
  std::cerr << "Usage: hew-codegen [options] [input.msgpack]\n"
            << "  (no input file = read msgpack AST from stdin)\n"
            << "\n"
            << "Options:\n"
            << "  --emit-mlir         Dump MLIR to stdout\n"
            << "  --emit-llvm         Dump LLVM IR to stdout\n"
            << "  --emit-obj          Emit object file (default if no emit flag)\n"
            << "  --input-json        Read JSON input instead of msgpack\n"
            << "  --debug             Compile at -O0 for debugger use\n"
            << "  -o <path>           Output object file path (default: output.o)\n"
            << "  --target=<triple>   Cross-compilation target triple\n"
            << "  --help              Show this help\n";
}

auto parse_args(int argc, char *argv[]) -> Options {
  Options opts;
  std::vector<std::string> args(argv + 1, argv + argc);

  for (size_t i = 0; i < args.size(); ++i) {
    if (args[i] == "--help" || args[i] == "-h") {
      printUsage();
      std::exit(0);
    } else if (args[i] == "--emit-mlir") {
      opts.emit_mlir = true;
    } else if (args[i] == "--emit-llvm") {
      opts.emit_llvm = true;
    } else if (args[i] == "--emit-obj") {
      opts.emit_object = true;
    } else if (args[i] == "--input-json") {
      opts.input_json = true;
    } else if (args[i] == "--debug") {
      opts.debug_info = true;
    } else if (args[i].starts_with("--target=")) {
      opts.target_triple = args[i].substr(std::string("--target=").size());
    } else if (args[i] == "-o" && i + 1 < args.size()) {
      opts.output_path = args[++i];
    } else if (args[i][0] != '-') {
      opts.input_file = args[i];
    } else {
      std::cerr << "Unknown option: " << args[i] << "\n";
      printUsage();
      std::exit(1);
    }
  }

  return opts;
}

/// Set up the MLIR context with all required dialects.
void initMLIRContext(mlir::MLIRContext &context) {
  context.loadDialect<hew::HewDialect>();
  context.loadDialect<mlir::func::FuncDialect>();
  context.loadDialect<mlir::arith::ArithDialect>();
  context.loadDialect<mlir::scf::SCFDialect>();
  context.loadDialect<mlir::memref::MemRefDialect>();
  context.loadDialect<mlir::cf::ControlFlowDialect>();
  context.loadDialect<mlir::LLVM::LLVMDialect>();
}

} // namespace

auto main(int argc, char *argv[]) -> int {
  auto opts = parse_args(argc, argv);

  // Read input from stdin or file
  std::vector<uint8_t> inputData;
  if (!opts.input_file.empty()) {
    std::ifstream f(opts.input_file, std::ios::binary);
    if (!f) {
      std::cerr << "Error: could not open file: " << opts.input_file << "\n";
      return 1;
    }
    inputData = std::vector<uint8_t>(std::istreambuf_iterator<char>(f), {});
  } else {
#ifdef _WIN32
    // Windows opens stdin in text mode by default, which translates \r\n → \n
    // and treats 0x1a (Ctrl-Z) as EOF — both corrupt binary msgpack data.
    _setmode(_fileno(stdin), _O_BINARY);
#endif
    inputData = std::vector<uint8_t>(std::istreambuf_iterator<char>(std::cin), {});
  }

  if (inputData.empty()) {
    std::cerr << "Error: no input data\n";
    return 1;
  }

  // Parse AST (msgpack by default, JSON with --input-json)
  hew::ast::Program program;
  try {
    if (opts.input_json) {
      program = hew::parseJsonAST(inputData.data(), inputData.size());
    } else {
      program = hew::parseMsgpackAST(inputData.data(), inputData.size());
    }
  } catch (const std::exception &e) {
    std::cerr << "Error: " << e.what() << "\n";
    return 1;
  }

  // Set up MLIR context
  mlir::MLIRContext context;
  initMLIRContext(context);

  // Generate MLIR
  hew::MLIRGen mlirGen(context, opts.target_triple);
  auto module = mlirGen.generate(program);
  if (!module) {
    std::cerr << "Error: MLIR generation failed\n";
    return 1;
  }

  // Handle output modes
  if (opts.emit_mlir) {
    // Verify before dumping so users see valid IR
    if (mlir::failed(mlir::verify(module))) {
      std::cerr << "Error: module verification failed\n";
      module->dump();
      module->destroy();
      return 1;
    }
    module->print(llvm::outs());
    llvm::outs() << "\n";
    module->destroy();
    return 0;
  }

  if (opts.emit_llvm) {
    hew::Codegen codegen(context);
    hew::CodegenOptions codegenOpts;
    codegenOpts.emit_llvm_ir = true;
    codegenOpts.debug_info = opts.debug_info;
    codegenOpts.target_triple = opts.target_triple;
    int ret = codegen.compile(module, codegenOpts);
    module->destroy();
    return ret;
  }

  // Default: emit object file
  hew::Codegen codegen(context);
  hew::CodegenOptions codegenOpts;
  codegenOpts.emit_object = true;
  codegenOpts.debug_info = opts.debug_info;
  codegenOpts.output_path = opts.output_path.empty() ? "output.o" : opts.output_path;
  codegenOpts.target_triple = opts.target_triple;
  int ret = codegen.compile(module, codegenOpts);
  module->destroy();
  return ret;
}
