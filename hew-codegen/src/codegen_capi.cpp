#include "hew/codegen_capi.h"

#include "hew/codegen.h"
#include "hew/mlir/HewDialect.h"
#include "hew/mlir/HewOps.h"
#include "hew/mlir/MLIRGen.h"
#include "hew/msgpack_reader.h"

#include "mlir/Dialect/Arith/IR/Arith.h"
#include "mlir/Dialect/ControlFlow/IR/ControlFlowOps.h"
#include "mlir/Dialect/Func/IR/FuncOps.h"
#include "mlir/Dialect/LLVMIR/LLVMDialect.h"
#include "mlir/Dialect/Math/IR/Math.h"
#include "mlir/Dialect/MemRef/IR/MemRef.h"
#include "mlir/Dialect/SCF/IR/SCF.h"
#include "mlir/IR/Diagnostics.h"
#include "mlir/IR/MLIRContext.h"
#include "mlir/IR/Verifier.h"

#include "llvm/Support/raw_ostream.h"

#include <cstdlib>
#include <cstring>
#include <new>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace {

thread_local std::string lastError;

void initMLIRContext(mlir::MLIRContext &context) {
  context.loadDialect<hew::HewDialect>();
  context.loadDialect<mlir::func::FuncDialect>();
  context.loadDialect<mlir::arith::ArithDialect>();
  context.loadDialect<mlir::scf::SCFDialect>();
  context.loadDialect<mlir::memref::MemRefDialect>();
  context.loadDialect<mlir::cf::ControlFlowDialect>();
  context.loadDialect<mlir::LLVM::LLVMDialect>();
  context.loadDialect<mlir::math::MathDialect>();
}

void setLastError(std::string message) {
  lastError = std::move(message);
}

int copyTextToBuffer(const std::string &text, HewCodegenBuffer *buffer) {
  if (!buffer) {
    setLastError("text output buffer is required for textual codegen modes");
    return 1;
  }

  auto *data = static_cast<char *>(std::malloc(text.size()));
  if (text.size() != 0 && data == nullptr) {
    setLastError("failed to allocate codegen output buffer");
    return 1;
  }
  if (!text.empty())
    std::memcpy(data, text.data(), text.size());

  buffer->data = data;
  buffer->len = text.size();
  return 0;
}

std::string asString(const char *value) {
  return value ? value : "";
}

} // namespace

namespace hew::codegen_detail {

std::string formatEmitMlirVerificationFailure(mlir::ModuleOp module) {
  std::string diagnostics;
  {
    llvm::raw_string_ostream diagnosticsStream(diagnostics);
    mlir::ScopedDiagnosticHandler handler(module->getContext(), [&](mlir::Diagnostic &diag) {
      diag.print(diagnosticsStream);
      diagnosticsStream << '\n';
      return mlir::success();
    });

    if (mlir::succeeded(mlir::verify(module)))
      return {};

    diagnosticsStream.flush();
  }

  std::string message;
  llvm::raw_string_ostream messageStream(message);
  messageStream << "module verification failed while emitting MLIR\n";
  if (!diagnostics.empty())
    messageStream << diagnostics;
  messageStream << "MLIR module dump:\n";
  module->print(messageStream);
  messageStream.flush();
  return message;
}

} // namespace hew::codegen_detail

extern "C" {

int hew_codegen_compile_msgpack(const uint8_t *data, size_t size, const HewCodegenOptions *options,
                                HewCodegenBuffer *text_output) {
  lastError.clear();

  if (text_output) {
    text_output->data = nullptr;
    text_output->len = 0;
  }

  if (!data || size == 0) {
    setLastError("codegen input is empty");
    return 1;
  }
  if (!options) {
    setLastError("codegen options are missing");
    return 1;
  }

  try {
    auto program = hew::parseMsgpackAST(data, size);

    mlir::MLIRContext context;
    initMLIRContext(context);

    hew::MLIRGen mlirGen(context, asString(options->target_triple), program.source_path,
                         program.line_map);
    auto module = mlirGen.generate(program);
    if (!module) {
      setLastError("MLIR generation failed");
      return 1;
    }

    if (options->mode == HEW_CODEGEN_EMIT_MLIR) {
      if (std::string verifierFailure =
              hew::codegen_detail::formatEmitMlirVerificationFailure(module);
          !verifierFailure.empty()) {
        module->destroy();
        setLastError(std::move(verifierFailure));
        return 1;
      }

      std::string text;
      llvm::raw_string_ostream stream(text);
      module->print(stream);
      stream << "\n";
      stream.flush();
      module->destroy();
      return copyTextToBuffer(text, text_output);
    }

    hew::Codegen codegen(context);
    hew::CodegenOptions codegenOptions;
    codegenOptions.debug_info = options->debug_info != 0;
    codegenOptions.target_triple = asString(options->target_triple);
    codegenOptions.source_path = program.source_path;
    codegenOptions.line_map = program.line_map;

    if (options->mode == HEW_CODEGEN_EMIT_OBJECT) {
      if (!options->output_path || options->output_path[0] == '\0') {
        module->destroy();
        setLastError("object emission requires an output path");
        return 1;
      }

      codegenOptions.emit_object = true;
      codegenOptions.output_path = options->output_path;
      const int result = codegen.compile(module, codegenOptions);
      module->destroy();
      if (result != 0)
        setLastError("object emission failed");
      return result;
    }

    llvm::LLVMContext llvmContext;
    std::unique_ptr<llvm::Module> llvmModule;
    try {
      llvmModule = codegen.buildLLVMModule(module, codegenOptions, llvmContext);
    } catch (...) {
      module->destroy();
      throw;
    }
    module->destroy();
    if (!llvmModule) {
      setLastError("LLVM lowering failed");
      return 1;
    }

    std::string text;
    llvm::raw_string_ostream stream(text);
    llvmModule->print(stream, nullptr);
    stream.flush();
    return copyTextToBuffer(text, text_output);
  } catch (const std::exception &error) {
    setLastError(error.what());
    return 1;
  }
}

void hew_codegen_buffer_free(HewCodegenBuffer buffer) {
  std::free(buffer.data);
}

const char *hew_codegen_last_error(void) {
  return lastError.c_str();
}

} // extern "C"
