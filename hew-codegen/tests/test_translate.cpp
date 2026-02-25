//===- test_translate.cpp - Isolate translateModuleToLLVMIR hang -----------===//
//
// Test harness to determine if translateModuleToLLVMIR hangs due to:
//   A) Context state pollution from passes
//   B) A linking/library issue
//   C) Something in the module itself
//
// Tests:
//   1. Parse known-good MLIR text -> translate (fresh context, no passes)
//   2. Build module programmatically -> run passes -> translate
//   3. Build module programmatically -> translate (no passes)
//
//===----------------------------------------------------------------------===//

#include "hew/codegen.h"
#include "hew/mlir/HewDialect.h"
#include "hew/mlir/HewOps.h"

#include "mlir/Dialect/Arith/IR/Arith.h"
#include "mlir/Dialect/ControlFlow/IR/ControlFlowOps.h"
#include "mlir/Dialect/Func/IR/FuncOps.h"
#include "mlir/Dialect/LLVMIR/LLVMDialect.h"
#include "mlir/Dialect/MemRef/IR/MemRef.h"
#include "mlir/Dialect/SCF/IR/SCF.h"

#include "mlir/Conversion/ArithToLLVM/ArithToLLVM.h"
#include "mlir/Conversion/ControlFlowToLLVM/ControlFlowToLLVM.h"
#include "mlir/Conversion/FuncToLLVM/ConvertFuncToLLVM.h"
#include "mlir/Conversion/FuncToLLVM/ConvertFuncToLLVMPass.h"
#include "mlir/Conversion/MemRefToLLVM/MemRefToLLVM.h"
#include "mlir/Conversion/Passes.h"
#include "mlir/Conversion/ReconcileUnrealizedCasts/ReconcileUnrealizedCasts.h"
#include "mlir/Conversion/SCFToControlFlow/SCFToControlFlow.h"

#include "mlir/IR/Builders.h"
#include "mlir/IR/BuiltinOps.h"
#include "mlir/IR/BuiltinTypes.h"
#include "mlir/IR/MLIRContext.h"
#include "mlir/IR/Verifier.h"
#include "mlir/Parser/Parser.h"
#include "mlir/Pass/PassManager.h"
#include "mlir/Target/LLVMIR/Dialect/Builtin/BuiltinToLLVMIRTranslation.h"
#include "mlir/Target/LLVMIR/Dialect/LLVMIR/LLVMToLLVMIRTranslation.h"
#include "mlir/Target/LLVMIR/ModuleTranslation.h"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"

#include <cassert>
#include <csignal>
#include <cstdio>

#ifdef _WIN32
#include <atomic>
#include <thread>
#include <chrono>
#else
#include <unistd.h>
#endif

#ifndef _WIN32
static volatile sig_atomic_t timed_out = 0;

static void alarm_handler(int) {
  timed_out = 1;
}
#endif

// Translate with a timeout. Returns true on success, false on hang/failure.
static bool translateWithTimeout(mlir::ModuleOp module, mlir::MLIRContext &context,
                                 int timeout_sec = 5) {
  mlir::registerLLVMDialectTranslation(context);
  mlir::registerBuiltinDialectTranslation(context);

#ifdef _WIN32
  // Windows: run translation in a thread with a timeout
  std::atomic<bool> done{false};
  std::unique_ptr<llvm::Module> result;
  llvm::LLVMContext llvmContext;

  std::thread worker([&] {
    result = mlir::translateModuleToLLVMIR(module, llvmContext);
    done = true;
  });

  auto deadline = std::chrono::steady_clock::now() + std::chrono::seconds(timeout_sec);
  while (!done && std::chrono::steady_clock::now() < deadline) {
    std::this_thread::sleep_for(std::chrono::milliseconds(100));
  }

  if (!done) {
    fprintf(stderr, "  TIMEOUT: translateModuleToLLVMIR hung!\n");
    worker.detach();
    return false;
  }
  worker.join();

  if (!result) {
    fprintf(stderr, "  FAILED: translateModuleToLLVMIR returned null\n");
    return false;
  }
#else
  timed_out = 0;
  struct sigaction sa = {};
  sa.sa_handler = alarm_handler;
  sigaction(SIGALRM, &sa, nullptr);
  alarm(timeout_sec);

  llvm::LLVMContext llvmContext;
  auto llvmModule = mlir::translateModuleToLLVMIR(module, llvmContext);

  alarm(0); // cancel alarm

  if (timed_out) {
    fprintf(stderr, "  TIMEOUT: translateModuleToLLVMIR hung!\n");
    return false;
  }

  if (!llvmModule) {
    fprintf(stderr, "  FAILED: translateModuleToLLVMIR returned null\n");
    return false;
  }
#endif

  fprintf(stderr, "  OK: translation succeeded\n");
  return true;
}

static void initContext(mlir::MLIRContext &ctx) {
  ctx.disableMultithreading();
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();
  ctx.loadDialect<mlir::arith::ArithDialect>();
  ctx.loadDialect<mlir::scf::SCFDialect>();
  ctx.loadDialect<mlir::memref::MemRefDialect>();
  ctx.loadDialect<mlir::cf::ControlFlowDialect>();
  ctx.loadDialect<mlir::LLVM::LLVMDialect>();
}

static bool hasUnrealizedConversionCast(mlir::Operation *op) {
  bool found = false;
  op->walk([&](mlir::UnrealizedConversionCastOp) { found = true; });
  return found;
}

//=== Test 0a: Minimal empty function translate ===
static bool test0a_minimal() {
  fprintf(stderr, "\n=== Test 0a: Minimal llvm.func -> translate ===\n");

  mlir::MLIRContext context;
  context.disableMultithreading();
  context.loadDialect<mlir::LLVM::LLVMDialect>();

  const char *mlirText = R"(
    module {
      llvm.func @main() {
        llvm.return
      }
    }
  )";

  auto module = mlir::parseSourceString<mlir::ModuleOp>(mlirText, &context);
  if (!module) {
    fprintf(stderr, "  FAILED: could not parse\n");
    return false;
  }

  fprintf(stderr, "  Calling translateModuleToLLVMIR...\n");
  mlir::registerLLVMDialectTranslation(context);
  mlir::registerBuiltinDialectTranslation(context);

  llvm::LLVMContext llvmContext;
  auto llvmModule = mlir::translateModuleToLLVMIR(*module, llvmContext);
  if (!llvmModule) {
    fprintf(stderr, "  FAILED: translation returned null\n");
    return false;
  }
  fprintf(stderr, "  OK\n");
  return true;
}

//=== Test 0b: fadd with disableVerification ===
static bool test0b_fadd_constants() {
  fprintf(stderr, "\n=== Test 0b: fadd (disableVerification=true) ===\n");

  mlir::MLIRContext context;
  context.disableMultithreading();
  context.loadDialect<mlir::LLVM::LLVMDialect>();

  const char *mlirText = R"(
    module {
      llvm.func @main() {
        %0 = llvm.mlir.constant(3.140000e+00 : f64) : f64
        %1 = llvm.mlir.constant(1 : i32) : i32
        %2 = llvm.mlir.constant(1.000000e+00 : f64) : f64
        %3 = llvm.fadd %2, %0 : f64
        llvm.return
      }
    }
  )";

  auto module = mlir::parseSourceString<mlir::ModuleOp>(mlirText, &context);
  if (!module) {
    fprintf(stderr, "  FAILED: could not parse\n");
    return false;
  }

  fprintf(stderr, "  Calling translateModuleToLLVMIR (noVerify)...\n");
  mlir::registerLLVMDialectTranslation(context);
  mlir::registerBuiltinDialectTranslation(context);

  llvm::LLVMContext llvmContext;
  auto llvmModule =
      mlir::translateModuleToLLVMIR(*module, llvmContext, "test", /*disableVerification=*/true);
  if (!llvmModule) {
    fprintf(stderr, "  FAILED: translation returned null\n");
    return false;
  }
  fprintf(stderr, "  OK\n");
  return true;
}

//=== Test 0c: Just f64 constant (no fadd, no i32) ===
static bool test0c_just_f64() {
  fprintf(stderr, "\n=== Test 0c: Just f64 constant -> translate ===\n");

  mlir::MLIRContext context;
  context.disableMultithreading();
  context.loadDialect<mlir::LLVM::LLVMDialect>();

  const char *mlirText = R"(
    module {
      llvm.func @main() {
        %0 = llvm.mlir.constant(3.140000e+00 : f64) : f64
        llvm.return
      }
    }
  )";

  auto module = mlir::parseSourceString<mlir::ModuleOp>(mlirText, &context);
  if (!module) {
    fprintf(stderr, "  FAILED: could not parse\n");
    return false;
  }

  fprintf(stderr, "  Calling translateModuleToLLVMIR...\n");
  mlir::registerLLVMDialectTranslation(context);
  mlir::registerBuiltinDialectTranslation(context);

  llvm::LLVMContext llvmContext;
  auto llvmModule = mlir::translateModuleToLLVMIR(*module, llvmContext);
  if (!llvmModule) {
    fprintf(stderr, "  FAILED: translation returned null\n");
    return false;
  }
  fprintf(stderr, "  OK\n");
  return true;
}

//=== Test 0d: fadd with two f64 constants (no i32) ===
static bool test0d_fadd_f64_only() {
  fprintf(stderr, "\n=== Test 0d: fadd with f64 constants only ===\n");

  mlir::MLIRContext context;
  context.disableMultithreading();
  context.loadDialect<mlir::LLVM::LLVMDialect>();

  const char *mlirText = R"(
    module {
      llvm.func @main() {
        %0 = llvm.mlir.constant(3.140000e+00 : f64) : f64
        %1 = llvm.mlir.constant(1.000000e+00 : f64) : f64
        %2 = llvm.fadd %1, %0 : f64
        llvm.return
      }
    }
  )";

  auto module = mlir::parseSourceString<mlir::ModuleOp>(mlirText, &context);
  if (!module) {
    fprintf(stderr, "  FAILED: could not parse\n");
    return false;
  }

  fprintf(stderr, "  Calling translateModuleToLLVMIR...\n");
  mlir::registerLLVMDialectTranslation(context);
  mlir::registerBuiltinDialectTranslation(context);

  llvm::LLVMContext llvmContext;
  auto llvmModule = mlir::translateModuleToLLVMIR(*module, llvmContext);
  if (!llvmModule) {
    fprintf(stderr, "  FAILED: translation returned null\n");
    return false;
  }
  fprintf(stderr, "  OK\n");
  return true;
}

//=== Test 0d2: fadd with data layout ===
static bool test0d2_fadd_with_datalayout() {
  fprintf(stderr, "\n=== Test 0d2: fadd with data layout ===\n");

  mlir::MLIRContext context;
  context.disableMultithreading();
  context.loadDialect<mlir::LLVM::LLVMDialect>();

  const char *mlirText = R"(
    module attributes {llvm.data_layout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"} {
      llvm.func @main() {
        %0 = llvm.mlir.constant(3.140000e+00 : f64) : f64
        %1 = llvm.mlir.constant(1.000000e+00 : f64) : f64
        %2 = llvm.fadd %1, %0 : f64
        llvm.return
      }
    }
  )";

  auto module = mlir::parseSourceString<mlir::ModuleOp>(mlirText, &context);
  if (!module) {
    fprintf(stderr, "  FAILED: could not parse\n");
    return false;
  }

  fprintf(stderr, "  Calling translateModuleToLLVMIR...\n");
  mlir::registerLLVMDialectTranslation(context);
  mlir::registerBuiltinDialectTranslation(context);

  llvm::LLVMContext llvmContext;
  auto llvmModule = mlir::translateModuleToLLVMIR(*module, llvmContext);
  if (!llvmModule) {
    fprintf(stderr, "  FAILED: translation returned null\n");
    return false;
  }
  fprintf(stderr, "  OK\n");
  return true;
}

//=== Test 0e: dead i32 constant alongside f64 (no fadd) ===
static bool test0e_dead_i32() {
  fprintf(stderr, "\n=== Test 0e: dead i32 constant + f64 constant ===\n");

  mlir::MLIRContext context;
  context.disableMultithreading();
  context.loadDialect<mlir::LLVM::LLVMDialect>();

  const char *mlirText = R"(
    module {
      llvm.func @main() {
        %0 = llvm.mlir.constant(3.140000e+00 : f64) : f64
        %1 = llvm.mlir.constant(1 : i32) : i32
        llvm.return
      }
    }
  )";

  auto module = mlir::parseSourceString<mlir::ModuleOp>(mlirText, &context);
  if (!module) {
    fprintf(stderr, "  FAILED: could not parse\n");
    return false;
  }

  fprintf(stderr, "  Calling translateModuleToLLVMIR...\n");
  mlir::registerLLVMDialectTranslation(context);
  mlir::registerBuiltinDialectTranslation(context);

  llvm::LLVMContext llvmContext;
  auto llvmModule = mlir::translateModuleToLLVMIR(*module, llvmContext);
  if (!llvmModule) {
    fprintf(stderr, "  FAILED: translation returned null\n");
    return false;
  }
  fprintf(stderr, "  OK\n");
  return true;
}

//=== Test 1: Parse LLVM dialect MLIR from text, translate (no passes) ===
static bool test1_parse_llvm_dialect() {
  fprintf(stderr, "\n=== Test 1: Parse LLVM dialect MLIR -> translate ===\n");

  mlir::MLIRContext context;
  initContext(context);

  const char *mlirText = R"(
    module {
      llvm.func @main() {
        %0 = llvm.mlir.constant(3.140000e+00 : f64) : f64
        %1 = llvm.mlir.constant(1 : i32) : i32
        %2 = llvm.mlir.constant(1.000000e+00 : f64) : f64
        %3 = llvm.fadd %2, %0 : f64
        llvm.return
      }
    }
  )";

  auto module = mlir::parseSourceString<mlir::ModuleOp>(mlirText, &context);
  if (!module) {
    fprintf(stderr, "  FAILED: could not parse MLIR text\n");
    return false;
  }

  return translateWithTimeout(*module, context);
}

//=== Test 2: Build arith module, run passes, translate ===
static bool test2_build_and_lower() {
  fprintf(stderr, "\n=== Test 2: Build arith module -> passes -> translate ===\n");

  mlir::MLIRContext context;
  initContext(context);

  // Build: func @main() { sitofp(1:i32) + 3.14:f64, return }
  mlir::OpBuilder builder(&context);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  auto f64Type = builder.getF64Type();
  auto i32Type = builder.getI32Type();

  auto funcType = builder.getFunctionType({}, {});
  auto funcOp = builder.create<mlir::func::FuncOp>(loc, "main", funcType);
  module.push_back(funcOp);

  auto *entryBlock = funcOp.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto cst314 = builder.create<mlir::arith::ConstantOp>(loc, builder.getF64FloatAttr(3.14));
  auto cst1 = builder.create<mlir::arith::ConstantOp>(loc, builder.getI32IntegerAttr(1));
  auto sitofp = builder.create<mlir::arith::SIToFPOp>(loc, f64Type, cst1);
  auto addf = builder.create<mlir::arith::AddFOp>(loc, cst314, sitofp);
  builder.create<mlir::func::ReturnOp>(loc);

  // Verify before passes
  if (mlir::failed(mlir::verify(module))) {
    fprintf(stderr, "  FAILED: module verification failed before passes\n");
    module.dump();
    return false;
  }

  fprintf(stderr, "  Module before passes:\n");
  module.dump();

  // Run the same pass pipeline as our codegen
  mlir::PassManager pm(&context);
  pm.addPass(mlir::createSCFToControlFlowPass());
  pm.addPass(mlir::createConvertFuncToLLVMPass());
  pm.addPass(mlir::createArithToLLVMConversionPass());
  pm.addPass(mlir::createConvertControlFlowToLLVMPass());
  pm.addPass(mlir::createFinalizeMemRefToLLVMConversionPass());
  pm.addPass(mlir::createReconcileUnrealizedCastsPass());

  if (mlir::failed(pm.run(module))) {
    fprintf(stderr, "  FAILED: pass pipeline failed\n");
    return false;
  }

  fprintf(stderr, "  Module after passes:\n");
  module.dump();

  if (hasUnrealizedConversionCast(module.getOperation())) {
    fprintf(stderr, "  FAILED: unrealized_conversion_cast remained after reconcile\n");
    module.dump();
    return false;
  }

  return translateWithTimeout(module, context);
}

//=== Test 3: Build LLVM dialect module directly, translate (no passes) ===
static bool test3_build_llvm_directly() {
  fprintf(stderr, "\n=== Test 3: Build LLVM dialect directly -> translate ===\n");

  mlir::MLIRContext context;
  initContext(context);

  mlir::OpBuilder builder(&context);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  auto f64Type = builder.getF64Type();
  auto i32Type = builder.getI32Type();

  // Build llvm.func @main() directly
  auto funcType = mlir::LLVM::LLVMFunctionType::get(mlir::LLVM::LLVMVoidType::get(&context), {});
  auto funcOp = builder.create<mlir::LLVM::LLVMFuncOp>(loc, "main", funcType);
  module.push_back(funcOp);

  auto *entryBlock = funcOp.addEntryBlock(builder);
  builder.setInsertionPointToStart(entryBlock);

  auto cst314 = builder.create<mlir::LLVM::ConstantOp>(loc, f64Type, builder.getF64FloatAttr(3.14));
  auto cst1i32 = builder.create<mlir::LLVM::ConstantOp>(loc, i32Type, builder.getI32IntegerAttr(1));
  auto cst1f64 = builder.create<mlir::LLVM::ConstantOp>(loc, f64Type, builder.getF64FloatAttr(1.0));
  auto fadd = builder.create<mlir::LLVM::FAddOp>(loc, f64Type, cst1f64, cst314);
  builder.create<mlir::LLVM::ReturnOp>(loc, mlir::ValueRange{});

  fprintf(stderr, "  Module:\n");
  module.dump();

  return translateWithTimeout(module, context);
}

//=== Test 4: Ensure codegen rejects unreconciled casts before translation ===
static bool test4_reject_unreconciled_cast() {
  fprintf(stderr, "\n=== Test 4: lowerToLLVMIR rejects unreconciled casts ===\n");

  mlir::MLIRContext context;
  initContext(context);

  mlir::OpBuilder builder(&context);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);

  auto f64Type = builder.getF64Type();
  auto funcType = builder.getFunctionType({}, {f64Type});
  auto funcOp = builder.create<mlir::func::FuncOp>(loc, "main", funcType);
  module.push_back(funcOp);

  auto *entryBlock = funcOp.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto cst1 = builder.create<mlir::arith::ConstantIntOp>(loc, 1, 32);
  auto badCast = builder.create<mlir::UnrealizedConversionCastOp>(loc, f64Type, cst1.getResult());
  builder.create<mlir::func::ReturnOp>(loc, mlir::ValueRange{badCast.getResult(0)});

  if (mlir::failed(mlir::verify(module))) {
    fprintf(stderr, "  FAILED: malformed test module\n");
    module.dump();
    return false;
  }

  hew::Codegen codegen(context);
  llvm::LLVMContext llvmContext;
  auto llvmModule = codegen.lowerToLLVMIR(module, llvmContext);
  if (llvmModule) {
    fprintf(stderr, "  FAILED: expected lowerToLLVMIR to reject unreconciled casts\n");
    return false;
  }

  fprintf(stderr, "  OK: unreconciled cast was rejected before translation\n");
  return true;
}

int main(int argc, char **argv) {
  int passed = 0;
  int failed = 0;

  // Run specific test if argument provided, otherwise all
  int testNum = (argc > 1) ? atoi(argv[1]) : -1;

  auto run = [&](int n, bool (*fn)()) {
    if (testNum >= 0 && testNum != n)
      return;
    if (fn())
      passed++;
    else
      failed++;
  };

  run(0, test0a_minimal);
  run(1, test0b_fadd_constants);
  run(2, test0c_just_f64);
  run(3, test0d_fadd_f64_only);
  run(4, test0d2_fadd_with_datalayout);
  run(5, test0e_dead_i32);
  run(20, test1_parse_llvm_dialect);
  run(21, test2_build_and_lower);
  run(22, test3_build_llvm_directly);
  run(23, test4_reject_unreconciled_cast);

  fprintf(stderr, "\n=== Results: %d passed, %d failed ===\n", passed, failed);
  return failed > 0 ? 1 : 0;
}
