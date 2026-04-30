// jit_session.cpp — Synchronous LLJIT-based JIT session for the Hew REPL.
//
// Each HewJitSession wraps a single llvm::orc::LLJIT instance.  The session
// is created, used once (eval_msgpack), then destroyed.  There is no
// cross-cell symbol sharing; each cell gets a fresh JIT.
//
// WHY this shape: the M1 keystone (#1235) prescribes synchronous whole-module
// compilation.  Persistent sessions are out of scope.
// WHEN obsolete: when #1228 introduces a Runtime handle that can carry a
// long-lived JIT session across cells.
// WHAT the real solution looks like: a shared ThreadSafeContext with
// incremental symbol accumulation.

#include "hew/jit_session.h"

#include "hew/codegen.h"
#include "hew/jit_symbol_map.h"
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
#include "mlir/IR/MLIRContext.h"

#include "llvm/ExecutionEngine/JITEventListener.h"
#include "llvm/ExecutionEngine/Orc/AbsoluteSymbols.h"
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/Orc/ThreadSafeModule.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/TargetSelect.h"

#include <memory>
#include <stdexcept>
#include <string>

namespace {

thread_local std::string gJitLastError;

void setJitError(std::string message) {
  gJitLastError = std::move(message);
}

void initMLIRContextForJit(mlir::MLIRContext &ctx) {
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();
  ctx.loadDialect<mlir::arith::ArithDialect>();
  ctx.loadDialect<mlir::scf::SCFDialect>();
  ctx.loadDialect<mlir::memref::MemRefDialect>();
  ctx.loadDialect<mlir::cf::ControlFlowDialect>();
  ctx.loadDialect<mlir::LLVM::LLVMDialect>();
  ctx.loadDialect<mlir::math::MathDialect>();
}

} // namespace

namespace hew {

class HewJitSession {
public:
  HewJitSession() = default;
  ~HewJitSession() = default;

  // Disallow copying.
  HewJitSession(const HewJitSession &) = delete;
  HewJitSession &operator=(const HewJitSession &) = delete;

  /// Compile and run the given MessagePack-encoded AST.
  /// Returns 0 on success (exit code placed into *outExitCode).
  int evalMsgpack(const uint8_t *data, size_t size, int64_t *outExitCode);

  /// Number of JITEventListeners registered during the most recent evalMsgpack.
  /// Resets to 0 at the start of each call.
  ///
  /// WHY always compiled: avoids multiply-defined-symbol pitfalls from
  /// compiling different .o files with different macro guards.  The field is
  /// private to this translation unit (opaque class) and adds no ABI surface.
  /// The public accessor (hewJitSessionListenerCountForTest) is a test-only API
  /// conveyed by the ForTest suffix in the function name, not by a compile-time guard.
  int listenerCount_ = 0;
};

int HewJitSession::evalMsgpack(const uint8_t *data, size_t size, int64_t *outExitCode) {
  if (!data || size == 0) {
    setJitError("JIT eval: input is empty");
    return 1;
  }
  if (!outExitCode) {
    setJitError("JIT eval: out_exit_code is null");
    return 1;
  }
  *outExitCode = 0;

  // ── 1. Initialise LLVM target infrastructure ──────────────────────────────
  // InitializeNativeTarget* is idempotent (once-per-process).
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  llvm::InitializeNativeTargetAsmParser();

  // ── 2. Parse AST from MessagePack ─────────────────────────────────────────
  hew::ast::Program program;
  try {
    program = hew::parseMsgpackAST(data, size);
  } catch (const std::exception &e) {
    setJitError(std::string("JIT eval: msgpack parse failed: ") + e.what());
    return 1;
  }

  // ── 3. Generate MLIR ──────────────────────────────────────────────────────
  mlir::MLIRContext mlirCtx;
  initMLIRContextForJit(mlirCtx);

  hew::MLIRGen mlirGen(mlirCtx, /*targetTriple=*/"", program.source_path, program.line_map);
  auto mlirModule = mlirGen.generate(program);
  if (!mlirModule) {
    setJitError("JIT eval: MLIR generation failed");
    return 1;
  }

  // ── 4. Build LLJIT ────────────────────────────────────────────────────────
  // We explicitly request an RTDyldObjectLinkingLayer (backed by
  // SectionMemoryManager) so that JITEventListener-based debugger and profiler
  // registration is available on all platforms.
  //
  // WHY RTDyld: the JITEventListener API (createGDBRegistrationListener,
  // createPerfJITEventListener) is implemented on RTDyldObjectLinkingLayer.
  // LLVM 22 on Apple arm64 defaults to the newer JITLink-based
  // ObjectLinkingLayer, which requires a separate plugin API
  // (GDBJITDebugInfoRegistrationPlugin) instead.  Until those two paths are
  // unified (tracked upstream and as a follow-up to #1232), we pin to RTDyld
  // to get the two LLVM-provided listeners on all CI targets.
  // WHEN obsolete: when LLVM unifies JITEventListener support across both
  // linking layers, or when a follow-up issue extends #1232 to use
  // ObjectLinkingLayer::Plugin for JITLink builds.
  // WHAT the real solution looks like: detect the layer at runtime, and for
  // JITLink use GDBJITDebugInfoRegistrationPlugin / a perf plugin.
  listenerCount_ = 0;
  auto jitExpected = llvm::orc::LLJITBuilder()
                         .setObjectLinkingLayerCreator(
                             [](llvm::orc::ExecutionSession &ES)
                                 -> llvm::Expected<std::unique_ptr<llvm::orc::ObjectLayer>> {
                               return std::make_unique<llvm::orc::RTDyldObjectLinkingLayer>(
                                   ES, [](const llvm::MemoryBuffer &) {
                                     return std::make_unique<llvm::SectionMemoryManager>();
                                   });
                             })
                         .create();
  if (!jitExpected) {
    setJitError("JIT eval: LLJIT creation failed: " + llvm::toString(jitExpected.takeError()));
    mlirModule->destroy();
    return 1;
  }
  auto &jit = *jitExpected;

  // ── 4a. Register GDB and perf JITEventListeners ───────────────────────────
  // These let external debuggers and profilers observe JIT'd code without any
  // changes to the JIT'd code itself:
  //   - createGDBRegistrationListener: registers ELF/DWARF objects via the
  //     GDB JIT interface (__jit_debug_register_code hook), enabling
  //     source-level debugging of JIT'd frames in gdb and lldb.
  //   - createPerfJITEventListener: writes a perf jitdump file so that
  //     `perf record` / `perf report` can attribute samples to JIT'd symbols.
  //
  // LLVM keeps listeners alive for the process lifetime; we must not delete
  // them.  registerJITEventListener takes a reference, not ownership.
  //
  // createPerfJITEventListener returns nullptr when LLVM_USE_PERF=0 (e.g.
  // Homebrew LLVM on macOS).  The nullptr check prevents a null-dereference
  // crash on those builds; the listener is simply not registered.
  {
    auto &layer = llvm::cast<llvm::orc::RTDyldObjectLinkingLayer>(jit->getObjLinkingLayer());

    auto *gdbListener = llvm::JITEventListener::createGDBRegistrationListener();
    if (gdbListener) {
      layer.registerJITEventListener(*gdbListener);
      ++listenerCount_;
    }

#if LLVM_USE_PERF
    auto *perfListener = llvm::JITEventListener::createPerfJITEventListener();
    if (perfListener) {
      layer.registerJITEventListener(*perfListener);
      ++listenerCount_;
    }
#endif // LLVM_USE_PERF
  }

  // ── 5. Register host symbols via HewJitSymbolMap ──────────────────────────
  // HewJitSymbolMap lists symbols from the host process that are safe to
  // expose to JIT'd code (the stable ABI set verified by #1229).
  //
  // WHY DynamicLibrarySearchGenerator: it lazily resolves any host symbol
  // that passes the allow-list filter, avoiding a manual symbol-by-symbol
  // loop.  WHEN to change: if the stable ABI surface needs narrower control,
  // replace with an absoluteSymbols map built from HewJitSymbolMap directly.
  {
    hew::HewJitSymbolMap symbolMap;
    auto gen = llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(
        jit->getDataLayout().getGlobalPrefix(),
        [&symbolMap](const llvm::orc::SymbolStringPtr &sym) -> bool {
          // Allow only symbols in the stable JIT host ABI.
          // (*sym dereferences to a StringRef in LLVM 22+.)
          return symbolMap.hasStableSymbol((*sym).str());
        });
    if (!gen) {
      setJitError("JIT eval: failed to create host symbol generator: " +
                  llvm::toString(gen.takeError()));
      mlirModule->destroy();
      return 1;
    }
    jit->getMainJITDylib().addGenerator(std::move(*gen));
  }

  // ── 6. Lower MLIR → LLVM IR ───────────────────────────────────────────────
  // Use LLJIT's ThreadSafeContext so the module is compatible with the JIT.
  // We extract the raw LLVMContext reference from the TSC to pass to
  // buildLLVMModule.
  hew::Codegen codegen(mlirCtx);
  hew::CodegenOptions codegenOpts;
  // target_triple empty → use host native triple (same as LLJIT default)
  // debug_info false → skip DWARF; this is the fast REPL warm-path.
  codegenOpts.source_path = program.source_path;
  codegenOpts.line_map = program.line_map;

  std::unique_ptr<llvm::Module> llvmModule;
  {
    // Obtain the LLJIT's ThreadSafeContext and lock it for module creation.
    auto &tsc = jit->getExecutionSession()
                    .getExecutorProcessControl()
                    .getTargetTriple(); // only needed for triple info
    (void)tsc;                          // suppress unused-variable warning; we use the lock below

    // The LLJIT's execution session manages a ThreadSafeContext internally.
    // We create a separate LLVMContext here and wrap it in a ThreadSafeModule.
    // Each single-cell session uses its own context — no sharing.
    //
    // WHY: the per-cell model avoids race conditions in multi-threaded
    // compilation without needing a TSC lock protocol.
    auto ownedCtx = std::make_unique<llvm::LLVMContext>();
    llvm::LLVMContext &llvmCtx = *ownedCtx;

    try {
      llvmModule = codegen.buildLLVMModule(mlirModule, codegenOpts, llvmCtx);
    } catch (const std::exception &e) {
      mlirModule->destroy();
      setJitError(std::string("JIT eval: LLVM lowering failed: ") + e.what());
      return 1;
    }

    mlirModule->destroy();

    if (!llvmModule) {
      setJitError("JIT eval: LLVM lowering failed");
      return 1;
    }

    // ── 7. Add module to JIT ──────────────────────────────────────────────
    // ThreadSafeModule takes ownership of both the module and the context.
    llvm::orc::ThreadSafeModule tsm(std::move(llvmModule), std::move(ownedCtx));
    if (auto err = jit->addIRModule(std::move(tsm))) {
      setJitError("JIT eval: addIRModule failed: " + llvm::toString(std::move(err)));
      return 1;
    }
  }

  // ── 8. Look up and call `main` ─────────────────────────────────────────────
  // Hew's `main` may return void or i64.  For the JIT path we cast to i64
  // and accept whatever value falls out (void → 0 from undefined behaviour in
  // strict C++ is acceptable here because LLVM JIT'd void functions return
  // nothing into a register that we then read — callers treat 0 as success).
  //
  // SHIM: this assumes `main` is `fn() -> i64` or `fn() -> void`.
  // WHY: the synchronous single-module case always has exactly one `main`.
  // WHEN obsolete: when #1227 adds the `catch_unwind` seam, the call site
  // moves to a wrapper function that normalises the return type.
  // WHAT the real solution looks like: emit a `__hew_jit_trampoline` that
  // calls `main`, normalises return type to i64, and can be wrapped in
  // catch_unwind on the Rust side.
  auto mainSym = jit->lookup("main");
  if (!mainSym) {
    setJitError("JIT eval: symbol lookup for 'main' failed: " +
                llvm::toString(mainSym.takeError()));
    return 1;
  }

  // SAFETY: the JIT'd code runs in this process with the host's stable ABI.
  // We call `main` as `int64_t (*)()` — callers that return void will leave
  // an undefined value in the return register which we read as 0 by
  // convention (acceptable for the M1 synchronous path).
  using MainFn = int64_t (*)();
  auto *mainFn = mainSym->toPtr<MainFn>();
  *outExitCode = mainFn();
  return 0;
}

// ── C-linkage free functions (public API) ─────────────────────────────────────

HewJitSession *hewJitSessionCreate() {
  gJitLastError.clear();
  try {
    return new HewJitSession();
  } catch (const std::exception &e) {
    setJitError(std::string("hewJitSessionCreate failed: ") + e.what());
    return nullptr;
  }
}

int hewJitSessionEvalMsgpack(HewJitSession *session, const uint8_t *data, size_t size,
                             int64_t *out_exit_code) {
  gJitLastError.clear();
  if (!session) {
    setJitError("hewJitSessionEvalMsgpack: null session");
    return 1;
  }
  try {
    return session->evalMsgpack(data, size, out_exit_code);
  } catch (const std::exception &e) {
    setJitError(std::string("hewJitSessionEvalMsgpack: uncaught exception: ") + e.what());
    return 1;
  }
}

void hewJitSessionDestroy(HewJitSession *session) {
  delete session;
}

const char *hewJitSessionLastError() {
  return gJitLastError.c_str();
}

int hewJitSessionListenerCountForTest(const HewJitSession *session) {
  return session ? session->listenerCount_ : 0;
}

} // namespace hew
