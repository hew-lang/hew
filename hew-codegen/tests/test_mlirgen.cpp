//===- test_mlirgen.cpp - Tests for Hew AST-to-MLIR lowering --------------===//
//
// Verifies that the MLIRGen class correctly lowers various Hew AST constructs
// to valid MLIR operations. Each test parses a Hew source string, runs
// MLIRGen, verifies the module, and optionally prints the MLIR IR.
//
//===----------------------------------------------------------------------===//

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
#include "mlir/IR/BuiltinOps.h"
#include "mlir/IR/MLIRContext.h"
#include "mlir/IR/Verifier.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <string>
#include <vector>

#ifdef _WIN32
#include <process.h>
#define getpid _getpid
#else
#include <unistd.h>
#endif

static int tests_run = 0;
static int tests_passed = 0;

#define TEST(name)                                                                                 \
  do {                                                                                             \
    tests_run++;                                                                                   \
    printf("  test %s ... ", #name);                                                               \
  } while (0)

#define PASS()                                                                                     \
  do {                                                                                             \
    tests_passed++;                                                                                \
    printf("ok\n");                                                                                \
  } while (0)

#define FAIL(msg)                                                                                  \
  do {                                                                                             \
    printf("FAILED: %s\n", msg);                                                                   \
  } while (0)

// ---------------------------------------------------------------------------
// Helper: set up MLIR context with all required dialects
// ---------------------------------------------------------------------------
static void initContext(mlir::MLIRContext &ctx) {
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();
  ctx.loadDialect<mlir::arith::ArithDialect>();
  ctx.loadDialect<mlir::scf::SCFDialect>();
  ctx.loadDialect<mlir::memref::MemRefDialect>();
  ctx.loadDialect<mlir::cf::ControlFlowDialect>();
  ctx.loadDialect<mlir::LLVM::LLVMDialect>();
}

// ---------------------------------------------------------------------------
// Helper: look up a FuncOp by name suffix (handles module-qualified mangling)
// ---------------------------------------------------------------------------
static mlir::func::FuncOp lookupFuncBySuffix(mlir::ModuleOp module, llvm::StringRef suffix) {
  mlir::func::FuncOp found;
  module.walk([&](mlir::func::FuncOp f) {
    if (f.getName().ends_with(suffix))
      found = f;
  });
  return found;
}

// ---------------------------------------------------------------------------
// Helper: find hew CLI binary (used as frontend for parsing)
// ---------------------------------------------------------------------------
static std::string findHewCli() {
  // Prefer the HEW_CLI environment variable (set by CMake)
  if (const char *env = std::getenv("HEW_CLI"))
    return env;
  // Check relative to the test binary's likely build location
#ifdef _WIN32
  constexpr const char *hewName = "hew.exe";
#else
  constexpr const char *hewName = "hew";
#endif
  for (auto candidate : {
           std::filesystem::path("../../../target/release") / hewName,
           std::filesystem::path("../../../target/debug") / hewName,
           std::filesystem::path("../../target/release") / hewName,
           std::filesystem::path("../../target/debug") / hewName,
       }) {
    if (std::filesystem::exists(candidate))
      return std::filesystem::canonical(candidate).string();
  }
  // Fall back to PATH
  return "hew";
}

// ---------------------------------------------------------------------------
// Helper: parse source -> AST (via hew CLI) -> MLIR module
// Writes source to a temp file, invokes `hew build --emit-json`, deserializes
// JSON AST. Returns a null ModuleOp on failure, prints errors.
// ---------------------------------------------------------------------------
static mlir::ModuleOp generateMLIR(mlir::MLIRContext &ctx, const std::string &source,
                                   bool dumpIR = false) {
  // Write source to a temp file
  std::string tmpPath = (std::filesystem::temp_directory_path() /
                         ("test_mlirgen_" + std::to_string(getpid()) + ".hew"))
                            .string();
  {
    std::ofstream tmp(tmpPath);
    if (!tmp) {
      printf("  Failed to write temp file %s\n", tmpPath.c_str());
      return {};
    }
    tmp << source;
  }

  // Invoke hew build --emit-json via popen
  static std::string hewCli = findHewCli();
#ifdef _WIN32
  // On Windows _popen() routes through cmd.exe /c. The CRT quotes the
  // whole command string for CreateProcess, and cmd.exe then strips the
  // first and last '"' — mangling any interior quotes.  Avoid quoting
  // paths here; temp_directory_path() and HEW_CLI are space-free.
  std::string cmd = hewCli + " build " + tmpPath + " --emit-json 2>NUL";
#else
  std::string cmd = "\"" + hewCli + "\" build \"" + tmpPath + "\" --emit-json 2>/dev/null";
#endif
#ifdef _WIN32
  FILE *pipe = _popen(cmd.c_str(), "r");
#else
  FILE *pipe = popen(cmd.c_str(), "r");
#endif
  if (!pipe) {
    printf("  Failed to start hew CLI\n");
    std::filesystem::remove(tmpPath);
    return {};
  }

  std::vector<uint8_t> astData;
  uint8_t buf[4096];
  while (size_t n = fread(buf, 1, sizeof(buf), pipe)) {
    astData.insert(astData.end(), buf, buf + n);
  }

#ifdef _WIN32
  int rc = _pclose(pipe);
#else
  int rc = pclose(pipe);
#endif
  std::filesystem::remove(tmpPath);

  if (rc != 0) {
    printf("  hew CLI failed (exit %d)\n", rc);
    return {};
  }

  if (astData.empty()) {
    printf("  hew CLI produced no output\n");
    return {};
  }

  hew::ast::Program program;
  try {
    program = hew::parseJsonAST(astData.data(), astData.size());
  } catch (const std::exception &e) {
    printf("  Failed to parse JSON AST: %s\n", e.what());
    return {};
  }

  hew::MLIRGen mlirGen(ctx);
  auto module = mlirGen.generate(program);

  if (module && dumpIR) {
    printf("\n--- MLIR ---\n");
    module.dump();
    printf("--- End ---\n");
  }

  return module;
}

// ============================================================================
// Test: Simple add function
// ============================================================================
static void test_simple_add() {
  TEST(simple_add);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn add(a: i32, b: i32) -> i32 {
    a + b
}
fn main() -> i32 {
    add(5, 3)
}
  )",
                             /*dumpIR=*/true);

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  // Check that add and main functions exist
  auto addFunc = lookupFuncBySuffix(module, "F3add");
  auto mainFunc = module.lookupSymbol<mlir::func::FuncOp>("main");
  if (!addFunc) {
    FAIL("add function not found");
    module.getOperation()->destroy();
    return;
  }
  if (!mainFunc) {
    FAIL("main function not found");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Fibonacci function
// ============================================================================
static void test_fibonacci() {
  TEST(fibonacci);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn fibonacci(n: i32) -> i32 {
    if n <= 1 {
        n
    } else {
        fibonacci(n - 1) + fibonacci(n - 2)
    }
}
fn main() -> i32 {
    let n = 10;
    let result = fibonacci(n);
    println(result);
    0
}
  )",
                             /*dumpIR=*/true);

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  // Check that fibonacci and main exist
  auto fib = lookupFuncBySuffix(module, "F9fibonacci");
  auto main = module.lookupSymbol<mlir::func::FuncOp>("main");
  if (!fib) {
    FAIL("fibonacci function not found");
    module.getOperation()->destroy();
    return;
  }
  if (!main) {
    FAIL("main function not found");
    module.getOperation()->destroy();
    return;
  }

  // Check fibonacci signature: (i32) -> i32
  if (fib.getNumArguments() != 1) {
    FAIL("fibonacci should have 1 argument");
    module.getOperation()->destroy();
    return;
  }
  if (fib.getResultTypes().size() != 1) {
    FAIL("fibonacci should have 1 result");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Mutable variables (var)
// ============================================================================
static void test_mutable_variables() {
  TEST(mutable_variables);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn main() -> i32 {
    let x = 5;
    var y: i32 = 10;
    y = y + x;
    y
}
  )",
                             /*dumpIR=*/true);

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  // Check that memref operations were generated
  bool hasAlloca = false;
  bool hasStore = false;
  bool hasLoad = false;
  module.walk([&](mlir::Operation *op) {
    if (llvm::isa<mlir::memref::AllocaOp>(op))
      hasAlloca = true;
    if (llvm::isa<mlir::memref::StoreOp>(op))
      hasStore = true;
    if (llvm::isa<mlir::memref::LoadOp>(op))
      hasLoad = true;
  });

  if (!hasAlloca) {
    FAIL("expected memref.alloca for var declaration");
    module.getOperation()->destroy();
    return;
  }
  if (!hasStore) {
    FAIL("expected memref.store for var assignment");
    module.getOperation()->destroy();
    return;
  }
  if (!hasLoad) {
    FAIL("expected memref.load for var read");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Print operation
// ============================================================================
static void test_print() {
  TEST(print);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn main() -> i32 {
    println(42);
    0
}
  )",
                             /*dumpIR=*/true);

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  // Check that hew.print was generated
  bool hasPrint = false;
  module.walk([&](hew::PrintOp op) {
    hasPrint = true;
    // Check that newline is true
    if (!op.getNewline()) {
      FAIL("expected newline=true for println");
    }
  });

  if (!hasPrint) {
    FAIL("expected hew.print operation");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: While loop
// ============================================================================
static void test_while_loop() {
  TEST(while_loop);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn main() -> i32 {
    var sum: i32 = 0;
    var i: i32 = 1;
    while i <= 10 {
        sum = sum + i;
        i = i + 1;
    }
    sum
}
  )",
                             /*dumpIR=*/true);

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  // Check that scf.while was generated
  bool hasWhile = false;
  module.walk([&](mlir::scf::WhileOp) { hasWhile = true; });

  if (!hasWhile) {
    FAIL("expected scf.while operation");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: If/else expression
// ============================================================================
static void test_if_else_expr() {
  TEST(if_else_expr);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn abs(x: i32) -> i32 {
    if x < 0 {
        0 - x
    } else {
        x
    }
}
fn main() -> i32 {
    abs(-5)
}
  )",
                             /*dumpIR=*/true);

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  // Check that scf.if was generated
  bool hasIf = false;
  module.walk([&](mlir::scf::IfOp) { hasIf = true; });

  if (!hasIf) {
    FAIL("expected scf.if operation");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Arithmetic operations
// ============================================================================
static void test_arithmetic() {
  TEST(arithmetic);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn compute(a: i32, b: i32) -> i32 {
    let sum = a + b;
    let diff = a - b;
    let prod = a * b;
    let quot = a / b;
    let rem = a % b;
    sum + diff + prod + quot + rem
}
fn main() -> i32 {
    compute(10, 3)
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  // Check for various arith ops
  bool hasAdd = false, hasSub = false, hasMul = false;
  bool hasDiv = false, hasRem = false;
  module.walk([&](mlir::Operation *op) {
    if (llvm::isa<mlir::arith::AddIOp>(op))
      hasAdd = true;
    if (llvm::isa<mlir::arith::SubIOp>(op))
      hasSub = true;
    if (llvm::isa<mlir::arith::MulIOp>(op))
      hasMul = true;
    if (llvm::isa<mlir::arith::DivSIOp>(op))
      hasDiv = true;
    if (llvm::isa<mlir::arith::RemSIOp>(op))
      hasRem = true;
  });

  if (!hasAdd || !hasSub || !hasMul || !hasDiv || !hasRem) {
    FAIL("expected all 5 arithmetic operations");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Comparison operations
// ============================================================================
static void test_comparisons() {
  TEST(comparisons);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn test_cmp(a: i32, b: i32) -> i32 {
    if a < b { 1 } else { 0 }
}
fn main() -> i32 {
    test_cmp(3, 5)
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  bool hasCmp = false;
  module.walk([&](mlir::arith::CmpIOp) { hasCmp = true; });

  if (!hasCmp) {
    FAIL("expected arith.cmpi operation");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Return statement
// ============================================================================
static void test_return_stmt() {
  TEST(return_stmt);

  mlir::MLIRContext ctx;
  initContext(ctx);
  // Phase 1: test explicit return in a simple function (not inside scf.if).
  // Early return inside structured control flow (scf.if) requires more complex
  // lowering (return-flag pattern or cf.cond_br) which will be added later.
  auto module = generateMLIR(ctx, R"(
fn identity(x: i32) -> i32 {
    return x;
}
fn add_one(x: i32) -> i32 {
    return x + 1;
}
fn main() -> i32 {
    identity(add_one(5))
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  // Check for func.return operations
  int returnCount = 0;
  module.walk([&](mlir::func::ReturnOp) { returnCount++; });

  // Should have at least 3 returns: one per function
  if (returnCount < 3) {
    FAIL("expected at least 3 func.return operations");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Logical operators (short-circuit)
// ============================================================================
static void test_logical_ops() {
  TEST(logical_ops);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn test_and(a: bool, b: bool) -> bool {
    a && b
}
fn test_or(a: bool, b: bool) -> bool {
    a || b
}
fn main() -> i32 {
    0
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  // Short-circuit && and || should use scf.if
  int ifCount = 0;
  module.walk([&](mlir::scf::IfOp) { ifCount++; });

  // We expect at least 2 scf.if ops (one for &&, one for ||)
  if (ifCount < 2) {
    FAIL("expected at least 2 scf.if ops for short-circuit logic");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Unary operators
// ============================================================================
static void test_unary_ops() {
  TEST(unary_ops);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn negate(x: i32) -> i32 {
    0 - x
}
fn main() -> i32 {
    negate(5)
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  bool hasSub = false;
  module.walk([&](mlir::arith::SubIOp) { hasSub = true; });

  if (!hasSub) {
    FAIL("expected arith.subi for negation");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Compound assignment
// ============================================================================
static void test_compound_assignment() {
  TEST(compound_assignment);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn main() -> i32 {
    var x: i32 = 10;
    x += 5;
    x -= 2;
    x *= 3;
    x
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  // Should have add, sub, mul for compound assignments
  bool hasAdd = false, hasSub = false, hasMul = false;
  module.walk([&](mlir::Operation *op) {
    if (llvm::isa<mlir::arith::AddIOp>(op))
      hasAdd = true;
    if (llvm::isa<mlir::arith::SubIOp>(op))
      hasSub = true;
    if (llvm::isa<mlir::arith::MulIOp>(op))
      hasMul = true;
  });

  if (!hasAdd || !hasSub || !hasMul) {
    FAIL("expected add, sub, mul for compound assignments");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Multiple functions calling each other
// ============================================================================
static void test_function_calls() {
  TEST(function_calls);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn double(x: i32) -> i32 {
    x * 2
}
fn add_doubled(a: i32, b: i32) -> i32 {
    double(a) + double(b)
}
fn main() -> i32 {
    add_doubled(3, 4)
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  // Check for func.call operations (stdlib may add some extra calls)
  int callCount = 0;
  module.walk([&](mlir::func::CallOp op) {
    auto callee = op.getCallee();
    // Only count calls to user functions (not runtime/stdlib)
    if (!callee.starts_with("hew_"))
      callCount++;
  });

  // Should have at least 3 user calls: double(a), double(b), add_doubled(3,4)
  if (callCount < 3) {
    FAIL("expected at least 3 func.call operations");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Void function (no return type)
// ============================================================================
static void test_void_function() {
  TEST(void_function);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn greet() {
    println(42);
}
fn main() -> i32 {
    greet();
    0
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  auto greet = lookupFuncBySuffix(module, "F5greet");
  if (!greet) {
    FAIL("greet function not found");
    module.getOperation()->destroy();
    return;
  }
  if (greet.getResultTypes().size() != 0) {
    FAIL("greet should have no result types");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Builtin Result constructors omit explicit payload_positions
// ============================================================================
static void test_result_constructors_without_payload_positions() {
  TEST(result_constructors_without_payload_positions);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn main() -> i32 {
    Ok(7);
    Err(9);
    0
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  int resultConstructCount = 0;
  bool hasExplicitPayloadPositions = false;
  module.walk([&](hew::EnumConstructOp op) {
    if (op.getEnumName() != "__Result")
      return;
    resultConstructCount++;
    if (op.getPayloadPositions())
      hasExplicitPayloadPositions = true;
  });

  if (resultConstructCount < 2) {
    FAIL("expected at least 2 Result enum constructions");
    module.getOperation()->destroy();
    return;
  }
  if (hasExplicitPayloadPositions) {
    FAIL("Result constructors should not require payload_positions");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Unresolved named type fails MLIR generation
// ============================================================================
static void test_unresolved_named_type_fails() {
  TEST(unresolved_named_type_fails);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn takes_unknown(x: MissingType) -> i32 {
    0
}
fn main() -> i32 {
    takes_unknown(1)
}
  )");

  if (module) {
    FAIL("expected MLIR generation failure for unresolved named type");
    module.getOperation()->destroy();
    return;
  }

  PASS();
}

// ============================================================================
// Test: Wire encode uses heap-owned buffer
// ============================================================================
static void test_wire_encode_uses_heap_buffer() {
  TEST(wire_encode_uses_heap_buffer);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
wire type Packet {
    id: i32 @1;
    value: i32 @2;
}
fn main() -> i32 {
    0
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  auto encodeFn = module.lookupSymbol<mlir::func::FuncOp>("Packet_encode");
  if (!encodeFn) {
    FAIL("Packet_encode function not found");
    module.getOperation()->destroy();
    return;
  }

  bool hasBufNew = false;
  bool hasBufInit = false;
  bool hasStackAlloca = false;
  encodeFn.walk([&](hew::RuntimeCallOp op) {
    auto callee = op.getCallee().str();
    if (callee == "hew_wire_buf_new")
      hasBufNew = true;
    if (callee == "hew_wire_buf_init")
      hasBufInit = true;
  });
  encodeFn.walk([&](mlir::LLVM::AllocaOp) { hasStackAlloca = true; });

  if (!hasBufNew) {
    FAIL("expected hew_wire_buf_new call in wire encode");
    module.getOperation()->destroy();
    return;
  }
  if (hasBufInit) {
    FAIL("wire encode should not initialize a stack buffer");
    module.getOperation()->destroy();
    return;
  }
  if (hasStackAlloca) {
    FAIL("wire encode should not stack-allocate hew_wire_buf");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Mixed-payload wire enum uses ABI-safe payload layout
// ============================================================================
static void test_wire_enum_mixed_payload_layout() {
  TEST(wire_enum_mixed_payload_layout);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
wire enum Mixed {
    Int(i32);
    Big(i64);
    Text(String);
    Unit;
}

wire type Packet {
    id: i32 @1;
    note: String @2;
}

fn main() -> i32 {
    let _a = Int(7);
    let _b = Big(7000000000);
    let _c = Text("hello");
    let _d = Unit;
    0
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  // Mixed wire enums should not break wire type encode/decode/json/yaml helpers.
  for (const char *fnName : {"Packet_encode", "Packet_decode", "Packet_to_json", "Packet_from_json",
                             "Packet_to_yaml", "Packet_from_yaml"}) {
    if (!module.lookupSymbol<mlir::func::FuncOp>(fnName)) {
      FAIL("expected wire helper function to be generated");
      module.getOperation()->destroy();
      return;
    }
  }

  bool sawVariant[4] = {false, false, false, false};
  bool checkedLayout = false;
  bool badPayloadPos = false;

  module.walk([&](hew::EnumConstructOp op) {
    if (op.getEnumName() != "Mixed")
      return;

    auto variantIdx = op.getVariantIndex();
    if (variantIdx >= 4)
      return;
    sawVariant[variantIdx] = true;

    if (!checkedLayout) {
      auto structTy = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(op.getResult().getType());
      if (!structTy || structTy.getBody().size() != 4 ||
          !mlir::isa<mlir::IntegerType>(structTy.getBody()[0]) ||
          !mlir::isa<mlir::IntegerType>(structTy.getBody()[1]) ||
          !mlir::isa<mlir::IntegerType>(structTy.getBody()[2]) ||
          (!mlir::isa<hew::StringRefType>(structTy.getBody()[3]) &&
           !mlir::isa<mlir::LLVM::LLVMPointerType>(structTy.getBody()[3]))) {
        badPayloadPos = true;
      }
      checkedLayout = true;
    }

    auto positions = op.getPayloadPositions();
    if (variantIdx == 1) {
      if (!positions || positions->size() != 1 ||
          mlir::cast<mlir::IntegerAttr>((*positions)[0]).getInt() != 2) {
        badPayloadPos = true;
      }
    } else if (variantIdx == 2) {
      if (!positions || positions->size() != 1 ||
          mlir::cast<mlir::IntegerAttr>((*positions)[0]).getInt() != 3) {
        badPayloadPos = true;
      }
    }
  });

  if (!checkedLayout || !sawVariant[0] || !sawVariant[1] || !sawVariant[2] || !sawVariant[3]) {
    FAIL("missing Mixed enum variant constructions");
    module.getOperation()->destroy();
    return;
  }
  if (badPayloadPos) {
    FAIL("Mixed wire enum payload layout/positions are incorrect");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Mixed-payload wire enum match extracts payloads from per-variant slots
// ============================================================================
static void test_wire_enum_mixed_payload_match_positions() {
  TEST(wire_enum_mixed_payload_match_positions);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
wire enum Mixed {
    Int(i32);
    Big(i64);
    Text(String);
    Unit;
}

fn main() -> i32 {
    let m = Big(9);
    match m {
        Big(x) if x > 0 => 1,
        Text(s) => 2,
        Int(_) => 3,
        Unit => 4
    }
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  bool sawBigField = false;
  bool sawTextField = false;
  bool sawUnexpectedField = false;

  module.walk([&](hew::EnumExtractPayloadOp op) {
    auto fieldIdx = static_cast<int64_t>(op.getFieldIndex());
    if (fieldIdx == 2) {
      sawBigField = true;
    } else if (fieldIdx == 3) {
      sawTextField = true;
    } else {
      sawUnexpectedField = true;
    }
  });

  if (!sawBigField || !sawTextField) {
    FAIL("missing mixed-payload match extraction slots");
    module.getOperation()->destroy();
    return;
  }
  if (sawUnexpectedField) {
    FAIL("mixed-payload match extracted payload from wrong slot");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Unresolved generic substitution type fails MLIR generation
// ============================================================================
static void test_unresolved_generic_substitution_type_fails() {
  TEST(unresolved_generic_substitution_type_fails);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn identity<T>(x: T) -> T {
    x
}
fn main() -> i32 {
    let _ = identity<MissingType>(42);
    0
}
  )");

  if (module) {
    FAIL("expected MLIR generation failure for unresolved generic substitution type");
    module.getOperation()->destroy();
    return;
  }

  PASS();
}

// ============================================================================
// Main
// ============================================================================

int main() {
  printf("=== Hew MLIRGen Tests ===\n");

  test_simple_add();
  test_fibonacci();
  test_mutable_variables();
  test_print();
  test_while_loop();
  test_if_else_expr();
  test_arithmetic();
  test_comparisons();
  test_return_stmt();
  test_logical_ops();
  test_unary_ops();
  test_compound_assignment();
  test_function_calls();
  test_void_function();
  test_result_constructors_without_payload_positions();
  test_unresolved_named_type_fails();
  test_wire_encode_uses_heap_buffer();
  test_wire_enum_mixed_payload_layout();
  test_wire_enum_mixed_payload_match_positions();
  test_unresolved_generic_substitution_type_fails();

  printf("\n%d/%d tests passed.\n", tests_passed, tests_run);
  return (tests_passed == tests_run) ? 0 : 1;
}
