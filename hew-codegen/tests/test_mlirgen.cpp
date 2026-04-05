//===- test_mlirgen.cpp - Tests for Hew AST-to-MLIR lowering --------------===//
//
// Verifies that the MLIRGen class correctly lowers various Hew AST constructs
// to valid MLIR operations. Each test parses a Hew source string, runs
// MLIRGen, verifies the module, and optionally prints the MLIR IR.
//
//===----------------------------------------------------------------------===//

#include "hew/codegen.h"
#include "hew/mlir/HewDialect.h"
#include "hew/mlir/HewOps.h"
#include "hew/mlir/MLIRGen.h"
#include "hew/msgpack_reader.h"
#include "test_utils.h"

#include "mlir/Dialect/Arith/IR/Arith.h"
#include "mlir/Dialect/ControlFlow/IR/ControlFlowOps.h"
#include "mlir/Dialect/Func/IR/FuncOps.h"
#include "mlir/Dialect/LLVMIR/LLVMDialect.h"
#include "mlir/Dialect/MemRef/IR/MemRef.h"
#include "mlir/Dialect/SCF/IR/SCF.h"
#include "mlir/IR/BuiltinOps.h"
#include "mlir/IR/MLIRContext.h"
#include "mlir/IR/Verifier.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"

#include <cassert>
#include <cstdint>
#include <cstdio>
#include <utility>

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

static int countCallsByCallee(mlir::Operation *op, llvm::StringRef callee) {
  int count = 0;
  op->walk([&](mlir::func::CallOp call) {
    if (call.getCallee() == callee)
      count++;
  });
  return count;
}

static int countRuntimeCallsByCallee(mlir::Operation *op, llvm::StringRef callee) {
  int count = 0;
  op->walk([&](hew::RuntimeCallOp call) {
    if (call.getCallee().str() == callee)
      count++;
  });
  return count;
}

static int countDropOpsByDropFn(mlir::Operation *op, llvm::StringRef dropFn, bool isUserDrop) {
  int count = 0;
  op->walk([&](hew::DropOp drop) {
    if (drop.getDropFn() == dropFn && drop.getIsUserDrop() == isUserDrop)
      count++;
  });
  return count;
}

static int countLLVMCallsByCallee(llvm::Function *function, llvm::StringRef callee) {
  if (!function)
    return 0;

  int count = 0;
  for (auto &block : *function) {
    for (auto &inst : block) {
      auto *call = llvm::dyn_cast<llvm::CallBase>(&inst);
      auto *called = call ? call->getCalledFunction() : nullptr;
      if (called && called->getName() == callee)
        count++;
    }
  }
  return count;
}

enum class RuntimePrintKind : uint64_t {
  I32 = 0,
  I64 = 1,
  F64 = 2,
  Bool = 3,
  Str = 4,
  U32 = 5,
  U64 = 6,
};

static llvm::CallBase *findLLVMCallByCallee(llvm::Function *function, llvm::StringRef callee) {
  if (!function)
    return nullptr;

  for (auto &block : *function) {
    for (auto &inst : block) {
      auto *call = llvm::dyn_cast<llvm::CallBase>(&inst);
      auto *called = call ? call->getCalledFunction() : nullptr;
      if (called && called->getName() == callee)
        return call;
    }
  }
  return nullptr;
}

static bool llvmCallHasConstIntArg(llvm::CallBase *call, unsigned argIndex, uint64_t expected) {
  auto *value = call ? llvm::dyn_cast<llvm::ConstantInt>(call->getArgOperand(argIndex)) : nullptr;
  return value && value->getZExtValue() == expected;
}

static int countPanicOps(mlir::Operation *op) {
  int count = 0;
  op->walk([&](hew::PanicOp) { count++; });
  return count;
}

/// Count both hew.panic ops and hew.runtime_call @hew_panic ops so that
/// tests are agnostic to which form MLIRGen uses in different contexts.
static int countAllPanics(mlir::Operation *op) {
  int count = countPanicOps(op);
  op->walk([&](hew::RuntimeCallOp call) {
    if (call.getCallee().str() == "hew_panic")
      count++;
  });
  return count;
}

static int countSelectAddOps(mlir::Operation *op) {
  int count = 0;
  op->walk([&](hew::SelectAddOp) { count++; });
  return count;
}

static int countResultfulIfOps(mlir::Operation *op) {
  int count = 0;
  op->walk([&](mlir::scf::IfOp ifOp) {
    if (ifOp->getNumResults() > 0)
      count++;
  });
  return count;
}

static size_t countSubstringOccurrences(llvm::StringRef haystack, llvm::StringRef needle) {
  if (needle.empty())
    return 0;

  size_t count = 0;
  size_t pos = 0;
  while ((pos = haystack.find(needle, pos)) != llvm::StringRef::npos) {
    ++count;
    pos += needle.size();
  }
  return count;
}

static std::unique_ptr<hew::ast::Spanned<hew::ast::Expr>> makeExpr(hew::ast::Expr expr,
                                                                   hew::ast::Span span = {0, 0}) {
  expr.span = span;
  return std::make_unique<hew::ast::Spanned<hew::ast::Expr>>(
      hew::ast::Spanned<hew::ast::Expr>{std::move(expr), span});
}

static hew::ast::Program
buildResultfulStmtMatchProgram(std::unique_ptr<hew::ast::Spanned<hew::ast::Expr>> firstBody,
                               std::unique_ptr<hew::ast::Spanned<hew::ast::Expr>> secondBody,
                               bool secondArmWildcard) {
  using namespace hew::ast;

  const Span span{0, 0};

  auto mkNamedType = [&](llvm::StringRef name) -> Spanned<TypeExpr> {
    TypeExpr ty;
    ty.kind = TypeNamed{name.str(), std::nullopt};
    return {std::move(ty), span};
  };

  auto mkBoolPattern = [&](bool value) -> Spanned<Pattern> {
    Pattern pattern;
    pattern.kind = PatLiteral{Literal(LitBool{value})};
    return {std::move(pattern), span};
  };

  auto mkWildcardPattern = [&]() -> Spanned<Pattern> {
    Pattern pattern;
    pattern.kind = PatWildcard{};
    return {std::move(pattern), span};
  };

  MatchArm firstArm;
  firstArm.pattern = mkBoolPattern(true);
  firstArm.guard = nullptr;
  firstArm.body = std::move(firstBody);

  MatchArm secondArm;
  secondArm.pattern = secondArmWildcard ? mkWildcardPattern() : mkBoolPattern(false);
  secondArm.guard = nullptr;
  secondArm.body = std::move(secondBody);

  Expr scrutineeExpr;
  scrutineeExpr.kind = ExprIdentifier{"flag"};
  scrutineeExpr.span = span;

  StmtMatch matchStmt;
  matchStmt.scrutinee = {std::move(scrutineeExpr), span};
  matchStmt.arms.push_back(std::move(firstArm));
  matchStmt.arms.push_back(std::move(secondArm));

  Stmt matchStmtWrapper;
  matchStmtWrapper.kind = std::move(matchStmt);
  matchStmtWrapper.span = span;

  FnDecl fnDecl;
  fnDecl.is_async = false;
  fnDecl.is_generator = false;
  fnDecl.visibility = Visibility::Pub;
  fnDecl.is_pure = false;
  fnDecl.name = "main";
  fnDecl.return_type = mkNamedType("i64");

  Param flagParam;
  flagParam.name = "flag";
  flagParam.ty = mkNamedType("bool");
  flagParam.is_mutable = false;
  fnDecl.params.push_back(std::move(flagParam));
  fnDecl.body.stmts.push_back(
      std::make_unique<Spanned<Stmt>>(Spanned<Stmt>{std::move(matchStmtWrapper), span}));

  Program program;
  program.items.push_back({Item{std::move(fnDecl)}, span});
  return program;
}

static bool allSelectAddsReturnI32(mlir::Operation *op) {
  bool ok = true;
  op->walk([&](hew::SelectAddOp add) {
    ok = ok && add->getNumResults() == 1 && add->getResult(0).getType().isInteger(32);
  });
  return ok;
}

// ---------------------------------------------------------------------------
// Helper: parse source -> TypedProgram msgpack (via hew CLI).
// Writes source to a temp file, invokes `hew build --emit-msgpack -o`, then
// deserializes the msgpack payload with the production reader.
// ---------------------------------------------------------------------------
static bool loadProgramFromSource(const std::string &source, hew::ast::Program &program) {
  // Write source to a temp file
  std::string tmpPath = (std::filesystem::temp_directory_path() /
                         ("test_mlirgen_" + std::to_string(getpid()) + ".hew"))
                            .string();
  {
    std::ofstream tmp(tmpPath);
    if (!tmp) {
      printf("  Failed to write temp file %s\n", tmpPath.c_str());
      return false;
    }
    tmp << source;
  }

  std::string astPath = (std::filesystem::temp_directory_path() /
                         ("test_mlirgen_" + std::to_string(getpid()) + ".msgpack"))
                            .string();

  // Invoke hew build --emit-msgpack -o <file>
  static std::string hewCli = findHewCli();
#ifdef _WIN32
  std::string cmd = "\"\"" + hewCli + "\" build \"" + tmpPath + "\" -o \"" + astPath +
                    "\" --emit-msgpack 2>NUL\"";
#else
  std::string cmd = "\"" + hewCli + "\" build \"" + tmpPath + "\" -o \"" + astPath +
                    "\" --emit-msgpack 2>/dev/null";
#endif
  int rc = std::system(cmd.c_str());
  std::filesystem::remove(tmpPath);
  if (std::filesystem::exists(astPath)) {
    std::ifstream astFile(astPath, std::ios::binary);
    std::vector<uint8_t> astData(std::istreambuf_iterator<char>(astFile), {});
    std::filesystem::remove(astPath);

    if (rc != 0) {
      printf("  hew CLI failed (exit %d)\n", rc);
      return false;
    }

    if (astData.empty()) {
      printf("  hew CLI produced no output\n");
      return false;
    }

    try {
      program = hew::parseMsgpackAST(astData.data(), astData.size());
    } catch (const std::exception &e) {
      printf("  Failed to parse msgpack AST: %s\n", e.what());
      return false;
    }

    return true;
  }

  if (rc != 0) {
    printf("  hew CLI failed (exit %d)\n", rc);
  } else {
    printf("  hew CLI produced no msgpack file\n");
  }
  std::filesystem::remove(astPath);
  return false;
}

// ---------------------------------------------------------------------------
// Helper: lower a TypedProgram to MLIR.
// ---------------------------------------------------------------------------
static mlir::ModuleOp generateMLIR(mlir::MLIRContext &ctx, const hew::ast::Program &program,
                                   bool dumpIR = false) {
  hew::MLIRGen mlirGen(ctx);
  auto module = mlirGen.generate(program);

  if (module && dumpIR) {
    printf("\n--- MLIR ---\n");
    module.dump();
    printf("--- End ---\n");
  }

  return module;
}

// ---------------------------------------------------------------------------
// Helper: parse source -> TypedProgram msgpack (via hew CLI) -> MLIR module.
// ---------------------------------------------------------------------------
static mlir::ModuleOp generateMLIR(mlir::MLIRContext &ctx, const std::string &source,
                                   bool dumpIR = false) {
  hew::ast::Program program;
  if (!loadProgramFromSource(source, program))
    return {};
  return generateMLIR(ctx, program, dumpIR);
}

static hew::ast::Program makeDiscardedBlockLikeBadTailProgram(bool useUnsafe) {
  using namespace hew::ast;

  uint64_t nextSpan = 900000000000ULL;
  auto mkSpan = [&]() -> Span {
    auto start = nextSpan;
    nextSpan += 8;
    return {start, start + 1};
  };
  auto mkType = [&](const std::string &name) -> Spanned<TypeExpr> {
    TypeExpr typeExpr;
    typeExpr.kind = TypeNamed{name, std::nullopt};
    return {std::move(typeExpr), mkSpan()};
  };
  auto mkExpr = [&](auto node) -> std::unique_ptr<Spanned<Expr>> {
    Expr expr;
    expr.kind = std::move(node);
    expr.span = mkSpan();
    return std::make_unique<Spanned<Expr>>(Spanned<Expr>{std::move(expr), mkSpan()});
  };
  auto mkInt = [&](int64_t value) {
    ExprLiteral lit;
    lit.lit = LitInteger{value};
    return mkExpr(std::move(lit));
  };
  auto mkPrintCall = [&]() {
    ExprCall call;
    call.function = mkExpr(ExprIdentifier{"println"});
    call.type_args = std::nullopt;
    call.args.push_back(CallArgPositional{mkInt(1)});
    call.is_tail_call = false;
    return mkExpr(std::move(call));
  };
  auto mkBadBinary = [&]() {
    ExprBinary bin;
    bin.left = mkPrintCall();
    bin.op = BinaryOp::Add;
    bin.right = mkInt(2);
    return mkExpr(std::move(bin));
  };

  Block innerBlock;
  innerBlock.trailing_expr = mkBadBinary();

  auto outerSpan = mkSpan();
  Expr outerExpr;
  outerExpr.span = outerSpan;
  if (useUnsafe)
    outerExpr.kind = ExprUnsafe{std::move(innerBlock)};
  else
    outerExpr.kind = ExprBlock{std::move(innerBlock)};

  StmtExpression exprStmt;
  exprStmt.expr = Spanned<Expr>{std::move(outerExpr), outerSpan};

  Stmt stmt;
  stmt.kind = std::move(exprStmt);

  FnDecl mainFn;
  mainFn.name = "main";
  mainFn.return_type = mkType("int");
  mainFn.body.stmts.push_back(
      std::make_unique<Spanned<Stmt>>(Spanned<Stmt>{std::move(stmt), mkSpan()}));
  mainFn.body.trailing_expr = mkInt(0);

  Item item;
  item.kind = std::move(mainFn);

  Program program;
  program.items.push_back({std::move(item), mkSpan()});
  return program;
}

enum class StatementStyleMatchArmBadBodyKind {
  DirectExpr,
  BlockTailIf,
  BlockTailMatch,
};

static hew::ast::Program
makeStatementStyleMatchArmBadBodyProgram(StatementStyleMatchArmBadBodyKind kind) {
  using namespace hew::ast;

  uint64_t nextSpan = 900000001000ULL;
  auto mkSpan = [&]() -> Span {
    auto start = nextSpan;
    nextSpan += 8;
    return {start, start + 1};
  };
  auto mkType = [&](const std::string &name) -> Spanned<TypeExpr> {
    TypeExpr typeExpr;
    typeExpr.kind = TypeNamed{name, std::nullopt};
    return {std::move(typeExpr), mkSpan()};
  };
  auto mkExpr = [&](auto node) -> std::unique_ptr<Spanned<Expr>> {
    Expr expr;
    expr.kind = std::move(node);
    expr.span = mkSpan();
    return std::make_unique<Spanned<Expr>>(Spanned<Expr>{std::move(expr), mkSpan()});
  };
  auto takeExpr = [&](std::unique_ptr<Spanned<Expr>> expr) -> Spanned<Expr> {
    return std::move(*expr);
  };
  auto mkStmt = [&](auto node) -> std::unique_ptr<Spanned<Stmt>> {
    Stmt stmt;
    stmt.kind = std::move(node);
    return std::make_unique<Spanned<Stmt>>(Spanned<Stmt>{std::move(stmt), mkSpan()});
  };
  auto mkPattern = [&](bool value) -> Spanned<Pattern> {
    Pattern pattern;
    pattern.kind = PatLiteral{LitBool{value}};
    return {std::move(pattern), mkSpan()};
  };
  auto mkInt = [&](int64_t value) {
    ExprLiteral lit;
    lit.lit = LitInteger{value};
    return mkExpr(std::move(lit));
  };
  auto mkBool = [&](bool value) -> Spanned<Expr> {
    Expr expr;
    expr.kind = ExprLiteral{LitBool{value}};
    expr.span = mkSpan();
    return {std::move(expr), mkSpan()};
  };
  auto mkPrintCall = [&](int64_t value) {
    ExprCall call;
    call.function = mkExpr(ExprIdentifier{"println"});
    call.type_args = std::nullopt;
    call.args.push_back(CallArgPositional{mkInt(value)});
    call.is_tail_call = false;
    return mkExpr(std::move(call));
  };
  auto mkBadBinary = [&]() {
    ExprBinary bin;
    bin.left = mkPrintCall(1);
    bin.op = BinaryOp::Add;
    bin.right = mkInt(2);
    return mkExpr(std::move(bin));
  };

  MatchArm trueArm;
  trueArm.pattern = mkPattern(true);
  switch (kind) {
  case StatementStyleMatchArmBadBodyKind::DirectExpr:
    trueArm.body = mkBadBinary();
    break;
  case StatementStyleMatchArmBadBodyKind::BlockTailIf: {
    StmtIf ifStmt;
    ifStmt.condition = takeExpr(mkBadBinary());
    ifStmt.then_block.trailing_expr = mkInt(10);

    ElseBlock elseBlock;
    elseBlock.is_if = false;
    elseBlock.block = Block{};
    elseBlock.block->trailing_expr = mkInt(20);
    ifStmt.else_block = std::move(elseBlock);

    Block outerBlock;
    outerBlock.stmts.push_back(mkStmt(std::move(ifStmt)));
    trueArm.body = mkExpr(ExprBlock{std::move(outerBlock)});
    break;
  }
  case StatementStyleMatchArmBadBodyKind::BlockTailMatch: {
    MatchArm nestedTrueArm;
    nestedTrueArm.pattern = mkPattern(true);
    nestedTrueArm.body = mkInt(10);

    MatchArm nestedFalseArm;
    nestedFalseArm.pattern = mkPattern(false);
    nestedFalseArm.body = mkInt(20);

    StmtMatch nestedMatch;
    nestedMatch.scrutinee = takeExpr(mkBadBinary());
    nestedMatch.arms.push_back(std::move(nestedTrueArm));
    nestedMatch.arms.push_back(std::move(nestedFalseArm));

    Block outerBlock;
    outerBlock.stmts.push_back(mkStmt(std::move(nestedMatch)));
    trueArm.body = mkExpr(ExprBlock{std::move(outerBlock)});
    break;
  }
  }

  MatchArm falseArm;
  falseArm.pattern = mkPattern(false);
  falseArm.body = mkPrintCall(0);

  StmtMatch matchStmt;
  matchStmt.scrutinee = mkBool(true);
  matchStmt.arms.push_back(std::move(trueArm));
  matchStmt.arms.push_back(std::move(falseArm));

  Stmt stmt;
  stmt.kind = std::move(matchStmt);

  FnDecl mainFn;
  mainFn.name = "main";
  mainFn.return_type = mkType("int");
  mainFn.body.stmts.push_back(
      std::make_unique<Spanned<Stmt>>(Spanned<Stmt>{std::move(stmt), mkSpan()}));
  mainFn.body.trailing_expr = mkInt(0);

  Item item;
  item.kind = std::move(mainFn);

  Program program;
  program.items.push_back({std::move(item), mkSpan()});
  return program;
}

// ---------------------------------------------------------------------------
// Helper: simulate the frontend's TypeDecl-based wire-enum path in MLIRGen.
// ---------------------------------------------------------------------------
static bool desugarWireEnumToTypeDecl(hew::ast::Program &program, const std::string &enumName) {
  for (auto &item : program.items) {
    auto *wireDecl = std::get_if<hew::ast::WireDecl>(&item.value.kind);
    if (!wireDecl || wireDecl->kind != hew::ast::WireDeclKind::Enum || wireDecl->name != enumName)
      continue;

    hew::ast::TypeDecl typeDecl;
    typeDecl.visibility = wireDecl->visibility;
    typeDecl.kind = hew::ast::TypeDeclKind::Enum;
    typeDecl.name = wireDecl->name;
    typeDecl.body.reserve(wireDecl->variants.size());
    for (auto &variant : wireDecl->variants)
      typeDecl.body.push_back(hew::ast::TypeBodyItem{
          hew::ast::TypeBodyVariant{std::move(variant)},
      });

    hew::ast::WireMetadata metadata;
    metadata.json_case = wireDecl->json_case;
    metadata.yaml_case = wireDecl->yaml_case;
    metadata.version = wireDecl->version;
    metadata.min_version = wireDecl->min_version;
    typeDecl.wire = std::move(metadata);

    item.value.kind = std::move(typeDecl);
    return true;
  }

  return {};
}

// ============================================================================
// Test: Simple add function
// ============================================================================
static void test_simple_add() {
  TEST(simple_add);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn add(a: int, b: int) -> int {
    a + b
}
fn main() -> int {
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
fn fibonacci(n: int) -> int {
    if n <= 1 {
        n
    } else {
        fibonacci(n - 1) + fibonacci(n - 2)
    }
}
fn main() -> int {
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

  // Check fibonacci signature: (int) -> int
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
fn main() -> int {
    let x = 5;
    var y: int = 10;
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
// Test: println lowers to newline=true and the newline-print runtime call.
// ============================================================================
static void test_print() {
  TEST(print);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn main() -> int {
    println(42);
    0
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  // Walk collects observable facts; decisions and FAIL/PASS live outside the
  // lambda so that a check failure is not silently swallowed by the lambda
  // return and then masked by an unconditional PASS() at the end.
  bool hasPrint = false;
  bool newlineOk = true;
  module.walk([&](hew::PrintOp op) {
    hasPrint = true;
    if (!op.getNewline())
      newlineOk = false;
  });

  if (!hasPrint) {
    FAIL("expected hew.print operation");
    module.getOperation()->destroy();
    return;
  }
  if (!newlineOk) {
    FAIL("expected newline=true for println");
    module.getOperation()->destroy();
    return;
  }

  hew::Codegen codegen(ctx);
  hew::CodegenOptions opts;
  llvm::LLVMContext llvmContext;
  auto llvmModule = codegen.buildLLVMModule(module, opts, llvmContext);
  module.getOperation()->destroy();

  if (!llvmModule) {
    FAIL("LLVM lowering failed for println");
    return;
  }

  auto *mainFn = llvmModule->getFunction("main");
  if (!mainFn) {
    FAIL("main function not found in lowered LLVM module");
    return;
  }

  if (countLLVMCallsByCallee(mainFn, "hew_print_value") != 1 ||
      countLLVMCallsByCallee(mainFn, "hew_println_i64") != 0 ||
      countLLVMCallsByCallee(mainFn, "hew_print_i64") != 0) {
    FAIL("expected println lowering to call only hew_print_value");
    return;
  }

  auto *printCall = findLLVMCallByCallee(mainFn, "hew_print_value");
  if (!llvmCallHasConstIntArg(printCall, 0, static_cast<uint64_t>(RuntimePrintKind::I64)) ||
      !llvmCallHasConstIntArg(printCall, 2, 1)) {
    FAIL("expected println lowering to pass I64 kind with newline=true");
    return;
  }

  PASS();
}

// ============================================================================
// Test: print (no newline) lowers to newline=false and the non-newline runtime
// call. Checking both directions keeps newline-lowering regressions from hiding
// behind an attribute-only assertion hole.
// ============================================================================
static void test_print_no_newline() {
  TEST(print_no_newline);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn main() -> int {
    print(42);
    0
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  bool hasPrint = false;
  bool newlineWrong = false;
  module.walk([&](hew::PrintOp op) {
    hasPrint = true;
    if (op.getNewline())
      newlineWrong = true;
  });

  if (!hasPrint) {
    FAIL("expected hew.print operation");
    module.getOperation()->destroy();
    return;
  }
  if (newlineWrong) {
    FAIL("expected newline=false for print (not println)");
    module.getOperation()->destroy();
    return;
  }

  hew::Codegen codegen(ctx);
  hew::CodegenOptions opts;
  llvm::LLVMContext llvmContext;
  auto llvmModule = codegen.buildLLVMModule(module, opts, llvmContext);
  module.getOperation()->destroy();

  if (!llvmModule) {
    FAIL("LLVM lowering failed for print");
    return;
  }

  auto *mainFn = llvmModule->getFunction("main");
  if (!mainFn) {
    FAIL("main function not found in lowered LLVM module");
    return;
  }

  if (countLLVMCallsByCallee(mainFn, "hew_print_value") != 1 ||
      countLLVMCallsByCallee(mainFn, "hew_print_i64") != 0 ||
      countLLVMCallsByCallee(mainFn, "hew_println_i64") != 0) {
    FAIL("expected print lowering to call only hew_print_value");
    return;
  }

  auto *printCall = findLLVMCallByCallee(mainFn, "hew_print_value");
  if (!llvmCallHasConstIntArg(printCall, 0, static_cast<uint64_t>(RuntimePrintKind::I64)) ||
      !llvmCallHasConstIntArg(printCall, 2, 0)) {
    FAIL("expected print lowering to pass I64 kind with newline=false");
    return;
  }

  PASS();
}

// ============================================================================
// Test: print lowering routes scalar/string kinds through the generic runtime
// dispatcher, including narrow integer promotion and f32 -> f64 widening.
// ============================================================================
static void test_print_runtime_dispatch_kinds() {
  TEST(print_runtime_dispatch_kinds);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn main() -> int {
    let small: i8 = -7;
    let wide: u16 = 42;
    let huge: u64 = 99;
    let ratio: f32 = 3.5;
    let precise: f64 = 4.5;
    println(small);
    println(wide);
    println(huge);
    println(ratio);
    println(precise);
    println(true);
    println("ok");
    0
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  hew::Codegen codegen(ctx);
  hew::CodegenOptions opts;
  llvm::LLVMContext llvmContext;
  auto llvmModule = codegen.buildLLVMModule(module, opts, llvmContext);
  module.getOperation()->destroy();

  if (!llvmModule) {
    FAIL("LLVM lowering failed for generic print dispatch");
    return;
  }

  auto *mainFn = llvmModule->getFunction("main");
  if (!mainFn) {
    FAIL("main function not found in lowered LLVM module");
    return;
  }

  int legacyPrintCalls = 0;
  int i32Calls = 0;
  int u32Calls = 0;
  int u64Calls = 0;
  int f64Calls = 0;
  int boolCalls = 0;
  int strCalls = 0;
  bool newlineOk = true;
  bool kindOk = true;

  for (auto &block : *mainFn) {
    for (auto &inst : block) {
      auto *call = llvm::dyn_cast<llvm::CallBase>(&inst);
      auto *callee = call ? call->getCalledFunction() : nullptr;
      if (!callee)
        continue;

      auto calleeName = callee->getName();
      if (calleeName.starts_with("hew_print") && calleeName != "hew_print_value")
        legacyPrintCalls++;

      if (calleeName != "hew_print_value")
        continue;

      auto *kind = llvm::dyn_cast<llvm::ConstantInt>(call->getArgOperand(0));
      auto *newline = llvm::dyn_cast<llvm::ConstantInt>(call->getArgOperand(2));
      if (!kind || !newline || !newline->isOne()) {
        newlineOk = false;
        continue;
      }

      switch (kind->getZExtValue()) {
      case static_cast<uint64_t>(RuntimePrintKind::I32):
        i32Calls++;
        break;
      case static_cast<uint64_t>(RuntimePrintKind::U32):
        u32Calls++;
        break;
      case static_cast<uint64_t>(RuntimePrintKind::U64):
        u64Calls++;
        break;
      case static_cast<uint64_t>(RuntimePrintKind::F64):
        f64Calls++;
        break;
      case static_cast<uint64_t>(RuntimePrintKind::Bool):
        boolCalls++;
        break;
      case static_cast<uint64_t>(RuntimePrintKind::Str):
        strCalls++;
        break;
      default:
        kindOk = false;
        break;
      }
    }
  }

  if (legacyPrintCalls != 0) {
    FAIL("expected lowering to avoid typed print runtime wrappers");
    return;
  }
  if (!newlineOk) {
    FAIL("expected generic print calls to preserve newline=true");
    return;
  }
  if (!kindOk || i32Calls != 1 || u32Calls != 1 || u64Calls != 1 || f64Calls != 2 ||
      boolCalls != 1 || strCalls != 1) {
    FAIL("expected generic print call kinds for i8/u16/u64/f32/f64/bool/string");
    return;
  }

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
fn main() -> int {
    var sum: int = 0;
    var i: int = 1;
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
fn abs(x: int) -> int {
    if x < 0 {
        0 - x
    } else {
        x
    }
}
fn main() -> int {
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
// Test: Statement-position if/match endings lower without scf.if results
// ============================================================================
static void test_statement_position_if_and_match_lower_without_results() {
  TEST(statement_position_if_and_match_lower_without_results);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
enum Direction {
    North;
    South;
}

fn stmt_if(flag: bool) {
    if flag {
        if flag {
            println(1);
        } else {
            println(2);
        }
    }
}

fn stmt_match(d: Direction) {
    if true {
        match d {
            North => println(3),
            South => println(4),
        }
    }
}

fn main() {
    stmt_if(true);
    stmt_match(North);
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  auto stmtIf = lookupFuncBySuffix(module, "stmt_if");
  auto stmtMatch = lookupFuncBySuffix(module, "stmt_match");
  if (!stmtIf || !stmtMatch) {
    FAIL("statement-position test functions not found");
    module.getOperation()->destroy();
    return;
  }

  if (countResultfulIfOps(stmtIf) != 0) {
    FAIL("statement-position if should not produce scf.if results");
    module.getOperation()->destroy();
    return;
  }

  if (countResultfulIfOps(stmtMatch) != 0) {
    FAIL("statement-position match should not produce scf.if results");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Unannotated early-return tails lower final if/match as statements
// ============================================================================
static void test_unannotated_early_return_tail_if_match_lower_without_results() {
  TEST(unannotated_early_return_tail_if_match_lower_without_results);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn unannotated_match_with_early_return(x: bool) {
    if x {
        return;
    }
    match x {
        true => println(1),
        false => println(2),
    }
}

fn unannotated_if_with_early_return(x: bool) {
    if x {
        return;
    }
    if x {
        println(3);
    } else {
        println(4);
    }
}

fn main() {
    unannotated_match_with_early_return(false);
    unannotated_if_with_early_return(false);
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  auto matchFn = lookupFuncBySuffix(module, "unannotated_match_with_early_return");
  auto ifFn = lookupFuncBySuffix(module, "unannotated_if_with_early_return");
  if (!matchFn || !ifFn) {
    FAIL("unannotated early-return test functions not found");
    module.getOperation()->destroy();
    return;
  }

  if (countResultfulIfOps(matchFn) != 0) {
    FAIL("unannotated early-return tail match should not produce scf.if results");
    module.getOperation()->destroy();
    return;
  }

  if (countResultfulIfOps(ifFn) != 0) {
    FAIL("unannotated early-return tail if should not produce scf.if results");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Nested statement-position if inside non-void function produces no
//       resultful scf.if ops (regression for generateIfStmt statementPosition)
// ============================================================================
static void test_non_void_nested_stmt_if_no_results() {
  TEST(non_void_nested_stmt_if_no_results);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn compute(flag: bool, x: int) -> int {
    if flag {
        if x > 0 {
            println(1);
        } else {
            println(2);
        }
    } else {
        if x > 0 {
            println(3);
        }
    }
    x
}

fn main() -> int {
    compute(true, 42)
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  auto computeFn = lookupFuncBySuffix(module, "F7compute");
  if (!computeFn) {
    FAIL("compute function not found");
    module.getOperation()->destroy();
    return;
  }

  // The only legitimate resultful scf.if in a non-void function is the final
  // return-value materialization op.  Nested control-flow ifs (statement
  // position) must produce no results.  The bug was that generateIfStmt did
  // not pass statementPosition=true to its then/else generateBlock calls,
  // causing the last StmtIf inside those blocks to be lowered via
  // generateIfStmtAsExpr → unnecessary resultful scf.if.
  if (countResultfulIfOps(computeFn) > 1) {
    FAIL("nested statement-position if inside non-void function should not produce extra resultful "
         "scf.if");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Discarded block expression must keep tail if lowering in statement mode
// ============================================================================
static void test_discarded_block_expr_tail_if_no_extra_results() {
  TEST(discarded_block_expr_tail_if_no_extra_results);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn block_demo(flag: bool) -> int {
    {
        if flag {
            println(1);
        } else {
            println(2);
        }
    };
    0
}

fn main() -> int {
    block_demo(true)
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  auto demoFn = lookupFuncBySuffix(module, "block_demo");
  if (!demoFn) {
    FAIL("block_demo function not found");
    module.getOperation()->destroy();
    return;
  }

  if (countResultfulIfOps(demoFn) != 1) {
    FAIL("discarded block expression should leave only the final return materialization scf.if");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Discarded unsafe block expression must keep tail if lowering in
//       statement mode
// ============================================================================
static void test_discarded_unsafe_expr_tail_if_no_extra_results() {
  TEST(discarded_unsafe_expr_tail_if_no_extra_results);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn unsafe_demo(flag: bool) -> int {
    unsafe {
        if flag {
            println(3);
        } else {
            println(4);
        }
    };
    0
}

fn main() -> int {
    unsafe_demo(true)
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  auto demoFn = lookupFuncBySuffix(module, "unsafe_demo");
  if (!demoFn) {
    FAIL("unsafe_demo function not found");
    module.getOperation()->destroy();
    return;
  }

  if (countResultfulIfOps(demoFn) != 1) {
    FAIL("discarded unsafe block expression should leave only the final return materialization "
         "scf.if");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Discarded block expression fail-closes when a nested tail expression
//       lowers to null without its own diagnostic
// ============================================================================
static void test_discarded_block_expr_bad_tail_fails_closed() {
  TEST(discarded_block_expr_bad_tail_fails_closed);

  auto program = makeDiscardedBlockLikeBadTailProgram(/*useUnsafe=*/false);

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure for discarded block expression with bad tail");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("discarded block expression in statement position failed to lower") ==
      std::string::npos) {
    FAIL("expected discarded block-expression fail-closed diagnostic");
    return;
  }

  PASS();
}

// ============================================================================
// Test: Discarded unsafe expression fail-closes when a nested tail expression
//       lowers to null without its own diagnostic
// ============================================================================
static void test_discarded_unsafe_expr_bad_tail_fails_closed() {
  TEST(discarded_unsafe_expr_bad_tail_fails_closed);

  auto program = makeDiscardedBlockLikeBadTailProgram(/*useUnsafe=*/true);

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure for discarded unsafe expression with bad tail");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("discarded unsafe expression in statement position failed to lower") ==
      std::string::npos) {
    FAIL("expected discarded unsafe-expression fail-closed diagnostic");
    return;
  }

  PASS();
}

// ============================================================================
// Test: Statement-style match arm bodies fail-close when expression lowering
//       returns null without its own diagnostic
// ============================================================================
static void test_statement_style_match_arm_bad_body_fails_closed() {
  TEST(statement_style_match_arm_bad_body_fails_closed);

  auto program =
      makeStatementStyleMatchArmBadBodyProgram(StatementStyleMatchArmBadBodyKind::DirectExpr);

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure for statement-style match arm bad body");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("discarded match arm body in statement position failed to lower") ==
      std::string::npos) {
    FAIL("expected statement-style match arm fail-closed diagnostic");
    return;
  }

  PASS();
}

// ============================================================================
// Test: Statement-style match arm block tails fail-close when a value-producing
//       tail if returns null without its own diagnostic
// ============================================================================
static void test_statement_style_match_arm_block_tail_if_fails_closed() {
  TEST(statement_style_match_arm_block_tail_if_fails_closed);

  auto program =
      makeStatementStyleMatchArmBadBodyProgram(StatementStyleMatchArmBadBodyKind::BlockTailIf);

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure for statement-style match arm block-tail if");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("discarded match arm body in statement position failed to lower") ==
      std::string::npos) {
    FAIL("expected statement-style match arm block-tail if fail-closed diagnostic");
    return;
  }

  PASS();
}

// ============================================================================
// Test: Statement-style match arm block tails fail-close when a value-producing
//       tail match returns null without its own diagnostic
// ============================================================================
static void test_statement_style_match_arm_block_tail_match_fails_closed() {
  TEST(statement_style_match_arm_block_tail_match_fails_closed);

  auto program =
      makeStatementStyleMatchArmBadBodyProgram(StatementStyleMatchArmBadBodyKind::BlockTailMatch);

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure for statement-style match arm block-tail match");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("discarded match arm body in statement position failed to lower") ==
      std::string::npos) {
    FAIL("expected statement-style match arm block-tail match fail-closed diagnostic");
    return;
  }

  PASS();
}

// ============================================================================
// Test: Statement-style match arm discarded block tails with statement-only if
//       still lower successfully
// ============================================================================
static void test_statement_style_match_arm_statement_only_if_tail_lowers() {
  TEST(statement_style_match_arm_statement_only_if_tail_lowers);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn stmt_arm_tail_if(flag: bool) {
    match flag {
        true => {
            if flag {
                println(1);
            } else {
                println(2);
            }
        },
        false => println(3),
    }
}

fn main() {
    stmt_arm_tail_if(true);
}
  )");

  if (!module) {
    FAIL("MLIR generation failed for statement-only match arm if tail");
    return;
  }

  auto armFn = lookupFuncBySuffix(module, "stmt_arm_tail_if");
  if (!armFn) {
    FAIL("stmt_arm_tail_if function not found");
    module.getOperation()->destroy();
    return;
  }

  if (countResultfulIfOps(armFn) != 0) {
    FAIL("statement-only if tail in discarded match arm body should not produce resultful scf.if");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Statement-style match arm discarded block tails with statement-only
//       match still lower successfully
// ============================================================================
static void test_statement_style_match_arm_statement_only_match_tail_lowers() {
  TEST(statement_style_match_arm_statement_only_match_tail_lowers);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn stmt_arm_tail_match(flag: bool) {
    match flag {
        true => {
            match flag {
                true => println(4),
                false => println(5),
            }
        },
        false => println(6),
    }
}

fn main() {
    stmt_arm_tail_match(true);
}
  )");

  if (!module) {
    FAIL("MLIR generation failed for statement-only match arm match tail");
    return;
  }

  auto armFn = lookupFuncBySuffix(module, "stmt_arm_tail_match");
  if (!armFn) {
    FAIL("stmt_arm_tail_match function not found");
    module.getOperation()->destroy();
    return;
  }

  if (countResultfulIfOps(armFn) != 0) {
    FAIL("statement-only match tail in discarded match arm body should not produce resultful "
         "scf.if");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Discarded scope expression must keep tail if lowering in statement mode
// ============================================================================
static void test_discarded_scope_expr_tail_if_no_extra_results() {
  TEST(discarded_scope_expr_tail_if_no_extra_results);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn scope_demo(flag: bool) -> int {
    scope {
        if flag {
            println(5);
        } else {
            println(6);
        }
    };
    0
}

fn main() -> int {
    scope_demo(true)
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  auto demoFn = lookupFuncBySuffix(module, "scope_demo");
  if (!demoFn) {
    FAIL("scope_demo function not found");
    module.getOperation()->destroy();
    return;
  }

  if (countResultfulIfOps(demoFn) != 1) {
    FAIL("discarded scope expression should leave only the final return materialization scf.if");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Nested discarded block-like expressions keep tail if lowering in
//       statement mode
// ============================================================================
static void test_nested_discarded_scope_expr_tail_if_no_extra_results() {
  TEST(nested_discarded_scope_expr_tail_if_no_extra_results);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn nested_scope_trailing(flag: bool) -> int {
    {
        scope {
            if flag {
                println(7);
            } else {
                println(8);
            }
        }
    };
    0
}

fn nested_scope_stmt(flag: bool) -> int {
    {
        scope {
            if flag {
                println(9);
            } else {
                println(10);
            }
        };
    };
    0
}

fn main() -> int {
    nested_scope_trailing(true) + nested_scope_stmt(false)
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  auto trailingFn = lookupFuncBySuffix(module, "nested_scope_trailing");
  auto stmtFn = lookupFuncBySuffix(module, "nested_scope_stmt");
  if (!trailingFn || !stmtFn) {
    FAIL("nested discarded scope test functions not found");
    module.getOperation()->destroy();
    return;
  }

  if (countResultfulIfOps(trailingFn) != 1) {
    FAIL("nested discarded trailing scope expression should leave only the final return "
         "materialization scf.if");
    module.getOperation()->destroy();
    return;
  }

  if (countResultfulIfOps(stmtFn) != 1) {
    FAIL("nested discarded scope expression statement should leave only the final return "
         "materialization scf.if");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Discarded scope wrappers keep nested tail if lowering in statement mode
// ============================================================================
static void test_discarded_scope_wrapper_tail_if_no_extra_results() {
  TEST(discarded_scope_wrapper_tail_if_no_extra_results);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn nested_scope_block_wrapper_trailing(flag: bool) -> int {
    scope {
        {
            if flag {
                println(11);
            } else {
                println(12);
            }
        }
    };
    0
}

fn nested_scope_block_wrapper_stmt(flag: bool) -> int {
    scope {
        {
            if flag {
                println(13);
            } else {
                println(14);
            }
        };
    };
    0
}

fn nested_scope_unsafe_wrapper_trailing(flag: bool) -> int {
    scope {
        unsafe {
            if flag {
                println(15);
            } else {
                println(16);
            }
        }
    };
    0
}

fn nested_scope_unsafe_wrapper_stmt(flag: bool) -> int {
    scope {
        unsafe {
            if flag {
                println(17);
            } else {
                println(18);
            }
        };
    };
    0
}

fn main() -> int {
    nested_scope_block_wrapper_trailing(true) +
    nested_scope_block_wrapper_stmt(false) +
    nested_scope_unsafe_wrapper_trailing(true) +
    nested_scope_unsafe_wrapper_stmt(false)
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  auto blockTrailingFn = lookupFuncBySuffix(module, "nested_scope_block_wrapper_trailing");
  auto blockStmtFn = lookupFuncBySuffix(module, "nested_scope_block_wrapper_stmt");
  auto unsafeTrailingFn = lookupFuncBySuffix(module, "nested_scope_unsafe_wrapper_trailing");
  auto unsafeStmtFn = lookupFuncBySuffix(module, "nested_scope_unsafe_wrapper_stmt");
  if (!blockTrailingFn || !blockStmtFn || !unsafeTrailingFn || !unsafeStmtFn) {
    FAIL("nested scope wrapper test functions not found");
    module.getOperation()->destroy();
    return;
  }

  if (countResultfulIfOps(blockTrailingFn) != 1) {
    FAIL("discarded scope block wrapper trailing expression should leave only the final return "
         "materialization scf.if");
    module.getOperation()->destroy();
    return;
  }

  if (countResultfulIfOps(blockStmtFn) != 1) {
    FAIL("discarded scope block wrapper statement should leave only the final return "
         "materialization scf.if");
    module.getOperation()->destroy();
    return;
  }

  if (countResultfulIfOps(unsafeTrailingFn) != 1) {
    FAIL("discarded scope unsafe wrapper trailing expression should leave only the final return "
         "materialization scf.if");
    module.getOperation()->destroy();
    return;
  }

  if (countResultfulIfOps(unsafeStmtFn) != 1) {
    FAIL("discarded scope unsafe wrapper statement should leave only the final return "
         "materialization scf.if");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Discarded scope expression must keep tail match lowering in
//       statement mode
// ============================================================================
static void test_discarded_scope_expr_tail_match_no_extra_results() {
  TEST(discarded_scope_expr_tail_match_no_extra_results);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn scope_match_demo(flag: bool) -> int {
    scope {
        match flag {
            true => println(19),
            false => println(20),
        }
    };
    0
}

fn main() -> int {
    scope_match_demo(true)
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  auto demoFn = lookupFuncBySuffix(module, "scope_match_demo");
  if (!demoFn) {
    FAIL("scope_match_demo function not found");
    module.getOperation()->destroy();
    return;
  }

  if (countResultfulIfOps(demoFn) != 1) {
    FAIL("discarded scope match expression should leave only the final return materialization "
         "scf.if");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Nested discarded scope match expressions keep tail lowering in
//       statement mode
// ============================================================================
static void test_nested_discarded_scope_expr_tail_match_no_extra_results() {
  TEST(nested_discarded_scope_expr_tail_match_no_extra_results);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn nested_scope_match_trailing(flag: bool) -> int {
    {
        scope {
            match flag {
                true => println(21),
                false => println(22),
            }
        }
    };
    0
}

fn nested_scope_match_stmt(flag: bool) -> int {
    {
        scope {
            match flag {
                true => println(23),
                false => println(24),
            }
        };
    };
    0
}

fn main() -> int {
    nested_scope_match_trailing(true) + nested_scope_match_stmt(false)
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  auto trailingFn = lookupFuncBySuffix(module, "nested_scope_match_trailing");
  auto stmtFn = lookupFuncBySuffix(module, "nested_scope_match_stmt");
  if (!trailingFn || !stmtFn) {
    FAIL("nested discarded scope match test functions not found");
    module.getOperation()->destroy();
    return;
  }

  if (countResultfulIfOps(trailingFn) != 1) {
    FAIL("nested discarded trailing scope match should leave only the final return "
         "materialization scf.if");
    module.getOperation()->destroy();
    return;
  }

  if (countResultfulIfOps(stmtFn) != 1) {
    FAIL("nested discarded scope match statement should leave only the final return "
         "materialization scf.if");
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
fn compute(a: int, b: int) -> int {
    let sum = a + b;
    let diff = a - b;
    let prod = a * b;
    let quot = a / b;
    let rem = a % b;
    sum + diff + prod + quot + rem
}
fn main() -> int {
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
fn test_cmp(a: int, b: int) -> int {
    if a < b { 1 } else { 0 }
}
fn main() -> int {
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

static void test_materialized_unsigned_range_uses_unsigned_compare() {
  TEST(materialized_unsigned_range_uses_unsigned_compare);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn count(lo: u64, hi: u64) -> int {
    let r: Range<u64> = lo..hi;
    var n: int = 0;
    for _ in r {
        n = n + 1;
    }
    n
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  auto countFn = lookupFuncBySuffix(module, "count");
  if (!countFn) {
    FAIL("count function not found");
    module.getOperation()->destroy();
    return;
  }

  bool hasUnsignedRangeCompare = false;
  bool hasSignedRangeCompare = false;
  countFn.walk([&](mlir::arith::CmpIOp cmp) {
    hasUnsignedRangeCompare |= cmp.getPredicate() == mlir::arith::CmpIPredicate::ult;
    hasSignedRangeCompare |= cmp.getPredicate() == mlir::arith::CmpIPredicate::slt;
  });

  if (!hasUnsignedRangeCompare) {
    FAIL("expected materialized Range<u64> loop to use arith.cmpi ult");
    module.getOperation()->destroy();
    return;
  }

  if (hasSignedRangeCompare) {
    FAIL("materialized Range<u64> loop should not use arith.cmpi slt");
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
fn identity(x: int) -> int {
    return x;
}
fn add_one(x: int) -> int {
    return x + 1;
}
fn main() -> int {
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
fn main() -> int {
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
fn negate(x: int) -> int {
    0 - x
}
fn main() -> int {
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
fn main() -> int {
    var x: int = 10;
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
fn double(x: int) -> int {
    x * 2
}
fn add_doubled(a: int, b: int) -> int {
    double(a) + double(b)
}
fn main() -> int {
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
fn main() -> int {
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
// Test: Statement-style match always emits a runtime trap on the unmatched path
//
// Every conditional arm in a statement-style match must have an else region
// that either chains to the next arm or traps — never silently falls through.
// The regression exercises two shapes:
//   (a) An exhaustive enum variant match where the last arm is still lowered
//       through generateTagMatch (enum-variant arms are always conditional).
//   (b) A bool literal match — same issue: last literal arm has no else.
// Both should emit at least one hew.panic after the fix.
// ============================================================================
static void test_stmt_match_last_arm_emits_panic() {
  TEST(stmt_match_last_arm_emits_panic);

  mlir::MLIRContext ctx;
  initContext(ctx);

  // (a) Exhaustive two-variant enum: the 'Done' arm is the last conditional.
  auto module = generateMLIR(ctx, R"(
enum Status {
    Active;
    Done;
}

fn stmt_enum_match(s: Status) {
    match s {
        Active => println(1),
        Done => println(2),
    }
}

fn main() {
    stmt_enum_match(Active);
}
  )");

  if (!module) {
    FAIL("MLIR generation failed (enum variant case)");
    return;
  }

  auto enumFn = lookupFuncBySuffix(module, "stmt_enum_match");
  if (!enumFn) {
    FAIL("stmt_enum_match function not found");
    module.getOperation()->destroy();
    return;
  }

  // The 'Done' arm is the last conditional arm.  Its else path must trap so
  // that a corrupt/unexpected discriminant value doesn't silently fall through.
  if (countAllPanics(enumFn) < 1) {
    FAIL("last conditional arm of a statement-style match must emit hew.panic in its else path");
    module.getOperation()->destroy();
    return;
  }

  if (countResultfulIfOps(enumFn) != 0) {
    FAIL("statement-style enum match should not produce resultful scf.if ops");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();

  // (b) Bool literal match: last arm is 'false' — a literal pattern that
  //     generateTagMatch handles, producing no else without the fix.
  auto module2 = generateMLIR(ctx, R"(
fn stmt_bool_match(x: bool) {
    match x {
        true => println(1),
        false => println(2),
    }
}

fn main() {
    stmt_bool_match(true);
}
  )");

  if (!module2) {
    FAIL("MLIR generation failed (bool literal case)");
    return;
  }

  auto boolFn = lookupFuncBySuffix(module2, "stmt_bool_match");
  if (!boolFn) {
    FAIL("stmt_bool_match function not found");
    module2.getOperation()->destroy();
    return;
  }

  if (countAllPanics(boolFn) < 1) {
    FAIL("last conditional arm of bool literal match must emit hew.panic in its else path");
    module2.getOperation()->destroy();
    return;
  }

  if (countResultfulIfOps(boolFn) != 0) {
    FAIL("statement-style bool literal match should not produce resultful scf.if ops");
    module2.getOperation()->destroy();
    return;
  }

  module2.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: statement-style match unmatched panic blocks do not fall through
//
// The MLIR match lowering already emits hew.panic for the unmatched path, but
// the lowered CFG must also terminate that block so a corrupt enum tag cannot
// continue into the post-match continuation.
// ============================================================================
static void test_stmt_match_unmatched_panic_block_does_not_fall_through() {
  TEST(stmt_match_unmatched_panic_block_does_not_fall_through);

  mlir::MLIRContext ctx;
  initContext(ctx);

  auto module = generateMLIR(ctx, R"(
enum Status {
    Active;
    Done;
}

extern "C" {
    fn status() -> Status;
}

fn f(x: Status) {
    match x {
        Active => println(1),
        Done => println(2),
    }
    println(9);
}

fn main() {
    unsafe {
        f(status());
    }
}
  )");

  if (!module) {
    FAIL("MLIR generation failed for statement match panic CFG test");
    return;
  }

  hew::Codegen codegen(ctx);
  hew::CodegenOptions opts;
  opts.debug_info = true;
  llvm::LLVMContext llvmContext;
  auto llvmModule = codegen.buildLLVMModule(module, opts, llvmContext);
  module.getOperation()->destroy();

  if (!llvmModule) {
    FAIL("LLVM lowering failed for statement match panic CFG test");
    return;
  }

  llvm::BasicBlock *panicBlock = nullptr;
  llvm::BasicBlock *continueBlock = nullptr;
  for (auto &fn : *llvmModule) {
    if (fn.isDeclaration())
      continue;
    for (auto &block : fn) {
      for (auto &inst : block) {
        auto *call = llvm::dyn_cast<llvm::CallBase>(&inst);
        auto *callee = call ? call->getCalledFunction() : nullptr;
        if (!callee)
          continue;

        if (callee->getName() == "hew_panic")
          panicBlock = &block;

        if (callee->getName() == "hew_print_value" &&
            llvmCallHasConstIntArg(call, 0, static_cast<uint64_t>(RuntimePrintKind::I64))) {
          auto *value = llvm::dyn_cast<llvm::ConstantInt>(call->getArgOperand(1));
          if (value && value->equalsInt(9) && llvmCallHasConstIntArg(call, 2, 1))
            continueBlock = &block;
        }
      }
    }
  }

  if (!panicBlock) {
    FAIL("expected lowered statement match to contain an unmatched hew_panic block");
    return;
  }

  if (!continueBlock) {
    FAIL("expected lowered statement match to contain the post-match continuation");
    return;
  }

  if (panicBlock->getParent() != continueBlock->getParent()) {
    FAIL("panic block and continuation should remain in the same lowered function");
    return;
  }

  for (auto *pred : llvm::predecessors(continueBlock)) {
    if (pred == panicBlock) {
      FAIL("unmatched statement-style match must not fall through into the continuation");
      return;
    }
  }

  if (!llvm::isa<llvm::UnreachableInst>(panicBlock->getTerminator())) {
    FAIL("unmatched statement-style match panic block must terminate with unreachable");
    return;
  }

  PASS();
}

// ============================================================================
// Test: Resultful match lowering fails closed when an arm body disappears
// ============================================================================
static void test_resultful_match_missing_arm_body_fails_closed() {
  TEST(resultful_match_missing_arm_body_fails_closed);

  hew::ast::Expr firstBodyExpr;
  firstBodyExpr.kind = hew::ast::ExprLiteral{hew::ast::Literal(hew::ast::LitInteger{1})};
  auto program = buildResultfulStmtMatchProgram(
      /*firstBody=*/nullptr,
      /*secondBody=*/makeExpr(std::move(firstBodyExpr)),
      /*secondArmWildcard=*/false);

  mlir::MLIRContext ctx;
  initContext(ctx);
  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure for missing resultful match arm body");
    module.getOperation()->destroy();
    return;
  }

  constexpr llvm::StringLiteral kDiag = "match expression arm lowering did not produce a value";
  if (stderrText.find(kDiag.str()) == std::string::npos) {
    FAIL("expected resultful match arm fail-closed diagnostic");
    return;
  }
  if (countSubstringOccurrences(stderrText, kDiag) != 1) {
    FAIL("expected exactly one resultful match arm fail-closed diagnostic");
    return;
  }

  PASS();
}

// ============================================================================
// Test: Resultful match else-chain lowering fails closed on silent nullptr
// ============================================================================
static void test_resultful_match_missing_else_chain_value_fails_closed() {
  TEST(resultful_match_missing_else_chain_value_fails_closed);

  hew::ast::Expr firstBodyExpr;
  firstBodyExpr.kind = hew::ast::ExprLiteral{hew::ast::Literal(hew::ast::LitInteger{1})};
  auto program = buildResultfulStmtMatchProgram(
      /*firstBody=*/makeExpr(std::move(firstBodyExpr)),
      /*secondBody=*/nullptr,
      /*secondArmWildcard=*/true);

  mlir::MLIRContext ctx;
  initContext(ctx);
  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure for missing resultful match else-chain value");
    module.getOperation()->destroy();
    return;
  }

  constexpr llvm::StringLiteral kDiag =
      "match expression else-chain lowering did not produce a value";
  if (stderrText.find(kDiag.str()) == std::string::npos) {
    FAIL("expected resultful match else-chain fail-closed diagnostic");
    return;
  }
  if (countSubstringOccurrences(stderrText, kDiag) != 1) {
    FAIL("expected exactly one resultful match else-chain fail-closed diagnostic");
    return;
  }

  PASS();
}

// ============================================================================
// Test: Resultful match fail-closed diagnostics do not double-report nested
//       arm lowering errors
// ============================================================================
static void test_resultful_match_nested_arm_error_is_not_double_reported() {
  TEST(resultful_match_nested_arm_error_is_not_double_reported);

  hew::ast::Expr firstBodyExpr;
  firstBodyExpr.kind = hew::ast::ExprLiteral{hew::ast::Literal(hew::ast::LitInteger{1})};
  hew::ast::Expr secondBodyExpr;
  secondBodyExpr.kind = hew::ast::ExprIdentifier{"missing"};
  auto program = buildResultfulStmtMatchProgram(
      /*firstBody=*/makeExpr(std::move(firstBodyExpr)),
      /*secondBody=*/makeExpr(std::move(secondBodyExpr)),
      /*secondArmWildcard=*/true);

  mlir::MLIRContext ctx;
  initContext(ctx);
  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure for nested resultful match arm error");
    module.getOperation()->destroy();
    return;
  }
  if (stderrText.find("undeclared variable 'missing'") == std::string::npos) {
    FAIL("expected nested arm diagnostic for undeclared variable");
    return;
  }
  if (stderrText.find("match expression arm lowering did not produce a value") !=
          std::string::npos ||
      stderrText.find("match expression else-chain lowering did not produce a value") !=
          std::string::npos) {
    FAIL("nested resultful match arm diagnostics should not be double-reported");
    return;
  }

  PASS();
}

// ============================================================================
// Test: Resultful match arms that panic still lower without missing-value
//       diagnostics
// ============================================================================
static void test_resultful_match_panic_arm_lowers() {
  TEST(resultful_match_panic_arm_lowers);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
fn lower_panic_arm(flag: bool) -> i64 {
    let value = match flag {
        true => 1,
        false => { panic("boom"); },
    };
    value
}

fn main() -> i64 {
    lower_panic_arm(true)
}
  )",
                             program)) {
    FAIL("failed to parse resultful match panic-arm source");
    return;
  }

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (!module) {
    FAIL("MLIR generation failed for resultful match panic arm");
    return;
  }
  if (stderrText.find("match expression arm lowering did not produce a value") !=
          std::string::npos ||
      stderrText.find("match expression else-chain lowering did not produce a value") !=
          std::string::npos) {
    FAIL("panic-only resultful match arms should not trip missing-value diagnostics");
    module.getOperation()->destroy();
    return;
  }

  auto panicFn = lookupFuncBySuffix(module, "lower_panic_arm");
  if (!panicFn) {
    FAIL("lower_panic_arm function not found");
    module.getOperation()->destroy();
    return;
  }
  if (countAllPanics(panicFn) < 1) {
    FAIL("expected resultful match panic arm to retain a panic path");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Trailing void if/match use statement lowering
// ============================================================================
static void test_void_trailing_if_match_stmt_lowering() {
  TEST(void_trailing_if_match_stmt_lowering);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
enum Choice {
    Left;
    Right;
}

fn emitIf(flag: bool) -> () {
    if flag {
        println(1);
    } else {
        println(2);
    }
}

fn emitMatch(choice: Choice) -> () {
    match choice {
        Left => println(3),
        Right => println(4),
    }
}

fn main() -> int {
    emitIf(true);
    emitMatch(Left);
    0
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  auto emitIf = lookupFuncBySuffix(module, "F6emitIf");
  auto emitMatch = lookupFuncBySuffix(module, "F9emitMatch");
  if (!emitIf || !emitMatch) {
    FAIL("void helper function not found");
    module.getOperation()->destroy();
    return;
  }

  auto allIfOpsAreStatementLowered = [](mlir::Operation *op) {
    int ifCount = 0;
    bool ok = true;
    op->walk([&](mlir::scf::IfOp ifOp) {
      ++ifCount;
      ok = ok && ifOp->getNumResults() == 0;
    });
    return std::pair<int, bool>{ifCount, ok};
  };

  auto [emitIfCount, emitIfOk] = allIfOpsAreStatementLowered(emitIf);
  auto [emitMatchCount, emitMatchOk] = allIfOpsAreStatementLowered(emitMatch);

  if (emitIfCount == 0 || emitMatchCount == 0) {
    FAIL("expected scf.if operations in void helper lowering");
    module.getOperation()->destroy();
    return;
  }
  if (!emitIfOk || !emitMatchOk) {
    FAIL("void trailing if/match should lower through no-result statement paths");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Builtin Option/Result constructors carry explicit payload_positions
// ============================================================================
static void test_builtin_enum_constructors_use_explicit_payload_positions() {
  TEST(builtin_enum_constructors_use_explicit_payload_positions);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn main() -> int {
    let maybe = Some(3);
    maybe;
    Ok(7);
    Err(9);
    0
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  int optionSomeCount = 0;
  int resultOkCount = 0;
  int resultErrCount = 0;
  bool optionSomeUsesFieldOne = false;
  bool resultOkUsesFieldOne = false;
  bool resultErrUsesFieldTwo = false;
  module.walk([&](hew::EnumConstructOp op) {
    auto positions = op.getPayloadPositions();
    if (!positions || positions->size() != 1)
      return;

    auto payloadField = mlir::cast<mlir::IntegerAttr>((*positions)[0]).getInt();
    if (op.getEnumName() == "Option" && op.getVariantIndex() == 1) {
      optionSomeCount++;
      optionSomeUsesFieldOne |= payloadField == 1;
      return;
    }
    if (op.getEnumName() != "__Result")
      return;
    if (op.getVariantIndex() == 0) {
      resultOkCount++;
      resultOkUsesFieldOne |= payloadField == 1;
      return;
    }
    if (op.getVariantIndex() == 1) {
      resultErrCount++;
      resultErrUsesFieldTwo |= payloadField == 2;
    }
  });

  if (optionSomeCount < 1 || resultOkCount < 1 || resultErrCount < 1) {
    FAIL("expected explicit payload_positions on Some/Ok/Err constructors");
    module.getOperation()->destroy();
    return;
  }
  if (!optionSomeUsesFieldOne || !resultOkUsesFieldOne || !resultErrUsesFieldTwo) {
    FAIL("builtin enum constructors should use the expected payload field positions");
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
fn takes_unknown(x: MissingType) -> int {
    0
}
fn main() -> int {
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
    id: int @1;
    value: int @2;
}
fn main() -> int {
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
// Test: JSON/YAML bytes helpers use dedicated base64 runtime shims
// ============================================================================
static void test_wire_bytes_use_base64_serial_helpers() {
  TEST(wire_bytes_use_base64_serial_helpers);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
wire type BlobPacket {
    payload: bytes @1;
    note: String @2;
}

fn main() -> int {
    0
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  auto encodeFn = module.lookupSymbol<mlir::func::FuncOp>("BlobPacket_encode");
  auto decodeFn = module.lookupSymbol<mlir::func::FuncOp>("BlobPacket_decode");
  auto toJsonFn = module.lookupSymbol<mlir::func::FuncOp>("BlobPacket_to_json");
  auto fromJsonFn = module.lookupSymbol<mlir::func::FuncOp>("BlobPacket_from_json");
  auto toYamlFn = module.lookupSymbol<mlir::func::FuncOp>("BlobPacket_to_yaml");
  auto fromYamlFn = module.lookupSymbol<mlir::func::FuncOp>("BlobPacket_from_yaml");

  if (!encodeFn || !decodeFn || !toJsonFn || !fromJsonFn || !toYamlFn || !fromYamlFn) {
    FAIL("expected BlobPacket wire helpers to be generated");
    module.getOperation()->destroy();
    return;
  }

  if (countRuntimeCallsByCallee(encodeFn, "hew_wire_encode_field_bytes") != 1 ||
      countRuntimeCallsByCallee(encodeFn, "hew_wire_encode_field_string") != 1 ||
      countRuntimeCallsByCallee(decodeFn, "hew_wire_decode_bytes") != 1 ||
      countRuntimeCallsByCallee(decodeFn, "hew_wire_decode_string") != 1) {
    FAIL("binary wire bytes/string helpers should remain unchanged");
    module.getOperation()->destroy();
    return;
  }

  if (countRuntimeCallsByCallee(toJsonFn, "hew_json_object_set_bytes") != 1 ||
      countRuntimeCallsByCallee(toJsonFn, "hew_json_object_set_string") != 1 ||
      countRuntimeCallsByCallee(fromJsonFn, "hew_json_get_bytes") != 1 ||
      countRuntimeCallsByCallee(fromJsonFn, "hew_json_get_string") != 1 ||
      countRuntimeCallsByCallee(toYamlFn, "hew_yaml_object_set_bytes") != 1 ||
      countRuntimeCallsByCallee(toYamlFn, "hew_yaml_object_set_string") != 1 ||
      countRuntimeCallsByCallee(fromYamlFn, "hew_yaml_get_bytes") != 1 ||
      countRuntimeCallsByCallee(fromYamlFn, "hew_yaml_get_string") != 1) {
    FAIL("JSON/YAML bytes fields should route through dedicated base64 helpers");
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
    Int(int);
    Big(i64);
    Text(String);
    Unit;
}

wire type Packet {
    id: int @1;
    note: String @2;
}

fn main() -> int {
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
// Test: TypeDecl-based wire enum preserves variant payload metadata
// ============================================================================
static void test_wire_enum_typedecl_preserves_variants() {
  TEST(wire_enum_typedecl_preserves_variants);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
wire enum Mixed {
    Int(int);
    Big(i64);
    Text(String);
    Unit;
}

fn main() -> int {
    let _a = Int(7);
    let _b = Big(7000000000);
    let _c = Text("hello");
    let _d = Unit;
    0
}
  )",
                             program)) {
    FAIL("failed to load typed program");
    return;
  }

  if (!desugarWireEnumToTypeDecl(program, "Mixed")) {
    FAIL("failed to desugar wire enum into TypeDecl");
    return;
  }

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, program);

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  bool sawVariant[4] = {false, false, false, false};
  bool badPayloadPos = false;

  module.walk([&](hew::EnumConstructOp op) {
    if (op.getEnumName() != "Mixed")
      return;

    auto variantIdx = op.getVariantIndex();
    if (variantIdx >= 4)
      return;
    sawVariant[variantIdx] = true;

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

  if (!sawVariant[0] || !sawVariant[1] || !sawVariant[2] || !sawVariant[3]) {
    FAIL("missing TypeDecl-based Mixed enum variant constructions");
    module.getOperation()->destroy();
    return;
  }
  if (badPayloadPos) {
    FAIL("TypeDecl-based Mixed wire enum payload positions are incorrect");
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
    Int(int);
    Big(i64);
    Text(String);
    Unit;
}

fn main() -> int {
    let m = Big(9);
    match m {
        Big(x) if x > 0 => 1,
        Big(_) => 0,
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
// Test: Unit wire enums generate JSON/YAML helpers and dispatch through wrappers
// ============================================================================
static void test_wire_enum_unit_serial_helpers_and_dispatch() {
  TEST(wire_enum_unit_serial_helpers_and_dispatch);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
#[json(camelCase)]
wire enum ReviewStatus {
    PendingReview;
    ActiveNow;
    Completed;
}

fn main() -> int {
    let status = PendingReview;
    let json = status.to_json();
    let _roundtrip_json = ReviewStatus.from_json(json);
    let yaml = status.to_yaml();
    let _roundtrip_yaml = ReviewStatus.from_yaml(yaml);
    0
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  for (const char *fnName : {"ReviewStatus_to_json", "ReviewStatus_from_json",
                             "ReviewStatus_to_yaml", "ReviewStatus_from_yaml"}) {
    if (!module.lookupSymbol<mlir::func::FuncOp>(fnName)) {
      FAIL("expected wire enum JSON/YAML helper function to be generated");
      module.getOperation()->destroy();
      return;
    }
  }

  if (module.lookupSymbol<mlir::func::FuncOp>("ReviewStatus_encode") ||
      module.lookupSymbol<mlir::func::FuncOp>("ReviewStatus_decode")) {
    FAIL("unit wire enum should not synthesize binary encode/decode helpers");
    module.getOperation()->destroy();
    return;
  }

  auto toJsonWrapper = lookupFuncBySuffix(module, "ReviewStatusF7to_json");
  auto fromJsonWrapper = lookupFuncBySuffix(module, "ReviewStatusF9from_json");
  auto toYamlWrapper = lookupFuncBySuffix(module, "ReviewStatusF7to_yaml");
  auto fromYamlWrapper = lookupFuncBySuffix(module, "ReviewStatusF9from_yaml");
  auto fromJsonHelper = module.lookupSymbol<mlir::func::FuncOp>("ReviewStatus_from_json");
  auto fromYamlHelper = module.lookupSymbol<mlir::func::FuncOp>("ReviewStatus_from_yaml");

  if (!toJsonWrapper || !fromJsonWrapper || !toYamlWrapper || !fromYamlWrapper || !fromJsonHelper ||
      !fromYamlHelper) {
    FAIL("expected wire enum method wrapper to be generated");
    module.getOperation()->destroy();
    return;
  }

  if (countRuntimeCallsByCallee(fromJsonHelper, "hew_json_string_free") < 2 ||
      countRuntimeCallsByCallee(fromJsonHelper, "hew_json_free") < 3 ||
      countRuntimeCallsByCallee(fromYamlHelper, "hew_yaml_string_free") < 2 ||
      countRuntimeCallsByCallee(fromYamlHelper, "hew_yaml_free") < 3) {
    FAIL("expected wire enum decode helpers to clean up parse state on panic paths");
    module.getOperation()->destroy();
    return;
  }

  auto mainFn = module.lookupSymbol<mlir::func::FuncOp>("main");
  if (!mainFn) {
    FAIL("expected main function");
    module.getOperation()->destroy();
    return;
  }

  if (countCallsByCallee(mainFn, toJsonWrapper.getName()) != 1 ||
      countCallsByCallee(mainFn, fromJsonWrapper.getName()) != 1 ||
      countCallsByCallee(mainFn, toYamlWrapper.getName()) != 1 ||
      countCallsByCallee(mainFn, fromYamlWrapper.getName()) != 1) {
    FAIL("expected enum method calls to dispatch through mangled wrappers");
    module.getOperation()->destroy();
    return;
  }

  if (countCallsByCallee(mainFn, "ReviewStatus_to_json") != 0 ||
      countCallsByCallee(mainFn, "ReviewStatus_from_json") != 0 ||
      countCallsByCallee(mainFn, "ReviewStatus_to_yaml") != 0 ||
      countCallsByCallee(mainFn, "ReviewStatus_from_yaml") != 0) {
    FAIL("main should call wire enum method wrappers, not helper bodies directly");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: generic struct constructor in non-generic function body (main)
// Regression: "unknown struct type 'Wrapper'" when typeParamSubstitutions empty
// ============================================================================
static void test_generic_struct_constructor_in_nongeneric_context() {
  TEST(generic_struct_constructor_in_nongeneric_context);

  mlir::MLIRContext ctx;
  initContext(ctx);
  // Wrapper { inner: 42 } inside main() — no explicit type annotation.
  // The type checker infers Wrapper<i32> and records it in expr_types.
  // The codegen must use that info to materialise Wrapper_i32.
  auto module = generateMLIR(ctx, R"(
type Wrapper<T> { inner: T; }
fn main() {
    let w = Wrapper { inner: 42 };
    println(w.inner);
}
  )");

  if (!module) {
    FAIL("MLIR generation failed for generic struct constructor in non-generic context");
    return;
  }
  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: generic struct constructor in monomorphic helper with return annotation
// Regression: "unknown struct type 'Wrapper'" in non-generic fn with explicit
//             return type (Wrapper<i32>).
// ============================================================================
static void test_generic_struct_constructor_monomorphic_helper() {
  TEST(generic_struct_constructor_monomorphic_helper);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
type Wrapper<T> { inner: T; }
fn wrap_i32(x: i32) -> Wrapper<i32> {
    Wrapper { inner: x }
}
fn main() {
    let v = wrap_i32(99);
    println(v.inner);
}
  )");

  if (!module) {
    FAIL("MLIR generation failed for generic struct constructor in monomorphic helper");
    return;
  }
  module.getOperation()->destroy();
  PASS();
}

static void test_unresolved_generic_substitution_type_fails() {
  TEST(unresolved_generic_substitution_type_fails);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn identity<T>(x: T) -> T {
    x
}
fn main() -> int {
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
// Test: Unsupported return coercion fails before verifier noise
// ============================================================================
static void test_unsupported_return_coercion_stops_before_verifier() {
  TEST(unsupported_return_coercion_stops_before_verifier);

  auto astData = hewToMsgpack(R"(
fn main() -> int {
    42
}
  )");
  if (astData.empty()) {
    FAIL("failed to produce msgpack AST");
    return;
  }
  if (replaceMsgpackFixStr(astData, "int", "str") == 0) {
    FAIL("failed to rewrite int return type in msgpack AST");
    return;
  }

  hew::ast::Program program;
  try {
    program = hew::parseMsgpackAST(astData.data(), astData.size());
  } catch (const std::exception &e) {
    FAIL(e.what());
    return;
  }

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure for unsupported return coercion");
    module.getOperation()->destroy();
    return;
  }
  if (stderrText.find("coerceType: no known conversion") == std::string::npos) {
    FAIL("expected unsupported coercion diagnostic");
    return;
  }
  if (stderrText.find("module verification failed") != std::string::npos) {
    FAIL("unexpected downstream verifier failure for unsupported coercion");
    return;
  }

  PASS();
}

// ============================================================================
// Test: Struct with all-primitive fields does NOT get encode wrappers unless
//       they are actually called (demand-gating).
// ============================================================================
static void test_prim_struct_no_serial_call_emits_no_wrappers() {
  TEST(prim_struct_no_serial_call_emits_no_wrappers);

  mlir::MLIRContext ctx;
  initContext(ctx);
  // Point has all-primitive fields, but main() never calls Point.to_json /
  // Point.from_json / etc.  The demand-gate must suppress all 6 wrappers.
  auto module = generateMLIR(ctx, R"(
type Point {
    x: i64;
    y: i64;
}
fn main() -> i64 {
    let p = Point { x: 1, y: 2 };
    p.x
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  // The generated names are mangled (e.g. "_HT5PointF7to_json") so we match
  // by substring rather than exact name.
  bool foundAnyWrapper = false;
  module.walk([&](mlir::func::FuncOp fn) {
    auto n = fn.getName();
    if (!n.contains("Point"))
      return;
    if (n.contains("to_json") || n.contains("from_json") || n.contains("to_yaml") ||
        n.contains("from_yaml") || n.contains("to_toml") || n.contains("from_toml"))
      foundAnyWrapper = true;
  });
  if (foundAnyWrapper) {
    FAIL("a Point encode/decode wrapper was emitted despite no call-site (demand-gate broken)");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Instance-syntax demand gate — p.to_json() on a plain struct.
//
// Uses the real hew type-checker pipeline (loadProgramFromSource).  Only
// the demanded to_json wrapper must be emitted; yaml/toml must stay absent.
// ============================================================================
static void test_prim_struct_instance_serial_call_emits_demanded_wrapper() {
  TEST(prim_struct_instance_serial_call_emits_demanded_wrapper);

  mlir::MLIRContext ctx;
  initContext(ctx);

  // The type-checker accepts p.to_json() on plain structs that have all
  // primitive fields.  Use it to exercise the *instance* dispatch path in
  // generateMethodCall (not the static TypeName.method path).
  auto module = generateMLIR(ctx, R"(
type Sensor {
    id: i64;
    reading: f64;
}
fn main() -> String {
    let s = Sensor { id: 1, reading: 3.14 };
    s.to_json()
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  // The to_json wrapper must have been generated (demand-gate triggered on
  // the instance dispatch path in generateMethodCall).
  bool foundToJson = false;
  module.walk([&](mlir::func::FuncOp fn) {
    auto n = fn.getName();
    if (n.contains("Sensor") && n.contains("to_json"))
      foundToJson = true;
  });
  if (!foundToJson) {
    FAIL("Sensor to_json wrapper not generated for instance-style call (instance demand-gate "
         "missing)");
    module.getOperation()->destroy();
    return;
  }

  // yaml / toml variants were never referenced — they must be absent.
  bool foundUncalled = false;
  module.walk([&](mlir::func::FuncOp fn) {
    auto n = fn.getName();
    if (!n.contains("Sensor"))
      return;
    if (n.contains("to_yaml") || n.contains("from_yaml") || n.contains("to_toml") ||
        n.contains("from_toml"))
      foundUncalled = true;
  });
  if (foundUncalled) {
    FAIL("un-called Sensor yaml/toml wrapper emitted unexpectedly (demand-gate broken)");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Static-dispatch demand gate — Point.to_json(p) with real pipeline.
//
// Uses the real hew type-checker pipeline.  Complements the instance test
// by covering the static receiver path in generateModuleMethodCall.
// ============================================================================
static void test_prim_struct_static_serial_call_emits_demanded_wrapper() {
  TEST(prim_struct_static_serial_call_emits_demanded_wrapper);

  mlir::MLIRContext ctx;
  initContext(ctx);

  // The static TypeName.method(instance) call form is a codegen-only feature:
  // the hew type-checker does not expose it.  We construct the AST directly
  // so we can exercise the generateModuleMethodCall static dispatch path
  // (the other half of the demand-gate, complementing the instance test above).
  //
  //   type Reading { value: i64; channel: i32; }
  //   fn main(r: Reading) -> i64 {
  //       let _ = Reading.to_json(r);   // static-receiver demand-gate
  //       0
  //   }

  using namespace hew::ast;

  auto mkSpan = []() -> Span { return {0, 0}; };
  auto mkNamedType = [&](const std::string &name) -> Spanned<TypeExpr> {
    TypeExpr te;
    te.kind = TypeNamed{name, std::nullopt};
    return {std::move(te), mkSpan()};
  };
  auto mkIdent = [&](const std::string &name) -> std::unique_ptr<Spanned<Expr>> {
    Expr e;
    e.kind = ExprIdentifier{name};
    return std::make_unique<Spanned<Expr>>(Spanned<Expr>{std::move(e), mkSpan()});
  };

  // type Reading { value: i64; channel: i32; }
  TypeDecl readingDecl;
  readingDecl.visibility = Visibility::Pub;
  readingDecl.kind = TypeDeclKind::Struct;
  readingDecl.name = "Reading";
  for (auto &[fname, ftype] :
       std::vector<std::pair<std::string, std::string>>{{"value", "i64"}, {"channel", "i32"}}) {
    TypeBodyItemField f;
    f.name = fname;
    f.ty = mkNamedType(ftype);
    TypeBodyItem bi;
    bi.kind = std::move(f);
    readingDecl.body.push_back(std::move(bi));
  }

  // fn main(r: Reading) -> i64 { let _ = Reading.to_json(r); 0 }
  FnDecl mainDecl;
  mainDecl.is_async = false;
  mainDecl.is_generator = false;
  mainDecl.visibility = Visibility::Pub;
  mainDecl.is_pure = false;
  mainDecl.name = "main";
  mainDecl.return_type = mkNamedType("i64");
  // parameter r: Reading
  {
    Param p;
    p.name = "r";
    p.ty = mkNamedType("Reading");
    p.is_mutable = false;
    mainDecl.params.push_back(std::move(p));
  }
  // let _ = Reading.to_json(r)  (static receiver = TypeName)
  {
    Expr mcExpr;
    ExprMethodCall mc;
    mc.receiver = mkIdent("Reading"); // static receiver: type name
    mc.method = "to_json";
    CallArgPositional arg;
    arg.expr = mkIdent("r");
    mc.args.push_back(std::move(arg));
    mcExpr.kind = std::move(mc);

    Stmt letStmt;
    StmtLet sl;
    Pattern wp;
    wp.kind = PatWildcard{};
    sl.pattern = {std::move(wp), mkSpan()};
    sl.ty = std::nullopt;
    sl.value = Spanned<Expr>{std::move(mcExpr), mkSpan()};
    letStmt.kind = std::move(sl);
    mainDecl.body.stmts.push_back(
        std::make_unique<Spanned<Stmt>>(Spanned<Stmt>{std::move(letStmt), mkSpan()}));
  }
  // trailing expr: 0
  {
    Expr e;
    ExprLiteral lit;
    lit.lit = LitInteger{0};
    e.kind = std::move(lit);
    mainDecl.body.trailing_expr =
        std::make_unique<Spanned<Expr>>(Spanned<Expr>{std::move(e), mkSpan()});
  }

  Program program;
  program.items.push_back({Item{std::move(readingDecl)}, mkSpan()});
  program.items.push_back({Item{std::move(mainDecl)}, mkSpan()});

  auto module = generateMLIR(ctx, program);
  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  bool foundToJson = false;
  module.walk([&](mlir::func::FuncOp fn) {
    auto n = fn.getName();
    if (n.contains("Reading") && n.contains("to_json"))
      foundToJson = true;
  });
  if (!foundToJson) {
    FAIL(
        "Reading to_json wrapper not generated for static-style call (static demand-gate missing)");
    module.getOperation()->destroy();
    return;
  }

  // yaml / toml variants were never demanded.
  bool foundUncalled = false;
  module.walk([&](mlir::func::FuncOp fn) {
    auto n = fn.getName();
    if (!n.contains("Reading"))
      return;
    if (n.contains("to_yaml") || n.contains("from_yaml") || n.contains("to_toml") ||
        n.contains("from_toml"))
      foundUncalled = true;
  });
  if (foundUncalled) {
    FAIL("un-called Reading yaml/toml wrapper emitted unexpectedly (static demand-gate broken)");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Select emits explicit send-failure cleanup and panic paths
// ============================================================================
static void test_select_emits_send_failure_cleanup() {
  TEST(select_emits_send_failure_cleanup);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
actor Fast {
    let value: int;
    receive fn get() -> int {
        value
    }
}

actor Slow {
    let value: int;
    receive fn get() -> int {
        sleep_ms(50);
        value
    }
}

fn main() -> int {
    let fast = spawn Fast(value: 42);
    let slow = spawn Slow(value: 999);
    select {
        x from await fast.get() => x,
        y from await slow.get() => y,
        after 10ms => -1,
    }
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  auto mainFn = module.lookupSymbol<mlir::func::FuncOp>("main");
  if (!mainFn) {
    FAIL("main function not found");
    module.getOperation()->destroy();
    return;
  }

  if (countSelectAddOps(mainFn) != 2 || !allSelectAddsReturnI32(mainFn)) {
    FAIL("select.add should return an i32 status for every arm");
    module.getOperation()->destroy();
    return;
  }

  if (countAllPanics(mainFn) != 2) {
    FAIL("select should panic on failed sends");
    module.getOperation()->destroy();
    return;
  }

  int cancelCalls = countCallsByCallee(mainFn, "hew_reply_channel_cancel");
  if (cancelCalls != 5) {
    FAIL("select should clean up non-winners and every send-failure prefix");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Join emits explicit send-failure cleanup and preserves destroy paths
// ============================================================================
static void test_join_emits_send_failure_cleanup() {
  TEST(join_emits_send_failure_cleanup);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
actor Worker {
    let id: int;
    receive fn compute() -> int {
        id * 10
    }
}

fn main() -> int {
    let w1 = spawn Worker(id: 1);
    let w2 = spawn Worker(id: 2);
    let results = join {
        await w1.compute(),
        await w2.compute(),
    };
    results.0 + results.1
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  auto mainFn = module.lookupSymbol<mlir::func::FuncOp>("main");
  if (!mainFn) {
    FAIL("main function not found");
    module.getOperation()->destroy();
    return;
  }

  if (countSelectAddOps(mainFn) != 2 || !allSelectAddsReturnI32(mainFn)) {
    FAIL("join select.add should return an i32 status for every expression");
    module.getOperation()->destroy();
    return;
  }

  if (countAllPanics(mainFn) != 2) {
    FAIL("join should panic on failed sends");
    module.getOperation()->destroy();
    return;
  }

  if (countCallsByCallee(mainFn, "hew_reply_channel_cancel") != 3) {
    FAIL("join should cancel every already-created channel on send failure");
    module.getOperation()->destroy();
    return;
  }

  int destroyCount = 0;
  mainFn.walk([&](hew::SelectDestroyOp) { destroyCount++; });
  if (destroyCount != 5) {
    FAIL("join should destroy wait paths and every send-failure prefix");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Generator with wrapped yields (ExprCall variant ctor) lowers correctly
// ============================================================================
static void test_generator_wrapped_yield_drop_exclusion() {
  TEST(generator_wrapped_yield_drop_exclusion);

  mlir::MLIRContext ctx;
  initContext(ctx);
  // A generator that yields a Drop-implementing Token wrapped inside an
  // enum variant constructor (Item(t)).  With the bug, collectYieldExpr
  // skipped ExprCall nodes so `t` was NOT excluded from scope drops —
  // the generator body would emit a spurious drop at every yield site.
  // The module must generate and verify cleanly with the fix applied.
  auto module = generateMLIR(ctx, R"(
type Token {
    id: int;
}

impl Drop for Token {
    fn drop(t: Token) {
        println(t.id);
    }
}

enum Slot {
    Item(Token);
    Empty;
}

gen fn slotted(n: int) -> Slot {
    var i = 0;
    loop {
        if i >= n { break; }
        let t = Token { id: i + 1 };
        yield Item(t);
        i = i + 1;
    }
}

fn main() -> int {
    let g = slotted(1);
    match g.next() {
        Item(t) => { println(t.id); },
        Empty => {},
    }
    return 0;
}
  )");

  if (!module) {
    FAIL("MLIR generation failed for generator with wrapped yield");
    return;
  }

  if (mlir::failed(mlir::verify(module))) {
    FAIL("MLIR verification failed for generator with wrapped yield");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: actor receive handler with http.Request param emits hew_http_request_free
// ============================================================================

static void test_actor_receive_http_request_drop() {
  TEST(actor_receive_http_request_drop);

  mlir::MLIRContext ctx;
  initContext(ctx);

  // The handler must NOT contain an explicit .free() call.  The drop is
  // expected to be emitted automatically by the receive-handler param
  // registration in generateActorDecl().
  auto module = generateMLIR(ctx, R"(
import std::net::http;

actor Handler {
    receive fn handle(req: http.Request) {
        // req is owned by this handler; dropped via hew_http_request_free
    }
}

fn main() {}
  )");

  if (!module) {
    FAIL("MLIR generation failed for http.Request receive handler");
    return;
  }

  if (countCallsByCallee(module, "hew_http_request_free") < 1) {
    FAIL("expected hew_http_request_free call in actor receive handler");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: actor receive handler with http.Server param emits hew_http_server_close
// ============================================================================

static void test_actor_receive_http_server_drop() {
  TEST(actor_receive_http_server_drop);

  mlir::MLIRContext ctx;
  initContext(ctx);

  auto module = generateMLIR(ctx, R"(
import std::net::http;

actor ServerHolder {
    receive fn hold(srv: http.Server) {
        // srv is owned by this handler; dropped via hew_http_server_close
    }
}

fn main() {}
  )");

  if (!module) {
    FAIL("MLIR generation failed for http.Server receive handler");
    return;
  }

  if (countCallsByCallee(module, "hew_http_server_close") < 1) {
    FAIL("expected hew_http_server_close call in actor receive handler");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: actor receive handler with regex.Pattern param emits hew_regex_free
// ============================================================================

static void test_actor_receive_regex_pattern_drop() {
  TEST(actor_receive_regex_pattern_drop);

  mlir::MLIRContext ctx;
  initContext(ctx);

  auto module = generateMLIR(ctx, R"(
import std::text::regex;

actor Matcher {
    receive fn check(pat: Pattern) {
        // pat is owned by this handler; dropped via hew_regex_free
    }
}

fn main() {}
  )");

  if (!module) {
    FAIL("MLIR generation failed for regex.Pattern receive handler");
    return;
  }

  if (countCallsByCallee(module, "hew_regex_free") < 1) {
    FAIL("expected hew_regex_free call in actor receive handler");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: imported json.Value metadata drives scope-exit auto-drop
// ============================================================================

static void test_imported_json_value_scope_drop_uses_metadata() {
  TEST(imported_json_value_scope_drop_uses_metadata);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
import std::encoding::json;

fn main() {
    let val = json.parse("{\"name\":\"Hew\"}");
    let name = val.get_field("name");
    println(name.get_string());
    println(val.stringify());
}
  )",
                             program)) {
    FAIL("failed to load typed json scope-drop program");
    return;
  }

  bool jsonValueInHandleTypes = std::find(program.handle_types.begin(), program.handle_types.end(),
                                          "json.Value") != program.handle_types.end();
  if (!jsonValueInHandleTypes) {
    FAIL("json.Value missing from handle metadata");
    return;
  }

  auto dropIt = program.drop_funcs.find("json.Value");
  if (dropIt == program.drop_funcs.end() || dropIt->second != "hew_json_free") {
    FAIL("expected json.Value drop metadata to map to hew_json_free");
    return;
  }

  mlir::MLIRContext ctx;
  initContext(ctx);

  auto module = generateMLIR(ctx, program);
  if (!module) {
    FAIL("MLIR generation failed for json.Value scope-drop program");
    return;
  }

  auto mainFn = module.lookupSymbol<mlir::func::FuncOp>("main");
  if (!mainFn) {
    FAIL("expected main function for json.Value scope-drop program");
    module.getOperation()->destroy();
    return;
  }

  auto dropWrapper = lookupFuncBySuffix(module, "json.ValueF4drop");
  if (!dropWrapper) {
    FAIL("expected a generated json.Value drop wrapper");
    module.getOperation()->destroy();
    return;
  }

  if (countCallsByCallee(dropWrapper, "hew_json_free") < 1) {
    FAIL("expected generated json.Value drop wrapper to call hew_json_free");
    module.getOperation()->destroy();
    return;
  }

  if (countDropOpsByDropFn(mainFn, dropWrapper.getName(), true) < 2) {
    FAIL("expected scope-exit auto-drop to emit user drop ops for imported json.Value locals");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: typeExprToHandleString uses program.handle_types (drift fix)
//
// Regression guard: knownHandleTypes is populated from program.handle_types
// (the Rust type-checker's authoritative list), NOT from a hardcoded array.
// A type present in program.handle_types but absent from the old static list
// must be treated as a handle.  A type absent from program.handle_types (e.g.
// csv.Table, which is a struct in the real type checker) must NOT be treated
// as a handle even though it appeared in the old hardcoded array.
// ============================================================================

static void test_handle_registry_uses_metadata_not_hardcoded_list() {
  TEST(handle_registry_uses_metadata_not_hardcoded_list);

  // ── sub-test 1: type in handle_types → treated as handle ──────────────
  // Load a program that uses http.Request so the real handle_types is
  // populated by the Rust type checker.
  hew::ast::Program programWithHandle;
  if (!loadProgramFromSource(R"(
import std::net::http;

actor Sink {
    receive fn ingest(req: http.Request) {}
}

fn main() {}
  )",
                             programWithHandle)) {
    FAIL("failed to load typed program (sub-test 1)");
    return;
  }

  // http.Request must appear in the metadata-driven knownHandleTypes.
  bool httpRequestInHandleTypes =
      std::find(programWithHandle.handle_types.begin(), programWithHandle.handle_types.end(),
                "http.Request") != programWithHandle.handle_types.end();
  if (!httpRequestInHandleTypes) {
    FAIL("http.Request missing from program.handle_types — Rust metadata not populated");
    return;
  }

  // MLIR gen must emit a drop call for the handle.
  mlir::MLIRContext ctx1;
  initContext(ctx1);
  auto m1 = generateMLIR(ctx1, programWithHandle);
  if (!m1) {
    FAIL("MLIR generation failed (sub-test 1)");
    return;
  }
  if (countCallsByCallee(m1, "hew_http_request_free") < 1) {
    FAIL("expected hew_http_request_free — handle not recognised via metadata");
    m1.getOperation()->destroy();
    return;
  }
  m1.getOperation()->destroy();

  // ── sub-test 2: csv.Table must NOT be in handle_types ─────────────────
  // csv.Table was incorrectly hardcoded as a handle in the old static array
  // but is in fact a struct in the Rust type checker.  After the fix, it must
  // not be reported as a handle by the metadata-driven lookup.
  hew::ast::Program programWithCsv;
  if (!loadProgramFromSource(R"(
import std::encoding::csv;

fn take(tbl: csv.Table) {}
fn main() {}
  )",
                             programWithCsv)) {
    FAIL("could not load csv program");
    return;
  }

  bool csvTableInHandleTypes =
      std::find(programWithCsv.handle_types.begin(), programWithCsv.handle_types.end(),
                "csv.Table") != programWithCsv.handle_types.end();
  if (csvTableInHandleTypes) {
    FAIL("csv.Table incorrectly listed in program.handle_types — it is a struct");
    return;
  }

  PASS();
}

// ============================================================================
// Test: local non-void actor asks null-check reply pointers before loading
// ============================================================================

static void test_local_actor_non_void_ask_panics_on_null_reply_before_load() {
  TEST(local_actor_non_void_ask_panics_on_null_reply_before_load);

  mlir::MLIRContext ctx;
  initContext(ctx);

  auto module = generateMLIR(ctx, R"(
actor Stats {
    let value: int;
    receive fn snapshot() -> int {
        value
    }
}

fn main() -> int {
    let stats = spawn Stats(value: 42);
    await stats.snapshot()
}
  )");

  if (!module) {
    FAIL("MLIR generation failed for local actor ask");
    return;
  }

  hew::Codegen codegen(ctx);
  hew::CodegenOptions opts;
  opts.debug_info = true;
  llvm::LLVMContext llvmContext;
  auto llvmModule = codegen.buildLLVMModule(module, opts, llvmContext);
  module.getOperation()->destroy();

  if (!llvmModule) {
    FAIL("LLVM lowering failed for local actor ask");
    return;
  }

  auto *mainFn = llvmModule->getFunction("main");
  if (!mainFn) {
    FAIL("main function not found in lowered LLVM module");
    return;
  }

  llvm::CallBase *askCall = nullptr;
  for (auto &block : *mainFn) {
    for (auto &inst : block) {
      auto *call = llvm::dyn_cast<llvm::CallBase>(&inst);
      if (!call)
        continue;
      auto *callee = call->getCalledFunction();
      if (callee && callee->getName() == "hew_actor_ask") {
        askCall = call;
        break;
      }
    }
    if (askCall)
      break;
  }

  if (!askCall) {
    FAIL("expected lowered local ask to call hew_actor_ask");
    return;
  }

  llvm::ICmpInst *nullGuard = nullptr;
  llvm::BranchInst *guardBranch = nullptr;
  for (llvm::User *user : askCall->users()) {
    auto *icmp = llvm::dyn_cast<llvm::ICmpInst>(user);
    if (!icmp || icmp->getPredicate() != llvm::CmpInst::ICMP_EQ)
      continue;

    bool comparesAskToNull = (icmp->getOperand(0) == askCall &&
                              llvm::isa<llvm::ConstantPointerNull>(icmp->getOperand(1))) ||
                             (icmp->getOperand(1) == askCall &&
                              llvm::isa<llvm::ConstantPointerNull>(icmp->getOperand(0)));
    if (!comparesAskToNull)
      continue;

    auto *branch = llvm::dyn_cast<llvm::BranchInst>(icmp->getParent()->getTerminator());
    if (!branch || !branch->isConditional() || branch->getCondition() != icmp)
      continue;

    nullGuard = icmp;
    guardBranch = branch;
    break;
  }

  if (!nullGuard || !guardBranch) {
    FAIL("expected lowered local ask to branch on a null reply check");
    return;
  }

  if (guardBranch->getParent() != askCall->getParent() || !askCall->comesBefore(nullGuard)) {
    FAIL("null reply guard should be emitted immediately after the ask call");
    return;
  }

  llvm::BasicBlock *panicBlock = nullptr;
  llvm::BasicBlock *loadBlock = nullptr;
  for (unsigned i = 0; i < guardBranch->getNumSuccessors(); ++i) {
    auto *successor = guardBranch->getSuccessor(i);
    bool containsPanic = false;
    bool containsReplyLoad = false;
    for (auto &inst : *successor) {
      auto *call = llvm::dyn_cast<llvm::CallBase>(&inst);
      auto *callee = call ? call->getCalledFunction() : nullptr;
      if (callee && callee->getName() == "hew_panic")
        containsPanic = true;

      auto *load = llvm::dyn_cast<llvm::LoadInst>(&inst);
      if (load && load->getPointerOperand() == askCall)
        containsReplyLoad = true;
    }
    if (containsPanic)
      panicBlock = successor;
    if (containsReplyLoad)
      loadBlock = successor;
  }

  if (!panicBlock) {
    FAIL("expected a null-reply successor block that fails via hew_panic");
    return;
  }

  if (!loadBlock) {
    FAIL("expected a non-null successor block that loads the reply value");
    return;
  }

  if (panicBlock == loadBlock) {
    FAIL("panic and reply-load paths must be in distinct successor blocks");
    return;
  }

  int loadPredCount = 0;
  bool loadPredIsGuard = false;
  for (auto *pred : llvm::predecessors(loadBlock)) {
    ++loadPredCount;
    if (pred == guardBranch->getParent())
      loadPredIsGuard = true;
  }

  if (loadPredCount != 1 || !loadPredIsGuard) {
    FAIL("reply load must only be reachable from the non-null guard edge");
    return;
  }

  for (auto *succ : llvm::successors(panicBlock)) {
    if (succ == loadBlock) {
      FAIL("panic path must not fall through into the reply-load block");
      return;
    }
  }

  if (!panicBlock->getTerminator()) {
    FAIL("panic block should terminate explicitly");
    return;
  }

  PASS();
}

// ============================================================================
// Test: remote void actor asks do not free the reply sentinel
// ============================================================================

static void test_remote_actor_void_ask_does_not_free_reply() {
  TEST(remote_actor_void_ask_does_not_free_reply);

  mlir::MLIRContext ctx;
  initContext(ctx);

  auto module = generateMLIR(ctx, R"(
actor Stats {
    receive fn refresh() {
    }
}

fn main() {
    let remote: Stats = Node::lookup("stats");
    await remote.refresh();
}
  )");

  if (!module) {
    FAIL("MLIR generation failed for remote void actor ask");
    return;
  }

  hew::Codegen codegen(ctx);
  hew::CodegenOptions opts;
  opts.debug_info = true;
  llvm::LLVMContext llvmContext;
  auto llvmModule = codegen.buildLLVMModule(module, opts, llvmContext);
  module.getOperation()->destroy();

  if (!llvmModule) {
    FAIL("LLVM lowering failed for remote void actor ask");
    return;
  }

  auto *mainFn = llvmModule->getFunction("main");
  if (!mainFn) {
    FAIL("main function not found in lowered LLVM module");
    return;
  }

  llvm::CallBase *askCall = nullptr;
  bool freesAskReply = false;
  for (auto &block : *mainFn) {
    for (auto &inst : block) {
      auto *call = llvm::dyn_cast<llvm::CallBase>(&inst);
      if (!call)
        continue;
      auto *callee = call->getCalledFunction();
      if (!callee)
        continue;
      if (callee->getName() == "hew_node_api_ask")
        askCall = call;
      if (callee->getName() == "free" && call->arg_size() == 1 && call->getArgOperand(0) == askCall)
        freesAskReply = true;
    }
  }

  if (!askCall) {
    FAIL("expected lowered remote void ask to call hew_node_api_ask");
    return;
  }
  if (freesAskReply) {
    FAIL("remote void ask should not free the reply pointer");
    return;
  }

  PASS();
}

// ============================================================================
// Test: remote actor asks pass an explicit reply size to hew_node_api_ask
// ============================================================================

static void test_remote_actor_ask_passes_reply_size() {
  TEST(remote_actor_ask_passes_reply_size);

  mlir::MLIRContext ctx;
  initContext(ctx);

  auto module = generateMLIR(ctx, R"(
actor Stats {
    receive fn snapshot() -> (int, int) {
        (10, 20)
    }
}

fn main() -> int {
    let remote: Stats = Node::lookup("stats");
    let snapshot = await remote.snapshot();
    snapshot.0 + snapshot.1
}
  )");

  if (!module) {
    FAIL("MLIR generation failed for remote actor ask");
    return;
  }

  hew::Codegen codegen(ctx);
  hew::CodegenOptions opts;
  opts.debug_info = true;
  llvm::LLVMContext llvmContext;
  auto llvmModule = codegen.buildLLVMModule(module, opts, llvmContext);
  module.getOperation()->destroy();

  if (!llvmModule) {
    FAIL("LLVM lowering failed for remote actor ask");
    return;
  }

  auto *mainFn = llvmModule->getFunction("main");
  if (!mainFn) {
    FAIL("main function not found in lowered LLVM module");
    return;
  }

  llvm::CallBase *askCall = nullptr;
  for (auto &block : *mainFn) {
    for (auto &inst : block) {
      auto *call = llvm::dyn_cast<llvm::CallBase>(&inst);
      if (!call)
        continue;
      auto *callee = call->getCalledFunction();
      if (callee && callee->getName() == "hew_node_api_ask") {
        askCall = call;
        break;
      }
    }
    if (askCall)
      break;
  }

  if (!askCall) {
    FAIL("expected lowered remote ask to call hew_node_api_ask");
    return;
  }

  if (askCall->arg_size() != 5) {
    FAIL("remote ask should pass a fifth reply_size operand");
    return;
  }

  auto *replySize = llvm::dyn_cast<llvm::ConstantInt>(askCall->getArgOperand(4));
  if (!replySize || replySize->getZExtValue() != 16) {
    FAIL("remote ask should pass the tuple reply size as the fifth operand");
    return;
  }

  PASS();
}

// ============================================================================
// Test: remote actor asks panic on a null reply sentinel before loading
// ============================================================================

static void test_remote_actor_ask_panics_on_null_reply() {
  TEST(remote_actor_ask_panics_on_null_reply);

  mlir::MLIRContext ctx;
  initContext(ctx);

  auto module = generateMLIR(ctx, R"(
actor Stats {
    receive fn snapshot() -> int {
        10
    }
}

fn main() -> int {
    let remote: Stats = Node::lookup("stats");
    await remote.snapshot()
}
  )");

  if (!module) {
    FAIL("MLIR generation failed for remote actor ask null-reply panic test");
    return;
  }

  hew::Codegen codegen(ctx);
  hew::CodegenOptions opts;
  opts.debug_info = true;
  llvm::LLVMContext llvmContext;
  auto llvmModule = codegen.buildLLVMModule(module, opts, llvmContext);
  module.getOperation()->destroy();

  if (!llvmModule) {
    FAIL("LLVM lowering failed for remote actor ask null-reply panic test");
    return;
  }

  auto *mainFn = llvmModule->getFunction("main");
  if (!mainFn) {
    FAIL("main function not found in lowered LLVM module");
    return;
  }

  llvm::CallBase *askCall = nullptr;
  llvm::CallBase *panicCall = nullptr;
  for (auto &block : *mainFn) {
    for (auto &inst : block) {
      auto *call = llvm::dyn_cast<llvm::CallBase>(&inst);
      if (!call)
        continue;
      auto *callee = call->getCalledFunction();
      if (!callee)
        continue;
      if (callee->getName() == "hew_node_api_ask")
        askCall = call;
      if (callee->getName() == "hew_panic")
        panicCall = call;
    }
  }

  if (!askCall) {
    FAIL("expected lowered remote ask to call hew_node_api_ask");
    return;
  }
  if (!panicCall) {
    FAIL("expected lowered remote ask to call hew_panic on null reply");
    return;
  }

  llvm::ICmpInst *nullReplyCheck = nullptr;
  for (auto &block : *mainFn) {
    for (auto &inst : block) {
      auto *icmp = llvm::dyn_cast<llvm::ICmpInst>(&inst);
      if (!icmp || icmp->getPredicate() != llvm::CmpInst::ICMP_EQ)
        continue;

      auto *lhs = icmp->getOperand(0);
      auto *rhs = icmp->getOperand(1);
      bool comparesAskAgainstNull = (lhs == askCall && llvm::isa<llvm::ConstantPointerNull>(rhs)) ||
                                    (rhs == askCall && llvm::isa<llvm::ConstantPointerNull>(lhs));
      if (comparesAskAgainstNull) {
        nullReplyCheck = icmp;
        break;
      }
    }
    if (nullReplyCheck)
      break;
  }

  if (!nullReplyCheck) {
    FAIL("expected lowered remote ask to compare the reply pointer against null");
    return;
  }

  bool nullCheckBranchesToPanic = false;
  for (auto &block : *mainFn) {
    auto *branch = llvm::dyn_cast<llvm::BranchInst>(block.getTerminator());
    if (!branch || !branch->isConditional() || branch->getCondition() != nullReplyCheck)
      continue;

    for (unsigned i = 0; i < branch->getNumSuccessors(); ++i) {
      if (branch->getSuccessor(i) == panicCall->getParent()) {
        nullCheckBranchesToPanic = true;
        break;
      }
    }
  }

  if (!nullCheckBranchesToPanic) {
    FAIL("expected the null reply check to branch to the hew_panic block");
    return;
  }

  PASS();
}

int main() {
  printf("=== Hew MLIRGen Tests ===\n");

  test_simple_add();
  test_fibonacci();
  test_mutable_variables();
  test_print();
  test_print_no_newline();
  test_print_runtime_dispatch_kinds();
  test_while_loop();
  test_if_else_expr();
  test_statement_position_if_and_match_lower_without_results();
  test_unannotated_early_return_tail_if_match_lower_without_results();
  test_non_void_nested_stmt_if_no_results();
  test_discarded_block_expr_tail_if_no_extra_results();
  test_discarded_unsafe_expr_tail_if_no_extra_results();
  test_discarded_block_expr_bad_tail_fails_closed();
  test_discarded_unsafe_expr_bad_tail_fails_closed();
  test_statement_style_match_arm_bad_body_fails_closed();
  test_statement_style_match_arm_block_tail_if_fails_closed();
  test_statement_style_match_arm_block_tail_match_fails_closed();
  test_statement_style_match_arm_statement_only_if_tail_lowers();
  test_statement_style_match_arm_statement_only_match_tail_lowers();
  test_discarded_scope_expr_tail_if_no_extra_results();
  test_nested_discarded_scope_expr_tail_if_no_extra_results();
  test_discarded_scope_wrapper_tail_if_no_extra_results();
  test_discarded_scope_expr_tail_match_no_extra_results();
  test_nested_discarded_scope_expr_tail_match_no_extra_results();
  test_arithmetic();
  test_comparisons();
  test_materialized_unsigned_range_uses_unsigned_compare();
  test_return_stmt();
  test_logical_ops();
  test_unary_ops();
  test_compound_assignment();
  test_function_calls();
  test_void_function();
  test_void_trailing_if_match_stmt_lowering();
  test_stmt_match_last_arm_emits_panic();
  test_stmt_match_unmatched_panic_block_does_not_fall_through();
  test_resultful_match_missing_arm_body_fails_closed();
  test_resultful_match_missing_else_chain_value_fails_closed();
  test_resultful_match_nested_arm_error_is_not_double_reported();
  test_resultful_match_panic_arm_lowers();
  test_builtin_enum_constructors_use_explicit_payload_positions();
  test_unresolved_named_type_fails();
  test_wire_encode_uses_heap_buffer();
  test_wire_bytes_use_base64_serial_helpers();
  test_wire_enum_mixed_payload_layout();
  test_wire_enum_typedecl_preserves_variants();
  test_wire_enum_mixed_payload_match_positions();
  test_wire_enum_unit_serial_helpers_and_dispatch();
  test_unresolved_generic_substitution_type_fails();
  test_unsupported_return_coercion_stops_before_verifier();
  test_select_emits_send_failure_cleanup();
  test_join_emits_send_failure_cleanup();
  test_generator_wrapped_yield_drop_exclusion();
  test_actor_receive_http_request_drop();
  test_actor_receive_http_server_drop();
  test_actor_receive_regex_pattern_drop();
  test_imported_json_value_scope_drop_uses_metadata();
  test_handle_registry_uses_metadata_not_hardcoded_list();
  test_local_actor_non_void_ask_panics_on_null_reply_before_load();
  test_remote_actor_void_ask_does_not_free_reply();
  test_remote_actor_ask_passes_reply_size();
  test_remote_actor_ask_panics_on_null_reply();
  test_generic_struct_constructor_in_nongeneric_context();
  test_generic_struct_constructor_monomorphic_helper();
  test_prim_struct_no_serial_call_emits_no_wrappers();
  test_prim_struct_instance_serial_call_emits_demanded_wrapper();
  test_prim_struct_static_serial_call_emits_demanded_wrapper();

  printf("\n%d/%d tests passed.\n", tests_passed, tests_run);
  return (tests_passed == tests_run) ? 0 : 1;
}
