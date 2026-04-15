//===- test_mlirgen.cpp - Tests for Hew AST-to-MLIR lowering --------------===//
//
// Verifies that the MLIRGen class correctly lowers various Hew AST constructs
// to valid MLIR operations. Each test parses a Hew source string, runs
// MLIRGen, verifies the module, and optionally prints the MLIR IR.
//
//===----------------------------------------------------------------------===//

#include "../src/mlir/MLIRGenHelpers.h"
#include "hew/ast_helpers.h"
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

#include <algorithm>
#include <cassert>
#include <cstdint>
#include <cstdio>
#include <optional>
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

static int countUserDropOps(mlir::Operation *op) {
  int count = 0;
  op->walk([&](hew::DropOp drop) {
    if (drop.getIsUserDrop())
      count++;
  });
  return count;
}

static bool hasZeroInitializedUserDropSlot(mlir::Operation *op) {
  bool found = false;
  op->walk([&](hew::DropOp drop) {
    if (found || !drop.getIsUserDrop())
      return;

    auto load = drop.getValue().getDefiningOp<mlir::memref::LoadOp>();
    if (!load)
      return;

    auto slot = load.getMemref();
    auto alloca = slot.getDefiningOp<mlir::memref::AllocaOp>();
    if (!alloca)
      return;

    for (mlir::Operation &candidate : *alloca->getBlock()) {
      auto store = llvm::dyn_cast<mlir::memref::StoreOp>(candidate);
      if (!store || store.getMemref() != slot)
        continue;
      auto zero = store.getValue().getDefiningOp<mlir::LLVM::ZeroOp>();
      if (zero && mlir::isa<mlir::LLVM::LLVMStructType>(zero.getType())) {
        found = true;
        break;
      }
    }
  });
  return found;
}

static bool isZeroLiteralValue(mlir::Value value) {
  if (value.getDefiningOp<mlir::LLVM::ZeroOp>())
    return true;

  if (auto constant = value.getDefiningOp<mlir::arith::ConstantOp>()) {
    auto attr = constant.getValue();
    if (auto intAttr = llvm::dyn_cast<mlir::IntegerAttr>(attr))
      return intAttr.getValue().isZero();
    if (auto floatAttr = llvm::dyn_cast<mlir::FloatAttr>(attr))
      return floatAttr.getValue().isZero();
  }

  return false;
}

static bool isIntegerLiteralValue(mlir::Value value, int64_t expected) {
  if (expected == 0 && isZeroLiteralValue(value))
    return true;

  if (auto constant = value.getDefiningOp<mlir::arith::ConstantOp>()) {
    if (auto intAttr = llvm::dyn_cast<mlir::IntegerAttr>(constant.getValue()))
      return intAttr.getValue().getLimitedValue() == static_cast<uint64_t>(expected);
  }

  return false;
}

static bool hasInitFlagGuardedUserDrop(mlir::Operation *op) {
  bool found = false;
  op->walk([&](mlir::scf::IfOp ifOp) {
    if (found)
      return;

    bool guardedUserDrop = false;
    ifOp.getThenRegion().walk([&](hew::DropOp drop) {
      if (drop.getIsUserDrop())
        guardedUserDrop = true;
    });
    if (!guardedUserDrop)
      return;

    auto load = ifOp.getCondition().getDefiningOp<mlir::memref::LoadOp>();
    if (!load)
      return;

    auto memrefTy = mlir::dyn_cast<mlir::MemRefType>(load.getMemref().getType());
    if (!memrefTy)
      return;
    auto intTy = mlir::dyn_cast<mlir::IntegerType>(memrefTy.getElementType());
    if (!intTy || intTy.getWidth() != 1)
      return;

    bool sawFalse = false;
    bool sawTrue = false;
    op->walk([&](mlir::memref::StoreOp store) {
      if (store.getMemref() != load.getMemref())
        return;
      sawFalse |= isIntegerLiteralValue(store.getValue(), 0);
      sawTrue |= isIntegerLiteralValue(store.getValue(), 1);
    });
    found = sawFalse && sawTrue;
  });
  return found;
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

static std::vector<hew::ast::Spanned<hew::ast::Item>> *rootItems(hew::ast::Program &program) {
  if (program.module_graph) {
    auto it = program.module_graph->modules.find(program.module_graph->root);
    if (it != program.module_graph->modules.end())
      return &it->second.items;
  }
  return &program.items;
}

static hew::ast::FnDecl *findFunctionDecl(hew::ast::Program &program, llvm::StringRef name) {
  for (auto &item : *rootItems(program)) {
    if (auto *fn = std::get_if<hew::ast::FnDecl>(&item.value.kind); fn && fn->name == name)
      return fn;
  }
  return nullptr;
}

static hew::ast::StmtFor *findFirstForStmt(hew::ast::FnDecl &fn) {
  for (auto &stmt : fn.body.stmts) {
    if (auto *forStmt = std::get_if<hew::ast::StmtFor>(&stmt->value.kind))
      return forStmt;
  }
  return nullptr;
}

static hew::ast::StmtMatch *findFirstMatchStmt(hew::ast::FnDecl &fn) {
  for (auto &stmt : fn.body.stmts) {
    if (auto *matchStmt = std::get_if<hew::ast::StmtMatch>(&stmt->value.kind))
      return matchStmt;
  }
  return nullptr;
}

static hew::ast::StmtIfLet *findFirstIfLetStmt(hew::ast::FnDecl &fn) {
  for (auto &stmt : fn.body.stmts) {
    if (auto *ifLetStmt = std::get_if<hew::ast::StmtIfLet>(&stmt->value.kind))
      return ifLetStmt;
  }
  return nullptr;
}

static hew::ast::StmtWhileLet *findFirstWhileLetStmt(hew::ast::FnDecl &fn) {
  for (auto &stmt : fn.body.stmts) {
    if (auto *whileLetStmt = std::get_if<hew::ast::StmtWhileLet>(&stmt->value.kind))
      return whileLetStmt;
  }
  return nullptr;
}

static hew::ast::Spanned<hew::ast::Expr> *findReturnedMatchExpr(hew::ast::FnDecl &fn) {
  for (auto &stmt : fn.body.stmts) {
    auto *retStmt = std::get_if<hew::ast::StmtReturn>(&stmt->value.kind);
    if (!retStmt || !retStmt->value)
      continue;
    if (std::holds_alternative<hew::ast::ExprMatch>(retStmt->value->value.kind))
      return &*retStmt->value;
  }
  return nullptr;
}

static hew::ast::ExprIfLet *findReturnedIfLetExpr(hew::ast::FnDecl &fn) {
  for (auto &stmt : fn.body.stmts) {
    auto *retStmt = std::get_if<hew::ast::StmtReturn>(&stmt->value.kind);
    if (!retStmt || !retStmt->value)
      continue;
    return std::get_if<hew::ast::ExprIfLet>(&retStmt->value->value.kind);
  }
  return nullptr;
}

static bool hasMissingIndirectEnumScrutineeDiag(const std::string &stderrText) {
  return stderrText.find("missing expr_types entry for") != std::string::npos &&
         stderrText.find("indirect enum") != std::string::npos;
}

static hew::ast::Program
makeIdentifierAssignmentAuthorityProgram(hew::ast::AssignTargetKindData targetKind) {
  using namespace hew::ast;

  uint64_t nextSpan = 920000000000ULL;
  auto mkSpan = [&]() -> Span {
    auto start = nextSpan;
    nextSpan += 8;
    return {start, start + 1};
  };
  auto mkType = [&](llvm::StringRef name) -> Spanned<TypeExpr> {
    TypeExpr typeExpr;
    typeExpr.kind = TypeNamed{name.str(), std::nullopt};
    auto span = mkSpan();
    return {std::move(typeExpr), span};
  };
  auto mkIntExpr = [&](int64_t value) -> Spanned<Expr> {
    Expr expr;
    auto span = mkSpan();
    expr.kind = ExprLiteral{LitInteger{value}};
    expr.span = span;
    return {std::move(expr), span};
  };
  auto mkIdentExpr = [&](llvm::StringRef name) -> Spanned<Expr> {
    Expr expr;
    auto span = mkSpan();
    expr.kind = ExprIdentifier{name.str()};
    expr.span = span;
    return {std::move(expr), span};
  };
  auto mkStmt = [&](auto node) -> std::unique_ptr<Spanned<Stmt>> {
    auto span = mkSpan();
    Stmt stmt;
    stmt.kind = std::move(node);
    stmt.span = span;
    return std::make_unique<Spanned<Stmt>>(Spanned<Stmt>{std::move(stmt), span});
  };

  StmtVar varStmt;
  varStmt.name = "local";
  varStmt.ty = mkType("int");
  varStmt.value = mkIntExpr(0);

  StmtAssign assignStmt;
  assignStmt.target = mkIdentExpr("local");
  auto targetSpan = assignStmt.target.span;
  assignStmt.value = mkIntExpr(1);

  FnDecl mainFn;
  mainFn.is_async = false;
  mainFn.is_generator = false;
  mainFn.is_pure = false;
  mainFn.name = "main";
  mainFn.return_type = mkType("int");
  mainFn.body.stmts.push_back(mkStmt(std::move(varStmt)));
  mainFn.body.stmts.push_back(mkStmt(std::move(assignStmt)));
  mainFn.body.trailing_expr = std::make_unique<Spanned<Expr>>(mkIntExpr(0));

  Item mainItem;
  mainItem.kind = std::move(mainFn);

  Program program;
  program.schema_version = 5;
  program.items.push_back({std::move(mainItem), mkSpan()});
  program.assign_target_kinds.push_back({targetSpan.start, targetSpan.end, std::move(targetKind)});
  program.assign_target_shapes.push_back({targetSpan.start, targetSpan.end, /*is_unsigned=*/false});
  return program;
}

static hew::ast::Program
makeActorFieldAssignmentAuthorityProgram(hew::ast::AssignTargetKindData targetKind) {
  using namespace hew::ast;

  uint64_t nextSpan = 930000000000ULL;
  auto mkSpan = [&]() -> Span {
    auto start = nextSpan;
    nextSpan += 8;
    return {start, start + 1};
  };
  auto mkType = [&](llvm::StringRef name) -> Spanned<TypeExpr> {
    TypeExpr typeExpr;
    typeExpr.kind = TypeNamed{name.str(), std::nullopt};
    auto span = mkSpan();
    return {std::move(typeExpr), span};
  };
  auto mkIntExpr = [&](int64_t value) -> Spanned<Expr> {
    Expr expr;
    auto span = mkSpan();
    expr.kind = ExprLiteral{LitInteger{value}};
    expr.span = span;
    return {std::move(expr), span};
  };
  auto mkIdentExpr = [&](llvm::StringRef name) -> Spanned<Expr> {
    Expr expr;
    auto span = mkSpan();
    expr.kind = ExprIdentifier{name.str()};
    expr.span = span;
    return {std::move(expr), span};
  };
  auto mkStmt = [&](auto node) -> std::unique_ptr<Spanned<Stmt>> {
    auto span = mkSpan();
    Stmt stmt;
    stmt.kind = std::move(node);
    stmt.span = span;
    return std::make_unique<Spanned<Stmt>>(Spanned<Stmt>{std::move(stmt), span});
  };

  FieldDecl field;
  field.name = "total";
  field.ty = mkType("int");

  Param valueParam;
  valueParam.name = "v";
  valueParam.ty = mkType("int");
  valueParam.is_mutable = false;

  StmtAssign assignStmt;
  assignStmt.target = mkIdentExpr("total");
  auto targetSpan = assignStmt.target.span;
  assignStmt.value = mkIdentExpr("v");

  ReceiveFnDecl receive;
  receive.is_generator = false;
  receive.is_pure = false;
  receive.name = "set";
  receive.params.push_back(std::move(valueParam));
  receive.body.stmts.push_back(mkStmt(std::move(assignStmt)));
  receive.span = mkSpan();

  ActorDecl actor;
  actor.name = "Counter";
  actor.fields.push_back(std::move(field));
  actor.receive_fns.push_back(std::move(receive));

  FnDecl mainFn;
  mainFn.is_async = false;
  mainFn.is_generator = false;
  mainFn.is_pure = false;
  mainFn.name = "main";
  mainFn.return_type = mkType("int");
  mainFn.body.trailing_expr = std::make_unique<Spanned<Expr>>(mkIntExpr(0));

  Item actorItem;
  actorItem.kind = std::move(actor);
  Item mainItem;
  mainItem.kind = std::move(mainFn);

  Program program;
  program.schema_version = 5;
  program.items.push_back({std::move(actorItem), mkSpan()});
  program.items.push_back({std::move(mainItem), mkSpan()});
  program.assign_target_kinds.push_back({targetSpan.start, targetSpan.end, std::move(targetKind)});
  program.assign_target_shapes.push_back({targetSpan.start, targetSpan.end, /*is_unsigned=*/false});
  return program;
}

static hew::ast::Program makeMissingStructFieldAssignmentProgram() {
  using namespace hew::ast;

  uint64_t nextSpan = 940000000000ULL;
  auto mkSpan = [&]() -> Span {
    auto start = nextSpan;
    nextSpan += 8;
    return {start, start + 1};
  };
  auto mkType = [&](llvm::StringRef name) -> Spanned<TypeExpr> {
    TypeExpr typeExpr;
    typeExpr.kind = TypeNamed{name.str(), std::nullopt};
    auto span = mkSpan();
    return {std::move(typeExpr), span};
  };
  auto mkIntExpr = [&](int64_t value) -> Spanned<Expr> {
    Expr expr;
    auto span = mkSpan();
    expr.kind = ExprLiteral{LitInteger{value}};
    expr.span = span;
    return {std::move(expr), span};
  };
  auto mkIdentExpr = [&](llvm::StringRef name) -> Spanned<Expr> {
    Expr expr;
    auto span = mkSpan();
    expr.kind = ExprIdentifier{name.str()};
    expr.span = span;
    return {std::move(expr), span};
  };
  auto mkFieldAccessExpr = [&](llvm::StringRef objectName,
                               llvm::StringRef fieldName) -> Spanned<Expr> {
    Expr expr;
    auto span = mkSpan();
    expr.kind =
        ExprFieldAccess{std::make_unique<Spanned<Expr>>(mkIdentExpr(objectName)), fieldName.str()};
    expr.span = span;
    return {std::move(expr), span};
  };
  auto mkStmt = [&](auto node) -> std::unique_ptr<Spanned<Stmt>> {
    auto span = mkSpan();
    Stmt stmt;
    stmt.kind = std::move(node);
    stmt.span = span;
    return std::make_unique<Spanned<Stmt>>(Spanned<Stmt>{std::move(stmt), span});
  };

  TypeDecl boxed;
  boxed.visibility = Visibility::Pub;
  boxed.kind = TypeDeclKind::Struct;
  boxed.name = "Boxed";
  TypeBodyItem boxedFieldItem;
  boxedFieldItem.kind = TypeBodyItemField{"value", mkType("int")};
  boxed.body.push_back(std::move(boxedFieldItem));

  StmtVar varStmt;
  varStmt.name = "boxed";
  varStmt.ty = mkType("Boxed");
  varStmt.value = std::nullopt;

  StmtAssign assignStmt;
  assignStmt.target = mkFieldAccessExpr("boxed", "missing");
  auto targetSpan = assignStmt.target.span;
  assignStmt.value = mkIntExpr(1);

  FnDecl mainFn;
  mainFn.is_async = false;
  mainFn.is_generator = false;
  mainFn.is_pure = false;
  mainFn.visibility = Visibility::Pub;
  mainFn.name = "main";
  mainFn.return_type = mkType("int");
  mainFn.body.stmts.push_back(mkStmt(std::move(varStmt)));
  mainFn.body.stmts.push_back(mkStmt(std::move(assignStmt)));
  mainFn.body.trailing_expr = std::make_unique<Spanned<Expr>>(mkIntExpr(0));

  Item typeItem;
  typeItem.kind = std::move(boxed);
  Item mainItem;
  mainItem.kind = std::move(mainFn);

  Program program;
  program.schema_version = 5;
  program.items.push_back({std::move(typeItem), mkSpan()});
  program.items.push_back({std::move(mainItem), mkSpan()});
  program.assign_target_kinds.push_back(
      {targetSpan.start, targetSpan.end, AssignTargetKindFieldAccess{}});
  program.assign_target_shapes.push_back({targetSpan.start, targetSpan.end, /*is_unsigned=*/false});
  return program;
}

static hew::ast::Program makeNonStructFieldAssignmentProgram() {
  using namespace hew::ast;

  uint64_t nextSpan = 950000000000ULL;
  auto mkSpan = [&]() -> Span {
    auto start = nextSpan;
    nextSpan += 8;
    return {start, start + 1};
  };
  auto mkType = [&](llvm::StringRef name) -> Spanned<TypeExpr> {
    TypeExpr typeExpr;
    typeExpr.kind = TypeNamed{name.str(), std::nullopt};
    auto span = mkSpan();
    return {std::move(typeExpr), span};
  };
  auto mkIntExpr = [&](int64_t value) -> Spanned<Expr> {
    Expr expr;
    auto span = mkSpan();
    expr.kind = ExprLiteral{LitInteger{value}};
    expr.span = span;
    return {std::move(expr), span};
  };
  auto mkIdentExpr = [&](llvm::StringRef name) -> Spanned<Expr> {
    Expr expr;
    auto span = mkSpan();
    expr.kind = ExprIdentifier{name.str()};
    expr.span = span;
    return {std::move(expr), span};
  };
  auto mkFieldAccessExpr = [&](llvm::StringRef objectName,
                               llvm::StringRef fieldName) -> Spanned<Expr> {
    Expr expr;
    auto span = mkSpan();
    expr.kind =
        ExprFieldAccess{std::make_unique<Spanned<Expr>>(mkIdentExpr(objectName)), fieldName.str()};
    expr.span = span;
    return {std::move(expr), span};
  };
  auto mkStmt = [&](auto node) -> std::unique_ptr<Spanned<Stmt>> {
    auto span = mkSpan();
    Stmt stmt;
    stmt.kind = std::move(node);
    stmt.span = span;
    return std::make_unique<Spanned<Stmt>>(Spanned<Stmt>{std::move(stmt), span});
  };

  StmtVar varStmt;
  varStmt.name = "count";
  varStmt.ty = mkType("int");
  varStmt.value = mkIntExpr(0);

  StmtAssign assignStmt;
  assignStmt.target = mkFieldAccessExpr("count", "value");
  auto targetSpan = assignStmt.target.span;
  assignStmt.value = mkIntExpr(1);

  FnDecl mainFn;
  mainFn.is_async = false;
  mainFn.is_generator = false;
  mainFn.is_pure = false;
  mainFn.visibility = Visibility::Pub;
  mainFn.name = "main";
  mainFn.return_type = mkType("int");
  mainFn.body.stmts.push_back(mkStmt(std::move(varStmt)));
  mainFn.body.stmts.push_back(mkStmt(std::move(assignStmt)));
  mainFn.body.trailing_expr = std::make_unique<Spanned<Expr>>(mkIntExpr(0));

  Item mainItem;
  mainItem.kind = std::move(mainFn);

  Program program;
  program.schema_version = 5;
  program.items.push_back({std::move(mainItem), mkSpan()});
  program.assign_target_kinds.push_back(
      {targetSpan.start, targetSpan.end, AssignTargetKindFieldAccess{}});
  program.assign_target_shapes.push_back({targetSpan.start, targetSpan.end, /*is_unsigned=*/false});
  return program;
}

static bool eraseExprTypeEntryForSpan(hew::ast::Program &program, const hew::ast::Span &span) {
  auto oldSize = program.expr_types.size();
  program.expr_types.erase(std::remove_if(program.expr_types.begin(), program.expr_types.end(),
                                          [&](const hew::ast::ExprTypeEntry &entry) {
                                            return entry.start == span.start &&
                                                   entry.end == span.end;
                                          }),
                           program.expr_types.end());
  return program.expr_types.size() != oldSize;
}

static bool replaceStreamElementExprTypeForSpan(hew::ast::Program &program,
                                                const hew::ast::Span &span,
                                                llvm::StringRef elementTypeName) {
  bool replacedAny = false;
  for (auto &entry : program.expr_types) {
    if (entry.start != span.start || entry.end != span.end)
      continue;

    auto *streamType = std::get_if<hew::ast::TypeNamed>(&entry.ty.value.kind);
    if (!streamType || !streamType->type_args || streamType->type_args->size() != 1)
      continue;

    auto *elementType =
        std::get_if<hew::ast::TypeNamed>(&streamType->type_args->front().value.kind);
    if (!elementType)
      continue;

    elementType->name = elementTypeName.str();
    elementType->type_args = std::nullopt;
    replacedAny = true;
  }
  return replacedAny;
}

static bool eraseMethodCallReceiverKindEntryForSpan(hew::ast::Program &program,
                                                    const hew::ast::Span &span) {
  auto oldSize = program.method_call_receiver_kinds.size();
  program.method_call_receiver_kinds.erase(
      std::remove_if(program.method_call_receiver_kinds.begin(),
                     program.method_call_receiver_kinds.end(),
                     [&](const hew::ast::MethodCallReceiverKindEntry &entry) {
                       return entry.start == span.start && entry.end == span.end;
                     }),
      program.method_call_receiver_kinds.end());
  return program.method_call_receiver_kinds.size() != oldSize;
}

static bool eraseAssignTargetKindEntryForSpan(hew::ast::Program &program,
                                              const hew::ast::Span &span) {
  auto oldSize = program.assign_target_kinds.size();
  program.assign_target_kinds.erase(
      std::remove_if(program.assign_target_kinds.begin(), program.assign_target_kinds.end(),
                     [&](const hew::ast::AssignTargetKindEntry &entry) {
                       return entry.start == span.start && entry.end == span.end;
                     }),
      program.assign_target_kinds.end());
  return program.assign_target_kinds.size() != oldSize;
}

static bool eraseAssignTargetShapeEntryForSpan(hew::ast::Program &program,
                                               const hew::ast::Span &span) {
  auto oldSize = program.assign_target_shapes.size();
  program.assign_target_shapes.erase(
      std::remove_if(program.assign_target_shapes.begin(), program.assign_target_shapes.end(),
                     [&](const hew::ast::AssignTargetShapeEntry &entry) {
                       return entry.start == span.start && entry.end == span.end;
                     }),
      program.assign_target_shapes.end());
  return program.assign_target_shapes.size() != oldSize;
}

static bool eraseLoweringFactEntryForSpan(hew::ast::Program &program, const hew::ast::Span &span) {
  auto oldSize = program.lowering_facts.size();
  program.lowering_facts.erase(
      std::remove_if(program.lowering_facts.begin(), program.lowering_facts.end(),
                     [&](const hew::ast::LoweringFactEntry &entry) {
                       return entry.start == span.start && entry.end == span.end;
                     }),
      program.lowering_facts.end());
  return program.lowering_facts.size() != oldSize;
}

static std::optional<hew::ast::Span> findFunctionAssignTargetSpan(const hew::ast::FnDecl &fn) {
  for (const auto &stmt : fn.body.stmts) {
    const auto *assign = std::get_if<hew::ast::StmtAssign>(&stmt->value.kind);
    if (assign)
      return assign->target.span;
  }
  return std::nullopt;
}

static std::optional<hew::ast::Span> restoreReturnedHandleMethodCall(hew::ast::FnDecl &fn,
                                                                     llvm::StringRef callee,
                                                                     llvm::StringRef methodName) {
  for (auto &stmt : fn.body.stmts) {
    auto *retStmt = std::get_if<hew::ast::StmtReturn>(&stmt->value.kind);
    if (!retStmt || !retStmt->value)
      continue;

    auto *callExpr = std::get_if<hew::ast::ExprCall>(&retStmt->value->value.kind);
    if (!callExpr || !callExpr->function)
      continue;

    auto *calleeIdent = std::get_if<hew::ast::ExprIdentifier>(&callExpr->function->value.kind);
    if (!calleeIdent || calleeIdent->name != callee)
      continue;

    if (callExpr->args.size() != 1)
      continue;

    auto *receiverArg = std::get_if<hew::ast::CallArgPositional>(&callExpr->args.front());
    if (!receiverArg || !receiverArg->expr)
      continue;

    auto receiverSpan = receiverArg->expr->span;
    hew::ast::ExprMethodCall methodCall;
    methodCall.receiver = std::move(receiverArg->expr);
    methodCall.method = methodName.str();
    retStmt->value->value.kind = std::move(methodCall);
    return receiverSpan;
  }
  return std::nullopt;
}

static hew::ast::Span *findMutableReturnedMethodReceiverSpan(hew::ast::FnDecl &fn,
                                                             llvm::StringRef methodName) {
  for (auto &stmt : fn.body.stmts) {
    auto *retStmt = std::get_if<hew::ast::StmtReturn>(&stmt->value.kind);
    if (!retStmt || !retStmt->value)
      continue;

    auto *methodCall = std::get_if<hew::ast::ExprMethodCall>(&retStmt->value->value.kind);
    if (!methodCall || methodCall->method != methodName)
      continue;

    return &methodCall->receiver->span;
  }
  return nullptr;
}

static std::optional<hew::ast::Span> findMethodReceiverSpanInStmt(const hew::ast::Stmt &stmt,
                                                                  const std::string &methodName) {
  auto receiverSpanForExpr =
      [&](const hew::ast::Spanned<hew::ast::Expr> &expr) -> std::optional<hew::ast::Span> {
    auto *methodCall = std::get_if<hew::ast::ExprMethodCall>(&expr.value.kind);
    if (!methodCall || methodCall->method != methodName)
      return std::nullopt;
    return methodCall->receiver->span;
  };

  if (auto *retStmt = std::get_if<hew::ast::StmtReturn>(&stmt.kind))
    return retStmt->value ? receiverSpanForExpr(*retStmt->value) : std::nullopt;

  if (auto *exprStmt = std::get_if<hew::ast::StmtExpression>(&stmt.kind))
    return receiverSpanForExpr(exprStmt->expr);

  return std::nullopt;
}

static std::optional<hew::ast::Span> findMethodCallSpanInStmt(const hew::ast::Stmt &stmt,
                                                              const std::string &methodName) {
  auto methodCallSpanForExpr =
      [&](const hew::ast::Spanned<hew::ast::Expr> &expr) -> std::optional<hew::ast::Span> {
    auto *methodCall = std::get_if<hew::ast::ExprMethodCall>(&expr.value.kind);
    if (!methodCall || methodCall->method != methodName)
      return std::nullopt;
    return expr.span;
  };

  if (auto *retStmt = std::get_if<hew::ast::StmtReturn>(&stmt.kind))
    return retStmt->value ? methodCallSpanForExpr(*retStmt->value) : std::nullopt;

  if (auto *exprStmt = std::get_if<hew::ast::StmtExpression>(&stmt.kind))
    return methodCallSpanForExpr(exprStmt->expr);

  return std::nullopt;
}

static std::optional<hew::ast::Span> findFunctionMethodReceiverSpan(const hew::ast::FnDecl &fn,
                                                                    const std::string &methodName) {
  for (const auto &stmt : fn.body.stmts)
    if (auto span = findMethodReceiverSpanInStmt(stmt->value, methodName))
      return span;

  if (fn.body.trailing_expr) {
    auto *methodCall = std::get_if<hew::ast::ExprMethodCall>(&fn.body.trailing_expr->value.kind);
    if (methodCall && methodCall->method == methodName)
      return methodCall->receiver->span;
  }

  return std::nullopt;
}

static std::optional<hew::ast::Span> findFunctionMethodCallSpan(const hew::ast::FnDecl &fn,
                                                                const std::string &methodName) {
  for (const auto &stmt : fn.body.stmts)
    if (auto span = findMethodCallSpanInStmt(stmt->value, methodName))
      return span;

  if (fn.body.trailing_expr) {
    auto *methodCall = std::get_if<hew::ast::ExprMethodCall>(&fn.body.trailing_expr->value.kind);
    if (methodCall && methodCall->method == methodName)
      return fn.body.trailing_expr->span;
  }

  return std::nullopt;
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

static hew::ast::Program makeDiscardedScopeBadTailProgram() {
  using namespace hew::ast;

  uint64_t nextSpan = 910000000000ULL;
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
  outerExpr.kind = ExprScope{std::nullopt, std::move(innerBlock)};
  outerExpr.span = outerSpan;

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

enum class DiscardedIfBadPart {
  Condition,
  ThenBranch,
  SideEffectBranches,
};

static hew::ast::Program makeDiscardedIfBadPartProgram(DiscardedIfBadPart badPart) {
  using namespace hew::ast;

  uint64_t nextSpan = 920000000000ULL;
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
  auto mkBool = [&](bool value) {
    ExprLiteral lit;
    lit.lit = LitBool{value};
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

  ExprIf ifExpr;
  ifExpr.condition = badPart == DiscardedIfBadPart::Condition ? mkBadBinary() : mkBool(true);
  if (badPart == DiscardedIfBadPart::ThenBranch) {
    ifExpr.then_block = mkBadBinary();
    ifExpr.else_block = mkInt(20);
  } else if (badPart == DiscardedIfBadPart::SideEffectBranches) {
    ifExpr.then_block = mkPrintCall();
    ifExpr.else_block = mkPrintCall();
  } else {
    ifExpr.then_block = mkInt(10);
    ifExpr.else_block = mkInt(20);
  }

  auto ifSpan = mkSpan();
  Expr ifNode;
  ifNode.kind = std::move(ifExpr);
  ifNode.span = ifSpan;

  Block scopeBlock;
  scopeBlock.trailing_expr =
      std::make_unique<Spanned<Expr>>(Spanned<Expr>{std::move(ifNode), ifSpan});

  auto scopeSpan = mkSpan();
  Expr expr;
  expr.kind = ExprScope{std::nullopt, std::move(scopeBlock)};
  expr.span = scopeSpan;

  StmtExpression exprStmt;
  exprStmt.expr = Spanned<Expr>{std::move(expr), scopeSpan};

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

static bool desugarWireStructToTypeDecl(hew::ast::Program &program, const std::string &structName) {
  const hew::ast::Span span{0, 0};
  for (auto &item : program.items) {
    auto *wireDecl = std::get_if<hew::ast::WireDecl>(&item.value.kind);
    if (!wireDecl || wireDecl->kind != hew::ast::WireDeclKind::Struct ||
        wireDecl->name != structName)
      continue;

    hew::ast::TypeDecl typeDecl;
    typeDecl.visibility = wireDecl->visibility;
    typeDecl.kind = hew::ast::TypeDeclKind::Struct;
    typeDecl.name = wireDecl->name;
    typeDecl.body.reserve(wireDecl->fields.size());
    for (const auto &field : wireDecl->fields) {
      hew::ast::TypeBodyItemField bodyField;
      bodyField.name = field.name;
      hew::ast::TypeExpr typeExpr;
      typeExpr.kind = hew::ast::TypeNamed{field.ty, std::nullopt};
      bodyField.ty = {std::move(typeExpr), span};
      typeDecl.body.push_back(hew::ast::TypeBodyItem{std::move(bodyField)});
    }

    hew::ast::WireMetadata metadata;
    metadata.field_meta.reserve(wireDecl->fields.size());
    for (const auto &field : wireDecl->fields) {
      hew::ast::WireFieldMeta fieldMeta;
      fieldMeta.field_name = field.name;
      fieldMeta.field_number = field.field_number;
      fieldMeta.is_optional = field.is_optional;
      fieldMeta.is_deprecated = field.is_deprecated;
      fieldMeta.is_repeated = field.is_repeated;
      fieldMeta.json_name = field.json_name;
      fieldMeta.yaml_name = field.yaml_name;
      fieldMeta.since = field.since;
      metadata.field_meta.push_back(std::move(fieldMeta));
    }
    metadata.json_case = wireDecl->json_case;
    metadata.yaml_case = wireDecl->yaml_case;
    metadata.version = wireDecl->version;
    metadata.min_version = wireDecl->min_version;
    typeDecl.wire = std::move(metadata);

    item.value.kind = std::move(typeDecl);
    return true;
  }

  return false;
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
// Test: Discarded scope expression fail-closes when a nested tail expression
//       lowers to null without its own diagnostic
// ============================================================================
static void test_discarded_scope_expr_bad_tail_fails_closed() {
  TEST(discarded_scope_expr_bad_tail_fails_closed);

  auto program = makeDiscardedScopeBadTailProgram();

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure for discarded scope expression with bad tail");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("discarded scope expression in statement position failed to lower") ==
      std::string::npos) {
    FAIL("expected discarded scope-expression fail-closed diagnostic");
    return;
  }

  PASS();
}

// ============================================================================
// Test: Discarded scope-tail if fail-closes when its condition lowers to null
//       without its own diagnostic
// ============================================================================
static void test_discarded_scope_tail_if_bad_condition_fails_closed() {
  TEST(discarded_scope_tail_if_bad_condition_fails_closed);

  auto program = makeDiscardedIfBadPartProgram(DiscardedIfBadPart::Condition);

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure for discarded scope-tail if with bad condition");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("discarded if expression in statement position failed to lower the "
                      "condition") == std::string::npos) {
    FAIL("expected discarded if-expression condition fail-closed diagnostic");
    return;
  }

  PASS();
}

// ============================================================================
// Test: Discarded scope-tail if fail-closes when a direct branch lowers to null
//       without its own diagnostic
// ============================================================================
static void test_discarded_scope_tail_if_bad_branch_fails_closed() {
  TEST(discarded_scope_tail_if_bad_branch_fails_closed);

  auto program = makeDiscardedIfBadPartProgram(DiscardedIfBadPart::ThenBranch);

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure for discarded scope-tail if with bad branch");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("discarded if expression in statement position failed to lower the "
                      "then-branch expression") == std::string::npos) {
    FAIL("expected discarded if-expression branch fail-closed diagnostic");
    return;
  }

  PASS();
}

// ============================================================================
// Test: Discarded scope-tail if with side-effect-only branches lowers cleanly
// ============================================================================
static void test_discarded_scope_tail_if_side_effect_branches_lower() {
  TEST(discarded_scope_tail_if_side_effect_branches_lower);

  auto program = makeDiscardedIfBadPartProgram(DiscardedIfBadPart::SideEffectBranches);

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (!module) {
    FAIL("expected MLIR generation success for discarded scope-tail if with side-effect branches");
    return;
  }

  if (stderrText.find("discarded if expression in statement position failed to lower") !=
      std::string::npos) {
    FAIL("unexpected discarded if-expression fail-closed diagnostic for side-effect branches");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
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
// Test: Parenthesized if-expressions nested in discarded scope wrappers lower
//       through the statement/no-result path
// ============================================================================
static void test_discarded_scope_if_expr_no_extra_results() {
  TEST(discarded_scope_if_expr_no_extra_results);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn scope_if_expr(flag: bool) -> int {
    scope {
        (if flag { 1 } else { 2 })
    };
    0
}

fn main() -> int {
    scope_if_expr(true)
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  auto testFn = lookupFuncBySuffix(module, "scope_if_expr");
  if (!testFn) {
    FAIL("discarded scope if-expression test function not found");
    module.getOperation()->destroy();
    return;
  }

  if (countResultfulIfOps(testFn) != 1) {
    FAIL("discarded scope if-expression should produce exactly one resultful scf.if");
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
// Test: Discarded if-expressions materialize owned branch temporaries
// ============================================================================
static void test_if_expr_branch_temporaries_drop() {
  TEST(if_expr_branch_temporaries_drop);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn make_vec(n: int) -> Vec<int> {
    let v: Vec<int> = Vec::new();
    v.push(n);
    v
}

fn direct_vec_temp(flag: bool) -> int {
    (if flag { make_vec(1) } else { make_vec(2) });
    0
}

fn nested_scope_string_temp(flag: bool) -> int {
    scope {
        (if flag { "left" } else { "right" });
    };
    0
}

fn main() -> int {
    direct_vec_temp(true) + nested_scope_string_temp(false)
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  auto directVecFn = lookupFuncBySuffix(module, "direct_vec_temp");
  auto nestedScopeStringFn = lookupFuncBySuffix(module, "nested_scope_string_temp");
  if (!directVecFn || !nestedScopeStringFn) {
    FAIL("discarded if-expression temporary-drop test functions not found");
    module.getOperation()->destroy();
    return;
  }

  if (countDropOpsByDropFn(directVecFn, "hew_vec_free", false) < 2) {
    FAIL("discarded if-expression should materialize and drop owned Vec branch temporaries");
    module.getOperation()->destroy();
    return;
  }

  if (countDropOpsByDropFn(nestedScopeStringFn, "hew_string_drop", false) < 2) {
    FAIL("nested discarded scope if-expression should materialize and drop selected String branch "
         "temporaries");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Discarded if-expressions still materialize owned branch temporaries
// ============================================================================
static void test_discarded_if_expr_branch_temporaries_drop() {
  TEST(discarded_if_expr_branch_temporaries_drop);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn make_vec(n: int) -> Vec<int> {
    let v: Vec<int> = Vec::new();
    v.push(n);
    v
}

fn scope_vec_temp(flag: bool) -> int {
    scope {
        (if flag { make_vec(1) } else { make_vec(2) });
    };
    0
}

fn scope_string_temp(flag: bool) -> int {
    scope {
        (if flag { "x" } else { "y" });
    };
    0
}

fn main() -> int {
    scope_vec_temp(true) + scope_string_temp(false)
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  auto vecFn = lookupFuncBySuffix(module, "scope_vec_temp");
  auto stringFn = lookupFuncBySuffix(module, "scope_string_temp");
  if (!vecFn || !stringFn) {
    FAIL("discarded if-expression temporary-drop test functions not found");
    module.getOperation()->destroy();
    return;
  }

  if (countDropOpsByDropFn(vecFn, "hew_vec_free", false) < 2) {
    FAIL("discarded scope if-expression should materialize and drop owned Vec branch temporaries");
    module.getOperation()->destroy();
    return;
  }

  if (countDropOpsByDropFn(stringFn, "hew_string_drop", false) < 2) {
    FAIL("discarded scope if-expression should materialize and drop selected String branch "
         "temporaries");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Statement-form if still materializes owned branch temporaries
// ============================================================================
static void test_if_stmt_branch_temporaries_drop() {
  TEST(if_stmt_branch_temporaries_drop);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn make_vec(n: int) -> Vec<int> {
    let v: Vec<int> = Vec::new();
    v.push(n);
    v
}

fn if_stmt_vec_temp(flag: bool) -> int {
    if flag { make_vec(1) } else { make_vec(2) };
    0
}

fn if_stmt_string_temp(flag: bool) -> int {
    if flag { "left" } else { "right" };
    0
}

fn main() -> int {
    if_stmt_vec_temp(true) + if_stmt_string_temp(false)
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  auto vecFn = lookupFuncBySuffix(module, "if_stmt_vec_temp");
  auto stringFn = lookupFuncBySuffix(module, "if_stmt_string_temp");
  if (!vecFn || !stringFn) {
    FAIL("statement-form if temporary-drop test functions not found");
    module.getOperation()->destroy();
    return;
  }

  if (countDropOpsByDropFn(vecFn, "hew_vec_free", false) < 2) {
    FAIL("statement-form if should materialize and drop owned Vec branch temporaries");
    module.getOperation()->destroy();
    return;
  }

  if (countDropOpsByDropFn(stringFn, "hew_string_drop", false) < 2) {
    FAIL("statement-form if should materialize and drop selected String branch temporaries");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Collection builtin type hints do not leak into sibling literals
// ============================================================================
static void test_collection_builtin_hint_does_not_leak_to_sibling_literals() {
  TEST(collection_builtin_hint_does_not_leak_to_sibling_literals);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn choose(v: Vec<int>, arr: [int; 3]) -> Vec<int> {
    v
}

fn main() -> int {
    let v: Vec<int> = choose(Vec::new(), [1, 2, 3]);
    v.len()
}
  )");

  if (module) {
    FAIL("expected non-direct Vec::new inside choose(...) to fail closed without leaking outer "
         "hints");
    module.getOperation()->destroy();
    return;
  }

  PASS();
}

// ============================================================================
// Test: declared collection hints lower array and empty HashMap literals locally
// ============================================================================
static void test_declared_collection_hints_lower_array_and_empty_hashmap_literals() {
  TEST(declared_collection_hints_lower_array_and_empty_hashmap_literals);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn main() -> int {
    let empty_vec: Vec<int> = [];
    let nums: Vec<int> = [1, 2, 3];
    let empty_map: HashMap<String, int> = {};
    empty_vec.len() + nums.len() + empty_map.len()
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  auto mainFn = lookupFuncBySuffix(module, "main");
  if (!mainFn) {
    FAIL("declared collection hint test function not found");
    module.getOperation()->destroy();
    return;
  }

  int vecNewCount = 0;
  int vecPushCount = 0;
  int arrayCreateCount = 0;
  int hashMapNewCount = 0;
  mainFn.walk([&](hew::VecNewOp) { vecNewCount++; });
  mainFn.walk([&](hew::VecPushOp) { vecPushCount++; });
  mainFn.walk([&](hew::ArrayCreateOp) { arrayCreateCount++; });
  mainFn.walk([&](hew::HashMapNewOp) { hashMapNewCount++; });

  if (vecNewCount != 2) {
    FAIL("expected typed array literals to lower as VecNewOp");
    module.getOperation()->destroy();
    return;
  }

  if (vecPushCount != 3) {
    FAIL("expected populated typed Vec literal to push each element");
    module.getOperation()->destroy();
    return;
  }

  if (arrayCreateCount != 0) {
    FAIL("did not expect ArrayCreateOp for typed Vec literals");
    module.getOperation()->destroy();
    return;
  }

  if (hashMapNewCount != 1) {
    FAIL("expected empty typed HashMap literal to lower as HashMapNewOp");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: nested Vec::new fails closed instead of capturing outer array hint
// ============================================================================
static void test_nested_vec_new_does_not_capture_outer_array_hint() {
  TEST(nested_vec_new_does_not_capture_outer_array_hint);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn main() -> int {
    let v: Vec<Vec<int>> = [Vec::new()];
    v.len()
}
  )");

  if (module) {
    FAIL("expected nested Vec::new without a local hint to fail closed");
    module.getOperation()->destroy();
    return;
  }

  PASS();
}

// ============================================================================
// Test: direct constructor hints lower builtin constructors without side-channel state
// ============================================================================
static void test_direct_constructor_type_hints_lower_builtins() {
  TEST(direct_constructor_type_hints_lower_builtins);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn main() -> int {
    let none_direct: Option<u32> = None;
    let some_direct: Option<u32> = Some(1);
    var ok_direct: Result<u32, int> = Ok(2);
    var err_direct: Result<u32, int> = Err(3);
    let vec: Vec<int> = Vec::new();
    let map: HashMap<String, int> = HashMap::new();
    let set: HashSet<String> = HashSet::new();
    vec.len() + map.len() + set.len()
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  auto mainFn = lookupFuncBySuffix(module, "main");
  if (!mainFn) {
    FAIL("direct constructor hint test function not found");
    module.getOperation()->destroy();
    return;
  }

  int vecNewCount = 0;
  bool vecNewIsI64 = false;
  int hashMapNewCount = 0;
  bool hashMapIsStringToI64 = false;
  int optionNoneU32Count = 0;
  bool someTypeIsOptionU32 = false;
  bool okTypeIsU32I64 = false;
  bool errTypeIsU32I64 = false;

  mainFn.walk([&](hew::VecNewOp op) {
    vecNewCount++;
    if (auto vecType = mlir::dyn_cast<hew::VecType>(op.getType()))
      vecNewIsI64 |= vecType.getElementType().isInteger(64);
  });
  mainFn.walk([&](hew::HashMapNewOp op) {
    hashMapNewCount++;
    if (auto mapType = mlir::dyn_cast<hew::HashMapType>(op.getType())) {
      hashMapIsStringToI64 |= mlir::isa<hew::StringRefType>(mapType.getKeyType()) &&
                              mapType.getValueType().isInteger(64);
    }
  });
  mainFn.walk([&](hew::EnumConstructOp op) {
    if (op.getEnumName() == "Option" && op.getVariantIndex() == 0) {
      if (auto optionType = mlir::dyn_cast<hew::OptionEnumType>(op.getType()))
        optionNoneU32Count += optionType.getInnerType().isInteger(32);
      return;
    }
    if (op.getEnumName() == "Option" && op.getVariantIndex() == 1) {
      if (auto optionType = mlir::dyn_cast<hew::OptionEnumType>(op.getType()))
        someTypeIsOptionU32 |= optionType.getInnerType().isInteger(32);
      return;
    }
    if (op.getEnumName() != "__Result")
      return;
    if (auto resultType = mlir::dyn_cast<hew::ResultEnumType>(op.getType())) {
      bool isU32I64Pair =
          resultType.getOkType().isInteger(32) && resultType.getErrType().isInteger(64);
      if (op.getVariantIndex() == 0)
        okTypeIsU32I64 |= isU32I64Pair;
      else if (op.getVariantIndex() == 1)
        errTypeIsU32I64 |= isU32I64Pair;
    }
  });

  if (vecNewCount != 1 || !vecNewIsI64) {
    FAIL("expected direct Vec::new hint to lower exactly one Vec<i64>");
    module.getOperation()->destroy();
    return;
  }

  if (hashMapNewCount != 1 || !hashMapIsStringToI64) {
    FAIL("expected direct HashMap::new hint to lower exactly one HashMap<String, i64>");
    module.getOperation()->destroy();
    return;
  }

  if (countRuntimeCallsByCallee(mainFn, "hew_hashset_new") != 1) {
    FAIL("expected direct HashSet::new hint to lower through hew_hashset_new exactly once");
    module.getOperation()->destroy();
    return;
  }

  if (optionNoneU32Count != 1) {
    FAIL("expected direct None hint to lower as Option<u32>");
    module.getOperation()->destroy();
    return;
  }

  if (!someTypeIsOptionU32) {
    FAIL("expected direct Some hint to lower as Option<u32>");
    module.getOperation()->destroy();
    return;
  }

  if (!okTypeIsU32I64 || !errTypeIsU32I64) {
    FAIL("expected direct Ok/Err hints to preserve Result<u32, i64> types");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: nested None does not inherit outer constructor hints
// ============================================================================
static void test_nested_none_does_not_inherit_outer_constructor_hints() {
  TEST(nested_none_does_not_inherit_outer_constructor_hints);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn main() -> int {
    let nested_option: Option<Option<int>> = Some(None);
    var nested_ok: Result<Option<int>, Option<int>> = Ok(None);
    var nested_err: Result<Option<int>, Option<int>> = Err(None);
    0
}
  )");

  if (module) {
    FAIL("expected nested None constructors without a local hint to fail closed");
    module.getOperation()->destroy();
    return;
  }

  PASS();
}

// ============================================================================
// Test: None without a direct or resolved type hint fails closed
// ============================================================================
static void test_none_without_type_context_fails_closed() {
  TEST(none_without_type_context_fails_closed);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn main() -> int {
    None;
    0
}
  )");

  if (module) {
    FAIL("expected bare None without type context to fail closed");
    module.getOperation()->destroy();
    return;
  }

  PASS();
}

// ============================================================================
// Test: resultful match arms thread direct constructor hints locally
// ============================================================================
static void test_match_arm_direct_none_uses_match_result_type_hint() {
  TEST(match_arm_direct_none_uses_match_result_type_hint);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn main() -> int {
    let input: Option<int> = None;
    let output = match input {
        Some(v) => Some(v),
        None => None,
    };
    match output {
        Some(v) => v,
        None => 0,
    }
}
  )");

  if (!module) {
    FAIL("expected resultful match arm None to use the match result type hint");
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Discarded if-expressions zero-init user-drop struct branch temporaries
// ============================================================================
static void test_discarded_if_expr_user_drop_branch_temp_zero_init() {
  TEST(discarded_if_expr_user_drop_branch_temp_zero_init);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
type Tracker {
    id: int;
}

impl Drop for Tracker {
    fn drop(t: Tracker) {
        println(t.id);
    }
}

type NamedTracker {
    name: String;
    id: int;
}

impl Drop for NamedTracker {
    fn drop(t: NamedTracker) {
        println(t.id);
    }
}

fn discarded_tracker_temp(flag: bool) -> int {
    (if flag { Tracker { id: 1 } } else { Tracker { id: 2 } });
    0
}

fn discarded_zero_tracker_temp(flag: bool) -> int {
    (if flag { Tracker { id: 0 } } else { Tracker { id: 0 } });
    0
}

fn discarded_named_tracker_temp(flag: bool) -> int {
    (if flag {
        NamedTracker { name: "left", id: 1 }
    } else {
        NamedTracker { name: "right", id: 2 }
    });
    0
}

fn discarded_named_tracker_loop(flag: bool) -> int {
    var i = 0;
    while i < 2 {
        (if flag {
            NamedTracker { name: "left", id: i }
        } else {
            NamedTracker { name: "right", id: i }
        });
        i = i + 1;
    }
    0
}

fn main() -> int {
    discarded_tracker_temp(true) + discarded_zero_tracker_temp(false)
        + discarded_named_tracker_temp(false)
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  auto trackerFn = lookupFuncBySuffix(module, "discarded_tracker_temp");
  auto zeroTrackerFn = lookupFuncBySuffix(module, "discarded_zero_tracker_temp");
  auto namedTrackerFn = lookupFuncBySuffix(module, "discarded_named_tracker_temp");
  auto namedLoopFn = lookupFuncBySuffix(module, "discarded_named_tracker_loop");
  if (!trackerFn || !zeroTrackerFn || !namedTrackerFn || !namedLoopFn) {
    FAIL("discarded tracker temporary test functions not found");
    module.getOperation()->destroy();
    return;
  }

  if (countUserDropOps(trackerFn) == 0) {
    FAIL("discarded if-expression should materialize user-drop branch temporaries");
    module.getOperation()->destroy();
    return;
  }

  if (!hasZeroInitializedUserDropSlot(trackerFn)) {
    FAIL("discarded if-expression should zero-initialize user-drop temporary slots");
    module.getOperation()->destroy();
    return;
  }

  if (!hasInitFlagGuardedUserDrop(trackerFn)) {
    FAIL("discarded if-expression should init-guard scalar-only user-drop temporary slots");
    module.getOperation()->destroy();
    return;
  }

  if (!hasInitFlagGuardedUserDrop(zeroTrackerFn)) {
    FAIL("discarded if-expression should still drop zero-valued user-drop temporaries");
    module.getOperation()->destroy();
    return;
  }

  if (countDropOpsByDropFn(namedTrackerFn, "hew_string_drop", false) == 0) {
    FAIL("discarded if-expression should drop owned fields for user-drop branch temporaries");
    module.getOperation()->destroy();
    return;
  }

  if (countDropOpsByDropFn(namedLoopFn, "hew_string_drop", false) < 3) {
    FAIL("discarded if-expression loops should drop overwritten owned-field temporaries");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Expression-position if (ExprIf in StmtExpression) gets init-bit guard
//
// Uses (if ...) syntax which parses as StmtExpression(ExprIf), routed through
// generateExprStmt.  Without the ExprIf → generateDiscardedExpr routing fix,
// the no-else case leaks the branch temporary (no drop ops emitted at all).
// ============================================================================
static void test_stmt_if_user_drop_initbit_guard() {
  TEST(stmt_if_user_drop_initbit_guard);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
type Tracker {
    id: int;
}

impl Drop for Tracker {
    fn drop(t: Tracker) {
        println(t.id);
    }
}

fn expr_if_no_else_user_drop(flag: bool) -> int {
    (if flag { Tracker { id: 42 } });
    0
}

fn main() -> int {
    expr_if_no_else_user_drop(false)
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  auto noElseFn = lookupFuncBySuffix(module, "expr_if_no_else_user_drop");
  if (!noElseFn) {
    FAIL("expression-position if user-drop init-bit test function not found");
    module.getOperation()->destroy();
    return;
  }

  // The then-branch temp must be materialized with a user-drop op.
  // Without the fix, generateExprStmt falls through to generateExpression
  // which calls generateIfExpr(statementPosition=false); the no-else path
  // creates a resultless scf.if but never materialises the branch value,
  // so no drop ops are emitted at all (resource leak).
  if (countUserDropOps(noElseFn) == 0) {
    FAIL("expression-position if (no else) must produce user-drop ops, not leak the value");
    module.getOperation()->destroy();
    return;
  }

  if (!hasZeroInitializedUserDropSlot(noElseFn)) {
    FAIL("expression-position if (no else) should zero-initialize the user-drop temp slot");
    module.getOperation()->destroy();
    return;
  }

  if (!hasInitFlagGuardedUserDrop(noElseFn)) {
    FAIL("expression-position if (no else) user-drop must be init-flag guarded");
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

static void test_direct_unsigned_range_missing_expr_type_fails_closed() {
  TEST(direct_unsigned_range_missing_expr_type_fails_closed);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
fn broken_direct_range(lo: u64, hi: u64) -> int {
    var n: int = 0;
    for _ in lo..hi {
        n = n + 1;
    }
    n
}
  )",
                             program)) {
    FAIL("failed to load typed program");
    return;
  }

  auto *fn = findFunctionDecl(program, "broken_direct_range");
  if (!fn) {
    FAIL("broken_direct_range function not found");
    return;
  }

  auto *forStmt = findFirstForStmt(*fn);
  if (!forStmt) {
    FAIL("expected direct range for-loop");
    return;
  }

  auto *rangeExpr = std::get_if<hew::ast::ExprBinary>(&forStmt->iterable.value.kind);
  if (!rangeExpr || !rangeExpr->left) {
    FAIL("expected direct range iterable expression");
    return;
  }

  if (!eraseExprTypeEntryForSpan(program, rangeExpr->left->span)) {
    FAIL("failed to remove expr_types entry for direct range lower bound");
    return;
  }

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure when direct range metadata is missing");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("missing expr_types entry for direct range loop signedness") ==
      std::string::npos) {
    FAIL("expected direct range fail-closed diagnostic");
    return;
  }

  if (stderrText.find("module verification failed") != std::string::npos) {
    FAIL("unexpected downstream verifier failure for direct range metadata");
    return;
  }

  PASS();
}

static void test_direct_unsigned_range_missing_upper_expr_type_fails_closed() {
  TEST(direct_unsigned_range_missing_upper_expr_type_fails_closed);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
fn broken_direct_range_upper(lo: u64, hi: u64) -> int {
    var n: int = 0;
    for _ in lo..hi {
        n = n + 1;
    }
    n
}
  )",
                             program)) {
    FAIL("failed to load typed program");
    return;
  }

  auto *fn = findFunctionDecl(program, "broken_direct_range_upper");
  if (!fn) {
    FAIL("broken_direct_range_upper function not found");
    return;
  }

  auto *forStmt = findFirstForStmt(*fn);
  if (!forStmt) {
    FAIL("expected direct range for-loop");
    return;
  }

  auto *rangeExpr = std::get_if<hew::ast::ExprBinary>(&forStmt->iterable.value.kind);
  if (!rangeExpr || !rangeExpr->right) {
    FAIL("expected direct range iterable expression");
    return;
  }

  if (!eraseExprTypeEntryForSpan(program, rangeExpr->right->span)) {
    FAIL("failed to remove expr_types entry for direct range upper bound");
    return;
  }

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure when direct range upper metadata is missing");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("missing expr_types entry for direct range loop upper bound signedness") ==
      std::string::npos) {
    FAIL("expected direct range upper bound fail-closed diagnostic");
    return;
  }

  if (stderrText.find("module verification failed") != std::string::npos) {
    FAIL("unexpected downstream verifier failure for direct range upper metadata");
    return;
  }

  PASS();
}

static void test_materialized_unsigned_range_missing_expr_type_fails_closed() {
  TEST(materialized_unsigned_range_missing_expr_type_fails_closed);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
fn broken_materialized_range(lo: u64, hi: u64) -> int {
    let r: Range<u64> = lo..hi;
    var n: int = 0;
    for _ in r {
        n = n + 1;
    }
    n
}
  )",
                             program)) {
    FAIL("failed to load typed program");
    return;
  }

  auto *fn = findFunctionDecl(program, "broken_materialized_range");
  if (!fn) {
    FAIL("broken_materialized_range function not found");
    return;
  }

  auto *forStmt = findFirstForStmt(*fn);
  if (!forStmt) {
    FAIL("expected materialized range for-loop");
    return;
  }

  if (!eraseExprTypeEntryForSpan(program, forStmt->iterable.span)) {
    FAIL("failed to remove expr_types entry for materialized range iterable");
    return;
  }

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure when materialized range metadata is missing");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("missing expr_types entry for materialized range loop signedness") ==
      std::string::npos) {
    FAIL("expected materialized range fail-closed diagnostic");
    return;
  }

  if (stderrText.find("module verification failed") != std::string::npos) {
    FAIL("unexpected downstream verifier failure for materialized range metadata");
    return;
  }

  PASS();
}

static void test_indirect_enum_match_missing_scrutinee_expr_type_fails_closed() {
  TEST(indirect_enum_match_missing_scrutinee_expr_type_fails_closed);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
indirect enum List {
    Nil;
    Cons(int, List);
}

fn head_or_zero(list: List) -> int {
    match list {
        Nil => 0,
        Cons(head, _) => head,
    }
}
  )",
                             program)) {
    FAIL("failed to load typed program");
    return;
  }

  auto *fn = findFunctionDecl(program, "head_or_zero");
  if (!fn) {
    FAIL("head_or_zero function not found");
    return;
  }

  auto *matchStmt = findFirstMatchStmt(*fn);
  if (!matchStmt) {
    FAIL("expected indirect enum trailing match statement");
    return;
  }

  if (!eraseExprTypeEntryForSpan(program, matchStmt->scrutinee.span)) {
    FAIL("failed to remove expr_types entry for indirect enum match scrutinee");
    return;
  }

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure when indirect enum scrutinee metadata is missing");
    module.getOperation()->destroy();
    return;
  }

  if (!hasMissingIndirectEnumScrutineeDiag(stderrText)) {
    FAIL("expected indirect enum match fail-closed diagnostic");
    return;
  }

  if (stderrText.find("module verification failed") != std::string::npos) {
    FAIL("unexpected downstream verifier failure for indirect enum match metadata");
    return;
  }

  PASS();
}

// Test: Indirect enum scrutinee metadata is required for match expressions (ExprMatch path)
// Covers the generateMatchExpr -> derefIndirectEnumScrutinee boundary for return-match syntax.
static void test_indirect_enum_match_expr_missing_scrutinee_expr_type_fails_closed() {
  TEST(indirect_enum_match_expr_missing_scrutinee_expr_type_fails_closed);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
indirect enum List {
    Nil;
    Cons(int, List);
}

fn head_or_zero(list: List) -> int {
    return match list {
        Nil => 0,
        Cons(head, _) => head,
    };
}
  )",
                             program)) {
    FAIL("failed to load typed program");
    return;
  }

  auto *fn = findFunctionDecl(program, "head_or_zero");
  if (!fn) {
    FAIL("head_or_zero function not found");
    return;
  }

  auto *matchExprSpanned = findReturnedMatchExpr(*fn);
  if (!matchExprSpanned) {
    FAIL("expected returned indirect enum match expression");
    return;
  }

  auto *exprMatch = std::get_if<hew::ast::ExprMatch>(&matchExprSpanned->value.kind);
  if (!exprMatch || !exprMatch->scrutinee) {
    FAIL("expected ExprMatch with scrutinee");
    return;
  }

  if (!eraseExprTypeEntryForSpan(program, exprMatch->scrutinee->span)) {
    FAIL("failed to remove expr_types entry for indirect enum match expr scrutinee");
    return;
  }

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure when indirect enum match expr scrutinee metadata is "
         "missing");
    module.getOperation()->destroy();
    return;
  }

  if (!hasMissingIndirectEnumScrutineeDiag(stderrText)) {
    FAIL("expected indirect enum match expr fail-closed diagnostic");
    return;
  }

  if (stderrText.find("module verification failed") != std::string::npos) {
    FAIL("unexpected downstream verifier failure for indirect enum match expr metadata");
    return;
  }

  PASS();
}

static void test_indirect_enum_iflet_stmt_missing_scrutinee_expr_type_fails_closed() {
  TEST(indirect_enum_iflet_stmt_missing_scrutinee_expr_type_fails_closed);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
indirect enum List {
    Nil;
    Cons(int, List);
}

fn head_or_zero(list: List) -> int {
    if let Cons(head, _) = list {
        return head;
    } else {
        return 0;
    }
}
  )",
                             program)) {
    FAIL("failed to load typed program");
    return;
  }

  auto *fn = findFunctionDecl(program, "head_or_zero");
  if (!fn) {
    FAIL("head_or_zero function not found");
    return;
  }

  auto *ifLetStmt = findFirstIfLetStmt(*fn);
  if (!ifLetStmt || !ifLetStmt->expr) {
    FAIL("expected indirect enum if-let statement");
    return;
  }

  if (!eraseExprTypeEntryForSpan(program, ifLetStmt->expr->span)) {
    FAIL("failed to remove expr_types entry for indirect enum if-let scrutinee");
    return;
  }

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure when indirect enum if-let metadata is missing");
    module.getOperation()->destroy();
    return;
  }

  if (!hasMissingIndirectEnumScrutineeDiag(stderrText)) {
    FAIL("expected indirect enum if-let fail-closed diagnostic");
    return;
  }

  if (stderrText.find("module verification failed") != std::string::npos) {
    FAIL("unexpected downstream verifier failure for indirect enum if-let metadata");
    return;
  }

  PASS();
}

static void test_indirect_enum_iflet_expr_missing_scrutinee_expr_type_fails_closed() {
  TEST(indirect_enum_iflet_expr_missing_scrutinee_expr_type_fails_closed);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
indirect enum List {
    Nil;
    Cons(int, List);
}

fn head_or_zero_expr(list: List) -> int {
    return if let Cons(head, _) = list { head } else { 0 };
}
  )",
                             program)) {
    FAIL("failed to load typed program");
    return;
  }

  auto *fn = findFunctionDecl(program, "head_or_zero_expr");
  if (!fn) {
    FAIL("head_or_zero_expr function not found");
    return;
  }

  auto *ifLetExpr = findReturnedIfLetExpr(*fn);
  if (!ifLetExpr || !ifLetExpr->expr) {
    FAIL("expected returned indirect enum if-let expression");
    return;
  }

  if (!eraseExprTypeEntryForSpan(program, ifLetExpr->expr->span)) {
    FAIL("failed to remove expr_types entry for indirect enum if-let expression scrutinee");
    return;
  }

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure when indirect enum if-let expression metadata is "
         "missing");
    module.getOperation()->destroy();
    return;
  }

  if (!hasMissingIndirectEnumScrutineeDiag(stderrText)) {
    FAIL("expected indirect enum if-let expression fail-closed diagnostic");
    return;
  }

  if (stderrText.find("module verification failed") != std::string::npos) {
    FAIL("unexpected downstream verifier failure for indirect enum if-let expression metadata");
    return;
  }

  PASS();
}

static void test_indirect_enum_whilelet_missing_scrutinee_expr_type_fails_closed() {
  TEST(indirect_enum_whilelet_missing_scrutinee_expr_type_fails_closed);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
indirect enum List {
    Nil;
    Cons(int, List);
}

fn length(seed: List) -> int {
    var list: List = seed;
    var n: int = 0;
    while let Cons(_, tail) = list {
        n = n + 1;
        list = tail;
    }
    n
}
  )",
                             program)) {
    FAIL("failed to load typed program");
    return;
  }

  auto *fn = findFunctionDecl(program, "length");
  if (!fn) {
    FAIL("length function not found");
    return;
  }

  auto *whileLetStmt = findFirstWhileLetStmt(*fn);
  if (!whileLetStmt || !whileLetStmt->expr) {
    FAIL("expected indirect enum while-let statement");
    return;
  }

  if (!eraseExprTypeEntryForSpan(program, whileLetStmt->expr->span)) {
    FAIL("failed to remove expr_types entry for indirect enum while-let scrutinee");
    return;
  }

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure when indirect enum while-let metadata is missing");
    module.getOperation()->destroy();
    return;
  }

  if (!hasMissingIndirectEnumScrutineeDiag(stderrText)) {
    FAIL("expected indirect enum while-let fail-closed diagnostic");
    return;
  }

  if (stderrText.find("module verification failed") != std::string::npos) {
    FAIL("unexpected downstream verifier failure for indirect enum while-let metadata");
    return;
  }

  PASS();
}

static void test_option_result_match_expr_missing_result_type_fails_closed() {
  TEST(option_result_match_expr_missing_result_type_fails_closed);

  struct MatchCase {
    const char *label;
    const char *fnName;
    const char *source;
  };
  const MatchCase cases[] = {
      {"Option", "unwrap_or_zero", R"(
fn unwrap_or_zero(opt: Option<int>) -> int {
    return match opt {
        Some(value) => value,
        None => 0,
    };
}
  )"},
      {"Result", "unwrap_result", R"(
fn unwrap_result(res: Result<int, int>) -> int {
    return match res {
        Ok(value) => value,
        Err(err) => err,
    };
}
  )"},
  };

  for (const auto &testCase : cases) {
    hew::ast::Program program;
    if (!loadProgramFromSource(testCase.source, program)) {
      FAIL("failed to load typed program");
      return;
    }

    auto *fn = findFunctionDecl(program, testCase.fnName);
    if (!fn) {
      FAIL("match expression function not found");
      return;
    }

    auto *matchExpr = findReturnedMatchExpr(*fn);
    if (!matchExpr) {
      FAIL("expected returned match expression");
      return;
    }

    if (!eraseExprTypeEntryForSpan(program, matchExpr->span)) {
      FAIL("failed to remove expr_types entry for match expression");
      return;
    }

    mlir::MLIRContext ctx;
    initContext(ctx);

    hew::MLIRGen mlirGen(ctx);
    mlir::ModuleOp module;
    auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

    if (module) {
      FAIL("expected MLIR generation failure when match expression metadata is missing");
      module.getOperation()->destroy();
      return;
    }

    if (stderrText.find("missing expr_types entry for match expression result type") ==
        std::string::npos) {
      FAIL("expected match-expression fail-closed diagnostic");
      return;
    }

    if (stderrText.find("module verification failed") != std::string::npos) {
      FAIL("unexpected downstream verifier failure for match expression metadata");
      return;
    }
  }

  PASS();
}

static void test_unsigned_binary_ops_use_unsigned_lowering() {
  TEST(unsigned_binary_ops_use_unsigned_lowering);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
fn unsigned_ops() -> u16 {
    let a: u8 = 3;
    let b: u16 = 9;
    let c: u16 = 12;
    let widened: u16 = a + b;
    if widened < c {
        c / widened
    } else {
        c % widened
    }
}
  )");

  if (!module) {
    FAIL("MLIR generation failed");
    return;
  }

  auto fn = lookupFuncBySuffix(module, "unsigned_ops");
  if (!fn) {
    FAIL("unsigned_ops function not found");
    module.getOperation()->destroy();
    return;
  }

  bool hasExtUI = false;
  bool hasExtSI = false;
  bool hasDivUI = false;
  bool hasDivSI = false;
  bool hasRemUI = false;
  bool hasRemSI = false;
  bool hasUnsignedCompare = false;
  bool hasSignedCompare = false;

  fn.walk([&](mlir::arith::ExtUIOp) { hasExtUI = true; });
  fn.walk([&](mlir::arith::ExtSIOp) { hasExtSI = true; });
  fn.walk([&](mlir::arith::DivUIOp) { hasDivUI = true; });
  fn.walk([&](mlir::arith::DivSIOp) { hasDivSI = true; });
  fn.walk([&](mlir::arith::RemUIOp) { hasRemUI = true; });
  fn.walk([&](mlir::arith::RemSIOp) { hasRemSI = true; });
  fn.walk([&](mlir::arith::CmpIOp cmp) {
    hasUnsignedCompare |= cmp.getPredicate() == mlir::arith::CmpIPredicate::ult;
    hasSignedCompare |= cmp.getPredicate() == mlir::arith::CmpIPredicate::slt;
  });

  if (!hasExtUI || hasExtSI || !hasDivUI || hasDivSI || !hasRemUI || hasRemSI ||
      !hasUnsignedCompare || hasSignedCompare) {
    FAIL("expected unsigned widening/division/remainder/compare lowering");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

static void test_unsigned_binary_expr_missing_expr_type_fails_closed() {
  TEST(unsigned_binary_expr_missing_expr_type_fails_closed);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
fn broken_unsigned_cmp() -> bool {
    let a: u8 = 1;
    let b: u16 = 2;
    a < b
}
  )",
                             program)) {
    FAIL("failed to load typed program");
    return;
  }

  auto *fn = findFunctionDecl(program, "broken_unsigned_cmp");
  if (!fn || !fn->body.trailing_expr) {
    FAIL("broken_unsigned_cmp body missing trailing expression");
    return;
  }

  auto *binary = std::get_if<hew::ast::ExprBinary>(&fn->body.trailing_expr->value.kind);
  if (!binary) {
    FAIL("expected trailing binary expression");
    return;
  }

  if (!eraseExprTypeEntryForSpan(program, binary->left->span)) {
    FAIL("failed to remove expr_types entry for left operand");
    return;
  }

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure when unsigned binary metadata is missing");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("missing expr_types entry for integer widening of binary left operand") ==
      std::string::npos) {
    FAIL("expected missing expr_types fail-closed diagnostic");
    return;
  }

  if (stderrText.find("module verification failed") != std::string::npos) {
    FAIL("unexpected downstream verifier failure for missing expr_types");
    return;
  }

  PASS();
}

// ============================================================================
// Test: println_int missing expr_type metadata fails closed
// ============================================================================
static void test_println_int_missing_expr_type_fails_closed() {
  TEST(println_int_missing_expr_type_fails_closed);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
fn broken_print(x: int) {
    println_int(x);
}
  )",
                             program)) {
    FAIL("failed to load typed program");
    return;
  }

  auto *fn = findFunctionDecl(program, "broken_print");
  if (!fn) {
    FAIL("broken_print function not found");
    return;
  }

  // println_int(x) is a statement expression: stmts[0] -> StmtExpression -> ExprCall.
  if (fn->body.stmts.empty()) {
    FAIL("expected at least one statement in broken_print");
    return;
  }
  auto *stmtExpr = std::get_if<hew::ast::StmtExpression>(&fn->body.stmts[0]->value.kind);
  if (!stmtExpr) {
    FAIL("expected first statement to be an expression statement");
    return;
  }
  auto *call = std::get_if<hew::ast::ExprCall>(&stmtExpr->expr.value.kind);
  if (!call || call->args.empty()) {
    FAIL("expected expression statement to be a call with arguments");
    return;
  }

  const auto &argSpan = hew::ast::callArgExpr(call->args[0]).span;
  if (!eraseExprTypeEntryForSpan(program, argSpan)) {
    FAIL("failed to remove expr_types entry for println_int argument");
    return;
  }

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure when println_int argument type is missing");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("missing expr_types entry for println_int argument signedness") ==
      std::string::npos) {
    FAIL("expected println_int fail-closed diagnostic");
    return;
  }

  if (stderrText.find("module verification failed") != std::string::npos) {
    FAIL("unexpected downstream verifier failure for println_int missing type");
    return;
  }

  PASS();
}

// ============================================================================
// Test: int_to_string missing expr_type metadata fails closed
// ============================================================================
static void test_int_to_string_missing_expr_type_fails_closed() {
  TEST(int_to_string_missing_expr_type_fails_closed);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
fn broken_to_string(x: int) -> string {
    int_to_string(x)
}
  )",
                             program)) {
    FAIL("failed to load typed program");
    return;
  }

  auto *fn = findFunctionDecl(program, "broken_to_string");
  if (!fn) {
    FAIL("broken_to_string function not found");
    return;
  }

  // int_to_string(x) is the trailing expression of the function body.
  auto *call = fn->body.trailing_expr
                   ? std::get_if<hew::ast::ExprCall>(&fn->body.trailing_expr->value.kind)
                   : nullptr;
  if (!call || call->args.empty()) {
    FAIL("expected trailing ExprCall in broken_to_string");
    return;
  }

  const auto &argSpan = hew::ast::callArgExpr(call->args[0]).span;
  if (!eraseExprTypeEntryForSpan(program, argSpan)) {
    FAIL("failed to remove expr_types entry for int_to_string argument");
    return;
  }

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure when int_to_string argument type is missing");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("missing expr_types entry for int_to_string argument signedness") ==
      std::string::npos) {
    FAIL("expected int_to_string fail-closed diagnostic");
    return;
  }

  if (stderrText.find("module verification failed") != std::string::npos) {
    FAIL("unexpected downstream verifier failure for int_to_string missing type");
    return;
  }

  PASS();
}

// ============================================================================
// Test: inline scope.launch await lowers by consulting resolved task metadata
// ============================================================================
static void test_scope_await_inline_launch_uses_resolved_task_type() {
  TEST(scope_await_inline_launch_uses_resolved_task_type);

  mlir::MLIRContext ctx;
  initContext(ctx);

  auto module = generateMLIR(ctx, R"(
fn main() -> int {
    scope |s| {
        await (s.launch {
            1
        })
    }
}
  )");

  if (!module) {
    FAIL("MLIR generation failed for inline scope.await launch expression");
    return;
  }

  auto mainFn = lookupFuncBySuffix(module, "main");
  if (!mainFn) {
    FAIL("main function not found for inline scope.await launch test");
    module.getOperation()->destroy();
    return;
  }

  hew::ScopeAwaitOp awaitOp;
  mlir::LLVM::LoadOp awaitLoad;
  mainFn.walk([&](hew::ScopeAwaitOp op) { awaitOp = op; });
  mainFn.walk([&](mlir::LLVM::LoadOp op) {
    if (awaitOp && op.getAddr() == awaitOp.getResult())
      awaitLoad = op;
  });

  if (!awaitOp) {
    FAIL("expected a hew.scope.await operation for inline task await");
    module.getOperation()->destroy();
    return;
  }

  if (!awaitLoad || !awaitLoad.getResult().getType().isInteger(64)) {
    FAIL("expected inline task await to lower to an i64 load from the await result pointer");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: inline scope.launch await without expr_types fails closed
// ============================================================================
static void test_scope_await_inline_launch_missing_expr_type_fails_closed() {
  TEST(scope_await_inline_launch_missing_expr_type_fails_closed);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
fn main() -> int {
    scope |s| {
        await (s.launch {
            1
        })
    }
}
  )",
                             program)) {
    FAIL("failed to load typed program");
    return;
  }

  auto *mainFn = findFunctionDecl(program, "main");
  if (!mainFn || !mainFn->body.trailing_expr) {
    FAIL("main function or trailing scope expression missing");
    return;
  }

  auto *scopeExpr = std::get_if<hew::ast::ExprScope>(&mainFn->body.trailing_expr->value.kind);
  if (!scopeExpr || !scopeExpr->block.trailing_expr) {
    FAIL("expected scope expression with trailing await");
    return;
  }

  auto *awaitExpr = std::get_if<hew::ast::ExprAwait>(&scopeExpr->block.trailing_expr->value.kind);
  if (!awaitExpr || !awaitExpr->inner) {
    FAIL("expected trailing await expression inside scope");
    return;
  }

  if (!eraseExprTypeEntryForSpan(program, awaitExpr->inner->span)) {
    FAIL("failed to remove expr_types entry for inline awaited task expression");
    return;
  }

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure when inline awaited task metadata is missing");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("missing expr_types entry for scope.await operand") == std::string::npos) {
    FAIL("expected missing-expr_types diagnostic for inline awaited task expression");
    return;
  }

  if (stderrText.find("module verification failed") != std::string::npos) {
    FAIL("unexpected downstream verifier failure for missing inline task await metadata");
    return;
  }

  PASS();
}

// ============================================================================
// Test: for await on Receiver<T> with unresolved element type fails closed
// ============================================================================
static void test_for_await_receiver_missing_elem_type_fails_closed() {
  TEST(for_await_receiver_missing_elem_type_fails_closed);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
import std::channel::channel;

fn drain(rx: channel.Receiver<String>) {
    for await msg in rx {
        println(msg);
    }
    rx.close();
}
  )",
                             program)) {
    FAIL("failed to load typed program");
    return;
  }

  auto *fn = findFunctionDecl(program, "drain");
  if (!fn) {
    FAIL("drain function not found");
    return;
  }

  auto *forStmt = findFirstForStmt(*fn);
  if (!forStmt) {
    FAIL("expected for-await statement inside drain");
    return;
  }

  // Corrupt the Receiver<T> parameter type by replacing its inner type arg
  // with TypeInfer, simulating an unresolved channel element type leaking
  // to the codegen boundary.
  if (fn->params.empty()) {
    FAIL("drain function has no parameters");
    return;
  }
  auto *rxNamed = std::get_if<hew::ast::TypeNamed>(&fn->params[0].ty.value.kind);
  if (!rxNamed || !rxNamed->type_args || rxNamed->type_args->empty()) {
    FAIL("rx parameter does not have a qualified Receiver type with type arg");
    return;
  }
  // Replace the inner type arg (String) with TypeInfer to simulate a leaked
  // inference hole on the channel element type.
  (*rxNamed->type_args)[0].value.kind = hew::ast::TypeInfer{};

  // Also erase the iterable span from expr_types so the first resolution
  // strategy (resolvedTypeOf) cannot supply a well-formed type.
  eraseExprTypeEntryForSpan(program, forStmt->iterable.span);

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure when Receiver<T> has TypeInfer element type");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("requires a resolved named element type") == std::string::npos) {
    FAIL("expected Receiver<TypeInfer> fail-closed diagnostic");
    return;
  }

  // No downstream verifier failures expected.
  if (stderrText.find("module verification failed") != std::string::npos) {
    FAIL("unexpected downstream verifier failure for Receiver<TypeInfer>");
    return;
  }

  PASS();
}

// ============================================================================
// Test: bytes-stream let/var fallback preserves bytes ABI without expr_types
// ============================================================================
static void test_for_await_bytes_stream_binding_fallback_uses_bytes_abi() {
  TEST(for_await_bytes_stream_binding_fallback_uses_bytes_abi);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
import std::stream;

fn main() -> int {
    let (sink, input) = stream.bytes_pipe(8);
    let alias = input;
    var mapped = alias.map((b) => { b });
    for await item in mapped {
        return item.len();
    }
    0
}
  )",
                             program)) {
    FAIL("failed to load typed bytes-stream binding fallback program");
    return;
  }

  auto *fn = findFunctionDecl(program, "main");
  auto *forStmt = fn ? findFirstForStmt(*fn) : nullptr;
  if (!fn || !forStmt) {
    FAIL("main or for-await statement missing in bytes-stream binding fallback test");
    return;
  }

  auto findLetByName = [](hew::ast::FnDecl &fn, llvm::StringRef name) -> hew::ast::StmtLet * {
    for (const auto &stmt : fn.body.stmts) {
      auto *letStmt = std::get_if<hew::ast::StmtLet>(&stmt->value.kind);
      if (!letStmt)
        continue;
      auto *ident = std::get_if<hew::ast::PatIdentifier>(&letStmt->pattern.value.kind);
      if (ident && ident->name == name)
        return letStmt;
    }
    return nullptr;
  };
  auto findVarByName = [](hew::ast::FnDecl &fn, llvm::StringRef name) -> hew::ast::StmtVar * {
    for (const auto &stmt : fn.body.stmts) {
      auto *varStmt = std::get_if<hew::ast::StmtVar>(&stmt->value.kind);
      if (varStmt && varStmt->name == name)
        return varStmt;
    }
    return nullptr;
  };

  auto *aliasLet = findLetByName(*fn, "alias");
  auto *mappedVar = findVarByName(*fn, "mapped");
  if (!aliasLet || !aliasLet->value || !mappedVar || !mappedVar->value) {
    FAIL("expected alias let and mapped var in bytes-stream binding fallback test");
    return;
  }

  auto *mapCall = std::get_if<hew::ast::ExprMethodCall>(&mappedVar->value->value.kind);
  if (!mapCall || mapCall->method != "map" || !mapCall->receiver) {
    FAIL("expected mapped var initializer to be alias.map(...)");
    return;
  }

  if (!eraseExprTypeEntryForSpan(program, aliasLet->value->span)) {
    FAIL("failed to remove expr_types entry for alias binding input");
    return;
  }
  if (!eraseExprTypeEntryForSpan(program, mapCall->receiver->span)) {
    FAIL("failed to remove expr_types entry for map receiver alias");
    return;
  }
  if (!eraseExprTypeEntryForSpan(program, forStmt->iterable.span)) {
    FAIL("failed to remove expr_types entry for mapped iterable");
    return;
  }

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  auto module = mlirGen.generate(program);
  if (!module) {
    FAIL("expected MLIR generation success for bytes-stream binding fallback");
    return;
  }

  auto mainFn = lookupFuncBySuffix(module, "main");
  if (!mainFn) {
    FAIL("main function not found in bytes-stream binding fallback test");
    module.getOperation()->destroy();
    return;
  }

  if (countRuntimeCallsByCallee(mainFn.getOperation(), "hew_stream_map_bytes") != 1) {
    FAIL("expected bytes-stream map fallback to lower to hew_stream_map_bytes");
    module.getOperation()->destroy();
    return;
  }
  if (countRuntimeCallsByCallee(mainFn.getOperation(), "hew_stream_map_string") != 0) {
    FAIL("unexpected string-stream map runtime call in bytes-stream binding fallback");
    module.getOperation()->destroy();
    return;
  }
  if (countRuntimeCallsByCallee(mainFn.getOperation(), "hew_stream_next_bytes") != 1) {
    FAIL("expected bytes-stream iteration fallback to lower to hew_stream_next_bytes");
    module.getOperation()->destroy();
    return;
  }
  if (countRuntimeCallsByCallee(mainFn.getOperation(), "hew_stream_next") != 0) {
    FAIL("unexpected string-stream next runtime call in bytes-stream binding fallback");
    module.getOperation()->destroy();
    return;
  }
  if (countDropOpsByDropFn(mainFn.getOperation(), "hew_vec_free", false) < 1) {
    FAIL("expected bytes-stream iteration fallback to register hew_vec_free");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: bytes fallback beats conflicting string expr_types during alias tracking
// ============================================================================
static void test_for_await_bytes_stream_binding_fallback_overrides_conflicting_expr_type() {
  TEST(for_await_bytes_stream_binding_fallback_overrides_conflicting_expr_type);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
import std::stream;

fn main() -> int {
    let (sink, input) = stream.bytes_pipe(8);
    let alias = input;
    var mapped = alias.map((b) => { b });
    for await item in mapped {
        return item.len();
    }
    0
}
  )",
                             program)) {
    FAIL("failed to load typed bytes-stream conflicting expr_types program");
    return;
  }

  auto *fn = findFunctionDecl(program, "main");
  auto *forStmt = fn ? findFirstForStmt(*fn) : nullptr;
  if (!fn || !forStmt) {
    FAIL("main or for-await statement missing in conflicting expr_types test");
    return;
  }

  auto findLetByName = [](hew::ast::FnDecl &fn, llvm::StringRef name) -> hew::ast::StmtLet * {
    for (const auto &stmt : fn.body.stmts) {
      auto *letStmt = std::get_if<hew::ast::StmtLet>(&stmt->value.kind);
      if (!letStmt)
        continue;
      auto *ident = std::get_if<hew::ast::PatIdentifier>(&letStmt->pattern.value.kind);
      if (ident && ident->name == name)
        return letStmt;
    }
    return nullptr;
  };
  auto findVarByName = [](hew::ast::FnDecl &fn, llvm::StringRef name) -> hew::ast::StmtVar * {
    for (const auto &stmt : fn.body.stmts) {
      auto *varStmt = std::get_if<hew::ast::StmtVar>(&stmt->value.kind);
      if (varStmt && varStmt->name == name)
        return varStmt;
    }
    return nullptr;
  };

  auto *aliasLet = findLetByName(*fn, "alias");
  auto *mappedVar = findVarByName(*fn, "mapped");
  if (!aliasLet || !aliasLet->value || !mappedVar || !mappedVar->value) {
    FAIL("expected alias let and mapped var in conflicting expr_types test");
    return;
  }

  auto *mapCall = std::get_if<hew::ast::ExprMethodCall>(&mappedVar->value->value.kind);
  if (!mapCall || mapCall->method != "map" || !mapCall->receiver) {
    FAIL("expected mapped var initializer to be alias.map(...)");
    return;
  }

  if (!replaceStreamElementExprTypeForSpan(program, aliasLet->value->span, "string")) {
    FAIL("failed to replace expr_types entry for alias binding input with Stream<string>");
    return;
  }
  size_t rewrittenAliasExprTypeCount = 0;
  for (const auto &entry : program.expr_types) {
    if (entry.start != aliasLet->value->span.start || entry.end != aliasLet->value->span.end)
      continue;
    auto *streamType = std::get_if<hew::ast::TypeNamed>(&entry.ty.value.kind);
    auto *elementType =
        streamType && streamType->type_args && streamType->type_args->size() == 1
            ? std::get_if<hew::ast::TypeNamed>(&streamType->type_args->front().value.kind)
            : nullptr;
    if (!elementType || elementType->name != "string") {
      FAIL("expected all alias binding expr_types entries to be rewritten to Stream<string>");
      return;
    }
    ++rewrittenAliasExprTypeCount;
  }
  if (rewrittenAliasExprTypeCount == 0) {
    FAIL("expected at least one rewritten expr_types entry for alias binding input");
    return;
  }
  if (!eraseExprTypeEntryForSpan(program, mapCall->receiver->span)) {
    FAIL("failed to remove expr_types entry for map receiver alias");
    return;
  }
  if (!eraseExprTypeEntryForSpan(program, forStmt->iterable.span)) {
    FAIL("failed to remove expr_types entry for mapped iterable");
    return;
  }

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  auto module = mlirGen.generate(program);
  if (!module) {
    FAIL("expected MLIR generation success for conflicting expr_types fallback");
    return;
  }

  auto mainFn = lookupFuncBySuffix(module, "main");
  if (!mainFn) {
    FAIL("main function not found in conflicting expr_types fallback test");
    module.getOperation()->destroy();
    return;
  }

  if (countRuntimeCallsByCallee(mainFn.getOperation(), "hew_stream_map_bytes") != 1) {
    FAIL("expected recovered bytes fallback to lower map to hew_stream_map_bytes");
    module.getOperation()->destroy();
    return;
  }
  if (countRuntimeCallsByCallee(mainFn.getOperation(), "hew_stream_map_string") != 0) {
    FAIL("unexpected string-stream map runtime call after conflicting expr_types rewrite");
    module.getOperation()->destroy();
    return;
  }
  if (countRuntimeCallsByCallee(mainFn.getOperation(), "hew_stream_next_bytes") != 1) {
    FAIL("expected recovered bytes fallback to lower iteration to hew_stream_next_bytes");
    module.getOperation()->destroy();
    return;
  }
  if (countRuntimeCallsByCallee(mainFn.getOperation(), "hew_stream_next") != 0) {
    FAIL("unexpected string-stream next runtime call after conflicting expr_types rewrite");
    module.getOperation()->destroy();
    return;
  }
  if (countDropOpsByDropFn(mainFn.getOperation(), "hew_vec_free", false) < 1) {
    FAIL("expected recovered bytes fallback to register hew_vec_free");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: inline bytes-stream filter fallback preserves bytes ABI without expr_types
// ============================================================================
static void test_for_await_bytes_stream_inline_filter_fallback_uses_bytes_abi() {
  TEST(for_await_bytes_stream_inline_filter_fallback_uses_bytes_abi);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
import std::stream;

fn main() -> int {
    let (sink, input) = stream.bytes_pipe(8);
    for await item in input.filter((b) => b.len() > 0) {
        return item.len();
    }
    0
}
  )",
                             program)) {
    FAIL("failed to load typed bytes-stream inline filter fallback program");
    return;
  }

  auto *fn = findFunctionDecl(program, "main");
  auto *forStmt = fn ? findFirstForStmt(*fn) : nullptr;
  if (!fn || !forStmt) {
    FAIL("main or for-await statement missing in bytes-stream inline filter fallback test");
    return;
  }

  auto *filterCall = std::get_if<hew::ast::ExprMethodCall>(&forStmt->iterable.value.kind);
  if (!filterCall || filterCall->method != "filter" || !filterCall->receiver) {
    FAIL("expected iterable to be input.filter(...)");
    return;
  }

  if (!eraseExprTypeEntryForSpan(program, filterCall->receiver->span)) {
    FAIL("failed to remove expr_types entry for filter receiver input");
    return;
  }
  if (!eraseExprTypeEntryForSpan(program, forStmt->iterable.span)) {
    FAIL("failed to remove expr_types entry for inline filter iterable");
    return;
  }

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  auto module = mlirGen.generate(program);
  if (!module) {
    FAIL("expected MLIR generation success for bytes-stream inline filter fallback");
    return;
  }

  auto mainFn = lookupFuncBySuffix(module, "main");
  if (!mainFn) {
    FAIL("main function not found in bytes-stream inline filter fallback test");
    module.getOperation()->destroy();
    return;
  }

  int filterBytesCount =
      countRuntimeCallsByCallee(mainFn.getOperation(), "hew_stream_filter_bytes");
  int filterStringCount =
      countRuntimeCallsByCallee(mainFn.getOperation(), "hew_stream_filter_string");
  int nextBytesCount = countRuntimeCallsByCallee(mainFn.getOperation(), "hew_stream_next_bytes");
  int nextStringCount = countRuntimeCallsByCallee(mainFn.getOperation(), "hew_stream_next");
  int vecFreeCount = countDropOpsByDropFn(mainFn.getOperation(), "hew_vec_free", false);

  if (filterBytesCount != 1) {
    FAIL("expected bytes-stream filter fallback to lower to hew_stream_filter_bytes");
    module.getOperation()->destroy();
    return;
  }
  if (filterStringCount != 0) {
    FAIL("unexpected string-stream filter runtime call in bytes-stream inline filter fallback");
    module.getOperation()->destroy();
    return;
  }
  if (nextBytesCount != 1) {
    FAIL("expected bytes-stream inline filter iteration to lower to hew_stream_next_bytes");
    module.getOperation()->destroy();
    return;
  }
  if (nextStringCount != 0) {
    FAIL("unexpected string-stream next runtime call in bytes-stream inline filter fallback");
    module.getOperation()->destroy();
    return;
  }
  if (vecFreeCount < 1) {
    FAIL("expected bytes-stream inline filter fallback to register hew_vec_free");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: bytes-stream ABI selected via explicit known-call name only (no expr_types)
// Proves the fail-closed contract: after removing resolvedTypeOf from
// resolveStreamHandleInfo, ABI selection relies solely on tracked bindings and
// the known-call-name table — not on incidental type-map lookups.
// Tests the direct iteration path (no intermediate method chain or alias).
// ============================================================================
static void test_for_await_bytes_stream_known_call_name_abi_no_expr_types() {
  TEST(for_await_bytes_stream_known_call_name_abi_no_expr_types);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
import std::stream;

fn main() -> int {
    let (sink, input) = stream.bytes_pipe(8);
    for await item in input {
        return item.len();
    }
    0
}
  )",
                             program)) {
    FAIL("failed to load bytes-stream known-call-name test program");
    return;
  }

  auto *fn = findFunctionDecl(program, "main");
  auto *forStmt = fn ? findFirstForStmt(*fn) : nullptr;
  if (!fn || !forStmt) {
    FAIL("main or for-await statement missing in direct bytes-stream binding test");
    return;
  }

  // Erase only the iterable's expr_types entry so that resolvedTypeOf cannot
  // supply stream kind/element info for the for-await target. The bytes ABI
  // must come exclusively from the tracked binding recorded when the
  // bytes_pipe tuple was destructured — exercising the fail-closed path.
  if (!eraseExprTypeEntryForSpan(program, forStmt->iterable.span)) {
    FAIL("failed to remove expr_types entry for for-await iterable");
    return;
  }

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  auto module = mlirGen.generate(program);
  if (!module) {
    FAIL("expected MLIR generation success with direct bytes-stream binding (no expr_types)");
    return;
  }

  auto mainFn = lookupFuncBySuffix(module, "main");
  if (!mainFn) {
    FAIL("main function not found in known-call-name bytes ABI test");
    module.getOperation()->destroy();
    return;
  }

  if (countRuntimeCallsByCallee(mainFn.getOperation(), "hew_stream_next_bytes") != 1) {
    FAIL("expected hew_stream_next_bytes for direct bytes-stream binding iteration");
    module.getOperation()->destroy();
    return;
  }
  if (countRuntimeCallsByCallee(mainFn.getOperation(), "hew_stream_next") != 0) {
    FAIL("unexpected hew_stream_next (string ABI) for bytes-stream binding");
    module.getOperation()->destroy();
    return;
  }
  if (countDropOpsByDropFn(mainFn.getOperation(), "hew_vec_free", false) < 1) {
    FAIL("expected hew_vec_free drop for bytes-stream iteration element");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}
static void test_function_signature_type_infer_fails_closed() {
  TEST(function_signature_type_infer_fails_closed);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
fn id(x: i32) -> i32 {
    x
}
  )",
                             program)) {
    FAIL("failed to load typed program");
    return;
  }

  auto *fn = findFunctionDecl(program, "id");
  if (!fn) {
    FAIL("id function not found");
    return;
  }

  if (fn->params.empty()) {
    FAIL("id function has no parameters");
    return;
  }

  fn->params[0].ty.value.kind = hew::ast::TypeInfer{};

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure when function parameter type becomes TypeInfer");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("unresolved type inference placeholder `_` reached MLIR codegen boundary") ==
      std::string::npos) {
    FAIL("expected convertType TypeInfer boundary diagnostic");
    return;
  }

  if (stderrText.find("module verification failed") != std::string::npos) {
    FAIL("unexpected downstream verifier failure for function signature TypeInfer");
    return;
  }

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

static void test_identifier_local_assignment_kind_mismatch_fails_closed() {
  TEST(identifier_local_assignment_kind_mismatch_fails_closed);

  auto program = makeIdentifierAssignmentAuthorityProgram(hew::ast::AssignTargetKindActorField{});

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure when local assignment metadata is rewritten");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("assign_target_kinds says ActorField but assignment is not inside an actor "
                      "body") == std::string::npos) {
    FAIL("expected local-assignment authority mismatch fail-closed diagnostic");
    return;
  }

  if (stderrText.find("module verification failed") != std::string::npos) {
    FAIL("unexpected downstream verifier failure for local assignment authority mismatch");
    return;
  }

  PASS();
}

static void test_identifier_actor_field_assignment_kind_mismatch_fails_closed() {
  TEST(identifier_actor_field_assignment_kind_mismatch_fails_closed);

  auto program = makeActorFieldAssignmentAuthorityProgram(hew::ast::AssignTargetKindLocalVar{});

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure when actor-field metadata is rewritten");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("assign_target_kinds says LocalVar but no local binding named 'total' is "
                      "available for assignment") == std::string::npos) {
    FAIL("expected actor-field authority mismatch fail-closed diagnostic");
    return;
  }

  if (stderrText.find("module verification failed") != std::string::npos) {
    FAIL("unexpected downstream verifier failure for actor-field authority mismatch");
    return;
  }

  PASS();
}

static void test_missing_struct_field_assignment_fails_closed() {
  TEST(missing_struct_field_assignment_fails_closed);

  auto program = makeMissingStructFieldAssignmentProgram();

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure when field assignment metadata names a missing field");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("checker invariant violated: field assignment target is missing field "
                      "'missing'") == std::string::npos) {
    FAIL("expected missing-field assignment invariant diagnostic");
    return;
  }

  PASS();
}

static void test_nonstruct_field_assignment_fails_closed() {
  TEST(nonstruct_field_assignment_fails_closed);

  auto program = makeNonStructFieldAssignmentProgram();

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure when field assignment metadata targets a non-struct");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("checker invariant violated: non-struct value field assignment reached "
                      "MLIRGen") == std::string::npos) {
    FAIL("expected non-struct field-assignment invariant diagnostic");
    return;
  }

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
    Ok(Some(11));
    Err(Some(13));
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
  bool optionSomeTypeIsI64 = false;
  bool resultOkUsesFieldOne = false;
  bool resultOkTypeIsI64I64 = false;
  bool resultOkCompositeTypeIsOptionI64 = false;
  bool resultErrUsesFieldTwo = false;
  bool resultErrTypeIsI64I64 = false;
  bool resultErrCompositeTypeIsOptionI64 = false;
  module.walk([&](hew::EnumConstructOp op) {
    auto positions = op.getPayloadPositions();
    if (!positions || positions->size() != 1)
      return;

    auto payloadField = mlir::cast<mlir::IntegerAttr>((*positions)[0]).getInt();
    if (op.getEnumName() == "Option" && op.getVariantIndex() == 1) {
      optionSomeCount++;
      optionSomeUsesFieldOne |= payloadField == 1;
      if (auto optionType = mlir::dyn_cast<hew::OptionEnumType>(op.getType()))
        optionSomeTypeIsI64 |= optionType.getInnerType().isInteger(64);
      return;
    }
    if (op.getEnumName() != "__Result")
      return;
    if (op.getVariantIndex() == 0) {
      resultOkCount++;
      resultOkUsesFieldOne |= payloadField == 1;
      if (auto resultType = mlir::dyn_cast<hew::ResultEnumType>(op.getType())) {
        auto okType = resultType.getOkType();
        auto errType = resultType.getErrType();
        resultOkTypeIsI64I64 |= okType.isInteger(64) && errType.isInteger(64);
        auto okOption = mlir::dyn_cast<hew::OptionEnumType>(okType);
        auto errOption = mlir::dyn_cast<hew::OptionEnumType>(errType);
        resultOkCompositeTypeIsOptionI64 |= okOption && errOption &&
                                            okOption.getInnerType().isInteger(64) &&
                                            errOption.getInnerType().isInteger(64);
      }
      return;
    }
    if (op.getVariantIndex() == 1) {
      resultErrCount++;
      resultErrUsesFieldTwo |= payloadField == 2;
      if (auto resultType = mlir::dyn_cast<hew::ResultEnumType>(op.getType())) {
        auto okType = resultType.getOkType();
        auto errType = resultType.getErrType();
        resultErrTypeIsI64I64 |= okType.isInteger(64) && errType.isInteger(64);
        auto okOption = mlir::dyn_cast<hew::OptionEnumType>(okType);
        auto errOption = mlir::dyn_cast<hew::OptionEnumType>(errType);
        resultErrCompositeTypeIsOptionI64 |= okOption && errOption &&
                                             okOption.getInnerType().isInteger(64) &&
                                             errOption.getInnerType().isInteger(64);
      }
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
  if (!optionSomeTypeIsI64 || !resultOkTypeIsI64I64 || !resultErrTypeIsI64I64) {
    FAIL("builtin enum constructors should materialize concrete primitive Option/Result types");
    module.getOperation()->destroy();
    return;
  }
  if (!resultOkCompositeTypeIsOptionI64 || !resultErrCompositeTypeIsOptionI64) {
    FAIL("builtin Result constructors should prefer resolved expr types for composite payloads");
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
// Test: TypeDecl-based wire struct rejects missing field metadata
// ============================================================================
static void test_wire_struct_typedecl_missing_field_metadata_rejects() {
  TEST(wire_struct_typedecl_missing_field_metadata_rejects);

  using namespace hew::ast;

  uint64_t nextSpan = 930000000000ULL;
  auto mkSpan = [&]() -> Span {
    auto start = nextSpan;
    nextSpan += 8;
    return {start, start + 1};
  };
  auto mkType = [&](llvm::StringRef name) -> Spanned<TypeExpr> {
    TypeExpr typeExpr;
    typeExpr.kind = TypeNamed{name.str(), std::nullopt};
    auto span = mkSpan();
    return {std::move(typeExpr), span};
  };
  auto mkIntExpr = [&](int64_t value) -> std::unique_ptr<Spanned<Expr>> {
    Expr expr;
    auto span = mkSpan();
    expr.kind = ExprLiteral{LitInteger{value}};
    expr.span = span;
    return std::make_unique<Spanned<Expr>>(Spanned<Expr>{std::move(expr), span});
  };

  WireDecl packetDecl;
  packetDecl.kind = WireDeclKind::Struct;
  packetDecl.name = "Packet";
  packetDecl.fields.push_back(WireFieldDecl{"id", "int", 1, false, false, false, false,
                                            std::nullopt, std::nullopt, std::nullopt});
  packetDecl.fields.push_back(WireFieldDecl{"count", "int", 2, false, false, false, false,
                                            std::nullopt, std::nullopt, std::nullopt});

  FnDecl mainFn;
  mainFn.name = "main";
  mainFn.return_type = mkType("int");
  mainFn.body.trailing_expr = mkIntExpr(0);

  Program program;
  Item packetItem;
  packetItem.kind = std::move(packetDecl);
  program.items.push_back({std::move(packetItem), mkSpan()});
  Item mainItem;
  mainItem.kind = std::move(mainFn);
  program.items.push_back({std::move(mainItem), mkSpan()});

  if (!desugarWireStructToTypeDecl(program, "Packet")) {
    FAIL("failed to desugar wire struct into TypeDecl");
    return;
  }

  auto *typeDecl = std::get_if<hew::ast::TypeDecl>(&program.items.front().value.kind);
  if (!typeDecl || !typeDecl->wire.has_value() || typeDecl->wire->field_meta.size() != 2) {
    FAIL("failed to build TypeDecl wire metadata for Packet");
    return;
  }
  typeDecl->wire->field_meta.pop_back();

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure when TypeDecl wire metadata drops a body field");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("wire metadata for type 'Packet' is missing body field 'count'") ==
      std::string::npos) {
    FAIL("expected missing wire field metadata diagnostic");
    return;
  }

  if (stderrText.find("module verification failed") != std::string::npos) {
    FAIL("unexpected downstream verifier failure for missing wire field metadata");
    return;
  }

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
// Test: computed select timeout is evaluated before asks are armed
// ============================================================================
static void test_select_computed_timeout_evaluates_before_arming_asks() {
  TEST(select_computed_timeout_evaluates_before_arming_asks);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
actor Responder {
    receive fn get() -> int {
        sleep_ms(50);
        1
    }
}

fn delayed_zero_timeout() -> duration {
    sleep_ms(250);
    0ms + 0ms
}

fn main() -> int {
    let responder = spawn Responder();
    select {
        x from responder.get() => x,
        after delayed_zero_timeout() => -1,
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

  int timeoutCallIndex = -1;
  int firstSelectAddIndex = -1;
  int opIndex = 0;
  mainFn.walk([&](mlir::Operation *op) {
    if (timeoutCallIndex < 0) {
      if (auto call = llvm::dyn_cast<mlir::func::CallOp>(op);
          call && !call.getCallee().starts_with("hew_")) {
        timeoutCallIndex = opIndex;
      }
    }
    if (firstSelectAddIndex < 0 && llvm::isa<hew::SelectAddOp>(op))
      firstSelectAddIndex = opIndex;
    ++opIndex;
  });

  if (timeoutCallIndex < 0 || firstSelectAddIndex < 0) {
    FAIL("expected computed-timeout call and select.add in main");
    module.getOperation()->destroy();
    return;
  }

  if (timeoutCallIndex >= firstSelectAddIndex) {
    FAIL("computed timeout must be evaluated before any select.add arms are sent");
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
// Test: scoped actor spawns clean up and panic when scope registration fails
// ============================================================================
static void test_scoped_spawn_panics_on_scope_overflow() {
  TEST(scoped_spawn_panics_on_scope_overflow);

  mlir::MLIRContext ctx;
  initContext(ctx);
  auto module = generateMLIR(ctx, R"(
actor Worker {
    receive fn ping() {
    }
}

fn main() {
    scope {
        let worker = spawn Worker;
        worker.ping();
    };
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

  if (countRuntimeCallsByCallee(mainFn.getOperation(), "hew_scope_spawn") != 1) {
    FAIL("scoped spawn should call hew_scope_spawn exactly once");
    module.getOperation()->destroy();
    return;
  }

  hew::RuntimeCallOp scopeSpawnCall;
  mainFn.walk([&](hew::RuntimeCallOp call) {
    if (!scopeSpawnCall && call.getCallee().str() == "hew_scope_spawn")
      scopeSpawnCall = call;
  });
  if (!scopeSpawnCall) {
    FAIL("scoped spawn did not emit hew_scope_spawn runtime call");
    module.getOperation()->destroy();
    return;
  }

  mlir::arith::CmpIOp scopeOverflowCheck;
  mlir::Value scopeSpawnStatus = scopeSpawnCall.getResult();
  for (auto *user : scopeSpawnStatus.getUsers()) {
    auto cmp = mlir::dyn_cast<mlir::arith::CmpIOp>(user);
    if (!cmp)
      continue;
    if (cmp.getPredicate() != mlir::arith::CmpIPredicate::ne &&
        cmp.getPredicate() != mlir::arith::CmpIPredicate::eq)
      continue;

    if (cmp.getLhs() == scopeSpawnStatus && isZeroLiteralValue(cmp.getRhs())) {
      scopeOverflowCheck = cmp;
      break;
    }
    if (cmp.getRhs() == scopeSpawnStatus && isZeroLiteralValue(cmp.getLhs())) {
      scopeOverflowCheck = cmp;
      break;
    }
  }

  if (!scopeOverflowCheck) {
    FAIL("scoped spawn must compare hew_scope_spawn status against zero");
    module.getOperation()->destroy();
    return;
  }

  bool foundOverflowCleanup = false;
  mainFn.walk([&](mlir::scf::IfOp ifOp) {
    if (foundOverflowCleanup || ifOp.getCondition() != scopeOverflowCheck.getResult())
      return;

    bool sawClose = false;
    bool sawFree = false;
    bool sawPanicMsg = false;
    ifOp.getThenRegion().walk([&](hew::RuntimeCallOp call) {
      auto callee = call.getCallee().str();
      sawClose |= callee == "hew_actor_close";
      sawFree |= callee == "hew_actor_free";
      sawPanicMsg |= callee == "hew_panic_msg";
    });
    foundOverflowCleanup = sawClose && sawFree && sawPanicMsg;
  });

  if (!foundOverflowCleanup) {
    FAIL("scoped spawn overflow path must close/free the actor and panic with a diagnostic");
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
// Test: lambda actor receive handler with owned param emits hew_string_drop
// ============================================================================

static void test_lambda_actor_receive_string_param_drop() {
  TEST(lambda_actor_receive_string_param_drop);

  mlir::MLIRContext ctx;
  initContext(ctx);

  auto module = generateMLIR(ctx, R"(
fn main() {
    let handler = spawn (msg: String) => {
    };
}
  )");

  if (!module) {
    FAIL("MLIR generation failed for lambda actor String receive param");
    return;
  }

  auto recvFn = lookupFuncBySuffix(module, "__lambda_actor_0_receive");
  if (!recvFn) {
    FAIL("lambda actor receive function not found");
    module.getOperation()->destroy();
    return;
  }

  if (countDropOpsByDropFn(recvFn, "hew_string_drop", false) < 1) {
    FAIL("expected lambda actor receive handler to drop owned String params");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: lambda actor receive body clears enclosing funcLevelDropExclude state
// ============================================================================

static void test_lambda_actor_receive_clears_enclosing_drop_excludes() {
  TEST(lambda_actor_receive_clears_enclosing_drop_excludes);

  mlir::MLIRContext ctx;
  initContext(ctx);

  auto module = generateMLIR(ctx, R"(
fn make_actor() -> String {
    let kept = int_to_string(7);
    let worker = spawn (kept: String) => {
    };
    kept
}

fn main() -> int {
    let _ = make_actor();
    0
}
  )");

  if (!module) {
    FAIL("MLIR generation failed for lambda actor drop-exclude regression");
    return;
  }

  auto recvFn = lookupFuncBySuffix(module, "__lambda_actor_0_receive");
  if (!recvFn) {
    FAIL("lambda actor receive function not found");
    module.getOperation()->destroy();
    return;
  }

  if (countDropOpsByDropFn(recvFn, "hew_string_drop", false) < 1) {
    FAIL("lambda actor receive handler should not inherit enclosing function drop exclusions");
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
// Test: shared resolved-type classifier canonicalizes aliases + qualified names
// ============================================================================

static void test_resolved_type_classifier_canonicalizes_aliases_and_qualified_receivers() {
  TEST(resolved_type_classifier_canonicalizes_aliases_and_qualified_receivers);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
import std::channel::channel;
import std::stream;
import std::text::regex;

actor Stats {
    receive fn ping() {}
}

type Greeter {
    greeting: String;
}

enum Mode {
    Idle;
    Busy;
}

type RemoteStats = Stats;
type GreeterAlias = Greeter;
type ModeAlias = Mode;
type Input = stream.Stream<String>;
type Inbox = channel.Receiver<String>;
type PatternAlias = regex.Pattern;
type ActorVec = Vec<ActorRef<Stats>>;
type Score = int;
type Count = uint;
type Text = String;
type LowerText = str;
type ByteAlias = byte;
type FlagAlias = Bool;
type Timeout = Duration;

fn make_input() -> stream.Stream<String> {
    let (_sink, input) = stream.pipe(1);
    input
}

fn drain(rx: channel.Receiver<String>) {
    rx.close();
}

fn drain_alias(rx: Inbox) {
    rx.close();
}

fn main() {
    let remote: RemoteStats = Node::lookup("stats");
    let greeter: GreeterAlias = Greeter { greeting: "hi" };
    let mode: ModeAlias = Idle;
    let input: Input = make_input();
    let pattern: PatternAlias = regex.new("[0-9]+");
    let actors: ActorVec = Vec::new();
    let score: Score = 1;
    let count: Count = 2;
    let text: Text = "hi";
    let lower: LowerText = "bye";
    let byte_value: ByteAlias = 7;
    let flag: FlagAlias = true;
    let timeout: Timeout = 1s;
}
  )",
                             program)) {
    FAIL("failed to load typed program for resolved-type classifier test");
    return;
  }

  auto findFunction = [&](llvm::StringRef name) -> const hew::ast::FnDecl * {
    for (const auto &item : program.items) {
      if (auto *fn = std::get_if<hew::ast::FnDecl>(&item.value.kind); fn && fn->name == name)
        return fn;
    }
    return nullptr;
  };
  auto findLet = [](const hew::ast::FnDecl &fn, llvm::StringRef name) -> const hew::ast::StmtLet * {
    for (const auto &stmt : fn.body.stmts) {
      auto *letStmt = std::get_if<hew::ast::StmtLet>(&stmt->value.kind);
      if (!letStmt)
        continue;
      auto *ident = std::get_if<hew::ast::PatIdentifier>(&letStmt->pattern.value.kind);
      if (ident && ident->name == name)
        return letStmt;
    }
    return nullptr;
  };
  auto resolveAlias = [&](llvm::StringRef name) -> const hew::ast::TypeExpr * {
    for (const auto &item : program.items) {
      if (auto *alias = std::get_if<hew::ast::TypeAliasDecl>(&item.value.kind);
          alias && alias->name == name) {
        return &alias->ty.value;
      }
    }
    return nullptr;
  };

  const auto *mainFn = findFunction("main");
  const auto *makeInputFn = findFunction("make_input");
  const auto *drainFn = findFunction("drain");
  const auto *drainAliasFn = findFunction("drain_alias");
  if (!mainFn || !makeInputFn || !makeInputFn->return_type || !drainFn || !drainAliasFn) {
    FAIL("expected helper functions in resolved-type classifier test program");
    return;
  }

  const auto *remoteLet = findLet(*mainFn, "remote");
  const auto *greeterLet = findLet(*mainFn, "greeter");
  const auto *modeLet = findLet(*mainFn, "mode");
  const auto *inputLet = findLet(*mainFn, "input");
  const auto *patternLet = findLet(*mainFn, "pattern");
  const auto *actorsLet = findLet(*mainFn, "actors");
  const auto *scoreLet = findLet(*mainFn, "score");
  const auto *countLet = findLet(*mainFn, "count");
  const auto *textLet = findLet(*mainFn, "text");
  const auto *lowerLet = findLet(*mainFn, "lower");
  const auto *byteValueLet = findLet(*mainFn, "byte_value");
  const auto *flagLet = findLet(*mainFn, "flag");
  const auto *timeoutLet = findLet(*mainFn, "timeout");
  if (!remoteLet || !remoteLet->ty || !greeterLet || !greeterLet->ty || !modeLet || !modeLet->ty ||
      !inputLet || !inputLet->ty || !patternLet || !patternLet->ty || !actorsLet ||
      !actorsLet->ty || !scoreLet || !scoreLet->ty || !countLet || !countLet->ty || !textLet ||
      !textLet->ty || !lowerLet || !lowerLet->ty || !byteValueLet || !byteValueLet->ty ||
      !flagLet || !flagLet->ty || !timeoutLet || !timeoutLet->ty) {
    FAIL("expected typed let bindings in resolved-type classifier test program");
    return;
  }

  std::unordered_set<std::string> knownHandles(program.handle_types.begin(),
                                               program.handle_types.end());
  if (!knownHandles.count("regex.Pattern") || !knownHandles.count("channel.Receiver") ||
      !knownHandles.count("stream.Stream")) {
    FAIL("expected regex/channel/stream handles in metadata-driven known handle set");
    return;
  }

  if (hew::typeExprToTypeName(remoteLet->ty->value, resolveAlias) != "Stats") {
    FAIL("aliased actor receiver should resolve to canonical actor name");
    return;
  }
  if (hew::typeExprToTypeName(greeterLet->ty->value, resolveAlias) != "Greeter") {
    FAIL("aliased struct receiver should resolve to canonical struct name");
    return;
  }
  if (hew::typeExprToTypeName(modeLet->ty->value, resolveAlias) != "Mode") {
    FAIL("aliased enum receiver should resolve to canonical enum name");
    return;
  }
  if (hew::typeExprStreamKind(makeInputFn->return_type->value, resolveAlias) != "Stream") {
    FAIL("module-qualified stream type should classify as Stream");
    return;
  }
  if (hew::typeExprStreamKind(inputLet->ty->value, resolveAlias) != "Stream") {
    FAIL("aliased stream receiver should classify as Stream");
    return;
  }
  if (!hew::typeExprIsReceiver(drainFn->params[0].ty.value, resolveAlias) ||
      !hew::typeExprIsReceiver(drainAliasFn->params[0].ty.value, resolveAlias)) {
    FAIL("qualified and aliased receiver types should classify as Receiver");
    return;
  }
  if (hew::typeExprToHandleString(drainAliasFn->params[0].ty.value, knownHandles, resolveAlias) !=
      "channel.Receiver") {
    FAIL("aliased receiver handle should resolve to channel.Receiver");
    return;
  }
  if (hew::typeExprToHandleString(patternLet->ty->value, knownHandles, resolveAlias) !=
      "regex.Pattern") {
    FAIL("aliased handle should resolve to regex.Pattern");
    return;
  }
  if (hew::typeExprToCollectionString(actorsLet->ty->value, resolveAlias) !=
      "Vec<ActorRef<Stats>>") {
    FAIL("aliased collection receiver should preserve canonical nested actor type");
    return;
  }
  if (hew::resolvedTypeExprString(scoreLet->ty->value, resolveAlias) != "i64" ||
      hew::resolvedTypeExprString(countLet->ty->value, resolveAlias) != "u64" ||
      hew::resolvedTypeExprString(textLet->ty->value, resolveAlias) != "string" ||
      hew::resolvedTypeExprString(lowerLet->ty->value, resolveAlias) != "string" ||
      hew::resolvedTypeExprString(byteValueLet->ty->value, resolveAlias) != "u8" ||
      hew::resolvedTypeExprString(flagLet->ty->value, resolveAlias) != "bool" ||
      hew::resolvedTypeExprString(timeoutLet->ty->value, resolveAlias) != "duration") {
    FAIL("primitive aliases should resolve to canonical lowering spellings");
    return;
  }

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
// Test: local void actor asks consult hew_actor_ask_take_last_error and panic
//       on failure (fail-closed parity with the non-void path).
// hew_actor_ask returns null for BOTH void success and ask failure.  The
// codegen must call hew_actor_ask_take_last_error to distinguish the two and
// branch to hew_panic when the error slot is non-zero.
// ============================================================================

static void test_local_actor_void_ask_panics_on_failed_ask() {
  TEST(local_actor_void_ask_panics_on_failed_ask);

  mlir::MLIRContext ctx;
  initContext(ctx);

  auto module = generateMLIR(ctx, R"(
actor Sink {
    receive fn flush() {
    }
}

fn main() {
    let s = spawn Sink();
    await s.flush()
}
  )");

  if (!module) {
    FAIL("MLIR generation failed for local void actor ask");
    return;
  }

  hew::Codegen codegen(ctx);
  hew::CodegenOptions opts;
  opts.debug_info = true;
  llvm::LLVMContext llvmContext;
  auto llvmModule = codegen.buildLLVMModule(module, opts, llvmContext);
  module.getOperation()->destroy();

  if (!llvmModule) {
    FAIL("LLVM lowering failed for local void actor ask");
    return;
  }

  auto *mainFn = llvmModule->getFunction("main");
  if (!mainFn) {
    FAIL("main function not found in lowered LLVM module");
    return;
  }

  // Find the hew_actor_ask call.
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
    FAIL("expected lowered local void ask to call hew_actor_ask");
    return;
  }

  // Find hew_actor_ask_take_last_error called after the ask.
  llvm::CallBase *takeErrCall = nullptr;
  for (auto &block : *mainFn) {
    for (auto &inst : block) {
      auto *call = llvm::dyn_cast<llvm::CallBase>(&inst);
      if (!call)
        continue;
      auto *callee = call->getCalledFunction();
      if (callee && callee->getName() == "hew_actor_ask_take_last_error") {
        takeErrCall = call;
        break;
      }
    }
    if (takeErrCall)
      break;
  }

  if (!takeErrCall) {
    FAIL("local void ask must call hew_actor_ask_take_last_error to detect failure");
    return;
  }

  // The error value must be compared against zero (ne) to detect failure.
  llvm::ICmpInst *errCheck = nullptr;
  for (llvm::User *user : takeErrCall->users()) {
    auto *icmp = llvm::dyn_cast<llvm::ICmpInst>(user);
    if (!icmp)
      continue;
    if (icmp->getPredicate() != llvm::CmpInst::ICMP_NE &&
        icmp->getPredicate() != llvm::CmpInst::ICMP_EQ)
      continue;
    bool comparesErrToZero = false;
    for (unsigned i = 0; i < 2; ++i) {
      if (icmp->getOperand(i) == takeErrCall) {
        auto *other = icmp->getOperand(1 - i);
        if (auto *ci = llvm::dyn_cast<llvm::ConstantInt>(other))
          if (ci->isZero())
            comparesErrToZero = true;
      }
    }
    if (comparesErrToZero) {
      errCheck = icmp;
      break;
    }
  }

  if (!errCheck) {
    FAIL("local void ask must compare hew_actor_ask_take_last_error result against zero");
    return;
  }

  // The branch on that comparison must lead to hew_panic.
  llvm::BranchInst *errBranch =
      llvm::dyn_cast<llvm::BranchInst>(errCheck->getParent()->getTerminator());
  if (!errBranch || !errBranch->isConditional() || errBranch->getCondition() != errCheck) {
    FAIL("error check must be the condition of a conditional branch");
    return;
  }

  bool branchesToPanic = false;
  for (unsigned i = 0; i < errBranch->getNumSuccessors(); ++i) {
    for (auto &inst : *errBranch->getSuccessor(i)) {
      auto *call = llvm::dyn_cast<llvm::CallBase>(&inst);
      auto *callee = call ? call->getCalledFunction() : nullptr;
      if (callee && callee->getName() == "hew_panic") {
        branchesToPanic = true;
        break;
      }
    }
  }

  if (!branchesToPanic) {
    FAIL("local void ask must branch to hew_panic when error slot is non-zero");
    return;
  }

  PASS();
}

// ============================================================================
// Test: aliased i32-handle call receivers lower through handle dispatch
// ============================================================================

static void test_handle_alias_call_receiver_is_recognized() {
  TEST(handle_alias_call_receiver_is_recognized);

  mlir::MLIRContext ctx;
  initContext(ctx);

  auto module = generateMLIR(ctx, R"(
import std::net;

type Conn = net.Connection;

fn open() -> Conn {
    net.connect("127.0.0.1:1")
}

fn main() -> int {
    open().close()
}
  )");

  if (!module) {
    FAIL("MLIR generation failed for aliased handle call receiver");
    return;
  }

  auto mainFn = lookupFuncBySuffix(module, "main");
  if (!mainFn) {
    FAIL("main function not found for aliased handle call receiver");
    module.getOperation()->destroy();
    return;
  }

  if (countCallsByCallee(mainFn.getOperation(), "hew_tcp_close") != 1) {
    FAIL("expected aliased handle call receiver to lower to hew_tcp_close");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: duration method calls stay green with resolved receiver metadata.
// ============================================================================

static void test_duration_method_dispatch_uses_resolved_type() {
  TEST(duration_method_dispatch_uses_resolved_type);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
fn nanos_of(d: Duration) -> i64 {
    d.nanos()
}

fn secs_of(d: Duration) -> i64 {
    d.secs()
}

fn main() {}
  )",
                             program)) {
    FAIL("hew CLI unavailable; cannot run duration dispatch positive test");
    return;
  }

  mlir::MLIRContext ctx;
  initContext(ctx);
  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (!module) {
    FAIL("expected codegen to succeed for duration method dispatch");
    return;
  }

  if (!stderrText.empty()) {
    FAIL("expected no diagnostics for duration method dispatch");
    module.getOperation()->destroy();
    return;
  }

  auto secsFn = lookupFuncBySuffix(module, "secs_of");
  if (!secsFn) {
    FAIL("secs_of function not found for duration dispatch test");
    module.getOperation()->destroy();
    return;
  }

  bool hasDivSI = false;
  secsFn.walk([&](mlir::arith::DivSIOp) { hasDivSI = true; });
  if (!hasDivSI) {
    FAIL("expected duration secs() lowering to emit signed division");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: duration method calls fail closed without resolved receiver metadata.
// ============================================================================

static void test_duration_method_dispatch_requires_resolved_type() {
  TEST(duration_method_dispatch_requires_resolved_type);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
fn secs_of(d: Duration) -> i64 {
    d.secs()
}

fn main() {}
  )",
                             program)) {
    FAIL("hew CLI unavailable; cannot run duration dispatch negative test");
    return;
  }

  auto *secsFn = findFunctionDecl(program, "secs_of");
  if (!secsFn) {
    FAIL("failed to find secs_of function for duration dispatch negative test");
    return;
  }

  auto receiverSpan = findFunctionMethodReceiverSpan(*secsFn, "secs");
  if (!receiverSpan || !eraseExprTypeEntryForSpan(program, *receiverSpan)) {
    FAIL("failed to remove duration receiver expr_types entry");
    return;
  }

  mlir::MLIRContext ctx;
  initContext(ctx);
  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected codegen to fail for duration method dispatch without resolved type");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("missing expr_types entry for duration method call receiver") ==
      std::string::npos) {
    FAIL("expected missing expr_types diagnostic for duration method dispatch");
    return;
  }

  PASS();
}

// ============================================================================
// Test: Rc method calls stay green with resolved receiver metadata.
// ============================================================================

static void test_rc_method_dispatch_uses_resolved_type() {
  TEST(rc_method_dispatch_uses_resolved_type);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
fn count_refs() -> i64 {
    let data: Rc<String> = Rc::new("hi");
    let alias: Rc<String> = data.clone();
    alias.strong_count()
}

fn main() {}
  )",
                             program)) {
    FAIL("hew CLI unavailable; cannot run Rc dispatch positive test");
    return;
  }

  mlir::MLIRContext ctx;
  initContext(ctx);
  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (!module) {
    FAIL("expected codegen to succeed for Rc method dispatch");
    return;
  }

  if (!stderrText.empty()) {
    FAIL("expected no diagnostics for Rc method dispatch");
    module.getOperation()->destroy();
    return;
  }

  auto countFn = lookupFuncBySuffix(module, "count_refs");
  if (!countFn) {
    FAIL("count_refs function not found for Rc dispatch test");
    module.getOperation()->destroy();
    return;
  }

  bool hasRcClone = false;
  countFn.walk([&](hew::RcCloneOp) { hasRcClone = true; });
  if (!hasRcClone || countCallsByCallee(countFn.getOperation(), "hew_rc_count") != 1) {
    FAIL("expected Rc dispatch to emit Rc clone and hew_rc_count");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: Rc method calls fail closed without resolved receiver metadata.
// ============================================================================

static void test_rc_method_dispatch_requires_resolved_type() {
  TEST(rc_method_dispatch_requires_resolved_type);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
fn count_refs() -> i64 {
    let data: Rc<String> = Rc::new("hi");
    data.strong_count()
}

fn main() {}
  )",
                             program)) {
    FAIL("hew CLI unavailable; cannot run Rc dispatch negative test");
    return;
  }

  auto *countFn = findFunctionDecl(program, "count_refs");
  if (!countFn) {
    FAIL("failed to find count_refs function for Rc dispatch negative test");
    return;
  }

  auto receiverSpan = findFunctionMethodReceiverSpan(*countFn, "strong_count");
  if (!receiverSpan || !eraseExprTypeEntryForSpan(program, *receiverSpan)) {
    FAIL("failed to remove Rc receiver expr_types entry");
    return;
  }

  mlir::MLIRContext ctx;
  initContext(ctx);
  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected codegen to fail for Rc method dispatch without resolved type");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("missing expr_types entry for Rc method call receiver") ==
      std::string::npos) {
    FAIL("expected missing expr_types diagnostic for Rc method dispatch");
    return;
  }

  PASS();
}

// ============================================================================
// Test: aliased Node::lookup receivers lower through remote actor ask dispatch
// ============================================================================
// Test: handle dispatch survives when receiver expr_types metadata is absent.
//
// The checker now carries handle-dispatch authority in method_call_receiver_kinds.
// Even if the restored receiver span lacks expr_types metadata, codegen should
// still lower the method call through the authoritative receiver-kind entry.
// ============================================================================

static void test_handle_dispatch_uses_receiver_kind_metadata() {
  TEST(handle_dispatch_uses_receiver_kind_metadata);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
import std::net;

extern "C" {
    fn fake_conn() -> net.Connection;
}

fn use_conn() -> int {
    let conn: net.Connection = unsafe { fake_conn() };
    return conn.close();
}

fn main() {}
  )",
                             program)) {
    FAIL("hew CLI unavailable or std::net not found; cannot run handle dispatch negative test");
    return;
  }

  auto *useConn = findFunctionDecl(program, "use_conn");
  if (!useConn) {
    FAIL("failed to find use_conn function for handle dispatch negative test");
    return;
  }

  // Handle methods are rewritten to C calls during enrichment. Restore the
  // method-call shape here so the C++ MLIRGen handle-dispatch path sees the
  // same receiver expression and span metadata it would have consumed before
  // serialization rewrote `conn.close()` to `hew_tcp_close(conn)`.
  auto receiverSpan = restoreReturnedHandleMethodCall(*useConn, "hew_tcp_close", "close");
  auto *restoredReceiverSpan = findMutableReturnedMethodReceiverSpan(*useConn, "close");
  if (!receiverSpan || !restoredReceiverSpan) {
    FAIL("failed to restore handle dispatch shape");
    return;
  }
  *restoredReceiverSpan = {990000000000ULL, 990000000001ULL};

  // Simulate absent expr_types metadata at the restored receiver site by
  // moving the receiver to a fresh span with no expr_types entry. The
  // method_call_receiver_kinds entry on the method call itself must remain
  // sufficient for i32-backed handle dispatch.

  mlir::MLIRContext ctx;
  initContext(ctx);
  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (!module) {
    FAIL("expected codegen to succeed for handle dispatch with receiver-kind metadata");
    return;
  }

  if (!stderrText.empty()) {
    FAIL("expected no diagnostics for handle dispatch backed by receiver-kind metadata");
    module.getOperation()->destroy();
    return;
  }

  auto useConnFn = lookupFuncBySuffix(module, "use_conn");
  if (!useConnFn) {
    FAIL("use_conn function not found for handle dispatch metadata test");
    module.getOperation()->destroy();
    return;
  }

  if (countCallsByCallee(useConnFn.getOperation(), "hew_tcp_close") +
          countRuntimeCallsByCallee(useConnFn.getOperation(), "hew_tcp_close") !=
      1) {
    FAIL("expected handle dispatch to lower to one hew_tcp_close call via receiver-kind metadata");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: handle dispatch requires a checker-carried receiver-kind entry.
// ============================================================================

static void test_handle_dispatch_requires_receiver_kind() {
  TEST(handle_dispatch_requires_receiver_kind);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
import std::net;

extern "C" {
    fn fake_conn() -> net.Connection;
}

fn use_conn() -> int {
    let conn: net.Connection = unsafe { fake_conn() };
    return conn.close();
}

fn main() {}
  )",
                             program)) {
    FAIL("hew CLI unavailable or std::net not found; cannot run handle dispatch negative test");
    return;
  }

  auto *useConn = findFunctionDecl(program, "use_conn");
  if (!useConn) {
    FAIL("failed to find use_conn function for handle dispatch negative test");
    return;
  }

  auto receiverSpan = restoreReturnedHandleMethodCall(*useConn, "hew_tcp_close", "close");
  auto methodCallSpan = findFunctionMethodCallSpan(*useConn, "close");
  if (!receiverSpan || !methodCallSpan) {
    FAIL("failed to restore handle dispatch shape");
    return;
  }
  (void)eraseMethodCallReceiverKindEntryForSpan(program, *methodCallSpan);

  mlir::MLIRContext ctx;
  initContext(ctx);
  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected codegen to fail for handle dispatch without receiver-kind metadata");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("missing method_call_receiver_kinds entry for handle method call") ==
      std::string::npos) {
    FAIL("expected missing method_call_receiver_kinds diagnostic for handle dispatch");
    return;
  }

  PASS();
}

// ============================================================================
// Test: actor dispatch requires a resolved receiver type annotation.
//
// When the receiver's expr_types entry is removed, the actorVarTypes
// identifier fallback no longer exists. An actor receive-method call with no
// dispatch-site type annotation must fail closed instead of silently emitting
// wrong code.
// ============================================================================

static void test_actor_dispatch_requires_resolved_type() {
  TEST(actor_dispatch_requires_resolved_type);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
actor Counter {
    let count: int;
    receive fn get() -> int {
        count
    }
}

fn use_counter(ref_: ActorRef<Counter>) -> int {
    return ref_.get();
}

fn main() {}
  )",
                             program)) {
    FAIL("hew CLI unavailable; cannot run actor dispatch negative test");
    return;
  }

  auto *useCounter = findFunctionDecl(program, "use_counter");
  if (!useCounter) {
    FAIL("failed to find use_counter function for actor dispatch negative test");
    return;
  }

  auto receiverSpan = findFunctionMethodReceiverSpan(*useCounter, "get");
  if (!receiverSpan || !eraseExprTypeEntryForSpan(program, *receiverSpan)) {
    FAIL("failed to remove actor receiver expr_types entry");
    return;
  }

  // Simulate absent type-checker metadata at the dispatch site: the old
  // actorVarTypes fallback would have rescued this call; after its removal,
  // codegen must fail.

  mlir::MLIRContext ctx;
  initContext(ctx);
  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected codegen to fail for actor dispatch without resolved type annotation");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("method call on non-struct/enum type") == std::string::npos) {
    FAIL("expected 'method call on non-struct/enum type' diagnostic for unresolved actor dispatch");
    return;
  }

  PASS();
}

// ============================================================================
// Test: trait dispatch requires a checker-carried receiver-kind entry.
//
// The checker now records trait-object method dispatch in
// method_call_receiver_kinds. If that entry is removed, codegen must fail
// closed instead of re-discovering trait dispatch structurally.
// ============================================================================

static void test_trait_dispatch_requires_receiver_kind() {
  TEST(trait_dispatch_requires_receiver_kind);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
trait Greeter {
    fn greet(s: Self) -> String;
}

type Bot {
    name: String;
}

impl Greeter for Bot {
    fn greet(s: Bot) -> String {
        "hello"
    }
}

fn use_greeter(g: dyn Greeter) -> String {
    return g.greet();
}

fn main() {}
  )",
                             program)) {
    FAIL("hew CLI unavailable; cannot run trait dispatch negative test");
    return;
  }

  auto *useGreeter = findFunctionDecl(program, "use_greeter");
  if (!useGreeter) {
    FAIL("failed to find use_greeter function for trait dispatch negative test");
    return;
  }

  auto methodCallSpan = findFunctionMethodCallSpan(*useGreeter, "greet");
  if (!methodCallSpan || !eraseMethodCallReceiverKindEntryForSpan(program, *methodCallSpan)) {
    FAIL("failed to remove trait method_call_receiver_kinds entry");
    return;
  }

  mlir::MLIRContext ctx;
  initContext(ctx);
  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected codegen to fail for trait dispatch without receiver-kind metadata");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("missing method_call_receiver_kinds entry for trait object method call") ==
      std::string::npos) {
    FAIL("expected missing method_call_receiver_kinds diagnostic for trait dispatch");
    return;
  }

  PASS();
}

// ============================================================================

static void test_named_type_dispatch_requires_receiver_kind() {
  TEST(named_type_dispatch_requires_receiver_kind);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
type Widget {
    value: i64;
}

impl Widget {
    fn value_plus_one(w: Widget) -> i64 {
        w.value + 1
    }
}

fn use_widget(w: Widget) -> i64 {
    return w.value_plus_one();
}

fn main() {}
  )",
                             program)) {
    FAIL("hew CLI unavailable; cannot run named-type dispatch negative test");
    return;
  }

  auto *useWidget = findFunctionDecl(program, "use_widget");
  if (!useWidget) {
    FAIL("failed to find use_widget function for named-type dispatch negative test");
    return;
  }

  auto methodCallSpan = findFunctionMethodCallSpan(*useWidget, "value_plus_one");
  if (!methodCallSpan || !eraseMethodCallReceiverKindEntryForSpan(program, *methodCallSpan)) {
    FAIL("failed to remove named-type method_call_receiver_kinds entry");
    return;
  }

  mlir::MLIRContext ctx;
  initContext(ctx);
  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected codegen to fail for named-type dispatch without receiver-kind metadata");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("missing method_call_receiver_kinds entry for named-type method call") ==
      std::string::npos) {
    FAIL("expected missing method_call_receiver_kinds diagnostic for named-type dispatch");
    return;
  }

  PASS();
}

// ============================================================================
// Test: generic handle-backed impl dispatch requires receiver-kind metadata.
//
// Before this fix, json.Value method calls bypassed the authority table and
// used the MLIR handle-kind string as a heuristic. Now they must go through
// method_call_receiver_kinds, and missing metadata must fail closed.
// ============================================================================

static void test_generic_handle_impl_dispatch_requires_receiver_kind() {
  TEST(generic_handle_impl_dispatch_requires_receiver_kind);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
import std::encoding::json;

fn use_value(v: json.Value) -> i32 {
    return v.type_of();
}

fn main() {}
  )",
                             program)) {
    FAIL("hew CLI unavailable or std::encoding::json not found; skipping");
    return;
  }

  auto *useValue = findFunctionDecl(program, "use_value");
  if (!useValue) {
    FAIL("failed to find use_value function");
    return;
  }

  auto receiverSpan = restoreReturnedHandleMethodCall(*useValue, "hew_json_type", "type_of");
  if (!receiverSpan) {
    FAIL("failed to restore json.Value.type_of() method call shape");
    return;
  }

  auto methodCallSpan = findFunctionMethodCallSpan(*useValue, "type_of");
  if (!methodCallSpan) {
    FAIL("failed to find restored json.Value.type_of() method call span");
    return;
  }
  (void)eraseMethodCallReceiverKindEntryForSpan(program, *methodCallSpan);

  mlir::MLIRContext ctx;
  initContext(ctx);
  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL(
        "expected codegen to fail for generic handle impl dispatch without receiver-kind metadata");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("missing method_call_receiver_kinds entry") == std::string::npos) {
    FAIL("expected missing method_call_receiver_kinds diagnostic for generic handle impl dispatch");
    return;
  }

  PASS();
}

// ============================================================================
// Test: assignment with missing assign_target_kinds entry fails closed
// ============================================================================
static void test_assignment_missing_target_kind_fails_closed() {
  TEST(assignment_missing_target_kind_fails_closed);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
fn main() -> i64 {
    var x: i64;
    x = 1;
    x
}
  )",
                             program)) {
    FAIL("hew CLI unavailable; cannot run assign_target_kinds negative test");
    return;
  }

  auto *fn = findFunctionDecl(program, "main");
  if (!fn) {
    FAIL("failed to find main function for assign_target_kinds negative test");
    return;
  }

  auto targetSpan = findFunctionAssignTargetSpan(*fn);
  if (!targetSpan || !eraseAssignTargetKindEntryForSpan(program, *targetSpan)) {
    FAIL("failed to remove assign_target_kinds entry for assignment target");
    return;
  }

  mlir::MLIRContext ctx;
  initContext(ctx);
  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected codegen to fail for assignment without target-kind metadata");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("missing assign_target_kinds entry for assignment") == std::string::npos) {
    FAIL("expected missing assign_target_kinds diagnostic for assignment without metadata");
    return;
  }

  PASS();
}

// ============================================================================
// Test: assignment with missing assign_target_shapes entry fails closed
// ============================================================================
static void test_assignment_missing_target_shape_fails_closed() {
  TEST(assignment_missing_target_shape_fails_closed);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
fn main() -> i64 {
    var x: i64;
    x = 1;
    x
}
  )",
                             program)) {
    FAIL("hew CLI unavailable; cannot run assign_target_shapes negative test");
    return;
  }

  auto *fn = findFunctionDecl(program, "main");
  if (!fn) {
    FAIL("failed to find main function for assign_target_shapes negative test");
    return;
  }

  auto targetSpan = findFunctionAssignTargetSpan(*fn);
  if (!targetSpan || !eraseAssignTargetShapeEntryForSpan(program, *targetSpan)) {
    FAIL("failed to remove assign_target_shapes entry for assignment target");
    return;
  }

  mlir::MLIRContext ctx;
  initContext(ctx);
  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected codegen to fail for assignment without target-shape metadata");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("missing assign_target_shapes entry for assignment") == std::string::npos) {
    FAIL("expected missing assign_target_shapes diagnostic for assignment without metadata");
    return;
  }

  PASS();
}

// ============================================================================
// Test: HashSet method call with missing lowering_facts entry fails closed
// ============================================================================
static void test_hashset_method_missing_lowering_fact_fails_closed() {
  TEST(hashset_method_missing_lowering_fact_fails_closed);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
fn main() -> i64 {
    let s: HashSet<i64> = HashSet::new();
    s.len()
}
  )",
                             program)) {
    FAIL("hew CLI unavailable; cannot run lowering_facts negative test");
    return;
  }

  auto *fn = findFunctionDecl(program, "main");
  if (!fn) {
    FAIL("failed to find main function for lowering_facts negative test");
    return;
  }

  auto methodCallSpan = findFunctionMethodCallSpan(*fn, "len");
  if (!methodCallSpan || !eraseLoweringFactEntryForSpan(program, *methodCallSpan)) {
    FAIL("failed to remove lowering_facts entry for HashSet::len call");
    return;
  }

  mlir::MLIRContext ctx;
  initContext(ctx);
  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected codegen to fail for HashSet::len without lowering-fact metadata");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("missing lowering_facts entry for HashSet::len") == std::string::npos) {
    FAIL("expected missing lowering_facts diagnostic for HashSet::len without metadata");
    return;
  }

  PASS();
}

// ============================================================================
// Test: unsigned division missing expr_types entry fails closed
// (covers the "binary signedness decision" path distinct from widening/comparison)
// ============================================================================
static void test_unsigned_division_missing_expr_type_fails_closed() {
  TEST(unsigned_division_missing_expr_type_fails_closed);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
fn broken_unsigned_div() -> u32 {
    let a: u32 = 10;
    let b: u32 = 3;
    a / b
}
  )",
                             program)) {
    FAIL("failed to load typed program for unsigned division expr_types test");
    return;
  }

  auto *fn = findFunctionDecl(program, "broken_unsigned_div");
  if (!fn || !fn->body.trailing_expr) {
    FAIL("broken_unsigned_div body missing trailing expression");
    return;
  }

  auto *binary = std::get_if<hew::ast::ExprBinary>(&fn->body.trailing_expr->value.kind);
  if (!binary) {
    FAIL("expected trailing binary expression in broken_unsigned_div");
    return;
  }

  if (!eraseExprTypeEntryForSpan(program, binary->left->span)) {
    FAIL("failed to remove expr_types entry for left operand of unsigned division");
    return;
  }

  mlir::MLIRContext ctx;
  initContext(ctx);
  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure when unsigned division signedness metadata is missing");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("missing expr_types entry for binary signedness decision for the left "
                      "operand") == std::string::npos) {
    FAIL("expected missing expr_types fail-closed diagnostic for unsigned division signedness");
    return;
  }

  PASS();
}

// ============================================================================

static void test_remote_actor_alias_ask_is_recognized() {
  TEST(remote_actor_alias_ask_is_recognized);

  mlir::MLIRContext ctx;
  initContext(ctx);

  auto module = generateMLIR(ctx, R"(
actor Stats {
    receive fn snapshot() -> int {
        10
    }
}

type RemoteStats = Stats;

fn main() -> int {
    let remote: RemoteStats = Node::lookup("stats");
    await remote.snapshot()
}
  )");

  if (!module) {
    FAIL("MLIR generation failed for aliased remote actor ask");
    return;
  }

  hew::Codegen codegen(ctx);
  hew::CodegenOptions opts;
  opts.debug_info = true;
  llvm::LLVMContext llvmContext;
  auto llvmModule = codegen.buildLLVMModule(module, opts, llvmContext);
  module.getOperation()->destroy();

  if (!llvmModule) {
    FAIL("LLVM lowering failed for aliased remote actor ask");
    return;
  }

  auto *mainFn = llvmModule->getFunction("main");
  if (!mainFn) {
    FAIL("main function not found in lowered LLVM module");
    return;
  }

  bool foundRemoteAsk = false;
  for (auto &block : *mainFn) {
    for (auto &inst : block) {
      auto *call = llvm::dyn_cast<llvm::CallBase>(&inst);
      auto *callee = call ? call->getCalledFunction() : nullptr;
      if (callee && callee->getName() == "hew_node_api_ask") {
        foundRemoteAsk = true;
        break;
      }
    }
    if (foundRemoteAsk)
      break;
  }

  if (!foundRemoteAsk) {
    FAIL("expected aliased remote actor ask to lower to hew_node_api_ask");
    return;
  }

  PASS();
}

// ============================================================================
// Test: aliased actor-returning call receivers lower through remote actor ask
// ============================================================================

static void test_remote_actor_alias_call_receiver_is_recognized() {
  TEST(remote_actor_alias_call_receiver_is_recognized);

  mlir::MLIRContext ctx;
  initContext(ctx);

  auto module = generateMLIR(ctx, R"(
actor Stats {
    receive fn snapshot() -> int {
        10
    }
}

type RemoteStats = Stats;

fn main() -> int {
    await ({
        let remote: RemoteStats = Node::lookup("stats");
        remote
    }).snapshot()
}
  )");

  if (!module) {
    FAIL("MLIR generation failed for aliased remote actor call receiver");
    return;
  }

  hew::Codegen codegen(ctx);
  hew::CodegenOptions opts;
  opts.debug_info = true;
  llvm::LLVMContext llvmContext;
  auto llvmModule = codegen.buildLLVMModule(module, opts, llvmContext);
  module.getOperation()->destroy();

  if (!llvmModule) {
    FAIL("LLVM lowering failed for aliased remote actor call receiver");
    return;
  }

  auto *mainFn = llvmModule->getFunction("main");
  if (!mainFn) {
    FAIL("main function not found in lowered LLVM module");
    return;
  }

  bool foundRemoteAsk = false;
  for (auto &block : *mainFn) {
    for (auto &inst : block) {
      auto *call = llvm::dyn_cast<llvm::CallBase>(&inst);
      auto *callee = call ? call->getCalledFunction() : nullptr;
      if (callee && callee->getName() == "hew_node_api_ask") {
        foundRemoteAsk = true;
        break;
      }
    }
    if (foundRemoteAsk)
      break;
  }

  if (!foundRemoteAsk) {
    FAIL("expected aliased remote actor call receiver to lower to hew_node_api_ask");
    return;
  }

  PASS();
}

// ============================================================================
// Test: aliased for-await receivers use resolvedTypeOf before handle fallback
// ============================================================================

static void test_for_await_receiver_alias_inferred_binding_uses_resolved_type() {
  TEST(for_await_receiver_alias_inferred_binding_uses_resolved_type);

  mlir::MLIRContext ctx;
  initContext(ctx);

  auto module = generateMLIR(ctx, R"(
import std::channel::channel;

type Inbox = channel.Receiver<String>;

fn inbox() -> Inbox {
    let pair = channel.new(1);
    let tx = pair.0;
    let rx = pair.1;
    tx.send("hello");
    tx.close();
    rx
}

fn main() -> int {
    let rx = inbox();
    for await msg in rx {
        return 1;
    }
    0
}
  )");

  if (!module) {
    FAIL("MLIR generation failed for aliased for-await receiver iterable");
    return;
  }

  auto mainFn = lookupFuncBySuffix(module, "main");
  if (!mainFn) {
    FAIL("main function not found for aliased for-await receiver iterable");
    module.getOperation()->destroy();
    return;
  }

  if (countRuntimeCallsByCallee(mainFn.getOperation(), "hew_channel_recv") != 1) {
    FAIL("expected aliased for-await receiver iterable to lower to hew_channel_recv");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

static void test_for_await_receiver_int_alias_uses_canonical_primitive_classification() {
  TEST(for_await_receiver_int_alias_uses_canonical_primitive_classification);

  mlir::MLIRContext ctx;
  initContext(ctx);

  auto module = generateMLIR(ctx, R"(
import std::channel::channel;

type Count = Int;
type Inbox = channel.Receiver<Count>;

fn inbox() -> Inbox {
    let pair = channel.new(1);
    let tx = pair.0;
    let rx = pair.1;
    tx.send(41);
    tx.close();
    rx
}

fn main() -> int {
    let rx = inbox();
    for await msg in rx {
        return msg;
    }
    0
}
  )");

  if (!module) {
    FAIL("MLIR generation failed for aliased int for-await receiver iterable");
    return;
  }

  auto mainFn = lookupFuncBySuffix(module, "main");
  if (!mainFn) {
    FAIL("main function not found for aliased int for-await receiver iterable");
    module.getOperation()->destroy();
    return;
  }

  if (countRuntimeCallsByCallee(mainFn.getOperation(), "hew_channel_recv_int") != 1) {
    FAIL("expected aliased int for-await receiver iterable to lower to hew_channel_recv_int");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
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
// Test: remote void actor asks consult hew_node_ask_take_last_error and panic
//       on failure — covering both the true-remote and local-delegation paths.
//
// hew_node_api_ask returns a non-null sentinel for true-remote void success
// but returns null for local-delegation void success (the PIDs of locally
// registered actors route through hew_actor_ask_by_id which returns null for
// void replies).  A raw null check is therefore not a reliable discriminant.
//
// hew_node_ask_take_last_error() bridges both paths: it is 0 on success
// (both remote sentinel and local void-null) and non-zero on any failure.
// Codegen must consult that slot immediately after the ask call.
// ============================================================================

static void test_remote_actor_void_ask_panics_on_failed_ask() {
  TEST(remote_actor_void_ask_panics_on_failed_ask);

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
    FAIL("MLIR generation failed for remote void actor ask error-slot test");
    return;
  }

  hew::Codegen codegen(ctx);
  hew::CodegenOptions opts;
  opts.debug_info = true;
  llvm::LLVMContext llvmContext;
  auto llvmModule = codegen.buildLLVMModule(module, opts, llvmContext);
  module.getOperation()->destroy();

  if (!llvmModule) {
    FAIL("LLVM lowering failed for remote void actor ask error-slot test");
    return;
  }

  auto *mainFn = llvmModule->getFunction("main");
  if (!mainFn) {
    FAIL("main function not found in lowered LLVM module");
    return;
  }

  // Find hew_node_api_ask and hew_panic; verify no free on the reply pointer.
  llvm::CallBase *askCall = nullptr;
  llvm::CallBase *panicCall = nullptr;
  llvm::CallBase *takeErrCall = nullptr;
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
      if (callee->getName() == "hew_node_ask_take_last_error")
        takeErrCall = call;
      if (callee->getName() == "hew_panic")
        panicCall = call;
      if (callee->getName() == "free" && call->arg_size() == 1 && askCall &&
          call->getArgOperand(0) == askCall)
        freesAskReply = true;
    }
  }

  if (!askCall) {
    FAIL("expected lowered remote void ask to call hew_node_api_ask");
    return;
  }
  if (!takeErrCall) {
    FAIL("remote void ask must call hew_node_ask_take_last_error (raw null check is wrong "
         "for local-delegation void success)");
    return;
  }
  if (!panicCall) {
    FAIL("remote void ask must call hew_panic on failure (fail-closed)");
    return;
  }
  if (freesAskReply) {
    FAIL("remote void ask must not free the reply pointer (non-owning sentinel for true-remote, "
         "null for local-delegation)");
    return;
  }

  // The error value must be compared against zero (ne) to detect failure.
  llvm::ICmpInst *errCheck = nullptr;
  for (llvm::User *user : takeErrCall->users()) {
    auto *icmp = llvm::dyn_cast<llvm::ICmpInst>(user);
    if (!icmp)
      continue;
    if (icmp->getPredicate() != llvm::CmpInst::ICMP_NE &&
        icmp->getPredicate() != llvm::CmpInst::ICMP_EQ)
      continue;
    bool comparesErrToZero = false;
    for (unsigned i = 0; i < 2; ++i) {
      if (icmp->getOperand(i) == takeErrCall) {
        auto *other = icmp->getOperand(1 - i);
        if (auto *ci = llvm::dyn_cast<llvm::ConstantInt>(other))
          if (ci->isZero())
            comparesErrToZero = true;
      }
    }
    if (comparesErrToZero) {
      errCheck = icmp;
      break;
    }
  }

  if (!errCheck) {
    FAIL("remote void ask must compare hew_node_ask_take_last_error result against zero");
    return;
  }

  // That comparison must drive a conditional branch that leads to hew_panic.
  llvm::BranchInst *errBranch =
      llvm::dyn_cast<llvm::BranchInst>(errCheck->getParent()->getTerminator());
  if (!errBranch || !errBranch->isConditional() || errBranch->getCondition() != errCheck) {
    FAIL("error check must be the condition of a conditional branch");
    return;
  }

  bool branchesToPanic = false;
  for (unsigned i = 0; i < errBranch->getNumSuccessors(); ++i) {
    for (auto &inst : *errBranch->getSuccessor(i)) {
      auto *call = llvm::dyn_cast<llvm::CallBase>(&inst);
      auto *callee = call ? call->getCalledFunction() : nullptr;
      if (callee && callee->getName() == "hew_panic") {
        branchesToPanic = true;
        break;
      }
    }
  }

  if (!branchesToPanic) {
    FAIL("remote void ask must branch to hew_panic when error slot is non-zero");
    return;
  }

  // Confirm no raw null-check against the ask return value is used as the
  // failure discriminant (that path is incorrect for local-delegation).
  for (auto &block : *mainFn) {
    for (auto &inst : block) {
      auto *icmp = llvm::dyn_cast<llvm::ICmpInst>(&inst);
      if (!icmp || icmp->getPredicate() != llvm::CmpInst::ICMP_EQ)
        continue;
      auto *lhs = icmp->getOperand(0);
      auto *rhs = icmp->getOperand(1);
      if ((lhs == askCall && llvm::isa<llvm::ConstantPointerNull>(rhs)) ||
          (rhs == askCall && llvm::isa<llvm::ConstantPointerNull>(lhs))) {
        FAIL("remote void ask must not use a raw null-check on the reply pointer as the "
             "failure discriminant (incorrect for local-delegation void success)");
        return;
      }
    }
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

// ============================================================================
// Test: break outside a loop increments errorCount_ and aborts codegen
// (regression for the fail-closed fix in MLIRGenStmt.cpp)
// ============================================================================
static void test_break_outside_loop_stmt_fails_closed() {
  TEST(break_outside_loop_stmt_fails_closed);

  using namespace hew::ast;
  const Span span{0, 0};

  auto mkType = [&](llvm::StringRef name) -> Spanned<TypeExpr> {
    TypeExpr ty;
    ty.kind = TypeNamed{name.str(), std::nullopt};
    return {std::move(ty), span};
  };

  // Build: fn main() -> i64 { break; }
  StmtBreak breakStmt;
  breakStmt.label = std::nullopt;
  breakStmt.value = std::nullopt;

  Stmt stmtNode;
  stmtNode.kind = std::move(breakStmt);
  stmtNode.span = span;

  FnDecl fn;
  fn.name = "main";
  fn.is_async = false;
  fn.is_generator = false;
  fn.visibility = Visibility::Pub;
  fn.is_pure = false;
  fn.return_type = mkType("i64");
  fn.body.stmts.push_back(
      std::make_unique<Spanned<Stmt>>(Spanned<Stmt>{std::move(stmtNode), span}));

  Program program;
  program.items.push_back({Item{std::move(fn)}, span});

  mlir::MLIRContext ctx;
  initContext(ctx);
  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation to fail for break outside a loop");
    module.getOperation()->destroy();
    return;
  }

  constexpr llvm::StringLiteral kDiag = "break used outside of a loop";
  if (stderrText.find(kDiag.str()) == std::string::npos) {
    FAIL("expected 'break used outside of a loop' diagnostic");
    return;
  }

  PASS();
}

// ============================================================================
// Test: match arm with unknown constructor pattern increments errorCount_ and
// aborts codegen (regression for the fail-closed fix in MLIRGenMatch.cpp)
// ============================================================================
static void test_match_arm_unknown_constructor_fails_closed() {
  TEST(match_arm_unknown_constructor_fails_closed);

  using namespace hew::ast;
  const Span span{0, 0};

  auto mkType = [&](llvm::StringRef name) -> Spanned<TypeExpr> {
    TypeExpr ty;
    ty.kind = TypeNamed{name.str(), std::nullopt};
    return {std::move(ty), span};
  };

  // Build: fn main(x: i64) -> i64 { match x { Foo => 0, } }
  // PatConstructor "Foo" has no entry in variantLookup for an i64 scrutinee,
  // so generateMatchArmCondition hits "unknown constructor pattern 'Foo'".
  PatConstructor ctor;
  ctor.name = "Foo";

  Pattern ctorPat;
  ctorPat.kind = std::move(ctor);

  Expr bodyExpr;
  bodyExpr.kind = ExprLiteral{Literal(LitInteger{0})};
  bodyExpr.span = span;

  MatchArm arm;
  arm.pattern = {std::move(ctorPat), span};
  arm.guard = nullptr;
  arm.body = std::make_unique<Spanned<Expr>>(Spanned<Expr>{std::move(bodyExpr), span});

  Expr scrutineeExpr;
  scrutineeExpr.kind = ExprIdentifier{"x"};
  scrutineeExpr.span = span;

  StmtMatch matchStmt;
  matchStmt.scrutinee = {std::move(scrutineeExpr), span};
  matchStmt.arms.push_back(std::move(arm));

  Stmt stmtNode;
  stmtNode.kind = std::move(matchStmt);
  stmtNode.span = span;

  Param param;
  param.name = "x";
  param.ty = mkType("i64");
  param.is_mutable = false;

  FnDecl fn;
  fn.name = "main";
  fn.is_async = false;
  fn.is_generator = false;
  fn.visibility = Visibility::Pub;
  fn.is_pure = false;
  fn.return_type = mkType("i64");
  fn.params.push_back(std::move(param));
  fn.body.stmts.push_back(
      std::make_unique<Spanned<Stmt>>(Spanned<Stmt>{std::move(stmtNode), span}));

  Program program;
  program.items.push_back({Item{std::move(fn)}, span});

  mlir::MLIRContext ctx;
  initContext(ctx);
  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation to fail for match arm with unknown constructor");
    module.getOperation()->destroy();
    return;
  }

  constexpr llvm::StringLiteral kDiag = "unknown constructor pattern 'Foo' in match arm";
  if (stderrText.find(kDiag.str()) == std::string::npos) {
    FAIL("expected 'unknown constructor pattern' diagnostic");
    return;
  }

  PASS();
}

// ============================================================================
// ============================================================================
// Helper: verify that a guard slot used for a user-drop has a non-null store
// that lives inside a nested SCF if region.  This proves that
// nullOutDropSlot's retroactive store was inserted inside the nested region
// (fixing the dominance/leak edge) rather than left null-initialized.
// ============================================================================
static bool hasDropGuardSlotPopulatedInNestedRegion(mlir::Operation *funcOp) {
  bool found = false;
  funcOp->walk([&](hew::DropOp drop) {
    if (found || !drop.getIsUserDrop())
      return;
    // Unwrap any hew.bitcast ops between the drop and the underlying load.
    mlir::Value dropVal = drop.getValue();
    while (auto *defOp = dropVal.getDefiningOp()) {
      if (defOp->getName().getStringRef() == "hew.bitcast" && defOp->getNumOperands() == 1)
        dropVal = defOp->getOperand(0);
      else
        break;
    }
    auto load = dropVal.getDefiningOp<mlir::memref::LoadOp>();
    if (!load)
      return;
    auto slot = load.getMemref();
    if (!slot.getDefiningOp<mlir::memref::AllocaOp>())
      return;
    // Look for a non-null store to this slot inside a nested SCF if region.
    funcOp->walk([&](mlir::memref::StoreOp store) {
      if (found || store.getMemref() != slot)
        return;
      if (store.getValue().getDefiningOp<mlir::LLVM::ZeroOp>())
        return;
      if (isZeroLiteralValue(store.getValue()))
        return;
      auto *parentRegion = store->getParentRegion();
      if (parentRegion && mlir::isa<mlir::scf::IfOp>(parentRegion->getParentOp()))
        found = true;
    });
  });
  return found;
}

// ============================================================================
// Test: nullOutDropSlot guard slot is populated when handle defOp is inside a
// nested SCF region (regression for the dominance/leak edge).
//
// Before the fix, nullOutDropSlot skipped the retroactive guard-slot store
// when the handle-defining op lived inside a nested SCF if region.  The slot
// stayed null-initialized, so the scope-exit drop was always skipped → LEAK.
// After the fix, the store is inserted inside the nested region so the slot
// holds the live handle value when .free() is NOT called.
// ============================================================================
static void test_json_nested_scope_free_guard_slot_populated() {
  TEST(json_nested_scope_free_guard_slot_populated);

  mlir::MLIRContext ctx;
  initContext(ctx);

  auto module = generateMLIR(ctx, R"(
import std::encoding::json;

fn test(do_free: bool) -> i32 {
    if true {
        let val = json.parse("{\"n\": 1}");
        let t = val.type_of();
        if do_free {
            val.free();
        }
        t
    } else {
        0
    }
}

fn main() {
    println(test(true));
    println(test(false));
}
  )");

  if (!module) {
    FAIL("MLIR generation failed for json nested-scope free guard test");
    return;
  }

  auto testFn = lookupFuncBySuffix(module, "test");
  if (!testFn) {
    FAIL("test function not found");
    module.getOperation()->destroy();
    return;
  }

  // The scope-exit drop for val must be present (null-guarded hew.drop).
  auto dropWrapper = lookupFuncBySuffix(module, "json.ValueF4drop");
  if (!dropWrapper) {
    FAIL("expected a generated json.Value drop wrapper");
    module.getOperation()->destroy();
    return;
  }

  if (countDropOpsByDropFn(testFn, dropWrapper.getName(), true) < 1) {
    FAIL("expected scope-exit hew.drop for json.Value inside nested if block");
    module.getOperation()->destroy();
    return;
  }

  // The explicit .free() must still emit exactly one hew_json_free call.
  if (countCallsByCallee(testFn, "hew_json_free") != 1) {
    FAIL("expected exactly one hew_json_free call for the explicit .free()");
    module.getOperation()->destroy();
    return;
  }

  // The guard slot must have been populated with the live handle value inside
  // the nested SCF if region (the retroactive store fix).  Without the fix,
  // only the null-out store (from the .free() site) would be present.
  if (!hasDropGuardSlotPopulatedInNestedRegion(testFn)) {
    FAIL("guard slot for json.Value was not populated inside the nested SCF if "
         "region — the scope-exit drop will always be skipped (ASAN leak)");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// if-let / while-let pattern contract tests
//
// Wildcard and Identifier patterns at the top level of if-let / while-let
// must lower without errors.  Unsupported patterns (Struct, Tuple, etc.) that
// bypass the checker must increment errorCount_ and produce no module.
// ============================================================================

static void test_iflet_stmt_wildcard_pattern_lowers() {
  TEST(iflet_stmt_wildcard_pattern_lowers);

  mlir::MLIRContext ctx;
  initContext(ctx);

  auto module = generateMLIR(ctx, R"(
fn always_run(x: int) -> int {
    var result: int = 0;
    if let _ = x {
        result = 1;
    }
    result
}
  )");

  if (!module) {
    FAIL("MLIR generation failed for wildcard if-let statement");
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

static void test_iflet_stmt_identifier_pattern_lowers() {
  TEST(iflet_stmt_identifier_pattern_lowers);

  mlir::MLIRContext ctx;
  initContext(ctx);

  auto module = generateMLIR(ctx, R"(
fn use_identifier(x: int) -> int {
    var result: int = 0;
    if let y = x {
        result = y;
    }
    result
}
  )");

  if (!module) {
    FAIL("MLIR generation failed for identifier if-let statement");
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

static void test_iflet_expr_wildcard_pattern_lowers() {
  TEST(iflet_expr_wildcard_pattern_lowers);

  mlir::MLIRContext ctx;
  initContext(ctx);

  auto module = generateMLIR(ctx, R"(
fn always_one(x: int) -> int {
    return if let _ = x { 1 } else { 0 };
}
  )");

  if (!module) {
    FAIL("MLIR generation failed for wildcard if-let expression");
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

static void test_whilelet_stmt_wildcard_pattern_lowers() {
  TEST(whilelet_stmt_wildcard_pattern_lowers);

  mlir::MLIRContext ctx;
  initContext(ctx);

  // while let _ = expr always continues; break prevents infinite loop.
  auto module = generateMLIR(ctx, R"(
fn run_once(x: int) -> int {
    var count: int = 0;
    while let _ = x {
        count = count + 1;
        break;
    }
    count
}
  )");

  if (!module) {
    FAIL("MLIR generation failed for wildcard while-let statement");
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

static void test_whilelet_stmt_identifier_pattern_lowers() {
  TEST(whilelet_stmt_identifier_pattern_lowers);

  mlir::MLIRContext ctx;
  initContext(ctx);

  auto module = generateMLIR(ctx, R"(
fn capture_once(x: int) -> int {
    var result: int = 0;
    while let v = x {
        result = v;
        break;
    }
    result
}
  )");

  if (!module) {
    FAIL("MLIR generation failed for identifier while-let statement");
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// Fail-closed: inject an unsupported PatTuple into an if-let statement's
// pattern after successful type-checking, then verify that codegen increments
// errorCount_ and produces no module.
static void test_iflet_stmt_unsupported_pattern_fails_closed() {
  TEST(iflet_stmt_unsupported_pattern_fails_closed);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
enum Wrap { Val(int); }
fn test(w: Wrap) -> int {
    if let Val(x) = w { x } else { 0 }
}
  )",
                             program)) {
    FAIL("failed to load typed program for unsupported-pattern fail-closed test");
    return;
  }

  auto *fn = findFunctionDecl(program, "test");
  if (!fn) {
    FAIL("test function not found");
    return;
  }

  auto *ifLetStmt = findFirstIfLetStmt(*fn);
  if (!ifLetStmt) {
    FAIL("if-let statement not found");
    return;
  }

  // Replace the Constructor pattern with an unsupported PatTuple.
  ifLetStmt->pattern.value.kind = hew::ast::PatTuple{};

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure for unsupported pattern in if-let");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("only supports constructor") == std::string::npos) {
    FAIL(("expected unsupported-pattern diagnostic for if-let; got: " + stderrText).c_str());
    return;
  }

  PASS();
}

// Fail-closed: inject an unsupported PatTuple into a while-let statement's
// pattern after successful type-checking.
static void test_whilelet_stmt_unsupported_pattern_fails_closed() {
  TEST(whilelet_stmt_unsupported_pattern_fails_closed);

  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
enum Wrap { Val(int); }
fn length(w: Wrap) -> int {
    var n: int = 0;
    while let Val(x) = w {
        n = n + x;
        break;
    }
    n
}
  )",
                             program)) {
    FAIL("failed to load typed program for while-let unsupported-pattern fail-closed test");
    return;
  }

  auto *fn = findFunctionDecl(program, "length");
  if (!fn) {
    FAIL("length function not found");
    return;
  }

  auto *whileLetStmt = findFirstWhileLetStmt(*fn);
  if (!whileLetStmt) {
    FAIL("while-let statement not found");
    return;
  }

  // Replace the Constructor pattern with an unsupported PatTuple.
  whileLetStmt->pattern.value.kind = hew::ast::PatTuple{};

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure for unsupported pattern in while-let");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("only supports constructor") == std::string::npos) {
    FAIL(("expected unsupported-pattern diagnostic for while-let; got: " + stderrText).c_str());
    return;
  }

  PASS();
}

// ============================================================================
// Test: `let _ = expr` wildcard let materializes droppable temporaries
// (regression for the drop-leak fix: PatWildcard in generateLetStmt must call
// materializeTemporary so heap-allocated returns are freed at scope exit)
// ============================================================================
static void test_wildcard_let_droppable_temporary_is_materialized() {
  TEST(wildcard_let_droppable_temporary_is_materialized);

  mlir::MLIRContext ctx;
  initContext(ctx);

  // int_to_string returns an owned String (hew_string_drop).
  // Before the fix, `let _ = int_to_string(n)` leaked the string because the
  // PatWildcard branch in generateLetStmt did not call materializeTemporary.
  auto module = generateMLIR(ctx, R"(
fn wildcard_let_string(n: int) -> int {
    let _ = int_to_string(n);
    0
}

fn main() -> int {
    wildcard_let_string(42)
}
  )");

  if (!module) {
    FAIL("MLIR generation failed for wildcard let droppable temporary");
    return;
  }

  auto fn = lookupFuncBySuffix(module, "wildcard_let_string");
  if (!fn) {
    FAIL("wildcard_let_string function not found");
    module.getOperation()->destroy();
    return;
  }

  if (countDropOpsByDropFn(fn, "hew_string_drop", false) < 1) {
    FAIL("wildcard let of String temporary should emit a hew_string_drop via materializeTemporary");
    module.getOperation()->destroy();
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Test: StmtIfLet as last statement in a function body yielding a field of an
//       owned (callee-dropped) parameter is rejected fail-closed.
//       (regression: blockValueYieldsFieldOfDroppedParam must walk StmtIfLet
//       last-statement, not only ExprIfLet in trailing_expr position)
// ============================================================================
static void test_param_drop_stmt_if_let_value_position_fails_closed() {
  TEST(param_drop_stmt_if_let_value_position_fails_closed);

  // Parse a valid program with a user-drop type and a function that takes
  // an owned param of that type. The parsed program populates drop_funcs so
  // `w` ends up in droppedParamNames during codegen.
  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
type Wrapper {
    name: String;
}

impl Drop for Wrapper {
    fn drop(w: Wrapper) {
        println(w.name);
    }
}

fn extract_name(w: Wrapper) -> int {
    0
}
  )",
                             program)) {
    FAIL("failed to load typed program for stmt-if-let value-position fail-closed test");
    return;
  }

  auto *fn = findFunctionDecl(program, "extract_name");
  if (!fn) {
    FAIL("extract_name function not found in parsed program");
    return;
  }

  using namespace hew::ast;
  const Span span{0, 0};

  // Helpers that parallel the builder lambdas used by other AST-construction tests.
  auto mkIdentExpr = [&](llvm::StringRef name) -> Spanned<Expr> {
    Expr e;
    e.kind = ExprIdentifier{name.str()};
    e.span = span;
    return {std::move(e), span};
  };
  auto mkFieldAccessExpr = [&](llvm::StringRef obj, llvm::StringRef field) -> Spanned<Expr> {
    Expr e;
    e.kind = ExprFieldAccess{std::make_unique<Spanned<Expr>>(mkIdentExpr(obj)), field.str()};
    e.span = span;
    return {std::move(e), span};
  };

  // Build: StmtIfLet with wildcard pattern, scrutinee = w, body yields w.name.
  // No trailing_expr — the StmtIfLet is the last statement of the function body.
  Pattern pat;
  pat.kind = PatWildcard{};

  StmtIfLet ifLetStmt;
  ifLetStmt.pattern = {std::move(pat), span};
  ifLetStmt.expr = std::make_unique<Spanned<Expr>>(mkIdentExpr("w"));
  ifLetStmt.body.trailing_expr = std::make_unique<Spanned<Expr>>(mkFieldAccessExpr("w", "name"));
  ifLetStmt.else_body = std::nullopt;

  Stmt stmtNode;
  stmtNode.kind = std::move(ifLetStmt);
  stmtNode.span = span;

  // Replace the function body with just the StmtIfLet (no trailing_expr).
  fn->body.stmts.clear();
  fn->body.trailing_expr = nullptr;
  fn->body.stmts.push_back(
      std::make_unique<Spanned<Stmt>>(Spanned<Stmt>{std::move(stmtNode), span}));

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    module.getOperation()->destroy();
    FAIL("expected MLIR generation to reject StmtIfLet body that yields a field of an owned param");
    return;
  }

  constexpr llvm::StringLiteral kDiag = "returning a field of an owned parameter";
  if (stderrText.find(kDiag.str()) == std::string::npos) {
    FAIL(("expected 'returning a field of an owned parameter' diagnostic; got: " + stderrText)
             .c_str());
    return;
  }

  PASS();
}

// ============================================================================
// Test: A labeled inner loop that shadows the outer loop's label does NOT
//       cause the outer loop's break-value scan to falsely attribute the inner
//       break to the outer loop.
//       (regression: blockBreakValueYieldsFieldOfDroppedParam must pass
//       nullopt as targetLabel when descending into a same-name inner loop)
// ============================================================================
static void test_param_drop_shadow_label_inner_break_not_attributed() {
  TEST(param_drop_shadow_label_inner_break_not_attributed);

  // Parse a valid program with a user-drop type. We'll mutate the body to
  // inject nested loops with the same label.
  hew::ast::Program program;
  if (!loadProgramFromSource(R"(
type Wrapper {
    name: String;
}

impl Drop for Wrapper {
    fn drop(w: Wrapper) {
        println(w.name);
    }
}

fn extract_name(w: Wrapper) -> int {
    0
}
  )",
                             program)) {
    FAIL("failed to load typed program for shadow-label regression test");
    return;
  }

  auto *fn = findFunctionDecl(program, "extract_name");
  if (!fn) {
    FAIL("extract_name function not found in parsed program");
    return;
  }

  using namespace hew::ast;
  const Span span{0, 0};

  auto mkIdentExpr = [&](llvm::StringRef name) -> Spanned<Expr> {
    Expr e;
    e.kind = ExprIdentifier{name.str()};
    e.span = span;
    return {std::move(e), span};
  };
  auto mkFieldAccessExpr = [&](llvm::StringRef obj, llvm::StringRef field) -> Spanned<Expr> {
    Expr e;
    e.kind = ExprFieldAccess{std::make_unique<Spanned<Expr>>(mkIdentExpr(obj)), field.str()};
    e.span = span;
    return {std::move(e), span};
  };
  auto mkIntExpr = [&](int64_t val) -> Spanned<Expr> {
    Expr e;
    e.kind = ExprLiteral{LitInteger{val}};
    e.span = span;
    return {std::move(e), span};
  };
  auto mkStmt = [&](auto node) -> std::unique_ptr<Spanned<Stmt>> {
    Stmt s;
    s.kind = std::move(node);
    s.span = span;
    return std::make_unique<Spanned<Stmt>>(Spanned<Stmt>{std::move(s), span});
  };

  // Build the inner loop (@outer: loop { break @outer w.name; })
  // This break carries a field access but targets the *inner* @outer, not outer.
  StmtBreak innerBreak;
  innerBreak.label = "outer";
  innerBreak.value = mkFieldAccessExpr("w", "name");

  Block innerBody;
  innerBody.stmts.push_back(mkStmt(std::move(innerBreak)));

  StmtLoop innerLoop;
  innerLoop.label = "outer"; // same label as outer — shadows it
  innerLoop.body = std::move(innerBody);

  // Build the outer loop's own break (@outer: break @outer 0) — safe value
  StmtBreak outerBreak;
  outerBreak.label = "outer";
  outerBreak.value = mkIntExpr(0);

  // Assemble the outer loop body: [inner loop stmt, outer break stmt]
  Block outerBody;
  outerBody.stmts.push_back(mkStmt(std::move(innerLoop)));
  outerBody.stmts.push_back(mkStmt(std::move(outerBreak)));

  StmtLoop outerLoop;
  outerLoop.label = "outer";
  outerLoop.body = std::move(outerBody);

  // Replace the function body with just the outer loop (no trailing_expr).
  fn->body.stmts.clear();
  fn->body.trailing_expr = nullptr;
  fn->body.stmts.push_back(mkStmt(std::move(outerLoop)));

  mlir::MLIRContext ctx;
  initContext(ctx);

  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  // The outer loop's break value is safe (integer 0).
  // The inner loop reuses the label "outer" and its break carries w.name, but
  // that targets the inner loop, not the outer one.
  // With the shadow fix, MLIRGen must NOT reject this function.
  if (!module) {
    FAIL(("expected MLIR generation to succeed (inner break should not be attributed to outer "
          "loop); stderr: " +
          stderrText)
             .c_str());
    return;
  }

  module.getOperation()->destroy();
  PASS();
}

// ============================================================================
// Satellite fail-closed regressions: MLIRGenActor.cpp
// ============================================================================

// Test: spawn targeting an unregistered actor name increments errorCount_ and
// aborts codegen.  Exercises the `unknown actor type` guard added to
// generateSpawnExpr in MLIRGenActor.cpp.
static void test_spawn_unknown_actor_type_fails_closed() {
  TEST(spawn_unknown_actor_type_fails_closed);

  using namespace hew::ast;
  const Span span{0, 0};

  auto mkType = [&](llvm::StringRef name) -> Spanned<TypeExpr> {
    TypeExpr ty;
    ty.kind = TypeNamed{name.str(), std::nullopt};
    return {std::move(ty), span};
  };

  // Build: fn main() -> int { spawn Ghost(); }
  // "Ghost" has no ActorDecl in the program, so it is not in actorRegistry.
  Expr targetExpr;
  targetExpr.kind = ExprIdentifier{"Ghost"};
  targetExpr.span = span;

  ExprSpawn spawn;
  spawn.target = std::make_unique<Spanned<Expr>>(Spanned<Expr>{std::move(targetExpr), span});

  Expr spawnExpr;
  spawnExpr.kind = std::move(spawn);
  spawnExpr.span = span;

  StmtExpression stmtExpr;
  stmtExpr.expr = Spanned<Expr>{std::move(spawnExpr), span};

  Stmt stmt;
  stmt.kind = std::move(stmtExpr);
  stmt.span = span;

  FnDecl fn;
  fn.name = "main";
  fn.is_async = false;
  fn.is_generator = false;
  fn.visibility = Visibility::Pub;
  fn.is_pure = false;
  fn.return_type = mkType("int");
  fn.body.stmts.push_back(std::make_unique<Spanned<Stmt>>(Spanned<Stmt>{std::move(stmt), span}));

  Program program;
  program.items.push_back({Item{std::move(fn)}, span});

  mlir::MLIRContext ctx;
  initContext(ctx);
  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation to fail for spawn of unknown actor type");
    module.getOperation()->destroy();
    return;
  }

  constexpr llvm::StringLiteral kDiag = "unknown actor type: Ghost";
  if (stderrText.find(kDiag.str()) == std::string::npos) {
    FAIL(("expected 'unknown actor type' diagnostic; got: " + stderrText).c_str());
    return;
  }

  PASS();
}

// ============================================================================
// Satellite fail-closed regressions: MLIRGenSupervisor.cpp
// ============================================================================

// Test: supervisor with an unparseable window string increments errorCount_ and
// aborts codegen.  Exercises the `invalid supervisor window value` guard added
// to generateSupervisorDecl in MLIRGenSupervisor.cpp.
static void test_supervisor_invalid_window_fails_closed() {
  TEST(supervisor_invalid_window_fails_closed);

  using namespace hew::ast;
  const Span span{0, 0};

  SupervisorDecl sup;
  sup.name = "MySupervisor";
  sup.strategy = SupervisorStrategy::OneForOne;
  sup.window = "not_a_number";

  Program program;
  program.items.push_back({Item{std::move(sup)}, span});

  mlir::MLIRContext ctx;
  initContext(ctx);
  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation to fail for supervisor with invalid window value");
    module.getOperation()->destroy();
    return;
  }

  constexpr llvm::StringLiteral kDiag = "invalid supervisor window value";
  if (stderrText.find(kDiag.str()) == std::string::npos) {
    FAIL(("expected 'invalid supervisor window value' diagnostic; got: " + stderrText).c_str());
    return;
  }

  PASS();
}

// ============================================================================
// Satellite fail-closed regressions: MLIRGenIfLet.cpp
// ============================================================================

// Test: if-let statement whose constructor is not registered in variantLookup
// increments errorCount_ and aborts codegen.  Exercises the
// `unknown constructor in if-let pattern` guard in MLIRGenIfLet.cpp.
static void test_iflet_stmt_unknown_constructor_fails_closed() {
  TEST(iflet_stmt_unknown_constructor_fails_closed);

  using namespace hew::ast;
  const Span span{0, 0};

  auto mkType = [&](llvm::StringRef name) -> Spanned<TypeExpr> {
    TypeExpr ty;
    ty.kind = TypeNamed{name.str(), std::nullopt};
    return {std::move(ty), span};
  };

  // Build: fn main() -> int { if let Phantom = 0 { } else { } }
  // "Phantom" has no entry in variantLookup (only the builtins None/Some/Ok/Err
  // are pre-seeded), so generateIfLetStmt hits
  // "unknown constructor '...' in if-let pattern".
  Expr scrutineeExpr;
  scrutineeExpr.kind = ExprLiteral{Literal(LitInteger{0})};
  scrutineeExpr.span = span;

  PatConstructor ctor;
  ctor.name = "Phantom";
  Pattern pat;
  pat.kind = std::move(ctor);

  StmtIfLet ifLetStmt;
  ifLetStmt.pattern = {std::move(pat), span};
  ifLetStmt.expr = std::make_unique<Spanned<Expr>>(Spanned<Expr>{std::move(scrutineeExpr), span});

  Stmt stmt;
  stmt.kind = std::move(ifLetStmt);
  stmt.span = span;

  FnDecl fn;
  fn.name = "main";
  fn.is_async = false;
  fn.is_generator = false;
  fn.visibility = Visibility::Pub;
  fn.is_pure = false;
  fn.return_type = mkType("int");
  fn.body.stmts.push_back(std::make_unique<Spanned<Stmt>>(Spanned<Stmt>{std::move(stmt), span}));

  Program program;
  program.items.push_back({Item{std::move(fn)}, span});

  mlir::MLIRContext ctx;
  initContext(ctx);
  hew::MLIRGen mlirGen(ctx);
  mlir::ModuleOp module;
  auto stderrText = captureStderr([&] { module = mlirGen.generate(program); });

  if (module) {
    FAIL("expected MLIR generation failure for unknown constructor in if-let");
    module.getOperation()->destroy();
    return;
  }

  if (stderrText.find("unknown constructor") == std::string::npos) {
    FAIL(("expected 'unknown constructor' diagnostic for if-let; got: " + stderrText).c_str());
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
  test_discarded_scope_expr_bad_tail_fails_closed();
  test_discarded_scope_tail_if_bad_condition_fails_closed();
  test_discarded_scope_tail_if_bad_branch_fails_closed();
  test_discarded_scope_tail_if_side_effect_branches_lower();
  test_statement_style_match_arm_bad_body_fails_closed();
  test_statement_style_match_arm_block_tail_if_fails_closed();
  test_assignment_missing_target_kind_fails_closed();
  test_assignment_missing_target_shape_fails_closed();
  test_hashset_method_missing_lowering_fact_fails_closed();
  test_unsigned_division_missing_expr_type_fails_closed();
  test_statement_style_match_arm_block_tail_match_fails_closed();
  test_statement_style_match_arm_statement_only_if_tail_lowers();
  test_statement_style_match_arm_statement_only_match_tail_lowers();
  test_discarded_scope_expr_tail_if_no_extra_results();
  test_nested_discarded_scope_expr_tail_if_no_extra_results();
  test_discarded_scope_wrapper_tail_if_no_extra_results();
  test_discarded_scope_if_expr_no_extra_results();
  test_discarded_scope_expr_tail_match_no_extra_results();
  test_nested_discarded_scope_expr_tail_match_no_extra_results();
  test_if_expr_branch_temporaries_drop();
  test_discarded_if_expr_branch_temporaries_drop();
  test_if_stmt_branch_temporaries_drop();
  test_collection_builtin_hint_does_not_leak_to_sibling_literals();
  test_declared_collection_hints_lower_array_and_empty_hashmap_literals();
  test_nested_vec_new_does_not_capture_outer_array_hint();
  test_direct_constructor_type_hints_lower_builtins();
  test_nested_none_does_not_inherit_outer_constructor_hints();
  test_none_without_type_context_fails_closed();
  test_match_arm_direct_none_uses_match_result_type_hint();
  test_discarded_if_expr_user_drop_branch_temp_zero_init();
  test_stmt_if_user_drop_initbit_guard();
  test_arithmetic();
  test_comparisons();
  test_materialized_unsigned_range_uses_unsigned_compare();
  test_direct_unsigned_range_missing_expr_type_fails_closed();
  test_direct_unsigned_range_missing_upper_expr_type_fails_closed();
  test_materialized_unsigned_range_missing_expr_type_fails_closed();
  test_indirect_enum_match_missing_scrutinee_expr_type_fails_closed();
  test_indirect_enum_match_expr_missing_scrutinee_expr_type_fails_closed();
  test_indirect_enum_iflet_stmt_missing_scrutinee_expr_type_fails_closed();
  test_indirect_enum_iflet_expr_missing_scrutinee_expr_type_fails_closed();
  test_indirect_enum_whilelet_missing_scrutinee_expr_type_fails_closed();
  test_option_result_match_expr_missing_result_type_fails_closed();
  test_unsigned_binary_ops_use_unsigned_lowering();
  test_unsigned_binary_expr_missing_expr_type_fails_closed();
  test_println_int_missing_expr_type_fails_closed();
  test_int_to_string_missing_expr_type_fails_closed();
  test_scope_await_inline_launch_uses_resolved_task_type();
  test_scope_await_inline_launch_missing_expr_type_fails_closed();
  test_for_await_receiver_missing_elem_type_fails_closed();
  test_for_await_bytes_stream_binding_fallback_uses_bytes_abi();
  test_for_await_bytes_stream_binding_fallback_overrides_conflicting_expr_type();
  test_for_await_bytes_stream_inline_filter_fallback_uses_bytes_abi();
  test_for_await_bytes_stream_known_call_name_abi_no_expr_types();
  test_function_signature_type_infer_fails_closed();
  test_return_stmt();
  test_logical_ops();
  test_unary_ops();
  test_compound_assignment();
  test_identifier_local_assignment_kind_mismatch_fails_closed();
  test_identifier_actor_field_assignment_kind_mismatch_fails_closed();
  test_missing_struct_field_assignment_fails_closed();
  test_nonstruct_field_assignment_fails_closed();
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
  test_wire_struct_typedecl_missing_field_metadata_rejects();
  test_wire_enum_mixed_payload_match_positions();
  test_wire_enum_unit_serial_helpers_and_dispatch();
  test_unresolved_generic_substitution_type_fails();
  test_unsupported_return_coercion_stops_before_verifier();
  test_select_emits_send_failure_cleanup();
  test_select_computed_timeout_evaluates_before_arming_asks();
  test_join_emits_send_failure_cleanup();
  test_scoped_spawn_panics_on_scope_overflow();
  test_generator_wrapped_yield_drop_exclusion();
  test_actor_receive_http_request_drop();
  test_actor_receive_http_server_drop();
  test_actor_receive_regex_pattern_drop();
  test_lambda_actor_receive_string_param_drop();
  test_lambda_actor_receive_clears_enclosing_drop_excludes();
  test_imported_json_value_scope_drop_uses_metadata();
  test_resolved_type_classifier_canonicalizes_aliases_and_qualified_receivers();
  test_handle_registry_uses_metadata_not_hardcoded_list();
  test_local_actor_non_void_ask_panics_on_null_reply_before_load();
  test_local_actor_void_ask_panics_on_failed_ask();
  test_handle_alias_call_receiver_is_recognized();
  test_duration_method_dispatch_uses_resolved_type();
  test_duration_method_dispatch_requires_resolved_type();
  test_rc_method_dispatch_uses_resolved_type();
  test_rc_method_dispatch_requires_resolved_type();
  test_handle_dispatch_uses_receiver_kind_metadata();
  test_handle_dispatch_requires_receiver_kind();
  test_actor_dispatch_requires_resolved_type();
  test_trait_dispatch_requires_receiver_kind();
  test_named_type_dispatch_requires_receiver_kind();
  test_generic_handle_impl_dispatch_requires_receiver_kind();
  test_remote_actor_alias_ask_is_recognized();
  test_remote_actor_alias_call_receiver_is_recognized();
  test_for_await_receiver_alias_inferred_binding_uses_resolved_type();
  test_for_await_receiver_int_alias_uses_canonical_primitive_classification();
  test_remote_actor_void_ask_does_not_free_reply();
  test_remote_actor_void_ask_panics_on_failed_ask();
  test_remote_actor_ask_passes_reply_size();
  test_remote_actor_ask_panics_on_null_reply();
  test_generic_struct_constructor_in_nongeneric_context();
  test_generic_struct_constructor_monomorphic_helper();
  test_prim_struct_no_serial_call_emits_no_wrappers();
  test_prim_struct_instance_serial_call_emits_demanded_wrapper();
  test_prim_struct_static_serial_call_emits_demanded_wrapper();
  test_break_outside_loop_stmt_fails_closed();
  test_match_arm_unknown_constructor_fails_closed();
  test_json_nested_scope_free_guard_slot_populated();
  test_wildcard_let_droppable_temporary_is_materialized();
  test_iflet_stmt_wildcard_pattern_lowers();
  test_iflet_stmt_identifier_pattern_lowers();
  test_iflet_expr_wildcard_pattern_lowers();
  test_whilelet_stmt_wildcard_pattern_lowers();
  test_whilelet_stmt_identifier_pattern_lowers();
  test_iflet_stmt_unsupported_pattern_fails_closed();
  test_whilelet_stmt_unsupported_pattern_fails_closed();
  test_param_drop_stmt_if_let_value_position_fails_closed();
  test_param_drop_shadow_label_inner_break_not_attributed();
  test_spawn_unknown_actor_type_fails_closed();
  test_supervisor_invalid_window_fails_closed();
  test_iflet_stmt_unknown_constructor_fails_closed();

  printf("\n%d/%d tests passed.\n", tests_passed, tests_run);
  return (tests_passed == tests_run) ? 0 : 1;
}
