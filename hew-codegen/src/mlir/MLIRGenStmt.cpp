//===- MLIRGenStmt.cpp - Statement codegen for Hew MLIRGen ----------------===//
//
// Statement generation methods: let, var, assign, if, while, for, return,
// expression statements, block generation, return guards, loop/break/continue.
//
//===----------------------------------------------------------------------===//

#include "hew/ast_helpers.h"
#include "hew/mlir/HewDialect.h"
#include "hew/mlir/HewOps.h"
#include "hew/mlir/HewTypes.h"
#include "hew/mlir/MLIRGen.h"
#include "MLIRGenHelpers.h"

#include "mlir/Dialect/Arith/IR/Arith.h"
#include "mlir/Dialect/Func/IR/FuncOps.h"
#include "mlir/Dialect/LLVMIR/LLVMDialect.h"
#include "mlir/Dialect/MemRef/IR/MemRef.h"
#include "mlir/Dialect/SCF/IR/SCF.h"
#include "mlir/IR/Builders.h"
#include "mlir/IR/BuiltinAttributes.h"
#include "mlir/IR/BuiltinOps.h"
#include "mlir/IR/BuiltinTypes.h"
#include "mlir/IR/Diagnostics.h"
#include "mlir/IR/Location.h"
#include "mlir/IR/Value.h"

#include "llvm/ADT/ScopedHashTable.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

#include <cassert>
#include <cstdlib>
#include <set>
#include <string>

using namespace hew;
using namespace mlir;

namespace {

bool isInferredType(const ast::Spanned<ast::TypeExpr> &typeExpr) {
  return typeExpr.span.start == 0 && typeExpr.span.end == 0;
}

bool usesTimeoutPlaceholder(const std::optional<ast::Spanned<ast::Expr>> &expr) {
  return expr && std::holds_alternative<ast::ExprTimeout>(expr->value.kind);
}

} // namespace

// ============================================================================
// Shared helpers
// ============================================================================

mlir::Value MLIRGen::emitCompoundArithOp(ast::CompoundAssignOp op, mlir::Value lhs, mlir::Value rhs,
                                         bool isFloat, bool isUnsigned, mlir::Location location) {
  switch (op) {
  case ast::CompoundAssignOp::Add:
    return isFloat ? (mlir::Value)mlir::arith::AddFOp::create(builder, location, lhs, rhs)
                   : (mlir::Value)mlir::arith::AddIOp::create(builder, location, lhs, rhs);
  case ast::CompoundAssignOp::Subtract:
    return isFloat ? (mlir::Value)mlir::arith::SubFOp::create(builder, location, lhs, rhs)
                   : (mlir::Value)mlir::arith::SubIOp::create(builder, location, lhs, rhs);
  case ast::CompoundAssignOp::Multiply:
    return isFloat ? (mlir::Value)mlir::arith::MulFOp::create(builder, location, lhs, rhs)
                   : (mlir::Value)mlir::arith::MulIOp::create(builder, location, lhs, rhs);
  case ast::CompoundAssignOp::Divide:
    return isFloat      ? (mlir::Value)mlir::arith::DivFOp::create(builder, location, lhs, rhs)
           : isUnsigned ? (mlir::Value)mlir::arith::DivUIOp::create(builder, location, lhs, rhs)
                        : (mlir::Value)mlir::arith::DivSIOp::create(builder, location, lhs, rhs);
  case ast::CompoundAssignOp::Modulo:
    return isFloat      ? (mlir::Value)mlir::arith::RemFOp::create(builder, location, lhs, rhs)
           : isUnsigned ? (mlir::Value)mlir::arith::RemUIOp::create(builder, location, lhs, rhs)
                        : (mlir::Value)mlir::arith::RemSIOp::create(builder, location, lhs, rhs);
  case ast::CompoundAssignOp::BitAnd:
    return mlir::arith::AndIOp::create(builder, location, lhs, rhs);
  case ast::CompoundAssignOp::BitOr:
    return mlir::arith::OrIOp::create(builder, location, lhs, rhs);
  case ast::CompoundAssignOp::BitXor:
    return mlir::arith::XOrIOp::create(builder, location, lhs, rhs);
  case ast::CompoundAssignOp::Shl:
    return mlir::arith::ShLIOp::create(builder, location, lhs, rhs);
  case ast::CompoundAssignOp::Shr:
    return isUnsigned ? (mlir::Value)mlir::arith::ShRUIOp::create(builder, location, lhs, rhs)
                      : (mlir::Value)mlir::arith::ShRSIOp::create(builder, location, lhs, rhs);
  default:
    ++errorCount_;
    emitError(location, "unsupported compound assignment operator");
    return nullptr;
  }
}

mlir::Value MLIRGen::andNotReturned(mlir::Value cond, mlir::Location location) {
  if (!returnFlag)
    return cond;
  auto i1Type = builder.getI1Type();
  auto flagVal = mlir::memref::LoadOp::create(builder, location, returnFlag, mlir::ValueRange{});
  auto trueConst = createIntConstant(builder, location, i1Type, 1);
  auto notReturned = mlir::arith::XOrIOp::create(builder, location, flagVal, trueConst);
  return mlir::arith::AndIOp::create(builder, location, cond, notReturned);
}

void MLIRGen::ensureYieldTerminator(mlir::Location location) {
  auto *blk = builder.getInsertionBlock();
  if (blk->empty() || !blk->back().hasTrait<mlir::OpTrait::IsTerminator>())
    mlir::scf::YieldOp::create(builder, location);
}

MLIRGen::LoopControl MLIRGen::pushLoopControl(const std::optional<std::string> &label,
                                              mlir::Location location) {
  auto i1Type = builder.getI1Type();
  auto memrefI1 = mlir::MemRefType::get({}, i1Type);
  auto trueVal = createIntConstant(builder, location, i1Type, 1);
  auto falseVal = createIntConstant(builder, location, i1Type, 0);

  auto activeFlag = mlir::memref::AllocaOp::create(builder, location, memrefI1);
  mlir::memref::StoreOp::create(builder, location, trueVal, activeFlag);

  auto continueFlag = mlir::memref::AllocaOp::create(builder, location, memrefI1);
  mlir::memref::StoreOp::create(builder, location, falseVal, continueFlag);

  loopActiveStack.push_back(activeFlag);
  loopDropScopeBase.push_back(dropScopes.size());
  loopContinueStack.push_back(continueFlag);
  loopBreakValueStack.push_back(nullptr);

  LoopControl lc{activeFlag, continueFlag, {}, nullptr, nullptr};
  if (label) {
    lc.labelName = *label;
    // Save any existing entry so it can be restored when this label is popped
    // (handles nested loops that shadow the same label name).
    auto ait = labeledActiveFlags.find(*label);
    if (ait != labeledActiveFlags.end()) {
      lc.prevActiveFlag = ait->second;
      // The two label maps must always be updated together; a missing continue
      // entry here means a prior push left them out of sync — fail loudly.
      auto cit = labeledContinueFlags.find(*label);
      assert(cit != labeledContinueFlags.end() &&
             "labeledActiveFlags and labeledContinueFlags are out of sync");
      lc.prevContinueFlag = cit->second;
    }
    labeledActiveFlags[lc.labelName] = activeFlag;
    labeledContinueFlags[lc.labelName] = continueFlag;
  }
  return lc;
}

void MLIRGen::popLoopControl(const LoopControl &lc, mlir::Operation *whileOp) {
  auto breakValueAlloca = loopBreakValueStack.back();

  loopActiveStack.pop_back();
  loopDropScopeBase.pop_back();
  loopContinueStack.pop_back();
  loopBreakValueStack.pop_back();

  if (!lc.labelName.empty()) {
    if (lc.prevActiveFlag) {
      // Restore the label entry that was shadowed by this inner loop.
      labeledActiveFlags[lc.labelName] = lc.prevActiveFlag;
      labeledContinueFlags[lc.labelName] = lc.prevContinueFlag;
    } else {
      labeledActiveFlags.erase(lc.labelName);
      labeledContinueFlags.erase(lc.labelName);
    }
  }

  builder.setInsertionPointAfter(whileOp);

  if (breakValueAlloca) {
    lastBreakValue = mlir::memref::LoadOp::create(builder, whileOp->getLoc(), breakValueAlloca,
                                                  mlir::ValueRange{});
  } else {
    lastBreakValue = nullptr;
  }
}

// ============================================================================
// Return-guarded statement generation
// ============================================================================

void MLIRGen::generateStmtsWithReturnGuards(
    const std::vector<std::unique_ptr<ast::Spanned<ast::Stmt>>> &stmts, size_t startIdx,
    size_t endIdx, const ast::Expr *trailingExpr, mlir::Location location) {

  for (size_t i = startIdx; i < endIdx; ++i) {
    generateStatement(stmts[i]->value);

    if (hasRealTerminator(builder.getInsertionBlock())) {
      return;
    }

    // If this statement might contain a return, guard subsequent statements.
    if (stmtMightContainReturn(stmts[i]->value)) {
      auto flagVal =
          mlir::memref::LoadOp::create(builder, location, returnFlag, mlir::ValueRange{});
      auto trueConst = createIntConstant(builder, location, builder.getI1Type(), 1);
      auto notReturned = mlir::arith::XOrIOp::create(builder, location, flagVal, trueConst);
      auto guard = mlir::scf::IfOp::create(builder, location, mlir::TypeRange{}, notReturned,
                                           /*withElseRegion=*/false);
      builder.setInsertionPointToStart(&guard.getThenRegion().front());

      // Recursively generate remaining statements inside the guard.
      generateStmtsWithReturnGuards(stmts, i + 1, endIdx, trailingExpr, location);

      ensureYieldTerminator(location);
      builder.setInsertionPointAfter(guard);
      return;
    }
  }

  // After all guarded statements, generate the trailing expression if any.
  if (trailingExpr) {
    mlir::Value val = generateExpression(*trailingExpr);
    if (val && returnSlot) {
      auto slotType = mlir::cast<mlir::MemRefType>(returnSlot.getType()).getElementType();
      val = coerceTypeForSink(val, slotType, location);
      mlir::memref::StoreOp::create(builder, location, val, returnSlot);
      auto trueConst = createIntConstant(builder, location, builder.getI1Type(), 1);
      mlir::memref::StoreOp::create(builder, location, trueConst, returnFlag);
    }
  }
}

// ============================================================================
// Loop body with continue guards
// ============================================================================

void MLIRGen::generateLoopBodyWithContinueGuards(
    const std::vector<std::unique_ptr<ast::Spanned<ast::Stmt>>> &stmts, size_t startIdx,
    size_t endIdx, mlir::Value contFlag, mlir::Location location) {

  for (size_t i = startIdx; i < endIdx; ++i) {
    generateStatement(stmts[i]->value);

    if (hasRealTerminator(builder.getInsertionBlock())) {
      return;
    }

    // If this statement might contain a break or continue, guard remaining.
    if (stmtMightContainBreakOrContinue(stmts[i]->value)) {
      auto flagVal = mlir::memref::LoadOp::create(builder, location, contFlag, mlir::ValueRange{});
      auto trueConst = createIntConstant(builder, location, builder.getI1Type(), 1);
      auto notContinued = mlir::arith::XOrIOp::create(builder, location, flagVal, trueConst);
      auto guard = mlir::scf::IfOp::create(builder, location, mlir::TypeRange{}, notContinued,
                                           /*withElseRegion=*/false);
      builder.setInsertionPointToStart(&guard.getThenRegion().front());

      generateLoopBodyWithContinueGuards(stmts, i + 1, endIdx, contFlag, location);

      ensureYieldTerminator(location);
      builder.setInsertionPointAfter(guard);
      return;
    }
  }
}

// ============================================================================
// Block generation
// ============================================================================

mlir::Value MLIRGen::generateBlock(const ast::Block &block, bool statementPosition,
                                   bool isFunctionBodyBlock) {
  // Create a new scope for variables in this block
  SymbolTableScopeT varScope(symbolTable);
  MutableTableScopeT mutScope(mutableVars);

  // RAII drop scope: emit drops when block scope exits
  struct DropScopeGuard {
    MLIRGen &gen;
    DropScopeGuard(MLIRGen &g) : gen(g) { gen.pushDropScope(); }
    ~DropScopeGuard() { gen.popDropScope(); }
  } dropGuard(*this);

  // Drain pending param drops into the body scope.  These were queued in
  // generateFunction via dropFuncForType; draining here places params at
  // relDepth 0 alongside body-local variables so the funcLevelDropExcludeVars
  // pre-scan (already complete) correctly excludes returned/early-returned ones.
  if (isFunctionBodyBlock && !pendingFunctionParamDrops.empty()) {
    for (const auto &pd : pendingFunctionParamDrops)
      registerDroppable(pd.name, pd.dropFunc, pd.isUserDrop);
    pendingFunctionParamDrops.clear();
  }

  // Only activate the return-guard path when generating the actual function
  // body block in a non-void function.  Inline sub-blocks (unsafe { … },
  // block expressions, if/else bodies) share the FuncOp's insertion block in
  // the MLIR IR, so a parentOp check on the insertion block is insufficient —
  // it would fire for every inline block inside a non-void function.
  // Callers that generate the true function body pass isFunctionBodyBlock=true;
  // all other callers use the default false.
  bool useReturnGuards = isFunctionBodyBlock && returnFlag && returnSlot;

  const auto &stmts = block.stmts;
  auto rejectGeneratorYieldFieldAccess = [&](const ast::Expr &yieldedExpr,
                                             mlir::Location location) -> bool {
    if (!std::holds_alternative<ast::ExprFieldAccess>(yieldedExpr.kind) &&
        !exprYieldsFieldMatching(yieldedExpr, [](const ast::ExprFieldAccess &) { return true; }))
      return false;
    ++errorCount_;
    emitError(location) << "yielding a field from a generator is not yet supported "
                        << "(field-alias ownership tracking is required); "
                        << "yield the whole value or clone the field instead";
    return true;
  };
  auto generateTailExpr = [&](const ast::Spanned<ast::Expr> &expr) -> mlir::Value {
    if (auto *yieldExpr = std::get_if<ast::ExprYield>(&expr.value.kind)) {
      if (yieldExpr->value &&
          rejectGeneratorYieldFieldAccess((*yieldExpr->value)->value, currentLoc))
        return nullptr;
    }
    if (statementPosition)
      return generateDiscardedExpr(expr);
    return generateExpression(expr.value);
  };

  // Generate trailing expression if present (parser may store it in expression)
  if (block.trailing_expr) {
    if (useReturnGuards) {
      auto location = builder.getUnknownLoc();
      generateStmtsWithReturnGuards(stmts, 0, stmts.size(), &block.trailing_expr->value, location);
      return nullptr; // Value is in returnSlot
    }

    for (const auto &stmtPtr : stmts) {
      generateStatement(stmtPtr->value);
      if (hasRealTerminator(builder.getInsertionBlock())) {
        return nullptr;
      }
    }
    auto result = generateTailExpr(*block.trailing_expr);
    // Null RAII close alloca for the tail expression variable so the
    // block's scope-exit drop doesn't close a handle being returned.
    if (auto *id = std::get_if<ast::ExprIdentifier>(&block.trailing_expr->value.kind)) {
      nullOutRaiiAlloca(id->name);
    }
    return result;
  }

  // The parser often places the trailing expression (last expression before })
  // as the last ExprStmt in block.statements, with block.expression == nullptr.
  // Similarly, an IfStmt at the end of a block may be the block's value
  // (e.g., `fn foo(x: i32) -> i32 { if x > 0 { 1 } else { 0 } }`).
  //
  // We detect the last statement and try to generate it as a value.
  if (!stmts.empty()) {
    // When at function body level with returnFlag, use guarded generation
    if (useReturnGuards) {
      auto location = builder.getUnknownLoc();

      // Check if the last statement can be a trailing expression
      const auto &lastStmt = stmts.back()->value;
      const ast::Expr *trailingExpr = nullptr;
      size_t stmtCount = stmts.size();

      if (std::holds_alternative<ast::StmtExpression>(lastStmt.kind)) {
        auto *exprStmt = std::get_if<ast::StmtExpression>(&lastStmt.kind);
        trailingExpr = &exprStmt->expr.value;
        stmtCount--; // Exclude the last ExprStmt; it's the trailing expr
      }

      if (trailingExpr) {
        // Generate stmts[0..stmtCount) with guards, then trailing expr
        generateStmtsWithReturnGuards(stmts, 0, stmtCount, trailingExpr, location);
        return nullptr; // Value in returnSlot
      }

      bool canProduceGuardedTailValue =
          !statementPosition && currentFunction && currentFunction.getResultTypes().size() == 1;
      if (canProduceGuardedTailValue) {
        // Handle last statement as value-producing (IfStmt or MatchStmt)
        if (auto *ifNode = std::get_if<ast::StmtIf>(&lastStmt.kind)) {
          // Generate preceding stmts with guards
          if (stmtCount > 1) {
            generateStmtsWithReturnGuards(stmts, 0, stmtCount - 1, nullptr, location);
          }
          // Guard the value-producing if-statement
          auto flagVal =
              mlir::memref::LoadOp::create(builder, location, returnFlag, mlir::ValueRange{});
          auto trueConst = createIntConstant(builder, location, builder.getI1Type(), 1);
          auto notReturned = mlir::arith::XOrIOp::create(builder, location, flagVal, trueConst);
          auto guard = mlir::scf::IfOp::create(builder, location, mlir::TypeRange{}, notReturned,
                                               /*withElseRegion=*/false);
          builder.setInsertionPointToStart(&guard.getThenRegion().front());
          auto val = generateIfStmtAsExpr(*ifNode);
          if (val && returnSlot) {
            auto slotType = mlir::cast<mlir::MemRefType>(returnSlot.getType()).getElementType();
            val = coerceTypeForSink(val, slotType, location);
            mlir::memref::StoreOp::create(builder, location, val, returnSlot);
            mlir::memref::StoreOp::create(builder, location, trueConst, returnFlag);
          }
          ensureYieldTerminator(location);
          builder.setInsertionPointAfter(guard);
          return nullptr;
        }

        if (auto *matchNode = std::get_if<ast::StmtMatch>(&lastStmt.kind)) {
          if (stmtCount > 1) {
            generateStmtsWithReturnGuards(stmts, 0, stmtCount - 1, nullptr, location);
          }
          auto flagVal =
              mlir::memref::LoadOp::create(builder, location, returnFlag, mlir::ValueRange{});
          auto trueConst = createIntConstant(builder, location, builder.getI1Type(), 1);
          auto notReturned = mlir::arith::XOrIOp::create(builder, location, flagVal, trueConst);
          auto guard = mlir::scf::IfOp::create(builder, location, mlir::TypeRange{}, notReturned,
                                               /*withElseRegion=*/false);
          builder.setInsertionPointToStart(&guard.getThenRegion().front());
          auto scrutinee = generateExpression(matchNode->scrutinee.value);
          if (scrutinee)
            scrutinee = derefIndirectEnumScrutinee(scrutinee, matchNode->scrutinee.span, location,
                                                   &matchNode->arms);
          auto resultType = currentFunction.getResultTypes()[0];
          auto val = scrutinee ? generateMatchImpl(scrutinee, matchNode->arms, resultType, location)
                               : nullptr;
          if (val && returnSlot) {
            auto slotType = mlir::cast<mlir::MemRefType>(returnSlot.getType()).getElementType();
            val = coerceTypeForSink(val, slotType, location);
            mlir::memref::StoreOp::create(builder, location, val, returnSlot);
            mlir::memref::StoreOp::create(builder, location, trueConst, returnFlag);
          }
          ensureYieldTerminator(location);
          builder.setInsertionPointAfter(guard);
          return nullptr;
        }
      }

      // No trailing expression: generate all statements with guards
      generateStmtsWithReturnGuards(stmts, 0, stmts.size(), nullptr, location);
      return nullptr;
    }

    // Generate all statements except the last one
    for (size_t i = 0; i + 1 < stmts.size(); ++i) {
      generateStatement(stmts[i]->value);
      if (hasRealTerminator(builder.getInsertionBlock())) {
        return nullptr;
      }
    }

    // Handle the last statement specially
    const auto &lastStmt = stmts.back()->value;

    // Case 1: Expression statement -- treat as trailing expression
    if (std::holds_alternative<ast::StmtExpression>(lastStmt.kind)) {
      auto *exprStmt = std::get_if<ast::StmtExpression>(&lastStmt.kind);
      if (exprStmt) {
        auto result = generateTailExpr(exprStmt->expr);
        if (auto *id = std::get_if<ast::ExprIdentifier>(&exprStmt->expr.value.kind)) {
          nullOutRaiiAlloca(id->name);
        }
        return result;
      }
    }

    if (!statementPosition) {
      // Case 2: If statement as a value-producing block ending
      // When a block ends with `if ... { ... } else { ... }` and the enclosing
      // function returns a value, generate it as an if-expression.
      if (auto *ifStmt = std::get_if<ast::StmtIf>(&lastStmt.kind)) {
        if (!currentFunction || currentFunction.getResultTypes().size() != 1) {
          generateIfStmt(*ifStmt);
          return nullptr;
        }
        return generateIfStmtAsExpr(*ifStmt);
      }

      // Case 3: Match statement as a value-producing block ending
      if (auto *matchNode = std::get_if<ast::StmtMatch>(&lastStmt.kind)) {
        if (!currentFunction || currentFunction.getResultTypes().size() != 1) {
          generateMatchStmt(*matchNode);
          return nullptr;
        }
        auto location = loc(lastStmt.span);
        auto scrutinee = generateExpression(matchNode->scrutinee.value);
        if (!scrutinee)
          return nullptr;
        scrutinee = derefIndirectEnumScrutinee(scrutinee, matchNode->scrutinee.span, location,
                                               &matchNode->arms);
        if (!scrutinee)
          return nullptr;
        auto resultType = currentFunction.getResultTypes()[0];
        return generateMatchImpl(scrutinee, matchNode->arms, resultType, location);
      }
    }

    // Case 4: Loop/While/For as a value-producing block ending (via break-with-value)
    if (std::holds_alternative<ast::StmtLoop>(lastStmt.kind) ||
        std::holds_alternative<ast::StmtWhile>(lastStmt.kind) ||
        std::holds_alternative<ast::StmtWhileLet>(lastStmt.kind) ||
        std::holds_alternative<ast::StmtFor>(lastStmt.kind)) {
      lastBreakValue = nullptr;
      generateStatement(lastStmt);
      if (lastBreakValue)
        return lastBreakValue;
      return nullptr;
    }

    // Otherwise, generate it as a normal statement
    generateStatement(lastStmt);
  }

  return nullptr;
}

// ============================================================================
// Statement generation
// ============================================================================

void MLIRGen::generateStatement(const ast::Stmt &stmt) {
  currentLoc = loc(stmt.span);
  if (auto *s = std::get_if<ast::StmtLet>(&stmt.kind)) {
    generateLetStmt(*s);
    return;
  }
  if (auto *s = std::get_if<ast::StmtVar>(&stmt.kind)) {
    generateVarStmt(*s);
    return;
  }
  if (auto *s = std::get_if<ast::StmtAssign>(&stmt.kind)) {
    generateAssignStmt(*s);
    return;
  }
  if (auto *s = std::get_if<ast::StmtIf>(&stmt.kind)) {
    generateIfStmt(*s);
    return;
  }
  if (auto *s = std::get_if<ast::StmtWhile>(&stmt.kind)) {
    generateWhileStmt(*s);
    return;
  }
  if (auto *s = std::get_if<ast::StmtWhileLet>(&stmt.kind)) {
    generateWhileLetStmt(*s);
    return;
  }
  if (auto *s = std::get_if<ast::StmtFor>(&stmt.kind)) {
    if (s->is_await)
      generateForAwaitStmt(*s);
    else
      generateForStmt(*s);
    return;
  }
  if (auto *s = std::get_if<ast::StmtReturn>(&stmt.kind)) {
    generateReturnStmt(*s);
    return;
  }
  if (auto *s = std::get_if<ast::StmtExpression>(&stmt.kind)) {
    generateExprStmt(*s);
    return;
  }
  if (auto *s = std::get_if<ast::StmtLoop>(&stmt.kind)) {
    generateLoopStmt(*s);
    return;
  }
  if (auto *s = std::get_if<ast::StmtMatch>(&stmt.kind)) {
    generateMatchStmt(*s);
    return;
  }
  if (auto *s = std::get_if<ast::StmtBreak>(&stmt.kind)) {
    generateBreakStmt(*s);
    return;
  }
  if (auto *s = std::get_if<ast::StmtContinue>(&stmt.kind)) {
    generateContinueStmt(*s);
    return;
  }
  if (auto *s = std::get_if<ast::StmtDefer>(&stmt.kind)) {
    if (s->expr) {
      currentFnDefers.emplace_back(&s->expr->value, currentLoc);
    }
    return;
  }
  if (auto *ifLet = std::get_if<ast::StmtIfLet>(&stmt.kind)) {
    generateIfLetStmt(*ifLet);
    return;
  }
}

void MLIRGen::bindLetSubPattern(const ast::Pattern &pattern, mlir::Value value,
                                mlir::Location location) {
  if (auto *identPat = std::get_if<ast::PatIdentifier>(&pattern.kind)) {
    declareVariable(identPat->name, value);
  } else if (std::holds_alternative<ast::PatWildcard>(pattern.kind)) {
    // Wildcard — discard the value
  } else if (auto *tuplePat = std::get_if<ast::PatTuple>(&pattern.kind)) {
    for (uint32_t i = 0; i < tuplePat->elements.size(); i++) {
      mlir::Value elemVal;
      if (auto hewTuple = mlir::dyn_cast<hew::HewTupleType>(value.getType())) {
        elemVal = hew::TupleExtractOp::create(builder, location, hewTuple.getElementTypes()[i],
                                              value, static_cast<int64_t>(i));
      } else {
        elemVal = mlir::LLVM::ExtractValueOp::create(
            builder, location, value, llvm::ArrayRef<int64_t>{static_cast<int64_t>(i)});
      }
      bindLetSubPattern(tuplePat->elements[i]->value, elemVal, location);
    }
  } else if (auto *structPat = std::get_if<ast::PatStruct>(&pattern.kind)) {
    auto structIt = structTypes.find(structPat->name);
    if (structIt != structTypes.end()) {
      const auto &info = structIt->second;
      for (const auto &pf : structPat->fields) {
        for (const auto &fi : info.fields) {
          if (fi.name == pf.name) {
            auto fieldVal = hew::FieldGetOp::create(
                builder, location,
                mlir::cast<mlir::LLVM::LLVMStructType>(value.getType()).getBody()[fi.index], value,
                builder.getStringAttr(fi.name), static_cast<int64_t>(fi.index));
            if (pf.pattern) {
              bindLetSubPattern(pf.pattern->value, fieldVal, location);
            } else {
              declareVariable(pf.name, fieldVal);
            }
            break;
          }
        }
      }
    } else {
      ++errorCount_;
      emitError(location) << "unknown struct type '" << structPat->name << "' in let pattern";
    }
  } else {
    ++errorCount_;
    emitError(location) << "unsupported sub-pattern in let destructuring";
  }
}

void MLIRGen::generateLetStmt(const ast::StmtLet &stmt) {
  auto location = currentLoc;

  // ── Generic lambda deferred registration ─────────────────────────────────
  // If the RHS is a generic lambda (type_params present and non-empty), defer
  // code generation until a concrete call site supplies the type arguments.
  // Register the lambda under the binding name and return without emitting any
  // IR.  The call path will specialise it via specializeGenericLambda().
  if (stmt.value) {
    if (auto *lam = std::get_if<ast::ExprLambda>(&stmt.value->value.kind)) {
      if (lam->type_params.has_value() && !lam->type_params->empty()) {
        if (auto *identPat = std::get_if<ast::PatIdentifier>(&stmt.pattern.value.kind)) {
          genericLambdas[identPat->name] = lam;
          return;
        }
      }
    }
  }

  // Set the declared type so constructors (Vec::new, HashMap::new, None, Ok,
  // Err) can emit correctly typed results.
  // Note: stmt.ty may contain inferred unit type `()` → NoneType, which is
  // valid here. Use convertType (not convertTypeOrError) to allow NoneType.
  std::optional<mlir::Type> typeHint;
  if (stmt.ty) {
    auto declaredTypeHint = convertType(stmt.ty->value);
    if (declaredTypeHint)
      typeHint = declaredTypeHint;
  }

  mlir::Value value = nullptr;
  lastScopeLaunchResultType.reset();
  if (stmt.value) {
    value = generateExpression(stmt.value->value, typeHint);
  }
  if (!value)
    return;

  // Type coercion: if declared type doesn't match value type, try to coerce
  if (stmt.ty) {
    auto declaredType = convertType(stmt.ty->value);
    // Timeout lowering still evaluates the inner expression directly while the
    // enriched AST annotates `| after` results as inferred Option<T>.
    bool skipInferredTimeoutCoercion =
        isInferredType(*stmt.ty) && usesTimeoutPlaceholder(stmt.value);
    if (isValidType(declaredType) && !skipInferredTimeoutCoercion) {
      bool isUnsigned =
          mlir::isa<mlir::IntegerType>(declaredType) && isUnsignedTypeExpr(stmt.ty->value);
      value = coerceType(value, declaredType, location, isUnsigned);
    }
  }

  if (value && mlir::isa<hew::ClosureType>(value.getType()) && stmt.value &&
      (std::holds_alternative<ast::ExprIdentifier>(stmt.value->value.kind) ||
       std::holds_alternative<ast::ExprFieldAccess>(stmt.value->value.kind))) {
    auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
    auto fnPtr = hew::ClosureGetFnOp::create(builder, location, ptrType, value);
    auto envPtr = hew::ClosureGetEnvOp::create(builder, location, ptrType, value);
    auto clonedEnv = hew::RcCloneOp::create(builder, location, ptrType, envPtr).getResult();
    value = hew::ClosureCreateOp::create(
        builder, location, mlir::cast<hew::ClosureType>(value.getType()), fnPtr, clonedEnv);
  }

  // Extract the name from the pattern
  const auto &pattern = stmt.pattern.value;
  if (auto *identPat = std::get_if<ast::PatIdentifier>(&pattern.kind)) {
    auto varName = identPat->name;
    declareVariable(varName, value);

    // Track actor variable types for method call dispatch
    // Normal spawns: extract from type annotation (ActorRef<MyActor>)
    if (stmt.ty) {
      auto resolveAliasExpr = [this](llvm::StringRef name) { return resolveTypeAliasExpr(name); };
      auto actorName = typeExprToActorName(stmt.ty->value, resolveAliasExpr);
      if (!actorName.empty() && actorRegistry.count(actorName))
        actorVarTypes[varName] = actorName;
    }
    // Infer actor/supervisor type from spawn expression (no type annotation needed)
    if (stmt.value) {
      if (auto *spawn = std::get_if<ast::ExprSpawn>(&stmt.value->value.kind)) {
        if (auto *ident = std::get_if<ast::ExprIdentifier>(&spawn->target->value.kind)) {
          if (actorRegistry.count(ident->name) || supervisorChildren.count(ident->name))
            actorVarTypes[varName] = ident->name;
        }
      }
    }
    // Detect Node::lookup("name") → register variable as remote actor ref.
    // The actor type comes from the type annotation: let x: Counter = Node::lookup(...)
    if (stmt.value) {
      if (auto *callExpr = std::get_if<ast::ExprCall>(&stmt.value->value.kind)) {
        if (callExpr->function) {
          if (auto *fnIdent = std::get_if<ast::ExprIdentifier>(&callExpr->function->value.kind)) {
            if (fnIdent->name == "Node::lookup") {
              std::string actorName;
              if (stmt.ty) {
                auto resolveAliasExpr = [this](llvm::StringRef name) {
                  return resolveTypeAliasExpr(name);
                };
                actorName = typeExprToTypeName(stmt.ty->value, resolveAliasExpr);
              }
              if (!actorName.empty() && actorRegistry.count(actorName)) {
                actorVarTypes[varName] = actorName;
              }
            }
          }
        }
      }
    }
    // Lambda actors use generated names not in the type annotation
    if (stmt.value && std::holds_alternative<ast::ExprSpawnLambdaActor>(stmt.value->value.kind)) {
      if (lambdaActorCounter > 0)
        actorVarTypes[varName] = "__lambda_actor_" + std::to_string(lambdaActorCounter - 1);
    }

    // Track generator variables: let g = gen_func()
    if (stmt.value) {
      if (auto *callExpr = std::get_if<ast::ExprCall>(&stmt.value->value.kind)) {
        if (callExpr->function) {
          if (auto *fnIdent = std::get_if<ast::ExprIdentifier>(&callExpr->function->value.kind)) {
            auto calleeName = fnIdent->name;
            if (generatorFunctions.count(calleeName)) {
              generatorVarTypes[varName] = calleeName;
              // Generator handle is a malloc'd coroutine frame; free at scope exit.
              registerDroppable(varName, "free");
            }
            // Track supervisor_child() calls
            if (calleeName == "supervisor_child" && callExpr->args.size() >= 2) {
              if (auto *supIdent = std::get_if<ast::ExprIdentifier>(
                      &ast::callArgExpr(callExpr->args[0]).value.kind)) {
                auto supIt = actorVarTypes.find(supIdent->name);
                if (supIt != actorVarTypes.end()) {
                  auto childrenIt = supervisorChildren.find(supIt->second);
                  if (childrenIt != supervisorChildren.end()) {
                    if (auto *litExpr = std::get_if<ast::ExprLiteral>(
                            &ast::callArgExpr(callExpr->args[1]).value.kind)) {
                      if (auto *intLit = std::get_if<ast::LitInteger>(&litExpr->lit)) {
                        auto idx = intLit->value;
                        if (idx >= 0 && static_cast<size_t>(idx) < childrenIt->second.size())
                          actorVarTypes[varName] = childrenIt->second[static_cast<size_t>(idx)];
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }

    // Track named supervisor child access: let w = sup.child_name
    if (stmt.value) {
      if (auto *fa = std::get_if<ast::ExprFieldAccess>(&stmt.value->value.kind)) {
        if (auto *objIdent = std::get_if<ast::ExprIdentifier>(&fa->object->value.kind)) {
          auto avIt = actorVarTypes.find(objIdent->name);
          if (avIt != actorVarTypes.end()) {
            auto scnIt = supervisorChildNames.find(avIt->second);
            if (scnIt != supervisorChildNames.end()) {
              for (const auto &[childName, childType] : scnIt->second) {
                if (childName == fa->field) {
                  actorVarTypes[varName] = childType;
                  break;
                }
              }
            }
          }
        }
      }
    }

    // Track scope.launch / scope.spawn task result types for await
    if (stmt.value &&
        (std::holds_alternative<ast::ExprScopeLaunch>(stmt.value->value.kind) ||
         std::holds_alternative<ast::ExprScopeSpawn>(stmt.value->value.kind)) &&
        lastScopeLaunchResultType.has_value()) {
      taskResultTypes[varName] = *lastScopeLaunchResultType;
      lastScopeLaunchResultType.reset();
    }

    // Track handle variables from type annotation (filled by enrich_program)
    if (stmt.ty) {
      auto resolveAliasExpr = [this](llvm::StringRef name) { return resolveTypeAliasExpr(name); };
      auto handleStr = typeExprToHandleString(stmt.ty->value, knownHandleTypes, resolveAliasExpr);
      if (!handleStr.empty()) {
        handleVarTypes[varName] = handleStr;
        if (auto bindingIdentity = resolveCurrentBindingIdentity(varName))
          annotatedHandleTypes[bindingIdentity] = &stmt.ty->value;
      }
    }

    // ── Track first-class Stream<T> / Sink<T> variables ─────────────────
    if (stmt.ty) {
      if (auto streamInfo = streamHandleInfoFromTypeExpr(stmt.ty->value))
        rememberTrackedStreamHandleInfo(varName, *streamInfo);
    }
    if (stmt.value) {
      if (auto streamInfo = resolveStreamHandleInfo(stmt.value->value))
        rememberTrackedStreamHandleInfo(varName, *streamInfo);
    }
    // Track MPSC channel handle variables so the codegen uses ptr not struct.
    if (stmt.value) {
      if (auto *ce = std::get_if<ast::ExprCall>(&stmt.value->value.kind)) {
        if (ce->function) {
          if (auto *fi = std::get_if<ast::ExprIdentifier>(&ce->function->value.kind)) {
            if (fi->name == "hew_channel_sender_clone" || fi->name == "hew_channel_pair_sender")
              handleVarTypes[varName] = "Sender";
            else if (fi->name == "hew_channel_pair_receiver")
              handleVarTypes[varName] = "Receiver";
          }
        }
      }
    }

    // ── Drop registration (shared with generateVarStmt) ─────────────
    registerDropsForVariable(varName, value, &stmt.ty, &stmt.value,
                             /*isMutable=*/false, location);

  } else if (auto *tuplePat = std::get_if<ast::PatTuple>(&pattern.kind)) {
    bindTuplePatternFields(*tuplePat, value, location);

    // ── Track + RAII auto-close for destructured Stream/Sink tuple elements ──
    auto tupleStreamInfos = resolveTuplePatternStreamHandleInfos(stmt);
    for (size_t i = 0; i < tupleStreamInfos.size() && i < tuplePat->elements.size(); ++i) {
      auto *elemIdent = std::get_if<ast::PatIdentifier>(&tuplePat->elements[i]->value.kind);
      if (!elemIdent || !tupleStreamInfos[i].isTracked())
        continue;

      rememberTrackedStreamHandleInfo(elemIdent->name, tupleStreamInfos[i]);

      std::string closeFn;
      if (tupleStreamInfos[i].kind == "Stream")
        closeFn = "hew_stream_close";
      else if (tupleStreamInfos[i].kind == "Sink")
        closeFn = "hew_sink_close";
      else if (tupleStreamInfos[i].kind == "Pair")
        closeFn = "hew_stream_pair_free";
      if (!closeFn.empty()) {
        auto elemVal = lookupVariable(elemIdent->name);
        if (elemVal) {
          auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
          mlir::Value closeValue = elemVal;
          if (!mlir::isa<mlir::LLVM::LLVMPointerType>(closeValue.getType()))
            closeValue = hew::BitcastOp::create(builder, location, ptrType, closeValue);
          auto allocaType = mlir::MemRefType::get({}, ptrType);
          auto closeAlloca = mlir::memref::AllocaOp::create(builder, location, allocaType);
          mlir::memref::StoreOp::create(builder, location, closeValue, closeAlloca,
                                        mlir::ValueRange{});
          if (!dropScopes.empty()) {
            DropEntry entry;
            entry.varName = elemIdent->name;
            entry.dropFuncName = closeFn;
            entry.closeAlloca = closeAlloca;
            entry.bindingIdentity = resolveCurrentBindingIdentity(elemIdent->name);
            maybeRecordFunctionDropExclusion(elemIdent->name, entry.bindingIdentity);
            dropScopes.back().push_back(std::move(entry));
          }
        }
      }
    }
  } else if (std::holds_alternative<ast::PatStruct>(pattern.kind)) {
    bindLetSubPattern(pattern, value, location);
  } else if (std::holds_alternative<ast::PatWildcard>(pattern.kind)) {
    // Wildcard let: `let _ = expr` — discard the value but route droppable
    // heap temporaries through materializeTemporary so they are freed at scope
    // exit.  Mirrors the expression-statement discard path in generateExprStmt.
    // stmt.value is guaranteed non-null here (we returned early above if !value).
    materializeTemporary(value, stmt.value->value);
  } else {
    ++errorCount_;
    emitError(location) << "only simple identifier patterns supported for let";
  }
}

void MLIRGen::generateVarStmt(const ast::StmtVar &stmt) {
  auto location = currentLoc;
  auto varNameStr = stmt.name;
  // Set the declared type so constructors can emit correctly typed results.
  // Note: stmt.ty may contain inferred unit type `()` → NoneType, which is
  // valid here. Use convertType (not convertTypeOrError) to allow NoneType.
  std::optional<mlir::Type> typeHint;
  if (stmt.ty) {
    auto declaredTypeHint = convertType(stmt.ty->value);
    if (declaredTypeHint)
      typeHint = declaredTypeHint;
  }

  // Determine the type
  mlir::Type varType;
  mlir::Value initValue = nullptr;

  if (stmt.value) {
    initValue = generateExpression(stmt.value->value, typeHint);
    if (!initValue)
      return;
    varType = initValue.getType();
  }

  if (stmt.ty) {
    varType = convertType(stmt.ty->value);
    bool skipInferredTimeoutCoercion =
        isInferredType(*stmt.ty) && usesTimeoutPlaceholder(stmt.value);
    if (isValidType(varType) && initValue && !skipInferredTimeoutCoercion) {
      bool isUnsigned = mlir::isa<mlir::IntegerType>(varType) && isUnsignedTypeExpr(stmt.ty->value);
      initValue = coerceType(initValue, varType, location, isUnsigned);
    }
  }

  if (!varType) {
    ++errorCount_;
    emitError(location) << "cannot determine type for var declaration";
    return;
  }

  declareMutableVariable(varNameStr, varType, initValue);

  // Track handle variables from type annotation (filled by enrich_program)
  if (stmt.ty) {
    auto resolveAliasExpr = [this](llvm::StringRef name) { return resolveTypeAliasExpr(name); };
    auto handleStr = typeExprToHandleString(stmt.ty->value, knownHandleTypes, resolveAliasExpr);
    if (!handleStr.empty()) {
      handleVarTypes[varNameStr] = handleStr;
      if (auto bindingIdentity = resolveCurrentBindingIdentity(varNameStr))
        annotatedHandleTypes[bindingIdentity] = &stmt.ty->value;
    }
  }

  // ── Track first-class Stream<T> / Sink<T> for var statements ────────────
  if (stmt.ty) {
    if (auto streamInfo = streamHandleInfoFromTypeExpr(stmt.ty->value))
      rememberTrackedStreamHandleInfo(varNameStr, *streamInfo);
  }
  if (stmt.value) {
    if (auto streamInfo = resolveStreamHandleInfo(stmt.value->value))
      rememberTrackedStreamHandleInfo(varNameStr, *streamInfo);
  }

  // ── Drop registration (shared with generateLetStmt) ──────────────
  registerDropsForVariable(varNameStr, initValue, &stmt.ty, &stmt.value,
                           /*isMutable=*/true, location);
}

// ═══════════════════════════════════════════════════════════════════════════
// Shared drop registration for let/var bindings
// ═══════════════════════════════════════════════════════════════════════════

void MLIRGen::registerDropsForVariable(const std::string &varName, mlir::Value value,
                                       const std::optional<ast::Spanned<ast::TypeExpr>> *stmtTy,
                                       const std::optional<ast::Spanned<ast::Expr>> *stmtValue,
                                       bool isMutable, mlir::Location location) {

  // ── RAII auto-close for Stream/Sink handles ──────────────────────────
  {
    auto *streamInfo = lookupTrackedStreamHandleInfo(varName);
    if (!streamInfo && stmtValue && *stmtValue) {
      if (auto inferredInfo = resolveStreamHandleInfo((*stmtValue)->value)) {
        rememberTrackedStreamHandleInfo(varName, *inferredInfo);
        streamInfo = lookupTrackedStreamHandleInfo(varName);
      }
    }
    if (streamInfo && value) {
      std::string closeFn;
      std::string kind = streamInfo->kind;
      if (kind == "Stream")
        closeFn = "hew_stream_close";
      else if (kind == "Sink")
        closeFn = "hew_sink_close";
      else if (kind == "Pair")
        closeFn = "hew_stream_pair_free";
      if (!closeFn.empty()) {
        auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
        mlir::Value closeValue = value;
        if (!mlir::isa<mlir::LLVM::LLVMPointerType>(closeValue.getType()))
          closeValue = hew::BitcastOp::create(builder, location, ptrType, closeValue);
        auto allocaType = mlir::MemRefType::get({}, ptrType);
        auto closeAlloca = mlir::memref::AllocaOp::create(builder, location, allocaType);
        mlir::memref::StoreOp::create(builder, location, closeValue, closeAlloca,
                                      mlir::ValueRange{});
        if (!dropScopes.empty()) {
          DropEntry entry;
          entry.varName = varName;
          entry.dropFuncName = closeFn;
          entry.closeAlloca = closeAlloca;
          entry.bindingIdentity = resolveCurrentBindingIdentity(varName);
          maybeRecordFunctionDropExclusion(varName, entry.bindingIdentity);
          dropScopes.back().push_back(std::move(entry));
        }
      }
    }
  }

  bool isBorrowedGetString = false;

  // When `let x = struct_var.field` extracts a String field from a user
  // struct that has owned fields (and will free them via __auto_field_drop),
  // registering a separate hew_string_drop for `x` creates an aliased
  // pointer that gets freed twice: once by `x`'s drop and once by the
  // struct's auto-field-drop.  Detect this pattern and suppress the
  // hew_string_drop registration so the struct remains the sole owner.
  //
  // Restriction: only suppress when the source is a plain identifier
  // (variable or function parameter).  For temporaries like `f().field`,
  // the struct has no owner to free its fields, so `x` must keep its drop.
  // Detect `let x = struct_var.string_field` — the field extraction pattern.
  // When the RHS is an ExprFieldAccess from a plain identifier whose resolved
  // type is a user struct with owned fields, `x` must NOT get its own
  // hew_string_drop because the struct owner will free the field via
  // __auto_field_drop exactly once.
  //
  // Two complementary detection paths are used:
  //   (A) AST path: uses resolvedTypeOf() — works for local variables whose
  //       expression type was recorded by the type checker.
  //   (B) MLIR path: traces the defining op chain of the MLIR `value` back
  //       through bitcasts to a hew.field_get on an LLVM struct type.  This
  //       path handles imported-module function bodies, where the type checker
  //       does not type-check bodies (only signatures) and therefore leaves no
  //       entries in exprTypeMap.
  bool isFieldExtractionFromOwnedStruct = false;
  if (stmtValue && *stmtValue) {
    if (auto *fa = std::get_if<ast::ExprFieldAccess>(&(*stmtValue)->value.kind)) {
      if (std::get_if<ast::ExprIdentifier>(&fa->object->value.kind)) {
        // (A) AST-based path: look up the identifier's resolved type.
        if (auto *srcTy = resolvedTypeOf(fa->object->span)) {
          if (auto *named = std::get_if<ast::TypeNamed>(&srcTy->kind)) {
            if (structTypes.count(named->name) && !userDropFuncs.count(named->name) &&
                structHasOwnedFields(named->name))
              isFieldExtractionFromOwnedStruct = true;
          }
        }
        // (B) MLIR-value path: trace the MLIR value back through bitcasts to
        //     a hew.field_get on an identified LLVM struct with owned fields.
        //     This handles imported function bodies not seen by the type checker.
        if (!isFieldExtractionFromOwnedStruct && value) {
          mlir::Value v = value;
          // Peel through any bitcast layers.
          while (v) {
            auto *defOp = v.getDefiningOp();
            if (!defOp)
              break;
            if (auto bitcast = mlir::dyn_cast<hew::BitcastOp>(defOp)) {
              v = bitcast.getInput();
            } else if (auto fieldGet = mlir::dyn_cast<hew::FieldGetOp>(defOp)) {
              // Check the struct type of the object the field is accessed on.
              auto srcType = fieldGet.getStructVal().getType();
              if (auto structTy = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(srcType)) {
                if (structTy.isIdentified()) {
                  auto name = structTy.getName().str();
                  if (structTypes.count(name) && !userDropFuncs.count(name) &&
                      structHasOwnedFields(name))
                    isFieldExtractionFromOwnedStruct = true;
                }
              }
              break;
            } else {
              break;
            }
          }
        }
      }
    }
  }

  // ── Rc<T> implicit clone on rebinding ─────────────────────────────
  // When `let rc2 = rc` (plain identifier) binds an existing Rc, the new
  // binding aliases the same allocation.  Without a refcount increment
  // both scope-exit drops would decrement the same count-1 cell, causing
  // a use-after-free.  Emit an implicit RcCloneOp (matching the closure-
  // env prior art at the bottom of this function) and register the drop.
  //
  // Detection: stmtValue is ExprIdentifier AND the resolved type (or the
  // type annotation) is Rc.  We must NOT fire for Rc::new / .clone() /
  // other call expressions — those are handled by the later sections.
  if (value && stmtValue && *stmtValue &&
      std::holds_alternative<ast::ExprIdentifier>((*stmtValue)->value.kind)) {
    bool isRcRebind = false;
    // Check type annotation first
    if (stmtTy && *stmtTy) {
      if (auto *named = std::get_if<ast::TypeNamed>(&(*stmtTy)->value.kind))
        isRcRebind = resolveTypeAlias(named->name) == "Rc";
    }
    // Fall back to resolved type from type checker
    if (!isRcRebind) {
      if (auto *resolvedType = resolvedTypeOf((*stmtValue)->span)) {
        if (auto *named = std::get_if<ast::TypeNamed>(&resolvedType->kind))
          isRcRebind = (named->name == "Rc");
      }
    }
    if (isRcRebind && mlir::isa<mlir::LLVM::LLVMPointerType>(value.getType())) {
      auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
      hew::RcCloneOp::create(builder, location, ptrType, value);
      registerDroppable(varName, "hew_rc_drop");
    }
  }

  // ── Type-annotation-based drops ────────────────────────────────────
  if (stmtTy && *stmtTy) {
    if (auto *named = std::get_if<ast::TypeNamed>(&(*stmtTy)->value.kind)) {
      auto typeName = resolveTypeAlias(named->name);
      auto *defOp = (value && value.getDefiningOp()) ? value.getDefiningOp() : nullptr;
      bool isVecCtor = defOp && mlir::isa<hew::VecNewOp>(defOp);
      bool isHashMapCtor = defOp && mlir::isa<hew::HashMapNewOp>(defOp);
      bool isHashSetCtor =
          defOp && defOp->getName().getStringRef() == "hew.runtime_call" &&
          defOp->hasAttr("callee") &&
          mlir::cast<mlir::SymbolRefAttr>(defOp->getAttr("callee")).getLeafReference() ==
              "hew_hashset_new";
      bool isNewCollectionMethod = false;
      if (stmtValue && *stmtValue) {
        if (auto *mc = std::get_if<ast::ExprMethodCall>(&(*stmtValue)->value.kind))
          isNewCollectionMethod = (mc->method != "get");
      }
      bool isBytesLiteral = false;
      if (stmtValue && *stmtValue) {
        const auto &vk = (*stmtValue)->value.kind;
        isBytesLiteral = std::holds_alternative<ast::ExprByteStringLiteral>(vk) ||
                         std::holds_alternative<ast::ExprByteArrayLiteral>(vk);
      }
      if ((typeName == "Vec" || typeName == "bytes") &&
          (isVecCtor || isNewCollectionMethod || isBytesLiteral))
        registerDroppable(varName, "hew_vec_free");
      else if (typeName == "HashMap" && (isHashMapCtor || isNewCollectionMethod))
        registerDroppable(varName, "hew_hashmap_free_impl");
      else if (typeName == "HashSet" && (isHashSetCtor || isNewCollectionMethod))
        registerDroppable(varName, "hew_hashset_free");
      else if (typeName == "Rc") {
        bool isRcCtor = defOp && mlir::isa<hew::RcNewOp>(defOp);
        bool isCloneCall = false;
        if (stmtValue && *stmtValue) {
          if (auto *mc = std::get_if<ast::ExprMethodCall>(&(*stmtValue)->value.kind))
            isCloneCall = (mc->method == "clone");
        }
        if (isRcCtor || isCloneCall)
          registerDroppable(varName, "hew_rc_drop");
      } else if ((typeName == "String" || typeName == "string" || typeName == "str") &&
                 !handleVarTypes.count(varName) && !lookupTrackedStreamHandleInfo(varName)) {
        bool isBorrowed = isBorrowedGetString;
        if (stmtValue && *stmtValue) {
          if (auto *mc = std::get_if<ast::ExprMethodCall>(&(*stmtValue)->value.kind))
            isBorrowed = (mc->method == "get");
        }
        if (!isBorrowed && !isFieldExtractionFromOwnedStruct)
          registerDroppable(varName, "hew_string_drop");
      } else {
        auto dropIt = userDropFuncs.find(typeName);
        if (dropIt != userDropFuncs.end())
          registerDroppable(varName, dropIt->second, /*isUserDrop=*/true);
      }
    }
  }

  // ── Runtime string drop by value type ──────────────────────────────
  if (value && mlir::isa<hew::StringRefType>(value.getType())) {
    bool isHandle = handleVarTypes.count(varName) || lookupTrackedStreamHandleInfo(varName);

    bool alreadyRegistered = false;
    if (!dropScopes.empty()) {
      for (auto &e : dropScopes.back()) {
        if (e.varName == varName) {
          alreadyRegistered = true;
          break;
        }
      }
    }
    if (!alreadyRegistered && !isHandle && !isBorrowedGetString) {
      if (isMutable) {
        // Mutable variables gate on ownership to avoid double-free on
        // borrowed values that may be reassigned.
        bool isOwned = false;
        if (auto *defOp = value.getDefiningOp()) {
          isOwned = mlir::isa<hew::StringConcatOp>(defOp) ||
                    defOp->getName().getStringRef() == "hew.to_string" ||
                    defOp->getName().getStringRef() == "hew.constant" ||
                    mlir::isa<mlir::func::CallOp>(defOp);
        }
        if (stmtValue && *stmtValue) {
          const auto &vk = (*stmtValue)->value.kind;
          if (std::holds_alternative<ast::ExprInterpolatedString>(vk) ||
              std::holds_alternative<ast::ExprCall>(vk) ||
              std::holds_alternative<ast::ExprMethodCall>(vk) ||
              std::holds_alternative<ast::ExprBinary>(vk) ||
              std::holds_alternative<ast::ExprLiteral>(vk))
            isOwned = true;
        }
        if (isOwned && !isFieldExtractionFromOwnedStruct)
          registerDroppable(varName, "hew_string_drop");
      } else {
        if (!isFieldExtractionFromOwnedStruct)
          registerDroppable(varName, "hew_string_drop");
      }
    }
  }

  // ── MLIR-type fallback for Vec/HashMap/Rc ───────────────────────────
  if (value) {
    auto valType = value.getType();
    bool isOwned = false;
    if (auto *defOp = value.getDefiningOp()) {
      isOwned = mlir::isa<hew::VecNewOp>(defOp) || mlir::isa<hew::HashMapNewOp>(defOp) ||
                mlir::isa<hew::RcNewOp>(defOp);
    }
    if (!isOwned && stmtValue && *stmtValue) {
      const auto &vk = (*stmtValue)->value.kind;
      if (auto *mc = std::get_if<ast::ExprMethodCall>(&vk)) {
        if (mc->method != "get")
          isOwned = true;
      }
      if (auto *call = std::get_if<ast::ExprCall>(&vk)) {
        mlir::Value callResult = value;
        if (auto *defOp = callResult.getDefiningOp()) {
          if (defOp->getName().getStringRef() == "hew.bitcast" && defOp->getNumOperands() > 0)
            callResult = defOp->getOperand(0);
        }
        if (auto *defOp = callResult.getDefiningOp()) {
          if (auto callOp = mlir::dyn_cast<mlir::func::CallOp>(defOp)) {
            if (auto callee = module.lookupSymbol<mlir::func::FuncOp>(callOp.getCallee())) {
              if (callee.isExternal())
                isOwned = true;
            }
          }
        }
      }
      if (std::holds_alternative<ast::ExprByteStringLiteral>(vk) ||
          std::holds_alternative<ast::ExprByteArrayLiteral>(vk))
        isOwned = true;
      if (auto *ue = std::get_if<ast::ExprUnsafe>(&vk)) {
        if (ue->block.trailing_expr) {
          const auto &inner = ue->block.trailing_expr->value.kind;
          if (std::holds_alternative<ast::ExprCall>(inner) ||
              std::holds_alternative<ast::ExprMethodCall>(inner))
            isOwned = true;
        }
      }
    }
    auto alreadyRegistered = [&]() {
      if (dropScopes.empty())
        return false;
      for (auto &e : dropScopes.back())
        if (e.varName == varName)
          return true;
      return false;
    };
    if (isOwned && mlir::isa<hew::VecType>(valType) && !alreadyRegistered())
      registerDroppable(varName, "hew_vec_free");
    else if (isOwned && mlir::isa<hew::HashMapType>(valType) && !alreadyRegistered())
      registerDroppable(varName, "hew_hashmap_free_impl");
    else if (isOwned && mlir::isa<mlir::LLVM::LLVMPointerType>(valType) && !alreadyRegistered()) {
      std::string dropFn;
      if (stmtValue && *stmtValue) {
        if (auto *resolvedType = resolvedTypeOf((*stmtValue)->span))
          dropFn = dropFuncForType(*resolvedType);
      }
      if (dropFn.empty() && value.getDefiningOp()) {
        if (auto callOp = mlir::dyn_cast<mlir::func::CallOp>(value.getDefiningOp())) {
          if (auto callee = module.lookupSymbol<mlir::func::FuncOp>(callOp.getCallee())) {
            auto funcType = callee.getFunctionType();
            if (funcType.getNumResults() > 0)
              dropFn = dropFuncForMLIRType(funcType.getResult(0));
          }
        }
      }
      if (!dropFn.empty())
        registerDroppable(varName, dropFn);
    }
  }

  // ── User-defined Drop from struct init ─────────────────────────────
  if (stmtValue && *stmtValue) {
    if (auto *si = std::get_if<ast::ExprStructInit>(&(*stmtValue)->value.kind)) {
      bool hasTypedAnnotation =
          stmtTy && *stmtTy && std::holds_alternative<ast::TypeNamed>((*stmtTy)->value.kind);
      if (!hasTypedAnnotation) {
        auto dropIt = userDropFuncs.find(si->name);
        if (dropIt != userDropFuncs.end())
          registerDroppable(varName, dropIt->second, /*isUserDrop=*/true);
      }
    }
  }

  // ── Auto-field-drop for user structs with owned fields (no Drop impl) ──
  // Covers all registered user-defined structs, not just wire structs: any
  // struct with owned fields (String, Vec, nested Drop'd struct, etc.) that
  // lacks a user-written Drop impl must have its fields freed automatically.
  if (value && !dropScopes.empty()) {
    auto structTy = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(value.getType());
    if (structTy && structTy.isIdentified() && !userDropFuncs.count(structTy.getName().str()) &&
        structTypes.count(structTy.getName().str())) {
      bool needsFieldDrop = structHasOwnedFields(structTy.getName().str());
      if (needsFieldDrop) {
        bool isOwnedStruct = false;
        if (stmtValue && *stmtValue) {
          const auto &vk = (*stmtValue)->value.kind;
          isOwnedStruct = std::holds_alternative<ast::ExprStructInit>(vk) ||
                          std::holds_alternative<ast::ExprCall>(vk) ||
                          std::holds_alternative<ast::ExprMethodCall>(vk);
          if (auto *ue = std::get_if<ast::ExprUnsafe>(&vk)) {
            if (ue->block.trailing_expr) {
              const auto &inner = ue->block.trailing_expr->value.kind;
              isOwnedStruct = std::holds_alternative<ast::ExprCall>(inner) ||
                              std::holds_alternative<ast::ExprMethodCall>(inner) ||
                              std::holds_alternative<ast::ExprStructInit>(inner);
            }
          }
        }
        bool alreadyReg = false;
        for (auto &e : dropScopes.back())
          if (e.varName == varName) {
            alreadyReg = true;
            break;
          }
        if (isOwnedStruct && !alreadyReg) {
          registerDroppable(varName, "__auto_field_drop");
          // If a return guard is active (returnFlag non-null) and the struct
          // value may be defined inside a guard block (i.e. declareVariable
          // didn't promote it to mutableVars), the raw SSA value might not
          // dominate the drop point.  Create a hoisted alloca and store the
          // value now so emitDropEntry can load from it regardless of where
          // the drop eventually fires.
          auto &newEntry = dropScopes.back().back();
          if (!newEntry.promotedSlot && returnFlag) {
            auto storageType = toSlotStorageType(value.getType());
            auto alloca = createHoistedAlloca(storageType, value.getType());
            auto storedValue = coerceType(value, storageType, builder.getUnknownLoc());
            if (!storedValue)
              return;
            mlir::memref::StoreOp::create(builder, builder.getUnknownLoc(), storedValue, alloca);
            if (storageType != value.getType())
              slotSemanticTypes[alloca] = value.getType();
            newEntry.promotedSlot = alloca;
          }
        }
      }
    }
  }

  // ── Closure env RAII cleanup ───────────────────────────────────────
  if (value && mlir::isa<hew::ClosureType>(value.getType())) {
    registerDroppable(varName, "hew_rc_drop");
  }

  // ── Resolved-type catch-all for Rc drop ──────────────────────────
  // When an Rc value reaches a let binding through an indirect expression
  // (block expression, if/else, match, etc.) the specific-case sections
  // above may not recognise the value as owned Rc.  Use the type-checker's
  // resolved type as a final safety net: if the binding is Rc-typed and no
  // drop has been registered yet, register hew_rc_drop now.
  if (value && mlir::isa<mlir::LLVM::LLVMPointerType>(value.getType())) {
    bool rcDropRegistered = false;
    if (!dropScopes.empty()) {
      for (auto &e : dropScopes.back())
        if (e.varName == varName) {
          rcDropRegistered = true;
          break;
        }
    }
    if (!rcDropRegistered) {
      bool isRcByResolved = false;
      // Check the RHS expression's resolved type
      if (stmtValue && *stmtValue) {
        if (auto *rt = resolvedTypeOf((*stmtValue)->span)) {
          if (auto *named = std::get_if<ast::TypeNamed>(&rt->kind))
            isRcByResolved = (named->name == "Rc");
        }
      }
      // Also check the type annotation (e.g. `let x: Rc<int> = ...`)
      if (!isRcByResolved && stmtTy && *stmtTy) {
        if (auto *named = std::get_if<ast::TypeNamed>(&(*stmtTy)->value.kind))
          isRcByResolved = resolveTypeAlias(named->name) == "Rc";
      }
      if (isRcByResolved)
        registerDroppable(varName, "hew_rc_drop");
    }
  }

  // ── dyn Trait variable type tracking ───────────────────────────────
  if (stmtTy && *stmtTy) {
    if (auto *traitObj = std::get_if<ast::TypeTraitObject>(&(*stmtTy)->value.kind)) {
      if (!traitObj->bounds.empty())
        dynTraitVarTypes[varName] = traitObj->bounds[0].name;
    }
  }
}

void MLIRGen::generateAssignStmt(const ast::StmtAssign &stmt) {
  auto location = currentLoc;

  // Look up the checker-assigned kind fail-closed: if the checker rejected
  // this target no entry is present, and we must not proceed.
  const auto *kindEntry = requireAssignTargetKindOf(stmt.target.span, "assignment", location);
  if (!kindEntry)
    return;

  // Early authority mismatch checks for identifier assignments: fire the
  // mismatch diagnostic before requiring the shape entry so the real error is
  // reported rather than a spurious "missing assign_target_shapes" message.
  if (std::holds_alternative<ast::AssignTargetKindActorField>(kindEntry->kind) &&
      currentActorName.empty()) {
    ++errorCount_;
    emitError(location) << "assign_target_kinds says ActorField but assignment is not inside an "
                           "actor body";
    return;
  }
  if (std::holds_alternative<ast::AssignTargetKindLocalVar>(kindEntry->kind)) {
    if (auto *ti = std::get_if<ast::ExprIdentifier>(&stmt.target.value.kind)) {
      if (!lookupVariable(ti->name)) {
        ++errorCount_;
        emitError(location) << "assign_target_kinds says LocalVar but no local binding named '"
                            << ti->name << "' is available for assignment";
        return;
      }
    }
  }

  // Look up the checker-assigned type-shape fail-closed: the shape entry
  // carries the signedness flag for compound-assignment arithmetic so that
  // MLIR lowering does not need to re-derive it from the AST or fall back to
  // the unreliable `resolvedTypeOf` path (which can be absent).
  const auto *shapeEntry = requireAssignTargetShapeOf(stmt.target.span, "assignment", location);
  if (!shapeEntry)
    return;
  const bool targetIsUnsigned = shapeEntry->is_unsigned;

  auto coerceAssignedValue = [&](mlir::Value value, mlir::Type targetType) -> mlir::Value {
    const bool isUnsigned = mlir::isa<mlir::IntegerType>(targetType) && targetIsUnsigned;
    return coerceType(value, targetType, location, isUnsigned);
  };

  // Handle field assignment: self.field = value (pointer-based)
  if (std::holds_alternative<ast::AssignTargetKindFieldAccess>(kindEntry->kind)) {
    auto *fa = std::get_if<ast::ExprFieldAccess>(&stmt.target.value.kind);
    if (!fa) {
      ++errorCount_;
      emitError(location)
          << "assign_target_kinds says FieldAccess but target is not ExprFieldAccess";
      return;
    }
    auto operandVal = generateExpression(fa->object->value);
    if (!operandVal)
      return;

    mlir::Value rhs = generateExpression(stmt.value.value);
    if (!rhs)
      return;

    auto operandType = operandVal.getType();
    if (isPointerLikeType(operandType)) {
      auto fieldName = fa->field;
      std::string targetStructName;
      for (const auto &[typeName, stInfo] : structTypes) {
        if (!targetStructName.empty() && typeName != targetStructName)
          continue;
        auto structType = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(stInfo.mlirType);
        if (!structType)
          continue;
        for (const auto &field : stInfo.fields) {
          if (field.name == fieldName) {
            auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
            auto fieldPtr = mlir::LLVM::GEPOp::create(
                builder, location, ptrType, structType, operandVal,
                llvm::ArrayRef<mlir::LLVM::GEPArg>{0, static_cast<int32_t>(field.index)});
            // Handle compound assignment
            if (stmt.op) {
              auto current = mlir::LLVM::LoadOp::create(builder, location, field.type, fieldPtr);
              rhs = coerceAssignedValue(rhs, field.type);
              bool isFloat = llvm::isa<mlir::FloatType>(field.type);
              bool isUnsigned = mlir::isa<mlir::IntegerType>(field.type) && targetIsUnsigned;
              rhs = emitCompoundArithOp(*stmt.op, current, rhs, isFloat, isUnsigned, location);
              if (!rhs)
                return;
            }
            rhs = coerceAssignedValue(rhs, field.type);
            mlir::LLVM::StoreOp::create(builder, location, rhs, fieldPtr);
            return;
          }
        }
      }
      ++errorCount_;
      emitError(location)
          << "checker invariant violated: field assignment target is missing field '" << fieldName
          << "'";
      return;
    }
    // Value struct field assignment: load struct from mutable var, insertvalue, store back.
    auto *objIdent = std::get_if<ast::ExprIdentifier>(&fa->object->value.kind);
    if (!objIdent) {
      ++errorCount_;
      emitError(location) << "value struct field assignment requires a variable target";
      return;
    }
    auto varSlot = getMutableVarSlot(intern(objIdent->name));
    if (!varSlot) {
      ++errorCount_;
      emitError(location) << "checker invariant violated: immutable field assignment reached "
                             "MLIRGen for '"
                          << objIdent->name << "'";
      return;
    }

    auto fieldName = fa->field;
    auto structType = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(operandType);
    if (!structType || !structType.isIdentified()) {
      ++errorCount_;
      emitError(location)
          << "checker invariant violated: non-struct value field assignment reached MLIRGen";
      return;
    }
    auto stIt = structTypes.find(structType.getName().str());
    if (stIt == structTypes.end()) {
      ++errorCount_;
      emitError(location) << "unknown struct type '" << structType.getName() << "'";
      return;
    }
    const auto &stInfo = stIt->second;
    const StructFieldInfo *targetField = nullptr;
    for (const auto &field : stInfo.fields) {
      if (field.name == fieldName) {
        targetField = &field;
        break;
      }
    }
    if (!targetField) {
      ++errorCount_;
      emitError(location)
          << "checker invariant violated: field assignment target is missing field '" << fieldName
          << "' on struct '" << structType.getName() << "'";
      return;
    }

    // Reload the struct from the mutable variable slot
    auto currentStruct = mlir::memref::LoadOp::create(builder, location, varSlot);
    // Handle compound assignment
    if (stmt.op) {
      auto currentFieldVal = mlir::LLVM::ExtractValueOp::create(
          builder, location, currentStruct,
          llvm::ArrayRef<int64_t>{static_cast<int64_t>(targetField->index)});
      rhs = coerceAssignedValue(rhs, targetField->type);
      bool isFloat = llvm::isa<mlir::FloatType>(targetField->type);
      bool isUnsigned = mlir::isa<mlir::IntegerType>(targetField->type) && targetIsUnsigned;
      rhs = emitCompoundArithOp(*stmt.op, currentFieldVal, rhs, isFloat, isUnsigned, location);
      if (!rhs)
        return;
    }
    rhs = coerceAssignedValue(rhs, targetField->type);
    auto updated = mlir::LLVM::InsertValueOp::create(
        builder, location, currentStruct, rhs,
        llvm::ArrayRef<int64_t>{static_cast<int64_t>(targetField->index)});
    mlir::memref::StoreOp::create(builder, location, updated, varSlot);
    return;
  }

  // Handle indexed assignment: v[i] = x
  if (std::holds_alternative<ast::AssignTargetKindIndex>(kindEntry->kind)) {
    auto *idx = std::get_if<ast::ExprIndex>(&stmt.target.value.kind);
    if (!idx) {
      ++errorCount_;
      emitError(location) << "assign_target_kinds says Index but target is not ExprIndex";
      return;
    }
    auto collectionVal = generateExpression(idx->object->value);
    auto indexVal = generateExpression(idx->index->value);
    mlir::Value rhsVal = generateExpression(stmt.value.value);
    if (!collectionVal || !indexVal || !rhsVal)
      return;

    auto i64Type = builder.getI64Type();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
    if (indexVal.getType() != i64Type)
      indexVal = mlir::arith::ExtSIOp::create(builder, location, i64Type, indexVal);

    if (auto vecType = mlir::dyn_cast<hew::VecType>(collectionVal.getType())) {
      rhsVal = coerceAssignedValue(rhsVal, vecType.getElementType());
      if (stmt.op) {
        auto currentVal = hew::VecGetOp::create(builder, location, vecType.getElementType(),
                                                collectionVal, indexVal);
        bool isFloat = llvm::isa<mlir::FloatType>(vecType.getElementType());
        bool isUnsigned =
            mlir::isa<mlir::IntegerType>(vecType.getElementType()) && targetIsUnsigned;
        rhsVal = emitCompoundArithOp(*stmt.op, currentVal, rhsVal, isFloat, isUnsigned, location);
        if (!rhsVal)
          return;
      }
      hew::VecSetOp::create(builder, location, collectionVal, indexVal, rhsVal);
      return;
    }

    if (auto hewArrayType = mlir::dyn_cast<hew::HewArrayType>(collectionVal.getType())) {
      auto *ie = std::get_if<ast::ExprIdentifier>(&idx->object->value.kind);
      if (!ie) {
        ++errorCount_;
        emitError(location) << "array indexed assignment requires a variable target";
        return;
      }
      auto varSlot = getMutableVarSlot(intern(ie->name));
      if (!varSlot) {
        ++errorCount_;
        emitError(location) << "checker invariant violated: immutable array indexed assignment "
                               "reached MLIRGen for '"
                            << ie->name << "'";
        return;
      }

      rhsVal = coerceAssignedValue(rhsVal, hewArrayType.getElementType());
      auto llvmArrayType =
          mlir::LLVM::LLVMArrayType::get(hewArrayType.getElementType(), hewArrayType.getSize());
      auto llvmArray = hew::BitcastOp::create(builder, location, llvmArrayType, collectionVal);
      auto one = mlir::arith::ConstantIntOp::create(builder, location, 1, 64);
      auto alloca =
          mlir::LLVM::AllocaOp::create(builder, location, ptrType, llvmArrayType, one.getResult());
      mlir::LLVM::StoreOp::create(builder, location, llvmArray, alloca);
      auto zero = mlir::arith::ConstantIntOp::create(builder, location, 0, 64);
      auto elemPtr = mlir::LLVM::GEPOp::create(builder, location, ptrType, llvmArrayType, alloca,
                                               mlir::ValueRange{zero.getResult(), indexVal});
      if (stmt.op) {
        auto currentVal =
            mlir::LLVM::LoadOp::create(builder, location, hewArrayType.getElementType(), elemPtr);
        bool isFloat = llvm::isa<mlir::FloatType>(hewArrayType.getElementType());
        bool isUnsigned =
            mlir::isa<mlir::IntegerType>(hewArrayType.getElementType()) && targetIsUnsigned;
        rhsVal = emitCompoundArithOp(*stmt.op, currentVal, rhsVal, isFloat, isUnsigned, location);
        if (!rhsVal)
          return;
      }
      mlir::LLVM::StoreOp::create(builder, location, rhsVal, elemPtr);
      auto updatedArray = mlir::LLVM::LoadOp::create(builder, location, llvmArrayType, alloca);
      auto updatedHewArray =
          hew::BitcastOp::create(builder, location, hewArrayType, updatedArray.getResult());
      storeVariable(ie->name, updatedHewArray);
      return;
    }

    ++errorCount_;
    emitError(location) << "unsupported indexed assignment target";
    return;
  }

  // LocalVar or ActorField — both require an ExprIdentifier target.
  auto *targetIdent = std::get_if<ast::ExprIdentifier>(&stmt.target.value.kind);
  if (!targetIdent) {
    ++errorCount_;
    emitError(location) << "assign_target_kinds says Identifier but target is not ExprIdentifier";
    return;
  }

  const bool expectsLocalVar =
      std::holds_alternative<ast::AssignTargetKindLocalVar>(kindEntry->kind);
  const bool expectsActorField =
      std::holds_alternative<ast::AssignTargetKindActorField>(kindEntry->kind);
  if (!expectsLocalVar && !expectsActorField) {
    ++errorCount_;
    emitError(location)
        << "assign_target_kinds says non-identifier kind but target is ExprIdentifier";
    return;
  }

  auto name = targetIdent->name;

  mlir::Value rhs = generateExpression(stmt.value.value);
  if (!rhs)
    return;

  // The checker-owned assign_target_kinds metadata is authoritative for bare
  // identifier assignments; do not fall through between local and actor-field
  // storage based on runtime lookup alone.
  if (expectsLocalVar) {
    auto existingVar = lookupVariable(name);
    if (!existingVar) {
      ++errorCount_;
      emitError(location) << "assign_target_kinds says LocalVar but no local binding named '"
                          << name << "' is available for assignment";
      return;
    }

    // Assign to local variable
    if (stmt.op) {
      rhs = coerceAssignedValue(rhs, existingVar.getType());
      auto type = existingVar.getType();
      bool isFloat = llvm::isa<mlir::FloatType>(type);
      bool isUnsigned = mlir::isa<mlir::IntegerType>(type) && targetIsUnsigned;

      auto result = emitCompoundArithOp(*stmt.op, existingVar, rhs, isFloat, isUnsigned, location);
      if (!result)
        return;
      storeVariable(name, result);
    } else {
      rhs = coerceAssignedValue(rhs, existingVar.getType());
      // Drop old owned value before overwriting to prevent memory leaks.
      // Only safe when RHS is a fresh allocation (not loaded from another
      // variable), to avoid double-free from shared ownership.
      if (rhs && !rhs.getDefiningOp<mlir::memref::LoadOp>() && !mlir::isa<mlir::BlockArgument>(rhs))
        emitDropForVariable(name);
      storeVariable(name, rhs);
    }
    return;
  }

  if (currentActorName.empty()) {
    ++errorCount_;
    emitError(location) << "assign_target_kinds says ActorField but assignment is not inside an "
                           "actor body";
    return;
  }

  auto selfVal = lookupVariable("self");
  if (!selfVal) {
    ++errorCount_;
    emitError(location) << "assign_target_kinds says ActorField but `self` is unavailable";
    return;
  }

  auto actorIt = structTypes.find(currentActorName);
  if (actorIt == structTypes.end()) {
    ++errorCount_;
    emitError(location) << "assign_target_kinds says ActorField but actor state type '"
                        << currentActorName << "' is not registered";
    return;
  }

  auto structType = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(actorIt->second.mlirType);
  if (!structType) {
    ++errorCount_;
    emitError(location) << "assign_target_kinds says ActorField but actor state type '"
                        << currentActorName << "' is not an LLVM struct";
    return;
  }

  for (const auto &field : actorIt->second.fields) {
    if (field.name == name) {
      auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
      auto fieldPtr = mlir::LLVM::GEPOp::create(
          builder, location, ptrType, structType, selfVal,
          llvm::ArrayRef<mlir::LLVM::GEPArg>{0, static_cast<int32_t>(field.index)});
      if (stmt.op) {
        auto current =
            mlir::LLVM::LoadOp::create(builder, location, field.type, fieldPtr).getResult();
        rhs = coerceAssignedValue(rhs, field.type);
        bool isFloat = llvm::isa<mlir::FloatType>(field.type);
        bool isUnsigned = mlir::isa<mlir::IntegerType>(field.type) && targetIsUnsigned;
        rhs = emitCompoundArithOp(*stmt.op, current, rhs, isFloat, isUnsigned, location);
        if (!rhs)
          return;
      }
      rhs = coerceAssignedValue(rhs, field.type);
      mlir::LLVM::StoreOp::create(builder, location, rhs, fieldPtr);
      // Ownership transfer: the RHS is now owned by the actor
      // state.  Unregister any drop for the source variable so
      // the handler exit doesn't free the actor's field.
      if (auto *rhsIdent = std::get_if<ast::ExprIdentifier>(&stmt.value.value.kind))
        unregisterDroppable(rhsIdent->name);
      return;
    }
  }

  ++errorCount_;
  emitError(location) << "assign_target_kinds says ActorField but no actor field named '" << name
                      << "' exists on '" << currentActorName << "'";
}

void MLIRGen::generateIfStmt(const ast::StmtIf &stmt) {
  auto location = currentLoc;
  auto materializeBlockTemporary = [&](const ast::Block &block, mlir::Value value) {
    if (!value)
      return;
    if (block.trailing_expr) {
      materializeTemporary(value, block.trailing_expr->value);
      return;
    }
    if (!block.stmts.empty()) {
      if (auto *exprStmt = std::get_if<ast::StmtExpression>(&block.stmts.back()->value.kind))
        materializeTemporary(value, exprStmt->expr.value);
    }
  };

  mlir::Value cond = generateExpression(stmt.condition.value);
  if (!cond)
    return;

  if (cond.getType() != builder.getI1Type()) {
    auto zero = createIntConstant(builder, location, cond.getType(), 0);
    cond =
        mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::ne, cond, zero);
  }

  bool hasElse = stmt.else_block.has_value();

  auto ifOp =
      mlir::scf::IfOp::create(builder, location, /*resultTypes=*/mlir::TypeRange{}, cond, hasElse);

  builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
  materializeBlockTemporary(stmt.then_block,
                            generateBlock(stmt.then_block, /*statementPosition=*/true));
  ensureYieldTerminator(location);

  if (hasElse) {
    builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
    const auto &elseBlock = *stmt.else_block;
    if (elseBlock.is_if && elseBlock.if_stmt) {
      if (auto *innerIf = std::get_if<ast::StmtIf>(&elseBlock.if_stmt->value.kind))
        generateIfStmt(*innerIf);
    } else if (elseBlock.block) {
      materializeBlockTemporary(*elseBlock.block,
                                generateBlock(*elseBlock.block, /*statementPosition=*/true));
    }
    ensureYieldTerminator(location);
  }

  builder.setInsertionPointAfter(ifOp);
}

// ============================================================================
// If statement as expression (value-producing if at end of block)
// ============================================================================

mlir::Value MLIRGen::generateIfStmtAsExpr(const ast::StmtIf &stmt, bool statementPosition) {
  if (statementPosition) {
    generateIfStmt(stmt);
    return nullptr;
  }

  auto location = currentLoc;

  mlir::Value cond = generateExpression(stmt.condition.value);
  if (!cond)
    return nullptr;

  if (cond.getType() != builder.getI1Type()) {
    auto zero = createIntConstant(builder, location, cond.getType(), 0);
    cond =
        mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::ne, cond, zero);
  }

  mlir::Type resultType;
  if (currentFunction && currentFunction.getResultTypes().size() == 1) {
    resultType = currentFunction.getResultTypes()[0];
  } else {
    // Expression-position callers without a single-result function sink still
    // need a concrete result type for the value-producing scf.if.
    resultType = defaultIntType();
  }

  bool hasElse = stmt.else_block.has_value();
  if (!hasElse) {
    generateIfStmt(stmt);
    return nullptr;
  }

  auto hasUnsignedBlockResult = [&](const ast::Block &block) {
    if (!mlir::isa<mlir::IntegerType>(resultType))
      return false;

    auto isUnsignedExprResult = [&](const ast::Expr &expr) {
      if (auto *typeExpr = resolvedTypeOf(expr.span))
        return isUnsignedTypeExpr(*typeExpr);
      return false;
    };

    if (block.trailing_expr)
      return isUnsignedExprResult(block.trailing_expr->value);

    if (block.stmts.empty())
      return false;

    if (auto *exprStmt = std::get_if<ast::StmtExpression>(&block.stmts.back()->value.kind))
      return isUnsignedExprResult(exprStmt->expr.value);

    return false;
  };

  auto ifOp = mlir::scf::IfOp::create(builder, location, resultType, cond, /*withElseRegion=*/true);

  builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
  mlir::Value thenVal = generateBlock(stmt.then_block);
  bool thenIsUnsigned = hasUnsignedBlockResult(stmt.then_block);
  auto *thenBlock = builder.getInsertionBlock();
  if (thenBlock->empty() || !thenBlock->back().hasTrait<mlir::OpTrait::IsTerminator>()) {
    if (thenVal) {
      thenVal = coerceType(thenVal, resultType, location, thenIsUnsigned);
      if (!thenVal)
        return nullptr;
      mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{thenVal});
    } else {
      auto defVal = createDefaultValue(builder, location, resultType);
      mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{defVal});
    }
  }

  builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
  mlir::Value elseVal = nullptr;
  const auto &elseBlock = *stmt.else_block;
  if (elseBlock.is_if && elseBlock.if_stmt) {
    if (auto *innerIf = std::get_if<ast::StmtIf>(&elseBlock.if_stmt->value.kind))
      elseVal = generateIfStmtAsExpr(*innerIf);
  } else if (elseBlock.block) {
    elseVal = generateBlock(*elseBlock.block);
  }
  bool elseIsUnsigned = elseBlock.block && hasUnsignedBlockResult(*elseBlock.block);
  auto *elseBlk = builder.getInsertionBlock();
  if (elseBlk->empty() || !elseBlk->back().hasTrait<mlir::OpTrait::IsTerminator>()) {
    if (elseVal) {
      elseVal = coerceType(elseVal, resultType, location, elseIsUnsigned);
      if (!elseVal)
        return nullptr;
      mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{elseVal});
    } else {
      auto defVal = createDefaultValue(builder, location, resultType);
      mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{defVal});
    }
  }

  builder.setInsertionPointAfter(ifOp);
  return ifOp.getResult(0);
}

mlir::Value MLIRGen::generateDiscardedExpr(const ast::Spanned<ast::Expr> &expr) {
  if (auto *ifExpr = std::get_if<ast::ExprIf>(&expr.value.kind))
    return generateIfExpr(*ifExpr, expr.span, /*statementPosition=*/true);
  if (auto *blockExpr = std::get_if<ast::ExprBlock>(&expr.value.kind))
    return generateBlock(blockExpr->block, /*statementPosition=*/true);
  if (auto *scopeExpr = std::get_if<ast::ExprScope>(&expr.value.kind))
    return generateScopeExpr(*scopeExpr, /*statementPosition=*/true);
  if (auto *unsafeExpr = std::get_if<ast::ExprUnsafe>(&expr.value.kind))
    return generateBlock(unsafeExpr->block, /*statementPosition=*/true);
  return generateExpression(expr.value);
}

// ── Loop-invariant expression detection ──────────────────────────────────────
// Returns true when `expr` only references immutable locals, literals, field
// accesses and method calls on invariant receivers — i.e. values that cannot
// change across loop iterations.
//
// IMPORTANT: method calls are structurally invariant but NOT safe to hoist if
// the receiver is mutated in the loop body (e.g. v.push() inside a
// while i < v.len() loop). The caller must check bodyMutatesVar() separately.

// Extract the root identifier name from a method-call receiver chain.
// e.g. for v.len() returns "v", for a.b.len() returns "a".
static std::optional<std::string> extractReceiverVarName(const ast::Expr &expr) {
  if (auto *ident = std::get_if<ast::ExprIdentifier>(&expr.kind))
    return ident->name;
  if (auto *field = std::get_if<ast::ExprFieldAccess>(&expr.kind))
    return extractReceiverVarName(field->object->value);
  if (auto *method = std::get_if<ast::ExprMethodCall>(&expr.kind))
    return extractReceiverVarName(method->receiver->value);
  return std::nullopt;
}

// Check if an expression tree contains a method call on `varName`.
static bool exprCallsMethodOn(const ast::Expr &expr, const std::string &varName) {
  if (auto *mc = std::get_if<ast::ExprMethodCall>(&expr.kind)) {
    auto recv = extractReceiverVarName(mc->receiver->value);
    if (recv && *recv == varName)
      return true;
    if (exprCallsMethodOn(mc->receiver->value, varName))
      return true;
    for (auto &arg : mc->args) {
      if (auto *p = std::get_if<ast::CallArgPositional>(&arg)) {
        if (exprCallsMethodOn(p->expr->value, varName))
          return true;
      } else if (auto *n = std::get_if<ast::CallArgNamed>(&arg)) {
        if (exprCallsMethodOn(n->value->value, varName))
          return true;
      }
    }
  }
  if (auto *call = std::get_if<ast::ExprCall>(&expr.kind)) {
    for (auto &arg : call->args) {
      if (auto *p = std::get_if<ast::CallArgPositional>(&arg)) {
        if (exprCallsMethodOn(p->expr->value, varName))
          return true;
      }
    }
  }
  if (auto *bin = std::get_if<ast::ExprBinary>(&expr.kind))
    return exprCallsMethodOn(bin->left->value, varName) ||
           exprCallsMethodOn(bin->right->value, varName);
  if (auto *un = std::get_if<ast::ExprUnary>(&expr.kind))
    return exprCallsMethodOn(un->operand->value, varName);
  return false;
}

// Check if a block (loop body) contains any method calls on `varName`,
// or passes `varName` to a function call (potential mutation through alias).
static bool bodyMutatesVar(const ast::Block &block, const std::string &varName);

static bool stmtMutatesVar(const ast::Stmt &stmt, const std::string &varName) {
  if (auto *expr = std::get_if<ast::StmtExpression>(&stmt.kind))
    return exprCallsMethodOn(expr->expr.value, varName);
  if (auto *let_ = std::get_if<ast::StmtLet>(&stmt.kind))
    return let_->value && exprCallsMethodOn(let_->value->value, varName);
  if (auto *var_ = std::get_if<ast::StmtVar>(&stmt.kind))
    return var_->value && exprCallsMethodOn(var_->value->value, varName);
  if (auto *assign = std::get_if<ast::StmtAssign>(&stmt.kind))
    return exprCallsMethodOn(assign->value.value, varName);
  if (auto *if_ = std::get_if<ast::StmtIf>(&stmt.kind)) {
    if (bodyMutatesVar(if_->then_block, varName))
      return true;
    if (if_->else_block && if_->else_block->block &&
        bodyMutatesVar(*if_->else_block->block, varName))
      return true;
    if (if_->else_block && if_->else_block->if_stmt)
      return stmtMutatesVar(if_->else_block->if_stmt->value, varName);
    return false;
  }
  if (auto *for_ = std::get_if<ast::StmtFor>(&stmt.kind))
    return bodyMutatesVar(for_->body, varName);
  if (auto *while_ = std::get_if<ast::StmtWhile>(&stmt.kind))
    return bodyMutatesVar(while_->body, varName);
  if (auto *whileLet = std::get_if<ast::StmtWhileLet>(&stmt.kind))
    return bodyMutatesVar(whileLet->body, varName);
  if (auto *loop_ = std::get_if<ast::StmtLoop>(&stmt.kind))
    return bodyMutatesVar(loop_->body, varName);
  return false;
}

static bool bodyMutatesVar(const ast::Block &block, const std::string &varName) {
  for (auto &s : block.stmts) {
    if (stmtMutatesVar(s->value, varName))
      return true;
  }
  if (block.trailing_expr && exprCallsMethodOn(block.trailing_expr->value, varName))
    return true;
  return false;
}

bool MLIRGen::isExprLoopInvariant(const ast::Expr &expr) {
  if (std::get_if<ast::ExprLiteral>(&expr.kind))
    return true;

  if (auto *ident = std::get_if<ast::ExprIdentifier>(&expr.kind))
    return !mutableVars.lookup(intern(ident->name));

  // Method calls are only invariant if the receiver is immutable AND
  // no method is called on that same receiver inside the loop body
  // (since methods like push/insert can mutate the receiver's contents
  // even though the binding itself is immutable).
  if (auto *method = std::get_if<ast::ExprMethodCall>(&expr.kind)) {
    if (!isExprLoopInvariant(method->receiver->value))
      return false;
    for (const auto &arg : method->args) {
      if (auto *pos = std::get_if<ast::CallArgPositional>(&arg)) {
        if (!isExprLoopInvariant(pos->expr->value))
          return false;
      } else if (auto *named = std::get_if<ast::CallArgNamed>(&arg)) {
        if (!isExprLoopInvariant(named->value->value))
          return false;
      }
    }
    // Requires the caller to have checked the loop body for mutations
    // on this receiver (see bodyMutatesVar in generateWhileStmt).
    return true;
  }

  if (auto *field = std::get_if<ast::ExprFieldAccess>(&expr.kind))
    return isExprLoopInvariant(field->object->value);

  if (auto *binary = std::get_if<ast::ExprBinary>(&expr.kind))
    return isExprLoopInvariant(binary->left->value) && isExprLoopInvariant(binary->right->value);

  if (auto *unary = std::get_if<ast::ExprUnary>(&expr.kind))
    return isExprLoopInvariant(unary->operand->value);

  return false;
}

void MLIRGen::generateWhileStmt(const ast::StmtWhile &stmt) {
  auto location = currentLoc;

  auto lc = pushLoopControl(stmt.label, location);

  // ── Hoist loop-invariant sub-expressions from comparison conditions ──
  // For patterns like `while i < v.len()`, evaluate the invariant side
  // (v.len()) once before the loop so it is not re-evaluated every iteration.
  // SAFETY: Only hoist if the receiver variable is NOT mutated (no method
  // calls on it) anywhere in the loop body.
  const ast::Expr *hoistedExpr = nullptr;
  if (auto *binary = std::get_if<ast::ExprBinary>(&stmt.condition.value.kind)) {
    switch (binary->op) {
    case ast::BinaryOp::Less:
    case ast::BinaryOp::LessEqual:
    case ast::BinaryOp::Greater:
    case ast::BinaryOp::GreaterEqual:
    case ast::BinaryOp::Equal:
    case ast::BinaryOp::NotEqual: {
      // Determine which side (if any) is invariant.
      const ast::Expr *candidate = nullptr;
      if (isExprLoopInvariant(binary->right->value))
        candidate = &binary->right->value;
      else if (isExprLoopInvariant(binary->left->value))
        candidate = &binary->left->value;
      if (!candidate)
        break;

      // If the candidate contains a method call, check that the receiver
      // is not mutated in the loop body.
      auto recvName = extractReceiverVarName(*candidate);
      if (recvName && bodyMutatesVar(stmt.body, *recvName)) {
        // Not safe to hoist — receiver is mutated in the body.
        break;
      }

      hoistedExpr = candidate;
      mlir::Value val = generateExpression(*hoistedExpr);
      if (val)
        hoistedValues[hoistedExpr] = val;
      else
        hoistedExpr = nullptr;
      break;
    }
    default:
      break;
    }
  }

  auto whileOp =
      mlir::scf::WhileOp::create(builder, location, mlir::TypeRange{}, mlir::ValueRange{});

  auto *beforeBlock = builder.createBlock(&whileOp.getBefore());
  builder.setInsertionPointToStart(beforeBlock);

  auto isActive =
      mlir::memref::LoadOp::create(builder, location, lc.activeFlag, mlir::ValueRange{});

  mlir::Value cond = generateExpression(stmt.condition.value);
  if (!cond)
    cond = createIntConstant(builder, location, builder.getI1Type(), 0);

  if (cond.getType() != builder.getI1Type()) {
    auto zero = createIntConstant(builder, location, cond.getType(), 0);
    cond =
        mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::ne, cond, zero);
  }

  mlir::Value combinedCond = mlir::arith::AndIOp::create(builder, location, isActive, cond);
  combinedCond = andNotReturned(combinedCond, location);

  mlir::scf::ConditionOp::create(builder, location, combinedCond, mlir::ValueRange{});

  auto *afterBlock = builder.createBlock(&whileOp.getAfter());
  builder.setInsertionPointToStart(afterBlock);
  auto falseVal = createIntConstant(builder, location, builder.getI1Type(), 0);
  mlir::memref::StoreOp::create(builder, location, falseVal, lc.continueFlag);

  {
    SymbolTableScopeT bodyScope(symbolTable);
    MutableTableScopeT bodyMutScope(mutableVars);
    pushDropScope();
    generateLoopBodyWithContinueGuards(stmt.body.stmts, 0, stmt.body.stmts.size(), lc.continueFlag,
                                       location);
    popDropScope();
  }

  ensureYieldTerminator(location);

  popLoopControl(lc, whileOp);

  // Clean up hoisted value so it doesn't leak into subsequent code.
  if (hoistedExpr)
    hoistedValues.erase(hoistedExpr);
}

// ── for await: first-class Stream<T> variable iteration ─────────────────────

// ── for await: cross-actor stream iteration ─────────────────────────────

// ============================================================================
// Loop statement generation
// ============================================================================

void MLIRGen::generateForStreamStmt(const ast::StmtFor &stmt) {
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto i1Type = builder.getI1Type();
  auto i64Type = builder.getI64Type();
  std::string labelName;

  // Determine the stream's element type to select the correct runtime call.
  // "bytes" → hew_stream_next_bytes / hew_vec_free
  // anything else (default) → hew_stream_next / hew_string_drop
  bool isBytesStream = false;
  if (auto streamInfo = resolveStreamHandleInfo(stmt.iterable.value))
    isBytesStream = streamInfo->isBytesStream();

  std::string nextFn = isBytesStream ? "hew_stream_next_bytes" : "hew_stream_next";
  std::string dropFn = isBytesStream ? "hew_vec_free" : "hew_string_drop";

  // Generate the stream pointer expression.
  mlir::Value streamPtr = generateExpression(stmt.iterable.value);
  if (!streamPtr)
    return;

  // Hoist item-pointer alloca before the while op so both before/after regions
  // can access it.  The before-region stores the fetched pointer here; the
  // after-region loads it to bind the loop variable.
  auto one64 = mlir::arith::ConstantIntOp::create(builder, location, i64Type, 1);
  auto itemPtrAlloca = mlir::LLVM::AllocaOp::create(builder, location, ptrType, ptrType, one64);
  auto nullPtrVal = mlir::LLVM::ZeroOp::create(builder, location, ptrType);
  mlir::LLVM::StoreOp::create(builder, location, nullPtrVal, itemPtrAlloca);

  // Loop-control flags (break/continue/return support).
  auto lc = pushLoopControl(stmt.label, location);
  if (stmt.label)
    labelName = *stmt.label;

  auto whileOp =
      mlir::scf::WhileOp::create(builder, location, mlir::TypeRange{}, mlir::ValueRange{});

  // ── Before region: fetch next item ──────────────────────────────
  auto *beforeBlock = builder.createBlock(&whileOp.getBefore());
  builder.setInsertionPointToStart(beforeBlock);

  // Fetch next item using the element-type-appropriate runtime call.
  auto nextAttr = mlir::SymbolRefAttr::get(&context, nextFn);
  auto itemPtr = hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{ptrType}, nextAttr,
                                            mlir::ValueRange{streamPtr})
                     .getResult();

  // Stash the item pointer so the after-region can load it.
  mlir::LLVM::StoreOp::create(builder, location, itemPtr, itemPtrAlloca);

  // Condition: itemPtr != null && active && !returned
  auto notNull = mlir::LLVM::ICmpOp::create(builder, location, mlir::LLVM::ICmpPredicate::ne,
                                            itemPtr, nullPtrVal);
  auto isActive =
      mlir::memref::LoadOp::create(builder, location, lc.activeFlag, mlir::ValueRange{});
  mlir::Value combinedCond = mlir::arith::AndIOp::create(builder, location, notNull, isActive);
  combinedCond = andNotReturned(combinedCond, location);

  mlir::scf::ConditionOp::create(builder, location, combinedCond, mlir::ValueRange{});

  // ── After region: bind loop variable, run body ──────────────────
  auto *afterBlock = builder.createBlock(&whileOp.getAfter());
  builder.setInsertionPointToStart(afterBlock);
  auto falseVal = createIntConstant(builder, location, builder.getI1Type(), 0);
  mlir::memref::StoreOp::create(builder, location, falseVal, lc.continueFlag);

  {
    SymbolTableScopeT bodyScope(symbolTable);
    MutableTableScopeT bodyMutScope(mutableVars);

    // Load the item pointer stashed by the before-region.
    auto currentItemPtr = mlir::LLVM::LoadOp::create(builder, location, ptrType, itemPtrAlloca);

    // Transfer ownership: null out itemPtrAlloca immediately after loading
    // so the loop variable is the sole owner. If break fires, the post-loop
    // cleanup sees null and skips the drop (the loop variable's drop scope
    // already freed it).
    mlir::LLVM::StoreOp::create(builder, location, nullPtrVal, itemPtrAlloca);

    // For bytes streams, bitcast the raw pointer to VecType so that Vec
    // methods (.len(), .push(), etc.) dispatch correctly on the loop variable.
    mlir::Value loopVar = currentItemPtr;
    if (isBytesStream) {
      auto bytesType = hew::VecType::get(&context, builder.getI32Type());
      loopVar = hew::BitcastOp::create(builder, location, bytesType, currentItemPtr);
    }

    std::string loopVarName = "_stream_item";
    if (auto *identPat = std::get_if<ast::PatIdentifier>(&stmt.pattern.value.kind)) {
      loopVarName = identPat->name;
    }
    declareVariable(loopVarName, loopVar);

    pushDropScope();
    registerDroppable(loopVarName, dropFn);
    if (returnFlag) {
      auto flagVal =
          mlir::memref::LoadOp::create(builder, location, returnFlag, mlir::ValueRange{});
      auto trueConst = createIntConstant(builder, location, i1Type, 1);
      auto notReturned = mlir::arith::XOrIOp::create(builder, location, flagVal, trueConst);
      auto guard = mlir::scf::IfOp::create(builder, location, mlir::TypeRange{}, notReturned,
                                           /*withElseRegion=*/false);
      builder.setInsertionPointToStart(&guard.getThenRegion().front());
      pushDropScope();
      generateLoopBodyWithContinueGuards(stmt.body.stmts, 0, stmt.body.stmts.size(),
                                         lc.continueFlag, location);
      popDropScope();
      ensureYieldTerminator(location);
      builder.setInsertionPointAfter(guard);
    } else {
      pushDropScope();
      generateLoopBodyWithContinueGuards(stmt.body.stmts, 0, stmt.body.stmts.size(),
                                         lc.continueFlag, location);
      popDropScope();
    }
    popDropScope();
  }

  ensureYieldTerminator(location);

  popLoopControl(lc, whileOp);

  // Free the last fetched item if non-null (handles break case where
  // hew_stream_next returns one more item after break is signaled).
  auto lastItem = mlir::LLVM::LoadOp::create(builder, location, ptrType, itemPtrAlloca);
  auto isNotNull = mlir::LLVM::ICmpOp::create(builder, location, mlir::LLVM::ICmpPredicate::ne,
                                              lastItem, nullPtrVal);
  auto cleanupIf = mlir::scf::IfOp::create(builder, location, mlir::TypeRange{}, isNotNull,
                                           /*withElseRegion=*/false);
  builder.setInsertionPointToStart(&cleanupIf.getThenRegion().front());
  auto dropAttr = mlir::SymbolRefAttr::get(&context, dropFn);
  hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{}, dropAttr,
                             mlir::ValueRange{lastItem});
  // scf.if auto-adds yield; set insertion after guard
  builder.setInsertionPointAfter(cleanupIf);

  // Close the stream if it was an inline expression (e.g. `raw.lines()`).
  // Named stream variables are managed by the user via explicit `.close()`.
  bool isInlineStream = !std::get_if<ast::ExprIdentifier>(&stmt.iterable.value.kind);
  if (isInlineStream) {
    auto closeAttr = mlir::SymbolRefAttr::get(&context, "hew_stream_close");
    hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{}, closeAttr,
                               mlir::ValueRange{streamPtr});
  }
}

// ── for await: channel Receiver<T> iteration ────────────────────────────
//
// Lowers `for await item in rx { body }` into a loop that calls
// hew_channel_recv (string) or hew_channel_recv_int (int) on each
// iteration, breaking when the channel is closed.
void MLIRGen::generateForReceiverStmt(const ast::StmtFor &stmt,
                                      const ast::TypeNamed *receiverType) {
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto i1Type = builder.getI1Type();
  auto i32Type = builder.getI32Type();
  auto i64Type = builder.getI64Type();

  // Determine element type: int or string.
  if (!receiverType || !receiverType->type_args || receiverType->type_args->empty()) {
    ++errorCount_;
    emitError(location) << "for await on Receiver<T> requires a resolved element type; bare "
                           "Receiver is not supported";
    return;
  }

  auto *inner = std::get_if<ast::TypeNamed>(&(*receiverType->type_args)[0].value.kind);
  if (!inner) {
    ++errorCount_;
    emitError(location) << "for await on Receiver<T> requires a resolved named element type";
    return;
  }

  auto innerName = resolveTypeAlias(inner->name);
  auto canonicalInnerName = canonicalPrimitiveTypeName(innerName);
  bool isIntChannel = (canonicalInnerName == "i64");
  bool isStringChannel = !isIntChannel && (canonicalInnerName == "string");

  if (!isIntChannel && !isStringChannel) {
    ++errorCount_;
    emitError(location)
        << "for await on Receiver<T> is currently only supported for String and int";
    return;
  }

  // Generate the receiver pointer expression.
  mlir::Value rxPtr = generateExpression(stmt.iterable.value);
  if (!rxPtr)
    return;

  // Loop-control flags (break/continue/return support).
  auto lc = pushLoopControl(stmt.label, location);

  if (isIntChannel) {
    // ── Int channel: recv_int(rx, &out_valid) → i64 ────────────────
    // Hoist or reuse the shared out_valid alloca.
    if (!channelIntOutValidAlloca) {
      auto savedIP = builder.saveInsertionPoint();
      auto &entryBlock = currentFunction.front();
      builder.setInsertionPointToStart(&entryBlock);
      auto one = mlir::arith::ConstantIntOp::create(builder, builder.getUnknownLoc(), 1, 64);
      channelIntOutValidAlloca =
          mlir::LLVM::AllocaOp::create(builder, builder.getUnknownLoc(), ptrType, i32Type, one);
      builder.restoreInsertionPoint(savedIP);
    }

    // Alloca to pass the received value from before-region to after-region.
    auto one64 = mlir::arith::ConstantIntOp::create(builder, location, i64Type, 1);
    auto itemAlloca = mlir::LLVM::AllocaOp::create(builder, location, ptrType, i64Type, one64);

    auto whileOp =
        mlir::scf::WhileOp::create(builder, location, mlir::TypeRange{}, mlir::ValueRange{});

    // ── Before region: recv and check validity ──
    auto *beforeBlock = builder.createBlock(&whileOp.getBefore());
    builder.setInsertionPointToStart(beforeBlock);

    // Check isActive (and !returnFlag) before calling recv to avoid
    // blocking after break or return.
    auto isActive =
        mlir::memref::LoadOp::create(builder, location, lc.activeFlag, mlir::ValueRange{});
    mlir::Value shouldRecv = isActive;
    if (returnFlag) {
      auto flagVal =
          mlir::memref::LoadOp::create(builder, location, returnFlag, mlir::ValueRange{});
      auto trueConst = createIntConstant(builder, location, i1Type, 1);
      auto notReturned = mlir::arith::XOrIOp::create(builder, location, flagVal, trueConst);
      shouldRecv = mlir::arith::AndIOp::create(builder, location, isActive, notReturned);
    }
    auto recvGuard = mlir::scf::IfOp::create(builder, location, mlir::TypeRange{}, shouldRecv,
                                             /*withElseRegion=*/false);
    builder.setInsertionPointToStart(&recvGuard.getThenRegion().front());

    auto externFuncType = mlir::FunctionType::get(&context, {ptrType, ptrType}, {i64Type});
    getOrCreateExternFunc("hew_channel_recv_int", externFuncType);
    auto calleeAttr = mlir::SymbolRefAttr::get(&context, "hew_channel_recv_int");
    auto rawVal =
        hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{i64Type}, calleeAttr,
                                   mlir::ValueRange{rxPtr, channelIntOutValidAlloca})
            .getResult();
    mlir::LLVM::StoreOp::create(builder, location, rawVal, itemAlloca);

    ensureYieldTerminator(location);
    builder.setInsertionPointAfter(recvGuard);

    auto validFlag =
        mlir::LLVM::LoadOp::create(builder, location, i32Type, channelIntOutValidAlloca);
    auto zero32 = mlir::arith::ConstantIntOp::create(builder, location, 0, 32);
    auto isValid = mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::ne,
                                               validFlag, zero32);
    mlir::Value combinedCond = mlir::arith::AndIOp::create(builder, location, isValid, isActive);
    combinedCond = andNotReturned(combinedCond, location);

    mlir::scf::ConditionOp::create(builder, location, combinedCond, mlir::ValueRange{});

    // ── After region: bind loop variable, run body ──
    auto *afterBlock = builder.createBlock(&whileOp.getAfter());
    builder.setInsertionPointToStart(afterBlock);
    auto falseVal = createIntConstant(builder, location, i1Type, 0);
    mlir::memref::StoreOp::create(builder, location, falseVal, lc.continueFlag);

    {
      SymbolTableScopeT bodyScope(symbolTable);
      MutableTableScopeT bodyMutScope(mutableVars);

      auto currentItem = mlir::LLVM::LoadOp::create(builder, location, i64Type, itemAlloca);

      std::string loopVarName = "_recv_item";
      if (auto *identPat = std::get_if<ast::PatIdentifier>(&stmt.pattern.value.kind))
        loopVarName = identPat->name;
      declareVariable(loopVarName, currentItem);

      if (returnFlag) {
        auto flagVal =
            mlir::memref::LoadOp::create(builder, location, returnFlag, mlir::ValueRange{});
        auto trueConst = createIntConstant(builder, location, i1Type, 1);
        auto notReturned = mlir::arith::XOrIOp::create(builder, location, flagVal, trueConst);
        auto guard = mlir::scf::IfOp::create(builder, location, mlir::TypeRange{}, notReturned,
                                             /*withElseRegion=*/false);
        builder.setInsertionPointToStart(&guard.getThenRegion().front());
        pushDropScope();
        generateLoopBodyWithContinueGuards(stmt.body.stmts, 0, stmt.body.stmts.size(),
                                           lc.continueFlag, location);
        popDropScope();
        ensureYieldTerminator(location);
        builder.setInsertionPointAfter(guard);
      } else {
        pushDropScope();
        generateLoopBodyWithContinueGuards(stmt.body.stmts, 0, stmt.body.stmts.size(),
                                           lc.continueFlag, location);
        popDropScope();
      }
    }

    ensureYieldTerminator(location);
    popLoopControl(lc, whileOp);

  } else {
    // ── String channel: recv(rx) → ptr (NULL = closed) ─────────────
    // Nearly identical to generateForStreamStmt but calls hew_channel_recv.
    auto one64 = mlir::arith::ConstantIntOp::create(builder, location, i64Type, 1);
    auto itemPtrAlloca = mlir::LLVM::AllocaOp::create(builder, location, ptrType, ptrType, one64);
    auto nullPtrVal = mlir::LLVM::ZeroOp::create(builder, location, ptrType);
    mlir::LLVM::StoreOp::create(builder, location, nullPtrVal, itemPtrAlloca);

    auto whileOp =
        mlir::scf::WhileOp::create(builder, location, mlir::TypeRange{}, mlir::ValueRange{});

    // ── Before region: recv and check for NULL ──
    auto *beforeBlock = builder.createBlock(&whileOp.getBefore());
    builder.setInsertionPointToStart(beforeBlock);

    // Check isActive (and !returnFlag) before calling recv to avoid
    // blocking after break or return.
    auto isActive =
        mlir::memref::LoadOp::create(builder, location, lc.activeFlag, mlir::ValueRange{});
    mlir::Value shouldRecv = isActive;
    if (returnFlag) {
      auto flagVal =
          mlir::memref::LoadOp::create(builder, location, returnFlag, mlir::ValueRange{});
      auto trueConst = createIntConstant(builder, location, i1Type, 1);
      auto notReturned = mlir::arith::XOrIOp::create(builder, location, flagVal, trueConst);
      shouldRecv = mlir::arith::AndIOp::create(builder, location, isActive, notReturned);
    }
    auto recvGuard = mlir::scf::IfOp::create(builder, location, mlir::TypeRange{}, shouldRecv,
                                             /*withElseRegion=*/false);
    builder.setInsertionPointToStart(&recvGuard.getThenRegion().front());

    auto externFuncType = mlir::FunctionType::get(&context, {ptrType}, {ptrType});
    getOrCreateExternFunc("hew_channel_recv", externFuncType);
    auto calleeAttr = mlir::SymbolRefAttr::get(&context, "hew_channel_recv");
    auto itemPtr = hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{ptrType},
                                              calleeAttr, mlir::ValueRange{rxPtr})
                       .getResult();

    mlir::LLVM::StoreOp::create(builder, location, itemPtr, itemPtrAlloca);

    ensureYieldTerminator(location);
    builder.setInsertionPointAfter(recvGuard);

    auto storedItem = mlir::LLVM::LoadOp::create(builder, location, ptrType, itemPtrAlloca);
    auto notNull = mlir::LLVM::ICmpOp::create(builder, location, mlir::LLVM::ICmpPredicate::ne,
                                              storedItem, nullPtrVal);
    mlir::Value combinedCond = mlir::arith::AndIOp::create(builder, location, notNull, isActive);
    combinedCond = andNotReturned(combinedCond, location);

    mlir::scf::ConditionOp::create(builder, location, combinedCond, mlir::ValueRange{});

    // ── After region: bind loop variable, run body ──
    auto *afterBlock = builder.createBlock(&whileOp.getAfter());
    builder.setInsertionPointToStart(afterBlock);
    auto falseVal = createIntConstant(builder, location, i1Type, 0);
    mlir::memref::StoreOp::create(builder, location, falseVal, lc.continueFlag);

    {
      SymbolTableScopeT bodyScope(symbolTable);
      MutableTableScopeT bodyMutScope(mutableVars);

      auto currentItemPtr = mlir::LLVM::LoadOp::create(builder, location, ptrType, itemPtrAlloca);

      // Transfer ownership: null out itemPtrAlloca immediately after loading
      // so the loop variable is the sole owner. If break fires, the post-loop
      // cleanup sees null and skips the drop (the loop variable's drop scope
      // already freed it).
      mlir::LLVM::StoreOp::create(builder, location, nullPtrVal, itemPtrAlloca);

      std::string loopVarName = "_recv_item";
      if (auto *identPat = std::get_if<ast::PatIdentifier>(&stmt.pattern.value.kind))
        loopVarName = identPat->name;
      declareVariable(loopVarName, currentItemPtr);

      pushDropScope();
      registerDroppable(loopVarName, "hew_string_drop");
      if (returnFlag) {
        auto flagVal =
            mlir::memref::LoadOp::create(builder, location, returnFlag, mlir::ValueRange{});
        auto trueConst = createIntConstant(builder, location, i1Type, 1);
        auto notReturned = mlir::arith::XOrIOp::create(builder, location, flagVal, trueConst);
        auto guard = mlir::scf::IfOp::create(builder, location, mlir::TypeRange{}, notReturned,
                                             /*withElseRegion=*/false);
        builder.setInsertionPointToStart(&guard.getThenRegion().front());
        pushDropScope();
        generateLoopBodyWithContinueGuards(stmt.body.stmts, 0, stmt.body.stmts.size(),
                                           lc.continueFlag, location);
        popDropScope();
        ensureYieldTerminator(location);
        builder.setInsertionPointAfter(guard);
      } else {
        pushDropScope();
        generateLoopBodyWithContinueGuards(stmt.body.stmts, 0, stmt.body.stmts.size(),
                                           lc.continueFlag, location);
        popDropScope();
      }
      popDropScope();
    }

    ensureYieldTerminator(location);
    popLoopControl(lc, whileOp);

    // Free the last fetched item if non-null (handles break case).
    auto lastItem = mlir::LLVM::LoadOp::create(builder, location, ptrType, itemPtrAlloca);
    auto isNotNull = mlir::LLVM::ICmpOp::create(builder, location, mlir::LLVM::ICmpPredicate::ne,
                                                lastItem, nullPtrVal);
    auto cleanupIf = mlir::scf::IfOp::create(builder, location, mlir::TypeRange{}, isNotNull,
                                             /*withElseRegion=*/false);
    builder.setInsertionPointToStart(&cleanupIf.getThenRegion().front());
    auto dropAttr = mlir::SymbolRefAttr::get(&context, "hew_string_drop");
    hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{}, dropAttr,
                               mlir::ValueRange{lastItem});
    builder.setInsertionPointAfter(cleanupIf);
  }
}

// ── for await: cross-actor stream iteration ─────────────────────────────
void MLIRGen::generateForAwaitStmt(const ast::StmtFor &stmt) {
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto i8Type = builder.getI8Type();
  auto i1Type = builder.getI1Type();

  // First-class streams can arrive from checker metadata, tracked bindings,
  // or known stream-producing/chaining expressions. Method calls only count
  // here when the receiver is already a stream; actor receive-gen methods
  // intentionally fall through to the mailbox path below.
  if (auto streamInfo = resolveStreamHandleInfo(stmt.iterable.value)) {
    if (streamInfo->kind == "Stream") {
      generateForStreamStmt(stmt);
      return;
    }
  }

  // Check if the iterable is a Receiver<T> variable for channel iteration.
  if (auto *identExpr = std::get_if<ast::ExprIdentifier>(&stmt.iterable.value.kind)) {
    auto resolveAliasExpr = [this](llvm::StringRef name) { return resolveTypeAliasExpr(name); };
    if (auto *typeExpr = resolvedTypeOf(stmt.iterable.span)) {
      auto receiverType = classifyResolvedType(*typeExpr, resolveAliasExpr);
      if (receiverType.isReceiver()) {
        generateForReceiverStmt(stmt, receiverType.named);
        return;
      }
    }
    if (auto bindingIdentity = resolveCurrentBindingIdentity(identExpr->name)) {
      auto annotated = annotatedHandleTypes.find(bindingIdentity);
      if (annotated != annotatedHandleTypes.end()) {
        auto receiverType = classifyResolvedType(*annotated->second, resolveAliasExpr);
        if (receiverType.isReceiver()) {
          generateForReceiverStmt(stmt, receiverType.named);
          return;
        }
      }
    }
    // A handle-var entry only tells us that this is some Receiver handle; it
    // does not carry the Receiver<T> element type needed for lowering.
    auto hit = handleVarTypes.find(identExpr->name);
    if (hit != handleVarTypes.end() && isReceiverTypeName(hit->second)) {
      generateForReceiverStmt(stmt, nullptr);
      return;
    }
  }

  // The iterable must be a method call on an actor: actor.method(args)
  auto *mc = std::get_if<ast::ExprMethodCall>(&stmt.iterable.value.kind);
  if (!mc) {
    ++errorCount_;
    emitError(location) << "for await requires an actor method call as iterable";
    return;
  }

  // Resolve the receiver as an actor variable
  if (!mc->receiver) {
    ++errorCount_;
    emitError(location) << "for await: receiver must be an actor variable";
    return;
  }
  auto *recvIdent = std::get_if<ast::ExprIdentifier>(&mc->receiver->value.kind);
  if (!recvIdent) {
    ++errorCount_;
    emitError(location) << "for await: receiver must be an actor variable";
    return;
  }
  std::string receiverName = recvIdent->name;
  std::string methodName = mc->method;

  auto avIt = actorVarTypes.find(receiverName);
  if (avIt == actorVarTypes.end()) {
    ++errorCount_;
    emitError(location) << "for await: '" << receiverName << "' is not a known actor variable";
    return;
  }
  std::string actorTypeName = avIt->second;

  auto arIt = actorRegistry.find(actorTypeName);
  if (arIt == actorRegistry.end()) {
    ++errorCount_;
    emitError(location) << "for await: unknown actor type '" << actorTypeName << "'";
    return;
  }
  const auto &actorInfo = arIt->second;

  // Find the init handler (the generator receive fn)
  int64_t initIdx = -1;
  const ActorReceiveInfo *initInfo = nullptr;
  for (size_t i = 0; i < actorInfo.receiveFns.size(); ++i) {
    if (actorInfo.receiveFns[i].name == methodName) {
      initIdx = static_cast<int64_t>(i);
      initInfo = &actorInfo.receiveFns[i];
      break;
    }
  }
  if (initIdx < 0 || !initInfo || !initInfo->returnType) {
    ++errorCount_;
    emitError(location) << "for await: '" << methodName << "' is not a generator receive fn on '"
                        << actorTypeName << "'";
    return;
  }

  // Find the __next handler (should be immediately after init)
  int64_t nextIdx = -1;
  std::string nextMethodName = methodName + "__next";
  for (size_t i = 0; i < actorInfo.receiveFns.size(); ++i) {
    if (actorInfo.receiveFns[i].name == nextMethodName) {
      nextIdx = static_cast<int64_t>(i);
      break;
    }
  }
  if (nextIdx < 0) {
    ++errorCount_;
    emitError(location) << "for await: missing __next handler for '" << methodName << "' on '"
                        << actorTypeName << "'";
    return;
  }

  // The wrapper return type is { i8, YieldType }
  auto wrapperType = *initInfo->returnType;
  auto wrapperStructType = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(wrapperType);
  if (!wrapperStructType || wrapperStructType.getBody().size() != 2) {
    ++errorCount_;
    emitError(location) << "for await: unexpected wrapper type";
    return;
  }

  // Get the actor ref value
  auto actorPtr = generateExpression(mc->receiver->value);
  if (!actorPtr)
    return;

  auto actorRefType = hew::ActorRefType::get(&context);
  auto actorRef = hew::BitcastOp::create(builder, location, actorRefType, actorPtr);

  // Generate argument values for init call
  llvm::SmallVector<mlir::Value, 4> initArgs;
  for (const auto &argPtr : mc->args) {
    auto val = generateExpression(ast::callArgExpr(argPtr).value);
    if (!val)
      return;
    initArgs.push_back(val);
  }

  // Call init: actor_ask(actor, init_msg_type, args) → { i8, YieldType }
  auto initAsk = hew::ActorAskOp::create(builder, location, wrapperType, actorRef,
                                         builder.getI32IntegerAttr(static_cast<int32_t>(initIdx)),
                                         initArgs, /*timeout_ms=*/mlir::IntegerAttr{});
  auto initResult = initAsk.getResult();

  // Store wrapper result in alloca for mutable updates across iterations
  auto i64Type = builder.getI64Type();
  auto one = mlir::arith::ConstantIntOp::create(builder, location, i64Type, 1);
  auto resultAlloca = mlir::LLVM::AllocaOp::create(builder, location, ptrType, wrapperType, one);
  mlir::LLVM::StoreOp::create(builder, location, initResult, resultAlloca);

  // Loop control flags (break/continue support)
  auto lc = pushLoopControl(std::nullopt, location);

  // scf.while loop
  auto whileOp =
      mlir::scf::WhileOp::create(builder, location, mlir::TypeRange{}, mlir::ValueRange{});

  // Before region: check has_value
  auto *beforeBlock = builder.createBlock(&whileOp.getBefore());
  builder.setInsertionPointToStart(beforeBlock);

  auto isActive =
      mlir::memref::LoadOp::create(builder, location, lc.activeFlag, mlir::ValueRange{});

  auto currentResult = mlir::LLVM::LoadOp::create(builder, location, wrapperType, resultAlloca);
  auto hasValue = mlir::LLVM::ExtractValueOp::create(builder, location, currentResult,
                                                     llvm::ArrayRef<int64_t>{0});
  // has_value is i8, convert to i1
  auto zeroI8 = mlir::arith::ConstantIntOp::create(builder, location, i8Type, 0);
  auto hasValueBool = mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::ne,
                                                  hasValue, zeroI8);

  mlir::Value combinedCond = mlir::arith::AndIOp::create(builder, location, isActive, hasValueBool);
  combinedCond = andNotReturned(combinedCond, location);

  mlir::scf::ConditionOp::create(builder, location, combinedCond, mlir::ValueRange{});

  // After region: extract value, run body, call next
  auto *afterBlock = builder.createBlock(&whileOp.getAfter());
  builder.setInsertionPointToStart(afterBlock);

  auto falseVal = createIntConstant(builder, location, i1Type, 0);
  mlir::memref::StoreOp::create(builder, location, falseVal, lc.continueFlag);

  {
    SymbolTableScopeT bodyScope(symbolTable);
    MutableTableScopeT bodyMutScope(mutableVars);

    // Load current wrapper and extract the value
    auto wrapper = mlir::LLVM::LoadOp::create(builder, location, wrapperType, resultAlloca);
    auto value =
        mlir::LLVM::ExtractValueOp::create(builder, location, wrapper, llvm::ArrayRef<int64_t>{1});

    // Bind loop variable
    std::string loopVarName = "_await_var";
    if (auto *identPat = std::get_if<ast::PatIdentifier>(&stmt.pattern.value.kind)) {
      loopVarName = identPat->name;
    }
    declareVariable(loopVarName, value);

    // Generate loop body
    pushDropScope();
    generateLoopBodyWithContinueGuards(stmt.body.stmts, 0, stmt.body.stmts.size(), lc.continueFlag,
                                       location);
    popDropScope();
  }

  // Guard the __next call: only call if loop is still active (no break/return)
  auto stillActive =
      mlir::memref::LoadOp::create(builder, location, lc.activeFlag, mlir::ValueRange{});
  mlir::Value nextGuard = andNotReturned(stillActive, location);

  auto nextIfOp = mlir::scf::IfOp::create(builder, location, nextGuard, /*withElseRegion=*/false);
  builder.setInsertionPointToStart(&nextIfOp.getThenRegion().front());

  // Call next: actor_ask(actor, next_msg_type) → { i8, YieldType }
  auto nextAsk = hew::ActorAskOp::create(builder, location, wrapperType, actorRef,
                                         builder.getI32IntegerAttr(static_cast<int32_t>(nextIdx)),
                                         mlir::ValueRange{}, /*timeout_ms=*/mlir::IntegerAttr{});
  mlir::LLVM::StoreOp::create(builder, location, nextAsk.getResult(), resultAlloca);

  builder.setInsertionPointAfter(nextIfOp);

  ensureYieldTerminator(location);

  popLoopControl(lc, whileOp);
}

void MLIRGen::generateForStmt(const ast::StmtFor &stmt) {
  const ast::ExprBinary *rangeExpr = nullptr;
  if (auto *binExpr = std::get_if<ast::ExprBinary>(&stmt.iterable.value.kind)) {
    if (binExpr->op == ast::BinaryOp::Range || binExpr->op == ast::BinaryOp::RangeInclusive) {
      rangeExpr = binExpr;
    }
  }

  if (rangeExpr) {
    generateForRange(stmt, *rangeExpr);
    return;
  }

  // Collection-based for loop (for x in vec)
  generateForCollectionStmt(stmt);
}

void MLIRGen::generateForRange(const ast::StmtFor &stmt, const ast::ExprBinary &rangeExpr) {
  auto location = currentLoc;
  const ast::TypeExpr *rangeBoundType = nullptr;
  if (rangeExpr.left) {
    rangeBoundType =
        requireResolvedTypeOf(rangeExpr.left->span, "direct range loop signedness", location);
    if (!rangeBoundType)
      return;
  }
  if (rangeExpr.right &&
      !requireResolvedTypeOf(rangeExpr.right->span, "direct range loop upper bound signedness",
                             location)) {
    return;
  }

  mlir::Value lb = nullptr;
  mlir::Value ub = nullptr;
  if (rangeExpr.left)
    lb = generateExpression(rangeExpr.left->value);
  if (rangeExpr.right)
    ub = generateExpression(rangeExpr.right->value);

  if (!lb || !ub)
    return;

  // Convert lb/ub to index type first
  auto indexType = builder.getIndexType();
  if (lb.getType() != indexType) {
    lb = mlir::arith::IndexCastOp::create(builder, location, indexType, lb);
  }
  if (ub.getType() != indexType) {
    ub = mlir::arith::IndexCastOp::create(builder, location, indexType, ub);
  }

  // For inclusive range, add 1 to upper bound
  if (rangeExpr.op == ast::BinaryOp::RangeInclusive) {
    auto one = mlir::arith::ConstantIndexOp::create(builder, location, 1);
    ub = mlir::arith::AddIOp::create(builder, location, ub, one);
  }

  // Get the loop variable name
  std::string loopVarName;
  if (auto *identPat = std::get_if<ast::PatIdentifier>(&stmt.pattern.value.kind)) {
    loopVarName = identPat->name;
  } else {
    loopVarName = "_for_var";
  }

  // Determine if the range is over an unsigned type
  bool rangeIsUnsigned = rangeBoundType && isUnsignedTypeExpr(*rangeBoundType);

  // Use scf.while instead of scf.for to support break/continue.
  // This mirrors the pattern in generateForCollectionStmt.
  auto i64Type = builder.getI64Type();

  // Cast lb/ub from index → i64 for alloca storage
  auto lbI64 = mlir::arith::IndexCastOp::create(builder, location, i64Type, lb);
  auto ubI64 = mlir::arith::IndexCastOp::create(builder, location, i64Type, ub);

  // Index alloca initialized to lb
  auto memrefI64 = mlir::MemRefType::get({}, i64Type);
  mlir::Value indexAlloca = mlir::memref::AllocaOp::create(builder, location, memrefI64);
  mlir::memref::StoreOp::create(builder, location, lbI64, indexAlloca);

  auto lc = pushLoopControl(stmt.label, location);

  // Build scf.while
  auto whileOp =
      mlir::scf::WhileOp::create(builder, location, mlir::TypeRange{}, mlir::ValueRange{});

  // Before region: check index < ub && active && !returned
  auto *beforeBlock = builder.createBlock(&whileOp.getBefore());
  builder.setInsertionPointToStart(beforeBlock);

  auto isActive =
      mlir::memref::LoadOp::create(builder, location, lc.activeFlag, mlir::ValueRange{});
  auto curIdx = mlir::memref::LoadOp::create(builder, location, indexAlloca, mlir::ValueRange{});
  auto cond = mlir::arith::CmpIOp::create(builder, location,
                                          rangeIsUnsigned ? mlir::arith::CmpIPredicate::ult
                                                          : mlir::arith::CmpIPredicate::slt,
                                          curIdx, ubI64);
  mlir::Value combinedCond = mlir::arith::AndIOp::create(builder, location, isActive, cond);

  combinedCond = andNotReturned(combinedCond, location);

  mlir::scf::ConditionOp::create(builder, location, combinedCond, mlir::ValueRange{});

  // After region: loop body + index increment
  auto *afterBlock = builder.createBlock(&whileOp.getAfter());
  builder.setInsertionPointToStart(afterBlock);
  auto falseVal = createIntConstant(builder, location, builder.getI1Type(), 0);
  mlir::memref::StoreOp::create(builder, location, falseVal, lc.continueFlag);

  {
    SymbolTableScopeT loopScope(symbolTable);
    MutableTableScopeT loopMutScope(mutableVars);

    // Bind loop variable: load index as i64
    auto idx = mlir::memref::LoadOp::create(builder, location, indexAlloca, mlir::ValueRange{});
    declareVariable(loopVarName, idx);

    // Generate body with continue guards
    pushDropScope();
    generateLoopBodyWithContinueGuards(stmt.body.stmts, 0, stmt.body.stmts.size(), lc.continueFlag,
                                       location);
    popDropScope();

    // Increment index
    auto curI = mlir::memref::LoadOp::create(builder, location, indexAlloca, mlir::ValueRange{});
    auto one = createIntConstant(builder, location, i64Type, 1);
    auto nextIdx = mlir::arith::AddIOp::create(builder, location, curI, one);
    mlir::memref::StoreOp::create(builder, location, nextIdx, indexAlloca);
  }

  ensureYieldTerminator(location);

  popLoopControl(lc, whileOp);
}

void MLIRGen::generateForGeneratorStmt(const ast::StmtFor &stmt, const std::string &genFuncName) {
  auto location = currentLoc;
  auto i1Type = builder.getI1Type();

  // Generate the iterable expression to get the generator pointer
  mlir::Value genPtr = generateExpression(stmt.iterable.value);
  if (!genPtr)
    return;

  // Generator handle is a malloc'd coroutine frame; free at scope exit.
  {
    std::string tmpName =
        std::string("\0__gen_frame_", 13) + std::to_string(tempMaterializationCounter++);
    declareVariable(tmpName, genPtr);
    registerDroppable(tmpName, "free");
  }

  std::string nextName = genFuncName + "__next";
  std::string doneName = genFuncName + "__done";

  auto nextFuncOp = module.lookupSymbol<mlir::func::FuncOp>(nextName);
  auto doneFuncOp = module.lookupSymbol<mlir::func::FuncOp>(doneName);
  if (!nextFuncOp || !doneFuncOp) {
    ++errorCount_;
    emitError(location) << "generator functions not found for " << genFuncName;
    return;
  }

  std::string loopVarName;
  if (auto *identPat = std::get_if<ast::PatIdentifier>(&stmt.pattern.value.kind))
    loopVarName = identPat->name;
  else
    loopVarName = "_gen_var";

  auto lc = pushLoopControl(stmt.label, location);

  auto whileOp =
      mlir::scf::WhileOp::create(builder, location, mlir::TypeRange{}, mlir::ValueRange{});

  // Before region: check !done && active
  auto *beforeBlock = builder.createBlock(&whileOp.getBefore());
  builder.setInsertionPointToStart(beforeBlock);

  auto isActive =
      mlir::memref::LoadOp::create(builder, location, lc.activeFlag, mlir::ValueRange{});
  auto doneCall =
      mlir::func::CallOp::create(builder, location, doneFuncOp, mlir::ValueRange{genPtr});
  auto isDone = doneCall.getResult(0);
  auto trueVal = createIntConstant(builder, location, i1Type, 1);
  auto notDone = mlir::arith::XOrIOp::create(builder, location, isDone, trueVal);
  mlir::Value combinedCond = mlir::arith::AndIOp::create(builder, location, isActive, notDone);
  combinedCond = andNotReturned(combinedCond, location);

  mlir::scf::ConditionOp::create(builder, location, combinedCond, mlir::ValueRange{});

  // After region: call __next, bind loop var, run body
  auto *afterBlock = builder.createBlock(&whileOp.getAfter());
  builder.setInsertionPointToStart(afterBlock);

  auto falseVal = createIntConstant(builder, location, i1Type, 0);
  mlir::memref::StoreOp::create(builder, location, falseVal, lc.continueFlag);

  auto nextCall =
      mlir::func::CallOp::create(builder, location, nextFuncOp, mlir::ValueRange{genPtr});
  auto nextVal = nextCall.getResult(0);

  SymbolTableScopeT loopScope(symbolTable);
  MutableTableScopeT loopMutScope(mutableVars);
  declareVariable(loopVarName, nextVal);

  if (returnFlag) {
    auto flagVal = mlir::memref::LoadOp::create(builder, location, returnFlag, mlir::ValueRange{});
    auto trueConst = createIntConstant(builder, location, i1Type, 1);
    auto notReturned = mlir::arith::XOrIOp::create(builder, location, flagVal, trueConst);
    auto guard = mlir::scf::IfOp::create(builder, location, mlir::TypeRange{}, notReturned,
                                         /*withElseRegion=*/false);
    builder.setInsertionPointToStart(&guard.getThenRegion().front());
    pushDropScope();
    generateLoopBodyWithContinueGuards(stmt.body.stmts, 0, stmt.body.stmts.size(), lc.continueFlag,
                                       location);
    popDropScope();
    ensureYieldTerminator(location);
    builder.setInsertionPointAfter(guard);
  } else {
    pushDropScope();
    generateLoopBodyWithContinueGuards(stmt.body.stmts, 0, stmt.body.stmts.size(), lc.continueFlag,
                                       location);
    popDropScope();
  }

  ensureYieldTerminator(location);

  popLoopControl(lc, whileOp);
}

void MLIRGen::generateForCollectionStmt(const ast::StmtFor &stmt) {
  // Check if the iterable is a generator call or variable
  std::string genFuncName;
  if (auto *callExpr = std::get_if<ast::ExprCall>(&stmt.iterable.value.kind)) {
    if (callExpr->function) {
      if (auto *funcIdent = std::get_if<ast::ExprIdentifier>(&callExpr->function->value.kind)) {
        if (generatorFunctions.count(funcIdent->name))
          genFuncName = funcIdent->name;
      }
    }
  } else if (auto *identExpr = std::get_if<ast::ExprIdentifier>(&stmt.iterable.value.kind)) {
    auto git = generatorVarTypes.find(identExpr->name);
    if (git != generatorVarTypes.end())
      genFuncName = git->second;
  }

  // Create a helper method for range loop generation
  // void generateForRangeImpl(const ast::StmtFor &stmt, mlir::Value lb, mlir::Value ub, bool
  // inclusive); But since I can't easily modify header without knowing exact location... I'll
  // inline the logic but try to be concise.

  if (!genFuncName.empty()) {
    generateForGeneratorStmt(stmt, genFuncName);
    return;
  }

  mlir::Value collection = generateExpression(stmt.iterable.value);
  if (!collection)
    return;

  std::string collType;
  // Prefer resolved type from the type checker
  if (auto *typeExpr = resolvedTypeOf(stmt.iterable.span))
    collType = typeExprToCollectionString(
        *typeExpr, [this](llvm::StringRef name) { return resolveTypeAliasExpr(name); });
  // Check bare field access for actor collection fields
  if (collType.empty() && !currentActorName.empty()) {
    // Bare field name (e.g. `for item in items`)
    if (collType.empty()) {
      if (auto *ident = std::get_if<ast::ExprIdentifier>(&stmt.iterable.value.kind)) {
        auto key = currentActorName + "." + ident->name;
        auto cit = collectionFieldTypes.find(key);
        if (cit != collectionFieldTypes.end())
          collType = cit->second;
      }
    }
  }

  if (collType.rfind("Range", 0) == 0 ||
      (mlir::isa<hew::HewTupleType>(collection.getType()) &&
       mlir::cast<hew::HewTupleType>(collection.getType()).getElementTypes().size() == 2 &&
       mlir::cast<hew::HewTupleType>(collection.getType()).getElementTypes()[0] ==
           mlir::cast<hew::HewTupleType>(collection.getType()).getElementTypes()[1])) {

    auto location = currentLoc;
    auto *rangeType =
        requireResolvedTypeOf(stmt.iterable.span, "materialized range loop signedness", location);
    if (!rangeType)
      return;
    auto tupleType = mlir::cast<hew::HewTupleType>(collection.getType());
    auto elemType = tupleType.getElementTypes()[0];
    bool rangeIsUnsigned = false;
    if (auto *named = std::get_if<ast::TypeNamed>(&rangeType->kind)) {
      if (resolveTypeAlias(named->name) == "Range" && named->type_args &&
          !named->type_args->empty())
        rangeIsUnsigned = isUnsignedTypeExpr((*named->type_args)[0].value);
    } else if (auto *tuple = std::get_if<ast::TypeTuple>(&rangeType->kind)) {
      if (!tuple->elements.empty())
        rangeIsUnsigned = isUnsignedTypeExpr(tuple->elements.front().value);
    }

    // Extract start/end from the range tuple
    auto startVal = hew::TupleExtractOp::create(builder, location, elemType, collection, 0);
    auto endVal = hew::TupleExtractOp::create(builder, location, elemType, collection, 1);

    std::string loopVarName;
    if (auto *patIdent = std::get_if<ast::PatIdentifier>(&stmt.pattern.value.kind)) {
      loopVarName = patIdent->name;
    } else {
      loopVarName = "_for_var";
    }

    // Cast to i64 for the loop machinery (mirrors generateForRange)
    auto i64Type = builder.getI64Type();
    auto i1Type = builder.getI1Type();
    mlir::Value lbI64 = startVal;
    mlir::Value ubI64 = endVal;
    if (elemType != i64Type) {
      if (rangeIsUnsigned) {
        lbI64 = mlir::arith::ExtUIOp::create(builder, location, i64Type, startVal);
        ubI64 = mlir::arith::ExtUIOp::create(builder, location, i64Type, endVal);
      } else {
        lbI64 = mlir::arith::ExtSIOp::create(builder, location, i64Type, startVal);
        ubI64 = mlir::arith::ExtSIOp::create(builder, location, i64Type, endVal);
      }
    }

    // Index alloca
    auto memrefI64 = mlir::MemRefType::get({}, i64Type);
    mlir::Value indexAlloca = mlir::memref::AllocaOp::create(builder, location, memrefI64);
    mlir::memref::StoreOp::create(builder, location, lbI64, indexAlloca);

    auto lc = pushLoopControl(stmt.label, location);

    // scf.while
    auto whileOp =
        mlir::scf::WhileOp::create(builder, location, mlir::TypeRange{}, mlir::ValueRange{});

    // Before region: condition
    auto *beforeBlock = builder.createBlock(&whileOp.getBefore());
    builder.setInsertionPointToStart(beforeBlock);

    auto isActive =
        mlir::memref::LoadOp::create(builder, location, lc.activeFlag, mlir::ValueRange{});
    auto curIdx = mlir::memref::LoadOp::create(builder, location, indexAlloca, mlir::ValueRange{});
    auto cond = mlir::arith::CmpIOp::create(builder, location,
                                            rangeIsUnsigned ? mlir::arith::CmpIPredicate::ult
                                                            : mlir::arith::CmpIPredicate::slt,
                                            curIdx, ubI64);
    mlir::Value combinedCond = mlir::arith::AndIOp::create(builder, location, isActive, cond);
    combinedCond = andNotReturned(combinedCond, location);

    mlir::scf::ConditionOp::create(builder, location, combinedCond, mlir::ValueRange{});

    // After region: body + increment
    auto *afterBlock = builder.createBlock(&whileOp.getAfter());
    builder.setInsertionPointToStart(afterBlock);

    auto falseVal = createIntConstant(builder, location, i1Type, 0);
    mlir::memref::StoreOp::create(builder, location, falseVal, lc.continueFlag);

    {
      SymbolTableScopeT loopScope(symbolTable);
      MutableTableScopeT loopMutScope(mutableVars);
      auto loopVal =
          mlir::memref::LoadOp::create(builder, location, indexAlloca, mlir::ValueRange{});
      declareVariable(loopVarName, loopVal);
      pushDropScope();
      generateLoopBodyWithContinueGuards(stmt.body.stmts, 0, stmt.body.stmts.size(),
                                         lc.continueFlag, location);
      popDropScope();
    }

    // Increment index
    auto curForInc =
        mlir::memref::LoadOp::create(builder, location, indexAlloca, mlir::ValueRange{});
    auto one = createIntConstant(builder, location, i64Type, 1);
    auto nextIndex = mlir::arith::AddIOp::create(builder, location, curForInc, one);
    mlir::memref::StoreOp::create(builder, location, nextIndex, indexAlloca);

    ensureYieldTerminator(location);

    popLoopControl(lc, whileOp);

    return;
  }

  // Check if this is a HashMap iteration
  if (mlir::isa<hew::HashMapType>(collection.getType())) {
    generateForHashMap(stmt, collection, collType);
    return;
  }

  bool isStringCollection = collType == "bytes" || collType == "string" || collType == "String" ||
                            collType == "str" ||
                            mlir::isa<hew::StringRefType>(collection.getType());
  if (isStringCollection) {
    generateForString(stmt, collection, collType);
    return;
  }

  generateForVec(stmt, collection, collType);
}

void MLIRGen::generateForVec(const ast::StmtFor &stmt, mlir::Value collection,
                             const std::string &collType) {
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto i64Type = builder.getI64Type();
  auto i32Type = builder.getI32Type();
  auto i1Type = builder.getI1Type();

  mlir::Type typedVecElemType;
  if (auto vecType = mlir::dyn_cast<hew::VecType>(collection.getType()))
    typedVecElemType = vecType.getElementType();
  auto typedArrayType = mlir::dyn_cast<hew::HewArrayType>(collection.getType());
  mlir::Type typedArrayElemType;
  if (typedArrayType)
    typedArrayElemType = typedArrayType.getElementType();

  bool isVecPtr = typedVecElemType && mlir::isa<mlir::LLVM::LLVMPointerType>(typedVecElemType);

  // Get collection length
  mlir::Value len;
  mlir::Value arrayAlloca;
  mlir::LLVM::LLVMArrayType arrayStorageType;
  if (typedArrayType) {
    len = createIntConstant(builder, location, i64Type, typedArrayType.getSize());
    arrayStorageType = mlir::LLVM::LLVMArrayType::get(typedArrayElemType, typedArrayType.getSize());
    auto llvmArray = hew::BitcastOp::create(builder, location, arrayStorageType, collection);
    auto one = mlir::arith::ConstantIntOp::create(builder, location, 1, 64);
    arrayAlloca =
        mlir::LLVM::AllocaOp::create(builder, location, ptrType, arrayStorageType, one.getResult());
    mlir::LLVM::StoreOp::create(builder, location, llvmArray, arrayAlloca);
  } else {
    len = hew::VecLenOp::create(builder, location, i64Type, collection);
  }

  // Create index alloca (i64), initialized to 0
  auto memrefI64 = mlir::MemRefType::get({}, i64Type);
  mlir::Value indexAlloca = mlir::memref::AllocaOp::create(builder, location, memrefI64);
  auto zero = createIntConstant(builder, location, i64Type, 0);
  mlir::memref::StoreOp::create(builder, location, zero, indexAlloca);

  auto lc = pushLoopControl(stmt.label, location);

  // scf.while loop
  auto whileOp =
      mlir::scf::WhileOp::create(builder, location, mlir::TypeRange{}, mlir::ValueRange{});

  // Before region: check index < len && active && !returned
  auto *beforeBlock = builder.createBlock(&whileOp.getBefore());
  builder.setInsertionPointToStart(beforeBlock);

  auto isActive =
      mlir::memref::LoadOp::create(builder, location, lc.activeFlag, mlir::ValueRange{});
  mlir::Value curIdx =
      mlir::memref::LoadOp::create(builder, location, indexAlloca, mlir::ValueRange{});
  auto cond =
      mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::slt, curIdx, len);
  mlir::Value combinedCond = mlir::arith::AndIOp::create(builder, location, isActive, cond);
  combinedCond = andNotReturned(combinedCond, location);

  mlir::scf::ConditionOp::create(builder, location, combinedCond, mlir::ValueRange{});

  // After region: loop body
  auto *afterBlock = builder.createBlock(&whileOp.getAfter());
  builder.setInsertionPointToStart(afterBlock);

  auto falseVal = createIntConstant(builder, location, i1Type, 0);
  mlir::memref::StoreOp::create(builder, location, falseVal, lc.continueFlag);

  {
    SymbolTableScopeT bodyScope(symbolTable);
    MutableTableScopeT bodyMutScope(mutableVars);

    // Load current index
    mlir::Value idx =
        mlir::memref::LoadOp::create(builder, location, indexAlloca, mlir::ValueRange{});

    // Get element from collection based on type
    mlir::Value elem;
    {
      mlir::Type elemType;
      if (typedArrayType) {
        auto zero = mlir::arith::ConstantIntOp::create(builder, location, 0, 64);
        auto elemPtr =
            mlir::LLVM::GEPOp::create(builder, location, ptrType, arrayStorageType, arrayAlloca,
                                      mlir::ValueRange{zero.getResult(), idx});
        elem = mlir::LLVM::LoadOp::create(builder, location, typedArrayElemType, elemPtr);
      } else {
        elemType = typedVecElemType;
        if (!elemType) {
          ++errorCount_;
          emitError(location) << "unsupported for-loop Vec element type for iterable '" << collType
                              << "'";
          return;
        }
        elem = hew::VecGetOp::create(builder, location, elemType, collection, idx);
      }
    }

    // Bind element to the pattern variable
    std::string boundElemName;
    if (auto *identPat = std::get_if<ast::PatIdentifier>(&stmt.pattern.value.kind)) {
      boundElemName = identPat->name;
      declareVariable(boundElemName, elem);
      // Register loop variable for actor dispatch when iterating Vec<ActorRef<T>>
      if (isVecPtr && collType.find("Vec<ActorRef<") == 0) {
        auto start = std::string("Vec<ActorRef<").size();
        auto end = collType.rfind(">>");
        if (end != std::string::npos) {
          std::string innerActorName = collType.substr(start, end - start);
          actorVarTypes[boundElemName] = innerActorName;
        }
      }
    }

    // Generate loop body
    pushDropScope();
    // Register drop for owned loop variable (e.g. String from Vec<String>).
    // Only String elements are owned copies — hew_vec_get_str returns strdup.
    // Other types (structs, ints, pointers) are borrowed loads from Vec storage;
    // dropping them would free memory still owned by the Vec.
    if (!boundElemName.empty() && mlir::isa<hew::StringRefType>(elem.getType())) {
      registerDroppable(boundElemName, "hew_string_drop");
    }
    generateLoopBodyWithContinueGuards(stmt.body.stmts, 0, stmt.body.stmts.size(), lc.continueFlag,
                                       location);
    popDropScope();

    // Increment index
    mlir::Value curI =
        mlir::memref::LoadOp::create(builder, location, indexAlloca, mlir::ValueRange{});
    auto one = createIntConstant(builder, location, i64Type, 1);
    auto nextIdx = mlir::arith::AddIOp::create(builder, location, curI, one);
    mlir::memref::StoreOp::create(builder, location, nextIdx, indexAlloca);
  }

  // Ensure yield terminator
  ensureYieldTerminator(location);

  popLoopControl(lc, whileOp);
}

void MLIRGen::generateForString(const ast::StmtFor &stmt, mlir::Value collection,
                                const std::string &collType) {
  generateForVec(stmt, collection, collType);
}

void MLIRGen::generateForHashMap(const ast::StmtFor &stmt, mlir::Value collection,
                                 const std::string &collType) {
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto i64Type = builder.getI64Type();
  auto i1Type = builder.getI1Type();

  auto hmType = mlir::cast<hew::HashMapType>(collection.getType());
  mlir::Type hmKeyType = hmType.getKeyType();
  mlir::Type hmValType = hmType.getValueType();

  // Get keys as a Vec<K> via hew_hashmap_keys(map) -> !hew.vec<K>
  mlir::Type keysResultType = hew::VecType::get(&context, hmKeyType);
  auto keysVec =
      hew::HashMapKeysOp::create(builder, location, keysResultType, collection).getResult();

  // Get number of keys
  mlir::Value len = hew::VecLenOp::create(builder, location, i64Type, keysVec);

  // Index alloca
  auto memrefI64 = mlir::MemRefType::get({}, i64Type);
  mlir::Value indexAlloca = mlir::memref::AllocaOp::create(builder, location, memrefI64);
  auto zero = createIntConstant(builder, location, i64Type, 0);
  mlir::memref::StoreOp::create(builder, location, zero, indexAlloca);

  auto lc = pushLoopControl(stmt.label, location);

  // scf.while loop
  auto whileOp =
      mlir::scf::WhileOp::create(builder, location, mlir::TypeRange{}, mlir::ValueRange{});

  // Before region: check index < len && active && !returned
  auto *beforeBlock = builder.createBlock(&whileOp.getBefore());
  builder.setInsertionPointToStart(beforeBlock);

  auto isActive =
      mlir::memref::LoadOp::create(builder, location, lc.activeFlag, mlir::ValueRange{});
  mlir::Value curIdx =
      mlir::memref::LoadOp::create(builder, location, indexAlloca, mlir::ValueRange{});
  auto cond =
      mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::slt, curIdx, len);
  mlir::Value combinedCond = mlir::arith::AndIOp::create(builder, location, isActive, cond);

  combinedCond = andNotReturned(combinedCond, location);

  mlir::scf::ConditionOp::create(builder, location, combinedCond, mlir::ValueRange{});

  // After region: loop body
  auto *afterBlock = builder.createBlock(&whileOp.getAfter());
  builder.setInsertionPointToStart(afterBlock);

  auto falseVal = createIntConstant(builder, location, i1Type, 0);
  mlir::memref::StoreOp::create(builder, location, falseVal, lc.continueFlag);

  {
    SymbolTableScopeT bodyScope(symbolTable);
    MutableTableScopeT bodyMutScope(mutableVars);

    // Load current index
    mlir::Value idx =
        mlir::memref::LoadOp::create(builder, location, indexAlloca, mlir::ValueRange{});

    // Get key and value from hashmap
    mlir::Value key = hew::VecGetOp::create(builder, location, hmKeyType, keysVec, idx);
    mlir::Value val =
        hew::HashMapGetOp::create(builder, location, hmValType, collection, key).getResult();

    // Bind variables from the pattern
    std::string boundKeyName, boundValName;
    if (auto *tuplePat = std::get_if<ast::PatTuple>(&stmt.pattern.value.kind)) {
      if (tuplePat->elements.size() >= 1) {
        if (auto *kIdent = std::get_if<ast::PatIdentifier>(&tuplePat->elements[0]->value.kind)) {
          boundKeyName = kIdent->name;
          declareVariable(boundKeyName, key);
        }
      }
      if (tuplePat->elements.size() >= 2) {
        if (auto *vIdent = std::get_if<ast::PatIdentifier>(&tuplePat->elements[1]->value.kind)) {
          boundValName = vIdent->name;
          declareVariable(boundValName, val);
        }
      }
    } else if (auto *identPat = std::get_if<ast::PatIdentifier>(&stmt.pattern.value.kind)) {
      boundKeyName = identPat->name;
      declareVariable(boundKeyName, key);
    }

    // Generate loop body
    pushDropScope();
    // Register drops for owned loop variables (String keys/values from HashMap).
    // Only String types are owned copies (strdup'd). Other types are borrowed
    // from the HashMap's internal storage; dropping them would be a double-free.
    if (!boundKeyName.empty() && mlir::isa<hew::StringRefType>(key.getType()))
      registerDroppable(boundKeyName, "hew_string_drop");
    if (!boundValName.empty() && mlir::isa<hew::StringRefType>(val.getType()))
      registerDroppable(boundValName, "hew_string_drop");
    generateLoopBodyWithContinueGuards(stmt.body.stmts, 0, stmt.body.stmts.size(), lc.continueFlag,
                                       location);
    popDropScope();

    // Increment index
    mlir::Value curI =
        mlir::memref::LoadOp::create(builder, location, indexAlloca, mlir::ValueRange{});
    auto one = createIntConstant(builder, location, i64Type, 1);
    auto nextIdx = mlir::arith::AddIOp::create(builder, location, curI, one);
    mlir::memref::StoreOp::create(builder, location, nextIdx, indexAlloca);
  }

  // Ensure yield terminator
  ensureYieldTerminator(location);

  popLoopControl(lc, whileOp);

  // Free the temporary keys vec
  hew::VecFreeOp::create(builder, location, keysVec);
}

void MLIRGen::generateReturnStmt(const ast::StmtReturn &stmt) {
  auto location = currentLoc;

  // Check if we're directly inside the function body or nested in an SCF region
  auto *parentOp = builder.getInsertionBlock()->getParentOp();
  bool directlyInFunc = mlir::isa<mlir::func::FuncOp>(parentOp);

  if (!directlyInFunc && returnFlag) {
    // Inside an SCF region: store to return slot and set flag instead of
    // emitting func.return (which is invalid inside SCF ops).
    if (stmt.value) {
      // Lazily create returnSlot for aggregate types that skipped eager creation.
      ensureReturnSlot(location);
      if (returnSlot) {
        auto val = generateExpression(stmt.value->value);
        if (val) {
          collectVisibleBindingIdentities(stmt.value->value, funcLevelEarlyReturnExcludeValues,
                                          &funcLevelEarlyReturnExcludeResolvedNames);
          auto slotType = mlir::cast<mlir::MemRefType>(returnSlot.getType()).getElementType();
          val = coerceTypeForSink(val, slotType, location);
          mlir::memref::StoreOp::create(builder, location, val, returnSlot);
        }
      }
    }
    auto trueVal = createIntConstant(builder, location, builder.getI1Type(), 1);
    mlir::memref::StoreOp::create(builder, location, trueVal, returnFlag);
    // Mark that an EXPLICIT return statement was taken (not a trailing expression).
    if (earlyReturnFlag)
      mlir::memref::StoreOp::create(builder, location, trueVal, earlyReturnFlag);
    // Also set the innermost continue flag so that remaining statements
    // in the current loop iteration are skipped.
    if (!loopContinueStack.empty()) {
      mlir::memref::StoreOp::create(builder, location, trueVal, loopContinueStack.back());
    }
  } else {
    // At function top level: emit defers then drops before return
    emitDeferredCalls();
    if (stmt.value) {
      // Evaluate the return expression BEFORE emitting drops so that locals
      // referenced by the expression (e.g. method calls, binary ops) are still
      // alive.  The result is captured in a temporary, then drops run, then we
      // emit the ReturnOp with the already-computed value.
      auto val = generateExpression(stmt.value->value);
      if (val) {
        if (currentFunction && currentFunction.getResultTypes().size() == 1)
          val = coerceTypeForSink(val, currentFunction.getResultTypes()[0], location);
        DropValueSet returnValues;
        collectVisibleBindingIdentities(stmt.value->value, returnValues);
        if (!returnValues.empty())
          emitDropsExcept(returnValues);
        else
          emitAllDrops();
        mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{val});
      } else {
        emitAllDrops();
        if (stmt.value &&
            (std::holds_alternative<ast::ExprMatch>(stmt.value->value.kind) ||
             std::holds_alternative<ast::ExprIfLet>(stmt.value->value.kind)) &&
            currentFunction && currentFunction.getResultTypes().size() == 1) {
          // Control-flow expression failures should still leave a well-formed
          // return while the earlier diagnostic aborts codegen.
          auto fallback =
              createDefaultValue(builder, location, currentFunction.getResultTypes()[0]);
          mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{fallback});
        } else {
          mlir::func::ReturnOp::create(builder, location);
        }
      }
    } else {
      emitAllDrops();
      mlir::func::ReturnOp::create(builder, location);
    }
  }
}

void MLIRGen::generateExprStmt(const ast::StmtExpression &stmt) {
  auto location = currentLoc;
  auto rejectGeneratorYieldFieldAccess = [&](const ast::Expr &yieldedExpr) -> bool {
    if (!std::holds_alternative<ast::ExprFieldAccess>(yieldedExpr.kind) &&
        !exprYieldsFieldMatching(yieldedExpr, [](const ast::ExprFieldAccess &) { return true; }))
      return false;
    ++errorCount_;
    emitError(location) << "yielding a field from a generator is not yet supported "
                        << "(field-alias ownership tracking is required); "
                        << "yield the whole value or clone the field instead";
    return true;
  };
  if (auto *yieldExpr = std::get_if<ast::ExprYield>(&stmt.expr.value.kind)) {
    if (yieldExpr->value && rejectGeneratorYieldFieldAccess((*yieldExpr->value)->value))
      return;
  }
  auto blockTailRequiresValue = [&](const ast::Block &block,
                                    const auto &exprRequiresValue) -> bool {
    if (block.trailing_expr)
      return exprRequiresValue(block.trailing_expr->value, exprRequiresValue);

    if (block.stmts.empty())
      return false;

    auto *exprStmt = std::get_if<ast::StmtExpression>(&block.stmts.back()->value.kind);
    if (!exprStmt)
      return false;

    return exprRequiresValue(exprStmt->expr.value, exprRequiresValue);
  };
  auto exprRequiresValue = [&](const ast::Expr &expr, const auto &self) -> bool {
    if (auto *resolvedType = resolvedTypeOf(expr.span))
      if (auto *tupleType = std::get_if<ast::TypeTuple>(&resolvedType->kind);
          tupleType && tupleType->elements.empty())
        return false;

    if (auto *blockExpr = std::get_if<ast::ExprBlock>(&expr.kind))
      return blockTailRequiresValue(blockExpr->block, self);
    if (auto *scopeExpr = std::get_if<ast::ExprScope>(&expr.kind))
      return blockTailRequiresValue(scopeExpr->block, self);
    if (auto *unsafeExpr = std::get_if<ast::ExprUnsafe>(&expr.kind))
      return blockTailRequiresValue(unsafeExpr->block, self);
    if (auto *ifExpr = std::get_if<ast::ExprIf>(&expr.kind))
      return ifExpr->else_block.has_value();
    if (std::holds_alternative<ast::ExprCall>(expr.kind) ||
        std::holds_alternative<ast::ExprMethodCall>(expr.kind) ||
        std::holds_alternative<ast::ExprSend>(expr.kind) ||
        std::holds_alternative<ast::ExprJoin>(expr.kind) ||
        std::holds_alternative<ast::ExprTimeout>(expr.kind) ||
        std::holds_alternative<ast::ExprYield>(expr.kind) ||
        std::holds_alternative<ast::ExprCooperate>(expr.kind) ||
        std::holds_alternative<ast::ExprScopeLaunch>(expr.kind) ||
        std::holds_alternative<ast::ExprScopeSpawn>(expr.kind) ||
        std::holds_alternative<ast::ExprScopeCancel>(expr.kind))
      return false;

    return true;
  };
  auto scopeBlockTailRequiresValue = [&](const ast::Block &block,
                                         const auto &exprNeedsValue) -> bool {
    if (block.trailing_expr)
      return exprNeedsValue(block.trailing_expr->value, exprNeedsValue);

    if (block.stmts.empty())
      return false;

    auto *exprStmt = std::get_if<ast::StmtExpression>(&block.stmts.back()->value.kind);
    if (!exprStmt)
      return false;

    return exprNeedsValue(exprStmt->expr.value, exprNeedsValue);
  };
  auto scopeExprRequiresValue = [&](const ast::Expr &expr, const auto &self) -> bool {
    if (auto *resolvedType = resolvedTypeOf(expr.span))
      if (auto *tupleType = std::get_if<ast::TypeTuple>(&resolvedType->kind);
          tupleType && tupleType->elements.empty())
        return false;

    if (auto *blockExpr = std::get_if<ast::ExprBlock>(&expr.kind))
      return scopeBlockTailRequiresValue(blockExpr->block, self);
    if (auto *scopeExpr = std::get_if<ast::ExprScope>(&expr.kind))
      return scopeBlockTailRequiresValue(scopeExpr->block, self);
    if (auto *unsafeExpr = std::get_if<ast::ExprUnsafe>(&expr.kind))
      return scopeBlockTailRequiresValue(unsafeExpr->block, self);
    if (std::holds_alternative<ast::ExprIf>(expr.kind))
      return false;
    if (std::holds_alternative<ast::ExprCall>(expr.kind) ||
        std::holds_alternative<ast::ExprMethodCall>(expr.kind) ||
        std::holds_alternative<ast::ExprSend>(expr.kind) ||
        std::holds_alternative<ast::ExprJoin>(expr.kind) ||
        std::holds_alternative<ast::ExprTimeout>(expr.kind) ||
        std::holds_alternative<ast::ExprYield>(expr.kind) ||
        std::holds_alternative<ast::ExprCooperate>(expr.kind) ||
        std::holds_alternative<ast::ExprScopeLaunch>(expr.kind) ||
        std::holds_alternative<ast::ExprScopeSpawn>(expr.kind) ||
        std::holds_alternative<ast::ExprScopeCancel>(expr.kind))
      return false;

    return true;
  };
  auto failClosedDiscardedBlockLike = [&](llvm::StringRef exprKind, const ast::Block &block,
                                          mlir::Value value, size_t errorsBefore) -> bool {
    auto *insertBlock = builder.getInsertionBlock();
    if (value || errorCount_ != errorsBefore || (insertBlock && hasRealTerminator(insertBlock)) ||
        !blockTailRequiresValue(block, exprRequiresValue))
      return false;

    ++errorCount_;
    emitError(location) << "discarded " << exprKind
                        << " in statement position failed to lower a nested value expression";
    return true;
  };

  mlir::Value val = nullptr;
  if (auto *blockExpr = std::get_if<ast::ExprBlock>(&stmt.expr.value.kind)) {
    auto errorsBefore = errorCount_;
    val = generateBlock(blockExpr->block, /*statementPosition=*/true);
    if (failClosedDiscardedBlockLike("block expression", blockExpr->block, val, errorsBefore))
      return;
  } else if (auto *scopeExpr = std::get_if<ast::ExprScope>(&stmt.expr.value.kind)) {
    auto errorsBefore = errorCount_;
    val = generateScopeExpr(*scopeExpr, /*statementPosition=*/true);
    auto *insertBlock = builder.getInsertionBlock();
    if (!val && errorCount_ == errorsBefore && !(insertBlock && hasRealTerminator(insertBlock)) &&
        scopeBlockTailRequiresValue(scopeExpr->block, scopeExprRequiresValue)) {
      ++errorCount_;
      emitError(location)
          << "discarded scope expression in statement position failed to lower a nested value "
             "expression";
      return;
    }
  } else if (auto *unsafeExpr = std::get_if<ast::ExprUnsafe>(&stmt.expr.value.kind)) {
    auto errorsBefore = errorCount_;
    val = generateBlock(unsafeExpr->block, /*statementPosition=*/true);
    if (failClosedDiscardedBlockLike("unsafe expression", unsafeExpr->block, val, errorsBefore))
      return;
  } else if (std::holds_alternative<ast::ExprIf>(stmt.expr.value.kind)) {
    // Route statement-position if expressions through the discarded-expr
    // path so that each branch materialises its own temporary with an
    // individual init-bit guard.  Without this, a no-else if leaks the
    // branch value, and an if-else creates a value-producing scf.if whose
    // single temp lacks per-branch init-flag protection.
    val = generateDiscardedExpr(stmt.expr);
  } else {
    val = generateExpression(stmt.expr.value);
  }
  // Materialize discarded heap-allocated temporaries (e.g. bare function
  // calls whose return value is an owned string or collection).
  if (val)
    materializeTemporary(val, stmt.expr.value);
}

// ============================================================================
// Loop statement generation
// ============================================================================

void MLIRGen::generateLoopStmt(const ast::StmtLoop &stmt) {
  auto location = currentLoc;

  auto lc = pushLoopControl(stmt.label, location);

  // Build scf.while
  auto whileOp =
      mlir::scf::WhileOp::create(builder, location, mlir::TypeRange{}, mlir::ValueRange{});

  // Before region: check active flag
  auto *beforeBlock = builder.createBlock(&whileOp.getBefore());
  builder.setInsertionPointToStart(beforeBlock);
  auto cond = mlir::memref::LoadOp::create(builder, location, lc.activeFlag, mlir::ValueRange{})
                  .getResult();
  cond = andNotReturned(cond, location);
  mlir::scf::ConditionOp::create(builder, location, cond, mlir::ValueRange{});

  // After region: loop body
  auto *afterBlock = builder.createBlock(&whileOp.getAfter());
  builder.setInsertionPointToStart(afterBlock);

  // Reset continue flag at start of each iteration
  auto falseVal = createIntConstant(builder, location, builder.getI1Type(), 0);
  mlir::memref::StoreOp::create(builder, location, falseVal, lc.continueFlag);

  {
    SymbolTableScopeT loopScope(symbolTable);
    MutableTableScopeT loopMutScope(mutableVars);
    pushDropScope();
    generateLoopBodyWithContinueGuards(stmt.body.stmts, 0, stmt.body.stmts.size(), lc.continueFlag,
                                       location);
    popDropScope();
  }

  ensureYieldTerminator(location);

  popLoopControl(lc, whileOp);
}

void MLIRGen::generateBreakStmt(const ast::StmtBreak &stmt) {
  auto location = currentLoc;

  if (loopActiveStack.empty()) {
    ++errorCount_;
    emitError(location) << "break used outside of a loop";
    return;
  }

  // If break has a value, store it in the break value alloca
  if (stmt.value) {
    mlir::Value val = generateExpression(stmt.value->value);
    if (val && !loopBreakValueStack.empty()) {
      auto &breakAlloca = loopBreakValueStack.back();
      if (!breakAlloca) {
        // Lazily allocate the break value storage on first use
        auto savedIP = builder.saveInsertionPoint();
        auto &entryBlock = currentFunction.front();
        builder.setInsertionPointToStart(&entryBlock);
        auto memrefType = mlir::MemRefType::get({}, val.getType());
        breakAlloca = mlir::memref::AllocaOp::create(builder, location, memrefType);
        builder.restoreInsertionPoint(savedIP);
      }
      mlir::memref::StoreOp::create(builder, location, val, breakAlloca);
    }
  }

  // Determine which active/continue flags to set
  mlir::Value targetActive = loopActiveStack.back();
  mlir::Value targetContinue = loopContinueStack.empty() ? nullptr : loopContinueStack.back();

  if (stmt.label) {
    auto labelStr = *stmt.label;
    auto it = labeledActiveFlags.find(labelStr);
    if (it != labeledActiveFlags.end()) {
      targetActive = it->second;
    } else {
      ++errorCount_;
      emitError(location) << "unknown loop label '" << labelStr << "'";
      return;
    }
    auto cit = labeledContinueFlags.find(labelStr);
    if (cit != labeledContinueFlags.end()) {
      targetContinue = cit->second;
    }
  }

  // Drop variables in current scope (and intermediate scopes for labeled breaks)
  if (!dropScopes.empty()) {
    emitDropsForScope(dropScopes.back());
    dropScopes.back().clear(); // Prevent double-drop on block exit

    // For labeled breaks targeting outer loops, also drop intermediate scopes
    if (stmt.label && targetActive != loopActiveStack.back()) {
      size_t targetIdx = 0;
      for (size_t i = 0; i < loopActiveStack.size(); ++i) {
        if (loopActiveStack[i] == targetActive) {
          targetIdx = i;
          break;
        }
      }
      size_t stopAt = loopDropScopeBase[targetIdx];
      for (int i = (int)dropScopes.size() - 2; i > (int)stopAt; --i) {
        emitDropsForScope(dropScopes[i]);
        dropScopes[i].clear();
      }
    }
  }

  // Set the active flag to false (exit loop at condition check)
  auto i1Type = builder.getI1Type();
  auto falseVal = createIntConstant(builder, location, i1Type, 0);
  mlir::memref::StoreOp::create(builder, location, falseVal, targetActive);

  // Also set continue flag so remaining body statements are skipped
  if (targetContinue) {
    auto trueVal = createIntConstant(builder, location, i1Type, 1);
    mlir::memref::StoreOp::create(builder, location, trueVal, targetContinue);
  }

  // For labeled breaks, deactivate ALL intermediate loops (not just innermost)
  if (stmt.label && targetActive != loopActiveStack.back()) {
    auto trueVal = createIntConstant(builder, location, i1Type, 1);
    for (size_t i = loopActiveStack.size(); i > 0; --i) {
      if (loopActiveStack[i - 1] == targetActive)
        break;
      mlir::memref::StoreOp::create(builder, location, falseVal, loopActiveStack[i - 1]);
      // Also set continue flag for each intermediate loop so remaining
      // body statements in that iteration are skipped
      if (i - 1 < loopContinueStack.size()) {
        mlir::memref::StoreOp::create(builder, location, trueVal, loopContinueStack[i - 1]);
      }
    }
  }
}

void MLIRGen::generateContinueStmt(const ast::StmtContinue &stmt) {
  auto location = currentLoc;

  if (loopContinueStack.empty()) {
    ++errorCount_;
    emitError(location) << "continue used outside of a loop";
    return;
  }

  // Determine which continue flag to set
  mlir::Value targetContinue = loopContinueStack.back();

  if (stmt.label) {
    auto labelStr = *stmt.label;
    auto cit = labeledContinueFlags.find(labelStr);
    if (cit != labeledContinueFlags.end()) {
      targetContinue = cit->second;
    } else {
      ++errorCount_;
      emitError(location) << "unknown loop label '" << labelStr << "'";
      return;
    }
    // For labeled continue, deactivate ALL intermediate loops (not just innermost)
    if (targetContinue != loopContinueStack.back()) {
      auto i1Type = builder.getI1Type();
      auto falseVal = createIntConstant(builder, location, i1Type, 0);
      // Find target loop index in the continue stack
      size_t targetIdx = loopContinueStack.size() - 1;
      for (size_t i = 0; i < loopContinueStack.size(); ++i) {
        if (loopContinueStack[i] == targetContinue) {
          targetIdx = i;
          break;
        }
      }
      // Deactivate all loops inner to the target
      for (size_t i = targetIdx + 1; i < loopActiveStack.size(); ++i) {
        mlir::memref::StoreOp::create(builder, location, falseVal, loopActiveStack[i]);
      }
    }
  }

  // Drop variables in current scope (and intermediate scopes for labeled continues)
  if (!dropScopes.empty()) {
    emitDropsForScope(dropScopes.back());
    dropScopes.back().clear(); // Prevent double-drop on block exit

    // For labeled continues targeting outer loops, also drop intermediate scopes
    if (stmt.label && targetContinue != loopContinueStack.back()) {
      size_t targetIdx = 0;
      for (size_t i = 0; i < loopContinueStack.size(); ++i) {
        if (loopContinueStack[i] == targetContinue) {
          targetIdx = i;
          break;
        }
      }
      size_t stopAt = loopDropScopeBase[targetIdx];
      for (int i = (int)dropScopes.size() - 2; i > (int)stopAt; --i) {
        emitDropsForScope(dropScopes[i]);
        dropScopes[i].clear();
      }
    }
  }

  // Set the continue flag to true
  auto i1Type = builder.getI1Type();
  auto trueVal = createIntConstant(builder, location, i1Type, 1);
  mlir::memref::StoreOp::create(builder, location, trueVal, targetContinue);
  // Also set inner continue flag to skip remaining body statements
  if (targetContinue != loopContinueStack.back()) {
    mlir::memref::StoreOp::create(builder, location, trueVal, loopContinueStack.back());
  }
}
