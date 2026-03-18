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

  LoopControl lc{activeFlag, continueFlag, {}};
  if (label) {
    lc.labelName = *label;
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
    labeledActiveFlags.erase(lc.labelName);
    labeledContinueFlags.erase(lc.labelName);
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
      val = coerceType(val, slotType, location);
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

mlir::Value MLIRGen::generateBlock(const ast::Block &block) {
  // Create a new scope for variables in this block
  SymbolTableScopeT varScope(symbolTable);
  MutableTableScopeT mutScope(mutableVars);

  // RAII drop scope: emit drops when block scope exits
  struct DropScopeGuard {
    MLIRGen &gen;
    DropScopeGuard(MLIRGen &g) : gen(g) { gen.pushDropScope(); }
    ~DropScopeGuard() { gen.popDropScope(); }
  } dropGuard(*this);

  // NOTE: pendingFunctionParamDrops drain point — currently disabled until
  // null-after-move tracking prevents double-frees from consumed params.

  // Determine if this block is the direct function body (for return guard logic).
  // Only use the returnFlag guarded path at the function body level when a
  // returnSlot exists (non-void functions), not in nested blocks or void functions.
  bool useReturnGuards = false;
  if (returnFlag && returnSlot) {
    auto *parentOp = builder.getInsertionBlock()->getParentOp();
    useReturnGuards = mlir::isa<mlir::func::FuncOp>(parentOp);
  }

  const auto &stmts = block.stmts;

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
    auto result = generateExpression(block.trailing_expr->value);
    // Null RAII close alloca for the tail expression variable so the
    // block's scope-exit drop doesn't close a handle being returned.
    if (auto *id = std::get_if<ast::ExprIdentifier>(
            &block.trailing_expr->value.kind)) {
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
          val = coerceType(val, slotType, location);
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
        mlir::Type resultType;
        if (currentFunction && currentFunction.getResultTypes().size() == 1) {
          resultType = currentFunction.getResultTypes()[0];
        } else {
          // Statement-position match: result is discarded, type is irrelevant.
          // TODO: generate statement-position match without value-producing path.
          resultType = builder.getI32Type();
        }
        auto val = scrutinee ? generateMatchImpl(scrutinee, matchNode->arms, resultType, location)
                             : nullptr;
        if (val && returnSlot) {
          auto slotType = mlir::cast<mlir::MemRefType>(returnSlot.getType()).getElementType();
          val = coerceType(val, slotType, location);
          mlir::memref::StoreOp::create(builder, location, val, returnSlot);
          mlir::memref::StoreOp::create(builder, location, trueConst, returnFlag);
        }
        ensureYieldTerminator(location);
        builder.setInsertionPointAfter(guard);
        return nullptr;
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
        auto result = generateExpression(exprStmt->expr.value);
        if (auto *id = std::get_if<ast::ExprIdentifier>(
                &exprStmt->expr.value.kind)) {
          nullOutRaiiAlloca(id->name);
        }
        return result;
      }
    }

    // Case 2: If statement as a value-producing block ending
    // When a block ends with `if ... { ... } else { ... }` and the enclosing
    // function returns a value, generate it as an if-expression.
    if (auto *ifStmt = std::get_if<ast::StmtIf>(&lastStmt.kind)) {
      return generateIfStmtAsExpr(*ifStmt);
    }

    // Case 3: Match statement as a value-producing block ending
    if (auto *matchNode = std::get_if<ast::StmtMatch>(&lastStmt.kind)) {
      auto location = loc(lastStmt.span);
      auto scrutinee = generateExpression(matchNode->scrutinee.value);
      if (!scrutinee)
        return nullptr;
      scrutinee = derefIndirectEnumScrutinee(scrutinee, matchNode->scrutinee.span, location,
                                             &matchNode->arms);
      mlir::Type resultType;
      if (currentFunction && currentFunction.getResultTypes().size() == 1) {
        resultType = currentFunction.getResultTypes()[0];
      } else {
        // Statement-position match: result is discarded, type is irrelevant.
        // TODO: generate statement-position match without value-producing path.
        resultType = builder.getI32Type();
      }
      return generateMatchImpl(scrutinee, matchNode->arms, resultType, location);
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
                mlir::cast<mlir::LLVM::LLVMStructType>(value.getType()).getBody()[fi.index],
                value, builder.getStringAttr(fi.name), static_cast<int64_t>(fi.index));
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
      emitError(location) << "unknown struct type '" << structPat->name
                          << "' in let pattern";
    }
  } else {
    emitError(location) << "unsupported sub-pattern in let destructuring";
  }
}

void MLIRGen::generateLetStmt(const ast::StmtLet &stmt) {
  auto location = currentLoc;
  // Set the declared type so constructors (Vec::new, HashMap::new, None, Ok,
  // Err) can emit correctly typed results.
  // Note: stmt.ty may contain inferred unit type `()` → NoneType, which is
  // valid here. Use convertType (not convertTypeOrError) to allow NoneType.
  if (stmt.ty)
    pendingDeclaredType = convertType(stmt.ty->value);

  mlir::Value value = nullptr;
  lastScopeLaunchResultType.reset();
  if (stmt.value) {
    value = generateExpression(stmt.value->value);
  }
  pendingDeclaredType.reset();
  if (!value)
    return;

  // Type coercion: if declared type doesn't match value type, try to coerce
  if (stmt.ty) {
    auto declaredType = convertType(stmt.ty->value);
    if (isValidType(declaredType))
      value = coerceType(value, declaredType, location);
  }

  // Extract the name from the pattern
  const auto &pattern = stmt.pattern.value;
  if (auto *identPat = std::get_if<ast::PatIdentifier>(&pattern.kind)) {
    auto varName = identPat->name;
    declareVariable(varName, value);

    // Track actor variable types for method call dispatch
    // Normal spawns: extract from type annotation (ActorRef<MyActor>)
    if (stmt.ty) {
      auto actorName = typeExprToActorName(stmt.ty->value);
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
                actorName = typeExprToTypeName(stmt.ty->value);
              }
              if (stmt.ty) {
                actorName = typeExprToTypeName(stmt.ty->value);
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
      auto handleStr = typeExprToHandleString(stmt.ty->value);
      if (!handleStr.empty())
        handleVarTypes[varName] = handleStr;
    }

    // ── Track first-class Stream<T> / Sink<T> variables ─────────────────
    if (stmt.ty) {
      auto streamStr = typeExprStreamKind(stmt.ty->value);
      if (!streamStr.empty())
        streamHandleVarTypes[varName] = streamStr;
    }
    // Stream channel returns a Pair (not Stream or Sink)
    if (stmt.value) {
      if (auto *ce = std::get_if<ast::ExprCall>(&stmt.value->value.kind)) {
        if (ce->function) {
          if (auto *fi = std::get_if<ast::ExprIdentifier>(&ce->function->value.kind)) {
            if (fi->name == "hew_stream_channel")
              streamHandleVarTypes[varName] = "Pair";
            else if (fi->name == "hew_stream_from_file_read" || fi->name == "hew_stream_lines" ||
                     fi->name == "hew_stream_pair_stream" || fi->name == "hew_stream_chunks" ||
                     fi->name == "hew_stream_map_string" || fi->name == "hew_stream_filter_string" ||
                     fi->name == "hew_stream_take")
              streamHandleVarTypes[varName] = "Stream";
            else if (fi->name == "hew_stream_pair_sink" ||
                     fi->name == "hew_stream_from_file_write" ||
                     fi->name == "hew_http_respond_stream")
              streamHandleVarTypes[varName] = "Sink";
          }
        }
      }
    }
    // Track MPSC channel handle variables so the codegen uses ptr not struct.
    if (stmt.value) {
      if (auto *ce = std::get_if<ast::ExprCall>(&stmt.value->value.kind)) {
        if (ce->function) {
          if (auto *fi = std::get_if<ast::ExprIdentifier>(&ce->function->value.kind)) {
            if (fi->name == "hew_channel_sender_clone" ||
                fi->name == "hew_channel_pair_sender")
              handleVarTypes[varName] = "Sender";
            else if (fi->name == "hew_channel_pair_receiver")
              handleVarTypes[varName] = "Receiver";
          }
        }
      }
    }

    // ── RAII auto-close for Stream/Sink handles ──────────────────────────
    // When a Stream/Sink goes out of scope, call the appropriate close
    // function.  An alloca tracks the live handle pointer; explicit
    // .close() nulls the alloca so the scope-exit close is a no-op.
    {
      auto sit = streamHandleVarTypes.find(varName);
      if (sit != streamHandleVarTypes.end() && value) {
        std::string closeFn;
        if (sit->second == "Stream")
          closeFn = "hew_stream_close";
        else if (sit->second == "Sink")
          closeFn = "hew_sink_close";
        else if (sit->second == "Pair")
          closeFn = "hew_stream_pair_free";
        if (!closeFn.empty()) {
          auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
          auto allocaType = mlir::MemRefType::get({}, ptrType);
          auto closeAlloca = mlir::memref::AllocaOp::create(builder, location, allocaType);
          mlir::memref::StoreOp::create(builder, location, value, closeAlloca, mlir::ValueRange{});
          if (!dropScopes.empty())
            dropScopes.back().push_back({varName, closeFn, false, closeAlloca});
        }
      }
    }

    // Vec/HashMap string getters now return owned (strdup'd) copies
    bool isBorrowedGetString = false;

    // Register drop functions from type annotation
    if (stmt.ty) {
      if (auto *named = std::get_if<ast::TypeNamed>(&stmt.ty->value.kind)) {
        auto typeName = resolveTypeAlias(named->name);
        auto *defOp = (value && value.getDefiningOp()) ? value.getDefiningOp() : nullptr;
        bool isVecCtor = defOp && mlir::isa<hew::VecNewOp>(defOp);
        bool isHashMapCtor = defOp && mlir::isa<hew::HashMapNewOp>(defOp);
        bool isHashSetCtor =
            defOp && defOp->getName().getStringRef() == "hew.runtime_call" &&
            defOp->hasAttr("callee") &&
            mlir::cast<mlir::SymbolRefAttr>(defOp->getAttr("callee")).getLeafReference() ==
                "hew_hashset_new";
        if ((typeName == "Vec" || typeName == "bytes") && isVecCtor)
          registerDroppable(varName, "hew_vec_free");
        else if (typeName == "HashMap" && isHashMapCtor)
          registerDroppable(varName, "hew_hashmap_free_impl");
        else if (typeName == "HashSet" && isHashSetCtor)
          registerDroppable(varName, "hew_hashset_free");
        else if ((typeName == "String" || typeName == "string" || typeName == "str") &&
                 !handleVarTypes.count(varName) && !streamHandleVarTypes.count(varName)) {
          // Don't register string drop for borrowed references from .get()
          bool isBorrowed = isBorrowedGetString;
          if (stmt.value) {
            if (auto *mc = std::get_if<ast::ExprMethodCall>(&stmt.value->value.kind))
              isBorrowed = (mc->method == "get");
          }
          if (!isBorrowed)
            registerDroppable(varName, "hew_string_drop");
        } else {
          auto dropIt = userDropFuncs.find(typeName);
          if (dropIt != userDropFuncs.end())
            registerDroppable(varName, dropIt->second, /*isUserDrop=*/true);
        }
      }
    }

    // Register string drops when VALUE is string-typed
    if (value && mlir::isa<hew::StringRefType>(value.getType())) {
      // Opaque handle variables (stream, sink, pair, http, regex, etc.) are
      // NOT strings — they have their own lifecycle (e.g. hew_stream_close,
      // hew_sink_close, hew_stream_pair_free).  Do NOT register them for
      // hew_string_drop.
      bool isHandle = handleVarTypes.count(varName) || streamHandleVarTypes.count(varName);

      bool alreadyRegistered = false;
      if (!dropScopes.empty()) {
        for (auto &e : dropScopes.back()) {
          if (e.varName == varName) {
            alreadyRegistered = true;
            break;
          }
        }
      }
      if (!alreadyRegistered && !isHandle) {
        bool isStringExpr = false;
        if (stmt.value) {
          const auto &vk = stmt.value->value.kind;
          isStringExpr = std::holds_alternative<ast::ExprInterpolatedString>(vk) ||
                         std::holds_alternative<ast::ExprCall>(vk);
          // Method calls that produce OWNED strings should be dropped.
          // .get() on Vec/HashMap now returns strdup'd owned copies.
          if (std::get_if<ast::ExprMethodCall>(&vk)) {
            isStringExpr = true;
          }
        }
        if (!isBorrowedGetString && isStringExpr)
          registerDroppable(varName, "hew_string_drop");
      }
    }

    // Register user-defined Drop from struct init
    if (stmt.value) {
      if (auto *si = std::get_if<ast::ExprStructInit>(&stmt.value->value.kind)) {
        bool hasTypedAnnotation =
            stmt.ty && std::holds_alternative<ast::TypeNamed>(stmt.ty->value.kind);
        if (!hasTypedAnnotation) {
          auto dropIt = userDropFuncs.find(si->name);
          if (dropIt != userDropFuncs.end())
            registerDroppable(varName, dropIt->second, /*isUserDrop=*/true);
        }
      }
    }

    // Register closure env for RAII cleanup via hew_rc_drop.
    if (mlir::isa<hew::ClosureType>(value.getType())) {
      registerDroppable(varName, "hew_rc_drop");
      if (stmt.value && std::holds_alternative<ast::ExprIdentifier>(stmt.value->value.kind)) {
        auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
        auto envPtr = hew::ClosureGetEnvOp::create(builder, location, ptrType, value);
        hew::RcCloneOp::create(builder, location, ptrType, envPtr);
      }
    }
    // Track dyn Trait variable types
    if (stmt.ty) {
      if (auto *traitObj = std::get_if<ast::TypeTraitObject>(&stmt.ty->value.kind)) {
        if (!traitObj->bounds.empty())
          dynTraitVarTypes[varName] = traitObj->bounds[0].name;
      }
    }

  } else if (auto *tuplePat = std::get_if<ast::PatTuple>(&pattern.kind)) {
    bindTuplePatternFields(*tuplePat, value, location);

    // ── Track + RAII auto-close for destructured Stream/Sink tuple elements ──
    // When `let (sink, input): (Sink<T>, Stream<T>) = stream.pipe(N)`,
    // the tuple type annotation tells us which elements are Stream/Sink.
    if (stmt.ty) {
      if (auto *tupleType = std::get_if<ast::TypeTuple>(&stmt.ty->value.kind)) {
        for (size_t i = 0; i < tupleType->elements.size() && i < tuplePat->elements.size(); ++i) {
          auto *elemIdent = std::get_if<ast::PatIdentifier>(&tuplePat->elements[i]->value.kind);
          if (!elemIdent)
            continue;
          auto elemStream = typeExprStreamKind(tupleType->elements[i].value);
          if (elemStream.empty())
            continue;
          streamHandleVarTypes[elemIdent->name] = elemStream;
          // Register RAII auto-close
          std::string closeFn;
          if (elemStream == "Stream")
            closeFn = "hew_stream_close";
          else if (elemStream == "Sink")
            closeFn = "hew_sink_close";
          else if (elemStream == "Pair")
            closeFn = "hew_stream_pair_free";
          if (!closeFn.empty()) {
            auto elemVal = lookupVariable(elemIdent->name);
            if (elemVal) {
              auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
              auto allocaType = mlir::MemRefType::get({}, ptrType);
              auto closeAlloca = mlir::memref::AllocaOp::create(builder, location, allocaType);
              mlir::memref::StoreOp::create(builder, location, elemVal, closeAlloca,
                                            mlir::ValueRange{});
              if (!dropScopes.empty())
                dropScopes.back().push_back({elemIdent->name, closeFn, false, closeAlloca});
            }
          }
        }
      }
    }
  } else if (std::holds_alternative<ast::PatStruct>(pattern.kind)) {
    bindLetSubPattern(pattern, value, location);
  } else {
    emitError(location) << "only simple identifier patterns supported for let";
  }
}

void MLIRGen::generateVarStmt(const ast::StmtVar &stmt) {
  auto location = currentLoc;
  auto varNameStr = stmt.name;
  // Set the declared type so constructors can emit correctly typed results.
  // Note: stmt.ty may contain inferred unit type `()` → NoneType, which is
  // valid here. Use convertType (not convertTypeOrError) to allow NoneType.
  if (stmt.ty)
    pendingDeclaredType = convertType(stmt.ty->value);

  // Determine the type
  mlir::Type varType;
  mlir::Value initValue = nullptr;

  if (stmt.value) {
    initValue = generateExpression(stmt.value->value);
    pendingDeclaredType.reset();
    if (!initValue)
      return;
    varType = initValue.getType();
  } else {
    pendingDeclaredType.reset();
  }

  if (stmt.ty) {
    varType = convertType(stmt.ty->value);
    if (isValidType(varType) && initValue)
      initValue = coerceType(initValue, varType, location);
  }

  if (!varType) {
    emitError(location) << "cannot determine type for var declaration";
    return;
  }

  declareMutableVariable(varNameStr, varType, initValue);

  // Track handle variables from type annotation (filled by enrich_program)
  if (stmt.ty) {
    auto handleStr = typeExprToHandleString(stmt.ty->value);
    if (!handleStr.empty())
      handleVarTypes[varNameStr] = handleStr;
  }

  // ── Track first-class Stream<T> / Sink<T> for var statements ────────────
  if (stmt.ty) {
    auto streamStr = typeExprStreamKind(stmt.ty->value);
    if (!streamStr.empty())
      streamHandleVarTypes[varNameStr] = streamStr;
  }

  // ── RAII auto-close for Stream/Sink handles (var) ───────────────────────
  {
    auto sit = streamHandleVarTypes.find(varNameStr);
    if (sit != streamHandleVarTypes.end() && initValue) {
      std::string closeFn;
      if (sit->second == "Stream")
        closeFn = "hew_stream_close";
      else if (sit->second == "Sink")
        closeFn = "hew_sink_close";
      else if (sit->second == "Pair")
        closeFn = "hew_stream_pair_free";
      if (!closeFn.empty()) {
        auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
        auto allocaType = mlir::MemRefType::get({}, ptrType);
        auto closeAlloca = mlir::memref::AllocaOp::create(builder, location, allocaType);
        mlir::memref::StoreOp::create(builder, location, initValue, closeAlloca, mlir::ValueRange{});
        if (!dropScopes.empty())
          dropScopes.back().push_back({varNameStr, closeFn, false, closeAlloca});
      }
    }
  }

  // Register drop functions for collections and strings declared with var.
  if (stmt.ty) {
    if (auto *named = std::get_if<ast::TypeNamed>(&stmt.ty->value.kind)) {
      auto typeName = resolveTypeAlias(named->name);
      auto *defOp = (initValue && initValue.getDefiningOp()) ? initValue.getDefiningOp() : nullptr;
      bool isVecCtor = defOp && mlir::isa<hew::VecNewOp>(defOp);
      bool isHashMapCtor = defOp && mlir::isa<hew::HashMapNewOp>(defOp);
      bool isHashSetCtor =
          defOp && defOp->getName().getStringRef() == "hew.runtime_call" &&
          defOp->hasAttr("callee") &&
          mlir::cast<mlir::SymbolRefAttr>(defOp->getAttr("callee")).getLeafReference() ==
              "hew_hashset_new";
      if ((typeName == "Vec" || typeName == "bytes") && isVecCtor)
        registerDroppable(varNameStr, "hew_vec_free");
      else if (typeName == "HashMap" && isHashMapCtor)
        registerDroppable(varNameStr, "hew_hashmap_free_impl");
      else if (typeName == "HashSet" && isHashSetCtor)
        registerDroppable(varNameStr, "hew_hashset_free");
      else if ((typeName == "String" || typeName == "string" || typeName == "str") &&
               !handleVarTypes.count(varNameStr) && !streamHandleVarTypes.count(varNameStr))
        registerDroppable(varNameStr, "hew_string_drop");
    }
  }
}

void MLIRGen::generateAssignStmt(const ast::StmtAssign &stmt) {
  auto location = currentLoc;

  // Handle field assignment: self.field = value (pointer-based)
  if (auto *fa = std::get_if<ast::ExprFieldAccess>(&stmt.target.value.kind)) {
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
              rhs = coerceType(rhs, field.type, location);
              bool isFloat = llvm::isa<mlir::FloatType>(field.type);
              bool isUnsigned = false;
              if (mlir::isa<mlir::IntegerType>(field.type))
                if (auto *ty = resolvedTypeOf(stmt.target.span))
                  isUnsigned = isUnsignedTypeExpr(*ty);
              rhs = emitCompoundArithOp(*stmt.op, current, rhs, isFloat, isUnsigned, location);
              if (!rhs)
                return;
            }
            rhs = coerceType(rhs, field.type, location);
            mlir::LLVM::StoreOp::create(builder, location, rhs, fieldPtr);
            return;
          }
        }
      }
      emitError(location) << "field '" << fieldName << "' not found for assignment";
      return;
    }
    // Value struct field assignment: load struct from mutable var, insertvalue, store back.
    auto *objIdent = std::get_if<ast::ExprIdentifier>(&fa->object->value.kind);
    if (!objIdent) {
      emitError(location) << "value struct field assignment requires a variable target";
      return;
    }
    auto varSlot = getMutableVarSlot(intern(objIdent->name));
    if (!varSlot) {
      emitError(location) << "cannot assign field on immutable variable '" << objIdent->name << "'";
      return;
    }

    auto fieldName = fa->field;
    auto structType = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(operandType);
    if (!structType || !structType.isIdentified()) {
      emitError(location) << "field assignment on non-struct value type";
      return;
    }
    auto stIt = structTypes.find(structType.getName().str());
    if (stIt == structTypes.end()) {
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
      emitError(location) << "field '" << fieldName << "' not found on struct '"
                          << structType.getName() << "'";
      return;
    }

    // Reload the struct from the mutable variable slot
    auto currentStruct = mlir::memref::LoadOp::create(builder, location, varSlot);
    // Handle compound assignment
    if (stmt.op) {
      auto currentFieldVal = mlir::LLVM::ExtractValueOp::create(
          builder, location, currentStruct,
          llvm::ArrayRef<int64_t>{static_cast<int64_t>(targetField->index)});
      rhs = coerceType(rhs, targetField->type, location);
      bool isFloat = llvm::isa<mlir::FloatType>(targetField->type);
      bool isUnsigned = false;
      if (mlir::isa<mlir::IntegerType>(targetField->type))
        if (auto *ty = resolvedTypeOf(stmt.target.span))
          isUnsigned = isUnsignedTypeExpr(*ty);
      rhs = emitCompoundArithOp(*stmt.op, currentFieldVal, rhs, isFloat, isUnsigned, location);
      if (!rhs)
        return;
    }
    rhs = coerceType(rhs, targetField->type, location);
    auto updated = mlir::LLVM::InsertValueOp::create(
        builder, location, currentStruct, rhs,
        llvm::ArrayRef<int64_t>{static_cast<int64_t>(targetField->index)});
    mlir::memref::StoreOp::create(builder, location, updated, varSlot);
    return;
  }

  // Handle indexed assignment: v[i] = x
  if (auto *idx = std::get_if<ast::ExprIndex>(&stmt.target.value.kind)) {
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
      rhsVal = coerceType(rhsVal, vecType.getElementType(), location);
      if (stmt.op) {
        auto currentVal = hew::VecGetOp::create(builder, location, vecType.getElementType(),
                                                collectionVal, indexVal);
        bool isFloat = llvm::isa<mlir::FloatType>(vecType.getElementType());
        bool isUnsigned = false;
        if (mlir::isa<mlir::IntegerType>(vecType.getElementType()))
          if (auto *ty = resolvedTypeOf(stmt.target.span))
            isUnsigned = isUnsignedTypeExpr(*ty);
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
        emitError(location) << "array indexed assignment requires a variable target";
        return;
      }
      auto varSlot = getMutableVarSlot(intern(ie->name));
      if (!varSlot) {
        emitError(location) << "cannot assign index on immutable variable '" << ie->name << "'";
        return;
      }

      rhsVal = coerceType(rhsVal, hewArrayType.getElementType(), location);
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
        bool isUnsigned = false;
        if (mlir::isa<mlir::IntegerType>(hewArrayType.getElementType()))
          if (auto *ty = resolvedTypeOf(stmt.target.span))
            isUnsigned = isUnsignedTypeExpr(*ty);
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

    emitError(location) << "unsupported indexed assignment target";
    return;
  }

  // Get the target variable name
  auto *targetIdent = std::get_if<ast::ExprIdentifier>(&stmt.target.value.kind);
  if (!targetIdent) {
    emitError(location) << "only simple identifier targets supported for assignment";
    return;
  }

  auto name = targetIdent->name;

  mlir::Value rhs = generateExpression(stmt.value.value);
  if (!rhs)
    return;

  // Check local variables first — mirrors the read path in MLIRGenExpr so that
  // reads and writes target the same storage when a local shadows a field.
  auto existingVar = lookupVariable(name);
  if (existingVar) {
    // Assign to local variable
    if (stmt.op) {
      rhs = coerceType(rhs, existingVar.getType(), location);
      auto type = existingVar.getType();
      bool isFloat = llvm::isa<mlir::FloatType>(type);
      bool isUnsigned = false;
      if (mlir::isa<mlir::IntegerType>(type))
        if (auto *ty = resolvedTypeOf(stmt.target.span))
          isUnsigned = isUnsignedTypeExpr(*ty);

      auto result = emitCompoundArithOp(*stmt.op, existingVar, rhs, isFloat, isUnsigned, location);
      if (!result)
        return;
      storeVariable(name, result);
    } else {
      rhs = coerceType(rhs, existingVar.getType(), location);
      // Drop old owned value before overwriting to prevent memory leaks.
      // Only safe when RHS is a fresh allocation (not loaded from another
      // variable), to avoid double-free from shared ownership.
      if (rhs && !rhs.getDefiningOp<mlir::memref::LoadOp>() && !mlir::isa<mlir::BlockArgument>(rhs))
        emitDropForVariable(name);
      storeVariable(name, rhs);
    }
    return;
  }

  // Inside actor init/receive, bare field names (e.g. `handle = ...`) fall
  // through to actor state fields via GEP into the actor state struct.
  if (!currentActorName.empty()) {
    auto selfVal = lookupVariable("self");
    if (selfVal) {
      auto actorIt = structTypes.find(currentActorName);
      if (actorIt != structTypes.end()) {
        auto structType = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(actorIt->second.mlirType);
        if (structType) {
          for (const auto &field : actorIt->second.fields) {
            if (field.name == name) {
              auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
              auto fieldPtr = mlir::LLVM::GEPOp::create(
                  builder, location, ptrType, structType, selfVal,
                  llvm::ArrayRef<mlir::LLVM::GEPArg>{0, static_cast<int32_t>(field.index)});
              if (stmt.op) {
                auto current =
                    mlir::LLVM::LoadOp::create(builder, location, field.type, fieldPtr).getResult();
                rhs = coerceType(rhs, field.type, location);
                bool isFloat = llvm::isa<mlir::FloatType>(field.type);
                bool isUnsigned = false;
                if (mlir::isa<mlir::IntegerType>(field.type))
                  if (auto *ty = resolvedTypeOf(stmt.target.span))
                    isUnsigned = isUnsignedTypeExpr(*ty);
                rhs = emitCompoundArithOp(*stmt.op, current, rhs, isFloat, isUnsigned, location);
                if (!rhs)
                  return;
              }
              rhs = coerceType(rhs, field.type, location);
              mlir::LLVM::StoreOp::create(builder, location, rhs, fieldPtr);
              return;
            }
          }
        }
      }
    }
  }

  emitError(location) << "undefined variable '" << name << "' in assignment";
}

void MLIRGen::generateIfStmt(const ast::StmtIf &stmt) {
  auto location = currentLoc;

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
  generateBlock(stmt.then_block);
  ensureYieldTerminator(location);

  if (hasElse) {
    builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
    const auto &elseBlock = *stmt.else_block;
    if (elseBlock.is_if && elseBlock.if_stmt) {
      if (auto *innerIf = std::get_if<ast::StmtIf>(&elseBlock.if_stmt->value.kind))
        generateIfStmt(*innerIf);
    } else if (elseBlock.block) {
      generateBlock(*elseBlock.block);
    }
    ensureYieldTerminator(location);
  }

  builder.setInsertionPointAfter(ifOp);
}

// ============================================================================
// If statement as expression (value-producing if at end of block)
// ============================================================================

mlir::Value MLIRGen::generateIfStmtAsExpr(const ast::StmtIf &stmt) {
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
    // Statement-position if-else: result is discarded, type is irrelevant.
    // TODO: generate statement-position if-else without value-producing path.
    resultType = defaultIntType();
  }

  bool hasElse = stmt.else_block.has_value();
  if (!hasElse) {
    generateIfStmt(stmt);
    return nullptr;
  }

  auto ifOp = mlir::scf::IfOp::create(builder, location, resultType, cond, /*withElseRegion=*/true);

  builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
  mlir::Value thenVal = generateBlock(stmt.then_block);
  auto *thenBlock = builder.getInsertionBlock();
  if (thenBlock->empty() || !thenBlock->back().hasTrait<mlir::OpTrait::IsTerminator>()) {
    if (thenVal) {
      thenVal = coerceType(thenVal, resultType, location);
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
  auto *elseBlk = builder.getInsertionBlock();
  if (elseBlk->empty() || !elseBlk->back().hasTrait<mlir::OpTrait::IsTerminator>()) {
    if (elseVal) {
      elseVal = coerceType(elseVal, resultType, location);
      mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{elseVal});
    } else {
      auto defVal = createDefaultValue(builder, location, resultType);
      mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{defVal});
    }
  }

  builder.setInsertionPointAfter(ifOp);
  return ifOp.getResult(0);
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
  if (auto *typeExpr = resolvedTypeOf(stmt.iterable.span))
    isBytesStream = typeExprStreamElement(*typeExpr) == "bytes";

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
  bool isIntChannel = false;
  bool isStringChannel = false;
  if (receiverType && receiverType->type_args && !receiverType->type_args->empty()) {
    if (auto *inner = std::get_if<ast::TypeNamed>(&(*receiverType->type_args)[0].value.kind)) {
      isIntChannel = (inner->name == "int" || inner->name == "i64");
      if (!isIntChannel)
        isStringChannel = (inner->name == "String" || inner->name == "string");
    }
  } else {
    // No type args: default to String for backward compatibility.
    isStringChannel = true;
  }

  if (!isIntChannel && !isStringChannel) {
    emitError(location) << "for await on Receiver<T> is currently only supported for String and int";
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

    auto validFlag = mlir::LLVM::LoadOp::create(builder, location, i32Type, channelIntOutValidAlloca);
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

  // Fast path: if the iterable is a first-class Stream<T> variable, delegate.
  // NOTE: ExprMethodCall on actors is intentionally excluded — those use mailbox iteration.
  if (auto *identExpr = std::get_if<ast::ExprIdentifier>(&stmt.iterable.value.kind)) {
    bool isStreamVar = false;
    // Prefer resolved type from the type checker
    if (auto *typeExpr = resolvedTypeOf(stmt.iterable.span))
      isStreamVar = typeExprStreamKind(*typeExpr) == "Stream";
    // Fall back to map lookup
    if (!isStreamVar) {
      auto sit = streamHandleVarTypes.find(identExpr->name);
      isStreamVar = (sit != streamHandleVarTypes.end() && sit->second == "Stream");
    }
    if (isStreamVar) {
      generateForStreamStmt(stmt);
      return;
    }
  }

  // Check resolved type for any iterable expression.  Method calls that
  // return Stream<T> (e.g. stream.filter(f), stream.map(f)) use the stream
  // iteration path.  Actor receive-gen methods also resolve to Stream<T>,
  // so for ExprMethodCall we must check the RECEIVER type: if it is already
  // Stream<T>, the method is a stream combinator; if it is ActorRef<T>,
  // it falls through to the mailbox iteration path below.
  if (auto *mc = std::get_if<ast::ExprMethodCall>(&stmt.iterable.value.kind)) {
    if (mc->receiver) {
      if (auto *recvType = resolvedTypeOf(mc->receiver->span)) {
        if (typeExprStreamKind(*recvType) == "Stream") {
          generateForStreamStmt(stmt);
          return;
        }
      }
    }
  } else {
    if (auto *typeExpr = resolvedTypeOf(stmt.iterable.span)) {
      if (typeExprStreamKind(*typeExpr) == "Stream") {
        generateForStreamStmt(stmt);
        return;
      }
    }
    // Fall back to recognizing stream-returning C function calls by name.
    if (auto *ce = std::get_if<ast::ExprCall>(&stmt.iterable.value.kind)) {
      if (ce->function) {
        if (auto *fi = std::get_if<ast::ExprIdentifier>(&ce->function->value.kind)) {
          if (fi->name == "hew_stream_lines" || fi->name == "hew_stream_chunks" ||
              fi->name == "hew_stream_from_file_read" || fi->name == "hew_stream_pair_stream" ||
              fi->name == "hew_stream_map_string" || fi->name == "hew_stream_filter_string" ||
              fi->name == "hew_stream_take") {
            generateForStreamStmt(stmt);
            return;
          }
        }
      }
    }
  }

  // Check if the iterable is a Receiver<T> variable for channel iteration.
  if (auto *identExpr = std::get_if<ast::ExprIdentifier>(&stmt.iterable.value.kind)) {
    if (auto *typeExpr = resolvedTypeOf(stmt.iterable.span)) {
      auto *named = std::get_if<ast::TypeNamed>(&typeExpr->kind);
      if (named && (named->name == "Receiver" || named->name == "channel.Receiver")) {
        generateForReceiverStmt(stmt, named);
        return;
      }
    }
    // Also check the handle-var type map (fallback).
    auto hit = handleVarTypes.find(identExpr->name);
    if (hit != handleVarTypes.end() && hit->second == "Receiver") {
      generateForReceiverStmt(stmt, nullptr);
      return;
    }
  }

  // The iterable must be a method call on an actor: actor.method(args)
  auto *mc = std::get_if<ast::ExprMethodCall>(&stmt.iterable.value.kind);
  if (!mc) {
    emitError(location) << "for await requires an actor method call as iterable";
    return;
  }

  // Resolve the receiver as an actor variable
  if (!mc->receiver) {
    emitError(location) << "for await: receiver must be an actor variable";
    return;
  }
  auto *recvIdent = std::get_if<ast::ExprIdentifier>(&mc->receiver->value.kind);
  if (!recvIdent) {
    emitError(location) << "for await: receiver must be an actor variable";
    return;
  }
  std::string receiverName = recvIdent->name;
  std::string methodName = mc->method;

  auto avIt = actorVarTypes.find(receiverName);
  if (avIt == actorVarTypes.end()) {
    emitError(location) << "for await: '" << receiverName << "' is not a known actor variable";
    return;
  }
  std::string actorTypeName = avIt->second;

  auto arIt = actorRegistry.find(actorTypeName);
  if (arIt == actorRegistry.end()) {
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
    emitError(location) << "for await: missing __next handler for '" << methodName << "' on '"
                        << actorTypeName << "'";
    return;
  }

  // The wrapper return type is { i8, YieldType }
  auto wrapperType = *initInfo->returnType;
  auto wrapperStructType = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(wrapperType);
  if (!wrapperStructType || wrapperStructType.getBody().size() != 2) {
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
  bool rangeIsUnsigned = false;
  if (rangeExpr.left) {
    if (auto *ty = resolvedTypeOf(rangeExpr.left->span))
      rangeIsUnsigned = isUnsignedTypeExpr(*ty);
  }

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

  std::string nextName = genFuncName + "__next";
  std::string doneName = genFuncName + "__done";

  auto nextFuncOp = module.lookupSymbol<mlir::func::FuncOp>(nextName);
  auto doneFuncOp = module.lookupSymbol<mlir::func::FuncOp>(doneName);
  if (!nextFuncOp || !doneFuncOp) {
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
        *typeExpr, [this](const std::string &n) { return resolveTypeAlias(n); });
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
    auto tupleType = mlir::cast<hew::HewTupleType>(collection.getType());
    auto elemType = tupleType.getElementTypes()[0];

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
      lbI64 = mlir::arith::ExtSIOp::create(builder, location, i64Type, startVal);
      ubI64 = mlir::arith::ExtSIOp::create(builder, location, i64Type, endVal);
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
    auto cond = mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::slt,
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
          emitError(location) << "unsupported for-loop Vec element type for iterable '" << collType
                              << "'";
          return;
        }
        elem = hew::VecGetOp::create(builder, location, elemType, collection, idx);
      }
    }

    // Bind element to the pattern variable
    if (auto *identPat = std::get_if<ast::PatIdentifier>(&stmt.pattern.value.kind)) {
      auto patName = identPat->name;
      declareVariable(patName, elem);
      // Register loop variable for actor dispatch when iterating Vec<ActorRef<T>>
      if (isVecPtr && collType.find("Vec<ActorRef<") == 0) {
        auto start = std::string("Vec<ActorRef<").size();
        auto end = collType.rfind(">>");
        if (end != std::string::npos) {
          std::string innerActorName = collType.substr(start, end - start);
          actorVarTypes[patName] = innerActorName;
        }
      }
    }

    // Generate loop body
    pushDropScope();
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
    if (auto *tuplePat = std::get_if<ast::PatTuple>(&stmt.pattern.value.kind)) {
      if (tuplePat->elements.size() >= 1) {
        if (auto *kIdent = std::get_if<ast::PatIdentifier>(&tuplePat->elements[0]->value.kind)) {
          declareVariable(kIdent->name, key);
        }
      }
      if (tuplePat->elements.size() >= 2) {
        if (auto *vIdent = std::get_if<ast::PatIdentifier>(&tuplePat->elements[1]->value.kind)) {
          declareVariable(vIdent->name, val);
        }
      }
    } else if (auto *identPat = std::get_if<ast::PatIdentifier>(&stmt.pattern.value.kind)) {
      declareVariable(identPat->name, key);
    }

    // Generate loop body
    pushDropScope();
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
    if (stmt.value && returnSlot) {
      auto val = generateExpression(stmt.value->value);
      if (val) {
        auto slotType = mlir::cast<mlir::MemRefType>(returnSlot.getType()).getElementType();
        val = coerceType(val, slotType, location);
        mlir::memref::StoreOp::create(builder, location, val, returnSlot);
      }
    }
    auto trueVal = createIntConstant(builder, location, builder.getI1Type(), 1);
    mlir::memref::StoreOp::create(builder, location, trueVal, returnFlag);
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
          val = coerceType(val, currentFunction.getResultTypes()[0], location);
        // Collect simple identifier references to exclude from drops (returning
        // a variable directly means its storage must not be freed yet).
        std::set<std::string> returnVarNames;
        if (auto *id = std::get_if<ast::ExprIdentifier>(&stmt.value->value.kind)) {
          returnVarNames.insert(id->name);
        } else if (auto *si = std::get_if<ast::ExprStructInit>(&stmt.value->value.kind)) {
          for (const auto &[fieldName, fieldVal] : si->fields) {
            if (auto *id = std::get_if<ast::ExprIdentifier>(&fieldVal->value.kind))
              returnVarNames.insert(id->name);
          }
        }
        if (!returnVarNames.empty())
          emitDropsExcept(returnVarNames);
        else
          emitAllDrops();
        mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{val});
      } else {
        emitAllDrops();
        mlir::func::ReturnOp::create(builder, location);
      }
    } else {
      emitAllDrops();
      mlir::func::ReturnOp::create(builder, location);
    }
  }
}

void MLIRGen::generateExprStmt(const ast::StmtExpression &stmt) {
  generateExpression(stmt.expr.value);
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
