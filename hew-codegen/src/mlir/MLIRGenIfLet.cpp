//===- MLIRGenIfLet.cpp - If-let codegen for Hew MLIRGen ------------------===//
//
// If-let statement/expression generation: generateIfLetStmt, generateIfLetExpr
//
// If-let is semantically equivalent to a 1-arm match with an optional else.
// We generate the pattern test and branch logic directly.
//
//===----------------------------------------------------------------------===//

#include "hew/mlir/HewDialect.h"
#include "hew/mlir/HewOps.h"
#include "hew/mlir/HewTypes.h"
#include "hew/mlir/MLIRGen.h"
#include "MLIRGenHelpers.h"

#include "mlir/Dialect/Arith/IR/Arith.h"
#include "mlir/Dialect/LLVMIR/LLVMDialect.h"
#include "mlir/Dialect/SCF/IR/SCF.h"
#include "mlir/IR/Builders.h"
#include "mlir/IR/BuiltinTypes.h"
#include "mlir/IR/Diagnostics.h"

#include "llvm/ADT/ScopedHashTable.h"
#include "llvm/ADT/StringRef.h"

#include <string>

using namespace hew;
using namespace mlir;

// ============================================================================
// If-let statement generation
// ============================================================================

void MLIRGen::generateIfLetStmt(const ast::StmtIfLet &stmt) {
  auto location = currentLoc;

  if (!stmt.expr)
    return;

  // Generate the scrutinee expression
  auto scrutinee = generateExpression(stmt.expr->value);
  if (!scrutinee)
    return;

  const auto &pattern = stmt.pattern.value;
  bool hasElse = stmt.else_body.has_value();

  // Wildcard: always matches, no bindings, no enum deref needed.
  if (std::holds_alternative<ast::PatWildcard>(pattern.kind)) {
    auto trueVal = createIntConstant(builder, location, builder.getI1Type(), 1);
    auto ifOp = mlir::scf::IfOp::create(builder, location, mlir::TypeRange{}, trueVal, hasElse);
    builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
    generateBlock(stmt.body, /*statementPosition=*/true);
    ensureYieldTerminator(location);
    if (hasElse) {
      builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
      generateBlock(*stmt.else_body, /*statementPosition=*/true);
      ensureYieldTerminator(location);
    }
    builder.setInsertionPointAfter(ifOp);
    return;
  }

  // Identifier: always matches, bind the whole scrutinee to the named variable.
  if (auto *identPat = std::get_if<ast::PatIdentifier>(&pattern.kind)) {
    auto trueVal = createIntConstant(builder, location, builder.getI1Type(), 1);
    auto ifOp = mlir::scf::IfOp::create(builder, location, mlir::TypeRange{}, trueVal, hasElse);
    builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
    {
      SymbolTableScopeT scope(symbolTable);
      MutableTableScopeT mutScope(mutableVars);
      declareVariable(intern(identPat->name), scrutinee);
      generateBlock(stmt.body, /*statementPosition=*/true);
      ensureYieldTerminator(location);
    }
    if (hasElse) {
      builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
      generateBlock(*stmt.else_body, /*statementPosition=*/true);
      ensureYieldTerminator(location);
    }
    builder.setInsertionPointAfter(ifOp);
    return;
  }

  // Constructor: enum tag-test with indirect-enum deref.
  auto *ctorPat = std::get_if<ast::PatConstructor>(&pattern.kind);
  if (!ctorPat) {
    ++errorCount_;
    emitError(location) << "if-let only supports constructor, wildcard, and identifier patterns";
    return;
  }

  // Indirect enum: dereference pointer to get the inner struct
  scrutinee = derefIndirectEnumScrutinee(scrutinee, stmt.expr->span, location);
  if (!scrutinee)
    return;

  const auto &ctorName = ctorPat->name;
  auto ctorVarIt = variantLookup.find(ctorName);
  if (ctorVarIt == variantLookup.end()) {
    emitError(location) << "unknown constructor '" << ctorName << "' in if-let pattern";
    return;
  }

  auto variantIndex = static_cast<int64_t>(ctorVarIt->second.second);
  mlir::Value cond = emitTagEqualCondition(scrutinee, variantIndex, location);

  // Create scf.if for the branch
  auto ifOp = mlir::scf::IfOp::create(builder, location, mlir::TypeRange{}, cond, hasElse);

  // Then region: pattern matches
  builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
  {
    SymbolTableScopeT scope(symbolTable);
    MutableTableScopeT mutScope(mutableVars);

    bindConstructorPatternVars(*ctorPat, scrutinee, location);

    // Generate the then body
    generateBlock(stmt.body, /*statementPosition=*/true);

    ensureYieldTerminator(location);
  }

  // Else region (if present)
  if (hasElse) {
    builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
    generateBlock(*stmt.else_body, /*statementPosition=*/true);

    ensureYieldTerminator(location);
  }

  builder.setInsertionPointAfter(ifOp);
}

// ============================================================================
// If-let expression generation
// ============================================================================

mlir::Value MLIRGen::generateIfLetExpr(const ast::ExprIfLet &expr, const ast::Span &exprSpan) {
  auto location = currentLoc;

  if (!expr.expr)
    return nullptr;

  // Generate the scrutinee expression
  auto scrutinee = generateExpression(expr.expr->value);
  if (!scrutinee)
    return nullptr;

  // Use the type checker's resolved type for this if-let expression
  mlir::Type resultType = nullptr;
  if (auto *resolvedType = resolvedTypeOf(exprSpan)) {
    resultType = convertType(*resolvedType);
  }

  if (!resultType) {
    emitError(location) << "cannot determine result type for if-let expression";
    return nullptr;
  }

  const auto &pattern = expr.pattern.value;

  // Wildcard: always matches, no bindings, no enum deref needed.
  if (std::holds_alternative<ast::PatWildcard>(pattern.kind)) {
    auto trueVal = createIntConstant(builder, location, builder.getI1Type(), 1);
    auto ifOp =
        mlir::scf::IfOp::create(builder, location, resultType, trueVal, /*withElseRegion=*/true);
    builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
    {
      mlir::Value thenVal = generateBlock(expr.body);
      auto *thenBlock = builder.getInsertionBlock();
      if (thenBlock->empty() || !thenBlock->back().hasTrait<mlir::OpTrait::IsTerminator>()) {
        auto yieldVal = thenVal ? coerceType(thenVal, resultType, location)
                                : createDefaultValue(builder, location, resultType);
        mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{yieldVal});
      }
    }
    builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
    {
      mlir::Value elseVal = expr.else_body ? generateBlock(*expr.else_body)
                                           : createDefaultValue(builder, location, resultType);
      auto *elseBlock = builder.getInsertionBlock();
      if (elseBlock->empty() || !elseBlock->back().hasTrait<mlir::OpTrait::IsTerminator>()) {
        auto yieldVal = elseVal ? coerceType(elseVal, resultType, location)
                                : createDefaultValue(builder, location, resultType);
        mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{yieldVal});
      }
    }
    builder.setInsertionPointAfter(ifOp);
    return ifOp.getResult(0);
  }

  // Identifier: always matches, bind the whole scrutinee to the named variable.
  if (auto *identPat = std::get_if<ast::PatIdentifier>(&pattern.kind)) {
    auto trueVal = createIntConstant(builder, location, builder.getI1Type(), 1);
    auto ifOp =
        mlir::scf::IfOp::create(builder, location, resultType, trueVal, /*withElseRegion=*/true);
    builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
    {
      SymbolTableScopeT scope(symbolTable);
      MutableTableScopeT mutScope(mutableVars);
      declareVariable(intern(identPat->name), scrutinee);
      mlir::Value thenVal = generateBlock(expr.body);
      auto *thenBlock = builder.getInsertionBlock();
      if (thenBlock->empty() || !thenBlock->back().hasTrait<mlir::OpTrait::IsTerminator>()) {
        auto yieldVal = thenVal ? coerceType(thenVal, resultType, location)
                                : createDefaultValue(builder, location, resultType);
        mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{yieldVal});
      }
    }
    builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
    {
      mlir::Value elseVal = expr.else_body ? generateBlock(*expr.else_body)
                                           : createDefaultValue(builder, location, resultType);
      auto *elseBlock = builder.getInsertionBlock();
      if (elseBlock->empty() || !elseBlock->back().hasTrait<mlir::OpTrait::IsTerminator>()) {
        auto yieldVal = elseVal ? coerceType(elseVal, resultType, location)
                                : createDefaultValue(builder, location, resultType);
        mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{yieldVal});
      }
    }
    builder.setInsertionPointAfter(ifOp);
    return ifOp.getResult(0);
  }

  // Constructor: enum tag-test with indirect-enum deref.
  auto *ctorPat = std::get_if<ast::PatConstructor>(&pattern.kind);
  if (!ctorPat) {
    ++errorCount_;
    emitError(location) << "if-let only supports constructor, wildcard, and identifier patterns";
    return nullptr;
  }

  // Indirect enum: dereference pointer to get the inner struct
  scrutinee = derefIndirectEnumScrutinee(scrutinee, expr.expr->span, location);
  if (!scrutinee)
    return nullptr;

  const auto &ctorName = ctorPat->name;
  auto ctorVarIt = variantLookup.find(ctorName);
  if (ctorVarIt == variantLookup.end()) {
    emitError(location) << "unknown constructor '" << ctorName << "' in if-let pattern";
    return nullptr;
  }

  auto variantIndex = static_cast<int64_t>(ctorVarIt->second.second);
  mlir::Value cond = emitTagEqualCondition(scrutinee, variantIndex, location);

  // Create scf.if with result
  auto ifOp = mlir::scf::IfOp::create(builder, location, resultType, cond, /*withElseRegion=*/true);

  // Then region: pattern matches
  builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
  mlir::Value thenVal;
  {
    SymbolTableScopeT scope(symbolTable);
    MutableTableScopeT mutScope(mutableVars);

    bindConstructorPatternVars(*ctorPat, scrutinee, location);

    // Generate the then body
    thenVal = generateBlock(expr.body);

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
  }

  // Else region
  builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
  mlir::Value elseVal;
  if (expr.else_body) {
    elseVal = generateBlock(*expr.else_body);
  } else {
    elseVal = createDefaultValue(builder, location, resultType);
  }

  auto *elseBlock = builder.getInsertionBlock();
  if (elseBlock->empty() || !elseBlock->back().hasTrait<mlir::OpTrait::IsTerminator>()) {
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
