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
#include "mlir/Dialect/ControlFlow/IR/ControlFlowOps.h"
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
#include "mlir/IR/Verifier.h"

#include "llvm/ADT/ScopedHashTable.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <functional>
#include <string>

using namespace hew;
using namespace mlir;

// Extract the payload type at a given struct field index from any enum-like type
static mlir::Type getEnumFieldType(mlir::Type type, int64_t idx) {
  if (auto st = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(type))
    return st.getBody()[idx];
  if (auto ot = mlir::dyn_cast<hew::OptionEnumType>(type)) {
    if (idx == 1)
      return ot.getInnerType();
    return mlir::IntegerType::get(type.getContext(), 32);
  }
  if (auto rt = mlir::dyn_cast<hew::ResultEnumType>(type)) {
    if (idx == 1)
      return rt.getOkType();
    if (idx == 2)
      return rt.getErrType();
    return mlir::IntegerType::get(type.getContext(), 32);
  }
  return nullptr;
}

static bool isEnumLikeType(mlir::Type type) {
  return mlir::isa<mlir::LLVM::LLVMStructType>(type) || mlir::isa<hew::OptionEnumType>(type) ||
         mlir::isa<hew::ResultEnumType>(type);
}

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

  // Check if this is a constructor pattern (e.g., Some(x))
  auto *ctorPat = std::get_if<ast::PatConstructor>(&pattern.kind);
  if (!ctorPat) {
    emitError(location) << "if-let currently only supports constructor patterns";
    return;
  }

  std::string ctorName = ctorPat->name;
  auto ctorVarIt = variantLookup.find(ctorName);
  if (ctorVarIt == variantLookup.end()) {
    emitError(location) << "unknown constructor '" << ctorName << "' in if-let pattern";
    return;
  }

  auto variantIndex = static_cast<int64_t>(ctorVarIt->second.second);

  // Extract tag and compare
  auto tag = builder.create<hew::EnumExtractTagOp>(location, builder.getI32Type(), scrutinee);
  auto tagVal = createIntConstant(builder, location, builder.getI32Type(), variantIndex);
  mlir::Value cond =
      builder.create<mlir::arith::CmpIOp>(location, mlir::arith::CmpIPredicate::eq, tag, tagVal)
          .getResult();

  // Create scf.if for the branch
  bool hasElse = stmt.else_body.has_value();
  auto ifOp = builder.create<mlir::scf::IfOp>(location, mlir::TypeRange{}, cond, hasElse);

  // Then region: pattern matches
  builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
  {
    SymbolTableScopeT scope(symbolTable);
    MutableTableScopeT mutScope(mutableVars);

    // Bind pattern variables
    auto payloadFieldIndexForVariant = [&](llvm::StringRef varName,
                                           size_t payloadOrdinal) -> int64_t {
      auto varIt = variantLookup.find(varName.str());
      if (varIt == variantLookup.end())
        return 1 + static_cast<int64_t>(payloadOrdinal);

      const auto &enumName = varIt->second.first;
      const auto vIdx = varIt->second.second;

      auto enumIt = enumTypes.find(enumName);
      if (enumIt != enumTypes.end()) {
        for (const auto &variant : enumIt->second.variants) {
          if (variant.index != vIdx)
            continue;
          if (payloadOrdinal < variant.payloadPositions.size())
            return variant.payloadPositions[payloadOrdinal];
          break;
        }
      }

      return enumPayloadFieldIndex(enumName, static_cast<int32_t>(vIdx),
                                   static_cast<int64_t>(payloadOrdinal));
    };

    std::function<void(const ast::PatTuple &, mlir::Value)> bindTuplePatternFields;
    bindTuplePatternFields = [&](const ast::PatTuple &tp, mlir::Value tupleValue) -> void {
      for (size_t j = 0; j < tp.elements.size(); ++j) {
        const auto &elem = tp.elements[j];
        mlir::Value elemVal;
        if (auto hewTuple = mlir::dyn_cast<hew::HewTupleType>(tupleValue.getType())) {
          elemVal = builder.create<hew::TupleExtractOp>(location, hewTuple.getElementTypes()[j],
                                                        tupleValue, static_cast<int64_t>(j));
        } else {
          elemVal = builder.create<mlir::LLVM::ExtractValueOp>(
              location, tupleValue, llvm::ArrayRef<int64_t>{static_cast<int64_t>(j)});
        }
        if (auto *elemIdent = std::get_if<ast::PatIdentifier>(&elem->value.kind)) {
          declareVariable(elemIdent->name, elemVal);
        } else if (auto *elemTuple = std::get_if<ast::PatTuple>(&elem->value.kind)) {
          bindTuplePatternFields(*elemTuple, elemVal);
        }
      }
    };

    for (size_t i = 0; i < ctorPat->patterns.size(); ++i) {
      const auto &subPat = ctorPat->patterns[i]->value;
      if (auto *subIdent = std::get_if<ast::PatIdentifier>(&subPat.kind)) {
        if (isEnumLikeType(scrutinee.getType())) {
          int64_t fieldIdx = payloadFieldIndexForVariant(ctorName, i);
          auto fieldTy = getEnumFieldType(scrutinee.getType(), fieldIdx);
          auto payloadVal =
              builder.create<hew::EnumExtractPayloadOp>(location, fieldTy, scrutinee, fieldIdx);
          declareVariable(subIdent->name, payloadVal);
        }
      } else if (auto *subTuple = std::get_if<ast::PatTuple>(&subPat.kind)) {
        if (isEnumLikeType(scrutinee.getType())) {
          int64_t fieldIdx = payloadFieldIndexForVariant(ctorName, i);
          auto fieldTy = getEnumFieldType(scrutinee.getType(), fieldIdx);
          auto payloadVal =
              builder.create<hew::EnumExtractPayloadOp>(location, fieldTy, scrutinee, fieldIdx);
          bindTuplePatternFields(*subTuple, payloadVal);
        }
      }
    }

    // Generate the then body
    generateBlock(stmt.body);

    auto *thenBlock = builder.getInsertionBlock();
    if (thenBlock->empty() || !thenBlock->back().hasTrait<mlir::OpTrait::IsTerminator>()) {
      builder.create<mlir::scf::YieldOp>(location);
    }
  }

  // Else region (if present)
  if (hasElse) {
    builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
    generateBlock(*stmt.else_body);

    auto *elseBlock = builder.getInsertionBlock();
    if (elseBlock->empty() || !elseBlock->back().hasTrait<mlir::OpTrait::IsTerminator>()) {
      builder.create<mlir::scf::YieldOp>(location);
    }
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

  // Check if this is a constructor pattern (e.g., Some(x))
  auto *ctorPat = std::get_if<ast::PatConstructor>(&pattern.kind);
  if (!ctorPat) {
    emitError(location) << "if-let currently only supports constructor patterns";
    return nullptr;
  }

  std::string ctorName = ctorPat->name;
  auto ctorVarIt = variantLookup.find(ctorName);
  if (ctorVarIt == variantLookup.end()) {
    emitError(location) << "unknown constructor '" << ctorName << "' in if-let pattern";
    return nullptr;
  }

  auto variantIndex = static_cast<int64_t>(ctorVarIt->second.second);

  // Extract tag and compare
  auto tag = builder.create<hew::EnumExtractTagOp>(location, builder.getI32Type(), scrutinee);
  auto tagVal = createIntConstant(builder, location, builder.getI32Type(), variantIndex);
  mlir::Value cond =
      builder.create<mlir::arith::CmpIOp>(location, mlir::arith::CmpIPredicate::eq, tag, tagVal)
          .getResult();

  // Create scf.if with result
  auto ifOp = builder.create<mlir::scf::IfOp>(location, resultType, cond, /*withElseRegion=*/true);

  // Then region: pattern matches
  builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
  mlir::Value thenVal;
  {
    SymbolTableScopeT scope(symbolTable);
    MutableTableScopeT mutScope(mutableVars);

    // Bind pattern variables
    auto payloadFieldIndexForVariant = [&](llvm::StringRef varName,
                                           size_t payloadOrdinal) -> int64_t {
      auto varIt = variantLookup.find(varName.str());
      if (varIt == variantLookup.end())
        return 1 + static_cast<int64_t>(payloadOrdinal);

      const auto &enumName = varIt->second.first;
      const auto vIdx = varIt->second.second;

      auto enumIt = enumTypes.find(enumName);
      if (enumIt != enumTypes.end()) {
        for (const auto &variant : enumIt->second.variants) {
          if (variant.index != vIdx)
            continue;
          if (payloadOrdinal < variant.payloadPositions.size())
            return variant.payloadPositions[payloadOrdinal];
          break;
        }
      }

      return enumPayloadFieldIndex(enumName, static_cast<int32_t>(vIdx),
                                   static_cast<int64_t>(payloadOrdinal));
    };

    std::function<void(const ast::PatTuple &, mlir::Value)> bindTuplePatternFields;
    bindTuplePatternFields = [&](const ast::PatTuple &tp, mlir::Value tupleValue) -> void {
      for (size_t j = 0; j < tp.elements.size(); ++j) {
        const auto &elem = tp.elements[j];
        mlir::Value elemVal;
        if (auto hewTuple = mlir::dyn_cast<hew::HewTupleType>(tupleValue.getType())) {
          elemVal = builder.create<hew::TupleExtractOp>(location, hewTuple.getElementTypes()[j],
                                                        tupleValue, static_cast<int64_t>(j));
        } else {
          elemVal = builder.create<mlir::LLVM::ExtractValueOp>(
              location, tupleValue, llvm::ArrayRef<int64_t>{static_cast<int64_t>(j)});
        }
        if (auto *elemIdent = std::get_if<ast::PatIdentifier>(&elem->value.kind)) {
          declareVariable(elemIdent->name, elemVal);
        } else if (auto *elemTuple = std::get_if<ast::PatTuple>(&elem->value.kind)) {
          bindTuplePatternFields(*elemTuple, elemVal);
        }
      }
    };

    for (size_t i = 0; i < ctorPat->patterns.size(); ++i) {
      const auto &subPat = ctorPat->patterns[i]->value;
      if (auto *subIdent = std::get_if<ast::PatIdentifier>(&subPat.kind)) {
        if (isEnumLikeType(scrutinee.getType())) {
          int64_t fieldIdx = payloadFieldIndexForVariant(ctorName, i);
          auto fieldTy = getEnumFieldType(scrutinee.getType(), fieldIdx);
          auto payloadVal =
              builder.create<hew::EnumExtractPayloadOp>(location, fieldTy, scrutinee, fieldIdx);
          declareVariable(subIdent->name, payloadVal);
        }
      } else if (auto *subTuple = std::get_if<ast::PatTuple>(&subPat.kind)) {
        if (isEnumLikeType(scrutinee.getType())) {
          int64_t fieldIdx = payloadFieldIndexForVariant(ctorName, i);
          auto fieldTy = getEnumFieldType(scrutinee.getType(), fieldIdx);
          auto payloadVal =
              builder.create<hew::EnumExtractPayloadOp>(location, fieldTy, scrutinee, fieldIdx);
          bindTuplePatternFields(*subTuple, payloadVal);
        }
      }
    }

    // Generate the then body
    thenVal = generateBlock(expr.body);

    auto *thenBlock = builder.getInsertionBlock();
    if (thenBlock->empty() || !thenBlock->back().hasTrait<mlir::OpTrait::IsTerminator>()) {
      if (thenVal) {
        thenVal = coerceType(thenVal, resultType, location);
        builder.create<mlir::scf::YieldOp>(location, mlir::ValueRange{thenVal});
      } else {
        auto defVal = createDefaultValue(builder, location, resultType);
        builder.create<mlir::scf::YieldOp>(location, mlir::ValueRange{defVal});
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
      builder.create<mlir::scf::YieldOp>(location, mlir::ValueRange{elseVal});
    } else {
      auto defVal = createDefaultValue(builder, location, resultType);
      builder.create<mlir::scf::YieldOp>(location, mlir::ValueRange{defVal});
    }
  }

  builder.setInsertionPointAfter(ifOp);
  return ifOp.getResult(0);
}
