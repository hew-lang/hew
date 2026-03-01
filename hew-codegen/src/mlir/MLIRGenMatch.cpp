//===- MLIRGenMatch.cpp - Match/pattern codegen for Hew MLIRGen -----------===//
//
// Match statement/expression generation: generateMatchStmt, generateMatchExpr,
// generateMatchImpl, generateMatchArmsChain.
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

#include <algorithm>
#include <string>

using namespace hew;
using namespace mlir;

// ============================================================================
// Pattern helpers (shared by match and if-let)
// ============================================================================

int64_t MLIRGen::resolvePayloadFieldIndex(llvm::StringRef variantName,
                                           size_t payloadOrdinal) const {
  auto variantIt = variantLookup.find(variantName.str());
  if (variantIt == variantLookup.end())
    return 1 + static_cast<int64_t>(payloadOrdinal);

  const auto &enumName = variantIt->second.first;
  const auto variantIndex = variantIt->second.second;

  auto enumIt = enumTypes.find(enumName);
  if (enumIt != enumTypes.end()) {
    for (const auto &variant : enumIt->second.variants) {
      if (variant.index != variantIndex)
        continue;
      if (payloadOrdinal < variant.payloadPositions.size())
        return variant.payloadPositions[payloadOrdinal];
      break;
    }
  }

  return enumPayloadFieldIndex(enumName, static_cast<int32_t>(variantIndex),
                               static_cast<int64_t>(payloadOrdinal));
}

void MLIRGen::bindTuplePatternFields(const ast::PatTuple &tp, mlir::Value tupleValue,
                                      mlir::Location location) {
  for (size_t i = 0; i < tp.elements.size(); ++i) {
    const auto &elem = tp.elements[i];

    mlir::Value elemVal;
    if (auto hewTuple = mlir::dyn_cast<hew::HewTupleType>(tupleValue.getType())) {
      elemVal = builder.create<hew::TupleExtractOp>(location, hewTuple.getElementTypes()[i],
                                                    tupleValue, static_cast<int64_t>(i));
    } else {
      elemVal = builder.create<mlir::LLVM::ExtractValueOp>(
          location, tupleValue, llvm::ArrayRef<int64_t>{static_cast<int64_t>(i)});
    }

    if (auto *elemIdent = std::get_if<ast::PatIdentifier>(&elem->value.kind)) {
      declareVariable(elemIdent->name, elemVal);
    } else if (auto *elemTuple = std::get_if<ast::PatTuple>(&elem->value.kind)) {
      bindTuplePatternFields(*elemTuple, elemVal, location);
    }
    // Wildcards don't bind — skip
  }
}

void MLIRGen::bindConstructorPatternVars(const ast::PatConstructor &ctor, mlir::Value scrutinee,
                                          mlir::Location location) {
  if (!isEnumLikeType(scrutinee.getType()))
    return;
  const auto &ctorName = ctor.name;
  for (size_t i = 0; i < ctor.patterns.size(); ++i) {
    const auto &subPat = ctor.patterns[i]->value;
    if (auto *subIdent = std::get_if<ast::PatIdentifier>(&subPat.kind)) {
      int64_t fieldIdx = resolvePayloadFieldIndex(ctorName, i);
      auto fieldTy = getEnumFieldType(scrutinee.getType(), fieldIdx);
      auto payloadVal =
          builder.create<hew::EnumExtractPayloadOp>(location, fieldTy, scrutinee, fieldIdx);
      declareVariable(subIdent->name, payloadVal);
    } else if (auto *subTuple = std::get_if<ast::PatTuple>(&subPat.kind)) {
      int64_t fieldIdx = resolvePayloadFieldIndex(ctorName, i);
      auto fieldTy = getEnumFieldType(scrutinee.getType(), fieldIdx);
      auto payloadVal =
          builder.create<hew::EnumExtractPayloadOp>(location, fieldTy, scrutinee, fieldIdx);
      bindTuplePatternFields(*subTuple, payloadVal, location);
    }
    // Wildcard sub-patterns: skip binding
  }
}

mlir::Value MLIRGen::emitTagEqualCondition(mlir::Value scrutinee, int64_t variantIndex,
                                            mlir::Location location) {
  auto tag = builder.create<hew::EnumExtractTagOp>(location, builder.getI32Type(), scrutinee);
  auto tagVal = createIntConstant(builder, location, builder.getI32Type(), variantIndex);
  return builder.create<mlir::arith::CmpIOp>(location, mlir::arith::CmpIPredicate::eq, tag, tagVal)
      .getResult();
}

// ============================================================================
// Match statement generation
// ============================================================================

void MLIRGen::generateMatchStmt(const ast::StmtMatch &stmt) {
  auto location = currentLoc;

  auto scrutinee = generateExpression(stmt.scrutinee.value);
  if (!scrutinee)
    return;

  // Generate match as chain of if/else (no result needed)
  generateMatchImpl(scrutinee, stmt.arms, /*resultType=*/nullptr, location);
}

mlir::Value MLIRGen::generateMatchExpr(const ast::ExprMatch &expr, const ast::Span &exprSpan) {
  auto location = currentLoc;

  if (!expr.scrutinee)
    return nullptr;
  auto scrutinee = generateExpression(expr.scrutinee->value);
  if (!scrutinee)
    return nullptr;

  // Use the type checker's resolved type for this match expression.
  // The frontend type-checks all arms, unifies their types, and records
  // the result type in the expression type map keyed by span.
  mlir::Type resultType = nullptr;
  if (auto *resolvedType = resolvedTypeOf(exprSpan)) {
    resultType = convertType(*resolvedType);
  }

  // Fallback: if the type checker didn't record a type (e.g. statement
  // position or missing type info), use the scrutinee type to infer.
  if (!resultType) {
    emitWarning(location) << "match expression type not resolved; inferring from scrutinee type";
    if (auto rt = mlir::dyn_cast<hew::ResultEnumType>(scrutinee.getType()))
      resultType = rt.getOkType();
    else if (auto ot = mlir::dyn_cast<hew::OptionEnumType>(scrutinee.getType()))
      resultType = ot.getInnerType();
  }

  if (!resultType) {
    emitError(location) << "cannot determine result type for match expression"
                        << " (scrutinee type: " << scrutinee.getType() << ")";
    return nullptr;
  }

  return generateMatchImpl(scrutinee, expr.arms, resultType, location);
}

mlir::Value MLIRGen::generateOrPatternCondition(mlir::Value scrutinee, const ast::Pattern &pattern,
                                                mlir::Location location) {
  if (auto *litPat = std::get_if<ast::PatLiteral>(&pattern.kind)) {
    auto litVal = generateLiteral(litPat->lit);
    if (!litVal)
      return nullptr;
    auto scrType = scrutinee.getType();
    if (llvm::isa<mlir::FloatType>(scrType)) {
      return builder.create<mlir::arith::CmpFOp>(location, mlir::arith::CmpFPredicate::OEQ,
                                                 scrutinee, litVal);
    }
    if (llvm::isa<hew::StringRefType>(scrType)) {
      auto eqResult = builder.create<hew::StringMethodOp>(location, builder.getI32Type(),
                                                          builder.getStringAttr("equals"),
                                                          scrutinee, mlir::ValueRange{litVal});
      auto zero = createIntConstant(builder, location, builder.getI32Type(), 0);
      return builder.create<mlir::arith::CmpIOp>(location, mlir::arith::CmpIPredicate::ne,
                                                 eqResult.getResult(), zero);
    }
    if (litVal.getType() != scrType)
      litVal = coerceType(litVal, scrType, location);
    return builder.create<mlir::arith::CmpIOp>(location, mlir::arith::CmpIPredicate::eq, scrutinee,
                                               litVal);
  }
  if (auto *orPat = std::get_if<ast::PatOr>(&pattern.kind)) {
    auto leftCond = generateOrPatternCondition(scrutinee, orPat->left->value, location);
    auto rightCond = generateOrPatternCondition(scrutinee, orPat->right->value, location);
    if (!leftCond || !rightCond)
      return nullptr;
    return builder.create<mlir::arith::OrIOp>(location, leftCond, rightCond);
  }
  if (std::get_if<ast::PatWildcard>(&pattern.kind)) {
    return createIntConstant(builder, location, builder.getI1Type(), 1);
  }
  if (auto *identPat = std::get_if<ast::PatIdentifier>(&pattern.kind)) {
    auto varIt = variantLookup.find(identPat->name);
    if (varIt != variantLookup.end()) {
      return emitTagEqualCondition(scrutinee, static_cast<int64_t>(varIt->second.second), location);
    }
    // Variable binding: always matches (like wildcard)
    return createIntConstant(builder, location, builder.getI1Type(), 1);
  }
  return nullptr;
}

mlir::Value MLIRGen::generateMatchImpl(mlir::Value scrutinee,
                                       const std::vector<ast::MatchArm> &arms,
                                       mlir::Type resultType, mlir::Location location) {
  if (arms.empty())
    return nullptr;

  // Generate a chain of if/else for each arm
  // Wildcards and catch-all patterns are handled inside generateMatchArmsChain
  return generateMatchArmsChain(scrutinee, arms, 0, resultType, location);
}

mlir::Value MLIRGen::generateMatchArmsChain(mlir::Value scrutinee,
                                            const std::vector<ast::MatchArm> &arms, size_t idx,
                                            mlir::Type resultType, mlir::Location location) {
  if (idx >= arms.size()) {
    // No arm matched — non-exhaustive match at runtime. Trap.
    builder.create<hew::PanicOp>(location);
    if (resultType)
      return createDefaultValue(builder, location, resultType);
    return nullptr;
  }

  const auto &arm = arms[idx];
  bool isLast = (idx + 1 == arms.size());

  const auto &pattern = arm.pattern.value;

  // Determine pattern type: check if identifier is an enum variant
  bool isEnumVariantPattern = false;
  if (auto *identPat = std::get_if<ast::PatIdentifier>(&pattern.kind)) {
    isEnumVariantPattern = variantLookup.count(identPat->name) > 0;
  }

  // Check if this is a Constructor pattern (e.g., Some(x))
  auto *ctorPatPtr = std::get_if<ast::PatConstructor>(&pattern.kind);
  bool isConstructorPattern = (ctorPatPtr != nullptr);
  auto *tuplePatPtr = std::get_if<ast::PatTuple>(&pattern.kind);
  bool isTuplePattern = (tuplePatPtr != nullptr);

  bool isWildcard =
      (std::get_if<ast::PatWildcard>(&pattern.kind) != nullptr ||
       (std::get_if<ast::PatIdentifier>(&pattern.kind) != nullptr && !isEnumVariantPattern));
  auto *litPatPtr = std::get_if<ast::PatLiteral>(&pattern.kind);
  bool isLiteral = (litPatPtr != nullptr);
  auto *orPatPtr = std::get_if<ast::PatOr>(&pattern.kind);
  bool isOrPattern = (orPatPtr != nullptr);
  auto *structPatPtr = std::get_if<ast::PatStruct>(&pattern.kind);
  bool isStructPattern = (structPatPtr != nullptr);
  bool isStructVariantPattern = isStructPattern && variantLookup.count(structPatPtr->name) > 0;

  auto bindStructPatternFields = [&](const ast::PatStruct &sp) {
    const auto &spName = sp.name;
    auto varIt = variantLookup.find(spName);
    if (varIt != variantLookup.end()) {
      const auto &enumName = varIt->second.first;
      auto enumIt = enumTypes.find(enumName);
      if (enumIt != enumTypes.end()) {
        const EnumVariantInfo *vi = nullptr;
        for (const auto &v : enumIt->second.variants) {
          if (v.index == varIt->second.second) {
            vi = &v;
            break;
          }
        }
        if (vi) {
          for (const auto &pf : sp.fields) {
            auto fieldIt = std::find(vi->fieldNames.begin(), vi->fieldNames.end(), pf.name);
            if (fieldIt == vi->fieldNames.end())
              continue;
            size_t ordinal = static_cast<size_t>(fieldIt - vi->fieldNames.begin());
            if (isEnumLikeType(scrutinee.getType())) {
              int64_t fieldIdx = resolvePayloadFieldIndex(spName, ordinal);
              auto fieldTy = getEnumFieldType(scrutinee.getType(), fieldIdx);
              auto payloadVal =
                  builder.create<hew::EnumExtractPayloadOp>(location, fieldTy, scrutinee, fieldIdx);
              declareVariable(pf.name, payloadVal);
            }
          }
        }
      }
      return;
    }
    auto structIt = structTypes.find(spName);
    if (structIt != structTypes.end()) {
      const auto &info = structIt->second;
      for (const auto &pf : sp.fields) {
        for (const auto &fi : info.fields) {
          if (fi.name == pf.name) {
            auto fieldVal = builder.create<hew::FieldGetOp>(
                location,
                mlir::cast<mlir::LLVM::LLVMStructType>(scrutinee.getType()).getBody()[fi.index],
                scrutinee, builder.getStringAttr(fi.name), static_cast<int64_t>(fi.index));
            declareVariable(pf.name, fieldVal);
            break;
          }
        }
      }
    }
  };

  // Helper to generate arm body value
  auto generateArmBody = [&](const ast::MatchArm &a) -> mlir::Value {
    SymbolTableScopeT scope(symbolTable);
    MutableTableScopeT mutScope(mutableVars);

    {
      const auto &aPat = a.pattern.value;

      // If this is an identifier pattern (non-variant), bind the scrutinee
      if (auto *identPat = std::get_if<ast::PatIdentifier>(&aPat.kind)) {
        if (variantLookup.count(identPat->name) == 0) {
          declareVariable(identPat->name, scrutinee);
        }
      }

      // If this is a constructor pattern, bind sub-pattern variables to payloads
      if (auto *ctor = std::get_if<ast::PatConstructor>(&aPat.kind)) {
        bindConstructorPatternVars(*ctor, scrutinee, location);
      }

      // If this is a struct pattern, bind fields as variables
      if (auto *sp = std::get_if<ast::PatStruct>(&aPat.kind)) {
        bindStructPatternFields(*sp);
      }

      // If this is a tuple pattern, bind elements as variables
      if (auto *tp = std::get_if<ast::PatTuple>(&aPat.kind)) {
        bindTuplePatternFields(*tp, scrutinee, location);
      }
    }

    if (a.body) {
      return generateExpression(a.body->value);
    }
    return nullptr;
  };

  // Helper to generate if/else chain for tag comparison
  auto generateTagMatch = [&](mlir::Value cond) -> mlir::Value {
    if (resultType) {
      auto ifOp = builder.create<mlir::scf::IfOp>(location, resultType, cond,
                                                  /*withElseRegion=*/true);

      builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
      auto thenVal = generateArmBody(arm);
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

      builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
      auto elseVal = generateMatchArmsChain(scrutinee, arms, idx + 1, resultType, location);
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
    } else {
      bool hasMore = (idx + 1 < arms.size());
      auto ifOp = builder.create<mlir::scf::IfOp>(location, mlir::TypeRange{}, cond, hasMore);

      builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
      generateArmBody(arm);
      ensureYieldTerminator(location);

      if (hasMore) {
        builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
        generateMatchArmsChain(scrutinee, arms, idx + 1, nullptr, location);
        ensureYieldTerminator(location);
      }

      builder.setInsertionPointAfter(ifOp);
      return nullptr;
    }
  };

  // Wildcard or last arm without guard: generate body directly
  if ((isWildcard && !arm.guard) ||
      (isLast && !isLiteral && !isEnumVariantPattern && !isConstructorPattern && !isOrPattern &&
       !isTuplePattern && !isStructVariantPattern && !arm.guard)) {
    return generateArmBody(arm);
  }

  // Wildcard/identifier with guard: use guard expression as condition
  if (isWildcard && arm.guard) {
    SymbolTableScopeT guardScope(symbolTable);
    MutableTableScopeT guardMutScope(mutableVars);
    if (auto *identPat = std::get_if<ast::PatIdentifier>(&pattern.kind)) {
      if (variantLookup.count(identPat->name) == 0) {
        declareVariable(identPat->name, scrutinee);
      }
    }
    auto guardCond = generateExpression(arm.guard->value);
    if (!guardCond)
      return nullptr;
    return generateTagMatch(guardCond);
  }

  // Literal pattern: compare scrutinee with literal value
  if (isLiteral) {
    auto litVal = generateLiteral(litPatPtr->lit);
    if (!litVal)
      return nullptr;

    auto scrType = scrutinee.getType();
    mlir::Value cond;
    if (llvm::isa<mlir::FloatType>(scrType)) {
      cond = builder.create<mlir::arith::CmpFOp>(location, mlir::arith::CmpFPredicate::OEQ,
                                                 scrutinee, litVal);
    } else if (llvm::isa<hew::StringRefType>(scrType)) {
      // String comparison via hew_string_equals runtime call
      auto eqResult = builder.create<hew::StringMethodOp>(location, builder.getI32Type(),
                                                          builder.getStringAttr("equals"),
                                                          scrutinee, mlir::ValueRange{litVal});
      auto zero = createIntConstant(builder, location, builder.getI32Type(), 0);
      cond = builder.create<mlir::arith::CmpIOp>(location, mlir::arith::CmpIPredicate::ne,
                                                 eqResult.getResult(), zero);
    } else {
      if (litVal.getType() != scrType)
        litVal = coerceType(litVal, scrType, location);
      cond = builder.create<mlir::arith::CmpIOp>(location, mlir::arith::CmpIPredicate::eq,
                                                 scrutinee, litVal);
    }

    // Guard: AND with pattern condition
    if (arm.guard) {
      auto guardCond = generateExpression(arm.guard->value);
      if (guardCond)
        cond = builder.create<mlir::arith::AndIOp>(location, cond, guardCond);
    }

    return generateTagMatch(cond);
  }

  // Enum variant pattern (unit variant name): compare tag
  if (isEnumVariantPattern) {
    auto *identPat = std::get_if<ast::PatIdentifier>(&pattern.kind);
    auto varIt = variantLookup.find(identPat->name);
    mlir::Value cond =
        emitTagEqualCondition(scrutinee, static_cast<int64_t>(varIt->second.second), location);

    // Guard: AND with pattern condition
    if (arm.guard) {
      auto guardCond = generateExpression(arm.guard->value);
      if (guardCond)
        cond = builder.create<mlir::arith::AndIOp>(location, cond, guardCond);
    }

    return generateTagMatch(cond);
  }

  // Constructor pattern: e.g., Some(x), Ok(val)
  if (isConstructorPattern) {
    auto *ctor = ctorPatPtr;
    const auto &ctorName = ctor->name;
    auto ctorVarIt = variantLookup.find(ctorName);
    if (ctorVarIt != variantLookup.end()) {
      mlir::Value tagCond =
          emitTagEqualCondition(scrutinee, static_cast<int64_t>(ctorVarIt->second.second), location);

      // Guard: We must short-circuit to avoid extracting payload when tag doesn't match.
      // Use scf.if to only evaluate guard (and extract payload) when tag matches.
      if (arm.guard) {
        auto guardIfOp = builder.create<mlir::scf::IfOp>(location, builder.getI1Type(), tagCond,
                                                         /*withElseRegion=*/true);

        // Then region: tag matches, extract payload and evaluate guard
        builder.setInsertionPointToStart(&guardIfOp.getThenRegion().front());
        {
          SymbolTableScopeT guardScope(symbolTable);
          MutableTableScopeT guardMutScope(mutableVars);
          bindConstructorPatternVars(*ctor, scrutinee, location);
          auto guardCond = generateExpression(arm.guard->value);
          if (!guardCond) {
            emitError(location) << "failed to generate match guard expression";
            return nullptr;
          }
          builder.create<mlir::scf::YieldOp>(location, mlir::ValueRange{guardCond});
        }

        // Else region: tag doesn't match, return false
        builder.setInsertionPointToStart(&guardIfOp.getElseRegion().front());
        auto falseVal = createIntConstant(builder, location, builder.getI1Type(), 0);
        builder.create<mlir::scf::YieldOp>(location, mlir::ValueRange{falseVal});

        builder.setInsertionPointAfter(guardIfOp);
        mlir::Value cond = guardIfOp.getResult(0);
        return generateTagMatch(cond);
      }

      // No guard: tag check is sufficient
      return generateTagMatch(tagCond);
    }
    emitError(location) << "unknown constructor pattern '" << ctorName << "' in match arm";
    return nullptr;
  }

  // Or-pattern: e.g., 1 | 2 | 3
  if (isOrPattern) {
    auto cond = generateOrPatternCondition(scrutinee, pattern, location);
    if (cond) {
      // Guard: AND with pattern condition
      if (arm.guard) {
        auto guardCond = generateExpression(arm.guard->value);
        if (guardCond)
          cond = builder.create<mlir::arith::AndIOp>(location, cond, guardCond);
      }
      return generateTagMatch(cond);
    }
  }

  if (isStructVariantPattern) {
    const auto &spName = structPatPtr->name;
    auto varIt = variantLookup.find(spName);
    mlir::Value tagCond =
        emitTagEqualCondition(scrutinee, static_cast<int64_t>(varIt->second.second), location);

    if (arm.guard) {
      auto guardIfOp = builder.create<mlir::scf::IfOp>(location, builder.getI1Type(), tagCond,
                                                       /*withElseRegion=*/true);

      builder.setInsertionPointToStart(&guardIfOp.getThenRegion().front());
      {
        SymbolTableScopeT guardScope(symbolTable);
        MutableTableScopeT guardMutScope(mutableVars);
        bindStructPatternFields(*structPatPtr);
        auto guardCond = generateExpression(arm.guard->value);
        if (!guardCond) {
          emitError(location) << "failed to generate match guard expression";
          return nullptr;
        }
        builder.create<mlir::scf::YieldOp>(location, mlir::ValueRange{guardCond});
      }

      builder.setInsertionPointToStart(&guardIfOp.getElseRegion().front());
      auto falseVal = createIntConstant(builder, location, builder.getI1Type(), 0);
      builder.create<mlir::scf::YieldOp>(location, mlir::ValueRange{falseVal});

      builder.setInsertionPointAfter(guardIfOp);
      mlir::Value cond = guardIfOp.getResult(0);
      return generateTagMatch(cond);
    }

    return generateTagMatch(tagCond);
  }

  // Struct pattern: irrefutable unless guarded
  if (isStructPattern && !arm.guard) {
    return generateArmBody(arm);
  }
  if (isStructPattern && arm.guard) {
    SymbolTableScopeT guardScope(symbolTable);
    MutableTableScopeT guardMutScope(mutableVars);
    bindStructPatternFields(*structPatPtr);
    auto guardCond = generateExpression(arm.guard->value);
    if (!guardCond)
      return nullptr;
    return generateTagMatch(guardCond);
  }

  // Tuple pattern: irrefutable unless guarded
  if (isTuplePattern && !arm.guard) {
    return generateArmBody(arm);
  }
  if (isTuplePattern && arm.guard) {
    SymbolTableScopeT guardScope(symbolTable);
    MutableTableScopeT guardMutScope(mutableVars);
    bindTuplePatternFields(*tuplePatPtr, scrutinee, location);
    auto guardCond = generateExpression(arm.guard->value);
    if (!guardCond)
      return nullptr;
    return generateTagMatch(guardCond);
  }

  // For other pattern types, emit error
  emitError(location) << "unhandled pattern kind in match arm";
  return nullptr;
}
