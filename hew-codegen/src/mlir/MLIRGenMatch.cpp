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
#include "mlir/IR/BuiltinAttributes.h"
#include "mlir/IR/BuiltinTypes.h"
#include "mlir/IR/Diagnostics.h"

#include "llvm/ADT/ScopedHashTable.h"
#include "llvm/ADT/StringRef.h"

#include <algorithm>
#include <functional>
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

  (void)enumName;
  (void)variantIndex;
  return 1 + static_cast<int64_t>(payloadOrdinal);
}

void MLIRGen::bindTuplePatternFields(const ast::PatTuple &tp, mlir::Value tupleValue,
                                     mlir::Location location) {
  for (size_t i = 0; i < tp.elements.size(); ++i) {
    const auto &elem = tp.elements[i];

    mlir::Value elemVal;
    if (auto hewTuple = mlir::dyn_cast<hew::HewTupleType>(tupleValue.getType())) {
      elemVal = hew::TupleExtractOp::create(builder, location, hewTuple.getElementTypes()[i],
                                            tupleValue, static_cast<int64_t>(i));
    } else {
      elemVal = mlir::LLVM::ExtractValueOp::create(
          builder, location, tupleValue, llvm::ArrayRef<int64_t>{static_cast<int64_t>(i)});
    }

    if (auto *elemIdent = std::get_if<ast::PatIdentifier>(&elem->value.kind)) {
      declareVariable(elemIdent->name, elemVal);
      auto drop = dropFuncForMLIRType(elemVal.getType());
      if (!drop.empty()) {
        bool isUser = false;
        if (auto st = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(elemVal.getType()))
          isUser = st.isIdentified() && userDropFuncs.count(st.getName().str());
        registerDroppable(elemIdent->name, drop, isUser);
      }
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
          hew::EnumExtractPayloadOp::create(builder, location, fieldTy, scrutinee, fieldIdx);
      declareVariable(subIdent->name, payloadVal);
      // Register drop for extracted enum payload (e.g. String from Option<String>).
      // The payload is extracted by value — the enum wrapper has no destructor,
      // so this binding is the sole owner of the heap allocation.
      auto drop = dropFuncForMLIRType(payloadVal.getType());
      if (!drop.empty()) {
        bool isUser = false;
        if (auto st = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(payloadVal.getType()))
          isUser = st.isIdentified() && userDropFuncs.count(st.getName().str());
        registerDroppable(subIdent->name, drop, isUser);
      }
    } else if (auto *subTuple = std::get_if<ast::PatTuple>(&subPat.kind)) {
      int64_t fieldIdx = resolvePayloadFieldIndex(ctorName, i);
      auto fieldTy = getEnumFieldType(scrutinee.getType(), fieldIdx);
      auto payloadVal =
          hew::EnumExtractPayloadOp::create(builder, location, fieldTy, scrutinee, fieldIdx);
      bindTuplePatternFields(*subTuple, payloadVal, location);
    }
    // Wildcard sub-patterns: skip binding
  }
}

mlir::Value MLIRGen::emitTagEqualCondition(mlir::Value scrutinee, int64_t variantIndex,
                                           mlir::Location location) {
  auto tag = hew::EnumExtractTagOp::create(builder, location, builder.getI32Type(), scrutinee);
  auto tagVal = createIntConstant(builder, location, builder.getI32Type(), variantIndex);
  return mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::eq, tag, tagVal)
      .getResult();
}

// ============================================================================
// Indirect enum scrutinee dereferencing
// ============================================================================

/// Look up the indirect enum info for a given pointer-typed scrutinee.
/// Uses only the type checker's expression type metadata.
const MLIRGen::EnumTypeInfo *
MLIRGen::findIndirectEnumForScrutinee(mlir::Value scrutinee, const ast::Span &span,
                                      const std::vector<ast::MatchArm> * /*arms*/) const {
  // Only applies to pointer-typed scrutinees
  if (!mlir::isa<mlir::LLVM::LLVMPointerType>(scrutinee.getType()))
    return nullptr;

  // Strategy 1: Look up the source type from the type checker's expression type map
  if (auto *resolvedType = resolvedTypeOf(span)) {
    if (auto *named = std::get_if<ast::TypeNamed>(&resolvedType->kind)) {
      auto enumIt = enumTypes.find(named->name);
      if (enumIt != enumTypes.end() && enumIt->second.isIndirect && enumIt->second.innerStructType)
        return &enumIt->second;
    }
  }

  return nullptr;
}

mlir::Value MLIRGen::derefIndirectEnumScrutinee(mlir::Value scrutinee, const ast::Span &span,
                                                mlir::Location location,
                                                const std::vector<ast::MatchArm> *arms) {
  if (!mlir::isa<mlir::LLVM::LLVMPointerType>(scrutinee.getType()))
    return scrutinee;

  if (arms && !resolvedTypeOf(span)) {
    requireResolvedTypeOf(span, "match scrutinee indirect enum", location);
    return nullptr;
  }

  auto *info = findIndirectEnumForScrutinee(scrutinee, span, arms);
  if (!info)
    return scrutinee;
  return mlir::LLVM::LoadOp::create(builder, location, info->innerStructType, scrutinee);
}

// ============================================================================
// Match statement generation
// ============================================================================

void MLIRGen::generateMatchStmt(const ast::StmtMatch &stmt) {
  auto location = currentLoc;

  auto scrutinee = generateExpression(stmt.scrutinee.value);
  if (!scrutinee)
    return;

  // Indirect enum: dereference pointer to get the inner struct
  scrutinee = derefIndirectEnumScrutinee(scrutinee, stmt.scrutinee.span, location, &stmt.arms);
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

  // Indirect enum: dereference pointer to get the inner struct
  scrutinee = derefIndirectEnumScrutinee(scrutinee, expr.scrutinee->span, location, &expr.arms);
  if (!scrutinee)
    return nullptr;

  // Use the type checker's resolved type for this match expression.
  // The frontend type-checks all arms, unifies their types, and records the
  // result type in the expression type map keyed by span. Missing metadata is
  // a codegen boundary failure; do not reconstruct it from lowered scrutinee
  // types.
  auto *resolvedType = requireResolvedTypeOf(exprSpan, "match expression result type", location);
  if (!resolvedType)
    return nullptr;
  mlir::Type resultType = convertType(*resolvedType);
  // NoneType means the match arms all return void — treat as a statement-style
  // match with no result value (resultType == nullptr).
  if (mlir::isa<mlir::NoneType>(resultType))
    resultType = nullptr;

  return generateMatchImpl(scrutinee, expr.arms, resultType, location);
}

mlir::Value MLIRGen::generateOrPatternCondition(mlir::Value scrutinee, const ast::Pattern &pattern,
                                                mlir::Location location) {
  if (auto *litPat = std::get_if<ast::PatLiteral>(&pattern.kind)) {
    ast::Span noSpan{0, 0};
    auto litVal = generateLiteral(litPat->lit, noSpan);
    if (!litVal)
      return nullptr;
    auto scrType = scrutinee.getType();
    if (llvm::isa<mlir::FloatType>(scrType)) {
      return mlir::arith::CmpFOp::create(builder, location, mlir::arith::CmpFPredicate::OEQ,
                                         scrutinee, litVal);
    }
    if (llvm::isa<hew::StringRefType>(scrType)) {
      auto eqResult = hew::StringMethodOp::create(builder, location, builder.getI32Type(),
                                                  builder.getStringAttr("equals"), scrutinee,
                                                  mlir::ValueRange{litVal});
      auto zero = createIntConstant(builder, location, builder.getI32Type(), 0);
      return mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::ne,
                                         eqResult.getResult(), zero);
    }
    if (litVal.getType() != scrType) {
      litVal = coerceType(litVal, scrType, location);
      if (!litVal)
        return nullptr;
    }
    return mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::eq, scrutinee,
                                       litVal);
  }
  if (auto *orPat = std::get_if<ast::PatOr>(&pattern.kind)) {
    auto leftCond = generateOrPatternCondition(scrutinee, orPat->left->value, location);
    auto rightCond = generateOrPatternCondition(scrutinee, orPat->right->value, location);
    if (!leftCond || !rightCond)
      return nullptr;
    return mlir::arith::OrIOp::create(builder, location, leftCond, rightCond);
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
    // Use RuntimeCallOp here because we may be inside an scf.if region
    // where PanicOp (Terminator) cannot be the last op (scf.yield must be).
    hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                               mlir::SymbolRefAttr::get(&context, "hew_panic"), mlir::ValueRange{});
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
              auto payloadVal = hew::EnumExtractPayloadOp::create(builder, location, fieldTy,
                                                                  scrutinee, fieldIdx);
              declareVariable(pf.name, payloadVal);
              auto drop = dropFuncForMLIRType(payloadVal.getType());
              if (!drop.empty()) {
                bool isUser = false;
                if (auto st = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(payloadVal.getType()))
                  isUser = st.isIdentified() && userDropFuncs.count(st.getName().str());
                registerDroppable(pf.name, drop, isUser);
              }
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
            auto fieldVal = hew::FieldGetOp::create(
                builder, location,
                mlir::cast<mlir::LLVM::LLVMStructType>(scrutinee.getType()).getBody()[fi.index],
                scrutinee, builder.getStringAttr(fi.name), static_cast<int64_t>(fi.index));
            declareVariable(pf.name, fieldVal);
            break;
          }
        }
      }
    }
  };

  auto blockTailRequiresValue = [&](const ast::Block &block,
                                    const auto &exprRequiresValue) -> bool {
    if (block.trailing_expr)
      return exprRequiresValue(block.trailing_expr->value, exprRequiresValue);

    if (block.stmts.empty())
      return false;

    const auto &lastStmt = block.stmts.back()->value;
    if (auto *exprStmt = std::get_if<ast::StmtExpression>(&lastStmt.kind))
      return exprRequiresValue(exprStmt->expr.value, exprRequiresValue);

    bool canProduceTailValue = currentFunction && currentFunction.getResultTypes().size() == 1;
    if (auto *ifStmt = std::get_if<ast::StmtIf>(&lastStmt.kind))
      return canProduceTailValue && ifStmt->else_block.has_value();
    if (std::holds_alternative<ast::StmtMatch>(lastStmt.kind))
      return canProduceTailValue;

    return false;
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
  auto failClosedDiscardedArmBody = [&](const ast::Expr &expr, mlir::Value value,
                                        size_t errorsBefore) -> bool {
    auto *insertBlock = builder.getInsertionBlock();
    if (value || errorCount_ != errorsBefore || (insertBlock && hasRealTerminator(insertBlock)) ||
        !exprRequiresValue(expr, exprRequiresValue))
      return false;

    ++errorCount_;
    emitError(location) << "discarded match arm body in statement position failed to lower a "
                           "nested value expression";
    return true;
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
      auto errorsBefore = errorCount_;
      auto bodyVal = generateExpression(a.body->value);
      if (!resultType && failClosedDiscardedArmBody(a.body->value, bodyVal, errorsBefore))
        return nullptr;
      return bodyVal;
    }
    return nullptr;
  };

  auto lowerResultfulMatchValue = [&](auto &&lower) -> std::pair<mlir::Value, bool> {
    bool sawDiagnostic = false;
    unsigned prevErrorCount = errorCount_;
    mlir::ScopedDiagnosticHandler diagnosticProbe(&context, [&](mlir::Diagnostic &) {
      sawDiagnostic = true;
      return mlir::failure();
    });
    auto value = lower();
    return {value, sawDiagnostic || errorCount_ != prevErrorCount};
  };

  std::function<bool(const ast::Expr &)> exprDefinitelyPanics;
  auto blockDefinitelyPanics = [&](const ast::Block &block) -> bool {
    if (block.trailing_expr)
      return exprDefinitelyPanics(block.trailing_expr->value);
    if (block.stmts.empty())
      return false;
    auto *exprStmt = std::get_if<ast::StmtExpression>(&block.stmts.back()->value.kind);
    return exprStmt && exprDefinitelyPanics(exprStmt->expr.value);
  };
  exprDefinitelyPanics = [&](const ast::Expr &expr) -> bool {
    if (auto *call = std::get_if<ast::ExprCall>(&expr.kind)) {
      auto *callee = std::get_if<ast::ExprIdentifier>(&call->function->value.kind);
      return callee && callee->name == "panic";
    }
    if (auto *blockExpr = std::get_if<ast::ExprBlock>(&expr.kind))
      return blockDefinitelyPanics(blockExpr->block);
    if (auto *scopeExpr = std::get_if<ast::ExprScope>(&expr.kind))
      return blockDefinitelyPanics(scopeExpr->block);
    if (auto *unsafeExpr = std::get_if<ast::ExprUnsafe>(&expr.kind))
      return blockDefinitelyPanics(unsafeExpr->block);
    return false;
  };

  auto endsInNoReturnRuntimeCall = [&](mlir::Block *block) {
    if (!block || block->empty())
      return false;
    auto call = mlir::dyn_cast<hew::RuntimeCallOp>(block->back());
    return call && call.getCallee() == "hew_panic";
  };

  auto emitMissingResultfulMatchValue = [&](mlir::Location diagLoc, llvm::StringRef which) {
    ++errorCount_;
    emitError(diagLoc) << "match expression " << which << " did not produce a value";
  };

  // Helper to generate if/else chain for tag comparison
  auto generateTagMatch = [&](mlir::Value cond) -> mlir::Value {
    if (resultType) {
      auto ifOp = mlir::scf::IfOp::create(builder, location, resultType, cond,
                                          /*withElseRegion=*/true);

      builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
      auto [thenVal, thenHadDiagnostic] =
          lowerResultfulMatchValue([&]() -> mlir::Value { return generateArmBody(arm); });
      auto *thenBlock = builder.getInsertionBlock();
      if (thenBlock->empty() || !thenBlock->back().hasTrait<mlir::OpTrait::IsTerminator>()) {
        if (!thenVal) {
          if ((arm.body && exprDefinitelyPanics(arm.body->value)) ||
              endsInNoReturnRuntimeCall(thenBlock)) {
            thenVal = createDefaultValue(builder, location, resultType);
          } else if (!thenHadDiagnostic) {
            emitMissingResultfulMatchValue(arm.body ? loc(arm.body->span) : location,
                                           "arm lowering");
            return nullptr;
          } else {
            return nullptr;
          }
        }
        thenVal = coerceType(thenVal, resultType, location);
        if (!thenVal)
          return nullptr;
        mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{thenVal});
      }

      builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
      auto [elseVal, elseHadDiagnostic] = lowerResultfulMatchValue([&]() -> mlir::Value {
        return generateMatchArmsChain(scrutinee, arms, idx + 1, resultType, location);
      });
      auto *elseBlock = builder.getInsertionBlock();
      if (elseBlock->empty() || !elseBlock->back().hasTrait<mlir::OpTrait::IsTerminator>()) {
        if (!elseVal) {
          if (endsInNoReturnRuntimeCall(elseBlock)) {
            elseVal = createDefaultValue(builder, location, resultType);
          } else if (!elseHadDiagnostic) {
            mlir::Location elseLoc = idx + 1 < arms.size() && arms[idx + 1].body
                                         ? loc(arms[idx + 1].body->span)
                                         : location;
            emitMissingResultfulMatchValue(elseLoc, "else-chain lowering");
            return nullptr;
          } else
            return nullptr;
        }
        elseVal = coerceType(elseVal, resultType, location);
        if (!elseVal)
          return nullptr;
        mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{elseVal});
      }

      builder.setInsertionPointAfter(ifOp);
      return ifOp.getResult(0);
    } else {
      // Always emit an else region so that a conditional arm whose condition
      // is false at runtime reaches the next arm (or the base-case trap).
      // Without this, the last conditional arm in a statement-style match
      // silently falls through instead of trapping on a non-exhaustive match.
      auto ifOp = mlir::scf::IfOp::create(builder, location, mlir::TypeRange{}, cond,
                                          /*withElseRegion=*/true);

      builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
      generateArmBody(arm);
      ensureYieldTerminator(location);

      builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
      generateMatchArmsChain(scrutinee, arms, idx + 1, nullptr, location);
      ensureYieldTerminator(location);

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
    ast::Span noSpan{0, 0};
    auto litVal = generateLiteral(litPatPtr->lit, noSpan);
    if (!litVal)
      return nullptr;

    auto scrType = scrutinee.getType();
    mlir::Value cond;
    if (llvm::isa<mlir::FloatType>(scrType)) {
      cond = mlir::arith::CmpFOp::create(builder, location, mlir::arith::CmpFPredicate::OEQ,
                                         scrutinee, litVal);
    } else if (llvm::isa<hew::StringRefType>(scrType)) {
      // String comparison via hew_string_equals runtime call
      auto eqResult = hew::StringMethodOp::create(builder, location, builder.getI32Type(),
                                                  builder.getStringAttr("equals"), scrutinee,
                                                  mlir::ValueRange{litVal});
      auto zero = createIntConstant(builder, location, builder.getI32Type(), 0);
      cond = mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::ne,
                                         eqResult.getResult(), zero);
    } else {
      if (litVal.getType() != scrType) {
        litVal = coerceType(litVal, scrType, location);
        if (!litVal)
          return nullptr;
      }
      cond = mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::eq,
                                         scrutinee, litVal);
    }

    // Guard: AND with pattern condition
    if (arm.guard) {
      auto guardCond = generateExpression(arm.guard->value);
      if (guardCond)
        cond = mlir::arith::AndIOp::create(builder, location, cond, guardCond);
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
        cond = mlir::arith::AndIOp::create(builder, location, cond, guardCond);
    }

    return generateTagMatch(cond);
  }

  // Constructor pattern: e.g., Some(x), Ok(val)
  if (isConstructorPattern) {
    auto *ctor = ctorPatPtr;
    const auto &ctorName = ctor->name;
    auto ctorVarIt = variantLookup.find(ctorName);
    if (ctorVarIt != variantLookup.end()) {
      mlir::Value tagCond = emitTagEqualCondition(
          scrutinee, static_cast<int64_t>(ctorVarIt->second.second), location);

      // Guard: We must short-circuit to avoid extracting payload when tag doesn't match.
      // Use scf.if to only evaluate guard (and extract payload) when tag matches.
      if (arm.guard) {
        auto guardIfOp = mlir::scf::IfOp::create(builder, location, builder.getI1Type(), tagCond,
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
          mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{guardCond});
        }

        // Else region: tag doesn't match, return false
        builder.setInsertionPointToStart(&guardIfOp.getElseRegion().front());
        auto falseVal = createIntConstant(builder, location, builder.getI1Type(), 0);
        mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{falseVal});

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
          cond = mlir::arith::AndIOp::create(builder, location, cond, guardCond);
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
      auto guardIfOp = mlir::scf::IfOp::create(builder, location, builder.getI1Type(), tagCond,
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
        mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{guardCond});
      }

      builder.setInsertionPointToStart(&guardIfOp.getElseRegion().front());
      auto falseVal = createIntConstant(builder, location, builder.getI1Type(), 0);
      mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{falseVal});

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
