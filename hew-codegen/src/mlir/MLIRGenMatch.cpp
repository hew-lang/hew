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

#include <cassert>
#include <cstdlib>
#include <string>

using namespace hew;
using namespace mlir;

// Extract the payload type at a given struct field index from any enum-like
// type: LLVMStructType, OptionEnumType, or ResultEnumType.
static mlir::Type getEnumFieldType(mlir::Type type, int64_t idx) {
  if (auto st = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(type))
    return st.getBody()[idx];
  if (auto ot = mlir::dyn_cast<hew::OptionEnumType>(type)) {
    // Layout: (tag:i32, inner:T) — idx 0 is tag, idx 1 is inner
    if (idx == 1)
      return ot.getInnerType();
    return mlir::IntegerType::get(type.getContext(), 32);
  }
  if (auto rt = mlir::dyn_cast<hew::ResultEnumType>(type)) {
    // Layout: (tag:i32, ok:T, err:E) — idx 0 tag, idx 1 ok, idx 2 err
    if (idx == 1)
      return rt.getOkType();
    if (idx == 2)
      return rt.getErrType();
    return mlir::IntegerType::get(type.getContext(), 32);
  }
  return nullptr;
}

// Check if a type is an enum-like type (has a tag + payload struct layout)
static bool isEnumLikeType(mlir::Type type) {
  return mlir::isa<mlir::LLVM::LLVMStructType>(type) || mlir::isa<hew::OptionEnumType>(type) ||
         mlir::isa<hew::ResultEnumType>(type);
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
    // No more arms; return default value
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

  bool isWildcard =
      (std::get_if<ast::PatWildcard>(&pattern.kind) != nullptr ||
       (std::get_if<ast::PatIdentifier>(&pattern.kind) != nullptr && !isEnumVariantPattern));
  auto *litPatPtr = std::get_if<ast::PatLiteral>(&pattern.kind);
  bool isLiteral = (litPatPtr != nullptr);
  auto *orPatPtr = std::get_if<ast::PatOr>(&pattern.kind);
  bool isOrPattern = (orPatPtr != nullptr);
  auto *structPatPtr = std::get_if<ast::PatStruct>(&pattern.kind);
  bool isStructPattern = (structPatPtr != nullptr);

  // Helper to extract the tag from a scrutinee (handles both i32 and struct)
  auto extractTag = [&](mlir::Value scrut) -> mlir::Value {
    return builder.create<hew::EnumExtractTagOp>(location, builder.getI32Type(), scrut);
  };

  auto payloadFieldIndexForVariant = [&](llvm::StringRef variantName,
                                         size_t payloadOrdinal) -> int64_t {
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
        std::string ctorName = ctor->name;

        for (size_t i = 0; i < ctor->patterns.size(); ++i) {
          const auto &subPat = ctor->patterns[i]->value;
          if (auto *subIdent = std::get_if<ast::PatIdentifier>(&subPat.kind)) {
            if (isEnumLikeType(scrutinee.getType())) {
              int64_t fieldIdx = payloadFieldIndexForVariant(ctorName, i);
              auto fieldTy = getEnumFieldType(scrutinee.getType(), fieldIdx);
              auto payloadVal =
                  builder.create<hew::EnumExtractPayloadOp>(location, fieldTy, scrutinee, fieldIdx);
              declareVariable(subIdent->name, payloadVal);
            }
          }
          // Wildcard sub-patterns: skip binding
        }
      }

      // If this is a struct pattern, bind fields as variables
      if (auto *sp = std::get_if<ast::PatStruct>(&aPat.kind)) {
        std::string spName = sp->name;
        auto structIt = structTypes.find(spName);
        if (structIt != structTypes.end()) {
          const auto &info = structIt->second;
          for (const auto &pf : sp->fields) {
            std::string pfName = pf.name;
            for (const auto &fi : info.fields) {
              if (fi.name == pfName) {
                auto fieldVal = builder.create<hew::FieldGetOp>(
                    location,
                    mlir::cast<mlir::LLVM::LLVMStructType>(scrutinee.getType()).getBody()[fi.index],
                    scrutinee, builder.getStringAttr(fi.name), static_cast<int64_t>(fi.index));
                declareVariable(pfName, fieldVal);
                break;
              }
            }
          }
        }
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
      auto *thenBlock = builder.getInsertionBlock();
      if (thenBlock->empty() || !thenBlock->back().hasTrait<mlir::OpTrait::IsTerminator>()) {
        builder.create<mlir::scf::YieldOp>(location);
      }

      if (hasMore) {
        builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
        generateMatchArmsChain(scrutinee, arms, idx + 1, nullptr, location);
        auto *elseBlock = builder.getInsertionBlock();
        if (elseBlock->empty() || !elseBlock->back().hasTrait<mlir::OpTrait::IsTerminator>()) {
          builder.create<mlir::scf::YieldOp>(location);
        }
      }

      builder.setInsertionPointAfter(ifOp);
      return nullptr;
    }
  };

  // Wildcard or last arm without guard: generate body directly
  if ((isWildcard && !arm.guard) || (isLast && !isLiteral && !isEnumVariantPattern &&
                                     !isConstructorPattern && !isOrPattern && !arm.guard)) {
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
    std::string identName = identPat->name;
    auto varIt = variantLookup.find(identName);
    auto variantIndex = static_cast<int64_t>(varIt->second.second);

    auto tag = extractTag(scrutinee);
    auto tagVal = createIntConstant(builder, location, builder.getI32Type(), variantIndex);
    mlir::Value cond =
        builder.create<mlir::arith::CmpIOp>(location, mlir::arith::CmpIPredicate::eq, tag, tagVal)
            .getResult();

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
    std::string ctorName = ctor->name;
    auto ctorVarIt = variantLookup.find(ctorName);
    if (ctorVarIt != variantLookup.end()) {
      auto variantIndex = static_cast<int64_t>(ctorVarIt->second.second);

      auto tag = extractTag(scrutinee);
      auto tagVal = createIntConstant(builder, location, builder.getI32Type(), variantIndex);
      mlir::Value tagCond =
          builder.create<mlir::arith::CmpIOp>(location, mlir::arith::CmpIPredicate::eq, tag, tagVal)
              .getResult();

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
          for (size_t i = 0; i < ctor->patterns.size(); ++i) {
            const auto &sp = ctor->patterns[i]->value;
            if (auto *spIdent = std::get_if<ast::PatIdentifier>(&sp.kind)) {
              if (isEnumLikeType(scrutinee.getType())) {
                int64_t fieldIdx = payloadFieldIndexForVariant(ctorName, i);
                auto fieldTy = getEnumFieldType(scrutinee.getType(), fieldIdx);
                auto pv = builder.create<hew::EnumExtractPayloadOp>(location, fieldTy, scrutinee,
                                                                    fieldIdx);
                declareVariable(spIdent->name, pv);
              }
            }
          }
          auto guardCond = generateExpression(arm.guard->value);
          if (!guardCond)
            guardCond = createIntConstant(builder, location, builder.getI1Type(), 0);
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
    // Unknown constructor, fall through
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

  // Struct pattern: irrefutable unless guarded
  if (isStructPattern && !arm.guard) {
    return generateArmBody(arm);
  }
  if (isStructPattern && arm.guard) {
    SymbolTableScopeT guardScope(symbolTable);
    MutableTableScopeT guardMutScope(mutableVars);
    auto *sp = structPatPtr;
    std::string spName = sp->name;
    auto structIt = structTypes.find(spName);
    if (structIt != structTypes.end()) {
      const auto &info = structIt->second;
      for (const auto &pf : sp->fields) {
        std::string pfName = pf.name;
        for (const auto &fi : info.fields) {
          if (fi.name == pfName) {
            auto fieldVal = builder.create<hew::FieldGetOp>(
                location,
                mlir::cast<mlir::LLVM::LLVMStructType>(scrutinee.getType()).getBody()[fi.index],
                scrutinee, builder.getStringAttr(fi.name), static_cast<int64_t>(fi.index));
            declareVariable(pfName, fieldVal);
            break;
          }
        }
      }
    }
    auto guardCond = generateExpression(arm.guard->value);
    if (!guardCond)
      return nullptr;
    return generateTagMatch(guardCond);
  }

  // For other pattern types, skip to next arm
  return generateMatchArmsChain(scrutinee, arms, idx + 1, resultType, location);
}
