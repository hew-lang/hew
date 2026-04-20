//===- MLIRGenExprDispatch.cpp - Expression dispatcher for Hew MLIRGen ----===//
//
// Split expression dispatch helpers for the MLIR generator.
//
//===----------------------------------------------------------------------===//

#include "hew/ast_helpers.h"
#include "hew/mlir/HewDialect.h"
#include "hew/mlir/HewOps.h"
#include "hew/mlir/HewTypes.h"
#include "hew/mlir/MLIRGen.h"
#include "MLIRGenHelpers.h"

#include "mlir/Dialect/Arith/IR/Arith.h"
#include "mlir/Dialect/ControlFlow/IR/ControlFlowOps.h"
#include "mlir/Dialect/Func/IR/FuncOps.h"
#include "mlir/Dialect/LLVMIR/LLVMDialect.h"
#include "mlir/Dialect/Math/IR/Math.h"
#include "mlir/Dialect/MemRef/IR/MemRef.h"
#include "mlir/Dialect/SCF/IR/SCF.h"
#include "mlir/IR/Builders.h"
#include "mlir/IR/BuiltinAttributes.h"
#include "mlir/IR/BuiltinOps.h"
#include "mlir/IR/BuiltinTypes.h"
#include "mlir/IR/Diagnostics.h"
#include "mlir/IR/Location.h"
#include "mlir/IR/Value.h"

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/ErrorHandling.h"

#include <cassert>
#include <cstdlib>
#include <type_traits>

using namespace hew;
using namespace mlir;

namespace {
template <typename T> inline constexpr bool always_false_v = false;
} // namespace

mlir::Value MLIRGen::generateExpression(const ast::Expr &expr, std::optional<mlir::Type> typeHint) {
  currentLoc = loc(expr.span);

  auto hoistIt = hoistedValues.find(&expr);
  if (hoistIt != hoistedValues.end())
    return hoistIt->second;

  return std::visit(
      [this, &expr, typeHint](const auto &node) -> mlir::Value {
        using NodeT = std::decay_t<decltype(node)>;
        if constexpr (std::is_same_v<NodeT, ast::ExprLiteral>) {
          return generateLiteral(node.lit, expr.span);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprIdentifier>) {
          return generateIdentifierExpr(node, expr.span, typeHint);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprBinary>) {
          return generateBinaryExpr(node);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprUnary>) {
          return generateUnaryExpr(node);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprCall>) {
          return generateCallExpr(node, expr.span, typeHint);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprIf>) {
          return generateIfExpr(node, expr.span);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprBlock>) {
          return generateBlockExprWithHint(node, typeHint);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprCast>) {
          return generateCastDispatchExpr(node);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprPostfixTry>) {
          return generatePostfixExpr(node);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprMatch>) {
          return generateMatchExpr(node, expr.span);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprScope>) {
          return generateScopeExpr(node);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprScopeLaunch>) {
          return generateScopeLaunchExpr(node);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprScopeSpawn>) {
          return generateScopeSpawnExpr(node);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprScopeCancel>) {
          return generateScopeCancelExpr();
        } else if constexpr (std::is_same_v<NodeT, ast::ExprSelect>) {
          return generateSelectExpr(node);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprJoin>) {
          return generateJoinExpr(node);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprSpawn>) {
          return generateSpawnExpr(node);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprSpawnLambdaActor>) {
          return generateSpawnLambdaActorExpr(node);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprSend>) {
          return generateSendExpr(node);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprStructInit>) {
          return generateStructInit(node, expr.span);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprMethodCall>) {
          return generateMethodCall(node, expr.span);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprTuple>) {
          return generateTupleExpr(node, typeHint);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprArray>) {
          return generateArrayExpr(node, typeHint);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprMapLiteral>) {
          return generateMapLiteralExpr(node, expr.span, typeHint);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprLambda>) {
          return generateLambdaExpr(node);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprInterpolatedString>) {
          return generateInterpolatedString(node);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprRegexLiteral>) {
          return generateRegexLiteral(node);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprByteStringLiteral>) {
          return generateBytesLiteral(node.data);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprByteArrayLiteral>) {
          return generateBytesLiteral(node.data);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprIfLet>) {
          return generateIfLetExpr(node, expr.span);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprArrayRepeat>) {
          return generateArrayRepeatExpr(node, expr.span);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprTimeout>) {
          return generateTimeoutExpr(node);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprUnsafe>) {
          return generateUnsafeExpr(node);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprYield>) {
          return generateYieldExpr(node);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprCooperate>) {
          return generateCooperateExpr();
        } else if constexpr (std::is_same_v<NodeT, ast::ExprThis>) {
          return generateThisExpr();
        } else if constexpr (std::is_same_v<NodeT, ast::ExprFieldAccess>) {
          return generateFieldAccessExpr(node);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprIndex>) {
          return generateIndexExpr(node);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprAwait>) {
          return generateAwaitExpr(node);
        } else if constexpr (std::is_same_v<NodeT, ast::ExprRange>) {
          return generateRangeExpr(node);
        } else {
          static_assert(always_false_v<NodeT>, "unhandled expression kind in generateExpression");
        }
      },
      expr.kind);
}

mlir::Value MLIRGen::generateIdentifierExpr(const ast::ExprIdentifier &ident,
                                            const ast::Span &exprSpan,
                                            std::optional<mlir::Type> typeHint) {
  auto name = ident.name;
  auto val = lookupVariable(name);
  if (val)
    return val;

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
                  builder, currentLoc, ptrType, structType, selfVal,
                  llvm::ArrayRef<mlir::LLVM::GEPArg>{0, static_cast<int32_t>(field.index)});
              auto fieldVal =
                  mlir::LLVM::LoadOp::create(builder, currentLoc, field.type, fieldPtr).getResult();
              if ((mlir::isa<hew::VecType>(field.semanticType) ||
                   mlir::isa<hew::HashMapType>(field.semanticType)) &&
                  field.semanticType != field.type) {
                return coerceType(fieldVal, field.semanticType, currentLoc);
              }
              return fieldVal;
            }
          }
        }
      }
    }
  }

  auto constIt = moduleConstants.find(name);
  if (constIt != moduleConstants.end())
    return generateExpression(*constIt->second);

  auto varIt = variantLookup.find(name);
  if (varIt != variantLookup.end()) {
    const auto &enumName = varIt->second.first;
    auto variantIndex = static_cast<int64_t>(varIt->second.second);

    if (name == "None" && enumName == "__Option") {
      auto location = currentLoc;
      mlir::Type optionType = resolveOptionConstructorType(typeHint, exprSpan);
      if (!optionType) {
        ++errorCount_;
        emitError(location) << "cannot determine type for `None`; add an explicit type annotation";
        return nullptr;
      }
      return hew::EnumConstructOp::create(
          builder, location, optionType, static_cast<uint32_t>(variantIndex),
          llvm::StringRef("Option"), mlir::ValueRange{}, mlir::ArrayAttr{});
    }

    auto enumIt = enumTypes.find(enumName);
    if (enumIt != enumTypes.end()) {
      const auto &enumInfo = enumIt->second;
      if (enumInfo.hasPayloads) {
        auto location = currentLoc;
        if (enumInfo.isIndirect) {
          auto innerType = enumInfo.innerStructType;
          mlir::Value structVal = hew::EnumConstructOp::create(
              builder, location, innerType, static_cast<uint32_t>(variantIndex),
              llvm::StringRef(enumName), mlir::ValueRange{}, mlir::ArrayAttr{});
          auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
          auto mallocFuncType = mlir::FunctionType::get(&context, {sizeType()}, {ptrType});
          getOrCreateExternFunc("malloc", mallocFuncType);
          auto sizeVal =
              hew::SizeOfOp::create(builder, location, sizeType(), mlir::TypeAttr::get(innerType));
          auto mallocCall = mlir::func::CallOp::create(
              builder, location, "malloc", mlir::TypeRange{ptrType}, mlir::ValueRange{sizeVal});
          auto allocPtr = mallocCall.getResult(0);
          mlir::LLVM::StoreOp::create(builder, location, structVal, allocPtr);
          return allocPtr;
        }

        return hew::EnumConstructOp::create(
            builder, location, enumInfo.mlirType, static_cast<uint32_t>(variantIndex),
            llvm::StringRef(enumName), mlir::ValueRange{}, mlir::ArrayAttr{});
      }
      return createIntConstant(builder, currentLoc, builder.getI32Type(), variantIndex);
    }

    return createIntConstant(builder, currentLoc, builder.getI32Type(), variantIndex);
  }

  std::string mangledFuncName = mangleName(currentModulePath, "", name);
  auto funcOp = module.lookupSymbol<mlir::func::FuncOp>(mangledFuncName);
  if (!funcOp)
    funcOp = module.lookupSymbol<mlir::func::FuncOp>(name);
  if (funcOp) {
    auto symName = funcOp.getSymName();
    return mlir::func::ConstantOp::create(builder, currentLoc, funcOp.getFunctionType(),
                                          mlir::SymbolRefAttr::get(&context, symName));
  }

  ++errorCount_;
  emitError(currentLoc) << "undeclared variable '" << name
                        << "'; did you mean to declare it with 'let' or 'var'?";
  return nullptr;
}

mlir::Value MLIRGen::generateBlockExprWithHint(const ast::ExprBlock &expr,
                                               std::optional<mlir::Type> typeHint) {
  if (expr.block.stmts.empty() && !expr.block.trailing_expr && typeHint &&
      mlir::isa<hew::HashMapType>(*typeHint)) {
    return hew::HashMapNewOp::create(builder, currentLoc, *typeHint).getResult();
  }
  return generateBlockExpr(expr.block);
}

mlir::Value MLIRGen::generateCastDispatchExpr(const ast::ExprCast &expr) {
  auto location = currentLoc;
  auto value = generateExpression(expr.expr->value);
  if (!value)
    return nullptr;
  currentLoc = location;
  auto targetType = convertTypeOrError(expr.ty.value, "cannot resolve cast target type");
  if (!targetType)
    return nullptr;
  bool isUnsigned = isUnsignedTypeExpr(expr.ty.value);
  return coerceType(value, targetType, location, isUnsigned);
}

mlir::Value MLIRGen::generateTimeoutExpr(const ast::ExprTimeout &expr) {
  generateExpression(expr.duration->value);
  return generateExpression(expr.expr->value);
}

mlir::Value MLIRGen::generateUnsafeExpr(const ast::ExprUnsafe &expr) {
  auto unsafeResult = generateBlock(expr.block);
  if (!unsafeResult)
    return nullptr;
  return unsafeResult;
}

mlir::Value MLIRGen::generateYieldExpr(const ast::ExprYield &expr) {
  if (currentCoroPromisePtr) {
    if (expr.value.has_value() && *expr.value) {
      auto prevInsideGeneratorYieldValue = insideGeneratorYieldValue;
      insideGeneratorYieldValue = true;
      auto yieldVal = generateExpression((*expr.value)->value);
      insideGeneratorYieldValue = prevInsideGeneratorYieldValue;
      if (yieldVal) {
        auto yieldLocation = currentLoc;
        mlir::LLVM::StoreOp::create(builder, yieldLocation, yieldVal, currentCoroPromisePtr);
        auto suspendMarker = module.lookupSymbol<mlir::func::FuncOp>("__hew_coro_suspend");
        if (suspendMarker) {
          mlir::func::CallOp::create(builder, yieldLocation, suspendMarker,
                                     mlir::ValueRange{currentCoroPromisePtr});
        }
        return yieldVal;
      }
    }
    return nullptr;
  }

  if (currentGenCtx) {
    if (expr.value.has_value() && *expr.value) {
      auto prevInsideGeneratorYieldValue = insideGeneratorYieldValue;
      insideGeneratorYieldValue = true;
      auto yieldVal = generateExpression((*expr.value)->value);
      insideGeneratorYieldValue = prevInsideGeneratorYieldValue;
      if (yieldVal) {
        auto yieldLocation = currentLoc;
        auto ptrTy = mlir::LLVM::LLVMPointerType::get(&context);
        auto i64Ty = builder.getI64Type();
        auto valType = yieldVal.getType();
        auto one = mlir::arith::ConstantIntOp::create(builder, yieldLocation, i64Ty, 1);
        auto valAlloca = mlir::LLVM::AllocaOp::create(builder, yieldLocation, ptrTy, valType, one);
        mlir::LLVM::StoreOp::create(builder, yieldLocation, yieldVal, valAlloca);
        auto valSize =
            hew::SizeOfOp::create(builder, yieldLocation, sizeType(), mlir::TypeAttr::get(valType));
        auto i1Ty = builder.getI1Type();
        hew::GenYieldOp::create(builder, yieldLocation, i1Ty, currentGenCtx, valAlloca, valSize);
        return yieldVal;
      }
    }
  }

  if (!currentGenCtx && !currentCoroPromisePtr) {
    ++errorCount_;
    emitError(currentLoc) << "yield expression outside generator function";
  }
  return nullptr;
}

mlir::Value MLIRGen::generateCooperateExpr() {
  hew::CooperateOp::create(builder, currentLoc);
  return createIntConstant(builder, currentLoc, builder.getI32Type(), 0);
}

mlir::Value MLIRGen::generateThisExpr() {
  assert(!currentActorName.empty() && "'this' expression encountered outside of an actor context");
  auto refType = hew::TypedActorRefType::get(&context, builder.getStringAttr(currentActorName));
  return hew::ActorSelfOp::create(builder, currentLoc, refType).getResult();
}

mlir::Value MLIRGen::generateFieldAccessExpr(const ast::ExprFieldAccess &fa) {
  auto location = currentLoc;
  if (insideGeneratorYieldValue) {
    ++errorCount_;
    emitError(location) << "yielding a field from a generator is not yet supported "
                        << "(field-alias ownership tracking is required); "
                        << "yield the whole value or clone the field instead";
    return nullptr;
  }
  auto operandVal = generateExpression(fa.object->value);
  if (!operandVal)
    return nullptr;

  auto operandType = operandVal.getType();
  const auto &fieldName = fa.field;

  if (isPointerLikeType(operandType)) {
    if (auto handleTy = mlir::dyn_cast<hew::HandleType>(operandVal.getType())) {
      auto strRefType = hew::StringRefType::get(&context);
      if (handleTy.getHandleKind() == "http.Request") {
        if (fieldName == "path") {
          return hew::RuntimeCallOp::create(
                     builder, location, mlir::TypeRange{strRefType},
                     mlir::SymbolRefAttr::get(&context, "hew_http_request_path"),
                     mlir::ValueRange{operandVal})
              .getResult();
        }
        if (fieldName == "method") {
          return hew::RuntimeCallOp::create(
                     builder, location, mlir::TypeRange{strRefType},
                     mlir::SymbolRefAttr::get(&context, "hew_http_request_method"),
                     mlir::ValueRange{operandVal})
              .getResult();
        }
      }
    }

    if (auto *objIdent = std::get_if<ast::ExprIdentifier>(&fa.object->value.kind)) {
      auto avIt = actorVarTypes.find(objIdent->name);
      if (avIt != actorVarTypes.end()) {
        auto scnIt = supervisorChildNames.find(avIt->second);
        if (scnIt != supervisorChildNames.end()) {
          const auto &childNameTypes = scnIt->second;
          for (size_t i = 0; i < childNameTypes.size(); ++i) {
            if (childNameTypes[i].first == fieldName) {
              auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
              auto i32Type = builder.getI32Type();
              auto idxVal = createIntConstant(builder, location, i32Type, static_cast<int64_t>(i));
              bool childIsSupervisor = supervisorChildren.count(childNameTypes[i].second) > 0;
              if (childIsSupervisor) {
                return hew::RuntimeCallOp::create(
                           builder, location, mlir::TypeRange{ptrType},
                           mlir::SymbolRefAttr::get(&context,
                                                    "hew_supervisor_get_child_supervisor"),
                           mlir::ValueRange{operandVal, idxVal})
                    .getResult();
              }
              auto timeoutVal =
                  mlir::arith::ConstantIntOp::create(builder, location, builder.getI32Type(), 5000);
              return hew::RuntimeCallOp::create(
                         builder, location, mlir::TypeRange{ptrType},
                         mlir::SymbolRefAttr::get(&context, "hew_supervisor_get_child_wait"),
                         mlir::ValueRange{operandVal, idxVal, timeoutVal})
                  .getResult();
            }
          }
        }
      }
    }

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
          auto fieldVal =
              mlir::LLVM::LoadOp::create(builder, location, field.type, fieldPtr).getResult();
          if (field.semanticType != field.type)
            return coerceType(fieldVal, field.semanticType, location);
          return fieldVal;
        }
      }
    }
    ++errorCount_;
    emitError(location) << "field '" << fieldName << "' not found on pointer type";
    return nullptr;
  }

  if (auto hewTuple = mlir::dyn_cast<hew::HewTupleType>(operandType)) {
    char *end = nullptr;
    unsigned long numericIdx = std::strtoul(fieldName.c_str(), &end, 10);
    bool isNumericField = (end != fieldName.c_str() && *end == '\0');
    if (!isNumericField) {
      ++errorCount_;
      emitError(location) << "named field access on tuple type";
      return nullptr;
    }
    auto elemTypes = hewTuple.getElementTypes();
    if (numericIdx >= elemTypes.size()) {
      ++errorCount_;
      emitError(location) << "tuple index " << numericIdx << " out of bounds (size "
                          << elemTypes.size() << ")";
      return nullptr;
    }
    return hew::TupleExtractOp::create(builder, location, elemTypes[numericIdx], operandVal,
                                       numericIdx);
  }

  auto structType = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(operandType);
  if (!structType) {
    ++errorCount_;
    emitError(location) << "field access on non-struct type";
    return nullptr;
  }

  char *end = nullptr;
  unsigned long numericIdx = std::strtoul(fieldName.c_str(), &end, 10);
  bool isNumericField = (end != fieldName.c_str() && *end == '\0');
  if (isNumericField) {
    auto bodyTypes = structType.getBody();
    if (numericIdx >= bodyTypes.size()) {
      ++errorCount_;
      emitError(location) << "tuple index " << numericIdx << " out of bounds (size "
                          << bodyTypes.size() << ")";
      return nullptr;
    }
    return mlir::LLVM::ExtractValueOp::create(builder, location, operandVal, numericIdx);
  }

  if (!structType.isIdentified() && !currentMachineEventTypeName_.empty()) {
    auto enumIt = enumTypes.find(currentMachineEventTypeName_);
    if (enumIt != enumTypes.end() && !currentMachineEventVariant_.empty()) {
      for (const auto &variant : enumIt->second.variants) {
        if (variant.name != currentMachineEventVariant_)
          continue;
        for (size_t i = 0; i < variant.fieldNames.size(); ++i) {
          if (variant.fieldNames[i] == fieldName) {
            auto fieldTy = variant.payloadTypes[i];
            return hew::EnumExtractPayloadOp::create(builder, location, fieldTy, operandVal,
                                                     variant.payloadPositions[i]);
          }
        }
        break;
      }
    }
  }
  if (!structType.isIdentified()) {
    ++errorCount_;
    emitError(location) << "named field access on anonymous struct type";
    return nullptr;
  }
  llvm::StringRef structName = structType.getName();
  auto it = structTypes.find(structName.str());
  if (it != structTypes.end()) {
    const auto &info = it->second;
    for (const auto &field : info.fields) {
      if (field.name == fieldName) {
        auto fieldVal = hew::FieldGetOp::create(builder, location, field.type, operandVal,
                                                builder.getStringAttr(fieldName),
                                                builder.getI64IntegerAttr(field.index))
                            .getResult();
        if (field.semanticType != field.type)
          return coerceType(fieldVal, field.semanticType, location);
        return fieldVal;
      }
    }

    ++errorCount_;
    auto diag = emitError(location)
                << "no field '" << fieldName << "' on struct '" << structName << "'";
    if (!info.fields.empty()) {
      diag << "; available fields: ";
      for (size_t i = 0; i < info.fields.size(); ++i) {
        if (i > 0)
          diag << ", ";
        diag << info.fields[i].name;
      }
    }
    return nullptr;
  }

  std::string lookupName = structName.str();
  if (lookupName.empty() && !currentMachineEventTypeName_.empty())
    lookupName = currentMachineEventTypeName_;
  auto enumIt = enumTypes.find(lookupName);
  if (enumIt != enumTypes.end() && !currentMachineSourceVariant_.empty()) {
    const auto &enumInfo = enumIt->second;
    const std::string &variantName =
        (!currentMachineEventTypeName_.empty() && structName.str() == currentMachineEventTypeName_)
            ? currentMachineEventVariant_
            : currentMachineSourceVariant_;
    for (const auto &variant : enumInfo.variants) {
      if (variant.name != variantName)
        continue;
      for (size_t i = 0; i < variant.fieldNames.size(); ++i) {
        if (variant.fieldNames[i] == fieldName) {
          auto fieldTy = getEnumFieldType(operandType, variant.payloadPositions[i]);
          return hew::EnumExtractPayloadOp::create(builder, location, fieldTy, operandVal,
                                                   variant.payloadPositions[i]);
        }
      }
      break;
    }
  }

  ++errorCount_;
  emitError(location) << "unknown struct type '" << structName << "'";
  return nullptr;
}

mlir::Value MLIRGen::generateIndexExpr(const ast::ExprIndex &idx) {
  auto location = currentLoc;
  auto operandVal = generateExpression(idx.object->value);
  if (!operandVal)
    return nullptr;

  if (auto hewArrayType = mlir::dyn_cast<hew::HewArrayType>(operandVal.getType())) {
    auto indexVal = generateExpression(idx.index->value);
    if (!indexVal)
      return nullptr;
    if (auto constOp = indexVal.getDefiningOp<mlir::arith::ConstantIntOp>()) {
      auto idxConst = constOp.value();
      return hew::ArrayExtractOp::create(builder, location, hewArrayType.getElementType(),
                                         operandVal, idxConst);
    }
    auto llvmArrayType =
        mlir::LLVM::LLVMArrayType::get(hewArrayType.getElementType(), hewArrayType.getSize());
    auto llvmArray = hew::BitcastOp::create(builder, location, llvmArrayType, operandVal);
    auto alloca = mlir::LLVM::AllocaOp::create(
        builder, location, mlir::LLVM::LLVMPointerType::get(&context), llvmArrayType,
        mlir::arith::ConstantIntOp::create(builder, location, 1, 64));
    mlir::LLVM::StoreOp::create(builder, location, llvmArray, alloca);
    auto i64Type = builder.getI64Type();
    mlir::Value idx64 = indexVal;
    if (indexVal.getType() != i64Type)
      idx64 = mlir::arith::ExtSIOp::create(builder, location, i64Type, indexVal);
    auto zero = mlir::arith::ConstantIntOp::create(builder, location, 0, 64);
    auto elemPtr =
        mlir::LLVM::GEPOp::create(builder, location, mlir::LLVM::LLVMPointerType::get(&context),
                                  llvmArrayType, alloca, mlir::ValueRange{zero, idx64});
    return mlir::LLVM::LoadOp::create(builder, location, hewArrayType.getElementType(), elemPtr);
  }

  auto arrayType = mlir::dyn_cast<mlir::LLVM::LLVMArrayType>(operandVal.getType());
  if (arrayType) {
    auto indexVal = generateExpression(idx.index->value);
    if (!indexVal)
      return nullptr;
    if (auto constOp = indexVal.getDefiningOp<mlir::arith::ConstantIntOp>()) {
      auto idxConst = constOp.value();
      return mlir::LLVM::ExtractValueOp::create(builder, location, operandVal, idxConst);
    }
    auto alloca = mlir::LLVM::AllocaOp::create(
        builder, location, mlir::LLVM::LLVMPointerType::get(&context), arrayType,
        mlir::arith::ConstantIntOp::create(builder, location, 1, 64));
    mlir::LLVM::StoreOp::create(builder, location, operandVal, alloca);
    auto i64Type = builder.getI64Type();
    mlir::Value idx64 = indexVal;
    if (indexVal.getType() != i64Type)
      idx64 = mlir::arith::ExtSIOp::create(builder, location, i64Type, indexVal);
    auto zero = mlir::arith::ConstantIntOp::create(builder, location, 0, 64);
    auto elemPtr =
        mlir::LLVM::GEPOp::create(builder, location, mlir::LLVM::LLVMPointerType::get(&context),
                                  arrayType, alloca, mlir::ValueRange{zero, idx64});
    return mlir::LLVM::LoadOp::create(builder, location, arrayType.getElementType(), elemPtr);
  }

  if (auto vecType = mlir::dyn_cast<hew::VecType>(operandVal.getType())) {
    auto indexVal = generateExpression(idx.index->value);
    if (!indexVal)
      return nullptr;
    auto i64Type = builder.getI64Type();
    mlir::Value idx64 = indexVal;
    if (indexVal.getType() != i64Type)
      idx64 = mlir::arith::ExtSIOp::create(builder, location, i64Type, indexVal);
    return hew::VecGetOp::create(builder, location, vecType.getElementType(), operandVal, idx64);
  }

  if (auto structType = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(operandVal.getType())) {
    if (structType.isIdentified()) {
      auto indexVal = generateExpression(idx.index->value);
      if (!indexVal)
        return nullptr;
      std::string funcName = mangleName(currentModulePath, structType.getName().str(), "get");
      llvm::SmallVector<mlir::Value, 2> args{operandVal, indexVal};
      auto callee = module.lookupSymbol<mlir::func::FuncOp>(funcName);
      if (!callee)
        callee = lookupImportedFunc(structType.getName(), "get");
      if (callee) {
        auto funcType = callee.getFunctionType();
        for (size_t i = 0; i < args.size() && i < funcType.getNumInputs(); ++i) {
          if (args[i].getType() != funcType.getInput(i)) {
            args[i] = coerceType(args[i], funcType.getInput(i), location);
            if (!args[i])
              return nullptr;
          }
        }
        auto callOp = mlir::func::CallOp::create(builder, location, callee, args);
        if (callOp.getNumResults() > 0)
          return callOp.getResult(0);
        return nullptr;
      }
    }
  }

  ++errorCount_;
  emitError(location) << "indexing not supported for this type";
  return nullptr;
}

mlir::Value MLIRGen::generateAwaitExpr(const ast::ExprAwait &awaitE) {
  auto location = currentLoc;
  if (auto *mc = std::get_if<ast::ExprMethodCall>(&awaitE.inner->value.kind)) {
    auto mcLocation = loc(awaitE.inner->span);
    auto receiver = generateExpression(mc->receiver->value);
    if (!receiver)
      return nullptr;
    std::string actorTypeName = resolveActorTypeName(mc->receiver->value, &mc->receiver->span);
    if (!actorTypeName.empty()) {
      auto actorIt = actorRegistry.find(actorTypeName);
      if (actorIt != actorRegistry.end())
        return generateActorMethodAsk(receiver, actorIt->second, mc->method, mc->args, mcLocation);
    }
    ++errorCount_;
    emitError(location) << "await requires an actor method call";
    return nullptr;
  }

  auto operand = generateExpression(awaitE.inner->value);
  if (!operand)
    return nullptr;

  if (mlir::isa<hew::ActorRefType, hew::TypedActorRefType>(operand.getType())) {
    auto awaitOp = builder.create<hew::ActorAwaitOp>(location, builder.getI32Type(), operand);
    return awaitOp.getResult();
  }

  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  if (auto handleTy = mlir::dyn_cast<hew::HandleType>(operand.getType())) {
    if (handleTy.getHandleKind() == "Task")
      operand = hew::BitcastOp::create(builder, location, ptrType, operand);
  }
  if (mlir::isa<mlir::LLVM::LLVMPointerType>(operand.getType())) {
    auto resultPtr = hew::ScopeAwaitOp::create(builder, location, ptrType, operand);
    auto resolveScopeAwaitResultType = [&]() -> mlir::Type {
      if (auto *ie = std::get_if<ast::ExprIdentifier>(&awaitE.inner->value.kind)) {
        auto it = taskResultTypes.find(ie->name);
        if (it != taskResultTypes.end())
          return it->second;
      }
      const auto *resolvedTaskType =
          requireResolvedTypeOf(awaitE.inner->span, "scope.await operand", location);
      if (!resolvedTaskType)
        return nullptr;
      auto *taskNamed = std::get_if<ast::TypeNamed>(&resolvedTaskType->kind);
      if (!taskNamed) {
        ++errorCount_;
        emitError(location) << "scope.await operand must resolve to Task<T>";
        return nullptr;
      }
      if ((!taskNamed->type_args || taskNamed->type_args->size() != 1) &&
          resolveTypeAlias(taskNamed->name) != taskNamed->name) {
        if (const auto *aliasType = resolveTypeAliasExpr(taskNamed->name)) {
          if (const auto *aliasNamed = std::get_if<ast::TypeNamed>(&aliasType->kind))
            taskNamed = aliasNamed;
        }
      }
      auto taskName = resolveTypeAlias(taskNamed->name);
      if ((taskName != "Task" && taskName != "scope.Task") ||
          !(taskNamed->type_args && taskNamed->type_args->size() == 1)) {
        ++errorCount_;
        emitError(location) << "scope.await operand must resolve to Task<T>";
        return nullptr;
      }
      return convertTypeOrError((*taskNamed->type_args)[0].value,
                                "cannot determine scope.await result type", location);
    };

    auto resultType = resolveScopeAwaitResultType();
    if (!resultType)
      return nullptr;
    return mlir::LLVM::LoadOp::create(builder, location, resultType, resultPtr);
  }
  return operand;
}

mlir::Value MLIRGen::generateRangeExpr(const ast::ExprRange &range) {
  if (!range.start || !range.end) {
    ++errorCount_;
    emitError(currentLoc) << "unbounded ranges not yet supported as values";
    return nullptr;
  }

  auto startVal = generateExpression((*range.start)->value);
  auto endVal = generateExpression((*range.end)->value);
  if (!startVal || !endVal)
    return nullptr;

  if (startVal.getType() != endVal.getType()) {
    endVal = coerceType(endVal, startVal.getType(), currentLoc);
    if (!endVal)
      return nullptr;
  }

  if (range.inclusive) {
    if (endVal.getType().isIntOrIndex()) {
      auto one = createIntConstant(builder, currentLoc, endVal.getType(), 1);
      endVal = mlir::arith::AddIOp::create(builder, currentLoc, endVal, one);
    } else {
      ++errorCount_;
      emitError(currentLoc) << "inclusive range only supported for integers";
      return nullptr;
    }
  }

  auto tupleType = hew::HewTupleType::get(&context, {startVal.getType(), endVal.getType()});
  return hew::TupleCreateOp::create(builder, currentLoc, tupleType,
                                    mlir::ValueRange{startVal, endVal});
}
