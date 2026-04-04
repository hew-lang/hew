//===- MLIRGenExpr.cpp - Expression codegen for Hew MLIRGen ---------------===//
//
// Expression generation methods: literals, identifiers, binary/unary, calls,
// if-expr, postfix, struct init, method calls, tuples, arrays, lambdas.
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

#include "llvm/ADT/ScopedHashTable.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <functional>
#include <string>
#include <unordered_map>

using namespace hew;
using namespace mlir;

// ============================================================================
// Expression generation
// ============================================================================

mlir::Value MLIRGen::generateExpression(const ast::Expr &expr) {
  currentLoc = loc(expr.span);

  // Return pre-computed value for hoisted loop-invariant sub-expressions.
  auto hoistIt = hoistedValues.find(&expr);
  if (hoistIt != hoistedValues.end())
    return hoistIt->second;

  if (auto *lit = std::get_if<ast::ExprLiteral>(&expr.kind))
    return generateLiteral(lit->lit, expr.span);

  if (auto *ident = std::get_if<ast::ExprIdentifier>(&expr.kind)) {
    auto name = ident->name;
    // Check regular variables first
    auto val = lookupVariable(name);
    if (val)
      return val;

    // Inside actor init/receive, bare field names (e.g. `handle`) resolve to
    // self.field via GEP into the actor state struct.
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
                    mlir::LLVM::LoadOp::create(builder, currentLoc, field.type, fieldPtr)
                        .getResult();
                if ((mlir::isa<hew::VecType>(field.semanticType) ||
                     mlir::isa<hew::HashMapType>(field.semanticType)) &&
                    field.semanticType != field.type)
                  return coerceType(fieldVal, field.semanticType, currentLoc);
                return fieldVal;
              }
            }
          }
        }
      }
    }

    // Check module-level constants
    auto constIt = moduleConstants.find(name);
    if (constIt != moduleConstants.end()) {
      return generateExpression(*constIt->second);
    }

    // Check enum unit variants (e.g., Red, Green, Blue, None)
    auto varIt = variantLookup.find(name);
    if (varIt != variantLookup.end()) {
      const auto &enumName = varIt->second.first;
      auto variantIndex = static_cast<int64_t>(varIt->second.second);

      // Built-in None: construct Option { tag=0 }
      if (name == "None" && enumName == "__Option") {
        auto location = currentLoc;
        mlir::Type optionType;
        if (pendingDeclaredType && mlir::isa<hew::OptionEnumType>(*pendingDeclaredType))
          optionType = *pendingDeclaredType;
        else if (currentFunction && currentFunction.getResultTypes().size() == 1 &&
                 llvm::isa<hew::OptionEnumType>(currentFunction.getResultTypes()[0]))
          optionType = currentFunction.getResultTypes()[0];
        else if (auto *resolvedType = resolvedTypeOf(expr.span)) {
          auto converted = convertType(*resolvedType);
          if (converted && mlir::isa<hew::OptionEnumType>(converted))
            optionType = converted;
        }
        if (!optionType) {
          ++errorCount_;
          emitError(location)
              << "cannot determine type for `None`; add an explicit type annotation";
          return nullptr;
        }
        // No payload → mlir::ArrayAttr{} (null) is intentionally absent.
        // EnumConstructOp::verify() skips position checking when payloads
        // is empty, so the null ArrayAttr is semantically correct here.
        mlir::Value result = hew::EnumConstructOp::create(
            builder, location, optionType, static_cast<uint32_t>(variantIndex),
            llvm::StringRef("Option"), mlir::ValueRange{},
            /*payload_positions=*/mlir::ArrayAttr{});
        return result;
      }

      auto enumIt = enumTypes.find(enumName);
      if (enumIt != enumTypes.end()) {
        const auto &enumInfo = enumIt->second;

        if (enumInfo.hasPayloads) {
          // Unit variant of a payload enum: build struct { tag, undef... }
          auto location = currentLoc;

          if (enumInfo.isIndirect) {
            // Indirect enum: allocate on heap
            auto innerType = enumInfo.innerStructType;
            mlir::Value structVal = hew::EnumConstructOp::create(
                builder, location, innerType, static_cast<uint32_t>(variantIndex),
                llvm::StringRef(enumName), mlir::ValueRange{},
                /*payload_positions=*/mlir::ArrayAttr{});
            auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
            auto mallocFuncType = mlir::FunctionType::get(&context, {sizeType()}, {ptrType});
            getOrCreateExternFunc("malloc", mallocFuncType);
            auto sizeVal = hew::SizeOfOp::create(builder, location, sizeType(),
                                                 mlir::TypeAttr::get(innerType));
            auto mallocCall = mlir::func::CallOp::create(
                builder, location, "malloc", mlir::TypeRange{ptrType}, mlir::ValueRange{sizeVal});
            auto allocPtr = mallocCall.getResult(0);
            mlir::LLVM::StoreOp::create(builder, location, structVal, allocPtr);
            return allocPtr;
          }

          mlir::Value result = hew::EnumConstructOp::create(
              builder, location, enumInfo.mlirType, static_cast<uint32_t>(variantIndex),
              llvm::StringRef(enumName), mlir::ValueRange{},
              /*payload_positions=*/mlir::ArrayAttr{});
          return result;
        }
        // All-unit enum: just produce the tag index as i32
        return createIntConstant(builder, currentLoc, builder.getI32Type(), variantIndex);
      }

      // Built-in variant with no enumTypes entry
      return createIntConstant(builder, currentLoc, builder.getI32Type(), variantIndex);
    }

    // Check if the name refers to a module-level function (function-as-value)
    std::string mangledFuncName = mangleName(currentModulePath, "", name);
    auto funcOp = module.lookupSymbol<mlir::func::FuncOp>(mangledFuncName);
    if (!funcOp)
      funcOp = module.lookupSymbol<mlir::func::FuncOp>(name); // fallback to unmangled
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

  if (auto *bin = std::get_if<ast::ExprBinary>(&expr.kind))
    return generateBinaryExpr(*bin);
  if (auto *un = std::get_if<ast::ExprUnary>(&expr.kind))
    return generateUnaryExpr(*un);
  if (auto *call = std::get_if<ast::ExprCall>(&expr.kind))
    return generateCallExpr(*call);
  if (auto *ifE = std::get_if<ast::ExprIf>(&expr.kind))
    return generateIfExpr(*ifE, expr.span);
  if (auto *blockExpr = std::get_if<ast::ExprBlock>(&expr.kind)) {
    // Empty block {} coerces to HashMap when pendingDeclaredType expects it
    if (blockExpr->block.stmts.empty() && !blockExpr->block.trailing_expr && pendingDeclaredType &&
        mlir::isa<hew::HashMapType>(*pendingDeclaredType)) {
      auto hmType = *pendingDeclaredType;
      pendingDeclaredType.reset();
      return hew::HashMapNewOp::create(builder, currentLoc, hmType).getResult();
    }
    auto blockResult = generateBlockExpr(blockExpr->block);
    if (!blockResult)
      return nullptr;
    return blockResult;
  }
  if (auto *cast = std::get_if<ast::ExprCast>(&expr.kind)) {
    auto location = currentLoc;
    auto value = generateExpression(cast->expr->value);
    if (!value)
      return nullptr;
    currentLoc = location;
    auto targetType = convertTypeOrError(cast->ty.value, "cannot resolve cast target type");
    if (!targetType)
      return nullptr;
    bool isUnsigned = isUnsignedTypeExpr(cast->ty.value);
    return coerceType(value, targetType, location, isUnsigned);
  }
  if (auto *pf = std::get_if<ast::ExprPostfixTry>(&expr.kind))
    return generatePostfixExpr(*pf);
  if (auto *me = std::get_if<ast::ExprMatch>(&expr.kind))
    return generateMatchExpr(*me, expr.span);
  if (auto *se = std::get_if<ast::ExprScope>(&expr.kind))
    return generateScopeExpr(*se);
  if (auto *sle = std::get_if<ast::ExprScopeLaunch>(&expr.kind))
    return generateScopeLaunchExpr(*sle);
  if (auto *sse = std::get_if<ast::ExprScopeSpawn>(&expr.kind))
    return generateScopeSpawnExpr(*sse);
  if (std::get_if<ast::ExprScopeCancel>(&expr.kind))
    return generateScopeCancelExpr();

  if (auto *sel = std::get_if<ast::ExprSelect>(&expr.kind))
    return generateSelectExpr(*sel);
  if (auto *join = std::get_if<ast::ExprJoin>(&expr.kind))
    return generateJoinExpr(*join);
  if (auto *spawn = std::get_if<ast::ExprSpawn>(&expr.kind))
    return generateSpawnExpr(*spawn);
  if (auto *spawnLambda = std::get_if<ast::ExprSpawnLambdaActor>(&expr.kind))
    return generateSpawnLambdaActorExpr(*spawnLambda);
  if (auto *send = std::get_if<ast::ExprSend>(&expr.kind))
    return generateSendExpr(*send);
  if (auto *si = std::get_if<ast::ExprStructInit>(&expr.kind))
    return generateStructInit(*si, expr.span);
  if (auto *mc = std::get_if<ast::ExprMethodCall>(&expr.kind))
    return generateMethodCall(*mc);
  if (auto *tup = std::get_if<ast::ExprTuple>(&expr.kind))
    return generateTupleExpr(*tup);
  if (auto *arr = std::get_if<ast::ExprArray>(&expr.kind))
    return generateArrayExpr(*arr);
  if (auto *mapLit = std::get_if<ast::ExprMapLiteral>(&expr.kind))
    return generateMapLiteralExpr(*mapLit, expr.span);
  if (auto *lam = std::get_if<ast::ExprLambda>(&expr.kind))
    return generateLambdaExpr(*lam);
  if (auto *interp = std::get_if<ast::ExprInterpolatedString>(&expr.kind))
    return generateInterpolatedString(*interp);
  if (auto *regex = std::get_if<ast::ExprRegexLiteral>(&expr.kind))
    return generateRegexLiteral(*regex);
  if (auto *bsl = std::get_if<ast::ExprByteStringLiteral>(&expr.kind))
    return generateBytesLiteral(bsl->data);
  if (auto *bal = std::get_if<ast::ExprByteArrayLiteral>(&expr.kind))
    return generateBytesLiteral(bal->data);
  if (auto *ifLet = std::get_if<ast::ExprIfLet>(&expr.kind))
    return generateIfLetExpr(*ifLet, expr.span);
  if (auto *arrRepeat = std::get_if<ast::ExprArrayRepeat>(&expr.kind))
    return generateArrayRepeatExpr(*arrRepeat, expr.span);
  if (auto *to = std::get_if<ast::ExprTimeout>(&expr.kind)) {
    // Evaluate duration for validation but discard; timeout not yet enforced
    generateExpression(to->duration->value);
    return generateExpression(to->expr->value);
  }

  if (auto *ue = std::get_if<ast::ExprUnsafe>(&expr.kind)) {
    auto unsafeResult = generateBlock(ue->block);
    if (!unsafeResult)
      return nullptr;
    return unsafeResult;
  }

  if (auto *yield = std::get_if<ast::ExprYield>(&expr.kind)) {
    if (currentCoroPromisePtr) {
      // Coroutine-based generator: store value to promise ptr and call suspend marker
      if (yield->value.has_value() && *yield->value) {
        auto yieldVal = generateExpression((*yield->value)->value);
        if (yieldVal) {
          auto yieldLocation = currentLoc;
          auto ptrTy = mlir::LLVM::LLVMPointerType::get(&context);

          // Store the yielded value into the promise slot
          mlir::LLVM::StoreOp::create(builder, yieldLocation, yieldVal, currentCoroPromisePtr);

          // Call the suspend marker: __hew_coro_suspend(promise_ptr)
          auto suspendMarker = module.lookupSymbol<mlir::func::FuncOp>("__hew_coro_suspend");
          if (suspendMarker) {
            mlir::func::CallOp::create(builder, yieldLocation, suspendMarker,
                                       mlir::ValueRange{currentCoroPromisePtr});
          }

          return yieldVal; // Return the yielded value (unused by caller)
        }
      }
      return nullptr;
    }
    if (currentGenCtx) {
      // Thread-based generator: call hew_gen_yield(ctx, &val, sizeof(val))
      if (yield->value.has_value() && *yield->value) {
        auto yieldVal = generateExpression((*yield->value)->value);
        if (yieldVal) {
          auto yieldLocation = currentLoc;
          auto ptrTy = mlir::LLVM::LLVMPointerType::get(&context);
          auto i64Ty = builder.getI64Type();
          auto valType = yieldVal.getType();

          // Alloca for the value
          auto one = mlir::arith::ConstantIntOp::create(builder, yieldLocation, i64Ty, 1);
          auto valAlloca =
              mlir::LLVM::AllocaOp::create(builder, yieldLocation, ptrTy, valType, one);
          mlir::LLVM::StoreOp::create(builder, yieldLocation, yieldVal, valAlloca);

          // Compute sizeof(valType)
          auto valSize = hew::SizeOfOp::create(builder, yieldLocation, sizeType(),
                                               mlir::TypeAttr::get(valType));

          // Call hew_gen_yield(ctx, &val, size) -> bool (i1)
          auto i1Ty = builder.getI1Type();
          hew::GenYieldOp::create(builder, yieldLocation, i1Ty, currentGenCtx, valAlloca, valSize);
          // For now, ignore the return value (cancellation not handled)
          return yieldVal; // Return the yielded value (unused by caller)
        }
      }
    }
    // Yield expressions outside generator context are handled at the
    // statement level during static generator codegen.
    if (!currentGenCtx && !currentCoroPromisePtr) {
      emitError(currentLoc) << "yield expression outside generator function";
    }
    return nullptr;
  }

  if (std::get_if<ast::ExprCooperate>(&expr.kind)) {
    // Cooperative scheduler yield point
    hew::CooperateOp::create(builder, currentLoc);
    return createIntConstant(builder, currentLoc, builder.getI32Type(), 0);
  }

  if (std::get_if<ast::ExprThis>(&expr.kind)) {
    // Actor self-reference handle — must be inside an actor definition
    assert(!currentActorName.empty() &&
           "'this' expression encountered outside of an actor context");
    auto refType = hew::TypedActorRefType::get(&context, builder.getStringAttr(currentActorName));
    return hew::ActorSelfOp::create(builder, currentLoc, refType).getResult();
  }

  if (auto *fa = std::get_if<ast::ExprFieldAccess>(&expr.kind)) {
    auto location = currentLoc;
    auto operandVal = generateExpression(fa->object->value);
    if (!operandVal)
      return nullptr;

    auto operandType = operandVal.getType();
    const auto &fieldName = fa->field;

    // Handle pointer operands (e.g., `self` in actor receive handlers)
    if (isPointerLikeType(operandType)) {
      // Check for handle type property access (e.g., req.path, req.method)
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

      // Named supervisor child access: sup.child_name → supervisor_child(sup, idx)
      if (auto *objIdent = std::get_if<ast::ExprIdentifier>(&fa->object->value.kind)) {
        auto avIt = actorVarTypes.find(objIdent->name);
        if (avIt != actorVarTypes.end()) {
          auto scnIt = supervisorChildNames.find(avIt->second);
          if (scnIt != supervisorChildNames.end()) {
            const auto &childNameTypes = scnIt->second;
            for (size_t i = 0; i < childNameTypes.size(); ++i) {
              if (childNameTypes[i].first == fieldName) {
                auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
                auto i32Type = builder.getI32Type();
                auto idxVal =
                    createIntConstant(builder, location, i32Type, static_cast<int64_t>(i));

                // Check if the child is itself a supervisor
                bool childIsSupervisor = supervisorChildren.count(childNameTypes[i].second) > 0;
                if (childIsSupervisor) {
                  return hew::RuntimeCallOp::create(
                             builder, location, mlir::TypeRange{ptrType},
                             mlir::SymbolRefAttr::get(&context,
                                                      "hew_supervisor_get_child_supervisor"),
                             mlir::ValueRange{operandVal, idxVal})
                      .getResult();
                }
                // Wait up to 5 seconds for the child to be available
                // (handles restart windows where children[idx] is temporarily null).
                auto timeoutVal = mlir::arith::ConstantIntOp::create(
                    builder, location, builder.getI32Type(), 5000);
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

      // Search structTypes — prefer the specific struct when known
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
      emitError(location) << "field '" << fieldName << "' not found on pointer type";
      return nullptr;
    }

    // Check for Hew tuple type (numeric field access: t.0, t.1)
    if (auto hewTuple = mlir::dyn_cast<hew::HewTupleType>(operandType)) {
      char *end = nullptr;
      unsigned long numericIdx = std::strtoul(fieldName.c_str(), &end, 10);
      bool isNumericField = (end != fieldName.c_str() && *end == '\0');
      if (!isNumericField) {
        emitError(location) << "named field access on tuple type";
        return nullptr;
      }
      auto elemTypes = hewTuple.getElementTypes();
      if (numericIdx >= elemTypes.size()) {
        emitError(location) << "tuple index " << numericIdx << " out of bounds (size "
                            << elemTypes.size() << ")";
        return nullptr;
      }
      return hew::TupleExtractOp::create(builder, location, elemTypes[numericIdx], operandVal,
                                         numericIdx);
    }

    auto structType = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(operandType);
    if (!structType) {
      emitError(location) << "field access on non-struct type";
      return nullptr;
    }

    // Check for numeric field name (tuple element access: t.0, t.1, etc.)
    char *end = nullptr;
    unsigned long numericIdx = std::strtoul(fieldName.c_str(), &end, 10);
    bool isNumericField = (end != fieldName.c_str() && *end == '\0');

    if (isNumericField) {
      // Direct numeric index into struct/tuple
      auto bodyTypes = structType.getBody();
      if (numericIdx >= bodyTypes.size()) {
        emitError(location) << "tuple index " << numericIdx << " out of bounds (size "
                            << bodyTypes.size() << ")";
        return nullptr;
      }
      return mlir::LLVM::ExtractValueOp::create(builder, location, operandVal, numericIdx);
    }

    // Named field: look up struct info by type name
    // Special case: event types in machine transitions use anonymous structs.
    // Try enum-based resolution first for event field access.
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
      emitError(location) << "named field access on anonymous struct type";
      return nullptr;
    }
    llvm::StringRef structName = structType.getName();
    auto it = structTypes.find(structName.str());
    if (it != structTypes.end()) {
      const auto &info = it->second;

      // Find the field
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

      {
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
      }
      return nullptr;
    }

    // Machine/enum field access: resolve `state.field` and `event.field` inside transition bodies
    std::string lookupName = structName.str();
    // Event types use anonymous LLVM structs — look up by registered event type name instead
    if (lookupName.empty() && !currentMachineEventTypeName_.empty()) {
      lookupName = currentMachineEventTypeName_;
    }
    auto enumIt = enumTypes.find(lookupName);
    if (enumIt != enumTypes.end() && !currentMachineSourceVariant_.empty()) {
      const auto &enumInfo = enumIt->second;
      // Determine which variant to resolve: for event types use the event variant,
      // otherwise use the source state variant (for `state.field`).
      const std::string &variantName = (!currentMachineEventTypeName_.empty() &&
                                        structName.str() == currentMachineEventTypeName_)
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

    emitError(location) << "unknown struct type '" << structName << "'";
    return nullptr;
  }

  if (auto *idx = std::get_if<ast::ExprIndex>(&expr.kind)) {
    auto location = currentLoc;
    auto operandVal = generateExpression(idx->object->value);
    if (!operandVal)
      return nullptr;

    // Hew array type indexing
    if (auto hewArrayType = mlir::dyn_cast<hew::HewArrayType>(operandVal.getType())) {
      auto indexVal = generateExpression(idx->index->value);
      if (!indexVal)
        return nullptr;

      // Constant index -> hew.array_extract
      if (auto constOp = indexVal.getDefiningOp<mlir::arith::ConstantIntOp>()) {
        auto idxConst = constOp.value();
        return hew::ArrayExtractOp::create(builder, location, hewArrayType.getElementType(),
                                           operandVal, idxConst);
      }

      // Dynamic index -> cast to LLVM array, spill to alloca, GEP, load
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
      auto indexVal = generateExpression(idx->index->value);
      if (!indexVal)
        return nullptr;

      // Constant index -> extractvalue
      if (auto constOp = indexVal.getDefiningOp<mlir::arith::ConstantIntOp>()) {
        auto idxConst = constOp.value();
        return mlir::LLVM::ExtractValueOp::create(builder, location, operandVal, idxConst);
      }

      // Dynamic index -> spill array to alloca, GEP, load
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
      auto indexVal = generateExpression(idx->index->value);
      if (!indexVal)
        return nullptr;
      auto i64Type = builder.getI64Type();
      mlir::Value idx64 = indexVal;
      if (indexVal.getType() != i64Type)
        idx64 = mlir::arith::ExtSIOp::create(builder, location, i64Type, indexVal);
      return hew::VecGetOp::create(builder, location, vecType.getElementType(), operandVal, idx64);
    }

    // Custom type indexing: desugar obj[key] → obj.get(key)
    if (auto structType = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(operandVal.getType())) {
      if (structType.isIdentified()) {
        auto indexVal = generateExpression(idx->index->value);
        if (!indexVal)
          return nullptr;

        std::string funcName = mangleName(currentModulePath, structType.getName().str(), "get");
        llvm::SmallVector<mlir::Value, 2> args;
        args.push_back(operandVal);
        args.push_back(indexVal);

        auto callee = module.lookupSymbol<mlir::func::FuncOp>(funcName);
        if (!callee)
          callee = lookupImportedFunc(structType.getName(), "get");
        if (callee) {
          auto funcType = callee.getFunctionType();
          for (size_t i = 0; i < args.size() && i < funcType.getNumInputs(); ++i) {
            if (args[i].getType() != funcType.getInput(i))
              args[i] = coerceType(args[i], funcType.getInput(i), location);
          }
          auto callOp = mlir::func::CallOp::create(builder, location, callee, args);
          if (callOp.getNumResults() > 0)
            return callOp.getResult(0);
          return nullptr;
        }
      }
    }

    emitError(location) << "indexing not supported for this type";
    return nullptr;
  }

  if (auto *awaitE = std::get_if<ast::ExprAwait>(&expr.kind)) {
    auto location = currentLoc;
    // Check if the inner expression is a method call (actor ask pattern)
    if (auto *mc = std::get_if<ast::ExprMethodCall>(&awaitE->inner->value.kind)) {
      auto mcLocation = loc(awaitE->inner->span);

      // Generate receiver
      auto receiver = generateExpression(mc->receiver->value);
      if (!receiver)
        return nullptr;

      // Resolve actor type
      std::string actorTypeName = resolveActorTypeName(mc->receiver->value, &mc->receiver->span);

      if (!actorTypeName.empty()) {
        auto actorIt = actorRegistry.find(actorTypeName);
        if (actorIt != actorRegistry.end()) {
          return generateActorMethodAsk(receiver, actorIt->second, mc->method, mc->args,
                                        mcLocation);
        }
      }

      emitError(location) << "await requires an actor method call";
      return nullptr;
    }
    // Not a method call — operand might be an actor ref or a task handle.
    auto operand = generateExpression(awaitE->inner->value);
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

      mlir::Type resultType = builder.getI32Type();
      bool resolvedScopeAwaitType = false;
      if (auto *ie = std::get_if<ast::ExprIdentifier>(&awaitE->inner->value.kind)) {
        auto it = taskResultTypes.find(ie->name);
        if (it != taskResultTypes.end()) {
          resultType = it->second;
          resolvedScopeAwaitType = true;
        }
      }
      if (!resolvedScopeAwaitType) {
        emitError(location) << "cannot determine scope.await result type";
        return nullptr;
      }

      auto loadedResult = mlir::LLVM::LoadOp::create(builder, location, resultType, resultPtr);
      return loadedResult;
    }
    return operand;
  }

  if (auto *range = std::get_if<ast::ExprRange>(&expr.kind)) {
    // Range expression: start..end or start..=end
    if (!range->start || !range->end) {
      emitError(currentLoc) << "unbounded ranges not yet supported as values";
      return nullptr;
    }

    auto startVal = generateExpression((*range->start)->value);
    auto endVal = generateExpression((*range->end)->value);

    if (!startVal || !endVal)
      return nullptr;

    // Ensure types match
    if (startVal.getType() != endVal.getType()) {
      endVal = coerceType(endVal, startVal.getType(), currentLoc);
    }

    if (range->inclusive) {
      // For inclusive range ..=, add 1 to end value (assuming integer)
      if (endVal.getType().isIntOrIndex()) {
        auto one = createIntConstant(builder, currentLoc, endVal.getType(), 1);
        endVal = mlir::arith::AddIOp::create(builder, currentLoc, endVal, one);
      } else {
        emitError(currentLoc) << "inclusive range only supported for integers";
        return nullptr;
      }
    }

    auto tupleType = hew::HewTupleType::get(&context, {startVal.getType(), endVal.getType()});
    return hew::TupleCreateOp::create(builder, currentLoc, tupleType,
                                      mlir::ValueRange{startVal, endVal});
  }

  emitError(currentLoc) << "unsupported expression kind";
  return nullptr;
}

// ============================================================================
// Interpolated string generation
// ============================================================================

mlir::Value MLIRGen::generateInterpolatedString(const ast::ExprInterpolatedString &interp) {
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto strRefType = hew::StringRefType::get(&context);

  // Build a string value for each part, tracking heap-allocated intermediates.
  std::vector<mlir::Value> partValues;
  std::vector<mlir::Value> ownedTemps; // ToStringOp results we must free
  for (const auto &part : interp.parts) {
    if (auto *litPart = std::get_if<ast::StringPartLiteral>(&part)) {
      // Literal text segment — pointer into .rodata, no cleanup needed.
      const auto &text = litPart->text;
      if (!text.empty()) {
        auto symName = getOrCreateGlobalString(text);
        partValues.push_back(
            hew::ConstantOp::create(builder, location, strRefType, builder.getStringAttr(symName)));
      }
    } else if (auto *exprPart = std::get_if<ast::StringPartExpr>(&part)) {
      if (exprPart->expr) {
        // Expression segment — generate code and convert to string
        mlir::Value val = generateExpression(exprPart->expr->value);
        if (!val)
          continue;

        auto valType = val.getType();
        if (valType == ptrType || mlir::isa<hew::StringRefType>(valType)) {
          // Already a string — use directly
          partValues.push_back(val);
        } else if (valType.isIntOrFloat() || valType.isInteger(1)) {
          auto str = hew::ToStringOp::create(builder, location, strRefType, val);
          if (auto *typeExpr = resolvedTypeOf(exprPart->expr->span))
            if (isUnsignedTypeExpr(*typeExpr))
              str->setAttr("is_unsigned", builder.getBoolAttr(true));
          partValues.push_back(str);
          ownedTemps.push_back(str); // heap-allocated — we own this
        } else {
          emitError(location) << "unsupported type in string interpolation";
          return nullptr;
        }
      }
    }
  }

  // Empty interpolation -> return empty string
  if (partValues.empty()) {
    auto symName = getOrCreateGlobalString("");
    return hew::ConstantOp::create(builder, location, strRefType, builder.getStringAttr(symName));
  }

  // Single-part interpolations built from an expression still need to produce
  // an owned string. Returning the inner value directly aliases loop/variable
  // storage (e.g. `f"{r}"` in a `for await` body), and the original owner may
  // drop it before the interpolation result escapes. Route expression-only
  // singletons through concat-with-empty to force a fresh string allocation.
  if (partValues.size() == 1) {
    auto result = partValues[0];
    if (interp.parts.size() == 1 && std::holds_alternative<ast::StringPartExpr>(interp.parts[0])) {
      auto emptySym = getOrCreateGlobalString("");
      auto emptyStr =
          hew::ConstantOp::create(builder, location, strRefType, builder.getStringAttr(emptySym));
      result = hew::StringConcatOp::create(builder, location, strRefType, emptyStr, result);
    } else {
      for (auto it = ownedTemps.begin(); it != ownedTemps.end(); ++it) {
        if (*it == result) {
          ownedTemps.erase(it);
          break;
        }
      }
    }
    for (auto temp : ownedTemps)
      emitStringDrop(temp);
    return result;
  }

  // Concatenate all parts left-to-right
  mlir::Value result = partValues[0];
  for (size_t j = 1; j < partValues.size(); ++j) {
    mlir::Value prevResult = result;
    result = hew::StringConcatOp::create(builder, location, strRefType, result, partValues[j]);
    if (j > 1)
      ownedTemps.push_back(prevResult);
  }

  // Drop all intermediates
  for (auto temp : ownedTemps)
    emitStringDrop(temp);

  return result;
}

// ============================================================================
// Regex literal expression generation
// ============================================================================

mlir::Value MLIRGen::generateBytesLiteral(const std::vector<uint8_t> &data) {
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto i32Ty = builder.getI32Type();

  // Store byte data as a global string constant (reusing string infrastructure).
  std::string dataStr(data.begin(), data.end());
  auto symName = getOrCreateGlobalString(dataStr);

  // Get pointer to the global string data.
  auto dataPtr =
      hew::ConstantOp::create(builder, location, ptrType, builder.getStringAttr(symName));

  // Create length constant.
  auto lenVal = createIntConstant(builder, location, i32Ty, static_cast<int64_t>(data.size()));

  // Call hew_vec_from_u8_data(ptr, len) -> *HewVec
  auto funcType = mlir::FunctionType::get(&context, {ptrType, i32Ty}, {ptrType});
  auto func = getOrCreateExternFunc("hew_vec_from_u8_data", funcType);
  auto call =
      mlir::func::CallOp::create(builder, location, func, mlir::ValueRange{dataPtr, lenVal});
  return call.getResult(0);
}

mlir::Value MLIRGen::generateRegexLiteral(const ast::ExprRegexLiteral &regex) {
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto handleType = hew::HandleType::get(&context, "regex.Pattern");

  auto symName = getOrCreateGlobalString(regex.pattern);
  auto patternStr =
      hew::ConstantOp::create(builder, location, ptrType, builder.getStringAttr(symName));

  return hew::RegexNewOp::create(builder, location, handleType, patternStr);
}

// ============================================================================
// Literal generation
// ============================================================================

mlir::Value MLIRGen::generateLiteral(const ast::Literal &lit, const ast::Span &span) {
  auto location = currentLoc;

  if (auto *intLit = std::get_if<ast::LitInteger>(&lit)) {
    // Use the type checker's resolved type if available (for literal coercion)
    auto type = defaultIntType();
    if (auto *resolved = resolvedTypeOf(span)) {
      auto converted = convertType(*resolved);
      if (converted &&
          (llvm::isa<mlir::IntegerType>(converted) || llvm::isa<mlir::FloatType>(converted)))
        type = converted;
    }
    if (llvm::isa<mlir::FloatType>(type)) {
      return mlir::arith::ConstantOp::create(
          builder, location, builder.getFloatAttr(type, static_cast<double>(intLit->value)));
    }
    return createIntConstant(builder, location, type, intLit->value);
  }
  if (auto *floatLit = std::get_if<ast::LitFloat>(&lit)) {
    // Use the type checker's resolved type if available (for literal coercion)
    auto type = defaultFloatType();
    if (auto *resolved = resolvedTypeOf(span)) {
      auto converted = convertType(*resolved);
      if (converted && llvm::isa<mlir::FloatType>(converted))
        type = converted;
    }
    return mlir::arith::ConstantOp::create(builder, location,
                                           builder.getFloatAttr(type, floatLit->value));
  }
  if (auto *boolLit = std::get_if<ast::LitBool>(&lit)) {
    auto type = builder.getI1Type();
    return createIntConstant(builder, location, type, boolLit->value ? 1 : 0);
  }
  if (auto *strLit = std::get_if<ast::LitString>(&lit)) {
    auto symName = getOrCreateGlobalString(strLit->value);
    return hew::ConstantOp::create(builder, location, hew::StringRefType::get(&context),
                                   builder.getStringAttr(symName));
  }
  if (auto *charLit = std::get_if<ast::LitChar>(&lit)) {
    return mlir::arith::ConstantIntOp::create(builder, location, builder.getI32Type(),
                                              static_cast<int64_t>(charLit->value));
  }
  if (auto *durLit = std::get_if<ast::LitDuration>(&lit)) {
    auto type = defaultIntType();
    return createIntConstant(builder, location, type, durLit->value);
  }
  emitError(location) << "unsupported literal kind";
  return nullptr;
}

// ============================================================================
// Binary expression generation
// ============================================================================

mlir::Value MLIRGen::generateBinaryExpr(const ast::ExprBinary &expr) {
  auto location = currentLoc;

  // Handle short-circuit operators specially
  if (expr.op == ast::BinaryOp::And) {
    auto lhs = generateExpression(expr.left->value);
    if (!lhs)
      return nullptr;

    auto i1Type = builder.getI1Type();
    auto ifOp = mlir::scf::IfOp::create(builder, location, i1Type, lhs,
                                        /*withElseRegion=*/true);

    builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
    auto rhs = generateExpression(expr.right->value);
    if (!rhs)
      rhs = createIntConstant(builder, location, i1Type, 0);
    mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{rhs});

    builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
    auto falseVal = createIntConstant(builder, location, i1Type, 0);
    mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{falseVal});

    builder.setInsertionPointAfter(ifOp);
    return ifOp.getResult(0);
  }

  if (expr.op == ast::BinaryOp::Or) {
    auto lhs = generateExpression(expr.left->value);
    if (!lhs)
      return nullptr;

    auto i1Type = builder.getI1Type();
    auto ifOp = mlir::scf::IfOp::create(builder, location, i1Type, lhs,
                                        /*withElseRegion=*/true);

    builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
    auto trueVal = createIntConstant(builder, location, i1Type, 1);
    mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{trueVal});

    builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
    auto rhs = generateExpression(expr.right->value);
    if (!rhs)
      rhs = createIntConstant(builder, location, i1Type, 0);
    mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{rhs});

    builder.setInsertionPointAfter(ifOp);
    return ifOp.getResult(0);
  }

  // Normal binary ops: evaluate both sides
  auto lhs = generateExpression(expr.left->value);
  auto rhs = generateExpression(expr.right->value);
  if (!lhs || !rhs)
    return nullptr;

  // Type coercion: if one side is float and the other is int, promote int
  bool lhsIsFloat = llvm::isa<mlir::FloatType>(lhs.getType());
  bool rhsIsFloat = llvm::isa<mlir::FloatType>(rhs.getType());
  if (lhsIsFloat && !rhsIsFloat) {
    bool rhsUns = false;
    if (auto *rt = resolvedTypeOf(expr.right->span))
      rhsUns = isUnsignedTypeExpr(*rt);
    rhs = coerceType(rhs, lhs.getType(), location, rhsUns);
  } else if (rhsIsFloat && !lhsIsFloat) {
    bool lhsUns = false;
    if (auto *lt = resolvedTypeOf(expr.left->span))
      lhsUns = isUnsignedTypeExpr(*lt);
    lhs = coerceType(lhs, rhs.getType(), location, lhsUns);
  }

  // Integer width promotion
  auto lhsInt = mlir::dyn_cast<mlir::IntegerType>(lhs.getType());
  auto rhsInt = mlir::dyn_cast<mlir::IntegerType>(rhs.getType());
  if (lhsInt && rhsInt && lhsInt.getWidth() != rhsInt.getWidth()) {
    if (lhsInt.getWidth() < rhsInt.getWidth()) {
      bool lhsUnsigned = false;
      if (auto *lt = resolvedTypeOf(expr.left->span))
        lhsUnsigned = isUnsignedTypeExpr(*lt);
      lhs = lhsUnsigned
                ? mlir::arith::ExtUIOp::create(builder, location, rhs.getType(), lhs).getResult()
                : mlir::arith::ExtSIOp::create(builder, location, rhs.getType(), lhs).getResult();
    } else {
      bool rhsUnsigned = false;
      if (auto *rt = resolvedTypeOf(expr.right->span))
        rhsUnsigned = isUnsignedTypeExpr(*rt);
      rhs = rhsUnsigned
                ? mlir::arith::ExtUIOp::create(builder, location, lhs.getType(), rhs).getResult()
                : mlir::arith::ExtSIOp::create(builder, location, lhs.getType(), rhs).getResult();
    }
  }

  auto type = lhs.getType();
  bool isFloat = llvm::isa<mlir::FloatType>(type);
  bool isPtr = isPointerLikeType(type);
  // Derive signedness from the type checker's resolved type for the LHS operand.
  // MLIR IntegerTypes are signless, so intType.isUnsigned() always returns false;
  // we must consult the source type name instead.
  bool isUnsigned = false;
  if (mlir::isa<mlir::IntegerType>(type)) {
    if (auto *lhsType = resolvedTypeOf(expr.left->span))
      isUnsigned = isUnsignedTypeExpr(*lhsType);
  }

  // Hoist actor-pointer detection for pointer comparison operators.
  bool isActorPtr = false;
  if (isPtr) {
    if (auto *ie = std::get_if<ast::ExprIdentifier>(&expr.left->value.kind))
      if (actorVarTypes.count(ie->name))
        isActorPtr = true;
    if (auto *ie = std::get_if<ast::ExprIdentifier>(&expr.right->value.kind))
      if (actorVarTypes.count(ie->name))
        isActorPtr = true;
  }

  // Helper: string ordering comparison via compare() method.
  auto ptrOrderingCmp = [&](mlir::arith::CmpIPredicate pred) -> mlir::Value {
    if (isActorPtr) {
      emitError(location, "ordering comparison on actor references is not supported");
      return nullptr;
    }
    materializeTemporary(lhs, expr.left->value);
    materializeTemporary(rhs, expr.right->value);
    auto cmpResult =
        hew::StringMethodOp::create(builder, location, builder.getI32Type(),
                                    builder.getStringAttr("compare"), lhs, mlir::ValueRange{rhs});
    auto zero = createIntConstant(builder, location, builder.getI32Type(), 0);
    return mlir::arith::CmpIOp::create(builder, location, pred, cmpResult.getResult(), zero)
        .getResult();
  };

  // Helper: pointer equality/inequality via PtrToInt (actors) or equals() (strings).
  auto ptrEqualityCmp = [&](mlir::arith::CmpIPredicate pred) -> mlir::Value {
    if (isActorPtr) {
      auto i64Type = builder.getI64Type();
      auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
      mlir::Value lhsPtr = lhs, rhsPtr = rhs;
      if (!mlir::isa<mlir::LLVM::LLVMPointerType>(lhs.getType()))
        lhsPtr = hew::BitcastOp::create(builder, location, ptrType, lhs);
      if (!mlir::isa<mlir::LLVM::LLVMPointerType>(rhs.getType()))
        rhsPtr = hew::BitcastOp::create(builder, location, ptrType, rhs);
      auto lhsI = mlir::LLVM::PtrToIntOp::create(builder, location, i64Type, lhsPtr);
      auto rhsI = mlir::LLVM::PtrToIntOp::create(builder, location, i64Type, rhsPtr);
      return mlir::arith::CmpIOp::create(builder, location, pred, lhsI, rhsI).getResult();
    }
    // String equality: equals() returns non-zero on match.
    // Materialize string temporaries (e.g. v.get(i)) so they are freed
    // at scope exit.  Without this, strdup'd copies from method calls
    // leak through comparison expressions.
    materializeTemporary(lhs, expr.left->value);
    materializeTemporary(rhs, expr.right->value);
    auto eqResult =
        hew::StringMethodOp::create(builder, location, builder.getI32Type(),
                                    builder.getStringAttr("equals"), lhs, mlir::ValueRange{rhs});
    auto zero = createIntConstant(builder, location, builder.getI32Type(), 0);
    // For ==, check equals() != 0; for !=, check equals() == 0.
    auto stringPred = (pred == mlir::arith::CmpIPredicate::eq) ? mlir::arith::CmpIPredicate::ne
                                                               : mlir::arith::CmpIPredicate::eq;
    return mlir::arith::CmpIOp::create(builder, location, stringPred, eqResult.getResult(), zero)
        .getResult();
  };

  switch (expr.op) {
  // Arithmetic
  case ast::BinaryOp::Add:
    if (isPtr) {
      // Materialize heap-allocated intermediates (previous ConcatOp results,
      // ToStringOp results) so they are freed at scope exit.  Constants,
      // variable loads, and identifiers are skipped by inferDropFuncForTemporary.
      materializeTemporary(lhs, expr.left->value);
      materializeTemporary(rhs, expr.right->value);
      return hew::StringConcatOp::create(builder, location, hew::StringRefType::get(&context), lhs,
                                         rhs)
          .getResult();
    }
    return isFloat ? mlir::arith::AddFOp::create(builder, location, lhs, rhs).getResult()
                   : mlir::arith::AddIOp::create(builder, location, lhs, rhs).getResult();
  case ast::BinaryOp::Subtract:
    return isFloat ? mlir::arith::SubFOp::create(builder, location, lhs, rhs).getResult()
                   : mlir::arith::SubIOp::create(builder, location, lhs, rhs).getResult();
  case ast::BinaryOp::Multiply:
    return isFloat ? mlir::arith::MulFOp::create(builder, location, lhs, rhs).getResult()
                   : mlir::arith::MulIOp::create(builder, location, lhs, rhs).getResult();
  case ast::BinaryOp::Divide:
    if (isFloat)
      return mlir::arith::DivFOp::create(builder, location, lhs, rhs).getResult();
    if (isUnsigned)
      return mlir::arith::DivUIOp::create(builder, location, lhs, rhs).getResult();
    return mlir::arith::DivSIOp::create(builder, location, lhs, rhs).getResult();
  case ast::BinaryOp::Modulo:
    if (isFloat)
      return mlir::arith::RemFOp::create(builder, location, lhs, rhs).getResult();
    if (isUnsigned)
      return mlir::arith::RemUIOp::create(builder, location, lhs, rhs).getResult();
    return mlir::arith::RemSIOp::create(builder, location, lhs, rhs).getResult();

  // Comparisons
  case ast::BinaryOp::Less:
    if (isFloat)
      return mlir::arith::CmpFOp::create(builder, location, mlir::arith::CmpFPredicate::OLT, lhs,
                                         rhs)
          .getResult();
    if (isPtr)
      return ptrOrderingCmp(mlir::arith::CmpIPredicate::slt);
    return mlir::arith::CmpIOp::create(builder, location,
                                       isUnsigned ? mlir::arith::CmpIPredicate::ult
                                                  : mlir::arith::CmpIPredicate::slt,
                                       lhs, rhs)
        .getResult();
  case ast::BinaryOp::LessEqual:
    if (isFloat)
      return mlir::arith::CmpFOp::create(builder, location, mlir::arith::CmpFPredicate::OLE, lhs,
                                         rhs)
          .getResult();
    if (isPtr)
      return ptrOrderingCmp(mlir::arith::CmpIPredicate::sle);
    return mlir::arith::CmpIOp::create(builder, location,
                                       isUnsigned ? mlir::arith::CmpIPredicate::ule
                                                  : mlir::arith::CmpIPredicate::sle,
                                       lhs, rhs)
        .getResult();
  case ast::BinaryOp::Greater:
    if (isFloat)
      return mlir::arith::CmpFOp::create(builder, location, mlir::arith::CmpFPredicate::OGT, lhs,
                                         rhs)
          .getResult();
    if (isPtr)
      return ptrOrderingCmp(mlir::arith::CmpIPredicate::sgt);
    return mlir::arith::CmpIOp::create(builder, location,
                                       isUnsigned ? mlir::arith::CmpIPredicate::ugt
                                                  : mlir::arith::CmpIPredicate::sgt,
                                       lhs, rhs)
        .getResult();
  case ast::BinaryOp::GreaterEqual:
    if (isFloat)
      return mlir::arith::CmpFOp::create(builder, location, mlir::arith::CmpFPredicate::OGE, lhs,
                                         rhs)
          .getResult();
    if (isPtr)
      return ptrOrderingCmp(mlir::arith::CmpIPredicate::sge);
    return mlir::arith::CmpIOp::create(builder, location,
                                       isUnsigned ? mlir::arith::CmpIPredicate::uge
                                                  : mlir::arith::CmpIPredicate::sge,
                                       lhs, rhs)
        .getResult();
  case ast::BinaryOp::Equal:
    if (isFloat)
      return mlir::arith::CmpFOp::create(builder, location, mlir::arith::CmpFPredicate::OEQ, lhs,
                                         rhs)
          .getResult();
    if (isPtr)
      return ptrEqualityCmp(mlir::arith::CmpIPredicate::eq);
    return mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::eq, lhs, rhs)
        .getResult();
  case ast::BinaryOp::NotEqual:
    if (isFloat)
      return mlir::arith::CmpFOp::create(builder, location, mlir::arith::CmpFPredicate::ONE, lhs,
                                         rhs)
          .getResult();
    if (isPtr)
      return ptrEqualityCmp(mlir::arith::CmpIPredicate::ne);
    return mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::ne, lhs, rhs)
        .getResult();

  case ast::BinaryOp::RegexMatch: {
    return hew::RegexIsMatchOp::create(builder, location, builder.getI1Type(), rhs, lhs);
  }
  case ast::BinaryOp::RegexNotMatch: {
    auto matchResult =
        hew::RegexIsMatchOp::create(builder, location, builder.getI1Type(), rhs, lhs);
    auto trueVal = mlir::arith::ConstantOp::create(builder, location, builder.getBoolAttr(true));
    return mlir::arith::XOrIOp::create(builder, location, matchResult, trueVal).getResult();
  }

  // Bitwise operators
  case ast::BinaryOp::BitAnd:
    return mlir::arith::AndIOp::create(builder, location, lhs, rhs).getResult();
  case ast::BinaryOp::BitOr:
    return mlir::arith::OrIOp::create(builder, location, lhs, rhs).getResult();
  case ast::BinaryOp::BitXor:
    return mlir::arith::XOrIOp::create(builder, location, lhs, rhs).getResult();
  case ast::BinaryOp::Shl:
    return mlir::arith::ShLIOp::create(builder, location, lhs, rhs).getResult();
  case ast::BinaryOp::Shr:
    return isUnsigned ? mlir::arith::ShRUIOp::create(builder, location, lhs, rhs).getResult()
                      : mlir::arith::ShRSIOp::create(builder, location, lhs, rhs).getResult();

  case ast::BinaryOp::Range: {
    // Treat as range expression: start..end
    auto tupleType = hew::HewTupleType::get(&context, {lhs.getType(), rhs.getType()});
    return hew::TupleCreateOp::create(builder, location, tupleType, mlir::ValueRange{lhs, rhs});
  }

  case ast::BinaryOp::RangeInclusive: {
    // Treat as range expression: start..=end -> (start, end+1)
    if (rhs.getType().isIntOrIndex()) {
      auto one = createIntConstant(builder, location, rhs.getType(), 1);
      rhs = mlir::arith::AddIOp::create(builder, location, rhs, one);
    } else {
      emitError(location) << "inclusive range only supported for integers";
      return nullptr;
    }
    auto tupleType = hew::HewTupleType::get(&context, {lhs.getType(), rhs.getType()});
    return hew::TupleCreateOp::create(builder, location, tupleType, mlir::ValueRange{lhs, rhs});
  }

  case ast::BinaryOp::Send:
    hew::ActorSendOp::create(builder, location, lhs, builder.getI32IntegerAttr(0),
                             mlir::ValueRange{rhs});
    return nullptr;

  default:
    emitError(location) << "unsupported binary operator";
    return nullptr;
  }
}

// ============================================================================
// Unary expression generation
// ============================================================================

mlir::Value MLIRGen::generateUnaryExpr(const ast::ExprUnary &expr) {
  auto location = currentLoc;

  auto operand = generateExpression(expr.operand->value);
  if (!operand)
    return nullptr;

  switch (expr.op) {
  case ast::UnaryOp::Negate: {
    auto type = operand.getType();
    if (llvm::isa<mlir::FloatType>(type)) {
      return mlir::arith::NegFOp::create(builder, location, operand).getResult();
    }
    // Integer negate: 0 - operand
    auto zero = createIntConstant(builder, location, type, 0);
    return mlir::arith::SubIOp::create(builder, location, zero, operand).getResult();
  }
  case ast::UnaryOp::Not: {
    auto type = operand.getType();
    if (type == builder.getI1Type()) {
      auto trueVal = createIntConstant(builder, location, type, 1);
      return mlir::arith::XOrIOp::create(builder, location, operand, trueVal).getResult();
    }
    auto zero = createIntConstant(builder, location, type, 0);
    return mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::eq, operand,
                                       zero)
        .getResult();
  }
  case ast::UnaryOp::BitNot: {
    auto type = operand.getType();
    auto allOnes = createIntConstant(builder, location, type, -1);
    return mlir::arith::XOrIOp::create(builder, location, operand, allOnes).getResult();
  }
  }
  return nullptr;
}

// ============================================================================
// Call expression generation
// ============================================================================

mlir::Value MLIRGen::emitOptionWrap(mlir::Value condition, mlir::Value payload,
                                    mlir::Type optionType, mlir::Location location) {
  auto ifOp = mlir::scf::IfOp::create(builder, location, optionType, condition,
                                      /*withElseRegion=*/true);
  builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
  auto someVal = hew::EnumConstructOp::create(
      builder, location, optionType, static_cast<uint32_t>(1), llvm::StringRef("Option"),
      mlir::ValueRange{payload}, /*payload_positions=*/builder.getI64ArrayAttr({1}));
  mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{someVal});
  builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
  auto noneVal = hew::EnumConstructOp::create(
      builder, location, optionType, static_cast<uint32_t>(0), llvm::StringRef("Option"),
      mlir::ValueRange{}, /*payload_positions=*/mlir::ArrayAttr{});
  mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{noneVal});
  builder.setInsertionPointAfter(ifOp);
  return ifOp.getResult(0);
}

mlir::Value MLIRGen::generateCallExpr(const ast::ExprCall &call) {
  auto location = currentLoc;

  // Check if the callee is a simple identifier (direct call)
  auto *calleeIdentExpr = std::get_if<ast::ExprIdentifier>(&call.function->value.kind);
  if (!calleeIdentExpr) {
    emitError(location) << "only direct function calls supported";
    return nullptr;
  }

  const auto &calleeName = calleeIdentExpr->name;

  // ── Intercept enriched log calls ─────────────────────────────────────
  // The enrich.rs step rewrites log.setup()/log.info()/etc. into direct
  // calls to hew_log_init/hew_log_info/etc. before codegen sees them.
  // We intercept those here and generate the new level-guarded hew_log_emit.
  if (calleeName == "hew_log_init") {
    // log.setup() → hew_log_set_level(2) (default INFO)
    auto i32Type = builder.getI32Type();
    auto infoLevel = mlir::arith::ConstantIntOp::create(builder, location, 2, 32);
    auto funcType = mlir::FunctionType::get(&context, {i32Type}, {});
    getOrCreateExternFunc("hew_log_set_level", funcType);
    hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                               mlir::SymbolRefAttr::get(&context, "hew_log_set_level"),
                               mlir::ValueRange{infoLevel});
    return nullptr;
  }
  {
    int logLevel = -1;
    if (calleeName == "hew_log_error")
      logLevel = 0;
    else if (calleeName == "hew_log_warn")
      logLevel = 1;
    else if (calleeName == "hew_log_info")
      logLevel = 2;
    else if (calleeName == "hew_log_debug")
      logLevel = 3;
    else if (calleeName == "hew_log_trace")
      logLevel = 4;
    if (logLevel >= 0)
      return generateLogEmit(call.args, logLevel);
  }

  // ── Intercept channel try_recv → Option<T> wrapping ──────────────────
  // The enricher rewrites rx.try_recv() into hew_channel_try_recv(rx) or
  // hew_channel_try_recv_int(rx). The runtime returns NULL (string) or
  // uses an out_valid flag (int) to indicate "no message". We wrap the
  // raw result in Option<T> here.
  if (calleeName == "hew_channel_try_recv") {
    // String variant: hew_channel_try_recv(rx) → ptr (NULL = empty).
    if (call.args.size() != 1) {
      emitError(location) << "hew_channel_try_recv expects 1 argument";
      return nullptr;
    }
    auto rxArg = generateExpression(ast::callArgExpr(call.args[0]).value);
    if (!rxArg)
      return nullptr;

    auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
    auto stringType = hew::StringRefType::get(&context);

    // Declare and call the extern function (returns ptr, NULL for empty).
    auto externFuncType = mlir::FunctionType::get(&context, {ptrType}, {ptrType});
    getOrCreateExternFunc("hew_channel_try_recv", externFuncType);
    auto calleeAttr = mlir::SymbolRefAttr::get(&context, "hew_channel_try_recv");
    auto rawPtr = hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{ptrType},
                                             calleeAttr, mlir::ValueRange{rxArg})
                      .getResult();

    // Build Option<String> via EnumConstructOp (matches HashMap.get() path).
    auto optionType = hew::OptionEnumType::get(&context, stringType);
    auto nullVal = mlir::LLVM::ZeroOp::create(builder, location, ptrType);
    auto isNotNull =
        mlir::LLVM::ICmpOp::create(builder, location, mlir::LLVM::ICmpPredicate::ne, rawPtr, nullVal);

    return emitOptionWrap(isNotNull, rawPtr, optionType, location);
  }

  if (calleeName == "hew_channel_try_recv_int") {
    // Int variant: hew_channel_try_recv_int(rx, &out_valid) → i64.
    // The enricher only passes 1 arg (rx); we synthesise the out_valid alloca.
    if (call.args.size() != 1) {
      emitError(location) << "hew_channel_try_recv_int expects 1 argument";
      return nullptr;
    }
    auto rxArg = generateExpression(ast::callArgExpr(call.args[0]).value);
    if (!rxArg)
      return nullptr;

    auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
    auto i32Type = builder.getI32Type();
    auto i64Type = builder.getI64Type();

    // Reuse a single out_valid alloca hoisted to the function entry block.
    if (!channelIntOutValidAlloca) {
      auto savedIP = builder.saveInsertionPoint();
      auto &entryBlock = currentFunction.front();
      builder.setInsertionPointToStart(&entryBlock);
      auto one = mlir::arith::ConstantIntOp::create(builder, builder.getUnknownLoc(), 1, 64);
      channelIntOutValidAlloca =
          mlir::LLVM::AllocaOp::create(builder, builder.getUnknownLoc(), ptrType, i32Type, one);
      builder.restoreInsertionPoint(savedIP);
    }

    // Declare the extern with the correct 2-arg ABI: (ptr, ptr) → i64.
    auto externFuncType = mlir::FunctionType::get(&context, {ptrType, ptrType}, {i64Type});
    getOrCreateExternFunc("hew_channel_try_recv_int", externFuncType);
    auto calleeAttr = mlir::SymbolRefAttr::get(&context, "hew_channel_try_recv_int");
    auto rawVal =
        hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{i64Type}, calleeAttr,
                                   mlir::ValueRange{rxArg, channelIntOutValidAlloca})
            .getResult();

    // Load the validity flag.
    auto validFlag = mlir::LLVM::LoadOp::create(builder, location, i32Type, channelIntOutValidAlloca);

    // Build Option<int> via EnumConstructOp (matches HashMap.get() path).
    auto optionType = hew::OptionEnumType::get(&context, i64Type);
    auto zero = mlir::arith::ConstantIntOp::create(builder, location, 0, 32);
    auto isValid = mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::ne,
                                               validFlag, zero);

    return emitOptionWrap(isValid, rawVal, optionType, location);
  }

  // ── Intercept channel recv → Option<T> wrapping ──────────────────────
  // Same as try_recv but for the blocking variant. The runtime returns
  // NULL (string) or uses out_valid (int) when the channel is closed.
  if (calleeName == "hew_channel_recv") {
    // String variant: hew_channel_recv(rx) → ptr (NULL = closed).
    if (call.args.size() != 1) {
      emitError(location) << "hew_channel_recv expects 1 argument";
      return nullptr;
    }
    auto rxArg = generateExpression(ast::callArgExpr(call.args[0]).value);
    if (!rxArg)
      return nullptr;

    auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
    auto stringType = hew::StringRefType::get(&context);

    auto externFuncType = mlir::FunctionType::get(&context, {ptrType}, {ptrType});
    getOrCreateExternFunc("hew_channel_recv", externFuncType);
    auto calleeAttr = mlir::SymbolRefAttr::get(&context, "hew_channel_recv");
    auto rawPtr = hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{ptrType},
                                             calleeAttr, mlir::ValueRange{rxArg})
                      .getResult();

    // Build Option<String> via EnumConstructOp (matches HashMap.get() path).
    auto optionType = hew::OptionEnumType::get(&context, stringType);
    auto nullVal = mlir::LLVM::ZeroOp::create(builder, location, ptrType);
    auto isNotNull =
        mlir::LLVM::ICmpOp::create(builder, location, mlir::LLVM::ICmpPredicate::ne, rawPtr, nullVal);

    return emitOptionWrap(isNotNull, rawPtr, optionType, location);
  }

  if (calleeName == "hew_channel_recv_int") {
    // Int variant: hew_channel_recv_int(rx, &out_valid) → i64.
    if (call.args.size() != 1) {
      emitError(location) << "hew_channel_recv_int expects 1 argument";
      return nullptr;
    }
    auto rxArg = generateExpression(ast::callArgExpr(call.args[0]).value);
    if (!rxArg)
      return nullptr;

    auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
    auto i32Type = builder.getI32Type();
    auto i64Type = builder.getI64Type();

    // Reuse a single out_valid alloca hoisted to the function entry block.
    if (!channelIntOutValidAlloca) {
      auto savedIP = builder.saveInsertionPoint();
      auto &entryBlock = currentFunction.front();
      builder.setInsertionPointToStart(&entryBlock);
      auto one = mlir::arith::ConstantIntOp::create(builder, builder.getUnknownLoc(), 1, 64);
      channelIntOutValidAlloca =
          mlir::LLVM::AllocaOp::create(builder, builder.getUnknownLoc(), ptrType, i32Type, one);
      builder.restoreInsertionPoint(savedIP);
    }

    auto externFuncType = mlir::FunctionType::get(&context, {ptrType, ptrType}, {i64Type});
    getOrCreateExternFunc("hew_channel_recv_int", externFuncType);
    auto calleeAttr = mlir::SymbolRefAttr::get(&context, "hew_channel_recv_int");
    auto rawVal =
        hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{i64Type}, calleeAttr,
                                   mlir::ValueRange{rxArg, channelIntOutValidAlloca})
            .getResult();

    auto validFlag = mlir::LLVM::LoadOp::create(builder, location, i32Type, channelIntOutValidAlloca);

    // Build Option<int> via EnumConstructOp (matches HashMap.get() path).
    auto optionType = hew::OptionEnumType::get(&context, i64Type);
    auto zero = mlir::arith::ConstantIntOp::create(builder, location, 0, 32);
    auto isValid = mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::ne,
                                               validFlag, zero);

    return emitOptionWrap(isValid, rawVal, optionType, location);
  }

  // Handle generic function calls with explicit type arguments
  if (call.type_args.has_value() && !call.type_args->empty()) {
    std::vector<std::string> typeArgNames;
    for (const auto &ta : *call.type_args)
      typeArgNames.push_back(resolveTypeArgMangledName(ta.value));

    // Named generic function (fn<T>(...) { ... }) path.
    auto genIt = genericFunctions.find(calleeName);
    if (genIt != genericFunctions.end()) {
      auto specializedFunc = specializeGenericFunction(calleeName, typeArgNames);
      if (!specializedFunc)
        return nullptr;
      llvm::SmallVector<mlir::Value, 4> args;
      for (const auto &arg : call.args) {
        auto val = generateExpression(ast::callArgExpr(arg).value);
        if (!val)
          return nullptr;
        args.push_back(val);
      }
      auto funcType = specializedFunc.getFunctionType();
      for (size_t i = 0; i < args.size() && i < funcType.getNumInputs(); ++i) {
        auto expectedType = funcType.getInput(i);
        if (args[i].getType() != expectedType) {
          args[i] = coerceType(args[i], expectedType, location);
        }
      }
      auto callOp = mlir::func::CallOp::create(builder, location, specializedFunc, args);
      if (call.is_tail_call)
        callOp->setAttr("hew.tail_call", builder.getUnitAttr());
      if (callOp.getNumResults() > 0)
        return callOp.getResult(0);
      return nullptr;
    }

    // Let-bound generic lambda (let r = <T>(...) => ...) path.
    auto lamIt = genericLambdas.find(calleeName);
    if (lamIt != genericLambdas.end()) {
      auto specializedFunc = specializeGenericLambda(calleeName, typeArgNames);
      if (!specializedFunc)
        return nullptr;
      llvm::SmallVector<mlir::Value, 4> args;
      for (const auto &arg : call.args) {
        auto val = generateExpression(ast::callArgExpr(arg).value);
        if (!val)
          return nullptr;
        args.push_back(val);
      }
      auto funcType = specializedFunc.getFunctionType();
      for (size_t i = 0; i < args.size() && i < funcType.getNumInputs(); ++i) {
        auto expectedType = funcType.getInput(i);
        if (args[i].getType() != expectedType) {
          args[i] = coerceType(args[i], expectedType, location);
        }
      }
      auto callOp = mlir::func::CallOp::create(builder, location, specializedFunc, args);
      if (call.is_tail_call)
        callOp->setAttr("hew.tail_call", builder.getUnitAttr());
      if (callOp.getNumResults() > 0)
        return callOp.getResult(0);
      return nullptr;
    }
  }

  // ── Enriched Stream<bytes>.next() → Option<bytes> ─────────────────────
  // The enricher rewrites `.next()` on `Stream<bytes>` to a direct call to
  // `hew_stream_next_bytes(stream)`.  The C function returns a raw ptr
  // (null=EOF, non-null=HewVec*); we wrap that into Option<bytes>.
  if (calleeName == "hew_stream_next_bytes") {
    if (call.args.size() != 1) {
      emitError(location) << "hew_stream_next_bytes expects exactly 1 argument";
      return nullptr;
    }
    auto streamVal = generateExpression(ast::callArgExpr(call.args[0]).value);
    if (!streamVal)
      return nullptr;

    auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
    auto bytesType = hew::VecType::get(&context, builder.getI32Type());
    auto optType = hew::OptionEnumType::get(&context, bytesType);

    auto externFuncType = mlir::FunctionType::get(&context, {ptrType}, {ptrType});
    getOrCreateExternFunc("hew_stream_next_bytes", externFuncType);

    auto calleeAttr = mlir::SymbolRefAttr::get(&context, "hew_stream_next_bytes");
    auto rawPtr =
        hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{ptrType}, calleeAttr,
                                   mlir::ValueRange{streamVal})
            .getResult();

    // Build Option<bytes>: null → None (tag 0), non-null → Some (tag 1).
    auto nullVal = mlir::LLVM::ZeroOp::create(builder, location, ptrType);
    auto isNotNull = mlir::LLVM::ICmpOp::create(builder, location,
                                                 mlir::LLVM::ICmpPredicate::ne, rawPtr, nullVal);
    auto oneTag = mlir::arith::ConstantIntOp::create(builder, location, 1, 32);
    auto zeroTag = mlir::arith::ConstantIntOp::create(builder, location, 0, 32);
    auto tag = mlir::arith::SelectOp::create(builder, location, isNotNull, oneTag, zeroTag);

    auto optLLVMType = mlir::LLVM::LLVMStructType::getLiteral(
        &context, {builder.getI32Type(), ptrType});
    auto undef = mlir::LLVM::UndefOp::create(builder, location, optLLVMType);
    auto withTag =
        mlir::LLVM::InsertValueOp::create(builder, location, undef, tag, llvm::ArrayRef<int64_t>{0});
    auto withPayload =
        mlir::LLVM::InsertValueOp::create(builder, location, withTag, rawPtr, llvm::ArrayRef<int64_t>{1});

    return hew::BitcastOp::create(builder, location, optType, withPayload).getResult();
  }

  // Handle built-in print/println
  if (calleeName == "println") {
    return generatePrintCall(call, /*newline=*/true);
  }
  if (calleeName == "print") {
    return generatePrintCall(call, /*newline=*/false);
  }

  // Check for named builtins (O(1) lookup before calling generateBuiltinCall).
  {
    static const llvm::StringSet<> builtinNames = {"println_str",
                                                   "print_str",
                                                   "println_int",
                                                   "print_int",
                                                   "println_f64",
                                                   "print_f64",
                                                   "println_bool",
                                                   "print_bool",
                                                   "sqrt",
                                                   "abs",
                                                   "min",
                                                   "max",
                                                   "string_concat",
                                                   "string_length",
                                                   "string_equals",
                                                   "sleep_ms",
                                                   "string_char_at",
                                                   "string_slice",
                                                   "read_file",
                                                   "string_find",
                                                   "string_contains",
                                                   "string_starts_with",
                                                   "string_ends_with",
                                                   "string_trim",
                                                   "string_replace",
                                                   "string_to_int",
                                                   "string_from_int",
                                                   "int_to_string",
                                                   "char_to_string",
                                                   "substring",
                                                   "stop",
                                                   "close",
                                                   "link",
                                                   "unlink",
                                                   "monitor",
                                                   "demonitor",
                                                   "supervisor_child",
                                                   "supervisor_stop",
                                                   "panic",
                                                   "assert",
                                                   "assert_eq",
                                                   "assert_ne",
                                                   "Vec::new",
                                                   "Vec::from",
                                                   "HashMap::new",
                                                   "HashSet::new",
                                                   "Rc::new",
                                                   "bytes::new",
                                                   "bytes::from",
                                                   "duration::from_nanos",
                                                   "Node::start",
                                                   "Node::shutdown",
                                                   "Node::connect",
                                                   "Node::set_transport",
                                                   "Node::register",
                                                   "Node::lookup",
                                                   "to_float"};
    if (builtinNames.contains(calleeName))
      return generateBuiltinCall(calleeName, call.args, location);
  }

  // Check if this is an enum variant constructor: Some(42), Ok(val), etc.
  {
    auto varIt = variantLookup.find(calleeName);
    if (varIt != variantLookup.end()) {
      const auto &enumName = varIt->second.first;
      auto variantIndex = static_cast<int64_t>(varIt->second.second);

      // Built-in Some(x)
      if (calleeName == "Some" && enumName == "__Option") {
        if (call.args.size() != 1) {
          emitError(location) << "Some() expects exactly one argument";
          return nullptr;
        }
        auto argVal = generateExpression(ast::callArgExpr(call.args[0]).value);
        if (!argVal)
          return nullptr;
        mlir::Type optType;
        if (pendingDeclaredType && mlir::isa<hew::OptionEnumType>(*pendingDeclaredType))
          optType = *pendingDeclaredType;
        else if (currentFunction && currentFunction.getResultTypes().size() == 1 &&
                 mlir::isa<hew::OptionEnumType>(currentFunction.getResultTypes()[0]))
          optType = currentFunction.getResultTypes()[0];
        if (!optType)
          optType = hew::OptionEnumType::get(&context, argVal.getType());
        if (auto optionType = mlir::dyn_cast<hew::OptionEnumType>(optType);
            optionType && argVal.getType() != optionType.getInnerType()) {
          argVal = coerceType(argVal, optionType.getInnerType(), location);
          if (!argVal)
            return nullptr;
        }
        mlir::Value result = hew::EnumConstructOp::create(
            builder, location, optType, static_cast<uint32_t>(variantIndex),
            llvm::StringRef("Option"), mlir::ValueRange{argVal},
            /*payload_positions=*/builder.getI64ArrayAttr({1}));
        return result;
      }

      // Built-in Ok(x)
      if (calleeName == "Ok" && enumName == "__Result") {
        if (call.args.size() != 1) {
          emitError(location) << "Ok() expects exactly one argument";
          return nullptr;
        }
        auto argVal = generateExpression(ast::callArgExpr(call.args[0]).value);
        if (!argVal)
          return nullptr;
        mlir::Type resultType;
        if (pendingDeclaredType && mlir::isa<hew::ResultEnumType>(*pendingDeclaredType))
          resultType = *pendingDeclaredType;
        else if (currentFunction && currentFunction.getResultTypes().size() == 1 &&
                 mlir::isa<hew::ResultEnumType>(currentFunction.getResultTypes()[0]))
          resultType = currentFunction.getResultTypes()[0];
        else {
          resultType = hew::ResultEnumType::get(&context, argVal.getType(), builder.getI32Type());
        }
        if (auto resultEnumType = mlir::dyn_cast<hew::ResultEnumType>(resultType);
            resultEnumType && argVal.getType() != resultEnumType.getOkType()) {
          argVal = coerceType(argVal, resultEnumType.getOkType(), location);
          if (!argVal)
            return nullptr;
        }
        mlir::Value result = hew::EnumConstructOp::create(
            builder, location, resultType, static_cast<uint32_t>(variantIndex),
            llvm::StringRef("__Result"), mlir::ValueRange{argVal},
            /*payload_positions=*/builder.getI64ArrayAttr({1}));
        return result;
      }

      // Built-in Err(x)
      if (calleeName == "Err" && enumName == "__Result") {
        if (call.args.size() != 1) {
          emitError(location) << "Err() expects exactly one argument";
          return nullptr;
        }
        auto argVal = generateExpression(ast::callArgExpr(call.args[0]).value);
        if (!argVal)
          return nullptr;
        mlir::Type resultType;
        if (pendingDeclaredType && mlir::isa<hew::ResultEnumType>(*pendingDeclaredType))
          resultType = *pendingDeclaredType;
        else if (currentFunction && currentFunction.getResultTypes().size() == 1 &&
                 mlir::isa<hew::ResultEnumType>(currentFunction.getResultTypes()[0]))
          resultType = currentFunction.getResultTypes()[0];
        else {
          resultType = hew::ResultEnumType::get(&context, builder.getI32Type(), argVal.getType());
        }
        if (auto resultEnumType = mlir::dyn_cast<hew::ResultEnumType>(resultType);
            resultEnumType && argVal.getType() != resultEnumType.getErrType()) {
          argVal = coerceType(argVal, resultEnumType.getErrType(), location);
          if (!argVal)
            return nullptr;
        }
        mlir::Value result = hew::EnumConstructOp::create(
            builder, location, resultType, static_cast<uint32_t>(variantIndex),
            llvm::StringRef("__Result"), mlir::ValueRange{argVal},
            /*payload_positions=*/builder.getI64ArrayAttr({2}));
        return result;
      }

      // User-defined enum variant constructor
      auto enumIt = enumTypes.find(enumName);
      if (enumIt != enumTypes.end()) {
        const auto &enumInfo = enumIt->second;

        if (enumInfo.hasPayloads) {
          const EnumVariantInfo *vi = nullptr;
          for (const auto &v : enumInfo.variants) {
            if (v.index == static_cast<unsigned>(variantIndex)) {
              vi = &v;
              break;
            }
          }
          llvm::SmallVector<mlir::Value> payloads;
          for (size_t i = 0; i < call.args.size(); ++i) {
            auto argVal = generateExpression(ast::callArgExpr(call.args[i]).value);
            if (!argVal)
              return nullptr;
            if (vi && i < vi->payloadTypes.size() && argVal.getType() != vi->payloadTypes[i]) {
              argVal = coerceType(argVal, vi->payloadTypes[i], location);
              if (!argVal)
                return nullptr;
            }
            payloads.push_back(argVal);
          }

          if (enumInfo.isIndirect) {
            // Indirect enum: build struct, malloc, store, return pointer
            auto innerType = enumInfo.innerStructType;
            auto payloadPositionsAttr =
                vi ? buildPayloadPositionsAttr(builder, vi->payloadPositions, payloads.size())
                   : nullptr;
            mlir::Value structVal = hew::EnumConstructOp::create(
                builder, location, innerType, static_cast<uint32_t>(variantIndex),
                llvm::StringRef(enumName), payloads, payloadPositionsAttr);

            // malloc(sizeof(innerType))
            auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
            auto mallocFuncType = mlir::FunctionType::get(&context, {sizeType()}, {ptrType});
            getOrCreateExternFunc("malloc", mallocFuncType);
            auto sizeVal = hew::SizeOfOp::create(builder, location, sizeType(),
                                                 mlir::TypeAttr::get(innerType));
            auto mallocCall = mlir::func::CallOp::create(
                builder, location, "malloc", mlir::TypeRange{ptrType}, mlir::ValueRange{sizeVal});
            auto allocPtr = mallocCall.getResult(0);

            // Store struct into malloc'd memory
            mlir::LLVM::StoreOp::create(builder, location, structVal, allocPtr);
            return allocPtr;
          }

          auto payloadPositionsAttr =
              vi ? buildPayloadPositionsAttr(builder, vi->payloadPositions, payloads.size())
                 : nullptr;
          mlir::Value result = hew::EnumConstructOp::create(
              builder, location, enumInfo.mlirType, static_cast<uint32_t>(variantIndex),
              llvm::StringRef(enumName), payloads, payloadPositionsAttr);
          return result;
        }
        // Unit variant of indirect enum: still needs heap allocation
        if (enumInfo.isIndirect && enumInfo.innerStructType) {
          auto innerType = enumInfo.innerStructType;
          mlir::Value structVal = hew::EnumConstructOp::create(
              builder, location, innerType, static_cast<uint32_t>(variantIndex),
              llvm::StringRef(enumName), mlir::ValueRange{},
              /*payload_positions=*/mlir::ArrayAttr{});

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
        return createIntConstant(builder, location, builder.getI32Type(), variantIndex);
      }
    }
  }

  // Generate arguments, propagating expected types to lambdas for inference.
  // Try mangled name first, then fall back to unmangled (for externs like hew_*).
  std::string mangledCallee = mangleName(currentModulePath, "", calleeName);
  auto callee = module.lookupSymbol<mlir::func::FuncOp>(mangledCallee);
  if (!callee)
    callee = module.lookupSymbol<mlir::func::FuncOp>(calleeName);
  // Try alias resolution: e.g. hello → mylib.greet (scoped to current module)
  if (!callee) {
    auto modKey = currentModuleKey();
    auto aliasIt = aliasToFunction.find(modKey + "::" + calleeName);
    if (aliasIt != aliasToFunction.end()) {
      const auto &[origPath, origName] = aliasIt->second;
      std::string aliasMangled = mangleName(origPath, "", origName);
      callee = module.lookupSymbol<mlir::func::FuncOp>(aliasMangled);
    }
  }
  // Try imported module paths (for cross-module calls like diamond deps).
  if (!callee)
    callee = lookupImportedFunc("", calleeName);
  mlir::FunctionType calleeFuncType = callee ? callee.getFunctionType() : nullptr;

  llvm::SmallVector<mlir::Value, 4> args;
  for (size_t i = 0; i < call.args.size(); ++i) {
    const auto &arg = call.args[i];
    const auto &argSpanned = ast::callArgExpr(arg);

    if (calleeFuncType && i < calleeFuncType.getNumInputs()) {
      auto expectedArgType = calleeFuncType.getInput(i);
      if (std::holds_alternative<ast::ExprLambda>(argSpanned.value.kind)) {
        if (mlir::isa<hew::ClosureType>(expectedArgType) ||
            mlir::isa<mlir::FunctionType>(expectedArgType)) {
          pendingLambdaExpectedType = expectedArgType;
        }
      }
    }

    auto val = generateExpression(argSpanned.value);
    pendingLambdaExpectedType.reset();
    if (!val)
      return nullptr;

    // Coerce concrete struct -> dyn Trait fat pointer if the parameter expects one.
    // Derive the required trait from the callee's declared parameter type so that
    // the exact vtable is always chosen, regardless of how many traits the concrete
    // type implements.
    if (calleeFuncType && i < calleeFuncType.getNumInputs()) {
      auto expectedType = calleeFuncType.getInput(i);
      auto valType = val.getType();
      if (expectedType != valType) {
        if (auto traitObjType = mlir::dyn_cast<hew::HewTraitObjectType>(expectedType)) {
          if (auto identStruct = llvm::dyn_cast<mlir::LLVM::LLVMStructType>(valType)) {
            if (identStruct.isIdentified()) {
              std::string structName = identStruct.getName().str();
              std::string traitName = traitObjType.getTraitName().str();
              auto dispIt = traitDispatchRegistry.find(traitName);
              if (dispIt != traitDispatchRegistry.end()) {
                for (const auto &impl : dispIt->second.impls) {
                  if (impl.typeName == structName) {
                    val = coerceToDynTrait(val, structName, traitName, location);
                    break;
                  }
                }
              }
            }
          }
        }
      }
    }

    // Materialize heap-allocated temporaries as implicit let-bindings so they
    // enter the normal scope-exit drop system.  This prevents leaks from
    // expressions like foo(Vec::new()), println(f"count: {n}"), etc.
    materializeTemporary(val, argSpanned.value);

    args.push_back(val);
  }

  if (callee) {
    auto funcType = callee.getFunctionType();
    for (size_t i = 0; i < args.size() && i < funcType.getNumInputs(); ++i) {
      auto expectedType = funcType.getInput(i);
      auto actualType = args[i].getType();
      if (actualType != expectedType && isPointerLikeType(actualType) &&
          mlir::isa<mlir::LLVM::LLVMPointerType>(expectedType)) {
        args[i] = hew::BitcastOp::create(builder, location, expectedType, args[i]);
      } else if (actualType != expectedType && mlir::isa<mlir::LLVM::LLVMPointerType>(actualType) &&
                 isPointerLikeType(expectedType)) {
        args[i] = hew::BitcastOp::create(builder, location, expectedType, args[i]);
      } else if (actualType != expectedType) {
        bool argUnsigned = false;
        if (i < call.args.size()) {
          auto *argType = resolvedTypeOf(ast::callArgExpr(call.args[i]).span);
          if (argType && isUnsignedTypeExpr(*argType))
            argUnsigned = true;
        }
        args[i] = coerceType(args[i], expectedType, location, argUnsigned);
        if (!args[i])
          return nullptr;
      }
    }
    auto callOp = mlir::func::CallOp::create(builder, location, callee, args);
    if (call.is_tail_call)
      callOp->setAttr("hew.tail_call", builder.getUnitAttr());

    // After a call that takes ownership of stream/sink arguments, null out
    // the corresponding RAII allocas so scope-exit auto-close is a no-op.
    // This handles: explicit .close(), stream.forward (pipe), and adapter
    // functions (lines, chunks, map, filter, take, collect) that consume
    // the input stream.
    {
      // Map: argument index → true if that arg's handle is consumed.
      // Most consuming functions consume arg 0; hew_stream_pipe consumes both.
      std::vector<size_t> consumedArgIndices;
      if (calleeName == "hew_stream_close" || calleeName == "hew_sink_close" ||
          calleeName == "hew_stream_pair_free" || calleeName == "hew_stream_lines" ||
          calleeName == "hew_stream_chunks" || calleeName == "hew_stream_map_string" ||
          calleeName == "hew_stream_map_bytes" || calleeName == "hew_stream_filter_string" ||
          calleeName == "hew_stream_filter_bytes" || calleeName == "hew_stream_take" ||
          calleeName == "hew_stream_collect" || calleeName == "hew_stream_collect_string" ||
          calleeName == "hew_stream_collect_bytes") {
        consumedArgIndices.push_back(0);
      } else if (calleeName == "hew_stream_pipe") {
        consumedArgIndices.push_back(0);
        consumedArgIndices.push_back(1);
      }
      // For user-defined functions (not hew_* runtime), assume all
      // stream/sink arguments transfer ownership to the callee.
      // Limitation: functions that only observe (borrow) a handle will
      // disable the caller's auto-close, causing a leak. A proper fix
      // requires move semantics in the type system. For now this is
      // the lesser evil vs double-frees from callee-consumed handles.
      static const std::unordered_set<std::string> nonConsumingFns = {
          "hew_sink_write",        "hew_sink_write_string",
          "hew_sink_write_bytes",  "hew_sink_flush",
          "hew_stream_is_valid",   "hew_sink_is_valid",
          "hew_stream_is_closed",  "hew_stream_next",
          "hew_stream_next_sized", "hew_stream_next_bytes",
          "hew_stream_pair_sink",  "hew_stream_pair_stream"};
      if (consumedArgIndices.empty() &&
          (calleeName.substr(0, 4) != "hew_" ||
           calleeName.find("stream") != std::string::npos ||
           calleeName.find("sink") != std::string::npos) &&
          !nonConsumingFns.count(calleeName)) {
        // For non-runtime or unknown runtime calls, null all stream/sink args
        for (size_t i = 0; i < call.args.size(); ++i) {
          auto *id = std::get_if<ast::ExprIdentifier>(
              &ast::callArgExpr(call.args[i]).value.kind);
          if (!id)
            continue;
          if (streamHandleVarTypes.count(id->name))
            consumedArgIndices.push_back(i);
        }
      }
      for (size_t argIdx : consumedArgIndices) {
        if (argIdx >= call.args.size())
          continue;
        auto *id = std::get_if<ast::ExprIdentifier>(
            &ast::callArgExpr(call.args[argIdx]).value.kind);
        if (!id)
          continue;
        nullOutRaiiAlloca(id->name);
      }
    }

    // Drop RC environments of temporary closure arguments after the call.
    // Variable-bound closures (identifiers, field accesses) are dropped by
    // popDropScope via their let-binding registration.  All other closure
    // arguments (inline lambdas, call-returned closures, block expressions)
    // are unbound temporaries that need explicit cleanup here.
    {
      auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
      for (size_t i = 0; i < args.size() && i < call.args.size(); ++i) {
        if (!mlir::isa<hew::ClosureType>(args[i].getType()))
          continue;
        const auto &argExpr = ast::callArgExpr(call.args[i]).value;
        if (std::holds_alternative<ast::ExprIdentifier>(argExpr.kind))
          continue;
        if (std::holds_alternative<ast::ExprFieldAccess>(argExpr.kind))
          continue;
        auto envPtr = hew::ClosureGetEnvOp::create(builder, location, ptrType, args[i]);
        hew::DropOp::create(builder, location, envPtr, "hew_rc_drop", false);
      }
    }

    if (callOp.getNumResults() > 0) {
      auto result = callOp.getResult(0);
      auto externRetIt = externSemanticReturnTypes.find(callee.getSymName().str());
      if (externRetIt != externSemanticReturnTypes.end() &&
          mlir::isa<mlir::LLVM::LLVMPointerType>(result.getType()) &&
          result.getType() != externRetIt->second) {
        result = hew::BitcastOp::create(builder, location, externRetIt->second, result);
      }
      return result;
    }
    return nullptr;
  }

  // Check if callee is a variable holding a closure or function reference
  auto calleeVal = lookupVariable(calleeName);
  if (calleeVal) {
    if (auto closureType = mlir::dyn_cast<hew::ClosureType>(calleeVal.getType())) {
      auto closurePtrType = mlir::LLVM::LLVMPointerType::get(&context);
      auto fnPtr = hew::ClosureGetFnOp::create(builder, location, closurePtrType, calleeVal);
      auto envPtr = hew::ClosureGetEnvOp::create(builder, location, closurePtrType, calleeVal);

      // Use LLVM storage types so the func.call_indirect passes legality
      // checks during the Hew dialect lowering (partial conversion).
      llvm::SmallVector<mlir::Type, 8> indirectParamTypes;
      indirectParamTypes.push_back(closurePtrType);
      for (auto inTy : closureType.getInputTypes())
        indirectParamTypes.push_back(toLLVMStorageType(inTy));

      auto hewRetType = closureType.getResultType();
      auto retType = toLLVMStorageType(hewRetType);
      bool hasReturn = retType && !mlir::isa<mlir::NoneType>(retType);
      auto indirectFuncType = hasReturn
                                  ? mlir::FunctionType::get(&context, indirectParamTypes, {retType})
                                  : mlir::FunctionType::get(&context, indirectParamTypes, {});

      auto fnRef = hew::BitcastOp::create(builder, location, indirectFuncType, fnPtr);

      llvm::SmallVector<mlir::Value, 8> indirectArgs;
      indirectArgs.push_back(envPtr);
      for (size_t i = 0; i < args.size() && i < closureType.getInputTypes().size(); ++i) {
        auto expectedTy = toLLVMStorageType(closureType.getInputTypes()[i]);
        if (args[i].getType() != expectedTy)
          args[i] = coerceType(args[i], expectedTy, location);
        indirectArgs.push_back(args[i]);
      }

      auto callOp = mlir::func::CallIndirectOp::create(builder, location, fnRef, indirectArgs);
      if (callOp.getNumResults() > 0) {
        auto result = callOp.getResult(0);
        // Bitcast back to the Hew type if the lowered return type differs.
        if (hewRetType != retType)
          result = hew::BitcastOp::create(builder, location, hewRetType, result);
        return result;
      }
      return nullptr;
    }
  }

  ++errorCount_;
  emitError(location) << "undefined function '" << calleeName << "'; is it declared or imported?";
  return nullptr;
}

// ============================================================================
// Print built-in
// ============================================================================

mlir::Value MLIRGen::generatePrintCall(const ast::ExprCall &call, bool newline) {
  auto location = currentLoc;

  if (call.args.empty()) {
    emitError(location) << "print/println requires at least one argument";
    return nullptr;
  }

  auto val = generateExpression(ast::callArgExpr(call.args[0]).value);
  if (!val)
    return nullptr;

  // Materialize temporary strings (f-strings, to_string(), etc.) so they
  // are dropped at scope exit instead of requiring ad-hoc cleanup.
  materializeTemporary(val, ast::callArgExpr(call.args[0]).value);

  auto printOp = hew::PrintOp::create(builder, location, val, builder.getBoolAttr(newline));
  // Propagate unsigned type info so the lowering uses unsigned print routines.
  if (auto *argType = resolvedTypeOf(ast::callArgExpr(call.args[0]).span))
    if (isUnsignedTypeExpr(*argType))
      printOp->setAttr("is_unsigned", builder.getBoolAttr(true));

  return nullptr; // print returns void
}

// ============================================================================
// If expression generation (expression form that yields a value)
// ============================================================================

mlir::Value MLIRGen::generateIfExpr(const ast::ExprIf &ifE, const ast::Span &exprSpan) {
  auto location = currentLoc;

  auto cond = generateExpression(ifE.condition->value);
  if (!cond)
    return nullptr;

  if (cond.getType() != builder.getI1Type()) {
    auto zero = createIntConstant(builder, location, cond.getType(), 0);
    cond =
        mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::ne, cond, zero);
  }

  bool hasElse = ifE.else_block.has_value();

  if (!hasElse) {
    auto ifOp = mlir::scf::IfOp::create(builder, location, /*resultTypes=*/mlir::TypeRange{}, cond,
                                        /*withElseRegion=*/false);

    builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
    if (ifE.then_block) {
      generateExpression(ifE.then_block->value);
    }
    auto *thenBlock = builder.getInsertionBlock();
    if (thenBlock->empty() || !thenBlock->back().hasTrait<mlir::OpTrait::IsTerminator>()) {
      mlir::scf::YieldOp::create(builder, location);
    }

    builder.setInsertionPointAfter(ifOp);
    return nullptr;
  }

  // Use the type checker's resolved type for this if-expression when available.
  // This is correct even when the if-expression is not in tail position.
  mlir::Type resultType;
  if (auto *resolvedType = resolvedTypeOf(exprSpan)) {
    resultType = convertType(*resolvedType);
  } else if (currentFunction && currentFunction.getResultTypes().size() == 1) {
    resultType = currentFunction.getResultTypes()[0];
  } else {
    emitError(location) << "if-expression result type not resolved";
    return nullptr;
  }

  auto ifOp = mlir::scf::IfOp::create(builder, location, resultType, cond, /*withElseRegion=*/true);

  // Then branch
  builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
  mlir::Value thenVal = nullptr;
  if (ifE.then_block) {
    thenVal = generateExpression(ifE.then_block->value);
  }
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

  // Else branch
  builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
  mlir::Value elseVal = nullptr;
  if (ifE.else_block.has_value() && *ifE.else_block) {
    elseVal = generateExpression((*ifE.else_block)->value);
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

// ============================================================================
// Block expression generation
// ============================================================================

mlir::Value MLIRGen::generateBlockExpr(const ast::Block &block) {
  return generateBlock(block);
}

// ============================================================================
// Postfix try (?) expression generation
// ============================================================================

mlir::Value MLIRGen::generatePostfixExpr(const ast::ExprPostfixTry &expr) {
  auto location = currentLoc;

  auto operandVal = generateExpression(expr.inner->value);
  if (!operandVal)
    return nullptr;

  auto operandType = operandVal.getType();

  // Handle Option? — unwrap Some or propagate None
  if (auto optType = mlir::dyn_cast<hew::OptionEnumType>(operandType)) {
    auto tag = hew::EnumExtractTagOp::create(builder, location, builder.getI32Type(), operandVal);
    auto zeroTag = createIntConstant(builder, location, builder.getI32Type(), 0);
    auto isNone = mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::eq,
                                              tag, zeroTag);

    auto innerType = optType.getInnerType();
    auto someFieldIndex = resolvePayloadFieldIndex("Some", /*payloadOrdinal=*/0);

    mlir::Type funcRetType;
    if (currentFunction && currentFunction.getResultTypes().size() == 1)
      funcRetType = currentFunction.getResultTypes()[0];

    auto *noneBlock = currentFunction.addBlock();
    auto *someBlock = currentFunction.addBlock();

    mlir::cf::CondBranchOp::create(builder, location, isNone, noneBlock, someBlock);

    builder.setInsertionPointToStart(noneBlock);
    if (funcRetType && mlir::isa<hew::OptionEnumType>(funcRetType)) {
      mlir::Value noneResult = hew::EnumConstructOp::create(
          builder, location, funcRetType, static_cast<uint32_t>(0), llvm::StringRef("Option"),
          mlir::ValueRange{}, /*payload_positions=*/mlir::ArrayAttr{});
      mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{noneResult});
    } else {
      mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{});
    }

    builder.setInsertionPointToStart(someBlock);
    auto someVal = hew::EnumExtractPayloadOp::create(builder, location, innerType, operandVal,
                                                     /*field_index=*/someFieldIndex);
    return someVal;
  }

  auto resType = mlir::dyn_cast<hew::ResultEnumType>(operandType);
  if (!resType) {
    emitError(location) << "? operator requires a Result or Option type";
    return nullptr;
  }

  auto tag = hew::EnumExtractTagOp::create(builder, location, builder.getI32Type(), operandVal);
  auto oneTag = createIntConstant(builder, location, builder.getI32Type(), 1);
  auto isErr =
      mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::eq, tag, oneTag);

  auto okType = resType.getOkType();
  auto errType = resType.getErrType();
  auto okFieldIndex = resolvePayloadFieldIndex("Ok", /*payloadOrdinal=*/0);
  auto errFieldIndex = resolvePayloadFieldIndex("Err", /*payloadOrdinal=*/0);

  mlir::Type funcRetType;
  if (currentFunction && currentFunction.getResultTypes().size() == 1)
    funcRetType = currentFunction.getResultTypes()[0];

  auto *errBlock = currentFunction.addBlock();
  auto *okBlock = currentFunction.addBlock();

  mlir::cf::CondBranchOp::create(builder, location, isErr, errBlock, okBlock);

  builder.setInsertionPointToStart(errBlock);
  if (tryErrorDest) {
    auto errVal = hew::EnumExtractPayloadOp::create(builder, location, errType, operandVal,
                                                    /*field_index=*/errFieldIndex);
    auto coerced = coerceType(errVal, builder.getI32Type(), location);
    if (coerced)
      mlir::memref::StoreOp::create(builder, location, coerced, tryErrorSlot);
    mlir::cf::BranchOp::create(builder, location, tryErrorDest);
  } else if (funcRetType) {
    auto errVal = hew::EnumExtractPayloadOp::create(builder, location, errType, operandVal,
                                                    /*field_index=*/errFieldIndex);
    mlir::Value errResult = hew::EnumConstructOp::create(
        builder, location, funcRetType, static_cast<uint32_t>(1), llvm::StringRef("__Result"),
        mlir::ValueRange{errVal}, /*payload_positions=*/builder.getI64ArrayAttr({2}));
    mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{errResult});
  } else {
    mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{});
  }

  builder.setInsertionPointToStart(okBlock);
  auto okVal = hew::EnumExtractPayloadOp::create(builder, location, okType, operandVal,
                                                 /*field_index=*/okFieldIndex);
  return okVal;
}

// ============================================================================
// Struct initialization
// ============================================================================

mlir::Value MLIRGen::generateStructInit(const ast::ExprStructInit &si,
                                        const ast::Span &exprSpan) {
  auto location = currentLoc;
  const auto &structName = si.name;

  auto it = structTypes.find(structName);
  if (it == structTypes.end()) {
    // Try mangled name based on active type param substitutions
    auto genIt = genericStructs.find(structName);
    if (genIt != genericStructs.end() && !typeParamSubstitutions.empty()) {
      const auto *genDecl = genIt->second;
      if (genDecl->type_params) {
        std::string mangledName = structName;
        for (const auto &tp : *genDecl->type_params) {
          auto substIt = typeParamSubstitutions.find(tp.name);
          if (substIt != typeParamSubstitutions.end())
            mangledName += "_" + substIt->second;
        }
        it = structTypes.find(mangledName);
      }
    }
    // Non-generic context: use the type checker's resolved concrete type
    // (e.g., Wrapper<i32>) to materialize and look up the specialization.
    // This handles generic struct constructor expressions in non-generic
    // function bodies (e.g., `Wrapper { inner: 42 }` in main()) where
    // typeParamSubstitutions is empty but the type checker already inferred
    // the concrete type args.
    if (it == structTypes.end()) {
      if (const auto *resolvedTy = resolvedTypeOf(exprSpan)) {
        if (auto *named = std::get_if<ast::TypeNamed>(&resolvedTy->kind)) {
          if (named->type_args && !named->type_args->empty()) {
            // resolveTypeArgMangledName calls convertType internally, which
            // can insert into structTypes and rehash the map, invalidating
            // any iterator obtained before this call.  Always re-fetch `it`
            // from the map after the call so the subsequent end() check uses
            // a valid iterator regardless of whether specialization succeeded.
            std::string mangledName = resolveTypeArgMangledName(*resolvedTy);
            it = structTypes.find(mangledName);
          }
        }
      }
    }
    if (it == structTypes.end()) {
      auto varIt = variantLookup.find(structName);
      if (varIt != variantLookup.end()) {
        const auto &enumName = varIt->second.first;
        auto enumIt = enumTypes.find(enumName);
        if (enumIt == enumTypes.end()) {
          emitError(location) << "unknown enum type '" << enumName << "'";
          return nullptr;
        }
        const auto &enumInfo = enumIt->second;
        const EnumVariantInfo *vi = nullptr;
        for (const auto &v : enumInfo.variants) {
          if (v.index == varIt->second.second) {
            vi = &v;
            break;
          }
        }
        if (!vi) {
          emitError(location) << "unknown variant '" << structName << "' in enum '" << enumName
                              << "'";
          return nullptr;
        }
        if (vi->fieldNames.empty()) {
          emitError(location) << "enum variant '" << structName
                              << "' does not support struct-style initialization";
          return nullptr;
        }
        llvm::SmallVector<mlir::Value, 4> payloads(vi->payloadTypes.size(), nullptr);
        for (const auto &[fieldName, fieldVal] : si.fields) {
          auto fieldIt = std::find(vi->fieldNames.begin(), vi->fieldNames.end(), fieldName);
          if (fieldIt == vi->fieldNames.end()) {
            ++errorCount_;
            emitError(location) << "no field '" << fieldName << "' on variant '" << structName
                                << "'";
            return nullptr;
          }
          size_t fieldIdx = static_cast<size_t>(fieldIt - vi->fieldNames.begin());
          auto val = generateExpression(fieldVal->value);
          if (!val)
            return nullptr;
          val = coerceType(val, vi->payloadTypes[fieldIdx], location);
          payloads[fieldIdx] = val;
        }
        for (size_t i = 0; i < payloads.size(); ++i) {
          if (!payloads[i]) {
            emitError(location) << "missing field '" << vi->fieldNames[i] << "' in initializer of '"
                                << structName << "'";
            return nullptr;
          }
        }
        auto payloadPositionsAttr =
            buildPayloadPositionsAttr(builder, vi->payloadPositions, payloads.size());
        return hew::EnumConstructOp::create(
            builder, location, enumInfo.mlirType, static_cast<uint32_t>(varIt->second.second),
            llvm::StringRef(enumName), payloads, payloadPositionsAttr);
      }
      emitError(location) << "unknown struct type '" << structName << "'";
      return nullptr;
    }
  }

  const auto &info = it->second;

  llvm::SmallVector<mlir::Value, 4> fieldValues(info.fields.size(), nullptr);
  llvm::SmallVector<std::string, 4> fieldNames;
  for (const auto &field : info.fields) {
    fieldNames.push_back(field.name);
  }

  for (const auto &[fieldInitName, fieldInitVal] : si.fields) {
    bool found = false;
    for (const auto &field : info.fields) {
      if (field.name == fieldInitName) {
        auto val = generateExpression(fieldInitVal->value);
        if (!val)
          return nullptr;
        val = coerceType(val, field.type, location);
        fieldValues[field.index] = val;
        found = true;
        break;
      }
    }
    if (!found) {
      ++errorCount_;
      auto diag = emitError(location)
                  << "no field '" << fieldInitName << "' on struct '" << structName << "'";
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
  }

  for (size_t i = 0; i < info.fields.size(); ++i) {
    if (!fieldValues[i]) {
      emitError(location) << "missing field '" << info.fields[i].name << "' in struct init for '"
                          << structName << "'";
      return nullptr;
    }
  }

  auto fieldNamesAttr = builder.getStrArrayAttr(
      llvm::SmallVector<llvm::StringRef, 4>(fieldNames.begin(), fieldNames.end()));

  return hew::StructInitOp::create(builder, location, info.mlirType, fieldValues, fieldNamesAttr,
                                   builder.getStringAttr(structName));
}

// ============================================================================
// Log call generation (log.error / log.warn / log.info / log.debug / log.trace)
// ============================================================================

mlir::Value MLIRGen::generateLogCall(const ast::ExprMethodCall &mc) {
  auto location = currentLoc;
  const auto &method = mc.method;
  auto i32Type = builder.getI32Type();

  // Map level method names to integer levels.
  // Non-emit methods (setup, set_level, etc.) are forwarded as runtime calls.
  int levelInt = -1;
  if (method == "error")
    levelInt = 0;
  else if (method == "warn")
    levelInt = 1;
  else if (method == "info")
    levelInt = 2;
  else if (method == "debug")
    levelInt = 3;
  else if (method == "trace")
    levelInt = 4;

  // Non-emit log methods: forward as plain runtime calls.
  if (levelInt < 0) {
    std::string callee;
    if (method == "setup" || method == "setup_level" || method == "set_level")
      callee = "hew_log_set_level";
    else if (method == "get_level" || method == "is_enabled")
      callee = "hew_log_get_level";
    else {
      emitError(location) << "unknown log method: " << method;
      return nullptr;
    }

    llvm::SmallVector<mlir::Value, 4> argVals;
    for (const auto &arg : mc.args) {
      auto val = generateExpression(ast::callArgExpr(arg).value);
      if (!val)
        return nullptr;
      argVals.push_back(val);
    }

    // log.setup() takes no args but calls hew_log_set_level(2) (default INFO)
    if (method == "setup" && argVals.empty()) {
      auto infoLevel = mlir::arith::ConstantIntOp::create(builder, location, 2, 32);
      argVals.push_back(infoLevel);
    }

    // Determine argument types for the extern declaration.
    llvm::SmallVector<mlir::Type, 4> argTypes;
    for (auto v : argVals)
      argTypes.push_back(v.getType());

    bool hasResult = (method == "get_level" || method == "is_enabled");
    auto funcType = hasResult ? mlir::FunctionType::get(&context, argTypes, {i32Type})
                              : mlir::FunctionType::get(&context, argTypes, {});
    getOrCreateExternFunc(callee, funcType);

    auto calleeAttr = mlir::SymbolRefAttr::get(&context, callee);
    if (hasResult) {
      auto op = hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{i32Type}, calleeAttr,
                                           argVals);
      return op.getResult();
    }
    hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{}, calleeAttr, argVals);
    return nullptr;
  }

  // ── Emit-level methods — delegate to shared helper ──────────────────
  return generateLogEmit(mc.args, levelInt);
}

// ============================================================================
// Log emit helper — shared by generateLogCall and generateCallExpr
// ============================================================================

mlir::Value MLIRGen::generateLogEmit(const std::vector<ast::CallArg> &args, int levelInt) {
  auto location = currentLoc;
  auto i32Type = builder.getI32Type();
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto strRefType = hew::StringRefType::get(&context);

  // 1. Call hew_log_get_level() to read the current filter level.
  auto getLevelFuncType = mlir::FunctionType::get(&context, {}, {i32Type});
  getOrCreateExternFunc("hew_log_get_level", getLevelFuncType);
  auto currentLevel =
      hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{i32Type},
                                 mlir::SymbolRefAttr::get(&context, "hew_log_get_level"),
                                 mlir::ValueRange{})
          .getResult();

  // 2. Compare: emit if levelInt <= currentLevel (i.e., the message level
  //    is at or above the configured filter).
  auto levelConst = createIntConstant(builder, location, i32Type, levelInt);
  auto enabled = mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::sle,
                                             levelConst, currentLevel);

  // 3. Wrap the log emission in scf.if(enabled).
  mlir::scf::IfOp::create(builder, location, enabled, [&](mlir::OpBuilder &b, mlir::Location loc) {
    // Track heap-allocated intermediates for cleanup.
    std::vector<mlir::Value> ownedTemps;

    // Generate the message string from the first positional arg.
    mlir::Value msgStr = nullptr;
    if (!args.empty()) {
      msgStr = generateExpression(ast::callArgExpr(args[0]).value);
      if (!msgStr)
        return;
      // Ensure it is a string; convert non-strings via hew.to_string.
      if (!mlir::isa<hew::StringRefType>(msgStr.getType()) &&
          !mlir::isa<mlir::LLVM::LLVMPointerType>(msgStr.getType())) {
        auto toStr = hew::ToStringOp::create(b, loc, strRefType, msgStr);
        if (auto *argType = resolvedTypeOf(ast::callArgExpr(args[0]).span))
          if (isUnsignedTypeExpr(*argType))
            toStr->setAttr("is_unsigned", b.getBoolAttr(true));
        msgStr = toStr;
        ownedTemps.push_back(toStr);
      }
    } else {
      // No args at all — use empty string.
      auto sym = getOrCreateGlobalString("");
      msgStr = hew::ConstantOp::create(b, loc, strRefType, b.getStringAttr(sym));
    }

    // Prepend actor context if inside an actor body.
    if (!currentActorName.empty()) {
      // " actor=TypeName" (compile-time constant)
      std::string actorPrefix = " actor=" + currentActorName;
      auto actorPrefixSym = getOrCreateGlobalString(actorPrefix);
      auto actorPrefixStr =
          hew::ConstantOp::create(b, loc, strRefType, b.getStringAttr(actorPrefixSym));
      ownedTemps.push_back(msgStr);
      msgStr = hew::StringConcatOp::create(b, loc, strRefType, msgStr, actorPrefixStr);

      // " actor_id=<runtime_id>" (runtime value)
      auto actorIdPrefixSym = getOrCreateGlobalString(" actor_id=");
      auto actorIdPrefixStr =
          hew::ConstantOp::create(b, loc, strRefType, b.getStringAttr(actorIdPrefixSym));

      auto i64Type = b.getI64Type();
      auto getIdFuncType = mlir::FunctionType::get(&context, {}, {i64Type});
      getOrCreateExternFunc("hew_actor_current_id", getIdFuncType);
      auto actorId =
          hew::RuntimeCallOp::create(b, loc, mlir::TypeRange{i64Type},
                                     mlir::SymbolRefAttr::get(&context, "hew_actor_current_id"),
                                     mlir::ValueRange{})
              .getResult();
      auto actorIdStr = hew::ToStringOp::create(b, loc, strRefType, actorId);
      ownedTemps.push_back(actorIdStr);

      ownedTemps.push_back(msgStr);
      msgStr = hew::StringConcatOp::create(b, loc, strRefType, msgStr, actorIdPrefixStr);
      ownedTemps.push_back(msgStr);
      msgStr = hew::StringConcatOp::create(b, loc, strRefType, msgStr, actorIdStr);
    }

    // Append named arguments as " key=value" pairs.
    for (size_t i = 0; i < args.size(); ++i) {
      auto name = ast::callArgName(args[i]);
      if (name.empty())
        continue; // skip positional args (already handled above)

      // Create the " key=" prefix string.
      std::string prefix = " " + name + "=";
      auto prefixSym = getOrCreateGlobalString(prefix);
      auto prefixStr = hew::ConstantOp::create(b, loc, strRefType, b.getStringAttr(prefixSym));

      // Generate the value expression.
      auto val = generateExpression(ast::callArgExpr(args[i]).value);
      if (!val)
        return;

      // Convert non-string values to string.
      mlir::Value valStr;
      if (mlir::isa<hew::StringRefType>(val.getType()) ||
          mlir::isa<mlir::LLVM::LLVMPointerType>(val.getType())) {
        valStr = val;
      } else {
        auto toStr = hew::ToStringOp::create(b, loc, strRefType, val);
        if (auto *argType = resolvedTypeOf(ast::callArgExpr(args[i]).span))
          if (isUnsignedTypeExpr(*argType))
            toStr->setAttr("is_unsigned", b.getBoolAttr(true));
        valStr = toStr;
        ownedTemps.push_back(toStr);
      }

      // Concat: msgStr + " key=" + valStr
      ownedTemps.push_back(msgStr);
      msgStr = hew::StringConcatOp::create(b, loc, strRefType, msgStr, prefixStr);
      ownedTemps.push_back(msgStr);
      msgStr = hew::StringConcatOp::create(b, loc, strRefType, msgStr, valStr);
    }

    // Cast the final string to !llvm.ptr for the C ABI call.
    mlir::Value msgPtr = msgStr;
    if (!mlir::isa<mlir::LLVM::LLVMPointerType>(msgStr.getType()))
      msgPtr = hew::BitcastOp::create(b, loc, ptrType, msgStr);

    // Declare and call hew_log_emit(level, msg).
    auto emitFuncType = mlir::FunctionType::get(&context, {i32Type, ptrType}, {});
    getOrCreateExternFunc("hew_log_emit", emitFuncType);

    hew::RuntimeCallOp::create(b, loc, mlir::TypeRange{},
                               mlir::SymbolRefAttr::get(&context, "hew_log_emit"),
                               mlir::ValueRange{levelConst, msgPtr});

    // Free the final concatenated message string.
    emitStringDrop(msgStr);

    // Free all heap-allocated intermediate strings (skip msgStr to avoid double-free).
    for (auto temp : ownedTemps)
      if (temp != msgStr)
        emitStringDrop(temp);

    mlir::scf::YieldOp::create(b, loc);
  });

  return nullptr;
}

std::optional<mlir::Value> MLIRGen::generateBuiltinMethodCall(const ast::ExprMethodCall &mc,
                                                              mlir::Value receiver,
                                                              mlir::Location location) {
  auto receiverType = receiver.getType();
  const auto &methodName = mc.method;
  auto i32Type = builder.getI32Type();
  auto i64Type = builder.getI64Type();
  const auto &method = methodName;

  auto emitVecMethod = [&](mlir::Value vecValue, mlir::Type elemType,
                           mlir::Value &resultOut) -> bool {
    if (method == "push") {
      auto val = generateExpression(ast::callArgExpr(mc.args[0]).value);
      if (!val)
        return true;
      val = coerceType(val, elemType, location);
      hew::VecPushOp::create(builder, location, vecValue, val);
      resultOut = nullptr;
      return true;
    }
    if (method == "get") {
      auto idx = generateExpression(ast::callArgExpr(mc.args[0]).value);
      if (!idx)
        return true;
      if (idx.getType() != i64Type)
        idx = mlir::arith::ExtSIOp::create(builder, location, i64Type, idx);
      resultOut = hew::VecGetOp::create(builder, location, elemType, vecValue, idx).getResult();
      return true;
    }
    if (method == "set") {
      auto idx = generateExpression(ast::callArgExpr(mc.args[0]).value);
      auto val = generateExpression(ast::callArgExpr(mc.args[1]).value);
      if (!idx || !val)
        return true;
      if (idx.getType() != i64Type)
        idx = mlir::arith::ExtSIOp::create(builder, location, i64Type, idx);
      val = coerceType(val, elemType, location);
      hew::VecSetOp::create(builder, location, vecValue, idx, val);
      resultOut = nullptr;
      return true;
    }
    if (method == "pop") {
      resultOut = hew::VecPopOp::create(builder, location, elemType, vecValue).getResult();
      return true;
    }
    if (method == "remove") {
      if (!mc.args.empty()) {
        auto argVal = generateExpression(ast::callArgExpr(mc.args[0]).value);
        if (!argVal)
          return true;
        // Always use value-based removal (remove first occurrence of value).
        // For index-based removal, use remove_at().
        argVal = coerceType(argVal, elemType, location);
        hew::VecRemoveOp::create(builder, location, vecValue, argVal);
      }
      resultOut = nullptr;
      return true;
    }
    if (method == "len") {
      resultOut = hew::VecLenOp::create(builder, location, i64Type, vecValue).getResult();
      return true;
    }
    if (method == "is_empty") {
      resultOut =
          hew::VecIsEmptyOp::create(builder, location, builder.getI1Type(), vecValue).getResult();
      return true;
    }
    if (method == "clear") {
      hew::VecClearOp::create(builder, location, vecValue);
      resultOut = nullptr;
      return true;
    }
    if (method == "append" || method == "extend") {
      if (mc.args.empty()) {
        emitError(location) << ".append() requires one argument";
        resultOut = nullptr;
        return true;
      }
      auto src = generateExpression(ast::callArgExpr(mc.args[0]).value);
      if (!src) {
        resultOut = nullptr;
        return true;
      }
      auto calleeAttr = mlir::SymbolRefAttr::get(&context, "hew_vec_append");
      hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{}, calleeAttr,
                                 mlir::ValueRange{vecValue, src});
      resultOut = nullptr;
      return true;
    }
    if (method == "to_string") {
      auto strType = hew::StringRefType::get(&context);
      auto calleeAttr = mlir::SymbolRefAttr::get(&context, "hew_bytes_to_string");
      resultOut = hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{strType},
                                             calleeAttr, mlir::ValueRange{vecValue})
                      .getResult();
      return true;
    }
    if (method == "join") {
      auto sep = generateExpression(ast::callArgExpr(mc.args[0]).value);
      if (!sep) {
        resultOut = nullptr;
        return true;
      }
      auto strType = hew::StringRefType::get(&context);
      auto calleeAttr = mlir::SymbolRefAttr::get(&context, "hew_vec_join_str");
      resultOut = builder
                      .create<hew::RuntimeCallOp>(location, mlir::TypeRange{strType}, calleeAttr,
                                                  mlir::ValueRange{vecValue, sep})
                      .getResult();
      return true;
    }
    if (method == "map") {
      // Generate the closure argument
      auto closureVal = generateExpression(ast::callArgExpr(mc.args[0]).value);
      if (!closureVal) {
        resultOut = nullptr;
        return true;
      }
      auto closureType = mlir::dyn_cast<hew::ClosureType>(closureVal.getType());
      if (!closureType) {
        emitError(location) << "Vec::map argument must be a closure";
        resultOut = nullptr;
        return true;
      }

      // Determine the return element type from the closure
      auto retElemType = closureType.getResultType();

      // Extract function pointer and environment pointer from closure
      auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
      auto fnPtr = hew::ClosureGetFnOp::create(builder, location, ptrType, closureVal);
      auto envPtr = hew::ClosureGetEnvOp::create(builder, location, ptrType, closureVal);

      // Build the indirect function type using LLVM storage types.
      auto llvmElemType = toLLVMStorageType(elemType);
      auto llvmRetElemType = toLLVMStorageType(retElemType);
      llvm::SmallVector<mlir::Type, 4> indirectParamTypes;
      indirectParamTypes.push_back(ptrType);       // env pointer
      indirectParamTypes.push_back(llvmElemType);  // element
      auto indirectFuncType = mlir::FunctionType::get(&context, indirectParamTypes, {llvmRetElemType});
      auto fnRef = hew::BitcastOp::create(builder, location, indirectFuncType, fnPtr);

      // Create result vec
      auto resultVecType = hew::VecType::get(&context, retElemType);
      auto resultVec = hew::VecNewOp::create(builder, location, resultVecType).getResult();

      // Loop from 0 to len
      auto len = hew::VecLenOp::create(builder, location, i64Type, vecValue).getResult();
      auto zero = createIntConstant(builder, location, i64Type, 0);
      auto one = createIntConstant(builder, location, i64Type, 1);
      auto loop = mlir::scf::ForOp::create(builder, location, zero, len, one);
      auto *body = loop.getBody();
      auto iv = loop.getInductionVar();
      builder.setInsertionPoint(body->getTerminator());

      // Get element, call closure, push result
      auto elem = hew::VecGetOp::create(builder, location, elemType, vecValue, iv).getResult();
      mlir::Value callElem = elem;
      if (elemType != llvmElemType)
        callElem = hew::BitcastOp::create(builder, location, llvmElemType, elem);
      auto callResult = mlir::func::CallIndirectOp::create(builder, location, fnRef,
                                                           mlir::ValueRange{envPtr, callElem});
      mlir::Value pushVal = callResult.getResult(0);
      if (retElemType != llvmRetElemType)
        pushVal = hew::BitcastOp::create(builder, location, retElemType, pushVal);
      hew::VecPushOp::create(builder, location, resultVec, pushVal);

      builder.setInsertionPointAfter(loop);

      // Drop closure env after use
      hew::DropOp::create(builder, location, envPtr, "hew_rc_drop", false);

      resultOut = resultVec;
      return true;
    }
    if (method == "filter") {
      // Generate the closure argument
      auto closureVal = generateExpression(ast::callArgExpr(mc.args[0]).value);
      if (!closureVal) {
        resultOut = nullptr;
        return true;
      }
      auto closureType = mlir::dyn_cast<hew::ClosureType>(closureVal.getType());
      if (!closureType) {
        emitError(location) << "Vec::filter argument must be a closure";
        resultOut = nullptr;
        return true;
      }

      // Extract function pointer and environment pointer
      auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
      auto fnPtr = hew::ClosureGetFnOp::create(builder, location, ptrType, closureVal);
      auto envPtr = hew::ClosureGetEnvOp::create(builder, location, ptrType, closureVal);

      // Build indirect function type using LLVM storage types.
      auto llvmElemType = toLLVMStorageType(elemType);
      llvm::SmallVector<mlir::Type, 4> indirectParamTypes;
      indirectParamTypes.push_back(ptrType);
      indirectParamTypes.push_back(llvmElemType);
      auto indirectFuncType =
          mlir::FunctionType::get(&context, indirectParamTypes, {builder.getI1Type()});
      auto fnRef = hew::BitcastOp::create(builder, location, indirectFuncType, fnPtr);

      // Create result vec (same type as source)
      auto resultVecType = hew::VecType::get(&context, elemType);
      auto resultVec = hew::VecNewOp::create(builder, location, resultVecType).getResult();

      // Loop from 0 to len
      auto len = hew::VecLenOp::create(builder, location, i64Type, vecValue).getResult();
      auto zero = createIntConstant(builder, location, i64Type, 0);
      auto one = createIntConstant(builder, location, i64Type, 1);
      auto loop = mlir::scf::ForOp::create(builder, location, zero, len, one);
      auto *body = loop.getBody();
      auto iv = loop.getInductionVar();
      builder.setInsertionPoint(body->getTerminator());

      // Get element, call closure to check predicate
      auto elem = hew::VecGetOp::create(builder, location, elemType, vecValue, iv).getResult();
      mlir::Value callElem = elem;
      if (elemType != llvmElemType)
        callElem = hew::BitcastOp::create(builder, location, llvmElemType, elem);
      auto callResult = mlir::func::CallIndirectOp::create(builder, location, fnRef,
                                                           mlir::ValueRange{envPtr, callElem});
      auto cond = callResult.getResult(0);

      // If true, push element to result vec
      auto ifOp = mlir::scf::IfOp::create(builder, location, mlir::TypeRange{}, cond,
                                          /*withElseRegion=*/false);
      // IfOp without results auto-creates a yield terminator in the then block
      builder.setInsertionPoint(ifOp.getThenRegion().front().getTerminator());
      hew::VecPushOp::create(builder, location, resultVec, elem);
      builder.setInsertionPointAfter(ifOp);

      builder.setInsertionPointAfter(loop);

      // Drop closure env after use
      hew::DropOp::create(builder, location, envPtr, "hew_rc_drop", false);

      resultOut = resultVec;
      return true;
    }
    if (method == "fold") {
      // First argument: initial accumulator value
      auto initAcc = generateExpression(ast::callArgExpr(mc.args[0]).value);
      if (!initAcc) {
        resultOut = nullptr;
        return true;
      }
      auto accType = initAcc.getType();

      // Second argument: closure (acc, elem) -> acc
      auto closureVal = generateExpression(ast::callArgExpr(mc.args[1]).value);
      if (!closureVal) {
        resultOut = nullptr;
        return true;
      }
      auto closureType = mlir::dyn_cast<hew::ClosureType>(closureVal.getType());
      if (!closureType) {
        emitError(location) << "Vec::fold second argument must be a closure";
        resultOut = nullptr;
        return true;
      }

      // Extract function pointer and environment pointer
      auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
      auto fnPtr = hew::ClosureGetFnOp::create(builder, location, ptrType, closureVal);
      auto envPtr = hew::ClosureGetEnvOp::create(builder, location, ptrType, closureVal);

      // Build indirect function type using LLVM storage types.
      auto llvmAccType = toLLVMStorageType(accType);
      auto llvmElemType = toLLVMStorageType(elemType);
      llvm::SmallVector<mlir::Type, 4> indirectParamTypes;
      indirectParamTypes.push_back(ptrType);
      indirectParamTypes.push_back(llvmAccType);
      indirectParamTypes.push_back(llvmElemType);
      auto indirectFuncType = mlir::FunctionType::get(&context, indirectParamTypes, {llvmAccType});
      auto fnRef = hew::BitcastOp::create(builder, location, indirectFuncType, fnPtr);

      // Loop from 0 to len with iter_args for accumulator
      auto len = hew::VecLenOp::create(builder, location, i64Type, vecValue).getResult();
      auto zero = createIntConstant(builder, location, i64Type, 0);
      auto one = createIntConstant(builder, location, i64Type, 1);
      auto loop =
          mlir::scf::ForOp::create(builder, location, zero, len, one, mlir::ValueRange{initAcc});
      auto *body = loop.getBody();
      auto iv = loop.getInductionVar();
      // The accumulator is the first region iter arg (after induction var)
      auto accArg = loop.getRegionIterArgs()[0];
      // No implicit terminator when iter_args present — insert at end of body
      builder.setInsertionPointToEnd(body);

      // Get element, call closure with (acc, elem), yield new acc
      auto elem = hew::VecGetOp::create(builder, location, elemType, vecValue, iv).getResult();
      mlir::Value callElem = elem;
      if (elemType != llvmElemType)
        callElem = hew::BitcastOp::create(builder, location, llvmElemType, elem);
      mlir::Value callAcc = accArg;
      if (accType != llvmAccType)
        callAcc = hew::BitcastOp::create(builder, location, llvmAccType, accArg);
      auto callResult = mlir::func::CallIndirectOp::create(builder, location, fnRef,
                                                           mlir::ValueRange{envPtr, callAcc, callElem});
      auto newAcc = callResult.getResult(0);
      if (accType != llvmAccType)
        newAcc = hew::BitcastOp::create(builder, location, accType, newAcc);

      // Yield the new accumulator value back to the loop
      mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{newAcc});

      builder.setInsertionPointAfter(loop);

      // Drop closure env after use
      hew::DropOp::create(builder, location, envPtr, "hew_rc_drop", false);

      resultOut = loop.getResult(0);
      return true;
    }
    return false;
  };

  auto emitHashMapMethod = [&](mlir::Value mapValue, mlir::Type keyType, mlir::Type valueType,
                               mlir::Value &resultOut) -> bool {
    if (method == "insert" || method == "set") {
      auto key = generateExpression(ast::callArgExpr(mc.args[0]).value);
      auto val = generateExpression(ast::callArgExpr(mc.args[1]).value);
      if (!key || !val)
        return true;
      key = coerceType(key, keyType, location);
      val = coerceType(val, valueType, location);
      hew::HashMapInsertOp::create(builder, location, mapValue, key, val);
      resultOut = nullptr;
      return true;
    }
    if (method == "get") {
      auto key = generateExpression(ast::callArgExpr(mc.args[0]).value);
      if (!key)
        return true;
      key = coerceType(key, keyType, location);
      // Wrap raw value in Option<V>: check contains_key, then get or None
      auto optionType = hew::OptionEnumType::get(&context, valueType);
      auto exists =
          hew::HashMapContainsKeyOp::create(builder, location, builder.getI1Type(), mapValue, key)
              .getResult();
      auto ifOp =
          mlir::scf::IfOp::create(builder, location, optionType, exists, /*withElseRegion=*/true);
      builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
      auto rawVal =
          hew::HashMapGetOp::create(builder, location, valueType, mapValue, key).getResult();
      auto someVal = hew::EnumConstructOp::create(
          builder, location, optionType, static_cast<uint32_t>(1), llvm::StringRef("Option"),
          mlir::ValueRange{rawVal}, /*payload_positions=*/builder.getI64ArrayAttr({1}));
      mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{someVal});
      builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
      auto noneVal = hew::EnumConstructOp::create(
          builder, location, optionType, static_cast<uint32_t>(0), llvm::StringRef("Option"),
          mlir::ValueRange{}, /*payload_positions=*/mlir::ArrayAttr{});
      mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{noneVal});
      builder.setInsertionPointAfter(ifOp);
      resultOut = ifOp.getResult(0);
      return true;
    }
    if (method == "remove") {
      auto key = generateExpression(ast::callArgExpr(mc.args[0]).value);
      if (!key)
        return true;
      key = coerceType(key, keyType, location);
      hew::HashMapRemoveOp::create(builder, location, mapValue, key);
      resultOut = nullptr;
      return true;
    }
    if (method == "contains_key" || method == "contains") {
      auto key = generateExpression(ast::callArgExpr(mc.args[0]).value);
      if (!key)
        return true;
      key = coerceType(key, keyType, location);
      resultOut =
          hew::HashMapContainsKeyOp::create(builder, location, builder.getI1Type(), mapValue, key)
              .getResult();
      return true;
    }
    if (method == "keys") {
      auto keysType = hew::VecType::get(&context, keyType);
      resultOut = hew::HashMapKeysOp::create(builder, location, keysType, mapValue).getResult();
      return true;
    }
    if (method == "values") {
      auto keysType = hew::VecType::get(&context, keyType);
      auto keysVec = hew::HashMapKeysOp::create(builder, location, keysType, mapValue).getResult();
      auto valuesType = hew::VecType::get(&context, valueType);
      auto valuesVec = hew::VecNewOp::create(builder, location, valuesType).getResult();
      auto len = hew::VecLenOp::create(builder, location, i64Type, keysVec).getResult();
      auto zero = createIntConstant(builder, location, i64Type, 0);
      auto one = createIntConstant(builder, location, i64Type, 1);
      auto loop = mlir::scf::ForOp::create(builder, location, zero, len, one);
      auto *body = loop.getBody();
      auto iv = loop.getInductionVar();
      // Insert before the implicit scf.yield created by ForOp builder
      builder.setInsertionPoint(body->getTerminator());
      auto key = hew::VecGetOp::create(builder, location, keyType, keysVec, iv).getResult();
      auto val = hew::HashMapGetOp::create(builder, location, valueType, mapValue, key).getResult();
      hew::VecPushOp::create(builder, location, valuesVec, val);
      builder.setInsertionPointAfter(loop);
      hew::VecFreeOp::create(builder, location, keysVec);
      resultOut = valuesVec;
      return true;
    }
    if (method == "len") {
      resultOut = hew::HashMapLenOp::create(builder, location, i64Type, mapValue).getResult();
      return true;
    }
    return false;
  };

  // HashSet<T> method dispatcher
  auto emitHashSetMethod = [&](mlir::Value setValue, mlir::Type elemType, mlir::Value argValue,
                               mlir::Value &resultOut) -> bool {
    // Helper for insert/contains/remove — identical structure, different runtime function.
    auto emitHashSetElemOp = [&](llvm::StringRef opName) -> bool {
      if (!argValue) {
        emitError(location) << "HashSet::" << opName << " requires an argument";
        return true;
      }
      auto val = coerceType(argValue, elemType, location);
      std::string funcName;
      if (elemType.isInteger(64)) {
        funcName = ("hew_hashset_" + opName + "_int").str();
      } else if (mlir::isa<hew::StringRefType>(elemType)) {
        funcName = ("hew_hashset_" + opName + "_string").str();
      } else {
        emitError(location) << "HashSet::" << opName
                            << " only supports int and String element types";
        return true;
      }
      resultOut = hew::RuntimeCallOp::create(
                      builder, location, mlir::TypeRange{builder.getI1Type()},
                      mlir::SymbolRefAttr::get(&context, funcName), mlir::ValueRange{setValue, val})
                      .getResult();
      return true;
    };

    if (method == "insert")
      return emitHashSetElemOp("insert");
    if (method == "contains")
      return emitHashSetElemOp("contains");
    if (method == "remove")
      return emitHashSetElemOp("remove");
    if (method == "len") {
      resultOut = hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{i64Type},
                                             mlir::SymbolRefAttr::get(&context, "hew_hashset_len"),
                                             mlir::ValueRange{setValue})
                      .getResult();
      return true;
    }
    if (method == "is_empty") {
      resultOut =
          hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{builder.getI1Type()},
                                     mlir::SymbolRefAttr::get(&context, "hew_hashset_is_empty"),
                                     mlir::ValueRange{setValue})
              .getResult();
      return true;
    }
    if (method == "clear") {
      hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                                 mlir::SymbolRefAttr::get(&context, "hew_hashset_clear"),
                                 mlir::ValueRange{setValue});
      resultOut = nullptr;
      return true;
    }
    return false;
  };

  if (auto vecType = mlir::dyn_cast<hew::VecType>(receiverType)) {
    mlir::Value vecResult;
    if (emitVecMethod(receiver, vecType.getElementType(), vecResult))
      return vecResult;
    emitError(location) << "unknown method '" << method << "' on collection type '" << receiverType
                        << "'";
    return mlir::Value{};
  }

  if (auto hmType = mlir::dyn_cast<hew::HashMapType>(receiverType)) {
    mlir::Value hmResult;
    if (emitHashMapMethod(receiver, hmType.getKeyType(), hmType.getValueType(), hmResult))
      return hmResult;
    emitError(location) << "unknown method '" << method << "' on collection type '" << receiverType
                        << "'";
    return mlir::Value{};
  }

  // HashSet<T> methods (HandleType with name "HashSet")
  if (auto handleType = mlir::dyn_cast<hew::HandleType>(receiverType)) {
    if (handleType.getHandleKind() == "HashSet") {
      // Determine element type from method arguments at the call site.
      // The i64 default below is a LOW-RISK fallback — it is only used when
      // the method takes no arguments (e.g., len(), clear()), where element
      // type doesn't affect the runtime call. Methods that process elements
      // (insert, contains, remove) always override from the actual argument type.
      mlir::Type elemType = i64Type;
      mlir::Value argValue;
      const bool methodRequiresArg =
          method == "insert" || method == "contains" || method == "remove";

      if (methodRequiresArg) {
        if (mc.args.empty()) {
          emitError(location) << "HashSet method '" << method << "' requires an argument";
          return mlir::Value{};
        }
        argValue = generateExpression(ast::callArgExpr(mc.args[0]).value);
        if (!argValue)
          return mlir::Value{};
        if (argValue.getType()) {
          elemType = argValue.getType();
        }
      }

      mlir::Value setResult;
      if (emitHashSetMethod(receiver, elemType, argValue, setResult))
        return setResult;
      emitError(location) << "unknown method '" << method << "' on HashSet";
      return mlir::Value{};
    }
  }

  // ── Stream<T> functional operators: map, filter, take ──────────────────────
  // Stream maps to raw LLVM pointer; use the resolved type to identify stream receivers.
  if (mlir::isa<mlir::LLVM::LLVMPointerType>(receiverType)) {
    bool isStream = false;
    if (auto *typeExpr = resolvedTypeOf(mc.receiver->span))
      isStream = typeExprStreamKind(*typeExpr) == "Stream";
    if (!isStream) {
      if (auto *ie = std::get_if<ast::ExprIdentifier>(&mc.receiver->value.kind)) {
        auto sit = streamHandleVarTypes.find(ie->name);
        isStream = (sit != streamHandleVarTypes.end() && sit->second == "Stream");
      }
    }
    if (isStream) {
      auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
      auto strType = hew::StringRefType::get(&context);
      if (method == "map" || method == "filter") {
        if (mc.args.empty()) {
          emitError(location) << "Stream::" << method << " requires a closure argument";
          return mlir::Value{};
        }
        // Determine the stream's element type to pick the right ABI.
        bool isBytesStream = false;
        if (auto *typeExpr = resolvedTypeOf(mc.receiver->span))
          isBytesStream = typeExprStreamElement(*typeExpr) == "bytes";
        // Set the expected closure type so that parameter types in unannotated
        // lambdas can be inferred.
        // For bytes streams the closure receives/returns hew.vec<i32> (bytes).
        mlir::Type paramType;
        if (isBytesStream)
          paramType = hew::VecType::get(&context, builder.getI32Type());
        else
          paramType = strType;
        mlir::Type retType;
        if (method == "map") {
          retType = paramType; // fn(T) -> T
        } else {
          // Filter predicate: fn(T) -> bool.
          // Both String and bytes filter runtime ABIs expect i32 return type
          // (StringFilterFn and BytesFilterFn both return i32).  The codegen
          // will zext the i1 comparison result to i32 when lowering the
          // closure.
          retType = builder.getI32Type();
        }
        auto expectedClosure = hew::ClosureType::get(&context, {paramType}, retType);
        pendingLambdaExpectedType = expectedClosure;
        auto closureVal = generateExpression(ast::callArgExpr(mc.args[0]).value);
        pendingLambdaExpectedType.reset();
        if (!closureVal)
          return mlir::Value{};
        auto closureType = mlir::dyn_cast<hew::ClosureType>(closureVal.getType());
        if (!closureType) {
          emitError(location) << "Stream::" << method << " argument must be a closure";
          return mlir::Value{};
        }
        // Extract the function pointer and environment pointer from the closure struct.
        auto fnPtr = hew::ClosureGetFnOp::create(builder, location, ptrType, closureVal);
        auto envPtr = hew::ClosureGetEnvOp::create(builder, location, ptrType, closureVal);
        // RC-clone env_ptr so the lazy stream adapter keeps its own reference alive.
        auto clonedEnv = hew::RcCloneOp::create(builder, location, ptrType, envPtr).getResult();
        // Dispatch to the correct runtime function based on element type.
        std::string runtimeFn;
        if (isBytesStream)
          runtimeFn = (method == "map") ? "hew_stream_map_bytes" : "hew_stream_filter_bytes";
        else
          runtimeFn = (method == "map") ? "hew_stream_map_string" : "hew_stream_filter_string";
        auto calleeAttr = mlir::SymbolRefAttr::get(&context, runtimeFn);
        auto resultStream =
            hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{ptrType}, calleeAttr,
                                       mlir::ValueRange{receiver, fnPtr, clonedEnv})
                .getResult();
        // Drop the caller's reference to the closure env (stream adapter holds its own clone).
        hew::DropOp::create(builder, location, envPtr, "hew_rc_drop", false);
        // Null RAII alloca — map/filter consume the input stream.
        if (auto *recvId = std::get_if<ast::ExprIdentifier>(&mc.receiver->value.kind))
          nullOutRaiiAlloca(recvId->name);
        return resultStream;
      }
      if (method == "take") {
        if (mc.args.empty()) {
          emitError(location) << "Stream::take requires a count argument";
          return mlir::Value{};
        }
        auto countVal = generateExpression(ast::callArgExpr(mc.args[0]).value);
        if (!countVal)
          return mlir::Value{};
        countVal = coerceType(countVal, i64Type, location);
        auto calleeAttr = mlir::SymbolRefAttr::get(&context, "hew_stream_take");
        auto result = hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{ptrType},
                                                  calleeAttr,
                                                  mlir::ValueRange{receiver, countVal})
            .getResult();
        if (auto *recvId = std::get_if<ast::ExprIdentifier>(&mc.receiver->value.kind))
          nullOutRaiiAlloca(recvId->name);
        return result;
      }
    }
  }

  if (method == "clone") {
    // Rc<T>.clone() must route to RcCloneOp (handled later in the Rc
    // methods section), not the String clone path.
    bool receiverIsRc = false;
    if (auto *typeExpr = resolvedTypeOf(mc.receiver->span)) {
      if (auto *named = std::get_if<ast::TypeNamed>(&typeExpr->kind))
        receiverIsRc = (named->name == "Rc");
    }
    if (!receiverIsRc) {
      return hew::StringMethodOp::create(builder, location, hew::StringRefType::get(&context),
                                         builder.getStringAttr("clone"), receiver, mlir::ValueRange{})
          .getResult();
    }
  }
  if (method == "trim") {
    return hew::StringMethodOp::create(builder, location, hew::StringRefType::get(&context),
                                       builder.getStringAttr("trim"), receiver, mlir::ValueRange{})
        .getResult();
  }
  if (method == "to_lower" || method == "to_lowercase") {
    return hew::StringMethodOp::create(builder, location, hew::StringRefType::get(&context),
                                       builder.getStringAttr("to_lowercase"), receiver,
                                       mlir::ValueRange{})
        .getResult();
  }
  if (method == "to_upper" || method == "to_uppercase") {
    return hew::StringMethodOp::create(builder, location, hew::StringRefType::get(&context),
                                       builder.getStringAttr("to_uppercase"), receiver,
                                       mlir::ValueRange{})
        .getResult();
  }
  if (method == "replace") {
    auto old_s = generateExpression(ast::callArgExpr(mc.args[0]).value);
    auto new_s = generateExpression(ast::callArgExpr(mc.args[1]).value);
    if (!old_s || !new_s)
      return mlir::Value{};
    return hew::StringMethodOp::create(builder, location, hew::StringRefType::get(&context),
                                       builder.getStringAttr("replace"), receiver,
                                       mlir::ValueRange{old_s, new_s})
        .getResult();
  }
  if (method == "slice") {
    auto start = generateExpression(ast::callArgExpr(mc.args[0]).value);
    auto end = generateExpression(ast::callArgExpr(mc.args[1]).value);
    if (!start || !end)
      return mlir::Value{};
    start = coerceType(start, i32Type, location);
    end = coerceType(end, i32Type, location);
    return hew::StringMethodOp::create(builder, location, hew::StringRefType::get(&context),
                                       builder.getStringAttr("slice"), receiver,
                                       mlir::ValueRange{start, end})
        .getResult();
  }
  if (method == "repeat") {
    auto n = generateExpression(ast::callArgExpr(mc.args[0]).value);
    if (!n)
      return mlir::Value{};
    n = coerceType(n, i32Type, location);
    return hew::StringMethodOp::create(builder, location, hew::StringRefType::get(&context),
                                       builder.getStringAttr("repeat"), receiver,
                                       mlir::ValueRange{n})
        .getResult();
  }
  if (method == "char_at") {
    auto idx = generateExpression(ast::callArgExpr(mc.args[0]).value);
    if (!idx)
      return mlir::Value{};
    idx = coerceType(idx, i32Type, location);
    return hew::StringMethodOp::create(builder, location, i32Type,
                                       builder.getStringAttr("char_at"), receiver,
                                       mlir::ValueRange{idx})
        .getResult();
  }
  if (method == "split") {
    auto sep = generateExpression(ast::callArgExpr(mc.args[0]).value);
    if (!sep)
      return mlir::Value{};
    return hew::StringMethodOp::create(builder, location, hew::StringRefType::get(&context),
                                       builder.getStringAttr("split"), receiver,
                                       mlir::ValueRange{sep})
        .getResult();
  }
  if (method == "lines") {
    auto strType = hew::StringRefType::get(&context);
    auto vecType = hew::VecType::get(&context, strType);
    auto calleeAttr = mlir::SymbolRefAttr::get(&context, "hew_string_lines");
    return builder
        .create<hew::RuntimeCallOp>(location, mlir::TypeRange{vecType}, calleeAttr,
                                    mlir::ValueRange{receiver})
        .getResult();
  }
  if (method == "chars") {
    auto vecType = hew::VecType::get(&context, i32Type);
    auto calleeAttr = mlir::SymbolRefAttr::get(&context, "hew_string_chars");
    return builder
        .create<hew::RuntimeCallOp>(location, mlir::TypeRange{vecType}, calleeAttr,
                                    mlir::ValueRange{receiver})
        .getResult();
  }
  if (method == "is_digit" || method == "is_alpha" || method == "is_alphanumeric" ||
      method == "is_empty") {
    return builder
        .create<hew::StringMethodOp>(location, builder.getI1Type(), builder.getStringAttr(method),
                                     receiver, mlir::ValueRange{})
        .getResult();
  }
  if (method == "contains") {
    auto sub = generateExpression(ast::callArgExpr(mc.args[0]).value);
    if (!sub)
      return mlir::Value{};
    return hew::StringMethodOp::create(builder, location, builder.getI1Type(),
                                       builder.getStringAttr("contains"), receiver,
                                       mlir::ValueRange{sub})
        .getResult();
  }
  if (method == "starts_with") {
    auto prefix = generateExpression(ast::callArgExpr(mc.args[0]).value);
    if (!prefix)
      return mlir::Value{};
    return hew::StringMethodOp::create(builder, location, builder.getI1Type(),
                                       builder.getStringAttr("starts_with"), receiver,
                                       mlir::ValueRange{prefix})
        .getResult();
  }
  if (method == "ends_with") {
    auto suffix = generateExpression(ast::callArgExpr(mc.args[0]).value);
    if (!suffix)
      return mlir::Value{};
    return hew::StringMethodOp::create(builder, location, builder.getI1Type(),
                                       builder.getStringAttr("ends_with"), receiver,
                                       mlir::ValueRange{suffix})
        .getResult();
  }
  if (method == "len") {
    return hew::StringMethodOp::create(builder, location, i32Type, builder.getStringAttr("length"),
                                       receiver, mlir::ValueRange{})
        .getResult();
  }
  if (method == "find") {
    auto sub = generateExpression(ast::callArgExpr(mc.args[0]).value);
    if (!sub)
      return mlir::Value{};
    return hew::StringMethodOp::create(builder, location, i32Type, builder.getStringAttr("find"),
                                       receiver, mlir::ValueRange{sub})
        .getResult();
  }
  if (method == "index_of") {
    auto sub = generateExpression(ast::callArgExpr(mc.args[0]).value);
    if (!sub)
      return mlir::Value{};
    auto startIdx = createIntConstant(builder, location, i32Type, 0);
    return hew::StringMethodOp::create(builder, location, i32Type,
                                       builder.getStringAttr("index_of"), receiver,
                                       mlir::ValueRange{sub, startIdx})
        .getResult();
  }

  // --- Duration methods (inline arith ops on i64 nanosecond values) ---
  // Duration maps to i64 at the MLIR level; use resolvedTypeOf to distinguish
  // from plain i64 integer methods.
  {
    bool isDuration = false;
    if (auto *typeExpr = resolvedTypeOf(mc.receiver->span)) {
      if (auto *named = std::get_if<ast::TypeNamed>(&typeExpr->kind))
        isDuration = (named->name == "duration");
    }
    if (isDuration && receiverType.isInteger(64)) {
      if (method == "nanos") {
        return receiver;
      }
      if (method == "micros") {
        auto divisor = mlir::arith::ConstantOp::create(builder, location, i64Type,
                                                       builder.getI64IntegerAttr(1000LL));
        return mlir::arith::DivSIOp::create(builder, location, receiver, divisor).getResult();
      }
      if (method == "millis") {
        auto divisor = mlir::arith::ConstantOp::create(builder, location, i64Type,
                                                       builder.getI64IntegerAttr(1000000LL));
        return mlir::arith::DivSIOp::create(builder, location, receiver, divisor).getResult();
      }
      if (method == "secs") {
        auto divisor = mlir::arith::ConstantOp::create(builder, location, i64Type,
                                                       builder.getI64IntegerAttr(1000000000LL));
        return mlir::arith::DivSIOp::create(builder, location, receiver, divisor).getResult();
      }
      if (method == "mins") {
        auto divisor = mlir::arith::ConstantOp::create(builder, location, i64Type,
                                                       builder.getI64IntegerAttr(60000000000LL));
        return mlir::arith::DivSIOp::create(builder, location, receiver, divisor).getResult();
      }
      if (method == "hours") {
        auto divisor = mlir::arith::ConstantOp::create(builder, location, i64Type,
                                                       builder.getI64IntegerAttr(3600000000000LL));
        return mlir::arith::DivSIOp::create(builder, location, receiver, divisor).getResult();
      }
      if (method == "abs") {
        auto zero = mlir::arith::ConstantOp::create(builder, location, i64Type,
                                                    builder.getI64IntegerAttr(0));
        auto isNeg = mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::slt,
                                                 receiver, zero);
        auto neg = mlir::arith::SubIOp::create(builder, location, zero, receiver);
        return mlir::arith::SelectOp::create(builder, location, isNeg, neg, receiver).getResult();
      }
      if (method == "is_zero") {
        auto zero = mlir::arith::ConstantOp::create(builder, location, i64Type,
                                                    builder.getI64IntegerAttr(0));
        return mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::eq,
                                           receiver, zero)
            .getResult();
      }
    }
  }

  // --- Numeric type conversion methods (.to_i8, .to_i16, .to_i32, .to_i64,
  //     .to_u8, .to_u16, .to_u32, .to_u64, .to_f32, .to_f64,
  //     .to_isize, .to_usize) ---
  // These are spec §10.1 compiler intrinsics on all numeric types.
  {
    mlir::Type targetType = nullptr;
    bool isUnsigned = false;

    if (method == "to_i8") {
      targetType = builder.getIntegerType(8);
    } else if (method == "to_i16") {
      targetType = builder.getIntegerType(16);
    } else if (method == "to_i32") {
      targetType = builder.getIntegerType(32);
    } else if (method == "to_i64") {
      targetType = builder.getIntegerType(64);
    } else if (method == "to_u8") {
      targetType = builder.getIntegerType(8);
      isUnsigned = true;
    } else if (method == "to_u16") {
      targetType = builder.getIntegerType(16);
      isUnsigned = true;
    } else if (method == "to_u32") {
      targetType = builder.getIntegerType(32);
      isUnsigned = true;
    } else if (method == "to_u64") {
      targetType = builder.getIntegerType(64);
      isUnsigned = true;
    } else if (method == "to_f32") {
      targetType = builder.getF32Type();
    } else if (method == "to_f64") {
      targetType = builder.getF64Type();
    } else if (method == "to_isize") {
      targetType = builder.getIntegerType(isWasm32_ ? 32 : 64);
    } else if (method == "to_usize") {
      targetType = builder.getIntegerType(isWasm32_ ? 32 : 64);
      isUnsigned = true;
    }

    if (targetType) {
      bool srcIsInt = llvm::isa<mlir::IntegerType>(receiverType);
      bool srcIsFloat = llvm::isa<mlir::FloatType>(receiverType);
      if (srcIsInt || srcIsFloat) {
        // Use the same CastOp infrastructure as coerceType
        if (receiverType == targetType)
          return receiver; // no-op cast
        auto castOp = hew::CastOp::create(builder, location, targetType, receiver);
        if (isUnsigned)
          castOp->setAttr("is_unsigned", builder.getBoolAttr(true));
        return castOp.getResult();
      }
    }
  }

  // --- Rc<T> methods ---
  // Detect via resolvedTypeOf returning TypeNamed { name: "Rc", ... }.
  // The MLIR type is LLVMPointerType (same as Stream/Sender) so we cannot
  // use the MLIR type alone; we always check the type-checker annotation.
  {
    bool isRc = false;
    const ast::TypeNamed *rcNamed = nullptr;
    if (auto *typeExpr = resolvedTypeOf(mc.receiver->span)) {
      if (auto *named = std::get_if<ast::TypeNamed>(&typeExpr->kind)) {
        if (named->name == "Rc") {
          isRc = true;
          rcNamed = named;
        }
      }
    }
    if (isRc && mlir::isa<mlir::LLVM::LLVMPointerType>(receiverType)) {
      auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
      auto i64Type = builder.getI64Type();

      if (method == "clone") {
        // Increment reference count and return a new handle pointing to the same data.
        return hew::RcCloneOp::create(builder, location, ptrType, receiver).getResult();
      }

      if (method == "get") {
        // Bitwise load of the inner value from the Rc data region.
        // The type-checker enforces T: Copy on `rc.get()`, so the LoadOp
        // duplicates a value with no ownership — no double-free possible.
        mlir::Type innerMlirType;
        if (rcNamed && rcNamed->type_args && !rcNamed->type_args->empty()) {
          innerMlirType = convertType((*rcNamed->type_args)[0].value);
        }
        if (!innerMlirType) {
          emitError(location) << "Rc::get: cannot determine inner type";
          return mlir::Value{};
        }
        return mlir::LLVM::LoadOp::create(builder, location, innerMlirType, receiver).getResult();
      }

      if (method == "strong_count") {
        // Call hew_rc_count(ptr) -> u32, then zero-extend to i64.
        // hew_rc_count returns an unsigned refcount; use ExtUIOp to avoid
        // sign-extending counts >= 2^31 into negative i64 values.
        auto i32Type = builder.getI32Type();
        auto countFuncType = builder.getFunctionType({ptrType}, {i32Type});
        auto countFunc = getOrCreateExternFunc("hew_rc_count", countFuncType);
        auto count =
            mlir::func::CallOp::create(builder, location, countFunc, mlir::ValueRange{receiver})
                .getResult(0);
        return mlir::arith::ExtUIOp::create(builder, location, i64Type, count).getResult();
      }

      emitError(location) << "unknown method '" << method << "' on Rc<T>";
      return mlir::Value{};
    }
  }

  return std::nullopt;
}

// ============================================================================
// Method call generation
// ============================================================================

/// Dispatch a module-qualified method call (math.exp, random.seed, etc.).
/// Returns the result value if the call was handled, or std::nullopt if
/// the receiver is not a known module/struct name.
std::optional<mlir::Value> MLIRGen::generateModuleMethodCall(
    const ast::ExprMethodCall &mc, const ast::ExprIdentifier &ident,
    mlir::Location location) {
  const auto &methodName = mc.method;

  // Special handling for log (has custom emit logic).
  if (ident.name == "log") {
    return generateLogCall(mc);
  }

  // std::math module — emit LLVM math intrinsics directly
  // No import required: math.exp(x), math.log(x), etc. are always available.
  if (ident.name == "math") {
    if (mc.args.empty()) {
      // Constants: math.pi, math.e
      if (methodName == "pi")
        return mlir::arith::ConstantOp::create(builder, location,
                                                builder.getF64FloatAttr(3.14159265358979323846))
            .getResult();
      if (methodName == "e")
        return mlir::arith::ConstantOp::create(builder, location,
                                                builder.getF64FloatAttr(2.71828182845904523536))
            .getResult();
      emitError(location) << "unknown math constant: math." << methodName;
      return nullptr;
    }

    auto arg = generateExpression(ast::callArgExpr(mc.args[0]).value);
    if (!arg)
      return nullptr;
    auto f64Type = builder.getF64Type();
    if (arg.getType() != f64Type)
      arg = coerceType(arg, f64Type, location);

    // Single-argument math functions → LLVM intrinsics
    if (methodName == "exp")
      return mlir::math::ExpOp::create(builder, location, arg).getResult();
    if (methodName == "log")
      return mlir::math::LogOp::create(builder, location, arg).getResult();
    if (methodName == "sqrt")
      return mlir::math::SqrtOp::create(builder, location, arg).getResult();
    if (methodName == "sin")
      return mlir::math::SinOp::create(builder, location, arg).getResult();
    if (methodName == "cos")
      return mlir::math::CosOp::create(builder, location, arg).getResult();
    if (methodName == "floor")
      return mlir::math::FloorOp::create(builder, location, arg).getResult();
    if (methodName == "ceil")
      return mlir::math::CeilOp::create(builder, location, arg).getResult();
    if (methodName == "abs")
      return mlir::math::AbsFOp::create(builder, location, arg).getResult();
    if (methodName == "abs_f")
      return mlir::math::AbsFOp::create(builder, location, arg).getResult();
    if (methodName == "round")
      return mlir::math::RoundOp::create(builder, location, arg).getResult();
    if (methodName == "tanh")
      return mlir::math::TanhOp::create(builder, location, arg).getResult();
    if (methodName == "log2")
      return mlir::math::Log2Op::create(builder, location, arg).getResult();
    if (methodName == "log10")
      return mlir::math::Log10Op::create(builder, location, arg).getResult();
    if (methodName == "exp2")
      return mlir::math::Exp2Op::create(builder, location, arg).getResult();

    // Two-argument: math.pow(base, exp)
    if (methodName == "pow") {
      if (mc.args.size() < 2) {
        emitError(location) << "math.pow requires 2 arguments";
        return nullptr;
      }
      auto arg2 = generateExpression(ast::callArgExpr(mc.args[1]).value);
      if (!arg2)
        return nullptr;
      if (arg2.getType() != f64Type)
        arg2 = coerceType(arg2, f64Type, location);
      return mlir::math::PowFOp::create(builder, location, arg, arg2).getResult();
    }
    // math.max(a, b), math.min(a, b)
    if (methodName == "max") {
      if (mc.args.size() < 2) {
        emitError(location) << "math.max requires 2 arguments";
        return nullptr;
      }
      auto arg2 = generateExpression(ast::callArgExpr(mc.args[1]).value);
      if (!arg2)
        return nullptr;
      if (arg2.getType() != f64Type)
        arg2 = coerceType(arg2, f64Type, location);
      return mlir::arith::MaximumFOp::create(builder, location, arg, arg2).getResult();
    }
    if (methodName == "min") {
      if (mc.args.size() < 2) {
        emitError(location) << "math.min requires 2 arguments";
        return nullptr;
      }
      auto arg2 = generateExpression(ast::callArgExpr(mc.args[1]).value);
      if (!arg2)
        return nullptr;
      if (arg2.getType() != f64Type)
        arg2 = coerceType(arg2, f64Type, location);
      return mlir::arith::MinimumFOp::create(builder, location, arg, arg2).getResult();
    }

    // math.sign(x) — returns -1, 0, or 1 (integer)
    if (methodName == "sign") {
      auto i64Type = builder.getI64Type();
      // If arg is f64, convert to i64 for integer sign
      if (arg.getType() == f64Type)
        arg = mlir::arith::FPToSIOp::create(builder, location, i64Type, arg).getResult();
      auto zero = mlir::arith::ConstantOp::create(builder, location, i64Type,
                                                    builder.getI64IntegerAttr(0));
      auto negOne = mlir::arith::ConstantOp::create(builder, location, i64Type,
                                                      builder.getI64IntegerAttr(-1));
      auto posOne = mlir::arith::ConstantOp::create(builder, location, i64Type,
                                                      builder.getI64IntegerAttr(1));
      auto isNeg = mlir::arith::CmpIOp::create(builder, location,
                                                  mlir::arith::CmpIPredicate::slt, arg, zero);
      auto isPos = mlir::arith::CmpIOp::create(builder, location,
                                                  mlir::arith::CmpIPredicate::sgt, arg, zero);
      auto selPos = mlir::arith::SelectOp::create(builder, location, isPos, posOne, zero);
      return mlir::arith::SelectOp::create(builder, location, isNeg, negOne, selPos).getResult();
    }

    // math.clamp(x, lo, hi) — clamp x to [lo, hi]
    if (methodName == "clamp") {
      if (mc.args.size() < 3) {
        emitError(location) << "math.clamp requires 3 arguments";
        return nullptr;
      }
      auto lo = generateExpression(ast::callArgExpr(mc.args[1]).value);
      auto hi = generateExpression(ast::callArgExpr(mc.args[2]).value);
      if (!lo || !hi)
        return nullptr;
      auto i64Type = builder.getI64Type();
      if (arg.getType() == f64Type)
        arg = mlir::arith::FPToSIOp::create(builder, location, i64Type, arg).getResult();
      if (lo.getType() == f64Type)
        lo = mlir::arith::FPToSIOp::create(builder, location, i64Type, lo).getResult();
      if (hi.getType() == f64Type)
        hi = mlir::arith::FPToSIOp::create(builder, location, i64Type, hi).getResult();
      if (lo.getType() != i64Type)
        lo = coerceType(lo, i64Type, location);
      if (hi.getType() != i64Type)
        hi = coerceType(hi, i64Type, location);
      if (arg.getType() != i64Type)
        arg = coerceType(arg, i64Type, location);
      // clamp = max(lo, min(x, hi))
      auto minXHi = mlir::arith::MinSIOp::create(builder, location, arg, hi);
      return mlir::arith::MaxSIOp::create(builder, location, lo, minXHi).getResult();
    }

    emitError(location) << "unknown math function: math." << methodName;
    return nullptr;
  }

  // std::random module — route to hew_random_* runtime functions
  if (ident.name == "random") {
    auto f64Type = builder.getF64Type();
    auto i64Type = builder.getI64Type();

    if (methodName == "seed") {
      auto arg = generateExpression(ast::callArgExpr(mc.args[0]).value);
      if (!arg)
        return nullptr;
      if (arg.getType() != i64Type)
        arg = coerceType(arg, i64Type, location);
      emitRuntimeCall("hew_random_seed", {}, {arg}, location);
      return nullptr;
    }
    if (methodName == "random") {
      return emitRuntimeCall("hew_random_random", f64Type, {}, location);
    }
    if (methodName == "gauss") {
      auto mu = generateExpression(ast::callArgExpr(mc.args[0]).value);
      auto sigma = generateExpression(ast::callArgExpr(mc.args[1]).value);
      if (!mu || !sigma)
        return nullptr;
      if (mu.getType() != f64Type)
        mu = coerceType(mu, f64Type, location);
      if (sigma.getType() != f64Type)
        sigma = coerceType(sigma, f64Type, location);
      return emitRuntimeCall("hew_random_gauss", f64Type, {mu, sigma}, location);
    }
    if (methodName == "randint") {
      auto lo = generateExpression(ast::callArgExpr(mc.args[0]).value);
      auto hi = generateExpression(ast::callArgExpr(mc.args[1]).value);
      if (!lo || !hi)
        return nullptr;
      if (lo.getType() != i64Type)
        lo = coerceType(lo, i64Type, location);
      if (hi.getType() != i64Type)
        hi = coerceType(hi, i64Type, location);
      return emitRuntimeCall("hew_random_randint", i64Type, {lo, hi}, location);
    }
    if (methodName == "shuffle") {
      auto vec = generateExpression(ast::callArgExpr(mc.args[0]).value);
      if (!vec)
        return nullptr;
      emitRuntimeCall("hew_random_shuffle_i64", {}, {vec}, location);
      return nullptr;
    }
    if (methodName == "choices") {
      auto cumWeights = generateExpression(ast::callArgExpr(mc.args[0]).value);
      auto total = generateExpression(ast::callArgExpr(mc.args[1]).value);
      auto n = generateExpression(ast::callArgExpr(mc.args[2]).value);
      if (!cumWeights || !total || !n)
        return nullptr;
      return emitRuntimeCall("hew_random_choices_vec", i64Type, {cumWeights, total, n}, location);
    }
    emitError(location) << "unknown random function: random." << methodName;
    return nullptr;
  }

  // General module-qualified call: string.from_int(), crypto.sha256(), etc.
  auto modIt = moduleNameToPath.find(ident.name);
  if (modIt != moduleNameToPath.end()) {
    const auto &modulePath = modIt->second;
    std::string mangledFunc = mangleName(modulePath, "", methodName);
    auto callee = module.lookupSymbol<mlir::func::FuncOp>(mangledFunc);
    if (!callee) {
      emitError(location) << "undefined function '" << ident.name << "." << methodName
                          << "' (mangled: " << mangledFunc << ")";
      return nullptr;
    }

    llvm::SmallVector<mlir::Value, 4> args;
    auto calleeFuncType = callee.getFunctionType();
    for (size_t i = 0; i < mc.args.size(); ++i) {
      const auto &arg = mc.args[i];
      auto val = generateExpression(ast::callArgExpr(arg).value);
      if (!val)
        return nullptr;
      materializeTemporary(val, ast::callArgExpr(arg).value);
      if (i < calleeFuncType.getNumInputs()) {
        auto expectedType = calleeFuncType.getInput(i);
        if (val.getType() != expectedType)
          val = coerceType(val, expectedType, location);
      }
      args.push_back(val);
    }

    auto callOp = mlir::func::CallOp::create(builder, location, callee, args);
    if (callOp.getNumResults() > 0)
      return callOp.getResult(0);
    return nullptr;
  }

  // Wire/static helper methods: Point.from_json(json), Status.from_yaml(str),
  // etc. When the receiver is an identifier naming a known type (not a
  // variable or module), look up the mangled static method and call it.
  if (structTypes.count(ident.name) || enumTypes.count(ident.name)) {
    std::string funcName = mangleName(currentModulePath, ident.name, methodName);
    auto callee = module.lookupSymbol<mlir::func::FuncOp>(funcName);
    if (!callee)
      callee = lookupImportedFunc(ident.name, methodName);

    // Demand-gated struct encode/decode: generate the specific wrapper now if
    // the struct is eligible and the callee has not been emitted yet.
    if (!callee && encodeEligibleStructs_.count(ident.name)) {
      // clang-format off
      static const std::unordered_map<std::string, std::pair<bool, std::string>>
          kStructSerialMethods = {
            {"to_json",   {true,  "json"}},
            {"from_json", {false, "json"}},
            {"to_yaml",   {true,  "yaml"}},
            {"from_yaml", {false, "yaml"}},
            {"to_toml",   {true,  "toml"}},
            {"from_toml", {false, "toml"}},
          };
      // clang-format on
      auto it = kStructSerialMethods.find(methodName);
      if (it != kStructSerialMethods.end()) {
        const auto &[isTo, format] = it->second;
        if (isTo)
          generateStructToSerial(ident.name, format);
        else
          generateStructFromSerial(ident.name, format);
        callee = module.lookupSymbol<mlir::func::FuncOp>(funcName);
      }
    }

    if (callee) {
      llvm::SmallVector<mlir::Value, 4> args;
      auto calleeFuncType = callee.getFunctionType();
      for (size_t i = 0; i < mc.args.size(); ++i) {
        auto val = generateExpression(ast::callArgExpr(mc.args[i]).value);
        if (!val)
          return nullptr;
        materializeTemporary(val, ast::callArgExpr(mc.args[i]).value);
        if (i < calleeFuncType.getNumInputs()) {
          auto expectedType = calleeFuncType.getInput(i);
          if (val.getType() != expectedType)
            val = coerceType(val, expectedType, location);
        }
        args.push_back(val);
      }
      auto callOp = mlir::func::CallOp::create(builder, location, callee, args);
      if (callOp.getNumResults() > 0)
        return callOp.getResult(0);
      return nullptr;
    }
  }

  return std::nullopt;
}

/// Dispatch a method call on a typed handle (http.Server, net.Connection, etc.)
/// or an i32-typed handle. Returns the result if handled, std::nullopt otherwise.
std::optional<mlir::Value> MLIRGen::generateHandleMethodCall(
    const ast::ExprMethodCall &mc, mlir::Value receiver,
    mlir::Location location) {
  const auto &methodName = mc.method;

  // Local wrapper around member emitRuntimeCall, capturing `location`.
  auto rtCall = [&](llvm::StringRef callee, mlir::Type resultType,
                    mlir::ValueRange args) -> mlir::Value {
    return emitRuntimeCall(callee, resultType, args, location);
  };

  // Check if receiver is a typed handle (http.Server, net.Connection, etc.)
  if (auto handleTy = mlir::dyn_cast<hew::HandleType>(receiver.getType())) {
    const auto handleType = handleTy.getHandleKind().str();
    const auto &method = methodName;
    auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
    auto i32Type = builder.getI32Type();

    // Generate argument values
    llvm::SmallVector<mlir::Value, 4> argVals;
    argVals.push_back(receiver);
    for (const auto &arg : mc.args) {
      auto val = generateExpression(ast::callArgExpr(arg).value);
      if (!val)
        return nullptr;
      argVals.push_back(val);
    }

    // http.Server methods
    if (handleType == "http.Server") {
      if (method == "accept")
        return rtCall("hew_http_server_recv", hew::HandleType::get(&context, "http.Request"),
                      argVals);
      if (method == "close") {
        rtCall("hew_http_server_close", {}, argVals);
        return nullptr;
      }
    }

    // http.Request methods
    if (handleType == "http.Request") {
      if (method == "path")
        return rtCall("hew_http_request_path", hew::StringRefType::get(&context), {receiver});
      if (method == "method")
        return rtCall("hew_http_request_method", hew::StringRefType::get(&context), {receiver});
      if (method == "body")
        return rtCall("hew_http_request_body_string", hew::StringRefType::get(&context), argVals);
      if (method == "header")
        return rtCall("hew_http_request_header", hew::StringRefType::get(&context), argVals);
      if (method == "respond")
        return rtCall("hew_http_respond_bridge", i32Type, argVals);
      if (method == "respond_text")
        return rtCall("hew_http_respond_text", i32Type, argVals);
      if (method == "respond_json")
        return rtCall("hew_http_respond_json", i32Type, argVals);
      if (method == "respond_stream")
        return rtCall("hew_http_respond_stream", ptrType, argVals);
      if (method == "free") {
        rtCall("hew_http_request_free", {}, argVals);
        return nullptr;
      }
    }

    // net.Listener methods
    if (handleType == "net.Listener") {
      if (method == "accept")
        return rtCall("hew_tcp_accept", i32Type, argVals);
      if (method == "close")
        return rtCall("hew_tcp_close", i32Type, argVals);
    }

    // net.Connection methods
    if (handleType == "net.Connection") {
      if (method == "read")
        return rtCall("hew_tcp_read", ptrType, argVals);
      if (method == "write")
        return rtCall("hew_tcp_write", i32Type, argVals);
      if (method == "close")
        return rtCall("hew_tcp_close", i32Type, argVals);
      if (method == "set_read_timeout")
        return rtCall("hew_tcp_set_read_timeout", i32Type, argVals);
      if (method == "set_write_timeout")
        return rtCall("hew_tcp_set_write_timeout", i32Type, argVals);
    }

    // regex.Pattern methods
    if (handleType == "regex.Pattern") {
      if (method == "is_match")
        return hew::RegexIsMatchOp::create(builder, location, builder.getI1Type(), argVals[0],
                                            argVals[1]);
      if (method == "find")
        return hew::RegexFindOp::create(builder, location, hew::StringRefType::get(&context),
                                         argVals[0], argVals[1]);
      if (method == "replace")
        return hew::RegexReplaceOp::create(builder, location, hew::StringRefType::get(&context),
                                            argVals[0], argVals[1], argVals[2]);
      if (method == "free") {
        hew::RegexFreeOp::create(builder, location, argVals[0]);
        return nullptr;
      }
    }

    // process.Child methods
    if (handleType == "process.Child") {
      if (method == "wait")
        return rtCall("hew_process_wait", i32Type, argVals);
      if (method == "kill")
        return rtCall("hew_process_kill", i32Type, argVals);
    }
  }

  // Check if receiver is an i32-typed handle (net.Listener, net.Connection)
  // These types map to i32 at the MLIR level but need handle method dispatch.
  auto receiverType = receiver.getType();
  if (receiverType.isInteger(32)) {
    auto normalizeHandleType = [](std::string typeName) {
      if (typeName == "Listener")
        return std::string("net.Listener");
      if (typeName == "Connection")
        return std::string("net.Connection");
      return typeName;
    };

    std::string handleType;
    // Prefer resolved type from the type checker
    if (auto *typeExpr = resolvedTypeOf(mc.receiver->span))
      handleType = typeExprToHandleString(*typeExpr, knownHandleTypes);
    // Fall back to identifier-based map lookup
    if (handleType.empty()) {
      if (auto *ie = std::get_if<ast::ExprIdentifier>(&mc.receiver->value.kind)) {
        auto hit = handleVarTypes.find(ie->name);
        if (hit != handleVarTypes.end())
          handleType = hit->second;
      }
    }
    handleType = normalizeHandleType(handleType);
    // Check field access (bare field name as receiver, e.g. conn.method())
    if (handleType.empty() && !currentActorName.empty()) {
      // Bare field name as receiver (e.g. conn.method())
      if (handleType.empty()) {
        if (auto *ie = std::get_if<ast::ExprIdentifier>(&mc.receiver->value.kind)) {
          auto key = currentActorName + "." + ie->name;
          auto aft = actorFieldTypes.find(key);
          if (aft != actorFieldTypes.end())
            handleType = normalizeHandleType(aft->second);
        }
      }
    }
    if (!handleType.empty()) {
      const auto &method = methodName;
      auto i32Type = builder.getI32Type();
      auto vecType = hew::VecType::get(&context, builder.getIntegerType(32));

      llvm::SmallVector<mlir::Value, 4> argVals;
      argVals.push_back(receiver);
      for (const auto &arg : mc.args) {
        auto val = generateExpression(ast::callArgExpr(arg).value);
        if (!val)
          return nullptr;
        argVals.push_back(val);
      }

      if (handleType == "net.Listener") {
        if (method == "accept")
          return rtCall("hew_tcp_accept", i32Type, argVals);
        if (method == "close")
          return rtCall("hew_tcp_close", i32Type, argVals);
      }
      if (handleType == "net.Connection") {
        if (method == "read")
          return rtCall("hew_tcp_read", vecType, argVals);
        if (method == "read_string") {
          auto bytes = rtCall("hew_tcp_read", vecType, argVals);
          return rtCall("hew_bytes_to_string", hew::StringRefType::get(&context),
                        mlir::ValueRange{bytes});
        }
        if (method == "write") {
          rtCall("hew_tcp_write", {}, argVals);
          return nullptr;
        }
        if (method == "write_string") {
          if (argVals.size() < 2) {
            emitError(location) << ".write_string() requires one argument";
            return nullptr;
          }
          auto bytes = rtCall("hew_string_to_bytes", vecType, mlir::ValueRange{argVals[1]});
          rtCall("hew_tcp_write", {}, mlir::ValueRange{receiver, bytes});
          return nullptr;
        }
        if (method == "close")
          return rtCall("hew_tcp_close", i32Type, argVals);
        if (method == "set_read_timeout")
          return rtCall("hew_tcp_set_read_timeout", i32Type, argVals);
        if (method == "set_write_timeout")
          return rtCall("hew_tcp_set_write_timeout", i32Type, argVals);
      }
    }
  }

  return std::nullopt;
}

/// Dispatch a method call on an actor reference or generator.
/// Returns the result if handled, std::nullopt otherwise.
std::optional<mlir::Value> MLIRGen::generateActorMethodCall(
    const ast::ExprMethodCall &mc, mlir::Value receiver,
    mlir::Location location) {
  const auto &methodName = mc.method;
  auto receiverType = receiver.getType();

  // Check if receiver is an actor (ptr type + tracked in actorVarTypes,
  // or i64 PID from Node::lookup for remote actors)
  if (isPointerLikeType(receiverType) || mlir::isa<mlir::IntegerType>(receiverType)) {
    std::string actorTypeName = resolveActorTypeName(mc.receiver->value, &mc.receiver->span);

    if (!actorTypeName.empty()) {
      auto actorIt = actorRegistry.find(actorTypeName);
      if (actorIt != actorRegistry.end()) {
        // If the receive handler has a return type, use ask (blocking)
        // instead of send (fire-and-forget) so the caller gets the result
        for (const auto &recv : actorIt->second.receiveFns) {
          if (recv.name == methodName && recv.returnType.has_value()) {
            return generateActorMethodAsk(receiver, actorIt->second, methodName, mc.args, location);
          }
        }
        return generateActorMethodSend(receiver, actorIt->second, methodName, mc.args, location);
      }
    }

    // Check for special methods on actor ptrs: stop(), close()
    if (methodName == "stop") {
      hew::ActorStopOp::create(builder, location, receiver);
      return nullptr;
    }
    if (methodName == "close") {
      hew::ActorCloseOp::create(builder, location, receiver);
      return nullptr;
    }

    // Fallback: .send() on an untracked actor variable
    if (methodName == "send" && isPointerLikeType(receiverType)) {
      llvm::SmallVector<mlir::Value, 4> argVals;
      for (const auto &arg : mc.args) {
        auto val = generateExpression(ast::callArgExpr(arg).value);
        if (!val)
          return nullptr;
        argVals.push_back(val);
      }
      hew::ActorSendOp::create(builder, location, receiver, builder.getI32IntegerAttr(0), argVals);
      return nullptr;
    }

    // Check for generator .next() method calls
    if (auto *recvIdent = std::get_if<ast::ExprIdentifier>(&mc.receiver->value.kind)) {
      if (methodName == "next") {
        auto git = generatorVarTypes.find(recvIdent->name);
        if (git != generatorVarTypes.end()) {
          std::string nextFuncName = git->second + "__next";
          auto nextFuncOp = module.lookupSymbol<mlir::func::FuncOp>(nextFuncName);
          if (nextFuncOp) {
            auto callResult = mlir::func::CallOp::create(builder, location, nextFuncOp,
                                                          mlir::ValueRange{receiver});
            return callResult.getResult(0);
          }
        }
      }
    }
  }

  return std::nullopt;
}

mlir::Value MLIRGen::generateMethodCall(const ast::ExprMethodCall &mc) {
  auto location = currentLoc;
  const auto &methodName = mc.method;

  // Module-qualified calls (math.exp, random.seed, log.info, etc.)
  if (auto *ident = std::get_if<ast::ExprIdentifier>(&mc.receiver->value.kind)) {
    if (auto result = generateModuleMethodCall(mc, *ident, location))
      return *result;
  }

  // Generate receiver expression
  auto receiver = generateExpression(mc.receiver->value);
  if (!receiver)
    return nullptr;

  // Handle type dispatch (http.Server, net.Connection, etc.)
  if (auto result = generateHandleMethodCall(mc, receiver, location))
    return *result;

  // Actor / generator dispatch
  if (auto result = generateActorMethodCall(mc, receiver, location))
    return *result;

  // Trait object dispatch
  {
    std::string traitName;
    // Prefer resolved type from the type checker
    if (auto *typeExpr = resolvedTypeOf(mc.receiver->span))
      traitName = typeExprTraitName(*typeExpr);
    // Fall back to identifier-based map lookup
    if (traitName.empty()) {
      if (auto *recvIdent = std::get_if<ast::ExprIdentifier>(&mc.receiver->value.kind)) {
        auto dtIt = dynTraitVarTypes.find(recvIdent->name);
        if (dtIt != dynTraitVarTypes.end())
          traitName = dtIt->second;
      }
    }
    if (!traitName.empty()) {
      auto dispIt = traitDispatchRegistry.find(traitName);
      if (dispIt == traitDispatchRegistry.end() || dispIt->second.impls.empty()) {
        emitError(location) << "no implementations for trait '" << traitName << "'";
        return nullptr;
      }

      auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);

      auto dataPtr = hew::TraitObjectDataOp::create(builder, location, ptrType, receiver);
      auto vtablePtr = hew::TraitObjectTagOp::create(builder, location, ptrType, receiver);

      llvm::SmallVector<mlir::Value, 4> extraArgs;
      for (const auto &arg : mc.args) {
        auto val = generateExpression(ast::callArgExpr(arg).value);
        if (!val)
          return nullptr;
        extraArgs.push_back(val);
      }

      auto traitIt = traitRegistry.find(traitName);
      mlir::Type returnType;
      unsigned methodIdx = 0;
      if (traitIt != traitRegistry.end()) {
        auto idxIt = traitIt->second.methodIndex.find(methodName);
        if (idxIt != traitIt->second.methodIndex.end())
          methodIdx = idxIt->second;
        for (const auto *tm : traitIt->second.methods) {
          if (tm->name == methodName && tm->return_type.has_value()) {
            returnType = convertType(tm->return_type->value);
            break;
          }
        }
      }

      auto methodIndexAttr = builder.getI64IntegerAttr(methodIdx);

      // Emit hew.trait_dispatch (vtable-based O(1) dispatch)
      if (!returnType) {
        hew::TraitDispatchOp::create(
            builder, location, mlir::Type{}, builder.getStringAttr(traitName),
            builder.getStringAttr(methodName), dataPtr, vtablePtr, extraArgs, methodIndexAttr);
        return nullptr;
      }

      return hew::TraitDispatchOp::create(
                 builder, location, returnType, builder.getStringAttr(traitName),
                 builder.getStringAttr(methodName), dataPtr, vtablePtr, extraArgs, methodIndexAttr)
          .getResult();
    }
  }

  // Builtin methods on scalars
  if (auto result = generateBuiltinMethodCall(mc, receiver, location))
    return *result;

  // Determine type name from the receiver's MLIR type (struct/enum or handle).
  auto receiverType = receiver.getType();
  auto structType = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(receiverType);
  std::string resolvedTypeName;
  if (structType && structType.isIdentified()) {
    resolvedTypeName = structType.getName().str();
  } else if (auto handleTy = mlir::dyn_cast<hew::HandleType>(receiverType)) {
    // Handle types (json.Value, ws.Conn, etc.) can have trait impl methods
    // registered under their handle kind name (e.g. "json.Value").
    resolvedTypeName = handleTy.getHandleKind().str();
  } else if (auto *typeExpr = resolvedTypeOf(mc.receiver->span)) {
    auto candidate = typeExprToTypeName(*typeExpr);
    if (!candidate.empty() && (structTypes.count(candidate) || enumTypes.count(candidate)))
      resolvedTypeName = candidate;
  }
  if (resolvedTypeName.empty()) {
    emitError(location) << "method call on non-struct/enum type"
                        << " (method='" << methodName << "'"
                        << ", receiver type: " << receiverType << ")";
    return nullptr;
  }

  // Machine step() — mutates the receiver variable in place.
  // The generated step function still returns the new machine value; the
  // call site stores it back into the receiver's mutable-variable slot.
  if (methodName == "step" && structType) {
    auto enumIt = enumTypes.find(resolvedTypeName);
    if (enumIt != enumTypes.end()) {
      std::string funcName = mangleName(currentModulePath, resolvedTypeName, "step");
      auto callee = module.lookupSymbol<mlir::func::FuncOp>(funcName);
      if (!callee)
        callee = lookupImportedFunc(resolvedTypeName, "step");
      if (callee) {
        llvm::SmallVector<mlir::Value, 4> args;
        args.push_back(receiver);
        for (const auto &arg : mc.args) {
          auto val = generateExpression(ast::callArgExpr(arg).value);
          if (!val)
            return nullptr;
          args.push_back(val);
        }
        auto ft = callee.getFunctionType();
        for (size_t i = 0; i < args.size() && i < ft.getNumInputs(); ++i) {
          if (args[i].getType() != ft.getInput(i))
            args[i] = coerceType(args[i], ft.getInput(i), location);
        }
        auto callOp = mlir::func::CallOp::create(builder, location, callee, args);
        // Store result back into the receiver variable
        if (callOp.getNumResults() > 0) {
          if (auto *ident = std::get_if<ast::ExprIdentifier>(&mc.receiver->value.kind))
            storeVariable(ident->name, callOp.getResult(0));
        }
        return nullptr; // step() returns void to the caller
      }
    }
  }

  std::string funcName = mangleName(currentModulePath, resolvedTypeName, methodName);

  llvm::SmallVector<mlir::Value, 4> args;
  args.push_back(receiver);
  for (const auto &arg : mc.args) {
    auto val = generateExpression(ast::callArgExpr(arg).value);
    if (!val)
      return nullptr;
    args.push_back(val);
  }

  auto callee = module.lookupSymbol<mlir::func::FuncOp>(funcName);
  // If not found in current module, try imported module paths.
  // This handles cross-module struct methods (e.g. bench.Suite.add defined
  // in std::bench but called from bench_basic).
  if (!callee)
    callee = lookupImportedFunc(resolvedTypeName, methodName);
  // Try specializing a generic impl method (e.g. impl<T> Box<T> { fn unwrap … }).
  if (!callee && structType) {
    auto originIt = structTypeOrigin.find(resolvedTypeName);
    if (originIt != structTypeOrigin.end()) {
      const auto &[baseName, typeArgs] = originIt->second;
      callee = specializeGenericImplMethod(baseName, typeArgs, methodName);
    }
  }
  if (!callee) {
    // Demand-gated struct encode/decode wrappers on the instance path:
    // p.to_json(), p.to_yaml(), p.to_toml(), p.from_json(...), etc.
    // Mirror the same gate used in generateModuleMethodCall for the
    // static-dispatch path (TypeName.to_json(p)).
    if (encodeEligibleStructs_.count(resolvedTypeName)) {
      static const std::unordered_map<std::string, std::pair<bool, std::string>>
          kStructSerialMethods = {
            {"to_json",   {true,  "json"}},
            {"from_json", {false, "json"}},
            {"to_yaml",   {true,  "yaml"}},
            {"from_yaml", {false, "yaml"}},
            {"to_toml",   {true,  "toml"}},
            {"from_toml", {false, "toml"}},
          };
      auto it = kStructSerialMethods.find(methodName);
      if (it != kStructSerialMethods.end()) {
        const auto &[isTo, format] = it->second;
        if (isTo)
          generateStructToSerial(resolvedTypeName, format);
        else
          generateStructFromSerial(resolvedTypeName, format);
        callee = module.lookupSymbol<mlir::func::FuncOp>(funcName);
      }
    }
  }
  if (!callee) {
    emitError(location) << "undefined method '" << methodName << "' on type '"
                        << resolvedTypeName << "'";
    return nullptr;
  }

  auto funcType = callee.getFunctionType();
  for (size_t i = 0; i < args.size() && i < funcType.getNumInputs(); ++i) {
    if (args[i].getType() != funcType.getInput(i))
      args[i] = coerceType(args[i], funcType.getInput(i), location);
  }

  auto callOp = mlir::func::CallOp::create(builder, location, callee, args);
  if (callOp.getNumResults() > 0)
    return callOp.getResult(0);
  return nullptr;
}

// ============================================================================
// Tuple expression
// ============================================================================

mlir::Value MLIRGen::generateTupleExpr(const ast::ExprTuple &tup) {
  auto location = currentLoc;

  if (tup.elements.empty()) {
    return nullptr;
  }

  llvm::SmallVector<mlir::Value, 4> values;
  llvm::SmallVector<mlir::Type, 4> types;
  for (const auto &elem : tup.elements) {
    auto val = generateExpression(elem->value);
    if (!val)
      return nullptr;
    values.push_back(val);
    types.push_back(val.getType());
  }

  auto tupleType = hew::HewTupleType::get(&context, types);
  return hew::TupleCreateOp::create(builder, location, tupleType, values);
}

// ============================================================================
// Array expression
// ============================================================================

mlir::Value MLIRGen::generateArrayExpr(const ast::ExprArray &arr) {
  auto location = currentLoc;

  if (arr.elements.empty()) {
    // Empty array literal: coerce to Vec<T> if type context expects it
    if (pendingDeclaredType && mlir::isa<hew::VecType>(*pendingDeclaredType)) {
      auto vecType = mlir::cast<hew::VecType>(*pendingDeclaredType);
      pendingDeclaredType.reset();
      return hew::VecNewOp::create(builder, location, vecType).getResult();
    }
    emitError(location) << "empty array literal without type context";
    return nullptr;
  }

  // Non-empty array with Vec target: build Vec directly, coercing each
  // element to the Vec's element type.  This avoids creating an intermediate
  // [T; N] array whose element type is inferred from the first value — which
  // fails for enum variants that may have different MLIR representations.
  if (pendingDeclaredType && mlir::isa<hew::VecType>(*pendingDeclaredType)) {
    auto vecType = mlir::cast<hew::VecType>(*pendingDeclaredType);
    pendingDeclaredType.reset();
    auto targetElemType = vecType.getElementType();
    auto vec = hew::VecNewOp::create(builder, location, vecType).getResult();
    for (const auto &elem : arr.elements) {
      auto val = generateExpression(elem->value);
      if (!val)
        return nullptr;
      val = coerceType(val, targetElemType, location);
      if (!val)
        return nullptr;
      if (val.getType() != targetElemType) {
        ++errorCount_;
        emitError(location) << "coerceType: no known conversion from " << val.getType()
                            << " to " << targetElemType;
        break;
      }
      hew::VecPushOp::create(builder, location, vec, val);
    }
    return vec;
  }

  llvm::SmallVector<mlir::Value, 8> values;
  for (const auto &elem : arr.elements) {
    auto val = generateExpression(elem->value);
    if (!val)
      return nullptr;
    values.push_back(val);
  }

  auto elemType = values[0].getType();
  for (size_t i = 1; i < values.size(); ++i) {
    values[i] = coerceType(values[i], elemType, location);
  }

  auto arrayType = hew::HewArrayType::get(&context, elemType, values.size());
  return hew::ArrayCreateOp::create(builder, location, arrayType, values);
}

// ============================================================================
// Map literal expression
// ============================================================================

mlir::Value MLIRGen::generateMapLiteralExpr(const ast::ExprMapLiteral &mapLit,
                                            const ast::Span &exprSpan) {
  auto location = currentLoc;

  // Determine HashMap type: prefer pendingDeclaredType (from let/var annotation),
  // fall back to the type inferred by the type checker (from expression type map).
  mlir::Type hmType;
  if (pendingDeclaredType && mlir::isa<hew::HashMapType>(*pendingDeclaredType)) {
    hmType = *pendingDeclaredType;
    pendingDeclaredType.reset();
  } else if (auto *resolvedType = resolvedTypeOf(exprSpan)) {
    auto resolvedMlirType = convertType(*resolvedType);
    if (mlir::isa<hew::HashMapType>(resolvedMlirType)) {
      hmType = resolvedMlirType;
    } else {
      emitError(location) << "map literal must produce a HashMap, got " << resolvedMlirType;
      return nullptr;
    }
  } else {
    emitError(location)
        << "cannot determine key/value types for map literal; add explicit type annotation";
    return nullptr;
  }

  auto hashMapType = mlir::cast<hew::HashMapType>(hmType);
  auto keyType = hashMapType.getKeyType();
  auto valueType = hashMapType.getValueType();

  // Create empty HashMap
  auto mapValue = hew::HashMapNewOp::create(builder, location, hmType).getResult();

  // Insert each entry
  for (const auto &entry : mapLit.entries) {
    auto key = generateExpression(entry.key->value);
    auto val = generateExpression(entry.value->value);
    if (!key || !val)
      return nullptr;
    key = coerceType(key, keyType, location);
    val = coerceType(val, valueType, location);
    hew::HashMapInsertOp::create(builder, location, mapValue, key, val);
  }

  return mapValue;
}

mlir::Value MLIRGen::generateArrayRepeatExpr(const ast::ExprArrayRepeat &repeat,
                                             const ast::Span &exprSpan) {
  auto location = currentLoc;

  if (!repeat.value || !repeat.count) {
    emitError(location) << "array repeat expression requires value and count";
    return nullptr;
  }

  auto countVal = generateExpression(repeat.count->value);
  if (!countVal)
    return nullptr;

  auto i64Type = builder.getI64Type();
  bool countUnsigned = false;
  if (auto *countType = resolvedTypeOf(repeat.count->span))
    countUnsigned = isUnsignedTypeExpr(*countType);
  countVal = coerceType(countVal, i64Type, location, countUnsigned);

  auto valueVal = generateExpression(repeat.value->value);
  if (!valueVal)
    return nullptr;

  mlir::Type elementType = valueVal.getType();
  hew::VecType vecType = nullptr;
  if (auto *resolvedType = resolvedTypeOf(exprSpan)) {
    auto resolvedMlirType = convertType(*resolvedType);
    if (auto resolvedVec = mlir::dyn_cast<hew::VecType>(resolvedMlirType)) {
      vecType = resolvedVec;
      elementType = resolvedVec.getElementType();
      valueVal = coerceType(valueVal, elementType, location);
    } else {
      emitError(location) << "array repeat expression must produce a Vec";
      return nullptr;
    }
  }
  if (!vecType)
    vecType = hew::VecType::get(&context, elementType);

  auto vecValue = hew::VecNewOp::create(builder, location, vecType).getResult();

  auto zero = createIntConstant(builder, location, i64Type, 0);
  auto one = createIntConstant(builder, location, i64Type, 1);
  auto loop = mlir::scf::ForOp::create(builder, location, zero, countVal, one);
  auto *body = loop.getBody();
  builder.setInsertionPoint(body->getTerminator());
  hew::VecPushOp::create(builder, location, vecValue, valueVal);
  builder.setInsertionPointAfter(loop);

  return vecValue;
}

// ============================================================================
// Free variable collection (for lambda closure capture)
// ============================================================================

static void collectPatternBindings(const ast::Pattern &pat, llvm::StringSet<> &bound) {
  if (auto *ident = std::get_if<ast::PatIdentifier>(&pat.kind)) {
    if (!ident->name.empty())
      bound.insert(ident->name);
  } else if (auto *ctor = std::get_if<ast::PatConstructor>(&pat.kind)) {
    for (const auto &nested : ctor->patterns)
      collectPatternBindings(nested->value, bound);
  } else if (auto *ps = std::get_if<ast::PatStruct>(&pat.kind)) {
    for (const auto &field : ps->fields) {
      if (field.pattern)
        collectPatternBindings(field.pattern->value, bound);
    }
  } else if (auto *tuple = std::get_if<ast::PatTuple>(&pat.kind)) {
    for (const auto &elem : tuple->elements)
      collectPatternBindings(elem->value, bound);
  } else if (auto *patOr = std::get_if<ast::PatOr>(&pat.kind)) {
    if (patOr->left)
      collectPatternBindings(patOr->left->value, bound);
    else if (patOr->right)
      collectPatternBindings(patOr->right->value, bound);
  }
}

static void addPatternBindingsToSet(const ast::Pattern &pat, std::set<std::string> &bound) {
  llvm::StringSet<> patternBindings;
  collectPatternBindings(pat, patternBindings);
  for (const auto &entry : patternBindings)
    bound.insert(entry.getKey().str());
}

void MLIRGen::collectFreeVarsInExpr(const ast::Expr &expr, const std::set<std::string> &bound,
                                    std::set<std::string> &freeVars) {
  if (auto *ie = std::get_if<ast::ExprIdentifier>(&expr.kind)) {
    if (!ie->name.empty() && bound.find(ie->name) == bound.end())
      freeVars.insert(ie->name);
  } else if (auto *bin = std::get_if<ast::ExprBinary>(&expr.kind)) {
    collectFreeVarsInExpr(bin->left->value, bound, freeVars);
    collectFreeVarsInExpr(bin->right->value, bound, freeVars);
  } else if (auto *un = std::get_if<ast::ExprUnary>(&expr.kind)) {
    collectFreeVarsInExpr(un->operand->value, bound, freeVars);
  } else if (auto *call = std::get_if<ast::ExprCall>(&expr.kind)) {
    collectFreeVarsInExpr(call->function->value, bound, freeVars);
    for (const auto &arg : call->args)
      collectFreeVarsInExpr(ast::callArgExpr(arg).value, bound, freeVars);
  } else if (auto *mc = std::get_if<ast::ExprMethodCall>(&expr.kind)) {
    collectFreeVarsInExpr(mc->receiver->value, bound, freeVars);
    for (const auto &arg : mc->args)
      collectFreeVarsInExpr(ast::callArgExpr(arg).value, bound, freeVars);
  } else if (auto *ifE = std::get_if<ast::ExprIf>(&expr.kind)) {
    collectFreeVarsInExpr(ifE->condition->value, bound, freeVars);
    if (ifE->then_block)
      collectFreeVarsInExpr(ifE->then_block->value, bound, freeVars);
    if (ifE->else_block.has_value() && *ifE->else_block)
      collectFreeVarsInExpr((*ifE->else_block)->value, bound, freeVars);
  } else if (auto *be = std::get_if<ast::ExprBlock>(&expr.kind)) {
    collectFreeVarsInBlock(be->block, bound, freeVars);
  } else if (auto *tup = std::get_if<ast::ExprTuple>(&expr.kind)) {
    for (const auto &e : tup->elements)
      collectFreeVarsInExpr(e->value, bound, freeVars);
  } else if (auto *arr = std::get_if<ast::ExprArray>(&expr.kind)) {
    for (const auto &e : arr->elements)
      collectFreeVarsInExpr(e->value, bound, freeVars);
  } else if (auto *mapLit = std::get_if<ast::ExprMapLiteral>(&expr.kind)) {
    for (const auto &entry : mapLit->entries) {
      collectFreeVarsInExpr(entry.key->value, bound, freeVars);
      collectFreeVarsInExpr(entry.value->value, bound, freeVars);
    }
  } else if (auto *pf = std::get_if<ast::ExprPostfixTry>(&expr.kind)) {
    collectFreeVarsInExpr(pf->inner->value, bound, freeVars);
  } else if (auto *fa = std::get_if<ast::ExprFieldAccess>(&expr.kind)) {
    collectFreeVarsInExpr(fa->object->value, bound, freeVars);
  } else if (auto *idx = std::get_if<ast::ExprIndex>(&expr.kind)) {
    collectFreeVarsInExpr(idx->object->value, bound, freeVars);
    collectFreeVarsInExpr(idx->index->value, bound, freeVars);
  } else if (auto *si = std::get_if<ast::ExprStructInit>(&expr.kind)) {
    for (const auto &[name, val] : si->fields)
      collectFreeVarsInExpr(val->value, bound, freeVars);
  } else if (auto *me = std::get_if<ast::ExprMatch>(&expr.kind)) {
    collectFreeVarsInExpr(me->scrutinee->value, bound, freeVars);
    for (const auto &arm : me->arms) {
      auto armBound = bound;
      addPatternBindingsToSet(arm.pattern.value, armBound);
      if (arm.guard)
        collectFreeVarsInExpr(arm.guard->value, armBound, freeVars);
      if (arm.body)
        collectFreeVarsInExpr(arm.body->value, armBound, freeVars);
    }
  } else if (auto *awaitE = std::get_if<ast::ExprAwait>(&expr.kind)) {
    collectFreeVarsInExpr(awaitE->inner->value, bound, freeVars);
  } else if (auto *sendE = std::get_if<ast::ExprSend>(&expr.kind)) {
    collectFreeVarsInExpr(sendE->target->value, bound, freeVars);
    collectFreeVarsInExpr(sendE->message->value, bound, freeVars);
  } else if (auto *interp = std::get_if<ast::ExprInterpolatedString>(&expr.kind)) {
    for (const auto &part : interp->parts) {
      if (auto *ep = std::get_if<ast::StringPartExpr>(&part)) {
        if (ep->expr)
          collectFreeVarsInExpr(ep->expr->value, bound, freeVars);
      }
    }
  } else if (auto *ile = std::get_if<ast::ExprIfLet>(&expr.kind)) {
    if (ile->expr)
      collectFreeVarsInExpr(ile->expr->value, bound, freeVars);
    auto thenBound = bound;
    addPatternBindingsToSet(ile->pattern.value, thenBound);
    collectFreeVarsInBlock(ile->body, thenBound, freeVars);
    if (ile->else_body)
      collectFreeVarsInBlock(*ile->else_body, bound, freeVars);
  } else if (auto *arep = std::get_if<ast::ExprArrayRepeat>(&expr.kind)) {
    if (arep->value)
      collectFreeVarsInExpr(arep->value->value, bound, freeVars);
    if (arep->count)
      collectFreeVarsInExpr(arep->count->value, bound, freeVars);
  } else if (auto *spawn = std::get_if<ast::ExprSpawn>(&expr.kind)) {
    if (spawn->target)
      collectFreeVarsInExpr(spawn->target->value, bound, freeVars);
    for (const auto &[name, arg] : spawn->args) {
      (void)name;
      if (arg)
        collectFreeVarsInExpr(arg->value, bound, freeVars);
    }
  } else if (auto *spawnLambda = std::get_if<ast::ExprSpawnLambdaActor>(&expr.kind)) {
    auto lambdaBound = bound;
    for (const auto &param : spawnLambda->params)
      lambdaBound.insert(param.name);
    lambdaBound.insert("self");
    lambdaBound.insert("println_int");
    lambdaBound.insert("println_str");
    lambdaBound.insert("print_int");
    lambdaBound.insert("print_str");
    if (spawnLambda->body)
      collectFreeVarsInExpr(spawnLambda->body->value, lambdaBound, freeVars);
  } else if (auto *scope = std::get_if<ast::ExprScope>(&expr.kind)) {
    auto scopeBound = bound;
    if (scope->binding)
      scopeBound.insert(*scope->binding);
    collectFreeVarsInBlock(scope->block, scopeBound, freeVars);
  } else if (auto *scopeLaunch = std::get_if<ast::ExprScopeLaunch>(&expr.kind)) {
    collectFreeVarsInBlock(scopeLaunch->block, bound, freeVars);
  } else if (auto *scopeSpawn = std::get_if<ast::ExprScopeSpawn>(&expr.kind)) {
    collectFreeVarsInBlock(scopeSpawn->block, bound, freeVars);
  } else if (auto *selectE = std::get_if<ast::ExprSelect>(&expr.kind)) {
    for (const auto &arm : selectE->arms) {
      if (arm.source)
        collectFreeVarsInExpr(arm.source->value, bound, freeVars);
      auto armBound = bound;
      addPatternBindingsToSet(arm.binding.value, armBound);
      if (arm.body)
        collectFreeVarsInExpr(arm.body->value, armBound, freeVars);
    }
    if (selectE->timeout && *selectE->timeout) {
      const auto &timeout = **selectE->timeout;
      if (timeout.duration)
        collectFreeVarsInExpr(timeout.duration->value, bound, freeVars);
      if (timeout.body)
        collectFreeVarsInExpr(timeout.body->value, bound, freeVars);
    }
  } else if (auto *join = std::get_if<ast::ExprJoin>(&expr.kind)) {
    for (const auto &e : join->exprs)
      if (e)
        collectFreeVarsInExpr(e->value, bound, freeVars);
  } else if (auto *range = std::get_if<ast::ExprRange>(&expr.kind)) {
    if (range->start && *range->start)
      collectFreeVarsInExpr((*range->start)->value, bound, freeVars);
    if (range->end && *range->end)
      collectFreeVarsInExpr((*range->end)->value, bound, freeVars);
  } else if (auto *timeout = std::get_if<ast::ExprTimeout>(&expr.kind)) {
    if (timeout->expr)
      collectFreeVarsInExpr(timeout->expr->value, bound, freeVars);
    if (timeout->duration)
      collectFreeVarsInExpr(timeout->duration->value, bound, freeVars);
  } else if (auto *yieldE = std::get_if<ast::ExprYield>(&expr.kind)) {
    if (yieldE->value && *yieldE->value)
      collectFreeVarsInExpr((*yieldE->value)->value, bound, freeVars);
  } else if (auto *unsafeE = std::get_if<ast::ExprUnsafe>(&expr.kind)) {
    collectFreeVarsInBlock(unsafeE->block, bound, freeVars);
  }
  // Literal, Lambda (nested), ScopeCancel, Regex literal, Cooperate, etc. — skip
}

void MLIRGen::collectFreeVarsInStmt(const ast::Stmt &stmt, std::set<std::string> &bound,
                                    std::set<std::string> &freeVars) {
  if (auto *ls = std::get_if<ast::StmtLet>(&stmt.kind)) {
    if (ls->value.has_value())
      collectFreeVarsInExpr(ls->value->value, bound, freeVars);
    // After evaluating the RHS, bind the pattern names
    addPatternBindingsToSet(ls->pattern.value, bound);
  } else if (auto *vs = std::get_if<ast::StmtVar>(&stmt.kind)) {
    if (vs->value.has_value())
      collectFreeVarsInExpr(vs->value->value, bound, freeVars);
    bound.insert(vs->name);
  } else if (auto *as = std::get_if<ast::StmtAssign>(&stmt.kind)) {
    collectFreeVarsInExpr(as->target.value, bound, freeVars);
    collectFreeVarsInExpr(as->value.value, bound, freeVars);
  } else if (auto *es = std::get_if<ast::StmtExpression>(&stmt.kind)) {
    collectFreeVarsInExpr(es->expr.value, bound, freeVars);
  } else if (auto *rs = std::get_if<ast::StmtReturn>(&stmt.kind)) {
    if (rs->value.has_value())
      collectFreeVarsInExpr(rs->value->value, bound, freeVars);
  } else if (auto *is = std::get_if<ast::StmtIf>(&stmt.kind)) {
    collectFreeVarsInExpr(is->condition.value, bound, freeVars);
    collectFreeVarsInBlock(is->then_block, bound, freeVars);
    if (is->else_block.has_value()) {
      if (is->else_block->block.has_value())
        collectFreeVarsInBlock(*is->else_block->block, bound, freeVars);
      if (is->else_block->if_stmt)
        collectFreeVarsInStmt(is->else_block->if_stmt->value, bound, freeVars);
    }
  } else if (auto *ils = std::get_if<ast::StmtIfLet>(&stmt.kind)) {
    if (ils->expr)
      collectFreeVarsInExpr(ils->expr->value, bound, freeVars);
    auto thenBound = bound;
    addPatternBindingsToSet(ils->pattern.value, thenBound);
    collectFreeVarsInBlock(ils->body, thenBound, freeVars);
    if (ils->else_body)
      collectFreeVarsInBlock(*ils->else_body, bound, freeVars);
  } else if (auto *ws = std::get_if<ast::StmtWhile>(&stmt.kind)) {
    collectFreeVarsInExpr(ws->condition.value, bound, freeVars);
    collectFreeVarsInBlock(ws->body, bound, freeVars);
  } else if (auto *fs = std::get_if<ast::StmtFor>(&stmt.kind)) {
    collectFreeVarsInExpr(fs->iterable.value, bound, freeVars);
    auto loopBound = bound;
    addPatternBindingsToSet(fs->pattern.value, loopBound);
    collectFreeVarsInBlock(fs->body, loopBound, freeVars);
  } else if (auto *ls2 = std::get_if<ast::StmtLoop>(&stmt.kind)) {
    collectFreeVarsInBlock(ls2->body, bound, freeVars);
  } else if (auto *ms = std::get_if<ast::StmtMatch>(&stmt.kind)) {
    collectFreeVarsInExpr(ms->scrutinee.value, bound, freeVars);
    for (const auto &arm : ms->arms) {
      auto armBound = bound;
      addPatternBindingsToSet(arm.pattern.value, armBound);
      if (arm.guard)
        collectFreeVarsInExpr(arm.guard->value, armBound, freeVars);
      if (arm.body)
        collectFreeVarsInExpr(arm.body->value, armBound, freeVars);
    }
  } else if (auto *bs = std::get_if<ast::StmtBreak>(&stmt.kind)) {
    if (bs->value)
      collectFreeVarsInExpr(bs->value->value, bound, freeVars);
  } else if (auto *ds = std::get_if<ast::StmtDefer>(&stmt.kind)) {
    if (ds->expr)
      collectFreeVarsInExpr(ds->expr->value, bound, freeVars);
  }
}

void MLIRGen::collectFreeVarsInBlock(const ast::Block &block, const std::set<std::string> &bound,
                                     std::set<std::string> &freeVars) {
  std::set<std::string> localBound = bound;
  for (const auto &s : block.stmts)
    collectFreeVarsInStmt(s->value, localBound, freeVars);
  if (block.trailing_expr)
    collectFreeVarsInExpr(block.trailing_expr->value, localBound, freeVars);
}

void MLIRGen::gatherCapturedVars(const std::set<std::string> &freeVars,
                                 std::vector<CapturedVarInfo> &capturedVars,
                                 mlir::Location location) {
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);

  for (const auto &fv : freeVars) {
    if (module.lookupSymbol<mlir::func::FuncOp>(fv) ||
        module.lookupSymbol<mlir::func::FuncOp>(mangleName(currentModulePath, "", fv)))
      continue;
    if (variantLookup.count(fv))
      continue;
    if (moduleConstants.count(fv))
      continue;

    if (auto mutAlloca = mutableVars.lookup(fv)) {
      auto originalSlot = mutAlloca;
      auto remapIt = heapCellRebindings.find(mutAlloca);
      if (remapIt != heapCellRebindings.end())
        mutAlloca = remapIt->second;
      auto cellIt = heapCellValueTypes.find(mutAlloca);
      if (cellIt != heapCellValueTypes.end()) {
        auto cellPtr = mlir::memref::LoadOp::create(builder, location, mutAlloca);
        capturedVars.push_back({fv, cellPtr, true, cellIt->second});
        continue;
      }

      auto val = mlir::memref::LoadOp::create(builder, location, mutAlloca);
      auto valueType = val.getType();
      auto cellSize =
          hew::SizeOfOp::create(builder, location, sizeType(), mlir::TypeAttr::get(valueType));
      auto nullPtr = mlir::LLVM::ZeroOp::create(builder, location, ptrType);
      auto cellPtr = hew::RcNewOp::create(builder, location, ptrType, nullPtr, cellSize, nullPtr);
      mlir::LLVM::StoreOp::create(builder, location, val, cellPtr);

      auto ptrMemrefType = mlir::MemRefType::get({}, ptrType);
      mlir::Value newAlloca;
      if (returnFlag && currentFunction) {
        auto savedIP = builder.saveInsertionPoint();
        auto &entryBlock = currentFunction.front();
        builder.setInsertionPointToStart(&entryBlock);
        newAlloca = mlir::memref::AllocaOp::create(builder, builder.getUnknownLoc(), ptrMemrefType);
        builder.restoreInsertionPoint(savedIP);
      } else {
        newAlloca = mlir::memref::AllocaOp::create(builder, location, ptrMemrefType);
      }
      mlir::memref::StoreOp::create(builder, location, cellPtr, newAlloca);

      auto internedName = intern(fv);
      mutableVars.insert(internedName, newAlloca);
      heapCellRebindings[originalSlot] = newAlloca;
      heapCellValueTypes[newAlloca] = valueType;

      // The mutable variable was promoted from stack to RC heap for
      // closure capture; the RC allocation must be freed when the
      // original var's scope ends.  Register at the FUNCTION-level
      // drop scope (not the current scope) because scope blocks and
      // closures may create these cells inside nested scopes, but the
      // mutable var itself lives in the enclosing function.  Dropping
      // at block scope would free the cell before scope.join/destroy
      // run (causing use-after-free in scope spawn patterns).
      //
      // Use a hoisted alloca so the drop works after the declaring
      // scope's symbol table has been popped.
      {
        std::string rcName =
            std::string("\0__rc_cell_", 11) + std::to_string(tempMaterializationCounter++);
        auto ptrMemrefType = mlir::MemRefType::get({}, ptrType);
        auto savedIP = builder.saveInsertionPoint();
        auto &entryBlock = currentFunction.front();
        builder.setInsertionPointToStart(&entryBlock);
        auto rcAlloca =
            mlir::memref::AllocaOp::create(builder, builder.getUnknownLoc(), ptrMemrefType);
        auto zero = mlir::LLVM::ZeroOp::create(builder, builder.getUnknownLoc(), ptrType);
        mlir::memref::StoreOp::create(builder, builder.getUnknownLoc(), zero, rcAlloca);
        builder.restoreInsertionPoint(savedIP);
        mlir::memref::StoreOp::create(builder, location, cellPtr, rcAlloca);

        DropEntry entry;
        entry.varName = rcName;
        entry.dropFuncName = "hew_rc_drop";
        entry.promotedSlot = rcAlloca;
        entry.bindingIdentity = rcAlloca;
        if (dropScopes.size() > funcLevelDropScopeBase)
          dropScopes[funcLevelDropScopeBase].push_back(std::move(entry));
      }

      capturedVars.push_back({fv, cellPtr, true, valueType});
      continue;
    }

    if (auto val = lookupVariable(fv))
      capturedVars.push_back({fv, val, false, nullptr});
  }
}

// ============================================================================
// Closure env drop function generation
// ============================================================================

/// Generate a small function `__env_drop_N(ptr)` that, for each mutable-capture
/// field in the env struct, loads the RC cell pointer and calls `hew_rc_drop`.
/// Also handles inner closure captures whose env RC needs dropping.
/// Returns a `FuncPtrOp` value pointing to the generated function, or a null
/// zero pointer if the env has no RC-backed captures.
mlir::Value MLIRGen::generateEnvDropFn(
    const std::vector<CapturedVarInfo> &capturedVars,
    mlir::LLVM::LLVMStructType envStructType, mlir::Location location) {
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);

  // Collect indices of fields that need RC drops.
  llvm::SmallVector<size_t, 4> rcFieldIndices;
  for (size_t i = 0; i < capturedVars.size(); ++i) {
    if (capturedVars[i].isMutable ||
        mlir::isa<hew::ClosureType>(capturedVars[i].value.getType()))
      rcFieldIndices.push_back(i);
  }
  if (rcFieldIndices.empty())
    return mlir::LLVM::ZeroOp::create(builder, location, ptrType);

  // Ensure hew_rc_drop is declared.
  auto rcDropType =
      builder.getFunctionType({ptrType}, {});
  getOrCreateExternFunc("hew_rc_drop", rcDropType);

  // Generate the drop function.
  std::string fnName = "__env_drop_" + std::to_string(envDropCounter++);
  auto fnType = builder.getFunctionType({ptrType}, {});

  auto savedIP = builder.saveInsertionPoint();
  auto savedFunction = currentFunction;
  auto savedFunctionReturnTypeExpr = currentFunctionReturnTypeExpr;
  builder.setInsertionPointToEnd(module.getBody());

  auto dropFn = mlir::func::FuncOp::create(builder, location, fnName, fnType);
  dropFn.setVisibility(mlir::SymbolTable::Visibility::Private);
  auto *entryBlock = dropFn.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);
  currentFunction = dropFn;
  currentFunctionReturnTypeExpr = nullptr;

  auto envArg = entryBlock->getArgument(0);
  for (size_t idx : rcFieldIndices) {
    auto gep = mlir::LLVM::GEPOp::create(
        builder, location, ptrType, envStructType, envArg,
        llvm::ArrayRef<mlir::LLVM::GEPArg>{
            static_cast<int32_t>(0), static_cast<int32_t>(idx)});
    auto fieldVal = mlir::LLVM::LoadOp::create(
        builder, location, ptrType, gep);
    // Null-guard: the field may be zero if the lambda was only partially
    // initialised (shouldn't happen normally, but be safe).
    auto nullPtr = mlir::LLVM::ZeroOp::create(builder, location, ptrType);
    auto isNotNull = mlir::LLVM::ICmpOp::create(
        builder, location, builder.getI1Type(),
        mlir::LLVM::ICmpPredicate::ne, fieldVal, nullPtr);
    auto guard = mlir::scf::IfOp::create(
        builder, location, mlir::TypeRange{}, isNotNull,
        /*withElseRegion=*/false);
    builder.setInsertionPointToStart(&guard.getThenRegion().front());
    mlir::func::CallOp::create(builder, location, "hew_rc_drop",
                                mlir::TypeRange{}, mlir::ValueRange{fieldVal});
    builder.setInsertionPointAfter(guard);
  }
  mlir::func::ReturnOp::create(builder, location);

  currentFunction = savedFunction;
  currentFunctionReturnTypeExpr = savedFunctionReturnTypeExpr;
  builder.restoreInsertionPoint(savedIP);

  return hew::FuncPtrOp::create(builder, location, ptrType,
                                 mlir::SymbolRefAttr::get(&context, fnName))
      .getResult();
}

// ============================================================================
// Lambda expression generation
// ============================================================================

mlir::Value MLIRGen::generateLambdaExpr(const ast::ExprLambda &lam) {
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);

  std::string lambdaName = "__lambda_" + std::to_string(lambdaCounter++);

  const auto &params = lam.params;
  llvm::SmallVector<mlir::Type, 4> userParamTypes;

  hew::ClosureType expectedClosureType = nullptr;
  if (pendingLambdaExpectedType) {
    expectedClosureType = mlir::dyn_cast<hew::ClosureType>(*pendingLambdaExpectedType);
  }

  for (size_t i = 0; i < params.size(); ++i) {
    const auto &param = params[i];
    if (param.ty.has_value()) {
      userParamTypes.push_back(convertType(param.ty->value));
    } else if (expectedClosureType && i < expectedClosureType.getInputTypes().size()) {
      userParamTypes.push_back(expectedClosureType.getInputTypes()[i]);
    } else {
      emitError(location) << "cannot infer type for lambda parameter '" << param.name << "'";
      return nullptr;
    }
  }

  mlir::Type expectedReturnType = nullptr;
  if (expectedClosureType) {
    auto ret = expectedClosureType.getResultType();
    if (ret && !mlir::isa<mlir::NoneType>(ret))
      expectedReturnType = ret;
  }

  // Closure conversion: collect free variables from the body
  std::set<std::string> paramNames;
  for (const auto &param : params)
    paramNames.insert(param.name);

  std::set<std::string> freeVars;
  if (lam.body)
    collectFreeVarsInExpr(lam.body->value, paramNames, freeVars);

  std::vector<CapturedVarInfo> capturedVars;
  gatherCapturedVars(freeVars, capturedVars, location);

  mlir::Type returnType = nullptr;
  if (lam.return_type.has_value()) {
    returnType = convertType(lam.return_type->value);
  } else if (expectedReturnType) {
    returnType = expectedReturnType;
  }

  llvm::SmallVector<mlir::Type, 8> funcParamTypes;
  funcParamTypes.push_back(ptrType); // env pointer
  for (auto t : userParamTypes)
    funcParamTypes.push_back(t);

  auto funcType = returnType ? mlir::FunctionType::get(&context, funcParamTypes, {returnType})
                             : mlir::FunctionType::get(&context, funcParamTypes, {});

  auto savedIP = builder.saveInsertionPoint();

  builder.setInsertionPointToEnd(module.getBody());
  auto funcOp = mlir::func::FuncOp::create(builder, location, lambdaName, funcType);
  funcOp.setVisibility(mlir::SymbolTable::Visibility::Private);

  auto &entryBlock = *funcOp.addEntryBlock();
  builder.setInsertionPointToStart(&entryBlock);

  FunctionGenerationScope funcScope(*this, funcOp);
  if (lam.return_type.has_value())
    currentFunctionReturnTypeExpr = &lam.return_type->value;

  SymbolTableScopeT scope(symbolTable);
  MutableTableScopeT mutScope(mutableVars);

  mlir::Value envPtr = entryBlock.getArgument(0);

  for (size_t idx = 0; idx < userParamTypes.size(); ++idx) {
    declareVariable(params[idx].name, entryBlock.getArgument(idx + 1));
  }

  if (!capturedVars.empty()) {
    llvm::SmallVector<mlir::Type, 4> capturedTypes;
    for (const auto &cv : capturedVars)
      capturedTypes.push_back(toLLVMStorageType(cv.value.getType()));
    auto envStructType = mlir::LLVM::LLVMStructType::getLiteral(&context, capturedTypes);

    for (size_t i = 0; i < capturedVars.size(); ++i) {
      auto gepOp = mlir::LLVM::GEPOp::create(
          builder, location, ptrType, envStructType, envPtr,
          llvm::ArrayRef<mlir::LLVM::GEPArg>{static_cast<int32_t>(0), static_cast<int32_t>(i)});
      auto loadedVal = mlir::LLVM::LoadOp::create(builder, location, capturedTypes[i], gepOp);

      if (capturedVars[i].isMutable) {
        // Mutable capture: loadedVal is the heap cell pointer (!llvm.ptr).
        // Create a local memref to hold it and register as a heap-cell var.
        auto ptrMemrefType = mlir::MemRefType::get({}, ptrType);
        auto lambdaAlloca = mlir::memref::AllocaOp::create(builder, location, ptrMemrefType);
        mlir::memref::StoreOp::create(builder, location, loadedVal, lambdaAlloca);
        auto internedName = intern(capturedVars[i].name);
        mutableVars.insert(internedName, lambdaAlloca);
        heapCellValueTypes[lambdaAlloca] = capturedVars[i].valueType;
      } else {
        mlir::Value capturedVal = loadedVal;
        if (capturedTypes[i] != capturedVars[i].value.getType()) {
          capturedVal =
              hew::BitcastOp::create(builder, location, capturedVars[i].value.getType(), loadedVal);
        }
        declareVariable(capturedVars[i].name, capturedVal);
      }
    }
  }

  // Save/restore funcLevelDropExcludeVars and funcLevelDropScopeBase so the
  // lambda's body block (which is function-level from popDropScope's
  // perspective) doesn't inherit the enclosing function's excludes.  Collect
  // the lambda body's trailing expression identifiers so the return value is
  // NOT dropped before the return op.
  auto savedExcludeVars = std::move(funcLevelDropExcludeVars);
  auto savedReturnVarNames = std::move(funcLevelReturnVarNames);
  auto savedEarlyReturnVarNames = std::move(funcLevelEarlyReturnVarNames);
  auto savedExcludeValues = std::move(funcLevelDropExcludeValues);
  auto savedExcludeResolvedNames = std::move(funcLevelDropExcludeResolvedNames);
  auto savedEarlyReturnExcludeValues = std::move(funcLevelEarlyReturnExcludeValues);
  auto savedEarlyReturnExcludeResolvedNames =
      std::move(funcLevelEarlyReturnExcludeResolvedNames);
  auto savedDropScopeBase = funcLevelDropScopeBase;
  funcLevelDropExcludeVars.clear();
  funcLevelReturnVarNames.clear();
  funcLevelEarlyReturnVarNames.clear();
  funcLevelDropExcludeValues.clear();
  funcLevelDropExcludeResolvedNames.clear();
  funcLevelEarlyReturnExcludeValues.clear();
  funcLevelEarlyReturnExcludeResolvedNames.clear();
  funcLevelDropScopeBase = dropScopes.size();
  if (lam.body) {
    // Mutually recursive helpers: expr ↔ block ↔ stmtIf (depth-aware)
    using ExcludeSet = std::set<std::pair<std::string, size_t>>;
    auto &excludeVars = funcLevelDropExcludeVars;
    std::function<void(const ast::Expr &, ExcludeSet &, size_t)> collectReturnVars;
    std::function<void(const ast::Block &, ExcludeSet &, size_t)> fromBlock;
    std::function<void(const ast::StmtIf &, ExcludeSet &, size_t)> fromStmtIf;
    fromStmtIf = [&fromBlock, &fromStmtIf](
        const ast::StmtIf &si, ExcludeSet &out, size_t depth) {
      fromBlock(si.then_block, out, depth + 1);
      if (si.else_block) {
        if (si.else_block->block)
          fromBlock(*si.else_block->block, out, depth + 1);
        if (si.else_block->if_stmt) {
          const auto &nested = si.else_block->if_stmt->value;
          if (auto *nif = std::get_if<ast::StmtIf>(&nested.kind))
            fromStmtIf(*nif, out, depth);
        }
      }
    };
    fromBlock = [&collectReturnVars, &fromStmtIf](
        const ast::Block &blk, ExcludeSet &out, size_t depth) {
      if (blk.trailing_expr) {
        collectReturnVars(blk.trailing_expr->value, out, depth);
      } else if (!blk.stmts.empty()) {
        const auto &last = blk.stmts.back()->value;
        if (auto *es = std::get_if<ast::StmtExpression>(&last.kind))
          collectReturnVars(es->expr.value, out, depth);
        else if (auto *ifs = std::get_if<ast::StmtIf>(&last.kind))
          fromStmtIf(*ifs, out, depth);
        else if (auto *ms = std::get_if<ast::StmtMatch>(&last.kind)) {
          for (const auto &arm : ms->arms)
            if (arm.body)
              collectReturnVars(arm.body->value, out, depth);
        }
      }
      // Scan let/var bindings whose RHS is a match/if/block — the arm
      // results are ownership-transferred into the binding.
      for (const auto &stmt : blk.stmts) {
        const ast::Expr *rhs = nullptr;
        if (auto *letStmt = std::get_if<ast::StmtLet>(&stmt->value.kind)) {
          if (letStmt->value)
            rhs = &letStmt->value->value;
        } else if (auto *varStmt = std::get_if<ast::StmtVar>(&stmt->value.kind)) {
          if (varStmt->value)
            rhs = &varStmt->value->value;
        }
        if (!rhs)
          continue;
        if (std::get_if<ast::ExprMatch>(&rhs->kind) ||
            std::get_if<ast::ExprIf>(&rhs->kind) ||
            std::get_if<ast::ExprIfLet>(&rhs->kind) ||
            std::get_if<ast::ExprBlock>(&rhs->kind)) {
          collectReturnVars(*rhs, out, depth);
        }
      }
    };
    collectReturnVars = [&collectReturnVars, &fromBlock](
        const ast::Expr &expr, ExcludeSet &out, size_t depth) {
      if (auto *id = std::get_if<ast::ExprIdentifier>(&expr.kind)) {
        out.insert({id->name, depth});
      } else if (auto *si = std::get_if<ast::ExprStructInit>(&expr.kind)) {
        for (const auto &[fieldName, fieldVal] : si->fields) {
          if (auto *fid = std::get_if<ast::ExprIdentifier>(&fieldVal->value.kind))
            out.insert({fid->name, depth});
        }
      } else if (auto *ifE = std::get_if<ast::ExprIf>(&expr.kind)) {
        if (ifE->then_block)
          collectReturnVars(ifE->then_block->value, out, depth);
        if (ifE->else_block && *ifE->else_block)
          collectReturnVars((*ifE->else_block)->value, out, depth);
      } else if (auto *ifLet = std::get_if<ast::ExprIfLet>(&expr.kind)) {
        fromBlock(ifLet->body, out, depth + 1);
        if (ifLet->else_body)
          fromBlock(*ifLet->else_body, out, depth + 1);
      } else if (auto *matchE = std::get_if<ast::ExprMatch>(&expr.kind)) {
        for (const auto &arm : matchE->arms) {
          if (arm.body)
            collectReturnVars(arm.body->value, out, depth);
        }
      } else if (auto *blockE = std::get_if<ast::ExprBlock>(&expr.kind)) {
        fromBlock(blockE->block, out, depth + 1);
      } else if (auto *tupleE = std::get_if<ast::ExprTuple>(&expr.kind)) {
        for (const auto &elem : tupleE->elements)
          collectReturnVars(elem->value, out, depth);
      } else if (auto *unsafeE = std::get_if<ast::ExprUnsafe>(&expr.kind)) {
        fromBlock(unsafeE->block, out, depth + 1);
      } else if (auto *callE = std::get_if<ast::ExprCall>(&expr.kind)) {
        for (const auto &arg : callE->args)
          collectReturnVars(ast::callArgExpr(arg).value, out, depth);
      }
    };
    // If the body is a block expression, use fromBlock to handle
    // trailing expressions and StmtIf/StmtMatch in last position.
    if (auto *blockExpr = std::get_if<ast::ExprBlock>(&lam.body->value.kind)) {
      fromBlock(blockExpr->block, excludeVars, 0);
    } else {
      // Simple expression body — e.g. `(x) => x`
      collectReturnVars(lam.body->value, excludeVars, 0);
    }
    // Build flat return-var name set for RAII exclusion
    for (const auto &[name, depth] : excludeVars)
      funcLevelReturnVarNames.insert(name);
    resolveFunctionDropExclusionCandidates();
  }

  mlir::Value bodyVal = nullptr;
  if (lam.body) {
    // Expression-bodied lambdas (e.g. (s) => s + "!") don't have a block
    // to push/pop drop scopes.  Wrap them so materialized temporaries
    // (string constants in concat, etc.) are dropped inside the lambda,
    // not leaked into the enclosing function's drop scope.
    // Block-bodied lambdas already manage their own scopes via generateBlock.
    bool needsExprScope = !std::holds_alternative<ast::ExprBlock>(lam.body->value.kind);
    if (needsExprScope)
      pushDropScope();
    bodyVal = generateExpression(lam.body->value);
    if (needsExprScope)
      popDropScope();
  }
  funcLevelDropExcludeVars = std::move(savedExcludeVars);
  funcLevelReturnVarNames = std::move(savedReturnVarNames);
  funcLevelEarlyReturnVarNames = std::move(savedEarlyReturnVarNames);
  funcLevelDropExcludeValues = std::move(savedExcludeValues);
  funcLevelDropExcludeResolvedNames = std::move(savedExcludeResolvedNames);
  funcLevelEarlyReturnExcludeValues = std::move(savedEarlyReturnExcludeValues);
  funcLevelEarlyReturnExcludeResolvedNames =
      std::move(savedEarlyReturnExcludeResolvedNames);
  funcLevelDropScopeBase = savedDropScopeBase;

  if (!returnType && bodyVal && bodyVal.getType()) {
    auto bodyType = bodyVal.getType();
    if (!llvm::isa<mlir::NoneType>(bodyType)) {
      returnType = bodyType;
      auto newFuncType = mlir::FunctionType::get(&context, funcParamTypes, {returnType});
      funcOp.setFunctionType(newFuncType);
      funcType = newFuncType;
    }
  }

  auto *currentBlock = builder.getInsertionBlock();
  if (currentBlock &&
      (currentBlock->empty() || !currentBlock->back().hasTrait<mlir::OpTrait::IsTerminator>())) {
    if (returnType && bodyVal) {
      bodyVal = coerceTypeForSink(bodyVal, returnType, location);
      mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{bodyVal});
    } else if (returnType) {
      auto defVal = createDefaultValue(builder, location, returnType);
      mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{defVal});
    } else {
      mlir::func::ReturnOp::create(builder, location);
    }
  }

  builder.restoreInsertionPoint(savedIP);

  mlir::Value envPtrVal;
  if (!capturedVars.empty()) {
    llvm::SmallVector<mlir::Type, 4> capturedTypes;
    for (const auto &cv : capturedVars)
      capturedTypes.push_back(toLLVMStorageType(cv.value.getType()));
    auto envStructType = mlir::LLVM::LLVMStructType::getLiteral(&context, capturedTypes);

    auto envSize =
        hew::SizeOfOp::create(builder, location, sizeType(), mlir::TypeAttr::get(envStructType));

    auto nullData = mlir::LLVM::ZeroOp::create(builder, location, ptrType);
    auto envDropFnPtr = generateEnvDropFn(capturedVars, envStructType, location);
    envPtrVal = hew::RcNewOp::create(builder, location, ptrType, nullData, envSize, envDropFnPtr);

    for (size_t i = 0; i < capturedVars.size(); ++i) {
      if (mlir::isa<hew::ClosureType>(capturedVars[i].value.getType())) {
        auto innerEnv =
            hew::ClosureGetEnvOp::create(builder, location, ptrType, capturedVars[i].value);
        hew::RcCloneOp::create(builder, location, ptrType, innerEnv);
      }
      // Mutable captures are RC cells; clone so the cell survives
      // scope-exit drops when the closure escapes.
      if (capturedVars[i].isMutable)
        hew::RcCloneOp::create(builder, location, ptrType, capturedVars[i].value);
      auto gepOp = mlir::LLVM::GEPOp::create(
          builder, location, ptrType, envStructType, envPtrVal,
          llvm::ArrayRef<mlir::LLVM::GEPArg>{static_cast<int32_t>(0), static_cast<int32_t>(i)});
      mlir::Value storeVal = capturedVars[i].value;
      if (storeVal.getType() != capturedTypes[i]) {
        storeVal = hew::BitcastOp::create(builder, location, capturedTypes[i], storeVal);
      }
      mlir::LLVM::StoreOp::create(builder, location, storeVal, gepOp);
    }
  } else {
    envPtrVal = mlir::LLVM::ZeroOp::create(builder, location, ptrType);
  }

  auto fnPtrVal = hew::FuncPtrOp::create(builder, location, ptrType,
                                         mlir::SymbolRefAttr::get(&context, lambdaName))
                      .getResult();

  mlir::Type closureRetType = returnType ? returnType : mlir::NoneType::get(&context);
  auto closureType = hew::ClosureType::get(&context, userParamTypes, closureRetType);

  return hew::ClosureCreateOp::create(builder, location, closureType, fnPtrVal, envPtrVal);
}

// ============================================================================
// Scope expression codegen
// ============================================================================

mlir::Value MLIRGen::generateScopeExpr(const ast::ExprScope &se) {
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);

  auto scopeCreateOp = hew::ScopeCreateOp::create(builder, location, ptrType, ptrType);
  auto scopePtr = scopeCreateOp.getActorScope();
  auto taskScopePtr = scopeCreateOp.getTaskScope();

  auto prevScope = currentScopePtr;
  auto prevTaskScope = currentTaskScopePtr;
  currentScopePtr = scopePtr;
  currentTaskScopePtr = taskScopePtr;

  auto savedReturnFlag = returnFlag;
  auto savedReturnSlotIsLazy = returnSlotIsLazy;
  auto savedEarlyReturnFlag = earlyReturnFlag;
  returnFlag = nullptr;
  returnSlotIsLazy = false;
  earlyReturnFlag = nullptr;

  // Manually create scopes instead of calling generateBlock, so that
  // scope.join/destroy happen BEFORE the block-level drops fire.
  // This is critical because scope spawns create RC cells for mutable
  // captures; those cells must survive until tasks complete and the
  // env drops run (during scope.destroy).
  SymbolTableScopeT bindingScope(symbolTable);
  if (se.binding.has_value())
    declareVariable(*se.binding, scopePtr);

  SymbolTableScopeT varScope(symbolTable);
  MutableTableScopeT mutScope(mutableVars);
  pushDropScope();

  const auto &stmts = se.block.stmts;

  mlir::Value bodyResult = nullptr;
  if (se.block.trailing_expr) {
    // Block has an explicit trailing expression.
    for (const auto &stmtPtr : stmts) {
      generateStatement(stmtPtr->value);
      if (hasRealTerminator(builder.getInsertionBlock()))
        break;
    }
    bodyResult = generateExpression(se.block.trailing_expr->value);
    // Null RAII close alloca so scope-exit drop doesn't close a
    // handle being returned out of the scope block.
    if (auto *id = std::get_if<ast::ExprIdentifier>(
            &se.block.trailing_expr->value.kind))
      nullOutRaiiAlloca(id->name);
  } else if (!stmts.empty()) {
    // No trailing_expr — generate all but the last statement, then handle
    // the last one specially (mirrors generateBlock's value-producing
    // final-statement logic for if/match/loop/expr-stmt endings).
    for (size_t i = 0; i + 1 < stmts.size(); ++i) {
      generateStatement(stmts[i]->value);
      if (hasRealTerminator(builder.getInsertionBlock()))
        break;
    }
    if (!hasRealTerminator(builder.getInsertionBlock())) {
      const auto &lastStmt = stmts.back()->value;
      if (auto *exprStmt = std::get_if<ast::StmtExpression>(&lastStmt.kind)) {
        bodyResult = generateExpression(exprStmt->expr.value);
        if (auto *id = std::get_if<ast::ExprIdentifier>(
                &exprStmt->expr.value.kind))
          nullOutRaiiAlloca(id->name);
      } else if (auto *ifStmt = std::get_if<ast::StmtIf>(&lastStmt.kind)) {
        bodyResult = generateIfStmtAsExpr(*ifStmt);
      } else if (auto *matchNode = std::get_if<ast::StmtMatch>(&lastStmt.kind)) {
        auto loc_ = loc(lastStmt.span);
        auto scrutinee = generateExpression(matchNode->scrutinee.value);
        if (scrutinee) {
          scrutinee = derefIndirectEnumScrutinee(scrutinee, matchNode->scrutinee.span, loc_,
                                                 &matchNode->arms);
          mlir::Type resultType;
          if (currentFunction && currentFunction.getResultTypes().size() == 1)
            resultType = currentFunction.getResultTypes()[0];
          else
            resultType = builder.getI32Type();
          bodyResult = generateMatchImpl(scrutinee, matchNode->arms, resultType, loc_);
        }
      } else if (std::holds_alternative<ast::StmtLoop>(lastStmt.kind) ||
                 std::holds_alternative<ast::StmtWhile>(lastStmt.kind) ||
                 std::holds_alternative<ast::StmtWhileLet>(lastStmt.kind) ||
                 std::holds_alternative<ast::StmtFor>(lastStmt.kind)) {
        lastBreakValue = nullptr;
        generateStatement(lastStmt);
        if (lastBreakValue)
          bodyResult = lastBreakValue;
      } else {
        generateStatement(lastStmt);
      }
    }
  }

  // Join and destroy scope BEFORE the drop scope pops.
  // scope.destroy calls hew_task_free which drops env RC cells.
  // The block-level RC cell drops must fire AFTER this.
  currentScopePtr = prevScope;
  currentTaskScopePtr = prevTaskScope;
  hew::ScopeJoinOp::create(builder, location, scopePtr, taskScopePtr);
  hew::ScopeDestroyOp::create(builder, location, scopePtr, taskScopePtr);

  popDropScope();

  returnFlag = savedReturnFlag;
  returnSlotIsLazy = savedReturnSlotIsLazy;
  earlyReturnFlag = savedEarlyReturnFlag;

  return bodyResult;
}

// ============================================================================
// scope.launch { body }
// ============================================================================

mlir::Value MLIRGen::generateScopeLaunchExpr(const ast::ExprScopeLaunch &sle) {
  return generateScopeLaunchImpl(sle.block);
}

// ============================================================================
// scope.spawn { body } — identical to scope.launch for now
// ============================================================================

mlir::Value MLIRGen::generateScopeSpawnExpr(const ast::ExprScopeSpawn &sse) {
  return generateScopeLaunchImpl(sse.block);
}

// ============================================================================
// scope.launch / scope.spawn shared implementation
// ============================================================================

mlir::Value MLIRGen::generateScopeLaunchImpl(const ast::Block &block) {
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);

  std::set<std::string> boundNames;
  std::set<std::string> freeVars;
  collectFreeVarsInBlock(block, boundNames, freeVars);

  std::vector<CapturedVarInfo> capturedVars;
  gatherCapturedVars(freeVars, capturedVars, location);

  llvm::SmallVector<mlir::Type, 4> capturedTypes;
  mlir::LLVM::LLVMStructType envStructType = nullptr;
  mlir::Value envPtrVal;
  if (!capturedVars.empty()) {
    for (const auto &cv : capturedVars)
      capturedTypes.push_back(toLLVMStorageType(cv.value.getType()));
    envStructType = mlir::LLVM::LLVMStructType::getLiteral(&context, capturedTypes);

    auto envSize =
        hew::SizeOfOp::create(builder, location, sizeType(), mlir::TypeAttr::get(envStructType));

    auto envDropFnPtr = generateEnvDropFn(capturedVars, envStructType, location);
    auto nullData = mlir::LLVM::ZeroOp::create(builder, location, ptrType);
    envPtrVal = hew::RcNewOp::create(builder, location, ptrType, nullData, envSize, envDropFnPtr);

    for (size_t i = 0; i < capturedVars.size(); ++i) {
      if (mlir::isa<hew::ClosureType>(capturedVars[i].value.getType())) {
        auto innerEnv =
            hew::ClosureGetEnvOp::create(builder, location, ptrType, capturedVars[i].value);
        hew::RcCloneOp::create(builder, location, ptrType, innerEnv);
      }
      if (capturedVars[i].isMutable)
        hew::RcCloneOp::create(builder, location, ptrType, capturedVars[i].value);
      auto gepOp = mlir::LLVM::GEPOp::create(
          builder, location, ptrType, envStructType, envPtrVal,
          llvm::ArrayRef<mlir::LLVM::GEPArg>{static_cast<int32_t>(0), static_cast<int32_t>(i)});
      mlir::Value storeVal = capturedVars[i].value;
      if (storeVal.getType() != capturedTypes[i]) {
        storeVal = hew::BitcastOp::create(builder, location, capturedTypes[i], storeVal);
      }
      mlir::LLVM::StoreOp::create(builder, location, storeVal, gepOp);
    }
  } else {
    envPtrVal = mlir::LLVM::ZeroOp::create(builder, location, ptrType);
  }

  auto savedIP = builder.saveInsertionPoint();

  std::string taskFnName = "__scope_task_" + std::to_string(taskCounter++);
  auto voidType = builder.getFunctionType({ptrType}, {});

  builder.setInsertionPointToEnd(module.getBody());
  auto taskFn = mlir::func::FuncOp::create(builder, location, taskFnName, voidType);
  taskFn.setPrivate();

  auto *entryBlock = taskFn.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto taskArg = entryBlock->getArgument(0);

  FunctionGenerationScope funcScope(*this, taskFn);
  auto savedScopePtr = currentScopePtr;
  auto savedTaskScopePtr = currentTaskScopePtr;
  currentScopePtr = nullptr;
  currentTaskScopePtr = nullptr;

  // Save/restore drop scope state so the task body's drops don't leak into
  // the enclosing function's drop scopes (same pattern as lambda codegen).
  auto savedDropScopeBase = funcLevelDropScopeBase;
  auto savedExcludeVars = std::move(funcLevelDropExcludeVars);
  auto savedReturnVarNames = std::move(funcLevelReturnVarNames);
  auto savedEarlyReturnVarNames2 = std::move(funcLevelEarlyReturnVarNames);
  auto savedExcludeValues = std::move(funcLevelDropExcludeValues);
  auto savedExcludeResolvedNames = std::move(funcLevelDropExcludeResolvedNames);
  auto savedEarlyReturnExcludeValues = std::move(funcLevelEarlyReturnExcludeValues);
  auto savedEarlyReturnExcludeResolvedNames =
      std::move(funcLevelEarlyReturnExcludeResolvedNames);
  funcLevelDropScopeBase = dropScopes.size();
  funcLevelDropExcludeVars.clear();
  funcLevelReturnVarNames.clear();
  funcLevelEarlyReturnVarNames.clear();
  funcLevelDropExcludeValues.clear();
  funcLevelDropExcludeResolvedNames.clear();
  funcLevelEarlyReturnExcludeValues.clear();
  funcLevelEarlyReturnExcludeResolvedNames.clear();

  SymbolTableScopeT taskVarScope(symbolTable);
  MutableTableScopeT taskMutScope(mutableVars);

  if (!capturedVars.empty()) {
    auto envPtr = hew::TaskGetEnvOp::create(builder, location, ptrType, taskArg);
    for (size_t i = 0; i < capturedVars.size(); ++i) {
      auto gepOp = mlir::LLVM::GEPOp::create(
          builder, location, ptrType, envStructType, envPtr,
          llvm::ArrayRef<mlir::LLVM::GEPArg>{static_cast<int32_t>(0), static_cast<int32_t>(i)});
      auto loadedVal = mlir::LLVM::LoadOp::create(builder, location, capturedTypes[i], gepOp);

      if (capturedVars[i].isMutable) {
        auto ptrMemrefType = mlir::MemRefType::get({}, ptrType);
        auto alloca = mlir::memref::AllocaOp::create(builder, location, ptrMemrefType);
        mlir::memref::StoreOp::create(builder, location, loadedVal, alloca);
        auto internedName = intern(capturedVars[i].name);
        mutableVars.insert(internedName, alloca);
        heapCellValueTypes[alloca] = capturedVars[i].valueType;
      } else {
        mlir::Value capturedVal = loadedVal;
        if (capturedTypes[i] != capturedVars[i].value.getType()) {
          capturedVal =
              hew::BitcastOp::create(builder, location, capturedVars[i].value.getType(), loadedVal);
        }
        declareVariable(capturedVars[i].name, capturedVal);
      }
    }
  }

  auto bodyResult = generateBlock(block);

  funcLevelDropScopeBase = savedDropScopeBase;
  funcLevelDropExcludeVars = std::move(savedExcludeVars);
  funcLevelReturnVarNames = std::move(savedReturnVarNames);
  funcLevelEarlyReturnVarNames = std::move(savedEarlyReturnVarNames2);
  funcLevelDropExcludeValues = std::move(savedExcludeValues);
  funcLevelDropExcludeResolvedNames = std::move(savedExcludeResolvedNames);
  funcLevelEarlyReturnExcludeValues = std::move(savedEarlyReturnExcludeValues);
  funcLevelEarlyReturnExcludeResolvedNames =
      std::move(savedEarlyReturnExcludeResolvedNames);

  currentScopePtr = savedScopePtr;
  currentTaskScopePtr = savedTaskScopePtr;

  if (bodyResult) {
    lastScopeLaunchResultType = bodyResult.getType();

    auto sizeVal = hew::SizeOfOp::create(builder, location, sizeType(),
                                         mlir::TypeAttr::get(bodyResult.getType()));

    auto resultPtr = hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{ptrType},
                                                mlir::SymbolRefAttr::get(&context, "malloc"),
                                                mlir::ValueRange{sizeVal})
                         .getResult();
    mlir::LLVM::StoreOp::create(builder, location, bodyResult, resultPtr);

    hew::TaskSetResultOp::create(builder, location, taskArg, resultPtr, sizeVal);
    // Free the temp buffer after result is copied into task
    auto freeFuncType = mlir::FunctionType::get(&context, {ptrType}, {});
    getOrCreateExternFunc("free", freeFuncType);
    mlir::func::CallOp::create(builder, location, "free", mlir::TypeRange{},
                               mlir::ValueRange{resultPtr});
  }

  hew::TaskCompleteOp::create(builder, location, taskArg);
  mlir::func::ReturnOp::create(builder, location);

  builder.restoreInsertionPoint(savedIP);

  auto fnPtr = hew::FuncPtrOp::create(builder, location, ptrType,
                                      mlir::SymbolRefAttr::get(builder.getContext(), taskFnName))
                   .getResult();

  auto taskPtr =
      hew::ScopeLaunchOp::create(builder, location, ptrType, currentTaskScopePtr, fnPtr, envPtrVal);

  return taskPtr;
}

// ============================================================================
// scope.cancel()
// ============================================================================

mlir::Value MLIRGen::generateScopeCancelExpr() {
  auto location = builder.getUnknownLoc();
  if (!currentTaskScopePtr) {
    emitError(location) << "scope.cancel() used outside a scope block";
    return nullptr;
  }

  hew::ScopeCancelOp::create(builder, location, currentTaskScopePtr);
  return nullptr;
}

// ============================================================================
// Helper: join currentModulePath into a "::" delimited key
// ============================================================================

std::string MLIRGen::currentModuleKey() const {
  std::string key;
  for (const auto &seg : currentModulePath)
    key += (key.empty() ? "" : "::") + seg;
  return key;
}

// ============================================================================
// Helper: look up a function through imported module paths
// ============================================================================

mlir::func::FuncOp MLIRGen::lookupImportedFunc(llvm::StringRef typeName, llvm::StringRef funcName) {
  auto modKey = currentModuleKey();
  auto impIt = moduleImports.find(modKey);
  if (impIt == moduleImports.end())
    return nullptr;
  for (const auto &impPath : impIt->second) {
    std::string tryName = mangleName(impPath, typeName.str(), funcName.str());
    if (auto callee = module.lookupSymbol<mlir::func::FuncOp>(tryName))
      return callee;
  }
  return nullptr;
}

// ============================================================================
// Helper: emit a RuntimeCallOp
// ============================================================================

mlir::Value MLIRGen::emitRuntimeCall(llvm::StringRef callee, mlir::Type resultType,
                                     mlir::ValueRange args, mlir::Location location) {
  auto calleeAttr = mlir::SymbolRefAttr::get(&context, callee);
  if (resultType) {
    auto op = hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{resultType}, calleeAttr,
                                         args);
    return op.getResult();
  }
  hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{}, calleeAttr, args);
  return nullptr;
}

// ============================================================================
// Helper: resolve actor type name from an expression
// ============================================================================

std::string MLIRGen::resolveActorTypeName(const ast::Expr &expr, const ast::Span *span) {
  // Prefer resolved type from the type checker when a span is available.
  if (span) {
    if (auto *typeExpr = resolvedTypeOf(*span)) {
      auto name = typeExprToActorName(*typeExpr);
      if (!name.empty())
        return name;
    }
  }
  if (auto *ie = std::get_if<ast::ExprIdentifier>(&expr.kind)) {
    auto it = actorVarTypes.find(ie->name);
    if (it != actorVarTypes.end())
      return it->second;
    // Bare field name that is an actor-typed actor field (e.g. `target`)
    if (!currentActorName.empty()) {
      auto key = currentActorName + "." + ie->name;
      auto aft = actorFieldTypes.find(key);
      if (aft != actorFieldTypes.end() && actorRegistry.count(aft->second))
        return aft->second;
    }
  }
  if (auto *fa = std::get_if<ast::ExprFieldAccess>(&expr.kind)) {
    std::string baseName;
    if (auto *baseIE = std::get_if<ast::ExprIdentifier>(&fa->object->value.kind)) {
      auto baseIt = actorVarTypes.find(baseIE->name);
      if (baseIt != actorVarTypes.end())
        baseName = baseIt->second;
    }
    if (!baseName.empty()) {
      // Check supervisor child names first
      auto scnIt = supervisorChildNames.find(baseName);
      if (scnIt != supervisorChildNames.end()) {
        for (const auto &[childName, childType] : scnIt->second) {
          if (childName == fa->field)
            return childType;
        }
      }
      auto key = baseName + "." + fa->field;
      auto aft = actorFieldTypes.find(key);
      if (aft != actorFieldTypes.end() && actorRegistry.count(aft->second))
        return aft->second;
    }
  }
  return "";
}

// ============================================================================
// Helper: pack argument values into a stack-allocated buffer
// ============================================================================

std::pair<mlir::Value, mlir::Value> MLIRGen::packArgsForSend(llvm::ArrayRef<mlir::Value> args,
                                                             mlir::Location location) {
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);

  auto packOp =
      hew::PackArgsOp::create(builder, location, mlir::TypeRange{ptrType, sizeType()}, args);
  return {packOp.getDataPtr(), packOp.getDataSize()};
}

mlir::Value MLIRGen::waitOnReplyChannel(mlir::Value channel, mlir::Type resultType,
                                        mlir::Location location) {
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto replyPtr = hew::SelectWaitOp::create(builder, location, ptrType, channel);
  auto resultVal = mlir::LLVM::LoadOp::create(builder, location, resultType, replyPtr);

  getOrCreateExternFunc("free", mlir::FunctionType::get(&context, {ptrType}, {}));
  mlir::func::CallOp::create(builder, location, "free", mlir::TypeRange{},
                             mlir::ValueRange{replyPtr});
  return resultVal;
}

void MLIRGen::cancelReplyChannel(mlir::Value channel, mlir::Location location) {
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  getOrCreateExternFunc("hew_reply_channel_cancel",
                        mlir::FunctionType::get(&context, {ptrType}, {}));
  mlir::func::CallOp::create(builder, location, "hew_reply_channel_cancel", mlir::TypeRange{},
                             mlir::ValueRange{channel});
}

void MLIRGen::abandonReplyChannel(mlir::Value channel, mlir::Location location) {
  cancelReplyChannel(channel, location);
  hew::SelectDestroyOp::create(builder, location, channel);
}

void MLIRGen::panicOnReplySendFailure(mlir::Value sendStatus,
                                      llvm::ArrayRef<mlir::Value> pendingChannels,
                                      mlir::Location location) {
  auto i32Type = builder.getI32Type();
  auto okStatus = mlir::arith::ConstantIntOp::create(builder, location, i32Type, 0);
  auto sendFailed = mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::ne,
                                                sendStatus, okStatus);
  auto failIf = mlir::scf::IfOp::create(builder, location, sendFailed, /*withElseRegion=*/false);
  builder.setInsertionPointToStart(&failIf.getThenRegion().front());
  for (auto channel : pendingChannels)
    abandonReplyChannel(channel, location);
  // Use RuntimeCallOp here because we are inside an scf.if then-region where
  // PanicOp (Terminator) cannot be the last op — scf.yield must be.
  hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                             mlir::SymbolRefAttr::get(&context, "hew_panic"),
                             mlir::ValueRange{});
  builder.setInsertionPointAfter(failIf);
}

// ============================================================================
// Select expression codegen
// ============================================================================

mlir::Value MLIRGen::generateSelectExpr(const ast::ExprSelect &sel) {
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto i32Type = builder.getI32Type();
  auto i64Type = builder.getI64Type();

  const auto &arms = sel.arms;
  size_t armCount = arms.size();
  if (armCount == 0) {
    emitError(location) << "select expression must have at least one arm";
    return nullptr;
  }

  llvm::SmallVector<mlir::Value, 4> channels;
  llvm::SmallVector<mlir::Type, 4> resultTypes;

  for (size_t i = 0; i < armCount; ++i) {
    const auto &arm = arms[i];

    // Source is either actor.method(args) or await actor.method(args)
    const ast::ExprMethodCall *mcPtr = nullptr;
    if (auto *mc = std::get_if<ast::ExprMethodCall>(&arm.source->value.kind)) {
      mcPtr = mc;
    } else if (auto *awaitE = std::get_if<ast::ExprAwait>(&arm.source->value.kind)) {
      if (auto *mc = std::get_if<ast::ExprMethodCall>(&awaitE->inner->value.kind)) {
        mcPtr = mc;
      }
    }
    if (!mcPtr) {
      emitError(location) << "select arm source must be actor.method(args)";
      return nullptr;
    }

    auto receiver = generateExpression(mcPtr->receiver->value);
    if (!receiver)
      return nullptr;

    std::string actorTypeName =
        resolveActorTypeName(mcPtr->receiver->value, &mcPtr->receiver->span);
    if (actorTypeName.empty()) {
      emitError(location) << "cannot resolve actor type for select arm source";
      return nullptr;
    }
    auto actorIt = actorRegistry.find(actorTypeName);
    if (actorIt == actorRegistry.end()) {
      emitError(location) << "unknown actor type '" << actorTypeName << "' in select arm";
      return nullptr;
    }
    const auto &actorInfo = actorIt->second;

    const auto &selectMethodName = mcPtr->method;
    int64_t msgIdx = -1;
    const ActorReceiveInfo *recvInfo = nullptr;
    for (size_t j = 0; j < actorInfo.receiveFns.size(); ++j) {
      if (actorInfo.receiveFns[j].name == selectMethodName) {
        msgIdx = static_cast<int64_t>(j);
        recvInfo = &actorInfo.receiveFns[j];
        break;
      }
    }
    if (!recvInfo || !recvInfo->returnType.has_value()) {
      emitError(location) << "select arm requires receive handler '" << selectMethodName
                          << "' with a return type";
      return nullptr;
    }
    resultTypes.push_back(*recvInfo->returnType);

    auto ch = hew::SelectCreateOp::create(builder, location, ptrType);
    channels.push_back(ch);

    llvm::SmallVector<mlir::Value, 4> argVals;
    for (const auto &arg : mcPtr->args) {
      auto val = generateExpression(ast::callArgExpr(arg).value);
      if (!val)
        return nullptr;
      argVals.push_back(val);
    }
    auto [dataPtr, dataSize] = packArgsForSend(argVals, location);

    auto msgTypeVal = mlir::arith::ConstantIntOp::create(builder, location, i32Type, msgIdx);
    auto sendStatus = hew::SelectAddOp::create(builder, location, i32Type, receiver, msgTypeVal,
                                               dataPtr, dataSize, ch)
                          .getResult();
    panicOnReplySendFailure(sendStatus, channels, location);
  }

  auto armCountVal = mlir::arith::ConstantIntOp::create(builder, location, i64Type, armCount);
  auto channelArray =
      mlir::LLVM::AllocaOp::create(builder, location, ptrType, ptrType, armCountVal);

  for (size_t i = 0; i < armCount; ++i) {
    auto gep =
        mlir::LLVM::GEPOp::create(builder, location, ptrType, ptrType, channelArray,
                                  llvm::ArrayRef<mlir::LLVM::GEPArg>{static_cast<int32_t>(i)});
    mlir::LLVM::StoreOp::create(builder, location, channels[i], gep);
  }

  int64_t timeoutMs = 2147483647;
  if (sel.timeout.has_value() && *sel.timeout && (*sel.timeout)->duration) {
    auto timeoutVal = generateExpression((*sel.timeout)->duration->value);
    if (timeoutVal) {
      if (auto constOp = timeoutVal.getDefiningOp<mlir::arith::ConstantIntOp>()) {
        // Duration values are i64 nanoseconds; runtime expects milliseconds.
        timeoutMs = constOp.value() / 1'000'000;
      }
    }
  }

  auto countVal = mlir::arith::ConstantIntOp::create(builder, location, i32Type,
                                                     static_cast<int64_t>(armCount));
  auto timeoutVal = mlir::arith::ConstantIntOp::create(builder, location, i32Type, timeoutMs);
  auto winnerIdx =
      hew::SelectFirstOp::create(builder, location, i32Type, channelArray, countVal, timeoutVal);

  // Select consumes only one reply. Mark every other channel as abandoned so late
  // replies can self-clean in the runtime, including the timeout path where no arm wins.
  for (size_t i = 0; i < armCount; ++i) {
    auto armIdxVal =
        mlir::arith::ConstantIntOp::create(builder, location, i32Type, static_cast<int64_t>(i));
    auto shouldCancel = mlir::arith::CmpIOp::create(
        builder, location, mlir::arith::CmpIPredicate::ne, winnerIdx, armIdxVal);
    auto cancelIf = mlir::scf::IfOp::create(builder, location, shouldCancel,
                                            /*withElseRegion=*/false);
    builder.setInsertionPointToStart(&cancelIf.getThenRegion().front());
    abandonReplyChannel(channels[i], location);
    builder.setInsertionPointAfter(cancelIf);
  }

  mlir::Type selectResultType = resultTypes[0];

  bool hasTimeoutBody = sel.timeout.has_value() && *sel.timeout && (*sel.timeout)->body;
  auto coerceSelectResultForSink = [&](mlir::Value value) -> mlir::Value {
    auto coerced = coerceType(value, selectResultType, location);
    if (coerced && coerced.getType() == selectResultType)
      return coerced;

    if (coerced)
      ++errorCount_;
    return createDefaultValue(builder, location, selectResultType);
  };

  auto generateArmChain = [&](auto &&self, size_t armIdx) -> mlir::Value {
    if (armIdx >= armCount) {
      if (hasTimeoutBody) {
        SymbolTableScopeT scope(symbolTable);
        MutableTableScopeT mutScope(mutableVars);
        auto val = generateExpression((*sel.timeout)->body->value);
        return val ? coerceSelectResultForSink(val)
                   : createDefaultValue(builder, location, selectResultType);
      }
      return createDefaultValue(builder, location, selectResultType);
    }

    bool isLast = (armIdx + 1 == armCount && !hasTimeoutBody);

    if (isLast) {
      SymbolTableScopeT scope(symbolTable);
      MutableTableScopeT mutScope(mutableVars);
      const auto &arm = arms[armIdx];

      auto resultVal = waitOnReplyChannel(channels[armIdx], resultTypes[armIdx], location);

      if (auto *ip = std::get_if<ast::PatIdentifier>(&arm.binding.value.kind))
        declareVariable(ip->name, resultVal);

      auto bodyVal = generateExpression(arm.body->value);
      hew::SelectDestroyOp::create(builder, location, channels[armIdx]);

      auto retVal = bodyVal ? bodyVal : createDefaultValue(builder, location, selectResultType);
      return coerceSelectResultForSink(retVal);
    }

    auto armIdxVal = mlir::arith::ConstantIntOp::create(builder, location, i32Type,
                                                        static_cast<int64_t>(armIdx));
    auto cond = mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::eq,
                                            winnerIdx, armIdxVal);

    auto ifOp =
        mlir::scf::IfOp::create(builder, location, selectResultType, cond, /*withElseRegion=*/true);

    builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
    {
      SymbolTableScopeT scope(symbolTable);
      MutableTableScopeT mutScope(mutableVars);
      const auto &arm = arms[armIdx];

      auto resultVal = waitOnReplyChannel(channels[armIdx], resultTypes[armIdx], location);

      if (auto *ip = std::get_if<ast::PatIdentifier>(&arm.binding.value.kind))
        declareVariable(ip->name, resultVal);

      auto bodyVal = generateExpression(arm.body->value);
      hew::SelectDestroyOp::create(builder, location, channels[armIdx]);

      auto yieldVal = bodyVal ? bodyVal : createDefaultValue(builder, location, selectResultType);
      yieldVal = coerceSelectResultForSink(yieldVal);
      mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{yieldVal});
    }

    builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
    auto elseVal = self(self, armIdx + 1);
    auto *elseBlock = builder.getInsertionBlock();
    if (elseBlock->empty() || !elseBlock->back().hasTrait<mlir::OpTrait::IsTerminator>()) {
      auto yieldVal = elseVal ? elseVal : createDefaultValue(builder, location, selectResultType);
      yieldVal = coerceSelectResultForSink(yieldVal);
      mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{yieldVal});
    }

    builder.setInsertionPointAfter(ifOp);
    return ifOp.getResult(0);
  };

  return generateArmChain(generateArmChain, 0);
}

// ============================================================================
// Join expression codegen
// ============================================================================

mlir::Value MLIRGen::generateJoinExpr(const ast::ExprJoin &join) {
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto i32Type = builder.getI32Type();

  const auto &exprs = join.exprs;
  size_t exprCount = exprs.size();
  if (exprCount == 0) {
    emitError(location) << "join expression must have at least one expression";
    return nullptr;
  }

  llvm::SmallVector<mlir::Value, 4> channels;
  llvm::SmallVector<mlir::Type, 4> resultTypes;

  for (size_t i = 0; i < exprCount; ++i) {
    const auto &awaitExpr = exprs[i]->value;

    const ast::ExprMethodCall *mcPtr = nullptr;
    if (auto *mc = std::get_if<ast::ExprMethodCall>(&awaitExpr.kind)) {
      mcPtr = mc;
    } else if (auto *awaitE = std::get_if<ast::ExprAwait>(&awaitExpr.kind)) {
      if (auto *mc = std::get_if<ast::ExprMethodCall>(&awaitE->inner->value.kind)) {
        mcPtr = mc;
      }
    }
    if (!mcPtr) {
      emitError(location) << "join expression element must be actor.method(args)";
      return nullptr;
    }

    auto receiver = generateExpression(mcPtr->receiver->value);
    if (!receiver)
      return nullptr;

    std::string actorTypeName =
        resolveActorTypeName(mcPtr->receiver->value, &mcPtr->receiver->span);
    if (actorTypeName.empty()) {
      emitError(location) << "cannot resolve actor type for join element";
      return nullptr;
    }
    auto actorIt = actorRegistry.find(actorTypeName);
    if (actorIt == actorRegistry.end()) {
      emitError(location) << "unknown actor type '" << actorTypeName << "' in join";
      return nullptr;
    }
    const auto &actorInfo = actorIt->second;

    const auto &joinMethodName = mcPtr->method;
    int64_t msgIdx = -1;
    const ActorReceiveInfo *recvInfo = nullptr;
    for (size_t j = 0; j < actorInfo.receiveFns.size(); ++j) {
      if (actorInfo.receiveFns[j].name == joinMethodName) {
        msgIdx = static_cast<int64_t>(j);
        recvInfo = &actorInfo.receiveFns[j];
        break;
      }
    }
    if (!recvInfo || !recvInfo->returnType.has_value()) {
      emitError(location) << "join element requires receive handler '" << joinMethodName
                          << "' with a return type";
      return nullptr;
    }
    resultTypes.push_back(*recvInfo->returnType);

    auto ch = hew::SelectCreateOp::create(builder, location, ptrType);
    channels.push_back(ch);

    llvm::SmallVector<mlir::Value, 4> argVals;
    for (const auto &arg : mcPtr->args) {
      auto val = generateExpression(ast::callArgExpr(arg).value);
      if (!val)
        return nullptr;
      argVals.push_back(val);
    }
    auto [dataPtr, dataSize] = packArgsForSend(argVals, location);

    auto msgTypeVal = mlir::arith::ConstantIntOp::create(builder, location, i32Type, msgIdx);
    auto sendStatus = hew::SelectAddOp::create(builder, location, i32Type, receiver, msgTypeVal,
                                               dataPtr, dataSize, ch)
                          .getResult();
    panicOnReplySendFailure(sendStatus, channels, location);
  }

  llvm::SmallVector<mlir::Value, 4> results;
  for (size_t i = 0; i < exprCount; ++i) {
    auto resultVal = waitOnReplyChannel(channels[i], resultTypes[i], location);
    results.push_back(resultVal);
    hew::SelectDestroyOp::create(builder, location, channels[i]);
  }

  if (exprCount == 1)
    return results[0];

  llvm::SmallVector<mlir::Type, 4> tupleFieldTypes;
  for (auto &rt : resultTypes)
    tupleFieldTypes.push_back(rt);
  auto tupleType = hew::HewTupleType::get(&context, tupleFieldTypes);

  return hew::TupleCreateOp::create(builder, location, tupleType, results);
}
